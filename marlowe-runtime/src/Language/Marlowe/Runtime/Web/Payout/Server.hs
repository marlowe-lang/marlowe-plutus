{-# LANGUAGE ExplicitNamespaces #-}

-- | This module defines a server for the /payouts REST API.
module Language.Marlowe.Runtime.Web.Payout.Server (server) where

import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Language.Marlowe.Runtime.Query (Page (..), PayoutFilter (..))
import Language.Marlowe.Runtime.Web.Adapter.Links (
  WithLink (IncludeLink),
 )
import Language.Marlowe.Runtime.Web.Adapter.Pagination (
  PaginatedResponse,
 )
import Language.Marlowe.Runtime.Web.Adapter.Servant (
  ListObject (ListObject),
 )
import Language.Marlowe.Runtime.Web.Server.ApiError (badRequest', notFound', rangeNotSatisfiable')
import Language.Marlowe.Runtime.Web.Server.DTO (FromDTO (..), ToDTO (..), fromDTOThrow, fromPaginationRange)
import Language.Marlowe.Runtime.Web.Server.Monad (
  ServerM,
  loadPayoutL,
  loadPayoutsL, runEff2, runEff1,
 )
import Language.Marlowe.Runtime.Web.Payout.API (
  GetPayoutResponse,
  GetPayoutsResponse,
  PayoutHeader,
  PayoutStatus (..),
  PayoutsAPI,
 )

import Language.Marlowe.Runtime.Web.Core.Asset (AssetId)
import Language.Marlowe.Runtime.Web.Core.Tx (TxOutRef)
import Servant (
  HasServer (ServerT),
  Proxy (Proxy),
  addHeader,
  type (:<|>) ((:<|>)), Union,
 )
import Servant.Pagination (
  ExtractRange (extractRange),
  HasPagination (getDefaultRange),
  Range,
  Ranges,
  returnRange,
 )
import Language.Marlowe.Runtime.Web.Adapter.Servant.UVerbT (runUVerbT)
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Catch (MonadThrow(throwM))

server :: ServerT PayoutsAPI ServerM
server = get :<|> getOne

get
  :: [TxOutRef]
  -> [AssetId]
  -> Maybe PayoutStatus
  -> Maybe (Ranges '["payoutId"] GetPayoutsResponse)
  -> ServerM (Union '[ PaginatedResponse '["payoutId"] GetPayoutsResponse ])
get contractIds roleTokens status ranges = runUVerbT do
  let range :: Range "payoutId" TxOutRef
      range = fromMaybe (getDefaultRange (Proxy @PayoutHeader)) $ extractRange =<< ranges
  range' <- lift $ maybe (throwM $ rangeNotSatisfiable' "Invalid range value") pure $ fromPaginationRange range
  contractIds' <- lift $
    traverse
      ( \contractId -> maybe (throwM $ badRequest' $ "Invalid contractId value " <> show contractId) pure $ fromDTO contractId
      )
      contractIds
  roleTokens' <- lift $
    traverse
      (\assetId -> maybe (throwM $ badRequest' $ "Invalid contractId value " <> show assetId) pure $ fromDTO assetId)
      roleTokens
  let status' =
        status <&> \case
          Available -> False
          Withdrawn -> True
  let pFilter = PayoutFilter status' (Set.fromList contractIds') (Set.fromList roleTokens')
  runEff2 loadPayoutsL pFilter range' >>= \case
    Nothing -> lift $ throwM $ rangeNotSatisfiable' "Initial payout ID not found"
    Just Page{..} -> do
      let
        payouts :: [GetPayoutsResponse]
        payouts = IncludeLink (Proxy @"payout") <$> toDTO items

      response  <- returnRange range payouts
      let
        -- Without this type annotation compilation fails
        response' :: PaginatedResponse '["payoutId"] GetPayoutsResponse
        response' = addHeader @"Total-Count" totalCount $ ListObject <$> response
      pure response'

getOne :: TxOutRef -> ServerM (Union '[GetPayoutResponse])
getOne payoutId = runUVerbT do
  payoutId' <- lift $ fromDTOThrow (badRequest' "Invalid payout id value") payoutId
  runEff1 loadPayoutL payoutId' >>= \case
    Nothing -> lift $ throwM $ notFound' "Payout not found"
    Just result ->
      pure $
        IncludeLink (Proxy @"contract") $
          IncludeLink (Proxy @"transaction") $
            IncludeLink (Proxy @"withdrawal") $
              toDTO result
