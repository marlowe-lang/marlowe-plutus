{-# LANGUAGE OverloadedLists #-}

module Language.Marlowe.Runtime.Web.Withdrawal.Server (server) where

import Cardano.Api (
  BabbageEraOnwards (..),
  getTxId,
  makeSignedTransaction,
 )
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Language.Marlowe.Runtime.Query as Query
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.Transaction.Api (WithdrawTx (..), WithdrawTxInEra (..))
import Language.Marlowe.Runtime.Web.Adapter.Links (WithLink (..))
import Language.Marlowe.Runtime.Web.Adapter.Pagination (
  PaginatedResponse,
 )
import Language.Marlowe.Runtime.Web.Adapter.Servant (ListObject (..))

import Language.Marlowe.Runtime.Web.Server.DTO (
  FromDTO (fromDTO),
  ToDTO (toDTO),
  fromDTOThrow,
  fromPaginationRange,
 )
import Language.Marlowe.Runtime.Web.Server.Monad (
  ServerM,
  loadWithdrawalL,
  loadWithdrawalsL,
  withdrawL, runEff1, runEff2,
 )
import Language.Marlowe.Runtime.Web.Adapter.CommaList (CommaList (..))
import Language.Marlowe.Runtime.Web.Core.Address (Address)
import Language.Marlowe.Runtime.Web.Core.Asset (PolicyId)

import Language.Marlowe.Runtime.Web.Server.ApiError (
  badRequest',
  notFound',
  rangeNotSatisfiable',
  throwDTOError,
 )
import Language.Marlowe.Runtime.Web.Core.Tx (TxBodyInAnyEra (..), TxId, TransactionUnspentOutput)
import Language.Marlowe.Runtime.Web.Tx.API (CardanoTx, WithdrawTxEnvelope (..))
import Language.Marlowe.Runtime.Web.Withdrawal.API (
  GetWithdrawalsResponse,
  PostWithdrawalsRequest (..),
  PostWithdrawalsResponse,
  Withdrawal (..),
  WithdrawalAPI,
  WithdrawalHeader (..),
  WithdrawalsAPI,
 )
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
import Control.Monad.Trans.Class (MonadTrans(lift))
import Language.Marlowe.Runtime.Web.Adapter.Servant.UVerbT (UVerbT, runUVerbT)
import qualified Cardano.Api as C
import Language.Marlowe.Runtime.Transaction.Constraints (WalletContext(WalletContext))
import Language.Marlowe.Runtime.ChainSync.Api (toUTxOTuple, fromUTxOsList)
import Control.Monad.Catch (MonadThrow(throwM))

server :: ServerT WithdrawalsAPI ServerM
server =
  get
    :<|> postCreateTxResponse
    :<|> withdrawalServer

postCreateTxBody
  :: PostWithdrawalsRequest
  -> Address
  -> CommaList (TransactionUnspentOutput C.ConwayEra)
  -> UVerbT
      '[PostWithdrawalsResponse CardanoTx]
      ServerM
      TxBodyInAnyEra
postCreateTxBody PostWithdrawalsRequest{..} changeAddressDTO availableUTxOsDTO = do
  changeAddress <- lift $ fromDTOThrow (badRequest' "Invalid change address value") changeAddressDTO
  availableUTxOs <- lift $ fromDTOThrow (badRequest' "Invalid wallet available UTxOs") (unCommaList availableUTxOsDTO)
  payouts' <- lift $ fromDTOThrow (badRequest' "Invalid payouts") payouts
  let
    walletContext = WalletContext
      (fromUTxOsList availableUTxOs)
      (Set.fromList $ map (fst . toUTxOTuple) availableUTxOs)
      changeAddress
  runEff2 withdrawL walletContext payouts' >>= \case
    Left err -> lift $ throwDTOError err
    Right (WithdrawTx BabbageEraOnwardsBabbage WithdrawTxInEra{txBody}) -> pure $ TxBodyInAnyEra txBody
    Right (WithdrawTx BabbageEraOnwardsConway WithdrawTxInEra{txBody}) -> pure $ TxBodyInAnyEra txBody
    Right (WithdrawTx BabbageEraOnwardsDijkstra WithdrawTxInEra{txBody}) -> pure $ TxBodyInAnyEra txBody

postCreateTxResponse
  :: PostWithdrawalsRequest
  -> Address
  -> CommaList (TransactionUnspentOutput C.ConwayEra)
  -> ServerM (Union '[PostWithdrawalsResponse CardanoTx])
postCreateTxResponse req changeAddressDTO availableUtxosDTO = runUVerbT do
  TxBodyInAnyEra txBody <- postCreateTxBody req changeAddressDTO availableUtxosDTO
  let
    tx = makeSignedTransaction [] txBody
    (withdrawalId, tx') = toDTO (fromCardanoTxId $ getTxId txBody, tx)
    body = WithdrawTxEnvelope withdrawalId tx'
    response :: PostWithdrawalsResponse CardanoTx
    response = IncludeLink (Proxy @"withdrawal") body
  pure response

get
  :: [PolicyId]
  -> Maybe (Ranges '["withdrawalId"] GetWithdrawalsResponse)
  -- -> ServerM (PaginatedResponse '["withdrawalId"] GetWithdrawalsResponse)
  -> ServerM (Union '[PaginatedResponse '["withdrawalId"] GetWithdrawalsResponse])
get roleCurrencies ranges = runUVerbT do
  let range :: Range "withdrawalId" TxId
      range = fromMaybe (getDefaultRange (Proxy @WithdrawalHeader)) $ extractRange =<< ranges
  range' <- maybe (lift $ throwM $ rangeNotSatisfiable' "Invalid range value") pure $ fromPaginationRange range
  roleCurrencies' <-
    traverse
      (\role -> maybe (lift $ throwM $ badRequest' $ "Invalid role value " <> show role) pure $ fromDTO role)
      roleCurrencies
  let wFilter = Query.WithdrawalFilter $ Set.fromList roleCurrencies'
  runEff2 loadWithdrawalsL wFilter range' >>= \case
    Nothing -> lift $ throwM $ rangeNotSatisfiable' "Initial withdrawal ID not found"
    Just Query.Page{..} -> do
      let
        withdrawalHeaders = IncludeLink (Proxy @"withdrawal") . toWithdrawalHeader <$> toDTO items
      response <- returnRange range withdrawalHeaders
      let
        response' :: PaginatedResponse '["withdrawalId"] GetWithdrawalsResponse
        response' = addHeader @"Total-Count" totalCount $ ListObject <$> response
      pure response'

toWithdrawalHeader :: Withdrawal -> WithdrawalHeader
toWithdrawalHeader Withdrawal{..} = WithdrawalHeader{..}

withdrawalServer :: TxId -> ServerT WithdrawalAPI ServerM
withdrawalServer = getOne -- :<|> submitWithdrawalTx withdrawalId

getOne
  :: TxId
  -> ServerM
      (Union '[Withdrawal])
getOne withdrawalId = runUVerbT do
  withdrawalId' <- lift $ fromDTOThrow (badRequest' "Invalid withdrawal id value") withdrawalId
  runEff1 loadWithdrawalL withdrawalId' >>= \case
    Nothing -> lift $ throwM $ notFound' "Withdrawal not found"
    Just result -> do
      pure $ toDTO result
--
-- submitWithdrawalTx :: TxId -> TextEnvelope -> ServerM NoContent
-- submitWithdrawalTx withdrawalId body = do
--   withdrawalId' <- fromDTOThrow (badRequest' "Invalid withdrawal id value") withdrawalId
--   loadWithdrawal withdrawalId' >>= \case
--     Nothing -> throwM $ notFound' "Withdrawal not found"
--     Just (Left (TempTx era _ Unsigned WithdrawTxInEra{txBody})) -> handleLoaded withdrawalId' era txBody
--     Just _ ->
--       throwM $
--         ApiError.toServerError $
--           ApiError "Withdrawal already submitted" "WithdrawalAlreadySubmitted" Null 409
--   where
--     handleLoaded :: Chain.TxId -> BabbageEraOnwards era -> TxBody era -> ServerM NoContent
--     handleLoaded withdrawalId' BabbageEraOnwardsBabbage txBody = do
--       (req :: Maybe (Either (Cardano.Tx BabbageEra) (ShelleyTxWitness BabbageEra))) <- case teType body of
--         "Tx BabbageEra" -> pure $ Left <$> fromDTO body
--         "ShelleyTxWitness BabbageEra" -> pure $ Right <$> fromDTO body
--         _ ->
--           throwM $ badRequest' "Unknown envelope type - allowed types are: \"Tx BabbageEra\", \"ShelleyTxWitness BabbageEra\""
-- 
--       tx <- case req of
--         Nothing -> throwM $ badRequest' "Invalid text envelope cbor value"
--         Just (Left tx) -> pure tx
--         Just (Right (ShelleyTxWitness (AlonzoTxWits wtKeys _ _ _ _))) -> pure $ makeSignedTxWithWitnessKeys txBody wtKeys
--       submitWithdrawal withdrawalId' BabbageEraOnwardsBabbage tx >>= \case
--         Nothing -> pure NoContent
--         Just err -> throwM $ ApiError.toServerError $ ApiError (show err) "SubmissionError" Null 403
--     handleLoaded withdrawalId' BabbageEraOnwardsConway txBody = do
--       (req :: Maybe (Either (Cardano.Tx ConwayEra) (ShelleyTxWitness ConwayEra))) <- case teType body of
--         "Tx ConwayEra" -> pure $ Left <$> fromDTO body
--         "ShelleyTxWitness ConwayEra" -> pure $ Right <$> fromDTO body
--         _ ->
--           throwM $ badRequest' "Unknown envelope type - allowed types are: \"Tx ConwayEra\", \"ShelleyTxWitness ConwayEra\""
-- 
--       tx <- case req of
--         Nothing -> throwM $ badRequest' "Invalid text envelope cbor value"
--         Just (Left tx) -> pure tx
--         Just (Right (ShelleyTxWitness (AlonzoTxWits wtKeys _ _ _ _))) -> pure $ makeSignedTxWithWitnessKeys txBody wtKeys
--       submitWithdrawal withdrawalId' BabbageEraOnwardsConway tx >>= \case
--         Nothing -> pure NoContent
--         Just err -> throwM $ ApiError.toServerError $ ApiError (show err) "SubmissionError" Null 403
