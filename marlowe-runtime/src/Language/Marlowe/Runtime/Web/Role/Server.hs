{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Web.Role.Server (
  server,
) where

import qualified Data.Set as Set
import Language.Marlowe.Runtime.Web.Adapter.CommaList (CommaList (..))
import Language.Marlowe.Runtime.Web.Server.ApiError (
  ApiError (..),
  badRequest',
  constraintErrorToApiError,
  throwDTOError,
 )
import Language.Marlowe.Runtime.Web.Server.DTO (
  HasDTO (..),
  ToDTO (..),
  fromDTOThrow,
 )
import Language.Marlowe.Runtime.Web.Core.Address (Address)

import Language.Marlowe.Runtime.Web.Core.Tx (TransactionUnspentOutput)
import Language.Marlowe.Runtime.Web.Role.API (BurnRoleTokensTxEnvelope (..), RoleAPI)
import Language.Marlowe.Runtime.Web.Tx.API (
  CardanoTx,
 )
import Servant (Union)

import Cardano.Api (BabbageEraOnwards (..), getTxId, makeSignedTransaction)
import Data.Aeson (ToJSON (toJSON), Value (Null))
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.Transaction.Api (
  BurnRoleTokensError (..),
  BurnRoleTokensTx (BurnRoleTokensTx),
  BurnRoleTokensTxInEra (..),
 )
import Language.Marlowe.Runtime.Web.Server.Monad (
  ServerM,
  burnRoleTokensL, runEff2,
 )
import Language.Marlowe.Runtime.Web.Role.TokenFilter (RoleTokenFilter)
import Servant.Server (HasServer (ServerT))
import Language.Marlowe.Runtime.Web.Adapter.Servant.UVerbT (runUVerbT)
import Control.Monad.Trans.Class (MonadTrans(lift))
import qualified Cardano.Api as C
import Language.Marlowe.Runtime.ChainSync.Api (toUTxOTuple, fromUTxOsList)
import Language.Marlowe.Runtime.Transaction.Constraints (WalletContext(WalletContext))

server :: ServerT RoleAPI ServerM
server = buildTx

buildTx
  :: RoleTokenFilter
  -> Address
  -> CommaList (TransactionUnspentOutput C.ConwayEra)
  -> ServerM (Union '[BurnRoleTokensTxEnvelope CardanoTx])
buildTx roleTokenFilterDTO changeAddressDTO availableUTxOsDTO = runUVerbT $ do
  changeAddress <- lift $ fromDTOThrow (badRequest' "Invalid change address") changeAddressDTO
  availableUTxOs <- lift $ fromDTOThrow (badRequest' "Invalid available UTxOs") (unCommaList availableUTxOsDTO)
  roleTokenFilter <- lift $ fromDTOThrow (badRequest' "Invalid Role Token Filter") roleTokenFilterDTO
  let
    walletContext = WalletContext
      (fromUTxOsList availableUTxOs)
      (Set.fromList $ map (fst . toUTxOTuple) availableUTxOs)
      changeAddress
  (txId, txEnvelope) <- runEff2 burnRoleTokensL walletContext roleTokenFilter >>= \case
    Left err -> lift $ throwDTOError err
    Right (BurnRoleTokensTx BabbageEraOnwardsBabbage BurnRoleTokensTxInEra{txBody}) -> do
      pure $ toDTO (fromCardanoTxId $ getTxId txBody, makeSignedTransaction [] txBody)
    Right (BurnRoleTokensTx BabbageEraOnwardsConway BurnRoleTokensTxInEra{txBody}) -> do
      pure $ toDTO (fromCardanoTxId $ getTxId txBody, makeSignedTransaction [] txBody)
    Right (BurnRoleTokensTx BabbageEraOnwardsDijkstra BurnRoleTokensTxInEra{txBody}) -> do
      pure $ toDTO (fromCardanoTxId $ getTxId txBody, makeSignedTransaction [] txBody)
  pure (BurnRoleTokensTxEnvelope txId txEnvelope :: BurnRoleTokensTxEnvelope CardanoTx)

instance HasDTO BurnRoleTokensError where
  type DTO BurnRoleTokensError = ApiError

instance ToDTO BurnRoleTokensError where
  toDTO = \case
    BurnEraUnsupported era -> ApiError ("Current network era not supported: " <> show era) "BurnEraUnsupported" Null 503
    BurnRolesActive roles -> ApiError "Active roles detected, refusing to burn" "BurnRolesActive" (toJSON roles) 400
    BurnInvalidPolicyId policyIds -> ApiError "Invalid policies" "BurnInvalidPolicyId" (toJSON policyIds) 400
    BurnNoTokens -> ApiError "No tokens to burn" "BurnNoTokensToBurn" Null 400
    BurnFromCardanoError -> ApiError "Internal error" "BurnFromCardanoError" Null 400
    BurnConstraintError err -> constraintErrorToApiError err
