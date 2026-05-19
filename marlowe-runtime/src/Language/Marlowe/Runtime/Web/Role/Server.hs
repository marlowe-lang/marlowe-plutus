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

import Language.Marlowe.Runtime.Web.Core.Tx (
  TxOutRef,
 )
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
  WalletAddresses (..),
 )
import Language.Marlowe.Runtime.Web.Server.Monad (
  ServerM,
  burnRoleTokensL, runEff2,
 )
import Language.Marlowe.Runtime.Web.Role.TokenFilter (RoleTokenFilter)
import Servant.Server (HasServer (ServerT))
import Language.Marlowe.Runtime.Web.Adapter.Servant.UVerbT (runUVerbT)
import Control.Monad.Trans.Class (MonadTrans(lift))

server :: ServerT RoleAPI ServerM
server = buildTx

buildTx
  :: RoleTokenFilter
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> ServerM (Union '[BurnRoleTokensTxEnvelope CardanoTx])
buildTx roleTokenFilterDTO changeAddressDTO usedAddressesDTO collateralsDTO = runUVerbT do
  walletAddresses <- lift $ toWalletAddress (changeAddressDTO, usedAddressesDTO, collateralsDTO)
  roleTokenFilter <- lift $ fromDTOThrow (badRequest' "Invalid Role Token Filter") roleTokenFilterDTO
  (txId, txEnvelope) <- runEff2 burnRoleTokensL walletAddresses roleTokenFilter >>= \case
    Left err -> lift $ throwDTOError err
    Right (BurnRoleTokensTx BabbageEraOnwardsBabbage BurnRoleTokensTxInEra{txBody}) -> do
      pure $ toDTO (fromCardanoTxId $ getTxId txBody, makeSignedTransaction [] txBody)
    Right (BurnRoleTokensTx BabbageEraOnwardsConway BurnRoleTokensTxInEra{txBody}) -> do
      pure $ toDTO (fromCardanoTxId $ getTxId txBody, makeSignedTransaction [] txBody)
    Right (BurnRoleTokensTx BabbageEraOnwardsDijkstra BurnRoleTokensTxInEra{txBody}) -> do
      pure $ toDTO (fromCardanoTxId $ getTxId txBody, makeSignedTransaction [] txBody)
  pure (BurnRoleTokensTxEnvelope txId txEnvelope :: BurnRoleTokensTxEnvelope CardanoTx)

type WalletAddressesDTO = (Address, Maybe (CommaList Address), Maybe (CommaList TxOutRef))

toWalletAddress :: WalletAddressesDTO -> ServerM WalletAddresses
toWalletAddress (changeAddressDTO, usedAddressesDTO, collateralsDTO) = do
  changeAddress <- fromDTOThrow (badRequest' "Invalid change address") changeAddressDTO
  extraAddresses <-
    Set.fromList <$> fromDTOThrow (badRequest' "Invalid addresses header value") (maybe [] unCommaList usedAddressesDTO)
  collateralUtxos <-
    Set.fromList <$> fromDTOThrow (badRequest' "Invalid collateral header UTxO value") (maybe [] unCommaList collateralsDTO)
  pure WalletAddresses{..}

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
