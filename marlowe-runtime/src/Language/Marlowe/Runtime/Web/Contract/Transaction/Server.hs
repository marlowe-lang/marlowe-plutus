-- | This module defines a server for the /contracts/:contractId/transactions REST API.
module Language.Marlowe.Runtime.Web.Contract.Transaction.Server (server) where

import Cardano.Api (BabbageEraOnwards (..), getTxId, makeSignedTransaction)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Language.Marlowe.Runtime.Query as Query
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.Core.Api (
  MarloweTransactionMetadata (MarloweTransactionMetadata),
  MarloweVersion (..),
  SomeMarloweVersion (..),
 )
import qualified Language.Marlowe.Runtime.Core.Api as Core
import Language.Marlowe.Runtime.Transaction.Api (InputsApplied (..), InputsAppliedInEra (..), WalletAddresses (..))
import Language.Marlowe.Runtime.Web.Adapter.CommaList (
  CommaList (unCommaList),
 )
import Language.Marlowe.Runtime.Web.Adapter.Links (WithLink (..))
import Language.Marlowe.Runtime.Web.Adapter.Pagination (
  PaginatedResponse,
 )
import Language.Marlowe.Runtime.Web.Adapter.Servant (ListObject (..))
import Language.Marlowe.Runtime.Web.Server.ApiError (
  badRequest',
  notFound',
  rangeNotSatisfiable',
  throwDTOError,
 )
import Language.Marlowe.Runtime.Web.Server.DTO (
  ToDTO (toDTO),
  fromDTOThrow,
  fromPaginationRange,
 )
import Language.Marlowe.Runtime.Web.Server.Monad (
  ServerM,
  LoadTxError (..),
  applyInputsL,
  loadTransactionL,
  loadTransactionsL, runEff2,
 )
-- import Language.Marlowe.Runtime.Web.Adapter.Server.SyncClient (LoadTxError (..))
-- import Language.Marlowe.Runtime.Web.Adapter.Server.TxClient (TempTx (TempTx), TempTxStatus (..))
import Language.Marlowe.Runtime.Web.Contract.Transaction.API (
  GetTransactionResponse,
  GetTransactionsAPI,
  GetTransactionsResponse,
  PostTransactionsAPI,
  PostTransactionsResponse,
  TransactionAPI,
  TransactionsAPI,
 )
import Language.Marlowe.Runtime.Web.Core.Address (Address)
import Language.Marlowe.Runtime.Web.Core.Tx (TxBodyInAnyEra (..), TxId, TxOutRef)

import Marlowe.Plutus.Analysis.Safety.Types
import Language.Marlowe.Runtime.Web.Tx.API (
  ApplyInputsTxEnvelope (ApplyInputsTxEnvelope),
  CardanoTx,
  TxHeader,
 )
import Language.Marlowe.Runtime.Web.Withdrawal.API (
  PostTransactionsRequest (..),
 )
import Servant (
  HasServer (ServerT),
  Proxy (Proxy),
  Union,
  addHeader,
  throwError,
  type (:<|>) ((:<|>)),
 )
import Servant.Pagination (
  ExtractRange (extractRange),
  HasPagination (getDefaultRange),
  Range,
  Ranges,
  returnRange,
 )
import Control.Monad.Writer (MonadTrans(lift))
import Language.Marlowe.Runtime.Web.Adapter.Servant.UVerbT (runUVerbT, UVerbT)
import Control.Lens.Combinators (view)

server :: TxOutRef -> ServerT TransactionsAPI ServerM
server contractId =
  getTransactionsAPI
    :<|> postTransactionsAPI
    :<|> transactionAPI
  where
    getTransactionsAPI :: ServerT GetTransactionsAPI ServerM
    getTransactionsAPI = getTransactionsByContractId contractId

    postTransactionsAPI :: ServerT PostTransactionsAPI ServerM
    postTransactionsAPI = buildApplyInputTx contractId

    transactionAPI :: TxId -> ServerT TransactionAPI ServerM
    transactionAPI = getTransaction contractId

getTransactionsByContractId
  :: TxOutRef
  -> Maybe (Ranges '["transactionId"] GetTransactionsResponse)
  -> ServerM (Union '[PaginatedResponse '["transactionId"] GetTransactionsResponse])
getTransactionsByContractId contractId ranges = runUVerbT do
  let range :: Range "transactionId" TxId
      range = fromMaybe (getDefaultRange (Proxy @TxHeader)) $ extractRange =<< ranges
  contractId' <- lift $ fromDTOThrow (badRequest' "Invalid contract id value") contractId
  range' <- lift $ maybe (throwError $ rangeNotSatisfiable' "Invalid range value") pure $ fromPaginationRange range
  runEff2 loadTransactionsL contractId' range' >>= \case
    Left ContractNotFound -> lift $ throwError $ notFound' "Contract not found"
    Left TxNotFound -> lift $ throwError $ rangeNotSatisfiable' "Starting transaction not found"
    Right Query.Page{..} -> do
      let
        txHeaders :: [GetTransactionsResponse]
        txHeaders = IncludeLink (Proxy @"transaction") <$> toDTO items
      response <- returnRange range txHeaders
      let
        response' :: PaginatedResponse '["transactionId"] GetTransactionsResponse
        response' = addHeader @"Total-Count" totalCount $ ListObject <$> response
      pure response'

buildApplyInputTx
  :: TxOutRef
  -> PostTransactionsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> ServerM (Union '[PostTransactionsResponse CardanoTx])
buildApplyInputTx contractId req changeAddressDTO mAddresses mCollateralUtxos = runUVerbT do
  (TxBodyInAnyEra txBody, safetyErrors) <- buildApplyInputTxBody' contractId req changeAddressDTO mAddresses mCollateralUtxos
  let
    response :: PostTransactionsResponse CardanoTx
    response =
      IncludeLink (Proxy @"transaction") $
        ApplyInputsTxEnvelope
          contractId
          (toDTO $ fromCardanoTxId $ getTxId txBody)
          (toDTO $ makeSignedTransaction [] txBody)
          safetyErrors
  pure response

type ApplyInputsResult = (TxBodyInAnyEra, [SafetyError])

buildApplyInputTxBody'
  :: TxOutRef
  -> PostTransactionsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> UVerbT
      '[PostTransactionsResponse CardanoTx]
      ServerM
      ApplyInputsResult
buildApplyInputTxBody' contractId PostTransactionsRequest{..} changeAddressDTO mAddresses mCollateralUtxos = do
  SomeMarloweVersion _v@MarloweV1 <- fromDTOThrow (badRequest' "Invalid Marlowe version") version
  changeAddress <- fromDTOThrow (badRequest' "Invalid change address") changeAddressDTO
  extraAddresses <-
    Set.fromList <$> fromDTOThrow (badRequest' "Invalid addresses header value") (maybe [] unCommaList mAddresses)
  collateralUtxos <-
    Set.fromList
      <$> fromDTOThrow (badRequest' "Invalid collateral header UTxO value") (maybe [] unCommaList mCollateralUtxos)
  contractId' <- fromDTOThrow (badRequest' "Invalid contract id value") contractId
  transactionMetadata <- fromDTOThrow (badRequest' "Invalid metadata value") metadata
  marloweMetadata <-
    fromDTOThrow
      (badRequest' "Invalid tags value")
      if Map.null tags then Nothing else Just tags
  applyInputs <- view applyInputsL
  lift (applyInputs WalletAddresses{..} contractId' MarloweTransactionMetadata{..} invalidBefore invalidHereafter inputs) >>= \case
    Left err -> throwDTOError err
    Right (InputsApplied BabbageEraOnwardsBabbage InputsAppliedInEra{txBody, safetyErrors}) -> pure (TxBodyInAnyEra txBody, safetyErrors)
    Right (InputsApplied BabbageEraOnwardsConway InputsAppliedInEra{txBody, safetyErrors}) -> pure (TxBodyInAnyEra txBody, safetyErrors)
    Right (InputsApplied BabbageEraOnwardsDijkstra InputsAppliedInEra{txBody, safetyErrors}) -> pure (TxBodyInAnyEra txBody, safetyErrors)

getTransaction :: TxOutRef -> TxId -> ServerM (Union '[GetTransactionResponse])
getTransaction contractId txId = runUVerbT do
  contractId' <- lift $ fromDTOThrow (badRequest' "Invalid contract id value") contractId
  txId' <- lift $ fromDTOThrow (badRequest' "Invalid transaction id value") txId
  runEff2 loadTransactionL contractId' txId' >>= \case
    Nothing -> throwError $ notFound' "Transaction not found"
    Just contractState -> do
      let
        result :: GetTransactionResponse
        result =
          IncludeLink (Proxy @"previous") $
            IncludeLink (Proxy @"next") $ toDTO contractState
      pure result

-- submitApplyInputTx :: TxOutRef -> TxId -> TextEnvelope -> ServerM NoContent
-- submitApplyInputTx contractId txId body = do
--   contractId' <- fromDTOThrow (badRequest' "Invalid contract id value") contractId
--   txId' <- fromDTOThrow (badRequest' "Invalid transaction id value") txId
--   loadTransaction contractId' txId' >>= \case
--     Nothing -> throwError $ notFound' "Transaction not found"
--     Just (Left (TempTx era _ Unsigned InputsAppliedInEra{txBody})) -> handleLoaded contractId' txId' era txBody
--     Just _ ->
--       throwError $
--         ApiError.toServerError $
--           ApiError "Transaction already submitted" "ContractAlreadySubmitted" Null 409
--   where
--     handleLoaded
--       :: Core.ContractId -> Chain.TxId -> BabbageEraOnwards era -> TxBody era -> ServerM NoContent
--     handleLoaded contractId' txId' BabbageEraOnwardsBabbage txBody = do
--       (req :: Maybe (Either (Cardano.Tx BabbageEra) (ShelleyTxWitness BabbageEra))) <- case teType body of
--         "Tx BabbageEra" -> pure $ Left <$> fromDTO body
--         "ShelleyTxWitness BabbageEra" -> pure $ Right <$> fromDTO body
--         _ ->
--           throwError $ badRequest' "Unknown envelope type - allowed types are: \"Tx BabbageEra\", \"ShelleyTxWitness BabbageEra\""
-- 
--       tx <- case req of
--         Nothing -> throwError $ badRequest' "Invalid text envelope cbor value"
--         Just (Left tx) -> pure tx
--         Just (Right (ShelleyTxWitness (AlonzoTxWits wtKeys _ _ _ _))) -> pure $ makeSignedTxWithWitnessKeys txBody wtKeys
--       submitTransaction contractId' txId' BabbageEraOnwardsBabbage tx >>= \case
--         Nothing -> pure NoContent
--         Just err -> throwError $ ApiError.toServerError $ ApiError (show err) "SubmissionError" Null 403
--     handleLoaded contractId' txId' BabbageEraOnwardsConway txBody = do
--       (req :: Maybe (Either (Cardano.Tx ConwayEra) (ShelleyTxWitness ConwayEra))) <- case teType body of
--         "Tx ConwayEra" -> pure $ Left <$> fromDTO body
--         "ShelleyTxWitness ConwayEra" -> pure $ Right <$> fromDTO body
--         _ ->
--           throwError $ badRequest' "Unknown envelope type - allowed types are: \"Tx ConwayEra\", \"ShelleyTxWitness ConwayEra\""
-- 
--       tx <- case req of
--         Nothing -> throwError $ badRequest' "Invalid text envelope cbor value"
--         Just (Left tx) -> pure tx
--         Just (Right (ShelleyTxWitness (AlonzoTxWits wtKeys _ _ _ _))) -> pure $ makeSignedTxWithWitnessKeys txBody wtKeys
--       submitTransaction contractId' txId' BabbageEraOnwardsConway tx >>= \case
--         Nothing -> pure NoContent
--         Just err -> throwError $ ApiError.toServerError $ ApiError (show err) "SubmissionError" Null 403
