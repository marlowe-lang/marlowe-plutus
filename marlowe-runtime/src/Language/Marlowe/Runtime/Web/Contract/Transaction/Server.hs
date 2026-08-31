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
import Language.Marlowe.Runtime.Transaction.Api (InputsApplied (..), InputsAppliedInEra (..))
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
import Language.Marlowe.Runtime.Web.Core.Tx (TxBodyInAnyEra (..), TxId, TxOutRef, TransactionUnspentOutput)

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
import qualified Cardano.Api as C
import Language.Marlowe.Runtime.ChainSync.Api (fromUTxOsList, toUTxOTuple)
import Language.Marlowe.Runtime.Transaction.Constraints (WalletContext(WalletContext))
import Control.Monad.Catch (MonadThrow(throwM))

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
  range' <- lift $ maybe (throwM $ rangeNotSatisfiable' "Invalid range value") pure $ fromPaginationRange range
  runEff2 loadTransactionsL contractId' range' >>= \case
    Left ContractNotFound -> lift $ throwM $ notFound' "Contract not found"
    Left TxNotFound -> lift $ throwM $ rangeNotSatisfiable' "Starting transaction not found"
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
  -> CommaList (TransactionUnspentOutput C.ConwayEra)
  -> ServerM (Union '[PostTransactionsResponse CardanoTx])
buildApplyInputTx contractId req changeAddressDTO availableUTxOsDTO = runUVerbT do
  (TxBodyInAnyEra txBody, safetyErrors) <- buildApplyInputTxBody' contractId req changeAddressDTO availableUTxOsDTO
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
  -> CommaList (TransactionUnspentOutput C.ConwayEra)
  -> UVerbT
      '[PostTransactionsResponse CardanoTx]
      ServerM
      ApplyInputsResult
buildApplyInputTxBody' contractId PostTransactionsRequest{..} changeAddressDTO availableUTxOsDTO = do
  SomeMarloweVersion _v@MarloweV1 <- lift $ fromDTOThrow (badRequest' "Invalid Marlowe version") version
  changeAddress <- lift $ fromDTOThrow (badRequest' "Invalid change address") changeAddressDTO
  availableUTxOs <- lift $ fromDTOThrow (badRequest' "Invalid collateral header UTxO value") (unCommaList availableUTxOsDTO)
  contractId' <- lift $ fromDTOThrow (badRequest' "Invalid contract id value") contractId
  transactionMetadata <- lift $ fromDTOThrow (badRequest' "Invalid metadata value") metadata
  marloweMetadata <- lift $ fromDTOThrow
      (badRequest' "Invalid tags value")
      if Map.null tags then Nothing else Just tags
  applyInputs <- view applyInputsL
  let
    walletContext = WalletContext
      (fromUTxOsList availableUTxOs)
      (Set.fromList $ map (fst . toUTxOTuple) availableUTxOs)
      changeAddress
  lift (applyInputs walletContext contractId' MarloweTransactionMetadata{..} invalidBefore invalidHereafter inputs) >>= \case
    Left err -> lift $ throwDTOError err
    Right (InputsApplied BabbageEraOnwardsBabbage InputsAppliedInEra{txBody, safetyErrors}) -> pure (TxBodyInAnyEra txBody, safetyErrors)
    Right (InputsApplied BabbageEraOnwardsConway InputsAppliedInEra{txBody, safetyErrors}) -> pure (TxBodyInAnyEra txBody, safetyErrors)
    Right (InputsApplied BabbageEraOnwardsDijkstra InputsAppliedInEra{txBody, safetyErrors}) -> pure (TxBodyInAnyEra txBody, safetyErrors)

getTransaction :: TxOutRef -> TxId -> ServerM (Union '[GetTransactionResponse])
getTransaction contractId txId = runUVerbT do
  contractId' <- lift $ fromDTOThrow (badRequest' "Invalid contract id value") contractId
  txId' <- lift $ fromDTOThrow (badRequest' "Invalid transaction id value") txId
  runEff2 loadTransactionL contractId' txId' >>= \case
    Nothing -> lift $ throwM $ notFound' "Transaction not found"
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
--     Nothing -> throwM $ notFound' "Transaction not found"
--     Just (Left (TempTx era _ Unsigned InputsAppliedInEra{txBody})) -> handleLoaded contractId' txId' era txBody
--     Just _ ->
--       throwM $
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
--           throwM $ badRequest' "Unknown envelope type - allowed types are: \"Tx BabbageEra\", \"ShelleyTxWitness BabbageEra\""
-- 
--       tx <- case req of
--         Nothing -> throwM $ badRequest' "Invalid text envelope cbor value"
--         Just (Left tx) -> pure tx
--         Just (Right (ShelleyTxWitness (AlonzoTxWits wtKeys _ _ _ _))) -> pure $ makeSignedTxWithWitnessKeys txBody wtKeys
--       submitTransaction contractId' txId' BabbageEraOnwardsBabbage tx >>= \case
--         Nothing -> pure NoContent
--         Just err -> throwM $ ApiError.toServerError $ ApiError (show err) "SubmissionError" Null 403
--     handleLoaded contractId' txId' BabbageEraOnwardsConway txBody = do
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
--       submitTransaction contractId' txId' BabbageEraOnwardsConway tx >>= \case
--         Nothing -> pure NoContent
--         Just err -> throwM $ ApiError.toServerError $ ApiError (show err) "SubmissionError" Null 403
