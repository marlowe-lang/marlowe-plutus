-- | This module defines a server for the /contracts REST API.
module Language.Marlowe.Runtime.Web.Contract.Server (server) where

import Language.Marlowe.Runtime.Web.Adapter.Servant.UVerbT (
  runUVerbT,
 )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash (..), Lovelace (..))
import Language.Marlowe.Runtime.Web.Adapter.Links (WithLink (..))
import Language.Marlowe.Runtime.Web.Server.DTO (
  ToDTO (toDTO),
  fromDTOThrow,
  )
import Language.Marlowe.Runtime.Web.Server.Monad (
  ServerM, loadContractL, runEff1, createContractL,
 )
import Language.Marlowe.Runtime.Web.Contract.API as Web (
  ContractId,
  ContractsAPI,
  GetContractResponse, ContractAPI, ContractOrSourceId (ContractOrSourceId), PostContractsResponse,
  PostContractsRequest (contract, threadTokenName, roles, accounts, metadata, tags, version, minUTxODeposit),
  unContractSourceId, minUTxODeposit
 )
import qualified Language.Marlowe.Runtime.Web.Contract.Next.Server as Next
import Language.Marlowe.Runtime.Web.Server.ApiError (
   badRequest',
   notFound',
   throwDTOError,
  )
import Servant (
  HasServer (ServerT),
  Proxy (Proxy),
  throwError,
  type (:<|>) ((:<|>)),
  Union,
 )
import Control.Monad.Trans.Class (MonadTrans(lift))
import qualified Language.Marlowe.Runtime.Web.Contract.Transaction.Server as Transactions
import Language.Marlowe.Runtime.Web.Core.Address (StakeAddress, Address)
import Language.Marlowe.Runtime.Web.Adapter.CommaList (CommaList (unCommaList))
import Language.Marlowe.Runtime.Web.Core.Tx as Core
import qualified Language.Marlowe.Runtime.Web.Core.Address as Core
import qualified Language.Marlowe.Runtime.Core.Api as Core
import Marlowe.Plutus.Analysis.Safety.Types (SafetyError)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(MarloweV1))
import Language.Marlowe.Runtime.Core.Api (SomeMarloweVersion(SomeMarloweVersion))
import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses(WalletAddresses, changeAddress, collateralUtxos, extraAddresses))
import Language.Marlowe.Runtime.Transaction.Api (ContractCreated(ContractCreated))
import qualified Cardano.Api as C
import Language.Marlowe.Runtime.Transaction.Api (ContractCreatedInEra(ContractCreatedInEra, contractId, txBody, safetyErrors))
import Language.Marlowe.Runtime.Web.Tx.API (CardanoTx, CreateTxEnvelope (CreateTxEnvelope))
import Control.Lens (view)

server :: ServerT ContractsAPI ServerM
server =
  contractServer
  :<|> postCreateTxResponse

contractServer :: ContractId -> ServerT ContractAPI ServerM
contractServer contractId =
  getOne contractId
  :<|> Next.server contractId
  :<|> Transactions.server contractId

getOne
  :: ContractId
  -> ServerM (Union '[ GetContractResponse ])
getOne contractId = runUVerbT do
  contractId' <- lift $ fromDTOThrow (badRequest' "Invalid contract id value") contractId
  runEff1 loadContractL contractId' >>= \case
    Nothing -> throwError $ notFound' "Contract not found"
    Just result -> do
      let contractState = either toDTO toDTO result
      pure $ IncludeLink (Proxy @"transactions") contractState

postCreateTxBody
  :: PostContractsRequest
  -> Maybe Core.StakeAddress
  -> Core.Address
  -> Maybe (CommaList Core.Address)
  -> Maybe (CommaList Core.TxOutRef)
  -> ServerM (Core.ContractId, TxBodyInAnyEra, [SafetyError])
postCreateTxBody req stakeAddressDTO changeAddressDTO mAddresses mCollateralUtxos = do
  SomeMarloweVersion _v@MarloweV1 <- fromDTOThrow (badRequest' "Unsupported Marlowe version") req.version
  stakeAddress <- fromDTOThrow (badRequest' "Invalid stake address value") stakeAddressDTO
  changeAddress <- fromDTOThrow (badRequest' "Invalid change address value") changeAddressDTO
  extraAddresses <-
    Set.fromList <$> fromDTOThrow (badRequest' "Invalid addresses header value") (maybe [] unCommaList mAddresses)
  collateralUtxos <-
    Set.fromList
      <$> fromDTOThrow (badRequest' "Invalid collateral header UTxO value") (maybe [] unCommaList mCollateralUtxos)
  threadTokenName' <- fromDTOThrow (badRequest' "Invalid thread token name") req.threadTokenName
  roles' <- fromDTOThrow (badRequest' "Invalid roles value") req.roles
  accounts' <- fromDTOThrow (badRequest' "Invalid initial accounts") req.accounts
  transactionMetadata <- fromDTOThrow (badRequest' "Invalid metadata value") req.metadata
  marloweMetadata <-
    fromDTOThrow
      (badRequest' "Invalid tags value")
      if Map.null req.tags then Nothing else Just req.tags
  let
    ContractOrSourceId contract' = req.contract
    walletAddresses = WalletAddresses { changeAddress, extraAddresses, collateralUtxos }
    marloweTransactionMetadata = Core.MarloweTransactionMetadata{marloweMetadata, transactionMetadata}
  createContract <- view createContractL
  createContract
    stakeAddress
    walletAddresses
    threadTokenName'
    roles'
    marloweTransactionMetadata
    (Lovelace <$> req.minUTxODeposit)
    accounts'
    (DatumHash . unContractSourceId <$> contract')
    >>= \case
      Left err -> throwDTOError err
      Right
        (ContractCreated C.BabbageEraOnwardsBabbage ContractCreatedInEra{contractId, txBody, safetyErrors}) -> pure (contractId, TxBodyInAnyEra txBody, safetyErrors)
      Right
        (ContractCreated C.BabbageEraOnwardsConway ContractCreatedInEra{contractId, txBody, safetyErrors}) -> pure (contractId, TxBodyInAnyEra txBody, safetyErrors)
      Right
        (ContractCreated C.BabbageEraOnwardsDijkstra ContractCreatedInEra{contractId, txBody, safetyErrors}) -> pure (contractId, TxBodyInAnyEra txBody, safetyErrors)

postCreateTxResponse
  :: Maybe StakeAddress
  -> PostContractsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> ServerM (Union '[PostContractsResponse CardanoTx])
postCreateTxResponse stakeAddressDTO req changeAddressDTO mAddresses mCollateralUtxos = runUVerbT do
  (contractId, TxBodyInAnyEra txBody, safetyErrors) <- lift $
    postCreateTxBody req stakeAddressDTO changeAddressDTO mAddresses mCollateralUtxos
  let tx = C.makeSignedTransaction [] txBody
  let (contractId', tx') = toDTO (contractId, tx)
  let body = CreateTxEnvelope contractId' tx' safetyErrors
  pure (IncludeLink (Proxy @"contract") body :: PostContractsResponse CardanoTx)


