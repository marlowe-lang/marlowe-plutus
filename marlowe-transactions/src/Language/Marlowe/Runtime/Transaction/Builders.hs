-- FIXME: Drop this pragma when porting is finished:
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Language.Marlowe.Runtime.Transaction.Builders where

import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Cardano.Api as C
import Log (MonadLog)
import Language.Marlowe.Runtime.Transaction.BuildConstraints (MkRoleTokenMintingPolicy, MinAdaProvider (MinAdaProvider), initialMarloweState, invalidAddressesError, RolesPolicyId (RolesPolicyId), buildInitConstraints)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion (MarloweV1), MarloweTransactionMetadata, IsMarloweVersion (Contract), ContractId (ContractId), decodeMarloweTransactionMetadataLenient)
import Language.Marlowe.Runtime.Core.ScriptRegistry (MarloweScripts (MarloweScripts, marloweScript, payoutScript, helperScripts, marloweScriptUTxOs, payoutScriptUTxOs, helperScriptUTxOs), HelperScript (OpenRoleScript), ReferenceScriptUtxo, GetCurrentScripts)
import Language.Marlowe.Runtime.Transaction.Constraints (SolveConstraints, WalletContext (changeAddress), HelpersContext, MarloweContext (MarloweContext, scriptOutput, marloweAddress, payoutAddress, marloweScriptUTxO, payoutScriptUTxO, marloweScriptHash, payoutScriptHash))
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses(changeAddress), RoleTokensConfig (RoleTokensNone, RoleTokensMint, RoleTokensUsePolicy), Accounts, InitError(InitEraUnsupported, InitContractNotFound, ProtocolParamNoUTxOCostPerByte, InsufficientMinAdaDeposit, InitLoadMarloweContextFailed, InitToCardanoError, InitLoadHelpersContextFailed, InitSafetyAnalysisError, InitSafetyAnalysisFailed, InitConstraintError, InitTxOutputNotFound), ContractInitd(ContractInitd), LoadHelpersContextError, Mint (unMint), Destination (ToScript), MintRole (roleTokenRecipients), LoadMarloweContextError (MarloweScriptNotPublished, PayoutScriptNotPublished), unAccounts, ContractInitdInEra (ContractInitdInEra,
          contractId , rolesCurrency , metadata , txBody , marloweScriptHash , marloweScriptAddress , payoutScriptHash , payoutScriptAddress , version , datum , assets , safetyErrors))

import Control.Monad.Trans.Except (runExceptT, ExceptT(ExceptT), throwE, except, withExceptT)
import Data.Maybe (fromMaybe)
import Language.Marlowe.Runtime.Transaction.Safety (
  mkAdjustMinUTxO, ThreadTokenAssetId (ThreadTokenAssetId), noContinuations, minAdaUpperBound, checkContract, mockLockedRolesContext,
  checkTransactions,
  )
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad (guard, unless)
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Map.Strict as Map
import Data.Bifunctor (first)
import Control.Error (note)
import Language.Marlowe.Runtime.Cardano.Api (toCardanoPaymentCredential, fromCardanoAddressInEra, toCardanoStakeCredential, fromCardanoTxId)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Marlowe.Plutus.Analysis.Safety.Types (SafetyError (SafetyAnalysisTimeout))
import Control.Exception (SomeException, catch)
import qualified Control.Exception.Base as Exception
import qualified Control.DeepSeq as DeepSeq
import Control.Concurrent.Async (race)
import Control.Concurrent (threadDelay)
import Language.Marlowe.Runtime.ChainSync.Api (fromCardanoTxMetadata)
import qualified Data.List as List

type LoadWalletContext m = WalletAddresses -> m WalletContext

type LoadHelpersContext m =
  forall v
   . MarloweVersion v
  -> Either (Chain.PolicyId, RoleTokensConfig) (Maybe ContractId)
  -> m (Either LoadHelpersContextError HelpersContext)

execInit
  :: forall era m v
   . (MonadUnliftIO m, C.IsCardanoEra era, MonadLog m)
  => MkRoleTokenMintingPolicy m
  -> C.CardanoEra era
  -- -> Connector (QueryClient ContractRequest) m
  -> GetCurrentScripts v
  -> SolveConstraints era v
  -> C.LedgerProtocolParameters era
  -> LoadWalletContext m
  -> LoadHelpersContext m
  -> C.NetworkId
  -> Maybe Chain.StakeCredential
  -> MarloweVersion v
  -> WalletAddresses
  -> Maybe Chain.TokenName
  -> RoleTokensConfig
  -> MarloweTransactionMetadata
  -> Maybe Chain.Lovelace
  -> Accounts
  -> Either (Contract v) Chain.DatumHash
  -> NominalDiffTime
  -> m (Either InitError (ContractInitd v))
execInit mkRoleTokenMintingPolicy era getCurrentScripts solveConstraints protocolParameters loadWalletContext loadHelpersContext networkId mStakeCredential version addresses threadRole roleTokens metadata optMinAda accounts contract analysisTimeout = runExceptT do
  eon <- toBabbageEraOnwards (InitEraUnsupported $ C.AnyCardanoEra era) era
  let
    threadRole' = fromMaybe "" threadRole
    adjustMinUtxo = mkAdjustMinUTxO eon protocolParameters version
  walletContext <- lift $ loadWalletContext addresses
  dummyState <- do
    except $
      first invalidAddressesError $
        initialMarloweState
          adjustMinUtxo
          version
          accounts
          ( ThreadTokenAssetId (Chain.AssetId "00000000000000000000000000000000000000000000000000000000" threadRole') <$ guard case roleTokens of
              RoleTokensNone -> False
              RoleTokensMint (unMint -> mint) -> any (NEMap.member (ToScript OpenRoleScript) . roleTokenRecipients) mint
              RoleTokensUsePolicy _ distribution -> any (Map.member (ToScript OpenRoleScript)) distribution
          )
          (fromMaybe mempty optMinAda)
          (MinAdaProvider walletContext.changeAddress)

  (contract', continuations) <- case contract of
    Right hash -> case version of
      MarloweV1 -> throwE (InitContractNotFound "Not ported yet")
        -- let getContract' = MaybeT . runConnector contractQueryConnector . getContract
        -- Contract.ContractWithAdjacency{contract = c, ..} <- getContract' hash
        -- (c :: Contract v,) <$> foldMapM (fmap singletonContinuations . getContract') (Set.delete hash closure)
    Left c -> pure (c, noContinuations version)
  computedMinAdaDeposit <-
    except $
      note ProtocolParamNoUTxOCostPerByte $
        minAdaUpperBound eon protocolParameters version dummyState contract' continuations
  let minAda = fromMaybe computedMinAdaDeposit optMinAda
  unless (minAda >= computedMinAdaDeposit) $ throwE $ InsufficientMinAdaDeposit computedMinAdaDeposit
  ((datum, assets, RolesPolicyId rolesCurrency), constraints) <-
    ExceptT $ do
      buildInitConstraints
        mkRoleTokenMintingPolicy
        eon
        version
        walletContext
        threadRole'
        roleTokens
        metadata
        minAda
        accounts
        adjustMinUtxo
        contract'
  marloweContext <-
    mkMarloweContext
      networkId
      version
      getCurrentScripts
      mStakeCredential
  helpersContext <-
    withExceptT InitLoadHelpersContextFailed $
      ExceptT $
        loadHelpersContext version $
          Left (rolesCurrency, roleTokens)
  let -- Fast analysis of safety: examines bounds for transactions.
      contractSafetyErrors = checkContract networkId (Just roleTokens) version datum continuations

  transactionSafetyErrors <- do
    let threadTokenAssetId = ThreadTokenAssetId (Chain.AssetId rolesCurrency threadRole')
        lockedRolesContext = mockLockedRolesContext threadTokenAssetId adjustMinUtxo helpersContext
    ExceptT $
      liftIO $
        fmap (first InitSafetyAnalysisError) . limitAnalysisTime analysisTimeout $
          checkTransactions
            protocolParameters
            eon
            version
            marloweContext
            lockedRolesContext
            addresses.changeAddress
            datum
            continuations
  let safetyErrors = contractSafetyErrors <> transactionSafetyErrors
  unless (Map.null (unAccounts accounts) || null safetyErrors) do
    throwE $ InitSafetyAnalysisFailed safetyErrors
  txBody <-
    except $
      first InitConstraintError $
        solveConstraints
          eon
          (C.unLedgerProtocolParameters protocolParameters)
          version
          (Left marloweContext)
          walletContext
          helpersContext
          constraints
  contractOutput <- except . note InitTxOutputNotFound
    $ findMarloweOutput marloweContext.marloweAddress txBody
  pure $
    ContractInitd eon $
      ContractInitdInEra
        { contractId = ContractId contractOutput
        , rolesCurrency
        , metadata = decodeMarloweTransactionMetadataLenient case txBody of
            C.TxBody C.TxBodyContent{..} -> case txMetadata of
              C.TxMetadataNone -> mempty
              C.TxMetadataInEra _ m -> fromCardanoTxMetadata m
        , txBody
        , marloweScriptHash = marloweContext.marloweScriptHash
        , marloweScriptAddress = marloweContext.marloweAddress
        , payoutScriptHash = marloweContext.payoutScriptHash
        , payoutScriptAddress = marloweContext.payoutAddress
        , version
        , datum
        , assets
        , safetyErrors
        }

toBabbageEraOnwards
  :: (Monad m) => e -> C.CardanoEra era -> ExceptT e m (C.BabbageEraOnwards era)
toBabbageEraOnwards e = C.inEonForEra (throwE e) pure

lookupMarloweScriptUtxo :: C.NetworkId -> MarloweScripts -> Either LoadMarloweContextError ReferenceScriptUtxo
lookupMarloweScriptUtxo networkId MarloweScripts{..} = note (MarloweScriptNotPublished marloweScript) $ Map.lookup networkId marloweScriptUTxOs

lookupPayoutScriptUtxo :: C.NetworkId -> MarloweScripts -> Either LoadMarloweContextError ReferenceScriptUtxo
lookupPayoutScriptUtxo networkId MarloweScripts{..} = note (PayoutScriptNotPublished payoutScript) $ Map.lookup networkId payoutScriptUTxOs

findMarloweOutput :: forall era. C.IsCardanoEra era => Chain.Address -> C.TxBody era -> Maybe Chain.TxOutRef
findMarloweOutput address = \case
  body@(C.TxBody C.TxBodyContent{..}) ->
    fmap (Chain.TxOutRef (fromCardanoTxId $ C.getTxId body) . fst) $
      List.find (isToCurrentScriptAddress . snd) $
        zip [minBound ..] txOuts
  where
    isToCurrentScriptAddress (C.TxOut address' _ _ _) =
      address == fromCardanoAddressInEra (C.cardanoEra @era) address'

mkMarloweContext
  :: (MonadUnliftIO m)
  => C.NetworkId
  -> MarloweVersion v
  -> GetCurrentScripts v
  -> Maybe Chain.StakeCredential
  -> ExceptT InitError m (MarloweContext v)
mkMarloweContext networkId version getCurrentScripts mStakeCredential = do
  let
    scripts@MarloweScripts{..} = getCurrentScripts version
  mCardanoStakeCredential <- except $ traverse (note InitToCardanoError . toCardanoStakeCredential) mStakeCredential
  marlowePaymentCredential <- except . note InitToCardanoError . toCardanoPaymentCredential $ Chain.ScriptCredential marloweScript
  payoutPaymentCredential <- except . note InitToCardanoError . toCardanoPaymentCredential $ Chain.ScriptCredential payoutScript
  let
      stakeReference = maybe C.NoStakeAddress C.StakeAddressByValue mCardanoStakeCredential
      marloweAddress =
        fromCardanoAddressInEra C.BabbageEra $
          C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) $
            C.makeShelleyAddress
              networkId
              marlowePaymentCredential
              stakeReference
      payoutAddress =
        fromCardanoAddressInEra C.BabbageEra $
          C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) $
            C.makeShelleyAddress
              networkId
              payoutPaymentCredential
              C.NoStakeAddress
  except $ first InitLoadMarloweContextFailed do
    marloweScriptUTxO <- lookupMarloweScriptUtxo networkId scripts
    payoutScriptUTxO <- lookupPayoutScriptUtxo networkId scripts
    pure
      MarloweContext
        { scriptOutput = Nothing
        , marloweAddress
        , payoutAddress
        , marloweScriptUTxO
        , payoutScriptUTxO
        , marloweScriptHash = marloweScript
        , payoutScriptHash = payoutScript
        }

limitAnalysisTime :: NominalDiffTime -> IO (Either String [SafetyError]) -> IO (Either String [SafetyError])
limitAnalysisTime timeout analysis = do
  let analysis' = runExceptT do
        let go = do
              errs <- analysis >>= (Exception.evaluate . DeepSeq.force)
              pure $ Right errs
        res <-
          ExceptT $
            go `catch` \(e :: SomeException) -> do
              pure $ Left (show e)
        except res
  res <-
    race
      (threadDelay (floor $ nominalDiffTimeToSeconds timeout * 1_000_000))
      analysis'
  case res of
    -- Timeout reached
    Left () -> pure $ Right [SafetyAnalysisTimeout]
    -- Analysis finished with exception or internal error
    Right (Left e) -> pure $ Left e
    -- Analysis finished successfully
    Right (Right res') -> pure $ Right res'
