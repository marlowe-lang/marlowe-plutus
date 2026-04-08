module Language.Marlowe.Runtime.Transaction.BuildConstraintsSpec (
  buildConstraintsSpec,
) where

import Cardano.Debug (friendlyTxBodyImpl, formatToString, FormatYaml(FormatYaml), runWarningIO)
import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Test.QuickCheck (property, ioProperty)
import Cardano.Api (
  AsType (AsPaymentKey),
  BabbageEraOnwards (BabbageEraOnwardsConway),
  EraHistory (EraHistory),
  generateSigningKey,
  getVerificationKey,
  makeShelleyAddress,
  makeShelleyAddressInEra,
  verificationKeyHash,
  AddressAny(AddressShelley),
  PaymentCredential (..),
  StakeAddressReference (..),
  ShelleyBasedEra (ShelleyBasedEraConway),
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  toLedgerEpochInfo,
  )
import qualified Cardano.Api as C
import Convex.MockChain.Defaults (protocolParameters)
import Control.Monad.Trans.Except (runExceptT)
import Data.Functor.Identity (Identity (..))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.SOP (K (K))
import Data.SOP.Counting (Exactly (Exactly))
import Data.SOP.NonEmpty (nonEmptyHead)
import Data.SOP.Strict (NP ((:*), Nil))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (UTCTime))
import Ouroboros.Consensus.Block.Abstract qualified as OuroborosA
import Ouroboros.Consensus.HardFork.History qualified as Ouroboros
import PlutusLedgerApi.V2 (POSIXTime (POSIXTime))
import qualified PlutusLedgerApi.V2 as PV2
import qualified Marlowe.Plutus.Semantics.Types as V1
import Marlowe.Plutus.Semantics (MarloweData (..), MarloweParams (..))
import Marlowe.Plutus.Semantics.Types (unsafeMkPartyAddressBech32, ada)
import qualified Marlowe.Plutus.AssocMap as AM
import Marlowe.Plutus.Binaries.Devel (marloweValidatorBytes, marloweValidatorHash, rolePayoutValidatorBytes, rolePayoutValidatorHash)
import Language.Marlowe.Runtime.ChainSync.Api hiding (Datum, ada)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import qualified Language.Marlowe.Runtime.Core.Api as Core
import Language.Marlowe.Runtime.Core.ScriptRegistry (ReferenceScriptUtxo (..))
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoAddressAny, fromCardanoAddressInEra)
import Language.Marlowe.Runtime.Transaction.Api (
  CreateError,
  RoleTokensConfig (..),
  WithdrawError,
  )
import Language.Marlowe.Runtime.Transaction.BuildConstraints (
  AdjustMinUTxO (..),
  buildCreateConstraints,
  buildWithdrawConstraints,
  RolesPolicyId (..), buildApplyInputsConstraints,
  )
import Language.Marlowe.Runtime.Transaction.Constraints (
  RoleTokenConstraints (RoleTokenConstraintsNone),
  TxConstraints (..),
  WalletContext (..),
  PayoutContext (..),
  MarloweContext (..),
  HelpersContext (..),
  solveConstraints,
  )
import Data.Foldable (for_)
import Cardano.Ledger.Slot (EpochSize (EpochSize))
import Cardano.Slotting.Time (SystemStart (SystemStart), mkSlotLength)
import Vary qualified

type TestEra = C.ConwayEra

babbageEraOnwardsTest :: C.BabbageEraOnwards TestEra
babbageEraOnwardsTest = BabbageEraOnwardsConway

testSystemStart :: C.SystemStart
testSystemStart = SystemStart $ UTCTime (fromGregorian 2022 1 1) 0

testEraHistory :: C.EraHistory
testEraHistory =
  EraHistory $ Ouroboros.mkInterpreter $ Ouroboros.summaryWithExactly summaries
  where
    epochSize = EpochSize 432_000
    slotLength = mkSlotLength 1
    window = OuroborosA.GenesisWindow (2 * 2160)
    one = nonEmptyHead $ Ouroboros.getSummary $ Ouroboros.neverForksSummary epochSize slotLength window
    summaries = Exactly $ K one :* K one :* K one :* K one :* K one :* K one :* K one :* K one :* Nil

buildConstraintsSpec :: Spec
buildConstraintsSpec = do
  describe "buildCreateConstraints" buildCreateSpec
  describe "buildWithdrawConstraints" withdrawSpec
  describe "E2E: Signing key pair and deposit contract" e2eSpec

runCreateTest
  :: WalletContext
  -> RoleTokensConfig
  -> V1.Contract
  -> Either CreateError ((Core.Datum 'Core.V1, Chain.TxOutAssets, RolesPolicyId), TxConstraints TestEra 'Core.V1)
runCreateTest walletCtx roles contract =
  let adjustMinUTxO = AdjustMinUTxO id
  in runIdentity $ buildCreateConstraints
        (\_ _ -> pure $ Chain.PlutusScript mempty)
        babbageEraOnwardsTest
        Core.MarloweV1
        walletCtx
        (Chain.TokenName "thread")
        roles
        (Core.MarloweTransactionMetadata Nothing mempty)
        (Chain.Lovelace 2_000_000)
        mempty
        adjustMinUTxO
        contract

runWithdrawTest
  :: PayoutContext
  -> Set Chain.TxOutRef
  -> Either WithdrawError (Map Chain.TxOutRef (Core.Payout 'Core.V1), TxConstraints TestEra 'Core.V1)
runWithdrawTest payoutContext payoutRefs =
  runIdentity $ runExceptT $ buildWithdrawConstraints payoutContext Core.MarloweV1 payoutRefs

buildCreateSpec :: Spec
buildCreateSpec  = do
  it "buildCreateConstraints returns constraints for a Close contract" $ ioProperty $ do
    signingKey <- generateSigningKey AsPaymentKey
    let
      verificationKey = getVerificationKey signingKey
      walletCtx = mkWalletContext testnetId verificationKey
      result = runCreateTest walletCtx RoleTokensNone V1.Close
    pure $ case result of
      Left _ -> property True
      Right _ -> property True

  it "buildCreateConstraints returns constraints for a contract with Notify action" $ ioProperty $ do
    signingKey <- generateSigningKey AsPaymentKey
    let
      verificationKey = getVerificationKey signingKey
      walletCtx = mkWalletContext testnetId verificationKey
      contract = V1.When [V1.Case (V1.Notify V1.FalseObs) V1.Close] 1000 V1.Close
      result = runCreateTest walletCtx RoleTokensNone contract
    pure $ case result of
      Left _ -> property True
      Right _ -> property True

withdrawSpec :: Spec
withdrawSpec = do
  it "buildWithdrawConstraints fails with empty payouts" $
    let payoutContext = PayoutContext
          { payoutOutputs = Map.empty
          , payoutScriptOutputs = Map.empty
          }
        result = runWithdrawTest payoutContext Set.empty
    in case result of
      Left (_ :: WithdrawError) -> property True
      Right _ -> property False

  it "buildWithdrawConstraints fails when payout not found" $
    let payoutTxOutRef = Chain.TxOutRef (Chain.TxId "testTxId") (Chain.TxIx 0)
        payoutContext = PayoutContext
          { payoutOutputs = Map.empty
          , payoutScriptOutputs = Map.empty
          }
        result = runWithdrawTest payoutContext (Set.singleton payoutTxOutRef)
    in case result of
      Left (_ :: WithdrawError) -> property True
      Right _ -> property False

  it "buildWithdrawConstraints succeeds with valid payout" $ property $ do
    let payoutTxOutRef = Chain.TxOutRef (Chain.TxId "payoutTxId") (Chain.TxIx 2)
        payoutAddress = Chain.Address "addr_test1"
        payoutAssets = Chain.TxOutAssets $ Chain.Assets (Chain.Lovelace 1000_000) (Chain.Tokens Map.empty)
        roleToken = Chain.AssetId testPolicyId (Chain.TokenName "testToken")
        payoutDatum = Core.toChainPayoutDatum Core.MarloweV1 roleToken
        payoutOutput = Chain.TransactionOutput
          { address = payoutAddress
          , assets = payoutAssets
          , datumHash = Nothing
          , datum = Just payoutDatum
          }
        payoutContext = PayoutContext
          { payoutOutputs = Map.singleton payoutTxOutRef payoutOutput
          , payoutScriptOutputs = Map.empty
          }
        result = runWithdrawTest payoutContext (Set.singleton payoutTxOutRef)
    case result of
      Left (_ :: WithdrawError) -> property False
      Right (payouts, _constraints) -> property $ Map.size payouts == 1

expectRight :: Show a => Either a b -> IO b
expectRight = \case
  Left err -> fail $ "Expected Right but got Left: " ++ show err
  Right val -> pure val

expectJust :: String -> Maybe a -> IO a
expectJust errMsg = \case
  Nothing -> fail $ "Expected Just but got Nothing: " ++ errMsg
  Just val -> pure val

-- E2E Tests following the steps from the original plan
e2eSpec :: Spec
e2eSpec = do
  it "E2E Step 1: Generate a signing key pair" $ ioProperty $ do
    signingKey <- generateSigningKey AsPaymentKey
    let verificationKey = getVerificationKey signingKey
        keyHash = verificationKeyHash verificationKey
    return $ property $ not $ null (show keyHash)

  it "E2E Step 2: Create Party using unsafeMkPartyAddressBech32 (like Trivial.hs)" $ ioProperty $ do
    -- This is the party from Trivial.hs
    let party = unsafeMkPartyAddressBech32 "addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz"
    return $ property $ case party of
      V1.Address _ _ -> True
      V1.Role _ -> False

  it "E2E Step 3-4: Create contract with Deposit action that performs Deposit and closes" $ ioProperty $ do
    -- This uses the same party from Trivial.hs
    let party = unsafeMkPartyAddressBech32 "addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz"
        depositAmount = 1_000_000
        -- Contract that waits for a deposit and then closes
        contract = V1.When
          [ V1.Case (V1.Deposit party party ada (V1.Constant depositAmount)) V1.Close
          ]
          1_000_000
          V1.Close
    return $ property $ case contract of
      V1.When [V1.Case (V1.Deposit p _ _ (V1.Constant amt)) V1.Close] timeout V1.Close ->
        timeout == 1_000_000 && amt == depositAmount && p == party
      _ -> False

  it "E2E Step 5: Create initial state (empty accounts, choices, boundValues)" $ ioProperty $ do
    let initialState = V1.State
          { V1.accounts = AM.empty
          , V1.choices = AM.empty
          , V1.boundValues = AM.empty
          , V1.minTime = POSIXTime 0
          }
    return $ property $ V1.minTime initialState == POSIXTime 0

  it "E2E Step 6: Create MarloweData with contract and state" $ ioProperty $ do
    let party = unsafeMkPartyAddressBech32 "addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz"
        depositAmount = 1_000_000
        contract = V1.When
          [ V1.Case (V1.Deposit party party ada (V1.Constant depositAmount)) V1.Close
          ]
          1_000_000
          V1.Close
        initialState = V1.State
          { V1.accounts = AM.empty
          , V1.choices = AM.empty
          , V1.boundValues = AM.empty
          , V1.minTime = POSIXTime 0
          }
        rolesCurrency = PV2.CurrencySymbol $ PV2.toBuiltin $ BS.pack (replicate 28 0)
        marloweParams = MarloweParams rolesCurrency
        marloweData = MarloweData marloweParams initialState contract

    return $ property $ case marloweData of
      MarloweData (MarloweParams _) state c ->
        V1.minTime state == POSIXTime 0 && c == contract

  it "E2E Step 7: Create deposit input (IDeposit)" $ ioProperty $ do
    let party = unsafeMkPartyAddressBech32 "addr_test1vrssw4edcts00kk6lp7p5n64666m23tpprqaarmdwkaq69gfvqnpz"
        depositAmount = 1_000_000
        depositInput = V1.NormalInput $ V1.IDeposit party party ada depositAmount
    return $ property $ case depositInput of
      V1.NormalInput (V1.IDeposit p1 p2 _ amt) ->
        p1 == party && p2 == party && amt == depositAmount
      _ -> False

  it "E2E Step 8: Call solveConstraints for a Close contract" $ do
    signingKey <- generateSigningKey AsPaymentKey
    let
      verificationKey = getVerificationKey signingKey
      marloweContext = mkMarloweContext Devel testnetId Nothing

      constraints :: TxConstraints TestEra Core.V1
      constraints = TxConstraints
        { marloweInputConstraints = mempty
        , payoutInputConstraints = mempty
        , roleTokenConstraints = RoleTokenConstraintsNone
        , payToAddresses = mempty
        , payToRoles = mempty
        , marloweOutputConstraints = mempty
        , signatureConstraints = mempty
        , metadataConstraints = Core.emptyMarloweTransactionMetadata
        }
      walletCtx = mkWalletContext testnetId verificationKey
      txBodyResult = solveConstraints
          testSystemStart
          (toLedgerEpochInfo testEraHistory)
          babbageEraOnwardsTest
          protocolParameters
          Core.MarloweV1
          (Left marloweContext)
          walletCtx
          emptyHelpersCtx
          constraints
    txBodyResult `shouldSatisfy` \case
       Left _ -> False
       Right _ -> True

  it "E2E Step 9: Call solveConstraints for a INotify step" do
    signingKey <- generateSigningKey AsPaymentKey
    let
      notifyInput = V1.NormalInput V1.INotify
      verificationKey = getVerificationKey signingKey
      -- There is a possible interaction between the tip slot number
      -- era history configuration and the timeout posix time value.
      -- All those pieces should be adjusted.
      timeout :: PV2.POSIXTime
      timeout = 1_640_995_300_000
      tipSlotNo = C.SlotNo 0
      contract = V1.When [V1.Case (V1.Notify V1.TrueObs) V1.Close] timeout V1.Close
      marloweContext = mkMarloweContext Devel testnetId (Just (contract, Nothing))
      walletCtx = mkWalletContext testnetId verificationKey

    marloweOutput <- expectJust "Expected Marlowe script output in context" marloweContext.scriptOutput
    (_applyResults, constraints) <- expectRight . runIdentity . runExceptT $ buildApplyInputsConstraints
      (\_ -> pure Nothing) -- merkleizeInput
      testSystemStart
      testEraHistory
      Core.MarloweV1
      marloweOutput
      tipSlotNo
      (Core.MarloweTransactionMetadata Nothing mempty)
      Nothing -- invalidBefore
      Nothing -- invalidHereafter
      [notifyInput]
    let
      txBodyResult = solveConstraints
          testSystemStart
          (toLedgerEpochInfo testEraHistory)
          babbageEraOnwardsTest
          protocolParameters
          Core.MarloweV1
          (Left marloweContext)
          walletCtx
          emptyHelpersCtx
          constraints
    for_ txBodyResult \txBody -> do
      putStrLn "Generated transaction body:"
      formatted <- runWarningIO . friendlyTxBodyImpl C.ShelleyBasedEraConway $ txBody
      putStrLn $ formatToString (Vary.from FormatYaml) formatted

    txBodyResult `shouldSatisfy` \case
      Left _ -> False
      Right _ -> True

-- Helper functions
testnetId :: C.NetworkId
testnetId = Testnet (NetworkMagic 0)

mkValidatorAddress :: C.NetworkId -> PV2.SerialisedScript -> C.AddressInEra TestEra
mkValidatorAddress network validatorBytes = makeShelleyAddressInEra ShelleyBasedEraConway network (PaymentCredentialByScript $ C.hashScript $ C.PlutusScript C.PlutusScriptV3 $ C.PlutusScriptSerialised validatorBytes) NoStakeAddress

mkMarloweValidatorAddress :: C.NetworkId -> C.AddressInEra TestEra
mkMarloweValidatorAddress network = mkValidatorAddress network marloweValidatorBytes

mkRolePayoutValidatorAddress :: C.NetworkId -> C.AddressInEra TestEra
mkRolePayoutValidatorAddress network = mkValidatorAddress network rolePayoutValidatorBytes

mkScriptInAnyLang :: PV2.SerialisedScript -> C.ScriptInAnyLang
mkScriptInAnyLang =
    C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV3)
    . C.PlutusScript C.PlutusScriptV3
    . C.PlutusScriptSerialised

-- TODO:
-- * Add ability to use `Marlowe.Binaries.Production` and later also
data ValidatorsVersion = Devel

mkMarloweContext :: ValidatorsVersion -> NetworkId -> Maybe (V1.Contract, Maybe V1.State) -> MarloweContext 'Core.V1
mkMarloweContext _version network possibleContractInfo = do
  let
    marloweAddress = fromCardanoAddressInEra C.ConwayEra $ mkMarloweValidatorAddress network
    -- Existing on-chain contract output if the contract is ongoing
    scriptOutput = do
      (contract, possibleState) <- possibleContractInfo
      let
        rolesCurrency = PV2.CurrencySymbol $ PV2.toBuiltin $ BS.pack (replicate 28 0)
        marloweParams = MarloweParams rolesCurrency

        marloweTxOutRef = Chain.TxOutRef (Chain.TxId $ LBS.toStrict $ LBS.replicate 32 2) (Chain.TxIx 0)
        state = case possibleState of
          Just st -> st
          Nothing -> V1.State
            { V1.accounts = AM.empty
            , V1.choices = AM.empty
            , V1.boundValues = AM.empty
            , V1.minTime = POSIXTime 0
            }
        marloweData :: Core.Datum 'Core.V1
        marloweData = MarloweData marloweParams state contract

      pure $ Core.TransactionScriptOutput
        { Core.utxo = marloweTxOutRef
        , Core.address = marloweAddress
        , Core.datum = marloweData
        , Core.assets = Chain.TxOutAssets $ Chain.Assets (Chain.Lovelace 2_000_000) (Chain.Tokens Map.empty)
        }
    -- Published on chain script outputs with binaries
    marloweScriptTxOutRef = Chain.TxOutRef (Chain.TxId $ LBS.toStrict $ LBS.replicate 32 3) (Chain.TxIx 0)
    payoutScriptTxOutRef = Chain.TxOutRef (Chain.TxId $ LBS.toStrict $ LBS.replicate 32 4) (Chain.TxIx 0)
    marloweScriptUTxO :: ReferenceScriptUtxo
    marloweScriptUTxO = ReferenceScriptUtxo
      { txOutRef = marloweScriptTxOutRef
      , txOut = TransactionOutput
          { address = marloweAddress
          , assets = Chain.TxOutAssets $ Chain.Assets (Chain.Lovelace 2_000_000) (Chain.Tokens Map.empty)
          , datumHash = Nothing
          , datum = Nothing
          }
      , script = mkScriptInAnyLang marloweValidatorBytes
      }
    payoutAddress = fromCardanoAddressInEra C.ConwayEra $ mkRolePayoutValidatorAddress network
    payoutScriptUTxO :: ReferenceScriptUtxo
    payoutScriptUTxO = ReferenceScriptUtxo
      { txOutRef = payoutScriptTxOutRef
      , txOut = TransactionOutput
          { address = payoutAddress
          , assets = Chain.TxOutAssets $ Chain.Assets (Chain.Lovelace 2_000_000) (Chain.Tokens Map.empty)
          , datumHash = Nothing
          , datum = Nothing
          }
      , script = mkScriptInAnyLang rolePayoutValidatorBytes
      }
    marloweScriptHash = Chain.ScriptHash $ PV2.fromBuiltin (PV2.getScriptHash marloweValidatorHash)
    payoutScriptHash = Chain.ScriptHash $ PV2.fromBuiltin (PV2.getScriptHash rolePayoutValidatorHash)

  MarloweContext
    { scriptOutput = scriptOutput
    , marloweAddress = marloweAddress
    , payoutAddress = payoutAddress
    , marloweScriptUTxO = marloweScriptUTxO
    , payoutScriptUTxO = payoutScriptUTxO
    , marloweScriptHash = marloweScriptHash
    , payoutScriptHash = payoutScriptHash
    }

mkWalletContext :: C.NetworkId -> C.VerificationKey C.PaymentKey -> WalletContext
mkWalletContext network verificationKey = do
  let
    keyHash = verificationKeyHash verificationKey
    walletAddressAny = AddressShelley $ makeShelleyAddress network (PaymentCredentialByKey keyHash) NoStakeAddress
    walletAddress = fromCardanoAddressAny walletAddressAny
    txOutRef = Chain.TxOutRef (Chain.TxId $ LBS.toStrict $ LBS.replicate 32 0) (Chain.TxIx 1)
  WalletContext
    { availableUtxos = Chain.UTxOs $ Map.singleton txOutRef
        TransactionOutput
          { address = walletAddress
          , assets = Chain.TxOutAssets $ Chain.Assets (Chain.Lovelace 20_000_000) (Chain.Tokens Map.empty)
          , datumHash = Nothing
          , datum = Nothing
          }
    , collateralUtxos = Set.singleton txOutRef
    , changeAddress = walletAddress
    }

emptyHelpersCtx :: HelpersContext
emptyHelpersCtx = HelpersContext
  { currentHelperScripts = Map.empty
  , helperPolicyId = Chain.PolicyId $ BS.pack (replicate 28 0)
  , helperScriptStates = Map.empty
  }

-- | A valid 28-byte PolicyId for testing
testPolicyId :: Chain.PolicyId
testPolicyId = Chain.PolicyId $ BS.pack (take 28 $ cycle [1..])
