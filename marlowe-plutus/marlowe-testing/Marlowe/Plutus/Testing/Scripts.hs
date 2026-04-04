module Marlowe.Plutus.Testing.Scripts
  ( -- * Script abstraction
    AddNoise (..)
  , CheckPlutusLog (..)
  , FixtureName (..)
  , MarloweScripts (..)
  , RolePayoutAddress (..)
  , SemanticsAddress (..)
  , TestFailures (..)
  , Valid (..)
  , mkBareSemanticsTransaction
  , genValidPayoutTransaction
  , genValidSemanticsTransaction
  , specForScripts
  , specForFixture
  ) where

import Control.Lens (Lens', use, uses, (%=), (.=), (<>=), (<~), (^.))
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, execStateT)
import Control.Monad.Trans.Writer (runWriter)
import Data.Bifunctor (Bifunctor (..), bimap, second)
import Data.List (nub)
import Data.Maybe (maybeToList)
import Data.Traversable (for)
import Marlowe.Plutus.Merkle
  ( MerkleizedContract (..)
  , deepMerkleize
  , merkleizeInputs
  , dataHash
  )
import Marlowe.Plutus.Semantics
  ( MarloweData (..)
  , MarloweParams (..)
  , Payment (Payment)
  , TransactionInput (..)
  , TransactionOutput (..)
  , computeTransaction
  , paymentMoney
  , totalBalance
  )
import Marlowe.Plutus.Semantics.Types
  ( ChoiceId (ChoiceId)
  , Contract (..)
  , Input (..)
  , InputContent (IChoice, IDeposit)
  , Party (Role)
  , Payee (Party)
  , State (accounts)
  , Token (Token)
  , getInputContent
  )
import Marlowe.Plutus.Scripts (MarloweInput, MarloweTxInput (..))
import PlutusLedgerApi.V1 (Data, Lovelace (Lovelace))
import PlutusLedgerApi.V1.Address (toPubKeyHash)
import PlutusLedgerApi.V1.Value (flattenValue, gt, valueOf)
import PlutusLedgerApi.V3
  ( Address (Address)
  , Credential (..)
  , CurrencySymbol
  , Datum (..)
  , DatumHash (..)
  , Extended (Finite, NegInf, PosInf)
  , Interval (Interval)
  , LogOutput
  , LowerBound (LowerBound)
  , OutputDatum (..)
  , PubKeyHash
  , Redeemer (..)
  , ScriptContext (..)
  , ToData (..)
  , TokenName
  , TxInInfo (..)
  , TxInfo (..)
  , TxOut (..)
  , UpperBound (UpperBound)
  , Value
  , adaSymbol
  , adaToken
  , singleton
  , toData, ScriptInfo (SpendingScript), emptyMintValue, TxId (TxId), ScriptPurpose (Spending)
  )
import PlutusTx.These (These (..))
import Marlowe.Plutus.Testing.Transactions
  ( PayoutTransaction (..)
  , SemanticsTransaction (..)
  , amount
  , input
  , inputContract
  , inputState
  , marloweParamsPayout
  , role
  )
import Marlowe.Plutus.Testing.Contrib.PlutusTx.PlutusTransaction
  ( PlutusTransaction (..)
  , infoData
  , infoFee
  , infoInputs
  , infoOutputs
  , infoSignatories
  , infoValidRange
  , redeemer
  , scriptInfo, infoRedeemers
  )
import Marlowe.Plutus.Testing.Reference
  ( FixtureName (..)
  , ReferencePath (..)
  , arbitraryReferenceTransaction
  , readReferencePaths
  , readReferencePathsFor
  -- , referenceTransactions
  )
import Marlowe.Plutus.Testing.Semantics.Arbitrary
  ( arbitraryGoldenTransaction
  )
import Marlowe.Plutus.Testing.Contrib.Integer.Arbitrary (arbitraryPositiveInteger)
import Marlowe.Plutus.Testing.Semantics.Golden (GoldenTransaction)
import Test.Hspec
  ( describe
  , SpecWith, Spec
  )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  ( Arbitrary (arbitrary)
  , Gen
  , Property
  , Testable (property)
  , chooseInteger
  , forAll
  , frequency
  , listOf
  , listOf1
  , oneof
  , shuffle
  , suchThat
  )

import qualified Marlowe.Plutus.Semantics as M (MarloweData (marloweParams))
import qualified Marlowe.Plutus.Semantics.Types as M
  ( Party (Address)
  )
import qualified PlutusLedgerApi.V1.Value as V (singleton)
import qualified PlutusTx.AssocMap as PlutusTxAM
import qualified Marlowe.Plutus.AssocMap as AM
import qualified PlutusTx.Prelude as P
import qualified Marlowe.Plutus.Testing.Transactions as Transactions
import Marlowe.Plutus.Testing.Contrib.PlutusTx.Arbitrary (arbitraryAnyCurrencySymbol, arbitraryNonAdaCurrencySymbol)
import Marlowe.Plutus.Testing.Contrib.PlutusTx.Lens ((<><~))
import Marlowe.Plutus.Testing.Contrib.Data.Functor ((<@>))
import qualified PlutusTx.Prelude as PlutusTx
import qualified PlutusTx.AssocMap as PlusTxAM

newtype RolePayoutAddress = RolePayoutAddress { address :: Address }

newtype SemanticsAddress = SemanticsAddress { address :: Address }

-- | Abstraction over a pair of Marlowe scripts (semantics + payout) and a way
-- to run them on raw Plutus 'Data'.
--
-- This is the only place where the on-chain tests depend on how scripts are
-- executed. Callers (core semantics tests, binaries tests, etc.) provide an
-- instance built either from a pure Haskell model or from compiled scripts.
data MarloweScripts = MarloweScripts
  { semanticsAddress :: !SemanticsAddress
  , rolePayoutAddress    :: !RolePayoutAddress
  , runSemantics     :: Data -> These String LogOutput
  , runPayout        :: Data -> These String LogOutput
  }

type TransactionM m p a = StateT (PlutusTransaction p) m a

type ArbitraryTransactionM p a = TransactionM Gen p a

newtype TestFailures = TestFailures Bool

newtype Valid = Valid Bool

newtype MakeInvalid = MakeInvalid Bool

newtype AddNoise = AddNoise Bool

newtype AllowMerkleization = AllowMerkleization Bool

specForSemanticsScript :: String -> MarloweScripts -> [ReferencePath] -> TestFailures -> CheckPlutusLog -> Spec
specForSemanticsScript version scripts referencePaths (TestFailures testFailures) checkPlutusLog = do
  describe ("Scripts version: \"" ++ version ++ "\"") do
    describe "Marlowe validator" do
      -- it "Should be a reasonable size" $ marloweValidatorSize scripts
      describe "Valid transactions" do
        prop "Noiseless" $ checkSemanticsTransaction scripts mempty referencePaths noModify noModify noVeto (Valid True) (AddNoise False) (AllowMerkleization True)
        prop "Noisy" $ checkSemanticsTransaction scripts mempty referencePaths noModify noModify noVeto (Valid True) (AddNoise True) (AllowMerkleization True)
      when testFailures $ do
        prop "Constraint 2. Single Marlowe script input" $ checkDoubleInput scripts checkPlutusLog referencePaths
        prop "Constraint 3. Single Marlowe output" $ checkMultipleOutput scripts checkPlutusLog referencePaths
        prop "Constraint 4. No output to script on close" $ checkCloseOutput scripts checkPlutusLog referencePaths
-- #ifdef CHECK_PRECONDITIONS
--       prop "Constraint 5. Input value from script" $ checkValueInput scripts referencePaths
-- #endif
        prop "Constraint 6. Output value to script" $ checkValueOutput scripts checkPlutusLog referencePaths
        prop "Constraint 9. Marlowe parameters" $ checkParamsOutput scripts checkPlutusLog referencePaths
        prop "Constraint 10. Output state" $ checkStateOutput scripts checkPlutusLog referencePaths
        prop "Constraint 11. Output contract" $ checkContractOutput scripts checkPlutusLog referencePaths

      describe "Constraint 12. Merkleized continuations" do
        prop "Valid merkleization" $ checkMerkleization scripts checkPlutusLog referencePaths (MakeInvalid False)
        when testFailures $
          prop "Invalid merkleization" $ checkMerkleization scripts checkPlutusLog referencePaths (MakeInvalid True)
-- #if defined(CHECK_PRECONDITIONS) && defined(CHECK_POSITIVE_BALANCES)
--       prop "Constraint 13. Positive balances" $ checkPositiveAccounts scripts referencePaths
-- #endif
      when testFailures $ do
        prop "Constraint 14. Inputs authorized" $ checkAuthorization scripts checkPlutusLog referencePaths
        prop "Constraint 15. Sufficient payment" $ checkPayment scripts checkPlutusLog referencePaths
      prop "Constraint 18. Final balance" $ checkOutputConsistency scripts referencePaths
-- #if defined(CHECK_PRECONDITIONS) && (defined(CHECK_DUPLICATE_ACCOUNTS) || defined(CHECK_DUPLICATE_CHOICES) || defined(CHECK_DUPLICATE_BINDINGS))
--       prop "Constraint 19. No duplicates" $ checkInputDuplicates scripts referencePaths
-- #endif
      prop "Constraint 20. Single satisfaction" $ checkOtherValidators scripts checkPlutusLog referencePaths
--      prop "Hash golden test" $
--        checkValidatorHash
--          semanticsValidatorHash
--          -- DO NOT ALTER THE FOLLOWING VALUE UNLESS YOU ARE COMMITTING
--          -- APPROVED CHANGES TO MARLOWE'S SEMANTICS VALIDATOR. THIS HASH
--          -- HAS IMPLICATIONS FOR VERSIONING, AUDIT, AND CONTRACT DISCOVERY.
--          ( if checkPlutusLog
--              then "7bbd48635695786d0c63b7a42cd888d2327a7a9b2340b2f09229e9be"
--              else "377325ad84a55ba0282d844dff2d5f0f18c33fd4a28a0a9d73c6f60d"
--          )

specForScripts :: String -> MarloweScripts -> TestFailures -> CheckPlutusLog -> IO (SpecWith ())
specForScripts version scripts testFailures@(TestFailures tf) checkPlutusLog = do
  referencePaths <- readReferencePaths
  pure $ describe ("Scripts version: \"" ++ version ++ "\"") do
    specForSemanticsScript version scripts referencePaths testFailures checkPlutusLog
    describe "Payout validator" do
      describe "Valid transactions" do
        prop "Noiseless" $ checkPayoutTransaction scripts noModify noModify noVeto (Valid True) (AddNoise False)
        prop "Noisy" $ checkPayoutTransaction scripts noModify noModify noVeto (Valid True) (AddNoise True)
      when tf $ do
        describe "Constraint 17. Payment authorized" do
            prop "Invalid authorization for withdrawal" $ checkWithdrawal scripts True
            prop "Missing authorized withdrawal" $ checkWithdrawal scripts False
--      prop "Hash golden test" $
--        checkValidatorHash
--          payoutValidatorHash
--          -- DO NOT ALTER THE FOLLOWING VALUE UNLESS YOU ARE COMMITTING
--          -- APPROVED CHANGES TO MARLOWE'S ROLE VALIDATOR. THIS HASH HAS
--          -- IMPLICATIONS FOR VERSIONING, AUDIT, AND CONTRACT DISCOVERY.
--          ( if checkPlutusLog
--              then "7f01c268cb6c400315d6d5b9c28da380bdd4d39682d5373fae9c22a8"
--              else "fcb8885eb5e4f9a5cfca3c75e8c7280e482af32dcdf2d13e47d05d27"
--          )


specForFixture :: String -> FixtureName -> MarloweScripts -> TestFailures -> CheckPlutusLog -> IO (SpecWith ())
specForFixture version fixtureName scripts testFailures checkPlutusLog = do
  referencePaths <- readReferencePathsFor fixtureName
  pure $ do
    specForSemanticsScript version scripts referencePaths testFailures checkPlutusLog

  -- FIXME: Is this good idea?
  -- let nonTimeoutTransactions = filter hasInputs $ referenceTransactions referencePaths
  -- pure $ describe ("Scripts version: \"" ++ version ++ "\" focused fixture: " ++ show fixtureName) do
  --   describe "Marlowe validator" do
  --     describe "Focused fixture scenarios" do
  --       prop "Action path / noiseless / systematic" $
  --         checkSemanticsGoldenTransactions scripts mempty nonTimeoutTransactions noModify noModify noVeto True False
  --       prop "Action path / noisy / systematic" $
  --         checkSemanticsGoldenTransactions scripts mempty nonTimeoutTransactions noModify noModify noVeto True True
  --  where
  --   hasInputs (_, _, TransactionInput{txInputs}, _) = not (null txInputs)

{- HLINT ignore "Functor law" -}
-- | Create a transaction generator for spending, with empty or default values.
-- | Please not that consistency between `a` and the rest of the context is by
-- | default broken.
bareSpending :: a -> Gen (PlutusTransaction a)
bareSpending p =
  PlutusTransaction p
    <$> ( ScriptContext
            <$> ( TxInfo
                    mempty
                    -- ^ txInfoInputs
                    mempty
                    -- ^ txInfoReferenceInputs
                    mempty
                    -- ^ txInfoOutputs
                    PlutusTx.zero
                    -- ^ txInfoFee
                    emptyMintValue
                    -- ^ txInfoMint
                    mempty
                    -- ^ txInfoTxCerts
                    PlutusTxAM.empty
                    -- ^ txInfoWdrl
                    (Interval (LowerBound NegInf False) (UpperBound PosInf True))
                    -- ^ txInfoValidRange
                    mempty
                    -- ^ txInfoSignatories
                    PlutusTxAM.empty
                    -- ^ txInfoRedeemers
                    PlutusTxAM.empty
                    -- ^ txInfoData
                    <$> (TxId <$> arbitrary)
                    -- ^ txInfoId
                    <@> PlutusTxAM.empty
                    -- ^ txInfoVotes
                    <@> mempty
                    -- ^ txInfoProposalProcedures
                    <@> Nothing
                    -- ^ txInfoCurrentTreasuryAmount
                    <@> Nothing
                    -- ^ txInfoTreasuryDonation
                )
            <@> (Redeemer $ toBuiltinData ())
            <*> (SpendingScript <$> arbitrary <@> Nothing)
        )

-- | Create a Marlowe semantics transaction, with mostly empty and default values.
mkBareSemanticsTransaction
  :: GoldenTransaction
  -> Gen (PlutusTransaction SemanticsTransaction)
mkBareSemanticsTransaction (_state, _contract, _input, _output) =
  do
    rolesCurrency <- arbitraryAnyCurrencySymbol
    let _params = MarloweParams{..}
    bareSpending SemanticsTransaction{..}

-- | Make the datum for a Marlowe semantics transaction.
makeSemanticsDatum
  :: MarloweParams
  -> State
  -> Contract
  -> Datum
makeSemanticsDatum params state contract = Datum . toBuiltinData $ MarloweData params state contract

-- | Make the redeemer for a Marlowe semantics transaction.
makeSemanticsRedeemer
  :: MarloweInput
  -> Redeemer
makeSemanticsRedeemer = Redeemer . toBuiltinData

-- | Convert Marlowe input to Marlowe transaction input.
inputToMarloweTxInput
  :: Input
  -> (MarloweTxInput, [(DatumHash, Datum)])
inputToMarloweTxInput (NormalInput content) = (Input content, [])
inputToMarloweTxInput (MerkleizedInput content hash contract) = (MerkleizedTxInput content hash, [(DatumHash hash, Datum $ toBuiltinData contract)])

newtype SetAsSpending = SetAsSpending Bool

-- | Create script input for a Marlowe semantics transaction and optionally mutate
-- | the script info to make the script context a spending one.
makeScriptInput :: SemanticsAddress -> Datum -> SetAsSpending -> ArbitraryTransactionM SemanticsTransaction (TxInInfo, (DatumHash, Datum))
makeScriptInput SemanticsAddress{address} inDatum (SetAsSpending setAsSpending) =
  do
    inValue <- inputState `uses` totalValue
    -- inDatum <- use datumGetter
    let inDatumHash = DatumHash $ dataHash inDatum
    -- pure (inDatumHash, inDatum))
    txOutRef <- lift arbitrary
    let
      txInInfo = TxInInfo txOutRef (TxOut address inValue (OutputDatumHash inDatumHash) Nothing)
    when setAsSpending $ do
      let spendingInfo = SpendingScript txOutRef (Just inDatum)
      scriptInfo .= spendingInfo

    pure (txInInfo, (inDatumHash, inDatum))

-- | Total the value in a Marlowe state.
totalValue :: State -> Value
totalValue = foldMap (\((_, Token c n), i) -> V.singleton c n i) . AM.toList . accounts

-- | Create deposit inputs for a Marlowe semantics transaction.
makeDeposit
  :: Input
  -> ArbitraryTransactionM SemanticsTransaction [TxInInfo]
makeDeposit input' =
  do
    ref <- lift arbitrary
    address' <- lift $ arbitrary `suchThat` notScriptAddress
    pure $
      case getInputContent input' of
        IDeposit _ (M.Address _ address) (Token c n) i ->
          if i > 0
            then pure . TxInInfo ref $ TxOut address (V.singleton c n i) NoOutputDatum Nothing
            else mempty
        IDeposit _ (Role _) (Token c n) i ->
          if i > 0
            then pure . TxInInfo ref $ TxOut address' (V.singleton c n i) NoOutputDatum Nothing
            else mempty
        _ -> mempty

-- | Create role input for a Marlowe semantics transaction.
makeRoleIn
  :: Input
  -> ArbitraryTransactionM SemanticsTransaction [TxInInfo]
makeRoleIn input' =
  do
    MarloweParams currencySymbol <- use Transactions.marloweParams
    ref <- lift arbitrary
    address <- lift $ arbitrary `suchThat` notScriptAddress
    pure $
      case getInputContent input' of
        IDeposit _ (Role role') _ _ -> pure . TxInInfo ref $ TxOut address (V.singleton currencySymbol role' 1) NoOutputDatum Nothing
        IChoice (ChoiceId _ (Role role')) _ -> pure . TxInInfo ref $ TxOut address (V.singleton currencySymbol role' 1) NoOutputDatum Nothing
        _ -> mempty

-- | Create script continuing output for a Marlowe semantics transaction.
makeScriptOutput :: SemanticsAddress -> ArbitraryTransactionM SemanticsTransaction ([TxOut], [(DatumHash, Datum)])
makeScriptOutput SemanticsAddress{address} =
  do
    params <- use Transactions.marloweParams
    outState <- Transactions.output `uses` txOutState
    outContract <- Transactions.output `uses` txOutContract
    let outDatum = Datum . toBuiltinData $ MarloweData params outState outContract
        outDatumHash = DatumHash $ dataHash outDatum
    pure $
      unzip
        [ ( TxOut address (totalValue outState) (OutputDatumHash outDatumHash) Nothing
          , (outDatumHash, outDatum)
          )
        | outContract /= Close
        ]

-- | Create role output for a Marlowe semantics transaction.
makeRoleOut
  :: TxInInfo
  -> ArbitraryTransactionM SemanticsTransaction TxOut
makeRoleOut (TxInInfo _ (TxOut _ token _ _)) =
  TxOut <$> lift arbitrary <*> pure token <*> pure NoOutputDatum <*> pure Nothing

-- | Create a payment for a Marlowe semantics transaction.
makePayment
  :: RolePayoutAddress
  -> CurrencySymbol
  -> Payment
  -> ArbitraryTransactionM SemanticsTransaction ([TxOut], [(DatumHash, Datum)])
makePayment _ _ payment@(Payment _ (Party (M.Address _ address)) _ _) =
  pure
    ( pure $ TxOut address (paymentMoney payment) NoOutputDatum Nothing
    , mempty
    )
makePayment payoutAddress currencySymbol payment@(Payment _ (Party (Role role')) _ _) =
  do
    let roleDatum = Datum $ toBuiltinData (currencySymbol, role')
        roleDatumHash = DatumHash $ dataHash roleDatum
    pure
      ( pure $ TxOut payoutAddress.address (paymentMoney payment) (OutputDatumHash roleDatumHash) Nothing
      , pure (roleDatumHash, roleDatum)
      )
makePayment _ _ _ = pure (mempty, mempty)

-- | Create a deposit or choice signatory for a Marlowe semantics transaction.
makeActionSignatory
  :: Input
  -> [PubKeyHash]
makeActionSignatory input' =
  case getInputContent input' of
    IDeposit _ (M.Address _ (Address (PubKeyCredential pkh) _)) _ _ -> pure pkh
    IChoice (ChoiceId _ (M.Address _ (Address (PubKeyCredential pkh) _))) _ -> pure pkh
    _ -> mempty

-- | Create a spending signatory for a Marlowe semantics transaction.
makeSpendSignatory
  :: TxInInfo
  -> [PubKeyHash]
makeSpendSignatory (TxInInfo _ (TxOut (Address (PubKeyCredential pkh) _) _ _ _)) = pure pkh
makeSpendSignatory _ = mempty

-- | Combine payments with the same address and hash.
consolidatePayments :: [TxOut] -> [TxOut]
consolidatePayments ps =
  let extractAddressHash (TxOut address _ hash _) = (address, hash)
      extractValue (TxOut _ value _ _) = value
   in [ TxOut address value hash Nothing
      | ah@(address, hash) <- nub $ extractAddressHash <$> ps
      , let value = foldMap extractValue $ filter ((== ah) . extractAddressHash) ps
      ]

mappendPlutusTxAssocMapFirst
  :: forall k v
   . P.Eq k
  => PlutusTxAM.Map k v
  -> PlutusTxAM.Map k v
  -> PlutusTxAM.Map k v
mappendPlutusTxAssocMapFirst = PlutusTxAM.unionWith const

-- | Generate a valid Marlowe semantics transaction.
genValidSemanticsTransaction
  :: SemanticsAddress
  -> RolePayoutAddress
  -> AddNoise
  -- ^ Whether to add noise to the script context.
  -> ArbitraryTransactionM SemanticsTransaction ()
  -- ^ The generator.
genValidSemanticsTransaction semanticsAddress rolePayoutAddress (AddNoise addNoise) =
  do
    datum <- makeSemanticsDatum <$> use Transactions.marloweParams <*> use inputState <*> use inputContract
    -- -- The datum is `MarloweData`.
    -- datum <~ makeSemanticsDatum <$> use Transactions.marloweParams <*> use inputState <*> use inputContract

    -- The redeemer is `MarloweInput`, but we also track the merkleizations.
    (marloweInput, merkleizations) <- input `uses` (second mconcat . unzip . fmap inputToMarloweTxInput . txInputs)
    let
      semanticsRedeemer = makeSemanticsRedeemer marloweInput
    redeemer .= semanticsRedeemer
    infoData %= mappendPlutusTxAssocMapFirst (PlutusTxAM.unsafeFromList merkleizations)

    -- Add the spending from the script.
    (inScript, inData) <- makeScriptInput semanticsAddress datum (SetAsSpending True)
    infoInputs <>= [inScript]
    infoData %= mappendPlutusTxAssocMapFirst (PlutusTxAM.unsafeFromList [inData])

    -- Add the role inputs.
    roleInputs <- fmap concat . mapM makeRoleIn . txInputs =<< use input
    infoInputs <>= roleInputs

    -- Add the deposits.
    deposits <- do
      TransactionInput { txInputs=txIns } <- use input
      concat <$> for txIns makeDeposit

    infoInputs <>= deposits
    -- Add the redeemer to the redeemers info.
    -- In general it is ignored.
    infoRedeemers %= \redeemers -> do
      let
        TxInInfo txOutRef _ = inScript
        spendingRedeemer = (Spending txOutRef, semanticsRedeemer)
        redeemersList = PlusTxAM.toList redeemers
      PlutusTxAM.unsafeFromList $ spendingRedeemer : redeemersList

    -- Add the script output.
    (outScript, outData) <- makeScriptOutput semanticsAddress
    infoOutputs <>= outScript
    infoData %= mappendPlutusTxAssocMapFirst (PlutusTxAM.unsafeFromList outData)

    -- Add the role outputs.
    infoOutputs <><~ mapM makeRoleOut roleInputs

    MarloweParams currencySymbol <- use Transactions.marloweParams
    -- Add the payments.
    (payments, paymentData) <-
      fmap (bimap mconcat mconcat . unzip) . mapM (makePayment rolePayoutAddress currencySymbol) =<< (Transactions.output `uses` txOutPayments)
    infoOutputs <>= consolidatePayments payments
    infoData %= mappendPlutusTxAssocMapFirst (PlutusTxAM.unsafeFromList paymentData)

    -- Add the signatories.
    actionSignatories <- concatMap makeActionSignatory <$> input `uses` txInputs
    spendSignatories <- concatMap makeSpendSignatory <$> use infoInputs
    infoSignatories <>= nub (actionSignatories <> spendSignatories)

    -- Set the validity interval.
    interval <- input `uses` txInterval
    infoValidRange .= Interval (LowerBound (Finite $ fst interval) True) (UpperBound (Finite $ snd interval) True)

    -- Set the fee.
    infoFee <~ Lovelace <$> lift arbitraryPositiveInteger

    -- Add noise.
    when addNoise doAddNoise

    -- Shuffle.
    shuffleTransaction

-- | Generate an arbitrary, valid Marlowe semantics transaction: datum, redeemer, and script context.
arbitrarySemanticsTransaction
  :: SemanticsAddress
  -> RolePayoutAddress
  -- ^ The scripts being tested.
  -> [ReferencePath]
  -- ^ The reference execution paths from which to choose.
  -> ArbitraryTransactionM SemanticsTransaction ()
  -- ^ Modifications to make before building the valid transaction.
  -> ArbitraryTransactionM SemanticsTransaction ()
  -- ^ Modifications to make after building the valid transaction.
  -> AddNoise
  -- ^ Whether to add noise to the script context.
  -> AllowMerkleization
  -- ^ Whether to allow merkleization.
  -> Gen (PlutusTransaction SemanticsTransaction)
  -- ^ The generator.
arbitrarySemanticsTransaction semanticsAddress rolePayoutAddress referencePaths modifyBefore modifyAfter noisy (AllowMerkleization allowMerkleization) =
  do
    golden <-
      frequency
        [ (1, arbitraryGoldenTransaction allowMerkleization) -- Manually vetted transactions.
        , (5, arbitraryReferenceTransaction referencePaths) -- Transactions generated using `getAllInputs` and `computeTransaction`.
        ]
    semanticsTransactionFromGolden semanticsAddress rolePayoutAddress golden modifyBefore modifyAfter noisy

semanticsTransactionFromGolden
  :: SemanticsAddress
  -> RolePayoutAddress
  -> GoldenTransaction
  -> ArbitraryTransactionM SemanticsTransaction ()
  -> ArbitraryTransactionM SemanticsTransaction ()
  -> AddNoise
  -> Gen (PlutusTransaction SemanticsTransaction)
semanticsTransactionFromGolden semanticsAddress payoutAddress golden modifyBefore modifyAfter addNoise = do
  start <- mkBareSemanticsTransaction golden
  (modifyBefore >> genValidSemanticsTransaction semanticsAddress payoutAddress addNoise >> modifyAfter)
    `execStateT` start

-- | Create a Marlowe payout transaction, with mostly empty and default values.
barePayoutTransaction :: Gen (PlutusTransaction PayoutTransaction)
barePayoutTransaction =
  do
    rolesCurrency <- arbitraryNonAdaCurrencySymbol
    let _params' = MarloweParams{..}
    _role <- arbitrary
    _amount <- arbitrary `suchThat` (`gt` mempty)
    bareSpending PayoutTransaction{..}

-- | Create a script input for a Marlowe payout transaction.
makePayoutIn :: RolePayoutAddress -> SetAsSpending -> ArbitraryTransactionM PayoutTransaction (TxInInfo, (DatumHash, Datum))
makePayoutIn payoutAddress (SetAsSpending setAsSpending) =
  do
    txInInfoOutRef <- lift arbitrary
    inDatum <- ((Datum . toBuiltinData) .) . (,) <$> marloweParamsPayout `uses` rolesCurrency <*> use role
    let inDatumHash = DatumHash $ dataHash inDatum
    txInInfoResolved <- TxOut payoutAddress.address <$> use amount <*> pure (OutputDatumHash inDatumHash) <*> pure Nothing
    when setAsSpending $ do
      let spendingInfo = SpendingScript txInInfoOutRef (Just inDatum)
      scriptInfo .= spendingInfo
    pure (TxInInfo{..}, (inDatumHash, inDatum))

-- | Create a role input for a Marlowe payout transaction.
makePayoutRoleIn :: ArbitraryTransactionM PayoutTransaction TxInInfo
makePayoutRoleIn =
  do
    ref <- lift arbitrary
    address <- lift $ arbitrary `suchThat` notScriptAddress
    value <- V.singleton <$> marloweParamsPayout `uses` rolesCurrency <*> use role <*> pure 1
    pure
      . TxInInfo ref
      $ TxOut address value NoOutputDatum Nothing

-- | Create a payment output for a Marlowe payout transaction.
makePayoutOut :: ArbitraryTransactionM PayoutTransaction TxOut
makePayoutOut =
  TxOut
    <$> lift arbitrary
    <*> use amount
    <*> pure NoOutputDatum
    <*> pure Nothing

-- | Create a role output for a Marlowe payout transaction.
makePayoutRoleOut :: ArbitraryTransactionM PayoutTransaction TxOut
makePayoutRoleOut =
  do
    address <- lift arbitrary
    value <- V.singleton <$> marloweParamsPayout `uses` rolesCurrency <*> use role <*> pure 1
    pure $
      TxOut address value NoOutputDatum Nothing

-- | Create a spending signatory for a Marlowe payout transaction.
makePayoutSignatory
  :: TxInInfo
  -> [PubKeyHash]
makePayoutSignatory (TxInInfo _ (TxOut (Address (PubKeyCredential pkh) _) _ _ _)) = pure pkh
makePayoutSignatory _ = mempty

-- | Generate a valid Marlowe payout transaction.
genValidPayoutTransaction
  :: RolePayoutAddress
  -- ^ The scripts being tests.
  -> AddNoise
  -- ^ Whether to add noise to the script context.
  -> ArbitraryTransactionM PayoutTransaction ()
  -- ^ The generator.
genValidPayoutTransaction rolePayoutAddress (AddNoise noisy) =
  do
    -- Add the script input.
    (inScript, inData) <- makePayoutIn rolePayoutAddress (SetAsSpending True)
    infoInputs <>= [inScript]
    infoData %= mappendPlutusTxAssocMapFirst (PlutusTxAM.unsafeFromList [inData])

    -- The redeemer is unit.
    redeemer .= Redeemer (toBuiltinData ())

    -- Add the role input.
    infoInputs <><~ (pure <$> makePayoutRoleIn)

    -- Add the pay output.
    infoOutputs <><~ (pure <$> makePayoutOut)

    -- Add the role output.
    infoOutputs <><~ (pure <$> makePayoutRoleOut)

    -- Add the signatories.
    infoSignatories <><~ (infoInputs `uses` concatMap makePayoutSignatory)

    -- Set the fee.
    infoFee <~ Lovelace <$> lift arbitraryPositiveInteger

    -- Add noise.
    when noisy doAddNoise'

    -- Shuffle.
    shuffleTransaction

-- | Check that an address is not for a script.
notScriptAddress :: Address -> Bool
notScriptAddress (Address (ScriptCredential _) _) = False
notScriptAddress _ = True

-- -- | Check that an input is not from a script.
notScriptTxIn :: TxInInfo -> Bool
notScriptTxIn = not . isScriptTxIn

-- | Check that an input is from a script.
isScriptTxIn :: TxInInfo -> Bool
isScriptTxIn TxInInfo{txInInfoResolved = TxOut{txOutAddress = Address (ScriptCredential _) _}} = True
isScriptTxIn _ = False

-- | Add noise to the inputs, outputs, and data in a Plutus transaction.
doAddNoise :: ArbitraryTransactionM SemanticsTransaction ()
doAddNoise =
  do
    let isPayout (Payment _ (Party _) _ i) = i > 0
        isPayout _ = False
    hasPayments <- any isPayout . txOutPayments <$> use Transactions.output
    let arbitraryInput =
          if hasPayments
            then arbitrary `suchThat` notScriptTxIn
            else arbitrary
    infoInputs <><~ lift (listOf arbitraryInput `suchThat` ((< 5) . length))
    infoOutputs <><~ lift (arbitrary `suchThat` ((< 5) . length))
    datumHashMap <- lift $ arbitraryDatumHashMap (MaxLength 5)
    infoData %= mappendPlutusTxAssocMapFirst datumHashMap

newtype MaxLength = MaxLength Int

arbitraryDatumHashMap :: MaxLength -> Gen (PlutusTxAM.Map DatumHash Datum)
arbitraryDatumHashMap (MaxLength maxLength) = do
  pairs <- arbitrary `suchThat` ((< maxLength) . length)
  pure $ PlutusTxAM.unsafeFromList pairs


-- | Add noise to the inputs, outputs, and data in a Plutus transaction.
doAddNoise' :: ArbitraryTransactionM PayoutTransaction ()
doAddNoise' =
  do
    infoInputs <><~ lift (arbitrary `suchThat` ((< 5) . length))
    infoOutputs <><~ lift (arbitrary `suchThat` ((< 5) . length))
    datumHashMap <- lift $ arbitraryDatumHashMap (MaxLength 5)
    infoData %= mappendPlutusTxAssocMapFirst datumHashMap

-- | Shuffle the order of inputs, outputs, data, and signatories in a Plutus transaction.
shuffleTransaction :: ArbitraryTransactionM a ()
shuffleTransaction =
  do
    let go :: Lens' (PlutusTransaction a) [b] -> ArbitraryTransactionM a ()
        go field = field <~ (lift . shuffle =<< use field)
    go infoInputs
    go infoOutputs
    go infoSignatories
    infoData <~ (lift . fmap PlutusTxAM.unsafeFromList . shuffle . PlutusTxAM.toList =<< use infoData)

merkleize :: ArbitraryTransactionM SemanticsTransaction ()
merkleize = do
  state    <- use inputState
  contract <- use inputContract
  inputs   <- use input
  let (mcContract, mcContinuations) = runWriter $ deepMerkleize contract
      inputs' =
        either error id $
          merkleizeInputs MerkleizedContract{..} state inputs
  inputContract .= mcContract
  input         .= inputs'
  Transactions.output     .= computeTransaction inputs' state mcContract

-- | Generate an arbitrary, valid Marlowe payout transaction: datum, redeemer, and script context.
arbitraryPayoutTransaction
  :: RolePayoutAddress
  -- ^ The scripts being tested.
  -> ArbitraryTransactionM PayoutTransaction ()
  -- ^ Modifications to make before building the valid transaction.
  -> ArbitraryTransactionM PayoutTransaction ()
  -- ^ Modifications to make after building the valid transaction.
  -> AddNoise
  -- ^ Whether to add noise to the script context.
  -> Gen (PlutusTransaction PayoutTransaction)
  -- ^ The generator.
arbitraryPayoutTransaction rolePayoutAddress modifyBefore modifyAfter addNoise =
  do
    start <- barePayoutTransaction
    (modifyBefore >> genValidPayoutTransaction rolePayoutAddress addNoise >> modifyAfter)
      `execStateT` start

-- | Do not modify a Plutus transaction.
noModify :: ArbitraryTransactionM a ()
noModify = pure ()

-- | Do not eliminate generated Plutus transactions.
noVeto :: PlutusTransaction a -> Bool
noVeto = const True

-- | Check that a semantics transaction succeeds.
checkSemanticsTransaction
  :: MarloweScripts
  -- ^ The scripts being tested.
  -> Maybe LogOutput
  -- ^ At least one of these required log messages must be reported if the validation fails.
  -> [ReferencePath]
  -- ^ The reference execution paths from which to choose.
  -> ArbitraryTransactionM SemanticsTransaction ()
  -- ^ Modifications to make before building the valid transaction.
  -> ArbitraryTransactionM SemanticsTransaction ()
  -- ^ Modifications to make after building the valid transaction.
  -> (PlutusTransaction SemanticsTransaction -> Bool)
  -- ^ Whether to discard the transaction from the testing.
  -> Valid
  -- ^ Whether the transaction should test as valid.
  -> AddNoise
  -- ^ Whether to add noise to the script context.
  -> AllowMerkleization
  -- ^ Whether to allow merkleization.
  -> Property
  -- ^ The test property.
checkSemanticsTransaction scripts requiredLog referencePaths modifyBefore modifyAfter condition (Valid valid) addNoise allowMerkleization =
  property
    . forAll
      ( arbitrarySemanticsTransaction scripts.semanticsAddress scripts.rolePayoutAddress referencePaths modifyBefore modifyAfter addNoise allowMerkleization
          `suchThat` condition
      )
    $ \PlutusTransaction{..} ->
      case runSemantics scripts (toData _scriptContext) of
        This e -> not valid || error (show e)
        These e l -> not valid && matchesPlutusLog l || error (show e <> ": " <> show l)
        That _ -> valid
  where
    matchesPlutusLog l = case requiredLog of
      Nothing -> True
      (Just rl) -> any (`elem` l) rl

-- _checkSemanticsGoldenTransactions
--   :: MarloweScripts
--   -> Maybe LogOutput
--   -> [GoldenTransaction]
--   -> ArbitraryTransactionM SemanticsTransaction ()
--   -> ArbitraryTransactionM SemanticsTransaction ()
--   -> (PlutusTransaction SemanticsTransaction -> Bool)
--   -> Valid
--   -> AddNoise
--   -> Property
-- _checkSemanticsGoldenTransactions scripts requiredLog goldens modifyBefore modifyAfter condition (Valid valid) noisy =
--   conjoin
--     [ counterexample (show golden) $
--         property
--           . forAll
--             ( semanticsTransactionFromGolden scripts golden modifyBefore modifyAfter noisy
--                 `suchThat` condition
--             )
--           $ \PlutusTransaction{..} ->
--               case runSemantics scripts (toData _scriptContext) of
--                 This e -> not valid || error (show e)
--                 These e l -> not valid && matchesPlutusLog l || error (show e <> ": " <> show l)
--                 That _ -> valid
--     | golden <- goldens
--     ]
--  where
--   matchesPlutusLog l = case requiredLog of
--     Nothing -> True
--     Just rl -> any (`elem` l) rl

-- | Check that a payout transaction succeeds.
checkPayoutTransaction
  :: MarloweScripts
  -- ^ The scripts being tested.
  -> ArbitraryTransactionM PayoutTransaction ()
  -- ^ Modifications to make before building the valid transaction.
  -> ArbitraryTransactionM PayoutTransaction ()
  -- ^ Modifications to make after building the valid transaction.
  -> (PlutusTransaction PayoutTransaction -> Bool)
  -- ^ Whether to discard the transaction from the testing.
  -> Valid
  -- ^ Whether the transaction should test as valid.
  -> AddNoise
  -- ^ Whether to add noise to the script context.
  -> Property
  -- ^ The test property.
checkPayoutTransaction scripts modifyBefore modifyAfter condition (Valid valid) addNoise =
  property
    . forAll (arbitraryPayoutTransaction scripts.rolePayoutAddress modifyBefore modifyAfter addNoise `suchThat` condition)
    $ \PlutusTransaction{..} ->
      case runPayout scripts (toData _scriptContext) of
        This e -> not valid || error (show e)
        These e l -> not valid || error (show e <> ": " <> show l)
        That _ -> valid

newtype CheckPlutusLog = CheckPlutusLog Bool

-- | Check that validation fails if two Marlowe scripts are run.
checkDoubleInput :: MarloweScripts -> CheckPlutusLog -> [ReferencePath] -> Property
checkDoubleInput scripts@MarloweScripts{semanticsAddress} (CheckPlutusLog checkPlutusLog) referencePaths = do
  let
    modifyAfter =
        do
          -- Create a random datum.
          inDatum <- lift arbitrary
          let inDatumHash = DatumHash $ dataHash inDatum
          -- Create a random input to the script.
          inScript <-
            TxInInfo
              <$> lift arbitrary
              <*> (TxOut semanticsAddress.address <$> lift arbitrary <*> pure (OutputDatumHash inDatumHash) <*> pure Nothing)
          -- Add a second script input.
          infoInputs <>= [inScript]
          -- Add the new datum and its hash.
          infoData %= mappendPlutusTxAssocMapFirst (PlutusTxAM.unsafeFromList [(inDatumHash, inDatum)])
          shuffleTransaction
    plutusLog = if checkPlutusLog
          then Just ["w"]
          else Nothing
  checkSemanticsTransaction scripts plutusLog referencePaths noModify modifyAfter noVeto (Valid False) (AddNoise False) (AllowMerkleization False)

-- | Split a value in half.
splitValue :: Value -> [Value]
splitValue value =
  concat
    [ [ singleton c n half
      , singleton c n (i - half)
      ]
    | (c, n, i) <- flattenValue value
    , let half = i `div` 2
    ]

-- | Ensure that the contract closes the contract.
doesClose :: PlutusTransaction SemanticsTransaction -> Bool
doesClose = (== Close) . txOutContract . (^. Transactions.output)

-- | Ensure that the transaction does not close the contract.
notCloses :: PlutusTransaction SemanticsTransaction -> Bool
notCloses = not . doesClose

-- | Ensure that the contract makes payments.
hasPayouts :: PlutusTransaction SemanticsTransaction -> Bool
hasPayouts =
  let isPayout (Payment _ (Party _) _ i) = i > 0
      isPayout _ = False
   in any isPayout . txOutPayments . (^. Transactions.output)

-- | Check that validation fails if there is more than one Marlowe output.
checkMultipleOutput :: MarloweScripts -> CheckPlutusLog -> [ReferencePath] -> Property
checkMultipleOutput scripts@MarloweScripts{semanticsAddress} (CheckPlutusLog checkPlutusLog) referencePaths =
  let modifyAfter =
        do
          let -- Split a script output into two equal ones.
              splitOwnOutput txOut@(TxOut address value datum' _)
                | address == semanticsAddress.address = flip (TxOut address) datum' <$> splitValue value <*> pure Nothing
                | otherwise = pure txOut
          -- Update the outputs with the split script output.
          infoOutputs %= concatMap splitOwnOutput
          shuffleTransaction
      plutusLog = if checkPlutusLog
            then Just ["o"]
            else Nothing
   in checkSemanticsTransaction scripts plutusLog referencePaths noModify modifyAfter notCloses (Valid False) (AddNoise False) (AllowMerkleization False)

-- | Check that validation fails if there is one Marlowe output upon close.
checkCloseOutput :: MarloweScripts -> CheckPlutusLog -> [ReferencePath] -> Property
checkCloseOutput scripts@MarloweScripts{semanticsAddress} (CheckPlutusLog checkPlutusLog) referencePaths =
  let modifyAfter =
        do
          let -- Match the script input.
              matchOwnInput (TxInInfo _ (TxOut address _ _ _)) = address == semanticsAddress.address
          -- Find the script input.
          inScript <- infoInputs `uses` filter matchOwnInput
          -- Add a clone of the script input as output.
          infoOutputs <>= (txInInfoResolved <$> inScript)
          shuffleTransaction
      plutusLog = if checkPlutusLog
            then Just ["c"]
            else Nothing
   in checkSemanticsTransaction scripts plutusLog referencePaths noModify modifyAfter doesClose (Valid False) (AddNoise False) (AllowMerkleization False)

-- | Check that value output to a script matches its expectation.
checkValueOutput :: MarloweScripts -> CheckPlutusLog -> [ReferencePath] -> Property
checkValueOutput scripts@MarloweScripts{semanticsAddress} (CheckPlutusLog checkPlutusLog) referencePaths =
  let modifyAfter =
        do
          delta <- lift $ oneof [chooseInteger (-5, -1), chooseInteger (1, 5), arbitrary `suchThat` (/= 0)] -- Ensure small non-zero integers.
          let -- Add or subtract some lovelace to the output to the script.
              incrementOwnOutput txOut@(TxOut address value _ _)
                | address == semanticsAddress.address = txOut{txOutValue = value <> singleton adaSymbol adaToken delta}
                | otherwise = txOut
          -- Update the outputs with the incremented script output.
          infoOutputs %= fmap incrementOwnOutput
      plutusLog = if checkPlutusLog
            then Just ["d"]
            else Nothing
   in checkSemanticsTransaction scripts plutusLog referencePaths noModify modifyAfter notCloses (Valid False) (AddNoise False) (AllowMerkleization False)

-- | Check the consistency of the output value with the output state.
checkOutputConsistency :: MarloweScripts -> [ReferencePath] -> Property
checkOutputConsistency scripts@MarloweScripts{semanticsAddress} referencePaths =
  property
    . forAll (arbitrarySemanticsTransaction scripts.semanticsAddress scripts.rolePayoutAddress referencePaths noModify noModify (AddNoise False) (AllowMerkleization True))
    $ \tx ->
      let findOwnOutput (TxOut address value _ _)
            | address == semanticsAddress.address = value
            | otherwise = mempty
          outValue = foldMap findOwnOutput $ tx ^. infoOutputs
          finalBalance = totalBalance . accounts . txOutState $ tx ^. Transactions.output
          -- There is really no way to provoke this invalidity in a manner that isn't covered by other tests.
          valid = outValue == finalBalance
       in checkSemanticsTransaction scripts Nothing referencePaths noModify noModify notCloses (Valid valid) (AddNoise False) (AllowMerkleization False)

-- | Check that output datum to a script matches its semantic output.
checkDatumOutput :: MarloweScripts -> CheckPlutusLog -> [ReferencePath] -> (MarloweData -> Gen MarloweData) -> Property
checkDatumOutput scripts (CheckPlutusLog checkPlutusLog) referencePaths perturb =
  let modifyAfter =
        do
          -- Find the existing Marlowe data output.
          marloweData <- MarloweData <$> use Transactions.marloweParams <*> Transactions.output `uses` txOutState <*> Transactions.output `uses` txOutContract
          -- Compute its hash.
          let outDatumHash = DatumHash $ dataHash marloweData
          -- Modify the original datum.
          outDatum' <- fmap (Datum . toBuiltinData) . lift $ perturb marloweData
          -- Let
          let -- Replace an output datum with the modification.
              perturbOwnOutputDatum pair@(h, _)
                | h == outDatumHash = (h, outDatum')
                | otherwise = pair
          -- Update the data with the modification.
          infoData %= PlutusTxAM.unsafeFromList . fmap perturbOwnOutputDatum . PlutusTxAM.toList
      plutusLog = if checkPlutusLog
            then Just ["d"]
            else Nothing
   in checkSemanticsTransaction scripts plutusLog referencePaths noModify modifyAfter notCloses (Valid False) (AddNoise False) (AllowMerkleization False)

-- | Check that other validators are forbidden during payments.
checkOtherValidators :: MarloweScripts -> CheckPlutusLog -> [ReferencePath] -> Property
checkOtherValidators scripts (CheckPlutusLog checkPlutusLog) referencePaths =
  let modifyAfter =
        -- Add an extra script input.
        infoInputs <><~ lift (listOf1 $ makeScriptTxIn =<< arbitrary)
      plutusLog = if checkPlutusLog
            then Just ["z"]
            else Nothing
   in checkSemanticsTransaction scripts plutusLog referencePaths noModify modifyAfter hasPayouts (Valid False) (AddNoise False) (AllowMerkleization False)

makeScriptTxIn :: TxInInfo -> Gen TxInInfo
makeScriptTxIn (TxInInfo outRef out) = TxInInfo outRef <$> makeScriptTxOut out

makeScriptTxOut :: TxOut -> Gen TxOut
makeScriptTxOut out = do
  address' <- Address <$> (ScriptCredential <$> arbitrary) <*> arbitrary
  pure $ out{txOutAddress = address'}

-- | Check that parameters in the datum are not changed by the transaction.
checkParamsOutput :: MarloweScripts -> CheckPlutusLog -> [ReferencePath] -> Property
checkParamsOutput scripts checkPlutusLog referencePaths =
  checkDatumOutput scripts checkPlutusLog referencePaths $
    \marloweData ->
      do
        -- Replace the output parameters with a random one.
        let old = M.marloweParams marloweData
        new <- arbitrary `suchThat` (/= old)
        pure $ marloweData{M.marloweParams = new}

-- | Check that state output to a script matches its semantic output.
checkStateOutput :: MarloweScripts -> CheckPlutusLog -> [ReferencePath] -> Property
checkStateOutput scripts checkPlutusLog referencePaths =
  checkDatumOutput scripts checkPlutusLog referencePaths $
    \marloweData ->
      do
        -- Replace the output state with a random one.
        let old = marloweState marloweData
        new <- arbitrary `suchThat` (/= old)
        pure $ marloweData{marloweState = new}

-- | Check that contract output to a script matches its semantic output.
checkContractOutput :: MarloweScripts -> CheckPlutusLog -> [ReferencePath] -> Property
checkContractOutput scripts checkPlutusLog referencePaths =
  checkDatumOutput scripts checkPlutusLog referencePaths $
    \marloweData ->
      do
        -- Replace the output contract with a random one.
        let old = marloweContract marloweData
        new <- arbitrary `suchThat` (/= old)
        pure $ marloweData{marloweContract = new}

-- | Check that the input contract is merkleized.
hasMerkleizedInput :: PlutusTransaction SemanticsTransaction -> Bool
hasMerkleizedInput =
  let isMerkleized NormalInput{} = False
      isMerkleized MerkleizedInput{} = True
   in any isMerkleized . txInputs . (^. input)

-- | Check than an invalid merkleization is rejected.
checkMerkleization :: MarloweScripts -> CheckPlutusLog -> [ReferencePath] -> MakeInvalid -> Property
checkMerkleization scripts (CheckPlutusLog checkPlutusLog) referencePaths (MakeInvalid makeInvalid) =
  let -- Merkleized the contract and its input.
      modifyBefore = merkleize
      -- Extract the merkle hash, if any.
      merkleHash (NormalInput _) = mempty
      merkleHash (MerkleizedInput _ hash _) = pure $ DatumHash hash
      -- Modify the contract if requested.
      modifyAfter =
        when makeInvalid do
          -- Remove the merkleized continuation datums for the input.
          hashes <- input `uses` (concatMap merkleHash . txInputs)
          infoData %= (PlutusTxAM.unsafeFromList . filter ((`notElem` hashes) . fst) . PlutusTxAM.toList)
      plutusLog = if checkPlutusLog
            then Just ["h"]
            else Nothing
   in checkSemanticsTransaction scripts plutusLog referencePaths modifyBefore modifyAfter hasMerkleizedInput (Valid $ not makeInvalid) (AddNoise False) (AllowMerkleization False)

-- | Compute the authorization for an input.
authorizer :: Input -> ([PubKeyHash], [TokenName])
authorizer (NormalInput (IDeposit _ (M.Address _ address) _ _)) = (maybeToList $ toPubKeyHash address, mempty)
authorizer (NormalInput (IDeposit _ (Role role') _ _)) = (mempty, pure role')
authorizer (NormalInput (IChoice (ChoiceId _ (M.Address _ address)) _)) = (maybeToList $ toPubKeyHash address, mempty)
authorizer (NormalInput (IChoice (ChoiceId _ (Role role')) _)) = (mempty, pure role')
authorizer (MerkleizedInput (IDeposit _ (M.Address _ address) _ _) _ _) = (maybeToList $ toPubKeyHash address, mempty)
authorizer (MerkleizedInput (IDeposit _ (Role role') _ _) _ _) = (mempty, pure role')
authorizer (MerkleizedInput (IChoice (ChoiceId _ (M.Address _ address)) _) _ _) = (maybeToList $ toPubKeyHash address, mempty)
authorizer (MerkleizedInput (IChoice (ChoiceId _ (Role role')) _) _ _) = (mempty, pure role')
authorizer _ = (mempty, mempty)

-- | Determine whether there are any authorizations in the transaction.
hasAuthorizations :: PlutusTransaction SemanticsTransaction -> Bool
hasAuthorizations = (/= ([], [])) . bimap concat concat . unzip . fmap authorizer . txInputs . (^. input)

-- | Delete only the first matching value in a list.
deleteFirst
  :: (a -> Bool)
  -- ^ The condition for deleting an element.
  -> [a]
  -- ^ The list.
  -> [a]
  -- ^ The list with the first matching element removed.
deleteFirst f z =
  case break f z of
    (x, []) -> x
    (x, y) -> x <> tail y

-- | Check that a missing authorization causes failure.
checkAuthorization :: MarloweScripts -> CheckPlutusLog -> [ReferencePath] -> Property
checkAuthorization scripts (CheckPlutusLog checkPlutusLog) referencePaths =
  let modifyAfter =
        do
          currency <- Transactions.marloweParams `uses` rolesCurrency
          -- Determine the authorizations.
          (pkhs, roles) <- input `uses` (bimap concat concat . unzip . fmap authorizer . txInputs)
          let -- Determine whether a role token is present.
              matchRole TxInInfo{txInInfoResolved = TxOut{txOutValue}} = any (\role' -> valueOf txOutValue currency role' > 0) roles
              -- Determine whether a PKH authorization is present.
              matchPkh = (`elem` pkhs)
          -- Remove the first role token from the input.
          infoInputs %= deleteFirst matchRole
          -- Remove the first PKH signatory.
          infoSignatories %= deleteFirst matchPkh
      plutusLog = if checkPlutusLog
            then Just ["s", "t"]
            else Nothing
   in checkSemanticsTransaction scripts plutusLog referencePaths noModify modifyAfter hasAuthorizations (Valid False) (AddNoise False) (AllowMerkleization False)

-- | Determine whether there are any external payments in a transaction.
hasExternalPayments :: PlutusTransaction SemanticsTransaction -> Bool
hasExternalPayments = any externalPayment . txOutPayments . (^. Transactions.output)

-- | Determine whether a payment is external.
externalPayment :: Payment -> Bool
externalPayment (Payment _ (Party _) _ q) = q > 0
externalPayment _ = False

-- | Decrement the value of each token by one.
decrementValue :: Value -> Value
decrementValue = foldMap (\(c, n, i) -> singleton c n (i - 1)) . flattenValue

-- | Check that an insufficient payment causes failure.
checkPayment :: MarloweScripts -> CheckPlutusLog -> [ReferencePath] -> Property
checkPayment scripts@MarloweScripts{rolePayoutAddress} (CheckPlutusLog checkPlutusLog) referencePaths =
  let modifyAfter =
        do
          let -- Decrement a payment by one unit.
              decrementPayment txOut@(TxOut address value (OutputDatumHash _) _)
                | address == rolePayoutAddress.address =
                    txOut{txOutValue = decrementValue value}
                | otherwise = txOut
              decrementPayment txOut@(TxOut (Address (PubKeyCredential _) _) value NoOutputDatum _) = txOut{txOutValue = decrementValue value}
              decrementPayment txOut = txOut
          -- Update the outputs.
          infoOutputs %= fmap decrementPayment
      plutusLog = if checkPlutusLog
            then Just ["p", "r"]
            else Nothing
   in checkSemanticsTransaction scripts plutusLog referencePaths noModify modifyAfter hasExternalPayments (Valid False) (AddNoise False) (AllowMerkleization False)

-- | Remove a role input UTxOs from the transaction.
removeRoleIn :: ArbitraryTransactionM PayoutTransaction ()
removeRoleIn =
  do
    -- Determine the roles currency and name.
    currency <- marloweParamsPayout `uses` rolesCurrency
    name <- use role
    let -- Determine if the input has the role token.
        notMatch (TxInInfo _ (TxOut _ value _ _)) = valueOf value currency name == 0
    -- Update the transaction inputs
    infoInputs %= filter notMatch

-- | Change the role name in an input UTxO from the transaction.
mutateRoleIn :: ArbitraryTransactionM PayoutTransaction ()
mutateRoleIn =
  do
    -- Determine the roles currency and name.
    currency <- marloweParamsPayout `uses` rolesCurrency
    name <- use role
    -- Randomly choose a different name.
    name' <- lift $ arbitrary `suchThat` (/= name)
    let -- Mutate the roles currency.
        mutate txIn@(TxInInfo _ (TxOut _ value _ _)) =
          if valueOf value currency name /= 0
            then txIn{txInInfoResolved = (txInInfoResolved txIn){txOutValue = singleton currency name' 1}}
            else txIn
    -- Update the transaction inputs
    infoInputs %= fmap mutate

-- | Check that an invalid withdrawal transaction fails.
checkWithdrawal
  :: MarloweScripts
  -> Bool
  -> Property
checkWithdrawal scripts mutate =
  checkPayoutTransaction
    scripts
    noModify
    (if mutate then mutateRoleIn else removeRoleIn)
    noVeto
    (Valid False)
    (AddNoise False)

-- | Check that a validator hash is correct.
-- checkValidatorHash
--   :: ScriptHash
--   -> ScriptHash
--   -> Property
-- checkValidatorHash actual reference =
--   property $ actual === reference

