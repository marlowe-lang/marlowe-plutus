{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- | Marlowe tests.
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
module Spec.Marlowe.Marlowe (
  -- * Testing
  prop_noFalsePositives,
  prop_showWorksForContracts,
  tests,
) where

import Cardano.Api (SerialiseAsRawBytes (deserialiseFromRawBytes))
import qualified Cardano.Api as C
import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Data.Aeson (decode, eitherDecode, encode)
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor (first)
import Data.Either (isRight)
import Data.Maybe (isJust, isNothing)
import Data.SBV ()
import qualified Data.Set as Set
import Data.String (IsString (fromString))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Lazy (toStrict)
import qualified Paths_marlowe_plutus as Paths
import GHC.IO (unsafePerformIO)
import Language.Haskell.Interpreter (
  Extension (OverloadedStrings),
  MonadInterpreter,
  OptionVal ((:=)),
  as,
  interpret,
  languageExtensions,
  runInterpreter,
  set,
  setImports,
 )
import Language.Marlowe.Analysis.FSSemantics (SlotLength (SlotLength), warningsTrace)
import Language.Marlowe.Plutus.Semantics (
  TransactionInput (TransactionInput, txInputs, txInterval),
  TransactionOutput (TransactionOutput, txOutState),
  computeTransaction,
  evalValue,
 )
import Language.Marlowe.Plutus.Semantics.Types (
  Action (Choice, Deposit),
  Bound (Bound),
  Case (Case),
  ChoiceId (ChoiceId),
  Contract (Close, If, Pay, When),
  Environment (Environment),
  Observation (ValueGE),
  Party,
  Payee (Account, Party),
  State (State, accounts, boundValues, choices, minTime),
  Value (AddValue, Constant, DivValue, MulValue, NegValue, SubValue, UseValue, AvailableMoney),
  ValueId (ValueId),
  ada,
  emptyState,
  mkRoleByteString,
  mkTokenNameByteString,
  mkTokenNameUtf8,
  unsafeMkCurrencySymbolHex, Input, Token (Token),
 )
import Language.Marlowe.Plutus.Semantics.Types.Address (
  deserialiseAddress,
  deserialiseAddressBech32,
  mainnet,
  serialiseAddress,
  serialiseAddressBech32,
 )
import Language.Marlowe.Plutus.AssocMap as AM
import Language.Marlowe.Plutus.Foldings (extractNonMerkleizedContractRoles)
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V1.Credential as Credential
import PlutusLedgerApi.V2 (POSIXTime (POSIXTime))
import qualified PlutusTx.Prelude as P
import qualified PlutusTx.Ratio as P
import Marlowe.Testing.Common (alicePk, amount, contractGen, pangramContract, shrinkContract, valueGen)
import Marlowe.Testing.Semantics.Arbitrary ()
import System.Timeout (timeout)
import Test.QuickCheck (
  arbitrary,
  counterexample,
  forAll,
  forAllShrink,
  property,
  suchThat,
  tabulate,
  (.&&.),
  (=/=),
  (===),
 )
import Test.QuickCheck.Instances.ByteString ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@=?))
import Test.Tasty.QuickCheck (Property, testProperty)
import Cardano.Api.Internal.Address (StakeCredential(..))

-- | Set to `True` to print the JSON for the pangram contract.
_PRINT_PANGRAM_JSON_ :: Bool
_PRINT_PANGRAM_JSON_ = False

-- | Run the tests.
tests :: TestTree
tests =
  testGroup
    "Contracts"
    [ testCase "Token Show instance respects HEX and Unicode" tokenShowTest
    , testCase "Pangram Contract serializes into valid JSON" pangramContractSerialization
    , testCase "State serializes into valid JSON" stateSerialization
    , testCase "Input serializes into valid JSON" inputSerialization
    , testCase "Mul analysis" mulAnalysisTest
    , testCase "Div analysis" divAnalysisTest
    , testCase "Div tests" divTest
    , testCase "Transfers between accounts work" transferBetweenAccountsTest
    , testCase "extractContractRoles" extractContractRolesTest
    , testProperty "Value equality is reflexive, symmetric, and transitive" checkEqValue
    , testProperty "Value double negation" doubleNegation
    , testProperty "Values form abelian group" valuesFormAbelianGroup
    , testProperty "Values can be serialized to JSON" valueSerialization
    , testProperty "Multiply by zero" mulTest
    , testProperty "Divide zero and by zero" divZeroTest
    , testProperty "DivValue rounding" divisionRoundingTest
    , testGroup
        "Address serialisation"
        [ testProperty "Compare to Cardano API serialisation to Bech32" addressSerialiseCardanoApi
        , testProperty "Serialise to bytes then deserialise" addressSerialiseDeserialiseBytes
        , testProperty "Serialise to Bech32 then deserialise" addressSerialiseDeserialiseBech32
        ]
    , testGroup
        "Party serialization"
        [ testProperty "Serialise toJSON then deserialise" partySerialiseDeserialiseJSON
        ]
    ]

aliceToken :: PV1.TokenName
aliceToken = mkTokenNameByteString "Alice"

alice :: Party
alice = mkRoleByteString "Alice"

bobToken :: PV1.TokenName
bobToken = mkTokenNameByteString "Bob"

bob :: Party
bob = mkRoleByteString "Bob"

-- | Test `extractNonMerkleizedContractRoles`.
extractContractRolesTest :: IO ()
extractContractRolesTest = do
  extractNonMerkleizedContractRoles Close @=? mempty
  extractNonMerkleizedContractRoles
    (Pay alice (Party bob) ada (Constant 1) Close)
    @=? Set.fromList [aliceToken, bobToken]
  extractNonMerkleizedContractRoles
    (When [Case (Deposit bob alice ada (Constant 10)) Close] 10 Close)
    @=? Set.fromList [aliceToken, bobToken]
  extractNonMerkleizedContractRoles
    (When [Case (Choice (ChoiceId "test" alice) [Bound 0 1]) Close] 10 Close)
    @=? Set.fromList [aliceToken]

-- | Test that equality on `Value` is symmetric and transitive.
checkEqValue :: Property
checkEqValue = property $ do
  let gen = do
        a <- valueGen
        b <- valueGen
        c <- valueGen
        return (a, b, c)
  forAll gen $ \(a, b, c) ->
    (a P.== a) -- reflective
      .&&. ((a P.== b) == (b P.== a)) -- symmetric
      --   (a == b && b == c) => (a == c)
      .&&. (not (a P.== b && b P.== c) || a P.== c) -- transitive

-- | Test that `NegValue` is its own inverse.
doubleNegation :: Property
doubleNegation = property $ do
  let eval = evalValue (Environment (POSIXTime 10, POSIXTime 1_000)) (emptyState (POSIXTime 10))
  forAll valueGen $ \a -> eval (NegValue (NegValue a)) === eval a

-- | Test that `Value` forms an Abelian group.
valuesFormAbelianGroup :: Property
valuesFormAbelianGroup = property $ do
  let gen = do
        a <- valueGen
        b <- valueGen
        c <- valueGen
        return (a, b, c)
  let eval = evalValue (Environment (POSIXTime 10, POSIXTime 1_000)) (emptyState (POSIXTime 10))
  forAll gen $ \(a, b, c) ->
    -- associativity of addition
    eval (AddValue (AddValue a b) c)
      === eval (AddValue a (AddValue b c))
      .&&.
      -- commutativity of addition
      eval (AddValue a b)
        === eval (AddValue b a)
      .&&.
      -- additive identity
      eval (AddValue a (Constant 0))
        === eval a
      .&&.
      -- additive inverse
      eval (AddValue a (NegValue a))
        === 0
      .&&.
      -- subtraction works
      eval (SubValue (AddValue a b) b)
        === eval a

-- | Test rounding of `DivValue`.
divisionRoundingTest :: Property
divisionRoundingTest = property $ do
  let eval = evalValue (Environment (POSIXTime 10, POSIXTime 1_000)) (emptyState (POSIXTime 10))
  -- test half-even rounding
  let gen = do
        n <- amount
        d <- suchThat amount (/= 0)
        return (n, d)
  forAll gen $ \(n, d) -> eval (DivValue (Constant n) (Constant d)) === roundToZero (P.unsafeRatio n d)
  where
    roundToZero = P.truncate

-- | Test `DivValue` with zero in numerator or denominator.
divZeroTest :: Property
divZeroTest = property $ do
  let eval = evalValue (Environment (POSIXTime 10, POSIXTime 1_000)) (emptyState (POSIXTime 10))
  forAll valueGen $ \a ->
    eval (DivValue (Constant 0) a)
      === 0
      .&&. eval (DivValue a (Constant 0))
        === 0

-- | Test `MulValue` with a zero numerator.
mulTest :: Property
mulTest = property $ do
  let eval = evalValue (Environment (POSIXTime 10, POSIXTime 1_000)) (emptyState (POSIXTime 10))
  forAll valueGen $ \a ->
    eval (MulValue (Constant 0) a) === 0

-- | Test the serialization of `Value`.
valueSerialization :: Property
valueSerialization = property $
  forAll valueGen $ \a ->
    let decoded :: Maybe (Value Observation)
        decoded = decode $ encode a
     in Just a === decoded

-- | Test a complicated sequence of multiplications.
mulAnalysisTest :: IO ()
mulAnalysisTest = do
  let slotLength = SlotLength 1_000
      multiply = foldl (\a _ -> MulValue (UseValue $ ValueId "a") a) (Constant 1) ([1 .. 100] :: [Int])
      contract = If (multiply `ValueGE` Constant 10_000) Close (Pay alicePk (Party alicePk) ada (Constant (-100)) Close)
  result <- warningsTrace slotLength contract
  --  print result
  assertBool "Analysis ok" $ isRight result

-- | Test a complicated division.
divAnalysisTest :: IO ()
divAnalysisTest = do
  let slotLength = SlotLength 1_000
      contract n d =
        If
          (DivValue (Constant n) (Constant d) `ValueGE` Constant 5)
          Close
          (Pay alicePk (Party alicePk) ada (Constant (-100)) Close)
  result <- warningsTrace slotLength (contract 11 2)
  assertBool "Analysis ok" $ isRight result && either (const False) isNothing result
  result' <- warningsTrace slotLength (contract 9 2)
  assertBool "Analysis ok" $ isRight result' && either (const False) isJust result'

  let eval = evalValue (Environment (POSIXTime 10, POSIXTime 1_000)) (emptyState (POSIXTime 10))
  eval (DivValue (Constant 0) (Constant 2)) @=? 0
  eval (DivValue (Constant 1) (Constant 0)) @=? 0
  eval (DivValue (Constant 5) (Constant 2)) @=? 2
  eval (DivValue (Constant (-5)) (Constant 2)) @=? -2
  eval (DivValue (Constant 7) (Constant 2)) @=? 3
  eval (DivValue (Constant (-7)) (Constant 2)) @=? -3

-- | Golden tests for `DivValue`.
divTest :: IO ()
divTest = do
  let eval = evalValue (Environment (POSIXTime 10, POSIXTime 1_000)) (emptyState (POSIXTime 10))
  eval (DivValue (Constant 0) (Constant 2)) @=? 0
  eval (DivValue (Constant 1) (Constant 0)) @=? 0
  eval (DivValue (Constant 5) (Constant 2)) @=? 2
  eval (DivValue (Constant (-5)) (Constant 2)) @=? -2
  eval (DivValue (Constant 7) (Constant 2)) @=? 3
  eval (DivValue (Constant (-7)) (Constant 2)) @=? -3

-- | Test transfer of funds between internal accounts.
transferBetweenAccountsTest :: IO ()
transferBetweenAccountsTest = do
  let state =
        State
          { accounts = AM.unsafeFromList [((alice, ada), 100)]
          , choices = AM.empty
          , boundValues = AM.empty
          , minTime = 10
          }
  let contract = Pay alice (Account bob) ada (Constant 100) (When [] 100 Close)
  let txInput =
        TransactionInput
          { txInterval = (20, 30)
          , txInputs = []
          }
  case computeTransaction txInput state contract of
    TransactionOutput{txOutState = State{accounts}} -> do
      assertBool "Accounts check" $ accounts == AM.unsafeFromList [((bob, ada), 100)]
    e -> fail $ show e

-- | Serialize the Pangram contract.
pangramContractSerialization :: IO ()
pangramContractSerialization = do
  when _PRINT_PANGRAM_JSON_ $ do
    let json = toStrict (encodeToLazyText pangramContract)
    T.putStrLn json
    Just pangramContract @=? (decode $ encode pangramContract)
    T.putStrLn . T.pack . show $ pangramContract
  contractJson <- Paths.getDataFileName "test/contract.json"
  contract <- readFile contractJson
  let decoded :: Maybe Contract
      decoded = decode (fromString contract)
  case decoded of
    Just cont -> cont @=? pangramContract
    _ -> assertFailure "Nope"

-- | Test printing of tokens.
tokenShowTest :: IO ()
tokenShowTest = do
  -- SCP-834, CurrencySymbol is HEX encoded ByteString,
  -- and TokenSymbol as UTF8 encoded Unicode string
  let token =
        Token
          (unsafeMkCurrencySymbolHex "00010afF")
          (mkTokenNameUtf8 "ÚSD©")

      actual :: Value Observation
      actual = AvailableMoney (mkRoleByteString "alice") token

  show actual @=? "AvailableMoney \"alice\" (Token \"00010aff\" \"ÚSD©\")"

-- | Test JSON serialization of input.
inputSerialization :: IO ()
inputSerialization = do
  inputJson <- Paths.getDataFileName "test/input.json"
  state <- readFile inputJson
  let decoded :: Either String [Input]
      decoded = eitherDecode (fromString state)
  case decoded of
    Right input ->
      case eitherDecode $ encode input of
        Right input' -> assertBool "Should be equal" (input == input')
        Left e -> assertFailure $ "Could not decode encoded input: " <> e
    Left e -> assertFailure $ "Could not decode test/input.json: " <> e

-- | Test JSON serialization of state.
stateSerialization :: IO ()
stateSerialization = do
  stateJson <- Paths.getDataFileName "test/state.json"
  state <- readFile stateJson
  let decoded :: Maybe State
      decoded = decode (fromString state)
  case decoded of
    Just st ->
      case decode $ encode st of
        Just st' -> assertBool "Should be equal" (st == st')
        Nothing -> assertFailure "Nope"
    Nothing -> assertFailure "Nope"

-- | Test that showing contracts works, with shrinkage of test cases.
prop_showWorksForContracts :: Property
prop_showWorksForContracts = forAllShrink contractGen shrinkContract showWorksForContract

-- | Test that showing a contract works.
showWorksForContract :: Contract -> Property
showWorksForContract contract = unsafePerformIO $ do
  res <-
    runInterpreter $
      setImports ["Language.Marlowe"]
        >> set [languageExtensions := [OverloadedStrings]]
        >> interpretContractString (show contract)
  return
    ( case res of
        Right x -> x === contract
        Left err -> counterexample (show err) False
    )

-- | Test reading of contracts.
interpretContractString :: (MonadInterpreter m) => String -> m Contract
interpretContractString contractStr = interpret contractStr (as :: Contract)

-- | Test that a contract execution does not exhibit false positives for warnings.
noFalsePositivesForContract :: Maybe Int -> Contract -> Property
noFalsePositivesForContract timeLimit cont =
  unsafePerformIO
    ( do
        let slotLength = SlotLength 1_000
        res <-
          catch
            (limitTime $ first Right <$> warningsTrace slotLength cont)
            (\exc -> return . Just . Left $ Left (exc :: SomeException))
        return
          ( case res of
              Nothing ->
                tabulate
                  ( "Timed out after "
                      <> show timeLimit
                      <> " seconds"
                  )
                  ["True"]
                  True
              Just (Left err) -> counterexample (show err) False
              Just (Right answer) ->
                tabulate
                  "Has counterexample"
                  [show (isJust answer)]
                  ( case answer of
                      Nothing ->
                        tabulate
                          "Is empty contract"
                          [show (cont == Close)]
                          True
                      Just (is, li, warns) ->
                        counterexample ("Trace: " ++ show (is, li)) $
                          tabulate
                            "Number of warnings"
                            [show (length warns)]
                            (warns =/= [])
                  )
          )
    )
  where
    limitTime = maybe (Just <$>) (timeout . (1_000_000 *)) timeLimit

-- | Test that contract execution does not exhibit false positives for warnings.
prop_noFalsePositives :: Maybe Int -> Property
prop_noFalsePositives = forAllShrink contractGen shrinkContract . noFalsePositivesForContract

-- | Compare address serialisation to Cardano API.
addressSerialiseCardanoApi :: Property
addressSerialiseCardanoApi =
  property
    . forAll arbitrary
    $ \(network, address) ->
      let encoded = serialiseAddressBech32 network address
          encoded' =
            C.serialiseAddress
              <$> toCardanoAddressInEra (if network == mainnet then C.Mainnet else C.Testnet (C.NetworkMagic 2)) address
       in Just encoded === encoded'

toCardanoAddressInEra :: C.NetworkId -> PV1.Address -> Maybe (C.AddressInEra C.BabbageEra)
toCardanoAddressInEra networkId (PV1.Address addressCredential addressStakingCredential) =
  C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage)
    <$> ( C.makeShelleyAddress networkId
            <$> toCardanoPaymentCredential addressCredential
            <*> toCardanoStakeAddressReference addressStakingCredential
        )

toCardanoPaymentCredential :: Credential.Credential -> Maybe C.PaymentCredential
toCardanoPaymentCredential (Credential.PubKeyCredential pubKeyHash) = C.PaymentCredentialByKey <$> toCardanoPaymentKeyHash pubKeyHash
toCardanoPaymentCredential (Credential.ScriptCredential validatorHash) = C.PaymentCredentialByScript <$> toCardanoScriptHash validatorHash

toCardanoScriptHash :: PV1.ScriptHash -> Maybe C.ScriptHash
toCardanoScriptHash (PV1.ScriptHash bs) = either (const Nothing) Just $ deserialiseFromRawBytes C.AsScriptHash $ PV1.fromBuiltin bs

toCardanoPaymentKeyHash :: PV1.PubKeyHash -> Maybe (C.Hash C.PaymentKey)
toCardanoPaymentKeyHash (PV1.PubKeyHash bs) =
  let bsx = PV1.fromBuiltin bs
   in either (const Nothing) Just $ deserialiseFromRawBytes (C.AsHash C.AsPaymentKey) bsx

toCardanoStakeAddressReference :: Maybe Credential.StakingCredential -> Maybe C.StakeAddressReference
toCardanoStakeAddressReference Nothing = pure C.NoStakeAddress
toCardanoStakeAddressReference (Just (Credential.StakingHash credential)) =
  C.StakeAddressByValue <$> toCardanoStakeCredential credential
toCardanoStakeAddressReference (Just Credential.StakingPtr{}) = Nothing

toCardanoStakeCredential :: Credential.Credential -> Maybe C.StakeCredential
toCardanoStakeCredential (Credential.PubKeyCredential pubKeyHash) = StakeCredentialByKey <$> toCardanoStakeKeyHash pubKeyHash
toCardanoStakeCredential (Credential.ScriptCredential validatorHash) = StakeCredentialByScript <$> toCardanoScriptHash validatorHash

toCardanoStakeKeyHash :: PV1.PubKeyHash -> Maybe (C.Hash C.StakeKey)
toCardanoStakeKeyHash (PV1.PubKeyHash bs) = either (const Nothing) Just $ deserialiseFromRawBytes (C.AsHash C.AsStakeKey) (PV1.fromBuiltin bs)

-- | Serialise an address to bytes and then deserialize.
addressSerialiseDeserialiseBytes :: Property
addressSerialiseDeserialiseBytes =
  property
    . forAll arbitrary
    $ \(network, address) ->
      let encoded = serialiseAddress network address
          decoded = deserialiseAddress encoded
       in Just (network, address) === decoded

-- | Serialise an address to Bech32 and then deserialize.
addressSerialiseDeserialiseBech32 :: Property
addressSerialiseDeserialiseBech32 =
  property
    . forAll arbitrary
    $ \(network, address) ->
      let encoded = serialiseAddressBech32 network address
          decoded = deserialiseAddressBech32 encoded
       in Just (network, address) === decoded

-- | Serialise a party to JSON and then deserialise.
partySerialiseDeserialiseJSON :: Property
partySerialiseDeserialiseJSON =
  property
    . forAll arbitrary
    $ \party ->
      let encoded = encode (party :: Party)
          decoded = decode encoded
       in Just party === decoded
