{- FOURMOLU_DISABLE -}

{-# LANGUAGE CPP #-}

-- Plinth options
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

-- Recommended extensions and flags
-- https://plutus.cardano.intersectmbo.org/docs/using-plinth/extensions-flags-pragmas
-- https://plutus.cardano.intersectmbo.org/docs/auction-smart-contract/on-chain-code#4-script-context
{-# LANGUAGE Strict #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-full-laziness #-}
{-# OPTIONS -fno-ignore-interface-pragmas #-}
{-# OPTIONS -fno-omit-interface-pragmas #-}
{-# OPTIONS -fno-spec-constr #-}
{-# OPTIONS -fno-specialise #-}
{-# OPTIONS -fno-strictness #-}
{-# OPTIONS -fno-unbox-small-strict-fields #-}
{-# OPTIONS -fno-unbox-strict-fields #-}

-- Custom warning suppression
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Marlowe semantics validator.
module Marlowe.Plutus.Scripts (
  -- * Validation
  mkRolePayoutValidator,
  mkMarloweValidator,
  MarloweInput,
  MarloweTxInput (..),
) where

import Marlowe.Plutus.Contrib.PlutusTx.Debugging.Trace (
  traceError,
  traceIfFalse,
  )

-- import Marlowe.Plutus.Contrib.PlutusTx.Debugging.Trace (
--     showDebug, traceVerbose
--   )
-- import PlutusTx.Builtins (appendString)
-- Example:
-- traceVerbose (appendString "COMPUTED:" (showDebug computedTxOutContract)) "o" $ checkOwnOutputConstraint marloweData finalBalance
import Marlowe.Plutus.Scripts.Types (MarloweInput, MarloweTxInput (..))
import Marlowe.Plutus.Semantics as Semantics (
  MarloweData (..),
  MarloweParams (MarloweParams, rolesCurrency),
  Payment (..),
  TransactionError (
    TEAmbiguousTimeIntervalError,
    TEApplyNoMatchError,
    TEHashMismatch,
    TEIntervalError,
    TEUselessTransaction
  ),
  TransactionInput (TransactionInput, txInputs, txInterval),
  TransactionOutput (
    Error,
    TransactionOutput,
    txOutContract,
    txOutPayments,
    txOutState
  ),
  computeTransaction,
  totalBalance,
  foldMapList,
 )
import Marlowe.Plutus.Semantics.Types as Semantics (
  ChoiceId (ChoiceId),
  Contract (Close),
  Input (..),
  InputContent (..),
  IntervalError (IntervalInPastError, InvalidInterval),
  Party (..),
  Payee (Account, Party),
  State (..),
  Token (Token),
  getInputContent,
 )
import PlutusLedgerApi.V3 (
  Credential (..),
  Datum (Datum),
  DatumHash (DatumHash),
  Extended (..),
  Interval (..),
  LowerBound (..),
  POSIXTime (..),
  POSIXTimeRange,
  ScriptContext (ScriptContext, scriptContextScriptInfo, scriptContextRedeemer, scriptContextTxInfo),
  ScriptHash (..),
  ScriptInfo (SpendingScript),
  TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
  TxInfo (TxInfo, txInfoInputs, txInfoOutputs, txInfoValidRange),
  UnsafeFromData (..),
  UpperBound (..),
  ToData (toBuiltinData)
 )
import PlutusLedgerApi.V3.Contexts (findDatum, findDatumHash, txSignedBy, valueSpent)
import PlutusLedgerApi.V2.Tx (OutputDatum (OutputDatumHash), TxOut (TxOut, txOutAddress, txOutDatum, txOutValue))

import PlutusLedgerApi.V1.Address as Address (scriptHashAddress)
import PlutusTx.Prelude as PlutusTxPrelude (
  AdditiveGroup ((-)),
  AdditiveMonoid (zero),
  AdditiveSemigroup ((+)),
  Bool (..),
  Enum (fromEnum),
  Eq (..),
  Functor (fmap),
  Maybe (..),
  Ord ((>)),
  otherwise,
  snd,
  ($),
  (&&),
  (.),
  (>=),
  (/=),
  (||),
 )

import qualified PlutusTx.List as List

import qualified PlutusLedgerApi.V1.Value as Val
import qualified PlutusLedgerApi.V2 as Ledger (Address (Address))
import qualified Marlowe.Plutus.AssocMap as AssocMap
import Prelude qualified as H
import PlutusLedgerApi.V2 (Redeemer(Redeemer))

{-# INLINEABLE closeInterval #-}
-- | Convert a Plutus POSIX time range into the closed interval needed by Marlowe semantics.
closeInterval :: POSIXTimeRange -> Maybe (POSIXTime, POSIXTime)
closeInterval (Interval (LowerBound (Finite (POSIXTime l)) lc) (UpperBound (Finite (POSIXTime h)) hc)) =
  Just
    ( POSIXTime $ l + 1 - fromEnum lc -- Add one millisecond if the interval was open.
    , POSIXTime $ h - 1 + fromEnum hc -- Subtract one millisecond if the interval was open.
    )
closeInterval _ = Nothing

{-# INLINEABLE mkRolePayoutValidator #-}
-- | The Marlowe payout validator.
mkRolePayoutValidator
  :: ScriptContext
  -- ^ The script context.
  -> Bool
  -- ^ Whether the transaction validated.
mkRolePayoutValidator ScriptContext{scriptContextTxInfo, scriptContextScriptInfo} =
  let
    (currency, role) = case scriptContextScriptInfo of
      SpendingScript _ (H.Just (Datum d)) -> unsafeFromBuiltinData d
      _ -> traceError "q"
  in
    -- The role token for the correct currency must be present.
    -- [Marlowe-Cardano Specification: "17. Payment authorized".]
    Val.singleton currency role 1 `Val.leq` valueSpent scriptContextTxInfo

{-# INLINEABLE mkMarloweValidator #-}
-- | The Marlowe semantics validator.
mkMarloweValidator
  :: ScriptHash
  -- ^ The hash of the corresponding Marlowe payout validator.
  -> ScriptContext
  -- ^ The script context.
  -> Bool
  -- ^ Whether the transaction validated.
mkMarloweValidator
  rolePayoutValidatorHash
  ctx@ScriptContext{scriptContextTxInfo, scriptContextRedeemer=Redeemer (unsafeFromBuiltinData -> marloweTxInputs), scriptContextScriptInfo} = do
    let scriptInValue = txOutValue $ txInInfoResolved ownInput
    let interval =
          -- Marlowe semantics require a closed interval, so we might adjust by one millisecond.
          case closeInterval $ txInfoValidRange scriptContextTxInfo of
            Just interval' -> interval'
            Nothing -> traceError "a"

    -- Find Contract continuation in TxInfo datums by hash or fail with error.
    let inputs = fmap marloweTxInputToInput marloweTxInputs

    {-  We do not check that a transaction contains exact input payments.
        We only require an evidence from a party, e.g. a signature for PubKey party,
        or a spend of a 'party role' token.  This gives huge flexibility by allowing
        parties to provide multiple inputs (either other contracts or P2PKH).
        Then, we check scriptOutput to be correct.
     -}
    let inputContents = fmap getInputContent inputs

    -- Check that the required signatures and role tokens are present.
    -- [Marlowe-Cardano Specification: "Constraint 14. Inputs authorized".]
    let inputsOk = allInputsAreAuthorized inputContents

    -- We decided to drop this redundancy:
    -- [Marlowe-Cardano Specification: "Constraint 5. Input value from script".]
    -- [Marlowe-Cardano Specification: "Constraint 13. Positive balances".]
    -- [Marlowe-Cardano Specification: "Constraint 19. No duplicates".]
    -- [Marlowe-Cardano Specification: "Constraint 0. Input to semantics".]
    -- Package the inputs to be applied in the semantics.
    let txInput =
          TransactionInput
            { txInterval = interval
            , txInputs = inputs
            }

    -- [Marlowe-Cardano Specification: "Constraint 7. Input state".]
    -- [Marlowe-Cardano Specification: "Constraint 8. Input contract".]
    -- The semantics computation operates on the state and contract from
    -- the incoming datum.
    let computedResult = computeTransaction txInput marloweState marloweContract
    case computedResult of
      TransactionOutput
        { txOutPayments = computedTxOutPayments
        , txOutState = computedTxOutState
        , txOutContract = computedTxOutContract
        } -> do
        -- [Marlowe-Cardano Specification: "Constraint 9. Marlowe parameters".]
        -- [Marlowe-Cardano Specification: "Constraint 10. Output state".]
        -- [Marlowe-Cardano Specification: "Constraint 11. Output contract."]
        -- The output datum maintains the parameters and uses the state
        -- and contract resulting from the semantics computation.
        let marloweData =
              MarloweData
                { marloweParams = marloweParams
                , marloweContract = computedTxOutContract
                , marloweState = computedTxOutState
                }

            -- Each party must receive as least as much value as the semantics specify.
            -- [Marlowe-Cardano Specification: "Constraint 15. Sufficient payment."]
            payoutsByParty = AssocMap.toList $ foldMapList payoutByParty computedTxOutPayments
            payoutsOk = payoutConstraints payoutsByParty

            checkContinuation =
              case computedTxOutContract of
                -- [Marlowe-Cardano Specification: "Constraint 4. No output to script on close".]
                Close -> do
                  let
                    -- Check for any output to the script address.
                    hasNoOutputToOwnScript :: Bool
                    hasNoOutputToOwnScript = List.all ((/= ownAddress) . txOutAddress) allOutputs
                  traceIfFalse "c" hasNoOutputToOwnScript
                _ -> do
                  let totalIncome = foldMapList collectDeposits inputContents
                      totalPayouts = foldMapList snd payoutsByParty
                      finalBalance = scriptInValue + totalIncome - totalPayouts
                  -- [Marlowe-Cardano Specification: "Constraint 3. Single Marlowe output".]
                  -- [Marlowe-Cardano Specification: "Constraint 6. Output value to script."]
                  -- Check that the single Marlowe output has the correct datum and value.
                  checkOwnOutputConstraint marloweData finalBalance
                    -- [Marlowe-Cardano Specification: "Constraint 18. Final balance."]
                    -- [Marlowe-Cardano Specification: "Constraint 13. Positive balances".]
                    -- [Marlowe-Cardano Specification: "Constraint 19. No duplicates".]
                    -- Check that the final state obeys the Semantic's invariants.
                    && checkState finalBalance computedTxOutState
        inputsOk
          && payoutsOk
          && checkContinuation
          -- [Marlowe-Cardano Specification: "20. Single satisfaction".]
          -- Either there must be no payouts, or there must be no other validators.
          && traceIfFalse "z" (List.null payoutsByParty || noOthers)
      Error TEAmbiguousTimeIntervalError -> traceError "i"
      Error TEApplyNoMatchError -> traceError "n"
      Error (TEIntervalError (InvalidInterval _)) -> traceError "j"
      Error (TEIntervalError (IntervalInPastError _ _)) -> traceError "k"
      Error TEUselessTransaction -> traceError "u"
      Error TEHashMismatch -> traceError "m"
    where
      MarloweData {..} = case scriptContextScriptInfo of
        SpendingScript _ (H.Just (Datum d)) -> unsafeFromBuiltinData d
        _ -> traceError "f"

      -- The roles currency is in the Marlowe parameters.
      MarloweParams{rolesCurrency} = marloweParams

      -- Find the input being spent by a script.
      findOwnInput :: ScriptContext -> Maybe TxInInfo
      findOwnInput ScriptContext{scriptContextTxInfo = TxInfo{txInfoInputs}, scriptContextScriptInfo = SpendingScript txOutRef _datum} =
        List.find (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == txOutRef) txInfoInputs
      findOwnInput _ = Nothing

      -- [Marlowe-Cardano Specification: "2. Single Marlowe script input".]
      -- The inputs being spent by this script, and whether other validators are present.
      ownInput :: TxInInfo
      noOthers :: Bool
      (ownInput@TxInInfo{txInInfoResolved = TxOut{txOutAddress = ownAddress}}, noOthers) =
        case findOwnInput ctx of
          Just ownTxInInfo -> examineScripts (sameValidatorHash ownTxInInfo) Nothing True (txInfoInputs scriptContextTxInfo)
          _ -> traceError "x" -- Input to be validated was not found.

      -- Check for the presence of multiple Marlowe validators or other Plutus validators.
      examineScripts
        :: (ScriptHash -> Bool) -- Test for this validator.
        -> Maybe TxInInfo -- The input for this validator, if found so far.
        -> Bool -- Whether no other validator has been found so far.
        -> [TxInInfo] -- The inputs remaining to be examined.
        -> (TxInInfo, Bool) -- The input for this validator and whether no other validators are present.
        -- This validator has not been found.
      examineScripts _ Nothing _ [] = traceError "x"
      -- This validator has been found, and other validators may have been found.
      examineScripts _ (Just self) noOthers [] = (self, noOthers)
      -- Found both this validator and another script, so we short-cut.
      examineScripts _ (Just self) False _ = (self, False)
      -- Found one script.
      examineScripts f mSelf noOthers (tx@TxInInfo{txInInfoResolved = TxOut{txOutAddress = Ledger.Address (ScriptCredential vh) _}} : txs)
        -- The script is this validator.
        | f vh = case mSelf of
            -- We hadn't found it before, so we save it in `mSelf`.
            Nothing -> examineScripts f (Just tx) noOthers txs
            -- We already had found this validator before
            Just _ -> traceError "w"
        -- The script is something else, so we set `noOther` to `False`.
        | otherwise = examineScripts f mSelf False txs
      -- An input without a validator is encountered.
      examineScripts f self others (_ : txs) = examineScripts f self others txs

      -- Check if inputs are being spent from the same script.
      sameValidatorHash :: TxInInfo -> ScriptHash -> Bool
      sameValidatorHash TxInInfo{txInInfoResolved = TxOut{txOutAddress = Ledger.Address (ScriptCredential vh1) _}} vh2 = vh1 == vh2
      sameValidatorHash _ _ = False

      -- Check a state for the correct value, positive accounts, and no duplicates.
      checkState :: Val.Value -> State -> Bool
      checkState expected State{..} =
         -- [Marlowe-Cardano Specification: "Constraint 5. Input value from script".]
         -- and/or
         -- [Marlowe-Cardano Specification: "Constraint 18. Final balance."]
         traceIfFalse "v" (totalBalance accounts == expected)
         -- These checks were redundant:
         -- [Marlowe-Cardano Specification: "Constraint 13. Positive balances".]
         -- [Marlowe-Cardano Specification: "Constraint 19. No duplicates".]

      -- Look up the Datum hash for specific data.
      findDatumHash' :: (ToData o) => o -> Maybe DatumHash
      findDatumHash' datum = findDatumHash (Datum $ toBuiltinData datum) scriptContextTxInfo

      -- Check that the correct datum and value is being output to the script.
      checkOwnOutputConstraint :: MarloweData -> Val.Value -> Bool
      checkOwnOutputConstraint ocDatum ocValue = do
        let 
          hsh = findDatumHash' ocDatum
          continuingOutput = case List.filter (\TxOut{txOutAddress} -> ownAddress == txOutAddress) allOutputs of
            [out] -> out
            _ -> traceError "o" -- No continuation or multiple Marlowe contract outputs is forbidden.
        traceIfFalse "d" $ checkScriptOutput (==) ownAddress hsh ocValue continuingOutput -- "Output constraint"


      -- Check that address, value, and datum match the specified.
      checkScriptOutput :: (Val.Value -> Val.Value -> Bool) -> Ledger.Address -> Maybe DatumHash -> Val.Value -> TxOut -> Bool
      checkScriptOutput comparison addr hsh value TxOut{txOutAddress, txOutValue, txOutDatum = OutputDatumHash svh} =
        txOutValue `comparison` value && hsh == Just svh && txOutAddress == addr
      checkScriptOutput _ _ _ _ _ = False

      -- All of the script outputs.
      allOutputs :: [TxOut]
      allOutputs = txInfoOutputs scriptContextTxInfo

      -- Check merkleization and transform transaction input to semantics input.
      marloweTxInputToInput :: MarloweTxInput -> Input
      marloweTxInputToInput (MerkleizedTxInput input hash) =
        case findDatum (DatumHash hash) scriptContextTxInfo of
          Just (Datum d) ->
            let continuation = unsafeFromBuiltinData d
             in MerkleizedInput input hash continuation
          Nothing -> traceError "h"
      marloweTxInputToInput (Input input) = NormalInput input

      -- Check that inputs are authorized.
      allInputsAreAuthorized :: [InputContent] -> Bool
      allInputsAreAuthorized = List.all validateInputWitness
        where
          validateInputWitness :: InputContent -> Bool
          validateInputWitness input =
            case input of
              IDeposit _ party _ _ -> validatePartyWitness party -- The party must witness a deposit.
              IChoice (ChoiceId _ party) _ -> validatePartyWitness party -- The party must witness a choice.
              INotify -> True -- No witness is needed for a notify.
            where
              validatePartyWitness :: Party -> Bool
              validatePartyWitness (Address _ address) = traceIfFalse "s" $ txSignedByAddress address -- The key must have signed.
              validatePartyWitness (Role role) =
                traceIfFalse "t"
                  $ Val.singleton rolesCurrency role 1 -- The role token must be present.
                  `Val.leq` valueSpent scriptContextTxInfo

      -- Tally the deposits in the input.
      collectDeposits :: InputContent -> Val.Value
      collectDeposits (IDeposit _ _ (Token cur tok) amount)
        | amount > 0 = Val.singleton cur tok amount -- SCP-5123: Semantically negative deposits
        | otherwise = zero -- do not remove funds from the script's UTxO.
      collectDeposits _ = zero

      -- Extract the payout to a party.
      payoutByParty :: Payment -> AssocMap.Map Party Val.Value
      payoutByParty (Payment _ (Party party) (Token cur tok) amount)
        | amount > 0 = AssocMap.singleton party $ Val.singleton cur tok amount
        | otherwise = AssocMap.empty -- NOTE: Perhaps required because semantics may make zero payments
        -- (though this passes the test suite), but removing this function's
        -- guard reduces the validator size by 20 bytes.
      payoutByParty (Payment _ (Account _) _ _) = AssocMap.empty

      -- -- Check outgoing payments.
      payoutConstraints :: [(Party, Val.Value)] -> Bool
      payoutConstraints = List.all payoutToTxOut
        where
          payoutToTxOut :: (Party, Val.Value) -> Bool
          payoutToTxOut (party, value) = case party of
            -- [Marlowe-Cardano Specification: "Constraint 15. Sufficient Payment".]
            -- SCP-5128: Note that the payment to an address may be split into several outputs but the payment to a role must be
            -- a single output. The flexibility of multiple outputs accommodates wallet-related practicalities such as the change and
            -- the return of the role token being in separate UTxOs in situations where a contract is also paying to the address
            -- where that change and that role token are sent.
            Address _ address -> traceIfFalse "p" $ valuePaidToAddress address `geqSingleton` value -- At least sufficient value paid.
            Role role ->
              let hsh = findDatumHash' (rolesCurrency, role)
                  addr = Address.scriptHashAddress rolePayoutValidatorHash
               in -- Some output must have the correct value and datum to the role-payout address.
                  traceIfFalse "r" $ List.any (checkScriptOutput geqSingleton addr hsh value) allOutputs

      -- Check that a multiasset value is not less than another value, optimized for singletons.
      -- Note that the semantics of this function differ from `geq` in cases where the first
      -- argument contains negative quantities; however, this never occurs in values from UTxOs.
      geqSingleton :: Val.Value -> Val.Value -> Bool
      geqSingleton multi single =
        case Val.flattenValue single of
          [(c, t, a)] -> Val.valueOf multi c t >= a  -- The second value was a singleton.
          _ -> multi `Val.geq` single                -- The second value wasn't a singleton.

      -- The key for the address must have signed.
      txSignedByAddress :: Ledger.Address -> Bool
      txSignedByAddress (Ledger.Address (PubKeyCredential pkh) _) = scriptContextTxInfo `txSignedBy` pkh
      txSignedByAddress _ = False

      -- Tally the value paid to an address.
      valuePaidToAddress :: Ledger.Address -> Val.Value
      valuePaidToAddress address = foldMapList txOutValue $ List.filter ((== address) . txOutAddress) allOutputs

