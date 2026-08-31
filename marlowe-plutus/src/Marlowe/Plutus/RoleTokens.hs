{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

-- Recommended extensions and flags
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

module Marlowe.Plutus.RoleTokens (
  -- * Minting
  MintAction (..),
  RoleTokens,
  mkRoleTokens,
  mkRoleTokensHash,
  mkPolicy,
  wrapMintingPolicy,
) where

import Marlowe.Plutus.RoleTokens.Types (
  MintAction (..),
  RoleTokens,
  RoleTokensHash,
  mkRoleTokens,
  mkRoleTokensHash,
 )
import PlutusTx.Prelude (
  Bool,
  BuiltinData,
  BuiltinUnit,
  Eq ((==)),
  check,
  traceIfFalse,
  ($),
  (&&),
 )
import qualified PlutusLedgerApi.V3 as PV3
import qualified PlutusLedgerApi.V1.Value as Val
import qualified PlutusTx.List as List

-- | One shot policy which encodes the `txOutRef` and tokens minting in its hash.
-- In theory this currency can be reused across multiple Marlowe Contracts when the precise
-- token distribution is not so important to the participant but rather a possible token
-- redundancy or uniqueness.
{-# INLINEABLE mkPolicy #-}
mkPolicy
  :: RoleTokensHash
  -> PV3.TxOutRef
  -> MintAction
  -> PV3.ScriptContext
  -> Bool
mkPolicy roleTokensHash seedInput action context =
  case action of
    Mint -> validateMinting roleTokensHash seedInput context
    Burn -> validateBurning context

{-# INLINEABLE ownCurrencySymbol #-}
ownCurrencySymbol :: PV3.ScriptContext -> PV3.CurrencySymbol
ownCurrencySymbol ctx =
  case PV3.scriptContextScriptInfo ctx of
    PV3.MintingScript cs -> cs
    _ -> PV3.CurrencySymbol ""

-- | Convert MintValue to Value
-- Since MintValue and Value have the same underlying representation (Map CurrencySymbol (Map TokenName Integer)),
-- we can convert by going through BuiltinData
{-# INLINEABLE mintValueToValue #-}
mintValueToValue :: PV3.MintValue -> Val.Value
mintValueToValue mv = Val.Value $ PV3.unsafeFromBuiltinData (PV3.toBuiltinData mv)

{-# INLINEABLE validateMinting #-}
validateMinting :: RoleTokensHash -> PV3.TxOutRef -> PV3.ScriptContext -> Bool
validateMinting roleTokens seedInput context =
  traceIfFalse "Mint failed"
    $ seedInputIsConsumed
    && roleTokensAreMinted
  where
    PV3.ScriptContext{scriptContextTxInfo = txInfo} = context
    ownCurrency = ownCurrencySymbol context

    seedInputIsConsumed :: Bool
    seedInputIsConsumed = List.any (\txInInfo -> PV3.txInInfoOutRef txInInfo == seedInput) (PV3.txInfoInputs txInfo)

    roleTokensAreMinted :: Bool
    roleTokensAreMinted = txMintedRoleTokens == roleTokens
      where
        mintedTokensValue = mintValueToValue (PV3.txInfoMint txInfo)
        ownMintedTokens = List.filter (\(cs, _, _) -> cs == ownCurrency) (Val.flattenValue mintedTokensValue)
        tokenList = List.map (\(_, tn, a) -> (tn, a)) ownMintedTokens
        txMintedRoleTokens = mkRoleTokensHash (mkRoleTokens tokenList)

{-# INLINEABLE validateBurning #-}
validateBurning :: PV3.ScriptContext -> Bool
validateBurning context = do
  traceIfFalse "Burn failed" allBurned
  where
    ownCurrency = ownCurrencySymbol context
    allBurned = missingFromOutputsValue ownCurrency context

{-# INLINEABLE missingFromOutputsValue #-}
missingFromOutputsValue :: PV3.CurrencySymbol -> PV3.ScriptContext -> Bool
missingFromOutputsValue currencySymbol PV3.ScriptContext{scriptContextTxInfo} = do
  let PV3.TxInfo{txInfoOutputs} = scriptContextTxInfo
      isCurrencyAbsent PV3.TxOut{txOutValue} =
        Val.valueOf txOutValue currencySymbol (PV3.TokenName "") == 0
  List.all isCurrencyAbsent txInfoOutputs

type MintingPolicyFn = BuiltinData -> BuiltinData -> BuiltinUnit

{-# INLINEABLE wrapMintingPolicy #-}
wrapMintingPolicy
  :: (PV3.UnsafeFromData redeemer, PV3.UnsafeFromData context)
  => (redeemer -> context -> Bool)
  -> MintingPolicyFn
wrapMintingPolicy f r c =
  PlutusTx.Prelude.check (f redeemer context)
  where
    redeemer = PV3.unsafeFromBuiltinData r
    context = PV3.unsafeFromBuiltinData c
