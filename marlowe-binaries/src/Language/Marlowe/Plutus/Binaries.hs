{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- Let's ignore for now non used warnings in this module.
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-preserve-logging #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fobject-code #-}

module Language.Marlowe.Plutus.Binaries where

import PlutusTx (CompiledCode, unsafeApplyCode, liftCodeDef)

import PlutusTx.Prelude as PlutusTxPrelude (
  BuiltinData,
  check,
  toBuiltin,
  Semigroup ((<>)),
  ($),
  (.),
  BuiltinUnit,
 )

import qualified Cardano.Crypto.Hash as Hash
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import qualified PlutusTx
import qualified Prelude as Haskell
import Language.Marlowe.Plutus.Scripts (mkRolePayoutValidator, mkMarloweValidator)
import Language.Marlowe.Plutus.OpenRoles (mkOpenRoleValidator)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode, unsafeFromBuiltinData)
import PlutusLedgerApi.Data.V1 (ScriptHash(ScriptHash))
import PlutusCore (DefaultUni)
import PlutusTx.Lift.Class (Lift)
import PlutusTx.Blueprint.PlutusVersion (PlutusVersion(..))

{-# INLINEABLE rolePayoutValidator #-}
-- | The Marlowe payout validator.
rolePayoutValidator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
rolePayoutValidator =
  $$(PlutusTx.compile [||rolePayoutValidator'||])
  where
    rolePayoutValidator' :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit
    rolePayoutValidator' d r p =
      check
        $ mkRolePayoutValidator
          (unsafeFromBuiltinData d)
          (unsafeFromBuiltinData r)
          (unsafeFromBuiltinData p)


-- | Compute the hash of a script.
hashScript :: PlutusVersion -> CompiledCode fn -> ScriptHash
hashScript pv = do
  let
    prefix = case pv of
      PlutusV1 -> "\x01"
      PlutusV2 -> "\x02"
      PlutusV3 -> "\x03"
  ScriptHash
    . toBuiltin
    . (Hash.hashToBytes :: Hash.Hash Hash.Blake2b_224 SBS.ShortByteString -> BS.ByteString)
    . Hash.hashWith (BS.append prefix . SBS.fromShort) -- For Plutus V3.
    . serialiseCompiledCode

-- | The hash of the Marlowe payout validator.
rolePayoutValidatorHash :: ScriptHash
rolePayoutValidatorHash = hashScript PlutusV3 rolePayoutValidator

-- | The serialisation of the Marlowe payout validator.
rolePayoutValidatorBytes :: SerialisedScript
rolePayoutValidatorBytes = serialiseCompiledCode rolePayoutValidator

applyArg
  :: (Lift DefaultUni params)
  => CompiledCode (params -> res)
  -> params
  -> CompiledCode res
applyArg code = unsafeApplyCode code . liftCodeDef

-- applyParams
--   :: (Lift DefaultUni params)
--   => CompiledCode (params -> ScriptSignature)
--   -> params
--   -> CompiledCode ScriptSignature
-- applyParams code = unsafeApplyCode code . liftCodeDef

{-# INLINEABLE marloweValidator #-}
-- | The validator for Marlowe semantics.
marloweValidator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
marloweValidator = do
  let marloweValidator' :: ScriptHash -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit
      marloweValidator' rpvh d r p =
        check
          $ mkMarloweValidator
            rpvh
            (unsafeFromBuiltinData d)
            (unsafeFromBuiltinData r)
            (unsafeFromBuiltinData p)

  $$(PlutusTx.compile [||marloweValidator'||])
    `applyArg` rolePayoutValidatorHash
 --in case errorOrApplied of
 --     Haskell.Left err -> Haskell.error $ "Application of role-payout validator hash to marlowe validator failed." Haskell.<> err
 --     Haskell.Right applied -> applied

-- | The hash of the Marlowe validator.
marloweValidatorHash :: ScriptHash
marloweValidatorHash = hashScript PlutusV3 marloweValidator

-- | The serialisation of the Marlowe validator.
marloweValidatorBytes :: SerialisedScript
marloweValidatorBytes = serialiseCompiledCode marloweValidator


-- policy :: RoleTokens -> PV2.TxOutRef -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinUnit)
-- policy roleTokens txOutRef =
--   let roleTokensHash = mkRoleTokensHash roleTokens
--       errorOrApplied =
--         $$(PlutusTx.compile [||\rs seed -> wrapMintingPolicy (mkPolicy rs seed)||])
--           `PlutusTx.applyCode` PlutusTx.liftCode plcVersion200 roleTokensHash
--           >>= (`PlutusTx.applyCode` PlutusTx.liftCode plcVersion200 txOutRef)
--    in case errorOrApplied of
--         Haskell.Left err -> Haskell.error $ "Application of arguments to minting validator failed." Haskell.<> err
--         Haskell.Right applied -> applied
-- 
--
-- openRoleValidator,
-- openRoleValidatorBytes,
-- openRoleValidatorHash,

-- Copied from marlowe-cardano. This is pretty standard way to minimize size of the typed validator:
--  * Wrap validator function so it accepts raw `BuiltinData`.
--  * Create a validator which is simply typed.
--  * Create "typed by `Any` validator".
--  * Coerce it if you like. This step is not required - we only need `TypedValidator`.
-- openRoleValidator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
-- openRoleValidator =
--   let openRoleValidator' :: ScriptHash -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit
--       openRoleValidator' mvh d r p =
--         check
--           $ mkOpenRoleValidator
--             mvh
--             (unsafeFromBuiltinData d)
--             (unsafeFromBuiltinData r)
--             (unsafeFromBuiltinData p)
--       errorOrApplied =
--         $$(PlutusTx.compile [||openRoleValidator'||])
--           `PlutusTx.applyCode` PlutusTx.liftCode plcVersion200 marloweValidatorHash
--    in case errorOrApplied of
--         Haskell.Left err -> Haskell.error $ "Application of marlowe validator hash to openRole validator failed." Haskell.<> err
--         Haskell.Right applied -> applied
-- 
-- openRoleValidatorBytes :: SerialisedScript
-- openRoleValidatorBytes = serialiseCompiledCode openRoleValidator
-- 
-- openRoleValidatorHash :: ScriptHash
-- openRoleValidatorHash = hashScript openRoleValidator
