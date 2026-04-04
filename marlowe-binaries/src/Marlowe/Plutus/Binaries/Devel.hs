{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:preserve-logging #-}

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
{-# OPTIONS_GHC -fobject-code #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Marlowe.Plutus.Binaries.Devel where

import Marlowe.Plutus.Binaries.Core (applyArg, hashScript)
import Marlowe.Plutus.Scripts (mkMarloweValidator, mkRolePayoutValidator)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode, unsafeFromBuiltinData)
import PlutusLedgerApi.Data.V1 (ScriptHash)
import PlutusTx (CompiledCode)
import PlutusTx.Blueprint.PlutusVersion (PlutusVersion (PlutusV3))
import PlutusTx.Prelude (BuiltinData, BuiltinUnit, check, ($))
import PlutusTx qualified

{-# INLINEABLE rolePayoutValidator #-}
rolePayoutValidator :: CompiledCode (BuiltinData -> BuiltinUnit)
rolePayoutValidator =
  $$(PlutusTx.compile [||rolePayoutValidator'||])
 where
  rolePayoutValidator' :: BuiltinData -> BuiltinUnit
  rolePayoutValidator' ctx =
    check $ mkRolePayoutValidator (unsafeFromBuiltinData ctx)

rolePayoutValidatorHash :: ScriptHash
rolePayoutValidatorHash = hashScript PlutusV3 rolePayoutValidator

rolePayoutValidatorBytes :: SerialisedScript
rolePayoutValidatorBytes = serialiseCompiledCode rolePayoutValidator

{-# INLINEABLE marloweValidator #-}
marloweValidator :: CompiledCode (BuiltinData -> BuiltinUnit)
marloweValidator =
  $$(PlutusTx.compile [||marloweValidator'||])
    `applyArg` rolePayoutValidatorHash
 where
  marloweValidator' :: ScriptHash -> BuiltinData -> BuiltinUnit
  marloweValidator' rolePayoutHash ctx =
    check $ mkMarloweValidator rolePayoutHash (unsafeFromBuiltinData ctx)

marloweValidatorHash :: ScriptHash
marloweValidatorHash = hashScript PlutusV3 marloweValidator

marloweValidatorBytes :: SerialisedScript
marloweValidatorBytes = serialiseCompiledCode marloweValidator
