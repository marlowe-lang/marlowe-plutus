module Language.Marlowe.Scripts (
  marloweDevelValidatorWithoutTraces,
  payoutDevelValidatorWithoutTraces,
  marloweDevelValidatorWithTraces,
  payoutDevelValidatorWithTraces,
  openRolesValidator,
) where

import Cardano.Api (PlutusScript (PlutusScriptSerialised), PlutusScriptV3)
import Marlowe.Plutus.Binaries.Devel qualified as WithTraces
import Marlowe.Plutus.Binaries.Production qualified as WithoutTraces

marloweDevelValidatorWithoutTraces :: PlutusScript PlutusScriptV3
marloweDevelValidatorWithoutTraces = PlutusScriptSerialised WithoutTraces.marloweValidatorBytes

payoutDevelValidatorWithoutTraces :: PlutusScript PlutusScriptV3
payoutDevelValidatorWithoutTraces = PlutusScriptSerialised WithoutTraces.rolePayoutValidatorBytes

marloweDevelValidatorWithTraces :: PlutusScript PlutusScriptV3
marloweDevelValidatorWithTraces = PlutusScriptSerialised WithTraces.marloweValidatorBytes

payoutDevelValidatorWithTraces :: PlutusScript PlutusScriptV3
payoutDevelValidatorWithTraces = PlutusScriptSerialised WithTraces.rolePayoutValidatorBytes

-- TODO: openRolesValidator is not compiled in marlowe-binaries yet.
-- Using payoutValidator as placeholder - open roles won't work until this is fixed.
openRolesValidator :: PlutusScript PlutusScriptV3
openRolesValidator = PlutusScriptSerialised WithoutTraces.rolePayoutValidatorBytes

