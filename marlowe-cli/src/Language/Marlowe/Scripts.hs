module Language.Marlowe.Scripts (
  marloweValidator,
  openRolesValidator,
  payoutValidator,
) where

import Cardano.Api (PlutusScript (PlutusScriptSerialised), PlutusScriptV3)
import Marlowe.Plutus.Binaries.Production qualified as Binaries

marloweValidator :: PlutusScript PlutusScriptV3
marloweValidator = PlutusScriptSerialised Binaries.marloweValidatorBytes

-- TODO: openRolesValidator is not compiled in marlowe-binaries yet.
-- Using payoutValidator as placeholder - open roles won't work until this is fixed.
openRolesValidator :: PlutusScript PlutusScriptV3
openRolesValidator = PlutusScriptSerialised Binaries.rolePayoutValidatorBytes

payoutValidator :: PlutusScript PlutusScriptV3
payoutValidator = PlutusScriptSerialised Binaries.rolePayoutValidatorBytes
