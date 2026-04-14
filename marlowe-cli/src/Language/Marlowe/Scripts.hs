module Language.Marlowe.Scripts (
  marloweValidator,
  openRolesValidator,
  payoutValidator,
) where

import Cardano.Api (PlutusScript, PlutusScriptV2)
import Cardano.Api qualified as C
import Marlowe.Plutus.Scripts qualified as MPS

marloweValidator :: PlutusScript PlutusScriptV2
marloweValidator = error "TODO: marloweValidator - needs PlutusScript generation from MPS.mkMarloweValidator"

openRolesValidator :: PlutusScript PlutusScriptV2
openRolesValidator = error "TODO: openRolesValidator - needs PlutusScript generation"

payoutValidator :: PlutusScript PlutusScriptV2
payoutValidator = error "TODO: payoutValidator - needs PlutusScript generation from MPS.mkRolePayoutValidator"
