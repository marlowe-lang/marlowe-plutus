-- | Minimal stub
module Language.Marlowe.Runtime.Cardano.Api where

import qualified Cardano.Api as C
import Language.Marlowe.Runtime.ChainSync.Api (PlutusScript(..), ScriptHash(..))
import qualified Ouroboros.Network.Block as O

type SlotNo = C.SlotNo

fromCardanoSlotNo :: O.SlotNo -> SlotNo
fromCardanoSlotNo = error "stub"

plutusScriptHash :: PlutusScript -> Maybe ScriptHash
plutusScriptHash = error "stub"

toCardanoAddressAny :: a -> Maybe b
toCardanoAddressAny = error "stub"

toCardanoPlutusScript :: PlutusScript -> Maybe (C.PlutusScript C.PlutusScriptV2)
toCardanoPlutusScript = error "stub"

toCardanoSlotNo :: SlotNo -> O.SlotNo
toCardanoSlotNo = error "stub"
