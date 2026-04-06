-- | Minimal stub
module Language.Marlowe.Runtime.Cardano.Api where

import qualified Cardano.Api as C
import Language.Marlowe.Runtime.ChainSync.Api (PlutusScript(..), ScriptHash(..))
import qualified Ouroboros.Network.Block as O

type SlotNo = C.SlotNo

fromCardanoSlotNo :: O.SlotNo -> SlotNo
fromCardanoSlotNo (O.SlotNo n) = C.SlotNo n

plutusScriptHash :: PlutusScript -> Maybe ScriptHash
plutusScriptHash (PlutusScript bs) = Just $ ScriptHash bs

toCardanoAddressAny :: a -> Maybe b
toCardanoAddressAny _ = Nothing

toCardanoPlutusScript :: PlutusScript -> Maybe (C.PlutusScript C.PlutusScriptV2)
toCardanoPlutusScript (PlutusScript _bs) = Nothing

toCardanoSlotNo :: SlotNo -> O.SlotNo
toCardanoSlotNo (C.SlotNo n) = O.SlotNo n
