module Language.Marlowe.CLI.Cardano.Api.Address where

import Cardano.Api qualified as C
import Cardano.Api qualified as CS
import Cardano.Ledger.Credential (StakeReference (StakeRefBase, StakeRefPtr, StakeRefNull))

toShelleyStakeReference
  :: C.StakeAddressReference
  -> StakeReference
toShelleyStakeReference (C.StakeAddressByValue stakecred) =
  StakeRefBase (C.toShelleyStakeCredential stakecred)
toShelleyStakeReference (C.StakeAddressByPointer ptr) =
  StakeRefPtr (C.unStakeAddressPointer ptr)
toShelleyStakeReference C.NoStakeAddress =
  StakeRefNull
