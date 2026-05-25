-- | Minimal stub
module Language.Marlowe.Runtime.Plutus.V3.Api where

import Language.Marlowe.Runtime.ChainSync.Api (TxOutRef (TxOutRef), TxId (TxId), TxIx (TxIx))
import qualified PlutusLedgerApi.V3 as PV3

toPlutusTxOutRef :: TxOutRef -> PV3.TxOutRef
toPlutusTxOutRef (TxOutRef (TxId txId) (TxIx txIx)) =
  PV3.TxOutRef (PV3.TxId . PV3.toBuiltin $ txId) (PV3.toBuiltin . toInteger $ txIx)
