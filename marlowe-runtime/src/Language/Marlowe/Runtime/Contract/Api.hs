{-# LANGUAGE StrictData #-}

-- | The contract-store data types that are shared between the runtime, the
-- web server, and the indexer. Mirrors the original
-- `Language.Marlowe.Runtime.Contract.Api` from the `marlowe-cardano` runtime
-- (see `.external-references/marlowe-cardano/marlowe-runtime/contract-api/Language/Marlowe/Runtime/Contract/Api.hs:127-140`).
module Language.Marlowe.Runtime.Contract.Api where

import Data.Set (Set)
import GHC.Generics (Generic)
import Language.Marlowe.Object.Types (ContractHash)
import Language.Marlowe.Runtime.Core.Api ()
import Marlowe.Plutus.Semantics.Types (Contract, Input, IntervalError)
import Data.Binary (Binary)
import Data.Variations (Variations)

-- | A contract with its adjacency and closure information.
data ContractWithAdjacency = ContractWithAdjacency
  { contractHash :: ContractHash
  -- ^ The hash of the contract.
  , contract :: Contract
  -- ^ The contract.
  , adjacency :: Set ContractHash
  -- ^ The set of continuation hashes explicitly contained in the contract.
  , closure :: Set ContractHash
  -- ^ The set of hashes contained in the contract and all recursive continuations of the contract.
  -- includes the hash of the contract itself.
  -- Does not contain the hash of the close contract.
  }
  deriving stock (Show, Eq, Ord, Generic)

data MerkleizeInputsError
  = MerkleizeInputsContractNotFound ContractHash
  | MerkleizeInputsApplyNoMatch Input
  | MerkleizeInputsApplyAmbiguousInterval Input
  | MerkleizeInputsReduceAmbiguousInterval Input
  | MerkleizeInputsIntervalError IntervalError
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary, Variations)
