-- | Golden tests of contract execution.
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
module Marlowe.Plutus.Testing.Semantics.Golden (
  -- * Types
  GoldenCase,
  GoldenTransaction,

  -- * Reference contracts
  goldenContracts,
  goldenTransactions,
) where

import Marlowe.Plutus.Semantics (TransactionInput, TransactionOutput (..), computeTransaction)
import Marlowe.Plutus.Semantics.Types (Contract, State (State))
import PlutusLedgerApi.V2 (POSIXTime)

import qualified Marlowe.Plutus.AssocMap as AM (empty)
import qualified Marlowe.Plutus.Testing.Semantics.Golden.Escrow as Escrow (contract, valids)
import qualified Marlowe.Plutus.Testing.Semantics.Golden.Negative as Negative (contract, valids)
import qualified Marlowe.Plutus.Testing.Semantics.Golden.Pangram as Pangram (contract, valids)
import qualified Marlowe.Plutus.Testing.Semantics.Golden.Swap as Swap (contract, valids)
import qualified Marlowe.Plutus.Testing.Semantics.Golden.Trivial as Trivial (contract, valids)
import qualified Marlowe.Plutus.Testing.Semantics.Golden.ZeroCouponBond as ZCB (contract, valids)

-- | The golden contracts.
goldenContracts :: [Contract]
goldenContracts =
  [ Escrow.contract
  , Swap.contract
  , Trivial.contract
  , ZCB.contract
  , Negative.contract
  -- Note that Pangram is omitted because `getAllInputs` takes 30 minutes for it.
  ]

-- | A golden test case.
type GoldenCase = (POSIXTime, [TransactionInput], TransactionOutput)

-- | A golden transaction.
type GoldenTransaction = (State, Contract, TransactionInput, TransactionOutput)

-- | List all golden transactions.
goldenTransactions :: [[GoldenTransaction]]
goldenTransactions =
  uncurry validTransactions
    <$> [ (Escrow.contract, Escrow.valids)
        , (Pangram.contract, Pangram.valids)
        , (Swap.contract, Swap.valids)
        , (Trivial.contract, Trivial.valids)
        , (ZCB.contract, ZCB.valids)
        , (Negative.contract, Negative.valids)
        ]

-- | Extract all of the valid transactions from a golden test case.
validTransactions
  :: Contract
  -> [(POSIXTime, [TransactionInput], TransactionOutput)]
  -> [GoldenTransaction]
validTransactions contract =
  let progress (time, inputs, _) = progression (State AM.empty AM.empty AM.empty time) contract inputs
   in concatMap progress

-- | Apply input to a contract state.
progression
  :: State
  -> Contract
  -> [TransactionInput]
  -> [GoldenTransaction]
progression _ _ [] = []
progression state contract (input : inputs) =
  case computeTransaction input state contract of
    output@TransactionOutput{..} ->
      (state, contract, input, output)
        : progression txOutState txOutContract inputs
    _ -> []
