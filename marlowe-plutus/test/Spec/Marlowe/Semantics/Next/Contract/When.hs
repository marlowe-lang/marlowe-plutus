module Spec.Marlowe.Semantics.Next.Contract.When (
  When' (..),
  indexedCaseActions,
  reducibleToAWhen,
) where

import Data.Bifunctor (first)
import Data.List.Index
import Language.Marlowe.Plutus.Next.Indexed
import Language.Marlowe.Plutus.Next.IsMerkleizedContinuation
import Language.Marlowe.Plutus.Semantics (ReduceResult (ContractQuiescent), reduceContractUntilQuiescent)
import Language.Marlowe.Plutus.Semantics.Types (Action, Case (..), Contract (When), Environment, State, Timeout)
import Marlowe.Testing.Semantics.Arbitrary ()
import PlutusLedgerApi.Common (ToData, UnsafeFromData)

data When' = When' {indexedActions :: [Indexed (IsMerkleizedContinuation, Action)], timeout :: Timeout}

reducibleToAWhen :: Environment -> State -> Contract -> Maybe (State, When')
reducibleToAWhen environment' state contract =
  case reduceContractUntilQuiescent environment' state contract of
    ContractQuiescent _ _ _ newState (When caseActions timeout' _) -> Just (newState, When' (indexedCaseActions caseActions) timeout')
    _otherwise -> Nothing

indexedCaseActions :: [Case Contract] -> [Indexed (IsMerkleizedContinuation, Action)]
indexedCaseActions =
  fmap (uncurry Indexed . first (CaseIndex . fromIntegral))
    . indexed
    . (getAction' <$>)

getAction' :: UnsafeFromData a => ToData a => Case a -> (IsMerkleizedContinuation, Action)
getAction' (Case action _) = (IsMerkleizedContinuation False, action)
getAction' (MerkleizedCase action _) = (IsMerkleizedContinuation True, action)
