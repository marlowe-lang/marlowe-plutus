module Spec.Marlowe.Semantics.Next.Contract.When.Notify (
  firstNotifyTrueIndex,
) where

import Marlowe.Plutus.Next.Indexed (CaseIndex, Indexed (..))
import Marlowe.Plutus.Next.IsMerkleizedContinuation (IsMerkleizedContinuation)
import Marlowe.Plutus.Semantics (evalObservation)
import Marlowe.Plutus.Semantics.Types (Action (Notify), Case, Contract, Environment, State)
import Marlowe.Plutus.Testing.Semantics.Arbitrary ()
import Spec.Marlowe.Semantics.Next.Contract.When (indexedCaseActions)

firstNotifyTrueIndex :: Environment -> State -> [Case Contract] -> Maybe CaseIndex
firstNotifyTrueIndex e s = firstNotifyTrueIndex' e s . indexedCaseActions

firstNotifyTrueIndex' :: Environment -> State -> [Indexed (IsMerkleizedContinuation, Action)] -> Maybe CaseIndex
firstNotifyTrueIndex' _ _ [] = Nothing
firstNotifyTrueIndex' e s ((Indexed caseIndex (_, Notify observation)) : _) | evalObservation e s observation = Just caseIndex
firstNotifyTrueIndex' e s (_ : xs) = firstNotifyTrueIndex' e s xs
