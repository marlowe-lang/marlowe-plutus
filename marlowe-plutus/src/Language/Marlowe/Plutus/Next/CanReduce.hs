{-# LANGUAGE NoImplicitPrelude #-}

module Language.Marlowe.Plutus.Next.CanReduce (
  CanReduce (..),
  tryReduce,
) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson.Types ()

import Deriving.Aeson (Generic)
import Language.Marlowe.Plutus.Semantics (
  ReduceResult (ContractQuiescent, RRAmbiguousTimeIntervalError),
  reduceContractUntilQuiescent,
 )
import Language.Marlowe.Plutus.Semantics.Types (Contract, Environment, State)
import Prelude

newtype CanReduce = CanReduce {unCanReduce :: Bool}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

tryReduce :: Environment -> State -> Contract -> Either () (CanReduce, State, Contract)
tryReduce environment state =
  ( \case
      ContractQuiescent isReduced _ _ newState newContract -> Right (CanReduce isReduced, newState, newContract)
      RRAmbiguousTimeIntervalError -> Left ()
  )
    . reduceContractUntilQuiescent environment state
