{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Marlowe.Testing.Semantics.Next.Arbitrary (

) where

import Test.QuickCheck (Arbitrary (..))

import Marlowe.Testing.Semantics.Arbitrary ()

import Language.Marlowe.Plutus.Next (Next (Next))

import Language.Marlowe.Plutus.Next.Applicables.CanChoose (CanChoose (CanChoose))
import Language.Marlowe.Plutus.Next.Applicables.CanDeposit (CanDeposit (..))
import Language.Marlowe.Plutus.Next.Applicables.CanNotify (CanNotify (..))
import Language.Marlowe.Plutus.Next.Indexed (CaseIndex (CaseIndex), Indexed (..))
import Language.Marlowe.Plutus.Next.IsMerkleizedContinuation (IsMerkleizedContinuation (IsMerkleizedContinuation))

import Language.Marlowe.Plutus.Next.Applicables (ApplicableInputs (ApplicableInputs))
import Language.Marlowe.Plutus.Next.CanReduce (CanReduce (CanReduce))

instance Arbitrary Next where
  arbitrary = Next . CanReduce <$> arbitrary <*> arbitrary

instance Arbitrary CaseIndex where
  arbitrary = CaseIndex <$> arbitrary

instance Arbitrary ApplicableInputs where
  arbitrary =
    ApplicableInputs <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Indexed CanNotify) where
  arbitrary = Indexed <$> (CaseIndex <$> arbitrary) <*> (CanNotify <$> (IsMerkleizedContinuation <$> arbitrary))

instance Arbitrary (Indexed CanDeposit) where
  arbitrary =
    Indexed
      <$> (CaseIndex <$> arbitrary)
      <*> (CanDeposit <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> (IsMerkleizedContinuation <$> arbitrary))

instance Arbitrary (Indexed CanChoose) where
  arbitrary =
    Indexed
      <$> (CaseIndex <$> arbitrary)
      <*> (CanChoose <$> arbitrary <*> arbitrary <*> (IsMerkleizedContinuation <$> arbitrary))
