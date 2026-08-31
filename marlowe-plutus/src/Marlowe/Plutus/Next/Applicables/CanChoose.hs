{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Marlowe.Plutus.Next.Applicables.CanChoose (
  CanChoose (..),
  compactAdjoinedBounds,
  difference,
  overlaps,
) where

import Control.Monad ((<=<))
import Data.Aeson.Types (FromJSON (parseJSON), KeyValue ((.=)), ToJSON (toJSON), Value (Object), object, (.:))
import Data.List (tails)
import GHC.Generics
import qualified Marlowe.Plutus.Next.Applicables.Bound as Bound
import Marlowe.Plutus.Next.Indexed
import Marlowe.Plutus.Next.IsMerkleizedContinuation (IsMerkleizedContinuation)
import Marlowe.Plutus.Semantics.Types (Bound (..), ChoiceId)
import Prelude

data CanChoose = CanChoose
  { choiceId :: ChoiceId
  , bounds :: [Bound]
  , isMerkleizedContinuation :: IsMerkleizedContinuation
  }
  deriving stock (Show, Eq, Ord, Generic)

overlaps :: [Indexed CanChoose] -> Bool
overlaps xs = overlaps' $ getIndexedValue <$> xs

overlaps' :: [CanChoose] -> Bool
overlaps' l =
  let combinations = [(x, y) | (x : ys) <- tails l, y <- ys]
   in any (uncurry overlapWith) combinations

overlapWith :: CanChoose -> CanChoose -> Bool
overlapWith a b
  | choiceId a /= choiceId b =
      let combinations = [(x, y) | x <- bounds a, y <- bounds b]
       in any (uncurry Bound.overlapWith) combinations
overlapWith _ _ = False

boundsByChoiceId :: ChoiceId -> [Indexed CanChoose] -> [Bound]
boundsByChoiceId choiceId' = bounds <=< (filter (\x -> choiceId' == choiceId x) . fmap getIndexedValue)

compactAdjoinedBounds :: [Indexed CanChoose] -> [Bound]
compactAdjoinedBounds xs = Bound.compactAdjoinedBounds ((bounds . getIndexedValue) =<< xs)

difference :: Indexed CanChoose -> [Indexed CanChoose] -> Maybe (Indexed CanChoose)
difference (Indexed i CanChoose{..}) =
  ( \case
      [] -> Nothing
      newBounds -> Just $ Indexed i (CanChoose choiceId newBounds isMerkleizedContinuation)
  )
    . Bound.difference bounds
    . boundsByChoiceId choiceId

instance FromJSON (Indexed CanChoose) where
  parseJSON (Object v) =
    Indexed
      <$> (CaseIndex <$> v .: "case_index")
      <*> ( CanChoose
              <$> v .: "for_choice"
              <*> v .: "can_choose_between"
              <*> v .: "is_merkleized_continuation"
          )
  parseJSON _ = fail "CanChoose must be an object CanChoose "

instance ToJSON (Indexed CanChoose) where
  toJSON (Indexed caseIndex (CanChoose choiceId bounds isMerkleizedContinuation)) =
    object
      [ "for_choice" .= choiceId
      , "can_choose_between" .= bounds
      , "case_index" .= caseIndex
      , "is_merkleized_continuation" .= isMerkleizedContinuation
      ]
