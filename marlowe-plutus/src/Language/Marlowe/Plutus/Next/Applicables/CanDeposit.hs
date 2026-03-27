{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Marlowe.Plutus.Next.Applicables.CanDeposit (
  CanDeposit (..),
) where

import Data.Aeson (FromJSON (parseJSON), KeyValue ((.=)), ToJSON (toJSON), Value (Object), object, (.:))
import Data.Aeson.Types ()

import Deriving.Aeson (Generic)
import Language.Marlowe.Plutus.Semantics.Types (AccountId, Party, Token)

import Language.Marlowe.Plutus.Next.Indexed (CaseIndex (CaseIndex), Indexed (..))
import Language.Marlowe.Plutus.Next.IsMerkleizedContinuation (IsMerkleizedContinuation)
import Prelude

data CanDeposit = CanDeposit AccountId Party Token Integer IsMerkleizedContinuation
  deriving stock (Show, Eq, Ord, Generic)

instance FromJSON (Indexed CanDeposit) where
  parseJSON (Object v) =
    Indexed
      <$> (CaseIndex <$> v .: "case_index")
      <*> ( CanDeposit
              <$> v .: "into_account"
              <*> v .: "party"
              <*> v .: "of_token"
              <*> v .: "can_deposit"
              <*> v .: "is_merkleized_continuation"
          )
  parseJSON _ = fail "CanDeposit must be either an object"

instance ToJSON (Indexed CanDeposit) where
  toJSON (Indexed caseIndex (CanDeposit accountId party token quantity isMerkleizedContinuation)) =
    object
      [ "party" .= party
      , "can_deposit" .= quantity
      , "of_token" .= token
      , "into_account" .= accountId
      , "case_index" .= caseIndex
      , "is_merkleized_continuation" .= isMerkleizedContinuation
      ]
