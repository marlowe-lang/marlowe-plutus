{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Marlowe.Plutus.Next.IsMerkleizedContinuation (
  IsMerkleizedContinuation (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types ()

import Deriving.Aeson (Generic)
import Prelude

newtype IsMerkleizedContinuation = IsMerkleizedContinuation {unIsMerkleizedContinuation :: Bool}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)
