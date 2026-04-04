{-# LANGUAGE DeriveGeneric #-}

module Language.Marlowe.Runtime.Web.Core.Address (
  Address (..),
  StakeAddress (..),
) where

import qualified Data.Text as T
import GHC.Generics (Generic)

newtype Address = Address {unAddress :: T.Text}
  deriving (Eq, Ord, Generic)

newtype StakeAddress = StakeAddress {unStakeAddress :: T.Text}
  deriving (Eq, Ord, Generic)
