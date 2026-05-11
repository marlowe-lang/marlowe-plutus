{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.Web.Core.NetworkId (NetworkId (..)) where

import Data.Word (Word32)

data NetworkId
  = Mainnet
  | Testnet Word32
  deriving (Show, Eq, Ord)
