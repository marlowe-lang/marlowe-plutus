{-# LANGUAGE DeriveGeneric #-}

module Language.Marlowe.Runtime.Web.Core.BlockHeader (
  BlockHeader (..),
) where

import Control.DeepSeq (NFData)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Web.Core.Base16 (Base16)
import Data.Aeson (ToJSON, FromJSON)
import Data.OpenApi (ToSchema)

data BlockHeader = BlockHeader
  { slotNo :: Word64
  , blockNo :: Word64
  , blockHeaderHash :: Base16
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

instance NFData BlockHeader
