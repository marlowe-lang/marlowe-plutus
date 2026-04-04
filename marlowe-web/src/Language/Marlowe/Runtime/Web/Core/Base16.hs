module Language.Marlowe.Runtime.Web.Core.Base16 (Base16 (..)) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

newtype Base16 = Base16 {unBase16 :: String}
  deriving (Show, Eq, Ord, Generic)

instance NFData Base16
