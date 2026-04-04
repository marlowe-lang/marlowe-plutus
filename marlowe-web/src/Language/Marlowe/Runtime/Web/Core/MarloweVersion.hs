module Language.Marlowe.Runtime.Web.Core.MarloweVersion (MarloweVersion (..)) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

data MarloweVersion = V1
  deriving (Show, Eq, Ord, Generic)

instance NFData MarloweVersion
