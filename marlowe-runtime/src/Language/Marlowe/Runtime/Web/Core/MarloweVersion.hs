module Language.Marlowe.Runtime.Web.Core.MarloweVersion (MarloweVersion (..)) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.OpenApi (ToSchema)

data MarloweVersion = V1
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

instance NFData MarloweVersion
