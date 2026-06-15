module Language.Marlowe.Runtime.Web.Core.MarloweVersion (MarloweVersion (..)) where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON (toJSON), FromJSON (parseJSON))
import Data.OpenApi (ToSchema (declareNamedSchema))
import Data.OpenApi qualified as OpenApi
import GHC.Generics (Generic)
import Data.Function ((&))
import Control.Lens ((?~))

data MarloweVersion = V1
  deriving (Show, Eq, Ord, Generic)

instance NFData MarloweVersion

instance ToJSON MarloweVersion where
  toJSON V1 = "v1"

instance FromJSON MarloweVersion where
  parseJSON "v1" = pure V1
  parseJSON _ = fail "Invalid Marlowe version tag. Expected 'v1'."

instance ToSchema MarloweVersion where
  declareNamedSchema _ =
    pure $
      OpenApi.NamedSchema (Just "MarloweVersion") $
        mempty
          & OpenApi.type_ ?~ OpenApi.OpenApiString
          & OpenApi.enum_
            ?~ [ "v1" ]
          & OpenApi.description
            ?~ "The version of the Marlowe language used in the contract."
