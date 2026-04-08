module Cardano.Debug.Json
  ( FormatJson (..)
  , FormatYaml (..)
  , encodeJson
  , encodeYaml
  , formatToLazyByteString
  , formatToByteString
  , formatToString
  , formatToText
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.Yaml.Pretty qualified as Yaml
import Vary (Vary)
import Vary qualified
import Data.Text qualified as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS

-- | Encode JSON with pretty printing.
encodeJson :: Aeson.ToJSON a => a -> LBS.ByteString
encodeJson =
  Aeson.encodePretty' jsonConfig

-- | Encode YAML with pretty printing.
encodeYaml :: Aeson.ToJSON a => a -> LBS.ByteString
encodeYaml =
  LBS.fromStrict . Yaml.encodePretty yamlConfig

jsonConfig :: Aeson.Config
jsonConfig =
  Aeson.defConfig
    { Aeson.confCompare = compare
    }

yamlConfig :: Yaml.Config
yamlConfig =
  Yaml.setConfCompare compare Yaml.defConfig


data FormatJson = FormatJson
  deriving (Enum, Eq, Ord, Show)

data FormatYaml = FormatYaml
  deriving (Enum, Eq, Ord, Show)

formatToLazyByteString :: Aeson.ToJSON a => Vary [FormatJson, FormatYaml] -> a -> LBS.ByteString
formatToLazyByteString format value = do
  format
    & ( Vary.on (\FormatJson -> encodeJson)
          . Vary.on (\FormatYaml -> encodeYaml)
          $ Vary.exhaustiveCase
      )
    $ value

formatToByteString :: Aeson.ToJSON a => Vary [FormatJson, FormatYaml] -> a -> BS.ByteString
formatToByteString format value = do
  LBS.toStrict (formatToLazyByteString format value)

formatToText :: Aeson.ToJSON a => Vary [FormatJson, FormatYaml] -> a -> T.Text
formatToText format value =
  T.decodeUtf8 (formatToByteString format value)

formatToString :: Aeson.ToJSON a => Vary [FormatJson, FormatYaml] -> a -> String
formatToString format value =
  T.unpack (formatToText format value)
