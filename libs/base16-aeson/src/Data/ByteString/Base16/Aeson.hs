-- | Encoding and decoding of 'ByteString' and serialisable values
--   as base16 encoded JSON strings
module Data.ByteString.Base16.Aeson (
  EncodeBase16 (..),
  byteStringFromJSON,
  byteStringToJSON,
  unBase16,
) where

import Data.Aeson (FromJSON, ToJSON (toJSON), FromJSONKey)
import Data.Aeson qualified as A
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as A
import Data.Base16.Types (extractBase16)
import Data.Bifunctor (first)
import Data.ByteString qualified as BSS
import Data.ByteString.Base16 (decodeBase16Untyped, encodeBase16)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE

newtype EncodeBase16 = EncodeBase16 { bytes :: BSS.ByteString }
  deriving (Eq, Ord, Show)

unBase16 (EncodeBase16 bs) = bs

instance ToJSON EncodeBase16 where
  toJSON (EncodeBase16 bs) = do
    let text = extractBase16 . encodeBase16 $ bs
    toJSON text

instance FromJSON EncodeBase16 where
  parseJSON v = do
    let base16Parser = either (fail . T.unpack) pure . decodeBase16Untyped . TE.encodeUtf8
    EncodeBase16 <$> Aeson.withText "ByteString" base16Parser v

instance A.ToJSONKey EncodeBase16 where
  toJSONKey = A.toJSONKeyText (\(EncodeBase16 bytes) -> extractBase16 . encodeBase16 $ bytes)

instance FromJSONKey EncodeBase16 where
  fromJSONKey = A.FromJSONKeyValue A.parseJSON

byteStringToJSON :: BSS.ByteString -> Aeson.Value
byteStringToJSON = toJSON . EncodeBase16

byteStringFromJSON :: Aeson.Value -> Aeson.Result BSS.ByteString
byteStringFromJSON v = unBase16 <$> Aeson.fromJSON v
