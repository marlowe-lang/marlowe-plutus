module Commands.Options.MessageFormat where

import Options.Applicative (ReadM, Parser, help, long, metavar, option, showDefault, value, eitherReader)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import System.Exit (die)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Yaml.Pretty as Y
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.ByteString.Char8 as BS8

data MessageFormat = MessageFormatText | MessageFormatJson | MessageFormatYaml
  deriving (Eq)

instance Show MessageFormat where
  show = \case
    MessageFormatText -> "text"
    MessageFormatJson -> "json"
    MessageFormatYaml -> "yaml"

instance A.ToJSON MessageFormat where
  toJSON = A.String . \case
    MessageFormatText -> "text"
    MessageFormatJson -> "json"
    MessageFormatYaml -> "yaml"

messageFormatParser :: Parser MessageFormat
messageFormatParser = do
  let
    readMessageFormat :: ReadM MessageFormat
    readMessageFormat = eitherReader $ \case
      "text" -> Right MessageFormatText
      "json" -> Right MessageFormatJson
      "yaml" -> Right MessageFormatYaml
      other -> Left $ "Unknown message format: " <> other <> ". Expected one of: text, json, yaml."
  option readMessageFormat
    ( long "message-format"
        <> metavar "text|json|yaml"
        <> value MessageFormatText
        <> showDefault
        <> help "Format of command output."
    )

dieBS :: BS.ByteString -> IO a
dieBS = die . T.unpack . T.decodeUtf8

dieLBS :: LBS.ByteString -> IO a
dieLBS = dieBS . LBS.toStrict

yamlFormattingConfig :: Y.Config
yamlFormattingConfig = Y.setConfCompare compare Y.defConfig

type JsonStringifier = A.Value -> LBS.ByteString

emitJSONErrorWith :: MessageFormat -> A.Value -> JsonStringifier -> IO a
emitJSONErrorWith messageFormat json mkMsg = do
  case messageFormat of
    MessageFormatText -> dieLBS $ mkMsg json
    MessageFormatJson -> dieLBS $ A.encodePretty json
    MessageFormatYaml -> dieBS $ Y.encodePretty yamlFormattingConfig json

emitJSONError :: MessageFormat -> A.Value -> IO a
emitJSONError messageFormat json = emitJSONErrorWith messageFormat json A.encodePretty

emitErrorWith :: forall a b. A.ToJSON a => MessageFormat -> a -> JsonStringifier -> IO b
emitErrorWith messageFormat err = emitJSONErrorWith messageFormat (A.toJSON err)

emitError :: forall a b. A.ToJSON a => MessageFormat -> a -> IO b
emitError messageFormat err = emitErrorWith messageFormat err (A.encodePretty . A.toJSON)

-- When format is text then it uses the provided stringifier.
emitResponseWith :: A.ToJSON a => MessageFormat -> a -> JsonStringifier -> IO ()
emitResponseWith messageFormat response mkMsg = do
  let
    json = A.toJSON response
  case messageFormat of
    MessageFormatText -> LBS8.putStrLn (mkMsg json)
    MessageFormatJson -> LBS8.putStrLn (A.encodePretty json)
    MessageFormatYaml -> BS8.putStrLn $ Y.encodePretty yamlFormattingConfig json

emitResponse :: A.ToJSON a => MessageFormat -> a -> IO ()
emitResponse messageFormat response = do
  let
    stringifier res = do
      let
        yaml = LBS.fromStrict . Y.encodePretty yamlFormattingConfig . A.toJSON $ res
      "Command finished successfully:\n" <> yaml
  emitResponseWith messageFormat response stringifier

