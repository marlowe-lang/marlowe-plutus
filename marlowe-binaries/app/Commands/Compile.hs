module Commands.Compile where

import Data.Aeson qualified as A
import Data.Aeson.Encode.Pretty qualified as A
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Yaml qualified as Y
import Marlowe.Plutus.Binaries.Api.Compile (CompileRequest (..), CompileResponse (..), ScriptOutput (..), ScriptVariant (..), compileScripts)
import Options.Applicative (
  Parser,
  ParserInfo,
  ReadM,
  eitherReader,
  help,
  info,
  long,
  metavar,
  option,
  progDesc,
  short,
  showDefault,
  strOption,
  switch,
  value,
 )
import System.Exit (die)

data CompileCommand = CompileCommand
  { develScripts :: Bool
  , outputDir :: FilePath
  , messageFormat :: MessageFormat
  }

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

readMessageFormat :: ReadM MessageFormat
readMessageFormat = eitherReader $ \case
  "text" -> Right MessageFormatText
  "json" -> Right MessageFormatJson
  "yaml" -> Right MessageFormatYaml
  other -> Left $ "Unknown message format: " <> other <> ". Expected one of: text, json, yaml."

messageFormatParser :: Parser MessageFormat
messageFormatParser =
  option readMessageFormat
    ( long "message-format"
        <> metavar "text|json|yaml"
        <> value MessageFormatText
        <> showDefault
        <> help "Format of command output."
    )

compileCommandParser :: ParserInfo CompileCommand
compileCommandParser =
  info
    ( CompileCommand
        <$> switch
          ( long "devel-scripts"
              <> help "Compile the devel script variants with tracing preserved."
          )
        <*> strOption
          ( long "output-dir"
              <> short 'o'
              <> value "out"
              <> showDefault
              <> help "Directory where `.plutus` files and hashes will be written."
          )
        <*> messageFormatParser
    )
    (progDesc "Compile and export Marlowe validator scripts.")

runCompileCommand :: CompileCommand -> IO ()
runCompileCommand CompileCommand{develScripts, outputDir, messageFormat} = do
  let request =
        CompileRequest
          { requestVariant = if develScripts then DevelScripts else ProductionScripts
          , requestOutputDir = outputDir
          }
  case messageFormat of
    MessageFormatText -> putStrLn $ "Writing " <> show (requestVariant request) <> " scripts to " <> show outputDir <> "."
    _ -> pure ()
  compileScripts request >>=
    either (emitError messageFormat) (emitSummary messageFormat)

emitSummary :: MessageFormat -> CompileResponse -> IO ()
emitSummary messageFormat response@CompileResponse{responseVariant, responseOutputDir, responseScripts} =
  case messageFormat of
    MessageFormatText -> do
      let variantLabel = case responseVariant of
            DevelScripts -> "devel"
            ProductionScripts -> "production"
      putStrLn $ "Finished writing " <> variantLabel <> " scripts to " <> show responseOutputDir <> "."
      mapM_ printScript responseScripts
    MessageFormatJson ->
      LBS8.putStrLn $ A.encodePretty response
    MessageFormatYaml ->
      BS8.putStrLn $ Y.encode response
 where
  printScript script = do
    putStrLn $ "  wrote " <> scriptFile script
    putStrLn $ "  hash  " <> scriptHash script

emitError :: MessageFormat -> String -> IO a
emitError messageFormat err =
  case messageFormat of
    MessageFormatText -> die err
    MessageFormatJson -> LBS8.putStrLn (A.encodePretty err) >> die "compile command failed"
    MessageFormatYaml -> BS8.putStrLn (Y.encode err) >> die "compile command failed"
