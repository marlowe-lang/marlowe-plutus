module Commands.Benchmark
  ( benchmarkCommandParser
  ) where

import Data.Aeson qualified as A
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Foldable (fold)
import Data.Maybe (fromMaybe)
import Data.Yaml qualified as Y
import Marlowe.Binaries.Api.Benchmark
  ( BenchmarkCaseResult (..)
  , BenchmarkError (..)
  , BenchmarkGenerateRequest (..)
  , BenchmarkGenerateResponse (..)
  , BenchmarkGeneratedSuite (..)
  , BenchmarkRequest (..)
  , BenchmarkResponse (..)
  , ScriptName (..)
  , benchmarkScripts
  , generateBenchmarks
  )
import Options.Applicative
  ( Parser
  , ParserInfo
  , ReadM
  , command
  , eitherReader
  , help
  , hsubparser
  , info
  , long
  , metavar
  , option
  , optional
  , progDesc
  , showDefault
  , strOption
  , value
  )
import Paths_marlowe_binaries (getDataDir)
import System.Exit (die)
import System.FilePath ((</>))

data MessageFormat = MessageFormatText | MessageFormatJson | MessageFormatYaml
  deriving (Eq)

instance Show MessageFormat where
  show = \case
    MessageFormatText -> "text"
    MessageFormatJson -> "json"
    MessageFormatYaml -> "yaml"

data BenchmarkRunCommand = BenchmarkRunCommand
  { runSemanticsScriptFile :: Maybe FilePath
  , runBenchmarkRootDir :: Maybe FilePath
  , runMessageFormat :: MessageFormat
  }

data BenchmarkGenerateCommand = BenchmarkGenerateCommand
  { generateSemanticsScriptFile' :: Maybe FilePath
  , generateBenchmarkRootDir :: FilePath
  , generateMessageFormat :: MessageFormat
  }

benchmarkCommandParser :: ParserInfo (IO ())
benchmarkCommandParser =
  info
    ( hsubparser
        . fold
        $ [ command "generate" (runGenerateCommand <$> generateCommandParser)
          , command "run" (runRunCommand <$> runCommandParser)
          ]
    )
    (progDesc "Generate and run Marlowe benchmarks.")

runCommandParser :: ParserInfo BenchmarkRunCommand
runCommandParser =
  info
    ( BenchmarkRunCommand
        <$> scriptFileOption "semantics-script-file" "Path to the semantics script `.plutus` file. If omitted, the command reads from stdin."
        <*> optional
          ( strOption
              ( long "benchmark-dir"
                  <> metavar "DIR"
                  <> help "Benchmark root directory containing scenarios. Defaults to packaged benchmarks."
              )
          )
        <*> messageFormatParser
    )
    (progDesc "Run Marlowe benchmarks from a benchmark root directory.")

generateCommandParser :: ParserInfo BenchmarkGenerateCommand
generateCommandParser =
  info
    ( BenchmarkGenerateCommand
        <$> scriptFileOption "semantics-script-file" "Path to the semantics script `.plutus` file. If omitted, the command reads from stdin."
        <*> strOption
          ( long "output-dir"
              <> metavar "DIR"
              <> value "benchmarks"
              <> showDefault
              <> help "Benchmark root directory to write scenarios to."
          )
        <*> messageFormatParser
    )
    (progDesc "Generate Marlowe benchmark scenarios.")

runRunCommand :: BenchmarkRunCommand -> IO ()
runRunCommand BenchmarkRunCommand{runSemanticsScriptFile, runBenchmarkRootDir, runMessageFormat} = do
  dataDir <- getDataDir
  semanticsScriptFile <- resolveSemanticsScriptFile runSemanticsScriptFile
  let request =
        BenchmarkRequest
          { semanticsScriptFile
          , benchmarkRootDir = fromMaybe (dataDir </> "benchmarks") runBenchmarkRootDir
          }
  benchmarkScripts request >>= either (emitBenchmarkError runMessageFormat) (emitBenchmarkSummary runMessageFormat)

runGenerateCommand :: BenchmarkGenerateCommand -> IO ()
runGenerateCommand BenchmarkGenerateCommand{generateSemanticsScriptFile', generateBenchmarkRootDir, generateMessageFormat} = do
  semanticsScriptFile <- resolveSemanticsScriptFile generateSemanticsScriptFile'
  let request =
        BenchmarkGenerateRequest
          { generateSemanticsScriptFile = semanticsScriptFile
          , generateScenarioRootDir = generateBenchmarkRootDir
          }
  generateBenchmarks request >>= either (emitBenchmarkError generateMessageFormat) (emitGenerateSummary generateMessageFormat)

resolveSemanticsScriptFile :: Maybe FilePath -> IO FilePath
resolveSemanticsScriptFile (Just semanticsFile) = pure semanticsFile
resolveSemanticsScriptFile Nothing = do
  input <- LBS8.getContents
  either fail pure $ A.eitherDecode input

emitBenchmarkSummary :: MessageFormat -> BenchmarkResponse -> IO ()
emitBenchmarkSummary messageFormat response@BenchmarkResponse{benchmarkResults} =
  case messageFormat of
    MessageFormatText -> mapM_ printResult benchmarkResults
    MessageFormatJson -> LBS8.putStrLn $ A.encodePretty response
    MessageFormatYaml -> BS8.putStrLn $ Y.encode response
 where
  printResult result@BenchmarkCaseResult{..} = do
    putStrLn $ show scenarioId
    case result.result of
      Left err -> putStrLn $ "  error: " <> show err
      Right (usage, logs) -> do
        putStrLn $ "  cpu: " <> show (cpu usage)
        putStrLn $ "  memory: " <> show (memory usage)
        putStrLn $ "  logs: " <> show (logs.logs)

emitGenerateSummary :: MessageFormat -> BenchmarkGenerateResponse -> IO ()
emitGenerateSummary messageFormat response@BenchmarkGenerateResponse{generatedBenchmarkSuites} =
  case messageFormat of
    MessageFormatText -> mapM_ printSuite generatedBenchmarkSuites
    MessageFormatJson -> LBS8.putStrLn $ A.encodePretty response
    MessageFormatYaml -> BS8.putStrLn $ Y.encode response
 where
  printSuite BenchmarkGeneratedSuite{generatedScenarioDir, generatedScenarios} = do
    putStrLn $ "directory: " <> generatedScenarioDir
    putStrLn $ "  scenarios: " <> show (length generatedScenarios)

emitBenchmarkError :: MessageFormat -> BenchmarkError -> IO a
emitBenchmarkError messageFormat err@BenchmarkError{benchmarkErrorMessage} =
  case messageFormat of
    MessageFormatText -> die benchmarkErrorMessage
    MessageFormatJson -> LBS8.putStrLn (A.encodePretty err) >> die "benchmark command failed"
    MessageFormatYaml -> BS8.putStrLn (Y.encode err) >> die "benchmark command failed"

scriptFileOption :: String -> String -> Parser (Maybe FilePath)
scriptFileOption name description =
  optional
    ( strOption
        ( long name
            <> metavar "FILE"
            <> help description
        )
    )

messageFormatParser :: Parser MessageFormat
messageFormatParser =
  option readMessageFormat
    ( long "message-format"
        <> metavar "text|json|yaml"
        <> value MessageFormatText
        <> showDefault
        <> help "Format of command output."
    )

readMessageFormat :: ReadM MessageFormat
readMessageFormat = eitherReader $ \case
  "text" -> Right MessageFormatText
  "json" -> Right MessageFormatJson
  "yaml" -> Right MessageFormatYaml
  other -> Left $ "Unknown message format: " <> other <> ". Expected one of: text, json, yaml."
