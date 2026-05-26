module Main where

import Log (LogLevel (..), runLogT)
import Log.Backend.StandardOutput (withStdOutLogger)
import Log.Backend.StandardOutputInlined (withStdOutInlinedLogger)
import Control.Concurrent.Component (runComponent_)
import Control.Monad (join, when)
import Data.Version (showVersion)
import qualified Data.Set as Set
import qualified Hasql.Pool as Pool
import Language.Marlowe.Runtime.Plutus.V2.Api (fromPlutusValidatorHash)
import Language.Marlowe.Runtime.Indexer.Database.PostgreSQL as PostgreSQL
import Marlowe.Indexer (IndexerDependencies (..), mkIndexer)
import Marlowe.Indexer.NodeFollower (MemoryCostConfig (..), ChangesMemoryCostModel (..))
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  infoOption,
  long,
  metavar,
  option,
  progDesc,
  short,
  strOption, ReadM, asum, flag', eitherReader,
 )
import Paths_marlowe_indexer (version)
import Cardano.Api qualified as C
import Cardano.Api.Network (NetworkId (..))
import Marlowe.Plutus.Binaries.Devel (marloweValidatorHash, rolePayoutValidatorHash)
import Data.Foldable (Foldable(..))
import qualified Hasql.Connection.Settings as Hasql
import qualified Hasql.Pool.Config as Hasql
import qualified Data.Text as T
import Log.Class (logInfo_, logInfo)

data Options = Options
  { databaseUri :: Hasql.Settings
  , logLevel :: LogLevel
  , nodeSocketPath :: FilePath
  , networkId :: C.NetworkId
  }

longOption :: ReadM a -> String -> String -> String -> Parser a
longOption reader longText helpText metavarText =
  option reader (long longText <> help helpText <> metavar metavarText)

databaseUriParser :: Parser Hasql.Settings
databaseUriParser = do
  let
    readSettings = eitherReader \s -> do
      let
        settings = Hasql.connectionString (T.pack s)
      when (settings == mempty) $ Left ("Invalid database URI" :: String)
      pure settings

  longOption
    readSettings
    "database-uri"
    "URI of the claim processing database"
    "DATABASE_URI"

logLevelParser :: Parser LogLevel
logLevelParser =
  asum
    [ flag' LogTrace $
        fold
          [ long "verbose"
          , help "Enable trace-level logging"
          ]
    , flag' LogAttention $
        fold
          [ long "quiet"
          , help "Only emit attention-level logs"
          ]
    , pure LogInfo
    ]


nodeSocketPathParser :: Parser FilePath
nodeSocketPathParser =
  strOption $
    fold
      [ long "node-socket-file"
      , metavar "FILE_PATH"
      , help "The path to the node socket."
      ]

networkIdParser :: Parser C.NetworkId
networkIdParser =
  asum
    [ flag' Mainnet $
        fold
          [ long "mainnet"
          , help "Connect to a mainnet node."
          ]
    , Testnet . C.NetworkMagic
        <$> option
          auto
          ( fold
              [ long "testnet-magic"
              , help "Connect to a testnet node with the given network magic."
              ]
          )
    ]

runIndexer :: Options -> IO ()
runIndexer Options{..} = do
  pool <- do
    let
      cfg = Hasql.settings [ Hasql.staticConnectionSettings databaseUri ]
    Pool.acquire cfg
  putStrLn "Database connection pool created"
  let
    localNodeConnectInfo = C.LocalNodeConnectInfo
      { C.localConsensusModeParams = C.CardanoModeParams $ C.EpochSlots 21_600
      , C.localNodeNetworkId = networkId
      , C.localNodeSocketPath = C.File nodeSocketPath
      }
    dbQueries = PostgreSQL.databaseQueries pool
    marloweScriptHashes = Set.fromList
      [ fromPlutusValidatorHash marloweValidatorHash
      , fromPlutusValidatorHash rolePayoutValidatorHash
      ]
    memoryCostConfig = MemoryCostConfig
      { maxMemoryCost = 100_000
      , changesMemoryCostModel = ChangesMemoryCostModel
          { blockMemoryCost = 100
          , txMemoryCost = 1
          }
      }
    prettyLog = True
    withLogger = if prettyLog then withStdOutLogger else withStdOutInlinedLogger
  withLogger \logger ->
    runLogT "marlowe-indexer" logger logLevel do

    logInfo_ "Starting Marlowe Indexer:"
    logInfo "Script hashes:" marloweScriptHashes
    -- logInfo "Memory cost config:" memoryCostConfig
    -- logInfo "Node connection info:" localNodeConnectInfo

    runComponent_ $ mkIndexer IndexerDependencies
      { localNodeConnectInfo
      , databaseQueries = dbQueries
      , memoryCostConfig = memoryCostConfig
      , marloweScriptHashes = marloweScriptHashes
      }

parser :: Parser (IO ())
parser = do
  let
    parserOptions = Options
      <$> databaseUriParser
      <*> logLevelParser
      <*> nodeSocketPathParser
      <*> networkIdParser

    versionOption =
      infoOption ("marlowe-indexer " <> showVersion version) $
        long "version" <> short 'v' <> help "Show version."

  helper <*> versionOption <*> (runIndexer <$> parserOptions)

main :: IO ()
main = do
  let
    parserInfo :: ParserInfo (IO ())
    parserInfo = do
      let
        description =
          fullDesc
            <> progDesc "Marlowe Runtime Indexer - indexes Marlowe contracts from the blockchain."
            <> header "marlowe-indexer"
      info parser description
  join $ execParser parserInfo

