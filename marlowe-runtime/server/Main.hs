module Main where

import Cardano.Api qualified as C
import Language.Marlowe.Runtime.Core.ScriptRegistry qualified as ScriptRegistry
import Marlowe.Runtime.Server.Contrib.Servant.Err500 (err500, err500JSON)
import Marlowe.Runtime.Server.Contrib.Servant.ResponseRewriterMiddleware as ResponseRewriterMiddleware
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import Control.Monad.IO.Class (liftIO)
import Control.Exception (throwIO, Exception, SomeException, try)
import Control.Exception.Lifted qualified as Exception.Lifted
import Log (LogLevel (..), runLogT, Logger, logAttention, LogT, MonadLog)
import Log.Backend.StandardOutput (withStdOutLogger)
import Log.Backend.StandardOutputInlined (withStdOutInlinedLogger)
import Network.HTTP.Types qualified as H
import Control.Monad (join, when, (<=<))
import Data.Version (showVersion)
import qualified Hasql.Pool as Pool
import Language.Marlowe.Runtime.Query.Database.PostgreSQL (databaseQueries)
import Language.Marlowe.Runtime.Query.Database
    ( hoistDatabaseQueries,
      logDatabaseQueries,
      DatabaseQueries(getContractState) )
import Paths_marlowe_runtime (version)
import Servant (
  Application,
  ServerError (..),
  hoistServer,
  serve, Handler (Handler),
 )
import Servant.Server.Internal.ServerError (responseServerError)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Wai
import Network.Wai.Logger qualified as Wai
import Network.Wai.Middleware.Cors (
  CorsResourcePolicy (corsRequestHeaders),
  cors,
  simpleCorsResourcePolicy,
 )

import Options.Applicative (
  Parser,
  ParserInfo,
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
  short, ReadM, asum, flag', eitherReader, strOption, auto, showDefault, value,
 )
import Data.Foldable (Foldable(..))
import qualified Hasql.Connection.Settings as Hasql
import qualified Hasql.Pool.Config as Hasql
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.CaseInsensitive as CI
import Language.Marlowe.Runtime.Web.Server
  (runServer, serverWithOpenApi, ServerDependencies(..), RuntimeAPIWithOpenAPI)
import qualified Data.ByteString.Lazy as BSL
import Data.Proxy (Proxy(Proxy))
import Data.Functor ((<&>))
import Language.Marlowe.Runtime.Web.Server.Monad (ServerM, InitContract)
import Data.Text (Text)
import qualified Cardano.Ledger.Core as L
import qualified Language.Marlowe.Runtime.Transaction.Constraints as Constraints
import Language.Marlowe.Runtime.Transaction.Api (LoadHelpersContextError(LoadHelpersContextErrorNotFound))
import qualified PlutusLedgerApi.V3 as PV3
import Marlowe.Plutus.RoleTokens (mkRoleTokens)
import Marlowe.Plutus.Binaries.Production qualified as Production
import Marlowe.Plutus.Binaries.Devel qualified as Devel
import qualified Data.Map as Map
import Language.Marlowe.Runtime.Cardano.Api (fromPlutusSerialisedScript)
import Language.Marlowe.Runtime.Plutus.V3.Api (toPlutusTxOutRef)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Core
import Language.Marlowe.Runtime.Transaction.BuildConstraints (MkRoleTokenMintingPolicy)
import Language.Marlowe.Runtime.Transaction.Builders (execInit)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(MarloweV1))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import System.Exit (die)
import Control.Monad.Except (ExceptT(ExceptT))
import System.Environment.Blank (getEnv)
import qualified Text.Read as T
import Control.Applicative (Alternative((<|>)))

newtype Port = Port Int

data Options = Options
  { databaseUri :: Hasql.Settings
  , logLevel :: LogLevel
  , nodeSocketPath :: FilePath
  , networkId :: C.NetworkId
  , port :: Port
  }

longOption :: ReadM a -> String -> String -> String -> Parser a
longOption reader longText helpText metavarText =
  option reader (long longText <> help helpText <> metavar metavarText)

getEnvNetworkId :: IO (Maybe C.NetworkId)
getEnvNetworkId =
  fmap (C.Testnet . C.NetworkMagic)
    . (T.readMaybe =<<)
    <$> getEnv "CARDANO_NODE_NETWORK_ID"

mkNetworkIdParser :: IO (Parser C.NetworkId)
mkNetworkIdParser = do
  possibleNodeNetworkId <- getEnvNetworkId
  pure do
    let
      networkMod = maybe mempty value possibleNodeNetworkId
      mainnetParser = flag' C.Mainnet (long "mainnet" <> help "Execute on mainnet.")
      testnetParser = option
          (C.Testnet . C.NetworkMagic . toEnum <$> auto)
          ( long "testnet-magic"
              <> metavar "INTEGER"
              <> networkMod
              <> help "Network magic. Defaults to the CARDANO_TESTNET_MAGIC environment variable's value."
          )
    mainnetParser <|> testnetParser

mkNodeSocketParser :: IO (Parser FilePath)
mkNodeSocketParser = do
  possibleNodeSocketPath <- getEnv "CARDANO_NODE_SOCKET_PATH"
  pure $ strOption do
    let
      opt = long "socket-path"
        <> short 's'
        <> showDefault
        <> help "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value."
    case possibleNodeSocketPath of
      Just path -> opt <> value path
      Nothing -> opt

portParser :: Parser Port
portParser = option
  (Port <$> auto)
  ( long "port"
    <> short 'p'
    <> help "The port to serve the server on"
    <> metavar "PORT"
    <> value (Port 8090)
  )

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

newtype UseDevelScripts = UseDevelScripts Bool

mkRoleTokensPolicy :: Applicative m => UseDevelScripts -> MkRoleTokenMintingPolicy m
mkRoleTokensPolicy (UseDevelScripts useDevelScripts) txOutRef tokens = do
  let
    mkPolicy = if useDevelScripts then Devel.mkRoleTokensPolicyBytes else Production.mkRoleTokensPolicyBytes
    roleTokens = mkRoleTokens $ Map.toList tokens <&> \(Core.TokenName bs, amount) -> do
      (PV3.TokenName . PV3.toBuiltin $ bs, amount)
  pure . fromPlutusSerialisedScript C.PlutusScriptV3 . mkPolicy roleTokens $ toPlutusTxOutRef txOutRef

mkInitContract
  :: MonadUnliftIO m
  => MonadLog m
  => LedgerInfo
  -> UseDevelScripts
  -> InitContract m
mkInitContract (networkId, systemStart, eraHistory, protocolParams) useDevelScripts = do
  let
    solveConstraints = Constraints.solveConstraints systemStart (C.toLedgerEpochInfo eraHistory)
    loadHelpersContext _ _ = pure $ Left LoadHelpersContextErrorNotFound
    analysisTimeout = 60
  \stakeCredential walletContext threadTokenName roleTokensConfig transactionMetadata optMinAda accounts contract -> do
    execInit
      (mkRoleTokensPolicy useDevelScripts)
      C.ConwayEra
      ScriptRegistry.getCurrentScripts
      solveConstraints
      protocolParams
      walletContext
      loadHelpersContext
      networkId
      stakeCredential
      MarloweV1
      threadTokenName
      roleTokensConfig
      transactionMetadata
      optMinAda
      accounts
      contract
      analysisTimeout

type LedgerInfo = (C.NetworkId, C.SystemStart, C.EraHistory, L.PParams (C.ShelleyLedgerEra C.ConwayEra))

queryLedgerInfo :: C.NetworkId -> C.LocalNodeConnectInfo -> IO LedgerInfo
queryLedgerInfo networkId connectInfo = do
  rawQueryResult <- C.executeLocalStateQueryExpr connectInfo C.VolatileTip $
    (,,)
      <$> C.querySystemStart
      <*> C.queryEraHistory
      <*> C.queryProtocolParameters C.ShelleyBasedEraConway
  case rawQueryResult of
    Right (Right systemStart, Right eraHistory, Right (Right protocolParams)) -> do
      pure (networkId, systemStart, eraHistory, protocolParams)
    _ -> liftIO . die $ "Failed to query the cardano-node for necessary information."

mkServerDependencies
  :: Pool.Pool
  -> LedgerInfo
  -> ServerDependencies ServerM
mkServerDependencies pool ledgerInfo = do
  let
    dbQueries :: DatabaseQueries ServerM
    dbQueries =
      logDatabaseQueries $
        hoistDatabaseQueries
          (either (liftIO . throwIO) pure <=< liftIO . Pool.use pool)
          databaseQueries
  let
    initContract = mkInitContract ledgerInfo (UseDevelScripts True)
  ServerDependencies
    { applyInputs = undefined
    , burnRoleTokens = undefined
    , initContract
    , loadContract = fmap (fmap Right) . getContractState dbQueries
    , loadPayout = undefined
    , loadPayouts = undefined
    , loadTransaction = undefined
    , loadTransactions = undefined
    , loadWithdrawal = undefined
    , loadWithdrawals = undefined
    , withdraw = undefined
    }

runApp :: Options -> IO ()
runApp opts = do
  pool <- do
    let
      cfg = Hasql.settings [ Hasql.staticConnectionSettings opts.databaseUri ]
    Pool.acquire cfg
  putStrLn "Database connection pool created"

  let
    -- FIXME: Move these to Options
    debugInfoHttpResponse = True

  Wai.withStdoutLogger \waiLogger -> do
    let
      Port port = opts.port
      localNodeConnectInfo = C.LocalNodeConnectInfo
        { C.localConsensusModeParams = C.CardanoModeParams $ C.EpochSlots 21_600
        , C.localNodeNetworkId = opts.networkId
        , C.localNodeSocketPath = C.File opts.nodeSocketPath
        }
    putStrLn $ "Server running on port " ++ show port
    ledgerInfo <- liftIO $ queryLedgerInfo opts.networkId localNodeConnectInfo
    let
      prettyLog = True
      dependencies = mkServerDependencies pool ledgerInfo
      withLogger = if prettyLog then withStdOutLogger else withStdOutInlinedLogger
      errorRewriter = ResponseRewriterMiddleware.defaultErrorRewriter
      handleException :: SomeException -> Wai.Response
      handleException e = do
        let
          headers =
            [ (H.hContentType, "application/json")
            , ("Access-Control-Allow-Origin", "*")
            ]
        Wai.responseLBS
          H.internalServerError500
          headers
          (BSL.fromStrict . T.encodeUtf8 . T.pack $ show e)

      waiSettings =
        Wai.setOnExceptionResponse handleException $
          Wai.setPort port $
            Wai.setTimeout 600 $
              Wai.setLogger waiLogger Wai.defaultSettings

      api :: Proxy RuntimeAPIWithOpenAPI
      api = Proxy

    withLogger \appLogger -> do
      Wai.runSettings waiSettings do
        ResponseRewriterMiddleware.mkMiddleware errorRewriter $
          serverMiddleware debugInfoHttpResponse appLogger opts.logLevel $
            serve api $
              hoistServer
                api
                (Handler . ExceptT . try . runServer appLogger opts.logLevel dependencies)
                serverWithOpenApi

mkParser :: IO (Parser (IO ()))
mkParser = do
  nodeSocketParser <- mkNodeSocketParser
  networkIdParser <- mkNetworkIdParser
  let
    parserOptions = Options
      <$> databaseUriParser
      <*> logLevelParser
      <*> nodeSocketParser
      <*> networkIdParser
      <*> portParser

    versionOption =
      infoOption ("marlowe-runtime-server " <> showVersion version) $
        long "version" <> short 'v' <> help "Show version."

  pure $ helper <*> versionOption <*> (runApp <$> parserOptions)

main :: IO ()
main = do
  parser <- mkParser
  let
    parserInfo :: ParserInfo (IO ())
    parserInfo = do
      let
        description =
          fullDesc
            <> progDesc "Marlowe Runtime Server"
            <> header "marlowe-runtime-server - A server for the Marlowe Runtime API"
      info parser description
  join $ execParser parserInfo


decodeUtf8OrEncodeHex :: BS.ByteString -> T.Text
decodeUtf8OrEncodeHex bs =
  case T.decodeUtf8' bs of
    Left _ -> T.decodeUtf8 $ Base16.encode bs
    Right t -> t

runLogT' :: Logger -> LogLevel -> LogT IO a -> IO a
runLogT' = runLogT "marlowe-runtime-server"

serverMiddleware :: Bool -> Logger -> LogLevel -> Application -> Application
serverMiddleware debugInfoHttpResponse logger logLevel = do
  let
    accessControlAllowOriginKey = "Access-Control-Allow-Origin"
    accessControlAllowOrigin = (accessControlAllowOriginKey, "*")
    handle500Errors errMsg req res = do
      reqBody <- Wai.getRequestBodyChunk req
      let headers =
            Aeson.object $
              Wai.requestHeaders req <&> \(k, v) -> do
                let bs = CI.original k
                    key = decodeUtf8OrEncodeHex bs
                (Aeson.fromText key, Aeson.toJSON $ decodeUtf8OrEncodeHex v)
          err = Aeson.object
            [ "msg" Aeson..= ("Captured 500" :: Text)
            , "body" Aeson..=
              Aeson.object
                [ ("remaining-body", Aeson.String $ T.decodeUtf8 reqBody)
                , ("original-error", Aeson.String $ T.pack errMsg)
                , ("headers", headers)
                ]
            ]
      runLogT' logger logLevel $
        logAttention "500 Internal Server Error" err
      let errorResponse =
            if debugInfoHttpResponse
              then err500JSON err
              else err500 "Internal Server Error"
      res $ responseServerError errorResponse

    -- Catch any IO exception and return a 500 error with cors header
    catchMiddleware =
      protect
        [ ErrHandler $ \(e :: SomeException) req res -> handle500Errors (show e) req res
        ]
    -- Add missing CORS headers to the application raised error
    addCorsHeadersMiddleware =
      protect
        [ ErrHandler $ \err@ServerError{errHeaders, errHTTPCode} req res -> do
            let err' =
                  if accessControlAllowOriginKey `elem` fmap fst errHeaders
                    then err
                    else
                      err
                        { errHeaders =
                            ("Content-Type", "application/json") : accessControlAllowOrigin : errHeaders
                        }
            if errHTTPCode >= 500
              then handle500Errors (show err) req res
              else res $ responseServerError err'
        ]
    simpleCorsMiddleware = do
      let simpleCorsResourcePolicy' =
            simpleCorsResourcePolicy
              { corsRequestHeaders = ["Content-Type"]
              }
      cors (const $ Just simpleCorsResourcePolicy')
  addCorsHeadersMiddleware . simpleCorsMiddleware . catchMiddleware

data ErrHandler = forall e. (Exception e) => ErrHandler (e -> Application)

protect :: [ErrHandler] -> Wai.Middleware
protect handlers app req res = do
  let handlers' = fmap (\(ErrHandler h) -> Exception.Lifted.Handler \e -> h e req res) handlers
  Exception.Lifted.catches (app req res) handlers'
