module Main where

import App.Contrib.Servant.Err500 (err500, err500JSON)
import App.Contrib.Servant.ResponseRewriterMiddleware as ResponseRewriterMiddleware
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import Control.Monad.IO.Class (liftIO)
import Control.Exception (throwIO, Exception, SomeException)
import Control.Exception.Lifted qualified as Exception.Lifted
import Log (LogLevel (..), runLogT, Logger, logAttention, LogT)
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
  serve,
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
  short, ReadM, asum, flag', eitherReader,
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
import Language.Marlowe.Runtime.Web.Server.Monad (ServerM)
import Data.Text (Text)

data Options = Options
  { databaseUri :: Hasql.Settings
  , logLevel :: LogLevel
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

-- mkRoleTokensPolicy (UseDevelScripts useDevelScripts) txOutRef tokens = do
--   let
--     mkPolicy = if useDevelScripts then Devel.mkRoleTokensPolicyBytes else Production.mkRoleTokensPolicyBytes
--     roleTokens = mkRoleTokens $ Map.toList tokens <&> \(TokenName bs, amount) -> do
--       (PV3.TokenName . PV3.toBuiltin $ bs, amount)
--   pure . fromPlutusSerialisedScript C.PlutusScriptV3 . mkPolicy roleTokens $ toPlutusTxOutRef txOutRef
-- 
-- mkInitContract
--   :: C.SystemStart
--   -> C.EraHistory
--   -> L.PParams (C.ShelleyLedgerEra C.DijkstraEra)
--   -> C.NetworkId
--   -> UseDevelScripts
--   -> InitContract
-- mkInitContract systemStart eraHistory protocolParams networkId useDevelScripts = do
--   let
--     solveConstraints = Constraints.solveConstraints systemStart (C.toLedgerEpochInfo eraHistory)
--     loadHelpersContext _ _ = pure $ Left LoadHelpersContextErrorNotFound
--     analysisTimeout = 60
-- 
--   -- type InitContract 
--   --   Maybe StakeCredential
--   --   -> WalletContext
--   --   -> Maybe TokenName
--   --   -> RoleTokensConfig
--   --   -> MarloweTransactionMetadata
--   --   -> Maybe Lovelace
--   --   -> Accounts
--   --   -> Either (Contract V1) DatumHash
--   --   -> m (Either InitError (ContractInitialized V1))
--   \stakeCredential walletContext threadTokenName roleTokensConfig transactionMetadata optMinAda accounts contract -> do
--     -- execInit
--     --   :: forall era m v
--     --    . (MonadUnliftIO m, C.IsCardanoEra era, MonadLog m)
--     --   => MkRoleTokenMintingPolicy m
--     --   -> C.CardanoEra era
--     --   -- -> Connector (QueryClient ContractRequest) m
--     --   -> GetCurrentScripts v
--     --   -> SolveConstraints era v
--     --   -- -> C.LedgerProtocolParameters era
--     --   -> Ledger.PParams (C.ShelleyLedgerEra era)
--     --   -> WalletContext
--     --   -> LoadHelpersContext m
--     --   -> C.NetworkId
--     --   -> Maybe Chain.StakeCredential
--     --   -> MarloweVersion v
--     --   -> Maybe Chain.TokenName
--     --   -> RoleTokensConfig
--     --   -> MarloweTransactionMetadata
--     --   -> Maybe Chain.Lovelace
--     --   -> Accounts
--     --   -> Either (Contract v) Chain.DatumHash
--     --   -> NominalDiffTime
--     --   -> m (Either InitError (ContractInitialized v))
--     initResult <- execInit
--       (mkRoleTokensPolicy useDevelScripts)
--       C.DijkstraEra
--       ScriptRegistry.getCurrentScripts
--       solveConstraints
--       protocolParams
--       walletContext
--       loadHelpersContext
--       cmd.networkId
--       stakeCredential
--       MarloweV1
--       threadTokenName
--       roleTokensConfig
--       transactionMetadata
--       optMinAda
--       accounts
--       (Left contract)
--       analysisTimeout
-- 
--     -- data InitError
--     --   = InitEraUnsupported AnyCardanoEra
--     --   | InitConstraintError ConstraintError
--     --   | InitLoadMarloweContextFailed LoadMarloweContextError
--     --   | InitLoadHelpersContextFailed LoadHelpersContextError
--     --   | InitBuildupFailed InitBuildupError
--     --   | InitTxOutputNotFound
--     --   | InitToCardanoError
--     --   | InitSafetyAnalysisFailed [SafetyError]
--     --   | -- | This error is thrown when the safety analysis process fails itself
--     --     -- due to a timeout or other reasons, such as missing merkleization data.
--     --     InitSafetyAnalysisError String
--     --   | InitContractNotFound String
--     --   | ProtocolParamNoUTxOCostPerByte
--     --   | InsufficientMinAdaDeposit Lovelace
--     --   deriving (Generic)
--     --
--     --  data ContractInitialized v = ContractInitialized
--     --    { createdContractId :: TxOutRef
--     --    , createdRolesCurrency :: Maybe RolesCurrency
--     --    , createdVersion :: MarloweVersion v
--     --    } deriving (Show, Eq)


mkServerDependencies :: Pool.Pool -> ServerDependencies ServerM
mkServerDependencies pool = do
  let
    dbQueries :: DatabaseQueries ServerM
    dbQueries =
      logDatabaseQueries $
        hoistDatabaseQueries
          (either (liftIO . throwIO) pure <=< liftIO . Pool.use pool)
          databaseQueries

  ServerDependencies
    { applyInputs = undefined
    , burnRoleTokens = undefined
    , initContract = undefined
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
runApp Options{..} = do
  pool <- do
    let
      cfg = Hasql.settings [ Hasql.staticConnectionSettings databaseUri ]
    Pool.acquire cfg
  putStrLn "Database connection pool created"

  let
    -- FIXME: Move these to Options
    port = 8090
    debugInfoHttpResponse = True

  Wai.withStdoutLogger \waiLogger -> do
    putStrLn $ "Server running on port " ++ show port
    let
      prettyLog = True
      dependencies = mkServerDependencies pool
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
          serverMiddleware debugInfoHttpResponse appLogger logLevel $
            serve api $
              hoistServer
                api
                (runServer appLogger logLevel dependencies)
                serverWithOpenApi

parser :: Parser (IO ())
parser = do
  let
    parserOptions = Options
      <$> databaseUriParser
      <*> logLevelParser

    versionOption =
      infoOption ("marlowe-runtime-server " <> showVersion version) $
        long "version" <> short 'v' <> help "Show version."

  helper <*> versionOption <*> (runApp <$> parserOptions)

main :: IO ()
main = do
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
