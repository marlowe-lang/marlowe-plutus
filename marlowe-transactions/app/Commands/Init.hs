module Commands.Init where

import Data.Aeson qualified as A
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
  value, flag', Alternative ((<|>)), auto,
 )
import qualified Text.Read as T
import qualified Cardano.Api as C
import System.Environment.Blank (getEnv)

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

data InitCommand = InitCommand
  { develScripts :: Bool
  , outputDir :: FilePath
  , messageFormat :: MessageFormat
  , networkId :: C.NetworkId
  , nodeSocketPath :: FilePath
  }

mkNodeSocketOpt :: IO (Parser FilePath)
mkNodeSocketOpt = do
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

getEnvNetworkId :: IO (Maybe C.NetworkId)
getEnvNetworkId =
  fmap (C.Testnet . C.NetworkMagic)
    . (T.readMaybe =<<)
    <$> getEnv "CARDANO_NODE_NETWORK_ID"

-- | Read the CARDANO_NODE_SOCKET_PATH environment variable for the default node socket path.
getEnvNodeSocketPath :: IO (Maybe FilePath)
getEnvNodeSocketPath = getEnv "CARDANO_NODE_SOCKET_PATH"

mkNetworkIdOpt :: IO (Parser C.NetworkId)
mkNetworkIdOpt = do
  possibleNodeNetworkId <- getEnvNetworkId
  pure do
    let
      networkMod = maybe mempty value possibleNodeNetworkId
      mainnetFlag = flag' C.Mainnet (long "mainnet" <> help "Execute on mainnet.")
      testnetOpt = option
          (C.Testnet . C.NetworkMagic . toEnum <$> auto)
          ( long "testnet-magic"
              <> metavar "INTEGER"
              <> networkMod
              <> help "Network magic. Defaults to the CARDANO_TESTNET_MAGIC environment variable's value."
          )
    mainnetFlag <|> testnetOpt

mkInitCommandParser :: IO (ParserInfo InitCommand)
mkInitCommandParser = do
  networkIdOpt <- mkNetworkIdOpt
  socketPathOpt <- mkNodeSocketOpt
  let
    desc = progDesc "Build initial marlowe transaction"
    cmd = InitCommand
      <$> switch
        ( long "devel-scripts"
            <> help "Compile the devel script variants with tracing preserved."
        )
      <*> strOption
        ( long "output-dir"
            <> short 'o'
            <> value "out"
            <> showDefault
            <> help "Directory where transaction file will be written."
        )
      <*> messageFormatParser
      <*> networkIdOpt
      <*> socketPathOpt
  pure $ info cmd desc

-- The goal is to call this function:
-- execInit
--   :: forall era m v
--    . (MonadUnliftIO m, C.IsCardanoEra era, MonadLog m)
-- FIXME: Probalby we can replace this one with a direct call to the marlowe-binaries
--   => MkRoleTokenMintingPolicy m
--
--   -> C.CardanoEra era
-- TODO: We should also accept a JSON file with the script info
-- , getCurrentScripts = ScriptRegistry.getCurrentScripts
--   -> GetCurrentScripts v
--
-- TODO:
-- --   Accept a connection to the cardano-node or a path to the files with era history and system-start
--   let solveConstraints :: Constraints.SolveConstraints era v
--       solveConstraints =
--         Constraints.solveConstraints
--           systemStart
--           (toLedgerEpochInfo eraHistory)
--   -> SolveConstraints era v
--
-- TODO: Accept a connection to the cardano-node or a path to the protocol parameters file
--   -> C.LedgerProtocolParameters era
--
-- type LoadWalletContext m = WalletAddresses -> m WalletContext
--   -> LoadWalletContext m
--
--   -> LoadHelpersContext m
--
--   -> C.NetworkId
--
--   -> Maybe Chain.StakeCredential
--   -> MarloweVersion v
--
--   -> WalletAddresses
--   -> Maybe Chain.TokenName
--   -> RoleTokensConfig
--   -> MarloweTransactionMetadata
--   -> Maybe Chain.Lovelace
--   -> Accounts
--   -> Either (Contract v) Chain.DatumHash
--   -> NominalDiffTime
--   -> m (Either InitError (ContractInitd v))

runInitCommand :: InitCommand -> IO ()
runInitCommand InitCommand{outputDir, messageFormat} = do
  case messageFormat of
    MessageFormatText -> putStrLn $ "Creating initial Marlowe transaction to: " <> show outputDir <> "."
    _ -> pure ()
  pure ()

-- emitSummary :: MessageFormat -> CompileResponse -> IO ()
-- emitSummary messageFormat response@CompileResponse{responseVariant, responseOutputDir, responseScripts} =
--   case messageFormat of
--     MessageFormatText -> do
--       let variantLabel = case responseVariant of
--             DevelScripts -> "devel"
--             ProductionScripts -> "production"
--       putStrLn $ "Finished writing " <> variantLabel <> " scripts to " <> show responseOutputDir <> "."
--       mapM_ printScript responseScripts
--     MessageFormatJson ->
--       LBS8.putStrLn $ A.encodePretty response
--     MessageFormatYaml ->
--       BS8.putStrLn $ Y.encode response
--  where
--   printScript script = do
--     putStrLn $ "  wrote " <> scriptFile script
--     putStrLn $ "  hash  " <> scriptHash script
-- 
-- emitError :: MessageFormat -> String -> IO a
-- emitError messageFormat err =
--   case messageFormat of
--     MessageFormatText -> die err
--     MessageFormatJson -> LBS8.putStrLn (A.encodePretty err) >> die "compile command failed"
--     MessageFormatYaml -> BS8.putStrLn (Y.encode err) >> die "compile command failed"
