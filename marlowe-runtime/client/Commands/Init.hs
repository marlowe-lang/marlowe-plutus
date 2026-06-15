module Commands.Init where

import Cardano.Api qualified as C
import Commands.Options.ServantClientRunner (mkServantClientRunnerParser, ServantClientRunner (ServantClientRunner))
import Commands.Options.MessageFormat (MessageFormat (MessageFormatText), messageFormatParser, emitError, emitJSONError, emitResponseWith)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson qualified as A
import Data.Aeson.Encode.Pretty qualified as A
import Data.Bifunctor (first)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Foldable (Foldable(fold))
import Data.Set qualified as Set
import Data.String (IsString(fromString))
import Data.Yaml qualified as Yaml
import Language.Marlowe.Runtime.Web.Client (postContract)
import Language.Marlowe.Runtime.Web.Contract.API (PostContractsRequest(PostContractsRequest, accounts, contract, metadata, minUTxODeposit, roles, tags, threadTokenName, version), ContractOrSourceId(ContractOrSourceId))
import Language.Marlowe.Runtime.Web.Core.Address qualified as Web
import Language.Marlowe.Runtime.Web.Core.MarloweVersion (MarloweVersion (V1))
import Language.Marlowe.Runtime.Web.Core.Tx qualified as Web
import Options.Applicative ( Parser, ParserInfo, ReadM, eitherReader, help, info, long, metavar, option, progDesc, short, showDefault, strOption, switch, value, flag', Alternative ((<|>)), auto,)
import Servant.Client (ClientError)
import System.FilePath ((</>))
import System.Environment.Blank (getEnv)
import Text.Read qualified as T
import Language.Marlowe.Runtime.Web.Tx.API (CreateTxEnvelope, CardanoTx)
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Commands.Formatters.Servant (clientErrorToJSON)

readAddress :: ReadM (C.Address C.ShelleyAddr)
readAddress =
  eitherReader $
    first show . C.deserialiseFromBech32 . fromString

fundingAddressParser :: Parser (C.Address C.ShelleyAddr)
fundingAddressParser =
  option readAddress $
    fold
      [ long "funding-wallet-address"
      , help "The address which pays for claim transactions."
      , metavar "BECH32"
      ]

data InitCommand = InitCommand
  { contractFile :: FilePath
  , develScripts :: Bool
  , fundingAddress :: C.Address C.ShelleyAddr
  , messageFormat :: MessageFormat
  , networkId :: C.NetworkId
  , nodeSocketPath :: FilePath
  , outputDir :: FilePath
  , servantClientRunner :: ServantClientRunner
  }

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

getEnvNetworkId :: IO (Maybe C.NetworkId)
getEnvNetworkId =
  fmap (C.Testnet . C.NetworkMagic)
    . (T.readMaybe =<<)
    <$> getEnv "CARDANO_NODE_NETWORK_ID"

-- | Read the CARDANO_NODE_SOCKET_PATH environment variable for the default node socket path.
getEnvNodeSocketPath :: IO (Maybe FilePath)
getEnvNodeSocketPath = getEnv "CARDANO_NODE_SOCKET_PATH"

decodeFileStrict :: (A.FromJSON a, MonadIO m) => MessageFormat -> FilePath -> m a
decodeFileStrict msgFormat filePath = do
  result <- liftIO $ Yaml.decodeFileEither filePath
  case result of
    Left err ->
      liftIO . emitError msgFormat $
        "Failed to parse the contract file. Details: " <> Yaml.prettyPrintParseException err
    Right v -> pure v

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
              <> help "Network magic. Defaults to the CARDANO_NODE_NETWORK_ID environment variable's value."
          )
    mainnetParser <|> testnetParser

mkInitCommandParser :: IO (ParserInfo InitCommand)
mkInitCommandParser = do
  networkIdParser <- mkNetworkIdParser
  socketPathParser <- mkNodeSocketParser
  servantClientRunnerParser <- mkServantClientRunnerParser
  let
    desc = progDesc "Build initial marlowe transaction"
    cmd = InitCommand
      <$> strOption do
        long "contract-file"
          <> metavar "CONTRACT_FILE"
          <> help "JSON input file for the contract."
      <*> switch do
        long "devel-scripts"
          <> help "Compile the devel script variants with tracing preserved."
      <*> fundingAddressParser
      <*> messageFormatParser
      <*> networkIdParser
      <*> socketPathParser
      <*> strOption do
        long "output-dir"
          <> short 'o'
          <> value "out"
          <> showDefault
          <> help "Directory where transaction file will be written."
      <*> servantClientRunnerParser
  pure $ info cmd desc

runInitCommand :: InitCommand -> IO ()
runInitCommand cmd = do
  case cmd.messageFormat of
    MessageFormatText -> putStrLn $ "Creating initial Marlowe transaction to: " <> show cmd.outputDir <> "."
    _ -> pure ()

  let
    connectInfo =
      C.LocalNodeConnectInfo
        (C.CardanoModeParams $ C.EpochSlots 21600)
        cmd.networkId
        (C.File cmd.nodeSocketPath)
  rawQueryResult <- do
    let
      query = C.queryUtxo C.ShelleyBasedEraConway (C.QueryUTxOByAddress $ Set.fromList [C.AddressShelley cmd.fundingAddress])
    C.executeLocalStateQueryExpr
      connectInfo
      C.VolatileTip
      query

  walletUtxos <- case rawQueryResult of
    Right (Right (Right walletUtxos@(C.UTxO walletUtxosMap))) -> do
      when (null walletUtxosMap) $
        emitError cmd.messageFormat $ "The funding address " <> show cmd.fundingAddress <> " has no UTXOs. Please provide an address with at least one UTXO."
      pure walletUtxos
    _ -> do
      emitError cmd.messageFormat ("Failed to query the cardano-node for necessary information." :: String)

  contract <- decodeFileStrict cmd.messageFormat cmd.contractFile
  let
    ServantClientRunner runWebClient = cmd.servantClientRunner
    stakeCredential = Nothing
    request = PostContractsRequest
      { tags = mempty
      , metadata = mempty
      , version = V1
      , roles = Nothing
      , threadTokenName = Nothing
      , contract = ContractOrSourceId $ Left contract
      , accounts = mempty
      , minUTxODeposit = Nothing
      }
    changeAddress = Web.Address $ C.serialiseToBech32 cmd.fundingAddress
    availableUTxOs = Web.fromCardanoUTxO walletUtxos
  (result :: Either ClientError (CreateTxEnvelope CardanoTx)) <- runWebClient $ postContract stakeCredential changeAddress availableUTxOs request
  case result of
    Left err -> emitJSONError cmd.messageFormat (clientErrorToJSON err)
    Right createTxEnvelope -> do
      let
        outputFile = cmd.outputDir </> "init.tx.json"
      LBS8.writeFile outputFile (A.encodePretty $ A.toJSON createTxEnvelope)
      emitResponseWith
        cmd.messageFormat
        createTxEnvelope
        \_ -> do
          let
            outputFile' = LBS.fromStrict . T.encodeUtf8 . T.pack $ outputFile
          "Initial transaction created successfully and written to " <> outputFile' <> "."

