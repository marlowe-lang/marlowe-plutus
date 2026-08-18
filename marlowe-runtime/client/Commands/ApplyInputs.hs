module Commands.ApplyInputs where

import Cardano.Api qualified as C
import Commands.Formatters.Servant (clientErrorToJSON)
import Commands.Options.ContractId (contractIdParser)
import Commands.Options.CardanoNode (mkNetworkIdParser, mkNodeSocketParser)
import Commands.Options.MessageFormat (MessageFormat (MessageFormatText), messageFormatParser, emitError, emitJSONError, emitResponseWith)
import Commands.Options.ServantClientRunner (mkServantClientRunnerParser, ServantClientRunner (ServantClientRunner))
import Control.Monad (when, unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson qualified as A
import Data.Aeson.Encode.Pretty qualified as A
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Yaml qualified as Yaml
import Language.Marlowe.Runtime.Web.Client (postTransaction)
import Language.Marlowe.Runtime.Web.Contract.API ( ContractId )
import Language.Marlowe.Runtime.Web.Contract.Transaction.API (PostTransactionsRequest(PostTransactionsRequest, inputs, invalidBefore, invalidHereafter, metadata, tags, version))
import Language.Marlowe.Runtime.Web.Core.Address qualified as Web
import Language.Marlowe.Runtime.Web.Core.MarloweVersion (MarloweVersion (V1))
import Language.Marlowe.Runtime.Web.Core.Tx qualified as Web
import Language.Marlowe.Runtime.Web.Tx.API (CardanoTx, ApplyInputsTxEnvelope)
import Options.Applicative ( Parser, ParserInfo, help, info, long, metavar, progDesc, short, showDefault, strOption, value,)
import Servant.Client (ClientError)
import System.FilePath ((</>))
import System.Directory (doesDirectoryExist)
import qualified Commands.Options.Address as Addr

walletAddressParser :: Parser (C.Address C.ShelleyAddr)
walletAddressParser = Addr.mkAddressParser $ Addr.AddressParserConfig
  { Addr.long = "user-wallet-address"
  , Addr.short = 'w'
  , Addr.help = "The address which both pays for the transaction and executes the contract action."
  }

data ApplyInputsCommand = ApplyInputsCommand
  { inputFile :: FilePath
  , contractId :: ContractId
  , messageFormat :: MessageFormat
  , networkId :: C.NetworkId
  , nodeSocketPath :: FilePath
  , outputDir :: FilePath
  , servantClientRunner :: ServantClientRunner
  , walletAddress :: C.Address C.ShelleyAddr
  }

decodeFileStrict :: (A.FromJSON a, MonadIO m) => MessageFormat -> FilePath -> m a
decodeFileStrict msgFormat filePath = do
  result <- liftIO $ Yaml.decodeFileEither filePath
  case result of
    Left err ->
      liftIO . emitError msgFormat $
        "Failed to parse the contract file. Details: " <> Yaml.prettyPrintParseException err
    Right v -> pure v

mkApplyInputsCommandParser :: IO (ParserInfo ApplyInputsCommand)
mkApplyInputsCommandParser = do
  networkIdParser <- mkNetworkIdParser
  socketPathParser <- mkNodeSocketParser
  servantClientRunnerParser <- mkServantClientRunnerParser
  let
    desc = progDesc "Build input application transaction"
    cmd = ApplyInputsCommand
      <$> strOption do
        short 'i'
          <> long "marlowe-inputs-file"
          <> metavar "MARLOWE_INPUTS_FILE"
          <> help "JSON file which contains a list of Marlowe contract inputs which should be applied."
      <*> contractIdParser
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
      <*> walletAddressParser
  pure $ info cmd desc

runApplyInputsCommand :: ApplyInputsCommand -> IO ()
runApplyInputsCommand cmd = do
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
      query = C.queryUtxo C.ShelleyBasedEraConway (C.QueryUTxOByAddress $ Set.fromList [C.AddressShelley cmd.walletAddress])
    C.executeLocalStateQueryExpr
      connectInfo
      C.VolatileTip
      query

  walletUtxos <- case rawQueryResult of
    Right (Right (Right walletUtxos@(C.UTxO walletUtxosMap))) -> do
      when (null walletUtxosMap) $
        emitError cmd.messageFormat $ "The funding address " <> show cmd.walletAddress <> " has no UTXOs. Please provide an address with at least one UTXO."
      pure walletUtxos
    _ -> do
      emitError cmd.messageFormat ("Failed to query the cardano-node for necessary information." :: String)

  marloweInputs <- decodeFileStrict cmd.messageFormat cmd.inputFile
  let
    ServantClientRunner runWebClient = cmd.servantClientRunner
    request = PostTransactionsRequest
      { inputs = marloweInputs
      , invalidBefore = Nothing
      , invalidHereafter = Nothing
      , metadata = mempty
      , tags = mempty
      , version = V1
      }
    changeAddress = Web.Address $ C.serialiseToBech32 cmd.walletAddress
    availableUTxOs = Web.fromCardanoUTxO walletUtxos
    outputFile = cmd.outputDir </> "apply-inputs.tx.json"
  outputDirExists <- liftIO $ doesDirectoryExist cmd.outputDir
  unless outputDirExists do
    emitError cmd.messageFormat $ "The output directory " <> show cmd.outputDir <> " does not exist. Please create it before running this command."

  (result :: Either ClientError (ApplyInputsTxEnvelope CardanoTx)) <- runWebClient $ postTransaction changeAddress availableUTxOs cmd.contractId request
  case result of
    Left err -> emitJSONError cmd.messageFormat (clientErrorToJSON err)
    Right applyInputsTxEnvelope -> do
      LBS8.writeFile outputFile (A.encodePretty $ A.toJSON applyInputsTxEnvelope)
      emitResponseWith
        cmd.messageFormat
        applyInputsTxEnvelope
        \_ -> do
          let
            outputFile' = LBS.fromStrict . T.encodeUtf8 . T.pack $ outputFile
          "Inputs application transaction created successfully and written to " <> outputFile' <> "."

