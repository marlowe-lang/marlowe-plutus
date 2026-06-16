module Commands.Options.CardanoNode where

import Cardano.Api qualified as C
import Commands.Formatters.Servant (clientErrorToJSON)
import Commands.Options.MessageFormat (MessageFormat (MessageFormatText), messageFormatParser, emitError, emitJSONError, emitResponseWith)
import Commands.Options.ServantClientRunner (mkServantClientRunnerParser, ServantClientRunner (ServantClientRunner))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson qualified as A
import Data.Aeson.Encode.Pretty qualified as A
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Foldable (Foldable(fold))
import Data.Set qualified as Set
import Data.String (IsString(fromString))
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Yaml qualified as Yaml
import Language.Marlowe.Runtime.Web.Client (postContract)
import Language.Marlowe.Runtime.Web.Contract.API (PostContractsRequest(PostContractsRequest, accounts, contract, metadata, minUTxODeposit, roles, tags, threadTokenName, version), ContractOrSourceId(ContractOrSourceId))
import Language.Marlowe.Runtime.Web.Core.Address qualified as Web
import Language.Marlowe.Runtime.Web.Core.MarloweVersion (MarloweVersion (V1))
import Language.Marlowe.Runtime.Web.Core.Tx qualified as Web
import Language.Marlowe.Runtime.Web.Tx.API (CreateTxEnvelope, CardanoTx)
import Options.Applicative ( Parser, ParserInfo, ReadM, eitherReader, help, info, long, metavar, option, progDesc, short, showDefault, strOption, switch, value, flag', Alternative ((<|>)), auto,)
import Servant.Client (ClientError)
import System.Environment.Blank (getEnv)
import System.FilePath ((</>))
import Text.Read qualified as T


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

