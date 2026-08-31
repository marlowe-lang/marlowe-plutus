{-# LANGUAGE ApplicativeDo #-}

module Commands.Options.ServantClientRunner where

import GHC.Generics (Generic)
import Control.Newtype (Newtype)
import Options.Applicative (Parser, option, auto, long, short, help, metavar, value, showDefault)
import Servant.Client.Streaming (ClientM, ClientError, Scheme (Http), BaseUrl (BaseUrl), mkClientEnv, runClientM)
import Control.DeepSeq (NFData)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import qualified Control.Newtype as N

newtype Port = Port Int
  deriving (Eq, Show, Generic)

instance Newtype Port Int

newtype Host = Host String
  deriving (Eq, Show, Generic)

instance Newtype Host String

portParser :: Parser Port
portParser = option
  (Port <$> auto)
  ( long "server-port"
    <> short 'p'
    <> help "The port on which the web server is running. Defaults to 8090."
    <> metavar "PORT"
    <> value (Port 8090)
  )

hostParser :: Parser Host
hostParser = option
  (Host <$> auto)
  ( long "server-host"
    <> short 'h'
    <> help "The host on which the web server is running. Defaults to localhost."
    <> metavar "HOST"
    <> value (Host "localhost")
    <> showDefault
  )

data ServantClientRunnerOptions = ServantClientRunnerOptions
  { serverHost :: Host
  , serverPort :: Port
  }

servantClientRunnerOptionsParser :: Parser ServantClientRunnerOptions
servantClientRunnerOptionsParser =
  ServantClientRunnerOptions
    <$> hostParser
    <*> portParser

newtype ServantClientRunner = ServantClientRunner
  (forall a. NFData a => ClientM a -> IO (Either ClientError a))

-- You can either create the ServantClientRunner directly
-- after parsing of its options.
mkServantClientRunner :: ServantClientRunnerOptions -> IO ServantClientRunner
mkServantClientRunner cmd = do
  manager <- newManager defaultManagerSettings
  let
    baseUrl = BaseUrl Http (N.unpack cmd.serverHost) (N.unpack cmd.serverPort) ""
    clientEnv = mkClientEnv manager baseUrl
  pure $ ServantClientRunner $ flip runClientM clientEnv

-- You can also create the final parser in IO up front.
mkServantClientRunnerParser :: IO (Parser ServantClientRunner)
mkServantClientRunnerParser = do
  manager <- newManager defaultManagerSettings
  let
    mkRunner opts = do
      let
        baseUrl = BaseUrl Http (N.unpack opts.serverHost) (N.unpack opts.serverPort) ""
        clientEnv = mkClientEnv manager baseUrl
      ServantClientRunner $ flip runClientM clientEnv
  pure $ mkRunner <$> servantClientRunnerOptionsParser
