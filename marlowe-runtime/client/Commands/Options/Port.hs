module Commands.Options.Port where

import GHC.Generics (Generic)
import Control.Newtype (Newtype)
import Options.Applicative (Parser, option, auto, long, short, help, metavar, value)

newtype Port = Port Int
  deriving (Eq, Show, Generic)

instance Newtype Port Int

portParser :: Parser Port
portParser = option
  (Port <$> auto)
  ( long "server-port"
    <> short 'p'
    <> help "The port on which the web server is running. Defaults to 8090."
    <> metavar "PORT"
    <> value (Port 8090)
  )

