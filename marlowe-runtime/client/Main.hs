module Main where

import Commands (mkCommandParser)
import Data.Version (showVersion)
import Paths_marlowe_runtime (version)
import Options.Applicative (
  ParserInfo,
  execParser,
  fullDesc,
  header,
  helper,
  info,
  infoOption,
  long,
  progDesc,
  short, help,
 )
import Control.Monad (join)

main :: IO ()
main = do
  commandParser <- mkCommandParser
  let
    versionOption =
      infoOption ("marlowe-runtime-client " <> showVersion version) $
        long "version" <> short 'v' <> help "Show version."

    parserInfo :: ParserInfo (IO ())
    parserInfo =
      info (helper <*> versionOption <*> commandParser) $
        fullDesc
          <> progDesc "Marlowe Runtime Client"
          <> header "marlowe-runtime-client - A client for the Marlowe Runtime API"

  join $ execParser parserInfo
