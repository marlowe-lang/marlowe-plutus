module Options where

import Commands (commandParser)
import Data.Foldable (fold)
import Data.Version (showVersion)
import Options.Applicative (
  InfoMod,
  Parser,
  ParserInfo,
  fullDesc,
  header,
  help,
  helper,
  info,
  infoOption,
  long,
  progDesc,
  short,
 )
import Paths_marlowe_binaries (version)

options :: ParserInfo (IO ())
options = info parser description

parser :: Parser (IO ())
parser = helper <*> versionOption <*> commandParser

versionOption :: Parser (a -> a)
versionOption =
  infoOption ("marlowe-binaries " <> showVersion version) $
    long "version" <> short 'v' <> help "Show version."

description :: InfoMod (IO ())
description =
  fold
    [ fullDesc
    , progDesc "CLI for working with Marlowe Plutus scripts."
    , header "marlowe-binaries: compile, export and benchmark Marlowe Plutus scripts."
    ]
