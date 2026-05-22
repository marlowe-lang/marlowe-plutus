module Options where

import Commands (mkCommandParser)
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
import Paths_marlowe_transactions (version)

mkOptions :: IO (ParserInfo (IO ()))
mkOptions = do
  parser <- mkParser
  pure $ info parser description

mkParser :: IO (Parser (IO ()))
mkParser = do
  commandParser <- mkCommandParser
  pure $ helper <*> versionOption <*> commandParser

versionOption :: Parser (a -> a)
versionOption =
  infoOption ("marlowe-transactions " <> showVersion version) $
    long "version" <> short 'v' <> help "Show version."

description :: InfoMod (IO ())
description =
  fold
    [ fullDesc
    , progDesc "CLI for working with Marlowe transactions on Cardano."
    , header "marlowe-transactions: build, submit and inpsect Marlowe transactions."
    ]
