module Commands where

import Commands.Init (mkInitCommandParser, runInitCommand)
import Data.Foldable (fold)
import Options.Applicative (Parser, command, hsubparser)

mkCommandParser :: IO (Parser (IO ()))
mkCommandParser = do
  initCommandParser <- mkInitCommandParser
  pure $ hsubparser $
    fold
      [ command "init" $ runInitCommand <$> initCommandParser
      ]
