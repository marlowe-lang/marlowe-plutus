module Commands where

import Data.Foldable (fold)
import Options.Applicative (
  Parser,
  command,
  hsubparser,
  info,
  progDesc,
 )
import Commands.Init (mkInitCommandParser, runInitCommand)
import Commands.GetContract (mkGetContractCommandParser, runGetContractCommand)

mkCommandParser :: IO (Parser (IO ()))
mkCommandParser = do
  contractCommandParser <- mkContractCommandParser
  pure . hsubparser . fold $
    [ command "contract" $ info contractCommandParser $ progDesc "Contract operations"
    ]

mkContractCommandParser :: IO (Parser (IO ()))
mkContractCommandParser = do
  initCommandParser <- mkInitCommandParser
  getContractParser <- mkGetContractCommandParser
  pure . hsubparser . fold $
    [ command "init" $ runInitCommand <$> initCommandParser
    , command "get" $ runGetContractCommand <$> getContractParser
    ]
