module Commands.Options.Address where

import Cardano.Api qualified as C
import Data.String (IsString(fromString))
import Options.Applicative ( ReadM, eitherReader, Parser, option)
import Options.Applicative qualified as O
import Data.Bifunctor (first)
import Data.Foldable (Foldable(fold))

readAddress :: ReadM (C.Address C.ShelleyAddr)
readAddress =
  eitherReader $
    first show . C.deserialiseFromBech32 . fromString

data AddressParserConfig = AddressParserConfig
  { long :: String
  , short :: Char
  , help :: String
  }

mkAddressParser :: AddressParserConfig -> Parser (C.Address C.ShelleyAddr)
mkAddressParser cfg =
  option readAddress $
    fold
      [ O.short cfg.short
      , O.long cfg.long
      , O.help cfg.help
      , O.metavar "BECH32"
      ]


