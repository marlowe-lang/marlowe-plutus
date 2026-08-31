module Commands.Options.ContractId where

import Options.Applicative (Parser, ReadM, option, long, help, metavar, eitherReader)
import qualified Data.ByteString as BS
import Language.Marlowe.Runtime.Web.Contract.API (ContractId)
import Language.Marlowe.Runtime.Web.Core.Tx (TxOutRef(TxOutRef), TxId (TxId))
import Text.Read (readMaybe)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS8
import Control.Error (note)

parseHex :: String -> Either String BS.ByteString
parseHex str = case Base16.decode (BS8.pack str) of
    Right bytes -> Right bytes
    Left err -> Left $ "Invalid hex string: " <> str <> ". Error: " <> show err

contractIdReader :: ReadM ContractId
contractIdReader = eitherReader $ \arg -> case break (== '#') arg of
  (txIdStr, '#' : txIxStr) -> do
    txIdBytes <- parseHex txIdStr
    txIx <- note "Invalid txIx: " $ readMaybe txIxStr
    if BS.length txIdBytes == 32
      then Right $ TxOutRef (TxId txIdBytes) txIx
      else Left $ "Invalid txId length: " <> arg <> ". Expected 64 hex characters (32 bytes)."
  _ -> Left $ "Invalid contract id format: " <> arg <> ". Expected format: <txId>#<txIx>."

contractIdParser :: Parser ContractId
contractIdParser = option contractIdReader
  ( long "contract-id"
    <> metavar "CONTRACT_ID"
    <> help "The id of the Marlowe contract to query, in the format <txId>#<txIx>."
  )

