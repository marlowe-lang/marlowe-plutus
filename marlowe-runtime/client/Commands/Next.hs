module Commands.Next where

import Data.Time (UTCTime)
import qualified Data.Text as T
import Data.Aeson (Value)
import Options.Applicative (ParserInfo, progDesc, info, option, long, metavar, help, eitherReader, ReadM, many)
import Commands.Options.ContractId (contractIdParser)
import Commands.Options.MessageFormat (MessageFormat, messageFormatParser, emitJSONError, emitResponse)
import Commands.Options.ServantClientRunner (ServantClientRunner (ServantClientRunner), mkServantClientRunnerParser)
import Commands.Formatters.Servant (clientErrorToJSON)
import Language.Marlowe.Runtime.Web.Client (getContractNext)
import Language.Marlowe.Runtime.Web.Contract.API (ContractId)
import Language.Marlowe.Runtime.Web.Core.Party (Party (Party))
import Servant.Client (ClientError)

data NextCommand = NextCommand
  { contractId :: ContractId
  , validityStart :: UTCTime
  , validityEnd :: UTCTime
  , parties :: [Party]
  , messageFormat :: MessageFormat
  , servantClientRunner :: ServantClientRunner
  }

utctimeReader :: ReadM UTCTime
utctimeReader = eitherReader $ \s ->
  case reads s of
    [(t, "")] -> Right t
    _ -> Left $ "Invalid ISO-8601 timestamp: " <> s

partyReader :: ReadM Party
partyReader = eitherReader $ \s ->
  if null s
    then Left "Party cannot be empty"
    else Right (Party (T.pack s))

mkNextCommandParser :: IO (ParserInfo NextCommand)
mkNextCommandParser = do
  servantClientRunnerParser <- mkServantClientRunnerParser
  let
    desc = progDesc "Get the next applicable inputs for a Marlowe contract in a validity range, optionally filtered by party."
    prs =
      NextCommand
        <$> contractIdParser
        <*> option utctimeReader
          ( long "validity-start"
              <> metavar "ISO_TIMESTAMP"
              <> help "The beginning of the validity range (ISO-8601)."
          )
        <*> option utctimeReader
          ( long "validity-end"
              <> metavar "ISO_TIMESTAMP"
              <> help "The end of the validity range (ISO-8601)."
          )
        <*> many (option partyReader
          ( long "party"
              <> metavar "ROLE_OR_ADDRESS"
              <> help "Filter inputs by party (role name or bech32 address). May be given multiple times."
          ))
        <*> messageFormatParser
        <*> servantClientRunnerParser
    opt = info prs desc
  pure opt

runNextCommand :: NextCommand -> IO ()
runNextCommand cmd = do
  let
    ServantClientRunner runWebClient = cmd.servantClientRunner
  (result :: Either ClientError Value) <-
    runWebClient $ getContractNext cmd.contractId cmd.validityStart cmd.validityEnd cmd.parties
  case result of
    Left err -> emitJSONError cmd.messageFormat (clientErrorToJSON err)
    Right nextValue -> emitResponse cmd.messageFormat nextValue