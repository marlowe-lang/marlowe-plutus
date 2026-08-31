module Commands.GetContractSource where

import Commands.Formatters.Servant (clientErrorToJSON)
import Commands.Options.MessageFormat (MessageFormat, messageFormatParser, emitJSONError, emitResponse)
import Commands.Options.ServantClientRunner (mkServantClientRunnerParser, ServantClientRunner (ServantClientRunner))
import Data.Text qualified as T
import Language.Marlowe.Runtime.Web.Client (getContractSource)
import Language.Marlowe.Runtime.Web.Contract.API (ContractSourceId)
import Marlowe.Plutus.Semantics.Types (Contract)
import Options.Applicative (Parser, ParserInfo, ReadM, eitherReader, help, info, long, metavar, option, progDesc, switch)
import Servant.API (FromHttpApiData (parseUrlPiece))
import Servant.Client (ClientError)

contractSourceIdReader :: ReadM ContractSourceId
contractSourceIdReader = eitherReader $ \s ->
  case parseUrlPiece (T.pack s) of
    Right csid -> Right csid
    Left err -> Left $ "Invalid contract source id: " <> s <> ". " <> T.unpack err

contractSourceIdParser :: Parser ContractSourceId
contractSourceIdParser = option contractSourceIdReader
  ( long "contract-source-id"
    <> metavar "CONTRACT_SOURCE_ID"
    <> help "The hex-encoded identifier of the contract source."
  )

data GetContractSourceCommand = GetContractSourceCommand
  { contractSourceId :: ContractSourceId
  , expand :: Bool
  , messageFormat :: MessageFormat
  , servantClientRunner :: ServantClientRunner
  }

mkGetContractSourceCommandParser :: IO (ParserInfo GetContractSourceCommand)
mkGetContractSourceCommandParser = do
  servantClientRunnerParser <- mkServantClientRunnerParser
  let
    desc = progDesc "Get a contract source by ID."
    prs =
      GetContractSourceCommand
        <$> contractSourceIdParser
        <*> switch
          ( long "expand"
            <> help "Demerkleize the contract before returning it."
          )
        <*> messageFormatParser
        <*> servantClientRunnerParser
    opt = info prs desc
  pure opt

runGetContractSourceCommand :: GetContractSourceCommand -> IO ()
runGetContractSourceCommand cmd = do
  let
    ServantClientRunner runWebClient = cmd.servantClientRunner
  (result :: Either ClientError Contract) <- runWebClient $ getContractSource cmd.contractSourceId cmd.expand
  case result of
    Left err -> emitJSONError cmd.messageFormat (clientErrorToJSON err)
    Right contract -> emitResponse cmd.messageFormat contract
