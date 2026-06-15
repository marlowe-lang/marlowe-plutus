module Commands.GetContract where

import Commands.Options.ContractId (contractParser)
import Commands.Options.MessageFormat (MessageFormat, messageFormatParser, emitJSONError, emitResponse)
import Language.Marlowe.Runtime.Web.Contract.API (ContractId, ContractState)
import Language.Marlowe.Runtime.Web.Client (getContract)
import Options.Applicative (progDesc, info, ParserInfo)
import Servant.Client (ClientError)
import Commands.Formatters.Servant (clientErrorToJSON)
import Commands.Options.ServantClientRunner (ServantClientRunner (ServantClientRunner), mkServantClientRunnerParser)

data GetContractCommand = GetContractCommand
  { contractId :: ContractId
  , messageFormat :: MessageFormat
  , servantClientRunner :: ServantClientRunner
  }

mkGetContractCommandParser :: IO (ParserInfo GetContractCommand)
mkGetContractCommandParser = do
  servantClientRunnerParser <- mkServantClientRunnerParser
  let
    desc = progDesc "Get the current state of a Marlowe contract from the web server."
    prs =
      GetContractCommand
        <$> contractParser
        <*> messageFormatParser
        <*> servantClientRunnerParser
    opt = info prs desc
  pure opt

runGetContractCommand :: GetContractCommand -> IO ()
runGetContractCommand cmd = do
  let
    ServantClientRunner runWebClient = cmd.servantClientRunner
  (result :: Either ClientError ContractState) <- runWebClient $ getContract cmd.contractId
  case result of
    Left err -> emitJSONError cmd.messageFormat (clientErrorToJSON err)
    Right contractState -> do
      emitResponse
        cmd.messageFormat
        contractState

