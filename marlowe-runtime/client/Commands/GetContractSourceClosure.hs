module Commands.GetContractSourceClosure where

import Commands.Formatters.Servant (clientErrorToJSON)
import Commands.GetContractSource (contractSourceIdParser)
import Commands.Options.MessageFormat (MessageFormat, messageFormatParser, emitJSONError, emitResponse)
import Commands.Options.ServantClientRunner (mkServantClientRunnerParser, ServantClientRunner (ServantClientRunner))
import Data.Set qualified as Set
import Language.Marlowe.Runtime.Web.Client (getContractSourceClosure)
import Language.Marlowe.Runtime.Web.Contract.API (ContractSourceId)
import Options.Applicative (ParserInfo, info, progDesc)
import Servant.Client (ClientError)

import Data.Set (Set)

data GetContractSourceClosureCommand = GetContractSourceClosureCommand
  { contractSourceId :: ContractSourceId
  , messageFormat :: MessageFormat
  , servantClientRunner :: ServantClientRunner
  }

mkGetContractSourceClosureCommandParser :: IO (ParserInfo GetContractSourceClosureCommand)
mkGetContractSourceClosureCommandParser = do
  servantClientRunnerParser <- mkServantClientRunnerParser
  let
    desc = progDesc "Get the contract source IDs which appear in the full hierarchy of the given contract source (including the ID of the source itself)."
    prs =
      GetContractSourceClosureCommand
        <$> contractSourceIdParser
        <*> messageFormatParser
        <*> servantClientRunnerParser
    opt = info prs desc
  pure opt

runGetContractSourceClosureCommand :: GetContractSourceClosureCommand -> IO ()
runGetContractSourceClosureCommand cmd = do
  let
    ServantClientRunner runWebClient = cmd.servantClientRunner
  (result :: Either ClientError (Set ContractSourceId)) <- runWebClient $ getContractSourceClosure cmd.contractSourceId
  case result of
    Left err -> emitJSONError cmd.messageFormat (clientErrorToJSON err)
    Right closure -> emitResponse cmd.messageFormat (Set.toList closure)
