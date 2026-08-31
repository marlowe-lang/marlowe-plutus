module Commands.GetContractSourceAdjacency where

import Commands.Formatters.Servant (clientErrorToJSON)
import Commands.GetContractSource (contractSourceIdParser)
import Commands.Options.MessageFormat (MessageFormat, messageFormatParser, emitJSONError, emitResponse)
import Commands.Options.ServantClientRunner (mkServantClientRunnerParser, ServantClientRunner (ServantClientRunner))
import Data.Set qualified as Set
import Language.Marlowe.Runtime.Web.Client (getContractSourceAdjacency)
import Language.Marlowe.Runtime.Web.Contract.API (ContractSourceId)
import Options.Applicative (ParserInfo, info, progDesc)
import Servant.Client (ClientError)

import Data.Set (Set)

data GetContractSourceAdjacencyCommand = GetContractSourceAdjacencyCommand
  { contractSourceId :: ContractSourceId
  , messageFormat :: MessageFormat
  , servantClientRunner :: ServantClientRunner
  }

mkGetContractSourceAdjacencyCommandParser :: IO (ParserInfo GetContractSourceAdjacencyCommand)
mkGetContractSourceAdjacencyCommandParser = do
  servantClientRunnerParser <- mkServantClientRunnerParser
  let
    desc = progDesc "Get the contract source IDs which are adjacent to the given contract source."
    prs =
      GetContractSourceAdjacencyCommand
        <$> contractSourceIdParser
        <*> messageFormatParser
        <*> servantClientRunnerParser
    opt = info prs desc
  pure opt

runGetContractSourceAdjacencyCommand :: GetContractSourceAdjacencyCommand -> IO ()
runGetContractSourceAdjacencyCommand cmd = do
  let
    ServantClientRunner runWebClient = cmd.servantClientRunner
  (result :: Either ClientError (Set ContractSourceId)) <- runWebClient $ getContractSourceAdjacency cmd.contractSourceId
  case result of
    Left err -> emitJSONError cmd.messageFormat (clientErrorToJSON err)
    Right adjacency -> emitResponse cmd.messageFormat (Set.toList adjacency)
