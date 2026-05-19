module Language.Marlowe.Runtime.Web.Contract.Next.Server (
  server,
) where

import Control.Monad.Except (MonadError)
import Data.Time (UTCTime)
import Marlowe.Plutus.Next (Next)
import qualified Marlowe.Plutus.Next as Semantics
import Marlowe.Plutus.Semantics.Types (Contract, Environment, State)
import qualified Marlowe.Plutus.Semantics.Types as Semantics

import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Language.Marlowe.Runtime.Web.Server.DTO (ToDTO (toDTO), fromDTOThrow)

import Language.Marlowe.Runtime.Web.Server.ApiError (badRequest', badRequest'', notFoundWithErrorCode)
import Language.Marlowe.Runtime.Web.Server.Monad (ServerM, runEff1, loadContractL)
import Language.Marlowe.Runtime.Web.Contract.API (ContractState (..))
import Language.Marlowe.Runtime.Web.Contract.Next.API (NextAPI)
import Language.Marlowe.Runtime.Web.Core.Party (Party)
import Language.Marlowe.Runtime.Web.Core.Tx (TxOutRef)
import Servant (throwError, Union)
import Servant.Server (HasServer (ServerT))
import Language.Marlowe.Runtime.Web.Adapter.Servant.UVerbT (UVerbT, runUVerbT)
import Control.Monad.Trans.Class (lift)

server :: TxOutRef -> ServerT NextAPI ServerM
server = nextOverCardano'

nextOverCardano'
  :: TxOutRef
  -> UTCTime
  -> UTCTime
  -> [Party]
  -> ServerM (Union '[ Next ])
nextOverCardano' contractId validityStart validityEnd parties = runUVerbT do
    a <- lift $ fromDTOThrow (badRequest' "Invalid contract id value") contractId
    b <- lift $ fromDTOThrow (badRequest' "Invalid parties") (nonEmpty parties)
    nextOverCardano (Semantics.mkEnvironment validityStart validityEnd) a b

nextOverCardano
  :: Environment
  -> ContractId
  -> Maybe (NonEmpty Semantics.Party)
  -> UVerbT '[ Next ] ServerM Next
nextOverCardano environment contractId parties =
  -- FIXME: Turn those errors into typed errors
  runEff1 loadContractL contractId
    >>= whenNothingThrow (notFoundWithErrorCode "Contract not found" "contractNotFound")
    >>= whenNothingThrow (notFoundWithErrorCode "Contract Closed" "contractClosed")
      . notClosedContractMaybe
      . either toDTO toDTO
    >>= whenLeftThrow (badRequest'' "Invalid Interval" "invalidInterval")
      . fmap (Semantics.filterByParties parties)
      . (uncurry $ Semantics.next environment)

notClosedContractMaybe :: ContractState -> Maybe (State, Contract)
notClosedContractMaybe ContractState{state = Just state, currentContract = Just contract} = Just (state, contract)
notClosedContractMaybe _ = Nothing

whenNothingThrow :: (MonadError e m) => e -> Maybe a -> m a
whenNothingThrow err = maybe (throwError err) pure

whenLeftThrow :: (MonadError e m) => (a -> e) -> Either a b -> m b
whenLeftThrow toErr = either (throwError . toErr) pure
