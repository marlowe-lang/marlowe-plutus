-- | This module defines a server for the root REST API.
module Language.Marlowe.Runtime.Web.Server
  ( ServerDependencies(..)
  , runServer
  , server
  ) where

import Language.Marlowe.Runtime.Web.API (RuntimeAPI)
import Language.Marlowe.Runtime.Web.Server.Monad (ServerM, runServer, ServerDependencies(..))
import qualified Language.Marlowe.Runtime.Web.Contract.Server as Contract
import qualified Language.Marlowe.Runtime.Web.Payout.Server as Payout
import qualified Language.Marlowe.Runtime.Web.Role.Server as Role
import qualified Language.Marlowe.Runtime.Web.Withdrawal.Server as Withdrawals
import Servant (
  HasServer (ServerT),
  NoContent (..),
  type (:<|>) ((:<|>)),
 )

server :: ServerT RuntimeAPI ServerM
server =
  Contract.server
    :<|> Withdrawals.server
    :<|> Payout.server
    :<|> Role.server
    :<|> healthcheckServer

healthcheckServer :: ServerM NoContent
healthcheckServer = pure NoContent
