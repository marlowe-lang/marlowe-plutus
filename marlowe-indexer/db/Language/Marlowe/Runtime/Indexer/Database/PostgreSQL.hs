module Language.Marlowe.Runtime.Indexer.Database.PostgreSQL where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Hasql.Pool (Pool)
import qualified Hasql.Pool as Pool
import Hasql.Transaction (Transaction)
import qualified Hasql.Transaction.Sessions as TS
import qualified Language.Marlowe.Runtime.Indexer.Database as DB
import qualified Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.CommitBlocks as CommitBlocks
import qualified Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.CommitRollback as CommitRollback
import qualified Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.GetIntersectionPoints as GetIntersectionPoints
import qualified Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.GetMarloweUTxO as GetMarloweUTxO
import UnliftIO (throwIO)

databaseQueries :: MonadIO m => Pool-> DB.DatabaseQueries m
databaseQueries pool =
  DB.DatabaseQueries
    { commitRollback = \point -> runTransaction pool $ CommitRollback.commitRollback point
    , commitBlocks = \blocks -> runTransaction pool $ CommitBlocks.commitBlocks blocks
    , getIntersectionPoints = \securityParameter -> runTransaction pool $ GetIntersectionPoints.getIntersectionPoints securityParameter
    , getMarloweUTxO = \slotNo -> runTransaction pool $ GetMarloweUTxO.getMarloweUTxO slotNo
    , getLatestMarloweUTxO = runTransaction pool GetMarloweUTxO.getLatestMarloweUTxO
    }

runTransaction :: MonadIO m => Pool -> Transaction a -> m a
runTransaction pool transaction = do
  result <- liftIO $ Pool.use pool $ TS.transaction TS.Serializable TS.Write transaction
  case result of
    Left ex -> throwIO ex
    Right a -> pure a
