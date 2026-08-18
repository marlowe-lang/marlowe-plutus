{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Indexer.Database where

import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, SlotNo, IndexerTip, NodeTip)
import Language.Marlowe.Runtime.Indexer.MarloweBlock (MarloweBlock, MarloweUTxO)
import qualified Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.GetIntersectionPoints as GetIntersectionPoints

data DatabaseQueries m = DatabaseQueries
  { commitBlocks :: [MarloweBlock] -> m ()
  , commitIndexerTip :: IndexerTip -> m ()
  , commitNodeTip :: NodeTip -> m ()
  , commitRollback :: ChainPoint -> m ()
  , getIntersectionPoints :: GetIntersectionPoints.SecurityParameter -> m [BlockHeader]
  , getMarloweUTxO :: SlotNo -> m MarloweUTxO
  , getLatestMarloweUTxO :: m MarloweUTxO
  }

hoistDatabaseQueries :: (forall a. m a -> n a) -> DatabaseQueries m -> DatabaseQueries n
hoistDatabaseQueries f DatabaseQueries{..} =
  DatabaseQueries
    { commitBlocks = f <$> commitBlocks
    , commitIndexerTip = f <$> commitIndexerTip
    , commitNodeTip = f <$> commitNodeTip
    , commitRollback = f <$> commitRollback
    , getIntersectionPoints = f <$> getIntersectionPoints
    , getMarloweUTxO = f <$> getMarloweUTxO
    , getLatestMarloweUTxO = f getLatestMarloweUTxO
    }
