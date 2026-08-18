{-# LANGUAGE StrictData #-}

module Marlowe.Indexer.Store where

import Control.Concurrent.Component (Component, mkComponent, mkComponent_)
import Control.Concurrent.STM (STM, modifyTVar, newTVar, readTVar, writeTVar)
import Control.Monad (forever, unless, guard)
import Data.Aeson (ToJSON, (.=))
import Data.Aeson qualified as A
import Data.Foldable (for_)
import Data.Function (on)
import Data.List (partition)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Semigroup (Last (..))
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.ChainSync.Api (ChainPoint, WithGenesis(..), IndexerTip(..), NodeTip, genesisIndexerTip, genesisNodeTip, ChainPoint, TxId, WithGenesis(..), chainTipFromChainPoint )
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Language.Marlowe.Runtime.History.Api (ExtractCreationError, ExtractMarloweTransactionError)
import Language.Marlowe.Runtime.Indexer.Database (DatabaseQueries (..))
import Language.Marlowe.Runtime.Indexer.MarloweBlock (MarloweBlock (..), MarloweTransaction (..))
import Log (MonadLog, logInfo)
import Marlowe.Indexer.MarloweChainFollower (ChainEvent (..))
import UnliftIO (MonadUnliftIO, atomically)

data StoreDependencies m = StoreDependencies
  { databaseQueries :: DatabaseQueries m
  , pullEvent :: STM ChainEvent
  }

-- | The store component aggregates changes into batches in one thread, and
-- pulls batches to save in another.
mkStore
  :: MonadUnliftIO m
  => MonadLog m
  => StoreDependencies m
  -> Component m ()
mkStore StoreDependencies{..} = do
  -- Spawn the aggregator thread
  readChanges <- mkAggregator pullEvent
  -- Spawn the persister thread to read the changes accumulated by the aggregator.
  mkPersister PersisterDependencies{..}

data ChangesStatistics = ChangesStatistics
  { blockCount :: Int
  , createTxCount :: Int
  , applyInputsTxCount :: Int
  , withdrawTxCount :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON ChangesStatistics

instance Semigroup ChangesStatistics where
  a <> b =
    ChangesStatistics
      { blockCount = on (+) blockCount a b
      , createTxCount = on (+) createTxCount a b
      , applyInputsTxCount = on (+) applyInputsTxCount a b
      , withdrawTxCount = on (+) withdrawTxCount a b
      }

instance Monoid ChangesStatistics where
  mempty = ChangesStatistics 0 0 0 0

subStats :: ChangesStatistics -> ChangesStatistics -> ChangesStatistics
subStats a b =
  ChangesStatistics
    { blockCount = on (-) blockCount a b
    , createTxCount = on (-) createTxCount a b
    , applyInputsTxCount = on (-) applyInputsTxCount a b
    , withdrawTxCount = on (-) withdrawTxCount a b
    }

computeStats :: MarloweBlock -> ChangesStatistics
computeStats MarloweBlock{..} = (foldMap computeTxStats transactions){blockCount = 1}
  where
    computeTxStats CreateTransaction{} = mempty{createTxCount = 1}
    computeTxStats ApplyInputsTransaction{} = mempty{applyInputsTxCount = 1}
    computeTxStats WithdrawTransaction{} = mempty{withdrawTxCount = 1}
    computeTxStats _ = mempty

data Changes = Changes
  { rollbackTo :: Maybe ChainPoint
  , blocks :: [MarloweBlock]
  , statistics :: ChangesStatistics
  , indexerTip :: IndexerTip
  , nodeTip :: NodeTip
  , invalidCreateTxs :: Map ContractId ExtractCreationError
  , invalidApplyInputsTxs :: Map TxId ExtractMarloweTransactionError
  }
  deriving (Show, Eq, Generic)

instance Semigroup Changes where
  a <> b =
    let a' = maybe id applyRollback (rollbackTo b) a
     in Changes
          { rollbackTo = rollbackTo a'
          , blocks = on (<>) blocks a' b
          , statistics = on (<>) statistics a' b
          , indexerTip = getLast $ on (<>) (Last . indexerTip) a b
          , nodeTip = getLast $ on (<>) (Last . nodeTip) a b
          , invalidCreateTxs = on (<>) invalidCreateTxs a b
          , invalidApplyInputsTxs = on (<>) invalidApplyInputsTxs a b
          }

instance Monoid Changes where
  mempty = Changes Nothing mempty mempty genesisIndexerTip genesisNodeTip mempty mempty

applyRollback :: ChainPoint -> Changes -> Changes
applyRollback Genesis _ = mempty{rollbackTo = Just Genesis}
applyRollback (At block) Changes{..} =
  if null blocksNotRolledBack
    then mempty{rollbackTo = Just (At block)}
    else
      mempty
        { blocks = blocksNotRolledBack
        , statistics = statistics `subStats` foldMap computeStats blocksRolledBack
        }
  where
    (blocksRolledBack, blocksNotRolledBack) = partition isRolledBack blocks
    isRolledBack MarloweBlock{..} = blockHeader > block


-- | TODO: Drop the aggregator component - it makes no sens really to optimize
-- | a blockchain pipeline around a database *and* we actually added
-- | node tip and indexer tip update here which should be pushed to the storage
-- | ASAP.
-- |
-- | The aggregator component pulls chain events and accumulates a batch of
-- changes to persist.
mkAggregator
  :: MonadUnliftIO m
  => MonadLog m
  => STM ChainEvent
  -> Component m (STM Changes)
mkAggregator pullEvent = mkComponent "indexer-store-aggregator" do
  -- A variable which will hold the accumulated changes.
  changesVar <- newTVar mempty

  let -- An action to read and empty the changes.
      readChanges = do
        -- Read the current changes
        changes <- readTVar changesVar
        -- Retry the STM transaction if the changes are empty
        guard (changes /= mempty)
        -- Empty the changes variable
        writeTVar changesVar mempty
        -- Return the changes
        pure changes

      -- The IO action loop to run in this component's thread.
      runAggregator = forever do
        -- Pull an event from the queue.
        event <- atomically pullEvent

        let -- Compute the changes for the pulled event.
            changes = case event of
              RollForward blocks indexerTip nodeTip ->
                mempty
                  { blocks = snd <$> blocks
                  , statistics = foldMap (computeStats . snd) blocks
                  -- I'm not sure if falling back to the genesis tip here is
                  -- the right thing to do.
                  , indexerTip
                  , nodeTip
                  , invalidCreateTxs = flip foldMap blocks \(_, block) -> flip foldMap (transactions block) \case
                      InvalidCreateTransaction contractId err -> Map.singleton contractId err
                      _ -> mempty
                  , invalidApplyInputsTxs = flip foldMap blocks \(_, block) -> flip foldMap (transactions block) \case
                      InvalidApplyInputsTransaction txId _ err -> Map.singleton txId err
                      _ -> mempty
                  }

              RollBackward point tip -> do
                mempty
                  { rollbackTo = Just point
                  , indexerTip = IndexerTip $ chainTipFromChainPoint point
                  , nodeTip = tip
                  }

        -- Append the new changes to the existing changes.
        atomically $ modifyTVar changesVar (<> changes)
  pure (runAggregator, readChanges)

data PersisterDependencies m = PersisterDependencies
  { databaseQueries :: DatabaseQueries m
  , readChanges :: STM Changes
  }

-- | A component to save batches of changes to the database.
mkPersister
  :: MonadUnliftIO m
  => MonadLog m
  => PersisterDependencies m
  -> Component m ()
mkPersister PersisterDependencies{..} = mkComponent_ "indexer-store-persister" $ forever do
  -- Read the next batch of changes.
  Changes{..} <- atomically readChanges

  logInfo "Saving changes to the database" $ A.object
    [ "indexerTip" .= indexerTip
    , "nodeTip" .= nodeTip
    , "statistics" .= statistics
    , "invalidCreateTxs" .= invalidCreateTxs
    , "invalidApplyInputsTxs" .= invalidApplyInputsTxs
    ]

  -- If there is a rollback, save it first.
  for_ rollbackTo \point -> do
    logInfo "Rollback point" point
    commitRollback databaseQueries point

  commitNodeTip databaseQueries nodeTip
  commitIndexerTip databaseQueries indexerTip

  -- If there are blocks to save, save them.
  unless (null blocks) $ commitBlocks databaseQueries blocks

