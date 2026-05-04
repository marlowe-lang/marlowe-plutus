module Marlowe.Indexer.MarloweChainFollower (
  MarloweChainFollowerDependencies (..),
  MarloweChainFollower (..),
  ChainEvent (..),
  LocalTip (..),
  NodeTip (..),
  genesisLocalTip,
  genesisNodeTip,
  mkMarloweChainFollower,
) where

import qualified Cardano.Api as C
import Control.Concurrent.STM (STM)
import qualified Marlowe.Indexer.NodeFollower as NodeFollower
import Marlowe.Indexer.NodeFollower.Block (withCardanoEraTxs)
import Language.Marlowe.Runtime.Indexer.MarloweBlock (MarloweBlock, MarloweUTxO)
import UnliftIO (MonadUnliftIO, newTQueue, writeTQueue, atomically, readTQueue, MonadIO (..))
import Log (MonadLog)
import qualified Log (logInfo_, logInfo, logAttention)
import Control.Concurrent.Component (Component, mkComponent)
import Data.Foldable (for_)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Control.Monad (unless)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import Marlowe.Indexer.MarloweChainFollower.Extractor (extractMarloweBlock)
import Language.Marlowe.Runtime.ChainSync.Api (ChainTip, ChainPoint, WithGenesis(..), genesisChainTip, ScriptHash)
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoChainTip, fromCardanoBlockHeader, fromCardanoTransaction)
import Marlowe.Indexer.NodeQuerier (NodeQuerier(..), Query (QueryHistory, QueryStartup), hoistNodeQuerier)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.Trans.Reader (runReaderT, ReaderT (..))
import Control.Monad.Trans.Class (lift, MonadTrans)
import Data.Set (Set)
import Control.Monad.State (runStateT)
import Data.Traversable (for)
import Control.Monad.Trans.Writer (runWriter)

-- | The set of dependencies needed by the NodeFollower component.
data MarloweChainFollowerDependencies m = MarloweChainFollowerDependencies
  { changes :: STM NodeFollower.Changes
  , getLatestMarloweUTxO :: m MarloweUTxO
  , marloweScriptHashes  :: Set ScriptHash
  , nodeQuerier :: NodeQuerier m
  }

hoistMarloweChainFollowerDependencies
  :: (forall x. m x -> n x)
  -> MarloweChainFollowerDependencies m
  -> MarloweChainFollowerDependencies n
hoistMarloweChainFollowerDependencies f MarloweChainFollowerDependencies{..} =
  MarloweChainFollowerDependencies
    { changes = changes
    , getLatestMarloweUTxO = f getLatestMarloweUTxO
    , marloweScriptHashes = marloweScriptHashes
    , nodeQuerier = hoistNodeQuerier f nodeQuerier
    }

newtype NodeTip = NodeTip ChainTip
  deriving newtype (Eq, Show, ToJSON)

genesisNodeTip :: NodeTip
genesisNodeTip = NodeTip genesisChainTip

fromCardanoNodeTip :: NodeFollower.NodeTip -> NodeTip
fromCardanoNodeTip (NodeFollower.NodeTip chainTip) = NodeTip $ fromCardanoChainTip chainTip

newtype LocalTip = LocalTip ChainTip
  deriving newtype (Eq, Show, ToJSON)

genesisLocalTip :: LocalTip
genesisLocalTip = LocalTip genesisChainTip

-- | A change to the chain with respect to Marlowe contracts
data ChainEvent
  = -- | A change in which a new block of Marlowe transactions is added to the chain.
    RollForward [(ChainPoint, MarloweBlock)] NodeTip
  | -- | A change in which the chain is reverted to a previous point, discarding later blocks.
    RollBackward ChainPoint NodeTip

newtype MarloweChainFollower = MarloweChainFollower
  { events :: STM ChainEvent
  -- ^ Stream of chain events relevant to Marlowe contracts.
  }

-- | Create a new NodeFollower component.
mkMarloweChainFollower
  :: forall m
   . MonadUnliftIO m
  => MonadLog m
  => MarloweChainFollowerDependencies m
  -> Component m MarloweChainFollower
mkMarloweChainFollower deps = mkComponent "marlowe-chain-follower" do
  eventQueue <- newTQueue
  let
    emit :: ChainEvent -> ThreadT m ()
    emit = lift . atomically . writeTQueue eventQueue

    deps' :: MarloweChainFollowerDependencies (ThreadT m)
    deps' = hoistMarloweChainFollowerDependencies lift deps

    follower :: ThreadT m ()
    follower = mkFollowerThread emit Nothing

    process :: m ()
    process = runThreadT follower deps'

  pure (process, MarloweChainFollower{events = readTQueue eventQueue})

logInfo_ :: MonadLog m => Text -> m ()
logInfo_ msg = Log.logInfo_ $ "[MarloweChainFollower] " <> msg

logInfo :: ToJSON a => MonadLog m => Text -> a -> m ()
logInfo msg = Log.logInfo ("[MarloweChainFollower] " <> msg)

logAttention :: ToJSON a => MonadLog m => Text -> a -> m ()
logAttention msg = Log.logAttention ("[MarloweChainFollower] " <> msg)

chainPointFromCardanoRollback :: NodeFollower.RollbackToBlock -> ChainPoint
chainPointFromCardanoRollback (NodeFollower.RollbackToBlock Nothing) = Genesis
chainPointFromCardanoRollback (NodeFollower.RollbackToBlock (Just blockHeader)) =
  At $ fromCardanoBlockHeader blockHeader

newtype ThreadT m a = ThreadT { _thread :: ReaderT (MarloweChainFollowerDependencies (ThreadT m)) m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLog, MonadReader (MarloweChainFollowerDependencies (ThreadT m)))

instance MonadTrans ThreadT where
  lift = ThreadT . lift

runThreadT :: ThreadT m a -> MarloweChainFollowerDependencies (ThreadT m) -> m a
runThreadT (ThreadT action) = runReaderT action

getSystemStart
  :: MonadLog m
  => NodeQuerier m
  -> Maybe C.SystemStart
  -> m C.SystemStart
getSystemStart nodeQuerier = \case
  Just ss -> pure ss
  Nothing -> do
    logInfo_ "Querying system start from node..."
    runQuery nodeQuerier QueryStartup >>= \case
      (ss, _, _) -> do
        logInfo "System start is" ss
        pure ss

mkFollowerThread
  :: MonadLog m
  => MonadIO m
  => MonadReader (MarloweChainFollowerDependencies m) m
  => (ChainEvent -> m ())
  -> Maybe C.SystemStart
  -> m ()
mkFollowerThread emit possibleSystemStart = do
  MarloweChainFollowerDependencies{..} <- ask
  systemStart <- getSystemStart nodeQuerier possibleSystemStart
  NodeFollower.Changes{..} <- liftIO $ atomically changes
  for_ changesRollback $ \rollbackTo -> do
    let
      rollbackPoint = chainPointFromCardanoRollback rollbackTo
    logInfo "Rolling back to" rollbackPoint
    emit $ RollBackward rollbackPoint (fromCardanoNodeTip changesTip)

  (eraHistory :: C.EraHistory) <- runQuery nodeQuerier QueryHistory
  marloweUTxO <- getLatestMarloweUTxO

  blocks' <- for (reverse changesBlocks) \(C.BlockInMode _ block) -> do
    let
      blockHeader = fromCardanoBlockHeader . C.getBlockHeader $ block
    case withCardanoEraTxs block (traverse fromCardanoTransaction) of
      Right transactions -> pure (blockHeader, transactions)
      Left info -> do
        logAttention "Failed to extract transactions from block" info
        error "Failed to extract transactions from block"

  let
    ((possibleBlocks, marloweUTxO'), logs) = runWriter $ flip runStateT marloweUTxO $ for blocks' \(blockHeader, transactions) -> do
      let
        chainPoint =  At blockHeader
      possibleBlock <- extractMarloweBlock
        systemStart
        eraHistory
        marloweScriptHashes
        blockHeader
        (Set.fromList transactions)
      pure $ (chainPoint,) <$> possibleBlock

    marloweBlocks = catMaybes possibleBlocks

  for_ logs $ \l ->
    logInfo_ $ "Extract marlowe block: " <> l

  unless (null marloweBlocks) $ do
    -- FIXME: Add JSON instance
    -- logInfo "Extracted marlowe blocks" marloweBlocks
    logInfo "Updated marlowe UTxO" marloweUTxO'
    emit $ RollForward marloweBlocks (fromCardanoNodeTip changesTip)

  mkFollowerThread emit (Just systemStart)

