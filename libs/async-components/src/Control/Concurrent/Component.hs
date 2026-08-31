module Control.Concurrent.Component where

import Data.Text (Text)
import Control.Monad (forever, void)
import Control.Natural (type (~>))
import Log (MonadLog)
import Log.Class (logAttention_)
import Prelude hiding ((.))
import UnliftIO
import UnliftIO.Concurrent (forkIO)
import qualified Data.Text as Text

-- The core idea behind this design:
-- * Components are executed concurrently in separate threads.
-- * They communicate via STM.
-- * The initialization procedure allows to setup the required communication channels.
-- * The monadic composition of components allows to wire them up.
newtype Component m a = Component { init :: STM (Concurrently m (), a) }
  deriving (Functor)

instance MonadUnliftIO m => Applicative (Component m) where
  pure a = Component $ pure (pure (), a)
  cf <*> ca = Component $ do
    (thread1, f) <- cf.init
    (thread2, a) <- ca.init
    pure (thread1 <> thread2, f a)

instance MonadUnliftIO m => Monad (Component m) where
  c >>= k = Component $ do
    (thread1, x) <- c.init
    (thread2, b) <- (k x).init
    pure (thread1 <> thread2, b)

instance (MonadUnliftIO m, Semigroup a) => Semigroup (Component m a) where
  c1 <> c2 = Component $ do
    (thread1, a1) <- c1.init
    (thread2, a2) <- c2.init
    pure (thread1 <> thread2, a1 <> a2)

instance (MonadUnliftIO m, Monoid a) => Monoid (Component m a) where
  mempty = Component $ pure (pure (), mempty)

runComponent :: Component m a -> STM (m (), a)
runComponent c = do
  (run, a) <- c.init
  pure (runConcurrently run, a)

runComponent_ :: MonadIO m => Component m a -> m ()
runComponent_ c = do
  (run, _) <- atomically (runComponent c)
  void run

hoistComponent :: forall a m n. (m ~> n) -> Component m a -> Component n a
hoistComponent f (Component c) = Component $ do
  (run, a) <- c
  pure (Concurrently $ f $ runConcurrently run, a)

mkComponent :: (MonadLog m, MonadUnliftIO m) => Text -> STM (m (), b) -> Component m b
mkComponent name run = Component $ do
  (action, b) <- run
  let action' =
        action `catch` \(SomeException e) -> do
          logAttention_ $ "Component " <> name <> " crashed with exception:"
          logAttention_ $ Text.pack $ displayException e
          throwIO e
  pure (Concurrently action', b)

mkComponent_ :: (MonadLog m, MonadUnliftIO m) => Text -> m a -> Component m ()
mkComponent_ name thread = mkComponent name (pure (void thread, ()))

mkServerComponent
  :: forall m a
   . (MonadLog m, MonadUnliftIO m)
  => Text
  -> STM (m a)
  -> (a -> Component m ())
  -> Component m ()
mkServerComponent name accept mkWorker = mkComponent name $ forever do
  ma <- accept
  pure $ forkIO do
    a <- ma
    runComponent_ (mkWorker a)

