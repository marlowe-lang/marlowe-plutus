{-# LANGUAGE UndecidableInstances #-}

module Language.Marlowe.Runtime.Web.Adapter.Servant.UVerbT where

import Control.Error (ExceptT (ExceptT), runExceptT)
import Control.Exception (Exception, throw)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.RWS (MonadReader (local))
import Control.Monad.Reader.Class (MonadReader (ask))
import Control.Monad.Trans (MonadTrans (..))
import Log (MonadLog)
import Servant (HasStatus, IsMember, Union, respond)

-- Copied from the docs: https://docs.servant.dev/en/stable/cookbook/uverb/UVerb.html
newtype UVerbT xs m a = UVerbT {unUVerbT :: ExceptT (Union xs) m a}
  deriving (Functor)
  deriving newtype (Applicative, Monad, MonadTrans, MonadIO, MonadLog)

instance (MonadReader r m) => MonadReader r (UVerbT xs m) where
  ask = lift ask
  local f (UVerbT act) = UVerbT $ ExceptT $ local f $ runExceptT act

-- | Deliberately hide 'ExceptT's 'MonadError' instance to be able to use
-- underlying monad's instance.
instance (Exception e, MonadError e m) => MonadError e (UVerbT xs m) where
  throwError = lift . throw
  catchError (UVerbT act) h =
    UVerbT $
      ExceptT $
        runExceptT act `catchError` (runExceptT . unUVerbT . h)

mapUVerbError
  :: (Monad m) => (Union xs -> Union ys) -> UVerbT xs m a -> UVerbT ys m a
mapUVerbError f = UVerbT . ExceptT . fmap (either (Left . f) Right) . runExceptT . unUVerbT

-- | This combinator runs 'UVerbT'. It applies 'respond' internally, so the handler
-- may use the usual 'return'.
runUVerbT
  :: (Monad m, HasStatus x, IsMember x xs) => UVerbT xs m x -> m (Union xs)
runUVerbT (UVerbT act) = either id id <$> runExceptT (act >>= respond)

-- | Short-circuit 'UVerbT' computation returning one of the response types.
throwUVerb
  :: (Monad m) => (HasStatus x) => (IsMember x xs) => x -> UVerbT xs m a
throwUVerb = UVerbT . ExceptT . fmap Left . respond

exceptUVerb
  :: (Monad m)
  => (HasStatus x)
  => (IsMember x xs)
  => Either x a
  -> UVerbT xs m a
exceptUVerb = \case
  Left err -> throwUVerb err
  Right a -> pure a
