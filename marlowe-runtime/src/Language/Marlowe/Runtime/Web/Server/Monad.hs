{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImpredicativeTypes #-}

-- | Defines a custom Monad for the web server's handler functions to run in.
module Language.Marlowe.Runtime.Web.Server.Monad (
  LoadTxError (..),
  ServerDependencies (..),
  ServerM (..),
  applyInputsL,
  burnRoleTokensL,
  createContractL,
  loadContractL,
  loadPayoutL,
  loadPayoutsL,
  loadTransactionL,
  loadTransactionsL,
  loadWithdrawalL,
  loadWithdrawalsL,
  withdrawL,
  runEff0,
  runEff1,
  runEff2,
  runEff3,
  runEff4,
  runServer,
) where

import Language.Marlowe.Runtime.ChainSync.Api (DatumHash, Lovelace, StakeCredential, TokenName, TxId, TxOutRef)
import Language.Marlowe.Runtime.Transaction.Api (Accounts, ContractCreatedInEra, WalletAddresses, RoleTokenFilter, RoleTokensConfig, BurnRoleTokensError, BurnRoleTokensTx, WithdrawTx, InitError)
import Language.Marlowe.Runtime.Web.Adapter.Control.Lens (makeSuffixedLenses)

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Catch.Pure (MonadMask)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Language.Marlowe.Runtime.Core.Api (Contract, ContractId, MarloweTransactionMetadata, Transaction, Inputs)
import Servant (Handler, ServerError)
import Language.Marlowe.Runtime.Query (SomeContractState, TempTx)
import qualified Language.Marlowe.Runtime.Query as Query
import Log (LogT, MonadLog, logInfo_, Logger, LogLevel)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Lens (Getter, view)
import Log.Monad (runLogT)
import Language.Marlowe.Runtime.Core.Api (MarloweVersionTag(V1))
import Data.Time (UTCTime)
import Language.Marlowe.Runtime.Transaction.Api (ApplyInputsError)
import Language.Marlowe.Runtime.Transaction.Api (InputsApplied)
import Language.Marlowe.Runtime.Transaction.Api (WithdrawError)
import Data.Set (Set)
import Language.Marlowe.Runtime.Transaction.Api (ContractCreated)

newtype ServerM a = ServerM {runServerM :: LogT (ReaderT (ServerDependencies ServerM) Handler) a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadLog
    , MonadIO
    , MonadReader (ServerDependencies ServerM)
    , MonadFail
    , MonadCatch
    , MonadMask
    , MonadThrow
    , MonadBaseControl IO
    , MonadError ServerError
    , MonadBase IO
    )

runServer :: Logger -> LogLevel -> ServerDependencies ServerM -> ServerM a -> Handler a
runServer logger logLevel deps server =
  flip runReaderT deps $ runLogT "marlowe-runtime-server" logger logLevel $ runServerM do
    logInfo_ "Starting Marlowe Runtime Server"
    server

runEff0
  :: (MonadTrans t)
  => (MonadReader (ServerDependencies m) m)
  => Getter (ServerDependencies m) (m a)
  -> t m a
runEff0 l = do
  fn <- lift $ view l
  lift fn

runEff1
  :: (MonadTrans t)
  => (MonadReader (ServerDependencies m) m)
  => Getter (ServerDependencies m) (a -> m b)
  -> a
  -> t m b
runEff1 l a = do
  fn <- lift $ view l
  lift $ fn a

runEff2
  :: (MonadTrans t)
  => (MonadReader (ServerDependencies m) m)
  => Getter (ServerDependencies m) (a -> b -> m c)
  -> a
  -> b
  -> t m c
runEff2 l a b = do
  fn <- lift $ view l
  lift $ fn a b

runEff3
  :: (MonadTrans t)
  => (MonadReader (ServerDependencies m) m)
  => Getter (ServerDependencies m) (a -> b -> c -> m d)
  -> a
  -> b
  -> c
  -> t m d
runEff3 l a b c = do
  fn <- lift $ view l
  lift $ fn a b c

runEff4
  :: (MonadTrans t)
  => (MonadReader (ServerDependencies m) m)
  => Getter (ServerDependencies m) (a -> b -> c -> d -> m e)
  -> a
  -> b
  -> c
  -> d
  -> t m e
runEff4 l a b c d = do
  fn <- lift $ view l
  lift $ fn a b c d

-- FIXME: To speed up the migration I hard coded `V1` below.
-- We should probably migrate the Api so it is working in most cases
-- mostly with existential wrappers.
type ApplyInputs m =
  WalletAddresses
  -> ContractId
  -> MarloweTransactionMetadata
  -> Maybe UTCTime
  -> Maybe UTCTime
  -> Inputs V1
  -> m (Either ApplyInputsError (InputsApplied V1))

type BurnRoleTokens m =
  WalletAddresses
  -> RoleTokenFilter
  -> m (Either BurnRoleTokensError (BurnRoleTokensTx V1))

type CreateContract m =
  Maybe StakeCredential
  -> WalletAddresses
  -> Maybe TokenName
  -> RoleTokensConfig
  -> MarloweTransactionMetadata
  -> Maybe Lovelace
  -> Accounts
  -> Either (Contract V1) DatumHash
  -> m (Either InitError (ContractCreated V1))

type LoadContract m =
  ContractId
  -- ^ ID of the contract to load
  -> m (Maybe (Either (TempTx ContractCreatedInEra) SomeContractState))
  -- ^ Nothing if the ID is not found

-- | Signature for a delegate that loads a list of payouts.
type LoadPayouts m =
  Query.PayoutFilter
  -> Query.Range TxOutRef
  -> m (Maybe (Query.Page TxOutRef Query.PayoutHeader))
  -- ^ Nothing if the initial ID is not found

-- | Signature for a delegate that loads a single payout.
type LoadPayout m =
  TxOutRef
  -> m (Maybe Query.SomePayoutState)

data LoadTxError
  = ContractNotFound
  | TxNotFound

type LoadTransactions m =
  ContractId
  -- ^ ID of the contract to load transactions for.
  -> Query.Range TxId
  -- ^ The range of transactions to load.
  -> m (Either LoadTxError (Query.Page TxId (Transaction 'V1)))

-- | Signature for a delegate that loads a transaction for a contract.
type LoadTransaction m =
  ContractId
  -- ^ ID of the contract to load transactions for.
  -> TxId
  -- ^ ID of the transaction to load.
  -> m (Maybe Query.SomeTransaction)

-- | Signature for a delegate that loads a list of withdrawals.
type LoadWithdrawals m =
  Query.WithdrawalFilter
  -> Query.Range TxId
  -> m (Maybe (Query.Page TxId Query.Withdrawal))
  -- ^ Nothing if the initial ID is not found

-- | Signature for a delegate that loads the state of a single withdrawal.
type LoadWithdrawal m =
  TxId
  -- ^ ID of the contract to load
  -> m (Maybe Query.Withdrawal)
  -- ^ Nothing if the ID is not found

type Withdraw m =
  WalletAddresses
  -> Set TxOutRef
  -> m (Either WithdrawError (WithdrawTx V1))

data ServerDependencies m = ServerDependencies
  { applyInputs :: ApplyInputs m
  , burnRoleTokens :: BurnRoleTokens m
  , createContract :: CreateContract m
  , loadContract :: LoadContract m
  , loadPayout :: LoadPayout m
  , loadPayouts :: LoadPayouts m
  , loadTransaction :: LoadTransaction m
  , loadTransactions :: LoadTransactions m
  , loadWithdrawal :: LoadWithdrawal m
  , loadWithdrawals :: LoadWithdrawals m
  , withdraw :: Withdraw m
  }

makeSuffixedLenses ''ServerDependencies

