module Language.Marlowe.Runtime.Query.Database where

import Data.Aeson (ToJSON)
import Data.Set (Set)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Query (
  ContractFilter,
  ContractHeader,
  Page,
  PayoutFilter,
  PayoutHeader,
  Range,
  RoleCurrency,
  RoleCurrencyFilter,
  SomeContractState,
  SomePayoutState,
  SomeTransaction,
  SomeTransactions,
  Withdrawal,
  WithdrawalFilter,
 )
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, TxId, TxOutRef)
import Language.Marlowe.Runtime.Core.Api (ContractId, SomeMarloweVersion)
import Language.Marlowe.Runtime.History.Api
    ( MarloweBlock, SomeCreateStep, SomeContractStep )
import Log (logTrace, MonadLog)

data GetPayoutsArguments = GetPayoutsArguments
  { filter :: PayoutFilter
  , range :: Range TxOutRef
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data GetWithdrawalsArguments = GetWithdrawalsArguments
  { filter :: WithdrawalFilter
  , range :: Range TxId
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data GetHeadersArguments = GetHeadersArguments
  { filter :: ContractFilter
  , range :: Range ContractId
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data GetCreateStepResult = GetCreateStepResult
  { block :: BlockHeader
  , createStep :: SomeCreateStep
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data GetIntersectionForContractArguments = GetIntersectionForContractArguments
  { contractId :: ContractId
  , points :: [BlockHeader]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data GetIntersectionForContractResult = GetIntersectionForContractResult
  { block :: BlockHeader
  , version :: SomeMarloweVersion
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data GetNextStepsArguments v = GetNextStepsArguments
  { contractId :: ContractId
  , fromPoint :: ChainPoint
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

logDatabaseQueries :: MonadLog m => DatabaseQueries m -> DatabaseQueries m
logDatabaseQueries DatabaseQueries{..} =
  DatabaseQueries
    { getTip = do
        let
          ev = "GetTip"
        result <- getTip
        logTrace ev result
        pure result
    , getTipForContract = \contractId -> do
        let
          ev = "GetTipForContract"
        logTrace ev contractId
        result <- getTipForContract contractId
        logTrace ev result
        pure result
    , getCreateStep = \contractId -> do
        let
          ev = "GetCreateStep"
        logTrace ev contractId
        result <- getCreateStep contractId
        logTrace ev result
        pure result
    , getIntersection = \points -> do
        let
          ev = "GetIntersection"
        logTrace ev points
        result <- getIntersection points
        logTrace ev result
        pure result
    , getIntersectionForContract = \contractId points -> do
        let
          ev = "GetIntersectionForContract"
        logTrace ev GetIntersectionForContractArguments{..}
        result <- getIntersectionForContract contractId points
        logTrace ev result
        pure result
    , getNextHeaders = \fromPoint -> do
        let
          ev = "GetNextHeaders"
        logTrace ev fromPoint
        result <- getNextHeaders fromPoint
        logTrace ev result
        pure result
    , getNextBlocks = \batchSize fromPoint -> do
        let
          ev = "GetNextBlocks"
        logTrace ev (batchSize, fromPoint)
        result <- getNextBlocks batchSize fromPoint
        logTrace ev result
        pure result
    , getNextSteps = \contractId fromPoint -> do
        let
          ev = "GetNextSteps"
        logTrace ev GetNextStepsArguments{..}
        result <- getNextSteps contractId fromPoint
        logTrace ev result
        pure result
    , getHeaders = \cFilter range -> do
        let
          ev = "GetHeaders"
        logTrace ev $ GetHeadersArguments cFilter range
        result <- getHeaders cFilter range
        logTrace ev result
        pure result
    , getContractState = \contractId -> do
        let
          ev = "GetContractState"
        logTrace ev contractId
        result <- getContractState contractId
        logTrace ev result
        pure result
    , getTransaction = \txId -> do
        let
          ev = "GetTransaction"
        logTrace ev txId
        result <- getTransaction txId
        logTrace ev result
        pure result
    , getTransactions = \contractId -> do
        let
          ev = "GetTransactions"
        logTrace ev contractId
        result <- getTransactions contractId
        logTrace ev result
        pure result
    , getWithdrawal = \txId -> do
        let
          ev = "GetWithdrawal"
        logTrace ev txId
        result <- getWithdrawal txId
        logTrace ev result
        pure result
    , getWithdrawals = \wFilter range -> do
        let
          ev = "GetWithdrawals"
        logTrace ev $ GetWithdrawalsArguments wFilter range
        result <- getWithdrawals wFilter range
        logTrace ev result
        pure result
    , getPayouts = \pFilter range -> do
        let
          ev = "GetPayouts"
        logTrace ev $ GetPayoutsArguments pFilter range
        result <- getPayouts pFilter range
        logTrace ev result
        pure result
    , getPayout = \payoutId -> do
        let
          ev = "GetPayout"
        logTrace ev payoutId
        result <- getPayout payoutId
        logTrace ev result
        pure result
    , getRoleCurrencies = \cFilter -> do
        let
          ev = "GetRoleCurrencies"
        logTrace ev cFilter
        result <- getRoleCurrencies cFilter
        logTrace ev result
        pure result
    }

hoistDatabaseQueries :: (forall x. m x -> n x) -> DatabaseQueries m -> DatabaseQueries n
hoistDatabaseQueries f DatabaseQueries{..} =
  DatabaseQueries
    { getTip = f getTip
    , getTipForContract = f . getTipForContract
    , getCreateStep = f . getCreateStep
    , getIntersectionForContract = fmap f . getIntersectionForContract
    , getIntersection = f . getIntersection
    , getNextHeaders = f . getNextHeaders
    , getNextBlocks = fmap f . getNextBlocks
    , getNextSteps = fmap f . getNextSteps
    , getHeaders = fmap f . getHeaders
    , getContractState = f . getContractState
    , getTransaction = f . getTransaction
    , getTransactions = f . getTransactions
    , getWithdrawal = f . getWithdrawal
    , getWithdrawals = fmap f . getWithdrawals
    , getPayouts = fmap f . getPayouts
    , getPayout = f . getPayout
    , getRoleCurrencies = f . getRoleCurrencies
    }

data DatabaseQueries m = DatabaseQueries
  { getTip :: m ChainPoint
  , getTipForContract :: ContractId -> m ChainPoint
  , getCreateStep :: ContractId -> m (Maybe (BlockHeader, SomeCreateStep))
  , getIntersection :: [BlockHeader] -> m (Maybe BlockHeader)
  , getIntersectionForContract :: ContractId -> [BlockHeader] -> m (Maybe (BlockHeader, SomeMarloweVersion))
  , getNextHeaders :: ChainPoint -> m (Next ContractHeader)
  , getNextBlocks :: Word8 -> ChainPoint -> m (Next MarloweBlock)
  , getNextSteps :: ContractId -> ChainPoint -> m (Next SomeContractStep)
  , getHeaders :: ContractFilter -> Range ContractId -> m (Maybe (Page ContractId ContractHeader))
  , getContractState :: ContractId -> m (Maybe SomeContractState)
  , getTransaction :: TxId -> m (Maybe SomeTransaction)
  , getTransactions :: ContractId -> m (Maybe SomeTransactions)
  , getWithdrawal :: TxId -> m (Maybe Withdrawal)
  , getWithdrawals :: WithdrawalFilter -> Range TxId -> m (Maybe (Page TxId Withdrawal))
  , getPayouts :: PayoutFilter -> Range TxOutRef -> m (Maybe (Page TxOutRef PayoutHeader))
  , getPayout :: TxOutRef -> m (Maybe SomePayoutState)
  , getRoleCurrencies :: RoleCurrencyFilter -> m (Set RoleCurrency)
  }

data Next a
  = Rollback ChainPoint ChainPoint
  | Wait
  | Next BlockHeader BlockHeader [a]
  deriving stock (Generic, Functor)
  deriving anyclass (ToJSON)
