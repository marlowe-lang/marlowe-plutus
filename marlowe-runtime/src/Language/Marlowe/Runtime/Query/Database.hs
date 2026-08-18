module Language.Marlowe.Runtime.Query.Database where

import Data.Aeson (ToJSON)
import Data.Set (Set)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, TxId, TxOutRef, IndexerTip, MarloweTip, NodeTip)
import Language.Marlowe.Runtime.Core.Api (ContractId, SomeMarloweVersion)
import Language.Marlowe.Runtime.History.Api (SomeCreateStep)
import Language.Marlowe.Runtime.Query (ContractFilter, ContractHeader, Page, PayoutFilter, PayoutHeader, Range, RoleCurrency, RoleCurrencyFilter, SomeContractState, SomePayoutState, SomeTransaction, SomeTransactions, Withdrawal, WithdrawalFilter,)
import Log (logTrace, MonadLog)
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import Data.ByteString.Base16.Aeson (EncodeBase16(EncodeBase16))

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

logDatabaseQueries :: MonadLog m => DatabaseQueries m -> DatabaseQueries m
logDatabaseQueries DatabaseQueries{..} =
  DatabaseQueries
    { getTipForContract = \contractId -> do
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
    , getIndexerTip = do
        result <- getIndexerTip
        logTrace "GetIndexerTip" result
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
    , getMarloweTip = do
        result <- getMarloweTip
        logTrace "GetMarloweTip" result
        pure result
    , getNodeTip = do
        result <- getNodeTip
        logTrace "GetNodeTip" result
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
    { getTipForContract = f . getTipForContract
    , getCreateStep = f . getCreateStep
    , getIndexerTip = f getIndexerTip
    , getIntersectionForContract = fmap f . getIntersectionForContract
    , getIntersection = f . getIntersection
    , getHeaders = fmap f . getHeaders
    , getContractState = f . getContractState
    , getMarloweTip = f getMarloweTip
    , getNodeTip = f getNodeTip
    , getTransaction = f . getTransaction
    , getTransactions = f . getTransactions
    , getWithdrawal = f . getWithdrawal
    , getWithdrawals = fmap f . getWithdrawals
    , getPayouts = fmap f . getPayouts
    , getPayout = f . getPayout
    , getRoleCurrencies = f . getRoleCurrencies
    }

data GetIndexerTipError
  = MissingIndexerTip
  | InvalidIndexerTip ByteString
  deriving (Show)

instance A.ToJSON GetIndexerTipError where
  toJSON = \case
    MissingIndexerTip -> A.object [ "type" A..= ("MissingIndexerTip" :: String) ]
    InvalidIndexerTip tipBytes -> A.object
      [ "type" A..= ("InvalidIndexerTip" :: String)
      , "tipBytes" A..= EncodeBase16 tipBytes
      ]

data GetNodeTipError
  = MissingNodeTip
  | InvalidNodeTip ByteString
  deriving (Show)

instance A.ToJSON GetNodeTipError where
  toJSON = \case
    MissingNodeTip -> A.object [ "type" A..= ("MissingNodeTip" :: String) ]
    InvalidNodeTip tipBytes -> A.object
      [ "type" A..= ("InvalidNodeTip" :: String)
      , "tipBytes" A..= EncodeBase16 tipBytes
      ]


data DatabaseQueries m = DatabaseQueries
  { getTipForContract :: ContractId -> m ChainPoint
  , getCreateStep :: ContractId -> m (Maybe (BlockHeader, SomeCreateStep))
  , getIndexerTip :: m (Either GetIndexerTipError IndexerTip)
  , getIntersection :: [BlockHeader] -> m (Maybe BlockHeader)
  , getIntersectionForContract :: ContractId -> [BlockHeader] -> m (Maybe (BlockHeader, SomeMarloweVersion))
  , getHeaders :: ContractFilter -> Range ContractId -> m (Maybe (Page ContractId ContractHeader))
  , getContractState :: ContractId -> m (Maybe SomeContractState)
  , getMarloweTip :: m (Maybe MarloweTip)
  , getNodeTip :: m (Either GetNodeTipError NodeTip)
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
