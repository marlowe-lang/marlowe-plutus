{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.Indexer.MarloweBlock where

import Data.Aeson (ToJSON)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Set (Set)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.ChainSync.Api
    ( BlockHeader,
      TxId,
      TxOutRef(..)
    )
import Language.Marlowe.Runtime.Core.Api (ContractId (..))
import Language.Marlowe.Runtime.History.Api (
  MarloweApplyInputsTransaction (..),
  MarloweCreateTransaction (..),
  MarloweWithdrawTransaction (..),
  UnspentContractOutput (..), ExtractMarloweTransactionError, ExtractCreationError,
 )

data MarloweBlock = MarloweBlock
  { blockHeader :: BlockHeader
  , transactions :: NonEmpty MarloweTransaction
  }
  deriving (Eq, Show, Generic)

data MarloweTransaction
  = CreateTransaction MarloweCreateTransaction
  | ApplyInputsTransaction MarloweApplyInputsTransaction
  | WithdrawTransaction MarloweWithdrawTransaction
  | InvalidCreateTransaction ContractId ExtractCreationError
  | InvalidApplyInputsTransaction TxId (Set TxOutRef) ExtractMarloweTransactionError
  deriving (Eq, Show, Generic)

-- | The global Marlowe UTxO set
data MarloweUTxO = MarloweUTxO
  { unspentContractOutputs :: Map ContractId UnspentContractOutput
  -- ^ The UTxO set to the marlowe validators keyed by the contract ID
  , unspentPayoutOutputs :: Map ContractId (Set TxOutRef)
  -- ^ The UTxO set to the payout validators keyed by the contract Id
  }
  deriving (Eq, Show, Generic)

instance ToJSON MarloweUTxO

