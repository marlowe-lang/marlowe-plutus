{-# LANGUAGE DuplicateRecordFields #-}

module Language.Marlowe.Runtime.Indexer.Types (
  module Language.Marlowe.Runtime.History.Api
, MarloweBlock (..)
, MarloweTransaction (..)
, MarloweUTxO (..)
) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Set (Set)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.ChainSync.Api
import Language.Marlowe.Runtime.Core.Api (ContractId (..))
import Language.Marlowe.Runtime.History.Api hiding (UnspentContractOutput)
import Language.Marlowe.Runtime.History.Api (UnspentContractOutput(..))

data MarloweBlock = MarloweBlock
  { blockHeader :: BlockHeader
  , transactions :: NonEmpty MarloweTransaction
  }
  deriving (Eq, Show, Generic)

data MarloweTransaction
  = CreateTransaction MarloweCreateTransaction
  | ApplyInputsTransaction MarloweApplyInputsTransaction
  | WithdrawTransaction MarloweWithdrawTransaction
  | InvalidCreateTransaction ContractId
  | InvalidApplyInputsTransaction TxId (Set TxOutRef) ()
  deriving (Eq, Show, Generic)

data MarloweUTxO = MarloweUTxO
  { unspentContractOutputs :: Map ContractId UnspentContractOutput
  , unspentPayoutOutputs :: Map ContractId (Set TxOutRef)
  }
  deriving (Eq, Show, Generic)
