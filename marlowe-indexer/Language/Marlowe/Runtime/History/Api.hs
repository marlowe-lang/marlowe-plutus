{-# LANGUAGE DuplicateRecordFields #-}

module Language.Marlowe.Runtime.History.Api where

import Data.Binary (Binary)
import Data.Map (Map)
import Data.Set (Set)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.ChainSync.Api
import Language.Marlowe.Runtime.Core.Api

data UnspentContractOutput = UnspentContractOutput
  { marloweVersion :: SomeMarloweVersion
  , txOutRef :: TxOutRef
  , marloweAddress :: Address
  , payoutValidatorHash :: ScriptHash
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (Binary)

data MarloweCreateTransaction = MarloweCreateTransaction
  { txId :: TxId
  }
  deriving (Show, Eq, Generic)

data MarloweApplyInputsTransaction = MarloweApplyInputsTransaction
  { marloweInput :: UnspentContractOutput
  }
  deriving (Show, Eq, Generic)

data MarloweWithdrawTransaction = MarloweWithdrawTransaction
  { consumedPayouts :: Map ContractId (Set TxOutRef)
  , consumingTx :: TxId
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Binary)
