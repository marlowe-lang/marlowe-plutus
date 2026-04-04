{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}

module Language.Marlowe.Runtime.ChainSync.Api where

import Data.Binary (Binary (..))
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.String (IsString)
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)

data BlockHeader = BlockHeader
  { slotNo :: SlotNo
  , headerHash :: BlockHeaderHash
  , blockNo :: BlockNo
  }
  deriving (Show, Eq, Ord, Generic)

newtype SlotNo = SlotNo {unSlotNo :: Word64}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Integral, Real, Enum, Binary)

newtype BlockNo = BlockNo {unBlockNo :: Word64}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Integral, Real, Enum, Binary)

newtype BlockHeaderHash = BlockHeaderHash {unBlockHeaderHash :: ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, IsString, Show)

data WithGenesis a = Genesis | At a
  deriving (Show, Eq, Ord, Generic, Functor)

type ChainPoint = WithGenesis BlockHeader

newtype TxId = TxId {unTxId :: ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, IsString, Show)

newtype TxIx = TxIx {unTxIx :: Word16}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Binary, Bounded, Enum)

data TxOutRef = TxOutRef
  { txId :: TxId
  , txIx :: TxIx
  }
  deriving (Show, Eq, Ord, Generic)

instance Binary TxOutRef where
  put (TxOutRef txId' txIx') = put txId' *> put txIx'
  get = TxOutRef <$> get <*> get

newtype Address = Address {unAddress :: ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, IsString, Show)

newtype ScriptHash = ScriptHash {unScriptHash :: ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, IsString, Show)

newtype TokenName = TokenName {unTokenName :: ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, Show)

newtype Quantity = Quantity {unQuantity :: Integer}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Binary)

newtype Lovelace = Lovelace {unLovelace :: Integer}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Binary)

newtype PolicyId = PolicyId {unPolicyId :: ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, IsString, Show)

data AssetId = AssetId
  { policyId :: PolicyId
  , tokenName :: TokenName
  }
  deriving (Show, Eq, Ord, Generic)

newtype Tokens = Tokens {unTokens :: Map AssetId Quantity}
  deriving (Show, Eq, Generic)

newtype TxOutAssets = TxOutAssets {unTxOutAssets :: Assets}
  deriving (Show, Eq, Generic)

data Assets = Assets
  { ada :: Lovelace
  , tokens :: Tokens
  }
  deriving (Show, Eq, Generic)
