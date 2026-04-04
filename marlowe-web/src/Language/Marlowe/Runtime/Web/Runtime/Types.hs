-- | Stubs for marlowe-runtime types
-- This module provides minimal type stubs for porting purposes.
-- Actual implementations will be added incrementally.
module Language.Marlowe.Runtime.Web.Runtime.Types where

import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude (Word, Show, Eq, Maybe(..), Integer, error)

-- | Stub for MarloweVersion
data MarloweVersion v = MarloweVersion
  deriving (Show, Eq, Generic)

-- | Stub for MarloweV1
marloweV1 :: MarloweVersion v
marloweV1 = MarloweVersion

-- | Stub for TransactionScriptOutput
data TransactionScriptOutput v = TransactionScriptOutput
  { txOutAddress :: Address
  , txOutAssets :: Value
  , txOutUtxo :: TxOutRef
  } deriving (Show, Eq, Generic)

-- | Stub for Payout
data Payout v = Payout
  { payoutAddr :: Address
  , payoutAssets :: Value
  } deriving (Show, Eq, Generic)

-- | Stub for Wallet
data Wallet = Wallet
  { walletAddress :: Address
  , walletUtxos :: Set TxOutRef
  } deriving (Show, Eq, Generic)

-- | Stub for Address
data Address = Address
  { addressHash :: ByteString
  , addressNetwork :: NetworkId
  } deriving (Show, Eq, Generic)

-- | Stub for NetworkId
data NetworkId = Mainnet | Testnet Word
  deriving (Show, Eq, Generic)

-- | Stub for TxOutRef
data TxOutRef = TxOutRef
  { txOutRefId :: TxId
  , txOutRefIdx :: Word
  } deriving (Show, Eq, Generic)

-- | Stub for TxId
data TxId = TxId ByteString
  deriving (Show, Eq, Generic)

-- | Stub for Value
data Value = Value
  { valueLovelace :: Integer
  , valueTokens :: Map ByteString ByteString
  } deriving (Show, Eq, Generic)

-- | Stub for ByteString
data ByteString = ByteString
  deriving (Show, Eq, Generic)

-- | Stub for Map
data Map k v = Map
  deriving (Show, Eq)

-- | Stub for Datum
data Datum v
  = MarloweDatum
  | PayoutDatum
  | OtherDatum
  deriving (Show, Eq)

-- | Stub for MarloweData (actual type from marlowe-plutus)
data MarloweData = MarloweData
  { marloweDataParams :: ()
  , marloweDataState :: ()
  , marloweDataContract :: ()
  } deriving (Show, Eq)

-- | Stub for ChainContext
data ChainContext = ChainContext
  { chainBlock :: ()
  , chainSlot :: ()
  , chainAddresses :: [Address]
  } deriving (Show, Eq)

-- | Stub for RolesCurrency
newtype RolesCurrency = RolesCurrency PolicyId
  deriving (Show, Eq)

-- | Stub for PolicyId
newtype PolicyId = PolicyId ByteString
  deriving (Show, Eq)

-- | Stub for ContractCreated
data ContractCreated v = ContractCreated
  { createdContractId :: TxOutRef
  , createdRolesCurrency :: Maybe RolesCurrency
  , createdVersion :: MarloweVersion v
  } deriving (Show, Eq)

-- | Stub for ContractState
data ContractState v = ContractState
  { stateContractId :: TxOutRef
  , stateData :: ()
  , stateContract :: ()
  , stateVersion :: MarloweVersion v
  } deriving (Show, Eq)

-- | Stub for Transaction
data Transaction = Transaction
  { txId :: TxOutRef
  , txContractId :: TxOutRef
  , txInputs :: ()
  } deriving (Show, Eq)

-- | Stub for Withdrawal
data Withdrawal = Withdrawal
  { withdrawalId :: TxOutRef
  , withdrawalRole :: Text
  } deriving (Show, Eq)

-- | Stub for PayoutStatus
data PayoutStatus = PayoutStatus
  { payoutStatusId :: TxOutRef
  , payoutStatusText :: Text
  } deriving (Show, Eq)

-- | Stub for SomeChainContext
data SomeChainContext = SomeChainContext
  deriving (Show, Eq)

-- | Stub function for toChainContext
toChainContext :: a -> SomeChainContext
toChainContext _ = SomeChainContext

-- | Stub function for toCardanoAddressInEra
toCardanoAddressInEra :: Text -> Maybe Address
toCardanoAddressInEra _ = error "stub: toCardanoAddressInEra"
