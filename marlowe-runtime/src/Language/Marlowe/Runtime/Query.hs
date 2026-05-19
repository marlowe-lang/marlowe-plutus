module Language.Marlowe.Runtime.Query where

import Cardano.Api (NetworkId, BabbageEraOnwards)
import Control.Monad (join)
import Data.Aeson (FromJSON, ToJSON (..), Value (String), object, (.=))
import Data.Bifunctor (Bifunctor (..))
import Data.Binary (Binary (..))
import Data.Function (on)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import Data.Monoid (Any (..))
import Data.Set (Set)
import Data.Time (UTCTime)
import Data.Type.Equality (testEquality, type (:~:) (Refl))
import Data.Version (Version)
import GHC.Generics (Generic)
import GHC.Show (showSpace)
import Language.Marlowe.Runtime.ChainSync.Api (Address, AssetId, BlockHeader, ChainPoint, PolicyId, ScriptHash, TxId, TxOutRef)
import Language.Marlowe.Runtime.Core.Api (
  ContractId,
  IsMarloweVersion (..),
  MarloweMetadataTag,
  MarloweTransactionMetadata,
  MarloweVersion (..),
  MarloweVersionTag (..),
  Payout,
  SomeMarloweVersion (..),
  Transaction,
  TransactionScriptOutput,
 )
import Language.Marlowe.Runtime.Core.ScriptRegistry ()
import Data.Variations (Variations (..), varyAp)
import Data.Kind (Type)
import qualified Cardano.Api as C
import Language.Marlowe.Runtime.Transaction.Api (SubmitError)

-- | A Marlowe contract header is a compact structure that contains all the
-- significant metadata related to a Marlowe Contract on chain.
data ContractHeader = ContractHeader
  { contractId :: ContractId
  -- ^ The ID of the Marlowe contract instance.
  , rolesCurrency :: PolicyId
  -- ^ The ID of the minting policy used to mint the role tokens.
  , metadata :: MarloweTransactionMetadata
  -- ^ Any custom metadata attached to the contract's creation transaction.
  , marloweScriptHash :: ScriptHash
  -- ^ The hash of the validator script.
  , marloweScriptAddress :: Address
  -- ^ The address of the validator script.
  , payoutScriptHash :: ScriptHash
  -- ^ The address of the payout validator script.
  , marloweVersion :: SomeMarloweVersion
  -- ^ The version of the contract.
  , blockHeader :: BlockHeader
  -- ^ The header of the block in which the contract instance was published.
  }
  deriving (Show, Eq, Ord, Generic, Binary)

instance ToJSON ContractHeader
instance Variations ContractHeader

newtype WithdrawalFilter = WithdrawalFilter
  { roleCurrencies :: Set PolicyId
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, Binary, Variations)
  deriving newtype (Semigroup, Monoid)

data ContractFilter = ContractFilter
  { tags :: Set MarloweMetadataTag
  , roleCurrencies :: Set PolicyId
  , partyRoles :: Set AssetId
  , partyAddresses :: Set Address
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, Binary, Variations)

instance Semigroup ContractFilter where
  a <> b =
    ContractFilter
      { tags = on (<>) tags a b
      , roleCurrencies = on (<>) (\ContractFilter{..} -> roleCurrencies) a b
      , partyRoles = on (<>) partyRoles a b
      , partyAddresses = on (<>) partyAddresses a b
      }

instance Monoid ContractFilter where
  mempty =
    ContractFilter
      { tags = mempty
      , roleCurrencies = mempty
      , partyRoles = mempty
      , partyAddresses = mempty
      }

data PayoutFilter = PayoutFilter
  { isWithdrawn :: Maybe Bool
  , contractIds :: Set ContractId
  , roleTokens :: Set AssetId
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, Binary, Variations)

instance Semigroup PayoutFilter where
  a <> b =
    PayoutFilter
      { isWithdrawn = getAny <$> on (<>) (fmap Any . isWithdrawn) a b
      , contractIds = on (<>) contractIds a b
      , roleTokens = on (<>) roleTokens a b
      }

instance Monoid PayoutFilter where
  mempty =
    PayoutFilter
      { isWithdrawn = Nothing
      , contractIds = mempty
      , roleTokens = mempty
      }

data RuntimeStatus = RuntimeStatus
  { nodeTip :: ChainPoint
  , nodeTipUTC :: UTCTime
  , runtimeChainTip :: ChainPoint
  , runtimeChainTipUTC :: UTCTime
  , runtimeTip :: ChainPoint
  , runtimeTipUTC :: UTCTime
  , networkId :: NetworkId
  , runtimeVersion :: Version
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, Variations)

data RoleCurrency = RoleCurrency
  { rolePolicyId :: PolicyId
  , roleContract :: ContractId
  , active :: Bool
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (ToJSON)
  deriving anyclass (Binary, Variations)

data RoleCurrencyFilter
  = RoleCurrencyAnd RoleCurrencyFilter RoleCurrencyFilter
  | RoleCurrencyOr RoleCurrencyFilter RoleCurrencyFilter
  | RoleCurrencyNot RoleCurrencyFilter
  | RoleCurrencyFilterNone
  | RoleCurrencyFilterByContract (Set ContractId)
  | RoleCurrencyFilterByPolicy (Set PolicyId)
  | RoleCurrencyFilterAny
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON)

instance Variations RoleCurrencyFilter where
  variations =
    join $
      NE.fromList
        [ pure (RoleCurrencyOr RoleCurrencyFilterNone RoleCurrencyFilterNone)
        , pure (RoleCurrencyAnd RoleCurrencyFilterNone RoleCurrencyFilterNone)
        , pure (RoleCurrencyNot RoleCurrencyFilterNone)
        , pure RoleCurrencyFilterNone
        , pure RoleCurrencyFilterAny
        , RoleCurrencyFilterByContract <$> variations
        , RoleCurrencyFilterByPolicy <$> variations
        ]

data MarloweSyncRequest a where
  ReqStatus :: MarloweSyncRequest RuntimeStatus
  ReqContractHeaders :: ContractFilter -> Range ContractId -> MarloweSyncRequest (Maybe (Page ContractId ContractHeader))
  ReqContractState :: ContractId -> MarloweSyncRequest (Maybe SomeContractState)
  ReqTransaction :: TxId -> MarloweSyncRequest (Maybe SomeTransaction)
  ReqTransactions :: ContractId -> MarloweSyncRequest (Maybe SomeTransactions)
  ReqWithdrawal :: TxId -> MarloweSyncRequest (Maybe Withdrawal)
  ReqWithdrawals :: WithdrawalFilter -> Range TxId -> MarloweSyncRequest (Maybe (Page TxId Withdrawal))
  ReqPayouts :: PayoutFilter -> Range TxOutRef -> MarloweSyncRequest (Maybe (Page TxOutRef PayoutHeader))
  ReqPayout :: TxOutRef -> MarloweSyncRequest (Maybe SomePayoutState)
  ReqRoleCurrencies :: RoleCurrencyFilter -> MarloweSyncRequest (Set RoleCurrency)

deriving instance Show (MarloweSyncRequest a)
deriving instance Eq (MarloweSyncRequest a)

instance ToJSON (MarloweSyncRequest a) where
  toJSON = \case
    ReqContractHeaders cFilter range ->
      object
        [ "get-contract-headers"
            .= object
              [ "filter" .= cFilter
              , "range" .= range
              ]
        ]
    ReqContractState contractId ->
      object
        [ "get-contract-state" .= contractId
        ]
    ReqTransaction txId ->
      object
        [ "get-transaction" .= txId
        ]
    ReqTransactions contractId ->
      object
        [ "get-transactions" .= contractId
        ]
    ReqWithdrawal txId ->
      object
        [ "get-withdrawal" .= txId
        ]
    ReqWithdrawals wFilter range ->
      object
        [ "get-withdrawals"
            .= object
              [ "filter" .= wFilter
              , "range" .= range
              ]
        ]
    ReqPayouts pFilter range ->
      object
        [ "get-payouts"
            .= object
              [ "filter" .= pFilter
              , "range" .= range
              ]
        ]
    ReqPayout payoutId ->
      object
        [ "get-payouts" .= payoutId
        ]
    ReqStatus -> String "get-status"
    ReqRoleCurrencies cFilter ->
      object
        [ "get-role-currencies" .= cFilter
        ]

data Range a = Range
  { rangeStart :: Maybe a
  , rangeOffset :: Int
  , rangeLimit :: Int
  , rangeDirection :: Order
  }
  deriving stock (Eq, Show, Read, Ord, Functor, Generic, Foldable, Traversable)
  deriving anyclass (ToJSON, Binary, Variations)

data Page a b = Page
  { items :: [b]
  , nextRange :: Maybe (Range a)
  , totalCount :: Int
  }
  deriving stock (Eq, Show, Read, Ord, Functor, Generic, Foldable, Traversable)
  deriving anyclass (ToJSON, Binary, Variations)

instance Bifunctor Page where
  bimap f g Page{..} = Page{items = g <$> items, nextRange = fmap f <$> nextRange, ..}

data SomeContractState = forall v. SomeContractState (MarloweVersion v) (ContractState v)

instance Show SomeContractState where
  showsPrec p (SomeContractState MarloweV1 state) =
    showParen
      (p >= 11)
      ( showString "SomeContractState"
          . showSpace
          . showsPrec 11 MarloweV1
          . showSpace
          . showsPrec 11 state
      )

instance Eq SomeContractState where
  SomeContractState v state == SomeContractState v' state' = case testEquality v v' of
    Nothing -> False
    Just Refl -> case v of
      MarloweV1 -> state == state'

instance Binary SomeContractState where
  put (SomeContractState MarloweV1 state) = do
    put $ SomeMarloweVersion MarloweV1
    put state
  get = do
    SomeMarloweVersion MarloweV1 <- get
    SomeContractState MarloweV1 <$> get

instance Variations SomeContractState where
  variations = SomeContractState MarloweV1 <$> variations

instance ToJSON SomeContractState where
  toJSON (SomeContractState MarloweV1 state) =
    object
      [ "version" .= MarloweV1
      , "state" .= state
      ]

data SomePayoutState = forall v. SomePayoutState (MarloweVersion v) (PayoutState v)

instance Show SomePayoutState where
  showsPrec p (SomePayoutState MarloweV1 state) =
    showParen
      (p >= 11)
      ( showString "SomePayoutState"
          . showSpace
          . showsPrec 11 MarloweV1
          . showSpace
          . showsPrec 11 state
      )

instance Eq SomePayoutState where
  SomePayoutState v state == SomePayoutState v' state' = case testEquality v v' of
    Nothing -> False
    Just Refl -> case v of
      MarloweV1 -> state == state'

instance Binary SomePayoutState where
  put (SomePayoutState MarloweV1 state) = do
    put $ SomeMarloweVersion MarloweV1
    put state
  get = do
    SomeMarloweVersion MarloweV1 <- get
    SomePayoutState MarloweV1 <$> get

instance Variations SomePayoutState where
  variations = SomePayoutState MarloweV1 <$> variations

instance ToJSON SomePayoutState where
  toJSON (SomePayoutState MarloweV1 state) =
    object
      [ "version" .= MarloweV1
      , "state" .= state
      ]

data SomeTransaction = forall v.
  SomeTransaction
  { version :: MarloweVersion v
  , input :: TxOutRef
  , inputDatum :: Datum v
  , consumedBy :: Maybe TxId
  , transaction :: Transaction v
  }

instance Show SomeTransaction where
  showsPrec p (SomeTransaction MarloweV1 input inputDatum consumedBy transaction) =
    showParen
      (p >= 11)
      ( showString "SomeTransaction"
          . showSpace
          . showsPrec 11 MarloweV1
          . showSpace
          . showsPrec 11 input
          . showSpace
          . showsPrec 11 inputDatum
          . showSpace
          . showsPrec 11 consumedBy
          . showSpace
          . showsPrec 11 transaction
      )

instance Eq SomeTransaction where
  SomeTransaction v input inputDatum consumedBy tx == SomeTransaction v' input' inputDatum' consumedBy' tx' = case testEquality v v' of
    Nothing -> False
    Just Refl -> case v of
      MarloweV1 -> input == input' && inputDatum == inputDatum' && consumedBy == consumedBy' && tx == tx'

instance Binary SomeTransaction where
  put (SomeTransaction MarloweV1 input inputDatum consumedBy tx) = do
    put $ SomeMarloweVersion MarloweV1
    put input
    put inputDatum
    put consumedBy
    put tx
  get = do
    SomeMarloweVersion MarloweV1 <- get
    SomeTransaction MarloweV1 <$> get <*> get <*> get <*> get

instance Variations SomeTransaction where
  variations = SomeTransaction MarloweV1 <$> variations `varyAp` variations `varyAp` variations `varyAp` variations

instance ToJSON SomeTransaction where
  toJSON (SomeTransaction MarloweV1 input inputDatum consumedBy tx) =
    object
      [ "version" .= MarloweV1
      , "input" .= input
      , "inputDatum" .= inputDatum
      , "consumedBy" .= consumedBy
      , "transaction" .= tx
      ]

data SomeTransactions = forall v. SomeTransactions (MarloweVersion v) [Transaction v]

instance Show SomeTransactions where
  showsPrec p (SomeTransactions MarloweV1 transactions) =
    showParen
      (p >= 11)
      ( showString "SomeTransactions"
          . showSpace
          . showsPrec 11 MarloweV1
          . showSpace
          . showsPrec 11 transactions
      )

instance Eq SomeTransactions where
  SomeTransactions v txs == SomeTransactions v' txs' = case testEquality v v' of
    Nothing -> False
    Just Refl -> case v of
      MarloweV1 -> txs == txs'

instance Binary SomeTransactions where
  put (SomeTransactions MarloweV1 txs) = do
    put $ SomeMarloweVersion MarloweV1
    put txs
  get = do
    SomeMarloweVersion MarloweV1 <- get
    SomeTransactions MarloweV1 <$> get

instance Variations SomeTransactions where
  variations = SomeTransactions MarloweV1 <$> variations

instance ToJSON SomeTransactions where
  toJSON (SomeTransactions MarloweV1 txs) =
    object
      [ "version" .= MarloweV1
      , "transactions" .= txs
      ]

data ContractState v = ContractState
  { contractId :: ContractId
  , roleTokenMintingPolicyId :: PolicyId
  , metadata :: MarloweTransactionMetadata
  , initialBlock :: BlockHeader
  , initialOutput :: TransactionScriptOutput v
  , latestBlock :: BlockHeader
  , latestOutput :: Maybe (TransactionScriptOutput v)
  , unclaimedPayouts :: Map TxOutRef (Payout v)
  }
  deriving (Generic)

deriving instance Show (ContractState 'V1)
deriving instance Eq (ContractState 'V1)
deriving instance ToJSON (ContractState 'V1)
deriving instance Variations (ContractState 'V1)
deriving instance Binary (ContractState 'V1)

data PayoutHeader = PayoutHeader
  { contractId :: ContractId
  , payoutId :: TxOutRef
  , withdrawalId :: Maybe TxId
  , role :: AssetId
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, Binary, Variations)

data PayoutState v = PayoutState
  { contractId :: ContractId
  , payoutId :: TxOutRef
  , withdrawalId :: Maybe TxId
  , payout :: Payout v
  }
  deriving (Generic)

deriving instance Show (PayoutState 'V1)
deriving instance Eq (PayoutState 'V1)
deriving instance ToJSON (PayoutState 'V1)
deriving instance Variations (PayoutState 'V1)
deriving instance Binary (PayoutState 'V1)

data Withdrawal = Withdrawal
  { block :: BlockHeader
  , withdrawnPayouts :: Map TxOutRef PayoutHeader
  , withdrawalTx :: TxId
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, Binary, Variations)

data Order = Ascending | Descending
  deriving stock (Eq, Show, Read, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, Binary, Variations)

data TempTxStatus = Unsigned | Submitted

type Submit r m = r -> Submit' m

type Submit' m = forall era. C.BabbageEraOnwards era -> C.Tx era -> m (Maybe SubmitError)

data TempTx (tx :: Type -> MarloweVersionTag -> Type) where
  TempTx
    :: BabbageEraOnwards era -> MarloweVersion v -> TempTxStatus -> tx era v -> TempTx tx

