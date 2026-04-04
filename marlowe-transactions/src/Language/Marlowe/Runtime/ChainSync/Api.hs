-- | Minimal stub for ChainSync.Api - types for BuildConstraints
module Language.Marlowe.Runtime.ChainSync.Api (
  Address(..),
  AssetId(..),
  Assets(..),
  Datum(..),
  DatumHash(..),
  Lovelace(..),
  Metadata(..),
  PaymentKeyHash(..),
  PlutusScript(..),
  PolicyId(..),
  Quantity(..),
  ScriptHash(..),
  SlotNo,
  TokenName(..),
  Tokens(..),
  TransactionMetadata(..),
  TransactionOutput(..),
  TxOutAssets(..),
  TxOutRef,
  UTxO(..),
  UTxOs(..),
  mkTxOutAssets,
  toUTxOTuple,
  toUTxOsList,
  unInterpreter,
  unsafeTxOutAssets,
  pattern TxOutAssetsContent
) where

import Cardano.Api (SlotNo, TxIn)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map (Map, toList)
import qualified Data.Map as Map
import Data.String (IsString(fromString))
import Data.Text (Text)
import Data.Word (Word64)
import Ouroboros.Consensus.HardFork.History (Summary(..))

newtype ScriptHash = ScriptHash { unScriptHash :: ByteString }
  deriving (Show, Eq)

newtype DatumHash = DatumHash { unDatumHash :: ByteString }
  deriving (Show, Eq)

newtype Address = Address { unAddress :: ByteString }
  deriving (Show, Eq, Ord)

newtype PlutusScript = PlutusScript { unPlutusScript :: ByteString }
  deriving (Show, Eq)

data Datum
  = Constr Integer [Datum]
  | Map [(Datum, Datum)]
  | List [Datum]
  | I Integer
  | B ByteString
  deriving (Show, Eq)

type TxOutRef = TxIn
data AssetId = AssetId PolicyId TokenName
  deriving (Show, Eq, Ord)
newtype PaymentKeyHash = PaymentKeyHash { unPaymentKeyHash :: ByteString }
  deriving (Show, Eq)
newtype PolicyId = PolicyId { unPolicyId :: ByteString }
  deriving (Show, Eq, Ord)

instance IsString PolicyId where
  fromString = PolicyId . BS.pack . map (toEnum . fromEnum)
newtype TokenName = TokenName { unTokenName :: ByteString }
  deriving (Show, Eq, Ord)

newtype Lovelace = Lovelace { unLovelace :: Integer }
  deriving (Show, Eq, Ord)
newtype Tokens = Tokens { unTokens :: Map AssetId Quantity }
  deriving (Show, Eq)
newtype Quantity = Quantity { unQuantity :: Integer }
  deriving (Show, Eq, Ord)

instance Semigroup Quantity where
  Quantity q1 <> Quantity q2 = Quantity $ q1 + q2

instance Monoid Quantity where
  mempty = Quantity 0

instance Semigroup Tokens where
  Tokens t1 <> Tokens t2 = Tokens $ Map.unionWith (<>) t1 t2

instance Monoid Tokens where
  mempty = Tokens mempty

instance Semigroup Lovelace where
  Lovelace a <> Lovelace b = Lovelace $ a + b

instance Monoid Lovelace where
  mempty = Lovelace 0

instance Ord Tokens where
  compare (Tokens t1) (Tokens t2) = compare (Map.toList t1) (Map.toList t2)

data Assets = Assets
  { ada :: Lovelace
  , tokens :: Tokens
  }
  deriving (Show, Eq)

data TransactionOutput = TransactionOutput
  { address :: Address
  , assets :: TxOutAssets
  , datumHash :: Maybe DatumHash
  , datum :: Maybe Datum
  }
  deriving (Show, Eq)

newtype TxOutAssets = TxOutAssets Assets
  deriving (Show, Eq)

instance Semigroup TxOutAssets where
  TxOutAssets a <> TxOutAssets b = TxOutAssets $ Assets
    { ada = ada a <> ada b
    , tokens = tokens a <> tokens b
    }

instance Monoid TxOutAssets where
  mempty = TxOutAssets $ Assets mempty mempty

data UTxO = UTxO
  { txOutRef :: TxOutRef
  , transactionOutput :: TransactionOutput
  }
  deriving (Show, Eq)

newtype UTxOs = UTxOs { unUTxOs :: Map TxOutRef TransactionOutput }
  deriving (Show, Eq)

data Metadata
  = MetadataMap [(Metadata, Metadata)]
  | MetadataList [Metadata]
  | MetadataNumber Integer
  | MetadataBytes ByteString
  | MetadataText Text
  deriving (Show, Eq)

newtype TransactionMetadata = TransactionMetadata { unTransactionMetadata :: Map Word64 Metadata }
  deriving (Show, Eq)

instance Semigroup TransactionMetadata where
  TransactionMetadata a <> TransactionMetadata b = TransactionMetadata $ Map.union a b

instance Monoid TransactionMetadata where
  mempty = TransactionMetadata mempty

pattern TxOutAssetsContent :: Assets -> TxOutAssets
pattern TxOutAssetsContent assets = TxOutAssets assets

toUTxOTuple :: UTxO -> [(TxOutRef, TransactionOutput)]
toUTxOTuple (UTxO ref out) = [(ref, out)]

toUTxOsList :: UTxOs -> [UTxO]
toUTxOsList (UTxOs m) = map (uncurry UTxO) (toList m)

mkTxOutAssets :: Assets -> Maybe TxOutAssets
mkTxOutAssets = error "stub"

unsafeTxOutAssets :: Assets -> TxOutAssets
unsafeTxOutAssets = TxOutAssets

unInterpreter :: a -> Summary xs
unInterpreter = error "stub"