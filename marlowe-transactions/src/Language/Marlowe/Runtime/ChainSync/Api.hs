{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Minimal stub for ChainSync.Api - types for BuildConstraints
module Language.Marlowe.Runtime.ChainSync.Api (
  Address(..),
  AssetId(..),
  Assets(..),
  Credential(..),
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
  TxId(..),
  TxIx(..),
  TxOutAssets(..),
  TxOutRef(..),
  UTxO(..),
  UTxOs(..),
  lookupUTxO,
  mkTxOutAssets,
  paymentCredential,
  toCardanoAddressAny,
  toCardanoMetadata,
  toDatum,
  fromDatum,
  toUTxOTuple,
  toUTxOsList,
  unInterpreter,
  unsafeTxOutAssets,
  pattern TxOutAssetsContent
) where

import Cardano.Api (SlotNo, deserialiseFromRawBytes)
import qualified Cardano.Api as C
import qualified Cardano.Api as Cardano
import qualified Data.Aeson as Aeson
import Data.Aeson (ToJSON, ToJSONKey(toJSONKey))
import Data.Aeson.Types (toJSONKeyText)
import qualified Data.ByteString as BS
import Control.Monad ((>=>))
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString (ByteString)
import Data.Map (Map, toList)
import qualified Data.Map as Map
import Data.String (IsString(fromString))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeLatin1)
import Data.Word (Word64)
import Ouroboros.Consensus.HardFork.History (Summary(..))
import qualified PlutusLedgerApi.V1 as Plutus

newtype ScriptHash = ScriptHash { unScriptHash :: ByteString }
  deriving (Show, Eq, Ord)

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

newtype TxId = TxId { unTxId :: ByteString }
  deriving (Show, Eq, Ord)

newtype TxIx = TxIx { unTxIx :: Word64 }
  deriving (Show, Eq, Ord)

data TxOutRef = TxOutRef
  { txOutRefId :: TxId
  , txOutRefIdx :: TxIx
  }
  deriving (Show, Eq, Ord)

instance ToJSON TxOutRef where
  toJSON txOutRef = Aeson.String $ Text.pack $ show txOutRef

instance ToJSONKey TxOutRef where
  toJSONKey = toJSONKeyText (Text.pack . show)

data AssetId = AssetId
  { policyId :: PolicyId
  , tokenName :: TokenName
  }
  deriving (Show, Eq, Ord)

instance ToJSON AssetId where
  toJSON (AssetId p t) = Aeson.String $ Text.pack $ show (p, t)
newtype PaymentKeyHash = PaymentKeyHash { unPaymentKeyHash :: ByteString }
  deriving (Show, Eq, Ord)
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

instance ToJSON Quantity where
  toJSON (Quantity q) = Aeson.toJSON q

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

instance ToJSON TransactionOutput where
  toJSON TransactionOutput{address} = Aeson.String $ Text.pack $ show address

newtype TxOutAssets = TxOutAssets {unTxOutAssets :: Assets}
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

instance ToJSON UTxOs where
  toJSON (UTxOs utxos) = Aeson.toJSON (Map.size utxos)

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
mkTxOutAssets assets = Just $ TxOutAssets assets

unsafeTxOutAssets :: Assets -> TxOutAssets
unsafeTxOutAssets = TxOutAssets

unInterpreter :: a -> Summary xs
unInterpreter = error "stub"

toCardanoMetadata :: Metadata -> C.TxMetadataValue
toCardanoMetadata = \case
  MetadataMap ms -> C.TxMetaMap $ bimap toCardanoMetadata toCardanoMetadata <$> ms
  MetadataList ds -> C.TxMetaList $ toCardanoMetadata <$> ds
  MetadataNumber n -> C.TxMetaNumber n
  MetadataBytes bs -> C.TxMetaBytes bs
  MetadataText t -> C.TxMetaText t

lookupUTxO :: TxOutRef -> UTxOs -> Maybe TransactionOutput
lookupUTxO txOutRef (UTxOs utxos) = Map.lookup txOutRef utxos

toCardanoAddressAny :: Address -> Maybe C.AddressAny
toCardanoAddressAny = hush . deserialiseFromRawBytes C.AsAddressAny . unAddress
  where
    hush :: Either a b -> Maybe b
    hush = either (const Nothing) Just

data Credential
  = PaymentKeyCredential PaymentKeyHash
  | ScriptCredential ScriptHash
  deriving (Show, Eq, Ord)

paymentCredential :: Address -> Maybe Credential
paymentCredential =
  toCardanoAddressAny >=> \case
    Cardano.AddressShelley (Cardano.ShelleyAddress _ credential _) ->
      Just $ fromCardanoPaymentCredential $ Cardano.fromShelleyPaymentCredential credential
    _ -> Nothing

fromCardanoPaymentCredential :: Cardano.PaymentCredential -> Credential
fromCardanoPaymentCredential = \case
  Cardano.PaymentCredentialByKey pkh -> PaymentKeyCredential $ fromCardanoPaymentKeyHash pkh
  Cardano.PaymentCredentialByScript scriptHash -> ScriptCredential $ fromCardanoScriptHash scriptHash

fromCardanoPaymentKeyHash :: Cardano.Hash Cardano.PaymentKey -> PaymentKeyHash
fromCardanoPaymentKeyHash = PaymentKeyHash . Cardano.serialiseToRawBytes

fromCardanoScriptHash :: Cardano.ScriptHash -> ScriptHash
fromCardanoScriptHash = ScriptHash . Cardano.serialiseToRawBytes

toDatum :: (Plutus.ToData a) => a -> Datum
toDatum = fromPlutusData . Plutus.toData

fromDatum :: (Plutus.FromData a) => Datum -> Maybe a
fromDatum = Plutus.fromData . toPlutusData

fromPlutusData :: Plutus.Data -> Datum
fromPlutusData (Plutus.Constr i ds) = Constr i $ fromPlutusData <$> ds
fromPlutusData (Plutus.Map m) = Map $ bimap fromPlutusData fromPlutusData <$> m
fromPlutusData (Plutus.List ds) = List $ fromPlutusData <$> ds
fromPlutusData (Plutus.I i) = I i
fromPlutusData (Plutus.B b) = B b

toPlutusData :: Datum -> Plutus.Data
toPlutusData (Constr i ds) = Plutus.Constr i $ toPlutusData <$> ds
toPlutusData (Map m) = Plutus.Map $ bimap toPlutusData toPlutusData <$> m
toPlutusData (List ds) = Plutus.List $ toPlutusData <$> ds
toPlutusData (I i) = Plutus.I i
toPlutusData (B b) = Plutus.B b

instance ToJSON Address where
  toJSON = Aeson.String . decodeLatin1 . unAddress

instance ToJSONKey Address where
  toJSONKey = toJSONKeyText (decodeLatin1 . unAddress)

instance ToJSON ScriptHash where
  toJSON = Aeson.String . decodeLatin1 . unScriptHash

instance ToJSONKey ScriptHash where
  toJSONKey = toJSONKeyText (decodeLatin1 . unScriptHash)