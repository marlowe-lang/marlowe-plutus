{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Marlowe.Runtime.Core.Api where

import Control.Applicative (Alternative((<|>)))
import Control.Monad ((<=<))
import Control.Monad (zipWithM)
import Data.Aeson (ToJSON(toJSON), ToJSONKey, FromJSON)
import Data.Bifunctor (first)
import Data.Either (fromRight)
import GHC.Generics (Generic)
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric.Natural (Natural)

import Marlowe.Plutus.Semantics (MarloweData(..))
import qualified Marlowe.Plutus.Semantics as PlutusSemantics
import qualified Marlowe.Plutus.Semantics.Types as V1
import qualified Language.Marlowe.Runtime.ChainSync.Api as ChainSync
import qualified PlutusLedgerApi.V1 as Plutus
import Data.Aeson.Types (toJSONKey, FromJSON (parseJSON), toJSONKeyText)
import qualified Data.Aeson as A
import Data.Time (UTCTime)

-- | The ID of a contract is the TxId and TxIx of the UTxO that first created
-- the contract.
newtype ContractId = ContractId {unContractId :: ChainSync.TxOutRef}
  deriving stock (Show, Eq, Ord, Generic)
  -- deriving newtype (IsString)
  -- deriving anyclass (Binary, Variations)

instance ToJSON ContractId where
  toJSON = A.String . renderContractId

instance ToJSONKey ContractId where
  toJSONKey = toJSONKeyText renderContractId

instance FromJSON ContractId where
  parseJSON json = do
    t <- parseJSON json
    case parseContractId t of
      Nothing -> fail $ "Invalid ContractId JSON: " <> t
      Just cid -> pure cid

parseContractId :: String -> Maybe ContractId
parseContractId = fmap ContractId . ChainSync.parseTxOutRef . T.pack

renderContractId :: ContractId -> Text
renderContractId = ChainSync.renderTxOutRef . unContractId

data MarloweVersionTag = V1

data MarloweVersion (v :: MarloweVersionTag) where
  MarloweV1 :: MarloweVersion 'V1

newtype MarloweMetadataTag = MarloweMetadataTag {getMarloweMetadataTag :: Text}
  deriving newtype (Show, Eq, Ord, IsString, ToJSON, ToJSONKey)

data MarloweMetadata = MarloweMetadata
  { tags :: Map MarloweMetadataTag (Maybe ChainSync.Metadata)
  , continuations :: Maybe Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass ToJSON

data MarloweTransactionMetadata = MarloweTransactionMetadata
  { marloweMetadata :: Maybe MarloweMetadata
  , transactionMetadata :: ChainSync.TransactionMetadata
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass ToJSON

instance Semigroup MarloweTransactionMetadata where
  a <> b = MarloweTransactionMetadata
    { marloweMetadata = marloweMetadata a <|> marloweMetadata b
    , transactionMetadata = transactionMetadata a <> transactionMetadata b
    }

instance Monoid MarloweTransactionMetadata where
  mempty = MarloweTransactionMetadata
    { marloweMetadata = Nothing
    , transactionMetadata = mempty
    }

data Transaction v = Transaction
  { transactionId :: ChainSync.TxId
  , contractId :: ContractId
  , metadata :: MarloweTransactionMetadata
  , blockHeader :: ChainSync.BlockHeader
  , validityLowerBound :: UTCTime
  , validityUpperBound :: UTCTime
  , inputs :: Inputs v
  , output :: TransactionOutput v
  }
  deriving (Generic)

deriving instance Show (Transaction 'V1)
deriving instance Eq (Transaction 'V1)
instance ToJSON (Transaction 'V1)
-- instance Variations (Transaction 'V1)

data TransactionOutput v = TransactionOutput
  { payouts :: Map ChainSync.TxOutRef (Payout v)
  , scriptOutput :: Maybe (TransactionScriptOutput v)
  }
  deriving (Generic)

deriving instance Show (TransactionOutput 'V1)
deriving instance Eq (TransactionOutput 'V1)
instance ToJSON (TransactionOutput 'V1)
-- instance Variations (TransactionOutput 'V1)

data Payout (v :: MarloweVersionTag) = Payout
  { payoutAddress :: ChainSync.Address
  , payoutAssets :: ChainSync.TxOutAssets
  , payoutDatum :: PayoutDatum v
  }
  deriving (Generic)

deriving instance Show (Payout 'V1)
deriving instance Eq (Payout 'V1)
instance ToJSON (Payout 'V1)
-- instance Variations (Payout 'V1)

data TransactionScriptOutput (v :: MarloweVersionTag) = TransactionScriptOutput
  { address :: ChainSync.Address
  , assets :: ChainSync.TxOutAssets
  , utxo :: ChainSync.TxOutRef
  , datum :: Datum v
  }
  deriving (Generic)

deriving instance Show (TransactionScriptOutput 'V1)
deriving instance Eq (TransactionScriptOutput 'V1)
instance ToJSON (TransactionScriptOutput 'V1)
-- instance Variations (TransactionScriptOutput 'V1)

class IsMarloweVersion (v :: MarloweVersionTag) where
  type Contract v :: Type
  type TransactionError v :: Type
  type Datum v :: Type
  type State v :: Type
  type Inputs (v :: MarloweVersionTag) :: Type
  type PayoutDatum v :: Type
  marloweVersion :: MarloweVersion v

instance IsMarloweVersion 'V1 where
  type Contract 'V1 = V1.Contract
  type TransactionError 'V1 = PlutusSemantics.TransactionError
  type Datum 'V1 = MarloweData
  type State 'V1 = V1.State
  type Inputs 'V1 = [V1.Input]
  type PayoutDatum 'V1 = ChainSync.AssetId
  marloweVersion = MarloweV1

fromChainPayoutDatum :: MarloweVersion v -> ChainSync.Datum -> Maybe (PayoutDatum v)
fromChainPayoutDatum = \case
  MarloweV1 -> \datum -> do
    (p, t) <- ChainSync.fromDatum datum
    let p' = ChainSync.PolicyId . Plutus.fromBuiltin . Plutus.unCurrencySymbol $ p
        t' = ChainSync.TokenName . Plutus.fromBuiltin . Plutus.unTokenName $ t
    pure $ ChainSync.AssetId p' t'

toChainPayoutDatum :: MarloweVersion v -> PayoutDatum v -> ChainSync.Datum
toChainPayoutDatum = \case
  MarloweV1 -> \case
    ChainSync.AssetId policyId tokenName -> do
      let currencySymbol = Plutus.currencySymbol . ChainSync.unPolicyId $ policyId
          tokenName' = V1.TokenName . Plutus.toBuiltin . ChainSync.unTokenName $ tokenName
      ChainSync.toDatum (currencySymbol, tokenName')

withMarloweVersion :: MarloweVersion v -> a -> a
withMarloweVersion = const id

emptyMarloweTransactionMetadata :: MarloweTransactionMetadata
emptyMarloweTransactionMetadata = MarloweTransactionMetadata Nothing mempty

data MetadataDecodeError
  = ExpectedMap
  | ExpectedList
  | ExpectedNumber
  | ExpectedText
  | ExpectedChunkedText
  | ExpectedBytes
  | ErrorInMap ChainSync.Metadata MetadataDecodeError
  | ErrorInList Natural MetadataDecodeError
  | ListIndexNotFound Natural
  | InvalidValue Text
  | OneOf MetadataDecodeError MetadataDecodeError
  deriving (Eq, Ord, Show)

encodeMarloweTransactionMetadata :: MarloweTransactionMetadata -> ChainSync.TransactionMetadata
encodeMarloweTransactionMetadata MarloweTransactionMetadata{..} = case marloweMetadata of
  Nothing -> transactionMetadata
  Just m ->
    ChainSync.TransactionMetadata $
      Map.insert 1564 (encodeMarloweMetadata m) $
        ChainSync.unTransactionMetadata transactionMetadata

decodeMarloweTransactionMetadataLenient :: ChainSync.TransactionMetadata -> MarloweTransactionMetadata
decodeMarloweTransactionMetadataLenient metadata =
  fromRight (MarloweTransactionMetadata Nothing metadata) $ decodeMarloweTransactionMetadata metadata

decodeMarloweTransactionMetadata :: ChainSync.TransactionMetadata -> Either MetadataDecodeError MarloweTransactionMetadata
decodeMarloweTransactionMetadata metadata =
  MarloweTransactionMetadata
    <$> traverse decodeMarloweMetadata (Map.lookup 1564 $ ChainSync.unTransactionMetadata metadata)
    <*> pure metadata

encodeMarloweMetadata :: MarloweMetadata -> ChainSync.Metadata
encodeMarloweMetadata MarloweMetadata{..} =
  ChainSync.MetadataList $
    catMaybes
      [ Just $ ChainSync.MetadataNumber 2
      , Just $ ChainSync.MetadataList $ uncurry encodeMetadataTag <$> Map.toList tags
      , ChainSync.MetadataText <$> continuations
      ]

encodeMetadataTag :: MarloweMetadataTag -> Maybe ChainSync.Metadata -> ChainSync.Metadata
encodeMetadataTag (MarloweMetadataTag tag) = \case
  Nothing -> ChainSync.MetadataList [tagEncoded]
  Just payload -> ChainSync.MetadataList [tagEncoded, payload]
  where
    tagEncoded = case T.chunksOf 64 tag of
      [] -> ChainSync.MetadataText ""
      [x] -> ChainSync.MetadataText x
      xs -> ChainSync.MetadataList $ ChainSync.MetadataText <$> xs

decodeMarloweMetadata :: ChainSync.Metadata -> Either MetadataDecodeError MarloweMetadata
decodeMarloweMetadata m = do
  decoder <- flip (withMetadataListIx 0) m $ withMetadataNumber \case
    1 -> pure decodeMarloweMetadataV1
    2 -> pure decodeMarloweMetadataV2
    v -> Left $ InvalidValue $ T.pack $ "Unknown contract metadata version " <> show v
  decoder m

decodeMarloweMetadataV1 :: ChainSync.Metadata -> Either MetadataDecodeError MarloweMetadata
decodeMarloweMetadataV1 m =
  MarloweMetadata
    <$> withMetadataListIx 1 decodeMetadataTagsV1 m
    <*> withMetadataListIxOptional 2 (withMetadataText pure) m

decodeMarloweMetadataV2 :: ChainSync.Metadata -> Either MetadataDecodeError MarloweMetadata
decodeMarloweMetadataV2 m =
  MarloweMetadata
    <$> withMetadataListIx 1 decodeMetadataTagsV2 m
    <*> withMetadataListIxOptional 2 (withMetadataText pure) m

decodeMetadataTagsV1
  :: ChainSync.Metadata
  -> Either MetadataDecodeError (Map MarloweMetadataTag (Maybe ChainSync.Metadata))
decodeMetadataTagsV1 =
  fmap Map.fromList . withMetadataList \case
    ChainSync.MetadataList ms ->
      withMetadataTuple
        (withMetadataText $ pure . MarloweMetadataTag)
        (pure . Just)
        (ChainSync.MetadataList ms)
    ChainSync.MetadataText tag -> pure (MarloweMetadataTag tag, Nothing)
    _ -> Left $ OneOf ExpectedList ExpectedText

decodeMetadataTagsV2
  :: ChainSync.Metadata
  -> Either MetadataDecodeError (Map MarloweMetadataTag (Maybe ChainSync.Metadata))
decodeMetadataTagsV2 =
  fmap Map.fromList . withMetadataList \m ->
    (,)
      <$> withMetadataListIx 0 decodeMetadataTag m
      <*> withMetadataListIxOptional 1 pure m

decodeMetadataTag :: ChainSync.Metadata -> Either MetadataDecodeError MarloweMetadataTag
decodeMetadataTag = withMetadataChunkedText $ pure . MarloweMetadataTag

withMetadataNumber :: (Integer -> Either MetadataDecodeError a) -> ChainSync.Metadata -> Either MetadataDecodeError a
withMetadataNumber f = \case
  ChainSync.MetadataNumber i -> f i
  _ -> Left ExpectedNumber

withMetadataChunkedText :: (Text -> Either MetadataDecodeError a) -> ChainSync.Metadata -> Either MetadataDecodeError a
withMetadataChunkedText f = \case
  ChainSync.MetadataList xs -> f . T.concat =<< withMetadataList (withMetadataText pure) (ChainSync.MetadataList xs)
  ChainSync.MetadataText i -> f i
  _ -> Left ExpectedChunkedText

withMetadataText :: (Text -> Either MetadataDecodeError a) -> ChainSync.Metadata -> Either MetadataDecodeError a
withMetadataText f = \case
  ChainSync.MetadataText i -> f i
  _ -> Left ExpectedText

withMetadataList :: (ChainSync.Metadata -> Either MetadataDecodeError a) -> ChainSync.Metadata -> Either MetadataDecodeError [a]
withMetadataList f = \case
  ChainSync.MetadataList ms -> zipWithM (\i -> first (ErrorInList i) . f) [0 ..] ms
  _ -> Left ExpectedList

withMetadataListIx
  :: Natural -> (ChainSync.Metadata -> Either MetadataDecodeError a) -> ChainSync.Metadata -> Either MetadataDecodeError a
withMetadataListIx ix f = maybe (Left $ ListIndexNotFound ix) pure <=< withMetadataListIxOptional ix f

withMetadataListIxOptional
  :: Natural -> (ChainSync.Metadata -> Either MetadataDecodeError a) -> ChainSync.Metadata -> Either MetadataDecodeError (Maybe a)
withMetadataListIxOptional ix f = \case
  ChainSync.MetadataList ms -> case drop (fromIntegral ix) ms of
    [] -> pure Nothing
    m : _ -> first (ErrorInList ix) $ Just <$> f m
  _ -> Left ExpectedList

withMetadataTuple
  :: (ChainSync.Metadata -> Either MetadataDecodeError a)
  -> (ChainSync.Metadata -> Either MetadataDecodeError b)
  -> ChainSync.Metadata
  -> Either MetadataDecodeError (a, b)
withMetadataTuple f g m = (,) <$> withMetadataListIx 0 f m <*> withMetadataListIx 1 g m

toChainDatum :: MarloweVersion v -> Datum v -> ChainSync.Datum
toChainDatum = \case
  MarloweV1 -> ChainSync.toDatum

fromChainDatum :: MarloweVersion v -> ChainSync.Datum -> Maybe (Datum v)
fromChainDatum = \case
  MarloweV1 -> ChainSync.fromDatum
