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
import Data.Aeson (ToJSON(toJSON), ToJSONKey)
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
import Language.Marlowe.Runtime.ChainSync.Api hiding (Datum)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import qualified PlutusLedgerApi.V1 as Plutus

data MarloweVersionTag = V1

data MarloweVersion (v :: MarloweVersionTag) where
  MarloweV1 :: MarloweVersion 'V1

newtype MarloweMetadataTag = MarloweMetadataTag {getMarloweMetadataTag :: Text}
  deriving newtype (Show, Eq, Ord, IsString, ToJSON, ToJSONKey)

data MarloweMetadata = MarloweMetadata
  { tags :: Map MarloweMetadataTag (Maybe Chain.Metadata)
  , continuations :: Maybe Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass ToJSON

data MarloweTransactionMetadata = MarloweTransactionMetadata
  { marloweMetadata :: Maybe MarloweMetadata
  , transactionMetadata :: Chain.TransactionMetadata
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

data Payout (v :: MarloweVersionTag) = Payout
  { payoutAddress :: Address
  , payoutAssets :: TxOutAssets
  , payoutDatum :: PayoutDatum v
  }

data TransactionScriptOutput (v :: MarloweVersionTag) = TransactionScriptOutput
  { address :: Address
  , assets :: TxOutAssets
  , utxo :: TxOutRef
  , datum :: Datum v
  }

instance ToJSON (TransactionScriptOutput v) where
  toJSON TransactionScriptOutput{address} = toJSON address

instance Show (TransactionScriptOutput 'V1) where
  show TransactionScriptOutput{..} = unwords
    [ "TransactionScriptOutput"
    , show address
    , show assets
    , show utxo
    , show datum
    ]

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
  type PayoutDatum 'V1 = AssetId
  marloweVersion = MarloweV1

fromChainPayoutDatum :: MarloweVersion v -> Chain.Datum -> Maybe (PayoutDatum v)
fromChainPayoutDatum = \case
  MarloweV1 -> \datum -> do
    (p, t) <- Chain.fromDatum datum
    let p' = PolicyId . Plutus.fromBuiltin . Plutus.unCurrencySymbol $ p
        t' = TokenName . Plutus.fromBuiltin . Plutus.unTokenName $ t
    pure $ AssetId p' t'

toChainPayoutDatum :: MarloweVersion v -> PayoutDatum v -> Chain.Datum
toChainPayoutDatum = \case
  MarloweV1 -> \case
    Chain.AssetId policyId tokenName -> do
      let currencySymbol = Plutus.currencySymbol . unPolicyId $ policyId
          tokenName' = V1.TokenName . Plutus.toBuiltin . Chain.unTokenName $ tokenName
      Chain.toDatum (currencySymbol, tokenName')

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
  | ErrorInMap Chain.Metadata MetadataDecodeError
  | ErrorInList Natural MetadataDecodeError
  | ListIndexNotFound Natural
  | InvalidValue Text
  | OneOf MetadataDecodeError MetadataDecodeError
  deriving (Eq, Ord, Show)

encodeMarloweTransactionMetadata :: MarloweTransactionMetadata -> Chain.TransactionMetadata
encodeMarloweTransactionMetadata MarloweTransactionMetadata{..} = case marloweMetadata of
  Nothing -> transactionMetadata
  Just m ->
    Chain.TransactionMetadata $
      Map.insert 1564 (encodeMarloweMetadata m) $
        Chain.unTransactionMetadata transactionMetadata

decodeMarloweTransactionMetadataLenient :: Chain.TransactionMetadata -> MarloweTransactionMetadata
decodeMarloweTransactionMetadataLenient metadata =
  fromRight (MarloweTransactionMetadata Nothing metadata) $ decodeMarloweTransactionMetadata metadata

decodeMarloweTransactionMetadata :: Chain.TransactionMetadata -> Either MetadataDecodeError MarloweTransactionMetadata
decodeMarloweTransactionMetadata metadata =
  MarloweTransactionMetadata
    <$> traverse decodeMarloweMetadata (Map.lookup 1564 $ Chain.unTransactionMetadata metadata)
    <*> pure metadata

encodeMarloweMetadata :: MarloweMetadata -> Chain.Metadata
encodeMarloweMetadata MarloweMetadata{..} =
  Chain.MetadataList $
    catMaybes
      [ Just $ Chain.MetadataNumber 2
      , Just $ Chain.MetadataList $ uncurry encodeMetadataTag <$> Map.toList tags
      , Chain.MetadataText <$> continuations
      ]

encodeMetadataTag :: MarloweMetadataTag -> Maybe Chain.Metadata -> Chain.Metadata
encodeMetadataTag (MarloweMetadataTag tag) = \case
  Nothing -> Chain.MetadataList [tagEncoded]
  Just payload -> Chain.MetadataList [tagEncoded, payload]
  where
    tagEncoded = case T.chunksOf 64 tag of
      [] -> Chain.MetadataText ""
      [x] -> Chain.MetadataText x
      xs -> Chain.MetadataList $ Chain.MetadataText <$> xs

decodeMarloweMetadata :: Chain.Metadata -> Either MetadataDecodeError MarloweMetadata
decodeMarloweMetadata m = do
  decoder <- flip (withMetadataListIx 0) m $ withMetadataNumber \case
    1 -> pure decodeMarloweMetadataV1
    2 -> pure decodeMarloweMetadataV2
    v -> Left $ InvalidValue $ T.pack $ "Unknown contract metadata version " <> show v
  decoder m

decodeMarloweMetadataV1 :: Chain.Metadata -> Either MetadataDecodeError MarloweMetadata
decodeMarloweMetadataV1 m =
  MarloweMetadata
    <$> withMetadataListIx 1 decodeMetadataTagsV1 m
    <*> withMetadataListIxOptional 2 (withMetadataText pure) m

decodeMarloweMetadataV2 :: Chain.Metadata -> Either MetadataDecodeError MarloweMetadata
decodeMarloweMetadataV2 m =
  MarloweMetadata
    <$> withMetadataListIx 1 decodeMetadataTagsV2 m
    <*> withMetadataListIxOptional 2 (withMetadataText pure) m

decodeMetadataTagsV1
  :: Chain.Metadata
  -> Either MetadataDecodeError (Map MarloweMetadataTag (Maybe Chain.Metadata))
decodeMetadataTagsV1 =
  fmap Map.fromList . withMetadataList \case
    Chain.MetadataList ms ->
      withMetadataTuple
        (withMetadataText $ pure . MarloweMetadataTag)
        (pure . Just)
        (Chain.MetadataList ms)
    Chain.MetadataText tag -> pure (MarloweMetadataTag tag, Nothing)
    _ -> Left $ OneOf ExpectedList ExpectedText

decodeMetadataTagsV2
  :: Chain.Metadata
  -> Either MetadataDecodeError (Map MarloweMetadataTag (Maybe Chain.Metadata))
decodeMetadataTagsV2 =
  fmap Map.fromList . withMetadataList \m ->
    (,)
      <$> withMetadataListIx 0 decodeMetadataTag m
      <*> withMetadataListIxOptional 1 pure m

decodeMetadataTag :: Chain.Metadata -> Either MetadataDecodeError MarloweMetadataTag
decodeMetadataTag = withMetadataChunkedText $ pure . MarloweMetadataTag

withMetadataNumber :: (Integer -> Either MetadataDecodeError a) -> Chain.Metadata -> Either MetadataDecodeError a
withMetadataNumber f = \case
  Chain.MetadataNumber i -> f i
  _ -> Left ExpectedNumber

withMetadataChunkedText :: (Text -> Either MetadataDecodeError a) -> Chain.Metadata -> Either MetadataDecodeError a
withMetadataChunkedText f = \case
  Chain.MetadataList xs -> f . T.concat =<< withMetadataList (withMetadataText pure) (Chain.MetadataList xs)
  Chain.MetadataText i -> f i
  _ -> Left ExpectedChunkedText

withMetadataText :: (Text -> Either MetadataDecodeError a) -> Chain.Metadata -> Either MetadataDecodeError a
withMetadataText f = \case
  Chain.MetadataText i -> f i
  _ -> Left ExpectedText

withMetadataList :: (Chain.Metadata -> Either MetadataDecodeError a) -> Chain.Metadata -> Either MetadataDecodeError [a]
withMetadataList f = \case
  Chain.MetadataList ms -> zipWithM (\i -> first (ErrorInList i) . f) [0 ..] ms
  _ -> Left ExpectedList

withMetadataListIx
  :: Natural -> (Chain.Metadata -> Either MetadataDecodeError a) -> Chain.Metadata -> Either MetadataDecodeError a
withMetadataListIx ix f = maybe (Left $ ListIndexNotFound ix) pure <=< withMetadataListIxOptional ix f

withMetadataListIxOptional
  :: Natural -> (Chain.Metadata -> Either MetadataDecodeError a) -> Chain.Metadata -> Either MetadataDecodeError (Maybe a)
withMetadataListIxOptional ix f = \case
  Chain.MetadataList ms -> case drop (fromIntegral ix) ms of
    [] -> pure Nothing
    m : _ -> first (ErrorInList ix) $ Just <$> f m
  _ -> Left ExpectedList

withMetadataTuple
  :: (Chain.Metadata -> Either MetadataDecodeError a)
  -> (Chain.Metadata -> Either MetadataDecodeError b)
  -> Chain.Metadata
  -> Either MetadataDecodeError (a, b)
withMetadataTuple f g m = (,) <$> withMetadataListIx 0 f m <*> withMetadataListIx 1 g m

toChainDatum :: MarloweVersion v -> Datum v -> Chain.Datum
toChainDatum = \case
  MarloweV1 -> Chain.toDatum

fromChainDatum :: MarloweVersion v -> Chain.Datum -> Maybe (Datum v)
fromChainDatum = \case
  MarloweV1 -> Chain.fromDatum
