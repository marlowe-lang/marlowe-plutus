{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}

-- | Utilities for converting to and from Cardano API types.
module Language.Marlowe.Runtime.Cardano.Api where

import Cardano.Api (getScriptData, unsafeHashableScriptData)
import qualified Cardano.Api as C
import Cardano.Api.Value (valueFromList, valueToList)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable (fold)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Debug.Trace (traceShow)
import Language.Marlowe.Runtime.Cardano.Feature
import Language.Marlowe.Runtime.ChainSync.Api

babbageEraOnwardsToMaryEraOnwards :: C.BabbageEraOnwards era -> C.MaryEraOnwards era
babbageEraOnwardsToMaryEraOnwards = \case
  C.BabbageEraOnwardsBabbage -> C.MaryEraOnwardsBabbage
  C.BabbageEraOnwardsConway -> C.MaryEraOnwardsConway
  C.BabbageEraOnwardsDijkstra -> C.MaryEraOnwardsDijkstra

babbageEraOnwardsToAlonzoEraOnwards :: C.BabbageEraOnwards era -> C.AlonzoEraOnwards era
babbageEraOnwardsToAlonzoEraOnwards = \case
  C.BabbageEraOnwardsBabbage -> C.AlonzoEraOnwardsBabbage
  C.BabbageEraOnwardsConway -> C.AlonzoEraOnwardsConway
  C.BabbageEraOnwardsDijkstra -> C.AlonzoEraOnwardsDijkstra

babbageEraOnwardsToShelleyBasedEra :: C.BabbageEraOnwards era -> C.ShelleyBasedEra era
babbageEraOnwardsToShelleyBasedEra = \case
  C.BabbageEraOnwardsBabbage -> C.ShelleyBasedEraBabbage
  C.BabbageEraOnwardsConway -> C.ShelleyBasedEraConway
  C.BabbageEraOnwardsDijkstra -> C.ShelleyBasedEraDijkstra

toCardanoScriptHash :: ScriptHash -> Maybe C.ScriptHash
toCardanoScriptHash = hush . C.deserialiseFromRawBytes C.AsScriptHash . unScriptHash

toCardanoDatumHash :: DatumHash -> Maybe (C.Hash C.ScriptData)
toCardanoDatumHash = hush . C.deserialiseFromRawBytes (C.AsHash C.AsScriptData) . unDatumHash

fromCardanoDatumHash :: C.Hash C.ScriptData -> DatumHash
fromCardanoDatumHash = DatumHash . C.serialiseToRawBytes

toCardanoScriptData :: Datum -> C.ScriptData
toCardanoScriptData = \case
  Constr i ds -> C.ScriptDataConstructor i $ toCardanoScriptData <$> ds
  Map ds -> C.ScriptDataMap $ bimap toCardanoScriptData toCardanoScriptData <$> ds
  List ds -> C.ScriptDataList $ toCardanoScriptData <$> ds
  I i -> C.ScriptDataNumber i
  B bs -> C.ScriptDataBytes bs

fromCardanoScriptData :: C.ScriptData -> Datum
fromCardanoScriptData = \case
  C.ScriptDataConstructor i ds -> Constr i $ fromCardanoScriptData <$> ds
  C.ScriptDataMap ds -> Map $ bimap fromCardanoScriptData fromCardanoScriptData <$> ds
  C.ScriptDataList ds -> List $ fromCardanoScriptData <$> ds
  C.ScriptDataNumber i -> I i
  C.ScriptDataBytes bs -> B bs

toCardanoPaymentKeyHash :: PaymentKeyHash -> Maybe (C.Hash C.PaymentKey)
toCardanoPaymentKeyHash = hush . C.deserialiseFromRawBytes (C.AsHash C.AsPaymentKey) . unPaymentKeyHash

toCardanoPolicyId :: PolicyId -> Maybe C.PolicyId
toCardanoPolicyId = hush . C.deserialiseFromRawBytes C.AsPolicyId . unPolicyId

fromCardanoPolicyId :: C.PolicyId -> PolicyId
fromCardanoPolicyId = PolicyId . C.serialiseToRawBytes

toCardanoAssetName :: TokenName -> C.AssetName
toCardanoAssetName = C.UnsafeAssetName . unTokenName

fromCardanoAssetName :: C.AssetName -> TokenName
fromCardanoAssetName (C.UnsafeAssetName name) = TokenName name

toCardanoQuantity :: Quantity -> C.Quantity
toCardanoQuantity = C.Quantity . unQuantity

fromCardanoQuantity :: C.Quantity -> Quantity
fromCardanoQuantity (C.Quantity q) = Quantity q

toCardanoLovelace :: Lovelace -> C.Coin
toCardanoLovelace = C.Coin . unLovelace

fromCardanoLovelace :: C.Coin -> Lovelace
fromCardanoLovelace (C.Coin l) = Lovelace l

tokensToCardanoValue :: Tokens -> Maybe C.Value
tokensToCardanoValue = fmap valueFromList . traverse toCardanoValue' . Map.toList . unTokens
  where
    toCardanoValue' :: (AssetId, Quantity) -> Maybe (C.AssetId, C.Quantity)
    toCardanoValue' (AssetId policyId tokenName, quantity) =
      (,)
        <$> (C.AssetId <$> toCardanoPolicyId policyId <*> pure (toCardanoAssetName tokenName))
        <*> pure (toCardanoQuantity quantity)

tokensFromCardanoValue :: C.Value -> Tokens
tokensFromCardanoValue = Tokens . Map.fromList . mapMaybe fromCardanoValue' . valueToList
  where
    fromCardanoValue' :: (C.AssetId, C.Quantity) -> Maybe (AssetId, Quantity)
    fromCardanoValue' (C.AdaAssetId, _) = Nothing
    fromCardanoValue' (C.AssetId policy name, q) =
      Just (AssetId (fromCardanoPolicyId policy) (fromCardanoAssetName name), fromCardanoQuantity q)

assetsToCardanoValue :: TxOutAssets -> Maybe C.Value
assetsToCardanoValue (TxOutAssets (Assets ada tokens)) =
  fold
    <$> sequence
      [ Just $ valueFromList [(C.AdaAssetId, C.lovelaceToQuantity $ toCardanoLovelace ada)]
      , tokensToCardanoValue tokens
      ]

assetsFromCardanoValue :: C.Value -> Assets
assetsFromCardanoValue value =
  Assets
    { ada = fromCardanoLovelace $ C.selectLovelace value
    , tokens = tokensFromCardanoValue value
    }

toCardanoTxOutValue :: C.MaryEraOnwards era -> TxOutAssets -> Maybe (C.TxOutValue era)
toCardanoTxOutValue C.MaryEraOnwardsMary assets =
  C.TxOutValueShelleyBased C.ShelleyBasedEraMary . C.toLedgerValue C.MaryEraOnwardsMary <$> assetsToCardanoValue assets
toCardanoTxOutValue C.MaryEraOnwardsAlonzo assets =
  C.TxOutValueShelleyBased C.ShelleyBasedEraAlonzo . C.toLedgerValue C.MaryEraOnwardsAlonzo
    <$> assetsToCardanoValue assets
toCardanoTxOutValue C.MaryEraOnwardsBabbage assets =
  C.TxOutValueShelleyBased C.ShelleyBasedEraBabbage . C.toLedgerValue C.MaryEraOnwardsBabbage
    <$> assetsToCardanoValue assets
toCardanoTxOutValue C.MaryEraOnwardsConway assets =
  C.TxOutValueShelleyBased C.ShelleyBasedEraConway . C.toLedgerValue C.MaryEraOnwardsConway
    <$> assetsToCardanoValue assets
toCardanoTxOutValue C.MaryEraOnwardsDijkstra assets =
  C.TxOutValueShelleyBased C.ShelleyBasedEraDijkstra . C.toLedgerValue C.MaryEraOnwardsDijkstra
    <$> assetsToCardanoValue assets

fromCardanoTxOutValue :: C.TxOutValue era -> Assets
fromCardanoTxOutValue = \case
  C.TxOutValueByron value -> assetsFromCardanoValue $ C.lovelaceToValue value
  C.TxOutValueShelleyBased era value -> assetsFromCardanoValue $ C.fromLedgerValue era value

toCardanoTxOutDatum
  :: C.CardanoEra era
  -> Maybe DatumHash
  -> Maybe Datum
  -> Maybe (C.TxOutDatum C.CtxTx era)
toCardanoTxOutDatum =
  curry . C.inEonForEra (const $ Just C.TxOutDatumNone) \scriptDataSupported -> \case
    (Nothing, Nothing) -> Just C.TxOutDatumNone
    (Just hash, Nothing) -> C.TxOutDatumHash scriptDataSupported <$> toCardanoDatumHash hash
    (_, Just datum) -> Just $ C.TxOutSupplementalDatum scriptDataSupported $ unsafeHashableScriptData $ toCardanoScriptData datum

toCardanoTxOutDatum'
  :: C.CardanoEra era
  -> Maybe DatumHash
  -> Maybe (C.TxOutDatum ctx era)
toCardanoTxOutDatum' = C.inEonForEra (const $ Just C.TxOutDatumNone) \scriptDataSupported -> \case
  Nothing -> Just C.TxOutDatumNone
  Just hash -> C.TxOutDatumHash scriptDataSupported <$> toCardanoDatumHash hash

fromCardanoTxOutDatum :: C.TxOutDatum C.CtxTx era -> (Maybe DatumHash, Maybe Datum)
fromCardanoTxOutDatum = \case
  C.TxOutDatumNone -> (Nothing, Nothing)
  C.TxOutDatumHash _ hash -> (Just $ fromCardanoDatumHash hash, Nothing)
  C.TxOutDatumInline _ datum -> (Nothing, Just $ fromCardanoScriptData $ getScriptData datum)
  C.TxOutSupplementalDatum _ datum -> (Nothing, Just $ fromCardanoScriptData $ getScriptData datum)

toCardanoTxOut :: C.MaryEraOnwards era -> TransactionOutput -> Maybe (C.TxOut C.CtxTx era)
toCardanoTxOut era TransactionOutput{..} = do
  printIfNone $
    C.TxOut
      <$> toCardanoAddressInEra (C.toCardanoEra era) address
      <*> toCardanoTxOutValue era assets
      <*> toCardanoTxOutDatum (C.toCardanoEra era) datumHash datum
      <*> pure C.ReferenceScriptNone
  where
    printIfNone = \case
      Nothing -> traceShow TransactionOutput{..} Nothing
      Just a -> Just a

toCardanoTxOut'
  :: C.MaryEraOnwards era
  -> TransactionOutput
  -> Maybe C.ScriptInAnyLang
  -> Maybe (C.TxOut ctx era)
toCardanoTxOut' era TransactionOutput{..} mScript = do
  ref <- case (mScript, era) of
    (Nothing, _) -> Just C.ReferenceScriptNone
    (_, C.MaryEraOnwardsMary) -> Nothing
    (_, C.MaryEraOnwardsAlonzo) -> Nothing
    (Just script, C.MaryEraOnwardsBabbage) -> Just $ C.ReferenceScript C.BabbageEraOnwardsBabbage script
    (Just script, C.MaryEraOnwardsConway) -> Just $ C.ReferenceScript C.BabbageEraOnwardsConway script
    (Just script, C.MaryEraOnwardsDijkstra) -> Just $ C.ReferenceScript C.BabbageEraOnwardsDijkstra script
  C.TxOut
    <$> toCardanoAddressInEra (C.toCardanoEra era) address
    <*> toCardanoTxOutValue era assets
    <*> toCardanoTxOutDatum' (C.toCardanoEra era) datumHash
    <*> pure ref

fromCardanoTxOut
  :: C.CardanoEra era
  -> C.TxOut C.CtxTx era
  -> Maybe TransactionOutput
fromCardanoTxOut era (C.TxOut address value txOutDatum _) = do
  txOutAssets <- mkTxOutAssets (fromCardanoTxOutValue value)
  pure $
    TransactionOutput
      (fromCardanoAddressInEra era address)
      txOutAssets
      hash
      datum
  where
    (hash, datum) = fromCardanoTxOutDatum txOutDatum

cardanoEraToAsType :: C.CardanoEra era -> C.AsType era
cardanoEraToAsType = \case
  C.ByronEra -> C.AsByronEra
  C.ShelleyEra -> C.AsShelleyEra
  C.AllegraEra -> C.AsAllegraEra
  C.MaryEra -> C.AsMaryEra
  C.AlonzoEra -> C.AsAlonzoEra
  C.BabbageEra -> C.AsBabbageEra
  C.ConwayEra -> C.AsConwayEra
  C.DijkstraEra -> C.AsDijkstraEra

toCardanoAddressAny :: Address -> Maybe C.AddressAny
toCardanoAddressAny = hush . C.deserialiseFromRawBytes C.AsAddressAny . unAddress

fromCardanoAddressAny :: C.AddressAny -> Address
fromCardanoAddressAny = Address . C.serialiseToRawBytes

toCardanoAddressInEra :: C.CardanoEra era -> Address -> Maybe (C.AddressInEra era)
toCardanoAddressInEra era =
  withCardanoEra era $
    hush . C.deserialiseFromRawBytes (C.AsAddressInEra $ cardanoEraToAsType era) . unAddress

fromCardanoAddressInEra :: C.CardanoEra era -> C.AddressInEra era -> Address
fromCardanoAddressInEra era =
  withCardanoEra era $
    Address . C.serialiseToRawBytes

toCardanoTxIn :: TxOutRef -> Maybe C.TxIn
toCardanoTxIn (TxOutRef txId txIx) = C.TxIn <$> toCardanoTxId txId <*> pure (toCardanoTxIx txIx)

fromCardanoTxIn :: C.TxIn -> TxOutRef
fromCardanoTxIn (C.TxIn txId txIx) = TxOutRef (fromCardanoTxId txId) (fromCardanoTxIx txIx)

toCardanoTxId :: TxId -> Maybe C.TxId
toCardanoTxId = hush . C.deserialiseFromRawBytes C.AsTxId . unTxId

fromCardanoTxId :: C.TxId -> TxId
fromCardanoTxId = TxId . C.serialiseToRawBytes

toCardanoTxIx :: TxIx -> C.TxIx
toCardanoTxIx = C.TxIx . fromIntegral . unTxIx

fromCardanoTxIx :: C.TxIx -> TxIx
fromCardanoTxIx (C.TxIx txIx) = TxIx $ fromIntegral txIx

toCardanoSlotNo :: SlotNo -> C.SlotNo
toCardanoSlotNo = id

fromCardanoSlotNo :: C.SlotNo -> SlotNo
fromCardanoSlotNo = id
