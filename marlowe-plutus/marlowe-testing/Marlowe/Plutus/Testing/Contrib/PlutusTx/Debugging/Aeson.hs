{-# LANGUAGE CPP #-}

module Marlowe.Plutus.Testing.Contrib.PlutusTx.Debugging.Aeson
  ( builtinDataToJSON
  , scriptContextToJSON
  , valueToJSON
  ) where

import Data.Aeson (Value(..), object, (.=), ToJSON (toJSON))
import qualified Data.Text as T

import PlutusLedgerApi.V3
  ( ScriptContext(..)
  , ScriptInfo(..)
  , TxInfo(..)
  , TxInInfo(..)
  , TxOut(..)
  , Address(..)
  , Credential(..)
  , OutputDatum(..)
  , Redeemer(..)
  , Datum(..)
  , TxId(..), BuiltinData (BuiltinData), CurrencySymbol (CurrencySymbol), fromBuiltin, TokenName (TokenName), StakingCredential (StakingHash, StakingPtr), FromData (fromBuiltinData)
  )

import qualified PlutusLedgerApi.V1.Value as V

import qualified PlutusCore.Data as PLC
import Data.ByteString.Base16.Aeson (EncodeBase16(EncodeBase16))
import qualified PlutusTx.AssocMap as PlutusAM
import Data.Functor ((<&>))
import Data.Bifunctor (second)
import qualified Data.Aeson.Key as AK
import Marlowe.Plutus.Semantics (MarloweData)
import Marlowe.Plutus.Scripts (MarloweTxInput(..))

scriptContextToJSON :: ScriptContext -> Value
scriptContextToJSON ScriptContext{..} =
  object
    [ "txInfo" .= txInfoToJSON scriptContextTxInfo
    , "scriptInfo" .= scriptInfoToJSON scriptContextScriptInfo
    , "redeemer" .= redeemerToJSON scriptContextRedeemer
    ]

txInfoToJSON :: TxInfo -> Value
txInfoToJSON TxInfo{..} =
  object
    [ "inputs" .= fmap txInInfoToJSON txInfoInputs
    , "outputs" .= fmap txOutToJSON txInfoOutputs
    , "signatories" .= fmap show txInfoSignatories
    , "validRange" .= show txInfoValidRange
    , "redeemers" .= assocMapToJSON (toJSON . show) redeemerToJSON txInfoRedeemers
    , "data" .= assocMapToJSON (toJSON . show) datumToJSON txInfoData
    , "txId" .= txIdToJSON txInfoId
    ]

txInInfoToJSON :: TxInInfo -> Value
txInInfoToJSON TxInInfo{..} =
  object
    [ "outRef" .= show txInInfoOutRef
    , "resolved" .= txOutToJSON txInInfoResolved
    ]

txOutToJSON :: TxOut -> Value
txOutToJSON TxOut{..} =
  object
    [ "address" .= addressToJSON txOutAddress
    , "value" .= valueToJSON txOutValue
    , "datum" .= outputDatumToJSON txOutDatum
    ]

addressToJSON :: Address -> Value
addressToJSON (Address cred staking) =
  toJSON
    [ credentialToJSON cred
    , maybe Null stakingCredentialToJSON staking
    ]

stakingCredentialToJSON :: StakingCredential -> Value
stakingCredentialToJSON (StakingHash cred) = credentialToJSON cred
stakingCredentialToJSON (StakingPtr slot idx cert) =
  object
    [ "stakingPtr" .= object
        [ "slot" .= slot
        , "idx" .= idx
        , "cert" .= cert
        ]
    ]

credentialToJSON :: Credential -> Value
credentialToJSON (PubKeyCredential pkh) =
  object ["pubKeyHash" .= show pkh]
credentialToJSON (ScriptCredential vh) =
  object ["scriptHash" .= show vh]

scriptInfoToJSON :: ScriptInfo -> Value
scriptInfoToJSON si =
  case si of
    CertifyingScript idx _cert ->
      object
        [ "CertifyingScript" .= idx ]
    MintingScript (CurrencySymbol cs) ->
      object
        [ "MintingScript" .= EncodeBase16 (fromBuiltin cs) ]
    ProposingScript idx _procedure ->
      object
        [ "ProposingScript" .= idx ]
    RewardingScript cred ->
      object
        [ "RewardingScript" .= credentialToJSON cred ]
    SpendingScript ref mDatum ->
      object
        [ "SpendingScript" .= object
            [ "outRef" .= show ref
            , "datum" .= maybe Null datumToJSON mDatum
            ]
        ]
    VotingScript voter ->
      object
        [ "VotingScript" .= show voter ]

marloweInputToJSON :: MarloweTxInput -> Value
marloweInputToJSON (Input inputContent) =
  object ["Input" .= toJSON inputContent]
marloweInputToJSON (MerkleizedTxInput inputContent hash) =
  object
    [ "MerkleizedTxInput" .= object
        [ "inputContent" .= toJSON inputContent
        , "hash" .= EncodeBase16 (fromBuiltin hash)
        ]
    ]
redeemerToJSON :: Redeemer -> Value
redeemerToJSON (Redeemer d) = do
  let
    -- Try to decode the redeemer as MarloweData
    marloweInputs = fromBuiltinData d :: Maybe [MarloweTxInput]
  case marloweInputs of
    Just inputs -> toJSON $ fmap marloweInputToJSON inputs
    _ -> object ["redeemerData" .= builtinDataToJSON d]

-- Let's try to decode the datum as MarloweData if it fails let's print the raw datum
datumToJSON :: Datum -> Value
datumToJSON (Datum d) = do
  let
    -- Try to decode the datum as MarloweData
    marloweData = fromBuiltinData d :: Maybe MarloweData
  case marloweData of
    Just md -> toJSON md
    Nothing -> builtinDataToJSON d

outputDatumToJSON :: OutputDatum -> Value
outputDatumToJSON NoOutputDatum = String "NoOutputDatum"
outputDatumToJSON (OutputDatumHash h) =
  object ["datumHash" .= show h]
outputDatumToJSON (OutputDatum d) =
  datumToJSON d

txIdToJSON :: TxId -> Value
txIdToJSON (TxId bs) = String (T.pack (show bs))

assocMapToJSON :: (key -> Value) -> (value -> Value) -> PlutusAM.Map key value -> Value
assocMapToJSON encodeKey encodeValue m = toJSON $
 fmap (\(k, v) -> [encodeKey k, encodeValue v]) $ PlutusAM.toList m

currencySymbolToJSON :: CurrencySymbol -> Value
currencySymbolToJSON (CurrencySymbol bs) = toJSON $ EncodeBase16 (fromBuiltin bs)

tokenNameToJSON :: TokenName -> Value
tokenNameToJSON (TokenName bs) = toJSON $ EncodeBase16 (fromBuiltin bs)

valueToJSON :: V.Value -> Value
valueToJSON = do
  let
    flatten value = do
      let
        step (curr@(CurrencySymbol currBS), tokens) = do
          tokens <&> \(token@(TokenName tokBS), amt) ->
            if currBS == "" && tokBS == ""
              then Number (fromIntegral amt)
              else do
                let
                  currJSON = currencySymbolToJSON curr
                  tokenNameJSON = tokenNameToJSON token
                toJSON (currJSON, tokenNameJSON, amt)
      concatMap (step . second PlutusAM.toList) (PlutusAM.toList . V.getValue $ value)
  toJSON . flatten

builtinDataToJSON :: BuiltinData -> Value
builtinDataToJSON (BuiltinData d) = plcDataToJSON d

plcDataToJSON :: PLC.Data -> Value
plcDataToJSON = do
  let
    mapEntryToJSON :: (PLC.Data, PLC.Data) -> Value
    mapEntryToJSON (k, v) =
      toJSON
        [ plcDataToJSON k
        , plcDataToJSON v
        ]
    encodeFields [field] = toJSON field
    encodeFields fields = toJSON fields
  \case
    PLC.Constr i xs ->
      object
        [ AK.fromString (show i) .= encodeFields (fmap plcDataToJSON xs) ]
    PLC.Map kvs -> case length kvs of
      0 -> object []
      _ -> object
        [ "Map" .= fmap mapEntryToJSON kvs
        ]
    PLC.List xs ->
      toJSON (fmap plcDataToJSON xs)
    PLC.I i ->
      toJSON i
    PLC.B bs ->
      toJSON (EncodeBase16 bs)

