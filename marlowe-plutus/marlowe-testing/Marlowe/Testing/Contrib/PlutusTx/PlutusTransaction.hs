-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--

-----------------------------------------------------------------------------
-- | Types for tests of Marlowe's Plutus implementation.
module Marlowe.Testing.Contrib.PlutusTx.PlutusTransaction (
  -- * Types
  PlutusTransaction (..),

  -- * Lenses
  datumGetter,
  infoData,
  infoFee,
  infoId,
  infoInputs,
  infoMint,
  infoOutputs,
  infoRedeemers,
  infoReferenceInputs,
  infoSignatories,
  infoValidRange,
  infoWdrl,
  parameters,
  redeemer,
  scriptContext,
  scriptInfo,
  txInfo,
) where

import Control.Lens (Lens', lens, Getter)
import PlutusLedgerApi.V2 (Lovelace, Credential)
import PlutusLedgerApi.V3 (
  Datum,
  DatumHash,
  Map,
  POSIXTimeRange,
  PubKeyHash,
  Redeemer,
  ScriptContext,
  ScriptPurpose,
  TxId,
  TxInInfo,
  TxInfo,
  TxOut,
  ScriptInfo, MintValue,
 )
import Marlowe.Testing.Contrib.PlutusTx.Lens (
  scriptContextInfoLens,
  scriptContextRedeemerLens,
  scriptContextTxInfoLens,
  txInfoDataLens,
  txInfoFeeLens,
  txInfoIdLens,
  txInfoInputsLens,
  txInfoMintLens,
  txInfoOutputsLens,
  txInfoRedeemersLens,
  txInfoReferenceInputsLens,
  txInfoSignatoriesLens,
  txInfoValidRangeLens,
  txInfoWdrlLens, scriptInfoDatumGetter,
 )
import qualified Data.Aeson as A
import Marlowe.Testing.Contrib.PlutusTx.Debugging.Aeson (scriptContextToJSON)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import qualified Data.Yaml as Y

-- | A Plutus transaction.
data PlutusTransaction a = PlutusTransaction
  { _parameters :: a
  -- ^ Parameters providing context.
  , _scriptContext :: ScriptContext
  -- ^ The script context.
  }

plutusTransactionToJSON
  :: A.ToJSON a
  => PlutusTransaction a
  -> A.Value
plutusTransactionToJSON PlutusTransaction{..} = do
  A.object
    [ "parameters" A..= A.toJSON _parameters
    , "scriptContext" A..= scriptContextToJSON _scriptContext
    ]

instance A.ToJSON a => Show (PlutusTransaction a) where
  show = T.unpack . decodeUtf8 .  Y.encode . plutusTransactionToJSON

parameters :: Lens' (PlutusTransaction a) a
parameters = lens _parameters $ \s x -> s{_parameters = x}

datumGetter :: Getter (PlutusTransaction a) (Maybe Datum)
datumGetter = scriptContext . scriptContextInfoLens . scriptInfoDatumGetter

scriptContextLens :: Lens' (PlutusTransaction a) ScriptContext
scriptContextLens = lens _scriptContext $ \s x -> s{_scriptContext = x}

redeemer :: Lens' (PlutusTransaction a) Redeemer
redeemer = scriptContextLens . scriptContextRedeemerLens

scriptContext :: Lens' (PlutusTransaction a) ScriptContext
scriptContext = lens _scriptContext $ \s x -> s{_scriptContext = x}

txInfo :: Lens' (PlutusTransaction a) TxInfo
txInfo = scriptContext . scriptContextTxInfoLens

scriptInfo :: Lens' (PlutusTransaction a) ScriptInfo
scriptInfo = scriptContext . scriptContextInfoLens

infoInputs :: Lens' (PlutusTransaction a) [TxInInfo]
infoInputs = scriptContext . scriptContextTxInfoLens . txInfoInputsLens

infoReferenceInputs :: Lens' (PlutusTransaction a) [TxInInfo]
infoReferenceInputs = scriptContext . scriptContextTxInfoLens . txInfoReferenceInputsLens

infoOutputs :: Lens' (PlutusTransaction a) [TxOut]
infoOutputs = scriptContext . scriptContextTxInfoLens . txInfoOutputsLens

infoFee :: Lens' (PlutusTransaction a) Lovelace
infoFee = scriptContext . scriptContextTxInfoLens . txInfoFeeLens

infoMint :: Lens' (PlutusTransaction a) MintValue
infoMint = scriptContext . scriptContextTxInfoLens . txInfoMintLens

infoWdrl :: Lens' (PlutusTransaction a) (Map Credential Lovelace)
infoWdrl = scriptContext . scriptContextTxInfoLens . txInfoWdrlLens

infoValidRange :: Lens' (PlutusTransaction a) POSIXTimeRange
infoValidRange = scriptContext . scriptContextTxInfoLens . txInfoValidRangeLens

infoSignatories :: Lens' (PlutusTransaction a) [PubKeyHash]
infoSignatories = scriptContext . scriptContextTxInfoLens . txInfoSignatoriesLens

infoRedeemers :: Lens' (PlutusTransaction a) (Map ScriptPurpose Redeemer)
infoRedeemers = scriptContext . scriptContextTxInfoLens . txInfoRedeemersLens

infoData :: Lens' (PlutusTransaction a) (Map DatumHash Datum)
infoData = scriptContext . scriptContextTxInfoLens . txInfoDataLens

infoId :: Lens' (PlutusTransaction a) TxId
infoId = scriptContext . scriptContextTxInfoLens . txInfoIdLens

