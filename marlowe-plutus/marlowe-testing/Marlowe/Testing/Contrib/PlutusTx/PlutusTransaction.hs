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
  datum,
  infoDCert,
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
  scriptPurpose,
  txInfo,
) where

import Control.Lens (Lens', lens)
import PlutusLedgerApi.V2 (
  DCert,
  Datum,
  DatumHash,
  Map,
  POSIXTimeRange,
  PubKeyHash,
  Redeemer,
  ScriptContext,
  ScriptPurpose,
  StakingCredential,
  TxId,
  TxInInfo,
  TxInfo,
  TxOut,
  Value,
 )
import Marlowe.Testing.Contrib.PlutusTx.Lens (
  scriptContextPurposeLens,
  scriptContextTxInfoLens,
  txInfoDCertLens,
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
  txInfoWdrlLens,
 )

-- | A Plutus transaction.
data PlutusTransaction a = PlutusTransaction
  { _parameters :: a
  -- ^ Parameters providing context.
  , _datum :: Datum
  -- ^ The datum.
  , _redeemer :: Redeemer
  -- ^ The redeemer.
  , _scriptContext :: ScriptContext
  -- ^ The script context.
  }
  deriving (Show)

parameters :: Lens' (PlutusTransaction a) a
parameters = lens _parameters $ \s x -> s{_parameters = x}

datum :: Lens' (PlutusTransaction a) Datum
datum = lens _datum $ \s x -> s{_datum = x}

redeemer :: Lens' (PlutusTransaction a) Redeemer
redeemer = lens _redeemer $ \s x -> s{_redeemer = x}

scriptContext :: Lens' (PlutusTransaction a) ScriptContext
scriptContext = lens _scriptContext $ \s x -> s{_scriptContext = x}

txInfo :: Lens' (PlutusTransaction a) TxInfo
txInfo = scriptContext . scriptContextTxInfoLens

scriptPurpose :: Lens' (PlutusTransaction a) ScriptPurpose
scriptPurpose = scriptContext . scriptContextPurposeLens

infoInputs :: Lens' (PlutusTransaction a) [TxInInfo]
infoInputs = scriptContext . scriptContextTxInfoLens . txInfoInputsLens

infoReferenceInputs :: Lens' (PlutusTransaction a) [TxInInfo]
infoReferenceInputs = scriptContext . scriptContextTxInfoLens . txInfoReferenceInputsLens

infoOutputs :: Lens' (PlutusTransaction a) [TxOut]
infoOutputs = scriptContext . scriptContextTxInfoLens . txInfoOutputsLens

infoFee :: Lens' (PlutusTransaction a) Value
infoFee = scriptContext . scriptContextTxInfoLens . txInfoFeeLens

infoMint :: Lens' (PlutusTransaction a) Value
infoMint = scriptContext . scriptContextTxInfoLens . txInfoMintLens

infoDCert :: Lens' (PlutusTransaction a) [DCert]
infoDCert = scriptContext . scriptContextTxInfoLens . txInfoDCertLens

infoWdrl :: Lens' (PlutusTransaction a) (Map StakingCredential Integer)
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

