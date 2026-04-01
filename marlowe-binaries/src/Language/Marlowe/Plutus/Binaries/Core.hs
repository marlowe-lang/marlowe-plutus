{-# LANGUAGE NoImplicitPrelude #-}

module Language.Marlowe.Plutus.Binaries.Core
  ( applyArg
  , hashScript
  ) where

import PlutusTx (CompiledCode, liftCodeDef, unsafeApplyCode)
import PlutusTx.Blueprint.PlutusVersion (PlutusVersion(..))
import PlutusTx.Lift.Class (Lift)
import PlutusTx.Prelude (toBuiltin, (.))

import Cardano.Crypto.Hash qualified as Hash
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import PlutusCore (DefaultUni)
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusLedgerApi.Data.V1 (ScriptHash (ScriptHash))

applyArg
  :: (Lift DefaultUni params)
  => CompiledCode (params -> res)
  -> params
  -> CompiledCode res
applyArg code = unsafeApplyCode code . liftCodeDef

hashScript :: PlutusVersion -> CompiledCode fn -> ScriptHash
hashScript pv =
  ScriptHash
    . toBuiltin
    . (Hash.hashToBytes :: Hash.Hash Hash.Blake2b_224 SBS.ShortByteString -> BS.ByteString)
    . Hash.hashWith (BS.append prefix . SBS.fromShort)
    . serialiseCompiledCode
 where
  prefix = case pv of
    PlutusV1 -> "\x01"
    PlutusV2 -> "\x02"
    PlutusV3 -> "\x03"
