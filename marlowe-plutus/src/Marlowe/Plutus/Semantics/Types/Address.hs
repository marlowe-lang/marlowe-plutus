{-# LANGUAGE NoImplicitPrelude #-}

-- Plinth options
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

-- Recommended extensions and flags
-- https://plutus.cardano.intersectmbo.org/docs/using-plinth/extensions-flags-pragmas
-- https://plutus.cardano.intersectmbo.org/docs/auction-smart-contract/on-chain-code#4-script-context
{-# LANGUAGE Strict #-}
{-# OPTIONS -fno-full-laziness #-}
{-# OPTIONS -fno-ignore-interface-pragmas #-}
{-# OPTIONS -fno-omit-interface-pragmas #-}
{-# OPTIONS -fno-spec-constr #-}
{-# OPTIONS -fno-specialise #-}
{-# OPTIONS -fno-strictness #-}
{-# OPTIONS -fno-unbox-small-strict-fields #-}
{-# OPTIONS -fno-unbox-strict-fields #-}


module Marlowe.Plutus.Semantics.Types.Address (
  -- * Types
  Network,
  mainnet,
  testnet,

  -- * Serialisation
  deserialiseAddress,
  deserialiseAddressBech32,
  serialiseAddress,
  serialiseAddressBech32,
) where

import Codec.Binary.Bech32 (
  HumanReadablePart,
  dataPartFromBytes,
  dataPartToBytes,
  decodeLenient,
  encodeLenient,
  humanReadablePartFromText,
 )
import Control.Monad (guard)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.List (find)
import Data.Text (Text)
import PlutusLedgerApi.V2 (
  Address (Address),
  BuiltinByteString,
  Credential (PubKeyCredential, ScriptCredential),
  PubKeyHash (PubKeyHash),
  ScriptHash (..),
  StakingCredential (StakingHash, StakingPtr),
  fromBuiltin,
  toBuiltin,
 )
import PlutusTx.Builtins (consByteString, lengthOfByteString, sliceByteString)
import qualified PlutusTx.Prelude as P
import qualified Prelude as H

import qualified Data.ByteString as BS (uncons)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text.Encoding as Text (decodeUtf8)
import qualified Data.Text as Text

-- | Type of network.
type Network = P.Bool

-- | The main network.
mainnet :: Network
mainnet = P.True

-- | A test network.
testnet :: Network
testnet = P.False

byteStringToHex :: ByteString -> H.String
byteStringToHex = Text.unpack H.. Text.decodeUtf8 H.. Base16.encode

-- | The human-readable Bech32 prefix for a network.
humanReadableFromNetwork :: Network -> HumanReadablePart
humanReadableFromNetwork network
  | network H.== mainnet = H.either (H.error H.. H.show) H.id H.$ humanReadablePartFromText "addr" -- The error condition never occurs for this prefix.
  | H.otherwise = H.either (H.error H.. H.show) H.id H.$ humanReadablePartFromText "addr_test" -- The error condition never occurs for this prefix.

-- | The network corresponding to a human-readable Bech32 prefix.
humanReadableToNetwork :: HumanReadablePart -> H.Maybe Network
humanReadableToNetwork human = find ((H.== human) H.. humanReadableFromNetwork) [mainnet, testnet]

-- | Serialize a Plutus network address to CIP-19 bytes.
serialiseAddress :: Network -> Address -> ByteString
serialiseAddress network address =
  -- See CIP-19 binary format, <https://cips.cardano.org/cips/cip19/#binaryformat>.
  let header x = consByteString (x H.+ H.toInteger (H.fromEnum network)) H.mempty
      -- We are in Strict world here
      encodeInteger :: H.Integer -> BuiltinByteString
      encodeInteger _val = do
        let
          stakingCredentialBytes = \case
            H.Just (StakingHash (PubKeyCredential (PubKeyHash spkh))) -> spkh
            H.Just (StakingHash (ScriptCredential (ScriptHash svah))) -> svah
            H.Just (StakingPtr _slot _transaction _certificate) -> "<StakingPtr>"
            H.Nothing -> H.mempty
          addressBytes = \case
            Address (PubKeyCredential (PubKeyHash pkh)) s -> pkh H.<> stakingCredentialBytes s
            Address (ScriptCredential (ScriptHash vah)) s -> vah H.<> stakingCredentialBytes s
        H.error H.$ "serialiseAddress: staking pointers not supported: " H.<> byteStringToHex (fromBuiltin H.. addressBytes H.$ address)
   in fromBuiltin H.$
        case address of
          Address (PubKeyCredential (PubKeyHash pkh)) (H.Just (StakingHash (PubKeyCredential (PubKeyHash spkh)))) -> header 0x00 H.<> pkh H.<> spkh
          Address (ScriptCredential (ScriptHash vah)) (H.Just (StakingHash (PubKeyCredential (PubKeyHash spkh)))) -> header 0x10 H.<> vah H.<> spkh
          Address (PubKeyCredential (PubKeyHash pkh)) (H.Just (StakingHash (ScriptCredential (ScriptHash svah)))) -> header 0x20 H.<> pkh H.<> svah
          Address (ScriptCredential (ScriptHash vah)) (H.Just (StakingHash (ScriptCredential (ScriptHash svah)))) -> header 0x30 H.<> vah H.<> svah
          Address (PubKeyCredential (PubKeyHash pkh)) (H.Just (StakingPtr slot transaction certificate)) -> header 0x40 H.<> pkh H.<> encodeInteger slot H.<> encodeInteger transaction H.<> encodeInteger certificate
          Address (ScriptCredential (ScriptHash vah)) (H.Just (StakingPtr slot transaction certificate)) -> header 0x50 H.<> vah H.<> encodeInteger slot H.<> encodeInteger transaction H.<> encodeInteger certificate
          Address (PubKeyCredential (PubKeyHash pkh)) H.Nothing -> header 0x60 H.<> pkh
          Address (ScriptCredential (ScriptHash vah)) H.Nothing -> header 0x70 H.<> vah

-- | Deserialize a Plutus network address from CIP-19 bytes.
deserialiseAddress :: ByteString -> H.Maybe (Network, Address)
deserialiseAddress bs =
  do
    (header, payload) <- BS.uncons bs
    let payload' = toBuiltin payload
        n = lengthOfByteString payload'
        k = 28
        slice i = sliceByteString (k H.* i) k payload'
        guard1 f = guard (n H.== k) H.>> H.pure (f payload', H.Nothing)
        guard2 f g = guard (n H.== 2 H.* k) H.>> H.pure (f H.$ slice 0, H.Just H.. StakingHash H.. g H.$ slice 1)
        pkc = PubKeyCredential H.. PubKeyHash
        scc = ScriptCredential H.. ScriptHash
    network <- find ((H.fromEnum (header .&. 0x0F) H.==) H.. H.fromEnum) [mainnet, testnet]
    let
      errorMessage = "deserialiseAddress: Staking pointers not supported: " H.<> byteStringToHex bs
    (network,)
      H.. H.uncurry Address
      H.<$> case header .&. 0xF0 of
        0x00 -> guard2 pkc pkc
        0x10 -> guard2 scc pkc
        0x20 -> guard2 pkc scc
        0x30 -> guard2 scc scc
        0x40 -> H.fail errorMessage -- TODO: Add support for staking pointers, even though these may never have been used on the ledger.
        0x50 -> H.fail errorMessage -- TODO: Add support for staking pointers, even though these may never have been used on the ledger.
        0x60 -> guard1 pkc
        0x70 -> guard1 scc
        _ -> H.fail "Invalid address header."

-- | Serialize a Plutus address to CIP-19 Bech32.
serialiseAddressBech32 :: Network -> Address -> Text
serialiseAddressBech32 network address =
  let human = humanReadableFromNetwork network
      bs = serialiseAddress network address
   in encodeLenient human H.$ dataPartFromBytes bs

-- | Deserialize a Plutus address from CIP-19 Bech32.
deserialiseAddressBech32 :: Text -> H.Maybe (Network, Address)
deserialiseAddressBech32 encoded =
  do
    (human, dataPart) <- H.either (H.fail H.. H.show) H.Just H.$ decodeLenient encoded
    network <- humanReadableToNetwork human
    bs <- dataPartToBytes dataPart
    (network', address) <- deserialiseAddress bs
    guard H.$ network H.== network'
    H.pure (network, address)
