-- | Minimal stub
module Language.Marlowe.Runtime.Core.ScriptRegistry where

import Cardano.Api (ScriptInAnyLang)
import Language.Marlowe.Runtime.ChainSync.Api (ScriptHash, TxOutRef, TransactionOutput(..))
import Data.Aeson (ToJSON, ToJSONKey)
import GHC.Generics (Generic)

data HelperScript = OpenRoleScript
  deriving (Show, Eq, Ord, Generic, ToJSON, ToJSONKey)

data ReferenceScriptUtxo = ReferenceScriptUtxo
  { txOutRef :: TxOutRef
  , txOut :: TransactionOutput
  , script :: ScriptInAnyLang
  }
  deriving (Show, Eq, Generic, ToJSON)

data MarloweScripts = MarloweScripts
  { marloweScript :: ScriptHash
  , payoutScript :: ScriptHash
  , helperScripts :: ()
  , marloweScriptUTxOs :: ()
  , payoutScriptUTxOs :: ()
  , helperScriptUTxOs :: ()
  }
  deriving (Show, Eq, Ord)
