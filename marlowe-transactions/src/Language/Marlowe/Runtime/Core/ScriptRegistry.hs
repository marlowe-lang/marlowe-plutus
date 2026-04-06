-- | Minimal stub
module Language.Marlowe.Runtime.Core.ScriptRegistry where

import Language.Marlowe.Runtime.ChainSync.Api (ScriptHash, TxOutRef)
import Data.Aeson (ToJSON(toJSON), Value(String))
import qualified Data.Text as Text

data HelperScript = OpenRoleScript
  deriving (Show, Eq, Ord)

instance ToJSON HelperScript where
  toJSON OpenRoleScript = toJSON ("OpenRoleScript" :: String)

data ReferenceScriptUtxo = ReferenceScriptUtxo
  { txOutRef :: TxOutRef
  , referenceScriptUTxO :: ()
  , script :: ()
  }
  deriving (Show, Eq, Ord)

instance ToJSON ReferenceScriptUtxo where
  toJSON ReferenceScriptUtxo{txOutRef} = String $ Text.pack $ show txOutRef

data MarloweScripts = MarloweScripts
  { marloweScript :: ScriptHash
  , payoutScript :: ScriptHash
  , helperScripts :: ()
  , marloweScriptUTxOs :: ()
  , payoutScriptUTxOs :: ()
  , helperScriptUTxOs :: ()
  }
  deriving (Show, Eq, Ord)