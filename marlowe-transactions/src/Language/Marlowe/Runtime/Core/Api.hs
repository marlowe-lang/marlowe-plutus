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
import Data.Kind (Type)
import Marlowe.Plutus.Semantics (MarloweData(..))
import qualified Marlowe.Plutus.Semantics as PlutusSemantics
import qualified Marlowe.Plutus.Semantics.Types as V1
import Language.Marlowe.Runtime.ChainSync.Api hiding (Datum)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import qualified PlutusLedgerApi.V1 as Plutus

data MarloweVersionTag = V1

data MarloweVersion (v :: MarloweVersionTag) where
  MarloweV1 :: MarloweVersion 'V1

data MarloweTransactionMetadata = MarloweTransactionMetadata
  { marloweMetadata :: Maybe ()
  , transactionMetadata :: TransactionMetadata
  } deriving (Eq, Show)

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
fromChainPayoutDatum = error "stub"

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
emptyMarloweTransactionMetadata = mempty

encodeMarloweTransactionMetadata :: MarloweTransactionMetadata -> Maybe ()
encodeMarloweTransactionMetadata = marloweMetadata

toChainDatum :: MarloweVersion v -> Datum v -> Chain.Datum
toChainDatum = \case
  MarloweV1 -> Chain.toDatum

fromChainDatum :: MarloweVersion v -> Chain.Datum -> Maybe (Datum v)
fromChainDatum = \case
  MarloweV1 -> Chain.fromDatum