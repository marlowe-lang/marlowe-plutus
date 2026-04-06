{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-redundant-constraints #-}

-- | Minimal stub
module Language.Marlowe.Runtime.Transaction.Constraints (
  ConstraintError(..),
  TxConstraints (..),
  WalletContext (..),
  PayoutContext (..),
  MarloweInputConstraints(..),
  MarloweOutputConstraints(..),
  RoleTokenConstraints(..),
  mustConsumeMarloweOutput,
  mustDistributeRoleToken,
  mustMintRoleToken,
  mustPayToAddress,
  mustPayToRole,
  mustSendMarloweOutput,
  mustSpendRoleToken,
  mustConsumePayout,
  requiresMetadata,
  requiresSignature,
) where

import Language.Marlowe.Runtime.ChainSync.Api hiding (Datum)
import Data.Map (Map)
import Language.Marlowe.Runtime.Core.Api (IsMarloweVersion(..), MarloweVersionTag, MarloweTransactionMetadata(..))
import qualified Language.Marlowe.Runtime.Core.Api as Core
import Language.Marlowe.Runtime.Transaction.Api (ConstraintError(..))

data TxConstraints era (v :: MarloweVersionTag) = TxConstraints
  { marloweInputConstraints :: MarloweInputConstraints v
  , payoutInputConstraints :: ()
  , roleTokenConstraints :: RoleTokenConstraints era
  , payToAddresses :: ()
  , payToRoles :: ()
  , marloweOutputConstraints :: MarloweOutputConstraints v
  , signatureConstraints :: ()
  , metadataConstraints :: MarloweTransactionMetadata
  }
data RoleTokenConstraints era = RoleTokenConstraintsNone
  deriving (Eq, Show)
data MarloweInputConstraints (v :: MarloweVersionTag) = MarloweInputConstraintsNone
  deriving (Eq, Show)
data MarloweOutputConstraints (v :: MarloweVersionTag) = MarloweOutputConstraintsNone
  deriving (Eq, Show)

instance Semigroup (MarloweInputConstraints v) where
  _ <> _ = MarloweInputConstraintsNone

instance Monoid (MarloweInputConstraints v) where
  mempty = MarloweInputConstraintsNone

instance Semigroup (MarloweOutputConstraints v) where
  _ <> _ = MarloweOutputConstraintsNone

instance Monoid (MarloweOutputConstraints v) where
  mempty = MarloweOutputConstraintsNone

instance Semigroup (RoleTokenConstraints era) where
  _ <> _ = RoleTokenConstraintsNone

instance Monoid (RoleTokenConstraints era) where
  mempty = RoleTokenConstraintsNone

instance Semigroup (TxConstraints era v) where
  a <> b = TxConstraints
    { marloweInputConstraints = marloweInputConstraints a <> marloweInputConstraints b
    , payoutInputConstraints = ()
    , roleTokenConstraints = roleTokenConstraints a <> roleTokenConstraints b
    , payToAddresses = ()
    , payToRoles = ()
    , marloweOutputConstraints = marloweOutputConstraints a <> marloweOutputConstraints b
    , signatureConstraints = ()
    , metadataConstraints = metadataConstraints a <> metadataConstraints b
    }

instance Monoid (TxConstraints era v) where
  mempty = TxConstraints
    { marloweInputConstraints = mempty
    , payoutInputConstraints = ()
    , roleTokenConstraints = mempty
    , payToAddresses = ()
    , payToRoles = ()
    , marloweOutputConstraints = mempty
    , signatureConstraints = ()
    , metadataConstraints = mempty
    }
data WalletContext = WalletContext
  { changeAddress :: Address
  , availableUtxos :: UTxOs
  }
data PayoutContext = PayoutContext
  { payoutOutputs :: Map TxOutRef TransactionOutput
  }
data MarloweContext (v :: MarloweVersionTag) = MarloweContext
data HelpersContext = HelpersContext
data HelperScriptInfo = HelperScriptInfo
data HelperScriptState = HelperScriptState

mustConsumeMarloweOutput :: forall v era. IsMarloweVersion v => SlotNo -> SlotNo -> Inputs v -> TxConstraints era v
mustConsumeMarloweOutput _ _ _ = mempty

mustDistributeRoleToken :: a -> b -> c -> TxConstraints era v
mustDistributeRoleToken _ _ _ = mempty

mustMintRoleToken :: a -> b -> c -> d -> e -> TxConstraints era v
mustMintRoleToken _ _ _ _ _ = mempty

mustPayToAddress :: a -> b -> TxConstraints era v
mustPayToAddress _ _ = mempty

mustPayToRole :: a -> b -> TxConstraints era v
mustPayToRole _ _ = mempty

mustSendMarloweOutput :: TxOutAssets -> Core.Datum v -> TxConstraints era v
mustSendMarloweOutput _ _ = mempty

mustSpendRoleToken :: a -> TxConstraints era v
mustSpendRoleToken _ = mempty

mustConsumePayout :: TxOutRef -> TxConstraints era v
mustConsumePayout _ = mempty

data WithdrawConstraintError = WithdrawConstraintError ConstraintError

requiresMetadata :: MarloweTransactionMetadata -> TxConstraints era v
requiresMetadata metadataConstraints = TxConstraints
  { marloweInputConstraints = MarloweInputConstraintsNone
  , payoutInputConstraints = ()
  , roleTokenConstraints = RoleTokenConstraintsNone
  , payToAddresses = ()
  , payToRoles = ()
  , marloweOutputConstraints = MarloweOutputConstraintsNone
  , signatureConstraints = ()
  , metadataConstraints
  }

requiresSignature :: PaymentKeyHash -> TxConstraints era v
requiresSignature _ = mempty