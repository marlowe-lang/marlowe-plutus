{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Minimal stub
module Language.Marlowe.Runtime.Transaction.Api (
  Account(..),
  Accounts(..),
  ApplyInputsConstraintsBuildupError(..),
  ApplyInputsError(..),
  ConstraintError(..),
  CreateBuildupError(..),
  CreateError(..),
  Destination(..),
  Mint(..),
  MintRole(..),
  RoleTokenMetadata(..),
  RoleTokensConfig(RoleTokensNone, UnsafeRoleTokensUsePolicy, UnsafeRoleTokensMint),
  WithdrawError(..),
  encodeRoleTokenMetadata,
  getTokenQuantities,
  mkAccounts,
) where

import Language.Marlowe.Runtime.ChainSync.Api
import Language.Marlowe.Runtime.Core.ScriptRegistry (HelperScript(..))
import Marlowe.Plutus.Semantics (TransactionError)
import Data.Foldable (Foldable (fold))
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Map.NonEmpty (NEMap)
import Data.Text (Text)

data Account
  = RoleAccount TokenName
  | AddressAccount Address
  deriving (Show, Eq, Ord)
newtype Accounts = Accounts { unAccounts :: Map Account TxOutAssets }
  deriving (Eq)

instance Semigroup Accounts where
  Accounts a <> Accounts b = Accounts $ Map.unionWith (<>) a b

instance Monoid Accounts where
  mempty = Accounts mempty
data ApplyInputsConstraintsBuildupError
  = MarloweComputeTransactionFailed TransactionError
  | InvalidMarloweState
  | UnableToDetermineTransactionTimeout
  deriving (Show, Eq)
data ApplyInputsError
  = ApplyInputsError
  | ValidityLowerBoundTooHigh SlotNo SlotNo
  | SlotConversionFailed String
  | ApplyInputsConstraintsBuildupFailed ApplyInputsConstraintsBuildupError
  deriving (Show, Eq)
data CreateBuildupError
  = MintingUtxoSelectionFailed
  | AddressesDecodingFailed [Address]
  | InvalidInitialState
  | MintingScriptDecodingFailed PlutusScript
  deriving (Show, Eq)
data CreateError
  = CreateBuildupFailed CreateBuildupError
  | CreateError
  deriving (Show, Eq)
data Destination = ToAddress Address | ToScript HelperScript
  deriving (Show, Eq, Ord)
newtype Mint = Mint { unMint :: NEMap TokenName MintRole }
  deriving (Show, Eq)
data MintRole = MintRole
  { roleMetadata :: Maybe RoleTokenMetadata
  , roleTokenRecipients :: NEMap Destination Quantity
  }
  deriving (Show, Eq)
data RoleTokenMetadata = RoleTokenMetadata
  { name :: Maybe Text
  , image :: Maybe Text
  , mediaType :: Maybe Text
  , description :: Maybe Text
  , files :: Maybe [RoleTokenMetadataFile]
  }
  deriving (Show, Eq)
data RoleTokenMetadataFile = RoleTokenMetadataFile
  { fileName :: Text
  , fileMediaType :: Maybe Text
  }
  deriving (Show, Eq)

data RoleTokensConfig
  = RoleTokensNone
  | UnsafeRoleTokensUsePolicy PolicyId (Map TokenName (Map Destination Quantity))
  | UnsafeRoleTokensMint Mint
  deriving (Show, Eq)

pattern RoleTokensUsePolicy :: PolicyId -> Map TokenName (Map Destination Quantity) -> RoleTokensConfig
pattern RoleTokensUsePolicy policyId dist = UnsafeRoleTokensUsePolicy policyId dist

pattern RoleTokensMint :: Mint -> RoleTokensConfig
pattern RoleTokensMint mint = UnsafeRoleTokensMint mint

data WithdrawError
  = WithdrawError
  | EmptyPayouts
  | WithdrawConstraintError ConstraintError
  deriving (Show, Eq)

data ConstraintError
  = PayoutNotFound TxOutRef
  | InvalidPayoutDatum TxOutRef (Maybe Datum)
  deriving (Show, Eq)

encodeRoleTokenMetadata :: a -> b
encodeRoleTokenMetadata = error "stub"

getTokenQuantities :: Mint -> NEMap TokenName Quantity
getTokenQuantities (Mint mintMap) = fmap (fold . roleTokenRecipients) mintMap

mkAccounts :: Map Account TxOutAssets -> Either () Accounts
mkAccounts = Right . Accounts