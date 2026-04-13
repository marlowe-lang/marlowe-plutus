{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Minimal stub
module Language.Marlowe.Runtime.Transaction.Api (
  Account(..),
  Accounts(..),
  ApplyInputsConstraintsBuildupError(..),
  ApplyInputsError(..),
  CoinSelectionError(..),
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

import qualified Language.Marlowe.Runtime.ChainSync.Api as ChainSync
import Data.Set (Set)
import Language.Marlowe.Runtime.Core.ScriptRegistry (HelperScript(..))
import Marlowe.Plutus.Semantics (TransactionError)
import qualified Data.Aeson as Aeson
import Data.Aeson (ToJSON, ToJSONKey)
import Data.Aeson.Types (toJSONKeyText)
import Data.Foldable (Foldable (fold))
import GHC.Generics (Generic)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Map.NonEmpty (NEMap)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Cardano.Api as C
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersion(MarloweV1), MarloweTransactionMetadata, MarloweVersionTag (V1), TransactionScriptOutput, Inputs, Payout, TransactionOutput)
import Marlowe.Plutus.Analysis.Safety.Types (SafetyError)
import Data.Time (UTCTime)
import qualified Language.Marlowe.Runtime.Core.Api as M

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
data CoinSelectionError
  = NoCollateralFound (Set TxOutRef)
  | InsufficientLovelace Integer Integer
  | InsufficientTokens Tokens
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
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Destination where
  toJSON = \case
    ToAddress addr -> Aeson.object ["tag" Aeson..= ("ToAddress" :: Text), "address" Aeson..= addr]
    ToScript hs -> Aeson.object ["tag" Aeson..= ("ToScript" :: Text), "helperScript" Aeson..= hs]

instance ToJSONKey Destination where
  toJSONKey = toJSONKeyText (Text.pack . show)
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
  = MintingUtxoNotFound TxOutRef
  | RoleTokenNotFound AssetId
  | ToCardanoError
  | MissingMarloweInput
  | PayoutNotFound TxOutRef
  | InvalidPayoutDatum TxOutRef (Maybe Datum)
  | InvalidHelperDatum TxOutRef (Maybe Datum)
  | InvalidPayoutScriptAddress TxOutRef Address
  | InvalidTokenQuantity AssetId Quantity
  | CalculateMinUtxoFailed String
  | CoinSelectionFailed CoinSelectionError
  | BalancingError String
  | MarloweInputInWithdraw
  | MarloweOutputInWithdraw
  | PayoutOutputInWithdraw
  | PayoutInputInCreateOrApply
  | UnknownPayoutScript ScriptHash
  | HelperScriptNotFound TokenName
  deriving (Show, Eq)

encodeRoleTokenMetadata :: a -> b
encodeRoleTokenMetadata = error "stub"

getTokenQuantities :: Mint -> NEMap TokenName Quantity
getTokenQuantities (Mint mintMap) = fmap (fold . roleTokenRecipients) mintMap

mkAccounts :: Map Account TxOutAssets -> Either () Accounts
mkAccounts = Right . Accounts


data ContractCreated v where
  ContractCreated
    :: C.BabbageEraOnwards era -> ContractCreatedInEra era v -> ContractCreated v

-- instance Variations (ContractCreated 'V1) where
--   variations =
--     sconcat
--       [ ContractCreated C.BabbageEraOnwardsBabbage <$> variations
--       , ContractCreated C.BabbageEraOnwardsConway <$> variations
--       ]

instance Show (ContractCreated 'V1) where
  showsPrec p (ContractCreated C.BabbageEraOnwardsBabbage created) =
    showParen (p > 10) $
      showString "ContractCreated"
        . showSpace
        . showString "BabbageEraOnwardsBabbage"
        . showsPrec 11 created
  showsPrec p (ContractCreated C.BabbageEraOnwardsConway created) =
    showParen (p > 10) $
      showString "ContractCreated"
        . showSpace
        . showString "BabbageEraOnwardsConway"
        . showsPrec 11 created

instance Eq (ContractCreated 'V1) where
  ContractCreated C.BabbageEraOnwardsBabbage a == ContractCreated C.BabbageEraOnwardsBabbage b =
    a == b
  ContractCreated C.BabbageEraOnwardsBabbage _ == _ = False
  ContractCreated C.BabbageEraOnwardsConway a == ContractCreated C.BabbageEraOnwardsConway b =
    a == b
  ContractCreated C.BabbageEraOnwardsConway _ == _ = False

instance ToJSON (ContractCreated 'V1) where
  toJSON (ContractCreated C.BabbageEraOnwardsBabbage created) =
    object
      [ "era" .= String "babbage"
      , "contractCreated" .= created
      ]
  toJSON (ContractCreated C.BabbageEraOnwardsConway created) =
    object
      [ "era" .= String "conway"
      , "contractCreated" .= created
      ]

-- instance Binary (ContractCreated 'V1) where
--   put (ContractCreated C.BabbageEraOnwardsBabbage created) = do
--     putWord8 0
--     put created
--   put (ContractCreated C.BabbageEraOnwardsConway created) = do
--     putWord8 1
--     put created
--   get = do
--     eraTag <- getWord8
--     case eraTag of
--       0 -> ContractCreated C.BabbageEraOnwardsBabbage <$> get
--       1 -> ContractCreated C.BabbageEraOnwardsConway <$> get
--       _ -> fail $ "Invalid era tag value: " <> show eraTag

data ContractCreatedInEra era v = ContractCreatedInEra
  { contractId :: ContractId
  , rolesCurrency :: PolicyId
  , metadata :: MarloweTransactionMetadata
  , marloweScriptHash :: ScriptHash
  , marloweScriptAddress :: Address
  , payoutScriptHash :: ScriptHash
  , payoutScriptAddress :: Address
  , version :: MarloweVersion v
  , datum :: M.Datum v
  , assets :: TxOutAssets
  , txBody :: C.TxBody era
  , safetyErrors :: ![SafetyError]
  }

deriving instance Show (ContractCreatedInEra C.BabbageEra 'V1)
deriving instance Show (ContractCreatedInEra C.ConwayEra 'V1)
deriving instance Eq (ContractCreatedInEra C.BabbageEra 'V1)
deriving instance Eq (ContractCreatedInEra C.ConwayEra 'V1)

-- instance (IsShelleyBasedEra era) => Variations (ContractCreatedInEra era 'V1) where
--   variations =
--     ContractCreatedInEra
--       <$> variations
--         `varyAp` variations
--         `varyAp` variations
--         `varyAp` variations
--         `varyAp` variations
--         `varyAp` variations
--         `varyAp` variations
--         `varyAp` pure MarloweV1
--         `varyAp` variations
--         `varyAp` variations
--         `varyAp` variations
--         `varyAp` variations

instance (C.IsShelleyBasedEra era) => ToJSON (ContractCreatedInEra era 'V1) where
  toJSON ContractCreatedInEra{..} =
    object
      [ "contract-id" .= contractId
      , "roles-currency" .= rolesCurrency
      , "metadata" .= metadata
      , "marlowe-script-hash" .= marloweScriptHash
      , "marlowe-script-address" .= marloweScriptAddress
      , "payout-script-hash" .= payoutScriptHash
      , "payout-script-address" .= payoutScriptAddress
      , "datum" .= datum
      , "assets" .= assets
      , "tx-body" .= serialiseToTextEnvelope Nothing txBody
      , "safety-errors" .= safetyErrors
      ]

data InputsApplied v where
  InputsApplied
    :: C.BabbageEraOnwards era -> InputsAppliedInEra era v -> InputsApplied v

-- instance Variations (InputsApplied 'V1) where
--   variations = InputsApplied C.BabbageEraOnwardsBabbage <$> variations
-- 
-- instance Show (InputsApplied 'V1) where
--   showsPrec p (InputsApplied C.BabbageEraOnwardsBabbage created) =
--     showParen (p > 10) $
--       showString "InputsApplied"
--         . showSpace
--         . showString "BabbageEraOnwardsBabbage"
--         . showsPrec 11 created
--   showsPrec p (InputsApplied C.BabbageEraOnwardsConway created) =
--     showParen (p > 10) $
--       showString "InputsApplied"
--         . showSpace
--         . showString "BabbageEraOnwardsConway"
--         . showsPrec 11 created

instance Eq (InputsApplied 'V1) where
  InputsApplied C.BabbageEraOnwardsBabbage a == InputsApplied C.BabbageEraOnwardsBabbage b =
    a == b
  InputsApplied C.BabbageEraOnwardsBabbage _ == _ = False
  InputsApplied C.BabbageEraOnwardsConway a == InputsApplied C.BabbageEraOnwardsConway b =
    a == b
  InputsApplied C.BabbageEraOnwardsConway _ == _ = False

instance ToJSON (InputsApplied 'V1) where
  toJSON (InputsApplied C.BabbageEraOnwardsBabbage created) =
    object
      [ "era" .= String "babbage"
      , "contractCreated" .= created
      ]
  toJSON (InputsApplied C.BabbageEraOnwardsConway created) =
    object
      [ "era" .= String "conway"
      , "contractCreated" .= created
      ]


data InputsAppliedInEra era v = InputsAppliedInEra
  { version :: MarloweVersion v
  , contractId :: ContractId
  , metadata :: MarloweTransactionMetadata
  , input :: TransactionScriptOutput v
  , output :: TransactionOutput v
  , invalidBefore :: UTCTime
  , invalidHereafter :: UTCTime
  , inputs :: Inputs v
  , txBody :: C.TxBody era
  , safetyErrors :: [SafetyError]
  }

deriving instance Show (InputsAppliedInEra C.BabbageEra 'V1)
deriving instance Eq (InputsAppliedInEra C.BabbageEra 'V1)
deriving instance Show (InputsAppliedInEra C.ConwayEra 'V1)
deriving instance Eq (InputsAppliedInEra C.ConwayEra 'V1)

-- instance (IsShelleyBasedEra era) => Variations (InputsAppliedInEra era 'V1) where
--   variations =
--     InputsAppliedInEra MarloweV1
--       <$> variations
--         `varyAp` variations
--         `varyAp` variations
--         `varyAp` variations
--         `varyAp` variations
--         `varyAp` variations
--         `varyAp` variations
--         `varyAp` variations
--         `varyAp` variations

data WithdrawTx v where
  WithdrawTx
    :: C.BabbageEraOnwards era -> WithdrawTxInEra era v -> WithdrawTx v

-- instance Variations (WithdrawTx 'V1) where
--   variations = WithdrawTx C.BabbageEraOnwardsBabbage <$> variations

instance Show (WithdrawTx 'V1) where
  showsPrec p (WithdrawTx C.BabbageEraOnwardsBabbage created) =
    showParen (p > 10) $
      showString "WithdrawTx"
        . showSpace
        . showString "BabbageEraOnwardsBabbage"
        . showsPrec 11 created
  showsPrec p (WithdrawTx C.BabbageEraOnwardsConway created) =
    showParen (p > 10) $
      showString "WithdrawTx"
        . showSpace
        . showString "BabbageEraOnwardsConway"
        . showsPrec 11 created

instance Eq (WithdrawTx 'V1) where
  WithdrawTx C.BabbageEraOnwardsBabbage a == WithdrawTx C.BabbageEraOnwardsBabbage b =
    a == b
  WithdrawTx C.BabbageEraOnwardsBabbage _ == _ = False
  WithdrawTx C.BabbageEraOnwardsConway a == WithdrawTx C.BabbageEraOnwardsConway b =
    a == b
  WithdrawTx C.BabbageEraOnwardsConway _ == _ = False

data WithdrawTxInEra era v = WithdrawTxInEra
  { version :: MarloweVersion v
  , inputs :: Map TxOutRef (Payout v)
  , txBody :: C.TxBody era
  }

deriving instance Show (WithdrawTxInEra C.BabbageEra 'V1)
deriving instance Eq (WithdrawTxInEra C.BabbageEra 'V1)
deriving instance Show (WithdrawTxInEra C.ConwayEra 'V1)
deriving instance Eq (WithdrawTxInEra C.ConwayEra 'V1)

-- instance (IsShelleyBasedEra era) => Variations (WithdrawTxInEra era 'V1) where
--   variations = WithdrawTxInEra MarloweV1 <$> variations `varyAp` variations

instance (C.IsShelleyBasedEra era) => ToJSON (InputsAppliedInEra era 'V1) where
  toJSON InputsAppliedInEra{..} =
    object
      [ "contract-id" .= contractId
      , "input" .= input
      , "output" .= output
      , "invalid-before" .= invalidBefore
      , "invalid-hereafter" .= invalidHereafter
      , "inputs" .= inputs
      , "tx-body" .= serialiseToTextEnvelope Nothing txBody
      , "safety-errors" .= safetyErrors
      ]
