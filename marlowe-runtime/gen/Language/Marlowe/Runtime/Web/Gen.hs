{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Web.Gen where

import Cardano.Api qualified as C
import Control.Monad (replicateM)
import Data.Aeson (ToJSON, Value (Null))
import Data.ByteString qualified as BS
import Data.Data (Typeable)
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.OpenApi (Pattern, ToSchema)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Language.Marlowe.Object.Gen ()
import Language.Marlowe.Runtime.Transaction.Gen ()
import Language.Marlowe.Runtime.Web.Adapter.Links qualified as Web
import Language.Marlowe.Runtime.Web.Adapter.Servant (WithRuntimeStatus)
import Language.Marlowe.Runtime.Web.Adapter.Servant qualified as Web
import Language.Marlowe.Runtime.Web.Contract.API (ContractOrSourceId (..))
import Language.Marlowe.Runtime.Web.Contract.API qualified as Web
import Language.Marlowe.Runtime.Web.Core.Address qualified as Web
import Language.Marlowe.Runtime.Web.Core.Asset qualified as Web
import Language.Marlowe.Runtime.Web.Core.Base16 qualified as Web
import Language.Marlowe.Runtime.Web.Core.BlockHeader qualified as Web
import Language.Marlowe.Runtime.Web.Core.MarloweVersion qualified as Web
import Language.Marlowe.Runtime.Web.Core.Metadata qualified as Web
import Language.Marlowe.Runtime.Web.Core.Party qualified as Web
import Language.Marlowe.Runtime.Web.Core.Roles qualified as Web
import Language.Marlowe.Runtime.Web.Core.Tx (TransactionUnspentOutput(TransactionUnspentOutput))
import Language.Marlowe.Runtime.Web.Core.Tx qualified as Web
import Language.Marlowe.Runtime.Web.Payout.API qualified as Web
import Language.Marlowe.Runtime.Web.Role.API qualified as Web
import Language.Marlowe.Runtime.Web.Role.TokenFilter qualified as Web
import Language.Marlowe.Runtime.Web.Tx.API qualified as Web
import Language.Marlowe.Runtime.Web.Withdrawal.API qualified as Web
import Marlowe.Plutus.Analysis.Safety.Types ( SafetyError (MissingRolesCurrency),)
import Marlowe.Plutus.Analysis.Safety.Types qualified as Web
import Marlowe.Plutus.Semantics (TransactionInput (..))
import Marlowe.Plutus.Semantics.Types qualified as Semantics (Input (..))
import Marlowe.Plutus.Semantics.Types qualified as V1
import Marlowe.Plutus.Testing.Semantics.Arbitrary ( ValidContractStructure (unValidContractStructure),)
import Marlowe.Plutus.Testing.Semantics.Next.Arbitrary ()
import Servant.API ( Headers, ReqBody', Verb, type (:<|>), type (:>),)
import Test.Gen.Cardano.Api ()
import Test.Gen.Cardano.Api.Typed (genTxOutValue, genAddressInEra, genTxOutDatumHashUTxOContext)
import Test.QuickCheck (Arbitrary (..), Gen, chooseInt, elements, genericShrink, listOf, oneof, resize, sized, suchThat)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Hedgehog (hedgehog)
import Text.Regex.Posix ((=~))

type family RetractRuntimeStatus api where
  RetractRuntimeStatus (WithRuntimeStatus api) = api

type family WrapContractBodies (api :: Type) :: Type where
  WrapContractBodies (ReqBody' mods cs V1.Contract :> api) = ReqBody' mods cs WrappedContract :> WrapContractBodies api
  WrapContractBodies ((e :: k) :> api) = e :> WrapContractBodies api
  WrapContractBodies (api1 :<|> api2) = WrapContractBodies api1 :<|> WrapContractBodies api2
  WrapContractBodies (Verb v s cs (Headers hs V1.Contract)) = Verb v s cs (Headers hs WrappedContract)
  WrapContractBodies (Verb v s cs V1.Contract) = Verb v s cs WrappedContract
  WrapContractBodies (Verb v s cs a) = Verb v s cs a
  WrapContractBodies api = api

newtype WrappedContract = WrappedContract {unWrappedContract :: V1.Contract}
  deriving (Typeable)
  deriving newtype (Show, ToJSON, ToSchema)

instance Arbitrary WrappedContract where
  arbitrary = WrappedContract <$> resize 6 arbitrary
  shrink = fmap WrappedContract . shrink . unWrappedContract

patternChecker :: Pattern -> Text -> Bool
patternChecker pat text = T.unpack text =~ T.unpack pat

instance Arbitrary Web.PostContractSourceResponse where
  arbitrary =
    Web.PostContractSourceResponse
      <$> arbitrary
      <*> arbitrary

instance Arbitrary Web.WithdrawalHeader where
  arbitrary =
    Web.WithdrawalHeader
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary Web.ContractHeader where
  arbitrary =
    Web.ContractHeader
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary Web.TxHeader where
  arbitrary =
    Web.TxHeader
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary Web.PayoutState where
  arbitrary =
    Web.PayoutState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

instance Arbitrary Web.ContractState where
  arbitrary =
    Web.ContractState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      -- size of 6 will result in a 1-layer deep contract being generated (this is
      -- all we care about for the purposes of schema checking).
      <*> resize 6 arbitrary
      <*> arbitrary
      <*> resize 6 arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

instance Arbitrary Web.Payout where
  arbitrary =
    Web.Payout
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

instance Arbitrary Web.PayoutStatus where
  arbitrary = elements [Web.Available, Web.Withdrawn]
  shrink = genericShrink

instance Arbitrary Web.PayoutHeader where
  arbitrary =
    Web.PayoutHeader
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

instance Arbitrary Web.Assets where
  arbitrary = Web.Assets <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary Web.Tokens where
  arbitrary = Web.Tokens <$> arbitrary
  shrink = genericShrink

instance Arbitrary Web.Withdrawal where
  arbitrary =
    Web.Withdrawal
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

instance Arbitrary Web.RoleTokenFilter where
  arbitrary =
    oneof
      [ Web.RoleTokensAnd <$> arbitrary <*> arbitrary
      , Web.RoleTokensOr <$> arbitrary <*> arbitrary
      , Web.RoleTokenNot <$> arbitrary
      , Web.RoleTokenNot <$> arbitrary
      , pure Web.RoleTokenFilterNone
      , Web.RoleTokenFilterByContracts <$> arbitrary
      , Web.RoleTokenFilterByPolicyIds <$> arbitrary
      , Web.RoleTokenFilterByTokens <$> arbitrary
      , pure Web.RoleTokenFilterAny
      ]
  shrink = genericShrink

instance Arbitrary Web.Tx where
  arbitrary = do
    -- size of 6 will result in a 1-layer deep contract being generated (this is
    -- all we care about for the purposes of schema checking).
    contract <- resize 6 arbitrary
    Web.Tx
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> pure contract
      <*> arbitrary
      <*> arbitraryNormal -- FIXME: This should handle merkleized input, too.
      <*> arbitrary
      <*> elements [Nothing, Just contract]
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> (TransactionInput <$> arbitrary <*> arbitraryNormal)
      <*> resize 6 arbitrary
      <*> arbitrary
  shrink = genericShrink

instance Arbitrary Web.PostWithdrawalsRequest where
  arbitrary = Web.PostWithdrawalsRequest <$> arbitrary

instance Arbitrary Web.PostContractsRequest where
  arbitrary =
    Web.PostContractsRequest
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

instance Arbitrary Web.Party where
  arbitrary = Web.Party <$> arbitrary
  shrink = genericShrink

instance Arbitrary Web.ContractOrSourceId where
  arbitrary =
    ContractOrSourceId
      <$> oneof
        [ Right <$> arbitrary
        , Left <$> resize 6 (arbitrary <&> unValidContractStructure)
        ]
  shrink = genericShrink

instance Arbitrary Web.PostTransactionsRequest where
  arbitrary =
    Web.PostTransactionsRequest
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitraryNormal -- FIXME: This should handle merkleized input, too.
  shrink = genericShrink

instance Arbitrary (Web.CreateTxEnvelope tx) where
  arbitrary = Web.CreateTxEnvelope <$> arbitrary <*> arbitrary <*> resize 5 arbitrary
  shrink = genericShrink

instance Arbitrary (Web.WithdrawTxEnvelope tx) where
  arbitrary = Web.WithdrawTxEnvelope <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary (Web.ApplyInputsTxEnvelope tx) where
  arbitrary =
    Web.ApplyInputsTxEnvelope
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> (fmap . fmap) unWebSafetyError arbitrary
  shrink = genericShrink

newtype WebSafetyError = WebSafetyError {unWebSafetyError :: Web.SafetyError}
  deriving (Show, Generic)

instance Arbitrary WebSafetyError where
  arbitrary = pure . WebSafetyError $ MissingRolesCurrency
  shrink = genericShrink

instance Arbitrary (Web.BurnRoleTokensTxEnvelope tx) where
  arbitrary = Web.BurnRoleTokensTxEnvelope <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary Web.MarloweVersion where
  arbitrary = pure Web.V1

instance Arbitrary Web.RolesConfig where
  arbitrary =
    oneof
      [ Web.UsePolicy <$> arbitrary
      , Web.Mint <$> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary Web.RoleTokenConfig where
  arbitrary = Web.RoleTokenConfig <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary Web.RoleTokenRecipient where
  arbitrary =
    oneof
      [ Web.ClosedRole <$> arbitrary
      , pure Web.OpenRole
      ]
  shrink = genericShrink

instance Arbitrary Web.TokenMetadata where
  arbitrary =
    Web.TokenMetadata
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> oneof
        [ pure Nothing
        , Just <$> sized \size -> do
            len <- chooseInt (0, size)
            case len of
              0 -> pure []
              _ -> do
                let itemSize = size `div` len
                resize itemSize $ replicateM len arbitrary
        ]
      <*> pure mempty
  shrink = genericShrink

instance Arbitrary Web.TokenMetadataFile where
  arbitrary =
    Web.TokenMetadataFile
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> pure mempty
  shrink = genericShrink

instance Arbitrary Web.Address where
  arbitrary = Web.Address <$> arbitrary
  shrink = genericShrink

instance Arbitrary Web.StakeAddress where
  arbitrary = Web.StakeAddress <$> arbitrary
  shrink = genericShrink

instance (Arbitrary a) => Arbitrary (Web.ListObject a) where
  arbitrary = Web.ListObject <$> arbitrary
  shrink = genericShrink

instance Arbitrary Web.TextEnvelope where
  arbitrary = Web.TextEnvelope <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary Web.TxOutRef where
  arbitrary = Web.TxOutRef <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary Web.AssetId where
  arbitrary = Web.AssetId <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary Web.ContractSourceId where
  arbitrary = Web.ContractSourceId . BS.pack <$> replicateM 32 arbitrary

instance Arbitrary Web.TxId where
  arbitrary = Web.TxId . BS.pack <$> replicateM 32 arbitrary

instance Arbitrary Web.PolicyId where
  arbitrary = Web.PolicyId . BS.pack <$> listOf arbitrary

instance Arbitrary Web.Metadata where
  arbitrary = pure $ Web.Metadata Null

instance Arbitrary Web.TxStatus where
  arbitrary =
    elements
      [ Web.Unsigned
      , Web.Submitted
      , Web.Confirmed
      ]

instance Arbitrary Web.BlockHeader where
  arbitrary = Web.BlockHeader <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Web.Base16 where
  arbitrary = Web.Base16 . BS.pack <$> listOf arbitrary

instance (Arbitrary a) => Arbitrary (Web.WithLink name a) where
  arbitrary =
    oneof
      [ Web.OmitLink <$> arbitrary
      , Web.IncludeLink (Proxy @name) <$> arbitrary
      ]
  shrink (Web.OmitLink a) = Web.OmitLink <$> shrink a
  shrink (Web.IncludeLink n a) = [Web.OmitLink a] <> (Web.IncludeLink n <$> shrink a)

arbitraryNormal :: Gen [Semantics.Input]
arbitraryNormal =
  arbitrary `suchThat` all isNormal
  where
    isNormal (Semantics.NormalInput _) = True
    isNormal _ = False

arbitraryTxIn :: Gen C.TxIn
arbitraryTxIn = do
  txId <- C.TxId <$> arbitrary
  txIx <- C.TxIx <$> arbitrary
  pure $ C.TxIn txId txIx

arbitraryTxOutConwayEra :: Gen (C.TxOut C.CtxUTxO C.ConwayEra)
arbitraryTxOutConwayEra = do
  let
    era = C.shelleyBasedEra
  hedgehog $ do
    address <- genAddressInEra era
    txOutValue <- genTxOutValue era
    datumHash <- genTxOutDatumHashUTxOContext era
    pure $ C.TxOut address txOutValue datumHash C.ReferenceScriptNone

instance Arbitrary (TransactionUnspentOutput C.ConwayEra) where
  arbitrary = TransactionUnspentOutput <$> arbitraryTxIn <*> arbitraryTxOutConwayEra
  shrink (TransactionUnspentOutput _txIn _txOut) = []
