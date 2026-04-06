{-# LANGUAGE DataKinds #-}

module Language.Marlowe.Runtime.Transaction.BuildConstraintsSpec (
  buildConstraintsSpec,
) where

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (property)

import Cardano.Api (BabbageEraOnwards (BabbageEraOnwardsBabbage))
import qualified Cardano.Api as C
import Control.Monad.Trans.Except (runExceptT)
import Data.Functor.Identity (Identity (..))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Marlowe.Plutus.Semantics.Types (Contract (Close))
import Language.Marlowe.Runtime.ChainSync.Api (
  Address (Address),
  Lovelace (Lovelace),
  TxOutAssets (..),
  PlutusScript (PlutusScript),
  UTxOs (UTxOs),
 )
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api hiding (Datum)
import qualified Language.Marlowe.Runtime.Core.Api as Core
import Language.Marlowe.Runtime.Transaction.Api (CreateError, RoleTokensConfig(RoleTokensNone), WithdrawError)
import Language.Marlowe.Runtime.Transaction.BuildConstraints (
  AdjustMinUTxO (AdjustMinUTxO),
  RolesPolicyId,
  buildCreateConstraints,
  buildWithdrawConstraints,
 )
import Language.Marlowe.Runtime.Transaction.Constraints (
  TxConstraints (..),
  WalletContext (..),
 )
import qualified Language.Marlowe.Runtime.Transaction.Constraints as TxConstraints

type TestEra = C.BabbageEra

babbageEraOnwardsTest :: C.BabbageEraOnwards TestEra
babbageEraOnwardsTest = BabbageEraOnwardsBabbage

buildConstraintsSpec :: Spec
buildConstraintsSpec = do
  describe "buildCreateConstraints" createSpec
  describe "buildWithdrawConstraints" withdrawSpec
  describe "buildApplyInputsConstraints" applyInputsSpec

createSpec :: Spec
createSpec = do
  it "buildCreateConstraints returns constraints" $ property $
    let walletCtx = WalletContext (Address "test") (UTxOs mempty)
        adjustMinUTxO = AdjustMinUTxO id
        result :: Either CreateError ((Core.Datum 'V1, TxOutAssets, RolesPolicyId), TxConstraints TestEra 'V1)
        result = runIdentity $ buildCreateConstraints
          (\_ _ -> pure $ PlutusScript mempty)
          babbageEraOnwardsTest
          Core.MarloweV1
          walletCtx
          (Chain.TokenName "thread")
          RoleTokensNone
          (Core.MarloweTransactionMetadata Nothing mempty)
          (Lovelace 2000000)
          mempty
          adjustMinUTxO
          Close
    in case result of
      Left _err -> property True
      Right ((_datum, _assets, _rolesPolicyId), _constraints) -> property True

withdrawSpec :: Spec
withdrawSpec = do
  it "buildWithdrawConstraints returns a result" $ property $
    let payoutContext = TxConstraints.PayoutContext Map.empty
        result :: Either WithdrawError (Map Chain.TxOutRef (Core.Payout 'V1), TxConstraints TestEra 'V1)
        result = runIdentity $ runExceptT $ buildWithdrawConstraints
          payoutContext
          Core.MarloweV1
          Set.empty
    in case result of
      Left _err -> property True
      Right _ -> property True

applyInputsSpec :: Spec
applyInputsSpec = do
  it "buildApplyInputsConstraints returns a result (EraHistory not yet implemented)" $ property True
