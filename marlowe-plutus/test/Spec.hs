{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Marlowe tests.
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
module Main (
  -- * Testing
  main,
) where

import GHC.Word (Word8)
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)
import Test.Tasty.QuickCheck

import qualified Spec.Marlowe.Marlowe (prop_noFalsePositives, tests)
import qualified Spec.Marlowe.Merkle (tests)
import qualified Spec.Marlowe.Plate (tests)
import qualified Spec.Marlowe.Semantics (tests)
import qualified Spec.Marlowe.Serialization (tests)
import qualified Spec.Marlowe.Service.Isabelle (tests)
import qualified Spec.Marlowe.StaticAnalysis (tests)
import qualified Spec.PlutusTx (tests)
import PlutusLedgerApi.V3 (Address(..), ScriptHash (ScriptHash), Credential (..))
import Marlowe.Plutus.Scripts
    ( mkMarloweValidator, mkRolePayoutValidator )
import PlutusLedgerApi.Data.V3 (toBuiltin, fromData)
import qualified Data.ByteString as BS
import PlutusTx.These (These(..))
import Marlowe.Plutus.Testing.Scripts
    ( CheckPlutusLog(..),
      FixtureName(..),
      MarloweScripts(..),
      specForFixture,
      specForScripts,
      TestFailures(..),
      SemanticsAddress(..),
      RolePayoutAddress(..) )
import Data.Traversable (for)

-- | Timeout seconds for static analysis, which can take so much time on a complex contract
--   that it exceeds hydra/CI resource limits, see SCP-4267.
timeout :: Maybe Int
#ifdef STATIC_ANALYSIS_TIMEOUT
timeout = Just STATIC_ANALYSIS_TIMEOUT
#else
timeout = Nothing
#endif

-- | Entry point for the tests.
main :: IO ()
main = do
  let
    -- Create somewhat characteristic script hash
    -- so it is easier to read the logs
    mkPsudoHash :: (Word8, Word8, Word8, Word8) -> ScriptHash
    mkPsudoHash (a, b, c, d) = ScriptHash . toBuiltin . BS.pack . concatMap (replicate 7) $ [a, b, c, d]
    semanticsValidatorHash = mkPsudoHash (1, 2, 3, 4)
    payoutValidatorHash = mkPsudoHash (5, 6, 7, 8)
    semanticsValidator = mkMarloweValidator payoutValidatorHash
    marloweScripts = MarloweScripts
      { semanticsAddress = SemanticsAddress $ Address (ScriptCredential semanticsValidatorHash) Nothing
      , rolePayoutAddress = RolePayoutAddress $ Address (ScriptCredential payoutValidatorHash) Nothing
      , runSemantics = \builtinData -> case fromData builtinData of
          Just scriptContext -> do
            let
              valid = semanticsValidator scriptContext
            if valid
              then That []
              else This "Failed to validate semantics script context"
          Nothing -> This "Failed to decode semantics script context"
      , runPayout = \builtinData -> case fromData builtinData of
          Just scriptContext -> do
            let
              valid = mkRolePayoutValidator scriptContext
            if valid
              then That []
              else This "Failed to validate payout script context"
          Nothing -> This "Failed to decode payout script context"
      }
  testName <- lookupEnv "MARLOWE_TESTING_FIXTURE"
  specificFixtureScriptsHSpec <- for testName $ \name -> do
    specForFixture "PureDevel" (FixtureName name) marloweScripts (TestFailures False) (CheckPlutusLog False)

  case specificFixtureScriptsHSpec of
    Just specificHSpec ->
      defaultMain =<< testSpec "Scripts" specificHSpec
    Nothing -> do
      broadScriptsHSpec <- specForScripts "PureDevel" marloweScripts (TestFailures False) (CheckPlutusLog False)
      scriptsSpec <- testSpec "Scripts" broadScriptsHSpec
      defaultMain $ mkTests scriptsSpec

-- | Run the tests.
mkTests :: TestTree -> TestTree
mkTests scriptsSpec = do
  testGroup
    "Marlowe"
    [ scriptsSpec
    , Spec.Marlowe.Marlowe.tests
    , Spec.Marlowe.Merkle.tests
    , Spec.Marlowe.Plate.tests
    , testGroup
        "Static Analysis"
        [ testProperty "No false positives" $ Spec.Marlowe.Marlowe.prop_noFalsePositives timeout
        ]
    , Spec.Marlowe.Serialization.tests
    , Spec.Marlowe.Semantics.tests
    , Spec.Marlowe.Service.Isabelle.tests
    , Spec.Marlowe.StaticAnalysis.tests
    , Spec.PlutusTx.tests
    ]
