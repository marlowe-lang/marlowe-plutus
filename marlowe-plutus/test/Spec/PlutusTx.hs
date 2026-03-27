-- | Tests of Marlowe's Plutus implementation.
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
module Spec.PlutusTx (
  -- * Testing
  tests,
) where

import Test.Tasty (TestTree, testGroup)

import qualified Spec.PlutusTx.AssocMap (tests)
import qualified Spec.PlutusTx.MList (tests)
import qualified Spec.PlutusTx.Prelude (tests)
import qualified Spec.PlutusTx.ScriptContext (tests)
import qualified Spec.PlutusTx.Value (tests)

-- | Run tests.
tests :: TestTree
tests =
  testGroup
    "Plutus"
    [ Spec.PlutusTx.Prelude.tests
    , Spec.PlutusTx.AssocMap.tests
    , Spec.PlutusTx.MList.tests
    , Spec.PlutusTx.Value.tests
    , Spec.PlutusTx.ScriptContext.tests
    ]
