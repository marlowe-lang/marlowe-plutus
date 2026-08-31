-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Spec.Marlowe.Merkle (
  tests,
) where

import Control.Monad.Writer (runWriter)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Marlowe.Plutus.Merkle (Continuations, deepMerkleize)
import Marlowe.Plutus.Semantics.Types qualified as Core
import PlutusLedgerApi.V2 (
  POSIXTime (POSIXTime),
  TokenName (TokenName),
  adaSymbol,
  adaToken,
 )
import qualified PlutusLedgerApi.V1
import PlutusTx.Builtins.HasOpaque (stringToBuiltinByteString)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "Merkle - selective preservation"
    [ testCase "empty preserve-set behaves like the original merkleizer" emptyPreservesAll
    , testCase "preserved Case stays as Case" preserveKeepsOuterCase
    , testCase "preserved Case's continuation is still recursively merkleized" preserveStillMerkleizesInner
    , testCase "non-preserved Case in the same When becomes MerkleizedCase" nonPreservedStillReplaced
    , testCase "preservation at depth leaves every ancestor fully merkleized" preservedAtDepth
    ]

depositAction :: Core.Action
depositAction =
  Core.Deposit
    (Core.Role (TokenName "alice"))
    (Core.Role (TokenName "alice"))
    (Core.Token adaSymbol adaToken)
    (Core.Constant 1)

notifyAction :: Core.Action
notifyAction = Core.Notify Core.TrueObs

-- | A Choice action that we'll preserve at the deepest layer of the
-- @preservedAtDepth@ fixture.
choiceAction :: Core.Action
choiceAction =
  Core.Choice
    (Core.ChoiceId (stringToBuiltinByteString "team-1-vs-team-2") (Core.Role (TokenName "oracle")))
    [Core.Bound 0 2]

-- A non-terminal inner contract that should produce a MerkleizedCase when
-- the walker reaches it.
nonTerminal :: Core.Contract
nonTerminal =
  Core.When
    [Core.Case notifyAction innerNonTerminal]
    (POSIXTime 1)
    Core.Close

-- A second non-terminal inner contract (used as a grandchild to make sure
-- the walker keeps descending past preserved `Case` boundaries).
innerNonTerminal :: Core.Contract
innerNonTerminal =
  Core.When
    [Core.Case depositAction Core.Close]
    (POSIXTime 2)
    Core.Close

-- | Fixture: a @When@ with a deposit case and a notification case, each
-- pointing at a non-terminal inner @When@ so the walker must reach the
-- inner continuations.
fixture :: Core.Contract
fixture =
  Core.When
    [ Core.Case depositAction nonTerminal
    , Core.Case notifyAction nonTerminal
    ]
    (POSIXTime 0)
    Core.Close

emptyPreservesAll :: IO ()
emptyPreservesAll = do
  let (mc, continuations) = runWriter $ deepMerkleize Set.empty fixture
  case mc of
    Core.When cases _ _ ->
      assertEqual
        "Both top-level cases become MerkleizedCase with an empty preserve-set"
        [True, True]
        (map isMerkleizedCase cases)
    _ -> assertBool "Expected a top-level When" False
  -- The walker descends into each outer case's continuation, producing one
  -- hash per non-Close contract: one for the shared `nonTerminal` inner
  -- When and one for the grandchild `innerNonTerminal` it references.
  assertEqual "Two continuations emitted (outer + grandchild)" 2 (Map.size continuations)

preserveKeepsOuterCase :: IO ()
preserveKeepsOuterCase = do
  let (mc, _) = runWriter $ deepMerkleize (Set.singleton depositAction) fixture
  case mc of
    Core.When cases _ _ -> do
      let depositCase = head cases
          notifyCase = cases !! 1
      assertBool
        "Deposit Case is preserved (action visible)"
        (isCaseWithAction depositAction depositCase)
      assertBool
        "Notify Case is still merkleized (not in the preserve-set)"
        (isMerkleizedCase notifyCase)
    _ -> assertBool "Expected a top-level When" False

preserveStillMerkleizesInner :: IO ()
preserveStillMerkleizesInner = do
  -- The preserved Case has a non-Close inner When with its own non-preserved
  -- case, so we can directly observe recursive merkleization: the inner
  -- non-preserved Case is replaced with a MerkleizedCase, proving the walker
  -- kept descending past the preserved outer Case.
  let outer = Core.When [Core.Case depositAction nonTerminal] (POSIXTime 0) Core.Close
      (mc, continuations) = runWriter $ deepMerkleize (Set.singleton depositAction) outer
  case mc of
    Core.When [Core.Case _ (Core.When innerCases _ _)] _ _ ->
      assertBool
        "Inner non-preserved Case is still merkleized"
        (isMerkleizedCase (head innerCases))
    _ -> assertBool "Expected a When wrapping a preserved Case around a When" False
  assertBool "Recursive walk produces at least one continuation entry"
    (Map.size continuations >= 1)

nonPreservedStillReplaced :: IO ()
nonPreservedStillReplaced = do
  let (mc, _) = runWriter $ deepMerkleize (Set.singleton depositAction) fixture
  case mc of
    Core.When cases _ _ ->
      assertBool
        "Notify case (not preserved) becomes MerkleizedCase"
        (isMerkleizedCase (cases !! 1))
    _ -> assertBool "Expected a top-level When" False

-- | Three-level fixture where the *only* preserved action is at the
-- bottom of the tree. Everything above (outer cases, intermediate
-- continuations) must be hashed.
--
-- > outer When
-- >   [ Case depositOuter nonTerminal1 ]   -- NOT preserved
-- >   (t0) Close
-- > nonTerminal1 =
-- >   When
-- >     [ Case notifyOuter nonTerminal2 ]   -- NOT preserved
-- >     (t1) Close
-- > nonTerminal2 =
-- >   When
-- >     [ Case choiceInner Close ]          -- PRESERVED
-- >     (t2) Close
preservedAtDepth :: IO ()
preservedAtDepth = do
  let outer = Core.When [Core.Case depositAction nonTerminalAtDepth1] (POSIXTime 0) Core.Close
      (mc, continuations) = runWriter $ deepMerkleize (Set.singleton choiceAction) outer
  -- 1. Top-level: the only case is merkleized (preserved action is deeper).
  case mc of
    Core.When [outerCase] _ _ ->
      assertBool
        "Top-level Case is fully merkleized (preservation is at depth)"
        (isMerkleizedCase outerCase)
    _ -> assertBool "Expected a top-level When with one Case" False
  -- 2. Closure contains both intermediate continuations; neither leaks the
  --    preserved action as an unmerkleized Case.
  assertEqual "Two intermediate continuations are emitted" 2 (Map.size continuations)
  let allCases = walksContract mc continuations
  -- 3. Exactly one preserved Case carrying the Choice action in the whole
  --    walked tree; no sibling leak.
  let preservedCases =
        filter
          ( \c -> case c of
              Core.Case action _ -> action == choiceAction
              Core.MerkleizedCase{} -> False
          )
          allCases
  assertEqual "Exactly one preserved Case carrying the Choice action" 1 (length preservedCases)
  case preservedCases of
    [Core.Case action _] ->
      assertBool
        "The only preserved Case carries the Choice action"
        (action == choiceAction)
    _ -> assertBool "Expected a single preserved Case carrying the Choice action" False

-- | Flatten a (possibly merkleized) contract into the list of all `Case`
-- constructors reachable through hashed continuations. Recursively
-- dereferences hashes via the supplied @Continuations@ map so the caller
-- can assert properties of the entire closure, not just the visible
-- outermost cases.
walksContract :: Core.Contract -> Continuations -> [Core.Case Core.Contract]
walksContract contract continuations = walkCase (Core.Case (Core.Notify Core.TrueObs) contract) []
  where
    walkCase :: Core.Case Core.Contract -> [Core.Case Core.Contract] -> [Core.Case Core.Contract]
    walkCase (Core.Case action continuation) acc = walkContract continuation (Core.Case action continuation : acc)
    walkCase (Core.MerkleizedCase action hash) acc = case Map.lookup (PlutusLedgerApi.V1.DatumHash hash) continuations of
      Just continuation -> walkContract continuation acc
      Nothing -> Core.Case action Core.Close : acc

    walkContract :: Core.Contract -> [Core.Case Core.Contract] -> [Core.Case Core.Contract]
    walkContract Core.Close acc = acc
    walkContract (Core.Pay _ _ _ _ c) acc = walkContract c acc
    walkContract (Core.If _ c1 c2) acc = walkContract c1 acc <> walkContract c2 acc
    walkContract (Core.When cases _ c) acc = foldr walkCase acc cases <> walkContract c acc
    walkContract (Core.Let _ _ c) acc = walkContract c acc
    walkContract (Core.Assert _ c) acc = walkContract c acc

isMerkleizedCase :: Core.Case Core.Contract -> Bool
isMerkleizedCase Core.MerkleizedCase{} = True
isMerkleizedCase _ = False

isCaseWithAction :: Core.Action -> Core.Case Core.Contract -> Bool
isCaseWithAction expected (Core.Case action _) = action == expected
isCaseWithAction _ _ = False

-- | Inner When for the @preservedAtDepth@ fixture; pulls in the deeper
-- grandchild.
nonTerminalAtDepth1 :: Core.Contract
nonTerminalAtDepth1 =
  Core.When
    [Core.Case notifyAction nonTerminalAtDepth2]
    (POSIXTime 1)
    Core.Close

-- | Deepest layer: a single @Choice@ case (the action we preserve) and a
-- @Close@ continuation. This is the only spot in the whole tree where
-- preservation is allowed to surface.
nonTerminalAtDepth2 :: Core.Contract
nonTerminalAtDepth2 =
  Core.When
    [Core.Case choiceAction Core.Close]
    (POSIXTime 2)
    Core.Close
