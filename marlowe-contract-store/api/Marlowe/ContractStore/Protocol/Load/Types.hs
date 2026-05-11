{-# LANGUAGE UndecidableInstances #-}

-- | The Marlowe load protocol is a protocol for incrementally loading and
-- merkleizing large contracts in a space-efficient way.
module Marlowe.ContractStore.Protocol.Load.Types where

import Data.Singletons (SingI (sing), withSingI)
import Data.Binary (Binary (..), Get, getWord8, putWord8)
import Marlowe.Plutus.Semantics.Types
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash)
import Language.Marlowe.Runtime.Core.Api ()
import Marlowe.ContractStore.Protocol.Codec (BinaryMessage (..))
import Network.TypedProtocol.Core (Protocol(..), Agency(ClientAgency, ServerAgency, NobodyAgency), StateTokenI (stateToken))
import Data.Singletons.Sigma (Sing)
import Data.Kind (Type)
import Network.TypedProtocol (SomeMessage(..), notActiveState)
import Unsafe.Coerce (unsafeCoerce)

-- | A kind-level datatype for the states in the MarloweLoad protocol.
data MarloweLoad where
  -- | In this state, the server is processing contract nodes pushed by the
  -- client.
  StProcessing :: Node -> MarloweLoad
  -- | In this state, the client can push N more nodes to the server before the
  -- server needs to process them.
  StCanPush :: Nat -> Node -> MarloweLoad
  -- | The terminal state
  StDone :: MarloweLoad
  -- | In this state, there are no more nodes to push, and the server is
  -- preparing to send the hash of the contract to the client.
  StComplete :: MarloweLoad

-- | A location in a marlowe contract.
data Node where
  -- | The root of the contract
  RootNode :: Node
  -- | A pay contract.
  PayNode
    :: Node
    -- ^ The parent node.
    -> Node
  -- | An if contract with an incomplete "then" clause.
  IfLNode
    :: Node
    -- ^ The parent node.
    -> Node
  -- | An if contract with an complete "then" clause.
  IfRNode
    :: Node
    -- ^ The parent node.
    -> Node
  -- | A when contract somewhere in a contract, with completed cases.
  WhenNode
    :: Node
    -- ^ The parent node.
    -> Node
  -- | A case in a when contract somewhere in a contract.
  CaseNode
    :: Node
    -- ^ The parent node of the when node above this case.
    -> Node
  -- | A let contract somewhere in a contract.
  LetNode
    :: Node
    -- ^ The parent node.
    -> Node
  -- | An assert contract somewhere in a contract.
  AssertNode
    :: Node
    -- ^ The parent node.
    -> Node

data SNode :: Node -> Type where
  SRootNode   :: SNode 'RootNode
  SPayNode    :: SNode n -> SNode ('PayNode n)
  SIfLNode    :: SNode n -> SNode ('IfLNode n)
  SIfRNode    :: SNode n -> SNode ('IfRNode n)
  SWhenNode   :: SNode n -> SNode ('WhenNode n)
  SCaseNode   :: SNode n -> SNode ('CaseNode n)
  SLetNode    :: SNode n -> SNode ('LetNode n)
  SAssertNode :: SNode n -> SNode ('AssertNode n)

type instance Sing @Node = SNode

instance SingI 'RootNode where sing = SRootNode
instance SingI n => SingI (PayNode n) where sing = SPayNode sing
instance SingI n => SingI (IfLNode n) where sing = SIfLNode sing
instance SingI n => SingI (IfRNode n) where sing = SIfRNode sing
instance SingI n => SingI (WhenNode n) where sing = SWhenNode sing
instance SingI n => SingI (CaseNode n) where sing = SCaseNode sing
instance SingI n => SingI (LetNode n) where sing = SLetNode sing
instance SingI n => SingI (AssertNode n) where sing = SAssertNode sing

data Nat = Zero | Succ Nat

data SNat :: Nat -> Type where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)

type instance Sing @Nat = SNat

natToInt :: SNat n -> Int
natToInt SZero = 0
natToInt (SSucc n) = 1 + natToInt n

unsafeIntToNat :: forall n. Int -> SNat n
unsafeIntToNat 0 = unsafeCoerce SZero
unsafeIntToNat n | n > 0 = unsafeCoerce $ SSucc (unsafeIntToNat (n - 1))
unsafeIntToNat n = error $ "Cannot convert negative integer to Nat: " <> show n

instance SingI 'Zero where sing = SZero
instance SingI n => SingI ('Succ n) where sing = SSucc sing

data SMarloweLoad (st :: MarloweLoad) where
  SStProcessing :: SNode (node :: Node) -> SMarloweLoad (StProcessing node)
  SStCanPush :: SNat n -> SNode (node :: Node) -> SMarloweLoad (StCanPush n node)
  SStDone :: SMarloweLoad 'StDone
  SStComplete :: SMarloweLoad 'StComplete

instance SingI node => StateTokenI (StProcessing node) where
  stateToken = SStProcessing sing
instance (SingI n, SingI node) => StateTokenI (StCanPush n node) where
  stateToken = SStCanPush sing sing
instance StateTokenI 'StDone where
  stateToken = SStDone
instance StateTokenI 'StComplete where
  stateToken = SStComplete

instance Protocol MarloweLoad where
  data Message MarloweLoad st st' where
    -- \| The server tells the client to resume pushing.
    MsgResume
      :: SNat (Succ n)
      -- \^ The number of pushes the client is allowed to perform.
      -> Message
        MarloweLoad
        ('StProcessing node)
        ('StCanPush (Succ n) node)
    -- \| Push a close node to the stack. This closes the current stack and pops
    -- until the next incomplete location in the contract is reached.
    MsgPushClose
      :: Message
          MarloweLoad
          (StCanPush (Succ n) node)
          (Pop n node)
    -- \| Push a pay node to the stack.
    MsgPushPay
      :: AccountId
      -> Payee
      -> Token
      -> Value Observation
      -> Message
          MarloweLoad
          (StCanPush (Succ n) node)
          (StCanPush n ('PayNode node))
    -- \| Push an if node to the stack.
    MsgPushIf
      :: Observation
      -> Message
          MarloweLoad
          (StCanPush (Succ n) node)
          (StCanPush n ('IfLNode node))
    -- \| Push a when node to the stack
    MsgPushWhen
      :: Timeout
      -> Message
          MarloweLoad
          (StCanPush (Succ n) node)
          (StCanPush n ('WhenNode node))
    -- \| Push a case node to the stack. Only available if currently on a when node.
    MsgPushCase
      :: Action
      -> Message
          MarloweLoad
          (StCanPush (Succ n) ('WhenNode node))
          (StCanPush n ('CaseNode node))
    -- \| Push a let node to the stack.
    MsgPushLet
      :: ValueId
      -> Value Observation
      -> Message
          MarloweLoad
          (StCanPush (Succ n) node)
          (StCanPush n ('LetNode node))
    -- \| Push an assert node to the stack.
    MsgPushAssert
      :: Observation
      -> Message
          MarloweLoad
          (StCanPush (Succ n) node)
          (StCanPush n ('AssertNode node))
    -- \| Request to resume pushing.
    MsgRequestResume
      :: Message
          MarloweLoad
          (StCanPush Zero node)
          (StProcessing node)
    -- \| Abort the load
    MsgAbort
      :: Message
          MarloweLoad
          ('StCanPush n node)
          'StDone
    -- \| The server sends the hash of the completed (merkleized) contract back
    -- to the client.
    MsgComplete
      :: DatumHash
      -> Message
          MarloweLoad
          'StComplete
          'StDone

  type StateAgency (StProcessing node) = ServerAgency
  type StateAgency (StCanPush n node) = ClientAgency
  type StateAgency StComplete = ServerAgency
  type StateAgency StDone = NobodyAgency

  type StateToken = SMarloweLoad

-- -- A type family that computes the next protocol state when popping
-- -- (completing) a node.
type family Pop (n :: Nat) (node :: Node) :: MarloweLoad where
  -- Popping the root node transitions to the complete state.
  Pop n 'RootNode = 'StComplete
  -- Popping a pay node pops its parent node.
  Pop n ('PayNode node) = Pop n node
  -- Popping an ifL node transitions to pushing to the else clause.
  Pop n ('IfLNode node) = 'StCanPush n ('IfRNode node)
  -- Popping an ifR node pops its parent node.
  Pop n ('IfRNode node) = Pop n node
  -- Popping a when node pops its parent node.
  Pop n ('WhenNode node) = Pop n node
  -- Popping a case node transitions to pushing to the when clause (pushing the
  -- timeout fallback case).
  Pop n ('CaseNode node) = 'StCanPush n ('WhenNode node)
  -- Popping a let node pops its parent node.
  Pop n ('LetNode node) = Pop n node
  -- Popping an assert node pops its parent node.
  Pop n ('AssertNode node) = Pop n node

sPop :: SNat n -> SNode node -> SMarloweLoad (Pop n node)
sPop n = \case
  SRootNode -> SStComplete
  SPayNode node -> sPop n node
  SIfLNode node -> SStCanPush n (SIfRNode node)
  SIfRNode node -> sPop n node
  SWhenNode node -> sPop n node
  SCaseNode node -> SStCanPush n (SWhenNode node)
  SLetNode node -> sPop n node
  SAssertNode node -> sPop n node

getPushMsg
  :: forall node n
   . SNat n
  -> (forall n' r. SNat n' -> (Get (SomeMessage (StCanPush (Succ n') node)) -> r) -> r)
  -> Get (SomeMessage (StCanPush n node))
getPushMsg SZero _ = fail "No pushes left"
getPushMsg (SSucc n') handle = handle n' id

withMarloweLoadStateTokenI :: SMarloweLoad st' -> (StateTokenI st' => r) -> r
withMarloweLoadStateTokenI sml k = case sml of
  SStProcessing node -> withSingI node k
  SStCanPush n node -> withSingI n $ withSingI node k
  SStComplete -> k
  SStDone -> k

instance BinaryMessage MarloweLoad where
  putMessage = \case
    SStCanPush _ _ -> \case
      MsgPushClose -> putWord8 0x00
      MsgPushPay payor payee token value -> do
        putWord8 0x01
        put payor
        put payee
        put token
        put value
      MsgPushIf cond -> do
        putWord8 0x02
        put cond
      MsgPushWhen timeout -> do
        putWord8 0x03
        put timeout
      MsgPushCase action -> do
        putWord8 0x04
        put action
      MsgPushLet valueId value -> do
        putWord8 0x05
        put valueId
        put value
      MsgPushAssert obs -> do
        putWord8 0x06
        put obs
      MsgRequestResume -> putWord8 0x07
      MsgAbort -> putWord8 0x08
    SStComplete -> \case
      MsgComplete hash -> put hash
    SStProcessing _ -> \case
      MsgResume n -> put $ natToInt n
    SStDone -> \_msg -> notActiveState SStDone

  getMessage = \case
    SStCanPush n node -> withSingI n $ withSingI node do
      tag <- getWord8
      case tag of
        0x00 -> getPushMsg n $ \n' k -> do
          let
            st' = sPop n' node
          withSingI n' $ withMarloweLoadStateTokenI st' $ k $ pure $ SomeMessage MsgPushClose
        0x01 -> getPushMsg n $ \n' k -> k do
          msg <- MsgPushPay <$> get <*> get <*> get <*> get
          pure $ withSingI n' $ SomeMessage msg
        0x02 -> getPushMsg n \n' k -> k do
          msg <- MsgPushIf <$> get
          pure $ withSingI n' $ SomeMessage msg
        0x03 -> getPushMsg n \n' k -> k do
           msg <- MsgPushWhen <$> get
           pure $ withSingI n' $ SomeMessage msg
        0x04 -> getPushMsg n \n' k -> k case node of
          SWhenNode node' -> do
            msg <- MsgPushCase <$> get
            pure $ withSingI n' $ withSingI n $ withSingI node' $ SomeMessage msg
          _ -> fail "Invalid protocol state for MsgPushCase"
        0x05 -> getPushMsg n \n' k -> k do
          msg <- MsgPushLet <$> get <*> get
          pure $ withSingI n' $ withSingI n $ withSingI node $ SomeMessage msg
        0x06 -> getPushMsg n \n' k -> k do
          msg <- MsgPushAssert <$> get
          pure $ withSingI n' $ SomeMessage msg
        0x07 -> case n of
          SZero -> pure $ SomeMessage MsgRequestResume
          _ -> fail "Must push"
        0x08 -> pure $ SomeMessage MsgAbort
        _ -> fail $ "Invalid message tag " <> show tag
    SStComplete -> SomeMessage . MsgComplete <$> get
    SStProcessing node -> withSingI node $ do
      i <- get
      let
        -- unsafeIntToNat together with an arbitrary `n` is actually safe
        -- here - why? Because we immediately forget the type because of
        -- existential types (used by `SomeMessage`). The correct type will
        -- be brought into scope when pattern matching on the pattern synonyms
        -- `Zero` or `Succ n`.
        -- We need this specific `Succ Zero` type to witness the `SingI`
        -- constraint.
        i' :: SNat (Succ Zero)
        i' = unsafeIntToNat i
      pure $ SomeMessage $ MsgResume i'
    SStDone -> notActiveState SStDone

-- -- instance HasSignature MarloweLoad where
-- --   signature _ = "MarloweLoad"
-- 
-- -- instance MessageEq MarloweLoad where
-- --   messageEq (AnyMessageAndAgency _ msg) (AnyMessageAndAgency _ msg') = case msg of
-- --     MsgPushClose -> case msg' of
-- --       MsgPushClose -> True
-- --       _ -> False
-- --     MsgPushPay accountId payee token value -> case msg' of
-- --       MsgPushPay accountId' payee' token' value' ->
-- --         (accountId, payee, token, value) == (accountId', payee', token', value')
-- --       _ -> False
-- --     MsgPushIf obs -> case msg' of
-- --       MsgPushIf obs' ->
-- --         obs == obs'
-- --       _ -> False
-- --     MsgPushWhen timeout -> case msg' of
-- --       MsgPushWhen timeout' ->
-- --         timeout == timeout'
-- --       _ -> False
-- --     MsgPushCase action -> case msg' of
-- --       MsgPushCase action' ->
-- --         action == action'
-- --       _ -> False
-- --     MsgPushLet valueId value -> case msg' of
-- --       MsgPushLet valueId' value' ->
-- --         (valueId, value) == (valueId', value')
-- --       _ -> False
-- --     MsgPushAssert obs -> case msg' of
-- --       MsgPushAssert obs' ->
-- --         obs == obs'
-- --       _ -> False
-- --     MsgResume n -> case msg' of
-- --       MsgResume n' ->
-- --         natToInt n == natToInt n'
-- --       _ -> False
-- --     MsgComplete hash -> case msg' of
-- --       MsgComplete hash' ->
-- --         hash == hash'
-- --       _ -> False
-- --     MsgRequestResume -> case msg' of
-- --       MsgRequestResume -> True
-- --       _ -> False
-- --     MsgAbort -> case msg' of
-- --       MsgAbort -> True
-- --       _ -> False
-- -- 
-- -- instance MessageVariations MarloweLoad where
-- --   messageVariations = \case
-- --     ClientAgency (TokCanPush Succ{} node) ->
-- --       join $
-- --         NE.fromList
-- --           [ pure $ SomeMessage MsgPushClose
-- --           , SomeMessage
-- --               <$> (MsgPushPay <$> variations `varyAp` variations `varyAp` variations `varyAp` variations)
-- --           , SomeMessage . MsgPushIf <$> variations
-- --           , case node of
-- --               SWhenNode _ ->
-- --                 join $
-- --                   NE.fromList
-- --                     [ SomeMessage . MsgPushCase <$> variations
-- --                     , SomeMessage . MsgPushWhen <$> variations
-- --                     ]
-- --               _ -> SomeMessage . MsgPushWhen <$> variations
-- --           , SomeMessage <$> (MsgPushLet <$> variations `varyAp` variations)
-- --           , SomeMessage . MsgPushAssert <$> variations
-- --           ]
-- --     ClientAgency (TokCanPush Zero _) ->
-- --       NE.fromList
-- --         [ SomeMessage MsgRequestResume
-- --         , SomeMessage MsgAbort
-- --         ]
-- --     ServerAgency (TokProcessing _) -> pure $ SomeMessage $ MsgResume $ Succ Zero
-- --     ServerAgency TokComplete -> SomeMessage . MsgComplete <$> variations
-- -- 
-- --   agencyVariations =
-- --     NE.fromList
-- --       [ Codec.SomePeerHasAgency $ ClientAgency $ TokCanPush Zero $ SWhenNode SRootNode
-- --       , Codec.SomePeerHasAgency $ ClientAgency $ TokCanPush (Succ Zero) $ SWhenNode SRootNode
-- --       , Codec.SomePeerHasAgency $ ServerAgency $ TokProcessing SRootNode
-- --       , Codec.SomePeerHasAgency $ ServerAgency TokComplete
-- --       ]
-- -- 
-- -- instance ShowProtocol MarloweLoad where
-- --   showsPrecMessage p = \case
-- --     ClientAgency (TokCanPush _ _) -> \case
-- --       MsgPushClose -> showString "MsgPushClose"
-- --       MsgRequestResume -> showString "MsgRequestResume"
-- --       MsgAbort -> showString "MsgAbort"
-- --       MsgPushPay accountId payee token value ->
-- --         showParen
-- --           (p >= 11)
-- --           ( showString "MsgPushPay"
-- --               . showSpace
-- --               . showsPrec 11 accountId
-- --               . showSpace
-- --               . showsPrec 11 payee
-- --               . showSpace
-- --               . showsPrec 11 token
-- --               . showSpace
-- --               . showsPrec 11 value
-- --           )
-- --       MsgPushIf obs ->
-- --         showParen
-- --           (p >= 11)
-- --           ( showString "MsgPushIf"
-- --               . showSpace
-- --               . showsPrec 11 obs
-- --           )
-- --       MsgPushWhen timeout ->
-- --         showParen
-- --           (p >= 11)
-- --           ( showString "MsgPushWhen"
-- --               . showSpace
-- --               . showsPrec 11 timeout
-- --           )
-- --       MsgPushCase action ->
-- --         showParen
-- --           (p >= 11)
-- --           ( showString "MsgPushCase"
-- --               . showSpace
-- --               . showsPrec 11 action
-- --           )
-- --       MsgPushLet valueId value ->
-- --         showParen
-- --           (p >= 11)
-- --           ( showString "MsgPushLet"
-- --               . showSpace
-- --               . showsPrec 11 valueId
-- --               . showSpace
-- --               . showsPrec 11 value
-- --           )
-- --       MsgPushAssert obs ->
-- --         showParen
-- --           (p >= 11)
-- --           ( showString "MsgPushAssert"
-- --               . showSpace
-- --               . showsPrec 11 obs
-- --           )
-- --     ServerAgency (TokProcessing _) -> \case
-- --       MsgResume n ->
-- --         showParen
-- --           (p >= 11)
-- --           ( showString "MsgResume"
-- --               . showSpace
-- --               . showParen
-- --                 True
-- --                 ( showString "unsafeIntToNat"
-- --                     . showSpace
-- --                     . showsPrec 11 (natToInt n)
-- --                 )
-- --           )
-- --     ServerAgency TokComplete -> \case
-- --       MsgComplete hash ->
-- --         showParen
-- --           (p >= 11)
-- --           ( showString "MsgComplete"
-- --               . showSpace
-- --               . showsPrec 11 hash
-- --           )
-- -- 
-- --   showsPrecServerHasAgency p = \case
-- --     TokProcessing node ->
-- --       showParen
-- --         (p >= 11)
-- --         ( showString "TokProcessing"
-- --             . showSpace
-- --             . showsPrecSNode p node
-- --         )
-- --     TokComplete -> showString "TokComplete"
-- --   showsPrecClientHasAgency p = \case
-- --     TokCanPush n node ->
-- --       showParen
-- --         (p >= 11)
-- --         ( showString "TokCanPush"
-- --             . showSpace
-- --             . showParen
-- --               True
-- --               ( showString "unsafeIntToNat"
-- --                   . showSpace
-- --                   . showsPrec 11 (natToInt n)
-- --               )
-- --             . showSpace
-- --             . showsPrecSNode p node
-- --         )
-- -- 
-- -- showsPrecSNode :: Int -> SNode node -> ShowS
-- -- showsPrecSNode p = \case
-- --   SRootNode -> showString "SRootNode"
-- --   SPayNode node ->
-- --     showParen (p >= 11) (showString "SPayNode" . showSpace . showsPrecSNode 11 node)
-- --   SIfLNode node ->
-- --     showParen (p >= 11) (showString "SIfLNode" . showSpace . showsPrecSNode 11 node)
-- --   SIfRNode node ->
-- --     showParen (p >= 11) (showString "SIfRNode" . showSpace . showsPrecSNode 11 node)
-- --   SWhenNode node ->
-- --     showParen (p >= 11) (showString "SWhenNode" . showSpace . showsPrecSNode 11 node)
-- --   SCaseNode node ->
-- --     showParen (p >= 11) (showString "SCaseNode" . showSpace . showsPrecSNode 11 node)
-- --   SLetNode node ->
-- --     showParen (p >= 11) (showString "SLetNode" . showSpace . showsPrecSNode 11 node)
-- --   SAssertNode node ->
-- --     showParen (p >= 11) (showString "SAssertNode" . showSpace . showsPrecSNode 11 node)


