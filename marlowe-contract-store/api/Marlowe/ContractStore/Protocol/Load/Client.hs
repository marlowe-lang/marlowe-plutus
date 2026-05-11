module Marlowe.ContractStore.Protocol.Load.Client where

import Data.Kind (Type)
import qualified Marlowe.Plutus.Semantics.Types as S
import Marlowe.ContractStore.Protocol.Load.Types (Nat(Succ, Zero), Node (IfLNode, WhenNode, LetNode, CaseNode, PayNode, AssertNode, RootNode, IfRNode), SNat (SZero, SSucc), SNode (SWhenNode, SRootNode, SPayNode, SIfLNode, SIfRNode, SCaseNode, SLetNode, SAssertNode), MarloweLoad (StProcessing, StCanPush, StComplete), Message (MsgResume, MsgAbort, MsgRequestResume, MsgPushClose, MsgComplete, MsgPushPay, MsgPushIf, MsgPushWhen, MsgPushCase, MsgPushLet, MsgPushAssert), Pop, withMarloweLoadStateTokenI, sPop)
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash)
import Network.TypedProtocol.Peer.Client (Client, IsPipelined (NonPipelined), pattern Await, pattern Effect, pattern Yield, pattern Done)
import Data.Singletons (withSingI)

-- A client in the Processing state.
newtype ClientStProcessing (node :: Node) m a = ClientStProcessing
  { recvMsgResume :: forall n. SNat (Succ n) -> m (ClientStCanPush (Succ n) node m a)
  -- ^ The server has instructed the client to resume pushing contract nodes.
  }

-- A type family that computes the next client state when popping a node.
-- Corresponds to the @@Pop@@ type family.
type family ClientStPop (n :: Nat) (node :: Node) :: (Type -> Type) -> Type -> Type where
  ClientStPop n 'RootNode = ClientStComplete
  ClientStPop n ('PayNode node) = ClientStPop n node
  ClientStPop n ('IfLNode node) = ClientStCanPush n ('IfRNode node)
  ClientStPop n ('IfRNode node) = ClientStPop n node
  ClientStPop n ('WhenNode node) = ClientStPop n node
  ClientStPop n ('CaseNode node) = ClientStCanPush n ('WhenNode node)
  ClientStPop n ('LetNode node) = ClientStPop n node
  ClientStPop n ('AssertNode node) = ClientStPop n node

-- A client in the CanPush state.
data ClientStCanPush (n :: Nat) (node :: Node) m a where
  -- | Push a close node to the stack. This closes the current stack and pops
  -- until the next incomplete location in the contract is reached.
  PushClose
    :: m (ClientStPop n node m a)
    -- ^ The next client to run after popping the current stack.
    -> ClientStCanPush (Succ n) node m a
  -- | Push a pay node to the stack.
  PushPay
    :: S.AccountId
    -- ^ The account from which to withdraw the payment.
    -> S.Payee
    -- ^ To whom send the payment.
    -> S.Token
    -- ^ The token to be paid.
    -> S.Value S.Observation
    -- ^ The amount to be paid.
    -> m (ClientStCanPush n ('PayNode node) m a)
    -- ^ The next client to push the sub-contract.
    -> ClientStCanPush (Succ n) node m a
  -- | Push an if node to the stack.
  PushIf
    :: S.Observation
    -- ^ The observation to choose which branch to follow.
    -> m (ClientStCanPush n ('IfLNode node) m a)
    -- ^ The next client to push the "then" sub-contract.
    -> ClientStCanPush (Succ n) node m a
  -- | Push a when node to the stack
  PushWhen
    :: S.Timeout
    -- ^ The time (in POSIX milliseconds) after which this contract will time out.
    -> m (ClientStCanPush n ('WhenNode node) m a)
    -- ^ The next client to push either the first case or the timeout continuation.
    -> ClientStCanPush (Succ n) node m a
  -- | Push a case node to the stack. Only available if currently on a when node.
  PushCase
    :: S.Action
    -- ^ The action which will match this case.
    -> m (ClientStCanPush n (CaseNode node) m a)
    -- ^ The next client to push the sub-contract.
    -> ClientStCanPush (Succ n) (WhenNode node) m a
  -- | Push a let node to the stack.
  PushLet
    :: S.ValueId
    -- ^ The name that refers to the value.
    -> S.Value S.Observation
    -- ^ The value to bind to the name.
    -> m (ClientStCanPush n (LetNode node) m a)
    -- ^ The next client to push the sub-contract.
    -> ClientStCanPush (Succ n) node m a
  -- | Push an assert node to the stack.
  PushAssert
    :: S.Observation
    -- ^ The observation to assert.
    -> m (ClientStCanPush n ('AssertNode node) m a)
    -- ^ The next client to push the sub-contract.
    -> ClientStCanPush (Succ n) node m a
  -- | Request to resume pushing.
  RequestResume
    :: ClientStProcessing node m a
    -- ^ The next client to run on resume
    -> ClientStCanPush Zero node m a
  -- | Abort the load.
  Abort :: a -> ClientStCanPush n node m a

newtype ClientStComplete m a = ClientStComplete
  { recvMsgComplete :: DatumHash -> m a
  -- ^ The server sends the hash of the merkleized root contract.
  }

newtype MarloweLoadClient m a = MarloweLoadClient
  { runMarloweLoadClient :: m (ClientStProcessing 'RootNode m a)
  }

hoistMarloweLoadClient
  :: forall m n a
   . (Functor m)
  => (forall x. m x -> n x)
  -> MarloweLoadClient m a
  -> MarloweLoadClient n a
hoistMarloweLoadClient f = MarloweLoadClient . f . fmap (hoistProcessing SRootNode) . runMarloweLoadClient
  where
    hoistProcessing :: SNode node -> ClientStProcessing node m a -> ClientStProcessing node n a
    hoistProcessing node ClientStProcessing{..} =
      ClientStProcessing \n -> f $ fmap (hoistCanPush node n) $ recvMsgResume n

    hoistCanPush :: SNode node -> SNat i -> ClientStCanPush i node m a -> ClientStCanPush i node n a
    hoistCanPush node SZero = \case
      Abort a -> Abort a
      RequestResume processing -> RequestResume $ hoistProcessing node processing
    hoistCanPush node (SSucc n) = \case
      Abort a -> Abort a
      PushClose next -> PushClose $ f $ hoistPop node n <$> next
      PushPay payor payee token value next -> PushPay payor payee token value $ f $ hoistCanPush (SPayNode node) n <$> next
      PushIf cond next -> PushIf cond $ f $ hoistCanPush (SIfLNode node) n <$> next
      PushWhen timeout next -> PushWhen timeout $ f $ hoistCanPush (SWhenNode node) n <$> next
      PushCase action next -> PushCase action case node of
        SWhenNode node' -> f $ hoistCanPush (SCaseNode node') n <$> next
      PushLet valueId value next -> PushLet valueId value $ f $ hoistCanPush (SLetNode node) n <$> next
      PushAssert obs next -> PushAssert obs $ f $ hoistCanPush (SAssertNode node) n <$> next

    hoistPop :: SNode node -> SNat i -> ClientStPop i node m a -> ClientStPop i node n a
    hoistPop node n = case node of
      SRootNode -> hoistComplete
      SPayNode node' -> hoistPop node' n
      SIfLNode node' -> hoistCanPush (SIfRNode node') n
      SIfRNode node' -> hoistPop node' n
      SWhenNode node' -> hoistPop node' n
      SCaseNode node' -> hoistCanPush (SWhenNode node') n
      SLetNode node' -> hoistPop node' n
      SAssertNode node' -> hoistPop node' n

    hoistComplete :: ClientStComplete m a -> ClientStComplete n a
    hoistComplete = ClientStComplete . fmap f . recvMsgComplete
-- ^ Interpret a client as a traced typed-protocols peer.

type Client' st m a = Client MarloweLoad NonPipelined st m a

peerProcessing
  :: Monad m
  => SNode node
  -> ClientStProcessing node m a
  -> Client' ('StProcessing node) m a
peerProcessing node ClientStProcessing{..} =
  withSingI node $ Await \case
    MsgResume n -> Effect do
      stCanPush <- recvMsgResume n
      pure $ peerCanPush n node stCanPush

peerPop
  :: Monad m
  => SNat n
  -> SNode node
  -> ClientStPop n node m a
  -> Client' (Pop n node) m a
peerPop n node client = case node of
  SRootNode -> peerComplete client
  SPayNode node' -> peerPop n node' client
  SIfLNode node' -> peerCanPush n (SIfRNode node') client
  SIfRNode node' -> peerPop n node' client
  SWhenNode node' -> peerPop n node' client
  SCaseNode node' -> peerCanPush n (SWhenNode node') client
  SLetNode node' -> peerPop n node' client
  SAssertNode node' -> peerPop n node' client

peerComplete
  :: Monad m
  => ClientStComplete m a
  -> Client' StComplete m a
peerComplete ClientStComplete{..} = Await \case
  MsgComplete hash -> Effect do
    a <- recvMsgComplete hash
    pure $ Done a

peerCanPush
  :: Monad m
  => SNat n
  -> SNode node
  -> ClientStCanPush n node m a
  -> Client' ('StCanPush n node) m a
peerCanPush SZero node = withSingI node \case
  Abort a -> Yield MsgAbort $ Done a
  RequestResume ClientStProcessing {..} -> Yield MsgRequestResume $ Await \case
    MsgResume n -> Effect do
      clientStCanPush <- recvMsgResume n
      pure $ peerCanPush n node clientStCanPush
peerCanPush n@(SSucc n') node = withSingI n' $ withSingI n $ withSingI node \case
  Abort a -> Yield MsgAbort $ Done a
  PushClose next -> do
    let
      st' = sPop n' node
    withMarloweLoadStateTokenI st' $ Yield MsgPushClose $ Effect do
      clientStPop <- next
      pure $ peerPop n' node clientStPop
  PushPay payor payee token value next -> Yield (MsgPushPay payor payee token value) $ Effect do
      clientStCanPush <- next
      pure $ peerCanPush n' (SPayNode node) clientStCanPush
  PushIf cond next -> Yield (MsgPushIf cond) $ Effect do
      clientStCanPush <- next
      pure $ peerCanPush n' (SIfLNode node) clientStCanPush
  PushWhen timeout next -> Yield (MsgPushWhen timeout) $ Effect do
      clientStCanPush <- next
      pure $ peerCanPush n' (SWhenNode node) clientStCanPush
  PushCase action next -> case node of
    SWhenNode node' -> withSingI node' $ Yield (MsgPushCase action) $ Effect do
      clientStCanPush <- next
      pure $ peerCanPush n' (SCaseNode node') clientStCanPush
  PushLet valueId value next -> Yield (MsgPushLet valueId value) $ Effect do
      clientStCanPush <- next
      pure $ peerCanPush n' (SLetNode node) clientStCanPush
  PushAssert obs next -> Yield (MsgPushAssert obs) $ Effect do
      clientStCanPush <- next
      pure $ peerCanPush n' (SAssertNode node) clientStCanPush

marloweLoadClientPeer
  :: forall m a
   . Monad m
  => MarloweLoadClient m a
  -> Client' ('StProcessing 'RootNode) m a
marloweLoadClientPeer MarloweLoadClient{..} = Effect do
  clientStProcessing <- runMarloweLoadClient
  pure $ peerProcessing SRootNode clientStProcessing

