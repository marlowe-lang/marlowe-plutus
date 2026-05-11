module Marlowe.ContractStore.Protocol.Load.Server where

import Data.Type.Equality (type (:~:) (..))
import Marlowe.ContractStore.Protocol.Load.Types (Nat(Succ, Zero), Node (IfLNode, WhenNode, LetNode, CaseNode, PayNode, AssertNode, RootNode, IfRNode), SNat (SZero, SSucc), SNode (SWhenNode, SRootNode, SPayNode, SIfLNode, SIfRNode, SCaseNode, SLetNode, SAssertNode), MarloweLoad (StProcessing, StCanPush, StComplete), Message (MsgResume, MsgAbort, MsgRequestResume, MsgPushClose, MsgComplete, MsgPushPay, MsgPushIf, MsgPushWhen, MsgPushCase, MsgPushLet, MsgPushAssert), Pop)
import qualified Marlowe.Plutus.Semantics.Types as S
import Data.Kind (Type)
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash)
import Network.TypedProtocol (IsPipelined(NonPipelined))
import Network.TypedProtocol.Peer.Server (Server, pattern Yield, pattern Await, pattern Effect, pattern Done)
import Data.Singletons (withSingI)

data ServerStProcessing (node :: Node) m a where
  -- | Instruct the client to resume pushing contract nodes.
  SendMsgResume
    :: SNat (Succ n)
    -- ^ The number of nodes the client is allowed to push.
    -> ServerStCanPush (Succ n) node m a -- The next server to handle the next client push.
    -> ServerStProcessing node m a

-- A server in the CanPush state.
data ServerStCanPush (n :: Nat) (node :: Node) m a where
  ServerStCanPush
    :: ServerStCanPushSucc n node m a
    -> ServerStCanPush (Succ n) node m a

  ServerStPaused
    :: ServerStCanPushZero node m a
    -> ServerStCanPush Zero node m a

-- A server in the CanPush state when the client has no pushes left.
data ServerStCanPushZero (node :: Node) m a = ServerStCanPushZero
  { recvMsgRequestResume :: m (ServerStProcessing node m a)
  , recvMsgAbort :: m a
  }

type family ServerStPop (n :: Nat) (node :: Node) :: (Type -> Type) -> Type -> Type where
  ServerStPop n RootNode = ServerStComplete
  ServerStPop n (PayNode node) = ServerStPop n node
  ServerStPop n (IfLNode node) = ServerStCanPush n (IfRNode node)
  ServerStPop n (IfRNode node) = ServerStPop n node
  ServerStPop n ('WhenNode node) = ServerStPop n node
  ServerStPop n ('CaseNode node) = ServerStCanPush n (WhenNode node)
  ServerStPop n (LetNode node) = ServerStPop n node
  ServerStPop n (AssertNode node) = ServerStPop n node

-- A server in the CanPush state where the client has pushes left.
data ServerStCanPushSucc (n :: Nat) (node :: Node) m a = ServerStCanPushSucc
  { recvClose :: m (ServerStPop n node m a)
  -- ^ Receive a close node, popping the current stack to the next incomplete
  -- contract location.
  , recvPay :: S.AccountId -> S.Payee -> S.Token -> S.Value S.Observation -> m (ServerStCanPush n (PayNode node) m a)
  -- ^ Receive a pay node, then start receiving the sub-contract.
  , recvIf :: S.Observation -> m (ServerStCanPush n (IfLNode node) m a)
  -- ^ Receive an if node, then start receiving the then clause.
  , recvWhen :: S.Timeout -> m (ServerStCanPush n (WhenNode node) m a)
  -- ^ Receive a when node, then start receiving either the cases or the
  -- timeout continuation.
  , recvCase :: forall st'. node :~: WhenNode st' -> S.Action -> m (ServerStCanPush n (CaseNode st') m a)
  -- ^ Receive a case node (will only be called when currently on a when node).
  , recvLet :: S.ValueId -> S.Value S.Observation -> m (ServerStCanPush n (LetNode node) m a)
  -- ^ Receive a let node, then start receiving the sub-contract.
  , recvAssert :: S.Observation -> m (ServerStCanPush n (AssertNode node) m a)
  -- ^ Receive an assert node, then start receiving the sub-contract.
  , recvMsgAbort :: m a
  }

-- A server in the Complete state.
data ServerStComplete m a where
  -- | Send the hash of the merkleized root contract to the client.
  SendMsgComplete :: DatumHash -> m a -> ServerStComplete m a

newtype MarloweLoadServer m a = MarloweLoadServer
  { runMarloweLoadServer :: m (ServerStProcessing 'RootNode m a)
  }

hoistMarloweLoadServer
  :: forall m n a
   . (Functor m)
  => (forall x. m x -> n x)
  -> MarloweLoadServer m a
  -> MarloweLoadServer n a
hoistMarloweLoadServer f = MarloweLoadServer . f . fmap (hoistProcessing SRootNode) . runMarloweLoadServer
  where
    hoistProcessing :: SNode node -> ServerStProcessing node m a -> ServerStProcessing node n a
    hoistProcessing node = \case
      SendMsgResume n canPush -> SendMsgResume n $ hoistCanPush node n canPush

    hoistCanPush :: SNode node -> SNat i -> ServerStCanPush i node m a -> ServerStCanPush i node n a
    hoistCanPush node SZero (ServerStPaused ServerStCanPushZero{..}) =
      ServerStPaused
        ServerStCanPushZero
          { recvMsgRequestResume = f $ hoistProcessing node <$> recvMsgRequestResume
          , recvMsgAbort = f recvMsgAbort
          }
    hoistCanPush node (SSucc n) (ServerStCanPush ServerStCanPushSucc{..}) =
      ServerStCanPush
        ServerStCanPushSucc
          { recvClose = f $ fmap (hoistPop node n) recvClose
          , recvPay = (fmap . fmap . fmap) (f . fmap (hoistCanPush (SPayNode node) n)) . recvPay
          , recvIf = f . fmap (hoistCanPush (SIfLNode node) n) . recvIf
          , recvWhen = f . fmap (hoistCanPush (SWhenNode node) n) . recvWhen
          , recvCase = \case
              Refl -> case node of
                SWhenNode node' -> f . fmap (hoistCanPush (SCaseNode node') n) . recvCase Refl
          , recvLet = fmap (f . fmap (hoistCanPush (SLetNode node) n)) . recvLet
          , recvAssert = f . fmap (hoistCanPush (SAssertNode node) n) . recvAssert
          , recvMsgAbort = f recvMsgAbort
          }

    hoistPop :: SNode node -> SNat i -> ServerStPop i node m a -> ServerStPop i node n a
    hoistPop node n = case node of
      SRootNode -> hoistComplete
      SPayNode node' -> hoistPop node' n
      SIfLNode node' -> hoistCanPush (SIfRNode node') n
      SIfRNode node' -> hoistPop node' n
      SWhenNode node' -> hoistPop node' n
      SCaseNode node' -> hoistCanPush (SWhenNode node') n
      SLetNode node' -> hoistPop node' n
      SAssertNode node' -> hoistPop node' n

    hoistComplete :: ServerStComplete m a -> ServerStComplete n a
    hoistComplete = \case
      SendMsgComplete hash a -> SendMsgComplete hash $ f a

type Server' st m a = Server MarloweLoad NonPipelined st m a

peerPop
  :: Monad m
  => SNat n
  -> SNode node
  -> ServerStPop n node m a
  -> Server' (Pop n node) m a
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
  => ServerStComplete m a
  -> Server' StComplete m a
peerComplete (SendMsgComplete hash next) =
  Yield (MsgComplete hash) $ Effect do
    a <- next
    pure $ Done a

peerCanPush
  :: Monad m
  => SNat n
  -> SNode node
  -> ServerStCanPush n node m a
  -> Server' (StCanPush n node) m a
peerCanPush SZero node (ServerStPaused ServerStCanPushZero{..}) =
  withSingI node $ Await \case
    MsgAbort -> Effect do
        a <- recvMsgAbort
        pure $ Done a
    MsgRequestResume -> Effect do
        next <- recvMsgRequestResume
        pure $ peerProcessing node next
peerCanPush n@(SSucc n') node (ServerStCanPush ServerStCanPushSucc{..}) =
  withSingI n $ withSingI node $ Await \case
    MsgAbort -> Effect do
      a <- recvMsgAbort
      pure $ Done a
    MsgPushClose -> Effect do
      next <- recvClose
      pure $ peerPop n' node next
    MsgPushPay payor payee token value -> Effect do
      next <- recvPay payor payee token value
      pure $ peerCanPush n' (SPayNode node) next
    MsgPushIf cond -> Effect do
      next <- recvIf cond
      pure $ peerCanPush n' (SIfLNode node) next
    MsgPushWhen timeout -> Effect do
      next <- recvWhen timeout
      pure $ peerCanPush n' (SWhenNode node) next
    MsgPushCase action -> case node of
      SWhenNode st' -> Effect do
        next <- recvCase Refl action
        pure $ peerCanPush n' (SCaseNode st') next
    MsgPushLet valueId value -> Effect do
      next <- recvLet valueId value
      pure $ peerCanPush n' (SLetNode node) next
    MsgPushAssert obs ->
      Effect do
        next <- recvAssert obs
        pure $ peerCanPush n' (SAssertNode node) next

peerProcessing
  :: Monad m
  => SNode node
  -> ServerStProcessing node m a
  -> Server' (StProcessing node) m a
peerProcessing node = \case
  SendMsgResume n canPush -> do
    let
      msg = MsgResume n
      peer = peerCanPush n node canPush
    withSingI node $ withSingI n $ Yield msg peer

marloweLoadServerPeer
  :: forall m a
   . Monad m
  => MarloweLoadServer m a
  -> Server' ('StProcessing 'RootNode) m a
marloweLoadServerPeer MarloweLoadServer{..} = Effect do
  stProcessing <- runMarloweLoadServer
  pure $ peerProcessing SRootNode stProcessing
