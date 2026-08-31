module Marlowe.ContractStore.Protocol.Transfer.Server where

import Numeric.Natural (Natural)
import Language.Marlowe.Object.Types (ObjectBundle, Label, ContractHash)
import Data.Map (Map)
import Marlowe.ContractStore.Protocol.Transfer.Types (ImportError, Message (MsgStartImport, MsgRequestExport, MsgDone, MsgStartExport, MsgContractNotFound, MsgUpload, MsgImported, MsgUploaded, MsgUploadFailed, MsgDownload, MsgCancel, MsgDownloaded, MsgExported), MarloweTransfer (StIdle, StCanUpload, StCanDownload))
import Network.TypedProtocol.Peer.Server (Server, pattern Yield, pattern Await, pattern Effect, pattern Done, IsPipelined (NonPipelined))
import Data.Functor ((<&>))

newtype MarloweTransferServer m a = MarloweTransferServer
  { runMarloweTransferServer :: m (ServerStIdle m a)
  }
  deriving (Functor)

data ServerStIdle m a = ServerStIdle
  { recvMsgStartImport :: m (ServerStCanUpload m a)
  , recvMsgRequestExport :: ContractHash -> m (ServerStExport m a)
  , recvMsgDone :: m a
  }
  deriving (Functor)

data ServerStExport m a where
  SendMsgStartExport :: ServerStCanDownload m a -> ServerStExport m a
  SendMsgContractNotFound :: ServerStIdle m a -> ServerStExport m a

deriving instance (Functor m) => Functor (ServerStExport m)

data ServerStCanUpload m a = ServerStCanUpload
  { recvMsgUpload :: ObjectBundle -> m (ServerStUpload m a)
  , recvMsgImported :: m (ServerStIdle m a)
  }
  deriving (Functor)

data ServerStCanDownload m a = ServerStCanDownload
  { recvMsgDownload :: Natural -> m (ServerStDownload m a)
  , recvMsgCancel :: m (ServerStIdle m a)
  }
  deriving (Functor)

data ServerStDownload m a where
  SendMsgDownloaded :: ObjectBundle -> ServerStCanDownload m a -> ServerStDownload m a
  SendMsgExported :: ServerStIdle m a -> ServerStDownload m a

deriving instance (Functor m) => Functor (ServerStDownload m)

data ServerStUpload m a where
  SendMsgUploaded :: Map Label ContractHash -> ServerStCanUpload m a -> ServerStUpload m a
  SendMsgUploadFailed :: ImportError -> ServerStIdle m a -> ServerStUpload m a

deriving instance (Functor m) => Functor (ServerStUpload m)

hoistMarloweTransferServer
  :: forall m n a
   . (Functor m)
  => (forall x. m x -> n x)
  -> MarloweTransferServer m a
  -> MarloweTransferServer n a
hoistMarloweTransferServer f = MarloweTransferServer . f . fmap hoistIdle . runMarloweTransferServer
  where
    hoistIdle :: ServerStIdle m a -> ServerStIdle n a
    hoistIdle ServerStIdle{..} =
      ServerStIdle
        { recvMsgStartImport = f $ hoistCanUpload <$> recvMsgStartImport
        , recvMsgRequestExport = f . fmap hoistExport . recvMsgRequestExport
        , recvMsgDone = f recvMsgDone
        }

    hoistExport :: ServerStExport m a -> ServerStExport n a
    hoistExport = \case
      SendMsgStartExport next -> SendMsgStartExport $ hoistCanDownload next
      SendMsgContractNotFound next -> SendMsgContractNotFound $ hoistIdle next

    hoistCanUpload :: ServerStCanUpload m a -> ServerStCanUpload n a
    hoistCanUpload ServerStCanUpload{..} =
      ServerStCanUpload
        { recvMsgUpload = f . fmap hoistUpload . recvMsgUpload
        , recvMsgImported = f $ hoistIdle <$> recvMsgImported
        }

    hoistDownload :: ServerStDownload m a -> ServerStDownload n a
    hoistDownload = \case
      SendMsgDownloaded bundle next -> SendMsgDownloaded bundle $ hoistCanDownload next
      SendMsgExported next -> SendMsgExported $ hoistIdle next

    hoistUpload :: ServerStUpload m a -> ServerStUpload n a
    hoistUpload = \case
      SendMsgUploaded hashes next -> SendMsgUploaded hashes $ hoistCanUpload next
      SendMsgUploadFailed err next -> SendMsgUploadFailed err $ hoistIdle next

    hoistCanDownload :: ServerStCanDownload m a -> ServerStCanDownload n a
    hoistCanDownload ServerStCanDownload{..} =
      ServerStCanDownload
        { recvMsgDownload = f . fmap hoistDownload . recvMsgDownload
        , recvMsgCancel = f $ hoistIdle <$> recvMsgCancel
        }

type Server' st m a = Server MarloweTransfer NonPipelined st m a

peerIdle
  :: Monad m
  => ServerStIdle m a
  -> Server' StIdle m a
peerIdle ServerStIdle{..} = Await \case
  MsgStartImport -> Effect do
    stCanUpload <- recvMsgStartImport
    pure $ peerCanUpload stCanUpload
  MsgRequestExport hash -> Effect do
    recvMsgRequestExport hash <&> \case
      SendMsgStartExport next -> Yield MsgStartExport $ peerCanDownload next
      SendMsgContractNotFound next -> Yield MsgContractNotFound $ peerIdle next
  MsgDone -> Effect do
    a <- recvMsgDone
    pure $ Done a

peerCanUpload :: Monad m => ServerStCanUpload m a -> Server' 'StCanUpload m a
peerCanUpload ServerStCanUpload{..} = Await \case
  MsgUpload bundle -> Effect do
    recvMsgUpload bundle <&> \case
      SendMsgUploaded hashes idle -> Yield (MsgUploaded hashes) $ peerCanUpload idle
      SendMsgUploadFailed err next -> Yield (MsgUploadFailed err) $ peerIdle next
  MsgImported -> Effect do
    stIdle <- recvMsgImported
    pure $ peerIdle stIdle

peerCanDownload :: Monad m => ServerStCanDownload m a -> Server' 'StCanDownload m a
peerCanDownload ServerStCanDownload{..} = Await \case
  MsgDownload i -> Effect do
    recvMsgDownload i <&> \case
      SendMsgDownloaded bundle next -> Yield (MsgDownloaded bundle) $ peerCanDownload next
      SendMsgExported next -> Yield MsgExported $ peerIdle next
  MsgCancel -> Effect do
    stIdle <- recvMsgCancel
    pure $ peerIdle stIdle


marloweTransferServerPeer
  :: forall m a
   . Monad m
  => MarloweTransferServer m a
  -> Server' StIdle m a
marloweTransferServerPeer MarloweTransferServer{..} = Effect do
  stIdle <- runMarloweTransferServer
  pure $ peerIdle stIdle

