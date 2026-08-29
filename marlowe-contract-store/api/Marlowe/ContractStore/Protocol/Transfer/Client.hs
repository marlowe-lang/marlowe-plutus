module Marlowe.ContractStore.Protocol.Transfer.Client where

-- import Control.Monad (join)
-- import Data.Map (Map)
-- import Language.Marlowe.Object.Types hiding (Close)
-- import Language.Marlowe.Protocol.Transfer.Server
-- import Language.Marlowe.Protocol.Transfer.Types
-- import Network.Protocol.Peer.Trace
-- import Network.TypedProtocol
-- import Numeric.Natural (Natural)

import Numeric.Natural (Natural)
import Language.Marlowe.Object.Types (ObjectBundle, Label, ContractHash)
import Data.Map (Map)
import Marlowe.ContractStore.Protocol.Transfer.Types (ImportError, Message (MsgStartImport, MsgRequestExport, MsgDone, MsgStartExport, MsgContractNotFound, MsgUpload, MsgImported, MsgUploaded, MsgUploadFailed, MsgDownload, MsgCancel, MsgDownloaded, MsgExported), MarloweTransfer (StIdle, StCanUpload, StCanDownload))
import Network.TypedProtocol.Peer.Client (Client, pattern Yield, pattern Await, pattern Effect, pattern Done, IsPipelined (NonPipelined))

newtype MarloweTransferClient m a = MarloweTransferClient
  { runMarloweTransferClient :: m (ClientStIdle m a)
  }
  deriving (Functor)

data ClientStIdle m a where
  SendMsgStartImport :: ClientStCanUpload m a -> ClientStIdle m a
  SendMsgRequestExport :: ContractHash -> ClientStExport m a -> ClientStIdle m a
  SendMsgDone :: a -> ClientStIdle m a

deriving instance (Functor m) => Functor (ClientStIdle m)

data ClientStExport m a = ClientStExport
  { recvMsgStartExport :: m (ClientStCanDownload m a)
  , recvMsgContractNotFound :: m (ClientStIdle m a)
  }
  deriving (Functor)

data ClientStCanUpload m a where
  SendMsgUpload :: ObjectBundle -> ClientStUpload m a -> ClientStCanUpload m a
  SendMsgImported :: ClientStIdle m a -> ClientStCanUpload m a

data ClientStDownload m a = ClientStDownload
  { recvMsgDownloaded :: ObjectBundle -> m (ClientStCanDownload m a)
  , recvMsgExported :: m (ClientStIdle m a)
  }
  deriving (Functor)

deriving instance (Functor m) => Functor (ClientStCanUpload m)

data ClientStUpload m a = ClientStUpload
  { recvMsgUploaded :: Map Label ContractHash -> m (ClientStCanUpload m a)
  , recvMsgUploadFailed :: ImportError -> m (ClientStIdle m a)
  }
  deriving (Functor)

data ClientStCanDownload m a where
  SendMsgDownload :: Natural -> ClientStDownload m a -> ClientStCanDownload m a
  SendMsgCancel :: ClientStIdle m a -> ClientStCanDownload m a

deriving instance (Functor m) => Functor (ClientStCanDownload m)

hoistMarloweTransferClient
  :: forall m n a
   . (Functor m)
  => (forall x. m x -> n x)
  -> MarloweTransferClient m a
  -> MarloweTransferClient n a
hoistMarloweTransferClient f = MarloweTransferClient . f . fmap hoistIdle . runMarloweTransferClient
  where
    hoistIdle :: ClientStIdle m a -> ClientStIdle n a
    hoistIdle = \case
      SendMsgStartImport next -> SendMsgStartImport $ hoistCanUpload next
      SendMsgRequestExport hash next -> SendMsgRequestExport hash $ hoistExport next
      SendMsgDone a -> SendMsgDone a

    hoistExport :: ClientStExport m a -> ClientStExport n a
    hoistExport ClientStExport{..} =
      ClientStExport
        { recvMsgStartExport = f $ hoistCanDownload <$> recvMsgStartExport
        , recvMsgContractNotFound = f $ hoistIdle <$> recvMsgContractNotFound
        }

    hoistCanUpload :: ClientStCanUpload m a -> ClientStCanUpload n a
    hoistCanUpload = \case
      SendMsgUpload bundle next -> SendMsgUpload bundle $ hoistUpload next
      SendMsgImported next -> SendMsgImported $ hoistIdle next

    hoistUpload :: ClientStUpload m a -> ClientStUpload n a
    hoistUpload ClientStUpload{..} =
      ClientStUpload
        { recvMsgUploaded = f . fmap hoistCanUpload . recvMsgUploaded
        , recvMsgUploadFailed = f . fmap hoistIdle . recvMsgUploadFailed
        }

    hoistDownload :: ClientStDownload m a -> ClientStDownload n a
    hoistDownload ClientStDownload{..} =
      ClientStDownload
        { recvMsgDownloaded = f . fmap hoistCanDownload . recvMsgDownloaded
        , recvMsgExported = f $ hoistIdle <$> recvMsgExported
        }

    hoistCanDownload :: ClientStCanDownload m a -> ClientStCanDownload n a
    hoistCanDownload = \case
      SendMsgDownload i next -> SendMsgDownload i $ hoistDownload next
      SendMsgCancel next -> SendMsgCancel $ hoistIdle next

type Client' st m a = Client MarloweTransfer NonPipelined st m a

peerIdle :: Monad m => ClientStIdle m a -> Client' StIdle m a
peerIdle = \case
  SendMsgStartImport next ->
    Yield MsgStartImport $ peerCanUpload next
  SendMsgRequestExport hash ClientStExport{..} ->
    Yield (MsgRequestExport hash) $
      Await \case
        MsgStartExport -> Effect $ peerCanDownload <$> recvMsgStartExport
        MsgContractNotFound -> Effect $ peerIdle <$> recvMsgContractNotFound
  SendMsgDone a -> Yield MsgDone $ Done a

peerCanUpload
  :: Monad m
  => ClientStCanUpload m a
  -> Client' 'StCanUpload m a
peerCanUpload = \case
  SendMsgUpload bundle ClientStUpload{..} ->
    Yield (MsgUpload bundle) $
      Await \case
        MsgUploaded hashes -> Effect $ peerCanUpload <$> recvMsgUploaded hashes
        MsgUploadFailed err -> Effect $ peerIdle <$> recvMsgUploadFailed err
  SendMsgImported next ->
    Yield MsgImported $ peerIdle next

peerCanDownload
  :: Monad m
  => ClientStCanDownload m a
  -> Client' 'StCanDownload m a
peerCanDownload = \case
  SendMsgDownload i ClientStDownload{..} ->
    Yield (MsgDownload i) $
      Await \case
        MsgDownloaded bundle -> Effect $ peerCanDownload <$> recvMsgDownloaded bundle
        MsgExported -> Effect $ peerIdle <$> recvMsgExported
  SendMsgCancel next ->
    Yield MsgCancel $ peerIdle next

marloweTransferClientPeer
  :: forall m a
   . Monad m
  => MarloweTransferClient m a
  -> Client' 'StIdle m a
marloweTransferClientPeer MarloweTransferClient{..} = Effect do
  stIdle <- runMarloweTransferClient
  pure $ peerIdle stIdle 

-- serveMarloweTransferClient
--   :: forall m a b
--    . (Monad m)
--   => MarloweTransferServer m a
--   -> MarloweTransferClient m b
--   -> m (a, b)
-- serveMarloweTransferClient MarloweTransferServer{..} MarloweTransferClient{..} =
--   join $ serveIdle <$> runMarloweTransferServer <*> runMarloweTransferClient
--   where
--     serveIdle :: ServerStIdle m a -> ClientStIdle m b -> m (a, b)
--     serveIdle ServerStIdle{..} = \case
--       SendMsgStartImport next -> flip serveCanUpload next =<< recvMsgStartImport
--       SendMsgRequestExport hash next -> serveExport next =<< recvMsgRequestExport hash
--       SendMsgDone b -> (,b) <$> recvMsgDone
-- 
--     serveExport :: ClientStExport m b -> ServerStExport m a -> m (a, b)
--     serveExport ClientStExport{..} = \case
--       SendMsgStartExport next -> serveCanDownload next =<< recvMsgStartExport
--       SendMsgContractNotFound next -> serveIdle next =<< recvMsgContractNotFound
-- 
--     serveCanUpload :: ServerStCanUpload m a -> ClientStCanUpload m b -> m (a, b)
--     serveCanUpload ServerStCanUpload{..} = \case
--       SendMsgUpload bundle next -> serveUpload next =<< recvMsgUpload bundle
--       SendMsgImported next -> flip serveIdle next =<< recvMsgImported
-- 
--     serveUpload :: ClientStUpload m b -> ServerStUpload m a -> m (a, b)
--     serveUpload ClientStUpload{..} = \case
--       SendMsgUploaded hash next -> serveCanUpload next =<< recvMsgUploaded hash
--       SendMsgUploadFailed err next -> serveIdle next =<< recvMsgUploadFailed err
-- 
--     serveDownload :: ClientStDownload m b -> ServerStDownload m a -> m (a, b)
--     serveDownload ClientStDownload{..} = \case
--       SendMsgDownloaded bundle next -> serveCanDownload next =<< recvMsgDownloaded bundle
--       SendMsgExported next -> serveIdle next =<< recvMsgExported
-- 
--     serveCanDownload :: ServerStCanDownload m a -> ClientStCanDownload m b -> m (a, b)
--     serveCanDownload ServerStCanDownload{..} = \case
--       SendMsgDownload i next -> serveDownload next =<< recvMsgDownload i
--       SendMsgCancel next -> flip serveIdle next =<< recvMsgCancel
-- 
