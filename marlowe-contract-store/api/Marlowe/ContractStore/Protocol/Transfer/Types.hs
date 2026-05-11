module Marlowe.ContractStore.Protocol.Transfer.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary (..), getWord8, putWord8)
import Data.Map (Map)
import Data.Variations (Variations(..))
import GHC.Generics (Generic)
import Language.Marlowe.Object.Link (LinkError)
import Language.Marlowe.Object.Types (ContractHash, ObjectBundle, Label)
import Network.TypedProtocol.Core (Protocol(..), Agency(ClientAgency, ServerAgency, NobodyAgency), StateTokenI (stateToken), notActiveState)
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash)
import GHC.Natural (Natural)
import Marlowe.ContractStore.Protocol.Codec (BinaryMessage (putMessage, getMessage))
import Network.TypedProtocol (SomeMessage(SomeMessage))

data ImportError
  = ContinuationNotInStore ContractHash
  | LinkError LinkError
  deriving stock (Show, Generic, Eq, Ord)
  deriving anyclass (FromJSON, ToJSON, Binary, Variations)

data MarloweTransfer where
  StIdle :: MarloweTransfer
  StExport :: MarloweTransfer
  StCanUpload :: MarloweTransfer
  StUpload :: MarloweTransfer
  StCanDownload :: MarloweTransfer
  StDownload :: MarloweTransfer
  StDone :: MarloweTransfer

data SMarloweTransfer (st :: MarloweTransfer) where
  SStIdle :: SMarloweTransfer 'StIdle
  SStExport :: SMarloweTransfer 'StExport
  SStCanUpload :: SMarloweTransfer 'StCanUpload
  SStUpload :: SMarloweTransfer 'StUpload
  SStCanDownload :: SMarloweTransfer 'StCanDownload
  SStDownload :: SMarloweTransfer 'StDownload
  SStDone :: SMarloweTransfer 'StDone

instance StateTokenI 'StIdle where
  stateToken = SStIdle
instance StateTokenI 'StExport where
  stateToken = SStExport
instance StateTokenI 'StCanUpload where
  stateToken = SStCanUpload
instance StateTokenI 'StUpload where
  stateToken = SStUpload
instance StateTokenI 'StCanDownload where
  stateToken = SStCanDownload
instance StateTokenI 'StDownload where
  stateToken = SStDownload
instance StateTokenI 'StDone where
  stateToken = SStDone

instance Protocol MarloweTransfer where
  data Message MarloweTransfer st st' where
    MsgStartImport :: Message MarloweTransfer 'StIdle 'StCanUpload
    MsgRequestExport :: DatumHash -> Message MarloweTransfer 'StIdle 'StExport
    MsgStartExport :: Message MarloweTransfer 'StExport 'StCanDownload
    MsgContractNotFound :: Message MarloweTransfer 'StExport 'StIdle
    MsgDone :: Message MarloweTransfer 'StIdle 'StDone
    MsgUpload :: ObjectBundle -> Message MarloweTransfer 'StCanUpload 'StUpload
    MsgUploaded :: Map Label DatumHash -> Message MarloweTransfer 'StUpload 'StCanUpload
    MsgUploadFailed :: ImportError -> Message MarloweTransfer 'StUpload 'StIdle
    MsgImported :: Message MarloweTransfer 'StCanUpload 'StIdle
    MsgDownloaded :: ObjectBundle -> Message MarloweTransfer 'StDownload 'StCanDownload
    MsgDownload :: Natural -> Message MarloweTransfer 'StCanDownload 'StDownload
    MsgCancel :: Message MarloweTransfer 'StCanDownload 'StIdle
    MsgExported :: Message MarloweTransfer 'StDownload 'StIdle

  type StateAgency StIdle = ClientAgency
  type StateAgency StCanUpload = ClientAgency
  type StateAgency StCanDownload = ClientAgency
  type StateAgency StExport = ServerAgency
  type StateAgency StUpload = ServerAgency
  type StateAgency StDownload = ServerAgency
  type StateAgency StDone = NobodyAgency

  type StateToken = SMarloweTransfer

deriving instance Show (Message MarloweTransfer st st')
deriving instance Eq (Message MarloweTransfer st st')
deriving instance Ord (Message MarloweTransfer st st')


instance BinaryMessage MarloweTransfer where
  putMessage = \case
    SStIdle -> \case
      MsgStartImport -> putWord8 0
      MsgRequestExport hash -> do
        putWord8 1
        put hash
      MsgDone -> putWord8 2
    SStCanUpload -> \case
      MsgUpload bundle -> do
        putWord8 0
        put bundle
      MsgImported -> putWord8 1
    SStCanDownload -> \case
      MsgDownload i -> do
        putWord8 0
        put i
      MsgCancel -> putWord8 1
    SStExport -> \case
      MsgStartExport -> putWord8 0
      MsgContractNotFound -> putWord8 1
    SStUpload -> \case
      MsgUploaded hashes -> do
        putWord8 0
        put hashes
      MsgUploadFailed err -> do
        putWord8 1
        put err
    SStDownload -> \case
      MsgDownloaded bundle -> do
        putWord8 0
        put bundle
      MsgExported -> putWord8 1
    SStDone -> \_msg -> notActiveState SStDone

  getMessage = \case
    SStIdle -> do
      tag <- getWord8
      case tag of
        0 -> pure $ SomeMessage MsgStartImport
        1 -> SomeMessage . MsgRequestExport <$> get
        2 -> pure $ SomeMessage MsgDone
        _ -> fail $ "invalid tag byte " <> show tag
    SStCanUpload -> do
      tag <- getWord8
      case tag of
        0 -> SomeMessage . MsgUpload <$> get
        1 -> pure $ SomeMessage MsgImported
        _ -> fail $ "invalid tag byte " <> show tag
    SStCanDownload -> do
      tag <- getWord8
      case tag of
        0 -> SomeMessage . MsgDownload <$> get
        1 -> pure $ SomeMessage MsgCancel
        _ -> fail $ "invalid tag byte " <> show tag
    SStExport -> do
      tag <- getWord8
      case tag of
        0 -> pure $ SomeMessage MsgStartExport
        1 -> pure $ SomeMessage MsgContractNotFound
        _ -> fail $ "invalid tag byte " <> show tag
    SStUpload -> do
      tag <- getWord8
      case tag of
        0 -> SomeMessage . MsgUploaded <$> get
        1 -> SomeMessage . MsgUploadFailed <$> get
        _ -> fail $ "invalid tag byte " <> show tag
    SStDownload -> do
      tag <- getWord8
      case tag of
        0 -> SomeMessage . MsgDownloaded <$> get
        1 -> pure $ SomeMessage MsgExported
        _ -> fail $ "invalid tag byte " <> show tag
    SStDone -> notActiveState SStDone

