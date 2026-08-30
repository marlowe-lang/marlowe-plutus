{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Runtime.Contract.TransferServer (
  ImportBundle,
  MainLabel (..),
  TransferServerDependencies (..),
  runTransferServer,
  runImport,
  mkImportBundle,
  merkleizeAndStoreContracts,
) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.RWS (RWST, evalRWST)
import qualified Control.Monad.Trans.RWS as RWS
import qualified Data.DList as DList
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Foldable (traverse_, find)
import Data.Maybe (mapMaybe)
import qualified Language.Marlowe.Object.Link as O
import qualified Language.Marlowe.Object.Types as O
import Language.Marlowe.Runtime.Contract.Api (ContractWithAdjacency (..))
import Language.Marlowe.Runtime.Contract.Store (ContractStagingArea (..), ContractStore (..))
import Language.Marlowe.Object.Types
    ( ContractHash(ContractHash, unContractHash),
      Label,
      ObjectBundle,
      LabelledObject(LabelledObject),
      ObjectType(ContractType),
      pattern SomeObjectType,
      ObjectBundle(ObjectBundle) )
import Marlowe.ContractStore.Protocol.Transfer.Server (
  ServerStCanDownload (..),
  ServerStCanUpload (..),
  ServerStExport (..),
  ServerStIdle (..),
  ServerStDownload (..),
  ServerStUpload (..),
 )
import Marlowe.ContractStore.Protocol.Transfer.Types (ImportError (..))
import Marlowe.Plutus.Semantics.Types qualified as Core
import qualified PlutusLedgerApi.V2 as PV2
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import Pipes (Pipe, await, yield, void)
import Data.Map (Map)
import Language.Marlowe.Object.Link (LinkError(TypeMismatch), linkBundle')
import Debug.Trace (traceM)

-- | Dependencies of the transfer server.
newtype TransferServerDependencies m = TransferServerDependencies
  { contractStore :: ContractStore m
  }

-- | Drive the transfer server algebra. Returns the initial `ServerStIdle`
-- state; the caller can then invoke `recvMsgStartImport`,
-- `recvMsgUpload`, etc. to feed bundles and observe results.
--
-- This is the web-server-friendly equivalent of the original
-- `transferServer :: TransferServerDependencies m -> ServerSource
-- MarloweTransferServer m ()`. The bodies of `idleServer`,
-- `uploadServer`, and `downloadServer` are preserved from the original.
runTransferServer
  :: forall m a
   . (MonadFail m)
  => TransferServerDependencies m
  -> m (ServerStIdle m a)
runTransferServer deps@TransferServerDependencies{contractStore} = do
  stage <- createContractStagingArea contractStore
  pure $ idleServer deps stage

idleServer :: forall m a. (MonadFail m) => TransferServerDependencies m -> ContractStagingArea m -> ServerStIdle m a
idleServer deps stage =
  ServerStIdle
    { recvMsgStartImport = pure $ uploadServer deps mempty stage
    , recvMsgRequestExport = \rootHash -> do
        hashesO <- getClosureInExportOrder (contractStore deps) rootHash
        let toContractHash (ContractHash bs) = ContractHash bs
        pure $
          maybe
            (SendMsgContractNotFound (idleServer deps stage))
            (SendMsgStartExport . downloadServer deps stage)
            (map toContractHash <$> hashesO)
    , recvMsgDone = error "runTransferServer: recvMsgDone reached; pass the resulting ServerStIdle back to your driver."
    }

uploadServer
  :: forall m a
   . (MonadFail m)
  => TransferServerDependencies m
  -> O.SymbolTable
  -> ContractStagingArea m
  -> ServerStCanUpload m a
uploadServer deps objects stage =
  ServerStCanUpload
    { recvMsgImported = do
        _ <- commit stage
        pure $ idleServer deps stage
    , recvMsgUpload = \bundle -> do
        result <-
          runExceptT $
            O.linkBundle' bundle (merkleizeAndStoreContracts stage) objects
        case result of
          Left err -> pure $ SendMsgUploadFailed err (idleServer deps stage)
          Right (Left err) -> pure $ SendMsgUploadFailed (LinkError err) (idleServer deps stage)
          Right (Right (linked, objects')) -> do
            _ <- flush stage
            _ <- commit stage
            pure $
              SendMsgUploaded (Map.fromList $ mapMaybe sequence linked) $
                uploadServer deps objects' stage
    }

downloadServer
  :: forall m a
   . MonadFail m
  => TransferServerDependencies m
  -> ContractStagingArea m
  -> [ContractHash]
  -> ServerStCanDownload m a
downloadServer deps stage hashes =
  ServerStCanDownload
    { recvMsgCancel = pure $ idleServer deps stage
    , recvMsgDownload = \i -> do
        let (batchHashes, hashes') = splitAt (fromIntegral i) hashes
        case batchHashes of
          [] -> pure $ SendMsgExported (idleServer deps stage)
          _ -> do
            let getHashContract (ContractHash bs) =
                  loadContract (contractStore deps) (ContractHash bs)
            batches <- traverse getHashContract batchHashes
            pure $
              SendMsgDownloaded (O.ObjectBundle batches) $
                downloadServer deps stage hashes'
    }

getClosureInExportOrder :: forall m. Monad m => ContractStore m -> ContractHash -> m (Maybe [O.ContractHash])
getClosureInExportOrder store rootHash = runMaybeT $ DList.toList . snd <$> evalRWST (writeClosureInExportOrder rootHash) () mempty
  where
    writeClosureInExportOrder :: ContractHash -> RWST () (DList.DList O.ContractHash) (Set.Set O.ContractHash) (MaybeT m) ()
    writeClosureInExportOrder hash = do
      visited <- RWS.get
      let hashO = O.fromCoreContractHash (BuiltinByteString (unContractHash hash))
      if Set.member hashO visited
        then pure ()
        else do
          ContractWithAdjacency{..} <- lift $ MaybeT $ getContract store hash
          let toContractHash (O.ContractHash bs) = ContractHash bs
          traverse_ writeClosureInExportOrder
            (Set.toList $ Set.map toContractHash adjacency)
          RWS.tell $ pure hashO
          RWS.modify $ Set.insert hashO

loadContract :: (MonadFail m) => ContractStore m -> ContractHash -> m O.LabelledObject
loadContract store hash = do
  Just ContractWithAdjacency{..} <- getContract store hash
  let hashO = O.fromCoreContractHash (BuiltinByteString (unContractHash hash))
  pure $ O.LabelledObject (O.Label $ T.pack $ show hashO) O.ContractType $ O.fromCoreContract contract

merkleizeAndStoreContracts
  :: (Monad m) => ContractStagingArea m -> O.LinkedObject -> ExceptT ImportError m (O.LinkedObject, Maybe ContractHash)
merkleizeAndStoreContracts stage = \case
  O.LinkedContract contract -> do
    merkleizedContract <- merkleizeAndStore stage contract
    hash <- lift $ stageContract stage merkleizedContract
    pure (O.LinkedContract merkleizedContract, Just hash)
  obj -> pure (obj, Nothing)

merkleizeAndStore :: (Monad m) => ContractStagingArea m -> Core.Contract -> ExceptT ImportError m Core.Contract
merkleizeAndStore stage = \case
  Core.Close -> pure Core.Close
  Core.Pay account payee token value contract -> Core.Pay account payee token value <$> merkleizeAndStore stage contract
  Core.If obs c1 c2 -> Core.If obs <$> merkleizeAndStore stage c1 <*> merkleizeAndStore stage c2
  Core.When cases timeout contract ->
    Core.When <$> traverse (merkleizeAndStoreCase stage) cases <*> pure timeout <*> merkleizeAndStore stage contract
  Core.Let valueId value contract -> Core.Let valueId value <$> merkleizeAndStore stage contract
  Core.Assert obs contract -> Core.Assert obs <$> merkleizeAndStore stage contract

merkleizeAndStoreCase
  :: (Monad m) => ContractStagingArea m -> Core.Case Core.Contract -> ExceptT ImportError m (Core.Case Core.Contract)
merkleizeAndStoreCase stage@ContractStagingArea{..} = \case
  Core.Case action Core.Close -> pure $ Core.Case action Core.Close
  Core.Case action contract -> do
    contract' <- merkleizeAndStore stage contract
    ContractHash hash <- lift $ stageContract contract'
    pure $ Core.MerkleizedCase action $ BuiltinByteString hash
  Core.MerkleizedCase action hash -> do
    exists <- lift $ doesContractExist $ ContractHash (PV2.fromBuiltin @PV2.BuiltinByteString hash)
    if exists
      then pure $ Core.MerkleizedCase action hash
      else throwE $ ContinuationNotInStore $ O.fromCoreContractHash hash

-- | One-shot wrapper around `runTransferServer`: opens a staging area,
-- links the bundle, merkleizes every linked contract, commits, and
-- returns the final `Map Label ContractHash`. This is the convenience entry
-- point the web handler uses to drive the import; the original
-- `transferServer` returned a `ServerSource` instead, but for the web
-- case a single-shot result is the right shape.
runImport
  :: forall m
   . (Monad m)
  => TransferServerDependencies m
  -> O.ObjectBundle
  -> m (Either ImportError (Map.Map O.Label ContractHash))
runImport TransferServerDependencies{contractStore} bundle = do
  stage <- createContractStagingArea contractStore
  result <-
    runExceptT $
      O.linkBundle' bundle (merkleizeAndStoreContracts stage) mempty
  case result of
    Left err -> pure (Left err)
    Right (Left err) -> pure (Left (LinkError err))
    Right (Right (linked, _)) -> do
      _ <- flush stage
      _ <- commit stage
      pure $ Right $ Map.fromList $ mapMaybe sequence linked

-- watchForMain :: (Monad m) => Label -> Pipe ObjectBundle BundlePart m (Either ImportError (Map Label DatumHash))
-- watchForMain main = do
--   ObjectBundle bundle <- await
--   case find ((main ==) . _label) bundle of
--     Nothing -> do
--       yield $ IntermediatePart $ ObjectBundle bundle
--       watchForMain main
--     Just (LabelledObject _ ContractType _) -> do
--       yield $ FinalPart $ ObjectBundle bundle
--       pure $ Right mempty
--     Just (LabelledObject _ t _) ->
--       pure $
--         Left $
--           LinkError $
--             TypeMismatch
--               (UnsafeSomeObjectType $ unsafeCoerce ContractType)
--               (UnsafeSomeObjectType $ unsafeCoerce t)
-- 
-- data BundlePart
--   = Intermediate
--   | Final
--   | MainTypeMismatch O.SomeObjectType O.SomeObjectType
--   deriving (Show, Eq)
--
newtype MainLabel = MainLabel {label :: Label}
  deriving (Show, Eq, Ord)


-- data Proxy a' a b' b m r
-- A Proxy is a monad transformer that receives and sends information on both an upstream and downstream interface.
-- 
-- The type variables signify:
-- 
-- a' and a - The upstream interface, where (a')s go out and (a)s come in
-- b' and b - The downstream interface, where (b)s go out and (b')s come in
-- m - The base monad
-- r - The return value
--
-- type Pipe a b = Proxy () a () b
-- type Producer b = Proxy X () () b
--
type ImportBundle m = MainLabel -> Pipe ObjectBundle (Map Label ContractHash) m (Either ImportError (Map Label ContractHash))

mkImportBundle
  :: Monad m
  => ContractStagingArea m
  -> ImportBundle m
mkImportBundle stage@ContractStagingArea{..} (MainLabel main) =
  loop mempty
  where
    loop objects = do
      bundle@(ObjectBundle bundleObjects) <- await
      case find (\LabelledObject { _label } -> _label == main) bundleObjects of
        Nothing ->
          step objects bundle >>= \case
            Left err ->
              pure (Left err)
            Right (hashes, objects') -> do
              yield hashes
              loop objects'

        Just (LabelledObject _ ContractType _) -> do
          step objects bundle >>= \case
            Left err ->
              pure (Left err)
            Right (hashes, _) -> do
              traceM "COMMITTING"
              lift $ void commit
              pure (Right hashes)

        Just (LabelledObject _ actual _) ->
          pure $ Left $ LinkError $ TypeMismatch (SomeObjectType ContractType) (SomeObjectType actual)

    step objects bundle = lift $ runExceptT $ do
      linked <- linkBundle' bundle (merkleizeAndStoreContracts stage) objects
      case linked of
        Left linkError ->
          throwE $ LinkError linkError
        Right (hashPairs, objects') -> do
          lift $ void flush
          pure (Map.fromList (mapMaybe sequence hashPairs), objects')

