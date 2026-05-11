module Marlowe.ContractStore.Protocol.Codec where

import Control.Exception (Exception)
import Control.Monad (mfilter)
import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Network.TypedProtocol (Message, Protocol)
import Network.TypedProtocol.Codec

class (Protocol ps) => BinaryMessage ps where
  -- => ActiveState st
  putMessage
    :: forall (st :: ps) (st' :: ps)
     . ActiveState st
    => StateToken st
    -> Message ps st st'
    -> B.Put

  getMessage
    :: forall (st :: ps)
     . ActiveState st
    => StateToken st
    -> B.Get (SomeMessage st)


data DeserializeError = DeserializeError
  { message :: !String
  , offset :: !B.ByteOffset
  , unconsumedInput :: !BS.ByteString
  }
  deriving (Show)

instance Exception DeserializeError

binaryCodec
  :: forall ps m
   . Applicative m
  => BinaryMessage ps
  => Codec ps DeserializeError m LBS.ByteString
binaryCodec = Codec encode (decodeGet . getMessage)
  where
    encode
      :: forall (st :: ps) (st' :: ps)
       . ActiveState st
      => StateTokenI st
      => Message ps st st'
      -> LBS.ByteString
    encode = B.runPut . putMessage (stateToken :: StateToken st)

encodePut :: (a -> B.Put) -> a -> LBS.ByteString
encodePut = fmap B.runPut

decodeGet :: (Applicative m) => B.Get a -> m (DecodeStep LBS.ByteString DeserializeError m a)
decodeGet = go . B.runGetIncremental
  where
    go =
      pure . \case
        B.Fail unconsumedInput offset message -> DecodeFail DeserializeError{..}
        B.Partial f -> DecodePartial $ go . f . fmap LBS.toStrict
        B.Done unconsumedInput _ a -> DecodeDone a $ mfilter (not . LBS.null) $ Just $ LBS.fromStrict unconsumedInput
