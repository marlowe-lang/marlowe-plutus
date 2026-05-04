module Log.Backend.StandardOutputInlined (
  withStdOutInlinedLogger,
)
where

import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import Data.Aeson (Value (..), encode)
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Log (LogMessage (..), Logger, mkLogger, showLogLevel)
import Log.Internal.Logger (withLogger)
import System.IO (hFlush, stdout)

-- | Render a 'LogMessage' to 'Text'.
showLogMessage
  :: Maybe UTCTime
  -- ^ The time that message was added to the log.
  -> LogMessage
  -- ^ The actual message.
  -> Text
showLogMessage mInsertionTime LogMessage{..} =
  T.concat $
    [ T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" lmTime
    , case mInsertionTime of
        Nothing -> " "
        Just it -> T.pack $ formatTime defaultTimeLocale " (%H:%M:%S) " it
    , T.toUpper $ showLogLevel lmLevel
    , " "
    , T.intercalate "/" $ lmComponent : lmDomain
    , ": "
    , lmMessage
    ]
      ++ if lmData == emptyObject
        then []
        else [" ", textifyData lmData]
  where
    textifyData :: Value -> T.Text
    textifyData = T.decodeUtf8 . toStrict . encode

-- | Create a logger that prints messages to standard output for the duration of
-- the given action but on single line, in contrast with `withStdOutLogger` which
-- uses pretty printing for json data.
withStdOutInlinedLogger :: (MonadUnliftIO m) => (Logger -> m r) -> m r
withStdOutInlinedLogger act = withRunInIO $ \unlift -> do
  logger <- mkLogger "stdout" $ \msg -> do
    T.putStrLn $ showLogMessage Nothing msg
    hFlush stdout
  withLogger logger (unlift . act)

