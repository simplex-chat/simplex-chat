{-# LANGUAGE OverloadedStrings #-}

-- | The SMP agent logs its per-connection traffic ("Agent connected", "A (1) --> ... SUB",
-- "subscribed N queues") as unconditional logInfo on the process-global Control.Logger.Simple
-- level, which has no per-source filter. Silencing it means raising that global level to Warn,
-- which would also drop the service's own info lines. So the service logs on this channel
-- instead: its own level, but Control.Logger.Simple's exact line format, so a service line and
-- an agent warning read the same in one stream.
module BadgeService.Log
  ( SvcLogLevel (..),
    setSvcLogLevel,
    logInfo,
    logWarn,
    logError,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)
import GHC.Stack (CallStack, HasCallStack, callStack, getCallStack, srcLocFile, srcLocStartLine)
import System.IO (stderr)
import System.IO.Unsafe (unsafePerformIO)

data SvcLogLevel = SvcDebug | SvcInfo | SvcWarn | SvcError
  deriving (Eq, Ord)

{-# NOINLINE svcLogLevel #-}
svcLogLevel :: IORef SvcLogLevel
svcLogLevel = unsafePerformIO (newIORef SvcInfo)

setSvcLogLevel :: MonadIO m => SvcLogLevel -> m ()
setSvcLogLevel = liftIO . writeIORef svcLogLevel

logInfo :: (HasCallStack, MonadIO m) => Text -> m ()
logInfo = logAt SvcInfo "INFO" callStack

logWarn :: (HasCallStack, MonadIO m) => Text -> m ()
logWarn = logAt SvcWarn "WARN" callStack

logError :: (HasCallStack, MonadIO m) => Text -> m ()
logError = logAt SvcError "ERROR" callStack

logAt :: MonadIO m => SvcLogLevel -> Text -> CallStack -> Text -> m ()
logAt level tag cs msg = liftIO $ do
  threshold <- readIORef svcLogLevel
  when (level >= threshold) $ do
    ts <- formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %z" <$> getZonedTime
    T.hPutStrLn stderr $ "[" <> tag <> " " <> T.pack ts <> " " <> loc <> "] " <> msg
  where
    loc = case getCallStack cs of
      ((_, l) : _) -> T.pack (srcLocFile l) <> ":" <> T.pack (show (srcLocStartLine l))
      [] -> "unknown"
