{-# LANGUAGE OverloadedStrings #-}

-- | The SMP agent logs unconditionally at info on the global logger, so the service keeps its own info lines on this separate channel when that logger is raised to Warn.
module BadgeService.Log
  ( logInfo,
    logWarn,
    logError,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)
import GHC.Stack (CallStack, HasCallStack, callStack, getCallStack, srcLocFile, srcLocStartLine)
import System.IO (stderr)

logInfo :: (HasCallStack, MonadIO m) => Text -> m ()
logInfo = logAt "INFO" callStack

logWarn :: (HasCallStack, MonadIO m) => Text -> m ()
logWarn = logAt "WARN" callStack

logError :: (HasCallStack, MonadIO m) => Text -> m ()
logError = logAt "ERROR" callStack

logAt :: MonadIO m => Text -> CallStack -> Text -> m ()
logAt tag cs msg = liftIO $ do
  ts <- formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %z" <$> getZonedTime
  T.hPutStrLn stderr $ "[" <> tag <> " " <> T.pack ts <> " " <> loc <> "] " <> msg
  where
    loc = case getCallStack cs of
      ((_, l) : _) -> T.pack (srcLocFile l) <> ":" <> T.pack (show (srcLocStartLine l))
      [] -> "unknown"
