{-# LANGUAGE NamedFieldPuns #-}

module Main where

import StoreService.Options (StoreServiceOpts (..))
import StoreService.Service
import Control.Logger.Simple (LogConfig (..), LogLevel (..), setLogLevel, withGlobalLogging)
import GHC.IO.Encoding (setLocaleEncoding)
import Simplex.Chat.Terminal (terminalChatConfig)
import System.IO (hSetEncoding, stderr, stdout, utf8)

-- | withGlobalLogging installs the SMP agent's log sinks, which the chat core otherwise installs only under --log-agent.
main :: IO ()
main = do
  -- Without a UTF-8 locale GHC reads the ini and writes logs as ASCII, and throws on a non-ASCII group name.
  setLocaleEncoding utf8
  mapM_ (`hSetEncoding` utf8) [stdout, stderr]
  withGlobalLogging LogConfig {lc_file = Nothing, lc_stderr = True} $ do
    setLogLevel LogWarn
    opts@StoreServiceOpts {runCLI} <- welcomeGetOpts
    if runCLI
      then storeServiceCLI opts
      else newServiceState >>= storeService opts terminalChatConfig
