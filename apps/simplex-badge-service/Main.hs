{-# LANGUAGE NamedFieldPuns #-}

module Main where

import BadgeService.Options (BadgeServiceOpts (..))
import BadgeService.Service
import Control.Logger.Simple (LogConfig (..), LogLevel (..), setLogLevel, withGlobalLogging)
import Simplex.Chat.Terminal (terminalChatConfig)

-- | Without this every logInfo, logWarn and logError in the service is discarded: the
-- library's sinks start empty, and the chat core only installs them under --log-agent.
main :: IO ()
main = withGlobalLogging LogConfig {lc_file = Nothing, lc_stderr = True} $ do
  -- quiet by default: the agent's per-connection info lines are noise. --log-agent (or -l debug)
  -- raises the level to info through the chat core.
  setLogLevel LogWarn
  opts@BadgeServiceOpts {runCLI} <- welcomeGetOpts
  if runCLI
    then badgeServiceCLI opts
    else newServiceState >>= badgeService opts terminalChatConfig
