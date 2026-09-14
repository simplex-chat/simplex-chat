{-# LANGUAGE NamedFieldPuns #-}

module Main where

import BadgeService.Options (BadgeServiceOpts (..))
import BadgeService.Service
import Control.Logger.Simple (LogConfig (..), LogLevel (..), setLogLevel, withGlobalLogging)
import Simplex.Chat.Terminal (terminalChatConfig)

-- | withGlobalLogging installs the sinks the SMP agent logs through; without it the chat core
-- only installs them under --log-agent. Warn keeps the agent's per-connection info chatter off
-- while still surfacing its faults; the service's own info lines print on BadgeService.Log.
main :: IO ()
main = withGlobalLogging LogConfig {lc_file = Nothing, lc_stderr = True} $ do
  setLogLevel LogWarn
  opts@BadgeServiceOpts {runCLI} <- welcomeGetOpts
  if runCLI
    then badgeServiceCLI opts
    else newServiceState >>= badgeService opts terminalChatConfig
