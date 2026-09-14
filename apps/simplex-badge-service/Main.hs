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
  -- info by default, so the service's own operational lines are visible: webhook deliveries, invoice
  -- settlement, the boot summary. The agent's per-connection noise is not among them; its sinks stay
  -- empty until --log-agent installs them, so info here does not turn that on.
  setLogLevel LogInfo
  opts@BadgeServiceOpts {runCLI} <- welcomeGetOpts
  if runCLI
    then badgeServiceCLI opts
    else newServiceState >>= badgeService opts terminalChatConfig
