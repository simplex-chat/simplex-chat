{-# LANGUAGE NamedFieldPuns #-}

module Main where

import BadgeService.Options (BadgeServiceOpts (..))
import BadgeService.Service
import Control.Logger.Simple (LogConfig (..), withGlobalLogging)
import Simplex.Chat.Terminal (terminalChatConfig)

-- | Without this every logInfo, logWarn and logError in the service is discarded: the
-- library's sinks start empty, and the chat core only installs them under --log-agent.
main :: IO ()
main = withGlobalLogging LogConfig {lc_file = Nothing, lc_stderr = True} $ do
  opts@BadgeServiceOpts {runCLI} <- welcomeGetOpts
  if runCLI
    then badgeServiceCLI opts
    else newServiceState >>= badgeService opts terminalChatConfig
