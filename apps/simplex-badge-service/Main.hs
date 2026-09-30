{-# LANGUAGE NamedFieldPuns #-}

module Main where

import BadgeService.Options (BadgeServiceOpts (..))
import BadgeService.Service
import Control.Logger.Simple (LogConfig (..), LogLevel (..), setLogLevel, withGlobalLogging)
import Simplex.Chat.Terminal (terminalChatConfig)

-- | withGlobalLogging installs the SMP agent's log sinks, which the chat core otherwise installs only under --log-agent.
main :: IO ()
main = withGlobalLogging LogConfig {lc_file = Nothing, lc_stderr = True} $ do
  setLogLevel LogWarn
  opts@BadgeServiceOpts {runCLI} <- welcomeGetOpts
  if runCLI
    then badgeServiceCLI opts
    else newServiceState >>= badgeService opts terminalChatConfig
