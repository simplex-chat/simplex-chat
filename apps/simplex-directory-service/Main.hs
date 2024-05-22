{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Directory.Options
import Directory.Service
import Directory.Store
import Simplex.Chat.Core
import Simplex.Chat.Terminal (terminalChatConfig)

directoryCfg :: ChatConfig
directoryCfg =
  terminalChatConfig
    { agentConfig = (agentConfig terminalChatConfig) {persistErrorInterval = 30}
    }

main :: IO ()
main = do
  opts@DirectoryOpts {directoryLog} <- welcomeGetOpts
  st <- restoreDirectoryStore directoryLog
  simplexChatCore directoryCfg (mkChatOpts opts) $ directoryService st opts
