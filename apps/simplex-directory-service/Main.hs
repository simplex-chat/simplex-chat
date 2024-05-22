{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Directory.Options
import Directory.Service
import Directory.Store
import Simplex.Chat.Controller (ChatConfig (..))
import Simplex.Chat.Core
import Simplex.Chat.Terminal (terminalChatConfig)
import Simplex.Messaging.Agent.Env.SQLite (AgentConfig (..))

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
