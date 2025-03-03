{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Directory.Options
import Directory.Service
import Directory.Store
import Simplex.Chat.Core
import Simplex.Chat.Terminal (terminalChatConfig)

main :: IO ()
main = do
  opts@DirectoryOpts {directoryLog, runCLI} <- welcomeGetOpts
  st <- restoreDirectoryStore directoryLog
  if runCLI
    then directoryServiceCLI st opts
    else simplexChatCore terminalChatConfig (mkChatOpts opts) $ directoryService st opts
