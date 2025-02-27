{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Directory.Options
import Directory.Service
import Directory.Store
import Simplex.Chat.Controller (ChatConfig (..), ChatHooks (..), defaultChatHooks)
import Simplex.Chat.Core
import Simplex.Chat.Terminal (terminalChatConfig)

main :: IO ()
main = do
  opts@DirectoryOpts {directoryLog, runCLI} <- welcomeGetOpts
  st <- restoreDirectoryStore directoryLog
  if runCLI
    then directoryServiceCLI st opts
    else do
      let cfg = terminalChatConfig {chatHooks = defaultChatHooks {acceptMember = Just acceptMemberHook}}
      simplexChatCore cfg (mkChatOpts opts) $ directoryService st opts
