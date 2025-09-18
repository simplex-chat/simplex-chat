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
      env <- newServiceState opts
      let chatHooks =
            defaultChatHooks
              { postStartHook = Just $ directoryStartHook st opts,
                acceptMember = Just $ acceptMemberHook opts env
              }
      simplexChatCore (terminalChatConfig {chatHooks}) (mkChatOpts opts) $ directoryService st opts env
