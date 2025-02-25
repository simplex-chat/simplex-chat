{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Directory.Options
import Directory.Service
import Directory.Store
import Simplex.Chat.Core

main :: IO ()
main = do
  opts@DirectoryOpts {directoryLog, runCLI} <- welcomeGetOpts
  st <- restoreDirectoryStore directoryLog
  if runCLI
    then directoryServiceCLI st opts
    else do
      cfg <- directoryChatConfig opts
      simplexChatCore cfg (mkChatOpts opts) $ directoryService st opts
