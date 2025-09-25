{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Directory.Options
import Directory.Service
import Directory.Store
import Directory.Store.Migrate
import Simplex.Chat.Terminal (terminalChatConfig)

main :: IO ()
main = do
  opts@DirectoryOpts {directoryLog, migrateDirectoryLog, runCLI} <- welcomeGetOpts
  case migrateDirectoryLog of
    Just cmd -> case cmd of
      MLImport -> importLogToDB opts
      MLExport -> exportDBToLog opts
      MLCheck -> checkDBStoreLog opts
    Nothing -> do -- TODO do not restore directory log
      st <- restoreDirectoryStore directoryLog
      if runCLI
        then directoryServiceCLI st opts
        else directoryService st opts terminalChatConfig
