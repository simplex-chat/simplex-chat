{-# LANGUAGE LambdaCase #-}
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
    Just cmd -> migrate cmd opts terminalChatConfig
    Nothing -> do
      st <- openDirectoryLog directoryLog
      if runCLI
        then directoryServiceCLI st opts
        else directoryService st opts terminalChatConfig
  where
    migrate = \case
      MLCheck -> checkDirectoryLog
      MLImport -> importDirectoryLogToDB
      MLExport -> exportDBToDirectoryLog
      MLListing -> saveGroupListingFiles
