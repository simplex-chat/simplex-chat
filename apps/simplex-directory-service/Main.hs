{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Maybe (isJust)
import Directory.Options
import Directory.Service
import Directory.Store.Migrate
import Simplex.Chat.Terminal (terminalChatConfig)

main :: IO ()
main = do
  opts@DirectoryOpts {runCLI} <- welcomeGetOpts
  if runCLI
    then directoryServiceCLI opts
    else directoryService opts terminalChatConfig
