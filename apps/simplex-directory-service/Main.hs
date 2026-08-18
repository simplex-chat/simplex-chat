{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Directory.Options
import Directory.Service
import Simplex.Chat.Terminal (terminalChatConfig)

main :: IO ()
main = do
  opts@DirectoryOpts {runCLI} <- welcomeGetOpts
  if runCLI
    then directoryServiceCLI opts
    else directoryService opts terminalChatConfig
