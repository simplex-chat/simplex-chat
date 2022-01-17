{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Simplex.Chat
import Simplex.Chat.Controller (versionNumber)
import Simplex.Chat.Options
import System.Directory (getAppUserDataDirectory)
import System.Terminal (withTerminal)

main :: IO ()
main = do
  opts <- welcomeGetOpts
  t <- withTerminal pure
  simplexChatTerminal defaultChatConfig opts t

welcomeGetOpts :: IO ChatOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@ChatOpts {dbFile} <- getChatOpts appDir
  putStrLn $ "SimpleX Chat v" ++ versionNumber
  putStrLn $ "db: " <> dbFile <> "_chat.db, " <> dbFile <> "_agent.db"
  putStrLn "type \"/help\" or \"/h\" for usage info"
  pure opts
