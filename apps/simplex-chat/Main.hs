{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Simplex.Chat
import Simplex.Chat.Controller (versionNumber)
import Simplex.Chat.Options
import Simplex.Chat.Terminal
import System.Directory (getAppUserDataDirectory)
import System.Terminal (withTerminal)

main :: IO ()
main = do
  opts <- welcomeGetOpts
  t <- withTerminal pure
  simplexChat defaultChatConfig opts t

welcomeGetOpts :: IO ChatOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@ChatOpts {dbFilePrefix} <- getChatOpts appDir "simplex_v1"
  putStrLn $ "SimpleX Chat v" ++ versionNumber
  putStrLn $ "db: " <> dbFilePrefix <> "_chat.db, " <> dbFilePrefix <> "_agent.db"
  putStrLn "type \"/help\" or \"/h\" for usage info"
  pure opts
