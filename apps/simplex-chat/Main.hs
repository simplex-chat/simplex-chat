{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Concurrent (threadDelay)
import Server
import Simplex.Chat
import Simplex.Chat.Controller (versionNumber)
import Simplex.Chat.Core
import Simplex.Chat.Options
import Simplex.Chat.Terminal
import Simplex.Chat.View (serializeChatResponse)
import System.Directory (getAppUserDataDirectory)
import System.Terminal (withTerminal)

main :: IO ()
main = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@ChatOpts {chatCmd, chatServerPort} <- getChatOpts appDir "simplex_v1"
  if null chatCmd
    then case chatServerPort of
      Just chatPort ->
        simplexChatServer defaultChatServerConfig {chatPort} defaultChatConfig opts
      _ -> do
        welcome opts
        t <- withTerminal pure
        simplexChatTerminal defaultChatConfig opts t
    else simplexChatCore defaultChatConfig opts Nothing $ \_ cc -> do
      r <- sendChatCmd cc chatCmd
      putStrLn $ serializeChatResponse r
      threadDelay $ chatCmdDelay opts * 1000000

welcome :: ChatOpts -> IO ()
welcome ChatOpts {dbFilePrefix} = do
  putStrLn $ "SimpleX Chat v" ++ versionNumber
  putStrLn $ "db: " <> dbFilePrefix <> "_chat.db, " <> dbFilePrefix <> "_agent.db"
  putStrLn "type \"/help\" or \"/h\" for usage info"
