{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Simplex.Chat
import Simplex.Chat.Bot
import Simplex.Chat.Controller (versionNumber)
import Simplex.Chat.Core
import Simplex.Chat.Options
import System.Directory (getAppUserDataDirectory)
import Text.Read

main :: IO ()
main = do
  opts <- welcomeGetOpts
  simplexChatCore defaultChatConfig opts Nothing $
    chatBotRepl "Hello! I am a simple squaring bot - if you send me a number, I will calculate its square" $ \msg ->
      case readMaybe msg :: Maybe Integer of
        Just n -> msg <> " * " <> msg <> " = " <> show (n * n)
        _ -> "\"" <> msg <> "\" is not a number"

welcomeGetOpts :: IO ChatOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@ChatOpts {dbFilePrefix} <- getChatOpts appDir "simplex_bot"
  putStrLn $ "SimpleX Chat Bot v" ++ versionNumber
  putStrLn $ "db: " <> dbFilePrefix <> "_chat.db, " <> dbFilePrefix <> "_agent.db"
  pure opts
