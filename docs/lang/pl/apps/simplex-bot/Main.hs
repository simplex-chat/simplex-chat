{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Simplex.Chat.Bot
import Simplex.Chat.Controller (versionNumber)
import Simplex.Chat.Core
import Simplex.Chat.Options
import Simplex.Chat.Terminal (terminalChatConfig)
import System.Directory (getAppUserDataDirectory)
import Text.Read

main :: IO ()
main = do
  opts <- welcomeGetOpts
  simplexChatCore terminalChatConfig opts Nothing $
    chatBotRepl welcomeMessage $ \_contact msg ->
      pure $ case readMaybe msg :: Maybe Integer of
        Just n -> msg <> " * " <> msg <> " = " <> show (n * n)
        _ -> "\"" <> msg <> "\" nie jest liczbą"

welcomeMessage :: String
welcomeMessage = "Witam! Jestem prostym botem obliczającym kwadrat liczby.\nJeśli wyślesz mi jakąś liczbę, obliczę jej kwadrat"

welcomeGetOpts :: IO ChatOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@ChatOpts {coreOptions = CoreChatOpts {dbFilePrefix}} <- getChatOpts appDir "simplex_bot"
  putStrLn $ "SimpleX Chat Bot v" ++ versionNumber
  putStrLn $ "db: " <> dbFilePrefix <> "_chat.db, " <> dbFilePrefix <> "_agent.db"
  pure opts
