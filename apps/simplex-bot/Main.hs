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
  simplexChatCore terminalChatConfig opts $
    chatBotRepl welcomeMessage $ \_contact msg ->
      pure $ case readMaybe msg :: Maybe Integer of
        Just n -> msg <> " * " <> msg <> " = " <> show (n * n)
        _ -> "\"" <> msg <> "\" is not a number"

welcomeMessage :: String
welcomeMessage = "Hello! I am a simple squaring bot.\nIf you send me a number, I will calculate its square"

welcomeGetOpts :: IO ChatOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@ChatOpts {coreOptions} <- getChatOpts appDir "simplex_bot"
  putStrLn $ "SimpleX Chat Bot v" ++ versionNumber
  printDbOpts coreOptions
  pure opts
