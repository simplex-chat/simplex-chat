{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Reader
import qualified Data.Text as T
import Simplex.Chat.Bot
import Simplex.Chat.Controller
import Simplex.Chat.Core
import Simplex.Chat.Messages
import Simplex.Chat.Options
import Simplex.Chat.Terminal (terminalChatConfig)
import Simplex.Chat.Types
import System.Directory (getAppUserDataDirectory)
import Text.Read

main :: IO ()
main = do
  opts <- welcomeGetOpts
  simplexChatCore terminalChatConfig opts Nothing mySquaringBot

welcomeGetOpts :: IO ChatOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@ChatOpts {coreOptions = CoreChatOpts {dbFilePrefix}} <- getChatOpts appDir "simplex_bot"
  putStrLn $ "SimpleX Chat Bot v" ++ versionNumber
  putStrLn $ "db: " <> dbFilePrefix <> "_chat.db, " <> dbFilePrefix <> "_agent.db"
  pure opts

welcomeMessage :: String
welcomeMessage = "Witam! Jestem prostym botem obliczającym kwadrat liczby.\nJeśli wyślesz mi jakąś liczbę, obliczę jej kwadrat"

mySquaringBot :: User -> ChatController -> IO ()
mySquaringBot _user cc = do
  initializeBotAddress cc
  race_ (forever $ void getLine) . forever $ do
    (_, resp) <- atomically . readTBQueue $ outputQ cc
    case resp of
      CRContactConnected _ contact _ -> do
        contactConnected contact
        sendMessage cc contact welcomeMessage
      CRNewChatItem _ (AChatItem _ SMDRcv (DirectChat contact) ChatItem {content = mc@CIRcvMsgContent {}}) -> do
        let msg = T.unpack $ ciContentToText mc
            number_ = readMaybe msg :: Maybe Integer
        sendMessage cc contact $ case number_ of
          Just n -> msg <> " * " <> msg <> " = " <> show (n * n)
          _ -> "\"" <> msg <> "\" nie jest liczbą"
      _ -> pure ()
  where
    contactConnected Contact {localDisplayName} = putStrLn $ T.unpack localDisplayName <> " podłączony"
