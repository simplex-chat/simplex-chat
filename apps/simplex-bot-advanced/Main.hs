{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Reader
import qualified Data.Text as T
import Simplex.Chat
import Simplex.Chat.Bot
import Simplex.Chat.Controller
import Simplex.Chat.Messages
import Simplex.Chat.Options
import Simplex.Chat.Types
import System.Directory (getAppUserDataDirectory)
import Text.Read

main :: IO ()
main = do
  opts <- welcomeGetOpts
  simplexChatBot defaultChatConfig opts mySquaringBot

welcomeGetOpts :: IO ChatOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@ChatOpts {dbFilePrefix} <- getChatOpts appDir "simplex_bot"
  putStrLn $ "SimpleX Chat Bot v" ++ versionNumber
  putStrLn $ "db: " <> dbFilePrefix <> "_chat.db, " <> dbFilePrefix <> "_agent.db"
  pure opts

mySquaringBot :: User -> ChatController -> IO ()
mySquaringBot _user cc = do
  initializeBotAddress cc
  race_ (forever $ void getLine) . forever $ do
    (_, resp) <- atomically . readTBQueue $ outputQ cc
    case resp of
      CRContactConnected contact -> do
        contactConnected contact
        void . sendMsg contact $ "Hello! I am a simple squaring bot - if you send me a number, I will calculate its square"
      CRNewChatItem (AChatItem _ SMDRcv (DirectChat contact) ChatItem {content}) -> do
        let msg = T.unpack $ ciContentToText content
            number_ = readMaybe msg :: Maybe Integer
        void . sendMsg contact $ case number_ of
          Nothing -> "\"" <> msg <> "\" is not a number"
          Just n -> msg <> " * " <> msg <> " = " <> show (n * n)
      _ -> pure ()
  where
    sendMsg Contact {contactId} msg = sendCmd cc $ "/_send @" <> show contactId <> " text " <> msg
    contactConnected Contact {localDisplayName} = putStrLn $ T.unpack localDisplayName <> " connected"
