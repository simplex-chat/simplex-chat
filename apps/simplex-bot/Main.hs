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
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Simplex.Chat
import Simplex.Chat.Bot
import Simplex.Chat.Controller
import Simplex.Chat.Messages
import Simplex.Chat.Options
import Simplex.Chat.Store
import Simplex.Chat.Types
import Simplex.Messaging.Encoding.String
import System.Directory (getAppUserDataDirectory)
import System.Exit (exitFailure)
import Text.Read

main :: IO ()
main = do
  opts <- welcomeGetOpts
  simplexChatBot defaultChatConfig opts myChatBot

welcomeGetOpts :: IO ChatOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@ChatOpts {dbFilePrefix} <- getChatOpts appDir "simplex_bot"
  putStrLn $ "SimpleX Chat Bot v" ++ versionNumber
  putStrLn $ "db: " <> dbFilePrefix <> "_chat.db, " <> dbFilePrefix <> "_agent.db"
  pure opts

myChatBot :: User -> ChatController -> IO ()
myChatBot User {} cc@ChatController {outputQ} = do
  initializeBotAddress
  race_ (forever $ void getLine) . forever $ do
    (_, resp) <- atomically (readTBQueue outputQ)
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
    initializeBotAddress = do
      sendCmd "/show_address" >>= \case
        CRUserContactLink uri _ -> showBotAddress uri
        CRChatCmdError (ChatErrorStore SEUserContactLinkNotFound) -> do
          putStrLn $ "No bot address, creating..."
          sendCmd "/address" >>= \case
            CRUserContactLinkCreated uri -> showBotAddress uri
            _ -> putStrLn "can't create bot address" >> exitFailure
        _ -> putStrLn "unexpected response" >> exitFailure
    showBotAddress uri = do
      putStrLn $ "Bot's contact address is: " <> B.unpack (strEncode uri)
      void $ sendCmd "/auto_accept on"
    sendCmd s = runReaderT (execChatCommand $ B.pack s) cc
    sendMsg Contact {contactId} msg = sendCmd $ "/_send @" <> show contactId <> " text " <> msg
    contactConnected Contact {localDisplayName} = putStrLn $ T.unpack localDisplayName <> " connected"
