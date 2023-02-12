{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Reader
import qualified Data.Text as T
import Options
import Simplex.Chat.Bot
import Simplex.Chat.Controller
import Simplex.Chat.Core
import Simplex.Chat.Messages
import Simplex.Chat.Options
import Simplex.Chat.Terminal (terminalChatConfig)
import Simplex.Chat.Types
import System.Directory (getAppUserDataDirectory)

main :: IO ()
main = do
  opts@BroadcastBotOpts {chatOptions} <- welcomeGetOpts
  simplexChatCore terminalChatConfig chatOptions Nothing $ broadcastBot opts

welcomeGetOpts :: IO BroadcastBotOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@BroadcastBotOpts {chatOptions = ChatOpts {dbFilePrefix}} <- getBroadcastBotOpts appDir "simplex_status_bot"
  putStrLn $ "SimpleX Chat Bot v" ++ versionNumber
  putStrLn $ "db: " <> dbFilePrefix <> "_chat.db, " <> dbFilePrefix <> "_agent.db"
  pure opts

broadcastBot :: BroadcastBotOpts -> User -> ChatController -> IO ()
broadcastBot BroadcastBotOpts {publishers, welcomeMessage, prohibitedMessage} _user cc = do
  initializeBotAddress cc
  race_ (forever $ void getLine) . forever $ do
    (_, resp) <- atomically . readTBQueue $ outputQ cc
    case resp of
      CRContactConnected _ contact _ -> do
        contactConnected contact
        sendMsg contact welcomeMessage
      CRNewChatItem _ (AChatItem _ SMDRcv (DirectChat ct) ChatItem {content = mc@CIRcvMsgContent {}})
        | publisher ct `elem` publishers -> do
          sendChatCmd cc "/contacts" >>= \case
            CRContactsList _ cts -> do
              let cts' = filter (broadcastFrom $ publisher ct) cts
              forM_ cts' $ \ct' -> sendMsg ct' $ T.unpack (ciContentToText mc)
              sendMsg ct $ "Your message is sent to " <> show (length cts') <> " contact(s)"
            r -> putStrLn $ "Error getting contacts list: " <> show r
        | otherwise ->
          sendMsg ct prohibitedMessage
      _ -> pure ()
  where
    broadcastFrom Publisher {contactId} ct@Contact {activeConn = conn@Connection {connStatus}} =
      (connStatus == ConnSndReady || connStatus == ConnReady)
        && not (connDisabled conn)
        && contactId' ct /= contactId
    sendMsg ct msg = void $ sendChatCmd cc $ "/_send @" <> show (contactId' ct) <> " text " <> msg
    contactConnected ct = putStrLn $ T.unpack (localDisplayName' ct) <> " connected"
    publisher Contact {contactId, localDisplayName} = Publisher {contactId, localDisplayName}
