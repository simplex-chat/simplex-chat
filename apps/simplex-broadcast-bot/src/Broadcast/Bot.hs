{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Broadcast.Bot where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as T
import Broadcast.Options
import Simplex.Chat.Bot
import Simplex.Chat.Bot.KnownContacts
import Simplex.Chat.Controller
import Simplex.Chat.Core
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Options
import Simplex.Chat.Protocol (MsgContent (..))
import Simplex.Chat.Types
import Simplex.Messaging.Util (tshow)
import System.Directory (getAppUserDataDirectory)

welcomeGetOpts :: IO BroadcastBotOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@BroadcastBotOpts {coreOptions} <- getBroadcastBotOpts appDir "simplex_status_bot"
  putStrLn $ "SimpleX Chat Bot v" ++ versionNumber
  printDbOpts coreOptions
  pure opts

broadcastBot :: BroadcastBotOpts -> User -> ChatController -> IO ()
broadcastBot BroadcastBotOpts {publishers, welcomeMessage, prohibitedMessage} _user cc = do
  initializeBotAddress cc
  race_ (forever $ void getLine) . forever $ do
    (_, _, resp) <- atomically . readTBQueue $ outputQ cc
    case resp of
      CRContactConnected _ ct _ -> do
        contactConnected ct
        sendMessage cc ct welcomeMessage
      CRNewChatItems {chatItems = (AChatItem _ SMDRcv (DirectChat ct) ci@ChatItem {content = CIRcvMsgContent mc}) : _}
        | publisher `elem` publishers ->
          if allowContent mc
            then do
              sendChatCmd cc ListContacts >>= \case
                CRContactsList _ cts -> void . forkIO $ do
                  sendChatCmd cc (SendMessageBroadcast mc) >>= \case
                    CRBroadcastSent {successes, failures} ->
                      sendReply $ "Forwarded to " <> tshow successes <> " contact(s), " <> tshow failures <> " errors"
                    r -> putStrLn $ "Error broadcasting message: " <> show r
                r -> putStrLn $ "Error getting contacts list: " <> show r
            else sendReply "!1 Message is not supported!"
        | otherwise -> do
          sendReply prohibitedMessage
          deleteMessage cc ct $ chatItemId' ci
        where
          sendReply = sendComposedMessage cc ct (Just $ chatItemId' ci) . MCText
          publisher = KnownContact {contactId = contactId' ct, localDisplayName = localDisplayName' ct}
          allowContent = \case
            MCText _ -> True
            MCLink {} -> True
            MCImage {} -> True
            _ -> False
          broadcastTo Contact {activeConn = Nothing} = False
          broadcastTo ct'@Contact {activeConn = Just conn@Connection {connStatus}} =
            (connStatus == ConnSndReady || connStatus == ConnReady)
              && not (connDisabled conn)
              && contactId' ct' /= contactId' ct
      _ -> pure ()
  where
    contactConnected ct = putStrLn $ T.unpack (localDisplayName' ct) <> " connected"
