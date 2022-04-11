{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Bot where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Simplex.Chat.Controller
import Simplex.Chat.Core
import Simplex.Chat.Messages
import Simplex.Chat.Store
import Simplex.Chat.Types (Contact (..), User (..))
import Simplex.Messaging.Encoding.String (strEncode)
import System.Exit (exitFailure)

chatBotRepl :: String -> (String -> String) -> User -> ChatController -> IO ()
chatBotRepl welcome answer _user cc = do
  initializeBotAddress cc
  race_ (forever $ void getLine) . forever $ do
    (_, resp) <- atomically . readTBQueue $ outputQ cc
    case resp of
      CRContactConnected contact -> do
        contactConnected contact
        void $ sendMsg contact welcome
      CRNewChatItem (AChatItem _ SMDRcv (DirectChat contact) ChatItem {content}) -> do
        let msg = T.unpack $ ciContentToText content
        void . sendMsg contact $ answer msg
      _ -> pure ()
  where
    sendMsg Contact {contactId} msg = sendChatCmd cc $ "/_send @" <> show contactId <> " text " <> msg
    contactConnected Contact {localDisplayName} = putStrLn $ T.unpack localDisplayName <> " connected"

initializeBotAddress :: ChatController -> IO ()
initializeBotAddress cc = do
  sendChatCmd cc "/show_address" >>= \case
    CRUserContactLink uri _ -> showBotAddress uri
    CRChatCmdError (ChatErrorStore SEUserContactLinkNotFound) -> do
      putStrLn $ "No bot address, creating..."
      sendChatCmd cc "/address" >>= \case
        CRUserContactLinkCreated uri -> showBotAddress uri
        _ -> putStrLn "can't create bot address" >> exitFailure
    _ -> putStrLn "unexpected response" >> exitFailure
  where
    showBotAddress uri = do
      putStrLn $ "Bot's contact address is: " <> B.unpack (strEncode uri)
      void $ sendChatCmd cc "/auto_accept on"
