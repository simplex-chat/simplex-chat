{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Bot where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Reader
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T
import Simplex.Chat.Controller
import Simplex.Chat.Core
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Protocol (MsgContent (..))
import Simplex.Chat.Store
import Simplex.Chat.Types (Contact (..), IsContact (..), User (..))
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Util (safeDecodeUtf8)
import System.Exit (exitFailure)

chatBotRepl :: String -> (Contact -> String -> IO String) -> User -> ChatController -> IO ()
chatBotRepl welcome answer _user cc = do
  initializeBotAddress cc
  race_ (forever $ void getLine) . forever $ do
    (_, resp) <- atomically . readTBQueue $ outputQ cc
    case resp of
      CRContactConnected _ contact _ -> do
        contactConnected contact
        void $ sendMsg contact welcome
      CRNewChatItem _ (AChatItem _ SMDRcv (DirectChat contact) ChatItem {content = mc@CIRcvMsgContent {}}) -> do
        let msg = T.unpack $ ciContentToText mc
        void $ sendMsg contact =<< answer contact msg
      _ -> pure ()
  where
    sendMsg Contact {contactId} msg = sendChatCmd cc $ "/_send @" <> show contactId <> " text " <> msg
    contactConnected Contact {localDisplayName} = putStrLn $ T.unpack localDisplayName <> " connected"

initializeBotAddress :: ChatController -> IO ()
initializeBotAddress cc = do
  sendChatCmd cc "/show_address" >>= \case
    CRUserContactLink _ UserContactLink {connReqContact} -> showBotAddress connReqContact
    CRChatCmdError _ (ChatErrorStore SEUserContactLinkNotFound) -> do
      putStrLn "No bot address, creating..."
      sendChatCmd cc "/address" >>= \case
        CRUserContactLinkCreated _ uri -> showBotAddress uri
        _ -> putStrLn "can't create bot address" >> exitFailure
    _ -> putStrLn "unexpected response" >> exitFailure
  where
    showBotAddress uri = do
      putStrLn $ "Bot's contact address is: " <> B.unpack (strEncode uri)
      void $ sendChatCmd cc "/auto_accept on"

sendMessage :: ChatController -> Contact -> String -> IO ()
sendMessage cc ct = sendComposedMessage cc ct Nothing . textMsgContent

sendComposedMessage :: ChatController -> Contact -> Maybe ChatItemId -> MsgContent -> IO ()
sendComposedMessage cc ct quotedItemId msgContent = do
  let cm = ComposedMessage {filePath = Nothing, quotedItemId, msgContent}
  sendChatCmd cc ("/_send @" <> show (contactId' ct) <> " json " <> jsonEncode cm) >>= \case
    CRNewChatItem {} -> printLog cc CLLInfo $ "sent message to " <> contactInfo ct
    r -> putStrLn $ "unexpected send message response: " <> show r
  where
    jsonEncode = T.unpack . safeDecodeUtf8 . LB.toStrict . J.encode

deleteMessage :: ChatController -> Contact -> ChatItemId -> IO ()
deleteMessage cc ct chatItemId = do
  let cmd = "/_delete item @" <> show (contactId' ct) <> " " <> show chatItemId <> " internal"
  sendChatCmd cc cmd >>= \case
    CRChatItemDeleted {} -> printLog cc CLLInfo $ "deleted message from " <> contactInfo ct
    r -> putStrLn $ "unexpected delete message response: " <> show r

textMsgContent :: String -> MsgContent
textMsgContent = MCText . T.pack

printLog :: ChatController -> ChatLogLevel -> String -> IO ()
printLog cc level s
  | logLevel (config cc) <= level = putStrLn s
  | otherwise = pure ()

contactInfo :: Contact -> String
contactInfo Contact {contactId, localDisplayName} = T.unpack localDisplayName <> " (" <> show contactId <> ")"
