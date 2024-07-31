{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}

module Simplex.Chat.Bot where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Simplex.Chat.Controller
import Simplex.Chat.Core
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Protocol (MsgContent (..))
import Simplex.Chat.Store
import Simplex.Chat.Types (Contact (..), ContactId, IsContact (..), User (..))
import Simplex.Messaging.Encoding.String (strEncode)
import System.Exit (exitFailure)

chatBotRepl :: String -> (Contact -> String -> IO String) -> User -> ChatController -> IO ()
chatBotRepl welcome answer _user cc = do
  initializeBotAddress cc
  race_ (forever $ void getLine) . forever $ do
    (_, _, resp) <- atomically . readTBQueue $ outputQ cc
    case resp of
      CRContactConnected _ contact _ -> do
        contactConnected contact
        void $ sendMessage cc contact welcome
      CRNewChatItem _ (AChatItem _ SMDRcv (DirectChat contact) ChatItem {content = mc@CIRcvMsgContent {}}) -> do
        let msg = T.unpack $ ciContentToText mc
        void $ sendMessage cc contact =<< answer contact msg
      _ -> pure ()
  where
    contactConnected Contact {localDisplayName} = putStrLn $ T.unpack localDisplayName <> " connected"

initializeBotAddress :: ChatController -> IO ()
initializeBotAddress = initializeBotAddress' True

initializeBotAddress' :: Bool -> ChatController -> IO ()
initializeBotAddress' logAddress cc = do
  sendChatCmd cc ShowMyAddress >>= \case
    CRUserContactLink _ UserContactLink {connReqContact} -> showBotAddress connReqContact
    CRChatCmdError _ (ChatErrorStore SEUserContactLinkNotFound) -> do
      when logAddress $ putStrLn "No bot address, creating..."
      sendChatCmd cc CreateMyAddress >>= \case
        CRUserContactLinkCreated _ uri -> showBotAddress uri
        _ -> putStrLn "can't create bot address" >> exitFailure
    _ -> putStrLn "unexpected response" >> exitFailure
  where
    showBotAddress uri = do
      when logAddress $ putStrLn $ "Bot's contact address is: " <> B.unpack (strEncode uri)
      void $ sendChatCmd cc $ AddressAutoAccept $ Just AutoAccept {acceptIncognito = False, autoReply = Nothing}

sendMessage :: ChatController -> Contact -> String -> IO ()
sendMessage cc ct = sendComposedMessage cc ct Nothing . textMsgContent

sendMessage' :: ChatController -> ContactId -> String -> IO ()
sendMessage' cc ctId = sendComposedMessage' cc ctId Nothing . textMsgContent

sendComposedMessage :: ChatController -> Contact -> Maybe ChatItemId -> MsgContent -> IO ()
sendComposedMessage cc = sendComposedMessage' cc . contactId'

sendComposedMessage' :: ChatController -> ContactId -> Maybe ChatItemId -> MsgContent -> IO ()
sendComposedMessage' cc ctId quotedItemId msgContent = do
  let cm = ComposedMessage {fileSource = Nothing, quotedItemId, msgContent}
  sendChatCmd cc (APISendMessage (ChatRef CTDirect ctId) False Nothing cm) >>= \case
    CRNewChatItem {} -> printLog cc CLLInfo $ "sent message to contact ID " <> show ctId
    r -> putStrLn $ "unexpected send message response: " <> show r

deleteMessage :: ChatController -> Contact -> ChatItemId -> IO ()
deleteMessage cc ct chatItemId = do
  let cmd = APIDeleteChatItem (contactRef ct) [chatItemId] CIDMInternal
  sendChatCmd cc cmd >>= \case
    CRChatItemsDeleted {} -> printLog cc CLLInfo $ "deleted message(s) from " <> contactInfo ct
    r -> putStrLn $ "unexpected delete message response: " <> show r

contactRef :: Contact -> ChatRef
contactRef = ChatRef CTDirect . contactId'

textMsgContent :: String -> MsgContent
textMsgContent = MCText . T.pack

printLog :: ChatController -> ChatLogLevel -> String -> IO ()
printLog cc level s
  | logLevel (config cc) <= level = putStrLn s
  | otherwise = pure ()

contactInfo :: Contact -> String
contactInfo Contact {contactId, localDisplayName} = T.unpack localDisplayName <> " (" <> show contactId <> ")"
