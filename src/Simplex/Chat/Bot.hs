{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}

module Simplex.Chat.Bot where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as L
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Simplex.Chat.Controller
import Simplex.Chat.Core
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Protocol (MsgContent (..))
import Simplex.Chat.Store
import Simplex.Chat.Types (Contact (..), ContactId, IsContact (..), User (..))
import Simplex.Messaging.Agent.Protocol (CreatedConnLink (..))
import Simplex.Messaging.Encoding.String (strEncode)
import System.Exit (exitFailure)

chatBotRepl :: String -> (Contact -> String -> IO String) -> User -> ChatController -> IO ()
chatBotRepl welcome answer _user cc = do
  initializeBotAddress cc
  race_ (forever $ void getLine) . forever $ do
    (_, event) <- atomically . readTBQueue $ outputQ cc
    case event of
      Right (CEvtContactConnected _ contact _) -> do
        contactConnected contact
        void $ sendMessage cc contact $ T.pack welcome
      Right CEvtNewChatItems {chatItems = (AChatItem _ SMDRcv (DirectChat contact) ChatItem {content = mc@CIRcvMsgContent {}}) : _} -> do
        let msg = T.unpack $ ciContentToText mc
        void $ sendMessage cc contact . T.pack =<< answer contact msg
      _ -> pure ()
  where
    contactConnected Contact {localDisplayName} = putStrLn $ T.unpack localDisplayName <> " connected"

initializeBotAddress :: ChatController -> IO ()
initializeBotAddress = initializeBotAddress' True

initializeBotAddress' :: Bool -> ChatController -> IO ()
initializeBotAddress' logAddress cc = do
  sendChatCmd cc ShowMyAddress >>= \case
    Right (CRUserContactLink _ UserContactLink {connLinkContact}) -> showBotAddress connLinkContact
    Left (ChatErrorStore SEUserContactLinkNotFound) -> do
      when logAddress $ putStrLn "No bot address, creating..."
      -- TODO [short links] create short link by default
      sendChatCmd cc (CreateMyAddress False) >>= \case
        Right (CRUserContactLinkCreated _ ccLink) -> showBotAddress ccLink
        _ -> putStrLn "can't create bot address" >> exitFailure
    _ -> putStrLn "unexpected response" >> exitFailure
  where
    showBotAddress (CCLink uri shortUri) = do
      when logAddress $ putStrLn $ "Bot's contact address is: " <> B.unpack (maybe (strEncode uri) strEncode shortUri)
      when (isJust shortUri) $ putStrLn $ "Full contact address for old clients: " <> B.unpack (strEncode uri)
      void $ sendChatCmd cc $ AddressAutoAccept $ Just AutoAccept {businessAddress = False, acceptIncognito = False, autoReply = Nothing}

sendMessage :: ChatController -> Contact -> Text -> IO ()
sendMessage cc ct = sendComposedMessage cc ct Nothing . MCText

sendMessage' :: ChatController -> ContactId -> Text -> IO ()
sendMessage' cc ctId = sendComposedMessage' cc ctId Nothing . MCText

sendComposedMessage :: ChatController -> Contact -> Maybe ChatItemId -> MsgContent -> IO ()
sendComposedMessage cc = sendComposedMessage' cc . contactId'

sendComposedMessage' :: ChatController -> ContactId -> Maybe ChatItemId -> MsgContent -> IO ()
sendComposedMessage' cc ctId qiId mc = sendComposedMessages_ cc (SRDirect ctId) [(qiId, mc)]

sendComposedMessages :: ChatController -> SendRef -> NonEmpty MsgContent -> IO ()
sendComposedMessages cc sendRef = sendComposedMessages_ cc sendRef . L.map (Nothing,)

sendComposedMessages_ :: ChatController -> SendRef -> NonEmpty (Maybe ChatItemId, MsgContent) -> IO ()
sendComposedMessages_ cc sendRef qmcs = do
  let cms = L.map (\(qiId, mc) -> ComposedMessage {fileSource = Nothing, quotedItemId = qiId, msgContent = mc, mentions = M.empty}) qmcs
  sendChatCmd cc (APISendMessages sendRef False Nothing cms) >>= \case
    Right (CRNewChatItems {}) -> printLog cc CLLInfo $ "sent " <> show (length cms) <> " messages to " <> show sendRef
    r -> putStrLn $ "unexpected send message response: " <> show r

deleteMessage :: ChatController -> Contact -> ChatItemId -> IO ()
deleteMessage cc ct chatItemId = do
  let cmd = APIDeleteChatItem (contactRef ct) [chatItemId] CIDMInternal
  sendChatCmd cc cmd >>= \case
    Right (CRChatItemsDeleted {}) -> printLog cc CLLInfo $ "deleted message(s) from " <> contactInfo ct
    r -> putStrLn $ "unexpected delete message response: " <> show r

contactRef :: Contact -> ChatRef
contactRef = ChatRef CTDirect . contactId'

printLog :: ChatController -> ChatLogLevel -> String -> IO ()
printLog cc level s
  | logLevel (config cc) <= level = putStrLn s
  | otherwise = pure ()

contactInfo :: Contact -> String
contactInfo Contact {contactId, localDisplayName} = T.unpack localDisplayName <> " (" <> show contactId <> ")"
