{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.View
  ( printToView,
    showInvitation,
    showChatError,
    showContactDeleted,
    showContactConnected,
    showContactDisconnected,
    showReceivedMessage,
    showSentMessage,
    showGroupCreated,
    safeDecodeUtf8,
  )
where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (DiffTime, UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (TimeZone, ZonedTime, getCurrentTimeZone, getZonedTime, localDay, localTimeOfDay, timeOfDayToTime, utcToLocalTime, zonedTimeToLocalTime)
import Simplex.Chat.Controller
import Simplex.Chat.Markdown
import Simplex.Chat.Styled
import Simplex.Chat.Terminal (printToTerminal)
import Simplex.Chat.Types
import Simplex.Chat.Util (safeDecodeUtf8)
import Simplex.Messaging.Agent.Protocol
import System.Console.ANSI.Types

type ChatReader m = (MonadUnliftIO m, MonadReader ChatController m)

showInvitation :: ChatReader m => SMPQueueInfo -> m ()
showInvitation = printToView . invitation

showChatError :: ChatReader m => ChatError -> m ()
showChatError = printToView . chatError

showContactDeleted :: ChatReader m => ContactRef -> m ()
showContactDeleted = printToView . contactDeleted

showContactConnected :: ChatReader m => ContactRef -> m ()
showContactConnected = printToView . contactConnected

showContactDisconnected :: ChatReader m => ContactRef -> m ()
showContactDisconnected = printToView . contactDisconnected

showReceivedMessage :: ChatReader m => ContactRef -> UTCTime -> Text -> MsgIntegrity -> m ()
showReceivedMessage c utcTime msg mOk = printToView =<< liftIO (receivedMessage c utcTime msg mOk)

showSentMessage :: ChatReader m => ContactRef -> ByteString -> m ()
showSentMessage c msg = printToView =<< liftIO (sentMessage c msg)

showGroupCreated :: ChatReader m => GroupProfile -> m ()
showGroupCreated = printToView . groupCreated

invitation :: SMPQueueInfo -> [StyledString]
invitation qInfo =
  [ "pass this invitation to your contact (via another channel): ",
    "",
    (plain . serializeSmpQueueInfo) qInfo,
    "",
    "and ask them to connect: /c <name_for_you> <invitation_above>"
  ]

contactDeleted :: ContactRef -> [StyledString]
contactDeleted c = [ttyContact c <> " is deleted"]

contactConnected :: ContactRef -> [StyledString]
contactConnected c = [ttyContact c <> " is connected"]

contactDisconnected :: ContactRef -> [StyledString]
contactDisconnected c = ["disconnected from " <> ttyContact c <> " - restart chat"]

groupCreated :: GroupProfile -> [StyledString]
groupCreated GroupProfile {groupRef, displayName} = ["group " <> ttyGroup groupRef <> " (" <> plain displayName <> ") is created"]

receivedMessage :: ContactRef -> UTCTime -> Text -> MsgIntegrity -> IO [StyledString]
receivedMessage c utcTime msg mOk = do
  t <- formatUTCTime <$> getCurrentTimeZone <*> getZonedTime
  pure $ prependFirst (t <> " " <> ttyFromContact c) (msgPlain msg) ++ showIntegrity mOk
  where
    formatUTCTime :: TimeZone -> ZonedTime -> StyledString
    formatUTCTime localTz currentTime =
      let localTime = utcToLocalTime localTz utcTime
          format =
            if (localDay localTime < localDay (zonedTimeToLocalTime currentTime))
              && (timeOfDayToTime (localTimeOfDay localTime) > (6 * 60 * 60 :: DiffTime))
              then "%m-%d" -- if message is from yesterday or before and 6 hours has passed since midnight
              else "%H:%M"
       in styleTime $ formatTime defaultTimeLocale format localTime
    showIntegrity :: MsgIntegrity -> [StyledString]
    showIntegrity MsgOk = []
    showIntegrity (MsgError err) = msgError $ case err of
      MsgSkipped fromId toId ->
        "skipped message ID " <> show fromId
          <> if fromId == toId then "" else ".." <> show toId
      MsgBadId msgId -> "unexpected message ID " <> show msgId
      MsgBadHash -> "incorrect message hash"
      MsgDuplicate -> "duplicate message ID"
    msgError :: String -> [StyledString]
    msgError s = [styled (Colored Red) s]

sentMessage :: ContactRef -> ByteString -> IO [StyledString]
sentMessage c msg = do
  time <- formatTime defaultTimeLocale "%H:%M" <$> getZonedTime
  pure $ prependFirst (styleTime time <> " " <> ttyToContact c) (msgPlain $ safeDecodeUtf8 msg)

prependFirst :: StyledString -> [StyledString] -> [StyledString]
prependFirst s [] = [s]
prependFirst s (s' : ss) = (s <> s') : ss

msgPlain :: Text -> [StyledString]
msgPlain = map styleMarkdownText . T.lines

chatError :: ChatError -> [StyledString]
chatError = \case
  ChatErrorContact e -> case e of
    CENotFound c -> ["no contact " <> ttyContact c]
  ChatErrorGroup e -> case e of
    GEDuplicateGroup -> ["group with this alias already exists"]
  ChatErrorAgent err -> case err of
    -- CONN e -> case e of
    --   -- TODO replace with ChatErrorContact errors, these errors should never happen
    --   NOT_FOUND -> ["no contact " <> ttyContact c]
    --   DUPLICATE -> ["contact " <> ttyContact c <> " already exists"]
    --   SIMPLEX -> ["contact " <> ttyContact c <> " did not accept invitation yet"]
    e -> ["smp agent error: " <> plain (show e)]
  e -> ["chat error: " <> plain (show e)]

printToView :: (MonadUnliftIO m, MonadReader ChatController m) => [StyledString] -> m ()
printToView s = asks chatTerminal >>= liftIO . (`printToTerminal` s)

ttyContact :: ContactRef -> StyledString
ttyContact = styled (Colored Green)

ttyToContact :: ContactRef -> StyledString
ttyToContact c = styled (Colored Cyan) $ "@" <> c <> " "

ttyFromContact :: ContactRef -> StyledString
ttyFromContact c = styled (Colored Yellow) $ c <> "> "

ttyGroup :: GroupRef -> StyledString
ttyGroup g = styled (Colored Blue) $ "#" <> g

-- ttyFromGroup :: Group -> Contact -> StyledString
-- ttyFromGroup (Group g) (Contact a) = styled (Colored Yellow) $ "#" <> g <> " " <> a <> "> "

styleTime :: String -> StyledString
styleTime = Styled [SetColor Foreground Vivid Black]
