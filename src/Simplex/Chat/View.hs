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
import Simplex.Chat.Store (StoreError (..))
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

showContactDeleted :: ChatReader m => ContactName -> m ()
showContactDeleted = printToView . contactDeleted

showContactConnected :: ChatReader m => ContactName -> m ()
showContactConnected = printToView . contactConnected

showContactDisconnected :: ChatReader m => ContactName -> m ()
showContactDisconnected = printToView . contactDisconnected

showReceivedMessage :: ChatReader m => ContactName -> UTCTime -> Text -> MsgIntegrity -> m ()
showReceivedMessage c utcTime msg mOk = printToView =<< liftIO (receivedMessage c utcTime msg mOk)

showSentMessage :: ChatReader m => ContactName -> ByteString -> m ()
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

contactDeleted :: ContactName -> [StyledString]
contactDeleted c = [ttyContact c <> " is deleted"]

contactConnected :: ContactName -> [StyledString]
contactConnected c = [ttyContact c <> " is connected"]

contactDisconnected :: ContactName -> [StyledString]
contactDisconnected c = ["disconnected from " <> ttyContact c <> " - restart chat"]

groupCreated :: GroupProfile -> [StyledString]
groupCreated GroupProfile {displayName, fullName} = ["group " <> ttyGroup displayName <> " (" <> plain fullName <> ") is created"]

receivedMessage :: ContactName -> UTCTime -> Text -> MsgIntegrity -> IO [StyledString]
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

sentMessage :: ContactName -> ByteString -> IO [StyledString]
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
  ChatErrorStore err -> case err of
    SEDuplicateName -> ["this display name is already used by user, contact or group"]
    SEContactNotFound c -> ["no contact " <> ttyContact c]
    SEContactNotReady c -> ["contact " <> ttyContact c <> " is not active yet"]
    e -> ["chat db error: " <> plain (show e)]
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

ttyContact :: ContactName -> StyledString
ttyContact = styled (Colored Green)

ttyToContact :: ContactName -> StyledString
ttyToContact c = styled (Colored Cyan) $ "@" <> c <> " "

ttyFromContact :: ContactName -> StyledString
ttyFromContact c = styled (Colored Yellow) $ c <> "> "

ttyGroup :: GroupName -> StyledString
ttyGroup g = styled (Colored Blue) $ "#" <> g

-- ttyFromGroup :: Group -> Contact -> StyledString
-- ttyFromGroup (Group g) (Contact a) = styled (Colored Yellow) $ "#" <> g <> " " <> a <> "> "

styleTime :: String -> StyledString
styleTime = Styled [SetColor Foreground Vivid Black]
