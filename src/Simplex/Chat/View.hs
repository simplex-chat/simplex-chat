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
    showContactGroups,
    showContactConnected,
    showContactDisconnected,
    showContactAnotherClient,
    showContactSubscribed,
    showContactSubError,
    showGroupSubscribed,
    showGroupEmpty,
    showGroupRemoved,
    showMemberSubError,
    showReceivedMessage,
    showReceivedGroupMessage,
    showSentMessage,
    showSentGroupMessage,
    showSentFileInvitation,
    showSentGroupFileInvitation,
    showSentFileInfo,
    showSndFileStart,
    showSndFileComplete,
    showSndFileCancelled,
    showSndGroupFileCancelled,
    showSndFileRcvCancelled,
    receivedFileInvitation,
    showRcvFileAccepted,
    showRcvFileStart,
    showRcvFileComplete,
    showRcvFileCancelled,
    showRcvFileSndCancelled,
    showFileTransferStatus,
    showSndFileSubError,
    showRcvFileSubError,
    showGroupCreated,
    showGroupDeletedUser,
    showGroupDeleted,
    showSentGroupInvitation,
    showReceivedGroupInvitation,
    showJoinedGroupMember,
    showUserJoinedGroup,
    showJoinedGroupMemberConnecting,
    showConnectedToGroupMember,
    showDeletedMember,
    showDeletedMemberUser,
    showLeftMemberUser,
    showLeftMember,
    showGroupMembers,
    showContactsMerged,
    showUserProfile,
    showUserProfileUpdated,
    showContactUpdated,
    showMessageError,
    safeDecodeUtf8,
    msgPlain,
  )
where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.Composition ((.:), (.:.))
import Data.Function (on)
import Data.Int (Int64)
import Data.List (groupBy, intersperse, sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (DiffTime, UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (TimeZone, ZonedTime, getCurrentTimeZone, getZonedTime, localDay, localTimeOfDay, timeOfDayToTime, utcToLocalTime, zonedTimeToLocalTime)
import Numeric (showFFloat)
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

showContactGroups :: ChatReader m => ContactName -> [GroupName] -> m ()
showContactGroups = printToView .: contactGroups

showContactConnected :: ChatReader m => Contact -> m ()
showContactConnected = printToView . contactConnected

showContactDisconnected :: ChatReader m => ContactName -> m ()
showContactDisconnected = printToView . contactDisconnected

showContactAnotherClient :: ChatReader m => ContactName -> m ()
showContactAnotherClient = printToView . contactAnotherClient

showContactSubscribed :: ChatReader m => ContactName -> m ()
showContactSubscribed = printToView . contactSubscribed

showContactSubError :: ChatReader m => ContactName -> ChatError -> m ()
showContactSubError = printToView .: contactSubError

showGroupSubscribed :: ChatReader m => GroupName -> m ()
showGroupSubscribed = printToView . groupSubscribed

showGroupEmpty :: ChatReader m => GroupName -> m ()
showGroupEmpty = printToView . groupEmpty

showGroupRemoved :: ChatReader m => GroupName -> m ()
showGroupRemoved = printToView . groupRemoved

showMemberSubError :: ChatReader m => GroupName -> ContactName -> ChatError -> m ()
showMemberSubError = printToView .:. memberSubError

showReceivedMessage :: ChatReader m => ContactName -> UTCTime -> [StyledString] -> MsgIntegrity -> m ()
showReceivedMessage = showReceivedMessage_ . ttyFromContact

showReceivedGroupMessage :: ChatReader m => GroupName -> ContactName -> UTCTime -> [StyledString] -> MsgIntegrity -> m ()
showReceivedGroupMessage = showReceivedMessage_ .: ttyFromGroup

showReceivedMessage_ :: ChatReader m => StyledString -> UTCTime -> [StyledString] -> MsgIntegrity -> m ()
showReceivedMessage_ from utcTime msg mOk = printToView =<< liftIO (receivedMessage from utcTime msg mOk)

showSentMessage :: ChatReader m => ContactName -> ByteString -> m ()
showSentMessage = showSentMessage_ . ttyToContact

showSentGroupMessage :: ChatReader m => GroupName -> ByteString -> m ()
showSentGroupMessage = showSentMessage_ . ttyToGroup

showSentMessage_ :: ChatReader m => StyledString -> ByteString -> m ()
showSentMessage_ to msg = printToView =<< liftIO (sentMessage to msg)

showSentFileInvitation :: ChatReader m => ContactName -> FilePath -> m ()
showSentFileInvitation = showSentFileInvitation_ . ttyToContact

showSentGroupFileInvitation :: ChatReader m => GroupName -> FilePath -> m ()
showSentGroupFileInvitation = showSentFileInvitation_ . ttyToGroup

showSentFileInvitation_ :: ChatReader m => StyledString -> FilePath -> m ()
showSentFileInvitation_ to filePath = printToView =<< liftIO (sentFileInvitation to filePath)

showSentFileInfo :: ChatReader m => Int64 -> m ()
showSentFileInfo = printToView . sentFileInfo

showSndFileStart :: ChatReader m => SndFileTransfer -> m ()
showSndFileStart = printToView . sndFileStart

showSndFileComplete :: ChatReader m => SndFileTransfer -> m ()
showSndFileComplete = printToView . sndFileComplete

showSndFileCancelled :: ChatReader m => SndFileTransfer -> m ()
showSndFileCancelled = printToView . sndFileCancelled

showSndGroupFileCancelled :: ChatReader m => [SndFileTransfer] -> m ()
showSndGroupFileCancelled = printToView . sndGroupFileCancelled

showSndFileRcvCancelled :: ChatReader m => SndFileTransfer -> m ()
showSndFileRcvCancelled = printToView . sndFileRcvCancelled

showRcvFileAccepted :: ChatReader m => RcvFileTransfer -> FilePath -> m ()
showRcvFileAccepted = printToView .: rcvFileAccepted

showRcvFileStart :: ChatReader m => RcvFileTransfer -> m ()
showRcvFileStart = printToView . rcvFileStart

showRcvFileComplete :: ChatReader m => RcvFileTransfer -> m ()
showRcvFileComplete = printToView . rcvFileComplete

showRcvFileCancelled :: ChatReader m => RcvFileTransfer -> m ()
showRcvFileCancelled = printToView . rcvFileCancelled

showRcvFileSndCancelled :: ChatReader m => RcvFileTransfer -> m ()
showRcvFileSndCancelled = printToView . rcvFileSndCancelled

showFileTransferStatus :: ChatReader m => (FileTransfer, [Integer]) -> m ()
showFileTransferStatus = printToView . fileTransferStatus

showSndFileSubError :: ChatReader m => SndFileTransfer -> ChatError -> m ()
showSndFileSubError = printToView .: sndFileSubError

showRcvFileSubError :: ChatReader m => RcvFileTransfer -> ChatError -> m ()
showRcvFileSubError = printToView .: rcvFileSubError

showGroupCreated :: ChatReader m => Group -> m ()
showGroupCreated = printToView . groupCreated

showGroupDeletedUser :: ChatReader m => GroupName -> m ()
showGroupDeletedUser = printToView . groupDeletedUser

showGroupDeleted :: ChatReader m => GroupName -> GroupMember -> m ()
showGroupDeleted = printToView .: groupDeleted

showSentGroupInvitation :: ChatReader m => GroupName -> ContactName -> m ()
showSentGroupInvitation = printToView .: sentGroupInvitation

showReceivedGroupInvitation :: ChatReader m => Group -> ContactName -> GroupMemberRole -> m ()
showReceivedGroupInvitation = printToView .:. receivedGroupInvitation

showJoinedGroupMember :: ChatReader m => GroupName -> GroupMember -> m ()
showJoinedGroupMember = printToView .: joinedGroupMember

showUserJoinedGroup :: ChatReader m => GroupName -> m ()
showUserJoinedGroup = printToView . userJoinedGroup

showJoinedGroupMemberConnecting :: ChatReader m => GroupName -> GroupMember -> GroupMember -> m ()
showJoinedGroupMemberConnecting = printToView .:. joinedGroupMemberConnecting

showConnectedToGroupMember :: ChatReader m => GroupName -> GroupMember -> m ()
showConnectedToGroupMember = printToView .: connectedToGroupMember

showDeletedMember :: ChatReader m => GroupName -> Maybe GroupMember -> Maybe GroupMember -> m ()
showDeletedMember = printToView .:. deletedMember

showDeletedMemberUser :: ChatReader m => GroupName -> GroupMember -> m ()
showDeletedMemberUser = printToView .: deletedMemberUser

showLeftMemberUser :: ChatReader m => GroupName -> m ()
showLeftMemberUser = printToView . leftMemberUser

showLeftMember :: ChatReader m => GroupName -> GroupMember -> m ()
showLeftMember = printToView .: leftMember

showGroupMembers :: ChatReader m => Group -> m ()
showGroupMembers = printToView . groupMembers

showContactsMerged :: ChatReader m => Contact -> Contact -> m ()
showContactsMerged = printToView .: contactsMerged

showUserProfile :: ChatReader m => Profile -> m ()
showUserProfile = printToView . userProfile

showUserProfileUpdated :: ChatReader m => User -> User -> m ()
showUserProfileUpdated = printToView .: userProfileUpdated

showContactUpdated :: ChatReader m => Contact -> Contact -> m ()
showContactUpdated = printToView .: contactUpdated

showMessageError :: ChatReader m => Text -> Text -> m ()
showMessageError = printToView .: messageError

invitation :: SMPQueueInfo -> [StyledString]
invitation qInfo =
  [ "pass this invitation to your contact (via another channel): ",
    "",
    (plain . serializeSmpQueueInfo) qInfo,
    "",
    "and ask them to connect: " <> highlight' "/c <invitation_above>"
  ]

contactDeleted :: ContactName -> [StyledString]
contactDeleted c = [ttyContact c <> ": contact is deleted"]

contactGroups :: ContactName -> [GroupName] -> [StyledString]
contactGroups c gNames = [ttyContact c <> ": contact cannot be deleted, it is a member of the group(s) " <> ttyGroups gNames]
  where
    ttyGroups :: [GroupName] -> StyledString
    ttyGroups [] = ""
    ttyGroups [g] = ttyGroup g
    ttyGroups (g : gs) = ttyGroup g <> ", " <> ttyGroups gs

contactConnected :: Contact -> [StyledString]
contactConnected ct = [ttyFullContact ct <> ": contact is connected"]

contactDisconnected :: ContactName -> [StyledString]
contactDisconnected c = [ttyContact c <> ": disconnected from server (messages will be queued)"]

contactAnotherClient :: ContactName -> [StyledString]
contactAnotherClient c = [ttyContact c <> ": contact is connected to another client"]

contactSubscribed :: ContactName -> [StyledString]
contactSubscribed c = [ttyContact c <> ": connected to server"]

contactSubError :: ContactName -> ChatError -> [StyledString]
contactSubError c e = [ttyContact c <> ": contact error " <> sShow e]

groupSubscribed :: GroupName -> [StyledString]
groupSubscribed g = [ttyGroup g <> ": connected to server(s)"]

groupEmpty :: GroupName -> [StyledString]
groupEmpty g = [ttyGroup g <> ": group is empty"]

groupRemoved :: GroupName -> [StyledString]
groupRemoved g = [ttyGroup g <> ": you are no longer a member or group deleted"]

memberSubError :: GroupName -> ContactName -> ChatError -> [StyledString]
memberSubError g c e = [ttyGroup g <> " member " <> ttyContact c <> " error: " <> sShow e]

groupCreated :: Group -> [StyledString]
groupCreated g@Group {localDisplayName} =
  [ "group " <> ttyFullGroup g <> " is created",
    "use " <> highlight ("/a " <> localDisplayName <> " <name>") <> " to add members"
  ]

groupDeletedUser :: GroupName -> [StyledString]
groupDeletedUser g = groupDeleted_ g Nothing

groupDeleted :: GroupName -> GroupMember -> [StyledString]
groupDeleted g m = groupDeleted_ g (Just m) <> ["use " <> highlight ("/d #" <> g) <> " to delete the local copy of the group"]

groupDeleted_ :: GroupName -> Maybe GroupMember -> [StyledString]
groupDeleted_ g m = [ttyGroup g <> ": " <> memberOrUser m <> " deleted the group"]

sentGroupInvitation :: GroupName -> ContactName -> [StyledString]
sentGroupInvitation g c = ["invitation to join the group " <> ttyGroup g <> " sent to " <> ttyContact c]

receivedGroupInvitation :: Group -> ContactName -> GroupMemberRole -> [StyledString]
receivedGroupInvitation g@Group {localDisplayName} c role =
  [ ttyFullGroup g <> ": " <> ttyContact c <> " invites you to join the group as " <> plain (serializeMemberRole role),
    "use " <> highlight ("/j " <> localDisplayName) <> " to accept"
  ]

joinedGroupMember :: GroupName -> GroupMember -> [StyledString]
joinedGroupMember g m = [ttyGroup g <> ": " <> ttyMember m <> " joined the group "]

userJoinedGroup :: GroupName -> [StyledString]
userJoinedGroup g = [ttyGroup g <> ": you joined the group"]

joinedGroupMemberConnecting :: GroupName -> GroupMember -> GroupMember -> [StyledString]
joinedGroupMemberConnecting g host m = [ttyGroup g <> ": " <> ttyMember host <> " added " <> ttyFullMember m <> " to the group (connecting...)"]

connectedToGroupMember :: GroupName -> GroupMember -> [StyledString]
connectedToGroupMember g m = [ttyGroup g <> ": " <> connectedMember m <> " is connected"]

deletedMember :: GroupName -> Maybe GroupMember -> Maybe GroupMember -> [StyledString]
deletedMember g by m = [ttyGroup g <> ": " <> memberOrUser by <> " removed " <> memberOrUser m <> " from the group"]

deletedMemberUser :: GroupName -> GroupMember -> [StyledString]
deletedMemberUser g by = deletedMember g (Just by) Nothing <> groupPreserved g

leftMemberUser :: GroupName -> [StyledString]
leftMemberUser g = leftMember_ g Nothing <> groupPreserved g

leftMember :: GroupName -> GroupMember -> [StyledString]
leftMember g m = leftMember_ g (Just m)

leftMember_ :: GroupName -> Maybe GroupMember -> [StyledString]
leftMember_ g m = [ttyGroup g <> ": " <> memberOrUser m <> " left the group"]

groupPreserved :: GroupName -> [StyledString]
groupPreserved g = ["use " <> highlight ("/d #" <> g) <> " to delete the group"]

memberOrUser :: Maybe GroupMember -> StyledString
memberOrUser = maybe "you" ttyMember

connectedMember :: GroupMember -> StyledString
connectedMember m = case memberCategory m of
  GCPreMember -> "member " <> ttyFullMember m
  GCPostMember -> "new member " <> ttyMember m -- without fullName as as it was shown in joinedGroupMemberConnecting
  _ -> "member " <> ttyMember m -- these case is not used

groupMembers :: Group -> [StyledString]
groupMembers Group {membership, members} = map groupMember . filter (not . removedOrLeft) $ membership : members
  where
    removedOrLeft m = let s = memberStatus m in s == GSMemRemoved || s == GSMemLeft
    groupMember m = ttyFullMember m <> ": " <> role m <> ", " <> category m <> status m
    role = plain . serializeMemberRole . memberRole
    category m = case memberCategory m of
      GCUserMember -> "you, "
      GCInviteeMember -> "invited, "
      GCHostMember -> "host, "
      _ -> ""
    status m = case memberStatus m of
      GSMemRemoved -> "removed"
      GSMemLeft -> "left"
      GSMemInvited -> "not yet joined"
      GSMemConnected -> "connected"
      GSMemComplete -> "connected"
      GSMemCreator -> "created group"
      _ -> ""

contactsMerged :: Contact -> Contact -> [StyledString]
contactsMerged _to@Contact {localDisplayName = c1} _from@Contact {localDisplayName = c2} =
  [ "contact " <> ttyContact c2 <> " is merged into " <> ttyContact c1,
    "use " <> ttyToContact c1 <> highlight' "<message>" <> " to send messages"
  ]

userProfile :: Profile -> [StyledString]
userProfile Profile {displayName, fullName} =
  [ "user profile: " <> ttyFullName displayName fullName,
    "use " <> highlight' "/p <display name> [<full name>]" <> " to change it",
    "(the updated profile will be sent to all your contacts)"
  ]

userProfileUpdated :: User -> User -> [StyledString]
userProfileUpdated
  User {localDisplayName = n, profile = Profile {fullName}}
  User {localDisplayName = n', profile = Profile {fullName = fullName'}}
    | n == n' && fullName == fullName' = []
    | n == n' = ["user full name " <> (if T.null fullName' || fullName' == n' then "removed" else "changed to " <> plain fullName') <> notified]
    | otherwise = ["user profile is changed to " <> ttyFullName n' fullName' <> notified]
    where
      notified = " (your contacts are notified)"

contactUpdated :: Contact -> Contact -> [StyledString]
contactUpdated
  Contact {localDisplayName = n, profile = Profile {fullName}}
  Contact {localDisplayName = n', profile = Profile {fullName = fullName'}}
    | n == n' && fullName == fullName' = []
    | n == n' = ["contact " <> ttyContact n <> fullNameUpdate]
    | otherwise =
      [ "contact " <> ttyContact n <> " changed to " <> ttyFullName n' fullName',
        "use " <> ttyToContact n' <> highlight' "<message>" <> " to send messages"
      ]
    where
      fullNameUpdate = if T.null fullName' || fullName' == n' then " removed full name" else " updated full name: " <> plain fullName'

messageError :: Text -> Text -> [StyledString]
messageError prefix err = [plain prefix <> ": " <> plain err]

receivedMessage :: StyledString -> UTCTime -> [StyledString] -> MsgIntegrity -> IO [StyledString]
receivedMessage from utcTime msg mOk = do
  t <- formatUTCTime <$> getCurrentTimeZone <*> getZonedTime
  pure $ prependFirst (t <> " " <> from) msg ++ showIntegrity mOk
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

sentMessage :: StyledString -> ByteString -> IO [StyledString]
sentMessage to msg = sendWithTime_ to . msgPlain $ safeDecodeUtf8 msg

sentFileInvitation :: StyledString -> FilePath -> IO [StyledString]
sentFileInvitation to f = sendWithTime_ ("/f " <> to) [ttyFilePath f]

sendWithTime_ :: StyledString -> [StyledString] -> IO [StyledString]
sendWithTime_ to styledMsg = do
  time <- formatTime defaultTimeLocale "%H:%M" <$> getZonedTime
  pure $ prependFirst (styleTime time <> " " <> to) styledMsg

prependFirst :: StyledString -> [StyledString] -> [StyledString]
prependFirst s [] = [s]
prependFirst s (s' : ss) = (s <> s') : ss

msgPlain :: Text -> [StyledString]
msgPlain = map styleMarkdownText . T.lines

sentFileInfo :: Int64 -> [StyledString]
sentFileInfo fileId =
  ["use " <> highlight ("/fc " <> show fileId) <> " to cancel sending"]

sndFileStart :: SndFileTransfer -> [StyledString]
sndFileStart = sendingFile_ "started"

sndFileComplete :: SndFileTransfer -> [StyledString]
sndFileComplete = sendingFile_ "completed"

sndFileCancelled :: SndFileTransfer -> [StyledString]
sndFileCancelled = sendingFile_ "cancelled"

sndGroupFileCancelled :: [SndFileTransfer] -> [StyledString]
sndGroupFileCancelled fts =
  case filter (\SndFileTransfer {fileStatus = s} -> s /= FSCancelled && s /= FSComplete) fts of
    [] -> ["sending file can't be cancelled"]
    ts@(ft : _) -> ["cancelled sending " <> sndFile ft <> " to " <> listMembers ts]

sendingFile_ :: StyledString -> SndFileTransfer -> [StyledString]
sendingFile_ status ft@SndFileTransfer {recipientDisplayName = c} =
  [status <> " sending " <> sndFile ft <> " to " <> ttyContact c]

sndFileRcvCancelled :: SndFileTransfer -> [StyledString]
sndFileRcvCancelled ft@SndFileTransfer {recipientDisplayName = c} =
  [ttyContact c <> " cancelled receiving " <> sndFile ft]

sndFile :: SndFileTransfer -> StyledString
sndFile SndFileTransfer {fileId, fileName} = fileTransfer fileId fileName

receivedFileInvitation :: RcvFileTransfer -> [StyledString]
receivedFileInvitation RcvFileTransfer {fileId, fileInvitation = FileInvitation {fileName, fileSize}} =
  [ "sends file " <> ttyFilePath fileName <> " (" <> humanReadableSize fileSize <> " / " <> sShow fileSize <> " bytes)",
    "use " <> highlight ("/fr " <> show fileId <> " [<dir>/ | <path>]") <> " to receive it"
  ]

humanReadableSize :: Integer -> StyledString
humanReadableSize size
  | size < kB = sShow size <> " bytes"
  | size < mB = hrSize kB "KiB"
  | size < gB = hrSize mB "MiB"
  | otherwise = hrSize gB "GiB"
  where
    hrSize sB name = plain $ unwords [showFFloat (Just 1) (fromIntegral size / (fromIntegral sB :: Double)) "", name]
    kB = 1024
    mB = kB * 1024
    gB = mB * 1024

rcvFileAccepted :: RcvFileTransfer -> FilePath -> [StyledString]
rcvFileAccepted RcvFileTransfer {fileId, senderDisplayName = c} filePath =
  ["saving file " <> sShow fileId <> " from " <> ttyContact c <> " to " <> plain filePath]

rcvFileStart :: RcvFileTransfer -> [StyledString]
rcvFileStart = receivingFile_ "started"

rcvFileComplete :: RcvFileTransfer -> [StyledString]
rcvFileComplete = receivingFile_ "completed"

rcvFileCancelled :: RcvFileTransfer -> [StyledString]
rcvFileCancelled = receivingFile_ "cancelled"

receivingFile_ :: StyledString -> RcvFileTransfer -> [StyledString]
receivingFile_ status ft@RcvFileTransfer {senderDisplayName = c} =
  [status <> " receiving " <> rcvFile ft <> " from " <> ttyContact c]

rcvFileSndCancelled :: RcvFileTransfer -> [StyledString]
rcvFileSndCancelled ft@RcvFileTransfer {senderDisplayName = c} =
  [ttyContact c <> " cancelled sending " <> rcvFile ft]

rcvFile :: RcvFileTransfer -> StyledString
rcvFile RcvFileTransfer {fileId, fileInvitation = FileInvitation {fileName}} = fileTransfer fileId fileName

fileTransfer :: Int64 -> String -> StyledString
fileTransfer fileId fileName = "file " <> sShow fileId <> " (" <> ttyFilePath fileName <> ")"

fileTransferStatus :: (FileTransfer, [Integer]) -> [StyledString]
fileTransferStatus (FTSnd [ft@SndFileTransfer {fileStatus, fileSize, chunkSize}], chunksNum) =
  ["sending " <> sndFile ft <> " " <> sndStatus]
  where
    sndStatus = case fileStatus of
      FSNew -> "not accepted yet"
      FSAccepted -> "just started"
      FSConnected -> "progress " <> fileProgress chunksNum chunkSize fileSize
      FSComplete -> "complete"
      FSCancelled -> "cancelled"
fileTransferStatus (FTSnd [], _) = ["no file transfers (empty group)"]
fileTransferStatus (FTSnd fts@(ft : _), chunksNum) =
  case concatMap membersTransferStatus $ groupBy ((==) `on` fs) $ sortOn fs fts of
    [membersStatus] -> ["sending " <> sndFile ft <> " " <> membersStatus]
    membersStatuses -> ("sending " <> sndFile ft <> ": ") : map ("  " <>) membersStatuses
  where
    fs = fileStatus :: SndFileTransfer -> FileStatus
    membersTransferStatus [] = []
    membersTransferStatus ts@(SndFileTransfer {fileStatus, fileSize, chunkSize} : _) = [sndStatus <> ": " <> listMembers ts]
      where
        sndStatus = case fileStatus of
          FSNew -> "not accepted"
          FSAccepted -> "just started"
          FSConnected -> "in progress (" <> sShow (sum chunksNum * chunkSize * 100 `div` (toInteger (length chunksNum) * fileSize)) <> "%)"
          FSComplete -> "complete"
          FSCancelled -> "cancelled"
fileTransferStatus (FTRcv ft@RcvFileTransfer {fileId, fileInvitation = FileInvitation {fileSize}, fileStatus, chunkSize}, chunksNum) =
  ["receiving " <> rcvFile ft <> " " <> rcvStatus]
  where
    rcvStatus = case fileStatus of
      RFSNew -> "not accepted yet, use " <> highlight ("/fr " <> show fileId) <> " to receive file"
      RFSAccepted _ -> "just started"
      RFSConnected _ -> "progress " <> fileProgress chunksNum chunkSize fileSize
      RFSComplete RcvFileInfo {filePath} -> "complete, path: " <> plain filePath
      RFSCancelled RcvFileInfo {filePath} -> "cancelled, received part path: " <> plain filePath

listMembers :: [SndFileTransfer] -> StyledString
listMembers = mconcat . intersperse ", " . map (ttyContact . recipientDisplayName)

fileProgress :: [Integer] -> Integer -> Integer -> StyledString
fileProgress chunksNum chunkSize fileSize =
  sShow (sum chunksNum * chunkSize * 100 `div` fileSize) <> "% of " <> humanReadableSize fileSize

sndFileSubError :: SndFileTransfer -> ChatError -> [StyledString]
sndFileSubError SndFileTransfer {fileId, fileName} e =
  ["sent file " <> sShow fileId <> " (" <> plain fileName <> ") error: " <> sShow e]

rcvFileSubError :: RcvFileTransfer -> ChatError -> [StyledString]
rcvFileSubError RcvFileTransfer {fileId, fileInvitation = FileInvitation {fileName}} e =
  ["received file " <> sShow fileId <> " (" <> plain fileName <> ") error: " <> sShow e]

chatError :: ChatError -> [StyledString]
chatError = \case
  ChatError err -> case err of
    CEGroupDuplicateMember c -> ["contact " <> ttyContact c <> " is already in the group"]
    CEGroupDuplicateMemberId -> ["cannot add member - duplicate member ID"]
    CEGroupUserRole -> ["you have insufficient permissions for this group command"]
    CEGroupContactRole c -> ["contact " <> ttyContact c <> " has insufficient permissions for this group action"]
    CEGroupNotJoined g -> ["you did not join this group, use " <> highlight ("/join #" <> g)]
    CEGroupMemberNotActive -> ["you cannot invite other members yet, try later"]
    CEGroupMemberUserRemoved -> ["you are no longer the member of the group"]
    CEGroupMemberNotFound c -> ["contact " <> ttyContact c <> " is not a group member"]
    CEGroupInternal s -> ["chat group bug: " <> plain s]
    CEFileNotFound f -> ["file not found: " <> plain f]
    CEFileAlreadyReceiving f -> ["file is already accepted: " <> plain f]
    CEFileAlreadyExists f -> ["file already exists: " <> plain f]
    CEFileRead f e -> ["cannot read file " <> plain f, sShow e]
    CEFileWrite f e -> ["cannot write file " <> plain f, sShow e]
    CEFileSend fileId e -> ["error sending file " <> sShow fileId <> ": " <> sShow e]
    CEFileRcvChunk e -> ["error receiving file: " <> plain e]
    CEFileInternal e -> ["file error: " <> plain e]
  -- e -> ["chat error: " <> sShow e]
  ChatErrorStore err -> case err of
    SEDuplicateName -> ["this display name is already used by user, contact or group"]
    SEContactNotFound c -> ["no contact " <> ttyContact c]
    SEContactNotReady c -> ["contact " <> ttyContact c <> " is not active yet"]
    SEGroupNotFound g -> ["no group " <> ttyGroup g]
    SEGroupAlreadyJoined -> ["you already joined this group"]
    SEFileNotFound fileId -> fileNotFound fileId
    SESndFileNotFound fileId -> fileNotFound fileId
    SERcvFileNotFound fileId -> fileNotFound fileId
    e -> ["chat db error: " <> sShow e]
  ChatErrorAgent e -> ["smp agent error: " <> sShow e]
  ChatErrorMessage e -> ["chat message error: " <> sShow e]
  where
    fileNotFound fileId = ["file " <> sShow fileId <> " not found"]

printToView :: (MonadUnliftIO m, MonadReader ChatController m) => [StyledString] -> m ()
printToView s = asks chatTerminal >>= liftIO . (`printToTerminal` s)

ttyContact :: ContactName -> StyledString
ttyContact = styled (Colored Green)

ttyFullContact :: Contact -> StyledString
ttyFullContact Contact {localDisplayName, profile = Profile {fullName}} =
  ttyFullName localDisplayName fullName

ttyMember :: GroupMember -> StyledString
ttyMember GroupMember {localDisplayName} = ttyContact localDisplayName

ttyFullMember :: GroupMember -> StyledString
ttyFullMember GroupMember {localDisplayName, memberProfile = Profile {fullName}} =
  ttyFullName localDisplayName fullName

ttyFullName :: ContactName -> Text -> StyledString
ttyFullName c fullName = ttyContact c <> optFullName c fullName

ttyToContact :: ContactName -> StyledString
ttyToContact c = styled (Colored Cyan) $ "@" <> c <> " "

ttyFromContact :: ContactName -> StyledString
ttyFromContact c = styled (Colored Yellow) $ c <> "> "

ttyGroup :: GroupName -> StyledString
ttyGroup g = styled (Colored Blue) $ "#" <> g

ttyFullGroup :: Group -> StyledString
ttyFullGroup Group {localDisplayName, groupProfile = GroupProfile {fullName}} =
  ttyGroup localDisplayName <> optFullName localDisplayName fullName

ttyFromGroup :: GroupName -> ContactName -> StyledString
ttyFromGroup g c = styled (Colored Yellow) $ "#" <> g <> " " <> c <> "> "

ttyToGroup :: GroupName -> StyledString
ttyToGroup g = styled (Colored Cyan) $ "#" <> g <> " "

ttyFilePath :: FilePath -> StyledString
ttyFilePath = plain

optFullName :: ContactName -> Text -> StyledString
optFullName localDisplayName fullName
  | T.null fullName || localDisplayName == fullName = ""
  | otherwise = plain (" (" <> fullName <> ")")

highlight :: StyledFormat a => a -> StyledString
highlight = styled (Colored Cyan)

highlight' :: String -> StyledString
highlight' = highlight

styleTime :: String -> StyledString
styleTime = Styled [SetColor Foreground Vivid Black]
