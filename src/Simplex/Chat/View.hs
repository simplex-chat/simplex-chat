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
    showSndFileStart,
    showSndFileComplete,
    showReceivedFileInvitation,
    showRcvFileAccepted,
    showRcvFileStart,
    showRcvFileComplete,
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
    safeDecodeUtf8,
  )
where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.Composition ((.:), (.:.))
import Data.Int (Int64)
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

showReceivedMessage :: ChatReader m => ContactName -> UTCTime -> Text -> MsgIntegrity -> m ()
showReceivedMessage = showReceivedMessage_ . ttyFromContact

showReceivedGroupMessage :: ChatReader m => GroupName -> ContactName -> UTCTime -> Text -> MsgIntegrity -> m ()
showReceivedGroupMessage = showReceivedMessage_ .: ttyFromGroup

showReceivedMessage_ :: ChatReader m => StyledString -> UTCTime -> Text -> MsgIntegrity -> m ()
showReceivedMessage_ from utcTime msg mOk = printToView =<< liftIO (receivedMessage from utcTime msg mOk)

showSentMessage :: ChatReader m => ContactName -> ByteString -> m ()
showSentMessage = showSentMessage_ . ttyToContact

showSentGroupMessage :: ChatReader m => GroupName -> ByteString -> m ()
showSentGroupMessage = showSentMessage_ . ttyToGroup

showSentMessage_ :: ChatReader m => StyledString -> ByteString -> m ()
showSentMessage_ to msg = printToView =<< liftIO (sentMessage to msg)

showSentFileInvitation :: ChatReader m => ContactName -> SndFileTransfer -> m ()
showSentFileInvitation = printToView .: sentFileInvitation

showSndFileStart :: ChatReader m => Int64 -> m ()
showSndFileStart = printToView . sndFileStart

showSndFileComplete :: ChatReader m => Int64 -> m ()
showSndFileComplete = printToView . sndFileComplete

showReceivedFileInvitation :: ChatReader m => ContactName -> RcvFileTransfer -> m ()
showReceivedFileInvitation = printToView .: receivedFileInvitation

showRcvFileAccepted :: ChatReader m => Int64 -> FilePath -> m ()
showRcvFileAccepted = printToView .: rcvFileAccepted

showRcvFileStart :: ChatReader m => Int64 -> m ()
showRcvFileStart = printToView . rcvFileStart

showRcvFileComplete :: ChatReader m => Int64 -> m ()
showRcvFileComplete = printToView . rcvFileComplete

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
contactDisconnected c = [ttyContact c <> ": contact is disconnected (messages will be queued)"]

contactAnotherClient :: ContactName -> [StyledString]
contactAnotherClient c = [ttyContact c <> ": contact is connected to another client"]

contactSubscribed :: ContactName -> [StyledString]
contactSubscribed c = [ttyContact c <> ": contact is active"]

contactSubError :: ContactName -> ChatError -> [StyledString]
contactSubError c e = [ttyContact c <> ": contact error " <> sShow e]

groupSubscribed :: GroupName -> [StyledString]
groupSubscribed g = [ttyGroup g <> ": group is active"]

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

receivedMessage :: StyledString -> UTCTime -> Text -> MsgIntegrity -> IO [StyledString]
receivedMessage from utcTime msg mOk = do
  t <- formatUTCTime <$> getCurrentTimeZone <*> getZonedTime
  pure $ prependFirst (t <> " " <> from) (msgPlain msg) ++ showIntegrity mOk
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
sentMessage to msg = do
  time <- formatTime defaultTimeLocale "%H:%M" <$> getZonedTime
  pure $ prependFirst (styleTime time <> " " <> to) (msgPlain $ safeDecodeUtf8 msg)

prependFirst :: StyledString -> [StyledString] -> [StyledString]
prependFirst s [] = [s]
prependFirst s (s' : ss) = (s <> s') : ss

msgPlain :: Text -> [StyledString]
msgPlain = map styleMarkdownText . T.lines

sentFileInvitation :: ContactName -> SndFileTransfer -> [StyledString]
sentFileInvitation cName SndFileTransfer {fileId, fileName} =
  [ "sent file offer: " <> sShow fileName <> " to " <> ttyContact cName,
    "use " <> highlight ("/fc " <> show fileId) <> " to cancel sending"
  ]

sndFileStart :: Int64 -> [StyledString]
sndFileStart fileId = ["started sending the file " <> sShow fileId]

sndFileComplete :: Int64 -> [StyledString]
sndFileComplete fileId = ["completed sending the file " <> sShow fileId]

receivedFileInvitation :: ContactName -> RcvFileTransfer -> [StyledString]
receivedFileInvitation c RcvFileTransfer {fileId, fileInvitation = FileInvitation {fileName, fileSize}} =
  [ ttyContact c <> " wants to send you the file " <> plain fileName <> " (" <> plain (humanReadableSize fileSize) <> ")",
    "use " <> highlight ("/fr " <> show fileId <> " [<dir>/ | <path>]") <> " to receive it"
  ]

rcvFileAccepted :: Int64 -> FilePath -> [StyledString]
rcvFileAccepted fileId filePath = ["saving file " <> sShow fileId <> " to " <> plain filePath]

rcvFileStart :: Int64 -> [StyledString]
rcvFileStart fileId = ["started receiving the file " <> sShow fileId]

rcvFileComplete :: Int64 -> [StyledString]
rcvFileComplete fileId = ["completed receiving the file " <> sShow fileId]

humanReadableSize :: Integer -> String
humanReadableSize size
  | size < kB = byteSize
  | size < mB = hrSize kB "KiB"
  | size < gB = hrSize mB "MiB"
  | otherwise = hrSize gB "GiB"
  where
    byteSize = show size <> " bytes"
    hrSize sB name = unwords [showFFloat (Just 1) (fromIntegral size / (fromIntegral sB :: Double)) "", name, "/", byteSize]
    kB = 1024
    mB = kB * 1024
    gB = mB * 1024

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
    CEFileWrite f e -> ["cannot write file " <> plain f, sShow e]
    CEFileInternal e -> ["file error: " <> plain e]
    CEFileRcvChunk e -> ["file reception error: " <> plain e]
  -- e -> ["chat error: " <> sShow e]
  ChatErrorStore err -> case err of
    SEDuplicateName -> ["this display name is already used by user, contact or group"]
    SEContactNotFound c -> ["no contact " <> ttyContact c]
    SEContactNotReady c -> ["contact " <> ttyContact c <> " is not active yet"]
    SEGroupNotFound g -> ["no group " <> ttyGroup g]
    SEGroupAlreadyJoined -> ["you already joined this group"]
    SERcvFileNotFound fileId -> ["file " <> sShow fileId <> " not found"]
    e -> ["chat db error: " <> sShow e]
  ChatErrorAgent e -> ["smp agent error: " <> sShow e]
  ChatErrorMessage e -> ["chat message error: " <> sShow e]

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
