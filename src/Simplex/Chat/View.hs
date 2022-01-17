{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.View
  ( printToView,
    safeDecodeUtf8,
    msgPlain,
    clientVersionInfo,
    viewConnReqInvitation,
    viewSentConfirmation,
    viewSentInvitation,
    viewInvalidConnReq,
    viewContactDeleted,
    viewContactGroups,
    viewContactsList,
    viewUserContactLinkCreated,
    viewUserContactLinkDeleted,
    viewUserContactLink,
    viewAcceptingContactRequest,
    viewContactRequestRejected,
    viewGroupCreated,
    viewSentGroupInvitation,
    viewCannotResendInvitation,
    viewDeletedMember,
    viewLeftMemberUser,
    viewGroupDeletedUser,
    viewGroupMembers,
    viewSentFileInfo,
    viewRcvFileAccepted,
    viewRcvFileSndCancelled,
    viewSndGroupFileCancelled,
    viewRcvFileCancelled,
    viewFileTransferStatus,
    viewUserProfileUpdated,
    viewUserProfile,
    viewChatError,
    viewSentMessage,
    viewSentGroupMessage,
    viewSentGroupFileInvitation,
    viewSentFileInvitation,
    viewGroupsList,
    viewContactSubscribed,
    viewContactSubError,
    viewGroupInvitation,
    viewGroupEmpty,
    viewGroupRemoved,
    viewMemberSubError,
    viewGroupSubscribed,
    viewSndFileSubError,
    viewRcvFileSubError,
    viewUserContactLinkSubscribed,
    viewUserContactLinkSubError,
    viewContactConnected,
    viewContactDisconnected,
    viewContactAnotherClient,
    viewJoinedGroupMember,
    viewUserJoinedGroup,
    viewJoinedGroupMemberConnecting,
    viewConnectedToGroupMember,
    viewReceivedGroupInvitation,
    viewDeletedMemberUser,
    viewLeftMember,
    viewSndFileStart,
    viewSndFileComplete,
    viewSndFileCancelled,
    viewSndFileRcvCancelled,
    viewRcvFileStart,
    viewRcvFileComplete,
    viewReceivedContactRequest,
    viewMessageError,
    viewReceivedMessage,
    viewReceivedGroupMessage,
    viewReceivedFileInvitation,
    viewReceivedGroupFileInvitation,
    viewContactUpdated,
    viewContactsMerged,
    viewGroupDeleted,
  )
where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.ByteString.Char8 (ByteString)
import Data.Composition ((.:))
import Data.Function (on)
import Data.Int (Int64)
import Data.List (groupBy, intersperse, sort, sortOn)
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
import Simplex.Messaging.Encoding.String
import qualified Simplex.Messaging.Protocol as SMP
import System.Console.ANSI.Types

viewSentConfirmation :: [StyledString]
viewSentConfirmation = ["confirmation sent!"]

viewSentInvitation :: [StyledString]
viewSentInvitation = ["connection request sent!"]

viewInvalidConnReq :: [StyledString]
viewInvalidConnReq =
  [ "",
    "Connection link is invalid, possibly it was created in a previous version.",
    "Please ask your contact to check " <> highlight' "/version" <> " and update if needed.",
    plain updateStr
  ]

viewUserContactLinkSubscribed :: [StyledString]
viewUserContactLinkSubscribed = ["Your address is active! To show: " <> highlight' "/sa"]

viewConnReqInvitation :: ConnReqInvitation -> [StyledString]
viewConnReqInvitation cReq =
  [ "pass this invitation link to your contact (via another channel): ",
    "",
    (plain . strEncode) cReq,
    "",
    "and ask them to connect: " <> highlight' "/c <invitation_link_above>"
  ]

viewContactDeleted :: ContactName -> [StyledString]
viewContactDeleted c = [ttyContact c <> ": contact is deleted"]

viewContactGroups :: ContactName -> [GroupName] -> [StyledString]
viewContactGroups c gNames = [ttyContact c <> ": contact cannot be deleted, it is a member of the group(s) " <> ttyGroups gNames]
  where
    ttyGroups :: [GroupName] -> StyledString
    ttyGroups [] = ""
    ttyGroups [g] = ttyGroup g
    ttyGroups (g : gs) = ttyGroup g <> ", " <> ttyGroups gs

viewContactsList :: [Contact] -> [StyledString]
viewContactsList =
  let ldn = T.toLower . (localDisplayName :: Contact -> ContactName)
   in map ttyFullContact . sortOn ldn

viewContactConnected :: Contact -> [StyledString]
viewContactConnected ct = [ttyFullContact ct <> ": contact is connected"]

viewContactDisconnected :: ContactName -> [StyledString]
viewContactDisconnected c = [ttyContact c <> ": disconnected from server (messages will be queued)"]

viewContactAnotherClient :: ContactName -> [StyledString]
viewContactAnotherClient c = [ttyContact c <> ": contact is connected to another client"]

viewContactSubscribed :: ContactName -> [StyledString]
viewContactSubscribed c = [ttyContact c <> ": connected to server"]

viewContactSubError :: ContactName -> ChatError -> [StyledString]
viewContactSubError c e = [ttyContact c <> ": contact error " <> sShow e]

viewUserContactLinkCreated :: ConnReqContact -> [StyledString]
viewUserContactLinkCreated = connReqContact_ "Your new chat address is created!"

viewUserContactLinkDeleted :: [StyledString]
viewUserContactLinkDeleted =
  [ "Your chat address is deleted - accepted contacts will remain connected.",
    "To create a new chat address use " <> highlight' "/ad"
  ]

viewUserContactLink :: ConnReqContact -> [StyledString]
viewUserContactLink = connReqContact_ "Your chat address:"

connReqContact_ :: StyledString -> ConnReqContact -> [StyledString]
connReqContact_ intro cReq =
  [ intro,
    "",
    (plain . strEncode) cReq,
    "",
    "Anybody can send you contact requests with: " <> highlight' "/c <contact_link_above>",
    "to show it again: " <> highlight' "/sa",
    "to delete it: " <> highlight' "/da" <> " (accepted contacts will remain connected)"
  ]

viewReceivedContactRequest :: ContactName -> Profile -> [StyledString]
viewReceivedContactRequest c Profile {fullName} =
  [ ttyFullName c fullName <> " wants to connect to you!",
    "to accept: " <> highlight ("/ac " <> c),
    "to reject: " <> highlight ("/rc " <> c) <> " (the sender will NOT be notified)"
  ]

viewAcceptingContactRequest :: ContactName -> [StyledString]
viewAcceptingContactRequest c = [ttyContact c <> ": accepting contact request..."]

viewContactRequestRejected :: ContactName -> [StyledString]
viewContactRequestRejected c = [ttyContact c <> ": contact request rejected"]

viewUserContactLinkSubError :: ChatError -> [StyledString]
viewUserContactLinkSubError e =
  [ "user address error: " <> sShow e,
    "to delete your address: " <> highlight' "/da"
  ]

viewGroupSubscribed :: Group -> [StyledString]
viewGroupSubscribed g = [ttyFullGroup g <> ": connected to server(s)"]

viewGroupEmpty :: Group -> [StyledString]
viewGroupEmpty g = [ttyFullGroup g <> ": group is empty"]

viewGroupRemoved :: Group -> [StyledString]
viewGroupRemoved g = [ttyFullGroup g <> ": you are no longer a member or group deleted"]

viewMemberSubError :: GroupName -> ContactName -> ChatError -> [StyledString]
viewMemberSubError g c e = [ttyGroup g <> " member " <> ttyContact c <> " error: " <> sShow e]

viewGroupCreated :: Group -> [StyledString]
viewGroupCreated g@Group {localDisplayName} =
  [ "group " <> ttyFullGroup g <> " is created",
    "use " <> highlight ("/a " <> localDisplayName <> " <name>") <> " to add members"
  ]

viewGroupDeletedUser :: GroupName -> [StyledString]
viewGroupDeletedUser g = groupDeleted_ g Nothing

viewGroupDeleted :: GroupName -> GroupMember -> [StyledString]
viewGroupDeleted g m = groupDeleted_ g (Just m) <> ["use " <> highlight ("/d #" <> g) <> " to delete the local copy of the group"]

groupDeleted_ :: GroupName -> Maybe GroupMember -> [StyledString]
groupDeleted_ g m = [ttyGroup g <> ": " <> memberOrUser m <> " deleted the group"]

viewSentGroupInvitation :: GroupName -> ContactName -> [StyledString]
viewSentGroupInvitation g c = ["invitation to join the group " <> ttyGroup g <> " sent to " <> ttyContact c]

viewCannotResendInvitation :: GroupName -> ContactName -> [StyledString]
viewCannotResendInvitation g c =
  [ ttyContact c <> " is already invited to group " <> ttyGroup g,
    "to re-send invitation: " <> highlight ("/rm " <> g <> " " <> c) <> ", " <> highlight ("/a " <> g <> " " <> c)
  ]

viewReceivedGroupInvitation :: Group -> ContactName -> GroupMemberRole -> [StyledString]
viewReceivedGroupInvitation g@Group {localDisplayName} c role =
  [ ttyFullGroup g <> ": " <> ttyContact c <> " invites you to join the group as " <> plain (strEncode role),
    "use " <> highlight ("/j " <> localDisplayName) <> " to accept"
  ]

viewJoinedGroupMember :: GroupName -> GroupMember -> [StyledString]
viewJoinedGroupMember g m = [ttyGroup g <> ": " <> ttyMember m <> " joined the group "]

viewUserJoinedGroup :: GroupName -> [StyledString]
viewUserJoinedGroup g = [ttyGroup g <> ": you joined the group"]

viewJoinedGroupMemberConnecting :: GroupName -> GroupMember -> GroupMember -> [StyledString]
viewJoinedGroupMemberConnecting g host m = [ttyGroup g <> ": " <> ttyMember host <> " added " <> ttyFullMember m <> " to the group (connecting...)"]

viewConnectedToGroupMember :: GroupName -> GroupMember -> [StyledString]
viewConnectedToGroupMember g m = [ttyGroup g <> ": " <> connectedMember m <> " is connected"]

viewDeletedMember :: GroupName -> Maybe GroupMember -> Maybe GroupMember -> [StyledString]
viewDeletedMember g by m = [ttyGroup g <> ": " <> memberOrUser by <> " removed " <> memberOrUser m <> " from the group"]

viewDeletedMemberUser :: GroupName -> GroupMember -> [StyledString]
viewDeletedMemberUser g by = viewDeletedMember g (Just by) Nothing <> groupPreserved g

viewLeftMemberUser :: GroupName -> [StyledString]
viewLeftMemberUser g = leftMember_ g Nothing <> groupPreserved g

viewLeftMember :: GroupName -> GroupMember -> [StyledString]
viewLeftMember g m = leftMember_ g (Just m)

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

viewGroupMembers :: Group -> [StyledString]
viewGroupMembers Group {membership, members} = map groupMember . filter (not . removedOrLeft) $ membership : members
  where
    removedOrLeft m = let s = memberStatus m in s == GSMemRemoved || s == GSMemLeft
    groupMember m = ttyFullMember m <> ": " <> role m <> ", " <> category m <> status m
    role m = plain . strEncode $ memberRole (m :: GroupMember)
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

viewGroupsList :: [(GroupName, Text, GroupMemberStatus)] -> [StyledString]
viewGroupsList [] = ["you have no groups!", "to create: " <> highlight' "/g <name>"]
viewGroupsList gs = map groupSS $ sort gs
  where
    groupSS (displayName, fullName, GSMemInvited) = groupInvitation displayName fullName
    groupSS (displayName, fullName, _) = ttyGroup displayName <> optFullName displayName fullName

viewGroupInvitation :: Group -> [StyledString]
viewGroupInvitation Group {localDisplayName = ldn, groupProfile = GroupProfile {fullName}} =
  [groupInvitation ldn fullName]

groupInvitation :: GroupName -> Text -> StyledString
groupInvitation displayName fullName =
  highlight ("#" <> displayName)
    <> optFullName displayName fullName
    <> " - you are invited ("
    <> highlight ("/j " <> displayName)
    <> " to join, "
    <> highlight ("/d #" <> displayName)
    <> " to delete invitation)"

viewContactsMerged :: Contact -> Contact -> [StyledString]
viewContactsMerged _to@Contact {localDisplayName = c1} _from@Contact {localDisplayName = c2} =
  [ "contact " <> ttyContact c2 <> " is merged into " <> ttyContact c1,
    "use " <> ttyToContact c1 <> highlight' "<message>" <> " to send messages"
  ]

viewUserProfile :: Profile -> [StyledString]
viewUserProfile Profile {displayName, fullName} =
  [ "user profile: " <> ttyFullName displayName fullName,
    "use " <> highlight' "/p <display name> [<full name>]" <> " to change it",
    "(the updated profile will be sent to all your contacts)"
  ]

viewUserProfileUpdated :: User -> User -> [StyledString]
viewUserProfileUpdated
  User {localDisplayName = n, profile = Profile {fullName}}
  User {localDisplayName = n', profile = Profile {fullName = fullName'}}
    | n == n' && fullName == fullName' = []
    | n == n' = ["user full name " <> (if T.null fullName' || fullName' == n' then "removed" else "changed to " <> plain fullName') <> notified]
    | otherwise = ["user profile is changed to " <> ttyFullName n' fullName' <> notified]
    where
      notified = " (your contacts are notified)"

viewContactUpdated :: Contact -> Contact -> [StyledString]
viewContactUpdated
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

viewMessageError :: Text -> Text -> [StyledString]
viewMessageError prefix err = [plain prefix <> ": " <> plain err]

viewReceivedMessage :: ContactName -> UTCTime -> [StyledString] -> MsgIntegrity -> IO [StyledString]
viewReceivedMessage = viewReceivedMessage_ . ttyFromContact

viewReceivedGroupMessage :: GroupName -> ContactName -> UTCTime -> [StyledString] -> MsgIntegrity -> IO [StyledString]
viewReceivedGroupMessage = viewReceivedMessage_ .: ttyFromGroup

viewReceivedMessage_ :: StyledString -> UTCTime -> [StyledString] -> MsgIntegrity -> IO [StyledString]
viewReceivedMessage_ from utcTime msg mOk = do
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

viewSentMessage :: ContactName -> ByteString -> IO [StyledString]
viewSentMessage = viewSentMessage_ . ttyToContact

viewSentGroupMessage :: GroupName -> ByteString -> IO [StyledString]
viewSentGroupMessage = viewSentMessage_ . ttyToGroup

viewSentMessage_ :: StyledString -> ByteString -> IO [StyledString]
viewSentMessage_ to msg = sentWithTime_ to . msgPlain $ safeDecodeUtf8 msg

viewSentFileInvitation :: ContactName -> FilePath -> IO [StyledString]
viewSentFileInvitation = viewSentFileInvitation_ . ttyToContact

viewSentGroupFileInvitation :: GroupName -> FilePath -> IO [StyledString]
viewSentGroupFileInvitation = viewSentFileInvitation_ . ttyToGroup

viewSentFileInvitation_ :: StyledString -> FilePath -> IO [StyledString]
viewSentFileInvitation_ to f = sentWithTime_ ("/f " <> to) [ttyFilePath f]

sentWithTime_ :: StyledString -> [StyledString] -> IO [StyledString]
sentWithTime_ to styledMsg = do
  time <- formatTime defaultTimeLocale "%H:%M" <$> getZonedTime
  pure $ prependFirst (styleTime time <> " " <> to) styledMsg

prependFirst :: StyledString -> [StyledString] -> [StyledString]
prependFirst s [] = [s]
prependFirst s (s' : ss) = (s <> s') : ss

msgPlain :: Text -> [StyledString]
msgPlain = map styleMarkdownText . T.lines

viewSentFileInfo :: Int64 -> [StyledString]
viewSentFileInfo fileId =
  ["use " <> highlight ("/fc " <> show fileId) <> " to cancel sending"]

viewSndFileStart :: SndFileTransfer -> [StyledString]
viewSndFileStart = sendingFile_ "started"

viewSndFileComplete :: SndFileTransfer -> [StyledString]
viewSndFileComplete = sendingFile_ "completed"

viewSndFileCancelled :: SndFileTransfer -> [StyledString]
viewSndFileCancelled = sendingFile_ "cancelled"

viewSndGroupFileCancelled :: [SndFileTransfer] -> [StyledString]
viewSndGroupFileCancelled fts =
  case filter (\SndFileTransfer {fileStatus = s} -> s /= FSCancelled && s /= FSComplete) fts of
    [] -> ["sending file can't be cancelled"]
    ts@(ft : _) -> ["cancelled sending " <> sndFile ft <> " to " <> listMembers ts]

sendingFile_ :: StyledString -> SndFileTransfer -> [StyledString]
sendingFile_ status ft@SndFileTransfer {recipientDisplayName = c} =
  [status <> " sending " <> sndFile ft <> " to " <> ttyContact c]

viewSndFileRcvCancelled :: SndFileTransfer -> [StyledString]
viewSndFileRcvCancelled ft@SndFileTransfer {recipientDisplayName = c} =
  [ttyContact c <> " cancelled receiving " <> sndFile ft]

sndFile :: SndFileTransfer -> StyledString
sndFile SndFileTransfer {fileId, fileName} = fileTransfer fileId fileName

viewReceivedFileInvitation :: ContactName -> UTCTime -> RcvFileTransfer -> MsgIntegrity -> IO [StyledString]
viewReceivedFileInvitation c ts = viewReceivedMessage c ts . receivedFileInvitation_

viewReceivedGroupFileInvitation :: GroupName -> ContactName -> UTCTime -> RcvFileTransfer -> MsgIntegrity -> IO [StyledString]
viewReceivedGroupFileInvitation g c ts = viewReceivedGroupMessage g c ts . receivedFileInvitation_

receivedFileInvitation_ :: RcvFileTransfer -> [StyledString]
receivedFileInvitation_ RcvFileTransfer {fileId, fileInvitation = FileInvitation {fileName, fileSize}} =
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

viewRcvFileAccepted :: RcvFileTransfer -> FilePath -> [StyledString]
viewRcvFileAccepted RcvFileTransfer {fileId, senderDisplayName = c} filePath =
  ["saving file " <> sShow fileId <> " from " <> ttyContact c <> " to " <> plain filePath]

viewRcvFileStart :: RcvFileTransfer -> [StyledString]
viewRcvFileStart = receivingFile_ "started"

viewRcvFileComplete :: RcvFileTransfer -> [StyledString]
viewRcvFileComplete = receivingFile_ "completed"

viewRcvFileCancelled :: RcvFileTransfer -> [StyledString]
viewRcvFileCancelled = receivingFile_ "cancelled"

receivingFile_ :: StyledString -> RcvFileTransfer -> [StyledString]
receivingFile_ status ft@RcvFileTransfer {senderDisplayName = c} =
  [status <> " receiving " <> rcvFile ft <> " from " <> ttyContact c]

viewRcvFileSndCancelled :: RcvFileTransfer -> [StyledString]
viewRcvFileSndCancelled ft@RcvFileTransfer {senderDisplayName = c} =
  [ttyContact c <> " cancelled sending " <> rcvFile ft]

rcvFile :: RcvFileTransfer -> StyledString
rcvFile RcvFileTransfer {fileId, fileInvitation = FileInvitation {fileName}} = fileTransfer fileId fileName

fileTransfer :: Int64 -> String -> StyledString
fileTransfer fileId fileName = "file " <> sShow fileId <> " (" <> ttyFilePath fileName <> ")"

viewFileTransferStatus :: (FileTransfer, [Integer]) -> [StyledString]
viewFileTransferStatus (FTSnd [ft@SndFileTransfer {fileStatus, fileSize, chunkSize}], chunksNum) =
  ["sending " <> sndFile ft <> " " <> sndStatus]
  where
    sndStatus = case fileStatus of
      FSNew -> "not accepted yet"
      FSAccepted -> "just started"
      FSConnected -> "progress " <> fileProgress chunksNum chunkSize fileSize
      FSComplete -> "complete"
      FSCancelled -> "cancelled"
viewFileTransferStatus (FTSnd [], _) = ["no file transfers (empty group)"]
viewFileTransferStatus (FTSnd fts@(ft : _), chunksNum) =
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
viewFileTransferStatus (FTRcv ft@RcvFileTransfer {fileId, fileInvitation = FileInvitation {fileSize}, fileStatus, chunkSize}, chunksNum) =
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

viewSndFileSubError :: SndFileTransfer -> ChatError -> [StyledString]
viewSndFileSubError SndFileTransfer {fileId, fileName} e =
  ["sent file " <> sShow fileId <> " (" <> plain fileName <> ") error: " <> sShow e]

viewRcvFileSubError :: RcvFileTransfer -> ChatError -> [StyledString]
viewRcvFileSubError RcvFileTransfer {fileId, fileInvitation = FileInvitation {fileName}} e =
  ["received file " <> sShow fileId <> " (" <> plain fileName <> ") error: " <> sShow e]

viewChatError :: ChatError -> [StyledString]
viewChatError = \case
  ChatError err -> case err of
    CEGroupDuplicateMember c -> ["contact " <> ttyContact c <> " is already in the group"]
    CEGroupDuplicateMemberId -> ["cannot add member - duplicate member ID"]
    CEGroupUserRole -> ["you have insufficient permissions for this group command"]
    CEGroupContactRole c -> ["contact " <> ttyContact c <> " has insufficient permissions for this group action"]
    CEGroupNotJoined g -> ["you did not join this group, use " <> highlight ("/join #" <> g)]
    CEGroupMemberNotActive -> ["you cannot invite other members yet, try later"]
    CEGroupMemberUserRemoved -> ["you are no longer a member of the group"]
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
    CEAgentVersion -> ["unsupported agent version"]
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
    SEDuplicateContactLink -> ["you already have chat address, to show: " <> highlight' "/sa"]
    SEUserContactLinkNotFound -> ["no chat address, to create: " <> highlight' "/ad"]
    SEContactRequestNotFound c -> ["no contact request from " <> ttyContact c]
    e -> ["chat db error: " <> sShow e]
  ChatErrorAgent err -> case err of
    SMP SMP.AUTH -> ["error: this connection is deleted"]
    e -> ["smp agent error: " <> sShow e]
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

clientVersionInfo :: [StyledString]
clientVersionInfo = [plain versionStr, plain updateStr]
