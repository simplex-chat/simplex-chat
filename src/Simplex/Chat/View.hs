{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.View where

import Data.Composition ((.:))
import Data.Function (on)
import Data.Int (Int64)
import Data.List (groupBy, intersperse, sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (DiffTime, UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (TimeZone, ZonedTime (..), getCurrentTimeZone, getZonedTime, localDay, localTimeOfDay, timeOfDayToTime, utcToLocalTime, utcToZonedTime)
import Numeric (showFFloat)
import Simplex.Chat.Controller
import Simplex.Chat.Help
import Simplex.Chat.Markdown
import Simplex.Chat.Messages
import Simplex.Chat.Protocol
import Simplex.Chat.Store (StoreError (..))
import Simplex.Chat.Styled
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Encoding.String
import qualified Simplex.Messaging.Protocol as SMP
import System.Console.ANSI.Types

responseToView :: String -> ChatResponse -> [StyledString]
responseToView cmd = \case
  ChatResponse s -> s
  CRSentMessage c mc meta -> viewSentMessage c mc meta
  CRSentGroupMessage g mc meta -> viewSentGroupMessage g mc meta
  CRSentFileInvitation c fId fPath meta -> viewSentFileInvitation c fId fPath meta
  CRSentGroupFileInvitation g fId fPath meta -> viewSentGroupFileInvitation g fId fPath meta
  CRReceivedMessage c meta mc mOk -> viewReceivedMessage' c meta mc mOk
  CRReceivedGroupMessage g c meta mc mOk -> viewReceivedGroupMessage' g c meta mc mOk
  CRReceivedFileInvitattion c meta ft mOk -> viewReceivedFileInvitation' c meta ft mOk
  CRReceivedGroupFileInvitattion g c meta ft mOk -> viewReceivedGroupFileInvitation' g c meta ft mOk
  CRCommandAccepted _ -> r []
  CRChatHelp section -> case section of
    HSMain -> r chatHelpInfo
    HSFiles -> r filesHelpInfo
    HSGroups -> r groupsHelpInfo
    HSMyAddress -> r myAddressHelpInfo
    HSMarkdown -> r markdownInfo
  CRWelcome user -> r $ chatWelcome user
  CRContactsList cs -> r $ viewContactsList cs
  CRUserContactLink cReq -> r $ connReqContact_ "Your chat address:" cReq
  CRContactRequestRejected c -> r [ttyContact c <> ": contact request rejected"]
  CRGroupCreated g -> r $ viewGroupCreated g
  CRGroupMembers g -> r $ viewGroupMembers g
  CRGroupsList gs -> r $ viewGroupsList gs
  CRSentGroupInvitation g c -> r ["invitation to join the group " <> ttyGroup g <> " sent to " <> ttyContact c]
  CRFileTransferStatus ftStatus -> r $ viewFileTransferStatus ftStatus
  CRUserProfile p -> r $ viewUserProfile p
  CRUserProfileNoChange -> r ["user profile did not change"]
  CRVersionInfo -> r [plain versionStr, plain updateStr]
  CRInvitation cReq -> viewConnReqInvitation cReq
  CRSentConfirmation -> ["confirmation sent!"]
  CRSentInvitation -> ["connection request sent!"]
  CRContactDeleted c -> [ttyContact c <> ": contact is deleted"]
  CRUserContactLinkCreated cReq -> connReqContact_ "Your new chat address is created!" cReq
  CRUserContactLinkDeleted -> viewUserContactLinkDeleted
  CRReceivedContactRequest c p -> viewReceivedContactRequest c p
  CRAcceptingContactRequest c -> [ttyContact c <> ": accepting contact request..."]
  CRRcvFileAccepted RcvFileTransfer {fileId, senderDisplayName = c} filePath ->
    ["saving file " <> sShow fileId <> " from " <> ttyContact c <> " to " <> plain filePath]
  CRRcvFileSndCancelled ft@RcvFileTransfer {senderDisplayName = c} ->
    [ttyContact c <> " cancelled sending " <> rcvFile ft]
  CRRcvFileStart ft -> receivingFile_ "started" ft
  CRRcvFileComplete ft -> receivingFile_ "completed" ft
  CRRcvFileCancelled ft -> receivingFile_ "cancelled" ft
  CRSndFileStart ft -> sendingFile_ "started" ft
  CRSndFileComplete ft -> sendingFile_ "completed" ft
  CRSndFileCancelled ft -> sendingFile_ "cancelled" ft
  CRSndFileRcvCancelled ft@SndFileTransfer {recipientDisplayName = c} ->
    [ttyContact c <> " cancelled receiving " <> sndFile ft]
  CRSndGroupFileCancelled fts -> viewSndGroupFileCancelled fts
  CRUserProfileUpdated p p' -> viewUserProfileUpdated p p'
  CRContactConnected ct -> [ttyFullContact ct <> ": contact is connected"]
  CRContactAnotherClient c -> [ttyContact c <> ": contact is connected to another client"]
  CRContactDisconnected c -> [ttyContact c <> ": disconnected from server (messages will be queued)"]
  CRContactSubscribed c -> [ttyContact c <> ": connected to server"]
  CRContactSubError c e -> [ttyContact c <> ": contact error " <> sShow e]
  CRGroupInvitation Group {localDisplayName = ldn, groupProfile = GroupProfile {fullName}} ->
    [groupInvitation ldn fullName]
  CRUserJoinedGroup g -> [ttyGroup g <> ": you joined the group"]
  CRJoinedGroupMember g m -> [ttyGroup g <> ": " <> ttyMember m <> " joined the group "]
  CRJoinedGroupMemberConnecting g host m -> [ttyGroup g <> ": " <> ttyMember host <> " added " <> ttyFullMember m <> " to the group (connecting...)"]
  CRConnectedToGroupMember g m -> [ttyGroup g <> ": " <> connectedMember m <> " is connected"]
  CRDeletedMember g by m -> [ttyGroup g <> ": " <> memberOrUser by <> " removed " <> memberOrUser m <> " from the group"]
  CRDeletedMemberUser g by -> viewDeletedMember g (Just by) Nothing <> groupPreserved g
  CRLeftMember g m -> leftMember_ g (Just m)
  CRLeftMemberUser g -> leftMember_ g Nothing <> groupPreserved g
  CRGroupDeletedUser g -> groupDeleted_ g Nothing
  CRGroupEmpty g -> [ttyFullGroup g <> ": group is empty"]
  CRGroupRemoved g -> [ttyFullGroup g <> ": you are no longer a member or group deleted"]
  CRMemberSubError gn c e -> [ttyGroup gn <> " member " <> ttyContact c <> " error: " <> sShow e]
  CRGroupSubscribed g -> [ttyFullGroup g <> ": connected to server(s)"]
  CRSndFileSubError SndFileTransfer {fileId, fileName} e ->
    ["sent file " <> sShow fileId <> " (" <> plain fileName <> ") error: " <> sShow e]
  CRRcvFileSubError RcvFileTransfer {fileId, fileInvitation = FileInvitation {fileName}} e ->
    ["received file " <> sShow fileId <> " (" <> plain fileName <> ") error: " <> sShow e]
  CRUserContactLinkSubscribed -> ["Your address is active! To show: " <> highlight' "/sa"]
  CRUserContactLinkSubError e -> ["user address error: " <> sShow e, "to delete your address: " <> highlight' "/da"]
  CRMessageError prefix err -> [plain prefix <> ": " <> plain err]
  CRChatCmdError e -> r $ viewChatError e
  CRChatError e -> viewChatError e
  where
    r = (plain cmd :)

viewInvalidConnReq :: [StyledString]
viewInvalidConnReq =
  [ "",
    "Connection link is invalid, possibly it was created in a previous version.",
    "Please ask your contact to check " <> highlight' "/version" <> " and update if needed.",
    plain updateStr
  ]

viewConnReqInvitation :: ConnReqInvitation -> [StyledString]
viewConnReqInvitation cReq =
  [ "pass this invitation link to your contact (via another channel): ",
    "",
    (plain . strEncode) cReq,
    "",
    "and ask them to connect: " <> highlight' "/c <invitation_link_above>"
  ]

viewContactsList :: [Contact] -> [StyledString]
viewContactsList =
  let ldn = T.toLower . (localDisplayName :: Contact -> ContactName)
   in map ttyFullContact . sortOn ldn

viewUserContactLinkDeleted :: [StyledString]
viewUserContactLinkDeleted =
  [ "Your chat address is deleted - accepted contacts will remain connected.",
    "To create a new chat address use " <> highlight' "/ad"
  ]

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

viewGroupCreated :: Group -> [StyledString]
viewGroupCreated g@Group {localDisplayName} =
  [ "group " <> ttyFullGroup g <> " is created",
    "use " <> highlight ("/a " <> localDisplayName <> " <name>") <> " to add members"
  ]

viewGroupDeleted :: GroupName -> GroupMember -> [StyledString]
viewGroupDeleted g m = groupDeleted_ g (Just m) <> ["use " <> highlight ("/d #" <> g) <> " to delete the local copy of the group"]

groupDeleted_ :: GroupName -> Maybe GroupMember -> [StyledString]
groupDeleted_ g m = [ttyGroup g <> ": " <> memberOrUser m <> " deleted the group"]

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

viewDeletedMember :: GroupName -> Maybe GroupMember -> Maybe GroupMember -> [StyledString]
viewDeletedMember g by m = [ttyGroup g <> ": " <> memberOrUser by <> " removed " <> memberOrUser m <> " from the group"]

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

viewGroupsList :: [GroupInfo] -> [StyledString]
viewGroupsList [] = ["you have no groups!", "to create: " <> highlight' "/g <name>"]
viewGroupsList gs = map groupSS $ sortOn ldn_ gs
  where
    ldn_ = T.toLower . (localDisplayName :: GroupInfo -> GroupName)
    groupSS GroupInfo {localDisplayName = ldn, groupProfile = GroupProfile {fullName}, userMemberStatus} =
      case userMemberStatus of
        GSMemInvited -> groupInvitation ldn fullName
        _ -> ttyGroup ldn <> optFullName ldn fullName

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

viewUserProfileUpdated :: Profile -> Profile -> [StyledString]
viewUserProfileUpdated Profile {displayName = n, fullName} Profile {displayName = n', fullName = fullName'}
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

viewReceivedMessage' :: ContactName -> ChatMsgMeta -> MsgContent -> MsgIntegrity -> [StyledString]
viewReceivedMessage' = viewReceivedMessage_' . ttyFromContact

viewReceivedGroupMessage' :: GroupName -> ContactName -> ChatMsgMeta -> MsgContent -> MsgIntegrity -> [StyledString]
viewReceivedGroupMessage' = viewReceivedMessage_' .: ttyFromGroup

viewReceivedMessage_' :: StyledString -> ChatMsgMeta -> MsgContent -> MsgIntegrity -> [StyledString]
viewReceivedMessage_' from meta mc = receivedWithTime_ from meta (ttyMsgContent mc)

receivedWithTime_ :: StyledString -> ChatMsgMeta -> [StyledString] -> MsgIntegrity -> [StyledString]
receivedWithTime_ from ChatMsgMeta {localChatTs, createdAt} styledMsg mOk = do
  prependFirst (formatedTime <> " " <> from) styledMsg ++ showIntegrity mOk
  where
    formatedTime :: StyledString
    formatedTime =
      let localTime = zonedTimeToLocalTime localChatTs
          tz = zonedTimeZone localChatTs
          format =
            if (localDay localTime < localDay (zonedTimeToLocalTime $ utcToZonedTime tz createdAt))
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

viewSentMessage :: ContactName -> MsgContent -> ChatMsgMeta -> [StyledString]
viewSentMessage = viewSentMessage_ . ttyToContact

viewSentGroupMessage :: GroupName -> MsgContent -> ChatMsgMeta -> [StyledString]
viewSentGroupMessage = viewSentMessage_ . ttyToGroup

viewSentMessage_ :: StyledString -> MsgContent -> ChatMsgMeta -> [StyledString]
viewSentMessage_ to mc = sentWithTime_ $ prependFirst to (ttyMsgContent mc)

viewSentFileInvitation :: ContactName -> FileTransferId -> FilePath -> ChatMsgMeta -> [StyledString]
viewSentFileInvitation = viewSentFile_ . ttyToContact

viewSentGroupFileInvitation :: GroupName -> FileTransferId -> FilePath -> ChatMsgMeta -> [StyledString]
viewSentGroupFileInvitation = viewSentFile_ . ttyToGroup

viewSentFile_ :: StyledString -> FileTransferId -> FilePath -> ChatMsgMeta -> [StyledString]
viewSentFile_ to fId fPath meta = sentWithTime_ (ttySentFile to fId fPath) meta

sentWithTime_ :: [StyledString] -> ChatMsgMeta -> [StyledString]
sentWithTime_ styledMsg ChatMsgMeta {localChatTs} =
  prependFirst (ttyMsgTime localChatTs <> " ") styledMsg

ttyMsgTime :: ZonedTime -> StyledString
ttyMsgTime = styleTime . formatTime defaultTimeLocale "%H:%M"

ttyMsgContent :: MsgContent -> [StyledString]
ttyMsgContent = \case
  MCText t -> msgPlain t
  MCUnknown -> ["unknown message type"]

ttySentFile :: StyledString -> FileTransferId -> FilePath -> [StyledString]
ttySentFile to fId fPath = ["/f " <> to <> ttyFilePath fPath, "use " <> highlight ("/fc " <> show fId) <> " to cancel sending"]

prependFirst :: StyledString -> [StyledString] -> [StyledString]
prependFirst s [] = [s]
prependFirst s (s' : ss) = (s <> s') : ss

msgPlain :: Text -> [StyledString]
msgPlain = map styleMarkdownText . T.lines

viewSndGroupFileCancelled :: [SndFileTransfer] -> [StyledString]
viewSndGroupFileCancelled fts =
  case filter (\SndFileTransfer {fileStatus = s} -> s /= FSCancelled && s /= FSComplete) fts of
    [] -> ["sending file can't be cancelled"]
    ts@(ft : _) -> ["cancelled sending " <> sndFile ft <> " to " <> listMembers ts]

sendingFile_ :: StyledString -> SndFileTransfer -> [StyledString]
sendingFile_ status ft@SndFileTransfer {recipientDisplayName = c} =
  [status <> " sending " <> sndFile ft <> " to " <> ttyContact c]

sndFile :: SndFileTransfer -> StyledString
sndFile SndFileTransfer {fileId, fileName} = fileTransfer fileId fileName

viewReceivedFileInvitation :: ContactName -> UTCTime -> RcvFileTransfer -> MsgIntegrity -> IO [StyledString]
viewReceivedFileInvitation c ts = viewReceivedMessage c ts . receivedFileInvitation_

viewReceivedGroupFileInvitation :: GroupName -> ContactName -> UTCTime -> RcvFileTransfer -> MsgIntegrity -> IO [StyledString]
viewReceivedGroupFileInvitation g c ts = viewReceivedGroupMessage g c ts . receivedFileInvitation_

viewReceivedFileInvitation' :: ContactName -> ChatMsgMeta -> RcvFileTransfer -> MsgIntegrity -> [StyledString]
viewReceivedFileInvitation' = viewReceivedFileInvitation_' . ttyFromContact

viewReceivedGroupFileInvitation' :: GroupName -> ContactName -> ChatMsgMeta -> RcvFileTransfer -> MsgIntegrity -> [StyledString]
viewReceivedGroupFileInvitation' = viewReceivedFileInvitation_' .: ttyFromGroup

viewReceivedFileInvitation_' :: StyledString -> ChatMsgMeta -> RcvFileTransfer -> MsgIntegrity -> [StyledString]
viewReceivedFileInvitation_' from meta ft = receivedWithTime_ from meta (receivedFileInvitation_ ft)

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

receivingFile_ :: StyledString -> RcvFileTransfer -> [StyledString]
receivingFile_ status ft@RcvFileTransfer {senderDisplayName = c} =
  [status <> " receiving " <> rcvFile ft <> " from " <> ttyContact c]

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

viewChatError :: ChatError -> [StyledString]
viewChatError = \case
  ChatError err -> case err of
    CEInvalidConnReq -> viewInvalidConnReq
    CEContactGroups c gNames -> [ttyContact c <> ": contact cannot be deleted, it is a member of the group(s) " <> ttyGroups gNames]
    CEGroupDuplicateMember c -> ["contact " <> ttyContact c <> " is already in the group"]
    CEGroupDuplicateMemberId -> ["cannot add member - duplicate member ID"]
    CEGroupUserRole -> ["you have insufficient permissions for this group command"]
    CEGroupContactRole c -> ["contact " <> ttyContact c <> " has insufficient permissions for this group action"]
    CEGroupNotJoined g -> ["you did not join this group, use " <> highlight ("/join #" <> g)]
    CEGroupMemberNotActive -> ["you cannot invite other members yet, try later"]
    CEGroupMemberUserRemoved -> ["you are no longer a member of the group"]
    CEGroupMemberNotFound c -> ["contact " <> ttyContact c <> " is not a group member"]
    CEGroupMemberIntroNotFound c -> ["group member intro not found for " <> ttyContact c]
    CEGroupCantResendInvitation g c -> viewCannotResendInvitation g c
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
    CECommandError e -> ["bad chat command: " <> plain e]
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

ttyGroups :: [GroupName] -> StyledString
ttyGroups [] = ""
ttyGroups [g] = ttyGroup g
ttyGroups (g : gs) = ttyGroup g <> ", " <> ttyGroups gs

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
