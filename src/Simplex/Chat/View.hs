{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.View where

import Data.Function (on)
import Data.Int (Int64)
import Data.List (groupBy, intersperse, sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (DiffTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (ZonedTime (..), localDay, localTimeOfDay, timeOfDayToTime, utcToZonedTime)
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

serializeChatResponse :: ChatResponse -> String
serializeChatResponse = unlines . map unStyle . responseToView ""

responseToView :: String -> ChatResponse -> [StyledString]
responseToView cmd = \case
  CRApiChats chats -> api [sShow chats]
  CRApiChat chat -> api [sShow chat]
  CRNewChatItem (AChatItem _ _ chat item) -> viewChatItem chat item
  CRChatItemMSent _ -> []
  CRChatItemMErr _ e -> viewChatError $ ChatErrorAgent e
  CRMsgIntegrityError mErr -> viewMsgIntegrityError mErr
  CRCmdAccepted _ -> r []
  CRChatHelp section -> case section of
    HSMain -> r chatHelpInfo
    HSFiles -> r filesHelpInfo
    HSGroups -> r groupsHelpInfo
    HSMyAddress -> r myAddressHelpInfo
    HSMarkdown -> r markdownInfo
  CRWelcome user -> r $ chatWelcome user
  CRContactsList cs -> r $ viewContactsList cs
  CRUserContactLink cReq -> r $ connReqContact_ "Your chat address:" cReq
  CRContactRequestRejected UserContactRequest {localDisplayName = c} -> r [ttyContact c <> ": contact request rejected"]
  CRGroupCreated g -> r $ viewGroupCreated g
  CRGroupMembers g -> r $ viewGroupMembers g
  CRGroupsList gs -> r $ viewGroupsList gs
  CRSentGroupInvitation g c -> r ["invitation to join the group " <> ttyGroup' g <> " sent to " <> ttyContact' c]
  CRFileTransferStatus ftStatus -> r $ viewFileTransferStatus ftStatus
  CRUserProfile p -> r $ viewUserProfile p
  CRUserProfileNoChange -> r ["user profile did not change"]
  CRVersionInfo -> r [plain versionStr, plain updateStr]
  CRChatCmdError e -> r $ viewChatError e
  CRInvitation cReq -> r' $ viewConnReqInvitation cReq
  CRSentConfirmation -> r' ["confirmation sent!"]
  CRSentInvitation -> r' ["connection request sent!"]
  CRContactDeleted Contact {localDisplayName = c} -> r' [ttyContact c <> ": contact is deleted"]
  CRAcceptingContactRequest Contact {localDisplayName = c} -> r' [ttyContact c <> ": accepting contact request..."]
  CRUserContactLinkCreated cReq -> r' $ connReqContact_ "Your new chat address is created!" cReq
  CRUserContactLinkDeleted -> r' viewUserContactLinkDeleted
  CRUserAcceptedGroupSent _g -> r' [] -- [ttyGroup' g <> ": joining the group..."]
  CRUserDeletedMember g m -> r' [ttyGroup' g <> ": you removed " <> ttyMember m <> " from the group"]
  CRLeftMemberUser g -> r' $ [ttyGroup' g <> ": you left the group"] <> groupPreserved g
  CRGroupDeletedUser g -> r' [ttyGroup' g <> ": you deleted the group"]
  CRRcvFileAccepted RcvFileTransfer {fileId, senderDisplayName = c} filePath ->
    r' ["saving file " <> sShow fileId <> " from " <> ttyContact c <> " to " <> plain filePath]
  CRRcvFileAcceptedSndCancelled ft -> r' $ viewRcvFileSndCancelled ft
  CRSndGroupFileCancelled fts -> r' $ viewSndGroupFileCancelled fts
  CRRcvFileCancelled ft -> r' $ receivingFile_ "cancelled" ft
  CRUserProfileUpdated p p' -> r' $ viewUserProfileUpdated p p'
  CRContactUpdated c c' -> viewContactUpdated c c'
  CRContactsMerged intoCt mergedCt -> viewContactsMerged intoCt mergedCt
  CRReceivedContactRequest UserContactRequest {localDisplayName = c, profile} -> viewReceivedContactRequest c profile
  CRRcvFileStart ft -> receivingFile_ "started" ft
  CRRcvFileComplete ft -> receivingFile_ "completed" ft
  CRRcvFileSndCancelled ft -> viewRcvFileSndCancelled ft
  CRSndFileStart ft -> sendingFile_ "started" ft
  CRSndFileComplete ft -> sendingFile_ "completed" ft
  CRSndFileCancelled ft -> sendingFile_ "cancelled" ft
  CRSndFileRcvCancelled ft@SndFileTransfer {recipientDisplayName = c} ->
    [ttyContact c <> " cancelled receiving " <> sndFile ft]
  CRContactConnected ct -> [ttyFullContact ct <> ": contact is connected"]
  CRContactAnotherClient c -> [ttyContact' c <> ": contact is connected to another client"]
  CRContactDisconnected c -> [ttyContact' c <> ": disconnected from server (messages will be queued)"]
  CRContactSubscribed c -> [ttyContact' c <> ": connected to server"]
  CRContactSubError c e -> [ttyContact' c <> ": contact error " <> sShow e]
  CRGroupInvitation GroupInfo {localDisplayName = ldn, groupProfile = GroupProfile {fullName}} ->
    [groupInvitation ldn fullName]
  CRReceivedGroupInvitation g c role -> viewReceivedGroupInvitation g c role
  CRUserJoinedGroup g -> [ttyGroup' g <> ": you joined the group"]
  CRJoinedGroupMember g m -> [ttyGroup' g <> ": " <> ttyMember m <> " joined the group "]
  CRJoinedGroupMemberConnecting g host m -> [ttyGroup' g <> ": " <> ttyMember host <> " added " <> ttyFullMember m <> " to the group (connecting...)"]
  CRConnectedToGroupMember g m -> [ttyGroup' g <> ": " <> connectedMember m <> " is connected"]
  CRDeletedMemberUser g by -> [ttyGroup' g <> ": " <> ttyMember by <> " removed you from the group"] <> groupPreserved g
  CRDeletedMember g by m -> [ttyGroup' g <> ": " <> ttyMember by <> " removed " <> ttyMember m <> " from the group"]
  CRLeftMember g m -> [ttyGroup' g <> ": " <> ttyMember m <> " left the group"]
  CRGroupEmpty g -> [ttyFullGroup g <> ": group is empty"]
  CRGroupRemoved g -> [ttyFullGroup g <> ": you are no longer a member or group deleted"]
  CRGroupDeleted g m -> [ttyGroup' g <> ": " <> ttyMember m <> " deleted the group", "use " <> highlight ("/d #" <> groupName' g) <> " to delete the local copy of the group"]
  CRMemberSubError g c e -> [ttyGroup' g <> " member " <> ttyContact c <> " error: " <> sShow e]
  CRGroupSubscribed g -> [ttyFullGroup g <> ": connected to server(s)"]
  CRSndFileSubError SndFileTransfer {fileId, fileName} e ->
    ["sent file " <> sShow fileId <> " (" <> plain fileName <> ") error: " <> sShow e]
  CRRcvFileSubError RcvFileTransfer {fileId, fileInvitation = FileInvitation {fileName}} e ->
    ["received file " <> sShow fileId <> " (" <> plain fileName <> ") error: " <> sShow e]
  CRUserContactLinkSubscribed -> ["Your address is active! To show: " <> highlight' "/sa"]
  CRUserContactLinkSubError e -> ["user address error: " <> sShow e, "to delete your address: " <> highlight' "/da"]
  CRMessageError prefix err -> [plain prefix <> ": " <> plain err]
  CRChatError e -> viewChatError e
  where
    api = (highlight cmd :)
    r = (plain cmd :)
    -- this function should be `r` for "synchronous", `id` for "asynchronous" command responses
    -- r' = id
    r' = r

viewChatItem :: ChatInfo c -> ChatItem c d -> [StyledString]
viewChatItem chat (ChatItem cd meta content) = case (chat, cd) of
  (DirectChat c, CIDirectSnd) -> case content of
    CISndMsgContent mc -> viewSentMessage to mc meta
    CISndFileInvitation fId fPath -> viewSentFileInvitation to fId fPath meta
    where
      to = ttyToContact' c
  (DirectChat c, CIDirectRcv) -> case content of
    CIRcvMsgContent mc -> viewReceivedMessage from meta mc -- mOk
    CIRcvFileInvitation ft -> viewReceivedFileInvitation from meta ft -- mOk
    where
      from = ttyFromContact' c
  (GroupChat g, CIGroupSnd) -> case content of
    CISndMsgContent mc -> viewSentMessage to mc meta
    CISndFileInvitation fId fPath -> viewSentFileInvitation to fId fPath meta
    where
      to = ttyToGroup g
  (GroupChat g, CIGroupRcv m) -> case content of
    CIRcvMsgContent mc -> viewReceivedMessage from meta mc -- mOk
    CIRcvFileInvitation ft -> viewReceivedFileInvitation from meta ft -- mOk
    where
      from = ttyFromGroup' g m
  where
    ttyToContact' Contact {localDisplayName = c} = ttyToContact c
    ttyFromContact' Contact {localDisplayName = c} = ttyFromContact c
    ttyFromGroup' g GroupMember {localDisplayName = m} = ttyFromGroup g m

viewMsgIntegrityError :: MsgErrorType -> [StyledString]
viewMsgIntegrityError err = msgError $ case err of
  MsgSkipped fromId toId ->
    "skipped message ID " <> show fromId
      <> if fromId == toId then "" else ".." <> show toId
  MsgBadId msgId -> "unexpected message ID " <> show msgId
  MsgBadHash -> "incorrect message hash"
  MsgDuplicate -> "duplicate message ID"
  where
    msgError :: String -> [StyledString]
    msgError s = [styled (Colored Red) s]

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

viewGroupCreated :: GroupInfo -> [StyledString]
viewGroupCreated g@GroupInfo {localDisplayName} =
  [ "group " <> ttyFullGroup g <> " is created",
    "use " <> highlight ("/a " <> localDisplayName <> " <name>") <> " to add members"
  ]

viewCannotResendInvitation :: GroupInfo -> ContactName -> [StyledString]
viewCannotResendInvitation GroupInfo {localDisplayName = gn} c =
  [ ttyContact c <> " is already invited to group " <> ttyGroup gn,
    "to re-send invitation: " <> highlight ("/rm " <> gn <> " " <> c) <> ", " <> highlight ("/a " <> gn <> " " <> c)
  ]

viewReceivedGroupInvitation :: GroupInfo -> Contact -> GroupMemberRole -> [StyledString]
viewReceivedGroupInvitation g c role =
  [ ttyFullGroup g <> ": " <> ttyContact' c <> " invites you to join the group as " <> plain (strEncode role),
    "use " <> highlight ("/j " <> groupName' g) <> " to accept"
  ]

groupPreserved :: GroupInfo -> [StyledString]
groupPreserved g = ["use " <> highlight ("/d #" <> groupName' g) <> " to delete the group"]

connectedMember :: GroupMember -> StyledString
connectedMember m = case memberCategory m of
  GCPreMember -> "member " <> ttyFullMember m
  GCPostMember -> "new member " <> ttyMember m -- without fullName as as it was shown in joinedGroupMemberConnecting
  _ -> "member " <> ttyMember m -- these case is not used

viewGroupMembers :: Group -> [StyledString]
viewGroupMembers (Group GroupInfo {membership} members) = map groupMember . filter (not . removedOrLeft) $ membership : members
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
    groupSS GroupInfo {localDisplayName = ldn, groupProfile = GroupProfile {fullName}, membership} =
      case memberStatus membership of
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
viewContactsMerged _into@Contact {localDisplayName = c1} _merged@Contact {localDisplayName = c2} =
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

viewReceivedMessage :: StyledString -> CIMeta -> MsgContent -> [StyledString]
viewReceivedMessage from meta mc = receivedWithTime_ from meta (ttyMsgContent mc)

receivedWithTime_ :: StyledString -> CIMeta -> [StyledString] -> [StyledString]
receivedWithTime_ from CIMeta {localItemTs, createdAt} styledMsg = do
  prependFirst (formattedTime <> " " <> from) styledMsg -- ++ showIntegrity mOk
  where
    formattedTime :: StyledString
    formattedTime =
      let localTime = zonedTimeToLocalTime localItemTs
          tz = zonedTimeZone localItemTs
          format =
            if (localDay localTime < localDay (zonedTimeToLocalTime $ utcToZonedTime tz createdAt))
              && (timeOfDayToTime (localTimeOfDay localTime) > (6 * 60 * 60 :: DiffTime))
              then "%m-%d" -- if message is from yesterday or before and 6 hours has passed since midnight
              else "%H:%M"
       in styleTime $ formatTime defaultTimeLocale format localTime

viewSentMessage :: StyledString -> MsgContent -> CIMeta -> [StyledString]
viewSentMessage to = sentWithTime_ . prependFirst to . ttyMsgContent

viewSentFileInvitation :: StyledString -> FileTransferId -> FilePath -> CIMeta -> [StyledString]
viewSentFileInvitation to fId fPath = sentWithTime_ $ ttySentFile to fId fPath

sentWithTime_ :: [StyledString] -> CIMeta -> [StyledString]
sentWithTime_ styledMsg CIMeta {localItemTs} =
  prependFirst (ttyMsgTime localItemTs <> " ") styledMsg

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

viewRcvFileSndCancelled :: RcvFileTransfer -> [StyledString]
viewRcvFileSndCancelled ft@RcvFileTransfer {senderDisplayName = c} =
  [ttyContact c <> " cancelled sending " <> rcvFile ft]

viewSndGroupFileCancelled :: [SndFileTransfer] -> [StyledString]
viewSndGroupFileCancelled fts =
  case filter (\SndFileTransfer {fileStatus = s} -> s /= FSCancelled && s /= FSComplete) fts of
    [] -> ["sending file can't be cancelled"]
    ts@(ft : _) -> ["cancelled sending " <> sndFile ft <> " to " <> listMembers ts]

sendingFile_ :: StyledString -> SndFileTransfer -> [StyledString]
sendingFile_ status ft@SndFileTransfer {recipientDisplayName = c} =
  [status <> " sending " <> sndFile ft <> " to " <> ttyContact c]

sndFile :: SndFileTransfer -> StyledString
sndFile SndFileTransfer {fileId, fileName} = fileTransferStr fileId fileName

viewReceivedFileInvitation :: StyledString -> CIMeta -> RcvFileTransfer -> [StyledString]
viewReceivedFileInvitation from meta ft = receivedWithTime_ from meta (receivedFileInvitation_ ft)

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
rcvFile RcvFileTransfer {fileId, fileInvitation = FileInvitation {fileName}} = fileTransferStr fileId fileName

fileTransferStr :: Int64 -> String -> StyledString
fileTransferStr fileId fileName = "file " <> sShow fileId <> " (" <> ttyFilePath fileName <> ")"

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
    CEContactGroups Contact {localDisplayName} gNames -> [ttyContact localDisplayName <> ": contact cannot be deleted, it is a member of the group(s) " <> ttyGroups gNames]
    CEGroupDuplicateMember c -> ["contact " <> ttyContact c <> " is already in the group"]
    CEGroupDuplicateMemberId -> ["cannot add member - duplicate member ID"]
    CEGroupUserRole -> ["you have insufficient permissions for this group command"]
    CEGroupContactRole c -> ["contact " <> ttyContact c <> " has insufficient permissions for this group action"]
    CEGroupNotJoined g -> ["you did not join this group, use " <> highlight ("/join #" <> groupName' g)]
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
    SEContactNotFoundByName c -> ["no contact " <> ttyContact c]
    SEContactNotReady c -> ["contact " <> ttyContact c <> " is not active yet"]
    SEGroupNotFoundByName g -> ["no group " <> ttyGroup g]
    SEGroupAlreadyJoined -> ["you already joined this group"]
    SEFileNotFound fileId -> fileNotFound fileId
    SESndFileNotFound fileId -> fileNotFound fileId
    SERcvFileNotFound fileId -> fileNotFound fileId
    SEDuplicateContactLink -> ["you already have chat address, to show: " <> highlight' "/sa"]
    SEUserContactLinkNotFound -> ["no chat address, to create: " <> highlight' "/ad"]
    SEContactRequestNotFoundByName c -> ["no contact request from " <> ttyContact c]
    SEConnectionNotFound _ -> [] -- TODO mutes delete group error, but also mutes any error from getConnectionEntity
    e -> ["chat db error: " <> sShow e]
  ChatErrorAgent err -> case err of
    SMP SMP.AUTH -> ["error: this connection is deleted"]
    e -> ["smp agent error: " <> sShow e]
  ChatErrorMessage e -> ["chat message error: " <> sShow e]
  ChatErrorNotImplemented -> ["chat error: not implemented"]
  where
    fileNotFound fileId = ["file " <> sShow fileId <> " not found"]

ttyContact :: ContactName -> StyledString
ttyContact = styled (Colored Green)

ttyContact' :: Contact -> StyledString
ttyContact' Contact {localDisplayName = c} = ttyContact c

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

ttyGroup' :: GroupInfo -> StyledString
ttyGroup' = ttyGroup . groupName'

ttyGroups :: [GroupName] -> StyledString
ttyGroups [] = ""
ttyGroups [g] = ttyGroup g
ttyGroups (g : gs) = ttyGroup g <> ", " <> ttyGroups gs

ttyFullGroup :: GroupInfo -> StyledString
ttyFullGroup GroupInfo {localDisplayName = g, groupProfile = GroupProfile {fullName}} =
  ttyGroup g <> optFullName g fullName

ttyFromGroup :: GroupInfo -> ContactName -> StyledString
ttyFromGroup GroupInfo {localDisplayName = g} c = styled (Colored Yellow) $ "#" <> g <> " " <> c <> "> "

ttyToGroup :: GroupInfo -> StyledString
ttyToGroup GroupInfo {localDisplayName = g} = styled (Colored Cyan) $ "#" <> g <> " "

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
