{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Simplex.Chat.View where

import Data.Aeson (ToJSON)
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Function (on)
import Data.Int (Int64)
import Data.List (groupBy, intercalate, intersperse, partition, sortOn)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (DiffTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (ZonedTime (..), localDay, localTimeOfDay, timeOfDayToTime, utcToZonedTime)
import GHC.Generics (Generic)
import qualified Network.HTTP.Types as Q
import Numeric (showFFloat)
import Simplex.Chat (maxImageSize)
import Simplex.Chat.Call
import Simplex.Chat.Controller
import Simplex.Chat.Help
import Simplex.Chat.Markdown
import Simplex.Chat.Messages hiding (NewChatItem (..))
import Simplex.Chat.Protocol
import Simplex.Chat.Store (StoreError (..))
import Simplex.Chat.Styled
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Env.SQLite (NetworkConfig (..))
import Simplex.Messaging.Agent.Protocol
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (dropPrefix, taggedObjectJSON)
import Simplex.Messaging.Protocol (ProtocolServer (..))
import qualified Simplex.Messaging.Protocol as SMP
import Simplex.Messaging.Util (bshow)
import System.Console.ANSI.Types

serializeChatResponse :: ChatResponse -> String
serializeChatResponse = unlines . map unStyle . responseToView False

responseToView :: Bool -> ChatResponse -> [StyledString]
responseToView testView = \case
  CRActiveUser User {profile} -> viewUserProfile profile
  CRChatStarted -> ["chat started"]
  CRChatRunning -> ["chat is running"]
  CRChatStopped -> ["chat stopped"]
  CRChatSuspended -> ["chat suspended"]
  CRApiChats chats -> if testView then testViewChats chats else [plain . bshow $ J.encode chats]
  CRApiChat chat -> if testView then testViewChat chat else [plain . bshow $ J.encode chat]
  CRApiParsedMarkdown ft -> [plain . bshow $ J.encode ft]
  CRUserSMPServers smpServers -> viewSMPServers smpServers testView
  CRNetworkConfig cfg -> viewNetworkConfig cfg
  CRContactInfo ct cStats -> viewContactInfo ct cStats
  CRGroupMemberInfo g m cStats -> viewGroupMemberInfo g m cStats
  CRNewChatItem (AChatItem _ _ chat item) -> viewChatItem chat item False
  CRLastMessages chatItems -> concatMap (\(AChatItem _ _ chat item) -> viewChatItem chat item True) chatItems
  CRChatItemStatusUpdated _ -> []
  CRChatItemUpdated (AChatItem _ _ chat item) -> viewItemUpdate chat item
  CRChatItemDeleted (AChatItem _ _ chat deletedItem) (AChatItem _ _ _ toItem) -> viewItemDelete chat deletedItem toItem
  CRChatItemDeletedNotFound Contact {localDisplayName = c} _ -> [ttyFrom $ c <> "> [deleted - original message not found]"]
  CRBroadcastSent mc n ts -> viewSentBroadcast mc n ts
  CRMsgIntegrityError mErr -> viewMsgIntegrityError mErr
  CRCmdAccepted _ -> []
  CRCmdOk -> ["ok"]
  CRChatHelp section -> case section of
    HSMain -> chatHelpInfo
    HSFiles -> filesHelpInfo
    HSGroups -> groupsHelpInfo
    HSMyAddress -> myAddressHelpInfo
    HSMessages -> messagesHelpInfo
    HSMarkdown -> markdownInfo
    HSSettings -> settingsInfo
  CRWelcome user -> chatWelcome user
  CRContactsList cs -> viewContactsList cs
  CRUserContactLink cReqUri autoAccept autoReply -> connReqContact_ "Your chat address:" cReqUri <> autoAcceptStatus_ autoAccept autoReply
  CRUserContactLinkUpdated _ autoAccept autoReply -> autoAcceptStatus_ autoAccept autoReply
  CRContactRequestRejected UserContactRequest {localDisplayName = c} -> [ttyContact c <> ": contact request rejected"]
  CRGroupCreated g -> viewGroupCreated g
  CRGroupMembers g -> viewGroupMembers g
  CRGroupsList gs -> viewGroupsList gs
  CRSentGroupInvitation g c -> ["invitation to join the group " <> ttyGroup' g <> " sent to " <> ttyContact' c]
  CRFileTransferStatus ftStatus -> viewFileTransferStatus ftStatus
  CRUserProfile p -> viewUserProfile p
  CRUserProfileNoChange -> ["user profile did not change"]
  CRVersionInfo _ -> [plain versionStr, plain updateStr]
  CRChatCmdError e -> viewChatError e
  CRInvitation cReq -> viewConnReqInvitation cReq
  CRSentConfirmation -> ["confirmation sent!"]
  CRSentInvitation -> ["connection request sent!"]
  CRContactDeleted c -> [ttyContact' c <> ": contact is deleted"]
  CRChatCleared chatInfo -> viewChatCleared chatInfo
  CRAcceptingContactRequest c -> [ttyFullContact c <> ": accepting contact request..."]
  CRContactAlreadyExists c -> [ttyFullContact c <> ": contact already exists"]
  CRContactRequestAlreadyAccepted c -> [ttyFullContact c <> ": sent you a duplicate contact request, but you are already connected, no action needed"]
  CRUserContactLinkCreated cReq -> connReqContact_ "Your new chat address is created!" cReq
  CRUserContactLinkDeleted -> viewUserContactLinkDeleted
  CRUserAcceptedGroupSent _g -> [] -- [ttyGroup' g <> ": joining the group..."]
  CRUserDeletedMember g m -> [ttyGroup' g <> ": you removed " <> ttyMember m <> " from the group"]
  CRLeftMemberUser g -> [ttyGroup' g <> ": you left the group"] <> groupPreserved g
  CRGroupDeletedUser g -> [ttyGroup' g <> ": you deleted the group"]
  CRRcvFileAccepted ci -> savingFile' ci
  CRRcvFileAcceptedSndCancelled ft -> viewRcvFileSndCancelled ft
  CRSndGroupFileCancelled _ ftm fts -> viewSndGroupFileCancelled ftm fts
  CRRcvFileCancelled ft -> receivingFile_ "cancelled" ft
  CRUserProfileUpdated p p' -> viewUserProfileUpdated p p'
  CRContactUpdated c c' -> viewContactUpdated c c'
  CRContactsMerged intoCt mergedCt -> viewContactsMerged intoCt mergedCt
  CRReceivedContactRequest UserContactRequest {localDisplayName = c, profile} -> viewReceivedContactRequest c profile
  CRRcvFileStart ci -> receivingFile_' "started" ci
  CRRcvFileComplete ci -> receivingFile_' "completed" ci
  CRRcvFileSndCancelled ft -> viewRcvFileSndCancelled ft
  CRSndFileStart _ ft -> sendingFile_ "started" ft
  CRSndFileComplete _ ft -> sendingFile_ "completed" ft
  CRSndFileCancelled _ ft -> sendingFile_ "cancelled" ft
  CRSndFileRcvCancelled _ ft@SndFileTransfer {recipientDisplayName = c} ->
    [ttyContact c <> " cancelled receiving " <> sndFile ft]
  CRContactConnecting _ -> []
  CRContactConnected ct -> [ttyFullContact ct <> ": contact is connected"]
  CRContactAnotherClient c -> [ttyContact' c <> ": contact is connected to another client"]
  CRContactsDisconnected srv cs -> [plain $ "server disconnected " <> smpServer srv <> " (" <> contactList cs <> ")"]
  CRContactsSubscribed srv cs -> [plain $ "server connected " <> smpServer srv <> " (" <> contactList cs <> ")"]
  CRContactSubError c e -> [ttyContact' c <> ": contact error " <> sShow e]
  CRContactSubSummary summary ->
    [sShow (length subscribed) <> " contacts connected (use " <> highlight' "/cs" <> " for the list)" | not (null subscribed)] <> viewErrorsSummary errors " contact errors"
    where
      (errors, subscribed) = partition (isJust . contactError) summary
  CRGroupInvitation GroupInfo {localDisplayName = ldn, groupProfile = GroupProfile {fullName}} ->
    [groupInvitation' ldn fullName]
  CRReceivedGroupInvitation g c role -> viewReceivedGroupInvitation g c role
  CRUserJoinedGroup g _ -> [ttyGroup' g <> ": you joined the group"]
  CRJoinedGroupMember g m -> [ttyGroup' g <> ": " <> ttyMember m <> " joined the group "]
  CRJoinedGroupMemberConnecting g host m -> [ttyGroup' g <> ": " <> ttyMember host <> " added " <> ttyFullMember m <> " to the group (connecting...)"]
  CRConnectedToGroupMember g m -> [ttyGroup' g <> ": " <> connectedMember m <> " is connected"]
  CRDeletedMemberUser g by -> [ttyGroup' g <> ": " <> ttyMember by <> " removed you from the group"] <> groupPreserved g
  CRDeletedMember g by m -> [ttyGroup' g <> ": " <> ttyMember by <> " removed " <> ttyMember m <> " from the group"]
  CRLeftMember g m -> [ttyGroup' g <> ": " <> ttyMember m <> " left the group"]
  CRGroupEmpty g -> [ttyFullGroup g <> ": group is empty"]
  CRGroupRemoved g -> [ttyFullGroup g <> ": you are no longer a member or group deleted"]
  CRGroupDeleted g m -> [ttyGroup' g <> ": " <> ttyMember m <> " deleted the group", "use " <> highlight ("/d #" <> groupName' g) <> " to delete the local copy of the group"]
  CRGroupUpdated g g' m -> viewGroupUpdated g g' m
  CRMemberSubError g m e -> [ttyGroup' g <> " member " <> ttyMember m <> " error: " <> sShow e]
  CRMemberSubSummary summary -> viewErrorsSummary (filter (isJust . memberError) summary) " group member errors"
  CRGroupSubscribed g -> [ttyFullGroup g <> ": connected to server(s)"]
  CRPendingSubSummary _ -> []
  CRSndFileSubError SndFileTransfer {fileId, fileName} e ->
    ["sent file " <> sShow fileId <> " (" <> plain fileName <> ") error: " <> sShow e]
  CRRcvFileSubError RcvFileTransfer {fileId, fileInvitation = FileInvitation {fileName}} e ->
    ["received file " <> sShow fileId <> " (" <> plain fileName <> ") error: " <> sShow e]
  CRCallInvitation RcvCallInvitation {contact, callType, sharedKey} -> viewCallInvitation contact callType sharedKey
  CRCallOffer {contact, callType, offer, sharedKey} -> viewCallOffer contact callType offer sharedKey
  CRCallAnswer {contact, answer} -> viewCallAnswer contact answer
  CRCallExtraInfo {contact} -> ["call extra info from " <> ttyContact' contact]
  CRCallEnded {contact} -> ["call with " <> ttyContact' contact <> " ended"]
  CRCallInvitations _ -> []
  CRUserContactLinkSubscribed -> ["Your address is active! To show: " <> highlight' "/sa"]
  CRUserContactLinkSubError e -> ["user address error: " <> sShow e, "to delete your address: " <> highlight' "/da"]
  CRNewContactConnection _ -> []
  CRContactConnectionDeleted PendingContactConnection {pccConnId} -> ["connection :" <> sShow pccConnId <> " deleted"]
  CRNtfTokenStatus status -> ["device token status: " <> plain (smpEncode status)]
  CRNtfToken _ status mode -> ["device token status: " <> plain (smpEncode status) <> ", notifications mode: " <> plain (strEncode mode)]
  CRNtfMessages {} -> []
  CRMessageError prefix err -> [plain prefix <> ": " <> plain err]
  CRChatError e -> viewChatError e
  where
    testViewChats :: [AChat] -> [StyledString]
    testViewChats chats = [sShow $ map toChatView chats]
      where
        toChatView :: AChat -> (Text, Text, Maybe ConnStatus)
        toChatView (AChat _ (Chat (DirectChat Contact {localDisplayName, activeConn}) items _)) = ("@" <> localDisplayName, toCIPreview items, Just $ connStatus activeConn)
        toChatView (AChat _ (Chat (GroupChat GroupInfo {localDisplayName}) items _)) = ("#" <> localDisplayName, toCIPreview items, Nothing)
        toChatView (AChat _ (Chat (ContactRequest UserContactRequest {localDisplayName}) items _)) = ("<@" <> localDisplayName, toCIPreview items, Nothing)
        toChatView (AChat _ (Chat (ContactConnection PendingContactConnection {pccConnId, pccConnStatus}) items _)) = (":" <> T.pack (show pccConnId), toCIPreview items, Just $ pccConnStatus)
        toCIPreview :: [CChatItem c] -> Text
        toCIPreview ((CChatItem _ ChatItem {meta}) : _) = itemText meta
        toCIPreview _ = ""
    testViewChat :: AChat -> [StyledString]
    testViewChat (AChat _ Chat {chatItems}) = [sShow $ map toChatView chatItems]
      where
        toChatView :: CChatItem c -> ((Int, Text), Maybe (Int, Text), Maybe String)
        toChatView (CChatItem dir ChatItem {meta, quotedItem, file}) =
          ((msgDirectionInt $ toMsgDirection dir, itemText meta), qItem, fPath)
          where
            qItem = case quotedItem of
              Nothing -> Nothing
              Just CIQuote {chatDir = quoteDir, content} ->
                Just (msgDirectionInt $ quoteMsgDirection quoteDir, msgContentText content)
            fPath = case file of
              Just CIFile {filePath = Just fp} -> Just fp
              _ -> Nothing
    viewErrorsSummary :: [a] -> StyledString -> [StyledString]
    viewErrorsSummary summary s = [ttyError (T.pack . show $ length summary) <> s <> " (run with -c option to show each error)" | not (null summary)]
    smpServer :: SMPServer -> String
    smpServer SMP.ProtocolServer {host, port} = B.unpack . strEncode $ SrvLoc host port
    contactList :: [ContactRef] -> String
    contactList cs = T.unpack . T.intercalate ", " $ map (\ContactRef {localDisplayName = n} -> "@" <> n) cs

viewChatItem :: MsgDirectionI d => ChatInfo c -> ChatItem c d -> Bool -> [StyledString]
viewChatItem chat ChatItem {chatDir, meta, content, quotedItem, file} doShow = case chat of
  DirectChat c -> case chatDir of
    CIDirectSnd -> case content of
      CISndMsgContent mc -> withSndFile to $ sndMsg to quote mc
      CISndDeleted _ -> showSndItem to
      CISndCall {} -> showSndItem to
      CISndGroupInvitation {} -> showSndItem to
      CISndGroupEvent {} -> showSndItemProhibited to
      where
        to = ttyToContact' c
    CIDirectRcv -> case content of
      CIRcvMsgContent mc -> withRcvFile from $ rcvMsg from quote mc
      CIRcvDeleted _ -> showRcvItem from
      CIRcvCall {} -> showRcvItem from
      CIRcvIntegrityError err -> viewRcvIntegrityError from err meta
      CIRcvGroupInvitation {} -> showRcvItem from
      CIRcvGroupEvent {} -> showRcvItemProhibited from
      where
        from = ttyFromContact' c
    where
      quote = maybe [] (directQuote chatDir) quotedItem
  GroupChat g -> case chatDir of
    CIGroupSnd -> case content of
      CISndMsgContent mc -> withSndFile to $ sndMsg to quote mc
      CISndDeleted _ -> showSndItem to
      CISndCall {} -> showSndItem to
      CISndGroupInvitation {} -> showSndItemProhibited to
      CISndGroupEvent {} -> showSndItem to
      where
        to = ttyToGroup g
    CIGroupRcv m -> case content of
      CIRcvMsgContent mc -> withRcvFile from $ rcvMsg from quote mc
      CIRcvDeleted _ -> showRcvItem from
      CIRcvCall {} -> showRcvItem from
      CIRcvIntegrityError err -> viewRcvIntegrityError from err meta
      CIRcvGroupInvitation {} -> showRcvItemProhibited from
      CIRcvGroupEvent {} -> showRcvItem from
      where
        from = ttyFromGroup' g m
    where
      quote = maybe [] (groupQuote g) quotedItem
  _ -> []
  where
    withSndFile = withFile viewSentFileInvitation
    withRcvFile = withFile viewReceivedFileInvitation
    withFile view dir l = maybe l (\f -> l <> view dir f meta) file
    sndMsg = msg viewSentMessage
    rcvMsg = msg viewReceivedMessage
    msg view dir quote mc = case (msgContentText mc, file, quote) of
      ("", Just _, []) -> []
      ("", Just CIFile {fileName}, _) -> view dir quote (MCText $ T.pack fileName) meta
      _ -> view dir quote mc meta
    showSndItem to = showItem $ sentWithTime_ [to <> plainContent content] meta
    showRcvItem from = showItem $ receivedWithTime_ from [] meta [plainContent content]
    showSndItemProhibited to = showItem $ sentWithTime_ [to <> plainContent content <> " " <> prohibited] meta
    showRcvItemProhibited from = showItem $ receivedWithTime_ from [] meta [plainContent content <> " " <> prohibited]
    showItem ss = if doShow then ss else []
    plainContent = plain . ciContentToText
    prohibited = styled (colored Red) ("[prohibited - it's a bug if this chat item was created in this context, please report it to dev team]" :: String)

viewItemUpdate :: MsgDirectionI d => ChatInfo c -> ChatItem c d -> [StyledString]
viewItemUpdate chat ChatItem {chatDir, meta, content, quotedItem} = case chat of
  DirectChat Contact {localDisplayName = c} -> case chatDir of
    CIDirectRcv -> case content of
      CIRcvMsgContent mc -> viewReceivedMessage from quote mc meta
      _ -> []
      where
        from = ttyFromContactEdited c
        quote = maybe [] (directQuote chatDir) quotedItem
    CIDirectSnd -> ["message updated"]
  GroupChat g -> case chatDir of
    CIGroupRcv GroupMember {localDisplayName = m} -> case content of
      CIRcvMsgContent mc -> viewReceivedMessage from quote mc meta
      _ -> []
      where
        from = ttyFromGroupEdited g m
        quote = maybe [] (groupQuote g) quotedItem
    CIGroupSnd -> ["message updated"]
  _ -> []

viewItemDelete :: ChatInfo c -> ChatItem c d -> ChatItem c' d' -> [StyledString]
viewItemDelete chat ChatItem {chatDir, meta, content = deletedContent} ChatItem {content = toContent} = case chat of
  DirectChat Contact {localDisplayName = c} -> case (chatDir, deletedContent, toContent) of
    (CIDirectRcv, CIRcvMsgContent mc, CIRcvDeleted mode) -> case mode of
      CIDMBroadcast -> viewReceivedMessage (ttyFromContactDeleted c) [] mc meta
      CIDMInternal -> ["message deleted"]
    _ -> ["message deleted"]
  GroupChat g -> case (chatDir, deletedContent, toContent) of
    (CIGroupRcv GroupMember {localDisplayName = m}, CIRcvMsgContent mc, CIRcvDeleted mode) -> case mode of
      CIDMBroadcast -> viewReceivedMessage (ttyFromGroupDeleted g m) [] mc meta
      CIDMInternal -> ["message deleted"]
    _ -> ["message deleted"]
  _ -> []

directQuote :: forall d'. MsgDirectionI d' => CIDirection 'CTDirect d' -> CIQuote 'CTDirect -> [StyledString]
directQuote _ CIQuote {content = qmc, chatDir = quoteDir} =
  quoteText qmc $ if toMsgDirection (msgDirection @d') == quoteMsgDirection quoteDir then ">>" else ">"

groupQuote :: GroupInfo -> CIQuote 'CTGroup -> [StyledString]
groupQuote g CIQuote {content = qmc, chatDir = quoteDir} = quoteText qmc . ttyQuotedMember $ sentByMember g quoteDir

sentByMember :: GroupInfo -> CIQDirection 'CTGroup -> Maybe GroupMember
sentByMember GroupInfo {membership} = \case
  CIQGroupSnd -> Just membership
  CIQGroupRcv m -> m

quoteText :: MsgContent -> StyledString -> [StyledString]
quoteText qmc sentBy = prependFirst (sentBy <> " ") $ msgPreview qmc

msgPreview :: MsgContent -> [StyledString]
msgPreview = msgPlain . preview . msgContentText
  where
    preview t
      | T.length t <= 120 = t
      | otherwise = T.take 120 t <> "..."

viewRcvIntegrityError :: StyledString -> MsgErrorType -> CIMeta 'MDRcv -> [StyledString]
viewRcvIntegrityError from msgErr meta = receivedWithTime_ from [] meta $ viewMsgIntegrityError msgErr

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
    msgError s = [ttyError s]

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

viewChatCleared :: AChatInfo -> [StyledString]
viewChatCleared (AChatInfo _ chatInfo) = case chatInfo of
  DirectChat ct -> [ttyContact' ct <> ": all messages are removed locally ONLY"]
  GroupChat gi -> [ttyGroup' gi <> ": all messages are removed locally ONLY"]
  _ -> []

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

autoAcceptStatus_ :: Bool -> Maybe MsgContent -> [StyledString]
autoAcceptStatus_ autoAccept autoReply =
  ("auto_accept " <> if autoAccept then "on" else "off") :
  maybe [] ((["auto reply:"] <>) . ttyMsgContent) autoReply

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
        GSMemInvited -> groupInvitation' ldn fullName
        _ -> ttyGroup ldn <> optFullName ldn fullName

groupInvitation' :: GroupName -> Text -> StyledString
groupInvitation' displayName fullName =
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

viewSMPServers :: [SMPServer] -> Bool -> [StyledString]
viewSMPServers smpServers testView =
  if testView
    then [customSMPServers]
    else
      [ customSMPServers,
        "",
        "use " <> highlight' "/smp_servers <srv1[,srv2,...]>" <> " to switch to custom SMP servers",
        "use " <> highlight' "/smp_servers default" <> " to remove custom SMP servers and use default",
        "(chat option " <> highlight' "-s" <> " (" <> highlight' "--server" <> ") has precedence over saved SMP servers for chat session)"
      ]
  where
    customSMPServers =
      if null smpServers
        then "no custom SMP servers saved"
        else viewServers smpServers

viewNetworkConfig :: NetworkConfig -> [StyledString]
viewNetworkConfig NetworkConfig {socksProxy, tcpTimeout} =
  [ plain $ maybe "direct network connection" (("using SOCKS5 proxy " <>) . show) socksProxy,
    "TCP timeout: " <> sShow tcpTimeout,
    "use `/network socks=<on/off/[ipv4]:port>[ timeout=<seconds>]` to change settings"
  ]

viewContactInfo :: Contact -> ConnectionStats -> [StyledString]
viewContactInfo Contact {contactId} stats =
  ["contact ID: " <> sShow contactId] <> viewConnectionStats stats

viewGroupMemberInfo :: GroupInfo -> GroupMember -> Maybe ConnectionStats -> [StyledString]
viewGroupMemberInfo GroupInfo {groupId} GroupMember {groupMemberId} stats =
  [ "group ID: " <> sShow groupId,
    "member ID: " <> sShow groupMemberId
  ]
    <> maybe ["member not connected"] viewConnectionStats stats

viewConnectionStats :: ConnectionStats -> [StyledString]
viewConnectionStats ConnectionStats {rcvServers, sndServers} =
  ["receiving messages via: " <> viewServerHosts rcvServers | not $ null rcvServers]
    <> ["sending messages via: " <> viewServerHosts sndServers | not $ null sndServers]

viewServers :: [SMPServer] -> StyledString
viewServers = plain . intercalate ", " . map (B.unpack . strEncode)

viewServerHosts :: [SMPServer] -> StyledString
viewServerHosts = plain . intercalate ", " . map host

viewUserProfileUpdated :: Profile -> Profile -> [StyledString]
viewUserProfileUpdated Profile {displayName = n, fullName, image} Profile {displayName = n', fullName = fullName', image = image'}
  | n == n' && fullName == fullName' && image == image' = []
  | n == n' && fullName == fullName' = [if isNothing image' then "profile image removed" else "profile image updated"]
  | n == n' = ["user full name " <> (if T.null fullName' || fullName' == n' then "removed" else "changed to " <> plain fullName') <> notified]
  | otherwise = ["user profile is changed to " <> ttyFullName n' fullName' <> notified]
  where
    notified = " (your contacts are notified)"

viewGroupUpdated :: GroupInfo -> GroupInfo -> Maybe GroupMember -> [StyledString]
viewGroupUpdated
  GroupInfo {localDisplayName = n, groupProfile = GroupProfile {fullName, image}}
  g'@GroupInfo {localDisplayName = n', groupProfile = GroupProfile {fullName = fullName', image = image'}}
  m
    | n == n' && fullName == fullName' && image == image' = []
    | n == n' && fullName == fullName' = ["group " <> ttyGroup n <> ": profile image " <> (if isNothing image' then "removed" else "updated") <> byMember]
    | n == n' = ["group " <> ttyGroup n <> ": full name " <> if T.null fullName' || fullName' == n' then "removed" else "changed to " <> plain fullName' <> byMember]
    | otherwise = ["group " <> ttyGroup n <> " is changed to " <> ttyFullGroup g' <> byMember]
    where
      byMember = maybe "" ((" by " <>) . ttyMember) m

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

viewReceivedMessage :: StyledString -> [StyledString] -> MsgContent -> CIMeta d -> [StyledString]
viewReceivedMessage from quote mc meta = receivedWithTime_ from quote meta (ttyMsgContent mc)

receivedWithTime_ :: StyledString -> [StyledString] -> CIMeta d -> [StyledString] -> [StyledString]
receivedWithTime_ from quote CIMeta {localItemTs, createdAt} styledMsg = do
  prependFirst (formattedTime <> " " <> from) (quote <> prependFirst indent styledMsg)
  where
    indent = if null quote then "" else "      "
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

viewSentMessage :: StyledString -> [StyledString] -> MsgContent -> CIMeta d -> [StyledString]
viewSentMessage to quote mc = sentWithTime_ (prependFirst to $ quote <> prependFirst indent (ttyMsgContent mc))
  where
    indent = if null quote then "" else "      "

viewSentBroadcast :: MsgContent -> Int -> ZonedTime -> [StyledString]
viewSentBroadcast mc n ts = prependFirst (highlight' "/feed" <> " (" <> sShow n <> ") " <> ttyMsgTime ts <> " ") (ttyMsgContent mc)

viewSentFileInvitation :: StyledString -> CIFile d -> CIMeta d -> [StyledString]
viewSentFileInvitation to CIFile {fileId, filePath} = case filePath of
  Just fPath -> sentWithTime_ $ ttySentFile to fileId fPath
  _ -> const []

sentWithTime_ :: [StyledString] -> CIMeta d -> [StyledString]
sentWithTime_ styledMsg CIMeta {localItemTs} =
  prependFirst (ttyMsgTime localItemTs <> " ") styledMsg

ttyMsgTime :: ZonedTime -> StyledString
ttyMsgTime = styleTime . formatTime defaultTimeLocale "%H:%M"

ttyMsgContent :: MsgContent -> [StyledString]
ttyMsgContent = msgPlain . msgContentText

ttySentFile :: StyledString -> FileTransferId -> FilePath -> [StyledString]
ttySentFile to fId fPath = ["/f " <> to <> ttyFilePath fPath, "use " <> highlight ("/fc " <> show fId) <> " to cancel sending"]

prependFirst :: StyledString -> [StyledString] -> [StyledString]
prependFirst s [] = [s]
prependFirst s (s' : ss) = (s <> s') : ss

msgPlain :: Text -> [StyledString]
msgPlain = map (styleMarkdownList . parseMarkdownList) . T.lines

viewRcvFileSndCancelled :: RcvFileTransfer -> [StyledString]
viewRcvFileSndCancelled ft@RcvFileTransfer {senderDisplayName = c} =
  [ttyContact c <> " cancelled sending " <> rcvFile ft]

viewSndGroupFileCancelled :: FileTransferMeta -> [SndFileTransfer] -> [StyledString]
viewSndGroupFileCancelled FileTransferMeta {fileId, fileName} fts =
  case filter (\SndFileTransfer {fileStatus = s} -> s /= FSCancelled && s /= FSComplete) fts of
    [] -> ["cancelled sending " <> fileTransferStr fileId fileName]
    ts -> ["cancelled sending " <> fileTransferStr fileId fileName <> " to " <> listRecipients ts]

sendingFile_ :: StyledString -> SndFileTransfer -> [StyledString]
sendingFile_ status ft@SndFileTransfer {recipientDisplayName = c} =
  [status <> " sending " <> sndFile ft <> " to " <> ttyContact c]

sndFile :: SndFileTransfer -> StyledString
sndFile SndFileTransfer {fileId, fileName} = fileTransferStr fileId fileName

viewReceivedFileInvitation :: StyledString -> CIFile d -> CIMeta d -> [StyledString]
viewReceivedFileInvitation from file meta = receivedWithTime_ from [] meta (receivedFileInvitation_ file)

receivedFileInvitation_ :: CIFile d -> [StyledString]
receivedFileInvitation_ CIFile {fileId, fileName, fileSize} =
  [ "sends file " <> ttyFilePath fileName <> " (" <> humanReadableSize fileSize <> " / " <> sShow fileSize <> " bytes)",
    -- below is printed for auto-accepted files as well; auto-accept is disabled in terminal though so in reality it never happens
    "use " <> highlight ("/fr " <> show fileId <> " [<dir>/ | <path>]") <> " to receive it"
  ]

-- TODO remove
viewReceivedFileInvitation' :: StyledString -> RcvFileTransfer -> CIMeta d -> [StyledString]
viewReceivedFileInvitation' from RcvFileTransfer {fileId, fileInvitation = FileInvitation {fileName, fileSize}} meta = receivedWithTime_ from [] meta (receivedFileInvitation_' fileId fileName fileSize)

receivedFileInvitation_' :: Int64 -> String -> Integer -> [StyledString]
receivedFileInvitation_' fileId fileName fileSize =
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

savingFile' :: AChatItem -> [StyledString]
savingFile' (AChatItem _ _ (DirectChat Contact {localDisplayName = c}) ChatItem {file = Just CIFile {fileId, filePath = Just filePath}, chatDir = CIDirectRcv}) =
  ["saving file " <> sShow fileId <> " from " <> ttyContact c <> " to " <> plain filePath]
savingFile' (AChatItem _ _ _ ChatItem {file = Just CIFile {fileId, filePath = Just filePath}, chatDir = CIGroupRcv GroupMember {localDisplayName = m}}) =
  ["saving file " <> sShow fileId <> " from " <> ttyContact m <> " to " <> plain filePath]
savingFile' (AChatItem _ _ _ ChatItem {file = Just CIFile {fileId, filePath = Just filePath}}) =
  ["saving file " <> sShow fileId <> " to " <> plain filePath]
savingFile' _ = ["saving file"] -- shouldn't happen

receivingFile_' :: StyledString -> AChatItem -> [StyledString]
receivingFile_' status (AChatItem _ _ (DirectChat Contact {localDisplayName = c}) ChatItem {file = Just CIFile {fileId, fileName}, chatDir = CIDirectRcv}) =
  [status <> " receiving " <> fileTransferStr fileId fileName <> " from " <> ttyContact c]
receivingFile_' status (AChatItem _ _ _ ChatItem {file = Just CIFile {fileId, fileName}, chatDir = CIGroupRcv GroupMember {localDisplayName = m}}) =
  [status <> " receiving " <> fileTransferStr fileId fileName <> " from " <> ttyContact m]
receivingFile_' status _ = [status <> " receiving file"] -- shouldn't happen

receivingFile_ :: StyledString -> RcvFileTransfer -> [StyledString]
receivingFile_ status ft@RcvFileTransfer {senderDisplayName = c} =
  [status <> " receiving " <> rcvFile ft <> " from " <> ttyContact c]

rcvFile :: RcvFileTransfer -> StyledString
rcvFile RcvFileTransfer {fileId, fileInvitation = FileInvitation {fileName}} = fileTransferStr fileId fileName

fileTransferStr :: Int64 -> String -> StyledString
fileTransferStr fileId fileName = "file " <> sShow fileId <> " (" <> ttyFilePath fileName <> ")"

viewFileTransferStatus :: (FileTransfer, [Integer]) -> [StyledString]
viewFileTransferStatus (FTSnd FileTransferMeta {fileId, fileName, cancelled} [], _) =
  [ "sending " <> fileTransferStr fileId fileName <> ": no file transfers"
      <> if cancelled then ", file transfer cancelled" else ""
  ]
viewFileTransferStatus (FTSnd FileTransferMeta {cancelled} fts@(ft : _), chunksNum) =
  recipientStatuses <> ["file transfer cancelled" | cancelled]
  where
    recipientStatuses =
      case concatMap recipientsTransferStatus $ groupBy ((==) `on` fs) $ sortOn fs fts of
        [recipientsStatus] -> ["sending " <> sndFile ft <> " " <> recipientsStatus]
        recipientsStatuses -> ("sending " <> sndFile ft <> ": ") : map ("  " <>) recipientsStatuses
    fs = fileStatus :: SndFileTransfer -> FileStatus
    recipientsTransferStatus [] = []
    recipientsTransferStatus ts@(SndFileTransfer {fileStatus, fileSize, chunkSize} : _) = [sndStatus <> ": " <> listRecipients ts]
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
      RFSCancelled (Just RcvFileInfo {filePath}) -> "cancelled, received part path: " <> plain filePath
      RFSCancelled Nothing -> "cancelled"

listRecipients :: [SndFileTransfer] -> StyledString
listRecipients = mconcat . intersperse ", " . map (ttyContact . recipientDisplayName)

fileProgress :: [Integer] -> Integer -> Integer -> StyledString
fileProgress chunksNum chunkSize fileSize =
  sShow (sum chunksNum * chunkSize * 100 `div` fileSize) <> "% of " <> humanReadableSize fileSize

viewCallInvitation :: Contact -> CallType -> Maybe C.Key -> [StyledString]
viewCallInvitation ct@Contact {contactId} callType@CallType {media} sharedKey =
  [ ttyContact' ct <> " wants to connect with you via WebRTC " <> callMediaStr callType <> " call " <> encryptedCallText callType,
    "To accept the call, please open the link below in your browser" <> supporedBrowsers callType,
    "",
    "https://simplex.chat/call#" <> plain queryString
  ]
  where
    aesKey = B.unpack . strEncode . C.unKey <$> sharedKey
    queryString =
      Q.renderSimpleQuery
        False
        [ ("command", LB.toStrict . J.encode $ WCCallStart {media, aesKey, useWorker = True}),
          ("contact_id", B.pack $ show contactId)
        ]

viewCallOffer :: Contact -> CallType -> WebRTCSession -> Maybe C.Key -> [StyledString]
viewCallOffer ct@Contact {contactId} callType@CallType {media} WebRTCSession {rtcSession = offer, rtcIceCandidates = iceCandidates} sharedKey =
  [ ttyContact' ct <> " accepted your WebRTC " <> callMediaStr callType <> " call " <> encryptedCallText callType,
    "To connect, please open the link below in your browser" <> supporedBrowsers callType,
    "",
    "https://simplex.chat/call#" <> plain queryString
  ]
  where
    aesKey = B.unpack . strEncode . C.unKey <$> sharedKey
    queryString =
      Q.renderSimpleQuery
        False
        [ ("command", LB.toStrict . J.encode $ WCCallOffer {offer, iceCandidates, media, aesKey, useWorker = True}),
          ("contact_id", B.pack $ show contactId)
        ]

viewCallAnswer :: Contact -> WebRTCSession -> [StyledString]
viewCallAnswer ct WebRTCSession {rtcSession = answer, rtcIceCandidates = iceCandidates} =
  [ ttyContact' ct <> " continued the WebRTC call",
    "To connect, please paste the data below in your browser window you opened earlier and click Connect button",
    "",
    plain . LB.toStrict . J.encode $ WCCallAnswer {answer, iceCandidates}
  ]

callMediaStr :: CallType -> StyledString
callMediaStr CallType {media} = case media of
  CMVideo -> "video"
  CMAudio -> "audio"

encryptedCallText :: CallType -> StyledString
encryptedCallText callType
  | encryptedCall callType = "(e2e encrypted)"
  | otherwise = "(not e2e encrypted)"

supporedBrowsers :: CallType -> StyledString
supporedBrowsers callType
  | encryptedCall callType = " (only Chrome and Safari support e2e encryption for WebRTC, Safari may require enabling WebRTC insertable streams)"
  | otherwise = ""

data WCallCommand
  = WCCallStart {media :: CallMedia, aesKey :: Maybe String, useWorker :: Bool}
  | WCCallOffer {offer :: Text, iceCandidates :: Text, media :: CallMedia, aesKey :: Maybe String, useWorker :: Bool}
  | WCCallAnswer {answer :: Text, iceCandidates :: Text}
  deriving (Generic)

instance ToJSON WCallCommand where
  toEncoding = J.genericToEncoding . taggedObjectJSON $ dropPrefix "WCCall"
  toJSON = J.genericToJSON . taggedObjectJSON $ dropPrefix "WCCall"

viewChatError :: ChatError -> [StyledString]
viewChatError = \case
  ChatError err -> case err of
    CENoActiveUser -> ["error: active user is required"]
    CEActiveUserExists -> ["error: active user already exists"]
    CEChatNotStarted -> ["error: chat not started"]
    CEChatNotStopped -> ["error: chat not stopped"]
    CEChatStoreChanged -> ["error: chat store changed"]
    CEInvalidConnReq -> viewInvalidConnReq
    CEInvalidChatMessage e -> ["chat message error: " <> sShow e]
    CEContactNotReady c -> [ttyContact' c <> ": not ready"]
    CEContactGroups c gNames -> [ttyContact' c <> ": contact cannot be deleted, it is a member of the group(s) " <> ttyGroups gNames]
    CEGroupDuplicateMember c -> ["contact " <> ttyContact c <> " is already in the group"]
    CEGroupDuplicateMemberId -> ["cannot add member - duplicate member ID"]
    CEGroupUserRole -> ["you have insufficient permissions for this group command"]
    CEGroupContactRole c -> ["contact " <> ttyContact c <> " has insufficient permissions for this group action"]
    CEGroupNotJoined g -> ["you did not join this group, use " <> highlight ("/join #" <> groupName' g)]
    CEGroupMemberNotActive -> ["you cannot invite other members yet, try later"]
    CEGroupMemberUserRemoved -> ["you are no longer a member of the group"]
    CEGroupMemberNotFound -> ["group doesn't have this member"]
    CEGroupMemberIntroNotFound c -> ["group member intro not found for " <> ttyContact c]
    CEGroupCantResendInvitation g c -> viewCannotResendInvitation g c
    CEGroupInternal s -> ["chat group bug: " <> plain s]
    CEFileNotFound f -> ["file not found: " <> plain f]
    CEFileAlreadyReceiving f -> ["file is already accepted: " <> plain f]
    CEFileCancelled f -> ["file cancelled: " <> plain f]
    CEFileAlreadyExists f -> ["file already exists: " <> plain f]
    CEFileRead f e -> ["cannot read file " <> plain f, sShow e]
    CEFileWrite f e -> ["cannot write file " <> plain f, sShow e]
    CEFileSend fileId e -> ["error sending file " <> sShow fileId <> ": " <> sShow e]
    CEFileRcvChunk e -> ["error receiving file: " <> plain e]
    CEFileInternal e -> ["file error: " <> plain e]
    CEFileImageType _ -> ["max image size: " <> sShow maxImageSize <> " bytes, resize it or send as a file using " <> highlight' "/f"]
    CEFileImageSize _ -> ["image type must be JPG, send as a file using " <> highlight' "/f"]
    CEFileNotReceived fileId -> ["file " <> sShow fileId <> " not received"]
    CEInvalidQuote -> ["cannot reply to this message"]
    CEInvalidChatItemUpdate -> ["cannot update this item"]
    CEInvalidChatItemDelete -> ["cannot delete this item"]
    CEHasCurrentCall -> ["call already in progress"]
    CENoCurrentCall -> ["no call in progress"]
    CECallContact _ -> []
    CECallState _ -> []
    CEAgentVersion -> ["unsupported agent version"]
    CEAgentNoSubResult connId -> ["no subscription result for connection: " <> sShow connId]
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
    SEFileIdNotFoundBySharedMsgId _ -> [] -- recipient tried to accept cancelled file
    SEConnectionNotFound _ -> [] -- TODO mutes delete group error, but also mutes any error from getConnectionEntity
    SEQuotedChatItemNotFound -> ["message not found - reply is not sent"]
    e -> ["chat db error: " <> sShow e]
  ChatErrorAgent err -> case err of
    SMP SMP.AUTH ->
      [ "error: connection authorization failed - this could happen if connection was deleted,\
        \ secured with different credentials, or due to a bug - please re-create the connection"
      ]
    e -> ["smp agent error: " <> sShow e]
  where
    fileNotFound fileId = ["file " <> sShow fileId <> " not found"]

ttyContact :: ContactName -> StyledString
ttyContact = styled $ colored Green

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
ttyToContact c = styled (colored Cyan) $ "@" <> c <> " "

ttyFromContact :: ContactName -> StyledString
ttyFromContact c = ttyFrom $ c <> "> "

ttyFromContactEdited :: ContactName -> StyledString
ttyFromContactEdited c = ttyFrom $ c <> "> [edited] "

ttyFromContactDeleted :: ContactName -> StyledString
ttyFromContactDeleted c = ttyFrom $ c <> "> [deleted] "

ttyToContact' :: Contact -> StyledString
ttyToContact' Contact {localDisplayName = c} = ttyToContact c

ttyQuotedContact :: Contact -> StyledString
ttyQuotedContact Contact {localDisplayName = c} = ttyFrom $ c <> ">"

ttyQuotedMember :: Maybe GroupMember -> StyledString
ttyQuotedMember (Just GroupMember {localDisplayName = c}) = "> " <> ttyFrom c
ttyQuotedMember _ = "> " <> ttyFrom "?"

ttyFromContact' :: Contact -> StyledString
ttyFromContact' Contact {localDisplayName = c} = ttyFromContact c

ttyGroup :: GroupName -> StyledString
ttyGroup g = styled (colored Blue) $ "#" <> g

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
ttyFromGroup GroupInfo {localDisplayName = g} c = ttyFrom $ "#" <> g <> " " <> c <> "> "

ttyFromGroupEdited :: GroupInfo -> ContactName -> StyledString
ttyFromGroupEdited GroupInfo {localDisplayName = g} c = ttyFrom $ "#" <> g <> " " <> c <> "> [edited] "

ttyFromGroupDeleted :: GroupInfo -> ContactName -> StyledString
ttyFromGroupDeleted GroupInfo {localDisplayName = g} c = ttyFrom $ "#" <> g <> " " <> c <> "> [deleted] "

ttyFrom :: Text -> StyledString
ttyFrom = styled $ colored Yellow

ttyFromGroup' :: GroupInfo -> GroupMember -> StyledString
ttyFromGroup' g GroupMember {localDisplayName = m} = ttyFromGroup g m

ttyToGroup :: GroupInfo -> StyledString
ttyToGroup GroupInfo {localDisplayName = g} = styled (colored Cyan) $ "#" <> g <> " "

ttyFilePath :: FilePath -> StyledString
ttyFilePath = plain

optFullName :: ContactName -> Text -> StyledString
optFullName localDisplayName fullName = plain $ optionalFullName localDisplayName fullName

highlight :: StyledFormat a => a -> StyledString
highlight = styled $ colored Cyan

highlight' :: String -> StyledString
highlight' = highlight

styleTime :: String -> StyledString
styleTime = Styled [SetColor Foreground Vivid Black]

ttyError :: StyledFormat a => a -> StyledString
ttyError = styled $ colored Red

ttyError' :: String -> StyledString
ttyError' = ttyError
