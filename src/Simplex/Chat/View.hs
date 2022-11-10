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
import Data.Char (toUpper)
import Data.Function (on)
import Data.Int (Int64)
import Data.List (groupBy, intercalate, intersperse, partition, sortOn)
import Data.Maybe (isJust, isNothing, mapMaybe)
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
import Simplex.Chat.Store (AutoAccept (..), StoreError (..), UserContactLink (..))
import Simplex.Chat.Styled
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Env.SQLite (NetworkConfig (..))
import Simplex.Messaging.Agent.Protocol
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (dropPrefix, taggedObjectJSON)
import Simplex.Messaging.Protocol (AProtocolType, ProtocolServer (..))
import qualified Simplex.Messaging.Protocol as SMP
import Simplex.Messaging.Transport.Client (TransportHost (..))
import Simplex.Messaging.Util (bshow)
import System.Console.ANSI.Types

serializeChatResponse :: Maybe User -> ChatResponse -> String
serializeChatResponse user_ = unlines . map unStyle . responseToView user_ False

responseToView :: Maybe User -> Bool -> ChatResponse -> [StyledString]
responseToView user_ testView = \case
  CRActiveUser User {profile} -> viewUserProfile $ fromLocalProfile profile
  CRChatStarted -> ["chat started"]
  CRChatRunning -> ["chat is running"]
  CRChatStopped -> ["chat stopped"]
  CRChatSuspended -> ["chat suspended"]
  CRApiChats chats -> if testView then testViewChats chats else [plain . bshow $ J.encode chats]
  CRApiChat chat -> if testView then testViewChat chat else [plain . bshow $ J.encode chat]
  CRApiParsedMarkdown ft -> [plain . bshow $ J.encode ft]
  CRUserSMPServers smpServers -> viewSMPServers smpServers testView
  CRChatItemTTL ttl -> viewChatItemTTL ttl
  CRNetworkConfig cfg -> viewNetworkConfig cfg
  CRContactInfo ct cStats customUserProfile -> viewContactInfo ct cStats customUserProfile
  CRGroupMemberInfo g m cStats -> viewGroupMemberInfo g m cStats
  CRContactSwitch ct progress -> viewContactSwitch ct progress
  CRGroupMemberSwitch g m progress -> viewGroupMemberSwitch g m progress
  CRNewChatItem (AChatItem _ _ chat item) -> unmuted chat item $ viewChatItem chat item False
  CRLastMessages chatItems -> concatMap (\(AChatItem _ _ chat item) -> viewChatItem chat item True) chatItems
  CRChatItemStatusUpdated _ -> []
  CRChatItemUpdated (AChatItem _ _ chat item) -> unmuted chat item $ viewItemUpdate chat item
  CRChatItemDeleted (AChatItem _ _ chat deletedItem) (AChatItem _ _ _ toItem) -> unmuted chat deletedItem $ viewItemDelete chat deletedItem toItem
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
  CRUserContactLink UserContactLink {connReqContact, autoAccept} -> connReqContact_ "Your chat address:" connReqContact <> autoAcceptStatus_ autoAccept
  CRUserContactLinkUpdated UserContactLink {autoAccept} -> autoAcceptStatus_ autoAccept
  CRContactRequestRejected UserContactRequest {localDisplayName = c} -> [ttyContact c <> ": contact request rejected"]
  CRGroupCreated g -> viewGroupCreated g
  CRGroupMembers g -> viewGroupMembers g
  CRGroupsList gs -> viewGroupsList gs
  CRSentGroupInvitation g c _ ->
    if viaGroupLink . contactConn $ c
      then [ttyContact' c <> " invited to group " <> ttyGroup' g <> " via your group link"]
      else ["invitation to join the group " <> ttyGroup' g <> " sent to " <> ttyContact' c]
  CRFileTransferStatus ftStatus -> viewFileTransferStatus ftStatus
  CRUserProfile p -> viewUserProfile p
  CRUserProfileNoChange -> ["user profile did not change"]
  CRVersionInfo _ -> [plain versionStr, plain updateStr]
  CRChatCmdError e -> viewChatError e
  CRInvitation cReq -> viewConnReqInvitation cReq
  CRSentConfirmation -> ["confirmation sent!"]
  CRSentInvitation customUserProfile -> viewSentInvitation customUserProfile testView
  CRContactDeleted c -> [ttyContact' c <> ": contact is deleted"]
  CRChatCleared chatInfo -> viewChatCleared chatInfo
  CRAcceptingContactRequest c -> [ttyFullContact c <> ": accepting contact request..."]
  CRContactAlreadyExists c -> [ttyFullContact c <> ": contact already exists"]
  CRContactRequestAlreadyAccepted c -> [ttyFullContact c <> ": sent you a duplicate contact request, but you are already connected, no action needed"]
  CRUserContactLinkCreated cReq -> connReqContact_ "Your new chat address is created!" cReq
  CRUserContactLinkDeleted -> viewUserContactLinkDeleted
  CRUserAcceptedGroupSent _g _ -> [] -- [ttyGroup' g <> ": joining the group..."]
  CRUserDeletedMember g m -> [ttyGroup' g <> ": you removed " <> ttyMember m <> " from the group"]
  CRLeftMemberUser g -> [ttyGroup' g <> ": you left the group"] <> groupPreserved g
  CRGroupDeletedUser g -> [ttyGroup' g <> ": you deleted the group"]
  CRRcvFileAccepted ci -> savingFile' ci
  CRRcvFileAcceptedSndCancelled ft -> viewRcvFileSndCancelled ft
  CRSndGroupFileCancelled _ ftm fts -> viewSndGroupFileCancelled ftm fts
  CRRcvFileCancelled ft -> receivingFile_ "cancelled" ft
  CRUserProfileUpdated p p' -> viewUserProfileUpdated p p'
  CRContactPrefsUpdated {fromContact, toContact, preferences} -> case user_ of
    Just user -> viewUserContactPrefsUpdated user fromContact toContact preferences
    _ -> ["unexpected chat event CRContactPrefsUpdated without current user"]
  CRContactAliasUpdated c -> viewContactAliasUpdated c
  CRConnectionAliasUpdated c -> viewConnectionAliasUpdated c
  CRContactUpdated {fromContact = c, toContact = c', preferences} -> case user_ of
    Just user -> viewContactUpdated c c' <> viewContactPrefsUpdated user c c' preferences
    _ -> ["unexpected chat event CRContactUpdated without current user"]
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
  CRContactConnected ct userCustomProfile -> viewContactConnected ct userCustomProfile testView
  CRContactAnotherClient c -> [ttyContact' c <> ": contact is connected to another client"]
  CRSubscriptionEnd acEntity -> [sShow (connId (entityConnection acEntity :: Connection)) <> ": END"]
  CRContactsDisconnected srv cs -> [plain $ "server disconnected " <> showSMPServer srv <> " (" <> contactList cs <> ")"]
  CRContactsSubscribed srv cs -> [plain $ "server connected " <> showSMPServer srv <> " (" <> contactList cs <> ")"]
  CRContactSubError c e -> [ttyContact' c <> ": contact error " <> sShow e]
  CRContactSubSummary summary ->
    [sShow (length subscribed) <> " contacts connected (use " <> highlight' "/cs" <> " for the list)" | not (null subscribed)] <> viewErrorsSummary errors " contact errors"
    where
      (errors, subscribed) = partition (isJust . contactError) summary
  CRUserContactSubSummary summary ->
    map addressSS addresses
      <> ([sShow (length groupLinksSubscribed) <> " group links active" | not (null groupLinksSubscribed)] <> viewErrorsSummary groupLinkErrors " group link errors")
    where
      (addresses, groupLinks) = partition (\UserContactSubStatus {userContact} -> isNothing . userContactGroupId $ userContact) summary
      addressSS UserContactSubStatus {userContactError} = maybe ("Your address is active! To show: " <> highlight' "/sa") (\e -> "User address error: " <> sShow e <> ", to delete your address: " <> highlight' "/da") userContactError
      (groupLinkErrors, groupLinksSubscribed) = partition (isJust . userContactError) groupLinks
  CRGroupInvitation g -> [groupInvitation' g]
  CRReceivedGroupInvitation g c role -> viewReceivedGroupInvitation g c role
  CRUserJoinedGroup g _ -> viewUserJoinedGroup g
  CRJoinedGroupMember g m -> viewJoinedGroupMember g m
  CRHostConnected p h -> [plain $ "connected to " <> viewHostEvent p h]
  CRHostDisconnected p h -> [plain $ "disconnected from " <> viewHostEvent p h]
  CRJoinedGroupMemberConnecting g host m -> [ttyGroup' g <> ": " <> ttyMember host <> " added " <> ttyFullMember m <> " to the group (connecting...)"]
  CRConnectedToGroupMember g m -> [ttyGroup' g <> ": " <> connectedMember m <> " is connected"]
  CRMemberRole g by m r r' -> viewMemberRoleChanged g by m r r'
  CRMemberRoleUser g m r r' -> viewMemberRoleUserChanged g m r r'
  CRDeletedMemberUser g by -> [ttyGroup' g <> ": " <> ttyMember by <> " removed you from the group"] <> groupPreserved g
  CRDeletedMember g by m -> [ttyGroup' g <> ": " <> ttyMember by <> " removed " <> ttyMember m <> " from the group"]
  CRLeftMember g m -> [ttyGroup' g <> ": " <> ttyMember m <> " left the group"]
  CRGroupEmpty g -> [ttyFullGroup g <> ": group is empty"]
  CRGroupRemoved g -> [ttyFullGroup g <> ": you are no longer a member or group deleted"]
  CRGroupDeleted g m -> [ttyGroup' g <> ": " <> ttyMember m <> " deleted the group", "use " <> highlight ("/d #" <> groupName' g) <> " to delete the local copy of the group"]
  CRGroupUpdated g g' m -> viewGroupUpdated g g' m
  CRGroupLinkCreated g cReq -> groupLink_ "Group link is created!" g cReq
  CRGroupLink g cReq -> groupLink_ "Group link:" g cReq
  CRGroupLinkDeleted g -> viewGroupLinkDeleted g
  CRAcceptingGroupJoinRequest g c -> [ttyFullContact c <> ": accepting request to join group " <> ttyGroup' g <> "..."]
  CRMemberSubError g m e -> [ttyGroup' g <> " member " <> ttyMember m <> " error: " <> sShow e]
  CRMemberSubSummary summary -> viewErrorsSummary (filter (isJust . memberError) summary) " group member errors"
  CRGroupSubscribed g -> viewGroupSubscribed g
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
  CRSQLResult rows -> map plain rows
  CRDebugLocks {chatLockName, agentLocks} ->
    [ maybe "no chat lock" (("chat lock: " <>) . plain) chatLockName,
      plain $ "agent locks: " <> LB.unpack (J.encode agentLocks)
    ]
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
        toChatView (AChat _ (Chat (ContactConnection PendingContactConnection {pccConnId, pccConnStatus}) items _)) = (":" <> T.pack (show pccConnId), toCIPreview items, Just pccConnStatus)
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
    contactList :: [ContactRef] -> String
    contactList cs = T.unpack . T.intercalate ", " $ map (\ContactRef {localDisplayName = n} -> "@" <> n) cs
    unmuted :: ChatInfo c -> ChatItem c d -> [StyledString] -> [StyledString]
    unmuted chat ChatItem {chatDir} s = case (chat, chatDir) of
      (DirectChat Contact {chatSettings = DisableNtfs}, CIDirectRcv) -> []
      (GroupChat GroupInfo {chatSettings = DisableNtfs}, CIGroupRcv _) -> []
      _ -> s

viewGroupSubscribed :: GroupInfo -> [StyledString]
viewGroupSubscribed g@GroupInfo {membership} =
  [incognito <> ttyFullGroup g <> ": connected to server(s)"]
  where
    incognito = if memberIncognito membership then incognitoPrefix else ""

showSMPServer :: SMPServer -> String
showSMPServer = B.unpack . strEncode . host

viewHostEvent :: AProtocolType -> TransportHost -> String
viewHostEvent p h = map toUpper (B.unpack $ strEncode p) <> " host " <> B.unpack (strEncode h)

viewChatItem :: forall c d. MsgDirectionI d => ChatInfo c -> ChatItem c d -> Bool -> [StyledString]
viewChatItem chat ChatItem {chatDir, meta, content, quotedItem, file} doShow = case chat of
  DirectChat c -> case chatDir of
    CIDirectSnd -> case content of
      CISndMsgContent mc -> withSndFile to $ sndMsg to quote mc
      CISndGroupEvent {} -> showSndItemProhibited to
      _ -> showSndItem to
      where
        to = ttyToContact' c
    CIDirectRcv -> case content of
      CIRcvMsgContent mc -> withRcvFile from $ rcvMsg from quote mc
      CIRcvIntegrityError err -> viewRcvIntegrityError from err meta
      CIRcvGroupEvent {} -> showRcvItemProhibited from
      _ -> showRcvItem from
      where
        from = ttyFromContact' c
    where
      quote = maybe [] (directQuote chatDir) quotedItem
  GroupChat g -> case chatDir of
    CIGroupSnd -> case content of
      CISndMsgContent mc -> withSndFile to $ sndMsg to quote mc
      CISndGroupInvitation {} -> showSndItemProhibited to
      _ -> showSndItem to
      where
        to = ttyToGroup g
    CIGroupRcv m -> case content of
      CIRcvMsgContent mc -> withRcvFile from $ rcvMsg from quote mc
      CIRcvIntegrityError err -> viewRcvIntegrityError from err meta
      CIRcvGroupInvitation {} -> showRcvItemProhibited from
      _ -> showRcvItem from
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
      incognito ct = if contactConnIncognito ct then incognitoPrefix else ""
   in map (\ct -> incognito ct <> ttyFullContact ct <> muted ct <> alias ct) . sortOn ldn
  where
    muted Contact {chatSettings, localDisplayName = ldn}
      | enableNtfs chatSettings = ""
      | otherwise = " (muted, you can " <> highlight ("/unmute @" <> ldn) <> ")"
    alias Contact {profile = LocalProfile {localAlias}}
      | localAlias == "" = ""
      | otherwise = " (alias: " <> plain localAlias <> ")"

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

autoAcceptStatus_ :: Maybe AutoAccept -> [StyledString]
autoAcceptStatus_ = \case
  Just AutoAccept {acceptIncognito, autoReply} ->
    ("auto_accept on" <> if acceptIncognito then ", incognito" else "") :
    maybe [] ((["auto reply:"] <>) . ttyMsgContent) autoReply
  _ -> ["auto_accept off"]

groupLink_ :: StyledString -> GroupInfo -> ConnReqContact -> [StyledString]
groupLink_ intro g cReq =
  [ intro,
    "",
    (plain . strEncode) cReq,
    "",
    "Anybody can connect to you and join group with: " <> highlight' "/c <group_link_above>",
    "to show it again: " <> highlight ("/show link #" <> groupName' g),
    "to delete it: " <> highlight ("/delete link #" <> groupName' g) <> " (joined members will remain connected to you)"
  ]

viewGroupLinkDeleted :: GroupInfo -> [StyledString]
viewGroupLinkDeleted g =
  [ "Group link is deleted - joined members will remain connected.",
    "To create a new group link use " <> highlight ("/create link #" <> groupName' g)
  ]

viewSentInvitation :: Maybe Profile -> Bool -> [StyledString]
viewSentInvitation incognitoProfile testView =
  case incognitoProfile of
    Just profile ->
      if testView
        then incognitoProfile' profile : message
        else message
      where
        message = ["connection request sent incognito!"]
    Nothing -> ["connection request sent!"]

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

viewUserJoinedGroup :: GroupInfo -> [StyledString]
viewUserJoinedGroup g@GroupInfo {membership = membership@GroupMember {memberProfile}} =
  if memberIncognito membership
    then [ttyGroup' g <> ": you joined the group incognito as " <> incognitoProfile' (fromLocalProfile memberProfile)]
    else [ttyGroup' g <> ": you joined the group"]

viewJoinedGroupMember :: GroupInfo -> GroupMember -> [StyledString]
viewJoinedGroupMember g m =
  [ttyGroup' g <> ": " <> ttyMember m <> " joined the group "]

viewReceivedGroupInvitation :: GroupInfo -> Contact -> GroupMemberRole -> [StyledString]
viewReceivedGroupInvitation g@GroupInfo {membership = membership@GroupMember {memberProfile}} c role =
  ttyFullGroup g <> ": " <> ttyContact' c <> " invites you to join the group as " <> plain (strEncode role) :
  if memberIncognito membership
    then ["use " <> highlight ("/j " <> groupName' g) <> " to join incognito as " <> incognitoProfile' (fromLocalProfile memberProfile)]
    else ["use " <> highlight ("/j " <> groupName' g) <> " to accept"]

groupPreserved :: GroupInfo -> [StyledString]
groupPreserved g = ["use " <> highlight ("/d #" <> groupName' g) <> " to delete the group"]

connectedMember :: GroupMember -> StyledString
connectedMember m = case memberCategory m of
  GCPreMember -> "member " <> ttyFullMember m
  GCPostMember -> "new member " <> ttyMember m -- without fullName as as it was shown in joinedGroupMemberConnecting
  _ -> "member " <> ttyMember m -- these case is not used

viewMemberRoleChanged :: GroupInfo -> GroupMember -> GroupMember -> GroupMemberRole -> GroupMemberRole -> [StyledString]
viewMemberRoleChanged g@GroupInfo {membership} by m r r'
  | r == r' = [ttyGroup' g <> ": member role did not change"]
  | groupMemberId' membership == memId = view "your role"
  | groupMemberId' by == memId = view "the role"
  | otherwise = view $ "the role of " <> ttyMember m
  where
    memId = groupMemberId' m
    view s = [ttyGroup' g <> ": " <> ttyMember by <> " changed " <> s <> " from " <> showRole r <> " to " <> showRole r']

viewMemberRoleUserChanged :: GroupInfo -> GroupMember -> GroupMemberRole -> GroupMemberRole -> [StyledString]
viewMemberRoleUserChanged g@GroupInfo {membership} m r r'
  | r == r' = [ttyGroup' g <> ": member role did not change"]
  | groupMemberId' membership == groupMemberId' m = view "your role"
  | otherwise = view $ "the role of " <> ttyMember m
  where
    view s = [ttyGroup' g <> ": you changed " <> s <> " from " <> showRole r <> " to " <> showRole r']

showRole :: GroupMemberRole -> StyledString
showRole = plain . strEncode

viewGroupMembers :: Group -> [StyledString]
viewGroupMembers (Group GroupInfo {membership} members) = map groupMember . filter (not . removedOrLeft) $ membership : members
  where
    removedOrLeft m = let s = memberStatus m in s == GSMemRemoved || s == GSMemLeft
    groupMember m = incognito m <> ttyFullMember m <> ": " <> role m <> ", " <> category m <> status m
    incognito m = if memberIncognito m then incognitoPrefix else ""
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

viewContactConnected :: Contact -> Maybe Profile -> Bool -> [StyledString]
viewContactConnected ct@Contact {localDisplayName} userIncognitoProfile testView =
  case userIncognitoProfile of
    Just profile ->
      if testView
        then incognitoProfile' profile : message
        else message
      where
        message =
          [ ttyFullContact ct <> ": contact is connected, your incognito profile for this contact is " <> incognitoProfile' profile,
            "use " <> highlight ("/info " <> localDisplayName) <> " to print out this incognito profile again"
          ]
    Nothing ->
      [ttyFullContact ct <> ": contact is connected"]

viewGroupsList :: [GroupInfo] -> [StyledString]
viewGroupsList [] = ["you have no groups!", "to create: " <> highlight' "/g <name>"]
viewGroupsList gs = map groupSS $ sortOn ldn_ gs
  where
    ldn_ = T.toLower . (localDisplayName :: GroupInfo -> GroupName)
    groupSS g@GroupInfo {localDisplayName = ldn, groupProfile = GroupProfile {fullName}, membership, chatSettings} =
      case memberStatus membership of
        GSMemInvited -> groupInvitation' g
        s -> incognito <> ttyGroup ldn <> optFullName ldn fullName <> viewMemberStatus s
      where
        incognito = if memberIncognito membership then incognitoPrefix else ""
        viewMemberStatus = \case
          GSMemRemoved -> delete "you are removed"
          GSMemLeft -> delete "you left"
          GSMemGroupDeleted -> delete "group deleted"
          _
            | enableNtfs chatSettings -> ""
            | otherwise -> " (muted, you can " <> highlight ("/unmute #" <> ldn) <> ")"
        delete reason = " (" <> reason <> ", delete local copy: " <> highlight ("/d #" <> ldn) <> ")"

groupInvitation' :: GroupInfo -> StyledString
groupInvitation' GroupInfo {localDisplayName = ldn, groupProfile = GroupProfile {fullName}, membership = membership@GroupMember {memberProfile}} =
  highlight ("#" <> ldn)
    <> optFullName ldn fullName
    <> " - you are invited ("
    <> highlight ("/j " <> ldn)
    <> joinText
    <> highlight ("/d #" <> ldn)
    <> " to delete invitation)"
  where
    joinText =
      if memberIncognito membership
        then " to join as " <> incognitoProfile' (fromLocalProfile memberProfile) <> ", "
        else " to join, "

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

viewChatItemTTL :: Maybe Int64 -> [StyledString]
viewChatItemTTL = \case
  Nothing -> ["old messages are not being deleted"]
  Just ttl
    | ttl == 86400 -> deletedAfter "one day"
    | ttl == 7 * 86400 -> deletedAfter "one week"
    | ttl == 30 * 86400 -> deletedAfter "one month"
    | otherwise -> deletedAfter $ sShow ttl <> " second(s)"
  where
    deletedAfter ttlStr = ["old messages are set to be deleted after: " <> ttlStr]

viewNetworkConfig :: NetworkConfig -> [StyledString]
viewNetworkConfig NetworkConfig {socksProxy, tcpTimeout} =
  [ plain $ maybe "direct network connection" (("using SOCKS5 proxy " <>) . show) socksProxy,
    "TCP timeout: " <> sShow tcpTimeout,
    "use `/network socks=<on/off/[ipv4]:port>[ timeout=<seconds>]` to change settings"
  ]

viewContactInfo :: Contact -> ConnectionStats -> Maybe Profile -> [StyledString]
viewContactInfo Contact {contactId, profile = LocalProfile {localAlias}} stats incognitoProfile =
  ["contact ID: " <> sShow contactId] <> viewConnectionStats stats
    <> maybe
      ["you've shared main profile with this contact"]
      (\p -> ["you've shared incognito profile with this contact: " <> incognitoProfile' p])
      incognitoProfile
    <> if localAlias /= "" then ["alias: " <> plain localAlias] else ["alias not set"]

viewGroupMemberInfo :: GroupInfo -> GroupMember -> Maybe ConnectionStats -> [StyledString]
viewGroupMemberInfo GroupInfo {groupId} GroupMember {groupMemberId, memberProfile = LocalProfile {localAlias}} stats =
  [ "group ID: " <> sShow groupId,
    "member ID: " <> sShow groupMemberId
  ]
    <> maybe ["member not connected"] viewConnectionStats stats
    <> if localAlias /= "" then ["alias: " <> plain localAlias] else ["no alias for contact"]

viewConnectionStats :: ConnectionStats -> [StyledString]
viewConnectionStats ConnectionStats {rcvServers, sndServers} =
  ["receiving messages via: " <> viewServerHosts rcvServers | not $ null rcvServers]
    <> ["sending messages via: " <> viewServerHosts sndServers | not $ null sndServers]

viewServers :: [SMPServer] -> StyledString
viewServers = plain . intercalate ", " . map (B.unpack . strEncode)

viewServerHosts :: [SMPServer] -> StyledString
viewServerHosts = plain . intercalate ", " . map showSMPServer

viewContactSwitch :: Contact -> SwitchProgress -> [StyledString]
viewContactSwitch _ (SwitchProgress _ SPConfirmed _) = []
viewContactSwitch ct (SwitchProgress qd phase _) = case qd of
  QDRcv -> [ttyContact' ct <> ": you " <> viewSwitchPhase phase]
  QDSnd -> [ttyContact' ct <> " " <> viewSwitchPhase phase <> " for you"]

viewGroupMemberSwitch :: GroupInfo -> GroupMember -> SwitchProgress -> [StyledString]
viewGroupMemberSwitch _ _ (SwitchProgress _ SPConfirmed _) = []
viewGroupMemberSwitch g m (SwitchProgress qd phase _) = case qd of
  QDRcv -> [ttyGroup' g <> ": you " <> viewSwitchPhase phase <> " for " <> ttyMember m]
  QDSnd -> [ttyGroup' g <> ": " <> ttyMember m <> " " <> viewSwitchPhase phase <> " for you"]

viewSwitchPhase :: SwitchPhase -> StyledString
viewSwitchPhase SPCompleted = "changed address"
viewSwitchPhase phase = plain (strEncode phase) <> " changing address"

viewUserProfileUpdated :: Profile -> Profile -> [StyledString]
viewUserProfileUpdated Profile {displayName = n, fullName, image, preferences} Profile {displayName = n', fullName = fullName', image = image', preferences = prefs'} =
  profileUpdated <> viewPrefsUpdated preferences prefs'
  where
    profileUpdated
      | n == n' && fullName == fullName' && image == image' = []
      | n == n' && fullName == fullName' = [if isNothing image' then "profile image removed" else "profile image updated"]
      | n == n' = ["user full name " <> (if T.null fullName' || fullName' == n' then "removed" else "changed to " <> plain fullName') <> notified]
      | otherwise = ["user profile is changed to " <> ttyFullName n' fullName' <> notified]
    notified = " (your contacts are notified)"

viewUserContactPrefsUpdated :: User -> Contact -> Contact -> ContactUserPreferences -> [StyledString]
viewUserContactPrefsUpdated user ct ct' cups
  | null prefs = ["your preferences for " <> ttyContact' ct' <> " did not change"]
  | otherwise = ("you updated preferences for " <> ttyContact' ct' <> ":") : prefs
  where
    prefs = viewContactPreferences user ct ct' cups

viewContactPrefsUpdated :: User -> Contact -> Contact -> ContactUserPreferences -> [StyledString]
viewContactPrefsUpdated user ct ct' cups
  | null prefs = []
  | otherwise = (ttyContact' ct' <> " updated preferences for you:") : prefs
  where
    prefs = viewContactPreferences user ct ct' cups

viewContactPreferences :: User -> Contact -> Contact -> ContactUserPreferences -> [StyledString]
viewContactPreferences user ct ct' cups =
  mapMaybe (viewContactPref (mergeUserChatPrefs user ct) (mergeUserChatPrefs user ct') (preferences' ct) cups) allChatFeatures

viewContactPref :: FullPreferences -> FullPreferences -> Maybe Preferences -> ContactUserPreferences -> ChatFeature -> Maybe StyledString
viewContactPref userPrefs userPrefs' ctPrefs cups pt
  | userPref == userPref' && ctPref == contactPreference = Nothing
  | otherwise = Just $ plain (chatPrefName pt) <> ": " <> viewPrefEnabled enabled <> " (you allow: " <> viewCountactUserPref userPreference <> ", contact allows: " <> viewPreference contactPreference <> ")"
  where
    userPref = getPreference pt userPrefs
    userPref' = getPreference pt userPrefs'
    ctPref = getPreference pt ctPrefs
    ContactUserPreference {enabled, userPreference, contactPreference} = getContactUserPrefefence pt cups

viewPrefsUpdated :: Maybe Preferences -> Maybe Preferences -> [StyledString]
viewPrefsUpdated ps ps'
  | null prefs = []
  | otherwise = "updated preferences:" : prefs
  where
    prefs = mapMaybe viewPref allChatFeatures
    viewPref pt
      | pref ps == pref ps' = Nothing
      | otherwise = Just $ plain (chatPrefName pt) <> " allowed: " <> viewPreference (pref ps')
      where
        pref pss = getPreference pt $ mergePreferences pss Nothing

viewPreference :: Preference -> StyledString
viewPreference = \case
  Preference {allow} -> case allow of
    FAAlways -> "always"
    FAYes -> "yes"
    FANo -> "no"

viewCountactUserPref :: ContactUserPref -> StyledString
viewCountactUserPref = \case
  CUPUser p -> "default (" <> viewPreference p <> ")"
  CUPContact p -> viewPreference p

viewPrefEnabled :: PrefEnabled -> StyledString
viewPrefEnabled = \case
  PrefEnabled True True -> "enabled"
  PrefEnabled False False -> "off"
  PrefEnabled {forUser = True, forContact = False} -> "enabled for you"
  PrefEnabled {forUser = False, forContact = True} -> "enabled for contact"

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

viewContactAliasUpdated :: Contact -> [StyledString]
viewContactAliasUpdated Contact {localDisplayName = n, profile = LocalProfile {localAlias}}
  | localAlias == "" = ["contact " <> ttyContact n <> " alias removed"]
  | otherwise = ["contact " <> ttyContact n <> " alias updated: " <> plain localAlias]

viewConnectionAliasUpdated :: PendingContactConnection -> [StyledString]
viewConnectionAliasUpdated PendingContactConnection {pccConnId, localAlias}
  | localAlias == "" = ["connection " <> sShow pccConnId <> " alias removed"]
  | otherwise = ["connection " <> sShow pccConnId <> " alias updated: " <> plain localAlias]

viewContactUpdated :: Contact -> Contact -> [StyledString]
viewContactUpdated
  Contact {localDisplayName = n, profile = LocalProfile {fullName}}
  Contact {localDisplayName = n', profile = LocalProfile {fullName = fullName'}}
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
viewSentFileInvitation to CIFile {fileId, filePath, fileStatus} = case filePath of
  Just fPath -> sentWithTime_ $ ttySentFile fPath
  _ -> const []
  where
    ttySentFile fPath = ["/f " <> to <> ttyFilePath fPath] <> cancelSending
    cancelSending = case fileStatus of
      CIFSSndTransfer -> []
      _ -> ["use " <> highlight ("/fc " <> show fileId) <> " to cancel sending"]

sentWithTime_ :: [StyledString] -> CIMeta d -> [StyledString]
sentWithTime_ styledMsg CIMeta {localItemTs} =
  prependFirst (ttyMsgTime localItemTs <> " ") styledMsg

ttyMsgTime :: ZonedTime -> StyledString
ttyMsgTime = styleTime . formatTime defaultTimeLocale "%H:%M"

ttyMsgContent :: MsgContent -> [StyledString]
ttyMsgContent = msgPlain . msgContentText

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
receivedFileInvitation_ CIFile {fileId, fileName, fileSize, fileStatus} =
  ["sends file " <> ttyFilePath fileName <> " (" <> humanReadableSize fileSize <> " / " <> sShow fileSize <> " bytes)"]
    <> case fileStatus of
      CIFSRcvAccepted -> []
      _ -> ["use " <> highlight ("/fr " <> show fileId <> " [<dir>/ | <path>]") <> " to receive it"]

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
  ["sending " <> fileTransferStr fileId fileName <> ": no file transfers"]
    <> ["file transfer cancelled" | cancelled]
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
    CEChatStoreChanged -> ["error: chat store changed, please restart chat"]
    CEInvalidConnReq -> viewInvalidConnReq
    CEInvalidChatMessage e -> ["chat message error: " <> sShow e]
    CEContactNotReady c -> [ttyContact' c <> ": not ready"]
    CEGroupDuplicateMember c -> ["contact " <> ttyContact c <> " is already in the group"]
    CEGroupDuplicateMemberId -> ["cannot add member - duplicate member ID"]
    CEGroupUserRole -> ["you have insufficient permissions for this group command"]
    CEContactIncognitoCantInvite -> ["you're using your main profile for this group - prohibited to invite contacts to whom you are connected incognito"]
    CEGroupIncognitoCantInvite -> ["you've connected to this group using an incognito profile - prohibited to invite contacts"]
    CEGroupContactRole c -> ["contact " <> ttyContact c <> " has insufficient permissions for this group action"]
    CEGroupNotJoined g -> ["you did not join this group, use " <> highlight ("/join #" <> groupName' g)]
    CEGroupMemberNotActive -> ["you cannot invite other members yet, try later"]
    CEGroupMemberUserRemoved -> ["you are no longer a member of the group"]
    CEGroupMemberNotFound -> ["group doesn't have this member"]
    CEGroupMemberIntroNotFound c -> ["group member intro not found for " <> ttyContact c]
    CEGroupCantResendInvitation g c -> viewCannotResendInvitation g c
    CEGroupInternal s -> ["chat group bug: " <> plain s]
    CEFileNotFound f -> ["file not found: " <> plain f]
    CEFileAlreadyReceiving f -> ["file is already being received: " <> plain f]
    CEFileCancelled f -> ["file cancelled: " <> plain f]
    CEFileAlreadyExists f -> ["file already exists: " <> plain f]
    CEFileRead f e -> ["cannot read file " <> plain f, sShow e]
    CEFileWrite f e -> ["cannot write file " <> plain f, sShow e]
    CEFileSend fileId e -> ["error sending file " <> sShow fileId <> ": " <> sShow e]
    CEFileRcvChunk e -> ["error receiving file: " <> plain e]
    CEFileInternal e -> ["file error: " <> plain e]
    CEFileImageType _ -> ["image type must be jpg, send as a file using " <> highlight' "/f"]
    CEFileImageSize _ -> ["max image size: " <> sShow maxImageSize <> " bytes, resize it or send as a file using " <> highlight' "/f"]
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
    CEAgentCommandError e -> ["agent command error: " <> plain e]
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
    SEDuplicateGroupLink g -> ["you already have link for this group, to show: " <> highlight ("/show link #" <> groupName' g)]
    SEGroupLinkNotFound g -> ["no group link, to create: " <> highlight ("/create link #" <> groupName' g)]
    e -> ["chat db error: " <> sShow e]
  ChatErrorDatabase err -> case err of
    DBErrorEncrypted -> ["error: chat database is already encrypted"]
    DBErrorPlaintext -> ["error: chat database is not encrypted"]
    DBErrorExport e -> ["error encrypting database: " <> sqliteError' e]
    DBErrorOpen e -> ["error opening database after encryption: " <> sqliteError' e]
    e -> ["chat database error: " <> sShow e]
  ChatErrorAgent err -> case err of
    SMP SMP.AUTH ->
      [ "error: connection authorization failed - this could happen if connection was deleted,\
        \ secured with different credentials, or due to a bug - please re-create the connection"
      ]
    AGENT A_DUPLICATE -> []
    AGENT A_PROHIBITED -> []
    CONN NOT_FOUND -> []
    e -> ["smp agent error: " <> sShow e]
  where
    fileNotFound fileId = ["file " <> sShow fileId <> " not found"]
    sqliteError' = \case
      SQLiteErrorNotADatabase -> "wrong passphrase or invalid database file"
      SQLiteError e -> sShow e

ttyContact :: ContactName -> StyledString
ttyContact = styled $ colored Green

ttyContact' :: Contact -> StyledString
ttyContact' Contact {localDisplayName = c} = ttyContact c

ttyFullContact :: Contact -> StyledString
ttyFullContact Contact {localDisplayName, profile = LocalProfile {fullName}} =
  ttyFullName localDisplayName fullName

ttyMember :: GroupMember -> StyledString
ttyMember GroupMember {localDisplayName} = ttyContact localDisplayName

ttyFullMember :: GroupMember -> StyledString
ttyFullMember GroupMember {localDisplayName, memberProfile = LocalProfile {fullName}} =
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
ttyToContact' Contact {localDisplayName = c, activeConn = Connection {customUserProfileId}} =
  maybe "" (const incognitoPrefix) customUserProfileId <> ttyToContact c

ttyQuotedContact :: Contact -> StyledString
ttyQuotedContact Contact {localDisplayName = c} = ttyFrom $ c <> ">"

ttyQuotedMember :: Maybe GroupMember -> StyledString
ttyQuotedMember (Just GroupMember {localDisplayName = c}) = "> " <> ttyFrom c
ttyQuotedMember _ = "> " <> ttyFrom "?"

ttyFromContact' :: Contact -> StyledString
ttyFromContact' Contact {localDisplayName = c, activeConn = Connection {customUserProfileId}} =
  maybe "" (const incognitoPrefix) customUserProfileId <> ttyFromContact c

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
ttyFromGroup' g@GroupInfo {membership} GroupMember {localDisplayName = m} =
  (if memberIncognito membership then incognitoPrefix else "") <> ttyFromGroup g m

ttyToGroup :: GroupInfo -> StyledString
ttyToGroup GroupInfo {localDisplayName = g, membership} =
  (if memberIncognito membership then incognitoPrefix else "") <> styled (colored Cyan) ("#" <> g <> " ")

ttyFilePath :: FilePath -> StyledString
ttyFilePath = plain

optFullName :: ContactName -> Text -> StyledString
optFullName localDisplayName fullName = plain $ optionalFullName localDisplayName fullName

incognitoPrefix :: StyledString
incognitoPrefix = styleIncognito' "i "

incognitoProfile' :: Profile -> StyledString
incognitoProfile' Profile {displayName} = styleIncognito displayName

highlight :: StyledFormat a => a -> StyledString
highlight = styled $ colored Cyan

highlight' :: String -> StyledString
highlight' = highlight

styleIncognito :: StyledFormat a => a -> StyledString
styleIncognito = styled $ colored Magenta

styleIncognito' :: String -> StyledString
styleIncognito' = styleIncognito

styleTime :: String -> StyledString
styleTime = Styled [SetColor Foreground Vivid Black]

ttyError :: StyledFormat a => a -> StyledString
ttyError = styled $ colored Red

ttyError' :: String -> StyledString
ttyError' = ttyError
