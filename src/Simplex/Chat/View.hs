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
import qualified Data.List.NonEmpty as L
import Data.Maybe (isJust, isNothing, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (DiffTime, UTCTime)
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
import Simplex.Messaging.Agent.Client (SMPTestFailure (..), SMPTestStep (..))
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

type CurrentTime = UTCTime

serializeChatResponse :: Maybe User -> CurrentTime -> ChatResponse -> String
serializeChatResponse user_ ts = unlines . map unStyle . responseToView user_ False False ts

responseToView :: Maybe User -> Bool -> Bool -> CurrentTime -> ChatResponse -> [StyledString]
responseToView user_ testView liveItems ts = \case
  CRActiveUser User {profile} -> viewUserProfile $ fromLocalProfile profile
  CRChatStarted -> ["chat started"]
  CRChatRunning -> ["chat is running"]
  CRChatStopped -> ["chat stopped"]
  CRChatSuspended -> ["chat suspended"]
  CRApiChats chats -> if testView then testViewChats chats else [plain . bshow $ J.encode chats]
  CRApiChat chat -> if testView then testViewChat chat else [plain . bshow $ J.encode chat]
  CRApiParsedMarkdown ft -> [plain . bshow $ J.encode ft]
  CRUserSMPServers smpServers _ -> viewSMPServers (L.toList smpServers) testView
  CRSmpTestResult testFailure -> viewSMPTestResult testFailure
  CRChatItemTTL ttl -> viewChatItemTTL ttl
  CRNetworkConfig cfg -> viewNetworkConfig cfg
  CRContactInfo ct cStats customUserProfile -> viewContactInfo ct cStats customUserProfile
  CRGroupMemberInfo g m cStats -> viewGroupMemberInfo g m cStats
  CRContactSwitch ct progress -> viewContactSwitch ct progress
  CRGroupMemberSwitch g m progress -> viewGroupMemberSwitch g m progress
  CRConnectionVerified verified code -> [plain $ if verified then "connection verified" else "connection not verified, current code is " <> code]
  CRContactCode ct code -> viewContactCode ct code testView
  CRGroupMemberCode g m code -> viewGroupMemberCode g m code testView
  CRNewChatItem (AChatItem _ _ chat item) -> unmuted chat item $ viewChatItem chat item False ts
  CRChatItems chatItems -> concatMap (\(AChatItem _ _ chat item) -> viewChatItem chat item True ts) chatItems
  CRChatItemId itemId -> [plain $ maybe "no item" show itemId]
  CRChatItemStatusUpdated _ -> []
  CRChatItemUpdated (AChatItem _ _ chat item) -> unmuted chat item $ viewItemUpdate chat item liveItems ts
  CRChatItemDeleted (AChatItem _ _ chat deletedItem) toItem byUser timed -> unmuted chat deletedItem $ viewItemDelete chat deletedItem (isJust toItem) byUser timed ts
  CRChatItemDeletedNotFound Contact {localDisplayName = c} _ -> [ttyFrom $ c <> "> [deleted - original message not found]"]
  CRBroadcastSent mc n t -> viewSentBroadcast mc n ts t
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
  CRContactPrefsUpdated {fromContact, toContact} -> case user_ of
    Just user -> viewUserContactPrefsUpdated user fromContact toContact
    _ -> ["unexpected chat event CRContactPrefsUpdated without current user"]
  CRContactAliasUpdated c -> viewContactAliasUpdated c
  CRConnectionAliasUpdated c -> viewConnectionAliasUpdated c
  CRContactUpdated {fromContact = c, toContact = c'} -> case user_ of
    Just user -> viewContactUpdated c c' <> viewContactPrefsUpdated user c c'
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
  CRGroupProfile g -> viewGroupProfile g
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
        toCIPreview (ci : _) = testViewItem ci
        toCIPreview _ = ""
    testViewChat :: AChat -> [StyledString]
    testViewChat (AChat _ Chat {chatItems}) = [sShow $ map toChatView chatItems]
      where
        toChatView :: CChatItem c -> ((Int, Text), Maybe (Int, Text), Maybe String)
        toChatView ci@(CChatItem dir ChatItem {quotedItem, file}) =
          ((msgDirectionInt $ toMsgDirection dir, testViewItem ci), qItem, fPath)
          where
            qItem = case quotedItem of
              Nothing -> Nothing
              Just CIQuote {chatDir = quoteDir, content} ->
                Just (msgDirectionInt $ quoteMsgDirection quoteDir, msgContentText content)
            fPath = case file of
              Just CIFile {filePath = Just fp} -> Just fp
              _ -> Nothing
    testViewItem :: CChatItem c -> Text
    testViewItem (CChatItem _ ChatItem {meta = CIMeta {itemText, itemDeleted}}) = itemText <> if itemDeleted then " [marked deleted]" else ""
    viewErrorsSummary :: [a] -> StyledString -> [StyledString]
    viewErrorsSummary summary s = [ttyError (T.pack . show $ length summary) <> s <> " (run with -c option to show each error)" | not (null summary)]
    contactList :: [ContactRef] -> String
    contactList cs = T.unpack . T.intercalate ", " $ map (\ContactRef {localDisplayName = n} -> "@" <> n) cs
    unmuted :: ChatInfo c -> ChatItem c d -> [StyledString] -> [StyledString]
    unmuted chat chatItem s
      | muted chat chatItem = []
      | otherwise = s

muted :: ChatInfo c -> ChatItem c d -> Bool
muted chat ChatItem {chatDir} = case (chat, chatDir) of
  (DirectChat Contact {chatSettings = DisableNtfs}, CIDirectRcv) -> True
  (GroupChat GroupInfo {chatSettings = DisableNtfs}, CIGroupRcv _) -> True
  _ -> False

viewGroupSubscribed :: GroupInfo -> [StyledString]
viewGroupSubscribed g = [membershipIncognito g <> ttyFullGroup g <> ": connected to server(s)"]

showSMPServer :: SMPServer -> String
showSMPServer = B.unpack . strEncode . host

viewHostEvent :: AProtocolType -> TransportHost -> String
viewHostEvent p h = map toUpper (B.unpack $ strEncode p) <> " host " <> B.unpack (strEncode h)

viewChatItem :: forall c d. MsgDirectionI d => ChatInfo c -> ChatItem c d -> Bool -> CurrentTime -> [StyledString]
viewChatItem chat ChatItem {chatDir, meta = meta@CIMeta {itemDeleted}, content, quotedItem, file} doShow ts =
  withItemDeleted <$> case chat of
    DirectChat c -> case chatDir of
      CIDirectSnd -> case content of
        CISndMsgContent mc -> hideLive meta $ withSndFile to $ sndMsg to quote mc
        CISndGroupEvent {} -> showSndItemProhibited to
        _ -> showSndItem to
        where
          to = ttyToContact' c
      CIDirectRcv -> case content of
        CIRcvMsgContent mc -> withRcvFile from $ rcvMsg from quote mc
        CIRcvIntegrityError err -> viewRcvIntegrityError from err ts meta
        CIRcvGroupEvent {} -> showRcvItemProhibited from
        _ -> showRcvItem from
        where
          from = ttyFromContact c
      where
        quote = maybe [] (directQuote chatDir) quotedItem
    GroupChat g -> case chatDir of
      CIGroupSnd -> case content of
        CISndMsgContent mc -> hideLive meta $ withSndFile to $ sndMsg to quote mc
        CISndGroupInvitation {} -> showSndItemProhibited to
        _ -> showSndItem to
        where
          to = ttyToGroup g
      CIGroupRcv m -> case content of
        CIRcvMsgContent mc -> withRcvFile from $ rcvMsg from quote mc
        CIRcvIntegrityError err -> viewRcvIntegrityError from err ts meta
        CIRcvGroupInvitation {} -> showRcvItemProhibited from
        _ -> showRcvItem from
        where
          from = ttyFromGroup g m
      where
        quote = maybe [] (groupQuote g) quotedItem
    _ -> []
  where
    withItemDeleted item = if itemDeleted then item <> styled (colored Red) (" [marked deleted]" :: String) else item
    withSndFile = withFile viewSentFileInvitation
    withRcvFile = withFile viewReceivedFileInvitation
    withFile view dir l = maybe l (\f -> l <> view dir f ts meta) file
    sndMsg = msg viewSentMessage
    rcvMsg = msg viewReceivedMessage
    msg view dir quote mc = case (msgContentText mc, file, quote) of
      ("", Just _, []) -> []
      ("", Just CIFile {fileName}, _) -> view dir quote (MCText $ T.pack fileName) ts meta
      _ -> view dir quote mc ts meta
    showSndItem to = showItem $ sentWithTime_ ts [to <> plainContent content] meta
    showRcvItem from = showItem $ receivedWithTime_ ts from [] meta [plainContent content] False
    showSndItemProhibited to = showItem $ sentWithTime_ ts [to <> plainContent content <> " " <> prohibited] meta
    showRcvItemProhibited from = showItem $ receivedWithTime_ ts from [] meta [plainContent content <> " " <> prohibited] False
    showItem ss = if doShow then ss else []
    plainContent = plain . ciContentToText
    prohibited = styled (colored Red) ("[unexpected chat item created, please report to developers]" :: String)

viewItemUpdate :: MsgDirectionI d => ChatInfo c -> ChatItem c d -> Bool -> CurrentTime -> [StyledString]
viewItemUpdate chat ChatItem {chatDir, meta = meta@CIMeta {itemEdited, itemLive}, content, quotedItem} liveItems ts = case chat of
  DirectChat c -> case chatDir of
    CIDirectRcv -> case content of
      CIRcvMsgContent mc
        | itemLive == Just True && not liveItems -> []
        | otherwise -> viewReceivedUpdatedMessage from quote mc ts meta
      _ -> []
      where
        from = if itemEdited then ttyFromContactEdited c else ttyFromContact c
    CIDirectSnd -> case content of
      CISndMsgContent mc -> hideLive meta $ viewSentMessage to quote mc ts meta
      _ -> []
      where
        to = if itemEdited then ttyToContactEdited' c else ttyToContact' c
    where
      quote = maybe [] (directQuote chatDir) quotedItem
  GroupChat g -> case chatDir of
    CIGroupRcv m -> case content of
      CIRcvMsgContent mc
        | itemLive == Just True && not liveItems -> []
        | otherwise -> viewReceivedUpdatedMessage from quote mc ts meta
      _ -> []
      where
        from = if itemEdited then ttyFromGroupEdited g m else ttyFromGroup g m
    CIGroupSnd -> case content of
      CISndMsgContent mc -> hideLive meta $ viewSentMessage to quote mc ts meta
      _ -> []
      where
        to = if itemEdited then ttyToGroupEdited g else ttyToGroup g
    where
      quote = maybe [] (groupQuote g) quotedItem
  _ -> []

hideLive :: CIMeta d -> [StyledString] -> [StyledString]
hideLive CIMeta {itemLive = Just True} _ = []
hideLive _ s = s

viewItemDelete :: ChatInfo c -> ChatItem c d -> Bool -> Bool -> Bool -> CurrentTime -> [StyledString]
viewItemDelete chat ChatItem {chatDir, meta, content = deletedContent} markedDeleted byUser timed ts
  | timed = []
  | byUser = if markedDeleted then ["message marked deleted"] else ["message deleted"]
  | otherwise = case chat of
    DirectChat c -> case (chatDir, deletedContent) of
      (CIDirectRcv, CIRcvMsgContent mc) -> viewReceivedMessage (ttyFromContactDeleted c markedDeleted) [] mc ts meta
      _ -> prohibited
    GroupChat g -> case (chatDir, deletedContent) of
      (CIGroupRcv m, CIRcvMsgContent mc) -> viewReceivedMessage (ttyFromGroupDeleted g m markedDeleted) [] mc ts meta
      _ -> prohibited
    _ -> prohibited
  where
    prohibited = [styled (colored Red) ("[unexpected message deletion, please report to developers]" :: String)]

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

viewRcvIntegrityError :: StyledString -> MsgErrorType -> CurrentTime -> CIMeta 'MDRcv -> [StyledString]
viewRcvIntegrityError from msgErr ts meta = receivedWithTime_ ts from [] meta (viewMsgIntegrityError msgErr) False

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
   in map (\ct -> ctIncognito ct <> ttyFullContact ct <> muted' ct <> alias ct) . sortOn ldn
  where
    muted' Contact {chatSettings, localDisplayName = ldn}
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
viewGroupCreated g@GroupInfo {localDisplayName = n} =
  [ "group " <> ttyFullGroup g <> " is created",
    "to add members use " <> highlight ("/a " <> n <> " <name>") <> " or " <> highlight ("/create link #" <> n)
  ]

viewCannotResendInvitation :: GroupInfo -> ContactName -> [StyledString]
viewCannotResendInvitation GroupInfo {localDisplayName = gn} c =
  [ ttyContact c <> " is already invited to group " <> ttyGroup gn,
    "to re-send invitation: " <> highlight ("/rm " <> gn <> " " <> c) <> ", " <> highlight ("/a " <> gn <> " " <> c)
  ]

viewDirectMessagesProhibited :: MsgDirection -> Contact -> [StyledString]
viewDirectMessagesProhibited MDSnd c = ["direct messages to indirect contact " <> ttyContact' c <> " are prohibited"]
viewDirectMessagesProhibited MDRcv c = ["received prohibited direct message from indirect contact " <> ttyContact' c <> " (discarded)"]

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
    groupMember m = memIncognito m <> ttyFullMember m <> ": " <> role m <> ", " <> category m <> status m
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
            "use " <> highlight ("/i " <> localDisplayName) <> " to print out this incognito profile again"
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
        s -> membershipIncognito g <> ttyGroup ldn <> optFullName ldn fullName <> viewMemberStatus s
      where
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

viewSMPServers :: [ServerCfg] -> Bool -> [StyledString]
viewSMPServers smpServers testView =
  if testView
    then [customSMPServers]
    else
      [ customSMPServers,
        "",
        "use " <> highlight' "/smp test <srv>" <> " to test SMP server connection",
        "use " <> highlight' "/smp set <srv1[,srv2,...]>" <> " to switch to custom SMP servers",
        "use " <> highlight' "/smp default" <> " to remove custom SMP servers and use default",
        "(chat option " <> highlight' "-s" <> " (" <> highlight' "--server" <> ") has precedence over saved SMP servers for chat session)"
      ]
  where
    customSMPServers =
      if null smpServers
        then "no custom SMP servers saved"
        else viewServers smpServers

viewSMPTestResult :: Maybe SMPTestFailure -> [StyledString]
viewSMPTestResult = \case
  Just SMPTestFailure {testStep, testError} ->
    result
      <> ["Server requires authorization to create queues, check password" | testStep == TSCreateQueue && testError == SMP SMP.AUTH]
      <> ["Possibly, certificate fingerprint in server address is incorrect" | testStep == TSConnect && brokerErr]
    where
      result = ["SMP server test failed at " <> plain (drop 2 $ show testStep) <> ", error: " <> plain (strEncode testError)]
      brokerErr = case testError of
        BROKER _ NETWORK -> True
        _ -> False
  _ -> ["SMP server test passed"]

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
    "use " <> highlight' "/network socks=<on/off/[ipv4]:port>[ timeout=<seconds>]" <> " to change settings"
  ]

viewContactInfo :: Contact -> ConnectionStats -> Maybe Profile -> [StyledString]
viewContactInfo ct@Contact {contactId, profile = LocalProfile {localAlias}} stats incognitoProfile =
  ["contact ID: " <> sShow contactId] <> viewConnectionStats stats
    <> maybe
      ["you've shared main profile with this contact"]
      (\p -> ["you've shared incognito profile with this contact: " <> incognitoProfile' p])
      incognitoProfile
    <> ["alias: " <> plain localAlias | localAlias /= ""]
    <> [viewConnectionVerified (contactSecurityCode ct)]

viewGroupMemberInfo :: GroupInfo -> GroupMember -> Maybe ConnectionStats -> [StyledString]
viewGroupMemberInfo GroupInfo {groupId} m@GroupMember {groupMemberId, memberProfile = LocalProfile {localAlias}} stats =
  [ "group ID: " <> sShow groupId,
    "member ID: " <> sShow groupMemberId
  ]
    <> maybe ["member not connected"] viewConnectionStats stats
    <> ["alias: " <> plain localAlias | localAlias /= ""]
    <> [viewConnectionVerified (memberSecurityCode m) | isJust stats]

viewConnectionVerified :: Maybe SecurityCode -> StyledString
viewConnectionVerified (Just _) = "connection verified" -- TODO show verification time?
viewConnectionVerified _ = "connection not verified, use " <> highlight' "/code" <> " command to see security code"

viewConnectionStats :: ConnectionStats -> [StyledString]
viewConnectionStats ConnectionStats {rcvServers, sndServers} =
  ["receiving messages via: " <> viewServerHosts rcvServers | not $ null rcvServers]
    <> ["sending messages via: " <> viewServerHosts sndServers | not $ null sndServers]

viewServers :: [ServerCfg] -> StyledString
viewServers = plain . intercalate ", " . map (B.unpack . strEncode . (\ServerCfg {server} -> server))

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

viewContactCode :: Contact -> Text -> Bool -> [StyledString]
viewContactCode ct@Contact {localDisplayName = c} = viewSecurityCode (ttyContact' ct) ("/verify " <> c <> " <code from your contact>")

viewGroupMemberCode :: GroupInfo -> GroupMember -> Text -> Bool -> [StyledString]
viewGroupMemberCode g m@GroupMember {localDisplayName = n} = viewSecurityCode (ttyGroup' g <> " " <> ttyMember m) ("/verify #" <> groupName' g <> " " <> n <> " <code from your contact>")

viewSecurityCode :: StyledString -> Text -> Text -> Bool -> [StyledString]
viewSecurityCode name cmd code testView
  | testView = [plain code]
  | otherwise = [name <> " security code:", plain code, "pass this code to your contact and use " <> highlight cmd <> " to verify"]

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

viewUserContactPrefsUpdated :: User -> Contact -> Contact -> [StyledString]
viewUserContactPrefsUpdated user ct ct'@Contact {mergedPreferences = cups}
  | null prefs = ["your preferences for " <> ttyContact' ct' <> " did not change"]
  | otherwise = ("you updated preferences for " <> ttyContact' ct' <> ":") : prefs
  where
    prefs = viewContactPreferences user ct ct' cups

viewContactPrefsUpdated :: User -> Contact -> Contact -> [StyledString]
viewContactPrefsUpdated user ct ct'@Contact {mergedPreferences = cups}
  | null prefs = []
  | otherwise = (ttyContact' ct' <> " updated preferences for you:") : prefs
  where
    prefs = viewContactPreferences user ct ct' cups

viewContactPreferences :: User -> Contact -> Contact -> ContactUserPreferences -> [StyledString]
viewContactPreferences user ct ct' cups =
  mapMaybe (viewContactPref (mergeUserChatPrefs user ct) (mergeUserChatPrefs user ct') (preferences' ct) cups) allChatFeatures

viewContactPref :: FullPreferences -> FullPreferences -> Maybe Preferences -> ContactUserPreferences -> AChatFeature -> Maybe StyledString
viewContactPref userPrefs userPrefs' ctPrefs cups (ACF f)
  | userPref == userPref' && ctPref == contactPreference = Nothing
  | otherwise = Just $ viewFeatureText f <> ": " <> plain (prefEnabledToText enabled) <> " (you allow: " <> viewCountactUserPref userPreference <> ", contact allows: " <> viewPreference contactPreference <> ")"
  where
    userPref = getPreference f userPrefs
    userPref' = getPreference f userPrefs'
    ctPref = getPreference f ctPrefs
    ContactUserPreference {enabled, userPreference, contactPreference} = getContactUserPreference f cups

viewPrefsUpdated :: Maybe Preferences -> Maybe Preferences -> [StyledString]
viewPrefsUpdated ps ps'
  | null prefs = []
  | otherwise = "updated preferences:" : prefs
  where
    prefs = mapMaybe viewPref allChatFeatures
    viewPref (ACF f)
      | pref ps == pref ps' = Nothing
      | otherwise = Just $ viewFeatureText f <> " allowed: " <> viewPreference (pref ps')
      where
        pref pss = getPreference f $ mergePreferences pss Nothing

viewFeatureText :: SChatFeature f -> StyledString
viewFeatureText f = plain . chatFeatureToText $ chatFeature f

viewPreference :: FeatureI f => FeaturePreference f -> StyledString
viewPreference p = plain $ prefToText' p

viewCountactUserPref :: FeatureI f => ContactUserPref (FeaturePreference f) -> StyledString
viewCountactUserPref = \case
  CUPUser p -> "default (" <> viewPreference p <> ")"
  CUPContact p -> viewPreference p

viewGroupUpdated :: GroupInfo -> GroupInfo -> Maybe GroupMember -> [StyledString]
viewGroupUpdated
  GroupInfo {localDisplayName = n, groupProfile = GroupProfile {fullName, description, image, groupPreferences = gps}}
  g'@GroupInfo {localDisplayName = n', groupProfile = GroupProfile {fullName = fullName', description = description', image = image', groupPreferences = gps'}}
  m = do
    let update = groupProfileUpdated <> groupPrefsUpdated
    if null update
      then []
      else memberUpdated <> update
    where
      memberUpdated = maybe [] (\m' -> [ttyMember m' <> " updated group " <> ttyGroup n <> ":"]) m
      groupProfileUpdated =
        ["changed to " <> ttyFullGroup g' | n /= n']
          <> ["full name " <> if T.null fullName' || fullName' == n' then "removed" else "changed to: " <> plain fullName' | n == n' && fullName /= fullName']
          <> ["profile image " <> maybe "removed" (const "updated") image' | image /= image']
          <> (if description == description' then [] else maybe ["description removed"] ((bold' "description changed to:" :) . map plain . T.lines) description')
      groupPrefsUpdated
        | null prefs = []
        | otherwise = bold' "updated group preferences:" : prefs
        where
          prefs = mapMaybe viewPref allGroupFeatures
          viewPref (AGF f)
            | pref gps == pref gps' = Nothing
            | otherwise = Just $ viewGroupFeatureText f <> " enabled: " <> viewGroupPreference (pref gps')
            where
              pref = getGroupPreference f . mergeGroupPreferences

viewGroupFeatureText :: SGroupFeature f -> StyledString
viewGroupFeatureText f = plain . groupFeatureToText $ toGroupFeature f

viewGroupPreference :: GroupFeatureI f => GroupFeaturePreference f -> StyledString
viewGroupPreference p = plain $ groupPrefToText' p

viewGroupProfile :: GroupInfo -> [StyledString]
viewGroupProfile g@GroupInfo {groupProfile = GroupProfile {description, image, groupPreferences = gps}} =
  [ttyFullGroup g]
    <> maybe [] (const ["has profile image"]) image
    <> maybe [] ((bold' "description:" :) . map plain . T.lines) description
    <> (bold' "group preferences:" : map viewPref allGroupFeatures)
  where
    viewPref (AGF f) = viewGroupFeatureText f <> " enabled: " <> viewGroupPreference (pref gps)
      where
        pref = getGroupPreference f . mergeGroupPreferences

bold' :: String -> StyledString
bold' = styled Bold

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

viewReceivedMessage :: StyledString -> [StyledString] -> MsgContent -> CurrentTime -> CIMeta d -> [StyledString]
viewReceivedMessage = viewReceivedMessage_ False

viewReceivedUpdatedMessage :: StyledString -> [StyledString] -> MsgContent -> CurrentTime -> CIMeta d -> [StyledString]
viewReceivedUpdatedMessage = viewReceivedMessage_ True

viewReceivedMessage_ :: Bool -> StyledString -> [StyledString] -> MsgContent -> CurrentTime -> CIMeta d -> [StyledString]
viewReceivedMessage_ updated from quote mc ts meta = receivedWithTime_ ts from quote meta (ttyMsgContent mc) updated

receivedWithTime_ :: CurrentTime -> StyledString -> [StyledString] -> CIMeta d -> [StyledString] -> Bool -> [StyledString]
receivedWithTime_ ts from quote CIMeta {localItemTs, itemId, itemEdited, itemDeleted, itemLive} styledMsg updated = do
  prependFirst (ttyMsgTime ts localItemTs <> " " <> from) (quote <> prependFirst (indent <> live) styledMsg)
  where
    indent = if null quote then "" else "      "
    live
      | itemEdited || itemDeleted = ""
      | otherwise = case itemLive of
        Just True
          | updated -> ttyFrom "[LIVE] "
          | otherwise -> ttyFrom "[LIVE started]" <> " use " <> highlight' ("/show [on/off/" <> show itemId <> "] ")
        Just False -> ttyFrom "[LIVE ended] "
        _ -> ""

ttyMsgTime :: CurrentTime -> ZonedTime -> StyledString
ttyMsgTime ts t =
  let localTime = zonedTimeToLocalTime t
      tz = zonedTimeZone t
      fmt =
        if (localDay localTime < localDay (zonedTimeToLocalTime $ utcToZonedTime tz ts))
          && (timeOfDayToTime (localTimeOfDay localTime) > (6 * 60 * 60 :: DiffTime))
          then "%m-%d" -- if message is from yesterday or before and 6 hours has passed since midnight
          else "%H:%M"
   in styleTime $ formatTime defaultTimeLocale fmt localTime

viewSentMessage :: StyledString -> [StyledString] -> MsgContent -> CurrentTime -> CIMeta d -> [StyledString]
viewSentMessage to quote mc ts meta@CIMeta {itemEdited, itemDeleted, itemLive} = sentWithTime_ ts (prependFirst to $ quote <> prependFirst (indent <> live) (ttyMsgContent mc)) meta
  where
    indent = if null quote then "" else "      "
    live
      | itemEdited || itemDeleted = ""
      | otherwise = case itemLive of
        Just True -> ttyTo "[LIVE started] "
        Just False -> ttyTo "[LIVE] "
        _ -> ""

viewSentBroadcast :: MsgContent -> Int -> CurrentTime -> ZonedTime -> [StyledString]
viewSentBroadcast mc n ts t = prependFirst (highlight' "/feed" <> " (" <> sShow n <> ") " <> ttyMsgTime ts t <> " ") (ttyMsgContent mc)

viewSentFileInvitation :: StyledString -> CIFile d -> CurrentTime -> CIMeta d -> [StyledString]
viewSentFileInvitation to CIFile {fileId, filePath, fileStatus} ts = case filePath of
  Just fPath -> sentWithTime_ ts $ ttySentFile fPath
  _ -> const []
  where
    ttySentFile fPath = ["/f " <> to <> ttyFilePath fPath] <> cancelSending
    cancelSending = case fileStatus of
      CIFSSndTransfer -> []
      _ -> ["use " <> highlight ("/fc " <> show fileId) <> " to cancel sending"]

sentWithTime_ :: CurrentTime -> [StyledString] -> CIMeta d -> [StyledString]
sentWithTime_ ts styledMsg CIMeta {localItemTs} =
  prependFirst (ttyMsgTime ts localItemTs <> " ") styledMsg

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

viewReceivedFileInvitation :: StyledString -> CIFile d -> CurrentTime -> CIMeta d -> [StyledString]
viewReceivedFileInvitation from file ts meta = receivedWithTime_ ts from [] meta (receivedFileInvitation_ file) False

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
    CEInlineFileProhibited _ -> ["A small file sent without acceptance - you can enable receiving such files with -f option."]
    CEInvalidQuote -> ["cannot reply to this message"]
    CEInvalidChatItemUpdate -> ["cannot update this item"]
    CEInvalidChatItemDelete -> ["cannot delete this item"]
    CEHasCurrentCall -> ["call already in progress"]
    CENoCurrentCall -> ["no call in progress"]
    CECallContact _ -> []
    CECallState _ -> []
    CEDirectMessagesProhibited dir ct -> viewDirectMessagesProhibited dir ct
    CEAgentVersion -> ["unsupported agent version"]
    CEAgentNoSubResult connId -> ["no subscription result for connection: " <> sShow connId]
    CECommandError e -> ["bad chat command: " <> plain e]
    CEAgentCommandError e -> ["agent command error: " <> plain e]
    CEInternalError e -> ["internal chat error: " <> plain e]
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
ttyToContact c = ttyTo $ "@" <> c <> " "

ttyToContact' :: Contact -> StyledString
ttyToContact' ct@Contact {localDisplayName = c} = ctIncognito ct <> ttyToContact c

ttyToContactEdited' :: Contact -> StyledString
ttyToContactEdited' ct@Contact {localDisplayName = c} = ctIncognito ct <> ttyTo ("@" <> c <> " [edited] ")

ttyQuotedContact :: Contact -> StyledString
ttyQuotedContact Contact {localDisplayName = c} = ttyFrom $ c <> ">"

ttyQuotedMember :: Maybe GroupMember -> StyledString
ttyQuotedMember (Just GroupMember {localDisplayName = c}) = "> " <> ttyFrom c
ttyQuotedMember _ = "> " <> ttyFrom "?"

ttyFromContact :: Contact -> StyledString
ttyFromContact ct@Contact {localDisplayName = c} = ctIncognito ct <> ttyFrom (c <> "> ")

ttyFromContactEdited :: Contact -> StyledString
ttyFromContactEdited ct@Contact {localDisplayName = c} = ctIncognito ct <> ttyFrom (c <> "> [edited] ")

ttyFromContactDeleted :: Contact -> Bool -> StyledString
ttyFromContactDeleted ct@Contact {localDisplayName = c} markedDeleted =
  ctIncognito ct <> ttyFrom (c <> "> " <> deleted)
  where
    deleted = if markedDeleted then "[marked deleted] " else "[deleted] "

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

ttyFromGroup :: GroupInfo -> GroupMember -> StyledString
ttyFromGroup g m = membershipIncognito g <> ttyFrom (fromGroup_ g m)

ttyFromGroupEdited :: GroupInfo -> GroupMember -> StyledString
ttyFromGroupEdited g m = membershipIncognito g <> ttyFrom (fromGroup_ g m <> "[edited] ")

ttyFromGroupDeleted :: GroupInfo -> GroupMember -> Bool -> StyledString
ttyFromGroupDeleted g m markedDeleted =
  membershipIncognito g <> ttyFrom (fromGroup_ g m <> deleted)
  where
    deleted = if markedDeleted then "[marked deleted] " else "[deleted] "

fromGroup_ :: GroupInfo -> GroupMember -> Text
fromGroup_ GroupInfo {localDisplayName = g} GroupMember {localDisplayName = m} =
  "#" <> g <> " " <> m <> "> "

ttyFrom :: Text -> StyledString
ttyFrom = styled $ colored Yellow

ttyTo :: Text -> StyledString
ttyTo = styled $ colored Cyan

ttyToGroup :: GroupInfo -> StyledString
ttyToGroup g@GroupInfo {localDisplayName = n} =
  membershipIncognito g <> ttyTo ("#" <> n <> " ")

ttyToGroupEdited :: GroupInfo -> StyledString
ttyToGroupEdited g@GroupInfo {localDisplayName = n} =
  membershipIncognito g <> ttyTo ("#" <> n <> " [edited] ")

ttyFilePath :: FilePath -> StyledString
ttyFilePath = plain

optFullName :: ContactName -> Text -> StyledString
optFullName localDisplayName fullName = plain $ optionalFullName localDisplayName fullName

ctIncognito :: Contact -> StyledString
ctIncognito ct = if contactConnIncognito ct then incognitoPrefix else ""

membershipIncognito :: GroupInfo -> StyledString
membershipIncognito = memIncognito . membership

memIncognito :: GroupMember -> StyledString
memIncognito m = if memberIncognito m then incognitoPrefix else ""

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
