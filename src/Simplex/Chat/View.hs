{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Simplex.Chat.View where

import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Char (toUpper)
import Data.Function (on)
import Data.Int (Int64)
import Data.List (groupBy, intercalate, intersperse, partition, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1)
import Data.Time (LocalTime (..), TimeOfDay (..), TimeZone (..), utcToLocalTime)
import Data.Time.Calendar (addDays)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.Version as V
import qualified Network.HTTP.Types as Q
import Numeric (showFFloat)
import Simplex.Chat.Call
import Simplex.Chat.Controller
import Simplex.Chat.Help
import Simplex.Chat.Library.Commands (maxImageSize)
import Simplex.Chat.Markdown
import Simplex.Chat.Messages hiding (NewChatItem (..))
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Operators
import Simplex.Chat.Protocol
import Simplex.Chat.Remote.AppVersion (AppVersion (..), pattern AppVersionRange)
import Simplex.Chat.Remote.Types
import Simplex.Chat.Store (AddressSettings (..), AutoAccept (..), GroupLink (..), StoreError (..), UserContactLink (..))
import Simplex.Chat.Styled
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Chat.Types.Shared
import Simplex.Chat.Types.UITheme
import qualified Simplex.FileTransfer.Transport as XFTP
import Simplex.Messaging.Agent.Client (ProtocolTestFailure (..), ProtocolTestStep (..), SubscriptionsInfo (..))
import Simplex.Messaging.Agent.Env.SQLite (NetworkConfig (..), ServerRoles (..))
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Agent.Store.Entity
import Simplex.Messaging.Client (SMPProxyFallback, SMPProxyMode (..), SocksMode (..))
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.File (CryptoFile (..), CryptoFileArgs (..))
import qualified Simplex.Messaging.Crypto.Ratchet as CR
import Simplex.Messaging.Encoding
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (dropPrefix, taggedObjectJSON)
import Simplex.Messaging.Protocol (AProtoServerWithAuth (..), AProtocolType, BlockingInfo (..), BlockingReason (..), ProtocolServer (..), ProtocolTypeI, SProtocolType (..), UserProtocol)
import qualified Simplex.Messaging.Protocol as SMP
import Simplex.Messaging.Transport.Client (TransportHost (..))
import Simplex.Messaging.Util (safeDecodeUtf8, tshow)
import Simplex.Messaging.Version hiding (version)
import Simplex.RemoteControl.Types (RCCtrlAddress (..), RCErrorType (..))
import System.Console.ANSI.Types
#if !defined(dbPostgres)
import Simplex.Messaging.Agent.Store.SQLite.DB (SlowQueryStats (..))
#endif

type CurrentTime = UTCTime

data WCallCommand
  = WCCallStart {media :: CallMedia, aesKey :: Maybe String, useWorker :: Bool}
  | WCCallOffer {offer :: Text, iceCandidates :: Text, media :: CallMedia, aesKey :: Maybe String, useWorker :: Bool}
  | WCCallAnswer {answer :: Text, iceCandidates :: Text}

$(JQ.deriveToJSON (taggedObjectJSON $ dropPrefix "WCCall") ''WCallCommand)

serializeChatError :: Bool -> ChatConfig -> ChatError -> String
serializeChatError isCmd cfg = unlines . map unStyle . chatErrorToView isCmd cfg

serializeChatResponse :: ChatResponseEvent r => (Maybe RemoteHostId, Maybe User) -> ChatConfig -> CurrentTime -> TimeZone -> Maybe RemoteHostId -> r -> String
serializeChatResponse hu cfg ts tz remoteHost_ = unlines . map unStyle . responseToView hu cfg False ts tz remoteHost_

class ChatResponseEvent r where
  responseToView :: (Maybe RemoteHostId, Maybe User) -> ChatConfig -> Bool -> CurrentTime -> TimeZone -> Maybe RemoteHostId -> r -> [StyledString]
  isCommandResponse :: Bool

instance ChatResponseEvent ChatResponse where
  responseToView = chatResponseToView
  isCommandResponse = True

instance ChatResponseEvent ChatEvent where
  responseToView = chatEventToView
  isCommandResponse = False

chatErrorToView :: Bool -> ChatConfig -> ChatError -> [StyledString]
chatErrorToView isCmd ChatConfig {logLevel, testView} = viewChatError isCmd logLevel testView

chatResponseToView :: (Maybe RemoteHostId, Maybe User) -> ChatConfig -> Bool -> CurrentTime -> TimeZone -> Maybe RemoteHostId -> ChatResponse -> [StyledString]
chatResponseToView hu cfg@ChatConfig {logLevel, showReactions, testView} liveItems ts tz outputRH = \case
  CRActiveUser User {profile, uiThemes} -> viewUserProfile (fromLocalProfile profile) <> viewUITheme uiThemes
  CRUsersList users -> viewUsersList users
  CRChatStarted -> ["chat started"]
  CRChatRunning -> ["chat is running"]
  CRChatStopped -> ["chat stopped"]
  CRApiChats u chats -> ttyUser u $ if testView then testViewChats chats else [viewJSON chats]
  CRChats chats -> viewChats ts tz chats
  CRApiChat u chat _ -> ttyUser u $ if testView then testViewChat chat else [viewJSON chat]
  CRChatTags u tags -> ttyUser u $ [viewJSON tags]
  CRServerTestResult u srv testFailure -> ttyUser u $ viewServerTestResult srv testFailure
  CRServerOperatorConditions (ServerOperatorConditions ops _ ca) -> viewServerOperators ops ca
  CRUserServers u uss -> ttyUser u $ concatMap viewUserServers uss <> (if testView then [] else serversUserHelp)
  CRUserServersValidation {} -> []
  CRUsageConditions current _ accepted_ -> viewUsageConditions current accepted_
  CRChatItemTTL u ttl -> ttyUser u $ viewChatItemTTL ttl
  CRNetworkConfig netCfg -> viewNetworkConfig netCfg
  CRContactInfo u ct cStats customUserProfile -> ttyUser u $ viewContactInfo ct cStats customUserProfile
  CRGroupInfo u g s -> ttyUser u $ viewGroupInfo g s
  CRGroupMemberInfo u g m cStats -> ttyUser u $ viewGroupMemberInfo g m cStats
  CRQueueInfo _ msgInfo qInfo ->
    [ "last received msg: " <> maybe "none" viewJSON msgInfo,
      "server queue info: " <> viewJSON qInfo
    ]
  CRContactSwitchStarted {} -> ["switch started"]
  CRGroupMemberSwitchStarted {} -> ["switch started"]
  CRContactSwitchAborted {} -> ["switch aborted"]
  CRGroupMemberSwitchAborted {} -> ["switch aborted"]
  CRContactRatchetSyncStarted {} -> ["connection synchronization started"]
  CRGroupMemberRatchetSyncStarted {} -> ["connection synchronization started"]
  CRConnectionVerified u verified code -> ttyUser u [plain $ if verified then "connection verified" else "connection not verified, current code is " <> code]
  CRContactCode u ct code -> ttyUser u $ viewContactCode ct code testView
  CRGroupMemberCode u g m code -> ttyUser u $ viewGroupMemberCode g m code testView
  CRNewChatItems u chatItems -> viewChatItems ttyUser unmuted u chatItems ts tz
  CRChatItems u _ chatItems -> ttyUser u $ concatMap (\(AChatItem _ _ chat item) -> viewChatItem chat item True ts tz <> viewItemReactions item) chatItems
  CRChatItemInfo u ci ciInfo -> ttyUser u $ viewChatItemInfo ci ciInfo tz
  CRChatItemId u itemId -> ttyUser u [plain $ maybe "no item" show itemId]
  CRChatItemUpdated u (AChatItem _ _ chat item) -> ttyUser u $ unmuted u chat item $ viewItemUpdate chat item liveItems ts tz
  CRChatItemNotChanged u ci -> ttyUser u $ viewItemNotChanged ci
  CRTagsUpdated u _ _ -> ttyUser u ["chat tags updated"]
  CRChatItemsDeleted u deletions byUser timed -> ttyUser u $ viewChatItemsDeleted (unmuted u) deletions byUser timed ts tz testView
  CRGroupChatItemsDeleted u g ciIds byUser member_ -> ttyUser u $ viewGroupChatItemsDeleted g ciIds byUser member_
  CRChatItemReaction u added (ACIReaction _ _ chat reaction) -> ttyUser u $ unmutedReaction u chat reaction $ viewItemReaction showReactions chat reaction added ts tz
  CRReactionMembers u memberReactions -> ttyUser u $ viewReactionMembers memberReactions
  CRBroadcastSent u mc s f t -> ttyUser u $ viewSentBroadcast mc s f ts tz t
  CRCmdOk u_ -> ttyUser' u_ ["ok"]
  CRChatHelp section -> case section of
    HSMain -> chatHelpInfo
    HSFiles -> filesHelpInfo
    HSGroups -> groupsHelpInfo
    HSContacts -> contactsHelpInfo
    HSMyAddress -> myAddressHelpInfo
    HSIncognito -> incognitoHelpInfo
    HSMessages -> messagesHelpInfo
    HSMarkdown -> markdownInfo
    HSRemote -> remoteHelpInfo
    HSSettings -> settingsInfo
    HSDatabase -> databaseHelpInfo
  CRWelcome user -> chatWelcome user
  CRContactsList u cs -> ttyUser u $ viewContactsList cs
  CRUserContactLink u UserContactLink {connLinkContact, addressSettings} -> ttyUser u $ connReqContact_ "Your chat address:" connLinkContact <> viewAddressSettings addressSettings
  CRUserContactLinkUpdated u UserContactLink {addressSettings} -> ttyUser u $ viewAddressSettings addressSettings
  CRContactRequestRejected u UserContactRequest {localDisplayName = c} _ct_ -> ttyUser u [ttyContact c <> ": contact request rejected"]
  CRGroupCreated u g -> ttyUser u $ viewGroupCreated g testView
  CRGroupMembers u g -> ttyUser u $ viewGroupMembers g
  CRMemberSupportChats u g ms -> ttyUser u $ viewMemberSupportChats g ms
  -- CRGroupConversationsArchived u _g _conversations -> ttyUser u []
  -- CRGroupConversationsDeleted u _g _conversations -> ttyUser u []
  CRGroupsList u gs -> ttyUser u $ viewGroupsList gs
  CRSentGroupInvitation u g c _ -> ttyUser u $ viewSentGroupInvitation g c
  CRFileTransferStatus u ftStatus -> ttyUser u $ viewFileTransferStatus ftStatus
  CRFileTransferStatusXFTP u ci -> ttyUser u $ viewFileTransferStatusXFTP ci
  CRUserProfile u p -> ttyUser u $ viewUserProfile p
  CRUserProfileNoChange u -> ttyUser u ["user profile did not change"]
  CRUserPrivacy u u' -> ttyUserPrefix hu outputRH u $ viewUserPrivacy u u'
  CRVersionInfo info _ _ -> viewVersionInfo logLevel info
  CRInvitation u ccLink _ -> ttyUser u $ viewConnReqInvitation ccLink
  CRConnectionIncognitoUpdated u c customUserProfile -> ttyUser u $ viewConnectionIncognitoUpdated c customUserProfile testView
  CRConnectionUserChanged u c c' nu -> ttyUser u $ viewConnectionUserChanged u c nu c'
  CRConnectionPlan u connLink connectionPlan -> ttyUser u $ viewConnectionPlan cfg connLink connectionPlan
  CRNewPreparedChat u (AChat _ (Chat cInfo _ _)) -> ttyUser u $ case cInfo of
    DirectChat ct -> [ttyContact' ct <> ": contact is prepared"]
    GroupChat g _ -> [ttyGroup' g <> ": group is prepared"]
    _ -> ["prepared chat error: unexpected type"]
  CRContactUserChanged u c nu c' -> ttyUser u $ viewContactUserChanged u c nu c'
  CRGroupUserChanged u g nu g' -> ttyUser u $ viewGroupUserChanged u g nu g'
  CRSentConfirmation u _ _customUserProfile -> ttyUser u ["confirmation sent!"]
  CRSentInvitation u _ customUserProfile -> ttyUser u $ viewSentInvitation customUserProfile testView
  CRStartedConnectionToContact u c customUserProfile -> ttyUser u $ viewStartedConnectionToContact c customUserProfile testView
  CRStartedConnectionToGroup u g customUserProfile -> ttyUser u $ viewStartedConnectionToGroup g customUserProfile testView
  CRSentInvitationToContact u _c customUserProfile -> ttyUser u $ viewSentInvitation customUserProfile testView
  CRItemsReadForChat u _chatId -> ttyUser u ["items read for chat"]
  CRContactDeleted u c -> ttyUser u [ttyContact' c <> ": contact is deleted"]
  CRChatCleared u chatInfo -> ttyUser u $ viewChatCleared chatInfo
  CRAcceptingContactRequest u c -> ttyUser u $ viewAcceptingContactRequest c
  CRContactAlreadyExists u c -> ttyUser u [ttyFullContact c <> ": contact already exists"]
  CRUserContactLinkCreated u ccLink -> ttyUser u $ connReqContact_ "Your new chat address is created!" ccLink
  CRUserContactLinkDeleted u -> ttyUser u viewUserContactLinkDeleted
  CRUserAcceptedGroupSent u _g _ -> ttyUser u [] -- [ttyGroup' g <> ": joining the group..."]
  CRUserDeletedMembers u g members wm -> case members of
    [m] -> ttyUser u [ttyGroup' g <> ": you removed " <> ttyMember m <> " from the group" <> withMessages wm]
    mems' -> ttyUser u [ttyGroup' g <> ": you removed " <> sShow (length mems') <> " members from the group" <> withMessages wm]
  CRLeftMemberUser u g -> ttyUser u $ [ttyGroup' g <> ": you left the group"] <> groupPreserved g
  CRGroupDeletedUser u g -> ttyUser u [ttyGroup' g <> ": you deleted the group"]
  CRForwardPlan u count itemIds fc -> ttyUser u $ viewForwardPlan count itemIds fc
  CRRcvFileAccepted u ci -> ttyUser u $ savingFile' ci
  CRRcvFileAcceptedSndCancelled u ft -> ttyUser u $ viewRcvFileSndCancelled ft
  CRSndFileCancelled u _ ftm fts -> ttyUser u $ viewSndFileCancelled ftm fts
  CRRcvFileCancelled u _ ft -> ttyUser u $ receivingFile_ "cancelled" ft
  CRUserProfileUpdated u p p' summary -> ttyUser u $ viewUserProfileUpdated p p' summary
  CRUserProfileImage u p -> ttyUser u $ viewUserProfileImage p
  CRContactPrefsUpdated {user = u, fromContact, toContact} -> ttyUser u $ viewUserContactPrefsUpdated u fromContact toContact
  CRContactAliasUpdated u c -> ttyUser u $ viewContactAliasUpdated c
  CRGroupAliasUpdated u g -> ttyUser u $ viewGroupAliasUpdated g
  CRConnectionAliasUpdated u c -> ttyUser u $ viewConnectionAliasUpdated c
  CRRcvStandaloneFileCreated u ft -> ttyUser u $ receivingFileStandalone "started" ft
  CRSndStandaloneFileCreated u ft -> ttyUser u $ uploadingFileStandalone "started" ft
  CRStandaloneFileInfo info_ -> maybe ["no file information in URI"] (\j -> [viewJSON j]) info_
  CRNetworkStatuses u statuses -> if testView then ttyUser' u $ viewNetworkStatuses statuses else []
  CRJoinedGroupMember u g m -> ttyUser u $ viewJoinedGroupMember g m
  CRMemberAccepted u g m -> ttyUser u $ viewMemberAccepted g m
  CRMemberSupportChatDeleted u g m -> ttyUser u [ttyGroup' g <> ": " <> ttyMember m <> " support chat deleted"]
  CRMembersRoleUser u g members r' -> ttyUser u $ viewMemberRoleUserChanged g members r'
  CRMembersBlockedForAllUser u g members blocked -> ttyUser u $ viewMembersBlockedForAllUser g members blocked
  CRGroupUpdated u g g' m -> ttyUser u $ viewGroupUpdated g g' m
  CRGroupProfile u g -> ttyUser u $ viewGroupProfile g
  CRGroupDescription u g -> ttyUser u $ viewGroupDescription g
  CRGroupLinkCreated u g gLink -> ttyUser u $ groupLink_ "Group link is created!" g gLink
  CRGroupLink u g gLink -> ttyUser u $ groupLink_ "Group link:" g gLink
  CRGroupLinkDeleted u g -> ttyUser u $ viewGroupLinkDeleted g
  CRNewMemberContact u _ g m -> ttyUser u ["contact for member " <> ttyGroup' g <> " " <> ttyMember m <> " is created"]
  CRNewMemberContactSentInv u _ct g m -> ttyUser u ["sent invitation to connect directly to member " <> ttyGroup' g <> " " <> ttyMember m]
  CRCallInvitations _ -> []
  CRContactConnectionDeleted u PendingContactConnection {pccConnId} -> ttyUser u ["connection :" <> sShow pccConnId <> " deleted"]
  CRNtfTokenStatus status -> ["device token status: " <> plain (smpEncode status)]
  CRNtfToken _ status mode srv -> ["device token status: " <> plain (smpEncode status) <> ", notifications mode: " <> plain (strEncode mode) <> ", server: " <> sShow srv]
  CRNtfConns {ntfConns} -> map (\NtfConn {agentConnId, expectedMsg_} -> plain $ show agentConnId <> " " <> show expectedMsg_) ntfConns
  CRConnNtfMessages ntfMsgs -> [sShow ntfMsgs]
  CRCurrentRemoteHost rhi_ ->
    [ maybe
        "Using local profile"
        (\RemoteHostInfo {remoteHostId = rhId, hostDeviceName} -> "Using remote host " <> sShow rhId <> " (" <> plain hostDeviceName <> ")")
        rhi_
    ]
  CRRemoteHostList hs -> viewRemoteHosts hs
  CRRemoteHostStarted {remoteHost_, invitation, localAddrs = RCCtrlAddress {address} :| _, ctrlPort} ->
    [ plain $ maybe ("new remote host" <> started) (\RemoteHostInfo {remoteHostId = rhId} -> "remote host " <> show rhId <> started) remoteHost_,
      "Remote session invitation:",
      plain invitation
    ]
    where
      started = " started on " <> B.unpack (strEncode address) <> ":" <> ctrlPort
  CRRemoteFileStored rhId (CryptoFile filePath cfArgs_) ->
    [plain $ "file " <> filePath <> " stored on remote host " <> show rhId]
      <> maybe [] ((: []) . cryptoFileArgsStr testView) cfArgs_
  CRRemoteCtrlList cs -> viewRemoteCtrls cs
  CRRemoteCtrlConnecting {remoteCtrl_, ctrlAppInfo, appVersion} ->
    [ (maybe "connecting new remote controller" (\RemoteCtrlInfo {remoteCtrlId} -> "connecting remote controller " <> sShow remoteCtrlId) remoteCtrl_ <> ": ")
        <> viewRemoteCtrl ctrlAppInfo appVersion True
    ]
  CRRemoteCtrlConnected RemoteCtrlInfo {remoteCtrlId = rcId, ctrlDeviceName} ->
    ["remote controller " <> sShow rcId <> " session started with " <> plain ctrlDeviceName]
  CRSQLResult rows -> map plain rows
#if !defined(dbPostgres)
  CRArchiveExported archiveErrs -> if null archiveErrs then ["ok"] else ["archive export errors: " <> plain (show archiveErrs)]
  CRArchiveImported archiveErrs -> if null archiveErrs then ["ok"] else ["archive import errors: " <> plain (show archiveErrs)]
  CRSlowSQLQueries {chatQueries, agentQueries} ->
    let viewQuery SlowSQLQuery {query, queryStats = SlowQueryStats {count, timeMax, timeAvg}} =
          ("count: " <> sShow count)
            <> (" :: max: " <> sShow timeMax <> " ms")
            <> (" :: avg: " <> sShow timeAvg <> " ms")
            <> (" :: " <> plain (T.unwords $ T.lines query))
     in ("Chat queries" : map viewQuery chatQueries) <> [""] <> ("Agent queries" : map viewQuery agentQueries)
#endif
  CRDebugLocks {chatLockName, chatEntityLocks, agentLocks} ->
    [ maybe "no chat lock" (("chat lock: " <>) . plain) chatLockName,
      "chat entity locks: " <> viewJSON chatEntityLocks,
      "agent locks: " <> viewJSON agentLocks
    ]
  CRAgentSubsTotal u subsTotal _ -> ttyUser u ["total subscriptions: " <> sShow subsTotal]
  CRAgentServersSummary u serversSummary -> ttyUser u ["agent servers summary: " <> viewJSON serversSummary]
  CRAgentSubs {activeSubs, pendingSubs, removedSubs} ->
    [plain $ "Subscriptions: active = " <> show (sum activeSubs) <> ", pending = " <> show (sum pendingSubs) <> ", removed = " <> show (sum $ M.map length removedSubs)]
      <> ("active subscriptions:" : listSubs activeSubs)
      <> ("pending subscriptions:" : listSubs pendingSubs)
      <> ("removed subscriptions:" : listSubs removedSubs)
    where
      listSubs :: Show a => Map Text a -> [StyledString]
      listSubs = map (\(srv, info) -> plain $ srv <> ": " <> tshow info) . M.assocs
  CRAgentSubsDetails SubscriptionsInfo {activeSubscriptions, pendingSubscriptions, removedSubscriptions} ->
    ("active subscriptions:" : map sShow activeSubscriptions)
      <> ("pending subscriptions: " : map sShow pendingSubscriptions)
      <> ("removed subscriptions: " : map sShow removedSubscriptions)
  CRAgentWorkersSummary {agentWorkersSummary} -> ["agent workers summary: " <> viewJSON agentWorkersSummary]
  CRAgentWorkersDetails {agentWorkersDetails} ->
    [ "agent workers details:",
      viewJSON agentWorkersDetails -- this would be huge, but copypastable when has its own line
    ]
  CRAgentQueuesInfo {agentQueuesInfo} ->
    [ "agent queues info:",
      plain . LB.unpack $ J.encode agentQueuesInfo
    ]
  CRAppSettings as -> ["app settings: " <> viewJSON as]
  CRCustomChatResponse u r -> ttyUser' u $ map plain $ T.lines r
  where
    ttyUser :: User -> [StyledString] -> [StyledString]
    ttyUser user@User {showNtfs, activeUser, viewPwdHash} ss
      | (showNtfs && isNothing viewPwdHash) || activeUser = ttyUserPrefix hu outputRH user ss
      | otherwise = []
    ttyUser' :: Maybe User -> [StyledString] -> [StyledString]
    ttyUser' = maybe id ttyUser
    testViewChats :: [AChat] -> [StyledString]
    testViewChats chats = [sShow $ map toChatView chats]
      where
        toChatView :: AChat -> (Text, Text, Maybe ConnStatus)
        toChatView (AChat _ (Chat cInfo items _)) = case cInfo of
          DirectChat Contact {localDisplayName, activeConn} -> ("@" <> localDisplayName, toCIPreview items Nothing, connStatus <$> activeConn)
          GroupChat GroupInfo {membership, localDisplayName} _scopeInfo -> ("#" <> localDisplayName, toCIPreview items (Just membership), Nothing)
          LocalChat _ -> ("*", toCIPreview items Nothing, Nothing)
          ContactRequest UserContactRequest {localDisplayName} -> ("<@" <> localDisplayName, toCIPreview items Nothing, Nothing)
          ContactConnection PendingContactConnection {pccConnId, pccConnStatus} -> (":" <> T.pack (show pccConnId), toCIPreview items Nothing, Just pccConnStatus)
          CInfoInvalidJSON {} -> ("invalid chat info", "", Nothing)
        toCIPreview :: [CChatItem c] -> Maybe GroupMember -> Text
        toCIPreview (ci : _) membership_ = testViewItem ci membership_
        toCIPreview _ _ = ""
    testViewChat :: AChat -> [StyledString]
    testViewChat (AChat _ Chat {chatInfo, chatItems}) = [sShow $ map toChatView chatItems]
      where
        toChatView :: CChatItem c -> ((Int, Text), Maybe (Int, Text), Maybe String)
        toChatView ci@(CChatItem dir ChatItem {quotedItem, file}) =
          ((msgDirectionInt $ toMsgDirection dir, testViewItem ci (chatInfoMembership chatInfo)), qItem, fPath)
          where
            qItem = case quotedItem of
              Nothing -> Nothing
              Just CIQuote {chatDir = quoteDir, content} ->
                Just (msgDirectionInt $ quoteMsgDirection quoteDir, msgContentText content)
            fPath = case file of
              Just CIFile {fileSource = Just (CryptoFile fp _)} -> Just fp
              _ -> Nothing
    testViewItem :: CChatItem c -> Maybe GroupMember -> Text
    testViewItem (CChatItem _ ci@ChatItem {meta = CIMeta {itemText}}) membership_ =
      let deleted_ = maybe "" (\t -> " [" <> t <> "]") (chatItemDeletedText ci membership_)
       in itemText <> deleted_
    unmuted :: User -> ChatInfo c -> ChatItem c d -> [StyledString] -> [StyledString]
    unmuted u chat ci@ChatItem {chatDir} = unmuted' u chat chatDir $ isUserMention ci
    unmutedReaction :: User -> ChatInfo c -> CIReaction c d -> [StyledString] -> [StyledString]
    unmutedReaction u chat CIReaction {chatDir} = unmuted' u chat chatDir False
    unmuted' :: User -> ChatInfo c -> CIDirection c d -> Bool -> [StyledString] -> [StyledString]
    unmuted' u chat chatDir mention s
      | chatDirNtf u chat chatDir mention = s
      | testView = map (<> " <muted>") s
      | otherwise = []
    withMessages wm = if wm then " with all messages" else ""

ttyUserPrefix :: (Maybe RemoteHostId, Maybe User) -> Maybe RemoteHostId -> User -> [StyledString] -> [StyledString]
ttyUserPrefix _ _ _ [] = []
ttyUserPrefix (currentRH, user_) outputRH User {userId, localDisplayName = u} ss
  | null prefix = ss
  | otherwise = prependFirst ("[" <> mconcat prefix <> "] ") ss
  where
    prefix = intersperse ", " $ remotePrefix <> userPrefix
    remotePrefix = [maybe "local" (("remote: " <>) . highlight . show) outputRH | outputRH /= currentRH]
    userPrefix = ["user: " <> highlight u | Just userId /= currentUserId]
    currentUserId = (\User {userId = uId} -> uId) <$> user_

viewErrorsSummary :: [a] -> StyledString -> [StyledString]
viewErrorsSummary summary s = [ttyError (T.pack . show $ length summary) <> s <> " (run with -c option to show each error)" | not (null summary)]

contactList :: [ContactRef] -> String
contactList cs = T.unpack . T.intercalate ", " $ map (\ContactRef {localDisplayName = n} -> "@" <> n) cs

chatEventToView :: (Maybe RemoteHostId, Maybe User) -> ChatConfig -> Bool -> CurrentTime -> TimeZone -> Maybe RemoteHostId -> ChatEvent -> [StyledString]
chatEventToView hu ChatConfig {logLevel, showReactions, showReceipts, testView} liveItems ts tz outputRH = \case
  CEvtChatSuspended -> ["chat suspended"]
  CEvtContactSwitch u ct progress -> ttyUser u $ viewContactSwitch ct progress
  CEvtGroupMemberSwitch u g m progress -> ttyUser u $ viewGroupMemberSwitch g m progress
  CEvtContactRatchetSync u ct progress -> ttyUser u $ viewContactRatchetSync ct progress
  CEvtGroupMemberRatchetSync u g m progress -> ttyUser u $ viewGroupMemberRatchetSync g m progress
  CEvtChatInfoUpdated _ _ -> []
  CEvtNewChatItems u chatItems -> viewChatItems ttyUser unmuted u chatItems ts tz
  CEvtChatItemsStatusesUpdated u chatItems
    | length chatItems <= 20 ->
        concatMap
          (\ci -> ttyUser u $ viewChatItemStatusUpdated ci ts tz testView showReceipts)
          chatItems
    | testView && showReceipts ->
        ttyUser u [sShow (length chatItems) <> " message statuses updated"]
    | otherwise -> []
  CEvtChatItemUpdated u (AChatItem _ _ chat item) -> ttyUser u $ unmuted u chat item $ viewItemUpdate chat item liveItems ts tz
  CEvtChatItemNotChanged u ci -> ttyUser u $ viewItemNotChanged ci
  CEvtChatItemReaction u added (ACIReaction _ _ chat reaction) -> ttyUser u $ unmutedReaction u chat reaction $ viewItemReaction showReactions chat reaction added ts tz
  CEvtChatItemsDeleted u deletions byUser timed -> ttyUser u $ viewChatItemsDeleted (unmuted u) deletions byUser timed ts tz testView
  CEvtGroupChatItemsDeleted u g ciIds byUser member_ -> ttyUser u $ viewGroupChatItemsDeleted g ciIds byUser member_
  CEvtChatItemDeletedNotFound u Contact {localDisplayName = c} _ -> ttyUser u [ttyFrom $ c <> "> [deleted - original message not found]"]
  CEvtUserAcceptedGroupSent u _g _ -> ttyUser u [] -- [ttyGroup' g <> ": joining the group..."]
  CEvtSentGroupInvitation u g c _ -> ttyUser u $ viewSentGroupInvitation g c
  CEvtContactDeletedByContact u c -> ttyUser u [ttyFullContact c <> " deleted contact with you"]
  CEvtAcceptingContactRequest u c -> ttyUser u $ viewAcceptingContactRequest c
  CEvtAcceptingBusinessRequest u g -> ttyUser u $ viewAcceptingBusinessRequest g
  CEvtContactRequestAlreadyAccepted u c -> ttyUser u [ttyFullContact c <> ": sent you a duplicate contact request, but you are already connected, no action needed"]
  CEvtBusinessRequestAlreadyAccepted u g -> ttyUser u [ttyFullGroup g <> ": sent you a duplicate connection request, but you are already connected, no action needed"]
  CEvtGroupLinkConnecting u g _ -> ttyUser u [ttyGroup' g <> ": joining the group..."]
  CEvtBusinessLinkConnecting u g _ _ -> ttyUser u [ttyGroup' g <> ": joining the group..."]
  CEvtUnknownMemberCreated u g fwdM um -> ttyUser u [ttyGroup' g <> ": " <> ttyMember fwdM <> " forwarded a message from an unknown member, creating unknown member record " <> ttyMember um]
  CEvtUnknownMemberBlocked u g byM um -> ttyUser u [ttyGroup' g <> ": " <> ttyMember byM <> " blocked an unknown member, creating unknown member record " <> ttyMember um]
  CEvtUnknownMemberAnnounced u g _ um m -> ttyUser u [ttyGroup' g <> ": unknown member " <> ttyMember um <> " updated to " <> ttyMember m]
  CEvtRcvFileDescrReady _ _ _ _ -> []
  CEvtRcvFileAccepted u ci -> ttyUser u $ savingFile' ci
  CEvtRcvFileAcceptedSndCancelled u ft -> ttyUser u $ viewRcvFileSndCancelled ft
  CEvtRcvFileProgressXFTP {} -> []
  CEvtContactUpdated {user = u, fromContact = c, toContact = c'} -> ttyUser u $ viewContactUpdated c c' <> viewContactPrefsUpdated u c c'
  CEvtGroupMemberUpdated {} -> []
  CEvtContactsMerged u intoCt mergedCt ct' -> ttyUser u $ viewContactsMerged intoCt mergedCt ct'
  CEvtReceivedContactRequest u UserContactRequest {localDisplayName = c, profile} _chat -> ttyUser u $ viewReceivedContactRequest c profile
  CEvtRcvFileStart u ci -> ttyUser u $ receivingFile_' hu testView "started" ci
  CEvtRcvFileComplete u ci -> ttyUser u $ receivingFile_' hu testView "completed" ci
  CEvtRcvStandaloneFileComplete u _ ft -> ttyUser u $ receivingFileStandalone "completed" ft
  CEvtRcvFileSndCancelled u _ ft -> ttyUser u $ viewRcvFileSndCancelled ft
  CEvtRcvFileError u (Just ci) e _ -> ttyUser u $ receivingFile_' hu testView "error" ci <> [sShow e]
  CEvtRcvFileError u Nothing e ft -> ttyUser u $ receivingFileStandalone "error" ft <> [sShow e]
  CEvtRcvFileWarning u (Just ci) e _ -> ttyUser u $ receivingFile_' hu testView "warning: " ci <> [sShow e]
  CEvtRcvFileWarning u Nothing e ft -> ttyUser u $ receivingFileStandalone "warning: " ft <> [sShow e]
  CEvtSndFileStart u _ ft -> ttyUser u $ sendingFile_ "started" ft
  CEvtSndFileComplete u _ ft -> ttyUser u $ sendingFile_ "completed" ft
  CEvtSndFileProgressXFTP {} -> []
  CEvtSndFileRedirectStartXFTP u ft ftRedirect -> ttyUser u $ standaloneUploadRedirect ft ftRedirect
  CEvtSndStandaloneFileComplete u ft uris -> ttyUser u $ standaloneUploadComplete ft uris
  CEvtSndFileCompleteXFTP u ci _ -> ttyUser u $ uploadingFile "completed" ci
  CEvtSndFileError u Nothing ft e -> ttyUser u $ uploadingFileStandalone "error" ft <> [plain e]
  CEvtSndFileError u (Just ci) _ e -> ttyUser u $ uploadingFile "error" ci <> [plain e]
  CEvtSndFileWarning u Nothing ft e -> ttyUser u $ uploadingFileStandalone "warning: " ft <> [plain e]
  CEvtSndFileWarning u (Just ci) _ e -> ttyUser u $ uploadingFile "warning: " ci <> [plain e]
  CEvtSndFileRcvCancelled u _ ft@SndFileTransfer {recipientDisplayName = c} ->
    ttyUser u [ttyContact c <> " cancelled receiving " <> sndFile ft]
  CEvtContactConnecting u _ -> ttyUser u []
  CEvtContactConnected u ct userCustomProfile -> ttyUser u $ viewContactConnected ct userCustomProfile testView
  CEvtContactSndReady u ct -> ttyUser u [ttyFullContact ct <> ": you can send messages to contact"]
  CEvtContactAnotherClient u c -> ttyUser u [ttyContact' c <> ": contact is connected to another client"]
  CEvtSubscriptionEnd u acEntity ->
    let Connection {connId} = entityConnection acEntity
     in ttyUser u [sShow connId <> ": END"]
  CEvtContactsDisconnected srv cs -> [plain $ "server disconnected " <> showSMPServer srv <> " (" <> contactList cs <> ")"]
  CEvtContactsSubscribed srv cs -> [plain $ "server connected " <> showSMPServer srv <> " (" <> contactList cs <> ")"]
  CEvtContactSubError u c e -> ttyUser u [ttyContact' c <> ": contact error " <> sShow e]
  CEvtContactSubSummary u summary ->
    ttyUser u $ [sShow (length subscribed) <> " contacts connected (use " <> highlight' "/cs" <> " for the list)" | not (null subscribed)] <> viewErrorsSummary errors " contact errors"
    where
      (errors, subscribed) = partition (isJust . contactError) summary
  CEvtUserContactSubSummary u summary ->
    ttyUser u $
      map addressSS addresses
        <> ([sShow (length groupLinksSubscribed) <> " group links active" | not (null groupLinksSubscribed)] <> viewErrorsSummary groupLinkErrors " group link errors")
    where
      (addresses, groupLinks) = partition (\UserContactSubStatus {userContact} -> isNothing . userContactGroupId $ userContact) summary
      addressSS UserContactSubStatus {userContactError} = maybe ("Your address is active! To show: " <> highlight' "/sa") (\e -> "User address error: " <> sShow e <> ", to delete your address: " <> highlight' "/da") userContactError
      (groupLinkErrors, groupLinksSubscribed) = partition (isJust . userContactError) groupLinks
  CEvtNetworkStatus status conns -> if testView then [plain $ show (length conns) <> " connections " <> netStatusStr status] else []
  CEvtNetworkStatuses u statuses -> if testView then ttyUser' u $ viewNetworkStatuses statuses else []
  CEvtReceivedGroupInvitation {user = u, groupInfo = g, contact = c, memberRole = r} -> ttyUser u $ viewReceivedGroupInvitation g c r
  CEvtUserJoinedGroup u g _ -> ttyUser u $ viewUserJoinedGroup g
  CEvtJoinedGroupMember u g m -> ttyUser u $ viewJoinedGroupMember g m
  CEvtHostConnected p h -> [plain $ "connected to " <> viewHostEvent p h]
  CEvtHostDisconnected p h -> [plain $ "disconnected from " <> viewHostEvent p h]
  CEvtJoinedGroupMemberConnecting u g host m -> ttyUser u $ viewJoinedGroupMemberConnecting g host m
  CEvtConnectedToGroupMember u g m _ -> ttyUser u $ viewConnectedToGroupMember g m
  CEvtMemberAcceptedByOther u g acceptingMember m -> ttyUser u $ viewMemberAcceptedByOther g acceptingMember m
  CEvtMemberRole u g by m r r' -> ttyUser u $ viewMemberRoleChanged g by m r r'
  CEvtMemberBlockedForAll u g by m blocked -> ttyUser u $ viewMemberBlockedForAll g by m blocked
  CEvtDeletedMemberUser u g by wm -> ttyUser u $ [ttyGroup' g <> ": " <> ttyMember by <> " removed you from the group" <> withMessages wm] <> groupPreserved g
  CEvtDeletedMember u g by m wm -> ttyUser u [ttyGroup' g <> ": " <> ttyMember by <> " removed " <> ttyMember m <> " from the group" <> withMessages wm]
  CEvtLeftMember u g m -> ttyUser u [ttyGroup' g <> ": " <> ttyMember m <> " left the group"]
  CEvtGroupDeleted u g m -> ttyUser u [ttyGroup' g <> ": " <> ttyMember m <> " deleted the group", "use " <> highlight ("/d #" <> viewGroupName g) <> " to delete the local copy of the group"]
  CEvtGroupUpdated u g g' m -> ttyUser u $ viewGroupUpdated g g' m
  CEvtAcceptingGroupJoinRequestMember _ g m -> [ttyFullMember m <> ": accepting request to join group " <> ttyGroup' g <> "..."]
  CEvtNoMemberContactCreating u g m -> ttyUser u ["member " <> ttyGroup' g <> " " <> ttyMember m <> " does not have direct connection, creating"]
  CEvtNewMemberContactReceivedInv u ct g m -> ttyUser u [ttyGroup' g <> " " <> ttyMember m <> " is creating direct contact " <> ttyContact' ct <> " with you"]
  CEvtContactAndMemberAssociated u ct g m ct' -> ttyUser u $ viewContactAndMemberAssociated ct g m ct'
  CEvtCallInvitation RcvCallInvitation {user, contact, callType, sharedKey} -> ttyUser user $ viewCallInvitation contact callType sharedKey
  CEvtCallOffer {user = u, contact, callType, offer, sharedKey} -> ttyUser u $ viewCallOffer contact callType offer sharedKey
  CEvtCallAnswer {user = u, contact, answer} -> ttyUser u $ viewCallAnswer contact answer
  CEvtCallExtraInfo {user = u, contact} -> ttyUser u ["call extra info from " <> ttyContact' contact]
  CEvtCallEnded {user = u, contact} -> ttyUser u ["call with " <> ttyContact' contact <> " ended"]
  CEvtNtfMessage {} -> []
  CEvtRemoteHostSessionCode {remoteHost_, sessionCode} ->
    [ maybe "new remote host connecting" (\RemoteHostInfo {remoteHostId = rhId} -> "remote host " <> sShow rhId <> " connecting") remoteHost_,
      "Compare session code with host:",
      plain sessionCode
    ]
  CEvtNewRemoteHost RemoteHostInfo {remoteHostId = rhId, hostDeviceName} -> ["new remote host " <> sShow rhId <> " added: " <> plain hostDeviceName]
  CEvtRemoteHostConnected RemoteHostInfo {remoteHostId = rhId} -> ["remote host " <> sShow rhId <> " connected"]
  CEvtRemoteHostStopped {remoteHostId_} ->
    [ maybe "new remote host" (mappend "remote host " . sShow) remoteHostId_ <> " stopped"
    ]
  CEvtRemoteCtrlFound {remoteCtrl = RemoteCtrlInfo {remoteCtrlId, ctrlDeviceName}, ctrlAppInfo_, appVersion, compatible} ->
    [ ("remote controller " <> sShow remoteCtrlId <> " found: ")
        <> maybe (deviceName <> "not compatible") (\info -> viewRemoteCtrl info appVersion compatible) ctrlAppInfo_
    ]
      <> ["use " <> highlight ("/confirm remote ctrl " <> show remoteCtrlId) <> " to connect" | isJust ctrlAppInfo_ && compatible]
    where
      deviceName = if T.null ctrlDeviceName then "" else plain ctrlDeviceName <> ", "
  CEvtRemoteCtrlSessionCode {remoteCtrl_, sessionCode} ->
    [ maybe "new remote controller connected" (\RemoteCtrlInfo {remoteCtrlId} -> "remote controller " <> sShow remoteCtrlId <> " connected") remoteCtrl_,
      "Compare session code with controller and use:",
      "/verify remote ctrl " <> plain sessionCode -- TODO maybe pass rcId
    ]
  CEvtRemoteCtrlStopped {rcStopReason} -> viewRemoteCtrlStopped rcStopReason
  CEvtContactPQEnabled u c (CR.PQEncryption pqOn) -> ttyUser u [ttyContact' c <> ": " <> (if pqOn then "quantum resistant" else "standard") <> " end-to-end encryption enabled"]
  CEvtContactDisabled u c -> ttyUser u ["[" <> ttyContact' c <> "] connection is disabled, to enable: " <> highlight ("/enable " <> viewContactName c) <> ", to delete: " <> highlight ("/d " <> viewContactName c)]
  CEvtConnectionDisabled entity -> viewConnectionEntityDisabled entity
  CEvtConnectionInactive entity inactive -> viewConnectionEntityInactive entity inactive
  CEvtAgentRcvQueuesDeleted delQs -> ["completed deleting rcv queues: " <> sShow (length delQs) | logLevel <= CLLInfo]
  CEvtAgentConnsDeleted acIds -> ["completed deleting connections: " <> sShow (length acIds) | logLevel <= CLLInfo]
  CEvtAgentUserDeleted auId -> ["completed deleting user" <> if logLevel <= CLLInfo then ", agent user id: " <> sShow auId else ""]
  CEvtMessageError u prefix err -> ttyUser u [plain prefix <> ": " <> plain err | prefix == "error" || logLevel <= CLLWarning]
  CEvtChatErrors errs -> concatMap (viewChatError False logLevel testView) errs
  CEvtTimedAction _ _ -> []
  CEvtTerminalEvent te -> case te of
    TERejectingGroupJoinRequestMember _ g m reason -> [ttyFullMember m <> ": rejecting request to join group " <> ttyGroup' g <> ", reason: " <> sShow reason]
    TEGroupLinkRejected u g reason -> ttyUser u [ttyGroup' g <> ": join rejected, reason: " <> sShow reason]
    TENewMemberContact u _ g m -> ttyUser u ["contact for member " <> ttyGroup' g <> " " <> ttyMember m <> " is created"]
    TEContactVerificationReset u ct -> ttyUser u $ viewContactVerificationReset ct
    TEGroupMemberVerificationReset u g m -> ttyUser u $ viewGroupMemberVerificationReset g m
    TEGroupSubscribed u ShortGroupInfo {groupName = g} -> ttyUser u $ viewGroupSubscribed g
    TEGroupInvitation u g -> ttyUser u [groupInvitationSub g]
    TEGroupEmpty u ShortGroupInfo {groupName = g} -> ttyUser u [ttyGroup g <> ": group is empty"]
    TEMemberSubError u ShortGroupInfo {groupName = g} ShortGroupMember {memberName = n} e -> ttyUser u [ttyGroup g <> " member " <> ttyContact n <> " error: " <> sShow e]
    TEMemberSubSummary u summary -> ttyUser u $ viewErrorsSummary (filter (isJust . memberError) summary) " group member errors"
    TEPendingSubSummary u _ -> ttyUser u []
    TESndFileSubError u SndFileTransfer {fileId, fileName} e ->
      ttyUser u ["sent file " <> sShow fileId <> " (" <> plain fileName <> ") error: " <> sShow e]
    TERcvFileSubError u RcvFileTransfer {fileId, fileInvitation = FileInvitation {fileName}} e ->
      ttyUser u ["received file " <> sShow fileId <> " (" <> plain fileName <> ") error: " <> sShow e]
  CEvtCustomChatEvent u r -> ttyUser' u $ map plain $ T.lines r
  where
    ttyUser :: User -> [StyledString] -> [StyledString]
    ttyUser user@User {showNtfs, activeUser, viewPwdHash} ss
      | (showNtfs && isNothing viewPwdHash) || activeUser = ttyUserPrefix hu outputRH user ss
      | otherwise = []
    ttyUser' :: Maybe User -> [StyledString] -> [StyledString]
    ttyUser' = maybe id ttyUser
    withMessages wm = if wm then " with all messages" else ""
    unmuted :: User -> ChatInfo c -> ChatItem c d -> [StyledString] -> [StyledString]
    unmuted u chat ci@ChatItem {chatDir} = unmuted' u chat chatDir $ isUserMention ci
    unmutedReaction :: User -> ChatInfo c -> CIReaction c d -> [StyledString] -> [StyledString]
    unmutedReaction u chat CIReaction {chatDir} = unmuted' u chat chatDir False
    unmuted' :: User -> ChatInfo c -> CIDirection c d -> Bool -> [StyledString] -> [StyledString]
    unmuted' u chat chatDir mention s
      | chatDirNtf u chat chatDir mention = s
      | testView = map (<> " <muted>") s
      | otherwise = []

userNtf :: User -> Bool
userNtf User {showNtfs, activeUser} = showNtfs || activeUser

chatDirNtf :: User -> ChatInfo c -> CIDirection c d -> Bool -> Bool
chatDirNtf user cInfo chatDir mention = case (cInfo, chatDir) of
  (DirectChat ct, CIDirectRcv) -> contactNtf user ct mention
  (GroupChat g _scopeInfo, CIGroupRcv m) -> groupNtf user g mention && not (blockedByAdmin m) && showMessages (memberSettings m)
  _ -> True

contactNtf :: User -> Contact -> Bool -> Bool
contactNtf user Contact {chatSettings} mention =
  userNtf user && showMessageNtf chatSettings mention

groupNtf :: User -> GroupInfo -> Bool -> Bool
groupNtf user GroupInfo {chatSettings} mention =
  userNtf user && showMessageNtf chatSettings mention

showMessageNtf :: ChatSettings -> Bool -> Bool
showMessageNtf ChatSettings {enableNtfs} mention =
  enableNtfs == MFAll || (mention && enableNtfs == MFMentions)

chatItemDeletedText :: ChatItem c d -> Maybe GroupMember -> Maybe Text
chatItemDeletedText ChatItem {meta = CIMeta {itemDeleted}, content} membership_ =
  deletedText <$> itemDeleted
  where
    deletedText = \case
      CIModerated _ m -> markedDeleted content <> byMember m
      CIDeleted _ -> markedDeleted content
      CIBlocked _ -> "blocked"
      CIBlockedByAdmin _ -> "blocked by admin"
    markedDeleted = \case
      CISndModerated -> "deleted"
      CIRcvModerated -> "deleted"
      _ -> "marked deleted"
    byMember GroupMember {groupMemberId = mId, localDisplayName = n} = case membership_ of
      Just GroupMember {groupMemberId = membershipId} ->
        " by " <> if mId == membershipId then "you" else n
      _ -> ""

viewUsersList :: [UserInfo] -> [StyledString]
viewUsersList us =
  let ss = mapMaybe userInfo $ sortOn ldn us
   in if null ss then ["no users"] else ss
  where
    ldn (UserInfo User {localDisplayName = n} _) = T.toLower n
    userInfo (UserInfo User {localDisplayName = n, profile = LocalProfile {fullName, shortDescr}, activeUser, showNtfs, viewPwdHash} count)
      | activeUser || isNothing viewPwdHash = Just $ ttyFullName n fullName shortDescr <> infoStr
      | otherwise = Nothing
      where
        infoStr = if null info then "" else " (" <> mconcat (intersperse ", " info) <> ")"
        info =
          [highlight' "active" | activeUser]
            <> [highlight' "hidden" | isJust viewPwdHash]
            <> ["muted" | not showNtfs]
            <> [plain ("unread: " <> show count) | count /= 0]

viewGroupSubscribed :: GroupName -> [StyledString]
viewGroupSubscribed g = [ttyGroup g <> ": connected to server(s)"]

showSMPServer :: SMPServer -> String
showSMPServer ProtocolServer {host} = B.unpack $ strEncode host

viewHostEvent :: AProtocolType -> TransportHost -> String
viewHostEvent p h = map toUpper (B.unpack $ strEncode p) <> " host " <> B.unpack (strEncode h)

viewChats :: CurrentTime -> TimeZone -> [AChat] -> [StyledString]
viewChats ts tz = concatMap chatPreview . reverse
  where
    chatPreview (AChat _ (Chat chat items _)) = case items of
      CChatItem _ ci : _ -> case viewChatItem chat ci True ts tz of
        s : _ -> [let s' = sTake 120 s in if sLength s' < sLength s then s' <> "..." else s']
        _ -> chatName
      _ -> chatName
      where
        chatName = case chat of
          DirectChat ct -> ["      " <> ttyToContact' ct]
          GroupChat g scopeInfo -> ["      " <> ttyToGroup g scopeInfo]
          _ -> []

viewChatItems ::
  (User -> [StyledString] -> [StyledString]) ->
  (forall c d. User -> ChatInfo c -> ChatItem c d -> [StyledString] -> [StyledString]) ->
  User ->
  [AChatItem] ->
  UTCTime ->
  TimeZone ->
  [StyledString]
viewChatItems ttyUser unmuted u chatItems ts tz
  | length chatItems <= 20 =
      concatMap
        (\(AChatItem _ _ chat item) -> ttyUser u $ unmuted u chat item $ viewChatItem chat item False ts tz <> viewItemReactions item)
        chatItems
  | all (\aci -> aChatItemDir aci == MDRcv) chatItems = ttyUser u [sShow (length chatItems) <> " new messages"]
  | all (\aci -> aChatItemDir aci == MDSnd) chatItems = ttyUser u [sShow (length chatItems) <> " messages sent"]
  | otherwise = ttyUser u [sShow (length chatItems) <> " new messages created"]

viewChatItem :: forall c d. MsgDirectionI d => ChatInfo c -> ChatItem c d -> Bool -> CurrentTime -> TimeZone -> [StyledString]
viewChatItem chat ci@ChatItem {chatDir, meta = meta@CIMeta {itemForwarded, forwardedByMember, userMention}, content, quotedItem, file} doShow ts tz =
  withGroupMsgForwarded . withItemDeleted <$> viewCI
  where
    viewCI = case chat of
      DirectChat c -> case chatDir of
        CIDirectSnd -> case content of
          CISndMsgContent mc -> hideLive meta $ withSndFile to $ sndMsg to context mc
          CISndGroupEvent {} -> showSndItemProhibited to
          _ -> showSndItem to
          where
            to = ttyToContact' c
        CIDirectRcv -> case content of
          CIRcvMsgContent mc -> withRcvFile from $ rcvMsg from context mc
          CIRcvIntegrityError err -> viewRcvIntegrityError from err ts tz meta
          CIRcvGroupEvent {} -> showRcvItemProhibited from
          _ -> showRcvItem from
          where
            from = ttyFromContact c
        where
          context =
            maybe
              (maybe [] forwardedFrom itemForwarded)
              (directQuote chatDir)
              quotedItem
      GroupChat g scopeInfo -> case chatDir of
        CIGroupSnd -> case content of
          CISndMsgContent mc -> hideLive meta $ withSndFile to $ sndMsg to context mc
          CISndGroupInvitation {} -> showSndItemProhibited to
          _ -> showSndItem to
          where
            to = ttyToGroup g scopeInfo
        CIGroupRcv m -> case content of
          CIRcvMsgContent mc -> withRcvFile from $ rcvMsg from context mc
          CIRcvIntegrityError err -> viewRcvIntegrityError from err ts tz meta
          CIRcvGroupInvitation {} -> showRcvItemProhibited from
          CIRcvModerated {} -> receivedWithTime_ ts tz (ttyFromGroup g scopeInfo m) context meta [plainContent content] False
          CIRcvBlocked {} -> receivedWithTime_ ts tz (ttyFromGroup g scopeInfo m) context meta [plainContent content] False
          _ -> showRcvItem from
          where
            from = ttyFromGroupAttention g scopeInfo m userMention
        where
          context =
            maybe
              (maybe [] forwardedFrom itemForwarded)
              (groupQuote g)
              quotedItem
      LocalChat _ -> case chatDir of
        CILocalSnd -> case content of
          CISndMsgContent mc -> hideLive meta $ withLocalFile to $ sndMsg to context mc
          CISndGroupEvent {} -> showSndItemProhibited to
          _ -> showSndItem to
          where
            to = "* "
        CILocalRcv -> case content of
          CIRcvMsgContent mc -> withLocalFile from $ rcvMsg from context mc
          CIRcvIntegrityError err -> viewRcvIntegrityError from err ts tz meta
          CIRcvGroupEvent {} -> showRcvItemProhibited from
          _ -> showRcvItem from
          where
            from = "* "
        where
          context = maybe [] forwardedFrom itemForwarded
      ContactRequest {} -> []
      ContactConnection {} -> []
      CInfoInvalidJSON {} -> ["invalid chat info"]
    withItemDeleted item = case chatItemDeletedText ci (chatInfoMembership chat) of
      Nothing -> item
      Just t -> item <> styled (colored Red) (" [" <> t <> "]")
    withGroupMsgForwarded item = case forwardedByMember of
      Nothing -> item
      Just _ -> item <> styled (colored Yellow) (" [>>]" :: String)
    withSndFile = withFile viewSentFileInvitation
    withRcvFile = withFile viewReceivedFileInvitation
    withLocalFile = withFile viewLocalFile
    withFile view dir l = maybe l (\f -> l <> view dir f ts tz meta) file
    sndMsg = msg viewSentMessage
    rcvMsg = msg viewReceivedMessage
    msg view dir context mc = case (msgContentText mc, file, context) of
      ("", Just _, []) -> []
      ("", Just CIFile {fileName}, _) -> view dir context (MCText $ T.pack fileName) ts tz meta
      _ -> view dir context mc ts tz meta
    showSndItem to = showItem $ sentWithTime_ ts tz [to <> plainContent content] meta
    showRcvItem from = showItem $ receivedWithTime_ ts tz from [] meta [plainContent content] False
    showSndItemProhibited to = showItem $ sentWithTime_ ts tz [to <> plainContent content <> " " <> prohibited] meta
    showRcvItemProhibited from = showItem $ receivedWithTime_ ts tz from [] meta [plainContent content <> " " <> prohibited] False
    showItem ss = if doShow then ss else []
    plainContent = plain . ciContentToText
    prohibited = styled (colored Red) ("[unexpected chat item created, please report to developers]" :: String)

viewChatItemInfo :: AChatItem -> ChatItemInfo -> TimeZone -> [StyledString]
viewChatItemInfo (AChatItem _ msgDir _ ChatItem {meta = CIMeta {itemTs, itemTimed, createdAt}}) ChatItemInfo {itemVersions, forwardedFromChatItem} tz =
  ["sent at: " <> ts itemTs]
    <> receivedAt
    <> toBeDeletedAt
    <> versions
    <> forwardedFrom'
  where
    ts = styleTime . localTs tz
    receivedAt = case msgDir of
      SMDRcv -> ["received at: " <> ts createdAt]
      SMDSnd -> []
    toBeDeletedAt = case itemTimed >>= timedDeleteAt' of
      Just d -> ["to be deleted at: " <> ts d]
      Nothing -> []
    versions =
      if null itemVersions
        then []
        else ["message history:"] <> concatMap version itemVersions
      where
        version ChatItemVersion {msgContent, itemVersionTs} = prependFirst (ts itemVersionTs <> styleTime ": ") $ ttyMsgContent msgContent
    forwardedFrom' =
      case forwardedFromChatItem of
        Just fwdACI@(AChatItem _ fwdMsgDir fwdChatInfo _) ->
          [plain $ "forwarded from: " <> maybe "" (<> ", ") fwdDir_ <> fwdItemId]
          where
            fwdDir_ = case (fwdMsgDir, fwdChatInfo) of
              (SMDSnd, DirectChat ct) -> Just $ "you @" <> viewContactName ct
              (SMDRcv, DirectChat ct) -> Just $ "@" <> viewContactName ct
              (SMDSnd, GroupChat gInfo _scopeInfo) -> Just $ "you #" <> viewGroupName gInfo
              (SMDRcv, GroupChat gInfo _scopeInfo) -> Just $ "#" <> viewGroupName gInfo
              _ -> Nothing
            fwdItemId = "chat item id: " <> (T.pack . show $ aChatItemId fwdACI)
        _ -> []

localTs :: TimeZone -> UTCTime -> String
localTs tz ts = do
  let localTime = utcToLocalTime tz ts
      formattedTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" localTime
  formattedTime

viewChatItemStatusUpdated :: AChatItem -> CurrentTime -> TimeZone -> Bool -> Bool -> [StyledString]
viewChatItemStatusUpdated (AChatItem _ _ chat item@ChatItem {meta = CIMeta {itemStatus}}) ts tz testView showReceipts =
  case itemStatus of
    CISSndRcvd rcptStatus SSPPartial | testView && showReceipts ->
      prependFirst (viewDeliveryReceiptPartial rcptStatus <> " ") $ viewChatItem chat item False ts tz
    CISSndRcvd rcptStatus SSPComplete | testView && showReceipts ->
      prependFirst (viewDeliveryReceipt rcptStatus <> " ") $ viewChatItem chat item False ts tz
    _ -> []

viewDeliveryReceiptPartial :: MsgReceiptStatus -> StyledString
viewDeliveryReceiptPartial = \case
  MROk -> "%"
  MRBadMsgHash -> ttyError' "%!"

viewDeliveryReceipt :: MsgReceiptStatus -> StyledString
viewDeliveryReceipt = \case
  MROk -> "⩗"
  MRBadMsgHash -> ttyError' "⩗!"

viewItemUpdate :: MsgDirectionI d => ChatInfo c -> ChatItem c d -> Bool -> CurrentTime -> TimeZone -> [StyledString]
viewItemUpdate chat ChatItem {chatDir, meta = meta@CIMeta {itemForwarded, itemEdited, itemLive}, content, quotedItem} liveItems ts tz = case chat of
  DirectChat c -> case chatDir of
    CIDirectRcv -> case content of
      CIRcvMsgContent mc
        | itemLive == Just True && not liveItems -> []
        | otherwise -> viewReceivedUpdatedMessage from context mc ts tz meta
      _ -> []
      where
        from = if itemEdited then ttyFromContactEdited c else ttyFromContact c
    CIDirectSnd -> case content of
      CISndMsgContent mc -> hideLive meta $ viewSentMessage to context mc ts tz meta
      _ -> []
      where
        to = if itemEdited then ttyToContactEdited' c else ttyToContact' c
    where
      context =
        maybe
          (maybe [] forwardedFrom itemForwarded)
          (directQuote chatDir)
          quotedItem
  GroupChat g scopeInfo -> case chatDir of
    CIGroupRcv m -> case content of
      CIRcvMsgContent mc
        | itemLive == Just True && not liveItems -> []
        | otherwise -> viewReceivedUpdatedMessage from context mc ts tz meta
      _ -> []
      where
        from = if itemEdited then ttyFromGroupEdited g scopeInfo m else ttyFromGroup g scopeInfo m
    CIGroupSnd -> case content of
      CISndMsgContent mc -> hideLive meta $ viewSentMessage to context mc ts tz meta
      _ -> []
      where
        to = if itemEdited then ttyToGroupEdited g scopeInfo else ttyToGroup g scopeInfo
    where
      context =
        maybe
          (maybe [] forwardedFrom itemForwarded)
          (groupQuote g)
          quotedItem
  _ -> []

hideLive :: CIMeta c d -> [StyledString] -> [StyledString]
hideLive CIMeta {itemLive = Just True} _ = []
hideLive _ s = s

viewItemNotChanged :: AChatItem -> [StyledString]
viewItemNotChanged (AChatItem _ msgDir _ _) = case msgDir of
  SMDSnd -> ["message didn't change"]
  SMDRcv -> []

viewChatItemsDeleted ::
  (forall c d. ChatInfo c -> ChatItem c d -> [StyledString] -> [StyledString]) ->
  [ChatItemDeletion] ->
  Bool ->
  Bool ->
  UTCTime ->
  TimeZone ->
  Bool ->
  [StyledString]
viewChatItemsDeleted unmuted deletions byUser timed ts tz testView = case deletions of
  [ChatItemDeletion (AChatItem _ _ chat deletedItem) toItem] ->
    unmuted chat deletedItem $ viewItemDelete chat deletedItem toItem byUser timed ts tz testView
  deletions' -> [sShow (length deletions') <> " messages deleted"]

viewGroupChatItemsDeleted :: GroupInfo -> [ChatItemId] -> Bool -> Maybe GroupMember -> [StyledString]
viewGroupChatItemsDeleted g ciIds byUser member_ = [ttyGroup' g <> ": " <> sShow (length ciIds) <> " messages deleted by " <> if byUser then "user" else "member" <> maybe "" (\m -> " " <> ttyMember m) member_]

viewItemDelete :: ChatInfo c -> ChatItem c d -> Maybe AChatItem -> Bool -> Bool -> CurrentTime -> TimeZone -> Bool -> [StyledString]
viewItemDelete chat ci@ChatItem {chatDir, meta, content = deletedContent} toItem byUser timed ts tz testView
  | timed = [plain ("timed message deleted: " <> T.unpack (ciContentToText deletedContent)) | testView]
  | byUser = [plain $ "message " <> T.unpack (fromMaybe "deleted" deletedText_)] -- deletedText_ Nothing should be impossible here
  | otherwise = case chat of
      DirectChat c -> case (chatDir, deletedContent) of
        (CIDirectRcv, CIRcvMsgContent mc) -> viewReceivedMessage (ttyFromContactDeleted c deletedText_) [] mc ts tz meta
        _ -> prohibited
      GroupChat g scopeInfo -> case ciMsgContent deletedContent of
        Just mc ->
          let m = chatItemMember g ci
           in viewReceivedMessage (ttyFromGroupDeleted g scopeInfo m deletedText_) [] mc ts tz meta
        _ -> prohibited
      _ -> prohibited
  where
    deletedText_ :: Maybe Text
    deletedText_ = case toItem of
      Nothing -> Just "deleted"
      Just (AChatItem _ _ _ ci') -> chatItemDeletedText ci' $ chatInfoMembership chat
    prohibited = [styled (colored Red) ("[unexpected message deletion, please report to developers]" :: String)]

viewItemReaction :: forall c d. Bool -> ChatInfo c -> CIReaction c d -> Bool -> CurrentTime -> TimeZone -> [StyledString]
viewItemReaction showReactions chat CIReaction {chatDir, chatItem = CChatItem md ChatItem {chatDir = itemDir, content}, sentAt, reaction} added ts tz =
  case (chat, chatDir) of
    (DirectChat c, CIDirectRcv) -> case ciMsgContent content of
      Just mc -> view from $ reactionMsg mc
      _ -> []
      where
        from = ttyFromContact c
        reactionMsg mc = quoteText mc $ if toMsgDirection md == MDSnd then ">>" else ">"
    (GroupChat g scopeInfo, CIGroupRcv m) -> case ciMsgContent content of
      Just mc -> view from $ reactionMsg mc
      _ -> []
      where
        from = ttyFromGroup g scopeInfo m
        reactionMsg mc = quoteText mc . ttyQuotedMember . Just $ sentByMember' g itemDir
    (LocalChat _, CILocalRcv) -> case ciMsgContent content of
      Just mc -> view from $ reactionMsg mc
      _ -> []
      where
        from = "* "
        reactionMsg mc = quoteText mc $ if toMsgDirection md == MDSnd then ">>" else ">"
    (_, CIDirectSnd) -> [sentText]
    (_, CIGroupSnd) -> [sentText]
    (_, CILocalSnd) -> [sentText]
    (CInfoInvalidJSON {}, _) -> []
  where
    view from msg
      | showReactions = viewReceivedReaction from msg reactionText ts tz sentAt
      | otherwise = []
    reactionText = plain $ (if added then "+ " else "- ") <> [emoji]
    emoji = case reaction of
      MREmoji (MREmojiChar c) -> c
      _ -> '?'
    sentText = plain $ (if added then "added " else "removed ") <> [emoji]

viewItemReactions :: ChatItem c d -> [StyledString]
viewItemReactions ChatItem {reactions} = ["      " <> viewReactions reactions | not (null reactions)]
  where
    viewReactions = mconcat . intersperse " " . map viewReaction
    viewReaction CIReactionCount {reaction = MRUnknown {}} = "?"
    viewReaction CIReactionCount {reaction = MREmoji (MREmojiChar emoji), userReacted, totalReacted} =
      plain [emoji, ' '] <> (if userReacted then styled Italic else plain) (show totalReacted)

viewReactionMembers :: [MemberReaction] -> [StyledString]
viewReactionMembers memberReactions = [sShow (length memberReactions) <> " member(s) reacted"]

directQuote :: forall d'. MsgDirectionI d' => CIDirection 'CTDirect d' -> CIQuote 'CTDirect -> [StyledString]
directQuote _ CIQuote {content = qmc, chatDir = quoteDir} =
  quoteText qmc $ if toMsgDirection (msgDirection @d') == quoteMsgDirection quoteDir then ">>" else ">"

groupQuote :: GroupInfo -> CIQuote 'CTGroup -> [StyledString]
groupQuote g CIQuote {content = qmc, chatDir = quoteDir} = quoteText qmc . ttyQuotedMember $ sentByMember g quoteDir

forwardedFrom :: CIForwardedFrom -> [StyledString]
forwardedFrom = \case
  CIFFUnknown -> ["-> forwarded"]
  CIFFContact c MDSnd _ _ -> ["<- you @" <> (plain . viewName) c]
  CIFFContact c MDRcv _ _ -> ["<- @" <> (plain . viewName) c]
  CIFFGroup g MDSnd _ _ -> ["<- you #" <> (plain . viewName) g]
  CIFFGroup g MDRcv _ _ -> ["<- #" <> (plain . viewName) g]

sentByMember :: GroupInfo -> CIQDirection 'CTGroup -> Maybe GroupMember
sentByMember GroupInfo {membership} = \case
  CIQGroupSnd -> Just membership
  CIQGroupRcv m -> m

sentByMember' :: GroupInfo -> CIDirection 'CTGroup d -> GroupMember
sentByMember' GroupInfo {membership} = \case
  CIGroupSnd -> membership
  CIGroupRcv m -> m

quoteText :: MsgContent -> StyledString -> [StyledString]
quoteText qmc sentBy = prependFirst (sentBy <> " ") $ msgPreview qmc

msgPreview :: MsgContent -> [StyledString]
msgPreview = msgPlain . preview . msgContentText
  where
    preview t
      | T.length t <= 120 = t
      | otherwise = T.take 120 t <> "..."

viewRcvIntegrityError :: StyledString -> MsgErrorType -> CurrentTime -> TimeZone -> CIMeta c 'MDRcv -> [StyledString]
viewRcvIntegrityError from msgErr ts tz meta = receivedWithTime_ ts tz from [] meta (viewMsgIntegrityError msgErr) False

viewMsgIntegrityError :: MsgErrorType -> [StyledString]
viewMsgIntegrityError err = [ttyError $ msgIntegrityError err]

viewInvalidConnReq :: [StyledString]
viewInvalidConnReq =
  [ "",
    "Connection link is invalid, possibly it was created in a previous version.",
    "Please ask your contact to check " <> highlight' "/version" <> " and update if needed.",
    plain updateStr
  ]

viewConnReqInvitation :: CreatedLinkInvitation -> [StyledString]
viewConnReqInvitation (CCLink cReq shortLink) =
  [ "pass this invitation link to your contact (via another channel): ",
    "",
    plain $ maybe cReqStr strEncode shortLink,
    "",
    "and ask them to connect: " <> highlight' "/c <invitation_link_above>"
  ]
    <>
      if isJust shortLink
        then
          [ "The invitation link for old clients:",
            plain cReqStr
          ]
        else []
  where
    cReqStr = strEncode $ simplexChatInvitation cReq

simplexChatInvitation :: ConnReqInvitation -> ConnReqInvitation
simplexChatInvitation (CRInvitationUri crData e2e) = CRInvitationUri crData {crScheme = simplexChat} e2e

viewContactNotFound :: ContactName -> Maybe (GroupInfo, GroupMember) -> [StyledString]
viewContactNotFound cName suspectedMember =
  ["no contact " <> ttyContact cName <> useMessageMember]
  where
    useMessageMember = case suspectedMember of
      Just (g, m) -> ", use " <> highlight ("@#" <> viewGroupName g <> " " <> viewMemberName m <> " <your message>")
      _ -> ""

viewChatCleared :: AChatInfo -> [StyledString]
viewChatCleared (AChatInfo _ chatInfo) = case chatInfo of
  DirectChat ct -> [ttyContact' ct <> ": all messages are removed locally ONLY"]
  GroupChat gi _scopeInfo -> [ttyGroup' gi <> ": all messages are removed locally ONLY"]
  LocalChat _ -> ["notes: all messages are removed"]
  ContactRequest _ -> []
  ContactConnection _ -> []
  CInfoInvalidJSON {} -> []

viewContactsList :: [Contact] -> [StyledString]
viewContactsList =
  let getLDN :: Contact -> ContactName
      getLDN Contact {localDisplayName} = localDisplayName
      ldn = T.toLower . getLDN
   in map (\ct -> ctIncognito ct <> ttyFullContact ct <> muted' ct <> alias ct) . sortOn ldn
  where
    muted' Contact {chatSettings, localDisplayName = ldn}
      | chatHasNtfs chatSettings = ""
      | otherwise = " (muted, you can " <> highlight ("/unmute @" <> ldn) <> ")"
    alias Contact {profile = LocalProfile {localAlias}}
      | localAlias == "" = ""
      | otherwise = " (alias: " <> plain localAlias <> ")"

viewUserContactLinkDeleted :: [StyledString]
viewUserContactLinkDeleted =
  [ "Your chat address is deleted - accepted contacts will remain connected.",
    "To create a new chat address use " <> highlight' "/ad"
  ]

viewForwardPlan :: Int -> [ChatItemId] -> Maybe ForwardConfirmation -> [StyledString]
viewForwardPlan count itemIds = maybe [forwardCount] $ \fc -> [confirmation fc, forwardCount]
  where
    confirmation = \case
      FCFilesNotAccepted fileIds -> plain $ "Files can be received: " <> intercalate ", " (map show fileIds)
      FCFilesInProgress cnt -> plain $ "Still receiving " <> show cnt <> " file(s)"
      FCFilesMissing cnt -> plain $ show cnt <> " file(s) are missing"
      FCFilesFailed cnt -> plain $ "Receiving " <> show cnt <> " file(s) failed"
    forwardCount
      | count == len = "all messages can be forwarded"
      | len == 0 = "nothing to forward"
      | otherwise = plain $ show len <> " message(s) out of " <> show count <> " can be forwarded"
    len = length itemIds

connReqContact_ :: StyledString -> CreatedLinkContact -> [StyledString]
connReqContact_ intro (CCLink cReq shortLink) =
  [ intro,
    "",
    plain $ maybe cReqStr strEncode shortLink,
    "",
    "Anybody can send you contact requests with: " <> highlight' "/c <contact_link_above>",
    "to show it again: " <> highlight' "/sa",
    "to share with your contacts: " <> highlight' "/profile_address on",
    "to delete it: " <> highlight' "/da" <> " (accepted contacts will remain connected)"
  ]
    <> ["The contact link for old clients: " <> plain cReqStr | isJust shortLink]
  where
    cReqStr = strEncode $ simplexChatContact cReq

simplexChatContact :: ConnReqContact -> ConnReqContact
simplexChatContact (CRContactUri crData) = CRContactUri crData {crScheme = simplexChat}

simplexChatContact' :: ConnLinkContact -> ConnLinkContact
simplexChatContact' = \case
  CLFull (CRContactUri crData) -> CLFull $ CRContactUri crData {crScheme = simplexChat}
  l@(CLShort _) -> l

-- TODO [short links] show all settings
viewAddressSettings :: AddressSettings -> [StyledString]
viewAddressSettings AddressSettings {businessAddress, autoAccept, autoReply} = case autoAccept of
  Just AutoAccept {acceptIncognito} ->
    ("auto_accept on" <> aaInfo)
      : maybe [] ((["auto reply:"] <>) . ttyMsgContent) autoReply
    where
      aaInfo
        | businessAddress = ", business"
        | acceptIncognito = ", incognito"
        | otherwise = ""
  _ -> ["auto_accept off"]

groupLink_ :: StyledString -> GroupInfo -> GroupLink -> [StyledString]
groupLink_ intro g GroupLink {connLinkContact = CCLink cReq shortLink, acceptMemberRole} =
  [ intro,
    "",
    plain $ maybe cReqStr strEncode shortLink,
    "",
    "Anybody can connect to you and join group as " <> showRole acceptMemberRole <> " with: " <> highlight' "/c <group_link_above>",
    "to show it again: " <> highlight ("/show link #" <> viewGroupName g),
    "to delete it: " <> highlight ("/delete link #" <> viewGroupName g) <> " (joined members will remain connected to you)"
  ]
    <> ["The group link for old clients: " <> plain cReqStr | isJust shortLink]
  where
    cReqStr = strEncode $ simplexChatContact cReq

viewGroupLinkDeleted :: GroupInfo -> [StyledString]
viewGroupLinkDeleted g =
  [ "Group link is deleted - joined members will remain connected.",
    "To create a new group link use " <> highlight ("/create link #" <> viewGroupName g)
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

viewStartedConnectionToContact :: Contact -> Maybe Profile -> Bool -> [StyledString]
viewStartedConnectionToContact ct incognitoProfile testView =
  case incognitoProfile of
    Just profile ->
      if testView
        then incognitoProfile' profile : message
        else message
      where
        message = [ttyContact' ct <> ": connection started incognito"]
    Nothing -> [ttyContact' ct <> ": connection started"]

viewStartedConnectionToGroup :: GroupInfo -> Maybe Profile -> Bool -> [StyledString]
viewStartedConnectionToGroup g incognitoProfile testView =
  case incognitoProfile of
    Just profile ->
      if testView
        then incognitoProfile' profile : message
        else message
      where
        message = [ttyGroup' g <> ": connection started incognito"]
    Nothing -> [ttyGroup' g <> ": connection started"]

viewAcceptingContactRequest :: Contact -> [StyledString]
viewAcceptingContactRequest ct
  | contactReady ct = [ttyFullContact ct <> ": accepting contact request, you can send messages to contact"]
  | otherwise = [ttyFullContact ct <> ": accepting contact request..."]

viewAcceptingBusinessRequest :: GroupInfo -> [StyledString]
viewAcceptingBusinessRequest g = [ttyFullGroup g <> ": accepting business address request..."]

viewReceivedContactRequest :: ContactName -> Profile -> [StyledString]
viewReceivedContactRequest c Profile {fullName, shortDescr} =
  [ ttyFullName c fullName shortDescr <> " wants to connect to you!",
    "to accept: " <> highlight ("/ac " <> viewName c),
    "to reject: " <> highlight ("/rc " <> viewName c) <> " (the sender will NOT be notified)"
  ]

viewGroupCreated :: GroupInfo -> Bool -> [StyledString]
viewGroupCreated g testView =
  case incognitoMembershipProfile g of
    Just localProfile
      | testView -> incognitoProfile' profile : message
      | otherwise -> message
      where
        profile = fromLocalProfile localProfile
        message =
          [ "group " <> ttyFullGroup g <> " is created, your incognito profile for this group is " <> incognitoProfile' profile,
            "to add members use " <> highlight ("/create link #" <> viewGroupName g)
          ]
    Nothing ->
      [ "group " <> ttyFullGroup g <> " is created",
        "to add members use " <> highlight ("/a " <> viewGroupName g <> " <name>") <> " or " <> highlight ("/create link #" <> viewGroupName g)
      ]

viewCannotResendInvitation :: GroupInfo -> ContactName -> [StyledString]
viewCannotResendInvitation g c =
  [ ttyContact c <> " is already invited to group " <> ttyGroup' g,
    "to re-send invitation: " <> highlight ("/rm " <> viewGroupName g <> " " <> c) <> ", " <> highlight ("/a " <> viewGroupName g <> " " <> viewName c)
  ]

viewDirectMessagesProhibited :: MsgDirection -> Contact -> [StyledString]
viewDirectMessagesProhibited MDSnd c = ["direct messages to indirect contact " <> ttyContact' c <> " are prohibited"]
viewDirectMessagesProhibited MDRcv c = ["received prohibited direct message from indirect contact " <> ttyContact' c <> " (discarded)"]

viewNetworkStatuses :: [ConnNetworkStatus] -> [StyledString]
viewNetworkStatuses = map viewStatuses . L.groupBy ((==) `on` netStatus) . sortOn netStatus
  where
    netStatus ConnNetworkStatus {networkStatus} = networkStatus
    viewStatuses ss@(s :| _) = plain $ show (L.length ss) <> " connections " <> netStatusStr (netStatus s)

viewUserJoinedGroup :: GroupInfo -> [StyledString]
viewUserJoinedGroup g@GroupInfo {membership} =
  case incognitoMembershipProfile g of
    Just mp -> [ttyGroup' g <> ": you joined the group incognito as " <> incognitoProfile' (fromLocalProfile mp) <> pendingApproval_]
    Nothing -> [ttyGroup' g <> ": you joined the group" <> pendingApproval_]
  where
    pendingApproval_ = case memberStatus membership of
      GSMemPendingApproval -> ", pending approval"
      GSMemPendingReview -> ", connecting to group moderators for admission to group"
      _ -> ""

viewJoinedGroupMember :: GroupInfo -> GroupMember -> [StyledString]
viewJoinedGroupMember g@GroupInfo {groupId} m@GroupMember {groupMemberId, memberStatus} = case memberStatus of
  GSMemPendingApproval ->
    [ (ttyGroup' g <> ": " <> ttyMember m <> " connected and pending approval, ")
      <> ("use " <> highlight ("/_accept member #" <> show groupId <> " " <> show groupMemberId <> " <role>") <> " to accept member")
    ]
  GSMemPendingReview -> [ttyGroup' g <> ": " <> ttyMember m <> " connected and pending review"]
  _ -> [ttyGroup' g <> ": " <> ttyMember m <> " joined the group"]

viewMemberAccepted :: GroupInfo -> GroupMember -> [StyledString]
viewMemberAccepted g m@GroupMember {memberStatus} = case memberStatus of
  GSMemPendingReview -> [ttyGroup' g <> ": " <> ttyMember m <> " accepted and pending review (will introduce moderators)"]
  _ -> [ttyGroup' g <> ": " <> ttyMember m <> " accepted"]

viewMemberAcceptedByOther :: GroupInfo -> GroupMember -> GroupMember -> [StyledString]
viewMemberAcceptedByOther g acceptingMember m@GroupMember {memberCategory, memberStatus} = case memberCategory of
  GCUserMember -> case memberStatus of
    GSMemPendingReview -> [ttyGroup' g <> ": " <> ttyMember acceptingMember <> " accepted you to the group, pending review"]
    _ -> [ttyGroup' g <> ": " <> ttyMember acceptingMember <> " accepted you to the group [warning - unexpected]"]
  GCInviteeMember -> [ttyGroup' g <> ": " <> ttyMember acceptingMember <> " accepted " <> ttyMember m <> " to the group (will introduce remaining members)"]
  _ -> [ttyGroup' g <> ": " <> ttyMember acceptingMember <> " accepted " <> ttyMember m <> " to the group"]

viewJoinedGroupMemberConnecting :: GroupInfo -> GroupMember -> GroupMember -> [StyledString]
viewJoinedGroupMemberConnecting g@GroupInfo {groupId} host m@GroupMember {groupMemberId, memberStatus} = case memberStatus of
  GSMemPendingReview ->
    [ (ttyGroup' g <> ": " <> ttyMember host <> " added " <> ttyFullMember m <> " to the group (connecting and pending review...), ")
      <> ("use " <> highlight ("/_accept member #" <> show groupId <> " " <> show groupMemberId <> " <role>") <> " to accept member")
    ]
  _ -> [ttyGroup' g <> ": " <> ttyMember host <> " added " <> ttyFullMember m <> " to the group (connecting...)"]

viewConnectedToGroupMember :: GroupInfo -> GroupMember -> [StyledString]
viewConnectedToGroupMember g@GroupInfo {groupId} m@GroupMember {groupMemberId, memberStatus} = case memberStatus of
  GSMemPendingReview ->
    [ (ttyGroup' g <> ": " <> connectedMember m <> " is connected and pending review, ")
      <> ("use " <> highlight ("/_accept member #" <> show groupId <> " " <> show groupMemberId <> " <role>") <> " to accept member")
    ]
  _ -> [ttyGroup' g <> ": " <> connectedMember m <> " is connected"]

viewReceivedGroupInvitation :: GroupInfo -> Contact -> GroupMemberRole -> [StyledString]
viewReceivedGroupInvitation g c role =
  ttyFullGroup g <> ": " <> ttyContact' c <> " invites you to join the group as " <> plain (strEncode role)
    : case incognitoMembershipProfile g of
      Just mp -> ["use " <> highlight ("/j " <> viewGroupName g) <> " to join incognito as " <> incognitoProfile' (fromLocalProfile mp)]
      Nothing -> ["use " <> highlight ("/j " <> viewGroupName g) <> " to accept"]

groupPreserved :: GroupInfo -> [StyledString]
groupPreserved g = ["use " <> highlight ("/d #" <> viewGroupName g) <> " to delete the group"]

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

viewMemberRoleUserChanged :: GroupInfo -> [GroupMember] -> GroupMemberRole -> [StyledString]
viewMemberRoleUserChanged g members r = case members of
  [m] -> [ttyGroup' g <> ": you changed the role of " <> ttyMember m <> " to " <> showRole r]
  mems' -> [ttyGroup' g <> ": you changed the role of " <> sShow (length mems') <> " members to " <> showRole r]

viewMemberBlockedForAll :: GroupInfo -> GroupMember -> GroupMember -> Bool -> [StyledString]
viewMemberBlockedForAll g by m blocked =
  [ttyGroup' g <> ": " <> ttyMember by <> " " <> (if blocked then "blocked" else "unblocked") <> " " <> ttyMember m]

viewMembersBlockedForAllUser :: GroupInfo -> [GroupMember] -> Bool -> [StyledString]
viewMembersBlockedForAllUser g members blocked = case members of
  [m] -> [ttyGroup' g <> ": you " <> (if blocked then "blocked" else "unblocked") <> " " <> ttyMember m]
  mems' -> [ttyGroup' g <> ": you " <> (if blocked then "blocked" else "unblocked") <> " " <> sShow (length mems') <> " members"]

showRole :: GroupMemberRole -> StyledString
showRole = plain . strEncode

viewGroupMembers :: Group -> [StyledString]
viewGroupMembers (Group GroupInfo {membership} members) = map groupMember . filter (not . removedOrLeft) $ membership : members
  where
    removedOrLeft m = let s = memberStatus m in s == GSMemRejected || s == GSMemRemoved || s == GSMemLeft
    groupMember m = memIncognito m <> ttyFullMember m <> ": " <> plain (intercalate ", " $ [role m] <> category m <> status m <> muted m)
    role :: GroupMember -> String
    role GroupMember {memberRole} = B.unpack $ strEncode memberRole
    category m = case memberCategory m of
      GCUserMember -> ["you"]
      GCInviteeMember -> ["invited"]
      GCHostMember -> ["host"]
      _ -> []
    status m = case memberStatus m of
      GSMemRejected -> ["rejected"]
      GSMemRemoved -> ["removed"]
      GSMemLeft -> ["left"]
      GSMemUnknown -> ["status unknown"]
      GSMemInvited -> ["not yet joined"]
      GSMemConnected -> ["connected"]
      GSMemComplete -> ["connected"]
      GSMemCreator -> ["created group"]
      _ -> []
    muted m
      | blockedByAdmin m = ["blocked by admin"]
      | not (showMessages $ memberSettings m) = ["blocked"]
      | otherwise = []

viewMemberSupportChats :: GroupInfo -> [GroupMember] -> [StyledString]
viewMemberSupportChats GroupInfo {membership} ms = support <> map groupMember ms
  where
    support = case supportChat membership of
      Just sc -> ["support: " <> chatStats sc]
      Nothing -> []
    groupMember m@GroupMember {supportChat} = case supportChat of
      Just sc -> memIncognito m <> ttyFullMember m <> (" (id " <> sShow (groupMemberId' m) <> "): ") <> chatStats sc
      Nothing -> ""
    chatStats GroupSupportChat {unread, memberAttention, mentions} =
      "unread: " <> sShow unread <> ", require attention: " <> sShow memberAttention <> ", mentions: " <> sShow mentions

viewContactConnected :: Contact -> Maybe Profile -> Bool -> [StyledString]
viewContactConnected ct userIncognitoProfile testView =
  case userIncognitoProfile of
    Just profile ->
      if testView
        then incognitoProfile' profile : message
        else message
      where
        message =
          [ ttyFullContact ct <> ": contact is connected, your incognito profile for this contact is " <> incognitoProfile' profile,
            "use " <> highlight ("/i " <> viewContactName ct) <> " to print out this incognito profile again"
          ]
    Nothing ->
      [ttyFullContact ct <> ": contact is connected"]

viewGroupsList :: [GroupInfoSummary] -> [StyledString]
viewGroupsList [] = ["you have no groups!", "to create: " <> highlight' "/g <name>"]
viewGroupsList gs = map groupSS $ sortOn ldn_ gs
  where
    ldn_ :: GroupInfoSummary -> Text
    ldn_ (GIS GroupInfo {localDisplayName} _) = T.toLower localDisplayName
    groupSS (GIS g@GroupInfo {membership, chatSettings = ChatSettings {enableNtfs}} GroupSummary {currentMembers}) =
      case memberStatus membership of
        GSMemInvited -> groupInvitation' g
        s -> membershipIncognito g <> ttyFullGroup g <> viewMemberStatus s <> alias g
      where
        viewMemberStatus = \case
          GSMemRejected -> delete "you are rejected"
          GSMemRemoved -> delete "you are removed"
          GSMemLeft -> delete "you left"
          GSMemGroupDeleted -> delete "group deleted"
          _ -> " (" <> memberCount <> viewNtf <> ")"
            where
              viewNtf = case enableNtfs of
                MFAll -> ""
                MFNone -> ", muted, " <> unmute
                MFMentions -> ", mentions only, " <> unmute
              unmute = "you can " <> highlight ("/unmute #" <> viewGroupName g)
        delete reason = " (" <> reason <> ", delete local copy: " <> highlight ("/d #" <> viewGroupName g) <> ")"
        memberCount = sShow currentMembers <> " member" <> if currentMembers == 1 then "" else "s"
        alias GroupInfo {localAlias}
          | localAlias == "" = ""
          | otherwise = " (alias: " <> plain localAlias <> ")"

viewSentGroupInvitation :: GroupInfo -> Contact -> [StyledString]
viewSentGroupInvitation g c = case contactConn c of
  Just Connection {viaGroupLink}
    | viaGroupLink -> [ttyContact' c <> " invited to group " <> ttyGroup' g <> " via your group link"]
    | otherwise -> ["invitation to join the group " <> ttyGroup' g <> " sent to " <> ttyContact' c]
  Nothing -> []

groupInvitation' :: GroupInfo -> StyledString
groupInvitation' g@GroupInfo {localDisplayName = ldn, groupProfile = GroupProfile {fullName, shortDescr}} =
  highlight ("#" <> viewName ldn)
    <> optFullName ldn fullName shortDescr
    <> " - you are invited ("
    <> highlight ("/j " <> viewName ldn)
    <> joinText
    <> highlight ("/d #" <> viewName ldn)
    <> " to delete invitation)"
  where
    joinText = case incognitoMembershipProfile g of
      Just mp -> " to join as " <> incognitoProfile' (fromLocalProfile mp) <> ", "
      Nothing -> " to join, "

groupInvitationSub :: ShortGroupInfo -> StyledString
groupInvitationSub ShortGroupInfo {groupName = ldn} =
  highlight ("#" <> viewName ldn)
    <> " - you are invited ("
    <> highlight ("/j " <> viewName ldn)
    <> " to join, "
    <> highlight ("/d #" <> viewName ldn)
    <> " to delete invitation)"

viewContactsMerged :: Contact -> Contact -> Contact -> [StyledString]
viewContactsMerged c1 c2 ct' =
  [ "contact " <> ttyContact' c2 <> " is merged into " <> ttyContact' c1,
    "use " <> ttyToContact' ct' <> highlight' "<message>" <> " to send messages"
  ]

viewContactAndMemberAssociated :: Contact -> GroupInfo -> GroupMember -> Contact -> [StyledString]
viewContactAndMemberAssociated ct g m ct' =
  [ "contact and member are merged: " <> ttyContact' ct <> ", " <> ttyGroup' g <> " " <> ttyMember m,
    "use " <> ttyToContact' ct' <> highlight' "<message>" <> " to send messages"
  ]

viewUserProfile :: Profile -> [StyledString]
viewUserProfile Profile {displayName, fullName, shortDescr} =
  [ "user profile: " <> ttyFullName displayName fullName shortDescr,
    "use " <> highlight' "/p <name> [<bio>]" <> " to change it",
    "(the updated profile will be sent to all your contacts)"
  ]

viewUserPrivacy :: User -> User -> [StyledString]
viewUserPrivacy User {userId} User {userId = userId', localDisplayName = n', showNtfs, viewPwdHash} =
  [ plain $ (if userId == userId' then "current " else "") <> "user " <> viewName n' <> ":",
    "messages are " <> if showNtfs then "shown" else "hidden (use /tail to view)",
    "profile is " <> if isJust viewPwdHash then "hidden" else "visible"
  ]

viewUserServers :: UserOperatorServers -> [StyledString]
viewUserServers (UserOperatorServers _ [] []) = []
viewUserServers UserOperatorServers {operator, smpServers, xftpServers} =
  [plain $ maybe "Your servers" shortViewOperator operator]
    <> viewServers SPSMP smpServers
    <> viewServers SPXFTP xftpServers
  where
    viewServers :: (ProtocolTypeI p, UserProtocol p) => SProtocolType p -> [UserServer p] -> [StyledString]
    viewServers _ [] = []
    viewServers p srvs
      | maybe True (\ServerOperator {enabled} -> enabled) operator =
          ["  " <> protocolName p <> " servers" <> maybe "" ((" " <>) . viewRoles) operator]
            <> map (plain . ("    " <>) . viewServer) srvs
      | otherwise = []
      where
        viewServer UserServer {server, preset, tested, enabled} = safeDecodeUtf8 (strEncode server) <> serverInfo
          where
            serverInfo = if null serverInfo_ then "" else parens $ T.intercalate ", " serverInfo_
            serverInfo_ = ["preset" | preset] <> testedInfo <> ["disabled" | not enabled]
            testedInfo = maybe [] (\t -> ["test: " <> if t then "passed" else "failed"]) tested
        viewRoles op@ServerOperator {enabled}
          | not enabled = "disabled"
          | storage rs && proxy rs = "enabled"
          | storage rs = "enabled storage"
          | proxy rs = "enabled proxy"
          | otherwise = "disabled (servers known)"
          where
            rs = operatorRoles p op

serversUserHelp :: [StyledString]
serversUserHelp =
  [ "",
    "use " <> highlight' "/smp test <srv>" <> " to test SMP server connection",
    "use " <> highlight' "/smp <srv1[,srv2,...]>" <> " to configure SMP servers",
    "or the same commands starting from /xftp for XFTP servers",
    "chat options " <> highlight' "-s" <> " (" <> highlight' "--server" <> ") and " <> highlight' "--xftp-servers" <> " have precedence over preset servers for new user profiles"
  ]

protocolName :: ProtocolTypeI p => SProtocolType p -> StyledString
protocolName = plain . map toUpper . T.unpack . decodeLatin1 . strEncode

viewServerTestResult :: AProtoServerWithAuth -> Maybe ProtocolTestFailure -> [StyledString]
viewServerTestResult (AProtoServerWithAuth p _) = \case
  Just ProtocolTestFailure {testStep, testError} ->
    result
      <> [pName <> " server requires authorization to create queues, check password" | testStep == TSCreateQueue && (case testError of SMP _ SMP.AUTH -> True; _ -> False)]
      <> [pName <> " server requires authorization to upload files, check password" | testStep == TSCreateFile && (case testError of XFTP _ XFTP.AUTH -> True; _ -> False)]
      <> ["Possibly, certificate fingerprint in " <> pName <> " server address is incorrect" | testStep == TSConnect && brokerErr]
    where
      result = [pName <> " server test failed at " <> plain (drop 2 $ show testStep) <> ", error: " <> sShow testError]
      brokerErr = case testError of
        BROKER _ NETWORK -> True
        _ -> False
  _ -> [pName <> " server test passed"]
  where
    pName = protocolName p

viewServerOperators :: [ServerOperator] -> Maybe UsageConditionsAction -> [StyledString]
viewServerOperators ops ca = map (plain . viewOperator) ops <> maybe [] viewConditionsAction ca

viewOperator :: ServerOperator' s -> Text
viewOperator op@ServerOperator {tradeName, legalName, serverDomains, conditionsAcceptance} =
  viewOpIdTag op
    <> tradeName
    <> maybe "" parens legalName
    <> (", domains: " <> T.intercalate ", " serverDomains)
    <> (", servers: " <> viewOpEnabled op)
    <> (", conditions: " <> viewOpConditions conditionsAcceptance)

shortViewOperator :: ServerOperator -> Text
shortViewOperator ServerOperator {operatorId = DBEntityId opId, tradeName, enabled} =
  tshow opId <> ". " <> tradeName <> parens (if enabled then "enabled" else "disabled")

viewOpIdTag :: ServerOperator' s -> Text
viewOpIdTag ServerOperator {operatorId, operatorTag} = case operatorId of
  DBEntityId i -> tshow i <> tag
  DBNewEntity -> tag
  where
    tag = maybe "" (parens . textEncode) operatorTag <> ". "

viewOpConditions :: ConditionsAcceptance -> Text
viewOpConditions = \case
  CAAccepted ts _ -> viewCond "accepted" ts
  CARequired ts -> viewCond "required" ts
  where
    viewCond w ts = w <> maybe "" (parens . tshow) ts

viewOpEnabled :: ServerOperator' s -> Text
viewOpEnabled ServerOperator {enabled, smpRoles, xftpRoles}
  | not enabled = "disabled"
  | no smpRoles && no xftpRoles = "disabled (servers known)"
  | both smpRoles && both xftpRoles = "enabled"
  | otherwise = "SMP " <> viewRoles smpRoles <> ", XFTP " <> viewRoles xftpRoles
  where
    no rs = not $ storage rs || proxy rs
    both rs = storage rs && proxy rs
    viewRoles rs
      | both rs = "enabled"
      | storage rs = "enabled storage"
      | proxy rs = "enabled proxy"
      | otherwise = "disabled (servers known)"

viewConditionsAction :: UsageConditionsAction -> [StyledString]
viewConditionsAction = \case
  UCAReview {operators, deadline, showNotice} | showNotice -> case deadline of
    Just ts -> [plain $ "The new conditions will be accepted for " <> ops <> " at " <> tshow ts]
    Nothing -> [plain $ "The new conditions have to be accepted for " <> ops]
    where
      ops = T.intercalate ", " $ map legalName_ operators
      legalName_ ServerOperator {tradeName, legalName} = fromMaybe tradeName legalName
  _ -> []

viewUsageConditions :: UsageConditions -> Maybe UsageConditions -> [StyledString]
viewUsageConditions current accepted_ =
  [plain $ "Current conditions: " <> viewConds current <> maybe "" (\ac -> ", accepted conditions: " <> viewConds ac) accepted_]
  where
    viewConds UsageConditions {conditionsId, conditionsCommit, notifiedAt} =
      tshow conditionsId <> maybe "" (const " (notified)") notifiedAt <> ". " <> conditionsCommit

viewChatItemTTL :: Maybe Int64 -> [StyledString]
viewChatItemTTL = \case
  Nothing -> ["old messages are set to delete according to default user config"]
  Just ttl
    | ttl == 0 -> ["old messages are not being deleted"]
    | ttl == 86400 -> deletedAfter "one day"
    | ttl == 7 * 86400 -> deletedAfter "one week"
    | ttl == 30 * 86400 -> deletedAfter "one month"
    | ttl == 365 * 86400 -> deletedAfter "one year"
    | otherwise -> deletedAfter $ sShow ttl <> " second(s)"
  where
    deletedAfter ttlStr = ["old messages are set to be deleted after: " <> ttlStr]

viewNetworkConfig :: NetworkConfig -> [StyledString]
viewNetworkConfig NetworkConfig {socksProxy, socksMode, tcpTimeout, smpProxyMode, smpProxyFallback} =
  [ plain $ maybe "direct network connection" ((\sp -> "using SOCKS5 proxy " <> sp <> if socksMode == SMOnion then " for onion servers ONLY." else " for ALL servers.") . show) socksProxy,
    "TCP timeout: " <> sShow tcpTimeout,
    plain $ smpProxyModeStr smpProxyMode smpProxyFallback,
    "use " <> highlight' "/network socks=<on/off/[ipv4]:port>[ socks-mode=always/onion][ smp-proxy=always/unknown/unprotected/never][ smp-proxy-fallback=no/protected/yes][ timeout=<seconds>]" <> " to change settings"
  ]

smpProxyModeStr :: SMPProxyMode -> SMPProxyFallback -> String
smpProxyModeStr SPMNever _ = "private message routing disabled."
smpProxyModeStr mode fallback = T.unpack $ safeDecodeUtf8 $ "private message routing mode: " <> strEncode mode <> ", fallback: " <> strEncode fallback

viewContactInfo :: Contact -> Maybe ConnectionStats -> Maybe Profile -> [StyledString]
viewContactInfo ct@Contact {contactId, profile = LocalProfile {localAlias, contactLink}, activeConn, uiThemes, customData} stats incognitoProfile =
  ["contact ID: " <> sShow contactId]
    <> maybe [] viewConnectionStats stats
    <> maybe [] (\l -> ["contact address: " <> (plain . strEncode) (simplexChatContact' l)]) contactLink
    <> maybe
      ["you've shared main profile with this contact"]
      (\p -> ["you've shared incognito profile with this contact: " <> incognitoProfile' p])
      incognitoProfile
    <> ["alias: " <> plain localAlias | localAlias /= ""]
    <> [viewConnectionVerified (contactSecurityCode ct)]
    <> ["quantum resistant end-to-end encryption" | contactPQEnabled ct == CR.PQEncOn]
    <> maybe [] (\ac -> [viewPeerChatVRange (peerChatVRange ac)]) activeConn
    <> viewUITheme uiThemes
    <> viewCustomData customData

viewGroupInfo :: GroupInfo -> GroupSummary -> [StyledString]
viewGroupInfo GroupInfo {groupId, uiThemes, customData} s =
  [ "group ID: " <> sShow groupId,
    "current members: " <> sShow (currentMembers s)
  ]
    <> viewUITheme uiThemes
    <> viewCustomData customData

viewUITheme :: Maybe UIThemeEntityOverrides -> [StyledString]
viewUITheme = maybe [] (\uiThemes -> ["UI themes: " <> viewJSON uiThemes])

viewCustomData :: Maybe CustomData -> [StyledString]
viewCustomData = maybe [] (\(CustomData v) -> ["custom data: " <> viewJSON (J.Object v)])

viewGroupMemberInfo :: GroupInfo -> GroupMember -> Maybe ConnectionStats -> [StyledString]
viewGroupMemberInfo GroupInfo {groupId} m@GroupMember {groupMemberId, memberProfile = LocalProfile {localAlias, contactLink}, activeConn} stats =
  [ "group ID: " <> sShow groupId,
    "member ID: " <> sShow groupMemberId
  ]
    <> maybe ["member not connected"] viewConnectionStats stats
    <> maybe [] (\l -> ["contact address: " <> (plain . strEncode) (simplexChatContact' l)]) contactLink
    <> ["alias: " <> plain localAlias | localAlias /= ""]
    <> [viewConnectionVerified (memberSecurityCode m) | isJust stats]
    <> maybe [] (\ac -> [viewPeerChatVRange (peerChatVRange ac)]) activeConn

viewConnectionVerified :: Maybe SecurityCode -> StyledString
viewConnectionVerified (Just _) = "connection verified" -- TODO show verification time?
viewConnectionVerified _ = "connection not verified, use " <> highlight' "/code" <> " command to see security code"

viewPeerChatVRange :: VersionRangeChat -> StyledString
viewPeerChatVRange (VersionRange minVer maxVer) = "peer chat protocol version range: (" <> sShow minVer <> ", " <> sShow maxVer <> ")"

viewConnectionStats :: ConnectionStats -> [StyledString]
viewConnectionStats ConnectionStats {rcvQueuesInfo, sndQueuesInfo} =
  ["receiving messages via: " <> viewRcvQueuesInfo rcvQueuesInfo | not $ null rcvQueuesInfo]
    <> ["sending messages via: " <> viewSndQueuesInfo sndQueuesInfo | not $ null sndQueuesInfo]

viewRcvQueuesInfo :: [RcvQueueInfo] -> StyledString
viewRcvQueuesInfo = plain . intercalate ", " . map showQueueInfo
  where
    showQueueInfo RcvQueueInfo {rcvServer, rcvSwitchStatus, canAbortSwitch} =
      let switchCanBeAborted = if canAbortSwitch then ", can be aborted" else ""
       in showSMPServer rcvServer
            <> maybe "" (\s -> " (" <> showSwitchStatus s <> switchCanBeAborted <> ")") rcvSwitchStatus
    showSwitchStatus = \case
      RSSwitchStarted -> "switch started"
      RSSendingQADD -> "switch started"
      RSSendingQUSE -> "switch confirmed"
      RSReceivedMessage -> "switch secured"

viewSndQueuesInfo :: [SndQueueInfo] -> StyledString
viewSndQueuesInfo = plain . intercalate ", " . map showQueueInfo
  where
    showQueueInfo SndQueueInfo {sndServer, sndSwitchStatus} =
      showSMPServer sndServer
        <> maybe "" (\s -> " (" <> showSwitchStatus s <> ")") sndSwitchStatus
    showSwitchStatus = \case
      SSSendingQKEY -> "switch started"
      SSSendingQTEST -> "switch secured"

viewContactSwitch :: Contact -> SwitchProgress -> [StyledString]
viewContactSwitch _ (SwitchProgress _ SPConfirmed _) = []
viewContactSwitch _ (SwitchProgress _ SPSecured _) = []
viewContactSwitch ct (SwitchProgress qd phase _) = case qd of
  QDRcv -> [ttyContact' ct <> ": you " <> viewSwitchPhase phase]
  QDSnd -> [ttyContact' ct <> " " <> viewSwitchPhase phase <> " for you"]

viewGroupMemberSwitch :: GroupInfo -> GroupMember -> SwitchProgress -> [StyledString]
viewGroupMemberSwitch _ _ (SwitchProgress _ SPConfirmed _) = []
viewGroupMemberSwitch _ _ (SwitchProgress _ SPSecured _) = []
viewGroupMemberSwitch g m (SwitchProgress qd phase _) = case qd of
  QDRcv -> [ttyGroup' g <> ": you " <> viewSwitchPhase phase <> " for " <> ttyMember m]
  QDSnd -> [ttyGroup' g <> ": " <> ttyMember m <> " " <> viewSwitchPhase phase <> " for you"]

viewContactRatchetSync :: Contact -> RatchetSyncProgress -> [StyledString]
viewContactRatchetSync ct RatchetSyncProgress {ratchetSyncStatus = rss} =
  [ttyContact' ct <> ": " <> (plain . ratchetSyncStatusToText) rss]
    <> help
  where
    help = ["use " <> highlight ("/sync " <> viewContactName ct) <> " to synchronize" | rss `elem` [RSAllowed, RSRequired]]

viewGroupMemberRatchetSync :: GroupInfo -> GroupMember -> RatchetSyncProgress -> [StyledString]
viewGroupMemberRatchetSync g m RatchetSyncProgress {ratchetSyncStatus = rss} =
  [ttyGroup' g <> " " <> ttyMember m <> ": " <> (plain . ratchetSyncStatusToText) rss]
    <> help
  where
    help = ["use " <> highlight ("/sync #" <> viewGroupName g <> " " <> viewMemberName m) <> " to synchronize" | rss `elem` [RSAllowed, RSRequired]]

viewContactVerificationReset :: Contact -> [StyledString]
viewContactVerificationReset ct =
  [ttyContact' ct <> ": security code changed"]

viewGroupMemberVerificationReset :: GroupInfo -> GroupMember -> [StyledString]
viewGroupMemberVerificationReset g m =
  [ttyGroup' g <> " " <> ttyMember m <> ": security code changed"]

viewContactCode :: Contact -> Text -> Bool -> [StyledString]
viewContactCode ct = viewSecurityCode (ttyContact' ct) ("/verify " <> viewContactName ct <> " <code from your contact>")

viewGroupMemberCode :: GroupInfo -> GroupMember -> Text -> Bool -> [StyledString]
viewGroupMemberCode g m = viewSecurityCode (ttyGroup' g <> " " <> ttyMember m) ("/verify #" <> viewGroupName g <> " " <> viewMemberName m <> " <code from your contact>")

viewSecurityCode :: StyledString -> Text -> Text -> Bool -> [StyledString]
viewSecurityCode name cmd code testView
  | testView = [plain code]
  | otherwise = [name <> " security code:", plain code, "pass this code to your contact and use " <> highlight cmd <> " to verify"]

viewSwitchPhase :: SwitchPhase -> StyledString
viewSwitchPhase = \case
  SPStarted -> "started changing address"
  SPConfirmed -> "confirmed changing address"
  SPSecured -> "secured new address"
  SPCompleted -> "changed address"

viewUserProfileUpdated :: Profile -> Profile -> UserProfileUpdateSummary -> [StyledString]
viewUserProfileUpdated Profile {displayName = n, fullName, shortDescr, image, contactLink, preferences} Profile {displayName = n', fullName = fullName', shortDescr = shortDescr', image = image', contactLink = contactLink', preferences = prefs'} summary =
  profileUpdated <> viewPrefsUpdated preferences prefs'
  where
    UserProfileUpdateSummary {updateSuccesses = s, updateFailures = f} = summary
    profileUpdated
      | n == n' && fullName == fullName' && shortDescr == shortDescr' && image == image' && contactLink == contactLink' = []
      | n == n' && fullName == fullName' && shortDescr == shortDescr' && image == image' = [if isNothing contactLink' then "contact address removed" else "new contact address set"]
      | n == n' && fullName == fullName' && shortDescr == shortDescr' = [if isNothing image' then "profile image removed" else "profile image updated"]
      | n == n' && fullName == fullName' = ["user bio " <> (if maybe True T.null shortDescr' then "removed" else "changed to " <> maybe "" plain shortDescr') <> notified]
      | n == n' = ["user full name " <> (if T.null fullName' || fullName' == n' then "removed" else "changed to " <> plain fullName') <> notified]
      | otherwise = ["user profile is changed to " <> ttyFullName n' fullName' shortDescr' <> notified]
    notified = " (your " <> sShow s <> " contacts are notified" <> failures <> ")"
    failures
      | f > 0 = ", " <> sShow f <> " failures"
      | otherwise = ""

viewUserProfileImage :: Profile -> [StyledString]
viewUserProfileImage Profile {image} = case image of
  Just (ImageData img) -> ["Profile image:", plain img]
  _ -> ["No profile image"]

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
  | otherwise = Just . plain $ chatFeatureNameText' f <> ": " <> prefEnabledToText (chatFeature f) enabled (prefParam userPref') <> " (you allow: " <> countactUserPrefText userPreference <> ", contact allows: " <> preferenceText contactPreference <> ")"
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
      | otherwise = Just . plain $ chatFeatureNameText' f <> " allowed: " <> preferenceText (pref ps')
      where
        pref pss = getPreference f pss

countactUserPrefText :: FeatureI f => ContactUserPref (FeaturePreference f) -> Text
countactUserPrefText cup = case cup of
  CUPUser p -> "default (" <> preferenceText p <> ")"
  CUPContact p -> preferenceText p

viewGroupUpdated :: GroupInfo -> GroupInfo -> Maybe GroupMember -> [StyledString]
viewGroupUpdated
  GroupInfo {localDisplayName = n, groupProfile = GroupProfile {fullName, shortDescr, description, image, groupPreferences = gps, memberAdmission = ma}}
  g'@GroupInfo {localDisplayName = n', groupProfile = GroupProfile {fullName = fullName', shortDescr = shortDescr', description = description', image = image', groupPreferences = gps', memberAdmission = ma'}}
  m = do
    let update = groupProfileUpdated <> groupPrefsUpdated <> memberAdmissionUpdated
    if null update
      then []
      else memberUpdated <> update
    where
      memberUpdated = maybe [] (\m' -> [ttyMember m' <> " updated group " <> ttyGroup n <> ":"]) m
      groupProfileUpdated =
        ["changed to " <> ttyFullGroup g' | n /= n']
          <> ["full name " <> if T.null fullName' || fullName' == n' then "removed" else "changed to: " <> plain fullName' | n == n' && fullName /= fullName']
          <> ["description " <> if maybe True T.null shortDescr' then "removed" else "changed to: " <> maybe "" plain shortDescr' | n == n' && fullName == fullName' && shortDescr /= shortDescr']
          <> ["profile image " <> maybe "removed" (const "updated") image' | image /= image']
          <> (if description == description' then [] else maybe ["welcome message removed"] ((bold' "welcome message changed to:" :) . map plain . T.lines) description')
      groupPrefsUpdated
        | null prefs = []
        | otherwise = bold' "updated group preferences:" : prefs
        where
          prefs = mapMaybe viewPref allGroupFeatures
          viewPref (AGF f)
            | pref gps == pref gps' = Nothing
            | otherwise = Just . plain $ groupPreferenceText (pref gps')
            where
              pref = getGroupPreference f . mergeGroupPreferences
      memberAdmissionUpdated
        | ma == ma' = []
        | otherwise = ["changed member admission rules"]

viewGroupProfile :: GroupInfo -> [StyledString]
viewGroupProfile g@GroupInfo {groupProfile = GroupProfile {shortDescr, description, image, groupPreferences = gps}} =
  [ttyFullGroup g]
    <> maybe [] (\sd -> ["description: " <> plain sd]) shortDescr
    <> maybe [] (const ["has profile image"]) image
    <> maybe [] ((bold' "welcome message:" :) . map plain . T.lines) description
    <> (bold' "group preferences:" : map viewPref allGroupFeatures)
  where
    viewPref (AGF f) = plain $ groupPreferenceText (pref gps)
      where
        pref = getGroupPreference f . mergeGroupPreferences

viewGroupDescription :: GroupInfo -> [StyledString]
viewGroupDescription GroupInfo {groupProfile = GroupProfile {description}} =
  maybe ["No welcome message!"] ((bold' "Welcome message:" :) . map plain . T.lines) description

bold' :: String -> StyledString
bold' = styled Bold

viewContactAliasUpdated :: Contact -> [StyledString]
viewContactAliasUpdated ct@Contact {profile = LocalProfile {localAlias}}
  | localAlias == "" = ["contact " <> ttyContact' ct <> " alias removed"]
  | otherwise = ["contact " <> ttyContact' ct <> " alias updated: " <> plain localAlias]

viewGroupAliasUpdated :: GroupInfo -> [StyledString]
viewGroupAliasUpdated g@GroupInfo {localAlias}
  | localAlias == "" = ["group " <> ttyGroup' g <> " alias removed"]
  | otherwise = ["group " <> ttyGroup' g <> " alias updated: " <> plain localAlias]

viewConnectionAliasUpdated :: PendingContactConnection -> [StyledString]
viewConnectionAliasUpdated PendingContactConnection {pccConnId, localAlias}
  | localAlias == "" = ["connection " <> sShow pccConnId <> " alias removed"]
  | otherwise = ["connection " <> sShow pccConnId <> " alias updated: " <> plain localAlias]

viewConnectionIncognitoUpdated :: PendingContactConnection -> Maybe Profile -> Bool -> [StyledString]
viewConnectionIncognitoUpdated PendingContactConnection {pccConnId, customUserProfileId} incognitoProfile testView
  | isJust customUserProfileId =
      case incognitoProfile of
        Just profile
          | testView -> incognitoProfile' profile : message
          | otherwise -> message
          where message = ["connection " <> sShow pccConnId <> " changed to incognito"]
        Nothing -> ["unexpected response when changing connection, please report to developers"]
  | otherwise = ["connection " <> sShow pccConnId <> " changed to non incognito"]

viewConnectionUserChanged :: User -> PendingContactConnection -> User -> PendingContactConnection -> [StyledString]
viewConnectionUserChanged User {localDisplayName = n} PendingContactConnection {pccConnId} User {localDisplayName = n'} PendingContactConnection {connLinkInv = connLinkInv'} =
  case connLinkInv' of
    Just ccLink' -> [userChangedStr <> ", new link:"] <> newLink ccLink'
    _ -> [userChangedStr]
  where
    userChangedStr = "connection " <> sShow pccConnId <> " changed from user " <> plain n <> " to user " <> plain n'
    newLink (CCLink cReq shortLink) =
      [ "",
        plain $ maybe cReqStr strEncode shortLink,
        ""
      ]
        <>
          if isJust shortLink
            then
              [ "The invitation link for old clients:",
                plain cReqStr
              ]
            else []
      where
        cReqStr = strEncode $ simplexChatInvitation cReq

viewContactUserChanged :: User -> Contact -> User -> Contact -> [StyledString]
viewContactUserChanged
  User {localDisplayName = un}
  ct@Contact {localDisplayName = cn}
  User {localDisplayName = un'}
  Contact {localDisplayName = cn'}
    | cn' /= cn = [userChangedStr <> ", new local name: " <> ttyContact cn']
    | otherwise = [userChangedStr]
  where
    userChangedStr = "contact " <> ttyContact' ct <> " changed from user " <> plain un <> " to user " <> plain un'

viewGroupUserChanged :: User -> GroupInfo -> User -> GroupInfo -> [StyledString]
viewGroupUserChanged
  User {localDisplayName = un}
  g@GroupInfo {localDisplayName = gn}
  User {localDisplayName = un'}
  GroupInfo {localDisplayName = gn'}
    | gn' /= gn = [userChangedStr <> ", new local name: " <> ttyGroup gn']
    | otherwise = [userChangedStr]
  where
    userChangedStr = "group " <> ttyGroup' g <> " changed from user " <> plain un <> " to user " <> plain un'

viewConnectionPlan :: ChatConfig -> ACreatedConnLink -> ConnectionPlan -> [StyledString]
viewConnectionPlan ChatConfig {logLevel, testView} _connLink = \case
  CPInvitationLink ilp -> case ilp of
    ILPOk contactSLinkData -> [invOrBiz contactSLinkData "ok to connect"] <> [viewJSON contactSLinkData | testView]
    ILPOwnLink -> [invLink "own link"]
    ILPConnecting Nothing -> [invLink "connecting"]
    ILPConnecting (Just ct) -> [invLink ("connecting to contact " <> ttyContact' ct)]
    ILPKnown ct
      | nextConnectPrepared ct -> [invLink ("known prepared contact " <> ttyContact' ct)]
      | contactDeleted ct -> [invLink ("known deleted contact " <> ttyContact' ct)]
      | otherwise ->
          [ invLink ("known contact " <> ttyContact' ct),
            "use " <> ttyToContact' ct <> highlight' "<message>" <> " to send messages"
          ]
    where
      invLink = ("invitation link: " <>)
      invOrBiz = \case
        Just ContactShortLinkData {business}
          | business -> ("business address: " <>)
        _ -> ("invitation link: " <>)
  CPContactAddress cap -> case cap of
    CAPOk contactSLinkData -> [addrOrBiz contactSLinkData "ok to connect"] <> [viewJSON contactSLinkData | testView]
    CAPOwnLink -> [ctAddr "own address"]
    CAPConnectingConfirmReconnect -> [ctAddr "connecting, allowed to reconnect"]
    CAPConnectingProhibit ct -> [ctAddr ("connecting to contact " <> ttyContact' ct)]
    CAPKnown ct
      | nextConnectPrepared ct -> [ctAddr ("known prepared contact " <> ttyContact' ct)]
      | otherwise ->
          [ ctAddr ("known contact " <> ttyContact' ct),
            "use " <> ttyToContact' ct <> highlight' "<message>" <> " to send messages"
          ]
    CAPContactViaAddress ct -> [ctAddr ("known contact without connection " <> ttyContact' ct)]
    where
      ctAddr = ("contact address: " <>)
      addrOrBiz = \case
        Just ContactShortLinkData {business}
          | business -> ("business address: " <>)
        _ -> ("contact address: " <>)
  CPGroupLink glp -> case glp of
    GLPOk groupSLinkData -> [grpLink "ok to connect"] <> [viewJSON groupSLinkData | testView]
    GLPOwnLink g -> [grpLink "own link for group " <> ttyGroup' g]
    GLPConnectingConfirmReconnect -> [grpLink "connecting, allowed to reconnect"]
    GLPConnectingProhibit Nothing -> [grpLink "connecting"]
    GLPConnectingProhibit (Just g) -> connecting g
    GLPKnown g@GroupInfo {preparedGroup, membership = m} -> case preparedGroup of
      Just PreparedGroup {connLinkStartedConnection} -> case memberStatus m of
        GSMemUnknown
          | connLinkStartedConnection -> connecting g
          | otherwise -> [knownGroup "prepared "]
        GSMemAccepted -> connecting g
        _
          | memberRemoved m -> [knownGroup "deleted "] -- it should not get here, as this plan is returned as GLPOk
          | otherwise -> knownActive
      _ -> knownActive
      where
        knownActive =
          [ knownGroup "",
            "use " <> ttyToGroup g Nothing <> highlight' "<message>" <> " to send messages"
          ]
        knownGroup prepared = grpOrBizLink g <> ": known " <> prepared <> grpOrBiz g <> " " <> ttyGroup' g
    where
      connecting g = [grpOrBizLink g <> ": connecting to " <> grpOrBiz g <> " " <> ttyGroup' g]
      grpLink = ("group link: " <>)
      grpOrBizLink GroupInfo {businessChat} = case businessChat of
        Just _ -> "business address"
        Nothing -> "group link"
      grpOrBiz GroupInfo {businessChat} = case businessChat of
        Just _ -> "business"
        Nothing -> "group"
  CPError e -> viewChatError False logLevel testView e
  where
    nextConnectPrepared Contact {preparedContact, activeConn} = case preparedContact of
      Just _ -> maybe True (\c -> connStatus c == ConnPrepared) activeConn
      _ -> False

viewContactUpdated :: Contact -> Contact -> [StyledString]
viewContactUpdated
  Contact {localDisplayName = n, profile = LocalProfile {fullName, shortDescr, contactLink}}
  Contact {localDisplayName = n', profile = LocalProfile {fullName = fullName', shortDescr = shortDescr', contactLink = contactLink'}}
    | n == n' && fullName == fullName' && shortDescr == shortDescr' && contactLink == contactLink' = []
    | n == n' && fullName == fullName' && shortDescr == shortDescr' =
        if isNothing contactLink'
          then [ttyContact n <> " removed contact address"]
          else [ttyContact n <> " set new contact address, use " <> highlight ("/info " <> n) <> " to view"]
    | n == n' && fullName == fullName' =
        if maybe True T.null shortDescr'
          then ["contact " <> ttyContact n <> " removed bio"]
          else ["contact " <> ttyContact n <> " updated bio: " <> maybe "" plain shortDescr']
    | n == n' = ["contact " <> ttyContact n <> fullNameUpdate]
    | otherwise =
        [ "contact " <> ttyContact n <> " changed to " <> ttyFullName n' fullName' shortDescr',
          "use " <> ttyToContact n' <> highlight' "<message>" <> " to send messages"
        ]
    where
      fullNameUpdate = if T.null fullName' || fullName' == n' then " removed full name" else " updated full name: " <> plain fullName'

viewReceivedMessage :: StyledString -> [StyledString] -> MsgContent -> CurrentTime -> TimeZone -> CIMeta c d -> [StyledString]
viewReceivedMessage = viewReceivedMessage_ False

viewReceivedUpdatedMessage :: StyledString -> [StyledString] -> MsgContent -> CurrentTime -> TimeZone -> CIMeta c d -> [StyledString]
viewReceivedUpdatedMessage = viewReceivedMessage_ True

viewReceivedMessage_ :: Bool -> StyledString -> [StyledString] -> MsgContent -> CurrentTime -> TimeZone -> CIMeta c d -> [StyledString]
viewReceivedMessage_ updated from context mc ts tz meta = receivedWithTime_ ts tz from context meta (ttyMsgContent mc) updated

viewReceivedReaction :: StyledString -> [StyledString] -> StyledString -> CurrentTime -> TimeZone -> UTCTime -> [StyledString]
viewReceivedReaction from styledMsg reactionText ts tz reactionTs =
  prependFirst (ttyMsgTime ts tz reactionTs <> " " <> from) (styledMsg <> ["    " <> reactionText])

receivedWithTime_ :: CurrentTime -> TimeZone -> StyledString -> [StyledString] -> CIMeta c d -> [StyledString] -> Bool -> [StyledString]
receivedWithTime_ ts tz from context CIMeta {itemId, itemTs, itemEdited, itemDeleted, itemLive} styledMsg updated = do
  prependFirst (ttyMsgTime ts tz itemTs <> " " <> from) (context <> prependFirst (indent <> live) styledMsg)
  where
    indent = if null context then "" else "      "
    live
      | itemEdited || isJust itemDeleted = ""
      | otherwise = case itemLive of
          Just True
            | updated -> ttyFrom "[LIVE] "
            | otherwise -> ttyFrom "[LIVE started]" <> " use " <> highlight' ("/show [on/off/" <> show itemId <> "] ")
          Just False -> ttyFrom "[LIVE ended] "
          _ -> ""

ttyMsgTime :: CurrentTime -> TimeZone -> UTCTime -> StyledString
ttyMsgTime now tz time =
  let fmt = if recent now tz time then "%H:%M" else "%m-%d"
      localTime = utcToLocalTime tz time
   in styleTime $ formatTime defaultTimeLocale fmt localTime

recent :: CurrentTime -> TimeZone -> UTCTime -> Bool
recent now tz time = do
  let localNow = utcToLocalTime tz now
      localNowDay = localDay localNow
      localTime = utcToLocalTime tz time
      localTimeDay = localDay localTime
      previousDay18 = LocalTime (addDays (-1) localNowDay) (TimeOfDay 18 0 0)
      currentDay12 = LocalTime localNowDay (TimeOfDay 12 0 0)
  localNowDay == localTimeDay
    || (localNow < currentDay12 && localTime >= previousDay18 && localTimeDay < localNowDay)

viewSentMessage :: StyledString -> [StyledString] -> MsgContent -> CurrentTime -> TimeZone -> CIMeta c d -> [StyledString]
viewSentMessage to context mc ts tz meta@CIMeta {itemEdited, itemDeleted, itemLive} = sentWithTime_ ts tz (prependFirst to $ context <> prependFirst (indent <> live) (ttyMsgContent mc)) meta
  where
    indent = if null context then "" else "      "
    live
      | itemEdited || isJust itemDeleted = ""
      | otherwise = case itemLive of
          Just True -> ttyTo "[LIVE started] "
          Just False -> ttyTo "[LIVE] "
          _ -> ""

viewSentBroadcast :: MsgContent -> Int -> Int -> CurrentTime -> TimeZone -> UTCTime -> [StyledString]
viewSentBroadcast mc s f ts tz time = prependFirst (highlight' "/feed" <> " (" <> sShow s <> failures <> ") " <> ttyMsgTime ts tz time <> " ") (ttyMsgContent mc)
  where
    failures
      | f > 0 = ", " <> sShow f <> " failures"
      | otherwise = ""

viewSentFileInvitation :: StyledString -> CIFile d -> CurrentTime -> TimeZone -> CIMeta c d -> [StyledString]
viewSentFileInvitation to CIFile {fileId, fileSource, fileStatus} ts tz = case fileSource of
  Just (CryptoFile fPath _) -> sentWithTime_ ts tz $ ttySentFile fPath
  _ -> const []
  where
    ttySentFile fPath = ["/f " <> to <> ttyFilePath fPath] <> cancelSending
    cancelSending = case fileStatus of
      CIFSSndTransfer _ _ -> []
      _ -> ["use " <> highlight ("/fc " <> show fileId) <> " to cancel sending"]

sentWithTime_ :: CurrentTime -> TimeZone -> [StyledString] -> CIMeta c d -> [StyledString]
sentWithTime_ ts tz styledMsg CIMeta {itemTs} =
  prependFirst (ttyMsgTime ts tz itemTs <> " ") styledMsg

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

viewSndFileCancelled :: FileTransferMeta -> [SndFileTransfer] -> [StyledString]
viewSndFileCancelled FileTransferMeta {fileId, fileName} fts =
  case filter (\SndFileTransfer {fileStatus = s} -> s /= FSCancelled && s /= FSComplete) fts of
    [] -> ["cancelled sending " <> fileTransferStr fileId fileName]
    ts -> ["cancelled sending " <> fileTransferStr fileId fileName <> " to " <> listRecipients ts]

sendingFile_ :: StyledString -> SndFileTransfer -> [StyledString]
sendingFile_ status ft@SndFileTransfer {recipientDisplayName = c} =
  [status <> " sending " <> sndFile ft <> " to " <> ttyContact c]

uploadingFile :: StyledString -> AChatItem -> [StyledString]
uploadingFile status = \case
  AChatItem _ _ (DirectChat Contact {localDisplayName = c}) ChatItem {file = Just CIFile {fileId, fileName}, chatDir = CIDirectSnd} ->
    [status <> " uploading " <> fileTransferStr fileId fileName <> " for " <> ttyContact c]
  AChatItem _ _ (GroupChat g _scopeInfo) ChatItem {file = Just CIFile {fileId, fileName}, chatDir = CIGroupSnd} ->
    [status <> " uploading " <> fileTransferStr fileId fileName <> " for " <> ttyGroup' g]
  _ -> [status <> " uploading file"]

uploadingFileStandalone :: StyledString -> FileTransferMeta -> [StyledString]
uploadingFileStandalone status FileTransferMeta {fileId, fileName} = [status <> " standalone uploading " <> fileTransferStr fileId fileName]

standaloneUploadRedirect :: FileTransferMeta -> FileTransferMeta -> [StyledString]
standaloneUploadRedirect FileTransferMeta {fileId, fileName} FileTransferMeta {fileId = redirectId} =
  [fileTransferStr fileId fileName <> " uploaded, preparing redirect file " <> sShow redirectId]

standaloneUploadComplete :: FileTransferMeta -> [Text] -> [StyledString]
standaloneUploadComplete FileTransferMeta {fileId, fileName} = \case
  [] -> [fileTransferStr fileId fileName <> " upload complete."]
  uris ->
    fileTransferStr fileId fileName <> " upload complete. download with:"
      : map plain uris

sndFile :: SndFileTransfer -> StyledString
sndFile SndFileTransfer {fileId, fileName} = fileTransferStr fileId fileName

viewReceivedFileInvitation :: StyledString -> CIFile d -> CurrentTime -> TimeZone -> CIMeta c d -> [StyledString]
viewReceivedFileInvitation from file ts tz meta = receivedWithTime_ ts tz from [] meta (receivedFileInvitation_ file) False

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
savingFile' (AChatItem _ _ chat ChatItem {file = Just CIFile {fileId, fileSource = Just (CryptoFile filePath _)}, chatDir}) =
  ["saving file " <> sShow fileId <> fileFrom chat chatDir <> " to " <> plain filePath]
savingFile' _ = ["saving file"] -- shouldn't happen

receivingFile_' :: (Maybe RemoteHostId, Maybe User) -> Bool -> String -> AChatItem -> [StyledString]
receivingFile_' hu testView status (AChatItem _ _ chat ChatItem {file = Just CIFile {fileId, fileName, fileSource = Just f@(CryptoFile _ cfArgs_)}, chatDir}) =
  [plain status <> " receiving " <> fileTransferStr fileId fileName <> fileFrom chat chatDir] <> cfArgsStr cfArgs_ <> getRemoteFileStr
  where
    cfArgsStr (Just cfArgs) = [cryptoFileArgsStr testView cfArgs | status == "completed"]
    cfArgsStr _ = []
    getRemoteFileStr = case hu of
      (Just rhId, Just User {userId})
        | status == "completed" ->
            [ "File received to connected remote host " <> sShow rhId,
              "To download to this device use:",
              highlight ("/get remote file " <> show rhId <> " " <> LB.unpack (J.encode RemoteFile {userId, fileId, sent = False, fileSource = f}))
            ]
      _ -> []
receivingFile_' _ _ status _ = [plain status <> " receiving file"]

receivingFileStandalone :: String -> RcvFileTransfer -> [StyledString]
receivingFileStandalone status RcvFileTransfer {fileId, fileInvitation = FileInvitation {fileName}} =
  [plain status <> " standalone receiving " <> fileTransferStr fileId fileName]

viewLocalFile :: StyledString -> CIFile d -> CurrentTime -> TimeZone -> CIMeta c d -> [StyledString]
viewLocalFile to CIFile {fileId, fileSource} ts tz = case fileSource of
  Just (CryptoFile fPath _) -> sentWithTime_ ts tz [to <> fileTransferStr fileId fPath]
  _ -> const []

cryptoFileArgsStr :: Bool -> CryptoFileArgs -> StyledString
cryptoFileArgsStr testView cfArgs@(CFArgs key nonce)
  | testView = viewJSON cfArgs
  | otherwise = plain $ "encryption key: " <> strEncode key <> ", nonce: " <> strEncode nonce

fileFrom :: ChatInfo c -> CIDirection c d -> StyledString
fileFrom (DirectChat ct) CIDirectRcv = " from " <> ttyContact' ct
fileFrom _ (CIGroupRcv m) = " from " <> ttyMember m
fileFrom _ _ = ""

receivingFile_ :: StyledString -> RcvFileTransfer -> [StyledString]
receivingFile_ status ft@RcvFileTransfer {senderDisplayName = c} =
  [status <> " receiving " <> rcvFile ft <> if c == "" then "" else " from " <> ttyContact c]

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
    fs :: SndFileTransfer -> FileStatus
    fs SndFileTransfer {fileStatus} = fileStatus
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

viewFileTransferStatusXFTP :: AChatItem -> [StyledString]
viewFileTransferStatusXFTP (AChatItem _ _ _ ChatItem {file = Just CIFile {fileId, fileName, fileSize, fileStatus, fileSource}}) =
  case fileStatus of
    CIFSSndStored -> ["sending " <> fstr <> " just started"]
    CIFSSndTransfer progress total -> ["sending " <> fstr <> " in progress " <> fileProgressXFTP progress total fileSize]
    CIFSSndCancelled -> ["sending " <> fstr <> " cancelled"]
    CIFSSndComplete -> ["sending " <> fstr <> " complete"]
    CIFSSndError sndFileErr -> ["sending " <> fstr <> " error: " <> plain (show sndFileErr)]
    CIFSSndWarning sndFileErr -> ["sending " <> fstr <> " warning: " <> plain (show sndFileErr)]
    CIFSRcvInvitation -> ["receiving " <> fstr <> " not accepted yet, use " <> highlight ("/fr " <> show fileId) <> " to receive file"]
    CIFSRcvAccepted -> ["receiving " <> fstr <> " just started"]
    CIFSRcvTransfer progress total -> ["receiving " <> fstr <> " progress " <> fileProgressXFTP progress total fileSize]
    CIFSRcvAborted -> ["receiving " <> fstr <> " aborted, use " <> highlight ("/fr " <> show fileId) <> " to receive file"]
    CIFSRcvComplete -> ["receiving " <> fstr <> " complete" <> maybe "" (\(CryptoFile fp _) -> ", path: " <> plain fp) fileSource]
    CIFSRcvCancelled -> ["receiving " <> fstr <> " cancelled"]
    CIFSRcvError rcvFileErr -> ["receiving " <> fstr <> " error: " <> plain (show rcvFileErr)]
    CIFSRcvWarning rcvFileErr -> ["receiving " <> fstr <> " warning: " <> plain (show rcvFileErr)]
    CIFSInvalid text -> [fstr <> " invalid status: " <> plain text]
  where
    fstr = fileTransferStr fileId fileName
viewFileTransferStatusXFTP _ = ["no file status"]

listRecipients :: [SndFileTransfer] -> StyledString
listRecipients = mconcat . intersperse ", " . map (ttyContact . recipientDisplayName)

fileProgress :: [Integer] -> Integer -> Integer -> StyledString
fileProgress chunksNum chunkSize fileSize =
  sShow (sum chunksNum * chunkSize * 100 `div` fileSize) <> "% of " <> humanReadableSize fileSize

fileProgressXFTP :: Int64 -> Int64 -> Integer -> StyledString
fileProgressXFTP progress total fileSize =
  sShow (progress * 100 `div` total) <> "% of " <> humanReadableSize fileSize

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
    viewJSON WCCallAnswer {answer, iceCandidates}
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

viewVersionInfo :: ChatLogLevel -> CoreVersionInfo -> [StyledString]
viewVersionInfo logLevel CoreVersionInfo {version, simplexmqVersion, simplexmqCommit} =
  map plain $
    if logLevel <= CLLInfo
      then [versionString version, updateStr, "simplexmq: " <> simplexmqVersion <> parens simplexmqCommit]
      else [versionString version, updateStr]
  where

parens :: (IsString a, Semigroup a) => a -> a
parens s = " (" <> s <> ")"

viewRemoteHosts :: [RemoteHostInfo] -> [StyledString]
viewRemoteHosts = \case
  [] -> ["No remote hosts"]
  hs -> "Remote hosts: " : map viewRemoteHostInfo hs
  where
    viewRemoteHostInfo RemoteHostInfo {remoteHostId, hostDeviceName, sessionState, bindAddress_, bindPort_} =
      plain $ tshow remoteHostId <> ". " <> hostDeviceName <> maybe "" viewSessionState sessionState <> ctrlBinds bindAddress_ bindPort_
    ctrlBinds Nothing Nothing = ""
    ctrlBinds rca_ port_ = mconcat [" [", maybe "" rca rca_, maybe "" port port_, "]"]
      where
        rca RCCtrlAddress {interface, address} = interface <> " " <> decodeLatin1 (strEncode address)
        port p = ":" <> tshow p
    viewSessionState = \case
      RHSStarting -> " (starting)"
      RHSConnecting _ -> " (connecting)"
      RHSPendingConfirmation {sessionCode} -> " (pending confirmation, code: " <> sessionCode <> ")"
      RHSConfirmed _ -> " (confirmed)"
      RHSConnected _ -> " (connected)"

viewRemoteCtrls :: [RemoteCtrlInfo] -> [StyledString]
viewRemoteCtrls = \case
  [] -> ["No remote controllers"]
  hs -> "Remote controllers: " : map viewRemoteCtrlInfo hs
  where
    viewRemoteCtrlInfo RemoteCtrlInfo {remoteCtrlId, ctrlDeviceName, sessionState} =
      plain $ tshow remoteCtrlId <> ". " <> ctrlDeviceName <> maybe "" viewSessionState sessionState
    viewSessionState = \case
      RCSStarting -> " (starting)"
      RCSSearching -> " (searching)"
      RCSConnecting -> " (connecting)"
      RCSPendingConfirmation {sessionCode} -> " (pending confirmation, code: " <> sessionCode <> ")"
      RCSConnected _ -> " (connected)"

viewRemoteCtrl :: CtrlAppInfo -> AppVersion -> Bool -> StyledString
viewRemoteCtrl CtrlAppInfo {deviceName, appVersionRange = AppVersionRange _ (AppVersion ctrlVersion)} (AppVersion v) compatible =
  (if T.null deviceName then "" else plain deviceName <> ", ")
    <> ("v" <> plain (V.showVersion ctrlVersion) <> ctrlVersionInfo)
  where
    ctrlVersionInfo
      | ctrlVersion < v = " (older than this app - upgrade controller" <> showCompatible <> ")"
      | ctrlVersion > v = " (newer than this app - upgrade it" <> showCompatible <> ")"
      | otherwise = ""
    showCompatible = if compatible then "" else ", " <> bold' "not compatible"

viewRemoteCtrlStopped :: RemoteCtrlStopReason -> [StyledString]
viewRemoteCtrlStopped = \case
  RCSRConnectionFailed (ChatErrorAgent (RCP RCEIdentity) _) ->
    ["remote controller stopped: this link was used with another controller, please create a new link on the host"]
  _ -> ["remote controller stopped"]

viewChatError :: Bool -> ChatLogLevel -> Bool -> ChatError -> [StyledString]
viewChatError isCmd logLevel testView = \case
  ChatError err -> case err of
    CENoActiveUser -> ["error: active user is required"]
    CENoConnectionUser agentConnId -> ["error: message user not found, conn id: " <> sShow agentConnId | logLevel <= CLLError]
    CENoSndFileUser aFileId -> ["error: snd file user not found, file id: " <> sShow aFileId | logLevel <= CLLError]
    CENoRcvFileUser aFileId -> ["error: rcv file user not found, file id: " <> sShow aFileId | logLevel <= CLLError]
    CEActiveUserExists -> ["error: active user already exists"]
    CEUserExists name -> ["user with the name " <> ttyContact name <> " already exists"]
    CEUserUnknown -> ["user does not exist or incorrect password"]
    CEDifferentActiveUser commandUserId activeUserId -> ["error: different active user, command user id: " <> sShow commandUserId <> ", active user id: " <> sShow activeUserId]
    CECantDeleteActiveUser _ -> ["cannot delete active user"]
    CECantDeleteLastUser _ -> ["cannot delete last user"]
    CECantHideLastUser _ -> ["cannot hide the only not hidden user"]
    CEHiddenUserAlwaysMuted _ -> ["hidden user always muted when inactive"]
    CEEmptyUserPassword _ -> ["user password is required"]
    CEUserAlreadyHidden _ -> ["user is already hidden"]
    CEUserNotHidden _ -> ["user is not hidden"]
    CEInvalidDisplayName {displayName, validName} ->
      map plain $
        [if T.null displayName then "display name can't be empty" else "invalid display name: " <> viewName displayName]
          <> ["you could use this one: " <> viewName validName | not (T.null validName)]
    CEChatNotStarted -> ["error: chat not started"]
    CEChatNotStopped -> ["error: chat not stopped"]
    CEChatStoreChanged -> ["error: chat store changed, please restart chat"]
    CEInvalidConnReq -> viewInvalidConnReq
    CEUnsupportedConnReq -> [ "", "Connection link is not supported by the your app version, please ugrade it.", plain updateStr]
    CEInvalidChatMessage Connection {connId} msgMeta_ msg e ->
      [ plain $
          ("chat message error: " <> e <> " (" <> T.unpack (T.take 120 msg) <> ")")
            <> (", connection id: " <> show connId)
            <> maybe "" (\MsgMetaJSON {rcvId} -> ", agent msg rcv id: " <> show rcvId) msgMeta_
      ]
    CEConnReqMessageProhibited -> ["message is not allowed with this connection link"]
    CEContactNotFound cName m_ -> viewContactNotFound cName m_
    CEContactNotReady c -> [ttyContact' c <> ": not ready"]
    CEContactDisabled ct -> [ttyContact' ct <> ": disabled, to enable: " <> highlight ("/enable " <> viewContactName ct) <> ", to delete: " <> highlight ("/d " <> viewContactName ct)]
    CEContactNotActive c -> [ttyContact' c <> ": not active"]
    CEConnectionDisabled Connection {connId, connType} -> [plain $ "connection " <> textEncode connType <> " (" <> tshow connId <> ") is disabled" | logLevel <= CLLWarning]
    CEGroupDuplicateMember c -> ["contact " <> ttyContact c <> " is already in the group"]
    CEGroupDuplicateMemberId -> ["cannot add member - duplicate member ID"]
    CEGroupUserRole g role ->
      (: []) . (ttyGroup' g <>) $ case role of
        GRAuthor -> ": you don't have permission to send messages"
        _ -> ": you have insufficient permissions for this action, the required role is " <> plain (strEncode role)
    CEGroupMemberInitialRole g role -> [ttyGroup' g <> ": initial role for group member cannot be " <> plain (strEncode role) <> ", use member or observer"]
    CEContactIncognitoCantInvite -> ["you're using your main profile for this group - prohibited to invite contacts to whom you are connected incognito"]
    CEGroupIncognitoCantInvite -> ["you are using an incognito profile for this group - prohibited to invite contacts"]
    CEGroupContactRole c -> ["contact " <> ttyContact c <> " has insufficient permissions for this group action"]
    CEGroupNotJoined g -> ["you did not join this group, use " <> highlight ("/join #" <> viewGroupName g)]
    CEGroupMemberNotActive -> ["your group connection is not active yet, try later"]
    CECantBlockMemberForSelf g m showMsgs ->
      [ "admins or above can't block member for self, use "
          <> highlight
            ( (if showMsgs then "/unblock for all" else "/block for all")
                <> (" #" <> viewGroupName g <> " " <> viewMemberName m)
            )
      ]
    CEGroupMemberUserRemoved -> ["you are no longer a member of the group"]
    CEGroupMemberNotFound -> ["group doesn't have this member"]
    CEGroupCantResendInvitation g c -> viewCannotResendInvitation g c
    CEGroupInternal s -> ["chat group bug: " <> plain s]
    CEFileNotFound f -> ["file not found: " <> plain f]
    CEFileSize f -> ["file size exceeds the limit: " <> plain f]
    CEFileAlreadyReceiving f -> ["file is already being received: " <> plain f]
    CEFileCancelled f -> ["file cancelled: " <> plain f]
    CEFileCancel fileId e -> ["error cancelling file " <> sShow fileId <> ": " <> sShow e]
    CEFileAlreadyExists f -> ["file already exists: " <> plain f]
    CEFileRead f e -> ["cannot read file " <> plain f <> ": " <> plain e]
    CEFileWrite f e -> ["cannot write file " <> plain f <> ": " <> plain e]
    CEFileSend fileId e -> ["error sending file " <> sShow fileId <> ": " <> sShow e]
    CEFileRcvChunk e -> ["error receiving file: " <> plain e]
    CEFileInternal e -> ["file error: " <> plain e]
    CEFileImageType _ -> ["image type must be jpg, send as a file using " <> highlight' "/f"]
    CEFileImageSize _ -> ["max image size: " <> sShow maxImageSize <> " bytes, resize it or send as a file using " <> highlight' "/f"]
    CEFileNotReceived fileId -> ["file " <> sShow fileId <> " not received"]
    CEFileNotApproved fileId unknownSrvs -> ["file " <> sShow fileId <> " aborted, unknwon XFTP servers:"] <> map (plain . show) unknownSrvs
    CEFallbackToSMPProhibited fileId -> ["recipient tried to accept file " <> sShow fileId <> " via old protocol, prohibited"]
    CEInlineFileProhibited _ -> ["A small file sent without acceptance - you can enable receiving such files with -f option."]
    CEInvalidForward -> ["cannot forward message(s)"]
    CEInvalidChatItemUpdate -> ["cannot update this item"]
    CEInvalidChatItemDelete -> ["cannot delete this item"]
    CEHasCurrentCall -> ["call already in progress"]
    CENoCurrentCall -> ["no call in progress"]
    CECallContact _ -> []
    CECallState _ -> []
    CEDirectMessagesProhibited dir ct -> viewDirectMessagesProhibited dir ct
    CEAgentVersion -> ["unsupported agent version"]
    CEAgentNoSubResult connId -> ["no subscription result for connection: " <> sShow connId]
    CEServerProtocol p -> [plain $ "Servers for protocol " <> strEncode p <> " cannot be configured by the users"]
    CECommandError e -> ["bad chat command: " <> plain e]
    CEAgentCommandError e -> ["agent command error: " <> plain e]
    CEInvalidFileDescription e -> ["invalid file description: " <> plain e]
    CEConnectionIncognitoChangeProhibited -> ["incognito mode change prohibited"]
    CEConnectionUserChangeProhibited -> ["incognito mode change prohibited for user"]
    CEPeerChatVRangeIncompatible -> ["peer chat protocol version range incompatible"]
    CEInternalError e -> ["internal chat error: " <> plain e]
    CEException e -> ["exception: " <> plain e]
  -- e -> ["chat error: " <> sShow e]
  ChatErrorStore err -> case err of
    SEDuplicateName -> ["this display name is already used by user, contact or group"]
    SEUserNotFoundByName u -> ["no user " <> ttyContact u]
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
    SEConnectionNotFound agentConnId -> ["event connection not found, agent ID: " <> sShow agentConnId | logLevel <= CLLWarning] -- mutes delete group error
    SEChatItemNotFoundByText text -> ["message not found by text: " <> plain text]
    SEDuplicateGroupLink g -> ["you already have link for this group, to show: " <> highlight ("/show link #" <> viewGroupName g)]
    SEGroupLinkNotFound g -> ["no group link, to create: " <> highlight ("/create link #" <> viewGroupName g)]
    SERemoteCtrlNotFound rcId -> ["no remote controller " <> sShow rcId]
    SERemoteHostNotFound rhId -> ["no remote host " <> sShow rhId]
    SEDuplicateGroupMessage {groupId, sharedMsgId}
      | testView -> ["duplicate group message, group id: " <> sShow groupId <> ", message id: " <> sShow sharedMsgId]
      | otherwise -> []
    SEUserNoteFolderNotFound -> ["no notes folder"]
    e -> ["chat db error: " <> sShow e]
  ChatErrorDatabase err -> case err of
    DBErrorEncrypted -> ["error: chat database is already encrypted"]
    DBErrorPlaintext -> ["error: chat database is not encrypted"]
    DBErrorExport e -> ["error encrypting database: " <> sqliteError' e]
    DBErrorOpen e -> ["error opening database after encryption: " <> sqliteError' e]
    e -> ["chat database error: " <> sShow e]
  ChatErrorAgent err entity_ -> case err of
    CMD PROHIBITED cxt -> [withConnEntity <> plain ("error: command is prohibited, " <> cxt)]
    SMP _ SMP.AUTH ->
      [ withConnEntity
          <> "error: connection authorization failed - this could happen if connection was deleted, secured with different credentials, or due to a bug - please re-create the connection"
      ]
    SMP _ (SMP.BLOCKED BlockingInfo {reason}) ->
      [withConnEntity <> "error: connection blocked by server operator: " <> reasonStr]
      where
        reasonStr = case reason of
          BRSpam -> "spam"
          BRContent -> "content violates conditions of use"
    BROKER _ NETWORK | not isCmd -> []
    BROKER _ TIMEOUT | not isCmd -> []
    AGENT A_DUPLICATE -> [withConnEntity <> "error: AGENT A_DUPLICATE" | logLevel == CLLDebug || isCmd]
    AGENT (A_PROHIBITED e) -> [withConnEntity <> "error: AGENT A_PROHIBITED, " <> plain e | logLevel <= CLLWarning || isCmd]
    CONN NOT_FOUND _ -> [withConnEntity <> "error: CONN NOT_FOUND" | logLevel <= CLLWarning || isCmd]
    CRITICAL restart e -> [plain $ "critical error: " <> e] <> ["please restart the app" | restart]
    INTERNAL e -> [plain $ "internal error: " <> e]
    e -> [withConnEntity <> "smp agent error: " <> sShow e | logLevel <= CLLWarning || isCmd]
    where
      withConnEntity = case entity_ of
        Just entity@(RcvDirectMsgConnection conn contact_) -> case contact_ of
          Just Contact {contactId} ->
            "[" <> connEntityLabel entity <> ", contactId: " <> sShow contactId <> ", connId: " <> cId conn <> "] "
          Nothing ->
            "[" <> connEntityLabel entity <> ", connId: " <> cId conn <> "] "
        Just entity@(RcvGroupMsgConnection conn GroupInfo {groupId} GroupMember {groupMemberId}) ->
          "[" <> connEntityLabel entity <> ", groupId: " <> sShow groupId <> ", memberId: " <> sShow groupMemberId <> ", connId: " <> cId conn <> "] "
        Just entity@(RcvFileConnection conn RcvFileTransfer {fileId}) ->
          "[" <> connEntityLabel entity <> ", fileId: " <> sShow fileId <> ", connId: " <> cId conn <> "] "
        Just entity@(SndFileConnection conn SndFileTransfer {fileId}) ->
          "[" <> connEntityLabel entity <> ", fileId: " <> sShow fileId <> ", connId: " <> cId conn <> "] "
        Just entity@(UserContactConnection conn UserContact {userContactLinkId}) ->
          "[" <> connEntityLabel entity <> ", userContactLinkId: " <> sShow userContactLinkId <> ", connId: " <> cId conn <> "] "
        Nothing -> ""
      cId :: Connection -> StyledString
      cId Connection {connId} = sShow connId
  ChatErrorRemoteCtrl e -> [plain $ "remote controller error: " <> show e]
  ChatErrorRemoteHost RHNew e -> [plain $ "new remote host error: " <> show e]
  ChatErrorRemoteHost (RHId rhId) e -> [plain $ "remote host " <> show rhId <> " error: " <> show e]
  where
    fileNotFound fileId = ["file " <> sShow fileId <> " not found"]
    sqliteError' = \case
      SQLiteErrorNotADatabase -> "wrong passphrase or invalid database file"
      SQLiteError e -> sShow e

viewConnectionEntityDisabled :: ConnectionEntity -> [StyledString]
viewConnectionEntityDisabled entity = case entity of
  RcvDirectMsgConnection _ (Just c) -> ["[" <> entityLabel <> "] connection is disabled, to enable: " <> highlight ("/enable " <> viewContactName c) <> ", to delete: " <> highlight ("/d " <> viewContactName c)]
  RcvGroupMsgConnection _ g m -> ["[" <> entityLabel <> "] connection is disabled, to enable: " <> highlight ("/enable #" <> viewGroupName g <> " " <> viewMemberName m)]
  _ -> ["[" <> entityLabel <> "] connection is disabled"]
  where
    entityLabel = connEntityLabel entity

viewConnectionEntityInactive :: ConnectionEntity -> Bool -> [StyledString]
viewConnectionEntityInactive entity inactive
  | inactive = ["[" <> connEntityLabel entity <> "] connection is marked as inactive"]
  | otherwise = ["[" <> connEntityLabel entity <> "] inactive connection is marked as active"]

viewJSON :: J.ToJSON a => a -> StyledString
viewJSON = plain . LB.toStrict . J.encode

connEntityLabel :: ConnectionEntity -> StyledString
connEntityLabel = \case
  RcvDirectMsgConnection _ (Just Contact {localDisplayName = c}) -> plain c
  RcvDirectMsgConnection _ Nothing -> "rcv direct msg"
  RcvGroupMsgConnection _ GroupInfo {localDisplayName = g} GroupMember {localDisplayName = m} -> plain $ "#" <> g <> " " <> m
  RcvFileConnection _ RcvFileTransfer {fileInvitation = FileInvitation {fileName}} -> plain $ "rcv file " <> T.pack fileName
  SndFileConnection _ SndFileTransfer {fileName} -> plain $ "snd file " <> T.pack fileName
  UserContactConnection _ UserContact {} -> "contact address"

ttyContact :: ContactName -> StyledString
ttyContact = styled (colored Green) . viewName

ttyContact' :: Contact -> StyledString
ttyContact' Contact {localDisplayName = c} = ttyContact c

ttyFullContact :: Contact -> StyledString
ttyFullContact Contact {localDisplayName, profile = LocalProfile {fullName, shortDescr}} =
  ttyFullName localDisplayName fullName shortDescr

ttyMember :: GroupMember -> StyledString
ttyMember GroupMember {localDisplayName} = ttyContact localDisplayName

ttyFullMember :: GroupMember -> StyledString
ttyFullMember GroupMember {localDisplayName, memberProfile = LocalProfile {fullName, shortDescr}} =
  ttyFullName localDisplayName fullName shortDescr

ttyFullName :: ContactName -> Text -> Maybe Text -> StyledString
ttyFullName c fullName shortDescr = ttyContact c <> optFullName c fullName shortDescr

ttyToContact :: ContactName -> StyledString
ttyToContact c = ttyTo $ "@" <> viewName c <> " "

ttyToContact' :: Contact -> StyledString
ttyToContact' ct@Contact {localDisplayName = c} = ctIncognito ct <> ttyToContact c

ttyToContactEdited' :: Contact -> StyledString
ttyToContactEdited' ct@Contact {localDisplayName = c} = ctIncognito ct <> ttyTo ("@" <> viewName c <> " [edited] ")

ttyQuotedContact :: Contact -> StyledString
ttyQuotedContact Contact {localDisplayName = c} = ttyFrom $ viewName c <> ">"

ttyQuotedMember :: Maybe GroupMember -> StyledString
ttyQuotedMember (Just GroupMember {localDisplayName = c}) = "> " <> ttyFrom (viewName c)
ttyQuotedMember _ = "> " <> ttyFrom "?"

ttyFromContact :: Contact -> StyledString
ttyFromContact ct@Contact {localDisplayName = c} = ctIncognito ct <> ttyFrom (viewName c <> "> ")

ttyFromContactEdited :: Contact -> StyledString
ttyFromContactEdited ct@Contact {localDisplayName = c} = ctIncognito ct <> ttyFrom (viewName c <> "> [edited] ")

ttyFromContactDeleted :: Contact -> Maybe Text -> StyledString
ttyFromContactDeleted ct@Contact {localDisplayName = c} deletedText_ =
  ctIncognito ct <> ttyFrom (viewName c <> "> " <> maybe "" (\t -> "[" <> t <> "] ") deletedText_)

ttyGroup :: GroupName -> StyledString
ttyGroup g = styled (colored Blue) $ "#" <> viewName g

ttyGroup' :: GroupInfo -> StyledString
ttyGroup' = ttyGroup . groupName'

viewContactName :: Contact -> Text
viewContactName = viewName . localDisplayName'

viewGroupName :: GroupInfo -> Text
viewGroupName = viewName . groupName'

viewMemberName :: GroupMember -> Text
viewMemberName GroupMember {localDisplayName = n} = viewName n

ttyGroups :: [GroupName] -> StyledString
ttyGroups [] = ""
ttyGroups [g] = ttyGroup g
ttyGroups (g : gs) = ttyGroup g <> ", " <> ttyGroups gs

ttyFullGroup :: GroupInfo -> StyledString
ttyFullGroup GroupInfo {localDisplayName = g, groupProfile = GroupProfile {fullName, shortDescr}} =
  ttyGroup g <> optFullName g fullName shortDescr

ttyFromGroup :: GroupInfo -> Maybe GroupChatScopeInfo -> GroupMember -> StyledString
ttyFromGroup g scopeInfo m = ttyFromGroupAttention g scopeInfo m False

ttyFromGroupAttention :: GroupInfo -> Maybe GroupChatScopeInfo -> GroupMember -> Bool -> StyledString
ttyFromGroupAttention g scopeInfo m attention = membershipIncognito g <> ttyFrom (fromGroupAttention_ g scopeInfo m attention)

ttyFromGroupEdited :: GroupInfo -> Maybe GroupChatScopeInfo -> GroupMember -> StyledString
ttyFromGroupEdited g scopeInfo m = membershipIncognito g <> ttyFrom (fromGroup_ g scopeInfo m <> "[edited] ")

ttyFromGroupDeleted :: GroupInfo -> Maybe GroupChatScopeInfo -> GroupMember -> Maybe Text -> StyledString
ttyFromGroupDeleted g scopeInfo m deletedText_ =
  membershipIncognito g <> ttyFrom (fromGroup_ g scopeInfo m <> maybe "" (\t -> "[" <> t <> "] ") deletedText_)

fromGroup_ :: GroupInfo -> Maybe GroupChatScopeInfo -> GroupMember -> Text
fromGroup_ g scopeInfo m = fromGroupAttention_ g scopeInfo m False

fromGroupAttention_ :: GroupInfo -> Maybe GroupChatScopeInfo -> GroupMember -> Bool -> Text
fromGroupAttention_ g scopeInfo m attention =
  let attn = if attention then "!" else ""
   in "#" <> viewGroupName g <> " " <> groupScopeInfoStr scopeInfo <> viewMemberName m <> attn <> "> "

ttyFrom :: Text -> StyledString
ttyFrom = styled $ colored Yellow

ttyTo :: Text -> StyledString
ttyTo = styled $ colored Cyan

ttyToGroup :: GroupInfo -> Maybe GroupChatScopeInfo -> StyledString
ttyToGroup g scopeInfo = membershipIncognito g <> ttyTo ("#" <> viewGroupName g <> " " <> groupScopeInfoStr scopeInfo)

ttyToGroupEdited :: GroupInfo -> Maybe GroupChatScopeInfo -> StyledString
ttyToGroupEdited g scopeInfo = membershipIncognito g <> ttyTo ("#" <> viewGroupName g <> groupScopeInfoStr scopeInfo <> " [edited] ")

groupScopeInfoStr :: Maybe GroupChatScopeInfo -> Text
groupScopeInfoStr = \case
  Nothing -> ""
  Just (GCSIMemberSupport {groupMember_}) -> case groupMember_ of
    Nothing -> "(support) "
    Just m -> "(support: " <> viewMemberName m <> ") "

ttyFilePath :: FilePath -> StyledString
ttyFilePath = plain

optFullName :: ContactName -> Text -> Maybe Text -> StyledString
optFullName localDisplayName fullName shortDescr = plain $ optionalFullName localDisplayName fullName shortDescr

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
