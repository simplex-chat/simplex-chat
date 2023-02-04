{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Controller where

import Control.Concurrent (ThreadId)
import Control.Concurrent.Async (Async)
import Control.Exception
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Crypto.Random (ChaChaDRG)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (ord)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.String
import Data.Text (Text)
import Data.Time (ZonedTime)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)
import Data.Version (showVersion)
import GHC.Generics (Generic)
import Language.Haskell.TH (Exp, Q, runIO)
import Numeric.Natural
import qualified Paths_simplex_chat as SC
import Simplex.Chat.Call
import Simplex.Chat.Markdown (MarkdownList)
import Simplex.Chat.Messages
import Simplex.Chat.Protocol
import Simplex.Chat.Store (AutoAccept, StoreError, UserContactLink)
import Simplex.Chat.Types
import Simplex.Messaging.Agent (AgentClient)
import Simplex.Messaging.Agent.Client (AgentLocks, SMPTestFailure)
import Simplex.Messaging.Agent.Env.SQLite (AgentConfig, NetworkConfig)
import Simplex.Messaging.Agent.Lock
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Agent.Store.SQLite (SQLiteStore)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Notifications.Protocol (DeviceToken (..), NtfTknStatus)
import Simplex.Messaging.Parsers (dropPrefix, enumJSON, parseAll, parseString, sumTypeJSON)
import Simplex.Messaging.Protocol (AProtocolType, CorrId, MsgFlags, NtfServer, QueueId)
import Simplex.Messaging.TMap (TMap)
import Simplex.Messaging.Transport (simplexMQVersion)
import Simplex.Messaging.Transport.Client (TransportHost)
import System.IO (Handle)
import System.Mem.Weak (Weak)
import UnliftIO.STM

versionNumber :: String
versionNumber = showVersion SC.version

versionString :: String -> String
versionString version = "SimpleX Chat v" <> version

updateStr :: String
updateStr = "To update run: curl -o- https://raw.githubusercontent.com/simplex-chat/simplex-chat/master/install.sh | bash"

buildTimestampQ :: Q Exp
buildTimestampQ = do
  s <- formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S") <$> runIO getCurrentTime
  [|fromString s|]

simplexmqCommitQ :: Q Exp
simplexmqCommitQ = do
  s <- either error B.unpack . A.parseOnly commitHashP <$> runIO (B.readFile "./cabal.project")
  [|fromString s|]
  where
    commitHashP :: A.Parser ByteString
    commitHashP =
      A.manyTill' A.anyChar "location: https://github.com/simplex-chat/simplexmq.git"
        *> A.takeWhile (== ' ')
        *> A.endOfLine
        *> A.takeWhile (== ' ')
        *> "tag: "
        *> A.takeWhile (A.notInClass " \r\n")

coreVersionInfo :: String -> String -> CoreVersionInfo
coreVersionInfo buildTimestamp simplexmqCommit =
  CoreVersionInfo
    { version = versionNumber,
      buildTimestamp,
      simplexmqVersion = simplexMQVersion,
      simplexmqCommit
    }

data ChatConfig = ChatConfig
  { agentConfig :: AgentConfig,
    yesToMigrations :: Bool,
    defaultServers :: DefaultAgentServers,
    tbqSize :: Natural,
    fileChunkSize :: Integer,
    inlineFiles :: InlineFilesConfig,
    subscriptionConcurrency :: Int,
    subscriptionEvents :: Bool,
    hostEvents :: Bool,
    logLevel :: ChatLogLevel,
    testView :: Bool,
    ciExpirationInterval :: Int -- microseconds
  }

data DefaultAgentServers = DefaultAgentServers
  { smp :: NonEmpty SMPServerWithAuth,
    ntf :: [NtfServer],
    netCfg :: NetworkConfig
  }

data InlineFilesConfig = InlineFilesConfig
  { offerChunks :: Integer,
    sendChunks :: Integer,
    totalSendChunks :: Integer,
    receiveChunks :: Integer,
    receiveInstant :: Bool
  }

defaultInlineFilesConfig :: InlineFilesConfig
defaultInlineFilesConfig =
  InlineFilesConfig
    { offerChunks = 15, -- max when chunks are offered / received with the option - limited to 255 on the encoding level
      sendChunks = 6, -- max per file when chunks will be sent inline without acceptance
      totalSendChunks = 30, -- max per conversation when chunks will be sent inline without acceptance
      receiveChunks = 8, -- max when chunks are accepted
      receiveInstant = True -- allow receiving instant files, within receiveChunks limit
    }

data ActiveTo = ActiveNone | ActiveC ContactName | ActiveG GroupName
  deriving (Eq)

data ChatDatabase = ChatDatabase {chatStore :: SQLiteStore, agentStore :: SQLiteStore}

data ChatController = ChatController
  { currentUser :: TVar (Maybe User),
    activeTo :: TVar ActiveTo,
    firstTime :: Bool,
    smpAgent :: AgentClient,
    agentAsync :: TVar (Maybe (Async (), Maybe (Async ()))),
    chatStore :: SQLiteStore,
    chatStoreChanged :: TVar Bool, -- if True, chat should be fully restarted
    idsDrg :: TVar ChaChaDRG,
    inputQ :: TBQueue String,
    outputQ :: TBQueue (Maybe CorrId, ChatResponse),
    notifyQ :: TBQueue Notification,
    sendNotification :: Notification -> IO (),
    chatLock :: Lock,
    sndFiles :: TVar (Map Int64 Handle),
    rcvFiles :: TVar (Map Int64 Handle),
    currentCalls :: TMap ContactId Call,
    config :: ChatConfig,
    filesFolder :: TVar (Maybe FilePath), -- path to files folder for mobile apps,
    incognitoMode :: TVar Bool,
    expireCIThreads :: TMap UserId (Maybe (Async ())),
    expireCIFlags :: TMap UserId Bool,
    cleanupManagerAsync :: TVar (Maybe (Async ())),
    timedItemThreads :: TMap (ChatRef, ChatItemId) (TVar (Maybe (Weak ThreadId))),
    showLiveItems :: TVar Bool
  }

data HelpSection = HSMain | HSFiles | HSGroups | HSMyAddress | HSMarkdown | HSMessages | HSSettings
  deriving (Show, Generic)

instance ToJSON HelpSection where
  toJSON = J.genericToJSON . enumJSON $ dropPrefix "HS"
  toEncoding = J.genericToEncoding . enumJSON $ dropPrefix "HS"

data ChatCommand
  = ShowActiveUser
  | CreateActiveUser Profile Bool
  | ListUsers
  | APISetActiveUser UserId
  | SetActiveUser UserName
  | APIDeleteUser UserId Bool
  | DeleteUser UserName Bool
  | StartChat {subscribeConnections :: Bool, enableExpireChatItems :: Bool}
  | APIStopChat
  | APIActivateChat
  | APISuspendChat {suspendTimeout :: Int}
  | ResubscribeAllConnections
  | SetFilesFolder FilePath
  | SetIncognito Bool
  | APIExportArchive ArchiveConfig
  | APIImportArchive ArchiveConfig
  | APIDeleteStorage
  | APIStorageEncryption DBEncryptionConfig
  | ExecChatStoreSQL Text
  | ExecAgentStoreSQL Text
  | APIGetChats {userId :: UserId, pendingConnections :: Bool}
  | APIGetChat ChatRef ChatPagination (Maybe String)
  | APIGetChatItems Int
  | APISendMessage {chatRef :: ChatRef, liveMessage :: Bool, composedMessage :: ComposedMessage}
  | APIUpdateChatItem {chatRef :: ChatRef, chatItemId :: ChatItemId, liveMessage :: Bool, msgContent :: MsgContent}
  | APIDeleteChatItem ChatRef ChatItemId CIDeleteMode
  | APIChatRead ChatRef (Maybe (ChatItemId, ChatItemId))
  | APIChatUnread ChatRef Bool
  | APIDeleteChat ChatRef
  | APIClearChat ChatRef
  | APIAcceptContact Int64
  | APIRejectContact Int64
  | APISendCallInvitation ContactId CallType
  | SendCallInvitation ContactName CallType
  | APIRejectCall ContactId
  | APISendCallOffer ContactId WebRTCCallOffer
  | APISendCallAnswer ContactId WebRTCSession
  | APISendCallExtraInfo ContactId WebRTCExtraInfo
  | APIEndCall ContactId
  | APIGetCallInvitations
  | APICallStatus ContactId WebRTCCallStatus
  | APIUpdateProfile UserId Profile
  | APISetContactPrefs ContactId Preferences
  | APISetContactAlias ContactId LocalAlias
  | APISetConnectionAlias Int64 LocalAlias
  | APIParseMarkdown Text
  | APIGetNtfToken
  | APIRegisterToken DeviceToken NotificationsMode
  | APIVerifyToken DeviceToken C.CbNonce ByteString
  | APIDeleteToken DeviceToken
  | APIGetNtfMessage {nonce :: C.CbNonce, encNtfInfo :: ByteString}
  | APIAddMember GroupId ContactId GroupMemberRole
  | APIJoinGroup GroupId
  | APIMemberRole GroupId GroupMemberId GroupMemberRole
  | APIRemoveMember GroupId GroupMemberId
  | APILeaveGroup GroupId
  | APIListMembers GroupId
  | APIUpdateGroupProfile GroupId GroupProfile
  | APICreateGroupLink GroupId
  | APIDeleteGroupLink GroupId
  | APIGetGroupLink GroupId
  | APIGetUserSMPServers UserId
  | GetUserSMPServers
  | APISetUserSMPServers UserId SMPServersConfig
  | SetUserSMPServers SMPServersConfig
  | TestSMPServer UserId SMPServerWithAuth
  | APISetChatItemTTL UserId (Maybe Int64)
  | SetChatItemTTL (Maybe Int64)
  | APIGetChatItemTTL UserId
  | GetChatItemTTL
  | APISetNetworkConfig NetworkConfig
  | APIGetNetworkConfig
  | APISetChatSettings ChatRef ChatSettings
  | APIContactInfo ContactId
  | APIGroupMemberInfo GroupId GroupMemberId
  | APISwitchContact ContactId
  | APISwitchGroupMember GroupId GroupMemberId
  | APIGetContactCode ContactId
  | APIGetGroupMemberCode GroupId GroupMemberId
  | APIVerifyContact ContactId (Maybe Text)
  | APIVerifyGroupMember GroupId GroupMemberId (Maybe Text)
  | APIEnableContact ContactId
  | APIEnableGroupMember GroupId GroupMemberId
  | ShowMessages ChatName Bool
  | ContactInfo ContactName
  | GroupMemberInfo GroupName ContactName
  | SwitchContact ContactName
  | SwitchGroupMember GroupName ContactName
  | GetContactCode ContactName
  | GetGroupMemberCode GroupName ContactName
  | VerifyContact ContactName (Maybe Text)
  | VerifyGroupMember GroupName ContactName (Maybe Text)
  | EnableContact ContactName
  | EnableGroupMember GroupName ContactName
  | ChatHelp HelpSection
  | Welcome
  | APIAddContact UserId
  | AddContact
  | APIConnect UserId (Maybe AConnectionRequestUri)
  | Connect (Maybe AConnectionRequestUri)
  | ConnectSimplex -- UserId (not used in UI)
  | DeleteContact ContactName
  | ClearContact ContactName
  | APIListContacts UserId
  | ListContacts
  | APICreateMyAddress UserId
  | CreateMyAddress
  | APIDeleteMyAddress UserId
  | DeleteMyAddress
  | APIShowMyAddress UserId
  | ShowMyAddress
  | APIAddressAutoAccept UserId (Maybe AutoAccept)
  | AddressAutoAccept (Maybe AutoAccept)
  | AcceptContact ContactName
  | RejectContact ContactName
  | SendMessage ChatName ByteString
  | SendLiveMessage ChatName ByteString
  | SendMessageQuote {contactName :: ContactName, msgDir :: AMsgDirection, quotedMsg :: ByteString, message :: ByteString}
  | SendMessageBroadcast ByteString -- UserId (not used in UI)
  | DeleteMessage ChatName ByteString
  | EditMessage {chatName :: ChatName, editedMsg :: ByteString, message :: ByteString}
  | UpdateLiveMessage {chatName :: ChatName, chatItemId :: ChatItemId, liveMessage :: Bool, message :: ByteString}
  | APINewGroup UserId GroupProfile
  | NewGroup GroupProfile
  | AddMember GroupName ContactName GroupMemberRole
  | JoinGroup GroupName
  | MemberRole GroupName ContactName GroupMemberRole
  | RemoveMember GroupName ContactName
  | LeaveGroup GroupName
  | DeleteGroup GroupName
  | ClearGroup GroupName
  | ListMembers GroupName
  | ListGroups -- UserId (not used in UI)
  | UpdateGroupNames GroupName GroupProfile
  | ShowGroupProfile GroupName
  | UpdateGroupDescription GroupName (Maybe Text)
  | CreateGroupLink GroupName
  | DeleteGroupLink GroupName
  | ShowGroupLink GroupName
  | SendGroupMessageQuote {groupName :: GroupName, contactName_ :: Maybe ContactName, quotedMsg :: ByteString, message :: ByteString}
  | LastChats (Maybe Int) -- UserId (not used in UI)
  | LastMessages (Maybe ChatName) Int (Maybe String) -- UserId (not used in UI)
  | LastChatItemId (Maybe ChatName) Int -- UserId (not used in UI)
  | ShowChatItem (Maybe ChatItemId) -- UserId (not used in UI)
  | ShowLiveItems Bool
  | SendFile ChatName FilePath
  | SendImage ChatName FilePath
  | ForwardFile ChatName FileTransferId
  | ForwardImage ChatName FileTransferId
  | ReceiveFile {fileId :: FileTransferId, fileInline :: Maybe Bool, filePath :: Maybe FilePath}
  | CancelFile FileTransferId
  | FileStatus FileTransferId
  | ShowProfile -- UserId (not used in UI)
  | UpdateProfile ContactName Text -- UserId (not used in UI)
  | UpdateProfileImage (Maybe ImageData) -- UserId (not used in UI)
  | SetUserFeature AChatFeature FeatureAllowed -- UserId (not used in UI)
  | SetContactFeature AChatFeature ContactName (Maybe FeatureAllowed)
  | SetGroupFeature AGroupFeature GroupName GroupFeatureEnabled
  | SetUserTimedMessages Bool -- UserId (not used in UI)
  | SetContactTimedMessages ContactName (Maybe TimedMessagesEnabled)
  | SetGroupTimedMessages GroupName (Maybe Int)
  | QuitChat
  | ShowVersion
  | DebugLocks
  | GetAgentStats
  | ResetAgentStats
  deriving (Show)

data ChatResponse
  = CRActiveUser {user :: User}
  | CRUsersList {users :: [UserInfo]}
  | CRChatStarted
  | CRChatRunning
  | CRChatStopped
  | CRChatSuspended
  | CRApiChats {user :: User, chats :: [AChat]}
  | CRChats {chats :: [AChat]}
  | CRApiChat {user :: User, chat :: AChat}
  | CRChatItems {user :: User, chatItems :: [AChatItem]}
  | CRChatItemId User (Maybe ChatItemId)
  | CRApiParsedMarkdown {formattedText :: Maybe MarkdownList}
  | CRUserSMPServers {user :: User, smpServers :: NonEmpty ServerCfg, presetSMPServers :: NonEmpty SMPServerWithAuth}
  | CRSmpTestResult {user :: User, smpTestFailure :: Maybe SMPTestFailure}
  | CRChatItemTTL {user :: User, chatItemTTL :: Maybe Int64}
  | CRNetworkConfig {networkConfig :: NetworkConfig}
  | CRContactInfo {user :: User, contact :: Contact, connectionStats :: ConnectionStats, customUserProfile :: Maybe Profile}
  | CRGroupMemberInfo {user :: User, groupInfo :: GroupInfo, member :: GroupMember, connectionStats_ :: Maybe ConnectionStats}
  | CRContactSwitch {user :: User, contact :: Contact, switchProgress :: SwitchProgress}
  | CRGroupMemberSwitch {user :: User, groupInfo :: GroupInfo, member :: GroupMember, switchProgress :: SwitchProgress}
  | CRContactCode {user :: User, contact :: Contact, connectionCode :: Text}
  | CRGroupMemberCode {user :: User, groupInfo :: GroupInfo, member :: GroupMember, connectionCode :: Text}
  | CRConnectionVerified {user :: User, verified :: Bool, expectedCode :: Text}
  | CRNewChatItem {user :: User, chatItem :: AChatItem}
  | CRChatItemStatusUpdated {user :: User, chatItem :: AChatItem}
  | CRChatItemUpdated {user :: User, chatItem :: AChatItem}
  | CRChatItemDeleted {user :: User, deletedChatItem :: AChatItem, toChatItem :: Maybe AChatItem, byUser :: Bool, timed :: Bool}
  | CRChatItemDeletedNotFound {user :: User, contact :: Contact, sharedMsgId :: SharedMsgId}
  | CRBroadcastSent User MsgContent Int ZonedTime
  | CRMsgIntegrityError {user :: User, msgError :: MsgErrorType}
  | CRCmdAccepted {corr :: CorrId}
  | CRCmdOk {user_ :: Maybe User}
  | CRChatHelp {helpSection :: HelpSection}
  | CRWelcome {user :: User}
  | CRGroupCreated {user :: User, groupInfo :: GroupInfo}
  | CRGroupMembers {user :: User, group :: Group}
  | CRContactsList {user :: User, contacts :: [Contact]}
  | CRUserContactLink {user :: User, contactLink :: UserContactLink}
  | CRUserContactLinkUpdated {user :: User, contactLink :: UserContactLink}
  | CRContactRequestRejected {user :: User, contactRequest :: UserContactRequest}
  | CRUserAcceptedGroupSent {user :: User, groupInfo :: GroupInfo, hostContact :: Maybe Contact}
  | CRUserDeletedMember {user :: User, groupInfo :: GroupInfo, member :: GroupMember}
  | CRGroupsList {user :: User, groups :: [GroupInfo]}
  | CRSentGroupInvitation {user :: User, groupInfo :: GroupInfo, contact :: Contact, member :: GroupMember}
  | CRFileTransferStatus User (FileTransfer, [Integer]) -- TODO refactor this type to FileTransferStatus
  | CRUserProfile {user :: User, profile :: Profile}
  | CRUserProfileNoChange {user :: User}
  | CRVersionInfo {versionInfo :: CoreVersionInfo}
  | CRInvitation {user :: User, connReqInvitation :: ConnReqInvitation}
  | CRSentConfirmation {user :: User}
  | CRSentInvitation {user :: User, customUserProfile :: Maybe Profile}
  | CRContactUpdated {user :: User, fromContact :: Contact, toContact :: Contact}
  | CRContactsMerged {user :: User, intoContact :: Contact, mergedContact :: Contact}
  | CRContactDeleted {user :: User, contact :: Contact}
  | CRChatCleared {user :: User, chatInfo :: AChatInfo}
  | CRUserContactLinkCreated {user :: User, connReqContact :: ConnReqContact}
  | CRUserContactLinkDeleted {user :: User}
  | CRReceivedContactRequest {user :: User, contactRequest :: UserContactRequest}
  | CRAcceptingContactRequest {user :: User, contact :: Contact}
  | CRContactAlreadyExists {user :: User, contact :: Contact}
  | CRContactRequestAlreadyAccepted {user :: User, contact :: Contact}
  | CRLeftMemberUser {user :: User, groupInfo :: GroupInfo}
  | CRGroupDeletedUser {user :: User, groupInfo :: GroupInfo}
  | CRRcvFileAccepted {user :: User, chatItem :: AChatItem}
  | CRRcvFileAcceptedSndCancelled {user :: User, rcvFileTransfer :: RcvFileTransfer}
  | CRRcvFileStart {user :: User, chatItem :: AChatItem}
  | CRRcvFileComplete {user :: User, chatItem :: AChatItem}
  | CRRcvFileCancelled {user :: User, rcvFileTransfer :: RcvFileTransfer}
  | CRRcvFileSndCancelled {user :: User, rcvFileTransfer :: RcvFileTransfer}
  | CRSndFileStart {user :: User, chatItem :: AChatItem, sndFileTransfer :: SndFileTransfer}
  | CRSndFileComplete {user :: User, chatItem :: AChatItem, sndFileTransfer :: SndFileTransfer}
  | CRSndFileCancelled {chatItem :: AChatItem, sndFileTransfer :: SndFileTransfer}
  | CRSndFileRcvCancelled {user :: User, chatItem :: AChatItem, sndFileTransfer :: SndFileTransfer}
  | CRSndGroupFileCancelled {user :: User, chatItem :: AChatItem, fileTransferMeta :: FileTransferMeta, sndFileTransfers :: [SndFileTransfer]}
  | CRUserProfileUpdated {user :: User, fromProfile :: Profile, toProfile :: Profile}
  | CRContactAliasUpdated {user :: User, toContact :: Contact}
  | CRConnectionAliasUpdated {user :: User, toConnection :: PendingContactConnection}
  | CRContactPrefsUpdated {user :: User, fromContact :: Contact, toContact :: Contact}
  | CRContactConnecting {user :: User, contact :: Contact}
  | CRContactConnected {user :: User, contact :: Contact, userCustomProfile :: Maybe Profile}
  | CRContactAnotherClient {user :: User, contact :: Contact}
  | CRSubscriptionEnd {user :: User, connectionEntity :: ConnectionEntity}
  | CRContactsDisconnected {server :: SMPServer, contactRefs :: [ContactRef]}
  | CRContactsSubscribed {server :: SMPServer, contactRefs :: [ContactRef]}
  | CRContactSubError {user :: User, contact :: Contact, chatError :: ChatError}
  | CRContactSubSummary {user :: User, contactSubscriptions :: [ContactSubStatus]}
  | CRUserContactSubSummary {user :: User, userContactSubscriptions :: [UserContactSubStatus]}
  | CRHostConnected {protocol :: AProtocolType, transportHost :: TransportHost}
  | CRHostDisconnected {protocol :: AProtocolType, transportHost :: TransportHost}
  | CRGroupInvitation {user :: User, groupInfo :: GroupInfo}
  | CRReceivedGroupInvitation {user :: User, groupInfo :: GroupInfo, contact :: Contact, memberRole :: GroupMemberRole}
  | CRUserJoinedGroup {user :: User, groupInfo :: GroupInfo, hostMember :: GroupMember}
  | CRJoinedGroupMember {user :: User, groupInfo :: GroupInfo, member :: GroupMember}
  | CRJoinedGroupMemberConnecting {user :: User, groupInfo :: GroupInfo, hostMember :: GroupMember, member :: GroupMember}
  | CRMemberRole {user :: User, groupInfo :: GroupInfo, byMember :: GroupMember, member :: GroupMember, fromRole :: GroupMemberRole, toRole :: GroupMemberRole}
  | CRMemberRoleUser {user :: User, groupInfo :: GroupInfo, member :: GroupMember, fromRole :: GroupMemberRole, toRole :: GroupMemberRole}
  | CRConnectedToGroupMember {user :: User, groupInfo :: GroupInfo, member :: GroupMember}
  | CRDeletedMember {user :: User, groupInfo :: GroupInfo, byMember :: GroupMember, deletedMember :: GroupMember}
  | CRDeletedMemberUser {user :: User, groupInfo :: GroupInfo, member :: GroupMember}
  | CRLeftMember {user :: User, groupInfo :: GroupInfo, member :: GroupMember}
  | CRGroupEmpty {user :: User, groupInfo :: GroupInfo}
  | CRGroupRemoved {user :: User, groupInfo :: GroupInfo}
  | CRGroupDeleted {user :: User, groupInfo :: GroupInfo, member :: GroupMember}
  | CRGroupUpdated {user :: User, fromGroup :: GroupInfo, toGroup :: GroupInfo, member_ :: Maybe GroupMember}
  | CRGroupProfile {user :: User, groupInfo :: GroupInfo}
  | CRGroupLinkCreated {user :: User, groupInfo :: GroupInfo, connReqContact :: ConnReqContact}
  | CRGroupLink {user :: User, groupInfo :: GroupInfo, connReqContact :: ConnReqContact}
  | CRGroupLinkDeleted {user :: User, groupInfo :: GroupInfo}
  | CRAcceptingGroupJoinRequest {user :: User, groupInfo :: GroupInfo, contact :: Contact}
  | CRMemberSubError {user :: User, groupInfo :: GroupInfo, member :: GroupMember, chatError :: ChatError}
  | CRMemberSubSummary {user :: User, memberSubscriptions :: [MemberSubStatus]}
  | CRGroupSubscribed {user :: User, groupInfo :: GroupInfo}
  | CRPendingSubSummary {user :: User, pendingSubscriptions :: [PendingSubStatus]}
  | CRSndFileSubError {user :: User, sndFileTransfer :: SndFileTransfer, chatError :: ChatError}
  | CRRcvFileSubError {user :: User, rcvFileTransfer :: RcvFileTransfer, chatError :: ChatError}
  | CRCallInvitation {callInvitation :: RcvCallInvitation}
  | CRCallOffer {user :: User, contact :: Contact, callType :: CallType, offer :: WebRTCSession, sharedKey :: Maybe C.Key, askConfirmation :: Bool}
  | CRCallAnswer {user :: User, contact :: Contact, answer :: WebRTCSession}
  | CRCallExtraInfo {user :: User, contact :: Contact, extraInfo :: WebRTCExtraInfo}
  | CRCallEnded {user :: User, contact :: Contact}
  | CRCallInvitations {callInvitations :: [RcvCallInvitation]}
  | CRUserContactLinkSubscribed -- TODO delete
  | CRUserContactLinkSubError {chatError :: ChatError} -- TODO delete
  | CRNtfTokenStatus {status :: NtfTknStatus}
  | CRNtfToken {token :: DeviceToken, status :: NtfTknStatus, ntfMode :: NotificationsMode}
  | CRNtfMessages {user_ :: Maybe User, connEntity :: Maybe ConnectionEntity, msgTs :: Maybe UTCTime, ntfMessages :: [NtfMsgInfo]}
  | CRNewContactConnection {user :: User, connection :: PendingContactConnection}
  | CRContactConnectionDeleted {user :: User, connection :: PendingContactConnection}
  | CRSQLResult {rows :: [Text]}
  | CRDebugLocks {chatLockName :: Maybe String, agentLocks :: AgentLocks}
  | CRAgentStats {agentStats :: [[String]]}
  | CRConnectionDisabled {connectionEntity :: ConnectionEntity}
  | CRAgentRcvQueueDeleted {agentConnId :: AgentConnId, server :: SMPServer, agentQueueId :: AgentQueueId, agentError_ :: Maybe AgentErrorType}
  | CRAgentConnDeleted {agentConnId :: AgentConnId}
  | CRAgentUserDeleted {agentUserId :: Int64}
  | CRMessageError {user :: User, severity :: Text, errorMessage :: Text}
  | CRChatCmdError {user_ :: Maybe User, chatError :: ChatError}
  | CRChatError {user_ :: Maybe User, chatError :: ChatError}
  deriving (Show, Generic)

instance ToJSON ChatResponse where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "CR"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "CR"

newtype AgentQueueId = AgentQueueId QueueId
  deriving (Eq, Show)

instance StrEncoding AgentQueueId where
  strEncode (AgentQueueId qId) = strEncode qId
  strDecode s = AgentQueueId <$> strDecode s
  strP = AgentQueueId <$> strP

instance ToJSON AgentQueueId where
  toJSON = strToJSON
  toEncoding = strToJEncoding

data SMPServersConfig = SMPServersConfig {smpServers :: [ServerCfg]}
  deriving (Show, Generic, FromJSON)

data ArchiveConfig = ArchiveConfig {archivePath :: FilePath, disableCompression :: Maybe Bool, parentTempDirectory :: Maybe FilePath}
  deriving (Show, Generic, FromJSON)

data DBEncryptionConfig = DBEncryptionConfig {currentKey :: DBEncryptionKey, newKey :: DBEncryptionKey}
  deriving (Show, Generic, FromJSON)

newtype DBEncryptionKey = DBEncryptionKey String
  deriving (Show)

instance IsString DBEncryptionKey where fromString = parseString $ parseAll strP

instance StrEncoding DBEncryptionKey where
  strEncode (DBEncryptionKey s) = B.pack s
  strP = DBEncryptionKey . B.unpack <$> A.takeWhile (\c -> c /= ' ' && ord c >= 0x21 && ord c <= 0x7E)

instance FromJSON DBEncryptionKey where
  parseJSON = strParseJSON "DBEncryptionKey"

data ContactSubStatus = ContactSubStatus
  { contact :: Contact,
    contactError :: Maybe ChatError
  }
  deriving (Show, Generic)

instance ToJSON ContactSubStatus where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

data MemberSubStatus = MemberSubStatus
  { member :: GroupMember,
    memberError :: Maybe ChatError
  }
  deriving (Show, Generic)

instance ToJSON MemberSubStatus where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

data UserContactSubStatus = UserContactSubStatus
  { userContact :: UserContact,
    userContactError :: Maybe ChatError
  }
  deriving (Show, Generic)

instance ToJSON UserContactSubStatus where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

data PendingSubStatus = PendingSubStatus
  { connection :: PendingContactConnection,
    connError :: Maybe ChatError
  }
  deriving (Show, Generic)

instance ToJSON PendingSubStatus where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

data ComposedMessage = ComposedMessage
  { filePath :: Maybe FilePath,
    quotedItemId :: Maybe ChatItemId,
    msgContent :: MsgContent
  }
  deriving (Show, Generic, FromJSON)

data NtfMsgInfo = NtfMsgInfo {msgTs :: UTCTime, msgFlags :: MsgFlags}
  deriving (Show, Generic)

instance ToJSON NtfMsgInfo where toEncoding = J.genericToEncoding J.defaultOptions

crNtfToken :: (DeviceToken, NtfTknStatus, NotificationsMode) -> ChatResponse
crNtfToken (token, status, ntfMode) = CRNtfToken {token, status, ntfMode}

data SwitchProgress = SwitchProgress
  { queueDirection :: QueueDirection,
    switchPhase :: SwitchPhase,
    connectionStats :: ConnectionStats
  }
  deriving (Show, Generic)

instance ToJSON SwitchProgress where toEncoding = J.genericToEncoding J.defaultOptions

data ParsedServerAddress = ParsedServerAddress
  { serverAddress :: Maybe ServerAddress,
    parseError :: String
  }
  deriving (Show, Generic)

instance ToJSON ParsedServerAddress where toEncoding = J.genericToEncoding J.defaultOptions

data ServerAddress = ServerAddress
  { hostnames :: NonEmpty String,
    port :: String,
    keyHash :: String,
    basicAuth :: String
  }
  deriving (Show, Generic)

instance ToJSON ServerAddress where toEncoding = J.genericToEncoding J.defaultOptions

data TimedMessagesEnabled
  = TMEEnableSetTTL Int
  | TMEEnableKeepTTL
  | TMEDisableKeepTTL
  deriving (Show)

tmeToPref :: Maybe Int -> TimedMessagesEnabled -> TimedMessagesPreference
tmeToPref currentTTL tme = uncurry TimedMessagesPreference $ case tme of
  TMEEnableSetTTL ttl -> (FAYes, Just ttl)
  TMEEnableKeepTTL -> (FAYes, currentTTL)
  TMEDisableKeepTTL -> (FANo, currentTTL)

data ChatLogLevel = CLLDebug | CLLInfo | CLLWarning | CLLError | CLLImportant
  deriving (Eq, Ord, Show)

data CoreVersionInfo = CoreVersionInfo
  { version :: String,
    buildTimestamp :: String,
    simplexmqVersion :: String,
    simplexmqCommit :: String
  }
  deriving (Show, Generic)

instance ToJSON CoreVersionInfo where toEncoding = J.genericToEncoding J.defaultOptions

data ChatError
  = ChatError {errorType :: ChatErrorType}
  | ChatErrorAgent {agentError :: AgentErrorType, connectionEntity_ :: Maybe ConnectionEntity}
  | ChatErrorStore {storeError :: StoreError}
  | ChatErrorDatabase {databaseError :: DatabaseError}
  deriving (Show, Exception, Generic)

instance ToJSON ChatError where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "Chat"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "Chat"

data ChatErrorType
  = CENoActiveUser
  | CENoConnectionUser {agentConnId :: AgentConnId}
  | CEActiveUserExists -- TODO delete
  | CEUserExists {contactName :: ContactName}
  | CEDifferentActiveUser {commandUserId :: UserId, activeUserId :: UserId}
  | CECantDeleteActiveUser {userId :: UserId}
  | CECantDeleteLastUser {userId :: UserId}
  | CEChatNotStarted
  | CEChatNotStopped
  | CEChatStoreChanged
  | CEInvalidConnReq
  | CEInvalidChatMessage {message :: String}
  | CEContactNotReady {contact :: Contact}
  | CEContactDisabled {contact :: Contact}
  | CEConnectionDisabled {connection :: Connection}
  | CEGroupUserRole {requiredRole :: GroupMemberRole}
  | CEContactIncognitoCantInvite
  | CEGroupIncognitoCantInvite
  | CEGroupContactRole {contactName :: ContactName}
  | CEGroupDuplicateMember {contactName :: ContactName}
  | CEGroupDuplicateMemberId
  | CEGroupNotJoined {groupInfo :: GroupInfo}
  | CEGroupMemberNotActive
  | CEGroupMemberUserRemoved
  | CEGroupMemberNotFound
  | CEGroupMemberIntroNotFound {contactName :: ContactName}
  | CEGroupCantResendInvitation {groupInfo :: GroupInfo, contactName :: ContactName}
  | CEGroupInternal {message :: String}
  | CEFileNotFound {message :: String}
  | CEFileAlreadyReceiving {message :: String}
  | CEFileCancelled {message :: String}
  | CEFileAlreadyExists {filePath :: FilePath}
  | CEFileRead {filePath :: FilePath, message :: String}
  | CEFileWrite {filePath :: FilePath, message :: String}
  | CEFileSend {fileId :: FileTransferId, agentError :: AgentErrorType}
  | CEFileRcvChunk {message :: String}
  | CEFileInternal {message :: String}
  | CEFileImageType {filePath :: FilePath}
  | CEFileImageSize {filePath :: FilePath}
  | CEFileNotReceived {fileId :: FileTransferId}
  | CEInlineFileProhibited {fileId :: FileTransferId}
  | CEInvalidQuote
  | CEInvalidChatItemUpdate
  | CEInvalidChatItemDelete
  | CEHasCurrentCall
  | CENoCurrentCall
  | CECallContact {contactId :: Int64}
  | CECallState {currentCallState :: CallStateTag}
  | CEDirectMessagesProhibited {direction :: MsgDirection, contact :: Contact}
  | CEAgentVersion
  | CEAgentNoSubResult {agentConnId :: AgentConnId}
  | CECommandError {message :: String}
  | CEAgentCommandError {message :: String}
  | CEInternalError {message :: String}
  deriving (Show, Exception, Generic)

instance ToJSON ChatErrorType where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "CE"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "CE"

data DatabaseError
  = DBErrorEncrypted
  | DBErrorPlaintext
  | DBErrorNoFile {dbFile :: String}
  | DBErrorExport {sqliteError :: SQLiteError}
  | DBErrorOpen {sqliteError :: SQLiteError}
  deriving (Show, Exception, Generic)

instance ToJSON DatabaseError where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "DB"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "DB"

data SQLiteError = SQLiteErrorNotADatabase | SQLiteError String
  deriving (Show, Exception, Generic)

instance ToJSON SQLiteError where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "SQLite"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "SQLite"

throwDBError :: ChatMonad m => DatabaseError -> m ()
throwDBError = throwError . ChatErrorDatabase

type ChatMonad m = (MonadUnliftIO m, MonadReader ChatController m, MonadError ChatError m)

chatCmdError :: Maybe User -> String -> ChatResponse
chatCmdError user = CRChatCmdError user . ChatError . CECommandError

setActive :: (MonadUnliftIO m, MonadReader ChatController m) => ActiveTo -> m ()
setActive to = asks activeTo >>= atomically . (`writeTVar` to)

unsetActive :: (MonadUnliftIO m, MonadReader ChatController m) => ActiveTo -> m ()
unsetActive a = asks activeTo >>= atomically . (`modifyTVar` unset)
  where
    unset a' = if a == a' then ActiveNone else a'
