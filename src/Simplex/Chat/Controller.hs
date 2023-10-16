{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Simplex.Chat.Controller where

import Control.Concurrent (ThreadId)
import Control.Concurrent.Async (Async)
import Control.Exception
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Crypto.Random (ChaChaDRG)
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?))
import qualified Data.Aeson as J
import qualified Data.Aeson.TH  as JQ
import qualified Data.Aeson.Types as JT
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (ord)
import Data.Constraint (Dict (..))
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.String
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime)
import Data.Version (showVersion)
import GHC.Generics (Generic)
import Language.Haskell.TH (Exp, Q, runIO)
import Numeric.Natural
import qualified Paths_simplex_chat as SC
import Simplex.Chat.Call
import Simplex.Chat.Markdown (MarkdownList)
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Protocol
import Simplex.Chat.Remote.Types
import Simplex.Chat.Store (AutoAccept, StoreError (..), UserContactLink, UserMsgReceiptSettings)
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Messaging.Agent (AgentClient, SubscriptionsInfo)
import Simplex.Messaging.Agent.Client (AgentLocks, ProtocolTestFailure)
import Simplex.Messaging.Agent.Env.SQLite (AgentConfig, NetworkConfig)
import Simplex.Messaging.Agent.Lock
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Agent.Store.SQLite (MigrationConfirmation, SQLiteStore, UpMigration, withTransaction)
import Simplex.Messaging.Agent.Store.SQLite.DB (SlowQueryStats (..))
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.File (CryptoFile (..))
import qualified Simplex.Messaging.Crypto.File as CF
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Notifications.Protocol (DeviceToken (..), NtfTknStatus)
import Simplex.Messaging.Parsers (dropPrefix, enumJSON, parseAll, parseString, sumTypeJSON)
import Simplex.Messaging.Protocol (AProtoServerWithAuth, AProtocolType (..), CorrId, MsgFlags, NtfServer, ProtoServerWithAuth, ProtocolTypeI, QueueId, SProtocolType, SubscriptionMode (..), UserProtocol, XFTPServerWithAuth, userProtocol)
import Simplex.Messaging.TMap (TMap)
import Simplex.Messaging.Transport (simplexMQVersion)
import Simplex.Messaging.Transport.Client (TransportHost)
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Client)
import Simplex.Messaging.Util (allFinally, catchAllErrors, liftEitherError, tryAllErrors, (<$$>))
import Simplex.Messaging.Version
import System.IO (Handle)
import System.Mem.Weak (Weak)
import UnliftIO.STM

versionNumber :: String
versionNumber = showVersion SC.version

versionString :: String -> String
versionString ver = "SimpleX Chat v" <> ver

updateStr :: String
updateStr = "To update run: curl -o- https://raw.githubusercontent.com/simplex-chat/simplex-chat/master/install.sh | bash"

simplexmqCommitQ :: Q Exp
simplexmqCommitQ = do
  s <- either (const "") B.unpack . A.parseOnly commitHashP <$> runIO (B.readFile "./cabal.project")
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

coreVersionInfo :: String -> CoreVersionInfo
coreVersionInfo simplexmqCommit =
  CoreVersionInfo
    { version = versionNumber,
      simplexmqVersion = simplexMQVersion,
      simplexmqCommit
    }

data ChatConfig = ChatConfig
  { agentConfig :: AgentConfig,
    chatVRange :: VersionRange,
    confirmMigrations :: MigrationConfirmation,
    defaultServers :: DefaultAgentServers,
    tbqSize :: Natural,
    fileChunkSize :: Integer,
    xftpDescrPartSize :: Int,
    inlineFiles :: InlineFilesConfig,
    autoAcceptFileSize :: Integer,
    xftpFileConfig :: Maybe XFTPFileConfig, -- Nothing - XFTP is disabled
    tempDir :: Maybe FilePath,
    showReactions :: Bool,
    showReceipts :: Bool,
    subscriptionEvents :: Bool,
    hostEvents :: Bool,
    logLevel :: ChatLogLevel,
    testView :: Bool,
    initialCleanupManagerDelay :: Int64,
    cleanupManagerInterval :: NominalDiffTime,
    cleanupManagerStepDelay :: Int64,
    ciExpirationInterval :: Int64, -- microseconds
    coreApi :: Bool
  }

data DefaultAgentServers = DefaultAgentServers
  { smp :: NonEmpty SMPServerWithAuth,
    ntf :: [NtfServer],
    xftp :: NonEmpty XFTPServerWithAuth,
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

data ChatDatabase = ChatDatabase {chatStore :: SQLiteStore, agentStore :: SQLiteStore}

data ChatController = ChatController
  { currentUser :: TVar (Maybe User),
    currentRemoteHost :: TVar (Maybe RemoteHostId),
    firstTime :: Bool,
    smpAgent :: AgentClient,
    agentAsync :: TVar (Maybe (Async (), Maybe (Async ()))),
    chatStore :: SQLiteStore,
    chatStoreChanged :: TVar Bool, -- if True, chat should be fully restarted
    idsDrg :: TVar ChaChaDRG,
    inputQ :: TBQueue String,
    outputQ :: TBQueue (Maybe CorrId, Maybe RemoteHostId, ChatResponse),
    connNetworkStatuses :: TMap AgentConnId NetworkStatus,
    subscriptionMode :: TVar SubscriptionMode,
    chatLock :: Lock,
    sndFiles :: TVar (Map Int64 Handle),
    rcvFiles :: TVar (Map Int64 Handle),
    currentCalls :: TMap ContactId Call,
    localDeviceName :: TVar Text,
    remoteHostSessions :: TMap RemoteHostId RemoteHostSession, -- All the active remote hosts
    remoteCtrlSession :: TVar (Maybe RemoteCtrlSession), -- Supervisor process for hosted controllers
    config :: ChatConfig,
    filesFolder :: TVar (Maybe FilePath), -- path to files folder for mobile apps,
    expireCIThreads :: TMap UserId (Maybe (Async ())),
    expireCIFlags :: TMap UserId Bool,
    cleanupManagerAsync :: TVar (Maybe (Async ())),
    timedItemThreads :: TMap (ChatRef, ChatItemId) (TVar (Maybe (Weak ThreadId))),
    showLiveItems :: TVar Bool,
    encryptLocalFiles :: TVar Bool,
    userXFTPFileConfig :: TVar (Maybe XFTPFileConfig),
    tempDirectory :: TVar (Maybe FilePath),
    logFilePath :: Maybe FilePath,
    contactMergeEnabled :: TVar Bool
  }

data HelpSection = HSMain | HSFiles | HSGroups | HSContacts | HSMyAddress | HSIncognito | HSMarkdown | HSMessages | HSSettings | HSDatabase
  deriving (Show, Generic)

instance FromJSON HelpSection where
  parseJSON = J.genericParseJSON . enumJSON $ dropPrefix "HS"

instance ToJSON HelpSection where
  toJSON = J.genericToJSON . enumJSON $ dropPrefix "HS"
  toEncoding = J.genericToEncoding . enumJSON $ dropPrefix "HS"

data ChatCommand
  = ShowActiveUser
  | CreateActiveUser NewUser
  | ListUsers
  | APISetActiveUser UserId (Maybe UserPwd)
  | SetActiveUser UserName (Maybe UserPwd)
  | SetAllContactReceipts Bool
  | APISetUserContactReceipts UserId UserMsgReceiptSettings
  | SetUserContactReceipts UserMsgReceiptSettings
  | APISetUserGroupReceipts UserId UserMsgReceiptSettings
  | SetUserGroupReceipts UserMsgReceiptSettings
  | APIHideUser UserId UserPwd
  | APIUnhideUser UserId UserPwd
  | APIMuteUser UserId
  | APIUnmuteUser UserId
  | HideUser UserPwd
  | UnhideUser UserPwd
  | MuteUser
  | UnmuteUser
  | APIDeleteUser UserId Bool (Maybe UserPwd)
  | DeleteUser UserName Bool (Maybe UserPwd)
  | StartChat {subscribeConnections :: Bool, enableExpireChatItems :: Bool, startXFTPWorkers :: Bool}
  | APIStopChat
  | APIActivateChat
  | APISuspendChat {suspendTimeout :: Int}
  | ResubscribeAllConnections
  | SetTempFolder FilePath
  | SetFilesFolder FilePath
  | APISetXFTPConfig (Maybe XFTPFileConfig)
  | APISetEncryptLocalFiles Bool
  | SetContactMergeEnabled Bool
  | APIExportArchive ArchiveConfig
  | ExportArchive
  | APIImportArchive ArchiveConfig
  | APIDeleteStorage
  | APIStorageEncryption DBEncryptionConfig
  | ExecChatStoreSQL Text
  | ExecAgentStoreSQL Text
  | SlowSQLQueries
  | APIGetChats {userId :: UserId, pendingConnections :: Bool}
  | APIGetChat ChatRef ChatPagination (Maybe String)
  | APIGetChatItems ChatPagination (Maybe String)
  | APIGetChatItemInfo ChatRef ChatItemId
  | APISendMessage {chatRef :: ChatRef, liveMessage :: Bool, ttl :: Maybe Int, composedMessage :: ComposedMessage}
  | APIUpdateChatItem {chatRef :: ChatRef, chatItemId :: ChatItemId, liveMessage :: Bool, msgContent :: MsgContent}
  | APIDeleteChatItem ChatRef ChatItemId CIDeleteMode
  | APIDeleteMemberChatItem GroupId GroupMemberId ChatItemId
  | APIChatItemReaction {chatRef :: ChatRef, chatItemId :: ChatItemId, add :: Bool, reaction :: MsgReaction}
  | APIChatRead ChatRef (Maybe (ChatItemId, ChatItemId))
  | APIChatUnread ChatRef Bool
  | APIDeleteChat ChatRef Bool -- `notify` flag is only applied to direct chats
  | APIClearChat ChatRef
  | APIAcceptContact IncognitoEnabled Int64
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
  | APIGetNetworkStatuses
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
  | APICreateGroupLink GroupId GroupMemberRole
  | APIGroupLinkMemberRole GroupId GroupMemberRole
  | APIDeleteGroupLink GroupId
  | APIGetGroupLink GroupId
  | APICreateMemberContact GroupId GroupMemberId
  | APISendMemberContactInvitation {contactId :: ContactId, msgContent_ :: Maybe MsgContent}
  | APIGetUserProtoServers UserId AProtocolType
  | GetUserProtoServers AProtocolType
  | APISetUserProtoServers UserId AProtoServersConfig
  | SetUserProtoServers AProtoServersConfig
  | APITestProtoServer UserId AProtoServerWithAuth
  | TestProtoServer AProtoServerWithAuth
  | APISetChatItemTTL UserId (Maybe Int64)
  | SetChatItemTTL (Maybe Int64)
  | APIGetChatItemTTL UserId
  | GetChatItemTTL
  | APISetNetworkConfig NetworkConfig
  | APIGetNetworkConfig
  | ReconnectAllServers
  | APISetChatSettings ChatRef ChatSettings
  | APISetMemberSettings GroupId GroupMemberId GroupMemberSettings
  | APIContactInfo ContactId
  | APIGroupInfo GroupId
  | APIGroupMemberInfo GroupId GroupMemberId
  | APISwitchContact ContactId
  | APISwitchGroupMember GroupId GroupMemberId
  | APIAbortSwitchContact ContactId
  | APIAbortSwitchGroupMember GroupId GroupMemberId
  | APISyncContactRatchet ContactId Bool
  | APISyncGroupMemberRatchet GroupId GroupMemberId Bool
  | APIGetContactCode ContactId
  | APIGetGroupMemberCode GroupId GroupMemberId
  | APIVerifyContact ContactId (Maybe Text)
  | APIVerifyGroupMember GroupId GroupMemberId (Maybe Text)
  | APIEnableContact ContactId
  | APIEnableGroupMember GroupId GroupMemberId
  | SetShowMessages ChatName MsgFilter
  | SetSendReceipts ChatName (Maybe Bool)
  | SetShowMemberMessages GroupName ContactName Bool
  | ContactInfo ContactName
  | ShowGroupInfo GroupName
  | GroupMemberInfo GroupName ContactName
  | SwitchContact ContactName
  | SwitchGroupMember GroupName ContactName
  | AbortSwitchContact ContactName
  | AbortSwitchGroupMember GroupName ContactName
  | SyncContactRatchet ContactName Bool
  | SyncGroupMemberRatchet GroupName ContactName Bool
  | GetContactCode ContactName
  | GetGroupMemberCode GroupName ContactName
  | VerifyContact ContactName (Maybe Text)
  | VerifyGroupMember GroupName ContactName (Maybe Text)
  | EnableContact ContactName
  | EnableGroupMember GroupName ContactName
  | ChatHelp HelpSection
  | Welcome
  | APIAddContact UserId IncognitoEnabled
  | AddContact IncognitoEnabled
  | APISetConnectionIncognito Int64 IncognitoEnabled
  | APIConnectPlan UserId AConnectionRequestUri
  | APIConnect UserId IncognitoEnabled (Maybe AConnectionRequestUri)
  | Connect IncognitoEnabled (Maybe AConnectionRequestUri)
  | ConnectSimplex IncognitoEnabled -- UserId (not used in UI)
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
  | APISetProfileAddress UserId Bool
  | SetProfileAddress Bool
  | APIAddressAutoAccept UserId (Maybe AutoAccept)
  | AddressAutoAccept (Maybe AutoAccept)
  | AcceptContact IncognitoEnabled ContactName
  | RejectContact ContactName
  | SendMessage ChatName Text
  | SendMemberContactMessage GroupName ContactName Text
  | SendLiveMessage ChatName Text
  | SendMessageQuote {contactName :: ContactName, msgDir :: AMsgDirection, quotedMsg :: Text, message :: Text}
  | SendMessageBroadcast Text -- UserId (not used in UI)
  | DeleteMessage ChatName Text
  | DeleteMemberMessage GroupName ContactName Text
  | EditMessage {chatName :: ChatName, editedMsg :: Text, message :: Text}
  | UpdateLiveMessage {chatName :: ChatName, chatItemId :: ChatItemId, liveMessage :: Bool, message :: Text}
  | ReactToMessage {add :: Bool, reaction :: MsgReaction, chatName :: ChatName, reactToMessage :: Text}
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
  | APIListGroups UserId (Maybe ContactId) (Maybe String)
  | ListGroups (Maybe ContactName) (Maybe String)
  | UpdateGroupNames GroupName GroupProfile
  | ShowGroupProfile GroupName
  | UpdateGroupDescription GroupName (Maybe Text)
  | ShowGroupDescription GroupName
  | CreateGroupLink GroupName GroupMemberRole
  | GroupLinkMemberRole GroupName GroupMemberRole
  | DeleteGroupLink GroupName
  | ShowGroupLink GroupName
  | SendGroupMessageQuote {groupName :: GroupName, contactName_ :: Maybe ContactName, quotedMsg :: Text, message :: Text}
  | LastChats (Maybe Int) -- UserId (not used in UI)
  | LastMessages (Maybe ChatName) Int (Maybe String) -- UserId (not used in UI)
  | LastChatItemId (Maybe ChatName) Int -- UserId (not used in UI)
  | ShowChatItem (Maybe ChatItemId) -- UserId (not used in UI)
  | ShowChatItemInfo ChatName Text
  | ShowLiveItems Bool
  | SendFile ChatName FilePath
  | SendImage ChatName FilePath
  | ForwardFile ChatName FileTransferId
  | ForwardImage ChatName FileTransferId
  | SendFileDescription ChatName FilePath
  | ReceiveFile {fileId :: FileTransferId, storeEncrypted :: Maybe Bool, fileInline :: Maybe Bool, filePath :: Maybe FilePath}
  | SetFileToReceive {fileId :: FileTransferId, storeEncrypted :: Maybe Bool}
  | CancelFile FileTransferId
  | FileStatus FileTransferId
  | ShowProfile -- UserId (not used in UI)
  | UpdateProfile ContactName Text -- UserId (not used in UI)
  | UpdateProfileImage (Maybe ImageData) -- UserId (not used in UI)
  | ShowProfileImage
  | SetUserFeature AChatFeature FeatureAllowed -- UserId (not used in UI)
  | SetContactFeature AChatFeature ContactName (Maybe FeatureAllowed)
  | SetGroupFeature AGroupFeature GroupName GroupFeatureEnabled
  | SetUserTimedMessages Bool -- UserId (not used in UI)
  | SetContactTimedMessages ContactName (Maybe TimedMessagesEnabled)
  | SetGroupTimedMessages GroupName (Maybe Int)
  | SetLocalDeviceName Text
  | CreateRemoteHost -- ^ Configure a new remote host
  | ListRemoteHosts
  | StartRemoteHost RemoteHostId -- ^ Start and announce a remote host
  -- | SwitchRemoteHost (Maybe RemoteHostId) -- ^ Switch current remote host
  | StopRemoteHost RemoteHostId -- ^ Shut down a running session
  | DeleteRemoteHost RemoteHostId -- ^ Unregister remote host and remove its data
  | StartRemoteCtrl -- ^ Start listening for announcements from all registered controllers
  | RegisterRemoteCtrl RemoteCtrlOOB -- ^ Register OOB data for satellite discovery and handshake
  | ListRemoteCtrls
  | AcceptRemoteCtrl RemoteCtrlId -- ^ Accept discovered data and store confirmation
  | RejectRemoteCtrl RemoteCtrlId -- ^ Reject and blacklist discovered data
  | StopRemoteCtrl -- ^ Stop listening for announcements or terminate an active session
  | DeleteRemoteCtrl RemoteCtrlId -- ^ Remove all local data associated with a satellite session
  | QuitChat
  | ShowVersion
  | DebugLocks
  | GetAgentStats
  | ResetAgentStats
  | GetAgentSubs
  | GetAgentSubsDetails
  deriving (Show)

allowRemoteCommand :: ChatCommand -> Bool -- XXX: consider using Relay/Block/ForceLocal
allowRemoteCommand = \case
  StartChat {} -> False
  APIStopChat -> False
  APIActivateChat -> False
  APISuspendChat {} -> False
  SetTempFolder {} -> False
  QuitChat -> False
  CreateRemoteHost -> False
  ListRemoteHosts -> False
  StartRemoteHost {} -> False
  -- SwitchRemoteHost {} -> False
  StopRemoteHost {} -> False
  DeleteRemoteHost {} -> False
  RegisterRemoteCtrl {} -> False
  StartRemoteCtrl -> False
  ListRemoteCtrls -> False
  AcceptRemoteCtrl {} -> False
  RejectRemoteCtrl {} -> False
  StopRemoteCtrl -> False
  DeleteRemoteCtrl {} -> False
  _ -> True

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
  | CRChatItems {user :: User, chatName_ :: Maybe ChatName, chatItems :: [AChatItem]}
  | CRChatItemInfo {user :: User, chatItem :: AChatItem, chatItemInfo :: ChatItemInfo}
  | CRChatItemId User (Maybe ChatItemId)
  | CRApiParsedMarkdown {formattedText :: Maybe MarkdownList}
  | CRUserProtoServers {user :: User, servers :: AUserProtoServers}
  | CRServerTestResult {user :: User, testServer :: AProtoServerWithAuth, testFailure :: Maybe ProtocolTestFailure}
  | CRChatItemTTL {user :: User, chatItemTTL :: Maybe Int64}
  | CRNetworkConfig {networkConfig :: NetworkConfig}
  | CRContactInfo {user :: User, contact :: Contact, connectionStats :: ConnectionStats, customUserProfile :: Maybe Profile}
  | CRGroupInfo {user :: User, groupInfo :: GroupInfo, groupSummary :: GroupSummary}
  | CRGroupMemberInfo {user :: User, groupInfo :: GroupInfo, member :: GroupMember, connectionStats_ :: Maybe ConnectionStats}
  | CRContactSwitchStarted {user :: User, contact :: Contact, connectionStats :: ConnectionStats}
  | CRGroupMemberSwitchStarted {user :: User, groupInfo :: GroupInfo, member :: GroupMember, connectionStats :: ConnectionStats}
  | CRContactSwitchAborted {user :: User, contact :: Contact, connectionStats :: ConnectionStats}
  | CRGroupMemberSwitchAborted {user :: User, groupInfo :: GroupInfo, member :: GroupMember, connectionStats :: ConnectionStats}
  | CRContactSwitch {user :: User, contact :: Contact, switchProgress :: SwitchProgress}
  | CRGroupMemberSwitch {user :: User, groupInfo :: GroupInfo, member :: GroupMember, switchProgress :: SwitchProgress}
  | CRContactRatchetSyncStarted {user :: User, contact :: Contact, connectionStats :: ConnectionStats}
  | CRGroupMemberRatchetSyncStarted {user :: User, groupInfo :: GroupInfo, member :: GroupMember, connectionStats :: ConnectionStats}
  | CRContactRatchetSync {user :: User, contact :: Contact, ratchetSyncProgress :: RatchetSyncProgress}
  | CRGroupMemberRatchetSync {user :: User, groupInfo :: GroupInfo, member :: GroupMember, ratchetSyncProgress :: RatchetSyncProgress}
  | CRContactVerificationReset {user :: User, contact :: Contact}
  | CRGroupMemberVerificationReset {user :: User, groupInfo :: GroupInfo, member :: GroupMember}
  | CRContactCode {user :: User, contact :: Contact, connectionCode :: Text}
  | CRGroupMemberCode {user :: User, groupInfo :: GroupInfo, member :: GroupMember, connectionCode :: Text}
  | CRConnectionVerified {user :: User, verified :: Bool, expectedCode :: Text}
  | CRNewChatItem {user :: User, chatItem :: AChatItem}
  | CRChatItemStatusUpdated {user :: User, chatItem :: AChatItem}
  | CRChatItemUpdated {user :: User, chatItem :: AChatItem}
  | CRChatItemNotChanged {user :: User, chatItem :: AChatItem}
  | CRChatItemReaction {user :: User, added :: Bool, reaction :: ACIReaction}
  | CRChatItemDeleted {user :: User, deletedChatItem :: AChatItem, toChatItem :: Maybe AChatItem, byUser :: Bool, timed :: Bool}
  | CRChatItemDeletedNotFound {user :: User, contact :: Contact, sharedMsgId :: SharedMsgId}
  | CRBroadcastSent {user :: User, msgContent :: MsgContent, successes :: Int, failures :: Int, timestamp :: UTCTime}
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
  | CRGroupsList {user :: User, groups :: [(GroupInfo, GroupSummary)]}
  | CRSentGroupInvitation {user :: User, groupInfo :: GroupInfo, contact :: Contact, member :: GroupMember}
  | CRFileTransferStatus User (FileTransfer, [Integer]) -- TODO refactor this type to FileTransferStatus
  | CRFileTransferStatusXFTP User AChatItem
  | CRUserProfile {user :: User, profile :: Profile}
  | CRUserProfileNoChange {user :: User}
  | CRUserPrivacy {user :: User, updatedUser :: User}
  | CRVersionInfo {versionInfo :: CoreVersionInfo, chatMigrations :: [UpMigration], agentMigrations :: [UpMigration]}
  | CRInvitation {user :: User, connReqInvitation :: ConnReqInvitation, connection :: PendingContactConnection}
  | CRConnectionIncognitoUpdated {user :: User, toConnection :: PendingContactConnection}
  | CRConnectionPlan {user :: User, connectionPlan :: ConnectionPlan}
  | CRSentConfirmation {user :: User}
  | CRSentInvitation {user :: User, customUserProfile :: Maybe Profile}
  | CRContactUpdated {user :: User, fromContact :: Contact, toContact :: Contact}
  | CRContactsMerged {user :: User, intoContact :: Contact, mergedContact :: Contact, updatedContact :: Contact}
  | CRContactDeleted {user :: User, contact :: Contact}
  | CRContactDeletedByContact {user :: User, contact :: Contact}
  | CRChatCleared {user :: User, chatInfo :: AChatInfo}
  | CRUserContactLinkCreated {user :: User, connReqContact :: ConnReqContact}
  | CRUserContactLinkDeleted {user :: User}
  | CRReceivedContactRequest {user :: User, contactRequest :: UserContactRequest}
  | CRAcceptingContactRequest {user :: User, contact :: Contact}
  | CRContactAlreadyExists {user :: User, contact :: Contact}
  | CRContactRequestAlreadyAccepted {user :: User, contact :: Contact}
  | CRLeftMemberUser {user :: User, groupInfo :: GroupInfo}
  | CRGroupDeletedUser {user :: User, groupInfo :: GroupInfo}
  | CRRcvFileDescrReady {user :: User, chatItem :: AChatItem}
  | CRRcvFileAccepted {user :: User, chatItem :: AChatItem}
  | CRRcvFileAcceptedSndCancelled {user :: User, rcvFileTransfer :: RcvFileTransfer}
  | CRRcvFileDescrNotReady {user :: User, chatItem :: AChatItem}
  | CRRcvFileStart {user :: User, chatItem :: AChatItem}
  | CRRcvFileProgressXFTP {user :: User, chatItem :: AChatItem, receivedSize :: Int64, totalSize :: Int64}
  | CRRcvFileComplete {user :: User, chatItem :: AChatItem}
  | CRRcvFileCancelled {user :: User, chatItem :: AChatItem, rcvFileTransfer :: RcvFileTransfer}
  | CRRcvFileSndCancelled {user :: User, chatItem :: AChatItem, rcvFileTransfer :: RcvFileTransfer}
  | CRRcvFileError {user :: User, chatItem :: AChatItem, agentError :: AgentErrorType}
  | CRSndFileStart {user :: User, chatItem :: AChatItem, sndFileTransfer :: SndFileTransfer}
  | CRSndFileComplete {user :: User, chatItem :: AChatItem, sndFileTransfer :: SndFileTransfer}
  | CRSndFileRcvCancelled {user :: User, chatItem :: AChatItem, sndFileTransfer :: SndFileTransfer}
  | CRSndFileCancelled {user :: User, chatItem :: AChatItem, fileTransferMeta :: FileTransferMeta, sndFileTransfers :: [SndFileTransfer]}
  | CRSndFileStartXFTP {user :: User, chatItem :: AChatItem, fileTransferMeta :: FileTransferMeta}
  | CRSndFileProgressXFTP {user :: User, chatItem :: AChatItem, fileTransferMeta :: FileTransferMeta, sentSize :: Int64, totalSize :: Int64}
  | CRSndFileCompleteXFTP {user :: User, chatItem :: AChatItem, fileTransferMeta :: FileTransferMeta}
  | CRSndFileCancelledXFTP {user :: User, chatItem :: AChatItem, fileTransferMeta :: FileTransferMeta}
  | CRSndFileError {user :: User, chatItem :: AChatItem}
  | CRUserProfileUpdated {user :: User, fromProfile :: Profile, toProfile :: Profile, updateSummary :: UserProfileUpdateSummary}
  | CRUserProfileImage {user :: User, profile :: Profile}
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
  | CRNetworkStatus {networkStatus :: NetworkStatus, connections :: [AgentConnId]}
  | CRNetworkStatuses {user_ :: Maybe User, networkStatuses :: [ConnNetworkStatus]}
  | CRHostConnected {protocol :: AProtocolType, transportHost :: TransportHost}
  | CRHostDisconnected {protocol :: AProtocolType, transportHost :: TransportHost}
  | CRGroupInvitation {user :: User, groupInfo :: GroupInfo}
  | CRReceivedGroupInvitation {user :: User, groupInfo :: GroupInfo, contact :: Contact, fromMemberRole :: GroupMemberRole, memberRole :: GroupMemberRole}
  | CRUserJoinedGroup {user :: User, groupInfo :: GroupInfo, hostMember :: GroupMember}
  | CRJoinedGroupMember {user :: User, groupInfo :: GroupInfo, member :: GroupMember}
  | CRJoinedGroupMemberConnecting {user :: User, groupInfo :: GroupInfo, hostMember :: GroupMember, member :: GroupMember}
  | CRMemberRole {user :: User, groupInfo :: GroupInfo, byMember :: GroupMember, member :: GroupMember, fromRole :: GroupMemberRole, toRole :: GroupMemberRole}
  | CRMemberRoleUser {user :: User, groupInfo :: GroupInfo, member :: GroupMember, fromRole :: GroupMemberRole, toRole :: GroupMemberRole}
  | CRConnectedToGroupMember {user :: User, groupInfo :: GroupInfo, member :: GroupMember, memberContact :: Maybe Contact}
  | CRDeletedMember {user :: User, groupInfo :: GroupInfo, byMember :: GroupMember, deletedMember :: GroupMember}
  | CRDeletedMemberUser {user :: User, groupInfo :: GroupInfo, member :: GroupMember}
  | CRLeftMember {user :: User, groupInfo :: GroupInfo, member :: GroupMember}
  | CRGroupEmpty {user :: User, groupInfo :: GroupInfo}
  | CRGroupRemoved {user :: User, groupInfo :: GroupInfo}
  | CRGroupDeleted {user :: User, groupInfo :: GroupInfo, member :: GroupMember}
  | CRGroupUpdated {user :: User, fromGroup :: GroupInfo, toGroup :: GroupInfo, member_ :: Maybe GroupMember}
  | CRGroupProfile {user :: User, groupInfo :: GroupInfo}
  | CRGroupDescription {user :: User, groupInfo :: GroupInfo} -- only used in CLI
  | CRGroupLinkCreated {user :: User, groupInfo :: GroupInfo, connReqContact :: ConnReqContact, memberRole :: GroupMemberRole}
  | CRGroupLink {user :: User, groupInfo :: GroupInfo, connReqContact :: ConnReqContact, memberRole :: GroupMemberRole}
  | CRGroupLinkDeleted {user :: User, groupInfo :: GroupInfo}
  | CRAcceptingGroupJoinRequest {user :: User, groupInfo :: GroupInfo, contact :: Contact}
  | CRNoMemberContactCreating {user :: User, groupInfo :: GroupInfo, member :: GroupMember} -- only used in CLI
  | CRNewMemberContact {user :: User, contact :: Contact, groupInfo :: GroupInfo, member :: GroupMember}
  | CRNewMemberContactSentInv {user :: User, contact :: Contact, groupInfo :: GroupInfo, member :: GroupMember}
  | CRNewMemberContactReceivedInv {user :: User, contact :: Contact, groupInfo :: GroupInfo, member :: GroupMember}
  | CRContactAndMemberAssociated {user :: User, contact :: Contact, groupInfo :: GroupInfo, member :: GroupMember, updatedContact :: Contact}
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
  | CRRemoteHostCreated {remoteHost :: RemoteHostInfo}
  | CRRemoteHostList {remoteHosts :: [RemoteHostInfo]}
  | CRRemoteHostConnected {remoteHost :: RemoteHostInfo}
  | CRRemoteHostStopped {remoteHostId :: RemoteHostId}
  | CRRemoteCtrlList {remoteCtrls :: [RemoteCtrlInfo]}
  | CRRemoteCtrlRegistered {remoteCtrl :: RemoteCtrlInfo}
  | CRRemoteCtrlAnnounce {fingerprint :: C.KeyHash} -- unregistered fingerprint, needs confirmation
  | CRRemoteCtrlFound {remoteCtrl :: RemoteCtrlInfo} -- registered fingerprint, may connect
  | CRRemoteCtrlConnecting {remoteCtrl :: RemoteCtrlInfo}
  | CRRemoteCtrlConnected {remoteCtrl :: RemoteCtrlInfo}
  | CRRemoteCtrlStopped
  | CRSQLResult {rows :: [Text]}
  | CRSlowSQLQueries {chatQueries :: [SlowSQLQuery], agentQueries :: [SlowSQLQuery]}
  | CRDebugLocks {chatLockName :: Maybe String, agentLocks :: AgentLocks}
  | CRAgentStats {agentStats :: [[String]]}
  | CRAgentSubs {activeSubs :: Map Text Int, pendingSubs :: Map Text Int, removedSubs :: Map Text [String]}
  | CRAgentSubsDetails {agentSubs :: SubscriptionsInfo}
  | CRConnectionDisabled {connectionEntity :: ConnectionEntity}
  | CRAgentRcvQueueDeleted {agentConnId :: AgentConnId, server :: SMPServer, agentQueueId :: AgentQueueId, agentError_ :: Maybe AgentErrorType}
  | CRAgentConnDeleted {agentConnId :: AgentConnId}
  | CRAgentUserDeleted {agentUserId :: Int64}
  | CRMessageError {user :: User, severity :: Text, errorMessage :: Text}
  | CRChatCmdError {user_ :: Maybe User, chatError :: ChatError}
  | CRChatError {user_ :: Maybe User, chatError :: ChatError}
  | CRArchiveImported {archiveErrors :: [ArchiveError]}
  | CRTimedAction {action :: String, durationMilliseconds :: Int64}
  deriving (Show)

allowRemoteEvent :: ChatResponse -> Bool
allowRemoteEvent = \case
  CRRemoteHostCreated {} -> False
  CRRemoteHostList {} -> False
  CRRemoteHostConnected {} -> False
  CRRemoteHostStopped {} -> False
  CRRemoteCtrlList {} -> False
  CRRemoteCtrlRegistered {} -> False
  CRRemoteCtrlAnnounce {} -> False
  CRRemoteCtrlFound {} -> False
  CRRemoteCtrlConnecting {} -> False
  CRRemoteCtrlConnected {} -> False
  CRRemoteCtrlStopped {} -> False
  _ -> True

logResponseToFile :: ChatResponse -> Bool
logResponseToFile = \case
  CRContactsDisconnected {} -> True
  CRContactsSubscribed {} -> True
  CRContactSubError {} -> True
  CRMemberSubError {} -> True
  CRSndFileSubError {} -> True
  CRRcvFileSubError {} -> True
  CRHostConnected {} -> True
  CRHostDisconnected {} -> True
  CRConnectionDisabled {} -> True
  CRAgentRcvQueueDeleted {} -> True
  CRAgentConnDeleted {} -> True
  CRAgentUserDeleted {} -> True
  CRChatCmdError {} -> True
  CRChatError {} -> True
  CRMessageError {} -> True
  _ -> False

data ConnectionPlan
  = CPInvitationLink {invitationLinkPlan :: InvitationLinkPlan}
  | CPContactAddress {contactAddressPlan :: ContactAddressPlan}
  | CPGroupLink {groupLinkPlan :: GroupLinkPlan}
  deriving (Show, Generic)

instance FromJSON ConnectionPlan where
  parseJSON = J.genericParseJSON . sumTypeJSON $ dropPrefix "CP"

instance ToJSON ConnectionPlan where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "CP"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "CP"

data InvitationLinkPlan
  = ILPOk
  | ILPOwnLink
  | ILPConnecting {contact_ :: Maybe Contact}
  | ILPKnown {contact :: Contact}
  deriving (Show, Generic)

instance FromJSON InvitationLinkPlan where
  parseJSON = J.genericParseJSON . sumTypeJSON $ dropPrefix "ILP"

instance ToJSON InvitationLinkPlan where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "ILP"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "ILP"

data ContactAddressPlan
  = CAPOk
  | CAPOwnLink
  | CAPConnectingConfirmReconnect
  | CAPConnectingProhibit {contact :: Contact}
  | CAPKnown {contact :: Contact}
  deriving (Show, Generic)

instance FromJSON ContactAddressPlan where
  parseJSON = J.genericParseJSON . sumTypeJSON $ dropPrefix "CAP"

instance ToJSON ContactAddressPlan where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "CAP"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "CAP"

data GroupLinkPlan
  = GLPOk
  | GLPOwnLink {groupInfo :: GroupInfo}
  | GLPConnectingConfirmReconnect
  | GLPConnectingProhibit {groupInfo_ :: Maybe GroupInfo}
  | GLPKnown {groupInfo :: GroupInfo}
  deriving (Show, Generic)

instance FromJSON GroupLinkPlan where
  parseJSON = J.genericParseJSON . sumTypeJSON $ dropPrefix "GLP"

instance ToJSON GroupLinkPlan where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "GLP"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "GLP"

connectionPlanProceed :: ConnectionPlan -> Bool
connectionPlanProceed = \case
  CPInvitationLink ilp -> case ilp of
    ILPOk -> True
    ILPOwnLink -> True
    _ -> False
  CPContactAddress cap -> case cap of
    CAPOk -> True
    CAPOwnLink -> True
    CAPConnectingConfirmReconnect -> True
    _ -> False
  CPGroupLink glp -> case glp of
    GLPOk -> True
    GLPOwnLink _ -> True
    GLPConnectingConfirmReconnect -> True
    _ -> False

newtype UserPwd = UserPwd {unUserPwd :: Text}
  deriving (Eq, Show)

instance FromJSON UserPwd where
  parseJSON v = UserPwd <$> parseJSON v

instance ToJSON UserPwd where
  toJSON (UserPwd p) = toJSON p
  toEncoding (UserPwd p) = toEncoding p

newtype AgentQueueId = AgentQueueId QueueId
  deriving (Eq, Show)

instance StrEncoding AgentQueueId where
  strEncode (AgentQueueId qId) = strEncode qId
  strDecode s = AgentQueueId <$> strDecode s
  strP = AgentQueueId <$> strP

instance FromJSON AgentQueueId where
  parseJSON = strParseJSON "AgentQueueId"

instance ToJSON AgentQueueId where
  toJSON = strToJSON
  toEncoding = strToJEncoding

data ProtoServersConfig p = ProtoServersConfig {servers :: [ServerCfg p]}
  deriving (Show, Generic, FromJSON)

data AProtoServersConfig = forall p. ProtocolTypeI p => APSC (SProtocolType p) (ProtoServersConfig p)

deriving instance Show AProtoServersConfig

data UserProtoServers p = UserProtoServers
  { serverProtocol :: SProtocolType p,
    protoServers :: NonEmpty (ServerCfg p),
    presetServers :: NonEmpty (ProtoServerWithAuth p)
  }
  deriving (Show, Generic)

instance ProtocolTypeI p => FromJSON (UserProtoServers p) where
  parseJSON = J.genericParseJSON J.defaultOptions

instance ProtocolTypeI p => ToJSON (UserProtoServers p) where
  toEncoding = J.genericToEncoding J.defaultOptions

data AUserProtoServers = forall p. (ProtocolTypeI p, UserProtocol p) => AUPS (UserProtoServers p)

instance FromJSON AUserProtoServers where
  parseJSON v = J.withObject "AUserProtoServers" parse v
    where
      parse o = do
        AProtocolType (p :: SProtocolType p) <- o .: "serverProtocol"
        case userProtocol p of
          Just Dict -> AUPS <$> J.parseJSON @(UserProtoServers p) v
          Nothing -> fail $ "AUserProtoServers: unsupported protocol " <> show p

instance ToJSON AUserProtoServers where
  toJSON (AUPS s) = J.genericToJSON J.defaultOptions s
  toEncoding (AUPS s) = J.genericToEncoding J.defaultOptions s

deriving instance Show AUserProtoServers

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
  deriving (Show, Generic, FromJSON)

instance ToJSON ContactSubStatus where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

data MemberSubStatus = MemberSubStatus
  { member :: GroupMember,
    memberError :: Maybe ChatError
  }
  deriving (Show, Generic, FromJSON)

instance ToJSON MemberSubStatus where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

data UserContactSubStatus = UserContactSubStatus
  { userContact :: UserContact,
    userContactError :: Maybe ChatError
  }
  deriving (Show, Generic, FromJSON)

instance ToJSON UserContactSubStatus where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

data PendingSubStatus = PendingSubStatus
  { connection :: PendingContactConnection,
    connError :: Maybe ChatError
  }
  deriving (Show, Generic, FromJSON)

instance ToJSON PendingSubStatus where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

data UserProfileUpdateSummary = UserProfileUpdateSummary
  { notChanged :: Int,
    updateSuccesses :: Int,
    updateFailures :: Int,
    changedContacts :: [Contact]
  }
  deriving (Show, Generic, FromJSON)

instance ToJSON UserProfileUpdateSummary where toEncoding = J.genericToEncoding J.defaultOptions

data ComposedMessage = ComposedMessage
  { fileSource :: Maybe CryptoFile,
    quotedItemId :: Maybe ChatItemId,
    msgContent :: MsgContent
  }
  deriving (Show, Generic)

-- This instance is needed for backward compatibility, can be removed in v6.0
instance FromJSON ComposedMessage where
  parseJSON (J.Object v) = do
    fileSource <-
      (v .:? "fileSource") >>= \case
        Nothing -> CF.plain <$$> (v .:? "filePath")
        f -> pure f
    quotedItemId <- v .:? "quotedItemId"
    msgContent <- v .: "msgContent"
    pure ComposedMessage {fileSource, quotedItemId, msgContent}
  parseJSON invalid =
    JT.prependFailure "bad ComposedMessage, " (JT.typeMismatch "Object" invalid)

instance ToJSON ComposedMessage where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

data XFTPFileConfig = XFTPFileConfig
  { minFileSize :: Integer
  }
  deriving (Show, Generic, FromJSON)

defaultXFTPFileConfig :: XFTPFileConfig
defaultXFTPFileConfig = XFTPFileConfig {minFileSize = 0}

instance ToJSON XFTPFileConfig where toEncoding = J.genericToEncoding J.defaultOptions

data NtfMsgInfo = NtfMsgInfo {msgTs :: UTCTime, msgFlags :: MsgFlags}
  deriving (Show, Generic, FromJSON)

instance ToJSON NtfMsgInfo where toEncoding = J.genericToEncoding J.defaultOptions

crNtfToken :: (DeviceToken, NtfTknStatus, NotificationsMode) -> ChatResponse
crNtfToken (token, status, ntfMode) = CRNtfToken {token, status, ntfMode}

data SwitchProgress = SwitchProgress
  { queueDirection :: QueueDirection,
    switchPhase :: SwitchPhase,
    connectionStats :: ConnectionStats
  }
  deriving (Show, Generic, FromJSON)

instance ToJSON SwitchProgress where toEncoding = J.genericToEncoding J.defaultOptions

data RatchetSyncProgress = RatchetSyncProgress
  { ratchetSyncStatus :: RatchetSyncState,
    connectionStats :: ConnectionStats
  }
  deriving (Show, Generic, FromJSON)

instance ToJSON RatchetSyncProgress where toEncoding = J.genericToEncoding J.defaultOptions

data ParsedServerAddress = ParsedServerAddress
  { serverAddress :: Maybe ServerAddress,
    parseError :: String
  }
  deriving (Show, Generic, FromJSON)

instance ToJSON ParsedServerAddress where toEncoding = J.genericToEncoding J.defaultOptions

data ServerAddress = ServerAddress
  { serverProtocol :: AProtocolType,
    hostnames :: NonEmpty String,
    port :: String,
    keyHash :: String,
    basicAuth :: String
  }
  deriving (Show, Generic, FromJSON)

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
    simplexmqVersion :: String,
    simplexmqCommit :: String
  }
  deriving (Show, Generic, FromJSON)

instance ToJSON CoreVersionInfo where toEncoding = J.genericToEncoding J.defaultOptions

data SendFileMode
  = SendFileSMP (Maybe InlineFileMode)
  | SendFileXFTP
  deriving (Show, Generic)

data SlowSQLQuery = SlowSQLQuery
  { query :: Text,
    queryStats :: SlowQueryStats
  }
  deriving (Show, Generic, FromJSON)

instance ToJSON SlowSQLQuery where toEncoding = J.genericToEncoding J.defaultOptions

data ChatError
  = ChatError {errorType :: ChatErrorType}
  | ChatErrorAgent {agentError :: AgentErrorType, connectionEntity_ :: Maybe ConnectionEntity}
  | ChatErrorStore {storeError :: StoreError}
  | ChatErrorDatabase {databaseError :: DatabaseError}
  | ChatErrorRemoteCtrl {remoteCtrlError :: RemoteCtrlError}
  | ChatErrorRemoteHost {remoteHostId :: RemoteHostId, remoteHostError :: RemoteHostError}
  deriving (Show, Exception, Generic)

instance FromJSON ChatError where
  parseJSON = J.genericParseJSON . sumTypeJSON $ dropPrefix "Chat"

instance ToJSON ChatError where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "Chat"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "Chat"

data ChatErrorType
  = CENoActiveUser
  | CENoConnectionUser {agentConnId :: AgentConnId}
  | CENoSndFileUser {agentSndFileId :: AgentSndFileId}
  | CENoRcvFileUser {agentRcvFileId :: AgentRcvFileId}
  | CEUserUnknown
  | CEActiveUserExists -- TODO delete
  | CEUserExists {contactName :: ContactName}
  | CEDifferentActiveUser {commandUserId :: UserId, activeUserId :: UserId}
  | CECantDeleteActiveUser {userId :: UserId}
  | CECantDeleteLastUser {userId :: UserId}
  | CECantHideLastUser {userId :: UserId}
  | CEHiddenUserAlwaysMuted {userId :: UserId}
  | CEEmptyUserPassword {userId :: UserId}
  | CEUserAlreadyHidden {userId :: UserId}
  | CEUserNotHidden {userId :: UserId}
  | CEInvalidDisplayName {displayName :: Text, validName :: Text}
  | CEChatNotStarted
  | CEChatNotStopped
  | CEChatStoreChanged
  | CEConnectionPlan {connectionPlan :: ConnectionPlan}
  | CEInvalidConnReq
  | CEInvalidChatMessage {connection :: Connection, msgMeta :: Maybe MsgMetaJSON, messageData :: Text, message :: String}
  | CEContactNotFound {contactName :: ContactName, suspectedMember :: Maybe (GroupInfo, GroupMember)}
  | CEContactNotReady {contact :: Contact}
  | CEContactNotActive {contact :: Contact}
  | CEContactDisabled {contact :: Contact}
  | CEConnectionDisabled {connection :: Connection}
  | CEGroupUserRole {groupInfo :: GroupInfo, requiredRole :: GroupMemberRole}
  | CEGroupMemberInitialRole {groupInfo :: GroupInfo, initialRole :: GroupMemberRole}
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
  | CEFileSize {filePath :: FilePath}
  | CEFileAlreadyReceiving {message :: String}
  | CEFileCancelled {message :: String}
  | CEFileCancel {fileId :: FileTransferId, message :: String}
  | CEFileAlreadyExists {filePath :: FilePath}
  | CEFileRead {filePath :: FilePath, message :: String}
  | CEFileWrite {filePath :: FilePath, message :: String}
  | CEFileSend {fileId :: FileTransferId, agentError :: AgentErrorType}
  | CEFileRcvChunk {message :: String}
  | CEFileInternal {message :: String}
  | CEFileImageType {filePath :: FilePath}
  | CEFileImageSize {filePath :: FilePath}
  | CEFileNotReceived {fileId :: FileTransferId}
  | CEXFTPRcvFile {fileId :: FileTransferId, agentRcvFileId :: AgentRcvFileId, agentError :: AgentErrorType}
  | CEXFTPSndFile {fileId :: FileTransferId, agentSndFileId :: AgentSndFileId, agentError :: AgentErrorType}
  | CEFallbackToSMPProhibited {fileId :: FileTransferId}
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
  | CEServerProtocol {serverProtocol :: AProtocolType}
  | CEAgentCommandError {message :: String}
  | CEInvalidFileDescription {message :: String}
  | CEConnectionIncognitoChangeProhibited
  | CEPeerChatVRangeIncompatible
  | CEInternalError {message :: String}
  | CEException {message :: String}
  deriving (Show, Exception, Generic)

instance FromJSON ChatErrorType where
  parseJSON = J.genericParseJSON . sumTypeJSON $ dropPrefix "CE"

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

instance FromJSON DatabaseError where
  parseJSON = J.genericParseJSON . sumTypeJSON $ dropPrefix "DB"

instance ToJSON DatabaseError where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "DB"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "DB"

data SQLiteError = SQLiteErrorNotADatabase | SQLiteError String
  deriving (Show, Exception, Generic)

instance FromJSON SQLiteError where
  parseJSON = J.genericParseJSON . sumTypeJSON $ dropPrefix "SQLite"

instance ToJSON SQLiteError where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "SQLite"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "SQLite"

throwDBError :: ChatMonad m => DatabaseError -> m ()
throwDBError = throwError . ChatErrorDatabase

-- TODO review errors, some of it can be covered by HTTP2 errors
data RemoteHostError
  = RHMissing -- ^ No remote session matches this identifier
  | RHBusy -- ^ A session is already running
  | RHRejected -- ^ A session attempt was rejected by a host
  | RHTimeout -- ^ A discovery or a remote operation has timed out
  | RHDisconnected {reason :: Text} -- ^ A session disconnected by a host
  | RHConnectionLost {reason :: Text} -- ^ A session disconnected due to transport issues
  deriving (Show, Exception, Generic)

instance FromJSON RemoteHostError where
  parseJSON = J.genericParseJSON . sumTypeJSON $ dropPrefix "RH"

instance ToJSON RemoteHostError where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "RH"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "RH"

-- TODO review errors, some of it can be covered by HTTP2 errors
data RemoteCtrlError
  = RCEInactive -- ^ No session is running
  | RCEBusy -- ^ A session is already running
  | RCETimeout -- ^ Remote operation timed out
  | RCEDisconnected {remoteCtrlId :: RemoteCtrlId, reason :: Text} -- ^ A session disconnected by a controller
  | RCEConnectionLost {remoteCtrlId :: RemoteCtrlId, reason :: Text} -- ^ A session disconnected due to transport issues
  | RCECertificateExpired {remoteCtrlId :: RemoteCtrlId} -- ^ A connection or CA certificate in a chain have bad validity period
  | RCECertificateUntrusted {remoteCtrlId :: RemoteCtrlId} -- ^ TLS is unable to validate certificate chain presented for a connection
  | RCEBadFingerprint -- ^ Bad fingerprint data provided in OOB
  | RCEHTTP2Error {http2Error :: String}
  | RCEHTTP2RespStatus {statusCode :: Maybe Int} -- TODO remove
  | RCEInvalidResponse {responseError :: String}
  deriving (Show, Exception, Generic)

instance FromJSON RemoteCtrlError where
  parseJSON = J.genericParseJSON . sumTypeJSON $ dropPrefix "RCE"

instance ToJSON RemoteCtrlError where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "RCE"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "RCE"

data ArchiveError
  = AEImport {chatError :: ChatError}
  | AEImportFile {file :: String, chatError :: ChatError}
  deriving (Show, Exception, Generic)

instance FromJSON ArchiveError where
  parseJSON = J.genericParseJSON . sumTypeJSON $ dropPrefix "AE"

instance ToJSON ArchiveError where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "AE"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "AE"

data RemoteHostSession
  = RemoteHostSessionStarting
      { announcer :: Async ()
      }
  | RemoteHostSessionStarted
      { -- | Path for local resources to be synchronized with host
        storePath :: FilePath,
        ctrlClient :: HTTP2Client
      }

data RemoteCtrlSession = RemoteCtrlSession
  { -- | Host (mobile) side of transport to process remote commands and forward notifications
    discoverer :: Async (),
    supervisor :: Async (),
    hostServer :: Maybe (Async ()),
    discovered :: TMap C.KeyHash TransportHost,
    accepted :: TMVar RemoteCtrlId,
    remoteOutputQ :: TBQueue ChatResponse
  }

type ChatMonad' m = (MonadUnliftIO m, MonadReader ChatController m)

type ChatMonad m = (ChatMonad' m, MonadError ChatError m)

chatReadVar :: ChatMonad' m => (ChatController -> TVar a) -> m a
chatReadVar f = asks f >>= readTVarIO
{-# INLINE chatReadVar #-}

chatWriteVar :: ChatMonad' m => (ChatController -> TVar a) -> a -> m ()
chatWriteVar f value = asks f >>= atomically . (`writeTVar` value)
{-# INLINE chatWriteVar #-}

chatModifyVar :: ChatMonad' m => (ChatController -> TVar a) -> (a -> a) -> m ()
chatModifyVar f newValue = asks f >>= atomically . (`modifyTVar'` newValue)
{-# INLINE chatModifyVar #-}

setContactNetworkStatus :: ChatMonad' m => Contact -> NetworkStatus -> m ()
setContactNetworkStatus ct = chatModifyVar connNetworkStatuses . M.insert (contactAgentConnId ct)

tryChatError :: ChatMonad m => m a -> m (Either ChatError a)
tryChatError = tryAllErrors mkChatError
{-# INLINE tryChatError #-}

catchChatError :: ChatMonad m => m a -> (ChatError -> m a) -> m a
catchChatError = catchAllErrors mkChatError
{-# INLINE catchChatError #-}

chatFinally :: ChatMonad m => m a -> m b -> m a
chatFinally = allFinally mkChatError
{-# INLINE chatFinally #-}

onChatError :: ChatMonad m => m a -> m b -> m a
a `onChatError` onErr = a `catchChatError` \e -> onErr >> throwError e
{-# INLINE onChatError #-}

mkChatError :: SomeException -> ChatError
mkChatError = ChatError . CEException . show
{-# INLINE mkChatError #-}

chatCmdError :: Maybe User -> String -> ChatResponse
chatCmdError user = CRChatCmdError user . ChatError . CECommandError

throwChatError :: ChatMonad m => ChatErrorType -> m a
throwChatError = throwError . ChatError

-- | Emit local events.
toView :: ChatMonad' m => ChatResponse -> m ()
toView event = do
  localQ <- asks outputQ
  chatReadVar remoteCtrlSession >>= \case
    Nothing -> atomically $ writeTBQueue localQ (Nothing, Nothing, event)
    Just RemoteCtrlSession {remoteOutputQ} ->
      if allowRemoteEvent event
        then do
          -- TODO: filter events or let the UI ignore trigger events by itself?
          -- traceM $ "Sending event to remote Q: " <> show event
          atomically $ writeTBQueue remoteOutputQ event -- TODO: check full?
        else do
          -- traceM $ "Sending event to local Q: " <> show event
          atomically $ writeTBQueue localQ (Nothing, Nothing, event)

withStore' :: ChatMonad m => (DB.Connection -> IO a) -> m a
withStore' action = withStore $ liftIO . action

withStore :: ChatMonad m => (DB.Connection -> ExceptT StoreError IO a) -> m a
withStore = withStoreCtx Nothing

withStoreCtx' :: ChatMonad m => Maybe String -> (DB.Connection -> IO a) -> m a
withStoreCtx' ctx_ action = withStoreCtx ctx_ $ liftIO . action

withStoreCtx :: ChatMonad m => Maybe String -> (DB.Connection -> ExceptT StoreError IO a) -> m a
withStoreCtx ctx_ action = do
  ChatController {chatStore} <- ask
  liftEitherError ChatErrorStore $ case ctx_ of
    Nothing -> withTransaction chatStore (runExceptT . action) `catch` handleInternal ""
    -- uncomment to debug store performance
    -- Just ctx -> do
    --   t1 <- liftIO getCurrentTime
    --   putStrLn $ "withStoreCtx start       :: " <> show t1 <> " :: " <> ctx
    --   r <- withTransactionCtx ctx_ chatStore (runExceptT . action) `E.catch` handleInternal (" (" <> ctx <> ")")
    --   t2 <- liftIO getCurrentTime
    --   putStrLn $ "withStoreCtx end         :: " <> show t2 <> " :: " <> ctx <> " :: duration=" <> show (diffToMilliseconds $ diffUTCTime t2 t1)
    --   pure r
    Just _ -> withTransaction chatStore (runExceptT . action) `catch` handleInternal ""
  where
    handleInternal :: String -> SomeException -> IO (Either StoreError a)
    handleInternal ctxStr e = pure . Left . SEInternalError $ show e <> ctxStr

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "CR") ''ChatResponse)
