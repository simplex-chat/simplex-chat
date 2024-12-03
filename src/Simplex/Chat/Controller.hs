{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
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
{-# OPTIONS_GHC -fno-warn-implicit-lift #-}

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
import qualified Data.Aeson.TH as JQ
import qualified Data.Aeson.Types as JT
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bifunctor (first)
import Data.ByteArray (ScrubbedBytes)
import qualified Data.ByteArray as BA
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
import Data.Text.Encoding (decodeLatin1)
import Data.Time (NominalDiffTime, UTCTime)
import Data.Time.Clock.System (SystemTime (..), systemToUTCTime)
import Data.Version (showVersion)
import Data.Word (Word16)
import Database.SQLite.Simple (SQLError)
import qualified Database.SQLite.Simple as SQL
import Language.Haskell.TH (Exp, Q, runIO)
import Numeric.Natural
import qualified Paths_simplex_chat as SC
import Simplex.Chat.AppSettings
import Simplex.Chat.Call
import Simplex.Chat.Markdown (MarkdownList)
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Protocol
import Simplex.Chat.Remote.AppVersion
import Simplex.Chat.Remote.Types
import Simplex.Chat.Stats (PresentedServersSummary)
import Simplex.Chat.Store (AutoAccept, ChatLockEntity, StoreError (..), UserContactLink, UserMsgReceiptSettings)
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Chat.Types.Shared
import Simplex.Chat.Types.UITheme
import Simplex.Chat.Util (liftIOEither)
import Simplex.FileTransfer.Description (FileDescriptionURI)
import Simplex.Messaging.Agent (AgentClient, SubscriptionsInfo)
import Simplex.Messaging.Agent.Client (AgentLocks, AgentQueuesInfo (..), AgentWorkersDetails (..), AgentWorkersSummary (..), ProtocolTestFailure, SMPServerSubs, ServerQueueInfo, UserNetworkInfo)
import Simplex.Messaging.Agent.Env.SQLite (AgentConfig, NetworkConfig, ServerCfg)
import Simplex.Messaging.Agent.Lock
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Agent.Store.SQLite (MigrationConfirmation, SQLiteStore, UpMigration, withTransaction, withTransactionPriority)
import Simplex.Messaging.Agent.Store.SQLite.DB (SlowQueryStats (..))
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB
import Simplex.Messaging.Client (HostMode (..), SMPProxyFallback (..), SMPProxyMode (..), SocksMode (..))
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.File (CryptoFile (..))
import qualified Simplex.Messaging.Crypto.File as CF
import Simplex.Messaging.Crypto.Ratchet (PQEncryption)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Notifications.Protocol (DeviceToken (..), NtfTknStatus)
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON, parseAll, parseString, sumTypeJSON)
import Simplex.Messaging.Protocol (AProtoServerWithAuth, AProtocolType (..), CorrId, MsgId, NMsgMeta (..), NtfServer, ProtocolType (..), ProtocolTypeI, QueueId, SMPMsgMeta (..), SProtocolType, SubscriptionMode (..), UserProtocol, XFTPServer, userProtocol)
import Simplex.Messaging.TMap (TMap)
import Simplex.Messaging.Transport (TLS, simplexMQVersion)
import Simplex.Messaging.Transport.Client (SocksProxyWithAuth, TransportHost)
import Simplex.Messaging.Util (allFinally, catchAllErrors, catchAllErrors', tryAllErrors, tryAllErrors', (<$$>))
import Simplex.RemoteControl.Client
import Simplex.RemoteControl.Invitation (RCSignedInvitation, RCVerifiedInvitation)
import Simplex.RemoteControl.Types
import System.IO (Handle)
import System.Mem.Weak (Weak)
import qualified UnliftIO.Exception as E
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
    chatVRange :: VersionRangeChat,
    confirmMigrations :: MigrationConfirmation,
    defaultServers :: DefaultAgentServers,
    tbqSize :: Natural,
    fileChunkSize :: Integer,
    xftpDescrPartSize :: Int,
    inlineFiles :: InlineFilesConfig,
    autoAcceptFileSize :: Integer,
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
    coreApi :: Bool,
    highlyAvailable :: Bool,
    deviceNameForRemote :: Text,
    chatHooks :: ChatHooks
  }

-- The hooks can be used to extend or customize chat core in mobile or CLI clients.
data ChatHooks = ChatHooks
  { -- preCmdHook can be used to process or modify the commands before they are processed.
    -- This hook should be used to process CustomChatCommand.
    -- if this hook returns ChatResponse, the command processing will be skipped.
    preCmdHook :: ChatController -> ChatCommand -> IO (Either ChatResponse ChatCommand),
    -- eventHook can be used to additionally process or modify events,
    -- it is called before the event is sent to the user (or to the UI).
    eventHook :: ChatController -> ChatResponse -> IO ChatResponse
  }

defaultChatHooks :: ChatHooks
defaultChatHooks =
  ChatHooks
    { preCmdHook = \_ -> pure . Right,
      eventHook = \_ -> pure
    }

data DefaultAgentServers = DefaultAgentServers
  { smp :: NonEmpty (ServerCfg 'PSMP),
    useSMP :: Int,
    ntf :: [NtfServer],
    xftp :: NonEmpty (ServerCfg 'PXFTP),
    useXFTP :: Int,
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
    random :: TVar ChaChaDRG,
    eventSeq :: TVar Int,
    inputQ :: TBQueue String,
    outputQ :: TBQueue (Maybe CorrId, Maybe RemoteHostId, ChatResponse),
    connNetworkStatuses :: TMap AgentConnId NetworkStatus,
    subscriptionMode :: TVar SubscriptionMode,
    chatLock :: Lock,
    entityLocks :: TMap ChatLockEntity Lock,
    sndFiles :: TVar (Map Int64 Handle),
    rcvFiles :: TVar (Map Int64 Handle),
    currentCalls :: TMap ContactId Call,
    localDeviceName :: TVar Text,
    multicastSubscribers :: TMVar Int,
    remoteSessionSeq :: TVar Int,
    remoteHostSessions :: TMap RHKey (SessionSeq, RemoteHostSession), -- All the active remote hosts
    remoteHostsFolder :: TVar (Maybe FilePath), -- folder for remote hosts data
    remoteCtrlSession :: TVar (Maybe (SessionSeq, RemoteCtrlSession)), -- Supervisor process for hosted controllers
    config :: ChatConfig,
    filesFolder :: TVar (Maybe FilePath), -- path to files folder for mobile apps,
    expireCIThreads :: TMap UserId (Maybe (Async ())),
    expireCIFlags :: TMap UserId Bool,
    cleanupManagerAsync :: TVar (Maybe (Async ())),
    chatActivated :: TVar Bool,
    timedItemThreads :: TMap (ChatRef, ChatItemId) (TVar (Maybe (Weak ThreadId))),
    showLiveItems :: TVar Bool,
    encryptLocalFiles :: TVar Bool,
    tempDirectory :: TVar (Maybe FilePath),
    assetsDirectory :: TVar (Maybe FilePath),
    logFilePath :: Maybe FilePath,
    contactMergeEnabled :: TVar Bool
  }

data HelpSection = HSMain | HSFiles | HSGroups | HSContacts | HSMyAddress | HSIncognito | HSMarkdown | HSMessages | HSRemote | HSSettings | HSDatabase
  deriving (Show)

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
  | StartChat {mainApp :: Bool, enableSndFiles :: Bool} -- enableSndFiles has no effect when mainApp is True
  | CheckChatRunning
  | APIStopChat
  | APIActivateChat {restoreChat :: Bool}
  | APISuspendChat {suspendTimeout :: Int}
  | ResubscribeAllConnections
  | SetTempFolder FilePath
  | SetFilesFolder FilePath
  | SetRemoteHostsFolder FilePath
  | APISetAppFilePaths AppFilePathsConfig
  | APISetEncryptLocalFiles Bool
  | SetContactMergeEnabled Bool
  | APIExportArchive ArchiveConfig
  | ExportArchive
  | APIImportArchive ArchiveConfig
  | APISaveAppSettings AppSettings
  | APIGetAppSettings (Maybe AppSettings)
  | APIDeleteStorage
  | APIStorageEncryption DBEncryptionConfig
  | TestStorageEncryption DBEncryptionKey
  | ExecChatStoreSQL Text
  | ExecAgentStoreSQL Text
  | SlowSQLQueries
  | APIGetChats {userId :: UserId, pendingConnections :: Bool, pagination :: PaginationByTime, query :: ChatListQuery}
  | APIGetChat ChatRef ChatPagination (Maybe String)
  | APIGetChatItems ChatPagination (Maybe String)
  | APIGetChatItemInfo ChatRef ChatItemId
  | APISendMessages {chatRef :: ChatRef, liveMessage :: Bool, ttl :: Maybe Int, composedMessages :: NonEmpty ComposedMessage}
  | APICreateChatItems {noteFolderId :: NoteFolderId, composedMessages :: NonEmpty ComposedMessage}
  | APIUpdateChatItem {chatRef :: ChatRef, chatItemId :: ChatItemId, liveMessage :: Bool, msgContent :: MsgContent}
  | APIDeleteChatItem ChatRef (NonEmpty ChatItemId) CIDeleteMode
  | APIDeleteMemberChatItem GroupId (NonEmpty ChatItemId)
  | APIChatItemReaction {chatRef :: ChatRef, chatItemId :: ChatItemId, add :: Bool, reaction :: MsgReaction}
  | APIPlanForwardChatItems {fromChatRef :: ChatRef, chatItemIds :: NonEmpty ChatItemId}
  | APIForwardChatItems {toChatRef :: ChatRef, fromChatRef :: ChatRef, chatItemIds :: NonEmpty ChatItemId, ttl :: Maybe Int}
  | APIUserRead UserId
  | UserRead
  | APIChatRead ChatRef (Maybe (ChatItemId, ChatItemId))
  | APIChatItemsRead ChatRef (NonEmpty ChatItemId)
  | APIChatUnread ChatRef Bool
  | APIDeleteChat ChatRef ChatDeleteMode -- currently delete mode settings are only applied to direct chats
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
  | APISetUserUIThemes UserId (Maybe UIThemeEntityOverrides)
  | APISetChatUIThemes ChatRef (Maybe UIThemeEntityOverrides)
  | APIParseMarkdown Text
  | APIGetNtfToken
  | APIRegisterToken DeviceToken NotificationsMode
  | APIVerifyToken DeviceToken C.CbNonce ByteString
  | APIDeleteToken DeviceToken
  | APIGetNtfMessage {nonce :: C.CbNonce, encNtfInfo :: ByteString}
  | ApiGetConnNtfMessage {connId :: AgentConnId}
  | APIAddMember GroupId ContactId GroupMemberRole
  | APIJoinGroup GroupId
  | APIMemberRole GroupId GroupMemberId GroupMemberRole
  | APIBlockMemberForAll GroupId GroupMemberId Bool
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
  | SetNetworkConfig SimpleNetCfg
  | APISetNetworkInfo UserNetworkInfo
  | ReconnectAllServers
  | ReconnectServer UserId SMPServer
  | APISetChatSettings ChatRef ChatSettings
  | APISetMemberSettings GroupId GroupMemberId GroupMemberSettings
  | APIContactInfo ContactId
  | APIGroupInfo GroupId
  | APIGroupMemberInfo GroupId GroupMemberId
  | APIContactQueueInfo ContactId
  | APIGroupMemberQueueInfo GroupId GroupMemberId
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
  | ContactQueueInfo ContactName
  | GroupMemberQueueInfo GroupName ContactName
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
  | APIChangeConnectionUser Int64 UserId -- new user id to switch connection to
  | APIConnectPlan UserId AConnectionRequestUri
  | APIConnect UserId IncognitoEnabled (Maybe AConnectionRequestUri)
  | Connect IncognitoEnabled (Maybe AConnectionRequestUri)
  | APIConnectContactViaAddress UserId IncognitoEnabled ContactId
  | ConnectSimplex IncognitoEnabled -- UserId (not used in UI)
  | DeleteContact ContactName ChatDeleteMode
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
  | ForwardMessage {toChatName :: ChatName, fromContactName :: ContactName, forwardedMsg :: Text}
  | ForwardGroupMessage {toChatName :: ChatName, fromGroupName :: GroupName, fromMemberName_ :: Maybe ContactName, forwardedMsg :: Text}
  | ForwardLocalMessage {toChatName :: ChatName, forwardedMsg :: Text}
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
  | APINewGroup UserId IncognitoEnabled GroupProfile
  | NewGroup IncognitoEnabled GroupProfile
  | AddMember GroupName ContactName GroupMemberRole
  | JoinGroup GroupName
  | MemberRole GroupName ContactName GroupMemberRole
  | BlockForAll GroupName ContactName Bool
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
  | ClearNoteFolder
  | LastChats (Maybe Int) -- UserId (not used in UI)
  | LastMessages (Maybe ChatName) Int (Maybe String) -- UserId (not used in UI)
  | LastChatItemId (Maybe ChatName) Int -- UserId (not used in UI)
  | ShowChatItem (Maybe ChatItemId) -- UserId (not used in UI)
  | ShowChatItemInfo ChatName Text
  | ShowLiveItems Bool
  | SendFile ChatName CryptoFile
  | SendImage ChatName CryptoFile
  | ForwardFile ChatName FileTransferId
  | ForwardImage ChatName FileTransferId
  | SendFileDescription ChatName FilePath
  | ReceiveFile {fileId :: FileTransferId, userApprovedRelays :: Bool, storeEncrypted :: Maybe Bool, fileInline :: Maybe Bool, filePath :: Maybe FilePath}
  | SetFileToReceive {fileId :: FileTransferId, userApprovedRelays :: Bool, storeEncrypted :: Maybe Bool}
  | CancelFile FileTransferId
  | FileStatus FileTransferId
  | ShowProfile -- UserId (not used in UI)
  | UpdateProfile ContactName Text -- UserId (not used in UI)
  | UpdateProfileImage (Maybe ImageData) -- UserId (not used in UI)
  | ShowProfileImage
  | SetUserFeature AChatFeature FeatureAllowed -- UserId (not used in UI)
  | SetContactFeature AChatFeature ContactName (Maybe FeatureAllowed)
  | SetGroupFeature AGroupFeatureNoRole GroupName GroupFeatureEnabled
  | SetGroupFeatureRole AGroupFeatureRole GroupName GroupFeatureEnabled (Maybe GroupMemberRole)
  | SetUserTimedMessages Bool -- UserId (not used in UI)
  | SetContactTimedMessages ContactName (Maybe TimedMessagesEnabled)
  | SetGroupTimedMessages GroupName (Maybe Int)
  | SetLocalDeviceName Text
  | ListRemoteHosts
  | StartRemoteHost (Maybe (RemoteHostId, Bool)) (Maybe RCCtrlAddress) (Maybe Word16) -- Start new or known remote host with optional multicast for known host
  | SwitchRemoteHost (Maybe RemoteHostId) -- Switch current remote host
  | StopRemoteHost RHKey -- Shut down a running session
  | DeleteRemoteHost RemoteHostId -- Unregister remote host and remove its data
  | StoreRemoteFile {remoteHostId :: RemoteHostId, storeEncrypted :: Maybe Bool, localPath :: FilePath}
  | GetRemoteFile {remoteHostId :: RemoteHostId, file :: RemoteFile}
  | ConnectRemoteCtrl RCSignedInvitation -- Connect new or existing controller via OOB data
  | FindKnownRemoteCtrl -- Start listening for announcements from all existing controllers
  | ConfirmRemoteCtrl RemoteCtrlId -- Confirm the connection with found controller
  | VerifyRemoteCtrlSession Text -- Verify remote controller session
  | ListRemoteCtrls
  | StopRemoteCtrl -- Stop listening for announcements or terminate an active session
  | DeleteRemoteCtrl RemoteCtrlId -- Remove all local data associated with a remote controller session
  | APIUploadStandaloneFile UserId CryptoFile
  | APIDownloadStandaloneFile UserId FileDescriptionURI CryptoFile
  | APIStandaloneFileInfo FileDescriptionURI
  | QuitChat
  | ShowVersion
  | DebugLocks
  | DebugEvent ChatResponse
  | GetAgentSubsTotal UserId
  | GetAgentServersSummary UserId
  | ResetAgentServersStats
  | GetAgentSubs
  | GetAgentSubsDetails
  | GetAgentWorkers
  | GetAgentWorkersDetails
  | GetAgentQueuesInfo
  | -- The parser will return this command for strings that start from "//".
    -- This command should be processed in preCmdHook
    CustomChatCommand ByteString
  deriving (Show)

allowRemoteCommand :: ChatCommand -> Bool -- XXX: consider using Relay/Block/ForceLocal
allowRemoteCommand = \case
  StartChat {} -> False
  APIStopChat -> False
  APIActivateChat _ -> False
  APISuspendChat _ -> False
  QuitChat -> False
  SetTempFolder _ -> False
  SetFilesFolder _ -> False
  SetRemoteHostsFolder _ -> False
  APISetEncryptLocalFiles _ -> False
  APIExportArchive _ -> False
  APIImportArchive _ -> False
  ExportArchive -> False
  APIDeleteStorage -> False
  APIStorageEncryption _ -> False
  APISetNetworkConfig _ -> False
  APIGetNetworkConfig -> False
  SetLocalDeviceName _ -> False
  ListRemoteHosts -> False
  StartRemoteHost {} -> False
  SwitchRemoteHost {} -> False
  StoreRemoteFile {} -> False
  GetRemoteFile {} -> False
  StopRemoteHost _ -> False
  DeleteRemoteHost _ -> False
  ConnectRemoteCtrl {} -> False
  FindKnownRemoteCtrl -> False
  ConfirmRemoteCtrl _ -> False
  VerifyRemoteCtrlSession {} -> False
  ListRemoteCtrls -> False
  StopRemoteCtrl -> False
  DeleteRemoteCtrl _ -> False
  ExecChatStoreSQL _ -> False
  ExecAgentStoreSQL _ -> False
  SlowSQLQueries -> False
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
  | CRContactInfo {user :: User, contact :: Contact, connectionStats_ :: Maybe ConnectionStats, customUserProfile :: Maybe Profile}
  | CRGroupInfo {user :: User, groupInfo :: GroupInfo, groupSummary :: GroupSummary}
  | CRGroupMemberInfo {user :: User, groupInfo :: GroupInfo, member :: GroupMember, connectionStats_ :: Maybe ConnectionStats}
  | CRQueueInfo {user :: User, rcvMsgInfo :: Maybe RcvMsgInfo, queueInfo :: ServerQueueInfo}
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
  | CRNewChatItems {user :: User, chatItems :: [AChatItem]}
  | CRChatItemsStatusesUpdated {user :: User, chatItems :: [AChatItem]}
  | CRChatItemUpdated {user :: User, chatItem :: AChatItem}
  | CRChatItemNotChanged {user :: User, chatItem :: AChatItem}
  | CRChatItemReaction {user :: User, added :: Bool, reaction :: ACIReaction}
  | CRChatItemsDeleted {user :: User, chatItemDeletions :: [ChatItemDeletion], byUser :: Bool, timed :: Bool}
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
  | CRGroupLinkConnecting {user :: User, groupInfo :: GroupInfo, hostMember :: GroupMember}
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
  | CRConnectionUserChanged {user :: User, fromConnection :: PendingContactConnection, toConnection :: PendingContactConnection, newUser :: User}
  | CRConnectionPlan {user :: User, connectionPlan :: ConnectionPlan}
  | CRSentConfirmation {user :: User, connection :: PendingContactConnection}
  | CRSentInvitation {user :: User, connection :: PendingContactConnection, customUserProfile :: Maybe Profile}
  | CRSentInvitationToContact {user :: User, contact :: Contact, customUserProfile :: Maybe Profile}
  | CRContactUpdated {user :: User, fromContact :: Contact, toContact :: Contact}
  | CRGroupMemberUpdated {user :: User, groupInfo :: GroupInfo, fromMember :: GroupMember, toMember :: GroupMember}
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
  | CRForwardPlan {user :: User, itemsCount :: Int, chatItemIds :: [ChatItemId], forwardConfirmation :: Maybe ForwardConfirmation}
  | CRRcvFileDescrReady {user :: User, chatItem :: AChatItem, rcvFileTransfer :: RcvFileTransfer, rcvFileDescr :: RcvFileDescr}
  | CRRcvFileAccepted {user :: User, chatItem :: AChatItem}
  | CRRcvFileAcceptedSndCancelled {user :: User, rcvFileTransfer :: RcvFileTransfer}
  | CRStandaloneFileInfo {fileMeta :: Maybe J.Value}
  | CRRcvStandaloneFileCreated {user :: User, rcvFileTransfer :: RcvFileTransfer} -- returned by _download
  | CRRcvFileStart {user :: User, chatItem :: AChatItem} -- sent by chats
  | CRRcvFileProgressXFTP {user :: User, chatItem_ :: Maybe AChatItem, receivedSize :: Int64, totalSize :: Int64, rcvFileTransfer :: RcvFileTransfer}
  | CRRcvFileComplete {user :: User, chatItem :: AChatItem}
  | CRRcvStandaloneFileComplete {user :: User, targetPath :: FilePath, rcvFileTransfer :: RcvFileTransfer}
  | CRRcvFileCancelled {user :: User, chatItem_ :: Maybe AChatItem, rcvFileTransfer :: RcvFileTransfer}
  | CRRcvFileSndCancelled {user :: User, chatItem :: AChatItem, rcvFileTransfer :: RcvFileTransfer}
  | CRRcvFileError {user :: User, chatItem_ :: Maybe AChatItem, agentError :: AgentErrorType, rcvFileTransfer :: RcvFileTransfer}
  | CRRcvFileWarning {user :: User, chatItem_ :: Maybe AChatItem, agentError :: AgentErrorType, rcvFileTransfer :: RcvFileTransfer}
  | CRSndFileStart {user :: User, chatItem :: AChatItem, sndFileTransfer :: SndFileTransfer}
  | CRSndFileComplete {user :: User, chatItem :: AChatItem, sndFileTransfer :: SndFileTransfer}
  | CRSndFileRcvCancelled {user :: User, chatItem_ :: Maybe AChatItem, sndFileTransfer :: SndFileTransfer}
  | CRSndFileCancelled {user :: User, chatItem_ :: Maybe AChatItem, fileTransferMeta :: FileTransferMeta, sndFileTransfers :: [SndFileTransfer]}
  | CRSndStandaloneFileCreated {user :: User, fileTransferMeta :: FileTransferMeta} -- returned by _upload
  | CRSndFileStartXFTP {user :: User, chatItem :: AChatItem, fileTransferMeta :: FileTransferMeta} -- not used
  | CRSndFileProgressXFTP {user :: User, chatItem_ :: Maybe AChatItem, fileTransferMeta :: FileTransferMeta, sentSize :: Int64, totalSize :: Int64}
  | CRSndFileRedirectStartXFTP {user :: User, fileTransferMeta :: FileTransferMeta, redirectMeta :: FileTransferMeta}
  | CRSndFileCompleteXFTP {user :: User, chatItem :: AChatItem, fileTransferMeta :: FileTransferMeta}
  | CRSndStandaloneFileComplete {user :: User, fileTransferMeta :: FileTransferMeta, rcvURIs :: [Text]}
  | CRSndFileCancelledXFTP {user :: User, chatItem_ :: Maybe AChatItem, fileTransferMeta :: FileTransferMeta}
  | CRSndFileError {user :: User, chatItem_ :: Maybe AChatItem, fileTransferMeta :: FileTransferMeta, errorMessage :: Text}
  | CRSndFileWarning {user :: User, chatItem_ :: Maybe AChatItem, fileTransferMeta :: FileTransferMeta, errorMessage :: Text}
  | CRUserProfileUpdated {user :: User, fromProfile :: Profile, toProfile :: Profile, updateSummary :: UserProfileUpdateSummary}
  | CRUserProfileImage {user :: User, profile :: Profile}
  | CRContactAliasUpdated {user :: User, toContact :: Contact}
  | CRConnectionAliasUpdated {user :: User, toConnection :: PendingContactConnection}
  | CRContactPrefsUpdated {user :: User, fromContact :: Contact, toContact :: Contact}
  | CRContactConnecting {user :: User, contact :: Contact}
  | CRContactConnected {user :: User, contact :: Contact, userCustomProfile :: Maybe Profile}
  | CRContactSndReady {user :: User, contact :: Contact}
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
  | CRMemberBlockedForAll {user :: User, groupInfo :: GroupInfo, byMember :: GroupMember, member :: GroupMember, blocked :: Bool}
  | CRMemberBlockedForAllUser {user :: User, groupInfo :: GroupInfo, member :: GroupMember, blocked :: Bool}
  | CRConnectedToGroupMember {user :: User, groupInfo :: GroupInfo, member :: GroupMember, memberContact :: Maybe Contact}
  | CRDeletedMember {user :: User, groupInfo :: GroupInfo, byMember :: GroupMember, deletedMember :: GroupMember}
  | CRDeletedMemberUser {user :: User, groupInfo :: GroupInfo, member :: GroupMember}
  | CRLeftMember {user :: User, groupInfo :: GroupInfo, member :: GroupMember}
  | CRUnknownMemberCreated {user :: User, groupInfo :: GroupInfo, forwardedByMember :: GroupMember, member :: GroupMember}
  | CRUnknownMemberBlocked {user :: User, groupInfo :: GroupInfo, blockedByMember :: GroupMember, member :: GroupMember}
  | CRUnknownMemberAnnounced {user :: User, groupInfo :: GroupInfo, announcingMember :: GroupMember, unknownMember :: GroupMember, announcedMember :: GroupMember}
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
  | CRAcceptingGroupJoinRequestMember {user :: User, groupInfo :: GroupInfo, member :: GroupMember}
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
  | CRNtfToken {token :: DeviceToken, status :: NtfTknStatus, ntfMode :: NotificationsMode, ntfServer :: NtfServer}
  | CRNtfMessages {user_ :: Maybe User, connEntity_ :: Maybe ConnectionEntity, expectedMsg_ :: Maybe NtfMsgInfo, receivedMsg_ :: Maybe NtfMsgInfo}
  | CRConnNtfMessage {receivedMsg_ :: Maybe NtfMsgInfo}
  | CRNtfMessage {user :: User, connEntity :: ConnectionEntity, ntfMessage :: NtfMsgAckInfo}
  | CRContactConnectionDeleted {user :: User, connection :: PendingContactConnection}
  | CRRemoteHostList {remoteHosts :: [RemoteHostInfo]}
  | CRCurrentRemoteHost {remoteHost_ :: Maybe RemoteHostInfo}
  | CRRemoteHostStarted {remoteHost_ :: Maybe RemoteHostInfo, invitation :: Text, ctrlPort :: String, localAddrs :: NonEmpty RCCtrlAddress}
  | CRRemoteHostSessionCode {remoteHost_ :: Maybe RemoteHostInfo, sessionCode :: Text}
  | CRNewRemoteHost {remoteHost :: RemoteHostInfo}
  | CRRemoteHostConnected {remoteHost :: RemoteHostInfo}
  | CRRemoteHostStopped {remoteHostId_ :: Maybe RemoteHostId, rhsState :: RemoteHostSessionState, rhStopReason :: RemoteHostStopReason}
  | CRRemoteFileStored {remoteHostId :: RemoteHostId, remoteFileSource :: CryptoFile}
  | CRRemoteCtrlList {remoteCtrls :: [RemoteCtrlInfo]}
  | CRRemoteCtrlFound {remoteCtrl :: RemoteCtrlInfo, ctrlAppInfo_ :: Maybe CtrlAppInfo, appVersion :: AppVersion, compatible :: Bool}
  | CRRemoteCtrlConnecting {remoteCtrl_ :: Maybe RemoteCtrlInfo, ctrlAppInfo :: CtrlAppInfo, appVersion :: AppVersion}
  | CRRemoteCtrlSessionCode {remoteCtrl_ :: Maybe RemoteCtrlInfo, sessionCode :: Text}
  | CRRemoteCtrlConnected {remoteCtrl :: RemoteCtrlInfo}
  | CRRemoteCtrlStopped {rcsState :: RemoteCtrlSessionState, rcStopReason :: RemoteCtrlStopReason}
  | CRContactPQEnabled {user :: User, contact :: Contact, pqEnabled :: PQEncryption}
  | CRSQLResult {rows :: [Text]}
  | CRSlowSQLQueries {chatQueries :: [SlowSQLQuery], agentQueries :: [SlowSQLQuery]}
  | CRDebugLocks {chatLockName :: Maybe String, chatEntityLocks :: Map String String, agentLocks :: AgentLocks}
  | CRAgentSubsTotal {user :: User, subsTotal :: SMPServerSubs, hasSession :: Bool}
  | CRAgentServersSummary {user :: User, serversSummary :: PresentedServersSummary}
  | CRAgentWorkersDetails {agentWorkersDetails :: AgentWorkersDetails}
  | CRAgentWorkersSummary {agentWorkersSummary :: AgentWorkersSummary}
  | CRAgentSubs {activeSubs :: Map Text Int, pendingSubs :: Map Text Int, removedSubs :: Map Text [String]}
  | CRAgentSubsDetails {agentSubs :: SubscriptionsInfo}
  | CRAgentQueuesInfo {agentQueuesInfo :: AgentQueuesInfo}
  | CRContactDisabled {user :: User, contact :: Contact}
  | CRConnectionDisabled {connectionEntity :: ConnectionEntity}
  | CRConnectionInactive {connectionEntity :: ConnectionEntity, inactive :: Bool}
  | CRAgentRcvQueueDeleted {agentConnId :: AgentConnId, server :: SMPServer, agentQueueId :: AgentQueueId, agentError_ :: Maybe AgentErrorType}
  | CRAgentConnDeleted {agentConnId :: AgentConnId}
  | CRAgentUserDeleted {agentUserId :: Int64}
  | CRMessageError {user :: User, severity :: Text, errorMessage :: Text}
  | CRChatCmdError {user_ :: Maybe User, chatError :: ChatError}
  | CRChatError {user_ :: Maybe User, chatError :: ChatError}
  | CRChatErrors {user_ :: Maybe User, chatErrors :: [ChatError]}
  | CRArchiveExported {archiveErrors :: [ArchiveError]}
  | CRArchiveImported {archiveErrors :: [ArchiveError]}
  | CRAppSettings {appSettings :: AppSettings}
  | CRTimedAction {action :: String, durationMilliseconds :: Int64}
  | CRCustomChatResponse {user_ :: Maybe User, response :: Text}
  deriving (Show)

-- some of these can only be used as command responses
allowRemoteEvent :: ChatResponse -> Bool
allowRemoteEvent = \case
  CRChatStarted -> False
  CRChatRunning -> False
  CRChatStopped -> False
  CRChatSuspended -> False
  CRRemoteHostList _ -> False
  CRCurrentRemoteHost _ -> False
  CRRemoteHostStarted {} -> False
  CRRemoteHostSessionCode {} -> False
  CRNewRemoteHost _ -> False
  CRRemoteHostConnected _ -> False
  CRRemoteHostStopped {} -> False
  CRRemoteFileStored {} -> False
  CRRemoteCtrlList _ -> False
  CRRemoteCtrlFound {} -> False
  CRRemoteCtrlConnecting {} -> False
  CRRemoteCtrlSessionCode {} -> False
  CRRemoteCtrlConnected _ -> False
  CRRemoteCtrlStopped {} -> False
  CRSQLResult _ -> False
  CRSlowSQLQueries {} -> False
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

data ChatPagination
  = CPLast Int
  | CPAfter ChatItemId Int
  | CPBefore ChatItemId Int
  deriving (Show)

data PaginationByTime
  = PTLast Int
  | PTAfter UTCTime Int
  | PTBefore UTCTime Int
  deriving (Show)

data ChatListQuery
  = CLQFilters {favorite :: Bool, unread :: Bool}
  | CLQSearch {search :: String}
  deriving (Show)

clqNoFilters :: ChatListQuery
clqNoFilters = CLQFilters {favorite = False, unread = False}

data ChatDeleteMode
  = CDMFull {notify :: Bool} -- delete both contact and conversation
  | CDMEntity {notify :: Bool} -- delete contact (connection), keep conversation
  | CDMMessages -- delete conversation, keep contact - can be re-opened from Contacts view
  deriving (Show)

data ConnectionPlan
  = CPInvitationLink {invitationLinkPlan :: InvitationLinkPlan}
  | CPContactAddress {contactAddressPlan :: ContactAddressPlan}
  | CPGroupLink {groupLinkPlan :: GroupLinkPlan}
  deriving (Show)

data InvitationLinkPlan
  = ILPOk
  | ILPOwnLink
  | ILPConnecting {contact_ :: Maybe Contact}
  | ILPKnown {contact :: Contact}
  deriving (Show)

data ContactAddressPlan
  = CAPOk
  | CAPOwnLink
  | CAPConnectingConfirmReconnect
  | CAPConnectingProhibit {contact :: Contact}
  | CAPKnown {contact :: Contact}
  | CAPContactViaAddress {contact :: Contact}
  deriving (Show)

data GroupLinkPlan
  = GLPOk
  | GLPOwnLink {groupInfo :: GroupInfo}
  | GLPConnectingConfirmReconnect
  | GLPConnectingProhibit {groupInfo_ :: Maybe GroupInfo}
  | GLPKnown {groupInfo :: GroupInfo}
  deriving (Show)

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
    CAPContactViaAddress _ -> True
    _ -> False
  CPGroupLink glp -> case glp of
    GLPOk -> True
    GLPOwnLink _ -> True
    GLPConnectingConfirmReconnect -> True
    _ -> False

data ForwardConfirmation
  = FCFilesNotAccepted {fileIds :: [FileTransferId]}
  | FCFilesInProgress {filesCount :: Int}
  | FCFilesMissing {filesCount :: Int}
  | FCFilesFailed {filesCount :: Int}
  deriving (Show)

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
  deriving (Show)

data AProtoServersConfig = forall p. ProtocolTypeI p => APSC (SProtocolType p) (ProtoServersConfig p)

deriving instance Show AProtoServersConfig

data UserProtoServers p = UserProtoServers
  { serverProtocol :: SProtocolType p,
    protoServers :: NonEmpty (ServerCfg p),
    presetServers :: NonEmpty (ServerCfg p)
  }
  deriving (Show)

data AUserProtoServers = forall p. (ProtocolTypeI p, UserProtocol p) => AUPS (UserProtoServers p)

deriving instance Show AUserProtoServers

data ArchiveConfig = ArchiveConfig {archivePath :: FilePath, disableCompression :: Maybe Bool, parentTempDirectory :: Maybe FilePath}
  deriving (Show)

data DBEncryptionConfig = DBEncryptionConfig {currentKey :: DBEncryptionKey, newKey :: DBEncryptionKey, keepKey :: Maybe Bool}
  deriving (Show)

newtype DBEncryptionKey = DBEncryptionKey ScrubbedBytes
  deriving (Show)

instance IsString DBEncryptionKey where fromString = parseString $ parseAll strP

instance StrEncoding DBEncryptionKey where
  strEncode (DBEncryptionKey s) = BA.convert s
  strP = DBEncryptionKey . BA.convert <$> A.takeWhile (\c -> c /= ' ' && ord c >= 0x21 && ord c <= 0x7E)

instance FromJSON DBEncryptionKey where
  parseJSON = strParseJSON "DBEncryptionKey"

data AppFilePathsConfig = AppFilePathsConfig
  { appFilesFolder :: FilePath,
    appTempFolder :: FilePath,
    appAssetsFolder :: FilePath,
    appRemoteHostsFolder :: Maybe FilePath
  }
  deriving (Show)

data SimpleNetCfg = SimpleNetCfg
  { socksProxy :: Maybe SocksProxyWithAuth,
    socksMode :: SocksMode,
    hostMode :: HostMode,
    requiredHostMode :: Bool,
    smpProxyMode_ :: Maybe SMPProxyMode,
    smpProxyFallback_ :: Maybe SMPProxyFallback,
    smpWebPort :: Bool,
    tcpTimeout_ :: Maybe Int,
    logTLSErrors :: Bool
  }
  deriving (Show)

defaultSimpleNetCfg :: SimpleNetCfg
defaultSimpleNetCfg =
  SimpleNetCfg
    { socksProxy = Nothing,
      socksMode = SMAlways,
      hostMode = HMOnionViaSocks,
      requiredHostMode = False,
      smpProxyMode_ = Nothing,
      smpProxyFallback_ = Nothing,
      smpWebPort = False,
      tcpTimeout_ = Nothing,
      logTLSErrors = False
    }  

data ContactSubStatus = ContactSubStatus
  { contact :: Contact,
    contactError :: Maybe ChatError
  }
  deriving (Show)

data MemberSubStatus = MemberSubStatus
  { member :: GroupMember,
    memberError :: Maybe ChatError
  }
  deriving (Show)

data UserContactSubStatus = UserContactSubStatus
  { userContact :: UserContact,
    userContactError :: Maybe ChatError
  }
  deriving (Show)

data PendingSubStatus = PendingSubStatus
  { connection :: PendingContactConnection,
    connError :: Maybe ChatError
  }
  deriving (Show)

data UserProfileUpdateSummary = UserProfileUpdateSummary
  { updateSuccesses :: Int,
    updateFailures :: Int,
    changedContacts :: [Contact]
  }
  deriving (Show)

data ComposedMessage = ComposedMessage
  { fileSource :: Maybe CryptoFile,
    quotedItemId :: Maybe ChatItemId,
    msgContent :: MsgContent
  }
  deriving (Show)

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

data NtfMsgInfo = NtfMsgInfo {msgId :: Text, msgTs :: UTCTime}
  deriving (Show)

receivedMsgInfo :: SMPMsgMeta -> NtfMsgInfo
receivedMsgInfo SMPMsgMeta {msgId, msgTs} = ntfMsgInfo_ msgId msgTs

expectedMsgInfo :: NMsgMeta -> NtfMsgInfo
expectedMsgInfo NMsgMeta {msgId, msgTs} = ntfMsgInfo_ msgId msgTs

ntfMsgInfo_ :: MsgId -> SystemTime -> NtfMsgInfo
ntfMsgInfo_ msgId msgTs = NtfMsgInfo {msgId = decodeLatin1 $ strEncode msgId, msgTs = systemToUTCTime msgTs}

-- Acknowledged message info - used to correlate with expected message
data NtfMsgAckInfo = NtfMsgAckInfo {msgId :: Text, msgTs_ :: Maybe UTCTime}
  deriving (Show)

ntfMsgAckInfo :: MsgId -> Maybe UTCTime -> NtfMsgAckInfo
ntfMsgAckInfo msgId msgTs_ = NtfMsgAckInfo {msgId = decodeLatin1 $ strEncode msgId, msgTs_}

crNtfToken :: (DeviceToken, NtfTknStatus, NotificationsMode, NtfServer) -> ChatResponse
crNtfToken (token, status, ntfMode, ntfServer) = CRNtfToken {token, status, ntfMode, ntfServer}

data SwitchProgress = SwitchProgress
  { queueDirection :: QueueDirection,
    switchPhase :: SwitchPhase,
    connectionStats :: ConnectionStats
  }
  deriving (Show)

data RatchetSyncProgress = RatchetSyncProgress
  { ratchetSyncStatus :: RatchetSyncState,
    connectionStats :: ConnectionStats
  }
  deriving (Show)

data ParsedServerAddress = ParsedServerAddress
  { serverAddress :: Maybe ServerAddress,
    parseError :: String
  }
  deriving (Show)

data ServerAddress = ServerAddress
  { serverProtocol :: AProtocolType,
    hostnames :: NonEmpty String,
    port :: String,
    keyHash :: String,
    basicAuth :: String
  }
  deriving (Show)

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

data ChatItemDeletion = ChatItemDeletion
  { deletedChatItem :: AChatItem,
    toChatItem :: Maybe AChatItem
  }
  deriving (Show)

data ChatLogLevel = CLLDebug | CLLInfo | CLLWarning | CLLError | CLLImportant
  deriving (Eq, Ord, Show)

data CoreVersionInfo = CoreVersionInfo
  { version :: String,
    simplexmqVersion :: String,
    simplexmqCommit :: String
  }
  deriving (Show)

data SlowSQLQuery = SlowSQLQuery
  { query :: Text,
    queryStats :: SlowQueryStats
  }
  deriving (Show)

data ChatError
  = ChatError {errorType :: ChatErrorType}
  | ChatErrorAgent {agentError :: AgentErrorType, connectionEntity_ :: Maybe ConnectionEntity}
  | ChatErrorStore {storeError :: StoreError}
  | ChatErrorDatabase {databaseError :: DatabaseError}
  | ChatErrorRemoteCtrl {remoteCtrlError :: RemoteCtrlError}
  | ChatErrorRemoteHost {rhKey :: RHKey, remoteHostError :: RemoteHostError}
  deriving (Show, Exception)

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
  | CECantBlockMemberForSelf {groupInfo :: GroupInfo, member :: GroupMember, setShowMessages :: Bool}
  | CEGroupMemberUserRemoved
  | CEGroupMemberNotFound
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
  | CEFileNotApproved {fileId :: FileTransferId, unknownServers :: [XFTPServer]}
  | CEFallbackToSMPProhibited {fileId :: FileTransferId}
  | CEInlineFileProhibited {fileId :: FileTransferId}
  | CEInvalidQuote
  | CEInvalidForward
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
  | CEConnectionUserChangeProhibited
  | CEPeerChatVRangeIncompatible
  | CEInternalError {message :: String}
  | CEException {message :: String}
  deriving (Show, Exception)

data DatabaseError
  = DBErrorEncrypted
  | DBErrorPlaintext
  | DBErrorNoFile {dbFile :: String}
  | DBErrorExport {sqliteError :: SQLiteError}
  | DBErrorOpen {sqliteError :: SQLiteError}
  deriving (Show, Exception)

data SQLiteError = SQLiteErrorNotADatabase | SQLiteError {dbError :: String}
  deriving (Show, Exception)

throwDBError :: DatabaseError -> CM ()
throwDBError = throwError . ChatErrorDatabase

-- TODO review errors, some of it can be covered by HTTP2 errors
data RemoteHostError
  = RHEMissing -- No remote session matches this identifier
  | RHEInactive -- A session exists, but not active
  | RHEBusy -- A session is already running
  | RHETimeout
  | RHEBadState -- Illegal state transition
  | RHEBadVersion {appVersion :: AppVersion}
  | RHELocalCommand -- Command not allowed for remote execution
  | RHEDisconnected {reason :: Text} -- TODO should be sent when disconnected?
  | RHEProtocolError RemoteProtocolError
  deriving (Show, Exception)

data RemoteHostStopReason
  = RHSRConnectionFailed {chatError :: ChatError}
  | RHSRCrashed {chatError :: ChatError}
  | RHSRDisconnected
  deriving (Show, Exception)

-- TODO review errors, some of it can be covered by HTTP2 errors
data RemoteCtrlError
  = RCEInactive -- No session is running
  | RCEBadState -- A session is in a wrong state for the current operation
  | RCEBusy -- A session is already running
  | RCETimeout
  | RCENoKnownControllers -- No previously-contacted controllers to discover
  | RCEBadController -- Attempting to confirm a found controller with another ID
  | -- | A session disconnected by a controller
    RCEDisconnected {remoteCtrlId :: RemoteCtrlId, reason :: Text}
  | RCEBadInvitation
  | RCEBadVersion {appVersion :: AppVersion}
  | RCEHTTP2Error {http2Error :: Text} -- TODO currently not used
  | RCEProtocolError {protocolError :: RemoteProtocolError}
  deriving (Show, Exception)

data RemoteCtrlStopReason
  = RCSRDiscoveryFailed {chatError :: ChatError}
  | RCSRConnectionFailed {chatError :: ChatError}
  | RCSRSetupFailed {chatError :: ChatError}
  | RCSRDisconnected
  deriving (Show, Exception)

data ArchiveError
  = AEImport {importError :: String}
  | AEFileError {file :: String, fileError :: String}
  deriving (Show, Exception)

-- | Host (mobile) side of transport to process remote commands and forward notifications
data RemoteCtrlSession
  = RCSessionStarting
  | RCSessionSearching
      { action :: Async (),
        foundCtrl :: TMVar (RemoteCtrl, RCVerifiedInvitation)
      }
  | RCSessionConnecting
      { remoteCtrlId_ :: Maybe RemoteCtrlId,
        rcsClient :: RCCtrlClient,
        rcsWaitSession :: Async ()
      }
  | RCSessionPendingConfirmation
      { remoteCtrlId_ :: Maybe RemoteCtrlId,
        ctrlDeviceName :: Text,
        rcsClient :: RCCtrlClient,
        tls :: TLS,
        sessionCode :: Text,
        rcsWaitSession :: Async (),
        rcsWaitConfirmation :: TMVar (Either RCErrorType (RCCtrlSession, RCCtrlPairing))
      }
  | RCSessionConnected
      { remoteCtrlId :: RemoteCtrlId,
        rcsClient :: RCCtrlClient,
        tls :: TLS,
        rcsSession :: RCCtrlSession,
        http2Server :: Async (),
        remoteOutputQ :: TBQueue ChatResponse
      }

data RemoteCtrlSessionState
  = RCSStarting
  | RCSSearching
  | RCSConnecting
  | RCSPendingConfirmation {sessionCode :: Text}
  | RCSConnected {sessionCode :: Text}
  deriving (Show)

rcsSessionState :: RemoteCtrlSession -> RemoteCtrlSessionState
rcsSessionState = \case
  RCSessionStarting -> RCSStarting
  RCSessionSearching {} -> RCSSearching
  RCSessionConnecting {} -> RCSConnecting
  RCSessionPendingConfirmation {tls} -> RCSPendingConfirmation {sessionCode = tlsSessionCode tls}
  RCSessionConnected {tls} -> RCSConnected {sessionCode = tlsSessionCode tls}

-- | UI-accessible remote controller information
data RemoteCtrlInfo = RemoteCtrlInfo
  { remoteCtrlId :: RemoteCtrlId,
    ctrlDeviceName :: Text,
    sessionState :: Maybe RemoteCtrlSessionState
  }
  deriving (Show)

type CM' a = ReaderT ChatController IO a

type CM a = ExceptT ChatError (ReaderT ChatController IO) a

chatReadVar :: (ChatController -> TVar a) -> CM a
chatReadVar = lift . chatReadVar'
{-# INLINE chatReadVar #-}

chatReadVar' :: (ChatController -> TVar a) -> CM' a
chatReadVar' f = asks f >>= readTVarIO
{-# INLINE chatReadVar' #-}

chatWriteVar :: (ChatController -> TVar a) -> a -> CM ()
chatWriteVar f = lift . chatWriteVar' f
{-# INLINE chatWriteVar #-}

chatWriteVar' :: (ChatController -> TVar a) -> a -> CM' ()
chatWriteVar' f value = asks f >>= atomically . (`writeTVar` value)
{-# INLINE chatWriteVar' #-}

chatModifyVar :: (ChatController -> TVar a) -> (a -> a) -> CM ()
chatModifyVar f = lift . chatModifyVar' f
{-# INLINE chatModifyVar #-}

chatModifyVar' :: (ChatController -> TVar a) -> (a -> a) -> CM' ()
chatModifyVar' f newValue = asks f >>= atomically . (`modifyTVar'` newValue)
{-# INLINE chatModifyVar' #-}

setContactNetworkStatus :: Contact -> NetworkStatus -> CM' ()
setContactNetworkStatus Contact {activeConn = Nothing} _ = pure ()
setContactNetworkStatus Contact {activeConn = Just Connection {agentConnId}} status = chatModifyVar' connNetworkStatuses $ M.insert agentConnId status

tryChatError :: CM a -> CM (Either ChatError a)
tryChatError = tryAllErrors mkChatError
{-# INLINE tryChatError #-}

tryChatError' :: CM a -> CM' (Either ChatError a)
tryChatError' = tryAllErrors' mkChatError
{-# INLINE tryChatError' #-}

catchChatError :: CM a -> (ChatError -> CM a) -> CM a
catchChatError = catchAllErrors mkChatError
{-# INLINE catchChatError #-}

catchChatError' :: CM a -> (ChatError -> CM' a) -> CM' a
catchChatError' = catchAllErrors' mkChatError
{-# INLINE catchChatError' #-}

chatFinally :: CM a -> CM b -> CM a
chatFinally = allFinally mkChatError
{-# INLINE chatFinally #-}

onChatError :: CM a -> CM b -> CM a
a `onChatError` onErr = a `catchChatError` \e -> onErr >> throwError e
{-# INLINE onChatError #-}

mkChatError :: SomeException -> ChatError
mkChatError = ChatError . CEException . show
{-# INLINE mkChatError #-}

catchStoreError :: ExceptT StoreError IO a -> (StoreError -> ExceptT StoreError IO a) -> ExceptT StoreError IO a
catchStoreError = catchAllErrors mkStoreError
{-# INLINE catchStoreError #-}

tryStoreError' :: ExceptT StoreError IO a -> IO (Either StoreError a)
tryStoreError' = tryAllErrors' mkStoreError
{-# INLINE tryStoreError' #-}

mkStoreError :: SomeException -> StoreError
mkStoreError = SEInternalError . show
{-# INLINE mkStoreError #-}

chatCmdError :: Maybe User -> String -> ChatResponse
chatCmdError user = CRChatCmdError user . ChatError . CECommandError

throwChatError :: ChatErrorType -> CM a
throwChatError = throwError . ChatError

-- | Emit local events.
toView :: ChatResponse -> CM ()
toView = lift . toView'
{-# INLINE toView #-}

toView' :: ChatResponse -> CM' ()
toView' ev = do
  cc@ChatController {outputQ = localQ, remoteCtrlSession = session, config = ChatConfig {chatHooks}} <- ask
  event <- liftIO $ eventHook chatHooks cc ev
  atomically $
    readTVar session >>= \case
      Just (_, RCSessionConnected {remoteOutputQ})
        | allowRemoteEvent event -> writeTBQueue remoteOutputQ event
      -- TODO potentially, it should hold some events while connecting
      _ -> writeTBQueue localQ (Nothing, Nothing, event)

withStore' :: (DB.Connection -> IO a) -> CM a
withStore' action = withStore $ liftIO . action
{-# INLINE withStore' #-}

withFastStore' :: (DB.Connection -> IO a) -> CM a
withFastStore' action = withFastStore $ liftIO . action
{-# INLINE withFastStore' #-}

withStore :: (DB.Connection -> ExceptT StoreError IO a) -> CM a
withStore = withStorePriority False
{-# INLINE withStore #-}

withFastStore :: (DB.Connection -> ExceptT StoreError IO a) -> CM a
withFastStore = withStorePriority True
{-# INLINE withFastStore #-}

withStorePriority :: Bool -> (DB.Connection -> ExceptT StoreError IO a) -> CM a
withStorePriority priority action = do
  ChatController {chatStore} <- ask
  liftIOEither $ withTransactionPriority chatStore priority (runExceptT . withExceptT ChatErrorStore . action) `E.catches` handleDBErrors

withStoreBatch :: Traversable t => (DB.Connection -> t (IO (Either ChatError a))) -> CM' (t (Either ChatError a))
withStoreBatch actions = do
  ChatController {chatStore} <- ask
  liftIO $ withTransaction chatStore $ mapM (`E.catches` handleDBErrors) . actions

handleDBErrors :: [E.Handler IO (Either ChatError a)]
handleDBErrors =
  [ E.Handler $ \(e :: SQLError) ->
      let se = SQL.sqlError e
          busy = se == SQL.ErrorBusy || se == SQL.ErrorLocked
       in pure . Left . ChatErrorStore $ if busy then SEDBBusyError $ show se else SEDBException $ show e,
    E.Handler $ \(E.SomeException e) -> pure . Left . ChatErrorStore . SEDBException $ show e
  ]

withStoreBatch' :: Traversable t => (DB.Connection -> t (IO a)) -> CM' (t (Either ChatError a))
withStoreBatch' actions = withStoreBatch $ fmap (fmap Right) . actions

withAgent :: (AgentClient -> ExceptT AgentErrorType IO a) -> CM a
withAgent action =
  asks smpAgent
    >>= liftIO . runExceptT . action
    >>= liftEither . first (`ChatErrorAgent` Nothing)

withAgent' :: (AgentClient -> IO a) -> CM' a
withAgent' action = asks smpAgent >>= liftIO . action

$(JQ.deriveJSON (enumJSON $ dropPrefix "HS") ''HelpSection)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "CLQ") ''ChatListQuery)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "ILP") ''InvitationLinkPlan)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "CAP") ''ContactAddressPlan)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "GLP") ''GroupLinkPlan)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "CP") ''ConnectionPlan)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "FC") ''ForwardConfirmation)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "CE") ''ChatErrorType)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "RHE") ''RemoteHostError)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "RCE") ''RemoteCtrlError)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "SQLite") ''SQLiteError)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "DB") ''DatabaseError)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "Chat") ''ChatError)

$(JQ.deriveJSON defaultJSON ''AppFilePathsConfig)

$(JQ.deriveJSON defaultJSON ''ContactSubStatus)

$(JQ.deriveJSON defaultJSON ''MemberSubStatus)

$(JQ.deriveJSON defaultJSON ''UserContactSubStatus)

$(JQ.deriveJSON defaultJSON ''PendingSubStatus)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "AE") ''ArchiveError)

$(JQ.deriveJSON defaultJSON ''UserProfileUpdateSummary)

$(JQ.deriveJSON defaultJSON ''NtfMsgInfo)

$(JQ.deriveJSON defaultJSON ''NtfMsgAckInfo)

$(JQ.deriveJSON defaultJSON ''SwitchProgress)

$(JQ.deriveJSON defaultJSON ''RatchetSyncProgress)

$(JQ.deriveJSON defaultJSON ''ServerAddress)

$(JQ.deriveJSON defaultJSON ''ParsedServerAddress)

$(JQ.deriveJSON defaultJSON ''ChatItemDeletion)

$(JQ.deriveJSON defaultJSON ''CoreVersionInfo)

$(JQ.deriveJSON defaultJSON ''SlowSQLQuery)

instance ProtocolTypeI p => FromJSON (ProtoServersConfig p) where
  parseJSON = $(JQ.mkParseJSON defaultJSON ''ProtoServersConfig)

instance ProtocolTypeI p => FromJSON (UserProtoServers p) where
  parseJSON = $(JQ.mkParseJSON defaultJSON ''UserProtoServers)

instance ProtocolTypeI p => ToJSON (UserProtoServers p) where
  toJSON = $(JQ.mkToJSON defaultJSON ''UserProtoServers)
  toEncoding = $(JQ.mkToEncoding defaultJSON ''UserProtoServers)

instance FromJSON AUserProtoServers where
  parseJSON v = J.withObject "AUserProtoServers" parse v
    where
      parse o = do
        AProtocolType (p :: SProtocolType p) <- o .: "serverProtocol"
        case userProtocol p of
          Just Dict -> AUPS <$> J.parseJSON @(UserProtoServers p) v
          Nothing -> fail $ "AUserProtoServers: unsupported protocol " <> show p

instance ToJSON AUserProtoServers where
  toJSON (AUPS s) = $(JQ.mkToJSON defaultJSON ''UserProtoServers) s
  toEncoding (AUPS s) = $(JQ.mkToEncoding defaultJSON ''UserProtoServers) s

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "RCS") ''RemoteCtrlSessionState)

$(JQ.deriveJSON defaultJSON ''RemoteCtrlInfo)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "RCSR") ''RemoteCtrlStopReason)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "RHSR") ''RemoteHostStopReason)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "CR") ''ChatResponse)

$(JQ.deriveFromJSON defaultJSON ''ArchiveConfig)

$(JQ.deriveFromJSON defaultJSON ''DBEncryptionConfig)

$(JQ.deriveToJSON defaultJSON ''ComposedMessage)
