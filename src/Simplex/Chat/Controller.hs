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
import Control.Exception (Exception, SomeException)
import qualified Control.Exception as E
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
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.String
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1)
import Data.Time (NominalDiffTime, UTCTime)
import Data.Time.Clock.System (SystemTime (..), systemToUTCTime)
import Data.Version (showVersion)
import Data.Word (Word16)
import Language.Haskell.TH (Exp, Q, runIO)
import Network.Socket (HostName)
import Numeric.Natural
import qualified Paths_simplex_chat as SC
import Simplex.Chat.AppSettings
import Simplex.Chat.Call
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Operators
import Simplex.Chat.Protocol
import Simplex.Chat.Remote.AppVersion
import Simplex.Chat.Remote.Types
import Simplex.Chat.Stats (PresentedServersSummary)
import Simplex.Chat.Store (AutoAccept, ChatLockEntity, StoreError (..), UserContactLink, GroupLinkInfo, UserMsgReceiptSettings)
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
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction, withTransactionPriority)
import Simplex.Messaging.Agent.Store.Shared (MigrationConfirmation, UpMigration)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Client (HostMode (..), SMPProxyFallback (..), SMPProxyMode (..), SMPWebPortServers (..), SocksMode (..))
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.File (CryptoFile (..))
import qualified Simplex.Messaging.Crypto.File as CF
import Simplex.Messaging.Crypto.Ratchet (PQEncryption)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Notifications.Protocol (DeviceToken (..), NtfTknStatus)
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON, parseAll, parseString, sumTypeJSON)
import Simplex.Messaging.Protocol (AProtoServerWithAuth, AProtocolType (..), MsgId, NMsgMeta (..), NtfServer, ProtocolType (..), QueueId, SMPMsgMeta (..), SubscriptionMode (..), XFTPServer)
import Simplex.Messaging.TMap (TMap)
import Simplex.Messaging.Transport (TLS, simplexMQVersion)
import Simplex.Messaging.Transport.Client (SocksProxyWithAuth, TransportHost)
import Simplex.Messaging.Util (allFinally, catchAllErrors, catchAllErrors', tryAllErrors, tryAllErrors', (<$$>))
import Simplex.RemoteControl.Client
import Simplex.RemoteControl.Invitation (RCSignedInvitation, RCVerifiedInvitation)
import Simplex.RemoteControl.Types
import System.IO (Handle)
import System.Mem.Weak (Weak)
import UnliftIO.STM
#if !defined(dbPostgres)
import Database.SQLite.Simple (SQLError)
import qualified Database.SQLite.Simple as SQL
import Simplex.Messaging.Agent.Store.SQLite.DB (SlowQueryStats (..))
#endif

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
    presetServers :: PresetServers,
    shortLinkPresetServers :: NonEmpty SMPServer,
    presetDomains :: [HostName],
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

data RandomAgentServers = RandomAgentServers
  { smpServers :: NonEmpty (ServerCfg 'PSMP),
    xftpServers :: NonEmpty (ServerCfg 'PXFTP)
  }
  deriving (Show)

-- The hooks can be used to extend or customize chat core in mobile or CLI clients.
data ChatHooks = ChatHooks
  { -- preCmdHook can be used to process or modify the commands before they are processed.
    -- This hook should be used to process CustomChatCommand.
    -- if this hook returns ChatResponse, the command processing will be skipped.
    preCmdHook :: Maybe (ChatController -> ChatCommand -> IO (Either (Either ChatError ChatResponse) ChatCommand)),
    -- eventHook can be used to additionally process or modify events,
    -- it is called before the event is sent to the user (or to the UI).
    eventHook :: Maybe (ChatController -> Either ChatError ChatEvent -> IO (Either ChatError ChatEvent)),
    -- acceptMember hook can be used to accept or reject member connecting via group link without API calls
    acceptMember :: Maybe (GroupInfo -> GroupLinkInfo -> Profile -> IO (Either GroupRejectionReason (GroupAcceptance, GroupMemberRole)))
  }

defaultChatHooks :: ChatHooks
defaultChatHooks = ChatHooks Nothing Nothing Nothing

data PresetServers = PresetServers
  { operators :: NonEmpty PresetOperator,
    ntf :: [NtfServer],
    netCfg :: NetworkConfig
  }
  deriving (Show)

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

data ChatDatabase = ChatDatabase {chatStore :: DBStore, agentStore :: DBStore}

data ChatController = ChatController
  { currentUser :: TVar (Maybe User),
    randomPresetServers :: NonEmpty PresetOperator,
    randomAgentServers :: RandomAgentServers,
    currentRemoteHost :: TVar (Maybe RemoteHostId),
    firstTime :: Bool,
    smpAgent :: AgentClient,
    agentAsync :: TVar (Maybe (Async (), Maybe (Async ()))),
    chatStore :: DBStore,
    chatStoreChanged :: TVar Bool, -- if True, chat should be fully restarted
    random :: TVar ChaChaDRG,
    eventSeq :: TVar Int,
    inputQ :: TBQueue String,
    outputQ :: TBQueue (Maybe RemoteHostId, Either ChatError ChatEvent),
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
#if !defined(dbPostgres)
  | APIExportArchive ArchiveConfig
  | ExportArchive
  | APIImportArchive ArchiveConfig
  | APIDeleteStorage
  | APIStorageEncryption DBEncryptionConfig
  | TestStorageEncryption DBEncryptionKey
  | SlowSQLQueries
#endif
  | ExecChatStoreSQL Text
  | ExecAgentStoreSQL Text
  | APISaveAppSettings AppSettings
  | APIGetAppSettings (Maybe AppSettings)
  | APIGetChatTags UserId
  | APIGetChats {userId :: UserId, pendingConnections :: Bool, pagination :: PaginationByTime, query :: ChatListQuery}
  | APIGetChat ChatRef (Maybe MsgContentTag) ChatPagination (Maybe String)
  | APIGetChatItems ChatPagination (Maybe String)
  | APIGetChatItemInfo ChatRef ChatItemId
  | APISendMessages {sendRef :: SendRef, liveMessage :: Bool, ttl :: Maybe Int, composedMessages :: NonEmpty ComposedMessage}
  | APICreateChatTag ChatTagData
  | APISetChatTags ChatRef (Maybe (NonEmpty ChatTagId))
  | APIDeleteChatTag ChatTagId
  | APIUpdateChatTag ChatTagId ChatTagData
  | APIReorderChatTags (NonEmpty ChatTagId)
  | APICreateChatItems {noteFolderId :: NoteFolderId, composedMessages :: NonEmpty ComposedMessage}
  | APIReportMessage {groupId :: GroupId, chatItemId :: ChatItemId, reportReason :: ReportReason, reportText :: Text}
  | ReportMessage {groupName :: GroupName, contactName_ :: Maybe ContactName, reportReason :: ReportReason, reportedMessage :: Text}
  | APIUpdateChatItem {chatRef :: ChatRef, chatItemId :: ChatItemId, liveMessage :: Bool, updatedMessage :: UpdatedMessage}
  | APIDeleteChatItem ChatRef (NonEmpty ChatItemId) CIDeleteMode
  | APIDeleteMemberChatItem GroupId (NonEmpty ChatItemId)
  | APIArchiveReceivedReports GroupId
  | APIDeleteReceivedReports GroupId (NonEmpty ChatItemId) CIDeleteMode
  | APIChatItemReaction {chatRef :: ChatRef, chatItemId :: ChatItemId, add :: Bool, reaction :: MsgReaction}
  | APIGetReactionMembers UserId GroupId ChatItemId MsgReaction
  | APIPlanForwardChatItems {fromChatRef :: ChatRef, chatItemIds :: NonEmpty ChatItemId}
  | APIForwardChatItems {toChatRef :: ChatRef, fromChatRef :: ChatRef, chatItemIds :: NonEmpty ChatItemId, ttl :: Maybe Int}
  | APIUserRead UserId
  | UserRead
  | APIChatRead ChatRef
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
  | APISetGroupAlias GroupId LocalAlias
  | APISetConnectionAlias Int64 LocalAlias
  | APISetUserUIThemes UserId (Maybe UIThemeEntityOverrides)
  | APISetChatUIThemes ChatRef (Maybe UIThemeEntityOverrides)
  | APIGetNtfToken
  | APIRegisterToken DeviceToken NotificationsMode
  | APIVerifyToken DeviceToken C.CbNonce ByteString
  | APICheckToken DeviceToken
  | APIDeleteToken DeviceToken
  | APIGetNtfConns {nonce :: C.CbNonce, encNtfInfo :: ByteString}
  | APIGetConnNtfMessages (NonEmpty ConnMsgReq)
  | APIAddMember GroupId ContactId GroupMemberRole
  | APIJoinGroup {groupId :: GroupId, enableNtfs :: MsgFilter}
  | APIAcceptMember GroupId GroupMemberId GroupMemberRole
  | APIMembersRole GroupId (NonEmpty GroupMemberId) GroupMemberRole
  | APIBlockMembersForAll GroupId (NonEmpty GroupMemberId) Bool
  | APIRemoveMembers {groupId :: GroupId, groupMemberIds :: Set GroupMemberId, withMessages :: Bool}
  | APILeaveGroup GroupId
  | APIListMembers GroupId
  | APIUpdateGroupProfile GroupId GroupProfile
  | APICreateGroupLink GroupId GroupMemberRole CreateShortLink
  | APIGroupLinkMemberRole GroupId GroupMemberRole
  | APIDeleteGroupLink GroupId
  | APIGetGroupLink GroupId
  | APICreateMemberContact GroupId GroupMemberId
  | APISendMemberContactInvitation {contactId :: ContactId, msgContent_ :: Maybe MsgContent}
  | GetUserProtoServers AProtocolType
  | SetUserProtoServers AProtocolType [AProtoServerWithAuth]
  | APITestProtoServer UserId AProtoServerWithAuth
  | TestProtoServer AProtoServerWithAuth
  | APIGetServerOperators
  | APISetServerOperators (NonEmpty ServerOperator)
  | SetServerOperators (NonEmpty ServerOperatorRoles)
  | APIGetUserServers UserId
  | APISetUserServers UserId (NonEmpty UpdatedUserOperatorServers)
  | APIValidateServers UserId [UpdatedUserOperatorServers] -- response is CRUserServersValidation
  | APIGetUsageConditions
  | APISetConditionsNotified Int64
  | APIAcceptConditions Int64 (NonEmpty Int64)
  | APISetChatItemTTL UserId Int64
  | SetChatItemTTL Int64
  | APIGetChatItemTTL UserId
  | GetChatItemTTL
  | APISetChatTTL UserId ChatRef (Maybe Int64)
  | SetChatTTL ChatName (Maybe Int64)
  | GetChatTTL ChatName
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
  | APIAddContact UserId CreateShortLink IncognitoEnabled
  | AddContact CreateShortLink IncognitoEnabled
  | APISetConnectionIncognito Int64 IncognitoEnabled
  | APIChangeConnectionUser Int64 UserId -- new user id to switch connection to
  | APIConnectPlan UserId AConnectionLink
  | APIConnect UserId IncognitoEnabled (Maybe ACreatedConnLink)
  | Connect IncognitoEnabled (Maybe AConnectionLink)
  | APIConnectContactViaAddress UserId IncognitoEnabled ContactId
  | ConnectSimplex IncognitoEnabled -- UserId (not used in UI)
  | DeleteContact ContactName ChatDeleteMode
  | ClearContact ContactName
  | APIListContacts UserId
  | ListContacts
  | APICreateMyAddress UserId CreateShortLink
  | CreateMyAddress CreateShortLink
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
  | SendMessageBroadcast MsgContent -- UserId (not used in UI)
  | DeleteMessage ChatName Text
  | DeleteMemberMessage GroupName ContactName Text
  | EditMessage {chatName :: ChatName, editedMsg :: Text, message :: Text}
  | UpdateLiveMessage {chatName :: ChatName, chatItemId :: ChatItemId, liveMessage :: Bool, message :: Text}
  | ReactToMessage {add :: Bool, reaction :: MsgReaction, chatName :: ChatName, reactToMessage :: Text}
  | APINewGroup UserId IncognitoEnabled GroupProfile
  | NewGroup IncognitoEnabled GroupProfile
  | AddMember GroupName ContactName GroupMemberRole
  | JoinGroup {groupName :: GroupName, enableNtfs :: MsgFilter}
  | MemberRole GroupName ContactName GroupMemberRole
  | BlockForAll GroupName ContactName Bool
  | RemoveMembers {groupName :: GroupName, members :: Set ContactName, withMessages :: Bool}
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
  | CreateGroupLink GroupName GroupMemberRole CreateShortLink
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
  | DebugEvent ChatEvent
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
#if !defined(dbPostgres)
  APIExportArchive _ -> False
  APIImportArchive _ -> False
  ExportArchive -> False
  APIDeleteStorage -> False
  APIStorageEncryption _ -> False
  SlowSQLQueries -> False
#endif
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
  _ -> True

data ChatResponse
  = CRActiveUser {user :: User}
  | CRUsersList {users :: [UserInfo]}
  | CRChatStarted
  | CRChatRunning
  | CRChatStopped
  | CRApiChats {user :: User, chats :: [AChat]}
  | CRChats {chats :: [AChat]}
  | CRApiChat {user :: User, chat :: AChat, navInfo :: Maybe NavigationInfo}
  | CRChatTags {user :: User, userTags :: [ChatTag]}
  | CRChatItems {user :: User, chatName_ :: Maybe ChatName, chatItems :: [AChatItem]}
  | CRChatItemInfo {user :: User, chatItem :: AChatItem, chatItemInfo :: ChatItemInfo}
  | CRChatItemId User (Maybe ChatItemId)
  | CRServerTestResult {user :: User, testServer :: AProtoServerWithAuth, testFailure :: Maybe ProtocolTestFailure}
  | CRServerOperatorConditions {conditions :: ServerOperatorConditions}
  | CRUserServers {user :: User, userServers :: [UserOperatorServers]}
  | CRUserServersValidation {user :: User, serverErrors :: [UserServersError]}
  | CRUsageConditions {usageConditions :: UsageConditions, conditionsText :: Text, acceptedConditions :: Maybe UsageConditions}
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
  | CRContactRatchetSyncStarted {user :: User, contact :: Contact, connectionStats :: ConnectionStats}
  | CRGroupMemberRatchetSyncStarted {user :: User, groupInfo :: GroupInfo, member :: GroupMember, connectionStats :: ConnectionStats}
  | CRContactCode {user :: User, contact :: Contact, connectionCode :: Text}
  | CRGroupMemberCode {user :: User, groupInfo :: GroupInfo, member :: GroupMember, connectionCode :: Text}
  | CRConnectionVerified {user :: User, verified :: Bool, expectedCode :: Text}
  | CRTagsUpdated {user :: User, userTags :: [ChatTag], chatTags :: [ChatTagId]}
  | CRNewChatItems {user :: User, chatItems :: [AChatItem]}
  | CRChatItemUpdated {user :: User, chatItem :: AChatItem}
  | CRChatItemNotChanged {user :: User, chatItem :: AChatItem}
  | CRChatItemReaction {user :: User, added :: Bool, reaction :: ACIReaction}
  | CRReactionMembers {user :: User, memberReactions :: [MemberReaction]}
  | CRChatItemsDeleted {user :: User, chatItemDeletions :: [ChatItemDeletion], byUser :: Bool, timed :: Bool}
  | CRGroupChatItemsDeleted {user :: User, groupInfo :: GroupInfo, chatItemIDs :: [ChatItemId], byUser :: Bool, member_ :: Maybe GroupMember}
  | CRBroadcastSent {user :: User, msgContent :: MsgContent, successes :: Int, failures :: Int, timestamp :: UTCTime}
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
  | CRUserDeletedMembers {user :: User, groupInfo :: GroupInfo, members :: [GroupMember], withMessages :: Bool}
  | CRGroupsList {user :: User, groups :: [(GroupInfo, GroupSummary)]}
  | CRSentGroupInvitation {user :: User, groupInfo :: GroupInfo, contact :: Contact, member :: GroupMember}
  | CRFileTransferStatus User (FileTransfer, [Integer]) -- TODO refactor this type to FileTransferStatus
  | CRFileTransferStatusXFTP User AChatItem
  | CRUserProfile {user :: User, profile :: Profile}
  | CRUserProfileNoChange {user :: User}
  | CRUserPrivacy {user :: User, updatedUser :: User}
  | CRVersionInfo {versionInfo :: CoreVersionInfo, chatMigrations :: [UpMigration], agentMigrations :: [UpMigration]}
  | CRInvitation {user :: User, connLinkInvitation :: CreatedLinkInvitation, connection :: PendingContactConnection}
  | CRConnectionIncognitoUpdated {user :: User, toConnection :: PendingContactConnection}
  | CRConnectionUserChanged {user :: User, fromConnection :: PendingContactConnection, toConnection :: PendingContactConnection, newUser :: User}
  | CRConnectionPlan {user :: User, connLink :: ACreatedConnLink, connectionPlan :: ConnectionPlan}
  | CRSentConfirmation {user :: User, connection :: PendingContactConnection}
  | CRSentInvitation {user :: User, connection :: PendingContactConnection, customUserProfile :: Maybe Profile}
  | CRSentInvitationToContact {user :: User, contact :: Contact, customUserProfile :: Maybe Profile}
  | CRContactDeleted {user :: User, contact :: Contact}
  | CRChatCleared {user :: User, chatInfo :: AChatInfo}
  | CRUserContactLinkCreated {user :: User, connLinkContact :: CreatedLinkContact}
  | CRUserContactLinkDeleted {user :: User}
  | CRAcceptingContactRequest {user :: User, contact :: Contact}
  | CRContactAlreadyExists {user :: User, contact :: Contact}
  | CRLeftMemberUser {user :: User, groupInfo :: GroupInfo}
  | CRGroupDeletedUser {user :: User, groupInfo :: GroupInfo}
  | CRForwardPlan {user :: User, itemsCount :: Int, chatItemIds :: [ChatItemId], forwardConfirmation :: Maybe ForwardConfirmation}
  | CRRcvFileAccepted {user :: User, chatItem :: AChatItem}
  -- TODO add chatItem :: AChatItem
  | CRRcvFileAcceptedSndCancelled {user :: User, rcvFileTransfer :: RcvFileTransfer}
  | CRStandaloneFileInfo {fileMeta :: Maybe J.Value}
  | CRRcvStandaloneFileCreated {user :: User, rcvFileTransfer :: RcvFileTransfer} -- returned by _download
  | CRRcvFileCancelled {user :: User, chatItem_ :: Maybe AChatItem, rcvFileTransfer :: RcvFileTransfer}
  | CRSndFileCancelled {user :: User, chatItem_ :: Maybe AChatItem, fileTransferMeta :: FileTransferMeta, sndFileTransfers :: [SndFileTransfer]}
  | CRSndStandaloneFileCreated {user :: User, fileTransferMeta :: FileTransferMeta} -- returned by _upload
  | CRUserProfileUpdated {user :: User, fromProfile :: Profile, toProfile :: Profile, updateSummary :: UserProfileUpdateSummary}
  | CRUserProfileImage {user :: User, profile :: Profile}
  | CRContactAliasUpdated {user :: User, toContact :: Contact}
  | CRGroupAliasUpdated {user :: User, toGroup :: GroupInfo}
  | CRConnectionAliasUpdated {user :: User, toConnection :: PendingContactConnection}
  | CRContactPrefsUpdated {user :: User, fromContact :: Contact, toContact :: Contact}
  | CRNetworkStatuses {user_ :: Maybe User, networkStatuses :: [ConnNetworkStatus]}
  | CRJoinedGroupMember {user :: User, groupInfo :: GroupInfo, member :: GroupMember}
  | CRMembersRoleUser {user :: User, groupInfo :: GroupInfo, members :: [GroupMember], toRole :: GroupMemberRole}
  | CRMembersBlockedForAllUser {user :: User, groupInfo :: GroupInfo, members :: [GroupMember], blocked :: Bool}
  | CRGroupUpdated {user :: User, fromGroup :: GroupInfo, toGroup :: GroupInfo, member_ :: Maybe GroupMember}
  | CRGroupProfile {user :: User, groupInfo :: GroupInfo}
  | CRGroupDescription {user :: User, groupInfo :: GroupInfo} -- only used in CLI
  | CRGroupLinkCreated {user :: User, groupInfo :: GroupInfo, connLinkContact :: CreatedLinkContact, memberRole :: GroupMemberRole}
  | CRGroupLink {user :: User, groupInfo :: GroupInfo, connLinkContact :: CreatedLinkContact, memberRole :: GroupMemberRole}
  | CRGroupLinkDeleted {user :: User, groupInfo :: GroupInfo}
  | CRNewMemberContact {user :: User, contact :: Contact, groupInfo :: GroupInfo, member :: GroupMember}
  | CRNewMemberContactSentInv {user :: User, contact :: Contact, groupInfo :: GroupInfo, member :: GroupMember}
  | CRCallInvitations {callInvitations :: [RcvCallInvitation]}
  | CRNtfTokenStatus {status :: NtfTknStatus}
  | CRNtfToken {token :: DeviceToken, status :: NtfTknStatus, ntfMode :: NotificationsMode, ntfServer :: NtfServer}
  | CRNtfConns {ntfConns :: [NtfConn]}
  | CRConnNtfMessages {receivedMsgs :: NonEmpty RcvNtfMsgInfo}
  | CRContactConnectionDeleted {user :: User, connection :: PendingContactConnection}
  | CRRemoteHostList {remoteHosts :: [RemoteHostInfo]}
  | CRCurrentRemoteHost {remoteHost_ :: Maybe RemoteHostInfo}
  | CRRemoteHostStarted {remoteHost_ :: Maybe RemoteHostInfo, invitation :: Text, ctrlPort :: String, localAddrs :: NonEmpty RCCtrlAddress}
  | CRRemoteFileStored {remoteHostId :: RemoteHostId, remoteFileSource :: CryptoFile}
  | CRRemoteCtrlList {remoteCtrls :: [RemoteCtrlInfo]}
  | CRRemoteCtrlConnecting {remoteCtrl_ :: Maybe RemoteCtrlInfo, ctrlAppInfo :: CtrlAppInfo, appVersion :: AppVersion}
  | CRRemoteCtrlConnected {remoteCtrl :: RemoteCtrlInfo}
  | CRSQLResult {rows :: [Text]}
#if !defined(dbPostgres)
  | CRArchiveExported {archiveErrors :: [ArchiveError]}
  | CRArchiveImported {archiveErrors :: [ArchiveError]}
  | CRSlowSQLQueries {chatQueries :: [SlowSQLQuery], agentQueries :: [SlowSQLQuery]}
#endif
  | CRDebugLocks {chatLockName :: Maybe String, chatEntityLocks :: Map String String, agentLocks :: AgentLocks}
  | CRAgentSubsTotal {user :: User, subsTotal :: SMPServerSubs, hasSession :: Bool}
  | CRAgentServersSummary {user :: User, serversSummary :: PresentedServersSummary}
  | CRAgentWorkersDetails {agentWorkersDetails :: AgentWorkersDetails}
  | CRAgentWorkersSummary {agentWorkersSummary :: AgentWorkersSummary}
  | CRAgentSubs {activeSubs :: Map Text Int, pendingSubs :: Map Text Int, removedSubs :: Map Text [String]}
  | CRAgentSubsDetails {agentSubs :: SubscriptionsInfo}
  | CRAgentQueuesInfo {agentQueuesInfo :: AgentQueuesInfo}
  | CRAppSettings {appSettings :: AppSettings}
  | CRCustomChatResponse {user_ :: Maybe User, response :: Text}
  deriving (Show)

data ChatEvent
  = CEvtChatSuspended
  | CEvtContactSwitch {user :: User, contact :: Contact, switchProgress :: SwitchProgress}
  | CEvtGroupMemberSwitch {user :: User, groupInfo :: GroupInfo, member :: GroupMember, switchProgress :: SwitchProgress}
  | CEvtContactRatchetSync {user :: User, contact :: Contact, ratchetSyncProgress :: RatchetSyncProgress}
  | CEvtGroupMemberRatchetSync {user :: User, groupInfo :: GroupInfo, member :: GroupMember, ratchetSyncProgress :: RatchetSyncProgress}
  | CEvtNewChatItems {user :: User, chatItems :: [AChatItem]} -- there is the same command response
  | CEvtChatItemsStatusesUpdated {user :: User, chatItems :: [AChatItem]}
  | CEvtChatItemUpdated {user :: User, chatItem :: AChatItem} -- there is the same command response
  | CEvtChatItemNotChanged {user :: User, chatItem :: AChatItem} -- there is the same command response
  | CEvtChatItemReaction {user :: User, added :: Bool, reaction :: ACIReaction} -- there is the same command response
  | CEvtGroupChatItemsDeleted {user :: User, groupInfo :: GroupInfo, chatItemIDs :: [ChatItemId], byUser :: Bool, member_ :: Maybe GroupMember} -- there is the same command response
  | CEvtChatItemsDeleted {user :: User, chatItemDeletions :: [ChatItemDeletion], byUser :: Bool, timed :: Bool} -- there is the same command response
  | CEvtChatItemDeletedNotFound {user :: User, contact :: Contact, sharedMsgId :: SharedMsgId}
  | CEvtUserAcceptedGroupSent {user :: User, groupInfo :: GroupInfo, hostContact :: Maybe Contact} -- there is the same command response
  | CEvtGroupLinkConnecting {user :: User, groupInfo :: GroupInfo, hostMember :: GroupMember}
  | CEvtBusinessLinkConnecting {user :: User, groupInfo :: GroupInfo, hostMember :: GroupMember, fromContact :: Contact}
  | CEvtSentGroupInvitation {user :: User, groupInfo :: GroupInfo, contact :: Contact, member :: GroupMember} -- there is the same command response
  | CEvtContactUpdated {user :: User, fromContact :: Contact, toContact :: Contact}
  | CEvtGroupMemberUpdated {user :: User, groupInfo :: GroupInfo, fromMember :: GroupMember, toMember :: GroupMember}
  | CEvtContactsMerged {user :: User, intoContact :: Contact, mergedContact :: Contact, updatedContact :: Contact}
  | CEvtContactDeletedByContact {user :: User, contact :: Contact}
  | CEvtReceivedContactRequest {user :: User, contactRequest :: UserContactRequest}
  | CEvtAcceptingContactRequest {user :: User, contact :: Contact} -- there is the same command response
  | CEvtAcceptingBusinessRequest {user :: User, groupInfo :: GroupInfo}
  | CEvtContactRequestAlreadyAccepted {user :: User, contact :: Contact}
  | CEvtBusinessRequestAlreadyAccepted {user :: User, groupInfo :: GroupInfo}
  | CEvtRcvFileDescrReady {user :: User, chatItem :: AChatItem, rcvFileTransfer :: RcvFileTransfer, rcvFileDescr :: RcvFileDescr}
  | CEvtRcvFileAccepted {user :: User, chatItem :: AChatItem} -- there is the same command response
  -- TODO add chatItem :: AChatItem
  | CEvtRcvFileAcceptedSndCancelled {user :: User, rcvFileTransfer :: RcvFileTransfer} -- there is the same command response
  | CEvtRcvFileStart {user :: User, chatItem :: AChatItem} -- sent by chats
  | CEvtRcvFileProgressXFTP {user :: User, chatItem_ :: Maybe AChatItem, receivedSize :: Int64, totalSize :: Int64, rcvFileTransfer :: RcvFileTransfer}
  | CEvtRcvFileComplete {user :: User, chatItem :: AChatItem}
  | CEvtRcvStandaloneFileComplete {user :: User, targetPath :: FilePath, rcvFileTransfer :: RcvFileTransfer}
  | CEvtRcvFileSndCancelled {user :: User, chatItem :: AChatItem, rcvFileTransfer :: RcvFileTransfer}
  | CEvtRcvFileError {user :: User, chatItem_ :: Maybe AChatItem, agentError :: AgentErrorType, rcvFileTransfer :: RcvFileTransfer}
  | CEvtRcvFileWarning {user :: User, chatItem_ :: Maybe AChatItem, agentError :: AgentErrorType, rcvFileTransfer :: RcvFileTransfer}
  | CEvtSndFileStart {user :: User, chatItem :: AChatItem, sndFileTransfer :: SndFileTransfer}
  | CEvtSndFileComplete {user :: User, chatItem :: AChatItem, sndFileTransfer :: SndFileTransfer}
  | CEvtSndFileRcvCancelled {user :: User, chatItem_ :: Maybe AChatItem, sndFileTransfer :: SndFileTransfer}
  | CEvtSndFileStartXFTP {user :: User, chatItem :: AChatItem, fileTransferMeta :: FileTransferMeta} -- not used
  | CEvtSndFileProgressXFTP {user :: User, chatItem_ :: Maybe AChatItem, fileTransferMeta :: FileTransferMeta, sentSize :: Int64, totalSize :: Int64}
  | CEvtSndFileRedirectStartXFTP {user :: User, fileTransferMeta :: FileTransferMeta, redirectMeta :: FileTransferMeta}
  | CEvtSndFileCompleteXFTP {user :: User, chatItem :: AChatItem, fileTransferMeta :: FileTransferMeta}
  | CEvtSndStandaloneFileComplete {user :: User, fileTransferMeta :: FileTransferMeta, rcvURIs :: [Text]}
  | CEvtSndFileCancelledXFTP {user :: User, chatItem_ :: Maybe AChatItem, fileTransferMeta :: FileTransferMeta}
  | CEvtSndFileError {user :: User, chatItem_ :: Maybe AChatItem, fileTransferMeta :: FileTransferMeta, errorMessage :: Text}
  | CEvtSndFileWarning {user :: User, chatItem_ :: Maybe AChatItem, fileTransferMeta :: FileTransferMeta, errorMessage :: Text}
  | CEvtContactConnecting {user :: User, contact :: Contact}
  | CEvtContactConnected {user :: User, contact :: Contact, userCustomProfile :: Maybe Profile}
  | CEvtContactSndReady {user :: User, contact :: Contact}
  | CEvtContactAnotherClient {user :: User, contact :: Contact}
  | CEvtSubscriptionEnd {user :: User, connectionEntity :: ConnectionEntity}
  | CEvtContactsDisconnected {server :: SMPServer, contactRefs :: [ContactRef]}
  | CEvtContactsSubscribed {server :: SMPServer, contactRefs :: [ContactRef]}
  | CEvtContactSubError {user :: User, contact :: Contact, chatError :: ChatError}
  | CEvtContactSubSummary {user :: User, contactSubscriptions :: [ContactSubStatus]}
  | CEvtUserContactSubSummary {user :: User, userContactSubscriptions :: [UserContactSubStatus]}
  | CEvtNetworkStatus {networkStatus :: NetworkStatus, connections :: [AgentConnId]}
  | CEvtNetworkStatuses {user_ :: Maybe User, networkStatuses :: [ConnNetworkStatus]} -- there is the same command response
  | CEvtHostConnected {protocol :: AProtocolType, transportHost :: TransportHost}
  | CEvtHostDisconnected {protocol :: AProtocolType, transportHost :: TransportHost}
  | CEvtReceivedGroupInvitation {user :: User, groupInfo :: GroupInfo, contact :: Contact, fromMemberRole :: GroupMemberRole, memberRole :: GroupMemberRole}
  | CEvtUserJoinedGroup {user :: User, groupInfo :: GroupInfo, hostMember :: GroupMember}
  | CEvtJoinedGroupMember {user :: User, groupInfo :: GroupInfo, member :: GroupMember} -- there is the same command response
  | CEvtJoinedGroupMemberConnecting {user :: User, groupInfo :: GroupInfo, hostMember :: GroupMember, member :: GroupMember}
  | CEvtMemberRole {user :: User, groupInfo :: GroupInfo, byMember :: GroupMember, member :: GroupMember, fromRole :: GroupMemberRole, toRole :: GroupMemberRole}
  | CEvtMemberBlockedForAll {user :: User, groupInfo :: GroupInfo, byMember :: GroupMember, member :: GroupMember, blocked :: Bool}
  | CEvtConnectedToGroupMember {user :: User, groupInfo :: GroupInfo, member :: GroupMember, memberContact :: Maybe Contact}
  | CEvtDeletedMember {user :: User, groupInfo :: GroupInfo, byMember :: GroupMember, deletedMember :: GroupMember, withMessages :: Bool}
  | CEvtDeletedMemberUser {user :: User, groupInfo :: GroupInfo, member :: GroupMember, withMessages :: Bool}
  | CEvtLeftMember {user :: User, groupInfo :: GroupInfo, member :: GroupMember}
  | CEvtUnknownMemberCreated {user :: User, groupInfo :: GroupInfo, forwardedByMember :: GroupMember, member :: GroupMember}
  | CEvtUnknownMemberBlocked {user :: User, groupInfo :: GroupInfo, blockedByMember :: GroupMember, member :: GroupMember}
  | CEvtUnknownMemberAnnounced {user :: User, groupInfo :: GroupInfo, announcingMember :: GroupMember, unknownMember :: GroupMember, announcedMember :: GroupMember}
  | CEvtGroupDeleted {user :: User, groupInfo :: GroupInfo, member :: GroupMember}
  | CEvtGroupUpdated {user :: User, fromGroup :: GroupInfo, toGroup :: GroupInfo, member_ :: Maybe GroupMember} -- there is the same command response
  | CEvtAcceptingGroupJoinRequestMember {user :: User, groupInfo :: GroupInfo, member :: GroupMember}
  | CEvtNoMemberContactCreating {user :: User, groupInfo :: GroupInfo, member :: GroupMember} -- only used in CLI
  | CEvtNewMemberContactReceivedInv {user :: User, contact :: Contact, groupInfo :: GroupInfo, member :: GroupMember}
  | CEvtContactAndMemberAssociated {user :: User, contact :: Contact, groupInfo :: GroupInfo, member :: GroupMember, updatedContact :: Contact}
  | CEvtCallInvitation {callInvitation :: RcvCallInvitation}
  | CEvtCallOffer {user :: User, contact :: Contact, callType :: CallType, offer :: WebRTCSession, sharedKey :: Maybe C.Key, askConfirmation :: Bool}
  | CEvtCallAnswer {user :: User, contact :: Contact, answer :: WebRTCSession}
  | CEvtCallExtraInfo {user :: User, contact :: Contact, extraInfo :: WebRTCExtraInfo}
  | CEvtCallEnded {user :: User, contact :: Contact}
  | CEvtNtfMessage {user :: User, connEntity :: ConnectionEntity, ntfMessage :: NtfMsgAckInfo}
  | CEvtRemoteHostSessionCode {remoteHost_ :: Maybe RemoteHostInfo, sessionCode :: Text}
  | CEvtNewRemoteHost {remoteHost :: RemoteHostInfo}
  | CEvtRemoteHostConnected {remoteHost :: RemoteHostInfo}
  | CEvtRemoteHostStopped {remoteHostId_ :: Maybe RemoteHostId, rhsState :: RemoteHostSessionState, rhStopReason :: RemoteHostStopReason}
  | CEvtRemoteCtrlFound {remoteCtrl :: RemoteCtrlInfo, ctrlAppInfo_ :: Maybe CtrlAppInfo, appVersion :: AppVersion, compatible :: Bool}
  | CEvtRemoteCtrlSessionCode {remoteCtrl_ :: Maybe RemoteCtrlInfo, sessionCode :: Text}  
  | CEvtRemoteCtrlStopped {rcsState :: RemoteCtrlSessionState, rcStopReason :: RemoteCtrlStopReason}
  | CEvtContactPQEnabled {user :: User, contact :: Contact, pqEnabled :: PQEncryption}
  | CEvtContactDisabled {user :: User, contact :: Contact}
  | CEvtConnectionDisabled {connectionEntity :: ConnectionEntity}
  | CEvtConnectionInactive {connectionEntity :: ConnectionEntity, inactive :: Bool}
  | CEvtAgentRcvQueuesDeleted {deletedRcvQueues :: NonEmpty DeletedRcvQueue}
  | CEvtAgentConnsDeleted {agentConnIds :: NonEmpty AgentConnId}
  | CEvtAgentUserDeleted {agentUserId :: Int64}
  | CEvtMessageError {user :: User, severity :: Text, errorMessage :: Text}
  | CEvtChatErrors {chatErrors :: [ChatError]}
  | CEvtTimedAction {action :: String, durationMilliseconds :: Int64}
  | CEvtTerminalEvent TerminalEvent
  deriving (Show)

data TerminalEvent
  = TEGroupLinkRejected {user :: User, groupInfo :: GroupInfo, groupRejectionReason :: GroupRejectionReason}
  | TERejectingGroupJoinRequestMember {user :: User, groupInfo :: GroupInfo, member :: GroupMember, groupRejectionReason :: GroupRejectionReason}
  | TENewMemberContact {user :: User, contact :: Contact, groupInfo :: GroupInfo, member :: GroupMember}
  | TEContactVerificationReset {user :: User, contact :: Contact}
  | TEGroupMemberVerificationReset {user :: User, groupInfo :: GroupInfo, member :: GroupMember}
  | TEGroupEmpty {user :: User, shortGroupInfo :: ShortGroupInfo}
  | TEGroupSubscribed {user :: User, shortGroupInfo :: ShortGroupInfo}
  | TEGroupInvitation {user :: User, shortGroupInfo :: ShortGroupInfo}
  | TEMemberSubError {user :: User, shortGroupInfo :: ShortGroupInfo, memberToSubscribe :: ShortGroupMember, chatError :: ChatError}
  | TEMemberSubSummary {user :: User, memberSubscriptions :: [MemberSubStatus]}
  | TEPendingSubSummary {user :: User, pendingSubscriptions :: [PendingSubStatus]}
  | TESndFileSubError {user :: User, sndFileTransfer :: SndFileTransfer, chatError :: ChatError}
  | TERcvFileSubError {user :: User, rcvFileTransfer :: RcvFileTransfer, chatError :: ChatError}
  deriving (Show)

data DeletedRcvQueue = DeletedRcvQueue
  { agentConnId :: AgentConnId,
    server :: SMPServer,
    agentQueueId :: AgentQueueId,
    agentError_ :: Maybe AgentErrorType
  }
  deriving (Show)

allowRemoteEvent :: ChatEvent -> Bool
allowRemoteEvent = \case
  CEvtChatSuspended -> False
  CEvtRemoteHostSessionCode {} -> False
  CEvtNewRemoteHost _ -> False
  CEvtRemoteHostConnected _ -> False
  CEvtRemoteHostStopped {} -> False
  CEvtRemoteCtrlFound {} -> False
  CEvtRemoteCtrlSessionCode {} -> False
  CEvtRemoteCtrlStopped {} -> False
  _ -> True

logEventToFile :: ChatEvent -> Bool
logEventToFile = \case
  CEvtContactsDisconnected {} -> True
  CEvtContactsSubscribed {} -> True
  CEvtContactSubError {} -> True
  CEvtHostConnected {} -> True
  CEvtHostDisconnected {} -> True
  CEvtConnectionDisabled {} -> True
  CEvtAgentRcvQueuesDeleted {} -> True
  CEvtAgentConnsDeleted {} -> True
  CEvtAgentUserDeleted {} -> True
  -- CRChatCmdError {} -> True -- TODO this should be separately logged to file as command error
  CEvtMessageError {} -> True
  CEvtTerminalEvent te -> case te of
    TEMemberSubError {} -> True
    TESndFileSubError {} -> True
    TERcvFileSubError {} -> True
    _ -> False
  _ -> False

-- (Maybe GroupMemberId) can later be changed to GroupSndScope = GSSAll | GSSAdmins | GSSMember GroupMemberId
data SendRef
  = SRDirect ContactId
  | SRGroup GroupId (Maybe GroupMemberId)
  deriving (Eq, Show)

sendToChatRef :: SendRef -> ChatRef
sendToChatRef = \case
  SRDirect cId -> ChatRef CTDirect cId
  SRGroup gId _ -> ChatRef CTGroup gId

data ChatPagination
  = CPLast Int
  | CPAfter ChatItemId Int
  | CPBefore ChatItemId Int
  | CPAround ChatItemId Int
  | CPInitial Int
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
  | CPError {chatError :: ChatError}
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
  CPError _ -> True

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
    smpWebPortServers :: SMPWebPortServers,
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
      smpWebPortServers = SWPPreset,
      tcpTimeout_ = Nothing,
      logTLSErrors = False
    }

data ContactSubStatus = ContactSubStatus
  { contact :: Contact,
    contactError :: Maybe ChatError
  }
  deriving (Show)

data MemberSubStatus = MemberSubStatus
  { member :: ShortGroupMember,
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
    msgContent :: MsgContent,
    mentions :: Map MemberName GroupMemberId
  }
  deriving (Show)

data UpdatedMessage = UpdatedMessage
  { msgContent :: MsgContent,
    mentions :: Map MemberName GroupMemberId
  }
  deriving (Show)

data ChatTagData = ChatTagData
  { emoji :: Maybe Text,
    text :: Text
  }
  deriving (Show)

instance FromJSON ChatTagData where
  parseJSON (J.Object v) = ChatTagData <$> v .:? "emoji" <*> v .: "text"
  parseJSON invalid = JT.prependFailure "bad ChatTagData, " (JT.typeMismatch "Object" invalid)

data NtfConn = NtfConn
  { user :: User,
    agentConnId :: AgentConnId,
    agentDbQueueId :: Int64,
    connEntity :: ConnectionEntity,
    -- Decrypted ntf meta of the expected message (the one notification was sent for).
    -- Nothing means it failed to decrypt or to decode, we can still show event for entity
    expectedMsg_ :: Maybe NtfMsgInfo
  }
  deriving (Show)

-- msgTs is broker message timestamp, it is used in ConnMsgReq / APIGetConnNtfMessages
-- to set it as last connection message in case queue is empty
data NtfMsgInfo = NtfMsgInfo {msgId :: Text, msgTs :: UTCTime}
  deriving (Show)

data RcvNtfMsgInfo
  = RNMInfo {ntfMsgInfo :: Maybe NtfMsgInfo}
  | RNMError {ntfMsgError :: AgentErrorType}
  deriving (Show)

receivedMsgInfo :: Either AgentErrorType (Maybe SMPMsgMeta) -> RcvNtfMsgInfo
receivedMsgInfo = \case
  Right msgMeta_ -> RNMInfo $ (\SMPMsgMeta {msgId, msgTs} -> ntfMsgInfo_ msgId msgTs) <$> msgMeta_
  Left e -> RNMError e

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

#if !defined(dbPostgres)
data SlowSQLQuery = SlowSQLQuery
  { query :: Text,
    queryStats :: SlowQueryStats
  }
  deriving (Show)
#endif

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
  | CEInvalidConnReq
  | CEUnsupportedConnReq
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
        remoteOutputQ :: TBQueue (Either ChatError ChatEvent)
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

throwCmdError :: String -> CM a
throwCmdError = throwError . ChatError . CECommandError
{-# INLINE throwCmdError #-}

chatCmdError :: String -> Either ChatError ChatResponse
chatCmdError = Left . ChatError . CECommandError
{-# INLINE chatCmdError #-}

throwChatError :: ChatErrorType -> CM a
throwChatError = throwError . ChatError
{-# INLINE throwChatError #-}

toViewTE :: TerminalEvent -> CM ()
toViewTE = toView . CEvtTerminalEvent
{-# INLINE toViewTE #-}

-- | Emit local events.
toView :: ChatEvent -> CM ()
toView = lift . toView'
{-# INLINE toView #-}

toView' :: ChatEvent -> CM' ()
toView' = toView_ . Right
{-# INLINE toView' #-}

eToView :: ChatError -> CM ()
eToView = lift . eToView'
{-# INLINE eToView #-}

eToView' :: ChatError -> CM' ()
eToView' = toView_ . Left
{-# INLINE eToView' #-}

toView_ :: Either ChatError ChatEvent -> CM' ()
toView_ ev = do
  cc@ChatController {outputQ = localQ, remoteCtrlSession = session, config = ChatConfig {chatHooks}} <- ask
  event <- case eventHook chatHooks of
    Just hook -> liftIO $ hook cc ev
    Nothing -> pure ev
  atomically $
    readTVar session >>= \case
      Just (_, RCSessionConnected {remoteOutputQ})
        | either (const True) allowRemoteEvent event -> writeTBQueue remoteOutputQ event
      -- TODO potentially, it should hold some events while connecting
      _ -> writeTBQueue localQ (Nothing, event)

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

-- TODO [postgres] postgres specific error handling
handleDBErrors :: [E.Handler (Either ChatError a)]
handleDBErrors =
#if !defined(dbPostgres)
  ( E.Handler $ \(e :: SQLError) ->
      let se = SQL.sqlError e
          busy = se == SQL.ErrorBusy || se == SQL.ErrorLocked
       in pure . Left . ChatErrorStore $ if busy then SEDBBusyError $ show se else SEDBException $ show e
  ) :
#endif
  [ E.Handler $ \(E.SomeException e) -> pure . Left . ChatErrorStore . SEDBException $ show e
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

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "FC") ''ForwardConfirmation)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "CE") ''ChatErrorType)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "RHE") ''RemoteHostError)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "RCE") ''RemoteCtrlError)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "SQLite") ''SQLiteError)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "DB") ''DatabaseError)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "Chat") ''ChatError)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "CP") ''ConnectionPlan)

$(JQ.deriveJSON defaultJSON ''AppFilePathsConfig)

$(JQ.deriveJSON defaultJSON ''ContactSubStatus)

$(JQ.deriveJSON defaultJSON ''MemberSubStatus)

$(JQ.deriveJSON defaultJSON ''UserContactSubStatus)

$(JQ.deriveJSON defaultJSON ''PendingSubStatus)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "AE") ''ArchiveError)

$(JQ.deriveJSON defaultJSON ''UserProfileUpdateSummary)

$(JQ.deriveJSON defaultJSON ''NtfMsgInfo)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "RNM") ''RcvNtfMsgInfo)

$(JQ.deriveJSON defaultJSON ''NtfConn)

$(JQ.deriveJSON defaultJSON ''NtfMsgAckInfo)

$(JQ.deriveJSON defaultJSON ''SwitchProgress)

$(JQ.deriveJSON defaultJSON ''RatchetSyncProgress)

$(JQ.deriveJSON defaultJSON ''DeletedRcvQueue)

$(JQ.deriveJSON defaultJSON ''ServerAddress)

$(JQ.deriveJSON defaultJSON ''ParsedServerAddress)

$(JQ.deriveJSON defaultJSON ''ChatItemDeletion)

$(JQ.deriveJSON defaultJSON ''CoreVersionInfo)

#if !defined(dbPostgres)
$(JQ.deriveJSON defaultJSON ''SlowSQLQuery)
#endif

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "RCS") ''RemoteCtrlSessionState)

$(JQ.deriveJSON defaultJSON ''RemoteCtrlInfo)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "RCSR") ''RemoteCtrlStopReason)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "RHSR") ''RemoteHostStopReason)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "TE") ''TerminalEvent)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "CR") ''ChatResponse)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "CEvt") ''ChatEvent)

$(JQ.deriveFromJSON defaultJSON ''ArchiveConfig)

$(JQ.deriveFromJSON defaultJSON ''DBEncryptionConfig)

$(JQ.deriveToJSON defaultJSON ''ComposedMessage)

instance FromJSON ComposedMessage where
  parseJSON (J.Object v) = do
    fileSource <-
      (v .:? "fileSource") >>= \case
        Nothing -> CF.plain <$$> (v .:? "filePath")
        f -> pure f
    quotedItemId <- v .:? "quotedItemId"
    msgContent <- v .: "msgContent"
    mentions <- fromMaybe M.empty <$> v .:? "mentions"
    pure ComposedMessage {fileSource, quotedItemId, msgContent, mentions}
  parseJSON invalid =
    JT.prependFailure "bad ComposedMessage, " (JT.typeMismatch "Object" invalid)

$(JQ.deriveJSON defaultJSON ''UpdatedMessage)

$(JQ.deriveToJSON defaultJSON ''ChatTagData)
