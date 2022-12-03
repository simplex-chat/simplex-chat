{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Simplex.Chat.Controller where

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
import Data.Time.Clock (UTCTime)
import Data.Version (showVersion)
import GHC.Generics (Generic)
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
import Simplex.Messaging.Agent.Env.SQLite (AgentConfig, InitialAgentServers, NetworkConfig)
import Simplex.Messaging.Agent.Lock
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Agent.Store.SQLite (SQLiteStore)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Notifications.Protocol (DeviceToken (..), NtfTknStatus)
import Simplex.Messaging.Parsers (dropPrefix, enumJSON, parseAll, parseString, sumTypeJSON)
import Simplex.Messaging.Protocol (AProtocolType, CorrId, MsgFlags)
import Simplex.Messaging.TMap (TMap)
import Simplex.Messaging.Transport.Client (TransportHost)
import System.IO (Handle)
import UnliftIO.STM

versionNumber :: String
versionNumber = showVersion SC.version

versionStr :: String
versionStr = "SimpleX Chat v" <> versionNumber

updateStr :: String
updateStr = "To update run: curl -o- https://raw.githubusercontent.com/simplex-chat/simplex-chat/master/install.sh | bash"

data ChatConfig = ChatConfig
  { agentConfig :: AgentConfig,
    yesToMigrations :: Bool,
    defaultServers :: InitialAgentServers,
    tbqSize :: Natural,
    fileChunkSize :: Integer,
    inlineFiles :: InlineFilesConfig,
    subscriptionConcurrency :: Int,
    subscriptionEvents :: Bool,
    hostEvents :: Bool,
    testView :: Bool
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
    expireCIsAsync :: TVar (Maybe (Async ())),
    expireCIs :: TVar Bool
  }

data HelpSection = HSMain | HSFiles | HSGroups | HSMyAddress | HSMarkdown | HSMessages | HSSettings
  deriving (Show, Generic)

instance ToJSON HelpSection where
  toJSON = J.genericToJSON . enumJSON $ dropPrefix "HS"
  toEncoding = J.genericToEncoding . enumJSON $ dropPrefix "HS"

data ChatCommand
  = ShowActiveUser
  | CreateActiveUser Profile
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
  | APIGetChats {pendingConnections :: Bool}
  | APIGetChat ChatRef ChatPagination (Maybe String)
  | APIGetChatItems Int
  | APISendMessage ChatRef ComposedMessage
  | APIUpdateChatItem ChatRef ChatItemId MsgContent
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
  | APIUpdateProfile Profile
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
  | GetUserSMPServers
  | SetUserSMPServers SMPServersConfig
  | TestSMPServer SMPServerWithAuth
  | APISetChatItemTTL (Maybe Int64)
  | APIGetChatItemTTL
  | APISetNetworkConfig NetworkConfig
  | APIGetNetworkConfig
  | APISetChatSettings ChatRef ChatSettings
  | APIContactInfo ContactId
  | APIGroupMemberInfo GroupId GroupMemberId
  | APISwitchContact ContactId
  | APISwitchGroupMember GroupId GroupMemberId
  | ShowMessages ChatName Bool
  | ContactInfo ContactName
  | GroupMemberInfo GroupName ContactName
  | SwitchContact ContactName
  | SwitchGroupMember GroupName ContactName
  | ChatHelp HelpSection
  | Welcome
  | AddContact
  | Connect (Maybe AConnectionRequestUri)
  | ConnectSimplex
  | DeleteContact ContactName
  | ClearContact ContactName
  | ListContacts
  | CreateMyAddress
  | DeleteMyAddress
  | ShowMyAddress
  | AddressAutoAccept (Maybe AutoAccept)
  | AcceptContact ContactName
  | RejectContact ContactName
  | SendMessage ChatName ByteString
  | SendMessageQuote {contactName :: ContactName, msgDir :: AMsgDirection, quotedMsg :: ByteString, message :: ByteString}
  | SendMessageBroadcast ByteString
  | DeleteMessage ChatName ByteString
  | EditMessage {chatName :: ChatName, editedMsg :: ByteString, message :: ByteString}
  | NewGroup GroupProfile
  | AddMember GroupName ContactName GroupMemberRole
  | JoinGroup GroupName
  | MemberRole GroupName ContactName GroupMemberRole
  | RemoveMember GroupName ContactName
  | LeaveGroup GroupName
  | DeleteGroup GroupName
  | ClearGroup GroupName
  | ListMembers GroupName
  | ListGroups
  | UpdateGroupProfile GroupName GroupProfile
  | CreateGroupLink GroupName
  | DeleteGroupLink GroupName
  | ShowGroupLink GroupName
  | SendGroupMessageQuote {groupName :: GroupName, contactName_ :: Maybe ContactName, quotedMsg :: ByteString, message :: ByteString}
  | LastMessages (Maybe ChatName) Int (Maybe String)
  | SendFile ChatName FilePath
  | SendImage ChatName FilePath
  | ForwardFile ChatName FileTransferId
  | ForwardImage ChatName FileTransferId
  | ReceiveFile {fileId :: FileTransferId, fileInline :: Maybe Bool, filePath :: Maybe FilePath}
  | CancelFile FileTransferId
  | FileStatus FileTransferId
  | ShowProfile
  | UpdateProfile ContactName Text
  | UpdateProfileImage (Maybe ImageData)
  | SetUserFeature ChatFeature FeatureAllowed
  | SetContactFeature ChatFeature ContactName (Maybe FeatureAllowed)
  | SetGroupFeature GroupFeature GroupName GroupFeatureEnabled
  | QuitChat
  | ShowVersion
  | DebugLocks
  deriving (Show)

data ChatResponse
  = CRActiveUser {user :: User}
  | CRChatStarted
  | CRChatRunning
  | CRChatStopped
  | CRChatSuspended
  | CRApiChats {chats :: [AChat]}
  | CRApiChat {chat :: AChat}
  | CRLastMessages {chatItems :: [AChatItem]}
  | CRApiParsedMarkdown {formattedText :: Maybe MarkdownList}
  | CRUserSMPServers {smpServers :: NonEmpty ServerCfg, presetSMPServers :: NonEmpty SMPServerWithAuth}
  | CRSmpTestResult {smpTestFailure :: Maybe SMPTestFailure}
  | CRChatItemTTL {chatItemTTL :: Maybe Int64}
  | CRNetworkConfig {networkConfig :: NetworkConfig}
  | CRContactInfo {contact :: Contact, connectionStats :: ConnectionStats, customUserProfile :: Maybe Profile}
  | CRGroupMemberInfo {groupInfo :: GroupInfo, member :: GroupMember, connectionStats_ :: Maybe ConnectionStats}
  | CRContactSwitch {contact :: Contact, switchProgress :: SwitchProgress}
  | CRGroupMemberSwitch {groupInfo :: GroupInfo, member :: GroupMember, switchProgress :: SwitchProgress}
  | CRNewChatItem {chatItem :: AChatItem}
  | CRChatItemStatusUpdated {chatItem :: AChatItem}
  | CRChatItemUpdated {chatItem :: AChatItem}
  | CRChatItemDeleted {deletedChatItem :: AChatItem, toChatItem :: Maybe AChatItem, byUser :: Bool}
  | CRChatItemDeletedNotFound {contact :: Contact, sharedMsgId :: SharedMsgId}
  | CRBroadcastSent MsgContent Int ZonedTime
  | CRMsgIntegrityError {msgError :: MsgErrorType}
  | CRCmdAccepted {corr :: CorrId}
  | CRCmdOk
  | CRChatHelp {helpSection :: HelpSection}
  | CRWelcome {user :: User}
  | CRGroupCreated {groupInfo :: GroupInfo}
  | CRGroupMembers {group :: Group}
  | CRContactsList {contacts :: [Contact]}
  | CRUserContactLink {contactLink :: UserContactLink}
  | CRUserContactLinkUpdated {contactLink :: UserContactLink}
  | CRContactRequestRejected {contactRequest :: UserContactRequest}
  | CRUserAcceptedGroupSent {groupInfo :: GroupInfo, hostContact :: Maybe Contact}
  | CRUserDeletedMember {groupInfo :: GroupInfo, member :: GroupMember}
  | CRGroupsList {groups :: [GroupInfo]}
  | CRSentGroupInvitation {groupInfo :: GroupInfo, contact :: Contact, member :: GroupMember}
  | CRFileTransferStatus (FileTransfer, [Integer]) -- TODO refactor this type to FileTransferStatus
  | CRUserProfile {profile :: Profile}
  | CRUserProfileNoChange
  | CRVersionInfo {version :: String}
  | CRInvitation {connReqInvitation :: ConnReqInvitation}
  | CRSentConfirmation
  | CRSentInvitation {customUserProfile :: Maybe Profile}
  | CRContactUpdated {fromContact :: Contact, toContact :: Contact}
  | CRContactsMerged {intoContact :: Contact, mergedContact :: Contact}
  | CRContactDeleted {contact :: Contact}
  | CRChatCleared {chatInfo :: AChatInfo}
  | CRUserContactLinkCreated {connReqContact :: ConnReqContact}
  | CRUserContactLinkDeleted
  | CRReceivedContactRequest {contactRequest :: UserContactRequest}
  | CRAcceptingContactRequest {contact :: Contact}
  | CRContactAlreadyExists {contact :: Contact}
  | CRContactRequestAlreadyAccepted {contact :: Contact}
  | CRLeftMemberUser {groupInfo :: GroupInfo}
  | CRGroupDeletedUser {groupInfo :: GroupInfo}
  | CRRcvFileAccepted {chatItem :: AChatItem}
  | CRRcvFileAcceptedSndCancelled {rcvFileTransfer :: RcvFileTransfer}
  | CRRcvFileStart {chatItem :: AChatItem}
  | CRRcvFileComplete {chatItem :: AChatItem}
  | CRRcvFileCancelled {rcvFileTransfer :: RcvFileTransfer}
  | CRRcvFileSndCancelled {rcvFileTransfer :: RcvFileTransfer}
  | CRSndFileStart {chatItem :: AChatItem, sndFileTransfer :: SndFileTransfer}
  | CRSndFileComplete {chatItem :: AChatItem, sndFileTransfer :: SndFileTransfer}
  | CRSndFileCancelled {chatItem :: AChatItem, sndFileTransfer :: SndFileTransfer}
  | CRSndFileRcvCancelled {chatItem :: AChatItem, sndFileTransfer :: SndFileTransfer}
  | CRSndGroupFileCancelled {chatItem :: AChatItem, fileTransferMeta :: FileTransferMeta, sndFileTransfers :: [SndFileTransfer]}
  | CRUserProfileUpdated {fromProfile :: Profile, toProfile :: Profile}
  | CRContactAliasUpdated {toContact :: Contact}
  | CRConnectionAliasUpdated {toConnection :: PendingContactConnection}
  | CRContactPrefsUpdated {fromContact :: Contact, toContact :: Contact}
  | CRContactConnecting {contact :: Contact}
  | CRContactConnected {contact :: Contact, userCustomProfile :: Maybe Profile}
  | CRContactAnotherClient {contact :: Contact}
  | CRSubscriptionEnd {connectionEntity :: ConnectionEntity}
  | CRContactsDisconnected {server :: SMPServer, contactRefs :: [ContactRef]}
  | CRContactsSubscribed {server :: SMPServer, contactRefs :: [ContactRef]}
  | CRContactSubError {contact :: Contact, chatError :: ChatError}
  | CRContactSubSummary {contactSubscriptions :: [ContactSubStatus]}
  | CRUserContactSubSummary {userContactSubscriptions :: [UserContactSubStatus]}
  | CRHostConnected {protocol :: AProtocolType, transportHost :: TransportHost}
  | CRHostDisconnected {protocol :: AProtocolType, transportHost :: TransportHost}
  | CRGroupInvitation {groupInfo :: GroupInfo}
  | CRReceivedGroupInvitation {groupInfo :: GroupInfo, contact :: Contact, memberRole :: GroupMemberRole}
  | CRUserJoinedGroup {groupInfo :: GroupInfo, hostMember :: GroupMember}
  | CRJoinedGroupMember {groupInfo :: GroupInfo, member :: GroupMember}
  | CRJoinedGroupMemberConnecting {groupInfo :: GroupInfo, hostMember :: GroupMember, member :: GroupMember}
  | CRMemberRole {groupInfo :: GroupInfo, byMember :: GroupMember, member :: GroupMember, fromRole :: GroupMemberRole, toRole :: GroupMemberRole}
  | CRMemberRoleUser {groupInfo :: GroupInfo, member :: GroupMember, fromRole :: GroupMemberRole, toRole :: GroupMemberRole}
  | CRConnectedToGroupMember {groupInfo :: GroupInfo, member :: GroupMember}
  | CRDeletedMember {groupInfo :: GroupInfo, byMember :: GroupMember, deletedMember :: GroupMember}
  | CRDeletedMemberUser {groupInfo :: GroupInfo, member :: GroupMember}
  | CRLeftMember {groupInfo :: GroupInfo, member :: GroupMember}
  | CRGroupEmpty {groupInfo :: GroupInfo}
  | CRGroupRemoved {groupInfo :: GroupInfo}
  | CRGroupDeleted {groupInfo :: GroupInfo, member :: GroupMember}
  | CRGroupUpdated {fromGroup :: GroupInfo, toGroup :: GroupInfo, member_ :: Maybe GroupMember}
  | CRGroupLinkCreated {groupInfo :: GroupInfo, connReqContact :: ConnReqContact}
  | CRGroupLink {groupInfo :: GroupInfo, connReqContact :: ConnReqContact}
  | CRGroupLinkDeleted {groupInfo :: GroupInfo}
  | CRAcceptingGroupJoinRequest {groupInfo :: GroupInfo, contact :: Contact}
  | CRMemberSubError {groupInfo :: GroupInfo, member :: GroupMember, chatError :: ChatError}
  | CRMemberSubSummary {memberSubscriptions :: [MemberSubStatus]}
  | CRGroupSubscribed {groupInfo :: GroupInfo}
  | CRPendingSubSummary {pendingSubscriptions :: [PendingSubStatus]}
  | CRSndFileSubError {sndFileTransfer :: SndFileTransfer, chatError :: ChatError}
  | CRRcvFileSubError {rcvFileTransfer :: RcvFileTransfer, chatError :: ChatError}
  | CRCallInvitation {callInvitation :: RcvCallInvitation}
  | CRCallOffer {contact :: Contact, callType :: CallType, offer :: WebRTCSession, sharedKey :: Maybe C.Key, askConfirmation :: Bool}
  | CRCallAnswer {contact :: Contact, answer :: WebRTCSession}
  | CRCallExtraInfo {contact :: Contact, extraInfo :: WebRTCExtraInfo}
  | CRCallEnded {contact :: Contact}
  | CRCallInvitations {callInvitations :: [RcvCallInvitation]}
  | CRUserContactLinkSubscribed
  | CRUserContactLinkSubError {chatError :: ChatError}
  | CRNtfTokenStatus {status :: NtfTknStatus}
  | CRNtfToken {token :: DeviceToken, status :: NtfTknStatus, ntfMode :: NotificationsMode}
  | CRNtfMessages {connEntity :: Maybe ConnectionEntity, msgTs :: Maybe UTCTime, ntfMessages :: [NtfMsgInfo]}
  | CRNewContactConnection {connection :: PendingContactConnection}
  | CRContactConnectionDeleted {connection :: PendingContactConnection}
  | CRSQLResult {rows :: [Text]}
  | CRDebugLocks {chatLockName :: Maybe String, agentLocks :: AgentLocks}
  | CRMessageError {severity :: Text, errorMessage :: Text}
  | CRChatCmdError {chatError :: ChatError}
  | CRChatError {chatError :: ChatError}
  deriving (Show, Generic)

instance ToJSON ChatResponse where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "CR"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "CR"

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

data ChatError
  = ChatError {errorType :: ChatErrorType}
  | ChatErrorAgent {agentError :: AgentErrorType}
  | ChatErrorStore {storeError :: StoreError}
  | ChatErrorDatabase {databaseError :: DatabaseError}
  deriving (Show, Exception, Generic)

instance ToJSON ChatError where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "Chat"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "Chat"

data ChatErrorType
  = CENoActiveUser
  | CEActiveUserExists
  | CEChatNotStarted
  | CEChatNotStopped
  | CEChatStoreChanged
  | CEInvalidConnReq
  | CEInvalidChatMessage {message :: String}
  | CEContactNotReady {contact :: Contact}
  | CEGroupUserRole
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

chatCmdError :: String -> ChatResponse
chatCmdError = CRChatCmdError . ChatError . CECommandError

setActive :: (MonadUnliftIO m, MonadReader ChatController m) => ActiveTo -> m ()
setActive to = asks activeTo >>= atomically . (`writeTVar` to)

unsetActive :: (MonadUnliftIO m, MonadReader ChatController m) => ActiveTo -> m ()
unsetActive a = asks activeTo >>= atomically . (`modifyTVar` unset)
  where
    unset a' = if a == a' then ActiveNone else a'
