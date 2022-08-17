{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Controller where

import Control.Concurrent.Async (Async)
import Control.Exception
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Crypto.Random (ChaChaDRG)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import Data.ByteString.Char8 (ByteString)
import Data.Int (Int64)
import Data.Map.Strict (Map)
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
import Simplex.Chat.Store (StoreError)
import Simplex.Chat.Types
import Simplex.Messaging.Agent (AgentClient)
import Simplex.Messaging.Agent.Env.SQLite (AgentConfig, InitialAgentServers, NetworkConfig)
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Agent.Store.SQLite (SQLiteStore)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Notifications.Protocol (DeviceToken (..), NtfTknStatus)
import Simplex.Messaging.Parsers (dropPrefix, enumJSON, sumTypeJSON)
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
    subscriptionConcurrency :: Int,
    subscriptionEvents :: Bool,
    hostEvents :: Bool,
    testView :: Bool
  }

data ActiveTo = ActiveNone | ActiveC ContactName | ActiveG GroupName
  deriving (Eq)

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
    chatLock :: TMVar (),
    sndFiles :: TVar (Map Int64 Handle),
    rcvFiles :: TVar (Map Int64 Handle),
    currentCalls :: TMap ContactId Call,
    config :: ChatConfig,
    filesFolder :: TVar (Maybe FilePath), -- path to files folder for mobile apps,
    incognitoMode :: TVar Bool
  }

data HelpSection = HSMain | HSFiles | HSGroups | HSMyAddress | HSMarkdown | HSMessages | HSSettings
  deriving (Show, Generic)

instance ToJSON HelpSection where
  toJSON = J.genericToJSON . enumJSON $ dropPrefix "HS"
  toEncoding = J.genericToEncoding . enumJSON $ dropPrefix "HS"

data ChatCommand
  = ShowActiveUser
  | CreateActiveUser Profile
  | StartChat {subscribeConnections :: Bool}
  | APIStopChat
  | APIActivateChat
  | APISuspendChat {suspendTimeout :: Int}
  | ResubscribeAllConnections
  | SetFilesFolder FilePath
  | SetIncognito Bool
  | APIExportArchive ArchiveConfig
  | APIImportArchive ArchiveConfig
  | APIDeleteStorage
  | APIGetChats {pendingConnections :: Bool}
  | APIGetChat ChatRef ChatPagination (Maybe String)
  | APIGetChatItems Int
  | APISendMessage ChatRef ComposedMessage
  | APIUpdateChatItem ChatRef ChatItemId MsgContent
  | APIDeleteChatItem ChatRef ChatItemId CIDeleteMode
  | APIChatRead ChatRef (Maybe (ChatItemId, ChatItemId))
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
  | GetUserSMPServers
  | SetUserSMPServers [SMPServer]
  | APISetNetworkConfig NetworkConfig
  | APIGetNetworkConfig
  | APIContactInfo ContactId
  | APIGroupMemberInfo GroupId GroupMemberId
  | ContactInfo ContactName
  | GroupMemberInfo GroupName ContactName
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
  | AddressAutoAccept Bool (Maybe MsgContent)
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
  | SendGroupMessageQuote {groupName :: GroupName, contactName_ :: Maybe ContactName, quotedMsg :: ByteString, message :: ByteString}
  | LastMessages (Maybe ChatName) Int
  | SendFile ChatName FilePath
  | SendImage ChatName FilePath
  | ForwardFile ChatName FileTransferId
  | ForwardImage ChatName FileTransferId
  | ReceiveFile FileTransferId (Maybe FilePath)
  | CancelFile FileTransferId
  | FileStatus FileTransferId
  | ShowProfile
  | UpdateProfile ContactName Text
  | UpdateProfileImage (Maybe ImageData)
  | QuitChat
  | ShowVersion
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
  | CRUserSMPServers {smpServers :: [SMPServer]}
  | CRNetworkConfig {networkConfig :: NetworkConfig}
  | CRContactInfo {contact :: Contact, connectionStats :: ConnectionStats, customUserProfile :: Maybe Profile}
  | CRGroupMemberInfo {groupInfo :: GroupInfo, member :: GroupMember, connectionStats_ :: Maybe ConnectionStats, mainProfile :: Maybe Profile}
  | CRNewChatItem {chatItem :: AChatItem}
  | CRChatItemStatusUpdated {chatItem :: AChatItem}
  | CRChatItemUpdated {chatItem :: AChatItem}
  | CRChatItemDeleted {deletedChatItem :: AChatItem, toChatItem :: AChatItem}
  | CRChatItemDeletedNotFound {contact :: Contact, sharedMsgId :: SharedMsgId}
  | CRBroadcastSent MsgContent Int ZonedTime
  | CRMsgIntegrityError {msgError :: MsgErrorType}
  | CRCmdAccepted {corr :: CorrId}
  | CRCmdOk
  | CRChatHelp {helpSection :: HelpSection}
  | CRWelcome {user :: User}
  | CRGroupCreated {groupInfo :: GroupInfo, customUserProfile :: Maybe Profile}
  | CRGroupMembers {group :: Group}
  | CRContactsList {contacts :: [Contact]}
  | CRUserContactLink {connReqContact :: ConnReqContact, autoAccept :: Bool, autoReply :: Maybe MsgContent}
  | CRUserContactLinkUpdated {connReqContact :: ConnReqContact, autoAccept :: Bool, autoReply :: Maybe MsgContent}
  | CRContactRequestRejected {contactRequest :: UserContactRequest}
  | CRUserAcceptedGroupSent {groupInfo :: GroupInfo}
  | CRUserDeletedMember {groupInfo :: GroupInfo, member :: GroupMember}
  | CRGroupsList {groups :: [GroupInfo]}
  | CRSentGroupInvitation {groupInfo :: GroupInfo, contact :: Contact, member :: GroupMember, sentCustomProfile :: Bool}
  | CRFileTransferStatus (FileTransfer, [Integer]) -- TODO refactor this type to FileTransferStatus
  | CRUserProfile {profile :: Profile}
  | CRUserProfileNoChange
  | CRVersionInfo {version :: String}
  | CRInvitation {connReqInvitation :: ConnReqInvitation}
  | CRSentConfirmation
  | CRSentInvitation
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
  | CRContactConnecting {contact :: Contact}
  | CRContactConnected {contact :: Contact, userCustomProfile :: Maybe Profile}
  | CRContactAnotherClient {contact :: Contact}
  | CRContactsDisconnected {server :: SMPServer, contactRefs :: [ContactRef]}
  | CRContactsSubscribed {server :: SMPServer, contactRefs :: [ContactRef]}
  | CRContactSubError {contact :: Contact, chatError :: ChatError}
  | CRContactSubSummary {contactSubscriptions :: [ContactSubStatus]}
  | CRHostConnected {protocol :: AProtocolType, transportHost :: TransportHost}
  | CRHostDisconnected {protocol :: AProtocolType, transportHost :: TransportHost}
  | CRGroupInvitation {groupInfo :: GroupInfo}
  | CRReceivedGroupInvitation {groupInfo :: GroupInfo, contact :: Contact, memberRole :: GroupMemberRole, receivedCustomProfile :: Maybe Profile}
  | CRUserJoinedGroup {groupInfo :: GroupInfo, hostMember :: GroupMember, usedCustomProfile :: Bool}
  | CRJoinedGroupMember {groupInfo :: GroupInfo, member :: GroupMember, mainProfile :: Maybe Profile}
  | CRJoinedGroupMemberConnecting {groupInfo :: GroupInfo, hostMember :: GroupMember, member :: GroupMember}
  | CRConnectedToGroupMember {groupInfo :: GroupInfo, member :: GroupMember}
  | CRDeletedMember {groupInfo :: GroupInfo, byMember :: GroupMember, deletedMember :: GroupMember}
  | CRDeletedMemberUser {groupInfo :: GroupInfo, member :: GroupMember}
  | CRLeftMember {groupInfo :: GroupInfo, member :: GroupMember}
  | CRGroupEmpty {groupInfo :: GroupInfo}
  | CRGroupRemoved {groupInfo :: GroupInfo}
  | CRGroupDeleted {groupInfo :: GroupInfo, member :: GroupMember}
  | CRGroupUpdated {fromGroup :: GroupInfo, toGroup :: GroupInfo, member_ :: Maybe GroupMember}
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
  | CRMessageError {severity :: Text, errorMessage :: Text}
  | CRChatCmdError {chatError :: ChatError}
  | CRChatError {chatError :: ChatError}
  deriving (Show, Generic)

instance ToJSON ChatResponse where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "CR"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "CR"

data ArchiveConfig = ArchiveConfig {archivePath :: FilePath, disableCompression :: Maybe Bool, parentTempDirectory :: Maybe FilePath}
  deriving (Show, Generic, FromJSON)

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

data ChatError
  = ChatError {errorType :: ChatErrorType}
  | ChatErrorAgent {agentError :: AgentErrorType}
  | ChatErrorStore {storeError :: StoreError}
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
  | CEContactGroups {contact :: Contact, groupNames :: [GroupName]}
  | CEGroupUserRole
  | CEGroupNotIncognitoCantInvite
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
  | CEInvalidQuote
  | CEInvalidChatItemUpdate
  | CEInvalidChatItemDelete
  | CEHasCurrentCall
  | CENoCurrentCall
  | CECallContact {contactId :: Int64}
  | CECallState {currentCallState :: CallStateTag}
  | CEAgentVersion
  | CEAgentNoSubResult {agentConnId :: AgentConnId}
  | CECommandError {message :: String}
  deriving (Show, Exception, Generic)

instance ToJSON ChatErrorType where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "CE"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "CE"

type ChatMonad m = (MonadUnliftIO m, MonadReader ChatController m, MonadError ChatError m)

chatCmdError :: String -> ChatResponse
chatCmdError = CRChatCmdError . ChatError . CECommandError

setActive :: (MonadUnliftIO m, MonadReader ChatController m) => ActiveTo -> m ()
setActive to = asks activeTo >>= atomically . (`writeTVar` to)

unsetActive :: (MonadUnliftIO m, MonadReader ChatController m) => ActiveTo -> m ()
unsetActive a = asks activeTo >>= atomically . (`modifyTVar` unset)
  where
    unset a' = if a == a' then ActiveNone else a'
