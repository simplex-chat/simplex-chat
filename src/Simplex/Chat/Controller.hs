{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Data.Version (showVersion)
import Data.Word (Word16)
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
import Simplex.Messaging.Agent.Env.SQLite (AgentConfig, InitialAgentServers)
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Agent.Store.SQLite (SQLiteStore)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Notifications.Protocol (DeviceToken (..), NtfTknStatus)
import Simplex.Messaging.Parsers (dropPrefix, enumJSON, sumTypeJSON)
import Simplex.Messaging.Protocol (CorrId)
import Simplex.Messaging.TMap (TMap)
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
    dbPoolSize :: Int,
    yesToMigrations :: Bool,
    defaultServers :: InitialAgentServers,
    tbqSize :: Natural,
    fileChunkSize :: Integer,
    subscriptionConcurrency :: Int,
    subscriptionEvents :: Bool,
    testView :: Bool
  }

data ActiveTo = ActiveNone | ActiveC ContactName | ActiveG GroupName
  deriving (Eq)

data ChatController = ChatController
  { currentUser :: TVar (Maybe User),
    activeTo :: TVar ActiveTo,
    firstTime :: Bool,
    smpAgent :: AgentClient,
    agentAsync :: TVar (Maybe (Async ())),
    chatStore :: SQLiteStore,
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
    filesFolder :: TVar (Maybe FilePath) -- path to files folder for mobile apps
  }

data HelpSection = HSMain | HSFiles | HSGroups | HSMyAddress | HSMarkdown | HSMessages
  deriving (Show, Generic)

instance ToJSON HelpSection where
  toJSON = J.genericToJSON . enumJSON $ dropPrefix "HS"
  toEncoding = J.genericToEncoding . enumJSON $ dropPrefix "HS"

data ChatCommand
  = ShowActiveUser
  | CreateActiveUser Profile
  | StartChat
  | ResubscribeAllConnections
  | SetFilesFolder FilePath
  | APIGetChats {pendingConnections :: Bool}
  | APIGetChat ChatRef ChatPagination
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
  | APIRejectCall ContactId
  | APISendCallOffer ContactId WebRTCCallOffer
  | APISendCallAnswer ContactId WebRTCSession
  | APISendCallExtraInfo ContactId WebRTCExtraInfo
  | APIEndCall ContactId
  | APICallStatus ContactId WebRTCCallStatus
  | APIUpdateProfile Profile
  | APIParseMarkdown Text
  | APIRegisterToken DeviceToken
  | APIVerifyToken DeviceToken ByteString C.CbNonce
  | APIIntervalNofication DeviceToken Word16
  | APIDeleteToken DeviceToken
  | GetUserSMPServers
  | SetUserSMPServers [SMPServer]
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
  | AddressAutoAccept Bool
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
  | RemoveMember GroupName ContactName
  | MemberRole GroupName ContactName GroupMemberRole
  | LeaveGroup GroupName
  | DeleteGroup GroupName
  | ClearGroup GroupName
  | ListMembers GroupName
  | ListGroups
  | SendGroupMessageQuote {groupName :: GroupName, contactName_ :: Maybe ContactName, quotedMsg :: ByteString, message :: ByteString}
  | LastMessages (Maybe ChatName) Int
  | SendFile ChatName FilePath
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
  | CRApiChats {chats :: [AChat]}
  | CRApiChat {chat :: AChat}
  | CRLastMessages {chatItems :: [AChatItem]}
  | CRApiParsedMarkdown {formattedText :: Maybe MarkdownList}
  | CRUserSMPServers {smpServers :: [SMPServer]}
  | CRNewChatItem {chatItem :: AChatItem}
  | CRChatItemStatusUpdated {chatItem :: AChatItem}
  | CRChatItemUpdated {chatItem :: AChatItem}
  | CRChatItemDeleted {deletedChatItem :: AChatItem, toChatItem :: AChatItem}
  | CRChatItemDeletedNotFound {contact :: Contact, sharedMsgId :: SharedMsgId}
  | CRBroadcastSent MsgContent Int ZonedTime
  | CRMsgIntegrityError {msgerror :: MsgErrorType} -- TODO make it chat item to support in mobile
  | CRCmdAccepted {corr :: CorrId}
  | CRCmdOk
  | CRChatHelp {helpSection :: HelpSection}
  | CRWelcome {user :: User}
  | CRGroupCreated {groupInfo :: GroupInfo}
  | CRGroupMembers {group :: Group}
  | CRContactsList {contacts :: [Contact]}
  | CRUserContactLink {connReqContact :: ConnReqContact, autoAccept :: Bool}
  | CRUserContactLinkUpdated {connReqContact :: ConnReqContact, autoAccept :: Bool}
  | CRContactRequestRejected {contactRequest :: UserContactRequest}
  | CRUserAcceptedGroupSent {groupInfo :: GroupInfo}
  | CRUserDeletedMember {groupInfo :: GroupInfo, member :: GroupMember}
  | CRGroupsList {groups :: [GroupInfo]}
  | CRSentGroupInvitation {groupInfo :: GroupInfo, contact :: Contact}
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
  | CRContactConnected {contact :: Contact}
  | CRContactAnotherClient {contact :: Contact}
  | CRContactsDisconnected {server :: SMPServer, contactRefs :: [ContactRef]}
  | CRContactsSubscribed {server :: SMPServer, contactRefs :: [ContactRef]}
  | CRContactSubError {contact :: Contact, chatError :: ChatError}
  | CRContactSubSummary {contactSubscriptions :: [ContactSubStatus]}
  | CRGroupInvitation {groupInfo :: GroupInfo}
  | CRReceivedGroupInvitation {groupInfo :: GroupInfo, contact :: Contact, memberRole :: GroupMemberRole}
  | CRUserJoinedGroup {groupInfo :: GroupInfo}
  | CRJoinedGroupMember {groupInfo :: GroupInfo, member :: GroupMember}
  | CRJoinedGroupMemberConnecting {groupInfo :: GroupInfo, hostMember :: GroupMember, member :: GroupMember}
  | CRConnectedToGroupMember {groupInfo :: GroupInfo, member :: GroupMember}
  | CRDeletedMember {groupInfo :: GroupInfo, byMember :: GroupMember, deletedMember :: GroupMember}
  | CRDeletedMemberUser {groupInfo :: GroupInfo, member :: GroupMember}
  | CRLeftMember {groupInfo :: GroupInfo, member :: GroupMember}
  | CRGroupEmpty {groupInfo :: GroupInfo}
  | CRGroupRemoved {groupInfo :: GroupInfo}
  | CRGroupDeleted {groupInfo :: GroupInfo, member :: GroupMember}
  | CRMemberSubError {groupInfo :: GroupInfo, contactName :: ContactName, chatError :: ChatError} -- TODO Contact?  or GroupMember?
  | CRMemberSubErrors {memberSubErrors :: [MemberSubError]}
  | CRGroupSubscribed {groupInfo :: GroupInfo}
  | CRPendingSubSummary {pendingSubStatus :: [PendingSubStatus]}
  | CRSndFileSubError {sndFileTransfer :: SndFileTransfer, chatError :: ChatError}
  | CRRcvFileSubError {rcvFileTransfer :: RcvFileTransfer, chatError :: ChatError}
  | CRCallInvitation {contact :: Contact, callType :: CallType, sharedKey :: Maybe C.Key}
  | CRCallOffer {contact :: Contact, callType :: CallType, offer :: WebRTCSession, sharedKey :: Maybe C.Key, askConfirmation :: Bool}
  | CRCallAnswer {contact :: Contact, answer :: WebRTCSession}
  | CRCallExtraInfo {contact :: Contact, extraInfo :: WebRTCExtraInfo}
  | CRCallEnded {contact :: Contact}
  | CRUserContactLinkSubscribed
  | CRUserContactLinkSubError {chatError :: ChatError}
  | CRNtfTokenStatus {status :: NtfTknStatus}
  | CRNewContactConnection {connection :: PendingContactConnection}
  | CRContactConnectionDeleted {connection :: PendingContactConnection}
  | CRMessageError {severity :: Text, errorMessage :: Text}
  | CRChatCmdError {chatError :: ChatError}
  | CRChatError {chatError :: ChatError}
  deriving (Show, Generic)

instance ToJSON ChatResponse where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "CR"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "CR"

data ContactSubStatus = ContactSubStatus
  { contact :: Contact,
    contactError :: Maybe ChatError
  }
  deriving (Show, Generic)

instance ToJSON ContactSubStatus where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

data MemberSubError = MemberSubError
  { member :: GroupMember,
    memberError :: ChatError
  }
  deriving (Show, Generic)

instance ToJSON MemberSubError where
  toEncoding = J.genericToEncoding J.defaultOptions

data PendingSubStatus = PendingSubStatus
  { connId :: AgentConnId,
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
  | CEInvalidConnReq
  | CEInvalidChatMessage {message :: String}
  | CEContactNotReady {contact :: Contact}
  | CEContactGroups {contact :: Contact, groupNames :: [GroupName]}
  | CEGroupUserRole
  | CEGroupContactRole {contactName :: ContactName}
  | CEGroupDuplicateMember {contactName :: ContactName}
  | CEGroupDuplicateMemberId
  | CEGroupNotJoined {groupInfo :: GroupInfo}
  | CEGroupMemberNotActive
  | CEGroupMemberUserRemoved
  | CEGroupMemberNotFound {contactName :: ContactName}
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
  | CEInvalidQuote
  | CEInvalidChatItemUpdate
  | CEInvalidChatItemDelete
  | CEHasCurrentCall
  | CENoCurrentCall
  | CECallContact {contactId :: Int64}
  | CECallState {currentCallState :: CallStateTag}
  | CEAgentVersion
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
