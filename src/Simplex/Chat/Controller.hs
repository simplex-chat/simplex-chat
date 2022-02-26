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
import Data.Aeson (ToJSON)
import qualified Data.Aeson as J
import Data.ByteString.Char8 (ByteString)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Numeric.Natural
import Simplex.Chat.Messages
import Simplex.Chat.Protocol
import Simplex.Chat.Store (StoreError)
import Simplex.Chat.Types
import Simplex.Messaging.Agent (AgentClient)
import Simplex.Messaging.Agent.Env.SQLite (AgentConfig)
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Agent.Store.SQLite (SQLiteStore)
import Simplex.Messaging.Parsers (dropPrefix, enumJSON, sumTypeJSON)
import Simplex.Messaging.Protocol (CorrId)
import System.IO (Handle)
import UnliftIO.STM

versionNumber :: String
versionNumber = "1.3.0"

versionStr :: String
versionStr = "SimpleX Chat v" <> versionNumber

updateStr :: String
updateStr = "To update run: curl -o- https://raw.githubusercontent.com/simplex-chat/simplex-chat/master/install.sh | bash"

data ChatConfig = ChatConfig
  { agentConfig :: AgentConfig,
    dbPoolSize :: Int,
    yesToMigrations :: Bool,
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
    config :: ChatConfig
  }

data HelpSection = HSMain | HSFiles | HSGroups | HSMyAddress | HSMarkdown
  deriving (Show, Generic)

instance ToJSON HelpSection where
  toJSON = J.genericToJSON . enumJSON $ dropPrefix "HS"
  toEncoding = J.genericToEncoding . enumJSON $ dropPrefix "HS"

data ChatCommand
  = ShowActiveUser
  | CreateActiveUser Profile
  | StartChat
  | APIGetChats
  | APIGetChat ChatType Int64 ChatPagination
  | APIGetChatItems Int
  | APISendMessage ChatType Int64 MsgContent
  | APIChatRead ChatType Int64 (ChatItemId, ChatItemId)
  | APIDeleteChat ChatType Int64
  | APIAcceptContact Int64
  | APIRejectContact Int64
  | ChatHelp HelpSection
  | Welcome
  | AddContact
  | Connect (Maybe AConnectionRequestUri)
  | ConnectAdmin
  | DeleteContact ContactName
  | ListContacts
  | CreateMyAddress
  | DeleteMyAddress
  | ShowMyAddress
  | AddressAutoAccept Bool
  | AcceptContact ContactName
  | RejectContact ContactName
  | SendMessage ContactName ByteString
  | NewGroup GroupProfile
  | AddMember GroupName ContactName GroupMemberRole
  | JoinGroup GroupName
  | RemoveMember GroupName ContactName
  | MemberRole GroupName ContactName GroupMemberRole
  | LeaveGroup GroupName
  | DeleteGroup GroupName
  | ListMembers GroupName
  | ListGroups
  | SendGroupMessage GroupName ByteString
  | SendFile ContactName FilePath
  | SendGroupFile GroupName FilePath
  | ReceiveFile FileTransferId (Maybe FilePath)
  | CancelFile FileTransferId
  | FileStatus FileTransferId
  | ShowProfile
  | UpdateProfile Profile
  | QuitChat
  | ShowVersion
  deriving (Show)

data ChatResponse
  = CRActiveUser {user :: User}
  | CRChatStarted
  | CRApiChats {chats :: [AChat]}
  | CRApiChat {chat :: AChat}
  | CRNewChatItem {chatItem :: AChatItem}
  | CRChatItemUpdated {chatItem :: AChatItem}
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
  | CRUserContactLinkCreated {connReqContact :: ConnReqContact}
  | CRUserContactLinkDeleted
  | CRReceivedContactRequest {contactRequest :: UserContactRequest}
  | CRAcceptingContactRequest {contact :: Contact}
  | CRContactAlreadyExists {contact :: Contact}
  | CRContactRequestAlreadyAccepted {contact :: Contact}
  | CRLeftMemberUser {groupInfo :: GroupInfo}
  | CRGroupDeletedUser {groupInfo :: GroupInfo}
  | CRRcvFileAccepted {fileTransfer :: RcvFileTransfer, filePath :: FilePath}
  | CRRcvFileAcceptedSndCancelled {rcvFileTransfer :: RcvFileTransfer}
  | CRRcvFileStart {rcvFileTransfer :: RcvFileTransfer}
  | CRRcvFileComplete {rcvFileTransfer :: RcvFileTransfer}
  | CRRcvFileCancelled {rcvFileTransfer :: RcvFileTransfer}
  | CRRcvFileSndCancelled {rcvFileTransfer :: RcvFileTransfer}
  | CRSndFileStart {sndFileTransfer :: SndFileTransfer}
  | CRSndFileComplete {sndFileTransfer :: SndFileTransfer}
  | CRSndFileCancelled {sndFileTransfer :: SndFileTransfer}
  | CRSndFileRcvCancelled {sndFileTransfer :: SndFileTransfer}
  | CRSndGroupFileCancelled {sndFileTransfers :: [SndFileTransfer]}
  | CRUserProfileUpdated {fromProfile :: Profile, toProfile :: Profile}
  | CRContactConnecting {contact :: Contact}
  | CRContactConnected {contact :: Contact}
  | CRContactAnotherClient {contact :: Contact}
  | CRContactDisconnected {contact :: Contact}
  | CRContactSubscribed {contact :: Contact}
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
  | CRSndFileSubError {sndFileTransfer :: SndFileTransfer, chatError :: ChatError}
  | CRRcvFileSubError {rcvFileTransfer :: RcvFileTransfer, chatError :: ChatError}
  | CRUserContactLinkSubscribed
  | CRUserContactLinkSubError {chatError :: ChatError}
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
  | CEFileAlreadyExists {filePath :: FilePath}
  | CEFileRead {filePath :: FilePath, message :: String}
  | CEFileWrite {filePath :: FilePath, message :: String}
  | CEFileSend {fileId :: FileTransferId, agentError :: AgentErrorType}
  | CEFileRcvChunk {message :: String}
  | CEFileInternal {message :: String}
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
