{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Simplex.Chat.Remote.Protocol where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Crypto.Hash (SHA512)
import qualified Crypto.Hash as CH
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Key as JK
import qualified Data.Aeson.KeyMap as JM
import Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as JT
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder, byteString, lazyByteString)
import qualified Data.ByteString.Lazy as LB
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word32)
import qualified Network.HTTP.Types as N
import qualified Network.HTTP2.Client as H
import Network.Transport.Internal (decodeWord32, encodeWord32)
import Simplex.Chat.Call (RcvCallInvitation (..))
import Simplex.Chat.Controller
import Simplex.Chat.Messages (AChat (..), Chat (..))
import Simplex.Chat.Remote.Transport
import Simplex.Chat.Remote.Types
import Simplex.Chat.Types (RemoteHostId, User (..), UserInfo (..), ServerCfg (..))
import Simplex.FileTransfer.Description (FileDigest (..))
import Simplex.Messaging.Agent.Client (agentDRG)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.File (CryptoFile (..))
import Simplex.Messaging.Crypto.Lazy  (LazyByteString)
import Simplex.Messaging.Encoding
import Simplex.Messaging.Parsers (dropPrefix, taggedObjectJSON, pattern SingleFieldJSONTag, pattern TaggedObjectJSONData, pattern TaggedObjectJSONTag)
import Simplex.Messaging.Transport.Buffer (getBuffered)
import Simplex.Messaging.Transport.HTTP2 (HTTP2Body (..), HTTP2BodyChunk, getBodyChunk)
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Client, HTTP2Response (..), closeHTTP2Client, sendRequestDirect)
import Simplex.Messaging.Util (liftEitherError, liftEitherWith, liftError, tshow)
import Simplex.RemoteControl.Types (CtrlSessKeys (..), HostSessKeys (..), RCErrorType (..), SessionCode)
import Simplex.RemoteControl.Client (xrcpBlockSize)
import qualified Simplex.RemoteControl.Client as RC
import System.FilePath (takeFileName, (</>))
import UnliftIO

data RemoteCommand
  = RCSend {command :: Text} -- TODO maybe ChatCommand here?
  | RCRecv {wait :: Int} -- this wait should be less than HTTP timeout
  | -- local file encryption is determined by the host, but can be overridden for videos
    RCStoreFile {fileName :: String, fileSize :: Word32, fileDigest :: FileDigest} -- requires attachment
  | RCGetFile {file :: RemoteFile}
  deriving (Show)

data RemoteResponse
  = RRChatResponse {chatResponse :: ChatResponse}
  | RRChatEvent {chatEvent :: Maybe ChatResponse} -- ^ 'Nothing' on poll timeout
  | RRFileStored {filePath :: String}
  | RRFile {fileSize :: Word32, fileDigest :: FileDigest} -- provides attachment , fileDigest :: FileDigest
  | RRProtocolError {remoteProcotolError :: RemoteProtocolError} -- ^ The protocol error happened on the server side
  deriving (Show)

-- Force platform-independent encoding as the types aren't UI-visible
$(deriveJSON (taggedObjectJSON $ dropPrefix "RC") ''RemoteCommand)
$(deriveJSON (taggedObjectJSON $ dropPrefix "RR") ''RemoteResponse)

-- * Client side / desktop

mkRemoteHostClient :: ChatMonad m => HTTP2Client -> HostSessKeys -> SessionCode -> FilePath -> HostAppInfo -> m RemoteHostClient
mkRemoteHostClient httpClient sessionKeys sessionCode storePath HostAppInfo {encoding, deviceName, encryptFiles} = do
  drg <- asks $ agentDRG . smpAgent
  counter <- newTVarIO 1
  let HostSessKeys {hybridKey, idPrivKey, sessPrivKey} = sessionKeys
      signatures = RSSign {idPrivKey, sessPrivKey}
      encryption = RemoteCrypto {drg, counter, sessionCode, hybridKey, signatures}
  pure
    RemoteHostClient
      { hostEncoding = encoding,
        hostDeviceName = deviceName,
        httpClient,
        encryption,
        encryptHostFiles = encryptFiles,
        storePath
      }

mkCtrlRemoteCrypto :: ChatMonad m => CtrlSessKeys -> SessionCode -> m RemoteCrypto
mkCtrlRemoteCrypto CtrlSessKeys {hybridKey, idPubKey, sessPubKey} sessionCode = do
  drg <- asks $ agentDRG . smpAgent
  counter <- newTVarIO 1
  let signatures = RSVerify {idPubKey, sessPubKey}
  pure RemoteCrypto {drg, counter, sessionCode, hybridKey, signatures}

closeRemoteHostClient :: MonadIO m => RemoteHostClient -> m ()
closeRemoteHostClient RemoteHostClient {httpClient} = liftIO $ closeHTTP2Client httpClient

-- ** Commands

remoteSend :: RemoteHostId -> RemoteHostClient -> ByteString -> ExceptT RemoteProtocolError IO ChatResponse
remoteSend rhId c cmd =
  sendRemoteCommand' c Nothing RCSend {command = decodeUtf8 cmd} >>= \case
    RRChatResponse cr -> pure $ enrichCRRemoteHostId cr rhId
    r -> badResponse r

remoteRecv :: RemoteHostId -> RemoteHostClient -> Int -> ExceptT RemoteProtocolError IO (Maybe ChatResponse)
remoteRecv rhId c ms =
  sendRemoteCommand' c Nothing RCRecv {wait = ms} >>= \case
    RRChatEvent cr_ -> pure $ (`enrichCRRemoteHostId` rhId) <$> cr_
    r -> badResponse r

enrichCRRemoteHostId :: ChatResponse -> RemoteHostId -> ChatResponse
enrichCRRemoteHostId cr rhId = case cr of
  CRActiveUser {user} -> CRActiveUser {user = enrichUser user}
  CRUsersList {users} -> CRUsersList {users = map enrichUserInfo users}
  CRChatStarted -> cr
  CRChatRunning -> cr
  CRChatStopped -> cr
  CRChatSuspended -> cr
  CRApiChats {user, chats} -> CRApiChats {user = enrichUser user, chats = map enrichAChat chats}
  CRChats {chats} -> CRChats {chats = map enrichAChat chats}
  CRApiChat {user, chat} -> CRApiChat {user = enrichUser user, chat = enrichAChat chat}
  CRChatItems {user, chatName_, chatItems} -> CRChatItems {user = enrichUser user, chatName_, chatItems}
  CRChatItemInfo {user, chatItem, chatItemInfo} -> CRChatItemInfo {user = enrichUser user, chatItem, chatItemInfo}
  CRChatItemId user chatItemId_ -> CRChatItemId (enrichUser user) chatItemId_
  CRApiParsedMarkdown {formattedText = _ft} -> cr
  CRUserProtoServers {user, servers} -> CRUserProtoServers {user = enrichUser user, servers = enrichAUserProtoServers servers}
  CRServerTestResult {user, testServer, testFailure} -> CRServerTestResult {user = enrichUser user, testServer, testFailure}
  CRChatItemTTL {user, chatItemTTL} -> CRChatItemTTL {user = enrichUser user, chatItemTTL}
  CRNetworkConfig {networkConfig = _nc} -> cr
  CRContactInfo {user, contact, connectionStats_, customUserProfile} -> CRContactInfo {user = enrichUser user, contact, connectionStats_, customUserProfile}
  CRGroupInfo {user, groupInfo, groupSummary} -> CRGroupInfo {user = enrichUser user, groupInfo, groupSummary}
  CRGroupMemberInfo {user, groupInfo, member, connectionStats_} -> CRGroupMemberInfo {user = enrichUser user, groupInfo, member, connectionStats_}
  CRContactSwitchStarted {user, contact, connectionStats} -> CRContactSwitchStarted {user = enrichUser user, contact, connectionStats}
  CRGroupMemberSwitchStarted {user, groupInfo, member, connectionStats} -> CRGroupMemberSwitchStarted {user = enrichUser user, groupInfo, member, connectionStats}
  CRContactSwitchAborted {user, contact, connectionStats} -> CRContactSwitchAborted {user = enrichUser user, contact, connectionStats}
  CRGroupMemberSwitchAborted {user, groupInfo, member, connectionStats} -> CRGroupMemberSwitchAborted {user = enrichUser user, groupInfo, member, connectionStats}
  CRContactSwitch {user, contact, switchProgress} -> CRContactSwitch {user = enrichUser user, contact, switchProgress}
  CRGroupMemberSwitch {user, groupInfo, member, switchProgress} -> CRGroupMemberSwitch {user = enrichUser user, groupInfo, member, switchProgress}
  CRContactRatchetSyncStarted {user, contact, connectionStats} -> CRContactRatchetSyncStarted {user = enrichUser user, contact, connectionStats}
  CRGroupMemberRatchetSyncStarted {user, groupInfo, member, connectionStats} -> CRGroupMemberRatchetSyncStarted {user = enrichUser user, groupInfo, member, connectionStats}
  CRContactRatchetSync {user, contact, ratchetSyncProgress} -> CRContactRatchetSync {user = enrichUser user, contact, ratchetSyncProgress}
  CRGroupMemberRatchetSync {user, groupInfo, member, ratchetSyncProgress} -> CRGroupMemberRatchetSync {user = enrichUser user, groupInfo, member, ratchetSyncProgress}
  CRContactVerificationReset {user, contact} -> CRContactVerificationReset {user = enrichUser user, contact}
  CRGroupMemberVerificationReset {user, groupInfo, member} -> CRGroupMemberVerificationReset {user = enrichUser user, groupInfo, member}
  CRContactCode {user, contact, connectionCode} -> CRContactCode {user = enrichUser user, contact, connectionCode}
  CRGroupMemberCode {user, groupInfo, member, connectionCode} -> CRGroupMemberCode {user = enrichUser user, groupInfo, member, connectionCode}
  CRConnectionVerified {user, verified, expectedCode} -> CRConnectionVerified {user = enrichUser user, verified, expectedCode}
  CRNewChatItem {user, chatItem} -> CRNewChatItem {user = enrichUser user, chatItem}
  CRChatItemStatusUpdated {user, chatItem} -> CRChatItemStatusUpdated {user = enrichUser user, chatItem}
  CRChatItemUpdated {user, chatItem} -> CRChatItemUpdated {user = enrichUser user, chatItem}
  CRChatItemNotChanged {user, chatItem} -> CRChatItemNotChanged {user = enrichUser user, chatItem}
  CRChatItemReaction {user, added, reaction} -> CRChatItemReaction {user = enrichUser user, added, reaction}
  CRChatItemDeleted {user, deletedChatItem, toChatItem, byUser, timed} -> CRChatItemDeleted {user = enrichUser user, deletedChatItem, toChatItem, byUser, timed}
  CRChatItemDeletedNotFound {user, contact, sharedMsgId} -> CRChatItemDeletedNotFound {user = enrichUser user, contact, sharedMsgId}
  CRBroadcastSent {user, msgContent, successes, failures, timestamp} -> CRBroadcastSent {user = enrichUser user, msgContent, successes, failures, timestamp}
  CRMsgIntegrityError {user, msgError} -> CRMsgIntegrityError {user = enrichUser user, msgError}
  CRCmdAccepted {corr = _corr} -> cr
  CRCmdOk {user_} -> CRCmdOk {user_ = enrichUser <$> user_}
  CRChatHelp {helpSection = _hs} -> cr
  CRWelcome {user} -> CRWelcome {user = enrichUser user}
  CRGroupCreated {user, groupInfo} -> CRGroupCreated {user = enrichUser user, groupInfo}
  CRGroupMembers {user, group} -> CRGroupMembers {user = enrichUser user, group}
  CRContactsList {user, contacts} -> CRContactsList {user = enrichUser user, contacts}
  CRUserContactLink {user, contactLink} -> CRUserContactLink {user = enrichUser user, contactLink}
  CRUserContactLinkUpdated {user, contactLink} -> CRUserContactLinkUpdated {user = enrichUser user, contactLink}
  CRContactRequestRejected {user, contactRequest} -> CRContactRequestRejected {user = enrichUser user, contactRequest}
  CRUserAcceptedGroupSent {user, groupInfo, hostContact} -> CRUserAcceptedGroupSent {user = enrichUser user, groupInfo, hostContact}
  CRGroupLinkConnecting {user, groupInfo, hostMember} -> CRGroupLinkConnecting {user = enrichUser user, groupInfo, hostMember}
  CRUserDeletedMember {user, groupInfo, member} -> CRUserDeletedMember {user = enrichUser user, groupInfo, member}
  CRGroupsList {user, groups} -> CRGroupsList {user = enrichUser user, groups}
  CRSentGroupInvitation {user, groupInfo, contact, member} -> CRSentGroupInvitation {user = enrichUser user, groupInfo, contact, member}
  CRFileTransferStatus user ftStatus -> CRFileTransferStatus (enrichUser user) ftStatus
  CRFileTransferStatusXFTP user chatItem -> CRFileTransferStatusXFTP (enrichUser user) chatItem
  CRUserProfile {user, profile} -> CRUserProfile {user = enrichUser user, profile}
  CRUserProfileNoChange {user} -> CRUserProfileNoChange {user = enrichUser user}
  CRUserPrivacy {user, updatedUser} -> CRUserPrivacy {user = enrichUser user, updatedUser = enrichUser updatedUser}
  CRVersionInfo {versionInfo = _v, chatMigrations = _cm, agentMigrations = _am} -> cr
  CRInvitation {user, connReqInvitation, connection} -> CRInvitation {user = enrichUser user, connReqInvitation, connection}
  CRConnectionIncognitoUpdated {user, toConnection} -> CRConnectionIncognitoUpdated {user = enrichUser user, toConnection}
  CRConnectionPlan {user, connectionPlan} -> CRConnectionPlan {user = enrichUser user, connectionPlan}
  CRSentConfirmation {user} -> CRSentConfirmation {user = enrichUser user}
  CRSentInvitation {user, customUserProfile} -> CRSentInvitation {user = enrichUser user, customUserProfile}
  CRSentInvitationToContact {user, contact, customUserProfile} -> CRSentInvitationToContact {user = enrichUser user, contact, customUserProfile}
  CRContactUpdated {user, fromContact, toContact} -> CRContactUpdated {user = enrichUser user, fromContact, toContact}
  CRGroupMemberUpdated {user, groupInfo, fromMember, toMember} -> CRGroupMemberUpdated {user = enrichUser user, groupInfo, fromMember, toMember}
  CRContactsMerged {user, intoContact, mergedContact, updatedContact} -> CRContactsMerged {user = enrichUser user, intoContact, mergedContact, updatedContact}
  CRContactDeleted {user, contact} -> CRContactDeleted {user = enrichUser user, contact}
  CRContactDeletedByContact {user, contact} -> CRContactDeletedByContact {user = enrichUser user, contact}
  CRChatCleared {user, chatInfo} -> CRChatCleared {user = enrichUser user, chatInfo}
  CRUserContactLinkCreated {user, connReqContact} -> CRUserContactLinkCreated {user = enrichUser user, connReqContact}
  CRUserContactLinkDeleted {user} -> CRUserContactLinkDeleted {user = enrichUser user}
  CRReceivedContactRequest {user, contactRequest} -> CRReceivedContactRequest {user = enrichUser user, contactRequest}
  CRAcceptingContactRequest {user, contact} -> CRAcceptingContactRequest {user = enrichUser user, contact}
  CRContactAlreadyExists {user, contact} -> CRContactAlreadyExists {user = enrichUser user, contact}
  CRContactRequestAlreadyAccepted {user, contact} -> CRContactRequestAlreadyAccepted {user = enrichUser user, contact}
  CRLeftMemberUser {user, groupInfo} -> CRLeftMemberUser {user = enrichUser user, groupInfo}
  CRGroupDeletedUser {user, groupInfo} -> CRGroupDeletedUser {user = enrichUser user, groupInfo}
  CRRcvFileDescrReady {user, chatItem} -> CRRcvFileDescrReady {user = enrichUser user, chatItem}
  CRRcvFileAccepted {user, chatItem} -> CRRcvFileAccepted {user = enrichUser user, chatItem}
  CRRcvFileAcceptedSndCancelled {user, rcvFileTransfer} -> CRRcvFileAcceptedSndCancelled {user = enrichUser user, rcvFileTransfer}
  CRRcvFileDescrNotReady {user, chatItem} -> CRRcvFileDescrNotReady {user = enrichUser user, chatItem}
  CRRcvFileStart {user, chatItem} -> CRRcvFileStart {user = enrichUser user, chatItem}
  CRRcvFileProgressXFTP {user, chatItem, receivedSize, totalSize} -> CRRcvFileProgressXFTP {user = enrichUser user, chatItem, receivedSize, totalSize}
  CRRcvFileComplete {user, chatItem} -> CRRcvFileComplete {user = enrichUser user, chatItem}
  CRRcvFileCancelled {user, chatItem, rcvFileTransfer} -> CRRcvFileCancelled {user = enrichUser user, chatItem, rcvFileTransfer}
  CRRcvFileSndCancelled {user, chatItem, rcvFileTransfer} -> CRRcvFileSndCancelled {user = enrichUser user, chatItem, rcvFileTransfer}
  CRRcvFileError {user, chatItem, agentError} -> CRRcvFileError {user = enrichUser user, chatItem, agentError}
  CRSndFileStart {user, chatItem, sndFileTransfer} -> CRSndFileStart {user = enrichUser user, chatItem, sndFileTransfer}
  CRSndFileComplete {user, chatItem, sndFileTransfer} -> CRSndFileComplete {user = enrichUser user, chatItem, sndFileTransfer}
  CRSndFileRcvCancelled {user, chatItem, sndFileTransfer} -> CRSndFileRcvCancelled {user = enrichUser user, chatItem, sndFileTransfer}
  CRSndFileCancelled {user, chatItem, fileTransferMeta, sndFileTransfers} -> CRSndFileCancelled {user = enrichUser user, chatItem, fileTransferMeta, sndFileTransfers}
  CRSndFileStartXFTP {user, chatItem, fileTransferMeta} -> CRSndFileStartXFTP {user = enrichUser user, chatItem, fileTransferMeta}
  CRSndFileProgressXFTP {user, chatItem, fileTransferMeta, sentSize, totalSize} -> CRSndFileProgressXFTP {user = enrichUser user, chatItem, fileTransferMeta, sentSize, totalSize}
  CRSndFileCompleteXFTP {user, chatItem, fileTransferMeta} -> CRSndFileCompleteXFTP {user = enrichUser user, chatItem, fileTransferMeta}
  CRSndFileCancelledXFTP {user, chatItem, fileTransferMeta} -> CRSndFileCancelledXFTP {user = enrichUser user, chatItem, fileTransferMeta}
  CRSndFileError {user, chatItem} -> CRSndFileError {user = enrichUser user, chatItem}
  CRUserProfileUpdated {user, fromProfile, toProfile, updateSummary} -> CRUserProfileUpdated {user = enrichUser user, fromProfile, toProfile, updateSummary}
  CRUserProfileImage {user, profile} -> CRUserProfileImage {user = enrichUser user, profile}
  CRContactAliasUpdated {user, toContact} -> CRContactAliasUpdated {user = enrichUser user, toContact}
  CRConnectionAliasUpdated {user, toConnection} -> CRConnectionAliasUpdated {user = enrichUser user, toConnection}
  CRContactPrefsUpdated {user, fromContact, toContact} -> CRContactPrefsUpdated {user = enrichUser user, fromContact, toContact}
  CRContactConnecting {user, contact} -> CRContactConnecting {user = enrichUser user, contact}
  CRContactConnected {user, contact, userCustomProfile} -> CRContactConnected {user = enrichUser user, contact, userCustomProfile}
  CRContactAnotherClient {user, contact} -> CRContactAnotherClient {user = enrichUser user, contact}
  CRSubscriptionEnd {user, connectionEntity} -> CRSubscriptionEnd {user = enrichUser user, connectionEntity}
  CRContactsDisconnected {server = _srv, contactRefs = _cr} -> cr
  CRContactsSubscribed {server = _srv, contactRefs = _cr} -> cr
  CRContactSubError {user, contact, chatError} -> CRContactSubError {user = enrichUser user, contact, chatError}
  CRContactSubSummary {user, contactSubscriptions} -> CRContactSubSummary {user = enrichUser user, contactSubscriptions}
  CRUserContactSubSummary {user, userContactSubscriptions} -> CRUserContactSubSummary {user = enrichUser user, userContactSubscriptions}
  CRNetworkStatus {networkStatus = _ns, connections = _cns} -> cr
  CRNetworkStatuses {user_, networkStatuses} -> CRNetworkStatuses {user_ = enrichUser <$> user_, networkStatuses}
  CRHostConnected {protocol = _p, transportHost = _th} -> cr
  CRHostDisconnected {protocol = _p, transportHost = _th} -> cr
  CRGroupInvitation {user, groupInfo} -> CRGroupInvitation {user = enrichUser user, groupInfo}
  CRReceivedGroupInvitation {user, groupInfo, contact, fromMemberRole, memberRole} -> CRReceivedGroupInvitation {user = enrichUser user, groupInfo, contact, fromMemberRole, memberRole}
  CRUserJoinedGroup {user, groupInfo, hostMember} -> CRUserJoinedGroup {user = enrichUser user, groupInfo, hostMember}
  CRJoinedGroupMember {user, groupInfo, member} -> CRJoinedGroupMember {user = enrichUser user, groupInfo, member}
  CRJoinedGroupMemberConnecting {user, groupInfo, hostMember, member} -> CRJoinedGroupMemberConnecting {user = enrichUser user, groupInfo, hostMember, member}
  CRMemberRole {user, groupInfo, byMember, member, fromRole, toRole} -> CRMemberRole {user = enrichUser user, groupInfo, byMember, member, fromRole, toRole}
  CRMemberRoleUser {user, groupInfo, member, fromRole, toRole} -> CRMemberRoleUser {user = enrichUser user, groupInfo, member, fromRole, toRole}
  CRConnectedToGroupMember {user, groupInfo, member, memberContact} -> CRConnectedToGroupMember {user = enrichUser user, groupInfo, member, memberContact}
  CRDeletedMember {user, groupInfo, byMember, deletedMember} -> CRDeletedMember {user = enrichUser user, groupInfo, byMember, deletedMember}
  CRDeletedMemberUser {user, groupInfo, member} -> CRDeletedMemberUser {user = enrichUser user, groupInfo, member}
  CRLeftMember {user, groupInfo, member} -> CRLeftMember {user = enrichUser user, groupInfo, member}
  CRGroupEmpty {user, groupInfo} -> CRGroupEmpty {user = enrichUser user, groupInfo}
  CRGroupRemoved {user, groupInfo} -> CRGroupRemoved {user = enrichUser user, groupInfo}
  CRGroupDeleted {user, groupInfo, member} -> CRGroupDeleted {user = enrichUser user, groupInfo, member}
  CRGroupUpdated {user, fromGroup, toGroup, member_} -> CRGroupUpdated {user = enrichUser user, fromGroup, toGroup, member_}
  CRGroupProfile {user, groupInfo} -> CRGroupProfile {user = enrichUser user, groupInfo}
  CRGroupDescription {user, groupInfo} -> CRGroupDescription {user = enrichUser user, groupInfo}
  CRGroupLinkCreated {user, groupInfo, connReqContact, memberRole} -> CRGroupLinkCreated {user = enrichUser user, groupInfo, connReqContact, memberRole}
  CRGroupLink {user, groupInfo, connReqContact, memberRole} -> CRGroupLink {user = enrichUser user, groupInfo, connReqContact, memberRole}
  CRGroupLinkDeleted {user, groupInfo} -> CRGroupLinkDeleted {user = enrichUser user, groupInfo}
  CRAcceptingGroupJoinRequest {user, groupInfo, contact} -> CRAcceptingGroupJoinRequest {user = enrichUser user, groupInfo, contact}
  CRAcceptingGroupJoinRequestMember {user, groupInfo, member} -> CRAcceptingGroupJoinRequestMember {user = enrichUser user, groupInfo, member}
  CRNoMemberContactCreating {user, groupInfo, member} -> CRNoMemberContactCreating {user = enrichUser user, groupInfo, member}
  CRNewMemberContact {user, contact, groupInfo, member} -> CRNewMemberContact {user = enrichUser user, contact, groupInfo, member}
  CRNewMemberContactSentInv {user, contact, groupInfo, member} -> CRNewMemberContactSentInv {user = enrichUser user, contact, groupInfo, member}
  CRNewMemberContactReceivedInv {user, contact, groupInfo, member} -> CRNewMemberContactReceivedInv {user = enrichUser user, contact, groupInfo, member}
  CRContactAndMemberAssociated {user, contact, groupInfo, member, updatedContact} -> CRContactAndMemberAssociated {user = enrichUser user, contact, groupInfo, member, updatedContact}
  CRMemberSubError {user, groupInfo, member, chatError} -> CRMemberSubError {user = enrichUser user, groupInfo, member, chatError}
  CRMemberSubSummary {user, memberSubscriptions} -> CRMemberSubSummary {user = enrichUser user, memberSubscriptions}
  CRGroupSubscribed {user, groupInfo} -> CRGroupSubscribed {user = enrichUser user, groupInfo}
  CRPendingSubSummary {user, pendingSubscriptions} -> CRPendingSubSummary {user = enrichUser user, pendingSubscriptions}
  CRSndFileSubError {user, sndFileTransfer, chatError} -> CRSndFileSubError {user = enrichUser user, sndFileTransfer, chatError}
  CRRcvFileSubError {user, rcvFileTransfer, chatError} -> CRRcvFileSubError {user = enrichUser user, rcvFileTransfer, chatError}
  CRCallInvitation {callInvitation} -> CRCallInvitation {callInvitation = enrichRcvCallInvitation callInvitation}
  CRCallOffer {user, contact, callType, offer, sharedKey, askConfirmation} -> CRCallOffer {user = enrichUser user, contact, callType, offer, sharedKey, askConfirmation}
  CRCallAnswer {user, contact, answer} -> CRCallAnswer {user = enrichUser user, contact, answer}
  CRCallExtraInfo {user, contact, extraInfo} -> CRCallExtraInfo {user = enrichUser user, contact, extraInfo}
  CRCallEnded {user, contact} -> CRCallEnded {user = enrichUser user, contact}
  CRCallInvitations {callInvitations} -> CRCallInvitations {callInvitations = map enrichRcvCallInvitation callInvitations}
  CRUserContactLinkSubscribed -> cr
  CRUserContactLinkSubError {chatError = _ce} -> cr
  CRNtfTokenStatus {status = _s} -> cr
  CRNtfToken {token = _t, status = _s, ntfMode = _nm} -> cr
  CRNtfMessages {user_, connEntity, msgTs, ntfMessages} -> CRNtfMessages {user_ = enrichUser <$> user_, connEntity, msgTs, ntfMessages}
  CRNewContactConnection {user, connection} -> CRNewContactConnection {user = enrichUser user, connection}
  CRContactConnectionDeleted {user, connection} -> CRContactConnectionDeleted {user = enrichUser user, connection}
  CRRemoteHostList {remoteHosts = _rh} -> cr
  CRCurrentRemoteHost {remoteHost_ = _rh} -> cr
  CRRemoteHostStarted {remoteHost_ = _rh, invitation = _i} -> cr
  CRRemoteHostSessionCode {remoteHost_ = _rh, sessionCode = _sc} -> cr
  CRNewRemoteHost {remoteHost = _rh} -> cr
  CRRemoteHostConnected {remoteHost = _rh} -> cr
  CRRemoteHostStopped {remoteHostId_ = _rh} -> cr
  CRRemoteFileStored {remoteHostId = _rhId, remoteFileSource = _rfs} -> cr
  CRRemoteCtrlList {remoteCtrls = _rc} -> cr
  CRRemoteCtrlFound {remoteCtrl = _rc} -> cr
  CRRemoteCtrlConnecting {remoteCtrl_ = _rc, ctrlAppInfo = _cai, appVersion = _av} -> cr
  CRRemoteCtrlSessionCode {remoteCtrl_ = _rc, sessionCode = _sc} -> cr
  CRRemoteCtrlConnected {remoteCtrl = _rc} -> cr
  CRRemoteCtrlStopped -> cr
  CRSQLResult {rows = _r} -> cr
  CRSlowSQLQueries {chatQueries = _cq, agentQueries = _aq} -> cr
  CRDebugLocks {chatLockName = _cl, agentLocks = _al} -> cr
  CRAgentStats {agentStats = _as} -> cr
  CRAgentSubs {activeSubs = _as, pendingSubs = _ps, removedSubs = _rs} -> cr
  CRAgentSubsDetails {agentSubs = _as} -> cr
  CRConnectionDisabled {connectionEntity = _ce} -> cr
  CRAgentRcvQueueDeleted {agentConnId = _acId, server = _srv, agentQueueId = _aqId, agentError_ = _ae} -> cr
  CRAgentConnDeleted {agentConnId = _acId} -> cr
  CRAgentUserDeleted {agentUserId = _auId} -> cr
  CRMessageError {user, severity, errorMessage} -> CRMessageError {user = enrichUser user, severity, errorMessage}
  CRChatCmdError {user_, chatError} -> CRChatCmdError {user_ = enrichUser <$> user_, chatError}
  CRChatError {user_, chatError} -> CRChatError {user_ = enrichUser <$> user_, chatError}
  CRArchiveImported {archiveErrors = _ae} -> cr
  CRTimedAction {action = _a, durationMilliseconds = _dm} -> cr
  where
    enrichUser :: User -> User
    enrichUser u = u {remoteHostId = Just rhId}
    enrichUserInfo :: UserInfo -> UserInfo
    enrichUserInfo uInfo@UserInfo {user} = uInfo {user = enrichUser user}
    enrichAChat :: AChat -> AChat
    enrichAChat (AChat cType chat) = AChat cType chat {remoteHostId = Just rhId}
    enrichServerCfg :: ServerCfg p -> ServerCfg p
    enrichServerCfg cfg = cfg {remoteHostId = Just rhId}
    enrichAUserProtoServers :: AUserProtoServers -> AUserProtoServers
    enrichAUserProtoServers (AUPS ups@UserProtoServers {protoServers}) = AUPS ups {protoServers = fmap enrichServerCfg protoServers}
    enrichRcvCallInvitation :: RcvCallInvitation -> RcvCallInvitation
    enrichRcvCallInvitation rci = rci {remoteHostId = Just rhId}


remoteStoreFile :: RemoteHostClient -> FilePath -> FilePath -> ExceptT RemoteProtocolError IO FilePath
remoteStoreFile c localPath fileName = do
  (fileSize, fileDigest) <- getFileInfo localPath
  let send h = sendRemoteCommand' c (Just (h, fileSize)) RCStoreFile {fileName, fileSize, fileDigest}
  withFile localPath ReadMode send >>= \case
    RRFileStored {filePath = filePath'} -> pure filePath'
    r -> badResponse r

remoteGetFile :: RemoteHostClient -> FilePath -> RemoteFile -> ExceptT RemoteProtocolError IO ()
remoteGetFile c@RemoteHostClient{encryption} destDir rf@RemoteFile {fileSource = CryptoFile {filePath}} =
  sendRemoteCommand c Nothing RCGetFile {file = rf} >>= \case
    (getChunk, RRFile {fileSize, fileDigest}) -> do
      -- TODO we could optimize by checking size and hash before receiving the file
      let localPath = destDir </> takeFileName filePath
      receiveEncryptedFile encryption getChunk fileSize fileDigest localPath
    (_, r) -> badResponse r

-- TODO validate there is no attachment in response
sendRemoteCommand' :: RemoteHostClient -> Maybe (Handle, Word32) -> RemoteCommand -> ExceptT RemoteProtocolError IO RemoteResponse
sendRemoteCommand' c attachment_ rc = snd <$> sendRemoteCommand c attachment_ rc

sendRemoteCommand :: RemoteHostClient -> Maybe (Handle, Word32) -> RemoteCommand -> ExceptT RemoteProtocolError IO (Int -> IO ByteString, RemoteResponse)
sendRemoteCommand RemoteHostClient {httpClient, hostEncoding, encryption} file_ cmd = do
  encFile_ <- mapM (prepareEncryptedFile encryption) file_
  req <- httpRequest encFile_ <$> encryptEncodeHTTP2Body encryption (J.encode cmd)
  HTTP2Response {response, respBody} <- liftEitherError (RPEHTTP2 . tshow) $ sendRequestDirect httpClient req Nothing
  (header, getNext) <- parseDecryptHTTP2Body encryption response respBody
  rr <- liftEitherWith (RPEInvalidJSON . fromString) $ J.eitherDecode header >>= JT.parseEither J.parseJSON . convertJSON hostEncoding localEncoding
  pure (getNext, rr)
  where
    httpRequest encFile_ cmdBld = H.requestStreaming N.methodPost "/" mempty $ \send flush -> do
      send cmdBld
      forM_ encFile_ (`sendEncryptedFile` send)
      flush

badResponse :: RemoteResponse -> ExceptT RemoteProtocolError IO a
badResponse = \case
  RRProtocolError e -> throwError e
  -- TODO handle chat errors?
  r -> throwError $ RPEUnexpectedResponse $ tshow r

-- * Transport-level wrappers

convertJSON :: PlatformEncoding -> PlatformEncoding -> J.Value -> J.Value
convertJSON _remote@PEKotlin _local@PEKotlin = id
convertJSON PESwift PESwift = id
convertJSON PESwift PEKotlin = owsf2tagged
convertJSON PEKotlin PESwift = error "unsupported convertJSON: K/S" -- guarded by handshake

-- | Convert swift single-field sum encoding into tagged/discriminator-field
owsf2tagged :: J.Value -> J.Value
owsf2tagged = fst . convert
  where
    convert val = case val of
      J.Object o
        | JM.size o == 2 ->
            case JM.toList o of
              [OwsfTag, o'] -> tagged o'
              [o', OwsfTag] -> tagged o'
              _ -> props
        | otherwise -> props
        where
          props = (J.Object $ fmap owsf2tagged o, False)
      J.Array a -> (J.Array $ fmap owsf2tagged a, False)
      _ -> (val, False)
    -- `tagged` converts the pair of single-field object encoding to tagged encoding.
    -- It sets innerTag returned by `convert` to True to prevent the tag being overwritten.
    tagged (k, v) = (J.Object pairs, True)
      where
        (v', innerTag) = convert v
        pairs = case v' of
          -- `innerTag` indicates that internal object already has tag,
          -- so the current tag cannot be inserted into it.
          J.Object o
            | innerTag -> pair
            | otherwise -> JM.insert TaggedObjectJSONTag tag o
          _ -> pair
        tag = J.String $ JK.toText k
        pair = JM.fromList [TaggedObjectJSONTag .= tag, TaggedObjectJSONData .= v']

pattern OwsfTag :: (JK.Key, J.Value)
pattern OwsfTag = (SingleFieldJSONTag, J.Bool True)

-- ```
-- commandBody = encBody sessSignature idSignature (attachment / noAttachment)
-- responseBody = encBody attachment; should match counter in the command
-- encBody = nonce encLength32 encrypted(tlsunique counter body)
-- attachment = %x01 nonce encLength32 encrypted(attachment)
-- noAttachment = %x00
-- tlsunique = length 1*OCTET
-- nonce = 24*24 OCTET
-- counter = 8*8 OCTET ; int64
-- encLength32 = 4*4 OCTET ; uint32, includes authTag
-- ```

-- See https://github.com/simplex-chat/simplexmq/blob/master/rfcs/2023-10-25-remote-control.md for encoding

encryptEncodeHTTP2Body :: RemoteCrypto -> LazyByteString -> ExceptT RemoteProtocolError IO Builder
encryptEncodeHTTP2Body RemoteCrypto {drg, counter, sessionCode, hybridKey, signatures} s = do
  corrId <- atomically $ stateTVar counter $ \c -> (c, c + 1)
  let pfx = smpEncode (sessionCode, corrId)
  (nonce, ct) <- liftError PRERemoteControl $ RC.rcEncryptBody drg hybridKey $ LB.fromStrict pfx <> s
  let ctLen = encodeWord32 (fromIntegral $ LB.length ct)
      signed = LB.fromStrict (smpEncode nonce <> ctLen) <> ct
  sigs <- bodySignatures signed
  pure $ lazyByteString signed <> sigs
  where
    bodySignatures :: LazyByteString -> ExceptT RemoteProtocolError IO Builder
    bodySignatures signed = case signatures of
      RSSign {idPrivKey, sessPrivKey} -> do
        let hc = CH.hashUpdates (CH.hashInit @SHA512) (LB.toChunks signed)
            ssig = sign sessPrivKey hc
            idsig = sign idPrivKey $ CH.hashUpdate hc ssig
        pure $ byteString $ smpEncode (ssig, idsig)
      _ -> pure mempty
    sign :: C.PrivateKeyEd25519 -> CH.Context SHA512 -> ByteString
    sign k = C.signatureBytes . C.sign' k . BA.convert . CH.hashFinalize

-- | Parse and decrypt HTTP2 request/response
parseDecryptHTTP2Body :: HTTP2BodyChunk a => RemoteCrypto -> a -> HTTP2Body -> ExceptT RemoteProtocolError IO (LazyByteString, Int -> IO ByteString)
parseDecryptHTTP2Body RemoteCrypto {hybridKey, sessionCode, signatures} hr HTTP2Body {bodyBuffer} = do
  (nonce, ct) <- getBody
  s <- liftError PRERemoteControl $ RC.rcDecryptBody hybridKey nonce ct
  (,getNext) <$> parseBody s
  where
    getBody :: ExceptT RemoteProtocolError IO (C.CbNonce, LazyByteString)
    getBody = do
      nonceStr <- liftIO $ getNext 24
      nonce <- liftEitherWith RPEInvalidBody $ smpDecode nonceStr
      ctLenStr <- liftIO $ getNext 4
      let ctLen = decodeWord32 ctLenStr
      when (ctLen > fromIntegral (maxBound :: Int)) $ throwError RPEInvalidSize
      chunks <- liftIO $ getLazy $ fromIntegral ctLen
      let hc = CH.hashUpdates (CH.hashInit @SHA512) [nonceStr, ctLenStr]
          hc' = CH.hashUpdates hc chunks
      verifySignatures hc'
      pure (nonce, LB.fromChunks chunks)
    getLazy :: Int -> IO [ByteString]
    getLazy 0 = pure []
    getLazy n = do
      let sz = min n xrcpBlockSize
      bs <- getNext sz
      let n' = if B.length bs < sz then 0 else max 0 (n - xrcpBlockSize)
      (bs :) <$> getLazy n'
    verifySignatures :: CH.Context SHA512 -> ExceptT RemoteProtocolError IO ()
    verifySignatures hc = case signatures of
      RSVerify {sessPubKey, idPubKey} -> do
        ssig <- getSig
        idsig <- getSig
        verifySig sessPubKey ssig hc
        verifySig idPubKey idsig $ CH.hashUpdate hc $ C.signatureBytes ssig
      _ -> pure ()
      where
        getSig = do
          len <- liftIO $ B.head <$> getNext 1
          liftEitherError RPEInvalidBody $ C.decodeSignature <$> getNext (fromIntegral len)
        verifySig key sig hc' = do
          let signed = BA.convert $ CH.hashFinalize hc'
          unless (C.verify' key sig signed) $ throwError $ PRERemoteControl RCECtrlAuth
    parseBody :: LazyByteString -> ExceptT RemoteProtocolError IO LazyByteString
    parseBody s = case LB.uncons s of
      Nothing -> throwError $ RPEInvalidBody "empty body"
      Just (scLen, rest) -> do
        (sessCode', rest') <- takeBytes (fromIntegral scLen) rest
        unless (sessCode' == sessionCode) $ throwError PRESessionCode
        (_corrId, s') <- takeBytes 8 rest'
        pure s'
      where
        takeBytes n s' = do
          let (bs, rest) = LB.splitAt n s'
          unless (LB.length bs == n) $ throwError PRESessionCode
          pure (LB.toStrict bs, rest)
    getNext sz = getBuffered bodyBuffer sz Nothing $ getBodyChunk hr
