import {ChatItemId, MsgContent, DeleteMode, Profile, GroupMemberRole} from "./command"

export type ChatResponse =
  | CRActiveUser
  | CRChatStarted
  | CRChatRunning
  | CRChatStopped
  | CRApiChats
  | CRApiChat
  | CRApiParsedMarkdown
  | CRUserSMPServers
  | CRContactInfo
  | CRGroupMemberInfo
  | CRNewChatItem
  | CRChatItemStatusUpdated
  | CRChatItemUpdated
  | CRChatItemDeleted
  | CRMsgIntegrityError
  | CRCmdOk
  | CRUserContactLink
  | CRUserContactLinkUpdated
  | CRContactRequestRejected
  | CRUserProfile
  | CRUserProfileNoChange
  | CRUserProfileUpdated
  | CRContactAliasUpdated
  | CRInvitation
  | CRSentConfirmation
  | CRSentInvitation
  | CRContactUpdated
  | CRContactsMerged
  | CRContactDeleted
  | CRChatCleared
  | CRUserContactLinkCreated
  | CRUserContactLinkDeleted
  | CRReceivedContactRequest
  | CRAcceptingContactRequest
  | CRContactAlreadyExists
  | CRContactRequestAlreadyAccepted
  | CRContactConnecting
  | CRContactConnected
  | CRContactAnotherClient
  | CRContactDisconnected
  | CRContactSubscribed
  | CRContactSubError
  | CRContactSubSummary
  | CRContactsDisconnected
  | CRContactsSubscribed
  | CRHostConnected
  | CRHostDisconnected
  | CRGroupEmpty
  | CRMemberSubError
  | CRMemberSubSummary
  | CRGroupSubscribed
  | CRRcvFileAccepted
  | CRRcvFileAcceptedSndCancelled
  | CRRcvFileStart
  | CRRcvFileComplete
  | CRRcvFileCancelled
  | CRRcvFileSndCancelled
  | CRSndFileStart
  | CRSndFileComplete
  | CRSndFileCancelled
  | CRSndFileRcvCancelled
  | CRSndGroupFileCancelled
  | CRSndFileSubError
  | CRRcvFileSubError
  | CRPendingSubSummary
  | CRGroupCreated
  | CRGroupMembers
  | CRUserAcceptedGroupSent
  | CRUserDeletedMember
  | CRSentGroupInvitation
  | CRLeftMemberUser
  | CRGroupDeletedUser
  | CRGroupInvitation
  | CRReceivedGroupInvitation
  | CRUserJoinedGroup
  | CRJoinedGroupMember
  | CRJoinedGroupMemberConnecting
  | CRConnectedToGroupMember
  | CRDeletedMember
  | CRDeletedMemberUser
  | CRLeftMember
  | CRGroupRemoved
  | CRGroupDeleted
  | CRGroupUpdated
  | CRUserContactLinkSubscribed
  | CRUserContactLinkSubError
  | CRNewContactConnection
  | CRContactConnectionDeleted
  | CRMessageError
  | CRChatCmdError
  | CRChatError

// not included
// CRChatItemDeletedNotFound
// CRBroadcastSent
// CRGroupsList
// CRFileTransferStatus

type ChatResponseTag =
  | "activeUser"
  | "chatStarted"
  | "chatRunning"
  | "chatStopped"
  | "apiChats"
  | "apiChat"
  | "apiParsedMarkdown"
  | "userSMPServers"
  | "contactInfo"
  | "groupMemberInfo"
  | "newChatItem"
  | "chatItemStatusUpdated"
  | "chatItemUpdated"
  | "chatItemDeleted"
  | "msgIntegrityError"
  | "cmdOk"
  | "userContactLink"
  | "userContactLinkUpdated"
  | "userContactLinkCreated"
  | "userContactLinkDeleted"
  | "contactRequestRejected"
  | "userProfile"
  | "userProfileNoChange"
  | "userProfileUpdated"
  | "contactAliasUpdated"
  | "invitation"
  | "sentConfirmation"
  | "sentInvitation"
  | "contactUpdated"
  | "contactsMerged"
  | "contactDeleted"
  | "chatCleared"
  | "receivedContactRequest"
  | "acceptingContactRequest"
  | "contactAlreadyExists"
  | "contactRequestAlreadyAccepted"
  | "contactConnecting"
  | "contactConnected"
  | "contactAnotherClient"
  | "contactDisconnected"
  | "contactSubscribed"
  | "contactSubError"
  | "contactSubSummary"
  | "contactsDisconnected"
  | "contactsSubscribed"
  | "hostConnected"
  | "hostDisconnected"
  | "groupEmpty"
  | "memberSubError"
  | "memberSubSummary"
  | "groupSubscribed"
  | "rcvFileAccepted"
  | "rcvFileAcceptedSndCancelled"
  | "rcvFileStart"
  | "rcvFileComplete"
  | "rcvFileCancelled"
  | "rcvFileSndCancelled"
  | "sndFileStart"
  | "sndFileComplete"
  | "sndFileCancelled"
  | "sndFileRcvCancelled"
  | "sndGroupFileCancelled"
  | "fileTransferStatus"
  | "sndFileSubError"
  | "rcvFileSubError"
  | "pendingSubSummary"
  | "groupCreated"
  | "groupMembers"
  | "userAcceptedGroupSent"
  | "userDeletedMember"
  | "sentGroupInvitation"
  | "leftMemberUser"
  | "groupDeletedUser"
  | "groupInvitation"
  | "receivedGroupInvitation"
  | "userJoinedGroup"
  | "joinedGroupMember"
  | "joinedGroupMemberConnecting"
  | "connectedToGroupMember"
  | "deletedMember"
  | "deletedMemberUser"
  | "leftMember"
  | "groupRemoved"
  | "groupDeleted"
  | "groupUpdated"
  | "userContactLinkSubscribed"
  | "userContactLinkSubError"
  | "newContactConnection"
  | "contactConnectionDeleted"
  | "messageError"
  | "chatCmdError"
  | "chatError"

interface CR {
  type: ChatResponseTag
}

export interface CRActiveUser extends CR {
  type: "activeUser"
  user: User
}

export interface CRChatStarted extends CR {
  type: "chatStarted"
}

export interface CRChatRunning extends CR {
  type: "chatRunning"
}

export interface CRChatStopped extends CR {
  type: "chatStopped"
}

export interface CRApiChats extends CR {
  type: "apiChats"
  chats: Chat[]
}

export interface CRApiChat extends CR {
  type: "apiChat"
  chat: Chat
}

export interface CRApiParsedMarkdown extends CR {
  type: "apiParsedMarkdown"
  formattedText?: FormattedText[]
}

export interface CRUserSMPServers extends CR {
  type: "userSMPServers"
  smpServers: string[]
}

export interface CRContactInfo extends CR {
  type: "contactInfo"
  contact: Contact
  connectionStats: ConnectionStats
  customUserProfile?: Profile
}

export interface CRGroupMemberInfo extends CR {
  type: "groupMemberInfo"
  groupInfo: GroupInfo
  member: GroupMember
  connectionStats_?: ConnectionStats
}

export interface CRNewChatItem extends CR {
  type: "newChatItem"
  chatItem: AChatItem
}

export interface CRChatItemStatusUpdated extends CR {
  type: "chatItemStatusUpdated"
  chatItem: AChatItem
}

export interface CRChatItemUpdated extends CR {
  type: "chatItemUpdated"
  chatItem: AChatItem
}

export interface CRChatItemDeleted extends CR {
  type: "chatItemDeleted"
  deletedChatItem: AChatItem
  toChatItem?: AChatItem
  byUser: boolean
}

export interface CRMsgIntegrityError extends CR {
  type: "msgIntegrityError"
  msgError: MsgErrorType
}

export interface CRCmdOk extends CR {
  type: "cmdOk"
}

export interface CRUserContactLink extends CR {
  type: "userContactLink"
  contactLink: UserContactLink
}

export interface CRUserContactLinkUpdated extends CR {
  type: "userContactLinkUpdated"
  connReqContact: string
  autoAccept: boolean
  autoReply?: MsgContent
}

export interface CRContactRequestRejected extends CR {
  type: "contactRequestRejected"
  contactRequest: UserContactRequest
}

export interface CRUserProfile extends CR {
  type: "userProfile"
  profile: Profile
}

export interface CRUserProfileNoChange extends CR {
  type: "userProfileNoChange"
}

export interface CRUserProfileUpdated extends CR {
  type: "userProfileUpdated"
  fromProfile: Profile
  toProfile: Profile
}

export interface CRContactAliasUpdated extends CR {
  type: "contactAliasUpdated"
  toContact: Contact
}

export interface CRInvitation extends CR {
  type: "invitation"
  connReqInvitation: string
}

export interface CRSentConfirmation extends CR {
  type: "sentConfirmation"
}

export interface CRSentInvitation extends CR {
  type: "sentInvitation"
}

export interface CRContactUpdated extends CR {
  type: "contactUpdated"
  fromContact: Contact
  toContact: Contact
}

export interface CRContactsMerged extends CR {
  type: "contactsMerged"
  intoContact: Contact
  mergedContact: Contact
}

export interface CRContactDeleted extends CR {
  type: "contactDeleted"
  contact: Contact
}

export interface CRChatCleared extends CR {
  type: "chatCleared"
  chatInfo: ChatInfo
}

export interface CRUserContactLinkCreated extends CR {
  type: "userContactLinkCreated"
  connReqContact: string
}

export interface CRUserContactLinkDeleted extends CR {
  type: "userContactLinkDeleted"
}

export interface CRReceivedContactRequest extends CR {
  type: "receivedContactRequest"
  contactRequest: UserContactRequest
}

export interface CRAcceptingContactRequest extends CR {
  type: "acceptingContactRequest"
  contact: Contact
}

export interface CRContactAlreadyExists extends CR {
  type: "contactAlreadyExists"
  contact: Contact
}

export interface CRContactRequestAlreadyAccepted extends CR {
  type: "contactRequestAlreadyAccepted"
  contact: Contact
}

export interface CRContactConnecting extends CR {
  type: "contactConnecting"
  contact: Contact
}

export interface CRContactConnected extends CR {
  type: "contactConnected"
  contact: Contact
  userCustomProfile?: Profile
}

export interface CRContactAnotherClient extends CR {
  type: "contactAnotherClient"
  contact: Contact
}

export interface CRContactDisconnected extends CR {
  type: "contactDisconnected"
  contact: Contact
}

export interface CRContactSubscribed extends CR {
  type: "contactSubscribed"
  contact: Contact
}

export interface CRContactSubError extends CR {
  type: "contactSubError"
  contact: Contact
  chatError: ChatError
}

export interface CRContactSubSummary extends CR {
  type: "contactSubSummary"
  contactSubscriptions: ContactSubStatus[]
}

export interface CRContactsDisconnected extends CR {
  type: "contactsDisconnected"
  server: string
  contactRefs: ContactRef[]
}

export interface CRContactsSubscribed extends CR {
  type: "contactsSubscribed"
  server: string
  contactRefs: ContactRef[]
}

export interface CRHostConnected extends CR {
  type: "hostConnected"
  protocol: string
  transportHost: string
}

export interface CRHostDisconnected extends CR {
  type: "hostDisconnected"
  protocol: string
  transportHost: string
}

export interface CRGroupEmpty extends CR {
  type: "groupEmpty"
  groupInfo: GroupInfo
}

export interface CRMemberSubError extends CR {
  type: "memberSubError"
  groupInfo: GroupInfo
  member: GroupMember
  chatError: ChatError
}

export interface CRMemberSubSummary extends CR {
  type: "memberSubSummary"
  memberSubscriptions: MemberSubStatus[]
}

export interface CRGroupSubscribed extends CR {
  type: "groupSubscribed"
  groupInfo: GroupInfo
}

export interface CRRcvFileAccepted extends CR {
  type: "rcvFileAccepted"
  chatItem: AChatItem
}

export interface CRRcvFileAcceptedSndCancelled extends CR {
  type: "rcvFileAcceptedSndCancelled"
  rcvFileTransfer: RcvFileTransfer
}

export interface CRRcvFileStart extends CR {
  type: "rcvFileStart"
  chatItem: AChatItem
}

export interface CRRcvFileComplete extends CR {
  type: "rcvFileComplete"
  chatItem: AChatItem
}

export interface CRRcvFileCancelled extends CR {
  type: "rcvFileCancelled"
  rcvFileTransfer: RcvFileTransfer
}

export interface CRRcvFileSndCancelled extends CR {
  type: "rcvFileSndCancelled"
  rcvFileTransfer: RcvFileTransfer
}

export interface CRSndFileStart extends CR {
  type: "sndFileStart"
  chatItem: AChatItem
  sndFileTransfer: SndFileTransfer
}

export interface CRSndFileComplete extends CR {
  type: "sndFileComplete"
  chatItem: AChatItem
  sndFileTransfer: SndFileTransfer
}

export interface CRSndFileCancelled extends CR {
  type: "sndFileCancelled"
  chatItem: AChatItem
  sndFileTransfer: SndFileTransfer
}

export interface CRSndFileRcvCancelled extends CR {
  type: "sndFileRcvCancelled"
  chatItem: AChatItem
  sndFileTransfer: SndFileTransfer
}

export interface CRSndGroupFileCancelled extends CR {
  type: "sndGroupFileCancelled"
  chatItem: AChatItem
  fileTransferMeta: FileTransferMeta
  sndFileTransfers: SndFileTransfer[]
}

export interface CRSndFileSubError extends CR {
  type: "sndFileSubError"
  sndFileTransfer: SndFileTransfer
  chatError: ChatError
}

export interface CRRcvFileSubError extends CR {
  type: "rcvFileSubError"
  rcvFileTransfer: RcvFileTransfer
  chatError: ChatError
}

export interface CRPendingSubSummary extends CR {
  type: "pendingSubSummary"
  pendingSubStatus: PendingSubStatus[]
}

export interface CRGroupCreated extends CR {
  type: "groupCreated"
  groupInfo: GroupInfo
}

export interface CRGroupMembers extends CR {
  type: "groupMembers"
  group: Group
}

export interface CRUserAcceptedGroupSent extends CR {
  type: "userAcceptedGroupSent"
  groupInfo: GroupInfo
  hostContact?: Contact // included when joining group via group link
}

export interface CRUserDeletedMember extends CR {
  type: "userDeletedMember"
  groupInfo: GroupInfo
  member: GroupMember
}

export interface CRSentGroupInvitation extends CR {
  type: "sentGroupInvitation"
  groupInfo: GroupInfo
  contact: Contact
  member: GroupMember
}

export interface CRLeftMemberUser extends CR {
  type: "leftMemberUser"
  groupInfo: GroupInfo
}

export interface CRGroupDeletedUser extends CR {
  type: "groupDeletedUser"
  groupInfo: GroupInfo
}

export interface CRGroupInvitation extends CR {
  type: "groupInvitation"
  groupInfo: GroupInfo
}

export interface CRReceivedGroupInvitation extends CR {
  type: "receivedGroupInvitation"
  groupInfo: GroupInfo
  contact: Contact
  memberRole: GroupMemberRole
}

export interface CRUserJoinedGroup extends CR {
  type: "userJoinedGroup"
  groupInfo: GroupInfo
  hostMember: GroupMember
}

export interface CRJoinedGroupMember extends CR {
  type: "joinedGroupMember"
  groupInfo: GroupInfo
  member: GroupMember
}

export interface CRJoinedGroupMemberConnecting extends CR {
  type: "joinedGroupMemberConnecting"
  groupInfo: GroupInfo
  hostMember: GroupMember
  member: GroupMember
}

export interface CRConnectedToGroupMember extends CR {
  type: "connectedToGroupMember"
  groupInfo: GroupInfo
  member: GroupMember
}

export interface CRDeletedMember extends CR {
  type: "deletedMember"
  groupInfo: GroupInfo
  byMember: GroupMember
  deletedMember: GroupMember
}

export interface CRDeletedMemberUser extends CR {
  type: "deletedMemberUser"
  groupInfo: GroupInfo
  member: GroupMember
}

export interface CRLeftMember extends CR {
  type: "leftMember"
  groupInfo: GroupInfo
  member: GroupMember
}

export interface CRGroupRemoved extends CR {
  type: "groupRemoved"
  groupInfo: GroupInfo
}

export interface CRGroupDeleted extends CR {
  type: "groupDeleted"
  groupInfo: GroupInfo
  member: GroupMember
}

export interface CRGroupUpdated extends CR {
  type: "groupUpdated"
  fromGroup: GroupInfo
  toGroup: GroupInfo
  member_?: GroupMember
}

export interface CRUserContactLinkSubscribed extends CR {
  type: "userContactLinkSubscribed"
}

export interface CRUserContactLinkSubError extends CR {
  type: "userContactLinkSubError"
  chatError: ChatError
}

export interface CRNewContactConnection extends CR {
  type: "newContactConnection"
  connection: PendingContactConnection
}

export interface CRContactConnectionDeleted extends CR {
  type: "contactConnectionDeleted"
  connection: PendingContactConnection
}

export interface CRMessageError extends CR {
  type: "messageError"
  severity: string
  errorMessage: string
}

export interface CRChatCmdError extends CR {
  type: "chatCmdError"
  chatError: ChatError
}

export interface CRChatError extends CR {
  type: "chatError"
  chatError: ChatError
}

export interface User {
  userId: number
  userContactId: number
  localDisplayName: string
  profile: Profile
  activeUser: boolean
}

export interface Chat {
  chatInfo: ChatInfo
  chatItems: [ChatItem]
  chatStats: ChatStats
}

export type ChatInfo = CInfoDirect | CInfoGroup | CInfoContactRequest

export enum ChatInfoType {
  Direct = "direct",
  Group = "group",
  ContactRequest = "contactRequest",
}

interface IChatInfo {
  type: ChatInfoType
}

interface CInfoDirect extends IChatInfo {
  type: ChatInfoType.Direct
  contact: Contact
}

interface CInfoGroup extends IChatInfo {
  type: ChatInfoType.Group
  groupInfo: GroupInfo
}

interface CInfoContactRequest extends IChatInfo {
  type: ChatInfoType.ContactRequest
  contactRequest: UserContactRequest
}

export interface Contact {
  contactId: number
  localDisplayName: string
  profile: Profile
  activeConn: Connection
  viaGroup?: number
  createdAt: Date
}

export interface ContactRef {
  contactId: number
  localDisplayName: string
}

export interface Group {
  groupInfo: GroupInfo
  members: GroupMember[]
}

export interface GroupInfo {
  groupId: number
  localDisplayName: string
  groupProfile: GroupProfile
  membership: GroupMember
  createdAt: Date
}

export interface GroupProfile {
  displayName: string
  fullName: string
  image?: string // web-compatible data/base64 string for the image
}

export interface GroupMember {
  groupMemberId: number
  memberId: string
  memberRole: GroupMemberRole
  // memberCategory: GroupMemberCategory
  // memberStatus: GroupMemberStatus
  // invitedBy: InvitedBy
  localDisplayName: string
  memberProfile: Profile
  memberContactId?: number
  activeConn?: Connection
}

export interface UserContactRequest {
  contactRequestId: number
  localDisplayName: string
  profile: Profile
  createdAt: Date
}

interface Connection {
  connId: number
}

export interface AChatItem {
  chatInfo: ChatInfo
  chatItem: ChatItem
}

export interface ChatItem {
  chatDir: CIDirection
  meta: CIMeta
  content: CIContent
  formattedText?: FormattedText[]
  quotedItem?: CIQuote
}

export type CIDirection = CIDirectSnd | CIDirectRcv | CIGroupSnd | CIGroupRcv

interface ICIDirection {
  type: "directSnd" | "directRcv" | "groupSnd" | "groupRcv"
}

interface CIDirectSnd extends ICIDirection {
  type: "directSnd"
}

interface CIDirectRcv extends ICIDirection {
  type: "directRcv"
}

interface CIGroupSnd extends ICIDirection {
  type: "groupSnd"
}

interface CIGroupRcv extends ICIDirection {
  type: "groupRcv"
  groupMember: GroupMember
}

export interface CIMeta {
  itemId: ChatItemId
  itemTs: Date
  itemText: string
  itemStatus: CIStatus
  createdAt: Date
  itemDeleted: boolean
  itemEdited: boolean
  editable: boolean
}

export type CIContent = CISndMsgContent | CIRcvMsgContent | CISndDeleted | CIRcvDeleted | CISndFileInvitation | CIRcvFileInvitation

interface ICIContent {
  type: "sndMsgContent" | "rcvMsgContent" | "sndDeleted" | "rcvDeleted" | "sndFileInvitation" | "rcvFileInvitation"
}

interface CISndMsgContent extends ICIContent {
  type: "sndMsgContent"
  msgContent: MsgContent
}

interface CIRcvMsgContent extends ICIContent {
  type: "rcvMsgContent"
  msgContent: MsgContent
}

interface CISndDeleted extends ICIContent {
  type: "sndDeleted"
  deleteMode: DeleteMode
}

interface CIRcvDeleted extends ICIContent {
  type: "rcvDeleted"
  deleteMode: DeleteMode
}

interface CISndFileInvitation extends ICIContent {
  type: "sndFileInvitation"
  fileId: number
  filePath: string
}

interface CIRcvFileInvitation extends ICIContent {
  type: "rcvFileInvitation"
  rcvFileTransfer: RcvFileTransfer
}

export function ciContentText(content: CIContent): string | undefined {
  switch (content.type) {
    case "sndMsgContent":
      return content.msgContent.text
    case "rcvMsgContent":
      return content.msgContent.text
    default:
      return undefined
  }
}

interface RcvFileTransfer {
  fileId: number
  // fileInvitation: FileInvitation
  // fileStatus: RcvFileStatus
  senderDisplayName: string
  chunkSize: number
  cancelled: boolean
  grpMemberId?: number
}

interface SndFileTransfer {
  fileId: number
  fileName: string
  filePath: string
  fileSize: number
  chunkSize: number
  recipientDisplayName: string
  connId: number
  // agentConnId: string
  // fileStatus: FileStatus
}

interface FileTransferMeta {
  fileId: number
  fileName: string
  filePath: string
  fileSize: number
  chunkSize: number
  cancelled: boolean
}

interface UserContactLink {
  connReqContact: string
  autoAccept?: AutoAccept
}

interface AutoAccept {
  acceptIncognito: boolean
  autoReply?: MsgContent
}

export interface ChatStats {
  unreadCount: number
  minUnreadItemId: number
}

export interface CIQuote {
  chatDir?: CIDirection
  itemId?: number
  sharedMsgId?: string
  sentAt: Date
  content: MsgContent
  formattedText?: FormattedText[]
}

export type CIStatus = CISndNew | CISndSent | CISndErrorAuth | CISndError | CIRcvNew | CIRcvRead

interface ICIStatus {
  type: "sndNew" | "sndSent" | "sndErrorAuth" | "sndError" | "rcvNew" | "rcvRead"
}

interface CISndNew extends ICIStatus {
  type: "sndNew"
}

interface CISndSent extends ICIStatus {
  type: "sndSent"
}

interface CISndErrorAuth extends ICIStatus {
  type: "sndErrorAuth"
}

interface CISndError extends ICIStatus {
  type: "sndError"
  agentError: AgentErrorType
}

interface CIRcvNew extends ICIStatus {
  type: "rcvNew"
}

interface CIRcvRead extends ICIStatus {
  type: "rcvRead"
}

interface FormattedText {}

interface MsgErrorType {}

export type ChatError = ChatErrorChat | ChatErrorAgent | ChatErrorStore

interface ChatErrorChat {
  type: "error"
  errorType: ChatErrorType
}

interface ChatErrorAgent {
  type: "errorAgent"
  agentError: AgentErrorType
}

interface ChatErrorStore {
  type: "errorStore"
  storeError: StoreErrorType
}

type ChatErrorType = CENoActiveUser | CEActiveUserExists

interface CENoActiveUser {
  type: "noActiveUser"
}

interface CEActiveUserExists {
  type: "activeUserExists"
}

interface ContactSubStatus {}

interface PendingSubStatus {}

export interface ConnectionStats {
  rcvServers?: string[]
  sndServers?: string[]
}

interface PendingContactConnection {}

interface MemberSubStatus {
  member: GroupMember
  memberError?: ChatError
}

interface AgentErrorType {
  type: string
  [x: string]: any
}

interface StoreErrorType {
  type: string
  [x: string]: any
}
