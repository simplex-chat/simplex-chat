import {ChatItemId, MsgContent, DeleteMode, Profile, GroupMemberRole, LocalProfile, ServerProtocol, ServerCfg} from "./command"

export type ChatResponse =
  | CRActiveUser
  | CRUsersList
  | CRChatStarted
  | CRChatRunning
  | CRChatStopped
  | CRApiChats
  | CRApiChat
  | CRApiParsedMarkdown
  | CRUserProtoServers
  | CRContactInfo
  | CRGroupMemberInfo
  | CRNewChatItems
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
  | CRUserContactLinkSubError
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
  | "usersList"
  | "chatStarted"
  | "chatRunning"
  | "chatStopped"
  | "apiChats"
  | "apiChat"
  | "apiParsedMarkdown"
  | "userProtoServers"
  | "contactInfo"
  | "groupMemberInfo"
  | "newChatItems"
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

export interface CRUsersList extends CR {
  type: "usersList"
  users: UserInfo[]
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
  user: User
  chats: Chat[]
}

export interface CRApiChat extends CR {
  type: "apiChat"
  user: User
  chat: Chat
}

export interface CRApiParsedMarkdown extends CR {
  type: "apiParsedMarkdown"
  formattedText?: FormattedText[]
}

export interface CRUserProtoServers extends CR {
  type: "userProtoServers"
  user: User
  servers: UserProtoServers
}

export interface CRContactInfo extends CR {
  type: "contactInfo"
  user: User
  contact: Contact
  connectionStats: ConnectionStats
  customUserProfile?: Profile
}

export interface CRGroupMemberInfo extends CR {
  type: "groupMemberInfo"
  user: User
  groupInfo: GroupInfo
  member: GroupMember
  connectionStats_?: ConnectionStats
}

export interface CRNewChatItems extends CR {
  type: "newChatItems"
  user: User
  chatItems: AChatItem[]
}

export interface CRChatItemStatusUpdated extends CR {
  type: "chatItemStatusUpdated"
  user: User
  chatItem: AChatItem
}

export interface CRChatItemUpdated extends CR {
  type: "chatItemUpdated"
  user: User
  chatItem: AChatItem
}

export interface CRChatItemDeleted extends CR {
  type: "chatItemDeleted"
  user: User
  deletedChatItem: AChatItem
  toChatItem?: AChatItem
  byUser: boolean
}

export interface CRMsgIntegrityError extends CR {
  type: "msgIntegrityError"
  user: User
  msgError: MsgErrorType
}

export interface CRCmdOk extends CR {
  type: "cmdOk"
  user_?: User
}

export interface CRUserContactLink extends CR {
  type: "userContactLink"
  user: User
  contactLink: UserContactLink
}

export interface CRUserContactLinkUpdated extends CR {
  type: "userContactLinkUpdated"
  user: User
  connReqContact: string
  autoAccept: boolean
  autoReply?: MsgContent
}

export interface CRContactRequestRejected extends CR {
  type: "contactRequestRejected"
  user: User
  contactRequest: UserContactRequest
}

export interface CRUserProfile extends CR {
  type: "userProfile"
  user: User
  profile: Profile
}

export interface CRUserProfileNoChange extends CR {
  type: "userProfileNoChange"
  user: User
}

export interface CRUserProfileUpdated extends CR {
  type: "userProfileUpdated"
  user: User
  fromProfile: Profile
  toProfile: Profile
}

export interface CRContactAliasUpdated extends CR {
  type: "contactAliasUpdated"
  user: User
  toContact: Contact
}

export interface CRInvitation extends CR {
  type: "invitation"
  user: User
  connReqInvitation: string
}

export interface CRSentConfirmation extends CR {
  type: "sentConfirmation"
  user: User
}

export interface CRSentInvitation extends CR {
  type: "sentInvitation"
  user: User
}

export interface CRContactUpdated extends CR {
  type: "contactUpdated"
  user: User
  fromContact: Contact
  toContact: Contact
}

export interface CRContactsMerged extends CR {
  type: "contactsMerged"
  user: User
  intoContact: Contact
  mergedContact: Contact
}

export interface CRContactDeleted extends CR {
  type: "contactDeleted"
  user: User
  contact: Contact
}

export interface CRChatCleared extends CR {
  type: "chatCleared"
  user: User
  chatInfo: ChatInfo
}

export interface CRUserContactLinkCreated extends CR {
  type: "userContactLinkCreated"
  user: User
  connReqContact: string
}

export interface CRUserContactLinkDeleted extends CR {
  type: "userContactLinkDeleted"
  user: User
}

export interface CRReceivedContactRequest extends CR {
  type: "receivedContactRequest"
  user: User
  contactRequest: UserContactRequest
}

export interface CRAcceptingContactRequest extends CR {
  type: "acceptingContactRequest"
  user: User
  contact: Contact
}

export interface CRContactAlreadyExists extends CR {
  type: "contactAlreadyExists"
  user: User
  contact: Contact
}

export interface CRContactRequestAlreadyAccepted extends CR {
  type: "contactRequestAlreadyAccepted"
  user: User
  contact: Contact
}

export interface CRContactConnecting extends CR {
  type: "contactConnecting"
  user: User
  contact: Contact
}

export interface CRContactConnected extends CR {
  type: "contactConnected"
  contact: Contact
  user: User
  userCustomProfile?: Profile
}

export interface CRContactAnotherClient extends CR {
  type: "contactAnotherClient"
  user: User
  contact: Contact
}

export interface CRContactSubError extends CR {
  type: "contactSubError"
  user: User
  contact: Contact
  chatError: ChatError
}

export interface CRContactSubSummary extends CR {
  type: "contactSubSummary"
  user: User
  contactSubscriptions: ContactSubStatus[]
}

export interface CRContactsDisconnected extends CR {
  type: "contactsDisconnected"
  user: User
  server: string
  contactRefs: ContactRef[]
}

export interface CRContactsSubscribed extends CR {
  type: "contactsSubscribed"
  user: User
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
  user: User
  groupInfo: GroupInfo
}

export interface CRMemberSubError extends CR {
  type: "memberSubError"
  user: User
  groupInfo: GroupInfo
  member: GroupMember
  chatError: ChatError
}

export interface CRMemberSubSummary extends CR {
  type: "memberSubSummary"
  user: User
  memberSubscriptions: MemberSubStatus[]
}

export interface CRGroupSubscribed extends CR {
  type: "groupSubscribed"
  user: User
  groupInfo: GroupInfo
}

export interface CRRcvFileAccepted extends CR {
  type: "rcvFileAccepted"
  user: User
  chatItem: AChatItem
}

export interface CRRcvFileAcceptedSndCancelled extends CR {
  type: "rcvFileAcceptedSndCancelled"
  user: User
  rcvFileTransfer: RcvFileTransfer
}

export interface CRRcvFileStart extends CR {
  type: "rcvFileStart"
  user: User
  chatItem: AChatItem
}

export interface CRRcvFileComplete extends CR {
  type: "rcvFileComplete"
  user: User
  chatItem: AChatItem
}

export interface CRRcvFileCancelled extends CR {
  type: "rcvFileCancelled"
  user: User
  rcvFileTransfer: RcvFileTransfer
}

export interface CRRcvFileSndCancelled extends CR {
  type: "rcvFileSndCancelled"
  user: User
  rcvFileTransfer: RcvFileTransfer
}

export interface CRSndFileStart extends CR {
  type: "sndFileStart"
  user: User
  chatItem: AChatItem
  sndFileTransfer: SndFileTransfer
}

export interface CRSndFileComplete extends CR {
  type: "sndFileComplete"
  user: User
  chatItem: AChatItem
  sndFileTransfer: SndFileTransfer
}

export interface CRSndFileCancelled extends CR {
  type: "sndFileCancelled"
  user: User
  chatItem: AChatItem
  sndFileTransfer: SndFileTransfer
}

export interface CRSndFileRcvCancelled extends CR {
  type: "sndFileRcvCancelled"
  user: User
  chatItem: AChatItem
  sndFileTransfer: SndFileTransfer
}

export interface CRSndGroupFileCancelled extends CR {
  type: "sndGroupFileCancelled"
  user: User
  chatItem: AChatItem
  fileTransferMeta: FileTransferMeta
  sndFileTransfers: SndFileTransfer[]
}

export interface CRSndFileSubError extends CR {
  type: "sndFileSubError"
  user: User
  sndFileTransfer: SndFileTransfer
  chatError: ChatError
}

export interface CRRcvFileSubError extends CR {
  type: "rcvFileSubError"
  user: User
  rcvFileTransfer: RcvFileTransfer
  chatError: ChatError
}

export interface CRPendingSubSummary extends CR {
  type: "pendingSubSummary"
  user: User
  pendingSubStatus: PendingSubStatus[]
}

export interface CRGroupCreated extends CR {
  type: "groupCreated"
  user: User
  groupInfo: GroupInfo
}

export interface CRGroupMembers extends CR {
  type: "groupMembers"
  user: User
  group: Group
}

export interface CRUserAcceptedGroupSent extends CR {
  type: "userAcceptedGroupSent"
  user: User
  groupInfo: GroupInfo
  hostContact?: Contact // included when joining group via group link
}

export interface CRUserDeletedMember extends CR {
  type: "userDeletedMember"
  user: User
  groupInfo: GroupInfo
  member: GroupMember
}

export interface CRSentGroupInvitation extends CR {
  type: "sentGroupInvitation"
  user: User
  groupInfo: GroupInfo
  contact: Contact
  member: GroupMember
}

export interface CRLeftMemberUser extends CR {
  type: "leftMemberUser"
  user: User
  groupInfo: GroupInfo
}

export interface CRGroupDeletedUser extends CR {
  type: "groupDeletedUser"
  user: User
  groupInfo: GroupInfo
}

export interface CRGroupInvitation extends CR {
  type: "groupInvitation"
  user: User
  groupInfo: GroupInfo
}

export interface CRReceivedGroupInvitation extends CR {
  type: "receivedGroupInvitation"
  user: User
  groupInfo: GroupInfo
  contact: Contact
  memberRole: GroupMemberRole
}

export interface CRUserJoinedGroup extends CR {
  type: "userJoinedGroup"
  user: User
  groupInfo: GroupInfo
  hostMember: GroupMember
}

export interface CRJoinedGroupMember extends CR {
  type: "joinedGroupMember"
  user: User
  groupInfo: GroupInfo
  member: GroupMember
}

export interface CRJoinedGroupMemberConnecting extends CR {
  type: "joinedGroupMemberConnecting"
  user: User
  groupInfo: GroupInfo
  hostMember: GroupMember
  member: GroupMember
}

export interface CRConnectedToGroupMember extends CR {
  type: "connectedToGroupMember"
  user: User
  groupInfo: GroupInfo
  member: GroupMember
}

export interface CRDeletedMember extends CR {
  type: "deletedMember"
  user: User
  groupInfo: GroupInfo
  byMember: GroupMember
  deletedMember: GroupMember
}

export interface CRDeletedMemberUser extends CR {
  type: "deletedMemberUser"
  user: User
  groupInfo: GroupInfo
  member: GroupMember
}

export interface CRLeftMember extends CR {
  type: "leftMember"
  user: User
  groupInfo: GroupInfo
  member: GroupMember
}

export interface CRGroupRemoved extends CR {
  type: "groupRemoved"
  user: User
  groupInfo: GroupInfo
}

export interface CRGroupDeleted extends CR {
  type: "groupDeleted"
  user: User
  groupInfo: GroupInfo
  member: GroupMember
}

export interface CRGroupUpdated extends CR {
  type: "groupUpdated"
  user: User
  fromGroup: GroupInfo
  toGroup: GroupInfo
  member_?: GroupMember
}

export interface CRUserContactLinkSubError extends CR {
  type: "userContactLinkSubError"
  chatError: ChatError
}

export interface CRContactConnectionDeleted extends CR {
  type: "contactConnectionDeleted"
  user: User
  connection: PendingContactConnection
}

export interface CRMessageError extends CR {
  type: "messageError"
  user: User
  severity: string
  errorMessage: string
}

export interface CRChatCmdError extends CR {
  type: "chatCmdError"
  user_?: User
  chatError: ChatError
}

export interface CRChatError extends CR {
  type: "chatError"
  user_?: User
  chatError: ChatError
}

export interface User {
  userId: number
  agentUserId: string
  userContactId: number
  localDisplayName: string
  profile: LocalProfile
  // fullPreferences :: FullPreferences
  activeUser: boolean
  viewPwdHash: string
  showNtfs: boolean
}

export interface UserProtoServers {
  serverProtocol: ServerProtocol
  protoServers: ServerCfg[]
  presetServers: string
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

export interface UserPwdHash {
  hash: string
  salt: string
}

interface UserInfo {
  user: User
  unreadCount: number
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
