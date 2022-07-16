import {ChatItemId, MsgContent, DeleteMode, Profile} from "./command"

export type ChatResponse =
  | CRActiveUser
  | CRChatStarted
  | CRChatRunning
  | CRApiChats
  | CRApiChat
  | CRApiParsedMarkdown
  | CRUserSMPServers
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
  | CRInvitation
  | CRSentConfirmation
  | CRSentInvitation
  | CRContactUpdated
  | CRContactDeleted
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
  | CRContactSubSummary // TODO remove
  | CRGroupEmpty
  | CRPendingSubSummary
  | CRUserContactLinkSubscribed
  | CRUserContactLinkSubError
  | CRMessageError
  | CRChatCmdError
  | CRChatError

type ChatResponseTag =
  | "activeUser"
  | "chatStarted"
  | "chatRunning"
  | "apiChats"
  | "apiChat"
  | "apiParsedMarkdown"
  | "userSMPServers"
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
  | "invitation"
  | "sentConfirmation"
  | "sentInvitation"
  | "contactUpdated"
  | "contactDeleted"
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
  | "groupEmpty"
  | "pendingSubSummary"
  | "userContactLinkSubscribed"
  | "userContactLinkSubError"
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
  toChatItem: AChatItem
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
  connReqContact: string
  autoAccept: boolean
}

export interface CRUserContactLinkUpdated extends CR {
  type: "userContactLinkUpdated"
  connReqContact: string
  autoAccept: boolean
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

export interface CRContactDeleted extends CR {
  type: "contactDeleted"
  contact: Contact
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

// TODO remove
export interface CRContactSubSummary extends CR {
  type: "contactSubSummary"
  contactSubscriptions: ContactSubStatus[]
}

export interface CRGroupEmpty extends CR {
  type: "groupEmpty"
  groupInfo: GroupInfo
}

export interface CRPendingSubSummary extends CR {
  type: "pendingSubSummary"
  pendingSubStatus: PendingSubStatus[]
}

export interface CRUserContactLinkSubscribed extends CR {
  type: "userContactLinkSubscribed"
}

export interface CRUserContactLinkSubError extends CR {
  type: "userContactLinkSubError"
  chatError: ChatError
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
  // memberRole: GroupMemberRole
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

interface RcvFileTransfer {}

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

interface AgentErrorType {
  type: string
  [x: string]: any
}

interface StoreErrorType {
  type: string
  [x: string]: any
}
