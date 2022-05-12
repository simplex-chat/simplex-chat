export type ChatCommand =
  | ShowActiveUser
  | CreateActiveUser
  | StartChat
  | SetFilesFolder
  | APIGetChats
  | APIGetChat
  | APISendMessage
  | APIUpdateChatItem
  | APIDeleteChatItem
  | APIChatRead
  | APIDeleteChat
  | APIAcceptContact
  | APIRejectContact
  | APIUpdateProfile
  | APIParseMarkdown
  | GetUserSMPServers
  | SetUserSMPServers
  | AddContact
  | Connect
  | ConnectSimplex
  | CreateMyAddress
  | DeleteMyAddress
  | ShowMyAddress
  | AddressAutoAccept

type ChatCommandTag =
  | "showActiveUser"
  | "createActiveUser"
  | "startChat"
  | "setFilesFolder"
  | "apiGetChats"
  | "apiGetChat"
  | "apiSendMessage"
  | "apiUpdateChatItem"
  | "apiDeleteChatItem"
  | "apiChatRead"
  | "apiDeleteChat"
  | "apiAcceptContact"
  | "apiRejectContact"
  | "apiUpdateProfile"
  | "apiParseMarkdown"
  | "getUserSMPServers"
  | "setUserSMPServers"
  | "addContact"
  | "connect"
  | "connectSimplex"
  | "createMyAddress"
  | "deleteMyAddress"
  | "showMyAddress"
  | "addressAutoAccept"

interface IChatCommand {
  type: ChatCommandTag
}

export interface ShowActiveUser extends IChatCommand {
  type: "showActiveUser"
}

export interface CreateActiveUser extends IChatCommand {
  type: "createActiveUser"
  profile: Profile
}

export interface StartChat extends IChatCommand {
  type: "startChat"
}

export interface SetFilesFolder extends IChatCommand {
  type: "setFilesFolder"
  filePath: string
}

export interface APIGetChats extends IChatCommand {
  type: "apiGetChats"
}

export interface APIGetChat extends IChatCommand {
  type: "apiGetChat"
  chatType: ChatType
  chatId: number
  pagination: ChatPagination
}

export interface APISendMessage extends IChatCommand {
  type: "apiSendMessage"
  chatType: ChatType
  chatId: number
  filePath?: number
  quotedItem?: ChatItemId
  msgContent: MsgContent
}

export interface APIUpdateChatItem extends IChatCommand {
  type: "apiUpdateChatItem"
  chatType: ChatType
  chatId: number
  chatItemId: ChatItemId
  msgContent: MsgContent
}

export interface APIDeleteChatItem extends IChatCommand {
  type: "apiDeleteChatItem"
  chatType: ChatType
  chatId: number
  chatItemId: ChatItemId
  deleteMode: DeleteMode
}

export interface APIChatRead extends IChatCommand {
  type: "apiChatRead"
  chatType: ChatType
  chatId: number
  fromItem: ChatItemId
  toItem: ChatItemId
}

export interface APIDeleteChat extends IChatCommand {
  type: "apiDeleteChat"
  chatType: ChatType
  chatId: number
}

export interface APIAcceptContact extends IChatCommand {
  type: "apiAcceptContact"
  contactReqId: number
}

export interface APIRejectContact extends IChatCommand {
  type: "apiRejectContact"
  contactReqId: number
}

export interface APIUpdateProfile extends IChatCommand {
  type: "apiUpdateProfile"
  profile: Profile
}

export interface APIParseMarkdown extends IChatCommand {
  type: "apiParseMarkdown"
  text: string
}

export interface GetUserSMPServers extends IChatCommand {
  type: "getUserSMPServers"
}

export interface SetUserSMPServers extends IChatCommand {
  type: "setUserSMPServers"
  servers: [string]
}

export interface AddContact extends IChatCommand {
  type: "addContact"
}

export interface Connect extends IChatCommand {
  type: "connect"
  connReq: string
}

export interface ConnectSimplex extends IChatCommand {
  type: "connectSimplex"
}

export interface CreateMyAddress extends IChatCommand {
  type: "createMyAddress"
}

export interface DeleteMyAddress extends IChatCommand {
  type: "deleteMyAddress"
}

export interface ShowMyAddress extends IChatCommand {
  type: "showMyAddress"
}

export interface AddressAutoAccept extends IChatCommand {
  type: "addressAutoAccept"
  enable: boolean
}

export interface Profile {
  displayName: string
  fullName: string // can be empty string
  image?: string
}

enum ChatType {
  CTDirect = "@",
  CTGroup = "#",
  CTContactRequest = "<@",
}

type ChatPagination =
  | {count: number} // count from the last item in case neither after nor before specified
  | {count: number; after: ChatItemId}
  | {count: number; before: ChatItemId}

type ChatItemId = number

type MsgContentTag = "text" | "link" | "images"

export type MsgContent = MCText | MCUnknown

interface MC {
  type: MsgContentTag
}

interface MCText extends MC {
  type: "text"
  text: string
}

interface MCUnknown {
  type: string
  text?: string
}

enum DeleteMode {
  DMBroadcast = "broadcast",
  DMInternal = "internal",
}

export function cmdString(cmd: ChatCommand): string {
  switch (cmd.type) {
    case "showActiveUser":
      return "/u"
    case "createActiveUser":
      return `/u ${JSON.stringify(cmd.profile)}`
    case "startChat":
      return "/_start"
    case "setFilesFolder":
      return `/_files_folder ${cmd.filePath}`
    case "apiGetChats":
      return "/_get chats"
    case "apiGetChat":
      return `/_get chat ${cmd.chatType}${cmd.chatId}${paginationStr(cmd.pagination)}`
    case "apiSendMessage":
      return `/_send ${cmd.chatType}${cmd.chatId}${tagged("file", cmd.filePath)}${tagged("quoted", cmd.quotedItem)} json ${JSON.stringify(
        cmd.msgContent
      )}`
    case "apiUpdateChatItem":
      return `/_update item ${cmd.chatType}${cmd.chatId} ${cmd.chatItemId} json ${JSON.stringify(cmd.msgContent)}`
    case "apiDeleteChatItem":
      return `/_delete item ${cmd.chatType}${cmd.chatId} ${cmd.chatItemId} ${cmd.deleteMode}`
    case "apiChatRead":
      return `/_read chat ${cmd.chatType}${cmd.chatId} from=${cmd.fromItem} to=${cmd.toItem}`
    case "apiDeleteChat":
      return `/_delete ${cmd.chatType}${cmd.chatId}`
    case "apiAcceptContact":
      return `/_accept ${cmd.contactReqId}`
    case "apiRejectContact":
      return `/_reject ${cmd.contactReqId}`
    case "apiUpdateProfile":
      return `/_profile ${JSON.stringify(cmd.profile)}`
    case "apiParseMarkdown":
      return `/_parse ${cmd.text}`
    case "getUserSMPServers":
      return "/smp_servers"
    case "setUserSMPServers":
      return `/smp_servers ${cmd.servers.join(",") || "default"}`
    case "addContact":
      return "/connect"
    case "connect":
      return `/connect ${cmd.connReq}`
    case "connectSimplex":
      return "/simplex"
    case "createMyAddress":
      return "/address"
    case "deleteMyAddress":
      return "/delete_address"
    case "showMyAddress":
      return "/show_address"
    case "addressAutoAccept":
      return `/auto_accept ${cmd.enable ? "on" : "off"}`
  }
}

function paginationStr(cp: ChatPagination): string {
  const base = "after" in cp ? ` after=${cp.after}` : "before" in cp ? ` before=${cp.before}` : ""
  return base + ` count=${cp.count}`
}

function tagged<T>(tag: string, x?: T): string {
  return x ? ` ${tag}=${x}` : ""
}

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
  | CRContactSubSummary
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
  chats: Chat
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

interface Chat {
  chatInfo: ChatInfo
  chatItems: [ChatItem]
  chatStats: ChatStats
}

type ChatInfo = CInfoDirect | CInfoGroup | CInfoContactRequest

interface IChatInfo {
  type: ChatType
}

interface CInfoDirect extends IChatInfo {
  type: ChatType.CTDirect
  contact: Contact
}

interface CInfoGroup extends IChatInfo {
  type: ChatType.CTGroup
  groupInfo: GroupInfo
}

interface CInfoContactRequest extends IChatInfo {
  type: ChatType.CTContactRequest
  contactRequest: UserContactRequest
}

interface Contact {
  contactId: number
  localDisplayName: string
  profile: Profile
  activeConn: Connection
  viaGroup?: number
  createdAt: Date
}

interface GroupInfo {
  groupId: number
  localDisplayName: string
  groupProfile: GroupProfile
  membership: GroupMember
  createdAt: Date
}

interface GroupProfile {
  displayName: string
  fullName: string
  image?: string // web-compatible data/base64 string for the image
}

interface GroupMember {
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

interface UserContactRequest {
  contactRequestId: number
  localDisplayName: string
  profile: Profile
  createdAt: Date
}

interface Connection {
  connId: number
}

interface AChatItem {
  chatInfo: ChatInfo
  chatItem: ChatItem
}

interface ChatItem {
  chatDir: CIDirection
  meta: CIMeta
  content: CIContent
  formattedText?: FormattedText[]
  quotedItem?: CIQuote
}

type CIDirection = CIDirectSnd | CIDirectRcv | CIGroupSnd | CIGroupRcv

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

interface CIMeta {
  itemId: number
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
  deleteMode: CIDeleteMode
}

interface CIRcvDeleted extends ICIContent {
  type: "rcvDeleted"
  deleteMode: CIDeleteMode
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

enum CIDeleteMode {
  Broadcast = "broadcast",
  Internal = "internal",
}

interface RcvFileTransfer {}

interface ChatStats {
  unreadCount: number
  minUnreadItemId: number
}

interface CIQuote {
  chatDir?: CIDirection
  itemId?: number
  sharedMsgId?: string
  sentAt: Date
  content: MsgContent
  formattedText?: FormattedText[]
}

type CIStatus = CISndNew | CISndSent | CISndErrorAuth | CISndError | CIRcvNew | CIRcvRead

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

type ChatError = ChatErrorChat | ChatErrorAgent | ChatErrorStore

interface ChatErrorChat {
  type: "error"
  errorType: ChatErrorType
}

interface ChatErrorAgent {
  type: "errorAgent"
}

interface ChatErrorStore {
  type: "errorStore"
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

interface AgentErrorType {}
