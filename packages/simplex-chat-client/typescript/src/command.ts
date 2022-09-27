export type ChatCommand =
  | ShowActiveUser
  | CreateActiveUser
  | StartChat
  | APIStopChat
  | SetFilesFolder
  | SetIncognito
  | APIExportArchive
  | APIImportArchive
  | APIDeleteStorage
  | APIGetChats
  | APIGetChat
  | APISendMessage
  | APIUpdateChatItem
  | APIDeleteChatItem
  | APIChatRead
  | APIDeleteChat
  | APIClearChat
  | APIAcceptContact
  | APIRejectContact
  | APIUpdateProfile
  | APISetContactAlias
  | APIParseMarkdown
  | NewGroup
  | APIAddMember
  | APIJoinGroup
  | APIRemoveMember
  | APILeaveGroup
  | APIListMembers
  | APIUpdateGroupProfile
  | GetUserSMPServers
  | SetUserSMPServers
  | APIContactInfo
  | APIGroupMemberInfo
  | AddContact
  | Connect
  | ConnectSimplex
  | CreateMyAddress
  | DeleteMyAddress
  | ShowMyAddress
  | AddressAutoAccept
  | ReceiveFile
  | CancelFile
  | FileStatus

// not included commands (they are not needed for Websocket clients, and can still be sent as strings):
// APIActivateChat
// APISuspendChat
// ResubscribeAllConnections
// APIGetChatItems - not implemented
// APISendCallInvitation
// APIRejectCall
// APISendCallOffer
// APISendCallAnswer
// APISendCallExtraInfo
// APIEndCall
// APIGetCallInvitations
// APICallStatus
// APIGetNtfToken
// APIRegisterToken
// APIVerifyToken
// APIDeleteToken
// APIGetNtfMessage
// APIMemberRole -- not implemented
// ListContacts
// ListGroups
// APISetNetworkConfig
// APIGetNetworkConfig
// APISetChatSettings
// ShowMessages
// LastMessages
// SendMessageBroadcast

type ChatCommandTag =
  | "showActiveUser"
  | "createActiveUser"
  | "startChat"
  | "apiStopChat"
  | "setFilesFolder"
  | "setIncognito"
  | "apiExportArchive"
  | "apiImportArchive"
  | "apiDeleteStorage"
  | "apiGetChats"
  | "apiGetChat"
  | "apiSendMessage"
  | "apiUpdateChatItem"
  | "apiDeleteChatItem"
  | "apiChatRead"
  | "apiDeleteChat"
  | "apiClearChat"
  | "apiAcceptContact"
  | "apiRejectContact"
  | "apiUpdateProfile"
  | "apiSetContactAlias"
  | "apiParseMarkdown"
  | "newGroup"
  | "apiAddMember"
  | "apiJoinGroup"
  | "apiRemoveMember"
  | "apiLeaveGroup"
  | "apiListMembers"
  | "apiUpdateGroupProfile"
  | "getUserSMPServers"
  | "setUserSMPServers"
  | "apiContactInfo"
  | "apiGroupMemberInfo"
  | "addContact"
  | "connect"
  | "connectSimplex"
  | "createMyAddress"
  | "deleteMyAddress"
  | "showMyAddress"
  | "addressAutoAccept"
  | "receiveFile"
  | "cancelFile"
  | "fileStatus"

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
  subscribeConnections?: boolean
  expireChatItems?: boolean
}

export interface APIStopChat extends IChatCommand {
  type: "apiStopChat"
}

export interface SetFilesFolder extends IChatCommand {
  type: "setFilesFolder"
  filePath: string
}

export interface SetIncognito extends IChatCommand {
  type: "setIncognito"
  incognito: boolean
}

export interface APIExportArchive extends IChatCommand {
  type: "apiExportArchive"
  config: ArchiveConfig
}

export interface APIImportArchive extends IChatCommand {
  type: "apiImportArchive"
  config: ArchiveConfig
}

export interface APIDeleteStorage extends IChatCommand {
  type: "apiDeleteStorage"
}

export interface APIGetChats extends IChatCommand {
  type: "apiGetChats"
  pendingConnections?: boolean
}

export interface APIGetChat extends IChatCommand {
  type: "apiGetChat"
  chatType: ChatType
  chatId: number
  pagination: ChatPagination
  search?: string
}

export interface APISendMessage extends IChatCommand {
  type: "apiSendMessage"
  chatType: ChatType
  chatId: number
  message: ComposedMessage
}

export interface ComposedMessage {
  filePath?: string
  quotedItemId?: ChatItemId
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
  itemRange?: ItemRange
}

export interface ItemRange {
  fromItem: ChatItemId
  toItem: ChatItemId
}

export interface APIDeleteChat extends IChatCommand {
  type: "apiDeleteChat"
  chatType: ChatType
  chatId: number
}

export interface APIClearChat extends IChatCommand {
  type: "apiClearChat"
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

export interface APISetContactAlias extends IChatCommand {
  type: "apiSetContactAlias"
  contactId: number
  localAlias: string
}

export interface APIParseMarkdown extends IChatCommand {
  type: "apiParseMarkdown"
  text: string
}

export interface NewGroup extends IChatCommand {
  type: "newGroup"
  groupProfile: GroupProfile
}

export interface APIAddMember extends IChatCommand {
  type: "apiAddMember"
  groupId: number
  contactId: number
  memberRole: GroupMemberRole
}

export interface APIJoinGroup extends IChatCommand {
  type: "apiJoinGroup"
  groupId: number
}

export interface APIRemoveMember extends IChatCommand {
  type: "apiRemoveMember"
  groupId: number
  memberId: number
}

export interface APILeaveGroup extends IChatCommand {
  type: "apiLeaveGroup"
  groupId: number
}

export interface APIListMembers extends IChatCommand {
  type: "apiListMembers"
  groupId: number
}

export interface APIUpdateGroupProfile extends IChatCommand {
  type: "apiUpdateGroupProfile"
  groupId: number
  groupProfile: GroupProfile
}

export interface GetUserSMPServers extends IChatCommand {
  type: "getUserSMPServers"
}

export interface SetUserSMPServers extends IChatCommand {
  type: "setUserSMPServers"
  servers: [string]
}

export interface APIContactInfo extends IChatCommand {
  type: "apiContactInfo"
  contactId: number
}

export interface APIGroupMemberInfo extends IChatCommand {
  type: "apiGroupMemberInfo"
  groupId: number
  memberId: number
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
  autoAccept: boolean
  autoReply?: MsgContent
}

export interface ReceiveFile extends IChatCommand {
  type: "receiveFile"
  fileId: number
  filePath?: string
}

export interface CancelFile extends IChatCommand {
  type: "cancelFile"
  fileId: number
}

export interface FileStatus extends IChatCommand {
  type: "fileStatus"
  fileId: number
}

export interface Profile {
  displayName: string
  fullName: string // can be empty string
  image?: string
}

export enum ChatType {
  Direct = "@",
  Group = "#",
  ContactRequest = "<@",
}

export type ChatPagination =
  | {count: number} // count from the last item in case neither after nor before specified
  | {count: number; after: ChatItemId}
  | {count: number; before: ChatItemId}

export type ChatItemId = number

type MsgContentTag = "text" | "link" | "image" | "file"

export type MsgContent = MCText | MCLink | MCImage | MCFile | MCUnknown

interface MC {
  type: MsgContentTag
  text: string
}

interface MCText extends MC {
  type: "text"
  text: string
}

interface MCLink extends MC {
  type: "link"
  text: string
  preview: LinkPreview
}

interface MCImage extends MC {
  type: "image"
  image: string // image preview as base64 encoded data string
}

interface MCFile extends MC {
  type: "file"
  text: string
}

interface MCUnknown {
  type: string
  text: string
}

interface LinkPreview {
  uri: string
  title: string
  description: string
  image: string
}

export enum DeleteMode {
  Broadcast = "broadcast",
  Internal = "internal",
}

interface ArchiveConfig {
  archivePath: string
  disableCompression?: boolean
  parentTempDirectory?: string
}

export enum GroupMemberRole {
  GRMember = "member",
  GRAdmin = "admin",
  GROwner = "owner",
}

interface GroupProfile {
  displayName: string
  fullName: string // can be empty string
  image?: string
}

export function cmdString(cmd: ChatCommand): string {
  switch (cmd.type) {
    case "showActiveUser":
      return "/u"
    case "createActiveUser":
      return `/u ${JSON.stringify(cmd.profile)}`
    case "startChat":
      return `/_start subscribe=${cmd.subscribeConnections ? "on" : "off"} expire=${cmd.expireChatItems ? "on" : "off"}`
    case "apiStopChat":
      return "/_stop"
    case "setFilesFolder":
      return `/_files_folder ${cmd.filePath}`
    case "setIncognito":
      return `/incognito ${cmd.incognito ? "on" : "off"}`
    case "apiExportArchive":
      return `/_db export ${JSON.stringify(cmd.config)}`
    case "apiImportArchive":
      return `/_db import ${JSON.stringify(cmd.config)}`
    case "apiDeleteStorage":
      return "/_db delete"
    case "apiGetChats":
      return `/_get chats pcc=${cmd.pendingConnections ? "on" : "off"}`
    case "apiGetChat":
      return `/_get chat ${cmd.chatType}${cmd.chatId}${paginationStr(cmd.pagination)}`
    case "apiSendMessage":
      return `/_send ${cmd.chatType}${cmd.chatId} json ${JSON.stringify(cmd.message)}`
    case "apiUpdateChatItem":
      return `/_update item ${cmd.chatType}${cmd.chatId} ${cmd.chatItemId} json ${JSON.stringify(cmd.msgContent)}`
    case "apiDeleteChatItem":
      return `/_delete item ${cmd.chatType}${cmd.chatId} ${cmd.chatItemId} ${cmd.deleteMode}`
    case "apiChatRead": {
      const itemRange = cmd.itemRange ? ` from=${cmd.itemRange.fromItem} to=${cmd.itemRange.toItem}` : ""
      return `/_read chat ${cmd.chatType}${cmd.chatId}${itemRange}`
    }
    case "apiDeleteChat":
      return `/_delete ${cmd.chatType}${cmd.chatId}`
    case "apiClearChat":
      return `/_clear chat ${cmd.chatType}${cmd.chatId}`
    case "apiAcceptContact":
      return `/_accept ${cmd.contactReqId}`
    case "apiRejectContact":
      return `/_reject ${cmd.contactReqId}`
    case "apiUpdateProfile":
      return `/_profile ${JSON.stringify(cmd.profile)}`
    case "apiSetContactAlias":
      return `/_set alias @${cmd.contactId} ${cmd.localAlias.trim()}`
    case "apiParseMarkdown":
      return `/_parse ${cmd.text}`
    case "newGroup":
      return `/_group ${JSON.stringify(cmd.groupProfile)}`
    case "apiAddMember":
      return `/_add #${cmd.groupId} ${cmd.contactId} ${cmd.memberRole}`
    case "apiJoinGroup":
      return `/_join #${cmd.groupId}`
    case "apiRemoveMember":
      return `/_remove #${cmd.groupId} ${cmd.memberId}`
    case "apiLeaveGroup":
      return `/_leave #${cmd.groupId}`
    case "apiListMembers":
      return `/_members #${cmd.groupId}`
    case "apiUpdateGroupProfile":
      return `/_group_profile #${cmd.groupId} ${JSON.stringify(cmd.groupProfile)}`
    case "getUserSMPServers":
      return "/smp_servers"
    case "setUserSMPServers":
      return `/smp_servers ${cmd.servers.join(",") || "default"}`
    case "apiContactInfo":
      return `/_info @${cmd.contactId}`
    case "apiGroupMemberInfo":
      return `/_info #${cmd.groupId} ${cmd.memberId}`
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
      return `/auto_accept ${cmd.autoAccept ? "on" : "off"}${cmd.autoReply ? " " + JSON.stringify(cmd.autoReply) : ""}`
    case "receiveFile":
      return `/freceive ${cmd.fileId}${cmd.filePath ? " " + cmd.filePath : ""}`
    case "cancelFile":
      return `/fcancel ${cmd.fileId}`
    case "fileStatus":
      return `/fstatus ${cmd.fileId}`
  }
}

function paginationStr(cp: ChatPagination): string {
  const base = "after" in cp ? ` after=${cp.after}` : "before" in cp ? ` before=${cp.before}` : ""
  return base + ` count=${cp.count}`
}
