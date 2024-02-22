export type ChatCommand =
  | ShowActiveUser
  | CreateActiveUser
  | ListUsers
  | APISetActiveUser
  | APIHideUser
  | APIUnhideUser
  | APIMuteUser
  | APIUnmuteUser
  | APIDeleteUser
  | StartChat
  | APIStopChat
  | SetTempFolder
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
  | APIDeleteMemberChatItem
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
  | APICreateGroupLink
  | APIGroupLinkMemberRole
  | APIDeleteGroupLink
  | APIGetGroupLink
  | APIGetUserProtoServers
  | APISetUserProtoServers
  | APIContactInfo
  | APIGroupMemberInfo
  | APIGetContactCode
  | APIGetGroupMemberCode
  | APIVerifyContact
  | APIVerifyGroupMember
  | AddContact
  | Connect
  | ConnectSimplex
  | CreateMyAddress
  | DeleteMyAddress
  | ShowMyAddress
  | SetProfileAddress
  | AddressAutoAccept
  | APICreateMyAddress
  | APIDeleteMyAddress
  | APIShowMyAddress
  | APISetProfileAddress
  | APIAddressAutoAccept
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
// APISetChatItemTTL
// APIGetChatItemTTL
// APISetNetworkConfig
// APIGetNetworkConfig
// APISetChatSettings
// ShowMessages
// LastMessages
// SendMessageBroadcast

type ChatCommandTag =
  | "showActiveUser"
  | "createActiveUser"
  | "listUsers"
  | "apiSetActiveUser"
  | "setActiveUser"
  | "apiHideUser"
  | "apiUnhideUser"
  | "apiMuteUser"
  | "apiUnmuteUser"
  | "apiDeleteUser"
  | "startChat"
  | "apiStopChat"
  | "setTempFolder"
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
  | "apiDeleteMemberChatItem"
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
  | "apiCreateGroupLink"
  | "apiGroupLinkMemberRole"
  | "apiDeleteGroupLink"
  | "apiGetGroupLink"
  | "apiGetUserProtoServers"
  | "apiSetUserProtoServers"
  | "apiContactInfo"
  | "apiGroupMemberInfo"
  | "apiGetContactCode"
  | "apiGetGroupMemberCode"
  | "apiVerifyContact"
  | "apiVerifyGroupMember"
  | "addContact"
  | "connect"
  | "connectSimplex"
  | "createMyAddress"
  | "deleteMyAddress"
  | "showMyAddress"
  | "setProfileAddress"
  | "addressAutoAccept"
  | "apiCreateMyAddress"
  | "apiDeleteMyAddress"
  | "apiShowMyAddress"
  | "apiSetProfileAddress"
  | "apiAddressAutoAccept"
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
  profile?: Profile
  sameServers: boolean
  pastTimestamp: boolean
}

export interface ListUsers extends IChatCommand {
  type: "listUsers"
}

export interface APISetActiveUser extends IChatCommand {
  type: "apiSetActiveUser"
  userId: number
  viewPwd?: string
}

export interface APIHideUser extends IChatCommand {
  type: "apiHideUser"
  userId: number
  viewPwd: string
}

export interface APIUnhideUser extends IChatCommand {
  type: "apiUnhideUser"
  userId: number
  viewPwd: string
}

export interface APIMuteUser extends IChatCommand {
  type: "apiMuteUser"
  userId: number
}

export interface APIUnmuteUser extends IChatCommand {
  type: "apiUnmuteUser"
  userId: number
}

export interface APIDeleteUser extends IChatCommand {
  type: "apiDeleteUser"
  userId: number
  delSMPQueues: boolean
  viewPwd?: string
}

export interface StartChat extends IChatCommand {
  type: "startChat"
  subscribeConnections?: boolean
  enableExpireChatItems?: boolean
  startXFTPWorkers?: boolean
}

export interface APIStopChat extends IChatCommand {
  type: "apiStopChat"
}

export interface SetTempFolder extends IChatCommand {
  type: "setTempFolder"
  tempFolder: string
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
  userId: number
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

export interface APIDeleteMemberChatItem extends IChatCommand {
  type: "apiDeleteMemberChatItem"
  groupId: number
  groupMemberId: number
  itemId: number
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
  userId: number
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

export interface APICreateGroupLink extends IChatCommand {
  type: "apiCreateGroupLink"
  groupId: number
  memberRole: GroupMemberRole
}

export interface APIGroupLinkMemberRole extends IChatCommand {
  type: "apiGroupLinkMemberRole"
  groupId: number
  memberRole: GroupMemberRole
}

export interface APIDeleteGroupLink extends IChatCommand {
  type: "apiDeleteGroupLink"
  groupId: number
}

export interface APIGetGroupLink extends IChatCommand {
  type: "apiGetGroupLink"
  groupId: number
}

export interface APIGetUserProtoServers extends IChatCommand {
  type: "apiGetUserProtoServers"
  userId: number
  serverProtocol: ServerProtocol
}

export interface APISetUserProtoServers extends IChatCommand {
  type: "apiSetUserProtoServers"
  userId: number
  serverProtocol: ServerProtocol
  servers: ServerCfg[]
}

export interface ServerCfg {
  server: string
  preset: boolean
  tested?: boolean
  enabled: boolean
}

export enum ServerProtocol {
  SMP = "smp",
  XFTP = "xftp",
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

export interface APIGetContactCode extends IChatCommand {
  type: "apiGetContactCode"
  contactId: number
}

export interface APIGetGroupMemberCode extends IChatCommand {
  type: "apiGetGroupMemberCode"
  groupId: number
  groupMemberId: number
}

export interface APIVerifyContact extends IChatCommand {
  type: "apiVerifyContact"
  contactId: number
  connectionCode: string
}

export interface APIVerifyGroupMember extends IChatCommand {
  type: "apiVerifyGroupMember"
  groupId: number
  groupMemberId: number
  connectionCode: string
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

export interface SetProfileAddress extends IChatCommand {
  type: "setProfileAddress"
  includeInProfile: boolean
}

export interface AddressAutoAccept extends IChatCommand {
  type: "addressAutoAccept"
  autoAccept?: AutoAccept
}

export interface APICreateMyAddress extends IChatCommand {
  type: "apiCreateMyAddress"
  userId: number
}

export interface APIDeleteMyAddress extends IChatCommand {
  type: "apiDeleteMyAddress"
  userId: number
}

export interface APIShowMyAddress extends IChatCommand {
  type: "apiShowMyAddress"
  userId: number
}

export interface APISetProfileAddress extends IChatCommand {
  type: "apiSetProfileAddress"
  userId: number
  includeInProfile: boolean
}

export interface APIAddressAutoAccept extends IChatCommand {
  type: "apiAddressAutoAccept"
  userId: number
  autoAccept?: AutoAccept
}

export interface AutoAccept {
  acceptIncognito: boolean
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

interface NewUser {
  profile?: Profile
  sameServers: boolean
  pastTimestamp: boolean
}

export interface Profile {
  displayName: string
  fullName: string // can be empty string
  image?: string
  contactLink?: string
  // preferences?: Preferences
}

export interface LocalProfile {
  profileId: number
  displayName: string
  fullName: string
  image?: string
  contactLink?: string
  // preferences?: Preferences
  localAlias: string
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
    case "createActiveUser": {
      const user: NewUser = {profile: cmd.profile, sameServers: cmd.sameServers, pastTimestamp: cmd.pastTimestamp}
      return `/_create user ${JSON.stringify(user)}`
    }
    case "listUsers":
      return `/users`
    case "apiSetActiveUser":
      return `/_user ${cmd.userId}${maybeJSON(cmd.viewPwd)}`
    case "apiHideUser":
      return `/_hide user ${cmd.userId} ${JSON.stringify(cmd.viewPwd)}`
    case "apiUnhideUser":
      return `/_unhide user ${cmd.userId} ${JSON.stringify(cmd.viewPwd)}`
    case "apiMuteUser":
      return `/_mute user ${cmd.userId}`
    case "apiUnmuteUser":
      return `/_unmute user ${cmd.userId}`
    case "apiDeleteUser":
      return `/_delete user ${cmd.userId} del_smp=${onOff(cmd.delSMPQueues)}${maybeJSON(cmd.viewPwd)}`
    case "startChat":
      return `/_start subscribe=${cmd.subscribeConnections ? "on" : "off"} expire=${cmd.enableExpireChatItems ? "on" : "off"}`
    case "apiStopChat":
      return "/_stop"
    case "setTempFolder":
      return `/_temp_folder ${cmd.tempFolder}`
    case "setFilesFolder":
      return `/_files_folder ${cmd.filePath}`
    case "setIncognito":
      return `/incognito ${onOff(cmd.incognito)}`
    case "apiExportArchive":
      return `/_db export ${JSON.stringify(cmd.config)}`
    case "apiImportArchive":
      return `/_db import ${JSON.stringify(cmd.config)}`
    case "apiDeleteStorage":
      return "/_db delete"
    case "apiGetChats":
      return `/_get chats pcc=${onOff(cmd.pendingConnections)}`
    case "apiGetChat":
      return `/_get chat ${cmd.chatType}${cmd.chatId}${paginationStr(cmd.pagination)}`
    case "apiSendMessage":
      return `/_send ${cmd.chatType}${cmd.chatId} json ${JSON.stringify(cmd.message)}`
    case "apiUpdateChatItem":
      return `/_update item ${cmd.chatType}${cmd.chatId} ${cmd.chatItemId} json ${JSON.stringify(cmd.msgContent)}`
    case "apiDeleteChatItem":
      return `/_delete item ${cmd.chatType}${cmd.chatId} ${cmd.chatItemId} ${cmd.deleteMode}`
    case "apiDeleteMemberChatItem":
      return `/_delete member item #${cmd.groupId} ${cmd.groupMemberId} ${cmd.itemId}`
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
      return `/_profile ${cmd.userId} ${JSON.stringify(cmd.profile)}`
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
    case "apiCreateGroupLink":
      return `/_create link #${cmd.groupId} ${cmd.memberRole}`
    case "apiGroupLinkMemberRole":
      return `/_set link role #${cmd.groupId} ${cmd.memberRole}`
    case "apiDeleteGroupLink":
      return `/_delete link #${cmd.groupId}`
    case "apiGetGroupLink":
      return `/_get link #${cmd.groupId}`
    case "apiGetUserProtoServers":
      return `/_servers ${cmd.userId} ${cmd.serverProtocol}`
    case "apiSetUserProtoServers":
      return `/_servers ${cmd.userId} ${cmd.serverProtocol} ${JSON.stringify({servers: cmd.servers})}`
    case "apiContactInfo":
      return `/_info @${cmd.contactId}`
    case "apiGroupMemberInfo":
      return `/_info #${cmd.groupId} ${cmd.memberId}`
    case "apiGetContactCode":
      return `/_get code @${cmd.contactId}`
    case "apiGetGroupMemberCode":
      return `/_get code #${cmd.groupId} ${cmd.groupMemberId}`
    case "apiVerifyContact":
      return `/_verify code @${cmd.contactId}${maybe(cmd.connectionCode)}`
    case "apiVerifyGroupMember":
      return `/_verify code #${cmd.groupId} ${cmd.groupMemberId}${maybe(cmd.connectionCode)}`
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
    case "setProfileAddress":
      return `/profile_address ${onOff(cmd.includeInProfile)}`
    case "addressAutoAccept":
      return `/auto_accept ${autoAcceptStr(cmd.autoAccept)}`
    case "apiCreateMyAddress":
      return `/_address ${cmd.userId}`
    case "apiDeleteMyAddress":
      return `/_delete_address ${cmd.userId}`
    case "apiShowMyAddress":
      return `/_show_address ${cmd.userId}`
    case "apiSetProfileAddress":
      return `/_profile_address ${cmd.userId} ${onOff(cmd.includeInProfile)}`
    case "apiAddressAutoAccept":
      return `/_auto_accept ${cmd.userId} ${autoAcceptStr(cmd.autoAccept)}`
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

function maybe<T>(value: T | undefined): string {
  return value ? ` ${value}` : ""
}

function maybeJSON<T>(value: T | undefined): string {
  return value ? ` json ${JSON.stringify(value)}` : ""
}

function onOff<T>(value: T | undefined): string {
  return value ? "on" : "off"
}

function autoAcceptStr(autoAccept: AutoAccept | undefined): string {
  if (!autoAccept) return "off"
  const msg = autoAccept.autoReply
  return "on" + (autoAccept.acceptIncognito ? " incognito=on" : "") + (msg ? " json " + JSON.stringify(msg) : "")
}
