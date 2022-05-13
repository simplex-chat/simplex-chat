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

export enum ChatType {
  CTDirect = "@",
  CTGroup = "#",
  CTContactRequest = "<@",
}

export type ChatPagination =
  | {count: number} // count from the last item in case neither after nor before specified
  | {count: number; after: ChatItemId}
  | {count: number; before: ChatItemId}

export type ChatItemId = number

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

export enum DeleteMode {
  Broadcast = "broadcast",
  Internal = "internal",
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
      return `/_send ${cmd.chatType}${cmd.chatId} json ${JSON.stringify(cmd.message)}`
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
