type ChatCommand =
  | ShowActiveUser
  | CreateActiveUser
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

interface CC {
  type: ChatCommandTag
}

interface ShowActiveUser extends CC {
  type: "showActiveUser"
}

interface CreateActiveUser extends CC {
  type: "createActiveUser"
  profile: Profile
}

interface SetFilesFolder extends CC {
  type: "setFilesFolder"
  filePath: string
}

interface APIGetChats extends CC {
  type: "apiGetChats"
}

interface APIGetChat extends CC {
  type: "apiGetChat"
  chatType: ChatType
  chatId: number
  pagination: ChatPagination
}

interface APISendMessage extends CC {
  type: "apiSendMessage"
  chatType: ChatType
  chatId: number
  filePath?: number
  quotedItem?: ChatItemId
  msgContent: MsgContent
}

interface APIUpdateChatItem extends CC {
  type: "apiUpdateChatItem"
  chatType: ChatType
  chatId: number
  chatItemId: ChatItemId
  msgContent: MsgContent
}

interface APIDeleteChatItem extends CC {
  type: "apiDeleteChatItem"
  chatType: ChatType
  chatId: number
  chatItemId: ChatItemId
  deleteMode: DeleteMode
}

interface APIChatRead extends CC {
  type: "apiChatRead"
  chatType: ChatType
  chatId: number
  fromItem: ChatItemId
  toItem: ChatItemId
}

interface APIDeleteChat extends CC {
  type: "apiDeleteChat"
  chatType: ChatType
  chatId: number
}

interface APIAcceptContact extends CC {
  type: "apiAcceptContact"
  contactReqId: number
}

interface APIRejectContact extends CC {
  type: "apiRejectContact"
  contactReqId: number
}

interface APIUpdateProfile extends CC {
  type: "apiUpdateProfile"
  profile: Profile
}

interface APIParseMarkdown extends CC {
  type: "apiParseMarkdown"
  text: string
}

interface GetUserSMPServers extends CC {
  type: "getUserSMPServers"
}

interface SetUserSMPServers extends CC {
  type: "setUserSMPServers"
  servers: [string]
}

interface AddContact extends CC {
  type: "addContact"
}

interface Connect extends CC {
  type: "connect"
  connReq: string
}

interface ConnectSimplex extends CC {
  type: "connectSimplex"
}

interface CreateMyAddress extends CC {
  type: "createMyAddress"
}

interface DeleteMyAddress extends CC {
  type: "deleteMyAddress"
}

interface ShowMyAddress extends CC {
  type: "showMyAddress"
}

interface AddressAutoAccept extends CC {
  type: "addressAutoAccept"
  enable: boolean
}

interface Profile {
  displayName: string
  fullName: string // can be empty string
  image?: string
}

enum ChatType {
  CTDirect = "@",
  CTGroup = "#",
}

type ChatPagination =
  | {count: number} // count from the last item in case neither after nor before specified
  | {count: number; after: ChatItemId}
  | {count: number; before: ChatItemId}

type ChatItemId = number

type MsgContentTag = "text" | "link" | "images"

type MsgContent = MCText | MCUnknown

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

function cmdString(cmd: ChatCommand): string {
  switch (cmd.type) {
    case "showActiveUser":
      return "/u"
    case "createActiveUser":
      return `/u ${JSON.stringify(cmd.profile)}`
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
