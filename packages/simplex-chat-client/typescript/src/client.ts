import {ABQueue} from "./queue"
import {ChatTransport, ChatServer, ChatSrvRequest, ChatSrvResponse, ChatResponseError, localServer, noop} from "./transport"
import {ChatCommand, ChatType, Profile} from "./command"
import {ChatResponse, ChatInfo} from "./response"
import * as CC from "./command"
import * as CR from "./response"

export interface ChatClientConfig {
  readonly qSize: number
  readonly tcpTimeout: number
}

export interface Request {
  readonly resolve: (resp: ChatResponse) => void
  readonly reject: (err?: ChatResponseError | CR.CRChatCmdError) => void
}

export class ChatCommandError extends Error {
  constructor(public message: string, public response: ChatResponse) {
    super(message)
  }
}

export enum ConnReqType {
  Invitation = "invitation",
  Contact = "contact",
}

export class ChatClient {
  private _connected = true
  private clientCorrId = 0
  private readonly sentCommands = new Map<string, Request>()

  static defaultConfig: ChatClientConfig = {
    qSize: 16,
    tcpTimeout: 4000,
  }

  private constructor(
    readonly server: ChatServer | string,
    readonly config: ChatClientConfig,
    readonly msgQ: ABQueue<ChatResponse>,
    readonly client: Promise<void>,
    private readonly transport: ChatTransport
  ) {}

  static async create(server: ChatServer | string = localServer, cfg: ChatClientConfig = ChatClient.defaultConfig): Promise<ChatClient> {
    const transport = await ChatTransport.connect(server, cfg.tcpTimeout, cfg.qSize)
    const msgQ = new ABQueue<ChatResponse>(cfg.qSize)
    const client = runClient().then(noop, noop)
    const c = new ChatClient(server, cfg, msgQ, client, transport)
    return c

    async function runClient(): Promise<void> {
      for await (const t of transport) {
        const apiResp = (t instanceof Promise ? await t : t) as ChatSrvResponse | ChatResponseError
        if (apiResp instanceof ChatResponseError) {
          console.log("chat response error: ", apiResp)
        } else {
          const {corrId, resp} = apiResp
          if (corrId) {
            const req = c.sentCommands.get(corrId)
            if (req) {
              c.sentCommands.delete(corrId)
              req.resolve(resp)
            } else {
              // TODO send error to errQ?
              console.log("no command sent for chat response: ", apiResp)
            }
          } else {
            await msgQ.enqueue(resp)
          }
        }
      }
      c._connected = false
    }
  }

  sendChatCmdStr(cmd: string): Promise<ChatResponse> {
    const corrId = `${++this.clientCorrId}`
    const t: ChatSrvRequest = {corrId, cmd}
    this.transport.write(t).then(noop, noop)
    return new Promise((resolve, reject) => this.sentCommands.set(corrId, {resolve, reject}))
  }

  sendChatCommand(command: ChatCommand): Promise<ChatResponse> {
    return this.sendChatCmdStr(CC.cmdString(command))
  }

  async disconnect(): Promise<void> {
    await this.transport.close()
    await this.client
  }

  async apiGetActiveUser(): Promise<CR.User | undefined> {
    const r = await this.sendChatCommand({type: "showActiveUser"})
    switch (r.type) {
      case "activeUser":
        return r.user
      case "chatCmdError":
        if (r.chatError.type === "error" && r.chatError.errorType.type === "noActiveUser") return undefined
        throw new ChatCommandError("unexpected response error", r)
      default:
        throw new ChatCommandError("unexpected response", r)
    }
  }

  async apiCreateActiveUser(profile?: Profile, sameServers = true, pastTimestamp = false): Promise<CR.User> {
    const r = await this.sendChatCommand({type: "createActiveUser", profile, sameServers, pastTimestamp})
    if (r.type === "activeUser") return r.user
    throw new ChatCommandError("unexpected response", r)
  }

  async apiStartChat(): Promise<void> {
    const r = await this.sendChatCommand({type: "startChat"})
    if (r.type !== "chatStarted" && r.type !== "chatRunning") {
      throw new ChatCommandError("error starting chat", r)
    }
  }

  async apiStopChat(): Promise<void> {
    const r = await this.sendChatCommand({type: "apiStopChat"})
    if (r.type !== "chatStopped") {
      throw new ChatCommandError("error stopping chat", r)
    }
  }

  apiSetIncognito(incognito: boolean): Promise<void> {
    return this.okChatCommand({type: "setIncognito", incognito})
  }

  async enableAddressAutoAccept(acceptIncognito = false, autoReply?: CC.MsgContent): Promise<void> {
    const r = await this.sendChatCommand({type: "addressAutoAccept", autoAccept: {acceptIncognito, autoReply}})
    if (r.type !== "userContactLinkUpdated") {
      throw new ChatCommandError("error changing user contact address mode", r)
    }
  }

  async disableAddressAutoAccept(): Promise<void> {
    const r = await this.sendChatCommand({type: "addressAutoAccept"})
    if (r.type !== "userContactLinkUpdated") {
      throw new ChatCommandError("error changing user contact address mode", r)
    }
  }

  async apiGetChats(userId: number): Promise<CR.Chat[]> {
    const r = await this.sendChatCommand({type: "apiGetChats", userId})
    if (r.type === "apiChats") return r.chats
    throw new ChatCommandError("error loading chats", r)
  }

  async apiGetChat(
    chatType: ChatType,
    chatId: number,
    pagination: CC.ChatPagination = {count: 100},
    search: string | undefined = undefined
  ): Promise<CR.Chat> {
    const r = await this.sendChatCommand({type: "apiGetChat", chatType, chatId, pagination, search})
    if (r.type === "apiChat") return r.chat
    throw new ChatCommandError("error loading chat", r)
  }

  async apiSendMessages(chatType: ChatType, chatId: number, messages: CC.ComposedMessage[]): Promise<CR.AChatItem[]> {
    const r = await this.sendChatCommand({type: "apiSendMessage", chatType, chatId, messages})
    if (r.type === "newChatItems") return r.chatItems
    throw new ChatCommandError("unexpected response", r)
  }

  async apiSendTextMessage(chatType: ChatType, chatId: number, text: string): Promise<CR.AChatItem[]> {
    return this.apiSendMessages(chatType, chatId, [{msgContent: {type: "text", text}}])
  }

  async apiUpdateChatItem(chatType: ChatType, chatId: number, chatItemId: CC.ChatItemId, msgContent: CC.MsgContent): Promise<CR.ChatItem> {
    const r = await this.sendChatCommand({type: "apiUpdateChatItem", chatType, chatId, chatItemId, msgContent})
    if (r.type === "chatItemUpdated") return r.chatItem.chatItem
    throw new ChatCommandError("error updating chat item", r)
  }

  async apiDeleteChatItem(
    chatType: ChatType,
    chatId: number,
    chatItemId: number,
    deleteMode: CC.DeleteMode
  ): Promise<CR.ChatItem | undefined> {
    const r = await this.sendChatCommand({type: "apiDeleteChatItem", chatType, chatId, chatItemId, deleteMode})
    if (r.type === "chatItemDeleted") return r.toChatItem?.chatItem
    throw new ChatCommandError("error deleting chat item", r)
  }

  async apiCreateLink(): Promise<string> {
    const r = await this.sendChatCommand({type: "addContact"})
    if (r.type === "invitation") return r.connReqInvitation
    throw new ChatCommandError("error creating link", r)
  }

  async apiConnect(connReq: string): Promise<ConnReqType> {
    const r = await this.sendChatCommand({type: "connect", connReq})
    switch (r.type) {
      case "sentConfirmation":
        return ConnReqType.Invitation
      case "sentInvitation":
        return ConnReqType.Contact
      default:
        throw new ChatCommandError("connection error", r)
    }
  }

  async apiDeleteChat(chatType: ChatType, chatId: number): Promise<void> {
    const r = await this.sendChatCommand({type: "apiDeleteChat", chatType, chatId})
    switch (chatType) {
      case ChatType.Direct:
        if (r.type === "contactDeleted") return
        break
      case ChatType.Group:
        if (r.type === "groupDeletedUser") return
        break
      case ChatType.ContactRequest:
        if (r.type === "contactConnectionDeleted") return
        break
    }
    throw new ChatCommandError("error deleting chat", r)
  }

  async apiClearChat(chatType: ChatType, chatId: number): Promise<ChatInfo> {
    const r = await this.sendChatCommand({type: "apiClearChat", chatType, chatId})
    if (r.type === "chatCleared") return r.chatInfo
    throw new ChatCommandError("error clearing chat", r)
  }

  async apiUpdateProfile(userId: number, profile: CC.Profile): Promise<CC.Profile | undefined> {
    const r = await this.sendChatCommand({type: "apiUpdateProfile", userId, profile})
    switch (r.type) {
      case "userProfileNoChange":
        return undefined
      case "userProfileUpdated":
        return r.toProfile
      default:
        throw new ChatCommandError("error updating profile", r)
    }
  }

  async apiSetContactAlias(contactId: number, localAlias: string): Promise<CR.Contact> {
    const r = await this.sendChatCommand({type: "apiSetContactAlias", contactId, localAlias})
    if (r.type === "contactAliasUpdated") return r.toContact
    throw new ChatCommandError("error updating contact alias", r)
  }

  async apiCreateUserAddress(): Promise<string> {
    const r = await this.sendChatCommand({type: "createMyAddress"})
    if (r.type === "userContactLinkCreated") return r.connReqContact
    throw new ChatCommandError("error creating user address", r)
  }

  async apiDeleteUserAddress(): Promise<void> {
    const r = await this.sendChatCommand({type: "deleteMyAddress"})
    if (r.type === "userContactLinkDeleted") return
    throw new ChatCommandError("error deleting user address", r)
  }

  async apiGetUserAddress(): Promise<string | undefined> {
    const r = await this.sendChatCommand({type: "showMyAddress"})
    switch (r.type) {
      case "userContactLink":
        return r.contactLink.connReqContact
      default:
        if (r.type === "chatCmdError" && r.chatError.type === "errorStore" && r.chatError.storeError.type === "userContactLinkNotFound") {
          return undefined
        }
        throw new ChatCommandError("error loading user address", r)
    }
  }

  async apiAcceptContactRequest(contactReqId: number): Promise<CR.Contact> {
    const r = await this.sendChatCommand({type: "apiAcceptContact", contactReqId})
    if (r.type === "acceptingContactRequest") return r.contact
    throw new ChatCommandError("error accepting contact request", r)
  }

  async apiRejectContactRequest(contactReqId: number): Promise<void> {
    const r = await this.sendChatCommand({type: "apiRejectContact", contactReqId})
    if (r.type === "contactRequestRejected") return
    throw new ChatCommandError("error rejecting contact request", r)
  }

  apiChatRead(chatType: ChatType, chatId: number, itemRange?: CC.ItemRange): Promise<void> {
    return this.okChatCommand({type: "apiChatRead", chatType, chatId, itemRange})
  }

  async apiContactInfo(contactId: number): Promise<[CR.ConnectionStats?, Profile?]> {
    const r = await this.sendChatCommand({type: "apiContactInfo", contactId})
    if (r.type === "contactInfo") return [r.connectionStats, r.customUserProfile]
    throw new ChatCommandError("error getting contact info", r)
  }

  async apiGroupMemberInfo(groupId: number, memberId: number): Promise<CR.ConnectionStats | undefined> {
    const r = await this.sendChatCommand({type: "apiGroupMemberInfo", groupId, memberId})
    if (r.type === "groupMemberInfo") return r.connectionStats_
    throw new ChatCommandError("error getting group info", r)
  }

  async apiReceiveFile(fileId: number): Promise<CR.AChatItem> {
    const r = await this.sendChatCommand({type: "receiveFile", fileId})
    if (r.type === "rcvFileAccepted") return r.chatItem
    throw new ChatCommandError("error receiving file", r)
  }

  async apiNewGroup(groupProfile: CR.GroupProfile): Promise<CR.GroupInfo> {
    const r = await this.sendChatCommand({type: "newGroup", groupProfile})
    if (r.type === "groupCreated") return r.groupInfo
    throw new ChatCommandError("error creating group", r)
  }

  async apiAddMember(groupId: number, contactId: number, memberRole: CC.GroupMemberRole): Promise<CR.GroupMember> {
    const r = await this.sendChatCommand({type: "apiAddMember", groupId, contactId, memberRole})
    if (r.type === "sentGroupInvitation") return r.member
    throw new ChatCommandError("error adding member", r)
  }

  async apiJoinGroup(groupId: number): Promise<CR.GroupInfo> {
    const r = await this.sendChatCommand({type: "apiJoinGroup", groupId})
    if (r.type === "userAcceptedGroupSent") return r.groupInfo
    throw new ChatCommandError("error joining group", r)
  }

  async apiRemoveMember(groupId: number, memberId: number): Promise<CR.GroupMember> {
    const r = await this.sendChatCommand({type: "apiRemoveMember", groupId, memberId})
    if (r.type === "userDeletedMember") return r.member
    throw new ChatCommandError("error removing member", r)
  }

  async apiLeaveGroup(groupId: number): Promise<CR.GroupInfo> {
    const r = await this.sendChatCommand({type: "apiLeaveGroup", groupId})
    if (r.type === "leftMemberUser") return r.groupInfo
    throw new ChatCommandError("error leaving group", r)
  }

  async apiListMembers(groupId: number): Promise<CR.GroupMember[]> {
    const r = await this.sendChatCommand({type: "apiListMembers", groupId})
    if (r.type === "groupMembers") return r.group.members
    throw new ChatCommandError("error getting group members", r)
  }

  async apiUpdateGroup(groupId: number, groupProfile: CR.GroupProfile): Promise<CR.GroupInfo> {
    const r = await this.sendChatCommand({type: "apiUpdateGroupProfile", groupId, groupProfile})
    if (r.type === "groupUpdated") return r.toGroup
    throw new ChatCommandError("error updating group", r)
  }

  private async okChatCommand(command: ChatCommand): Promise<void> {
    const r = await this.sendChatCommand(command)
    if (r.type !== "cmdOk") throw new ChatCommandError(`${command.type} command error`, r)
  }

  get connected(): boolean {
    return this._connected
  }
}
