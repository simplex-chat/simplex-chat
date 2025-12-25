import {ABQueue} from "./queue"
import {ChatTransport, ChatServer, ChatSrvRequest, ChatSrvResponse, ChatResponseError, localServer, noop, ChatSrvEvent} from "./transport"
import {CC, ChatResponse, CR, ChatEvent, T} from "@simplex-chat/types"

export interface ChatClientConfig {
  readonly qSize: number
  readonly tcpTimeout: number
}

export interface Request {
  readonly resolve: (resp: ChatResponse) => void
  readonly reject: (err?: ChatResponseError | CR.ChatCmdError) => void
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
    readonly msgQ: ABQueue<ChatEvent>,
    readonly client: Promise<void>,
    private readonly transport: ChatTransport
  ) {}

  static async create(server: ChatServer | string = localServer, cfg: ChatClientConfig = ChatClient.defaultConfig): Promise<ChatClient> {
    const transport = await ChatTransport.connect(server, cfg.tcpTimeout, cfg.qSize)
    const msgQ = new ABQueue<ChatEvent>(cfg.qSize)
    const client = runClient().then(noop, noop)
    const c = new ChatClient(server, cfg, msgQ, client, transport)
    return c

    async function runClient(): Promise<void> {
      for await (const t of transport) {
        const apiResp = (t instanceof Promise ? await t : t) as ChatSrvResponse | ChatSrvEvent | ChatResponseError
        if (apiResp instanceof ChatResponseError) {
          console.log("chat response error: ", apiResp)
        } else if ("corrId" in apiResp) {
          const {corrId, resp} = apiResp
          const req = c.sentCommands.get(corrId)
          if (req) {
            c.sentCommands.delete(corrId)
            req.resolve(resp)
          } else {
            // TODO send error to errQ?
            console.log("no command sent for chat response: ", apiResp)
          }
        } else {
          await msgQ.enqueue(apiResp.resp)
        }
      }
      c._connected = false
    }
  }

  sendChatCmd(cmd: string): Promise<ChatResponse> {
    const corrId = `${++this.clientCorrId}`
    const t: ChatSrvRequest = {corrId, cmd}
    const p = new Promise<ChatResponse>((resolve, reject) => this.sentCommands.set(corrId, {resolve, reject}))
    this.transport.write(t).then(noop, noop)
    return p
  }

  async disconnect(): Promise<void> {
    await this.transport.close()
    await this.client
  }

  async apiGetActiveUser(): Promise<T.User | undefined> {
    const r = await this.sendChatCmd(CC.ShowActiveUser.cmdString({}))
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

  async apiCreateActiveUser(profile?: T.Profile): Promise<T.User> {
    const r = await this.sendChatCmd(CC.CreateActiveUser.cmdString({newUser: {profile, pastTimestamp: false}}))
    if (r.type === "activeUser") return r.user
    throw new ChatCommandError("unexpected response", r)
  }

  async enableAddressAutoAccept(userId: number, autoReply?: T.MsgContent, businessAddress = false): Promise<void> {
    const r = await this.sendChatCmd(
      CC.APISetAddressSettings.cmdString({userId, settings: {businessAddress, autoAccept: {acceptIncognito: false}, autoReply}})
    )
    if (r.type !== "userContactLinkUpdated") {
      throw new ChatCommandError("error changing user contact address mode", r)
    }
  }

  async disableAddressAutoAccept(userId: number, businessAddress = false): Promise<void> {
    const r = await this.sendChatCmd(CC.APISetAddressSettings.cmdString({userId, settings: {businessAddress}}))
    if (r.type !== "userContactLinkUpdated") {
      throw new ChatCommandError("error changing user contact address mode", r)
    }
  }

  async apiSendMessages(chatType: T.ChatType, chatId: number, messages: T.ComposedMessage[], ttl?: number): Promise<T.AChatItem[]> {
    const r = await this.sendChatCmd(
      CC.APISendMessages.cmdString({sendRef: {chatType, chatId}, composedMessages: messages, ttl, liveMessage: false})
    )
    if (r.type === "newChatItems") return r.chatItems
    throw new ChatCommandError("unexpected response", r)
  }

  async apiSendTextMessage(chatType: T.ChatType, chatId: number, text: string): Promise<T.AChatItem[]> {
    return this.apiSendMessages(chatType, chatId, [{msgContent: {type: "text", text}, mentions: {}}])
  }

  async apiUpdateChatItem(chatType: T.ChatType, chatId: number, chatItemId: number, msgContent: T.MsgContent): Promise<T.ChatItem> {
    const r = await this.sendChatCmd(
      CC.APIUpdateChatItem.cmdString({
        chatRef: {chatType, chatId},
        chatItemId,
        liveMessage: false,
        updatedMessage: {msgContent, mentions: {}},
      })
    )
    if (r.type === "chatItemUpdated") return r.chatItem.chatItem
    throw new ChatCommandError("error updating chat item", r)
  }

  async apiDeleteChatItems(
    chatType: T.ChatType,
    chatId: number,
    chatItemIds: number[],
    deleteMode: T.CIDeleteMode
  ): Promise<T.ChatItemDeletion[] | undefined> {
    const r = await this.sendChatCmd(CC.APIDeleteChatItem.cmdString({chatRef: {chatType, chatId}, chatItemIds, deleteMode}))
    if (r.type === "chatItemsDeleted") return r.chatItemDeletions
    throw new ChatCommandError("error deleting chat item", r)
  }

  async apiCreateLink(userId: number): Promise<string> {
    const r = await this.sendChatCmd(CC.APIAddContact.cmdString({userId, incognito: false}))
    if (r.type === "invitation") {
      const link = r.connLinkInvitation
      return link.connShortLink || link.connFullLink
    }
    throw new ChatCommandError("error creating link", r)
  }

  async apiConnectActiveUser(connLink: string): Promise<ConnReqType> {
    const r = await this.sendChatCmd(CC.Connect.cmdString({incognito: false, connLink_: connLink}))
    switch (r.type) {
      case "sentConfirmation":
        return ConnReqType.Invitation
      case "sentInvitation":
        return ConnReqType.Contact
      default:
        throw new ChatCommandError("connection error", r)
    }
  }

  async apiDeleteChat(chatType: T.ChatType, chatId: number, deleteMode: T.ChatDeleteMode = {type: "full", notify: true}): Promise<void> {
    const r = await this.sendChatCmd(CC.APIDeleteChat.cmdString({chatRef: {chatType, chatId}, chatDeleteMode: deleteMode}))
    switch (chatType) {
      case T.ChatType.Direct:
        if (r.type === "contactDeleted") return
        break
      case T.ChatType.Group:
        if (r.type === "groupDeletedUser") return
        break
    }
    throw new ChatCommandError("error deleting chat", r)
  }

  async apiUpdateProfile(userId: number, profile: T.Profile): Promise<T.Profile | undefined> {
    const r = await this.sendChatCmd(CC.APIUpdateProfile.cmdString({userId, profile}))
    switch (r.type) {
      case "userProfileNoChange":
        return undefined
      case "userProfileUpdated":
        return r.toProfile
      default:
        throw new ChatCommandError("error updating profile", r)
    }
  }

  async apiCreateUserAddress(userId: number): Promise<string> {
    const r = await this.sendChatCmd(CC.APICreateMyAddress.cmdString({userId}))
    if (r.type === "userContactLinkCreated") {
      const link = r.connLinkContact
      return link.connShortLink || link.connFullLink
    }
    throw new ChatCommandError("error creating user address", r)
  }

  async apiDeleteUserAddress(userId: number): Promise<void> {
    const r = await this.sendChatCmd(CC.APIDeleteMyAddress.cmdString({userId}))
    if (r.type === "userContactLinkDeleted") return
    throw new ChatCommandError("error deleting user address", r)
  }

  async apiGetUserAddress(userId: number): Promise<string | undefined> {
    const r = await this.sendChatCmd(CC.APIShowMyAddress.cmdString({userId}))
    switch (r.type) {
      case "userContactLink": {
        const link = r.contactLink.connLinkContact
        return link.connShortLink || link.connFullLink
      }
      default:
        if (r.type === "chatCmdError" && r.chatError.type === "errorStore" && r.chatError.storeError.type === "userContactLinkNotFound") {
          return undefined
        }
        throw new ChatCommandError("error loading user address", r)
    }
  }

  async apiAcceptContactRequest(contactReqId: number): Promise<T.Contact> {
    const r = await this.sendChatCmd(CC.APIAcceptContact.cmdString({contactReqId}))
    if (r.type === "acceptingContactRequest") return r.contact
    throw new ChatCommandError("error accepting contact request", r)
  }

  async apiRejectContactRequest(contactReqId: number): Promise<void> {
    const r = await this.sendChatCmd(CC.APIRejectContact.cmdString({contactReqId}))
    if (r.type === "contactRequestRejected") return
    throw new ChatCommandError("error rejecting contact request", r)
  }

  async apiReceiveFile(fileId: number): Promise<T.AChatItem> {
    const r = await this.sendChatCmd(CC.ReceiveFile.cmdString({fileId, userApprovedRelays: true}))
    if (r.type === "rcvFileAccepted") return r.chatItem
    throw new ChatCommandError("error receiving file", r)
  }

  async apiNewGroup(userId: number, groupProfile: T.GroupProfile): Promise<T.GroupInfo> {
    const r = await this.sendChatCmd(CC.APINewGroup.cmdString({userId, groupProfile, incognito: false}))
    if (r.type === "groupCreated") return r.groupInfo
    throw new ChatCommandError("error creating group", r)
  }

  async apiAddMember(groupId: number, contactId: number, memberRole: T.GroupMemberRole): Promise<T.GroupMember> {
    const r = await this.sendChatCmd(CC.APIAddMember.cmdString({groupId, contactId, memberRole}))
    if (r.type === "sentGroupInvitation") return r.member
    throw new ChatCommandError("error adding member", r)
  }

  async apiJoinGroup(groupId: number): Promise<T.GroupInfo> {
    const r = await this.sendChatCmd(CC.APIJoinGroup.cmdString({groupId}))
    if (r.type === "userAcceptedGroupSent") return r.groupInfo
    throw new ChatCommandError("error joining group", r)
  }

  async apiRemoveMembers(groupId: number, memberIds: number[], withMessages = false): Promise<T.GroupMember[]> {
    const r = await this.sendChatCmd(CC.APIRemoveMembers.cmdString({groupId, groupMemberIds: memberIds, withMessages}))
    if (r.type === "userDeletedMembers") return r.members
    throw new ChatCommandError("error removing member", r)
  }

  async apiLeaveGroup(groupId: number): Promise<T.GroupInfo> {
    const r = await this.sendChatCmd(CC.APILeaveGroup.cmdString({groupId}))
    if (r.type === "leftMemberUser") return r.groupInfo
    throw new ChatCommandError("error leaving group", r)
  }

  async apiListMembers(groupId: number): Promise<T.GroupMember[]> {
    const r = await this.sendChatCmd(CC.APIListMembers.cmdString({groupId}))
    if (r.type === "groupMembers") return r.group.members
    throw new ChatCommandError("error getting group members", r)
  }

  async apiUpdateGroup(groupId: number, groupProfile: T.GroupProfile): Promise<T.GroupInfo> {
    const r = await this.sendChatCmd(CC.APIUpdateGroupProfile.cmdString({groupId, groupProfile}))
    if (r.type === "groupUpdated") return r.toGroup
    throw new ChatCommandError("error updating group", r)
  }

  get connected(): boolean {
    return this._connected
  }
}
