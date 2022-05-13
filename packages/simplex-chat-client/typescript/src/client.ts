import {ABQueue} from "./queue"
import {ChatTransport, ChatServer, ChatSrvRequest, ChatSrvResponse, ChatResponseError, localServer, noop} from "./transport"
import {ChatCommand, ChatResponse, ChatType} from "./command"
import * as C from "./command"

export interface ChatClientConfig {
  readonly qSize: number
  readonly tcpTimeout: number
}

export interface Request {
  readonly resolve: (resp: ChatResponse) => void
  readonly reject: (err?: ChatResponseError | C.CRChatCmdError) => void
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
              if (resp.type === "chatCmdError") {
                req.reject(resp)
              } else {
                req.resolve(resp)
              }
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

  sendChatCommand(command: ChatCommand): Promise<ChatResponse> {
    const corrId = `${++this.clientCorrId}`
    const t: ChatSrvRequest = {corrId, cmd: C.cmdString(command)}
    this.transport.write(t).then(noop, noop)
    return new Promise((resolve, reject) => this.sentCommands.set(corrId, {resolve, reject}))
  }

  async disconnect(): Promise<void> {
    await this.transport.close()
    await this.client
  }

  async apiGetActiveUser(): Promise<C.User | undefined> {
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

  async apiCreateActiveUser(profile: C.Profile): Promise<C.User> {
    const r = await this.sendChatCommand({type: "createActiveUser", profile})
    if (r.type === "activeUser") return r.user
    throw new ChatCommandError("unexpected response", r)
  }

  async apiStartChat(): Promise<void> {
    const r = await this.sendChatCommand({type: "startChat"})
    if (r.type !== "chatStarted" && r.type !== "chatRunning") {
      throw new ChatCommandError("error starting chat", r)
    }
  }

  async apiGetChats(): Promise<C.Chat[]> {
    const r = await this.sendChatCommand({type: "apiGetChats"})
    if (r.type === "apiChats") return r.chats
    throw new ChatCommandError("error loading chats", r)
  }

  async apiGetChat(chatType: ChatType, chatId: number, pagination: C.ChatPagination = {count: 100}): Promise<C.Chat> {
    const r = await this.sendChatCommand({type: "apiGetChat", chatType, chatId, pagination})
    if (r.type === "apiChat") return r.chat
    throw new ChatCommandError("error loading chat", r)
  }

  async apiSendMessage(chatType: ChatType, chatId: number, message: C.ComposedMessage): Promise<C.AChatItem> {
    const r = await this.sendChatCommand({type: "apiSendMessage", chatType, chatId, message})
    if (r.type === "newChatItem") return r.chatItem
    throw new ChatCommandError("unexpected response", r)
  }

  apiSendTextMessage(chatType: ChatType, chatId: number, text: string): Promise<C.AChatItem> {
    return this.apiSendMessage(chatType, chatId, {msgContent: {type: "text", text}})
  }

  async apiUpdateChatItem(chatType: ChatType, chatId: number, chatItemId: C.ChatItemId, msgContent: C.MsgContent): Promise<C.ChatItem> {
    const r = await this.sendChatCommand({type: "apiUpdateChatItem", chatType, chatId, chatItemId, msgContent})
    if (r.type === "chatItemUpdated") return r.chatItem.chatItem
    throw new ChatCommandError("error updating chat item", r)
  }

  async apiDeleteChatItem(chatType: ChatType, chatId: number, chatItemId: number, deleteMode: C.DeleteMode): Promise<C.ChatItem> {
    const r = await this.sendChatCommand({type: "apiDeleteChatItem", chatType, chatId, chatItemId, deleteMode})
    if (r.type === "chatItemDeleted") return r.toChatItem.chatItem
    throw new ChatCommandError("error deleting chat item", r)
  }

  // func getUserSMPServers() throws -> [String] {
  //     let r = chatSendCmdSync(.getUserSMPServers)
  //     if case let .userSMPServers(smpServers) = r { return smpServers }
  //     throw r
  // }

  // func setUserSMPServers(smpServers: [String]) async throws {
  //     let r = await chatSendCmd(.setUserSMPServers(smpServers: smpServers))
  //     if case .cmdOk = r { return }
  //     throw r
  // }

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

  // func apiConnect(connReq: String) async throws -> Bool {
  //     let r = await chatSendCmd(.connect(connReq: connReq))
  //     let am = AlertManager.shared
  //     switch r {
  //     case .sentConfirmation: return true
  //     case .sentInvitation: return true
  //     case let .contactAlreadyExists(contact):
  //         am.showAlertMsg(
  //             title: "Contact already exists",
  //             message: "You are already connected to \(contact.displayName) via this link."
  //         )
  //         return false
  //     case .chatCmdError(.error(.invalidConnReq)):
  //         am.showAlertMsg(
  //             title: "Invalid connection link",
  //             message: "Please check that you used the correct link or ask your contact to send you another one."
  //         )
  //         return false
  //     case .chatCmdError(.errorAgent(.BROKER(.TIMEOUT))):
  //         am.showAlertMsg(
  //             title: "Connection timeout",
  //             message: "Please check your network connection and try again."
  //         )
  //         return false
  //     case .chatCmdError(.errorAgent(.BROKER(.NETWORK))):
  //         am.showAlertMsg(
  //             title: "Connection error",
  //             message: "Please check your network connection and try again."
  //         )
  //         return false
  //     default: throw r
  //     }
  // }

  async apiDeleteChat(chatType: ChatType, chatId: number): Promise<void> {
    let r = await this.sendChatCommand({type: "apiDeleteChat", chatType, chatId})
    if (r.type !== "contactDeleted") throw new ChatCommandError("error deleting chat", r)
  }

  // func apiUpdateProfile(profile: Profile) async throws -> Profile? {
  //     let r = await chatSendCmd(.apiUpdateProfile(profile: profile))
  //     switch r {
  //     case .userProfileNoChange: return nil
  //     case let .userProfileUpdated(_, toProfile): return toProfile
  //     default: throw r
  //     }
  // }

  // func apiParseMarkdown(text: String) throws -> [FormattedText]? {
  //     let r = chatSendCmdSync(.apiParseMarkdown(text: text))
  //     if case let .apiParsedMarkdown(formattedText) = r { return formattedText }
  //     throw r
  // }

  // func apiCreateUserAddress() async throws -> String {
  //     let r = await chatSendCmd(.createMyAddress)
  //     if case let .userContactLinkCreated(connReq) = r { return connReq }
  //     throw r
  // }

  // func apiDeleteUserAddress() async throws {
  //     let r = await chatSendCmd(.deleteMyAddress)
  //     if case .userContactLinkDeleted = r { return }
  //     throw r
  // }

  // func apiGetUserAddress() async throws -> String? {
  //     let r = await chatSendCmd(.showMyAddress)
  //     switch r {
  //     case let .userContactLink(connReq):
  //         return connReq
  //     case .chatCmdError(chatError: .errorStore(storeError: .userContactLinkNotFound)):
  //         return nil
  //     default: throw r
  //     }
  // }

  // func apiAcceptContactRequest(contactReqId: Int64) async throws -> Contact {
  //     let r = await chatSendCmd(.apiAcceptContact(contactReqId: contactReqId))
  //     if case let .acceptingContactRequest(contact) = r { return contact }
  //     throw r
  // }

  // func apiRejectContactRequest(contactReqId: Int64) async throws {
  //     let r = await chatSendCmd(.apiRejectContact(contactReqId: contactReqId))
  //     if case .contactRequestRejected = r { return }
  //     throw r
  // }

  // func apiChatRead(type: ChatType, id: Int64, itemRange: (Int64, Int64)) async throws {
  //     let r = await chatSendCmd(.apiChatRead(type: type, id: id, itemRange: itemRange))
  //     if case .cmdOk = r { return }
  //     throw r
  // }

  // func acceptContactRequest(_ contactRequest: UserContactRequest) async {
  //     do {
  //         let contact = try await apiAcceptContactRequest(contactReqId: contactRequest.apiId)
  //         let chat = Chat(chatInfo: ChatInfo.direct(contact: contact), chatItems: [])
  //         DispatchQueue.main.async { ChatModel.shared.replaceChat(contactRequest.id, chat) }
  //     } catch let error {
  //         logger.error("acceptContactRequest error: \(error.localizedDescription)")
  //     }
  // }

  // func rejectContactRequest(_ contactRequest: UserContactRequest) async {
  //     do {
  //         try await apiRejectContactRequest(contactReqId: contactRequest.apiId)
  //         DispatchQueue.main.async { ChatModel.shared.removeChat(contactRequest.id) }
  //     } catch let error {
  //         logger.error("rejectContactRequest: \(error.localizedDescription)")
  //     }
  // }

  // func markChatRead(_ chat: Chat) async {
  //     do {
  //         let minItemId = chat.chatStats.minUnreadItemId
  //         let itemRange = (minItemId, chat.chatItems.last?.id ?? minItemId)
  //         let cInfo = chat.chatInfo
  //         try await apiChatRead(type: cInfo.chatType, id: cInfo.apiId, itemRange: itemRange)
  //         DispatchQueue.main.async { ChatModel.shared.markChatItemsRead(cInfo) }
  //     } catch {
  //         logger.error("markChatRead apiChatRead error: \(error.localizedDescription)")
  //     }
  // }

  // func markChatItemRead(_ cInfo: ChatInfo, _ cItem: ChatItem) async {
  //     do {
  //         try await apiChatRead(type: cInfo.chatType, id: cInfo.apiId, itemRange: (cItem.id, cItem.id))
  //         DispatchQueue.main.async { ChatModel.shared.markChatItemRead(cInfo, cItem) }
  //     } catch {
  //         logger.error("markChatItemRead apiChatRead error: \(error.localizedDescription)")
  //     }
  // }

  // async createSMPQueue(rcvKey: SignKey, rcvPubKey: PublicKey<KeyType.Verify>): Promise<SMP.IDS> {
  //   const pubKeyStr = new Uint8Array(await C.encodePubKey(rcvPubKey))
  //   const resp = await this.sendSMPCommand(rcvKey, B.empty, SMP.cNEW(pubKeyStr))
  //   if (resp.cmd === "IDS") return resp
  //   throw new Error("unexpected response")
  // }

  // subscribeSMPQueue(rcvKey: SignKey, queueId: Uint8Array): Promise<void> {
  //   return this.msgSMPCommand(rcvKey, queueId, SMP.cSUB())
  // }

  // async secureSMPQueue(rcvKey: SignKey, queueId: Uint8Array, sndPubKey: PublicKey<KeyType.Verify>): Promise<void> {
  //   const pubKeyStr = new Uint8Array(await C.encodePubKey(sndPubKey))
  //   return this.okSMPCommand(rcvKey, queueId, SMP.cKEY(pubKeyStr))
  // }

  // async sendSMPMessage(sndKey: SignKey | undefined, queueId: Uint8Array, msg: Uint8Array): Promise<void> {
  //   const resp = await this.sendSMPCommand(sndKey, queueId, SMP.cSEND(msg))
  //   if (resp.cmd !== "OK") throw new Error("unexpected response")
  // }

  // ackSMPMessage(rcvKey: SignKey, queueId: Uint8Array): Promise<void> {
  //   return this.msgSMPCommand(rcvKey, queueId, SMP.cACK())
  // }

  // suspendSMPQueue(rcvKey: SignKey, queueId: Uint8Array): Promise<void> {
  //   return this.okSMPCommand(rcvKey, queueId, SMP.cOFF())
  // }

  // deleteSMPQueue(rcvKey: SignKey, queueId: Uint8Array): Promise<void> {
  //   return this.okSMPCommand(rcvKey, queueId, SMP.cDEL())
  // }

  // private async msgSMPCommand(rcvKey: SignKey, queueId: Uint8Array, command: SMPCommand<Client>): Promise<void> {
  //   const resp = await this.sendSMPCommand(rcvKey, queueId, command)
  //   switch (resp.cmd) {
  //     case "OK":
  //       return
  //     case "MSG":
  //       return this.msgQ.enqueue({server: this.server, queueId, command: resp})
  //     default:
  //       throw new Error("unexpected response")
  //   }
  // }

  // private async okChatCommand(command: ChatCommand): Promise<void> {
  //   const resp = await this.sendChatCommand(command)
  //   if (resp.type !== "cmdOk") throw new ChatCommandError("unexpected response", resp)
  // }

  get connected(): boolean {
    return this._connected
  }
}
