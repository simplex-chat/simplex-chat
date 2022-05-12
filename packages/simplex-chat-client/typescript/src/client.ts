// import * as SMP from "./protocol"
// import {SMPCommand, SMPError, Party, Client} from "./protocol"
import {ABQueue} from "./queue"
import {ChatTransport, ChatServer, APICommand, APIResponse, ChatResponseError, noop} from "./transport"
import {ChatCommand, ChatResponse} from "./command"
import * as C from "./command"

export interface ChatClientConfig {
  readonly qSize: number
  readonly tcpTimeout: number
}

export const defaultChatClientConfig: ChatClientConfig = {
  qSize: 16,
  tcpTimeout: 4000,
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

export class ChatClient {
  private _connected = true
  private clientCorrId = 0
  private readonly sentCommands = new Map<number, Request>()

  private constructor(
    readonly server: ChatServer,
    readonly config: ChatClientConfig,
    readonly msgQ: ABQueue<ChatResponse>,
    readonly client: Promise<void>,
    private readonly transport: ChatTransport
  ) {}

  static async create(server: ChatServer, cfg: ChatClientConfig, msgQ: ABQueue<ChatResponse>): Promise<ChatClient> {
    const transport = await ChatTransport.connect(server, cfg.tcpTimeout, cfg.qSize)
    const client = runClient().then(noop, noop)
    const c = new ChatClient(server, cfg, msgQ, client, transport)
    return c

    async function runClient(): Promise<void> {
      for await (const t of transport) {
        const apiResp: APIResponse | ChatResponseError = t instanceof Promise ? await t : t
        if (apiResp instanceof ChatResponseError) {
          console.log("chat response error: ", apiResp)
        } else {
          const {corrId, resp} = apiResp
          if (corrId) {
            const req = c.sentCommands.get(corrId)
            if (req) {
              c.sentCommands.delete(corrId)
              if (resp.type == "chatCmdError") {
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
    const corrId = ++this.clientCorrId
    const t: APICommand = {corrId, command}
    this.transport.write(t).then(noop, noop)
    return new Promise((resolve, reject) => this.sentCommands.set(corrId, {resolve, reject}))
  }

  async disconnect(): Promise<void> {
    await this.transport.close()
    await this.client
  }

  async getActiveUser(): Promise<C.User | undefined> {
    const r = await this.sendChatCommand({type: "showActiveUser"})
    switch (r.type) {
      case "activeUser":
        return r.user
      case "chatCmdError":
        if (r.chatError.type == "error" && r.chatError.errorType.type == "noActiveUser") return undefined
        throw new ChatCommandError("unexpected response error", r)
      default:
        throw new ChatCommandError("unexpected response", r)
    }
  }

  async createActiveUser(profile: C.Profile): Promise<C.User> {
    const r = await this.sendChatCommand({type: "createActiveUser", profile})
    if (r.type == "activeUser") return r.user
    throw new ChatCommandError("unexpected response", r)
  }

  async apiStartChat(): Promise<void> {
    const r = await this.sendChatCommand({type: "startChat"})
    if (r.type != "chatStarted") throw new ChatCommandError("unexpected response", r)
  }

  // func apiGetChats() throws -> [Chat] {
  //     let r = chatSendCmdSync(.apiGetChats)
  //     if case let .apiChats(chats) = r { return chats.map { Chat.init($0) } }
  //     throw r
  // }

  // func apiGetChat(type: ChatType, id: Int64) throws -> Chat {
  //     let r = chatSendCmdSync(.apiGetChat(type: type, id: id))
  //     if case let .apiChat(chat) = r { return Chat.init(chat) }
  //     throw r
  // }

  // async sendMessage(type: ChatType, id: number, msg: C.MsgContent)

  // func apiSendMessage(type: ChatType, id: Int64, quotedItemId: Int64?, msg: MsgContent) async throws -> ChatItem {
  //     let chatModel = ChatModel.shared
  //     let cmd: ChatCommand
  //     if let itemId = quotedItemId {
  //         cmd = .apiSendMessageQuote(type: type, id: id, itemId: itemId, msg: msg)
  //     } else {
  //         cmd = .apiSendMessage(type: type, id: id, msg: msg)
  //     }
  //     let r: ChatResponse
  //     if type == .direct {
  //         var cItem: ChatItem!
  //         let endTask = beginBGTask({ if cItem != nil { chatModel.messageDelivery.removeValue(forKey: cItem.id) } })
  //         r = await chatSendCmd(cmd, bgTask: false)
  //         if case let .newChatItem(aChatItem) = r {
  //             cItem = aChatItem.chatItem
  //             chatModel.messageDelivery[cItem.id] = endTask
  //             return cItem
  //         }
  //         endTask()
  //     } else {
  //         r = await chatSendCmd(cmd, bgDelay: msgDelay)
  //         if case let .newChatItem(aChatItem) = r {
  //             return aChatItem.chatItem
  //         }
  //     }
  //     throw r
  // }

  // func apiUpdateChatItem(type: ChatType, id: Int64, itemId: Int64, msg: MsgContent) async throws -> ChatItem {
  //   let r = await chatSendCmd(.apiUpdateChatItem(type: type, id: id, itemId: itemId, msg: msg), bgDelay: msgDelay)
  //   if case let .chatItemUpdated(aChatItem) = r { return aChatItem.chatItem }
  //   throw r
  // }

  // func apiDeleteChatItem(type: ChatType, id: Int64, itemId: Int64, mode: CIDeleteMode) async throws -> ChatItem {
  //     let r = await chatSendCmd(.apiDeleteChatItem(type: type, id: id, itemId: itemId, mode: mode), bgDelay: msgDelay)
  //     if case let .chatItemDeleted(_, toChatItem) = r { return toChatItem.chatItem }
  //     throw r
  // }

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

  // func apiAddContact() throws -> String {
  //     let r = chatSendCmdSync(.addContact, bgTask: false)
  //     if case let .invitation(connReqInvitation) = r { return connReqInvitation }
  //     throw r
  // }

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

  // func apiDeleteChat(type: ChatType, id: Int64) async throws {
  //     let r = await chatSendCmd(.apiDeleteChat(type: type, id: id), bgTask: false)
  //     if case .contactDeleted = r { return }
  //     throw r
  // }

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
