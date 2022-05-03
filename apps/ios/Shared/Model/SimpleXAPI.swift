//
//  ChatAPI.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 27/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import UIKit
import Dispatch
import BackgroundTasks

private var chatController: chat_ctrl?

enum TerminalItem: Identifiable {
    case cmd(Date, ChatCommand)
    case resp(Date, ChatResponse)

    var id: Date {
        get {
            switch self {
            case let .cmd(id, _): return id
            case let .resp(id, _): return id
            }
        }
    }

    var label: String {
        get {
            switch self {
            case let .cmd(_, cmd): return "> \(cmd.cmdString.prefix(30))"
            case let .resp(_, resp): return "< \(resp.responseType)"
            }
        }
    }

    var details: String {
        get {
            switch self {
            case let .cmd(_, cmd): return cmd.cmdString
            case let .resp(_, resp): return resp.details
            }
        }
    }
}

private func beginBGTask(_ handler: (() -> Void)? = nil) -> (() -> Void) {
    var id: UIBackgroundTaskIdentifier!
    var running = true
    let endTask = {
//        logger.debug("beginBGTask: endTask \(id.rawValue)")
        if running {
            running = false
            if let h = handler {
//                logger.debug("beginBGTask: user handler")
                h()
            }
            if id != .invalid {
                UIApplication.shared.endBackgroundTask(id)
                id = .invalid
            }
        }
    }
    id = UIApplication.shared.beginBackgroundTask(expirationHandler: endTask)
//    logger.debug("beginBGTask: \(id.rawValue)")
    return endTask
}

let msgDelay: Double = 7.5
let maxTaskDuration: Double = 15

private func withBGTask(bgDelay: Double? = nil, f: @escaping () -> ChatResponse) -> ChatResponse {
    let endTask = beginBGTask()
    DispatchQueue.global().asyncAfter(deadline: .now() + maxTaskDuration, execute: endTask)
    let r = f()
    if let d = bgDelay {
        DispatchQueue.global().asyncAfter(deadline: .now() + d, execute: endTask)
    } else {
        endTask()
    }
    return r
}

func chatSendCmdSync(_ cmd: ChatCommand, bgTask: Bool = true, bgDelay: Double? = nil) -> ChatResponse {
    logger.debug("chatSendCmd \(cmd.cmdType)")
    let resp = bgTask
                ? withBGTask(bgDelay: bgDelay) { sendSimpleXCmd(cmd) }
                : sendSimpleXCmd(cmd)
    logger.debug("chatSendCmd \(cmd.cmdType): \(resp.responseType)")
    if case let .response(_, json) = resp {
        logger.debug("chatSendCmd \(cmd.cmdType) response: \(json)")
    }
    if case .apiParseMarkdown = cmd {} else {
        DispatchQueue.main.async {
            ChatModel.shared.terminalItems.append(.cmd(.now, cmd))
            ChatModel.shared.terminalItems.append(.resp(.now, resp))
        }
    }
    return resp
}

func chatSendCmd(_ cmd: ChatCommand, bgTask: Bool = true, bgDelay: Double? = nil) async -> ChatResponse {
    await withCheckedContinuation { cont in
        cont.resume(returning: chatSendCmdSync(cmd, bgTask: bgTask, bgDelay: bgDelay))
    }
}

func chatRecvMsg() async -> ChatResponse {
    await withCheckedContinuation { cont in
        _ = withBGTask(bgDelay: msgDelay) {
            let resp = chatResponse(chat_recv_msg(getChatCtrl())!)
            cont.resume(returning: resp)
            return resp
        }
    }
}

func apiGetActiveUser() throws -> User? {
    let _ = getChatCtrl()
    let r = chatSendCmdSync(.showActiveUser)
    switch r {
    case let .activeUser(user): return user
    case .chatCmdError(.error(.noActiveUser)): return nil
    default: throw r
    }
}

func apiCreateActiveUser(_ p: Profile) throws -> User {
    let r = chatSendCmdSync(.createActiveUser(profile: p))
    if case let .activeUser(user) = r { return user }
    throw r
}

func apiStartChat() throws {
    let r = chatSendCmdSync(.startChat)
    if case .chatStarted = r { return }
    throw r
}

func apiSetFilesFolder(filesFolder: String) throws {
    let r = chatSendCmdSync(.setFilesFolder(filesFolder: filesFolder))
    if case .cmdOk = r { return }
    throw r
}

func apiGetChats() throws -> [Chat] {
    let r = chatSendCmdSync(.apiGetChats)
    if case let .apiChats(chats) = r { return chats.map { Chat.init($0) } }
    throw r
}

func apiGetChat(type: ChatType, id: Int64) throws -> Chat {
    let r = chatSendCmdSync(.apiGetChat(type: type, id: id))
    if case let .apiChat(chat) = r { return Chat.init(chat) }
    throw r
}

func apiSendMessage(type: ChatType, id: Int64, file: String?, quotedItemId: Int64?, msg: MsgContent) async throws -> ChatItem {
    let chatModel = ChatModel.shared
    let cmd: ChatCommand = .apiSendMessage(type: type, id: id, file: file, quotedItemId: quotedItemId, msg: msg)
    let r: ChatResponse
    if type == .direct {
        var cItem: ChatItem!
        let endTask = beginBGTask({ if cItem != nil { chatModel.messageDelivery.removeValue(forKey: cItem.id) } })
        r = await chatSendCmd(cmd, bgTask: false)
        if case let .newChatItem(aChatItem) = r {
            cItem = aChatItem.chatItem
            chatModel.messageDelivery[cItem.id] = endTask
            return cItem
        }
        endTask()
    } else {
        r = await chatSendCmd(cmd, bgDelay: msgDelay)
        if case let .newChatItem(aChatItem) = r {
            return aChatItem.chatItem
        }
    }
    throw r
}

func apiUpdateChatItem(type: ChatType, id: Int64, itemId: Int64, msg: MsgContent) async throws -> ChatItem {
    let r = await chatSendCmd(.apiUpdateChatItem(type: type, id: id, itemId: itemId, msg: msg), bgDelay: msgDelay)
    if case let .chatItemUpdated(aChatItem) = r { return aChatItem.chatItem }
    throw r
}

func apiDeleteChatItem(type: ChatType, id: Int64, itemId: Int64, mode: CIDeleteMode) async throws -> ChatItem {
    let r = await chatSendCmd(.apiDeleteChatItem(type: type, id: id, itemId: itemId, mode: mode), bgDelay: msgDelay)
    if case let .chatItemDeleted(_, toChatItem) = r { return toChatItem.chatItem }
    throw r
}

func apiRegisterToken(token: String) async throws -> NtfTknStatus {
    let r = await chatSendCmd(.apiRegisterToken(token: token))
    if case let .ntfTokenStatus(status) = r { return status }
    throw r
}

func apiVerifyToken(token: String, code: String, nonce: String) async throws {
    try await sendCommandOkResp(.apiVerifyToken(token: token, code: code, nonce: nonce))
}

func apiIntervalNofication(token: String, interval: Int) async throws {
    try await sendCommandOkResp(.apiIntervalNofication(token: token, interval: interval))
}

func apiDeleteToken(token: String) async throws {
    try await sendCommandOkResp(.apiDeleteToken(token: token))
}

func getUserSMPServers() throws -> [String] {
    let r = chatSendCmdSync(.getUserSMPServers)
    if case let .userSMPServers(smpServers) = r { return smpServers }
    throw r
}

func setUserSMPServers(smpServers: [String]) async throws {
    try await sendCommandOkResp(.setUserSMPServers(smpServers: smpServers))
}

func apiAddContact() throws -> String {
    let r = chatSendCmdSync(.addContact, bgTask: false)
    if case let .invitation(connReqInvitation) = r { return connReqInvitation }
    throw r
}

func apiConnect(connReq: String) async throws -> ConnReqType? {
    let r = await chatSendCmd(.connect(connReq: connReq))
    let am = AlertManager.shared
    switch r {
    case .sentConfirmation: return .invitation
    case .sentInvitation: return .contact
    case let .contactAlreadyExists(contact):
        am.showAlertMsg(
            title: "Contact already exists",
            message: "You are already connected to \(contact.displayName) via this link."
        )
        return nil
    case .chatCmdError(.error(.invalidConnReq)):
        am.showAlertMsg(
            title: "Invalid connection link",
            message: "Please check that you used the correct link or ask your contact to send you another one."
        )
        return nil
    case .chatCmdError(.errorAgent(.BROKER(.TIMEOUT))):
        am.showAlertMsg(
            title: "Connection timeout",
            message: "Please check your network connection and try again."
        )
        return nil
    case .chatCmdError(.errorAgent(.BROKER(.NETWORK))):
        am.showAlertMsg(
            title: "Connection error",
            message: "Please check your network connection and try again."
        )
        return nil
    case .chatCmdError(.errorAgent(.SMP(.AUTH))):
        am.showAlertMsg(
            title: "Connection error (AUTH)",
            message: "Unless your contact deleted the connection or this link was already used, it might be a bug - please report it.\nTo connect, please ask your contact to create another connection link and check that you have a stable network connection."
        )
        return nil
    default: throw r
    }
}

func apiDeleteChat(type: ChatType, id: Int64) async throws {
    let r = await chatSendCmd(.apiDeleteChat(type: type, id: id), bgTask: false)
    if case .direct = type, case .contactDeleted = r { return }
    if case .contactConnection = type, case .contactConnectionDeleted = r { return }
    throw r
}

func apiUpdateProfile(profile: Profile) async throws -> Profile? {
    let r = await chatSendCmd(.apiUpdateProfile(profile: profile))
    switch r {
    case .userProfileNoChange: return nil
    case let .userProfileUpdated(_, toProfile): return toProfile
    default: throw r
    }
}

func apiParseMarkdown(text: String) throws -> [FormattedText]? {
    let r = chatSendCmdSync(.apiParseMarkdown(text: text))
    if case let .apiParsedMarkdown(formattedText) = r { return formattedText }
    throw r
}

func apiCreateUserAddress() async throws -> String {
    let r = await chatSendCmd(.createMyAddress)
    if case let .userContactLinkCreated(connReq) = r { return connReq }
    throw r
}

func apiDeleteUserAddress() async throws {
    let r = await chatSendCmd(.deleteMyAddress)
    if case .userContactLinkDeleted = r { return }
    throw r
}

func apiGetUserAddress() throws -> String? {
    let r = chatSendCmdSync(.showMyAddress)
    switch r {
    case let .userContactLink(connReq):
        return connReq
    case .chatCmdError(chatError: .errorStore(storeError: .userContactLinkNotFound)):
        return nil
    default: throw r
    }
}

func apiAcceptContactRequest(contactReqId: Int64) async throws -> Contact {
    let r = await chatSendCmd(.apiAcceptContact(contactReqId: contactReqId))
    if case let .acceptingContactRequest(contact) = r { return contact }
    throw r
}

func apiRejectContactRequest(contactReqId: Int64) async throws {
    let r = await chatSendCmd(.apiRejectContact(contactReqId: contactReqId))
    if case .contactRequestRejected = r { return }
    throw r
}

func apiChatRead(type: ChatType, id: Int64, itemRange: (Int64, Int64)) async throws {
    try await sendCommandOkResp(.apiChatRead(type: type, id: id, itemRange: itemRange))
}

func receiveFile(fileId: Int64) async throws {
    let r = await chatSendCmd(.receiveFile(fileId: fileId))
    if case .rcvFileAccepted = r { return }
    throw r
}

func acceptContactRequest(_ contactRequest: UserContactRequest) async {
    do {
        let contact = try await apiAcceptContactRequest(contactReqId: contactRequest.apiId)
        let chat = Chat(chatInfo: ChatInfo.direct(contact: contact), chatItems: [])
        DispatchQueue.main.async { ChatModel.shared.replaceChat(contactRequest.id, chat) }
    } catch let error {
        logger.error("acceptContactRequest error: \(responseError(error))")
    }
}

func rejectContactRequest(_ contactRequest: UserContactRequest) async {
    do {
        try await apiRejectContactRequest(contactReqId: contactRequest.apiId)
        DispatchQueue.main.async { ChatModel.shared.removeChat(contactRequest.id) }
    } catch let error {
        logger.error("rejectContactRequest: \(responseError(error))")
    }
}

func markChatRead(_ chat: Chat) async {
    do {
        let minItemId = chat.chatStats.minUnreadItemId
        let itemRange = (minItemId, chat.chatItems.last?.id ?? minItemId)
        let cInfo = chat.chatInfo
        try await apiChatRead(type: cInfo.chatType, id: cInfo.apiId, itemRange: itemRange)
        DispatchQueue.main.async { ChatModel.shared.markChatItemsRead(cInfo) }
    } catch {
        logger.error("markChatRead apiChatRead error: \(responseError(error))")
    }
}

func apiMarkChatItemRead(_ cInfo: ChatInfo, _ cItem: ChatItem) async {
    do {
        try await apiChatRead(type: cInfo.chatType, id: cInfo.apiId, itemRange: (cItem.id, cItem.id))
        DispatchQueue.main.async { ChatModel.shared.markChatItemRead(cInfo, cItem) }
    } catch {
        logger.error("markChatItemRead apiChatRead error: \(responseError(error))")
    }
}

private func sendCommandOkResp(_ cmd: ChatCommand) async throws {
    let r = await chatSendCmd(cmd)
    if case .cmdOk = r { return }
    throw r
}

func initializeChat() {
    do {
        ChatModel.shared.currentUser = try apiGetActiveUser()
    } catch {
        fatalError("Failed to initialize chat controller or database: \(error)")
    }
}

class ChatReceiver {
    private var receiveLoop: Task<Void, Never>?
    private var receiveMessages = true
    private var _lastMsgTime = Date.now

    static let shared = ChatReceiver()

    var lastMsgTime: Date { get { _lastMsgTime } }

    func start() {
        logger.debug("ChatReceiver.start")
        receiveMessages = true
        _lastMsgTime = .now
        if receiveLoop != nil { return }
        receiveLoop = Task { await receiveMsgLoop() }
    }

    func receiveMsgLoop() async {
        let msg = await chatRecvMsg()
        self._lastMsgTime = .now
        processReceivedMsg(msg)
        if self.receiveMessages {
            do { try await Task.sleep(nanoseconds: 7_500_000) }
            catch { logger.error("receiveMsgLoop: Task.sleep error: \(error.localizedDescription)") }
            await receiveMsgLoop()
        }
    }

    func stop() {
        logger.debug("ChatReceiver.stop")
        receiveMessages = false
        receiveLoop?.cancel()
        receiveLoop = nil
    }
}

func processReceivedMsg(_ res: ChatResponse) {
    let m = ChatModel.shared
    DispatchQueue.main.async {
        m.terminalItems.append(.resp(.now, res))
        logger.debug("processReceivedMsg: \(res.responseType)")
        switch res {
        case let .newContactConnection(connection):
            m.updateContactConnection(connection)
        case let .contactConnectionDeleted(connection):
            m.removeChat(connection.id)
        case let .contactConnected(contact):
            m.updateContact(contact)
            m.removeChat(contact.activeConn.id)
            m.updateNetworkStatus(contact.id, .connected)
            NtfManager.shared.notifyContactConnected(contact)
        case let .contactConnecting(contact):
            m.updateContact(contact)
            m.removeChat(contact.activeConn.id)
        case let .receivedContactRequest(contactRequest):
            m.addChat(Chat(
                chatInfo: ChatInfo.contactRequest(contactRequest: contactRequest),
                chatItems: []
            ))
            NtfManager.shared.notifyContactRequest(contactRequest)
        case let .contactUpdated(toContact):
            let cInfo = ChatInfo.direct(contact: toContact)
            if m.hasChat(toContact.id) {
                m.updateChatInfo(cInfo)
            }
        case let .contactsSubscribed(_, contactRefs):
            updateContactsStatus(contactRefs, status: .connected)
        case let .contactsDisconnected(_, contactRefs):
            updateContactsStatus(contactRefs, status: .disconnected)
        case let .contactSubError(contact, chatError):
            processContactSubError(contact, chatError)
        case let .contactSubSummary(contactSubscriptions):
            for sub in contactSubscriptions {
                if let err = sub.contactError {
                    processContactSubError(sub.contact, err)
                } else {
                    m.updateContact(sub.contact)
                    m.updateNetworkStatus(sub.contact.id, .connected)
                }
            }
        case let .newChatItem(aChatItem):
            let cInfo = aChatItem.chatInfo
            let cItem = aChatItem.chatItem
            m.addChatItem(cInfo, cItem)
            if let file = cItem.file,
               file.fileSize <= maxImageSize {
                Task {
                    do {
                        try await receiveFile(fileId: file.fileId)
                    } catch {
                        logger.error("receiveFile error: \(error.localizedDescription)")
                    }
                }
            }
            NtfManager.shared.notifyMessageReceived(cInfo, cItem)
        case let .chatItemStatusUpdated(aChatItem):
            let cInfo = aChatItem.chatInfo
            let cItem = aChatItem.chatItem
            var res = false
            if !cItem.isDeletedContent() {
                res = m.upsertChatItem(cInfo, cItem)
            }
            if res {
                NtfManager.shared.notifyMessageReceived(cInfo, cItem)
            } else if let endTask = m.messageDelivery[cItem.id] {
                switch cItem.meta.itemStatus {
                case .sndSent: endTask()
                case .sndErrorAuth: endTask()
                case .sndError: endTask()
                default: break
                }
            }
        case let .chatItemUpdated(aChatItem):
            let cInfo = aChatItem.chatInfo
            let cItem = aChatItem.chatItem
            if m.upsertChatItem(cInfo, cItem) {
                NtfManager.shared.notifyMessageReceived(cInfo, cItem)
            }
        case let .chatItemDeleted(_, toChatItem):
            let cInfo = toChatItem.chatInfo
            let cItem = toChatItem.chatItem
            if cItem.meta.itemDeleted {
                m.removeChatItem(cInfo, cItem)
            } else {
                // currently only broadcast deletion of rcv message can be received, and only this case should happen
                _ = m.upsertChatItem(cInfo, cItem)
            }
        case let .rcvFileComplete(aChatItem):
            let cInfo = aChatItem.chatInfo
            let cItem = aChatItem.chatItem
            if m.upsertChatItem(cInfo, cItem) {
                NtfManager.shared.notifyMessageReceived(cInfo, cItem)
            }
        default:
            logger.debug("unsupported event: \(res.responseType)")
        }
    }
}

func updateContactsStatus(_ contactRefs: [ContactRef], status: Chat.NetworkStatus) {
    let m = ChatModel.shared
    for c in contactRefs {
        m.updateNetworkStatus(c.id, status)
    }
}

func processContactSubError(_ contact: Contact, _ chatError: ChatError) {
    let m = ChatModel.shared
    m.updateContact(contact)
    var err: String
    switch chatError {
    case .errorAgent(agentError: .BROKER(brokerErr: .NETWORK)): err = "network"
    case .errorAgent(agentError: .SMP(smpErr: .AUTH)): err = "contact deleted"
    default: err = String(describing: chatError)
    }
    m.updateNetworkStatus(contact.id, .error(err))
}

private struct UserResponse: Decodable {
    var user: User?
    var error: String?
}
