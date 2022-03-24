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
private let jsonDecoder = getJSONDecoder()
private let jsonEncoder = getJSONEncoder()

enum ChatCommand {
    case showActiveUser
    case createActiveUser(profile: Profile)
    case startChat
    case apiGetChats
    case apiGetChat(type: ChatType, id: Int64)
    case apiSendMessage(type: ChatType, id: Int64, msg: MsgContent)
    case apiSendMessageQuote(type: ChatType, id: Int64, itemId: Int64, msg: MsgContent)
    case getUserSMPServers
    case setUserSMPServers(smpServers: [String])
    case addContact
    case connect(connReq: String)
    case apiDeleteChat(type: ChatType, id: Int64)
    case updateProfile(profile: Profile)
    case updateProfileImage(image: String?)
    case createMyAddress
    case deleteMyAddress
    case showMyAddress
    case apiAcceptContact(contactReqId: Int64)
    case apiRejectContact(contactReqId: Int64)
    case apiChatRead(type: ChatType, id: Int64, itemRange: (Int64, Int64))
    case string(String)

    var cmdString: String {
        get {
            switch self {
            case .showActiveUser: return "/u"
            case let .createActiveUser(profile): return "/u \(profile.displayName) \(profile.fullName)"
            case .startChat: return "/_start"
            case .apiGetChats: return "/_get chats"
            case let .apiGetChat(type, id): return "/_get chat \(ref(type, id)) count=100"
            case let .apiSendMessage(type, id, mc): return "/_send \(ref(type, id)) \(mc.cmdString)"
            case let .apiSendMessageQuote(type, id, itemId, mc): return "/_send_quote \(ref(type, id)) \(itemId) \(mc.cmdString)"
            case .getUserSMPServers: return "/smp_servers"
            case let .setUserSMPServers(smpServers): return "/smp_servers \(smpServersStr(smpServers: smpServers))"
            case .addContact: return "/connect"
            case let .connect(connReq): return "/connect \(connReq)"
            case let .apiDeleteChat(type, id): return "/_delete \(ref(type, id))"
            case let .updateProfile(profile): return "/profile \(profile.displayName) \(profile.fullName)"
            case let .updateProfileImage(image): return image == nil ? "/profile_image" : "/profile_image \(image!)"
//            with the updated core:
//            case let .updateProfile(profile): return "/_profile \(encodeJSON(profile))"
            case .createMyAddress: return "/address"
            case .deleteMyAddress: return "/delete_address"
            case .showMyAddress: return "/show_address"
            case let .apiAcceptContact(contactReqId): return "/_accept \(contactReqId)"
            case let .apiRejectContact(contactReqId): return "/_reject \(contactReqId)"
            case let .apiChatRead(type, id, itemRange: (from, to)): return "/_read chat \(ref(type, id)) from=\(from) to=\(to)"
            case let .string(str): return str
            }
        }
    }

    var cmdType: String {
        get {
            switch self {
            case .showActiveUser: return "showActiveUser"
            case .createActiveUser: return "createActiveUser"
            case .startChat: return "startChat"
            case .apiGetChats: return "apiGetChats"
            case .apiGetChat: return "apiGetChat"
            case .apiSendMessage: return "apiSendMessage"
            case .apiSendMessageQuote: return "apiSendMessageQuote"
            case .getUserSMPServers: return "getUserSMPServers"
            case .setUserSMPServers: return "setUserSMPServers"
            case .addContact: return "addContact"
            case .connect: return "connect"
            case .apiDeleteChat: return "apiDeleteChat"
            case .updateProfile: return "updateProfile"
            case .updateProfileImage: return "updateProfileImage"
            case .createMyAddress: return "createMyAddress"
            case .deleteMyAddress: return "deleteMyAddress"
            case .showMyAddress: return "showMyAddress"
            case .apiAcceptContact: return "apiAcceptContact"
            case .apiRejectContact: return "apiRejectContact"
            case .apiChatRead: return "apiChatRead"
            case .string: return "console command"
            }
        }
    }

    func ref(_ type: ChatType, _ id: Int64) -> String {
        "\(type.rawValue)\(id)"
    }
    
    func smpServersStr(smpServers: [String]) -> String {
        smpServers.isEmpty ? "default" : smpServers.joined(separator: ",")
    }
}

struct APIResponse: Decodable {
    var resp: ChatResponse
}

enum ChatResponse: Decodable, Error {
    case response(type: String, json: String)
    case activeUser(user: User)
    case chatStarted
    case chatRunning
    case apiChats(chats: [ChatData])
    case apiChat(chat: ChatData)
    case userSMPServers(smpServers: [String])
    case invitation(connReqInvitation: String)
    case sentConfirmation
    case sentInvitation
    case contactDeleted(contact: Contact)
    case userProfileNoChange
    case userProfileUpdated(fromProfile: Profile, toProfile: Profile)
    case userContactLink(connReqContact: String)
    case userContactLinkCreated(connReqContact: String)
    case userContactLinkDeleted
    case contactConnected(contact: Contact)
    case receivedContactRequest(contactRequest: UserContactRequest)
    case acceptingContactRequest(contact: Contact)
    case contactRequestRejected
    case contactUpdated(toContact: Contact)
    case contactSubscribed(contact: Contact)
    case contactDisconnected(contact: Contact)
    case contactSubError(contact: Contact, chatError: ChatError)
    case contactSubSummary(contactSubscriptions: [ContactSubStatus])
    case groupSubscribed(groupInfo: GroupInfo)
    case memberSubErrors(memberSubErrors: [MemberSubError])
    case groupEmpty(groupInfo: GroupInfo)
    case userContactLinkSubscribed
    case newChatItem(chatItem: AChatItem)
    case chatItemUpdated(chatItem: AChatItem)
    case cmdOk
    case chatCmdError(chatError: ChatError)
    case chatError(chatError: ChatError)

    var responseType: String {
        get {
            switch self {
            case let .response(type, _): return "* \(type)"
            case .activeUser: return "activeUser"
            case .chatStarted: return "chatStarted"
            case .chatRunning: return "chatRunning"
            case .apiChats: return "apiChats"
            case .apiChat: return "apiChat"
            case .userSMPServers: return "userSMPServers"
            case .invitation: return "invitation"
            case .sentConfirmation: return "sentConfirmation"
            case .sentInvitation: return "sentInvitation"
            case .contactDeleted: return "contactDeleted"
            case .userProfileNoChange: return "userProfileNoChange"
            case .userProfileUpdated: return "userProfileNoChange"
            case .userContactLink: return "userContactLink"
            case .userContactLinkCreated: return "userContactLinkCreated"
            case .userContactLinkDeleted: return "userContactLinkDeleted"
            case .contactConnected: return "contactConnected"
            case .receivedContactRequest: return "receivedContactRequest"
            case .acceptingContactRequest: return "acceptingContactRequest"
            case .contactRequestRejected: return "contactRequestRejected"
            case .contactUpdated: return "contactUpdated"
            case .contactSubscribed: return "contactSubscribed"
            case .contactDisconnected: return "contactDisconnected"
            case .contactSubError: return "contactSubError"
            case .contactSubSummary: return "contactSubSummary"
            case .groupSubscribed: return "groupSubscribed"
            case .memberSubErrors: return "memberSubErrors"
            case .groupEmpty: return "groupEmpty"
            case .userContactLinkSubscribed: return "userContactLinkSubscribed"
            case .newChatItem: return "newChatItem"
            case .chatItemUpdated: return "chatItemUpdated"
            case .cmdOk: return "cmdOk"
            case .chatCmdError: return "chatCmdError"
            case .chatError: return "chatError"
            }
        }
    }
    
    var details: String {
        get {
            switch self {
            case let .response(_, json): return json
            case let .activeUser(user): return String(describing: user)
            case .chatStarted: return noDetails
            case .chatRunning: return noDetails
            case let .apiChats(chats): return String(describing: chats)
            case let .apiChat(chat): return String(describing: chat)
            case let .userSMPServers(smpServers): return String(describing: smpServers)
            case let .invitation(connReqInvitation): return connReqInvitation
            case .sentConfirmation: return noDetails
            case .sentInvitation: return noDetails
            case let .contactDeleted(contact): return String(describing: contact)
            case .userProfileNoChange: return noDetails
            case let .userProfileUpdated(_, toProfile): return String(describing: toProfile)
            case let .userContactLink(connReq): return connReq
            case let .userContactLinkCreated(connReq): return connReq
            case .userContactLinkDeleted: return noDetails
            case let .contactConnected(contact): return String(describing: contact)
            case let .receivedContactRequest(contactRequest): return String(describing: contactRequest)
            case let .acceptingContactRequest(contact): return String(describing: contact)
            case .contactRequestRejected: return noDetails
            case let .contactUpdated(toContact): return String(describing: toContact)
            case let .contactSubscribed(contact): return String(describing: contact)
            case let .contactDisconnected(contact): return String(describing: contact)
            case let .contactSubError(contact, chatError): return "contact:\n\(String(describing: contact))\nerror:\n\(String(describing: chatError))"
            case let .contactSubSummary(contactSubscriptions): return String(describing: contactSubscriptions)
            case let .groupSubscribed(groupInfo): return String(describing: groupInfo)
            case let .memberSubErrors(memberSubErrors): return String(describing: memberSubErrors)
            case let .groupEmpty(groupInfo): return String(describing: groupInfo)
            case .userContactLinkSubscribed: return noDetails
            case let .newChatItem(chatItem): return String(describing: chatItem)
            case let .chatItemUpdated(chatItem): return String(describing: chatItem)
            case .cmdOk: return noDetails
            case let .chatCmdError(chatError): return String(describing: chatError)
            case let .chatError(chatError): return String(describing: chatError)
            }
        }
    }

    private var noDetails: String { get { "\(responseType): no details" } }
}

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

private func _sendCmd(_ cmd: ChatCommand) -> ChatResponse {
    var c = cmd.cmdString.cString(using: .utf8)!
    return chatResponse(chat_send_cmd(getChatCtrl(), &c))
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
                ? withBGTask(bgDelay: bgDelay) { _sendCmd(cmd) }
                : _sendCmd(cmd)
    logger.debug("chatSendCmd \(cmd.cmdType): \(resp.responseType)")
    if case let .response(_, json) = resp {
        logger.debug("chatSendCmd \(cmd.cmdType) response: \(json)")
    }
    DispatchQueue.main.async {
        ChatModel.shared.terminalItems.append(.cmd(.now, cmd))
        ChatModel.shared.terminalItems.append(.resp(.now, resp))
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

func apiSendMessage(type: ChatType, id: Int64, quotedItemId: Int64?, msg: MsgContent) async throws -> ChatItem {
    let chatModel = ChatModel.shared
    let cmd: ChatCommand
    if let itemId = quotedItemId {
        cmd = .apiSendMessageQuote(type: type, id: id, itemId: itemId, msg: msg)
    } else {
        cmd = .apiSendMessage(type: type, id: id, msg: msg)
    }
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

func getUserSMPServers() throws -> [String] {
    let r = chatSendCmdSync(.getUserSMPServers)
    if case let .userSMPServers(smpServers) = r { return smpServers }
    throw r
}

func setUserSMPServers(smpServers: [String]) async throws {
    let r = await chatSendCmd(.setUserSMPServers(smpServers: smpServers))
    if case .cmdOk = r { return }
    throw r
}

func apiAddContact() throws -> String {
    let r = chatSendCmdSync(.addContact, bgTask: false)
    if case let .invitation(connReqInvitation) = r { return connReqInvitation }
    throw r
}

func apiConnect(connReq: String) async throws {
    let r = await chatSendCmd(.connect(connReq: connReq))
    switch r {
    case .sentConfirmation: return
    case .sentInvitation: return
    default: throw r
    }
}

func apiDeleteChat(type: ChatType, id: Int64) async throws {
    let r = await chatSendCmd(.apiDeleteChat(type: type, id: id), bgTask: false)
    if case .contactDeleted = r { return }
    throw r
}

func apiUpdateProfile(profile: Profile) async throws -> Profile {
    let r = await chatSendCmd(.updateProfile(profile: profile))
    switch r {
    case .userProfileNoChange: return profile
    case let .userProfileUpdated(_, toProfile): return toProfile
    default: throw r
    }
}

func apiUpdateProfileImage(image: String?) async throws {
    let r = await chatSendCmd(.updateProfileImage(image: image))
    switch r {
    case .userProfileNoChange: return
    case .userProfileUpdated: return
    default: throw r
    }
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

func apiGetUserAddress() async throws -> String? {
    let r = await chatSendCmd(.showMyAddress)
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
    let r = await chatSendCmd(.apiChatRead(type: type, id: id, itemRange: itemRange))
    if case .cmdOk = r { return }
    throw r
}

func acceptContactRequest(_ contactRequest: UserContactRequest) async {
    do {
        let contact = try await apiAcceptContactRequest(contactReqId: contactRequest.apiId)
        let chat = Chat(chatInfo: ChatInfo.direct(contact: contact), chatItems: [])
        DispatchQueue.main.async { ChatModel.shared.replaceChat(contactRequest.id, chat) }
    } catch let error {
        logger.error("acceptContactRequest error: \(error.localizedDescription)")
    }
}

func rejectContactRequest(_ contactRequest: UserContactRequest) async {
    do {
        try await apiRejectContactRequest(contactReqId: contactRequest.apiId)
        DispatchQueue.main.async { ChatModel.shared.removeChat(contactRequest.id) }
    } catch let error {
        logger.error("rejectContactRequest: \(error.localizedDescription)")
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
        logger.error("markChatRead apiChatRead error: \(error.localizedDescription)")
    }
}

func markChatItemRead(_ cInfo: ChatInfo, _ cItem: ChatItem) async {
    do {
        try await apiChatRead(type: cInfo.chatType, id: cInfo.apiId, itemRange: (cItem.id, cItem.id))
        DispatchQueue.main.async { ChatModel.shared.markChatItemRead(cInfo, cItem) }
    } catch {
        logger.error("markChatItemRead apiChatRead error: \(error.localizedDescription)")
    }
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
    let chatModel = ChatModel.shared
    DispatchQueue.main.async {
        chatModel.terminalItems.append(.resp(.now, res))
        logger.debug("processReceivedMsg: \(res.responseType)")
        switch res {
        case let .contactConnected(contact):
            chatModel.updateContact(contact)
            chatModel.updateNetworkStatus(contact, .connected)
            NtfManager.shared.notifyContactConnected(contact)
        case let .receivedContactRequest(contactRequest):
            chatModel.addChat(Chat(
                chatInfo: ChatInfo.contactRequest(contactRequest: contactRequest),
                chatItems: []
            ))
            NtfManager.shared.notifyContactRequest(contactRequest)
        case let .contactUpdated(toContact):
            let cInfo = ChatInfo.direct(contact: toContact)
            if chatModel.hasChat(toContact.id) {
                chatModel.updateChatInfo(cInfo)
            }
        case let .contactSubscribed(contact):
            processContactSubscribed(contact)
        case let .contactDisconnected(contact):
            chatModel.updateContact(contact)
            chatModel.updateNetworkStatus(contact, .disconnected)
        case let .contactSubError(contact, chatError):
            processContactSubError(contact, chatError)
        case let .contactSubSummary(contactSubscriptions):
            for sub in contactSubscriptions {
                if let err = sub.contactError {
                    processContactSubError(sub.contact, err)
                } else {
                    processContactSubscribed(sub.contact)
                }
            }
        case let .newChatItem(aChatItem):
            let cInfo = aChatItem.chatInfo
            let cItem = aChatItem.chatItem
            chatModel.addChatItem(cInfo, cItem)
            NtfManager.shared.notifyMessageReceived(cInfo, cItem)
        case let .chatItemUpdated(aChatItem):
            let cInfo = aChatItem.chatInfo
            let cItem = aChatItem.chatItem
            if chatModel.upsertChatItem(cInfo, cItem) {
                NtfManager.shared.notifyMessageReceived(cInfo, cItem)
            } else if let endTask = chatModel.messageDelivery[cItem.id] {
                switch cItem.meta.itemStatus {
                case .sndSent: endTask()
                case .sndErrorAuth: endTask()
                case .sndError: endTask()
                default: break
                }
            }
        default:
            logger.debug("unsupported event: \(res.responseType)")
        }
    }
}

func processContactSubscribed(_ contact: Contact) {
    let m = ChatModel.shared
    m.updateContact(contact)
    m.updateNetworkStatus(contact, .connected)
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
    m.updateNetworkStatus(contact, .error(err))
}

private struct UserResponse: Decodable {
    var user: User?
    var error: String?
}

private func chatResponse(_ cjson: UnsafeMutablePointer<CChar>) -> ChatResponse {
    let s = String.init(cString: cjson)
    let d = s.data(using: .utf8)!
// TODO is there a way to do it without copying the data? e.g:
//    let p = UnsafeMutableRawPointer.init(mutating: UnsafeRawPointer(cjson))
//    let d = Data.init(bytesNoCopy: p, count: strlen(cjson), deallocator: .free)

// TODO some mechanism to update model without passing it - maybe Publisher / Subscriber?

    do {
        let r = try jsonDecoder.decode(APIResponse.self, from: d)
        return r.resp
    } catch {
        logger.error("chatResponse jsonDecoder.decode error: \(error.localizedDescription)")
    }
        
    var type: String?
    var json: String?
    if let j = try? JSONSerialization.jsonObject(with: d) as? NSDictionary {
        if let j1 = j["resp"] as? NSDictionary, j1.count == 1 {
            type = j1.allKeys[0] as? String
        }
        json = prettyJSON(j)
    }
    free(cjson)
    return ChatResponse.response(type: type ?? "invalid", json: json ?? s)
}

func prettyJSON(_ obj: NSDictionary) -> String? {
    if let d = try? JSONSerialization.data(withJSONObject: obj, options: .prettyPrinted) {
        return String(decoding: d, as: UTF8.self)
    }
    return nil
}

private func getChatCtrl() -> chat_ctrl {
    if let controller = chatController { return controller }
    let dataDir = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.path + "/mobile_v1"
    var cstr = dataDir.cString(using: .utf8)!
    logger.debug("getChatCtrl: chat_init")
    ChatModel.shared.terminalItems.append(.cmd(.now, .string("chat_init")))
    chatController = chat_init(&cstr)
    ChatModel.shared.terminalItems.append(.resp(.now, .response(type: "chat_controller", json: "chat_controller: no details")))
    return chatController!
}

private func decodeCJSON<T: Decodable>(_ cjson: UnsafePointer<CChar>) -> T? {
    let s = String.init(cString: cjson)
    let d = s.data(using: .utf8)!
//    let p = UnsafeMutableRawPointer.init(mutating: UnsafeRawPointer(cjson))
//    let d = Data.init(bytesNoCopy: p, count: strlen(cjson), deallocator: .free)
    return try? jsonDecoder.decode(T.self, from: d)
}

private func getJSONObject(_ cjson: UnsafePointer<CChar>) -> NSDictionary? {
    let s = String.init(cString: cjson)
    let d = s.data(using: .utf8)!
    return try? JSONSerialization.jsonObject(with: d) as? NSDictionary
}

private func encodeJSON<T: Encodable>(_ value: T) -> String {
    let data = try! jsonEncoder.encode(value)
    return String(decoding: data, as: UTF8.self)
}

private func encodeCJSON<T: Encodable>(_ value: T) -> [CChar] {
    encodeJSON(value).cString(using: .utf8)!
}

enum ChatError: Decodable {
    case error(errorType: ChatErrorType)
    case errorAgent(agentError: AgentErrorType)
    case errorStore(storeError: StoreError)
}

enum ChatErrorType: Decodable {
    case noActiveUser
    case activeUserExists
    case chatNotStarted
    case invalidConnReq
    case invalidChatMessage(message: String)
    case contactNotReady(contact: Contact)
    case contactGroups(contact: Contact, groupNames: [GroupName])
    case groupUserRole
    case groupContactRole(contactName: ContactName)
    case groupDuplicateMember(contactName: ContactName)
    case groupDuplicateMemberId
    case groupNotJoined(groupInfo: GroupInfo)
    case groupMemberNotActive
    case groupMemberUserRemoved
    case groupMemberNotFound(contactName: ContactName)
    case groupMemberIntroNotFound(contactName: ContactName)
    case groupCantResendInvitation(groupInfo: GroupInfo, contactName: ContactName)
    case groupInternal(message: String)
    case fileNotFound(message: String)
    case fileAlreadyReceiving(message: String)
    case fileAlreadyExists(filePath: String)
    case fileRead(filePath: String, message: String)
    case fileWrite(filePath: String, message: String)
    case fileSend(fileId: Int64, agentError: String)
    case fileRcvChunk(message: String)
    case fileInternal(message: String)
    case agentVersion
    case commandError(message: String)
}

enum StoreError: Decodable {
    case duplicateName
    case contactNotFound(contactId: Int64)
    case contactNotFoundByName(contactName: ContactName)
    case contactNotReady(contactName: ContactName)
    case duplicateContactLink
    case userContactLinkNotFound
    case contactRequestNotFound(contactRequestId: Int64)
    case contactRequestNotFoundByName(contactName: ContactName)
    case groupNotFound(groupId: Int64)
    case groupNotFoundByName(groupName: GroupName)
    case groupWithoutUser
    case duplicateGroupMember
    case groupAlreadyJoined
    case groupInvitationNotFound
    case sndFileNotFound(fileId: Int64)
    case sndFileInvalid(fileId: Int64)
    case rcvFileNotFound(fileId: Int64)
    case fileNotFound(fileId: Int64)
    case rcvFileInvalid(fileId: Int64)
    case connectionNotFound(agentConnId: String)
    case introNotFound
    case uniqueID
    case internalError(message: String)
    case noMsgDelivery(connId: Int64, agentMsgId: String)
    case badChatItem(itemId: Int64)
    case chatItemNotFound(itemId: Int64)
}

enum AgentErrorType: Decodable {
    case CMD(cmdErr: CommandErrorType)
    case CONN(connErr: ConnectionErrorType)
    case SMP(smpErr: SMPErrorType)
    case BROKER(brokerErr: BrokerErrorType)
    case AGENT(agentErr: SMPAgentError)
    case INTERNAL(internalErr: String)
}

enum CommandErrorType: Decodable {
    case PROHIBITED
    case SYNTAX
    case NO_CONN
    case SIZE
    case LARGE
}

enum ConnectionErrorType: Decodable {
    case NOT_FOUND
    case DUPLICATE
    case SIMPLEX
    case NOT_ACCEPTED
    case NOT_AVAILABLE
}

enum BrokerErrorType: Decodable {
    case RESPONSE(smpErr: SMPErrorType)
    case UNEXPECTED
    case NETWORK
    case TRANSPORT(transportErr: SMPTransportError)
    case TIMEOUT
}

enum SMPErrorType: Decodable {
    case BLOCK
    case SESSION
    case CMD(cmdErr: SMPCommandError)
    case AUTH
    case QUOTA
    case NO_MSG
    case LARGE_MSG
    case INTERNAL
}

enum SMPCommandError: Decodable {
    case UNKNOWN
    case SYNTAX
    case NO_AUTH
    case HAS_AUTH
    case NO_QUEUE
}

enum SMPTransportError: Decodable {
    case badBlock
    case largeMsg
    case badSession
    case handshake(handshakeErr: SMPHandshakeError)
}

enum SMPHandshakeError: Decodable {
    case PARSE
    case VERSION
    case IDENTITY
}

enum SMPAgentError: Decodable {
    case A_MESSAGE
    case A_PROHIBITED
    case A_VERSION
    case A_ENCRYPTION
}
