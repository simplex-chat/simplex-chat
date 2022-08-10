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
import SwiftUI
import SimpleXChat

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

func beginBGTask(_ handler: (() -> Void)? = nil) -> (() -> Void) {
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

private func withBGTask<T>(bgDelay: Double? = nil, f: @escaping () -> T) -> T {
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

func chatRecvMsg() async -> ChatResponse? {
    await withCheckedContinuation { cont in
        _  = withBGTask(bgDelay: msgDelay) { () -> ChatResponse? in
            let resp = recvSimpleXMsg()
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

func apiStartChat() throws -> Bool {
    let r = chatSendCmdSync(.startChat(subscribe: true))
    switch r {
    case .chatStarted: return true
    case .chatRunning: return false
    default: throw r
    }
}

func apiStopChat() async throws {
    let r = await chatSendCmd(.apiStopChat)
    switch r {
    case .chatStopped: return
    default: throw r
    }
}

func apiActivateChat() {
    let r = chatSendCmdSync(.apiActivateChat)
    if case .cmdOk = r { return }
    logger.error("apiActivateChat error: \(String(describing: r))")
}

func apiSuspendChat(timeoutMicroseconds: Int) {
    let r = chatSendCmdSync(.apiSuspendChat(timeoutMicroseconds: timeoutMicroseconds))
    if case .cmdOk = r { return }
    logger.error("apiSuspendChat error: \(String(describing: r))")
}

func apiSetFilesFolder(filesFolder: String) throws {
    let r = chatSendCmdSync(.setFilesFolder(filesFolder: filesFolder))
    if case .cmdOk = r { return }
    throw r
}

func apiExportArchive(config: ArchiveConfig) async throws {
    try await sendCommandOkResp(.apiExportArchive(config: config))
}

func apiImportArchive(config: ArchiveConfig) async throws {
    try await sendCommandOkResp(.apiImportArchive(config: config))
}

func apiDeleteStorage() async throws {
    try await sendCommandOkResp(.apiDeleteStorage)
}

func apiGetChats() throws -> [ChatData] {
    let r = chatSendCmdSync(.apiGetChats)
    if case let .apiChats(chats) = r { return chats }
    throw r
}

func apiGetChat(type: ChatType, id: Int64, pagination: ChatPagination = .last(count: 100)) throws -> Chat {
    let r = chatSendCmdSync(.apiGetChat(type: type, id: id, pagination: pagination))
    if case let .apiChat(chat) = r { return Chat.init(chat) }
    throw r
}

func loadChat(chat: Chat) {
    do {
        let cInfo = chat.chatInfo
        let chat = try apiGetChat(type: cInfo.chatType, id: cInfo.apiId)
        let m = ChatModel.shared
        m.updateChatInfo(chat.chatInfo)
        m.chatItems = chat.chatItems
    } catch let error {
        logger.error("loadChat error: \(responseError(error))")
    }
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

func apiGetNtfToken() -> (DeviceToken?, NtfTknStatus?, NotificationsMode) {
    let r = chatSendCmdSync(.apiGetNtfToken)
    switch r {
    case let .ntfToken(token, status, ntfMode): return (token, status, ntfMode)
    case .chatCmdError(.errorAgent(.CMD(.PROHIBITED))): return (nil, nil, .off)
    default:
        logger.debug("apiGetNtfToken response: \(String(describing: r), privacy: .public)")
        return (nil, nil, .off)
    }
}

func apiRegisterToken(token: DeviceToken, notificationMode: NotificationsMode) async throws -> NtfTknStatus {
    let r = await chatSendCmd(.apiRegisterToken(token: token, notificationMode: notificationMode))
    if case let .ntfTokenStatus(status) = r { return status }
    throw r
}

func registerToken(token: DeviceToken) {
    let m = ChatModel.shared
    let mode = m.notificationMode
    if mode != .off && !m.tokenRegistered {
        m.tokenRegistered = true
        logger.debug("registerToken \(mode.rawValue)")
        Task {
            do {
                let status = try await apiRegisterToken(token: token, notificationMode: mode)
                await MainActor.run { m.tokenStatus = status }
            } catch let error {
                logger.error("registerToken apiRegisterToken error: \(responseError(error))")
            }
        }
    }
}

func apiVerifyToken(token: DeviceToken, nonce: String, code: String) async throws {
    try await sendCommandOkResp(.apiVerifyToken(token: token, nonce: nonce, code: code))
}

func apiDeleteToken(token: DeviceToken) async throws {
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

func getNetworkConfig() async throws -> NetCfg? {
    let r = await chatSendCmd(.apiGetNetworkConfig)
    if case let .networkConfig(cfg) = r { return cfg }
    throw r
}

func setNetworkConfig(_ cfg: NetCfg) throws {
    let r = chatSendCmdSync(.apiSetNetworkConfig(networkConfig: cfg))
    if case .cmdOk = r { return }
    throw r
}

func apiContactInfo(contactId: Int64) async throws -> ConnectionStats? {
    let r = await chatSendCmd(.apiContactInfo(contactId: contactId))
    if case let .contactInfo(_, connStats) = r { return connStats }
    throw r
}

func apiGroupMemberInfo(_ groupId: Int64, _ groupMemberId: Int64) async throws -> ConnectionStats? {
    let r = await chatSendCmd(.apiGroupMemberInfo(groupId: groupId, groupMemberId: groupMemberId))
    if case let .groupMemberInfo(_, _, connStats_) = r { return connStats_ }
    throw r
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
    case let .chatCmdError(.errorAgent(.INTERNAL(internalErr))):
        if internalErr == "SEUniqueID" {
            am.showAlertMsg(
                title: "Already connected?",
                message: "It seems like you are already connected via this link. If it is not the case, there was an error (\(responseError(r)))."
            )
            return nil
        } else {
            throw r
        }
    default: throw r
    }
}

func apiDeleteChat(type: ChatType, id: Int64) async throws {
    let r = await chatSendCmd(.apiDeleteChat(type: type, id: id), bgTask: false)
    if case .direct = type, case .contactDeleted = r { return }
    if case .contactConnection = type, case .contactConnectionDeleted = r { return }
    if case .group = type, case .groupDeletedUser = r { return }
    throw r
}

func deleteChat(_ chat: Chat) async {
    do {
        let cInfo = chat.chatInfo
        try await apiDeleteChat(type: cInfo.chatType, id: cInfo.apiId)
        DispatchQueue.main.async { ChatModel.shared.removeChat(cInfo.id) }
    } catch {
        logger.error("deleteChat apiDeleteChat error: \(responseError(error))")
    }
}

func apiClearChat(type: ChatType, id: Int64) async throws -> ChatInfo {
    let r = await chatSendCmd(.apiClearChat(type: type, id: id), bgTask: false)
    if case let .chatCleared(updatedChatInfo) = r { return updatedChatInfo }
    throw r
}

func clearChat(_ chat: Chat) async {
    do {
        let cInfo = chat.chatInfo
        let updatedChatInfo = try await apiClearChat(type: cInfo.chatType, id: cInfo.apiId)
        DispatchQueue.main.async { ChatModel.shared.clearChat(updatedChatInfo) }
    } catch {
        logger.error("clearChat apiClearChat error: \(responseError(error))")
    }
}

func apiListContacts() throws -> [Contact] {
    let r = chatSendCmdSync(.listContacts)
    if case let .contactsList(contacts) = r { return contacts }
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

func receiveFile(fileId: Int64) async {
    do {
        let chatItem = try await apiReceiveFile(fileId: fileId)
        DispatchQueue.main.async { chatItemSimpleUpdate(chatItem) }
    } catch let error {
        logger.error("receiveFile error: \(responseError(error))")
    }
}

func apiReceiveFile(fileId: Int64) async throws -> AChatItem {
    let r = await chatSendCmd(.receiveFile(fileId: fileId))
    if case let .rcvFileAccepted(chatItem) = r { return chatItem }
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

func apiSendCallInvitation(_ contact: Contact, _ callType: CallType) async throws {
    try await sendCommandOkResp(.apiSendCallInvitation(contact: contact, callType: callType))
}

func apiRejectCall(_ contact: Contact) async throws {
    try await sendCommandOkResp(.apiRejectCall(contact: contact))
}

func apiSendCallOffer(_ contact: Contact, _ rtcSession: String, _ rtcIceCandidates: String, media: CallMediaType, capabilities: CallCapabilities) async throws {
    let webRtcSession = WebRTCSession(rtcSession: rtcSession, rtcIceCandidates: rtcIceCandidates)
    let callOffer = WebRTCCallOffer(callType: CallType(media: media, capabilities: capabilities), rtcSession: webRtcSession)
    try await sendCommandOkResp(.apiSendCallOffer(contact: contact, callOffer: callOffer))
}

func apiSendCallAnswer(_ contact: Contact, _ rtcSession: String, _ rtcIceCandidates: String) async throws {
    let answer = WebRTCSession(rtcSession: rtcSession, rtcIceCandidates: rtcIceCandidates)
    try await sendCommandOkResp(.apiSendCallAnswer(contact: contact, answer: answer))
}

func apiSendCallExtraInfo(_ contact: Contact, _ rtcIceCandidates: String) async throws {
    let extraInfo = WebRTCExtraInfo(rtcIceCandidates: rtcIceCandidates)
    try await sendCommandOkResp(.apiSendCallExtraInfo(contact: contact, extraInfo: extraInfo))
}

func apiEndCall(_ contact: Contact) async throws {
    try await sendCommandOkResp(.apiEndCall(contact: contact))
}

func apiGetCallInvitations() throws -> [RcvCallInvitation] {
    let r = chatSendCmdSync(.apiGetCallInvitations)
    if case let .callInvitations(invs) = r { return invs }
    throw r
}

func apiCallStatus(_ contact: Contact, _ status: String) async throws {
    if let callStatus = WebRTCCallStatus.init(rawValue: status) {
        try await sendCommandOkResp(.apiCallStatus(contact: contact, callStatus: callStatus))
    } else {
        logger.debug("apiCallStatus: call status \(status) not used")
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

func apiNewGroup(_ p: GroupProfile) throws -> GroupInfo {
    let r = chatSendCmdSync(.newGroup(groupProfile: p))
    if case let .groupCreated(groupInfo) = r { return groupInfo }
    throw r
}

func apiAddMember(_ groupId: Int64, _ contactId: Int64, _ memberRole: GroupMemberRole) async throws -> GroupMember {
    let r = await chatSendCmd(.apiAddMember(groupId: groupId, contactId: contactId, memberRole: memberRole))
    if case let .sentGroupInvitation(_, _, member) = r { return member }
    throw r
}

enum JoinGroupResult {
    case joined(groupInfo: GroupInfo)
    case invitationRemoved
    case groupNotFound
}

func apiJoinGroup(_ groupId: Int64) async throws -> JoinGroupResult {
    let r = await chatSendCmd(.apiJoinGroup(groupId: groupId))
    switch r {
    case let .userAcceptedGroupSent(groupInfo): return .joined(groupInfo: groupInfo)
    case .chatCmdError(.errorAgent(.SMP(.AUTH))): return .invitationRemoved
    case .chatCmdError(.errorStore(.groupNotFound)): return .groupNotFound
    default: throw r
    }
}

func apiRemoveMember(_ groupId: Int64, _ memberId: Int64) async throws -> GroupMember {
    let r = await chatSendCmd(.apiRemoveMember(groupId: groupId, memberId: memberId), bgTask: false)
    if case let .userDeletedMember(_, member) = r { return member }
    throw r
}

func leaveGroup(_ groupId: Int64) async {
    do {
        let groupInfo = try await apiLeaveGroup(groupId)
        DispatchQueue.main.async { ChatModel.shared.updateGroup(groupInfo) }
    } catch let error {
        logger.error("leaveGroup error: \(responseError(error))")
    }
}

func apiLeaveGroup(_ groupId: Int64) async throws -> GroupInfo {
    let r = await chatSendCmd(.apiLeaveGroup(groupId: groupId), bgTask: false)
    if case let .leftMemberUser(groupInfo) = r { return groupInfo }
    throw r
}

func apiListMembers(_ groupId: Int64) async -> [GroupMember] {
    let r = await chatSendCmd(.apiListMembers(groupId: groupId))
    if case let .groupMembers(group) = r { return group.members }
    return []
}

func filterMembersToAdd(_ ms: [GroupMember]) -> [Contact] {
    let memberContactIds = ms.compactMap{ m in m.memberCurrent ? m.memberContactId : nil }
    return ChatModel.shared.chats
        .compactMap{ $0.chatInfo.contact }
        .filter{ !memberContactIds.contains($0.apiId) }
        .sorted{ $0.displayName.lowercased() < $1.displayName.lowercased() }
}

func apiUpdateGroup(_ groupId: Int64, _ groupProfile: GroupProfile) async throws -> GroupInfo {
    let r = await chatSendCmd(.apiUpdateGroupProfile(groupId: groupId, groupProfile: groupProfile))
    if case let .groupUpdated(toGroup) = r { return toGroup }
    throw r
}

func initializeChat(start: Bool) throws {
    logger.debug("initializeChat")
    do {
        let m = ChatModel.shared
        try apiSetFilesFolder(filesFolder: getAppFilesDirectory().path)
        m.currentUser = try apiGetActiveUser()
        if m.currentUser == nil {
            m.onboardingStage = .step1_SimpleXInfo
        } else if start {
            try startChat()
        } else {
            m.chatRunning = false
        }
    } catch {
        fatalError("Failed to initialize chat controller or database: \(responseError(error))")
    }
}

func startChat() throws {
    logger.debug("startChat")
    let m = ChatModel.shared
    try setNetworkConfig(getNetCfg())
    let justStarted = try apiStartChat()
    if justStarted {
        m.userAddress = try apiGetUserAddress()
        m.userSMPServers = try getUserSMPServers()
        let chats = try apiGetChats()
        m.chats = chats.map { Chat.init($0) }
        NtfManager.shared.setNtfBadgeCount(m.totalUnreadCount())
        try refreshCallInvitations()
        (m.savedToken, m.tokenStatus, m.notificationMode) = apiGetNtfToken()
        if let token = m.deviceToken {
            registerToken(token: token)
        }
        withAnimation {
            m.onboardingStage = m.onboardingStage == .step2_CreateProfile
                                ? .step3_SetNotificationsMode
                                : m.chats.isEmpty
                                ? .step4_MakeConnection
                                : .onboardingComplete
        }
    }
    ChatReceiver.shared.start()
    m.chatRunning = true
    chatLastStartGroupDefault.set(Date.now)
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
        // TODO use function that has timeout
        if let msg = await chatRecvMsg() {
            self._lastMsgTime = .now
            await processReceivedMsg(msg)
        }
        if self.receiveMessages {
            _ = try? await Task.sleep(nanoseconds: 7_500_000)
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

func processReceivedMsg(_ res: ChatResponse) async {
    let m = ChatModel.shared
    await MainActor.run {
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
            let cInfo = ChatInfo.contactRequest(contactRequest: contactRequest)
            if m.hasChat(contactRequest.id) {
                m.updateChatInfo(cInfo)
            } else {
                m.addChat(Chat(
                    chatInfo: cInfo,
                    chatItems: []
                ))
                NtfManager.shared.notifyContactRequest(contactRequest)
            }
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
            if case .image = cItem.content.msgContent,
               let file = cItem.file,
               file.fileSize <= maxImageSize,
               UserDefaults.standard.bool(forKey: DEFAULT_PRIVACY_ACCEPT_IMAGES) {
                Task {
                    await receiveFile(fileId: file.fileId)
                }
            }
            if !cItem.chatDir.sent && !cItem.isCall() {
                NtfManager.shared.notifyMessageReceived(cInfo, cItem)
            }
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
            chatItemSimpleUpdate(aChatItem)
        case let .chatItemDeleted(_, toChatItem):
            let cInfo = toChatItem.chatInfo
            let cItem = toChatItem.chatItem
            if cItem.meta.itemDeleted {
                m.removeChatItem(cInfo, cItem)
            } else {
                // currently only broadcast deletion of rcv message can be received, and only this case should happen
                _ = m.upsertChatItem(cInfo, cItem)
            }
        case let .receivedGroupInvitation(groupInfo, _, _):
            m.addChat(Chat(
                chatInfo: .group(groupInfo: groupInfo),
                chatItems: []
            ))
            // NtfManager.shared.notifyContactRequest(contactRequest) // TODO notifyGroupInvitation?
        case let .joinedGroupMemberConnecting(groupInfo, _, member):
            _ = m.upsertGroupMember(groupInfo, member)
        case let .deletedMemberUser(groupInfo, _): // TODO update user member
            m.updateGroup(groupInfo)
        case let .deletedMember(groupInfo, _, deletedMember):
            _ = m.upsertGroupMember(groupInfo, deletedMember)
        case let .leftMember(groupInfo, member):
            _ = m.upsertGroupMember(groupInfo, member)
        case let .groupDeleted(groupInfo, _): // TODO update user member
            m.updateGroup(groupInfo)
        case let .userJoinedGroup(groupInfo):
            m.updateGroup(groupInfo)
        case let .joinedGroupMember(groupInfo, member):
            _ = m.upsertGroupMember(groupInfo, member)
        case let .connectedToGroupMember(groupInfo, member):
            _ = m.upsertGroupMember(groupInfo, member)
        case let .groupUpdated(toGroup):
            m.updateGroup(toGroup)
        case let .rcvFileStart(aChatItem):
            chatItemSimpleUpdate(aChatItem)
        case let .rcvFileComplete(aChatItem):
            chatItemSimpleUpdate(aChatItem)
        case let .sndFileStart(aChatItem, _):
            chatItemSimpleUpdate(aChatItem)
        case let .sndFileComplete(aChatItem, _):
            chatItemSimpleUpdate(aChatItem)
            let cItem = aChatItem.chatItem
            if aChatItem.chatInfo.chatType == .direct,
               let mc = cItem.content.msgContent,
               mc.isFile(),
               let fileName = cItem.file?.filePath {
                removeFile(fileName)
            }
        case let .callInvitation(invitation):
            m.callInvitations[invitation.contact.id] = invitation
            activateCall(invitation)

// This will be called from notification service extension
//            CXProvider.reportNewIncomingVoIPPushPayload([
//                "displayName": contact.displayName,
//                "contactId": contact.id,
//                "uuid": invitation.callkitUUID
//            ]) { error in
//                if let error = error {
//                    logger.error("reportNewIncomingVoIPPushPayload error \(error.localizedDescription)")
//                } else {
//                    logger.debug("reportNewIncomingVoIPPushPayload success for \(contact.id)")
//                }
//            }
        case let .callOffer(contact, callType, offer, sharedKey, _):
            withCall(contact) { call in
                call.callState = .offerReceived
                call.peerMedia = callType.media
                call.sharedKey = sharedKey
                let useRelay = UserDefaults.standard.bool(forKey: DEFAULT_WEBRTC_POLICY_RELAY)
                logger.debug(".callOffer useRelay \(useRelay)")
                m.callCommand = .offer(offer: offer.rtcSession, iceCandidates: offer.rtcIceCandidates, media: callType.media, aesKey: sharedKey, useWorker: true, relay: useRelay)
            }
        case let .callAnswer(contact, answer):
            withCall(contact) { call in
                call.callState = .answerReceived
                m.callCommand = .answer(answer: answer.rtcSession, iceCandidates: answer.rtcIceCandidates)
            }
        case let .callExtraInfo(contact, extraInfo):
            withCall(contact) { _ in
                m.callCommand = .ice(iceCandidates: extraInfo.rtcIceCandidates)
            }
        case let .callEnded(contact):
            if let invitation = m.callInvitations.removeValue(forKey: contact.id) {
                CallController.shared.reportCallRemoteEnded(invitation: invitation)
            }
            withCall(contact) { call in
                m.callCommand = .end
//                CallController.shared.reportCallRemoteEnded(call: call)
            }
        case .chatSuspended:
            chatSuspended()
        default:
            logger.debug("unsupported event: \(res.responseType)")
        }

        func withCall(_ contact: Contact, _ perform: (Call) -> Void) {
            if let call = m.activeCall, call.contact.apiId == contact.apiId {
                perform(call)
            } else {
                logger.debug("processReceivedMsg: ignoring \(res.responseType), not in call with the contact \(contact.id)")
            }
        }
    }
}

func chatItemSimpleUpdate(_ aChatItem: AChatItem) {
    let m = ChatModel.shared
    let cInfo = aChatItem.chatInfo
    let cItem = aChatItem.chatItem
    if m.upsertChatItem(cInfo, cItem) {
        NtfManager.shared.notifyMessageReceived(cInfo, cItem)
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

func refreshCallInvitations() throws {
    let m = ChatModel.shared
    let callInvitations = try apiGetCallInvitations()
    m.callInvitations = callInvitations.reduce(into: [ChatId: RcvCallInvitation]()) { result, inv in result[inv.contact.id] = inv }
    if let (chatId, ntfAction) = m.ntfCallInvitationAction,
       let invitation = m.callInvitations.removeValue(forKey: chatId) {
        m.ntfCallInvitationAction = nil
        CallController.shared.callAction(invitation: invitation, action: ntfAction)
    } else if let invitation = callInvitations.last {
        activateCall(invitation)
    }
}

func activateCall(_ callInvitation: RcvCallInvitation) {
    let m = ChatModel.shared
    CallController.shared.reportNewIncomingCall(invitation: callInvitation) { error in
        if let error = error {
            m.callInvitations[callInvitation.contact.id]?.callkitUUID = nil
            logger.error("reportNewIncomingCall error: \(error.localizedDescription)")
        } else {
            logger.debug("reportNewIncomingCall success")
        }
    }
}

private struct UserResponse: Decodable {
    var user: User?
    var error: String?
}
