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
        ChatModel.shared.terminalItems.append(.cmd(.now, cmd.obfuscated))
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
    let r = chatSendCmdSync(.startChat(subscribe: true, expire: true))
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

func apiSetIncognito(incognito: Bool) throws {
    let r = chatSendCmdSync(.setIncognito(incognito: incognito))
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

func apiStorageEncryption(currentKey: String = "", newKey: String = "") async throws {
    try await sendCommandOkResp(.apiStorageEncryption(config: DBEncryptionConfig(currentKey: currentKey, newKey: newKey)))
}

func apiGetChats() throws -> [ChatData] {
    let r = chatSendCmdSync(.apiGetChats)
    if case let .apiChats(chats) = r { return chats }
    throw r
}

func apiGetChat(type: ChatType, id: Int64, search: String = "") throws -> Chat {
    let r = chatSendCmdSync(.apiGetChat(type: type, id: id, pagination: .last(count: 50), search: search))
    if case let .apiChat(chat) = r { return Chat.init(chat) }
    throw r
}

func apiGetChatItems(type: ChatType, id: Int64, pagination: ChatPagination, search: String = "") async throws -> [ChatItem] {
    let r = await chatSendCmd(.apiGetChat(type: type, id: id, pagination: pagination, search: search))
    if case let .apiChat(chat) = r { return chat.chatItems }
    throw r
}

func loadChat(chat: Chat, search: String = "") {
    do {
        let cInfo = chat.chatInfo
        let m = ChatModel.shared
        m.reversedChatItems = []
        let chat = try apiGetChat(type: cInfo.chatType, id: cInfo.apiId, search: search)
        m.updateChatInfo(chat.chatInfo)
        m.reversedChatItems = chat.chatItems.reversed()
    } catch let error {
        logger.error("loadChat error: \(responseError(error))")
    }
}

func apiSendMessage(type: ChatType, id: Int64, file: String?, quotedItemId: Int64?, msg: MsgContent) async -> ChatItem? {
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
        if !networkErrorAlert(r) {
            sendMessageErrorAlert(r)
        }
        endTask()
        return nil
    } else {
        r = await chatSendCmd(cmd, bgDelay: msgDelay)
        if case let .newChatItem(aChatItem) = r {
            return aChatItem.chatItem
        }
        sendMessageErrorAlert(r)
        return nil
    }
}

private func sendMessageErrorAlert(_ r: ChatResponse) {
    logger.error("apiSendMessage error: \(String(describing: r))")
    AlertManager.shared.showAlertMsg(
        title: "Error sending message",
        message: "Error: \(String(describing: r))"
    )
}

func apiUpdateChatItem(type: ChatType, id: Int64, itemId: Int64, msg: MsgContent) async throws -> ChatItem {
    let r = await chatSendCmd(.apiUpdateChatItem(type: type, id: id, itemId: itemId, msg: msg), bgDelay: msgDelay)
    if case let .chatItemUpdated(aChatItem) = r { return aChatItem.chatItem }
    throw r
}

func apiDeleteChatItem(type: ChatType, id: Int64, itemId: Int64, mode: CIDeleteMode) async throws -> (ChatItem, ChatItem?) {
    let r = await chatSendCmd(.apiDeleteChatItem(type: type, id: id, itemId: itemId, mode: mode), bgDelay: msgDelay)
    if case let .chatItemDeleted(deletedChatItem, toChatItem, _) = r { return (deletedChatItem.chatItem, toChatItem?.chatItem) }
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

func getUserSMPServers() throws -> ([ServerCfg], [String]) {
    let r = chatSendCmdSync(.getUserSMPServers)
    if case let .userSMPServers(smpServers, presetServers) = r { return (smpServers, presetServers) }
    throw r
}

func setUserSMPServers(smpServers: [ServerCfg]) async throws {
    try await sendCommandOkResp(.setUserSMPServers(smpServers: smpServers))
}

func testSMPServer(smpServer: String) async throws -> Result<(), SMPTestFailure> {
    let r = await chatSendCmd(.testSMPServer(smpServer: smpServer))
    if case let .smpTestResult(testFailure) = r {
        if let t = testFailure {
            return .failure(t)
        }
        return .success(())
    }
    throw r
}

func getChatItemTTL() throws -> ChatItemTTL {
    let r = chatSendCmdSync(.apiGetChatItemTTL)
    if case let .chatItemTTL(chatItemTTL) = r { return ChatItemTTL(chatItemTTL) }
    throw r
}

func setChatItemTTL(_ chatItemTTL: ChatItemTTL) async throws {
    try await sendCommandOkResp(.apiSetChatItemTTL(seconds: chatItemTTL.seconds))
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

func apiSetChatSettings(type: ChatType, id: Int64, chatSettings: ChatSettings) async throws {
    try await sendCommandOkResp(.apiSetChatSettings(type: type, id: id, chatSettings: chatSettings))
}

func apiContactInfo(_ contactId: Int64) async throws -> (ConnectionStats?, Profile?) {
    let r = await chatSendCmd(.apiContactInfo(contactId: contactId))
    if case let .contactInfo(_, connStats, customUserProfile) = r { return (connStats, customUserProfile) }
    throw r
}

func apiGroupMemberInfo(_ groupId: Int64, _ groupMemberId: Int64) throws -> (ConnectionStats?) {
    let r = chatSendCmdSync(.apiGroupMemberInfo(groupId: groupId, groupMemberId: groupMemberId))
    if case let .groupMemberInfo(_, _, connStats_) = r { return (connStats_) }
    throw r
}

func apiSwitchContact(contactId: Int64) async throws {
    try await sendCommandOkResp(.apiSwitchContact(contactId: contactId))
}

func apiSwitchGroupMember(_ groupId: Int64, _ groupMemberId: Int64) async throws {
    try await sendCommandOkResp(.apiSwitchGroupMember(groupId: groupId, groupMemberId: groupMemberId))
}

func apiGetContactCode(_ contactId: Int64) async throws -> (Contact, String) {
    let r = await chatSendCmd(.apiGetContactCode(contactId: contactId))
    if case let .contactCode(contact, connectionCode) = r { return (contact, connectionCode) }
    throw r
}

func apiGetGroupMemberCode(_ groupId: Int64, _ groupMemberId: Int64) throws -> (GroupMember, String) {
    let r = chatSendCmdSync(.apiGetGroupMemberCode(groupId: groupId, groupMemberId: groupMemberId))
    if case let .groupMemberCode(_, member, connectionCode) = r { return (member, connectionCode) }
    throw r
}

func apiVerifyContact(_ contactId: Int64, connectionCode: String?) -> (Bool, String)? {
    let r = chatSendCmdSync(.apiVerifyContact(contactId: contactId, connectionCode: connectionCode))
    if case let .connectionVerified(verified, expectedCode) = r { return (verified, expectedCode) }
    logger.error("apiVerifyContact error: \(String(describing: r))")
    return nil
}

func apiVerifyGroupMember(_ groupId: Int64, _ groupMemberId: Int64, connectionCode: String?) -> (Bool, String)? {
    let r = chatSendCmdSync(.apiVerifyGroupMember(groupId: groupId, groupMemberId: groupMemberId, connectionCode: connectionCode))
    if case let .connectionVerified(verified, expectedCode) = r { return (verified, expectedCode) }
    logger.error("apiVerifyGroupMember error: \(String(describing: r))")
    return nil
}

func apiAddContact() async -> String? {
    let r = await chatSendCmd(.addContact, bgTask: false)
    if case let .invitation(connReqInvitation) = r { return connReqInvitation }
    connectionErrorAlert(r)
    return nil
}

func apiConnect(connReq: String) async -> ConnReqType? {
    let r = await chatSendCmd(.connect(connReq: connReq))
    let am = AlertManager.shared
    switch r {
    case .sentConfirmation: return .invitation
    case .sentInvitation: return .contact
    case let .contactAlreadyExists(contact):
        let m = ChatModel.shared
        if let c = m.getContactChat(contact.contactId) {
            await MainActor.run { m.chatId = c.id }
        }
        am.showAlertMsg(
            title: "Contact already exists",
            message: "You are already connected to \(contact.displayName)."
        )
        return nil
    case .chatCmdError(.error(.invalidConnReq)):
        am.showAlertMsg(
            title: "Invalid connection link",
            message: "Please check that you used the correct link or ask your contact to send you another one."
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
        }
    default: ()
    }
    connectionErrorAlert(r)
    return nil
}

private func connectionErrorAlert(_ r: ChatResponse) {
    if !networkErrorAlert(r) {
        AlertManager.shared.showAlertMsg(
            title: "Connection error",
            message: "Error: \(String(describing: r))"
        )
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
    } catch let error {
        logger.error("deleteChat apiDeleteChat error: \(responseError(error))")
        AlertManager.shared.showAlertMsg(
            title: "Error deleting chat!",
            message: "Error: \(responseError(error))"
        )
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

func apiSetContactPrefs(contactId: Int64, preferences: Preferences) async throws -> Contact? {
    let r = await chatSendCmd(.apiSetContactPrefs(contactId: contactId, preferences: preferences))
    if case let .contactPrefsUpdated(_, toContact) = r { return toContact }
    throw r
}

func apiSetContactAlias(contactId: Int64, localAlias: String) async throws -> Contact? {
    let r = await chatSendCmd(.apiSetContactAlias(contactId: contactId, localAlias: localAlias))
    if case let .contactAliasUpdated(toContact) = r { return toContact }
    throw r
}

func apiSetConnectionAlias(connId: Int64, localAlias: String) async throws -> PendingContactConnection? {
    let r = await chatSendCmd(.apiSetConnectionAlias(connId: connId, localAlias: localAlias))
    if case let .connectionAliasUpdated(toConnection) = r { return toConnection }
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

func apiGetUserAddress() throws -> UserContactLink? {
    let r = chatSendCmdSync(.showMyAddress)
    switch r {
    case let .userContactLink(contactLink): return contactLink
    case .chatCmdError(chatError: .errorStore(storeError: .userContactLinkNotFound)): return nil
    default: throw r
    }
}

func userAddressAutoAccept(_ autoAccept: AutoAccept?) async throws -> UserContactLink? {
    let r = await chatSendCmd(.addressAutoAccept(autoAccept: autoAccept))
    switch r {
    case let .userContactLinkUpdated(contactLink): return contactLink
    case .chatCmdError(chatError: .errorStore(storeError: .userContactLinkNotFound)): return nil
    default: throw r
    }
}

func apiAcceptContactRequest(contactReqId: Int64) async -> Contact? {
    let r = await chatSendCmd(.apiAcceptContact(contactReqId: contactReqId))
    let am = AlertManager.shared

    if case let .acceptingContactRequest(contact) = r { return contact }
    if case .chatCmdError(.errorAgent(.SMP(.AUTH))) = r {
        am.showAlertMsg(
            title: "Connection error (AUTH)",
            message: "Sender may have deleted the connection request."
        )
    } else if !networkErrorAlert(r) {
        logger.error("apiAcceptContactRequest error: \(String(describing: r))")
        am.showAlertMsg(
            title: "Error accepting contact request",
            message: "Error: \(String(describing: r))"
        )
    }
    return nil
}

func apiRejectContactRequest(contactReqId: Int64) async throws {
    let r = await chatSendCmd(.apiRejectContact(contactReqId: contactReqId))
    if case .contactRequestRejected = r { return }
    throw r
}

func apiChatRead(type: ChatType, id: Int64, itemRange: (Int64, Int64)) async throws {
    try await sendCommandOkResp(.apiChatRead(type: type, id: id, itemRange: itemRange))
}

func apiChatUnread(type: ChatType, id: Int64, unreadChat: Bool) async throws {
    try await sendCommandOkResp(.apiChatUnread(type: type, id: id, unreadChat: unreadChat))
}

func receiveFile(fileId: Int64) async {
    let inline = privacyTransferImagesInlineGroupDefault.get()
    if let chatItem = await apiReceiveFile(fileId: fileId, inline: inline) {
        DispatchQueue.main.async { chatItemSimpleUpdate(chatItem) }
    }
}

func apiReceiveFile(fileId: Int64, inline: Bool) async -> AChatItem? {
    let r = await chatSendCmd(.receiveFile(fileId: fileId, inline: inline))
    let am = AlertManager.shared
    if case let .rcvFileAccepted(chatItem) = r { return chatItem }
    if case .rcvFileAcceptedSndCancelled = r {
        am.showAlertMsg(
            title: "Cannot receive file",
            message: "Sender cancelled file transfer."
        )
    } else if !networkErrorAlert(r) {
        logger.error("apiReceiveFile error: \(String(describing: r))")
        switch r {
        case .chatCmdError(.error(.fileAlreadyReceiving)):
            logger.debug("apiReceiveFile ignoring fileAlreadyReceiving error")
        default:
            am.showAlertMsg(
                title: "Error receiving file",
                message: "Error: \(String(describing: r))"
            )
        }
    }
    return nil
}

func networkErrorAlert(_ r: ChatResponse) -> Bool {
    let am = AlertManager.shared
    switch r {
    case let .chatCmdError(.errorAgent(.BROKER(addr, .TIMEOUT))):
        am.showAlertMsg(
            title: "Connection timeout",
            message: "Please check your network connection with \(serverHostname(addr)) and try again."
        )
        return true
    case let .chatCmdError(.errorAgent(.BROKER(addr, .NETWORK))):
        am.showAlertMsg(
            title: "Connection error",
            message: "Please check your network connection with \(serverHostname(addr)) and try again."
        )
        return true
    default:
        return false
    }
}

func acceptContactRequest(_ contactRequest: UserContactRequest) async {
    if let contact = await apiAcceptContactRequest(contactReqId: contactRequest.apiId) {
        let chat = Chat(chatInfo: ChatInfo.direct(contact: contact), chatItems: [])
        DispatchQueue.main.async { ChatModel.shared.replaceChat(contactRequest.id, chat) }
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

func markChatRead(_ chat: Chat, aboveItem: ChatItem? = nil) async {
    do {
        if chat.chatStats.unreadCount > 0 {
            let minItemId = chat.chatStats.minUnreadItemId
            let itemRange = (minItemId, aboveItem?.id ?? chat.chatItems.last?.id ?? minItemId)
            let cInfo = chat.chatInfo
            try await apiChatRead(type: cInfo.chatType, id: cInfo.apiId, itemRange: itemRange)
            await MainActor.run { ChatModel.shared.markChatItemsRead(cInfo, aboveItem: aboveItem) }
        }
        if chat.chatStats.unreadChat {
            await markChatUnread(chat, unreadChat: false)
        }
    } catch {
        logger.error("markChatRead apiChatRead error: \(responseError(error))")
    }
}

func markChatUnread(_ chat: Chat, unreadChat: Bool = true) async {
    do {
        let cInfo = chat.chatInfo
        try await apiChatUnread(type: cInfo.chatType, id: cInfo.apiId, unreadChat: unreadChat)
        await MainActor.run { ChatModel.shared.markChatUnread(cInfo, unreadChat: unreadChat) }
    } catch {
        logger.error("markChatUnread apiChatUnread error: \(responseError(error))")
    }
}

func apiMarkChatItemRead(_ cInfo: ChatInfo, _ cItem: ChatItem) async {
    do {
        logger.debug("apiMarkChatItemRead: \(cItem.id)")
        try await apiChatRead(type: cInfo.chatType, id: cInfo.apiId, itemRange: (cItem.id, cItem.id))
        await MainActor.run { ChatModel.shared.markChatItemRead(cInfo, cItem) }
    } catch {
        logger.error("apiMarkChatItemRead apiChatRead error: \(responseError(error))")
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
    case let .userAcceptedGroupSent(groupInfo, _): return .joined(groupInfo: groupInfo)
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

func apiMemberRole(_ groupId: Int64, _ memberId: Int64, _ memberRole: GroupMemberRole) async throws -> GroupMember {
    let r = await chatSendCmd(.apiMemberRole(groupId: groupId, memberId: memberId, memberRole: memberRole), bgTask: false)
    if case let .memberRoleUser(_, member, _, _) = r { return member }
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

func apiListMembersSync(_ groupId: Int64) -> [GroupMember] {
    let r = chatSendCmdSync(.apiListMembers(groupId: groupId))
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

func apiCreateGroupLink(_ groupId: Int64) async throws -> String {
    let r = await chatSendCmd(.apiCreateGroupLink(groupId: groupId))
    if case let .groupLinkCreated(_, connReq) = r { return connReq }
    throw r
}

func apiDeleteGroupLink(_ groupId: Int64) async throws {
    let r = await chatSendCmd(.apiDeleteGroupLink(groupId: groupId))
    if case .groupLinkDeleted = r { return }
    throw r
}

func apiGetGroupLink(_ groupId: Int64) throws -> String? {
    let r = chatSendCmdSync(.apiGetGroupLink(groupId: groupId))
    switch r {
    case let .groupLink(_, connReq):
        return connReq
    case .chatCmdError(chatError: .errorStore(storeError: .groupLinkNotFound)):
        return nil
    default: throw r
    }
}

func initializeChat(start: Bool, dbKey: String? = nil) throws {
    logger.debug("initializeChat")
    let m = ChatModel.shared
    (m.chatDbEncrypted, m.chatDbStatus) = chatMigrateInit(dbKey)
    if  m.chatDbStatus != .ok { return }
    // If we migrated successfully means previous re-encryption process on database level finished successfully too
    if encryptionStartedDefault.get() {
        encryptionStartedDefault.set(false)
    }
    try apiSetFilesFolder(filesFolder: getAppFilesDirectory().path)
    try apiSetIncognito(incognito: incognitoGroupDefault.get())
    m.chatInitialized = true
    m.currentUser = try apiGetActiveUser()
    if m.currentUser == nil {
        m.onboardingStage = .step1_SimpleXInfo
    } else if start {
        try startChat()
    } else {
        m.chatRunning = false
    }
}

func startChat() throws {
    logger.debug("startChat")
    let m = ChatModel.shared
    try setNetworkConfig(getNetCfg())
    let justStarted = try apiStartChat()
    if justStarted {
        m.userAddress = try apiGetUserAddress()
        (m.userSMPServers, m.presetSMPServers) = try getUserSMPServers()
        m.chatItemTTL = try getChatItemTTL()
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
        case let .contactConnected(contact, _):
            if contact.directOrUsed {
                m.updateContact(contact)
                m.dismissConnReqView(contact.activeConn.id)
                m.removeChat(contact.activeConn.id)
                m.updateNetworkStatus(contact.id, .connected)
                NtfManager.shared.notifyContactConnected(contact)
            }
        case let .contactConnecting(contact):
            if contact.directOrUsed {
                m.updateContact(contact)
                m.dismissConnReqView(contact.activeConn.id)
                m.removeChat(contact.activeConn.id)
            }
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
        case let .contactsMerged(intoContact, mergedContact):
            if m.hasChat(mergedContact.id) {
                if m.chatId == mergedContact.id {
                    m.chatId = intoContact.id
                }
                m.removeChat(mergedContact.id)
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
               let mc = cItem.content.msgContent,
               file.fileSize <= MAX_IMAGE_SIZE {
                let acceptImages = UserDefaults.standard.bool(forKey: DEFAULT_PRIVACY_ACCEPT_IMAGES)
                if (mc.isImage && acceptImages)
                    || (mc.isVoice && ((file.fileSize > MAX_VOICE_MESSAGE_SIZE_INLINE_SEND && acceptImages) || cInfo.chatType == .group)) {
                    Task {
                        await receiveFile(fileId: file.fileId) // TODO check inlineFileMode != IFMSent
                    }
                }
            }
            if cItem.showNotification {
                NtfManager.shared.notifyMessageReceived(cInfo, cItem)
            }
        case let .chatItemStatusUpdated(aChatItem):
            let cInfo = aChatItem.chatInfo
            let cItem = aChatItem.chatItem
            var res = false
            if !cItem.isDeletedContent {
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
        case let .chatItemDeleted(deletedChatItem, toChatItem, _):
            if let toChatItem = toChatItem {
                _ = m.upsertChatItem(toChatItem.chatInfo, toChatItem.chatItem)
            } else {
                m.removeChatItem(deletedChatItem.chatInfo, deletedChatItem.chatItem)
            }
        case let .receivedGroupInvitation(groupInfo, _, _):
            m.updateGroup(groupInfo) // update so that repeat group invitations are not duplicated
            // NtfManager.shared.notifyContactRequest(contactRequest) // TODO notifyGroupInvitation?
        case let .userAcceptedGroupSent(groupInfo, hostContact):
            m.updateGroup(groupInfo)
            if let hostContact = hostContact {
                m.dismissConnReqView(hostContact.activeConn.id)
                m.removeChat(hostContact.activeConn.id)
            }
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
            let mc = cItem.content.msgContent
            if aChatItem.chatInfo.chatType == .direct,
               case .file = mc,
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
                let iceServers = getIceServers()
                logger.debug(".callOffer useRelay \(useRelay)")
                logger.debug(".callOffer iceServers \(String(describing: iceServers))")
                m.callCommand = .offer(
                    offer: offer.rtcSession,
                    iceCandidates: offer.rtcIceCandidates,
                    media: callType.media, aesKey: sharedKey,
                    useWorker: true,
                    iceServers: iceServers,
                    relay: useRelay
                )
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
    case .errorAgent(agentError: .BROKER(_, .NETWORK)): err = "network"
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
