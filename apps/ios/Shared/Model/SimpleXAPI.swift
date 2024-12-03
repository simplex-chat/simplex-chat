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

// currentChatVersion in core
public let CURRENT_CHAT_VERSION: Int = 2

// version range that supports establishing direct connection with a group member (xGrpDirectInvVRange in core)
public let CREATE_MEMBER_CONTACT_VRANGE = VersionRange(minVersion: 2, maxVersion: CURRENT_CHAT_VERSION)

private let networkStatusesLock = DispatchQueue(label: "chat.simplex.app.network-statuses.lock")

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

func chatSendCmdSync(_ cmd: ChatCommand, bgTask: Bool = true, bgDelay: Double? = nil, _ ctrl: chat_ctrl? = nil, log: Bool = true) -> ChatResponse {
    if log {
        logger.debug("chatSendCmd \(cmd.cmdType)")
    }
    let start = Date.now
    let resp = bgTask
                ? withBGTask(bgDelay: bgDelay) { sendSimpleXCmd(cmd, ctrl) }
                : sendSimpleXCmd(cmd, ctrl)
    if log {
        logger.debug("chatSendCmd \(cmd.cmdType): \(resp.responseType)")
        if case let .response(_, json) = resp {
            logger.debug("chatSendCmd \(cmd.cmdType) response: \(json)")
        }
        Task {
            await TerminalItems.shared.addCommand(start, cmd.obfuscated, resp)
        }
    }
    return resp
}

func chatSendCmd(_ cmd: ChatCommand, bgTask: Bool = true, bgDelay: Double? = nil, _ ctrl: chat_ctrl? = nil, log: Bool = true) async -> ChatResponse {
    await withCheckedContinuation { cont in
        cont.resume(returning: chatSendCmdSync(cmd, bgTask: bgTask, bgDelay: bgDelay, ctrl, log: log))
    }
}

func chatRecvMsg(_ ctrl: chat_ctrl? = nil) async -> ChatResponse? {
    await withCheckedContinuation { cont in
        _  = withBGTask(bgDelay: msgDelay) { () -> ChatResponse? in
            let resp = recvSimpleXMsg(ctrl)
            cont.resume(returning: resp)
            return resp
        }
    }
}

func apiGetActiveUser(ctrl: chat_ctrl? = nil) throws -> User? {
    let r = chatSendCmdSync(.showActiveUser, ctrl)
    switch r {
    case let .activeUser(user): return user
    case .chatCmdError(_, .error(.noActiveUser)): return nil
    default: throw r
    }
}

func apiCreateActiveUser(_ p: Profile?, pastTimestamp: Bool = false, ctrl: chat_ctrl? = nil) throws -> User {
    let r = chatSendCmdSync(.createActiveUser(profile: p, pastTimestamp: pastTimestamp), ctrl)
    if case let .activeUser(user) = r { return user }
    throw r
}

func listUsers() throws -> [UserInfo] {
    return try listUsersResponse(chatSendCmdSync(.listUsers))
}

func listUsersAsync() async throws -> [UserInfo] {
    return try listUsersResponse(await chatSendCmd(.listUsers))
}

private func listUsersResponse(_ r: ChatResponse) throws -> [UserInfo] {
    if case let .usersList(users) = r {
        return users.sorted { $0.user.chatViewName.compare($1.user.chatViewName) == .orderedAscending }
    }
    throw r
}

func apiSetActiveUser(_ userId: Int64, viewPwd: String?) throws -> User {
    let r = chatSendCmdSync(.apiSetActiveUser(userId: userId, viewPwd: viewPwd))
    if case let .activeUser(user) = r { return user }
    throw r
}

func apiSetActiveUserAsync(_ userId: Int64, viewPwd: String?) async throws -> User {
    let r = await chatSendCmd(.apiSetActiveUser(userId: userId, viewPwd: viewPwd))
    if case let .activeUser(user) = r { return user }
    throw r
}

func apiSetAllContactReceipts(enable: Bool) async throws {
    let r = await chatSendCmd(.setAllContactReceipts(enable: enable))
    if case .cmdOk = r { return }
    throw r
}

func apiSetUserContactReceipts(_ userId: Int64, userMsgReceiptSettings: UserMsgReceiptSettings) async throws {
    let r = await chatSendCmd(.apiSetUserContactReceipts(userId: userId, userMsgReceiptSettings: userMsgReceiptSettings))
    if case .cmdOk = r { return }
    throw r
}

func apiSetUserGroupReceipts(_ userId: Int64, userMsgReceiptSettings: UserMsgReceiptSettings) async throws {
    let r = await chatSendCmd(.apiSetUserGroupReceipts(userId: userId, userMsgReceiptSettings: userMsgReceiptSettings))
    if case .cmdOk = r { return }
    throw r
}

func apiHideUser(_ userId: Int64, viewPwd: String) async throws -> User {
    try await setUserPrivacy_(.apiHideUser(userId: userId, viewPwd: viewPwd))
}

func apiUnhideUser(_ userId: Int64, viewPwd: String) async throws -> User {
    try await setUserPrivacy_(.apiUnhideUser(userId: userId, viewPwd: viewPwd))
}

func apiMuteUser(_ userId: Int64) async throws -> User {
    try await setUserPrivacy_(.apiMuteUser(userId: userId))
}

func apiUnmuteUser(_ userId: Int64) async throws -> User {
    try await setUserPrivacy_(.apiUnmuteUser(userId: userId))
}

func setUserPrivacy_(_ cmd: ChatCommand) async throws -> User {
    let r = await chatSendCmd(cmd)
    if case let .userPrivacy(_, updatedUser) = r { return updatedUser }
    throw r
}

func apiDeleteUser(_ userId: Int64, _ delSMPQueues: Bool, viewPwd: String?) async throws {
    let r = await chatSendCmd(.apiDeleteUser(userId: userId, delSMPQueues: delSMPQueues, viewPwd: viewPwd))
    if case .cmdOk = r { return }
    throw r
}

func apiStartChat(ctrl: chat_ctrl? = nil) throws -> Bool {
    let r = chatSendCmdSync(.startChat(mainApp: true, enableSndFiles: true), ctrl)
    switch r {
    case .chatStarted: return true
    case .chatRunning: return false
    default: throw r
    }
}

func apiCheckChatRunning() throws -> Bool {
    let r = chatSendCmdSync(.checkChatRunning)
    switch r {
    case .chatRunning: return true
    case .chatStopped: return false
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
    chatReopenStore()
    let r = chatSendCmdSync(.apiActivateChat(restoreChat: true))
    if case .cmdOk = r { return }
    logger.error("apiActivateChat error: \(String(describing: r))")
}

func apiSuspendChat(timeoutMicroseconds: Int) {
    let r = chatSendCmdSync(.apiSuspendChat(timeoutMicroseconds: timeoutMicroseconds))
    if case .cmdOk = r { return }
    logger.error("apiSuspendChat error: \(String(describing: r))")
}

func apiSetAppFilePaths(filesFolder: String, tempFolder: String, assetsFolder: String, ctrl: chat_ctrl? = nil) throws {
    let r = chatSendCmdSync(.apiSetAppFilePaths(filesFolder: filesFolder, tempFolder: tempFolder, assetsFolder: assetsFolder), ctrl)
    if case .cmdOk = r { return }
    throw r
}

func apiSetEncryptLocalFiles(_ enable: Bool) throws {
    let r = chatSendCmdSync(.apiSetEncryptLocalFiles(enable: enable))
    if case .cmdOk = r { return }
    throw r
}

func apiSaveAppSettings(settings: AppSettings) throws {
    let r = chatSendCmdSync(.apiSaveSettings(settings: settings))
    if case .cmdOk = r { return }
    throw r
}

func apiGetAppSettings(settings: AppSettings) throws -> AppSettings {
    let r = chatSendCmdSync(.apiGetSettings(settings: settings))
    if case let .appSettings(settings) = r { return settings }
    throw r
}

func apiExportArchive(config: ArchiveConfig) async throws -> [ArchiveError] {
    let r = await chatSendCmd(.apiExportArchive(config: config))
    if case let .archiveExported(archiveErrors) = r { return archiveErrors }
    throw r
}

func apiImportArchive(config: ArchiveConfig) async throws -> [ArchiveError] {
    let r = await chatSendCmd(.apiImportArchive(config: config))
    if case let .archiveImported(archiveErrors) = r { return archiveErrors }
    throw r
}

func apiDeleteStorage() async throws {
    try await sendCommandOkResp(.apiDeleteStorage)
}

func apiStorageEncryption(currentKey: String = "", newKey: String = "") async throws {
    try await sendCommandOkResp(.apiStorageEncryption(config: DBEncryptionConfig(currentKey: currentKey, newKey: newKey)))
}

func testStorageEncryption(key: String, _ ctrl: chat_ctrl? = nil) async throws {
    try await sendCommandOkResp(.testStorageEncryption(key: key), ctrl)
}

func apiGetChats() throws -> [ChatData] {
    let userId = try currentUserId("apiGetChats")
    return try apiChatsResponse(chatSendCmdSync(.apiGetChats(userId: userId)))
}

func apiGetChatsAsync() async throws -> [ChatData] {
    let userId = try currentUserId("apiGetChats")
    return try apiChatsResponse(await chatSendCmd(.apiGetChats(userId: userId)))
}

private func apiChatsResponse(_ r: ChatResponse) throws -> [ChatData] {
    if case let .apiChats(_, chats) = r { return chats }
    throw r
}

let loadItemsPerPage = 50

func apiGetChat(type: ChatType, id: Int64, search: String = "") async throws -> Chat {
    let r = await chatSendCmd(.apiGetChat(type: type, id: id, pagination: .last(count: loadItemsPerPage), search: search))
    if case let .apiChat(_, chat) = r { return Chat.init(chat) }
    throw r
}

func apiGetChatItems(type: ChatType, id: Int64, pagination: ChatPagination, search: String = "") async throws -> [ChatItem] {
    let r = await chatSendCmd(.apiGetChat(type: type, id: id, pagination: pagination, search: search))
    if case let .apiChat(_, chat) = r { return chat.chatItems }
    throw r
}

func loadChat(chat: Chat, search: String = "", clearItems: Bool = true) async {
    do {
        let cInfo = chat.chatInfo
        let m = ChatModel.shared
        let im = ItemsModel.shared
        m.chatItemStatuses = [:]
        if clearItems {
            await MainActor.run { im.reversedChatItems = [] }
        }
        let chat = try await apiGetChat(type: cInfo.chatType, id: cInfo.apiId, search: search)
        await MainActor.run {
            im.reversedChatItems = chat.chatItems.reversed()
            m.updateChatInfo(chat.chatInfo)
        }
    } catch let error {
        logger.error("loadChat error: \(responseError(error))")
    }
}

func apiGetChatItemInfo(type: ChatType, id: Int64, itemId: Int64) async throws -> ChatItemInfo {
    let r = await chatSendCmd(.apiGetChatItemInfo(type: type, id: id, itemId: itemId))
    if case let .chatItemInfo(_, _, chatItemInfo) = r { return chatItemInfo }
    throw r
}

func apiPlanForwardChatItems(type: ChatType, id: Int64, itemIds: [Int64]) async throws -> ([Int64], ForwardConfirmation?) {
    let r = await chatSendCmd(.apiPlanForwardChatItems(toChatType: type, toChatId: id, itemIds: itemIds))
    if case let .forwardPlan(_, chatItemIds, forwardConfimation) = r { return (chatItemIds, forwardConfimation) }
    throw r
}

func apiForwardChatItems(toChatType: ChatType, toChatId: Int64, fromChatType: ChatType, fromChatId: Int64, itemIds: [Int64], ttl: Int?) async -> [ChatItem]? {
    let cmd: ChatCommand = .apiForwardChatItems(toChatType: toChatType, toChatId: toChatId, fromChatType: fromChatType, fromChatId: fromChatId, itemIds: itemIds, ttl: ttl)
    return await processSendMessageCmd(toChatType: toChatType, cmd: cmd)
}

func apiSendMessages(type: ChatType, id: Int64, live: Bool = false, ttl: Int? = nil, composedMessages: [ComposedMessage]) async -> [ChatItem]? {
    let cmd: ChatCommand = .apiSendMessages(type: type, id: id, live: live, ttl: ttl, composedMessages: composedMessages)
    return await processSendMessageCmd(toChatType: type, cmd: cmd)
}

private func processSendMessageCmd(toChatType: ChatType, cmd: ChatCommand) async -> [ChatItem]? {
    let chatModel = ChatModel.shared
    let r: ChatResponse
    if toChatType == .direct {
        var cItem: ChatItem? = nil
        let endTask = beginBGTask({
            if let cItem = cItem {
                DispatchQueue.main.async {
                    chatModel.messageDelivery.removeValue(forKey: cItem.id)
                }
            }
        })
        r = await chatSendCmd(cmd, bgTask: false)
        if case let .newChatItems(_, aChatItems) = r {
            let cItems = aChatItems.map { $0.chatItem }
            if let cItemLast = cItems.last {
                cItem = cItemLast
                chatModel.messageDelivery[cItemLast.id] = endTask
            }
            return cItems
        }
        if let networkErrorAlert = networkErrorAlert(r) {
            AlertManager.shared.showAlert(networkErrorAlert)
        } else {
            sendMessageErrorAlert(r)
        }
        endTask()
        return nil
    } else {
        r = await chatSendCmd(cmd, bgDelay: msgDelay)
        if case let .newChatItems(_, aChatItems) = r {
            return aChatItems.map { $0.chatItem }
        }
        sendMessageErrorAlert(r)
        return nil
    }
}

func apiCreateChatItems(noteFolderId: Int64, composedMessages: [ComposedMessage]) async -> [ChatItem]? {
    let r = await chatSendCmd(.apiCreateChatItems(noteFolderId: noteFolderId, composedMessages: composedMessages))
    if case let .newChatItems(_, aChatItems) = r { return aChatItems.map { $0.chatItem } }
    createChatItemsErrorAlert(r)
    return nil
}

private func sendMessageErrorAlert(_ r: ChatResponse) {
    logger.error("send message error: \(String(describing: r))")
    AlertManager.shared.showAlertMsg(
        title: "Error sending message",
        message: "Error: \(responseError(r))"
    )
}

private func createChatItemsErrorAlert(_ r: ChatResponse) {
    logger.error("apiCreateChatItems error: \(String(describing: r))")
    AlertManager.shared.showAlertMsg(
        title: "Error creating message",
        message: "Error: \(responseError(r))"
    )
}

func apiUpdateChatItem(type: ChatType, id: Int64, itemId: Int64, msg: MsgContent, live: Bool = false) async throws -> ChatItem {
    let r = await chatSendCmd(.apiUpdateChatItem(type: type, id: id, itemId: itemId, msg: msg, live: live), bgDelay: msgDelay)
    if case let .chatItemUpdated(_, aChatItem) = r { return aChatItem.chatItem }
    throw r
}

func apiChatItemReaction(type: ChatType, id: Int64, itemId: Int64, add: Bool, reaction: MsgReaction) async throws -> ChatItem {
    let r = await chatSendCmd(.apiChatItemReaction(type: type, id: id, itemId: itemId, add: add, reaction: reaction), bgDelay: msgDelay)
    if case let .chatItemReaction(_, _, reaction) = r { return reaction.chatReaction.chatItem }
    throw r
}

func apiGetReactionMembers(groupId: Int64, itemId: Int64, reaction: MsgReaction) async throws -> [MemberReaction] {
    let userId = try currentUserId("apiGetReactionMemebers")
    let r = await chatSendCmd(.apiGetReactionMembers(userId: userId, groupId: groupId, itemId: itemId, reaction: reaction ))
    if case let .reactionMembers(_, memberReactions) = r { return memberReactions }
    throw r
}

func apiDeleteChatItems(type: ChatType, id: Int64, itemIds: [Int64], mode: CIDeleteMode) async throws -> [ChatItemDeletion] {
    let r = await chatSendCmd(.apiDeleteChatItem(type: type, id: id, itemIds: itemIds, mode: mode), bgDelay: msgDelay)
    if case let .chatItemsDeleted(_, items, _) = r { return items }
    throw r
}

func apiDeleteMemberChatItems(groupId: Int64, itemIds: [Int64]) async throws -> [ChatItemDeletion] {
    let r = await chatSendCmd(.apiDeleteMemberChatItem(groupId: groupId, itemIds: itemIds), bgDelay: msgDelay)
    if case let .chatItemsDeleted(_, items, _) = r { return items }
    throw r
}

func apiGetNtfToken() -> (DeviceToken?, NtfTknStatus?, NotificationsMode, String?) {
    let r = chatSendCmdSync(.apiGetNtfToken)
    switch r {
    case let .ntfToken(token, status, ntfMode, ntfServer): return (token, status, ntfMode, ntfServer)
    case .chatCmdError(_, .errorAgent(.CMD(.PROHIBITED, _))): return (nil, nil, .off, nil)
    default:
        logger.debug("apiGetNtfToken response: \(String(describing: r))")
        return (nil, nil, .off, nil)
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

func testProtoServer(server: String) async throws -> Result<(), ProtocolTestFailure> {
    let userId = try currentUserId("testProtoServer")
    let r = await chatSendCmd(.apiTestProtoServer(userId: userId, server: server))
    if case let .serverTestResult(_, _, testFailure) = r {
        if let t = testFailure {
            return .failure(t)
        }
        return .success(())
    }
    throw r
}

func getServerOperators() throws -> ServerOperatorConditions {
    let r = chatSendCmdSync(.apiGetServerOperators)
    if case let .serverOperatorConditions(conditions) = r { return conditions }
    logger.error("getServerOperators error: \(String(describing: r))")
    throw r
}

func setServerOperators(operators: [ServerOperator]) async throws -> ServerOperatorConditions {
    let r = await chatSendCmd(.apiSetServerOperators(operators: operators))
    if case let .serverOperatorConditions(conditions) = r { return conditions }
    logger.error("setServerOperators error: \(String(describing: r))")
    throw r
}

func getUserServers() async throws -> [UserOperatorServers] {
    let userId = try currentUserId("getUserServers")
    let r = await chatSendCmd(.apiGetUserServers(userId: userId))
    if case let .userServers(_, userServers) = r { return userServers }
    logger.error("getUserServers error: \(String(describing: r))")
    throw r
}

func setUserServers(userServers: [UserOperatorServers]) async throws {
    let userId = try currentUserId("setUserServers")
    let r = await chatSendCmd(.apiSetUserServers(userId: userId, userServers: userServers))
    if case .cmdOk = r { return }
    logger.error("setUserServers error: \(String(describing: r))")
    throw r
}

func validateServers(userServers: [UserOperatorServers]) async throws -> [UserServersError] {
    let userId = try currentUserId("validateServers")
    let r = await chatSendCmd(.apiValidateServers(userId: userId, userServers: userServers))
    if case let .userServersValidation(_, serverErrors) = r { return serverErrors }
    logger.error("validateServers error: \(String(describing: r))")
    throw r
}

func getUsageConditions() async throws -> (UsageConditions, String?, UsageConditions?) {
    let r = await chatSendCmd(.apiGetUsageConditions)
    if case let .usageConditions(usageConditions, conditionsText, acceptedConditions) = r { return (usageConditions, conditionsText, acceptedConditions) }
    logger.error("getUsageConditions error: \(String(describing: r))")
    throw r
}

func setConditionsNotified(conditionsId: Int64) async throws {
    let r = await chatSendCmd(.apiSetConditionsNotified(conditionsId: conditionsId))
    if case .cmdOk = r { return }
    logger.error("setConditionsNotified error: \(String(describing: r))")
    throw r
}

func acceptConditions(conditionsId: Int64, operatorIds: [Int64]) async throws -> ServerOperatorConditions {
    let r = await chatSendCmd(.apiAcceptConditions(conditionsId: conditionsId, operatorIds: operatorIds))
    if case let .serverOperatorConditions(conditions) = r { return conditions }
    logger.error("acceptConditions error: \(String(describing: r))")
    throw r
}

func getChatItemTTL() throws -> ChatItemTTL {
    let userId = try currentUserId("getChatItemTTL")
    return try chatItemTTLResponse(chatSendCmdSync(.apiGetChatItemTTL(userId: userId)))
}

func getChatItemTTLAsync() async throws -> ChatItemTTL {
    let userId = try currentUserId("getChatItemTTLAsync")
    return try chatItemTTLResponse(await chatSendCmd(.apiGetChatItemTTL(userId: userId)))
}

private func chatItemTTLResponse(_ r: ChatResponse) throws -> ChatItemTTL {
    if case let .chatItemTTL(_, chatItemTTL) = r { return ChatItemTTL(chatItemTTL) }
    throw r
}

func setChatItemTTL(_ chatItemTTL: ChatItemTTL) async throws {
    let userId = try currentUserId("setChatItemTTL")
    try await sendCommandOkResp(.apiSetChatItemTTL(userId: userId, seconds: chatItemTTL.seconds))
}

func getNetworkConfig() async throws -> NetCfg? {
    let r = await chatSendCmd(.apiGetNetworkConfig)
    if case let .networkConfig(cfg) = r { return cfg }
    throw r
}

func setNetworkConfig(_ cfg: NetCfg, ctrl: chat_ctrl? = nil) throws {
    let r = chatSendCmdSync(.apiSetNetworkConfig(networkConfig: cfg), ctrl)
    if case .cmdOk = r { return }
    throw r
}

func apiSetNetworkInfo(_ networkInfo: UserNetworkInfo) throws {
    let r = chatSendCmdSync(.apiSetNetworkInfo(networkInfo: networkInfo))
    if case .cmdOk = r { return }
    throw r
}

func reconnectAllServers() async throws {
    try await sendCommandOkResp(.reconnectAllServers)
}

func reconnectServer(smpServer: String) async throws {
    let userId = try currentUserId("reconnectServer")
    try await sendCommandOkResp(.reconnectServer(userId: userId, smpServer: smpServer))
}

func apiSetChatSettings(type: ChatType, id: Int64, chatSettings: ChatSettings) async throws {
    try await sendCommandOkResp(.apiSetChatSettings(type: type, id: id, chatSettings: chatSettings))
}

func apiSetMemberSettings(_ groupId: Int64, _ groupMemberId: Int64, _ memberSettings: GroupMemberSettings) async throws {
    try await sendCommandOkResp(.apiSetMemberSettings(groupId: groupId, groupMemberId: groupMemberId, memberSettings: memberSettings))
}

func apiContactInfo(_ contactId: Int64) async throws -> (ConnectionStats?, Profile?) {
    let r = await chatSendCmd(.apiContactInfo(contactId: contactId))
    if case let .contactInfo(_, _, connStats, customUserProfile) = r { return (connStats, customUserProfile) }
    throw r
}

func apiGroupMemberInfoSync(_ groupId: Int64, _ groupMemberId: Int64) throws -> (GroupMember, ConnectionStats?) {
    let r = chatSendCmdSync(.apiGroupMemberInfo(groupId: groupId, groupMemberId: groupMemberId))
    if case let .groupMemberInfo(_, _, member, connStats_) = r { return (member, connStats_) }
    throw r
}

func apiGroupMemberInfo(_ groupId: Int64, _ groupMemberId: Int64) async throws -> (GroupMember, ConnectionStats?) {
    let r = await chatSendCmd(.apiGroupMemberInfo(groupId: groupId, groupMemberId: groupMemberId))
    if case let .groupMemberInfo(_, _, member, connStats_) = r { return (member, connStats_) }
    throw r
}

func apiContactQueueInfo(_ contactId: Int64) async throws -> (RcvMsgInfo?, ServerQueueInfo) {
    let r = await chatSendCmd(.apiContactQueueInfo(contactId: contactId))
    if case let .queueInfo(_, rcvMsgInfo, queueInfo) = r { return (rcvMsgInfo, queueInfo) }
    throw r
}

func apiGroupMemberQueueInfo(_ groupId: Int64, _ groupMemberId: Int64) async throws -> (RcvMsgInfo?, ServerQueueInfo) {
    let r = await chatSendCmd(.apiGroupMemberQueueInfo(groupId: groupId, groupMemberId: groupMemberId))
    if case let .queueInfo(_, rcvMsgInfo, queueInfo) = r { return (rcvMsgInfo, queueInfo) }
    throw r
}

func apiSwitchContact(contactId: Int64) throws -> ConnectionStats {
    let r = chatSendCmdSync(.apiSwitchContact(contactId: contactId))
    if case let .contactSwitchStarted(_, _, connectionStats) = r { return connectionStats }
    throw r
}

func apiSwitchGroupMember(_ groupId: Int64, _ groupMemberId: Int64) throws -> ConnectionStats {
    let r = chatSendCmdSync(.apiSwitchGroupMember(groupId: groupId, groupMemberId: groupMemberId))
    if case let .groupMemberSwitchStarted(_, _, _, connectionStats) = r { return connectionStats }
    throw r
}

func apiAbortSwitchContact(_ contactId: Int64) throws -> ConnectionStats {
    let r = chatSendCmdSync(.apiAbortSwitchContact(contactId: contactId))
    if case let .contactSwitchAborted(_, _, connectionStats) = r { return connectionStats }
    throw r
}

func apiAbortSwitchGroupMember(_ groupId: Int64, _ groupMemberId: Int64) throws -> ConnectionStats {
    let r = chatSendCmdSync(.apiAbortSwitchGroupMember(groupId: groupId, groupMemberId: groupMemberId))
    if case let .groupMemberSwitchAborted(_, _, _, connectionStats) = r { return connectionStats }
    throw r
}

func apiSyncContactRatchet(_ contactId: Int64, _ force: Bool) throws -> ConnectionStats {
    let r = chatSendCmdSync(.apiSyncContactRatchet(contactId: contactId, force: force))
    if case let .contactRatchetSyncStarted(_, _, connectionStats) = r { return connectionStats }
    throw r
}

func apiSyncGroupMemberRatchet(_ groupId: Int64, _ groupMemberId: Int64, _ force: Bool) throws -> (GroupMember, ConnectionStats) {
    let r = chatSendCmdSync(.apiSyncGroupMemberRatchet(groupId: groupId, groupMemberId: groupMemberId, force: force))
    if case let .groupMemberRatchetSyncStarted(_, _, member, connectionStats) = r { return (member, connectionStats) }
    throw r
}

func apiGetContactCode(_ contactId: Int64) async throws -> (Contact, String) {
    let r = await chatSendCmd(.apiGetContactCode(contactId: contactId))
    if case let .contactCode(_, contact, connectionCode) = r { return (contact, connectionCode) }
    throw r
}

func apiGetGroupMemberCode(_ groupId: Int64, _ groupMemberId: Int64) async throws -> (GroupMember, String) {
    let r = await chatSendCmd(.apiGetGroupMemberCode(groupId: groupId, groupMemberId: groupMemberId))
    if case let .groupMemberCode(_, _, member, connectionCode) = r { return (member, connectionCode) }
    throw r
}

func apiVerifyContact(_ contactId: Int64, connectionCode: String?) -> (Bool, String)? {
    let r = chatSendCmdSync(.apiVerifyContact(contactId: contactId, connectionCode: connectionCode))
    if case let .connectionVerified(_, verified, expectedCode) = r { return (verified, expectedCode) }
    logger.error("apiVerifyContact error: \(String(describing: r))")
    return nil
}

func apiVerifyGroupMember(_ groupId: Int64, _ groupMemberId: Int64, connectionCode: String?) -> (Bool, String)? {
    let r = chatSendCmdSync(.apiVerifyGroupMember(groupId: groupId, groupMemberId: groupMemberId, connectionCode: connectionCode))
    if case let .connectionVerified(_, verified, expectedCode) = r { return (verified, expectedCode) }
    logger.error("apiVerifyGroupMember error: \(String(describing: r))")
    return nil
}

func apiAddContact(incognito: Bool) async -> ((String, PendingContactConnection)?, Alert?) {
    guard let userId = ChatModel.shared.currentUser?.userId else {
        logger.error("apiAddContact: no current user")
        return (nil, nil)
    }
    let r = await chatSendCmd(.apiAddContact(userId: userId, incognito: incognito), bgTask: false)
    if case let .invitation(_, connReqInvitation, connection) = r { return ((connReqInvitation, connection), nil) }
    let alert = connectionErrorAlert(r)
    return (nil, alert)
}

func apiSetConnectionIncognito(connId: Int64, incognito: Bool) async throws -> PendingContactConnection? {
    let r = await chatSendCmd(.apiSetConnectionIncognito(connId: connId, incognito: incognito))
    if case let .connectionIncognitoUpdated(_, toConnection) = r { return toConnection }
    throw r
}

func apiChangeConnectionUser(connId: Int64, userId: Int64) async throws -> PendingContactConnection? {
    let r = await chatSendCmd(.apiChangeConnectionUser(connId: connId, userId: userId))
    
    if case let .connectionUserChanged(_, _, toConnection, _) = r {return toConnection}
    throw r
}

func apiConnectPlan(connReq: String) async throws -> ConnectionPlan {
    let userId = try currentUserId("apiConnectPlan")
    let r = await chatSendCmd(.apiConnectPlan(userId: userId, connReq: connReq))
    if case let .connectionPlan(_, connectionPlan) = r { return connectionPlan }
    logger.error("apiConnectPlan error: \(responseError(r))")
    throw r
}

func apiConnect(incognito: Bool, connReq: String) async -> (ConnReqType, PendingContactConnection)? {
    let (r, alert) = await apiConnect_(incognito: incognito, connReq: connReq)
    if let alert = alert {
        AlertManager.shared.showAlert(alert)
        return nil
    } else {
        return r
    }
}

func apiConnect_(incognito: Bool, connReq: String) async -> ((ConnReqType, PendingContactConnection)?, Alert?) {
    guard let userId = ChatModel.shared.currentUser?.userId else {
        logger.error("apiConnect: no current user")
        return (nil, nil)
    }
    let r = await chatSendCmd(.apiConnect(userId: userId, incognito: incognito, connReq: connReq))
    let m = ChatModel.shared
    switch r {
    case let .sentConfirmation(_, connection):
        return ((.invitation, connection), nil)
    case let .sentInvitation(_, connection):
        return ((.contact, connection), nil)
    case let .contactAlreadyExists(_, contact):
        if let c = m.getContactChat(contact.contactId) {
            ItemsModel.shared.loadOpenChat(c.id)
        }
        let alert = contactAlreadyExistsAlert(contact)
        return (nil, alert)
    case .chatCmdError(_, .error(.invalidConnReq)):
        let alert = mkAlert(
            title: "Invalid connection link",
            message: "Please check that you used the correct link or ask your contact to send you another one."
        )
        return (nil, alert)
    case .chatCmdError(_, .errorAgent(.SMP(_, .AUTH))):
        let alert = mkAlert(
            title: "Connection error (AUTH)",
            message: "Unless your contact deleted the connection or this link was already used, it might be a bug - please report it.\nTo connect, please ask your contact to create another connection link and check that you have a stable network connection."
        )
        return (nil, alert)
    case .chatCmdError(_, .errorAgent(.SMP(_, .QUOTA))):
        let alert = mkAlert(
            title: "Undelivered messages",
            message: "The connection reached the limit of undelivered messages, your contact may be offline."
        )
        return (nil, alert)
    case let .chatCmdError(_, .errorAgent(.INTERNAL(internalErr))):
        if internalErr == "SEUniqueID" {
            let alert = mkAlert(
                title: "Already connected?",
                message: "It seems like you are already connected via this link. If it is not the case, there was an error (\(responseError(r)))."
            )
            return (nil, alert)
        }
    default: ()
    }
    let alert = connectionErrorAlert(r)
    return (nil, alert)
}

func contactAlreadyExistsAlert(_ contact: Contact) -> Alert {
    mkAlert(
        title: "Contact already exists",
        message: "You are already connected to \(contact.displayName)."
    )
}

private func connectionErrorAlert(_ r: ChatResponse) -> Alert {
    if let networkErrorAlert = networkErrorAlert(r) {
        return networkErrorAlert
    } else {
        return mkAlert(
            title: "Connection error",
            message: "Error: \(responseError(r))"
        )
    }
}

func apiConnectContactViaAddress(incognito: Bool, contactId: Int64) async -> (Contact?, Alert?) {
    guard let userId = ChatModel.shared.currentUser?.userId else {
        logger.error("apiConnectContactViaAddress: no current user")
        return (nil, nil)
    }
    let r = await chatSendCmd(.apiConnectContactViaAddress(userId: userId, incognito: incognito, contactId: contactId))
    if case let .sentInvitationToContact(_, contact, _) = r { return (contact, nil) }
    logger.error("apiConnectContactViaAddress error: \(responseError(r))")
    let alert = connectionErrorAlert(r)
    return (nil, alert)
}

func apiDeleteChat(type: ChatType, id: Int64, chatDeleteMode: ChatDeleteMode = .full(notify: true)) async throws {
    let chatId = type.rawValue + id.description
    DispatchQueue.main.async { ChatModel.shared.deletedChats.insert(chatId) }
    defer { DispatchQueue.main.async { ChatModel.shared.deletedChats.remove(chatId) } }
    let r = await chatSendCmd(.apiDeleteChat(type: type, id: id, chatDeleteMode: chatDeleteMode), bgTask: false)
    if case .direct = type, case .contactDeleted = r { return }
    if case .contactConnection = type, case .contactConnectionDeleted = r { return }
    if case .group = type, case .groupDeletedUser = r { return }
    throw r
}

func apiDeleteContact(id: Int64, chatDeleteMode: ChatDeleteMode = .full(notify: true)) async throws -> Contact {
    let type: ChatType = .direct
    let chatId = type.rawValue + id.description
    if case .full = chatDeleteMode {
        DispatchQueue.main.async { ChatModel.shared.deletedChats.insert(chatId) }
    }
    defer {
        if case .full = chatDeleteMode {
            DispatchQueue.main.async { ChatModel.shared.deletedChats.remove(chatId) }
        }
    }
    let r = await chatSendCmd(.apiDeleteChat(type: type, id: id, chatDeleteMode: chatDeleteMode), bgTask: false)
    if case let .contactDeleted(_, contact) = r { return contact }
    throw r
}

func deleteChat(_ chat: Chat, chatDeleteMode: ChatDeleteMode = .full(notify: true)) async {
    do {
        let cInfo = chat.chatInfo
        try await apiDeleteChat(type: cInfo.chatType, id: cInfo.apiId, chatDeleteMode: chatDeleteMode)
        await MainActor.run { ChatModel.shared.removeChat(cInfo.id) }
    } catch let error {
        logger.error("deleteChat apiDeleteChat error: \(responseError(error))")
        AlertManager.shared.showAlertMsg(
            title: "Error deleting chat!",
            message: "Error: \(responseError(error))"
        )
    }
}

func deleteContactChat(_ chat: Chat, chatDeleteMode: ChatDeleteMode = .full(notify: true)) async -> Alert? {
    do {
        let cInfo = chat.chatInfo
        let ct = try await apiDeleteContact(id: cInfo.apiId, chatDeleteMode: chatDeleteMode)
        await MainActor.run {
            switch chatDeleteMode {
            case .full:
                ChatModel.shared.removeChat(cInfo.id)
            case .entity:
                ChatModel.shared.removeChat(cInfo.id)
                ChatModel.shared.addChat(Chat(
                    chatInfo: .direct(contact: ct),
                    chatItems: chat.chatItems
                ))
            case .messages:
                ChatModel.shared.removeChat(cInfo.id)
                ChatModel.shared.addChat(Chat(
                    chatInfo: .direct(contact: ct),
                    chatItems: []
                ))
            }
        }
    } catch let error {
        logger.error("deleteContactChat apiDeleteContact error: \(responseError(error))")
        return mkAlert(
            title: "Error deleting chat!",
            message: "Error: \(responseError(error))"
        )
    }
    return nil
}


func apiClearChat(type: ChatType, id: Int64) async throws -> ChatInfo {
    let r = await chatSendCmd(.apiClearChat(type: type, id: id), bgTask: false)
    if case let .chatCleared(_, updatedChatInfo) = r { return updatedChatInfo }
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
    let userId = try currentUserId("apiListContacts")
    let r = chatSendCmdSync(.apiListContacts(userId: userId))
    if case let .contactsList(_, contacts) = r { return contacts }
    throw r
}

func apiUpdateProfile(profile: Profile) async throws -> (Profile, [Contact])? {
    let userId = try currentUserId("apiUpdateProfile")
    let r = await chatSendCmd(.apiUpdateProfile(userId: userId, profile: profile))
    switch r {
    case .userProfileNoChange: return (profile, [])
    case let .userProfileUpdated(_, _, toProfile, updateSummary): return (toProfile, updateSummary.changedContacts)
    case .chatCmdError(_, .errorStore(.duplicateName)): return nil;
    default: throw r
    }
}

func apiSetProfileAddress(on: Bool) async throws -> User? {
    let userId = try currentUserId("apiSetProfileAddress")
    let r = await chatSendCmd(.apiSetProfileAddress(userId: userId, on: on))
    switch r {
    case .userProfileNoChange: return nil
    case let .userProfileUpdated(user, _, _, _): return user
    default: throw r
    }
}

func apiSetContactPrefs(contactId: Int64, preferences: Preferences) async throws -> Contact? {
    let r = await chatSendCmd(.apiSetContactPrefs(contactId: contactId, preferences: preferences))
    if case let .contactPrefsUpdated(_, _, toContact) = r { return toContact }
    throw r
}

func apiSetContactAlias(contactId: Int64, localAlias: String) async throws -> Contact? {
    let r = await chatSendCmd(.apiSetContactAlias(contactId: contactId, localAlias: localAlias))
    if case let .contactAliasUpdated(_, toContact) = r { return toContact }
    throw r
}

func apiSetConnectionAlias(connId: Int64, localAlias: String) async throws -> PendingContactConnection? {
    let r = await chatSendCmd(.apiSetConnectionAlias(connId: connId, localAlias: localAlias))
    if case let .connectionAliasUpdated(_, toConnection) = r { return toConnection }
    throw r
}

func apiSetUserUIThemes(userId: Int64, themes: ThemeModeOverrides?) async -> Bool {
    let r = await chatSendCmd(.apiSetUserUIThemes(userId: userId, themes: themes))
    if case .cmdOk = r { return true }
    logger.error("apiSetUserUIThemes bad response: \(String(describing: r))")
    return false
}

func apiSetChatUIThemes(chatId: ChatId, themes: ThemeModeOverrides?) async -> Bool {
    let r = await chatSendCmd(.apiSetChatUIThemes(chatId: chatId, themes: themes))
    if case .cmdOk = r { return true }
    logger.error("apiSetChatUIThemes bad response: \(String(describing: r))")
    return false
}


func apiCreateUserAddress() async throws -> String {
    let userId = try currentUserId("apiCreateUserAddress")
    let r = await chatSendCmd(.apiCreateMyAddress(userId: userId))
    if case let .userContactLinkCreated(_, connReq) = r { return connReq }
    throw r
}

func apiDeleteUserAddress() async throws -> User? {
    let userId = try currentUserId("apiDeleteUserAddress")
    let r = await chatSendCmd(.apiDeleteMyAddress(userId: userId))
    if case let .userContactLinkDeleted(user) = r { return user }
    throw r
}

func apiGetUserAddress() throws -> UserContactLink? {
    let userId = try currentUserId("apiGetUserAddress")
    return try userAddressResponse(chatSendCmdSync(.apiShowMyAddress(userId: userId)))
}

func apiGetUserAddressAsync() async throws -> UserContactLink? {
    let userId = try currentUserId("apiGetUserAddressAsync")
    return try userAddressResponse(await chatSendCmd(.apiShowMyAddress(userId: userId)))
}

private func userAddressResponse(_ r: ChatResponse) throws -> UserContactLink? {
    switch r {
    case let .userContactLink(_, contactLink): return contactLink
    case .chatCmdError(_, chatError: .errorStore(storeError: .userContactLinkNotFound)): return nil
    default: throw r
    }
}

func userAddressAutoAccept(_ autoAccept: AutoAccept?) async throws -> UserContactLink? {
    let userId = try currentUserId("userAddressAutoAccept")
    let r = await chatSendCmd(.apiAddressAutoAccept(userId: userId, autoAccept: autoAccept))
    switch r {
    case let .userContactLinkUpdated(_, contactLink): return contactLink
    case .chatCmdError(_, chatError: .errorStore(storeError: .userContactLinkNotFound)): return nil
    default: throw r
    }
}

func apiAcceptContactRequest(incognito: Bool, contactReqId: Int64) async -> Contact? {
    let r = await chatSendCmd(.apiAcceptContact(incognito: incognito, contactReqId: contactReqId))
    let am = AlertManager.shared

    if case let .acceptingContactRequest(_, contact) = r { return contact }
    if case .chatCmdError(_, .errorAgent(.SMP(_, .AUTH))) = r {
        am.showAlertMsg(
            title: "Connection error (AUTH)",
            message: "Sender may have deleted the connection request."
        )
    } else if let networkErrorAlert = networkErrorAlert(r) {
        am.showAlert(networkErrorAlert)
    } else {
        logger.error("apiAcceptContactRequest error: \(String(describing: r))")
        am.showAlertMsg(
            title: "Error accepting contact request",
            message: "Error: \(responseError(r))"
        )
    }
    return nil
}

func apiRejectContactRequest(contactReqId: Int64) async throws {
    let r = await chatSendCmd(.apiRejectContact(contactReqId: contactReqId))
    if case .contactRequestRejected = r { return }
    throw r
}

func apiChatRead(type: ChatType, id: Int64) async throws {
    try await sendCommandOkResp(.apiChatRead(type: type, id: id))
}

func apiChatItemsRead(type: ChatType, id: Int64, itemIds: [Int64]) async throws {
    try await sendCommandOkResp(.apiChatItemsRead(type: type, id: id, itemIds: itemIds))
}

func apiChatUnread(type: ChatType, id: Int64, unreadChat: Bool) async throws {
    try await sendCommandOkResp(.apiChatUnread(type: type, id: id, unreadChat: unreadChat))
}

func uploadStandaloneFile(user: any UserLike, file: CryptoFile, ctrl: chat_ctrl? = nil) async -> (FileTransferMeta?, String?) {
    let r = await chatSendCmd(.apiUploadStandaloneFile(userId: user.userId, file: file), ctrl)
    if case let .sndStandaloneFileCreated(_, fileTransferMeta) = r {
        return (fileTransferMeta, nil)
    } else {
        logger.error("uploadStandaloneFile error: \(String(describing: r))")
        return (nil, responseError(r))
    }
}

func downloadStandaloneFile(user: any UserLike, url: String, file: CryptoFile, ctrl: chat_ctrl? = nil) async -> (RcvFileTransfer?, String?) {
    let r = await chatSendCmd(.apiDownloadStandaloneFile(userId: user.userId, url: url, file: file), ctrl)
    if case let .rcvStandaloneFileCreated(_, rcvFileTransfer) = r {
        return (rcvFileTransfer, nil)
    } else {
        logger.error("downloadStandaloneFile error: \(String(describing: r))")
        return (nil, responseError(r))
    }
}

func standaloneFileInfo(url: String, ctrl: chat_ctrl? = nil) async -> MigrationFileLinkData? {
    let r = await chatSendCmd(.apiStandaloneFileInfo(url: url), ctrl)
    if case let .standaloneFileInfo(fileMeta) = r {
        return fileMeta
    } else {
        logger.error("standaloneFileInfo error: \(String(describing: r))")
        return nil
    }
}

func receiveFile(user: any UserLike, fileId: Int64, userApprovedRelays: Bool = false, auto: Bool = false) async {
    await receiveFiles(
        user: user,
        fileIds: [fileId],
        userApprovedRelays: userApprovedRelays,
        auto: auto
    )
}

func receiveFiles(user: any UserLike, fileIds: [Int64], userApprovedRelays: Bool = false, auto: Bool = false) async {
    var fileIdsToApprove = [Int64]()
    var srvsToApprove = Set<String>()
    var otherFileErrs = [ChatResponse]()

    for fileId in fileIds {
        let r = await chatSendCmd(
            .receiveFile(
                fileId: fileId,
                userApprovedRelays: userApprovedRelays || !privacyAskToApproveRelaysGroupDefault.get(),
                encrypted: privacyEncryptLocalFilesGroupDefault.get(),
                inline: nil
            )
        )
        switch r {
        case let .rcvFileAccepted(_, chatItem):
            await chatItemSimpleUpdate(user, chatItem)
        default:
            if let chatError = chatError(r) {
                switch chatError {
                case let .fileNotApproved(fileId, unknownServers):
                    fileIdsToApprove.append(fileId)
                    srvsToApprove.formUnion(unknownServers)
                default:
                    otherFileErrs.append(r)
                }
            }
        }
    }

    if !auto {
        let otherErrsStr = if otherFileErrs.isEmpty {
            ""
        } else if otherFileErrs.count == 1 {
            "\(otherFileErrs[0])"
        } else if otherFileErrs.count == 2 {
            "\(otherFileErrs[0])\n\(otherFileErrs[1])"
        } else {
            "\(otherFileErrs[0])\n\(otherFileErrs[1])\nand \(otherFileErrs.count - 2) other error(s)"
        }

        // If there are not approved files, alert is shown the same way both in case of singular and plural files reception
        if !fileIdsToApprove.isEmpty {
            let srvs = srvsToApprove
                .map { s in
                    if let srv = parseServerAddress(s), !srv.hostnames.isEmpty {
                        srv.hostnames[0]
                    } else {
                        serverHost(s)
                    }
                }
                .sorted()
                .joined(separator: ", ")
            let fIds = fileIdsToApprove
            await MainActor.run {
                showAlert(
                    title: NSLocalizedString("Unknown servers!", comment: "alert title"),
                    message: (
                        String.localizedStringWithFormat(NSLocalizedString("Without Tor or VPN, your IP address will be visible to these XFTP relays: %@.", comment: "alert message"), srvs) +
                        (otherErrsStr != "" ? "\n\n" + String.localizedStringWithFormat(NSLocalizedString("Other file errors:\n%@", comment: "alert message"), otherErrsStr) : "")
                    ),
                    buttonTitle: NSLocalizedString("Download", comment: "alert button"),
                    buttonAction: {
                        Task {
                            logger.debug("apiReceiveFile fileNotApproved alert - in Task")
                            if let user = ChatModel.shared.currentUser {
                                await receiveFiles(user: user, fileIds: fIds, userApprovedRelays: true)
                            }
                        }
                    },
                    cancelButton: true
                )
            }
        } else if otherFileErrs.count == 1 { // If there is a single other error, we differentiate on it
            let errorResponse = otherFileErrs.first!
            switch errorResponse {
            case let .rcvFileAcceptedSndCancelled(_, rcvFileTransfer):
                logger.debug("receiveFiles error: sender cancelled file transfer \(rcvFileTransfer.fileId)")
                await MainActor.run {
                    showAlert(
                        NSLocalizedString("Cannot receive file", comment: "alert title"),
                        message: NSLocalizedString("Sender cancelled file transfer.", comment: "alert message")
                    )
                }
            default:
                if let chatError = chatError(errorResponse) {
                    switch chatError {
                    case .fileCancelled, .fileAlreadyReceiving:
                        logger.debug("receiveFiles ignoring FileCancelled or FileAlreadyReceiving error")
                    default:
                        await MainActor.run {
                            showAlert(
                                NSLocalizedString("Error receiving file", comment: "alert title"),
                                message: responseError(errorResponse)
                            )
                        }
                    }
                }
            }
        } else if otherFileErrs.count > 1 { // If there are multiple other errors, we show general alert
            await MainActor.run {
                showAlert(
                    NSLocalizedString("Error receiving file", comment: "alert title"),
                    message: String.localizedStringWithFormat(NSLocalizedString("File errors:\n%@", comment: "alert message"), otherErrsStr)
                )
            }
        }
    }
}

func cancelFile(user: User, fileId: Int64) async {
    if let chatItem = await apiCancelFile(fileId: fileId) {
        await chatItemSimpleUpdate(user, chatItem)
        cleanupFile(chatItem)
    }
}

func apiCancelFile(fileId: Int64, ctrl: chat_ctrl? = nil) async -> AChatItem? {
    let r = await chatSendCmd(.cancelFile(fileId: fileId), ctrl)
    switch r {
    case let .sndFileCancelled(_, chatItem, _, _) : return chatItem
    case let .rcvFileCancelled(_, chatItem, _) : return chatItem
    default:
        logger.error("apiCancelFile error: \(String(describing: r))")
        return nil
    }
}

func setLocalDeviceName(_ displayName: String) throws {
    try sendCommandOkRespSync(.setLocalDeviceName(displayName: displayName))
}

func connectRemoteCtrl(desktopAddress: String) async throws -> (RemoteCtrlInfo?, CtrlAppInfo, String) {
    let r = await chatSendCmd(.connectRemoteCtrl(xrcpInvitation: desktopAddress))
    if case let .remoteCtrlConnecting(rc_, ctrlAppInfo, v) = r { return (rc_, ctrlAppInfo, v) }
    throw r
}

func findKnownRemoteCtrl() async throws {
    try await sendCommandOkResp(.findKnownRemoteCtrl)
}

func confirmRemoteCtrl(_ rcId: Int64) async throws -> (RemoteCtrlInfo?, CtrlAppInfo, String) {
    let r = await chatSendCmd(.confirmRemoteCtrl(remoteCtrlId: rcId))
    if case let .remoteCtrlConnecting(rc_, ctrlAppInfo, v) = r { return (rc_, ctrlAppInfo, v) }
    throw r
}

func verifyRemoteCtrlSession(_ sessCode: String) async throws -> RemoteCtrlInfo {
    let r = await chatSendCmd(.verifyRemoteCtrlSession(sessionCode: sessCode))
    if case let .remoteCtrlConnected(rc) = r { return rc }
    throw r
}

func listRemoteCtrls() throws -> [RemoteCtrlInfo] {
    let r = chatSendCmdSync(.listRemoteCtrls)
    if case let .remoteCtrlList(rcInfo) = r { return rcInfo }
    throw r
}

func stopRemoteCtrl() async throws {
    try await sendCommandOkResp(.stopRemoteCtrl)
}

func deleteRemoteCtrl(_ rcId: Int64) async throws {
    try await sendCommandOkResp(.deleteRemoteCtrl(remoteCtrlId: rcId))
}

func networkErrorAlert(_ r: ChatResponse) -> Alert? {
    if let alert = getNetworkErrorAlert(r) {
        return mkAlert(title: alert.title, message: alert.message)
    } else {
        return nil
    }
}

func acceptContactRequest(incognito: Bool, contactRequest: UserContactRequest) async {
    if let contact = await apiAcceptContactRequest(incognito: incognito, contactReqId: contactRequest.apiId) {
        let chat = Chat(chatInfo: ChatInfo.direct(contact: contact), chatItems: [])
        await MainActor.run {
            ChatModel.shared.replaceChat(contactRequest.id, chat)
            NetworkModel.shared.setContactNetworkStatus(contact, .connected)
        }
        if contact.sndReady {
            DispatchQueue.main.async {
                dismissAllSheets(animated: true) {
                    ItemsModel.shared.loadOpenChat(chat.id)
                }
            }
        }
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

func apiGetCallInvitationsSync() throws -> [RcvCallInvitation] {
    let r = chatSendCmdSync(.apiGetCallInvitations)
    if case let .callInvitations(invs) = r { return invs }
    throw r
}

func apiGetCallInvitations() async throws -> [RcvCallInvitation] {
    let r = await chatSendCmd(.apiGetCallInvitations)
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

func apiGetNetworkStatuses() throws -> [ConnNetworkStatus] {
    let r = chatSendCmdSync(.apiGetNetworkStatuses)
    if case let .networkStatuses(_, statuses) = r { return statuses }
    throw r
}

func markChatRead(_ chat: Chat) async {
    do {
        if chat.chatStats.unreadCount > 0 {
            let cInfo = chat.chatInfo
            try await apiChatRead(type: cInfo.chatType, id: cInfo.apiId)
            await MainActor.run {
                withAnimation { ChatModel.shared.markChatItemsRead(cInfo) }
            }
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
        await MainActor.run {
            withAnimation { ChatModel.shared.markChatUnread(cInfo, unreadChat: unreadChat) }
        }
    } catch {
        logger.error("markChatUnread apiChatUnread error: \(responseError(error))")
    }
}

func apiMarkChatItemsRead(_ cInfo: ChatInfo, _ itemIds: [ChatItem.ID]) async {
    do {
        try await apiChatItemsRead(type: cInfo.chatType, id: cInfo.apiId, itemIds: itemIds)
        DispatchQueue.main.async {
            ChatModel.shared.markChatItemsRead(cInfo, itemIds)
        }
    } catch {
        logger.error("apiChatItemsRead error: \(responseError(error))")
    }
}

private func sendCommandOkResp(_ cmd: ChatCommand, _ ctrl: chat_ctrl? = nil) async throws {
    let r = await chatSendCmd(cmd, ctrl)
    if case .cmdOk = r { return }
    throw r
}

private func sendCommandOkRespSync(_ cmd: ChatCommand) throws {
    let r = chatSendCmdSync(cmd)
    if case .cmdOk = r { return }
    throw r
}

func apiNewGroup(incognito: Bool, groupProfile: GroupProfile) throws -> GroupInfo {
    let userId = try currentUserId("apiNewGroup")
    let r = chatSendCmdSync(.apiNewGroup(userId: userId, incognito: incognito, groupProfile: groupProfile))
    if case let .groupCreated(_, groupInfo) = r { return groupInfo }
    throw r
}

func apiAddMember(_ groupId: Int64, _ contactId: Int64, _ memberRole: GroupMemberRole) async throws -> GroupMember {
    let r = await chatSendCmd(.apiAddMember(groupId: groupId, contactId: contactId, memberRole: memberRole))
    if case let .sentGroupInvitation(_, _, _, member) = r { return member }
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
    case let .userAcceptedGroupSent(_, groupInfo, _): return .joined(groupInfo: groupInfo)
    case .chatCmdError(_, .errorAgent(.SMP(_, .AUTH))): return .invitationRemoved
    case .chatCmdError(_, .errorStore(.groupNotFound)): return .groupNotFound
    default: throw r
    }
}

func apiRemoveMember(_ groupId: Int64, _ memberId: Int64) async throws -> GroupMember {
    let r = await chatSendCmd(.apiRemoveMember(groupId: groupId, memberId: memberId), bgTask: false)
    if case let .userDeletedMember(_, _, member) = r { return member }
    throw r
}

func apiMemberRole(_ groupId: Int64, _ memberId: Int64, _ memberRole: GroupMemberRole) async throws -> GroupMember {
    let r = await chatSendCmd(.apiMemberRole(groupId: groupId, memberId: memberId, memberRole: memberRole), bgTask: false)
    if case let .memberRoleUser(_, _, member, _, _) = r { return member }
    throw r
}

func apiBlockMemberForAll(_ groupId: Int64, _ memberId: Int64, _ blocked: Bool) async throws -> GroupMember {
    let r = await chatSendCmd(.apiBlockMemberForAll(groupId: groupId, memberId: memberId, blocked: blocked), bgTask: false)
    if case let .memberBlockedForAllUser(_, _, member, _) = r { return member }
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
    if case let .leftMemberUser(_, groupInfo) = r { return groupInfo }
    throw r
}

func apiListMembers(_ groupId: Int64) async -> [GroupMember] {
    let r = await chatSendCmd(.apiListMembers(groupId: groupId))
    if case let .groupMembers(_, group) = r { return group.members }
    return []
}

func filterMembersToAdd(_ ms: [GMember]) -> [Contact] {
    let memberContactIds = ms.compactMap{ m in m.wrapped.memberCurrent ? m.wrapped.memberContactId : nil }
    return ChatModel.shared.chats
        .compactMap{ $0.chatInfo.contact }
        .filter{ c in c.sendMsgEnabled && !c.nextSendGrpInv && !memberContactIds.contains(c.apiId) }
        .sorted{ $0.displayName.lowercased() < $1.displayName.lowercased() }
}

func apiUpdateGroup(_ groupId: Int64, _ groupProfile: GroupProfile) async throws -> GroupInfo {
    let r = await chatSendCmd(.apiUpdateGroupProfile(groupId: groupId, groupProfile: groupProfile))
    if case let .groupUpdated(_, toGroup) = r { return toGroup }
    throw r
}

func apiCreateGroupLink(_ groupId: Int64, memberRole: GroupMemberRole = .member) async throws -> (String, GroupMemberRole) {
    let r = await chatSendCmd(.apiCreateGroupLink(groupId: groupId, memberRole: memberRole))
    if case let .groupLinkCreated(_, _, connReq, memberRole) = r { return (connReq, memberRole) }
    throw r
}

func apiGroupLinkMemberRole(_ groupId: Int64, memberRole: GroupMemberRole = .member) async throws -> (String, GroupMemberRole) {
    let r = await chatSendCmd(.apiGroupLinkMemberRole(groupId: groupId, memberRole: memberRole))
    if case let .groupLink(_, _, connReq, memberRole) = r { return (connReq, memberRole) }
    throw r
}

func apiDeleteGroupLink(_ groupId: Int64) async throws {
    let r = await chatSendCmd(.apiDeleteGroupLink(groupId: groupId))
    if case .groupLinkDeleted = r { return }
    throw r
}

func apiGetGroupLink(_ groupId: Int64) throws -> (String, GroupMemberRole)? {
    let r = chatSendCmdSync(.apiGetGroupLink(groupId: groupId))
    switch r {
    case let .groupLink(_, _, connReq, memberRole):
        return (connReq, memberRole)
    case .chatCmdError(_, chatError: .errorStore(storeError: .groupLinkNotFound)):
        return nil
    default: throw r
    }
}

func apiCreateMemberContact(_ groupId: Int64, _ groupMemberId: Int64) async throws -> Contact {
    let r = await chatSendCmd(.apiCreateMemberContact(groupId: groupId, groupMemberId: groupMemberId))
    if case let .newMemberContact(_, contact, _, _) = r { return contact }
    throw r
}

func apiSendMemberContactInvitation(_ contactId: Int64, _ msg: MsgContent) async throws -> Contact {
    let r = await chatSendCmd(.apiSendMemberContactInvitation(contactId: contactId, msg: msg), bgDelay: msgDelay)
    if case let .newMemberContactSentInv(_, contact, _, _) = r { return contact }
    throw r
}

func apiGetVersion() throws -> CoreVersionInfo {
    let r = chatSendCmdSync(.showVersion)
    if case let .versionInfo(info, _, _) = r { return info }
    throw r
}

func getAgentSubsTotal() async throws -> (SMPServerSubs, Bool) {
    let userId = try currentUserId("getAgentSubsTotal")
    let r = await chatSendCmd(.getAgentSubsTotal(userId: userId), log: false)
    if case let .agentSubsTotal(_, subsTotal, hasSession) = r { return (subsTotal, hasSession) }
    logger.error("getAgentSubsTotal error: \(String(describing: r))")
    throw r
}

func getAgentServersSummary() throws -> PresentedServersSummary {
    let userId = try currentUserId("getAgentServersSummary")
    let r = chatSendCmdSync(.getAgentServersSummary(userId: userId), log: false)
    if case let .agentServersSummary(_, serversSummary) = r { return serversSummary }
    logger.error("getAgentServersSummary error: \(String(describing: r))")
    throw r
}

func resetAgentServersStats() async throws {
    try await sendCommandOkResp(.resetAgentServersStats)
}

private func currentUserId(_ funcName: String) throws -> Int64 {
    if let userId = ChatModel.shared.currentUser?.userId {
        return userId
    }
    throw RuntimeError("\(funcName): no current user")
}

func initializeChat(start: Bool, confirmStart: Bool = false, dbKey: String? = nil, refreshInvitations: Bool = true, confirmMigrations: MigrationConfirmation? = nil) throws {
    logger.debug("initializeChat")
    let m = ChatModel.shared
    m.ctrlInitInProgress = true
    defer { m.ctrlInitInProgress = false }
    (m.chatDbEncrypted, m.chatDbStatus) = chatMigrateInit(dbKey, confirmMigrations: confirmMigrations)
    if  m.chatDbStatus != .ok { return }
    NetworkObserver.shared.restartMonitor()
    // If we migrated successfully means previous re-encryption process on database level finished successfully too
    if encryptionStartedDefault.get() {
        encryptionStartedDefault.set(false)
    }
    try apiSetAppFilePaths(filesFolder: getAppFilesDirectory().path, tempFolder: getTempFilesDirectory().path, assetsFolder: getWallpaperDirectory().deletingLastPathComponent().path)
    try apiSetEncryptLocalFiles(privacyEncryptLocalFilesGroupDefault.get())
    m.chatInitialized = true
    m.currentUser = try apiGetActiveUser()
    m.conditions = try getServerOperators()
    if shouldImportAppSettingsDefault.get() {
        do {
            let appSettings = try apiGetAppSettings(settings: AppSettings.current.prepareForExport())
            appSettings.importIntoApp()
            shouldImportAppSettingsDefault.set(false)
        } catch {
            logger.error("Error while importing app settings: \(error)")
        }
    }
    if m.currentUser == nil {
        onboardingStageDefault.set(.step1_SimpleXInfo)
        privacyDeliveryReceiptsSet.set(true)
        m.onboardingStage = .step1_SimpleXInfo
    } else if confirmStart {
        showStartChatAfterRestartAlert { start in
            do {
                if start { AppChatState.shared.set(.active) }
                try chatInitialized(start: start, refreshInvitations: refreshInvitations)
            } catch let error {
                logger.error("ChatInitialized error: \(error)")
            }
        }
    } else {
        try chatInitialized(start: start, refreshInvitations: refreshInvitations)
    }
}

func showStartChatAfterRestartAlert(result: @escaping (_ start: Bool) -> Void) {
    AlertManager.shared.showAlert(Alert(
        title: Text("Start chat?"),
        message: Text("Chat is stopped. If you already used this database on another device, you should transfer it back before starting chat."),
        primaryButton: .default(Text("Ok")) {
            result(true)
        },
        secondaryButton: .cancel {
            result(false)
        }
    ))
}

private func chatInitialized(start: Bool, refreshInvitations: Bool) throws {
    let m = ChatModel.shared
    if m.currentUser == nil { return }
    if start {
        try startChat(refreshInvitations: refreshInvitations)
    } else {
        m.chatRunning = false
        try getUserChatData()
        NtfManager.shared.setNtfBadgeCount(m.totalUnreadCountForAllUsers())
        m.onboardingStage = onboardingStageDefault.get()
    }
}

func startChat(refreshInvitations: Bool = true, onboarding: Bool = false) throws {
    logger.debug("startChat")
    let m = ChatModel.shared
    try setNetworkConfig(getNetCfg())
    let chatRunning = try apiCheckChatRunning()
    m.users = try listUsers()
    if !chatRunning {
        try getUserChatData()
        NtfManager.shared.setNtfBadgeCount(m.totalUnreadCountForAllUsers())
        if (refreshInvitations) {
            Task { try await refreshCallInvitations() }
        }
        (m.savedToken, m.tokenStatus, m.notificationMode, m.notificationServer) = apiGetNtfToken()
        _ = try apiStartChat()
        // deviceToken is set when AppDelegate.application(didRegisterForRemoteNotificationsWithDeviceToken:) is called,
        // when it is called before startChat
        if let token = m.deviceToken {
            registerToken(token: token)
        }
        if !onboarding {
            withAnimation {
                let savedOnboardingStage = onboardingStageDefault.get()
                m.onboardingStage = [.step1_SimpleXInfo, .step2_CreateProfile].contains(savedOnboardingStage) && m.users.count == 1
                ? .step3_ChooseServerOperators
                : savedOnboardingStage
                if m.onboardingStage == .onboardingComplete && !privacyDeliveryReceiptsSet.get() {
                    m.setDeliveryReceipts = true
                }
            }
        }
    }
    ChatReceiver.shared.start()
    m.chatRunning = true
    chatLastStartGroupDefault.set(Date.now)
}

func startChatWithTemporaryDatabase(ctrl: chat_ctrl) throws -> User? {
    logger.debug("startChatWithTemporaryDatabase")
    let migrationActiveUser = try? apiGetActiveUser(ctrl: ctrl) ?? apiCreateActiveUser(Profile(displayName: "Temp", fullName: ""), ctrl: ctrl)
    try setNetworkConfig(getNetCfg(), ctrl: ctrl)
    try apiSetAppFilePaths(filesFolder: getMigrationTempFilesDirectory().path, tempFolder: getMigrationTempFilesDirectory().path, assetsFolder: getWallpaperDirectory().deletingLastPathComponent().path, ctrl: ctrl)
    _ = try apiStartChat(ctrl: ctrl)
    return migrationActiveUser
}

func changeActiveUser(_ userId: Int64, viewPwd: String?) {
    do {
        try changeActiveUser_(userId, viewPwd: viewPwd)
    } catch let error {
        logger.error("Unable to set active user: \(responseError(error))")
    }
}

private func changeActiveUser_(_ userId: Int64, viewPwd: String?) throws {
    let m = ChatModel.shared
    m.currentUser = try apiSetActiveUser(userId, viewPwd: viewPwd)
    m.users = try listUsers()
    try getUserChatData()
}

func changeActiveUserAsync_(_ userId: Int64?, viewPwd: String?) async throws {
    let currentUser = if let userId = userId {
        try await apiSetActiveUserAsync(userId, viewPwd: viewPwd)
    } else {
        try apiGetActiveUser()
    }
    let users = try await listUsersAsync()
    await MainActor.run {
        let m = ChatModel.shared
        m.currentUser = currentUser
        m.users = users
    }
    try await getUserChatDataAsync()
    await MainActor.run {
        if let currentUser = currentUser, var (_, invitation) = ChatModel.shared.callInvitations.first(where: { _, inv in inv.user.userId == userId }) {
            invitation.user = currentUser
            activateCall(invitation)
        }
    }
}

func getUserChatData() throws {
    let m = ChatModel.shared
    m.userAddress = try apiGetUserAddress()
    m.chatItemTTL = try getChatItemTTL()
    let chats = try apiGetChats()
    m.updateChats(chats)
}

private func getUserChatDataAsync() async throws {
    let m = ChatModel.shared
    if m.currentUser != nil {
        let userAddress = try await apiGetUserAddressAsync()
        let chatItemTTL = try await getChatItemTTLAsync()
        let chats = try await apiGetChatsAsync()
        await MainActor.run {
            m.userAddress = userAddress
            m.chatItemTTL = chatItemTTL
            m.updateChats(chats)
        }
    } else {
        await MainActor.run {
            m.userAddress = nil
            m.updateChats([])
        }
    }
}

class ChatReceiver {
    private var receiveLoop: Task<Void, Never>?
    private var receiveMessages = true
    private var _lastMsgTime = Date.now

    var messagesChannel: ((ChatResponse) -> Void)? = nil

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
        while self.receiveMessages {
            if let msg = await chatRecvMsg() {
                self._lastMsgTime = .now
                await processReceivedMsg(msg)
                if let messagesChannel {
                    messagesChannel(msg)
                }
            }
            _ = try? await Task.sleep(nanoseconds: 7_500_000)
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
    Task {
        await TerminalItems.shared.add(.resp(.now, res))
    }
    let m = ChatModel.shared
    let n = NetworkModel.shared
    logger.debug("processReceivedMsg: \(res.responseType)")
    switch res {
    case let .contactDeletedByContact(user, contact):
        if active(user) && contact.directOrUsed {
            await MainActor.run {
                m.updateContact(contact)
            }
        }
    case let .contactConnected(user, contact, _):
        if active(user) && contact.directOrUsed {
            await MainActor.run {
                m.updateContact(contact)
                if let conn = contact.activeConn {
                    m.dismissConnReqView(conn.id)
                    m.removeChat(conn.id)
                }
            }
        }
        if contact.directOrUsed {
            NtfManager.shared.notifyContactConnected(user, contact)
        }
        await MainActor.run {
            n.setContactNetworkStatus(contact, .connected)
        }
    case let .contactConnecting(user, contact):
        if active(user) && contact.directOrUsed {
            await MainActor.run {
                m.updateContact(contact)
                if let conn = contact.activeConn {
                    m.dismissConnReqView(conn.id)
                    m.removeChat(conn.id)
                }
            }
        }
    case let .contactSndReady(user, contact):
        if active(user) && contact.directOrUsed {
            await MainActor.run {
                m.updateContact(contact)
                if let conn = contact.activeConn {
                    m.dismissConnReqView(conn.id)
                    m.removeChat(conn.id)
                }
            }
        }
        await MainActor.run {
            n.setContactNetworkStatus(contact, .connected)
        }
    case let .receivedContactRequest(user, contactRequest):
        if active(user) {
            let cInfo = ChatInfo.contactRequest(contactRequest: contactRequest)
            await MainActor.run {
                if m.hasChat(contactRequest.id) {
                    m.updateChatInfo(cInfo)
                } else {
                    m.addChat(Chat(
                        chatInfo: cInfo,
                        chatItems: []
                    ))
                }
            }
        }
        NtfManager.shared.notifyContactRequest(user, contactRequest)
    case let .contactUpdated(user, toContact):
        if active(user) && m.hasChat(toContact.id) {
            await MainActor.run {
                let cInfo = ChatInfo.direct(contact: toContact)
                m.updateChatInfo(cInfo)
            }
        }
    case let .groupMemberUpdated(user, groupInfo, _, toMember):
        if active(user) {
            await MainActor.run {
                _ = m.upsertGroupMember(groupInfo, toMember)
            }
        }
    case let .contactsMerged(user, intoContact, mergedContact):
        if active(user) && m.hasChat(mergedContact.id) {
            await MainActor.run {
                if m.chatId == mergedContact.id {
                    ItemsModel.shared.loadOpenChat(mergedContact.id)
                }
                m.removeChat(mergedContact.id)
            }
        }
    case let .networkStatus(status, connections):
        // dispatch queue to synchronize access
        networkStatusesLock.sync {
            var ns = n.networkStatuses
            // slow loop is on the background thread
            for cId in connections {
                ns[cId] = status
            }
            // fast model update is on the main thread
            DispatchQueue.main.sync {
                n.networkStatuses = ns
            }
        }
    case let .networkStatuses(_, statuses): ()
        // dispatch queue to synchronize access
        networkStatusesLock.sync {
            var ns = n.networkStatuses
            // slow loop is on the background thread
            for s in statuses {
                ns[s.agentConnId] = s.networkStatus
            }
            // fast model update is on the main thread
            DispatchQueue.main.sync {
                n.networkStatuses = ns
            }
        }
    case let .newChatItems(user, chatItems):
        for chatItem in chatItems {
            let cInfo = chatItem.chatInfo
            let cItem = chatItem.chatItem
            await MainActor.run {
                if active(user) {
                    m.addChatItem(cInfo, cItem)
                } else if cItem.isRcvNew && cInfo.ntfsEnabled {
                    m.increaseUnreadCounter(user: user)
                }
            }
            if let file = cItem.autoReceiveFile() {
                Task {
                    await receiveFile(user: user, fileId: file.fileId, auto: true)
                }
            }
            if cItem.showNotification {
                NtfManager.shared.notifyMessageReceived(user, cInfo, cItem)
            }
        }
    case let .chatItemsStatusesUpdated(user, chatItems):
        for chatItem in chatItems {
            let cInfo = chatItem.chatInfo
            let cItem = chatItem.chatItem
            if !cItem.isDeletedContent && active(user) {
                await MainActor.run { m.updateChatItem(cInfo, cItem, status: cItem.meta.itemStatus) }
            }
            if let endTask = m.messageDelivery[cItem.id] {
                switch cItem.meta.itemStatus {
                case .sndNew: ()
                case .sndSent: endTask()
                case .sndRcvd: endTask()
                case .sndErrorAuth: endTask()
                case .sndError: endTask()
                case .sndWarning: endTask()
                case .rcvNew: ()
                case .rcvRead: ()
                case .invalid: ()
                }
            }
        }
    case let .chatItemUpdated(user, aChatItem):
        await chatItemSimpleUpdate(user, aChatItem)
    case let .chatItemReaction(user, _, r):
        if active(user) {
            await MainActor.run {
                m.updateChatItem(r.chatInfo, r.chatReaction.chatItem)
            }
        }
    case let .chatItemsDeleted(user, items, _):
        if !active(user) {
            for item in items {
                if item.toChatItem == nil && item.deletedChatItem.chatItem.isRcvNew && item.deletedChatItem.chatInfo.ntfsEnabled {
                    await MainActor.run {
                        m.decreaseUnreadCounter(user: user)
                    }
                }
            }
            return
        }

        await MainActor.run {
            for item in items {
                if let toChatItem = item.toChatItem {
                    _ = m.upsertChatItem(toChatItem.chatInfo, toChatItem.chatItem)
                } else {
                    m.removeChatItem(item.deletedChatItem.chatInfo, item.deletedChatItem.chatItem)
                }
            }
        }
    case let .receivedGroupInvitation(user, groupInfo, _, _):
        if active(user) {
            await MainActor.run {
                m.updateGroup(groupInfo) // update so that repeat group invitations are not duplicated
                // NtfManager.shared.notifyContactRequest(contactRequest) // TODO notifyGroupInvitation?
            }
        }
    case let .userAcceptedGroupSent(user, groupInfo, hostContact):
        if !active(user) { return }

        await MainActor.run {
            m.updateGroup(groupInfo)
            if let conn = hostContact?.activeConn {
                m.dismissConnReqView(conn.id)
                m.removeChat(conn.id)
            }
        }
    case let .groupLinkConnecting(user, groupInfo, hostMember):
        if !active(user) { return }
        
        await MainActor.run {
            m.updateGroup(groupInfo)
            if let hostConn = hostMember.activeConn {
                m.dismissConnReqView(hostConn.id)
                m.removeChat(hostConn.id)
            }
        }
    case let .businessLinkConnecting(user, groupInfo, hostMember, fromContact):
        if !active(user) { return }

        await MainActor.run {
            m.updateGroup(groupInfo)
            m.removeChat(fromContact.id)
        }
    case let .joinedGroupMemberConnecting(user, groupInfo, _, member):
        if active(user) {
            await MainActor.run {
                _ = m.upsertGroupMember(groupInfo, member)
            }
        }
    case let .deletedMemberUser(user, groupInfo, _): // TODO update user member
        if active(user) {
            await MainActor.run {
                m.updateGroup(groupInfo)
            }
        }
    case let .deletedMember(user, groupInfo, _, deletedMember):
        if active(user) {
            await MainActor.run {
                _ = m.upsertGroupMember(groupInfo, deletedMember)
            }
        }
    case let .leftMember(user, groupInfo, member):
        if active(user) {
            await MainActor.run {
                _ = m.upsertGroupMember(groupInfo, member)
            }
        }
    case let .groupDeleted(user, groupInfo, _): // TODO update user member
        if active(user) {
            await MainActor.run {
                m.updateGroup(groupInfo)
            }
        }
    case let .userJoinedGroup(user, groupInfo):
        if active(user) {
            await MainActor.run {
                m.updateGroup(groupInfo)
            }
        }
    case let .joinedGroupMember(user, groupInfo, member):
        if active(user) {
            await MainActor.run {
                _ = m.upsertGroupMember(groupInfo, member)
            }
        }
    case let .connectedToGroupMember(user, groupInfo, member, memberContact):
        if active(user) {
            await MainActor.run {
                _ = m.upsertGroupMember(groupInfo, member)
            }
        }
        if let contact = memberContact {
            await MainActor.run {
                n.setContactNetworkStatus(contact, .connected)
            }
        }
    case let .groupUpdated(user, toGroup):
        if active(user) {
            await MainActor.run {
                m.updateGroup(toGroup)
            }
        }
    case let .memberRole(user, groupInfo, byMember: _, member: member, fromRole: _, toRole: _):
        if active(user) {
            await MainActor.run {
                m.updateGroup(groupInfo)
                _ = m.upsertGroupMember(groupInfo, member)
            }
        }
    case let .memberBlockedForAll(user, groupInfo, byMember: _, member: member, blocked: _):
        if active(user) {
            await MainActor.run {
                m.updateGroup(groupInfo)
                _ = m.upsertGroupMember(groupInfo, member)
            }
        }
    case let .newMemberContactReceivedInv(user, contact, _, _):
        if active(user) {
            await MainActor.run {
                m.updateContact(contact)
            }
        }
    case let .rcvFileAccepted(user, aChatItem): // usually rcvFileAccepted is a response, but it's also an event for XFTP files auto-accepted from NSE
        await chatItemSimpleUpdate(user, aChatItem)
    case let .rcvFileStart(user, aChatItem):
        await chatItemSimpleUpdate(user, aChatItem)
    case let .rcvFileComplete(user, aChatItem):
        await chatItemSimpleUpdate(user, aChatItem)
    case let .rcvFileSndCancelled(user, aChatItem, _):
        await chatItemSimpleUpdate(user, aChatItem)
        Task { cleanupFile(aChatItem) }
    case let .rcvFileProgressXFTP(user, aChatItem, _, _, _):
        if let aChatItem = aChatItem {
            await chatItemSimpleUpdate(user, aChatItem)
        }
    case let .rcvFileError(user, aChatItem, _, _):
        if let aChatItem = aChatItem {
            await chatItemSimpleUpdate(user, aChatItem)
            Task { cleanupFile(aChatItem) }
        }
    case let .rcvFileWarning(user, aChatItem, _, _):
        if let aChatItem = aChatItem {
            await chatItemSimpleUpdate(user, aChatItem)
        }
    case let .sndFileStart(user, aChatItem, _):
        await chatItemSimpleUpdate(user, aChatItem)
    case let .sndFileComplete(user, aChatItem, _):
        await chatItemSimpleUpdate(user, aChatItem)
        Task { cleanupDirectFile(aChatItem) }
    case let .sndFileRcvCancelled(user, aChatItem, _):
        if let aChatItem = aChatItem {
            await chatItemSimpleUpdate(user, aChatItem)
            Task { cleanupDirectFile(aChatItem) }
        }
    case let .sndFileProgressXFTP(user, aChatItem, _, _, _):
        if let aChatItem = aChatItem {
            await chatItemSimpleUpdate(user, aChatItem)
        }
    case let .sndFileCompleteXFTP(user, aChatItem, _):
        await chatItemSimpleUpdate(user, aChatItem)
    case let .sndFileError(user, aChatItem, _, _):
        if let aChatItem = aChatItem {
            await chatItemSimpleUpdate(user, aChatItem)
            Task { cleanupFile(aChatItem) }
        }
    case let .sndFileWarning(user, aChatItem, _, _):
        if let aChatItem = aChatItem {
            await chatItemSimpleUpdate(user, aChatItem)
        }
    case let .callInvitation(invitation):
        await MainActor.run {
            m.callInvitations[invitation.contact.id] = invitation
        }
        activateCall(invitation)
    case let .callOffer(_, contact, callType, offer, sharedKey, _):
        await withCall(contact) { call in
            await MainActor.run {
                call.callState = .offerReceived
                call.sharedKey = sharedKey
            }
            let useRelay = UserDefaults.standard.bool(forKey: DEFAULT_WEBRTC_POLICY_RELAY)
            let iceServers = getIceServers()
            logger.debug(".callOffer useRelay \(useRelay)")
            logger.debug(".callOffer iceServers \(String(describing: iceServers))")
            await m.callCommand.processCommand(.offer(
                offer: offer.rtcSession,
                iceCandidates: offer.rtcIceCandidates,
                media: callType.media, aesKey: sharedKey,
                iceServers: iceServers,
                relay: useRelay
            ))
        }
    case let .callAnswer(_, contact, answer):
        await withCall(contact) { call in
            await MainActor.run {
                call.callState = .answerReceived
            }
            await m.callCommand.processCommand(.answer(answer: answer.rtcSession, iceCandidates: answer.rtcIceCandidates))
        }
    case let .callExtraInfo(_, contact, extraInfo):
        await withCall(contact) { _ in
            await m.callCommand.processCommand(.ice(iceCandidates: extraInfo.rtcIceCandidates))
        }
    case let .callEnded(_, contact):
        if let invitation = await MainActor.run(body: { m.callInvitations.removeValue(forKey: contact.id) }) {
            CallController.shared.reportCallRemoteEnded(invitation: invitation)
        }
        await withCall(contact) { call in
            await m.callCommand.processCommand(.end)
            CallController.shared.reportCallRemoteEnded(call: call)
        }
    case .chatSuspended:
        chatSuspended()
    case let .contactSwitch(user, contact, switchProgress):
        if active(user) {
            await MainActor.run {
                m.updateContactConnectionStats(contact, switchProgress.connectionStats)
            }
        }
    case let .groupMemberSwitch(user, groupInfo, member, switchProgress):
        if active(user) {
            await MainActor.run {
                m.updateGroupMemberConnectionStats(groupInfo, member, switchProgress.connectionStats)
            }
        }
    case let .contactRatchetSync(user, contact, ratchetSyncProgress):
        if active(user) {
            await MainActor.run {
                m.updateContactConnectionStats(contact, ratchetSyncProgress.connectionStats)
            }
        }
    case let .groupMemberRatchetSync(user, groupInfo, member, ratchetSyncProgress):
        if active(user) {
            await MainActor.run {
                m.updateGroupMemberConnectionStats(groupInfo, member, ratchetSyncProgress.connectionStats)
            }
        }
    case let .contactDisabled(user, contact):
        if active(user) {
            await MainActor.run {
                m.updateContact(contact)
            }
        }
    case let .remoteCtrlFound(remoteCtrl, ctrlAppInfo_, appVersion, compatible):
        await MainActor.run {
            if let sess = m.remoteCtrlSession, case .searching = sess.sessionState {
                let state = UIRemoteCtrlSessionState.found(remoteCtrl: remoteCtrl, compatible: compatible)
                m.remoteCtrlSession = RemoteCtrlSession(
                    ctrlAppInfo: ctrlAppInfo_,
                    appVersion: appVersion,
                    sessionState: state
                )
            }
        }
    case let .remoteCtrlSessionCode(remoteCtrl_, sessionCode):
        await MainActor.run {
            let state = UIRemoteCtrlSessionState.pendingConfirmation(remoteCtrl_: remoteCtrl_, sessionCode: sessionCode)
            m.remoteCtrlSession = m.remoteCtrlSession?.updateState(state)
        }
    case let .remoteCtrlConnected(remoteCtrl):
        // TODO currently it is returned in response to command, so it is redundant
        await MainActor.run {
            let state = UIRemoteCtrlSessionState.connected(remoteCtrl: remoteCtrl, sessionCode: m.remoteCtrlSession?.sessionCode ?? "")
            m.remoteCtrlSession = m.remoteCtrlSession?.updateState(state)
        }
    case let .remoteCtrlStopped(_, rcStopReason):
        // This delay is needed to cancel the session that fails on network failure,
        // e.g. when user did not grant permission to access local network yet.
        if let sess = m.remoteCtrlSession {
            await MainActor.run {
                m.remoteCtrlSession = nil
                dismissAllSheets() {
                    switch rcStopReason {
                    case .disconnected:
                        ()
                    case .connectionFailed(.errorAgent(.RCP(.identity))):
                        AlertManager.shared.showAlertMsg(
                            title: "Connection with desktop stopped",
                            message: "This link was used with another mobile device, please create a new link on the desktop."
                        )
                    default:
                        AlertManager.shared.showAlert(Alert(
                            title: Text("Connection with desktop stopped"),
                            message: Text("Please check that mobile and desktop are connected to the same local network, and that desktop firewall allows the connection.\nPlease share any other issues with the developers."),
                            primaryButton: .default(Text("Ok")),
                            secondaryButton: .default(Text("Copy error")) { UIPasteboard.general.string = String(describing: rcStopReason) }
                        ))
                    }
                }
            }
            if case .connected = sess.sessionState {
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.1) {
                    switchToLocalSession()
                }
            }
        }
    case let .contactPQEnabled(user, contact, _):
        if active(user) {
            await MainActor.run {
                m.updateContact(contact)
            }
        }
    default:
        logger.debug("unsupported event: \(res.responseType)")
    }

    func withCall(_ contact: Contact, _ perform: (Call) async -> Void) async {
        if let call = m.activeCall, call.contact.apiId == contact.apiId {
            await perform(call)
        } else {
            logger.debug("processReceivedMsg: ignoring \(res.responseType), not in call with the contact \(contact.id)")
        }
    }
}

func switchToLocalSession() {
    let m = ChatModel.shared
    let n = NetworkModel.shared
    m.remoteCtrlSession = nil
    do {
        m.users = try listUsers()
        try getUserChatData()
        let statuses = (try apiGetNetworkStatuses()).map { s in (s.agentConnId, s.networkStatus) }
        n.networkStatuses = Dictionary(uniqueKeysWithValues: statuses)
    } catch let error {
        logger.debug("error updating chat data: \(responseError(error))")
    }
}

func active(_ user: any UserLike) -> Bool {
    user.userId == ChatModel.shared.currentUser?.id
}

func chatItemSimpleUpdate(_ user: any UserLike, _ aChatItem: AChatItem) async {
    let m = ChatModel.shared
    let cInfo = aChatItem.chatInfo
    let cItem = aChatItem.chatItem
    if active(user) {
        if await MainActor.run(body: { m.upsertChatItem(cInfo, cItem) }) {
            if cItem.showNotification {
                NtfManager.shared.notifyMessageReceived(user, cInfo, cItem)
            }
        }
    }
}

func refreshCallInvitations() async throws {
    let m = ChatModel.shared
    let callInvitations = try await apiGetCallInvitations()
    await MainActor.run {
        m.callInvitations = callsByChat(callInvitations)
        if let (chatId, ntfAction) = m.ntfCallInvitationAction,
           let invitation = m.callInvitations.removeValue(forKey: chatId) {
            m.ntfCallInvitationAction = nil
            CallController.shared.callAction(invitation: invitation, action: ntfAction)
        } else if let invitation = callInvitations.last(where: { $0.user.showNotifications }) {
            activateCall(invitation)
        }
    }
}

func justRefreshCallInvitations() async throws {
    let callInvitations = try apiGetCallInvitationsSync()
    await MainActor.run {
        ChatModel.shared.callInvitations = callsByChat(callInvitations)
    }
}

private func callsByChat(_ callInvitations: [RcvCallInvitation]) -> [ChatId: RcvCallInvitation] {
    callInvitations.reduce(into: [ChatId: RcvCallInvitation]()) {
        result, inv in result[inv.contact.id] = inv
    }
}

func activateCall(_ callInvitation: RcvCallInvitation) {
    let m = ChatModel.shared
    logger.debug("reportNewIncomingCall activeCallUUID \(String(describing: m.activeCall?.callUUID)) invitationUUID \(String(describing: callInvitation.callUUID))")
    if !callInvitation.user.showNotifications || m.activeCall?.callUUID == callInvitation.callUUID { return }
    CallController.shared.reportNewIncomingCall(invitation: callInvitation) { error in
        if let error = error {
            DispatchQueue.main.async {
                m.callInvitations[callInvitation.contact.id]?.callUUID = nil
            }
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
