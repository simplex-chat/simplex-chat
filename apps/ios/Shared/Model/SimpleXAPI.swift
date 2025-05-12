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
@preconcurrency import SimpleXChat

private var chatController: chat_ctrl?

private let networkStatusesLock = DispatchQueue(label: "chat.simplex.app.network-statuses.lock")

enum TerminalItem: Identifiable {
    case cmd(Date, ChatCommand)
    case res(Date, ChatAPIResult)
    case err(Date, ChatError)
    case bad(Date, String, Data?)

    var id: Date {
        switch self {
        case let .cmd(d, _): d
        case let .res(d, _): d
        case let .err(d, _): d
        case let .bad(d, _, _): d
        }
    }

    var label: String {
        switch self {
        case let .cmd(_, cmd): "> \(cmd.cmdString.prefix(30))"
        case let .res(_, res): "< \(res.responseType)"
        case let .err(_, err): "< error \(err.errorType)"
        case let .bad(_, type, _): "< * \(type)"
        }
    }

    var details: String {
        switch self {
        case let .cmd(_, cmd): cmd.cmdString
        case let .res(_, res): res.details
        case let .err(_, err): String(describing: err)
        case let .bad(_, _, json): dataToString(json)
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

@inline(__always)
func chatSendCmdSync<R: ChatAPIResult>(_ cmd: ChatCommand, bgTask: Bool = true, bgDelay: Double? = nil, ctrl: chat_ctrl? = nil, log: Bool = true) throws -> R {
    let res: APIResult<R> = chatApiSendCmdSync(cmd, bgTask: bgTask, bgDelay: bgDelay, ctrl: ctrl, log: log)
    return try apiResult(res)
}

func chatApiSendCmdSync<R: ChatAPIResult>(_ cmd: ChatCommand, bgTask: Bool = true, bgDelay: Double? = nil, ctrl: chat_ctrl? = nil, log: Bool = true) -> APIResult<R> {
    if log {
        logger.debug("chatSendCmd \(cmd.cmdType)")
    }
    let start = Date.now
    let resp: APIResult<R> = bgTask
                ? withBGTask(bgDelay: bgDelay) { sendSimpleXCmd(cmd, ctrl) }
                : sendSimpleXCmd(cmd, ctrl)
    if log {
        logger.debug("chatSendCmd \(cmd.cmdType): \(resp.responseType)")
        if case let .invalid(_, json) = resp {
            logger.debug("chatSendCmd \(cmd.cmdType) response: \(dataToString(json))")
        }
        Task {
            await TerminalItems.shared.addCommand(start, cmd.obfuscated, resp)
        }
    }
    return resp
}

@inline(__always)
func chatSendCmd<R: ChatAPIResult>(_ cmd: ChatCommand, bgTask: Bool = true, bgDelay: Double? = nil, ctrl: chat_ctrl? = nil, log: Bool = true) async throws -> R {
    let res: APIResult<R> = await chatApiSendCmd(cmd, bgTask: bgTask, bgDelay: bgDelay, ctrl: ctrl, log: log)
    return try apiResult(res)
}

@inline(__always)
func chatApiSendCmd<R: ChatAPIResult>(_ cmd: ChatCommand, bgTask: Bool = true, bgDelay: Double? = nil, ctrl: chat_ctrl? = nil, log: Bool = true) async -> APIResult<R> {
    await withCheckedContinuation { cont in
        cont.resume(returning: chatApiSendCmdSync(cmd, bgTask: bgTask, bgDelay: bgDelay, ctrl: ctrl, log: log))
    }
}

@inline(__always)
func apiResult<R: ChatAPIResult>(_ res: APIResult<R>) throws -> R {
    switch res {
    case let .result(r): return r
    case let .error(e): throw e
    case let .invalid(type, _): throw ChatError.unexpectedResult(type: type)
    }
}

func chatRecvMsg(_ ctrl: chat_ctrl? = nil) async -> APIResult<ChatEvent>? {
    await withCheckedContinuation { cont in
        _  = withBGTask(bgDelay: msgDelay) { () -> APIResult<ChatEvent>? in
            let evt: APIResult<ChatEvent>? = recvSimpleXMsg(ctrl)
            cont.resume(returning: evt)
            return evt
        }
    }
}

func apiGetActiveUser(ctrl: chat_ctrl? = nil) throws -> User? {
    let r: APIResult<ChatResponse0> = chatApiSendCmdSync(.showActiveUser, ctrl: ctrl)
    switch r {
    case let .result(.activeUser(user)): return user
    case .error(.error(.noActiveUser)): return nil
    default: throw r.unexpected
    }
}

func apiCreateActiveUser(_ p: Profile?, pastTimestamp: Bool = false, ctrl: chat_ctrl? = nil) throws -> User {
    let r: ChatResponse0 = try chatSendCmdSync(.createActiveUser(profile: p, pastTimestamp: pastTimestamp), ctrl: ctrl)
    if case let .activeUser(user) = r { return user }
    throw r.unexpected
}

func listUsers() throws -> [UserInfo] {
    return try listUsersResponse(chatSendCmdSync(.listUsers))
}

func listUsersAsync() async throws -> [UserInfo] {
    return try listUsersResponse(await chatSendCmd(.listUsers))
}

private func listUsersResponse(_ r: ChatResponse0) throws -> [UserInfo] {
    if case let .usersList(users) = r {
        return users.sorted { $0.user.chatViewName.compare($1.user.chatViewName) == .orderedAscending }
    }
    throw r.unexpected
}

func apiSetActiveUser(_ userId: Int64, viewPwd: String?) throws -> User {
    let r: ChatResponse0 = try chatSendCmdSync(.apiSetActiveUser(userId: userId, viewPwd: viewPwd))
    if case let .activeUser(user) = r { return user }
    throw r.unexpected
}

func apiSetActiveUserAsync(_ userId: Int64, viewPwd: String?) async throws -> User {
    let r: ChatResponse0 = try await chatSendCmd(.apiSetActiveUser(userId: userId, viewPwd: viewPwd))
    if case let .activeUser(user) = r { return user }
    throw r.unexpected
}

func apiSetAllContactReceipts(enable: Bool) async throws {
    try await sendCommandOkResp(.setAllContactReceipts(enable: enable))
}

func apiSetUserContactReceipts(_ userId: Int64, userMsgReceiptSettings: UserMsgReceiptSettings) async throws {
    try await sendCommandOkResp(.apiSetUserContactReceipts(userId: userId, userMsgReceiptSettings: userMsgReceiptSettings))
}

func apiSetUserGroupReceipts(_ userId: Int64, userMsgReceiptSettings: UserMsgReceiptSettings) async throws {
    try await sendCommandOkResp(.apiSetUserGroupReceipts(userId: userId, userMsgReceiptSettings: userMsgReceiptSettings))
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
    let r: ChatResponse1 = try await chatSendCmd(cmd)
    if case let .userPrivacy(_, updatedUser) = r { return updatedUser }
    throw r.unexpected
}

func apiDeleteUser(_ userId: Int64, _ delSMPQueues: Bool, viewPwd: String?) async throws {
    try await sendCommandOkResp(.apiDeleteUser(userId: userId, delSMPQueues: delSMPQueues, viewPwd: viewPwd))
}

func apiStartChat(ctrl: chat_ctrl? = nil) throws -> Bool {
    let r: ChatResponse0 = try chatSendCmdSync(.startChat(mainApp: true, enableSndFiles: true), ctrl: ctrl)
    switch r {
    case .chatStarted: return true
    case .chatRunning: return false
    default: throw r.unexpected
    }
}

func apiCheckChatRunning() throws -> Bool {
    let r: ChatResponse0 = try chatSendCmdSync(.checkChatRunning)
    switch r {
    case .chatRunning: return true
    case .chatStopped: return false
    default: throw r.unexpected
    }
}

func apiStopChat() async throws {
    let r: ChatResponse0 = try await chatSendCmd(.apiStopChat)
    switch r {
    case .chatStopped: return
    default: throw r.unexpected
    }
}

func apiActivateChat() {
    chatReopenStore()
    do {
        try sendCommandOkRespSync(.apiActivateChat(restoreChat: true))
    } catch {
        logger.error("apiActivateChat error: \(responseError(error))")
    }
}

func apiSuspendChat(timeoutMicroseconds: Int) {
    do {
        try sendCommandOkRespSync(.apiSuspendChat(timeoutMicroseconds: timeoutMicroseconds))
    } catch {
        logger.error("apiSuspendChat error: \(responseError(error))")
    }
}

func apiSetAppFilePaths(filesFolder: String, tempFolder: String, assetsFolder: String, ctrl: chat_ctrl? = nil) throws {
    let r: ChatResponse2 = try chatSendCmdSync(.apiSetAppFilePaths(filesFolder: filesFolder, tempFolder: tempFolder, assetsFolder: assetsFolder), ctrl: ctrl)
    if case .cmdOk = r { return }
    throw r.unexpected
}

func apiSetEncryptLocalFiles(_ enable: Bool) throws {
    try sendCommandOkRespSync(.apiSetEncryptLocalFiles(enable: enable))
}

func apiSaveAppSettings(settings: AppSettings) throws {
    try sendCommandOkRespSync(.apiSaveSettings(settings: settings))
}

func apiGetAppSettings(settings: AppSettings) throws -> AppSettings {
    let r: ChatResponse2 = try chatSendCmdSync(.apiGetSettings(settings: settings))
    if case let .appSettings(settings) = r { return settings }
    throw r.unexpected
}

func apiExportArchive(config: ArchiveConfig) async throws -> [ArchiveError] {
    let r: ChatResponse2 = try await chatSendCmd(.apiExportArchive(config: config))
    if case let .archiveExported(archiveErrors) = r { return archiveErrors }
    throw r.unexpected
}

func apiImportArchive(config: ArchiveConfig) async throws -> [ArchiveError] {
    let r: ChatResponse2 = try await chatSendCmd(.apiImportArchive(config: config))
    if case let .archiveImported(archiveErrors) = r { return archiveErrors }
    throw r.unexpected
}

func apiDeleteStorage() async throws {
    try await sendCommandOkResp(.apiDeleteStorage)
}

func apiStorageEncryption(currentKey: String = "", newKey: String = "") async throws {
    try await sendCommandOkResp(.apiStorageEncryption(config: DBEncryptionConfig(currentKey: currentKey, newKey: newKey)))
}

func testStorageEncryption(key: String, ctrl: chat_ctrl? = nil) async throws {
    try await sendCommandOkResp(.testStorageEncryption(key: key), ctrl: ctrl)
}

func apiGetChats() throws -> [ChatData] {
    let userId = try currentUserId("apiGetChats")
    return try apiChatsResponse(chatSendCmdSync(.apiGetChats(userId: userId)))
}

func apiGetChatsAsync() async throws -> [ChatData] {
    let userId = try currentUserId("apiGetChats")
    return try apiChatsResponse(await chatSendCmd(.apiGetChats(userId: userId)))
}

private func apiChatsResponse(_ r: ChatResponse0) throws -> [ChatData] {
    if case let .apiChats(_, chats) = r { return chats }
    throw r.unexpected
}

func apiGetChatTags() throws -> [ChatTag] {
    let userId = try currentUserId("apiGetChatTags")
    let r: ChatResponse0 = try chatSendCmdSync(.apiGetChatTags(userId: userId))
    if case let .chatTags(_, tags) = r { return tags }
    throw r.unexpected
}

func apiGetChatTagsAsync() async throws -> [ChatTag] {
    let userId = try currentUserId("apiGetChatTags")
    let r: ChatResponse0 = try await chatSendCmd(.apiGetChatTags(userId: userId))
    if case let .chatTags(_, tags) = r { return tags }
    throw r.unexpected
}

let loadItemsPerPage = 50

func apiGetChat(chatId: ChatId, pagination: ChatPagination, search: String = "") async throws -> (Chat, NavigationInfo) {
    let r: ChatResponse0 = try await chatSendCmd(.apiGetChat(chatId: chatId, pagination: pagination, search: search))
    if case let .apiChat(_, chat, navInfo) = r { return (Chat.init(chat), navInfo ?? NavigationInfo()) }
    throw r.unexpected
}

func loadChat(chat: Chat, search: String = "", clearItems: Bool = true) async {
    await loadChat(chatId: chat.chatInfo.id, search: search, clearItems: clearItems)
}

func loadChat(chatId: ChatId, search: String = "", openAroundItemId: ChatItem.ID? = nil, clearItems: Bool = true) async {
    let m = ChatModel.shared
    let im = ItemsModel.shared
    await MainActor.run {
        m.chatItemStatuses = [:]
        if clearItems {
            im.reversedChatItems = []
            ItemsModel.shared.chatState.clear()
        }
    }
    await apiLoadMessages(chatId, openAroundItemId != nil ? .around(chatItemId: openAroundItemId!, count: loadItemsPerPage)  : (search == "" ? .initial(count: loadItemsPerPage) : .last(count: loadItemsPerPage)), im.chatState, search, openAroundItemId, { 0...0 })
}

func apiGetChatItemInfo(type: ChatType, id: Int64, itemId: Int64) async throws -> ChatItemInfo {
    let r: ChatResponse0 = try await chatSendCmd(.apiGetChatItemInfo(type: type, id: id, itemId: itemId))
    if case let .chatItemInfo(_, _, chatItemInfo) = r { return chatItemInfo }
    throw r.unexpected
}

func apiPlanForwardChatItems(type: ChatType, id: Int64, itemIds: [Int64]) async throws -> ([Int64], ForwardConfirmation?) {
    let r: ChatResponse1 = try await chatSendCmd(.apiPlanForwardChatItems(toChatType: type, toChatId: id, itemIds: itemIds))
    if case let .forwardPlan(_, chatItemIds, forwardConfimation) = r { return (chatItemIds, forwardConfimation) }
    throw r.unexpected
}

func apiForwardChatItems(toChatType: ChatType, toChatId: Int64, fromChatType: ChatType, fromChatId: Int64, itemIds: [Int64], ttl: Int?) async -> [ChatItem]? {
    let cmd: ChatCommand = .apiForwardChatItems(toChatType: toChatType, toChatId: toChatId, fromChatType: fromChatType, fromChatId: fromChatId, itemIds: itemIds, ttl: ttl)
    return await processSendMessageCmd(toChatType: toChatType, cmd: cmd)
}

func apiCreateChatTag(tag: ChatTagData) async throws -> [ChatTag] {
    let r: ChatResponse0 = try await chatSendCmd(.apiCreateChatTag(tag: tag))
    if case let .chatTags(_, userTags) = r {
        return userTags
    }
    throw r.unexpected
}

func apiSetChatTags(type: ChatType, id: Int64, tagIds: [Int64]) async throws -> ([ChatTag], [Int64]) {
    let r: ChatResponse0 = try await chatSendCmd(.apiSetChatTags(type: type, id: id, tagIds: tagIds))
    if case let .tagsUpdated(_, userTags, chatTags) = r {
        return (userTags, chatTags)
    }
    throw r.unexpected
}

func apiDeleteChatTag(tagId: Int64) async throws  {
    try await sendCommandOkResp(.apiDeleteChatTag(tagId: tagId))
}

func apiUpdateChatTag(tagId: Int64, tag: ChatTagData) async throws  {
    try await sendCommandOkResp(.apiUpdateChatTag(tagId: tagId, tagData: tag))
}

func apiReorderChatTags(tagIds: [Int64]) async throws {
    try await sendCommandOkResp(.apiReorderChatTags(tagIds: tagIds))
}

func apiSendMessages(type: ChatType, id: Int64, live: Bool = false, ttl: Int? = nil, composedMessages: [ComposedMessage]) async -> [ChatItem]? {
    let cmd: ChatCommand = .apiSendMessages(type: type, id: id, live: live, ttl: ttl, composedMessages: composedMessages)
    return await processSendMessageCmd(toChatType: type, cmd: cmd)
}

private func processSendMessageCmd(toChatType: ChatType, cmd: ChatCommand) async -> [ChatItem]? {
    let chatModel = ChatModel.shared
    let r: APIResult<ChatResponse1>
    if toChatType == .direct {
        var cItem: ChatItem? = nil
        let endTask = beginBGTask({
            if let cItem = cItem {
                DispatchQueue.main.async {
                    chatModel.messageDelivery.removeValue(forKey: cItem.id)
                }
            }
        })
        r = await chatApiSendCmd(cmd, bgTask: false)
        if case let .result(.newChatItems(_, aChatItems)) = r {
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
            sendMessageErrorAlert(r.unexpected)
        }
        endTask()
        return nil
    } else {
        r = await chatApiSendCmd(cmd, bgDelay: msgDelay)
        if case let .result(.newChatItems(_, aChatItems)) = r {
            return aChatItems.map { $0.chatItem }
        }
        sendMessageErrorAlert(r.unexpected)
        return nil
    }
}

func apiCreateChatItems(noteFolderId: Int64, composedMessages: [ComposedMessage]) async -> [ChatItem]? {
    let r: APIResult<ChatResponse1> = await chatApiSendCmd(.apiCreateChatItems(noteFolderId: noteFolderId, composedMessages: composedMessages))
    if case let .result(.newChatItems(_, aChatItems)) = r { return aChatItems.map { $0.chatItem } }
    createChatItemsErrorAlert(r.unexpected)
    return nil
}

func apiReportMessage(groupId: Int64, chatItemId: Int64, reportReason: ReportReason, reportText: String) async -> [ChatItem]? {
    let r: APIResult<ChatResponse1> = await chatApiSendCmd(.apiReportMessage(groupId: groupId, chatItemId: chatItemId, reportReason: reportReason, reportText: reportText))
    if case let .result(.newChatItems(_, aChatItems)) = r { return aChatItems.map { $0.chatItem } }

    logger.error("apiReportMessage error: \(String(describing: r))")
    AlertManager.shared.showAlertMsg(
        title: "Error creating report",
        message: "Error: \(responseError(r.unexpected))"
    )
    return nil
}

private func sendMessageErrorAlert(_ r: ChatError) {
    logger.error("send message error: \(String(describing: r))")
    AlertManager.shared.showAlertMsg(
        title: "Error sending message",
        message: "Error: \(responseError(r))"
    )
}

private func createChatItemsErrorAlert(_ r: ChatError) {
    logger.error("apiCreateChatItems error: \(String(describing: r))")
    AlertManager.shared.showAlertMsg(
        title: "Error creating message",
        message: "Error: \(responseError(r))"
    )
}

func apiUpdateChatItem(type: ChatType, id: Int64, itemId: Int64, updatedMessage: UpdatedMessage, live: Bool = false) async throws -> ChatItem {
    let r: ChatResponse1 = try await chatSendCmd(.apiUpdateChatItem(type: type, id: id, itemId: itemId, updatedMessage: updatedMessage, live: live), bgDelay: msgDelay)
    switch r {
    case let .chatItemUpdated(_, aChatItem): return aChatItem.chatItem
    case let .chatItemNotChanged(_, aChatItem): return aChatItem.chatItem
    default: throw r.unexpected
    }
}

func apiChatItemReaction(type: ChatType, id: Int64, itemId: Int64, add: Bool, reaction: MsgReaction) async throws -> ChatItem {
    let r: ChatResponse1 = try await chatSendCmd(.apiChatItemReaction(type: type, id: id, itemId: itemId, add: add, reaction: reaction), bgDelay: msgDelay)
    if case let .chatItemReaction(_, _, reaction) = r { return reaction.chatReaction.chatItem }
    throw r.unexpected
}

func apiGetReactionMembers(groupId: Int64, itemId: Int64, reaction: MsgReaction) async throws -> [MemberReaction] {
    let userId = try currentUserId("apiGetReactionMemebers")
    let r: ChatResponse1 = try await chatSendCmd(.apiGetReactionMembers(userId: userId, groupId: groupId, itemId: itemId, reaction: reaction ))
    if case let .reactionMembers(_, memberReactions) = r { return memberReactions }
    throw r.unexpected
}

func apiDeleteChatItems(type: ChatType, id: Int64, itemIds: [Int64], mode: CIDeleteMode) async throws -> [ChatItemDeletion] {
    let r: ChatResponse1 = try await chatSendCmd(.apiDeleteChatItem(type: type, id: id, itemIds: itemIds, mode: mode), bgDelay: msgDelay)
    if case let .chatItemsDeleted(_, items, _) = r { return items }
    throw r.unexpected
}

func apiDeleteMemberChatItems(groupId: Int64, itemIds: [Int64]) async throws -> [ChatItemDeletion] {
    let r: ChatResponse1 = try await chatSendCmd(.apiDeleteMemberChatItem(groupId: groupId, itemIds: itemIds), bgDelay: msgDelay)
    if case let .chatItemsDeleted(_, items, _) = r { return items }
    throw r.unexpected
}

func apiArchiveReceivedReports(groupId: Int64) async throws -> ChatResponse1 {
    let r: ChatResponse1 = try await chatSendCmd(.apiArchiveReceivedReports(groupId: groupId), bgDelay: msgDelay)
    if case .groupChatItemsDeleted = r { return r }
    throw r.unexpected
}

func apiDeleteReceivedReports(groupId: Int64, itemIds: [Int64], mode: CIDeleteMode) async throws -> [ChatItemDeletion] {
    let r: ChatResponse1 = try await chatSendCmd(.apiDeleteReceivedReports(groupId: groupId, itemIds: itemIds, mode: mode), bgDelay: msgDelay)
    if case let .chatItemsDeleted(_, chatItemDeletions, _) = r { return chatItemDeletions }
    throw r.unexpected
}

func apiGetNtfToken() -> (DeviceToken?, NtfTknStatus?, NotificationsMode, String?) {
    let r: APIResult<ChatResponse2> = chatApiSendCmdSync(.apiGetNtfToken)
    switch r {
    case let .result(.ntfToken(token, status, ntfMode, ntfServer)): return (token, status, ntfMode, ntfServer)
    case .error(.errorAgent(.CMD(.PROHIBITED, _))): return (nil, nil, .off, nil)
    default:
        logger.debug("apiGetNtfToken response: \(String(describing: r))")
        return (nil, nil, .off, nil)
    }
}

func apiRegisterToken(token: DeviceToken, notificationMode: NotificationsMode) async throws -> NtfTknStatus {
    let r: ChatResponse2 = try await chatSendCmd(.apiRegisterToken(token: token, notificationMode: notificationMode))
    if case let .ntfTokenStatus(status) = r { return status }
    throw r.unexpected
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
                await MainActor.run {
                    m.tokenStatus = status
                    if !status.workingToken {
                        m.reRegisterTknStatus = status
                    }
                }
            } catch let error {
                logger.error("registerToken apiRegisterToken error: \(responseError(error))")
            }
        }
    }
}

func tokenStatusInfo(_ status: NtfTknStatus, register: Bool) -> String {
    String.localizedStringWithFormat(NSLocalizedString("Token status: %@.", comment: "token status"), status.text)
    + "\n" + status.info(register: register)
}

func reRegisterToken(token: DeviceToken) {
    let m = ChatModel.shared
    let mode = m.notificationMode
    logger.debug("reRegisterToken \(mode.rawValue)")
    Task {
        do {
            let status = try await apiRegisterToken(token: token, notificationMode: mode)
            await MainActor.run {
                m.tokenStatus = status
                showAlert(
                    status.workingToken
                    ? NSLocalizedString("Notifications status", comment: "alert title")
                    : NSLocalizedString("Notifications error", comment: "alert title"),
                    message: tokenStatusInfo(status, register: false)
                )
            }
        } catch let error {
            logger.error("reRegisterToken apiRegisterToken error: \(responseError(error))")
            await MainActor.run {
                showAlert(
                    NSLocalizedString("Error registering for notifications", comment: "alert title"),
                    message: responseError(error)
                )
            }
        }
    }
}

func apiVerifyToken(token: DeviceToken, nonce: String, code: String) async throws {
    try await sendCommandOkResp(.apiVerifyToken(token: token, nonce: nonce, code: code))
}

func apiCheckToken(token: DeviceToken) async throws -> NtfTknStatus {
    let r: ChatResponse2 = try await chatSendCmd(.apiCheckToken(token: token))
    if case let .ntfTokenStatus(status) = r { return status }
    throw r.unexpected
}

func apiDeleteToken(token: DeviceToken) async throws {
    try await sendCommandOkResp(.apiDeleteToken(token: token))
}

func testProtoServer(server: String) async throws -> Result<(), ProtocolTestFailure> {
    let userId = try currentUserId("testProtoServer")
    let r: ChatResponse0 = try await chatSendCmd(.apiTestProtoServer(userId: userId, server: server))
    if case let .serverTestResult(_, _, testFailure) = r {
        if let t = testFailure {
            return .failure(t)
        }
        return .success(())
    }
    throw r.unexpected
}

func getServerOperators() async throws -> ServerOperatorConditions {
    let r: ChatResponse0 = try await chatSendCmd(.apiGetServerOperators)
    if case let .serverOperatorConditions(conditions) = r { return conditions }
    logger.error("getServerOperators error: \(String(describing: r))")
    throw r.unexpected
}

func getServerOperatorsSync() throws -> ServerOperatorConditions {
    let r: ChatResponse0 = try chatSendCmdSync(.apiGetServerOperators)
    if case let .serverOperatorConditions(conditions) = r { return conditions }
    logger.error("getServerOperators error: \(String(describing: r))")
    throw r.unexpected
}

func setServerOperators(operators: [ServerOperator]) async throws -> ServerOperatorConditions {
    let r: ChatResponse0 = try await chatSendCmd(.apiSetServerOperators(operators: operators))
    if case let .serverOperatorConditions(conditions) = r { return conditions }
    logger.error("setServerOperators error: \(String(describing: r))")
    throw r.unexpected
}

func getUserServers() async throws -> [UserOperatorServers] {
    let userId = try currentUserId("getUserServers")
    let r: ChatResponse0 = try await chatSendCmd(.apiGetUserServers(userId: userId))
    if case let .userServers(_, userServers) = r { return userServers }
    logger.error("getUserServers error: \(String(describing: r))")
    throw r.unexpected
}

func setUserServers(userServers: [UserOperatorServers]) async throws {
    let userId = try currentUserId("setUserServers")
    let r: ChatResponse2 = try await chatSendCmd(.apiSetUserServers(userId: userId, userServers: userServers))
    if case .cmdOk = r { return }
    logger.error("setUserServers error: \(String(describing: r))")
    throw r.unexpected
}

func validateServers(userServers: [UserOperatorServers]) async throws -> [UserServersError] {
    let userId = try currentUserId("validateServers")
    let r: ChatResponse0 = try await chatSendCmd(.apiValidateServers(userId: userId, userServers: userServers))
    if case let .userServersValidation(_, serverErrors) = r { return serverErrors }
    logger.error("validateServers error: \(String(describing: r))")
    throw r.unexpected
}

func getUsageConditions() async throws -> (UsageConditions, String?, UsageConditions?) {
    let r: ChatResponse0 = try await chatSendCmd(.apiGetUsageConditions)
    if case let .usageConditions(usageConditions, conditionsText, acceptedConditions) = r { return (usageConditions, conditionsText, acceptedConditions) }
    logger.error("getUsageConditions error: \(String(describing: r))")
    throw r.unexpected
}

func setConditionsNotified(conditionsId: Int64) async throws {
    let r: ChatResponse2 = try await chatSendCmd(.apiSetConditionsNotified(conditionsId: conditionsId))
    if case .cmdOk = r { return }
    logger.error("setConditionsNotified error: \(String(describing: r))")
    throw r.unexpected
}

func acceptConditions(conditionsId: Int64, operatorIds: [Int64]) async throws -> ServerOperatorConditions {
    let r: ChatResponse0 = try await chatSendCmd(.apiAcceptConditions(conditionsId: conditionsId, operatorIds: operatorIds))
    if case let .serverOperatorConditions(conditions) = r { return conditions }
    logger.error("acceptConditions error: \(String(describing: r))")
    throw r.unexpected
}

func getChatItemTTL() throws -> ChatItemTTL {
    let userId = try currentUserId("getChatItemTTL")
    return try chatItemTTLResponse(chatSendCmdSync(.apiGetChatItemTTL(userId: userId)))
}

func getChatItemTTLAsync() async throws -> ChatItemTTL {
    let userId = try currentUserId("getChatItemTTLAsync")
    return try chatItemTTLResponse(await chatSendCmd(.apiGetChatItemTTL(userId: userId)))
}

private func chatItemTTLResponse(_ r: ChatResponse0) throws -> ChatItemTTL {
    if case let .chatItemTTL(_, chatItemTTL) = r {
        if let ttl = chatItemTTL {
            return ChatItemTTL(ttl)
        } else {
            throw RuntimeError("chatItemTTLResponse: invalid ttl")
        }
    }
    throw r.unexpected
}

func setChatItemTTL(_ chatItemTTL: ChatItemTTL) async throws {
    let userId = try currentUserId("setChatItemTTL")
    try await sendCommandOkResp(.apiSetChatItemTTL(userId: userId, seconds: chatItemTTL.seconds))
}

func setChatTTL(chatType: ChatType, id: Int64, _ chatItemTTL: ChatTTL) async throws {
    let userId = try currentUserId("setChatItemTTL")
    try await sendCommandOkResp(.apiSetChatTTL(userId: userId, type: chatType, id: id, seconds: chatItemTTL.value))
}

func getNetworkConfig() async throws -> NetCfg? {
    let r: ChatResponse0 = try await chatSendCmd(.apiGetNetworkConfig)
    if case let .networkConfig(cfg) = r { return cfg }
    throw r.unexpected
}

func setNetworkConfig(_ cfg: NetCfg, ctrl: chat_ctrl? = nil) throws {
    let r: ChatResponse2 = try chatSendCmdSync(.apiSetNetworkConfig(networkConfig: cfg), ctrl: ctrl)
    if case .cmdOk = r { return }
    throw r.unexpected
}

func apiSetNetworkInfo(_ networkInfo: UserNetworkInfo) throws {
    let r: ChatResponse2 = try chatSendCmdSync(.apiSetNetworkInfo(networkInfo: networkInfo))
    if case .cmdOk = r { return }
    throw r.unexpected
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
    let r: ChatResponse0 = try await chatSendCmd(.apiContactInfo(contactId: contactId))
    if case let .contactInfo(_, _, connStats, customUserProfile) = r { return (connStats, customUserProfile) }
    throw r.unexpected
}

func apiGroupMemberInfoSync(_ groupId: Int64, _ groupMemberId: Int64) throws -> (GroupMember, ConnectionStats?) {
    let r: ChatResponse0 = try chatSendCmdSync(.apiGroupMemberInfo(groupId: groupId, groupMemberId: groupMemberId))
    if case let .groupMemberInfo(_, _, member, connStats_) = r { return (member, connStats_) }
    throw r.unexpected
}

func apiGroupMemberInfo(_ groupId: Int64, _ groupMemberId: Int64) async throws -> (GroupMember, ConnectionStats?) {
    let r: ChatResponse0 = try await chatSendCmd(.apiGroupMemberInfo(groupId: groupId, groupMemberId: groupMemberId))
    if case let .groupMemberInfo(_, _, member, connStats_) = r { return (member, connStats_) }
    throw r.unexpected
}

func apiContactQueueInfo(_ contactId: Int64) async throws -> (RcvMsgInfo?, ServerQueueInfo) {
    let r: ChatResponse0 = try await chatSendCmd(.apiContactQueueInfo(contactId: contactId))
    if case let .queueInfo(_, rcvMsgInfo, queueInfo) = r { return (rcvMsgInfo, queueInfo) }
    throw r.unexpected
}

func apiGroupMemberQueueInfo(_ groupId: Int64, _ groupMemberId: Int64) async throws -> (RcvMsgInfo?, ServerQueueInfo) {
    let r: ChatResponse0 = try await chatSendCmd(.apiGroupMemberQueueInfo(groupId: groupId, groupMemberId: groupMemberId))
    if case let .queueInfo(_, rcvMsgInfo, queueInfo) = r { return (rcvMsgInfo, queueInfo) }
    throw r.unexpected
}

func apiSwitchContact(contactId: Int64) throws -> ConnectionStats {
    let r: ChatResponse0 = try chatSendCmdSync(.apiSwitchContact(contactId: contactId))
    if case let .contactSwitchStarted(_, _, connectionStats) = r { return connectionStats }
    throw r.unexpected
}

func apiSwitchGroupMember(_ groupId: Int64, _ groupMemberId: Int64) throws -> ConnectionStats {
    let r: ChatResponse0 = try chatSendCmdSync(.apiSwitchGroupMember(groupId: groupId, groupMemberId: groupMemberId))
    if case let .groupMemberSwitchStarted(_, _, _, connectionStats) = r { return connectionStats }
    throw r.unexpected
}

func apiAbortSwitchContact(_ contactId: Int64) throws -> ConnectionStats {
    let r: ChatResponse0 = try chatSendCmdSync(.apiAbortSwitchContact(contactId: contactId))
    if case let .contactSwitchAborted(_, _, connectionStats) = r { return connectionStats }
    throw r.unexpected
}

func apiAbortSwitchGroupMember(_ groupId: Int64, _ groupMemberId: Int64) throws -> ConnectionStats {
    let r: ChatResponse0 = try chatSendCmdSync(.apiAbortSwitchGroupMember(groupId: groupId, groupMemberId: groupMemberId))
    if case let .groupMemberSwitchAborted(_, _, _, connectionStats) = r { return connectionStats }
    throw r.unexpected
}

func apiSyncContactRatchet(_ contactId: Int64, _ force: Bool) throws -> ConnectionStats {
    let r: ChatResponse0 = try chatSendCmdSync(.apiSyncContactRatchet(contactId: contactId, force: force))
    if case let .contactRatchetSyncStarted(_, _, connectionStats) = r { return connectionStats }
    throw r.unexpected
}

func apiSyncGroupMemberRatchet(_ groupId: Int64, _ groupMemberId: Int64, _ force: Bool) throws -> (GroupMember, ConnectionStats) {
    let r: ChatResponse0 = try chatSendCmdSync(.apiSyncGroupMemberRatchet(groupId: groupId, groupMemberId: groupMemberId, force: force))
    if case let .groupMemberRatchetSyncStarted(_, _, member, connectionStats) = r { return (member, connectionStats) }
    throw r.unexpected
}

func apiGetContactCode(_ contactId: Int64) async throws -> (Contact, String) {
    let r: ChatResponse0 = try await chatSendCmd(.apiGetContactCode(contactId: contactId))
    if case let .contactCode(_, contact, connectionCode) = r { return (contact, connectionCode) }
    throw r.unexpected
}

func apiGetGroupMemberCode(_ groupId: Int64, _ groupMemberId: Int64) async throws -> (GroupMember, String) {
    let r: ChatResponse0 = try await chatSendCmd(.apiGetGroupMemberCode(groupId: groupId, groupMemberId: groupMemberId))
    if case let .groupMemberCode(_, _, member, connectionCode) = r { return (member, connectionCode) }
    throw r.unexpected
}

func apiVerifyContact(_ contactId: Int64, connectionCode: String?) -> (Bool, String)? {
    let r: APIResult<ChatResponse0> = chatApiSendCmdSync(.apiVerifyContact(contactId: contactId, connectionCode: connectionCode))
    if case let .result(.connectionVerified(_, verified, expectedCode)) = r { return (verified, expectedCode) }
    logger.error("apiVerifyContact error: \(String(describing: r))")
    return nil
}

func apiVerifyGroupMember(_ groupId: Int64, _ groupMemberId: Int64, connectionCode: String?) -> (Bool, String)? {
    let r: APIResult<ChatResponse0> = chatApiSendCmdSync(.apiVerifyGroupMember(groupId: groupId, groupMemberId: groupMemberId, connectionCode: connectionCode))
    if case let .result(.connectionVerified(_, verified, expectedCode)) = r { return (verified, expectedCode) }
    logger.error("apiVerifyGroupMember error: \(String(describing: r))")
    return nil
}

func apiAddContact(incognito: Bool) async -> ((CreatedConnLink, PendingContactConnection)?, Alert?) {
    guard let userId = ChatModel.shared.currentUser?.userId else {
        logger.error("apiAddContact: no current user")
        return (nil, nil)
    }
    let short = UserDefaults.standard.bool(forKey: DEFAULT_PRIVACY_SHORT_LINKS)
    let r: APIResult<ChatResponse1> = await chatApiSendCmd(.apiAddContact(userId: userId, short: short, incognito: incognito), bgTask: false)
    if case let .result(.invitation(_, connLinkInv, connection)) = r { return ((connLinkInv, connection), nil) }
    let alert = connectionErrorAlert(r)
    return (nil, alert)
}

func apiSetConnectionIncognito(connId: Int64, incognito: Bool) async throws -> PendingContactConnection? {
    let r: ChatResponse1 = try await chatSendCmd(.apiSetConnectionIncognito(connId: connId, incognito: incognito))
    if case let .connectionIncognitoUpdated(_, toConnection) = r { return toConnection }
    throw r.unexpected
}

func apiChangeConnectionUser(connId: Int64, userId: Int64) async throws -> PendingContactConnection {
    let r: ChatResponse1 = try await chatSendCmd(.apiChangeConnectionUser(connId: connId, userId: userId))

    if case let .connectionUserChanged(_, _, toConnection, _) = r {return toConnection}
    throw r.unexpected
}

func apiConnectPlan(connLink: String) async -> ((CreatedConnLink, ConnectionPlan)?, Alert?) {
    guard let userId = ChatModel.shared.currentUser?.userId else {
        logger.error("apiConnectPlan: no current user")
        return (nil, nil)
    }
    let r: APIResult<ChatResponse1> = await chatApiSendCmd(.apiConnectPlan(userId: userId, connLink: connLink))
    if case let .result(.connectionPlan(_, connLink, connPlan)) = r { return ((connLink, connPlan), nil) }
    let alert = apiConnectResponseAlert(r.unexpected) ?? connectionErrorAlert(r)
    return (nil, alert)
}

func apiConnect(incognito: Bool, connLink: CreatedConnLink) async -> (ConnReqType, PendingContactConnection)? {
    let (r, alert) = await apiConnect_(incognito: incognito, connLink: connLink)
    if let alert = alert {
        AlertManager.shared.showAlert(alert)
        return nil
    } else {
        return r
    }
}

func apiConnect_(incognito: Bool, connLink: CreatedConnLink) async -> ((ConnReqType, PendingContactConnection)?, Alert?) {
    guard let userId = ChatModel.shared.currentUser?.userId else {
        logger.error("apiConnect: no current user")
        return (nil, nil)
    }
    let r: APIResult<ChatResponse1> = await chatApiSendCmd(.apiConnect(userId: userId, incognito: incognito, connLink: connLink))
    let m = ChatModel.shared
    switch r {
    case let .result(.sentConfirmation(_, connection)):
        return ((.invitation, connection), nil)
    case let .result(.sentInvitation(_, connection)):
        return ((.contact, connection), nil)
    case let .result(.contactAlreadyExists(_, contact)):
        if let c = m.getContactChat(contact.contactId) {
            ItemsModel.shared.loadOpenChat(c.id)
        }
        let alert = contactAlreadyExistsAlert(contact)
        return (nil, alert)
    default: ()
    }
    let alert = apiConnectResponseAlert(r.unexpected) ?? connectionErrorAlert(r)
    return (nil, alert)
}

private func apiConnectResponseAlert(_ r: ChatError) -> Alert? {
    switch r {
    case .error(.invalidConnReq):
        mkAlert(
            title: "Invalid connection link",
            message: "Please check that you used the correct link or ask your contact to send you another one."
        )
    case .error(.unsupportedConnReq):
        mkAlert(
            title: "Unsupported connection link",
            message: "This link requires a newer app version. Please upgrade the app or ask your contact to send a compatible link."
        )
    case .errorAgent(.SMP(_, .AUTH)):
        mkAlert(
            title: "Connection error (AUTH)",
            message: "Unless your contact deleted the connection or this link was already used, it might be a bug - please report it.\nTo connect, please ask your contact to create another connection link and check that you have a stable network connection."
        )
    case let .errorAgent(.SMP(_, .BLOCKED(info))):
        Alert(
            title: Text("Connection blocked"),
            message: Text("Connection is blocked by server operator:\n\(info.reason.text)"),
            primaryButton: .default(Text("Ok")),
            secondaryButton: .default(Text("How it works")) {
                DispatchQueue.main.async {
                    UIApplication.shared.open(contentModerationPostLink)
                }
            }
        )
    case .errorAgent(.SMP(_, .QUOTA)):
        mkAlert(
            title: "Undelivered messages",
            message: "The connection reached the limit of undelivered messages, your contact may be offline."
        )
    case let .errorAgent(.INTERNAL(internalErr)):
        if internalErr == "SEUniqueID" {
            mkAlert(
                title: "Already connected?",
                message: "It seems like you are already connected via this link. If it is not the case, there was an error (\(responseError(r)))."
            )
        } else {
            nil
        }
    default: nil
    }
}

func contactAlreadyExistsAlert(_ contact: Contact) -> Alert {
    mkAlert(
        title: "Contact already exists",
        message: "You are already connected to \(contact.displayName)."
    )
}

private func connectionErrorAlert<R>(_ r: APIResult<R>) -> Alert {
    if let networkErrorAlert = networkErrorAlert(r) {
        return networkErrorAlert
    } else {
        return mkAlert(
            title: "Connection error",
            message: "Error: \(responseError(r.unexpected))"
        )
    }
}

func apiConnectContactViaAddress(incognito: Bool, contactId: Int64) async -> (Contact?, Alert?) {
    guard let userId = ChatModel.shared.currentUser?.userId else {
        logger.error("apiConnectContactViaAddress: no current user")
        return (nil, nil)
    }
    let r: APIResult<ChatResponse1> = await chatApiSendCmd(.apiConnectContactViaAddress(userId: userId, incognito: incognito, contactId: contactId))
    if case let .result(.sentInvitationToContact(_, contact, _)) = r { return (contact, nil) }
    logger.error("apiConnectContactViaAddress error: \(responseError(r.unexpected))")
    let alert = connectionErrorAlert(r)
    return (nil, alert)
}

func apiDeleteChat(type: ChatType, id: Int64, chatDeleteMode: ChatDeleteMode = .full(notify: true)) async throws {
    let chatId = type.rawValue + id.description
    DispatchQueue.main.async { ChatModel.shared.deletedChats.insert(chatId) }
    defer { DispatchQueue.main.async { ChatModel.shared.deletedChats.remove(chatId) } }
    let r: ChatResponse1 = try await chatSendCmd(.apiDeleteChat(type: type, id: id, chatDeleteMode: chatDeleteMode), bgTask: false)
    if case .direct = type, case .contactDeleted = r { return }
    if case .contactConnection = type, case .contactConnectionDeleted = r { return }
    if case .group = type, case .groupDeletedUser = r { return }
    throw r.unexpected
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
    let r: ChatResponse1 = try await chatSendCmd(.apiDeleteChat(type: type, id: id, chatDeleteMode: chatDeleteMode), bgTask: false)
    if case let .contactDeleted(_, contact) = r { return contact }
    throw r.unexpected
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
    let r: ChatResponse1 = try await chatSendCmd(.apiClearChat(type: type, id: id), bgTask: false)
    if case let .chatCleared(_, updatedChatInfo) = r { return updatedChatInfo }
    throw r.unexpected
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
    let r: ChatResponse1 = try chatSendCmdSync(.apiListContacts(userId: userId))
    if case let .contactsList(_, contacts) = r { return contacts }
    throw r.unexpected
}

func apiUpdateProfile(profile: Profile) async throws -> (Profile, [Contact])? {
    let userId = try currentUserId("apiUpdateProfile")
    let r: APIResult<ChatResponse1> = await chatApiSendCmd(.apiUpdateProfile(userId: userId, profile: profile))
    switch r {
    case .result(.userProfileNoChange): return (profile, [])
    case let .result(.userProfileUpdated(_, _, toProfile, updateSummary)): return (toProfile, updateSummary.changedContacts)
    case .error(.errorStore(.duplicateName)): return nil;
    default: throw r.unexpected
    }
}

func apiSetProfileAddress(on: Bool) async throws -> User? {
    let userId = try currentUserId("apiSetProfileAddress")
    let r: ChatResponse1 = try await chatSendCmd(.apiSetProfileAddress(userId: userId, on: on))
    switch r {
    case .userProfileNoChange: return nil
    case let .userProfileUpdated(user, _, _, _): return user
    default: throw r.unexpected
    }
}

func apiSetContactPrefs(contactId: Int64, preferences: Preferences) async throws -> Contact? {
    let r: ChatResponse1 = try await chatSendCmd(.apiSetContactPrefs(contactId: contactId, preferences: preferences))
    if case let .contactPrefsUpdated(_, _, toContact) = r { return toContact }
    throw r.unexpected
}

func apiSetContactAlias(contactId: Int64, localAlias: String) async throws -> Contact? {
    let r: ChatResponse1 = try await chatSendCmd(.apiSetContactAlias(contactId: contactId, localAlias: localAlias))
    if case let .contactAliasUpdated(_, toContact) = r { return toContact }
    throw r.unexpected
}

func apiSetGroupAlias(groupId: Int64, localAlias: String) async throws -> GroupInfo? {
    let r: ChatResponse1 = try await chatSendCmd(.apiSetGroupAlias(groupId: groupId, localAlias: localAlias))
    if case let .groupAliasUpdated(_, toGroup) = r { return toGroup }
    throw r.unexpected
}

func apiSetConnectionAlias(connId: Int64, localAlias: String) async throws -> PendingContactConnection? {
    let r: ChatResponse1 = try await chatSendCmd(.apiSetConnectionAlias(connId: connId, localAlias: localAlias))
    if case let .connectionAliasUpdated(_, toConnection) = r { return toConnection }
    throw r.unexpected
}

func apiSetUserUIThemes(userId: Int64, themes: ThemeModeOverrides?) async -> Bool {
    do {
        try await sendCommandOkResp(.apiSetUserUIThemes(userId: userId, themes: themes))
        return true
    } catch {
        logger.error("apiSetUserUIThemes bad response: \(responseError(error))")
        return false
    }
}

func apiSetChatUIThemes(chatId: ChatId, themes: ThemeModeOverrides?) async -> Bool {
    do {
        try await sendCommandOkResp(.apiSetChatUIThemes(chatId: chatId, themes: themes))
        return true
    } catch {
        logger.error("apiSetChatUIThemes bad response: \(responseError(error))")
        return false
    }
}


func apiCreateUserAddress(short: Bool) async throws -> CreatedConnLink {
    let userId = try currentUserId("apiCreateUserAddress")
    let r: ChatResponse1 = try await chatSendCmd(.apiCreateMyAddress(userId: userId, short: short))
    if case let .userContactLinkCreated(_, connLink) = r { return connLink }
    throw r.unexpected
}

func apiDeleteUserAddress() async throws -> User? {
    let userId = try currentUserId("apiDeleteUserAddress")
    let r: ChatResponse1 = try await chatSendCmd(.apiDeleteMyAddress(userId: userId))
    if case let .userContactLinkDeleted(user) = r { return user }
    throw r.unexpected
}

func apiGetUserAddress() throws -> UserContactLink? {
    let userId = try currentUserId("apiGetUserAddress")
    return try userAddressResponse(chatApiSendCmdSync(.apiShowMyAddress(userId: userId)))
}

func apiGetUserAddressAsync() async throws -> UserContactLink? {
    let userId = try currentUserId("apiGetUserAddressAsync")
    return try userAddressResponse(await chatApiSendCmd(.apiShowMyAddress(userId: userId)))
}

private func userAddressResponse(_ r: APIResult<ChatResponse1>) throws -> UserContactLink? {
    switch r {
    case let .result(.userContactLink(_, contactLink)): return contactLink
    case .error(.errorStore(storeError: .userContactLinkNotFound)): return nil
    default: throw r.unexpected
    }
}

func userAddressAutoAccept(_ autoAccept: AutoAccept?) async throws -> UserContactLink? {
    let userId = try currentUserId("userAddressAutoAccept")
    let r: APIResult<ChatResponse1> = await chatApiSendCmd(.apiAddressAutoAccept(userId: userId, autoAccept: autoAccept))
    switch r {
    case let .result(.userContactLinkUpdated(_, contactLink)): return contactLink
    case .error(.errorStore(storeError: .userContactLinkNotFound)): return nil
    default: throw r.unexpected
    }
}

func apiAcceptContactRequest(incognito: Bool, contactReqId: Int64) async -> Contact? {
    let r: APIResult<ChatResponse1> = await chatApiSendCmd(.apiAcceptContact(incognito: incognito, contactReqId: contactReqId))
    let am = AlertManager.shared

    if case let .result(.acceptingContactRequest(_, contact)) = r { return contact }
    if case .error(.errorAgent(.SMP(_, .AUTH))) = r {
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
            message: "Error: \(responseError(r.unexpected))"
        )
    }
    return nil
}

func apiRejectContactRequest(contactReqId: Int64) async throws {
    let r: ChatResponse1 = try await chatSendCmd(.apiRejectContact(contactReqId: contactReqId))
    if case .contactRequestRejected = r { return }
    throw r.unexpected
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
    let r: APIResult<ChatResponse2> = await chatApiSendCmd(.apiUploadStandaloneFile(userId: user.userId, file: file), ctrl: ctrl)
    if case let .result(.sndStandaloneFileCreated(_, fileTransferMeta)) = r {
        return (fileTransferMeta, nil)
    } else {
        let err = responseError(r.unexpected)
        logger.error("uploadStandaloneFile error: \(err)")
        return (nil, err)
    }
}

func downloadStandaloneFile(user: any UserLike, url: String, file: CryptoFile, ctrl: chat_ctrl? = nil) async -> (RcvFileTransfer?, String?) {
    let r: APIResult<ChatResponse2> = await chatApiSendCmd(.apiDownloadStandaloneFile(userId: user.userId, url: url, file: file), ctrl: ctrl)
    if case let .result(.rcvStandaloneFileCreated(_, rcvFileTransfer)) = r {
        return (rcvFileTransfer, nil)
    } else {
        let err = responseError(r.unexpected)
        logger.error("downloadStandaloneFile error: \(err)")
        return (nil, err)
    }
}

func standaloneFileInfo(url: String, ctrl: chat_ctrl? = nil) async -> MigrationFileLinkData? {
    let r: APIResult<ChatResponse2> = await chatApiSendCmd(.apiStandaloneFileInfo(url: url), ctrl: ctrl)
    if case let .result(.standaloneFileInfo(fileMeta)) = r {
        return fileMeta
    } else {
        logger.error("standaloneFileInfo error: \(responseError(r.unexpected))")
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
    var fileIdsToApprove: [Int64] = []
    var srvsToApprove: Set<String> = []
    var otherFileErrs: [APIResult<ChatResponse2>] = []
    
    for fileId in fileIds {
        let r: APIResult<ChatResponse2> = await chatApiSendCmd(
            .receiveFile(
                fileId: fileId,
                userApprovedRelays: userApprovedRelays || !privacyAskToApproveRelaysGroupDefault.get(),
                encrypted: privacyEncryptLocalFilesGroupDefault.get(),
                inline: nil
            )
        )
        switch r {
        case let .result(.rcvFileAccepted(_, chatItem)):
            await chatItemSimpleUpdate(user, chatItem)
            // TODO when aChatItem added
            //        case let .rcvFileAcceptedSndCancelled(user, aChatItem, _):
            //            await chatItemSimpleUpdate(user, aChatItem)
            //            Task { cleanupFile(aChatItem) }
        case let .error(.error(.fileNotApproved(fileId, unknownServers))):
            fileIdsToApprove.append(fileId)
            srvsToApprove.formUnion(unknownServers)
        default:
            otherFileErrs.append(r)
        }
    }
    
    if !auto {
        let otherErrsStr = fileErrorStrs(otherFileErrs)
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
            case let .result(.rcvFileAcceptedSndCancelled(_, rcvFileTransfer)):
                logger.debug("receiveFiles error: sender cancelled file transfer \(rcvFileTransfer.fileId)")
                await MainActor.run {
                    showAlert(
                        NSLocalizedString("Cannot receive file", comment: "alert title"),
                        message: NSLocalizedString("Sender cancelled file transfer.", comment: "alert message")
                    )
                }
            case .error(.error(.fileCancelled)), .error(.error(.fileAlreadyReceiving)):
                logger.debug("receiveFiles ignoring FileCancelled or FileAlreadyReceiving error")
            default:
                await MainActor.run {
                    showAlert(
                        NSLocalizedString("Error receiving file", comment: "alert title"),
                        message: responseError(errorResponse.unexpected)
                    )
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
    
    func fileErrorStrs(_ errs: [APIResult<ChatResponse2>]) -> String {
        var errStr = ""
        if errs.count >= 1 {
            errStr = String(describing: errs[0].unexpected)
        }
        if errs.count >= 2 {
            errStr += "\n\(String(describing: errs[1].unexpected))"
        }
        if errs.count > 2 {
            errStr += "\nand \(errs.count - 2) other error(s)"
        }
        return errStr
    }
}
    
func cancelFile(user: User, fileId: Int64) async {
    if let chatItem = await apiCancelFile(fileId: fileId) {
        await chatItemSimpleUpdate(user, chatItem)
        cleanupFile(chatItem)
    }
}

func apiCancelFile(fileId: Int64, ctrl: chat_ctrl? = nil) async -> AChatItem? {
    let r: APIResult<ChatResponse2> = await chatApiSendCmd(.cancelFile(fileId: fileId), ctrl: ctrl)
    switch r {
    case let .result(.sndFileCancelled(_, chatItem, _, _)) : return chatItem
    case let .result(.rcvFileCancelled(_, chatItem, _)) : return chatItem
    default:
        logger.error("apiCancelFile error: \(responseError(r.unexpected))")
        return nil
    }
}

func setLocalDeviceName(_ displayName: String) throws {
    try sendCommandOkRespSync(.setLocalDeviceName(displayName: displayName))
}

func connectRemoteCtrl(desktopAddress: String) async throws -> (RemoteCtrlInfo?, CtrlAppInfo, String) {
    let r: ChatResponse2 = try await chatSendCmd(.connectRemoteCtrl(xrcpInvitation: desktopAddress))
    if case let .remoteCtrlConnecting(rc_, ctrlAppInfo, v) = r { return (rc_, ctrlAppInfo, v) }
    throw r.unexpected
}

func findKnownRemoteCtrl() async throws {
    try await sendCommandOkResp(.findKnownRemoteCtrl)
}

func confirmRemoteCtrl(_ rcId: Int64) async throws -> (RemoteCtrlInfo?, CtrlAppInfo, String) {
    let r: ChatResponse2 = try await chatSendCmd(.confirmRemoteCtrl(remoteCtrlId: rcId))
    if case let .remoteCtrlConnecting(rc_, ctrlAppInfo, v) = r { return (rc_, ctrlAppInfo, v) }
    throw r.unexpected
}

func verifyRemoteCtrlSession(_ sessCode: String) async throws -> RemoteCtrlInfo {
    let r: ChatResponse2 = try await chatSendCmd(.verifyRemoteCtrlSession(sessionCode: sessCode))
    if case let .remoteCtrlConnected(rc) = r { return rc }
    throw r.unexpected
}

func listRemoteCtrls() throws -> [RemoteCtrlInfo] {
    let r: ChatResponse2 = try chatSendCmdSync(.listRemoteCtrls)
    if case let .remoteCtrlList(rcInfo) = r { return rcInfo }
    throw r.unexpected
}

func stopRemoteCtrl() async throws {
    try await sendCommandOkResp(.stopRemoteCtrl)
}

func deleteRemoteCtrl(_ rcId: Int64) async throws {
    try await sendCommandOkResp(.deleteRemoteCtrl(remoteCtrlId: rcId))
}

func networkErrorAlert<R>(_ res: APIResult<R>) -> Alert? {
    if case let .error(e) = res, let alert = getNetworkErrorAlert(e) {
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
    let r: ChatResponse2 = try chatSendCmdSync(.apiGetCallInvitations)
    if case let .callInvitations(invs) = r { return invs }
    throw r.unexpected
}

func apiGetCallInvitations() async throws -> [RcvCallInvitation] {
    let r: ChatResponse2 = try await chatSendCmd(.apiGetCallInvitations)
    if case let .callInvitations(invs) = r { return invs }
    throw r.unexpected
}

func apiCallStatus(_ contact: Contact, _ status: String) async throws {
    if let callStatus = WebRTCCallStatus.init(rawValue: status) {
        try await sendCommandOkResp(.apiCallStatus(contact: contact, callStatus: callStatus))
    } else {
        logger.debug("apiCallStatus: call status \(status) not used")
    }
}

func apiGetNetworkStatuses() throws -> [ConnNetworkStatus] {
    let r: ChatResponse1 = try chatSendCmdSync(.apiGetNetworkStatuses)
    if case let .networkStatuses(_, statuses) = r { return statuses }
    throw r.unexpected
}

func markChatRead(_ chat: Chat) async {
    do {
        if chat.chatStats.unreadCount > 0 {
            let cInfo = chat.chatInfo
            try await apiChatRead(type: cInfo.chatType, id: cInfo.apiId)
            await MainActor.run {
                withAnimation { ChatModel.shared.markAllChatItemsRead(cInfo) }
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

func apiMarkChatItemsRead(_ cInfo: ChatInfo, _ itemIds: [ChatItem.ID], mentionsRead: Int) async {
    do {
        try await apiChatItemsRead(type: cInfo.chatType, id: cInfo.apiId, itemIds: itemIds)
        DispatchQueue.main.async {
            ChatModel.shared.markChatItemsRead(cInfo, itemIds, mentionsRead)
        }
    } catch {
        logger.error("apiChatItemsRead error: \(responseError(error))")
    }
}

private func sendCommandOkResp(_ cmd: ChatCommand, ctrl: chat_ctrl? = nil) async throws {
    let r: ChatResponse2 = try await chatSendCmd(cmd, ctrl: ctrl)
    if case .cmdOk = r { return }
    throw r.unexpected
}

private func sendCommandOkRespSync(_ cmd: ChatCommand) throws {
    let r: ChatResponse2 = try chatSendCmdSync(cmd)
    if case .cmdOk = r { return }
    throw r.unexpected
}

func apiNewGroup(incognito: Bool, groupProfile: GroupProfile) throws -> GroupInfo {
    let userId = try currentUserId("apiNewGroup")
    let r: ChatResponse2 = try chatSendCmdSync(.apiNewGroup(userId: userId, incognito: incognito, groupProfile: groupProfile))
    if case let .groupCreated(_, groupInfo) = r { return groupInfo }
    throw r.unexpected
}

func apiAddMember(_ groupId: Int64, _ contactId: Int64, _ memberRole: GroupMemberRole) async throws -> GroupMember {
    let r: ChatResponse2 = try await chatSendCmd(.apiAddMember(groupId: groupId, contactId: contactId, memberRole: memberRole))
    if case let .sentGroupInvitation(_, _, _, member) = r { return member }
    throw r.unexpected
}

enum JoinGroupResult {
    case joined(groupInfo: GroupInfo)
    case invitationRemoved
    case groupNotFound
}

func apiJoinGroup(_ groupId: Int64) async throws -> JoinGroupResult {
    let r: APIResult<ChatResponse2> = await chatApiSendCmd(.apiJoinGroup(groupId: groupId))
    switch r {
    case let .result(.userAcceptedGroupSent(_, groupInfo, _)): return .joined(groupInfo: groupInfo)
    case .error(.errorAgent(.SMP(_, .AUTH))): return .invitationRemoved
    case .error(.errorStore(.groupNotFound)): return .groupNotFound
    default: throw r.unexpected
    }
}

func apiRemoveMembers(_ groupId: Int64, _ memberIds: [Int64], _ withMessages: Bool = false) async throws -> [GroupMember] {
    let r: ChatResponse2 = try await chatSendCmd(.apiRemoveMembers(groupId: groupId, memberIds: memberIds, withMessages: withMessages), bgTask: false)
    if case let .userDeletedMembers(_, _, members, withMessages) = r { return members }
    throw r.unexpected
}

func apiMembersRole(_ groupId: Int64, _ memberIds: [Int64], _ memberRole: GroupMemberRole) async throws -> [GroupMember] {
    let r: ChatResponse2 = try await chatSendCmd(.apiMembersRole(groupId: groupId, memberIds: memberIds, memberRole: memberRole), bgTask: false)
    if case let .membersRoleUser(_, _, members, _) = r { return members }
    throw r.unexpected
}

func apiBlockMembersForAll(_ groupId: Int64, _ memberIds: [Int64], _ blocked: Bool) async throws -> [GroupMember] {
    let r: ChatResponse2 = try await chatSendCmd(.apiBlockMembersForAll(groupId: groupId, memberIds: memberIds, blocked: blocked), bgTask: false)
    if case let .membersBlockedForAllUser(_, _, members, _) = r { return members }
    throw r.unexpected
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
    let r: ChatResponse2 = try await chatSendCmd(.apiLeaveGroup(groupId: groupId), bgTask: false)
    if case let .leftMemberUser(_, groupInfo) = r { return groupInfo }
    throw r.unexpected
}

// use ChatModel's loadGroupMembers from views
func apiListMembers(_ groupId: Int64) async -> [GroupMember] {
    let r: APIResult<ChatResponse2> = await chatApiSendCmd(.apiListMembers(groupId: groupId))
    if case let .result(.groupMembers(_, group)) = r { return group.members }
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
    let r: ChatResponse2 = try await chatSendCmd(.apiUpdateGroupProfile(groupId: groupId, groupProfile: groupProfile))
    if case let .groupUpdated(_, toGroup) = r { return toGroup }
    throw r.unexpected
}

func apiCreateGroupLink(_ groupId: Int64, memberRole: GroupMemberRole = .member) async throws -> (CreatedConnLink, GroupMemberRole) {
    let short = UserDefaults.standard.bool(forKey: DEFAULT_PRIVACY_SHORT_LINKS)
    let r: ChatResponse2 = try await chatSendCmd(.apiCreateGroupLink(groupId: groupId, memberRole: memberRole, short: short))
    if case let .groupLinkCreated(_, _, connLink, memberRole) = r { return (connLink, memberRole) }
    throw r.unexpected
}

func apiGroupLinkMemberRole(_ groupId: Int64, memberRole: GroupMemberRole = .member) async throws -> (CreatedConnLink, GroupMemberRole) {
    let r: ChatResponse2 = try await chatSendCmd(.apiGroupLinkMemberRole(groupId: groupId, memberRole: memberRole))
    if case let .groupLink(_, _, connLink, memberRole) = r { return (connLink, memberRole) }
    throw r.unexpected
}

func apiDeleteGroupLink(_ groupId: Int64) async throws {
    let r: ChatResponse2 = try await chatSendCmd(.apiDeleteGroupLink(groupId: groupId))
    if case .groupLinkDeleted = r { return }
    throw r.unexpected
}

func apiGetGroupLink(_ groupId: Int64) throws -> (CreatedConnLink, GroupMemberRole)? {
    let r: APIResult<ChatResponse2> = chatApiSendCmdSync(.apiGetGroupLink(groupId: groupId))
    switch r {
    case let .result(.groupLink(_, _, connLink, memberRole)):
        return (connLink, memberRole)
    case .error(.errorStore(storeError: .groupLinkNotFound)):
        return nil
    default: throw r.unexpected
    }
}

func apiCreateMemberContact(_ groupId: Int64, _ groupMemberId: Int64) async throws -> Contact {
    let r: ChatResponse2 = try await chatSendCmd(.apiCreateMemberContact(groupId: groupId, groupMemberId: groupMemberId))
    if case let .newMemberContact(_, contact, _, _) = r { return contact }
    throw r.unexpected
}

func apiSendMemberContactInvitation(_ contactId: Int64, _ msg: MsgContent) async throws -> Contact {
    let r: ChatResponse2 = try await chatSendCmd(.apiSendMemberContactInvitation(contactId: contactId, msg: msg), bgDelay: msgDelay)
    if case let .newMemberContactSentInv(_, contact, _, _) = r { return contact }
    throw r.unexpected
}

func apiGetVersion() throws -> CoreVersionInfo {
    let r: ChatResponse2 = try chatSendCmdSync(.showVersion)
    if case let .versionInfo(info, _, _) = r { return info }
    throw r.unexpected
}

func getAgentSubsTotal() async throws -> (SMPServerSubs, Bool) {
    let userId = try currentUserId("getAgentSubsTotal")
    let r: ChatResponse2 = try await chatSendCmd(.getAgentSubsTotal(userId: userId), log: false)
    if case let .agentSubsTotal(_, subsTotal, hasSession) = r { return (subsTotal, hasSession) }
    logger.error("getAgentSubsTotal error: \(String(describing: r))")
    throw r.unexpected
}

func getAgentServersSummary() throws -> PresentedServersSummary {
    let userId = try currentUserId("getAgentServersSummary")
    let r: ChatResponse2 = try chatSendCmdSync(.getAgentServersSummary(userId: userId), log: false)
    if case let .agentServersSummary(_, serversSummary) = r { return serversSummary }
    logger.error("getAgentServersSummary error: \(String(describing: r))")
    throw r.unexpected
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
    m.conditions = try getServerOperatorsSync()
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
    let tags = try apiGetChatTags()
    m.updateChats(chats)
    let tm = ChatTagsModel.shared
    tm.activeFilter = nil
    tm.userTags = tags
    tm.updateChatTags(m.chats)
}

private func getUserChatDataAsync() async throws {
    let m = ChatModel.shared
    let tm = ChatTagsModel.shared
    if m.currentUser != nil {
        let userAddress = try await apiGetUserAddressAsync()
        let chatItemTTL = try await getChatItemTTLAsync()
        let chats = try await apiGetChatsAsync()
        let tags = try await apiGetChatTagsAsync()
        await MainActor.run {
            m.userAddress = userAddress
            m.chatItemTTL = chatItemTTL
            m.updateChats(chats)
            tm.activeFilter = nil
            tm.userTags = tags
            tm.updateChatTags(m.chats)
        }
    } else {
        await MainActor.run {
            m.userAddress = nil
            m.updateChats([])
            tm.activeFilter = nil
            tm.userTags = []
            tm.presetTags = [:]
        }
    }
}

class ChatReceiver {
    private var receiveLoop: Task<Void, Never>?
    private var receiveMessages = true
    private var _lastMsgTime = Date.now

    var messagesChannel: ((APIResult<ChatEvent>) -> Void)? = nil

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
                Task { await TerminalItems.shared.addResult(msg) }
                switch msg {
                case let .result(evt): await processReceivedMsg(evt)
                case let .error(err): logger.debug("chatRecvMsg error: \(responseError(err))")
                case let .invalid(type, json): logger.debug("chatRecvMsg event: * \(type) \(dataToString(json))")
                }
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

func processReceivedMsg(_ res: ChatEvent) async {
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
                    if cItem.isActiveReport {
                        m.increaseGroupReportsCounter(cInfo.id)
                    }
                } else if cItem.isRcvNew && cInfo.ntfsEnabled(chatItem: cItem) {
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
                let d = item.deletedChatItem
                if item.toChatItem == nil && d.chatItem.isRcvNew && d.chatInfo.ntfsEnabled(chatItem: d.chatItem) {
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
                if item.deletedChatItem.chatItem.isActiveReport {
                    m.decreaseGroupReportsCounter(item.deletedChatItem.chatInfo.id)
                }
            }
        }
    case let .groupChatItemsDeleted(user, groupInfo, chatItemIDs, _, member_):
        await groupChatItemsDeleted(user, groupInfo, chatItemIDs, member_)
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
    case let .businessLinkConnecting(user, groupInfo, _, fromContact):
        if !active(user) { return }

        await MainActor.run {
            m.updateGroup(groupInfo)
        }
        if m.chatId == fromContact.id {
            ItemsModel.shared.loadOpenChat(groupInfo.id)
        }
        await MainActor.run {
            m.removeChat(fromContact.id)
        }
    case let .joinedGroupMemberConnecting(user, groupInfo, _, member):
        if active(user) {
            await MainActor.run {
                _ = m.upsertGroupMember(groupInfo, member)
            }
        }
    case let .deletedMemberUser(user, groupInfo, member, withMessages): // TODO update user member
        if active(user) {
            await MainActor.run {
                m.updateGroup(groupInfo)
                if withMessages {
                    m.removeMemberItems(groupInfo.membership, byMember: member, groupInfo)
                }
            }
        }
    case let .deletedMember(user, groupInfo, byMember, deletedMember, withMessages):
        if active(user) {
            await MainActor.run {
                _ = m.upsertGroupMember(groupInfo, deletedMember)
                if withMessages {
                    m.removeMemberItems(deletedMember, byMember: byMember, groupInfo)
                }
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
// TODO when aChatItem added
//    case let .rcvFileAcceptedSndCancelled(user, aChatItem, _): // usually rcvFileAcceptedSndCancelled is a response, but it's also an event for XFTP files auto-accepted from NSE
//        await chatItemSimpleUpdate(user, aChatItem)
//        Task { cleanupFile(aChatItem) }
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

func groupChatItemsDeleted(_ user: UserRef, _ groupInfo: GroupInfo, _ chatItemIDs: Set<Int64>, _ member_: GroupMember?) async {
    let m = ChatModel.shared
    if !active(user) {
        do {
            let users = try listUsers()
            await MainActor.run {
                m.users = users
            }
        } catch {
            logger.error("Error loading users: \(error)")
        }
        return
    }
    let im = ItemsModel.shared
    let cInfo = ChatInfo.group(groupInfo: groupInfo)
    await MainActor.run {
        m.decreaseGroupReportsCounter(cInfo.id, by: chatItemIDs.count)
    }
    var notFound = chatItemIDs.count
    for ci in im.reversedChatItems {
        if chatItemIDs.contains(ci.id) {
            let deleted = if case let .groupRcv(groupMember) = ci.chatDir, let member_, groupMember.groupMemberId != member_.groupMemberId {
                CIDeleted.moderated(deletedTs: Date.now, byGroupMember: member_)
            } else {
                CIDeleted.deleted(deletedTs: Date.now)
            }
            await MainActor.run {
                var newItem = ci
                newItem.meta.itemDeleted = deleted
                _ = m.upsertChatItem(cInfo, newItem)
            }
            notFound -= 1
            if notFound == 0 { break }
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
