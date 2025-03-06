//
//  ShareAPI.swift
//  SimpleX SE
//
//  Created by User on 15/07/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import OSLog
import Foundation
import SimpleXChat

let logger = Logger()

func apiGetActiveUser() throws -> User? {
    let r = sendSimpleXCmd(.showActiveUser)
    switch r {
    case let .activeUser(user): return user
    case .chatCmdError(_, .error(.noActiveUser)): return nil
    default: throw r
    }
}

func apiStartChat() throws -> Bool {
    let r = sendSimpleXCmd(.startChat(mainApp: false, enableSndFiles: true))
    switch r {
    case .chatStarted: return true
    case .chatRunning: return false
    default: throw r
    }
}

func apiSetNetworkConfig(_ cfg: NetCfg) throws {
    let r = sendSimpleXCmd(.apiSetNetworkConfig(networkConfig: cfg))
    if case .cmdOk = r { return }
    throw r
}

func apiSetAppFilePaths(filesFolder: String, tempFolder: String, assetsFolder: String) throws {
    let r = sendSimpleXCmd(.apiSetAppFilePaths(filesFolder: filesFolder, tempFolder: tempFolder, assetsFolder: assetsFolder))
    if case .cmdOk = r { return }
    throw r
}

func apiSetEncryptLocalFiles(_ enable: Bool) throws {
    let r = sendSimpleXCmd(.apiSetEncryptLocalFiles(enable: enable))
    if case .cmdOk = r { return }
    throw r
}

func apiGetChats(userId: User.ID) throws -> Array<ChatData> {
    let r = sendSimpleXCmd(.apiGetChats(userId: userId))
    if case let .apiChats(user: _, chats: chats) = r { return chats }
    throw r
}

func apiSendMessages(
    chatInfo: ChatInfo,
    composedMessages: [ComposedMessage]
) throws -> [AChatItem] {
    let r = sendSimpleXCmd(
        chatInfo.chatType == .local
        ? .apiCreateChatItems(
            noteFolderId: chatInfo.apiId,
            composedMessages: composedMessages
        )
        : .apiSendMessages(
            type: chatInfo.chatType,
            id: chatInfo.apiId,
            live: false,
            ttl: nil,
            composedMessages: composedMessages
        )
    )
    if case let .newChatItems(_, chatItems) = r {
        return chatItems
    } else {
        for composedMessage in composedMessages {
            if let filePath = composedMessage.fileSource?.filePath { removeFile(filePath) }
        }
        throw r
    }
}

func apiActivateChat() throws {
    chatReopenStore()
    let r = sendSimpleXCmd(.apiActivateChat(restoreChat: false))
    if case .cmdOk = r { return }
    throw r
}

func apiSuspendChat(expired: Bool) {
    let r = sendSimpleXCmd(.apiSuspendChat(timeoutMicroseconds: expired ? 0 : 3_000000))
    // Block until `chatSuspended` received or 3 seconds has passed
    var suspended = false
    if case .cmdOk = r, !expired {
        let startTime = CFAbsoluteTimeGetCurrent()
        while CFAbsoluteTimeGetCurrent() - startTime < 3 {
            switch recvSimpleXMsg(messageTimeout: 3_500000) {
            case .chatSuspended:
                suspended = false
                break
            default: continue
            }
        }
    }
    if !suspended {
        _ = sendSimpleXCmd(.apiSuspendChat(timeoutMicroseconds: 0))
    }
    logger.debug("close store")
    chatCloseStore()
    SEChatState.shared.set(.inactive)
}
