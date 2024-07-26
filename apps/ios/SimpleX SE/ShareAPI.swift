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

func apiSendMessage(
    chatInfo: ChatInfo,
    cryptoFile: CryptoFile?,
    msgContent: MsgContent
) throws -> AChatItem {
    let r = sendSimpleXCmd(
        chatInfo.chatType == .local
        ? .apiCreateChatItem(
            noteFolderId: chatInfo.apiId,
            file: cryptoFile,
            msg: msgContent
        )
        : .apiSendMessage(
            type: chatInfo.chatType,
            id: chatInfo.apiId,
            file: cryptoFile,
            quotedItemId: nil,
            msg: msgContent,
            live: false,
            ttl: nil
        )
    )
    if case let .newChatItem(_, chatItem) = r {
        return chatItem
    } else {
        if let filePath = cryptoFile?.filePath { removeFile(filePath) }
        throw r
    }
}

func apiSuspendChat(expired: Bool) {
    let r = sendSimpleXCmd(
        .apiSuspendChat(
            timeoutMicroseconds: expired
            ? .zero
            : 3_000_000
        )
    )
    // Block until `chatSuspended` received or 3 seconds has passed
    let startTime = CFAbsoluteTimeGetCurrent()
    if case .cmdOk = r, !expired {
        while CFAbsoluteTimeGetCurrent() - startTime < 3 {
            switch recvSimpleXMsg() {
            case .chatSuspended: break
            default: continue
            }
        }
    }
    chatCloseStore()
}
