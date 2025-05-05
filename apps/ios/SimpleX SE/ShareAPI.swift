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
    let r: APIResult<SEChatResponse> = sendSimpleXCmd(SEChatCommand.showActiveUser)
    switch r {
    case let .result(.activeUser(user)): return user
    case .error(.error(.noActiveUser)): return nil
    default: throw r.unexpected
    }
}

func apiStartChat() throws -> Bool {
    let r: APIResult<SEChatResponse> = sendSimpleXCmd(SEChatCommand.startChat(mainApp: false, enableSndFiles: true))
    switch r {
    case .result(.chatStarted): return true
    case .result(.chatRunning): return false
    default: throw r.unexpected
    }
}

func apiSetNetworkConfig(_ cfg: NetCfg) throws {
    let r: APIResult<SEChatResponse> = sendSimpleXCmd(SEChatCommand.apiSetNetworkConfig(networkConfig: cfg))
    if case .result(.cmdOk) = r { return }
    throw r.unexpected
}

func apiSetAppFilePaths(filesFolder: String, tempFolder: String, assetsFolder: String) throws {
    let r: APIResult<SEChatResponse> = sendSimpleXCmd(SEChatCommand.apiSetAppFilePaths(filesFolder: filesFolder, tempFolder: tempFolder, assetsFolder: assetsFolder))
    if case .result(.cmdOk) = r { return }
    throw r.unexpected
}

func apiSetEncryptLocalFiles(_ enable: Bool) throws {
    let r: APIResult<SEChatResponse> = sendSimpleXCmd(SEChatCommand.apiSetEncryptLocalFiles(enable: enable))
    if case .result(.cmdOk) = r { return }
    throw r.unexpected
}

func apiGetChats(userId: User.ID) throws -> Array<ChatData> {
    let r: APIResult<SEChatResponse> = sendSimpleXCmd(SEChatCommand.apiGetChats(userId: userId))
    if case let .result(.apiChats(user: _, chats: chats)) = r { return chats }
    throw r.unexpected
}

func apiSendMessages(
    chatInfo: ChatInfo,
    composedMessages: [ComposedMessage]
) throws -> [AChatItem] {
    let r: APIResult<SEChatResponse> = sendSimpleXCmd(
        chatInfo.chatType == .local
        ? SEChatCommand.apiCreateChatItems(
            noteFolderId: chatInfo.apiId,
            composedMessages: composedMessages
        )
        : SEChatCommand.apiSendMessages(
            type: chatInfo.chatType,
            id: chatInfo.apiId,
            live: false,
            ttl: nil,
            composedMessages: composedMessages
        )
    )
    if case let .result(.newChatItems(_, chatItems)) = r {
        return chatItems
    } else {
        for composedMessage in composedMessages {
            if let filePath = composedMessage.fileSource?.filePath { removeFile(filePath) }
        }
        throw r.unexpected
    }
}

func apiActivateChat() throws {
    chatReopenStore()
    let r: APIResult<SEChatResponse> = sendSimpleXCmd(SEChatCommand.apiActivateChat(restoreChat: false))
    if case .result(.cmdOk) = r { return }
    throw r.unexpected
}

func apiSuspendChat(expired: Bool) {
    let r: APIResult<SEChatResponse> = sendSimpleXCmd(SEChatCommand.apiSuspendChat(timeoutMicroseconds: expired ? 0 : 3_000000))
    // Block until `chatSuspended` received or 3 seconds has passed
    var suspended = false
    if case .result(.cmdOk) = r, !expired {
        let startTime = CFAbsoluteTimeGetCurrent()
        while CFAbsoluteTimeGetCurrent() - startTime < 3 {
            let msg: APIResult<SEChatEvent>? = recvSimpleXMsg(messageTimeout: 3_500000)
            switch msg {
            case .result(.chatSuspended):
                suspended = false
                break
            default: continue
            }
        }
    }
    if !suspended {
        let _r1: APIResult<SEChatResponse> = sendSimpleXCmd(SEChatCommand.apiSuspendChat(timeoutMicroseconds: 0))
    }
    logger.debug("close store")
    chatCloseStore()
    SEChatState.shared.set(.inactive)
}

enum SEChatCommand: ChatCmdProtocol {
    case showActiveUser
    case startChat(mainApp: Bool, enableSndFiles: Bool)
    case apiActivateChat(restoreChat: Bool)
    case apiSuspendChat(timeoutMicroseconds: Int)
    case apiSetNetworkConfig(networkConfig: NetCfg)
    case apiSetAppFilePaths(filesFolder: String, tempFolder: String, assetsFolder: String)
    case apiSetEncryptLocalFiles(enable: Bool)
    case apiGetChats(userId: Int64)
    case apiCreateChatItems(noteFolderId: Int64, composedMessages: [ComposedMessage])
    case apiSendMessages(type: ChatType, id: Int64, live: Bool, ttl: Int?, composedMessages: [ComposedMessage])
    
    var cmdString: String {
        switch self {
        case .showActiveUser: return "/u"
        case let .startChat(mainApp, enableSndFiles): return "/_start main=\(onOff(mainApp)) snd_files=\(onOff(enableSndFiles))"
        case let .apiActivateChat(restore): return "/_app activate restore=\(onOff(restore))"
        case let .apiSuspendChat(timeoutMicroseconds): return "/_app suspend \(timeoutMicroseconds)"
        case let .apiSetNetworkConfig(networkConfig): return "/_network \(encodeJSON(networkConfig))"
        case let .apiSetAppFilePaths(filesFolder, tempFolder, assetsFolder):
            return "/set file paths \(encodeJSON(AppFilePaths(appFilesFolder: filesFolder, appTempFolder: tempFolder, appAssetsFolder: assetsFolder)))"
        case let .apiSetEncryptLocalFiles(enable): return "/_files_encrypt \(onOff(enable))"
        case let .apiGetChats(userId): return "/_get chats \(userId) pcc=on"
        case let .apiCreateChatItems(noteFolderId, composedMessages):
            let msgs = encodeJSON(composedMessages)
            return "/_create *\(noteFolderId) json \(msgs)"
        case let .apiSendMessages(type, id, live, ttl, composedMessages):
            let msgs = encodeJSON(composedMessages)
            let ttlStr = ttl != nil ? "\(ttl!)" : "default"
            return "/_send \(ref(type, id)) live=\(onOff(live)) ttl=\(ttlStr) json \(msgs)"
        }
    }
    
    func ref(_ type: ChatType, _ id: Int64) -> String {
        "\(type.rawValue)\(id)"
    }
}

enum SEChatResponse: Decodable, ChatAPIResult {
    case activeUser(user: User)
    case chatStarted
    case chatRunning
    case apiChats(user: UserRef, chats: [ChatData])
    case newChatItems(user: UserRef, chatItems: [AChatItem])
    case cmdOk(user_: UserRef?)
    
    var responseType: String {
        switch self {
        case .activeUser: "activeUser"
        case .chatStarted: "chatStarted"
        case .chatRunning: "chatRunning"
        case .apiChats: "apiChats"
        case .newChatItems: "newChatItems"
        case .cmdOk: "cmdOk"
        }
    }
    
    var details: String {
        switch self {
        case let .activeUser(user): return String(describing: user)
        case .chatStarted: return noDetails
        case .chatRunning: return noDetails
        case let .apiChats(u, chats): return withUser(u, String(describing: chats))
        case let .newChatItems(u, chatItems):
            let itemsString = chatItems.map { chatItem in String(describing: chatItem) }.joined(separator: "\n")
            return withUser(u, itemsString)
        case .cmdOk: return noDetails
        }
    }

    static func fallbackResult(_ type: String, _ json: NSDictionary) -> SEChatResponse? {
        if type == "apiChats", let r = parseApiChats(json) {
            .apiChats(user: r.user, chats: r.chats)
        } else {
            nil
        }
    }
}

enum SEChatEvent: Decodable, ChatAPIResult {
    case chatSuspended
    case sndFileProgressXFTP(user: UserRef, chatItem_: AChatItem?, fileTransferMeta: FileTransferMeta, sentSize: Int64, totalSize: Int64)
    case sndFileCompleteXFTP(user: UserRef, chatItem: AChatItem, fileTransferMeta: FileTransferMeta)
    case chatItemsStatusesUpdated(user: UserRef, chatItems: [AChatItem])
    case sndFileError(user: UserRef, chatItem_: AChatItem?, fileTransferMeta: FileTransferMeta, errorMessage: String)
    case sndFileWarning(user: UserRef, chatItem_: AChatItem?, fileTransferMeta: FileTransferMeta, errorMessage: String)
    
    var responseType: String {
        switch self {
        case .chatSuspended: "chatSuspended"
        case .sndFileProgressXFTP: "sndFileProgressXFTP"
        case .sndFileCompleteXFTP: "sndFileCompleteXFTP"
        case .chatItemsStatusesUpdated: "chatItemsStatusesUpdated"
        case .sndFileError: "sndFileError"
        case .sndFileWarning: "sndFileWarning"
        }
    }
    
    var details: String {
        switch self {
        case .chatSuspended: return noDetails
        case let .sndFileProgressXFTP(u, chatItem, _, sentSize, totalSize): return withUser(u, "chatItem: \(String(describing: chatItem))\nsentSize: \(sentSize)\ntotalSize: \(totalSize)")
        case let .sndFileCompleteXFTP(u, chatItem, _): return withUser(u, String(describing: chatItem))
        case let .chatItemsStatusesUpdated(u, chatItems):
            let itemsString = chatItems.map { chatItem in String(describing: chatItem) }.joined(separator: "\n")
            return withUser(u, itemsString)
        case let .sndFileError(u, chatItem, _, err): return withUser(u, "error: \(String(describing: err))\nchatItem: \(String(describing: chatItem))")
        case let .sndFileWarning(u, chatItem, _, err): return withUser(u, "error: \(String(describing: err))\nchatItem: \(String(describing: chatItem))")
        }
    }    
}
