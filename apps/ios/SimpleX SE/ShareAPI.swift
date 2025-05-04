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
    let r: SEChatResponse = sendSimpleXCmd(SEChatCommand.showActiveUser)
    switch r {
    case let .activeUser(user): return user
    case .chatCmdError(_, .error(.noActiveUser)): return nil
    default: throw r
    }
}

func apiStartChat() throws -> Bool {
    let r: SEChatResponse = sendSimpleXCmd(SEChatCommand.startChat(mainApp: false, enableSndFiles: true))
    switch r {
    case .chatStarted: return true
    case .chatRunning: return false
    default: throw r
    }
}

func apiSetNetworkConfig(_ cfg: NetCfg) throws {
    let r: SEChatResponse = sendSimpleXCmd(SEChatCommand.apiSetNetworkConfig(networkConfig: cfg))
    if case .cmdOk = r { return }
    throw r
}

func apiSetAppFilePaths(filesFolder: String, tempFolder: String, assetsFolder: String) throws {
    let r: SEChatResponse = sendSimpleXCmd(SEChatCommand.apiSetAppFilePaths(filesFolder: filesFolder, tempFolder: tempFolder, assetsFolder: assetsFolder))
    if case .cmdOk = r { return }
    throw r
}

func apiSetEncryptLocalFiles(_ enable: Bool) throws {
    let r: SEChatResponse = sendSimpleXCmd(SEChatCommand.apiSetEncryptLocalFiles(enable: enable))
    if case .cmdOk = r { return }
    throw r
}

func apiGetChats(userId: User.ID) throws -> Array<ChatData> {
    let r: SEChatResponse = sendSimpleXCmd(SEChatCommand.apiGetChats(userId: userId))
    if case let .apiChats(user: _, chats: chats) = r { return chats }
    throw r
}

func apiSendMessages(
    chatInfo: ChatInfo,
    composedMessages: [ComposedMessage]
) throws -> [AChatItem] {
    let r: SEChatResponse = sendSimpleXCmd(
        chatInfo.chatType == .local
        ? SEChatCommand.apiCreateChatItems(
            noteFolderId: chatInfo.apiId,
            composedMessages: composedMessages
        )
        : SEChatCommand.apiSendMessages(
            type: chatInfo.chatType,
            id: chatInfo.apiId,
            scope: chatInfo.groupChatScope(),
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
    let r: SEChatResponse = sendSimpleXCmd(SEChatCommand.apiActivateChat(restoreChat: false))
    if case .cmdOk = r { return }
    throw r
}

func apiSuspendChat(expired: Bool) {
    let r: SEChatResponse = sendSimpleXCmd(SEChatCommand.apiSuspendChat(timeoutMicroseconds: expired ? 0 : 3_000000))
    // Block until `chatSuspended` received or 3 seconds has passed
    var suspended = false
    if case .cmdOk = r, !expired {
        let startTime = CFAbsoluteTimeGetCurrent()
        while CFAbsoluteTimeGetCurrent() - startTime < 3 {
            let msg: SEChatResponse? = recvSimpleXMsg(messageTimeout: 3_500000)
            switch msg {
            case .chatSuspended:
                suspended = false
                break
            default: continue
            }
        }
    }
    if !suspended {
        let _r1: SEChatResponse = sendSimpleXCmd(SEChatCommand.apiSuspendChat(timeoutMicroseconds: 0))
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
    case apiSendMessages(type: ChatType, id: Int64, scope: GroupChatScope?, live: Bool, ttl: Int?, composedMessages: [ComposedMessage])
    
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
        case let .apiSendMessages(type, id, scope, live, ttl, composedMessages):
            let msgs = encodeJSON(composedMessages)
            let ttlStr = ttl != nil ? "\(ttl!)" : "default"
            return "/_send \(ref(type, id, scope: scope)) live=\(onOff(live)) ttl=\(ttlStr) json \(msgs)"
        }
    }
    
    func ref(_ type: ChatType, _ id: Int64, scope: GroupChatScope?) -> String {
        "\(type.rawValue)\(id)\(scopeRef(scope: scope))"
    }

    func scopeRef(scope: GroupChatScope?) -> String {
        switch (scope) {
        case .none: ""
        case let .memberSupport(groupMemberId_):
            if let groupMemberId = groupMemberId_ {
                "(_support:\(groupMemberId))"
            } else {
                "(_support)"
            }
        }
    }
}

enum SEChatResponse: Decodable, Error, ChatRespProtocol {
    case response(type: String, json: String)
    case activeUser(user: User)
    case chatStarted
    case chatRunning
    case chatSuspended
    case apiChats(user: UserRef, chats: [ChatData])
    case newChatItems(user: UserRef, chatItems: [AChatItem])
    case sndFileProgressXFTP(user: UserRef, chatItem_: AChatItem?, fileTransferMeta: FileTransferMeta, sentSize: Int64, totalSize: Int64)
    case sndFileCompleteXFTP(user: UserRef, chatItem: AChatItem, fileTransferMeta: FileTransferMeta)
    case chatItemsStatusesUpdated(user: UserRef, chatItems: [AChatItem])
    case sndFileError(user: UserRef, chatItem_: AChatItem?, fileTransferMeta: FileTransferMeta, errorMessage: String)
    case sndFileWarning(user: UserRef, chatItem_: AChatItem?, fileTransferMeta: FileTransferMeta, errorMessage: String)
    case cmdOk(user_: UserRef?)
    case chatCmdError(user_: UserRef?, chatError: ChatError)
    case chatError(user_: UserRef?, chatError: ChatError)
    
    var responseType: String {
        switch self {
        case let .response(type, _): "* \(type)"
        case .activeUser: "activeUser"
        case .chatStarted: "chatStarted"
        case .chatRunning: "chatRunning"
        case .chatSuspended: "chatSuspended"
        case .apiChats: "apiChats"
        case .newChatItems: "newChatItems"
        case .sndFileProgressXFTP: "sndFileProgressXFTP"
        case .sndFileCompleteXFTP: "sndFileCompleteXFTP"
        case .chatItemsStatusesUpdated: "chatItemsStatusesUpdated"
        case .sndFileError: "sndFileError"
        case .sndFileWarning: "sndFileWarning"
        case .cmdOk: "cmdOk"
        case .chatCmdError: "chatCmdError"
        case .chatError: "chatError"
        }
    }
    
    var details: String {
        switch self {
        case let .response(_, json): return json
        case let .activeUser(user): return String(describing: user)
        case .chatStarted: return noDetails
        case .chatRunning: return noDetails
        case .chatSuspended: return noDetails
        case let .apiChats(u, chats): return withUser(u, String(describing: chats))
        case let .newChatItems(u, chatItems):
            let itemsString = chatItems.map { chatItem in String(describing: chatItem) }.joined(separator: "\n")
            return withUser(u, itemsString)
        case let .sndFileProgressXFTP(u, chatItem, _, sentSize, totalSize): return withUser(u, "chatItem: \(String(describing: chatItem))\nsentSize: \(sentSize)\ntotalSize: \(totalSize)")
        case let .sndFileCompleteXFTP(u, chatItem, _): return withUser(u, String(describing: chatItem))
        case let .chatItemsStatusesUpdated(u, chatItems):
            let itemsString = chatItems.map { chatItem in String(describing: chatItem) }.joined(separator: "\n")
            return withUser(u, itemsString)
        case let .sndFileError(u, chatItem, _, err): return withUser(u, "error: \(String(describing: err))\nchatItem: \(String(describing: chatItem))")
        case let .sndFileWarning(u, chatItem, _, err): return withUser(u, "error: \(String(describing: err))\nchatItem: \(String(describing: chatItem))")
        case .cmdOk: return noDetails
        case let .chatCmdError(u, chatError): return withUser(u, String(describing: chatError))
        case let .chatError(u, chatError): return withUser(u, String(describing: chatError))
        }
    }

    var noDetails: String { "\(responseType): no details" }

    static func chatResponse(_ s: String) -> SEChatResponse {
        let d = s.data(using: .utf8)!
        // TODO is there a way to do it without copying the data? e.g:
        //    let p = UnsafeMutableRawPointer.init(mutating: UnsafeRawPointer(cjson))
        //    let d = Data.init(bytesNoCopy: p, count: strlen(cjson), deallocator: .free)
        do {
            let r = try jsonDecoder.decode(APIResponse<SEChatResponse>.self, from: d)
            return r.resp
        } catch {
            logger.error("chatResponse jsonDecoder.decode error: \(error.localizedDescription)")
        }
        
        var type: String?
        var json: String?
        if let j = try? JSONSerialization.jsonObject(with: d) as? NSDictionary {
            if let jResp = j["resp"] as? NSDictionary, jResp.count == 1 || jResp.count == 2 {
                type = jResp.allKeys[0] as? String
                if jResp.count == 2 && type == "_owsf" {
                    type = jResp.allKeys[1] as? String
                }
                if type == "apiChats" {
                    if let r = parseApiChats(jResp) {
                        return .apiChats(user: r.user, chats: r.chats)
                    }
                } else if type == "chatCmdError" {
                    if let jError = jResp["chatCmdError"] as? NSDictionary {
                        return .chatCmdError(user_: decodeUser_(jError), chatError: .invalidJSON(json: errorJson(jError) ?? ""))
                    }
                } else if type == "chatError" {
                    if let jError = jResp["chatError"] as? NSDictionary {
                        return .chatError(user_: decodeUser_(jError), chatError: .invalidJSON(json: errorJson(jError) ?? ""))
                    }
                }
            }
            json = serializeJSON(j, options: .prettyPrinted)
        }
        return SEChatResponse.response(type: type ?? "invalid", json: json ?? s)
    }
    
    var chatError: ChatError? {
        switch self {
        case let .chatCmdError(_, error): error
        case let .chatError(_, error): error
        default: nil
        }
    }
    
    var chatErrorType: ChatErrorType? {
        switch self {
        case let .chatCmdError(_, .error(error)): error
        case let .chatError(_, .error(error)): error
        default: nil
        }
    }
}
