//
//  APITypes.swift
//  SimpleX
//
//  Created by EP on 01/05/2025.
//  Copyright © 2025 SimpleX Chat. All rights reserved.
//

import SimpleXChat

enum NSEChatCommand: ChatCmdProtocol {
    case showActiveUser
    case startChat(mainApp: Bool, enableSndFiles: Bool)
    case apiActivateChat(restoreChat: Bool)
    case apiSuspendChat(timeoutMicroseconds: Int)
    case apiSetNetworkConfig(networkConfig: NetCfg)
    case apiSetAppFilePaths(filesFolder: String, tempFolder: String, assetsFolder: String)
    case apiSetEncryptLocalFiles(enable: Bool)
    case apiGetNtfConns(nonce: String, encNtfInfo: String)
    case apiGetConnNtfMessages(connMsgReqs: [ConnMsgReq])
    case receiveFile(fileId: Int64, userApprovedRelays: Bool, encrypted: Bool?, inline: Bool?)
    case setFileToReceive(fileId: Int64, userApprovedRelays: Bool, encrypted: Bool?)
    
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
        case let .apiGetNtfConns(nonce, encNtfInfo): return "/_ntf conns \(nonce) \(encNtfInfo)"
        case let .apiGetConnNtfMessages(connMsgReqs): return "/_ntf conn messages \(connMsgReqs.map { $0.cmdString }.joined(separator: ","))"
        case let .receiveFile(fileId, userApprovedRelays, encrypt, inline): return "/freceive \(fileId)\(onOffParam("approved_relays", userApprovedRelays))\(onOffParam("encrypt", encrypt))\(onOffParam("inline", inline))"
        case let .setFileToReceive(fileId, userApprovedRelays, encrypt): return "/_set_file_to_receive \(fileId)\(onOffParam("approved_relays", userApprovedRelays))\(onOffParam("encrypt", encrypt))"
        }
    }

    private func onOffParam(_ param: String, _ b: Bool?) -> String {
        if let b = b {
            " \(param)=\(onOff(b))"
        } else {
            ""
        }
    }
}

enum NSEChatResponse: Decodable, Error, ChatRespProtocol {
    case response(type: String, json: String)
    case activeUser(user: User)
    case chatStarted
    case chatRunning
    case chatSuspended
    case contactConnected(user: UserRef, contact: Contact, userCustomProfile: Profile?)
    case receivedContactRequest(user: UserRef, contactRequest: UserContactRequest)
    case newChatItems(user: UserRef, chatItems: [AChatItem])
    case rcvFileAccepted(user: UserRef, chatItem: AChatItem)
    case rcvFileSndCancelled(user: UserRef, chatItem: AChatItem, rcvFileTransfer: RcvFileTransfer)
    case sndFileComplete(user: UserRef, chatItem: AChatItem, sndFileTransfer: SndFileTransfer)
    case sndFileRcvCancelled(user: UserRef, chatItem_: AChatItem?, sndFileTransfer: SndFileTransfer)
    case callInvitation(callInvitation: RcvCallInvitation)
    case ntfConns(ntfConns: [NtfConn])
    case connNtfMessages(receivedMsgs: [NtfMsgInfo?])
    case ntfMessage(user: UserRef, connEntity: ConnectionEntity, ntfMessage: NtfMsgAckInfo)
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
        case .contactConnected: "contactConnected"
        case .receivedContactRequest: "receivedContactRequest"
        case .newChatItems: "newChatItems"
        case .rcvFileAccepted: "rcvFileAccepted"
        case .rcvFileSndCancelled: "rcvFileSndCancelled"
        case .sndFileComplete: "sndFileComplete"
        case .sndFileRcvCancelled: "sndFileRcvCancelled"
        case .callInvitation: "callInvitation"
        case .ntfConns: "ntfConns"
        case .connNtfMessages: "connNtfMessages"
        case .ntfMessage: "ntfMessage"
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
        case let .contactConnected(u, contact, _): return withUser(u, String(describing: contact))
        case let .receivedContactRequest(u, contactRequest): return withUser(u, String(describing: contactRequest))
        case let .newChatItems(u, chatItems):
            let itemsString = chatItems.map { chatItem in String(describing: chatItem) }.joined(separator: "\n")
            return withUser(u, itemsString)
        case let .rcvFileAccepted(u, chatItem): return withUser(u, String(describing: chatItem))
        case let .rcvFileSndCancelled(u, chatItem, _): return withUser(u, String(describing: chatItem))
        case let .sndFileComplete(u, chatItem, _): return withUser(u, String(describing: chatItem))
        case let .sndFileRcvCancelled(u, chatItem, _): return withUser(u, String(describing: chatItem))
        case let .callInvitation(inv): return String(describing: inv)
        case let .ntfConns(ntfConns): return String(describing: ntfConns)
        case let .connNtfMessages(receivedMsgs): return "receivedMsgs: \(String(describing: receivedMsgs))"
        case let .ntfMessage(u, connEntity, ntfMessage): return withUser(u, "connEntity: \(String(describing: connEntity))\nntfMessage: \(String(describing: ntfMessage))")
        case .cmdOk: return noDetails
        case let .chatCmdError(u, chatError): return withUser(u, String(describing: chatError))
        case let .chatError(u, chatError): return withUser(u, String(describing: chatError))
        }
    }
    
    var noDetails: String { "\(responseType): no details" }

    static func chatResponse(_ s: String) -> NSEChatResponse {
        let d = s.data(using: .utf8)!
        // TODO is there a way to do it without copying the data? e.g:
        //    let p = UnsafeMutableRawPointer.init(mutating: UnsafeRawPointer(cjson))
        //    let d = Data.init(bytesNoCopy: p, count: strlen(cjson), deallocator: .free)
        do {
            let r = try jsonDecoder.decode(APIResponse<NSEChatResponse>.self, from: d)
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
                if type == "chatCmdError" {
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
        return NSEChatResponse.response(type: type ?? "invalid", json: json ?? s)
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
