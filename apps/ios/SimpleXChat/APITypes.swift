//
//  SimpleXAPI.swift
//  SimpleX NSE
//
//  Created by Evgeny on 26/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI
import Network

public let jsonDecoder = getJSONDecoder()
public let jsonEncoder = getJSONEncoder()

public protocol ChatCmdProtocol {
    var cmdString: String { get }
}

@inline(__always)
public func onOff(_ b: Bool) -> String {
    b ? "on" : "off"
}

public enum APIResult<R>: Decodable where R: Decodable, R: ChatAPIResult {
    case result(R)
    case error(ChatError)
    case invalid(type: String, json: Data)
    
    public var responseType: String {
        switch self {
        case let .result(r): r.responseType
        case let .error(e): "error \(e.errorType)"
        case let .invalid(type, _): "* \(type)"
        }
    }
    
    public var unexpected: ChatError {
        switch self {
        case let .result(r): .unexpectedResult(type: r.responseType)
        case let .error(e): e
        case let .invalid(type, _): .unexpectedResult(type: "* \(type)")
        }
    }

    public init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        if container.contains(.result) {
            let result = try container.decode(R.self, forKey: .result)
            self = .result(result)
        } else {
            let error = try container.decode(ChatError.self, forKey: .error)
            self = .error(error)
        }
    }
    
    private enum CodingKeys: String, CodingKey {
        case result, error
    }
}

public protocol ChatAPIResult: Decodable {
    var responseType: String { get }
    var details: String { get }
    static func fallbackResult(_ type: String, _ json: NSDictionary) -> Self?
}

extension ChatAPIResult {
    public var noDetails: String { "\(self.responseType): no details" }
    
    @inline(__always)
    public static func fallbackResult(_ type: String, _ json: NSDictionary) -> Self? {
        nil
    }
    
    @inline(__always)
    public var unexpected: ChatError {
        .unexpectedResult(type: self.responseType)
    }
}

public func decodeAPIResult<R: ChatAPIResult>(_ d: Data) -> APIResult<R> {
//    print("decodeAPIResult \(String(describing: R.self))")
    do {
//        return try withStackSizeLimit { try jsonDecoder.decode(APIResult<R>.self, from: d) }
        return try jsonDecoder.decode(APIResult<R>.self, from: d)
    } catch {}
    if let j = try? JSONSerialization.jsonObject(with: d) as? NSDictionary {
        if let (_, jErr) = getOWSF(j, "error") {
            return APIResult.error(.invalidJSON(json: errorJson(jErr))) as APIResult<R>
        } else if let (type, jRes) = getOWSF(j, "result") {
            return if let r = R.fallbackResult(type, jRes) {
                APIResult.result(r)
            } else {
                APIResult.invalid(type: type, json: dataPrefix(d))
            }
        }
    }
    return APIResult.invalid(type: "invalid", json: dataPrefix(d))
}

// Default stack size for the main thread is 1mb, for secondary threads - 512 kb.
// This function can be used to test what size is used (or to increase available stack size).
// Stack size must be a multiple of system page size (16kb).
//private let stackSizeLimit: Int = 256 * 1024
//
//private func withStackSizeLimit<T>(_ f: @escaping () throws -> T) throws -> T {
//    let semaphore = DispatchSemaphore(value: 0)
//    var result: Result<T, Error>?
//    let thread = Thread {
//        do {
//            result = .success(try f())
//        } catch {
//            result = .failure(error)
//        }
//        semaphore.signal()
//    }
//
//    thread.stackSize = stackSizeLimit
//    thread.qualityOfService = Thread.current.qualityOfService
//    thread.start()
//
//    semaphore.wait()
//
//    switch result! {
//    case let .success(r): return r
//    case let .failure(e): throw e
//    }
//}

public func parseApiChats(_ jResp: NSDictionary) -> (user: UserRef, chats: [ChatData])? {
    if let jApiChats = jResp["apiChats"] as? NSDictionary,
       let user: UserRef = try? decodeObject(jApiChats["user"] as Any),
       let jChats = jApiChats["chats"] as? NSArray {
        let chats = jChats.map { jChat in
            if let chatData = try? parseChatData(jChat) {
                return chatData.0
            }
            return ChatData.invalidJSON(serializeJSON(jChat, options: .prettyPrinted))
        }
        return (user, chats)
    } else {
        return nil
    }
}

public func withUser(_ u: (any UserLike)?, _ s: String) -> String {
    if let id = u?.userId {
        return "userId: \(id)\n\(s)"
    }
    return s
}

public struct CreatedConnLink: Decodable, Hashable {
    public var connFullLink: String
    public var connShortLink: String?

    public init(connFullLink: String, connShortLink: String?) {
        self.connFullLink = connFullLink
        self.connShortLink = connShortLink
    }

    public func simplexChatUri(short: Bool = true) -> String {
        short ? (connShortLink ?? simplexChatLink(connFullLink)) : simplexChatLink(connFullLink)
    }
}

public func simplexChatLink(_ uri: String) -> String {
    uri.starts(with: "simplex:/")
    ? uri.replacingOccurrences(of: "simplex:/", with: "https://simplex.chat/")
    : uri
}

public struct ComposedMessage: Encodable {
    public var fileSource: CryptoFile?
    var quotedItemId: Int64?
    public var msgContent: MsgContent
    public var mentions: [String: Int64]

    public init(fileSource: CryptoFile? = nil, quotedItemId: Int64? = nil, msgContent: MsgContent, mentions: [String: Int64] = [:]) {
        self.fileSource = fileSource
        self.quotedItemId = quotedItemId
        self.msgContent = msgContent
        self.mentions = mentions
    }
}

public enum ServerProtocol: String, Decodable {
    case smp
    case xftp
}

public struct ServerAddress: Decodable {
    public var serverProtocol: ServerProtocol
    public var hostnames: [String]
    public var port: String
    public var keyHash: String
    public var basicAuth: String

    public init(serverProtocol: ServerProtocol, hostnames: [String], port: String, keyHash: String, basicAuth: String = "") {
        self.serverProtocol = serverProtocol
        self.hostnames = hostnames
        self.port = port
        self.keyHash = keyHash
        self.basicAuth = basicAuth
    }

    public var uri: String {
        "\(serverProtocol)://\(keyHash)\(basicAuth == "" ? "" : ":" + basicAuth)@\(hostnames.joined(separator: ","))"
    }

    public var valid: Bool {
        hostnames.count > 0 && Set(hostnames).count == hostnames.count
    }

    static func empty(_ serverProtocol: ServerProtocol) -> ServerAddress {
        ServerAddress(
            serverProtocol: serverProtocol,
            hostnames: [],
            port: "",
            keyHash: "",
            basicAuth: ""
        )
    }

    static public var sampleData = ServerAddress(
        serverProtocol: .smp,
        hostnames: ["smp.simplex.im", "1234.onion"],
        port: "",
        keyHash: "LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=",
        basicAuth: "server_password"
    )
}

public struct NetCfg: Codable, Equatable {
    public var socksProxy: String? = nil
    var socksMode: SocksMode = .always
    public var hostMode: HostMode = .publicHost
    public var requiredHostMode = true
    public var sessionMode = TransportSessionMode.user
    public var smpProxyMode: SMPProxyMode = .always
    public var smpProxyFallback: SMPProxyFallback = .allowProtected
    public var smpWebPortServers: SMPWebPortServers = .preset
    public var tcpConnectTimeout: Int // microseconds
    public var tcpTimeout: Int // microseconds
    public var tcpTimeoutPerKb: Int // microseconds
    public var rcvConcurrency: Int // pool size
    public var tcpKeepAlive: KeepAliveOpts? = KeepAliveOpts.defaults
    public var smpPingInterval: Int // microseconds
    public var smpPingCount: Int = 3 // times
    public var logTLSErrors: Bool = false

    public static let defaults: NetCfg = NetCfg(
        tcpConnectTimeout: 25_000_000,
        tcpTimeout: 15_000_000,
        tcpTimeoutPerKb: 10_000,
        rcvConcurrency: 12,
        smpPingInterval: 1200_000_000
    )

    static let proxyDefaults: NetCfg = NetCfg(
        tcpConnectTimeout: 35_000_000,
        tcpTimeout: 20_000_000,
        tcpTimeoutPerKb: 15_000,
        rcvConcurrency: 8,
        smpPingInterval: 1200_000_000
    )

    public var withProxyTimeouts: NetCfg {
        var cfg = self
        cfg.tcpConnectTimeout = NetCfg.proxyDefaults.tcpConnectTimeout
        cfg.tcpTimeout = NetCfg.proxyDefaults.tcpTimeout
        cfg.tcpTimeoutPerKb = NetCfg.proxyDefaults.tcpTimeoutPerKb
        cfg.rcvConcurrency = NetCfg.proxyDefaults.rcvConcurrency
        cfg.smpPingInterval = NetCfg.proxyDefaults.smpPingInterval
        return cfg
    }

    public var hasProxyTimeouts: Bool {
        tcpConnectTimeout == NetCfg.proxyDefaults.tcpConnectTimeout &&
        tcpTimeout == NetCfg.proxyDefaults.tcpTimeout &&
        tcpTimeoutPerKb == NetCfg.proxyDefaults.tcpTimeoutPerKb &&
        rcvConcurrency == NetCfg.proxyDefaults.rcvConcurrency &&
        smpPingInterval == NetCfg.proxyDefaults.smpPingInterval
    }

    public var enableKeepAlive: Bool { tcpKeepAlive != nil }
}

public enum HostMode: String, Codable {
    case onionViaSocks
    case onionHost = "onion"
    case publicHost = "public"
}

public enum SocksMode: String, Codable {
    case always = "always"
    case onion = "onion"
}

public enum SMPProxyMode: String, Codable, SelectableItem {
    case always = "always"
    case unknown = "unknown"
    case unprotected = "unprotected"
    case never = "never"

    public var label: LocalizedStringKey {
        switch self {
        case .always: return "always"
        case .unknown: return "unknown servers"
        case .unprotected: return "unprotected"
        case .never: return "never"
        }
    }

    public var id: SMPProxyMode { self }

    public static let values: [SMPProxyMode] = [.always, .unknown, .unprotected, .never]
}

public enum SMPProxyFallback: String, Codable, SelectableItem {
    case allow = "allow"
    case allowProtected = "allowProtected"
    case prohibit = "prohibit"

    public var label: LocalizedStringKey {
        switch self {
        case .allow: return "yes"
        case .allowProtected: return "when IP hidden"
        case .prohibit: return "no"
        }
    }

    public var id: SMPProxyFallback { self }

    public static let values: [SMPProxyFallback] = [.allow, .allowProtected, .prohibit]
}

public enum SMPWebPortServers: String, Codable, CaseIterable {
    case all = "all"
    case preset = "preset"
    case off = "off"
    
    public var text: LocalizedStringKey {
        switch self {
        case .all: "All servers"
        case .preset: "Preset servers"
        case .off: "Off"
        }
    }
}

public enum OnionHosts: String, Identifiable {
    case no
    case prefer
    case require

    public var text: LocalizedStringKey {
        switch self {
        case .no: return "No"
        case .prefer: return "When available"
        case .require: return "Required"
        }
    }

    public var hostMode: (HostMode, Bool) {
        switch self {
        case .no: return (.publicHost, true)
        case .prefer: return (.onionHost, false)
        case .require: return (.onionHost, true)
        }
    }

    public init(netCfg: NetCfg) {
        switch netCfg.hostMode {
        case .onionViaSocks: self = .no
        case .onionHost: self = netCfg.requiredHostMode ? .require : .prefer
        case .publicHost: self = .no
        }
    }

    public var id: OnionHosts { self }

    public static let values: [OnionHosts] = [.no, .prefer, .require]
}

public enum TransportSessionMode: String, Codable, Identifiable {
    case user
    case session
    case server
    case entity

    public var text: LocalizedStringKey {
        switch self {
        case .user: return "Chat profile"
        case .session: return "App session"
        case .server: return "Server"
        case .entity: return "Connection"
        }
    }

    public var id: TransportSessionMode { self }

    public static let values: [TransportSessionMode] = [.user, .session, .server, .entity]
}

public struct KeepAliveOpts: Codable, Equatable {
    public var keepIdle: Int // seconds
    public var keepIntvl: Int // seconds
    public var keepCnt: Int // times

    public static let defaults: KeepAliveOpts = KeepAliveOpts(keepIdle: 30, keepIntvl: 15, keepCnt: 4)
}

public struct NetworkProxy: Equatable, Codable {
    public var host: String = ""
    public var port: Int = 0
    public var auth: NetworkProxyAuth = .username
    public var username: String = ""
    public var password: String = ""

    public static var def: NetworkProxy {
        NetworkProxy()
    }

    public var valid: Bool {
        let hostOk = switch NWEndpoint.Host(host) {
        case .ipv4: true
        case .ipv6: true
        default: false
        }
        return hostOk &&
                port > 0 && port <= 65535 &&
                NetworkProxy.validCredential(username) && NetworkProxy.validCredential(password)
    }

    public static func validCredential(_ s: String) -> Bool {
        !s.contains(":") && !s.contains("@")
    }

    public func toProxyString() -> String? {
        if !valid { return nil }
        var res = ""
        switch auth {
        case .username:
            let usernameTrimmed = username.trimmingCharacters(in: .whitespaces)
            let passwordTrimmed = password.trimmingCharacters(in: .whitespaces)
            if usernameTrimmed != "" || passwordTrimmed != "" {
                res += usernameTrimmed + ":" + passwordTrimmed + "@"
            } else {
                res += "@"
            }
        case .isolate: ()
        }
        if host != "" {
            if host.contains(":") {
                res += "[\(host.trimmingCharacters(in: [" ", "[", "]"]))]"
            } else {
                res += host.trimmingCharacters(in: .whitespaces)
            }
        }
        res += ":\(port)"
        return res
    }
}

public enum NetworkProxyAuth: String, Codable {
    case username
    case isolate
}

public struct ChatSettings: Codable, Hashable {
    public var enableNtfs: MsgFilter
    public var sendRcpts: Bool?
    public var favorite: Bool

    public init(enableNtfs: MsgFilter, sendRcpts: Bool?, favorite: Bool) {
        self.enableNtfs = enableNtfs
        self.sendRcpts = sendRcpts
        self.favorite = favorite
    }

    public static let defaults: ChatSettings = ChatSettings(enableNtfs: .all, sendRcpts: nil, favorite: false)
}

public struct NavigationInfo: Decodable {
    public var afterUnread: Int = 0
    public var afterTotal: Int = 0

    public init(afterUnread: Int = 0, afterTotal: Int = 0) {
        self.afterUnread = afterUnread
        self.afterTotal = afterTotal
    }
}

public enum MsgFilter: String, Codable, Hashable {
    case none
    case all
    case mentions
    
    public func nextMode(mentions: Bool) -> MsgFilter {
        switch self {
        case .all: mentions ? .mentions : .none
        case .mentions: .none
        case .none: .all
        }
    }
    
    public func text(mentions: Bool) -> String {
        switch self {
        case .all: NSLocalizedString("Unmute", comment: "notification label action")
        case .mentions: NSLocalizedString("Mute", comment: "notification label action")
        case .none:
            mentions
            ? NSLocalizedString("Mute all", comment: "notification label action")
            : NSLocalizedString("Mute", comment: "notification label action")
        }
    }
    
    public var icon: String {
        return switch self {
        case .all: "speaker.wave.2"
        case .mentions: "speaker.badge.exclamationmark"
        case .none: "speaker.slash"
        }
    }
    
    public var iconFilled: String {
        return switch self {
        case .all: "speaker.wave.2.fill"
        case .mentions: "speaker.badge.exclamationmark.fill"
        case .none: "speaker.slash.fill"
        }
    }
}

public struct ConnectionStats: Decodable, Hashable {
    public var connAgentVersion: Int
    public var rcvQueuesInfo: [RcvQueueInfo]
    public var sndQueuesInfo: [SndQueueInfo]
    public var ratchetSyncState: RatchetSyncState
    public var ratchetSyncSupported: Bool

    public var ratchetSyncAllowed: Bool {
        ratchetSyncSupported && [.allowed, .required].contains(ratchetSyncState)
    }

    public var ratchetSyncSendProhibited: Bool {
        [.required, .started, .agreed].contains(ratchetSyncState)
    }

    public var ratchetSyncInProgress: Bool {
        [.started, .agreed].contains(ratchetSyncState)
    }
}

public struct RcvQueueInfo: Codable, Hashable {
    public var rcvServer: String
    public var rcvSwitchStatus: RcvSwitchStatus?
    public var canAbortSwitch: Bool
}

public enum RcvSwitchStatus: String, Codable, Hashable {
    case switchStarted = "switch_started"
    case sendingQADD = "sending_qadd"
    case sendingQUSE = "sending_quse"
    case receivedMessage = "received_message"
}

public struct SndQueueInfo: Codable, Hashable {
    public var sndServer: String
    public var sndSwitchStatus: SndSwitchStatus?
}

public enum SndSwitchStatus: String, Codable, Hashable {
    case sendingQKEY = "sending_qkey"
    case sendingQTEST = "sending_qtest"
}

public enum QueueDirection: String, Decodable {
    case rcv
    case snd
}

public struct SwitchProgress: Decodable {
    public var queueDirection: QueueDirection
    public var switchPhase: SwitchPhase
    public var connectionStats: ConnectionStats
}

public struct RatchetSyncProgress: Decodable {
    public var ratchetSyncStatus: RatchetSyncState
    public var connectionStats: ConnectionStats
}

public enum RatchetSyncState: String, Decodable {
    case ok
    case allowed
    case required
    case started
    case agreed
}

public protocol SelectableItem: Identifiable, Equatable {
    var label: LocalizedStringKey { get }
    static var values: [Self] { get }
}

public enum NotificationPreviewMode: String, SelectableItem, Codable {
    case hidden
    case contact
    case message

    public var label: LocalizedStringKey {
        switch self {
        case .hidden: return "Hidden"
        case .contact: return "Contact name"
        case .message: return "Message text"
        }
    }

    public var id: String { self.rawValue }

    public static var values: [NotificationPreviewMode] = [.message, .contact, .hidden]
}

public func decodeJSON<T: Decodable>(_ json: String) -> T? {
    if let data = json.data(using: .utf8) {
        return try? jsonDecoder.decode(T.self, from: data)
    }
    return nil
}

public func encodeJSON<T: Encodable>(_ value: T) -> String {
    let data = try! jsonEncoder.encode(value)
    return String(decoding: data, as: UTF8.self)
}

private func encodeCJSON<T: Encodable>(_ value: T) -> [CChar] {
    encodeJSON(value).cString(using: .utf8)!
}

public enum ChatError: Decodable, Hashable, Error {
    case error(errorType: ChatErrorType)
    case errorAgent(agentError: AgentErrorType)
    case errorStore(storeError: StoreError)
    case errorDatabase(databaseError: DatabaseError)
    case errorRemoteCtrl(remoteCtrlError: RemoteCtrlError)
    case invalidJSON(json: Data?) // additional case used to pass errors that failed to parse
    case unexpectedResult(type: String) // additional case used to pass unexpected responses
    
    public var errorType: String {
        switch self {
        case .error: "chat"
        case .errorAgent: "agent"
        case .errorStore: "store"
        case .errorDatabase: "database"
        case .errorRemoteCtrl: "remoteCtrl"
        case .invalidJSON: "invalid"
        case let .unexpectedResult(type): "! \(type)"
        }
    }
}

public enum ChatErrorType: Decodable, Hashable {
    case noActiveUser
    case noConnectionUser(agentConnId: String)
    case noSndFileUser(agentSndFileId: String)
    case noRcvFileUser(agentRcvFileId: String)
    case userUnknown
    case activeUserExists
    case userExists
    case invalidDisplayName
    case differentActiveUser(commandUserId: Int64, activeUserId: Int64)
    case cantDeleteActiveUser(userId: Int64)
    case cantDeleteLastUser(userId: Int64)
    case cantHideLastUser(userId: Int64)
    case hiddenUserAlwaysMuted(userId: Int64)
    case emptyUserPassword(userId: Int64)
    case userAlreadyHidden(userId: Int64)
    case userNotHidden(userId: Int64)
    case chatNotStarted
    case chatNotStopped
    case chatStoreChanged
    case invalidConnReq
    case unsupportedConnReq
    case invalidChatMessage(connection: Connection, message: String)
    case contactNotReady(contact: Contact)
    case contactNotActive(contact: Contact)
    case contactDisabled(contact: Contact)
    case connectionDisabled(connection: Connection)
    case groupUserRole(groupInfo: GroupInfo, requiredRole: GroupMemberRole)
    case groupMemberInitialRole(groupInfo: GroupInfo, initialRole: GroupMemberRole)
    case contactIncognitoCantInvite
    case groupIncognitoCantInvite
    case groupContactRole(contactName: ContactName)
    case groupDuplicateMember(contactName: ContactName)
    case groupDuplicateMemberId
    case groupNotJoined(groupInfo: GroupInfo)
    case groupMemberNotActive
    case groupMemberUserRemoved
    case groupMemberNotFound
    case groupCantResendInvitation(groupInfo: GroupInfo, contactName: ContactName)
    case groupInternal(message: String)
    case fileNotFound(message: String)
    case fileSize(filePath: String)
    case fileAlreadyReceiving(message: String)
    case fileCancelled(message: String)
    case fileCancel(fileId: Int64, message: String)
    case fileAlreadyExists(filePath: String)
    case fileRead(filePath: String, message: String)
    case fileWrite(filePath: String, message: String)
    case fileSend(fileId: Int64, agentError: String)
    case fileRcvChunk(message: String)
    case fileInternal(message: String)
    case fileImageType(filePath: String)
    case fileImageSize(filePath: String)
    case fileNotReceived(fileId: Int64)
    case fileNotApproved(fileId: Int64, unknownServers: [String])
    case fallbackToSMPProhibited(fileId: Int64)
    case inlineFileProhibited(fileId: Int64)
    case invalidQuote
    case invalidForward
    case invalidChatItemUpdate
    case invalidChatItemDelete
    case hasCurrentCall
    case noCurrentCall
    case callContact(contactId: Int64)
    case callState
    case directMessagesProhibited(contact: Contact)
    case agentVersion
    case agentNoSubResult(agentConnId: String)
    case commandError(message: String)
    case serverProtocol
    case agentCommandError(message: String)
    case invalidFileDescription(message: String)
    case connectionIncognitoChangeProhibited
    case connectionUserChangeProhibited
    case peerChatVRangeIncompatible
    case internalError(message: String)
    case exception(message: String)
}

public enum StoreError: Decodable, Hashable {
    case duplicateName
    case userNotFound(userId: Int64)
    case userNotFoundByName(contactName: ContactName)
    case userNotFoundByContactId(contactId: Int64)
    case userNotFoundByGroupId(groupId: Int64)
    case userNotFoundByFileId(fileId: Int64)
    case userNotFoundByContactRequestId(contactRequestId: Int64)
    case contactNotFound(contactId: Int64)
    case contactNotFoundByName(contactName: ContactName)
    case contactNotFoundByMemberId(groupMemberId: Int64)
    case contactNotReady(contactName: ContactName)
    case duplicateContactLink
    case userContactLinkNotFound
    case contactRequestNotFound(contactRequestId: Int64)
    case contactRequestNotFoundByName(contactName: ContactName)
    case groupNotFound(groupId: Int64)
    case groupNotFoundByName(groupName: GroupName)
    case groupMemberNameNotFound(groupId: Int64, groupMemberName: ContactName)
    case groupMemberNotFound(groupMemberId: Int64)
    case groupMemberNotFoundByMemberId(memberId: String)
    case memberContactGroupMemberNotFound(contactId: Int64)
    case groupWithoutUser
    case duplicateGroupMember
    case groupAlreadyJoined
    case groupInvitationNotFound
    case sndFileNotFound(fileId: Int64)
    case sndFileInvalid(fileId: Int64)
    case rcvFileNotFound(fileId: Int64)
    case rcvFileDescrNotFound(fileId: Int64)
    case fileNotFound(fileId: Int64)
    case rcvFileInvalid(fileId: Int64)
    case rcvFileInvalidDescrPart
    case sharedMsgIdNotFoundByFileId(fileId: Int64)
    case fileIdNotFoundBySharedMsgId(sharedMsgId: String)
    case sndFileNotFoundXFTP(agentSndFileId: String)
    case rcvFileNotFoundXFTP(agentRcvFileId: String)
    case extraFileDescrNotFoundXFTP(fileId: Int64)
    case connectionNotFound(agentConnId: String)
    case connectionNotFoundById(connId: Int64)
    case connectionNotFoundByMemberId(groupMemberId: Int64)
    case pendingConnectionNotFound(connId: Int64)
    case introNotFound
    case uniqueID
    case internalError(message: String)
    case noMsgDelivery(connId: Int64, agentMsgId: String)
    case badChatItem(itemId: Int64)
    case chatItemNotFound(itemId: Int64)
    case chatItemNotFoundByText(text: String)
    case chatItemSharedMsgIdNotFound(sharedMsgId: String)
    case chatItemNotFoundByFileId(fileId: Int64)
    case chatItemNotFoundByGroupId(groupId: Int64)
    case profileNotFound(profileId: Int64)
    case duplicateGroupLink(groupInfo: GroupInfo)
    case groupLinkNotFound(groupInfo: GroupInfo)
    case hostMemberIdNotFound(groupId: Int64)
    case contactNotFoundByFileId(fileId: Int64)
    case noGroupSndStatus(itemId: Int64, groupMemberId: Int64)
    case dBException(message: String)
}

public enum DatabaseError: Decodable, Hashable {
    case errorEncrypted
    case errorPlaintext
    case errorNoFile(dbFile: String)
    case errorExport(sqliteError: SQLiteError)
    case errorOpen(sqliteError: SQLiteError)
}

public enum SQLiteError: Decodable, Hashable {
    case errorNotADatabase
    case error(dbError: String)
}

public enum AgentErrorType: Decodable, Hashable {
    case CMD(cmdErr: CommandErrorType, errContext: String)
    case CONN(connErr: ConnectionErrorType)
    case SMP(serverAddress: String, smpErr: ProtocolErrorType)
    case NTF(ntfErr: ProtocolErrorType)
    case XFTP(xftpErr: XFTPErrorType)
    case PROXY(proxyServer: String, relayServer: String, proxyErr: ProxyClientError)
    case RCP(rcpErr: RCErrorType)
    case BROKER(brokerAddress: String, brokerErr: BrokerErrorType)
    case AGENT(agentErr: SMPAgentError)
    case INTERNAL(internalErr: String)
    case CRITICAL(offerRestart: Bool, criticalErr: String)
    case INACTIVE
}

public enum CommandErrorType: Decodable, Hashable {
    case PROHIBITED
    case SYNTAX
    case NO_CONN
    case SIZE
    case LARGE
}

public enum ConnectionErrorType: Decodable, Hashable {
    case NOT_FOUND
    case DUPLICATE
    case SIMPLEX
    case NOT_ACCEPTED
    case NOT_AVAILABLE
}

public enum BrokerErrorType: Decodable, Hashable {
    case RESPONSE(smpErr: String)
    case UNEXPECTED
    case NETWORK
    case HOST
    case TRANSPORT(transportErr: ProtocolTransportError)
    case TIMEOUT
}

public enum ProtocolErrorType: Decodable, Hashable {
    case BLOCK
    case SESSION
    case CMD(cmdErr: ProtocolCommandError)
    indirect case PROXY(proxyErr: ProxyError)
    case AUTH
    case BLOCKED(blockInfo: BlockingInfo)
    case CRYPTO
    case QUOTA
    case STORE(storeErr: String)
    case NO_MSG
    case LARGE_MSG
    case EXPIRED
    case INTERNAL
}

public enum ProxyError: Decodable, Hashable {
    case PROTOCOL(protocolErr: ProtocolErrorType)
    case BROKER(brokerErr: BrokerErrorType)
    case BASIC_AUTH
    case NO_SESSION
}

public struct BlockingInfo: Decodable, Equatable, Hashable {
    public var reason: BlockingReason
}

public enum BlockingReason: String, Decodable {
    case spam
    case content

    public var text: String {
        switch self {
        case .spam: NSLocalizedString("Spam", comment: "blocking reason")
        case .content: NSLocalizedString("Content violates conditions of use", comment: "blocking reason")
        }
    }
}

public enum XFTPErrorType: Decodable, Hashable {
    case BLOCK
    case SESSION
    case CMD(cmdErr: ProtocolCommandError)
    case AUTH
    case BLOCKED(blockInfo: BlockingInfo)
    case SIZE
    case QUOTA
    case DIGEST
    case CRYPTO
    case NO_FILE
    case HAS_FILE
    case FILE_IO
    case TIMEOUT
    case REDIRECT(redirectError: String)
    case INTERNAL
}

public enum ProxyClientError: Decodable, Hashable {
    case protocolError(protocolErr: ProtocolErrorType)
    case unexpectedResponse(responseStr: String)
    case responseError(responseErr: ProtocolErrorType)
}

public enum RCErrorType: Decodable, Hashable {
    case `internal`(internalErr: String)
    case identity
    case noLocalAddress
    case newController
    case notDiscovered
    case tlsStartFailed
    case exception(exception: String)
    case ctrlAuth
    case ctrlNotFound
    case ctrlError(ctrlErr: String)
    case version
    case encrypt
    case decrypt
    case blockSize
    case syntax(syntaxErr: String)
}

public enum ProtocolCommandError: Decodable, Hashable {
    case UNKNOWN
    case SYNTAX
    case PROHIBITED
    case NO_AUTH
    case HAS_AUTH
    case NO_ENTITY
}

public enum ProtocolTransportError: Decodable, Hashable {
    case badBlock
    case version
    case largeMsg
    case badSession
    case noServerAuth
    case handshake(handshakeErr: SMPHandshakeError)
}

public enum SMPHandshakeError: Decodable, Hashable {
    case PARSE
    case VERSION
    case IDENTITY
    case BAD_AUTH
}

public enum SMPAgentError: Decodable, Hashable {
    case A_MESSAGE
    case A_PROHIBITED
    case A_VERSION
    case A_CRYPTO
    case A_DUPLICATE
    case A_QUEUE(queueErr: String)
}

public enum ArchiveError: Decodable, Hashable {
    case `import`(importError: String)
    case fileError(file: String, fileError: String)
}

public enum RemoteCtrlError: Decodable, Hashable {
    case inactive
    case badState
    case busy
    case timeout
    case noKnownControllers
    case badController
    case disconnected(remoteCtrlId: Int64, reason: String)
    case badInvitation
    case badVersion(appVersion: String)
    case hTTP2Error(http2Error: String)
    case protocolError
}

public struct AppFilePaths: Encodable {
    public let appFilesFolder: String
    public let appTempFolder: String
    public let appAssetsFolder: String
    
    public init(appFilesFolder: String, appTempFolder: String, appAssetsFolder: String) {
        self.appFilesFolder = appFilesFolder
        self.appTempFolder = appTempFolder
        self.appAssetsFolder = appAssetsFolder
    }
}
