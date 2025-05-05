//
//  API.swift
//  SimpleX NSE
//
//  Created by Evgeny on 26/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation

private var chatController: chat_ctrl?

private var migrationResult: (Bool, DBMigrationResult)?

public func hasChatCtrl() -> Bool {
    chatController != nil
}

public func getChatCtrl() -> chat_ctrl {
    if let controller = chatController { return controller }
    fatalError("chat controller not initialized")
}

public func chatMigrateInit(_ useKey: String? = nil, confirmMigrations: MigrationConfirmation? = nil, backgroundMode: Bool = false) -> (Bool, DBMigrationResult) {
    if let res = migrationResult { return res }
    let dbPath = getAppDatabasePath().path
    var dbKey = ""
    let useKeychain = storeDBPassphraseGroupDefault.get()
    logger.debug("chatMigrateInit uses keychain: \(useKeychain)")
    if let key = useKey {
        dbKey = key
    } else if useKeychain {
        if !hasDatabase() {
            logger.debug("chatMigrateInit generating a random DB key")
            dbKey = randomDatabasePassword()
            initialRandomDBPassphraseGroupDefault.set(true)
        } else if let key = kcDatabasePassword.get() {
            dbKey = key
        }
    }
    let confirm = confirmMigrations ?? defaultMigrationConfirmation()
    logger.debug("chatMigrateInit DB path: \(dbPath), confirm: \(confirm.rawValue)")
//    logger.debug("chatMigrateInit DB key: \(dbKey)")
    var cPath = dbPath.cString(using: .utf8)!
    var cKey = dbKey.cString(using: .utf8)!
    var cConfirm = confirm.rawValue.cString(using: .utf8)!
    // the last parameter of chat_migrate_init is used to return the pointer to chat controller
    let cjson = chat_migrate_init_key(&cPath, &cKey, 1, &cConfirm, backgroundMode ? 1 : 0, &chatController)!
    let dbRes = dbMigrationResult(dataFromCString(cjson))
    let encrypted = dbKey != ""
    let keychainErr = dbRes == .ok && useKeychain && encrypted && !kcDatabasePassword.set(dbKey)
    let result = (encrypted, keychainErr ? .errorKeychain : dbRes)
    migrationResult = result
    return result
}

public func chatInitTemporaryDatabase(url: URL, key: String? = nil, confirmation: MigrationConfirmation = .error) -> (DBMigrationResult, chat_ctrl?) {
    let dbPath = url.path
    let dbKey = key ?? randomDatabasePassword()
    logger.debug("chatInitTemporaryDatabase path: \(dbPath)")
    var temporaryController: chat_ctrl? = nil
    var cPath = dbPath.cString(using: .utf8)!
    var cKey = dbKey.cString(using: .utf8)!
    var cConfirm = confirmation.rawValue.cString(using: .utf8)!
    let cjson = chat_migrate_init_key(&cPath, &cKey, 1, &cConfirm, 0, &temporaryController)!
    return (dbMigrationResult(dataFromCString(cjson)), temporaryController)
}

public func chatInitControllerRemovingDatabases() {
    let dbPath = getAppDatabasePath().path
    let fm = FileManager.default
    // Remove previous databases, otherwise, can be .errorNotADatabase with nil controller
    try? fm.removeItem(atPath: dbPath + CHAT_DB)
    try? fm.removeItem(atPath: dbPath + AGENT_DB)

    let dbKey = randomDatabasePassword()
    logger.debug("chatInitControllerRemovingDatabases path: \(dbPath)")
    var cPath = dbPath.cString(using: .utf8)!
    var cKey = dbKey.cString(using: .utf8)!
    var cConfirm = MigrationConfirmation.error.rawValue.cString(using: .utf8)!
    chat_migrate_init_key(&cPath, &cKey, 1, &cConfirm, 0, &chatController)

    // We need only controller, not databases
    try? fm.removeItem(atPath: dbPath + CHAT_DB)
    try? fm.removeItem(atPath: dbPath + AGENT_DB)
}


public func chatCloseStore() {
    // Prevent crash when exiting the app with already closed store (for example, after changing a database passpharase)
    guard hasChatCtrl() else {
        logger.error("chatCloseStore: already closed, chatCtrl is nil")
        return
    }
    let err = fromCString(chat_close_store(getChatCtrl()))
    if err != "" {
        logger.error("chatCloseStore error: \(err)")
    }
}

public func chatReopenStore() {
    let err = fromCString(chat_reopen_store(getChatCtrl()))
    if err != "" {
        logger.error("chatReopenStore error: \(err)")
    }
}

public func resetChatCtrl() {
    chatController = nil
    migrationResult = nil
}

@inline(__always)
public func sendSimpleXCmd<R: ChatAPIResult>(_ cmd: ChatCmdProtocol, _ ctrl: chat_ctrl? = nil) -> APIResult<R> {
    if let d = sendSimpleXCmdStr(cmd.cmdString, ctrl) {    
        decodeAPIResult(d)
    } else {
        APIResult.error(.invalidJSON(json: nil))
    }
}

@inline(__always)
public func sendSimpleXCmdStr(_ cmd: String, _ ctrl: chat_ctrl? = nil) -> Data? {
    var c = cmd.cString(using: .utf8)!
    return if let cjson = chat_send_cmd(ctrl ?? getChatCtrl(), &c) {
        dataFromCString(cjson)
    } else {
        nil
    }
}

// in microseconds
public let MESSAGE_TIMEOUT: Int32 = 15_000_000

@inline(__always)
public func recvSimpleXMsg<R: ChatAPIResult>(_ ctrl: chat_ctrl? = nil, messageTimeout: Int32 = MESSAGE_TIMEOUT) -> APIResult<R>? {
    if let cjson = chat_recv_msg_wait(ctrl ?? getChatCtrl(), messageTimeout),
       let d = dataFromCString(cjson) {
        decodeAPIResult(d)
    } else {
        nil
    }
}

public func parseSimpleXMarkdown(_ s: String) -> [FormattedText]? {
    var c = s.cString(using: .utf8)!
    if let cjson = chat_parse_markdown(&c) {
        if let d = dataFromCString(cjson) {
            do {
                let r = try jsonDecoder.decode(ParsedMarkdown.self, from: d)
                return r.formattedText
            } catch {
                logger.error("parseSimpleXMarkdown jsonDecoder.decode error: \(error.localizedDescription)")
            }
        }
    }
    return nil
}

public func chatJsonLength(_ s: String) -> Int {
    var c = s.cString(using: .utf8)!
    return Int(chat_json_length(&c))
}

struct ParsedMarkdown: Decodable {
    var formattedText: [FormattedText]?
}

public func parseServerAddress(_ s: String) -> ServerAddress? {
    var c = s.cString(using: .utf8)!
    if let cjson = chat_parse_server(&c) {
         if let d = dataFromCString(cjson) {
            do {
                let r = try jsonDecoder.decode(ParsedServerAddress.self, from: d)
                return r.serverAddress
            } catch {
                logger.error("parseServerAddress jsonDecoder.decode error: \(error.localizedDescription)")
            }
        }
    }
    return nil
}

struct ParsedServerAddress: Decodable {
    var serverAddress: ServerAddress?
    var parseError: String
}

@inline(__always)
public func fromCString(_ c: UnsafeMutablePointer<CChar>) -> String {
    let s = String.init(cString: c)
    free(c)
    return s
}

@inline(__always)
public func dataFromCString(_ c: UnsafeMutablePointer<CChar>) -> Data? {
    let len = strlen(c)
    if len > 0 {
        return Data(bytesNoCopy: c, count: len, deallocator: .free)
    } else {
        free(c)
        return nil
    }
}

@inline(__always)
public func dataToString(_ d: Data?) -> String {
    if let d {
        String(data: d, encoding: .utf8) ?? "invalid string"
    } else {
        "no data"
    }
}

public func decodeUser_(_ jDict: NSDictionary) -> UserRef? {
    if let user_ = jDict["user_"] {
        try? decodeObject(user_ as Any)
    } else {
        nil
    }
}

public func errorJson(_ jDict: NSDictionary) -> Data? {
    if let chatError = jDict["chatError"] {
        serializeJSON(chatError)
    } else {
        serializeJSON(jDict)
    }
}

public func parseChatData(_ jChat: Any, _ jNavInfo: Any? = nil) throws -> (ChatData, NavigationInfo) {
    let jChatDict = jChat as! NSDictionary
    let chatInfo: ChatInfo = try decodeObject(jChatDict["chatInfo"]!)
    let chatStats: ChatStats = try decodeObject(jChatDict["chatStats"]!)
    let navInfo: NavigationInfo = if let jNavInfo = jNavInfo as? NSDictionary, let jNav = jNavInfo["navInfo"] {
        try decodeObject(jNav)
    } else {
        NavigationInfo()
    }
    let jChatItems = jChatDict["chatItems"] as! NSArray
    let chatItems = jChatItems.map { jCI in
        if let ci: ChatItem = try? decodeObject(jCI) {
            return ci
        }
        return ChatItem.invalidJSON(
            chatDir: decodeProperty(jCI, "chatDir"),
            meta: decodeProperty(jCI, "meta"),
            json: serializeJSON(jCI, options: .prettyPrinted)
        )
    }
    return (ChatData(chatInfo: chatInfo, chatItems: chatItems, chatStats: chatStats), navInfo)
}

@inline(__always)
public func decodeObject<T: Decodable>(_ obj: Any) throws -> T {
    try jsonDecoder.decode(T.self, from: JSONSerialization.data(withJSONObject: obj))
}

@inline(__always)
func decodeProperty<T: Decodable>(_ obj: Any, _ prop: NSString) -> T? {
    if let jProp = (obj as? NSDictionary)?[prop] {
        return try? decodeObject(jProp)
    }
    return nil
}

@inline(__always)
func getOWSF(_ obj: NSDictionary, _ prop: NSString) -> (type: String, object: NSDictionary)? {
    if let j = obj[prop] as? NSDictionary, j.count == 1 || j.count == 2 {
        var type = j.allKeys[0] as? String
        if j.count == 2 && type == "_owsf" {
            type = j.allKeys[1] as? String
        }
        if let type {
            return (type, j)
        }
    }
    return nil
}

@inline(__always)
public func serializeJSON(_ obj: Any, options: JSONSerialization.WritingOptions = []) -> Data? {
    if let d = try? JSONSerialization.data(withJSONObject: obj, options: options) {
        dataPrefix(d)
    } else {
        nil
    }
}

let MAX_JSON_VIEW_LENGTH = 2048

@inline(__always)
public func dataPrefix(_ d: Data) -> Data {
    d.count > MAX_JSON_VIEW_LENGTH
    ? Data(d.prefix(MAX_JSON_VIEW_LENGTH))
    : d
}

public func responseError(_ err: Error) -> String {
    if let e = err as? ChatError {
        chatErrorString(e)
    } else {
        String(describing: err)
    }
}

public func chatErrorString(_ err: ChatError) -> String {
    switch err {
    case let .invalidJSON(json): dataToString(json)
    case let .unexpectedResult(type): "unexpected result: \(type)"
    default: String(describing: err)
    }
}

public enum DBMigrationResult: Decodable, Equatable {
    case ok
    case invalidConfirmation
    case errorNotADatabase(dbFile: String)
    case errorMigration(dbFile: String, migrationError: MigrationError)
    case errorSQL(dbFile: String, migrationSQLError: String)
    case errorKeychain
    case unknown(json: String)
}

public enum MigrationConfirmation: String {
    case yesUp
    case yesUpDown
    case error
}

public func defaultMigrationConfirmation() -> MigrationConfirmation {
    confirmDBUpgradesGroupDefault.get() ? .error : .yesUp
}

public enum MigrationError: Decodable, Equatable {
    case upgrade(upMigrations: [UpMigration])
    case downgrade(downMigrations: [String])
    case migrationError(mtrError: MTRError)
}

public struct UpMigration: Decodable, Equatable {
    public var upName: String
//    public var withDown: Bool
}

public enum MTRError: Decodable, Equatable {
    case noDown(dbMigrations: [String])
    case different(appMigration: String, dbMigration: String)
}

func dbMigrationResult(_ d: Data?) -> DBMigrationResult {
    if let d {
        do {
            return try jsonDecoder.decode(DBMigrationResult.self, from: d)
        } catch let error {
            logger.error("chatResponse jsonDecoder.decode error: \(error.localizedDescription)")
            return .unknown(json: dataToString(d))
        }
    } else {
        return .unknown(json: "no data")
    }
}
