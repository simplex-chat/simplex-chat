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
    let dbRes = dbMigrationResult(fromCString(cjson))
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
    return (dbMigrationResult(fromCString(cjson)), temporaryController)
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

public func sendSimpleXCmd(_ cmd: ChatCommand, _ ctrl: chat_ctrl? = nil) -> ChatResponse {
    var c = cmd.cmdString.cString(using: .utf8)!
    let cjson  = chat_send_cmd(ctrl ?? getChatCtrl(), &c)!
    return chatResponse(fromCString(cjson))
}

// in microseconds
let MESSAGE_TIMEOUT: Int32 = 15_000_000

public func recvSimpleXMsg(_ ctrl: chat_ctrl? = nil, messageTimeout: Int32? = nil) -> ChatResponse? {
    if let cjson = chat_recv_msg_wait(ctrl ?? getChatCtrl(), messageTimeout ?? MESSAGE_TIMEOUT) {
        let s = fromCString(cjson)
        return s == "" ? nil : chatResponse(s)
    }
    return nil
}

public func parseSimpleXMarkdown(_ s: String) -> [FormattedText]? {
    var c = s.cString(using: .utf8)!
    if let cjson = chat_parse_markdown(&c) {
        if let d = fromCString(cjson).data(using: .utf8) {
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
         if let d = fromCString(cjson).data(using: .utf8) {
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

public func fromCString(_ c: UnsafeMutablePointer<CChar>) -> String {
    let s = String.init(cString: c)
    free(c)
    return s
}

public func chatResponse(_ s: String) -> ChatResponse {
    let d = s.data(using: .utf8)!
    // TODO is there a way to do it without copying the data? e.g:
    //    let p = UnsafeMutableRawPointer.init(mutating: UnsafeRawPointer(cjson))
    //    let d = Data.init(bytesNoCopy: p, count: strlen(cjson), deallocator: .free)
    do {
        let r = try jsonDecoder.decode(APIResponse.self, from: d)
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
                if let jApiChats = jResp["apiChats"] as? NSDictionary,
                   let user: UserRef = try? decodeObject(jApiChats["user"] as Any),
                   let jChats = jApiChats["chats"] as? NSArray {
                    let chats = jChats.map { jChat in
                        if let chatData = try? parseChatData(jChat) {
                            return chatData
                        }
                        return ChatData.invalidJSON(serializeJSON(jChat, options: .prettyPrinted) ?? "")
                    }
                    return .apiChats(user: user, chats: chats)
                }
            } else if type == "apiChat" {
                if let jApiChat = jResp["apiChat"] as? NSDictionary,
                   let user: UserRef = try? decodeObject(jApiChat["user"] as Any),
                   let jChat = jApiChat["chat"] as? NSDictionary,
                   let chat = try? parseChatData(jChat) {
                    return .apiChat(user: user, chat: chat)
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
    return ChatResponse.response(type: type ?? "invalid", json: json ?? s)
}

private func decodeUser_(_ jDict: NSDictionary) -> UserRef? {
    if let user_ = jDict["user_"] {
        try? decodeObject(user_ as Any)
    } else {
        nil
    }
}

private func errorJson(_ jDict: NSDictionary) -> String? {
    if let chatError = jDict["chatError"] {
        serializeJSON(chatError)
    } else {
        serializeJSON(jDict)
    }
}

func parseChatData(_ jChat: Any) throws -> ChatData {
    let jChatDict = jChat as! NSDictionary
    let chatInfo: ChatInfo = try decodeObject(jChatDict["chatInfo"]!)
    let chatStats: ChatStats = try decodeObject(jChatDict["chatStats"]!)
    let jChatItems = jChatDict["chatItems"] as! NSArray
    let chatItems = jChatItems.map { jCI in
        if let ci: ChatItem = try? decodeObject(jCI) {
            return ci
        }
        return ChatItem.invalidJSON(
            chatDir: decodeProperty(jCI, "chatDir"),
            meta: decodeProperty(jCI, "meta"),
            json: serializeJSON(jCI, options: .prettyPrinted) ?? ""
        )
    }
    return ChatData(chatInfo: chatInfo, chatItems: chatItems, chatStats: chatStats)
}

func decodeObject<T: Decodable>(_ obj: Any) throws -> T {
    try jsonDecoder.decode(T.self, from: JSONSerialization.data(withJSONObject: obj))
}

func decodeProperty<T: Decodable>(_ obj: Any, _ prop: NSString) -> T? {
    if let jProp = (obj as? NSDictionary)?[prop] {
        return try? decodeObject(jProp)
    }
    return nil
}

func serializeJSON(_ obj: Any, options: JSONSerialization.WritingOptions = []) -> String? {
    if let d = try? JSONSerialization.data(withJSONObject: obj, options: options) {
        return String(decoding: d, as: UTF8.self)
    }
    return nil
}

public func responseError(_ err: Error) -> String {
    if let r = err as? ChatResponse {
        switch r {
        case let .chatCmdError(_, chatError): return chatErrorString(chatError)
        case let .chatError(_, chatError): return chatErrorString(chatError)
        default: return "\(String(describing: r.responseType)), details: \(String(describing: r.details))"
        }
    } else {
        return String(describing: err)
    }
}

func chatErrorString(_ err: ChatError) -> String {
    if case let .invalidJSON(json) = err { return json }
    return String(describing: err)
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

func dbMigrationResult(_ s: String) -> DBMigrationResult {
    let d = s.data(using: .utf8)!
// TODO is there a way to do it without copying the data? e.g:
//    let p = UnsafeMutableRawPointer.init(mutating: UnsafeRawPointer(cjson))
//    let d = Data.init(bytesNoCopy: p, count: strlen(cjson), deallocator: .free)
    do {
        return try jsonDecoder.decode(DBMigrationResult.self, from: d)
    } catch let error {
        logger.error("chatResponse jsonDecoder.decode error: \(error.localizedDescription)")
        return .unknown(json: s)
    }
}
