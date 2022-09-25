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

public func getChatCtrl(_ useKey: String? = nil) -> chat_ctrl {
    if let controller = chatController { return controller }
    fatalError("chat controller not initialized")
}

public func chatMigrateInit(_ useKey: String? = nil) -> (Bool, DBMigrationResult) {
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
        } else if let key = getDatabaseKey() {
            dbKey = key
        }
    }
    logger.debug("chatMigrateInit DB path: \(dbPath)")
//    logger.debug("chatMigrateInit DB key: \(dbKey)")
    var cPath = dbPath.cString(using: .utf8)!
    var cKey = dbKey.cString(using: .utf8)!
    // the last parameter of chat_migrate_init is used to return the pointer to chat controller
    let cjson = chat_migrate_init(&cPath, &cKey, &chatController)!
    let dbRes = dbMigrationResult(fromCString(cjson))
    let encrypted = dbKey != ""
    let keychainErr = dbRes == .ok && useKeychain && encrypted && !setDatabaseKey(dbKey)
    let result = (encrypted, keychainErr ? .errorKeychain : dbRes)
    migrationResult = result
    return result
}

public func resetChatCtrl() {
    chatController = nil
    migrationResult = nil
}

public func sendSimpleXCmd(_ cmd: ChatCommand) -> ChatResponse {
    var c = cmd.cmdString.cString(using: .utf8)!
    let cjson  = chat_send_cmd(getChatCtrl(), &c)!
    return chatResponse(fromCString(cjson))
}

// in microseconds
let MESSAGE_TIMEOUT: Int32 = 15_000_000

public func recvSimpleXMsg() -> ChatResponse? {
    if let cjson = chat_recv_msg_wait(getChatCtrl(), MESSAGE_TIMEOUT) {
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

struct ParsedMarkdown: Decodable {
    var formattedText: [FormattedText]?
}

private func fromCString(_ c: UnsafeMutablePointer<CChar>) -> String {
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
        if let j1 = j["resp"] as? NSDictionary, j1.count == 1 {
            type = j1.allKeys[0] as? String
        }
        json = prettyJSON(j)
    }
    return ChatResponse.response(type: type ?? "invalid", json: json ?? s)
}

func prettyJSON(_ obj: NSDictionary) -> String? {
    if let d = try? JSONSerialization.data(withJSONObject: obj, options: .prettyPrinted) {
        return String(decoding: d, as: UTF8.self)
    }
    return nil
}

public func responseError(_ err: Error) -> String {
    if let r = err as? ChatResponse {
        return String(describing: r)
    } else {
        return err.localizedDescription
    }
}

public enum DBMigrationResult: Decodable, Equatable {
    case ok
    case errorNotADatabase(dbFile: String)
    case error(dbFile: String, migrationError: String)
    case errorKeychain
    case unknown(json: String)
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
