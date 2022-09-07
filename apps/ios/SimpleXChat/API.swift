//
//  API.swift
//  SimpleX NSE
//
//  Created by Evgeny on 26/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation

private var chatController: chat_ctrl?

public func getChatCtrl(_ useKey: String? = nil) -> chat_ctrl {
    if let controller = chatController { return controller }
    let dbPath = getAppDatabasePath().path
    let dbKey = useKey ?? getDatabaseKey() ?? ""
    logger.debug("getChatCtrl DB path: \(dbPath)")
    var cPath = dbPath.cString(using: .utf8)!
    var cKey = dbKey.cString(using: .utf8)!
    chatController = chat_init_key(&cPath, &cKey)
    logger.debug("getChatCtrl: chat_init_key")
    return chatController!
}

public func migrateChatDatabase(_ useKey: String? = nil) -> (Bool, DBMigrationResult) {
    logger.debug("migrateChatDatabase \(storeDBPassphraseGroupDefault.get())")
    let dbPath = getAppDatabasePath().path
    var dbKey = ""
    let useKeychain = storeDBPassphraseGroupDefault.get()
    if let key = useKey {
        dbKey = key
    } else if useKeychain {
        if !hasDatabase() {
            dbKey = randomDatabasePassword()
            initialRandomDBPassphraseGroupDefault.set(true)
        } else if let key = getDatabaseKey() {
            dbKey = key
        }
    }
    logger.debug("migrateChatDatabase DB path: \(dbPath)")
//    logger.debug("migrateChatDatabase DB key: \(dbKey)")
    var cPath = dbPath.cString(using: .utf8)!
    var cKey = dbKey.cString(using: .utf8)!
    let cjson = chat_migrate_db(&cPath, &cKey)!
    let res = dbMigrationResult(fromCString(cjson))
    let encrypted = dbKey != ""
    if case .ok = res, useKeychain && encrypted && !setDatabaseKey(dbKey) {
        return (encrypted, .errorKeychain)
    }
    return (encrypted, res)
}

public func resetChatCtrl() {
    chatController = nil
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
