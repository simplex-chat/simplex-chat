//
//  API.swift
//  SimpleX NSE
//
//  Created by Evgeny on 26/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation

private var chatController: chat_ctrl?

public func getChatCtrl() -> chat_ctrl {
    if let controller = chatController { return controller }
    let dbPath = getAppDatabasePath().path
    logger.debug("getChatCtrl DB path: \(dbPath)")
    var cstr = dbPath.cString(using: .utf8)!
    chatController = chat_init(&cstr)
    logger.debug("getChatCtrl: chat_init")
    return chatController!
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
