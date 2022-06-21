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

public func sendSimpleXCmd(_ cmd: ChatCommand) -> ChatResponse {
    var c = cmd.cmdString.cString(using: .utf8)!
    return chatResponse(chat_send_cmd(getChatCtrl(), &c))
}

public func chatResponse(_ cjson: UnsafeMutablePointer<CChar>) -> ChatResponse {
    let s = String.init(cString: cjson)
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
    free(cjson)
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
