//
//  API.swift
//  SimpleX NSE
//
//  Created by Evgeny on 26/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import OSLog

let logger = Logger()

private var chatController: chat_ctrl?

private func getChatCtrl() -> chat_ctrl {
    if let controller = chatController { return controller }
    let dbFilePrefix = getDocumentsDirectory().path + "/mobile_v1"
    logger.debug("getChatCtrl: dbFilePrefix \(dbFilePrefix)")
    var cstr = dbFilePrefix.cString(using: .utf8)!
    chatController = chat_init(&cstr)
    logger.debug("getChatCtrl: chat_init")
    return chatController!
}

func getDocumentsDirectory() -> URL {
    FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!
//    FileManager.default.containerURL(forSecurityApplicationGroupIdentifier: APP_GROUP_NAME)!
}

//public func sendSimpleXCmd(_ cmd: ChatCommand) -> ChatResponse {
//    var c = cmd.cmdString.cString(using: .utf8)!
//    return chatResponse(chat_send_cmd(getChatCtrl(), &c))
//}

public func chatSendCmd(_ cmd: String) -> String {
    var c = cmd.cString(using: .utf8)!
    return rawChatResponse(chat_send_cmd(getChatCtrl(), &c))
}

public func chatRecvMsg() -> String {
    rawChatResponse(chat_recv_msg(getChatCtrl()))
}

public func rawChatResponse(_ cjson: UnsafeMutablePointer<CChar>) -> String {
    let s = String.init(cString: cjson)
    free(cjson)
    return s
}

//public func chatResponse(_ cjson: UnsafeMutablePointer<CChar>) -> ChatResponse {
//    let s = String.init(cString: cjson)
//    let d = s.data(using: .utf8)!
//// TODO is there a way to do it without copying the data? e.g:
////    let p = UnsafeMutableRawPointer.init(mutating: UnsafeRawPointer(cjson))
////    let d = Data.init(bytesNoCopy: p, count: strlen(cjson), deallocator: .free)
//    do {
//        let r = try jsonDecoder.decode(APIResponse.self, from: d)
//        return r.resp
//    } catch {
//        logger.error("chatResponse jsonDecoder.decode error: \(error.localizedDescription)")
//    }
//
//    var type: String?
//    var json: String?
//    if let j = try? JSONSerialization.jsonObject(with: d) as? NSDictionary {
//        if let j1 = j["resp"] as? NSDictionary, j1.count == 1 {
//            type = j1.allKeys[0] as? String
//        }
//        json = prettyJSON(j)
//    }
//    free(cjson)
//    return ChatResponse.response(type: type ?? "invalid", json: json ?? s)
//}
//
//public func responseError(_ err: Error) -> String {
//    if let r = err as? ChatResponse {
//        return String(describing: r)
//    } else {
//        return err.localizedDescription
//    }
//}
