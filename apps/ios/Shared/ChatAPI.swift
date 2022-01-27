//
//  ChatAPI.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 27/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation

private var chatStore: chat_store?
private var chatController: chat_ctrl?
private let jsonDecoder = JSONDecoder()
private let jsonEncoder = JSONEncoder()

enum ChatCommand {
    case string(String)
    case help
}

enum ChatResponse: Codable {
    case string(String)
//    case chatHelp(String)
//    case newSentInvitation
//    case contactConnected(contact: Contact)
}

func chatGetUser() -> User? {
    let store = getStore()
    print("chatGetUser")
    let r: UserResponse? = decodeCJSON(chat_get_user(store))
    let user = r?.user
    if user != nil { initChatCtrl(store) }
    print("user", user as Any)
    return user
}

func chatCreateUser(_ p: Profile) -> User? {
    let store = getStore()
    print("chatCreateUser")
    var str = encodeCJSON(p)
    chat_create_user(store, &str)
    let user = chatGetUser()
    if user != nil { initChatCtrl(store) }
    print("user", user as Any)
    return user
}

func chatSendCmd(cmd: ChatCommand) -> ChatResponse? {
    var c = commandString(cmd).cString(using: .utf8)!
    return chatResponse(chat_send_cmd(getChatCtrl(), &c)!)
}

func chatRecvMsg() -> ChatResponse? {
    chatResponse(chat_recv_msg(getChatCtrl())!)
}

private struct UserResponse: Decodable {
    var user: User?
    var error: String?
}

private func commandString(_ cmd: ChatCommand) -> String {
    switch cmd {
    case let .string(str):
        return str
    case .help:
        return "/help"
    }
}

private func chatResponse(_ s: UnsafePointer<CChar>) -> ChatResponse? {
    let str = String.init(cString: s)
    print(str)
    return ChatResponse.string(str)
    //    return decodeCJSON(r)
}

private func getStore() -> chat_store {
    if let store = chatStore { return store }
    let dataDir = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.path + "/mobile_v1"
    var cstr = dataDir.cString(using: .utf8)!
    chatStore = chat_init_store(&cstr)
    return chatStore!
}

private func initChatCtrl(_ store: chat_store) {
    if chatController == nil {
        chatController = chat_start(store)        
    }
}

private func getChatCtrl() -> chat_ctrl {
    if let controller = chatController { return controller }
    fatalError("Chat controller was not started!")
}

private func decodeCJSON<T: Decodable>(_ cjson: UnsafePointer<CChar>) -> T? {
    let s = String.init(cString: cjson)
    print("decodeCJSON", s)
    let d = s.data(using: .utf8)!
//    let p = UnsafeMutableRawPointer.init(mutating: UnsafeRawPointer(cjson))
//    let d = Data.init(bytesNoCopy: p, count: strlen(cjson), deallocator: .free)
    return try? jsonDecoder.decode(T.self, from: d)
}

private func encodeCJSON<T: Encodable>(_ value: T) -> [CChar] {
    let data = try! jsonEncoder.encode(value)
    let str = String(decoding: data, as: UTF8.self)
    print("encodeCJSON", str)
    return str.cString(using: .utf8)!
}
