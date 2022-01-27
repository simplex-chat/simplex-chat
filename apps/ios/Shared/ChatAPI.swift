//
//  ChatStore.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 27/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation

private var chatStore: chat_store?

private var chatController: chat_ctrl?

private func getStore() -> chat_store {
    if let store = chatStore { return store }
    let dataDir = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.path + "/mobile_v1"
    var cstr = dataDir.cString(using: .utf8)!
    chatStore = chat_init_store(&cstr)
    return chatStore!
}

func chatGetUser() -> User? {
    let store = getStore()
    let user: User? = decodeCJSON(chat_get_user(store))
    if user != nil { chatController = chat_start(store) }
    return user
}

private func getChatCtrl() -> chat_ctrl {
    if let controller = chatController { return controller }
    fatalError("Chat controller was not started!")
}

func chatSendCmd(cmd: ChatCommand) async -> ChatResponse? {
    var c = commandString(cmd).cString(using: .utf8)!
    let r = chat_send_cmd(getChatCtrl(), &c)!
    print(String.init(cString: r))
    return decodeCJSON(r)
}

private let jsonDecoder = JSONDecoder()

func decodeCJSON<T: Decodable>(_ cjson: UnsafePointer<CChar>) -> T? {
    let p = UnsafeMutableRawPointer.init(mutating: UnsafeRawPointer(cjson))
    let d = Data.init(bytesNoCopy: p, count: strlen(cjson), deallocator: .free)
    return try? jsonDecoder.decode(T.self, from: d)
}

enum ChatCommand {
    case string(String)
    case help
}

private func commandString(_ cmd: ChatCommand) -> String {
    switch cmd {
    case let .string(str):
        return str
    case .help:
        return "/help"
    }
}

enum ChatResponse: Codable {
    case chatHelp(String)
    case newSentInvitation
    case contactConnected(contact: Contact)
}
