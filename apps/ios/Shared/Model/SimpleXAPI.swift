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
    case apiGetChats
    case apiGetChatItems(type: String, id: Int64)
    case string(String)
    case help

    var cmdString: String {
        get {
            switch self {
            case .apiGetChats:
                return "/api/v1/chats"
            case let .apiGetChatItems(type, id):
                return "/api/v1/chat/items/\(type)/\(id)"
            case let .string(str):
                return str
            case .help: return "/help"
            }
        }
    }
}

struct APIResponse: Identifiable {
    var resp: ChatResponse
    var id: Int64
}

struct APIResponseJSON: Decodable {
    var resp: ChatResponse
}

enum ChatResponse: Codable {
    case response(type: String, json: String)
    case apiChats(chats: [ChatPreview])
    case apiDirectChat(chat: Chat) // direct/<id> or group/<id>, same as ChatPreview.id
//    case chatHelp(String)
//    case newSentInvitation
    case contactConnected(contact: Contact)

    var responseType: String {
        get {
            switch self {
            case let .response(type, _): return "* \(type)"
            case .apiChats(_): return "apiChats"
            case .apiDirectChat(_): return "apiDirectChat"
            case .contactConnected(_): return "contactConnected"
            }
        }
    }
    
    var details: String {
        get {
            switch self {
            case let .response(_, json): return json
            case let .apiChats(chats): return String(describing: chats)
            case let .apiDirectChat(chat): return String(describing: chat)
            case let .contactConnected(contact): return String(describing: contact)
            }
        }
    }
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

func chatSendCmd(_ chatModel: ChatModel, _ cmd: ChatCommand) {
    var c = cmd.cmdString.cString(using: .utf8)!
    processAPIResponse(chatModel,
        apiResponse(
            chat_send_cmd(getChatCtrl(), &c)!))
}

func chatRecvMsg(_ chatModel: ChatModel) {
    processAPIResponse(chatModel,
        apiResponse(
            chat_recv_msg(getChatCtrl())!))
}

private func processAPIResponse(_ chatModel: ChatModel, _ res: APIResponse?) {
    if let r = res {
        DispatchQueue.main.async {
            chatModel.apiResponses.append(r)
            switch r.resp {
            case let .apiChats(chats):
                chatModel.chatPreviews = chats
            case let .apiDirectChat(chat):
                chatModel.chats[chat.chatInfo.id] = chat
            case let .contactConnected(contact):
                chatModel.chatPreviews.insert(
                    ChatPreview(chatInfo: .direct(contact: contact)),
                    at: 0
                )
            default: return

//            case let .response(type, _):
//                chatModel.chatItems.append(ChatItem(
//                    ts: Date.now,
//                    content: .text(type)
//                ))
            }
        }
    }
}

private struct UserResponse: Decodable {
    var user: User?
    var error: String?
}

private var respId: Int64 = 0

private func apiResponse(_ cjson: UnsafePointer<CChar>) -> APIResponse? {
    let s = String.init(cString: cjson)
    print("apiResponse", s)
    let d = s.data(using: .utf8)!
//  TODO is there a way to do it without copying the data? e.g:
//    let p = UnsafeMutableRawPointer.init(mutating: UnsafeRawPointer(cjson))
//    let d = Data.init(bytesNoCopy: p, count: strlen(cjson), deallocator: .free)

    do {
        let r = try jsonDecoder.decode(APIResponseJSON.self, from: d)
        return APIResponse(resp: r.resp, id: respId)
    } catch {
        print (error)
    }
        
    var type: String?
    var json: String?
    if let j = try? JSONSerialization.jsonObject(with: d) as? NSDictionary {
        if let j1 = j["resp"] as? NSDictionary, j1.count == 1 {
            type = j1.allKeys[0] as? String
        }
        json = prettyJSON(j)
    }
    respId += 1
    return APIResponse(
        resp: ChatResponse.response(type: type ?? "invalid", json: json ?? s),
        id: respId
    )
}

func prettyJSON(_ obj: NSDictionary) -> String? {
    if let d = try? JSONSerialization.data(withJSONObject: obj, options: .prettyPrinted) {
        return String(decoding: d, as: UTF8.self)
    }
    return nil
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

private func getJSONObject(_ cjson: UnsafePointer<CChar>) -> NSDictionary? {
    let s = String.init(cString: cjson)
    let d = s.data(using: .utf8)!
    return try? JSONSerialization.jsonObject(with: d) as? NSDictionary
}

private func encodeCJSON<T: Encodable>(_ value: T) -> [CChar] {
    let data = try! jsonEncoder.encode(value)
    let str = String(decoding: data, as: UTF8.self)
    print("encodeCJSON", str)
    return str.cString(using: .utf8)!
}
