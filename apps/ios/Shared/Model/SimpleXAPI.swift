//
//  ChatAPI.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 27/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import UIKit

private var chatStore: chat_store?
private var chatController: chat_ctrl?
private let jsonDecoder = getJSONDecoder()
private let jsonEncoder = getJSONEncoder()

enum ChatCommand {
    case apiGetChats
    case apiGetChat(type: ChatType, id: Int64)
    case apiSendMessage(type: ChatType, id: Int64, msg: MsgContent)
    case addContact
    case connect(connReq: String)
    case apiDeleteChat(type: ChatType, id: Int64)
    case apiUpdateProfile(profile: Profile)
    case createMyAddress
    case deleteMyAddress
    case showMyAddress
    case string(String)

    var cmdString: String {
        get {
            switch self {
            case .apiGetChats:
                return "/get chats"
            case let .apiGetChat(type, id):
                return "/get chat \(type.rawValue)\(id)"
            case let .apiSendMessage(type, id, mc):
                return "/send msg \(type.rawValue)\(id) \(mc.cmdString)"
            case .addContact:
                return "/c"
            case let .connect(connReq):
                return "/c \(connReq)"
            case let .apiDeleteChat(type, id):
                return "/_del \(type.rawValue)\(id)"
            case let .apiUpdateProfile(profile):
                return "/p \(profile.displayName) \(profile.fullName)"
            case .createMyAddress:
                return "/ad"
            case .deleteMyAddress:
                return "/da"
            case .showMyAddress:
                return "/sa"
            case let .string(str):
                return str
            }
        }
    }
}

struct APIResponse: Decodable {
    var resp: ChatResponse
}

enum ChatResponse: Decodable, Error {
    case response(type: String, json: String)
    case apiChats(chats: [Chat])
    case apiChat(chat: Chat)
    case invitation(connReqInvitation: String)
    case sentConfirmation
    case sentInvitation
    case contactDeleted(contact: Contact)
    case userProfileNoChange
    case userProfileUpdated(fromProfile: Profile, toProfile: Profile)
    case userContactLink(connReqContact: String)
    case userContactLinkCreated(connReqContact: String)
    case userContactLinkDeleted
    case contactConnected(contact: Contact)
    case receivedContactRequest(contactRequest: UserContactRequest)
    case acceptingContactRequest(contactRequest: UserContactRequest)
    case contactRequestRejected(contactRequest: UserContactRequest)
    case newChatItem(chatItem: AChatItem)
    case chatCmdError(chatError: ChatError)

    var responseType: String {
        get {
            switch self {
            case let .response(type, _): return "* \(type)"
            case .apiChats: return "apiChats"
            case .apiChat: return "apiChat"
            case .invitation: return "invitation"
            case .sentConfirmation: return "sentConfirmation"
            case .sentInvitation: return "sentInvitation"
            case .contactDeleted: return "contactDeleted"
            case .userProfileNoChange: return "userProfileNoChange"
            case .userProfileUpdated: return "userProfileNoChange"
            case .userContactLink: return "userContactLink"
            case .userContactLinkCreated: return "userContactLinkCreated"
            case .userContactLinkDeleted: return "userContactLinkDeleted"
            case .contactConnected: return "contactConnected"
            case .receivedContactRequest: return "receivedContactRequest"
            case .acceptingContactRequest: return "acceptingContactRequest"
            case .contactRequestRejected: return "contactRequestRejected"
            case .newChatItem: return "newChatItem"
            case .chatCmdError: return "chatCmdError"
            }
        }
    }
    
    var details: String {
        get {
            switch self {
            case let .response(_, json): return json
            case let .apiChats(chats): return String(describing: chats)
            case let .apiChat(chat): return String(describing: chat)
            case let .invitation(connReqInvitation): return connReqInvitation
            case .sentConfirmation: return noDetails
            case .sentInvitation: return noDetails
            case let .contactDeleted(contact): return String(describing: contact)
            case .userProfileNoChange: return noDetails
            case let .userProfileUpdated(_, toProfile): return String(describing: toProfile)
            case let .userContactLink(connReq): return connReq
            case let .userContactLinkCreated(connReq): return connReq
            case .userContactLinkDeleted: return noDetails
            case let .contactConnected(contact): return String(describing: contact)
            case let .receivedContactRequest(contactRequest): return String(describing: contactRequest)
            case let .acceptingContactRequest(contactRequest): return String(describing: contactRequest)
            case let .contactRequestRejected(contactRequest): return String(describing: contactRequest)
            case let .newChatItem(chatItem): return String(describing: chatItem)
            case let .chatCmdError(chatError): return String(describing: chatError)
            }
        }
    }

    private var noDetails: String { get { "\(responseType): no details" } }
}

enum TerminalItem: Identifiable {
    case cmd(Date, ChatCommand)
    case resp(Date, ChatResponse)

    var id: Date {
        get {
            switch self {
            case let .cmd(id, _): return id
            case let .resp(id, _): return id
            }
        }
    }

    var label: String {
        get {
            switch self {
            case let .cmd(_, cmd): return "> \(cmd.cmdString.prefix(30))"
            case let .resp(_, resp): return "< \(resp.responseType)"
            }
        }
    }

    var details: String {
        get {
            switch self {
            case let .cmd(_, cmd): return cmd.cmdString
            case let .resp(_, resp): return resp.details
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

func chatSendCmd(_ cmd: ChatCommand) throws -> ChatResponse {
    var c = cmd.cmdString.cString(using: .utf8)!
    print("command", cmd.cmdString)
// TODO some mechanism to update model without passing it - maybe Publisher / Subscriber?
//    DispatchQueue.main.async {
//        termId += 1
//        chatModel.terminalItems.append(.cmd(termId, cmd))
//    }
    return chatResponse(chat_send_cmd(getChatCtrl(), &c)!)
}

func chatRecvMsg() throws -> ChatResponse {
    chatResponse(chat_recv_msg(getChatCtrl())!)
}

func apiGetChats() throws -> [Chat] {
    let r = try chatSendCmd(.apiGetChats)
    if case let .apiChats(chats) = r { return chats }
    throw r
}

func apiGetChat(type: ChatType, id: Int64) throws -> Chat {
    let r = try chatSendCmd(.apiGetChat(type: type, id: id))
    if case let .apiChat(chat) = r { return chat }
    throw r
}

func apiSendMessage(type: ChatType, id: Int64, msg: MsgContent) throws -> ChatItem {
    let r = try chatSendCmd(.apiSendMessage(type: type, id: id, msg: msg))
    if case let .newChatItem(aChatItem) = r { return aChatItem.chatItem }
    throw r
}

func apiAddContact() throws -> String {
    let r = try chatSendCmd(.addContact)
    if case let .invitation(connReqInvitation) = r { return connReqInvitation }
    throw r
}

func apiConnect(connReq: String) throws {
    let r = try chatSendCmd(.connect(connReq: connReq))
    switch r {
    case .sentConfirmation: return
    case .sentInvitation: return
    default: throw r
    }
}

func apiDeleteChat(type: ChatType, id: Int64) throws {
    let r = try chatSendCmd(.apiDeleteChat(type: type, id: id))
    if case .contactDeleted = r { return }
    throw r
}

func apiUpdateProfile(profile: Profile) throws -> Profile? {
    let r = try chatSendCmd(.apiUpdateProfile(profile: profile))
    switch r {
    case .userProfileNoChange: return nil
    case let .userProfileUpdated(_, toProfile): return toProfile
    default: throw r
    }
}

func apiCreateUserAddress() throws -> String {
    let r = try chatSendCmd(.createMyAddress)
    if case let .userContactLinkCreated(connReq) = r { return connReq }
    throw r
}

func apiDeleteUserAddress() throws {
    let r = try chatSendCmd(.deleteMyAddress)
    if case .userContactLinkDeleted = r { return }
    throw r
}

func apiGetUserAddress() throws -> String? {
    let r = try chatSendCmd(.showMyAddress)
    switch r {
    case let .userContactLink(connReq):
        return connReq
    case .chatCmdError(chatError: .errorStore(storeError: .userContactLinkNotFound)):
        return nil
    default: throw r
    }
}


func processReceivedMsg(_ chatModel: ChatModel, _ res: ChatResponse) {
    DispatchQueue.main.async {
        chatModel.terminalItems.append(.resp(Date.now, res))
        switch res {
        case let .contactConnected(contact):
            chatModel.chatPreviews.insert(
                Chat(chatInfo: .direct(contact: contact), chatItems: []),
                at: 0
            )
        case let .receivedContactRequest(contactRequest):
            return // TODO
        case let .newChatItem(aChatItem):
            let ci = aChatItem.chatInfo
            let chat = chatModel.chats[ci.id] ?? Chat(chatInfo: ci, chatItems: [])
            chatModel.chats[ci.id] = chat
            chat.chatItems.append(aChatItem.chatItem)
        default:
            print("unsupported response: ", res.responseType)
        }
    }
}

private struct UserResponse: Decodable {
    var user: User?
    var error: String?
}

private func chatResponse(_ cjson: UnsafePointer<CChar>) -> ChatResponse {
    let s = String.init(cString: cjson)
    let d = s.data(using: .utf8)!
// TODO is there a way to do it without copying the data? e.g:
//    let p = UnsafeMutableRawPointer.init(mutating: UnsafeRawPointer(cjson))
//    let d = Data.init(bytesNoCopy: p, count: strlen(cjson), deallocator: .free)

// TODO some mechanism to update model without passing it - maybe Publisher / Subscriber?

    do {
        let r = try jsonDecoder.decode(APIResponse.self, from: d)
        return r.resp
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
    return ChatResponse.response(type: type ?? "invalid", json: json ?? s)
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
    return str.cString(using: .utf8)!
}

enum ChatError: Decodable {
    case errorStore(storeError: StoreError)
    // TODO other error cases
}

enum StoreError: Decodable {
    case userContactLinkNotFound
    // TODO other error cases
}
