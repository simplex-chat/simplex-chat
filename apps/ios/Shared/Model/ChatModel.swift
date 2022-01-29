//
//  ChatModel.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 22/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import Combine

final class ChatModel: ObservableObject {
    @Published var currentUser: User?
    @Published var chats: Dictionary<String, Chat> = [:]
    @Published var chatPreviews: [ChatPreview] = []
    @Published var chatItems: [ChatItem] = []
    @Published var apiResponses: [APIResponse] = []
}

struct User: Codable {
    var userId: Int64
    var userContactId: Int64
    var localDisplayName: ContactName
    var profile: Profile
    var activeUser: Bool
}

typealias ContactName = String

typealias GroupName = String

struct Profile: Codable {
    var displayName: String
    var fullName: String
}

struct ChatPreview: Identifiable, Codable {
    var chatInfo: ChatInfo
    var lastChatItem: ChatItem?

    var id: String {
        get { chatInfo.id }
    }
}

enum ChatInfo: Identifiable, Codable {
    case direct(contact: Contact)
//    case group()
    
    var displayName: String {
        get {
            switch self {
            case let .direct(contact): return "@\(contact.localDisplayName)"
//            case let .group(groupInfo, _): return "#\(groupInfo.localDisplayName)"
            }
        }
    }
    
    var id: String {
        get {
            switch self {
            case let .direct(contact): return "@\(contact.contactId)"
//            case let .group(contact): return group.id
            }
        }
    }

    var apiType: String {
        get {
            switch self {
            case .direct(_): return "direct"
//            case let .group(_): return "group"
            }
        }
    }
    
    var apiId: Int64 {
        get {
            switch self {
            case let .direct(contact): return contact.contactId
//            case let .group(contact): return group.id
            }
        }
    }
}

class Chat: Codable {
    var chatInfo: ChatInfo
    var chatItems: [ChatItem]
}

struct Contact: Identifiable, Codable {
    var contactId: Int64
    var localDisplayName: ContactName
    var profile: Profile
    var viaGroup: Int64?
    
    var id: String { get { "@\(contactId)" } }
}

struct GroupInfo: Identifiable, Codable {
    var groupId: Int64
    var localDisplayName: GroupName
    var groupProfile: GroupProfile
    
    var id: String { get { "#\(groupId)" } }
}

struct GroupProfile: Codable {
    var displayName: String
    var fullName: String
}

struct GroupMember: Codable {

}

struct ChatItem: Identifiable, Codable {
    var chatDir: CIDirection
    var meta: CIMeta
    var content: CIContent
    
    var id: Int64 { get { meta.itemId } }
}

enum CIDirection: Codable {
    case directSnd
    case directRcv
    case groupSnd
    case groupRcv(GroupMember)
}

struct CIMeta: Codable {
    var itemId: Int64
    var itemTs: Date
    var itemText: String
    var createdAt: Date
}

enum CIContent: Codable {
    case sndMsgContent(msgContent: MsgContent)
    case rcvMsgContent(msgContent: MsgContent)
    // files etc.

    var text: String {
        get {
            switch self {
            case let .sndMsgContent(mc): return mc.string
            case let .rcvMsgContent(mc): return mc.string
            }
        }
    }
}

enum MsgContent: Codable {
    case text(String)
    case unknown(type: String, text: String, json: String)
    case invalid(json: String)

    init(from: Decoder) throws {
        self = .invalid(json: "")
    }
    
    var string: String {
        get {
            switch self {
            case let .text(str): return str
            case .unknown: return "unknown"
            case .invalid:  return "invalid"
            }
        }
    }
}

//func parseMsgContent(_ mc: SomeMsgContent) -> MsgContent {
//    if let type = mc["type"] as? String {
//        let text_ = mc["text"] as? String
//        switch type {
//        case "text":
//            if let text = text_ { return .text(text) }
//        case let t:
//            return .unknown(type: t, text: text_ ?? "unknown item", json: prettyJSON(mc) ?? "error")
//        }
//    }
//    return .invalid(json: prettyJSON(mc) ?? "error")
//}
