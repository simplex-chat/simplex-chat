//
//  ChatModel.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 22/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import Combine
import SwiftUI

final class ChatModel: ObservableObject {
    @Published var currentUser: User?
    @Published var chats: Dictionary<String, Chat> = [:]
    @Published var chatPreviews: [Chat] = []
    @Published var chatItems: [ChatItem] = []
    @Published var terminalItems: [TerminalItem] = []
    @Published var userAddress: String?
}

class User: Decodable {
    var userId: Int64
    var userContactId: Int64
    var localDisplayName: ContactName
    var profile: Profile
    var activeUser: Bool

    internal init(userId: Int64, userContactId: Int64, localDisplayName: ContactName, profile: Profile, activeUser: Bool) {
        self.userId = userId
        self.userContactId = userContactId
        self.localDisplayName = localDisplayName
        self.profile = profile
        self.activeUser = activeUser
    }
}

let sampleUser = User(
    userId: 1,
    userContactId: 1,
    localDisplayName: "alice",
    profile: sampleProfile,
    activeUser: true
)

typealias ContactName = String

typealias GroupName = String

struct Profile: Decodable {
    var displayName: String
    var fullName: String
}

let sampleProfile = Profile(
    displayName: "alice",
    fullName: "Alice"
)

enum ChatType: String {
    case direct = "@"
    case group = "#"
}

enum ChatInfo: Identifiable, Decodable {
    case direct(contact: Contact)
    case group(groupInfo: GroupInfo)
    
    var localDisplayName: String {
        get {
            switch self {
            case let .direct(contact): return "@\(contact.localDisplayName)"
            case let .group(groupInfo): return "#\(groupInfo.localDisplayName)"
            }
        }
    }
    
    var id: String {
        get {
            switch self {
            case let .direct(contact): return "@\(contact.contactId)"
            case let .group(groupInfo): return "#\(groupInfo.groupId)"
            }
        }
    }

    var chatType: ChatType {
        get {
            switch self {
            case .direct: return .direct
            case .group: return .group
            }
        }
    }
    
    var apiId: Int64 {
        get {
            switch self {
            case let .direct(contact): return contact.contactId
            case let .group(groupInfo): return groupInfo.groupId
            }
        }
    }
}

let sampleDirectChatInfo = ChatInfo.direct(contact: sampleContact)

let sampleGroupChatInfo = ChatInfo.group(groupInfo: sampleGroupInfo)

class Chat: Decodable, Identifiable {
    var chatInfo: ChatInfo
    var chatItems: [ChatItem]

    init(chatInfo: ChatInfo, chatItems: [ChatItem]) {
        self.chatInfo = chatInfo
        self.chatItems = chatItems
    }

    var id: String { get { chatInfo.id } }
}

struct Contact: Identifiable, Decodable {
    var contactId: Int64
    var localDisplayName: ContactName
    var profile: Profile
    var viaGroup: Int64?
    
    var id: String { get { "@\(contactId)" } }
}

let sampleContact = Contact(
    contactId: 1,
    localDisplayName: "alice",
    profile: sampleProfile
)

struct UserContactRequest: Decodable {
    var contactRequestId: Int64
    var localDisplayName: ContactName
    var profile: Profile
}

struct GroupInfo: Identifiable, Decodable {
    var groupId: Int64
    var localDisplayName: GroupName
    var groupProfile: GroupProfile
    
    var id: String { get { "#\(groupId)" } }
}

let sampleGroupInfo = GroupInfo(
    groupId: 1,
    localDisplayName: "team",
    groupProfile: sampleGroupProfile
)

struct GroupProfile: Codable {
    var displayName: String
    var fullName: String
}

let sampleGroupProfile = GroupProfile(
    displayName: "team",
    fullName: "My Team"
)

struct GroupMember: Decodable {

}

struct AChatItem: Decodable {
    var chatInfo: ChatInfo
    var chatItem: ChatItem
}

struct ChatItem: Identifiable, Decodable {
    var chatDir: CIDirection
    var meta: CIMeta
    var content: CIContent
    
    var id: Int64 { get { meta.itemId } }
}

func chatItemSample(_ id: Int64, _ dir: CIDirection, _ ts: Date, _ text: String) -> ChatItem {
    ChatItem(
       chatDir: dir,
       meta: ciMetaSample(id, ts, text),
       content: .sndMsgContent(msgContent: .text(text))
   )
}

enum CIDirection: Decodable {
    case directSnd
    case directRcv
    case groupSnd
    case groupRcv(GroupMember)

    var sent: Bool {
        get {
            switch self {
            case .directSnd: return true
            case .directRcv: return false
            case .groupSnd: return true
            case .groupRcv: return false
            }
        }
    }
}

struct CIMeta: Decodable {
    var itemId: Int64
    var itemTs: Date
    var itemText: String
    var createdAt: Date
}

func ciMetaSample(_ id: Int64, _ ts: Date, _ text: String) -> CIMeta {
    CIMeta(
        itemId: id,
        itemTs: ts,
        itemText: text,
        createdAt: ts
    )
}

enum CIContent: Decodable {
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

enum MsgContent {
    case text(String)
    case unknown(type: String, text: String)
    case invalid(error: String)

    var string: String {
        get {
            switch self {
            case let .text(text): return text
            case let .unknown(_, text): return text
            case .invalid:  return "invalid"
            }
        }
    }

    var cmdString: String {
        get {
            switch self {
            case let .text(text): return "text \(text)"
            default: return ""
            }
        }
    }

    enum CodingKeys: String, CodingKey {
        case type
        case text
    }
}

extension MsgContent: Decodable {
    init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        do {
            let type = try container.decode(String.self, forKey: CodingKeys.type)
            switch type {
            case "text":
                let text = try container.decode(String.self, forKey: CodingKeys.text)
                self = .text(text)
            default:
                let text = try? container.decode(String.self, forKey: CodingKeys.text)
                self = .unknown(type: type, text: text ?? "unknown message format")
            }
        } catch {
            self = .invalid(error: String(describing: error))
        }
    }
}
