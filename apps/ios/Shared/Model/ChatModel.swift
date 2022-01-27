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
    @Published var chats: [Chat] = []
    @Published var chatItems: [ChatItem] = []
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

struct Profile: Hashable, Equatable, Codable {
    var displayName: String
    var fullName: String
}

enum Chat: Hashable, Equatable {
    case direct(Contact, [ChatItem])
    case group(GroupInfo, [ChatItem])
    
    func label() -> String {
        switch self {
        case let .direct(contact, _):
            return "@" + contact.localDisplayName
        case let .group(groupInfo, _):
            return "#" + groupInfo.localDisplayName
        }
    }
}

struct Contact: Hashable, Equatable, Codable {
    var contactId: Int64
    var localDisplayName: ContactName
    var profile: Profile
    var viaGroup: Int64?
}

struct GroupInfo: Hashable, Equatable, Codable {
    var groupId: Int64
    var localDisplayName: GroupName
    var groupProfile: GroupProfile
}

struct GroupProfile: Hashable, Equatable, Codable {
    var displayName: String
    var fullName: String
}

struct ChatItem: Hashable, Equatable, Codable {
//    var from: GroupMember?
    var ts: Date
    var content: MsgContent
    
    func text() -> String {
        switch content {
        case .text(let str):
            return str
        case .unknown:
            return "unknown"
        }
    }
}

enum MsgContent: Hashable, Equatable, Codable {
    case text(String)
    case unknown
}
