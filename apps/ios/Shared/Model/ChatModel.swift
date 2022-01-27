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
    @Published var userChats: [Chat] = []
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

enum Chat {
    case direct(Contact, [ChatMessage])
    case group(GroupInfo, [ChatMessage])
}

struct Contact: Codable {
    var contactId: Int64
    var localDisplayName: ContactName
    var profile: Profile
    var viaGroup: Int64?
}

struct GroupInfo: Codable {
    var groupId: Int64
    var localDisplayName: GroupName
    var groupProfile: GroupProfile
}

struct GroupProfile: Codable {
    var displayName: String
    var fullName: String
}

struct ChatMessage {
//    var from: GroupMember?
    var ts: Date
    var content: MsgContent
}

enum MsgContent {
    case text(String)
    case unknown
}
