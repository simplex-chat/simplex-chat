//
//  ChatUtils.swift
//  SimpleXChat
//
//  Created by Levitating Pineapple on 15/07/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import Foundation

public protocol ChatLike {
    var chatInfo: ChatInfo { get}
    var chatItems: [ChatItem] { get }
    var chatStats: ChatStats { get }
}

public func filterChatsToForwardTo<C: ChatLike>(chats: [C]) -> [C] {
    var filteredChats = chats.filter { c in
        c.chatInfo.chatType != .local && canForwardToChat(c.chatInfo)
    }
    if let privateNotes = chats.first(where: { $0.chatInfo.chatType == .local }) {
        filteredChats.insert(privateNotes, at: 0)
    }
    return filteredChats
}

public func foundChat(_ chat: ChatLike, _ searchStr: String) -> Bool {
    let cInfo = chat.chatInfo
    return switch cInfo {
    case let .direct(contact):
        viewNameContains(cInfo, searchStr) ||
        contact.profile.displayName.localizedLowercase.contains(searchStr) ||
        contact.fullName.localizedLowercase.contains(searchStr)
    default:
        viewNameContains(cInfo, searchStr)
    }

    func viewNameContains(_ cInfo: ChatInfo, _ s: String) -> Bool {
        cInfo.chatViewName.localizedLowercase.contains(s)
    }
}

private func canForwardToChat(_ cInfo: ChatInfo) -> Bool {
    switch cInfo {
    case let .direct(contact): contact.sendMsgEnabled && !contact.nextSendGrpInv
    case let .group(groupInfo): groupInfo.sendMsgEnabled
    case let .local(noteFolder): noteFolder.sendMsgEnabled
    case .contactRequest: false
    case .contactConnection: false
    case .invalidJSON: false
    }
}

