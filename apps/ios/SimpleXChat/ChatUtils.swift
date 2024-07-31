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

extension ChatLike {
    public func groupFeatureEnabled(_ feature: GroupFeature) -> Bool {
        if case let .group(groupInfo) = chatInfo {
            let p = groupInfo.fullGroupPreferences
            return switch feature {
            case .timedMessages: p.timedMessages.on
            case .directMessages: p.directMessages.on(for: groupInfo.membership)
            case .fullDelete: p.fullDelete.on
            case .reactions: p.reactions.on
            case .voice: p.voice.on(for: groupInfo.membership)
            case .files: p.files.on(for: groupInfo.membership)
            case .simplexLinks: p.simplexLinks.on(for: groupInfo.membership)
            case .history: p.history.on
            }
        } else {
            return true
        }
    }

    public func prohibitedByPref(
        hasSimplexLink: Bool,
        isMediaOrFileAttachment: Bool,
        isVoice: Bool
    ) -> Bool {
        // preference checks should match checks in compose view
        let simplexLinkProhibited = hasSimplexLink && !groupFeatureEnabled(.simplexLinks)
        let fileProhibited = isMediaOrFileAttachment && !groupFeatureEnabled(.files)
        let voiceProhibited = isVoice && !chatInfo.featureEnabled(.voice)
        return switch chatInfo {
        case .direct: voiceProhibited
        case .group: simplexLinkProhibited || fileProhibited || voiceProhibited
        case .local: false
        case .contactRequest: false
        case .contactConnection: false
        case .invalidJSON: false
        }
    }
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

public func chatIconName(_ cInfo: ChatInfo) -> String {
    switch cInfo {
    case .direct: "person.crop.circle.fill"
    case .group: "person.2.circle.fill"
    case .local: "folder.circle.fill"
    case .contactRequest: "person.crop.circle.fill"
    default:  "circle.fill"
    }
}

public func hasSimplexLink(_ text: String?) -> Bool {
    if let text, let parsedMsg = parseSimpleXMarkdown(text) {
        parsedMsgHasSimplexLink(parsedMsg)
    } else {
        false
    }
}

public func parsedMsgHasSimplexLink(_ parsedMsg: [FormattedText]) -> Bool {
    parsedMsg.contains(where: { ft in ft.format?.isSimplexLink ?? false })
}
