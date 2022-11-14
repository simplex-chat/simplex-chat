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
import WebKit
import SimpleXChat

final class ChatModel: ObservableObject {
    @Published var onboardingStage: OnboardingStage?
    @Published var v3DBMigration: V3DBMigrationState = v3DBMigrationDefault.get()
    @Published var currentUser: User?
    @Published var chatInitialized = false
    @Published var chatRunning: Bool?
    @Published var chatDbChanged = false
    @Published var chatDbEncrypted: Bool?
    @Published var chatDbStatus: DBMigrationResult?
    // list of chat "previews"
    @Published var chats: [Chat] = []
    // current chat
    @Published var chatId: String?
    @Published var reversedChatItems: [ChatItem] = []
    @Published var chatToTop: String?
    @Published var groupMembers: [GroupMember] = []
    // items in the terminal view
    @Published var terminalItems: [TerminalItem] = []
    @Published var userAddress: UserContactLink?
    @Published var userSMPServers: [String]?
    @Published var chatItemTTL: ChatItemTTL = .none
    @Published var appOpenUrl: URL?
    @Published var deviceToken: DeviceToken?
    @Published var savedToken: DeviceToken?
    @Published var tokenRegistered = false
    @Published var tokenStatus: NtfTknStatus?
    @Published var notificationMode = NotificationsMode.off
    @Published var notificationPreview: NotificationPreviewMode? = ntfPreviewModeGroupDefault.get()
    @Published var incognito: Bool = incognitoGroupDefault.get()
    // pending notification actions
    @Published var ntfContactRequest: ChatId?
    @Published var ntfCallInvitationAction: (ChatId, NtfCallAction)?
    // current WebRTC call
    @Published var callInvitations: Dictionary<ChatId, RcvCallInvitation> = [:]
    @Published var activeCall: Call?
    @Published var callCommand: WCallCommand?
    @Published var showCallView = false
    // currently showing QR code
    @Published var connReqInv: String?
    var callWebView: WKWebView?

    var messageDelivery: Dictionary<Int64, () -> Void> = [:]

    static let shared = ChatModel()

    static var ok: Bool { ChatModel.shared.chatDbStatus == .ok }

    func hasChat(_ id: String) -> Bool {
        chats.first(where: { $0.id == id }) != nil
    }

    func getChat(_ id: String) -> Chat? {
        chats.first(where: { $0.id == id })
    }

    func getContactChat(_ contactId: Int64) -> Chat? {
        chats.first { chat in
            if case let .direct(contact) = chat.chatInfo {
                return contact.contactId == contactId
            } else {
                return false
            }
        }
    }

    private func getChatIndex(_ id: String) -> Int? {
        chats.firstIndex(where: { $0.id == id })
    }

    func addChat(_ chat: Chat, at position: Int = 0) {
        withAnimation {
            chats.insert(chat, at: position)
        }
    }

    func updateChatInfo(_ cInfo: ChatInfo) {
        if let i = getChatIndex(cInfo.id) {
            chats[i].chatInfo = cInfo
        }
    }

    func updateContactConnection(_ contactConnection: PendingContactConnection) {
        updateChat(.contactConnection(contactConnection: contactConnection))
    }

    func updateContact(_ contact: Contact) {
        updateChat(.direct(contact: contact), addMissing: !contact.isIndirectContact && !contact.viaGroupLink)
    }

    func updateGroup(_ groupInfo: GroupInfo) {
        updateChat(.group(groupInfo: groupInfo))
    }

    private func updateChat(_ cInfo: ChatInfo, addMissing: Bool = true) {
        if hasChat(cInfo.id) {
            updateChatInfo(cInfo)
        } else if addMissing {
            addChat(Chat(chatInfo: cInfo, chatItems: []))
        }
    }

    private func _updateChat(_ id: ChatId, _ update: @escaping (Chat) -> Void) {
        if let i = getChatIndex(id) {
            // we need to separately update the chat object, as it is ObservedObject,
            // and chat in the list so the list view is updated...
            // simply updating chats[i] replaces the object without updating the current object in the list
            let chat = chats[i]
            update(chat)
            chats[i] = chat
        }
    }

    func updateNetworkStatus(_ id: ChatId, _ status: Chat.NetworkStatus) {
        if let i = getChatIndex(id) {
            chats[i].serverInfo.networkStatus = status
        }
    }

    func replaceChat(_ id: String, _ chat: Chat) {
        if let i = getChatIndex(id) {
            let serverInfo = chats[i].serverInfo
            chats[i] = chat
            chats[i].serverInfo = serverInfo
        } else {
            // invalid state, correcting
            chats.insert(chat, at: 0)
        }
    }

    func updateChats(with newChats: [ChatData]) {
        for i in 0..<newChats.count {
            let c = newChats[i]
            if let j = getChatIndex(c.id)   {
                let chat = chats[j]
                chat.chatInfo = c.chatInfo
                chat.chatItems = c.chatItems
                chat.chatStats = c.chatStats
                if i != j {
                    if chatId != c.chatInfo.id  {
                        popChat_(j, to: i)
                    }  else if i == 0 {
                        chatToTop = c.chatInfo.id
                    }
                }
            } else {
                addChat(Chat(c), at: i)
            }
        }
        NtfManager.shared.setNtfBadgeCount(totalUnreadCount())
    }

//    func addGroup(_ group: SimpleXChat.Group) {
//        groups[group.groupInfo.id] = group
//    }

    func addChatItem(_ cInfo: ChatInfo, _ cItem: ChatItem) {
        // update previews
        if let i = getChatIndex(cInfo.id) {
            chats[i].chatItems = [cItem]
            if case .rcvNew = cItem.meta.itemStatus {
                chats[i].chatStats.unreadCount = chats[i].chatStats.unreadCount + 1
                NtfManager.shared.incNtfBadgeCount()
            }
            if i > 0 {
                if chatId == nil {
                    withAnimation { popChat_(i) }
                } else if chatId == cInfo.id  {
                    chatToTop = cInfo.id
                } else {
                    popChat_(i)
                }
            }
        } else {
            addChat(Chat(chatInfo: cInfo, chatItems: [cItem]))
        }
        // add to current chat
        if chatId == cInfo.id {
            _ = _upsertChatItem(cInfo, cItem)
        }
    }

    func upsertChatItem(_ cInfo: ChatInfo, _ cItem: ChatItem) -> Bool {
        // update previews
        var res: Bool
        if let chat = getChat(cInfo.id) {
            if let pItem = chat.chatItems.last {
                if pItem.id == cItem.id || (chatId == cInfo.id && reversedChatItems.first(where: { $0.id == cItem.id }) == nil) {
                    chat.chatItems = [cItem]
                }
            } else {
                chat.chatItems = [cItem]
            }
            res = false
        } else {
            addChat(Chat(chatInfo: cInfo, chatItems: [cItem]))
            res = true
        }
        // update current chat
        return chatId == cInfo.id ? _upsertChatItem(cInfo, cItem) : res
    }

    private func _upsertChatItem(_ cInfo: ChatInfo, _ cItem: ChatItem) -> Bool {
        if let i = reversedChatItems.firstIndex(where: { $0.id == cItem.id }) {
            let ci = reversedChatItems[i]
            withAnimation(.default) {
                self.reversedChatItems[i] = cItem
                self.reversedChatItems[i].viewTimestamp = .now
                if case .sndNew = cItem.meta.itemStatus {
                    self.reversedChatItems[i].meta = ci.meta
                }
            }
            return false
        } else {
            withAnimation { reversedChatItems.insert(cItem, at: 0) }
            return true
        }
    }
    
    func removeChatItem(_ cInfo: ChatInfo, _ cItem: ChatItem) {
        // update previews
        if let chat = getChat(cInfo.id) {
            if let pItem = chat.chatItems.last, pItem.id == cItem.id {
                chat.chatItems = [cItem]
            }
        }
        // remove from current chat
        if chatId == cInfo.id {
            if let i = reversedChatItems.firstIndex(where: { $0.id == cItem.id }) {
                if reversedChatItems[i].isRcvNew() == true {
                    NtfManager.shared.decNtfBadgeCount()
                }
                _ = withAnimation {
                    self.reversedChatItems.remove(at: i)
                }
            }
        }
    }

    func nextChatItemData<T>(_ chatItemId: Int64, previous: Bool, map: @escaping (ChatItem) -> T?) -> T? {
        guard var i = reversedChatItems.firstIndex(where: { $0.id == chatItemId }) else { return nil }
        if previous {
            while i < reversedChatItems.count - 1 {
                i += 1
                if let res = map(reversedChatItems[i]) { return res }
            }
        } else {
            while i > 0 {
                i -= 1
                if let res = map(reversedChatItems[i]) { return res }
            }
        }
        return nil
    }

    func markChatItemsRead(_ cInfo: ChatInfo) {
        // update preview
        _updateChat(cInfo.id) { chat in
            NtfManager.shared.decNtfBadgeCount(by: chat.chatStats.unreadCount)
            chat.chatStats = ChatStats()
        }
        // update current chat
        if chatId == cInfo.id {
            markCurrentChatRead()
        }
    }

    private func markCurrentChatRead(fromIndex i: Int = 0) {
        var j = i
        while j < reversedChatItems.count {
            if case .rcvNew = reversedChatItems[j].meta.itemStatus {
                reversedChatItems[j].meta.itemStatus = .rcvRead
                reversedChatItems[j].viewTimestamp = .now
            }
            j += 1
        }
    }

    func markChatItemsRead(_ cInfo: ChatInfo, aboveItem: ChatItem? = nil) {
        if let cItem = aboveItem {
            if chatId == cInfo.id, let i = reversedChatItems.firstIndex(where: { $0.id == cItem.id }) {
                markCurrentChatRead(fromIndex: i)
                _updateChat(cInfo.id) { chat in
                    var unreadBelow = 0
                    var j = i - 1
                    while j >= 0 {
                        if case .rcvNew = self.reversedChatItems[j].meta.itemStatus {
                            unreadBelow += 1
                        }
                        j -= 1
                    }
                    // update preview
                    let markedCount = chat.chatStats.unreadCount - unreadBelow
                    if markedCount > 0 {
                        NtfManager.shared.decNtfBadgeCount(by: markedCount)
                        chat.chatStats.unreadCount -= markedCount
                    }
                }
            }
        } else {
            markChatItemsRead(cInfo)
        }
    }
   
    func markChatUnread(_ cInfo: ChatInfo, unreadChat: Bool = true) {
        _updateChat(cInfo.id) { chat in
            chat.chatStats.unreadChat = unreadChat
        }
    }

    func clearChat(_ cInfo: ChatInfo) {
        // clear preview
        if let chat = getChat(cInfo.id) {
            NtfManager.shared.decNtfBadgeCount(by: chat.chatStats.unreadCount)
            chat.chatItems = []
            chat.chatStats = ChatStats()
            chat.chatInfo = cInfo
        }
        // clear current chat
        if chatId == cInfo.id {
            reversedChatItems = []
        }
    }

    func markChatItemRead(_ cInfo: ChatInfo, _ cItem: ChatItem) {
        // update preview
        if let i = getChatIndex(cInfo.id) {
            chats[i].chatStats.unreadCount = chats[i].chatStats.unreadCount - 1
        }
        // update current chat
        if chatId == cInfo.id, let j = reversedChatItems.firstIndex(where: { $0.id == cItem.id }) {
            reversedChatItems[j].meta.itemStatus = .rcvRead
            reversedChatItems[j].viewTimestamp = .now
        }
    }

    func totalUnreadCount() -> Int {
        chats.reduce(0, { count, chat in count + chat.chatStats.unreadCount })
    }

    func getPrevChatItem(_ ci: ChatItem) -> ChatItem? {
        if let i = reversedChatItems.firstIndex(where: { $0.id == ci.id }), i < reversedChatItems.count - 1  {
            return reversedChatItems[i + 1]
        } else {
            return nil
        }
    }
    
    func popChat(_ id: String) {
        if let i = getChatIndex(id) {
            popChat_(i)
        }
    }

    private func popChat_(_ i: Int, to position: Int = 0) {
        let chat = chats.remove(at: i)
        chats.insert(chat, at: position)
    }

    func dismissConnReqView(_ id: String) {
        if let connReqInv = connReqInv,
           let c = getChat(id),
           case let .contactConnection(contactConnection) = c.chatInfo,
           connReqInv == contactConnection.connReqInv {
            dismissAllSheets()
        }
    }

    func removeChat(_ id: String) {
        withAnimation {
            chats.removeAll(where: { $0.id == id })
        }
    }

    func upsertGroupMember(_ groupInfo: GroupInfo, _ member: GroupMember) -> Bool {
        // user member was updated
        if groupInfo.membership.groupMemberId == member.groupMemberId {
            updateGroup(groupInfo)
            return false
        }
        // update current chat
        if chatId == groupInfo.id {
            if let i = groupMembers.firstIndex(where: { $0.id == member.id }) {
                withAnimation(.default) {
                    self.groupMembers[i] = member
                }
                return false
            } else {
                withAnimation { groupMembers.append(member) }
                return true
            }
        } else {
            return false
        }
    }

    func unreadChatItemCounts(itemsInView: Set<String>) -> UnreadChatItemCounts {
        var i = 0
        var totalBelow = 0
        var unreadBelow = 0
        while i < reversedChatItems.count - 1 && !itemsInView.contains(reversedChatItems[i].viewId) {
            totalBelow += 1
            if reversedChatItems[i].isRcvNew() {
                unreadBelow += 1
            }
            i += 1
        }
        return UnreadChatItemCounts(totalBelow: totalBelow, unreadBelow: unreadBelow)
    }

    func topItemInView(itemsInView: Set<String>) -> ChatItem? {
        let maxIx = reversedChatItems.count - 1
        var i = 0
        let inView = { itemsInView.contains(self.reversedChatItems[$0].viewId) }
        while i < maxIx && !inView(i) { i += 1 }
        while i < maxIx && inView(i) { i += 1 }
        return reversedChatItems[min(i - 1, maxIx)]
    }
}

struct UnreadChatItemCounts {
    var totalBelow: Int
    var unreadBelow: Int
}

final class Chat: ObservableObject, Identifiable {
    @Published var chatInfo: ChatInfo
    @Published var chatItems: [ChatItem]
    @Published var chatStats: ChatStats
    @Published var serverInfo = ServerInfo(networkStatus: .unknown)
    var created = Date.now

    struct ServerInfo: Decodable {
        var networkStatus: NetworkStatus
    }

    enum NetworkStatus: Decodable, Equatable {
        case unknown
        case connected
        case disconnected
        case error(String)

        var statusString: LocalizedStringKey {
            get {
                switch self {
                case .connected: return "connected"
                case .error: return "error"
                default: return "connecting"
                }
            }
        }

        var statusExplanation: LocalizedStringKey {
            get {
                switch self {
                case .connected: return "You are connected to the server used to receive messages from this contact."
                case let .error(err): return "Trying to connect to the server used to receive messages from this contact (error: \(err))."
                default: return "Trying to connect to the server used to receive messages from this contact."
                }
            }
        }

        var imageName: String {
            get {
                switch self {
                case .unknown: return "circle.dotted"
                case .connected: return "circle.fill"
                case .disconnected: return "ellipsis.circle.fill"
                case .error: return "exclamationmark.circle.fill"
                }
            }
        }
    }

    init(_ cData: ChatData) {
        self.chatInfo = cData.chatInfo
        self.chatItems = cData.chatItems
        self.chatStats = cData.chatStats
    }

    init(chatInfo: ChatInfo, chatItems: [ChatItem] = [], chatStats: ChatStats = ChatStats(), serverInfo: ServerInfo = ServerInfo(networkStatus: .unknown)) {
        self.chatInfo = chatInfo
        self.chatItems = chatItems
        self.chatStats = chatStats
        self.serverInfo = serverInfo
    }

    var id: ChatId { get { chatInfo.id } }

    var viewId: String { get { "\(chatInfo.id) \(created.timeIntervalSince1970)" } }
}
