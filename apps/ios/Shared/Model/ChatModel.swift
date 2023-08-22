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
import SimpleXChat

actor TerminalItems {
    private var terminalItems: [TerminalItem] = []

    static let shared = TerminalItems()

    func items() -> [TerminalItem] {
        terminalItems
    }

    func add(_ item: TerminalItem) async {
        addTermItem(&terminalItems, item)
        let m = ChatModel.shared
        if m.showingTerminal {
            await MainActor.run {
                addTermItem(&m.terminalItems, item)
            }
        }
    }

    func addCommand(_ start: Date, _ cmd: ChatCommand, _ resp: ChatResponse) async {
        await add(.cmd(start, cmd))
        await add(.resp(.now, resp))
    }
}

private func addTermItem(_ items: inout [TerminalItem], _ item: TerminalItem) {
    if items.count >= 200 {
        items.removeFirst()
    }
    items.append(item)
}

final class ChatModel: ObservableObject {
    @Published var onboardingStage: OnboardingStage?
    @Published var setDeliveryReceipts = false
    @Published var v3DBMigration: V3DBMigrationState = v3DBMigrationDefault.get()
    @Published var currentUser: User?
    @Published var users: [UserInfo] = []
    @Published var chatInitialized = false
    @Published var chatRunning: Bool?
    @Published var chatDbChanged = false
    @Published var chatDbEncrypted: Bool?
    @Published var chatDbStatus: DBMigrationResult?
    @Published var laRequest: LocalAuthRequest?
    // list of chat "previews"
    @Published var chats: [Chat] = []
    // map of connections network statuses, key is agent connection id
    @Published var networkStatuses: Dictionary<String, NetworkStatus> = [:]
    // current chat
    @Published var chatId: String?
    @Published var reversedChatItems: [ChatItem] = []
    @Published var chatToTop: String?
    @Published var groupMembers: [GroupMember] = []
    // items in the terminal view
    @Published var showingTerminal = false
    @Published var terminalItems: [TerminalItem] = []
    @Published var userAddress: UserContactLink?
    @Published var chatItemTTL: ChatItemTTL = .none
    @Published var appOpenUrl: URL?
    @Published var deviceToken: DeviceToken?
    @Published var savedToken: DeviceToken?
    @Published var tokenRegistered = false
    @Published var tokenStatus: NtfTknStatus?
    @Published var notificationMode = NotificationsMode.off
    @Published var notificationPreview: NotificationPreviewMode = ntfPreviewModeGroupDefault.get()
    // pending notification actions
    @Published var ntfContactRequest: NTFContactRequest?
    @Published var ntfCallInvitationAction: (ChatId, NtfCallAction)?
    // current WebRTC call
    @Published var callInvitations: Dictionary<ChatId, RcvCallInvitation> = [:]
    @Published var activeCall: Call?
    @Published var callCommand: WCallCommand?
    @Published var showCallView = false
    // currently showing QR code
    @Published var connReqInv: String?
    // audio recording and playback
    @Published var stopPreviousRecPlay: URL? = nil // coordinates currently playing source
    @Published var draft: ComposeState?
    @Published var draftChatId: String?
    // tracks keyboard height via subscription in AppDelegate
    @Published var keyboardHeight: CGFloat = 0

    var messageDelivery: Dictionary<Int64, () -> Void> = [:]

    var filesToDelete: Set<URL> = []

    static let shared = ChatModel()

    static var ok: Bool { ChatModel.shared.chatDbStatus == .ok }

    var ntfEnableLocal: Bool {
        notificationMode == .off || ntfEnableLocalGroupDefault.get()
    }

    var ntfEnablePeriodic: Bool {
        notificationMode == .periodic || ntfEnablePeriodicGroupDefault.get()
    }

    func getUser(_ userId: Int64) -> User? {
        currentUser?.userId == userId
        ? currentUser
        : users.first { $0.user.userId == userId }?.user
    }

    func getUserIndex(_ user: User) -> Int? {
        users.firstIndex { $0.user.userId == user.userId }
    }

    func updateUser(_ user: User) {
        if let i = getUserIndex(user) {
            users[i].user = user
        }
        if currentUser?.userId == user.userId {
            currentUser = user
        }
    }

    func removeUser(_ user: User) {
        if let i = getUserIndex(user), users[i].user.userId != currentUser?.userId {
            users.remove(at: i)
        }
    }

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
        updateChat(.direct(contact: contact), addMissing: contact.directOrUsed)
    }

    func updateContactConnectionStats(_ contact: Contact, _ connectionStats: ConnectionStats) {
        var updatedConn = contact.activeConn
        updatedConn.connectionStats = connectionStats
        var updatedContact = contact
        updatedContact.activeConn = updatedConn
        updateContact(updatedContact)
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

    func replaceChat(_ id: String, _ chat: Chat) {
        if let i = getChatIndex(id) {
            chats[i] = chat
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
        NtfManager.shared.setNtfBadgeCount(totalUnreadCountForAllUsers())
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
                increaseUnreadCounter(user: currentUser!)
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
        if let i = getChatItemIndex(cItem) {
            withAnimation {
                _updateChatItem(at: i, with: cItem)
            }
            return false
        } else {
            withAnimation(itemAnimation()) {
                reversedChatItems.insert(cItem, at: hasLiveDummy ? 1 : 0)
            }
            return true
        }

        func itemAnimation() -> Animation? {
            switch cItem.chatDir {
            case .directSnd, .groupSnd: return cItem.meta.isLive ? nil : .default
            default: return .default
            }
        }
    }

    func updateChatItem(_ cInfo: ChatInfo, _ cItem: ChatItem) {
        if chatId == cInfo.id, let i = getChatItemIndex(cItem) {
            withAnimation {
                _updateChatItem(at: i, with: cItem)
            }
        }
    }

    private func _updateChatItem(at i: Int, with cItem: ChatItem) {
        let ci = reversedChatItems[i]
        reversedChatItems[i] = cItem
        reversedChatItems[i].viewTimestamp = .now
        // on some occasions the confirmation of message being accepted by the server (tick)
        // arrives earlier than the response from API, and item remains without tick
        if case .sndNew = cItem.meta.itemStatus {
            reversedChatItems[i].meta.itemStatus = ci.meta.itemStatus
        }
    }

    private func getChatItemIndex(_ cItem: ChatItem) -> Int? {
        reversedChatItems.firstIndex(where: { $0.id == cItem.id })
    }

    func removeChatItem(_ cInfo: ChatInfo, _ cItem: ChatItem) {
        if cItem.isRcvNew {
            decreaseUnreadCounter(cInfo)
        }
        // update previews
        if let chat = getChat(cInfo.id) {
            if let pItem = chat.chatItems.last, pItem.id == cItem.id {
                chat.chatItems = [ChatItem.deletedItemDummy()]
            }
        }
        // remove from current chat
        if chatId == cInfo.id {
            if let i = getChatItemIndex(cItem) {
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

    func updateCurrentUser(_ newProfile: Profile, _ preferences: FullPreferences? = nil) {
        if let current = currentUser {
            currentUser?.profile = toLocalProfile(current.profile.profileId, newProfile, "")
            if let preferences = preferences {
                currentUser?.fullPreferences = preferences
            }
            if let current = currentUser, let i = users.firstIndex(where: { $0.user.userId == current.userId }) {
                users[i].user = current
            }
        }
    }

    func addLiveDummy(_ chatInfo: ChatInfo) -> ChatItem {
        let cItem = ChatItem.liveDummy(chatInfo.chatType)
        withAnimation {
            reversedChatItems.insert(cItem, at: 0)
        }
        return cItem
    }

    func removeLiveDummy(animated: Bool = true) {
        if hasLiveDummy {
            if animated {
                withAnimation { _ = reversedChatItems.removeFirst() }
            } else {
                _ = reversedChatItems.removeFirst()
            }
        }
    }

    private var hasLiveDummy: Bool {
        reversedChatItems.first?.isLiveDummy == true
    }

    func markChatItemsRead(_ cInfo: ChatInfo) {
        // update preview
        _updateChat(cInfo.id) { chat in
            self.decreaseUnreadCounter(user: self.currentUser!, by: chat.chatStats.unreadCount)
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
            markChatItemRead_(j)
            j += 1
        }
    }

    func markChatItemsRead(_ cInfo: ChatInfo, aboveItem: ChatItem? = nil) {
        if let cItem = aboveItem {
            if chatId == cInfo.id, let i = getChatItemIndex(cItem) {
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
                        chat.chatStats.unreadCount -= markedCount
                        self.decreaseUnreadCounter(user: self.currentUser!, by: markedCount)
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
            self.decreaseUnreadCounter(user: self.currentUser!, by: chat.chatStats.unreadCount)
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
        decreaseUnreadCounter(cInfo)
        // update current chat
        if chatId == cInfo.id, let i = getChatItemIndex(cItem) {
            markChatItemRead_(i)
        }
    }

    private func markChatItemRead_(_ i: Int) {
        let meta = reversedChatItems[i].meta
        if case .rcvNew = meta.itemStatus {
            reversedChatItems[i].meta.itemStatus = .rcvRead
            reversedChatItems[i].viewTimestamp = .now
            if meta.itemLive != true, let ttl = meta.itemTimed?.ttl {
                reversedChatItems[i].meta.itemTimed?.deleteAt = .now + TimeInterval(ttl)
            }
        }
    }

    func decreaseUnreadCounter(_ cInfo: ChatInfo) {
        if let i = getChatIndex(cInfo.id) {
            chats[i].chatStats.unreadCount = chats[i].chatStats.unreadCount - 1
            decreaseUnreadCounter(user: currentUser!)
        }
    }

    func increaseUnreadCounter(user: any UserLike) {
        changeUnreadCounter(user: user, by: 1)
        NtfManager.shared.incNtfBadgeCount()
    }

    func decreaseUnreadCounter(user: any UserLike, by: Int = 1) {
        changeUnreadCounter(user: user, by: -by)
        NtfManager.shared.decNtfBadgeCount(by: by)
    }

    private func changeUnreadCounter(user: any UserLike, by: Int) {
        if let i = users.firstIndex(where: { $0.user.userId == user.userId }) {
            users[i].unreadCount += by
        }
    }

    func totalUnreadCountForAllUsers() -> Int {
        chats.filter { $0.chatInfo.ntfsEnabled }.reduce(0, { count, chat in count + chat.chatStats.unreadCount }) +
            users.filter { !$0.user.activeUser }.reduce(0, { unread, next -> Int in unread + next.unreadCount })
    }

    func getConnectedMemberNames(_ ci: ChatItem) -> [String] {
        guard var i = getChatItemIndex(ci) else { return [] }
        var ns: [String] = []
        while i < reversedChatItems.count, let m = reversedChatItems[i].memberConnected {
            ns.append(m.displayName)
            i += 1
        }
        return ns
    }

    func getChatItemNeighbors(_ ci: ChatItem) -> (ChatItem?, ChatItem?) {
        if let i = getChatItemIndex(ci)  {
            return (
                i + 1 < reversedChatItems.count ? reversedChatItems[i + 1] : nil,
                i - 1 >= 0 ? reversedChatItems[i - 1] : nil
            )
        } else {
            return (nil, nil)
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

    func updateGroupMemberConnectionStats(_ groupInfo: GroupInfo, _ member: GroupMember, _ connectionStats: ConnectionStats) {
        if let conn = member.activeConn {
            var updatedConn = conn
            updatedConn.connectionStats = connectionStats
            var updatedMember = member
            updatedMember.activeConn = updatedConn
            _ = upsertGroupMember(groupInfo, updatedMember)
        }
    }

    func unreadChatItemCounts(itemsInView: Set<String>) -> UnreadChatItemCounts {
        var i = 0
        var totalBelow = 0
        var unreadBelow = 0
        while i < reversedChatItems.count - 1 && !itemsInView.contains(reversedChatItems[i].viewId) {
            totalBelow += 1
            if reversedChatItems[i].isRcvNew {
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

    func setContactNetworkStatus(_ contact: Contact, _ status: NetworkStatus) {
        networkStatuses[contact.activeConn.agentConnId] = status
    }

    func contactNetworkStatus(_ contact: Contact) -> NetworkStatus {
        networkStatuses[contact.activeConn.agentConnId] ?? .unknown
    }
}

struct NTFContactRequest {
    var incognito: Bool
    var chatId: String
}

struct UnreadChatItemCounts {
    var totalBelow: Int
    var unreadBelow: Int
}

final class Chat: ObservableObject, Identifiable {
    @Published var chatInfo: ChatInfo
    @Published var chatItems: [ChatItem]
    @Published var chatStats: ChatStats
    var created = Date.now

    init(_ cData: ChatData) {
        self.chatInfo = cData.chatInfo
        self.chatItems = cData.chatItems
        self.chatStats = cData.chatStats
    }

    init(chatInfo: ChatInfo, chatItems: [ChatItem] = [], chatStats: ChatStats = ChatStats()) {
        self.chatInfo = chatInfo
        self.chatItems = chatItems
        self.chatStats = chatStats
    }

    func copy(chatInfo: ChatInfo? = nil, chatItems: [ChatItem]? = nil, chatStats: ChatStats? = nil) -> Chat {
        Chat(
            chatInfo: chatInfo ?? self.chatInfo,
            chatItems: chatItems ?? self.chatItems,
            chatStats: chatStats ?? self.chatStats
        )
    }

    var userCanSend: Bool {
        switch chatInfo {
        case .direct: return true
        case let .group(groupInfo):
            let m = groupInfo.membership
            return m.memberActive && m.memberRole >= .member
        default: return false
        }
    }

    var userIsObserver: Bool {
        switch chatInfo {
        case let .group(groupInfo):
            let m = groupInfo.membership
            return m.memberActive && m.memberRole == .observer
        default: return false
        }
    }

    var id: ChatId { get { chatInfo.id } }

    var viewId: String { get { "\(chatInfo.id) \(created.timeIntervalSince1970)" } }

    public static var sampleData: Chat = Chat(chatInfo: ChatInfo.sampleData.direct, chatItems: [])
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
