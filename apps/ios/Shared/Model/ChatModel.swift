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

class ItemsModel: ObservableObject {
    static let shared = ItemsModel()
    private let publisher = ObservableObjectPublisher()
    private var bag = Set<AnyCancellable>()
    var reversedChatItems: [ChatItem] = [] {
        willSet { publisher.send() }
    }
    var itemAdded = false {
        willSet { publisher.send() }
    }
    
    // Publishes directly to `objectWillChange` publisher,
    // this will cause reversedChatItems to be rendered without throttling
    @Published var isLoading = false
    @Published var showLoadingProgress = false

    init() {
        publisher
            .throttle(for: 0.2, scheduler: DispatchQueue.main, latest: true)
            .sink { self.objectWillChange.send() }
            .store(in: &bag)
    }

    func loadOpenChat(_ chatId: ChatId, willNavigate: @escaping () -> Void = {}) {
        let navigationTimeout = Task {
            do {
                try await Task.sleep(nanoseconds: 250_000000)
                await MainActor.run {
                    willNavigate()
                    ChatModel.shared.chatId = chatId
                }
            } catch {}
        }
        let progressTimeout = Task {
            do {
                try await Task.sleep(nanoseconds: 1500_000000)
                await MainActor.run { showLoadingProgress = true }
            } catch {}
        }
        Task {
            if let chat = ChatModel.shared.getChat(chatId) {
                await MainActor.run { self.isLoading = true }
//                try? await Task.sleep(nanoseconds: 5000_000000)
                await loadChat(chat: chat)
                navigationTimeout.cancel()
                progressTimeout.cancel()
                await MainActor.run {
                    self.isLoading = false
                    self.showLoadingProgress = false
                    willNavigate()
                    ChatModel.shared.chatId = chatId
                }
            }
        }
    }
}

class NetworkModel: ObservableObject {
    // map of connections network statuses, key is agent connection id
    @Published var networkStatuses: Dictionary<String, NetworkStatus> = [:]

    static let shared = NetworkModel()

    private init() { }

    func setContactNetworkStatus(_ contact: Contact, _ status: NetworkStatus) {
        if let conn = contact.activeConn {
            networkStatuses[conn.agentConnId] = status
        }
    }

    func contactNetworkStatus(_ contact: Contact) -> NetworkStatus {
        if let conn = contact.activeConn {
            networkStatuses[conn.agentConnId] ?? .unknown
        } else {
            .unknown
        }
    }
}

/// ChatItemWithMenu can depend on previous or next item for it's appearance
/// This dummy model is used to force an update of all chat items,
/// when they might have changed appearance.
class ChatItemDummyModel: ObservableObject {
    static let shared = ChatItemDummyModel()
    func sendUpdate() { objectWillChange.send() }
}

final class ChatModel: ObservableObject {
    @Published var onboardingStage: OnboardingStage?
    @Published var setDeliveryReceipts = false
    @Published var v3DBMigration: V3DBMigrationState = v3DBMigrationDefault.get()
    @Published var currentUser: User? {
        didSet {
            ThemeManager.applyTheme(currentThemeDefault.get())
        }
    }
    @Published var users: [UserInfo] = []
    @Published var chatInitialized = false
    @Published var chatRunning: Bool?
    @Published var chatDbChanged = false
    @Published var chatDbEncrypted: Bool?
    @Published var chatDbStatus: DBMigrationResult?
    @Published var ctrlInitInProgress: Bool = false
    @Published var notificationResponse: UNNotificationResponse?
    // local authentication
    @Published var contentViewAccessAuthenticated: Bool = false
    @Published var laRequest: LocalAuthRequest?
    // list of chat "previews"
    @Published private(set) var chats: [Chat] = []
    @Published var deletedChats: Set<String> = []
    // current chat
    @Published var chatId: String?
    var chatItemStatuses: Dictionary<Int64, CIStatus> = [:]
    @Published var chatToTop: String?
    @Published var groupMembers: [GMember] = []
    @Published var groupMembersIndexes: Dictionary<Int64, Int> = [:] // groupMemberId to index in groupMembers list
    @Published var membersLoaded = false
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
    @Published var notificationServer: String?
    @Published var notificationPreview: NotificationPreviewMode = ntfPreviewModeGroupDefault.get()
    // pending notification actions
    @Published var ntfContactRequest: NTFContactRequest?
    @Published var ntfCallInvitationAction: (ChatId, NtfCallAction)?
    // current WebRTC call
    @Published var callInvitations: Dictionary<ChatId, RcvCallInvitation> = [:]
    @Published var activeCall: Call?
    let callCommand: WebRTCCommandProcessor = WebRTCCommandProcessor()
    @Published var showCallView = false
    @Published var activeCallViewIsCollapsed = false
    // remote desktop
    @Published var remoteCtrlSession: RemoteCtrlSession?
    // currently showing invitation
    @Published var showingInvitation: ShowingInvitation?
    @Published var migrationState: MigrationToState? = MigrationToDeviceState.makeMigrationState()
    // audio recording and playback
    @Published var stopPreviousRecPlay: URL? = nil // coordinates currently playing source
    @Published var draft: ComposeState?
    @Published var draftChatId: String?
    @Published var networkInfo = UserNetworkInfo(networkType: .other, online: true)

    var messageDelivery: Dictionary<Int64, () -> Void> = [:]

    var filesToDelete: Set<URL> = []

    static let shared = ChatModel()

    let im = ItemsModel.shared

    static var ok: Bool { ChatModel.shared.chatDbStatus == .ok }

    let ntfEnableLocal = true

    var ntfEnablePeriodic: Bool {
        notificationMode != .off
    }

    var activeRemoteCtrl: Bool {
        remoteCtrlSession?.active ?? false
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
        if let i = getUserIndex(user) {
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

    func getGroupChat(_ groupId: Int64) -> Chat? {
        chats.first { chat in
            if case let .group(groupInfo) = chat.chatInfo {
                return groupInfo.groupId == groupId
            } else {
                return false
            }
        }
    }

    func populateGroupMembersIndexes() {
        groupMembersIndexes.removeAll()
        for (i, member) in groupMembers.enumerated() {
            groupMembersIndexes[member.groupMemberId] = i
        }
    }

    func getGroupMember(_ groupMemberId: Int64) -> GMember? {
        if let i = groupMembersIndexes[groupMemberId] {
            return groupMembers[i]
        }
        return nil
    }

    func loadGroupMembers(_ groupInfo: GroupInfo, updateView: @escaping () -> Void = {}) async {
        let groupMembers = await apiListMembers(groupInfo.groupId)
        await MainActor.run {
            if chatId == groupInfo.id {
                self.groupMembers = groupMembers.map { GMember.init($0) }
                self.populateGroupMembersIndexes()
                self.membersLoaded = true
                updateView()
            }
        }
    }

    private func getChatIndex(_ id: String) -> Int? {
        chats.firstIndex(where: { $0.id == id })
    }

    func addChat(_ chat: Chat) {
        if chatId == nil {
            withAnimation { addChat_(chat, at: 0) }
        } else {
            addChat_(chat, at: 0)
        }
        popChatCollector.throttlePopChat(chat.chatInfo.id, currentPosition: 0)
    }

    func addChat_(_ chat: Chat, at position: Int = 0) {
        chats.insert(chat, at: position)
    }

    func updateChatInfo(_ cInfo: ChatInfo) {
        if let i = getChatIndex(cInfo.id) {
            chats[i].chatInfo = cInfo
            chats[i].created = Date.now
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
        updatedConn?.connectionStats = connectionStats
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

    func updateChats(_ newChats: [ChatData]) {
        chats = newChats.map { Chat($0) }
        NtfManager.shared.setNtfBadgeCount(totalUnreadCountForAllUsers())
        popChatCollector.clear()
    }

//    func addGroup(_ group: SimpleXChat.Group) {
//        groups[group.groupInfo.id] = group
//    }

    func addChatItem(_ cInfo: ChatInfo, _ cItem: ChatItem) {
        // mark chat non deleted
        if case let .direct(contact) = cInfo, contact.chatDeleted {
            var updatedContact = contact
            updatedContact.chatDeleted = false
            updateContact(updatedContact)
        }
        // update previews
        if let i = getChatIndex(cInfo.id) {
            chats[i].chatItems = switch cInfo {
            case .group:
                if let currentPreviewItem = chats[i].chatItems.first {
                    if cItem.meta.itemTs >= currentPreviewItem.meta.itemTs {
                        [cItem]
                    } else {
                        [currentPreviewItem]
                    }
                } else {
                    [cItem]
                }
            default:
                [cItem]
            }
            if case .rcvNew = cItem.meta.itemStatus {
                unreadCollector.changeUnreadCounter(cInfo.id, by: 1)
            }
            popChatCollector.throttlePopChat(cInfo.id, currentPosition: i)
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
                if pItem.id == cItem.id || (chatId == cInfo.id && im.reversedChatItems.first(where: { $0.id == cItem.id }) == nil) {
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
        if cItem.isDeletedContent || cItem.meta.itemDeleted != nil {
            VoiceItemState.stopVoiceInChatView(cInfo, cItem)
        }
        // update current chat
        return chatId == cInfo.id ? _upsertChatItem(cInfo, cItem) : res
    }

    private func _upsertChatItem(_ cInfo: ChatInfo, _ cItem: ChatItem) -> Bool {
        if let i = getChatItemIndex(cItem) {
            _updateChatItem(at: i, with: cItem)
            ChatItemDummyModel.shared.sendUpdate()
            return false
        } else {
            var ci = cItem
            if let status = chatItemStatuses.removeValue(forKey: ci.id), case .sndNew = ci.meta.itemStatus {
                ci.meta.itemStatus = status
            }
            im.reversedChatItems.insert(ci, at: hasLiveDummy ? 1 : 0)
            im.itemAdded = true
            ChatItemDummyModel.shared.sendUpdate()
            return true
        }

        func itemAnimation() -> Animation? {
            switch cItem.chatDir {
            case .directSnd, .groupSnd: return cItem.meta.isLive ? nil : .default
            default: return .default
            }
        }
    }

    func updateChatItem(_ cInfo: ChatInfo, _ cItem: ChatItem, status: CIStatus? = nil) {
        if chatId == cInfo.id, let i = getChatItemIndex(cItem) {
            withConditionalAnimation {
                _updateChatItem(at: i, with: cItem)
            }
        } else if let status = status {
            chatItemStatuses.updateValue(status, forKey: cItem.id)
        }
    }

    private func _updateChatItem(at i: Int, with cItem: ChatItem) {
        im.reversedChatItems[i] = cItem
        im.reversedChatItems[i].viewTimestamp = .now
    }

    func getChatItemIndex(_ cItem: ChatItem) -> Int? {
        im.reversedChatItems.firstIndex(where: { $0.id == cItem.id })
    }

    func removeChatItem(_ cInfo: ChatInfo, _ cItem: ChatItem) {
        if cItem.isRcvNew {
            unreadCollector.changeUnreadCounter(cInfo.id, by: -1)
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
                    im.reversedChatItems.remove(at: i)
                }
            }
        }
        VoiceItemState.stopVoiceInChatView(cInfo, cItem)
    }

    func nextChatItemData<T>(_ chatItemId: Int64, previous: Bool, map: @escaping (ChatItem) -> T?) -> T? {
        guard var i = im.reversedChatItems.firstIndex(where: { $0.id == chatItemId }) else { return nil }
        if previous {
            while i < im.reversedChatItems.count - 1 {
                i += 1
                if let res = map(im.reversedChatItems[i]) { return res }
            }
        } else {
            while i > 0 {
                i -= 1
                if let res = map(im.reversedChatItems[i]) { return res }
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

    func updateCurrentUserUiThemes(uiThemes: ThemeModeOverrides?) {
        guard var current = currentUser, current.uiThemes != uiThemes else { return }
        current.uiThemes = uiThemes
        let i = users.firstIndex(where: { $0.user.userId == current.userId })
        if let i {
            users[i].user = current
        }
        currentUser = current
    }

    func addLiveDummy(_ chatInfo: ChatInfo) -> ChatItem {
        let cItem = ChatItem.liveDummy(chatInfo.chatType)
        withAnimation {
            im.reversedChatItems.insert(cItem, at: 0)
            im.itemAdded = true
        }
        return cItem
    }

    func removeLiveDummy(animated: Bool = true) {
        if hasLiveDummy {
            if animated {
                withAnimation { _ = im.reversedChatItems.removeFirst() }
            } else {
                _ = im.reversedChatItems.removeFirst()
            }
        }
    }

    private var hasLiveDummy: Bool {
        im.reversedChatItems.first?.isLiveDummy == true
    }

    func markChatItemsRead(_ cInfo: ChatInfo) {
        // update preview
        _updateChat(cInfo.id) { chat in
            self.decreaseUnreadCounter(user: self.currentUser!, by: chat.chatStats.unreadCount)
            self.updateFloatingButtons(unreadCount: 0)
            chat.chatStats = ChatStats()
        }
        // update current chat
        if chatId == cInfo.id {
            markCurrentChatRead()
        }
    }

    private func markCurrentChatRead(fromIndex i: Int = 0) {
        var j = i
        while j < im.reversedChatItems.count {
            markChatItemRead_(j)
            j += 1
        }
    }

    private func updateFloatingButtons(unreadCount: Int) {
        let fbm = ChatView.FloatingButtonModel.shared
        fbm.totalUnread = unreadCount
        fbm.objectWillChange.send()
    }

    func markChatItemsRead(_ cInfo: ChatInfo, aboveItem: ChatItem? = nil) {
        if let cItem = aboveItem {
            if chatId == cInfo.id, let i = getChatItemIndex(cItem) {
                markCurrentChatRead(fromIndex: i)
                _updateChat(cInfo.id) { chat in
                    var unreadBelow = 0
                    var j = i - 1
                    while j >= 0 {
                        if case .rcvNew = self.im.reversedChatItems[j].meta.itemStatus {
                            unreadBelow += 1
                        }
                        j -= 1
                    }
                    // update preview
                    let markedCount = chat.chatStats.unreadCount - unreadBelow
                    if markedCount > 0 {
                        chat.chatStats.unreadCount -= markedCount
                        self.decreaseUnreadCounter(user: self.currentUser!, by: markedCount)
                        self.updateFloatingButtons(unreadCount: chat.chatStats.unreadCount)
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
            chatItemStatuses = [:]
            im.reversedChatItems = []
        }
    }

    func markChatItemsRead(_ cInfo: ChatInfo, _ itemIds: [ChatItem.ID]) {
        if self.chatId == cInfo.id {
            for itemId in itemIds {
                if let i = im.reversedChatItems.firstIndex(where: { $0.id == itemId }) {
                    markChatItemRead_(i)
                }
            }
        }
        self.unreadCollector.changeUnreadCounter(cInfo.id, by: -itemIds.count)
    }

    private let unreadCollector = UnreadCollector()

    class UnreadCollector {
        private let subject = PassthroughSubject<Void, Never>()
        private var bag = Set<AnyCancellable>()
        private var unreadCounts: [ChatId: Int] = [:]

        init() {
            subject
                .debounce(for: 1, scheduler: DispatchQueue.main)
                .sink {
                    let m = ChatModel.shared
                    for (chatId, count) in self.unreadCounts {
                        if let i = m.getChatIndex(chatId) {
                            m.changeUnreadCounter(i, by: count)
                        }
                    }
                    self.unreadCounts = [:]
                }
                .store(in: &bag)
        }

        func changeUnreadCounter(_ chatId: ChatId, by count: Int) {
            if chatId == ChatModel.shared.chatId {
                ChatView.FloatingButtonModel.shared.totalUnread += count
            }
            self.unreadCounts[chatId] = (self.unreadCounts[chatId] ?? 0) + count
            subject.send()
        }
    }

    let popChatCollector = PopChatCollector()
    
    class PopChatCollector {
        private let subject = PassthroughSubject<Void, Never>()
        private var bag = Set<AnyCancellable>()
        private var chatsToPop: [ChatId: Date] = [:]
        private let popTsComparator = KeyPathComparator<Chat>(\.popTs, order: .reverse)

        init() {
            subject
                .throttle(for: 2, scheduler: DispatchQueue.main, latest: true)
                .sink { self.popCollectedChats() }
                .store(in: &bag)
        }
        
        func throttlePopChat(_ chatId: ChatId, currentPosition: Int) {
            let m = ChatModel.shared
            if currentPosition > 0 && m.chatId == chatId {
                m.chatToTop = chatId
            }
            if currentPosition > 0 || !chatsToPop.isEmpty {
                chatsToPop[chatId] = Date.now
                subject.send()
            }
        }
        
        func clear() {
            chatsToPop = [:]
        }

        func popCollectedChats() {
            let m = ChatModel.shared
            var ixs: IndexSet = []
            var chs: [Chat] = []
            // collect chats that received updates
            for (chatId, popTs) in self.chatsToPop {
                // Currently opened chat is excluded, removing it from the list would navigate out of it
                // It will be popped to top later when user exits from the list.
                if m.chatId != chatId, let i = m.getChatIndex(chatId) {
                    ixs.insert(i)
                    let ch = m.chats[i]
                    ch.popTs = popTs
                    chs.append(ch)
                }
            }

            let removeInsert = {
                m.chats.remove(atOffsets: ixs)
                // sort chats by pop timestamp in descending order
                m.chats.insert(contentsOf: chs.sorted(using: self.popTsComparator), at: 0)
            }

            if m.chatId == nil {
                withAnimation { removeInsert() }
            } else {
                removeInsert()
            }

            self.chatsToPop = [:]
        }
    }

    private func markChatItemRead_(_ i: Int) {
        let meta = im.reversedChatItems[i].meta
        if case .rcvNew = meta.itemStatus {
            im.reversedChatItems[i].meta.itemStatus = .rcvRead
            im.reversedChatItems[i].viewTimestamp = .now
            if meta.itemLive != true, let ttl = meta.itemTimed?.ttl {
                im.reversedChatItems[i].meta.itemTimed?.deleteAt = .now + TimeInterval(ttl)
            }
        }
    }

    func changeUnreadCounter(_ chatIndex: Int, by count: Int) {
        chats[chatIndex].chatStats.unreadCount = chats[chatIndex].chatStats.unreadCount + count
        changeUnreadCounter(user: currentUser!, by: count)
    }

    func increaseUnreadCounter(user: any UserLike) {
        changeUnreadCounter(user: user, by: 1)
    }

    func decreaseUnreadCounter(user: any UserLike, by: Int = 1) {
        changeUnreadCounter(user: user, by: -by)
    }

    private func changeUnreadCounter(user: any UserLike, by: Int) {
        if let i = users.firstIndex(where: { $0.user.userId == user.userId }) {
            users[i].unreadCount += by
        }
        NtfManager.shared.changeNtfBadgeCount(by: by)
    }

    func totalUnreadCountForAllUsers() -> Int {
        chats.filter { $0.chatInfo.ntfsEnabled }.reduce(0, { count, chat in count + chat.chatStats.unreadCount }) +
            users.filter { !$0.user.activeUser }.reduce(0, { unread, next -> Int in unread + next.unreadCount })
    }

    // this function analyses "connected" events and assumes that each member will be there only once
    func getConnectedMemberNames(_ chatItem: ChatItem) -> (Int, [String]) {
        var count = 0
        var ns: [String] = []
        if let ciCategory = chatItem.mergeCategory,
           var i = getChatItemIndex(chatItem) {
            while i < im.reversedChatItems.count {
                let ci = im.reversedChatItems[i]
                if ci.mergeCategory != ciCategory { break }
                if let m = ci.memberConnected {
                    ns.append(m.displayName)
                }
                count += 1
                i += 1
            }
        }
        return (count, ns)
    }

    // returns the index of the passed item and the next item (it has smaller index)
    func getNextChatItem(_ ci: ChatItem) -> (Int?, ChatItem?) {
        if let i = getChatItemIndex(ci) {
            (i, i > 0 ? im.reversedChatItems[i - 1] : nil)
        } else {
            (nil, nil)
        }
    }

    // returns the index of the first item in the same merged group (the first hidden item)
    // and the previous visible item with another merge category
    func getPrevShownChatItem(_ ciIndex: Int?, _ ciCategory: CIMergeCategory?) -> (Int?, ChatItem?) {
        guard var i = ciIndex else { return (nil, nil) }
        let fst = im.reversedChatItems.count - 1
        while i < fst {
            i = i + 1
            let ci = im.reversedChatItems[i]
            if ciCategory == nil || ciCategory != ci.mergeCategory {
                return (i - 1, ci)
            }
        }
        return (i, nil)
    }

    // returns the previous member in the same merge group and the count of members in this group
    func getPrevHiddenMember(_ member: GroupMember, _ range: ClosedRange<Int>) -> (GroupMember?, Int) {
        var prevMember: GroupMember? = nil
        var memberIds: Set<Int64> = []
        for i in range {
            if case let .groupRcv(m) = im.reversedChatItems[i].chatDir {
                if prevMember == nil && m.groupMemberId != member.groupMemberId { prevMember = m }
                memberIds.insert(m.groupMemberId)
            }
        }
        return (prevMember, memberIds.count)
    }

    func popChat(_ id: String) {
        if let i = getChatIndex(id) {
            // no animation here, for it not to look like it just moved when leaving the chat
            popChat_(i)
        }
    }

    private func popChat_(_ i: Int, to position: Int = 0) {
        let chat = chats.remove(at: i)
        chats.insert(chat, at: position)
    }

    func dismissConnReqView(_ id: String) {
        if id == showingInvitation?.connId {
            markShowingInvitationUsed()
            dismissAllSheets()
        }
    }

    func markShowingInvitationUsed() {
        showingInvitation?.connChatUsed = true
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
            if let i = groupMembersIndexes[member.groupMemberId] {
                withAnimation(.default) {
                    self.groupMembers[i].wrapped = member
                    self.groupMembers[i].created = Date.now
                }
                return false
            } else {
                withAnimation {
                    groupMembers.append(GMember(member))
                    groupMembersIndexes[member.groupMemberId] = groupMembers.count - 1
                }
                return true
            }
        } else {
            return false
        }
    }

    func updateGroupMemberConnectionStats(_ groupInfo: GroupInfo, _ member: GroupMember, _ connectionStats: ConnectionStats) {
        if var conn = member.activeConn {
            conn.connectionStats = connectionStats
            var updatedMember = member
            updatedMember.activeConn = conn
            _ = upsertGroupMember(groupInfo, updatedMember)
        }
    }
}

struct ShowingInvitation {
    var connId: String
    var connChatUsed: Bool
}

struct NTFContactRequest {
    var incognito: Bool
    var chatId: String
}

final class Chat: ObservableObject, Identifiable, ChatLike {
    @Published var chatInfo: ChatInfo
    @Published var chatItems: [ChatItem]
    @Published var chatStats: ChatStats
    var created = Date.now
    fileprivate var popTs: Date?

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
        case .local:
            return true
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

final class GMember: ObservableObject, Identifiable {
    @Published var wrapped: GroupMember
    var created = Date.now

    init(_ member: GroupMember) {
        self.wrapped = member
    }

    var id: String { wrapped.id }
    var groupId: Int64 { wrapped.groupId }
    var groupMemberId: Int64 { wrapped.groupMemberId }
    var displayName: String { wrapped.displayName }
    var viewId: String { get { "\(wrapped.id) \(created.timeIntervalSince1970)" } }
    static let sampleData = GMember(GroupMember.sampleData)
}

struct RemoteCtrlSession {
    var ctrlAppInfo: CtrlAppInfo?
    var appVersion: String
    var sessionState: UIRemoteCtrlSessionState

    func updateState(_ state: UIRemoteCtrlSessionState) -> RemoteCtrlSession {
        RemoteCtrlSession(ctrlAppInfo: ctrlAppInfo, appVersion: appVersion, sessionState: state)
    }

    var active: Bool {
        if case .connected = sessionState { true } else { false }
    }

    var discovery: Bool {
        if case .searching = sessionState { true } else { false }
    }

    var sessionCode: String? {
        switch sessionState {
        case let .pendingConfirmation(_, sessionCode): sessionCode
        case let .connected(_, sessionCode): sessionCode
        default: nil
        }
    }
}

enum UIRemoteCtrlSessionState {
    case starting
    case searching
    case found(remoteCtrl: RemoteCtrlInfo, compatible: Bool)
    case connecting(remoteCtrl_: RemoteCtrlInfo?)
    case pendingConfirmation(remoteCtrl_: RemoteCtrlInfo?, sessionCode: String)
    case connected(remoteCtrl: RemoteCtrlInfo, sessionCode: String)
}
