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

    func addCommand<R: ChatAPIResult>(_ start: Date, _ cmd: ChatCommand, _ res: APIResult<R>) async {
        await add(.cmd(start, cmd))
        await addResult(res)
    }

    func addResult<R: ChatAPIResult>(_ res: APIResult<R>) async {
        let item: TerminalItem = switch res {
        case let .result(r): .res(.now, r)
        case let .error(e): .err(.now, e)
        case let .invalid(type, json): .bad(.now, type, json)
        }
        await add(item)
    }
}

private func addTermItem(_ items: inout [TerminalItem], _ item: TerminalItem) {
    if items.count >= 200 {
        items.removeFirst()
    }
    items.append(item)
}

// analogue for SecondaryContextFilter in Kotlin
enum SecondaryItemsModelFilter {
    case groupChatScopeContext(groupScopeInfo: GroupChatScopeInfo)
    case msgContentTagContext(contentTag: MsgContentTag)

    func descr() -> String {
        switch self {
        case let .groupChatScopeContext(groupScopeInfo):
            return "groupChatScopeContext \(groupScopeInfo.toChatScope())"
        case let .msgContentTagContext(contentTag):
            return "msgContentTagContext \(contentTag.rawValue)"
        }
    }
}

// analogue for ChatsContext in Kotlin
class ItemsModel: ObservableObject {
    static let shared = ItemsModel(secondaryIMFilter: nil)
    public var secondaryIMFilter: SecondaryItemsModelFilter?
    public var preloadState = PreloadState()
    private let publisher = ObservableObjectPublisher()
    private var bag = Set<AnyCancellable>()
    var reversedChatItems: [ChatItem] = [] {
        willSet { publisher.send() }
    }
    var itemAdded = false {
        willSet { publisher.send() }
    }

    let chatState = ActiveChatState()

    // Publishes directly to `objectWillChange` publisher,
    // this will cause reversedChatItems to be rendered without throttling
    @Published var isLoading = false
    @Published var showLoadingProgress: ChatId? = nil

    private var navigationTimeoutTask: Task<Void, Never>? = nil
    private var loadChatTask: Task<Void, Never>? = nil

    var lastItemsLoaded: Bool {
        chatState.splits.isEmpty || chatState.splits.first != reversedChatItems.first?.id
    }

    init(secondaryIMFilter: SecondaryItemsModelFilter? = nil) {
        self.secondaryIMFilter = secondaryIMFilter
        publisher
            .throttle(for: 0.2, scheduler: DispatchQueue.main, latest: true)
            .sink { self.objectWillChange.send() }
            .store(in: &bag)
    }

    static func loadSecondaryChat(_ chatId: ChatId, chatFilter: SecondaryItemsModelFilter, willNavigate: @escaping () -> Void = {}) {
        let im = ItemsModel(secondaryIMFilter: chatFilter)
        ChatModel.shared.secondaryIM = im
        im.loadOpenChat(chatId, willNavigate: willNavigate)
    }

    func loadOpenChat(_ chatId: ChatId, willNavigate: @escaping () -> Void = {}) {
        navigationTimeoutTask?.cancel()
        loadChatTask?.cancel()
        navigationTimeoutTask = Task {
            do {
                try await Task.sleep(nanoseconds: 250_000000)
                await MainActor.run {
                    ChatModel.shared.chatId = chatId
                    willNavigate()
                }
            } catch {}
        }
        loadChatTask = Task {
            await MainActor.run { self.isLoading = true }
//            try? await Task.sleep(nanoseconds: 1000_000000)
            await loadChat(chatId: chatId, im: self)
            if !Task.isCancelled {
                await MainActor.run {
                    self.isLoading = false
                    self.showLoadingProgress = nil
                }
            }
        }
    }

    func loadOpenChatNoWait(_ chatId: ChatId, _ openAroundItemId: ChatItem.ID? = nil) {
        navigationTimeoutTask?.cancel()
        loadChatTask?.cancel()
        loadChatTask = Task {
            //            try? await Task.sleep(nanoseconds: 1000_000000)
            await loadChat(chatId: chatId, im: self, openAroundItemId: openAroundItemId, clearItems: openAroundItemId == nil)
            if !Task.isCancelled {
                await MainActor.run {
                    if openAroundItemId == nil {
                        ChatModel.shared.chatId = chatId
                    }
                }
            }
        }
    }

    public var contentTag: MsgContentTag? {
        switch secondaryIMFilter {
        case nil: nil
        case .groupChatScopeContext: nil
        case let .msgContentTagContext(contentTag): contentTag
        }
    }

    public var groupScopeInfo: GroupChatScopeInfo? {
        switch secondaryIMFilter {
        case nil: nil
        case let .groupChatScopeContext(scopeInfo): scopeInfo
        case .msgContentTagContext: nil
        }
    }
}

class PreloadState {
    var prevFirstVisible: Int64 = Int64.min
    var prevItemsCount: Int = 0
    var preloading: Bool = false

    func clear() {
        prevFirstVisible = Int64.min
        prevItemsCount = 0
        preloading = false
    }
}

class ChatTagsModel: ObservableObject {
    static let shared = ChatTagsModel()

    @Published var userTags: [ChatTag] = []
    @Published var activeFilter: ActiveFilter? = nil
    @Published var presetTags: [PresetTag:Int] = [:]
    @Published var unreadTags: [Int64:Int] = [:]

    func updateChatTags(_ chats: [Chat]) {
        let tm = ChatTagsModel.shared
        var newPresetTags: [PresetTag:Int] = [:]
        var newUnreadTags: [Int64:Int] = [:]
        for chat in chats {
            for tag in PresetTag.allCases {
                if presetTagMatchesChat(tag, chat.chatInfo, chat.chatStats) {
                    newPresetTags[tag] = (newPresetTags[tag] ?? 0) + 1
                }
            }
            if chat.unreadTag, let tags = chat.chatInfo.chatTags {
                for tag in tags {
                    newUnreadTags[tag] = (newUnreadTags[tag] ?? 0) + 1
                }
            }
        }
        presetTags = newPresetTags
        unreadTags = newUnreadTags
        clearActiveChatFilterIfNeeded()
    }

    func updateChatFavorite(favorite: Bool, wasFavorite: Bool) {
        let count = presetTags[.favorites]
        if favorite && !wasFavorite {
            presetTags[.favorites] = (count ?? 0) + 1
        } else if !favorite && wasFavorite, let count {
            presetTags[.favorites] = max(0, count - 1)
            clearActiveChatFilterIfNeeded()
        }
    }

    func addPresetChatTags(_ chatInfo: ChatInfo, _ chatStats: ChatStats) {
        for tag in PresetTag.allCases {
            if presetTagMatchesChat(tag, chatInfo, chatStats) {
                presetTags[tag] = (presetTags[tag] ?? 0) + 1
            }
        }
    }

    func removePresetChatTags(_ chatInfo: ChatInfo, _ chatStats: ChatStats) {
        for tag in PresetTag.allCases {
            if presetTagMatchesChat(tag, chatInfo, chatStats) {
                if let count = presetTags[tag] {
                    if count > 1 {
                        presetTags[tag] = count - 1
                    } else {
                        presetTags.removeValue(forKey: tag)
                    }
                }
            }
        }
        clearActiveChatFilterIfNeeded()
    }

    func markChatTagRead(_ chat: Chat) -> Void {
        if chat.unreadTag, let tags = chat.chatInfo.chatTags {
            decTagsReadCount(tags)
        }
    }

    func updateChatTagRead(_ chat: Chat, wasUnread: Bool) -> Void {
        guard let tags = chat.chatInfo.chatTags else { return }
        let nowUnread = chat.unreadTag
        if nowUnread && !wasUnread {
            for tag in tags {
                unreadTags[tag] = (unreadTags[tag] ?? 0) + 1
            }
        } else if !nowUnread && wasUnread {
            decTagsReadCount(tags)
        }
    }

    func decTagsReadCount(_ tags: [Int64]) -> Void {
        for tag in tags {
            if let count = unreadTags[tag] {
                unreadTags[tag] = max(0, count - 1)
            }
        }
    }

    func changeGroupReportsTag(_ by: Int = 0) {
        if by == 0 { return }
        presetTags[.groupReports] = max(0, (presetTags[.groupReports] ?? 0) + by)
        clearActiveChatFilterIfNeeded()
    }

    func clearActiveChatFilterIfNeeded() {
        let clear = switch activeFilter {
        case let .presetTag(tag): (presetTags[tag] ?? 0) == 0
        case let .userTag(tag): !userTags.contains(tag)
        case .unread, nil: false
        }
        if clear { activeFilter = nil }
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

class ConnectProgressManager: ObservableObject {
    @Published private var connectInProgress: String? = nil
    @Published private var connectProgressByTimeout: Bool = false
    private var onCancel: (() -> Void)?

    static let shared = ConnectProgressManager()

    func startConnectProgress(_ text: String, onCancel: (() -> Void)? = nil) {
        connectInProgress = text
        self.onCancel = onCancel
        DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
            self.connectProgressByTimeout = self.connectInProgress != nil
        }
    }

    func stopConnectProgress() {
        connectInProgress = nil
        onCancel = nil
        connectProgressByTimeout = false
    }

    func cancelConnectProgress() {
        onCancel?()
        stopConnectProgress()
    }

    var showConnectProgress: String? {
        connectProgressByTimeout ? connectInProgress : nil
    }

    var isInProgress: Bool {
        connectInProgress != nil
    }
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
    @Published var openAroundItemId: ChatItem.ID? = nil
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
    @Published var reRegisterTknStatus: NtfTknStatus? = nil
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
    // usage conditions
    @Published var conditions: ServerOperatorConditions = .empty

    var messageDelivery: Dictionary<Int64, () -> Void> = [:]

    var filesToDelete: Set<URL> = []

    static let shared = ChatModel()

    let im = ItemsModel.shared

    // ItemsModel for secondary chat view (such as support scope chat), as opposed to ItemsModel.shared used for primary chat
    @Published var secondaryIM: ItemsModel? = nil
    @Published var secondaryPendingInviteeChatOpened = false

    static var ok: Bool { ChatModel.shared.chatDbStatus == .ok }

    let ntfEnableLocal = true

    var ntfEnablePeriodic: Bool {
        notificationMode != .off
    }

    var activeRemoteCtrl: Bool {
        remoteCtrlSession?.active ?? false
    }

    var addressShortLinkDataSet: Bool {
        userAddress?.shortLinkDataSet ?? true
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
            if case let .group(groupInfo, _) = chat.chatInfo {
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
            if case let .group(groupInfo, groupChatScope) = cInfo, groupChatScope != nil {
                chats[i].chatInfo = .group(groupInfo: groupInfo, groupChatScope: nil)
            } else {
                chats[i].chatInfo = cInfo
            }
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
        updateChat(.group(groupInfo: groupInfo, groupChatScope: nil))
    }

    private func updateChat(_ cInfo: ChatInfo, addMissing: Bool = true) {
        if hasChat(cInfo.id) {
            updateChatInfo(cInfo)
        } else if addMissing {
            addChat(Chat(chatInfo: cInfo, chatItems: []))
            ChatTagsModel.shared.addPresetChatTags(cInfo, ChatStats())
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

    func updateChats(_ newChats: [ChatData], keepingChatId: String? = nil) {
        if let keepingChatId,
           let chatToKeep = getChat(keepingChatId),
           let i = newChats.firstIndex(where: { $0.id == keepingChatId }) {
            let remainingNewChats = Array(newChats[..<i] + newChats[(i + 1)...])
            chats = [chatToKeep] + remainingNewChats.map { Chat($0) }
        } else {
            chats = newChats.map { Chat($0) }
        }
        NtfManager.shared.setNtfBadgeCount(totalUnreadCountForAllUsers())
        popChatCollector.clear()
    }

//    func addGroup(_ group: SimpleXChat.Group) {
//        groups[group.groupInfo.id] = group
//    }

    func addChatItem(_ chatInfo: ChatInfo, _ cItem: ChatItem) {
        // updates membersRequireAttention
        let cInfo: ChatInfo
        if case let .direct(contact) = chatInfo, contact.chatDeleted {
            // mark chat non deleted
            var updatedContact = contact
            updatedContact.chatDeleted = false
            cInfo = .direct(contact: updatedContact)
        } else {
            cInfo = chatInfo
        }
        updateChatInfo(cInfo)
        // update chat list
        if let i = getChatIndex(cInfo.id) {
            // update preview
            if cInfo.groupChatScope() == nil || cInfo.groupInfo?.membership.memberPending ?? false {
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
                    unreadCollector.changeUnreadCounter(cInfo.id, by: 1, unreadMentions: cItem.meta.userMention ? 1 : 0)
                }
            }
            // pop chat
            popChatCollector.throttlePopChat(cInfo.id, currentPosition: i)
        } else {
            if cInfo.groupChatScope() == nil {
                addChat(Chat(chatInfo: cInfo, chatItems: [cItem]))
            } else {
                addChat(Chat(chatInfo: cInfo, chatItems: []))
            }
        }
        // add to current scope
        if let ciIM = getCIItemsModel(cInfo, cItem) {
            _ = _upsertChatItem(ciIM, cInfo, cItem)
        }
    }

    func getCIItemsModel(_ cInfo: ChatInfo, _ ci: ChatItem) -> ItemsModel? {
        let cInfoScope = cInfo.groupChatScope()
        if let cInfoScope = cInfoScope {
            switch cInfoScope {
            case .memberSupport:
                switch secondaryIM?.secondaryIMFilter {
                case .none:
                    return nil
                case let .groupChatScopeContext(groupScopeInfo):
                    return (cInfo.id == chatId && sameChatScope(cInfoScope, groupScopeInfo.toChatScope())) ? secondaryIM : nil
                case let .msgContentTagContext(contentTag):
                    return (cInfo.id == chatId && ci.isReport && contentTag == .report) ? secondaryIM : nil
                }
            }
        } else {
            return cInfo.id == chatId ? im : nil
        }
    }

    func upsertChatItem(_ cInfo: ChatInfo, _ cItem: ChatItem) -> Bool {
        // update chat list
        var itemAdded: Bool = false
        if cInfo.groupChatScope() == nil {
            if let chat = getChat(cInfo.id) {
                if let pItem = chat.chatItems.last {
                    if pItem.id == cItem.id || (chatId == cInfo.id && im.reversedChatItems.first(where: { $0.id == cItem.id }) == nil) {
                        chat.chatItems = [cItem]
                    }
                } else {
                    chat.chatItems = [cItem]
                }
            } else {
                addChat(Chat(chatInfo: cInfo, chatItems: [cItem]))
                itemAdded = true
            }
            if cItem.isDeletedContent || cItem.meta.itemDeleted != nil {
                VoiceItemState.stopVoiceInChatView(cInfo, cItem)
            }
        }
        // update current scope
        if let ciIM = getCIItemsModel(cInfo, cItem) {
            itemAdded = _upsertChatItem(ciIM, cInfo, cItem)
        }
        return itemAdded
    }

    private func _upsertChatItem(_ ciIM: ItemsModel, _ cInfo: ChatInfo, _ cItem: ChatItem) -> Bool {
        if let i = getChatItemIndex(ciIM, cItem) {
            let oldStatus = ciIM.reversedChatItems[i].meta.itemStatus
            let newStatus = cItem.meta.itemStatus
            var ci = cItem
            if shouldKeepOldSndCIStatus(oldStatus: oldStatus, newStatus: newStatus) {
                ci.meta.itemStatus = oldStatus
            }
            _updateChatItem(ciIM: ciIM, at: i, with: ci)
            ChatItemDummyModel.shared.sendUpdate() // TODO [knocking] review what's this
            return false
        } else {
            ciIM.reversedChatItems.insert(cItem, at: hasLiveDummy ? 1 : 0)
            ciIM.chatState.itemAdded((cItem.id, cItem.isRcvNew), hasLiveDummy ? 1 : 0)
            ciIM.itemAdded = true
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
        if let ciIM = getCIItemsModel(cInfo, cItem),
           let i = getChatItemIndex(ciIM, cItem) {
            withConditionalAnimation {
                _updateChatItem(ciIM: ciIM, at: i, with: cItem)
            }
        }
    }

    private func _updateChatItem(ciIM: ItemsModel, at i: Int, with cItem: ChatItem) {
        ciIM.reversedChatItems[i] = cItem
        ciIM.reversedChatItems[i].viewTimestamp = .now
    }

    func getChatItemIndex(_ ciIM: ItemsModel, _ cItem: ChatItem) -> Int? {
        ciIM.reversedChatItems.firstIndex(where: { $0.id == cItem.id })
    }

    func removeChatItem(_ cInfo: ChatInfo, _ cItem: ChatItem) {
        // update chat list
        if cInfo.groupChatScope() == nil {
            if cItem.isRcvNew {
                unreadCollector.changeUnreadCounter(cInfo.id, by: -1, unreadMentions: cItem.meta.userMention ? -1 : 0)
            }
            // update previews
            if let chat = getChat(cInfo.id) {
                if let pItem = chat.chatItems.last, pItem.id == cItem.id {
                    chat.chatItems = [ChatItem.deletedItemDummy()]
                }
            }
        }
        // remove from current scope
        if let ciIM = getCIItemsModel(cInfo, cItem) {
            if let i = getChatItemIndex(ciIM, cItem) {
                withAnimation {
                    let item = ciIM.reversedChatItems.remove(at: i)
                    ciIM.chatState.itemsRemoved([(item.id, i, item.isRcvNew)], im.reversedChatItems.reversed())
                }
            }
        }
        VoiceItemState.stopVoiceInChatView(cInfo, cItem)
    }

    func removeMemberItems(_ removedMember: GroupMember, byMember: GroupMember, _ groupInfo: GroupInfo) {
        // this should not happen, only another member can "remove" user, user can only "leave" (another event).
        if byMember.groupMemberId == groupInfo.membership.groupMemberId {
            logger.debug("exiting removeMemberItems")
            return
        }
        if chatId == groupInfo.id {
            for i in 0..<im.reversedChatItems.count {
                if let updatedItem = removedUpdatedItem(im.reversedChatItems[i]) {
                    _updateChatItem(ciIM: im, at: i, with: updatedItem) // TODO [knocking] review: use getCIItemsModel?
                }
            }
        } else if let chat = getChat(groupInfo.id),
                  chat.chatItems.count > 0,
                  let updatedItem = removedUpdatedItem(chat.chatItems[0]) {
                chat.chatItems = [updatedItem]
        }

        func removedUpdatedItem(_ item: ChatItem) -> ChatItem? {
            let newContent: CIContent
            if case .groupSnd = item.chatDir, removedMember.groupMemberId == groupInfo.membership.groupMemberId {
                newContent = .sndModerated
            } else if case let .groupRcv(groupMember) = item.chatDir, groupMember.groupMemberId == removedMember.groupMemberId {
                newContent = .rcvModerated
            } else {
                return nil
            }
            var updatedItem = item
            updatedItem.meta.itemDeleted = .moderated(deletedTs: Date.now, byGroupMember: byMember)
            if groupInfo.fullGroupPreferences.fullDelete.on {
                updatedItem.content = newContent
            }
            if item.isActiveReport {
                decreaseGroupReportsCounter(groupInfo.id)
            }
            return updatedItem
        }
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
            im.chatState.itemAdded((cItem.id, cItem.isRcvNew), 0)
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

    func markAllChatItemsRead(_ chatIM: ItemsModel, _ cInfo: ChatInfo) {
        // update preview
        _updateChat(cInfo.id) { chat in
            self.decreaseUnreadCounter(user: self.currentUser!, chat: chat)
            ChatTagsModel.shared.markChatTagRead(chat)
            chat.chatStats = ChatStats()
        }
        // update current chat
        if chatId == cInfo.id {
            var i = 0
            while i < im.reversedChatItems.count {
                markChatItemRead_(chatIM, i)
                i += 1
            }
            im.chatState.itemsRead(nil, im.reversedChatItems.reversed())
        }
    }
    func markChatUnread(_ cInfo: ChatInfo, unreadChat: Bool = true) {
        _updateChat(cInfo.id) { chat in
            let wasUnread = chat.unreadTag
            chat.chatStats.unreadChat = unreadChat
            ChatTagsModel.shared.updateChatTagRead(chat, wasUnread: wasUnread)
        }
    }

    func clearChat(_ cInfo: ChatInfo) {
        // clear preview
        if let chat = getChat(cInfo.id) {
            self.decreaseUnreadCounter(user: self.currentUser!, chat: chat)
            chat.chatItems = []
            ChatTagsModel.shared.markChatTagRead(chat)
            chat.chatStats = ChatStats()
            chat.chatInfo = cInfo
        }
        // clear current chat
        if chatId == cInfo.id {
            im.reversedChatItems = []
            im.chatState.clear()
        }
    }

    func markChatItemsRead(_ chatIM: ItemsModel, _ cInfo: ChatInfo, _ itemIds: [ChatItem.ID], _ mentionsRead: Int) {
        if self.chatId == cInfo.id {
            var unreadItemIds: Set<ChatItem.ID> = []
            var i = 0
            var ids = Set(itemIds)
            while i < chatIM.reversedChatItems.count && !ids.isEmpty {
                let item = chatIM.reversedChatItems[i]
                if ids.contains(item.id) && item.isRcvNew {
                    markChatItemRead_(chatIM, i)
                    unreadItemIds.insert(item.id)
                    ids.remove(item.id)
                }
                i += 1
            }
            chatIM.chatState.itemsRead(unreadItemIds, chatIM.reversedChatItems.reversed())
        }
        self.unreadCollector.changeUnreadCounter(cInfo.id, by: -itemIds.count, unreadMentions: -mentionsRead)
    }

    private let unreadCollector = UnreadCollector()

    class UnreadCollector {
        private let subject = PassthroughSubject<Void, Never>()
        private var bag = Set<AnyCancellable>()
        private var unreadCounts: [ChatId: (unread: Int, mentions: Int)] = [:]

        init() {
            subject
                .debounce(for: 1, scheduler: DispatchQueue.main)
                .sink {
                    let m = ChatModel.shared
                    for (chatId, (unread, mentions)) in self.unreadCounts {
                        if unread != 0 || mentions != 0, let i = m.getChatIndex(chatId) {
                            m.changeUnreadCounter(i, by: unread, unreadMentions: mentions)
                        }
                    }
                    self.unreadCounts = [:]
                }
                .store(in: &bag)
        }

        func changeUnreadCounter(_ chatId: ChatId, by count: Int, unreadMentions: Int) {
            let (unread, mentions) = self.unreadCounts[chatId] ?? (0, 0)
            self.unreadCounts[chatId] = (unread + count, mentions + unreadMentions)
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

    private func markChatItemRead_(_ chatIM: ItemsModel, _ i: Int) {
        let meta = chatIM.reversedChatItems[i].meta
        if case .rcvNew = meta.itemStatus {
            chatIM.reversedChatItems[i].meta.itemStatus = .rcvRead
            chatIM.reversedChatItems[i].viewTimestamp = .now
            if meta.itemLive != true, let ttl = meta.itemTimed?.ttl {
                chatIM.reversedChatItems[i].meta.itemTimed?.deleteAt = .now + TimeInterval(ttl)
            }
        }
    }

    func changeUnreadCounter(_ chatIndex: Int, by count: Int, unreadMentions: Int) {
        let wasUnread = chats[chatIndex].unreadTag
        let stats = chats[chatIndex].chatStats
        chats[chatIndex].chatStats.unreadCount = stats.unreadCount + count
        chats[chatIndex].chatStats.unreadMentions = stats.unreadMentions + unreadMentions
        ChatTagsModel.shared.updateChatTagRead(chats[chatIndex], wasUnread: wasUnread)
        changeUnreadCounter(user: currentUser!, by: count)
    }

    func increaseUnreadCounter(user: any UserLike) {
        changeUnreadCounter(user: user, by: 1)
    }

    func decreaseUnreadCounter(user: any UserLike, chat: Chat) {
        let by = chat.chatInfo.chatSettings?.enableNtfs == .mentions
                ? chat.chatStats.unreadMentions
                : chat.chatStats.unreadCount
        decreaseUnreadCounter(user: user, by: by)
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
        var unread: Int = 0
        for chat in chats {
            switch chat.chatInfo.chatSettings?.enableNtfs {
            case .all: unread += chat.chatStats.unreadCount
            case .mentions: unread += chat.chatStats.unreadMentions
            default: ()
            }
        }
        for u in users {
            if !u.user.activeUser {
                unread += u.unreadCount
            }
        }
        return unread
    }

    func increaseGroupReportsCounter(_ chatId: ChatId) {
        changeGroupReportsCounter(chatId, 1)
    }

    func decreaseGroupReportsCounter(_ chatId: ChatId, by: Int = 1) {
        changeGroupReportsCounter(chatId, -by)
    }

    private func changeGroupReportsCounter(_ chatId: ChatId, _ by: Int = 0) {
        if by == 0 { return }

        if let i = getChatIndex(chatId) {
            let chat = chats[i]
            let wasReportsCount = chat.chatStats.reportsCount
            chat.chatStats.reportsCount = max(0, chat.chatStats.reportsCount + by)
            let nowReportsCount = chat.chatStats.reportsCount
            let by = wasReportsCount == 0 && nowReportsCount > 0 ? 1 : (wasReportsCount > 0 && nowReportsCount == 0) ? -1 : 0
            ChatTagsModel.shared.changeGroupReportsTag(by)
        }
    }

    // this function analyses "connected" events and assumes that each member will be there only once
    func getConnectedMemberNames(_ chatItem: ChatItem) -> (Int, [String]) {
        var count = 0
        var ns: [String] = []
        if let ciCategory = chatItem.mergeCategory,
           var i = getChatItemIndex(im, chatItem) { // TODO [knocking] review: use getCIItemsModel?
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
        if let i = getChatItemIndex(im, ci) { // TODO [knocking] review: use getCIItemsModel?
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
        let items = im.reversedChatItems
        var prevMember: GroupMember? = nil
        var memberIds: Set<Int64> = []
        for i in range {
            if i < items.count {
                if case let .groupRcv(m) = items[i].chatDir {
                    if prevMember == nil && m.groupMemberId != member.groupMemberId { prevMember = m }
                    memberIds.insert(m.groupMemberId)
                }
            } else {
                logger.error("getPrevHiddenMember: index >= count of reversed items: \(i) vs \(items.count), range: \(String(describing: range))")
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
        if id == showingInvitation?.pcc.id {
            markShowingInvitationUsed()
            dismissAllSheets()
        }
    }

    func markShowingInvitationUsed() {
        showingInvitation?.connChatUsed = true
    }

    func removeChat(_ id: String) {
        withAnimation {
            if let i = getChatIndex(id) {
                let removed = chats.remove(at: i)
                ChatTagsModel.shared.removePresetChatTags(removed.chatInfo, removed.chatStats)
                removeWallpaperFilesFromChat(removed)
            }
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

    func removeWallpaperFilesFromChat(_ chat: Chat) {
        if case let .direct(contact) = chat.chatInfo {
            removeWallpaperFilesFromTheme(contact.uiThemes)
        } else if case let .group(groupInfo, _) = chat.chatInfo {
            removeWallpaperFilesFromTheme(groupInfo.uiThemes)
        }
    }

    func removeWallpaperFilesFromAllChats(_ user: User) {
        // Currently, only removing everything from currently active user is supported. Inactive users are TODO
        if user.userId == currentUser?.userId {
            chats.forEach {
                removeWallpaperFilesFromChat($0)
            }
        }
    }
}

struct ShowingInvitation {
    var pcc: PendingContactConnection
    var connChatUsed: Bool
}

struct NTFContactRequest {
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

    var unreadTag: Bool {
        switch chatInfo.chatSettings?.enableNtfs {
        case .all: chatStats.unreadChat || chatStats.unreadCount > 0
        case .mentions: chatStats.unreadChat || chatStats.unreadMentions > 0
        default: chatStats.unreadChat
        }
    }

    var id: ChatId { get { chatInfo.id } }

    var viewId: String { get { "\(chatInfo.id) \(created.timeIntervalSince1970)" } }

    var supportUnreadCount: Int {
        switch chatInfo {
        case let .group(groupInfo, _):
            if groupInfo.canModerate {
                return groupInfo.membersRequireAttention
            } else {
                return groupInfo.membership.supportChat?.unread ?? 0
            }
        default: return 0
        }
    }

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
