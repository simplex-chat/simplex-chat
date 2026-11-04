import XCTest
@testable import SimpleX
@testable import SimpleXChat

final class ChatItemsLoaderTests: XCTestCase {
    private var savedChats: [ChatData] = []
    private var savedUsers: [UserInfo] = []
    private var savedCurrentUser: User?
    private var savedChatId: ChatId?
    private var savedOpenAroundItemId: ChatItem.ID?
    private var savedChatToTop: String?
    private var savedItems: [ChatItem] = []
    private var savedItemAdded = false

    override func setUp() async throws {
        await saveSharedState()
    }

    override func tearDown() async throws {
        await restoreSharedState()
    }

    @MainActor
    func testItemReceivedWhileChatOpensIsShown() async {
        let opening = openingChat(groupId: 1001, itemIds: [101, 102])
        await load(opening, .initial(count: 2)) {
            ChatModel.shared.addChatItem(opening.chatInfo, receivedItem(103))
        }
        XCTAssertEqual(itemIds(), [101, 102, 103])
    }

    @MainActor
    func testItemReceivedWhileOpenedChatReloadsIsKept() async {
        let opened = openedChat(groupId: 1002, itemIds: [101, 102])
        await load(opened, .initial(count: 2)) {
            ChatModel.shared.addChatItem(opened.chatInfo, receivedItem(103))
        }
        XCTAssertEqual(itemIds(), [101, 102, 103])
    }

    @MainActor
    func testItemDeletedWhileChatOpensIsNotShown() async {
        let opening = openingChat(groupId: 1003, itemIds: [101, 102])
        let received = receivedItem(103)
        let pageReadBeforeDeletion = opening.copy(chatItems: opening.chatItems + [received])
        await load(pageReadBeforeDeletion, .initial(count: 3)) {
            ChatModel.shared.addChatItem(opening.chatInfo, received)
            ChatModel.shared.removeChatItem(opening.chatInfo, received)
        }
        XCTAssertEqual(itemIds(), [101, 102])
    }

    @MainActor
    func testItemReadWhileOpenedChatReloadsStaysRead() async {
        let opened = openedChat(groupId: 1004, itemIds: [101, 102])
        let received = receivedItem(103)
        let pageReadBeforeMark = opened.copy(chatItems: opened.chatItems + [received])
        await load(pageReadBeforeMark, .initial(count: 3)) {
            ChatModel.shared.addChatItem(opened.chatInfo, received)
            ChatModel.shared.markChatItemsRead(ItemsModel.shared, opened.chatInfo, [103], 0)
        }
        XCTAssertEqual(item(103)?.isRcvNew, false)
    }

    @MainActor
    func testGroupItemDeletedWhileChatOpensIsShownDeleted() async {
        let opening = openingChat(groupId: 1005, itemIds: [101, 102])
        let groupInfo = opening.chatInfo.groupInfo!
        let user = UserRef(userId: User.sampleData.userId, localDisplayName: User.sampleData.localDisplayName)
        let pageReadBeforeDeletion = opening.copy(chatItems: opening.chatItems + [receivedItem(103)])
        await load(pageReadBeforeDeletion, .initial(count: 3)) {
            await groupChatItemsDeleted(user, groupInfo, [103], GroupMember.sampleData)
        }
        guard case .deleted? = item(103)?.meta.itemDeleted else {
            return XCTFail("item 103 is not marked deleted")
        }
    }

    @MainActor
    func testItemDeletedBeforeItsAdditionWhileChatOpensIsNotShown() async {
        let opening = openingChat(groupId: 1006, itemIds: [101, 102])
        let received = receivedItem(103)
        await load(opening, .initial(count: 2)) {
            ChatModel.shared.removeChatItem(opening.chatInfo, received)
            ChatModel.shared.addChatItem(opening.chatInfo, received)
        }
        XCTAssertEqual(itemIds(), [101, 102])
    }

    @MainActor
    func testItemReceivedWhileChatOpensAtOlderPageIsNotAdded() async {
        let opening = openingChat(groupId: 1007, itemIds: [101, 102])
        await load(opening, .initial(count: 2), NavigationInfo(afterUnread: 2, afterTotal: 2)) {
            ChatModel.shared.addChatItem(opening.chatInfo, receivedItem(104))
        }
        XCTAssertEqual(itemIds(), [101, 102])
    }

    @MainActor
    func testItemReceivedAfterInstallBeforeChatIsShownIsShown() async {
        let opening = openingChat(groupId: 1008, itemIds: [101, 102])
        ItemsModel.shared.openingChatId = opening.id
        await load(opening, .initial(count: 2)) {}
        ChatModel.shared.addChatItem(opening.chatInfo, receivedItem(103))
        XCTAssertEqual(itemIds(), [101, 102, 103])
    }

    @MainActor
    func testRecordIsStoppedAtInstallWhenChatWasShownAndClosed() async {
        let opening = openingChat(groupId: 1009, itemIds: [101, 102])
        await load(opening, .initial(count: 2)) {}
        XCTAssertNil(ItemsModel.shared.openingItemUpdates)
        ChatModel.shared.addChatItem(opening.chatInfo, receivedItem(103))
        XCTAssertEqual(itemIds(), [101, 102])
    }

    @MainActor
    func testItemReceivedWhileChatOpensAtItemIsShown() async {
        let opening = openingChat(groupId: 1010, itemIds: [101, 102])
        await load(opening, .around(chatItemId: 101, count: 2), openAroundItemId: 101) {
            ChatModel.shared.addChatItem(opening.chatInfo, receivedItem(103))
        }
        XCTAssertEqual(itemIds(), [101, 102, 103])
    }

    @MainActor
    func testItemUpdatedWhileOpenedChatReloadsStaysRead() async {
        let opened = openedChat(groupId: 1011, itemIds: [101, 102])
        let pageReadAfterMark = opened.copy(chatItems: opened.chatItems + [receivedItem(103, .rcvRead)])
        await load(pageReadAfterMark, .initial(count: 3)) {
            ChatModel.shared.updateChatItem(opened.chatInfo, receivedItem(103))
        }
        XCTAssertEqual(item(103)?.isRcvNew, false)
    }

    @MainActor
    func testItemUpsertedWhileOpenedChatReloadsStaysDeleted() async {
        let opened = openedChat(groupId: 1012, itemIds: [101, 102])
        let pageReadAfterDeletion = opened.copy(chatItems: opened.chatItems + [receivedItem(103, itemDeleted: .deleted(deletedTs: nil))])
        await load(pageReadAfterDeletion, .initial(count: 3)) {
            _ = ChatModel.shared.upsertChatItem(opened.chatInfo, receivedItem(103))
        }
        XCTAssertNotNil(item(103)?.meta.itemDeleted)
    }

    @MainActor
    func testChatClearedWhileOpenedChatReloadsIsEmpty() async {
        let opened = openedChat(groupId: 1013, itemIds: [101, 102])
        await load(opened, .initial(count: 2)) {
            ChatModel.shared.clearChat(opened.chatInfo)
        }
        XCTAssertEqual(itemIds(), [])
    }

    @MainActor
    private func load(_ page: Chat, _ pagination: ChatPagination, _ navInfo: NavigationInfo = NavigationInfo(), openAroundItemId: ChatItem.ID? = nil, during events: @MainActor () async -> Void) async {
        let itemUpdates = ChatModel.ItemUpdatesWhileLoading()
        ChatModel.shared.startRecordingItemUpdates(itemUpdates)
        await events()
        processLoadedChat(page, navInfo, ItemsModel.shared, pagination, openAroundItemId, itemUpdates)
        ChatModel.shared.updateChats(savedChats)
        if ItemsModel.shared.openingItemUpdates !== itemUpdates {
            ChatModel.shared.stopRecordingItemUpdates(itemUpdates)
        }
    }

    @MainActor
    private func saveSharedState() {
        let m = ChatModel.shared
        savedChats = m.chats.map { ChatData(chatInfo: $0.chatInfo, chatItems: $0.chatItems, chatStats: $0.chatStats) }
        savedUsers = m.users
        savedCurrentUser = m.currentUser
        savedChatId = m.chatId
        savedOpenAroundItemId = m.openAroundItemId
        savedChatToTop = m.chatToTop
        savedItems = ItemsModel.shared.reversedChatItems
        savedItemAdded = ItemsModel.shared.itemAdded
    }

    @MainActor
    private func restoreSharedState() {
        let m = ChatModel.shared
        let im = ItemsModel.shared
        im.stopApplyingItemUpdates()
        im.openingChatId = nil
        im.reversedChatItems = savedItems
        im.itemAdded = savedItemAdded
        im.chatState.clear()
        im.preloadState.clear()
        m.chatId = savedChatId
        m.openAroundItemId = savedOpenAroundItemId
        m.chatToTop = savedChatToTop
        m.currentUser = savedCurrentUser
        m.users = savedUsers
        m.updateChats(savedChats)
    }

    @MainActor
    private func openingChat(groupId: Int64, itemIds: [Int64]) -> Chat {
        ItemsModel.shared.stopApplyingItemUpdates()
        ItemsModel.shared.openingChatId = nil
        ChatModel.shared.currentUser = User.sampleData
        ChatModel.shared.chatId = nil
        ItemsModel.shared.reversedChatItems = []
        ItemsModel.shared.chatState.clear()
        var groupInfo = GroupInfo.sampleData
        groupInfo.groupId = groupId
        let items = itemIds.map { ChatItem.getSample($0, .groupSnd, Date(timeIntervalSince1970: TimeInterval($0)), "") }
        return Chat(chatInfo: .group(groupInfo: groupInfo, groupChatScope: nil), chatItems: items)
    }

    @MainActor
    private func openedChat(groupId: Int64, itemIds: [Int64]) -> Chat {
        let chat = openingChat(groupId: groupId, itemIds: itemIds)
        ChatModel.shared.chatId = chat.id
        ItemsModel.shared.reversedChatItems = chat.chatItems.reversed()
        return chat
    }

    private func receivedItem(_ id: Int64, _ status: CIStatus = .rcvNew, itemDeleted: CIDeleted? = nil) -> ChatItem {
        ChatItem.getSample(id, .groupRcv(groupMember: GroupMember.sampleData), Date(timeIntervalSince1970: TimeInterval(id)), "", status, itemDeleted: itemDeleted)
    }

    private func itemIds() -> [Int64] {
        ItemsModel.shared.reversedChatItems.reversed().map { $0.id }
    }

    private func item(_ id: Int64) -> ChatItem? {
        ItemsModel.shared.reversedChatItems.first { $0.id == id }
    }
}
