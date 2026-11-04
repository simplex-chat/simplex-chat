import XCTest
@testable import SimpleX
@testable import SimpleXChat

final class ChatItemsLoaderTests: XCTestCase {

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
    private func load(_ page: Chat, _ pagination: ChatPagination, _ navInfo: NavigationInfo = NavigationInfo(), during events: () async -> Void) async {
        let itemUpdates = ChatModel.ItemUpdatesWhileLoading()
        ChatModel.shared.startRecordingItemUpdates(itemUpdates)
        await events()
        ChatModel.shared.stopRecordingItemUpdates(itemUpdates)
        processLoadedChat(page, navInfo, ItemsModel.shared, pagination, nil, itemUpdates)
    }

    @MainActor
    private func openingChat(groupId: Int64, itemIds: [Int64]) -> Chat {
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

    private func receivedItem(_ id: Int64) -> ChatItem {
        ChatItem.getSample(id, .groupRcv(groupMember: GroupMember.sampleData), Date(timeIntervalSince1970: TimeInterval(id)), "", .rcvNew)
    }

    private func itemIds() -> [Int64] {
        ItemsModel.shared.reversedChatItems.reversed().map { $0.id }
    }

    private func item(_ id: Int64) -> ChatItem? {
        ItemsModel.shared.reversedChatItems.first { $0.id == id }
    }
}
