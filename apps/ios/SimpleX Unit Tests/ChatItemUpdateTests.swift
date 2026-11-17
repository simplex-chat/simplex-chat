import XCTest
@testable import SimpleX
@testable import SimpleXChat

final class ChatItemUpdateTests: XCTestCase {
    @MainActor
    override func setUp() async throws {
        ChatModel.shared.currentUser = User.sampleData
        ChatModel.shared.users = [UserInfo(user: User.sampleData, unreadCount: 0)]
    }

    @MainActor
    override func tearDown() async throws {
        for chat in ChatModel.shared.chats { ChatModel.shared.removeChat(chat.id) }
        ChatModel.shared.im.reversedChatItems = []
        ChatModel.shared.chatId = nil
        ChatModel.shared.currentUser = nil
        ChatModel.shared.users = []
    }

    @MainActor
    func testEditReplacesLoadedItem() async throws {
        let events = try recordedEvents("updates/direct-edit")
        try listChat(events[0])
        try openChat(events[0])
        await processReceivedMsg(events[0])
        XCTAssertEqual(loadedTexts(), ["hello"])
        await processReceivedMsg(events[1])
        XCTAssertEqual(loadedTexts(), ["hello there"])
        XCTAssertEqual(previewText(), "hello there")
        try await assertCounts(1, 0, 1)
    }

    @MainActor
    func testUpdateOfItemNotLoadedInsertsNothing() async throws {
        let events = try recordedEvents("updates/direct-edit")
        try listChat(events[0])
        await processReceivedMsg(events[0])
        try openChat(events[0])
        await processReceivedMsg(events[1])
        XCTAssertEqual(loadedTexts(), [])
        XCTAssertEqual(previewText(), "hello there")
        await processReceivedMsg(events[2])
        XCTAssertEqual(loadedTexts(), ["how are you"])
        await processReceivedMsg(events[3])
        XCTAssertEqual(loadedTexts(), ["how are you"])
        XCTAssertEqual(previewText(), "how are you")
        try await assertCounts(2, 0, 2)
    }

    @MainActor
    func testCountersFollowPreviousAndNewItem() async throws {
        let events = try recordedEvents("updates/group-mention")
        try listChat(events[0])
        await processReceivedMsg(events[0])
        try await assertCounts(1, 0, 1)
        await processReceivedMsg(events[1])
        try await assertCounts(1, 1, 1)
        await processReceivedMsg(events[2])
        try await assertCounts(1, 0, 1)
    }

    @MainActor
    func testMentionEditsInAnotherProfileChangeItsUnreadCount() async throws {
        let events = try recordedEvents("updates/group-mention")
        var otherUser = User.sampleData
        otherUser.userId = 2
        ChatModel.shared.currentUser = otherUser
        await processReceivedMsg(events[0])
        await processReceivedMsg(events[1])
        XCTAssertEqual(ChatModel.shared.users.first?.unreadCount, 1)
        await processReceivedMsg(events[2])
        XCTAssertEqual(ChatModel.shared.users.first?.unreadCount, 0)
    }

    @MainActor
    func testUpdateOfChatNotListedAddsIt() async throws {
        let events = try recordedEvents("updates/direct-edit")
        await processReceivedMsg(events[1])
        XCTAssertEqual(previewText(), "hello there")
    }

    @MainActor
    func testRecreatedItemIsCounted() async throws {
        let events = try recordedEvents("updates/direct-recreated")
        try listChat(events[0])
        await processReceivedMsg(events[0])
        XCTAssertEqual(previewText(), "hello again")
        try await assertCounts(1, 0, 1)
    }

    private func recordedEvents(_ fixture: String) throws -> [ChatEvent] {
        try XCTUnwrap(try fixtureJSON(fixture) as? [Any]).map { json in
            let decoded: APIResult<ChatEvent> = decodeAPIResult(try JSONSerialization.data(withJSONObject: singleFieldJSON(json)))
            let event: ChatEvent? = if case let .result(r) = decoded { r } else { nil }
            return try XCTUnwrap(event, "event not decoded: \(decoded.responseType)")
        }
    }

    private func chatInfo(_ event: ChatEvent) throws -> ChatInfo {
        let chatItems: [AChatItem]? = if case let .newChatItems(_, items) = event { items } else { nil }
        return try XCTUnwrap(chatItems?.first?.chatInfo, "not a new item: \(event.responseType)")
    }

    @MainActor
    private func listChat(_ event: ChatEvent) throws {
        ChatModel.shared.addChat(Chat(chatInfo: try chatInfo(event), chatItems: []))
    }

    @MainActor
    private func openChat(_ event: ChatEvent) throws {
        ChatModel.shared.chatId = try chatInfo(event).id
    }

    @MainActor
    private func loadedTexts() -> [String] {
        ChatModel.shared.im.reversedChatItems.reversed().map(\.text)
    }

    @MainActor
    private func previewText() -> String? {
        ChatModel.shared.chats.first?.chatItems.last?.text
    }

    @MainActor
    private func assertCounts(_ unread: Int, _ mentions: Int, _ userUnread: Int, file: StaticString = #filePath, line: UInt = #line) async throws {
        func counts() -> [Int] {
            let stats = ChatModel.shared.chats.first?.chatStats
            return [stats?.unreadCount ?? -1, stats?.unreadMentions ?? -1, ChatModel.shared.users.first?.unreadCount ?? -1]
        }
        try? await waitFor { counts() == [unread, mentions, userUnread] }
        XCTAssertEqual(counts(), [unread, mentions, userUnread], file: file, line: line)
    }
}
