import XCTest
import UserNotifications
@testable import SimpleX
@testable import SimpleXChat

final class ChatItemUpdateTests: XCTestCase {
    private let ntfManager = NtfManager.shared
    private let sendCmd = sendSimpleXCmdStr
    private var notified: [String] = []
    private var sent: [String] = []

    @MainActor
    override func setUp() async throws {
        NtfManager.shared = RecordingNtfManager { [unowned self] in self.notified.append($0) }
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
        NtfManager.shared = ntfManager
        UNUserNotificationCenter.current().delegate = ntfManager
        sendSimpleXCmdStr = sendCmd
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
        ChatModel.shared.currentUser = otherUser()
        await processReceivedMsg(events[0])
        await processReceivedMsg(events[1])
        XCTAssertEqual(ChatModel.shared.users.first?.unreadCount, 1)
        await processReceivedMsg(events[2])
        XCTAssertEqual(ChatModel.shared.users.first?.unreadCount, 0)
        XCTAssertEqual(notified, [])
    }

    @MainActor
    func testUpdateOfChatNotListedAddsItAndNotifies() async throws {
        let events = try recordedEvents("updates/direct-edit")
        await processReceivedMsg(events[1])
        XCTAssertEqual(previewText(), "hello there")
        XCTAssertEqual(notified, ["hello there"])
    }

    @MainActor
    func testRecreatedItemIsCountedAndNotified() async throws {
        let events = try recordedEvents("updates/direct-recreated")
        try listChat(events[0])
        await processReceivedMsg(events[0])
        XCTAssertEqual(previewText(), "hello again")
        XCTAssertEqual(notified, ["hello again"])
        try await assertCounts(1, 0, 1)
    }

    @MainActor
    func testEditInAnotherProfileDoesNotNotify() async throws {
        let events = try recordedEvents("updates/direct-edit")
        ChatModel.shared.currentUser = otherUser()
        await processReceivedMsg(events[0])
        await processReceivedMsg(events[1])
        XCTAssertEqual(notified, ["hello"])
        XCTAssertEqual(ChatModel.shared.users.first?.unreadCount, 1)
    }

    @MainActor
    func testMissedCallDoesNotNotify() async throws {
        let events = try recordedEvents("updates/missed-call")
        try listChat(events[0])
        await processReceivedMsg(events[0])
        await processReceivedMsg(events[1])
        guard case let .rcvCall(status, _)? = ChatModel.shared.chats.first?.chatItems.last?.content else { return XCTFail("no call item") }
        XCTAssertEqual(status, .missed)
        XCTAssertEqual(notified, [])
        try await assertCounts(1, 0, 1)
    }

    @MainActor
    func testMissedCallInAnotherProfileDoesNotNotify() async throws {
        let events = try recordedEvents("updates/missed-call")
        ChatModel.shared.currentUser = otherUser()
        await processReceivedMsg(events[0])
        await processReceivedMsg(events[1])
        XCTAssertEqual(notified, [])
        XCTAssertEqual(ChatModel.shared.users.first?.unreadCount, 1)
    }

    @MainActor
    func testUpdateResponseHasUpdatedItem() async throws {
        let commands = try XCTUnwrap((try fixtureJSON("updates/update-response") as? [String: [String: Any]])?["commands"])
        let response = try XCTUnwrap(commands.values.first)
        sendSimpleXCmdStr = { [unowned self] cmd, _, _ in
            self.sent.append(cmd)
            return try? JSONSerialization.data(withJSONObject: singleFieldJSON(response))
        }
        let item = try await apiUpdateChatItem(type: .direct, id: 2, scope: nil, itemId: 8, updatedMessage: UpdatedMessage(msgContent: .text("hello there"), mentions: [:]))
        XCTAssertEqual(sent.count, 1)
        XCTAssertTrue(sent.first?.hasPrefix("/_update item @2 8 ") ?? false, "\(sent)")
        XCTAssertEqual(item.id, 8)
        XCTAssertEqual(item.text, "hello there")
    }

    private func recordedEvents(_ fixture: String) throws -> [ChatEvent] {
        try XCTUnwrap(try fixtureJSON(fixture) as? [Any]).map { json in
            let decoded: APIResult<ChatEvent> = decodeAPIResult(try JSONSerialization.data(withJSONObject: singleFieldJSON(json)))
            let event: ChatEvent? = if case let .result(r) = decoded { r } else { nil }
            return try XCTUnwrap(event, "event not decoded: \(decoded.responseType)")
        }
    }

    private func otherUser() -> User {
        var user = User.sampleData
        user.userId = 2
        return user
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

private final class RecordingNtfManager: NtfManager {
    private let record: (String) -> Void

    init(record: @escaping (String) -> Void) {
        self.record = record
        super.init()
    }

    override func addNotification(_ content: UNMutableNotificationContent) {
        record(content.body)
    }
}
