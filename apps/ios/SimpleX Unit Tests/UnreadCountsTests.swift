import XCTest
import UserNotifications
@testable import SimpleX
@testable import SimpleXChat

final class UnreadCountsTests: XCTestCase {
    private let ntfManager = NtfManager.shared
    private let sendCmd = sendSimpleXCmdStr
    private var badges: [Int] = []

    @MainActor
    override func setUp() async throws {
        NtfManager.shared = BadgeRecordingNtfManager { [unowned self] in self.badges.append($0) }
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
    func testStatusOfItemNotLoadedInsertsNothing() async throws {
        let events = try recordedEvents("counts/direct-statuses")
        try listChat(events[0], preview: try items(events[0])[0].chatItem)
        try openChat(events[0])
        await processReceivedMsg(events[0])
        await processReceivedMsg(events[1])
        XCTAssertEqual(loadedTexts(), [])
        guard case .sndRcvd = ChatModel.shared.chats.first?.chatItems.last?.meta.itemStatus else { return XCTFail("preview status is not received") }
    }

    @MainActor
    func testDeletionMarkOfItemNotLoadedInsertsNothing() async throws {
        let events = try recordedEvents("counts/group-marked")
        try listChat(events[0])
        await processReceivedMsg(events[0])
        await processReceivedMsg(events[1])
        try openChat(events[0])
        await processReceivedMsg(events[2])
        XCTAssertEqual(loadedTexts(), [])
        try await assertCounts(2, 1, 2)
    }

    @MainActor
    func testRemovedUnreadMentionLowersBothCounters() async throws {
        let events = try recordedEvents("counts/group-marked")
        let removed = try recordedEvents("counts/group-removed")
        try listChat(events[0])
        for event in events { await processReceivedMsg(event) }
        try await assertCounts(2, 1, 2)
        await processReceivedMsg(removed[0])
        try await assertCounts(1, 0, 1)
    }

    @MainActor
    func testProfileCountFollowsMentionsOnlyGroup() async throws {
        let events = try recordedEvents("updates/group-mention")
        try listChat(events[0])
        await processReceivedMsg(events[0])
        try await assertCounts(1, 0, 0)
        await processReceivedMsg(events[1])
        try await assertCounts(1, 1, 1)
        await processReceivedMsg(events[2])
        try await assertCounts(1, 0, 0)
        XCTAssertEqual(badges.last, 0)
    }

    @MainActor
    func testProfileCountFollowsMutedChat() async throws {
        let events = try recordedEvents("counts/direct-muted")
        try listChat(events[0])
        for event in events { await processReceivedMsg(event) }
        try await assertCounts(2, 0, 0)
        guard case var .direct(contact) = try chatInfo(events[0]) else { return XCTFail("not a contact") }
        contact.chatSettings.enableNtfs = .all
        ChatModel.shared.updateContact(contact)
        try await assertCounts(2, 0, 2)
        XCTAssertEqual(badges.last, 2)
    }

    @MainActor
    func testChatOutsideListCountsNewMessage() async throws {
        let events = try recordedEvents("counts/direct-muted")
        await processReceivedMsg(events[0])
        XCTAssertEqual(previewText(), "hello")
        try await assertCounts(1, 0, 0)
    }

    @MainActor
    func testPendingMemberRowCountsAdminMessages() async throws {
        let events = try recordedEvents("counts/pending-support")
        try listChat(events[0])
        await processReceivedMsg(events[0])
        XCTAssertEqual(previewText(), "5")
        XCTAssertEqual(rowCounts(), [1, 0])
        await processReceivedMsg(events[1])
        XCTAssertEqual(previewText(), "@cath 5")
        XCTAssertEqual(rowCounts(), [1, 1])
        await processReceivedMsg(events[2])
        XCTAssertEqual(rowCounts(), [2, 1])
        await processReceivedMsg(events[3])
        XCTAssertEqual(rowCounts(), [1, 1])
        XCTAssertNotEqual(previewText(), "6")
        XCTAssertEqual(ChatModel.shared.totalUnreadCountForAllUsers(), 1)
        try respond("counts/pending-read")
        let chat = try XCTUnwrap(ChatModel.shared.chats.first)
        await markChatRead(ItemsModel.shared, chat)
        XCTAssertEqual(rowCounts(), [0, 0])
        XCTAssertEqual(ChatModel.shared.totalUnreadCountForAllUsers(), 0)
    }

    @MainActor
    func testModeratorSupportMessageIsUnread() async throws {
        let events = try recordedEvents("counts/moderator-support")
        try listChat(events[0])
        await processReceivedMsg(events[0])
        XCTAssertEqual(rowCounts(), [0, 0])
        XCTAssertEqual(ChatModel.shared.totalUnreadCountForAllUsers(), 0)
        XCTAssertTrue(try XCTUnwrap(ChatModel.shared.chats.first).unreadTag)
    }

    @MainActor
    func testReadingSupportChatKeepsGroupRow() async throws {
        let events = try recordedEvents("counts/moderator-support")
        try listChat(events[1])
        for event in events { await processReceivedMsg(event) }
        try await assertCounts(1, 0, 1)
        try respond("counts/moderator-read")
        let support = try items(events[0])[0]
        await apiMarkChatItemsRead(ItemsModel.shared, support.chatInfo, [support.chatItem.id], mentionsRead: 0)
        try await assertCounts(1, 0, 1)
        XCTAssertEqual(try XCTUnwrap(ChatModel.shared.chats.first).supportUnreadCount, 0)
    }

    private func recordedEvents(_ fixture: String) throws -> [ChatEvent] {
        try XCTUnwrap(try fixtureJSON(fixture) as? [Any]).map { json in
            let decoded: APIResult<ChatEvent> = decodeAPIResult(try JSONSerialization.data(withJSONObject: singleFieldJSON(json)))
            let event: ChatEvent? = if case let .result(r) = decoded { r } else { nil }
            return try XCTUnwrap(event, "event not decoded: \(decoded.responseType)")
        }
    }

    private func respond(_ fixture: String) throws {
        let commands = try XCTUnwrap((try fixtureJSON(fixture) as? [String: [String: Any]])?["commands"])
        sendSimpleXCmdStr = { cmd, _, _ in
            guard let response = commands[cmd] else { return nil }
            return try? JSONSerialization.data(withJSONObject: singleFieldJSON(response))
        }
    }

    private func items(_ event: ChatEvent) throws -> [AChatItem] {
        let items: [AChatItem]? = switch event {
        case let .newChatItems(_, items): items
        case let .chatItemsStatusesUpdated(_, items): items
        case let .chatItemsDeleted(_, deletions, _): deletions.map { $0.deletedChatItem }
        case let .chatItemUpdated(_, update): [AChatItem(chatInfo: update.chatInfo, chatItem: update.chatItem)]
        default: nil
        }
        return try XCTUnwrap(items, "not an item event: \(event.responseType)")
    }

    private func chatInfo(_ event: ChatEvent) throws -> ChatInfo {
        try XCTUnwrap(try items(event).first?.chatInfo)
    }

    @MainActor
    private func listChat(_ event: ChatEvent, preview: ChatItem? = nil) throws {
        let cInfo: ChatInfo = switch try chatInfo(event) {
        case let .group(groupInfo, _): .group(groupInfo: groupInfo, groupChatScope: nil)
        case let other: other
        }
        ChatModel.shared.addChat(Chat(chatInfo: cInfo, chatItems: preview.map { [$0] } ?? []))
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
    private func rowCounts() -> [Int] {
        let stats = ChatModel.shared.chats.first?.unreadStats
        return [stats?.unreadCount ?? -1, stats?.unreadMentions ?? -1]
    }

    @MainActor
    private func assertCounts(_ unread: Int, _ mentions: Int, _ userUnread: Int, file: StaticString = #filePath, line: UInt = #line) async throws {
        func counts() -> [Int] { rowCounts() + [ChatModel.shared.totalUnreadCountForAllUsers()] }
        try? await waitFor { counts() == [unread, mentions, userUnread] }
        XCTAssertEqual(counts(), [unread, mentions, userUnread], file: file, line: line)
    }
}

private final class BadgeRecordingNtfManager: NtfManager {
    private let record: (Int) -> Void

    init(record: @escaping (Int) -> Void) {
        self.record = record
        super.init()
    }

    override func setNtfBadgeCount(_ count: Int) {
        record(count)
    }

    override func addNotification(_ content: UNMutableNotificationContent) {}
}
