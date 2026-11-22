import XCTest
import UserNotifications
@testable import SimpleX
@testable import SimpleXChat

final class MessagePriorityTests: XCTestCase {
    private let ntfManager = NtfManager.shared
    private let sendCmd = sendSimpleXCmdStr
    private var notified: [String] = []
    private let tag: Int64 = 1

    @MainActor
    override func setUp() async throws {
        NtfManager.shared = RecordingNtfManager { [unowned self] in self.notified.append($0) }
        ChatModel.shared.currentUser = User.sampleData
        ChatModel.shared.users = [UserInfo(user: User.sampleData, unreadCount: 0)]
        clearLabels()
    }

    @MainActor
    override func tearDown() async throws {
        for chat in ChatModel.shared.chats { ChatModel.shared.removeChat(chat.id) }
        ChatModel.shared.currentUser = nil
        ChatModel.shared.users = []
        clearLabels()
        NtfManager.shared = ntfManager
        UNUserNotificationCenter.current().delegate = ntfManager
        sendSimpleXCmdStr = sendCmd
    }

    @MainActor
    func testSilentMessagesDoNotNotifyAndAreLeftOutOfTotals() async throws {
        let events = try recordedEvents("priority/silent")
        try listChat(events[0])
        await processReceivedMsg(events[0])
        await processReceivedMsg(events[1])
        try await assertCounts(unread: 2, mentions: 0, silent: 2, highPriority: 0, user: 0)
        XCTAssertEqual(notified, [])
        let chat = try XCTUnwrap(ChatModel.shared.chats.first)
        XCTAssertEqual(chat.unreadTag, .silent)
        XCTAssertTrue(chat.chatStats.allUnreadSilent)
        XCTAssertEqual(labels(), [0, 1, 0])
    }

    @MainActor
    func testChatListWithOnlySilentUnread() async throws {
        let commands = try XCTUnwrap((try fixtureJSON("priority/chats-silent") as? [String: [String: Any]])?["commands"])
        sendSimpleXCmdStr = { cmd, _, _ in
            guard let response = commands[cmd] else { return nil }
            return try? JSONSerialization.data(withJSONObject: singleFieldJSON(response))
        }
        ChatModel.shared.updateChats(try await apiGetChatsAsync())
        ChatTagsModel.shared.updateChatTags(ChatModel.shared.chats)
        let users = try await listUsersAsync()
        let group = try XCTUnwrap(ChatModel.shared.chats.first { $0.chatInfo.chatType == .group })
        let direct = try XCTUnwrap(ChatModel.shared.chats.first { $0.chatInfo.chatType == .direct })
        XCTAssertEqual([group.chatStats.unreadCount, group.chatStats.unreadSilent], [2, 2])
        XCTAssertEqual(group.unreadTag, .silent)
        XCTAssertTrue(group.chatStats.allUnreadSilent)
        XCTAssertFalse(direct.chatStats.allUnreadSilent)
        XCTAssertEqual(labels(), [0, 1, 0])
        XCTAssertEqual(ChatModel.shared.totalUnreadCountForAllUsers(), 1)
        XCTAssertEqual(users.map(\.unreadCount), [1])
    }

    @MainActor
    func testHighPriorityMessageNotifiesWithBell() async throws {
        let events = try recordedEvents("priority/high")
        try listChat(events[0])
        await processReceivedMsg(events[0])
        await processReceivedMsg(events[1])
        try await assertCounts(unread: 2, mentions: 1, silent: 0, highPriority: 1, user: 2)
        XCTAssertEqual(notified, ["hello", "done"])
        let chat = try XCTUnwrap(ChatModel.shared.chats.first)
        XCTAssertEqual(chat.unreadTag, .highPriority)
        XCTAssertEqual(chat.chatItems.last?.meta.highPriority, true)
        XCTAssertEqual(labels(), [1, 0, 1])
    }

    @MainActor
    func testRaiseNotifiesAndMovesCounters() async throws {
        let events = try recordedEvents("priority/raise")
        try listChat(events[0])
        await processReceivedMsg(events[0])
        try await assertCounts(unread: 1, mentions: 0, silent: 0, highPriority: 0, user: 1)
        await processReceivedMsg(events[1])
        try await assertCounts(unread: 1, mentions: 1, silent: 0, highPriority: 1, user: 1)
        XCTAssertEqual(notified, ["almost there", "almost there"])
        await processReceivedMsg(events[2])
        try await assertCounts(unread: 2, mentions: 1, silent: 1, highPriority: 1, user: 1)
        await processReceivedMsg(events[3])
        try await assertCounts(unread: 2, mentions: 2, silent: 0, highPriority: 2, user: 2)
        XCTAssertEqual(notified, ["almost there", "almost there", "compiled"])
        XCTAssertEqual(labels(), [1, 0, 1])
    }

    @MainActor
    func testLowerPriorityMovesCountersWithoutNotification() async throws {
        let events = try recordedEvents("priority/lower")
        try listChat(events[0])
        await processReceivedMsg(events[0])
        try await assertCounts(unread: 1, mentions: 1, silent: 0, highPriority: 1, user: 1)
        await processReceivedMsg(events[1])
        try await assertCounts(unread: 1, mentions: 0, silent: 0, highPriority: 0, user: 1)
        await processReceivedMsg(events[2])
        try await assertCounts(unread: 2, mentions: 1, silent: 0, highPriority: 1, user: 2)
        await processReceivedMsg(events[3])
        try await assertCounts(unread: 2, mentions: 0, silent: 1, highPriority: 0, user: 1)
        XCTAssertEqual(notified, ["answer", "alert"])
    }

    @MainActor
    func testRaiseInAnotherProfileNotifiesAndCountsForIt() async throws {
        let events = try recordedEvents("priority/raise")
        ChatModel.shared.currentUser = otherUser()
        for event in events { await processReceivedMsg(event) }
        XCTAssertEqual(notified, ["almost there", "almost there", "compiled"])
        XCTAssertEqual(ChatModel.shared.users.first?.unreadCount, 2)
    }

    @MainActor
    private func labels() -> [Int] {
        let tm = ChatTagsModel.shared
        return [tm.unreadTags[tag] ?? 0, tm.silentTags[tag] ?? 0, tm.priorityTags[tag] ?? 0]
    }

    @MainActor
    private func clearLabels() {
        ChatTagsModel.shared.unreadTags = [:]
        ChatTagsModel.shared.silentTags = [:]
        ChatTagsModel.shared.priorityTags = [:]
    }

    @MainActor
    private func assertCounts(unread: Int, mentions: Int, silent: Int, highPriority: Int, user: Int, file: StaticString = #filePath, line: UInt = #line) async throws {
        let expected = [unread, mentions, silent, highPriority, user]
        func counts() -> [Int] {
            let stats = ChatModel.shared.chats.first?.chatStats
            return [stats?.unreadCount ?? -1, stats?.unreadMentions ?? -1, stats?.unreadSilent ?? -1, stats?.unreadHighPriority ?? -1, ChatModel.shared.users.first?.unreadCount ?? -1]
        }
        try? await waitFor { counts() == expected }
        XCTAssertEqual(counts(), expected, file: file, line: line)
    }
}
