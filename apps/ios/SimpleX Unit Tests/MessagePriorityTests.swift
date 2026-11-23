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
        XCTAssertEqual(chat.unreadTag, .read)
        XCTAssertTrue(chat.chatStats.allUnreadSilent)
        XCTAssertEqual(labels(), [0, 0])
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
        XCTAssertEqual(group.unreadTag, .read)
        XCTAssertTrue(group.chatStats.allUnreadSilent)
        XCTAssertFalse(direct.chatStats.allUnreadSilent)
        XCTAssertEqual(labels(), [0, 0])
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
        XCTAssertEqual(labels(), [1, 1])
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
        XCTAssertEqual(labels(), [1, 1])
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

    func testExtensionSilentMessageKeepsBestAttempt() async throws {
        let nse = NotificationService()
        let entity = try registerNtfEntity(nse, "priority/silent")
        let expectedMsg = try XCTUnwrap(entity.expectedMsg)
        let events = try nseEvents("priority/silent")
        let received = await receivedMsgNtf(events[0])
        let (id, ntf) = try XCTUnwrap(received)
        XCTAssertFalse(nse.processReceivedNtf(entity, expectedMsg, ntf))
        XCTAssertEqual(nse.notificationEntities[id]?.allowedGetNextAttempts, 4)
        let marker: NtfMsgAckInfo = try decodeObject(["msgId": expectedMsg.msgId])
        XCTAssertTrue(nse.processReceivedNtf(entity, expectedMsg, .msgInfo(marker)))
        guard case .noNtf? = nse.notificationEntities[id]?.msgBestAttemptNtf else { return XCTFail("the default best attempt is kept") }
    }

    func testExtensionBadgeFollowsUnreadChange() async throws {
        let badgeCount = ntfBadgeCountGroupDefault.get()
        defer { ntfBadgeCountGroupDefault.set(badgeCount) }
        ntfBadgeCountGroupDefault.set(0)
        let nse = NotificationService()
        let entity = try registerNtfEntity(nse, "priority/raise")
        let expectedMsg = try XCTUnwrap(entity.expectedMsg)
        let events = try nseEvents("priority/raise")
        var badges: [Int] = []
        for event in events {
            let received = await receivedMsgNtf(event)
            let (_, ntf) = try XCTUnwrap(received)
            _ = nse.processReceivedNtf(entity, expectedMsg, ntf)
            badges.append(nse.badgeCount)
        }
        XCTAssertEqual(badges, [1, 1, 1, 2])
    }

    func testSilentItemIsNotCountedForUserInModeAll() {
        let cInfo = ChatInfo.direct(contact: Contact.sampleData)
        let normal = ChatItem.getSample(1, .directRcv, .now, "hi", .rcvNew)
        let silent = ChatItem.getSample(2, .directRcv, .now, "hi", .rcvNew, priority: -1)
        XCTAssertEqual([cInfo.countedForUser(normal), cInfo.countedForUser(silent)], [true, false])
        XCTAssertEqual(cInfo.countedForUser(unreadCount: 3, unreadSilent: 2), 1)
    }

    private func nseEvents(_ fixture: String) throws -> [NSEChatEvent] {
        try XCTUnwrap(try fixtureJSON(fixture) as? [Any]).map { json in
            let decoded: APIResult<NSEChatEvent> = decodeAPIResult(try JSONSerialization.data(withJSONObject: singleFieldJSON(json)))
            let event: NSEChatEvent? = if case let .result(r) = decoded { r } else { nil }
            return try XCTUnwrap(event, "event not decoded: \(decoded.responseType)")
        }
    }

    private func registerNtfEntity(_ nse: NotificationService, _ fixture: String) throws -> NotificationEntity {
        let event = try XCTUnwrap((try fixtureJSON(fixture) as? [[String: Any]])?.first?["result"] as? [String: Any])
        let item = try XCTUnwrap((event["chatItems"] as? [[String: Any]])?.first)
        let groupInfo = try XCTUnwrap((item["chatInfo"] as? [String: Any])?["groupInfo"])
        let member = try XCTUnwrap(((item["chatItem"] as? [String: Any])?["chatDir"] as? [String: Any])?["groupMember"] as? [String: Any])
        let user = try XCTUnwrap(event["user"])
        let conn = try XCTUnwrap(member["activeConn"])
        let connEntity: [String: Any] = ["type": "rcvGroupMsgConnection", "entityConnection": conn, "groupInfo": groupInfo, "groupMember": member]
        let ntfConnJSON: [String: Any] = [
            "user": user,
            "agentConnId": "agentConnId",
            "agentDbQueueId": 1,
            "connEntity": connEntity,
            "expectedMsg_": ["msgId": "expected", "msgTs": "1970-01-01T00:00:00Z"]
        ]
        let ntfConn: NtfConn = try decodeObject(singleFieldJSON(ntfConnJSON))
        let entity = try XCTUnwrap(nse.mkNotificationEntity(ntfConn: ntfConn))
        nse.notificationEntities[entity.entityId] = entity
        return entity
    }

    @MainActor
    private func labels() -> [Int] {
        let tm = ChatTagsModel.shared
        return [tm.unreadTags[tag] ?? 0, tm.priorityTags[tag] ?? 0]
    }

    @MainActor
    private func clearLabels() {
        ChatTagsModel.shared.unreadTags = [:]
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
