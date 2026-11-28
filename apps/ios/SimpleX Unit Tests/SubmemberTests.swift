import XCTest
import UserNotifications
@testable import SimpleX
@testable import SimpleXChat

final class SubmemberTests: XCTestCase {
    private let sendCmd = sendSimpleXCmdStr
    private let ntfManager = NtfManager.shared

    @MainActor
    override func tearDown() async throws {
        for chat in ChatModel.shared.chats { ChatModel.shared.removeChat(chat.id) }
        ChatModel.shared.im.reversedChatItems = []
        ChatModel.shared.chatId = nil
        ChatModel.shared.currentUser = nil
        ChatModel.shared.users = []
        ChatModel.shared.groupMembers = []
        ChatModel.shared.groupMembersIndexes = [:]
        NtfManager.shared = ntfManager
        UNUserNotificationCenter.current().delegate = ntfManager
        sendSimpleXCmdStr = sendCmd
    }

    @MainActor
    func testUserSubmemberIsDecoded() async throws {
        try replay("submembers/members")
        let members = await apiListMembers(1) ?? []
        let reviewer = try XCTUnwrap(members.first { $0.memberProfile.displayName == "reviewer" })
        XCTAssertEqual(reviewer.memberVia, "memberVia")
    }

    @MainActor
    func testItemSentAsMemberIsDecoded() async throws {
        try replay("submembers/members")
        let (chat, _) = try await apiGetChat(chatId: "#1", scope: nil, pagination: .last(count: 1))
        XCTAssertEqual(chat.chatItems.first?.meta.sentAsMember?.memberProfile.displayName, "reviewer")
    }

    @MainActor
    func testHeaderQuoteAndMemberRowsNameMemberItIsVia() async throws {
        try replay("submembers/received")
        ChatModel.shared.groupMembers = (await apiListMembers(1) ?? []).map(GMember.init)
        let (chat, _) = try await apiGetChat(chatId: "#1", scope: nil, pagination: .last(count: 2))
        guard case let .groupRcv(sender) = chat.chatItems.first?.chatDir,
              case let .groupRcv(quoted) = chat.chatItems.last?.quotedItem?.chatDir else {
            return XCTFail("no item of a member and no quote of it: \(chat.chatItems)")
        }
        let reviewer = try XCTUnwrap(ChatModel.shared.groupMembers.first { $0.displayName == "reviewer" })
        XCTAssertEqual([sender, quoted, reviewer.wrapped].map(ChatModel.shared.memberViaName), ["alice", "alice", "alice"])
        XCTAssertEqual(chat.chatItems.map { $0.chatDir.memberVia != nil || $0.quotedItem?.chatDir?.memberVia != nil }, [true, true])
    }

    @MainActor
    func testCodeOfSubmemberIsNotRequested() async throws {
        try replay("submembers/received")
        let members = await apiListMembers(1) ?? []
        let (chat, _) = try await apiGetChat(chatId: "#1", scope: nil, pagination: .last(count: 2))
        guard case let .group(groupInfo, _) = chat.chatInfo else { return XCTFail("not a group: \(chat.chatInfo.id)") }
        XCTAssertEqual(members.filter { $0.canVerifyCode(groupInfo: groupInfo) }.map(\.displayName), ["alice"])
    }

    @MainActor
    func testSubmemberItemIsMatchedToViaMemberConnection() async throws {
        try replay("submembers/received")
        let members = await apiListMembers(1) ?? []
        let (chat, _) = try await apiGetChat(chatId: "#1", scope: nil, pagination: .last(count: 2))
        let alice = try XCTUnwrap(members.first { $0.displayName == "alice" })
        let item = try XCTUnwrap(chat.chatItems.first)
        XCTAssertEqual(itemNtfEntityId(chat.chatInfo, item, connEntities(chat.chatInfo, members)), alice.id)
    }

    @MainActor
    func testForwardedItemIsMatchedToForwarderConnection() async throws {
        try replay("submembers/forwarded")
        let members = await apiListMembers(1) ?? []
        let (chat, _) = try await apiGetChat(chatId: "#1", scope: nil, pagination: .last(count: 2))
        let bob = try XCTUnwrap(members.first { $0.displayName == "bob" })
        let item = try XCTUnwrap(chat.chatItems.first)
        XCTAssertEqual(itemNtfEntityId(chat.chatInfo, item, connEntities(chat.chatInfo, members)), bob.id)
    }

    @MainActor
    func testMemberItemIsMatchedToMemberConnection() async throws {
        try replay("submembers/forwarded")
        let members = await apiListMembers(1) ?? []
        let (chat, _) = try await apiGetChat(chatId: "#1", scope: nil, pagination: .last(count: 2))
        let bob = try XCTUnwrap(members.first { $0.displayName == "bob" })
        let item = try XCTUnwrap(chat.chatItems.last)
        XCTAssertEqual(itemNtfEntityId(chat.chatInfo, item, connEntities(chat.chatInfo, members)), bob.id)
    }

    @MainActor
    func testItemOfSubmemberIsMarkedDeletedWithItsViaMember() async throws {
        _ = try await receiveRemovalOfViaMember()
        let hi = try XCTUnwrap(ChatModel.shared.im.reversedChatItems.first { $0.text == "hi" })
        guard case let .moderated(_, byGroupMember)? = hi.meta.itemDeleted else { return XCTFail("not moderated: \(String(describing: hi.meta.itemDeleted))") }
        XCTAssertEqual(byGroupMember.displayName, "cath")
    }

    @MainActor
    func testSubmemberIsRemovedWithItsViaMember() async throws {
        guard let reviewer = try await receiveRemovalOfViaMember() else { return }
        XCTAssertEqual(ChatModel.shared.getGroupMember(reviewer.groupMemberId)?.wrapped.memberStatus, .memRemoved)
    }

    @MainActor
    func testSubmemberIsRemovedWhenUserLeaves() async throws {
        try replay("submembers/left")
        ChatModel.shared.chatId = "#1"
        ChatModel.shared.groupMembers = (await apiListMembers(1) ?? []).map(GMember.init)
        ChatModel.shared.populateGroupMembersIndexes()
        let reviewer = try XCTUnwrap(ChatModel.shared.groupMembers.first { $0.displayName == "reviewer" })
        await leaveGroup(1)
        try? await waitFor { reviewer.wrapped.memberStatus == .memRemoved }
        XCTAssertEqual(ChatModel.shared.getGroupMember(reviewer.groupMemberId)?.wrapped.memberStatus, .memRemoved)
    }

    @MainActor
    func testSubmemberIsRemovedWhenUserIsRemoved() async throws {
        let events = try recordedEvents("submembers/user-removed")
        guard case let .chatItemReaction(_, _, reaction) = events[0], case let .group(groupInfo, _) = reaction.chatInfo,
              let reviewer = reaction.chatReaction.chatItem.meta.sentAsMember else {
            return XCTFail("no item sent as a submember: \(events[0].responseType)")
        }
        ChatModel.shared.currentUser = User.sampleData
        ChatModel.shared.users = [UserInfo(user: User.sampleData, unreadCount: 0)]
        ChatModel.shared.chatId = groupInfo.id
        _ = ChatModel.shared.upsertGroupMember(groupInfo, reviewer)
        await processReceivedMsg(try XCTUnwrap(events.last))
        XCTAssertEqual(ChatModel.shared.getGroupMember(reviewer.groupMemberId)?.wrapped.memberStatus, .memRemoved)
    }

    @MainActor
    private func receiveRemovalOfViaMember() async throws -> GroupMember? {
        let events = try recordedEvents("submembers/member-removed-with-messages")
        guard case let .newChatItems(_, items) = events[0], let aci = items.first,
              case let .group(groupInfo, _) = aci.chatInfo, case let .groupRcv(reviewer) = aci.chatItem.chatDir else {
            XCTFail("no item of a submember: \(events[0].responseType)")
            return nil
        }
        NtfManager.shared = RecordingNtfManager { _ in }
        ChatModel.shared.currentUser = User.sampleData
        ChatModel.shared.users = [UserInfo(user: User.sampleData, unreadCount: 0)]
        try listChat(events[0])
        ChatModel.shared.chatId = aci.chatInfo.id
        _ = ChatModel.shared.upsertGroupMember(groupInfo, reviewer)
        await processReceivedMsg(events[0])
        await processReceivedMsg(events[2])
        return reviewer
    }

    private func connEntities(_ chatInfo: ChatInfo, _ members: [GroupMember]) -> [ConnectionEntity] {
        guard case let .group(groupInfo, _) = chatInfo else { return [] }
        return members.compactMap { member in
            member.activeConn.map { ConnectionEntity.rcvGroupMsgConnection(entityConnection: $0, groupInfo: groupInfo, groupMember: member) }
        }
    }

    @MainActor
    private func replay(_ fixture: String) throws {
        let commands = try XCTUnwrap((try fixtureJSON(fixture) as? [String: [String: Any]])?["commands"])
        sendSimpleXCmdStr = { cmd, _, _ in
            guard let response = commands[cmd] else { return nil }
            return try? JSONSerialization.data(withJSONObject: singleFieldJSON(response))
        }
    }
}
