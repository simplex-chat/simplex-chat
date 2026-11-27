import XCTest
@testable import SimpleX
@testable import SimpleXChat

final class SubmemberTests: XCTestCase {
    private let sendCmd = sendSimpleXCmdStr

    @MainActor
    override func tearDown() async throws {
        ChatModel.shared.groupMembers = []
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
    func testHeaderQuoteAndMemberRowsNameHost() async throws {
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
