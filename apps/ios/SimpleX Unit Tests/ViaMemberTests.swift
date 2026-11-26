import XCTest
@testable import SimpleX
@testable import SimpleXChat

final class ViaMemberTests: XCTestCase {
    private let sendCmd = sendSimpleXCmdStr

    @MainActor
    override func tearDown() async throws {
        ChatModel.shared.groupMembers = []
        sendSimpleXCmdStr = sendCmd
    }

    @MainActor
    func testMemberViaUserIsDecoded() async throws {
        try replay("via/members")
        let members = await apiListMembers(1) ?? []
        let reviewer = try XCTUnwrap(members.first { $0.memberProfile.displayName == "reviewer" })
        XCTAssertEqual(reviewer.memberVia, "memberVia")
    }

    @MainActor
    func testItemSentAsMemberIsDecoded() async throws {
        try replay("via/members")
        let (chat, _) = try await apiGetChat(chatId: "#1", scope: nil, pagination: .last(count: 1))
        XCTAssertEqual(chat.chatItems.first?.meta.sentAsMember?.memberProfile.displayName, "reviewer")
    }

    @MainActor
    func testHeaderQuoteAndMemberRowsNameHost() async throws {
        try replay("via/received")
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
    func testCodeOfViaMemberIsNotRequested() async throws {
        try replay("via/received")
        let members = await apiListMembers(1) ?? []
        let (chat, _) = try await apiGetChat(chatId: "#1", scope: nil, pagination: .last(count: 2))
        guard case let .group(groupInfo, _) = chat.chatInfo else { return XCTFail("not a group: \(chat.chatInfo.id)") }
        XCTAssertEqual(members.filter { $0.canVerifyCode(groupInfo: groupInfo) }.map(\.displayName), ["alice"])
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
