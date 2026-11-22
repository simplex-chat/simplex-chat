import XCTest
@testable import SimpleX
@testable import SimpleXChat

final class ViaMemberTests: XCTestCase {
    private let sendCmd = sendSimpleXCmdStr

    @MainActor
    override func setUp() async throws {
        let commands = try XCTUnwrap((try fixtureJSON("via/members") as? [String: [String: Any]])?["commands"])
        sendSimpleXCmdStr = { cmd, _, _ in
            guard let response = commands[cmd] else { return nil }
            return try? JSONSerialization.data(withJSONObject: singleFieldJSON(response))
        }
    }

    @MainActor
    override func tearDown() async throws {
        sendSimpleXCmdStr = sendCmd
    }

    @MainActor
    func testMemberViaUserIsDecoded() async throws {
        let members = await apiListMembers(1)
        let reviewer = try XCTUnwrap(members.first { $0.memberProfile.displayName == "reviewer" })
        XCTAssertEqual(reviewer.memberVia, "memberVia")
    }

    @MainActor
    func testItemSentAsMemberIsDecoded() async throws {
        let (chat, _) = try await apiGetChat(chatId: "#1", scope: nil, pagination: .last(count: 1))
        XCTAssertEqual(chat.chatItems.first?.meta.sentAsMember?.memberProfile.displayName, "reviewer")
    }
}
