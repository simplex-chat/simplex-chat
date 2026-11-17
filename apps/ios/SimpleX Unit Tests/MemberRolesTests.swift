import XCTest
@testable import SimpleX
@testable import SimpleXChat

final class MemberRolesTests: XCTestCase {
    private let sendCmd = sendSimpleXCmdStr
    private let lock = NSLock()
    private var unrecorded: [String] = []

    @MainActor
    override func setUp() async throws {
        ChatModel.shared.currentUser = User.sampleData
        let url = URL(fileURLWithPath: #filePath).deletingLastPathComponent().appendingPathComponent("../../../tests/fixtures/api/members/unknown-roles.json")
        let fixture = try XCTUnwrap(try JSONSerialization.jsonObject(with: Data(contentsOf: url)) as? [String: [String: Any]])
        let commands = try XCTUnwrap(fixture["commands"])
        sendSimpleXCmdStr = { [unowned self] cmd, _, _ in
            guard let response = commands[cmd] else { self.unrecordedRequest(cmd); return nil }
            return try? JSONSerialization.data(withJSONObject: singleFieldJSON(response))
        }
    }

    @MainActor
    override func tearDown() async throws {
        for chat in ChatModel.shared.chats { ChatModel.shared.removeChat(chat.id) }
        ChatModel.shared.currentUser = nil
        sendSimpleXCmdStr = sendCmd
        XCTAssertEqual(locked { unrecorded }, [], "requests without recorded responses")
    }

    @MainActor
    func testGroupOfUnknownRoleIsListed() throws {
        ChatModel.shared.updateChats(try apiGetChats())
        guard case let .group(groupInfo, _)? = ChatModel.shared.getGroupChat(1)?.chatInfo else { return XCTFail("group not listed") }
        XCTAssertEqual(groupInfo.membership.memberRole.rawValue, "vip")
        XCTAssertEqual(groupInfo.membership.memberRole.text(isChannel: false), "vip")
    }

    @MainActor
    func testRoleChangesToUnknownRolesAreShown() async throws {
        let (chat, _) = try await apiGetChat(chatId: "#1", scope: nil, pagination: .last(count: 2))
        XCTAssertEqual(chat.chatItems.map(\.content.text), ["changed role of cath to vip", "changed your role to vip"])
    }

    @MainActor
    func testMembersOfUnknownRoleAreListed() async {
        let members = await apiListMembers(1)
        XCTAssertEqual(members.map(\.displayName), ["alice", "cath"])
        XCTAssertEqual(members.map(\.memberRole.rawValue), ["owner", "vip"])
        XCTAssertEqual(members.map { $0.memberRole.text(isChannel: false) }, ["owner", "vip"])
        XCTAssertEqual(members.map(\.memberRole).sorted().map(\.rawValue), ["vip", "owner"])
    }

    private func unrecordedRequest(_ request: String) {
        locked { unrecorded.append(request) }
    }

    private func locked<T>(_ f: () -> T) -> T {
        lock.lock()
        defer { lock.unlock() }
        return f()
    }
}
