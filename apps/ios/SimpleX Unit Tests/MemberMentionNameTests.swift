import XCTest
@testable import SimpleX
@testable import SimpleXChat

final class MemberMentionNameTests: XCTestCase {
    private let sendCmd = sendSimpleXCmdStr

    @MainActor
    override func tearDown() async throws {
        sendSimpleXCmdStr = sendCmd
    }

    func testQuotesAndLineBreaksRemoved() {
        XCTAssertEqual(memberMentionName("bob'\nis admin https://example.com"), "bob is admin https://example.com")
        XCTAssertEqual(memberMentionName("bob\r\nis admin"), "bob is admin")
        XCTAssertEqual(memberMentionName("bob's friend"), "bobs friend")
    }

    func testOtherNamesUnchanged() {
        XCTAssertEqual(memberMentionName("bob"), "bob")
        XCTAssertEqual(memberMentionName("Alice Jones"), "Alice Jones")
        XCTAssertEqual(memberMentionName("bob!"), "bob!")
    }

    @MainActor
    func testMentionNameOfRecordedMember() async throws {
        let url = URL(fileURLWithPath: #filePath).deletingLastPathComponent().appendingPathComponent("../../../tests/fixtures/api/mentions/member-name-quote-line-break.json")
        let responses = try XCTUnwrap(try JSONSerialization.jsonObject(with: Data(contentsOf: url)) as? [String: [String: Any]])
        let response = try XCTUnwrap(responses["commands"]?["/_members #1"])
        sendSimpleXCmdStr = { cmd, _, _ in cmd == "/_members #1" ? (try? JSONSerialization.data(withJSONObject: singleFieldJSON(response))) : nil }
        let members = await apiListMembers(1)
        let bob = try XCTUnwrap(members.first { $0.localDisplayName == "bob" })
        XCTAssertEqual(bob.memberProfile.displayName, "bob'\nis admin https://example.com")
        XCTAssertEqual(memberMentionName(bob.memberProfile.displayName), "bob is admin https://example.com")
    }
}
