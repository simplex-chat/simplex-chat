import XCTest
@testable import SimpleX
@testable import SimpleXChat

final class SkippedMessagesTests: XCTestCase {
    private let sendCmd = sendSimpleXCmdStr
    private let lock = NSLock()
    private var unrecorded: [String] = []

    override func tearDown() {
        sendSimpleXCmdStr = sendCmd
        XCTAssertEqual(requests(), [], "requests without recorded responses")
    }

    func testSkippedMessagesTextOfRecordedItems() async throws {
        let url = URL(fileURLWithPath: #filePath).deletingLastPathComponent().appendingPathComponent("../../../tests/fixtures/api/messages/skipped-messages.json")
        let commands = try XCTUnwrap(try JSONSerialization.jsonObject(with: Data(contentsOf: url)) as? [String: [String: Any]])["commands"] ?? [:]
        sendSimpleXCmdStr = { [unowned self] cmd, _, _ in
            guard let response = commands[cmd] else { self.unrecordedRequest(cmd); return nil }
            return try? JSONSerialization.data(withJSONObject: singleFieldJSON(response))
        }
        let (chat, _) = try await apiGetChat(chatId: "@2", scope: nil, pagination: .last(count: 2))
        let texts = chat.chatItems.map(\.content.text)
        XCTAssertEqual(texts.count, 2)
        XCTAssertEqual(texts.first, "8 skipped message(s)")
        XCTAssertTrue(texts.last?.hasSuffix(" skipped message(s)") ?? false, "\(texts)")
    }

    private func unrecordedRequest(_ request: String) {
        lock.lock()
        defer { lock.unlock() }
        unrecorded.append(request)
    }

    private func requests() -> [String] {
        lock.lock()
        defer { lock.unlock() }
        return unrecorded
    }
}
