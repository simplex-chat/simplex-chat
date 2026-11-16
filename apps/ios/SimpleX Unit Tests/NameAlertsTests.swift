import XCTest
import UIKit
@testable import SimpleX
@testable import SimpleXChat

final class NameAlertsTests: XCTestCase {
    private let sendCmd = sendSimpleXCmdStr
    private let parseMarkdown = parseSimpleXMarkdownText

    @MainActor
    override func setUp() async throws {
        NSTimeZone.default = try XCTUnwrap(TimeZone(identifier: "UTC"))
        ChatModel.shared.currentUser = User.sampleData
    }

    @MainActor
    override func tearDown() async throws {
        if let shown = getTopViewController(), shown.presentingViewController != nil {
            await withCheckedContinuation { done in shown.dismiss(animated: false) { done.resume() } }
        }
        ChatModel.shared.currentUser = nil
        sendSimpleXCmdStr = sendCmd
        parseSimpleXMarkdownText = parseMarkdown
    }

    @MainActor
    func testKnownNameExpired() async throws {
        try await assertAlert("plan-known-expired", ["Name expired", "alice.simplex expired on January 1, 2026. Its owner can renew it until January 1, 2099.", "Open chat", "OK"])
    }

    @MainActor
    func testOwnNameExpired() async throws {
        try await assertAlert("plan-own-expired", ["Your name has expired", "Your name alice.simplex expired on January 1, 2026. Renew it before January 1, 2099.", "Renew", "OK"])
    }

    @MainActor
    func testKnownNameAvailable() async throws {
        try await assertAlert("plan-known-available", ["Name no longer registered", "alice.simplex is available for registration for $20 for 2 years.", "Register", "Open chat", "OK"])
    }

    @MainActor
    func testOwnNameAvailable() async throws {
        try await assertAlert("plan-own-available", ["Your name has expired", "Your name alice.simplex is no longer registered. It is available for registration for $20 for 2 years.", "Re-register", "OK"])
    }

    @MainActor
    func testKnownNameReserved() async throws {
        try await assertAlert("plan-known-reserved", ["Name not registered", "alice.simplex is reserved for community. Please contact SimpleX Chat if you are interested in using it.", "Connect to SimpleX team", "Open chat", "OK"])
    }

    @MainActor
    func testKnownNameMoved() async throws {
        try await assertAlert("plan-known-moved", ["cath", "@alice.simplex", "Cancel", "Open new chat"])
    }

    @MainActor
    func testNameAvailable() async throws {
        try await assertAlert("error-available", ["Name not registered", "nobody.simplex is available for registration for $20 for 2 years.", "Register", "OK"])
    }

    @MainActor
    func testNameNotRegistered() async throws {
        try await assertAlert("error-not-registered", ["Name not registered", "This SimpleX name is not registered. Please check the name.", "OK"])
    }

    @MainActor
    private func assertAlert(_ fixture: String, _ texts: [String], file: StaticString = #filePath, line: UInt = #line) async throws {
        let url = URL(fileURLWithPath: #filePath).deletingLastPathComponent().appendingPathComponent("../../../tests/fixtures/api/names/\(fixture).json")
        let recorded = try XCTUnwrap(JSONSerialization.jsonObject(with: Data(contentsOf: url)) as? [String: [String: Any]])
        let commands = try XCTUnwrap(recorded["commands"])
        let markdown = try XCTUnwrap(recorded["markdown"])
        sendSimpleXCmdStr = { cmd, _, _ in
            guard let response = commands[cmd] else { XCTFail("unrecorded command: \(cmd)"); return nil }
            return try? JSONSerialization.data(withJSONObject: singleFieldJSON(response))
        }
        parseSimpleXMarkdownText = { str in
            guard let parsed = markdown[str] else { XCTFail("unrecorded markdown: \(str)"); return nil }
            return try? jsonDecoder.decode(ParsedMarkdown.self, from: JSONSerialization.data(withJSONObject: singleFieldJSON(parsed)))
        }
        planAndConnect(try XCTUnwrap(markdown.keys.first), theme: CurrentColors.toAppTheme(), dismiss: false)
        let shown = shownTexts(try await presentedController())
        XCTAssertEqual(shown, texts, file: file, line: line)
    }

    @MainActor
    private func presentedController() async throws -> UIViewController {
        for _ in 0..<100 {
            if let shown = getTopViewController(), shown.presentingViewController != nil, !shown.isBeingPresented { return shown }
            try await Task.sleep(nanoseconds: 50_000_000)
        }
        throw XCTestError(.timeoutWhileWaiting)
    }

    private func shownTexts(_ shown: UIViewController) -> [String] {
        if let alert = shown as? UIAlertController {
            [alert.title, alert.message].compactMap { $0 } + alert.actions.compactMap(\.title)
        } else {
            viewTexts(shown.view)
        }
    }

    private func viewTexts(_ view: UIView) -> [String] {
        if let label = view as? UILabel {
            [label.text].compactMap { $0 }
        } else if let button = view as? UIButton {
            [button.title(for: .normal)].compactMap { $0 }
        } else {
            view.subviews.flatMap(viewTexts)
        }
    }
}

private func singleFieldJSON(_ value: Any) -> Any {
    if let object = value as? [String: Any] {
        var fields = object.mapValues(singleFieldJSON)
        guard let tag = object["type"] as? String else { return fields }
        fields["type"] = nil
        return ["_owsf": true, tag: fields]
    } else if let array = value as? [Any] {
        return array.map(singleFieldJSON)
    } else {
        return value
    }
}
