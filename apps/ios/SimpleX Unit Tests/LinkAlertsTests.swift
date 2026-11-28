import XCTest
import UIKit
@testable import SimpleX
@testable import SimpleXChat

final class LinkAlertsTests: XCTestCase {
    private let parseUri = parseSanitizeUri

    @MainActor
    override func setUp() async throws {
        try await dismissPresented()
        let url = URL(fileURLWithPath: #filePath).deletingLastPathComponent().appendingPathComponent("../../../tests/fixtures/api/links/received-links.json")
        let fixture = try XCTUnwrap(try JSONSerialization.jsonObject(with: Data(contentsOf: url)) as? [String: [String: Any]])
        let uris = try XCTUnwrap(fixture["uris"])
        parseSanitizeUri = { str, safe in
            guard !safe, let parsed = uris[str] else {
                XCTFail("uri without recorded parse: \(str)")
                return nil
            }
            return try? jsonDecoder.decode(ParsedUri.self, from: JSONSerialization.data(withJSONObject: parsed))
        }
    }

    @MainActor
    override func tearDown() async throws {
        try await dismissPresented()
        parseSanitizeUri = parseUri
    }

    @MainActor
    func testShortLinkShownInFull() async throws {
        try await assertAlert(shortLink, "Open link?", shortLink, ["Cancel", "Open"])
    }

    @MainActor
    func testLongPathShortened() async throws {
        try await assertAlert(longPathLink, "Open link?", "\(longPathLink.prefix(160))…", ["Cancel", "Open"])
    }

    @MainActor
    func testLongHostShownInFull() async throws {
        try await assertAlert(longHostLink, "Open link?", "https://\(longHost)…", ["Cancel", "Open"])
    }

    @MainActor
    func testLongUserInfoShownInFull() async throws {
        try await assertAlert(longUserInfoLink, "Open link?", "https://\(longUserInfo)@example.com…", ["Cancel", "Open"])
    }

    @MainActor
    func testCleanLinkKeepsReceivedHost() async throws {
        try await assertAlert(trackedLink, "Open link?", trackedLink, ["Cancel", "Open full link", "Open clean link"])
        XCTAssertEqual(sanitizeUri(trackedLink).url?.sanitizedUri, URL(string: "https://user%40name@example.com/page"))
    }

    @MainActor
    func testOtherSchemeNotOpened() async throws {
        try await assertAlert(ftpLink, "Invalid link", "Unsupported URI scheme: ftp\n\(ftpLink)", ["Ok"])
    }

    @MainActor
    private func assertAlert(_ uri: String, _ title: String, _ message: String, _ actions: [String], file: StaticString = #filePath, line: UInt = #line) async throws {
        openBrowserAlert(uri: uri)
        try? await waitFor { self.presented() != nil }
        let alert = try XCTUnwrap(presented() as? UIAlertController, file: file, line: line)
        XCTAssertEqual(alert.title, title, file: file, line: line)
        XCTAssertEqual(alert.message, message, file: file, line: line)
        XCTAssertEqual(alert.actions.compactMap(\.title), actions, file: file, line: line)
    }

    @MainActor
    private func waitFor(_ condition: @escaping () -> Bool) async throws {
        for _ in 0..<600 {
            if condition() { return }
            try await Task.sleep(nanoseconds: 50_000_000)
        }
        throw XCTestError(.timeoutWhileWaiting)
    }

    @MainActor
    private func presented() -> UIViewController? {
        if let shown = getTopViewController(), shown.presentingViewController != nil, !shown.isBeingPresented, !shown.isBeingDismissed { shown } else { nil }
    }

    @MainActor
    private func dismissPresented() async throws {
        for _ in 0..<600 {
            guard let shown = getTopViewController(), shown.presentingViewController != nil else { return }
            if shown.isBeingPresented || shown.isBeingDismissed {
                try await Task.sleep(nanoseconds: 50_000_000)
            } else {
                await withCheckedContinuation { done in shown.dismiss(animated: false) { done.resume() } }
            }
        }
    }
}

private let shortLink = "https://example.com/page?q=1"
private let longPathLink = "https://example.com/" + String(repeating: "b", count: 200)
private let longHost = Array(repeating: String(repeating: "a", count: 60), count: 3).joined(separator: ".") + ".example.com"
private let longHostLink = "https://\(longHost)/" + String(repeating: "b", count: 200)
private let longUserInfo = String(repeating: "a", count: 180)
private let longUserInfoLink = "https://\(longUserInfo)@example.com?q=" + String(repeating: "b", count: 200)
private let trackedLink = "https://user%40name@example.com/page?ref=123"
private let ftpLink = "ftp://example.com/file"
