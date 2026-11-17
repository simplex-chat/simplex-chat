import XCTest
import SwiftUI
import UIKit
@testable import SimpleX
@testable import SimpleXChat

class RecordedAPITestCase: XCTestCase {
    private let sendCmd = sendSimpleXCmdStr
    private let parseMarkdown = parseSimpleXMarkdownText
    private let parseUri = parseSanitizeUri
    private let lock = NSLock()
    private var unrecorded: [String] = []
    private var commands: [String: Any] = [:]
    var shownLocalChats: [String] = []
    var opened = 0

    @MainActor
    override func setUp() async throws {
        NSTimeZone.default = try XCTUnwrap(TimeZone(identifier: "UTC"))
        ChatModel.shared.currentUser = User.sampleData
        try await dismissPresented()
    }

    @MainActor
    override func tearDown() async throws {
        try await dismissPresented()
        for chat in ChatModel.shared.chats { ChatModel.shared.removeChat(chat.id) }
        ChatModel.shared.chatId = nil
        ChatModel.shared.appOpenUrl = nil
        ChatModel.shared.currentUser = nil
        sendSimpleXCmdStr = sendCmd
        parseSimpleXMarkdownText = parseMarkdown
        parseSanitizeUri = parseUri
        XCTAssertEqual(requests(), [], "requests without recorded responses")
    }

    func recorded(_ fixture: String) throws {
        let responses = try XCTUnwrap(try fixtureJSON(fixture) as? [String: [String: Any]])
        let commands = responses["commands"] ?? [:]
        let markdown = responses["markdown"] ?? [:]
        let uris = responses["uris"] ?? [:]
        self.commands = commands
        sendSimpleXCmdStr = { [unowned self] cmd, _, _ in
            guard let response = commands[cmd] else { self.unrecordedRequest(cmd); return nil }
            return try? JSONSerialization.data(withJSONObject: singleFieldJSON(response))
        }
        parseSimpleXMarkdownText = { [unowned self] str in
            guard let parsed = markdown[str] else { self.unrecordedRequest("markdown \(str)"); return nil }
            return try? jsonDecoder.decode(ParsedMarkdown.self, from: JSONSerialization.data(withJSONObject: singleFieldJSON(parsed)))
        }
        parseSanitizeUri = { [unowned self] str, safe in
            guard !safe, let parsed = uris[str] else { self.unrecordedRequest("uri \(str)"); return nil }
            return try? jsonDecoder.decode(ParsedUri.self, from: JSONSerialization.data(withJSONObject: singleFieldJSON(parsed)))
        }
    }

    func recordedResult<R: ChatAPIResult>(_ cmd: String) throws -> R {
        let response = try XCTUnwrap(commands[cmd], "no recorded response to \(cmd)")
        let r: APIResult<R> = decodeAPIResult(try JSONSerialization.data(withJSONObject: singleFieldJSON(response)))
        guard case let .result(res) = r else { throw RecordedResponseError(cmd: cmd, responseType: r.responseType) }
        return res
    }

    func fixtureJSON(_ fixture: String) throws -> Any {
        let url = URL(fileURLWithPath: #filePath).deletingLastPathComponent().appendingPathComponent("../../../tests/fixtures/api/\(fixture).json")
        return try JSONSerialization.jsonObject(with: Data(contentsOf: url))
    }

    private func unrecordedRequest(_ request: String) {
        locked { unrecorded.append(request) }
    }

    func requests() -> [String] {
        locked { unrecorded }
    }

    private func locked<T>(_ f: () -> T) -> T {
        lock.lock()
        defer { lock.unlock() }
        return f()
    }

    @MainActor
    func connect(_ name: String) {
        planAndConnect(name, theme: CurrentColors.toAppTheme(), dismiss: false, showLocalChats: { [unowned self] chats in self.shownLocalChats += chats.map(\.id) }, onOpen: { [unowned self] in self.opened += 1 })
    }

    @MainActor
    func assertRequested(_ expected: (String) -> Bool, file: StaticString = #filePath, line: UInt = #line) async throws {
        try? await waitFor { !self.requests().isEmpty }
        let shown = requests()
        XCTAssertEqual(shown.count, 1, "\(shown)", file: file, line: line)
        XCTAssertTrue(shown.allSatisfy(expected), "\(shown)", file: file, line: line)
        locked { unrecorded.removeAll() }
    }

    @MainActor
    func assertTexts(_ texts: [String], file: StaticString = #filePath, line: UInt = #line) async throws {
        try? await waitFor { self.shownTexts().sorted() == texts.sorted() }
        let shown = shownTexts()
        XCTAssertEqual(shown.sorted(), texts.sorted(), "\(shown)", file: file, line: line)
    }

    @MainActor
    func press(_ title: String, file: StaticString = #filePath, line: UInt = #line) async throws {
        try? await waitFor { self.presented() != nil }
        let shown = try XCTUnwrap(presented(), file: file, line: line)
        if let alert = shown as? UIAlertController {
            let action = try XCTUnwrap(alert.actions.first { $0.title == title }, file: file, line: line)
            let handler = unsafeBitCast(action.value(forKey: "handler") as AnyObject, to: (@convention(block) (UIAlertAction) -> Void).self)
            await dismiss(alert)
            handler(action)
        } else {
            let button = try XCTUnwrap(buttons(shown.view).first { $0.title(for: .normal) == title }, file: file, line: line)
            button.sendActions(for: .touchUpInside)
        }
    }

    @MainActor
    func waitFor(_ condition: @escaping () -> Bool) async throws {
        for _ in 0..<600 {
            if condition() { return }
            try await Task.sleep(nanoseconds: 50_000_000)
        }
        throw XCTestError(.timeoutWhileWaiting)
    }

    @MainActor
    func presented() -> UIViewController? {
        if let shown = getTopViewController(), shown.presentingViewController != nil, !shown.isBeingPresented, !shown.isBeingDismissed { shown } else { nil }
    }

    @MainActor
    func dismissPresented() async throws {
        for _ in 0..<600 {
            guard let shown = getTopViewController(), shown.presentingViewController != nil else { return }
            if shown.isBeingPresented || shown.isBeingDismissed {
                try await Task.sleep(nanoseconds: 50_000_000)
            } else {
                await dismiss(shown)
            }
        }
    }

    @MainActor
    func dismiss(_ shown: UIViewController) async {
        await withCheckedContinuation { done in shown.dismiss(animated: false) { done.resume() } }
    }

    @MainActor
    func shownTexts() -> [String] {
        guard let shown = presented() else { return [] }
        return if let alert = shown as? UIAlertController {
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

    private func buttons(_ view: UIView) -> [UIButton] {
        if let button = view as? UIButton { [button] } else { view.subviews.flatMap(buttons) }
    }
}

private struct RecordedResponseError: Error {
    var cmd: String
    var responseType: String
}

func singleFieldJSON(_ value: Any) -> Any {
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
