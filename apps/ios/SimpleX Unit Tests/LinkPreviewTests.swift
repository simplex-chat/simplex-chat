import XCTest
import SwiftUI
import UIKit
@testable import SimpleX
@testable import SimpleXChat

final class LinkPreviewTests: XCTestCase {
    private let sendCmd = sendSimpleXCmdStr
    private let parseMarkdown = parseSimpleXMarkdownText
    private let lock = NSLock()
    private var commands: [String: [String: Any]] = [:]
    private var requested: [String] = []
    private var unrecorded: [String] = []
    private var window: UIWindow?

    @MainActor
    override func setUp() async throws {
        UserDefaults.standard.set(false, forKey: DEFAULT_DEVELOPER_TOOLS)
        UserDefaults.standard.set(false, forKey: DEFAULT_SHOW_LINK_PREVIEW_ERRORS)
        let recording = try XCTUnwrap(try fixtureJSON("previews/link_preview") as? [String: Any])
        let commands = try XCTUnwrap(recording["commands"] as? [String: [String: Any]])
        let markdown = recording["markdown"] as? [String: Any] ?? [:]
        self.commands = commands
        sendSimpleXCmdStr = { [unowned self] cmd, _, _ in
            guard let response = commands[cmd] else { self.locked { self.unrecorded.append(cmd) }; return nil }
            self.locked { self.requested.append(cmd) }
            return try? JSONSerialization.data(withJSONObject: singleFieldJSON(response))
        }
        parseSimpleXMarkdownText = { [unowned self] str in
            guard let parsed = markdown[str] else { self.locked { self.unrecorded.append("markdown \(str)") }; return nil }
            return try? jsonDecoder.decode(ParsedMarkdown.self, from: JSONSerialization.data(withJSONObject: singleFieldJSON(parsed)))
        }
    }

    @MainActor
    override func tearDown() async throws {
        window?.isHidden = true
        window?.rootViewController = nil
        window = nil
        if let alert = shownAlert() {
            try await waitFor { !alert.isBeingPresented }
            await withCheckedContinuation { done in alert.dismiss(animated: false) { done.resume() } }
        }
        UserDefaults.standard.removeObject(forKey: DEFAULT_DEVELOPER_TOOLS)
        UserDefaults.standard.removeObject(forKey: DEFAULT_SHOW_LINK_PREVIEW_ERRORS)
        sendSimpleXCmdStr = sendCmd
        parseSimpleXMarkdownText = parseMarkdown
        XCTAssertEqual(locked { unrecorded }, [], "requests without recorded responses")
    }

    @MainActor
    func testPreviewIsRecordedResponse() async throws {
        let (uri, preview) = try recordedPreview()
        XCTAssertFalse(preview.description.isEmpty)
        let linkPreview = await apiGetLinkPreview(uri)
        XCTAssertEqual(linkPreview, preview)
        XCTAssertEqual(requests(), ["/_link preview \(uri)"])
        XCTAssertNil(shownAlert())
    }

    @MainActor
    func testFailureShowsNoAlertWithoutBothToggles() async throws {
        let (uri, _) = try recordedError()
        for (developerTools, showLinkPreviewErrors) in [(false, false), (true, false), (false, true)] {
            UserDefaults.standard.set(developerTools, forKey: DEFAULT_DEVELOPER_TOOLS)
            UserDefaults.standard.set(showLinkPreviewErrors, forKey: DEFAULT_SHOW_LINK_PREVIEW_ERRORS)
            let linkPreview = await apiGetLinkPreview(uri)
            XCTAssertNil(linkPreview)
            XCTAssertNil(shownAlert(), "developer options \(developerTools), link preview errors \(showLinkPreviewErrors)")
        }
    }

    @MainActor
    func testFailureShowsCoreErrorWithBothToggles() async throws {
        let (uri, message) = try recordedError()
        UserDefaults.standard.set(true, forKey: DEFAULT_DEVELOPER_TOOLS)
        UserDefaults.standard.set(true, forKey: DEFAULT_SHOW_LINK_PREVIEW_ERRORS)
        let linkPreview = await apiGetLinkPreview(uri)
        XCTAssertNil(linkPreview)
        try await waitFor { self.shownAlert()?.isBeingPresented == false }
        let alert = try XCTUnwrap(shownAlert())
        XCTAssertEqual([alert.title, alert.message], ["Link preview error", message])
        XCTAssertEqual(alert.actions.map(\.title), ["Ok"])
    }

    @MainActor
    func testShareExtensionDecodesRecordedResponses() throws {
        let (uri, preview) = try recordedPreview()
        XCTAssertEqual(try apiGetLinkPreview(uri: uri), preview)
        let (failingUri, message) = try recordedError()
        XCTAssertThrowsError(try apiGetLinkPreview(uri: failingUri)) { error in
            XCTAssertEqual(error as? ChatError, .error(errorType: .linkPreview(message: message)))
        }
        XCTAssertEqual(requests(), ["/_link preview \(uri)", "/_link preview \(failingUri)"])
    }

    @MainActor
    func testComposeShowsRecordedPreview() async throws {
        let (uri, preview) = try recordedPreview()
        let compose = try await showCompose()
        compose.composeState.message = uri
        try await waitFor { compose.composeState.linkPreview == preview }
        XCTAssertEqual(requests(), ["/_link preview \(uri)"])
        XCTAssertNil(shownAlert())
    }

    @MainActor
    func testComposeShowsNoPreviewOnFailure() async throws {
        let (uri, _) = try recordedError()
        let compose = try await showCompose()
        compose.composeState.message = uri
        try await waitFor { self.requests() == ["/_link preview \(uri)"] }
        try await waitFor { compose.composeState.noPreview }
        XCTAssertNil(shownAlert())
    }

    private func recordedPreview() throws -> (uri: String, preview: LinkPreview) {
        let (uri, result) = try recorded("result")
        let preview: LinkPreview = try decodeObject(XCTUnwrap(result["linkPreview"]))
        return (uri, preview)
    }

    private func recordedError() throws -> (uri: String, message: String) {
        let (uri, error) = try recorded("error")
        let errorType = try XCTUnwrap(error["errorType"] as? [String: Any])
        XCTAssertEqual(errorType["type"] as? String, "linkPreview")
        return (uri, try XCTUnwrap(errorType["message"] as? String))
    }

    private func recorded(_ outcome: String) throws -> (uri: String, response: [String: Any]) {
        let prefix = "/_link preview "
        let (cmd, response) = try XCTUnwrap(commands.first { $0.key.hasPrefix(prefix) && $0.value[outcome] != nil }, "no recorded \(outcome)")
        return (String(cmd.dropFirst(prefix.count)), try XCTUnwrap(response[outcome] as? [String: Any]))
    }

    private func requests() -> [String] {
        locked { requested }
    }

    private func locked<T>(_ f: () -> T) -> T {
        lock.lock()
        defer { lock.unlock() }
        return f()
    }

    @MainActor
    private func shownAlert() -> UIAlertController? {
        getTopViewController() as? UIAlertController
    }

    @MainActor
    private func showCompose() async throws -> ComposeHostState {
        let compose = ComposeHostState()
        let window = UIWindow(windowScene: try XCTUnwrap(UIApplication.shared.connectedScenes.compactMap { $0 as? UIWindowScene }.first))
        window.rootViewController = UIHostingController(rootView: ComposeHost(state: compose))
        window.isHidden = false
        self.window = window
        try await Task.sleep(nanoseconds: 100_000_000)
        return compose
    }
}

private final class ComposeHostState: ObservableObject {
    @Published var composeState = ComposeState()
    @Published var showCommandsMenu = false
    @Published var keyboardVisible = false
    @Published var keyboardHiddenDate = Date.now
    @Published var selectedRange = NSRange()
}

private struct ComposeHost: View {
    @ObservedObject var state: ComposeHostState
    let chat = Chat(chatInfo: .direct(contact: Contact.sampleData))
    let chatModel = ChatModel()
    let theme = CurrentColors.toAppTheme()

    var body: some View {
        ComposeView(
            chat: chat,
            im: ItemsModel.shared,
            composeState: $state.composeState,
            showCommandsMenu: $state.showCommandsMenu,
            keyboardVisible: $state.keyboardVisible,
            keyboardHiddenDate: $state.keyboardHiddenDate,
            selectedRange: $state.selectedRange
        )
        .environmentObject(chatModel)
        .environmentObject(theme)
    }
}
