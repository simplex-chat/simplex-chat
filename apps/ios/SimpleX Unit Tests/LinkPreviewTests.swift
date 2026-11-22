import XCTest
import SwiftUI
import UIKit
@testable import SimpleX
@testable import SimpleXChat

final class LinkPreviewTests: XCTestCase {
    private let sendCmd = sendSimpleXCmdStr
    private let parseMarkdown = parseSimpleXMarkdownText
    private let lock = NSLock()
    private var requested: [String] = []
    private var unrecorded: [String] = []
    private var held: [String: DispatchSemaphore] = [:]
    private var window: UIWindow?

    @MainActor
    override func setUp() async throws {
        UserDefaults.standard.set(false, forKey: DEFAULT_DEVELOPER_TOOLS)
        UserDefaults.standard.set(false, forKey: DEFAULT_SHOW_LINK_PREVIEW_ERRORS)
        let recording = try XCTUnwrap(try fixtureJSON("previews/link_preview") as? [String: [String: Any]])
        let commands = recording["commands"] ?? [:]
        let markdown = recording["markdown"] ?? [:]
        sendSimpleXCmdStr = { [unowned self] cmd, _, _ in
            guard let response = commands[cmd] else { self.locked { self.unrecorded.append(cmd) }; return nil }
            let gate: DispatchSemaphore? = self.locked { self.requested.append(cmd); return self.held[cmd] }
            gate?.wait()
            return try? JSONSerialization.data(withJSONObject: singleFieldJSON(response))
        }
        parseSimpleXMarkdownText = { [unowned self] str in
            guard let parsed = markdown[str] else { self.locked { self.unrecorded.append("markdown \(str)") }; return nil }
            return try? jsonDecoder.decode(ParsedMarkdown.self, from: JSONSerialization.data(withJSONObject: singleFieldJSON(parsed)))
        }
    }

    @MainActor
    override func tearDown() async throws {
        locked { held.values }.forEach { $0.signal() }
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
        let linkPreview = await apiGetLinkPreview("LINK_URL/page")
        assertRecordedPreview(linkPreview)
        XCTAssertEqual(requests(), ["/_link preview LINK_URL/page"])
        XCTAssertNil(shownAlert())
    }

    @MainActor
    func testFailureShowsNoAlertWithoutBothToggles() async throws {
        for (developerTools, showLinkPreviewErrors) in [(false, false), (true, false), (false, true)] {
            UserDefaults.standard.set(developerTools, forKey: DEFAULT_DEVELOPER_TOOLS)
            UserDefaults.standard.set(showLinkPreviewErrors, forKey: DEFAULT_SHOW_LINK_PREVIEW_ERRORS)
            let linkPreview = await apiGetLinkPreview("LINK_URL/no-image")
            XCTAssertNil(linkPreview)
            XCTAssertNil(shownAlert(), "developer options \(developerTools), link preview errors \(showLinkPreviewErrors)")
        }
    }

    @MainActor
    func testFailureShowsCoreErrorWithBothToggles() async throws {
        UserDefaults.standard.set(true, forKey: DEFAULT_DEVELOPER_TOOLS)
        UserDefaults.standard.set(true, forKey: DEFAULT_SHOW_LINK_PREVIEW_ERRORS)
        let linkPreview = await apiGetLinkPreview("LINK_URL/no-image")
        XCTAssertNil(linkPreview)
        try await waitFor { self.shownAlert()?.isBeingPresented == false }
        let alert = try XCTUnwrap(shownAlert())
        XCTAssertEqual([alert.title, alert.message], ["Link preview error", "no image"])
        XCTAssertEqual(alert.actions.map(\.title), ["Ok"])
    }

    @MainActor
    func testShareExtensionDecodesRecordedResponses() throws {
        assertRecordedPreview(try apiGetLinkPreview(uri: "LINK_URL/page"))
        XCTAssertThrowsError(try apiGetLinkPreview(uri: "LINK_URL/no-image")) { error in
            XCTAssertEqual(error as? ChatError, .error(errorType: .linkPreview(message: "no image")))
        }
        XCTAssertEqual(requests(), ["/_link preview LINK_URL/page", "/_link preview LINK_URL/no-image"])
    }

    @MainActor
    func testComposeShowsRecordedPreview() async throws {
        let compose = try await showCompose()
        compose.composeState.message = "LINK_URL/page"
        try await waitFor { compose.composeState.linkPreview != nil }
        assertRecordedPreview(compose.composeState.linkPreview)
        XCTAssertEqual(requests(), ["/_link preview LINK_URL/page"])
        XCTAssertNil(shownAlert())
    }

    @MainActor
    func testComposeShowsNoPreviewOnFailureAndDoesNotFetchItAgain() async throws {
        let compose = try await showCompose()
        compose.composeState.message = "LINK_URL/no-image"
        try await waitFor { self.requests() == ["/_link preview LINK_URL/no-image"] }
        try await waitFor { compose.composeState.noPreview }
        XCTAssertNil(shownAlert())
        compose.composeState.message = "LINK_URL/page"
        try await waitFor { compose.composeState.linkPreview != nil }
        compose.composeState.message = "LINK_URL/no-image"
        try await Task.sleep(nanoseconds: 2_000_000_000)
        XCTAssertTrue(compose.composeState.noPreview)
        XCTAssertEqual(requests(), ["/_link preview LINK_URL/no-image", "/_link preview LINK_URL/page"])
    }

    @MainActor
    func testComposeKeepsNewerPreviewWhenOlderLinkResponds() async throws {
        let older = hold("/_link preview LINK_URL/no-image")
        let compose = try await showCompose()
        compose.composeState.message = "LINK_URL/no-image"
        try await waitFor { self.requests() == ["/_link preview LINK_URL/no-image"] }
        compose.composeState.message = "LINK_URL/page"
        try await waitFor { compose.composeState.linkPreview != nil }
        older.signal()
        try await Task.sleep(nanoseconds: 1_500_000_000)
        assertRecordedPreview(compose.composeState.linkPreview)
        XCTAssertEqual(requests(), ["/_link preview LINK_URL/no-image", "/_link preview LINK_URL/page"])
    }

    private func hold(_ cmd: String) -> DispatchSemaphore {
        let gate = DispatchSemaphore(value: 0)
        locked { held[cmd] = gate }
        return gate
    }

    private func assertRecordedPreview(_ linkPreview: LinkPreview?, file: StaticString = #filePath, line: UInt = #line) {
        XCTAssertEqual(linkPreview?.uri, "LINK_URL/page", file: file, line: line)
        XCTAssertEqual(linkPreview?.title, "Link preview title", file: file, line: line)
        XCTAssertEqual(linkPreview?.description, "Link preview description", file: file, line: line)
        XCTAssertNotNil(linkPreview.flatMap { imageFromBase64($0.image) }, "image not decoded", file: file, line: line)
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
