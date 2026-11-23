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
    private var commands: [String: Any] = [:]
    private var window: UIWindow?
    private let settings: [(defaults: UserDefaults, key: String, value: Bool)] = [
        (UserDefaults.standard, DEFAULT_DEVELOPER_TOOLS, false),
        (UserDefaults.standard, DEFAULT_SHOW_LINK_PREVIEW_ERRORS, false),
        (UserDefaults.standard, DEFAULT_PRIVACY_SAVE_LAST_DRAFT, true),
        (groupDefaults, GROUP_DEFAULT_PRIVACY_LINK_PREVIEWS, true),
        (groupDefaults, GROUP_DEFAULT_PRIVACY_SANITIZE_LINKS, false)
    ]
    private var savedSettings: [Any?] = []

    @MainActor
    override func setUp() async throws {
        savedSettings = settings.map { $0.defaults.object(forKey: $0.key) }
        for s in settings { s.defaults.set(s.value, forKey: s.key) }
        let recording = try XCTUnwrap(try fixtureJSON("previews/link_preview") as? [String: [String: Any]])
        let commands = recording["commands"] ?? [:]
        let markdown = recording["markdown"] ?? [:]
        self.commands = commands
        sendSimpleXCmdStr = { [unowned self] cmd, _, _ in
            guard let match = self.recorded(cmd, commands) else { self.locked { self.unrecorded.append(cmd) }; return nil }
            let gate: DispatchSemaphore? = self.locked { self.requested.append(match.key); return self.held[match.key] }
            gate?.wait()
            return try? JSONSerialization.data(withJSONObject: singleFieldJSON(match.response))
        }
        parseSimpleXMarkdownText = { [unowned self] str in
            let recorded: Any? = str.isEmpty ? [String: Any]() : markdown[str]
            guard let parsed = recorded else { self.locked { self.unrecorded.append("markdown \(str)") }; return nil }
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
        for (s, saved) in zip(settings, savedSettings) { s.defaults.set(saved, forKey: s.key) }
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
    func testShareExtensionDecodesRecordedResponses() async throws {
        assertRecordedPreview(try await apiGetLinkPreview(uri: "LINK_URL/page"))
        do {
            _ = try await apiGetLinkPreview(uri: "LINK_URL/no-image")
            XCTFail("no error")
        } catch {
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
    func testComposeSendsCardOfNextLinkAndFetchesFailedLinkAfterSend() async throws {
        let presetTags = ChatTagsModel.shared.presetTags
        defer { ChatTagsModel.shared.presetTags = presetTags }
        let note = try XCTUnwrap(commands.keys.first { $0.hasPrefix("/_create ") })
        let compose = try await showCompose(ComposeHostState(chat: Chat(chatInfo: .local(noteFolder: NoteFolder.sampleData))))
        compose.composeState.liveMessage = LiveMessage(chatItem: ChatItem.liveDummy(.local), typedMsg: "", sentMsg: nil)
        compose.composeState.message = "LINK_URL/no-image LINK_URL/page"
        try await waitFor { self.requests() == ["/_link preview LINK_URL/no-image"] }
        try await Task.sleep(nanoseconds: 1_000_000_000)
        compose.composeState.message = "LINK_URL/no-image LINK_URL/page "
        try await waitFor { compose.composeState.linkPreview != nil }
        assertRecordedPreview(compose.composeState.linkPreview)
        compose.shown = false
        try await waitFor { self.requests() == ["/_link preview LINK_URL/no-image", "/_link preview LINK_URL/page", note] }
        try await waitFor { compose.composeState.message.isEmpty }
        XCTAssertNil(shownAlert())
        try await show(compose)
        compose.composeState.message = "LINK_URL/no-image"
        try await waitFor { self.requests() == ["/_link preview LINK_URL/no-image", "/_link preview LINK_URL/page", note, "/_link preview LINK_URL/no-image"] }
    }

    @MainActor
    func testNewComposeFetchesFailedLinkAgain() async throws {
        let compose = try await showCompose()
        compose.composeState.message = "LINK_URL/no-image"
        try await waitFor { self.requests() == ["/_link preview LINK_URL/no-image"] }
        try await waitFor { compose.composeState.noPreview }
        let newCompose = try await showCompose()
        newCompose.composeState.message = "LINK_URL/no-image"
        try await waitFor { self.requests() == ["/_link preview LINK_URL/no-image", "/_link preview LINK_URL/no-image"] }
    }

    @MainActor
    func testOpenedChatFetchesFailedLinkAgain() async throws {
        let compose = try await showCompose()
        compose.composeState.message = "LINK_URL/no-image"
        try await waitFor { self.requests() == ["/_link preview LINK_URL/no-image"] }
        try await waitFor { compose.composeState.noPreview }
        compose.chatModel.chatId = "@2"
        try await waitFor { self.requests() == ["/_link preview LINK_URL/no-image", "/_link preview LINK_URL/no-image"] }
    }

    @MainActor
    func testSavedDraftFetchesFailedLinkAgain() async throws {
        let compose = try await showCompose()
        compose.composeState.message = "LINK_URL/no-image"
        try await waitFor { self.requests() == ["/_link preview LINK_URL/no-image"] }
        try await waitFor { compose.composeState.noPreview }
        compose.shown = false
        try await waitFor { compose.chatModel.draft != nil }
        let draft = try XCTUnwrap(compose.chatModel.draft)
        XCTAssertEqual(draft.cancelledLinks, [])
        let restored = try await showCompose()
        restored.composeState = draft
        try await waitFor { self.requests() == ["/_link preview LINK_URL/no-image", "/_link preview LINK_URL/no-image"] }
    }

    @MainActor
    func testClearedComposeFetchesFailedLinkAgain() async throws {
        let compose = try await showCompose()
        compose.composeState.message = "LINK_URL/no-image"
        try await waitFor { self.requests() == ["/_link preview LINK_URL/no-image"] }
        try await waitFor { compose.composeState.noPreview }
        compose.composeState.message = ""
        try await waitFor { compose.composeState.parsedMessage.isEmpty }
        compose.composeState.message = "LINK_URL/no-image"
        try await waitFor { self.requests() == ["/_link preview LINK_URL/no-image", "/_link preview LINK_URL/no-image"] }
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

    @MainActor
    func testComposeRequestsRestoredLinkAgainAndNotTheReplacedOne() async throws {
        let compose = try await showCompose()
        compose.composeState.message = "LINK_URL/page"
        try await waitFor { compose.composeState.linkPreview != nil }
        compose.composeState.message = "LINK_URL/no-image"
        try await waitFor { compose.composeState.parsedMessage.first?.text == "LINK_URL/no-image" }
        compose.composeState.message = "LINK_URL/page"
        try await waitFor { compose.composeState.parsedMessage.first?.text == "LINK_URL/page" }
        try await waitFor { self.requests().count == 2 }
        try await waitFor { compose.composeState.linkPreview != nil }
        try await Task.sleep(nanoseconds: 1_000_000_000)
        assertRecordedPreview(compose.composeState.linkPreview)
        XCTAssertEqual(requests(), ["/_link preview LINK_URL/page", "/_link preview LINK_URL/page"])
    }

    @MainActor
    func testComposeRequestsLinkChangedAndRestoredBeforeItsRequestOnce() async throws {
        _ = hold("/_link preview LINK_URL/page")
        let compose = try await showCompose()
        compose.composeState.message = "LINK_URL/page"
        try await waitFor { compose.composeState.parsedMessage.first?.text == "LINK_URL/page" }
        compose.composeState.message = "LINK_URL/no-image"
        try await waitFor { compose.composeState.parsedMessage.first?.text == "LINK_URL/no-image" }
        compose.composeState.message = "LINK_URL/page"
        try await waitFor { compose.composeState.parsedMessage.first?.text == "LINK_URL/page" }
        try await waitFor { self.requests() == ["/_link preview LINK_URL/page"] }
        try await Task.sleep(nanoseconds: 1_000_000_000)
        XCTAssertEqual(requests(), ["/_link preview LINK_URL/page"])
    }

    @MainActor
    func testComposeKeepsCardOfEditedMessageWhenDiscardedLinkResponds() async throws {
        let response = hold("/_link preview LINK_URL/page")
        let compose = try await showCompose()
        compose.composeState.message = "LINK_URL/page"
        try await waitFor { self.requests() == ["/_link preview LINK_URL/page"] }
        let edited = LinkPreview(uri: "LINK_URL/no-image", title: "Edited message", image: "")
        compose.composeState = ComposeState(editingItem: ChatItem(
            chatDir: .directSnd,
            meta: CIMeta.getSample(1, .now, "LINK_URL/no-image"),
            content: .sndMsgContent(msgContent: .link(text: "LINK_URL/no-image", preview: edited)),
            formattedText: parseSimpleXMarkdown("LINK_URL/no-image")
        ), previewsOn: true)
        try await waitFor { compose.composeState.parsedMessage.first?.linkUri == "LINK_URL/no-image" }
        response.signal()
        try await Task.sleep(nanoseconds: 1_000_000_000)
        XCTAssertEqual(compose.composeState.linkPreview, edited)
        XCTAssertEqual(requests(), ["/_link preview LINK_URL/page"])
    }

    @MainActor
    func testEditedNoteKeepsCardOfItsSecondLink() async throws {
        let (note, item) = try await recordedNote()
        guard case let .link(text, preview)? = item.content.msgContent else { return XCTFail("note without a link preview") }
        let compose = try await showCompose()
        compose.composeState = ComposeState(editingItem: item, previewsOn: true)
        try await Task.sleep(nanoseconds: 2_000_000_000)
        XCTAssertEqual(status(compose.composeState), .shown(linkPreview: preview))
        XCTAssertEqual(compose.composeState.checkLinkPreview(), .link(text: text, preview: preview))
        compose.composeState.message = "LINK_URL/page"
        try await waitFor { compose.composeState.parsedMessage.first?.text == "LINK_URL/page" }
        try await Task.sleep(nanoseconds: 2_000_000_000)
        XCTAssertEqual(status(compose.composeState), .shown(linkPreview: preview))
        XCTAssertEqual(compose.composeState.checkLinkPreview(), .link(text: "LINK_URL/page", preview: preview))
        XCTAssertEqual(requests(), [note])
    }

    @MainActor
    func testSavedDraftOfEditedNoteKeepsCardOfItsSecondLink() async throws {
        let (note, item) = try await recordedNote()
        guard case let .link(text, preview)? = item.content.msgContent else { return XCTFail("note without a link preview") }
        let compose = try await showCompose()
        compose.composeState = ComposeState(editingItem: item, previewsOn: true)
        try await Task.sleep(nanoseconds: 200_000_000)
        compose.shown = false
        try await waitFor { compose.chatModel.draft != nil }
        let draft = try XCTUnwrap(compose.chatModel.draft)
        XCTAssertEqual(draft.cancelledLinks, ["LINK_URL/no-image"])
        XCTAssertEqual(status(draft), .shown(linkPreview: preview))
        let restored = try await showCompose()
        restored.composeState = draft
        try await Task.sleep(nanoseconds: 2_000_000_000)
        XCTAssertEqual(status(restored.composeState), .shown(linkPreview: preview))
        XCTAssertEqual(restored.composeState.checkLinkPreview(), .link(text: text, preview: preview))
        XCTAssertEqual(requests(), [note])
    }

    @MainActor
    func testComposeForwardedToSameChatKeepsLoadingLinkPreview() async throws {
        let response = hold("/_link preview LINK_URL/page")
        let compose = try await showCompose()
        compose.composeState.message = "LINK_URL/no-image LINK_URL/page"
        try await waitFor { self.requests() == ["/_link preview LINK_URL/no-image", "/_link preview LINK_URL/page"] }
        compose.composeState = compose.composeState.forwardingItems([ChatItem.getSample(1, .directRcv, .now, "hello")], fromChatInfo: compose.chat.chatInfo, previewsOn: true)
        response.signal()
        try await waitFor { compose.composeState.linkPreview != nil }
        let preview = try XCTUnwrap(compose.composeState.linkPreview)
        assertRecordedPreview(preview)
        XCTAssertTrue(compose.composeState.forwarding)
        XCTAssertEqual(compose.composeState.cancelledLinks, ["LINK_URL/no-image"])
        XCTAssertEqual(compose.composeState.checkLinkPreview(), .link(text: "LINK_URL/no-image LINK_URL/page", preview: preview))
        XCTAssertEqual(requests(), ["/_link preview LINK_URL/no-image", "/_link preview LINK_URL/page"])
    }

    @MainActor
    func testComposeForwardedToSameChatWithAttachmentRequestsLinkAtNextTextChange() async throws {
        let compose = try await showCompose()
        compose.composeState.message = "LINK_URL/no-image LINK_URL/page"
        try await waitFor { !compose.composeState.parsedMessage.isEmpty }
        attachFile(compose)
        compose.composeState = compose.composeState.forwardingItems([ChatItem.getSample(1, .directRcv, .now, "hello")], fromChatInfo: compose.chat.chatInfo, previewsOn: true)
        try await Task.sleep(nanoseconds: 2_000_000_000)
        XCTAssertTrue(compose.composeState.noPreview)
        XCTAssertEqual(requests(), [])
        compose.composeState.message = "LINK_URL/no-image LINK_URL/page "
        try await waitFor { compose.composeState.linkPreview != nil }
        assertRecordedPreview(compose.composeState.linkPreview)
        XCTAssertTrue(compose.composeState.forwarding)
        XCTAssertEqual(requests(), ["/_link preview LINK_URL/no-image", "/_link preview LINK_URL/page"])
    }

    @MainActor
    func testComposeKeepsSendInProgressWhenPreviewLoads() async throws {
        let response = hold("/_link preview LINK_URL/page")
        let compose = try await showCompose()
        compose.composeState.message = "LINK_URL/page"
        try await waitFor { self.requests() == ["/_link preview LINK_URL/page"] }
        compose.composeState.inProgress = true
        response.signal()
        try await waitFor { compose.composeState.linkPreview != nil }
        XCTAssertTrue(compose.composeState.inProgress)
    }

    @MainActor
    func testComposeLoadsRestoredLinkPreviewWithoutTextChange() async throws {
        let compose = try await showCompose()
        compose.composeState.message = "LINK_URL/page"
        try await waitFor { compose.composeState.linkPreview != nil }
        compose.composeState.preview = .linkPreview(status: .loading(uri: "LINK_URL/page", request: ComposeState.nextLinkRequest()))
        try await waitFor { self.requests() == ["/_link preview LINK_URL/page", "/_link preview LINK_URL/page"] }
        try await waitFor { compose.composeState.linkPreview != nil }
        assertRecordedPreview(compose.composeState.linkPreview)
    }

    @MainActor
    func testComposeKeepsAttachmentChosenBeforeLinkPreviewLoads() async throws {
        let compose = try await showCompose()
        compose.composeState.message = "LINK_URL/page"
        try await waitFor { !compose.composeState.parsedMessage.isEmpty }
        attachFile(compose)
        try await Task.sleep(nanoseconds: 2_000_000_000)
        XCTAssertTrue(compose.composeState.attachmentPreview)
        XCTAssertEqual(requests(), [])
    }

    @MainActor
    func testComposeKeepsAttachmentChosenWhileLinkPreviewLoads() async throws {
        let response = hold("/_link preview LINK_URL/page")
        let compose = try await showCompose()
        compose.composeState.message = "LINK_URL/page"
        try await waitFor { self.requests() == ["/_link preview LINK_URL/page"] }
        attachFile(compose)
        response.signal()
        try await Task.sleep(nanoseconds: 1_000_000_000)
        XCTAssertTrue(compose.composeState.attachmentPreview)
    }

    @MainActor
    func testComposeKeepsAttachmentChosenBeforeViewUpdate() async throws {
        let response = hold("/_link preview LINK_URL/page")
        let compose = try await showCompose()
        compose.composeState.message = "LINK_URL/page"
        try await waitFor { self.requests() == ["/_link preview LINK_URL/page"] }
        response.signal()
        Thread.sleep(forTimeInterval: 0.5)
        attachFile(compose)
        try await Task.sleep(nanoseconds: 1_000_000_000)
        XCTAssertTrue(compose.composeState.attachmentPreview)
    }

    @MainActor
    func testComposeKeepsFailedLinkAfterAttachmentRemoved() async throws {
        let compose = try await showCompose()
        compose.composeState.message = "LINK_URL/no-image"
        try await waitFor { self.requests() == ["/_link preview LINK_URL/no-image"] }
        try await waitFor { compose.composeState.noPreview }
        try await typeWithAttachment(compose)
        compose.composeState.message = "LINK_URL/no-image"
        try await waitFor { compose.composeState.parsedMessage.first?.text == "LINK_URL/no-image" }
        try await Task.sleep(nanoseconds: 2_000_000_000)
        XCTAssertTrue(compose.composeState.noPreview)
        XCTAssertEqual(requests(), ["/_link preview LINK_URL/no-image"])
    }

    @MainActor
    func testComposeKeepsDismissedLinkAfterAttachmentRemoved() async throws {
        let compose = try await showCompose()
        compose.composeState.message = "LINK_URL/page"
        try await waitFor { compose.composeState.linkPreview != nil }
        compose.composeState = compose.composeState.updateLinkPreview(.cancelled, previewsOn: true)
        try await typeWithAttachment(compose)
        compose.composeState.message = "LINK_URL/page"
        try await waitFor { compose.composeState.parsedMessage.first?.text == "LINK_URL/page" }
        try await Task.sleep(nanoseconds: 2_000_000_000)
        XCTAssertTrue(compose.composeState.noPreview)
        XCTAssertEqual(requests(), ["/_link preview LINK_URL/page"])
    }

    @MainActor
    func testComposeDropsPreviewOfLinkRemovedBeforeItLoads() async throws {
        let compose = try await showCompose()
        compose.composeState.message = "LINK_URL/page"
        try await waitFor { !compose.composeState.parsedMessage.isEmpty }
        compose.composeState.message = "text without a link"
        try await waitFor { compose.composeState.parsedMessage.first?.text == "text without a link" }
        try await Task.sleep(nanoseconds: 2_000_000_000)
        XCTAssertTrue(compose.composeState.noPreview)
        XCTAssertEqual(requests(), [])
    }

    @MainActor
    func testComposeDropsPreviewOfLinkRemovedWhileItLoads() async throws {
        let response = hold("/_link preview LINK_URL/page")
        let compose = try await showCompose()
        compose.composeState.message = "LINK_URL/page"
        try await waitFor { self.requests() == ["/_link preview LINK_URL/page"] }
        compose.composeState.message = "text without a link"
        try await waitFor { compose.composeState.parsedMessage.first?.text == "text without a link" }
        response.signal()
        try await Task.sleep(nanoseconds: 1_000_000_000)
        XCTAssertTrue(compose.composeState.noPreview)
    }

    @MainActor
    func testEmptyTextClearsCancelledLinksAndLinkPreview() {
        let failed = fetched(typed("LINK_URL/no-image"), nil)
        let cleared = typed("", in: failed)
        XCTAssertEqual(cleared.cancelledLinks, [])
        XCTAssertTrue(cleared.noPreview)
        let clearedWithFile = typed("", in: withFile(failed))
        XCTAssertEqual(clearedWithFile.cancelledLinks, [])
        XCTAssertTrue(clearedWithFile.attachmentPreview)
    }

    @MainActor
    func testTextWithTheSameLinkKeepsItsStatus() {
        let loading = typed("LINK_URL/page")
        XCTAssertEqual(status(loading.updateLinkPreview(.textChanged, previewsOn: true)), status(loading))
        let next = fetched(typed("LINK_URL/no-image LINK_URL/page"), nil)
        XCTAssertEqual(status(typed("LINK_URL/no-image LINK_URL/page ", in: next)), status(next))
        let shown = fetched(next, pagePreview)
        XCTAssertEqual(status(shown), .shown(linkPreview: pagePreview))
        XCTAssertEqual(status(typed("LINK_URL/no-image LINK_URL/page ", in: shown)), .shown(linkPreview: pagePreview))
    }

    @MainActor
    func testTextWithAnotherLinkRequestsIt() throws {
        let loading = typed("LINK_URL/page")
        let request = try XCTUnwrap(loading.loadingRequest)
        XCTAssertEqual(status(loading), .loading(uri: "LINK_URL/page", request: request))
        let replaced = typed("LINK_URL/no-image", in: loading)
        let newRequest = try XCTUnwrap(replaced.loadingRequest)
        XCTAssertNotEqual(newRequest, request)
        XCTAssertEqual(status(replaced), .loading(uri: "LINK_URL/no-image", request: newRequest))
        let fromShown = typed("LINK_URL/no-image", in: fetched(loading, pagePreview))
        XCTAssertEqual(loadingUri(fromShown), "LINK_URL/no-image")
    }

    @MainActor
    func testTextWithoutLinkOrWithPreviewsOffShowsNoPreview() {
        let loading = typed("LINK_URL/page")
        XCTAssertTrue(typed("text without a link", in: loading).noPreview)
        XCTAssertTrue(typed("text without a link", in: fetched(loading, pagePreview)).noPreview)
        XCTAssertTrue(typed("LINK_URL/no-image", in: loading, previewsOn: false).noPreview)
    }

    @MainActor
    func testTextWithAttachmentKeepsAttachmentAndCancelledLinks() {
        let withAttachment = withFile(fetched(typed("LINK_URL/no-image"), nil))
        let changed = typed("LINK_URL/page", in: withAttachment)
        XCTAssertTrue(changed.attachmentPreview)
        XCTAssertEqual(changed.cancelledLinks, ["LINK_URL/no-image"])
        XCTAssertNil(changed.loadingRequest)
    }

    @MainActor
    func testFetchedPreviewIsShown() {
        XCTAssertEqual(status(fetched(typed("LINK_URL/page"), pagePreview)), .shown(linkPreview: pagePreview))
    }

    @MainActor
    func testFailedLinkIsCancelledAndNextLinkRequested() {
        let failed = fetched(typed("LINK_URL/no-image LINK_URL/page"), nil)
        XCTAssertEqual(failed.cancelledLinks, ["LINK_URL/no-image"])
        XCTAssertEqual(loadingUri(failed), "LINK_URL/page")
        let failedOnly = fetched(typed("LINK_URL/no-image"), nil)
        XCTAssertEqual(failedOnly.cancelledLinks, ["LINK_URL/no-image"])
        XCTAssertTrue(failedOnly.noPreview)
    }

    @MainActor
    func testResultOfAnotherRequestChangesNothing() throws {
        let loading = typed("LINK_URL/page")
        let request = try XCTUnwrap(loading.loadingRequest)
        let states = [loading, fetched(loading, pagePreview), typed("text without a link", in: loading), withFile(loading), ComposeState()]
        for s in states {
            for linkPreview in [pagePreview, nil] {
                let other = request == s.loadingRequest ? request + 1 : request
                let r = s.updateLinkPreview(.fetched(request: other, linkPreview: linkPreview), previewsOn: true)
                XCTAssertEqual(status(r), status(s))
                XCTAssertEqual(r.attachmentPreview, s.attachmentPreview)
                XCTAssertEqual(r.cancelledLinks, s.cancelledLinks)
            }
        }
    }

    @MainActor
    func testCancelledLinkIsNotRequestedAgain() {
        let shown = fetched(typed("LINK_URL/page"), pagePreview)
        let dismissed = shown.updateLinkPreview(.cancelled, previewsOn: true)
        XCTAssertEqual(dismissed.cancelledLinks, ["LINK_URL/page"])
        XCTAssertTrue(dismissed.noPreview)
        XCTAssertTrue(typed("LINK_URL/page", in: dismissed).noPreview)
        let loading = typed("LINK_URL/no-image LINK_URL/page")
        let next = loading.updateLinkPreview(.cancelled, previewsOn: true)
        XCTAssertEqual(next.cancelledLinks, ["LINK_URL/no-image"])
        XCTAssertEqual(loadingUri(next), "LINK_URL/page")
    }

    @MainActor
    func testChangedChatRequestsCancelledLinkAgain() {
        let failed = fetched(typed("LINK_URL/no-image"), nil)
        let opened = failed.updateLinkPreview(.chatChanged, previewsOn: true)
        XCTAssertEqual(opened.cancelledLinks, [])
        XCTAssertEqual(loadingUri(opened), "LINK_URL/no-image")
        XCTAssertEqual(status(opened.updateLinkPreview(.chatChanged, previewsOn: true)), status(opened))
    }

    @MainActor
    func testAttachmentKeepsCancelledLinksAndRemovedAttachmentShowsNoPreview() throws {
        let next = fetched(typed("LINK_URL/no-image LINK_URL/page"), nil)
        let request = try XCTUnwrap(next.loadingRequest)
        let withAttachment = withFile(next)
        XCTAssertEqual(withAttachment.cancelledLinks, ["LINK_URL/no-image"])
        XCTAssertTrue(withAttachment.updateLinkPreview(.fetched(request: request, linkPreview: pagePreview), previewsOn: true).attachmentPreview)
        let removed = withAttachment.copy(preview: .noPreview)
        XCTAssertTrue(removed.noPreview)
        XCTAssertEqual(removed.cancelledLinks, ["LINK_URL/no-image"])
        XCTAssertEqual(loadingUri(typed("LINK_URL/no-image LINK_URL/page", in: removed)), "LINK_URL/page")
    }

    @MainActor
    func testNewAndEditedComposeHaveNoCancelledLinks() {
        XCTAssertEqual(ComposeState().cancelledLinks, [])
        let edited = ComposeState(editingItem: ChatItem(
            chatDir: .directSnd,
            meta: CIMeta.getSample(1, .now, "LINK_URL/page"),
            content: .sndMsgContent(msgContent: .link(text: "LINK_URL/page", preview: pagePreview)),
            formattedText: parseSimpleXMarkdown("LINK_URL/page")
        ), previewsOn: true)
        XCTAssertEqual(edited.cancelledLinks, [])
        XCTAssertEqual(status(edited), .shown(linkPreview: pagePreview))
        XCTAssertTrue(ComposeState(editingItem: ChatItem.getSample(1, .directSnd, .now, "LINK_URL/page"), previewsOn: true).noPreview)
    }

    @MainActor
    func testEditedNoteChangesAsNewMessage() async throws {
        let (_, item) = try await recordedNote()
        guard case let .link(text, preview)? = item.content.msgContent else { return XCTFail("note without a link preview") }
        let edited = ComposeState(editingItem: item, previewsOn: true)
        let composed = fetched(fetched(typed(text), nil), preview)
        XCTAssertEqual(edited.cancelledLinks, ["LINK_URL/no-image"])
        assertSameLinkPreview(edited, composed)
        for next in [text, text + " ", "LINK_URL/page", "LINK_URL/no-image", "text without a link", ""] {
            assertSameLinkPreview(typed(next, in: edited), typed(next, in: composed), next)
        }
        assertSameLinkPreview(typed(text + " ", in: edited, previewsOn: false), typed(text + " ", in: composed, previewsOn: false))
        assertSameLinkPreview(typed("LINK_URL/no-image", in: typed("", in: edited)), typed("LINK_URL/no-image", in: typed("", in: composed)))
        assertSameLinkPreview(edited.updateLinkPreview(.cancelled, previewsOn: true), composed.updateLinkPreview(.cancelled, previewsOn: true))
    }

    @MainActor
    func testChangedChatKeepsLinksBeforeCardOfEditedNoteCancelled() async throws {
        let (_, item) = try await recordedNote()
        guard case let .link(text, preview)? = item.content.msgContent else { return XCTFail("note without a link preview") }
        let edited = ComposeState(editingItem: item, previewsOn: true)
        for s in [edited, typed(text + " ", in: edited)] {
            let opened = s.updateLinkPreview(.chatChanged, previewsOn: true)
            XCTAssertEqual(opened.cancelledLinks, ["LINK_URL/no-image"])
            XCTAssertEqual(status(opened), .shown(linkPreview: preview))
            XCTAssertEqual(opened.checkLinkPreview(), .link(text: text, preview: preview))
        }
        let dismissed = edited.updateLinkPreview(.cancelled, previewsOn: true)
        XCTAssertTrue(dismissed.noPreview)
        let reopened = dismissed.updateLinkPreview(.chatChanged, previewsOn: true)
        XCTAssertEqual(reopened.cancelledLinks, ["LINK_URL/no-image"])
        XCTAssertEqual(loadingUri(reopened), "LINK_URL/page")
    }

    @MainActor
    func testEditWithPreviewOfLinkAbsentFromTextSelectsLinkOfText() async throws {
        var (_, item) = try await recordedNote()
        let text = item.content.text
        item.content = .sndMsgContent(msgContent: .link(text: text, preview: LinkPreview(uri: "LINK_URL/other", title: "Other page", image: "")))
        let edited = ComposeState(editingItem: item, previewsOn: true)
        XCTAssertEqual(edited.cancelledLinks, [])
        XCTAssertEqual(loadingUri(edited), "LINK_URL/no-image")
        XCTAssertEqual(edited.checkLinkPreview(), .text(text))
        XCTAssertTrue(ComposeState(editingItem: item, previewsOn: false).noPreview)
    }

    @MainActor
    func testForwardToSameChatKeepsLinkPreviewAndSelectsLinkInPlaceOfAttachmentAtNextTextChange() {
        let items = [ChatItem.getSample(1, .directRcv, .now, "hello")]
        let from = ChatInfo.direct(contact: Contact.sampleData)
        let loading = fetched(typed("LINK_URL/no-image LINK_URL/page"), nil)
        let forwardedLoading = loading.forwardingItems(items, fromChatInfo: from, previewsOn: true)
        XCTAssertEqual(status(forwardedLoading), status(loading))
        XCTAssertEqual(forwardedLoading.cancelledLinks, ["LINK_URL/no-image"])
        let shown = fetched(loading, pagePreview)
        XCTAssertEqual(status(shown.forwardingItems(items, fromChatInfo: from, previewsOn: true)), .shown(linkPreview: pagePreview))
        let forwardedFile = withFile(shown).forwardingItems(items, fromChatInfo: from, previewsOn: true)
        XCTAssertTrue(forwardedFile.noPreview)
        XCTAssertEqual(forwardedFile.cancelledLinks, ["LINK_URL/no-image"])
        XCTAssertEqual(loadingUri(typed("LINK_URL/no-image LINK_URL/page ", in: forwardedFile)), "LINK_URL/page")
    }

    @MainActor
    func testLinkPreviewEventsKeepSendInProgress() throws {
        var loading = typed("LINK_URL/no-image LINK_URL/page")
        loading.inProgress = true
        let request = try XCTUnwrap(loading.loadingRequest)
        let events: [LinkPreviewEvent] = [.textChanged, .fetched(request: request, linkPreview: pagePreview), .fetched(request: request, linkPreview: nil), .cancelled, .chatChanged]
        for event in events {
            XCTAssertTrue(loading.updateLinkPreview(event, previewsOn: true).inProgress)
        }
    }

    @MainActor
    func testSentContentHasLinkPreviewExactlyWhenItIsShown() {
        let loading = typed("LINK_URL/no-image LINK_URL/page")
        let nextLoading = fetched(loading, nil)
        let nextShown = fetched(nextLoading, pagePreview)
        let states = [
            loading,
            nextLoading,
            nextShown,
            typed("LINK_URL/no-image LINK_URL/page ", in: nextShown),
            nextShown.updateLinkPreview(.cancelled, previewsOn: true),
            nextShown.updateLinkPreview(.chatChanged, previewsOn: true),
            fetched(typed("LINK_URL/page"), pagePreview),
            typed("text without a link", in: nextShown),
            typed("", in: nextShown)
        ]
        for s in states {
            let text = s.message.trimmingCharacters(in: .whitespacesAndNewlines)
            XCTAssertEqual(s.checkLinkPreview(), s.linkPreview.map { MsgContent.link(text: text, preview: $0) } ?? .text(text))
        }
        XCTAssertEqual(nextShown.checkLinkPreview(), .link(text: "LINK_URL/no-image LINK_URL/page", preview: pagePreview))
    }

    private let pagePreview = LinkPreview(uri: "LINK_URL/page", title: "Link preview title", description: "Link preview description", image: "")

    @MainActor
    private func typed(_ text: String, in s: ComposeState = ComposeState(), previewsOn: Bool = true) -> ComposeState {
        var s = s
        s.message = text
        s.parsedMessage = parseSimpleXMarkdown(text) ?? FormattedText.plain(text)
        return s.updateLinkPreview(.textChanged, previewsOn: previewsOn)
    }

    private func fetched(_ s: ComposeState, _ linkPreview: LinkPreview?) -> ComposeState {
        s.updateLinkPreview(.fetched(request: s.loadingRequest ?? 0, linkPreview: linkPreview), previewsOn: true)
    }

    private func withFile(_ s: ComposeState) -> ComposeState {
        s.copy(preview: .filePreview(fileName: "test.pdf", file: URL(fileURLWithPath: "test.pdf")))
    }

    private func status(_ s: ComposeState) -> LinkPreviewStatus? {
        if case let .linkPreview(status) = s.preview { status } else { nil }
    }

    private func loadingUri(_ s: ComposeState) -> String? {
        if case let .linkPreview(.loading(uri, _)) = s.preview { uri } else { nil }
    }

    private func assertSameLinkPreview(_ s: ComposeState, _ expected: ComposeState, _ message: String = "", file: StaticString = #filePath, line: UInt = #line) {
        XCTAssertEqual(loadingUri(s), loadingUri(expected), message, file: file, line: line)
        XCTAssertEqual(s.linkPreview, expected.linkPreview, message, file: file, line: line)
        XCTAssertEqual(s.cancelledLinks, expected.cancelledLinks, message, file: file, line: line)
        XCTAssertEqual(s.checkLinkPreview(), expected.checkLinkPreview(), message, file: file, line: line)
    }

    @MainActor
    private func recordedNote() async throws -> (command: String, item: ChatItem) {
        let command = try XCTUnwrap(commands.keys.first { $0.hasPrefix("/_create ") })
        let r: ChatResponse1 = try await chatSendCmd(.string(command))
        let items: [AChatItem]? = if case let .newChatItems(_, chatItems) = r { chatItems } else { nil }
        return (command, try XCTUnwrap(items?.first?.chatItem, r.responseType))
    }

    @MainActor
    private func attachFile(_ compose: ComposeHostState) {
        compose.composeState = withFile(compose.composeState)
    }

    @MainActor
    private func typeWithAttachment(_ compose: ComposeHostState) async throws {
        attachFile(compose)
        try await Task.sleep(nanoseconds: 200_000_000)
        compose.composeState.message = "text without a link"
        try await waitFor { compose.composeState.parsedMessage.first?.text == "text without a link" }
        compose.composeState = compose.composeState.copy(preview: .noPreview)
        try await Task.sleep(nanoseconds: 200_000_000)
    }

    private func hold(_ cmd: String) -> DispatchSemaphore {
        let gate = DispatchSemaphore(value: 0)
        locked { held[cmd] = gate }
        return gate
    }

    private func recorded(_ cmd: String, _ commands: [String: Any]) -> (key: String, response: Any)? {
        if let response = commands[cmd] { return (cmd, response) }
        guard let sent = jsonCommand(cmd) else { return nil }
        for (key, response) in commands {
            if let command = jsonCommand(key), command.prefix == sent.prefix, command.json.isEqual(sent.json) {
                return (key, response)
            }
        }
        return nil
    }

    private func jsonCommand(_ cmd: String) -> (prefix: Substring, json: NSObject)? {
        guard let r = cmd.range(of: " json "),
              let json = try? JSONSerialization.jsonObject(with: Data(cmd[r.upperBound...].utf8)) as? NSObject
        else { return nil }
        return (cmd[..<r.lowerBound], json)
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
    private func showCompose(_ compose: ComposeHostState = ComposeHostState()) async throws -> ComposeHostState {
        self.window?.isHidden = true
        let window = UIWindow(windowScene: try XCTUnwrap(UIApplication.shared.connectedScenes.compactMap { $0 as? UIWindowScene }.first))
        window.rootViewController = UIHostingController(rootView: ComposeHost(state: compose))
        window.isHidden = false
        self.window = window
        try await Task.sleep(nanoseconds: 100_000_000)
        return compose
    }

    @MainActor
    private func show(_ compose: ComposeHostState) async throws {
        compose.shown = true
        try await Task.sleep(nanoseconds: 100_000_000)
    }
}

private final class ComposeHostState: ObservableObject {
    let chat: Chat
    let chatModel = ChatModel()
    @Published var shown = true
    @Published var composeState = ComposeState()
    @Published var showCommandsMenu = false
    @Published var keyboardVisible = false
    @Published var keyboardHiddenDate = Date.now
    @Published var selectedRange = NSRange()

    init(chat: Chat = Chat(chatInfo: .direct(contact: Contact.sampleData))) {
        self.chat = chat
    }
}

private struct ComposeHost: View {
    @ObservedObject var state: ComposeHostState
    let theme = CurrentColors.toAppTheme()

    var body: some View {
        if state.shown {
            ComposeView(
                chat: state.chat,
                im: ItemsModel.shared,
                composeState: $state.composeState,
                showCommandsMenu: $state.showCommandsMenu,
                keyboardVisible: $state.keyboardVisible,
                keyboardHiddenDate: $state.keyboardHiddenDate,
                selectedRange: $state.selectedRange
            )
            .environmentObject(state.chatModel)
            .environmentObject(theme)
        }
    }
}
