import XCTest
import SwiftUI
import UIKit
@testable import SimpleX
@testable import SimpleXChat

final class ChatItemsTests: XCTestCase {
    private let sendCmd = sendSimpleXCmdStr
    private let parseMarkdown = parseSimpleXMarkdownText
    private let parseUri = parseSanitizeUri
    private let acceptImages = privacyAcceptImagesGroupDefault.get()
    private let lock = NSLock()
    private var unrecorded: [String] = []

    @MainActor
    override func setUp() async throws {
        privacyAcceptImagesGroupDefault.set(true)
        ChatModel.shared.currentUser = User.sampleData
        sendSimpleXCmdStr = { [unowned self] cmd, _, _ in self.unrecordedRequest(cmd); return nil }
        parseSimpleXMarkdownText = { [unowned self] str in self.unrecordedRequest("markdown \(str)"); return nil }
        parseSanitizeUri = { [unowned self] str, _ in self.unrecordedRequest("uri \(str)"); return nil }
    }

    @MainActor
    override func tearDown() async throws {
        for chat in ChatModel.shared.chats { ChatModel.shared.removeChat(chat.id) }
        ChatModel.shared.currentUser = nil
        sendSimpleXCmdStr = sendCmd
        parseSimpleXMarkdownText = parseMarkdown
        parseSanitizeUri = parseUri
        privacyAcceptImagesGroupDefault.set(acceptImages)
        XCTAssertEqual(locked { unrecorded }, [], "requests without recorded responses")
    }

    @MainActor
    func testFilesOfBlockedMembersAreNotReceived() async throws {
        let (user, chat) = try recordedChat("files/images-of-blocked-members", "/_get chat #1 count=6")
        await processReceivedMsg(.newChatItems(user: user, chatItems: chat.chatItems.map { AChatItem(chatInfo: chat.chatInfo, chatItem: $0) }))
        try await waitFor { self.receivedFiles().contains("/freceive 3") }
        try await Task.sleep(nanoseconds: 500_000_000)
        XCTAssertEqual(receivedFiles(), ["/freceive 3"])
        locked { unrecorded.removeAll() }
    }

    func testReceivedChannelCardIsNotFromOwner() throws {
        let cards = try recordedChat("chat-links/channel-card-received", "/_get chat @2 count=1").chat.chatItems
        XCTAssertEqual(cards.map(ownerSigned), [true])
        XCTAssertEqual(cards.map(\.chatLinkSigned), [false])
    }

    func testSentChannelCardIsFromOwner() throws {
        let cards = try recordedChat("chat-links/channel-card-sent", "/_get chat @2 count=1").chat.chatItems
        XCTAssertEqual(cards.map(ownerSigned), [true])
        XCTAssertEqual(cards.map(\.chatLinkSigned), [true])
    }

    @MainActor
    func testReceivedChannelCardShownNotFromOwner() throws {
        let cards = try recordedChat("chat-links/channel-card-received", "/_get chat @2 count=1").chat.chatItems
        XCTAssertEqual(try cards.map { try ownerSigShown($0) }, [false])
    }

    @MainActor
    func testSentChannelCardShownFromOwner() throws {
        let cards = try recordedChat("chat-links/channel-card-sent", "/_get chat @2 count=1").chat.chatItems
        XCTAssertEqual(try cards.map { try ownerSigShown($0) }, [true])
    }

    private func recordedChat(_ fixture: String, _ command: String) throws -> (user: UserRef, chat: ChatData) {
        let url = URL(fileURLWithPath: #filePath).deletingLastPathComponent().appendingPathComponent("../../../tests/fixtures/api/\(fixture).json")
        let responses = try XCTUnwrap(try JSONSerialization.jsonObject(with: Data(contentsOf: url)) as? [String: [String: Any]])
        let response = try XCTUnwrap(responses["commands"]?[command])
        let r: ChatResponse0 = try apiResult(decodeAPIResult(try JSONSerialization.data(withJSONObject: singleFieldJSON(response))))
        guard case let .apiChat(user, chat, _) = r else { throw r.unexpected }
        return (user, chat)
    }

    private func ownerSigned(_ item: ChatItem) -> Bool {
        if case let .chat(_, _, ownerSig) = item.content.msgContent { ownerSig != nil } else { false }
    }

    @MainActor
    private func ownerSigShown(_ item: ChatItem) throws -> Bool? {
        guard case let .chat(text, chatLink, _?) = item.content.msgContent else { return nil }
        let unsignedContent = MsgContent.chat(text: text, chatLink: chatLink, ownerSig: nil)
        var unsigned = item
        unsigned.content = item.chatDir.sent ? .sndMsgContent(msgContent: unsignedContent) : .rcvMsgContent(msgContent: unsignedContent)
        let hasText = text != chatLink.connLinkStr
        return try viewImage(CIChatLinkHeader(chatItem: item, chatLink: chatLink, hasText: hasText))
            != viewImage(CIChatLinkHeader(chatItem: unsigned, chatLink: chatLink, hasText: hasText))
    }

    @MainActor
    private func viewImage(_ view: some View) throws -> Data {
        let window = UIWindow(windowScene: try XCTUnwrap(UIApplication.shared.connectedScenes.compactMap { $0 as? UIWindowScene }.first))
        window.overrideUserInterfaceStyle = .light
        window.rootViewController = UIHostingController(rootView: view.environmentObject(CurrentColors.toAppTheme()))
        window.frame = CGRect(x: 0, y: 0, width: 300, height: 200)
        window.isHidden = false
        RunLoop.main.run(until: Date().addingTimeInterval(0.6))
        let image = UIGraphicsImageRenderer(bounds: window.bounds).image { _ in
            _ = window.drawHierarchy(in: window.bounds, afterScreenUpdates: true)
        }
        window.isHidden = true
        window.rootViewController = nil
        return try XCTUnwrap(image.pngData())
    }

    private func unrecordedRequest(_ request: String) {
        locked { unrecorded.append(request) }
    }

    private func receivedFiles() -> [String] {
        locked { unrecorded.map { $0.split(separator: " ").prefix(2).joined(separator: " ") } }
    }

    private func locked<T>(_ f: () -> T) -> T {
        lock.lock()
        defer { lock.unlock() }
        return f()
    }

    @MainActor
    private func waitFor(_ condition: @escaping () -> Bool) async throws {
        for _ in 0..<600 {
            if condition() { return }
            try await Task.sleep(nanoseconds: 50_000_000)
        }
        throw XCTestError(.timeoutWhileWaiting)
    }
}
