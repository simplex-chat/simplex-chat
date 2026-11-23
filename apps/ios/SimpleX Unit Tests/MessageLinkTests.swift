import XCTest
import UIKit
@testable import SimpleX
@testable import SimpleXChat

final class MessageLinkTests: XCTestCase {
    private let sendCmd = sendSimpleXCmdStr
    private let lock = NSLock()
    private var unrecorded: [String] = []

    @MainActor
    override func setUp() async throws {
        ChatModel.shared.currentUser = User.sampleData
    }

    @MainActor
    override func tearDown() async throws {
        ChatModel.shared.currentUser = nil
        sendSimpleXCmdStr = sendCmd
    }

    func testMessageLinkIsParsed() throws {
        XCTAssertEqual(try parsedMarkdown("links/same-group", "see [q](GROUP_MSG_LINK)").formattedText, [
            FormattedText(text: "see "),
            FormattedText(text: "[q](GROUP_MSG_LINK)", format: .messageLink(showText: "q", msgLinkType: .group, msgLink: "GROUP_MSG_LINK"))
        ])
    }

    func testItemShowsLinkWithShortText() throws {
        let item = try lastItem("links/same-group", "/_get chat #1 count=1")
        XCTAssertEqual(item.formattedText?.last?.format, Format.messageLink(showText: "q", msgLinkType: .group, msgLink: "GROUP_MSG_LINK"))
        XCTAssertEqual(item.links, ["q": CILink(text: "Approve the deploy plan?", target: nil)])
        let r = messageText(item.text, item.formattedText, sender: nil, mentions: nil, links: item.links, userMemberId: nil, showSecrets: nil, backgroundColor: .white)
        XCTAssertEqual(r.string.string, "see q (Approve the deploy plan?)")
        XCTAssertTrue(r.handleTaps)
    }

    @MainActor
    func testLinkToSameChatOpensItem() async throws {
        let plan = try linkPlan("links/same-group", "/_msg link plan 1 GROUP_MSG_LINK chat=#1")
        guard case let .item(chatItem, linkText) = plan else { return XCTFail("not an item: \(plan)") }
        XCTAssertEqual(linkText, "Approve the deploy plan?")
        XCTAssertEqual(chatItem.chatItem.text, "Approve the deploy plan?")
        XCTAssertEqual(msgLinkAction(plan, target: nil), .openItem(chatId: "#1", itemId: chatItem.chatItem.id))
        try recorded("links/same-group")
        openMsgLink("GROUP_MSG_LINK", .group, chatId: "#1", target: nil, theme: CurrentColors.toAppTheme())
        try await assertRequested(["/_get chat #1 around=\(chatItem.chatItem.id) count=50"])
    }

    @MainActor
    func testLinkToOtherChatOpensTargetOfSenderCopy() async throws {
        let item = try lastItem("links/other-group-sender", "/_get chat #2 count=1")
        XCTAssertEqual(item.formattedText?.last?.format, Format.messageLink(showText: "q", msgLinkType: .unknownSource, msgLink: "simplex:/m"))
        let link = try XCTUnwrap(item.links?["q"])
        XCTAssertEqual(link.text, "Approve the deploy plan?")
        let target = try XCTUnwrap(link.target)
        XCTAssertEqual(target.chatType, .group)
        XCTAssertEqual(target.chatId, 1)
        XCTAssertEqual(msgLinkAction(.unknownSource, target: target), .openItem(chatId: "#1", itemId: target.chatItemId))
        try recorded("links/other-group-sender")
        openMsgLink("simplex:/m", .unknownSource, chatId: "#2", target: target, theme: CurrentColors.toAppTheme())
        try await assertRequested(["/_get chat #1 around=\(target.chatItemId) count=50"])
    }

    func testLinkToOtherChatIsFromUnknownSourceForRecipient() throws {
        let item = try lastItem("links/other-group-recipient", "/_get chat #2 count=1")
        XCTAssertEqual(item.formattedText?.last?.format, Format.messageLink(showText: "q", msgLinkType: .unknownSource, msgLink: "simplex:/m"))
        XCTAssertEqual(item.links, ["q": CILink(text: "Approve the deploy plan?", target: nil)])
        XCTAssertEqual(msgLinkAction(try linkPlan("links/same-group", "/_msg link plan 1 simplex:/m"), target: nil), .alert(title: "This link is from an unknown source."))
    }

    func testLinkFromUnknownSourceIsMuted() throws {
        XCTAssertEqual(labelColor(try lastItem("links/other-group-recipient", "/_get chat #2 count=1")), uiLinkColor.withAlphaComponent(0.7))
    }

    func testSendersLinkToOtherChatIsNotMuted() throws {
        XCTAssertEqual(labelColor(try lastItem("links/other-group-sender", "/_get chat #2 count=1")), uiLinkColor)
    }

    @MainActor
    func testCopyLinkCopiesWithoutConfirmation() async throws {
        let chat = try lastChat("links/same-group", "/_get chat #1 count=1")
        try recorded("links/same-group")
        UIPasteboard.general.string = ""
        await copyItemLink(chat.chatInfo, try XCTUnwrap(chat.chatItems.last))
        XCTAssertEqual(UIPasteboard.general.string, "ITEM_LINK")
        XCTAssertFalse(getTopViewController() is UIAlertController)
    }

    func testLinkNotInChatsShowsAlert() throws {
        let plan = try linkPlan("links/same-group", "/_msg link plan 1 simplex:/m#g/YWJjZGVmZ2hpamtsbW5vcA?a=MTIzNDU2Nzg5MDEyMzQ1Ng")
        XCTAssertEqual(msgLinkAction(plan, target: nil), .alert(title: "This message is not in your chats."))
    }

    func testLinkOfLaterVersionAsksForUpdate() throws {
        XCTAssertEqual(msgLinkAction(try linkPlan("links/same-group", "/_msg link plan 1 simplex:/m#x/abc"), target: nil), .alert(title: "Update the app to open this link."))
    }

    func testChannelPostLinkOfNonSubscriberJoinsChannel() throws {
        XCTAssertEqual(msgLinkAction(try linkPlan("links/channel", "/_msg link plan 1 CHANNEL_MSG_LINK"), target: nil), .connect(target: "CHANNEL_LINK"))
    }

    func testPublicChatMessageLinkCountsAsSimplexLink() throws {
        XCTAssertTrue(parsedMsgHasSimplexLink(try XCTUnwrap(parsedMarkdown("links/channel", "CHANNEL_MSG_LINK").formattedText)))
        XCTAssertFalse(parsedMsgHasSimplexLink(try XCTUnwrap(parsedMarkdown("links/same-group", "see [q](GROUP_MSG_LINK)").formattedText)))
    }

    func testChatWithoutLinkedMessageOpensChat() throws {
        let plan = try linkPlan("links/same-group", "/_msg link plan 1 GROUP_MSG_LINK chat=#1")
        guard case let .item(chatItem, _) = plan else { return XCTFail("not an item: \(plan)") }
        XCTAssertEqual(msgLinkAction(.chat(chatInfo: chatItem.chatInfo), target: nil), .openChat(chatId: "#1"))
    }

    private func fixtureCommands(_ fixture: String) throws -> [String: Any] {
        try XCTUnwrap((try fixtureJSON(fixture) as? [String: [String: Any]])?["commands"])
    }

    private func response(_ fixture: String, _ cmd: String) throws -> ChatResponse0 {
        let json = try XCTUnwrap(try fixtureCommands(fixture)[cmd], "\(cmd) not recorded in \(fixture)")
        let decoded: APIResult<ChatResponse0> = decodeAPIResult(try JSONSerialization.data(withJSONObject: singleFieldJSON(json)))
        let result: ChatResponse0? = if case let .result(r) = decoded { r } else { nil }
        return try XCTUnwrap(result, "response not decoded: \(decoded.responseType)")
    }

    private func linkPlan(_ fixture: String, _ cmd: String) throws -> MsgLinkPlan {
        let r = try response(fixture, cmd)
        let plan: MsgLinkPlan? = if case let .msgLinkPlan(_, p) = r { p } else { nil }
        return try XCTUnwrap(plan, "not a message link plan: \(r.responseType)")
    }

    private func lastChat(_ fixture: String, _ cmd: String) throws -> ChatData {
        let r = try response(fixture, cmd)
        let chat: ChatData? = if case let .apiChat(_, chat, _) = r { chat } else { nil }
        return try XCTUnwrap(chat, "no chat: \(r.responseType)")
    }

    private func lastItem(_ fixture: String, _ cmd: String) throws -> ChatItem {
        try XCTUnwrap(try lastChat(fixture, cmd).chatItems.last, "no chat item in \(fixture)")
    }

    private func labelColor(_ item: ChatItem) -> UIColor? {
        let r = messageText(item.text, item.formattedText, sender: nil, mentions: nil, links: item.links, userMemberId: nil, showSecrets: nil, backgroundColor: .white)
        return r.string.attribute(.foregroundColor, at: (r.string.string as NSString).range(of: "q").location, effectiveRange: nil) as? UIColor
    }

    private func parsedMarkdown(_ fixture: String, _ text: String) throws -> ParsedMarkdown {
        let json = try XCTUnwrap((try fixtureJSON(fixture) as? [String: [String: Any]])?["markdown"]?[text], "\(text) not recorded in \(fixture)")
        return try jsonDecoder.decode(ParsedMarkdown.self, from: JSONSerialization.data(withJSONObject: singleFieldJSON(json)))
    }

    private func recorded(_ fixture: String) throws {
        let commands = try fixtureCommands(fixture)
        sendSimpleXCmdStr = { [unowned self] cmd, _, _ in
            guard let response = commands[cmd] else {
                self.locked { self.unrecorded.append(cmd) }
                return nil
            }
            return try? JSONSerialization.data(withJSONObject: singleFieldJSON(response))
        }
    }

    @MainActor
    private func assertRequested(_ expected: [String], file: StaticString = #filePath, line: UInt = #line) async throws {
        try? await waitFor { self.locked { self.unrecorded == expected } }
        XCTAssertEqual(locked { unrecorded }, expected, file: file, line: line)
    }

    private func locked<T>(_ f: () -> T) -> T {
        lock.lock()
        defer { lock.unlock() }
        return f()
    }
}
