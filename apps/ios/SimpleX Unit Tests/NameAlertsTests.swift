import XCTest
import SwiftUI
import UIKit
@testable import SimpleX
@testable import SimpleXChat

final class NameAlertsTests: XCTestCase {
    private let sendCmd = sendSimpleXCmdStr
    private let parseMarkdown = parseSimpleXMarkdownText
    private let parseUri = parseSanitizeUri
    private let lock = NSLock()
    private var unrecorded: [String] = []
    private var shownLocalChats: [String] = []
    private var opened = 0

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

    @MainActor
    func testKnownNameExpired() async throws {
        try recorded("names/plan-known-expired")
        connect("@alice.simplex")
        try await assertTexts(["Name expired", "alice.simplex expired on January 1, 2026. Its owner can renew it until January 1, 2099.", "Open chat", "Ok"])
        XCTAssertEqual(shownLocalChats, ["@2"])
        try await press("Open chat")
        try await waitFor { ChatModel.shared.chatId == "@2" }
        try await assertRequested { $0.hasPrefix("/_get chat @2 ") }
        XCTAssertEqual(opened, 1)
    }

    @MainActor
    func testOwnNameExpired() async throws {
        try recorded("names/plan-own-expired")
        connect("@alice.simplex")
        try await assertTexts(["Your name has expired", "Your name alice.simplex expired on January 1, 2026. Renew it before January 1, 2099.", "Renew", "Ok"])
        XCTAssertEqual(shownLocalChats, [])
        try await press("Renew")
        try await assertTexts(["Open link?", "https://simplex.domains/#testing", "Cancel", "Open"])
    }

    @MainActor
    func testKnownNameAvailable() async throws {
        try recorded("names/plan-known-available")
        connect("@alice.simplex")
        try await assertTexts(["Name no longer registered", "alice.simplex is available for registration for $20 for 2 years.", "Register", "Open chat", "Ok"])
        XCTAssertEqual(shownLocalChats, ["@2"])
        try await press("Register")
        try await assertTexts(["Open link?", "https://simplex.domains/#testing", "Cancel", "Open"])
    }

    @MainActor
    func testOwnNameAvailable() async throws {
        try recorded("names/plan-own-available")
        connect("@alice.simplex")
        try await assertTexts(["Your name has expired", "Your name alice.simplex is no longer registered. It is available for registration for $20 for 2 years.", "Re-register", "Ok"])
        try await press("Re-register")
        try await assertTexts(["Open link?", "https://simplex.domains/#testing", "Cancel", "Open"])
    }

    @MainActor
    func testKnownNameReserved() async throws {
        try recorded("names/plan-known-reserved")
        connect("@alice.simplex")
        try await assertTexts(["Name not registered", "alice.simplex is reserved for community. Please contact SimpleX Chat if you are interested in using it.", "Connect to SimpleX team", "Open chat", "Ok"])
        XCTAssertEqual(shownLocalChats, ["@2"])
        try await press("Connect to SimpleX team")
        try await waitFor { ChatModel.shared.appOpenUrl == simplexTeamURL }
    }

    @MainActor
    func testKnownNameMoved() async throws {
        try recorded("names/plan-known-moved")
        connect("@alice.simplex")
        try await assertTexts(["cath", "@alice.simplex", "Open new chat", "Cancel"])
        XCTAssertEqual(shownLocalChats, ["@2"])
        try await press("Open new chat")
        try await assertRequested { $0.hasPrefix("/_prepare contact 1 ") && $0.contains(" domain=alice.simplex ") }
        try await assertTexts(["Error opening chat", "no data", "Ok"])
    }

    @MainActor
    func testNameAvailable() async throws {
        try recorded("names/error-available")
        connect("@nobody.simplex")
        try await assertTexts(["Name not registered", "nobody.simplex is available for registration for $20 for 2 years.", "Register", "Ok"])
    }

    @MainActor
    func testNameNotRegistered() async throws {
        try recorded("names/error-not-registered")
        connect("acme.simplex")
        try await assertTexts(["Name not registered", "This SimpleX name is not registered. Please check the name.", "Ok"])
    }

    @MainActor
    func testKnownName() async throws {
        try recorded("names/plan-known")
        connect("@alice.simplex")
        try await assertTexts(["alice", "@alice.simplex", "Open chat", "Cancel"])
        XCTAssertEqual(shownLocalChats, ["@2"])
    }

    @MainActor
    func testNameMovedToKnownContact() async throws {
        try recorded("names/plan-moved-known")
        connect("@alice.simplex")
        try await assertTexts(["cath", "@alice.simplex", "Open chat", "Cancel"])
        XCTAssertEqual(shownLocalChats, ["@3"])
    }

    @MainActor
    func testNameMovedToKnownBusiness() async throws {
        try recorded("names/plan-moved-business")
        connect("@alice.simplex")
        try await assertTexts(["cath", "@alice.simplex", "Open chat", "Cancel"])
        XCTAssertEqual(shownLocalChats, ["#1"])
    }

    @MainActor
    func testNameOfOtherKindOffered() async throws {
        try recorded("names/plan-other-kind")
        connect("team.simplex")
        try await assertTexts(["team", "#team", "1 subscribers", "You are a subscriber", "Open channel", "Connect to @team.simplex", "Cancel"])
        XCTAssertEqual(shownLocalChats, ["#1"])
        try await press("Connect to @team.simplex")
        try await assertTexts(["alisa", "@team.simplex", "Open new chat", "Cancel"])
        XCTAssertNotNil(ChatModel.shared.getChat("@2"))
    }

    @MainActor
    func testChannelWithoutRelays() async throws {
        try recorded("names/plan-channel-no-relays")
        connect("team.simplex")
        try await assertTexts(["team", "Channel has no active relays. Please try to join later.", "Connect to @team.simplex", "OK"])
        XCTAssertEqual(shownLocalChats, [])
        try await press("Connect to @team.simplex")
        try await assertTexts(["Unconfirmed name", "The SimpleX name team.simplex is registered, but not added to profile. Please add it to your address or channel profile, if you are the owner.", "Ok"])
    }

    @MainActor
    func testOwnChannel() async throws {
        try recorded("names/plan-own-channel")
        connect("#team.simplex")
        try await assertTexts(["Your channel", "This is your link for channel team (#team)!", "Open channel", "Cancel"])
        XCTAssertEqual(shownLocalChats, ["#1"])
    }

    @MainActor
    func testKnownNameExpiredWithoutGrace() async throws {
        try recorded("names/plan-known-expired-no-grace")
        connect("@alice.simplex")
        try await assertTexts(["Name expired", "alice.simplex expired on January 1, 2026.", "Open chat", "Ok"])
        XCTAssertEqual(shownLocalChats, ["@2"])
    }

    @MainActor
    func testOwnNameExpiredWithoutGrace() async throws {
        try recorded("names/plan-own-expired-no-grace")
        connect("@alice.simplex")
        try await assertTexts(["Your name has expired", "Your name alice.simplex expired on January 1, 2026.", "Renew", "Ok"])
    }

    @MainActor
    func testOwnChannelNameExpired() async throws {
        try recorded("names/plan-own-channel-expired")
        connect("#team.simplex")
        try await assertTexts(["Your name has expired", "Your name team.simplex expired on January 1, 2026. Renew it before January 1, 2099.", "Renew", "Open chat", "Ok"])
        XCTAssertEqual(shownLocalChats, ["#1"])
    }

    @MainActor
    func testChannelNameMoved() async throws {
        try recorded("names/plan-channel-moved")
        connect("#team.simplex")
        try await assertTexts(["teamv2", "#team", "1 subscribers", "Open channel", "Cancel"])
        XCTAssertEqual(shownLocalChats, ["#1"])
    }

    @MainActor
    func testMovedChannel() async throws {
        try recorded("names/plan-old-channel")
        connect("TEAM_LINK")
        try await assertTexts(["team", "1 subscribers", "You are a subscriber", "Open channel", "Cancel"])
        XCTAssertEqual(shownLocalChats, ["#1"])
        guard case let .group(groupInfo, _)? = ChatModel.shared.getGroupChat(1)?.chatInfo else { return XCTFail("channel not listed") }
        XCTAssertEqual(groupInfo.groupDomainVerified, .moved)
    }

    @MainActor
    func testChannelNameNotVerified() async throws {
        try recorded("names/plan-known-failed")
        connect("TEAM_LINK")
        try await assertTexts(["team", "2 subscribers", "You are a subscriber", "Open channel", "Cancel"])
        XCTAssertEqual(shownLocalChats, ["#1"])
        guard case let .group(groupInfo, _)? = ChatModel.shared.getGroupChat(1)?.chatInfo else { return XCTFail("channel not listed") }
        XCTAssertEqual(groupInfo.groupDomainVerified, .failed)
    }

    @MainActor
    func testContactViaAddress() async throws {
        try recorded("connect/contact-via-address")
        connect("CONTACT_LINK")
        try await assertTexts(["alice", "Open chat", "Cancel"])
        XCTAssertEqual(shownLocalChats, ["@3"])
    }

    @MainActor
    func testGroupLinkAsAddress() async throws {
        try recorded("connect/group-link-as-address")
        connect("GROUP_LINK")
        try await assertTexts(["Invalid connection link", "Please check that you used the correct link or ask your contact to send you another one.", "Ok"])
    }

    @MainActor
    func testGroupLinkAsChannel() async throws {
        try recorded("connect/group-link-as-channel")
        connect("GROUP_LINK")
        try await assertTexts(["Invalid connection link", "Please check that you used the correct link or ask your contact to send you another one.", "Ok"])
    }

    @MainActor
    func testRejectedContactInChatList() async throws {
        try recorded("connect/request-rejected")
        ChatModel.shared.updateChats(try apiGetChats())
        guard case let .direct(contact)? = ChatModel.shared.getContactChat(2)?.chatInfo else { return XCTFail("contact not decoded: \(ChatModel.shared.chats.map(\.id))") }
        XCTAssertEqual(contact.contactStatus.rawValue, "rejected")
        XCTAssertFalse(contact.active)
    }

    @MainActor
    func testNameMovedEvent() async throws {
        var contact = Contact.sampleData
        contact.contactId = 2
        ChatModel.shared.addChat(Chat(chatInfo: .direct(contact: contact), chatItems: []))
        ChatModel.shared.addChat(Chat(chatInfo: .group(groupInfo: GroupInfo.sampleData, groupChatScope: nil), chatItems: []))
        let event: APIResult<ChatEvent> = decodeAPIResult(try JSONSerialization.data(withJSONObject: singleFieldJSON(try fixtureJSON("names/event-name-moved"))))
        guard case let .result(ev) = event else { return XCTFail("event not decoded: \(event.responseType)") }
        XCTAssertEqual(ev.responseType, "nameMoved")
        XCTAssertEqual(ev.details, "userId: 1\ncontactIds: [2]\ngroupIds: [1]")
        await processReceivedMsg(ev)
        guard case let .direct(movedContact)? = ChatModel.shared.getContactChat(2)?.chatInfo,
              case let .group(movedGroup, _)? = ChatModel.shared.getGroupChat(1)?.chatInfo else { return XCTFail("chats removed") }
        XCTAssertEqual(movedContact.profile.contactDomainVerified, .moved)
        XCTAssertEqual(movedGroup.groupDomainVerified, .moved)
    }

    @MainActor
    func testChatListSearchShowsChatsOfName() async throws {
        try recorded("names/search-team")
        let state = SearchState()
        let window = UIWindow(windowScene: try XCTUnwrap(UIApplication.shared.connectedScenes.compactMap { $0 as? UIWindowScene }.first))
        window.rootViewController = UIHostingController(rootView: SearchHost(state: state))
        window.isHidden = false
        defer { window.isHidden = true }
        try await Task.sleep(nanoseconds: 100_000_000)
        state.searchText = "team.simplex"
        try await waitFor { state.filtered == ["@2", "#1"] }
        XCTAssertEqual(Set(ChatModel.shared.chats.map(\.id)), ["@2", "#1"])
    }

    private func recorded(_ fixture: String) throws {
        let responses = try XCTUnwrap(try fixtureJSON(fixture) as? [String: [String: Any]])
        let commands = responses["commands"] ?? [:]
        let markdown = responses["markdown"] ?? [:]
        let uris = responses["uris"] ?? [:]
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

    private func unrecordedRequest(_ request: String) {
        locked { unrecorded.append(request) }
    }

    private func requests() -> [String] {
        locked { unrecorded }
    }

    private func locked<T>(_ f: () -> T) -> T {
        lock.lock()
        defer { lock.unlock() }
        return f()
    }

    @MainActor
    private func connect(_ name: String) {
        planAndConnect(name, theme: CurrentColors.toAppTheme(), dismiss: false, showLocalChats: { [unowned self] chats in self.shownLocalChats += chats.map(\.id) }, onOpen: { [unowned self] in self.opened += 1 })
    }

    @MainActor
    private func assertRequested(_ expected: (String) -> Bool, file: StaticString = #filePath, line: UInt = #line) async throws {
        try? await waitFor { !self.requests().isEmpty }
        let shown = requests()
        XCTAssertEqual(shown.count, 1, "\(shown)", file: file, line: line)
        XCTAssertTrue(shown.allSatisfy(expected), "\(shown)", file: file, line: line)
        locked { unrecorded.removeAll() }
    }

    @MainActor
    private func assertTexts(_ texts: [String], file: StaticString = #filePath, line: UInt = #line) async throws {
        try? await waitFor { self.shownTexts().sorted() == texts.sorted() }
        let shown = shownTexts()
        XCTAssertEqual(shown.sorted(), texts.sorted(), "\(shown)", file: file, line: line)
    }

    @MainActor
    private func press(_ title: String, file: StaticString = #filePath, line: UInt = #line) async throws {
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
                await dismiss(shown)
            }
        }
    }

    @MainActor
    private func dismiss(_ shown: UIViewController) async {
        await withCheckedContinuation { done in shown.dismiss(animated: false) { done.resume() } }
    }

    @MainActor
    private func shownTexts() -> [String] {
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

private final class SearchState: ObservableObject {
    @Published var searchMode = false
    @Published var searchText = ""
    @Published var showingLink = false
    @Published var filtered: Set<String> = []
    @Published var parentSheet: SomeSheet<AnyView>? = nil
}

private struct SearchHost: View {
    @ObservedObject var state: SearchState
    @FocusState private var focused: Bool

    var body: some View {
        ChatListSearchBar(
            searchMode: $state.searchMode,
            searchFocussed: $focused,
            searchText: $state.searchText,
            searchShowingSimplexLink: $state.showingLink,
            searchChatFilteredBySimplexLink: $state.filtered,
            parentSheet: $state.parentSheet
        )
        .environmentObject(ChatModel.shared)
        .environmentObject(ChatTagsModel.shared)
        .environmentObject(CurrentColors.toAppTheme())
    }
}

func fixtureJSON(_ fixture: String) throws -> Any {
    let url = URL(fileURLWithPath: #filePath).deletingLastPathComponent().appendingPathComponent("../../../tests/fixtures/api/\(fixture).json")
    return try JSONSerialization.jsonObject(with: Data(contentsOf: url))
}

func singleFieldJSON(_ value: Any) -> Any {
    if let object = value as? [String: Any] {
        var fields = object.mapValues(singleFieldJSON)
        guard let tag = object["type"] as? String, !msgContentTypes.contains(tag) else { return fields }
        fields["type"] = nil
        return ["_owsf": true, tag: fields]
    } else if let array = value as? [Any] {
        return array.map(singleFieldJSON)
    } else {
        return value
    }
}

private let msgContentTypes: Set<String> = ["text", "link", "image", "video", "voice", "file", "report", "chat"]

@MainActor
func waitFor(_ condition: @escaping () -> Bool) async throws {
    for _ in 0..<600 {
        if condition() { return }
        try await Task.sleep(nanoseconds: 50_000_000)
    }
    throw XCTestError(.timeoutWhileWaiting)
}
