import XCTest
@testable import SimpleX
@testable import SimpleXChat

final class SuspendChatTests: XCTestCase {
    private let sendCmd = sendSimpleXCmdStr
    private let closeStore = chatCloseStore
    private let appState = AppChatState.shared.value
    private let nseState = nseStateGroupDefault.get()
    private let seState = seStateGroupDefault.get()
    private var sent: [String] = []
    private var closed = 0

    @MainActor
    override func setUp() async throws {
        let commands = try XCTUnwrap((try fixtureJSON("suspend/suspend-instantly") as? [String: [String: Any]])?["commands"])
        sendSimpleXCmdStr = { [unowned self] cmd, _, _ in
            self.sent.append("\(AppChatState.shared.value.rawValue): \(cmd)")
            return commands[cmd].flatMap { try? JSONSerialization.data(withJSONObject: singleFieldJSON($0)) }
        }
        chatCloseStore = { [unowned self] in self.closed += 1 }
    }

    @MainActor
    override func tearDown() async throws {
        AppChatState.shared.set(appState)
        nseStateGroupDefault.set(nseState)
        seStateGroupDefault.set(seState)
        sendSimpleXCmdStr = sendCmd
        chatCloseStore = closeStore
    }

    @MainActor
    func testShareWhileSuspendingSuspendsAtOnce() throws {
        AppChatState.shared.set(.suspending)
        try withChatOk { try receive(.sendingMessage) }
        XCTAssertEqual(sent, ["suspending: /_app suspend 0"])
        XCTAssertEqual(AppChatState.shared.value, .suspended)
        XCTAssertEqual(closed, 1)
    }

    @MainActor
    func testShareWhileActiveIsIgnored() throws {
        AppChatState.shared.set(.active)
        try withChatOk { try receive(.sendingMessage) }
        XCTAssertEqual(sent, [])
        XCTAssertEqual(AppChatState.shared.value, .active)
        XCTAssertEqual(closed, 0)
    }

    @MainActor
    func testShareEndWhileSuspendingIsIgnored() throws {
        AppChatState.shared.set(.suspending)
        try withChatOk { try receive(.inactive) }
        XCTAssertEqual(sent, [])
        XCTAssertEqual(AppChatState.shared.value, .suspending)
        XCTAssertEqual(closed, 0)
    }

    @MainActor
    func testTerminateWhileSuspendingSuspendsCoreFirst() {
        AppChatState.shared.set(.suspending)
        withChatOk { terminateChat() }
        XCTAssertEqual(sent, ["suspending: /_app suspend 0"])
        XCTAssertEqual(AppChatState.shared.value, .suspended)
        XCTAssertEqual(closed, 1)
    }

    @MainActor
    func testShareSetupWaitsForSuspendingApp() async {
        AppChatState.shared.set(.suspending)
        nseStateGroupDefault.set(.suspended)
        try? FileManager.default.removeItem(at: seMessagesSharedFile)
        let delivered = Task { @MainActor in
            for _ in 0..<100 where !FileManager.default.fileExists(atPath: seMessagesSharedFile.path) {
                try await Task.sleep(nanoseconds: 10 * NSEC_PER_MSEC)
            }
            try deliver()
        }
        let start = Date.now
        await prepareToStartChat()
        let delivery = await delivered.result
        XCTAssertNoThrow(try delivery.get())
        XCTAssertLessThan(Date.now.timeIntervalSince(start), 1.5)
        XCTAssertEqual(seStateGroupDefault.get(), .sendingMessage)
        XCTAssertEqual(AppChatState.shared.value, .suspended)
    }

    @MainActor
    func testShareSetupDoesNotWaitForActiveApp() async {
        AppChatState.shared.set(.active)
        nseStateGroupDefault.set(.active)
        let start = Date.now
        await prepareToStartChat()
        XCTAssertLessThan(Date.now.timeIntervalSince(start), 1)
        XCTAssertEqual(seStateGroupDefault.get(), .sendingMessage)
    }

    @MainActor
    private func withChatOk(_ f: () throws -> Void) rethrows {
        let status = ChatModel.shared.chatDbStatus
        ChatModel.shared.chatDbStatus = .ok
        defer { ChatModel.shared.chatDbStatus = status }
        try f()
    }

    private func receive(_ state: SEState) throws {
        try jsonEncoder.encode(ProcessMessage(message: SEProcessMessage.state(state: state))).write(to: seMessagesSharedFile, options: .atomic)
        try deliver()
    }

    private func deliver() throws {
        let subscriber = try XCTUnwrap(NSFileCoordinator.filePresenters.first { $0.presentedItemURL == seMessagesSharedFile }, "share extension subscriber not created at launch")
        subscriber.presentedItemDidChange?()
    }
}
