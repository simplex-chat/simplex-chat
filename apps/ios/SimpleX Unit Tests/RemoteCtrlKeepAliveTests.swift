import XCTest
import UIKit
@testable import SimpleX
@testable import SimpleXChat

final class RemoteCtrlKeepAliveTests: XCTestCase {
    private let keepAlive = RemoteCtrlKeepAlive.shared
    private let systemServices = RemoteCtrlKeepAlive.shared.services
    private let sendCmd = sendSimpleXCmdStr
    private let notificationPreview = ChatModel.shared.notificationPreview
    private let appState = AppChatState.shared.value
    private let services = RecordingServices()
    private let lock = NSLock()
    private var sent: [String] = []
    private var idleTimerDisabled = false

    @MainActor
    override func setUp() async throws {
        idleTimerDisabled = UIApplication.shared.isIdleTimerDisabled
        keepAlive.services = services
        let commands = try XCTUnwrap((try fixtureJSON("remote/stop-response") as? [String: [String: Any]])?["commands"])
        sendSimpleXCmdStr = { [unowned self] cmd, _, _ in
            self.locked { self.sent.append(cmd) }
            return if let response = commands[cmd] { try? JSONSerialization.data(withJSONObject: singleFieldJSON(response)) } else { nil }
        }
    }

    @MainActor
    override func tearDown() async throws {
        ChatModel.shared.remoteCtrlSession = nil
        ChatModel.shared.activeCall = nil
        ChatModel.shared.notificationPreview = notificationPreview
        if AppChatState.shared.value != appState { AppChatState.shared.set(appState) }
        UIApplication.shared.isIdleTimerDisabled = idleTimerDisabled
        keepAlive.services = systemServices
        sendSimpleXCmdStr = sendCmd
    }

    @MainActor
    func testConnectedSessionStartsKeepAlive() async throws {
        try await connect()
        XCTAssertTrue(keepAlive.running)
        XCTAssertEqual(services.actions, [.startAudio, .showIndicator("My desktop")])
        XCTAssertFalse(UIApplication.shared.isIdleTimerDisabled)
    }

    @MainActor
    func testHiddenPreviewsHideDesktopName() async throws {
        ChatModel.shared.notificationPreview = .hidden
        try await connect()
        XCTAssertEqual(services.actions, [.startAudio, .showIndicator(nil)])
    }

    @MainActor
    func testActiveCallKeepsScreenOn() async throws {
        ChatModel.shared.activeCall = Call(direction: .incoming, contact: Contact.sampleData, callUUID: nil, callState: .waitCapabilities, initialCallType: .audio)
        try await connect()
        XCTAssertFalse(keepAlive.running)
        XCTAssertEqual(services.actions, [])
        XCTAssertTrue(UIApplication.shared.isIdleTimerDisabled)
    }

    @MainActor
    func testStoppedSessionStopsKeepAlive() async throws {
        let events = try recordedEvents("remote/session")
        try await connect()
        await processReceivedMsg(events[2])
        XCTAssertFalse(keepAlive.running)
        XCTAssertEqual(services.actions, [.startAudio, .showIndicator("My desktop"), .stopAudio, .endIndicator])
        try await waitFor { self.requests() == ["/users"] }
    }

    @MainActor
    func testSuspendedChatClearsSession() async throws {
        try await connect()
        AppChatState.shared.set(.active)
        let dbStatus = ChatModel.shared.chatDbStatus
        ChatModel.shared.chatDbStatus = .ok
        suspendChat()
        ChatModel.shared.chatDbStatus = dbStatus
        try await waitFor { ChatModel.shared.remoteCtrlSession == nil }
        XCTAssertFalse(keepAlive.running)
        XCTAssertEqual(services.actions, [.startAudio, .showIndicator("My desktop"), .stopAudio, .endIndicator])
        XCTAssertEqual(requests(), ["/_app suspend \(appSuspendTimeout * 1000000)", "/users"])
    }

    @MainActor
    func testAudioFailureStopsSession() async throws {
        try await connect()
        services.failAudio("restart failed")
        try await waitFor { ChatModel.shared.remoteCtrlSession == nil }
        XCTAssertFalse(keepAlive.running)
        XCTAssertEqual(services.actions, [.startAudio, .showIndicator("My desktop"), .stopAudio, .endIndicator])
        XCTAssertEqual(requests(), ["/stop remote ctrl", "/users"])
    }

    @MainActor
    func testLaunchEndsIndicator() async throws {
        keepAlive.observeSession()
        XCTAssertEqual(services.actions, [.endIndicator])
        XCTAssertFalse(keepAlive.running)
    }

    @MainActor
    private func connect() async throws {
        let events = try recordedEvents("remote/session")
        ChatModel.shared.remoteCtrlSession = RemoteCtrlSession(ctrlAppInfo: nil, appVersion: "", sessionState: .connecting(remoteCtrl_: nil))
        await processReceivedMsg(events[0])
        await processReceivedMsg(events[1])
        XCTAssertTrue(ChatModel.shared.activeRemoteCtrl)
    }

    private func requests() -> [String] {
        locked { sent }
    }

    private func locked<T>(_ f: () -> T) -> T {
        lock.lock()
        defer { lock.unlock() }
        return f()
    }
}

private final class RecordingServices: RemoteCtrlKeepAliveServices {
    enum Action: Equatable {
        case startAudio
        case stopAudio
        case showIndicator(String?)
        case endIndicator
    }

    private(set) var actions: [Action] = []
    private var audioFailed: ((String) -> Void)?

    func startAudio(onFailure: @escaping (String) -> Void) -> Bool {
        actions.append(.startAudio)
        audioFailed = onFailure
        return true
    }

    func stopAudio() {
        actions.append(.stopAudio)
        audioFailed = nil
    }

    func showIndicator(_ desktopName: String?) {
        actions.append(.showIndicator(desktopName))
    }

    func endIndicator() {
        actions.append(.endIndicator)
    }

    func failAudio(_ reason: String) {
        audioFailed?(reason)
    }
}
