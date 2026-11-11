import XCTest
import SwiftUI
@testable import SimpleX
@testable import SimpleXChat

// Calls between two WebRTCClients in the app's process: the test passes each call message to the other client, as
// the chat does between two apps. Without STUN or TURN servers they connect over the host's own addresses.
final class CallTests: XCTestCase {
    private static let mediaTimeout: Double = 180
    private static let videoFrames: Int64 = 30

    override func setUp() async throws {
        // With CallKit, the app's default, WebRTC starts no audio until CallKit activates it, which it doesn't here: two
        // clients starting audio in one simulator deadlocked CoreAudio (run 41291626987). The UI call test checks audio.
        callKitEnabledGroupDefault.set(true)
        try await waitUntil("the app starts its chat core, which encrypts the frames", seconds: 60) { hasChatCtrl() }
        guard Bundle.main.path(forResource: "sounds/video", ofType: "mp4") != nil else {
            XCTFail("the simulator's camera plays sounds/video.mp4: make it with scripts/ci/ios-sim/test-video.swift")
            throw CallTestError.noTestVideo
        }
    }

    func testEncryptedVideoCall() async throws {
        let key = Self.mediaKey()
        let caller = CallPeer("caller")
        let callee = CallPeer("callee")

        // the caller's invitation carries its capabilities; the callee starts WebRTC and makes the offer
        await caller.client.sendCallCommand(command: .capabilities(media: .video))
        let capabilities: CallCapabilities = try await caller.response("capabilities") {
            if case let .capabilities(capabilities) = $0 { return capabilities }
            return nil
        }
        XCTAssertTrue(capabilities.encryption, "the caller does not encrypt calls")

        await callee.client.sendCallCommand(command: .start(media: .video, aesKey: key, iceServers: [], relay: false))
        let (offer, offerIce): (String, String) = try await callee.response("offer") {
            if case let .offer(offer, iceCandidates, _) = $0 { return (offer, iceCandidates) }
            return nil
        }

        await caller.client.sendCallCommand(command: .offer(offer: offer, iceCandidates: offerIce, media: .video, aesKey: key, iceServers: [], relay: false))
        await callee.responses.relayIce(to: caller.client)
        let (answer, answerIce): (String, String) = try await caller.response("answer") {
            if case let .answer(answer, iceCandidates) = $0 { return (answer, iceCandidates) }
            return nil
        }

        await callee.client.sendCallCommand(command: .answer(answer: answer, iceCandidates: answerIce))
        await caller.responses.relayIce(to: callee.client)
        for peer in [caller, callee] {
            let _: Bool = try await peer.response("connected") {
                if case .connected = $0 { return true }
                return nil
            }
        }

        for peer in [caller, callee] {
            try await waitUntil(
                "\(peer.name) decodes \(Self.videoFrames) video frames",
                seconds: Self.mediaTimeout,
                state: {
                    let stats = await peer.client.mediaStats()
                    return "\(String(describing: stats)), \(peer.client.frameCounts)"
                }
            ) {
                guard let stats = await peer.client.mediaStats() else { return false }
                return stats.videoFramesDecoded >= Self.videoFrames
            }
            let currentStats = await peer.client.mediaStats()
            let stats = try XCTUnwrap(currentStats, "\(peer.name) has no call")
            let frames = peer.client.frameCounts
            print("\(peer.name): \(stats), \(frames)")
            // the frame encryption leaves VP8's header in the clear for the packetizer, so another codec would not arrive
            XCTAssertEqual(stats.codecs.filter { $0.hasPrefix("video/") }, ["video/VP8"], "\(peer.name)'s video codecs")
            XCTAssertGreaterThan(frames.encrypted, 0, "\(peer.name) encrypted no frames")
            XCTAssertGreaterThan(frames.decrypted, 0, "\(peer.name) decrypted no frames")
            XCTAssertEqual(frames.errors, 0, "\(peer.name)'s frame encryption errors")
        }
        for peer in [caller, callee] {
            try await endCall(peer)
        }
    }

    // The app ends a call twice at once: the end command, and the call view on disappearing on the main thread. Both
    // closed the connection, which froze the app (CI run 41311546712). A hang aborts the app, whose crash report has
    // the stacks of all threads.
    private func endCall(_ peer: CallPeer) async throws {
        let ended = Flag()
        Task.detached {
            async let command: Void = peer.client.sendCallCommand(command: .end)
            async let view: Void = MainActor.run { peer.client.endCall() }
            _ = await (command, view)
            await ended.set()
        }
        do {
            try await waitUntil("\(peer.name) ends its call twice at once", seconds: 30) { await ended.isSet }
        } catch CallTestError.timeout {
            abort()
        }
        XCTAssertNil(peer.client.activeCall, "\(peer.name)'s call did not end")
    }

    // 32 random bytes in base64url, the media key the chat core makes for a call
    private static func mediaKey() -> String {
        Data((0..<32).map { _ in UInt8.random(in: 0...255) }).base64EncodedString()
            .replacingOccurrences(of: "+", with: "-")
            .replacingOccurrences(of: "/", with: "_")
    }
}

private enum CallTestError: Error {
    case noTestVideo
    case timeout(String)
}

private func waitUntil(
    _ what: String,
    seconds: Double,
    state: () async -> String = { "" },
    _ condition: () async -> Bool
) async throws {
    let deadline = Date().addingTimeInterval(seconds)
    while !(await condition()) {
        guard Date() < deadline else {
            let current = await state()
            XCTFail("waited \(Int(seconds)) s until \(what) \(current)")
            throw CallTestError.timeout(what)
        }
        try await Task.sleep(nanoseconds: 250_000_000)
    }
}

private final class CallPeer {
    let name: String
    let responses = Responses()
    private(set) var client: WebRTCClient!

    init(_ name: String) {
        self.name = name
        let responses = responses
        client = WebRTCClient({ message in await responses.add(message.resp) }, .constant(nil))
    }

    func response<T>(_ type: String, _ match: (WCallResponse) -> T?) async throws -> T {
        var result: T?
        try await waitUntil("\(name) responds with \(type)", seconds: 30) {
            let error: String? = await responses.first {
                if case let .error(message) = $0 { return message }
                return nil
            }
            if let error {
                XCTFail("\(name) responded with an error: \(error)")
                return true
            }
            result = await responses.first(match)
            return result != nil
        }
        return try XCTUnwrap(result, "\(name) did not respond with \(type)")
    }
}

private actor Flag {
    private(set) var isSet = false

    func set() {
        isSet = true
    }
}

private actor Responses {
    private var all: [WCallResponse] = []
    private var relay: ((String) async -> Void)?

    // The ICE candidates gathered after the offer or the answer go to the other client once it has the call, as the chat
    // delivers them after the offer and the answer: a client without the call responds with an error.
    func relayIce(to peer: WebRTCClient) async {
        relay = { candidates in await peer.sendCallCommand(command: .ice(iceCandidates: candidates)) }
        for case let .ice(candidates, nil) in all {
            await relay?(candidates)
        }
    }

    func add(_ response: WCallResponse) async {
        all.append(response)
        if case let .ice(candidates, nil) = response {
            await relay?(candidates)
        }
    }

    func first<T>(_ match: (WCallResponse) -> T?) -> T? {
        all.lazy.compactMap(match).first
    }
}
