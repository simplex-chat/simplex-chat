//
//  WebRTC.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 03/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI
import SimpleXChat
import WebRTC

class Call: ObservableObject, Equatable {
    static func == (lhs: Call, rhs: Call) -> Bool {
        lhs.contact.apiId == rhs.contact.apiId
    }

    var direction: CallDirection
    var contact: Contact
    var callkitUUID: UUID?
    var localMedia: CallMediaType
    @Published var callState: CallState
    @Published var localCapabilities: CallCapabilities?
    @Published var peerMedia: CallMediaType?
    @Published var sharedKey: String?
    @Published var audioEnabled = true
    @Published var speakerEnabled = false
    @Published var videoEnabled: Bool
    @Published var connectionInfo: ConnectionInfo?
    @Published var connectedAt: Date? = nil

    init(
        direction: CallDirection,
        contact: Contact,
        callkitUUID: UUID?,
        callState: CallState,
        localMedia: CallMediaType,
        sharedKey: String? = nil
    ) {
        self.direction = direction
        self.contact = contact
        self.callkitUUID = callkitUUID
        self.callState = callState
        self.localMedia = localMedia
        self.sharedKey = sharedKey
        self.videoEnabled = localMedia == .video
    }

    var encrypted: Bool { get { localEncrypted && sharedKey != nil } }
    var localEncrypted: Bool { get { localCapabilities?.encryption ?? false } }
    var encryptionStatus: LocalizedStringKey {
        get {
            switch callState {
            case .waitCapabilities: return ""
            case .invitationSent: return localEncrypted ? "e2e encrypted" : "no e2e encryption"
            case .invitationAccepted: return sharedKey == nil ? "contact has no e2e encryption" : "contact has e2e encryption"
            default: return !localEncrypted ? "no e2e encryption" : sharedKey == nil ? "contact has no e2e encryption" : "e2e encrypted" 
            }
        }
    }
    var hasMedia: Bool { get { callState == .offerSent || callState == .negotiated || callState == .connected } }
    var supportsVideo: Bool { get { peerMedia == .video || localMedia == .video } }
}

enum CallDirection {
    case incoming
    case outgoing
}

enum CallState {
    case waitCapabilities   // outgoing call started
    case invitationSent     // outgoing call - sent invitation
    case invitationAccepted // incoming call started
    case offerSent          // incoming - webrtc started and offer sent
    case offerReceived      // outgoing - webrtc offer received via API
    case answerReceived     // incoming - webrtc answer received via API
    case negotiated         // outgoing - webrtc offer processed and answer sent, incoming - webrtc answer processed
    case connected
    case ended

    var text: LocalizedStringKey {
        switch self {
        case .waitCapabilities: return "starting…"
        case .invitationSent: return "waiting for answer…"
        case .invitationAccepted: return "starting…"
        case .offerSent: return "waiting for confirmation…"
        case .offerReceived: return "received answer…"
        case .answerReceived: return "received confirmation…"
        case .negotiated: return "connecting…"
        case .connected: return "connected"
        case .ended: return "ended"
        }
    }
}

struct WVAPICall: Encodable {
    var corrId: Int? = nil
    var command: WCallCommand
}

struct WVAPIMessage: Equatable, Decodable, Encodable {
    var corrId: Int?
    var resp: WCallResponse
    var command: WCallCommand?
}

enum WCallCommand: Equatable, Encodable, Decodable {
    case capabilities(media: CallMediaType)
    case start(media: CallMediaType, aesKey: String? = nil, iceServers: [RTCIceServer]? = nil, relay: Bool? = nil)
    case offer(offer: String, iceCandidates: String, media: CallMediaType, aesKey: String? = nil, iceServers: [RTCIceServer]? = nil, relay: Bool? = nil)
    case answer(answer: String, iceCandidates: String)
    case ice(iceCandidates: String)
    case media(media: CallMediaType, enable: Bool)
    case end

    enum CodingKeys: String, CodingKey {
        case type
        case media
        case aesKey
        case offer
        case answer
        case iceCandidates
        case enable
        case iceServers
        case relay
    }

    var cmdType: String {
        get {
            switch self {
            case .capabilities: return "capabilities"
            case .start: return "start"
            case .offer: return "offer"
            case .answer: return "answer"
            case .ice: return "ice"
            case .media: return "media"
            case .end: return "end"
            }
        }
    }

    func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        switch self {
        case let .capabilities(media):
            try container.encode("capabilities", forKey: .type)
            try container.encode(media, forKey: .media)
        case let .start(media, aesKey, iceServers, relay):
            try container.encode("start", forKey: .type)
            try container.encode(media, forKey: .media)
            try container.encode(aesKey, forKey: .aesKey)
            try container.encode(iceServers, forKey: .iceServers)
            try container.encode(relay, forKey: .relay)
        case let .offer(offer, iceCandidates, media, aesKey, iceServers, relay):
            try container.encode("offer", forKey: .type)
            try container.encode(offer, forKey: .offer)
            try container.encode(iceCandidates, forKey: .iceCandidates)
            try container.encode(media, forKey: .media)
            try container.encode(aesKey, forKey: .aesKey)
            try container.encode(iceServers, forKey: .iceServers)
            try container.encode(relay, forKey: .relay)
        case let .answer(answer, iceCandidates):
            try container.encode("answer", forKey: .type)
            try container.encode(answer, forKey: .answer)
            try container.encode(iceCandidates, forKey: .iceCandidates)
        case let .ice(iceCandidates):
            try container.encode("ice", forKey: .type)
            try container.encode(iceCandidates, forKey: .iceCandidates)
        case let .media(media, enable):
            try container.encode("media", forKey: .type)
            try container.encode(media, forKey: .media)
            try container.encode(enable, forKey: .enable)
        case .end:
            try container.encode("end", forKey: .type)
        }
    }

    init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        let type = try container.decode(String.self, forKey: CodingKeys.type)
        switch type {
        case "capabilities":
            let media = try container.decode(CallMediaType.self, forKey: CodingKeys.media)
            self = .capabilities(media: media)
        case "start":
            let media = try container.decode(CallMediaType.self, forKey: CodingKeys.media)
            let aesKey = try? container.decode(String.self, forKey: CodingKeys.aesKey)
            let iceServers = try container.decode(([RTCIceServer]?).self, forKey: .iceServers)
            let relay = try container.decode((Bool?).self, forKey: .relay)
            self = .start(media: media, aesKey: aesKey, iceServers: iceServers, relay: relay)
        case "offer":
            let offer = try container.decode(String.self, forKey: CodingKeys.offer)
            let iceCandidates = try container.decode(String.self, forKey: CodingKeys.iceCandidates)
            let media = try container.decode(CallMediaType.self, forKey: CodingKeys.media)
            let aesKey = try? container.decode(String.self, forKey: CodingKeys.aesKey)
            let iceServers = try container.decode(([RTCIceServer]?).self, forKey: .iceServers)
            let relay = try container.decode((Bool?).self, forKey: .relay)
            self = .offer(offer: offer, iceCandidates: iceCandidates, media: media, aesKey: aesKey, iceServers: iceServers, relay: relay)
        case "answer":
            let answer = try container.decode(String.self, forKey: CodingKeys.answer)
            let iceCandidates = try container.decode(String.self, forKey: CodingKeys.iceCandidates)
            self = .answer(answer: answer, iceCandidates: iceCandidates)
        case "ice":
            let iceCandidates = try container.decode(String.self, forKey: CodingKeys.iceCandidates)
            self = .ice(iceCandidates: iceCandidates)
        case "media":
            let media = try container.decode(CallMediaType.self, forKey: CodingKeys.media)
            let enable = try container.decode(Bool.self, forKey: CodingKeys.enable)
            self = .media(media: media, enable: enable)
        case "end":
            self = .end
        default:
            throw DecodingError.typeMismatch(WCallCommand.self, DecodingError.Context(codingPath: [CodingKeys.type], debugDescription: "cannot decode WCallCommand, unknown type \(type)"))
        }
    }
}


enum WCallResponse: Equatable, Decodable {
    case capabilities(capabilities: CallCapabilities)
    case offer(offer: String, iceCandidates: String, capabilities: CallCapabilities)
    case answer(answer: String, iceCandidates: String)
    case ice(iceCandidates: String)
    case connection(state: ConnectionState)
    case connected(connectionInfo: ConnectionInfo)
    case ended
    case ok
    case error(message: String)
    case invalid(type: String)

    enum CodingKeys: String, CodingKey {
        case type
        case capabilities
        case offer
        case answer
        case iceCandidates
        case state
        case connectionInfo
        case message
    }

    var respType: String {
        get {
            switch self {
            case .capabilities: return "capabilities"
            case .offer: return "offer"
            case .answer: return "answer"
            case .ice: return "ice"
            case .connection: return "connection"
            case .connected: return "connected"
            case .ended: return "ended"
            case .ok: return "ok"
            case .error: return "error"
            case .invalid: return "invalid"
            }
        }
    }

    init(from decoder: Decoder) throws {
        do {
            let container = try decoder.container(keyedBy: CodingKeys.self)
            let type = try container.decode(String.self, forKey: CodingKeys.type)
            switch type {
            case "capabilities":
                let capabilities = try container.decode(CallCapabilities.self, forKey: CodingKeys.capabilities)
                self = .capabilities(capabilities: capabilities)
            case "offer":
                let offer = try container.decode(String.self, forKey: CodingKeys.offer)
                let iceCandidates = try container.decode(String.self, forKey: CodingKeys.iceCandidates)
                let capabilities = try container.decode(CallCapabilities.self, forKey: CodingKeys.capabilities)
                self = .offer(offer: offer, iceCandidates: iceCandidates, capabilities: capabilities)
            case "answer":
                let answer = try container.decode(String.self, forKey: CodingKeys.answer)
                let iceCandidates = try container.decode(String.self, forKey: CodingKeys.iceCandidates)
                self = .answer(answer: answer, iceCandidates: iceCandidates)
            case "ice":
                let iceCandidates = try container.decode(String.self, forKey: CodingKeys.iceCandidates)
                self = .ice(iceCandidates: iceCandidates)
            case "connection":
                let state = try container.decode(ConnectionState.self, forKey: CodingKeys.state)
                self = .connection(state: state)
            case "connected":
                let connectionInfo = try container.decode(ConnectionInfo.self, forKey: CodingKeys.connectionInfo)
                self = .connected(connectionInfo: connectionInfo)
            case "ended":
                self = .ended
            case "ok":
                self = .ok
            case "error":
                let message = try container.decode(String.self, forKey: CodingKeys.message)
                self = .error(message: message)
            default:
                self = .invalid(type: type)
            }
        } catch {
            self = .invalid(type: "unknown")
        }
    }
}

// This protocol is for debugging
extension WCallResponse: Encodable {
    func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        switch self {
        case .capabilities:
            try container.encode("capabilities", forKey: .type)
        case let .offer(offer, iceCandidates, capabilities):
            try container.encode("offer", forKey: .type)
            try container.encode(offer, forKey: .offer)
            try container.encode(iceCandidates, forKey: .iceCandidates)
            try container.encode(capabilities, forKey: .capabilities)
        case let .answer(answer, iceCandidates):
            try container.encode("answer", forKey: .type)
            try container.encode(answer, forKey: .answer)
            try container.encode(iceCandidates, forKey: .iceCandidates)
        case let .ice(iceCandidates):
            try container.encode("ice", forKey: .type)
            try container.encode(iceCandidates, forKey: .iceCandidates)
        case let .connection(state):
            try container.encode("connection", forKey: .type)
            try container.encode(state, forKey: .state)
        case let .connected(connectionInfo):
            try container.encode("connected", forKey: .type)
            try container.encode(connectionInfo, forKey: .connectionInfo)
        case .ended:
            try container.encode("ended", forKey: .type)
        case .ok:
            try container.encode("ok", forKey: .type)
        case let .error(message):
            try container.encode("error", forKey: .type)
            try container.encode(message, forKey: .message)
        case let .invalid(type):
            try container.encode(type, forKey: .type)
        }
    }
}

actor WebRTCCommandProcessor {
    private var client: WebRTCClient? = nil
    private var commands: [WCallCommand] = []
    private var running: Bool = false

    func setClient(_ client: WebRTCClient?) async {
        logger.debug("WebRTC: setClient, commands count \(self.commands.count)")
        self.client = client
        if client != nil {
            await processAllCommands()
        } else {
            commands.removeAll()
        }
    }

    func processCommand(_ c: WCallCommand) async {
//        logger.debug("WebRTC: process command \(c.cmdType)")
        commands.append(c)
        if !running && client != nil {
            await processAllCommands()
        }
    }

    func processAllCommands() async {
        logger.debug("WebRTC: process all commands, commands count \(self.commands.count), client == nil \(self.client == nil)")
        if let client = client {
            running = true
            while let c = commands.first, shouldRunCommand(client, c) {
                commands.remove(at: 0)
                await client.sendCallCommand(command: c)
                logger.debug("WebRTC: processed cmd \(c.cmdType)")
            }
            running = false
        }
    }

    func shouldRunCommand(_ client: WebRTCClient, _ c: WCallCommand) -> Bool {
        switch c {
        case .capabilities, .start, .offer, .end: true
        default: client.activeCall.wrappedValue != nil
        }
    }
}

struct ConnectionState: Codable, Equatable {
    var connectionState: String
    var iceConnectionState: String
    var iceGatheringState: String
    var signalingState: String
}

struct ConnectionInfo: Codable, Equatable {
    var localCandidate: RTCIceCandidate?
    var remoteCandidate: RTCIceCandidate?

    var text: LocalizedStringKey {
        let local = localCandidate?.candidateType
        let remote = remoteCandidate?.candidateType
        if local == .host && remote == .host {
            return "peer-to-peer"
        } else if local == .relay && remote == .relay {
            return "via relay"
        } else {
            let unknown = NSLocalizedString("unknown", comment: "connection info")
            return "\(local?.rawValue ?? unknown) / \(remote?.rawValue ?? unknown)"
        }
    }
}

// https://developer.mozilla.org/en-US/docs/Web/API/RTCIceCandidate
struct RTCIceCandidate: Codable, Equatable {
    var candidateType: RTCIceCandidateType?
    var `protocol`: String?
    var sdpMid: String?
    var sdpMLineIndex: Int?
    var candidate: String
}

// https://developer.mozilla.org/en-US/docs/Web/API/RTCIceCandidate/type
enum RTCIceCandidateType: String, Codable {
    case host = "host"
    case serverReflexive = "srflx"
    case peerReflexive = "prflx"
    case relay = "relay"
}

// https://developer.mozilla.org/en-US/docs/Web/API/RTCIceServer
struct RTCIceServer: Codable, Equatable {
    var urls: [String]
    var username: String? = nil
    var credential: String? = nil
}

// the servers are expected in this format:
// stun:stun.simplex.im:443?transport=tcp
// turn:private:yleob6AVkiNI87hpR94Z@turn.simplex.im:443?transport=tcp
func parseRTCIceServer(_ str: String) -> RTCIceServer? {
    var s = replaceScheme(str, "stun:")
    s = replaceScheme(s, "turn:")
    s = replaceScheme(s, "turns:")
    if let u: URL = URL(string: s),
       let scheme = u.scheme,
       let host = u.host,
       let port = u.port,
       u.path == "" && (scheme == "stun" || scheme == "turn" || scheme == "turns")  {
        let query = u.query == nil || u.query == "" ? "" : "?" + (u.query ?? "")
        return RTCIceServer(
            urls: ["\(scheme):\(host):\(port)\(query)"],
            username: u.user,
            credential: u.password
        )
    }
    return nil
}

private func replaceScheme(_ s: String, _ scheme: String) -> String {
    s.starts(with: scheme)
    ? s.replacingOccurrences(of: scheme, with: scheme + "//", options: .anchored, range: nil)
    : s
}

func parseRTCIceServers(_ servers: [String]) -> [RTCIceServer]? {
    var iceServers: [RTCIceServer] = []
    for s in servers {
        if let server = parseRTCIceServer(s) {
            iceServers.append(server)
        } else {
            return nil
        }
    }
    return iceServers.isEmpty ? nil : iceServers
}

func getIceServers() -> [RTCIceServer]? {
    if let servers = UserDefaults.standard.stringArray(forKey: DEFAULT_WEBRTC_ICE_SERVERS) {
        return parseRTCIceServers(servers)
    }
    return nil
}

func getWebRTCIceServers() -> [WebRTC.RTCIceServer]? {
    if let servers = UserDefaults.standard.stringArray(forKey: DEFAULT_WEBRTC_ICE_SERVERS) {
        return parseRTCIceServers(servers)?.toWebRTCIceServers()
    }
    return nil
}
