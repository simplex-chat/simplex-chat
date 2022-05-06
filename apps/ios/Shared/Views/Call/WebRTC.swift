//
//  WebRTC.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 03/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI

struct Call: Equatable {
    static func == (lhs: Call, rhs: Call) -> Bool {
        lhs.contact.apiId == rhs.contact.apiId
    }

    var contact: Contact
    var callState: CallState
    var localMedia: CallMediaType
    var localCapabilities: CallCapabilities?
    var peerMedia: CallMediaType?
    var sharedKey: String?

    func copy(
        contact: Contact? = nil,
        callState: CallState? = nil,
        localMedia: CallMediaType? = nil,
        localCapabilities: CallCapabilities? = nil,
        peerMedia: CallMediaType? = nil,
        sharedKey: String? = nil
    ) -> Call {
        Call (
            contact: contact ?? self.contact,
            callState: callState ?? self.callState,
            localMedia: localMedia ?? self.localMedia,
            localCapabilities: localCapabilities ?? self.localCapabilities,
            peerMedia: peerMedia ?? self.peerMedia,
            sharedKey: sharedKey ?? self.sharedKey
        )
    }

    var encrypted: Bool {
        (localCapabilities?.encryption ?? false) && sharedKey != nil
    }
}

enum CallState {
    case waitCapabilities
    case invitationSent
    case invitationReceived
    case offerSent
    case offerReceived
    case negotiated
    case connected

    var text: LocalizedStringKey {
        switch self {
        case .waitCapabilities: return "starting..."
        case .invitationSent: return "waiting for answer..."
        case .invitationReceived: return "starting..."
        case .offerSent: return "waiting for confirmation..."
        case .offerReceived: return "received answer..."
        case .negotiated: return "connecting..."
        case .connected: return "connected"
        }
    }
}

struct WVAPICall: Encodable {
    var corrId: Int? = nil
    var command: WCallCommand
}

struct WVAPIMessage: Equatable, Decodable {
    var corrId: Int?
    var resp: WCallResponse
    var command: WCallCommand?
}

enum WCallCommand: Equatable, Encodable, Decodable {
    case capabilities
    case start(media: CallMediaType, aesKey: String? = nil)
    case accept(offer: String, iceCandidates: [String], media: CallMediaType, aesKey: String? = nil)
    case answer(answer: String, iceCandidates: [String])
    case ice(iceCandidates: [String])
    case end

    enum CodingKeys: String, CodingKey {
        case type
        case media
        case aesKey
        case offer
        case answer
        case iceCandidates
    }

    var cmdType: String {
        get {
            switch self {
            case .capabilities: return("capabilities")
            case .start: return("start")
            case .accept: return("accept")
            case .answer: return("answer")
            case .ice: return("ice")
            case .end: return("end")
            }
        }
    }

    func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        switch self {
        case .capabilities:
            try container.encode("capabilities", forKey: .type)
        case let .start(media, aesKey):
            try container.encode("start", forKey: .type)
            try container.encode(media, forKey: .media)
            try container.encode(aesKey, forKey: .aesKey)
        case let .accept(offer, iceCandidates, media, aesKey):
            try container.encode("accept", forKey: .type)
            try container.encode(offer, forKey: .offer)
            try container.encode(iceCandidates, forKey: .iceCandidates)
            try container.encode(media, forKey: .media)
            try container.encode(aesKey, forKey: .aesKey)
        case let .answer(answer, iceCandidates):
            try container.encode("answer", forKey: .type)
            try container.encode(answer, forKey: .answer)
            try container.encode(iceCandidates, forKey: .iceCandidates)
        case let .ice(iceCandidates):
            try container.encode("ice", forKey: .type)
            try container.encode(iceCandidates, forKey: .iceCandidates)
        case .end:
            try container.encode("end", forKey: .type)
        }
    }

    init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        let type = try container.decode(String.self, forKey: CodingKeys.type)
        switch type {
        case "capabilities":
            self = .capabilities
        case "start":
            let media = try container.decode(CallMediaType.self, forKey: CodingKeys.media)
            let aesKey = try? container.decode(String.self, forKey: CodingKeys.aesKey)
            self = .start(media: media, aesKey: aesKey)
        case "accept":
            let offer = try container.decode(String.self, forKey: CodingKeys.offer)
            let iceCandidates = try container.decode([String].self, forKey: CodingKeys.iceCandidates)
            let media = try container.decode(CallMediaType.self, forKey: CodingKeys.media)
            let aesKey = try? container.decode(String.self, forKey: CodingKeys.aesKey)
            self = .accept(offer: offer, iceCandidates: iceCandidates, media: media, aesKey: aesKey)
        case "answer":
            let answer = try container.decode(String.self, forKey: CodingKeys.answer)
            let iceCandidates = try container.decode([String].self, forKey: CodingKeys.iceCandidates)
            self = .answer(answer: answer, iceCandidates: iceCandidates)
        case "ice":
            let iceCandidates = try container.decode([String].self, forKey: CodingKeys.iceCandidates)
            self = .ice(iceCandidates: iceCandidates)
        case "end":
            self = .end
        default:
            throw DecodingError.typeMismatch(WCallCommand.self, DecodingError.Context(codingPath: [CodingKeys.type], debugDescription: "cannot decode WCallCommand, unknown type \(type)"))
        }
    }
}


enum WCallResponse: Equatable, Decodable {
    case capabilities(capabilities: CallCapabilities)
    case offer(offer: String, iceCandidates: [String], capabilities: CallCapabilities)
    // TODO remove accept, it is needed for debugging
//    case accept(offer: String, iceCandidates: [String], media: CallMediaType, aesKey: String? = nil)
    case answer(answer: String, iceCandidates: [String])
    case ice(iceCandidates: [String])
    case connection(state: ConnectionState)
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
        case message
        // TODO remove media, aesKey
        case media
        case aesKey
    }

    var respType: String {
        get {
            switch self {
            case .capabilities: return("capabilities")
            case .offer: return("offer")
//            case .accept: return("accept")
            case .answer: return("answer (TODO remove)")
            case .ice: return("ice")
            case .connection: return("connection")
            case .ended: return("ended")
            case .ok: return("ok")
            case .error: return("error")
            case .invalid: return("invalid")
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
                let iceCandidates = try container.decode([String].self, forKey: CodingKeys.iceCandidates)
                let capabilities = try container.decode(CallCapabilities.self, forKey: CodingKeys.capabilities)
                self = .offer(offer: offer, iceCandidates: iceCandidates, capabilities: capabilities)
            case "answer":
                let answer = try container.decode(String.self, forKey: CodingKeys.answer)
                let iceCandidates = try container.decode([String].self, forKey: CodingKeys.iceCandidates)
                self = .answer(answer: answer, iceCandidates: iceCandidates)
            case "ice":
                let iceCandidates = try container.decode([String].self, forKey: CodingKeys.iceCandidates)
                self = .ice(iceCandidates: iceCandidates)
            case "connection":
                let state = try container.decode(ConnectionState.self, forKey: CodingKeys.state)
                self = .connection(state: state)
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
//extension WCallResponse: Encodable {
//    func encode(to encoder: Encoder) throws {
//        var container = encoder.container(keyedBy: CodingKeys.self)
//        switch self {
//        case .capabilities:
//            try container.encode("capabilities", forKey: .type)
//        case let .offer(offer, iceCandidates, capabilities):
//            try container.encode("offer", forKey: .type)
//            try container.encode(offer, forKey: .offer)
//            try container.encode(iceCandidates, forKey: .iceCandidates)
//            try container.encode(capabilities, forKey: .capabilities)
//        case let .answer(answer, iceCandidates):
//            try container.encode("answer", forKey: .type)
//            try container.encode(answer, forKey: .answer)
//            try container.encode(iceCandidates, forKey: .iceCandidates)
//        case let .ice(iceCandidates):
//            try container.encode("ice", forKey: .type)
//            try container.encode(iceCandidates, forKey: .iceCandidates)
//        case let .connection(state):
//            try container.encode("connection", forKey: .type)
//            try container.encode(state, forKey: .state)
//        case .ended:
//            try container.encode("ended", forKey: .type)
//        case .ok:
//            try container.encode("ok", forKey: .type)
//        case let .error(message):
//            try container.encode("error", forKey: .type)
//            try container.encode(message, forKey: .message)
//        case let .invalid(type):
//            try container.encode(type, forKey: .type)
//        }
//    }
//}

struct ConnectionState: Codable, Equatable {
    var connectionState: String
    var iceConnectionState: String
    var iceGatheringState: String
    var signalingState: String
}
