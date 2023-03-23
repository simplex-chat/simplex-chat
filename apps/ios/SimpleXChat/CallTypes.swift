//
//  CallTypes.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 05/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI

public struct WebRTCCallOffer: Encodable {
    public init(callType: CallType, rtcSession: WebRTCSession) {
        self.callType = callType
        self.rtcSession = rtcSession
    }

    public var callType: CallType
    public var rtcSession: WebRTCSession
}

public struct WebRTCSession: Codable {
    public init(rtcSession: String, rtcIceCandidates: String) {
        self.rtcSession = rtcSession
        self.rtcIceCandidates = rtcIceCandidates
    }

    public var rtcSession: String
    public var rtcIceCandidates: String
}

public struct WebRTCExtraInfo: Codable {
    public init(rtcIceCandidates: String) {
        self.rtcIceCandidates = rtcIceCandidates
    }

    public var rtcIceCandidates: String
}

public struct RcvCallInvitation: Decodable {
    public var user: User
    public var contact: Contact
    public var callType: CallType
    public var sharedKey: String?
    public var callTs: Date
    public var callTypeText: LocalizedStringKey {
        get {
            switch callType.media {
            case .video: return sharedKey == nil ? "video call (not e2e encrypted)" : "**e2e encrypted** video call"
            case .audio: return sharedKey == nil ? "audio call (not e2e encrypted)" : "**e2e encrypted** audio call"
            }
        }
    }

    public var callkitUUID: UUID? = UUID()

    private enum CodingKeys: String, CodingKey {
        case user, contact, callType, sharedKey, callTs
    }

    public static let sampleData = RcvCallInvitation(
        user: User.sampleData,
        contact: Contact.sampleData,
        callType: CallType(media: .audio, capabilities: CallCapabilities(encryption: false)),
        callTs: .now
    )
}

public struct CallType: Codable {
    public init(media: CallMediaType, capabilities: CallCapabilities) {
        self.media = media
        self.capabilities = capabilities
    }

    public var media: CallMediaType
    public var capabilities: CallCapabilities
}

public enum CallMediaType: String, Codable, Equatable {
    case video = "video"
    case audio = "audio"
}

public enum VideoCamera: String, Codable, Equatable {
    case user = "user"
    case environment = "environment"
}

public struct CallCapabilities: Codable, Equatable {
    public var encryption: Bool

    public init(encryption: Bool) {
        self.encryption = encryption
    }
}

public enum WebRTCCallStatus: String, Encodable {
    case connected = "connected"
    case connecting = "connecting"
    case disconnected = "disconnected"
    case failed = "failed"
}
