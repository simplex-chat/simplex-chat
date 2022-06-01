//
//  CallTypes.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 05/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI

struct WebRTCCallOffer: Encodable {
    var callType: CallType
    var rtcSession: WebRTCSession
}

struct WebRTCSession: Codable {
    var rtcSession: String
    var rtcIceCandidates: String
}

struct WebRTCExtraInfo: Codable {
    var rtcIceCandidates: String
}

struct CallInvitation {
    var contact: Contact
    var callkitUUID: UUID?
    var peerMedia: CallMediaType
    var sharedKey: String?
    var callTs: Date
    var callTypeText: LocalizedStringKey {
        get {
            switch peerMedia {
            case .video: return sharedKey == nil ? "video call (not e2e encrypted)" : "**e2e encrypted** video call"
            case .audio: return sharedKey == nil ? "audio call (not e2e encrypted)" : "**e2e encrypted** audio call"
            }
        }
    }

    static let sampleData = CallInvitation(
        contact: Contact.sampleData,
        peerMedia: .audio,
        callTs: .now
    )
}

struct CallType: Codable {
    var media: CallMediaType
    var capabilities: CallCapabilities
}

enum CallMediaType: String, Codable, Equatable {
    case video = "video"
    case audio = "audio"
}

enum VideoCamera: String, Codable, Equatable {
    case user = "user"
    case environment = "environment"
}

struct CallCapabilities: Codable, Equatable {
    var encryption: Bool
}

enum WebRTCCallStatus: String, Encodable {
    case connected = "connected"
    case connecting = "connecting"
    case disconnected = "disconnected"
    case failed = "failed"
}
