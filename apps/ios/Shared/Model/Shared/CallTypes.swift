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
    var peerMedia: CallMediaType
    var sharedKey: String?
    var callTypeText: LocalizedStringKey {
        get {
            switch peerMedia {
            case .video: return sharedKey == nil ? "video call (not e2e encrypted)." : "**e2e encrypted** video call."
            case .audio: return sharedKey == nil ? "audio call (not e2e encrypted)." : "**e2e encrypted** audio call."
            }
        }
    }
    var callTitle: LocalizedStringKey {
        get {
            switch peerMedia {
            case .video: return "Incoming video call"
            case .audio: return "Incoming audio call"
            }
        }
    }
    var encryptionText: LocalizedStringKey { get { sharedKey == nil ? "no e2e encryption" : "with e2e encryption" } }
}

struct CallType: Codable {
    var media: CallMediaType
    var capabilities: CallCapabilities
}

enum CallMediaType: String, Codable, Equatable {
    case video = "video"
    case audio = "audio"
}

struct CallCapabilities: Codable, Equatable {
    var encryption: Bool
}

enum WebRTCCallStatus: String, Encodable {
    case connected = "connected"
    case disconnected = "disconnected"
    case failed = "failed"
}
