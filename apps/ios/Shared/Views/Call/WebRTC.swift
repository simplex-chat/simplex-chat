//
//  WebRTC.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 03/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation

struct WebViewAPICall: Codable {
    var corrId: Int
    var command: WCallCommand
}

struct WebViewMessage: Codable {
    var corrId: Int?
    var resp: WError
}

struct WError: Codable {
    var type: String
    var message: String
}

struct CallCapabilities: Codable {
    var encryption: Bool
}

struct RTCSessionDescriptionInit: Codable {
    var sdp: String?
    var type: RTCSdpType
}

struct RTCIceCandidateInit: Codable {
    var candidate: String?
//    var sdpMLineIndex: NSNumber?
    var sdpMid: String?
    var usernameFragment: String?
}

struct CallState: Codable {
    var connectionState: String
    var iceConnectionState: String
    var iceGatheringSatet: String
    var signallingState: String
}

enum RTCSdpType: String, Codable {
    case answer = "answer"
    case offer = "offer"
    case pranswer = "pranswer"
    case rollback = "rollback"
}

enum WCallCommand: Codable {
    case capabilities
    case start(media: CallMediaType, aesKey: String? = nil)
    case accept(offer: RTCSessionDescriptionInit, iceCandidates: [RTCIceCandidateInit], media: CallMediaType, aesKey: String? = nil)
    case end
//    case callCommandResponse
}

enum WCallCommandResponse: Codable {
    case offer(offer: RTCSessionDescriptionInit, iceCanddiates: [RTCIceCandidateInit])
    case answer(offer: RTCSessionDescriptionInit, iceCanddiates: [RTCIceCandidateInit])
    case iceCandidates(iceCandidates: [RTCIceCandidateInit])
}

enum WCallMessageTag: String, Codable {
    case capabilities = "capabilities"
    case connection = "connection"
    case start = "start"
    case offer = "offer"
    case accept = "accept"
    case answer = "answer"
    case ice = "ice"
    case end = "end"
    case ended = "ended"
    case ok = "ok"
    case error = "error"
}

enum WCallResponse: Codable {
    case capabilities(capabilities: CallCapabilities)
    case connection(state: CallState)
    case ended
    case ok
    case error(message: String)
    //    case callCommandResponse
}

enum CallMediaType: String, Codable {
    case video = "video"
    case audio = "audio"
}

//interface WCAcceptOffer extends IWebCallMessage {
//  type: "accept"
//  offer: RTCSessionDescriptionInit
//  iceCandidates: RTCIceCandidateInit[]
//  media: CallMediaType
//  aesKey?: string
//}
//type WCallCommand = WCCapabilities | WCStartCall | WCAcceptOffer | WCEndCall | WCallCommandResponse
//
//type WCallResponse = WRCapabilities | WRConnection | WRCallEnded | WROk | WRError | WCallCommandResponse
//
//type WCallCommandResponse = WCallOffer | WCallAnswer | WCallIceCandidates
///
//interface IWebCallMessage {
//  type: WCallMessageTag
//}
//

//
//interface WRCallEnded extends IWebCallMessage {
//  type: "ended"
//}
//
//interface WROk extends IWebCallMessage {
//  type: "ok"
//}
//
//interface WRError extends IWebCallMessage {
//  type: "error"
//  message: string
//}
