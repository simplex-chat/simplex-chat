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

enum WCallCommand: Codable {
    case capabilities
    case startCall(media: CallMediaType, aesKey: String? = nil)
}

enum WCallResponse: Codable {
    case capabilities
}

enum CallMediaType: String, Codable {
    case video = "video"
    case audio = "audio"
}

//type WCallCommand = WCCapabilities | WCStartCall | WCAcceptOffer | WCEndCall | WCallCommandResponse
//
//type WCallResponse = WRCapabilities | WRConnection | WRCallEnded | WROk | WRError | WCallCommandResponse
//
//type WCallCommandResponse = WCallOffer | WCallAnswer | WCallIceCandidates
//
//type WCallMessageTag = "capabilities" | "connection" | "start" | "offer" | "accept" | "answer" | "ice" | "end" | "ended" | "ok" | "error"
//
//enum CallMediaType {
//  Audio = "audio",
//  Video = "video",
//}
//
//interface IWebCallMessage {
//  type: WCallMessageTag
//}
//
//interface WCCapabilities extends IWebCallMessage {
//  type: "capabilities"
//}
//
//interface WRConnection extends IWebCallMessage {
//  type: "connection",
//  state: {
//    connectionState: string
//    iceConnectionState: string
//    iceGatheringState: string
//    signalingState: string
//  }
//}
//
//interface WCStartCall extends IWebCallMessage {
//  type: "start"
//  media: CallMediaType
//  aesKey?: string
//}
//
//interface WCEndCall extends IWebCallMessage {
//  type: "end"
//}
//
//interface WCAcceptOffer extends IWebCallMessage {
//  type: "accept"
//  offer: RTCSessionDescriptionInit
//  iceCandidates: RTCIceCandidateInit[]
//  media: CallMediaType
//  aesKey?: string
//}
//
//interface WCallOffer extends IWebCallMessage {
//  type: "offer"
//  offer: RTCSessionDescriptionInit
//  iceCandidates: RTCIceCandidateInit[]
//}
//
//interface WCallAnswer extends IWebCallMessage {
//  type: "answer"
//  answer: RTCSessionDescriptionInit
//  iceCandidates: RTCIceCandidateInit[]
//}
//
//interface WCallIceCandidates extends IWebCallMessage {
//  type: "ice"
//  iceCandidates: RTCIceCandidateInit[]
//}
//
//interface WRCapabilities {
//  type: "capabilities"
//  capabilities: CallCapabilities
//}
//
//interface CallCapabilities {
//  encryption: boolean
//}
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
