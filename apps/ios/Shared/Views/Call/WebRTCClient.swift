//
// Created by Avently on 09.02.2023.
// Copyright (c) 2023 SimpleX Chat. All rights reserved.
//

import WebRTC
import LZString
import SwiftUI
import SimpleXChat

final class WebRTCClient: NSObject, RTCVideoViewDelegate, RTCFrameEncryptorDelegate, RTCFrameDecryptorDelegate {
    private static let factory: RTCPeerConnectionFactory = {
        RTCInitializeSSL()
        let videoEncoderFactory = RTCDefaultVideoEncoderFactory()
        let videoDecoderFactory = RTCDefaultVideoDecoderFactory()
        videoEncoderFactory.preferredCodec = RTCVideoCodecInfo(name: kRTCVp8CodecName)
        return RTCPeerConnectionFactory(encoderFactory: videoEncoderFactory, decoderFactory: videoDecoderFactory)
    }()
    private static let ivTagBytes: Int = 28
    private static let enableEncryption: Bool = true

    struct Call {
        var connection: RTCPeerConnection
        var iceCandidates: [RTCIceCandidate]
        var localMedia: CallMediaType
        var localCamera: RTCVideoCapturer?
        var localVideoSource: RTCVideoSource?
        var localStream: RTCVideoTrack?
        var remoteStream: RTCVideoTrack?
        var device: AVCaptureDevice.Position = .front
        var aesKey: String?
        var frameEncryptor: RTCFrameEncryptor?
        var frameDecryptor: RTCFrameDecryptor?
    }

    private let rtcAudioSession =  RTCAudioSession.sharedInstance()
    private let audioQueue = DispatchQueue(label: "audio")
    private var sendCallResponse: (WVAPIMessage) async -> Void
    private var activeCall: Binding<Call?>
    private var localRendererAspectRatio: Binding<CGFloat?>

    @available(*, unavailable)
    override init() {
        fatalError("Unimplemented")
    }

    required init(_ activeCall: Binding<Call?>, _ sendCallResponse: @escaping (WVAPIMessage) async -> Void, _ localRendererAspectRatio: Binding<CGFloat?>) {
        self.sendCallResponse = sendCallResponse
        self.activeCall = activeCall
        self.localRendererAspectRatio = localRendererAspectRatio
        rtcAudioSession.useManualAudio = CallController.useCallKit()
        rtcAudioSession.isAudioEnabled = !CallController.useCallKit()
        logger.debug("WebRTCClient: rtcAudioSession has manual audio \(self.rtcAudioSession.useManualAudio) and audio enabled \(self.rtcAudioSession.isAudioEnabled)}")
        super.init()
    }

    let defaultIceServers: [WebRTC.RTCIceServer] = [
        WebRTC.RTCIceServer(urlStrings: ["stun:stun.simplex.im:443"]),
        WebRTC.RTCIceServer(urlStrings: ["turn:turn.simplex.im:443?transport=udp"], username: "private", credential: "yleob6AVkiNI87hpR94Z"),
        WebRTC.RTCIceServer(urlStrings: ["turn:turn.simplex.im:443?transport=tcp"], username: "private", credential: "yleob6AVkiNI87hpR94Z"),
    ]

    func initializeCall(_ iceServers: [WebRTC.RTCIceServer]?, _ remoteIceCandidates: [RTCIceCandidate], _ mediaType: CallMediaType, _ aesKey: String?, _ relay: Bool?) -> Call {
        let connection = createPeerConnection(iceServers ?? getWebRTCIceServers() ?? defaultIceServers, relay)
        connection.delegate = self
        createAudioSender(connection)
        var localStream: RTCVideoTrack? = nil
        var remoteStream: RTCVideoTrack? = nil
        var localCamera: RTCVideoCapturer? = nil
        var localVideoSource: RTCVideoSource? = nil
        if mediaType == .video {
            (localStream, remoteStream, localCamera, localVideoSource) = createVideoSender(connection)
        }
        var frameEncryptor: RTCFrameEncryptor? = nil
        var frameDecryptor: RTCFrameDecryptor? = nil
        if aesKey != nil {
            let encryptor = RTCFrameEncryptor.init(sizeChange: Int32(WebRTCClient.ivTagBytes))
            encryptor.delegate = self
            frameEncryptor = encryptor
            connection.senders.forEach { $0.setRtcFrameEncryptor(encryptor) }

            let decryptor = RTCFrameDecryptor.init(sizeChange: -Int32(WebRTCClient.ivTagBytes))
            decryptor.delegate = self
            frameDecryptor = decryptor
            // Has no video receiver in outgoing call if applied here, see [peerConnection(_ connection: RTCPeerConnection, didChange newState]
            // connection.receivers.forEach { $0.setRtcFrameDecryptor(decryptor) }
        }
        return Call(
            connection: connection,
            iceCandidates: remoteIceCandidates,
            localMedia: mediaType,
            localCamera: localCamera,
            localVideoSource: localVideoSource,
            localStream: localStream,
            remoteStream: remoteStream,
            aesKey: aesKey,
            frameEncryptor: frameEncryptor,
            frameDecryptor: frameDecryptor
        )
    }

    func createPeerConnection(_ iceServers: [WebRTC.RTCIceServer], _ relay: Bool?) -> RTCPeerConnection {
        let constraints = RTCMediaConstraints(mandatoryConstraints: nil,
            optionalConstraints: ["DtlsSrtpKeyAgreement": kRTCMediaConstraintsValueTrue])

        guard let connection = WebRTCClient.factory.peerConnection(
            with: getCallConfig(iceServers, relay),
            constraints: constraints, delegate: nil
        )
        else {
            fatalError("Unable to create RTCPeerConnection")
        }
        return connection
    }

    func getCallConfig(_ iceServers: [WebRTC.RTCIceServer], _ relay: Bool?) -> RTCConfiguration {
        let config = RTCConfiguration()
        config.iceServers = iceServers
        config.sdpSemantics = .unifiedPlan
        config.continualGatheringPolicy = .gatherContinually
        config.iceTransportPolicy = relay == true ? .relay : .all
        // Allows to wait 30 sec before `failing` connection if the answer from remote side is not received in time
        config.iceInactiveTimeout = 30_000
        return config
    }

    func addIceCandidates(_ connection: RTCPeerConnection, _ remoteIceCandidates: [RTCIceCandidate]) {
        remoteIceCandidates.forEach { candidate in
            connection.add(candidate.toWebRTCCandidate()) { error in
                if let error = error {
                    logger.error("Adding candidate error \(error)")
                }
            }
        }
    }

    func sendCallCommand(command: WCallCommand) async {
        var resp: WCallResponse? = nil
        let pc = activeCall.wrappedValue?.connection
        switch command {
        case .capabilities:
            resp = .capabilities(capabilities: CallCapabilities(encryption: WebRTCClient.enableEncryption))
        case let .start(media: media, aesKey, iceServers, relay):
            logger.debug("starting incoming call - create webrtc session")
            if activeCall.wrappedValue != nil { endCall() }
            let encryption = WebRTCClient.enableEncryption
            let call = initializeCall(iceServers?.toWebRTCIceServers(), [], media, encryption ? aesKey : nil, relay)
            activeCall.wrappedValue = call
            call.connection.offer { answer in
                Task {
                    let gotCandidates = await self.waitWithTimeout(10_000, stepMs: 1000, until: { self.activeCall.wrappedValue?.iceCandidates.count ?? 0 > 0 })
                    if gotCandidates {
                        await self.sendCallResponse(.init(
                            corrId: nil,
                            resp: .offer(
                                offer: compressToBase64(input: encodeJSON(CustomRTCSessionDescription(type: answer.type.toSdpType(), sdp: answer.sdp))),
                                iceCandidates: compressToBase64(input: encodeJSON(self.activeCall.wrappedValue?.iceCandidates ?? [])),
                                capabilities: CallCapabilities(encryption: encryption)
                            ),
                            command: command)
                        )
                    } else {
                        self.endCall()
                    }
                }

            }
        case let .offer(offer, iceCandidates, media, aesKey, iceServers, relay):
            if activeCall.wrappedValue != nil {
                resp = .error(message: "accept: call already started")
            } else if !WebRTCClient.enableEncryption && aesKey != nil {
                resp = .error(message: "accept: encryption is not supported")
            } else if let offer: CustomRTCSessionDescription = decodeJSON(decompressFromBase64(input: offer)),
                      let remoteIceCandidates: [RTCIceCandidate] = decodeJSON(decompressFromBase64(input: iceCandidates)) {
                let call = initializeCall(iceServers?.toWebRTCIceServers(), remoteIceCandidates, media, WebRTCClient.enableEncryption ? aesKey : nil, relay)
                activeCall.wrappedValue = call
                let pc = call.connection
                if let type = offer.type, let sdp = offer.sdp {
                    if (try? await pc.setRemoteDescription(RTCSessionDescription(type: type.toWebRTCSdpType(), sdp: sdp))) != nil {
                        pc.answer { answer in
                            self.addIceCandidates(pc, remoteIceCandidates)
//                            Task {
//                                try? await Task.sleep(nanoseconds: 32_000 * 1000000)
                            Task {
                                await self.sendCallResponse(.init(
                                    corrId: nil,
                                    resp: .answer(
                                        answer: compressToBase64(input: encodeJSON(CustomRTCSessionDescription(type: answer.type.toSdpType(), sdp: answer.sdp))),
                                        iceCandidates: compressToBase64(input: encodeJSON(call.iceCandidates))
                                    ),
                                    command: command)
                                )
                            }
//                            }
                        }
                    } else {
                        resp = .error(message: "accept: remote description is not set")
                    }
                }
            }
        case let .answer(answer, iceCandidates):
            if pc == nil {
                resp = .error(message: "answer: call not started")
            } else if pc?.localDescription == nil {
                resp = .error(message: "answer: local description is not set")
            } else if pc?.remoteDescription != nil {
                resp = .error(message: "answer: remote description already set")
            } else if let answer: CustomRTCSessionDescription = decodeJSON(decompressFromBase64(input: answer)),
                      let remoteIceCandidates: [RTCIceCandidate] = decodeJSON(decompressFromBase64(input: iceCandidates)),
                      let type = answer.type, let sdp = answer.sdp,
                      let pc = pc {
                if (try? await pc.setRemoteDescription(RTCSessionDescription(type: type.toWebRTCSdpType(), sdp: sdp))) != nil {
                    addIceCandidates(pc, remoteIceCandidates)
                    resp = .ok
                } else {
                    resp = .error(message: "answer: remote description is not set")
                }
            }
        case let .ice(iceCandidates):
            if let pc = pc,
               let remoteIceCandidates: [RTCIceCandidate] = decodeJSON(decompressFromBase64(input: iceCandidates)) {
                addIceCandidates(pc, remoteIceCandidates)
                resp = .ok
            } else {
                resp = .error(message: "ice: call not started")
            }
        case let .media(media, enable):
            if activeCall.wrappedValue == nil {
                resp = .error(message: "media: call not started")
            } else if activeCall.wrappedValue?.localMedia == .audio && media == .video {
                resp = .error(message: "media: no video")
            } else {
                enableMedia(media, enable)
                resp = .ok
            }
        case .end:
            await sendCallResponse(.init(corrId: nil, resp: .ok, command: command))
            endCall()
        }
        if let resp = resp {
            await sendCallResponse(.init(corrId: nil, resp: resp, command: command))
        }
    }

    func enableMedia(_ media: CallMediaType, _ enable: Bool) {
        logger.debug("WebRTCClient: enabling media \(media.rawValue) \(enable)")
        media == .video ? setVideoEnabled(enable) : setAudioEnabled(enable)
    }

    func addLocalRenderer(_ activeCall: Call, _ renderer: RTCEAGLVideoView) {
        activeCall.localStream?.add(renderer)
        // To get width and height of a frame, see videoView(videoView:, didChangeVideoSize)
        renderer.delegate = self
    }

    func videoView(_ videoView: RTCVideoRenderer, didChangeVideoSize size: CGSize) {
        guard size.height > 0 else { return }
        localRendererAspectRatio.wrappedValue = size.width / size.height
    }

    func frameDecryptor(_ decryptor: RTCFrameDecryptor, mediaType: RTCRtpMediaType, withFrame encrypted: Data) -> Data? {
        guard encrypted.count > 0 else { return nil }
        if var key: [CChar] = activeCall.wrappedValue?.aesKey?.cString(using: .utf8),
           let pointer: UnsafeMutableRawPointer = malloc(encrypted.count) {
            memcpy(pointer, (encrypted as NSData).bytes, encrypted.count)
            let isKeyFrame = encrypted[0] & 1 == 0
            let clearTextBytesSize = mediaType.rawValue == 0 ? 1 : isKeyFrame ? 10 : 3
            logCrypto("decrypt", chat_decrypt_media(&key, pointer.advanced(by: clearTextBytesSize), Int32(encrypted.count - clearTextBytesSize)))
            return Data(bytes: pointer, count: encrypted.count - WebRTCClient.ivTagBytes)
        } else {
            return nil
        }
    }

    func frameEncryptor(_ encryptor: RTCFrameEncryptor, mediaType: RTCRtpMediaType, withFrame unencrypted: Data) -> Data? {
        guard unencrypted.count > 0 else { return nil }
        if var key: [CChar] = activeCall.wrappedValue?.aesKey?.cString(using: .utf8),
           let pointer: UnsafeMutableRawPointer = malloc(unencrypted.count + WebRTCClient.ivTagBytes) {
            memcpy(pointer, (unencrypted as NSData).bytes, unencrypted.count)
            let isKeyFrame = unencrypted[0] & 1 == 0
            let clearTextBytesSize = mediaType.rawValue == 0 ? 1 : isKeyFrame ? 10 : 3
            logCrypto("encrypt", chat_encrypt_media(&key, pointer.advanced(by: clearTextBytesSize), Int32(unencrypted.count + WebRTCClient.ivTagBytes - clearTextBytesSize)))
            return Data(bytes: pointer, count: unencrypted.count + WebRTCClient.ivTagBytes)
        } else {
            return nil
        }
    }

    private func logCrypto(_ op: String, _ r: UnsafeMutablePointer<CChar>?) {
        if let r = r {
            let err = fromCString(r)
            if err != "" {
                logger.error("\(op) error: \(err)")
//            } else {
//                logger.debug("\(op) ok")
            }
        }
    }

    func addRemoteRenderer(_ activeCall: Call, _ renderer: RTCVideoRenderer) {
        activeCall.remoteStream?.add(renderer)
    }

    func startCaptureLocalVideo(_ activeCall: Call) {
#if targetEnvironment(simulator)
        guard
            let capturer = activeCall.localCamera as? RTCFileVideoCapturer
        else {
            logger.error("Unable to work with a file capturer")
            return
        }
        capturer.stopCapture()
        // Drag video file named `video.mp4` to `sounds` directory in the project from any other path in filesystem
        capturer.startCapturing(fromFileNamed: "sounds/video.mp4")
#else
        guard
            let capturer = activeCall.localCamera as? RTCCameraVideoCapturer,
            let camera = (RTCCameraVideoCapturer.captureDevices().first { $0.position == activeCall.device })
        else {
            logger.error("Unable to find a camera")
            return
        }

        let supported = RTCCameraVideoCapturer.supportedFormats(for: camera)
        let height: (AVCaptureDevice.Format) -> Int32 = { (format: AVCaptureDevice.Format) in CMVideoFormatDescriptionGetDimensions(format.formatDescription).height }
        let format = supported.first(where: { height($0) == 1280 })
                    ?? supported.first(where: { height($0) >= 480 && height($0) < 1280 })
                    ?? supported.first(where: { height($0) > 1280 })
        guard
            let format = format,
            let fps = format.videoSupportedFrameRateRanges.max(by: { $0.maxFrameRate < $1.maxFrameRate })
        else {
            logger.error("Unable to find any format for camera or to choose FPS")
            return
        }

        logger.debug("Format for camera is \(format.description)")

        capturer.stopCapture()
        capturer.startCapture(with: camera,
            format: format,
            fps: Int(min(24, fps.maxFrameRate)))
#endif
    }

    private func createAudioSender(_ connection: RTCPeerConnection) {
        let streamId = "stream"
        let audioTrack = createAudioTrack()
        connection.add(audioTrack, streamIds: [streamId])
    }

    private func createVideoSender(_ connection: RTCPeerConnection) -> (RTCVideoTrack?, RTCVideoTrack?, RTCVideoCapturer?, RTCVideoSource?) {
        let streamId = "stream"
        let (localVideoTrack, localCamera, localVideoSource) = createVideoTrack()
        connection.add(localVideoTrack, streamIds: [streamId])
        return (localVideoTrack, connection.transceivers.first { $0.mediaType == .video }?.receiver.track as? RTCVideoTrack, localCamera, localVideoSource)
    }

    private func createAudioTrack() -> RTCAudioTrack {
        let audioConstrains = RTCMediaConstraints(mandatoryConstraints: nil, optionalConstraints: nil)
        let audioSource = WebRTCClient.factory.audioSource(with: audioConstrains)
        let audioTrack = WebRTCClient.factory.audioTrack(with: audioSource, trackId: "audio0")
        return audioTrack
    }

    private func createVideoTrack() -> (RTCVideoTrack, RTCVideoCapturer, RTCVideoSource) {
        let localVideoSource = WebRTCClient.factory.videoSource()

        #if targetEnvironment(simulator)
        let localCamera = RTCFileVideoCapturer(delegate: localVideoSource)
        #else
        let localCamera = RTCCameraVideoCapturer(delegate: localVideoSource)
        #endif

        let localVideoTrack = WebRTCClient.factory.videoTrack(with: localVideoSource, trackId: "video0")
        return (localVideoTrack, localCamera, localVideoSource)
    }

    func endCall() {
        guard let call = activeCall.wrappedValue else { return }
        logger.debug("WebRTCClient: ending the call")
        activeCall.wrappedValue = nil
        call.connection.close()
        call.connection.delegate = nil
        call.frameEncryptor?.delegate = nil
        call.frameDecryptor?.delegate = nil
        audioSessionToDefaults()
    }

    func waitWithTimeout(_ timeoutMs: UInt64, stepMs: UInt64, until success: () -> Bool) async -> Bool {
        let startedAt = DispatchTime.now()
        while !success() && startedAt.uptimeNanoseconds + timeoutMs * 1000000 > DispatchTime.now().uptimeNanoseconds {
            guard let _ = try? await Task.sleep(nanoseconds: stepMs * 1000000) else { break }
        }
        return success()
    }
}


extension WebRTC.RTCPeerConnection {
    func mediaConstraints() -> RTCMediaConstraints {
        RTCMediaConstraints(
            mandatoryConstraints: [kRTCMediaConstraintsOfferToReceiveAudio: kRTCMediaConstraintsValueTrue,
                                   kRTCMediaConstraintsOfferToReceiveVideo: kRTCMediaConstraintsValueTrue],
            optionalConstraints: nil)
    }

    func offer(_ completion: @escaping (_ sdp: RTCSessionDescription) -> Void) {
        offer(for: mediaConstraints()) { (sdp, error) in
            guard let sdp = sdp else {
                return
            }
            self.setLocalDescription(sdp, completionHandler: { (error) in
                completion(sdp)
            })
        }
    }

    func answer(_ completion: @escaping (_ sdp: RTCSessionDescription) -> Void)  {
        answer(for: mediaConstraints()) { (sdp, error) in
            guard let sdp = sdp else {
                return
            }
            self.setLocalDescription(sdp, completionHandler: { (error) in
                completion(sdp)
            })
        }
    }
}

extension WebRTCClient: RTCPeerConnectionDelegate {
    func peerConnection(_ connection: RTCPeerConnection, didChange stateChanged: RTCSignalingState) {
        logger.debug("Connection new signaling state: \(stateChanged.rawValue)")
    }

    func peerConnection(_ connection: RTCPeerConnection, didAdd stream: RTCMediaStream) {
        logger.debug("Connection did add stream")
    }

    func peerConnection(_ connection: RTCPeerConnection, didRemove stream: RTCMediaStream) {
        logger.debug("Connection did remove stream")
    }

    func peerConnectionShouldNegotiate(_ connection: RTCPeerConnection) {
        logger.debug("Connection should negotiate")
    }

    func peerConnection(_ connection: RTCPeerConnection, didChange newState: RTCIceConnectionState) {
        debugPrint("Connection new connection state: \(newState.toString() ?? "" + newState.rawValue.description) \(connection.receivers)")

        guard let call = activeCall.wrappedValue,
              let connectionStateString = newState.toString(),
              let iceConnectionStateString = connection.iceConnectionState.toString(),
              let iceGatheringStateString = connection.iceGatheringState.toString(),
              let signalingStateString = connection.signalingState.toString()
        else {
            return
        }
        Task {
            await sendCallResponse(.init(
                corrId: nil,
                resp: .connection(state: ConnectionState(
                    connectionState: connectionStateString,
                    iceConnectionState: iceConnectionStateString,
                    iceGatheringState: iceGatheringStateString,
                    signalingState: signalingStateString)
                ),
                command: nil)
            )

            switch newState {
            case .checking:
                if let frameDecryptor = activeCall.wrappedValue?.frameDecryptor {
                    connection.receivers.forEach { $0.setRtcFrameDecryptor(frameDecryptor) }
                }
                let enableSpeaker: Bool
                switch call.localMedia {
                case .video: enableSpeaker = true
                default: enableSpeaker = false
                }
                setSpeakerEnabledAndConfigureSession(enableSpeaker)
            case .disconnected, .failed: endCall()
            default: do {}
            }
        }
    }

    func peerConnection(_ connection: RTCPeerConnection, didChange newState: RTCIceGatheringState) {
        logger.debug("connection new gathering state: \(newState.toString() ?? "" + newState.rawValue.description)")
    }

    func peerConnection(_ connection: RTCPeerConnection, didGenerate candidate: WebRTC.RTCIceCandidate) {
//        logger.debug("Connection generated candidate \(candidate.debugDescription)")
        activeCall.wrappedValue?.iceCandidates.append(candidate.toCandidate(nil, nil, nil))
    }

    func peerConnection(_ connection: RTCPeerConnection, didRemove candidates: [WebRTC.RTCIceCandidate]) {
        logger.debug("Connection did remove candidates")
    }

    func peerConnection(_ connection: RTCPeerConnection, didOpen dataChannel: RTCDataChannel) {}

    func peerConnection(_ connection: RTCPeerConnection,
                        didChangeLocalCandidate local: WebRTC.RTCIceCandidate,
                        remoteCandidate remote: WebRTC.RTCIceCandidate,
                        lastReceivedMs lastDataReceivedMs: Int32,
                        changeReason reason: String) {
//        logger.debug("Connection changed candidate \(reason) \(remote.debugDescription) \(remote.description)")
        sendConnectedEvent(connection, local: local, remote: remote)
    }

    func sendConnectedEvent(_ connection: WebRTC.RTCPeerConnection, local: WebRTC.RTCIceCandidate, remote: WebRTC.RTCIceCandidate) {
        connection.statistics { (stats: RTCStatisticsReport) in
            stats.statistics.values.forEach { stat in
//                logger.debug("Stat \(stat.debugDescription)")
                if stat.type == "candidate-pair", stat.values["state"] as? String == "succeeded",
                   let localId = stat.values["localCandidateId"] as? String,
                   let remoteId = stat.values["remoteCandidateId"] as? String,
                   let localStats = stats.statistics[localId],
                   let remoteStats = stats.statistics[remoteId],
                   local.sdp.contains("\((localStats.values["ip"] as? String ?? "--")) \((localStats.values["port"] as? String ?? "--"))") &&
                   remote.sdp.contains("\((remoteStats.values["ip"] as? String ?? "--")) \((remoteStats.values["port"] as? String ?? "--"))")
                {
                    Task {
                        await self.sendCallResponse(.init(
                            corrId: nil,
                            resp: .connected(connectionInfo: ConnectionInfo(
                                localCandidate: local.toCandidate(
                                    RTCIceCandidateType.init(rawValue: localStats.values["candidateType"] as! String),
                                    localStats.values["protocol"] as? String,
                                    localStats.values["relayProtocol"] as? String
                                ),
                                remoteCandidate: remote.toCandidate(
                                    RTCIceCandidateType.init(rawValue: remoteStats.values["candidateType"] as! String),
                                    remoteStats.values["protocol"] as? String,
                                    remoteStats.values["relayProtocol"] as? String
                                ))),
                            command: nil)
                        )
                    }
                }
            }
        }
    }
}

extension WebRTCClient {
    func setAudioEnabled(_ enabled: Bool) {
        setTrackEnabled(RTCAudioTrack.self, enabled)
    }

    func setSpeakerEnabledAndConfigureSession( _ enabled: Bool) {
        logger.debug("WebRTCClient: configuring session with speaker enabled \(enabled)")
        audioQueue.async { [weak self] in
            guard let self = self else { return }
            self.rtcAudioSession.lockForConfiguration()
            defer {
                self.rtcAudioSession.unlockForConfiguration()
            }
            do {
                try self.rtcAudioSession.setCategory(AVAudioSession.Category.playAndRecord.rawValue)
                try self.rtcAudioSession.setMode(AVAudioSession.Mode.voiceChat.rawValue)
                try self.rtcAudioSession.overrideOutputAudioPort(enabled ? .speaker : .none)
                try self.rtcAudioSession.setActive(true)
                logger.debug("WebRTCClient: configuring session with speaker enabled \(enabled) success")
            } catch let error {
                logger.debug("Error configuring AVAudioSession: \(error)")
            }
        }
    }

    func audioSessionToDefaults() {
        logger.debug("WebRTCClient: audioSession to defaults")
        audioQueue.async { [weak self] in
            guard let self = self else { return }
            self.rtcAudioSession.lockForConfiguration()
            defer {
                self.rtcAudioSession.unlockForConfiguration()
            }
            do {
                try self.rtcAudioSession.setCategory(AVAudioSession.Category.ambient.rawValue)
                try self.rtcAudioSession.setMode(AVAudioSession.Mode.default.rawValue)
                try self.rtcAudioSession.overrideOutputAudioPort(.none)
                try self.rtcAudioSession.setActive(false)
                logger.debug("WebRTCClient: audioSession to defaults success")
            } catch let error {
                logger.debug("Error configuring AVAudioSession with defaults: \(error)")
            }
        }
    }

    func setVideoEnabled(_ enabled: Bool) {
        setTrackEnabled(RTCVideoTrack.self, enabled)
    }

    func flipCamera() {
        switch activeCall.wrappedValue?.device {
        case .front: activeCall.wrappedValue?.device = .back
        case .back: activeCall.wrappedValue?.device = .front
        default: ()
        }
        if let call = activeCall.wrappedValue {
            startCaptureLocalVideo(call)
        }
    }

    private func setTrackEnabled<T: RTCMediaStreamTrack>(_ type: T.Type, _ enabled: Bool) {
        activeCall.wrappedValue?.connection.transceivers
        .compactMap { $0.sender.track as? T }
        .forEach { $0.isEnabled = enabled }
    }
}

struct CustomRTCSessionDescription: Codable {
    public var type: RTCSdpType?
    public var sdp: String?
}

enum RTCSdpType: String, Codable {
    case answer
    case offer
    case pranswer
    case rollback
}

extension RTCIceCandidate {
    func toWebRTCCandidate() -> WebRTC.RTCIceCandidate {
        WebRTC.RTCIceCandidate(
            sdp: candidate,
            sdpMLineIndex: Int32(sdpMLineIndex ?? 0),
            sdpMid: sdpMid
        )
    }
}

extension WebRTC.RTCIceCandidate {
    func toCandidate(_ candidateType: RTCIceCandidateType?, _ protocol: String?, _ relayProtocol: String?) -> RTCIceCandidate {
        RTCIceCandidate(
            candidateType: candidateType,
            protocol: `protocol`,
            relayProtocol: relayProtocol,
            sdpMid: sdpMid,
            sdpMLineIndex: Int(sdpMLineIndex),
            candidate: sdp
        )
    }
}


extension [RTCIceServer] {
    func toWebRTCIceServers() -> [WebRTC.RTCIceServer] {
        self.map {
            WebRTC.RTCIceServer(
                urlStrings: $0.urls,
                username: $0.username,
                credential: $0.credential
            ) }
    }
}

extension RTCSdpType {
    func toWebRTCSdpType() -> WebRTC.RTCSdpType {
        switch self {
        case .answer: return WebRTC.RTCSdpType.answer
        case .offer: return WebRTC.RTCSdpType.offer
        case .pranswer: return WebRTC.RTCSdpType.prAnswer
        case .rollback: return WebRTC.RTCSdpType.rollback
        }
    }
}

extension WebRTC.RTCSdpType {
    func toSdpType() -> RTCSdpType {
        switch self {
        case .answer: return RTCSdpType.answer
        case .offer: return RTCSdpType.offer
        case .prAnswer: return RTCSdpType.pranswer
        case .rollback: return RTCSdpType.rollback
        default: return RTCSdpType.answer // should never be here
        }
    }
}

extension RTCPeerConnectionState {
    func toString() -> String? {
        switch self {
        case .new: return "new"
        case .connecting: return "connecting"
        case .connected: return "connected"
        case .failed: return"failed"
        case .disconnected: return "disconnected"
        case .closed: return "closed"
        default: return nil // unknown
        }
    }
}

extension RTCIceConnectionState {
    func toString() -> String? {
        switch self {
        case .new: return "new"
        case .checking: return "checking"
        case .connected: return "connected"
        case .completed: return "completed"
        case .failed: return "failed"
        case .disconnected: return "disconnected"
        case .closed: return "closed"
        default: return nil // unknown or unused on the other side
        }
    }
}

extension RTCIceGatheringState {
    func toString() -> String? {
        switch self {
        case .new: return "new"
        case .gathering: return "gathering"
        case .complete: return "complete"
        default: return nil // unknown
        }
    }
}

extension RTCSignalingState {
    func toString() -> String? {
        switch self {
        case .stable: return "stable"
        case .haveLocalOffer: return "have-local-offer"
        case .haveLocalPrAnswer: return "have-local-pranswer"
        case .haveRemoteOffer: return "have-remote-offer"
        case .haveRemotePrAnswer: return "have-remote-pranswer"
        case .closed: return "closed"
        default: return nil // unknown
        }
    }
}
