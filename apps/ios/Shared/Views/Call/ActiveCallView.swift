//
//  ActiveCallView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 05/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import WebKit
import SimpleXChat

struct ActiveCallView: View {
    @EnvironmentObject var m: ChatModel
    @ObservedObject var call: Call
    @State private var client: WebRTCClient? = nil
    @State private var activeCall: WebRTCClient.Call? = nil
    @State private var localRendererAspectRatio: CGFloat? = nil

    var body: some View {
        ZStack(alignment: .bottom) {
            if let client = client, [call.peerMedia, call.localMedia].contains(.video), activeCall != nil {
                ZStack(alignment: .topTrailing) {
                    CallViewRemote(client: client, activeCall: $activeCall)
                    CallViewLocal(client: client, activeCall: $activeCall, localRendererAspectRatio: $localRendererAspectRatio)
                    .cornerRadius(10)
                    .frame(width: (localRendererAspectRatio ?? 0) * 130, height: 130)
                    .padding([.top, .trailing], 17)
                }
            }
            if let call = m.activeCall, let client = client {
                ActiveCallOverlay(call: call, client: client)
            }
        }
        .onAppear {
            if client == nil {
                client = WebRTCClient($activeCall, { msg in await MainActor.run { processRtcMessage(msg: msg) } }, $localRendererAspectRatio)
                sendCommandToClient()
            }
        }
        .onChange(of: m.callCommand) { _ in sendCommandToClient()}
        .background(.black)
        .preferredColorScheme(.dark)
    }

    private func sendCommandToClient() {
        if m.activeCall != nil,
           let client = client,
           let cmd = m.callCommand {
            m.callCommand = nil
            logger.debug("sendCallCommand: \(cmd.cmdType)")
            Task {
                await client.sendCallCommand(command: cmd)
            }
        }
    }

    @MainActor
    private func processRtcMessage(msg: WVAPIMessage) {
        if let call = m.activeCall,
           let client = client {
            logger.debug("ActiveCallView: response \(msg.resp.respType)")
            switch msg.resp {
            case let .capabilities(capabilities):
                let callType = CallType(media: call.localMedia, capabilities: capabilities)
                Task {
                    do {
                        try await apiSendCallInvitation(call.contact, callType)
                    } catch {
                        logger.error("apiSendCallInvitation \(responseError(error))")
                    }
                    call.callState = .invitationSent
                    call.localCapabilities = capabilities
                }
            case let .offer(offer, iceCandidates, capabilities):
                Task {
                    do {
                        try await apiSendCallOffer(call.contact, offer, iceCandidates,
                                               media: call.localMedia, capabilities: capabilities)
                    } catch {
                        logger.error("apiSendCallOffer \(responseError(error))")
                    }
                    call.callState = .offerSent
                    call.localCapabilities = capabilities
                }
            case let .answer(answer, iceCandidates):
                Task {
                    do {
                        try await apiSendCallAnswer(call.contact, answer, iceCandidates)
                    } catch {
                        logger.error("apiSendCallAnswer \(responseError(error))")
                    }
                    call.callState = .negotiated
                }
            case let .ice(iceCandidates):
                Task {
                    do {
                        try await apiSendCallExtraInfo(call.contact, iceCandidates)
                    } catch {
                        logger.error("apiSendCallExtraInfo \(responseError(error))")
                    }
                }
            case let .connection(state):
                if let callStatus = WebRTCCallStatus.init(rawValue: state.connectionState),
                   case .connected = callStatus {
//                    if case .outgoing = call.direction {
//                        CallController.shared.reportOutgoingCall(call: call, connectedAt: nil)
//                    }
                    call.callState = .connected
                    // CallKit doesn't work well with WKWebView
                    // This is a hack to enable microphone in WKWebView after CallKit takes over it
                    if CallController.useCallKit {
                        DispatchQueue.main.asyncAfter(deadline: .now() + 2) {
                            m.callCommand = .camera(camera: call.localCamera)
                        }
                    }
                }
                if state.connectionState == "closed" && m.activeCall != nil {
                    closeCallView(client)
                    m.activeCall = nil
                }
                Task {
                    do {
                        try await apiCallStatus(call.contact, state.connectionState)
                    } catch {
                        logger.error("apiCallStatus \(responseError(error))")
                    }
                }
            case let .connected(connectionInfo):
                call.callState = .connected
                call.connectionInfo = connectionInfo
            case .ended:
                closeCallView(client)
                call.callState = .ended
                if let uuid = call.callkitUUID {
                    CallController.shared.endCall(callUUID: uuid)
                }
            case .ok:
                switch msg.command {
                case .answer:
                    call.callState = .negotiated
                case let .camera(camera):
                    call.localCamera = camera
                    Task {
                        // This disables microphone if it was disabled before flipping the camera
                        client.setAudioEnabled(call.audioEnabled)
                        // This compensates for the bug on some devices when remote video does not appear
                        // await webView.setCameraCaptureState(.muted)
                        // await webView.setCameraCaptureState(call.videoEnabled ? .active : .muted)
                    }
                case .end:
                    closeCallView(client)
                    m.activeCall = nil
                default: ()
                }
            case let .error(message):
                logger.debug("ActiveCallView: command error: \(message)")
            case let .invalid(type):
                logger.debug("ActiveCallView: invalid response: \(type)")
            }
        }
    }

    private func closeCallView(_ client: WebRTCClient) {
        m.showCallView = false
        Task {
            client.setAudioEnabled(false)
            client.setVideoEnabled(false)
        }
    }
}

struct ActiveCallOverlay: View {
    @EnvironmentObject var chatModel: ChatModel
    @ObservedObject var call: Call
    var client: WebRTCClient

    var body: some View {
        VStack {
            switch call.localMedia {
            case .video:
                callInfoView(call, .leading)
                .foregroundColor(.white)
                .opacity(0.8)
                .padding()

                Spacer()

                HStack {
                    toggleAudioButton()
                    Spacer()
                    Color.clear.frame(width: 40, height: 40)
                    Spacer()
                    endCallButton()
                    Spacer()
                    if call.videoEnabled {
                        flipCameraButton()
                    } else {
                        Color.clear.frame(width: 40, height: 40)
                    }
                    Spacer()
                    toggleVideoButton()
                }
                .padding(.horizontal, 20)
                .padding(.bottom, 16)
                .frame(maxWidth: .infinity, alignment: .center)

            case .audio:
                VStack {
                    ProfileImage(imageStr: call.contact.profile.image)
                        .scaledToFit()
                        .frame(width: 192, height: 192)
                    callInfoView(call, .center)
                }
                .foregroundColor(.white)
                .opacity(0.8)
                .padding()
                .frame(maxHeight: .infinity)

                Spacer()

                ZStack(alignment: .bottom) {
                    toggleAudioButton()
                        .frame(maxWidth: .infinity, alignment: .leading)
                    endCallButton()
                    toggleSpeakerButton()
                        .frame(maxWidth: .infinity, alignment: .trailing)
                }
                .padding(.bottom, 60)
                .padding(.horizontal, 48)
            }
        }
        .frame(maxWidth: .infinity)
    }

    private func callInfoView(_ call: Call, _ alignment: Alignment) -> some View {
        VStack {
            Text(call.contact.chatViewName)
                .lineLimit(1)
                .font(.title)
                .frame(maxWidth: .infinity, alignment: alignment)
            Group {
                Text(call.callState.text)
                HStack {
                    Text(call.encryptionStatus)
                    if let connInfo = call.connectionInfo {
//                        Text("(") + Text(connInfo.text) + Text(", \(connInfo.protocolText))")
                        Text("(") + Text(connInfo.text) + Text(")")
                    }
                }
            }
            .font(.subheadline)
            .frame(maxWidth: .infinity, alignment: alignment)
        }
    }

    private func endCallButton() -> some View {
        let cc = CallController.shared
        return callButton("phone.down.fill", size: 60) {
            if let uuid = call.callkitUUID {
                cc.endCall(callUUID: uuid)
            } else {
                cc.endCall(call: call) {}
            }
        }
        .foregroundColor(.red)
    }

    private func toggleAudioButton() -> some View {
        controlButton(call, call.audioEnabled ? "mic.fill" : "mic.slash") {
            Task {
                client.setAudioEnabled(!call.audioEnabled)
                DispatchQueue.main.async {
                    call.audioEnabled = !call.audioEnabled
                }
            }
        }
    }

    private func toggleSpeakerButton() -> some View {
        controlButton(call, call.speakerEnabled ? "speaker.fill" : "speaker.slash") {
            Task {
                client.setSpeakerEnabledAndConfigureSession(!call.speakerEnabled)
                DispatchQueue.main.async {
                    call.speakerEnabled = !call.speakerEnabled
                }
            }
        }
    }

    private func toggleVideoButton() -> some View {
        controlButton(call, call.videoEnabled ? "video.fill" : "video.slash") {
            Task {
                client.setVideoEnabled(!call.videoEnabled)
                DispatchQueue.main.async {
                    call.videoEnabled = !call.videoEnabled
                }
            }
        }
    }

    @ViewBuilder private func flipCameraButton() -> some View {
        let cmd = WCallCommand.camera(camera: call.localCamera == .user ? .environment : .user)
        controlButton(call, "arrow.triangle.2.circlepath") {
                Task {
                    // Microphone has to be enabled before flipping the camera to avoid prompt for user permission when getUserMedia is called in webview
                    client.flipCamera()
                    DispatchQueue.main.async {
                        chatModel.callCommand = cmd
                    }
                }
        }
    }

    @ViewBuilder private func controlButton(_ call: Call, _ imageName: String, _ perform: @escaping () -> Void) -> some View {
        if call.hasMedia {
            callButton(imageName, size: 40, perform)
                .foregroundColor(.white)
                .opacity(0.85)
        } else {
            Color.clear.frame(width: 40, height: 40)
        }
    }

    private func callButton(_ imageName: String, size: CGFloat, _ perform: @escaping () -> Void) -> some View {
        Button {
            perform()
        } label: {
            Image(systemName: imageName)
                .resizable()
                .scaledToFit()
                .frame(maxWidth: size, maxHeight: size)
        }
    }
}

struct ActiveCallOverlay_Previews: PreviewProvider {
    static var previews: some View {
        Group{
            ActiveCallOverlay(call: Call(direction: .incoming, contact: Contact.sampleData, callkitUUID: UUID(), callState: .offerSent, localMedia: .video), client: WebRTCClient(Binding.constant(nil), { _ in }, Binding.constant(nil)))
                .background(.black)
            ActiveCallOverlay(call: Call(direction: .incoming, contact: Contact.sampleData, callkitUUID: UUID(), callState: .offerSent, localMedia: .audio), client: WebRTCClient(Binding.constant(nil), { _ in }, Binding.constant(nil)))
                .background(.black)
        }
    }
}
