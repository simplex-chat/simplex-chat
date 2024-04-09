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
import AVFoundation

struct ActiveCallView: View {
    @EnvironmentObject var m: ChatModel
    @Environment(\.colorScheme) var colorScheme
    @ObservedObject var call: Call
    @Environment(\.scenePhase) var scenePhase
    @State private var client: WebRTCClient? = nil
    @State private var activeCall: WebRTCClient.Call? = nil
    @State private var localRendererAspectRatio: CGFloat? = nil
    @Binding var canConnectCall: Bool
    @State var prevColorScheme: ColorScheme = .dark
    @State var pipShown = false
    @State var wasConnected = false

    var body: some View {
        ZStack(alignment: .topLeading) {
            ZStack(alignment: .bottom) {
                if let client = client, [call.peerMedia, call.localMedia].contains(.video), activeCall != nil {
                    GeometryReader { g in
                        let width = g.size.width * 0.3
                        ZStack(alignment: .topTrailing) {
                            CallViewRemote(client: client, activeCall: $activeCall, activeCallViewIsCollapsed: $m.activeCallViewIsCollapsed, pipShown: $pipShown)
                            CallViewLocal(client: client, activeCall: $activeCall, localRendererAspectRatio: $localRendererAspectRatio, pipShown: $pipShown)
                                .cornerRadius(10)
                                .frame(width: width, height: width / (localRendererAspectRatio ?? 1))
                                .padding([.top, .trailing], 17)
                            ZStack(alignment: .center) {
                                // For some reason, when the view in GeometryReader and ZStack is visible, it steals clicks on a back button, so showing something on top like this with background color helps (.clear color doesn't work)
                            }
                            .frame(maxWidth: .infinity, maxHeight: .infinity)
                            .background(Color.primary.opacity(0.000001))
                        }
                    }
                }
                if let call = m.activeCall, let client = client, (!pipShown || !call.supportsVideo) {
                    ActiveCallOverlay(call: call, client: client)
                }
            }
        }
        .allowsHitTesting(!m.activeCallViewIsCollapsed)
        .opacity(m.activeCallViewIsCollapsed ? 0 : 1)
        .onAppear {
            logger.debug("ActiveCallView: appear client is nil \(client == nil), scenePhase \(String(describing: scenePhase)), canConnectCall \(canConnectCall)")
            AppDelegate.keepScreenOn(true)
            createWebRTCClient()
            dismissAllSheets()
            hideKeyboard()
            prevColorScheme = colorScheme
        }
        .onChange(of: canConnectCall) { _ in
            logger.debug("ActiveCallView: canConnectCall changed to \(canConnectCall)")
            createWebRTCClient()
        }
        .onChange(of: m.activeCallViewIsCollapsed) { _ in
            hideKeyboard()
        }
        .onDisappear {
            logger.debug("ActiveCallView: disappear")
            Task { await m.callCommand.setClient(nil) }
            AppDelegate.keepScreenOn(false)
            client?.endCall()
            CallSoundsPlayer.shared.stop()
            try? AVAudioSession.sharedInstance().setCategory(.soloAmbient)
            if (wasConnected) {
                CallSoundsPlayer.shared.vibrate()
            }
        }
        .background(m.activeCallViewIsCollapsed ? .clear : .black)
        // Quite a big delay when opening/closing the view when a scheme changes (globally) this way. It's not needed when CallKit is used since status bar is green with white text on it
        .preferredColorScheme(m.activeCallViewIsCollapsed || CallController.useCallKit() ? prevColorScheme : .dark)
    }

    private func createWebRTCClient() {
        if client == nil && canConnectCall {
            client = WebRTCClient($activeCall, { msg in await MainActor.run { processRtcMessage(msg: msg) } }, $localRendererAspectRatio)
            Task {
                await m.callCommand.setClient(client)
            }
        }
    }

    @MainActor
    private func processRtcMessage(msg: WVAPIMessage) {
        if call == m.activeCall,
           let call = m.activeCall,
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
                    await MainActor.run {
                        call.callState = .invitationSent
                        call.localCapabilities = capabilities
                    }
                    if call.supportsVideo {
                        try? AVAudioSession.sharedInstance().setCategory(.playback, options: .defaultToSpeaker)
                    }
                    CallSoundsPlayer.shared.startConnectingCallSound()
                    Task {
                        try? await Task.sleep(nanoseconds: 7000_000000)
                        CallSoundsPlayer.shared.startInCallSound()
                    }
                }
            case let .offer(offer, iceCandidates, capabilities):
                Task {
                    do {
                        try await apiSendCallOffer(call.contact, offer, iceCandidates,
                                                   media: call.localMedia, capabilities: capabilities)
                    } catch {
                        logger.error("apiSendCallOffer \(responseError(error))")
                    }
                    await MainActor.run {
                        call.callState = .offerSent
                        call.localCapabilities = capabilities
                    }
                }
            case let .answer(answer, iceCandidates):
                Task {
                    do {
                        try await apiSendCallAnswer(call.contact, answer, iceCandidates)
                    } catch {
                        logger.error("apiSendCallAnswer \(responseError(error))")
                    }
                    await MainActor.run {
                        call.callState = .negotiated
                        CallSoundsPlayer.shared.stop()
                        try? AVAudioSession.sharedInstance().setCategory(.soloAmbient)
                    }
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
                    call.direction == .outgoing
                    ? CallController.shared.reportOutgoingCall(call: call, connectedAt: nil)
                    : CallController.shared.reportIncomingCall(call: call, connectedAt: nil)
                    call.callState = .connected
                    call.connectedAt = .now
                    if !wasConnected {
                        CallSoundsPlayer.shared.vibrate(2)
                        wasConnected = true
                    }
                }
                if state.connectionState == "closed" {
                    closeCallView(client)
                    m.activeCall = nil
                    m.activeCallViewIsCollapsed = false
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
                call.connectedAt = .now
                if !wasConnected {
                    CallSoundsPlayer.shared.vibrate(2)
                    wasConnected = true
                }
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
                case .end:
                    closeCallView(client)
                    m.activeCall = nil
                    m.activeCallViewIsCollapsed = false
                default: ()
                }
            case let .error(message):
                logger.debug("ActiveCallView: command error: \(message)")
                AlertManager.shared.showAlert(Alert(title: Text("Error"), message: Text(message)))
            case let .invalid(type):
                logger.debug("ActiveCallView: invalid response: \(type)")
                AlertManager.shared.showAlert(Alert(title: Text("Invalid response"), message: Text(type)))
            }
        }
    }

    private func closeCallView(_ client: WebRTCClient) {
        if m.activeCall != nil {
            m.showCallView = false
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
                videoCallInfoView(call)
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
                ZStack(alignment: .topLeading) {
                    Button {
                        chatModel.activeCallViewIsCollapsed = true
                    } label: {
                        Label("Back", systemImage: "chevron.left")
                            .padding()
                            .foregroundColor(.white.opacity(0.8))
                    }
                    VStack {
                        ProfileImage(imageStr: call.contact.profile.image)
                            .scaledToFit()
                            .frame(width: 192, height: 192)
                        audioCallInfoView(call)
                    }
                    .foregroundColor(.white)
                    .opacity(0.8)
                    .padding()
                    .frame(maxHeight: .infinity)
                }

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

    private func audioCallInfoView(_ call: Call) -> some View {
        VStack {
            Text(call.contact.chatViewName)
                .lineLimit(1)
                .font(.title)
                .frame(maxWidth: .infinity, alignment: .center)
            Group {
                Text(call.callState.text)
                HStack {
                    Text(call.encryptionStatus)
                    if let connInfo = call.connectionInfo {
                        Text("(") + Text(connInfo.text) + Text(")")
                    }
                }
            }
            .font(.subheadline)
            .frame(maxWidth: .infinity, alignment: .center)
        }
    }

    private func videoCallInfoView(_ call: Call) -> some View {
        VStack {
            Button {
                chatModel.activeCallViewIsCollapsed = true
            } label: {
                HStack(alignment: .center, spacing: 16) {
                    Image(systemName: "chevron.left")
                        .resizable()
                        .frame(width: 10, height: 18)
                    Text(call.contact.chatViewName)
                        .lineLimit(1)
                        .font(.title)
                        .frame(maxWidth: .infinity, alignment: .leading)
                }
            }
            Group {
                Text(call.callState.text)
                HStack {
                    Text(call.encryptionStatus)
                    if let connInfo = call.connectionInfo {
                        Text("(") + Text(connInfo.text) + Text(")")
                    }
                }
            }
            .font(.subheadline)
            .frame(maxWidth: .infinity, alignment: .leading)
        }
    }

    private func endCallButton() -> some View {
        let cc = CallController.shared
        return callButton("phone.down.fill", width: 60, height: 60) {
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
        controlButton(call, call.speakerEnabled ? "speaker.wave.2.fill" : "speaker.wave.1.fill") {
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
        controlButton(call, "arrow.triangle.2.circlepath") {
                Task {
                    client.flipCamera()
                }
        }
    }

    @ViewBuilder private func controlButton(_ call: Call, _ imageName: String, _ perform: @escaping () -> Void) -> some View {
        if call.hasMedia {
            callButton(imageName, width: 50, height: 38, perform)
                .foregroundColor(.white)
                .opacity(0.85)
        } else {
            Color.clear.frame(width: 50, height: 38)
        }
    }

    private func callButton(_ imageName: String, width: CGFloat, height: CGFloat, _ perform: @escaping () -> Void) -> some View {
        Button {
            perform()
        } label: {
            Image(systemName: imageName)
                .resizable()
                .scaledToFit()
                .frame(maxWidth: width, maxHeight: height)
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
