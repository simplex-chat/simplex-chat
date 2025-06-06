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
    @State private var localRendererAspectRatio: CGFloat? = nil
    @State var remoteContentMode: UIView.ContentMode = .scaleAspectFill
    @Binding var canConnectCall: Bool
    @State var prevColorScheme: ColorScheme = .dark
    @State var pipShown = false
    @State var wasConnected = false

    var body: some View {
        ZStack(alignment: .topLeading) {
            ZStack(alignment: .bottom) {
                if let client = client, call.hasVideo {
                    GeometryReader { g in
                        let width = g.size.width * 0.3
                        ZStack(alignment: .topTrailing) {
                            ZStack(alignment: .center) {
                                // For some reason, when the view in GeometryReader and ZStack is visible, it steals clicks on a back button, so showing something on top like this with background color helps (.clear color doesn't work)
                            }
                            .frame(maxWidth: .infinity, maxHeight: .infinity)
                            .background(Color.primary.opacity(0.000001))

                            CallViewRemote(client: client, call: call, activeCallViewIsCollapsed: $m.activeCallViewIsCollapsed, contentMode: $remoteContentMode, pipShown: $pipShown)
                                .onTapGesture {
                                    remoteContentMode = remoteContentMode == .scaleAspectFill ? .scaleAspectFit : .scaleAspectFill
                                }

                            Group {
                                let localVideoTrack = client.activeCall?.localVideoTrack ?? client.notConnectedCall?.localCameraAndTrack?.1
                                if localVideoTrack != nil {
                                    CallViewLocal(client: client, localRendererAspectRatio: $localRendererAspectRatio, pipShown: $pipShown)
                                        .onDisappear {
                                            localRendererAspectRatio = nil
                                        }
                                } else {
                                    Rectangle().fill(.black)
                                }
                            }
                            .cornerRadius(10)
                            .frame(width: width, height: localRendererAspectRatio == nil ? (g.size.width < g.size.height ? width * 1.33 : width / 1.33) : width / (localRendererAspectRatio ?? 1))
                            .padding([.top, .trailing], 17)
                        }
                    }
                }
                if let call = m.activeCall, let client = client, (!pipShown || !call.hasVideo) {
                    ActiveCallOverlay(call: call, client: client)
                }
            }
        }
        .allowsHitTesting(!m.activeCallViewIsCollapsed)
        .opacity(m.activeCallViewIsCollapsed ? 0 : 1)
        .onAppear {
            logger.debug("ActiveCallView: appear client is nil \(client == nil), scenePhase \(String(describing: scenePhase)), canConnectCall \(canConnectCall)")
            AppDelegate.keepScreenOn(true)
            Task {
                await askRequiredPermissions()
            }
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
                CallSoundsPlayer.shared.vibrate(long: true)
            }
        }
        .background(m.activeCallViewIsCollapsed ? .clear : .black)
        // Quite a big delay when opening/closing the view when a scheme changes (globally) this way. It's not needed when CallKit is used since status bar is green with white text on it
        .preferredColorScheme(m.activeCallViewIsCollapsed || CallController.useCallKit() ? prevColorScheme : .dark)
    }

    private func createWebRTCClient() {
        if client == nil && canConnectCall {
            client = WebRTCClient({ msg in await MainActor.run { processRtcMessage(msg: msg) } }, $localRendererAspectRatio)
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
                let callType = CallType(media: call.initialCallType, capabilities: capabilities)
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
                    if call.hasVideo && !AVAudioSession.sharedInstance().hasExternalAudioDevice() {
                        try? AVAudioSession.sharedInstance().setCategory(.playback, options: [.allowBluetooth, .allowAirPlay, .allowBluetoothA2DP])
                    }
                    CallSoundsPlayer.shared.startConnectingCallSound()
                    activeCallWaitDeliveryReceipt()
                }
            case let .offer(offer, iceCandidates, capabilities):
                Task {
                    do {
                        try await apiSendCallOffer(call.contact, offer, iceCandidates,
                                                   media: call.initialCallType, capabilities: capabilities)
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
                        CallSoundsPlayer.shared.vibrate(long: false)
                        wasConnected = true
                    }
                }
                if state.connectionState == "closed" {
                    closeCallView(client)
                    if let callUUID = m.activeCall?.callUUID {
                        CallController.shared.endCall(callUUID: callUUID)
                    }
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
                    CallSoundsPlayer.shared.vibrate(long: false)
                    wasConnected = true
                }
            case let .peerMedia(source, enabled):
                switch source {
                    case .mic: call.peerMediaSources.mic = enabled
                    case .camera: call.peerMediaSources.camera = enabled
                    case .screenAudio: call.peerMediaSources.screenAudio = enabled
                    case .screenVideo: call.peerMediaSources.screenVideo = enabled
                    case .unknown: ()
                }
            case .ended:
                closeCallView(client)
                call.callState = .ended
                if let uuid = call.callUUID {
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

    private func activeCallWaitDeliveryReceipt() {
        ChatReceiver.shared.messagesChannel = { msg in
            guard let call = ChatModel.shared.activeCall, call.callState == .invitationSent else {
                ChatReceiver.shared.messagesChannel = nil
                return
            }
            if case let .result(.chatItemsStatusesUpdated(_, chatItems)) = msg,
               chatItems.contains(where: { ci in
                   ci.chatInfo.id == call.contact.id &&
                   ci.chatItem.content.isSndCall &&
                   ci.chatItem.meta.itemStatus.isSndRcvd
               }) {
                CallSoundsPlayer.shared.startInCallSound()
                ChatReceiver.shared.messagesChannel = nil
            }
        }
    }

    private func askRequiredPermissions() async {
        let mic = await WebRTCClient.isAuthorized(for: .audio)
        await MainActor.run {
            call.localMediaSources.mic = mic
        }
        let cameraAuthorized = AVCaptureDevice.authorizationStatus(for: .video) == .authorized
        var camera = call.initialCallType == .audio || cameraAuthorized
        if call.initialCallType == .video && !cameraAuthorized {
            camera = await WebRTCClient.isAuthorized(for: .video)
            await MainActor.run {
                if camera, let client {
                    client.setCameraEnabled(true)
                }
            }
        }
        if !mic || !camera {
            WebRTCClient.showUnauthorizedAlert(for: !mic ? .audio : .video)
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
    @ObservedObject private var deviceManager = CallAudioDeviceManager.shared

    var body: some View {
        VStack {
            switch call.hasVideo {
            case true:
                videoCallInfoView(call)
                .foregroundColor(.white)
                .opacity(0.8)
                .padding(.horizontal)
                // Fixed vertical padding required for preserving position of buttons row when changing audio-to-video and back in landscape orientation.
                // Otherwise, bigger padding is added by SwiftUI when switching call types
                .padding(.vertical, 10)
            case false:
                ZStack(alignment: .topLeading) {
                    Button {
                        chatModel.activeCallViewIsCollapsed = true
                    } label: {
                        Label("Back", systemImage: "chevron.left")
                            .padding()
                            .foregroundColor(.white.opacity(0.8))
                    }
                    VStack {
                        ProfileImage(imageStr: call.contact.profile.image, size: 192)
                        audioCallInfoView(call)
                    }
                    .foregroundColor(.white)
                    .opacity(0.8)
                    .padding(.horizontal)
                    .padding(.vertical, 10)
                    .frame(maxHeight: .infinity)
                }
            }

            Spacer()

            HStack {
                toggleMicButton()
                Spacer()
                audioDeviceButton()
                Spacer()
                endCallButton()
                Spacer()
                if call.localMediaSources.camera {
                    flipCameraButton()
                } else {
                    Color.clear.frame(width: 60, height: 60)
                }
                Spacer()
                toggleCameraButton()
            }
            .padding(.horizontal, 20)
            .padding(.bottom, 16)
            .frame(maxWidth: 440, alignment: .center)
        }
        .frame(maxWidth: .infinity)
        .onAppear {
            deviceManager.start()
        }
        .onDisappear {
            deviceManager.stop()
        }
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
                        Text(verbatim: "(") + Text(connInfo.text) + Text(verbatim: ")")
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
                        Text(verbatim: "(") + Text(connInfo.text) + Text(verbatim: ")")
                    }
                }
            }
            .font(.subheadline)
            .frame(maxWidth: .infinity, alignment: .leading)
        }
    }

    private func endCallButton() -> some View {
        let cc = CallController.shared
        return callButton("phone.down.fill", .red, padding: 10) {
            if let uuid = call.callUUID {
                cc.endCall(callUUID: uuid)
            } else {
                cc.endCall(call: call) {}
            }
        }
    }

    private func toggleMicButton() -> some View {
        controlButton(call, call.localMediaSources.mic ? "mic.fill" : "mic.slash", padding: 14) {
            Task {
                if await WebRTCClient.isAuthorized(for: .audio) {
                    client.setAudioEnabled(!call.localMediaSources.mic)
                } else { WebRTCClient.showUnauthorizedAlert(for: .audio) }
            }
        }
    }

    func audioDeviceButton() -> some View {
        // Check if the only input is microphone. And in this case show toggle button,
        // If there are more inputs, it probably means something like bluetooth headphones are available
        // and in this case show iOS button for choosing different output.
        // There is no way to get available outputs, only inputs
        Group {
            if deviceManager.availableInputs.allSatisfy({ $0.portType == .builtInMic }) {
                toggleSpeakerButton()
            } else {
                audioDevicePickerButton()
            }
        }
        .onChange(of: call.localMediaSources.hasVideo) { hasVideo in
            let current = AVAudioSession.sharedInstance().currentRoute.outputs.first?.portType
            let speakerEnabled = current == .builtInSpeaker
            let receiverEnabled = current == .builtInReceiver
            // react automatically only when receiver were selected, otherwise keep an external device selected
            if !speakerEnabled && hasVideo && receiverEnabled {
                client.setSpeakerEnabledAndConfigureSession(!speakerEnabled, skipExternalDevice: true)
                call.speakerEnabled = !speakerEnabled
            }
        }
    }

    private func toggleSpeakerButton() -> some View {
        controlButton(call, !call.peerMediaSources.mic ? "speaker.slash" : call.speakerEnabled ? "speaker.wave.2.fill" : "speaker.wave.1.fill", padding: !call.peerMediaSources.mic ? 16 : call.speakerEnabled ? 15 : 17) {
            let speakerEnabled = AVAudioSession.sharedInstance().currentRoute.outputs.first?.portType == .builtInSpeaker
            client.setSpeakerEnabledAndConfigureSession(!speakerEnabled)
            call.speakerEnabled = !speakerEnabled
        }
        .onAppear {
            deviceManager.call = call
            //call.speakerEnabled = AVAudioSession.sharedInstance().currentRoute.outputs.first?.portType == .builtInSpeaker
        }
    }

    private func toggleCameraButton() -> some View {
        controlButton(call, call.localMediaSources.camera ? "video.fill" : "video.slash", padding: call.localMediaSources.camera ? 16 : 14) {
            Task {
                if await WebRTCClient.isAuthorized(for: .video) {
                    client.setCameraEnabled(!call.localMediaSources.camera)
                } else { WebRTCClient.showUnauthorizedAlert(for: .video) }
            }
        }
        .disabled(call.initialCallType == .audio && client.activeCall?.peerHasOldVersion == true)
    }

    private func flipCameraButton() -> some View {
        controlButton(call, "arrow.triangle.2.circlepath", padding: 12) {
                Task {
                    if await WebRTCClient.isAuthorized(for: .video) {
                        client.flipCamera()
                    }
                }
        }
    }

    private func controlButton(_ call: Call, _ imageName: String, padding: CGFloat, _ perform: @escaping () -> Void) -> some View {
        callButton(imageName, call.peerMediaSources.hasVideo ? Color.black.opacity(0.2) : Color.white.opacity(0.2), padding: padding, perform)
    }

    private func audioDevicePickerButton() -> some View {
        AudioDevicePicker()
            .opacity(0.8)
            .scaleEffect(2)
            .padding(10)
            .frame(width: 60, height: 60)
            .background(call.peerMediaSources.hasVideo ? Color.black.opacity(0.2) : Color.white.opacity(0.2))
            .clipShape(.circle)
    }

    private func callButton(_ imageName: String, _ background: Color, padding: CGFloat, _ perform: @escaping () -> Void) -> some View {
        Button {
            perform()
        } label: {
            Image(systemName: imageName)
                .resizable()
                .scaledToFit()
                .padding(padding)
                .frame(width: 60, height: 60)
                .background(background)
        }
        .foregroundColor(whiteColorWithAlpha)
        .clipShape(.circle)
    }

    private var whiteColorWithAlpha: Color {
        get { Color(red: 204 / 255, green: 204 / 255, blue: 204 / 255) }
    }
}

struct ActiveCallOverlay_Previews: PreviewProvider {
    static var previews: some View {
        Group{
            ActiveCallOverlay(call: Call(direction: .incoming, contact: Contact.sampleData, callUUID: UUID().uuidString.lowercased(), callState: .offerSent, initialCallType: .video), client: WebRTCClient({ _ in }, Binding.constant(nil)))
                .background(.black)
            ActiveCallOverlay(call: Call(direction: .incoming, contact: Contact.sampleData, callUUID: UUID().uuidString.lowercased(), callState: .offerSent, initialCallType: .audio), client: WebRTCClient({ _ in }, Binding.constant(nil)))
                .background(.black)
        }
    }
}
