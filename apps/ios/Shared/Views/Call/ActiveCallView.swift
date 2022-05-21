//
//  ActiveCallView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 05/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ActiveCallView: View {
    @EnvironmentObject var chatModel: ChatModel
    @Environment(\.dismiss) private var dismiss
    @State private var coordinator: WebRTCCoordinator? = nil
    @State private var webViewReady: Bool = false
    @State private var webViewMsg: WVAPIMessage? = nil

    var body: some View {
        ZStack(alignment: .bottom) {
            WebRTCView(coordinator: $coordinator, webViewReady: $webViewReady, webViewMsg: $webViewMsg)
                .onAppear() { sendCommandToWebView() }
                .onChange(of: chatModel.callCommand) { _ in sendCommandToWebView() }
                .onChange(of: webViewReady) { _ in sendCommandToWebView() }
                .onChange(of: webViewMsg) { _ in processWebViewMessage() }
                .background(.black)
            ActiveCallOverlay(call: chatModel.activeCall, dismiss: { dismiss() })
        }
        .preferredColorScheme(.dark)
    }

    private func sendCommandToWebView() {
        if chatModel.activeCall != nil && webViewReady,
           let cmd = chatModel.callCommand,
           let c = coordinator {
            chatModel.callCommand = nil
            logger.debug("ActiveCallView: command \(cmd.cmdType)")
            c.sendCommand(command: cmd)
        }
    }

    private func processWebViewMessage() {
        let m = chatModel
        if let msg = webViewMsg,
           let call = chatModel.activeCall {
            logger.debug("ActiveCallView: response \(msg.resp.respType)")
            Task {
                switch msg.resp {
                case let .capabilities(capabilities):
                    let callType = CallType(media: call.localMedia, capabilities: capabilities)
                    try await apiSendCallInvitation(call.contact, callType)
                    m.activeCall = call.copy(callState: .invitationSent, localCapabilities: capabilities)
                case let .offer(offer, iceCandidates, capabilities):
                    try await apiSendCallOffer(call.contact, offer, iceCandidates,
                                               media: call.localMedia, capabilities: capabilities)
                    m.activeCall = call.copy(callState: .offerSent, localCapabilities: capabilities)
                case let .answer(answer, iceCandidates):
                    try await apiSendCallAnswer(call.contact, answer, iceCandidates)
                    m.activeCall = call.copy(callState: .negotiated)
                case let .ice(iceCandidates):
                    try await apiSendCallExtraInfo(call.contact, iceCandidates)
                case let .connection(state):
                    if let callStatus = WebRTCCallStatus.init(rawValue: state.connectionState),
                       case .connected = callStatus {
                        m.activeCall = call.copy(callState: .connected)
                    }
                    try await apiCallStatus(call.contact, state.connectionState)
                case let .connected(connectionInfo):
                    m.activeCall = call.copy(callState: .connected, connectionInfo: connectionInfo)
                case .ended:
                    m.activeCall = nil
                    m.activeCallInvitation = nil
                    m.callCommand = nil
                    m.showCallView = false
                case .ok:
                    switch msg.command {
                    case let .media(media, enable):
                        switch media {
                        case .video: m.activeCall = call.copy(videoEnabled: enable)
                        case .audio: m.activeCall = call.copy(audioEnabled: enable)
                        }
                    case let .camera(camera):
                        m.activeCall = call.copy(localCamera: camera)
                    case .end:
                        m.activeCall = nil
                        m.activeCallInvitation = nil
                        m.callCommand = nil
                        m.showCallView = false
                    default: ()
                    }
                case let .error(message):
                    logger.debug("ActiveCallView: command error: \(message)")
                case let .invalid(type):
                    logger.debug("ActiveCallView: invalid response: \(type)")
                }
            }
        }
    }
}

struct ActiveCallOverlay: View {
    @EnvironmentObject var chatModel: ChatModel
    var call: Call?
    var dismiss: () -> Void

    var body: some View {
        VStack {
            if let call = call {
                switch call.localMedia {
                case .video:
                    callInfoView(call, .leading)
                    .foregroundColor(.white)
                    .opacity(0.8)
                    .padding()

                    Spacer()

                    HStack {
                        controlButton(call, call.audioEnabled ? "mic.fill" : "mic.slash") {
                            chatModel.callCommand = .media(media: .audio, enable: !call.audioEnabled)
                        }
                        Spacer()
                        Color.clear.frame(width: 40, height: 40)
                        Spacer()
                        callButton("phone.down.fill", size: 60) { dismiss() }
                            .foregroundColor(.red)
                        Spacer()
                        controlButton(call, "arrow.triangle.2.circlepath") {
                            chatModel.callCommand = .camera(camera: call.localCamera == .user ? .environment : .user)
                        }
                        Spacer()
                        controlButton(call, call.videoEnabled ? "video.fill" : "video.slash") {
                            chatModel.callCommand = .media(media: .video, enable: !call.videoEnabled)
                        }
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
                        controlButton(call, call.audioEnabled ? "mic.fill" : "mic.slash") {
                            chatModel.callCommand = .media(media: .audio, enable: !call.audioEnabled)
                        }
                        .frame(maxWidth: .infinity, alignment: .leading)
                        callButton("phone.down.fill", size: 60) { dismiss() }
                            .foregroundColor(.red)
                    }
                    .padding(.bottom, 60)
                    .padding(.horizontal, 48)
                }
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
                    if let connInfo = call.connectionInfo?.text {
                        Text("(") + Text(connInfo) + Text(")")
                    }
                }
            }
            .font(.subheadline)
            .frame(maxWidth: .infinity, alignment: alignment)
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
            ActiveCallOverlay(call: Call(contact: Contact.sampleData, callState: .offerSent, localMedia: .video), dismiss: {})
                .background(.black)
            ActiveCallOverlay(call: Call(contact: Contact.sampleData, callState: .offerSent, localMedia: .audio), dismiss: {})
                .background(.black)
        }
    }
}
