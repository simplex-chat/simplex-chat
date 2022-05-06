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
    @Binding var showCallView: Bool
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
                case .ended:
                    m.activeCall = nil
                    m.activeCallInvitation = nil
                    m.callCommand = nil
                    showCallView = false
                case .ok:
                    if msg.command == .end {
                        m.activeCall = nil
                        m.activeCallInvitation = nil
                        m.callCommand = nil
                        showCallView = false
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
                }
            }
            Spacer()
            Button {
                dismiss()
            } label: {
                Image(systemName: "phone.down.fill")
                    .resizable()
                    .scaledToFit()
                    .frame(maxWidth: 60)
                    .foregroundColor(.red)
            }
            .padding(.bottom, 60)
        }
        .frame(maxWidth: .infinity)
    }

    private func callInfoView(_ call: Call, _ alignment: Alignment) -> some View {
        VStack {
            Text(call.contact.chatViewName)
                .lineLimit(1)
                .font(.title)
                .frame(maxWidth: .infinity, alignment: alignment)
            let status = call.callState == .connected
                            ? call.encrypted
                               ? "end-to-end encrypted"
                               : "no end-to-end encryption"
                            : call.callState.text
            Text(status)
                .font(.subheadline)
                .frame(maxWidth: .infinity, alignment: alignment)
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
