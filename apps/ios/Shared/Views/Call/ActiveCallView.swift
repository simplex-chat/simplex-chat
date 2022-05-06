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
    @Binding var showCallView: Bool
    @State private var coordinator: WebRTCCoordinator? = nil
    @State private var webViewReady: Bool = false
    @State private var webViewMsg: WCallResponse? = nil

    var body: some View {
        ZStack(alignment: .topTrailing) {
            WebRTCView(coordinator: $coordinator, webViewReady: $webViewReady, webViewMsg: $webViewMsg)
                .onAppear() { sendCommandToWebView() }
                .onChange(of: chatModel.callCommand) { _ in sendCommandToWebView() }
                .onChange(of: webViewReady) { _ in sendCommandToWebView() }
                .onChange(of: webViewMsg) { _ in processWebViewMessage() }
            Button {
                showCallView = false
            } label: {
                Image(systemName: "multiply")
            }
            .padding()
        }
    }

    private func sendCommandToWebView() {
        if chatModel.currentCall != nil && webViewReady,
           let cmd = chatModel.callCommand,
           let c = coordinator {
            chatModel.callCommand = nil
            logger.debug("ActiveCallView: command \(cmd.cmdType)")
            c.sendCommand(command: cmd)
        }
    }

    private func processWebViewMessage() {
        if let msg = webViewMsg,
           let call = chatModel.currentCall {
            logger.debug("ActiveCallView: response \(msg.respType)")
            Task {
                switch msg {
                case let .capabilities(capabilities):
                    let callType = CallType(media: call.localMedia, capabilities: capabilities)
                    try await apiSendCallInvitation(call.contact, callType)
                    chatModel.currentCall = call.copy(callState: .invitationSent, localCapabilities: capabilities)
                case let .offer(offer, iceCandidates, capabilities):
                    try await apiSendCallOffer(call.contact, offer, iceCandidates,
                                               media: call.localMedia, capabilities: capabilities)
                    chatModel.currentCall = call.copy(callState: .offerSent)
                case let .answer(answer, iceCandidates):
                    try await apiSendCallAnswer(call.contact, answer, iceCandidates)
                    chatModel.currentCall = call.copy(callState: .negotiated)
                case let .ice(iceCandidates):
                    try await apiSendCallExtraInfo(call.contact, iceCandidates)
                case let .connection(state):
                    try await apiCallStatus(call.contact, state.connectionState)
                case .ended:
                    try await apiEndCall(call.contact)
                case .ok:
                    logger.debug("ActiveCallView: command ok")
                case let .error(message):
                    logger.debug("ActiveCallView: command error: \(message)")
                case let .invalid(type):
                    logger.debug("ActiveCallView: invalid response: \(type)")
                }
            }
        }
    }
}

struct ActiveCallView_Previews: PreviewProvider {
    static var previews: some View {
        @State var showCallView: Bool = true
        return ActiveCallView(showCallView: $showCallView)
    }
}
