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
    @State var coordinator: WebRTCCoordinator? = nil

    var body: some View {
        ZStack {
            WebRTCView(coordinator: $coordinator)
                .onChange(of: chatModel.callCommand) { _ in
                    if let call = chatModel.currentCall,
                       let cmd = chatModel.callCommand,
                       let c = coordinator {
                        chatModel.callCommand = nil
                        Task {
                            let resp = await c.processCommand(command: cmd)
                            switch resp {
                            case let .capabilities(capabilities):
                                let callType = CallType(media: call.localMedia, capabilities: capabilities)
                                try await apiSendCallInvitation(call.contact, callType)
                                chatModel.currentCall = call.copy(callState: .invitationSent, localCapabilities: capabilities)
                            case let .offer(offer, iceCandidates):
                                if let capabilities = call.localCapabilities {
                                    try await apiSendCallOffer(call.contact, offer, iceCandidates,
                                                               media: call.localMedia, capabilities: capabilities)
                                    chatModel.currentCall = call.copy(callState: .offerSent)
                                } else {
                                    logger.error("ActiveCallView: offer response: no call capabilities")
                                }
                            // TODO remove accept, it's for debugging
                            case let .accept(offer, iceCandidates, _, _):
                                if let capabilities = call.localCapabilities {
                                    try await apiSendCallOffer(call.contact, offer, iceCandidates,
                                                               media: call.localMedia, capabilities: capabilities)
                                    chatModel.currentCall = call.copy(callState: .offerSent)
                                } else {
                                    logger.error("ActiveCallView: accept response: no call capabilities")
                                }
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
    }
}

struct ActiveCallView_Previews: PreviewProvider {
    static var previews: some View {
        ActiveCallView()
    }
}
