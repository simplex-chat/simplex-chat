//
//  CallManager.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 22/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import SimpleXChat

class CallManager {
    func newOutgoingCall(_ contact: Contact, _ media: CallMediaType) -> UUID {
        let uuid = UUID()
        ChatModel.shared.activeCall = Call(direction: .outgoing, contact: contact, callkitUUID: uuid, callState: .waitCapabilities, localMedia: media)
        return uuid
    }

    func startOutgoingCall(callUUID: UUID) -> Bool {
        let m = ChatModel.shared
        if let call = m.activeCall, call.callkitUUID == callUUID {
            m.showCallView = true
            m.callCommand = .capabilities(media: call.localMedia, useWorker: true)
            return true
        }
        return false
    }

    func answerIncomingCall(callUUID: UUID) -> Bool {
        if let invitation = getCallInvitation(callUUID) {
            answerIncomingCall(invitation: invitation)
            return true
        }
        return false
    }

    func answerIncomingCall(invitation: CallInvitation) {
        let m = ChatModel.shared
        m.callInvitations.removeValue(forKey: invitation.contact.id)
        m.activeCall = Call(
            direction: .incoming,
            contact: invitation.contact,
            callkitUUID: invitation.callkitUUID,
            callState: .invitationAccepted,
            localMedia: invitation.peerMedia,
            sharedKey: invitation.sharedKey
        )
        m.showCallView = true
        let useRelay = UserDefaults.standard.bool(forKey: DEFAULT_WEBRTC_POLICY_RELAY)
        logger.debug("answerIncomingCall useRelay \(useRelay)")
        m.callCommand = .start(media: invitation.peerMedia, aesKey: invitation.sharedKey, useWorker: true, relay: useRelay)
    }

    func endCall(callUUID: UUID, completed: @escaping (Bool) -> Void) {
        if let call = ChatModel.shared.activeCall, call.callkitUUID == callUUID {
            endCall(call: call) { completed(true) }
        } else if let invitation = getCallInvitation(callUUID) {
            endCall(invitation: invitation) { completed(true) }
        } else {
            completed(false)
        }
    }

    func endCall(call: Call, completed: @escaping () -> Void) {
        let m = ChatModel.shared
        if case .ended = call.callState {
            logger.debug("CallManager.endCall: call ended")
            m.activeCall = nil
            m.showCallView = false
            completed()
        } else {
            logger.debug("CallManager.endCall: ending call...")
            m.callCommand = .end
            m.showCallView = false
            Task {
                do {
                    try await apiEndCall(call.contact)
                } catch {
                    logger.error("CallController.provider apiEndCall error: \(responseError(error))")
                }
                DispatchQueue.main.async {
                    m.activeCall = nil
                    completed()
                }
            }
        }
    }

    func endCall(invitation: CallInvitation, completed: @escaping () -> Void) {
        ChatModel.shared.callInvitations.removeValue(forKey: invitation.contact.id)
        Task {
            do {
                try await apiRejectCall(invitation.contact)
            } catch {
                logger.error("CallController.provider apiRejectCall error: \(responseError(error))")
            }
            completed()
        }
    }

    private func getCallInvitation(_ callUUID: UUID) -> CallInvitation? {
        if let (_, invitation) = ChatModel.shared.callInvitations.first(where: { (_, inv) in inv.callkitUUID == callUUID }) {
            return invitation
        }
        return nil
    }
}
