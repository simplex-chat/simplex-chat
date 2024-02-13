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
        let call = Call(direction: .outgoing, contact: contact, callkitUUID: uuid, callState: .waitCapabilities, localMedia: media)
        call.speakerEnabled = media == .video
        ChatModel.shared.activeCall = call
        return uuid
    }

    func startOutgoingCall(callUUID: UUID) -> Bool {
        let m = ChatModel.shared
        if let call = m.activeCall, call.callkitUUID == callUUID {
            m.showCallView = true
            Task { await m.callCommand.processCommand(.capabilities(media: call.localMedia)) }
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

    func answerIncomingCall(invitation: RcvCallInvitation) {
        let m = ChatModel.shared
        m.callInvitations.removeValue(forKey: invitation.contact.id)
        let call = Call(
            direction: .incoming,
            contact: invitation.contact,
            callkitUUID: invitation.callkitUUID,
            callState: .invitationAccepted,
            localMedia: invitation.callType.media,
            sharedKey: invitation.sharedKey
        )
        call.speakerEnabled = invitation.callType.media == .video
        let useRelay = UserDefaults.standard.bool(forKey: DEFAULT_WEBRTC_POLICY_RELAY)
        let iceServers = getIceServers()
        logger.debug("answerIncomingCall useRelay: \(useRelay)")
        logger.debug("answerIncomingCall iceServers: \(String(describing: iceServers))")
        // When in active call user wants to accept another call, this can only work after delay (to hide and show activeCallView)
        DispatchQueue.main.asyncAfter(deadline: .now() + (m.activeCall == nil ? 0 : 1)) {
            m.activeCall = call
            m.showCallView = true

            Task {
                await m.callCommand.processCommand(.start(
                media: invitation.callType.media,
                aesKey: invitation.sharedKey,
                iceServers: iceServers,
                relay: useRelay
                ))
            }
        }
    }

    func enableMedia(media: CallMediaType, enable: Bool, callUUID: UUID) -> Bool {
        if let call = ChatModel.shared.activeCall, call.callkitUUID == callUUID {
            let m = ChatModel.shared
            Task { await m.callCommand.processCommand(.media(media: media, enable: enable)) }
            return true
        }
        return false
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
            m.activeCallViewIsCollapsed = false
            m.showCallView = false
            completed()
        } else {
            logger.debug("CallManager.endCall: ending call...")
            Task {
                await m.callCommand.processCommand(.end)
                await MainActor.run {
                    m.activeCall = nil
                    m.activeCallViewIsCollapsed = false
                    m.showCallView = false
                    completed()
                }
                do {
                    try await apiEndCall(call.contact)
                } catch {
                    logger.error("CallController.provider apiEndCall error: \(responseError(error))")
                }
            }
        }
    }

    func endCall(invitation: RcvCallInvitation, completed: @escaping () -> Void) {
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

    private func getCallInvitation(_ callUUID: UUID) -> RcvCallInvitation? {
        if let (_, invitation) = ChatModel.shared.callInvitations.first(where: { (_, inv) in inv.callkitUUID == callUUID }) {
            return invitation
        }
        return nil
    }
}
