//
//  CallController.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 21/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
//import CallKit
import AVFoundation
import SimpleXChat

//class CallController: NSObject, CXProviderDelegate, ObservableObject {
class CallController: NSObject, ObservableObject {
    static let useCallKit = false
    static let shared = CallController()
//    private let provider = CXProvider(configuration: CallController.configuration)
//    private let controller = CXCallController()
    private let callManager = CallManager()
    @Published var activeCallInvitation: RcvCallInvitation?

// PKPushRegistry will be used from notification service extension
//    let registry = PKPushRegistry(queue: nil)

//    static let configuration: CXProviderConfiguration = {
//        let configuration = CXProviderConfiguration()
//        configuration.supportsVideo = true
//        configuration.supportedHandleTypes = [.generic]
//        configuration.includesCallsInRecents = true // TODO disable or add option
//        configuration.maximumCallsPerCallGroup = 1
//        return configuration
//    }()

    override init() {
        super.init()
//        self.provider.setDelegate(self, queue: nil)
//        self.registry.delegate = self
//        self.registry.desiredPushTypes = [.voIP]
    }

//    func providerDidReset(_ provider: CXProvider) {
//    }

//    func provider(_ provider: CXProvider, perform action: CXStartCallAction) {
//        logger.debug("CallController.provider CXStartCallAction")
//        if callManager.startOutgoingCall(callUUID: action.callUUID) {
//            action.fulfill()
//            provider.reportOutgoingCall(with: action.callUUID, startedConnectingAt: nil)
//        } else {
//            action.fail()
//        }
//    }

//    func provider(_ provider: CXProvider, perform action: CXAnswerCallAction) {
//        logger.debug("CallController.provider CXAnswerCallAction")
//        if callManager.answerIncomingCall(callUUID: action.callUUID) {
//            action.fulfill()
//        } else {
//            action.fail()
//        }
//    }

//    func provider(_ provider: CXProvider, perform action: CXEndCallAction) {
//        logger.debug("CallController.provider CXEndCallAction")
//        callManager.endCall(callUUID: action.callUUID) { ok in
//            if ok {
//                action.fulfill()
//            } else {
//                action.fail()
//            }
//        }
//    }

//    func provider(_ provider: CXProvider, timedOutPerforming action: CXAction) {
//        print("timed out", #function)
//        action.fulfill()
//    }

//    func provider(_ provider: CXProvider, didActivate audioSession: AVAudioSession) {
//        print("received", #function)
////        do {
////            try audioSession.setCategory(.playAndRecord, mode: .voiceChat, options: .mixWithOthers)
////            logger.debug("audioSession category set")
////            try audioSession.setActive(true)
////            logger.debug("audioSession activated")
////        } catch {
////            print(error)
////            logger.error("failed activating audio session")
////        }
//    }

//    func provider(_ provider: CXProvider, didDeactivate audioSession: AVAudioSession) {
//        print("received", #function)
//    }

//    func pushRegistry(_ registry: PKPushRegistry, didUpdate pushCredentials: PKPushCredentials, for type: PKPushType) {
//
//    }

// This will be needed when we have notification service extension
//    func pushRegistry(_ registry: PKPushRegistry, didReceiveIncomingPushWith payload: PKPushPayload, for type: PKPushType, completion: @escaping () -> Void) {
//        if type == .voIP {
//           // Extract the call information from the push notification payload
//            if let displayName = payload.dictionaryPayload["displayName"] as? String,
//               let contactId = payload.dictionaryPayload["contactId"] as? String,
//               let uuidStr = payload.dictionaryPayload["uuid"] as? String,
//               let uuid = UUID(uuidString: uuidStr) {
//                let callUpdate = CXCallUpdate()
//                callUpdate.remoteHandle = CXHandle(type: .phoneNumber, value: displayName)
//                provider.reportNewIncomingCall(with: uuid, update: callUpdate, completion: { error in
//                    if error != nil {
//                        let m = ChatModel.shared
//                        m.callInvitations.removeValue(forKey: contactId)
//                    }
//                    // Tell PushKit that the notification is handled.
//                    completion()
//                })
//            }
//        }
//    }

    func reportNewIncomingCall(invitation: RcvCallInvitation, completion: @escaping (Error?) -> Void) {
        logger.debug("CallController.reportNewIncomingCall")
//        if CallController.useCallKit, let uuid = invitation.callkitUUID {
//            let update = CXCallUpdate()
//            update.remoteHandle = CXHandle(type: .generic, value: invitation.contact.displayName)
//            update.hasVideo = invitation.peerMedia == .video
//            provider.reportNewIncomingCall(with: uuid, update: update, completion: completion)
//        } else {
            NtfManager.shared.notifyCallInvitation(invitation)
            if invitation.callTs.timeIntervalSinceNow >= -180 {
                activeCallInvitation = invitation
            }
//        }
    }

//    func reportOutgoingCall(call: Call, connectedAt dateConnected: Date?) {
//        if CallController.useCallKit, let uuid = call.callkitUUID {
//            provider.reportOutgoingCall(with: uuid, connectedAt: dateConnected)
//        }
//    }

    func reportCallRemoteEnded(invitation: RcvCallInvitation) {
//        if CallController.useCallKit, let uuid = invitation.callkitUUID {
//            provider.reportCall(with: uuid, endedAt: nil, reason: .remoteEnded)
//        } else if invitation.contact.id == activeCallInvitation?.contact.id {
            activeCallInvitation = nil
//        }
    }

//    func reportCallRemoteEnded(call: Call) {
//        if CallController.useCallKit, let uuid = call.callkitUUID {
//            provider.reportCall(with: uuid, endedAt: nil, reason: .remoteEnded)
//        }
//    }

    func startCall(_ contact: Contact, _ media: CallMediaType) {
        logger.debug("CallController.startCall")
        let uuid = callManager.newOutgoingCall(contact, media)
//        if CallController.useCallKit {
//            let handle = CXHandle(type: .generic, value: contact.displayName)
//            let action = CXStartCallAction(call: uuid, handle: handle)
//            action.isVideo = media == .video
//            requestTransaction(with: action)
//        } else if callManager.startOutgoingCall(callUUID: uuid) {
        if callManager.startOutgoingCall(callUUID: uuid) {
            logger.debug("CallController.startCall: call started")
        } else {
            logger.error("CallController.startCall: no active call")
        }
    }

    func answerCall(invitation: RcvCallInvitation) {
        callManager.answerIncomingCall(invitation: invitation)
        if invitation.contact.id == self.activeCallInvitation?.contact.id {
            self.activeCallInvitation = nil
        }
    }

    func endCall(callUUID: UUID) {
//        if CallController.useCallKit {
//            requestTransaction(with: CXEndCallAction(call: callUUID))
//        } else {
            callManager.endCall(callUUID: callUUID) { ok in
                if ok {
                    logger.debug("CallController.endCall: call ended")
                } else {
                    logger.error("CallController.endCall: no actove call pr call invitation to end")
                }
            }
//        }
    }

    func endCall(invitation: RcvCallInvitation) {
        callManager.endCall(invitation: invitation) {
            if invitation.contact.id == self.activeCallInvitation?.contact.id {
                DispatchQueue.main.async {
                    self.activeCallInvitation = nil
                }
            }
        }
    }

    func endCall(call: Call, completed: @escaping () -> Void) {
        callManager.endCall(call: call, completed: completed)
    }

//    private func requestTransaction(with action: CXAction) {
//        let t = CXTransaction()
//        t.addAction(action)
//        controller.request(t) { error in
//            if let error = error {
//                logger.error("CallController.requestTransaction error requesting transaction: \(error.localizedDescription)")
//            } else {
//                logger.debug("CallController.requestTransaction requested transaction successfully")
//            }
//        }
//    }
}
