//
//  CallController.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 21/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import CallKit
import StoreKit
import PushKit
import AVFoundation
import SimpleXChat
import WebRTC

class CallController: NSObject, CXProviderDelegate, PKPushRegistryDelegate, ObservableObject {
    static let shared = CallController()
    static let isInChina = SKStorefront().countryCode == "CHN"
    static func useCallKit() -> Bool { !isInChina && callKitEnabledGroupDefault.get() }

    private let provider = CXProvider(configuration: {
        let configuration = CXProviderConfiguration()
        configuration.supportsVideo = true
        configuration.supportedHandleTypes = [.generic]
        configuration.includesCallsInRecents = false // UserDefaults.standard.bool(forKey: DEFAULT_CALL_KIT_CALLS_IN_RECENTS)
        configuration.maximumCallGroups = 1
        configuration.maximumCallsPerCallGroup = 1
        configuration.iconTemplateImageData = UIImage(named: "icon-transparent")?.pngData()
        return configuration
    }())
    private let controller = CXCallController()
    private let callManager = CallManager()
    @Published var activeCallInvitation: RcvCallInvitation?
    var onEndCall: (() -> Void)? = nil
    var fulfillOnConnect: CXAnswerCallAction? = nil

    // PKPushRegistry is used from notification service extension
    private let registry = PKPushRegistry(queue: nil)

    override init() {
        super.init()
        provider.setDelegate(self, queue: nil)
        registry.delegate = self
        registry.desiredPushTypes = [.voIP]
    }

    func providerDidReset(_ provider: CXProvider) {
    }

    func provider(_ provider: CXProvider, perform action: CXStartCallAction) {
        logger.debug("CallController.provider CXStartCallAction")
        if callManager.startOutgoingCall(callUUID: action.callUUID) {
            action.fulfill()
            provider.reportOutgoingCall(with: action.callUUID, startedConnectingAt: nil)
        } else {
            action.fail()
        }
    }

    func provider(_ provider: CXProvider, perform action: CXAnswerCallAction) {
        logger.debug("CallController.provider CXAnswerCallAction")
        dismissAllSheets {
            if self.callManager.answerIncomingCall(callUUID: action.callUUID) {
                // WebRTC call should be in connected state to fulfill.
                // Otherwise no audio and mic working on lockscreen
                self.fulfillOnConnect = action
            } else {
                action.fail()
            }
        }
    }

    func provider(_ provider: CXProvider, perform action: CXEndCallAction) {
        logger.debug("CallController.provider CXEndCallAction")
        // Should be nil here if connection was in connected state
        fulfillOnConnect?.fail()
        fulfillOnConnect = nil
        callManager.endCall(callUUID: action.callUUID) { ok in
            if ok {
                action.fulfill()
            } else {
                action.fail()
            }
        }
    }

    func provider(_ provider: CXProvider, perform action: CXSetMutedCallAction) {
        if callManager.enableMedia(media: .audio, enable: !action.isMuted, callUUID:  action.callUUID) {
            action.fulfill()
        } else {
            action.fail()
        }
    }

    func provider(_ provider: CXProvider, timedOutPerforming action: CXAction) {
        logger.debug("timed out: \(String(describing: action))")
        action.fulfill()
    }

    func provider(_ provider: CXProvider, didActivate audioSession: AVAudioSession) {
        logger.debug("CallController: activating audioSession and audio in WebRTCClient")
        RTCAudioSession.sharedInstance().audioSessionDidActivate(audioSession)
        RTCAudioSession.sharedInstance().isAudioEnabled = true
        do {
            try audioSession.setCategory(.playAndRecord, mode: .voiceChat, options: .mixWithOthers)
            logger.debug("audioSession category set")
            try audioSession.setActive(true)
            logger.debug("audioSession activated")
        } catch {
            print(error)
            logger.error("failed activating audio session")
        }
    }

    func provider(_ provider: CXProvider, didDeactivate audioSession: AVAudioSession) {
        logger.debug("CallController: deactivating audioSession and audio in WebRTCClient")
        RTCAudioSession.sharedInstance().audioSessionDidDeactivate(audioSession)
        RTCAudioSession.sharedInstance().isAudioEnabled = false
        do {
            try audioSession.setActive(false)
            logger.debug("audioSession deactivated")
        } catch {
            print(error)
            logger.error("failed deactivating audio session")
        }
        // Allows to accept second call while in call with a previous before suspending a chat,
        // see `.onChange(of: scenePhase)` in SimpleXApp
        DispatchQueue.main.asyncAfter(deadline: .now() + 3) { [weak self] in
            if ChatModel.shared.activeCall == nil {
                logger.debug("CallController: calling callback onEndCall which is \(self?.onEndCall == nil ? "nil" : "non-nil", privacy: .public)")
                self?.onEndCall?()
            }
        }
    }

    @objc(pushRegistry:didUpdatePushCredentials:forType:)
    func pushRegistry(_ registry: PKPushRegistry, didUpdate pushCredentials: PKPushCredentials, for type: PKPushType) {

    }

    func pushRegistry(_ registry: PKPushRegistry, didReceiveIncomingPushWith payload: PKPushPayload, for type: PKPushType, completion: @escaping () -> Void) {
        logger.debug("CallController: did receive push with type \(type.rawValue, privacy: .public)")
        if type == .voIP {
            if (!ChatModel.shared.chatInitialized) {
                logger.debug("CallController: initializing chat and returning")
                initChatAndMigrate(refreshInvitations: false)
                startChatAndActivate()
                CallController.shared.onEndCall = { terminateChat() }
            } else {
                logger.debug("CallController: starting chat (already initialized)")
                startChatAndActivate()
                CallController.shared.onEndCall = {
                    suspendChat()
                    BGManager.shared.schedule()
                }
            }
            // No actual list of invitations in model before this line
            let invitations = try? justRefreshCallInvitations()
            logger.debug("Invitations \(String(describing: invitations))")
            // Extract the call information from the push notification payload
            if let displayName = payload.dictionaryPayload["displayName"] as? String,
               let contactId = payload.dictionaryPayload["contactId"] as? String,
               let uuid = ChatModel.shared.callInvitations.first(where: { (key, value) in value.contact.id == contactId } )?.value.callkitUUID,
               let media = payload.dictionaryPayload["media"] as? String {
                let callUpdate = CXCallUpdate()
                callUpdate.remoteHandle = CXHandle(type: .generic, value: contactId)
                callUpdate.localizedCallerName = displayName
                callUpdate.hasVideo = media == CallMediaType.video.rawValue
                logger.debug("CallController: reporting incoming call directly to CallKit")
                CallController.shared.provider.reportNewIncomingCall(with: uuid, update: callUpdate, completion: { error in
                    if error != nil {
                        ChatModel.shared.callInvitations.removeValue(forKey: contactId)
                    }
                    // Tell PushKit that the notification is handled.
                    completion()
                })
            } else {
                reportFakeCall(completion)
            }
        }
    }

    private func reportFakeCall(_ completion: @escaping () -> Void) {
        let uuid = UUID()
        let callUpdate = CXCallUpdate()
        CallController.shared.provider.reportNewIncomingCall(with: uuid, update: callUpdate, completion: { error in
            completion()
            DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                CallController.shared.provider.reportCall(with: uuid, endedAt: nil, reason: .remoteEnded)
            }
        })
    }

    func reportNewIncomingCall(invitation: RcvCallInvitation, completion: @escaping (Error?) -> Void) {
        logger.debug("CallController.reportNewIncomingCall, UUID=\(String(describing: invitation.callkitUUID), privacy: .public)")
        if CallController.useCallKit(), let uuid = invitation.callkitUUID {
            let update = CXCallUpdate()
            update.remoteHandle = CXHandle(type: .generic, value: invitation.contact.id)
            update.hasVideo = invitation.callType.media == .video
            update.localizedCallerName = invitation.contact.displayName
            provider.reportNewIncomingCall(with: uuid, update: update, completion: completion)
        } else {
            NtfManager.shared.notifyCallInvitation(invitation)
            if invitation.callTs.timeIntervalSinceNow >= -180 {
                activeCallInvitation = invitation
            }
        }
    }

    func reportIncomingCall(call: Call, connectedAt dateConnected: Date?) {
        logger.debug("CallController: reporting incoming call connected")
        if CallController.useCallKit() {
            // Fulfilling this action only after connect, otherwise there are no audio and mic on lockscreen
            fulfillOnConnect?.fulfill()
            fulfillOnConnect = nil
        }
    }

    func reportOutgoingCall(call: Call, connectedAt dateConnected: Date?) {
        logger.debug("CallController: reporting outgoing call connected")
        if CallController.useCallKit(), let uuid = call.callkitUUID {
            provider.reportOutgoingCall(with: uuid, connectedAt: dateConnected)
        }
    }

    func reportCallRemoteEnded(invitation: RcvCallInvitation) {
        logger.debug("CallController: reporting remote ended")
        if CallController.useCallKit(), let uuid = invitation.callkitUUID {
            provider.reportCall(with: uuid, endedAt: nil, reason: .remoteEnded)
        } else if invitation.contact.id == activeCallInvitation?.contact.id {
            activeCallInvitation = nil
        }
    }

    func reportCallRemoteEnded(call: Call) {
        logger.debug("CallController: reporting remote ended")
        if CallController.useCallKit(), let uuid = call.callkitUUID {
            provider.reportCall(with: uuid, endedAt: nil, reason: .remoteEnded)
        }
    }

    func startCall(_ contact: Contact, _ media: CallMediaType) {
        logger.debug("CallController.startCall")
        let uuid = callManager.newOutgoingCall(contact, media)
        if CallController.useCallKit() {
            let handle = CXHandle(type: .generic, value: contact.id)
            let action = CXStartCallAction(call: uuid, handle: handle)
            action.isVideo = media == .video
            requestTransaction(with: action) {
                let update = CXCallUpdate()
                update.remoteHandle = CXHandle(type: .generic, value: contact.id)
                update.hasVideo = media == .video
                update.localizedCallerName = contact.displayName
                self.provider.reportCall(with: uuid, updated: update)
            }
        } else if callManager.startOutgoingCall(callUUID: uuid) {
            if callManager.startOutgoingCall(callUUID: uuid) {
                logger.debug("CallController.startCall: call started")
            } else {
                logger.error("CallController.startCall: no active call")
            }
        }
    }

    func answerCall(invitation: RcvCallInvitation) {
        logger.debug("CallController: answering a call")
        if CallController.useCallKit(), let callUUID = invitation.callkitUUID {
            requestTransaction(with: CXAnswerCallAction(call: callUUID))
        } else {
            callManager.answerIncomingCall(invitation: invitation)
        }
        if invitation.contact.id == self.activeCallInvitation?.contact.id {
            self.activeCallInvitation = nil
        }
    }

    func endCall(callUUID: UUID) {
        logger.debug("CallController: ending the call with UUID \(callUUID.uuidString)")
        if CallController.useCallKit() {
            requestTransaction(with: CXEndCallAction(call: callUUID))
        } else {
            callManager.endCall(callUUID: callUUID) { ok in
                if ok {
                    logger.debug("CallController.endCall: call ended")
                } else {
                    logger.error("CallController.endCall: no active call pr call invitation to end")
                }
            }
        }
    }

    func endCall(invitation: RcvCallInvitation) {
        logger.debug("CallController: ending the call with invitation")
        callManager.endCall(invitation: invitation) {
            if invitation.contact.id == self.activeCallInvitation?.contact.id {
                DispatchQueue.main.async {
                    self.activeCallInvitation = nil
                }
            }
        }
    }

    func endCall(call: Call, completed: @escaping () -> Void) {
        logger.debug("CallController: ending the call with call instance")
        callManager.endCall(call: call, completed: completed)
    }

    func callAction(invitation: RcvCallInvitation, action: NtfCallAction) {
        switch action {
        case .accept: answerCall(invitation: invitation)
        case .reject: endCall(invitation: invitation)
        }
    }

    func showInRecents(_ show: Bool) {
        let conf = provider.configuration
        conf.includesCallsInRecents = show
        provider.configuration = conf
    }

    func hasActiveCalls() -> Bool {
        controller.callObserver.calls.count > 0
    }

    private func requestTransaction(with action: CXAction, onSuccess: @escaping () -> Void = {}) {
        controller.request(CXTransaction(action: action)) { error in
            if let error = error {
                logger.error("CallController.requestTransaction error requesting transaction: \(error.localizedDescription, privacy: .public)")
            } else {
                logger.debug("CallController.requestTransaction requested transaction successfully")
                onSuccess()
            }
        }
    }
}
