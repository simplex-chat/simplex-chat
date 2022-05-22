//
//  CallProvider.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 21/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import CallKit
import AVFoundation

class CallController: NSObject, CXProviderDelegate {
    static let shared = CallController()
    let provider = CXProvider(configuration: CallController.configuration)
    let controller = CXCallController()

    static let configuration: CXProviderConfiguration = {
        let configuration = CXProviderConfiguration()
        configuration.supportsVideo = true
        configuration.includesCallsInRecents = true // TODO disable or add option
        configuration.maximumCallsPerCallGroup = 1
        return configuration
    }()

    override init() {
        super.init()
        self.provider.setDelegate(self, queue: nil)
    }

    func providerDidReset(_ provider: CXProvider) {
    }

    func provider(_ provider: CXProvider, perform action: CXStartCallAction) {
        logger.debug("CallController.provider CXStartCallAction")
        if let call = ChatModel.shared.activeCall {
            action.fulfill()
            provider.reportOutgoingCall(with: call.callkitUuid, connectedAt: nil)
        } else {
            action.fail()
        }
    }

    func provider(_ provider: CXProvider, perform action: CXAnswerCallAction) {
//        let m = ChatModel.shared
//        if m.activeCall != nil {
//            m.activeCall = nil
//            action.fulfill()
//        } else {
//            action.fail()
//        }
    }

    func provider(_ provider: CXProvider, perform action: CXEndCallAction) {
        action.fulfill()
    }

    func provider(_ provider: CXProvider, timedOutPerforming action: CXAction) {
        print("timed out", #function)
    }

    func provider(_ provider: CXProvider, didActivate audioSession: AVAudioSession) {
        print("received", #function)
        do {
//            try audioSession.setCategory(.playAndRecord, mode: .voiceChat, options: .mixWithOthers)
//            logger.debug("audioSession category set")
            try audioSession.setActive(false)
            logger.debug("audioSession deactivated")
        } catch {
            print(error)
            logger.error("failed activating audio session")
        }
    }

    func provider(_ provider: CXProvider, didDeactivate audioSession: AVAudioSession) {
        print("received", #function)
    }


    func startCall(_ call: Call) {
        logger.debug("CallController.startCall")
        let handle = CXHandle(type: .phoneNumber, value: call.contact.displayName)
        let startCallAction = CXStartCallAction(call: call.callkitUuid, handle: handle)
        startCallAction.isVideo = call.localMedia == .video
        let transaction = CXTransaction()
        transaction.addAction(startCallAction)
        requestTransaction(transaction)
    }

    func endCall(_ call: Call) {
        let transaction = CXTransaction()
        transaction.addAction(CXEndCallAction(call: call.callkitUuid))
        requestTransaction(transaction)
    }

    private func requestTransaction(_ t: CXTransaction) {
        controller.request(t) { error in
            if let error = error {
                logger.error("CallController.requestTransaction error requesting transaction: \(error.localizedDescription)")
            } else {
                logger.debug("CallController.requestTransaction requested transaction successfully")
            }
        }
    }
}
