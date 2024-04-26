//
//  CallAudioDeviceManager.swift
//  SimpleX (iOS)
//
//  Created by Avently on 23.04.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI
import SimpleXChat
import AVKit

class CallAudioDeviceManager {
    let audioSession: AVAudioSession
    let nc = NotificationCenter.default

    @State var devices: [AVAudioSessionPortDescription]
    @State var currentDevice: AVAudioSessionPortDescription? = nil
    @State var availableInputs: [AVAudioSessionPortDescription] = []


    init(_ audioSession: AVAudioSession? = nil) {
        self.audioSession = audioSession ?? AVAudioSession.sharedInstance()
        self.devices = self.audioSession.currentRoute.outputs
        self.availableInputs = self.audioSession.availableInputs ?? []
    }

    @objc func audioCallback(notification: Notification) {
        logger.debug("LALAL CALLED")
        guard let userInfo = notification.userInfo,
              let reasonValue = userInfo[AVAudioSessionRouteChangeReasonKey] as? UInt,
              let reason = AVAudioSession.RouteChangeReason(rawValue: reasonValue) else {
            return
        }

        switch reason {
        case .newDeviceAvailable:
            devices = audioSession.currentRoute.outputs
        case .oldDeviceUnavailable:
            devices = audioSession.currentRoute.outputs
        case .override:
            currentDevice = audioSession.currentRoute.outputs.first
        default: ()
        }
        self.availableInputs = audioSession.availableInputs ?? []
        logger.debug("LALAL inputs \(String(describing: self.availableInputs))")
    }

    func start() {
        logger.debug("LALAL START")
        nc.addObserver(self, selector: #selector(audioCallback), name: AVAudioSession.routeChangeNotification, object: nil)
    }

    func stop() {
        logger.debug("LALAL STOP")
        nc.removeObserver(self, name: AVAudioSession.routeChangeNotification, object: nil)
    }
}
