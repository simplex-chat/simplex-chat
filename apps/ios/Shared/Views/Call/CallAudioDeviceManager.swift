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
import WebRTC

class CallAudioDeviceManager: ObservableObject {
    static let shared = CallAudioDeviceManager()
    let audioSession: AVAudioSession
    let nc = NotificationCenter.default

    var call: Call?
    var timer: Timer? = nil

    // Actually, only one output
    @Published var outputs: [AVAudioSessionPortDescription]
    @Published var currentDevice: AVAudioSessionPortDescription? = nil
    // All devices that can record audio (the ones that can play audio are not included)
    @Published var availableInputs: [AVAudioSessionPortDescription] = []


    init(_ audioSession: AVAudioSession? = nil) {
        self.audioSession = audioSession ?? RTCAudioSession.sharedInstance().session
        self.outputs = self.audioSession.currentRoute.outputs
        self.availableInputs = self.audioSession.availableInputs ?? []
    }

    func reloadDevices() {
        outputs = audioSession.currentRoute.outputs
        currentDevice = audioSession.currentRoute.outputs.first
        availableInputs = audioSession.availableInputs ?? []
        call?.speakerEnabled = currentDevice?.portType == .builtInSpeaker


        // Workaround situation:
        // have bluetooth device connected, choosing speaker, disconnecting bluetooth device. In this case iOS will not post notification, so do it manually
        timer?.invalidate()
        if availableInputs.contains(where: { $0.portType != .builtInReceiver && $0.portType != .builtInSpeaker }) {
            timer = Timer.scheduledTimer(withTimeInterval: 2, repeats: false) { t in
                self.reloadDevices()
            }
        }
    }

    @objc func audioCallback(notification: Notification) {
        reloadDevices()

        logger.debug("Changes in devices, current audio devices: \(String(describing: self.availableInputs.map({ $0.portType.rawValue }))), output: \(String(describing: self.currentDevice?.portType.rawValue))")
    }

    func start() {
        nc.addObserver(self, selector: #selector(audioCallback), name: AVAudioSession.routeChangeNotification, object: nil)
    }

    func stop() {
        nc.removeObserver(self, name: AVAudioSession.routeChangeNotification, object: nil)
        timer?.invalidate()
    }
}
