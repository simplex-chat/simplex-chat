//
//  AudioRecPlay.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 19/11/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import AVFoundation
import SwiftUI
import SimpleXChat

class AudioRecorder {
    var onTimer: ((TimeInterval) -> Void)?
    var onFinishRecording: (() -> Void)?

    var audioRecorder: AVAudioRecorder?
    var recordingTimer: Timer?

    init(onTimer: @escaping ((TimeInterval) -> Void), onFinishRecording: @escaping (() -> Void)) {
        self.onTimer = onTimer
        self.onFinishRecording = onFinishRecording
    }

    enum StartError {
        case permission
        case error(String)
    }

    func start(fileName: String) async -> StartError? {
        let audioSession = AVAudioSession.sharedInstance()
        if !(await checkPermission()) { return .permission }
        do {
            try audioSession.setCategory(AVAudioSession.Category.playAndRecord, options: .defaultToSpeaker)
            try audioSession.setActive(true)
            let settings: [String : Any] = [
                AVFormatIDKey: kAudioFormatMPEG4AAC,
                AVSampleRateKey: 12000,
                AVEncoderBitRateKey: 12000,
                AVNumberOfChannelsKey: 1
            ]
            let url = getAppFilePath(fileName)
            audioRecorder = try AVAudioRecorder(url: url, settings: settings)
            audioRecorder?.record(forDuration: MAX_VOICE_MESSAGE_LENGTH)

            await MainActor.run {
                recordingTimer = Timer.scheduledTimer(withTimeInterval: 0.01, repeats: true) { timer in
                    guard let time = self.audioRecorder?.currentTime else { return }
                    self.onTimer?(time)
                    if time >= MAX_VOICE_MESSAGE_LENGTH {
                        self.stop()
                        self.onFinishRecording?()
                    }
                }
            }
            return nil
        } catch let error {
            logger.error("AudioRecorder startAudioRecording error \(error.localizedDescription)")
            return .error(error.localizedDescription)
        }
    }

    func stop() {
        if let recorder = audioRecorder {
            recorder.stop()
            let audioSession = AVAudioSession.sharedInstance()
            try? audioSession.setActive(false, options: .notifyOthersOnDeactivation)
            try? audioSession.setCategory(AVAudioSession.Category.ambient, mode: .default)
            try? audioSession.setActive(true)
        }
        audioRecorder = nil
        if let timer = recordingTimer {
            timer.invalidate()
        }
        recordingTimer = nil
    }

    private func checkPermission() async -> Bool {
        let audioSession = AVAudioSession.sharedInstance()
        switch audioSession.recordPermission {
        case .granted: return true
        case .denied: return false
        case .undetermined:
            return await withCheckedContinuation { cont in
                DispatchQueue.main.async {
                    audioSession.requestRecordPermission { allowed in
                        cont.resume(returning: allowed)
                    }
                }
            }
        @unknown default: return false
        }
    }
}

class AudioPlayer: NSObject, AVAudioPlayerDelegate {
    var onTimer: ((TimeInterval) -> Void)?
    var onFinishPlayback: (() -> Void)?

    var audioPlayer: AVAudioPlayer?
    var playbackTimer: Timer?
    static let dropAudioSessionAfter = 3.0
    static var lastPlayed: TimeInterval = ProcessInfo.processInfo.systemUptime

    init(onTimer: @escaping ((TimeInterval) -> Void), onFinishPlayback: @escaping (() -> Void)) {
        self.onTimer = onTimer
        self.onFinishPlayback = onFinishPlayback
    }

    func start(fileName: String) {
        AudioPlayer.audioSessionActivatePlayAndRecord()

        let url = getAppFilePath(fileName)
        audioPlayer = try? AVAudioPlayer(contentsOf: url)
        audioPlayer?.delegate = self
        audioPlayer?.prepareToPlay()
        audioPlayer?.play()

        playbackTimer = Timer.scheduledTimer(withTimeInterval: 0.01, repeats: true) { timer in
            if self.audioPlayer?.isPlaying ?? false {
                AudioPlayer.updateLastPlayed()
                guard let time = self.audioPlayer?.currentTime else { return }
                self.onTimer?(time)
            }
        }
    }

    func pause() {
        audioPlayer?.pause()
        AudioPlayer.audioSessionToDefaultAfterDelay()
    }

    func play() {
        AudioPlayer.audioSessionActivatePlayAndRecord()
        audioPlayer?.play()
    }

    func stop() {
        if let player = audioPlayer {
            player.stop()
        }
        audioPlayer = nil
        if let timer = playbackTimer {
            timer.invalidate()
        }
        playbackTimer = nil
        AudioPlayer.audioSessionToDefaultAfterDelay()
    }

    func audioPlayerDidFinishPlaying(_ player: AVAudioPlayer, successfully flag: Bool) {
        stop()
        self.onFinishPlayback?()
    }

    static func updateLastPlayed() {
        AudioPlayer.lastPlayed = ProcessInfo.processInfo.systemUptime
    }

    static func audioSessionActivatePlayAndRecord() {
        let audioSession = AVAudioSession.sharedInstance()
        if audioSession.category != .playAndRecord {
            try? audioSession.setCategory(AVAudioSession.Category.playAndRecord, options: .defaultToSpeaker)
            try? audioSession.setActive(true)
        }
    }

    static func audioSessionToDefaultAfterDelay() {
        DispatchQueue.main.asyncAfter(deadline: .now() + AudioPlayer.dropAudioSessionAfter) {
            if ProcessInfo.processInfo.systemUptime - AudioPlayer.dropAudioSessionAfter + 1 > AudioPlayer.lastPlayed {
                self.audioSessionToDefault()
            }
        }
    }

    private static func audioSessionToDefault() {
        let audioSession = AVAudioSession.sharedInstance()
        if audioSession.category != .ambient {
            try? audioSession.setActive(false, options: .notifyOthersOnDeactivation)
            try? audioSession.setCategory(AVAudioSession.Category.ambient, mode: .default)
            try? audioSession.setActive(true)
        }
    }
}
