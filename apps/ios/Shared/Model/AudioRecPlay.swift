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
        let av = AVAudioSession.sharedInstance()
        if !(await checkPermission()) { return .permission }
        do {
            try av.setCategory(AVAudioSession.Category.playAndRecord, options: .defaultToSpeaker)
            try av.setActive(true)
            let settings: [String : Any] = [
                AVFormatIDKey: kAudioFormatMPEG4AAC,
                AVSampleRateKey: 12000,
                AVEncoderBitRateKey: 12000,
                AVNumberOfChannelsKey: 1
            ]
            let url = getAppFilePath(fileName)
            NotificationCenter.default.post(name: .MediaStartedPlaying, object: nil, userInfo: ["url": url])
            addObserver(url)
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
        }
        audioRecorder = nil
        if let timer = recordingTimer {
            timer.invalidate()
        }
        recordingTimer = nil
        removeObserver()
    }

    private func checkPermission() async -> Bool {
        let av = AVAudioSession.sharedInstance()
        switch av.recordPermission {
        case .granted: return true
        case .denied: return false
        case .undetermined:
            return await withCheckedContinuation { cont in
                DispatchQueue.main.async {
                    av.requestRecordPermission { allowed in
                        cont.resume(returning: allowed)
                    }
                }
            }
        @unknown default: return false
        }
    }

    private func addObserver(_ url: URL) {
        NotificationCenter.default.addObserver(forName: .MediaStartedPlaying, object: nil, queue: .main) { ntf in
            self.stop()
            self.onFinishRecording?()
        }
    }

    private func removeObserver() {
        NotificationCenter.default.removeObserver(self)
    }
}

class AudioPlayer: NSObject, AVAudioPlayerDelegate {
    var onTimer: ((TimeInterval) -> Void)?
    var onFinishPlayback: (() -> Void)?

    var audioPlayer: AVAudioPlayer?
    var playbackTimer: Timer?

    init(onTimer: @escaping ((TimeInterval) -> Void), onFinishPlayback: @escaping (() -> Void)) {
        self.onTimer = onTimer
        self.onFinishPlayback = onFinishPlayback
    }

    func start(fileName: String) {
        let url = getAppFilePath(fileName)
        audioPlayer = try? AVAudioPlayer(contentsOf: url)
        audioPlayer?.delegate = self
        audioPlayer?.prepareToPlay()
        NotificationCenter.default.post(name: .MediaStartedPlaying, object: nil, userInfo: ["url": url])
        addObserver(url)
        audioPlayer?.play()

        playbackTimer = Timer.scheduledTimer(withTimeInterval: 0.01, repeats: true) { timer in
            if self.audioPlayer?.isPlaying ?? false {
                guard let time = self.audioPlayer?.currentTime else { return }
                self.onTimer?(time)
            }
        }
    }

    func pause() {
        audioPlayer?.pause()
    }

    func play() {
        audioPlayer?.play()
        if let url = audioPlayer?.url {
            NotificationCenter.default.post(name: .MediaStartedPlaying, object: nil, userInfo: ["url": url])
        }
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
        removeObserver()
    }

    func audioPlayerDidFinishPlaying(_ player: AVAudioPlayer, successfully flag: Bool) {
        stop()
        self.onFinishPlayback?()
    }

    private func addObserver(_ url: URL) {
        NotificationCenter.default.addObserver(forName: .MediaStartedPlaying, object: nil, queue: .main) { ntf in
            if let u = ntf.userInfo?.first { $0.key as? String == "url" }?.value as? URL, u != url {
                // Other player started to play
                self.stop()
                self.onFinishPlayback?()
            }
        }
    }

    private func removeObserver() {
        NotificationCenter.default.removeObserver(self)
    }
}
