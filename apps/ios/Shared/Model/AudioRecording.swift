//
//  AudioRecording.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 19/11/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import AVFoundation
import SwiftUI
import SimpleXChat

class AudioRecorder: NSObject, AVAudioRecorderDelegate {
    var onTimer: ((TimeInterval) -> Void)?
    var onFinishRecording: (() -> Void)?

    var audioRecorder: AVAudioRecorder?
    var recordingTimer: Timer?

    init(onTimer: @escaping ((TimeInterval) -> Void), onFinishRecording: @escaping (() -> Void)) {
        self.onTimer = onTimer
        self.onFinishRecording = onFinishRecording
    }

    func start(fileName: String) {
        let session = AVAudioSession.sharedInstance()
        do {
            try session.setCategory(AVAudioSession.Category.playAndRecord, options: .defaultToSpeaker)
            try session.setActive(true)
            let settings = [
                AVFormatIDKey: Int(kAudioFormatMPEG4AAC),
                AVSampleRateKey: 44100,
                AVNumberOfChannelsKey: 2,
                AVEncoderAudioQualityKey:AVAudioQuality.high.rawValue
            ]
            audioRecorder = try AVAudioRecorder(url: getAppFilePath(fileName), settings: settings)
            audioRecorder?.delegate = self
            audioRecorder?.record(forDuration: 10)

            recordingTimer = Timer.scheduledTimer(withTimeInterval: 0.2, repeats: true) { timer in
                logger.debug("recording timer")
                guard let time = self.audioRecorder?.currentTime else { return }
                self.onTimer?(time)
            }
        } catch let error {
            logger.error("AudioRecorder startAudioRecording error \(error.localizedDescription)")
            // alert
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
    }

    func audioRecorderDidFinishRecording(_ recorder: AVAudioRecorder, successfully flag: Bool) {
        stop()
        self.onFinishRecording?()
    }
}

func checkRecordPermission(granted: Binding<Bool>) {
    switch AVAudioSession.sharedInstance().recordPermission {
    case AVAudioSession.RecordPermission.granted:
        granted.wrappedValue = true
        break
    case AVAudioSession.RecordPermission.denied:
        granted.wrappedValue = false
        break
    case AVAudioSession.RecordPermission.undetermined:
        AVAudioSession.sharedInstance().requestRecordPermission({ allowed in
            granted.wrappedValue = allowed
        })
        break
    default:
        break
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
        audioPlayer = try? AVAudioPlayer(contentsOf: getAppFilePath(fileName))
        audioPlayer?.delegate = self
        audioPlayer?.prepareToPlay()
        audioPlayer?.play()

        playbackTimer = Timer.scheduledTimer(withTimeInterval: 0.2, repeats: true) { timer in
            logger.debug("playback timer")
            guard let time = self.audioPlayer?.currentTime else { return }
            self.onTimer?(time)
        }
    }

    func pause() {
        audioPlayer?.pause()
    }

    func play() {
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
    }

    func audioPlayerDidFinishPlaying(_ player: AVAudioPlayer, successfully flag: Bool) {
        stop()
        self.onFinishPlayback?()
    }
}
