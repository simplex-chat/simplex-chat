//
//  AudioRecording.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 19/11/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import AVFoundation
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

    func startAudioRecording(fileName: String) {
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

    func stopAudioRecording() {
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
        stopAudioRecording()
        self.onFinishRecording?()
    }
}

func startAudioPlayback(fileName: String) -> AVAudioPlayer? {
    if let player = try? AVAudioPlayer(contentsOf: getAppFilePath(fileName)) {
        //    player.delegate = self
        player.prepareToPlay()
        player.play()
        return player
    }
    return nil
}
