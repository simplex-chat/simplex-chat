//
//  AudioRecording.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 19/11/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import AVFoundation

func startAudioRecording(url: URL) -> AVAudioRecorder? {
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
//        let recordingUrl = getAppFilePath(generateNewFileName("voice", "m4a"))
        let audioRecorder = try AVAudioRecorder(url: url, settings: settings)
//            audioRecorder.delegate = self
//            audioRecorder.isMeteringEnabled = true
        audioRecorder.prepareToRecord()
        audioRecorder.record()
        return audioRecorder
    } catch let error {
        logger.error("RecordVC setupRecorder error \(error.localizedDescription)")
        // display_alert(msg_title: "Error", msg_desc: error.localizedDescription, action_title: "OK")
        return nil
    }
}

func startAudioPlayback(url: URL) -> AVAudioPlayer? {
    if let player = try? AVAudioPlayer(contentsOf: url) {
        //    player.delegate = self
        player.prepareToPlay()
        player.play()
        return player
    }
    return nil
}
