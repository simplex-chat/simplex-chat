//
//  SoundPlayer.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 24/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import AVFoundation

class SoundPlayer {
    static let shared = SoundPlayer()
    private var audioPlayer: AVAudioPlayer?

    func startRingtone() {
        audioPlayer?.stop()
        logger.debug("startRingtone")
        guard let path = Bundle.main.path(forResource: "ringtone2", ofType: "m4a", inDirectory: "sounds") else {
            logger.debug("startRingtone: file not found")
            return
        }
        do {
            let player = try AVAudioPlayer(contentsOf: URL(fileURLWithPath: path))
            if player.prepareToPlay() {
                audioPlayer = player
            }
        } catch {
            logger.debug("startRingtone: AVAudioPlayer error \(error.localizedDescription)")
        }

        Task {
            while let player = audioPlayer {
                player.play()
                AudioServicesPlayAlertSound(kSystemSoundID_Vibrate)
                _ = try? await Task.sleep(nanoseconds: UInt64(player.duration * 1_000_000_000))
            }
        }
    }

    func stopRingtone() {
        audioPlayer?.stop()
        audioPlayer = nil
    }
}
