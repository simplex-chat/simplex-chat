//
//  SoundPlayer.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 24/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import AVFoundation
import UIKit

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

class CallSoundsPlayer {
    static let shared = CallSoundsPlayer()
    private var audioPlayer: AVAudioPlayer?
    private var playerTask: Task = Task {}

    private func start(_ soundName: String, delayMs: Double) {
        audioPlayer?.stop()
        playerTask.cancel()
        logger.debug("start \(soundName)")
        guard let path = Bundle.main.path(forResource: soundName, ofType: "mp3", inDirectory: "sounds") else {
            logger.debug("start: file not found")
            return
        }
        do {
            let player = try AVAudioPlayer(contentsOf: URL(fileURLWithPath: path))
            if player.prepareToPlay() {
                audioPlayer = player
            }
        } catch {
            logger.debug("start: AVAudioPlayer error \(error.localizedDescription)")
        }

        playerTask = Task {
            while let player = audioPlayer {
                player.play()
                do {
                    try await Task.sleep(nanoseconds: UInt64((player.duration * 1_000_000_000) + delayMs * 1_000_000))
                } catch {
                    break
                }
            }
        }
    }

    func startConnectingCallSound() {
        start("connecting_call", delayMs: 0)
    }

    func startInCallSound() {
        // Taken from https://github.com/TelegramOrg/Telegram-Android
        // https://github.com/TelegramOrg/Telegram-Android/blob/master/LICENSE
        start("in_call", delayMs: 1000)
    }

    func stop() {
        playerTask.cancel()
        audioPlayer?.stop()
        audioPlayer = nil
    }

    func vibrate(long: Bool) {
        // iOS just don't want to vibrate more than once after a short period of time, and all 'styles' feel the same
        if long {
            AudioServicesPlayAlertSound(kSystemSoundID_Vibrate)
        } else {
            UIImpactFeedbackGenerator(style: .heavy).impactOccurred()
        }
    }
}
