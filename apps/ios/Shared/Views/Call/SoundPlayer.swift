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
    private var playing: SystemSound?

    enum SystemSound {
        case ringing

        var systemId: SystemSoundID {
            switch self {
            case .ringing: return 1151
            }
        }
    }

    func startSound(_ sound: SystemSound, delay: UInt64 = 3) {
        playing = sound
        Task {
            while playing == sound {
                AudioServicesPlaySystemSound(sound.systemId)
                do {
                    try await Task.sleep(nanoseconds: delay * 1_000_000_000)
                } catch {
                    logger.error("startSound: Task.sleep error: \(error.localizedDescription)")
                    playing = nil
                }
            }
        }
    }

    func stopSound() {
        playing = nil
    }
}
