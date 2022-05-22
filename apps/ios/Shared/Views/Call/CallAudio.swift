/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Provides call audio management helper functions.
*/

import Foundation

private var audioController: AudioController?

func configureAudioSession() {
    print("Configuring audio session")

    if audioController == nil {
        audioController = AudioController()
    }
}

func startAudio() {
    print("Starting audio")

    if audioController?.startIOUnit() == kAudioServicesNoError {
        audioController?.muteAudio = false
    } else {
        // Handle error.
    }
}

func stopAudio() {
    print("Stopping audio")

    if audioController?.stopIOUnit() != kAudioServicesNoError {
        // Handle error.
    }
}
