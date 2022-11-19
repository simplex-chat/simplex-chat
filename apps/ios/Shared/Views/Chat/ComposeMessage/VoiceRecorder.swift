//
//  VoiceRecorder.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 19/11/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat
import AVFoundation
import BackgroundTasks

enum RecordingState {
    case new
    case recording
    case finished
    case playing
    case paused
}

struct VoiceRecorder: View {
    @State var isAudioRecordingGranted: Bool = false
    @State var audioRecorder: AudioRecorder?
    @State var audioPlayer: AudioPlayer?
    @State var recordingFileName: String?
    @State var recordingState = RecordingState.new
    @State var recordingTime = "00:00"

    var body: some View {
        switch recordingState {
        case .new:
            Button("record") {
                if isAudioRecordingGranted {
                    record()
                } else {
                    checkRecordPermission(granted: $isAudioRecordingGranted)
                }
            }
            .onChange(of: isAudioRecordingGranted, perform: { granted in
                if granted {
                    record()
                }
            })
        case .recording:
            Button("finish") {
                audioRecorder?.stopAudioRecording()
                audioRecorder = nil
                recordingState = .finished
            }
            Text(recordingTime)
            cancelRecordingButton()
        case .finished:
            Button("play") {
                playback()
            }
            Text(recordingTime)
            cancelRecordingButton()
        case .playing:
            Button("pause") {
                audioPlayer?.pauseAudioPlayback()
                recordingState = .paused
            }
            Text(recordingTime)
            cancelRecordingButton()
        case .paused:
            Button("play") {
                audioPlayer?.playAudioPlayback()
                recordingState = .playing
            }
            Text(recordingTime)
            cancelRecordingButton()
        }
    }

    private func record() {
        let fileName = generateNewFileName("voice", "m4a")
        recordingFileName = fileName
        audioRecorder = AudioRecorder(
            onTimer: { displayTimer($0) },
            onFinishRecording: { recordingState = .finished }
        )
        audioRecorder?.startAudioRecording(fileName: fileName)
        recordingState = .recording
    }

    private func cancelRecordingButton() -> some View {
        Button("cancel") {
            cancelRecording()
        }
    }

    private func cancelRecording() {
        if let fileName = recordingFileName {
            print(1)
            audioRecorder?.stopAudioRecording()
            print(2)
            audioPlayer?.stopAudioPlayback()
            print(3)
            removeFile(fileName)
            print(4)
            recordingState = .new
            print(5)
            recordingTime = "00:00"
        }
    }

    private func playback() {
        if let fileName = recordingFileName {
            audioPlayer = AudioPlayer(
                onTimer: { displayTimer($0) },
                onFinishPlayback: { recordingState = .finished }
            )
            audioPlayer?.startAudioPlayback(fileName: fileName)
            recordingState = .playing
        }
    }

    private func displayTimer(_ time: TimeInterval) {
        let min = Int(time / 60)
        let sec = Int(time.truncatingRemainder(dividingBy: 60))
        DispatchQueue.main.async {
            recordingTime = String(format: "%02d:%02d", min, sec)
        }
    }
}

struct VoiceRecorder_Previews: PreviewProvider {
    static var previews: some View {
        VoiceRecorder()
    }
}
