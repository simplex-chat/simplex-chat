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
    case stopped
    case playing
    case paused
}

struct VoiceRecorder: View {
    @State var audioRecorder: AudioRecorder?
    @State var audioPlayer: AVAudioPlayer?
    @State var recordingTime = ""
    @State var recordingState = RecordingState.new
    @State var recordingFileName: String?

    var body: some View {
        switch recordingState {
        case .new:
            Button("record") {
                let fileName = generateNewFileName("voice", "m4a")
                recordingFileName = fileName
                audioRecorder = AudioRecorder(
                    onTimer: {time in
                        let min = Int(time / 60)
                        let sec = Int(time.truncatingRemainder(dividingBy: 60))
                        DispatchQueue.main.async {
                            recordingTime = String(format: "%02d:%02d", min, sec)
                        }
                    },
                    onFinishRecording: {
                        recordingState = .stopped
                    }
                )
                audioRecorder?.startAudioRecording(fileName: fileName)
                recordingState = .recording
            }
        case .recording:
            Button("stop") {
                audioRecorder?.stopAudioRecording()
                audioRecorder = nil
                recordingState = .stopped
            }
//            Button("delete") {}
            Text(recordingTime)
        case .stopped:
            Button("play") {
                if let fileName = recordingFileName {
                    audioPlayer = startAudioPlayback(fileName: fileName)
                    recordingState = .playing
                }
            }
            Text(recordingTime)
            Button("delete") {
                if let fileName = recordingFileName {
                    removeFile(fileName)
                }
            }
        case .playing:
            Button("pause") {
                audioPlayer?.pause()
                recordingState = .paused
            }
            Text(recordingTime)
            //            Button("delete") {}
        case .paused:
            Button("play") {
                audioPlayer?.play()
                recordingState = .playing
            }
            Button("delete") {
                if let fileName = recordingFileName {
                    removeFile(fileName)
                }
            }
        }
    }
}

struct VoiceRecorder_Previews: PreviewProvider {
    static var previews: some View {
        VoiceRecorder()
    }
}
