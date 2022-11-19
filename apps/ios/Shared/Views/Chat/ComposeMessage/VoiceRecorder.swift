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
    @State var audioRecorder: AVAudioRecorder?
    @State var audioPlayer: AVAudioPlayer?
    @State var recordingTimer: Timer?
    @State var recordingTime = ""
    @State var recordingState = RecordingState.new
    @State var recordingUrl: URL?

    var body: some View {
        switch recordingState {
        case .new:
            Button("record") {
                let url = getAppFilePath(generateNewFileName("voice", "m4a"))
                recordingUrl = url
                audioRecorder = startAudioRecording(url: url)
                if audioRecorder != nil  {
                    recordingState = .recording
                    recordingTimer = Timer.scheduledTimer(withTimeInterval: 0.2, repeats: true) { timer in
                        logger.debug("recording timer")
                        guard let time = audioRecorder?.currentTime else { return }
                        let min = Int(time / 60)
                        let sec = Int(time.truncatingRemainder(dividingBy: 60))
                        DispatchQueue.main.async {
                            recordingTime = String(format: "%02d:%02d", min, sec)
                        }
                    }
                } else {
                    // TODO
                }
            }
        case .recording:
            Button("stop") {
                audioRecorder?.stop()
                audioRecorder = nil
                recordingState = .stopped
                recordingTimer?.invalidate()
            }
//            Button("delete") {}
            Text(recordingTime)
        case .stopped:
            Button("play") {
                if let url = recordingUrl {
                    audioPlayer = startAudioPlayback(url: url)
                    recordingState = .playing
                }
            }
            Text(recordingTime)
            Button("delete") {
                if let url = recordingUrl {
                    _ = try? FileManager.default.removeItem(atPath: url.path)
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
                if let url = recordingUrl {
                    _ = try? FileManager.default.removeItem(atPath: url.path)
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
