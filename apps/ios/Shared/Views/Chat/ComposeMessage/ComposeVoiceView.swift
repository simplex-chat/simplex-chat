//
//  ComposeVoiceView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 21.11.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

enum VoiceMessagePlaybackState {
    case noPlayback
    case playing
    case paused
}

struct ComposeVoiceView: View {
    @Environment(\.colorScheme) var colorScheme
    var recordingFileName: String
    @Binding var recordingTime: TimeInterval?
    @Binding var recordingState: VoiceMessageRecordingState
    let cancelVoiceMessage: (() -> Void)
    let cancelEnabled: Bool

    @State var audioPlayer: AudioPlayer?
    @State var playbackState: VoiceMessagePlaybackState = .noPlayback
    @State var playbackTime: TimeInterval = TimeInterval(0)

    var body: some View {
        HStack(alignment: .center, spacing: 4) {
            if recordingState != .finished {
                recordingMode()
            } else {
                playbackMode()
            }
            Spacer()
            if cancelEnabled {
                Button {
                    audioPlayer?.stop()
                    cancelVoiceMessage()
                } label: {
                    Image(systemName: "multiply")
                }
            }
        }
        .padding(.vertical, 1)
        .padding(.trailing, 12)
        .frame(height: 50)
        .background(colorScheme == .light ? sentColorLight : sentColorDark)
        .frame(maxWidth: .infinity)
        .padding(.top, 8)
    }

    private func recordingMode() -> some View {
        HStack {
            Image(systemName: "play.fill")
                .resizable()
                .aspectRatio(contentMode: .fit)
                .frame(width: 20, height: 20)
                .foregroundColor(Color(uiColor: .tertiaryLabel))
                .padding(.leading, 12)
            if let time = recordingTime {
                Text(formattedTime(time))
            }
        }
    }

    private func playbackMode() -> some View {
        HStack {
            switch playbackState {
            case .noPlayback:
                Button { startPlayback() } label: {
                    Image(systemName: "play.fill")
                        .resizable()
                        .aspectRatio(contentMode: .fit)
                        .frame(width: 20, height: 20)
                        .padding(.leading, 12)
                }
            case .playing:
                Button {
                    audioPlayer?.pause()
                    playbackState = .paused
                } label: {
                    Image(systemName: "pause.fill")
                        .resizable()
                        .aspectRatio(contentMode: .fit)
                        .frame(width: 20, height: 20)
                        .padding(.leading, 12)
                }
            case .paused:
                Button {
                    audioPlayer?.play()
                    playbackState = .playing
                } label: {
                    Image(systemName: "play.fill")
                        .resizable()
                        .aspectRatio(contentMode: .fit)
                        .frame(width: 20, height: 20)
                        .padding(.leading, 12)
                }
            }
            Text(formattedTime(playbackTime))
        }
    }

    private func formattedTime(_ time: TimeInterval) -> String {
        let min = Int(time / 60)
        let sec = Int(time.truncatingRemainder(dividingBy: 60))
        return String(format: "%02d:%02d", min, sec)
    }

    private func startPlayback() {
        audioPlayer = AudioPlayer(
            onTimer: { playbackTime = $0 },
            onFinishPlayback: { playbackState = .noPlayback }
        )
        audioPlayer?.start(fileName: recordingFileName)
        playbackState = .playing
    }
}
