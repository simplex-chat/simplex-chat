//
//  ComposeVoiceView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 21.11.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

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
    let cancelVoiceMessage: ((String) -> Void)
    let cancelEnabled: Bool

    @State var audioPlayer: AudioPlayer?
    @State var playbackState: VoiceMessagePlaybackState = .noPlayback
    @State var playbackTime: TimeInterval?

    var body: some View {
        ZStack {
            if recordingState != .finished {
                recordingMode()
            } else {
                playbackMode()
            }
        }
        .padding(.vertical, 1)
        .frame(height: 50)
        .background(colorScheme == .light ? sentColorLight : sentColorDark)
        .frame(maxWidth: .infinity)
        .padding(.top, 8)
    }

    private func recordingMode() -> some View {
        ZStack {
            HStack(alignment: .center, spacing: 4) {
                playPauseIcon("play.fill", Color(uiColor: .tertiaryLabel))
                Text(formattedTime(recordingTime))
                Spacer()
                if cancelEnabled {
                    cancelButton()
                }
            }
            .padding(.trailing, 12)

            ProgressBar(length: maxVoiceMessageLength, value: $recordingTime)
        }
    }

    private func playbackMode() -> some View {
        ZStack {
            HStack(alignment: .center, spacing: 4) {
                switch playbackState {
                case .noPlayback:
                    Button {
                        startPlayback()
                    } label: {
                        playPauseIcon("play.fill")
                    }
                    Text(formattedTime(recordingTime))
                case .playing:
                    Button {
                        audioPlayer?.pause()
                        playbackState = .paused
                    } label: {
                        playPauseIcon("pause.fill")
                    }
                    Text(formattedTime(playbackTime))
                case .paused:
                    Button {
                        audioPlayer?.play()
                        playbackState = .playing
                    } label: {
                        playPauseIcon("play.fill")
                    }
                    Text(formattedTime(playbackTime))
                }
                Spacer()
                if cancelEnabled {
                    cancelButton()
                }
            }
            .padding(.trailing, 12)

            if let recordingLength = recordingTime {
                ProgressBar(length: recordingLength, value: $playbackTime)
            }
        }
    }

    private func playPauseIcon(_ image: String, _ color: Color = .accentColor) -> some View {
        Image(systemName: image)
            .resizable()
            .aspectRatio(contentMode: .fit)
            .frame(width: 20, height: 20)
            .foregroundColor(color)
            .padding(.leading, 12)
    }

    private func formattedTime(_ time: TimeInterval?) -> String {
        let t = time ?? TimeInterval(0)
        let min = Int(t / 60)
        let sec = Int(t.truncatingRemainder(dividingBy: 60))
        return String(format: "%02d:%02d", min, sec)
    }

    private func cancelButton() -> some View {
        Button {
            audioPlayer?.stop()
            cancelVoiceMessage(recordingFileName)
        } label: {
            Image(systemName: "multiply")
        }
    }

    struct ProgressBar: View {
        var length: TimeInterval
        @Binding var value: TimeInterval?

        var body: some View {
            GeometryReader { geometry in
                Rectangle()
                    .frame(width: min(CGFloat((value ?? TimeInterval(0)) / length) * geometry.size.width, geometry.size.width), height: 2)
                    .foregroundColor(.accentColor)
                    .animation(.linear, value: value)
            }
        }
    }

    private func startPlayback() {
        audioPlayer = AudioPlayer(
            onTimer: { playbackTime = $0 },
            onFinishPlayback: {
                playbackState = .noPlayback
                playbackTime = recordingTime // animate progress bar to the end
            }
        )
        audioPlayer?.start(fileName: recordingFileName)
        playbackTime = TimeInterval(0)
        playbackState = .playing
    }
}
