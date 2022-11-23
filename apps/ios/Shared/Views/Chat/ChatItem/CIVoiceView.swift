//
//  CIVoiceView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 22.11.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct CIVoiceView: View {
    var chatItem: ChatItem
    let recordingFile: CIFile?
    let duration: Int
    @State var playbackState: VoiceMessagePlaybackState = .noPlayback
    @State var playbackTime: TimeInterval?

    var body: some View {
        VStack (
            alignment: chatItem.chatDir.sent ? .trailing : .leading,
            spacing: 6
        ) {
            HStack {
                if chatItem.chatDir.sent {
                    playerTime()
                        .frame(width: 50, alignment: .leading)
                    player()
                } else {
                    player()
                    playerTime()
                        .frame(width: 50, alignment: .leading)
                }
            }
            CIMetaView(chatItem: chatItem)
                .padding(.leading, chatItem.chatDir.sent ? 0 : 12)
                .padding(.trailing, chatItem.chatDir.sent ? 12 : 0)
        }
        .padding(.bottom, 8)
    }

    private func player() -> some View {
        VoiceMessagePlayer(
            chatItem: chatItem,
            recordingFile: recordingFile,
            recordingTime: TimeInterval(duration),
            showBackground: true,
            playbackState: $playbackState,
            playbackTime: $playbackTime
        )
    }

    private func playerTime() -> some View {
        VoiceMessagePlayerTime(
            recordingTime: TimeInterval(duration),
            playbackState: $playbackState,
            playbackTime: $playbackTime
        )
        .foregroundColor(.secondary)
    }
}

struct VoiceMessagePlayerTime: View {
    var recordingTime: TimeInterval
    @Binding var playbackState: VoiceMessagePlaybackState
    @Binding var playbackTime: TimeInterval?

    var body: some View {
        switch playbackState {
        case .noPlayback:
            Text(voiceMessageTime(recordingTime))
        case .playing:
            Text(voiceMessageTime_(playbackTime))
        case .paused:
            Text(voiceMessageTime_(playbackTime))
        }
    }
}

struct VoiceMessagePlayer: View {
    @Environment(\.colorScheme) var colorScheme
    var chatItem: ChatItem
    var recordingFile: CIFile?
    var recordingTime: TimeInterval
    var showBackground: Bool

    @State var audioPlayer: AudioPlayer?
    @Binding var playbackState: VoiceMessagePlaybackState
    @Binding var playbackTime: TimeInterval?

    var body: some View {
        if let recordingFile = recordingFile {
            switch recordingFile.fileStatus {
            case .sndStored: playbackButton()
            case .sndTransfer: playbackButton()
            case .sndComplete: playbackButton()
            case .sndCancelled: playbackButton()
            case .rcvInvitation: loadingIcon()
            case .rcvAccepted: loadingIcon()
            case .rcvTransfer: loadingIcon()
            case .rcvComplete: playbackButton()
            case .rcvCancelled: playPauseIcon("play.fill", Color(uiColor: .tertiaryLabel))
            }
        } else {
            playPauseIcon("play.fill", Color(uiColor: .tertiaryLabel))
        }
    }

    @ViewBuilder private func playbackButton() -> some View {
        switch playbackState {
        case .noPlayback:
            Button {
                if let recordingFileName = getLoadedFileName(recordingFile) {
                    startPlayback(recordingFileName)
                }
            } label: {
                playPauseIcon("play.fill")
            }
        case .playing:
            Button {
                audioPlayer?.pause()
                playbackState = .paused
            } label: {
                playPauseIcon("pause.fill")
            }
        case .paused:
            Button {
                audioPlayer?.play()
                playbackState = .playing
            } label: {
                playPauseIcon("play.fill")
            }
        }
    }

    private func playPauseIcon(_ image: String, _ color: Color = .accentColor) -> some View {
        ZStack {
            Image(systemName: image)
                .resizable()
                .aspectRatio(contentMode: .fit)
                .frame(width: 20, height: 20)
                .foregroundColor(color)
                .padding(.leading, image == "play.fill" ? 4 : 0)
                .frame(width: 56, height: 56)
                .background(showBackground ? chatItemFrameColor(chatItem, colorScheme) : .clear)
                .clipShape(Circle())
            if recordingTime > 0 {
                ProgressCircle(length: recordingTime, progress: $playbackTime)
                    .frame(width: 54, height: 54) // this + ProgressCircle lineWidth = background circle diameter
            }
        }
    }

    struct ProgressCircle: View {
        var length: TimeInterval
        @Binding var progress: TimeInterval?

        var body: some View {
            Circle()
                .trim(from: 0, to: ((progress ?? TimeInterval(0)) / length))
                .stroke(
                    Color.accentColor,
                    style: StrokeStyle(
                        lineWidth: 2,
                        lineCap: .round
                    )
                )
                .animation(.linear, value: progress)
        }
    }

    private func loadingIcon() -> some View {
        ProgressView()
            .frame(width: 30, height: 30)
            .frame(width: 56, height: 56)
            .background(showBackground ? chatItemFrameColor(chatItem, colorScheme) : .clear)
            .clipShape(Circle())
    }

    private func startPlayback(_ recordingFileName: String) {
        audioPlayer = AudioPlayer(
            onTimer: { playbackTime = $0 },
            onFinishPlayback: {
                playbackState = .noPlayback
                playbackTime = TimeInterval(0)
            }
        )
        audioPlayer?.start(fileName: recordingFileName)
        playbackTime = TimeInterval(0)
        playbackState = .playing
    }
}

struct CIVoiceView_Previews: PreviewProvider {
    static var previews: some View {
        let sentVoiceMessage: ChatItem = ChatItem(
            chatDir: .directSnd,
            meta: CIMeta.getSample(1, .now, "", .sndSent, false, true, false),
            content: .sndMsgContent(msgContent: .voice(text: "", duration: 30)),
            quotedItem: nil,
            file: CIFile.getSample(fileStatus: .sndComplete)
        )
        let voiceMessageWtFile = ChatItem(
            chatDir: .directRcv,
            meta: CIMeta.getSample(1, .now, "", .rcvRead, false, false, false),
            content: .rcvMsgContent(msgContent: .voice(text: "", duration: 30)),
            quotedItem: nil,
            file: nil
        )
        Group {
            CIVoiceView(
                chatItem: ChatItem.getVoiceMsgContentSample(),
                recordingFile: CIFile.getSample(fileName: "voice.m4a", fileSize: 65536, fileStatus: .rcvComplete),
                duration: 30,
                playbackState: .playing,
                playbackTime: TimeInterval(20)
            )
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: sentVoiceMessage)
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getVoiceMsgContentSample())
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getVoiceMsgContentSample(fileStatus: .rcvTransfer))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: voiceMessageWtFile)
        }
        .previewLayout(.fixed(width: 360, height: 360))
    }
}
