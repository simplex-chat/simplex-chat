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
    @Environment(\.colorScheme) var colorScheme
    var chatItem: ChatItem
    let recordingFile: CIFile?
    let duration: Int
    @State var playbackState: VoiceMessagePlaybackState = .noPlayback
    @State var playbackTime: TimeInterval?

    var body: some View {
        let recordingTime = TimeInterval(duration)
        VStack{
            HStack {
                VoiceMessagePlayer(
                    recordingFile: recordingFile,
                    recordingTime: recordingTime,
                    showBackground: true,
                    playbackState: $playbackState,
                    playbackTime: $playbackTime
                )
                VoiceMessagePlayerTime(
                    recordingTime: recordingTime,
                    playbackState: $playbackState,
                    playbackTime: $playbackTime
                )
            }
            CIMetaView(chatItem: chatItem)
        }
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
            case .rcvInvitation: ProgressView().frame(width: 30, height: 30)
            case .rcvAccepted: ProgressView().frame(width: 30, height: 30)
            case .rcvTransfer: ProgressView().frame(width: 30, height: 30)
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
        Image(systemName: image)
            .resizable()
            .aspectRatio(contentMode: .fit)
            .frame(width: 20, height: 20)
            .foregroundColor(color)
            .padding(.leading, 12)
    }

    private func startPlayback(_ recordingFileName: String) {
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

struct CIVoiceView_Previews: PreviewProvider {
    static var previews: some View {
        let sentFile: ChatItem = ChatItem(
            chatDir: .directSnd,
            meta: CIMeta.getSample(1, .now, "", .sndSent, false, true, false),
            content: .sndMsgContent(msgContent: .file("")),
            quotedItem: nil,
            file: CIFile.getSample(fileStatus: .sndComplete)
        )
        let fileChatItemWtFile = ChatItem(
            chatDir: .directRcv,
            meta: CIMeta.getSample(1, .now, "", .rcvRead, false, false, false),
            content: .rcvMsgContent(msgContent: .file("")),
            quotedItem: nil,
            file: nil
        )
        Group{
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: sentFile)
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getFileMsgContentSample())
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getFileMsgContentSample(fileName: "some_long_file_name_here", fileStatus: .rcvInvitation))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getFileMsgContentSample(fileStatus: .rcvAccepted))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getFileMsgContentSample(fileStatus: .rcvTransfer))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getFileMsgContentSample(fileStatus: .rcvCancelled))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getFileMsgContentSample(fileSize: 1_000_000_000, fileStatus: .rcvInvitation))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getFileMsgContentSample(text: "Hello there", fileStatus: .rcvInvitation))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getFileMsgContentSample(text: "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.", fileStatus: .rcvInvitation))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: fileChatItemWtFile)
        }
        .previewLayout(.fixed(width: 360, height: 360))
    }
}
