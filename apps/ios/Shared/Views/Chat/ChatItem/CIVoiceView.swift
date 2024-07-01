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
    @ObservedObject var chat: Chat
    @EnvironmentObject var theme: AppTheme
    var chatItem: ChatItem
    let recordingFile: CIFile?
    let duration: Int
    @Binding var audioPlayer: AudioPlayer?
    @Binding var playbackState: VoiceMessagePlaybackState
    @Binding var playbackTime: TimeInterval?
    @Binding var allowMenu: Bool
    @State private var seek: (TimeInterval) -> Void = { _ in }

    var body: some View {
        Group {
            if chatItem.chatDir.sent {
                VStack (alignment: .trailing, spacing: 6) {
                    HStack {
                        if .playing == playbackState || (playbackTime ?? 0) > 0 || !allowMenu {
                            playbackSlider()
                        }
                        playerTime()
                        player()
                    }
                    .frame(alignment: .trailing)
                    metaView().padding(.trailing, 10)
                }
            } else {
                VStack (alignment: .leading, spacing: 6) {
                    HStack {
                        player()
                        playerTime()
                        if .playing == playbackState || (playbackTime ?? 0) > 0 || !allowMenu {
                            playbackSlider()
                        }
                    }
                    .frame(alignment: .leading)
                    metaView().padding(.leading, -6)
                }
            }
        }
        .padding(.top, 4)
        .padding(.bottom, 8)
    }

    private func player() -> some View {
        VoiceMessagePlayer(
            chatItem: chatItem,
            recordingFile: recordingFile,
            recordingTime: TimeInterval(duration),
            showBackground: true,
            seek: $seek,
            audioPlayer: $audioPlayer,
            playbackState: $playbackState,
            playbackTime: $playbackTime,
            allowMenu: $allowMenu
        )
    }

    private func playerTime() -> some View {
        VoiceMessagePlayerTime(
            recordingTime: TimeInterval(duration),
            playbackState: $playbackState,
            playbackTime: $playbackTime
        )
        .foregroundColor(theme.colors.secondary)
    }
    
    private func playbackSlider() -> some View {
            ComposeVoiceView.SliderBar(
                length: TimeInterval(duration),
                progress: $playbackTime,
                seek: {
                    let time = max(0.0001, $0)
                    seek(time)
                    playbackTime = time
                })
            .onChange(of: .playing == playbackState || (playbackTime ?? 0) > 0) { show in
                if !show {
                    allowMenu = true
                }
            }
            .tint(theme.colors.primary)
    }

    private func metaView() -> some View {
        CIMetaView(chat: chat, chatItem: chatItem, metaColor: theme.colors.secondary)
    }
}

struct VoiceMessagePlayerTime: View {
    var recordingTime: TimeInterval
    @Binding var playbackState: VoiceMessagePlaybackState
    @Binding var playbackTime: TimeInterval?

    var body: some View {
        ZStack(alignment: .leading) {
            Text(String("66:66")).foregroundColor(.clear)
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
}

struct VoiceMessagePlayer: View {
    @EnvironmentObject var chatModel: ChatModel
    @EnvironmentObject var theme: AppTheme
    var chatItem: ChatItem
    var recordingFile: CIFile?
    var recordingTime: TimeInterval
    var showBackground: Bool

    @Binding var seek: (TimeInterval) -> Void
    @Binding var audioPlayer: AudioPlayer?
    @Binding var playbackState: VoiceMessagePlaybackState
    @Binding var playbackTime: TimeInterval?
    @Binding var allowMenu: Bool

    var body: some View {
        ZStack {
            if let recordingFile = recordingFile {
                switch recordingFile.fileStatus {
                case .sndStored:
                    if recordingFile.fileProtocol == .local {
                        playbackButton()
                    } else {
                        loadingIcon()
                    }
                case .sndTransfer: loadingIcon()
                case .sndComplete: playbackButton()
                case .sndCancelled: playbackButton()
                case let .sndError(sndFileError):
                    fileStatusIcon("multiply", 14)
                        .onTapGesture {
                            AlertManager.shared.showAlert(Alert(
                                title: Text("File error"),
                                message: Text(sndFileError.errorInfo)
                            ))
                        }
                case let .sndWarning(sndFileError):
                    fileStatusIcon("exclamationmark.triangle.fill", 16)
                        .onTapGesture {
                            AlertManager.shared.showAlert(Alert(
                                title: Text("Temporary file error"),
                                message: Text(sndFileError.errorInfo)
                            ))
                        }
                case .rcvInvitation: downloadButton(recordingFile, "play.fill")
                case .rcvAccepted: loadingIcon()
                case .rcvTransfer: loadingIcon()
                case .rcvAborted: downloadButton(recordingFile, "exclamationmark.arrow.circlepath")
                case .rcvComplete: playbackButton()
                case .rcvCancelled: playPauseIcon("play.fill", Color(uiColor: .tertiaryLabel))
                case let .rcvError(rcvFileError):
                    fileStatusIcon("multiply", 14)
                        .onTapGesture {
                            AlertManager.shared.showAlert(Alert(
                                title: Text("File error"),
                                message: Text(rcvFileError.errorInfo)
                            ))
                        }
                case let .rcvWarning(rcvFileError):
                    fileStatusIcon("exclamationmark.triangle.fill", 16)
                        .onTapGesture {
                            AlertManager.shared.showAlert(Alert(
                                title: Text("Temporary file error"),
                                message: Text(rcvFileError.errorInfo)
                            ))
                        }
                case .invalid: playPauseIcon("play.fill", Color(uiColor: .tertiaryLabel))
                }
            } else {
                playPauseIcon("play.fill", Color(uiColor: .tertiaryLabel))
            }
        }
        .onAppear {
            seek = { to in audioPlayer?.seek(to) }
            audioPlayer?.onTimer = { playbackTime = $0 }
            audioPlayer?.onFinishPlayback = {
                playbackState = .noPlayback
                playbackTime = TimeInterval(0)
            }
        }
        .onChange(of: chatModel.stopPreviousRecPlay) { it in
            if let recordingFileName = getLoadedFileSource(recordingFile)?.filePath,
               chatModel.stopPreviousRecPlay != getAppFilePath(recordingFileName) {
                audioPlayer?.stop()
                playbackState = .noPlayback
                playbackTime = TimeInterval(0)
            }
        }
        .onChange(of: playbackState) { state in
            allowMenu = state == .paused || state == .noPlayback
        }
    }

    @ViewBuilder private func playbackButton() -> some View {
        switch playbackState {
        case .noPlayback:
            Button {
                if let recordingSource = getLoadedFileSource(recordingFile) {
                    startPlayback(recordingSource)
                }
            } label: {
                playPauseIcon("play.fill", theme.colors.primary)
            }
        case .playing:
            Button {
                audioPlayer?.pause()
                playbackState = .paused
            } label: {
                playPauseIcon("pause.fill", theme.colors.primary)
            }
        case .paused:
            Button {
                audioPlayer?.play()
                playbackState = .playing
            } label: {
                playPauseIcon("play.fill", theme.colors.primary)
            }
        }
    }

    private func playPauseIcon(_ image: String, _ color: Color/* = .accentColor*/) -> some View {
        ZStack {
            Image(systemName: image)
                .resizable()
                .aspectRatio(contentMode: .fit)
                .frame(width: 20, height: 20)
                .foregroundColor(color)
                .padding(.leading, image == "play.fill" ? 4 : 0)
                .frame(width: 56, height: 56)
                .background(showBackground ? chatItemFrameColor(chatItem, theme) : .clear)
                .clipShape(Circle())
            if recordingTime > 0 {
                ProgressCircle(length: recordingTime, progress: $playbackTime)
                    .frame(width: 53, height: 53) // this + ProgressCircle lineWidth = background circle diameter
            }
        }
    }

    private func downloadButton(_ recordingFile: CIFile, _ icon: String) -> some View {
        Button {
            Task {
                if let user = chatModel.currentUser {
                    await receiveFile(user: user, fileId: recordingFile.fileId)
                }
            }
        } label: {
            playPauseIcon(icon, theme.colors.primary)
        }
    }

    private struct ProgressCircle: View {
        @EnvironmentObject var theme: AppTheme
        var length: TimeInterval
        @Binding var progress: TimeInterval?

        var body: some View {
            Circle()
                .trim(from: 0, to: ((progress ?? TimeInterval(0)) / length))
                .stroke(
                    theme.colors.primary,
                    style: StrokeStyle(lineWidth: 3)
                )
                .rotationEffect(.degrees(-90))
                .animation(.linear, value: progress)
        }
    }

    private func fileStatusIcon(_ image: String, _ size: CGFloat) -> some View {
        Image(systemName: image)
            .resizable()
            .aspectRatio(contentMode: .fit)
            .frame(width: size, height: size)
            .foregroundColor(Color(uiColor: .tertiaryLabel))
            .frame(width: 56, height: 56)
            .background(showBackground ? chatItemFrameColor(chatItem, theme) : .clear)
            .clipShape(Circle())
    }

    private func loadingIcon() -> some View {
        ProgressView()
            .frame(width: 30, height: 30)
            .frame(width: 56, height: 56)
            .background(showBackground ? chatItemFrameColor(chatItem, theme) : .clear)
            .clipShape(Circle())
    }

    private func startPlayback(_ recordingSource: CryptoFile) {
        chatModel.stopPreviousRecPlay = getAppFilePath(recordingSource.filePath)
        audioPlayer = AudioPlayer(
            onTimer: { playbackTime = $0 },
            onFinishPlayback: {
                playbackState = .noPlayback
                playbackTime = TimeInterval(0)
            }
        )
        audioPlayer?.start(fileSource: recordingSource, at: playbackTime)
        playbackState = .playing
    }
}

struct CIVoiceView_Previews: PreviewProvider {
    static var previews: some View {
        let sentVoiceMessage: ChatItem = ChatItem(
            chatDir: .directSnd,
            meta: CIMeta.getSample(1, .now, "", .sndSent(sndProgress: .complete), itemEdited: true),
            content: .sndMsgContent(msgContent: .voice(text: "", duration: 30)),
            quotedItem: nil,
            file: CIFile.getSample(fileStatus: .sndComplete)
        )
        let voiceMessageWtFile = ChatItem(
            chatDir: .directRcv,
            meta: CIMeta.getSample(1, .now, "", .rcvRead),
            content: .rcvMsgContent(msgContent: .voice(text: "", duration: 30)),
            quotedItem: nil,
            file: nil
        )
        Group {
            CIVoiceView(
                chat: Chat.sampleData,
                chatItem: ChatItem.getVoiceMsgContentSample(),
                recordingFile: CIFile.getSample(fileName: "voice.m4a", fileSize: 65536, fileStatus: .rcvComplete),
                duration: 30,
                audioPlayer: .constant(nil),
                playbackState: .constant(.playing),
                playbackTime: .constant(TimeInterval(20)),
                allowMenu: Binding.constant(true)
            )
            ChatItemView(chat: Chat.sampleData, chatItem: sentVoiceMessage, revealed: Binding.constant(false), allowMenu: .constant(true), playbackState: .constant(.noPlayback), playbackTime: .constant(nil))
            ChatItemView(chat: Chat.sampleData, chatItem: ChatItem.getVoiceMsgContentSample(), revealed: Binding.constant(false), allowMenu: .constant(true), playbackState: .constant(.noPlayback), playbackTime: .constant(nil))
            ChatItemView(chat: Chat.sampleData, chatItem: ChatItem.getVoiceMsgContentSample(fileStatus: .rcvTransfer(rcvProgress: 7, rcvTotal: 10)), revealed: Binding.constant(false), allowMenu: .constant(true), playbackState: .constant(.noPlayback), playbackTime: .constant(nil))
            ChatItemView(chat: Chat.sampleData, chatItem: voiceMessageWtFile, revealed: Binding.constant(false), allowMenu: .constant(true), playbackState: .constant(.noPlayback), playbackTime: .constant(nil))
        }
        .previewLayout(.fixed(width: 360, height: 360))
    }
}
