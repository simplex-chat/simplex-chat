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

    @Binding var voiceItemsState: [String: VoiceItemState]

    //var onChangeVoiceState: (String, AudioPlayer?, VoiceMessagePlaybackState, TimeInterval?) -> Void

    @Binding var allowMenu: Bool
    var smallView: Bool = false
    @State private var seek: (TimeInterval) -> Void = { _ in }

    var body: some View {
        Group {
            if smallView {
                HStack(spacing: 10) {
                    player()
                    playerTime()
                        .allowsHitTesting(false)
                    if .playing == playbackState || (playbackTime ?? 0) > 0 || !allowMenu {
                        playbackSlider()
                    }
                }
            } else if chatItem.chatDir.sent {
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
            chat: chat,
            chatItem: chatItem,
            recordingFile: recordingFile,
            recordingTime: TimeInterval(duration),
            showBackground: true,
            seek: $seek,
            audioPlayer: $audioPlayer,
            playbackState: $playbackState,
            playbackTime: $playbackTime,
            voiceItemsState: $voiceItemsState,
            allowMenu: $allowMenu,
            sizeMultiplier: smallView ? voiceMessageSizeBasedOnSquareSize(36) / 56 : 1
            //onChangeVoiceState: onChangeVoiceState
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
    @ObservedObject var chat: Chat
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
    @Binding var voiceItemsState: [String: VoiceItemState]

    @Binding var allowMenu: Bool
    var sizeMultiplier: CGFloat
    //var onChangeVoiceState: (String, AudioPlayer?, VoiceMessagePlaybackState, TimeInterval?) -> Void

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
                notifyStateChange()
            }
            logger.debug("LALAL APPEAR \(String(describing: playbackState)) \(String(describing: playbackTime))")
        }
        .onChange(of: chatModel.stopPreviousRecPlay) { it in
            if let recordingFileName = getLoadedFileSource(recordingFile)?.filePath,
               chatModel.stopPreviousRecPlay != getAppFilePath(recordingFileName) {
                stopPlayback()
            }
        }
        .onChange(of: playbackState) { state in
            allowMenu = state == .paused || state == .noPlayback
            // Notify activeContentPreview in ChatPreviewView that playback is finished
            if state == .noPlayback, let recordingFileName = getLoadedFileSource(recordingFile)?.filePath,
               chatModel.stopPreviousRecPlay == getAppFilePath(recordingFileName) {
                chatModel.stopPreviousRecPlay = nil
            }
        }
        .onChange(of: chatModel.chatId) { _ in
            stopPlayback()
        }
        .onDisappear {
            if sizeMultiplier == 1 && chatModel.chatId == nil {
                stopPlayback()
            }
        }
    }

    @ViewBuilder private func playbackButton() -> some View {
        if sizeMultiplier != 1 {
            switch playbackState {
            case .noPlayback:
                playPauseIcon("play.fill", theme.colors.primary)
                    .onTapGesture {
                        if let recordingSource = getLoadedFileSource(recordingFile) {
                            startPlayback(recordingSource)
                        }
                    }
            case .playing:
                playPauseIcon("pause.fill", theme.colors.primary)
                    .onTapGesture {
                        audioPlayer?.pause()
                        playbackState = .paused
                        notifyStateChange()
                    }
            case .paused:
                playPauseIcon("play.fill", theme.colors.primary)
                    .onTapGesture {
                        audioPlayer?.play()
                        playbackState = .playing
                        notifyStateChange()
                    }
            }
        } else {
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
                    notifyStateChange()
                } label: {
                    playPauseIcon("pause.fill", theme.colors.primary)
                }
            case .paused:
                Button {
                    audioPlayer?.play()
                    playbackState = .playing
                    notifyStateChange()
                } label: {
                    playPauseIcon("play.fill", theme.colors.primary)
                }
            }
        }
    }

    private func playPauseIcon(_ image: String, _ color: Color/* = .accentColor*/) -> some View {
        ZStack {
            Image(systemName: image)
                .resizable()
                .aspectRatio(contentMode: .fit)
                .frame(width: 20 * sizeMultiplier, height: 20 * sizeMultiplier)
                .foregroundColor(color)
                .padding(.leading, image == "play.fill" ? 4 : 0)
                .frame(width: 56 * sizeMultiplier, height: 56 * sizeMultiplier)
                .background(showBackground ? chatItemFrameColor(chatItem, theme) : .clear)
                .clipShape(Circle())
            if recordingTime > 0 {
                ProgressCircle(length: recordingTime, progress: $playbackTime)
                    .frame(width: 53 * sizeMultiplier, height: 53 * sizeMultiplier) // this + ProgressCircle lineWidth = background circle diameter
            }
        }
    }

    private func downloadButton(_ recordingFile: CIFile, _ icon: String) -> some View {
        Group {
            if sizeMultiplier != 1 {
                playPauseIcon(icon, theme.colors.primary)
                    .onTapGesture {
                        Task {
                            if let user = chatModel.currentUser {
                                await receiveFile(user: user, fileId: recordingFile.fileId)
                            }
                        }
                    }
            } else {
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
        }
    }

    func notifyStateChange() {
        voiceItemsState[VoiceItemState.id(chat, chatItem)] = VoiceItemState(audioPlayer: audioPlayer, playbackState: playbackState, playbackTime: playbackTime)
        //onChangeVoiceState(VoiceItemState.id(chat, chatItem), audioPlayer, playbackState, playbackTime)
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
            .frame(width: size * sizeMultiplier, height: size * sizeMultiplier)
            .foregroundColor(Color(uiColor: .tertiaryLabel))
            .frame(width: 56 * sizeMultiplier, height: 56 * sizeMultiplier)
            .background(showBackground ? chatItemFrameColor(chatItem, theme) : .clear)
            .clipShape(Circle())
    }

    private func loadingIcon() -> some View {
        ProgressView()
            .frame(width: 30 * sizeMultiplier, height: 30 * sizeMultiplier)
            .frame(width: 56 * sizeMultiplier, height: 56 * sizeMultiplier)
            .background(showBackground ? chatItemFrameColor(chatItem, theme) : .clear)
            .clipShape(Circle())
    }

    private func startPlayback(_ recordingSource: CryptoFile) {
        chatModel.stopPreviousRecPlay = getAppFilePath(recordingSource.filePath)
        audioPlayer = AudioPlayer(
            onTimer: {
                playbackTime = $0
                notifyStateChange()
            },
            onFinishPlayback: {
                playbackState = .noPlayback
                playbackTime = TimeInterval(0)
                notifyStateChange()
            }
        )
        audioPlayer?.start(fileSource: recordingSource, at: playbackTime)
        playbackState = .playing
        notifyStateChange()
    }

    private func stopPlayback() {
        audioPlayer?.stop()
        playbackState = .noPlayback
        playbackTime = TimeInterval(0)
        notifyStateChange()
    }
}

func voiceMessageSizeBasedOnSquareSize(_ squareSize: CGFloat) -> CGFloat {
    let squareToCircleRatio = 0.935
    return squareSize + squareSize * (1 - squareToCircleRatio)
}

class VoiceItemState {
    var audioPlayer: AudioPlayer?
    var playbackState: VoiceMessagePlaybackState
    var playbackTime: TimeInterval?

    init(audioPlayer: AudioPlayer? = nil, playbackState: VoiceMessagePlaybackState, playbackTime: TimeInterval? = nil) {
        self.audioPlayer = audioPlayer
        self.playbackState = playbackState
        self.playbackTime = playbackTime
    }

    static func id(_ chat: Chat, _ chatItem: ChatItem) -> String {
        "\(chat.id) \(chatItem.id)"
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
                voiceItemsState: Binding.constant([:]),
                //onChangeVoiceState: { _, _, _, _ in },
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
