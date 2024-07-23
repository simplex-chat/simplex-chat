//
//  CIVideoView.swift
//  SimpleX
//
//  Created by Avently on 30/03/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import AVKit
import SimpleXChat
import Combine

struct CIVideoView: View {
    @EnvironmentObject var m: ChatModel
    private let chatItem: ChatItem
    private let preview: UIImage?
    @State private var duration: Int
    @State private var progress: Int = 0
    @State private var videoPlaying: Bool = false
    private let maxWidth: CGFloat
    private var videoWidth: CGFloat?
    private let smallView: Bool
    @State private var player: AVPlayer?
    @State private var fullPlayer: AVPlayer?
    @State private var url: URL?
    @State private var urlDecrypted: URL?
    @State private var decryptionInProgress: Bool = false
    @Binding private var showFullScreenPlayer: Bool
    @State private var timeObserver: Any? = nil
    @State private var fullScreenTimeObserver: Any? = nil
    @State private var publisher: AnyCancellable? = nil
    private var sizeMultiplier: CGFloat { smallView ? 0.38 : 1 }

    init(chatItem: ChatItem, preview: UIImage?, duration: Int, maxWidth: CGFloat, videoWidth: CGFloat?, smallView: Bool = false, showFullscreenPlayer: Binding<Bool>) {
        self.chatItem = chatItem
        self.preview = preview
        self._duration = State(initialValue: duration)
        self.maxWidth = maxWidth
        self.videoWidth = videoWidth
        self.smallView = smallView
        self._showFullScreenPlayer = showFullscreenPlayer
    }

    var body: some View {
        let file = chatItem.file
        ZStack(alignment: smallView ? .topLeading : .center) {
            ZStack(alignment: .topLeading) {
                if let file = file, let preview = preview, let decrypted = urlDecrypted, smallView {
                    smallVideoView(decrypted, file, preview)
                } else if let file = file, let preview = preview, let player = player, let decrypted = urlDecrypted {
                    videoView(player, decrypted, file, preview, duration)
                } else if let file = file, let defaultPreview = preview, file.loaded && urlDecrypted == nil, smallView {
                    smallVideoViewEncrypted(file, defaultPreview)
                } else if let file = file, let defaultPreview = preview, file.loaded && urlDecrypted == nil {
                    videoViewEncrypted(file, defaultPreview, duration)
                } else if let preview, let file {
                    Group { if smallView { smallViewImageView(preview, file) } else { imageView(preview) } }
                        .onTapGesture {
                            switch file.fileStatus {
                            case .rcvInvitation, .rcvAborted:
                                receiveFileIfValidSize(file: file, receiveFile: receiveFile)
                            case .rcvAccepted:
                                switch file.fileProtocol {
                                case .xftp:
                                    AlertManager.shared.showAlertMsg(
                                        title: "Waiting for video",
                                        message: "Video will be received when your contact completes uploading it."
                                    )
                                case .smp:
                                    AlertManager.shared.showAlertMsg(
                                        title: "Waiting for video",
                                        message: "Video will be received when your contact is online, please wait or check later!"
                                    )
                                case .local: ()
                                }
                            case .rcvTransfer: () // ?
                            case .rcvComplete: () // ?
                            case .rcvCancelled: () // TODO
                            default: ()
                            }
                        }
                }
                if !smallView {
                    durationProgress()
                }
            }
            if !smallView, let file = file, showDownloadButton(file.fileStatus) {
                Button {
                    receiveFileIfValidSize(file: file, receiveFile: receiveFile)
                } label: {
                    playPauseIcon("play.fill")
                }
            } else if let file = file, showDownloadButton(file.fileStatus) && !file.showStatusIconInSmallView {
                playPauseIcon("play.fill")
                    .onTapGesture {
                        receiveFileIfValidSize(file: file, receiveFile: receiveFile)
                    }
            }
        }
        .fullScreenCover(isPresented: $showFullScreenPlayer) {
            if let decrypted = urlDecrypted {
                fullScreenPlayer(decrypted)
            }
        }
        .onAppear {
            setupPlayer(chatItem.file)
        }
        .onChange(of: chatItem.file) { file in
            // ChatItem can be changed in small view on chat list screen
            setupPlayer(file)
        }
        .onDisappear {
            showFullScreenPlayer = false
        }
    }

    private func setupPlayer(_ file: CIFile?) {
        let newUrl = getLoadedVideo(file)
        if newUrl == url {
            return
        }
        url = nil
        urlDecrypted = nil
        player = nil
        fullPlayer = nil
        if let newUrl {
            let decrypted = file?.fileSource?.cryptoArgs == nil ? newUrl : file?.fileSource?.decryptedGet()
            urlDecrypted = decrypted
            if let decrypted = decrypted {
                player = VideoPlayerView.getOrCreatePlayer(decrypted, false)
                fullPlayer = AVPlayer(url: decrypted)
            }
            url = newUrl
        }
    }

    private func showDownloadButton(_ fileStatus: CIFileStatus) -> Bool {
        switch fileStatus {
        case .rcvInvitation: true
        case .rcvAborted: true
        default: false
        }
    }

    private func videoViewEncrypted(_ file: CIFile, _ defaultPreview: UIImage, _ duration: Int) -> some View {
        return ZStack(alignment: .topTrailing) {
            ZStack(alignment: .center) {
                let canBePlayed = !chatItem.chatDir.sent || file.fileStatus == CIFileStatus.sndComplete || (file.fileStatus == .sndStored && file.fileProtocol == .local)
                imageView(defaultPreview)
                .onTapGesture {
                    decrypt(file: file) {
                        showFullScreenPlayer = urlDecrypted != nil
                    }
                }
                .onChange(of: m.activeCallViewIsCollapsed) { _ in
                    showFullScreenPlayer = false
                }
                if !decryptionInProgress {
                    Button {
                        decrypt(file: file) {
                            if urlDecrypted != nil {
                                videoPlaying = true
                                player?.play()
                            }
                        }
                    } label: {
                        playPauseIcon(canBePlayed ? "play.fill" : "play.slash")
                    }
                    .disabled(!canBePlayed)
                } else {
                    videoDecryptionProgress()
                }
            }
        }
    }

    private func videoView(_ player: AVPlayer, _ url: URL, _ file: CIFile, _ preview: UIImage, _ duration: Int) -> some View {
        let w = preview.size.width <= preview.size.height ? maxWidth * 0.75 : maxWidth
        return ZStack(alignment: .topTrailing) {
            ZStack(alignment: .center) {
                let canBePlayed = !chatItem.chatDir.sent || file.fileStatus == CIFileStatus.sndComplete || (file.fileStatus == .sndStored && file.fileProtocol == .local)
                VideoPlayerView(player: player, url: url, showControls: false)
                .frame(width: w, height: w * preview.size.height / preview.size.width)
                .onChange(of: m.stopPreviousRecPlay) { playingUrl in
                    if playingUrl != url {
                        player.pause()
                        videoPlaying = false
                    }
                }
                .onTapGesture {
                    switch player.timeControlStatus {
                    case .playing:
                        player.pause()
                        videoPlaying = false
                    case .paused:
                        if canBePlayed {
                            showFullScreenPlayer = true
                        }
                    default: ()
                    }
                }
                .onChange(of: m.activeCallViewIsCollapsed) { _ in
                    showFullScreenPlayer = false
                }
                if !videoPlaying {
                    Button {
                        m.stopPreviousRecPlay = url
                        player.play()
                    } label: {
                        playPauseIcon(canBePlayed ? "play.fill" : "play.slash")
                    }
                    .disabled(!canBePlayed)
                }
            }
            fileStatusIcon()
        }
        .onAppear {
            addObserver(player, url)
        }
        .onDisappear {
            removeObserver()
            player.pause()
            videoPlaying = false
        }
    }

    private func smallVideoViewEncrypted(_ file: CIFile, _ preview: UIImage) -> some View {
        return ZStack(alignment: .topLeading) {
            let canBePlayed = !chatItem.chatDir.sent || file.fileStatus == CIFileStatus.sndComplete || (file.fileStatus == .sndStored && file.fileProtocol == .local)
            smallViewImageView(preview, file)
                .onTapGesture {
                    decrypt(file: file) {
                        showFullScreenPlayer = urlDecrypted != nil
                    }
                }
                .onChange(of: m.activeCallViewIsCollapsed) { _ in
                    showFullScreenPlayer = false
                }
            if file.showStatusIconInSmallView {
                // Show nothing
            } else if !decryptionInProgress {
                playPauseIcon(canBePlayed ? "play.fill" : "play.slash")
            } else {
                videoDecryptionProgress()
            }
        }
    }

    private func smallVideoView(_ url: URL, _ file: CIFile, _ preview: UIImage) -> some View {
        return ZStack(alignment: .topLeading) {
            smallViewImageView(preview, file)
                .onTapGesture {
                    showFullScreenPlayer = true
                }
                .onChange(of: m.activeCallViewIsCollapsed) { _ in
                    showFullScreenPlayer = false
                }

            if !file.showStatusIconInSmallView {
                playPauseIcon("play.fill")
            }
        }
    }


    private func playPauseIcon(_ image: String, _ color: Color = .white) -> some View {
        Image(systemName: image)
        .resizable()
        .aspectRatio(contentMode: .fit)
        .frame(width: smallView ? 12 * sizeMultiplier * 1.6 : 12, height: smallView ? 12 * sizeMultiplier * 1.6 : 12)
        .foregroundColor(color)
        .padding(.leading, smallView ? 0 : 4)
        .frame(width: 40 * sizeMultiplier, height: 40 * sizeMultiplier)
        .background(Color.black.opacity(0.35))
        .clipShape(Circle())
    }

    private func videoDecryptionProgress(_ color: Color = .white) -> some View {
        ProgressView()
            .progressViewStyle(.circular)
            .frame(width: smallView ? 12 * sizeMultiplier : 12, height: smallView ? 12 * sizeMultiplier : 12)
            .tint(color)
            .frame(width: smallView ? 40 * sizeMultiplier * 0.9 : 40, height: smallView ? 40 * sizeMultiplier * 0.9 : 40)
            .background(Color.black.opacity(0.35))
            .clipShape(Circle())
    }

    private func durationProgress() -> some View {
        HStack {
            Text("\(durationText(videoPlaying ? progress : duration))")
            .foregroundColor(.white)
            .font(.caption)
            .padding(.vertical, 3)
            .padding(.horizontal, 6)
            .background(Color.black.opacity(0.35))
            .cornerRadius(10)
            .padding([.top, .leading], 6)

            if let file = chatItem.file, !videoPlaying {
                Text("\(ByteCountFormatter.string(fromByteCount: file.fileSize, countStyle: .binary))")
                .foregroundColor(.white)
                .font(.caption)
                .padding(.vertical, 3)
                .padding(.horizontal, 6)
                .background(Color.black.opacity(0.35))
                .cornerRadius(10)
                .padding(.top, 6)
            }
        }
    }

    private func imageView(_ img: UIImage) -> some View {
        let w = img.size.width <= img.size.height ? maxWidth * 0.75 : maxWidth
        return ZStack(alignment: .topTrailing) {
            Image(uiImage: img)
            .resizable()
            .scaledToFit()
            .frame(width: w)
            fileStatusIcon()
        }
    }

    private func smallViewImageView(_ img: UIImage, _ file: CIFile) -> some View {
        ZStack(alignment: .center) {
            Image(uiImage: img)
                .resizable()
                .aspectRatio(contentMode: .fill)
                .frame(width: maxWidth, height: maxWidth)
            if file.showStatusIconInSmallView {
                fileStatusIcon()
                    .allowsHitTesting(false)
            }
        }
    }

    @ViewBuilder private func fileStatusIcon() -> some View {
        if let file = chatItem.file {
            switch file.fileStatus {
            case .sndStored:
                switch file.fileProtocol {
                case .xftp: progressView()
                case .smp: EmptyView()
                case .local: EmptyView()
                }
            case let .sndTransfer(sndProgress, sndTotal):
                switch file.fileProtocol {
                case .xftp: progressCircle(sndProgress, sndTotal)
                case .smp: progressView()
                case .local: EmptyView()
                }
            case .sndComplete: fileIcon("checkmark", 10, 13)
            case .sndCancelled: fileIcon("xmark", 10, 13)
            case let .sndError(sndFileError):
                fileIcon("xmark", 10, 13)
                    .onTapGesture {
                        AlertManager.shared.showAlert(Alert(
                            title: Text("File error"),
                            message: Text(sndFileError.errorInfo)
                        ))
                    }
            case let .sndWarning(sndFileError):
                fileIcon("exclamationmark.triangle.fill", 10, 13)
                    .onTapGesture {
                        AlertManager.shared.showAlert(Alert(
                            title: Text("Temporary file error"),
                            message: Text(sndFileError.errorInfo)
                        ))
                    }
            case .rcvInvitation: fileIcon("arrow.down", 10, 13)
            case .rcvAccepted: fileIcon("ellipsis", 14, 11)
            case let .rcvTransfer(rcvProgress, rcvTotal):
                if file.fileProtocol == .xftp && rcvProgress < rcvTotal {
                    progressCircle(rcvProgress, rcvTotal)
                } else {
                    progressView()
                }
            case .rcvAborted: fileIcon("exclamationmark.arrow.circlepath", 14, 11)
            case .rcvComplete: EmptyView()
            case .rcvCancelled: fileIcon("xmark", 10, 13)
            case let .rcvError(rcvFileError):
                fileIcon("xmark", 10, 13)
                    .onTapGesture {
                        AlertManager.shared.showAlert(Alert(
                            title: Text("File error"),
                            message: Text(rcvFileError.errorInfo)
                        ))
                    }
            case let .rcvWarning(rcvFileError):
                fileIcon("exclamationmark.triangle.fill", 10, 13)
                    .onTapGesture {
                        AlertManager.shared.showAlert(Alert(
                            title: Text("Temporary file error"),
                            message: Text(rcvFileError.errorInfo)
                        ))
                    }
            case .invalid: fileIcon("questionmark", 10, 13)
            }
        }
    }

    private func fileIcon(_ icon: String, _ size: CGFloat, _ padding: CGFloat) -> some View {
        Image(systemName: icon)
            .resizable()
            .aspectRatio(contentMode: .fit)
            .frame(width: size, height: size)
            .foregroundColor(.white)
            .padding(smallView ? 0 : padding)
    }

    private func progressView() -> some View {
        ProgressView()
        .progressViewStyle(.circular)
        .frame(width: 16, height: 16)
        .tint(.white)
        .padding(smallView ? 0 : 11)
    }

    private func progressCircle(_ progress: Int64, _ total: Int64) -> some View {
        Circle()
        .trim(from: 0, to: Double(progress) / Double(total))
        .stroke(
            Color(uiColor: .white),
            style: StrokeStyle(lineWidth: 2)
        )
        .rotationEffect(.degrees(-90))
        .frame(width: 16, height: 16)
        .padding([.trailing, .top], smallView ? 0 : 11)
    }

    // TODO encrypt: where file size is checked?
    private func receiveFileIfValidSize(file: CIFile, receiveFile: @escaping (User, Int64, Bool, Bool) async -> Void) {
        Task {
            if let user = m.currentUser {
                await receiveFile(user, file.fileId, false, false)
            }
        }
    }

    private func fullScreenPlayer(_ url: URL) -> some View {
        ZStack {
            Color.black.edgesIgnoringSafeArea(.all)
            VideoPlayer(player: fullPlayer)
            .overlay(alignment: .topLeading, content: {
                Button(action: { showFullScreenPlayer = false },
                    label: {
                        Image(systemName: "multiply")
                        .resizable()
                        .tint(.white)
                        .frame(width: 15, height: 15)
                        .padding(.leading, 15)
                        .padding(.top, 13)
                    }
                )
            })
            .gesture(
                DragGesture(minimumDistance: 80)
                .onChanged { gesture in
                    let t = gesture.translation
                    let w = abs(t.width)
                    if t.height > 60 && t.height > w * 2 {
                        showFullScreenPlayer = false
                    }
                }
            )
            .onAppear {
                DispatchQueue.main.asyncAfter(deadline: .now()) {
                    // Prevent feedback loop - setting `ChatModel`s property causes `onAppear` to be called on iOS17+
                    if m.stopPreviousRecPlay != url { m.stopPreviousRecPlay = url }
                    if let player = fullPlayer {
                        player.play()
                        var played = false
                        publisher = player.publisher(for: \.timeControlStatus).sink { status in
                            if played || status == .playing {
                                AppDelegate.keepScreenOn(status == .playing)
                                AudioPlayer.changeAudioSession(status == .playing)
                            }
                            played = status == .playing
                        }
                        fullScreenTimeObserver = NotificationCenter.default.addObserver(forName: .AVPlayerItemDidPlayToEndTime, object: player.currentItem, queue: .main) { _ in
                            player.seek(to: CMTime.zero)
                            player.play()
                        }
                    }
                }
            }
            .onDisappear {
                if let fullScreenTimeObserver = fullScreenTimeObserver {
                    NotificationCenter.default.removeObserver(fullScreenTimeObserver)
                }
                fullScreenTimeObserver = nil
                fullPlayer?.pause()
                fullPlayer?.seek(to: CMTime.zero)
                publisher?.cancel()
            }
        }
    }

    private func decrypt(file: CIFile, completed: (() -> Void)? = nil) {
        if decryptionInProgress { return }
        decryptionInProgress = true
        Task {
            urlDecrypted = await file.fileSource?.decryptedGetOrCreate(&ChatModel.shared.filesToDelete)
            await MainActor.run {
                if let decrypted = urlDecrypted {
                    if !smallView {
                        player = VideoPlayerView.getOrCreatePlayer(decrypted, false)
                    }
                    fullPlayer = AVPlayer(url: decrypted)
                }
                decryptionInProgress = false
                completed?()
            }
        }
    }

    private func addObserver(_ player: AVPlayer, _ url: URL) {
        timeObserver = player.addPeriodicTimeObserver(forInterval: CMTime(seconds: 0.01, preferredTimescale: CMTimeScale(NSEC_PER_SEC)), queue: .main) { time in
            if let item = player.currentItem {
                let dur = CMTimeGetSeconds(item.duration)
                if !dur.isInfinite && !dur.isNaN {
                    duration = Int(dur)
                }
                progress = Int(CMTimeGetSeconds(player.currentTime()))
                // `if` prevents showing Play button while the playback seeks to start and then plays
                if player.currentTime() != player.currentItem?.duration && player.currentTime() != .zero {
                    videoPlaying = player.timeControlStatus == .playing || player.timeControlStatus == .waitingToPlayAtSpecifiedRate
                }
            }
        }
    }

    private func removeObserver() {
        if let timeObserver = timeObserver {
            player?.removeTimeObserver(timeObserver)
        }
        timeObserver = nil
    }
}
