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
    @Environment(\.colorScheme) var colorScheme
    private let chatItem: ChatItem
    private let image: String
    @State private var duration: Int
    @State private var progress: Int = 0
    @State private var videoPlaying: Bool = false
    private let maxWidth: CGFloat
    @Binding private var videoWidth: CGFloat?
    @State private var scrollProxy: ScrollViewProxy?
    @State private var preview: UIImage? = nil
    @State private var player: AVPlayer?
    @State private var fullPlayer: AVPlayer?
    @State private var url: URL?
    @State private var urlDecrypted: URL?
    @State private var decryptionInProgress: Bool = false
    @State private var showFullScreenPlayer = false
    @State private var timeObserver: Any? = nil
    @State private var fullScreenTimeObserver: Any? = nil
    @State private var publisher: AnyCancellable? = nil

    init(chatItem: ChatItem, image: String, duration: Int, maxWidth: CGFloat, videoWidth: Binding<CGFloat?>, scrollProxy: ScrollViewProxy?) {
        self.chatItem = chatItem
        self.image = image
        self._duration = State(initialValue: duration)
        self.maxWidth = maxWidth
        self._videoWidth = videoWidth
        self.scrollProxy = scrollProxy
        if let url = getLoadedVideo(chatItem.file) {
            let decrypted = chatItem.file?.fileSource?.cryptoArgs == nil ? url : chatItem.file?.fileSource?.decryptedGet()
            self._urlDecrypted = State(initialValue: decrypted)
            if let decrypted = decrypted {
                self._player = State(initialValue: VideoPlayerView.getOrCreatePlayer(decrypted, false))
                self._fullPlayer = State(initialValue: AVPlayer(url: decrypted))
            }
            self._url = State(initialValue: url)
        }
        if let data = Data(base64Encoded: dropImagePrefix(image)),
           let uiImage = UIImage(data: data) {
            self._preview = State(initialValue: uiImage)
        }
    }

    var body: some View {
        let file = chatItem.file
        ZStack {
            ZStack(alignment: .topLeading) {
                if let file = file, let preview = preview, let player = player, let decrypted = urlDecrypted {
                    videoView(player, decrypted, file, preview, duration)
                } else if let file = file, let defaultPreview = preview, file.loaded && urlDecrypted == nil {
                    videoViewEncrypted(file, defaultPreview, duration)
                } else if let data = Data(base64Encoded: dropImagePrefix(image)),
                          let uiImage = UIImage(data: data) {
                    imageView(uiImage)
                    .onTapGesture {
                        if let file = file {
                            switch file.fileStatus {
                            case .rcvInvitation:
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
                }
                durationProgress()
            }
            if let file = file, case .rcvInvitation = file.fileStatus {
                Button {
                    receiveFileIfValidSize(file: file, receiveFile: receiveFile)
                } label: {
                    playPauseIcon("play.fill")
                }
            }
        }
    }

    private func videoViewEncrypted(_ file: CIFile, _ defaultPreview: UIImage, _ duration: Int) -> some View {
        return ZStack(alignment: .topTrailing) {
            ZStack(alignment: .center) {
                let canBePlayed = !chatItem.chatDir.sent || file.fileStatus == CIFileStatus.sndComplete || (file.fileStatus == .sndStored && file.fileProtocol == .local)
                imageView(defaultPreview)
                .fullScreenCover(isPresented: $showFullScreenPlayer) {
                    if let decrypted = urlDecrypted {
                        fullScreenPlayer(decrypted)
                    }
                }
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
        DispatchQueue.main.async { videoWidth = w }
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
                .fullScreenCover(isPresented: $showFullScreenPlayer) {
                    fullScreenPlayer(url)
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
            loadingIndicator()
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

    private func playPauseIcon(_ image: String, _ color: Color = .white) -> some View {
        Image(systemName: image)
        .resizable()
        .aspectRatio(contentMode: .fit)
        .frame(width: 12, height: 12)
        .foregroundColor(color)
        .padding(.leading, 4)
        .frame(width: 40, height: 40)
        .background(Color.black.opacity(0.35))
        .clipShape(Circle())
    }

    private func videoDecryptionProgress(_ color: Color = .white) -> some View {
        ProgressView()
            .progressViewStyle(.circular)
            .frame(width: 12, height: 12)
            .tint(color)
            .frame(width: 40, height: 40)
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
        DispatchQueue.main.async { videoWidth = w }
        return ZStack(alignment: .topTrailing) {
            Image(uiImage: img)
            .resizable()
            .scaledToFit()
            .frame(width: w)
            loadingIndicator()
        }
    }

    @ViewBuilder private func loadingIndicator() -> some View {
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
            case .sndError: fileIcon("xmark", 10, 13)
            case .rcvInvitation: fileIcon("arrow.down", 10, 13)
            case .rcvAccepted: fileIcon("ellipsis", 14, 11)
            case let .rcvTransfer(rcvProgress, rcvTotal):
                if file.fileProtocol == .xftp && rcvProgress < rcvTotal {
                    progressCircle(rcvProgress, rcvTotal)
                } else {
                    progressView()
                }
            case .rcvCancelled: fileIcon("xmark", 10, 13)
            case .rcvError: fileIcon("xmark", 10, 13)
            case .invalid: fileIcon("questionmark", 10, 13)
            default: EmptyView()
            }
        }
    }

    private func fileIcon(_ icon: String, _ size: CGFloat, _ padding: CGFloat) -> some View {
        Image(systemName: icon)
            .resizable()
            .aspectRatio(contentMode: .fit)
            .frame(width: size, height: size)
            .foregroundColor(.white)
            .padding(padding)
    }

    private func progressView() -> some View {
        ProgressView()
        .progressViewStyle(.circular)
        .frame(width: 16, height: 16)
        .tint(.white)
        .padding(11)
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
        .padding([.trailing, .top], 11)
    }

    // TODO encrypt: where file size is checked?
    private func receiveFileIfValidSize(file: CIFile, receiveFile: @escaping (User, Int64, Bool) async -> Void) {
        Task {
            if let user = m.currentUser {
                await receiveFile(user, file.fileId, false)
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
                    m.stopPreviousRecPlay = url
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
                    player = VideoPlayerView.getOrCreatePlayer(decrypted, false)
                    fullPlayer = AVPlayer(url: decrypted)
                }
                decryptionInProgress = true
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
