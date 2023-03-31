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

struct CIVideoView: View {
    @Environment(\.colorScheme) var colorScheme
    private let chatItem: ChatItem
    private let image: String
    @State private var duration: Int
    @State private var progress: Int = 0
    @State private var videoPlaying: Bool = false
    private let maxWidth: CGFloat
    @Binding private var imgWidth: CGFloat?
    @State private var scrollProxy: ScrollViewProxy?
    @State private var preview: UIImage? = nil
    @State private var player: AVPlayer?
    @State private var url: URL?
    @State private var showFullScreenImage = false

    init(chatItem: ChatItem, image: String, duration: Int, maxWidth: CGFloat, imgWidth: Binding<CGFloat?>, scrollProxy: ScrollViewProxy?) {
        self.chatItem = chatItem
        self.image = image
        self._duration = State(initialValue: duration)
        self.maxWidth = maxWidth
        self._imgWidth = imgWidth
        self.scrollProxy = scrollProxy
        if let url = getLoadedVideo(chatItem.file) {
            self._player = State(initialValue: VideoPlayerView.getOrCreatePlayer(url, false))
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
                if let file = file, let preview = preview, let player = player, let url = url {
                    videoView(player, url, file, preview, duration)
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
                                        title: "Waiting for image",
                                        message: "Image will be received when your contact completes uploading it."
                                    )
                                case .smp:
                                    AlertManager.shared.showAlertMsg(
                                        title: "Waiting for image",
                                        message: "Image will be received when your contact is online, please wait or check later!"
                                    )
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

    private func videoView(_ player: AVPlayer, _ url: URL, _ file: CIFile, _ preview: UIImage, _ duration: Int) -> some View {
        let w = preview.size.width <= preview.size.height ? maxWidth * 0.75 : preview.imageData == nil ? .infinity : maxWidth
        DispatchQueue.main.async { imgWidth = w }
        return ZStack(alignment: .center) {
            VideoPlayerView(player: player, url: url, showControls: false)
            .frame(width: w, height: w * preview.size.height / preview.size.width)
            .fullScreenCover(isPresented: $showFullScreenImage) {
                FullScreenMediaView(chatItem: chatItem, image: nil, player: VideoPlayerView.getOrCreatePlayer(url, true), url: url, showView: $showFullScreenImage, scrollProxy: scrollProxy)
            }
            .onTapGesture {
                switch player.timeControlStatus {
                case .playing:
                    player.pause()
                case .paused:
                    showFullScreenImage = true
                default: do {
                }
                }
            }
            if !videoPlaying {
                Button {
                    player.play()
                } label: {
                    playPauseIcon("play.fill")
                }
            }
            loadingIndicator()
        }
        .onAppear {
            addObserver(player, url)
        }
        .onDisappear {
            player.pause()
            removeObserver()
        }
    }

    private func playPauseIcon(_ image: String, _ color: Color = .white) -> some View {
        Image(systemName: image)
        .resizable()
        .aspectRatio(contentMode: .fit)
        .frame(width: 40, height: 40)
        .foregroundColor(color)
        .padding(.leading, 12)
    }

    private func durationProgress() -> some View {
        HStack {
            Text("\(durationText(videoPlaying ? progress : duration))")
            .foregroundColor(.white)
            .font(.caption)
            .padding(.vertical, 3)
            .padding(.horizontal, 6)
            .background(Color.black.opacity(0.4))
            .cornerRadius(10)
            .padding([.top, .leading], 6)

            if let file = chatItem.file, !videoPlaying {
                Text("\(ByteCountFormatter.string(fromByteCount: file.fileSize, countStyle: .binary))")
                .foregroundColor(.white)
                .font(.caption)
                .padding(.vertical, 3)
                .padding(.horizontal, 6)
                .background(Color.black.opacity(0.4))
                .cornerRadius(10)
                .padding(.top, 6)
            }
        }
    }

    private func imageView(_ img: UIImage) -> some View {
        let w = img.size.width <= img.size.height ? maxWidth * 0.75 : img.imageData == nil ? .infinity : maxWidth
        DispatchQueue.main.async { imgWidth = w }
        return ZStack(alignment: .topTrailing) {
            if img.imageData == nil {
                Image(uiImage: img)
                        .resizable()
                        .scaledToFit()
                        .frame(maxWidth: w)
            } else {
                SwiftyGif(image: img)
                        .frame(width: w, height: w * img.size.height / img.size.width)
                        .scaledToFit()
            }
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
                }
            case .sndTransfer:
                progressView()
            case .sndComplete:
                Image(systemName: "checkmark")
                    .resizable()
                    .aspectRatio(contentMode: .fit)
                    .frame(width: 10, height: 10)
                    .foregroundColor(.white)
                    .padding(13)
            case .rcvAccepted:
                Image(systemName: "ellipsis")
                    .resizable()
                    .aspectRatio(contentMode: .fit)
                    .frame(width: 14, height: 14)
                    .foregroundColor(.white)
                    .padding(11)
            case .rcvTransfer:
                progressView()
            default: EmptyView()
            }
        }
    }

    private func progressView() -> some View {
        ProgressView()
            .progressViewStyle(.circular)
            .frame(width: 20, height: 20)
            .tint(.white)
            .padding(8)
    }

    private func receiveFileIfValidSize(file: CIFile, receiveFile: @escaping (User, Int64) async -> Void) {
        Task {
            if let user = ChatModel.shared.currentUser {
                await receiveFile(user, file.fileId)
            }
            // TODO image accepted alert?
        }
    }

    private func addObserver(_ player: AVPlayer, _ url: URL) {
        player.addPeriodicTimeObserver(forInterval: CMTimeMakeWithSeconds(1/30.0, preferredTimescale: Int32(NSEC_PER_SEC)), queue: nil) { time in
            if let item = player.currentItem  {
                let dur = CMTimeGetSeconds(item.duration)
                if !dur.isInfinite && !dur.isNaN {
                    duration = Int(dur)
                }
                progress = Int(CMTimeGetSeconds(player.currentTime()))
                let wasPlaying = videoPlaying
                // `if` prevents showing Play button while the playback seeks to start and then plays
                if player.currentTime() != player.currentItem?.duration && player.currentTime() != .zero {
                    videoPlaying = player.timeControlStatus == .playing || player.timeControlStatus == .waitingToPlayAtSpecifiedRate
                }
                if wasPlaying != videoPlaying && videoPlaying {
                    NotificationCenter.default.post(name: .MediaStartedPlaying, object: nil, userInfo: ["url": url])
                }
            }
        }
    }

    private func removeObserver() {
        NotificationCenter.default.removeObserver(self)
    }
}
