//
//  FullScreenImageView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 08/10/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat
import SwiftyGif
import AVKit

struct FullScreenMediaView: View {
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var scrollModel: ReverseListScrollModel<ChatItem>
    @State var chatItem: ChatItem
    @State var image: UIImage?
    @State var player: AVPlayer? = nil
    @State var url: URL? = nil
    @Binding var showView: Bool
    @State private var showNext = false
    @State private var nextImage: UIImage?
    @State private var nextPlayer: AVPlayer?
    @State private var nextURL: URL?
    @State private var scrolling = false
    @State private var offset: CGFloat = 0
    @State private var nextOffset: CGFloat = 0

    var body: some View {
        GeometryReader(content: mediaScrollView)
    }

    func mediaScrollView(_ g: GeometryProxy) -> some View {
        ZStack {
            Color.black.edgesIgnoringSafeArea(.all)
            if showNext, let nextImage = nextImage {
                if let image = image {
                    imageView(image).offset(x: offset)
                } else if let player = player, let url = url {
                    videoView(player, url).offset(x: offset)
                }
                imageView(nextImage).offset(x: offset + nextOffset)
            } else if showNext, let nextPlayer = nextPlayer, let nextURL = nextURL {
                if let image = image {
                    imageView(image).offset(x: offset)
                } else if let player = player, let url = url {
                    videoView(player, url).offset(x: offset)
                }
                videoView(nextPlayer, nextURL).offset(x: offset + nextOffset)
            } else {
                ZoomableScrollView {
                    if let image = image {
                        imageView(image)
                    } else if let player = player, let url = url {
                        videoView(player, url)
                    }
                }
            }
        }
        .onAppear {
            startPlayerAndNotify()
        }
        .onDisappear {
            player?.pause()
        }
        .gesture(
            DragGesture(minimumDistance: 80)
            .onChanged { gesture in
                let t = gesture.translation
                let w = abs(t.width)
                if t.height > 60 && t.height > w * 2  {
                    showView = false
                    scrollModel.scrollToItem(id: chatItem.id)
                } else if w > 60 && w > abs(t.height) * 2 && !scrolling {
                    let previous = t.width > 0
                    scrolling = true
                    if let item = m.nextChatItemData(chatItem.id, previous: previous, map: chatItemImage) {
                        var img: UIImage?
                        var url: URL?
                        (chatItem, img, url) = item
                        nextImage = img
                        nextPlayer?.pause()
                        if let url = url {
                            nextPlayer = VideoPlayerView.getOrCreatePlayer(url, true)
                        } else {
                            nextPlayer = nil
                        }
                        nextURL = url
                        let s = g.size.width
                        var toOffset: CGFloat
                        (toOffset, nextOffset) = previous ? (s, -s) : (-s, s)
                        showNext = true
                        withAnimation(.easeIn(duration: 0.2)) {
                            offset = toOffset
                        }
                        DispatchQueue.main.asyncAfter(deadline: .now() + 0.2) {
                            image = img
                            player?.pause()
                            self.url = url
                            if let url = url {
                                player = VideoPlayerView.getOrCreatePlayer(url, true)
                                startPlayerAndNotify()
                            } else {
                                player = nil
                            }
                            showNext = false
                            offset = 0
                        }
                    }
                }
            }
            .onEnded { _ in scrolling = false }
        )
    }

    private func imageView(_ img: UIImage) -> some View {
        ZStack {
            Color.black
            if img.imageData == nil {
                Image(uiImage: img)
                    .resizable()
                    .scaledToFit()
            } else {
                SwiftyGif(image: img)
                        .scaledToFit()
            }
        }
        .onTapGesture { showView = false }
    }

    private func videoView( _ player: AVPlayer, _ url: URL) -> some View {
        VideoPlayerView(player: player, url: url, showControls: true)
    }

    private func chatItemImage(_ ci: ChatItem) -> (ChatItem, UIImage?, URL?)? {
        if case .image = ci.content.msgContent,
           let img = getLoadedImage(ci.file) {
            return (ci, img, nil)
        }
        // Currently, video support in gallery is not enabled
         /*else if case .video = ci.content.msgContent,
           let url = getLoadedVideo(ci.file) {
            return (ci, nil, url)
        }*/
        return nil
    }

    private func startPlayerAndNotify() {
        if let player = player {
            m.stopPreviousRecPlay = url
            player.play()
        }
    }
}
