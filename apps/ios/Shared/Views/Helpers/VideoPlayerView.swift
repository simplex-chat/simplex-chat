//
// Created by Avently on 30.03.2023.
// Copyright (c) 2023 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI
import AVKit

struct VideoPlayerView: UIViewRepresentable {

    static var players: [String: AVPlayer] = [:]
    static func getOrCreatePlayer(_ url: URL, _ gallery: Bool) -> AVPlayer {
        if let player = players[url.absoluteString + gallery.description] {
            return player
        } else {
            let player = AVPlayer(url: url)
            players[url.absoluteString + gallery.description] = player
            return player
        }
    }

    typealias UIViewType = UIView
    let player: AVPlayer
    let url: URL
    let showControls: Bool

    func makeUIView(context: UIViewRepresentableContext<VideoPlayerView>) -> UIView {
        let controller = AVPlayerViewController()
        controller.player = player
        controller.showsPlaybackControls = showControls
        context.coordinator.controller = controller
        NotificationCenter.default.addObserver(forName: .AVPlayerItemDidPlayToEndTime, object: player.currentItem, queue: .main) { _ in
            player.seek(to: CMTime.zero)
            player.play()
        }
        NotificationCenter.default.addObserver(forName: .MediaStartedPlaying, object: nil, queue: .main) { ntf in
            if let url = ntf.userInfo?.first(where: { $0.key as? String == "url" })?.value as? URL, url != self.url {
                // Other player started to play
                player.pause()
            }
        }
        return controller.view
    }

    func updateUIView(_ uiView: UIView, context: UIViewRepresentableContext<VideoPlayerView>) {
    }

    func makeCoordinator() -> VideoPlayerView.Coordinator {
        Coordinator()
    }

    class Coordinator: NSObject {
        var controller: AVPlayerViewController?

        deinit {
            print("deinit coordinator of VideoPlayer")
            NotificationCenter.default.removeObserver(self)
        }
    }
}

extension Notification.Name {
    static let MediaStartedPlaying: NSNotification.Name = Notification.Name(rawValue: "MediaStartedPlaying")
}
