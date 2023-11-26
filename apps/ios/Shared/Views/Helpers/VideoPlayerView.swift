//
// Created by Avently on 30.03.2023.
// Copyright (c) 2023 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI
import AVKit
import Combine

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
        controller.showsPlaybackControls = showControls
        if #available(iOS 16.0, *) {
            controller.speeds = []
        }
        controller.player = player
        context.coordinator.controller = controller
        context.coordinator.timeObserver = NotificationCenter.default.addObserver(forName: .AVPlayerItemDidPlayToEndTime, object: player.currentItem, queue: .main) { _ in
            player.seek(to: CMTime.zero)
            player.play()
        }
        var played = false
        context.coordinator.publisher = player.publisher(for: \.timeControlStatus).sink { status in
            if played || status == .playing {
                AppDelegate.keepScreenOn(status == .playing)
                AudioPlayer.changeAudioSession(status == .playing)
            }
            played = status == .playing
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
        var timeObserver: Any? = nil
        var publisher: AnyCancellable? = nil

        deinit {
            if let timeObserver = timeObserver {
                NotificationCenter.default.removeObserver(timeObserver)
            }
            publisher?.cancel()
        }
    }
}
