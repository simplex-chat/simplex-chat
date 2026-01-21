//
//  VideoPlayerFullScreenWrapper.swift
//  SimpleX
//
//  Created by Suren Poghosyan on 20.01.26.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat
import AVKit

struct VideoPlayerFullScreenWrapper: View {
  
    let chatItem: ChatItem
    
    @Binding var showView: Bool
    @State private var videoURL: URL?
    @State private var videoPlayer: AVPlayer?
    @State private var isDecrypting = true
    
    var body: some View {
        ZStack {
            if let videoURL, let videoPlayer, !isDecrypting {
                FullScreenMediaView(
                    chatItem: chatItem,
                    scrollToItem: nil,
                    player: videoPlayer,
                    url: videoURL,
                    showView: $showView
                )
            } else {
                Color.black.edgesIgnoringSafeArea(.all)
                ProgressView()
                    .tint(.white)
                    .scaleEffect(1.5)
            }
        }
        .onAppear {
            decryptAndLoadVideo()
        }
        .onDisappear {
            videoPlayer?.pause()
            videoPlayer = nil
        }
    }
    
    private func decryptAndLoadVideo() {
        guard let file = chatItem.file,
              let url = getLoadedVideo(file) else {
            showView = false
            return
        }
        
        Task {
            let decryptedURL = file.fileSource?.cryptoArgs == nil
                ? url
                : await file.fileSource?.decryptedGetOrCreate(&ChatModel.shared.filesToDelete)
            
            guard let finalURL = decryptedURL else {
                await MainActor.run {
                    showView = false
                }
                return
            }
            
            let player = VideoPlayerView.getOrCreatePlayer(finalURL, true)
            
            await MainActor.run {
                videoURL = finalURL
                videoPlayer = player
                isDecrypting = false
            }
        }
    }
}
