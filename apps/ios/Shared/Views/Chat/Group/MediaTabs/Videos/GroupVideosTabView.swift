//
//  GroupVideosTabView.swift
//  SimpleX (iOS)
//
//  Created by Suren Poghosyan on 14.01.26.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat
import AVKit

struct GroupVideosTabView: View {
  
    // MARK: - Environment
    @EnvironmentObject var theme: AppTheme
    @EnvironmentObject var m: ChatModel
    
    // MARK: - Properties
    let groupInfo: GroupInfo
    private let pageSize: Int = 100

    // MARK: - State
    @State private var mediaItems: [ChatItem] = []
    @State private var isLoading = false
    @State private var isLoadingMore = false
    @State private var hasMore = true
    @State private var selectedVideo: ChatItem?
    @State private var showFullScreenVideo = false
    @State private var videoURL: URL?
    @State private var videoPlayer: AVPlayer?
    @State private var isDecrypting = false
    @State private var lastRequestedBeforeId: Int64?
    
    // MARK: - Body
    var body: some View {
        Group {
            if isLoading {
                loadingView
            } else if mediaItems.isEmpty {
                emptyView
            } else {
                videoGridView
            }
        }
        .task {
            if mediaItems.isEmpty && !isLoading {
                loadVideos(reset: true)
            }
        }
    }
    
    // MARK: - Subviews
    private var loadingView: some View {
        HStack {
            Spacer()
            ProgressView()
                .padding(.vertical, 40)
            Spacer()
        }
    }
    
    private var emptyView: some View {
        HStack {
            Spacer()
            VStack(spacing: 12) {
                Image(systemName: "video")
                    .font(.system(size: 48))
                    .foregroundColor(theme.colors.secondary)
                Text("No videos")
                    .foregroundColor(theme.colors.secondary)
            }
            .padding(.vertical, 60)
            Spacer()
        }
    }
    
    private var videoGridView: some View {
        VStack(spacing: 0) {
            GeometryReader { geometry in
                let spacing: CGFloat = 1
                let availableWidth = geometry.size.width
                let videoSize = (availableWidth - (spacing * 2)) / 3
                let numberOfRows = ceil(Double(mediaItems.count) / 3.0)
                let totalHeight = (videoSize * CGFloat(numberOfRows)) + (spacing * max(0, CGFloat(numberOfRows - 1)))
                
                let columns = [
                    GridItem(.fixed(videoSize), spacing: spacing),
                    GridItem(.fixed(videoSize), spacing: spacing),
                    GridItem(.fixed(videoSize), spacing: spacing)
                ]
                
                LazyVGrid(columns: columns, spacing: spacing) {
                    ForEach(Array(mediaItems.enumerated()), id: \.element.id) { index, item in
                        if let file = item.file {
                            VideoThumbnailView(
                                chatItem: item,
                                file: file,
                                size: videoSize
                            )
                            .onTapGesture { openVideoPlayer(item) }
                            .onAppear {
                                if index == mediaItems.count - 1 {
                                    loadMoreIfNeeded()
                                }
                            }
                        }
                    }
                }
                .frame(width: availableWidth, height: totalHeight, alignment: .topLeading)
                
                if isLoadingMore {
                    HStack { Spacer(); ProgressView().padding(); Spacer() }
                }
            }
            .frame(height: mediaItems.isEmpty ? 1 : calculateEstimatedHeight())
            .clipped()
        }
        .fullScreenCover(isPresented: $showFullScreenVideo) {
            fullScreenVideoContent
        }
    }
    
    @ViewBuilder
    private var fullScreenVideoContent: some View {
        if let selectedVideo, let videoURL, let videoPlayer {
            FullScreenMediaView(
                chatItem: selectedVideo,
                scrollToItem: nil,
                player: videoPlayer,
                url: videoURL,
                showView: $showFullScreenVideo
            )
            .onDisappear(perform: cleanupVideoPlayer)
        } else if isDecrypting {
            decryptingView
        }
    }
    
    private var decryptingView: some View {
        ZStack {
            Color.black.edgesIgnoringSafeArea(.all)
            ProgressView()
                .tint(.white)
                .scaleEffect(1.5)
        }
    }
    
    // MARK: - Helper Methods
    private func calculateEstimatedHeight() -> CGFloat {
        guard !mediaItems.isEmpty else { return 0 }
        
        let spacing: CGFloat = 1
        let screenWidth = UIScreen.main.bounds.width - 32
        let videoSize = (screenWidth - (spacing * 2)) / 3
        let numberOfRows = ceil(Double(mediaItems.count) / 3.0)
        let totalHeight = (videoSize * CGFloat(numberOfRows)) + (spacing * max(0, CGFloat(numberOfRows - 1)))
        
        return totalHeight
    }
    
    private func loadVideos(reset: Bool = true) {
        if reset {
            guard !isLoading else { return }
            isLoading = true
            mediaItems = []
            hasMore = true
        } else {
            guard !isLoadingMore && hasMore else { return }
            isLoadingMore = true
        }

        let count = pageSize
        let pagination: ChatPagination = {
            if reset {
                return .last(count: count)
            } else if let lastId = mediaItems.last?.id {
                return .before(chatItemId: lastId, count: count)
            } else {
                return .last(count: count)
            }
        }()

        Task {
            do {
                let (chat, _) = try await apiGetChat(
                    chatId: groupInfo.id,
                    scope: nil,
                    contentTag: .video,
                    pagination: pagination,
                    search: ""
                )
                await MainActor.run {
                    if reset {
                        mediaItems = chat.chatItems
                        isLoading = false
                    } else {
                        mediaItems.append(contentsOf: chat.chatItems)
                        isLoadingMore = false
                    }
                    hasMore = chat.chatItems.count == pageSize
                }
            } catch {
                logger.error("GroupVideosTabView.loadVideos error: \(responseError(error))")
                await MainActor.run {
                    if reset { isLoading = false } else { isLoadingMore = false }
                }
            }
        }
    }
    
    private func loadMoreIfNeeded() {
        guard !isLoadingMore && hasMore else { return }
        guard let lastId = mediaItems.last?.id else { return }
        guard lastRequestedBeforeId != lastId else { return }
        lastRequestedBeforeId = lastId
        loadVideos(reset: false)
    }
    
    private func openVideoPlayer(_ item: ChatItem) {
        guard let file = item.file else {
            return
        }
        
        guard file.loaded else {
            showVideoNotAvailableAlert()
            return
        }
        
        selectedVideo = item
        isDecrypting = true
        showFullScreenVideo = true
        
        Task {
            await decryptAndPlayVideo(file: file)
        }
    }
    
    private func decryptAndPlayVideo(file: CIFile) async {
        guard let url = getLoadedVideo(file) else {
            logger.error("GroupVideosTabView: Failed to get video URL")
            dismissVideoPlayer()
            return
        }
        
        let decryptedURL = file.fileSource?.cryptoArgs == nil
            ? url
            : await file.fileSource?.decryptedGetOrCreate(&ChatModel.shared.filesToDelete)
        
        guard let finalURL = decryptedURL else {
            logger.error("GroupVideosTabView: Failed to decrypt video")
            dismissVideoPlayer()
            return
        }
        
        let player = VideoPlayerView.getOrCreatePlayer(finalURL, true)
        
        await MainActor.run {
            videoURL = finalURL
            videoPlayer = player
            isDecrypting = false
        }
    }
    
    @MainActor
    private func dismissVideoPlayer() {
        showFullScreenVideo = false
        isDecrypting = false
        cleanupVideoPlayer()
    }
    
    private func cleanupVideoPlayer() {
        selectedVideo = nil
        videoURL = nil
        videoPlayer = nil
    }
    
    private func showVideoNotAvailableAlert() {
        AlertManager.shared.showAlertMsg(
            title: "Video not available",
            message: "This video hasn't been downloaded yet. Please download it from the chat first."
        )
    }
}

