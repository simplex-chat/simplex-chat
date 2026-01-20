//
//  GroupImagesTabView.swift
//  SimpleX (iOS)
//
//  Created for media filtering feature
//

import SwiftUI
import SimpleXChat

struct GroupImagesTabView: View {
    @EnvironmentObject var theme: AppTheme
    let groupInfo: GroupInfo
    @Binding var selectedImage: ChatItem?
    @Binding var showImageFullScreen: Bool
    
    @State private var mediaItems: [ChatItem] = []
    @State private var isLoading = false
    @State private var isLoadingMore = false
    @State private var hasMore = true
    private let pageSize: Int = 100
    
    var body: some View {
        Group {
            if isLoading {
                loadingView
            } else if mediaItems.isEmpty {
                emptyView
            } else {
                VStack(spacing: 0) {
                    imageGridView
                }
            }
        }
        .onAppear {
            loadImages()
        }
    }
    
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
                Image(systemName: "photo")
                    .font(.system(size: 48))
                    .foregroundColor(theme.colors.secondary)
                Text("No images")
                    .foregroundColor(theme.colors.secondary)
            }
            .padding(.vertical, 60)
            Spacer()
        }
    }
    
    private var imageGridView: some View {
        VStack(spacing: 0) {
            GeometryReader { geometry in
                let spacing: CGFloat = 1
                let availableWidth = geometry.size.width
                let imageSize = (availableWidth - (spacing * 2)) / 3
                let numberOfRows = ceil(Double(mediaItems.count) / 3.0)
                let totalHeight = (imageSize * CGFloat(numberOfRows)) + (spacing * max(0, CGFloat(numberOfRows-1)))
                let columns = [
                    GridItem(.fixed(imageSize), spacing: spacing),
                    GridItem(.fixed(imageSize), spacing: spacing),
                    GridItem(.fixed(imageSize), spacing: spacing)
                ]
                
                LazyVGrid(columns: columns, spacing: spacing) {
                    ForEach(Array(mediaItems.enumerated()), id: \.element.id) { index, item in
                        if let file = item.file {
                            ImageThumbnailView(
                                chatItem: item,
                                file: file,
                                size: imageSize,
                                selectedImage: $selectedImage,
                                showFullScreen: $showImageFullScreen
                            )
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
            .frame(height: calculateEstimatedHeight())
            .clipped()
        }
    }
    
    private func calculateEstimatedHeight() -> CGFloat {
        guard !mediaItems.isEmpty else { return 0 }
        let spacing: CGFloat = 1
        let screenWidth = UIScreen.main.bounds.width - 32
        let imageSize = (screenWidth - (spacing * 2)) / 3
        let numberOfRows = ceil(Double(mediaItems.count) / 3.0)
        let totalHeight = (imageSize * CGFloat(numberOfRows)) + (spacing * max(0, CGFloat(numberOfRows - 1)))
        return totalHeight
    }
    
    private func loadImages(reset: Bool = true) {
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
                    contentTag: .image,
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
                logger.error("loadImages error: \(responseError(error))")
                await MainActor.run {
                    if reset { isLoading = false } else { isLoadingMore = false }
                }
            }
        }
    }
    
    private func loadMoreIfNeeded() {
        loadImages(reset: false)
    }
}

