//
//  GroupLinksTabView.swift
//  SimpleX (iOS)
//
//  Created by Suren Poghosyan on 14.01.26.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct GroupLinksTabView: View {
    @EnvironmentObject var theme: AppTheme
    
    @State private var mediaItems: [ChatItem] = []
    @State private var isLoading = false
    @State private var isLoadingMore = false
    @State private var hasMore = true
    
    private let pageSize: Int = 100
    let groupInfo: GroupInfo

    var body: some View {
        Group {
            if isLoading {
                loadingView
            } else if mediaItems.isEmpty {
                emptyView
            } else {
                linkListView
            }
        }
        .onAppear {
            loadLinks()
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
                Image(systemName: "link")
                    .font(.system(size: 48))
                    .foregroundColor(theme.colors.secondary)
                Text("No links")
                    .foregroundColor(theme.colors.secondary)
            }
            .padding(.vertical, 60)
            Spacer()
        }
    }
    
    private var linkListView: some View {
        LazyVStack(spacing: 0) {
            ForEach(Array(mediaItems.enumerated()), id: \.element.id) { index, item in
                LinkListRowView(chatItem: item)
                    .onTapGesture { openBrowserAlert(uri: item.content.text) }
                    .onAppear {
                        if index == mediaItems.count - 1 {
                            loadMoreIfNeeded()
                        }
                    }
            }
            if isLoadingMore {
                HStack { Spacer(); ProgressView().padding(); Spacer() }
            }
        }
    }
    
    private func loadLinks(reset: Bool = true) {
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
                    contentTag: .link,
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
                logger.error("GroupLinksTabView: loadLinks error: \(responseError(error))")
                await MainActor.run {
                    if reset { isLoading = false } else { isLoadingMore = false }
                }
            }
        }
    }
    
    private func loadMoreIfNeeded() {
        loadLinks(reset: false)
    }
}
