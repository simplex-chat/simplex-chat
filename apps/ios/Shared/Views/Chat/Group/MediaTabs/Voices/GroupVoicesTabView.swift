//
//  GroupVoicesTabView.swift
//  SimpleX (iOS)
//
//  Created by Suren Poghosyan on 14.01.26.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct GroupVoicesTabView: View {
    @EnvironmentObject var theme: AppTheme
    @EnvironmentObject var chatModel: ChatModel
    
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
                voiceListView
            }
        }
        .onAppear {
            loadVoices()
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
                Image(systemName: "waveform")
                    .font(.system(size: 48))
                    .foregroundColor(theme.colors.secondary)
                Text("No voice messages")
                    .foregroundColor(theme.colors.secondary)
            }
            .padding(.vertical, 60)
            Spacer()
        }
    }
    
    private var voiceListView: some View {
        LazyVStack(spacing: 0) {
            ForEach(Array(mediaItems.enumerated()), id: \.element.id) { index, item in
                VoiceListRowView(
                    chat: Chat(chatInfo: .group(groupInfo: groupInfo, groupChatScope: nil), chatItems: []),
                    chatItem: item
                )
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
    
    private func loadVoices(reset: Bool = true) {
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
                    contentTag: .voice,
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
                logger.error("loadVoices error: \(responseError(error))")
                await MainActor.run {
                    if reset { isLoading = false } else { isLoadingMore = false }
                }
            }
        }
    }
    
    private func loadMoreIfNeeded() {
        loadVoices(reset: false)
    }
}
