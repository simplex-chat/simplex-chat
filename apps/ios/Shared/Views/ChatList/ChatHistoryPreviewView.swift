//
//  ChatHistoryPreviewView.swift
//  SimpleX (iOS)
//
//  Created on 14/11/2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

// Spec: spec/client/chat-list.md#ChatHistoryPreviewView
struct ChatHistoryPreviewView: View {
    @ObservedObject var chat: Chat
    @EnvironmentObject var theme: AppTheme
    @State private var items: [ChatItem]
    @State private var isLoading = true

    init(chat: Chat) {
        self.chat = chat
        _items = State(initialValue: chat.chatItems)
    }

    var body: some View {
        VStack(spacing: 0) {
            headerView
            Divider()
            contentView
        }
        .background(theme.colors.background)
        .task {
            await loadRecentMessages()
        }
    }

    private var headerView: some View {
        HStack(spacing: 12) {
            ChatInfoImage(chat: chat, size: 38)
            VStack(alignment: .leading, spacing: 2) {
                Text(chat.chatInfo.displayName)
                    .font(.headline)
                    .foregroundColor(theme.colors.onSurface)
                    .lineLimit(1)
                Text(subtitle)
                    .font(.caption)
                    .foregroundColor(theme.colors.secondary)
                    .lineLimit(1)
            }
            Spacer()
            if isLoading {
                ProgressView()
                    .scaleEffect(0.8)
            }
        }
        .padding(.horizontal, 14)
        .padding(.vertical, 10)
        .background(theme.colors.surface)
    }

    private var subtitle: String {
        switch chat.chatInfo {
        case let .direct(contact):
            return contact.connected ? NSLocalizedString("Connected", comment: "chat subtitle") : NSLocalizedString("Connecting...", comment: "chat subtitle")
        case let .group(groupInfo, _):
            if groupInfo.useRelays {
                return NSLocalizedString("Channel", comment: "chat subtitle")
            } else {
                let count = groupInfo.membership.membersCount
                return String.localizedStringWithFormat(NSLocalizedString("%d members", comment: "chat subtitle"), count)
            }
        case .local:
            return NSLocalizedString("Notes to self", comment: "chat subtitle")
        default:
            return ""
        }
    }

    @ViewBuilder private var contentView: some View {
        if items.isEmpty && !isLoading {
            emptyStateView
        } else {
            messagesListView
        }
    }

    private var emptyStateView: some View {
        VStack(spacing: 8) {
            Spacer()
            Image(systemName: "bubble.left.and.bubble.right")
                .font(.system(size: 36))
                .foregroundColor(theme.colors.secondary.opacity(0.6))
            Text(NSLocalizedString("No messages yet", comment: "empty preview state"))
                .font(.subheadline)
                .foregroundColor(theme.colors.secondary)
            Spacer()
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
    }

    private var messagesListView: some View {
        ScrollViewReader { scrollProxy in
            ScrollView {
                LazyVStack(spacing: 8) {
                    ForEach(items, id: \.id) { item in
                        ChatPreviewBubbleView(chat: chat, item: item)
                            .id(item.id)
                    }
                }
                .padding(.horizontal, 12)
                .padding(.vertical, 10)
            }
            .onAppear {
                if let lastId = items.last?.id {
                    scrollProxy.scrollTo(lastId, anchor: .bottom)
                }
            }
            .onChange(of: items.count) { _ in
                if let lastId = items.last?.id {
                    withAnimation {
                        scrollProxy.scrollTo(lastId, anchor: .bottom)
                    }
                }
            }
        }
    }

    private func loadRecentMessages() async {
        do {
            let (chatWithItems, _) = try await apiGetChat(
                chatId: chat.chatInfo.id,
                scope: chat.chatInfo.groupChatScope(),
                pagination: .last(count: 10)
            )
            await MainActor.run {
                if !chatWithItems.chatItems.isEmpty {
                    self.items = chatWithItems.chatItems
                }
                self.isLoading = false
            }
        } catch {
            logger.error("ChatHistoryPreviewView apiGetChat error: \(responseError(error))")
            await MainActor.run {
                self.isLoading = false
            }
        }
    }
}

// Spec: spec/client/chat-list.md#ChatPreviewBubbleView
struct ChatPreviewBubbleView: View {
    @ObservedObject var chat: Chat
    let item: ChatItem
    @EnvironmentObject var theme: AppTheme

    private var isSent: Bool { item.chatDir.sent }

    private var senderDisplayName: String? {
        if isSent { return nil }
        switch item.chatDir {
        case let .groupRcv(groupMember):
            return groupMember.displayName
        case .channelRcv:
            return chat.chatInfo.displayName
        default:
            return nil
        }
    }

    var body: some View {
        HStack {
            if isSent { Spacer(minLength: 44) }
            VStack(alignment: isSent ? .trailing : .leading, spacing: 2) {
                if let sender = senderDisplayName {
                    Text(sender)
                        .font(.caption2)
                        .fontWeight(.semibold)
                        .foregroundColor(theme.colors.primary)
                        .padding(.horizontal, 4)
                }
                bubbleContent
                    .padding(.horizontal, 10)
                    .padding(.vertical, 7)
                    .background(isSent ? theme.appColors.sentMessage : theme.appColors.receivedMessage)
                    .cornerRadius(14)

                item.meta.timestampText
                    .font(.system(size: 10))
                    .foregroundColor(theme.colors.secondary)
                    .padding(.horizontal, 4)
            }
            if !isSent { Spacer(minLength: 44) }
        }
    }

    @ViewBuilder private var bubbleContent: some View {
        switch item.content {
        case let .sndMsgContent(msgContent), let .rcvMsgContent(msgContent):
            msgContentView(msgContent)
        default:
            Text(item.content.text)
                .font(.subheadline)
                .foregroundColor(theme.colors.onSurface)
        }
    }

    @ViewBuilder private func msgContentView(_ mc: MsgContent) -> some View {
        VStack(alignment: .leading, spacing: 4) {
            switch mc {
            case let .text(text):
                Text(text)
                    .font(.subheadline)
                    .foregroundColor(theme.colors.onSurface)
            case let .image(text, imageStr):
                if let uiImage = imageFromBase64(imageStr) {
                    Image(uiImage: uiImage)
                        .resizable()
                        .scaledToFit()
                        .frame(maxHeight: 140)
                        .cornerRadius(8)
                }
                if !text.isEmpty {
                    Text(text)
                        .font(.subheadline)
                        .foregroundColor(theme.colors.onSurface)
                }
            case let .video(text, imageStr, duration):
                ZStack {
                    if let uiImage = imageFromBase64(imageStr) {
                        Image(uiImage: uiImage)
                            .resizable()
                            .scaledToFit()
                            .frame(maxHeight: 140)
                            .cornerRadius(8)
                    }
                    Image(systemName: "play.circle.fill")
                        .font(.title)
                        .foregroundColor(.white.opacity(0.85))
                }
                if duration > 0 {
                    Text(voiceMessageTime(TimeInterval(duration)))
                        .font(.caption2)
                        .foregroundColor(theme.colors.secondary)
                }
                if !text.isEmpty {
                    Text(text)
                        .font(.subheadline)
                        .foregroundColor(theme.colors.onSurface)
                }
            case let .voice(text, duration):
                HStack(spacing: 6) {
                    Image(systemName: "mic.fill")
                        .foregroundColor(theme.colors.primary)
                    Text(voiceMessageTime(TimeInterval(duration)))
                        .font(.subheadline)
                        .foregroundColor(theme.colors.onSurface)
                }
                if !text.isEmpty {
                    Text(text)
                        .font(.subheadline)
                        .foregroundColor(theme.colors.onSurface)
                }
            case let .file(filename):
                HStack(spacing: 6) {
                    Image(systemName: "doc.fill")
                        .foregroundColor(theme.colors.primary)
                    Text(filename)
                        .font(.subheadline)
                        .lineLimit(1)
                        .foregroundColor(theme.colors.onSurface)
                }
            case let .link(text, _):
                Text(text)
                    .font(.subheadline)
                    .foregroundColor(theme.colors.primary)
            default:
                Text(mc.text)
                    .font(.subheadline)
                    .foregroundColor(theme.colors.onSurface)
            }
        }
    }
}
