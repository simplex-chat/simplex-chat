//
//  ReverseList.ContentView.swift
//  SimpleX (iOS)
//
//  Created by Levitating Pineapple on 12/06/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

extension Timeline {
    /// Renders a single timeline item
    struct ItemView: View {
        let item: Timeline.Item
        @EnvironmentObject var chatModel: ChatModel
        @ObservedObject var chat: Chat
        @Binding var expandedItems: Set<ChatItem.ID>
        @Binding var composeState: ComposeState
        @Binding var sheet: Sheet?
        @Binding var deleteItems: Array<ChatItem>?

        @State var audioPlayer: AudioPlayer?
        @State var playbackState: VoiceMessagePlaybackState = .noPlayback
        @State var playbackTime: TimeInterval?

        var body: some View {
            VStack(alignment: .leading, spacing: 4) {
                memberNames
                HStack(alignment: .top, spacing: 8) {
                    profileImageView
                    VStack {
                        if item.isExpanded {
                            ForEach(item.mergedChatItems.reversed()) { chatItem in
                                view(chatItem: chatItem)
                            }
                        }
                        view(chatItem: item.chatItem)
                    }
                }
            }
            .padding(.top, item.isDirectionPersisted ? .zero : 8)
            .padding(.horizontal, 8)
            .padding(.bottom, 4)
        }

        // TODO: Clarify merged item member names
        private var memberNames: some View {
            Group {
                if !item.isDirectionPersisted && item.chatItem.content.showMemberName,
                   let groupMember = item.chatItem.chatDir.groupMember {
                    Text(groupMember.displayName)
                        .font(.caption)
                        .foregroundStyle(.secondary)
                        .lineLimit(2)
                        .padding(.leading, Double(Timeline.profileImageSize) + 14)
                        .padding(.top, 8)
                }
            }
        }

        private var profileImageView: some View {
            Group {
                if let groupMember = item.chatItem.chatDir.groupMember {
                    if item.isDirectionPersisted {
                        Spacer(minLength: Double(Timeline.profileImageSize))
                    } else {
                        ProfileImage(
                            imageStr: groupMember.memberProfile.image,
                            size: Double(Timeline.profileImageSize)
                        ).onTapGesture {
                            Task {
                                if let gMember = chatModel.getGroupMember(groupMember.groupMemberId) {
                                    await MainActor.run { sheet = .memberInfo(gMember) }
                                }
                            }
                        }
                    }
                }
            }
        }

        private func view(chatItem: ChatItem) -> some View {
            AlignmentContainer(alignment: chatItem.alignment) {
                VStack(alignment: chatItem.horizontalAlignment, spacing: 2) {
                    ChatItemView(
                        chat: chat,
                        chatItem: chatItem,
                        revealed: .constant(item.isExpanded),
                        allowMenu: .constant(true),
                        audioPlayer: $audioPlayer,
                        playbackState: $playbackState,
                        playbackTime: $playbackTime
                    ).contextMenu {
                        ChatItemMenu(
                            chat: chat,
                            timelineItem: item,
                            mergedIndex: item.mergedChatItems.firstIndex(of: chatItem),
                            composeState: $composeState,
                            expandedItems: $expandedItems,
                            sheet: $sheet,
                            deleteItems: $deleteItems
                        )
                    }
                    reactions(chatItem: item.chatItem)
                }
            }
            .task { await chatItem.markAsRead(chatInfo: chat.chatInfo) }
        }

        private func reactions(chatItem: ChatItem) -> some View {
            Group {
                if chatItem.content.msgContent != nil &&
                  (chatItem.meta.itemDeleted == nil || item.isExpanded) &&
                   chatItem.reactions.count > 0 {
                    HStack(spacing: 4) {
                        ForEach(chatItem.reactions, id: \.reaction) { r in
                            let v = HStack(spacing: 4) {
                                switch r.reaction {
                                case let .emoji(emoji): Text(emoji.rawValue).font(.caption)
                                case .unknown: EmptyView()
                                }
                                if r.totalReacted > 1 {
                                    Text("\(r.totalReacted)")
                                        .font(.caption)
                                        .fontWeight(r.userReacted ? .bold : .light)
                                        .foregroundColor(r.userReacted ? .accentColor : .secondary)
                                }
                            }
                                .padding(.horizontal, 6)
                                .padding(.vertical, 4)
                            if chat.chatInfo.featureEnabled(.reactions) && (chatItem.allowAddReaction || r.userReacted) {
                                v.onTapGesture {
                                    Task {
                                        await chatModel.set(
                                            reaction: r.reaction,
                                            for: item.chatItem,
                                            chatInfo: chat.chatInfo,
                                            add: true
                                        )
                                    }
                                }
                            } else {
                                v
                            }
                        }
                    }
                }
            }
        }
    }
}

extension ChatItem {
    fileprivate var alignment: Alignment {
        chatDir.sent ? .trailing : .leading
    }

    fileprivate var horizontalAlignment: HorizontalAlignment {
        chatDir.sent ? .trailing : .leading
    }

    fileprivate func markAsRead(chatInfo: ChatInfo) async {
        // TODO: As there can be many items, these calls sould be debounced
        if isRcvNew { await apiMarkChatItemRead(chatInfo, self) }
    }
}
