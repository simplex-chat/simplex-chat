//
//  TimelineView.swift
//  SimpleX (iOS)
//
//  Created by Levitating Pineapple on 16/06/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

extension Timeline {
    struct TimelineView: View {
        @EnvironmentObject var chatModel: ChatModel
        @ObservedObject var chat: Chat
        @Binding var composeState: ComposeState

        @State private var items = Array<Item>()
        @State private var expanded = Set<ChatItem.ID>()
        @State private var loadState = LoadState.ready
        @State private var sheet: Sheet?
        @State private var deleteItems: Array<ChatItem>?
        @State private var scroll: ReverseList<Item, ItemView>.Scroll = .isNearBottom(true)

        var body: some View {
            ZStack(alignment: .bottomTrailing) {
                ReverseList(items: items, scroll: $scroll) { item in
                    Timeline.ItemView(
                        item: item,
                        chat: chat,
                        expandedItems: $expanded,
                        composeState: $composeState,
                        sheet: $sheet,
                        deleteItems: $deleteItems
                    )
                } loadedCell: {
                    if items.count >= Timeline.pageLoad,
                       items.count - $0.item < Timeline.pageLoad {
                        loadPage(chatInfo: chat.chatInfo)
                    }
                }
                if !scroll.isAtBottom { scrollToBottomButton }
            }

            // Data binding
            .onChange(of: chatModel.reversedChatItems) { update(chatItems: $0) }
            .onChange(of: expanded) { update(expanded: $0) }
            .onAppear { update() }

            // Modals
            .sheet(item: $sheet) { SheetView(sheet: $0, chat: chat, composeState: $composeState) }
            .confirmationDialog(
                "Remove Message?",
                isPresented: isDialogPresented,
                presenting: deleteItems
            ) { deleteButtons(items: $0) }
        }

        private var scrollToBottomButton: some View {
            Button {
                scroll = .scrollingToBottom
            } label: {
                Image(systemName: "chevron.down")
                    .imageScale(.large).offset(y: 1)
                    .frame(width: 44, height: 44)
                    .background(Material.ultraThin)
                    .clipShape(Circle())
                    .shadow(radius: 2)
                    .padding()
            }
        }

        private func update(
            chatItems: Array<ChatItem>? = nil,
            expanded: Set<ChatItem.ID>? = nil
        ) {
            Task {
                let items = Array<Item>(
                    reversedChatItems: chatItems ?? chatModel.reversedChatItems,
                    expanded: expanded ?? self.expanded
                )
                await MainActor.run { self.items = items }
            }
        }
    }
}

// MARK: Page Loading
extension Timeline.TimelineView {
    /// ┌─────┐ ─► ┌───────┐    ┌──────┐
    /// │ready│    │loading│ ─► │loaded│
    /// └─────┘ ◄─ └───────┘    └──────┘
    enum LoadState: String {
        /// Ready to load next page
        case ready
        /// Page load in progress
        case loading
        /// The timeline has been fully loaded
        case loaded
    }

    private func loadPage(chatInfo: ChatInfo) {
        if loadState != .ready { return }
        Task {
            do {
                loadState = .loading
                let items = try await apiGetChatItems(
                    type: chatInfo.chatType,
                    id: chatInfo.apiId,
                    pagination: chatModel.reversedChatItems.last.flatMap {
                        ChatPagination .before(chatItemId: $0.id, count: Timeline.pageSize)
                    } ?? .last(count: Timeline.pageSize)
                )
                if items.isEmpty {
                    loadState = .loaded
                } else {
                    await MainActor.run {
                        chatModel.reversedChatItems.append(contentsOf: items.reversed())
                        loadState = .ready
                    }
                }
            } catch let error {
                logger.error("apiGetChat error: \(responseError(error))")
            }
        }
    }
}

// MARK: Chat Item Deletion
extension Timeline.TimelineView {
    @ViewBuilder
    func deleteButtons(items: Array<ChatItem>) -> some View {
        // Removes chat item(s) locally
        Button("Delete for me", role: .destructive) {
            Task { await delete(items: items, mode: .cidmInternal) }
        }
        // Broadcast delete
        if items.count == 1, let di = items.first, di.meta.deletable && !di.localNote {
            Button(deleteButtonTitle(chatItem: di, chat: chat), role: .destructive) {
                Task { await delete(items: [di], mode: .cidmBroadcast) }
            }
        }
    }

    var isDialogPresented: Binding<Bool> {
        Binding(
            get: { deleteItems != nil },
            set: { if !$0 { deleteItems = nil } }
        )
    }

    private func deleteButtonTitle(chatItem: ChatItem, chat: Chat) -> String {
        if let (groupInfo, _) = chatItem.memberToModerate(chat.chatInfo) {
            // Group chat
            groupInfo.fullGroupPreferences.fullDelete.on
            ? "Delete member message"
            : "Moderate member message"
        } else {
            // Direct message
            chat.chatInfo.featureEnabled(.fullDelete)
            ? "Delete for everyone"
            : "Mark deleted for everyone"
        }
    }

    private struct Deleted {
        let item: ChatItem
        let totem: ChatItem?

        init(_ tuple: (ChatItem, ChatItem?)) {
            self.item = tuple.0
            self.totem = tuple.1
        }

        func update(model: ChatModel, info: ChatInfo) {
            if let totem {
                _ = model.upsertChatItem(info, totem)
            } else {
                model.removeChatItem(info, item)
            }
        }
    }

    private func delete(items: Array<ChatItem>, mode: CIDeleteMode) async {
        do {
            var deleted = Array<Deleted>()
            for item in items {
                if case .cidmBroadcast = mode,
                   let (groupInfo, groupMember) = item.memberToModerate(chat.chatInfo) {
                    deleted.append(
                        Deleted(
                            try await apiDeleteMemberChatItem(
                                groupId: groupInfo.apiId,
                                groupMemberId: groupMember.groupMemberId,
                                itemId: item.id
                            )
                        )
                    )

                } else {
                    deleted.append(
                        Deleted(
                            try await apiDeleteChatItem(
                                type: chat.chatInfo.chatType,
                                id: chat.chatInfo.apiId,
                                itemId: item.id,
                                mode: mode
                            )
                        )
                    )
                }
            }
            await MainActor.run {
                deleted.forEach { $0.update(model: chatModel, info: chat.chatInfo) }
            }
        } catch {
            logger.error("ChatView.deleteMessage error: \(error.localizedDescription)")
        }
    }
}
