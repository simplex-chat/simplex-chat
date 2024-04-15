//
//  ChatItemForwardingView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 12.04.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ChatItemForwardingView: View {
    @EnvironmentObject var chatModel: ChatModel
    @Environment(\.dismiss) var dismiss

    var ci: ChatItem
    var fromChatInfo: ChatInfo
    @Binding var composeState: ComposeState

    @State private var searchText: String = ""
    @FocusState private var searchFocused

    var body: some View {
        NavigationView {
            forwardListView()
                .toolbar {
                    ToolbarItem(placement: .navigationBarLeading) {
                        Button("Cancel") {
                            dismiss()
                        }
                    }
                    ToolbarItem(placement: .principal) {
                        Text("Forward")
                            .bold()
                    }
                }
        }
    }

    @ViewBuilder private func forwardListView() -> some View {
        VStack(alignment: .leading) {
            let chatsToForwardTo = filterChatsToForwardTo()
            if !chatsToForwardTo.isEmpty {
                ScrollView {
                    LazyVStack(alignment: .leading, spacing: 8) {
                        searchFieldView(text: $searchText, focussed: $searchFocused)
                            .padding(.leading, 2)
                        let s = searchText.trimmingCharacters(in: .whitespaces).localizedLowercase
                        let chats = s == "" ? chatsToForwardTo : chatsToForwardTo.filter { $0.chatInfo.chatViewName.localizedLowercase.contains(s) }
                        ForEach(chats) { chat in
                            Divider()
                            forwardListNavLinkView(chat)
                                .disabled(chatModel.deletedChats.contains(chat.chatInfo.id))
                        }
                    }
                    .padding(.horizontal)
                    .padding(.vertical, 8)
                    .background(Color(uiColor: .systemBackground))
                    .cornerRadius(12)
                    .padding(.horizontal)
                }
                .background(Color(.systemGroupedBackground))
            } else {
                emptyList()
            }
        }
    }

    private func filterChatsToForwardTo() -> [Chat] {
        var filteredChats = chatModel.chats.filter({ canForwardToChat($0) })
        if let index = filteredChats.firstIndex(where: { $0.chatInfo.chatType == .local }) {
            let privateNotes = filteredChats.remove(at: index)
            filteredChats.insert(privateNotes, at: 0)
        }
        return filteredChats
    }

    private func canForwardToChat(_ chat: Chat) -> Bool {
        switch chat.chatInfo {
        case let .direct(contact): contact.sendMsgEnabled && !contact.nextSendGrpInv
        case let .group(groupInfo): groupInfo.sendMsgEnabled
        case let .local(noteFolder): noteFolder.sendMsgEnabled
        case .contactRequest: false
        case .contactConnection: false
        case .invalidJSON: false
        }
    }

    private func emptyList() -> some View {
        Text("No filtered chats")
            .foregroundColor(.secondary)
            .frame(maxWidth: .infinity)
    }

    @ViewBuilder private func forwardListNavLinkView(_ chat: Chat) -> some View {
        Button {
            Task {
                await MainActor.run {
                    if chat.id != fromChatInfo.id {
                        chatModel.forwardToChatId = chat.id
                        chatModel.forward = ComposeState.init(forwardingItem: ci, fromChatInfo: fromChatInfo)
                        dismiss()
                        chatModel.chatId = chat.id
                    } else {
                        let currentComposeState = composeState
                        composeState = ComposeState(
                            message: currentComposeState.message,
                            preview: currentComposeState.linkPreview != nil ? currentComposeState.preview : .noPreview,
                            contextItem: .forwardingItem(chatItem: ci, fromChatInfo: fromChatInfo)
                        )
                        dismiss()
                    }
                }
            }
        } label: {
            HStack {
                ChatInfoImage(chat: chat)
                    .frame(width: 30, height: 30)
                    .padding(.trailing, 2)
                Text(chat.chatInfo.chatViewName)
                    .foregroundColor(.primary)
                    .lineLimit(1)
                if chat.chatInfo.incognito {
                    Spacer()
                    Image(systemName: "theatermasks")
                        .resizable()
                        .scaledToFit()
                        .frame(width: 22, height: 22)
                        .foregroundColor(.secondary)
                }
            }
            .frame(maxWidth: .infinity, alignment: .leading)
        }
    }
}

#Preview {
    ChatItemForwardingView(
        ci: ChatItem.getSample(1, .directSnd, .now, "hello"),
        fromChatInfo: .direct(contact: Contact.sampleData),
        composeState: Binding.constant(ComposeState(message: "hello"))
    )
}
