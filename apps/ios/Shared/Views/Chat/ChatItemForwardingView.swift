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
    @EnvironmentObject var theme: AppTheme
    @Environment(\.dismiss) var dismiss

    var ci: ChatItem
    var fromChatInfo: ChatInfo
    @Binding var composeState: ComposeState

    @State private var searchText: String = ""
    @FocusState private var searchFocused
    @State private var alert: SomeAlert?
    private let chatsToForwardTo = filterChatsToForwardTo(chats: ChatModel.shared.chats)

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
        .modifier(ThemedBackground())
        .alert(item: $alert) { $0.alert }
    }

    @ViewBuilder private func forwardListView() -> some View {
        VStack(alignment: .leading) {
            if !chatsToForwardTo.isEmpty {
                List {
                    searchFieldView(text: $searchText, focussed: $searchFocused, theme.colors.onBackground, theme.colors.secondary)
                        .padding(.leading, 2)
                    let s = searchText.trimmingCharacters(in: .whitespaces).localizedLowercase
                    let chats = s == "" ? chatsToForwardTo : chatsToForwardTo.filter { foundChat($0, s) }
                    ForEach(chats) { chat in
                        forwardListChatView(chat)
                            .disabled(chatModel.deletedChats.contains(chat.chatInfo.id))
                    }
                }
                .modifier(ThemedBackground(grouped: true))
            } else {
                ZStack {
                    emptyList()
                }
                .frame(maxWidth: .infinity, maxHeight: .infinity)
                .modifier(ThemedBackground())
            }
        }
    }

    private func emptyList() -> some View {
        Text("No filtered chats")
            .foregroundColor(theme.colors.secondary)
            .frame(maxWidth: .infinity)
    }

    @ViewBuilder private func forwardListChatView(_ chat: Chat) -> some View {
        let prohibited = chat.prohibitedByPref(
            hasSimplexLink: hasSimplexLink(ci.content.msgContent?.text),
            isMediaOrFileAttachment: ci.content.msgContent?.isMediaOrFileAttachment ?? false,
            isVoice: ci.content.msgContent?.isVoice ?? false
        )
        Button {
            if prohibited {
                alert = SomeAlert(
                    alert: mkAlert(
                        title: "Cannot forward message",
                        message: "Selected chat preferences prohibit this message."
                    ),
                    id: "forward prohibited by preferences"
                )
            } else {
                dismiss()
                if chat.id == fromChatInfo.id {
                    composeState = ComposeState(
                        message: composeState.message,
                        preview: composeState.linkPreview != nil ? composeState.preview : .noPreview,
                        contextItem: .forwardingItem(chatItem: ci, fromChatInfo: fromChatInfo)
                    )
                } else {
                    composeState = ComposeState.init(forwardingItem: ci, fromChatInfo: fromChatInfo)
                    chatModel.chatId = chat.id
                }
            }
        } label: {
            HStack {
                ChatInfoImage(chat: chat, size: 30)
                    .padding(.trailing, 2)
                Text(chat.chatInfo.chatViewName)
                    .foregroundColor(prohibited ? theme.colors.secondary : theme.colors.onBackground)
                    .lineLimit(1)
                if chat.chatInfo.incognito {
                    Spacer()
                    Image(systemName: "theatermasks")
                        .resizable()
                        .scaledToFit()
                        .frame(width: 22, height: 22)
                        .foregroundColor(theme.colors.secondary)
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
    ).environmentObject(CurrentColors.toAppTheme())
}

