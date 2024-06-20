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
    @State private var alert: SomeAlert?
    @State private var hasSimplexLink_: Bool?
    private let chatsToForwardTo = filterChatsToForwardTo()

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
                    searchFieldView(text: $searchText, focussed: $searchFocused)
                        .padding(.leading, 2)
                    let s = searchText.trimmingCharacters(in: .whitespaces).localizedLowercase
                    let chats = s == "" ? chatsToForwardTo : chatsToForwardTo.filter { foundChat($0, s) }
                    ForEach(chats) { chat in
                        forwardListChatView(chat)
                            .disabled(chatModel.deletedChats.contains(chat.chatInfo.id))
                    }
                }
                .modifier(ThemedBackground())
            } else {
                ZStack {
                    emptyList()
                }
                .frame(maxWidth: .infinity, maxHeight: .infinity)
                .modifier(ThemedBackground())
            }
        }
    }

    private func foundChat(_ chat: Chat, _ searchStr: String) -> Bool {
        let cInfo = chat.chatInfo
        return switch cInfo {
        case let .direct(contact):
            viewNameContains(cInfo, searchStr) ||
            contact.profile.displayName.localizedLowercase.contains(searchStr) ||
            contact.fullName.localizedLowercase.contains(searchStr)
        default:
            viewNameContains(cInfo, searchStr)
        }

        func viewNameContains(_ cInfo: ChatInfo, _ s: String) -> Bool {
            cInfo.chatViewName.localizedLowercase.contains(s)
        }
    }

    private func prohibitedByPref(_ chat: Chat) -> Bool {
        // preference checks should match checks in compose view
        let simplexLinkProhibited = hasSimplexLink && !chat.groupFeatureEnabled(.simplexLinks)
        let fileProhibited = (ci.content.msgContent?.isMediaOrFileAttachment ?? false) && !chat.groupFeatureEnabled(.files)
        let voiceProhibited = (ci.content.msgContent?.isVoice ?? false) && !chat.chatInfo.featureEnabled(.voice)
        return switch chat.chatInfo {
        case .direct: voiceProhibited
        case .group: simplexLinkProhibited || fileProhibited || voiceProhibited
        case .local: false
        case .contactRequest: false
        case .contactConnection: false
        case .invalidJSON: false
        }
    }

    private var hasSimplexLink: Bool {
        if let hasSimplexLink_ { return hasSimplexLink_ }
        let r =
            if let mcText = ci.content.msgContent?.text,
               let parsedMsg = parseSimpleXMarkdown(mcText) {
                parsedMsgHasSimplexLink(parsedMsg)
            } else {
                false
            }
        hasSimplexLink_ = r
        return r
    }

    private func emptyList() -> some View {
        Text("No filtered chats")
            .foregroundColor(.secondary)
            .frame(maxWidth: .infinity)
    }

    @ViewBuilder private func forwardListChatView(_ chat: Chat) -> some View {
        let prohibited = prohibitedByPref(chat)
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
                    .foregroundColor(prohibited ? .secondary : .primary)
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

private func filterChatsToForwardTo() -> [Chat] {
    var filteredChats = ChatModel.shared.chats.filter { c in
        c.chatInfo.chatType != .local && canForwardToChat(c)
    }
    if let privateNotes = ChatModel.shared.chats.first(where: { $0.chatInfo.chatType == .local }) {
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

#Preview {
    ChatItemForwardingView(
        ci: ChatItem.getSample(1, .directSnd, .now, "hello"),
        fromChatInfo: .direct(contact: Contact.sampleData),
        composeState: Binding.constant(ComposeState(message: "hello"))
    )
}
