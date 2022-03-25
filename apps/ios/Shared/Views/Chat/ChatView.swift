//
//  ChatView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 27/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ChatView: View {
    @EnvironmentObject var chatModel: ChatModel
    @Environment(\.colorScheme) var colorScheme
    @ObservedObject var chat: Chat
    @State var quotedItem: ChatItem? = nil
    @State var editingItem: ChatItem? = nil
    @State private var inProgress: Bool = false
    @FocusState private var keyboardVisible: Bool
    @State private var showChatInfo = false

    var body: some View {
        let cInfo = chat.chatInfo

        return VStack {
            GeometryReader { g in
                let maxWidth = g.size.width * 0.78
                ScrollViewReader { proxy in
                    ScrollView {
                        LazyVStack(spacing: 5)  {
                            ForEach(chatModel.chatItems) { ci in
                                let alignment: Alignment = ci.chatDir.sent ? .trailing : .leading
                                ChatItemView(chatItem: ci)
                                    .contextMenu {
                                        Button {
                                            withAnimation {
                                                editingItem = nil
                                                quotedItem = ci
                                            }
                                        } label: { Label("Reply", systemImage: "arrowshape.turn.up.left") }
                                        Button {
                                            showShareSheet(items: [ci.content.text])
                                        } label: { Label("Share", systemImage: "square.and.arrow.up") }
                                        Button {
                                            UIPasteboard.general.string = ci.content.text
                                        } label: { Label("Copy", systemImage: "doc.on.doc") }
                                        if (ci.chatDir.sent && ci.meta.editable) {
                                            Button {
                                                withAnimation {
                                                    quotedItem = nil
                                                    editingItem = ci
                                                }
                                            } label: { Label("Edit", systemImage: "square.and.pencil") }
                                        }
                                    }
                                    .padding(.horizontal)
                                    .frame(maxWidth: maxWidth, maxHeight: .infinity, alignment: alignment)
                                    .frame(minWidth: 0, maxWidth: .infinity, alignment: alignment)
                            }
                            .onAppear {
                                DispatchQueue.main.async {
                                    scrollToFirstUnread(proxy)
                                }
                                markAllRead()
                            }
                            .onChange(of: chatModel.chatItems.count) { _ in
                                scrollToBottom(proxy)
                            }
                            .onChange(of: keyboardVisible) { _ in
                                if keyboardVisible {
                                    DispatchQueue.main.asyncAfter(deadline: .now() + 0.25) {
                                        scrollToBottom(proxy, animation: .easeInOut(duration: 1))
                                    }
                                }
                            }
                        }
                    }
                    .onTapGesture {
                        UIApplication.shared.sendAction(#selector(UIResponder.resignFirstResponder), to: nil, from: nil, for: nil)
                    }
                }
            }

            Spacer(minLength: 0)

            ComposeView(
                quotedItem: $quotedItem,
                editingItem: $editingItem,
                sendMessage: sendMessage,
                inProgress: inProgress,
                keyboardVisible: $keyboardVisible
            )
        }
        .navigationTitle(cInfo.chatViewName)
        .navigationBarTitleDisplayMode(.inline)
        .toolbar {
            ToolbarItem(placement: .navigationBarLeading) {
                Button { chatModel.chatId = nil } label: {
                    HStack(spacing: 4) {
                        Image(systemName: "chevron.backward")
                        Text("Chats")
                    }
                }
            }
            ToolbarItem(placement: .principal) {
                Button {
                    showChatInfo = true
                } label: {
                    ChatInfoToolbar(chat: chat)
                }
                .sheet(isPresented: $showChatInfo) {
                    ChatInfoView(chat: chat, showChatInfo: $showChatInfo)
                }
            }
        }
        .navigationBarBackButtonHidden(true)
    }

    func scrollToBottom(_ proxy: ScrollViewProxy, animation: Animation = .default) {
        withAnimation(animation) { scrollToBottom_(proxy) }
    }

    func scrollToBottom_(_ proxy: ScrollViewProxy) {
        if let id = chatModel.chatItems.last?.id {
            proxy.scrollTo(id, anchor: .bottom)
        }
    }

    // align first unread with the top or the last unread with bottom
    func scrollToFirstUnread(_ proxy: ScrollViewProxy) {
        if let cItem = chatModel.chatItems.first(where: { $0.isRcvNew() }) {
            proxy.scrollTo(cItem.id)
        } else {
            scrollToBottom_(proxy)
        }
    }

    func markAllRead() {
        DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
            if chatModel.chatId == chat.id {
                Task { await markChatRead(chat) }
            }
        }
    }

    func sendMessage(_ msg: String) {
        Task {
            do {
                if let ei = editingItem {
                    let chatItem = try await apiUpdateMessage(
                        type: chat.chatInfo.chatType,
                        id: chat.chatInfo.apiId,
                        itemId: ei.id,
                        msg: .text(msg)
                    )
                    DispatchQueue.main.async {
                        editingItem = nil
                        let _ = chatModel.upsertChatItem(chat.chatInfo, chatItem)
                    }
                } else {
                    let chatItem = try await apiSendMessage(
                        type: chat.chatInfo.chatType,
                        id: chat.chatInfo.apiId,
                        quotedItemId: quotedItem?.meta.itemId,
                        msg: .text(msg)
                    )
                    DispatchQueue.main.async {
                        quotedItem = nil
                        chatModel.addChatItem(chat.chatInfo, chatItem)
                    }
                }
            } catch {
                logger.error("ChatView.sendMessage error: \(error.localizedDescription)")
            }
        }
    }
}

struct ChatView_Previews: PreviewProvider {
    static var previews: some View {
        let chatModel = ChatModel()
        chatModel.chatId = "@1"
        chatModel.chatItems = [
            ChatItem.getSample(1, .directSnd, .now, "hello"),
            ChatItem.getSample(2, .directRcv, .now, "hi"),
            ChatItem.getSample(3, .directRcv, .now, "hi there"),
            ChatItem.getSample(4, .directRcv, .now, "hello again"),
            ChatItem.getSample(5, .directSnd, .now, "hi there!!!"),
            ChatItem.getSample(6, .directSnd, .now, "how are you?"),
            ChatItem.getSample(7, .directSnd, .now, "👍👍👍👍"),
            ChatItem.getSample(8, .directSnd, .now, "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
        ]
        return ChatView(chat: Chat(chatInfo: ChatInfo.sampleData.direct, chatItems: []))
            .environmentObject(chatModel)
    }
}
