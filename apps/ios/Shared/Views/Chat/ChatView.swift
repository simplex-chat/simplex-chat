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
    @State private var inProgress: Bool = false
    @FocusState private var keyboardVisible: Bool
    @State private var showChatInfo = false

    var body: some View {
        let cInfo = chat.chatInfo

        return VStack {
            GeometryReader { g in
                ScrollViewReader { proxy in
                    ScrollView {
                        VStack(spacing: 5)  {
                            ForEach(chatModel.chatItems, id: \.id) {
                                ChatItemView(chatItem: $0, width: g.size.width)
                                    .frame(minWidth: 0, maxWidth: .infinity, alignment: $0.chatDir.sent ? .trailing : .leading)
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

            SendMessageView(
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
        DispatchQueue.main.asyncAfter(deadline: .now() + 2) {
            if chatModel.chatId == chat.id {
                markChatRead(chat)
            }
        }
    }

    func sendMessage(_ msg: String) {
        do {
            let chatItem = try apiSendMessage(type: chat.chatInfo.chatType, id: chat.chatInfo.apiId, msg: .text(msg))
            chatModel.addChatItem(chat.chatInfo, chatItem)
        } catch {
            logger.error("ChatView.sendMessage apiSendMessage error: \(error.localizedDescription)")
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
