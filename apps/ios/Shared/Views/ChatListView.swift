//
//  ChatListView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 27/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ChatListView: View {
    @EnvironmentObject var chatModel: ChatModel
    @State private var chatId: String?
    @State private var chatsToBeDeleted: IndexSet?
    @State private var showDeleteAlert = false

    var user: User

    var body: some View {
        return VStack {
//            if chatModel.chats.isEmpty {
//                VStack {
//                    Text("Hello chat")
//                    Text("Active user: \(user.localDisplayName) (\(user.profile.fullName))")
//                }
//            }

            ChatHeaderView(chatId: $chatId)

            NavigationView {
                List {
                    NavigationLink {
                        TerminalView()
                    } label: {
                        Text("Terminal")
                    }

                    ForEach(chatModel.chatPreviews) { chatPreview in
                        NavigationLink(tag: chatPreview.chatInfo.id, selection: $chatId, destination: {
                            ChatView(chatInfo: chatPreview.chatInfo)
                                .onAppear {
                                    do {
                                        let ci = chatPreview.chatInfo
                                        let chat = try apiGetChat(type: ci.chatType, id: ci.apiId)
                                        chatModel.chats[ci.id] = chat
                                    } catch {
                                        print("apiGetChatItems", error)
                                    }
                                }
                        }, label: {
                            ChatPreviewView(chatPreview: chatPreview)
                                .alert(isPresented: $showDeleteAlert) {
                                    deleteChatAlert((chatsToBeDeleted?.first)!)
                                }
                        })
                        .frame(height: 80)
                    }
                    .onDelete { idx in
                        chatsToBeDeleted = idx
                        showDeleteAlert = true
                    }
                }
                .padding(0)
                .offset(x: -8)
                .listStyle(.plain)
                .edgesIgnoringSafeArea(.top)
            }
        }
    }

    func deleteChatAlert(_ ix: IndexSet.Element) -> Alert {
        let ci = chatModel.chatPreviews[ix].chatInfo
        switch ci {
        case .direct:
            return Alert(
                title: Text("Delete contact?"),
                message: Text("Contact and all messages will be deleted"),
                primaryButton: .destructive(Text("Delete")) {
                    do {
                        try apiDeleteChat(type: ci.chatType, id: ci.apiId)
                        chatModel.chatPreviews.remove(at: ix)
                    } catch let error {
                        print("Error: \(error)")
                    }
                    chatsToBeDeleted = nil
                }, secondaryButton: .cancel() {
                    chatsToBeDeleted = nil
                }
            )
        case .group:
            return Alert(
                title: Text("Delete group"),
                message: Text("Group deletion is not supported")
            )
        }
    }
}

struct ChatListView_Previews: PreviewProvider {
    static var previews: some View {
        let chatModel = ChatModel()
        chatModel.chatPreviews = [
            Chat(
                chatInfo: sampleDirectChatInfo,
                chatItems: [chatItemSample(1, .directSnd, Date.now, "hello")]
            ),
            Chat(
                chatInfo: sampleGroupChatInfo,
                chatItems: [chatItemSample(1, .directSnd, Date.now, "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")]
            )
        ]
        return ChatListView(user: sampleUser)
            .environmentObject(chatModel)
    }
}
