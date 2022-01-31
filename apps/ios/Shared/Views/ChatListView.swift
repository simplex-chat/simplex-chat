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
    var user: User

    var body: some View {
        DispatchQueue.global().async {
            while(true) {
                do {
                    try processReceivedMsg(chatModel, chatRecvMsg())
                } catch {
                    print("error receiving message: ", error)
                }
            }
        }

        return VStack {
//            if chatModel.chats.isEmpty {
//                VStack {
//                    Text("Hello chat")
//                    Text("Active user: \(user.localDisplayName) (\(user.profile.fullName))")
//                }
//            }

            ChatHeaderView()

            NavigationView {
                List {
                    NavigationLink {
                        TerminalView()
                    } label: {
                        Text("Terminal")
                    }

                    ForEach(chatModel.chatPreviews) { chatPreview in
                        NavigationLink {
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
                        } label: {
                            ChatPreviewView(chatPreview: chatPreview)
                        }
                        .frame(height: 60)
                    }
                }
            }
            .navigationViewStyle(.automatic)
        }
    }
}

//struct ChatListView_Previews: PreviewProvider {
//    static var previews: some View {
//        let chatModel = ChatModel()
//        chatModel.chatPreviews = [
//            ChatPreview(
//                chatInfo: sampleDirectChatInfo,
//                lastChatItem: chatItemSample(1, .directSnd, Date.now, "hello")
//            ),
//            ChatPreview(
//                chatInfo: sampleGroupChatInfo,
//                lastChatItem: chatItemSample(1, .directSnd, Date.now, "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
//            )
//        ]
//        return ChatListView(user: sampleUser)
//            .environmentObject(chatModel)
//    }
//}
