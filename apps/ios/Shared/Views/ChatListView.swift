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
                    }
                }
            }
            .navigationViewStyle(.stack)
        }
    }
}

//struct ChatListView_Previews: PreviewProvider {
//    static var previews: some View {
//        let chatModel = ChatModel()
//        chatModel.chatPreviews = []
//        return ChatListView(user: sampleUser)
//            .environmentObject(chatModel)
//    }
//}
