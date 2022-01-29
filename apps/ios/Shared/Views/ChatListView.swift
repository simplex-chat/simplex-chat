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
                VStack {
                    Text("Hello chat")
                    Text("Active user: \(user.localDisplayName) (\(user.profile.fullName))")
                }
//            }
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
                                        let chat = try apiGetChatItems(type: .direct, id: chatPreview.chatInfo.apiId)
                                        chatModel.chats[chat.chatInfo.id] = chat
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
//        ChatListView()
//    }
//}
