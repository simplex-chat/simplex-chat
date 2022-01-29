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
    @State var inProgress: Bool = false

    var chatInfo: ChatInfo
    var body: some View {
        VStack {
            if let chat: Chat = chatModel.chats[chatInfo.id] {
                VStack {
                    ScrollView {
                        LazyVStack {
                            ForEach(chat.chatItems) { chatItem in
                                Text(chatItem.content.text)
                            }
                        }
                    }
                }
            } else {
                Text("unexpected: chat not found...")
            }
            
            Spacer()

            SendMessageView(sendMessage: sendMessage, inProgress: inProgress)
        }
    }

    func sendMessage(_ msg: String) {

    }
}

struct ChatView_Previews: PreviewProvider {
    static var previews: some View {
        let chatModel = ChatModel()
        chatModel.chats = [
            "@1": Chat(
                chatInfo: sampleDirectChatInfo,
                chatItems: []
            )
        ]
        return ChatView(chatInfo: sampleDirectChatInfo)
            .environmentObject(chatModel)
    }
}
