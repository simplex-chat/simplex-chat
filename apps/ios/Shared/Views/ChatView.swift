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
        }
    }
}

//struct ChatView_Previews: PreviewProvider {
//    static var previews: some View {
//        ChatView()
//    }
//}
