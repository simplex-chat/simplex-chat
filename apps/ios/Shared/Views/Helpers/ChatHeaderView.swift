//
//  ChatHeaderView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 29/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ChatHeaderView: View {
    @Binding var chatId: String?
    @EnvironmentObject var chatModel: ChatModel

    var body: some View {
        HStack {
            if let cId = chatId {
                Button { chatId = nil } label: { Image(systemName: "chevron.backward") }
                Spacer()
                Text(chatModel.chats[cId]?.chatInfo.localDisplayName ?? "")
                Spacer()
                EmptyView()
            } else {
//                Button("Edit", action: {})
                EmptyView()
                Spacer()
                Text("Your chats")
                Spacer()
                NewChatButton()
            }
        }
        .padding([.horizontal, .top])
    }
}

struct ChatHeaderView_Previews: PreviewProvider {
    static var previews: some View {
        @State var chatId1: String? = "@1"
        @State var chatId2: String?
        let chatModel = ChatModel()
        chatModel.chats = [
            "@1": Chat(
                chatInfo: sampleDirectChatInfo,
                chatItems: [chatItemSample(1, .directSnd, Date.now, "hello")]
            )
        ]
        return Group {
            ChatHeaderView(chatId: $chatId1)
            ChatHeaderView(chatId: $chatId2)
        }
        .previewLayout(.fixed(width: 300, height: 70))
        .environmentObject(chatModel)
    }
}
