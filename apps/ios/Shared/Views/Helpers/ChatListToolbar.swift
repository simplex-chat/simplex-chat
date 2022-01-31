//
//  ChatListToolbar.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 29/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ChatListToolbar: View {
    @EnvironmentObject var chatModel: ChatModel
    var width: CGFloat

    var body: some View {
        HStack {
            SettingsButton()
            Spacer()
            Text("Your chats")
                .font(.title3)
            Spacer()
            NewChatButton()
        }
        .padding(.horizontal)
        .frame(minWidth: width, maxWidth: .infinity, alignment: .center)
    }
}

struct ChatListToolbar_Previews: PreviewProvider {
    static var previews: some View {
        let chatModel = ChatModel()
        chatModel.chats = [
            "@1": Chat(
                chatInfo: sampleDirectChatInfo,
                chatItems: [chatItemSample(1, .directSnd, Date.now, "hello")]
            )
        ]
        return ChatListToolbar(width: 300)
            .previewLayout(.fixed(width: 300, height: 70))
            .environmentObject(chatModel)
    }
}
