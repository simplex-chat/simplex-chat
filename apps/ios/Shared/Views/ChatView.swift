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
    var width: CGFloat
    @State private var inProgress: Bool = false

    var body: some View {
        VStack {
            ScrollView {
                LazyVStack(spacing: 5) {
                    ForEach(chatModel.chatItems) {
                        ChatItemView(chatItem: $0)
                    }
                }
            }

            Spacer(minLength: 0)

            SendMessageView(sendMessage: sendMessage, inProgress: inProgress)
        }
        .toolbar {
            HStack {
                Button { chatModel.chatId = nil } label: {
                    Image(systemName: "chevron.backward")
                }
                Spacer()
                Text(chatInfo.localDisplayName)
                    .font(.title3)
                Spacer()
                EmptyView()
            }
            .padding(.horizontal)
            .frame(minWidth: width, maxWidth: .infinity, alignment: .center)
        }
        .navigationBarBackButtonHidden(true)

    }

    func sendMessage(_ msg: String) {
        do {
            let chatItem = try apiSendMessage(type: chatInfo.chatType, id: chatInfo.apiId, msg: .text(msg))
            chatModel.addChatItem(chatInfo, chatItem)
        } catch {
            print(error)
        }
    }
}

struct ChatView_Previews: PreviewProvider {
    static var previews: some View {
        let chatModel = ChatModel()
        chatModel.chatId = "@1"
        chatModel.chatItems = [
            chatItemSample(1, .directSnd, Date.now, "hello"),
            chatItemSample(2, .directRcv, Date.now, "hi"),
            chatItemSample(3, .directRcv, Date.now, "hi there"),
            chatItemSample(4, .directRcv, Date.now, "hello again"),
            chatItemSample(5, .directSnd, Date.now, "hi there!!!"),
            chatItemSample(6, .directSnd, Date.now, "how are you?"),
            chatItemSample(7, .directSnd, Date.now, "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
        ]
        return ChatView(chatInfo: sampleDirectChatInfo, width: 300)
            .environmentObject(chatModel)
    }
}
