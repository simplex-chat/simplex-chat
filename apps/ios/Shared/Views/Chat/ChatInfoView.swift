//
//  ChatInfoView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 05/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ChatInfoView: View {
    @ObservedObject var chat: Chat

    var body: some View {
        VStack{
            ChatInfoImage(chat: chat)
                .frame(width: 192, height: 192)
                .padding(.top, 48)
                .padding()
            Text(chat.chatInfo.localDisplayName).font(.largeTitle)
                .padding(.bottom, 2)
            Text(chat.chatInfo.fullName).font(.title)
                .padding(.bottom)

            if case .direct = chat.chatInfo {
                HStack {
                    serverImage()
                    Text(chat.serverInfo.networkStatus.statusString)
                }
            }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
    }

    func serverImage() -> some View {
        let status = chat.serverInfo.networkStatus
        return Image(systemName: status.imageName)
            .foregroundColor(status == .connected ? .green : .secondary)
    }
}

struct ChatInfoView_Previews: PreviewProvider {
    var chatInfo = sampleDirectChatInfo

    static var previews: some View {
        ChatInfoView(chat: Chat(chatInfo: sampleDirectChatInfo, chatItems: []))
    }
}
