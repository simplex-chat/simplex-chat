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
            while(true) { processAPIResponse(chatModel, chatRecvMsg()) }
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
                
                    ForEach(chatModel.chatPreviews, id: \.self) { cp in
                        NavigationLink {
//                            ChatView(chat: chat)
                            Text("hello")
                        } label: {
                            ChatPreviewView(chatPreview: cp)
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
