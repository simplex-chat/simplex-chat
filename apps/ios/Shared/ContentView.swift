//
//  ContentView.swift
//  Shared
//
//  Created by Evgeny Poberezkin on 17/01/2022.
//

import SwiftUI

struct ContentView: View {
    @EnvironmentObject var chatModel: ChatModel

    var body: some View {
        if let user = chatModel.currentUser {
            ChatListView(user: user)
                .onAppear {
                    do {
                        try apiStartChat()
                        chatModel.chats = try apiGetChats()
                    } catch {
                        fatalError("Failed to start or load chats: \(error)")
                    }

                    DispatchQueue.global().async {
                        while(true) {
                            do {
                                try processReceivedMsg(chatModel, chatRecvMsg())
                            } catch {
                                print("error receiving message: ", error)
                            }
                        }
                    }
                }
        } else {
            WelcomeView()
        }
    }    
}


//struct ContentView_Previews: PreviewProvider {
//    static var previews: some View {
//        ContentView(text: "Hello!")
//    }
//}
