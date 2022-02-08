//
//  ContentView.swift
//  Shared
//
//  Created by Evgeny Poberezkin on 17/01/2022.
//

import SwiftUI

struct ContentView: View {
    @EnvironmentObject var chatModel: ChatModel
    @State private var showNotificationAlert = false

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
                    ChatReceiver.shared.start(chatModel)
                    NtfManager.shared.requestPermission(onDeny: {
                        showNotificationAlert = true
                    })
                }
                .alert(isPresented : $showNotificationAlert){
                     Alert(
                        title: Text("Notification are disabled!"),
                         message: Text("Please open settings to enable"),
                         primaryButton: .default(Text("Open Settings")) {
                             DispatchQueue.main.async {
                                 UIApplication.shared.open(URL(string: UIApplication.openSettingsURLString)!, options: [:], completionHandler: nil)
                             }
                         },
                         secondaryButton: .cancel()
                     )
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
