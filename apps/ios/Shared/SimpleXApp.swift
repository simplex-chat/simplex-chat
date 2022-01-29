//
//  SimpleXApp.swift
//  Shared
//
//  Created by Evgeny Poberezkin on 17/01/2022.
//

import SwiftUI

@main
struct SimpleXApp: App {
    @StateObject private var chatModel = ChatModel()
    
    init() {
        hs_init(0, nil)
    }

    var body: some Scene {
        WindowGroup {
            ContentView()
                .environmentObject(chatModel)
                .onAppear() {
                    chatModel.currentUser = chatGetUser()
                }
        }
    }
}
