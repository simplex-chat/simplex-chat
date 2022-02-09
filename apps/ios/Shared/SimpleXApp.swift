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
    @Environment(\.scenePhase) var scenePhase

    init() {
        hs_init(0, nil)
        BGManager.shared.register()
        NtfManager.shared.registerCategories()
    }

    var body: some Scene {
        return WindowGroup {
            ContentView()
                .environmentObject(chatModel)
                .onOpenURL { url in
                    chatModel.appOpenUrl = url
                    chatModel.connectViaUrl = true
                    print(url)
                }
                .onAppear() {
                    initializeChat(chatModel)
                    NtfManager.shared.setModel(chatModel)
                }
                .onChange(of: scenePhase) { phase in
                    if phase == .background {
                        BGManager.shared.schedule(chatModel)
                    } else {
                        ChatReceiver.shared.restart(chatModel)
                    }
                }
        }
    }
}
