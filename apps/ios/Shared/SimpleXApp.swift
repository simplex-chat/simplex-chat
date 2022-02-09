//
//  SimpleXApp.swift
//  Shared
//
//  Created by Evgeny Poberezkin on 17/01/2022.
//

import SwiftUI
import OSLog

let logger = Logger()

@main
struct SimpleXApp: App {
    @StateObject private var chatModel = ChatModel.shared
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
                    logger.debug("ContentView.onOpenURL: \(url)")
                    chatModel.appOpenUrl = url
                    chatModel.connectViaUrl = true
                }
                .onAppear() {
                    initializeChat()
                }
                .onChange(of: scenePhase) { phase in
                    if phase == .background {
                        BGManager.shared.schedule()
                    } else {
                        BGManager.shared.invalidateStopTimer()
                        ChatReceiver.shared.restart()
                    }
                }
        }
    }
}
