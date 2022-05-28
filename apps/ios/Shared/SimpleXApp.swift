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
    @UIApplicationDelegateAdaptor(AppDelegate.self) var appDelegate
    @StateObject private var chatModel = ChatModel.shared
    @ObservedObject var alertManager = AlertManager.shared
    @Environment(\.scenePhase) var scenePhase
    @AppStorage(DEFAULT_PERFORM_LA) private var prefPerformLA = false
    @State private var userAuthorized: Bool? = nil
    @State private var doAuthenticate: Bool = false
    @State private var enteredBackground: Double? = nil

    init() {
        hs_init(0, nil)
        UserDefaults.standard.register(defaults: appDefaults)
        BGManager.shared.register()
        NtfManager.shared.registerCategories()
    }

    var body: some Scene {
        return WindowGroup {
            ContentView(doAuthenticate: $doAuthenticate, enteredBackground: $enteredBackground)
                .environmentObject(chatModel)
                .onOpenURL { url in
                    logger.debug("ContentView.onOpenURL: \(url)")
                    chatModel.appOpenUrl = url
                }
                .onAppear() {
                    initializeChat()
                }
                .onChange(of: scenePhase) { phase in
                    logger.debug("scenePhase \(String(describing: scenePhase))")
                    setAppState(phase)
                    switch (phase) {
                    case .background:
                        BGManager.shared.schedule()
                        doAuthenticate = false
                        enteredBackground = ProcessInfo.processInfo.systemUptime
                    case .active:
                        doAuthenticate = true
                    default:
                        break
                    }
                }
        }
    }
}
