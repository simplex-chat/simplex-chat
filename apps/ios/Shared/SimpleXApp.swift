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
    @Environment(\.scenePhase) var scenePhase
    @State private var userAuthorized: Bool? = nil
    @State private var doAuthenticate: Bool? = nil
    @State private var lastLA: Double? = nil

    init() {
        hs_init(0, nil)
        BGManager.shared.register()
        NtfManager.shared.registerCategories()
    }

    var body: some Scene {
        return WindowGroup {
            ContentView(userAuthorized: $userAuthorized)
                .environmentObject(chatModel)
                .onOpenURL { url in
                    logger.debug("ContentView.onOpenURL: \(url)")
                    chatModel.appOpenUrl = url
                }
                .onAppear() {
                    initializeChat()
                    doAuthenticate = true
                }
                .onChange(of: scenePhase) { phase in
                    logger.debug("scenePhase \(String(describing: scenePhase))")
                    setAppState(phase)
                    switch (phase) {
                    case .background:
                        BGManager.shared.schedule()
                        doAuthenticate = true
                    case .inactive:
                        authenticateUser()
                    case .active:
                        authenticateUser()
                    default:
                        break
                    }
                }
        }
    }

    private func authenticateUser() {
        if doAuthenticate == true,
           authenticationExpired() {
            doAuthenticate = false
            userAuthorized = false
            authenticate() { laResult in
                switch (laResult) {
                case .success:
                    userAuthorized = true
                    lastLA = ProcessInfo.processInfo.systemUptime
                case .failed:
                    laFailedAlert()
                case .unavailable:
                    userAuthorized = true
                    laUnavailableAlert()
                }
            }
        }
    }

    private func authenticationExpired() -> Bool {
        if (lastLA == nil) {
            return true
        }
        else if let lastLA = lastLA, ProcessInfo.processInfo.systemUptime - lastLA >= 30 {
            return true
        } else {
            return false
        }
    }
}
