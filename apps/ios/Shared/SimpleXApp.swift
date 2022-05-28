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
    @AppStorage(DEFAULT_PERFORM_LA) private var performLA = false
    @State private var userAuthorized: Bool? = nil
    @State private var doAuthenticate: Bool = true
    @State private var enteredBackground: Double? = nil

    init() {
        hs_init(0, nil)
        UserDefaults.standard.register(defaults: appDefaults)
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
                    chatModel.performLA = performLA
                    initializeChat()
                }
                .onChange(of: scenePhase) { phase in
                    logger.debug("scenePhase \(String(describing: scenePhase))")
                    setAppState(phase)
                    switch (phase) {
                    case .background:
                        BGManager.shared.schedule()
                        doAuthenticate = true
                        enteredBackground = ProcessInfo.processInfo.systemUptime
                    case .inactive:
                        authenticateOnPhaseChange()
                    case .active:
                        authenticateOnPhaseChange()
                    default:
                        break
                    }
                }
        }
    }

    private func authenticateOnPhaseChange() {
        if doAuthenticate {
            doAuthenticate = false
            if performLA,
               authenticationExpired() {
                userAuthorized = false
                authenticate(reason: "Unlock") { laResult in
                    switch (laResult) {
                    case .success:
                        userAuthorized = true
                    case .failed:
                        AlertManager.shared.showAlert(laFailedAlert())
                    case .unavailable:
                        userAuthorized = true
                        performLA = false
                        AlertManager.shared.showAlert(laUnavailableTurningOffAlert())
                    }
                }
            } else {
                userAuthorized = true
            }
        }
    }

    private func authenticationExpired() -> Bool {
        if (enteredBackground == nil) {
            return true
        }
        else if let enteredBackground = enteredBackground, ProcessInfo.processInfo.systemUptime - enteredBackground >= 30 {
            return true
        } else {
            return false
        }
    }
}
