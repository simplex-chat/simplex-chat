//
//  SimpleXApp.swift
//  Shared
//
//  Created by Evgeny Poberezkin on 17/01/2022.
//

import SwiftUI
import OSLog
import SimpleXAppShared

let logger = Logger()

@main
struct SimpleXApp: App {
    @UIApplicationDelegateAdaptor(AppDelegate.self) var appDelegate
    @StateObject private var chatModel = ChatModel.shared
    @ObservedObject var alertManager = AlertManager.shared
    @Environment(\.scenePhase) var scenePhase
    @AppStorage(DEFAULT_PERFORM_LA) private var prefPerformLA = false
    @State private var userAuthorized: Bool?
    @State private var doAuthenticate = false
    @State private var enteredBackground: Double? = nil

    init() {
//        hs_init(0, nil)
        UserDefaults.standard.register(defaults: appDefaults)
        BGManager.shared.register()
        NtfManager.shared.registerCategories()
    }

    var body: some Scene {
        return WindowGroup {
            ContentView(doAuthenticate: $doAuthenticate, userAuthorized: $userAuthorized)
                .environmentObject(chatModel)
                .onOpenURL { url in
                    logger.debug("ContentView.onOpenURL: \(url)")
                    chatModel.appOpenUrl = url
                }
                .onAppear {
                    Task { await initializeChat() }
                }
                .onChange(of: scenePhase) { phase in
                    logger.debug("scenePhase \(String(describing: scenePhase))")
//                    let res = machMessenger.sendMessageWithReply(NSE_MACH_PORT, msg: "App scenePhase changed to \(String(describing: scenePhase))")
//                    logger.debug("MachMessenger \(String(describing: res), privacy: .public)")
                    setAppState(phase)
                    switch (phase) {
                    case .background:
                        BGManager.shared.schedule()
                        if userAuthorized == true {
                            enteredBackground = ProcessInfo.processInfo.systemUptime
                        }
                        doAuthenticate = false
//                        machMessenger.stop()
                    case .active:
                        doAuthenticate = authenticationExpired()
//                        machMessenger.start()
                    default:
                        break
                    }
                }
        }
    }

    private func authenticationExpired() -> Bool {
        if let enteredBackground = enteredBackground {
            return ProcessInfo.processInfo.systemUptime - enteredBackground >= 30
        } else {
            return true
        }
    }
}
