//
//  SimpleXApp.swift
//  Shared
//
//  Created by Evgeny Poberezkin on 17/01/2022.
//

import SwiftUI
import OSLog
import SimpleXChat

let logger = Logger()

@main
struct SimpleXApp: App {
    @UIApplicationDelegateAdaptor(AppDelegate.self) var appDelegate
    @StateObject private var chatModel = ChatModel.shared
    @ObservedObject var alertManager = AlertManager.shared

    @Environment(\.scenePhase) var scenePhase
    @State private var showInitializationView = false
    @State private var enteredBackgroundAuthenticated: TimeInterval? = nil

    init() {
//        DispatchQueue.global(qos: .background).sync {
        haskell_init()
//            hs_init(0, nil)
//        }
        UserDefaults.standard.register(defaults: appDefaults)
        setGroupDefaults()
        registerGroupDefaults()
        setDbContainer()
        BGManager.shared.register()
        NtfManager.shared.registerCategories()
    }

    var body: some Scene {
        WindowGroup {
            // userAuthenticationExtended has to be passed to ContentView on view initialization,
            // so that it's computed by the time view renders, and not on event after rendering
            ContentView(
                showInitializationView: $showInitializationView,
                userAuthenticationExtended: !authenticationExpired()
            )
            .environmentObject(chatModel)
            .onOpenURL { url in
                logger.debug("ContentView.onOpenURL: \(url)")
                chatModel.appOpenUrl = url
            }
            .onAppear() {
                showInitializationView = true
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.15) {
                    initChatAndMigrate()
                }
            }
            .onChange(of: scenePhase) { phase in
                logger.debug("scenePhase was \(String(describing: scenePhase)), now \(String(describing: phase))")
                switch (phase) {
                case .background:
                    // see ContentView .onChange(of: scenePhase) for remaining authentication logic
                    if chatModel.userAuthenticated {
                        enteredBackgroundAuthenticated = ProcessInfo.processInfo.systemUptime
                    }
                    chatModel.userAuthenticated = false
                default:
                    break
                }
            }
        }
    }

    private func setDbContainer() {
// Uncomment and run once to open DB in app documents folder:
//         dbContainerGroupDefault.set(.documents)
//         v3DBMigrationDefault.set(.offer)
// to create database in app documents folder also uncomment:
//         let legacyDatabase = true
        let legacyDatabase = hasLegacyDatabase()
        if legacyDatabase, case .documents = dbContainerGroupDefault.get() {
            dbContainerGroupDefault.set(.documents)
            setMigrationState(.offer)
            logger.debug("SimpleXApp init: using legacy DB in documents folder: \(getAppDatabasePath(), privacy: .public)*.db")
        } else {
            dbContainerGroupDefault.set(.group)
            setMigrationState(.ready)
            logger.debug("SimpleXApp init: using DB in app group container: \(getAppDatabasePath(), privacy: .public)*.db")
            logger.debug("SimpleXApp init: legacy DB\(legacyDatabase ? "" : " not", privacy: .public) present")
        }
    }

    private func setMigrationState(_ state: V3DBMigrationState) {
        if case .migrated = v3DBMigrationDefault.get() { return }
        v3DBMigrationDefault.set(state)
    }

    private func authenticationExpired() -> Bool {
        if let enteredBackgroundAuthenticated = enteredBackgroundAuthenticated {
            let delay = Double(UserDefaults.standard.integer(forKey: DEFAULT_LA_LOCK_DELAY))
            return ProcessInfo.processInfo.systemUptime - enteredBackgroundAuthenticated >= delay
        } else {
            return true
        }
    }
}
