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
    @AppStorage(DEFAULT_PERFORM_LA) private var prefPerformLA = false
    @State private var userAuthorized: Bool?
    @State private var doAuthenticate = false
    @State private var enteredBackground: Double? = nil

    init() {
        hs_init(0, nil)
        UserDefaults.standard.register(defaults: appDefaults)
        setDbContainer()
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
                .onAppear() {
                    do {
                        try initializeChat(start: v3DBMigrationDefault.get().startChat)
                    } catch let error {
                        fatalError("Failed to start or load chats: \(responseError(error))")
                    }
                }
                .onChange(of: scenePhase) { phase in
                    logger.debug("scenePhase \(String(describing: scenePhase))")
                    switch (phase) {
                    case .background:
                        pauseApp()
                        BGManager.shared.schedule()
                        if userAuthorized == true {
                            enteredBackground = ProcessInfo.processInfo.systemUptime
                        }
                        doAuthenticate = false
                    case .active:
                        appStateGroupDefault.set(.active)
                        apiSetAppPhase(appPhase: .active)
                        doAuthenticate = authenticationExpired()
                    default:
                        break
                    }
                }
        }
    }

    private func setDbContainer() {
// Uncomment and run once to open DB in app documents
//        dbContainerGroupDefault.set(.documents)
//        v3DBMigrationDefault.set(.offer)
        let legacyDatabase = hasLegacyDatabase()
        if legacyDatabase, case .documents = dbContainerGroupDefault.get() {
            dbContainerGroupDefault.set(.documents)
            logger.debug("SimpleXApp init: using legacy DB in documents folder: \(getAppDatabasePath(), privacy: .public)*.db")
            switch v3DBMigrationDefault.get() {
            case .postponed: v3DBMigrationDefault.set(.offer)
            case .exporting: v3DBMigrationDefault.set(.export_error)
            case .migrating: v3DBMigrationDefault.set(.migration_error)
            default: ()
            }
        } else {
            dbContainerGroupDefault.set(.group)
            v3DBMigrationDefault.set(.ready)
            logger.debug("SimpleXApp init: using DB in app group container: \(getAppDatabasePath(), privacy: .public)*.db")
            logger.debug("SimpleXApp init: legacy DB\(legacyDatabase ? "" : " not", privacy: .public) present")
        }
    }

    private func pauseApp() {
        appStateGroupDefault.set(.pausing)
        apiSetAppPhase(appPhase: .paused)
        let endTask = beginBGTask {
            if appStateGroupDefault.get() != .active {
                appStateGroupDefault.set(.suspending)
                apiSetAppPhase(appPhase: .suspended)
            }
        }
        DispatchQueue.global().asyncAfter(deadline: .now() + maxTaskDuration, execute: endTask)
    }

    private func authenticationExpired() -> Bool {
        if let enteredBackground = enteredBackground {
            return ProcessInfo.processInfo.systemUptime - enteredBackground >= 30
        } else {
            return true
        }
    }
}
