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
                        chatModel.v3DBMigration = v3DBMigrationDefault.get()
                        try initializeChat(start: chatModel.v3DBMigration.startChat)
                    } catch let error {
                        fatalError("Failed to start or load chats: \(responseError(error))")
                    }
                }
                .onChange(of: scenePhase) { phase in
                    logger.debug("scenePhase \(String(describing: scenePhase))")
                    switch (phase) {
                    case .background:
                        suspendChat()
                        if chatModel.chatRunning == true {
                            ChatReceiver.shared.stop()
                        }
                        BGManager.shared.schedule()
                        if userAuthorized == true {
                            enteredBackground = ProcessInfo.processInfo.systemUptime
                        }
                        doAuthenticate = false
                    case .active:
                        if chatModel.chatRunning == true {
                            ChatReceiver.shared.start()
                        }
                        let appState = appStateGroupDefault.get()
                        activateChat()
                        if appState.inactive && chatModel.chatRunning == true {
                            do {
                                let chats = try apiGetChats()
                                chatModel.replaceChats(with: chats)
                            } catch let error {
                                logger.error("apiGetChats: cannot update chats \(responseError(error))")
                            }
                        }
                        doAuthenticate = authenticationExpired()
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
            switch v3DBMigrationDefault.get() {
            case .migrated: ()
            default: v3DBMigrationDefault.set(.offer)
            }
            logger.debug("SimpleXApp init: using legacy DB in documents folder: \(getAppDatabasePath(), privacy: .public)*.db")
        } else {
            dbContainerGroupDefault.set(.group)
            v3DBMigrationDefault.set(.ready)
            logger.debug("SimpleXApp init: using DB in app group container: \(getAppDatabasePath(), privacy: .public)*.db")
            logger.debug("SimpleXApp init: legacy DB\(legacyDatabase ? "" : " not", privacy: .public) present")
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
