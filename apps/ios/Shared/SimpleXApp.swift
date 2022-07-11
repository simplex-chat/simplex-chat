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
    @State private var enteredInactive: Double? = nil
    @State private var firstAuthentication: Bool = true

    init() {
        hs_init(0, nil)
        UserDefaults.standard.register(defaults: appDefaults)
        setDbContainer()
        BGManager.shared.register()
        NtfManager.shared.registerCategories()
    }

    var body: some Scene {
        return WindowGroup {
            ContentView(doAuthenticate: $doAuthenticate, userAuthorized: $userAuthorized, firstAuthentication: $firstAuthentication)
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
                        BGManager.shared.schedule()
                        enteredBackground = ProcessInfo.processInfo.systemUptime
                        doAuthenticate = false
                    case .inactive:
                        enteredInactive = ProcessInfo.processInfo.systemUptime
                        doAuthenticate = false
                    case .active:
                        if chatModel.chatRunning == true {
                            ChatReceiver.shared.start()
                        }
                        let appState = appStateGroupDefault.get()
                        activateChat()
                        if appState.inactive && chatModel.chatRunning == true {
                            updateChats()
                            updateCallInvitations()
                        }
                        doAuthenticate = authenticationExpired()
                        enteredBackground = nil
                        enteredInactive = nil
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
        if firstAuthentication {
            return true
        } else if let lastActive = lastActive() {
            return ProcessInfo.processInfo.systemUptime - lastActive >= 30
        } else {
            return false
        }
    }

    private func lastActive() -> Double? {
        if let eb = enteredBackground, let ei = enteredInactive {
            return min(eb, ei)
        } else if let eb = enteredBackground {
            return eb
        } else {
            return enteredInactive
        }
    }

    private func updateChats() {
        do {
            let chats = try apiGetChats()
            chatModel.updateChats(with: chats)
            if let id = chatModel.chatId,
               let chat = chatModel.getChat(id) {
                loadChat(chat: chat)
                DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                    if chatModel.chatId == chat.id {
                        Task { await markChatRead(chat) }
                    }
                }
            }
        } catch let error {
            logger.error("apiGetChats: cannot update chats \(responseError(error))")
        }
    }

    private func updateCallInvitations() {
        do {
            try refreshCallInvitations()
        }
        catch let error {
            logger.error("apiGetCallInvitations: cannot update call invitations \(responseError(error))")
        }
    }
}
