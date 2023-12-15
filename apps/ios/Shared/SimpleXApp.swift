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
    @StateObject private var chatModel = ChatModel.shared
    @UIApplicationDelegateAdaptor(AppDelegate.self) var appDelegate
    @ObservedObject var alertManager = AlertManager.shared

    @Environment(\.scenePhase) var scenePhase

    @AppStorage(DEFAULT_PERFORM_LA) private var prefPerformLA = false
    @State private var automaticAuthenticationAttempted = false
    @State private var automaticAuthenticationFailedOrCancelled = false
    @State private var enteredBackgroundAuthenticated: TimeInterval? = nil

    @State private var canConnectNonCallKitCall = false
    @State private var lastSuccessfulUnlock: TimeInterval? = nil

    @State private var showInitializationView = false

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
            ContentView(
                userAuthenticationExtended: !authenticationExpired(),
                automaticAuthenticationFailed: automaticAuthenticationFailedOrCancelled,
                authenticateContentViewAccess: authenticateContentViewAccess,
                canConnectNonCallKitCall: $canConnectNonCallKitCall,
                showInitializationView: $showInitializationView
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
                    // --- authentication
                    if chatModel.userAuthenticated {
                        enteredBackgroundAuthenticated = ProcessInfo.processInfo.systemUptime
                    }
                    chatModel.userAuthenticated = false
                    automaticAuthenticationAttempted = false
                    automaticAuthenticationFailedOrCancelled = false
                    canConnectNonCallKitCall = false
                    // authentication ---

                    if CallController.useCallKit() && chatModel.activeCall != nil {
                        CallController.shared.shouldSuspendChat = true
                    } else {
                        suspendChat()
                        BGManager.shared.schedule()
                    }
                    NtfManager.shared.setNtfBadgeCount(chatModel.totalUnreadCountForAllUsers())
                case .active:
                    CallController.shared.shouldSuspendChat = false
                    let appState = AppChatState.shared.value
                    let authExpired = authenticationExpired()

                    if appState != .stopped {
                        startChatAndActivate {
                            if appState.inactive && chatModel.chatRunning == true {
                                updateChats()
                                if !chatModel.showCallView && !CallController.shared.hasActiveCalls() {
                                    updateCallInvitations()
                                }
                            }
                            canConnectNonCallKitCall = !(authExpired && prefPerformLA) || unlockedRecently()
                        }
                    }

                    // --- authentication
                    // condition `!chatModel.userAuthenticated` is required for when authentication is enabled in settings or on initial notice
                    if prefPerformLA && !chatModel.userAuthenticated {
                        if appState != .stopped {
                            if !authExpired {
                                chatModel.userAuthenticated = true
                            } else {
                                if !automaticAuthenticationAttempted {
                                    automaticAuthenticationAttempted = true
                                    // authenticate if call kit call is not in progress
                                    if !(CallController.useCallKit() && chatModel.showCallView && chatModel.activeCall != nil) {
                                        authenticateContentViewAccess()
                                    } else {
                                        automaticAuthenticationFailedOrCancelled = true
                                    }
                                }
                            }
                        } else {
                            // this is to show lock button and not automatically attempt authentication when app is stopped
                            chatModel.userAuthenticated = !authExpired
                        }
                    }
                    // authentication ---
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

    private func authenticateContentViewAccess() {
        logger.debug("DEBUGGING: authenticateContentViewAccess")
        dismissAllSheets(animated: false) {
            logger.debug("DEBUGGING: authenticateContentViewAccess, in dismissAllSheets callback")
            chatModel.chatId = nil

            authenticate(reason: NSLocalizedString("Unlock app", comment: "authentication reason"), selfDestruct: true) { laResult in
                logger.debug("DEBUGGING: authenticate callback: \(String(describing: laResult))")
                switch (laResult) {
                case .success:
                    canConnectNonCallKitCall = true
                    lastSuccessfulUnlock = ProcessInfo.processInfo.systemUptime
                case .failed:
                    automaticAuthenticationFailedOrCancelled = true
                    if privacyLocalAuthModeDefault.get() == .passcode {
                        AlertManager.shared.showAlert(laFailedAlert())
                    }
                case .unavailable:
                    automaticAuthenticationFailedOrCancelled = true
                    prefPerformLA = false
                    canConnectNonCallKitCall = true
                    AlertManager.shared.showAlert(laUnavailableTurningOffAlert())
                }
            }
        }
    }

    private func authenticationExpired() -> Bool {
        if let enteredBackgroundAuthorized = enteredBackgroundAuthenticated {
            let delay = Double(UserDefaults.standard.integer(forKey: DEFAULT_LA_LOCK_DELAY))
            return ProcessInfo.processInfo.systemUptime - enteredBackgroundAuthorized >= delay
        } else {
            return true
        }
    }

    private func unlockedRecently() -> Bool {
        if let lastSuccessfulUnlock = lastSuccessfulUnlock {
            return ProcessInfo.processInfo.systemUptime - lastSuccessfulUnlock < 2
        } else {
            return false
        }
    }

    private func updateChats() {
        do {
            let chats = try apiGetChats()
            chatModel.updateChats(with: chats)
            if let id = chatModel.chatId,
               let chat = chatModel.getChat(id) {
                loadChat(chat: chat)
            }
            if let ncr = chatModel.ntfContactRequest {
                chatModel.ntfContactRequest = nil
                if case let .contactRequest(contactRequest) = chatModel.getChat(ncr.chatId)?.chatInfo {
                    Task { await acceptContactRequest(incognito: ncr.incognito, contactRequest: contactRequest) }
                }
            }
        } catch let error {
            logger.error("apiGetChats: cannot update chats \(responseError(error))")
        }
    }

    private func updateCallInvitations() {
        do {
            try refreshCallInvitations()
        } catch let error {
            logger.error("apiGetCallInvitations: cannot update call invitations \(responseError(error))")
        }
    }
}
