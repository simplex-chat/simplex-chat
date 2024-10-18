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
    @State private var enteredBackgroundAuthenticated: TimeInterval? = nil

    init() {
        DispatchQueue.global(qos: .background).sync {
            haskell_init()
//            hs_init(0, nil)
        }
        UserDefaults.standard.register(defaults: appDefaults)
        setGroupDefaults()
        registerGroupDefaults()
        setDbContainer()
        BGManager.shared.register()
        NtfManager.shared.registerCategories()
    }

    var body: some Scene {
        WindowGroup {
            // contentAccessAuthenticationExtended has to be passed to ContentView on view initialization,
            // so that it's computed by the time view renders, and not on event after rendering
            ContentView(contentAccessAuthenticationExtended: !authenticationExpired())
                .environmentObject(chatModel)
                .environmentObject(AppTheme.shared)
                .onOpenURL { url in
                    logger.debug("ContentView.onOpenURL: \(url)")
                    chatModel.appOpenUrl = url
                }
                .onAppear() {
                    // Present screen for continue migration if it wasn't finished yet
                    if chatModel.migrationState != nil {
                        // It's important, otherwise, user may be locked in undefined state
                        onboardingStageDefault.set(.step1_SimpleXInfo)
                        chatModel.onboardingStage = onboardingStageDefault.get()
                    } else if kcAppPassword.get() == nil || kcSelfDestructPassword.get() == nil {
                        DispatchQueue.main.asyncAfter(deadline: .now() + 0.15) {
                            initChatAndMigrate()
                        }
                    }
                }
                .onChange(of: scenePhase) { phase in
                    logger.debug("scenePhase was \(String(describing: scenePhase)), now \(String(describing: phase))")
                    AppSheetState.shared.scenePhaseActive = phase == .active
                    switch (phase) {
                    case .background:
                        // --- authentication
                        // see ContentView .onChange(of: scenePhase) for remaining authentication logic
                        if chatModel.contentViewAccessAuthenticated {
                            enteredBackgroundAuthenticated = ProcessInfo.processInfo.systemUptime
                        }
                        chatModel.contentViewAccessAuthenticated = false
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

                        if appState != .stopped {
                            startChatAndActivate {
                                if chatModel.chatRunning == true {
                                    if let ntfResponse = chatModel.notificationResponse {
                                        chatModel.notificationResponse = nil
                                        NtfManager.shared.processNotificationResponse(ntfResponse)
                                    }
                                    if appState.inactive {
                                        Task {
                                            await updateChats()
                                            if !chatModel.showCallView && !CallController.shared.hasActiveCalls() {
                                                await updateCallInvitations()
                                            }
                                        }
                                    }
                                }
                            }
                        }
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
            logger.debug("SimpleXApp init: using legacy DB in documents folder: \(getAppDatabasePath())*.db")
        } else {
            dbContainerGroupDefault.set(.group)
            setMigrationState(.ready)
            logger.debug("SimpleXApp init: using DB in app group container: \(getAppDatabasePath())*.db")
            logger.debug("SimpleXApp init: legacy DB\(legacyDatabase ? "" : " not") present")
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

    private func updateChats() async {
        do {
            let chats = try await apiGetChatsAsync()
            await MainActor.run { chatModel.updateChats(chats) }
            if let id = chatModel.chatId,
               let chat = chatModel.getChat(id) {
                Task { await loadChat(chat: chat, clearItems: false) }
            }
            if let ncr = chatModel.ntfContactRequest {
                await MainActor.run { chatModel.ntfContactRequest = nil }
                if case let .contactRequest(contactRequest) = chatModel.getChat(ncr.chatId)?.chatInfo {
                    Task { await acceptContactRequest(incognito: ncr.incognito, contactRequest: contactRequest) }
                }
            }
        } catch let error {
            logger.error("apiGetChats: cannot update chats \(responseError(error))")
        }
    }

    private func updateCallInvitations() async {
        do {
            try await refreshCallInvitations()
        } catch let error {
            logger.error("apiGetCallInvitations: cannot update call invitations \(responseError(error))")
        }
    }
}
