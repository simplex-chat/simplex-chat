//
//  LocalAuthView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 10/04/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct LocalAuthView: View {
    @EnvironmentObject var m: ChatModel
    var authRequest: LocalAuthRequest
    @State private var password = ""
    @State private var allowToReact = true

    var body: some View {
        PasscodeView(passcode: $password, title: authRequest.title ?? "Enter Passcode", reason: authRequest.reason, submitLabel: "Submit",
                     buttonsEnabled: $allowToReact) {
            if let sdPassword = kcSelfDestructPassword.get(), authRequest.selfDestruct && password == sdPassword {
                allowToReact = false
                deleteStorageAndRestart(sdPassword) { r in
                    m.laRequest = nil
                    authRequest.completed(r)
                }
                return
            }
            let r: LAResult
            if password == authRequest.password {
                if authRequest.selfDestruct && kcSelfDestructPassword.get() != nil && !m.chatInitialized {
                    initChatAndMigrate()
                }
                r = .success
            } else {
                r = .failed(authError: NSLocalizedString("Incorrect passcode", comment: "PIN entry"))
            }
            m.laRequest = nil
            authRequest.completed(r)
        } cancel: {
            m.laRequest = nil
            authRequest.completed(.failed(authError: NSLocalizedString("Authentication cancelled", comment: "PIN entry")))
        }
    }

    private func deleteStorageAndRestart(_ password: String, completed: @escaping (LAResult) -> Void) {
        Task {
            do {
                /** Waiting until [initializeChat] finishes */
                while (m.ctrlInitInProgress) {
                    try await Task.sleep(nanoseconds: 50_000000)
                }
                if m.chatRunning == true {
                    try await stopChatAsync()
                }
                if m.chatInitialized {
                    /**
                     * The following sequence can bring a user here:
                     * the user opened the app, entered app passcode, went to background, returned back, entered self-destruct code.
                     * In this case database should be closed to prevent possible situation when OS can deny database removal command
                     * */
                    chatCloseStore()
                }
                deleteAppDatabaseAndFiles()
                // Clear sensitive data on screen just in case app fails to hide its views while new database is created
                m.chatId = nil
                ItemsModel.shared.reversedChatItems = []
                m.chats = []
                m.popChatCollector.clear()
                m.users = []
                _ = kcAppPassword.set(password)
                _ = kcSelfDestructPassword.remove()
                await NtfManager.shared.removeAllNotifications()
                let displayName = UserDefaults.standard.string(forKey: DEFAULT_LA_SELF_DESTRUCT_DISPLAY_NAME)
                UserDefaults.standard.removeObject(forKey: DEFAULT_LA_SELF_DESTRUCT)
                UserDefaults.standard.removeObject(forKey: DEFAULT_LA_SELF_DESTRUCT_DISPLAY_NAME)
                await MainActor.run {
                    m.chatDbChanged = true
                    m.chatInitialized = false
                }
                resetChatCtrl()
                try initializeChat(start: true)
                m.chatDbChanged = false
                AppChatState.shared.set(.active)
                if m.currentUser != nil || !m.chatInitialized { return }
                var profile: Profile? = nil
                if let displayName = displayName, displayName != "" {
                    profile = Profile(displayName: displayName, fullName: "")
                }
                m.currentUser = try apiCreateActiveUser(profile, pastTimestamp: true)
                onboardingStageDefault.set(.onboardingComplete)
                m.onboardingStage = .onboardingComplete
                try startChat()
                completed(.success)
            } catch {
                completed(.failed(authError: NSLocalizedString("Incorrect passcode", comment: "PIN entry")))
            }
        }
    }
}

struct LocalAuthView_Previews: PreviewProvider {
    static var previews: some View {
        LocalAuthView(authRequest: LocalAuthRequest.sample)
    }
}
