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

    var body: some View {
        PasscodeView(passcode: $password, title: authRequest.title ?? "Enter Passcode", reason: authRequest.reason, submitLabel: "Submit") {
            if let sdPassword = kcSelfDestructPassword.get(), authRequest.selfDestruct && password == sdPassword {
                deleteStorageAndRestart(sdPassword) { r in
                    m.laRequest = nil
                    authRequest.completed(r)
                }
                return
            }
            let r: LAResult = password == authRequest.password
                            ? .success
                            : .failed(authError: NSLocalizedString("Incorrect passcode", comment: "PIN entry"))
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
                try await stopChatAsync()
                try await deleteChatAsync()
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
                if m.currentUser != nil { return }
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
