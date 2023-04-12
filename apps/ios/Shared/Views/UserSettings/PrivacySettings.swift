//
//  PrivacySettings.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 29/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct PrivacySettings: View {
    @AppStorage(DEFAULT_PRIVACY_ACCEPT_IMAGES) private var autoAcceptImages = true
    @AppStorage(DEFAULT_PRIVACY_LINK_PREVIEWS) private var useLinkPreviews = true
    @State private var simplexLinkMode = privacySimplexLinkModeDefault.get()
    @AppStorage(DEFAULT_PRIVACY_PROTECT_SCREEN) private var protectScreen = false
    @AppStorage(DEFAULT_PERFORM_LA) private var prefPerformLA = false
    @State private var currentLAMode = privacyLocalAuthModeDefault.get()

    var body: some View {
        VStack {
            List {
                Section("Device") {
                    NavigationLink {
                        SimplexLockView(prefPerformLA: $prefPerformLA, currentLAMode: $currentLAMode)
                            .navigationTitle("SimpleX Lock")
                    } label: {
                        if prefPerformLA {
                            settingsRow("lock.fill", color: .green) {
                                simplexLockRow(currentLAMode.text)
                            }
                        } else {
                            settingsRow("lock") {
                                simplexLockRow("Off")
                            }
                        }
                    }
                    settingsRow("eye.slash") {
                        Toggle("Protect app screen", isOn: $protectScreen)
                    }
                }

                Section {
                    settingsRow("photo") {
                        Toggle("Auto-accept images", isOn: $autoAcceptImages)
                            .onChange(of: autoAcceptImages) {
                                privacyAcceptImagesGroupDefault.set($0)
                            }
                    }
                    settingsRow("network") {
                        Toggle("Send link previews", isOn: $useLinkPreviews)
                    }
                    settingsRow("link") {
                        Picker("SimpleX links", selection: $simplexLinkMode) {
                            ForEach(SimpleXLinkMode.values) { mode in
                                Text(mode.text)
                            }
                        }
                    }
                    .frame(height: 36)
                    .onChange(of: simplexLinkMode) { mode in
                        privacySimplexLinkModeDefault.set(mode)
                    }
                } header: {
                    Text("Chats")
                } footer: {
                    if case .browser = simplexLinkMode {
                        Text("Opening the link in the browser may reduce connection privacy and security. Untrusted SimpleX links will be red.")
                    }
                }
            }
        }
    }

    private func simplexLockRow(_ value: LocalizedStringKey) -> some View {
        HStack {
            Text("SimpleX Lock")
            Spacer()
            Text(value)
        }
    }
}

enum LAMode: String, Identifiable, CaseIterable {
    case system
    case passcode

    public var id: Self { self }

    var text: LocalizedStringKey {
        switch self {
        case .system: return "System"
        case .passcode: return "Passcode"
        }
    }
}

struct SimplexLockView: View {
    @Binding var prefPerformLA: Bool
    @Binding var currentLAMode: LAMode
    @EnvironmentObject var m: ChatModel
    @AppStorage(DEFAULT_LA_NOTICE_SHOWN) private var prefLANoticeShown = false
    @State private var laMode: LAMode = privacyLocalAuthModeDefault.get()
    @AppStorage(DEFAULT_LA_LOCK_DELAY) private var laLockDelay = 30
    @State var performLA: Bool = UserDefaults.standard.bool(forKey: DEFAULT_PERFORM_LA)
    @State private var performLAToggleReset = false
    @State private var performLAModeReset = false
    @State private var showPasswordAction: PasswordAction? = nil
    @State private var showChangePassword = false
    @State var laAlert: LASettingViewAlert? = nil

    enum LASettingViewAlert: Identifiable {
        case laTurnedOnAlert
        case laFailedAlert
        case laUnavailableInstructionAlert
        case laUnavailableTurningOffAlert
        case laPasscodeSetAlert
        case laPasscodeChangedAlert
        case laPasscodeNotChangedAlert

        var id: Self { self }
    }

    enum PasswordAction: Identifiable {
        case enableAuth
        case toggleMode
        case changePassword

        var id: Self { self }
    }

    let laDelays: [Int] = [10, 30, 60, 180, 0]

    func laDelayText(_ t: Int) -> LocalizedStringKey {
        let m = t / 60
        let s = t % 60
        return t == 0
            ? "Immediately"
            : m == 0 || s != 0
            ? "\(s) seconds" // there are no options where both minutes and seconds are needed
            : "\(m) minutes"
    }

    var body: some View {
        VStack {
            List {
                Section("") {
                    Toggle("Enable lock", isOn: $performLA)
                    Picker("Lock mode", selection: $laMode) {
                        ForEach(LAMode.allCases) { mode in
                            Text(mode.text)
                        }
                    }
                    if performLA {
                        Picker("Lock after", selection: $laLockDelay) {
                            let delays = laDelays.contains(laLockDelay) ? laDelays : [laLockDelay] + laDelays
                            ForEach(delays, id: \.self) { t in
                                Text(laDelayText(t))
                            }
                        }
                        if showChangePassword && laMode == .passcode {
                            Button("Change Passcode") {
                                changeLAPassword()
                            }
                        }
                    }
                }
            }
        }
        .onChange(of: performLA) { performLAToggle in
            prefLANoticeShown = true
            if performLAToggleReset {
                performLAToggleReset = false
            } else if performLAToggle {
                switch currentLAMode {
                case .system:
                    enableLA()
                case .passcode:
                    resetLA()
                    showPasswordAction = .enableAuth
                }
            } else {
                disableLA()
            }
        }
        .onChange(of: laMode) { _ in
            if performLAModeReset {
                performLAModeReset = false
            } else if performLA {
                toggleLAMode()
            } else {
                updateLAMode()
            }
        }
        .alert(item: $laAlert) { alertItem in
            switch alertItem {
            case .laTurnedOnAlert: return laTurnedOnAlert()
            case .laFailedAlert: return laFailedAlert()
            case .laUnavailableInstructionAlert: return laUnavailableInstructionAlert()
            case .laUnavailableTurningOffAlert: return laUnavailableTurningOffAlert()
            case .laPasscodeSetAlert: return passcodeAlert("Passcode set!")
            case .laPasscodeChangedAlert: return passcodeAlert("Passcode changed!")
            case .laPasscodeNotChangedAlert: return mkAlert(title: "Passcode not changed!")
            }
        }
        .sheet(item: $showPasswordAction) { a in
            switch a {
            case .enableAuth:
                SetAppPasscodeView {
                    laLockDelay = 30
                    prefPerformLA = true
                    showChangePassword = true
                    showLAAlert(.laPasscodeSetAlert)
                } cancel: {
                    resetLAEnabled(false)
                }
            case .toggleMode:
                SetAppPasscodeView {
                    laLockDelay = 30
                    updateLAMode()
                    showChangePassword = true
                    showLAAlert(.laPasscodeSetAlert)
                } cancel: {
                    revertLAMode()
                }
            case .changePassword:
                SetAppPasscodeView {
                    showLAAlert(.laPasscodeChangedAlert)
                } cancel: {
                    showLAAlert(.laPasscodeNotChangedAlert)
                }
            }
        }
        .onAppear {
            showChangePassword = prefPerformLA && currentLAMode == .passcode
        }
        .onDisappear() {
            m.laRequest = nil
        }
    }

    private func  showLAAlert(_ a: LASettingViewAlert) {
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
            laAlert = a
        }
    }

    private func toggleLAMode() {
        authenticate(reason: NSLocalizedString("Change lock mode", comment: "authentication reason")) { laResult in
            switch laResult {
            case .failed:
                revertLAMode()
                laAlert = .laFailedAlert
            case .success:
                switch laMode {
                case .system:
                    updateLAMode()
                    authenticate(reason: NSLocalizedString("Enable SimpleX Lock", comment: "authentication reason")) { laResult in
                        switch laResult {
                        case .success:
                            _ = kcAppPassword.remove()
                            laAlert = .laTurnedOnAlert
                        case .failed, .unavailable:
                            currentLAMode = .passcode
                            privacyLocalAuthModeDefault.set(.passcode)
                            revertLAMode()
                            laAlert = .laFailedAlert
                        }
                    }
                case .passcode:
                    showPasswordAction = .toggleMode
                }
            case .unavailable:
                disableUnavailableLA()
            }
        }
    }

    private func changeLAPassword() {
        authenticate(title: "Current Passcode", reason: NSLocalizedString("Change passcode", comment: "authentication reason")) { laResult in
            switch laResult {
            case .failed: laAlert = .laFailedAlert
            case .success: showPasswordAction = .changePassword
            case .unavailable: disableUnavailableLA()
            }
        }
    }

    private func enableLA() {
        resetLA()
        authenticate(reason: NSLocalizedString("Enable SimpleX Lock", comment: "authentication reason")) { laResult in
            switch laResult {
            case .success:
                prefPerformLA = true
                laAlert = .laTurnedOnAlert
            case .failed:
                resetLAEnabled(false)
                laAlert = .laFailedAlert
            case .unavailable:
                disableUnavailableLA()
            }
        }
    }

    private func disableUnavailableLA() {
        resetLAEnabled(false)
        laMode = .system
        updateLAMode()
        laAlert = .laUnavailableInstructionAlert
    }

    private func disableLA() {
        authenticate(reason: NSLocalizedString("Disable SimpleX Lock", comment: "authentication reason")) { laResult in
            switch (laResult) {
            case .success:
                prefPerformLA = false
                resetLA()
            case .failed:
                resetLAEnabled(true)
                laAlert = .laFailedAlert
            case .unavailable:
                prefPerformLA = false
                laAlert = .laUnavailableTurningOffAlert
            }
        }
    }

    private func resetLA() {
        _ = kcAppPassword.remove()
        laLockDelay = 30
        showChangePassword = false
    }

    private func resetLAEnabled(_ onOff: Bool) {
        prefPerformLA = onOff
        performLAToggleReset = true
        withAnimation { performLA = onOff }
    }

    private func revertLAMode() {
        performLAModeReset = true
        withAnimation { laMode = currentLAMode }
    }

    private func updateLAMode() {
        currentLAMode = laMode
        privacyLocalAuthModeDefault.set(laMode)
    }

    private func passcodeAlert(_ title: LocalizedStringKey) -> Alert {
        mkAlert(title: title, message: "Please remember or store it securely - there is no way to recover a lost passcode!")
    }
}

struct PrivacySettings_Previews: PreviewProvider {
    static var previews: some View {
        PrivacySettings()
    }
}
