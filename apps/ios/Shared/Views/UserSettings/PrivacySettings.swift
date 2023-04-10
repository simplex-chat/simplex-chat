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

    var body: some View {
        VStack {
            List {
                Section("Device") {
                    NavigationLink {
                        SimplexLockView()
                            .navigationTitle("SimpleX Lock")
                    } label: {
                        settingsRow("lock") {
                            Text("SimpleX Lock")
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
}

enum LAMode: String, Identifiable, CaseIterable {
    case system
    case password

    public var id: Self { self }

    var text: LocalizedStringKey {
        switch self {
        case .system: return "System"
        case .password: return "Password"
        }
    }
}

struct SimplexLockView: View {
    @AppStorage(DEFAULT_LA_NOTICE_SHOWN) private var prefLANoticeShown = false
    @AppStorage(DEFAULT_PERFORM_LA) private var prefPerformLA = false
    @AppStorage(DEFAULT_LA_LOCK_DELAY) private var laLockDelay = 30
    @State var performLA: Bool = UserDefaults.standard.bool(forKey: DEFAULT_PERFORM_LA)
    @State private var performLAToggleReset = false
    @State var laAlert: LASettingViewAlert? = nil
    @State private var laMode: LAMode = privacyLocalAuthModeDefault.get()
    @State private var currentLAMode: LAMode = privacyLocalAuthModeDefault.get()
    @State private var showSetPassword = false

    enum LASettingViewAlert: Identifiable {
        case laTurnedOnAlert
        case laFailedAlert
        case laUnavailableInstructionAlert
        case laUnavailableTurningOffAlert

        var id: Self { self }
    }

    let laDelays: [Int] = [30, 60, 120, 180, 300, 0]

    func laDelayText(_ t: Int) -> LocalizedStringKey {
        let m = t / 60
        let s = t % 60
        return t == 0
            ? "Immediately"
            : m == 0
            ? "\(s) seconds"
            : s == 0
            ? "\(m) minutes"
            : "\(m) minutes \(s) seconds"
    }

    var body: some View {
        VStack {
            List {
                Section("") {
                    Toggle("Enable lock", isOn: $performLA)
                    if performLA {
                        Picker("Lock after", selection: $laLockDelay) {
                            ForEach(laDelays, id: \.self) { t in
                                Text(laDelayText(t))
                            }
                        }
                        Picker("Lock mode", selection: $laMode) {
                            ForEach(LAMode.allCases) { mode in
                                Text(mode.text)
                            }
                        }
                        if case .password = laMode {
                            Button("Change password") {
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
            } else {
                if performLAToggle {
                    enableLA()
                } else {
                    disableLA()
                }
            }
        }
        .onChange(of: laMode) { _ in
            toggleLAMode()
        }
        .alert(item: $laAlert) { alertItem in
            switch alertItem {
            case .laTurnedOnAlert: return laTurnedOnAlert()
            case .laFailedAlert: return laFailedAlert()
            case .laUnavailableInstructionAlert: return laUnavailableInstructionAlert()
            case .laUnavailableTurningOffAlert: return laUnavailableTurningOffAlert()
            }
        }
        .sheet(isPresented: $showSetPassword) {
            SetAppPaswordView()
        }
    }

    private func toggleLAMode() {
        if currentLAMode == .password {
            // TODO remove this branch
            currentLAMode = laMode
            privacyLocalAuthModeDefault.set(laMode)
            _ = kcAppPassword.set("")
            return
        }
        authenticate(reason: NSLocalizedString("Change authentication mode", comment: "authentication reason")) { laResult in
            switch laResult {
            case .failed:
                withAnimation { laMode = currentLAMode }
                performLAToggleReset = true
                laAlert = .laFailedAlert
            case .success:
                currentLAMode = laMode
                privacyLocalAuthModeDefault.set(laMode)
                switch laMode {
                case .system: _ = kcAppPassword.set("")
                case .password: showSetPassword = true
                }
            case .unavailable:
                disableUnavailableLA()
            }
        }
    }

    private func changeLAPassword() {
        // TODO request auth
        DispatchQueue.main.async {
            showSetPassword = true
        }
//        authenticate(reason: NSLocalizedString("Change password", comment: "authentication reason")) { laResult in
//            switch laResult {
//            case .failed: laAlert = .laFailedAlert
//            case .success: showSetPassword = true
//            case .unavailable: disableUnavailableLA()
//            }
//        }
    }

    private func enableLA() {
        privacyLocalAuthModeDefault.set(.system)
        laMode = .system
        authenticate(reason: NSLocalizedString("Enable SimpleX Lock", comment: "authentication reason")) { laResult in
            switch laResult {
            case .success:
                prefPerformLA = true
                laAlert = .laTurnedOnAlert
            case .failed:
                prefPerformLA = false
                withAnimation() { performLA = false }
                performLAToggleReset = true
                laAlert = .laFailedAlert
            case .unavailable:
                disableUnavailableLA()
            }
        }
    }

    private func disableUnavailableLA() {
        prefPerformLA = false
        withAnimation() { performLA = false }
        performLAToggleReset = true
        privacyLocalAuthModeDefault.set(.system)
        laMode = .system
        laAlert = .laUnavailableInstructionAlert
    }

    private func disableLA() {
        authenticate(reason: NSLocalizedString("Disable SimpleX Lock", comment: "authentication reason")) { laResult in
            switch (laResult) {
            case .success:
                prefPerformLA = false
                privacyLocalAuthModeDefault.set(.system)
                laMode = .system
            case .failed:
                prefPerformLA = true
                withAnimation() {
                    performLA = true
                }
                performLAToggleReset = true
                laAlert = .laFailedAlert
            case .unavailable:
                prefPerformLA = false
                laAlert = .laUnavailableTurningOffAlert
            }
        }
    }
}

struct PrivacySettings_Previews: PreviewProvider {
    static var previews: some View {
        PrivacySettings()
    }
}
