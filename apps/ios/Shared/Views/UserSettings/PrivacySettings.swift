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
    @EnvironmentObject var m: ChatModel
    @AppStorage(DEFAULT_PRIVACY_ACCEPT_IMAGES) private var autoAcceptImages = true
    @AppStorage(DEFAULT_PRIVACY_LINK_PREVIEWS) private var useLinkPreviews = true
    @State private var simplexLinkMode = privacySimplexLinkModeDefault.get()
    @AppStorage(DEFAULT_PRIVACY_PROTECT_SCREEN) private var protectScreen = false
    @AppStorage(DEFAULT_PERFORM_LA) private var prefPerformLA = false
    @State private var currentLAMode = privacyLocalAuthModeDefault.get()
    @State private var sendReceiptsContacts = false
    @State private var sendReceiptsContactsToSet = false
    @State private var sendReceiptsContactsToggleReset = false
    @State private var showResetContactsReceiptsOverridesDialogue = false
    @State private var alert: PrivacySettingsViewAlert?

    enum PrivacySettingsViewAlert: Identifiable {
        case error(title: LocalizedStringKey, error: LocalizedStringKey = "")

        var id: String {
            switch self {
            case let .error(title, _): return "error \(title)"
            }
        }
    }

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

                Section {
                    settingsRow("person") {
                        Toggle("Contacts", isOn: $sendReceiptsContacts)
                    }
                    settingsRow("person.2") {
                        Toggle("Small groups (max 10)", isOn: Binding.constant(false))
                    }
                    .foregroundColor(.secondary)
                    .disabled(true)
                } header: {
                    Text("Send delivery receipts to")
                } footer: {
                    VStack(alignment: .leading) {
                        Text("These settings are for your current profile **\(ChatModel.shared.currentUser?.displayName ?? "")**.")
                        Text("They can be overridden in contact and group settings")
                    }
                    .frame(maxWidth: .infinity, alignment: .leading)
                }
                .confirmationDialog("You have configured this setting for some contacts", isPresented: $showResetContactsReceiptsOverridesDialogue, titleVisibility: .visible) {
                    Button("Cancel") {
                        sendReceiptsContactsToggleReset = true
                        sendReceiptsContacts.toggle()
                    }
                    Button("Keep per contact overrides") {
                        setSendReceiptsContacts(sendReceiptsContactsToSet, clearOverrides: false)
                    }
                    Button("Reset per contact overrides", role: .destructive) {
                        setSendReceiptsContacts(sendReceiptsContactsToSet, clearOverrides: true)
                    }
                }
            }
        }
        .onAppear {
            if let currentUser = m.currentUser {
                sendReceiptsContactsToggleReset = true
                sendReceiptsContacts = currentUser.sendRcptsContacts
            }
        }
        .onChange(of: sendReceiptsContacts) { sendReceiptsContactsToggle in
            if sendReceiptsContactsToggleReset {
                sendReceiptsContactsToggleReset = false
            } else {
                setOrAskSendReceiptsContacts(sendReceiptsContactsToggle)
            }
        }
        .alert(item: $alert) { alert in
            switch alert {
            case let .error(title, error):
                return Alert(title: Text(title), message: Text(error))
            }
        }
    }

    private func setOrAskSendReceiptsContacts(_ enable: Bool) {
        let allContacts = m.chats.compactMap({ $0.chatInfo.contact })
        if allContacts.allSatisfy({ $0.chatSettings.sendRcpts == nil || $0.chatSettings.sendRcpts == enable }) {
            setSendReceiptsContacts(enable, clearOverrides: false)
        } else {
            sendReceiptsContactsToSet = enable
            showResetContactsReceiptsOverridesDialogue = true
        }
    }

    private func setSendReceiptsContacts(_ enable: Bool, clearOverrides: Bool) {
        Task {
            do {
                if let currentUser = m.currentUser {
                    let userMsgReceiptSettings = UserMsgReceiptSettings(enable: enable, clearOverrides: clearOverrides)
                    try await apiSetUserContactReceipts(currentUser.userId, userMsgReceiptSettings: userMsgReceiptSettings)
                    await MainActor.run {
                        var updatedUser = currentUser
                        updatedUser.sendRcptsContacts = enable
                        m.updateUser(updatedUser)
                    }
                }
            } catch let error {
                alert = .error(title: "Error setting delivery receipts!", error: "Error: \(responseError(error))")
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
    @State private var performLA: Bool = UserDefaults.standard.bool(forKey: DEFAULT_PERFORM_LA)
    @State private var selfDestruct: Bool = UserDefaults.standard.bool(forKey: DEFAULT_LA_SELF_DESTRUCT)
    @State private var currentSelfDestruct: Bool = UserDefaults.standard.bool(forKey: DEFAULT_LA_SELF_DESTRUCT)
    @AppStorage(DEFAULT_LA_SELF_DESTRUCT_DISPLAY_NAME) private var selfDestructDisplayName = ""
    @State private var performLAToggleReset = false
    @State private var performLAModeReset = false
    @State private var performLASelfDestructReset = false
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
        case laSelfDestructPasscodeSetAlert
        case laSelfDestructPasscodeChangedAlert
        case laPasscodeNotChangedAlert

        var id: Self { self }
    }

    enum PasswordAction: Identifiable {
        case enableAuth
        case toggleMode
        case changePasscode
        case enableSelfDestruct
        case changeSelfDestructPasscode
        case selfDestructInfo

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
                            Button("Change passcode") {
                                changeLAPassword()
                            }
                        }
                    }
                }

                if performLA && laMode == .passcode {
                    Section("Self-destruct passcode") {
                        Toggle(isOn: $selfDestruct) {
                            HStack(spacing: 6) {
                                Text("Enable self-destruct")
                                Image(systemName: "info.circle")
                                    .foregroundColor(.accentColor)
                                    .font(.system(size: 14))
                            }
                            .onTapGesture {
                                showPasswordAction = .selfDestructInfo
                            }
                        }
                        if selfDestruct {
                            TextField("New display name", text: $selfDestructDisplayName)
                            Button("Change self-destruct passcode") {
                                changeSelfDestructPassword()
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
            } else if prefPerformLA {
                toggleLAMode()
            } else {
                updateLAMode()
            }
        }
        .onChange(of: selfDestruct) { _ in
            if performLASelfDestructReset {
                performLASelfDestructReset = false
            } else if prefPerformLA {
                toggleSelfDestruct()
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
            case .laSelfDestructPasscodeSetAlert: return selfDestructPasscodeAlert("Self-destruct passcode enabled!")
            case .laSelfDestructPasscodeChangedAlert: return selfDestructPasscodeAlert("Self-destruct passcode changed!")
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
            case .changePasscode:
                SetAppPasscodeView {
                    showLAAlert(.laPasscodeChangedAlert)
                } cancel: {
                    showLAAlert(.laPasscodeNotChangedAlert)
                }
            case .enableSelfDestruct:
                SetAppPasscodeView(passcodeKeychain: kcSelfDestructPassword, title: "Set passcode", reason: NSLocalizedString("Enable self-destruct passcode", comment: "set passcode view")) {
                    updateSelfDestruct()
                    showLAAlert(.laSelfDestructPasscodeSetAlert)
                } cancel: {
                    revertSelfDestruct()
                }
            case .changeSelfDestructPasscode:
                SetAppPasscodeView(passcodeKeychain: kcSelfDestructPassword, reason: NSLocalizedString("Change self-destruct passcode", comment: "set passcode view")) {
                    showLAAlert(.laSelfDestructPasscodeChangedAlert)
                } cancel: {
                    showLAAlert(.laPasscodeNotChangedAlert)
                }
            case .selfDestructInfo:
                selfDestructInfoView()
            }
        }
        .onAppear {
            showChangePassword = prefPerformLA && currentLAMode == .passcode
        }
        .onDisappear() {
            m.laRequest = nil
        }
    }

    private func selfDestructInfoView() -> some View {
        VStack(alignment: .leading) {
            Text("Self-destruct")
                .font(.largeTitle)
                .bold()
                .padding(.vertical)
            ScrollView {
                VStack(alignment: .leading) {
                    Group {
                        Text("If you enter your self-destruct passcode while opening the app:")
                        VStack(spacing: 8) {
                            textListItem("1.", "All app data is deleted.")
                            textListItem("2.", "App passcode is replaced with self-destruct passcode.")
                            textListItem("3.", "An empty chat profile with the provided name is created, and the app opens as usual.")
                        }
                    }
                    .padding(.bottom)
                }
            }
        }
        .frame(maxWidth: .infinity)
        .padding()
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
                            resetSelfDestruct()
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

    private func toggleSelfDestruct() {
        authenticate(reason: NSLocalizedString("Change self-destruct mode", comment: "authentication reason")) { laResult in
            switch laResult {
            case .failed:
                revertSelfDestruct()
                laAlert = .laFailedAlert
            case .success:
                if selfDestruct {
                    showPasswordAction = .enableSelfDestruct
                } else {
                    resetSelfDestruct()
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
            case .success: showPasswordAction = .changePasscode
            case .unavailable: disableUnavailableLA()
            }
        }
    }

    private func changeSelfDestructPassword() {
        authenticate(reason: NSLocalizedString("Change self-destruct passcode", comment: "authentication reason")) { laResult in
            switch laResult {
            case .failed: laAlert = .laFailedAlert
            case .success: showPasswordAction = .changeSelfDestructPasscode
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
        resetSelfDestruct()
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

    private func resetSelfDestruct() {
        _ = kcSelfDestructPassword.remove()
        selfDestruct = false
        updateSelfDestruct()
    }

    private func revertSelfDestruct() {
        performLASelfDestructReset = true
        withAnimation { selfDestruct = currentSelfDestruct }
    }

    private func updateSelfDestruct() {
        UserDefaults.standard.set(selfDestruct, forKey: DEFAULT_LA_SELF_DESTRUCT)
        currentSelfDestruct = selfDestruct
    }

    private func passcodeAlert(_ title: LocalizedStringKey) -> Alert {
        mkAlert(title: title, message: "Please remember or store it securely - there is no way to recover a lost passcode!")
    }

    private func selfDestructPasscodeAlert(_ title: LocalizedStringKey) -> Alert {
        mkAlert(title: title, message: "If you enter this passcode when opening the app, all app data will be irreversibly removed!")
    }
}

struct PrivacySettings_Previews: PreviewProvider {
    static var previews: some View {
        PrivacySettings()
    }
}
