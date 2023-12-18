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
    @AppStorage(DEFAULT_PRIVACY_SHOW_CHAT_PREVIEWS) private var showChatPreviews = true
    @AppStorage(DEFAULT_PRIVACY_SAVE_LAST_DRAFT) private var saveLastDraft = true
    @AppStorage(GROUP_DEFAULT_PRIVACY_ENCRYPT_LOCAL_FILES, store: groupDefaults) private var encryptLocalFiles = true
    @State private var simplexLinkMode = privacySimplexLinkModeDefault.get()
    @AppStorage(DEFAULT_PRIVACY_PROTECT_SCREEN) private var protectScreen = false
    @AppStorage(DEFAULT_PERFORM_LA) private var prefPerformLA = false
    @State private var currentLAMode = privacyLocalAuthModeDefault.get()
    @State private var contactReceipts = false
    @State private var contactReceiptsReset = false
    @State private var contactReceiptsOverrides = 0
    @State private var contactReceiptsDialogue = false
    @State private var groupReceipts = false
    @State private var groupReceiptsReset = false
    @State private var groupReceiptsOverrides = 0
    @State private var groupReceiptsDialogue = false
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
                    settingsRow("lock.doc") {
                        Toggle("Encrypt local files", isOn: $encryptLocalFiles)
                            .onChange(of: encryptLocalFiles) {
                                setEncryptLocalFiles($0)
                            }
                    }
                    settingsRow("photo") {
                        Toggle("Auto-accept images", isOn: $autoAcceptImages)
                            .onChange(of: autoAcceptImages) {
                                privacyAcceptImagesGroupDefault.set($0)
                            }
                    }
                    settingsRow("network") {
                        Toggle("Send link previews", isOn: $useLinkPreviews)
                    }
                    settingsRow("message") {
                        Toggle("Show last messages", isOn: $showChatPreviews)
                    }
                    settingsRow("rectangle.and.pencil.and.ellipsis") {
                        Toggle("Message draft", isOn: $saveLastDraft)
                    }
                    .onChange(of: saveLastDraft) { saveDraft in
                        if !saveDraft {
                            m.draft = nil
                            m.draftChatId = nil
                        }
                    }
                    settingsRow("link") {
                        Picker("SimpleX links", selection: $simplexLinkMode) {
                            ForEach(
                                SimpleXLinkMode.values + (SimpleXLinkMode.values.contains(simplexLinkMode) ? [] : [simplexLinkMode])
                            ) { mode in
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
                }

                Section {
                    settingsRow("person") {
                        Toggle("Contacts", isOn: $contactReceipts)
                    }
                    settingsRow("person.2") {
                        Toggle("Small groups (max 20)", isOn: $groupReceipts)
                    }
                } header: {
                    Text("Send delivery receipts to")
                } footer: {
                    VStack(alignment: .leading) {
                        Text("These settings are for your current profile **\(m.currentUser?.displayName ?? "")**.")
                        Text("They can be overridden in contact and group settings.")
                    }
                    .frame(maxWidth: .infinity, alignment: .leading)
                }
                .confirmationDialog(contactReceiptsDialogTitle, isPresented: $contactReceiptsDialogue, titleVisibility: .visible) {
                    Button(contactReceipts ? "Enable (keep overrides)" : "Disable (keep overrides)") {
                        setSendReceiptsContacts(contactReceipts, clearOverrides: false)
                    }
                    Button(contactReceipts ? "Enable for all" : "Disable for all", role: .destructive) {
                        setSendReceiptsContacts(contactReceipts, clearOverrides: true)
                    }
                    Button("Cancel", role: .cancel) {
                        contactReceiptsReset = true
                        contactReceipts.toggle()
                    }
                }
                .confirmationDialog(groupReceiptsDialogTitle, isPresented: $groupReceiptsDialogue, titleVisibility: .visible) {
                    Button(groupReceipts ? "Enable (keep overrides)" : "Disable (keep overrides)") {
                        setSendReceiptsGroups(groupReceipts, clearOverrides: false)
                    }
                    Button(groupReceipts ? "Enable for all" : "Disable for all", role: .destructive) {
                        setSendReceiptsGroups(groupReceipts, clearOverrides: true)
                    }
                    Button("Cancel", role: .cancel) {
                        groupReceiptsReset = true
                        groupReceipts.toggle()
                    }
                }
            }
        }
        .onChange(of: contactReceipts) { _ in
            if contactReceiptsReset {
                contactReceiptsReset = false
            } else {
                setOrAskSendReceiptsContacts(contactReceipts)
            }
        }
        .onChange(of: groupReceipts) { _ in
            if groupReceiptsReset {
                groupReceiptsReset = false
            } else {
                setOrAskSendReceiptsGroups(groupReceipts)
            }
        }
        .onAppear {
            if let u = m.currentUser {
                if contactReceipts != u.sendRcptsContacts {
                    contactReceiptsReset = true
                    contactReceipts = u.sendRcptsContacts
                }
                if groupReceipts != u.sendRcptsSmallGroups {
                    groupReceiptsReset = true
                    groupReceipts = u.sendRcptsSmallGroups
                }
            }
        }
        .alert(item: $alert) { alert in
            switch alert {
            case let .error(title, error):
                return Alert(title: Text(title), message: Text(error))
            }
        }
    }

    private func setEncryptLocalFiles(_ enable: Bool) {
        do {
            try apiSetEncryptLocalFiles(enable)
        } catch let error {
            let err = responseError(error)
            logger.error("apiSetEncryptLocalFiles \(err)")
            alert = .error(title: "Error", error: "\(err)")
        }
    }

    private func setOrAskSendReceiptsContacts(_ enable: Bool) {
        contactReceiptsOverrides = m.chats.reduce(0) { count, chat in
            let sendRcpts = chat.chatInfo.contact?.chatSettings.sendRcpts
            return count + (sendRcpts == nil || sendRcpts == enable ? 0 : 1)
        }
        if contactReceiptsOverrides == 0 {
            setSendReceiptsContacts(enable, clearOverrides: false)
        } else {
            contactReceiptsDialogue = true
        }
    }

    private var contactReceiptsDialogTitle: LocalizedStringKey {
        contactReceipts
        ? "Sending receipts is disabled for \(contactReceiptsOverrides) contacts"
        : "Sending receipts is enabled for \(contactReceiptsOverrides) contacts"
    }

    private func setSendReceiptsContacts(_ enable: Bool, clearOverrides: Bool) {
        Task {
            do {
                if let currentUser = m.currentUser {
                    let userMsgReceiptSettings = UserMsgReceiptSettings(enable: enable, clearOverrides: clearOverrides)
                    try await apiSetUserContactReceipts(currentUser.userId, userMsgReceiptSettings: userMsgReceiptSettings)
                    privacyDeliveryReceiptsSet.set(true)
                    await MainActor.run {
                        var updatedUser = currentUser
                        updatedUser.sendRcptsContacts = enable
                        m.updateUser(updatedUser)
                        if clearOverrides {
                            m.chats.forEach { chat in
                                if var contact = chat.chatInfo.contact {
                                    let sendRcpts = contact.chatSettings.sendRcpts
                                    if sendRcpts != nil && sendRcpts != enable {
                                        contact.chatSettings.sendRcpts = nil
                                        m.updateContact(contact)
                                    }
                                }
                            }
                        }
                    }
                }
            } catch let error {
                alert = .error(title: "Error setting delivery receipts!", error: "Error: \(responseError(error))")
            }
        }
    }

    private func setOrAskSendReceiptsGroups(_ enable: Bool) {
        groupReceiptsOverrides = m.chats.reduce(0) { count, chat in
            let sendRcpts = chat.chatInfo.groupInfo?.chatSettings.sendRcpts
            return count + (sendRcpts == nil || sendRcpts == enable ? 0 : 1)
        }
        if groupReceiptsOverrides == 0 {
            setSendReceiptsGroups(enable, clearOverrides: false)
        } else {
            groupReceiptsDialogue = true
        }
    }

    private var groupReceiptsDialogTitle: LocalizedStringKey {
        groupReceipts
        ? "Sending receipts is disabled for \(groupReceiptsOverrides) groups"
        : "Sending receipts is enabled for \(groupReceiptsOverrides) groups"
    }

    private func setSendReceiptsGroups(_ enable: Bool, clearOverrides: Bool) {
        Task {
            do {
                if let currentUser = m.currentUser {
                    let userMsgReceiptSettings = UserMsgReceiptSettings(enable: enable, clearOverrides: clearOverrides)
                    try await apiSetUserGroupReceipts(currentUser.userId, userMsgReceiptSettings: userMsgReceiptSettings)
                    privacyDeliveryReceiptsSet.set(true)
                    await MainActor.run {
                        var updatedUser = currentUser
                        updatedUser.sendRcptsSmallGroups = enable
                        m.updateUser(updatedUser)
                        if clearOverrides {
                            m.chats.forEach { chat in
                                if var groupInfo = chat.chatInfo.groupInfo {
                                    let sendRcpts = groupInfo.chatSettings.sendRcpts
                                    if sendRcpts != nil && sendRcpts != enable {
                                        groupInfo.chatSettings.sendRcpts = nil
                                        m.updateGroup(groupInfo)
                                    }
                                }
                            }
                        }
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

    let laDelays: [Int] = [10, 30, 60, 180, 600, 0]

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
                    .frame(height: 36)
                    if performLA {
                        Picker("Lock after", selection: $laLockDelay) {
                            let delays = laDelays.contains(laLockDelay) ? laDelays : [laLockDelay] + laDelays
                            ForEach(delays, id: \.self) { t in
                                Text(laDelayText(t))
                            }
                        }
                        .frame(height: 36)
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
                    m.contentViewAccessAuthenticated = true
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
                m.contentViewAccessAuthenticated = true
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
