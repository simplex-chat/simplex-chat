//
//  ChatInfoView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 05/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

func infoRow(_ title: LocalizedStringKey, _ value: String) -> some View {
    HStack {
        Text(title)
        Spacer()
        Text(value)
            .foregroundStyle(.secondary)
    }
}

func infoRow(_ title: Text, _ value: String) -> some View {
    HStack {
        title
        Spacer()
        Text(value)
            .foregroundStyle(.secondary)
    }
}

func localizedInfoRow(_ title: LocalizedStringKey, _ value: LocalizedStringKey) -> some View {
    HStack {
        Text(title)
        Spacer()
        Text(value)
            .foregroundStyle(.secondary)
    }
}

@ViewBuilder func smpServers(_ title: LocalizedStringKey, _ servers: [String], _ secondaryColor: Color) -> some View {
    if servers.count > 0 {
        HStack {
            Text(title).frame(width: 120, alignment: .leading)
            Button(serverHost(servers[0])) {
                UIPasteboard.general.string = servers.joined(separator: ";")
            }
            .foregroundColor(secondaryColor)
            .lineLimit(1)
        }
    }
}

func serverHost(_ s: String) -> String {
    if let i = s.range(of: "@")?.lowerBound {
        return String(s[i...].dropFirst())
    } else {
        return s
    }
}

enum SendReceipts: Identifiable, Hashable {
    case yes
    case no
    case userDefault(Bool)

    var id: Self { self }

    var text: LocalizedStringKey {
        switch self {
        case .yes: return "yes"
        case .no: return "no"
        case let .userDefault(on): return on ? "default (yes)" : "default (no)"
        }
    }

    func bool() -> Bool? {
        switch self {
        case .yes: return true
        case .no: return false
        case .userDefault: return nil
        }
    }

    static func fromBool(_ enable: Bool?, userDefault def: Bool) -> SendReceipts {
        if let enable = enable {
            return enable ? .yes : .no
        }
        return .userDefault(def)
    }
}

struct ChatInfoView: View {
    @EnvironmentObject var chatModel: ChatModel
    @EnvironmentObject var theme: AppTheme
    @Environment(\.dismiss) var dismiss: DismissAction
    @ObservedObject var chat: Chat
    @State var contact: Contact
    @State var localAlias: String
    var onSearch: () -> Void
    @State private var connectionStats: ConnectionStats? = nil
    @State private var customUserProfile: Profile? = nil
    @State private var connectionCode: String? = nil
    @FocusState private var aliasTextFieldFocused: Bool
    @State private var alert: ChatInfoViewAlert? = nil
    @State private var actionSheet: SomeActionSheet? = nil
    @State private var sheet: SomeSheet<AnyView>? = nil
    @State private var showConnectContactViaAddressDialog = false
    @State private var sendReceipts = SendReceipts.userDefault(true)
    @State private var sendReceiptsUserDefault = true
    @AppStorage(DEFAULT_DEVELOPER_TOOLS) private var developerTools = false
    
    enum ChatInfoViewAlert: Identifiable {
        case clearChatAlert
        case networkStatusAlert
        case switchAddressAlert
        case abortSwitchAddressAlert
        case syncConnectionForceAlert
        case queueInfo(info: String)
        case someAlert(alert: SomeAlert)
        case error(title: LocalizedStringKey, error: LocalizedStringKey?)

        var id: String {
            switch self {
            case .clearChatAlert: return "clearChatAlert"
            case .networkStatusAlert: return "networkStatusAlert"
            case .switchAddressAlert: return "switchAddressAlert"
            case .abortSwitchAddressAlert: return "abortSwitchAddressAlert"
            case .syncConnectionForceAlert: return "syncConnectionForceAlert"
            case let .queueInfo(info): return "queueInfo \(info)"
            case let .someAlert(alert): return "chatInfoSomeAlert \(alert.id)"
            case let .error(title, _): return "error \(title)"
            }
        }
    }
    
    var body: some View {
        NavigationView {
            List {
                contactInfoHeader()
                    .listRowBackground(Color.clear)
                    .contentShape(Rectangle())
                    .onTapGesture {
                        aliasTextFieldFocused = false
                    }
                
                Group {
                    localAliasTextEdit()
                }
                .listRowBackground(Color.clear)
                .listRowSeparator(.hidden)
                
                HStack {
                    Spacer()
                    searchButton()
                    Spacer()
                    AudioCallButton(chat: chat, contact: contact, showAlert: { alert = .someAlert(alert: $0) })
                    Spacer()
                    VideoButton(chat: chat, contact: contact, showAlert: { alert = .someAlert(alert: $0) })
                    Spacer()
                    muteButton()
                    Spacer()
                }
                .padding(.horizontal)
                .listRowBackground(Color.clear)
                .listRowSeparator(.hidden)
                
                if let customUserProfile = customUserProfile {
                    Section(header: Text("Incognito").foregroundColor(theme.colors.secondary)) {
                        HStack {
                            Text("Your random profile")
                            Spacer()
                            Text(customUserProfile.chatViewName)
                                .foregroundStyle(.indigo)
                        }
                    }
                }
                
                Section {
                    Group {
                        if let code = connectionCode { verifyCodeButton(code) }
                        contactPreferencesButton()
                        sendReceiptsOption()
                        if let connStats = connectionStats,
                           connStats.ratchetSyncAllowed {
                            synchronizeConnectionButton()
                        }
                        // } else if developerTools {
                        //     synchronizeConnectionButtonForce()
                        // }
                    }
                    .disabled(!contact.ready || !contact.active)
                    NavigationLink {
                        ChatWallpaperEditorSheet(chat: chat)
                    } label: {
                        Label("Chat theme", systemImage: "photo")
                    }
                    //                    } else if developerTools {
                    //                        synchronizeConnectionButtonForce()
                    //                    }
                }
                .disabled(!contact.ready || !contact.active)
                
                if let conn = contact.activeConn {
                    Section {
                        infoRow(Text(String("E2E encryption")), conn.connPQEnabled ? "Quantum resistant" : "Standard")
                    }
                }
                
                if let contactLink = contact.contactLink {
                    Section {
                        SimpleXLinkQRCode(uri: contactLink)
                        Button {
                            showShareSheet(items: [simplexChatLink(contactLink)])
                        } label: {
                            Label("Share address", systemImage: "square.and.arrow.up")
                        }
                    } header: {
                        Text("Address")
                            .foregroundColor(theme.colors.secondary)
                    } footer: {
                        Text("You can share this address with your contacts to let them connect with **\(contact.displayName)**.")
                            .foregroundColor(theme.colors.secondary)
                    }
                }
                
                if contact.ready && contact.active {
                    Section(header: Text("Servers").foregroundColor(theme.colors.secondary)) {
                        networkStatusRow()
                            .onTapGesture {
                                alert = .networkStatusAlert
                            }
                        if let connStats = connectionStats {
                            Button("Change receiving address") {
                                alert = .switchAddressAlert
                            }
                            .disabled(
                                connStats.rcvQueuesInfo.contains { $0.rcvSwitchStatus != nil }
                                || connStats.ratchetSyncSendProhibited
                            )
                            if connStats.rcvQueuesInfo.contains(where: { $0.rcvSwitchStatus != nil }) {
                                Button("Abort changing address") {
                                    alert = .abortSwitchAddressAlert
                                }
                                .disabled(
                                    connStats.rcvQueuesInfo.contains { $0.rcvSwitchStatus != nil && !$0.canAbortSwitch }
                                    || connStats.ratchetSyncSendProhibited
                                )
                            }
                            smpServers("Receiving via", connStats.rcvQueuesInfo.map { $0.rcvServer }, theme.colors.secondary)
                            smpServers("Sending via", connStats.sndQueuesInfo.map { $0.sndServer }, theme.colors.secondary)
                        }
                    }
                }
                
                Section {
                    clearChatButton()
                    deleteContactButton()
                }
                
                if developerTools {
                    Section(header: Text("For console").foregroundColor(theme.colors.secondary)) {
                        infoRow("Local name", chat.chatInfo.localDisplayName)
                        infoRow("Database ID", "\(chat.chatInfo.apiId)")
                        Button ("Debug delivery") {
                            Task {
                                do {
                                    let info = queueInfoText(try await apiContactQueueInfo(chat.chatInfo.apiId))
                                    await MainActor.run { alert = .queueInfo(info: info) }
                                } catch let e {
                                    logger.error("apiContactQueueInfo error: \(responseError(e))")
                                    let a = getErrorAlert(e, "Error")
                                    await MainActor.run { alert = .error(title: a.title, error: a.message) }
                                }
                            }
                        }
                    }
                }
            }
            .modifier(ThemedBackground(grouped: true))
            .navigationBarHidden(true)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
        .onAppear {
            if let currentUser = chatModel.currentUser {
                sendReceiptsUserDefault = currentUser.sendRcptsContacts
            }
            sendReceipts = SendReceipts.fromBool(contact.chatSettings.sendRcpts, userDefault: sendReceiptsUserDefault)
            
            
            Task {
                do {
                    let (stats, profile) = try await apiContactInfo(chat.chatInfo.apiId)
                    let (ct, code) = try await apiGetContactCode(chat.chatInfo.apiId)
                    await MainActor.run {
                        connectionStats = stats
                        customUserProfile = profile
                        connectionCode = code
                        if contact.activeConn?.connectionCode != ct.activeConn?.connectionCode {
                            chat.chatInfo = .direct(contact: ct)
                        }
                    }
                } catch let error {
                    logger.error("apiContactInfo or apiGetContactCode error: \(responseError(error))")
                }
            }
        }
        .alert(item: $alert) { alertItem in
            switch(alertItem) {
            case .clearChatAlert: return clearChatAlert()
            case .networkStatusAlert: return networkStatusAlert()
            case .switchAddressAlert: return switchAddressAlert(switchContactAddress)
            case .abortSwitchAddressAlert: return abortSwitchAddressAlert(abortSwitchContactAddress)
            case .syncConnectionForceAlert: return syncConnectionForceAlert({ syncContactConnection(force: true) })
            case let .queueInfo(info): return queueInfoAlert(info)
            case let .someAlert(a): return a.alert
            case let .error(title, error): return mkAlert(title: title, message: error)
            }
        }
        .actionSheet(item: $actionSheet) { $0.actionSheet }
        .sheet(item: $sheet) { 
            if #available(iOS 16.0, *) {
                $0.content
                    .presentationDetents([.fraction(0.4)])
            } else {
                $0.content
            }
        }
    }
    
    private func contactInfoHeader() -> some View {
        VStack {
            let cInfo = chat.chatInfo
            ChatInfoImage(chat: chat, size: 192, color: Color(uiColor: .tertiarySystemFill))
                .padding(.top, 12)
                .padding()
            if contact.verified {
                (
                    Text(Image(systemName: "checkmark.shield"))
                        .foregroundColor(theme.colors.secondary)
                        .font(.title2)
                    + Text(" ")
                    + Text(contact.profile.displayName)
                        .font(.largeTitle)
                )
                .multilineTextAlignment(.center)
                .lineLimit(2)
                .padding(.bottom, 2)
            } else {
                Text(contact.profile.displayName)
                    .font(.largeTitle)
                    .multilineTextAlignment(.center)
                    .lineLimit(2)
                    .padding(.bottom, 2)
            }
            if cInfo.fullName != "" && cInfo.fullName != cInfo.displayName && cInfo.fullName != contact.profile.displayName {
                Text(cInfo.fullName)
                    .font(.title2)
                    .multilineTextAlignment(.center)
                    .lineLimit(4)
            }
        }
        .frame(maxWidth: .infinity, alignment: .center)
    }
    
    private func localAliasTextEdit() -> some View {
        TextField("Set contact name…", text: $localAlias)
            .disableAutocorrection(true)
            .focused($aliasTextFieldFocused)
            .submitLabel(.done)
            .onChange(of: aliasTextFieldFocused) { focused in
                if !focused {
                    setContactAlias()
                }
            }
            .onSubmit {
                setContactAlias()
            }
            .multilineTextAlignment(.center)
            .foregroundColor(theme.colors.secondary)
    }
    
    private func setContactAlias() {
        Task {
            do {
                if let contact = try await apiSetContactAlias(contactId: chat.chatInfo.apiId, localAlias: localAlias) {
                    await MainActor.run {
                        chatModel.updateContact(contact)
                    }
                }
            } catch {
                logger.error("setContactAlias error: \(responseError(error))")
            }
        }
    }

    private func searchButton() -> some View {
        InfoViewActionButtonLayout(image: "magnifyingglass", title: "open")
            .onTapGesture {
                dismiss()
                onSearch()
            }
            .disabled(!contact.ready || chat.chatItems.isEmpty)
    }

    private func muteButton() -> some View {
        InfoViewActionButtonLayout(
            image: chat.chatInfo.ntfsEnabled ? "speaker.slash" : "speaker.wave.2",
            title: chat.chatInfo.ntfsEnabled ? "mute" : "unmute"
        )
        .onTapGesture {
            toggleNotifications(chat, enableNtfs: !chat.chatInfo.ntfsEnabled)
        }
        .disabled(!contact.ready || !contact.active)
    }

    private func verifyCodeButton(_ code: String) -> some View {
        NavigationLink {
            VerifyCodeView(
                displayName: contact.displayName,
                connectionCode: code,
                connectionVerified: contact.verified,
                verify: { code in
                    if let r = apiVerifyContact(chat.chatInfo.apiId, connectionCode: code) {
                        let (verified, existingCode) = r
                        contact.activeConn?.connectionCode = verified ? SecurityCode(securityCode: existingCode, verifiedAt: .now) : nil
                        connectionCode = existingCode
                        DispatchQueue.main.async {
                            chat.chatInfo = .direct(contact: contact)
                        }
                        return r
                    }
                    return nil
                }
            )
            .navigationBarTitleDisplayMode(.inline)
            .navigationTitle("Security code")
            .modifier(ThemedBackground(grouped: true))
        } label: {
            Label(
                contact.verified ? "View security code" : "Verify security code",
                systemImage: contact.verified ? "checkmark.shield" : "shield"
            )
        }
    }
    
    private func contactPreferencesButton() -> some View {
        NavigationLink {
            ContactPreferencesView(
                contact: $contact,
                featuresAllowed: contactUserPrefsToFeaturesAllowed(contact.mergedPreferences),
                currentFeaturesAllowed: contactUserPrefsToFeaturesAllowed(contact.mergedPreferences)
            )
            .navigationBarTitle("Contact preferences")
            .modifier(ThemedBackground(grouped: true))
            .navigationBarTitleDisplayMode(.large)
        } label: {
            Label("Contact preferences", systemImage: "switch.2")
        }
    }
    
    private func sendReceiptsOption() -> some View {
        Picker(selection: $sendReceipts) {
            ForEach([.yes, .no, .userDefault(sendReceiptsUserDefault)]) { (opt: SendReceipts) in
                Text(opt.text)
            }
        } label: {
            Label("Send receipts", systemImage: "checkmark.message")
        }
        .frame(height: 36)
        .onChange(of: sendReceipts) { _ in
            setSendReceipts()
        }
    }
    
    private func setSendReceipts() {
        var chatSettings = chat.chatInfo.chatSettings ?? ChatSettings.defaults
        chatSettings.sendRcpts = sendReceipts.bool()
        updateChatSettings(chat, chatSettings: chatSettings)
    }
    
    private func synchronizeConnectionButton() -> some View {
        Button {
            syncContactConnection(force: false)
        } label: {
            Label("Fix connection", systemImage: "exclamationmark.arrow.triangle.2.circlepath")
                .foregroundColor(.orange)
        }
    }
    
    private func synchronizeConnectionButtonForce() -> some View {
        Button {
            alert = .syncConnectionForceAlert
        } label: {
            Label("Renegotiate encryption", systemImage: "exclamationmark.triangle")
                .foregroundColor(.red)
        }
    }
    
    private func networkStatusRow() -> some View {
        HStack {
            Text("Network status")
            Image(systemName: "info.circle")
                .foregroundColor(theme.colors.primary)
                .font(.system(size: 14))
            Spacer()
            Text(chatModel.contactNetworkStatus(contact).statusString)
                .foregroundColor(theme.colors.secondary)
            serverImage()
        }
    }
    
    private func serverImage() -> some View {
        let status = chatModel.contactNetworkStatus(contact)
        return Image(systemName: status.imageName)
            .foregroundColor(status == .connected ? .green : theme.colors.secondary)
            .font(.system(size: 12))
    }
    
    private func deleteContactButton() -> some View {
        Button(role: .destructive) {
            deleteContactDialog(
                chat,
                contact,
                showAlert: { alert = .someAlert(alert: $0) },
                showActionSheet: { actionSheet = $0 },
                showSheetContent: { sheet = $0 }
            )
        } label: {
            Label("Delete contact", systemImage: "person.badge.minus")
                .foregroundColor(Color.red)
        }
    }
    
    private func clearChatButton() -> some View {
        Button() {
            alert = .clearChatAlert
        } label: {
            Label("Clear conversation", systemImage: "gobackward")
                .foregroundColor(Color.orange)
        }
    }
    
    private func clearChatAlert() -> Alert {
        Alert(
            title: Text("Clear conversation?"),
            message: Text("All messages will be deleted - this cannot be undone! The messages will be deleted ONLY for you."),
            primaryButton: .destructive(Text("Clear")) {
                Task {
                    await clearChat(chat)
                    await MainActor.run { dismiss() }
                }
            },
            secondaryButton: .cancel()
        )
    }
    
    private func networkStatusAlert() -> Alert {
        Alert(
            title: Text("Network status"),
            message: Text(chatModel.contactNetworkStatus(contact).statusExplanation)
        )
    }
    
    private func switchContactAddress() {
        Task {
            do {
                let stats = try apiSwitchContact(contactId: contact.apiId)
                connectionStats = stats
                await MainActor.run {
                    chatModel.updateContactConnectionStats(contact, stats)
                    dismiss()
                }
            } catch let error {
                logger.error("switchContactAddress apiSwitchContact error: \(responseError(error))")
                let a = getErrorAlert(error, "Error changing address")
                await MainActor.run {
                    alert = .error(title: a.title, error: a.message)
                }
            }
        }
    }
    
    private func abortSwitchContactAddress() {
        Task {
            do {
                let stats = try apiAbortSwitchContact(contact.apiId)
                connectionStats = stats
                await MainActor.run {
                    chatModel.updateContactConnectionStats(contact, stats)
                }
            } catch let error {
                logger.error("abortSwitchContactAddress apiAbortSwitchContact error: \(responseError(error))")
                let a = getErrorAlert(error, "Error aborting address change")
                await MainActor.run {
                    alert = .error(title: a.title, error: a.message)
                }
            }
        }
    }
    
    private func syncContactConnection(force: Bool) {
        Task {
            do {
                let stats = try apiSyncContactRatchet(contact.apiId, force)
                connectionStats = stats
                await MainActor.run {
                    chatModel.updateContactConnectionStats(contact, stats)
                    dismiss()
                }
            } catch let error {
                logger.error("syncContactConnection apiSyncContactRatchet error: \(responseError(error))")
                let a = getErrorAlert(error, "Error synchronizing connection")
                await MainActor.run {
                    alert = .error(title: a.title, error: a.message)
                }
            }
        }
    }
}

struct AudioCallButton: View {
    var chat: Chat
    var contact: Contact
    var showAlert: (SomeAlert) -> Void

    var body: some View {
        CallButton(
            chat: chat,
            contact: contact,
            image: "phone",
            title: "call",
            mediaType: .audio,
            showAlert: showAlert
        )
    }
}

struct VideoButton: View {
    var chat: Chat
    var contact: Contact
    var showAlert: (SomeAlert) -> Void

    var body: some View {
        CallButton(
            chat: chat,
            contact: contact,
            image: "video",
            title: "video",
            mediaType: .video,
            showAlert: showAlert
        )
    }
}

private struct CallButton: View {
    var chat: Chat
    var contact: Contact
    var image: String
    var title: LocalizedStringKey
    var mediaType: CallMediaType
    var showAlert: (SomeAlert) -> Void

    var body: some View {
        let canCall = contact.ready && contact.active && chat.chatInfo.featureEnabled(.calls) && ChatModel.shared.activeCall == nil

        InfoViewActionButtonLayout(image: image, title: title, disabledLook: !canCall)
            .onTapGesture {
                if canCall {
                    CallController.shared.startCall(contact, mediaType)
                } else if contact.nextSendGrpInv {
                    showAlert(SomeAlert(
                        alert: mkAlert(
                            title: "Can't call contact",
                            message: "Send message to enable calls."
                        ),
                        id: "can't call contact, send message"
                    ))
                } else if !contact.active {
                    showAlert(SomeAlert(
                        alert: mkAlert(
                            title: "Can't call contact",
                            message: "Contact is deleted."
                        ),
                        id: "can't call contact, contact deleted"
                    ))
                } else if !contact.ready {
                    showAlert(SomeAlert(
                        alert: mkAlert(
                            title: "Can't call contact",
                            message: "Connecting to contact, please wait or check later!"
                        ),
                        id: "can't call contact, contact not ready"
                    ))
                } else if !chat.chatInfo.featureEnabled(.calls) {
                    switch chat.chatInfo.showEnableCallsAlert {
                    case .userEnable:
                        showAlert(SomeAlert(
                            alert: Alert(
                                title: Text("Allow calls?"),
                                message: Text("You need to allow your contact to call to be able to call them."),
                                primaryButton: .default(Text("Allow")) {
                                    allowFeatureToContact(contact, .calls)
                                },
                                secondaryButton: .cancel()
                            ),
                            id: "allow calls"
                        ))
                    case .askContact:
                        showAlert(SomeAlert(
                            alert: mkAlert(
                                title: "Calls prohibited!",
                                message: "Please ask your contact to enable calls."
                            ),
                            id: "calls prohibited, ask contact"
                        ))
                    case .other:
                        showAlert(SomeAlert(
                            alert: mkAlert(
                                title: "Calls prohibited!",
                                message: "Please check yours and your contact preferences."
                            )
                            , id: "calls prohibited, ask contact"
                        ))
                    }
                } else {
                    showAlert(SomeAlert(
                        alert: mkAlert(title: "Can't call contact"),
                        id: "calls prohibited, ask contact"
                    ))
                }
            }
            .disabled(ChatModel.shared.activeCall != nil)
    }
}

struct InfoViewActionButtonLayout: View {
    var image: String
    var title: LocalizedStringKey
    var disabledLook: Bool = false

    var body: some View {
        VStack(spacing: 4) {
            Image(systemName: image)
                .resizable()
                .scaledToFit()
                .frame(width: 20, height: 20)
            Text(title)
                .font(.caption)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .foregroundColor(.accentColor)
        .background(Color(.secondarySystemGroupedBackground))
        .cornerRadius(12.0)
        .frame(width: 82, height: 56)
        .disabled(disabledLook)
    }
}

struct ChatWallpaperEditorSheet: View {
    @Environment(\.dismiss) var dismiss
    @EnvironmentObject var theme: AppTheme
    @State private var globalThemeUsed: Bool = false
    @State var chat: Chat
    @State private var themes: ThemeModeOverrides

    init(chat: Chat) {
        self.chat = chat
        self.themes = if case let ChatInfo.direct(contact) = chat.chatInfo, let uiThemes = contact.uiThemes {
            uiThemes
        } else if case let ChatInfo.group(groupInfo) = chat.chatInfo, let uiThemes = groupInfo.uiThemes {
            uiThemes
        } else {
            ThemeModeOverrides()
        }
    }

    var body: some View {
        let preferred = themes.preferredMode(!theme.colors.isLight)
        let initialTheme = preferred ?? ThemeManager.defaultActiveTheme(ChatModel.shared.currentUser?.uiThemes, themeOverridesDefault.get())
        ChatWallpaperEditor(
            initialTheme: initialTheme,
            themeModeOverride: initialTheme,
            applyToMode: themes.light == themes.dark ? nil : initialTheme.mode,
            globalThemeUsed: $globalThemeUsed,
            save: { applyToMode, newTheme in
                await save(applyToMode, newTheme, $chat)
            }
        )
        .navigationTitle("Chat theme")
        .modifier(ThemedBackground(grouped: true))
        .navigationBarTitleDisplayMode(.inline)
        .onAppear {
            globalThemeUsed = preferred == nil
        }
        .onChange(of: theme.base.mode) { _ in
            globalThemeUsed = themesFromChat(chat).preferredMode(!theme.colors.isLight) == nil
        }
        .onChange(of: ChatModel.shared.chatId) { _ in
            dismiss()
        }
    }

    private func themesFromChat(_ chat: Chat) -> ThemeModeOverrides {
        if case let ChatInfo.direct(contact) = chat.chatInfo, let uiThemes = contact.uiThemes {
            uiThemes
        } else if case let ChatInfo.group(groupInfo) = chat.chatInfo, let uiThemes = groupInfo.uiThemes {
            uiThemes
        } else {
            ThemeModeOverrides()
        }
    }

    private static var updateBackendTask: Task = Task {}
    private func save(
        _ applyToMode: DefaultThemeMode?,
        _ newTheme: ThemeModeOverride?,
        _ chat: Binding<Chat>
    ) async {
        let unchangedThemes: ThemeModeOverrides = themesFromChat(chat.wrappedValue)
        var wallpaperFiles = Set([unchangedThemes.light?.wallpaper?.imageFile, unchangedThemes.dark?.wallpaper?.imageFile])
        var changedThemes: ThemeModeOverrides? = unchangedThemes
        let light: ThemeModeOverride? = if let newTheme {
            ThemeModeOverride(mode: DefaultThemeMode.light, colors: newTheme.colors, wallpaper: newTheme.wallpaper?.withFilledWallpaperPath())
        } else {
            nil
        }
        let dark: ThemeModeOverride? = if let newTheme {
            ThemeModeOverride(mode: DefaultThemeMode.dark, colors: newTheme.colors, wallpaper: newTheme.wallpaper?.withFilledWallpaperPath())
        } else {
            nil
        }

        if let applyToMode {
            switch applyToMode {
            case DefaultThemeMode.light:
                changedThemes?.light = light
            case DefaultThemeMode.dark:
                changedThemes?.dark = dark
            }
        } else {
            changedThemes?.light = light
            changedThemes?.dark = dark
        }
        if changedThemes?.light != nil || changedThemes?.dark != nil {
            let light = changedThemes?.light
            let dark = changedThemes?.dark
            let currentMode = CurrentColors.base.mode
            // same image file for both modes, copy image to make them as different files
            if var light, var dark, let lightWallpaper = light.wallpaper, let darkWallpaper = dark.wallpaper, let lightImageFile = lightWallpaper.imageFile, let darkImageFile = darkWallpaper.imageFile, lightWallpaper.imageFile == darkWallpaper.imageFile {
                let imageFile = if currentMode == DefaultThemeMode.light {
                    darkImageFile
                } else {
                    lightImageFile
                }
                let filePath = saveWallpaperFile(url: getWallpaperFilePath(imageFile))
                if currentMode == DefaultThemeMode.light {
                    dark.wallpaper?.imageFile = filePath
                    changedThemes = ThemeModeOverrides(light: changedThemes?.light, dark: dark)
                } else {
                    light.wallpaper?.imageFile = filePath
                    changedThemes = ThemeModeOverrides(light: light, dark: changedThemes?.dark)
                }
            }
        } else {
            changedThemes = nil
        }
        wallpaperFiles.remove(changedThemes?.light?.wallpaper?.imageFile)
        wallpaperFiles.remove(changedThemes?.dark?.wallpaper?.imageFile)
        wallpaperFiles.forEach(removeWallpaperFile)

        let changedThemesConstant = changedThemes
        ChatWallpaperEditorSheet.updateBackendTask.cancel()
        ChatWallpaperEditorSheet.updateBackendTask = Task {
            do {
                try await Task.sleep(nanoseconds: 300_000000)
                if await apiSetChatUIThemes(chatId: chat.id, themes: changedThemesConstant) {
                    if case var ChatInfo.direct(contact) = chat.wrappedValue.chatInfo {
                        contact.uiThemes = changedThemesConstant
                        await MainActor.run {
                            ChatModel.shared.updateChatInfo(ChatInfo.direct(contact: contact))
                            chat.wrappedValue = Chat.init(chatInfo: ChatInfo.direct(contact: contact))
                            themes = themesFromChat(chat.wrappedValue)
                        }
                    } else if case var ChatInfo.group(groupInfo) = chat.wrappedValue.chatInfo {
                        groupInfo.uiThemes = changedThemesConstant

                        await MainActor.run {
                            ChatModel.shared.updateChatInfo(ChatInfo.group(groupInfo: groupInfo))
                            chat.wrappedValue = Chat.init(chatInfo: ChatInfo.group(groupInfo: groupInfo))
                            themes = themesFromChat(chat.wrappedValue)
                        }
                    }
                }
            } catch {
                // canceled task
            }
        }
    }
}

func switchAddressAlert(_ switchAddress: @escaping () -> Void) -> Alert {
    Alert(
        title: Text("Change receiving address?"),
        message: Text("Receiving address will be changed to a different server. Address change will complete after sender comes online."),
        primaryButton: .default(Text("Change"), action: switchAddress),
        secondaryButton: .cancel()
    )
}

func abortSwitchAddressAlert(_ abortSwitchAddress: @escaping () -> Void) -> Alert {
    Alert(
        title: Text("Abort changing address?"),
        message: Text("Address change will be aborted. Old receiving address will be used."),
        primaryButton: .destructive(Text("Abort"), action: abortSwitchAddress),
        secondaryButton: .cancel()
    )
}

func syncConnectionForceAlert(_ syncConnectionForce: @escaping () -> Void) -> Alert {
    Alert(
        title: Text("Renegotiate encryption?"),
        message: Text("The encryption is working and the new encryption agreement is not required. It may result in connection errors!"),
        primaryButton: .destructive(Text("Renegotiate"), action: syncConnectionForce),
        secondaryButton: .cancel()
    )
}

func queueInfoText(_ info: (RcvMsgInfo?, QueueInfo)) -> String {
    let (rcvMsgInfo, qInfo) = info
    var msgInfo: String
    if let rcvMsgInfo { msgInfo = encodeJSON(rcvMsgInfo) } else { msgInfo = "none" }
    return String.localizedStringWithFormat(NSLocalizedString("server queue info: %@\n\nlast received msg: %@", comment: "queue info"), encodeJSON(qInfo), msgInfo)
}

func queueInfoAlert(_ info: String) -> Alert {
    Alert(
        title: Text("Message queue info"),
        message: Text(info),
        primaryButton: .default(Text("Ok")),
        secondaryButton: .default(Text("Copy")) { UIPasteboard.general.string = info }
    )
}

func deleteContactDialog(
    _ chat: Chat,
    _ contact: Contact,
    showAlert: @escaping (SomeAlert) -> Void,
    showActionSheet: @escaping (SomeActionSheet) -> Void,
    showSheetContent: @escaping (SomeSheet<AnyView>) -> Void
) {
    if contact.sndReady && contact.active && !contact.chatDeleted {
        deleteContactOrConversationDialog(chat, contact, showAlert, showActionSheet, showSheetContent)
    } else if contact.sndReady && contact.active && contact.chatDeleted {
        deleteContactWithoutConversation(chat, contact, showAlert, showActionSheet)
    } else { // !(contact.sndReady && contact.active)
        deleteNotReadyContact(chat, contact, showAlert, showActionSheet)
    }
}

private func deleteContactOrConversationDialog(
    _ chat: Chat,
    _ contact: Contact,
    _ showAlert: @escaping (SomeAlert) -> Void,
    _ showActionSheet: @escaping (SomeActionSheet) -> Void,
    _ showSheetContent: @escaping (SomeSheet<AnyView>) -> Void
) {
    showActionSheet(SomeActionSheet(
        actionSheet: ActionSheet(
            title: Text("Delete contact?"),
            buttons: [
                .destructive(Text("Only delete conversation")) {
                    deleteContactMaybeErrorAlert(chat, contact, chatDeleteMode: .messages, showAlert)
                },
                .destructive(Text("Delete contact")) {
                    showSheetContent(SomeSheet(
                        content: { AnyView(
                            DeleteActiveContactDialog(
                                chat: chat,
                                contact: contact,
                                showAlert: showAlert
                            )
                        ) },
                        id: "DeleteActiveContactDialog"
                    ))
                },
                .cancel()
            ]
        ),
        id: "deleteContactOrConversationDialog"
    ))
}

private func deleteContactMaybeErrorAlert(
    _ chat: Chat,
    _ contact: Contact,
    chatDeleteMode: ChatDeleteMode,
    _ showAlert: @escaping (SomeAlert) -> Void
) {
    Task {
        let alert_ = await deleteContactChat(chat, chatDeleteMode: chatDeleteMode)
        if let alert = alert_ {
            showAlert(SomeAlert(alert: alert, id: "deleteContactMaybeErrorAlert"))
        } else {
            await MainActor.run {
                ChatModel.shared.chatId = nil
            }
            DispatchQueue.main.async {
                dismissAllSheets(animated: true) {
                    if case .messages = chatDeleteMode, showDeleteConversationNoticeDefault.get() {
                        AlertManager.shared.showAlert(deleteConversationNotice(contact))
                    } else if chatDeleteMode.isEntity, showDeleteContactNoticeDefault.get() {
                        AlertManager.shared.showAlert(deleteContactNotice(contact))
                    }
                }
            }
        }
    }
}

private func deleteConversationNotice(_ contact: Contact) -> Alert {
    return Alert(
        title: Text("Conversation deleted!"),
        message: Text("You can still send messages to \(contact.displayName) from the Deleted chats."),
        primaryButton: .default(Text("Don't show again")) {
            showDeleteConversationNoticeDefault.set(false)
        },
        secondaryButton: .default(Text("Ok"))
    )
}

private func deleteContactNotice(_ contact: Contact) -> Alert {
    return Alert(
        title: Text("Contact deleted!"),
        message: Text("You can still view conversation with \(contact.displayName) in the list of chats."),
        primaryButton: .default(Text("Don't show again")) {
            showDeleteContactNoticeDefault.set(false)
        },
        secondaryButton: .default(Text("Ok"))
    )
}

enum ContactDeleteMode {
    case full
    case entity

    public func toChatDeleteMode(notify: Bool) -> ChatDeleteMode {
        switch self {
        case .full: .full(notify: notify)
        case .entity: .entity(notify: notify)
        }
    }
}

struct DeleteActiveContactDialog: View {
    @EnvironmentObject var theme: AppTheme
    var chat: Chat
    var contact: Contact
    var showAlert: (SomeAlert) -> Void
    @State private var keepConversation = false

    var body: some View {
        NavigationView {
            List {
                Section {
                    Toggle("Keep conversation", isOn: $keepConversation)

                    Button(role: .destructive) {
                        deleteContactMaybeErrorAlert(chat, contact, chatDeleteMode: contactDeleteMode.toChatDeleteMode(notify: false), showAlert)
                    } label: {
                        Text("Delete without notification")
                    }

                    Button(role: .destructive) {
                        deleteContactMaybeErrorAlert(chat, contact, chatDeleteMode: contactDeleteMode.toChatDeleteMode(notify: true), showAlert)
                    } label: {
                        Text("Delete and notify contact")
                    }
                } footer: {
                    Text("Contact will be deleted - this cannot be undone!")
                        .foregroundColor(theme.colors.secondary)
                }
            }
        }
        .modifier(ThemedBackground(grouped: true))
    }

    var contactDeleteMode: ContactDeleteMode {
        keepConversation ? .entity : .full
    }
}

private func deleteContactWithoutConversation(
    _ chat: Chat,
    _ contact: Contact,
    _ showAlert: @escaping (SomeAlert) -> Void,
    _ showActionSheet: @escaping (SomeActionSheet) -> Void
) {
    showActionSheet(SomeActionSheet(
        actionSheet: ActionSheet(
            title: Text("Confirm contact deletion?"),
            buttons: [
                .destructive(Text("Delete and notify contact")) {
                    deleteContactMaybeErrorAlert(chat, contact, chatDeleteMode: .full(notify: true), showAlert)
                },
                .destructive(Text("Delete without notification")) {
                    deleteContactMaybeErrorAlert(chat, contact, chatDeleteMode: .full(notify: false), showAlert)
                },
                .cancel()
            ]
        ),
        id: "deleteContactWithoutConversation"
    ))
}

private func deleteNotReadyContact(
    _ chat: Chat,
    _ contact: Contact,
    _ showAlert: @escaping (SomeAlert) -> Void,
    _ showActionSheet: @escaping (SomeActionSheet) -> Void
) {
    showActionSheet(SomeActionSheet(
        actionSheet: ActionSheet(
            title: Text("Confirm contact deletion?"),
            buttons: [
                .destructive(Text("Confirm")) {
                    deleteContactMaybeErrorAlert(chat, contact, chatDeleteMode: .full(notify: false), showAlert)
                },
                .cancel()
            ]
        ),
        id: "deleteNotReadyContact"
    ))
}

struct ChatInfoView_Previews: PreviewProvider {
    static var previews: some View {
        ChatInfoView(
            chat: Chat(chatInfo: ChatInfo.sampleData.direct, chatItems: []),
            contact: Contact.sampleData,
            localAlias: "",
            onSearch: {}
        )
    }
}
