//
//  UserAddressView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 26.04.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import MessageUI
@preconcurrency import SimpleXChat

struct UserAddressView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject private var chatModel: ChatModel
    @EnvironmentObject var theme: AppTheme
    @State var shareViaProfile = false
    @State var autoCreate = false
    @State private var showShortLink = true
    @State private var settings = AddressSettingsState()
    @State private var savedSettings = AddressSettingsState()
    @State private var showMailView = false
    @State private var mailViewResult: Result<MFMailComposeResult, Error>? = nil
    @State private var alert: UserAddressAlert?
    @State private var progressIndicator = false

    private enum UserAddressAlert: Identifiable {
        case deleteAddress
        case shareOnCreate
        case error(title: LocalizedStringKey, error: LocalizedStringKey?)

        var id: String {
            switch self {
            case .deleteAddress: return "deleteAddress"
            case .shareOnCreate: return "shareOnCreate"
            case let .error(title, _): return "error \(title)"
            }
        }
    }

    var body: some View {
        ZStack {
            userAddressView()

            if progressIndicator {
                ZStack {
                    if chatModel.userAddress != nil {
                        Circle()
                            .fill(.white)
                            .opacity(0.7)
                            .frame(width: 56, height: 56)
                    }
                    ProgressView().scaleEffect(2)
                }
            }
        }
        .onAppear {
            if chatModel.userAddress == nil, autoCreate {
                createAddress()
            }
        }
    }

    private func userAddressView() -> some View {
        List {
            if let userAddress = chatModel.userAddress {
                existingAddressView(userAddress)
                    .onAppear {
                        settings = AddressSettingsState(settings: userAddress.addressSettings)
                        savedSettings = AddressSettingsState(settings: userAddress.addressSettings)
                    }
            } else {
                Section {
                    createAddressButton()
                } header: {
                    Text("For social media")
                        .foregroundColor(theme.colors.secondary)
                }

                Section {
                    createOneTimeLinkButton()
                } header: {
                    Text("Or to share privately")
                        .foregroundColor(theme.colors.secondary)
                }

                Section {
                    learnMoreButton()
                }
            }
        }
        .alert(item: $alert) { alert in
            switch alert {
            case .deleteAddress:
                return Alert(
                    title: Text("Delete address?"),
                    message:
                        shareViaProfile
                    ? Text("All your contacts will remain connected. Profile update will be sent to your contacts.")
                    : Text("All your contacts will remain connected."),
                    primaryButton: .destructive(Text("Delete")) {
                        progressIndicator = true
                        Task {
                            do {
                                if let u = try await apiDeleteUserAddress() {
                                    DispatchQueue.main.async {
                                        chatModel.userAddress = nil
                                        chatModel.updateUser(u)
                                        if shareViaProfile {
                                            shareViaProfile = false
                                        }
                                    }
                                }
                                await MainActor.run { progressIndicator = false }
                            } catch let error {
                                logger.error("UserAddressView apiDeleteUserAddress: \(responseError(error))")
                                await MainActor.run { progressIndicator = false }
                            }
                        }
                    }, secondaryButton: .cancel()
                )
            case .shareOnCreate:
                return Alert(
                    title: Text("Share address with contacts?"),
                    message: Text("Add address to your profile, so that your contacts can share it with other people. Profile update will be sent to your contacts."),
                    primaryButton: .default(Text("Share")) {
                        setProfileAddress($progressIndicator, true)
                        shareViaProfile = true
                    }, secondaryButton: .cancel()
                )
            case let .error(title, error):
                return mkAlert(title: title, message: error)
            }
        }
    }

    @ViewBuilder private func existingAddressView(_ userAddress: UserContactLink) -> some View {
        Section {
            SimpleXCreatedLinkQRCode(link: userAddress.connLinkContact, short: $showShortLink)
                .id("simplex-contact-address-qrcode-\(userAddress.connLinkContact.simplexChatUri(short: showShortLink))")
            shareQRCodeButton(userAddress)
            // if MFMailComposeViewController.canSendMail() {
            //     shareViaEmailButton(userAddress)
            // }
            settingsRow("briefcase", color: theme.colors.secondary) {
                Toggle("Business address", isOn: $settings.businessAddress)
                    .onChange(of: settings.businessAddress) { ba in
                        if ba {
                            settings.autoAccept = true
                            settings.autoAcceptIncognito = false
                        }
                        saveAddressSettings(settings, $savedSettings)
                    }
            }
            addressSettingsButton(userAddress)
            if userAddress.connLinkContact.connShortLink == nil {
                addShortLinkButton()
            } else if !userAddress.shortLinkDataSet {
                addProfileToShortLinkButton()
            }
        } header: {
            ToggleShortLinkHeader(text: Text("For social media"), link: userAddress.connLinkContact, short: $showShortLink)
        } footer: {
            if settings.businessAddress {
                Text("Add your team members to the conversations.")
                    .foregroundColor(theme.colors.secondary)
            }
        }

        Section {
            createOneTimeLinkButton()
        } header: {
            Text("Or to share privately")
                .foregroundColor(theme.colors.secondary)
        }

        Section {
            learnMoreButton()
        }

        Section {
            deleteAddressButton()
        } footer: {
            Text("Your contacts will remain connected.")
                .foregroundColor(theme.colors.secondary)
        }
    }

    private func createAddressButton() -> some View {
        Button {
            createAddress()
        } label: {
            Label("Create SimpleX address", systemImage: "qrcode")
        }
    }

    private func createAddress() {
        progressIndicator = true
        Task {
            do {
                if let connLinkContact = try await apiCreateUserAddress() {
                    DispatchQueue.main.async {
                        chatModel.userAddress = UserContactLink(connLinkContact: connLinkContact, shortLinkDataSet: connLinkContact.connShortLink != nil, addressSettings: AddressSettings(businessAddress: false))
                        alert = .shareOnCreate
                        progressIndicator = false
                    }
                }
            } catch let error {
                logger.error("UserAddressView apiCreateUserAddress: \(responseError(error))")
                let a = getErrorAlert(error, "Error creating address")
                alert = .error(title: a.title, error: a.message)
                await MainActor.run { progressIndicator = false }
            }
        }
    }

    private func addShortLinkButton() -> some View {
        Button {
            showAddShortLinkAlert()
        } label: {
            settingsRow("plus", color: theme.colors.primary) {
                Text("Add short link")
            }
        }
    }

    private func addProfileToShortLinkButton() -> some View {
        Button {
            showAddShortLinkAlert()
        } label: {
            settingsRow("plus", color: theme.colors.primary) {
                Text("Share profile with address")
            }
        }
    }

    private func showAddShortLinkAlert() {
        showAlert(
            title: NSLocalizedString("Share profile with address", comment: "alert title"),
            message: NSLocalizedString("Profile will be shared with the address.", comment: "alert message"),
            buttonTitle: NSLocalizedString("Share profile", comment: "alert button"),
            buttonAction: { addShortLink() },
            cancelButton: true
        )
    }

    private func addShortLink() {
        progressIndicator = true
        Task {
            do {
                let userAddress = try await apiAddMyAddressShortLink()
                await MainActor.run {
                    chatModel.userAddress = userAddress
                }
                await MainActor.run { progressIndicator = false }
            } catch let error {
                logger.error("apiAddMyAddressShortLink: \(responseError(error))")
                let a = getErrorAlert(error, "Error adding short link")
                alert = .error(title: a.title, error: a.message)
                await MainActor.run { progressIndicator = false }
            }
        }
    }

    private func createOneTimeLinkButton() -> some View {
        NavigationLink {
            NewChatView(selection: .invite)
                .navigationTitle("New chat")
                .navigationBarTitleDisplayMode(.large)
                .modifier(ThemedBackground(grouped: true))
        } label: {
            Label("Create 1-time link", systemImage: "link.badge.plus")
                .foregroundColor(theme.colors.primary)
        }
    }

    private func deleteAddressButton() -> some View {
        Button(role: .destructive) {
            alert = .deleteAddress
        } label: {
            Label("Delete address", systemImage: "trash")
                .foregroundColor(Color.red)
        }
    }

    private func shareQRCodeButton(_ userAddress: UserContactLink) -> some View {
        Button {
            showShareSheet(items: [simplexChatLink(userAddress.connLinkContact.simplexChatUri(short: showShortLink))])
        } label: {
            settingsRow("square.and.arrow.up", color: theme.colors.secondary) {
                Text("Share address")
            }
        }
    }

    private func shareViaEmailButton(_ userAddress: UserContactLink) -> some View {
        Button {
            showMailView = true
        } label: {
            settingsRow("envelope", color: theme.colors.secondary) {
                Text("Invite friends")
            }
        }
        .sheet(isPresented: $showMailView) {
            SendAddressMailView(
                showMailView: $showMailView,
                mailViewResult: $mailViewResult,
                userAddress: userAddress
            )
            .edgesIgnoringSafeArea(.bottom)
        }
        .onChange(of: mailViewResult == nil) { _ in
            if let r = mailViewResult {
                switch r {
                case .success: ()
                case let .failure(error):
                    logger.error("UserAddressView share via email: \(responseError(error))")
                    let a = getErrorAlert(error, "Error sending email")
                    alert = .error(title: a.title, error: a.message)
                }
                mailViewResult = nil
            }
        }
    }

    private func addressSettingsButton(_ userAddress: UserContactLink) -> some View {
        NavigationLink {
            UserAddressSettingsView(shareViaProfile: $shareViaProfile)
                .navigationTitle("Address settings")
                .navigationBarTitleDisplayMode(.large)
                .modifier(ThemedBackground(grouped: true))
        } label: {
            Text("Address settings")
        }
    }

    private func learnMoreButton() -> some View {
        NavigationLink {
            UserAddressLearnMore()
                .navigationTitle("Address or 1-time link?")
                .modifier(ThemedBackground(grouped: true))
                .navigationBarTitleDisplayMode(.inline)
        } label: {
            settingsRow("info.circle", color: theme.colors.secondary) {
                Text("SimpleX address or 1-time link?")
            }
        }
    }
}

struct ToggleShortLinkHeader: View {
    @EnvironmentObject var theme: AppTheme
    let text: Text
    var link: CreatedConnLink
    @Binding var short: Bool

    var body: some View {
        if link.connShortLink == nil {
            text.foregroundColor(theme.colors.secondary)
        } else {
            HStack {
                text.foregroundColor(theme.colors.secondary)
                Spacer()
                Text(short ? "Full link" : "Short link")
                    .textCase(.none)
                    .foregroundColor(theme.colors.primary)
                    .onTapGesture { short.toggle() }
            }
        }
    }
}

struct AddressSettingsState: Equatable {
    var businessAddress = false
    var autoAccept = false
    var autoAcceptIncognito = false
    var autoReply = ""

    init() {}

    init(settings: AddressSettings) {
        self.businessAddress = settings.businessAddress
        self.autoAccept = settings.autoAccept != nil
        self.autoAcceptIncognito = settings.autoAccept?.acceptIncognito == true
        self.autoReply = settings.autoReply?.text ?? ""
    }

    var addressSettings: AddressSettings {
        AddressSettings(
            businessAddress: self.businessAddress,
            autoAccept: self.autoAccept ? AutoAccept(acceptIncognito: self.autoAcceptIncognito) : nil,
            autoReply: self.autoReply.isEmpty ? nil : MsgContent.text(self.autoReply)
        )
    }
}

private func setProfileAddress(_ progressIndicator: Binding<Bool>, _ on: Bool) {
    progressIndicator.wrappedValue = true
    Task {
        do {
            if let u = try await apiSetProfileAddress(on: on) {
                DispatchQueue.main.async {
                    ChatModel.shared.updateUser(u)
                }
            }
            await MainActor.run { progressIndicator.wrappedValue = false }
        } catch let error {
            logger.error("apiSetProfileAddress: \(responseError(error))")
            await MainActor.run { progressIndicator.wrappedValue = false }
        }
    }
}

struct UserAddressSettingsView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var theme: AppTheme
    @Binding var shareViaProfile: Bool
    @State private var settings = AddressSettingsState()
    @State private var savedSettings = AddressSettingsState()
    @State private var ignoreShareViaProfileChange = false
    @State private var progressIndicator = false

    var body: some View {
        ZStack {
            if let userAddress = ChatModel.shared.userAddress {
                userAddressSettingsView()
                    .onAppear {
                        settings = AddressSettingsState(settings: userAddress.addressSettings)
                        savedSettings = AddressSettingsState(settings: userAddress.addressSettings)
                    }
                    .onChange(of: settings.autoAccept) { autoAccept in
                        if !autoAccept {
                            settings.businessAddress = false
                            settings.autoReply = ""
                        }
                    }
                    .onDisappear {
                        if savedSettings != settings {
                            showAlert(
                                title: NSLocalizedString("SimpleX address settings", comment: "alert title"),
                                message: NSLocalizedString("Settings were changed.", comment: "alert message"),
                                buttonTitle: NSLocalizedString("Save", comment: "alert button"),
                                buttonAction: { saveAddressSettings(settings, $savedSettings) },
                                cancelButton: true
                            )
                        }
                    }
            } else {
                Text(String("Error opening address settings"))
            }
            if progressIndicator {
                ProgressView().scaleEffect(2)
            }
        }
    }

    private func userAddressSettingsView() -> some View {
        List {
            Section {
                shareWithContactsButton()
                autoAcceptToggle().disabled(settings.businessAddress)
            }

            // TODO v6.4.1 move auto-reply editor here
            // messageEditor(placeholder: NSLocalizedString("Enter welcome message… (optional)", comment: "placeholder"), text: $settings.autoReply)

            if settings.autoAccept {
                autoAcceptSection()
            }

            Section {
                saveAddressSettingsButton()
                    .disabled(settings == savedSettings)
            }
        }
    }

    private func shareWithContactsButton() -> some View {
        settingsRow("person", color: theme.colors.secondary) {
            Toggle("Share with contacts", isOn: $shareViaProfile)
                .onChange(of: shareViaProfile) { on in
                    if ignoreShareViaProfileChange {
                        ignoreShareViaProfileChange = false
                    } else {
                        if on {
                            showAlert(
                                NSLocalizedString("Share address with contacts?", comment: "alert title"),
                                message: NSLocalizedString("Profile update will be sent to your contacts.", comment: "alert message"),
                                actions: {[
                                    UIAlertAction(
                                        title: NSLocalizedString("Cancel", comment: "alert action"),
                                        style: .cancel,
                                        handler: { _ in
                                            ignoreShareViaProfileChange = true
                                            shareViaProfile = !on
                                        }
                                    ),
                                    UIAlertAction(
                                        title: NSLocalizedString("Share", comment: "alert action"),
                                        style: .default,
                                        handler: { _ in
                                            setProfileAddress($progressIndicator, on)
                                        }
                                    )
                                ]}
                            )
                        } else {
                            showAlert(
                                NSLocalizedString("Stop sharing address?", comment: "alert title"),
                                message: NSLocalizedString("Profile update will be sent to your contacts.", comment: "alert message"),
                                actions: {[
                                    UIAlertAction(
                                        title: NSLocalizedString("Cancel", comment: "alert action"),
                                        style: .cancel,
                                        handler: { _ in
                                            ignoreShareViaProfileChange = true
                                            shareViaProfile = !on
                                        }
                                    ),
                                    UIAlertAction(
                                        title: NSLocalizedString("Stop sharing", comment: "alert action"),
                                        style: .default,
                                        handler: { _ in
                                            setProfileAddress($progressIndicator, on)
                                        }
                                    )
                                ]}
                            )
                        }
                    }
                }
        }
    }

    private func autoAcceptToggle() -> some View {
        settingsRow("checkmark", color: theme.colors.secondary) {
            Toggle("Auto-accept", isOn: $settings.autoAccept)
                .onChange(of: settings.autoAccept) { _ in
                    saveAddressSettings(settings, $savedSettings)
                }
        }
    }

    private func autoAcceptSection() -> some View {
        Section {
            if !ChatModel.shared.addressShortLinkDataSet && !settings.businessAddress {
                acceptIncognitoToggle()
            }
            // TODO v6.4.1 show this message editor even with auto-accept disabled
            messageEditor(placeholder: NSLocalizedString("Enter welcome message… (optional)", comment: "placeholder"), text: $settings.autoReply)
        } header: {
            Text("Auto-accept")
                .foregroundColor(theme.colors.secondary)
        } footer: {
            Text("Sent to your contact after connection.")
        }
    }

    private func acceptIncognitoToggle() -> some View {
        settingsRow(
            settings.autoAcceptIncognito ? "theatermasks.fill" : "theatermasks",
            color: settings.autoAcceptIncognito ? .indigo : theme.colors.secondary
        ) {
            Toggle("Accept incognito", isOn: $settings.autoAcceptIncognito)
        }
    }

    private func messageEditor(placeholder: String, text: Binding<String>) -> some View {
        ZStack {
            Group {
                if text.wrappedValue.isEmpty {
                    TextEditor(text: Binding.constant(placeholder))
                        .foregroundColor(theme.colors.secondary)
                        .disabled(true)
                }
                TextEditor(text: text)
            }
            .padding(.horizontal, -5)
            .padding(.top, -8)
            .frame(height: 90, alignment: .topLeading)
            .frame(maxWidth: .infinity, alignment: .leading)
        }
    }

    private func saveAddressSettingsButton() -> some View {
        Button {
            hideKeyboard()
            saveAddressSettings(settings, $savedSettings)
        } label: {
            Text("Save")
        }
    }
}

private func saveAddressSettings(_ settings: AddressSettingsState, _ savedSettings: Binding<AddressSettingsState>) {
    Task {
        do {
            if let address = try await apiSetUserAddressSettings(settings.addressSettings) {
                await MainActor.run {
                    ChatModel.shared.userAddress = address
                    savedSettings.wrappedValue = settings
                }
            }
        } catch let error {
            logger.error("apiSetUserAddressSettings error: \(responseError(error))")
        }
    }
}

struct UserAddressView_Previews: PreviewProvider {
    static var previews: some View {
        let chatModel = ChatModel()
        chatModel.userAddress = UserContactLink(
            connLinkContact: CreatedConnLink(connFullLink: "https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D", connShortLink: nil),
            shortLinkDataSet: false,
            addressSettings: AddressSettings(businessAddress: false)
        )


        return Group {
            UserAddressView()
                .environmentObject(chatModel)
            UserAddressView()
                .environmentObject(ChatModel())
        }
    }
}
