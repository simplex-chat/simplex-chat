//
//  UserAddressView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 26.04.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import MessageUI
import SimpleXChat

struct UserAddressView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject private var chatModel: ChatModel
    @EnvironmentObject var theme: AppTheme
    @State var shareViaProfile = false
    @State var autoCreate = false
    @State private var aas = AutoAcceptState()
    @State private var savedAAS = AutoAcceptState()
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
            if let userAddress = chatModel.userAddress {
                aas = AutoAcceptState(userAddress: userAddress)
                savedAAS = aas
            }
        }
        .onChange(of: aas.enable) { aasEnabled in
            if !aasEnabled { aas = AutoAcceptState() }
        }

    }

    private func userAddressView() -> some View {
        List {
            if let userAddress = chatModel.userAddress {
                existingAddressView(userAddress)
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
            SimpleXLinkQRCode(uri: userAddress.connReqContact)
                .id("simplex-contact-address-qrcode-\(userAddress.connReqContact)")
            shareQRCodeButton(userAddress)
            // if MFMailComposeViewController.canSendMail() {
            //     shareViaEmailButton(userAddress)
            // }
            settingsRow("hand.wave", color: theme.colors.secondary) {
                Toggle("Business address", isOn: $aas.business)
                    .onChange(of: aas.business) { ba in
                        if ba {
                            aas.enable = true
                            aas.incognito = false
                        }
                    }
            }
            addressSettingsButton(userAddress)
        } header: {
            Text("For social media")
                .foregroundColor(theme.colors.secondary)
        } footer: {
            if aas.business {
                Text("Add your team members to the conversations").foregroundColor(theme.colors.secondary)
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
                let connReqContact = try await apiCreateUserAddress()
                DispatchQueue.main.async {
                    chatModel.userAddress = UserContactLink(connReqContact: connReqContact)
                    alert = .shareOnCreate
                    progressIndicator = false
                }
            } catch let error {
                logger.error("UserAddressView apiCreateUserAddress: \(responseError(error))")
                let a = getErrorAlert(error, "Error creating address")
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
            showShareSheet(items: [simplexChatLink(userAddress.connReqContact)])
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

private struct AutoAcceptState: Equatable {
    var enable = false
    var incognito = false
    var business = false
    var welcomeText = ""

    init(enable: Bool = false, incognito: Bool = false, business: Bool = false, welcomeText: String = "") {
        self.enable = enable
        self.incognito = incognito
        self.business = business
        self.welcomeText = welcomeText
    }

    init(userAddress: UserContactLink) {
        if let aa = userAddress.autoAccept {
            enable = true
            incognito = aa.acceptIncognito
            business = aa.businessAddress == true
            if let msg = aa.autoReply {
                welcomeText = msg.text
            } else {
                welcomeText = ""
            }
        } else {
            enable = false
            incognito = false
            business = false
            welcomeText = ""
        }
    }

    var autoAccept: AutoAccept? {
        if enable {
            var autoReply: MsgContent? = nil
            let s = welcomeText.trimmingCharacters(in: .whitespacesAndNewlines)
            if s != "" { autoReply = .text(s) }
            return AutoAccept(businessAddress: business, acceptIncognito: incognito, autoReply: autoReply)
        }
        return nil
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
    @State private var aas = AutoAcceptState()
    @State private var savedAAS = AutoAcceptState()
    @State private var ignoreShareViaProfileChange = false
    @State private var progressIndicator = false
    @FocusState private var keyboardVisible: Bool

    var body: some View {
        ZStack {
            if let userAddress = ChatModel.shared.userAddress {
                userAddressSettingsView()
                    .onAppear {
                        aas = AutoAcceptState(userAddress: userAddress)
                        savedAAS = aas
                    }
                    .onChange(of: aas.enable) { aasEnabled in
                        if !aasEnabled { aas = AutoAcceptState() }
                    }
                    .onDisappear {
                        if savedAAS != aas {
                            showAlert(
                                title: NSLocalizedString("Auto-accept settings", comment: "alert title"),
                                message: NSLocalizedString("Settings were changed.", comment: "alert message"),
                                buttonTitle: NSLocalizedString("Save", comment: "alert button"),
                                buttonAction: saveAAS,
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
                autoAcceptToggle().disabled(aas.business)
            }

            if aas.enable {
                autoAcceptSection()
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
                                        style: .default,
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
                                        style: .default,
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
            Toggle("Auto-accept", isOn: $aas.enable)
                .onChange(of: aas.enable) { _ in
                    saveAAS()
                }
        }
    }

    private func autoAcceptSection() -> some View {
        Section {
            if !aas.business {
                acceptIncognitoToggle()
            }
            welcomeMessageEditor()
            saveAASButton()
                .disabled(aas == savedAAS)
        } header: {
            Text("Auto-accept")
                .foregroundColor(theme.colors.secondary)
        }
    }

    private func acceptIncognitoToggle() -> some View {
        settingsRow(
            aas.incognito ? "theatermasks.fill" : "theatermasks",
            color: aas.incognito ? .indigo : theme.colors.secondary
        ) {
            Toggle("Accept incognito", isOn: $aas.incognito)
        }
    }

    private func welcomeMessageEditor() -> some View {
        ZStack {
            Group {
                if aas.welcomeText.isEmpty {
                    TextEditor(text: Binding.constant(NSLocalizedString("Enter welcome message… (optional)", comment: "placeholder")))
                        .foregroundColor(theme.colors.secondary)
                        .disabled(true)
                }
                TextEditor(text: $aas.welcomeText)
                    .focused($keyboardVisible)
            }
            .padding(.horizontal, -5)
            .padding(.top, -8)
            .frame(height: 90, alignment: .topLeading)
            .frame(maxWidth: .infinity, alignment: .leading)
        }
    }

    private func saveAASButton() -> some View {
        Button {
            keyboardVisible = false
            saveAAS()
        } label: {
            Text("Save")
        }
    }

    private func saveAAS() {
        Task {
            do {
                if let address = try await userAddressAutoAccept(aas.autoAccept) {
                    ChatModel.shared.userAddress = address
                    savedAAS = aas
                }
            } catch let error {
                logger.error("userAddressAutoAccept error: \(responseError(error))")
            }
        }
    }
}

struct UserAddressView_Previews: PreviewProvider {
    static var previews: some View {
        let chatModel = ChatModel()
        chatModel.userAddress = UserContactLink(connReqContact: "https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D")

        
        return Group {
            UserAddressView()
                .environmentObject(chatModel)
            UserAddressView()
                .environmentObject(ChatModel())
        }
    }
}
