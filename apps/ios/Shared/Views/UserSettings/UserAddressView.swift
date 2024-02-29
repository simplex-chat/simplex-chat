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
    @State var viaCreateLinkView = false
    @State var shareViaProfile = false
    @State private var aas = AutoAcceptState()
    @State private var savedAAS = AutoAcceptState()
    @State private var ignoreShareViaProfileChange = false
    @State private var showMailView = false
    @State private var mailViewResult: Result<MFMailComposeResult, Error>? = nil
    @State private var alert: UserAddressAlert?
    @State private var showSaveDialogue = false
    @State private var progressIndicator = false
    @FocusState private var keyboardVisible: Bool

    private enum UserAddressAlert: Identifiable {
        case deleteAddress
        case profileAddress(on: Bool)
        case shareOnCreate
        case error(title: LocalizedStringKey, error: LocalizedStringKey = "")

        var id: String {
            switch self {
            case .deleteAddress: return "deleteAddress"
            case let .profileAddress(on): return "profileAddress \(on)"
            case .shareOnCreate: return "shareOnCreate"
            case let .error(title, _): return "error \(title)"
            }
        }
    }
    
    var body: some View {
        ZStack {
            if viaCreateLinkView {
                userAddressScrollView()
            } else {
                userAddressScrollView()
                    .modifier(BackButton(disabled: Binding.constant(false)) {
                        if savedAAS == aas {
                            dismiss()
                        } else {
                            keyboardVisible = false
                            showSaveDialogue = true
                        }
                    })
                    .confirmationDialog("Save settings?", isPresented: $showSaveDialogue) {
                        Button("Save auto-accept settings") {
                            saveAAS()
                            dismiss()
                        }
                        Button("Exit without saving") { dismiss() }
                    }
            }
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
    }

    @Namespace private var bottomID

    private func userAddressScrollView() -> some View {
        ScrollViewReader { proxy in
            userAddressView()
                .onChange(of: keyboardVisible) { _ in
                    if keyboardVisible {
                        DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                            withAnimation {
                                proxy.scrollTo(bottomID, anchor: .top)
                            }
                        }
                    }
                }
        }
    }

    private func userAddressView() -> some View {
        List {
            if let userAddress = chatModel.userAddress {
                existingAddressView(userAddress)
                    .onAppear {
                        aas = AutoAcceptState(userAddress: userAddress)
                        savedAAS = aas
                    }
                    .onChange(of: aas.enable) { _ in
                        if !aas.enable { aas = AutoAcceptState() }
                    }
            } else {
                Section {
                    createAddressButton()
                } footer: {
                    Text("Create an address to let people connect with you.")
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
                                            ignoreShareViaProfileChange = true
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
            case let .profileAddress(on):
                if on {
                    return Alert(
                        title: Text("Share address with contacts?"),
                        message: Text("Profile update will be sent to your contacts."),
                        primaryButton: .default(Text("Share")) {
                            setProfileAddress(on)
                        }, secondaryButton: .cancel() {
                            ignoreShareViaProfileChange = true
                            shareViaProfile = !on
                        }
                    )
                } else {
                    return Alert(
                        title: Text("Stop sharing address?"),
                        message: Text("Profile update will be sent to your contacts."),
                        primaryButton: .default(Text("Stop sharing")) {
                            setProfileAddress(on)
                        }, secondaryButton: .cancel() {
                            ignoreShareViaProfileChange = true
                            shareViaProfile = !on
                        }
                    )
                }
            case .shareOnCreate:
                return Alert(
                    title: Text("Share address with contacts?"),
                    message: Text("Add address to your profile, so that your contacts can share it with other people. Profile update will be sent to your contacts."),
                    primaryButton: .default(Text("Share")) {
                        setProfileAddress(true)
                        ignoreShareViaProfileChange = true
                        shareViaProfile = true
                    }, secondaryButton: .cancel()
                )
            case let .error(title, error):
                return Alert(title: Text(title), message: Text(error))
            }
        }
    }

    @ViewBuilder private func existingAddressView(_ userAddress: UserContactLink) -> some View {
        Section {
            SimpleXLinkQRCode(uri: userAddress.connReqContact)
                .id("simplex-contact-address-qrcode-\(userAddress.connReqContact)")
            shareQRCodeButton(userAddress)
            if MFMailComposeViewController.canSendMail() {
                shareViaEmailButton(userAddress)
            }
            shareWithContactsButton()
            autoAcceptToggle()
            learnMoreButton()
        } header: {
            Text("Address")
        }

        if aas.enable {
            autoAcceptSection()
        }

        Section {
            deleteAddressButton()
        } footer: {
            Text("Your contacts will remain connected.")
        }
        .id(bottomID)
    }

    private func createAddressButton() -> some View {
        Button {
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
        } label: {
            Label("Create SimpleX address", systemImage: "qrcode")
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
            settingsRow("square.and.arrow.up") {
                Text("Share address")
            }
        }
    }

    private func shareViaEmailButton(_ userAddress: UserContactLink) -> some View {
        Button {
            showMailView = true
        } label: {
            settingsRow("envelope") {
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

    private func autoAcceptToggle() -> some View {
        settingsRow("checkmark") {
            Toggle("Auto-accept", isOn: $aas.enable)
                .onChange(of: aas.enable) { _ in
                    saveAAS()
                }
        }
    }

    private func learnMoreButton() -> some View {
        NavigationLink {
            UserAddressLearnMore()
                .navigationTitle("SimpleX address")
                .navigationBarTitleDisplayMode(.large)
        } label: {
            settingsRow("info.circle") {
                Text("About SimpleX address")
            }
        }
    }

    private func shareWithContactsButton() -> some View {
        settingsRow("person") {
            Toggle("Share with contacts", isOn: $shareViaProfile)
                .onChange(of: shareViaProfile) { on in
                    if ignoreShareViaProfileChange {
                        ignoreShareViaProfileChange = false
                    } else {
                        alert = .profileAddress(on: on)
                    }
                }
        }
    }

    private func setProfileAddress(_ on: Bool) {
        progressIndicator = true
        Task {
            do {
                if let u = try await apiSetProfileAddress(on: on) {
                    DispatchQueue.main.async {
                        chatModel.updateUser(u)
                    }
                }
                await MainActor.run { progressIndicator = false }
            } catch let error {
                logger.error("UserAddressView apiSetProfileAddress: \(responseError(error))")
                await MainActor.run { progressIndicator = false }
            }
        }
    }

    private struct AutoAcceptState: Equatable {
        var enable = false
        var incognito = false
        var welcomeText = ""

        init(enable: Bool = false, incognito: Bool = false, welcomeText: String = "") {
            self.enable = enable
            self.incognito = incognito
            self.welcomeText = welcomeText
        }

        init(userAddress: UserContactLink) {
            if let aa = userAddress.autoAccept {
                enable = true
                incognito = aa.acceptIncognito
                if let msg = aa.autoReply {
                    welcomeText = msg.text
                } else {
                    welcomeText = ""
                }
            } else {
                enable = false
                incognito = false
                welcomeText = ""
            }
        }

        var autoAccept: AutoAccept? {
            if enable {
                var autoReply: MsgContent? = nil
                let s = welcomeText.trimmingCharacters(in: .whitespacesAndNewlines)
                if s != "" { autoReply = .text(s) }
                return AutoAccept(acceptIncognito: incognito, autoReply: autoReply)
            }
            return nil
        }
    }

    @ViewBuilder private func autoAcceptSection() -> some View {
        Section {
            acceptIncognitoToggle()
            welcomeMessageEditor()
            saveAASButton()
                .disabled(aas == savedAAS)
        } header: {
            Text("Auto-accept")
        }
    }

    private func acceptIncognitoToggle() -> some View {
        settingsRow(
            aas.incognito ? "theatermasks.fill" : "theatermasks",
            color: aas.incognito ? .indigo : .secondary
        ) {
            Toggle("Accept incognito", isOn: $aas.incognito)
        }
    }

    private func welcomeMessageEditor() -> some View {
        ZStack {
            Group {
                if aas.welcomeText.isEmpty {
                    TextEditor(text: Binding.constant(NSLocalizedString("Enter welcome message… (optional)", comment: "placeholder")))
                        .foregroundColor(.secondary)
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
                    chatModel.userAddress = address
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
