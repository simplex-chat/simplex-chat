//
//  UserAddressView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 26.04.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct UserAddressView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject private var chatModel: ChatModel
    @State var viaCreateLinkView = false
    @State var shareViaProfile = false
    @State private var aas = AutoAcceptState()
    @State private var savedAAS = AutoAcceptState()
    @State private var ignoreShareViaProfileChange = false
    @State private var alert: UserAddressAlert?
    @State private var showSaveDialogue = false
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
        if viaCreateLinkView {
            userAddressView()
        } else {
            userAddressView()
                .modifier(BackButton {
                    if savedAAS == aas {
                        dismiss()
                    } else {
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
    }

    @ViewBuilder private func userAddressView() -> some View {
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
                    message: Text("All your contacts will remain connected"),
                    primaryButton: .destructive(Text("Delete")) {
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
                            } catch let error {
                                logger.error("UserAddressView apiDeleteUserAddress: \(responseError(error))")
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
            QRCode(uri: userAddress.connReqContact)
            shareQRCodeButton(userAddress)
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
    }

    private func createAddressButton() -> some View {
        Button {
            Task {
                do {
                    let connReqContact = try await apiCreateUserAddress()
                    DispatchQueue.main.async {
                        chatModel.userAddress = UserContactLink(connReqContact: connReqContact)
                        alert = .shareOnCreate
                    }
                } catch let error {
                    logger.error("UserAddressView apiCreateUserAddress: \(responseError(error))")
                    let a = getErrorAlert(error, "Error creating address")
                    alert = .error(title: a.title, error: a.message)
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

    private func shareQRCodeButton(_ userAdress: UserContactLink) -> some View {
        Button {
            showShareSheet(items: [userAdress.connReqContact])
        } label: {
            Label("Share address", systemImage: "square.and.arrow.up")
        }
    }

    private func autoAcceptToggle() -> some View {
        settingsRow("checkmark") {
            Toggle("Auto-accept", isOn: $aas.enable)
        }
    }

    private func learnMoreButton() -> some View {
        NavigationLink {
            UserAddressLearnMore()
                .navigationTitle("SimpleX address")
                .navigationBarTitleDisplayMode(.large)
        } label: {
            Label("Learn more", systemImage: "info.circle")
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
        Task {
            do {
                if let u = try await apiSetProfileAddress(on: on) {
                    DispatchQueue.main.async {
                        chatModel.updateUser(u)
                    }
                }
            } catch let error {
                logger.error("UserAddressView apiSetProfileAddress: \(responseError(error))")
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
            Text("Accept requests")
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
            if aas.welcomeText.isEmpty {
                TextEditor(text: Binding.constant("Type welcome message… (optional)"))
                    .foregroundColor(.secondary)
                    .disabled(true)
                    .padding(.horizontal, -5)
                    .padding(.top, -8)
                    .frame(height: 90, alignment: .topLeading)
                    .frame(maxWidth: .infinity, alignment: .leading)
            }
            TextEditor(text: $aas.welcomeText)
                .focused($keyboardVisible)
                .padding(.horizontal, -5)
                .padding(.top, -8)
                .frame(height: 90, alignment: .topLeading)
                .frame(maxWidth: .infinity, alignment: .leading)
        }
    }

    private func saveAASButton() -> some View {
        Button {
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
