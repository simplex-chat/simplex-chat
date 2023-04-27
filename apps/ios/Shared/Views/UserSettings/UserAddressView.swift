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
    @State private var aas = AutoAcceptState()
    @State private var savedAAS = AutoAcceptState()
    @State var shareViaProfile = false
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
        List {
            if let userAdress = chatModel.userAddress {
                aasView()
                    .onAppear {
                        aas = AutoAcceptState(userAdress: userAdress)
                        savedAAS = aas
                    }
                    .onChange(of: aas.enable) { _ in
                        if !aas.enable { aas = AutoAcceptState() }
                    }
                
                Section {
                    Toggle("Share with contacts", isOn: $shareViaProfile)
                        .onChange(of: shareViaProfile) { on in
                            if ignoreShareViaProfileChange {
                                ignoreShareViaProfileChange = false
                            } else {
                                alert = .profileAddress(on: on)
                            }
                        }
                } header: {
                    Text("Add to profile")
                } footer: {
                    Text("Add address to your profile, so that your contacts can share it with other people.")
                }
                
                Section {
                    QRCode(uri: userAdress.connReqContact)
                    Button {
                        showShareSheet(items: [userAdress.connReqContact])
                    } label: {
                        Label("Share address", systemImage: "square.and.arrow.up")
                    }
                    NavigationLink {
                        UserAddressLearnMore()
                            .navigationTitle("About SimpleX address")
                            .navigationBarTitleDisplayMode(.inline)
                    } label: {
                        Label("Learn more", systemImage: "info.circle")
                    }
                } header: {
                    Text("Address")
                } footer: {
                    Text("You can share this address to let people connect with you.")
                }
                
                Section {
                    deleteAddressButton()
                } footer: {
                    Text("Your contacts will remain connected.")
                }
            } else {
                Section {
                    createAddressButton()
                } footer: {
                    Text("Create an address to let people connect with you.")
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
            Label("Create address", systemImage: "qrcode")
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

        init(userAdress: UserContactLink) {
            if let aa = userAdress.autoAccept {
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

    @ViewBuilder private func aasView() -> some View {
        Section {
            settingsRow("checkmark") {
                Toggle("Automatically", isOn: $aas.enable)
            }
            if aas.enable {
                settingsRow(
                    aas.incognito ? "theatermasks.fill" : "theatermasks",
                    color: aas.incognito ? .indigo : .secondary
                ) {
                    Toggle("Incognito", isOn: $aas.incognito)
                }
            }
        } header: {
            Text("Accept requests")
        } footer: {
            saveAASButtons()
        }
        if aas.enable {
            Section {
                TextEditor(text: $aas.welcomeText)
                    .focused($keyboardVisible)
                    .padding(.horizontal, -5)
                    .padding(.top, -8)
                    .frame(height: 90, alignment: .topLeading)
                    .frame(maxWidth: .infinity, alignment: .leading)
            } header: {
                Text("Welcome message")
            }
        }
    }

    @ViewBuilder private func saveAASButtons() -> some View {
        HStack {
            Button {
                aas = savedAAS
            } label: {
                Label("Cancel", systemImage: "arrow.counterclockwise")
            }
            Spacer()
            Button {
                saveAAS()
            } label: {
                Label("Save", systemImage: "checkmark")
            }
        }
        .font(.body)
        .disabled(aas == savedAAS)
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
