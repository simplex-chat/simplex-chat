//
//  ChatInfoView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 05/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

func infoRow<S>(_ title: S, _ value: String) -> some View where S: StringProtocol {
    HStack {
        Text(title)
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

@ViewBuilder func smpServers(_ title: LocalizedStringKey, _ servers: [String]?) -> some View {
    if let servers = servers,
       servers.count > 0 {
        HStack {
            Text(title).frame(width: 120, alignment: .leading)
            Button(serverHost(servers[0])) {
                UIPasteboard.general.string = servers.joined(separator: ";")
            }
            .foregroundColor(.secondary)
            .lineLimit(1)
        }
    }
}

private func serverHost(_ s: String) -> String {
    if let i = s.range(of: "@")?.lowerBound {
        return String(s[i...].dropFirst())
    } else {
        return s
    }
}

struct ChatInfoView: View {
    @EnvironmentObject var chatModel: ChatModel
    @Environment(\.dismiss) var dismiss: DismissAction
    @ObservedObject var chat: Chat
    @State var contact: Contact
    @Binding var connectionStats: ConnectionStats?
    var customUserProfile: Profile?
    @State var localAlias: String
    @FocusState private var aliasTextFieldFocused: Bool
    @State private var alert: ChatInfoViewAlert? = nil
    @AppStorage(DEFAULT_DEVELOPER_TOOLS) private var developerTools = false

    enum ChatInfoViewAlert: Identifiable {
        case deleteContactAlert
        case clearChatAlert
        case networkStatusAlert
        case switchAddressAlert
        case error(title: LocalizedStringKey, error: LocalizedStringKey = "")

        var id: String {
            switch self {
            case .deleteContactAlert: return "deleteContactAlert"
            case .clearChatAlert: return "clearChatAlert"
            case .networkStatusAlert: return "networkStatusAlert"
            case .switchAddressAlert: return "switchAddressAlert"
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

                localAliasTextEdit()
                .listRowBackground(Color.clear)
                .listRowSeparator(.hidden)

                if let customUserProfile = customUserProfile {
                    Section("Incognito") {
                        infoRow("Your random profile", customUserProfile.chatViewName)
                    }
                }

                Section {
                    contactPreferencesButton()
                }

                Section("Servers") {
                    networkStatusRow()
                        .onTapGesture {
                            alert = .networkStatusAlert
                        }
                    Button("Change receiving address") {
                        alert = .switchAddressAlert
                    }
                    if let connStats = connectionStats {
                        smpServers("Receiving via", connStats.rcvServers)
                        smpServers("Sending via", connStats.sndServers)
                    }
                }

                Section {
                    clearChatButton()
                    deleteContactButton()
                }

                if developerTools {
                    Section(header: Text("For console")) {
                        infoRow("Local name", chat.chatInfo.localDisplayName)
                        infoRow("Database ID", "\(chat.chatInfo.apiId)")
                    }
                }
            }
            .navigationBarHidden(true)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
        .alert(item: $alert) { alertItem in
            switch(alertItem) {
            case .deleteContactAlert: return deleteContactAlert()
            case .clearChatAlert: return clearChatAlert()
            case .networkStatusAlert: return networkStatusAlert()
            case .switchAddressAlert: return switchAddressAlert(switchContactAddress)
            case let .error(title, error): return mkAlert(title: title, message: error)
            }
        }
    }

    func contactInfoHeader() -> some View {
        VStack {
            let cInfo = chat.chatInfo
            ChatInfoImage(chat: chat, color: Color(uiColor: .tertiarySystemFill))
                .frame(width: 192, height: 192)
                .padding(.top, 12)
                .padding()
            Text(contact.profile.displayName)
                .font(.largeTitle)
                .lineLimit(1)
                .padding(.bottom, 2)
            if cInfo.fullName != "" && cInfo.fullName != cInfo.displayName && cInfo.fullName != contact.profile.displayName {
                Text(cInfo.fullName)
                    .font(.title2)
                    .lineLimit(2)
            }
        }
        .frame(maxWidth: .infinity, alignment: .center)
    }

    func localAliasTextEdit() -> some View {
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
            .foregroundColor(.secondary)
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

    func contactPreferencesButton() -> some View {
        NavigationLink {
            ContactPreferencesView(
                contact: $contact,
                featuresAllowed: contactUserPrefsToFeaturesAllowed(contact.mergedPreferences),
                currentFeaturesAllowed: contactUserPrefsToFeaturesAllowed(contact.mergedPreferences)
            )
            .navigationBarTitle("Contact preferences")
            .navigationBarTitleDisplayMode(.large)
        } label: {
            Label("Contact preferences", systemImage: "switch.2")
        }
    }

    func networkStatusRow() -> some View {
        HStack {
            Text("Network status")
            Image(systemName: "info.circle")
                .foregroundColor(.accentColor)
                .font(.system(size: 14))
            Spacer()
            Text(chat.serverInfo.networkStatus.statusString)
                .foregroundColor(.secondary)
            serverImage()
        }
    }

    func serverImage() -> some View {
        let status = chat.serverInfo.networkStatus
        return Image(systemName: status.imageName)
            .foregroundColor(status == .connected ? .green : .secondary)
            .font(.system(size: 12))
    }

    func deleteContactButton() -> some View {
        Button(role: .destructive) {
            alert = .deleteContactAlert
        } label: {
            Label("Delete contact", systemImage: "trash")
                .foregroundColor(Color.red)
        }
    }

    func clearChatButton() -> some View {
        Button() {
            alert = .clearChatAlert
        } label: {
            Label("Clear conversation", systemImage: "gobackward")
                .foregroundColor(Color.orange)
        }
    }

    private func deleteContactAlert() -> Alert {
        Alert(
            title: Text("Delete contact?"),
            message: Text("Contact and all messages will be deleted - this cannot be undone!"),
            primaryButton: .destructive(Text("Delete")) {
                Task {
                    do {
                        try await apiDeleteChat(type: chat.chatInfo.chatType, id: chat.chatInfo.apiId)
                        await MainActor.run {
                            chatModel.removeChat(chat.chatInfo.id)
                            chatModel.chatId = nil
                            dismiss()
                        }
                    } catch let error {
                        logger.error("deleteContactAlert apiDeleteChat error: \(responseError(error))")
                        let a = getErrorAlert(error, "Error deleting contact")
                        await MainActor.run {
                            alert = .error(title: a.title, error: a.message)
                        }
                    }
                }
            },
            secondaryButton: .cancel()
        )
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
            message: Text(chat.serverInfo.networkStatus.statusExplanation)
        )
    }

    private func switchContactAddress() {
        Task {
            do {
                try await apiSwitchContact(contactId: contact.apiId)
            } catch let error {
                logger.error("switchContactAddress apiSwitchContact error: \(responseError(error))")
                let a = getErrorAlert(error, "Error changing address")
                await MainActor.run {
                    alert = .error(title: a.title, error: a.message)
                }
            }
        }
    }
}

func switchAddressAlert(_ switchAddress: @escaping () -> Void) -> Alert {
    Alert(
        title: Text("Change receiving address?"),
        message: Text("This feature is experimental! It will only work if the other client has version 4.2 installed. You should see the message in the conversation once the address change is completed – please check that you can still receive messages from this contact (or group member)."),
        primaryButton: .destructive(Text("Change"), action: switchAddress),
        secondaryButton: .cancel()
    )
}

struct ChatInfoView_Previews: PreviewProvider {
    static var previews: some View {
        ChatInfoView(
            chat: Chat(chatInfo: ChatInfo.sampleData.direct, chatItems: []),
            contact: Contact.sampleData,
            connectionStats: Binding.constant(nil),
            localAlias: ""
        )
    }
}
