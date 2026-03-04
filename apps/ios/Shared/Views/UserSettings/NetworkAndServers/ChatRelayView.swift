//
//  ChatRelayView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 23.02.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//
// Spec: spec/architecture.md

import SwiftUI
import SimpleXChat

@ViewBuilder func showRelayTestStatus(relay: UserChatRelay) -> some View {
    switch relay.tested {
    case .some(true): Image(systemName: "checkmark").foregroundColor(.green)
    case .some(false): Image(systemName: "multiply").foregroundColor(.red)
    case .none: Color.clear
    }
}

func validRelayName(_ name: String) -> Bool {
    name != "" && validDisplayName(name)
}

func showInvalidRelayNameAlert(_ name: Binding<String>) {
    let validName = mkValidName(name.wrappedValue)
    if validName == "" {
        showAlert(NSLocalizedString("Invalid name!", comment: "alert title"))
    } else {
        showAlert(
            NSLocalizedString("Invalid name!", comment: "alert title"),
            message: String.localizedStringWithFormat(NSLocalizedString("Correct name to %@?", comment: "alert message"), validName),
            actions: {[
                UIAlertAction(title: NSLocalizedString("Ok", comment: "alert action"), style: .default) { _ in
                    name.wrappedValue = validName
                },
                cancelAlertAction
            ]}
        )
    }
}

func parseRelayAddress(_ address: String) -> (simplexUri: String, smpHosts: [String])? {
    if let parsedMd = parseSimpleXMarkdown(address),
       parsedMd.count == 1,
       case let .simplexLink(_, .relay, simplexUri, smpHosts) = parsedMd.first?.format {
        (simplexUri, smpHosts)
    } else {
        nil
    }
}

func validRelayAddress(_ address: String) -> Bool {
    parseRelayAddress(address) != nil
}

// TODO [relays] TBC matching relay to operator by domain (relay address can be hosted on operator server)
func addChatRelay(
    _ relay: UserChatRelay,
    _ userServers: Binding<[UserOperatorServers]>,
    _ serverErrors: Binding<[UserServersError]>,
    _ serverWarnings: Binding<[UserServersWarning]>? = nil,
    _ dismiss: DismissAction
) {
    let nameEmpty = relay.name.trimmingCharacters(in: .whitespaces).isEmpty
    let addressEmpty = relay.address.trimmingCharacters(in: .whitespaces).isEmpty
    if nameEmpty && addressEmpty {
        dismiss()
    } else if !validRelayName(relay.name) {
        dismiss()
        showAlert(
            NSLocalizedString("Invalid relay name!", comment: "alert title"),
            message: NSLocalizedString("Check relay name and try again.", comment: "alert message")
        )
    } else if !validRelayAddress(relay.address) {
        dismiss()
        showAlert(
            NSLocalizedString("Invalid relay address!", comment: "alert title"),
            message: NSLocalizedString("Check relay address and try again.", comment: "alert message")
        )
    } else if let i = userServers.wrappedValue.firstIndex(where: { $0.operator == nil }) {
        userServers[i].wrappedValue.chatRelays.append(relay)
        validateServers_(userServers, serverErrors, serverWarnings)
        dismiss()
    } else { // Shouldn't happen
        dismiss()
        showAlert(NSLocalizedString("Error adding relay", comment: "alert title"))
    }
}

struct ChatRelayView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var theme: AppTheme
    @Binding var userServers: [UserOperatorServers]
    @Binding var serverErrors: [UserServersError]
    @Binding var serverWarnings: [UserServersWarning]
    @Binding var relay: UserChatRelay
    @State var relayToEdit: UserChatRelay
    var backLabel: LocalizedStringKey

    var body: some View {
        let validName = validRelayName(relayToEdit.name)
        let validAddress = relay.preset || validRelayAddress(relayToEdit.address)
        ZStack {
            if relay.preset {
                presetRelay(validName: validName)
            } else {
                customRelay(validName: validName, validAddress: validAddress)
            }
        }
        .modifier(BackButton(label: backLabel, disabled: Binding.constant(false)) {
            if !validRelayName(relayToEdit.name) {
                dismiss()
                showAlert(
                    NSLocalizedString("Invalid relay name!", comment: "alert title"),
                    message: NSLocalizedString("Check relay name and try again.", comment: "alert message")
                )
            } else if validAddress {
                relay = relayToEdit
                validateServers_($userServers, $serverErrors, $serverWarnings)
                dismiss()
            } else {
                dismiss()
                showAlert(
                    NSLocalizedString("Invalid relay address!", comment: "alert title"),
                    message: NSLocalizedString("Check relay address and try again.", comment: "alert message")
                )
            }
        })
    }

    private func relayNameHeader(validName: Bool) -> some View {
        HStack {
            Text("Relay name").foregroundColor(theme.colors.secondary)
            if !validName {
                Spacer()
                Image(systemName: "exclamationmark.circle").foregroundColor(.red)
                    .onTapGesture { showInvalidRelayNameAlert($relayToEdit.name) }
            }
        }
    }

    private func presetRelay(validName: Bool) -> some View {
        List {
            Section {
                TextField("Enter relay name…", text: $relayToEdit.name)
                    .autocorrectionDisabled(true)
            } header: {
                relayNameHeader(validName: validName)
            }
            Section(header: Text("Preset relay address").foregroundColor(theme.colors.secondary)) {
                Text(relayToEdit.address)
                    .textSelection(.enabled)
            }
            useRelaySection()
        }
    }

    private func customRelay(validName: Bool, validAddress: Bool) -> some View {
        List {
            Section {
                TextField("Enter relay name…", text: $relayToEdit.name)
                    .autocorrectionDisabled(true)
            } header: {
                relayNameHeader(validName: validName)
            }
            Section {
                TextEditor(text: $relayToEdit.address)
                    .multilineTextAlignment(.leading)
                    .autocorrectionDisabled(true)
                    .autocapitalization(.none)
                    .allowsTightening(true)
                    .lineLimit(10)
                    .frame(height: 144)
                    .padding(-6)
            } header: {
                HStack {
                    Text("Your relay address")
                        .foregroundColor(theme.colors.secondary)
                    if !validAddress {
                        Spacer()
                        Image(systemName: "exclamationmark.circle").foregroundColor(.red)
                    }
                }
            }
            useRelaySection(valid: validAddress)
            Section {
                Button(role: .destructive) {
                    relay.deleted = true
                    validateServers_($userServers, $serverErrors, $serverWarnings)
                    dismiss()
                } label: {
                    Label("Delete relay", systemImage: "trash")
                        .foregroundColor(.red)
                }
            }
        }
    }

    private func useRelaySection(valid: Bool = true) -> some View {
        Section(header: Text("Use relay").foregroundColor(theme.colors.secondary)) {
            HStack {
                Button("Test relay") {
                    showAlert(
                        NSLocalizedString("Not implemented", comment: "alert title"),
                        message: NSLocalizedString("Relay testing is not yet available.", comment: "alert message")
                    )
                }
                .disabled(!valid)
                Spacer()
                showRelayTestStatus(relay: relayToEdit)
            }
            Toggle("Use for new channels", isOn: $relayToEdit.enabled)
        }
    }
}

struct ChatRelayViewLink: View {
    @EnvironmentObject var theme: AppTheme
    @Binding var userServers: [UserOperatorServers]
    @Binding var serverErrors: [UserServersError]
    @Binding var serverWarnings: [UserServersWarning]
    @Binding var relay: UserChatRelay
    var backLabel: LocalizedStringKey
    @Binding var selectedServer: String?

    var body: some View {
        NavigationLink(tag: relay.id, selection: $selectedServer) {
            ChatRelayView(
                userServers: $userServers,
                serverErrors: $serverErrors,
                serverWarnings: $serverWarnings,
                relay: $relay,
                relayToEdit: relay,
                backLabel: backLabel
            )
            .navigationBarTitle("Chat relay")
            .modifier(ThemedBackground(grouped: true))
            .navigationBarTitleDisplayMode(.large)
        } label: {
            HStack {
                Group {
                    if !relay.enabled {
                        Image(systemName: "slash.circle").foregroundColor(theme.colors.secondary)
                    } else {
                        showRelayTestStatus(relay: relay)
                    }
                }
                .frame(width: 16, alignment: .center)
                .padding(.trailing, 4)

                let displayName = !relay.name.isEmpty ? relay.name : relay.domains.first ?? relay.address
                let v = Text(displayName).lineLimit(1)
                if relay.enabled {
                    v
                } else {
                    v.foregroundColor(theme.colors.secondary)
                }
            }
        }
    }
}

struct NewChatRelayView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var theme: AppTheme
    @Binding var userServers: [UserOperatorServers]
    @Binding var serverErrors: [UserServersError]
    @Binding var serverWarnings: [UserServersWarning]
    @State private var relayToEdit = UserChatRelay(
        chatRelayId: nil, address: "", name: "", domains: [],
        preset: false, tested: nil, enabled: true, deleted: false
    )

    var body: some View {
        let validName = validRelayName(relayToEdit.name)
        let validAddress = validRelayAddress(relayToEdit.address)
        List {
            Section {
                TextField("Enter relay name…", text: $relayToEdit.name)
                    .autocorrectionDisabled(true)
            } header: {
                HStack {
                    Text("Relay name").foregroundColor(theme.colors.secondary)
                    if !validName {
                        Spacer()
                        Image(systemName: "exclamationmark.circle").foregroundColor(.red)
                            .onTapGesture { showInvalidRelayNameAlert($relayToEdit.name) }
                    }
                }
            }
            Section {
                TextEditor(text: $relayToEdit.address)
                    .multilineTextAlignment(.leading)
                    .autocorrectionDisabled(true)
                    .autocapitalization(.none)
                    .allowsTightening(true)
                    .lineLimit(10)
                    .frame(height: 144)
                    .padding(-6)
            } header: {
                HStack {
                    Text("Your relay address")
                        .foregroundColor(theme.colors.secondary)
                    if !validAddress {
                        Spacer()
                        Image(systemName: "exclamationmark.circle").foregroundColor(.red)
                    }
                }
            }
            Section(header: Text("Use relay").foregroundColor(theme.colors.secondary)) {
                HStack {
                    Button("Test relay") {
                        showAlert(
                            NSLocalizedString("Not implemented", comment: "alert title"),
                            message: NSLocalizedString("Relay testing is not yet available.", comment: "alert message")
                        )
                    }
                    .disabled(!validAddress)
                    Spacer()
                    showRelayTestStatus(relay: relayToEdit)
                }
                Toggle("Use for new channels", isOn: $relayToEdit.enabled)
            }
        }
        .modifier(BackButton(disabled: Binding.constant(false)) {
            addChatRelay(relayToEdit, $userServers, $serverErrors, $serverWarnings, dismiss)
        })
    }
}
