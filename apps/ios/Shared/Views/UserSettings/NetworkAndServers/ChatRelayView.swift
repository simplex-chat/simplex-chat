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

func validRelayLink(_ address: String) -> Bool {
    if case .simplexLink(_, .relay, _, _) = strHasSingleSimplexLink(address)?.format { return true }
    return false
}

func addChatRelay(
    _ relay: UserChatRelay,
    _ userServers: Binding<[UserOperatorServers]>,
    _ serverErrors: Binding<[UserServersError]>,
    _ serverWarnings: Binding<[UserServersWarning]>? = nil,
    _ operatorIndex: Int,
    _ dismiss: DismissAction
) {
    if validRelayLink(relay.address) {
        if userServers[operatorIndex].wrappedValue.chatRelays == nil {
            userServers[operatorIndex].wrappedValue.chatRelays = [relay]
        } else {
            userServers[operatorIndex].wrappedValue.chatRelays?.append(relay)
        }
        validateServers_(userServers, serverErrors, serverWarnings)
        dismiss()
    } else {
        dismiss()
        if relay.address.trimmingCharacters(in: .whitespaces) != "" {
            showAlert(
                NSLocalizedString("Invalid relay link!", comment: "alert title"),
                message: NSLocalizedString("Check relay address and try again.", comment: "alert message")
            )
        }
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
        let valid = relay.preset || validRelayLink(relayToEdit.address)
        ZStack {
            if relay.preset {
                presetRelay()
            } else {
                customRelay(valid: valid)
            }
        }
        .modifier(BackButton(label: backLabel, disabled: Binding.constant(false)) {
            if relay.preset || validRelayLink(relayToEdit.address) {
                relay = relayToEdit
                validateServers_($userServers, $serverErrors, $serverWarnings)
                dismiss()
            } else {
                dismiss()
                showAlert(
                    NSLocalizedString("Invalid relay link!", comment: "alert title"),
                    message: NSLocalizedString("Check relay address and try again.", comment: "alert message")
                )
            }
        })
    }

    private func presetRelay() -> some View {
        List {
            Section(header: Text("Relay name").foregroundColor(theme.colors.secondary)) {
                TextField("Enter relay name…", text: $relayToEdit.name)
                    .autocorrectionDisabled(true)
            }
            Section(header: Text("Preset relay address").foregroundColor(theme.colors.secondary)) {
                Text(relayToEdit.address)
                    .textSelection(.enabled)
            }
            useRelaySection()
        }
    }

    private func customRelay(valid: Bool) -> some View {
        List {
            Section(header: Text("Relay name").foregroundColor(theme.colors.secondary)) {
                TextField("Enter relay name…", text: $relayToEdit.name)
                    .autocorrectionDisabled(true)
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
                    if !valid {
                        Spacer()
                        Image(systemName: "exclamationmark.circle").foregroundColor(.red)
                    }
                }
            }
            useRelaySection(valid: valid)
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
    var operatorIndex: Int
    @State private var relayToEdit = UserChatRelay(
        chatRelayId: nil, address: "", name: "", domains: [],
        preset: false, tested: nil, enabled: true, deleted: false
    )

    var body: some View {
        let valid = validRelayLink(relayToEdit.address)
        List {
            Section(header: Text("Relay name").foregroundColor(theme.colors.secondary)) {
                TextField("Enter relay name…", text: $relayToEdit.name)
                    .autocorrectionDisabled(true)
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
                    if !valid {
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
                    .disabled(!valid)
                    Spacer()
                    showRelayTestStatus(relay: relayToEdit)
                }
                Toggle("Use for new channels", isOn: $relayToEdit.enabled)
            }
        }
        .modifier(BackButton(disabled: Binding.constant(false)) {
            addChatRelay(relayToEdit, $userServers, $serverErrors, $serverWarnings, operatorIndex, dismiss)
        })
    }
}
