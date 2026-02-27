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
        ZStack {
            if relay.preset {
                presetRelay()
            } else {
                customRelay()
            }
        }
        .modifier(BackButton(label: backLabel, disabled: Binding.constant(false)) {
            relay = relayToEdit
            validateServers_($userServers, $serverErrors, $serverWarnings)
            dismiss()
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

    private func customRelay() -> some View {
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
                Text("Your relay address")
                    .foregroundColor(theme.colors.secondary)
            }
            useRelaySection()
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

    private func useRelaySection() -> some View {
        Section(header: Text("Use relay").foregroundColor(theme.colors.secondary)) {
            HStack {
                Button("Test relay") {
                    showAlert(
                        NSLocalizedString("Not implemented", comment: "alert title"),
                        message: NSLocalizedString("Relay testing is not yet available.", comment: "alert message")
                    )
                }
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
        List {
            Section(header: Text("Relay name").foregroundColor(theme.colors.secondary)) {
                TextField("Enter relay name…", text: $relayToEdit.name)
                    .autocorrectionDisabled(true)
            }
            Section(header: Text("Your relay address").foregroundColor(theme.colors.secondary)) {
                TextEditor(text: $relayToEdit.address)
                    .multilineTextAlignment(.leading)
                    .autocorrectionDisabled(true)
                    .autocapitalization(.none)
                    .allowsTightening(true)
                    .lineLimit(10)
                    .frame(height: 144)
                    .padding(-6)
            }
            Section(header: Text("Use relay").foregroundColor(theme.colors.secondary)) {
                HStack {
                    Button("Test relay") {
                        showAlert(
                            NSLocalizedString("Not implemented", comment: "alert title"),
                            message: NSLocalizedString("Relay testing is not yet available.", comment: "alert message")
                        )
                    }
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
