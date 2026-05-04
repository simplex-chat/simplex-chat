//
//  AddGroupRelayView.swift
//  SimpleX (iOS)
//
//  Created by simplex on 29.04.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct AddGroupRelayView: View {
    var groupInfo: GroupInfo
    var existingRelayIds: Set<Int64>
    var onRelayAdded: () -> Void
    @EnvironmentObject var theme: AppTheme
    @Environment(\.dismiss) var dismiss
    @State private var availableRelays: [UserChatRelay] = []
    @State private var selectedRelayIds: Set<Int64> = []
    @State private var isLoading = true
    @State private var isAdding = false

    var body: some View {
        NavigationView {
            List {
                if isLoading {
                    Section {
                        ProgressView()
                            .frame(maxWidth: .infinity)
                    }
                } else if availableRelays.isEmpty {
                    Section {
                        Text("No available relays")
                            .foregroundColor(theme.colors.secondary)
                    }
                } else {
                    Section {
                        ForEach(availableRelays, id: \.id) { relay in
                            relayCheckRow(relay)
                        }
                    } footer: {
                        Text("Select relays to add to the channel.")
                    }
                }
            }
            .navigationTitle("Add relays")
            .navigationBarTitleDisplayMode(.inline)
            .toolbar {
                ToolbarItem(placement: .cancellationAction) {
                    Button("Cancel") { dismiss() }
                }
                ToolbarItem(placement: .confirmationAction) {
                    Button("Add") { addSelectedRelays() }
                        .disabled(selectedRelayIds.isEmpty || isAdding)
                }
            }
        }
        .task { await loadAvailableRelays() }
    }

    private func relayCheckRow(_ relay: UserChatRelay) -> some View {
        let relayId = relay.chatRelayId!
        let selected = selectedRelayIds.contains(relayId)
        let title = !relay.displayName.isEmpty ? relay.displayName : (relay.domains.first ?? relay.address)
        return Button {
            if selected {
                selectedRelayIds.remove(relayId)
            } else {
                selectedRelayIds.insert(relayId)
            }
        } label: {
            HStack {
                VStack(alignment: .leading) {
                    Text(title)
                        .foregroundColor(theme.colors.onBackground)
                        .lineLimit(1)
                    if let domain = relay.domains.first, !relay.displayName.isEmpty {
                        Text(domain)
                            .font(.caption)
                            .foregroundColor(theme.colors.secondary)
                            .lineLimit(1)
                    }
                }
                Spacer()
                Image(systemName: selected ? "checkmark.circle.fill" : "circle")
                    .foregroundColor(selected ? .accentColor : theme.colors.secondary)
            }
        }
    }

    private func loadAvailableRelays() async {
        do {
            let servers = try await getUserServers()
            var relays: [UserChatRelay] = []
            for op in servers {
                if let oper = op.operator, oper.enabled != true { continue }
                for relay in op.chatRelays {
                    if relay.enabled && !relay.deleted,
                       let relayId = relay.chatRelayId,
                       !existingRelayIds.contains(relayId) {
                        relays.append(relay)
                    }
                }
            }
            await MainActor.run {
                availableRelays = relays
                isLoading = false
            }
        } catch {
            logger.error("loadAvailableRelays error: \(responseError(error))")
            await MainActor.run {
                isLoading = false
            }
        }
    }

    private func addSelectedRelays() {
        let relayIds = Array(selectedRelayIds)
        guard !relayIds.isEmpty else { return }
        isAdding = true
        Task {
            do {
                let result = try await apiAddGroupRelays(groupInfo.groupId, relayIds: relayIds)
                await MainActor.run {
                    isAdding = false
                    switch result {
                    case let .added(gInfo, _, relays):
                        ChannelRelaysModel.shared.set(groupId: gInfo.groupId, groupRelays: relays)
                        onRelayAdded()
                        dismiss()
                    case let .addFailed(results):
                        let errors = results.compactMap { $0.relayError }.map { responseError($0) }
                        showAlert(NSLocalizedString("Error adding relays", comment: "alert title"), message: errors.joined(separator: "\n"))
                    }
                }
            } catch {
                await MainActor.run {
                    isAdding = false
                    showAlert(NSLocalizedString("Error adding relays", comment: "alert title"), message: responseError(error))
                }
            }
        }
    }
}
