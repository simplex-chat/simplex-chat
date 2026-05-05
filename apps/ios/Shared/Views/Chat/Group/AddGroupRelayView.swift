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
    @State private var availableRelays: [(relayId: Int64, relay: UserChatRelay, operatorName: String?)] = []
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
                        ForEach(availableRelays, id: \.relayId) { item in
                            relayCheckRow(item.relayId, item.relay, operatorName: item.operatorName)
                        }
                    }
                }
            }
            .modifier(ThemedBackground(grouped: true))
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

    private func relayCheckRow(_ relayId: Int64, _ relay: UserChatRelay, operatorName: String?) -> some View {
        let selected = selectedRelayIds.contains(relayId)
        return Button {
            if selected {
                selectedRelayIds.remove(relayId)
            } else {
                selectedRelayIds.insert(relayId)
            }
        } label: {
            HStack {
                VStack(alignment: .leading) {
                    Text(chatRelayDisplayName(relay))
                        .foregroundColor(theme.colors.onBackground)
                        .lineLimit(1)
                    if let opName = operatorName {
                        Text(opName)
                            .font(.caption)
                            .foregroundColor(theme.colors.secondary)
                            .lineLimit(1)
                    }
                }
                Spacer()
                Image(systemName: selected ? "checkmark.circle.fill" : "circle")
                    .foregroundColor(selected ? theme.colors.primary : Color(uiColor: .tertiaryLabel).asAnotherColorFromSecondary(theme))
            }
        }
    }

    private func loadAvailableRelays() async {
        do {
            let servers = try await getUserServers()
            var relays: [(relayId: Int64, relay: UserChatRelay, operatorName: String?)] = []
            for op in servers {
                if let oper = op.operator, oper.enabled != true { continue }
                let opName: String? = op.operator?.operatorTag != nil ? op.operator?.tradeName : nil
                for relay in op.chatRelays {
                    if relay.enabled && !relay.deleted,
                       let relayId = relay.chatRelayId,
                       !existingRelayIds.contains(relayId) {
                        relays.append((relayId, relay, opName))
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
                guard let result = try await apiAddGroupRelays(groupInfo.groupId, relayIds: relayIds) else {
                    await MainActor.run { isAdding = false }
                    return
                }
                await MainActor.run {
                    isAdding = false
                    switch result {
                    case let .added(gInfo, _, relays):
                        ChannelRelaysModel.shared.set(groupId: gInfo.groupId, groupRelays: relays)
                        onRelayAdded()
                        dismiss()
                    case let .addFailed(results):
                        let successIds = Set(results.filter { $0.relayError == nil }.compactMap { $0.relay.chatRelayId })
                        if !successIds.isEmpty {
                            selectedRelayIds.subtract(successIds)
                            availableRelays.removeAll { successIds.contains($0.relayId) }
                            onRelayAdded()
                        }
                        let errorLines = results.filter { $0.relayError != nil }
                            .map { "\(chatRelayDisplayName($0.relay)): \($0.relayError.map { connErrorText($0) } ?? "")" }
                        let successNames = results.filter { $0.relayError == nil }
                            .map { chatRelayDisplayName($0.relay) }
                        var msg = errorLines.joined(separator: "\n")
                        if !successNames.isEmpty {
                            msg += "\n" + String.localizedStringWithFormat(NSLocalizedString("Relays added: %@.", comment: "alert message"), successNames.joined(separator: ", "))
                        }
                        showAlert(
                            NSLocalizedString("Error adding relays", comment: "alert title"),
                            message: msg
                        )
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
