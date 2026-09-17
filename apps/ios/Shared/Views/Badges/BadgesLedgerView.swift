//
//  BadgesLedgerView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 17.09.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct BadgesLedgerView: View {
    @EnvironmentObject var theme: AppTheme
    @EnvironmentObject var chatModel: ChatModel
    let badgeState: BadgeState
    @State private var entries: [StatementEntry]? = nil
    @State private var expanded: Set<String> = []

    var body: some View {
        List {
            if let entries {
                Section {
                    if entries.isEmpty {
                        Text("No entries")
                            .foregroundColor(theme.colors.secondary)
                    } else {
                        ForEach(entries, id: \.entryId) { entry in
                            ledgerRow(entry)
                        }
                    }
                }
            }
        }
        .navigationTitle("Badge ledger")
        .navigationBarTitleDisplayMode(.inline)
        .modifier(ThemedBackground(grouped: true))
        .onAppear(perform: loadLedger)
    }

    @ViewBuilder private func ledgerRow(_ entry: StatementEntry) -> some View {
        let isExpanded = expanded.contains(entry.entryId)
        Button {
            withAnimation {
                if isExpanded { expanded.remove(entry.entryId) } else { expanded.insert(entry.entryId) }
            }
        } label: {
            HStack {
                Text(entry.entryType.text)
                Spacer()
                Text(entry.changeMonths >= 0 ? "+\(entry.changeMonths)" : "\(entry.changeMonths)")
                    .foregroundStyle(.secondary)
                Image(systemName: isExpanded ? "chevron.up" : "chevron.down")
                    .foregroundColor(theme.colors.secondary)
            }
        }
        .foregroundColor(theme.colors.onBackground)
        if isExpanded {
            infoRow("Date", dateTimeText(entry.createdAt))
            infoRow("Balance", "\(entry.balanceMonths)")
            infoRow("Balance start", dateTimeText(entry.balanceStartTs))
            infoRow("Anchor", dateTimeText(entry.balanceAnchorTs))
            infoRow("Badge type", entry.balanceBadgeType.text)
            if let pausedSince = entry.wasPausedSince {
                infoRow("Paused since", dateTimeText(pausedSince))
            }
            infoRow("Entry ID", entry.entryId)
            payloadRow(entry.entryType)
        }
    }

    @ViewBuilder private func payloadRow(_ entryType: StatementEntryType) -> some View {
        switch entryType {
        case let .credit(credit):
            switch credit {
            case let .payment(invoiceId):
                if let invoiceId { infoRow("Invoice ID", invoiceId) }
            case let .charge(chargeId): infoRow("Charge ID", chargeId)
            case let .transferIn(fromPurchaseKey): infoRow("From purchase key", fromPurchaseKey)
            case .code, .support, .opening, .unknown: EmptyView()
            }
        case let .debit(debit):
            switch debit {
            case let .upgrade(toPurchaseKey), let .transferOut(toPurchaseKey): infoRow("To purchase key", toPurchaseKey)
            case .refund, .support, .badge, .lapse, .unknown: EmptyView()
            }
        }
    }

    private func dateTimeText(_ date: Date) -> String {
        DateFormatter.localizedString(from: date, dateStyle: .medium, timeStyle: .short)
    }

    private func loadLedger() {
        guard let user = chatModel.currentUser else { return }
        Task {
            do {
                let ledger = try await apiGetBadgeLedger(user.userId, badgeState.badgePurchaseId)
                await MainActor.run { entries = ledger }
            } catch let e {
                logger.error("apiGetBadgeLedger error: \(responseError(e))")
                await MainActor.run {
                    showErrorAlert(e, NSLocalizedString("Error", comment: ""))
                }
            }
        }
    }
}
