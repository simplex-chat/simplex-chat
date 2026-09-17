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
        .toolbar {
            ToolbarItem(placement: .navigationBarTrailing) {
                Button { showShareSheet(items: [ledgerShareText()]) } label: {
                    Image(systemName: "square.and.arrow.up")
                }
                .disabled(entries?.isEmpty ?? true)
            }
        }
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
                Text(changeText(entry))
                    .foregroundStyle(.secondary)
                Image(systemName: isExpanded ? "chevron.up" : "chevron.down")
                    .foregroundColor(theme.colors.secondary)
            }
        }
        .foregroundColor(theme.colors.onBackground)
        if isExpanded {
            ForEach(entryFields(entry), id: \.0) { field in
                infoRow(Text(field.0), field.1)
            }
        }
    }

    private func changeText(_ entry: StatementEntry) -> String {
        entry.changeMonths >= 0 ? "+\(entry.changeMonths)" : "\(entry.changeMonths)"
    }

    private func entryFields(_ entry: StatementEntry) -> [(String, String)] {
        var fields = [
            (NSLocalizedString("Date", comment: "ledger entry field"), dateTimeText(entry.createdAt)),
            (NSLocalizedString("Balance", comment: "ledger entry field"), "\(entry.balanceMonths)"),
            (NSLocalizedString("Balance start", comment: "ledger entry field"), dateTimeText(entry.balanceStartTs)),
            (NSLocalizedString("Anchor", comment: "ledger entry field"), dateTimeText(entry.balanceAnchorTs)),
            (NSLocalizedString("Badge type", comment: "ledger entry field"), entry.balanceBadgeType.text)
        ]
        if let pausedSince = entry.wasPausedSince {
            fields.append((NSLocalizedString("Paused since", comment: "ledger entry field"), dateTimeText(pausedSince)))
        }
        fields.append((NSLocalizedString("Entry ID", comment: "ledger entry field"), entry.entryId))
        if let payload = payloadField(entry.entryType) {
            fields.append(payload)
        }
        return fields
    }

    private func payloadField(_ entryType: StatementEntryType) -> (String, String)? {
        switch entryType {
        case let .credit(credit):
            switch credit {
            case let .payment(invoiceId):
                return invoiceId.map { (NSLocalizedString("Invoice ID", comment: "ledger entry field"), $0) }
            case let .charge(chargeId):
                return (NSLocalizedString("Charge ID", comment: "ledger entry field"), chargeId)
            case let .transferIn(fromPurchaseKey):
                return (NSLocalizedString("From purchase key", comment: "ledger entry field"), fromPurchaseKey)
            case .code, .support, .opening, .unknown:
                return nil
            }
        case let .debit(debit):
            switch debit {
            case let .upgrade(toPurchaseKey), let .transferOut(toPurchaseKey):
                return (NSLocalizedString("To purchase key", comment: "ledger entry field"), toPurchaseKey)
            case .refund, .support, .badge, .lapse, .unknown:
                return nil
            }
        }
    }

    // the JSON as core sent it: English field names and ISO dates, for support
    private func ledgerShareText() -> String {
        let encoder = getJSONEncoder()
        encoder.outputFormatting = .prettyPrinted
        let data = (try? encoder.encode(entries ?? [])) ?? Data()
        return String(decoding: data, as: UTF8.self)
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
