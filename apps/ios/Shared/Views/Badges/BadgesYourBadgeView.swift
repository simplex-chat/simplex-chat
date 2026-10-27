//
//  BadgesYourBadgeView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 11.09.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct BadgesYourBadgeView: View {
    @EnvironmentObject var theme: AppTheme
    @EnvironmentObject var chatModel: ChatModel
    @Environment(\.dismiss) private var dismiss
    @AppStorage(DEFAULT_DEVELOPER_TOOLS) private var developerTools = false
    @ObservedObject private var store = BadgeStore.shared
    let badgeState: BadgeState
    var showsAsSheet: Bool = false

    var body: some View {
        VStack(spacing: 0) {
            if showsAsSheet {
                Text("Your badge")
                    .font(.largeTitle)
                    .bold()
                    .foregroundColor(theme.colors.primary)
                    .multilineTextAlignment(.center)
                    .fixedSize(horizontal: false, vertical: true)
                    .padding(.horizontal, 25)
                    .padding(.top, 48)
            }

            List {
                Section {
                    BadgeSummary(badgeState: badgeState)
                }
                if let subscription = badgeState.subscription {
                    let ended = badgeState.paidThrough <= Date.now
                    let date = badgeState.paidThroughText
                    switch subscription.status {
                    case .billingRetry:
                        subscriptionSection(
                            HStack(spacing: 6) {
                                Image(systemName: "exclamationmark.triangle")
                                    .foregroundColor(.red)
                                Text("Payment problem")
                            },
                            footer: "The App Store could not charge your payment method and is retrying. Update it to keep your badge. Until the payment goes through, you cannot buy or redeem another badge."
                        )
                    case .cancelled where !ended:
                        subscriptionSection(Text("Ends \(date)"), footer: "Your subscription is cancelled. The badge stays until \(date).")
                    case .ended, .cancelled:
                        subscriptionSection(Text("Ended"), footer: "Your subscription has ended. The badge will be removed from your profile.")
                    case .renewing, .unknown, nil:
                        if ended {
                            subscriptionSection(Text("Renewing"), footer: "Your badge is renewed once the App Store charges your subscription. Until then you cannot buy or redeem another badge.")
                        } else if let price = store.subscriptionPrice(badgeState.badgeType, planMonths: subscription.planMonths) {
                            subscriptionSection(Text("Renews \(date) · \(price)"), footer: "Your subscription renews automatically. You can cancel it in the App Store.")
                        } else {
                            subscriptionSection(Text("Renews \(date)"), footer: "Your subscription renews automatically. You can cancel it in the App Store.")
                        }
                    }
                } else {
                    Section {
                        Text(badgeState.paidThroughText)
                    } header: {
                        Text("Ends")
                            .foregroundColor(theme.colors.secondary)
                    } footer: {
                        Text("Prepaid months have no billing date. The badge is reissued each month from the balance you already paid for, and ends when it runs out.")
                            .foregroundColor(theme.colors.secondary)
                    }
                }
                Section {
                    NavigationLink {
                        BadgesHowItWorksView()
                            .modifier(ThemedBackground())
                    } label: {
                        settingsRow("info.circle", color: theme.colors.secondary) {
                            Text("How badges protect your privacy")
                        }
                    }
                }
                if let issueError = badgeState.issueError {
                    Section {
                        Text(issueError.reason.text)
                            .foregroundColor(theme.colors.secondary)
                        infoRow("Since", badgeTimestamp(issueError.failedSince))
                        if issueError.lastAttemptAt != issueError.failedSince {
                            infoRow("Last attempt", badgeTimestamp(issueError.lastAttemptAt))
                        }
                        settingsRow("number", color: theme.colors.secondary) {
                            Button("Contact SimpleX team") {
                                dismiss()
                                DispatchQueue.main.async {
                                    // simplexTeamURL targets this same app; route to the in-app connect flow
                                    ChatModel.shared.appOpenUrl = simplexTeamURL
                                }
                            }
                        }
                    } header: {
                        HStack(spacing: 6) {
                            Image(systemName: "exclamationmark.triangle")
                                .foregroundColor(.red)
                            Text("Error")
                        }
                    }
                }
                if developerTools {
                    Section(header: Text("Credential").foregroundColor(theme.colors.secondary)) {
                        if let badge = chatModel.currentUser?.profile.localBadge {
                            infoRow("Status", badge.status.rawValue)
                            infoRow("Expires", badgeTimestamp(badge.badge.badgeExpiry))
                        }
                        infoRow("Months left", "\(badgeState.monthsLeft)")
                        infoRow("Purchase ID", "\(badgeState.badgePurchaseId)")
                        if let nextWakeAt = badgeState.nextWakeAt {
                            infoRow("Next check", badgeTimestamp(nextWakeAt))
                        }
                        if let issueError = badgeState.issueError {
                            infoRow("Error", issueError.reason.tag)
                        }
                        Button("Copy purchase key") {
                            UIPasteboard.general.string = badgeState.purchaseKey
                        }
                        NavigationLink {
                            BadgesLedgerView(badgeState: badgeState)
                        } label: {
                            Text("Badge ledger")
                        }
                    }
                }
            }
        }
        .frame(maxHeight: .infinity)
        .navigationTitle(showsAsSheet ? "" : "Your badge")
        .navigationBarTitleDisplayMode(showsAsSheet ? .inline : .large)
        .modifier(ThemedBackground(grouped: true))
    }

    private func subscriptionSection(_ status: some View, footer: LocalizedStringKey) -> some View {
        Section {
            status
            settingsRow("creditcard", color: theme.colors.secondary) {
                Button("Manage subscription") {
                    Task { await BadgeStore.shared.manageSubscriptions() }
                }
            }
        } header: {
            Text("Subscription")
                .foregroundColor(theme.colors.secondary)
        } footer: {
            Text(footer)
                .foregroundColor(theme.colors.secondary)
        }
    }

    private func badgeTimestamp(_ date: Date) -> String {
        DateFormatter.localizedString(from: date, dateStyle: .medium, timeStyle: .short)
    }
}

struct BadgeSummary: View {
    @EnvironmentObject var theme: AppTheme
    let badgeState: BadgeState

    var body: some View {
        VStack(spacing: 4) {
            Image(badgeImageName(badgeState.badgeType))
                .resizable()
                .scaledToFit()
                .frame(width: 68, height: 68)
                .padding(.bottom, 8)

            Text(badgeTypeName(badgeState.badgeType))
                .font(.title3)
                .fontWeight(.semibold)

            Text("shown on your profile")
                .font(.footnote)
                .foregroundColor(theme.colors.secondary)
        }
        .frame(maxWidth: .infinity)
        .padding(.vertical, 8)
    }
}
