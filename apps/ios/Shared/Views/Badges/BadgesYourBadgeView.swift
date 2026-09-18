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
                Section {
                    Text(badgeState.paidThroughText)
                } header: {
                    Text("Ends")
                        .foregroundColor(theme.colors.secondary)
                } footer: {
                    Text("Prepaid months have no billing date. The badge is reissued each month from the balance you already paid for, and ends when it runs out.")
                        .foregroundColor(theme.colors.secondary)
                }
                Section {
                    NavigationLink {
                        BadgesHowItWorksView()
                            .modifier(ThemedBackground())
                    } label: {
                        settingsRow("info.circle", color: theme.colors.secondary) {
                            Text("How private badges work")
                        }
                    }
                }
                if let issueError = badgeState.issueError {
                    Section {
                        Text(issueFailureText(issueError.reason))
                            .foregroundColor(theme.colors.secondary)
                        infoRow("Since", badgeTimestamp(issueError.failedSince))
                        if let nextWakeAt = badgeState.nextWakeAt {
                            infoRow("Next attempt", badgeTimestamp(nextWakeAt))
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
                            infoRow("Last attempt", badgeTimestamp(issueError.lastAttemptAt))
                            infoRow("Last error", issueFailureTag(issueError.reason))
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

    private func badgeTimestamp(_ date: Date) -> String {
        DateFormatter.localizedString(from: date, dateStyle: .medium, timeStyle: .short)
    }

    private func issueFailureText(_ reason: BadgeIssueFailure) -> String {
        switch reason {
        case let .serviceError(code, _):
            String.localizedStringWithFormat(NSLocalizedString("The badge service refused the renewal: %@", comment: "badge renewal error"), code.text)
        case .serviceTimeout: NSLocalizedString("The badge service did not respond.", comment: "badge renewal error")
        case .network: NSLocalizedString("The badge service could not be reached.", comment: "badge renewal error")
        case .invalidCredential: NSLocalizedString("The badge issued by the service cannot be verified.", comment: "badge renewal error")
        case let .unexpected(message):
            String.localizedStringWithFormat(NSLocalizedString("Unexpected error: %@", comment: "badge renewal error"), message)
        }
    }

    private func issueFailureTag(_ reason: BadgeIssueFailure) -> String {
        switch reason {
        case let .serviceError(code, retryable): "serviceError \(retryable ? "retry" : "final") \(code.text)"
        case .serviceTimeout: "serviceTimeout"
        case let .network(agentError): "network \(agentError)"
        case .invalidCredential: "invalidCredential"
        case let .unexpected(message): "unexpected \(message)"
        }
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
