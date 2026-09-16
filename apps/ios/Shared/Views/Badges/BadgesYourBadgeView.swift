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
            }
        }
        .frame(maxHeight: .infinity)
        .navigationTitle(showsAsSheet ? "" : "Your badge")
        .navigationBarTitleDisplayMode(showsAsSheet ? .inline : .large)
        .modifier(ThemedBackground(grouped: true))
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
