//
//  BadgesChooseBadgeView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.07.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

// TODO [badges]: replace with types produced by the badge purchase API when it lands.
enum BadgeLevel: String, CaseIterable, Identifiable {
    case supporter
    case legend

    var id: String { rawValue }

    var title: LocalizedStringKey {
        switch self {
        case .supporter: "Supporter"
        case .legend: "Legend"
        }
    }

    var fileSize: LocalizedStringKey {
        switch self {
        case .supporter: "Files up to 2 GB"
        case .legend: "Files up to 5 GB"
        }
    }

    var fileStorage: LocalizedStringKey {
        switch self {
        case .supporter: "Stored for 7 days"
        case .legend: "Stored for 21 days"
        }
    }

    var summary: LocalizedStringKey {
        switch self {
        case .supporter: "Supporter: 2 GB files available for 7 days."
        case .legend: "Legend: 5 GB files available for 21 days."
        }
    }

    var badgeType: BadgeType {
        switch self {
        case .supporter: .supporter
        case .legend: .legend
        }
    }
}

struct BadgesChooseBadgeView: View {
    @EnvironmentObject var theme: AppTheme
    @ObservedObject private var store = BadgeStore.shared
    @State private var selectedLevel: BadgeLevel = .supporter
    @State private var continueActive = false

    var body: some View {
        GeometryReader { g in
            ScrollView {
                VStack(alignment: .center, spacing: 16) {
                    Text("Choose your badge")
                        .font(.largeTitle)
                        .bold()
                        .foregroundColor(theme.colors.primary)
                        .multilineTextAlignment(.center)
                        .fixedSize(horizontal: false, vertical: true)

                    Text("Larger files that stay available longer.")
                        .font(.body)
                        .foregroundColor(theme.colors.secondary)
                        .multilineTextAlignment(.center)
                        .fixedSize(horizontal: false, vertical: true)

                    BadgeUserPreview(level: selectedLevel)
                        .padding(.top, 4)

                    Spacer(minLength: 12)

                    // fixedSize + maxHeight on the cards so both match the taller one when a
                    // store price wraps in one of them
                    HStack(alignment: .top, spacing: 12) {
                        levelCard(.supporter)
                        levelCard(.legend)
                    }
                    .fixedSize(horizontal: false, vertical: true)

                    Spacer(minLength: 12)

                    VStack(spacing: 10) {
                        continueButton()
                            .padding(.vertical, 10)
                        // redeeming a code is here only when Support SimpleX offers the browser instead
                        Group {
                            if badgeBrowserAllowed {
                                RedeemCodeButton()
                            } else {
                                Color.clear
                            }
                        }
                        .frame(height: 22)
                    }
                    .padding(.bottom, g.safeAreaInsets.bottom == 0 ? 20 : 0)
                }
                .padding(.horizontal, 25)
                .padding(.top, 0)
                .padding(.bottom, 20)
                .frame(minHeight: g.size.height)
            }
        }
        .frame(maxHeight: .infinity)
        .navigationBarTitleDisplayMode(.inline)
        .task { await store.load() }
    }

    private func levelCard(_ level: BadgeLevel) -> some View {
        let isSelected = level == selectedLevel
        return Button {
            selectedLevel = level
        } label: {
            VStack(spacing: 10) {
                Image(badgeImageName(level.badgeType))
                    .resizable()
                    .scaledToFit()
                    .frame(width: 44, height: 44)
                    .padding(.top, 16)
                Text(level.title)
                    .font(.title3)
                    .fontWeight(.bold)
                BadgePeriod.monthly.priceText(store.price(level, .monthly))
                    .font(.body)
                VStack(spacing: 2) {
                    Text(level.fileSize)
                    Text(level.fileStorage)
                }
                .font(.subheadline)
                .foregroundColor(theme.colors.secondary)
                .padding(.bottom, 16)
            }
            .multilineTextAlignment(.center)
            .padding(.horizontal, 12)
            .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
            .background(Color(uiColor: .secondarySystemGroupedBackground))
            .clipShape(RoundedRectangle(cornerRadius: 16))
            .overlay(
                RoundedRectangle(cornerRadius: 16)
                    .stroke(isSelected ? theme.colors.primary : Color(uiColor: .secondarySystemFill), lineWidth: 2)
            )
        }
        .buttonStyle(.plain)
    }

    private func continueButton() -> some View {
        ZStack {
            Button {
                continueActive = true
            } label: {
                Text("Continue")
            }
            .buttonStyle(OnboardingButtonStyle(isDisabled: false))

            NavigationLink(isActive: $continueActive) {
                BadgesHowLongView(level: selectedLevel)
                    .modifier(ThemedBackground())
            } label: {
                EmptyView()
            }
            .frame(width: 1, height: 1)
            .hidden()
        }
    }
}

struct BadgesChooseBadgeView_Previews: PreviewProvider {
    static var previews: some View {
        NavigationView {
            BadgesChooseBadgeView()
        }
    }
}
