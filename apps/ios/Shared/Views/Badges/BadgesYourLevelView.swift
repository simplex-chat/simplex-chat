//
//  BadgesYourLevelView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.07.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

// Draft levels used by the badges UI while the API/state machine is still being designed. TODO [badges]:
// replace with types produced by the badge purchase API when it lands.
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

    var filesDescription: LocalizedStringKey {
        switch self {
        case .supporter: "Send 2GB files"
        case .legend: "Send 5GB files"
        }
    }

    var monthlyPrice: LocalizedStringKey {
        switch self {
        case .supporter: "$7/month"
        case .legend: "$70/month"
        }
    }

    var oneMonthPrice: LocalizedStringKey {
        switch self {
        case .supporter: "$7"
        case .legend: "$70"
        }
    }

    var payMonthlyLabel: LocalizedStringKey {
        switch self {
        case .supporter: "Pay $7/month"
        case .legend: "Pay $70/month"
        }
    }

    var payOnceLabel: LocalizedStringKey {
        switch self {
        case .supporter: "Pay $7"
        case .legend: "Pay $70"
        }
    }

    var tagline: LocalizedStringKey {
        switch self {
        case .supporter: "Optional profile badge\nand 2GB files"
        case .legend: "Optional profile badge\nand 5GB files"
        }
    }

    var badgeType: BadgeType {
        switch self {
        case .supporter: .supporter
        case .legend: .legend
        }
    }
}

struct BadgesYourLevelView: View {
    @EnvironmentObject var theme: AppTheme
    @State private var selectedLevel: BadgeLevel = .supporter
    @State private var continueActive = false
    @State private var howItWorksActive = false

    var body: some View {
        GeometryReader { g in
            ScrollView {
                VStack(alignment: .center, spacing: 16) {
                    Text("Your level")
                        .font(.largeTitle)
                        .bold()
                        .foregroundColor(theme.colors.primary)
                        .multilineTextAlignment(.center)
                        .fixedSize(horizontal: false, vertical: true)

                    BadgeUserPreview(level: selectedLevel) {
                        Image(systemName: "chevron.down")
                            .font(.body)
                            .foregroundColor(theme.colors.primary)
                    }
                    .padding(.top, 4)

                    Spacer(minLength: 20)

                    HStack(alignment: .top, spacing: 12) {
                        levelCard(.supporter)
                        levelCard(.legend)
                    }

                    Spacer(minLength: 20)

                    VStack(spacing: 10) {
                        continueButton()
                            .padding(.vertical, 10)
                        howItWorksButton()
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
                    .frame(width: 60, height: 60)
                    .padding(.top, 20)
                Text(level.title)
                    .font(.title3)
                    .fontWeight(.bold)
                Text(level.filesDescription)
                    .font(.subheadline)
                    .foregroundColor(theme.colors.secondary)
                Text(level.monthlyPrice)
                    .font(.body)
                    .padding(.bottom, 20)
            }
            .frame(maxWidth: .infinity)
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
                BadgesPayView(level: selectedLevel)
                    .modifier(ThemedBackground())
            } label: {
                EmptyView()
            }
            .frame(width: 1, height: 1)
            .hidden()
        }
    }

    private func howItWorksButton() -> some View {
        ZStack {
            Button {
                howItWorksActive = true
            } label: {
                HStack(spacing: 4) {
                    Image(systemName: "info.circle")
                    Text("How private badges work").fontWeight(.medium)
                }
                .font(.body)
            }

            NavigationLink(isActive: $howItWorksActive) {
                BadgesHowItWorksView()
                    .modifier(ThemedBackground())
            } label: {
                EmptyView()
            }
            .frame(width: 1, height: 1)
            .hidden()
        }
    }
}

struct BadgesYourLevelView_Previews: PreviewProvider {
    static var previews: some View {
        NavigationView {
            BadgesYourLevelView()
        }
    }
}
