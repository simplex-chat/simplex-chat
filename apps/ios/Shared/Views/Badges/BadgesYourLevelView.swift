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

    var badgeAsset: String {
        switch self {
        case .supporter: "badge-supporter"
        case .legend: "badge-legend"
        }
    }
}

struct BadgesYourLevelView: View {
    @EnvironmentObject var chatModel: ChatModel
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

                    userPreview()
                        .padding(.top, 4)

                    HStack(alignment: .top, spacing: 12) {
                        levelCard(.supporter)
                        levelCard(.legend)
                    }
                    .padding(.top, 8)

                    Spacer(minLength: 20)

                    continueButton()

                    howItWorksButton()
                        .padding(.top, 4)
                        .padding(.bottom, g.safeAreaInsets.bottom == 0 ? 20 : 0)
                }
                .padding(.horizontal, 25)
                .padding(.top, 8)
                .padding(.bottom, 20)
                .frame(minHeight: g.size.height)
            }
        }
        .frame(maxHeight: .infinity)
    }

    // The avatar + name preview shows the user how their profile will look with the selected badge.
    // Uses the current user's real profile image and display name; when SIMPLEX_ASSETS is absent the
    // avatar falls back to ProfileImage's own default. TODO [badges] wire real LocalBadge preview.
    private func userPreview() -> some View {
        let user = chatModel.currentUser
        return VStack(spacing: 12) {
            ProfileImage(imageStr: user?.image, size: 128)
            HStack(alignment: .center, spacing: 6) {
                Text(user?.displayName ?? NSLocalizedString("My nickname", comment: "badges preview placeholder"))
                    .font(.title2)
                    .fontWeight(.semibold)
                    .lineLimit(1)
                    .minimumScaleFactor(0.75)
                Image(selectedLevel.badgeAsset)
                    .resizable()
                    .scaledToFit()
                    .frame(width: 28, height: 28)
                Image(systemName: "chevron.down")
                    .font(.body)
                    .foregroundColor(theme.colors.primary)
            }
        }
    }

    private func levelCard(_ level: BadgeLevel) -> some View {
        let isSelected = level == selectedLevel
        return Button {
            withAnimation { selectedLevel = level }
        } label: {
            VStack(spacing: 10) {
                Image(level.badgeAsset)
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
                    .stroke(isSelected ? theme.colors.primary : Color.clear, lineWidth: 2)
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
                Label("How private badges work", systemImage: "info.circle")
                    .font(.headline)
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
