//
//  BadgesPayView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.07.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

// Draft billing periods used by the badges UI while the API/state machine is still being designed.
// TODO [badges]: replace with types produced by the badge purchase API when it lands.
enum BadgePeriod: String, CaseIterable, Identifiable {
    case oneMonth
    case subscribe

    var id: String { rawValue }

    var icon: String {
        switch self {
        case .oneMonth: "calendar"
        case .subscribe: "arrow.clockwise"
        }
    }

    var label: LocalizedStringKey {
        switch self {
        case .oneMonth: "1 month"
        case .subscribe: "Subscribe"
        }
    }
}

struct BadgesPayView: View {
    @EnvironmentObject var chatModel: ChatModel
    @EnvironmentObject var theme: AppTheme
    let level: BadgeLevel
    @State private var selectedPeriod: BadgePeriod = .subscribe

    var body: some View {
        GeometryReader { g in
            ScrollView {
                VStack(alignment: .center, spacing: 16) {
                    Text(level.title)
                        .font(.largeTitle)
                        .bold()
                        .foregroundColor(theme.colors.primary)
                        .multilineTextAlignment(.center)
                        .fixedSize(horizontal: false, vertical: true)

                    userPreview()
                        .padding(.top, 4)

                    Text(level.tagline)
                        .font(.body)
                        .foregroundColor(theme.colors.secondary)
                        .multilineTextAlignment(.center)
                        .fixedSize(horizontal: false, vertical: true)
                        .padding(.top, 4)

                    HStack(alignment: .top, spacing: 12) {
                        periodCard(.oneMonth)
                        periodCard(.subscribe)
                    }
                    .padding(.top, 12)

                    Spacer(minLength: 20)

                    payButton()

                    Text(billingFooter)
                        .font(.footnote)
                        .foregroundColor(theme.colors.secondary)
                        .multilineTextAlignment(.center)
                        .fixedSize(horizontal: false, vertical: true)
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
                Image(level.badgeAsset)
                    .resizable()
                    .scaledToFit()
                    .frame(width: 28, height: 28)
            }
        }
    }

    private func periodCard(_ period: BadgePeriod) -> some View {
        let isSelected = period == selectedPeriod
        return Button {
            withAnimation { selectedPeriod = period }
        } label: {
            VStack(spacing: 12) {
                Image(systemName: period.icon)
                    .resizable()
                    .scaledToFit()
                    .frame(width: 32, height: 32)
                    .foregroundColor(isSelected ? theme.colors.primary : theme.colors.secondary)
                    .padding(.top, 20)
                Text(period.label)
                    .font(.title3)
                    .fontWeight(.bold)
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

    private func payButton() -> some View {
        Button {
            // TODO [badges] wire to purchase API when it lands.
        } label: {
            Text(selectedPeriod == .subscribe ? level.payMonthlyLabel : level.payOnceLabel)
        }
        .buttonStyle(OnboardingButtonStyle(isDisabled: false))
    }

    // TODO [badges] source the actual renewal/end date from the purchase state machine when wired.
    private var billingFooter: LocalizedStringKey {
        switch selectedPeriod {
        case .subscribe: "Renews on July 22, 2026. Cancel anytime."
        case .oneMonth: "Ends on August 22, 2026."
        }
    }
}

struct BadgesPayView_Previews: PreviewProvider {
    static var previews: some View {
        NavigationView {
            BadgesPayView(level: .supporter)
        }
    }
}
