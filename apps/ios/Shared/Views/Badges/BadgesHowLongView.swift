//
//  BadgesHowLongView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.07.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

// TODO [badges]: replace with types produced by the badge purchase API when it lands.
enum BadgePeriod: String, CaseIterable, Identifiable {
    case oneMonth
    case monthly
    case annual

    var id: String { rawValue }

    var icon: String {
        switch self {
        case .oneMonth: "calendar"
        case .monthly: "arrow.clockwise"
        case .annual: "arrow.clockwise"
        }
    }

    var label: LocalizedStringKey {
        switch self {
        case .oneMonth: "1 month"
        case .monthly: "Monthly"
        case .annual: "Annual"
        }
    }

    func priceText(_ price: BadgePrice) -> Text {
        switch price {
        case .loading: return Text(verbatim: "…")
        case .unavailable: return Text(verbatim: "—")
        case let .price(p):
            switch self {
            case .oneMonth: return Text(verbatim: p)
            case .monthly: return Text("\(p)/month")
            case .annual: return Text("\(p)/year")
            }
        }
    }

    func payText(_ price: BadgePrice) -> Text {
        switch price {
        case .loading: return Text("Loading…")
        case .unavailable: return Text("Not available")
        case let .price(p):
            switch self {
            case .oneMonth: return Text("Pay \(p)")
            case .monthly: return Text("Pay \(p)/month")
            case .annual: return Text("Pay \(p)/year")
            }
        }
    }
}

struct BadgesHowLongView: View {
    @EnvironmentObject var theme: AppTheme
    @ObservedObject private var store = BadgeStore.shared
    let level: BadgeLevel
    @State private var selectedPeriod: BadgePeriod = .monthly
    @State private var continueActive = false

    var body: some View {
        GeometryReader { g in
            ScrollView {
                VStack(alignment: .center, spacing: 16) {
                    Text("How long?")
                        .font(.largeTitle)
                        .bold()
                        .foregroundColor(theme.colors.primary)
                        .multilineTextAlignment(.center)
                        .fixedSize(horizontal: false, vertical: true)

                    Text(level.summary)
                        .font(.body)
                        .foregroundColor(theme.colors.secondary)
                        .multilineTextAlignment(.center)
                        .fixedSize(horizontal: false, vertical: true)

                    BadgeUserPreview(level: level)
                        .padding(.top, 4)

                    Spacer(minLength: 20)

                    // fixedSize + maxHeight on the cards so all three match the tallest one -
                    // only Annual carries a savings line, and prices wrap at large fonts
                    HStack(alignment: .top, spacing: 12) {
                        periodCard(.oneMonth)
                        periodCard(.monthly)
                        periodCard(.annual)
                    }
                    .fixedSize(horizontal: false, vertical: true)

                    Spacer(minLength: 20)

                    VStack(spacing: 10) {
                        continueButton()
                            .padding(.vertical, 10)
                        BadgeBillingFooter(period: selectedPeriod)
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

    private func periodCard(_ period: BadgePeriod) -> some View {
        let isSelected = period == selectedPeriod
        return Button {
            selectedPeriod = period
        } label: {
            VStack(spacing: 12) {
                Image(systemName: period.icon)
                    .resizable()
                    .scaledToFit()
                    .frame(width: 32, height: 32)
                    .foregroundColor(isSelected ? theme.colors.primary : theme.colors.secondary)
                Text(period.label)
                    .font(.title3)
                    .fontWeight(.bold)
                period.priceText(store.price(level, period))
                    .font(.body)
                if let percent = savingsPercent(period) {
                    Text("Save \(percent)%")
                        .font(.footnote)
                        .foregroundColor(isSelected ? theme.colors.primary : theme.colors.secondary)
                }
            }
            .multilineTextAlignment(.center)
            .padding(.vertical, 25)
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

    private func savingsPercent(_ period: BadgePeriod) -> Int? {
        period == .annual ? store.annualSavings(level) : nil
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
                BadgesCheckOrderView(level: level, period: selectedPeriod)
                    .modifier(ThemedBackground())
            } label: {
                EmptyView()
            }
            .frame(width: 1, height: 1)
            .hidden()
        }
    }
}

struct BadgeBillingFooter: View {
    @EnvironmentObject var theme: AppTheme
    let period: BadgePeriod

    var body: some View {
        Text(billingFooter)
            .font(.footnote)
            .foregroundColor(theme.colors.secondary)
            .multilineTextAlignment(.center)
            .fixedSize(horizontal: false, vertical: true)
            .frame(height: 22)
    }

    private var billingFooter: LocalizedStringKey {
        // TODO [badges] source the actual date from the purchase state machine when wired.
        var comps = DateComponents(); comps.year = 2026; comps.month = 7; comps.day = 22
        let stubDate = Calendar.current.date(from: comps) ?? Date()
        let date = DateFormatter.localizedString(from: stubDate, dateStyle: .long, timeStyle: .none)
        switch period {
        case .monthly, .annual: return "Renews on \(date). Cancel anytime."
        case .oneMonth: return "Ends on \(date)."
        }
    }
}

struct BadgesHowLongView_Previews: PreviewProvider {
    static var previews: some View {
        NavigationView {
            BadgesHowLongView(level: .supporter)
        }
    }
}
