//
//  BadgesCheckOrderView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 29.09.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import StoreKit
import SimpleXChat

struct BadgesCheckOrderView: View {
    @EnvironmentObject var theme: AppTheme
    @EnvironmentObject var chatModel: ChatModel
    @ObservedObject private var store = BadgeStore.shared
    // store.forSale reads the active profile's badge
    @ObservedObject private var badgeModel = BadgeModel.shared
    let level: BadgeLevel
    let period: BadgePeriod
    @State private var purchasing = false
    @State private var privacyPolicyActive = false
    // presented from this view, not AlertManager: its host is behind the sheet these views open in
    @State private var alert: SomeAlert?

    var body: some View {
        GeometryReader { g in
            ScrollView {
                VStack(alignment: .center, spacing: 16) {
                    Text("Check your order")
                        .font(.largeTitle)
                        .bold()
                        .foregroundColor(theme.colors.primary)
                        .multilineTextAlignment(.center)
                        .fixedSize(horizontal: false, vertical: true)

                    VStack(spacing: 0) {
                        orderRow("Your badge", Text(level.title))
                        orderRow("Duration", Text(period.label))
                        Divider()
                            .padding(.top, 8)
                            .padding(.bottom, 6)
                        orderRow("Total", period.priceText(store.price(level, period, compact: false)))
                    }
                    .padding(.horizontal, 24)
                    .padding(.vertical, 12)
                    .background(Color(uiColor: .secondarySystemGroupedBackground))
                    .clipShape(RoundedRectangle(cornerRadius: 16))
                    .padding(.top, 20)

                    if case let .price(p) = store.price(level, period, compact: false) {
                        switch period {
                        case .monthly: footnote("Payment is charged to your Apple ID when you confirm the purchase. The subscription renews automatically at \(p) per month unless you cancel it at least 24 hours before the end of the current period. You can manage and cancel it in your App Store account settings.")
                        case .annual: footnote("Payment is charged to your Apple ID when you confirm the purchase. The subscription renews automatically at \(p) per year unless you cancel it at least 24 hours before the end of the current period. You can manage and cancel it in your App Store account settings.")
                        case .oneMonth: EmptyView()
                        }
                    }

                    if !store.forSale(level, period) && store.subscribedBefore {
                        footnote("This Apple ID already has a badge subscription. Only the profile that bought it can continue it.")
                    }

                    if !AppStore.canMakePayments {
                        footnote("In-app purchases are not allowed on this device.")
                    }

                    Spacer(minLength: 20)

                    VStack(spacing: 10) {
                        payButton()
                            .padding(.vertical, 10)
                        BadgeBillingFooter(period: period)
                        HStack(spacing: 20) {
                            Link("Terms of use", destination: appleStandardEULAURL)
                            ZStack {
                                Button("Privacy policy") { privacyPolicyActive = true }
                                NavigationLink(isActive: $privacyPolicyActive) {
                                    ConditionsTextView()
                                        .padding()
                                        .navigationTitle("Privacy policy")
                                        .modifier(ThemedBackground(grouped: true))
                                } label: {
                                    EmptyView()
                                }
                                .frame(width: 1, height: 1)
                                .hidden()
                            }
                        }
                        .font(.footnote)
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
        .alert(item: $alert) { $0.alert }
    }

    private func footnote(_ text: LocalizedStringKey) -> some View {
        Text(text)
            .font(.footnote)
            .foregroundColor(theme.colors.secondary)
            .frame(maxWidth: .infinity, alignment: .leading)
            .fixedSize(horizontal: false, vertical: true)
    }

    private func orderRow(_ title: LocalizedStringKey, _ value: Text) -> some View {
        HStack {
            Text(title)
                .foregroundColor(theme.colors.secondary)
            Spacer()
            value
                .fontWeight(.medium)
        }
        .font(.body)
        .padding(.vertical, 8)
    }

    private func payButton() -> some View {
        let price = store.price(level, period, compact: false)
        let disabled = !price.canPurchase || purchasing || !store.canBuy(chatModel.currentUser?.userId) || !store.forSale(level, period) || !AppStore.canMakePayments
        return Button {
            purchase()
        } label: {
            period.payText(price)
        }
        .buttonStyle(OnboardingButtonStyle(isDisabled: disabled))
        .disabled(disabled)
    }

    private func purchase() {
        purchasing = true
        Task {
            do {
                if case let .purchased(receipt) = try await store.purchase(level, period), !receipt.signatureVerified {
                    await MainActor.run {
                        alert = SomeAlert(
                            alert: mkAlert(
                                title: "Cannot verify this purchase",
                                message: "The store returned a transaction that Apple has not signed. SimpleX cannot verify this purchase with the App Store."
                            ),
                            id: "badgePurchaseUnverified"
                        )
                    }
                }
                await MainActor.run { purchasing = false }
            } catch let error {
                logger.error("BadgesCheckOrderView.purchase: \(String(describing: error))")
                await MainActor.run {
                    purchasing = false
                    // thrown when the purchase sheet or its sign-in prompt is dismissed, which needs no alert
                    if case .userCancelled? = error as? StoreKitError { return }
                    alert = SomeAlert(
                        alert: Alert(
                            title: Text("Purchase error"),
                            message: Text(verbatim: redeemErrorText(error, purchase: true))
                        ),
                        id: "badgePurchaseError"
                    )
                }
            }
        }
    }
}

// Apple's standard licence agreement, which an app selling subscriptions links as its terms of use
let appleStandardEULAURL = URL(string: "https://www.apple.com/legal/internet-services/itunes/dev/stdeula/")!

struct RestorePurchasesButton: View {
    @State private var restoring = false

    var body: some View {
        Button("Restore purchases") {
            restoring = true
            Task {
                await restore()
                await MainActor.run { restoring = false }
            }
        }
        .font(.footnote)
        .disabled(restoring)
    }

    private func restore() async {
        do {
            let found = try await BadgeStore.shared.restorePurchases()
            if !found {
                await MainActor.run {
                    showAlert(
                        NSLocalizedString("Nothing to restore", comment: "alert title"),
                        message: NSLocalizedString("No App Store purchase waiting to be added was found.", comment: "alert message")
                    )
                }
            }
        } catch let error {
            logger.error("RestorePurchasesButton: \(String(describing: error))")
            if case .userCancelled? = error as? StoreKitError { return }
            await MainActor.run {
                showAlert(NSLocalizedString("Restore failed", comment: "alert title"), message: redeemErrorText(error, purchase: true))
            }
        }
    }
}

struct BadgesCheckOrderView_Previews: PreviewProvider {
    static var previews: some View {
        NavigationView {
            BadgesCheckOrderView(level: .supporter, period: .oneMonth)
        }
    }
}
