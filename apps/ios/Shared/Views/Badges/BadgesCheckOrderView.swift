//
//  BadgesCheckOrderView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 29.09.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct BadgesCheckOrderView: View {
    @EnvironmentObject var theme: AppTheme
    @EnvironmentObject var chatModel: ChatModel
    @ObservedObject private var store = BadgeStore.shared
    let level: BadgeLevel
    let period: BadgePeriod
    @State private var purchasing = false
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

                    VStack(spacing: 12) {
                        orderRow("Your badge", Text(level.title))
                        orderRow("Duration", Text(period.label))
                        orderRow("Total", period.priceText(store.price(level, period)))
                    }
                    .padding(.top, 20)

                    Text("Charged by the App Store to the account on this device.")
                        .font(.footnote)
                        .foregroundColor(theme.colors.secondary)
                        .multilineTextAlignment(.center)
                        .fixedSize(horizontal: false, vertical: true)

                    Spacer(minLength: 20)

                    VStack(spacing: 10) {
                        payButton()
                            .padding(.vertical, 10)
                        BadgeBillingFooter(period: period)
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

    private func orderRow(_ title: LocalizedStringKey, _ value: Text) -> some View {
        HStack {
            Text(title)
                .foregroundColor(theme.colors.secondary)
            Spacer()
            value
        }
        .font(.body)
    }

    private func payButton() -> some View {
        let price = store.price(level, period)
        let disabled = !price.canPurchase || purchasing || !store.canBuy(chatModel.currentUser?.userId)
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
                let (outcome, invoiceId) = try await store.purchase(level, period)
                if case let .purchased(receipt) = outcome {
                    if !badgeOneTimeProductIds.contains(receipt.productId) {
                        await MainActor.run { showPurchasedAlert(receipt, invoiceId) }
                    } else if !receipt.signatureVerified {
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
                }
                await MainActor.run { purchasing = false }
            } catch let error {
                logger.error("BadgesCheckOrderView.purchase: \(String(describing: error))")
                await MainActor.run {
                    purchasing = false
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

    // TODO [badges] store integration diagnostics - replaced by the issued badge once subscriptions are delivered.
    private func showPurchasedAlert(_ receipt: BadgeStoreReceipt, _ invoiceId: UUID) {
        let returnedInvoice: String
        if let returned = receipt.invoiceId {
            returnedInvoice = returned == invoiceId ? "yes" : "mismatch: \(returned.uuidString)"
        } else {
            returnedInvoice = "none"
        }
        var lines = [
            "Product: \(receipt.productId)",
            "Invoice: \(invoiceId.uuidString)",
            "Invoice returned by Apple: \(returnedInvoice)",
            "Transaction: \(receipt.transactionId)"
        ]
        if let environment = receipt.environment { lines.append("Environment: \(environment)") }
        lines.append("Signature: \(receipt.signatureVerified ? "verified" : "unverified")")
        let summary = lines.joined(separator: "\n")
        // logged as well as shown: the alert races StoreKit's own sheets, the log always lands
        logger.debug("badge purchase succeeded\n\(summary)")
        alert = SomeAlert(
            alert: Alert(
                title: Text("Purchase successful"),
                message: Text(verbatim: summary)
            ),
            id: "badgePurchased"
        )
    }
}

struct BadgesCheckOrderView_Previews: PreviewProvider {
    static var previews: some View {
        NavigationView {
            BadgesCheckOrderView(level: .supporter, period: .oneMonth)
        }
    }
}
