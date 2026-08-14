//
//  BadgeStore.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 14.08.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import Foundation
import Combine
import StoreKit

// TODO [badges] product ids will come from app config and prices from the badge service catalog;
// hardcoded here so the App Store integration can be tested before the purchase API lands.
func badgeProductId(_ level: BadgeLevel, _ period: BadgePeriod) -> String {
    switch (level, period) {
    case (.supporter, .oneMonth): "BADGE_SUPPORTER_01"
    case (.supporter, .monthly): "SUBSCR_BADGE_SUPPORTER_MONTH_01"
    case (.supporter, .annual): "SUBSCR_BADGE_SUPPORTER_YEAR_01"
    case (.legend, .oneMonth): "BADGE_LEGEND_01"
    case (.legend, .monthly): "SUBSCR_BADGE_LEGEND_MONTH_01"
    case (.legend, .annual): "SUBSCR_BADGE_LEGEND_YEAR_01"
    }
}

let badgeProductIds: [String] = BadgeLevel.allCases.flatMap { level in
    BadgePeriod.allCases.map { badgeProductId(level, $0) }
}

// TODO [badges] replaced by APIGetBadgeInvoice, which creates the invoice row and returns its id.
// Apple requires a UUID - it is sent as appAccountToken and echoed back in the signed transaction,
// which is how the service learns which invoice a store transaction settles.
func newBadgeInvoiceId() -> UUID { UUID() }

enum BadgePrice {
    case loading
    case price(String)
    case unavailable

    var canPurchase: Bool {
        switch self {
        case .price: true
        case .loading, .unavailable: false
        }
    }
}

struct BadgeStoreReceipt {
    // the signed token the badge service verifies - never transaction.jsonRepresentation
    let jws: String
    let productId: String
    let transactionId: UInt64
    let invoiceId: UUID?
    let environment: String?
    let signatureVerified: Bool
}

enum BadgePurchaseOutcome {
    case purchased(BadgeStoreReceipt)
    case pending
    case cancelled
}

enum BadgeStoreError: Error {
    case productUnavailable(productId: String)
    case unknownPurchaseResult
}

final class BadgeStore: ObservableObject {
    static let shared = BadgeStore()

    private enum LoadState { case notLoaded, loading, loaded, failed }

    @Published private var state: LoadState = .notLoaded
    private var products: [String: Product] = [:]

    private init() {}

    func price(_ level: BadgeLevel, _ period: BadgePeriod) -> BadgePrice {
        switch state {
        case .notLoaded, .loading: return .loading
        case .loaded, .failed:
            if let p = products[badgeProductId(level, period)] { return .price(compactPrice(p)) }
            return .unavailable
        }
    }

    // percentage the annual subscription saves against 12 monthly payments
    func annualSavings(_ level: BadgeLevel) -> Int? {
        guard let monthly = products[badgeProductId(level, .monthly)],
              let annual = products[badgeProductId(level, .annual)]
        else { return nil }
        let year = monthly.price * 12
        guard year > 0, annual.price < year else { return nil }
        let saved = (year - annual.price) / year * 100
        let percent = Int(NSDecimalNumber(decimal: saved).doubleValue.rounded())
        return percent > 0 ? percent : nil
    }

    func load() async {
        guard await startLoading() else { return }
        do {
            let loaded = try await Product.products(for: badgeProductIds)
            let byId = Dictionary(loaded.map { ($0.id, $0) }, uniquingKeysWith: { p, _ in p })
            let missing = badgeProductIds.filter { byId[$0] == nil }
            if !missing.isEmpty {
                logger.warning("BadgeStore.load: no product returned for \(missing.joined(separator: ", "))")
            }
            await MainActor.run {
                products = byId
                state = .loaded
            }
        } catch let error {
            logger.error("BadgeStore.load: \(String(describing: error))")
            await MainActor.run { state = .failed }
        }
    }

    func purchase(_ level: BadgeLevel, _ period: BadgePeriod, invoiceId: UUID) async throws -> BadgePurchaseOutcome {
        let productId = badgeProductId(level, period)
        guard let product = await MainActor.run(body: { products[productId] }) else {
            throw BadgeStoreError.productUnavailable(productId: productId)
        }
        switch try await product.purchase(options: [.appAccountToken(invoiceId)]) {
        case let .success(verification):
            let transaction: Transaction
            let signatureVerified: Bool
            switch verification {
            case let .verified(t):
                transaction = t
                signatureVerified = true
            case let .unverified(t, _):
                transaction = t
                signatureVerified = false
            }
            // nothing is delivered in this build, so the transaction is finished right away; once the
            // service issues credentials it must only be finished after the credential is stored
            await transaction.finish()
            return .purchased(storeReceipt(verification.jwsRepresentation, transaction, signatureVerified))
        case .pending: return .pending
        case .userCancelled: return .cancelled
        @unknown default: throw BadgeStoreError.unknownPurchaseResult
        }
    }

    @MainActor
    private func startLoading() -> Bool {
        switch state {
        case .notLoaded, .failed:
            state = .loading
            return true
        case .loading, .loaded:
            return false
        }
    }
}

// drops the fraction from whole amounts ("$7", not "$7.00") in the product's own currency style;
// Product.displayPrice remains the exact form for views that need the cents
private func compactPrice(_ product: Product) -> String {
    var whole = Decimal()
    var price = product.price
    NSDecimalRound(&whole, &price, 0, .plain)
    return whole == product.price
        ? product.price.formatted(product.priceFormatStyle.precision(.fractionLength(0)))
        : product.displayPrice
}

private func storeReceipt(_ jws: String, _ t: Transaction, _ signatureVerified: Bool) -> BadgeStoreReceipt {
    var environment: String? = nil
    if #available(iOS 16.0, *) { environment = t.environment.rawValue }
    return BadgeStoreReceipt(
        jws: jws,
        productId: t.productID,
        transactionId: t.id,
        invoiceId: t.appAccountToken,
        environment: environment,
        signatureVerified: signatureVerified
    )
}
