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
import SimpleXChat

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

// the only products sent to the badge service: nothing delivers a subscription yet
let badgeOneTimeProductIds: Set<String> = Set(BadgeLevel.allCases.map { badgeProductId($0, .oneMonth) })

// TODO [badges] replaced by APIGetBadgeInvoice, which creates the invoice row and returns its id.
// Apple requires a UUID - it is sent as appAccountToken and echoed back in the signed transaction,
// which is how the service learns which invoice a store transaction settles.
func newBadgeInvoiceId() -> UUID { UUID() }

// the page's app flag rides in the fragment, which never reaches the service
let badgePageUrl = "https://badges.simplex.chat/#/tier?app=true"

// where the store allows a link out to the badge page: the US for now, a set expected to widen.
// An unknown storefront does not count, as this decides whether the store sees a link out of the app.
var badgeBrowserAllowed: Bool {
    SKPaymentQueue.default().storefront?.countryCode == "USA"
}

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
    let transaction: Transaction
}

enum BadgePurchaseOutcome {
    case purchased(BadgeStoreReceipt)
    case pending
    case cancelled
}

enum BadgePurchaseState {
    case issuing
    case waitingForApproval
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
    // one-time transactions the store holds unfinished: each is a payment taken and not yet credited
    @Published private var unfinished: Set<UInt64> = []
    // kept for this run only: StoreKit lists no deferred purchase, and a declined one delivers nothing
    @Published private var waitingForApproval = false
    private var presenting: Set<UInt64> = []
    private var transactionUpdates: Task<Void, Never>? = nil

    private init() {}

    var purchaseState: BadgePurchaseState? {
        if !unfinished.isEmpty { return .issuing }
        if waitingForApproval { return .waitingForApproval }
        return nil
    }

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
                // the store drops ids it cannot resolve without saying why, so the storefront and
                // the bundle the request was made for are logged with them
                let storefront = await Storefront.current
                logger.warning("BadgeStore.load: no product returned for \(missing.joined(separator: ", ")) - bundle \(Bundle.main.bundleIdentifier ?? "?"), storefront \(storefront?.countryCode ?? "none"), canMakePayments \(AppStore.canMakePayments)")
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
            let receipt = storeReceipt(verification)
            // a one-time transaction is finished only once the service answers for it, as an unfinished one
            // is what the store re-delivers; nothing delivers a subscription, so nothing would finish it later
            if !badgeOneTimeProductIds.contains(receipt.productId) { await receipt.transaction.finish() }
            return .purchased(receipt)
        case .pending:
            await MainActor.run { waitingForApproval = true }
            return .pending
        case .userCancelled: return .cancelled
        @unknown default: throw BadgeStoreError.unknownPurchaseResult
        }
    }

    // only the purchase the user started may alert: an answer can reveal a profile other than the one on screen
    func presentPurchase(_ receipt: BadgeStoreReceipt, interactive: Bool) async {
        guard let userId = await MainActor.run(body: { ChatModel.shared.currentUser?.userId }),
              await claim(receipt.transactionId)
        else { return }
        do {
            switch try await apiPurchaseBadge(userId, .apple(jws: receipt.jws), retry: interactive) {
            case let .redeemed(user, badgeState):
                await MainActor.run {
                    BadgeModel.shared.set(userId: user.userId, badgeState: badgeState)
                    ChatModel.shared.updateUser(user)
                    if badgeState?.shown == true { UserDefaults.standard.set(true, forKey: DEFAULT_SUPPORTER_BANNER_SHOWN) }
                }
                await finish(receipt)
            case .deliveredToOtherProfile:
                await finish(receipt)
            case nil:
                break
            }
        } catch let error {
            logger.error("BadgeStore.presentPurchase: \(responseError(error))")
            if badgeReceiptRefused(error) {
                await finish(receipt)
                if interactive {
                    await MainActor.run {
                        showAlert(NSLocalizedString("Purchase error", comment: "alert title"), message: redeemErrorText(error))
                    }
                }
            }
        }
        await MainActor.run { _ = presenting.remove(receipt.transactionId) }
    }

    // at launch and on return to the foreground, never on a timer
    func presentUnfinished() async {
        await listenForTransactions()
        for await verification in Transaction.unfinished {
            await reconcile(verification)
        }
    }

    // transactions the store settles outside a purchase call, such as an approved Ask to Buy
    @MainActor
    private func listenForTransactions() {
        if transactionUpdates == nil {
            transactionUpdates = Task.detached {
                for await verification in Transaction.updates {
                    await BadgeStore.shared.reconcile(verification)
                }
            }
        }
    }

    // an unverified one-time transaction is never sent, and left unfinished rather than lost
    private func reconcile(_ verification: VerificationResult<Transaction>) async {
        let receipt = storeReceipt(verification)
        if !badgeOneTimeProductIds.contains(receipt.productId) {
            await receipt.transaction.finish()
        } else if receipt.signatureVerified {
            await presentPurchase(receipt, interactive: false)
        }
    }

    // one request per transaction: the purchase itself, launch, foreground and the store can each present it
    @MainActor
    private func claim(_ transactionId: UInt64) -> Bool {
        guard presenting.insert(transactionId).inserted else { return false }
        unfinished.insert(transactionId)
        waitingForApproval = false
        return true
    }

    private func finish(_ receipt: BadgeStoreReceipt) async {
        await receipt.transaction.finish()
        await MainActor.run { _ = unfinished.remove(receipt.transactionId) }
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

private func storeReceipt(_ verification: VerificationResult<Transaction>) -> BadgeStoreReceipt {
    let (t, signatureVerified) = switch verification {
    case let .verified(t): (t, true)
    case let .unverified(t, _): (t, false)
    }
    var environment: String? = nil
    if #available(iOS 16.0, *) { environment = t.environment.rawValue }
    return BadgeStoreReceipt(
        jws: verification.jwsRepresentation,
        productId: t.productID,
        transactionId: t.id,
        invoiceId: t.appAccountToken,
        environment: environment,
        signatureVerified: signatureVerified,
        transaction: t
    )
}

// the only answers after which the receipt will never be credited, so the store may stop re-delivering it
private func badgeReceiptRefused(_ error: Error) -> Bool {
    if case let .error(.badgeRedeemError(.serviceError(code))) = error as? ChatError {
        return code == .receiptInvalid || code == .receiptUsed
    }
    return false
}
