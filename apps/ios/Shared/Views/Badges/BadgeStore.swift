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

// subscriptions are for sale once renewals are delivered
let badgePeriodsForSale: [BadgePeriod] = [.oneMonth]

// A subscription's id only: core mints a one-time purchase's, and a subscription is never sent to core.
// Apple requires a UUID - it is sent as appAccountToken and echoed back in the signed transaction.
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

    var echoedInvoiceId: String? { invoiceId.map(coreInvoiceId) }
}

// core mints the id in lower case, and UUID formats it in upper case
func coreInvoiceId(_ invoiceId: UUID) -> String { invoiceId.uuidString.lowercased() }

enum BadgePurchaseOutcome {
    case purchased(BadgeStoreReceipt)
    case pending(invoiceId: String?)
    case cancelled
}

enum BadgePurchaseState {
    case issuing
    case waitingForApproval
    case checking
}

enum BadgeStoreError: Error {
    case productUnavailable(productId: String)
    case unknownPurchaseResult
    case noActiveProfile
    case invalidInvoiceId(String)
}

final class BadgeStore: ObservableObject {
    static let shared = BadgeStore()

    private enum LoadState { case notLoaded, loading, loaded, failed }

    @Published private var state: LoadState = .notLoaded
    private var products: [String: Product] = [:]
    // one-time transactions the store holds unfinished: each is a payment taken and not yet credited
    @Published private var unfinished: [UInt64: BadgeStoreReceipt] = [:]
    // core's open store purchases for the profile they were read for: core knows whose a purchase is
    @Published private var storePurchases: (userId: Int64, purchases: [OpenStorePurchase])? = nil
    // invoices whose purchase this session is still waiting on the store for
    @Published private var buying: Set<String> = []
    // kept for this run only: StoreKit lists no deferred purchase, and a declined one delivers nothing
    // by invoice id, so a pending purchase shows only under the profile whose record it names
    @Published private var waitingForApproval: Set<String> = []
    // until the store has been asked once, a purchase made while the app was not running is unknown
    @Published private var reconciledOnce = false
    // whether the purchase the user started waits on the presentation, so its outcome is shown whoever claimed it
    private var presenting: [UInt64: Bool] = [:]
    private var transactionUpdates: Task<Void, Never>? = nil

    private init() {}

    func purchaseState(_ userId: Int64?) -> BadgePurchaseState? {
        let purchases = openStorePurchases(userId)
        let held = Set(unfinished.values.compactMap { $0.echoedInvoiceId })
        if purchases.contains(where: { $0.invoiceId.map(held.contains) == true }) { return .issuing }
        if purchases.contains(where: { $0.invoiceId.map(waitingForApproval.contains) == true }) { return .waitingForApproval }
        if purchases.contains(where: { $0.transactionRef == nil && $0.invoiceId.map(buying.contains) != true }) { return .checking }
        return nil
    }

    func canBuy(_ userId: Int64?) -> Bool {
        reconciledOnce && buying.isEmpty && purchaseState(userId) == nil
    }

    func setStorePurchases(_ userId: Int64, _ purchases: [OpenStorePurchase]) {
        storePurchases = (userId, purchases)
    }

    private func openStorePurchases(_ userId: Int64?) -> [OpenStorePurchase] {
        guard let storePurchases, storePurchases.userId == userId else { return [] }
        return storePurchases.purchases
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

    // A one-time purchase has its core record before the store charges, and every store outcome reaches it.
    func purchase(_ level: BadgeLevel, _ period: BadgePeriod) async throws -> (outcome: BadgePurchaseOutcome, invoiceId: UUID) {
        let productId = badgeProductId(level, period)
        guard let product = await MainActor.run(body: { products[productId] }) else {
            throw BadgeStoreError.productUnavailable(productId: productId)
        }
        // a subscription is never sent to core, so nothing would ever finish it later
        guard badgeOneTimeProductIds.contains(productId) else {
            let invoiceId = newBadgeInvoiceId()
            let outcome = try await storePurchase(product, invoiceId)
            if case let .purchased(receipt) = outcome { await receipt.transaction.finish() }
            return (outcome, invoiceId)
        }
        guard let userId = await MainActor.run(body: { ChatModel.shared.currentUser?.userId }) else {
            throw BadgeStoreError.noActiveProfile
        }
        let invoice = try await apiCreateBadgeInvoice(userId)
        guard let invoiceId = UUID(uuidString: invoice) else { throw BadgeStoreError.invalidInvoiceId(invoice) }
        await MainActor.run { _ = buying.insert(invoice) }
        await loadBadgeStateAsync(userId)
        do {
            let outcome = try await storePurchase(product, invoiceId)
            switch outcome {
            // finished only once the service answers for it, as an unfinished transaction is what the store re-delivers
            case let .purchased(receipt) where receipt.signatureVerified: await presentPurchase(receipt, interactive: true)
            case .purchased, .cancelled: await closeInvoice(userId, invoice)
            case .pending: break
            }
            await MainActor.run { _ = buying.remove(invoice) }
            return (outcome, invoiceId)
        } catch let error {
            await closeInvoice(userId, invoice)
            await MainActor.run { _ = buying.remove(invoice) }
            throw error
        }
    }

    private func storePurchase(_ product: Product, _ invoiceId: UUID) async throws -> BadgePurchaseOutcome {
        switch try await product.purchase(options: [.appAccountToken(invoiceId)]) {
        case let .success(verification): return .purchased(storeReceipt(verification))
        case .pending:
            let pending = coreInvoiceId(invoiceId)
            await MainActor.run { _ = waitingForApproval.insert(pending) }
            return .pending(invoiceId: pending)
        case .userCancelled: return .cancelled
        @unknown default: throw BadgeStoreError.unknownPurchaseResult
        }
    }

    // only the purchase the user started may alert: an answer can reveal a profile other than the one on screen
    private func presentPurchase(_ receipt: BadgeStoreReceipt, interactive: Bool) async {
        guard let userId = await MainActor.run(body: { ChatModel.shared.currentUser?.userId }),
              await claim(receipt, interactive: interactive)
        else { return }
        var alertText: String? = nil
        do {
            switch try await apiPurchaseBadge(userId, receipt.echoedInvoiceId, .apple(jws: receipt.jws), retry: interactive) {
            case let .redeemed(user, badgeState):
                await MainActor.run {
                    // finished below whichever profile was credited, as it is paid for; only the profile on screen shows it
                    if active(user) {
                        BadgeModel.shared.set(userId: user.userId, badgeState: badgeState)
                        ChatModel.shared.updateUser(user)
                        if badgeState?.shown == true { UserDefaults.standard.set(true, forKey: DEFAULT_SUPPORTER_BANNER_SHOWN) }
                    }
                }
                await finish(receipt)
            case nil:
                break
            }
        } catch let error {
            logger.error("BadgeStore.presentPurchase: \(responseError(error))")
            let refused = badgeReceiptRefused(error)
            if refused { await finish(receipt) }
            let text = redeemErrorText(error)
            alertText = refused ? text : text + "\n\n" + NSLocalizedString("The purchase will be retried, and the badge will arrive.", comment: "alert message")
        }
        await loadCurrentBadgeState()
        let userWaiting = await MainActor.run { presenting.removeValue(forKey: receipt.transactionId) == true }
        if userWaiting, let alertText {
            await MainActor.run { showAlert(NSLocalizedString("Purchase error", comment: "alert title"), message: alertText) }
        }
    }

    // at launch and on return to the foreground, never on a timer
    func presentUnfinished() async {
        await listenForTransactions()
        var held: Set<String> = []
        for await verification in Transaction.unfinished {
            if let invoiceId = storeReceipt(verification).echoedInvoiceId { held.insert(invoiceId) }
            await reconcile(verification)
        }
        await closeAbandoned(held)
        await MainActor.run { reconciledOnce = true }
    }

    // A record no held transaction names and no purchase here waits on. If the store still charges for one,
    // as when this runs between an invoice's creation and its purchase starting, the receipt reopens it.
    private func closeAbandoned(_ held: Set<String>) async {
        guard let userId = await MainActor.run(body: { ChatModel.shared.currentUser?.userId }) else { return }
        await loadBadgeStateAsync(userId)
        let abandoned = await MainActor.run {
            openStorePurchases(userId)
                .compactMap { $0.transactionRef == nil ? $0.invoiceId : nil }
                .filter { !held.contains($0) && !buying.contains($0) && !waitingForApproval.contains($0) }
        }
        for invoiceId in abandoned {
            await closeInvoice(userId, invoiceId)
        }
    }

    private func closeInvoice(_ userId: Int64, _ invoiceId: String) async {
        do {
            try await apiCloseBadgeInvoice(userId, invoiceId)
        } catch let error {
            logger.error("BadgeStore.closeInvoice: \(responseError(error))")
        }
        await loadCurrentBadgeState()
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
        } else if let invoiceId = receipt.echoedInvoiceId,
                  let userId = await MainActor.run(body: { ChatModel.shared.currentUser?.userId }) {
            await closeInvoice(userId, invoiceId)
        }
    }

    // one request per transaction: the purchase itself, launch, foreground and the store can each present it
    @MainActor
    private func claim(_ receipt: BadgeStoreReceipt, interactive: Bool) -> Bool {
        if let waiting = presenting[receipt.transactionId] {
            presenting[receipt.transactionId] = waiting || interactive
            return false
        }
        presenting[receipt.transactionId] = interactive
        unfinished[receipt.transactionId] = receipt
        // an approved Ask to Buy arrives as a transaction, which ends the wait
        if let invoiceId = receipt.echoedInvoiceId { waitingForApproval.remove(invoiceId) }
        return true
    }

    private func finish(_ receipt: BadgeStoreReceipt) async {
        await receipt.transaction.finish()
        await MainActor.run { unfinished[receipt.transactionId] = nil }
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
