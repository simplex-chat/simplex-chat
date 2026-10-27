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
import UIKit
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

// the page's app flag rides in the fragment, which never reaches the service
let badgePageUrl = "https://badges.simplex.chat/#/tier?app=true"
// the dev deployment, for testing store purchases: swap with the line above
// let badgePageUrl = "https://smp7.simplex.im/#/tier?app=true"

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
    let invoiceId: UUID?
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
}

enum BadgeStoreError: Error {
    case productUnavailable(productId: String)
    case unknownPurchaseResult
    case noActiveProfile
    case invalidInvoiceId(String)
    case noWindowScene
    case subscribedBefore
}

final class BadgeStore: ObservableObject {
    static let shared = BadgeStore()

    private enum LoadState { case notLoaded, loading, loaded, failed }

    @Published private var state: LoadState = .notLoaded
    private var products: [String: Product] = [:]
    // core's open store purchases for the profile they were read for: core knows whose a purchase is
    @Published private var storePurchases: (userId: Int64, purchases: [OpenStorePurchase])? = nil
    // whether a store sheet this run opened has not returned
    @Published private var buying = false
    // kept for this run only: StoreKit lists no deferred purchase, and a declined one delivers nothing
    // by invoice id, so a pending purchase shows only under the profile whose record it names
    @Published private var waitingForApproval: Set<String> = []
    // set once presentUnfinished has read the store: until then, an Ask to Buy approved while the app was
    // closed, or a purchase it died before handing over, are both unknown, so canBuy refuses to buy again
    @Published private var reconciledOnce = false
    // the original ids of the Apple ID's subscriptions; nil until load() has read them. Apple folds a new subscription
    // of an Apple ID that subscribed before into the earlier one, which core resumes only for the profile it retired
    @Published private(set) var priorSubscriptions: Set<String>? = nil
    // the sweep has no buyer waiting and runs at four triggers, so a failure that cannot clear is told once a run
    private var sweepFailureShown = false
    private var transactionUpdates: Task<Void, Never>? = nil

    private init() {}

    func purchaseState(_ userId: Int64?) -> BadgePurchaseState? {
        let purchases = openStorePurchases(userId)
        if purchases.contains(where: { $0.transactionRef != nil }) { return .issuing }
        if purchases.contains(where: { $0.invoiceId.map(waitingForApproval.contains) == true }) { return .waitingForApproval }
        return nil
    }

    func creditError(_ userId: Int64?) -> BadgeIssueFailure? {
        openStorePurchases(userId).compactMap(\.creditError).first
    }

    var checkingPurchases: Bool { !reconciledOnce }

    func canBuy(_ userId: Int64?) -> Bool {
        reconciledOnce && !buying && purchaseState(userId) == nil
    }

    func setStorePurchases(_ userId: Int64, _ purchases: [OpenStorePurchase]) {
        storePurchases = (userId, purchases)
    }

    private func openStorePurchases(_ userId: Int64?) -> [OpenStorePurchase] {
        guard let storePurchases, storePurchases.userId == userId else { return [] }
        return storePurchases.purchases
    }

    func forSale(_ level: BadgeLevel, _ period: BadgePeriod) -> Bool {
        switch period {
        case .oneMonth: true
        case .monthly, .annual: priorSubscriptions?.allSatisfy { $0 == resumableSubscription(level) } ?? false
        }
    }

    // a subscription of an Apple ID that subscribed before resumes only the active profile's newest purchase,
    // once its badge is retired and only at its own level, as the service grants a new charge only of the same tier
    private func resumableSubscription(_ level: BadgeLevel) -> String? {
        let badgeModel = BadgeModel.shared
        guard badgeModel.userId == ChatModel.shared.currentUser?.userId,
              let badgeState = badgeModel.badgeState,
              !badgeState.shown,
              badgeState.badgeType == level.badgeType
        else { return nil }
        return badgeState.subscription?.transactionRef
    }

    var subscribedBefore: Bool { priorSubscriptions?.isEmpty == false }

    func price(_ level: BadgeLevel, _ period: BadgePeriod, compact: Bool = true) -> BadgePrice {
        switch state {
        case .notLoaded, .loading: return .loading
        case .loaded, .failed:
            if let p = products[badgeProductId(level, period)] { return .price(compact ? compactPrice(p) : p.displayPrice) }
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
            let prior = await subscriptionOriginalIds()
            await MainActor.run {
                products = byId
                priorSubscriptions = prior
                state = .loaded
            }
        } catch let error {
            logger.error("BadgeStore.load: \(String(describing: error))")
            await MainActor.run { state = .failed }
        }
    }

    // A purchase has its core record before the store charges, and every store outcome reaches it.
    func purchase(_ level: BadgeLevel, _ period: BadgePeriod) async throws -> BadgePurchaseOutcome {
        let productId = badgeProductId(level, period)
        guard let product = await MainActor.run(body: { products[productId] }) else {
            throw BadgeStoreError.productUnavailable(productId: productId)
        }
        // another device on this Apple ID may have subscribed since load(), and Apple would fold this purchase into that one
        if product.type == .autoRenewable {
            let prior = await subscriptionOriginalIds()
            await MainActor.run { priorSubscriptions = priorSubscriptions?.union(prior) }
        }
        guard await MainActor.run(body: { forSale(level, period) }) else {
            throw BadgeStoreError.subscribedBefore
        }
        guard let userId = await MainActor.run(body: { ChatModel.shared.currentUser?.userId }) else {
            throw BadgeStoreError.noActiveProfile
        }
        let invoice = try await apiCreateBadgeInvoice(userId)
        guard let invoiceId = UUID(uuidString: invoice) else { throw BadgeStoreError.invalidInvoiceId(invoice) }
        await MainActor.run { buying = true }
        await loadBadgeStateAsync(userId)
        do {
            let outcome = try await storePurchase(product, invoiceId)
            if product.type == .autoRenewable {
                switch outcome {
                case let .purchased(receipt): await MainActor.run { _ = priorSubscriptions?.insert(String(receipt.transaction.originalID)) }
                // StoreKit lists no deferred purchase, so subscriptions stay shut for this run
                case .pending: await MainActor.run { priorSubscriptions = nil }
                case .cancelled: break
                }
            }
            if case let .purchased(receipt) = outcome, receipt.signatureVerified {
                try await handOver(receipt, origin: .purchased)
            }
            await MainActor.run { buying = false }
            return outcome
        } catch let error {
            await MainActor.run { buying = false }
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

    // Core holds the receipt and credits it; the transaction stays unfinished until core answers it credited
    // or refused, as an unfinished transaction is what the store re-delivers if anything is lost on the way.
    private func handOver(_ receipt: BadgeStoreReceipt, origin: StoreReceiptOrigin) async throws {
        guard let userId = await MainActor.run(body: { ChatModel.shared.currentUser?.userId }) else { return }
        // a refusal is announced from core's event for it, so only an unexpected answer reaches the buyer
        do {
            switch try await apiPurchaseBadge(userId, receipt.echoedInvoiceId, origin: origin, .apple(jws: receipt.jws)) {
            case let .held(user, badgeState, storePurchases):
                await MainActor.run {
                    // the answer is the owner's, which may be another profile and a hidden one
                    if active(user) {
                        BadgeModel.shared.set(userId: user.userId, badgeState: badgeState)
                        setStorePurchases(user.userId, storePurchases)
                    }
                }
            case let .credited(user, badgeState):
                await MainActor.run {
                    if active(user) {
                        BadgeModel.shared.set(userId: user.userId, badgeState: badgeState)
                        ChatModel.shared.updateUser(user)
                        if badgeState?.shown == true { UserDefaults.standard.set(true, forKey: DEFAULT_SUPPORTER_BANNER_SHOWN) }
                    }
                }
                await receipt.transaction.finish()
            case .notRecorded:
                // left unfinished, for the device that bought it, or for Restore to claim it here
                break
            }
        } catch let error where badgeReceiptRefused(error) {
            logger.error("BadgeStore.handOver: \(responseError(error))")
            await receipt.transaction.finish()
            // Restore found this purchase, so it says why it was refused rather than that there was nothing to restore
            if origin == .restored { throw error }
        }
    }

    func storePurchaseCredited(_ user: UserRef, _ invoiceId: String) async {
        await MainActor.run {
            // an open badges screen is the notice only for the active profile, whose badge it shows
            if !active(user) || !badgesViewShown { showBadgeAddedAlert(user) }
        }
        await storePurchaseResolved(user, invoiceId)
    }

    func storePurchaseRefused(_ user: UserRef, _ invoiceId: String, _ refusal: BadgeIssueFailure) async {
        await MainActor.run {
            if active(user) && openStorePurchases(user.userId).contains(where: { $0.invoiceId == invoiceId }) {
                showAlert(refusal.purchaseAlertTitle, message: refusal.purchaseText)
            }
        }
        await storePurchaseResolved(user, invoiceId)
    }

    private func storePurchaseResolved(_ user: UserRef, _ invoiceId: String) async {
        // the row goes before the store is read, or the screen shows it open while the store answers;
        // the transaction is finished whoever owns it, as only the app can, while the cached row is the active profile's
        await MainActor.run {
            if active(user), let open = storePurchases, open.userId == user.userId {
                storePurchases = (open.userId, open.purchases.filter { $0.invoiceId != invoiceId })
            }
        }
        for await verification in Transaction.unfinished {
            let receipt = storeReceipt(verification)
            if receipt.echoedInvoiceId == invoiceId {
                await receipt.transaction.finish()
                return
            }
        }
    }

    // at launch, on return to the foreground and on a profile switch, never on a timer
    func presentUnfinished() async {
        await listenForTransactions()
        for await verification in Transaction.unfinished {
            await reconcileListed(verification)
        }
        await loadCurrentBadgeState()
        await MainActor.run { reconciledOnce = true }
        await reportSubscriptionStates()
    }

    // transactions the store settles outside a purchase call, such as an approved Ask to Buy
    @MainActor
    private func listenForTransactions() {
        if transactionUpdates == nil {
            transactionUpdates = Task.detached {
                for await verification in Transaction.updates {
                    await BadgeStore.shared.reconcileListed(verification)
                }
            }
        }
    }

    // the store lists an Apple ID's transactions on every device signed into it, so these are handed over as listed
    private func reconcileListed(_ verification: VerificationResult<Transaction>) async {
        // the receipt stays unfinished, so the next sweep hands it over again
        do { try await reconcile(verification, origin: .listed) } catch let error {
            logger.error("BadgeStore.reconcile: \(responseError(error))")
            await MainActor.run {
                // showAlert presents on the top view controller, which the launch sweep can precede;
                // an alert with nowhere to go must not spend the one this run gets
                if !sweepFailureShown, getTopViewController() != nil {
                    sweepFailureShown = true
                    showAlert(NSLocalizedString("Purchase error", comment: "alert title"), message: redeemErrorText(error, purchase: true))
                }
            }
        }
    }

    // an unverified transaction is never sent, and left unfinished rather than lost
    private func reconcile(_ verification: VerificationResult<Transaction>, origin: StoreReceiptOrigin) async throws {
        let receipt = storeReceipt(verification)
        let t = receipt.transaction
        if t.productType == .autoRenewable { await MainActor.run { _ = priorSubscriptions?.insert(String(t.originalID)) } }
        if t.productType == .autoRenewable && t.revocationDate != nil {
            // the service refuses a revoked charge as invalid, and that would refuse the whole subscription's record
            await t.finish()
        } else if !badgeProductIds.contains(receipt.productId) {
            await t.finish()
        } else if receipt.signatureVerified {
            try await handOver(receipt, origin: origin)
        }
    }

    // The service asks the store only once the paid period ends, so a cancellation or a failed charge before
    // then is seen only here. Sent with the latest transaction, which names the subscription as its first one did.
    func reportSubscriptionStates() async {
        guard !(await subscriptionOriginalIds()).isEmpty else { return }
        await load()
        guard let subscription = await MainActor.run(body: { products.values.first { $0.type == .autoRenewable }?.subscription }) else { return }
        do {
            for status in try await subscription.status {
                guard case let .verified(renewalInfo) = status.renewalInfo, case .verified = status.transaction else { continue }
                let state: BadgeSubscriptionState? = switch status.state {
                case .subscribed: renewalInfo.willAutoRenew ? .renewing : .cancelled
                case .inBillingRetryPeriod, .inGracePeriod: .billingRetry
                case .expired, .revoked: .ended
                default: nil
                }
                if let state {
                    try await apiSetBadgeSubscriptionStatus(state, .apple(jws: status.transaction.jwsRepresentation))
                }
            }
        } catch let error {
            logger.error("BadgeStore.reportSubscriptionStates: \(responseError(error))")
        }
    }

    func manageSubscriptions() async {
        let scene = await MainActor.run {
            UIApplication.shared.connectedScenes
                .compactMap { $0 as? UIWindowScene }
                .first { $0.windows.contains(where: \.isKeyWindow) }
        }
        do {
            guard let scene else { throw BadgeStoreError.noWindowScene }
            try await AppStore.showManageSubscriptions(in: scene)
        } catch let error {
            logger.error("BadgeStore.manageSubscriptions: \(String(describing: error))")
            if let url = URL(string: "https://apps.apple.com/account/subscriptions") {
                await MainActor.run { UIApplication.shared.open(url) }
            }
        }
        await reportSubscriptionStates()
        await loadCurrentBadgeState()
    }

    // Hands over as restored whatever the store lists, so this profile claims it, also from another install or profile.
    // Answers whether a badge is shown, or a purchase is being added or awaits approval, afterwards.
    func restorePurchases() async throws -> Bool {
        try await AppStore.sync()
        var handOverError: Error? = nil
        var handedOver: Set<UInt64> = []
        for transactions in [Transaction.currentEntitlements, Transaction.unfinished] {
            for await verification in transactions {
                // a repeat would be refused once the first hand-over's badge is shown
                guard handedOver.insert(verification.unsafePayloadValue.id).inserted else { continue }
                do { try await reconcile(verification, origin: .restored) } catch let error {
                    logger.error("BadgeStore.restorePurchases: \(responseError(error))")
                    if handOverError == nil { handOverError = error }
                }
            }
        }
        await reportSubscriptionStates()
        await loadCurrentBadgeState()
        let restored: Bool = await MainActor.run {
            let userId = ChatModel.shared.currentUser?.userId
            let badgeShown = BadgeModel.shared.userId == userId && BadgeModel.shared.badgeState?.shown == true
            return badgeShown || purchaseState(userId) != nil
        }
        if !restored, let handOverError { throw handOverError }
        return restored
    }

    func subscriptionPrice(_ badgeType: BadgeType, planMonths: Int) -> String? {
        let period: BadgePeriod? = switch planMonths {
        case 1: .monthly
        case 12: .annual
        default: nil
        }
        guard let period, let level = BadgeLevel.allCases.first(where: { $0.badgeType == badgeType }) else { return nil }
        return products[badgeProductId(level, period)]?.displayPrice
    }

    // an unverified transaction counts too: Apple, not this check, decides what a new subscription folds into
    private func subscriptionOriginalIds() async -> Set<String> {
        var ids: Set<String> = []
        for await verification in Transaction.all {
            let t = verification.unsafePayloadValue
            if t.productType == .autoRenewable { ids.insert(String(t.originalID)) }
        }
        return ids
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
    return BadgeStoreReceipt(
        jws: verification.jwsRepresentation,
        productId: t.productID,
        invoiceId: t.appAccountToken,
        signatureVerified: signatureVerified,
        transaction: t
    )
}

// the codes core refuses a receipt with for good, after which the store may stop re-delivering it;
// a sweep that hands over a receipt refused while the app was not running learns it only from this answer
private func badgeReceiptRefused(_ error: Error) -> Bool {
    let code = badgeServiceErrorCode(error)
    return code == .receiptInvalid || code == .receiptUsed || code == .testPurchase || code == .transferLimited
}

private func badgeServiceErrorCode(_ error: Error) -> BadgeServiceErrorCode? {
    if case let .error(.badgeRedeemError(.serviceError(code))) = error as? ChatError { return code }
    return nil
}
