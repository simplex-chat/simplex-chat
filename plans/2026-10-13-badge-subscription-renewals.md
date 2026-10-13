# Plan: store subscription renewals for supporter badges

Run: 001-badge-subscriptions · Base branch: `f/badge-subscriptions`, cut from `badge-in-app-checkout` once `f/badge-store-verifiers` is merged into it · Repos: simplex-chat-2 · Estimated diff: ~1700 changed lines (~900 code and docs, ~800 tests), one PR in eight slices

This run makes App Store and Google Play subscriptions renew badges: in the badge service, including its real connections to Apple's App Store Server API and Google Play's subscriptions API, and in the shared app core. It changes no iOS or Android screens, so nothing here puts a subscription on sale.

Sources: the draft model `plans/2026-07-30-supporter-badges-v3-ux.md` (§3 charges and O2, §5, §7), `plans/2026-08-04-badges-mvp-scope.md` milestone 4, and `docs/protocol/badges-rpc.md`.

Code is named by file and function, never by line, because `f/badge-store-verifiers` lands first and moves lines. That branch's store verifiers are the starting point:

- Apple JWS verification, offline and pinned to Apple's root: `verifyAppleTransaction` in `BadgeService/StoreReceipts/Apple.hs`.
- Play reads and acknowledgement of one-time products, with a cached service-account token: `playStoreVerifier`, `playCall`, `bearerToken` in `BadgeService/StoreReceipts/Google.hs`.
- Store config: `AppleStoreConfig`, `PlayStoreConfig` in `BadgeService/Config.hs`.
- Refusal classes, including `SRTestPurchase` answered `test_purchase`: `StoreRefusal` in `BadgeService/StoreReceipts.hs`.

Service paths below are under `apps/simplex-badge-service/src/`; core paths under `src/Simplex/Chat/`.

## 1. Goal and acceptance criteria

A badge bought as a store subscription keeps renewing. When its paid months run out, the app sends its usual signed issue request at the usual weekly moment. The service asks the store whether the subscription charged again, grants the charge's months dated to the store's billing period, and returns the next credential. The app keeps the badge until the service says the subscription has ended.

Renewal mechanics, tested with an in-process fake store:

- AC1: A verified subscription receipt creates a purchase whose ledger is one `charge` credit dated to the store's period start and one badge debit, records one charge row, issues the first month, and answers with `subscription = renewing`. A test purchase is answered `test_purchase`. Test: `testSubscriptionPurchase`.
- AC2: At zero months a subscription purchase is still asked about, a day before its shown credential expires. The badge stays shown past paidThrough. A new store charge is granted at its period start, and the next credential is issued and presented when the old one expires, so neither the user nor their contacts see a gap. Test: `testSubscriptionRenews`.
- AC3: When the store has not charged yet (billing retry), the service answers with no credential and `subscription = billingRetry` and writes no ledger row; the client records no failure, keeps the badge and asks again a day later. Once the charge lands, its month is issued with coverage from the store's period start. Test: `testSubscriptionAwaitsCharge`.
- AC4: When the service answers `subscription = ended` at or after paidThrough, the client retires the badge, raises the support-ended alert and sends no further requests for that purchase. Test: `testSubscriptionEnds`.
- AC5: Each store charge is granted at most once. A repeated issue request inside the issued period returns the same credential without asking the store, and a store reporting the same charge again adds no second grant and no second charge row. Test: `testSubscriptionChargeGrantedOnce`.
- AC6: The store is asked only when a subscription purchase's paid coverage has ended, and at most once per subscription per check interval (one hour), whether or not the store answered. Requests in between are answered from the stored state. Test: `testSubscriptionStoreChecksBounded`.
- AC7: On a renewal check, a revoked charge, a charge for another tier, a charge in a trial or introductory offer, and one whose period has not started or has already ended are recorded and not granted. Test: `testSubscriptionChargeNotGranted`.
- AC8: An unreachable store during a renewal check answers `provider_unavailable` with `retryAfter` of the check interval, stamps the check time, and writes no charge or ledger row. Test: `testSubscriptionStoreUnreachable`.
- AC9: A first purchase with no grantable charge is answered per §4's purchase rule and writes nothing. Test: `testSubscriptionPurchaseNotGrantable`.
- AC10: Every row the charge planner produces verifies on the client: `balanceChecked` gives `Just True` for the charge credit, the lapses and the badge debit. Cases: a first charge, a monthly renewal, an annual renewal with a month left at the period start, a renewal after billing-retry recovery, an annual subscriber away past the year end whose charge recovers inside the store's grace period, and a period start equal to the balance start. Test: `testChargeRowsVerify` (unit).
- AC11: A ledger holding a charge row is stored and read back by the service, and `entryTypeFromColumns` rebuilds `SCCharge` from its charge id. Tests: `testEntryTypeColumns`, the service schema down/up cycle.
- AC12: An Apple auto-renewable transaction is claimed by its `originalTransactionId`, every other Apple transaction by its `transactionId`, and the real Apple verifier vouches with the same ref. A renewal transaction handed over by the app resolves to the credited receipt record and creates no purchase; a held record that adopts a renewal JWS before it is credited is still credited. Tests: `testAppleTransactionRef` (unit), an Apple renewal-payload case in `StoreVerifierTests`, `testAppleRenewalResolvesToPurchase`.
- AC13: The `subscription` response field round-trips in JSON, is absent for prepaid purchases, and an unknown state decodes as `SSUnknown` rather than failing the response. Test: credential response JSON tests (unit).

Store connections, tested against fake HTTP stores:

- AC14: The Apple subscription check signs an App Store Server API token with the configured in-app purchase key, reads the subscription's status by its original transaction id, verifies the signed transaction and renewal info against the pinned Apple root, and maps them to a `SubscriptionCheck`. Its HTTP answers map to refusal classes as §4 says. Test: Apple subscription cases in `StoreVerifierTests`.
- AC15: The Play subscription check reads `purchases.subscriptionsv2` by token through the existing service-account client, maps state, plan, period and order id, and acknowledges an unacknowledged subscription before it is credited. Test: Play subscription cases in `StoreVerifierTests`.
- AC16: A service configured without the Apple API key, or without Play, answers that store's subscription receipts `provider_not_configured`, and its one-time purchases behave as before. Test: `ConfigTests` and a service test.

Unchanged:

- AC17: Prepaid purchases (codes and one-time store purchases) behave exactly as before. The existing "Supporter badges", "SimpleX badge service" and "SimpleX badge service e2e" suites pass, except two tests that pin `subscr_badge_supporter_01` as `product_unavailable`: `testStoreUnknownProduct`, whose subscription case moves to AC1, and `testStoreAcknowledgesBeforeCrediting`, whose unpriced case switches to a product no catalog maps (for example `badge_unknown_01`).

## 2. Non-goals

- Tier changes (upgrades and downgrades): a charge for another tier is recorded and not granted.
- Refunds: no refund handling or debit. A charge the store reports as revoked is simply not granted.
- Store notifications (App Store Server Notifications, Play real-time notifications).
- Resubscription after the badge was removed, and restoring on a new device.
- Spreading the weekly request moment, and taking store calls off the serial request loop.
- App screens, and handing subscription transactions over from the apps; the dev mock verifier's subscription support comes with them.
- Family Sharing, which stays off: family-shared Apple transactions are refused.

## 3. Current state

What the code does on `badge-in-app-checkout` with `f/badge-store-verifiers` merged:

- **The client stops at zero months.** `badgeRequestDue` (`Library/Commands.hs`) requires months left and a shown credential. `retireExpiredBadge` takes the badge off at paidThrough, after which the purchase is never asked about. Both rules rest on one premise: no months arrive after paidThrough. Subscriptions remove that premise, so both change together.
- **The service never reads a store after the first purchase and never grants onto an existing ledger.** `issueBadgeCmd` (`BadgeService/Service.hs`) lapses and issues. `signFirstMonth` seeds an empty ledger at `now` and issues without lapsing. `writeIssued` answers through `issueResponse`, which returns the current issuance, even when it skipped its write.
- **A charge row cannot be stored or read by the service.** `appendLedgerPlan` (`BadgeService/Store.hs`) writes no charge reference. `entryTypeFromColumns` (`Badges/Ledger.hs`) has no charge case, so a charge row reads as an empty ledger at the tip and fails any statement read. No charges table and no ledger `charge_id` exist on either side.
- **Apple renewals are not tied to their purchase.** Claims use `appleTransactionId` (`PaymentService.hs`), the transaction's own id, both in the client's `storeTransactionRef` and in the service's `toStoreReceipt`. A renewal has a new id, so the client's `holdStoreReceipt` (`Store/Badges.hs`) would give it a new receipt row with fresh keys. A Play renewal keeps its token and already resolves to the credited row.
- **The catalog sells no subscription.** `storeProduct` (`BadgeService/Catalog.hs`) maps only the four one-time SKUs; `testStoreUnknownProduct` pins `subscr_badge_supporter_01` as `product_unavailable`.
- **The verifiers read one-time purchases only.** Apple's is offline and parses no `originalTransactionId`, `expiresDate`, `type` or offer fields; it calls no App Store Server API. Google's reads `purchases/products` and acknowledges products only.
- **The client's statement check** (`balanceChecked`, `Badges/Ledger.hs`) re-runs each credit through `grantEntry` at its `createdAt` and flags a row dated before its predecessor. Its verdict is stored in `balance_checked` and read by nothing.
- **The client's "issued no credential" check** in `requestBadgeIssue` fires when the stored tip still has months left after an answer with no credential.
- **Wire:** version 1, aeson with unknown fields ignored and `Maybe` fields optional. `BSPBadgeCredential {credential, receipt, statement}` (`Badges/Service.hs`). `SCCharge {chargeId}` already exists. An unknown enum value fails the whole response unless its type has a fallback constructor, as `BSEUnknown`, `SCUnknown` and `SDUnknown` do.
- **Timing today:** a credential expires at the end of the Monday after its paid month (`endOfMondayAfter`), 1-8 days after paidThrough. The app asks for the next one a day before that and shows it at that moment. Recipients show a badge as active until 7 days after its expiry, then faded as expired for a month, then hide it (`mkBadgeStatus`, `Badges.hs`); servers honour file perks for a day after expiry.

Invariants this run keeps:

- The serial request loop is the only ledger writer.
- Every row is its operation re-run at its `createdAt`, and `createdAt` never decreases along a ledger.
- Lapse comes before grant.
- The tier comes from what the store charged, never from the client.
- A refusal writes no ledger row, and no ledger row or issuance is written before the credential is signed. (A renewal check does record its check time, the subscription's state and any charge it saw.)
- Claims are by the store's own reference.

## 4. Design

The core change:

- A subscription purchase is an ordinary purchase whose ledger is refilled by store charges instead of being paid once.
- Its store identity is the subscription, not one transaction: Apple's original transaction id (Play's token already is). Every renewal therefore resolves to the same purchase and the same receipt record on both sides, and the service has one thing to ask the store about.
- Months enter a subscription ledger only as `charge` credits whose operation time is the store's billing period start. The grant row's `createdAt` is that period start, as `grantEntry` already stamps its `t`. Coverage follows the store's periods without drift, every row verifies by re-running its operation at its `createdAt`, and `StatementEntry` gains no field.
- The first charge and every renewal go through one pure planner: lapse what ran out before the period start, grant the charge's months at the period start, lapse what ran out since, then issue at now.
- The service asks the store only when a subscription purchase's paid coverage has ended, and writes nothing unless a charge is granted.
- The client keeps asking at the shared weekly moment and keeps the badge until the service answers that the subscription has ended.

### Timing and grace, end to end

- The app asks at the shared weekly moment, a day before the shown credential's expiry. That moment always falls after paidThrough (expiry is 1-8 days after it), and stores charge at the period's end, so a renewed subscription gets its new credential before the old one expires and swaps it in at expiry. The badge stays active for the user and their contacts throughout, and requests stay indistinguishable by timing.
- If the store has not charged (billing retry), no month is issued without a charge. The badge stays active by the existing rules alone: servers honour file perks until a day after the last credential's expiry, and contacts show it as active until 7 days after it. After that, contacts show it faded as expired, as for any lapsed badge. The app asks once a day meanwhile.
- The badge stays on the profile until the service answers `ended`, so a payment the store recovers late re-activates it with a fresh credential. Taking it off at the end of grace would need the resubscription work this run leaves out, since a removed purchase is never asked about again.
- While it stays shown, the profile cannot start a store purchase or redeem a code, and held receipts wait, because every purchase path refuses while a badge is shown. Nothing is charged meanwhile, so no money is lost; the iOS run decides what the user is told.
- The stores bound the wait themselves (Apple retries for up to 60 days, Play holds for up to 30; unverified), after which the service answers `ended` and the app removes the badge.
- Store grace periods are off. During a store's grace period the subscriber keeps uninterrupted paid service, but this run issues nothing until the charge lands, so a subscriber who recovers inside grace could go without file perks for part of what they paid for. With Apple's Billing Grace Period off (it is opt-in, and offers only 3, 16 or 28 days), "the subscriber's days of paid service pause until Apple is able to collect payment", which is exactly this run's rule; Play's grace is set to its minimum (reportedly 0, with about a day applied; unverified). Contacts still see the badge as active until 7 days after the last credential's expiry.

### Shared code (`src/Simplex/Chat/`)

- `PaymentService.hs`:
  - `appleTransactionId` becomes `appleTransactionRef` (structural rename, slice 1).
  - It then returns the payload's `originalTransactionId` when `type` is `"Auto-Renewable Subscription"`, the `transactionId` otherwise (slice 3a).
  - The client's `storeTransactionRef` and the service's `toStoreReceipt` keep calling this one function, so client and service claims cannot disagree.
  - The real Apple verifier must vouch with the same ref, because `purchaseWithReceipt` answers `internal` whenever the verified ref differs from the claim. So in the same slice, `transaction` in `StoreReceipts/Apple.hs` parses `type` and `originalTransactionId` and vouches with the original id for an auto-renewable transaction, as the dev mock already does by calling the claim function. Without this, a held Apple subscription record that adopts a renewal JWS would be answered `internal` on every attempt, and the buyer would never be credited.
  - The `googlePurchaseRef` comment states that the hash stays the claim while the service also keeps a subscription's token to ask Play about it.
- `Badges/Service.hs`:
  - New enum `SubscriptionState = SSRenewing | SSCancelled | SSBillingRetry | SSEnded | SSUnknown Text`, JSON `renewing`, `cancelled`, `billingRetry`, `ended`, and any other string as `SSUnknown`.
  - Meaning, as the service last read the store: renewing (auto-renew on), cancelled (auto-renew off, the paid period still runs), billingRetry (a charge failed and the store is retrying it: grace, retry or hold), ended (no charge will come: expired, revoked or refunded). The client reads `SSUnknown` as "not ended".
  - `BSPBadgeCredential` gains `subscription :: Maybe SubscriptionState`, present exactly when the purchase is funded by a subscription. The version stays 1: the field is optional, and older clients ignore it.
- `Badges/Ledger.hs`:
  - `entryTypeFromColumns` takes the row's charge id and rebuilds `SCCharge` from it (slice 2).
  - A pure planner (suggested name `chargeEntries`) takes the tip or a seed, the charge's period start, its months, the charge id, `now` and fresh entry ids. It returns `lapse periodStart` (if any), `grant periodStart months (SCCharge chargeId)`, `lapse now` (if any). The caller issues at `now` with `issueEntry` as today.
  - The rows' `createdAt` are non-decreasing whenever the period start is not before the tip's `createdAt`. That holds on every path: the tip was issued before its month ended, the next charge's period starts around that month's end, and the service writes no row while a subscription waits for its charge.

### Badge service (`apps/simplex-badge-service/src/BadgeService/`)

- Catalog (`Catalog.hs`):
  - `storeProduct` answers a sum: `StoreOneTime {badgeType, months}` for today's four SKUs, `StoreSubscription` for the subscription products (Apple `SUBSCR_BADGE_{SUPPORTER,LEGEND}_{MONTH,YEAR}_01`, Play `subscr_badge_{supporter,legend}_01`).
  - A new `storePlan provider planId` maps what a charge was for to `(badgeType, months)`. Apple plans are those four product ids. Play plans are the base plans `subscr-badge-supporter-month-02`, `subscr-badge-supporter-year-01`, `subscr-badge-legend-month-01`, `subscr-badge-legend-year-01`, because a Play receipt names the product and only Play's subscription record names the base plan.
  - Months are 1 or 12 and come from the plan, never from the period's length.
- Verifier interface (`StoreReceipts.hs`):
  - `StoreVerifier` gains `checkSubscription :: Maybe (SubscriptionRef -> IO (Either StoreRefusal SubscriptionCheck))`, run under the verifier timeout. Timeouts and other throws are unreachable, as for the online Google calls; a `StoreVerifierFailure`, which the Apple chain check throws for a chain it cannot place, is caught first and is a verifier failure (`internal`), as the offline Apple path treats it.
  - `SubscriptionRef` names what to ask about: Apple's original transaction id, or Play's token alone (`subscriptionsv2.get` needs nothing else). The Play product id is needed only by the purchase-path acknowledgement, which has it from the receipt.
  - `SubscriptionCheck` holds the `SubscriptionState`, a test-purchase flag, and the latest paid period if any, as `StoreCharge {chargeRef, planId, periodStart, periodEnd, paid, revoked, offer}`. `chargeRef` is Apple's `transactionId` or Play's order id; `revoked` is true when the store reports that charge refunded or revoked; `offer` is true when the charge is in a trial or introductory offer.
  - `noStoreVerifier` and the dev mock leave it `Nothing`, which answers `provider_not_configured`.
- Store connections (`StoreReceipts/Apple.hs`, `StoreReceipts/Google.hs`, `Config.hs`):
  - Apple: `AppleStoreConfig` gains the App Store Server API key (key id, issuer id, the `.p8` private key file) and an API host defaulting to production. Without the key, Apple one-time purchases verify as today and Apple subscriptions answer `provider_not_configured`.
  - The Apple check calls "Get All Subscription Statuses" for the original transaction id, authenticated by an ES256 JWT (issuer id, key id, bundle id, audience `appstoreconnect-v1`, short expiry, reused until near expiry). It verifies each signed transaction and signed renewal info with the same pinned-root chain check `verifyAppleTransaction` uses, refactored so the chain check and payload parsing are separate functions.
  - Apple mapping: status 1 is renewing or cancelled by the renewal info's `autoRenewStatus`; 3 (billing retry) and 4 (grace) are billingRetry; 2 (expired) and 5 (revoked) are ended. The latest transaction gives `transactionId`, `productId` (the plan), `purchaseDate` and `expiresDate` (the period), `revocationDate` (revoked), `offerType` (offer), `environment` (test purchase) and `inAppOwnershipType` (`FAMILY_SHARED` is refused as `receipt_invalid`).
  - Apple HTTP answers: 401 and 403 are this service's failure (`internal`, logged); 404, 429, 5xx, timeouts and transport errors are unreachable; a body whose signed transaction or renewal info does not verify against the pinned root, whether the check returns `Left` or throws, is a verifier failure, never a refusal.
  - The production host answers 404 for a Sandbox transaction id, so a test purchase must never reach it: on the purchase path the offline JWS check's `environment` refuses a test purchase as `test_purchase` before `checkSubscription` is called. The API transaction's `environment` is checked again as a defence, which only a Sandbox host would exercise.
  - Play: the check calls `purchases.subscriptionsv2.get` by token through the existing `playCall` and `bearerToken`, with the same HTTP classes as today's product reads.
  - Play mapping: `SUBSCRIPTION_STATE_ACTIVE` is renewing or cancelled by `autoRenewingPlan.autoRenewEnabled`; `IN_GRACE_PERIOD` and `ON_HOLD` are billingRetry; `CANCELED` is cancelled until `expiryTime`; `EXPIRED` and `PENDING_PURCHASE_CANCELED` are ended; `PENDING` is answered `payment_pending` as a pending refusal. The line item gives the plan (`offerDetails.basePlanId`), the period end (`expiryTime`), the charge ref (`latestSuccessfulOrderId`) and the offer flag from its current phase (`offerPhase` in a free trial or introductory price). `offerDetails` is not the flag: it names the offer the subscription was bought with and stays for its life, so reading it would refuse every later full-price renewal of a subscription bought on an offer. Play reports no current-period start, so it is the period end minus the plan's length. `testPurchase` present marks a test purchase. Play's revocation of an order shows only as the subscription ending, so Play charges are never `revoked`.
  - Play acknowledgement: an unacknowledged subscription is acknowledged with `purchases.subscriptions.acknowledge` before it is credited, as one-time purchases are today, so Play's three-day refund never hits a credited subscription.
- Schema: one new service migration `20261012_badge_subscriptions`, SQLite and Postgres, appended to both lists. It follows the commented draft in the client migration `M20260915_user_badges` for the parts this run uses, and the draft drops those parts, as the service's `20260925_badge_store_receipts` did for `payment_id`.
  - `@subscription_charges(charge_id TEXT PK, payment_id → @payments ON DELETE CASCADE, provider_charge_ref, plan_id, period_start, period_end, amount NULL, currency NULL, created_at, UNIQUE(payment_id, provider_charge_ref))`. Amount and currency are nullable because Play reports no price; `charged_at` is not stored.
  - `@badge_ledger.charge_id → @subscription_charges`, with an index.
  - `@payments.subscription_state`, `@payments.subscription_checked_at`, `@payments.provider_token`.
  - The service's never-written `@payments.subscription_renews_at`, `grace_until` and `cancelled` are dropped (and restored by the down migration), so the state has one representation. The client schema does not change.
- The Play token: kept only for subscription payments, because every renewal check needs it and the client no longer holds it once credited (`createStashBadgePurchase` clears it). It is never logged, never returned and never exported. The claim stays its hash.
- Charge ids are `randomId` text, like payment ids. This settles the MVP scope's two "settle before milestone 4" items: the wire `chargeId` is a service-assigned random id, and ledger entries reference payments and charges by those ids and purchases by key, never by a local integer.
- Storage (`Store.hs`):
  - `appendLedgerPlan` writes `charge_id` from the `SCCharge` it is given, and the ledger reads select it (slice 2).
  - New functions read a purchase's subscription from its payment row, insert a charge if new (`ON CONFLICT (payment_id, provider_charge_ref) DO NOTHING`), tell whether a charge is granted (a ledger row references it), and record the state and check time.
- Grant rule, for both paths: a charge is grantable when no ledger row references it, it is not revoked, it is not in an offer, the state is not `ended`, its plan's tier is the ledger's (or, on a first purchase, any plan of the catalog), and `periodStart <= now < periodEnd`.
- Purchase (`Service.hs` `purchaseWithReceipt`/`newPurchase`), for a subscription product:
  - Play: the receipt is verified by one `checkSubscription` call, routed on the receipt's product id; the one-time `verifyGoogle` is not called.
  - Apple: the JWS is verified offline as today; a test purchase is refused there; then one `checkSubscription` call.
  - A test purchase is answered `test_purchase`, as one-time purchases are.
  - With a grantable charge: acknowledge it (Play), plan from `emptyEntry periodStart tier`, sign the issued month, and write payment (claimed by `(provider, subscription ref)` as today), purchase, charge, ledger rows, issuance, state and token in one transaction.
  - With no grantable charge, the receipt worker reschedules at whatever `retryAfter` the answer gives, with no cap on attempts, and no payment row exists yet to bound checks by. So:
    - The period has not started yet, or the store answers pending: `payment_pending` with today's 300 s.
    - Play, with the state `ended`: `receipt_invalid`, terminal. Play issues a new token when a subscription is bought again after it ended, so this one can never be credited again; this is the same premise as the existing one-time rule that "a canceled purchase stays canceled, and buying again issues a new token".
    - Anything else (a revoked or offer charge while the state is not `ended`, a period over while the store still retries, no charge yet, an unknown plan, and an Apple subscription whose state is `ended`): `payment_pending` with `retryAfter` of a day. The record stays presentable, so a later charge on the same token, or an Apple resubscription under the same original id, can still be credited from it.
    - The implementer confirms each case against the refusal classes `StoreRefusal` documents; a case that does not fit them is escalated, not guessed.
- Renewal (`Service.hs` `issueBadgeCmd`), for a subscription purchase:
  - While its paid coverage runs (`paidThrough tip > now`), the path is today's: lapse, issue if a month is due, otherwise answer the current issuance. No store call.
  - Once coverage has ended, the service asks the store, unless a check of this subscription was attempted within the check interval (one hour). Every attempt stamps `subscription_checked_at`.
  - Inside the interval it answers from the stored state without calling the store. A failed check answers `provider_unavailable` with `retryAfter` of the whole interval.
  - A grantable charge: the planner's rows from the tip, sign, then one transaction that re-reads the tip and writes charge, rows, issuance and state. If the tip moved, nothing is written and the answer is `internal`, never a reply that looks credited.
  - No grantable charge: record the charge (if any) and the state, log a charge that is not grantable, write no ledger row.
  - A charge whose period does not end after its start is a verifier failure (`internal`, logged).
- Answers: every `BSPBadgeCredential` for a subscription purchase is built by `issueResponse` (the statement after the asserted entry and the current issuance) plus `subscription`. Purchase replays answered from the record carry it too (`credentialResponse` takes it).

### Client core (`src/Simplex/Chat/`)

- `Store/Badges.hs`: `UserBadgePurchase` gains a named kind (suggested `PurchaseKind = PKPrepaid | PKSubscription`; `BadgeFunding` is taken), read in `getBadgePurchase` as `CASE WHEN EXISTS (a charge credit in this purchase's badge_ledger) THEN 1 ELSE 0 END`. A purchase is a subscription exactly when its months came from charges. The replica already says so, so there is no column and no client migration.
- `Library/Commands.hs`, in the pass (`updateUserBadge`):
  - A prepaid purchase is retired before the request, as today.
  - A subscription purchase is not retired at paidThrough. It is retired after a request whose answer is `SSEnded`, through the same `retireExpiredBadge`, whose own `paidThrough <= now` guard still applies.
- `badgeRequestDue`: a subscription purchase is due at zero months on the same condition as a prepaid one with months left: the latest credential is the shown one and it expires within a day. A request that issued nothing re-arms a day later (`stalledAt` in `updateUserBadge`), which is the retry cadence while the store has not charged.
- `requestBadgeIssue` returns the answer's `subscription` beside the balance. Its "issued no credential" check fires only while the ledger still owes a month (`paidThrough > now`), instead of while `balanceMonths > 0`. Every prepaid path gives the same answer, since the service lapses before it answers; a subscription tip whose months elapsed unlapsed (the service writes nothing while waiting) owes nothing.
- `derivedBadgeAlert` raises no `BASupportEnded` for a subscription purchase that is still shown: it is waiting for its renewal, not ended.

### What gets removed or simplified

- The premise "no months after paidThrough" becomes one rule per purchase kind, stated where the pass branches.
- `testStoreUnknownProduct`'s subscription case, and the catalog comment "subscriptions wait until renewals add months".
- The service's three never-written subscription columns on `@payments`.
- The draft charge schema in the released client migration's comment (the parts the service migration now implements).
- Not added: a client column or client migration. The purchase kind is derived from the replica, so there is nothing to keep in sync.

### Alternatives considered, and why each was rejected

- **Date the grant at request time** (as prepaid grants are): coverage drifts by the 0-7 days between paidThrough and the weekly request, every month, and the app's "Renews" date stops matching the store's.
- **Add the period start to the charge entry** instead of using `createdAt`: a new `StatementEntry` field is dropped by the client replica, which stores fixed columns, and needs a new client check, while `createdAt` already is the operation's time.
- **Ask the store from a background lane or notifications only**, keeping the issue path DB-only: a second ledger writer that needs a Postgres lock, and notifications need external setup. Left for later work, which can feed the same charges table.
- **The app hands renewals over** (a new purchase per renewal, or a top-up of the shown badge): an Apple renewal would get fresh keys under whichever profile is active, and the top-up machinery would be needed.
- **Keep a subscription flag or state on the client purchase**: a client migration now, and two sources of the same fact. The app screens can add stored state if they need it.
- **Request at the store's renewal time** (model §2.9, "after `renews_at`"): a per-user request time links requests (objectives 3 and 4 in `docs/protocol/badges-overview.md`). The shared weekly moment already falls after paidThrough.
- **Retire at paidThrough and re-show on renewal**: contacts see the badge removed and re-added every month, and a retired purchase is never asked about again.
- **Remove the badge when recipients' grace ends**, while the store still retries: a payment the store recovers afterwards could not re-activate it without resubscription work, so the buyer would pay for a month they never see. Contacts already see it faded as expired by then.
- **Write the lapse at `now` when no charge is grantable** (as prepaid does): it is dated after a charge that may later be granted at an earlier period start, and it moves the client's tip so the pass counts as issued and stops retrying.
- **Read the Apple subscription from the JWS the app hands over**, without the App Store Server API: the app is not involved in renewals, so only the service's own read sees them.

## 5. Compatibility and security

- **Wire:** one optional response field, `badgeCredential.subscription`, version unchanged.
  - An older client ignores the field (aeson `rejectUnknownFields = False`).
  - A newer client reading an older service gets `Nothing`, and an older service never writes a charge, so nothing is derived as a subscription.
  - A state added later decodes as `SSUnknown` on this client and does not fail the response.
  - Apple claim refs change only for auto-renewable transactions, which no client hands over today (the apps finish them uncredited).
  - `docs/protocol/badges-rpc.md` and `badges-rpc.schema.json` gain the field, the `charge` credit's dating, `issueBadge`'s renewal behaviour, and the subscription answers of `purchaseBadge`. The doc's sentence that "each renewal credits months of the charged badge type" changes to say a charge for another tier is recorded and not granted until tier changes are supported.
- **Database:** one new service migration with up and down, SQLite and Postgres, covered by the service schema down/up cycle. No client migration. The client query in `getBadgePurchase` changes, so `chat_query_plans.txt` changes; only the full local run regenerates it, since it is rewritten from every query a full run records.
- **Config:** new optional keys in the `[apple]` section for the App Store Server API key and host. A deployment without them keeps verifying Apple one-time purchases.
- **Older and newer clients:** an older client never holds a subscription purchase, since no app sells one. An app downgraded to an older core retires a subscription badge at paidThrough and never asks again, so it loses every renewal the store keeps charging.
- **Adversary:**
  - Quota exhaustion: a client holding a subscription key repeats issue requests to make the service hammer a store's API quota, which every subscriber shares. For an existing purchase the store is asked only once coverage has ended, and at most once per subscription per hour, failures included; requests in between are answered from the stored state. On the purchase path there is no payment row to stamp: an honest client retries a period that has not started, or a pending payment, every 300 s only until it starts or settles, stops on a Play subscription whose state is `ended`, and retries every other case once a day; a client that ignores `retryAfter` can repeat a purchase as it can today (each attempt is one store call, as one-time Play purchases already are).
  - Replay: a charge is unique per `(payment, provider charge ref)` and granted only if no ledger row references it, in the transaction that re-reads the tip. A replay inside the issued period gets the cached credential.
  - Forged renewal evidence: none is accepted from a client. Renewals come only from the service's own store read, and Apple's answers are verified against the pinned root, so a client cannot name a charge, a period or a tier.
  - Another key presenting the same subscription: the claim is the subscription's identity, so another key gets `receipt_used`, as today.
  - Store data out of range (period end not after start, unknown plan, another tier, an offer): not granted; a malformed period is a verifier failure, logged.
  - A dishonest service dating grants: grants are legitimately dated up to about a month back, so the client's check accepts any date between the previous row's and now; it still flags a row dated before its predecessor. Accepted: the verdict is stored and nothing reads it.
  - Metadata: the zero-month request goes out at the same shared weekly moment as every renewal, so it adds no per-user timing. The service links a subscription's renewals to its purchase key, as it already links monthly issuance.
  - Secrets at rest: the Play token and the Apple API private key are usable only with our store accounts. Neither is logged or returned; the Apple key file is read like the Play service-account file.
  - Resource use: each check is one store call of at most the verifier timeout on the serial loop, bounded per subscription here. The aggregate load at the weekly moment is a known limit (§8).

## 6. Slices

Before slice 1, the user creates the branch on the host, after `f/badge-store-verifiers` is merged into `badge-in-app-checkout`: `git switch -c f/badge-subscriptions badge-in-app-checkout`.

The implementer runs `cabal build` and the tests in every slice. Every slice runs `cabal build` (project default flags, never `-O0`), then the scoped suites named in it, from `/workspace/simplex-chat-2`. Check the example counts, not only the exit status. hspec `-m` matches a substring of the example path, and several `-m` are alternatives. The new service and client subscription tests go under a `describe "subscriptions"` inside `badgeServiceTests` (`tests/Bots/BadgeService/BotTests.hs`), so `-m "SimpleX badge service e2e/subscriptions"` runs exactly them.

### Slice 1: name Apple claims by reference (structural)

- Files: `src/Simplex/Chat/PaymentService.hs`, `src/Simplex/Chat/Library/Commands.hs`, `BadgeService/StoreReceipts.hs`, `BadgeService/StoreReceipts/Mock.hs`, and any test that calls the function.
- Change: rename `appleTransactionId` to `appleTransactionRef`, matching `googlePurchaseRef` and `StoreTransactionRef`; no behaviour change. Slice 3a makes it return a subscription's original id, which the old name would misstate.
- Verify: `cabal test simplex-chat-test --test-options='-m "Supporter badges"'`.
- Done when: it builds and the suite passes with the same example count.

### Slice 2: charge storage and the response field (structural)

- Files: `BadgeService/Store/{SQLite,Postgres}/Migrations.hs`, `BadgeService/{Store,Service}.hs`, `src/Simplex/Chat/Badges/{Ledger,Service}.hs`, `src/Simplex/Chat/Store/Badges.hs` (the fallback `entryTypeFromColumns` call), the client migration `M20260915_user_badges` in both backends (draft comment only), `tests/BadgeTests.hs`, `tests/Bots/BadgeService/WebTests.hs`.
- Change:
  - The `20261012_badge_subscriptions` migration (§4).
  - `appendLedgerPlan` writes `charge_id` for an `SCCharge` row; the ledger tip and statement reads select it; `entryTypeFromColumns` gains the charge id; the client's fallback call passes `Nothing`.
  - `SubscriptionState` and the `subscription` field; `credentialResponse` takes it, and every caller passes `Nothing`.
  - Nothing writes a charge or a state yet, so no behaviour changes.
- Tests: `testEntryTypeColumns` asserts the `SCCharge` round trip with a charge id and `Nothing` without one; JSON tests for the field (present, absent, unknown state); `testServiceColumns` covers the new and dropped columns; the down/up cycle covers the migration.
- Verify: `cabal test simplex-chat-test --test-options='-m "Supporter badges" -m "SimpleX badge service"'` (this also runs the e2e suite).
- Done when: the suites pass, the down/up cycle includes the new migration, and AC11 and AC13 hold.

### Slice 3a: the charge planner and Apple subscription claims (behavioural, pure)

- Files: `src/Simplex/Chat/Badges/Ledger.hs`, `src/Simplex/Chat/PaymentService.hs`, `BadgeService/StoreReceipts/Apple.hs`, `tests/BadgeTests.hs`, `tests/Bots/BadgeService/StoreVerifierTests.hs`; the comments on `VerifiedStoreTransaction.transactionRef` and `OpenStorePurchase` that call Apple's ref the transaction id.
- Change: the planner; `appleTransactionRef` returns the original id for auto-renewable transactions, and the Apple verifier's `transaction` vouches with that same ref.
- Tests: `testChargeRowsVerify` (AC10, every case listed there); `testAppleTransactionRef` (subscription, consumable, missing type); a `StoreVerifierTests` case with a renewal payload (`transactionId` different from `originalTransactionId`, type auto-renewable) asserting the verified ref equals `appleTransactionRef` of the JWS.
- Verify: `cabal test simplex-chat-test --test-options='-m "Supporter badges" -m "badge store verifiers"'`.
- Done when: AC10, the AC12 unit test and the new renewal-payload case hold, and the existing Apple verifier cases still pass. This slice tests the riskiest assumption (§8) before any service code depends on it.

### Slice 3b: credit a subscription purchase, against the fake store (behavioural)

- Files: `BadgeService/{Catalog,StoreReceipts,Service,Store}.hs`, `tests/Bots/BadgeService/{FakeStore,BotTests}.hs`, `tests/BadgeTests.hs` (its `uncalledVerifier` builds `StoreVerifier` directly and needs the new field), `docs/protocol/badges-rpc.md`, `docs/protocol/badges-rpc.schema.json`.
- Change: catalog `StoreProduct` sum and `storePlan`; `checkSubscription`, `SubscriptionRef`, `SubscriptionCheck`, `StoreCharge`; the grant rule; the purchase branch for subscription products, with its no-grantable-charge answers; replays carry the state.
- FakeStore gains subscriptions: Google keyed by token and Apple keyed by original id, each with a mutable state and latest charge, and a check counter. Its Apple subscription fixtures (`unsignedJWS` with `type` and `originalTransactionId`) vouch with the original id, which is the claim `appleTransactionRef` reads.
- Tests: `testSubscriptionPurchase` (AC1), `testSubscriptionPurchaseNotGrantable` (AC9: a period not started, `payment_pending` 300 s; a Play subscription whose state is `ended`, `receipt_invalid`; a Play revoked or offer charge while renewing and an Apple subscription whose state is `ended`, `payment_pending` with a day; each writing nothing); `testStoreUnknownProduct` and `testStoreAcknowledgesBeforeCrediting` keep their unpriced cases with a product no catalog maps instead of `subscr_badge_supporter_01`. FakeStore vouches for that product with its own token, so the request reaches `storeProduct` and is answered `product_unavailable` rather than refused by the fake verifier.
- Verify: `cabal test simplex-chat-test --test-options='-m "SimpleX badge service e2e/store purchases" -m "SimpleX badge service e2e/subscriptions"'`.
- Done when: AC1 and AC9 hold and the store purchase examples still pass.

### Slice 4: renew once coverage ends, on the service (behavioural)

- Files: `BadgeService/{Service,Store}.hs`, `tests/Bots/BadgeService/{BotTests,FakeStore}.hs`, `docs/protocol/badges-rpc.md`.
- Change: the renewal step in `issueBadgeCmd`, the check interval stamped on every attempt, the one-transaction write that answers `internal` when the tip moved, answers through `issueResponse`.
- Tests (service, through `serviceCmd` with the shared test clock): `testSubscriptionChargeGrantedOnce` (AC5, with the replay inside the issued period), `testSubscriptionStoreChecksBounded` (AC6: coverage running makes no check; two requests after coverage ended within the hour make one; a failed check also holds the next for the hour), `testSubscriptionChargeNotGranted` (AC7: revoked, other tier, offer, period not started, period ended), `testSubscriptionStoreUnreachable` (AC8).
- Verify: `cabal test simplex-chat-test --test-options='-m "SimpleX badge service e2e"'`.
- Done when: AC5-AC8 hold and the existing e2e examples still pass.

### Slice 5: keep asking at zero months, on the client (behavioural)

- Files: `src/Simplex/Chat/Store/Badges.hs`, `src/Simplex/Chat/Library/Commands.hs`, `tests/Bots/BadgeService/BotTests.hs`.
- Change: the purchase kind on `UserBadgePurchase`; `badgeRequestDue` at zero months for a subscription; retirement on `SSEnded` after the request; the "issued no credential" check on owed months; no support-ended alert while a subscription is shown.
- Tests (real client and service, test clock, `renewalMoments`/`armWakeAt`): `testSubscriptionRenews` (AC2, including that the new credential is presented at the old one's expiry with no gap), `testSubscriptionAwaitsCharge` (AC3), `testSubscriptionEnds` (AC4), `testAppleRenewalResolvesToPurchase` (AC12: two Apple JWS with one original id and two transaction ids resolve to one receipt record and one purchase; and a held record whose first attempt is answered `payment_pending`, then adopts the renewal JWS, is credited at its next attempt). `testNoCredentialMonthsRanOut` and `testNoCredentialMonthsLeft` stay green under the changed check.
- Verify: `cabal test simplex-chat-test --test-options='-m "SimpleX badge service e2e"'`, then `--test-options='-m "Supporter badges"'`. `chat_query_plans.txt` is left to the full run (§7); a scoped run must not be used to regenerate it.
- Done when: AC2-AC4, AC12 and AC17 hold.

### Slice 6: Apple subscription check (behavioural)

- Files: `BadgeService/StoreReceipts/Apple.hs`, `BadgeService/{Config,Service}.hs`, `apps/simplex-badge-service/badge_service.ini.example`, `scripts/badge-service/badge_service.ini.example`, `apps/simplex-badge-service/README.md`, `tests/Bots/BadgeService/{StoreVerifierTests,ConfigTests}.hs`, a new fake App Store API (for example `tests/Bots/BadgeService/FakeAppStore.hs`, a Warp server like `FakePlay`).
- Change:
  - Separate the pinned-root chain check from the transaction parser in `Apple.hs`, so the API's signed transactions and renewal info reuse it; parse the subscription fields (§4).
  - The API key config, the ES256 JWT with reuse, "Get All Subscription Statuses", the status mapping, the HTTP classes, and `FAMILY_SHARED` refusal.
  - `configureStoreVerifier` sets `checkSubscription` for Apple when the key is configured.
- Tests: Apple subscription cases in `StoreVerifierTests` against the fake API, signed with the test chain the existing Apple cases build: each status and its mapping, cancelled versus renewing by `autoRenewStatus`, revoked, offer, family-shared, a body signed by another root and a chain the check throws on, and a signed transaction or renewal info whose leaf the intermediate did not sign or whose signature does not verify (the check's `Left`), all verifier failures, never a refusal, 401/403 (internal), 404/429/5xx/timeout (unreachable), the JWT's claims and header checked by the fake. A service test: an Apple Sandbox subscription JWS is answered `test_purchase` without any call to the fake API. The API-side Sandbox case is a defence only a Sandbox host would exercise. `ConfigTests`: the key keys, an incomplete key, the default host.
- Verify: `cabal test simplex-chat-test --test-options='-m "SimpleX badge service"'`.
- Done when: AC14 and AC16's Apple part hold, and the existing Apple verifier cases pass.

### Slice 7: Play subscription check and acknowledgement (behavioural)

- Files: `BadgeService/StoreReceipts/Google.hs`, `BadgeService/{StoreReceipts,Service}.hs`, `tests/Bots/BadgeService/{StoreVerifierTests,FakePlay}.hs`.
- Change: `subscriptionsv2.get` through `playCall`; the state, plan, period, order and offer mapping; test purchases; the subscription acknowledgement, called before a subscription is credited and skipped when Play reports it acknowledged; `configureStoreVerifier` sets `checkSubscription` for Play; a Play subscription receipt is verified by `checkSubscription` alone.
- FakePlay gains the `purchases/subscriptionsv2/tokens/{token}` and `purchases/subscriptions/{productId}/tokens/{token}:acknowledge` routes; its existing `PlayRecord` answers serve subscription bodies.
- Tests: Play subscription cases in `StoreVerifierTests`: each state and its mapping, cancelled versus renewing, the derived period start for monthly and annual plans, an unknown base plan, a line item in a free-trial or introductory `offerPhase` (offer) and one bought on an offer now in its base-price phase (not an offer), a test purchase, a pending subscription, acknowledgement made once and skipped when already acknowledged, a failed acknowledgement crediting nothing, and the existing HTTP classes. A service test buys a Play subscription through FakePlay end to end.
- Verify: `cabal test simplex-chat-test --test-options='-m "SimpleX badge service"'`.
- Done when: AC15 and AC16's Play part hold, and the existing Play verifier cases pass.

## 7. Test plan

- New tests and what each proves: listed per acceptance criterion in §1 and per slice in §6. The pure planner test (AC10) proves the dating choice against the client's own check. The service tests prove grant-once, replays, bounded store use and the refusal classes. The client tests prove the badge stays on across paidThrough with no gap and comes off only on `ended`. The store tests prove each store's answers map to the right state and refusal class, and that Apple's answers are trusted only through the pinned root.
- Existing tests that must stay green: "Supporter badges" (ledger transitions, statement checks, JSON, store purchases, request loop), "SimpleX badge service" (config, web, catalog, schema cycle, store verifiers), "SimpleX badge service e2e" (renewals, lapses, retirement, alerts, store purchases, receipt worker). `testStoreUnknownProduct` changes as described.
- Full-suite check, at the end:
  - `cabal test simplex-chat-test`, which also rewrites `src/Simplex/Chat/Store/SQLite/Migrations/chat_query_plans.txt` for the changed purchase query.
  - On the host, a Postgres build of the service, `cabal build --flags=+client_postgres exe:simplex-badge-service`, and a Postgres run of `cabal test --flags=+client_postgres simplex-chat-test --test-options='-m "badge service schema"'`, which executes the new migration's Postgres SQL. The deployed service is Postgres-only, and CI tests SQLite only.

## 8. Risks

- **Riskiest assumption, and how the first slice tests it early:** that dating every charge grant to the store's period start keeps `createdAt` non-decreasing and every row verifiable on all renewal paths, including annual renewals and recovery after billing retry or grace. Slice 3a's `testChargeRowsVerify` runs the planner's rows through the client's own `balanceChecked` before any service code depends on them. A failure there changes the design (to an explicit period field) before slices 3b-7 are built.
- **Store behaviour assumed from documentation, confirmed only with real store accounts** (Sandbox and licence testers, after this run): that Apple renewals keep `originalTransactionId` and a resubscription in the same group keeps it too; that Apple's "Get All Subscription Statuses" answers by original transaction id with the latest signed transaction; that Play keeps the token across renewals and issues a new one on resubscription after the subscription ended; that Play's period start is the period end minus the plan's length. The fake stores follow the documentation, so tests cannot catch a mismatch. If one is wrong, `SubscriptionCheck` mapping changes; the planner, the ledger and the client do not. The Play terminal answer for an ended subscription depends on the new-token premise; if Play reused the token, it becomes the day-long `payment_pending`.
- **Load:** every subscriber renewing in a week asks at the same instant (the TODO above `badgeBoundary`), and each check is up to the verifier timeout on the serial loop against a 30 s client deadline. A large renewal cohort times out and retries until requests are spread across the lead day and store calls leave the serial loop.
- **The derived purchase kind:** a purchase is a subscription exactly when its replica holds a charge credit. A future re-bind, transfer or `opening` restatement (which the protocol specifies for new devices) must keep a charge credit in the replica, or the client would retire a live subscription at paidThrough.
- **A badge waiting for a charge blocks new purchases:** while a subscription badge waits for its store to charge, the profile cannot buy or redeem another badge, for up to the store's retry window, or indefinitely if a store never reports `ended` (for example after an Apple tier change, which this run does not grant).
- **One `ended` answer is final:** the client retires on the first `ended` at or after paidThrough, and a retired purchase is not asked about again. If a store briefly reports a subscription as expired around a renewal, the renewals after it are lost until resubscription support exists.
- **Before subscriptions can be sold** (not this run, each a pay-and-get-nothing or more-than-paid case): spreading the weekly requests and moving store calls off the serial loop; resubscription after the badge was removed and restore on a new device (an Apple resubscription after retirement resolves to the old, credited receipt record); tier changes (iOS lets a user switch tier from Settings, and the new tier's charges are recorded and never granted); refund handling (a refunded annual keeps issuing until its months run out); the app screens and hand-over.

## 9. Open questions

None. The timing and grace behaviour, the branch, who runs builds, tier changes, Family Sharing, the first-purchase answers and the store connections are decided in §2, §4 and §6.

## 10. Out of scope (noticed, not doing)

- `writeIssued` answers a skipped issuance as success. Harmless for issuance; only the charge path changes here.
- The prepaid-ending alert TODO above `derivedBadgeAlert`.
- Unused drafts: `PaymentTerm`/`PTSubscription` (`PaymentService/Types.hs`), `BadgeCharge` and `BadgePlan` (`Badges/Types.hs`), `BSCUpgradeBadgeSubscription`'s `badgeRequest` (`Badges/Service.hs`).
- The Apple verifier checks no certificate validity dates or revocation.
- A Play subscription purchase is offered by the first offer on its base plan, not a named offer (`PlayStore.kt`).
- Retirement does not signal the receipt worker.
- After a renewal that lands once the shown credential has expired (billing-retry recovery), the request and the profile update happen in the same pass, because `presentIssuedBadge` presents as soon as the shown credential has expired. This already happens to late prepaid renewals; it weakens objective 3 for recovered subscriptions.

## 11. Follow-up runs

Each item marked **(blocks sale)** must land before subscriptions are offered in either app.

- **(blocks sale)** Load and timing: spread requests across the lead day, drop queued requests older than the client deadline, take store calls off the serial loop behind a Postgres row lock on the purchase.
- **(blocks sale)** Resubscription and restore: a resubscription after the badge was removed and a new device's restore re-bind the subscription to the presenting key and show it again (model § recovery, "re-bind, capped per payment and period").
- **(blocks sale)** Tier changes: a tier-changing grant for Apple upgrades and downgrades and Play replacements, and the client updating `current_badge_type`.
- **(blocks sale)** Refunds: a per-charge refund debit, so a refunded annual stops issuing; Play's Voided Purchases API.
- Store notifications: App Store Server Notifications V2 and Play real-time notifications updating charges and state between requests (a second writer, after the row lock).
- **(blocks sale)** iOS app: sell monthly and annual, hand subscription transactions over instead of finishing them, "Renews"/"Ends"/"Payment problem" on Your badge, the manage-subscriptions sheet, Restore purchases, the subscription alerts, disclosures, dev mock subscriptions. A subscription receipt must not wait behind a shown badge while the store charges (refuse code redemption while the profile has an open store purchase, or hand subscription receipts over regardless). A held subscription record adopts the newest JWS; a revoked renewal's terminal refusal must not refuse the whole subscription, and adopting a newer transaction should make the record due at once. While a subscription badge waits for its store to charge, the profile cannot buy or redeem another badge; the app should say so.
- **(blocks sale)** Kotlin port of the iOS run, plus the Play sweep querying subscriptions and offer selection by base plan.

External waits before sale (not runs):

- The App Store Connect subscription group and products, with Billing Grace Period off; Play subscription products with the grace period at its minimum, account hold, and subscription pausing off (§4 Timing).
- No trial or introductory offers on any subscription plan.
- Family Sharing off for the subscription products; it cannot be undone once on.
- The subscription products stay unbuyable (not live, or not offered by any app build) until sale: today's apps finish a subscription transaction without crediting it, so a buyer would pay and get nothing.
- An App Store Server API key; a Play service account with order permissions.
- Store API quotas for the expected renewal checks, raised if needed.
- Store review of the subscription screens, including how review purchases are handled: Sandbox and licence-tester purchases are answered `test_purchase`, the same open question as the one-time lane's.
- Terms and privacy links; translations.
