# Plan: store subscription renewals for supporter badges

Run: 001-badge-subscriptions · Base branch: `f/badge-subscriptions`, cut from `badge-in-app-checkout` (5cddbffaa) · Repos: simplex-chat-2 · Estimated diff: ~850 changed lines (~480 code and docs, ~370 tests)

This run builds the renewal mechanics of store subscriptions end to end, in the badge service and the client core, against an in-test fake store. It does not call Apple or Google and changes no app UI; those are follow-up runs (§11). Nothing here puts a subscription on sale.

Sources: the draft model `plans/2026-07-30-supporter-badges-v3-ux.md` (§3 charges and O2, §5, §7), `plans/2026-08-04-badges-mvp-scope.md` milestone 4, and `docs/protocol/badges-rpc.md`.

Code references are file:line in this repository at `badge-in-app-checkout` 5cddbffaa. "vb" is the unmerged branch `f/badge-store-verifiers`. Abbreviations:

- C: `src/Simplex/Chat/Library/Commands.hs`
- SB: `src/Simplex/Chat/Store/Badges.hs`
- L: `src/Simplex/Chat/Badges/Ledger.hs`
- BS: `src/Simplex/Chat/Badges/Service.hs`
- T: `src/Simplex/Chat/Badges/Types.hs`
- PS: `src/Simplex/Chat/PaymentService.hs`
- SVC, STO, SR, CAT: `apps/simplex-badge-service/src/BadgeService/{Service,Store,StoreReceipts,Catalog}.hs`
- Bot: `tests/Bots/BadgeService/BotTests.hs`

## 1. Goal and acceptance criteria

A badge bought as a store subscription keeps renewing. When its paid months run out, the app sends its usual signed issue request at the usual weekly moment. The service asks the store whether the subscription charged again, grants the charge's months dated to the store's billing period, and returns the next credential. The app keeps the badge on until the service says the subscription has ended.

- AC1: A verified subscription receipt creates a purchase whose ledger is one `charge` credit dated to the store's period start and one badge debit, records one charge row, issues the first month, and answers with `subscription = renewing`. A test purchase is refused `receipt_invalid`. Test: `testSubscriptionPurchase` (service).
- AC2: At zero months a subscription purchase is still asked about, a day before its shown credential expires. The badge stays shown past paidThrough. A new store charge is granted at its period start, and the next credential is issued and presented at the old one's expiry. Test: `testSubscriptionRenews` (client and service).
- AC3: When the store has not charged yet (billing retry), the service answers with no credential and `subscription = billingRetry` and writes no ledger row; the client records no failure, keeps the badge and asks again a day later. Once the charge lands, its month is issued with coverage from the store's period start. Test: `testSubscriptionAwaitsCharge` (client and service).
- AC4: When the service answers `subscription = ended` at or after paidThrough, the client retires the badge, raises the support-ended alert and sends no further requests for that purchase. Test: `testSubscriptionEnds` (client and service).
- AC5: Each store charge is granted at most once. A repeated issue request inside the issued period returns the same credential without asking the store, and a store reporting the same charge again adds no second grant and no second charge row. Test: `testSubscriptionChargeGrantedOnce` (service).
- AC6: The store is asked only when a subscription purchase's paid coverage has ended, and at most once per subscription per check interval (one hour), whether or not the store answered. Requests in between are answered from the stored state. Test: `testSubscriptionStoreChecksBounded` (service, counts fake-store checks).
- AC7: On a renewal check, a charge that was refunded or revoked, a charge for another tier, and one whose period has not started or has already ended are recorded and not granted. (On the purchase path nothing is written; see AC1's test.) Test: `testSubscriptionChargeNotGranted` (service).
- AC8: An unreachable store during a renewal check answers `provider_unavailable` with `retryAfter` of the check interval, stamps the check time, and writes no charge or ledger row. Test: `testSubscriptionStoreUnreachable` (service).
- AC9: Every row the charge planner produces verifies on the client: `balanceChecked` gives `Just True` for the charge credit, the lapses and the badge debit. Cases: a first charge, a monthly renewal, an annual renewal with a month left at the period start, a renewal after billing-retry recovery, an annual subscriber away past the year end whose charge recovers inside the store's grace period, and a period start equal to the balance start. Test: `testChargeRowsVerify` (unit).
- AC10: A ledger holding a charge row is stored and read back by the service, and `entryTypeFromColumns` rebuilds `SCCharge` from its charge id. Tests: `testEntryTypeColumns` (updated), the service schema down/up cycle (`testSchemaDownUpCycle`).
- AC11: An Apple auto-renewable transaction is claimed by its `originalTransactionId`, every other Apple transaction by its `transactionId`. A renewal transaction handed over by the app resolves to the credited receipt record and creates no purchase. Tests: `testAppleTransactionRef` (unit), `testAppleRenewalResolvesToPurchase` (client and service).
- AC12: The `subscription` field round-trips in JSON, is absent for prepaid purchases, and an unknown state decodes as `SSUnknown` rather than failing the response. Test: credential response JSON tests (unit).
- AC13: Prepaid purchases (codes, one-time store purchases) behave exactly as before. The existing "Supporter badges", "SimpleX badge service" and "SimpleX badge service e2e" suites pass unchanged, except `testStoreUnknownProduct`, whose subscription case moves to AC1.

## 2. Non-goals

- Calling Apple or Google: the App Store Server API, Play `subscriptionsv2`, Play subscription acknowledgement (run 2).
- Selling subscriptions in the apps, handing subscription transactions over from the apps, management, cancel and restore screens, subscription alerts in the UI (runs 7 and 8).
- Tier changes, revival after retirement, restore and re-bind, store notifications, refunds (runs 4-6).
- Spreading the weekly request moment and taking store calls off the serial request loop (run 3).
- The dev mock verifier gains no subscription support, so a dev service answers subscription receipts `provider_not_configured` until run 7 adds it.

## 3. Current state

What the code does today, at 5cddbffaa:

- **The client stops at zero months.** `badgeRequestDue` (C:5687-5691) requires `balanceMonths > 0` and a shown credential. `retireExpiredBadge` (C:5827-5834) takes the badge off at paidThrough, after which the purchase is never asked about. Both rules rest on one premise: no months can arrive after paidThrough. Subscriptions remove that premise, so both rules change together.
- **The service never reads a store after the first purchase and never grants onto an existing ledger.** `issueBadgeCmd` (SVC:505-542) lapses and issues. `signFirstMonth` (SVC:484-496) seeds an empty ledger at `now` and issues without lapsing. `writeIssued` (SVC:528-533) replies normally when it skipped its write, through `issueResponse`, which returns the current issuance.
- **A charge row cannot be stored or read by the service.** `appendLedgerPlan` (STO:310-342) writes no `charge_id`. `entryTypeFromColumns` (L:214-231) has no charge case, so a charge row reads as an empty ledger at the tip (STO:247-261) and fails any statement read with `internal` (STO:272-290). No charges table and no `badge_ledger.charge_id` exist on either side.
- **Apple renewals are not tied to their purchase.** Claims use `appleTransactionId` (PS:46-54), the transaction's own id, on the client (C:5314-5319) and the service (SR:71). A renewal has a new id, so `holdStoreReceipt` (SB:131-166) would give it a new receipt row with fresh keys. A Play renewal keeps its token and already resolves to the credited row.
- **The catalog sells no subscription** (CAT:139-154). `testStoreUnknownProduct` (Bot:1746-1753) pins `subscr_badge_supporter_01` as `product_unavailable`.
- **The verifier interface has no subscription read** (SR:51-56). `verifyApple` is pure; `verifyGoogle` reads one-time products only (vb's real verifier calls `purchases.products`, which does not serve subscription tokens).
- **The client's statement check** (L:127-171) re-runs each credit through `grantEntry (createdAt e)` and flags `createdAt` earlier than the previous row's. Its verdict is stored in `balance_checked` and read by nothing.
- **The client's "issued no credential" check** (C:5779) fires when the stored tip still has `balanceMonths > 0` after an answer with no credential.
- **Wire:** version 1, aeson with unknown fields ignored and `Maybe` fields optional (BS:60-68, `defaultJSON`). `BSPBadgeCredential {credential, receipt, statement}` (BS:121-125). `SCCharge {chargeId :: Text}` already exists (BS:190). An unknown enum value fails the whole response unless the type has a fallback constructor, as `BSEUnknown`, `SCUnknown` and `SDUnknown` do.

Invariants this run keeps:

- The serial request loop is the only ledger writer.
- Every row is its operation re-run at its `createdAt`, and `createdAt` never decreases along a ledger.
- Lapse comes before grant.
- The tier comes from what the store charged, never from the client.
- A refusal writes nothing, and nothing is written before the credential is signed.
- Claims are by the store's own reference.

## 4. Design

The core change:

- A subscription purchase is an ordinary purchase whose ledger is refilled by store charges instead of being paid once.
- Its store identity is the subscription, not one transaction: Apple's original transaction id (Play's token already is). Every renewal therefore resolves to the same purchase and the same receipt record on both sides, and the service has one thing to ask the store about.
- Months enter a subscription ledger only as `charge` credits whose operation time is the store's billing period start. The grant row's `createdAt` is that period start, exactly as `grantEntry` already stamps its `t`. Coverage follows the store's periods without drift, every row still verifies by re-running its operation at its `createdAt`, and `StatementEntry` gains no field.
- The first charge and every renewal go through one pure planner: lapse what ran out before the period start, grant the charge's months at the period start, lapse what ran out since, then issue at now.
- The service asks the store only when a subscription purchase's paid coverage has ended, and writes nothing unless a charge is granted.
- The client keeps asking at the shared weekly moment and keeps the badge until the service answers that the subscription has ended.

### Shared code (`src/Simplex/Chat/`)

- `PaymentService.hs`:
  - `appleTransactionId` becomes `appleTransactionRef` (structural rename, slice 1).
  - It then returns the payload's `originalTransactionId` when `type` is `"Auto-Renewable Subscription"`, the `transactionId` otherwise (slice 3a).
  - The client's `storeTransactionRef` and the service's `toStoreReceipt` keep calling this one function, so client and service claims cannot disagree.
  - The `googlePurchaseRef` comment (PS:55) states that the hash stays the claim while the service also keeps a subscription's token to ask Play about it.
- `Badges/Service.hs`:
  - New enum `SubscriptionState = SSRenewing | SSCancelled | SSBillingRetry | SSEnded | SSUnknown Text`, JSON `renewing`, `cancelled`, `billingRetry`, `ended`, and any other string as `SSUnknown`.
  - Meaning, as the service last read the store: renewing (auto-renew on), cancelled (auto-renew off, the paid period still runs), billingRetry (a charge failed and the store is retrying it: grace, retry or hold), ended (no charge will come: expired, revoked or refunded). The client reads `SSUnknown` as "not ended".
  - `BSPBadgeCredential` gains `subscription :: Maybe SubscriptionState`, present exactly when the purchase is funded by a subscription. The version stays 1: the field is optional and older clients ignore it.
- `Badges/Ledger.hs`:
  - `entryTypeFromColumns` takes the row's charge id and rebuilds `SCCharge` from it (slice 2).
  - A pure planner (suggested name `chargeEntries`) takes the tip or a seed, the charge's period start, its months, the charge id, `now` and fresh entry ids. It returns `lapse periodStart` (if any), `grant periodStart months (SCCharge chargeId)`, `lapse now` (if any). The caller issues at `now` with `issueEntry` as today.
  - The rows' `createdAt` are non-decreasing whenever the period start is not before the tip's `createdAt`. That holds on every path: the tip was issued before its month ended, the next charge's period starts around that month's end, and the service writes no row while a subscription waits for its charge.

### Badge service (`apps/simplex-badge-service/src/BadgeService/`)

- Catalog (`Catalog.hs`):
  - `storeProduct` answers a sum: `StoreOneTime {badgeType, months}` for today's four SKUs, `StoreSubscription` for the subscription products (Apple `SUBSCR_BADGE_{SUPPORTER,LEGEND}_{MONTH,YEAR}_01`, Play `subscr_badge_{supporter,legend}_01`).
  - A new `storePlan provider planId` maps what a charge was for to `(badgeType, months)`. Apple plans are those four product ids. Play plans are the base plans `subscr-badge-supporter-month-02`, `subscr-badge-supporter-year-01`, `subscr-badge-legend-month-01`, `subscr-badge-legend-year-01`, because a Play receipt names the product and only the store's subscription record names the base plan.
  - Months are 1 or 12 and come from the plan, never from the period's length.
- Verifier (`StoreReceipts.hs`):
  - `StoreVerifier` gains `checkSubscription :: Maybe (SubscriptionRef -> IO (Either StoreRefusal SubscriptionCheck))`, run under the same timeout as `verifyGoogle`, with timeouts and throws answered unreachable.
  - `SubscriptionRef` names what to ask about: Apple's original transaction id, or Play's product id and token.
  - `SubscriptionCheck` holds the `SubscriptionState`, a test-purchase flag, and the latest paid period if any, as `StoreCharge {chargeRef, planId, periodStart, periodEnd, paid, revoked}`. `chargeRef` is Apple's `transactionId` or Play's order id; `revoked` is true when the store refunded or revoked that charge (Apple `revocationDate`, Play a voided order).
  - `noStoreVerifier` and the dev mock leave it `Nothing`, which answers `provider_not_configured`.
  - How each store fills these fields is run 2's work; the expected mapping is in §9.
- Schema: one new service migration `20261012_badge_subscriptions`, SQLite and Postgres, appended to both lists. It follows the commented draft in `M20260915_user_badges.hs:155-227` for the parts this run uses, and the draft drops those parts, as the service's `20260925_badge_store_receipts` did for `payment_id`.
  - `@subscription_charges(charge_id TEXT PK, payment_id → @payments ON DELETE CASCADE, provider_charge_ref, plan_id, period_start, period_end, amount NULL, currency NULL, created_at, UNIQUE(payment_id, provider_charge_ref))`. Amount and currency are nullable because Play reports no price; `charged_at` is not stored.
  - `@badge_ledger.charge_id → @subscription_charges`, with an index.
  - `@payments.subscription_state`, `@payments.subscription_checked_at`, `@payments.provider_token`.
  - The service's never-written `@payments.subscription_renews_at`, `grace_until` and `cancelled` are dropped (and restored by the down migration), so the state has one representation. The client's columns stay; nothing changes on the client schema.
- The Play token: kept only for subscription payments, because every renewal check needs it and the client no longer holds it once credited (SB:289). It is never logged, never returned and never exported. The claim stays its hash.
- Charge ids are `randomId` text, like payment ids. This settles the MVP scope's two "settle before milestone 4" items: the wire `chargeId` is a service-assigned random id, and ledger entries reference payments and charges by those ids and purchases by key, never by a local integer.
- Storage (`Store.hs`):
  - `appendLedgerPlan` writes `charge_id` from the `SCCharge` it is given, and the ledger reads select it (slice 2).
  - New functions read a purchase's subscription from its payment row, insert a charge if new (`ON CONFLICT (payment_id, provider_charge_ref) DO NOTHING`), tell whether a charge is granted (a ledger row references it), and record the state and check time.
- Purchase (`Service.hs` `purchaseWithReceipt`/`newPurchase`), for a subscription product:
  - Play: the receipt is verified by one `checkSubscription` call, routed on the receipt's product id; `verifyGoogle` is not called.
  - Apple: the JWS is verified offline as today, then one `checkSubscription` call.
  - A test purchase is refused `receipt_invalid`, as today.
  - With a grantable charge: plan from `emptyEntry periodStart tier`, sign the issued month, and write payment (claimed by `(provider, subscription ref)` as today), purchase, charge, ledger rows, issuance, state and token in one transaction.
  - With no grantable charge, the answer depends on why (Q7). The receipt worker reschedules at whatever `retryAfter` says (C:5534-5541), with no cap on attempts, and no payment row exists yet to bound checks by.
    - The latest charge's period has not started yet, or the store answers pending (Play `PENDING`): `payment_pending` with today's 300 s.
    - Play, and the subscription's state is `ended`: `receipt_invalid`, since a resubscription after expiry comes with a new token (unverified, run 2 confirms) and this one can never be credited again.
    - Anything else not grantable, on either store (a revoked charge while the state is not `ended`, a period over while the store still retries, no charge yet, an unknown plan, and an Apple subscription whose state is `ended`): `payment_pending` with `retryAfter` of a day. The record stays presentable, so a later charge on the same token, or an Apple resubscription under the same original id, can still be credited from it.
- Renewal (`Service.hs` `issueBadgeCmd`), for a subscription purchase:
  - While its paid coverage runs (`paidThrough tip > now`), the path is today's: lapse, issue if a month is due, otherwise answer the current issuance. No store call.
  - Once coverage has ended, the service asks the store, unless a check of this subscription was attempted within the check interval (one hour). Every attempt stamps `subscription_checked_at`.
  - Inside the interval it answers from the stored state without calling the store. A failed check answers `provider_unavailable` with `retryAfter` of the whole interval.
  - A charge is grantable when no ledger row references it, it is not revoked, the state is not `ended`, its plan's tier is the ledger's, and `periodStart <= now < periodEnd`. The same rule decides the first purchase's charge. This is the model's "charges for refunded periods are recorded without grants" (model line 284).
  - A grantable charge: the planner's rows from the tip, sign, then one transaction that re-reads the tip and writes charge, rows, issuance and state. If the tip moved, nothing is written and the answer is `internal`, never a reply that looks credited.
  - No grantable charge: record the charge (if any) and the state, log a charge that is not grantable, write no ledger row.
  - A charge whose period does not end after its start is a verifier failure (`internal`, logged).
- Answers: every `BSPBadgeCredential` for a subscription purchase is built by `issueResponse` (the statement after the asserted entry and the current issuance) plus `subscription`. Purchase replays answered from the record carry it too (`credentialResponse` takes it).

### Client core (`src/Simplex/Chat/`)

- `Store/Badges.hs`: `UserBadgePurchase` gains a named kind (suggested `PurchaseKind = PKPrepaid | PKSubscription`; `BadgeFunding` is taken), read in `getBadgePurchase` as `CASE WHEN EXISTS (a charge credit in this purchase's badge_ledger) THEN 1 ELSE 0 END`. A purchase is a subscription exactly when its months came from charges. The replica already says so, so there is no column and no client migration.
- `Library/Commands.hs`, in the pass (`updateUserBadge`):
  - A prepaid purchase is retired before the request, as today.
  - A subscription purchase is not retired at paidThrough. It is retired after a request whose answer is `SSEnded`, through the same `retireExpiredBadge`, whose own `paidThrough <= now` guard still applies.
- `badgeRequestDue`: a subscription purchase is due at zero months on the same condition as a prepaid one with months left: the latest credential is the shown one and it expires within a day. A request that issued nothing re-arms a day later (`stalledAt`, C:5579), which is the retry cadence while the store has not charged.
- `requestBadgeIssue` returns the answer's `subscription` beside the balance.
- The "issued no credential" check (C:5779) fires only while the ledger still owes a month (`paidThrough > now`), instead of `balanceMonths > 0`. Every prepaid path gives the same answer, since the service lapses before it answers. A subscription tip whose months elapsed unlapsed (the service writes nothing while waiting) owes nothing.
- `derivedBadgeAlert` raises no `BASupportEnded` for a subscription purchase that is still shown: it is waiting for its renewal, not ended.

### What gets removed or simplified

- The premise "no months after paidThrough" becomes one rule per purchase kind, stated where the pass branches.
- `testStoreUnknownProduct`'s subscription case, and the catalog comment "subscriptions wait until renewals add months".
- The service's three never-written subscription columns on `@payments`.
- The draft charge schema in the released client migration's comment (the parts the service migration now implements).
- Not added: a client column or client migration. The purchase kind is derived from the replica, so there is nothing to keep in sync.

### Alternatives considered, and why each was rejected

- **Date the grant at request time** (as prepaid grants are): coverage drifts by the 0-7 days between paidThrough and the weekly request, every month, and the app's "Renews" date stops matching the store's.
- **Add the period start to the charge entry** instead of using `createdAt`: a new `StatementEntry` field is dropped by the client replica (SB:493-506) and needs a new client check, while `createdAt` already is the operation's time.
- **Ask the store from a background lane or notifications only**, keeping the issue path DB-only: a second ledger writer that needs a Postgres lock, and notifications need external setup. Deferred to runs 3 and 6, which can feed the same charges table.
- **The app hands renewals over** (a new purchase per renewal, or a top-up of the shown badge): decided against by the user. An Apple renewal would get fresh keys under whichever profile is active (SB:131-166), and the top-up machinery would be needed.
- **Keep a subscription flag or state on the client purchase**: a client migration now, and two sources of the same fact. Run 7 can add stored state if the UI needs it.
- **Request at the store's renewal time** (model line 127, "after `renews_at`"): a per-user request time links requests (objectives 3 and 4, `badges-overview.md:205-206`). The shared weekly moment already falls on or after paidThrough (`endOfMondayAfter` is 1-8 days later).
- **Retire at paidThrough and re-show on renewal**: contacts see the badge removed and re-added every month, and a retired purchase is never asked about again.
- **Write the lapse at `now` when no charge is grantable** (as prepaid does): it is dated after a charge that may later be granted at an earlier period start, and it moves the client's tip so the pass counts as issued and stops retrying.

## 5. Compatibility and security

- **Wire:** one optional response field, `badgeCredential.subscription`, version unchanged.
  - An older client ignores the field (aeson `rejectUnknownFields = False`).
  - A newer client reading an older service gets `Nothing`, and an older service never writes a charge, so nothing is derived as a subscription.
  - A state added later decodes as `SSUnknown` on this client and does not fail the response.
  - Apple claim refs change only for auto-renewable transactions, which no client hands over today (the apps finish them uncredited).
  - `docs/protocol/badges-rpc.md` and `badges-rpc.schema.json` gain the field, the `charge` credit's dating, and `issueBadge`'s renewal behaviour.
- **Database:** one new service migration with up and down, SQLite and Postgres, covered by the WebTests down/up cycle. No client migration. The client query in `getBadgePurchase` changes, so `chat_query_plans.txt` changes; only the user's full run regenerates it, since it is rewritten from every query a full run records.
- **Older and newer clients:** an older client never holds a subscription purchase, since no app sells one. An app downgraded to an older core retires a subscription badge at paidThrough and never asks again, so it loses every renewal the store keeps charging until revival (run 4) lands.
- **Adversary:**
  - Quota exhaustion: a client holding a subscription key repeats issue requests to make the service hammer the store's API quota, which every subscriber shares. For an existing purchase the store is asked only once coverage has ended, and at most once per subscription per hour, failures included; requests in between are answered from the stored state (AC6, AC8). On the purchase path there is no payment row to stamp: an honest client retries a period that has not started, or a pending payment, every 300 s only until it starts or settles, stops on a Play subscription whose state is `ended` (`receipt_invalid`), and retries every other case once a day; a client that ignores `retryAfter` can repeat a purchase as it can today (each attempt is one store call, as one-time Play purchases already are).
  - Replay: a charge is unique per `(payment, provider charge ref)` and granted only if no ledger row references it, in the transaction that re-reads the tip. A replay inside the issued period gets the cached credential (AC5).
  - Forged renewal evidence: none is accepted from a client. Renewals come only from the service's own store read, so a client cannot name a charge, a period or a tier.
  - Another key presenting the same subscription: the claim is the subscription's identity, so another key gets `receipt_used`, as today.
  - Store data out of range (period end not after start, unknown plan, another tier): not granted; a malformed period is a verifier failure, logged (AC7).
  - A dishonest service dating grants: grants are now legitimately dated up to about a month back, so the client's check accepts any date between the previous row's and now; it still flags a row dated before its predecessor (L:164). Accepted: the verdict is stored and nothing reads it.
  - Metadata: the zero-month request goes out at the same shared weekly moment as every renewal, so it adds no per-user timing. The service links a subscription's renewals to its purchase key, as it already links monthly issuance.
  - The Play token at rest: usable only with our service account's credentials, never logged or returned.
  - Resource use: each check is one store call of at most 10 s on the serial loop, bounded per subscription here. The aggregate load at the weekly moment is run 3's (§8).

## 6. Slices

Before slice 1, the user creates the branch on the host: `git switch -c f/badge-subscriptions badge-in-app-checkout`. The workspace checkout is on `f/badge-topups-plan`, which carries the top-up design document, and `/ship` commits on the current branch.

Every slice runs `cabal build` (project default flags, never `-O0`), then the scoped suites named in it, from `/workspace/simplex-chat-2`. Check the example counts, not only the exit status. hspec `-m` matches a substring of the example path, and several `-m` are alternatives. The new subscription tests go under a `describe "subscriptions"` inside `badgeServiceTests` (Bot:83-175), so `-m "SimpleX badge service e2e/subscriptions"` runs exactly them.

### Slice 1: name Apple claims by reference (structural)

- Files: `src/Simplex/Chat/PaymentService.hs`, `src/Simplex/Chat/Library/Commands.hs`, `apps/simplex-badge-service/src/BadgeService/StoreReceipts.hs`, `.../StoreReceipts/Mock.hs`.
- Change: rename `appleTransactionId` to `appleTransactionRef`, matching `googlePurchaseRef` and `StoreTransactionRef`; no behaviour change. Slice 3a makes it return a subscription's original id, which the old name would misstate.
- Tests: none.
- Verify: `cabal test simplex-chat-test --test-options='-m "Supporter badges"'`.
- Done when: it builds and the suite passes with the same example count.

### Slice 2: charge storage and the response field (structural)

- Files: `apps/simplex-badge-service/src/BadgeService/Store/{SQLite,Postgres}/Migrations.hs`, `.../BadgeService/{Store,Service}.hs`, `src/Simplex/Chat/Badges/{Ledger,Service}.hs`, `src/Simplex/Chat/Store/Badges.hs` (the fallback call), `src/Simplex/Chat/Store/{SQLite,Postgres}/Migrations/M20260915_user_badges.hs` (draft comment only), `tests/BadgeTests.hs`, `tests/Bots/BadgeService/WebTests.hs`.
- Change:
  - The `20261012_badge_subscriptions` migration (§4).
  - `appendLedgerPlan` writes `charge_id` for an `SCCharge` row; the ledger tip and statement reads select it; `entryTypeFromColumns` gains the charge id; the client's fallback call passes `Nothing`.
  - `SubscriptionState` and the `subscription` field; `credentialResponse` takes it, and every caller passes `Nothing`.
  - Nothing writes a charge or a state yet, so no behaviour changes.
- Tests: `testEntryTypeColumns` asserts the `SCCharge` round trip with a charge id and `Nothing` without one; JSON tests for the field (present, absent, unknown state); `testServiceColumns` covers the new and dropped columns; the down/up cycle covers the migration.
- Verify: `cabal test simplex-chat-test --test-options='-m "Supporter badges" -m "SimpleX badge service"'` (this also runs the e2e suite).
- Done when: the suites pass, the down/up cycle includes the new migration, and AC10 and AC12 hold.

### Slice 3a: the charge planner and Apple subscription claims (behavioural, pure)

- Files: `src/Simplex/Chat/Badges/Ledger.hs`, `src/Simplex/Chat/PaymentService.hs`, `tests/BadgeTests.hs`; the comments at SR:31 and T:288 that call Apple's ref the transaction id.
- Change: the planner; `appleTransactionRef` returns the original id for auto-renewable transactions.
- Tests: `testChargeRowsVerify` (AC9, every case listed there); `testAppleTransactionRef` (subscription, consumable, missing type).
- Verify: `cabal test simplex-chat-test --test-options='-m "Supporter badges"'`.
- Done when: AC9 and the AC11 unit test hold. This slice tests the riskiest assumption (§8) before any service code depends on it.

### Slice 3b: credit a subscription purchase (behavioural)

- Files: `apps/simplex-badge-service/src/BadgeService/{Catalog,StoreReceipts,Service,Store}.hs`, `tests/Bots/BadgeService/{FakeStore,BotTests}.hs`, `tests/BadgeTests.hs` (`uncalledVerifier` at :904 builds `StoreVerifier` directly and needs the new field), `docs/protocol/badges-rpc.md`, `docs/protocol/badges-rpc.schema.json`.
- Change: catalog `StoreProduct` sum and `storePlan`; `checkSubscription`, `SubscriptionRef`, `SubscriptionCheck`, `StoreCharge`; the purchase branch for subscription products; replays carry the state.
- FakeStore gains subscriptions: Google keyed by token and Apple keyed by original id, each with a mutable state and latest charge, and a check counter. Its Apple subscription fixtures (`unsignedJWS` with `type` and `originalTransactionId`) vouch with the original id, which is the claim `appleTransactionRef` reads.
- Tests: `testSubscriptionPurchase` (AC1, plus the no-grantable-charge answers of §4, each writing nothing: a period not started yet answered `payment_pending` with 300 s; a Play subscription whose state is `ended` answered `receipt_invalid`; a Play subscription whose latest charge is revoked while its state is `renewing`, and an Apple subscription whose state is `ended`, each answered `payment_pending` with a day); `testStoreUnknownProduct` keeps refusing unknown products and drops its subscription case.
- Verify: `cabal test simplex-chat-test --test-options='-m "SimpleX badge service e2e/store purchases" -m "SimpleX badge service e2e/subscriptions"'`.
- Done when: AC1 holds and the store purchase examples still pass.

### Slice 4: renew once coverage ends, on the service (behavioural)

- Files: `apps/simplex-badge-service/src/BadgeService/{Service,Store}.hs`, `tests/Bots/BadgeService/{BotTests,FakeStore}.hs`, `docs/protocol/badges-rpc.md`.
- Change: the renewal step in `issueBadgeCmd`, the check interval stamped on every attempt, the grantability rule, the one-transaction write that answers `internal` when the tip moved, answers through `issueResponse`.
- Tests (service, through `serviceCmd` with the shared test clock): `testSubscriptionChargeGrantedOnce` (AC5, with the replay inside the issued period), `testSubscriptionStoreChecksBounded` (AC6: coverage running makes no check; two requests after coverage ended within the hour make one; a failed check also holds the next for the hour), `testSubscriptionChargeNotGranted` (AC7: refunded, other tier, period not started, period ended), `testSubscriptionStoreUnreachable` (AC8).
- Verify: `cabal test simplex-chat-test --test-options='-m "SimpleX badge service e2e"'`.
- Done when: AC5-AC8 hold and the existing e2e examples still pass.

### Slice 5: keep asking at zero months, on the client (behavioural)

- Files: `src/Simplex/Chat/Store/Badges.hs`, `src/Simplex/Chat/Library/Commands.hs`, `tests/Bots/BadgeService/BotTests.hs`.
- Change: the purchase kind on `UserBadgePurchase`; `badgeRequestDue` at zero months for a subscription; retirement on `SSEnded` after the request; the "issued no credential" check on owed months; no support-ended alert while a subscription is shown.
- Tests (real client and service, test clock, `renewalMoments`/`armWakeAt`): `testSubscriptionRenews` (AC2), `testSubscriptionAwaitsCharge` (AC3), `testSubscriptionEnds` (AC4), `testAppleRenewalResolvesToPurchase` (AC11: two Apple JWS with one original id and two transaction ids resolve to one receipt record and one purchase).
- Verify: `cabal test simplex-chat-test --test-options='-m "SimpleX badge service e2e"'`, then `--test-options='-m "Supporter badges"'`. `chat_query_plans.txt` is left to the user's full run (§7); a scoped run must not be used to regenerate it.
- Done when: AC2-AC4, AC11 and AC13 hold.

## 7. Test plan

- New tests and what each proves: listed per acceptance criterion in §1 and per slice in §6. The pure planner test (AC9) proves the dating choice against the client's own check. The service tests prove grant-once, replays, bounded store use and the refusal classes. The client tests prove the badge stays on across paidThrough and comes off only on `ended`.
- Existing tests that must stay green: "Supporter badges" (ledger transitions, statement checks, JSON, store purchases, request loop), "SimpleX badge service" (config, web, catalog, schema cycle), "SimpleX badge service e2e" (renewals, lapses, retirement, alerts, store purchases, receipt worker, `testNoCredentialMonthsRanOut` and `testNoCredentialMonthsLeft` for the changed check). `testStoreUnknownProduct` changes as described.
- Full-suite check (the user's):
  - `cabal test simplex-chat-test`, which also rewrites `src/Simplex/Chat/Store/SQLite/Migrations/chat_query_plans.txt` for the changed purchase query.
  - On the host, a Postgres build of the service, `cabal build --flags=+client_postgres exe:simplex-badge-service`, and a Postgres run of `cabal test --flags=+client_postgres simplex-chat-test --test-options='-m "badge service schema"'`, which executes the new migration's Postgres SQL. The deployed service is Postgres-only, and CI tests SQLite only.

## 8. Risks

- **Riskiest assumption, and how the first slice tests it early:** that dating every charge grant to the store's period start keeps `createdAt` non-decreasing and every row verifiable on all renewal paths, including annual renewals and recovery after billing retry or grace. Slice 3a's `testChargeRowsVerify` runs the planner's rows through the client's own `balanceChecked` before any service code depends on them. A failure there changes the design (to an explicit period field) before slices 3b-5 are built.
- **External, settled in run 2:** that the stores give the service the latest paid period of a subscription by its identity, with a stable per-charge id, and that Apple renewals keep `originalTransactionId`. Apple's "Get All Subscription Statuses" returns the latest signed transaction with `purchaseDate` and `expiresDate`. Play's `subscriptionsv2` gives `expiryTime`, the base plan and `latestSuccessfulOrderId`, but no current-period start; run 2 derives it as `expiryTime` minus the plan's period (recommended) or reads it from the order (one more call). If the stores do not give these, `SubscriptionCheck` changes; the planner and client logic do not. Run 2 also confirms that Play issues a new token when a subscription is bought again after it expired: Q7's terminal answer for an ended Play subscription depends on it, and if Play reused the token, that answer must become the day-long `payment_pending`.
- **Load:** every subscriber renewing in a week asks at the same instant (C:5696-5698), and each check is up to 10 s on the serial loop against a 30 s client deadline. Until run 3 spreads requests and moves store calls off the loop, a large renewal cohort times out and retries.
- **The derived purchase kind:** a purchase is a subscription exactly when its replica holds a charge credit. A future re-bind, transfer or `opening` restatement (which the protocol specifies for new devices) must keep a charge credit in the replica, for example by crediting transferred months as a charge, or the client would retire a live subscription at paidThrough.
- **One `ended` answer is final:** the client retires on the first `ended` it receives at or after paidThrough, and a retired purchase is not asked about again until run 4. If a store briefly reports a subscription as expired around a renewal (unverified), the renewals after it are lost until run 4 lands.
- **Blocking sale, not this run:** runs 3 (load), 4 (revival: an Apple resubscription after retirement resolves to the old credited record and is credited to nothing), 5 (tier change: iOS lets the user switch tier from Settings, and the new tier's charges are recorded and never granted), and the refund and revocation part of run 6 (a refunded annual keeps issuing for up to 11 months, since the store is not read while coverage runs). All four are money bugs if subscriptions sell without them.

## 9. Open questions

1. When does the app ask at zero months? Recommended: at the shared weekly moment, a day before the shown credential's shared expiry, as every renewal does today; it already falls on or after paidThrough. If instead at the store's renewal time (model line 127): requests get a per-user time, against objectives 3 and 4, and `badgeBoundary` gains a subscription moment.
2. What happens while the store retries a failed charge? Recommended: keep the badge, ask once a day, issue no month without a charge, and accept for now that the badge stays on the profile after its credential expires. Recipients show it expired a week after expiry, as for a prepaid badge whose app did not run. The stores bound the wait themselves (Apple retries up to 60 days, Play holds up to 30, unverified), after which the service answers `ended` and the badge retires. Run 7 adds the payment-problem alert and UI. Configure store grace at about a week, which the last credential plus recipients' grace cover. If instead the badge must come off once its credential has expired: retirement moves to credential expiry plus 7 days, and a charge recovered after that needs run 4's revival.
3. Where does the work land? Recommended: a new feature branch `f/badge-subscriptions` from `badge-in-app-checkout`, holding all subscription runs, with its own service migration `20261012_badge_subscriptions` that later runs edit in place until release. If on `f/badge-topups-plan`: the top-up design document ships in this PR. If folded into `badge-in-app-checkout`'s own `20260925` migration: it merges with the one-time lane in one step, and this run depends on that branch's merge order.
4. Who runs cabal during `/ship`? Your recorded preference (since 2026-10-01) is that you run builds and tests, and a brief's instruction does not override that. Recommended for this run: let the implementer build and run each slice's scoped SQLite suites (§6), because the review loop checks compiled code, and you run the full suite and the Postgres checks (§7). If you keep running all builds yourself: §6's Verify lines become your checklist, and `/ship` reports each slice as unbuilt.
5. Subscription upgrades: same purchase key or a new one? This run only refuses to grant another tier's charge; run 5 builds the change. Recommended: same key, as `badges-rpc.md:50-55` says, with a tier-changing grant, since the stores link a subscription's renewals anyway and a new key buys no privacy. If a new key (model §2.10): every upgrade becomes a new purchase, and the old one must be ended explicitly.

6. Family Sharing for the subscription products? It cannot be turned off once on, and each family member's device would hold its own transaction and claim its own purchase (unverified). Recommended: off, and run 2 refuses `FAMILY_SHARED` transactions as `receipt_invalid`. If on: family members get the badge, and run 2 must decide which original id each one claims by.

7. What does the purchase path answer when a subscription has no grantable charge? Each held receipt is retried at the answer's `retryAfter` with no cap on attempts (C:5524-5541), and each attempt is one store call. Recommended:
   - Period not started yet (clock skew, or run 2's derived Play start landing just after the purchase), or the store answers pending: `payment_pending` with today's 300 s, so a buyer who just paid is not left a day.
   - Play, with the subscription's state `ended`: `receipt_invalid`, terminal, because a resubscription after expiry gets a new token (unverified, run 2 confirms), so this token can never be credited again. Only the state decides this: a revoked charge on a subscription that is still renewing is not final.
   - Everything else not grantable, on either store, including an Apple subscription whose state is `ended`: `payment_pending` with a day, because a later charge on the same token or an Apple resubscription under the same original id can still be credited from this record.

   §4 is written to this answer. If instead the same day-long `payment_pending` for every case: simpler, but the buyer can wait a day after a clock-skewed first purchase, and an ended Play token is retried daily forever. If instead a cap (for example 30 daily attempts): bounded cost, but a late Apple resubscription is missed after the cap.

Expected store mappings for run 2 (unverified, recorded so this run's types fit them): Apple status 1 → renewing or cancelled by `autoRenewStatus`, 2 → ended, 3 (billing retry) → billingRetry, 4 (grace) → billingRetry, 5 (revoked) → ended. Play `ACTIVE` → renewing or cancelled by `autoRenewingPlan.autoRenewEnabled`, `IN_GRACE_PERIOD` and `ON_HOLD` → billingRetry, `CANCELED` → cancelled until `expiryTime`, `EXPIRED` and `PENDING_PURCHASE_CANCELED` → ended, `PENDING` → `payment_pending`; `PAUSED` has no mapping, so subscription pausing stays off in Play Console (§11). A revoked charge: Apple's latest transaction carrying `revocationDate`; Play's latest order voided (Voided Purchases API or RTDN). Apple `inAppOwnershipType = FAMILY_SHARED` transactions are refused while Family Sharing is off (Q6).

## 10. Out of scope (noticed, not doing)

- `writeIssued` answers a skipped issuance as success (SVC:528-533). Harmless for issuance; only the charge path changes here.
- The prepaid-ending alert TODO (C:5587-5589).
- Unused drafts: `PaymentTerm`/`PTSubscription` (`PaymentService/Types.hs:165-172`), `BadgeCharge` and `BadgePlan` (T:71-73, :186-198), `BSCUpgradeBadgeSubscription`'s `badgeRequest` (BS:95).
- vb's Apple verifier checks no certificate validity dates or revocation.
- A Play subscription purchase is offered by the first offer on its base plan, not a named offer (`PlayStore.kt:213`).
- Retirement does not signal the receipt worker (C:5827-5834).
- After a renewal that lands once the shown credential has expired (billing-retry recovery), the request and the profile update happen in the same pass, because `presentIssuedBadge` presents as soon as the shown credential has expired (C:5811-5824). This already happens to late prepaid renewals; it weakens objective 3 (`badges-overview.md:205`) for recovered subscriptions.

## 11. Follow-up runs

Runs marked **(blocks sale)** must land before subscriptions are offered in either app.

2. Store reads: an App Store Server API client (in-app purchase key config, ES256 JWT, "Get All Subscription Statuses", verification of the signed transaction and renewal info with vb's pinned chain) and Play `subscriptionsv2.get` plus subscription acknowledgement. vb's Apple verifier derives its verified ref through `appleTransactionRef`, as the mock does. Trial and introductory phases are refused, not granted. Revoked charges are reported as `revoked` (Apple `revocationDate`; Play voided orders). `FAMILY_SHARED` transactions are refused (Q6). Confirm that Play issues a new token on resubscription after expiry (§8, Q7). FakePlay and a fake App Store API. After vb merges.
3. **(blocks sale)** Load and timing: spread requests across the lead day (the C:5696-5698 TODO), drop queued requests older than the client deadline, take store calls off the serial loop behind a Postgres row lock on the purchase.
4. **(blocks sale)** Revival and restore: a resubscription after retirement and a new device's restore re-bind the subscription to the presenting key and show it again (model § recovery, "re-bind, capped per payment and period").
5. **(blocks sale)** Tier changes: the tier-changing grant for Apple sheet-led upgrades and downgrades and Play replacements, and the client updating `current_badge_type`.
6. Store notifications and refunds: App Store Server Notifications V2 and Play RTDN updating charges and state between requests (a second writer, after run 3's lock). **(blocks sale)** the refund and revocation debit, per charge, so a refunded annual stops issuing.
7. iOS app: sell monthly and annual, hand subscription transactions over instead of finishing them, "Renews"/"Ends"/"Payment problem" on Your badge, the manage-subscriptions sheet, Restore purchases, the subscription alerts, disclosures, dev mock subscriptions. A subscription receipt must not wait behind a shown badge while the store charges: refuse code redemption while the profile has an open store purchase (`getOpenStorePurchases`, SB:203-216), or hand subscription receipts over regardless of a shown badge. A held subscription record adopts the newest JWS (SB:136); if that one is a revoked renewal, its terminal refusal must not refuse the whole subscription. Adopting a newer transaction should also make the record due now, since SB:136 replaces the payment but not `next_attempt_at`, so a resubscription otherwise waits for the attempt already scheduled (up to a day).
8. Kotlin port of run 7, plus the Play sweep querying subscriptions and offer selection by base plan.

External waits before sale (not runs):

- The App Store Connect subscription group and products; Play subscription products with grace and account hold, and subscription pausing off.
- No trial or introductory offers on any subscription plan.
- The Family Sharing setting (Q6), which cannot be undone once on.
- The subscription products stay unbuyable (not live, or not offered by any app build) until sale: today's apps finish a subscription transaction without crediting it, so a buyer would pay and get nothing.
- An App Store Server API key; a Play service account with order permissions; notification endpoints.
- Store API quotas for the expected renewal checks, raised if needed.
- Store review of the subscription screens, including how review purchases are handled: Sandbox and licence-tester purchases are refused (AC1), the same open question as the one-time lane's.
- Terms and privacy links; translations.
