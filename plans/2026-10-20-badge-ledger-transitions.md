# Plan: badge ledger transitions — store tier changes and restoring a badge on a new device or profile

Base branch: `f/badge-subscriptions` at 26f846e1a · Repos: simplex-chat · Estimated diff: ~2,000 changed lines in three PRs (PR1 ~550, PR2 ~600, PR3 ~850), plus later work for receipts (~700)

Line numbers are at 26f846e1a. `SVC/` is `apps/simplex-badge-service/src/BadgeService/`; `Commands.hs` is `src/Simplex/Chat/Library/Commands.hs`; `Ledger.hs` is `src/Simplex/Chat/Badges/Ledger.hs`; `BotTests.hs` is `tests/Bots/BadgeService/BotTests.hs`. "The model" is `plans/2026-07-30-supporter-badges-v3-ux.md`.

## 1. Goal and acceptance criteria

Two things the model promises (§2.10, §3 "Recovery and transfer") and the code cannot do, both because the ledger cannot record months moving between tiers or between purchase keys:

- **Tier change.** A Supporter↔Legend change made in the App Store's own subscription settings (all four subscription products share one subscription group, so the original transaction id stays the same) is granted at the tier the store charged, instead of being recorded and not granted.
- **Restore.** A store subscription bought on one install or profile is moved, by an explicit Restore, to a new purchase key on another install or profile, capped per payment and month. A receipt path for Stripe and crypto payments is designed here (§4.4) and built later.

Acceptance criteria (tests named per slice):

- AC1 (L1): the service stores and re-reads `transferIn`, `transferOut` and `upgrade` entries with their purchase keys, so a statement carrying one is re-emitted; `testEntryTypeColumns` round-trips all three from columns; `testTransferEntriesRoundTrip`; schema down/up.
- AC2 (L2): `upgrade` and `transferOut` debits re-run on the client and verify `Just True`; a grant of another tier verifies only onto an empty balance and restarts the run; `testTierChangeRowsVerify`, `testDebitRowsVerify`.
- AC3 (T1): at a renewal check after coverage ended, a store charge of the other tier is granted at its period start, the next credential carries the new tier, the purchase's tier follows on both sides, and every row verifies; `testSubscriptionTierChangeAtRenewal`.
- AC4 (T2): `upgradeBadgeSubscription` from the purchase's own key, naming its own subscription, asks the store at once (hourly bound shared with renewal checks) and grants an immediate other-tier charge during coverage: an `upgrade` debit of the unissued months, then the new tier's charge, then the month issued now; within the hour, or while the store still reports the old tier, it answers `payment_pending`, never the old tier's credential; another key or another subscription is refused; `testSubscriptionUpgradeNow`, `testUpgradeRequestRefused`.
- AC5 (T3): an Apple subscription transaction handed over for a credited, shown purchase whose product is of another tier is held; the receipt worker sends `upgradeBadgeSubscription`, presents the new tier's credential at once and updates `BadgeState.badgeType`; while the store still reports the old tier the record stays held and retries; past `tierChangeWindow` it settles with no change; a renewal of the same tier sends nothing; `testAppleTierChangeHandOver`, `testAppleTierChangeStoreLags`.
- AC6 (R1a, R1b): `transferBadge` with a store receipt credited to another key moves the payment to the new key: the old ledger ends with `transferOut {toPurchaseKey}`, the new purchase is funded by the store's current charge and issued the running month; the same transfer re-sent is answered with the new key's credential; a second transfer of the same payment within a month is answered `transfer_limited` with `retryAfter`; the old key's next `issueBadge` is answered `subscription = ended` and no credential; `purchaseBadge` from another key still gets `receipt_used`; `testStoreRebind`, `testStoreRebindCapped`, `testStoreRebindOldKeyEnded`, `testTransferReplayAnswered`.
- AC7 (R2): Restore on a new install, and on another profile of the same install, re-binds the subscription to that profile and shows it; the old profile's badge retires at its next request; `testRestoreOnNewDevice`, `testRestoreOnOtherProfile`, `testRestoreAfterUnfinishedPass`.
- AC8 (R3): both apps' Restore hands over the store's current entitlements with the restore origin and explains `transfer_limited`; both `gaps.md` entries updated; checked by building both apps.

## 2. Non-goals

- The iOS `subscribedBefore` gate and the same-install resume of a retired Apple subscription, which are planned separately (§3, "Work this plan builds on").
- Tools for watching a renewal by hand (a dev flag that asks the store during coverage, a dev-only request command).
- Play tier changes: Play sells each tier as its own subscription product (`BadgeStore.kt:22-49`, `SVC/Catalog.hs:166-176`), the apps offer no replacement flow, and `linkedPurchaseToken` is parsed nowhere, so a Play tier change can only be a new token, i.e. a new purchase.
- Store notifications (App Store Server Notifications, Play RTDN), refund handling, top-up, pause.
- Prepaid (code, web) upgrades with a conversion credit (`getBadgeInvoice.upgrade`, model §2.10 prepaid).
- Building the receipt path (§4.4).

## 3. Current state

**The ledger cannot hold entries that name another purchase.**

- The service ledger has `payment_id` and a UNIQUE `charge_id` as its only reference columns (service migrations `20260925_badge_store_receipts`, `20261012_badge_subscriptions`, `SVC/Store/SQLite/Migrations.hs:164-229`). `appendLedgerPlan` writes only the tag of `transferIn`, `transferOut` and `upgrade` (`SVC/Store.hs:318-353`, TODO at `:319`), and `entryTypeFromColumns` cannot rebuild them (`Ledger.hs:220-244`, TODO at `:224`).
- One unreadable row fails every later answer of that purchase: `getLedgerEntries` is `mapM toEntry` (`SVC/Store.hs:282-300`), so the statement is `Nothing` and `creditedResponse`, `firstMonthResponse` and `issueResponse` answer `internal` (`SVC/Service.hs:673, 700, 790`); an unreadable tip reads as no ledger at all (`SVC/Store.hs:259`).
- The client stores each entry's type as JSON (`entry_type_value`) and reads that first (`src/Simplex/Chat/Store/Badges.hs:551-579, 613-616`), so it needs no reference column.
- The client's check (`balanceChecked`/`entryChecked`, `Ledger.hs:139-183`) re-runs lapse and badge debits, treats refund, upgrade, transferOut and support debits only as "uncontradicted", re-runs every other credit through `grantEntry`, which keeps the previous entry's tier, and checks an opening against the purchase's tier.

**The tier is fixed per purchase on both sides.**

- The client writes `current_badge_type` once, in `createStashBadgePurchase` (`Store/Badges.hs:330-341`), and `BadgeState.badgeType` reads it (`Commands.hs:5814-5823`).
- The service reads `current_badge_type` as the ledger's tier in `renewSubscription` (`SVC/Service.hs:736, 744`) and refuses another tier in `grantableCharge` (`NGOtherTier`, `:631`). Every ledger transition keeps the previous entry's `balanceBadgeType` (`Ledger.hs:76-117`), and the credential is signed for the issued entry's `balanceBadgeType` (`credentialForEntry`, `SVC/Service.hs:437-440`).
- An Apple tier change reaches the service only at the next renewal check after coverage ended (`SVC/Service.hs:711-712`), where it is recorded and not granted. On the device, core resolves the new transaction by its original id to the credited record and answers the old credential without asking the service (`Commands.hs:5377-5380`), and the app finishes the transaction (`BadgeStore.swift:260-268`).
- The Apple verifier reads the latest transaction's `productId`, `purchaseDate` and `expiresDate` from the status answer (`SVC/StoreReceipts/Apple.hs:297-335`); renewal info is parsed only for `originalTransactionId` and `autoRenewStatus` (`:394-397`). Per Apple's documentation (not verified here): an upgrade within a group is immediate, the old plan's unused time is refunded pro rata, and a new transaction for the new product starts under the same original id; a downgrade takes effect at the next renewal; a crossgrade of the same duration is immediate and of another duration waits for renewal.
- `upgradeBadgeSubscription` exists on the wire, unimplemented: `BSCUpgradeBadgeSubscription {badgeRequest, payment, balance}` (`src/Simplex/Chat/Badges/Service.hs:95-99`), with a TODO to drop `badgeRequest`; the service answers it `unsupported_version` (`SVC/Service.hs:422-428`).

**A store payment belongs to one purchase key for its whole life.**

- Claims are by `payments(provider, provider_ref)`; a purchase holds one payment (`badge_purchases.payment_id`, UNIQUE). Another key presenting a credited receipt gets `receipt_used`, only after the store vouched (`SVC/Service.hs:516-519, 555-557, 666-671`), and the client treats it as terminal (`Commands.hs:5409-5414`).
- On the client a store receipt record is install-wide, `badge_store_receipts(provider, transaction_ref)`, owned by one profile and linked from one purchase (`badge_purchases.badge_store_receipt_id`, UNIQUE).
- The client derives a purchase's kind from its replica: `PKSubscription` exactly when the replica holds a `charge` credit, with `planMonths` from the newest one (`Store/Badges.hs:464-492`). A subscription whose new ledger had no `charge` credit would read as prepaid and be retired at paidThrough without asking.
- Restore today: iOS `restorePurchases` reads `Transaction.unfinished` after `AppStore.sync()` (`BadgeStore.swift:410-431`); Kotlin reads Play's list (`BadgeStore.kt:439-464`); both comments and both `gaps.md` entries record that a new install or profile is answered `receipt_used`.
- No receipt code exists: the credential response's `receipt` is always absent (`SVC/Service.hs:445-447`), the client stores none, and web payments end as single-use redeem codes whose hash the service holds (`SVC/Orders.hs:62-117`).

**Work this plan builds on.**

- A subscription badge retires at once when its store says it ended, from the service's answer or the device's report (commit 55d69781e).
- The same-install resume of a retired Apple subscription, planned separately and being built: it splits the badge request's lock from its body (`sendBadgeIssue`), re-holds a credited record when a later charge is handed over (`reholdStoreReceipt`), and bounds the hold by `resumeWindow`, one day around the charge's purchase date. T3 and R2 reuse its re-hold and its request body.

**Invariants kept:** every row is its operation re-run at its `createdAt`, and `createdAt` never decreases; lapse before grant; no row or issuance before the credential is signed; a refusal writes no ledger row; the tier comes from what the store charged, never from the client; claims are by the store's reference; one payment funds one purchase at a time. One changes: the model's "`start` is non-decreasing" holds within a tier, and a tier change restarts the run at its own time.

## 4. Design

### 4.1 The shared ledger work

The core change: a ledger entry may name another purchase (`transferIn {fromPurchaseKey}`, `transferOut {toPurchaseKey}`, `upgrade {toPurchaseKey}`), and a grant may change the balance's tier. Both sides must be able to store, re-read and re-check such rows.

- **Service storage (L1).** Two reference columns, as `appendLedgerPlan`'s TODO names them: `badge_ledger.from_purchase_id` and `to_purchase_id`, each `REFERENCES badge_purchases`. `appendLedgerPlan` writes them from the entry's key (`(SELECT badge_purchase_id FROM badge_purchases WHERE purchase_key = ?)`); `getLedgerTip` and `getLedgerEntries` left-join `badge_purchases` twice to read the keys back. `entryTypeFromColumns` takes its references as one record (payment, charge id, from key, to key; the positional list would reach seven arguments) and rebuilds `SCTransferIn`, `SDTransferOut` and `SDUpgrade`. The client's fallback call passes no references, as today.
- **Transitions (L2, `Ledger.hs`).**
  - `debitEntry t entryId debit e`: the model's `debit reason` — `changeMonths = -balanceMonths`, `balanceMonths = 0`, start and anchor unchanged. Used for `upgrade` and `transferOut`.
  - `grantEntry` takes the grant's tier. A grant of the balance's tier behaves exactly as today. A grant of another tier restarts the run at `t` with that tier (`balanceMonths = n`, `balanceStartTs = balanceAnchorTs = t`, `balanceBadgeType = tier`); the service makes one only onto an empty balance, which an `upgrade` debit ensures.
  - `chargeEntries` takes the charge's tier and the purchase's own key. For the tip's tier it is unchanged. For another tier, at `t = max periodStart (createdAt tip)`: lapse to `t`, then `debitEntry t (SDUpgrade ownKey)` if months remain, then the grant of the new tier at `t`, then lapse to now; the caller issues at now as today. `t` is clamped to the tip so `createdAt` never decreases when the store check comes after a month was already issued.
  - `entryChecked` re-runs `debitEntry` for `upgrade` and `transferOut` (so they can verify `Just True`), re-runs a credit's grant with the row's own tier, and marks `Just False` a credit whose tier differs from its predecessor's while the predecessor still held months. The opening check compares with the purchase's current tier (the caller passes `current_badge_type`, which T1 and T3 keep up to date).
- **Why `upgrade {toPurchaseKey}` names its own key for a store change:** the protocol keeps a store subscription on one key (`docs/protocol/badges-rpc.md:68`); the field then marks an in-place conversion, and needs no new constructor or wire change.

### 4.2 Tier changes

**Ledger entries, and when they are written.**

- Downgrade (Legend → Supporter), and any change that takes effect at renewal: at the first renewal check after coverage ended (`renewSubscription`), the store's latest charge is of the other tier. The balance is empty (the old plan's months were issued or lapsed), so the rows are `lapse` (if any), `charge` credit of the new tier at the period start (restarting the run), `lapse` to now (if any), `badge` debit for the month issued now.
- Upgrade (Supporter → Legend), immediate on Apple, with the old plan's unused time refunded pro rata by Apple: when the service next asks the store — at once through `upgradeBadgeSubscription` (T2, T3), or at the next renewal check — the rows are `lapse` to `t`, `upgrade {own key}` debit of the old tier's unissued months (an annual plan's remaining months; none for a monthly one), `charge` credit of the new tier at `t`, `lapse`, `badge`. The old tier's credential already issued stays valid to its expiry, as every issued credential does (model O6); the profile switches to the new one at once.
- Crossgrades within a tier (month ↔ year) need nothing new: `storePlan` gives the months, and the tier does not change.
- An annual old plan's upgrade found only by the renewal backstop (the app did not hand it over in time) is found after the old coverage ended, so nothing is left to debit: the user keeps the old tier's months Apple refunded and gets the new tier's months from the clamped `t`, late but in full. This is bounded and happens only when T3 missed the change; refund handling can later reclaim the refunded months.

**How the service decides the client is allowed.** The tier is what the store charged: the service grants only the plan named by the latest charge in the store's own answer (Apple's App Store Server API verified against the pinned root; never the JWS the app sent). `upgradeBadgeSubscription` must be signed by the purchase key, and its evidence must claim the purchase's own payment (`storeTransactionRef` equal to the payment's `provider_ref`); it only makes the service ask now instead of at the next renewal. `NGOtherTier` is removed from `grantableCharge`, which loses its tier argument.

**Caps.** `upgradeBadgeSubscription` asks the store at most once an hour per subscription, sharing `subscription_checked_at` with renewal checks, failed attempts included, which protects the store quota every subscriber shares. A charge is granted at most once per purchase (the charge reference), so repeats add nothing. Its answers: a credential of the presented transaction's tier once that tier's charge is granted (now, or by an earlier request); otherwise — the hour still holds, or the store's latest charge is not yet of the presented product's tier (Apple's status lagging the device) — `payment_pending` with `retryAfter`, never the old tier's credential, so the client cannot mistake it for success. The presented product's tier comes from the JWS verified offline (`storePlan` of its `productId`); it only decides what to wait for, never what is granted. A new-product transaction exists only once the change is in effect (Apple creates it at once for an upgrade and at renewal for a downgrade), so this path has no "deferred" answer.

**Client core (T3).** `AppleTransactionFacts` gains `productId`. `storePlan` (Apple product id or Play base plan → tier and months) moves from `SVC/Catalog.hs` to shared core code (structural slice S0), so core and service read one mapping. Nothing is sent from the hand-over itself, whose contract is that nothing is sent there (`Commands.hs:5361-5362`); T3 uses the resume's re-hold instead:

- In `handOverStoreReceipt`'s credited branch, an Apple auto-renewable transaction whose product's tier differs from the purchase's `current_badge_type`, for a purchase that is its owner's newest and shown, and whose `purchaseDate` is within `tierChangeWindow` of the client clock (one day either side, as `resumeWindow`), re-holds the record (`reholdStoreReceipt`) with the hold kind "tier change". The app gets `held` and keeps the transaction unfinished.
- The receipt worker takes a tier-change record even while its own purchase is shown (other held records still wait for retirement), checks the hold kind before the resume's "purchase already shown → settle credited" branch, and sends `upgradeBadgeSubscription {payment, balance}` through `sendBadgeIssue`'s body.
- A credential of the transaction's tier settles the record credited, stores the statement, sets `current_badge_type` from the tip's `balanceBadgeType`, and presents the credential at once. Only this path presents at once; renewals from the badge worker keep `presentIssuedBadge`'s hold, so the renewal request and the profile update stay on different days (design objective 3, `docs/protocol/badges-overview.md:205`).
- `payment_pending` and transient failures retry on the worker's schedule (the hour at most). Past the window, or on a terminal answer, the record settles credited with no change, the app finishes the transaction, and T1's renewal backstop grants the tier at the next check after coverage ends.
- The hold kind is a client column, `badge_store_receipts.hold_kind` (purchase, transfer, tier change; default purchase), added in T3 and reused by R2. Any other hand-over of a credited record is answered as today; a retired purchase's later charge belongs to the resume.

**What the apps must show.** Your badge already shows `badgeState.badgeType` and prices "Renews {date} · {price}" by that tier and `planMonths` (`BadgesYourBadgeView.swift:38-63`, and its Kotlin mirror); both follow once core updates `current_badge_type`. The new tier's badge is presented at once. No new screen or alert is needed to stop charging for nothing; an optional iOS notice, "Changes to Supporter on {date}", can show a pending downgrade from StoreKit's renewal info (T4).

**What the service's statement reader needs.** `upgrade` rebuilt from `to_purchase_id` (L1).

**Migrations.** L1's service columns; the client column `badge_store_receipts.hold_kind` (T3). `current_badge_type` exists on both sides and is now updated.

**Threat model.**

- A tier change the user did not intend (a mis-tap in App Store settings, someone else using the Apple ID): the service grants exactly what the store charged, so the user always holds the tier they pay for, and the store's own controls undo it (change back, Apple refund). The apps never offer a tier change, so the app cannot cause one.
- A client claiming a tier it did not pay for: the JWS only names the subscription and what to wait for; the tier granted comes from the store's verified answer. A forged or other-subscription JWS is refused before the store is asked.
- Replayed `upgradeBadgeSubscription`: inside the hour answered `payment_pending` or with the granted credential; a charge is granted once.
- A dishonest or buggy service dating a tier-change grant: the client re-runs it; a tier switch over a non-empty balance is marked false.
- Metadata: an upgrade request follows the user's own store action, as a purchase does, so its timing reveals no more than a purchase; a downgrade's hand-over follows the renewal moment, a per-user time, but only once per change. Renewals of the same tier send nothing from the hand-over and still go out at the shared weekly moment.
- Refunds (later work): an upgrade's pro-rata refund is already accounted for by the `upgrade` debit, so refund handling must not debit the upgraded-from transaction again (Apple marks it `isUpgraded`).

### 4.3 Restore on a new device or profile, for store subscriptions

**Ledger entries, and when they are written.** In one service transaction, when `transferBadge` from a new key K2 presents a store receipt whose payment P is credited to K1:

- K1: `lapse` to now (if any), then `transferOut {toPurchaseKey = K2}` debit of its unissued months. It is written even with zero months, as K1's marker.
- K2 (a new purchase): the rows a first subscription purchase of P would hold now, funded by the store itself: `chargeEntries` from `emptyEntry periodStart tier` over the store's current charge (a `charge` credit at its period start, lapses, the month issued now), referencing the existing charge row. K2 gets no `transferIn`: the store funds it, and the client derives the subscription and its plan from that `charge` credit (§3), which a `transferIn` would lose, retiring the badge at paidThrough without ever asking for a renewal.
- P's binding moves: `badge_purchases.payment_id` cleared on K1 and set on K2; `payments.rebound_at = now`; logged with both purchase ids.
- K2's credential re-issues the running month, which K1 also holds until its expiry: that duplicate is what the cap bounds. Unissued months move once (K1's debit, K2's grant from the same charge), so no month is minted beyond the running one.
- Store one-time months are not re-bound: their month is issued at credit, Play consumes a finished one-time purchase so its list never shows it, and `currentEntitlements` excludes consumables, so no app path presents one. `transferBadge` answers a one-time receipt credited elsewhere `receipt_used`, as `purchaseBadge` does.

**How the service decides the client is allowed.** The store receipt is the authority (model decision 10, "stores re-bind via the store account"):

- The store must vouch for it now — Apple's JWS verified offline and the subscription's status read by original id; Play's subscription read — through the purchase path's existing checks and hourly per-claim bound.
- For Apple, the presented transaction must be the store's latest for that subscription (its `transactionId` equals the status answer's latest), so a leaked old JWS stops working at the next renewal. This is defence in depth, not the authority.
- The grant rule is the first purchase's (`grantableCharge` and `fundsMonthNow`): a subscription that has ended, is in billing retry with no running period, or whose charge is revoked or in an offer re-binds nothing and is answered as a first purchase would be.
- `purchaseBadge` keeps answering another key `receipt_used`. Only the explicit `transferBadge` re-binds, so the automatic sweeps on two devices of one store account never move the badge back and forth.
- A `transferBadge` for a payment credited nowhere (the buying device died before its hand-over) is answered exactly as `purchaseBadge`'s first purchase and sets no `rebound_at`.
- The checks run in this order, so a transfer re-sent after a lost answer gets its credential, not a refusal: (1) the payment already credited to the signing key → answered from the record, with no store call and no cap, as `purchaseWithReceipt`'s `creditedBy` does (`SVC/Service.hs:501-504`); (2) the signing key already holds another purchase → `bad_request`, before any store call, in the handler rather than the dispatcher's `purchaseKeyExists` path (an insert would otherwise fail as `internal` and be retried for ever); (3) the store vouches; (4) the cap → `transfer_limited`. `transfer_limited`, like `receipt_used`, is answered only after the store vouched, so neither reveals which payments were credited.
- `transferBadge` is routed to `PurchaseLane` (`requestLane`), since it makes store calls.

**The cap, and what it defends against.** A store payment is re-bound at most once per month: `transferBadge` is refused with the new error `transfer_limited` and `retryAfter` while `payments.rebound_at` is less than one calendar month old (`addMonths 1`). It is the only control behind this path, because a store receipt proves control of the store account but is not a one-time secret. It defends against:

- Minting: each re-bind issues the running month to a new key while the previous key's credential stays valid to its expiry. With the cap, each paid month is held by at most two keys, whatever the number of devices, profiles or people with access to the account.
- The store quota: re-binds are store reads, also under the hourly per-claim bound.

It does not stop whoever controls the store account, or holds a receipt from it, from moving the badge to themselves once a month: the store account is the authority, as the store itself gives the subscription to every device signed into it. The owner's recourse is Restore again after the month, or cancelling.

**The old key afterwards.** A purchase whose tip is a `transferOut` is answered `subscription = ended` and no credential, and the client retires a subscription badge on `ended` at once, at its next request. Its issued credential stays valid to expiry.

**Client core (R2).** `StoreReceiptOrigin` gains `SRORestored`. `holdStoreReceipt` with that origin:

- no record (a new install) → a held record with fresh keys and hold kind transfer;
- a held record of kind purchase whose stash funds no purchase yet → its hold kind becomes transfer (Restore's own `Transaction.unfinished` pass may have just created it as a purchase);
- a resume hold or a tier-change hold → answered `held` unchanged, since its keys already fund a purchase the service knows;
- a record refused with `receipt_used` and never credited → re-held as a transfer, keeping its keys;
- a record credited to another profile's purchase → re-held for the presenting profile with fresh keys and hold kind transfer, through `reholdStoreReceipt` extended with the owner and keys;
- a record credited to this profile's own retired purchase → left to the same-install resume, which keeps the purchase and its key and spends no cap.

The receipt worker sends `BSCTransferBadge {masterKey, payment}` for a transfer record. On success `createStashBadgePurchase` links the record to the new purchase, clearing the old purchase's `badge_store_receipt_id` in the same transaction (the column is UNIQUE). `transfer_limited` and the existing terminal refusals put the record back as credited to the old purchase, with the refusal recorded and announced as refusals are today.

**What the apps must show.** Restore (on Support SimpleX, both platforms) hands over the store's current entitlements — iOS `Transaction.currentEntitlements` after `AppStore.sync()`, and `Transaction.unfinished`; Kotlin the subscriptions Play lists — all with the new origin `restored`. The existing "being added" state and refusal alerts carry the result; the badge appears when credited (no success screens, model principles). New text for `transfer_limited`: "This purchase was moved to another device or profile recently. It can be moved again on {date}." Restore stays refused while the profile shows a badge (`refuseWhileBadgeHeld`). Both `gaps.md` entries shrink to the receipt path.

**What the service's statement reader needs.** `transferOut` rebuilt from `to_purchase_id` (L1); K2's rows are `charge` credits the reader already handles.

**Migrations.** Service (R1a): `payments.rebound_at`; `idx_badge_ledger_charge` changes from `UNIQUE(charge_id)` to `UNIQUE(badge_purchase_id, charge_id)`, since a re-bound charge funds K1's past rows and K2's new ones (granted at most once per purchase; `getSubscriptionCharge`'s "a ledger row references it", `SVC/Store.hs:448-458`, stays true). Client: none beyond T3's `hold_kind`. Up and down, SQLite and Postgres for each. The down steps cannot restore `UNIQUE(charge_id)` once two purchases reference one charge, and dropping L1's reference columns would make every `transferOut` and `upgrade` row unreadable, so each down step fails with a clear error while such rows exist, rather than corrupting the ledger; tested on an empty ledger and after a re-bind.

**Threat model.**

- Replayed `transferBadge` (a retry after a timeout): the payment is now credited to K2, so K2 is answered from the record by check (1); nothing is written.
- Leaked store receipt: a holder can move the badge to their own key once a month (the cap); Apple's latest-transaction rule limits a leaked JWS to its billing period; every re-bind is logged for support. Play tokens have no such freshness, so the cap is the only bound there.
- Two devices or profiles racing a transfer: the write transaction re-reads K1's tip and P's binding and writes nothing if either moved (answered `internal`, retried by the worker).
- K1 racing with `issueBadge`: the tip re-read guards both writers, as today.
- Malformed or oversized evidence: the purchase path's verifiers, unchanged.
- Metadata: a transfer links K2 to K1 at the service. The store account already links them (one payment), but `badges-overview.md` objective 4 must list it as an exception, and its "A badge cannot be transferred" line (`:95`) changes. The request follows the user's explicit Restore, as a purchase does, and the re-issued credential is presented at once, as a purchase's is.

### 4.4 Restore by receipt, for Stripe and crypto payments (later work; does not block subscriptions)

- **Receipts (R4, service).** Every crediting by a web payment or by a code that grants months (not lifetime) returns a receipt in `BSPBadgeCredential.receipt`: high-entropy, checksummed, human-copyable (the code format of `Badges/Code.hs`); the service stores only its hash, `badge_purchases.receipt_hash` (service migration).
- **Transfer (R5, service).** `transferBadge {masterKey, payment = SPReceipt receipt}` from K2: the purchase holding that receipt hash; once per receipt — the receipt is retired and a fresh one is returned to K2; unissued months only. Rows: K1 `lapse` to now, `transferOut {K2}` of its unissued months; K2 seeded at now, `transferIn {fromPurchaseKey = K1}` of those months at now, the first month issued now; nothing to move is answered `receipt_used`. The running month stays with K1, so no month is ever issued twice on this path. Disputes go to support (model).
- **Apps (R6).** Core stores the receipt (client migration `badge_purchases.receipt`), shows it on Your badge with copy and a one-time "keep your receipt — it recovers unused months if you lose this device" prompt at the first credit, and adds a "Move a badge from another device" entry beside "Redeem badge code" that takes a receipt.
- Caps: once per receipt; unissued months only. They defend against a receipt being used twice and against a leaked receipt minting time: a holder can take only the months not yet issued, once.
- Statement reader: `transferIn` rebuilt from `from_purchase_id`, `transferOut` from `to_purchase_id` (L1).

### 4.5 What blocks selling subscriptions

- **Blocks:** L1 and L2 (both features need them); T1 (a downgrade at renewal is otherwise charged and never granted); S0, T2 and T3 (an immediate upgrade is otherwise granted only at the next renewal check, so an annual subscriber pays for the new tier for up to 11 months while holding the old); R1a, R1b, R2 and R3 (restore, per `plans/2026-10-13-badge-subscription-renewals.md:339`, and App Review's restore expectation for auto-renewable subscriptions under guideline 3.1.1).
- **Does not block:** T4 (the pending-downgrade notice), the receipt path R4-R6, and documentation beyond the protocol changes.

### 4.6 Alternatives considered

- **Split the Apple subscription group per tier** (store configuration): removes in-store tier changes (crossing tiers becomes cancel-and-buy, a new original id), so T1-T3 shrink to a defensive refusal. Not the default because the model's §7 requires one group for native upgrades; a product cannot move between groups (not verified here), so this must be decided before launch (open question 1).
- **A new purchase under new keys for a store tier change** (model §2.10): the service discovers changes made in the store's settings without the client, and only the client can make keys; the protocol already keeps one key for store subscriptions (`badges-rpc.md:68`).
- **Send `upgradeBadgeSubscription` from the hand-over itself:** breaks the hand-over's contract that nothing is sent there, delays the app's sequential sweep by a round trip and a store check, and turns a transient failure into a "Purchase error" alert; the re-hold and the receipt worker already give retries and backoff.
- **Re-bind implicitly on any `purchaseBadge` from another key:** two devices on one store account would move the badge back and forth on every launch sweep.
- **`transferIn` on the new key for a store re-bind:** the client would read the re-bound subscription as prepaid and retire it at paidThrough, never asking for renewals; the store's own `charge` credit says what funds it.
- **Move unissued months only on the store path:** a monthly subscriber restoring mid-month would get nothing until the next renewal, and a reviewer testing restore on a second device would see no badge; the model gives stores a capped re-bind and the receipt path the unissued-months rule.
- **Move a purchase between profiles of one install locally** (the install holds the old purchase's keys, so core could re-own the purchase without asking the service, and without linking two keys there): cheaper and more private for that case, but it covers neither a new install nor a profile whose database was restored elsewhere, and it moves keys between profiles' records; one path is kept, and this is worth revisiting if the service-side link matters.
- **Check the store at every monthly issuance during coverage** instead of the device's hand-over: twelve times the store calls for annual subscribers and still up to a month late; the check at coverage end stays as the backstop.
- **Keep `start` non-decreasing across a tier change** (start the new tier where the old coverage ends): the upgrader would pay for the new tier now and get it up to a month later, with "Renews" off the store's date.

## 5. Compatibility and security

- **Wire (protocol version stays 1):** `BSCUpgradeBadgeSubscription` drops `badgeRequest` (never sent by any client; `badges-rpc.md:30` already says so); new `BSCTransferBadge {masterKey, payment}`; new error `transfer_limited` (with `retryAfter`); statements may now carry `transferOut` and `upgrade` (and later `transferIn`), which every client already decodes (`ChatTypes.swift:502-605`, and the Kotlin mirror); an older client never sends the new commands. `badges-rpc.md` and `badges-rpc.schema.json` change accordingly, and `badges-overview.md` objective 4 and line 95.
- **Database:** service migrations L1 and R1a (folded into one if they ship in one release), client migration T3 (`hold_kind`), each with up and down, SQLite and Postgres; schema down/up cycle tests. Changed client queries change `chat_query_plans.txt`, which only a full suite regenerates.
- **Older clients against the newer service:** a client without T3 never sends `upgradeBadgeSubscription` and gets tier changes at renewal (T1). A client without R2 never sends `transferBadge`. An older client receiving `transferOut` checks it as uncontradicted and keeps working; it retires on `ended` only from commit 55d69781e on.
- **Adversary:** the threat models in §4.2 and §4.3; tests in §6.

## 6. Slices

Every slice runs `cabal build --enable-tests` (project default `-O1`) and its scoped suites, checking example counts. `-m "Supporter badges"` is `tests/BadgeTests.hs`; `-m "SimpleX badge service e2e/subscriptions/"` and `-m "SimpleX badge service e2e/store purchases/"` are e2e groups in `BotTests.hs`; `-m "badge service schema"` and `-m "badge service store"` are the schema and store tests in `WebTests.hs`.

PR1 — the ledger and tier changes at renewal (L1 ~150, L2 ~200, T1 ~200 changed lines). PR2 — tier changes at once (S0 ~60, T2 ~250, T3 ~300). PR3 — store restore (R1a ~150, R1b ~300, R2 ~300, R3 ~150). Each PR ships after the one before it; T3 and R2 also after the same-install resume.

### Slice L1: store and re-read entries that name another purchase (behavioural, service)

- Files: `SVC/Store/{SQLite,Postgres}/Migrations.hs`, `SVC/Store.hs`, `Ledger.hs`, `src/Simplex/Chat/Store/Badges.hs` (the fallback call), `tests/BadgeTests.hs`, `tests/Bots/BadgeService/WebTests.hs`.
- Change: a migration adding `from_purchase_id` and `to_purchase_id`; `appendLedgerPlan` writes them; the two reads join the keys; `entryTypeFromColumns` takes a references record and rebuilds the three types.
- Tests: `testEntryTypeColumns` round-trips all three; `testTransferEntriesRoundTrip` (like `testChargeCreditRoundTrip`) writes each with a real second purchase and reads tip and statement back; `testServiceColumns`, `testSchemaDownUpCycle`, and the down step refusing while such rows exist.
- Verify: `cabal test simplex-chat-test --test-options='-m "Supporter badges" -m "badge service schema" -m "badge service store"'`.
- Done when: AC1.

### Slice L2: the transitions and the client's check (behavioural, pure)

- Files: `Ledger.hs`, its callers (`SVC/Service.hs` `signFirstMonth`, `newSubscription`, `renewSubscription`, `fundsMonthNow`), `tests/BadgeTests.hs`, `tests/Bots/BadgeService/WebTests.hs` (calls `grantEntry`, `:63`, `:736`).
- Change: `debitEntry`; `grantEntry` and `chargeEntries` take the tier (and `chargeEntries` the own key); `entryChecked` as in §4.1. Every current caller passes the purchase's tier, so behaviour is unchanged.
- Tests: `testDebitRowsVerify` (upgrade and transferOut verify `Just True`; a debit that leaves months is marked false); `testTierChangeRowsVerify` (a monthly downgrade at renewal, an annual-to-annual upgrade in month 3 with an `upgrade` debit, a tip written after the period start clamping `t`; each row `Just True` through `balanceChecked`; a tier switch over a non-empty balance `Just False`); `testChargeRowsVerify` unchanged.
- Verify: `cabal test simplex-chat-test --test-options='-m "Supporter badges"'`.
- Done when: AC2, and the riskiest assumption (§8) holds.

### Slice T1: grant another tier at renewal (behavioural, service)

- Files: `SVC/Service.hs` (`grantableCharge`, `renewSubscription`), `SVC/Store.hs` (update `current_badge_type`), `Commands.hs` (`applyBadgeStatement` updates `current_badge_type` from the tip), `src/Simplex/Chat/Store/Badges.hs`, `BotTests.hs`, `tests/Bots/BadgeService/FakeStore.hs` (a Legend subscription JWS fixture), `docs/protocol/badges-rpc.md`.
- Change: `NGOtherTier` removed; the renewal grants the other tier through `chargeEntries`; service and client update `current_badge_type` in the same write.
- Tests: `testSubscriptionTierChangeAtRenewal` (an Apple monthly Supporter whose next charge is Legend: granted, Legend credential, `BadgeState` Legend, rows verify; then back to Supporter at the next renewal; and an annual Supporter whose Legend charge is found only after its coverage ended: granted from the clamped `t`, rows verify); `testSubscriptionChargeNotGranted` loses its other-tier case. If the same-install resume has landed, its bounded-resume test's refused charge switches from another tier to a revoked one, and the comment on `resumeWindow` stops citing the other-tier refusal.
- Verify: `-m "SimpleX badge service e2e/subscriptions/"`, `-m "Supporter badges"`.
- Done when: AC3.

### Slice S0: share the store plan mapping (structural)

- Files: `SVC/Catalog.hs`, a shared module under `src/Simplex/Chat/Badges/` (or `src/Simplex/Chat/PaymentService.hs`), `simplex-chat.cabal` if a module is added, the callers.
- Change: move `storePlan`, no behaviour change.
- Verify: `cabal build --enable-tests`; `-m "badge catalog"`, `-m "Supporter badges"`.
- Done when: it builds, the suites are green, and the diff is the move.

### Slice T2: `upgradeBadgeSubscription` (behavioural, service)

- Files: `src/Simplex/Chat/Badges/Service.hs` (drop `badgeRequest`), `SVC/Service.hs` (the handler; `requestLane` routes it to the renewal lane), `docs/protocol/badges-rpc.md`, `docs/protocol/badges-rpc.schema.json`, `BotTests.hs`.
- Change: the handler is `renewSubscription` without its coverage condition, plus the claim check and the answers of §4.2 "Caps"; answers through `issueResponse` with `subscription`.
- Tests: `testSubscriptionUpgradeNow` (an annual Supporter in month 3 whose store reports a Legend annual charge started now: `upgrade` debit, Legend grant, Legend credential; a repeat within the hour makes no store call and, until the Legend charge is granted, answers `payment_pending` with `retryAfter`, never the Supporter credential; the store still reporting Supporter answers `payment_pending`; a same-tier request during coverage writes nothing); `testUpgradeRequestRefused` (another key, another subscription's JWS, a prepaid purchase); `testStoreUpgradeRefused` and `testStoreRouting` updated.
- Verify: `-m "SimpleX badge service e2e"`, `-m "Supporter badges"`.
- Done when: AC4.

### Slice T3: hand a tier change over from the app (behavioural, core)

- Files: `src/Simplex/Chat/PaymentService.hs` (`productId` in `AppleTransactionFacts`), a client migration for `badge_store_receipts.hold_kind` (SQLite, Postgres, the `Migrations.hs` lists, `simplex-chat.cabal`), `src/Simplex/Chat/Store/Badges.hs` (`reholdStoreReceipt` with a kind; the held-record query takes a tier-change record while its purchase is shown), `Commands.hs` (`handOverStoreReceipt`'s credited branch, the receipt worker's attempt, presentation at once on this path), `BotTests.hs`, `FakeStore.hs`.
- Change: §4.2 "Client core (T3)". Lands after the same-install resume, whose request-body split and `reholdStoreReceipt` it reuses.
- Tests: `testAppleTierChangeHandOver` (a Legend JWS under the same original id handed over while Supporter is shown: answered held, the worker's request grants Legend, Legend presented at once, `BadgeState` Legend; a same-tier renewal JWS answered credited, nothing sent); `testAppleTierChangeStoreLags` (the store still reports Supporter: held, deferred, granted once the store reports Legend; with the clock past `tierChangeWindow`, settled with no change and no request).
- Verify: `-m "SimpleX badge service e2e/store purchases/"`, `-m "SimpleX badge service e2e/subscriptions/"`.
- Done when: AC5.

### Slice R1a: re-bind storage (behavioural, service storage)

- Files: `SVC/Store/{SQLite,Postgres}/Migrations.hs` (`payments.rebound_at`, the charge index), `SVC/Store.hs` (move a payment's binding from one purchase to a new one with both purchases' ledger rows in one transaction, re-reading the old tip and the binding), `WebTests.hs`.
- Tests: `testServiceColumns`, `testSchemaDownUpCycle`, the down step refusing after a re-bind; a store-level test that moves a binding and reads both ledgers back.
- Verify: `-m "badge service schema"`, `-m "badge service store"`.
- Done when: storage and migration are in place, and no request uses them yet.

### Slice R1b: `transferBadge` for store subscriptions (behavioural, service)

- Files: `src/Simplex/Chat/Badges/Service.hs` (the command, `transfer_limited`), `SVC/Service.hs` (the handler beside `purchaseWithReceipt`, sharing its verification and hourly claim bound; `requestLane` routes it to `PurchaseLane`; `issueBadgeCmd` answers `ended` after a `transferOut` tip), `docs/protocol/badges-rpc.md`, `docs/protocol/badges-rpc.schema.json`, `docs/protocol/badges-overview.md` (objective 4, line 95), `BotTests.hs`.
- Change: §4.3.
- Tests: `testStoreRebind` (Play monthly and Apple annual: K1 `transferOut`, K2 funded by the charge and issued, rows verify, the next renewal funds K2); `testStoreRebindCapped` (a second transfer within a month gets `transfer_limited` with `retryAfter`, after the store vouched; after a month it is allowed); `testStoreRebindOldKeyEnded`; `testStoreRebindStaleAppleTransaction`; `testTransferUncredited` (answered as a first purchase, no `rebound_at`); `testTransferReplayAnswered` (the same transfer sent twice: the second answered with K2's credential, no store call, no `transfer_limited`); `testTransferKnownKeyRefused` (`bad_request`, no store call); `testStoreRouting` gains the transfer; `testStoreReceiptUsed` unchanged.
- Verify: `-m "SimpleX badge service e2e/store purchases/"`, `-m "SimpleX badge service e2e/subscriptions/"`.
- Done when: AC6.

### Slice R2: Restore in core (behavioural, core)

- Files: `src/Simplex/Chat/Badges/Types.hs` (`SRORestored`), `src/Simplex/Chat/Store/Badges.hs` (`holdStoreReceipt`'s restored cases), `Commands.hs` (the receipt worker sends `transferBadge` for hold kind transfer), `BotTests.hs`. No migration (T3's `hold_kind`).
- Change: §4.3 "Client core (R2)". Lands after the same-install resume and T3.
- Tests: `testRestoreOnNewDevice` (bob restores alice's Play subscription: shown on bob; alice's badge retires at her next request; and a subscription credited nowhere is credited as a first purchase); `testRestoreOnOtherProfile` (same install, `/create user`); `testRestoreAfterUnfinishedPass` (the same transaction first handed over as a purchase, then restored: the held record flips to transfer; a record already refused `receipt_used` is re-held and transferred; a resume or tier-change hold found by a Restore on another profile is answered `held` unchanged); `testPurchaseStashReceiptUsed` unchanged (a sweep still gets `receipt_used`).
- Verify: `-m "SimpleX badge service e2e/store purchases/"`, `-m "Supporter badges"`.
- Done when: AC7.

### Slice R3: Restore in the apps (behavioural, iOS and Kotlin)

- Files: `apps/ios/Shared/Views/Badges/BadgeStore.swift`, `apps/ios/Shared/Views/Badges/BadgesCheckOrderView.swift` (`RestorePurchasesButton` texts), `apps/ios/Shared/Model/SimpleXAPI.swift`, `apps/ios/SimpleXChat/ChatTypes.swift` (the origin, the error); Kotlin `BadgeStore.kt`, `BadgesCheckOrderView.kt`, the model and API files, strings; both `gaps.md`.
- Change: §4.3 "What the apps must show", mirrored on both platforms.
- Verify: build iOS and Android; test Restore on a second device and on a second profile against the dev service.
- Done when: AC8.

### Optional, not blocking

- T4: the pending-downgrade notice on Your badge (iOS).
- R4-R6: receipts and transfer by receipt (§4.4).

## 7. Test plan

- New tests: listed per slice. The pure ones (L2) prove every new row verifies on the client before any service code depends on it; the service ones prove the grant rules, caps and refusals; the e2e ones prove the client follows the tier and the move.
- Existing tests that must stay green: "Supporter badges", "SimpleX badge service" (unit, config, schema, store verifiers), "SimpleX badge service e2e" (managed-group tests can fail under load; rerun them alone).
- Final: `cabal test simplex-chat-test --test-options='-m "badge"'`; the full suite, which regenerates `chat_query_plans.txt`; the Postgres builds and `-m "badge service schema"` with `--flags=+client_postgres`.

## 8. Risks

- Riskiest assumption: that a tier change restarting the run at `t = max periodStart (createdAt tip)` keeps every row verifiable and `createdAt` non-decreasing on all paths (an annual upgrade mid-year, a downgrade at renewal, a late store check). L2's pure tests run the rows through `balanceChecked` before T1 is built, so a failure changes the transition before any service code depends on it.
- Apple behaviour taken from its documentation: an upgrade is immediate with a pro-rata refund and a new transaction under the same original id; a downgrade waits for renewal; the status answer's latest transaction names the new product; `Transaction.currentEntitlements` on a new device returns the latest transaction. To be confirmed with Sandbox before sale. If an upgrade is not refunded pro rata, the `upgrade` debit removes months the user paid for, and it becomes a conversion into the new tier instead.
- The cap's month and Apple's latest-transaction rule may refuse a genuine restore right after a renewal; the app shows the retry date or "try again".
- The same-install resume is being built in parallel on the same branch; T3 and R2 depend on it.

## 9. Open questions

The slices assume the recommended answers.

1. Keep one Apple subscription group and build tier changes, or split the group per tier before launch? Recommended: keep it and build T1-T3 (the model's §7 requires one group; native upgrades with Apple's proration). If split: new products before launch, T2 and T3 dropped, T1 kept as a net.
2. The store re-bind cap: once per payment per calendar month from the last re-bind. Recommended: yes. A stricter cap (once per billing period, i.e. once a year for annual plans) leaves a second lost phone in a year unable to restore; a looser one allows more simultaneous holders per paid month.
3. A store re-bind re-issues the running month. Recommended: yes, so restore works mid-month and on a reviewer's second device. If not: the store path moves unissued months only, and monthly subscribers wait for the next renewal.
4. An Apple re-bind requires the store's latest transaction. Recommended: yes. If not: a leaked JWS keeps working until the subscription ends, bounded only by the cap.
5. A store tier change stays on the same key, where the model's §2.10 makes a new badge row with new keys. Recommended: the same key. If new keys are required, changes made in the store's settings need the client to re-bind itself after the service detects them: R1's machinery plus a new trigger.

## 10. Out of scope (noticed, not doing)

- `issueResponse` re-sends the whole history when the asserted entry is unknown (TODO at `SVC/Service.hs:786-787`; `Commands.hs:6090-6092`).
- `writeIssued` answers a skipped write as success.
- `balance_checked` is stored and read by nothing.
- A refused store-receipt record is deleted after 30 days (`deleteUnfundedStoreReceipts`), after which a sweep presents it again.

## 11. Later work

- Receipts and transfer by receipt (R4-R6, §4.4), for Stripe and crypto payments.
- T4: the pending-downgrade notice on Your badge (iOS; Kotlin has no in-store tier change).
- Refund handling, which must account for upgrades (§4.2 threat model).
