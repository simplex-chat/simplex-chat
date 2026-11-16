# Badge subscriptions: tier changes and restore

Base: `f/badge-subscriptions` at 26f846e1a (line numbers at that commit). `SVC/` = `apps/simplex-badge-service/src/BadgeService/`. "Model" = `plans/2026-07-30-supporter-badges-v3-ux.md`.

## Summary

**What.** Two gaps that block selling subscriptions:

- **Tier change.** All four Apple subscription products share one subscription group, so a user can switch Supporter ↔ Legend in App Store settings. Apple keeps the same subscription and charges the new price; the service records the charge and grants nothing. We grant the tier the store charged.
- **Restore.** A subscriber on a new phone, or on another profile, taps Restore and gets `receipt_used`, because the payment belongs to the old install's purchase key. We let Restore move the subscription to the new key.

**Why together.** Both move months between tiers or between purchase keys. The ledger entry types for that exist (`transferIn`, `transferOut`, `upgrade`), but the service can neither store nor re-read them, and a grant cannot change a balance's tier.

**How.**

- Tier change: on a charge of the other tier, the service ends the old tier's remaining months with an `upgrade` debit and starts a new run of months at the new tier. It finds out at the next renewal check, or, for a raise, at once when the app sees the new App Store transaction and asks (`upgradeBadgeSubscription`, already in the protocol, unimplemented).
- Restore: a new request, `transferBadge`, signed by a fresh key and carrying the store receipt. The service moves the payment to the new key: `transferOut` on the old purchase; the store's current charge credited to the new one, with the current month issued. A payment moves again only once the credential issued to the key it last moved from has expired: a store receipt is not a secret, so this limit is the only safeguard.
- Stripe and crypto purchases are restored by a receipt code (once per receipt, unissued months only): designed here, built later; it does not block subscriptions.

**Where.** Service (`SVC/Service.hs`, `SVC/Store.hs`, migrations); shared ledger (`src/Simplex/Chat/Badges/Ledger.hs`); core (`Library/Commands.hs`, `Store/Badges.hs`); the Restore button in both apps.

**New types and wire changes.**

- `BSCTransferBadge {masterKey, payment}` and error `transfer_limited`.
- `BSCUpgradeBadgeSubscription` implemented; its `badgeRequest` field dropped (never sent).
- `StoreReceiptOrigin` gains `SRORestored`; `AppleTransactionFacts` gains `productId`.
- Ledger: `debitEntry`; `grantEntry` and `chargeEntries` take the grant's tier. No new entry types.

**Schema.**

- Service: `badge_ledger.from_purchase_id`, `badge_ledger.to_purchase_id`; `payments.rebound_at` (when the payment was last moved to another key, logged; the limit itself is checked against the previous key's latest credential expiry, which the service signed, and `transfer_limited` carries no date); charge index `UNIQUE(charge_id)` → `UNIQUE(badge_purchase_id, charge_id)`.
- Client: `badge_store_receipts.hold_kind` (purchase, transfer, tier change).
- Later, receipts: `badge_purchases.receipt_hash` (service), `badge_purchases.receipt` (client).

## Current state

- **Ledger.** The service ledger's only reference columns are `payment_id` and `charge_id`; `appendLedgerPlan` writes only the tag of the three types (`SVC/Store.hs:318-353`), and `entryTypeFromColumns` cannot rebuild them (`Ledger.hs:220-244`). One unreadable row makes every later answer for that purchase `internal` (`SVC/Store.hs:282-300`). The client stores each entry's JSON and needs no column (`Store/Badges.hs:551-616`).
- **Tier.** Fixed per purchase: `current_badge_type` is written once on both sides; every transition keeps the previous tier (`Ledger.hs:76-117`); the service refuses another tier (`NGOtherTier`, `SVC/Service.hs:631`). On the device, a new Apple transaction for the same subscription is answered from the old record and finished, without asking the service (`Commands.hs:5377-5380`).
- **Restore.** A payment belongs to one purchase key; any other key gets `receipt_used`, which the client treats as final (`SVC/Service.hs:666-671`, `Commands.hs:5409-5414`). The client marks a purchase as a subscription only when its ledger holds a `charge` credit (`Store/Badges.hs:464-492`).
- **Builds on:** retirement on `ended` (commit 55d69781e), and the same-install resume being built on this branch (`reholdStoreReceipt`, `resumeWindow`, `sendBadgeIssue`), whose re-hold T3 and R2 reuse.

## Design

### Ledger (L1, L2)

- **Storage (L1).** `appendLedgerPlan` writes `from_purchase_id`/`to_purchase_id` from the entry's key; the reads join `badge_purchases` for the keys; `entryTypeFromColumns` takes one record of references and rebuilds all three types.
- **Transitions (L2).**
  - `debitEntry`: all remaining months to zero, start unchanged (the model's `debit`).
  - A grant of another tier is allowed only onto an empty balance, and starts a new run at its own time with the new tier. So the model's "start never decreases" now holds within a tier.
  - `chargeEntries` for another tier, at `t = max periodStart (createdAt tip)`: lapse to `t`, `upgrade {own key}` debit if months remain, grant at `t`, lapse to now. Clamping `t` keeps `createdAt` non-decreasing.
  - The client's check re-runs `debitEntry` for `upgrade` and `transferOut`, and marks false a tier switch over a non-empty balance.

### Tier change (T1, S0, T2, T3)

- **At renewal (T1).** `NGOtherTier` is removed: a charge of the other tier is granted through `chargeEntries`, and both sides update `current_badge_type`. On its own this fixes downgrades, which take effect at renewal.
- **At once (T2, T3).** An Apple upgrade is immediate, and the unused part of the old plan is refunded pro rata. Without T2 and T3 an annual subscriber would pay for Legend for up to 11 months while holding Supporter.
  - S0 moves `storePlan` (product → tier and months) into shared code, so core can read a transaction's tier.
  - T3: when a handed-over Apple transaction raises the tier of the shown purchase, was bought after the purchase's last charge credit and has not expired, core re-holds the record as a "tier change". The app keeps the transaction unfinished, and the receipt worker sends `upgradeBadgeSubscription`. A raised tier's credential is presented at once, on this path and on T1's renewal check alike: the subscriber raised the tier in their store, so the broadcast tells the store nothing new. A lower tier keeps the renewal's hold-back, presented when the shown credential expires. Any other transaction, a lower tier or an expired one, is answered from the record and finished, and T1 catches the change at renewal.
  - T2 (service): the request must be signed by the purchase key and name its own subscription. The store is asked at most once an hour per subscription, sharing the renewal check's stamp. The answer is the new tier's credential once granted; until then `payment_pending` (within the hour, or while Apple's status still shows the old product), never the old credential.
- **Authority.** The tier granted is the one in the store's own verified answer. The JWS the app sends only names the subscription and what to wait for.
- **Apps.** Nothing new is required: Your badge shows `badgeState.badgeType`, which now follows the ledger. An optional iOS notice "Changes to Supporter on {date}" (T4) shows a pending downgrade.

### Restore for store subscriptions (R1a, R1b, R2, R3)

**`transferBadge`, signed by a new key K2, for payment P held by K1** — one transaction:

- K1: lapse, then `transferOut {K2}` of its unissued months. It is written even at zero, as the marker that makes K1's next answer `subscription = ended`, so K1's badge retires.
- K2: a new purchase funded as a first purchase of P would be: the store's current `charge` credit at its period start, then the current month issued. No `transferIn`: the client would then read K2 as prepaid and stop asking for renewals.
- P's binding moves to K2; `rebound_at = now`; the move is logged.
- Store one-time months are not re-bound: they are issued at purchase, and no app path lists them again.

**Check order**, so a request re-sent after a lost answer gets its credential:

1. P already credited to K2: answered from the record, with no store call and no limit.
2. K2 already holds another purchase: `bad_request`.
3. The store vouches for the receipt now. For Apple it must be the subscription's latest transaction, so a leaked old receipt stops working at the next renewal.
4. The limit: `transfer_limited`.

A payment credited nowhere is treated as a first purchase. `purchaseBadge` from another key still gets `receipt_used`, so the automatic sweeps on two devices never move the badge back and forth; only the Restore button transfers. The request is routed to the purchase lane, since it calls the store.

**Limit: a payment moves again only once the credential issued to the previous key has expired.** That credential stays valid until then, so each paid month is held by at most two keys, however many devices or people share the store account. The bound is the credential's own expiry rather than a calendar month, because a calendar month has an edge: a move on 31 January and another on 1 February are both allowed, and since a credential outlives a month, three keys would briefly hold valid ones. It does not stop someone holding the account, or a receipt from it, from moving the badge to themselves each time a credential lapses; the store account is the authority, as the stores themselves treat it.

**Core (R2).** Restore hands receipts over with the new origin `restored`:

- no record → a held transfer record with fresh keys;
- a held purchase record that funds no purchase yet → becomes a transfer;
- a record refused `receipt_used` → re-held as a transfer;
- a subscription record credited to another profile → re-held for this profile with fresh keys; a one-time record is left alone;
- resume and tier-change holds, and this profile's own retired subscription while it is the profile's newest purchase → left to the resume; an older one, or one whose payment moved away → re-held as a transfer with fresh keys.

The worker sends `transferBadge` for transfer records. On success the record links to the new purchase; on a refusal it goes back to the old one.

**Apps (R3).** Restore hands over `Transaction.currentEntitlements` and `Transaction.unfinished` on iOS, and Play's subscriptions on Android, with the `restored` origin. The result shows through the existing "being added" state and alerts. One new text, for `transfer_limited`: "This purchase was moved to another device or profile recently."

### Restore by receipt for Stripe and crypto (later, R4-R6)

- **R4 (service).** Each code or web purchase that grants months returns a receipt code; the service stores its hash.
- **R5 (service).** `transferBadge` with `SPReceipt` moves only the unissued months: `transferOut` on K1, `transferIn {K1}` on K2 starting now. The receipt is retired and a fresh one is returned.
- **R6 (apps).** The app stores the receipt, shows it with a "keep your receipt" prompt, and adds a "Move a badge from another device" entry.

### Threats

| Threat | Handling |
|---|---|
| Tier change the user did not intend | Granted exactly as the store charged; undone through the store (switch back, refund). The apps never offer a change. |
| Client claims a tier it did not pay for | The tier comes from the store's verified answer, not the JWS. |
| Replayed upgrade or transfer request | Upgrade: answered from state within the hour, and each charge is granted once. Transfer: answered from the record (check 1). |
| Leaked or replayed store receipt | A payment moves again only once the previous key's credential has expired; an Apple receipt expires at the next renewal; every move is logged. A Play token has only that limit. |
| Two devices racing a transfer | The write re-reads K1's tip and P's binding, and writes nothing if either moved. |
| Linking | A transfer links K2 to K1 at the service; the store account already links them. Design objective 4 in `badges-overview.md` gains this exception. Requests follow the user's own action, as purchases do. |

## Slices

One PR, built in these slices in order; T3 and R2 need the same-install resume on the branch first. Each slice runs `cabal build --enable-tests` and the suites named.

| Slice | Change | Tests | Lines |
|---|---|---|---|
| L1 | service reference columns + migration; rebuild the three types | `testEntryTypeColumns`, `testTransferEntriesRoundTrip`, schema down/up (`-m "Supporter badges" -m "badge service schema" -m "badge service store"`) | ~150 |
| L2 | `debitEntry`, tier-aware `grantEntry`/`chargeEntries`, client check | `testDebitRowsVerify`, `testTierChangeRowsVerify` (`-m "Supporter badges"`) | ~200 |
| T1 | grant other tier at renewal; update `current_badge_type` both sides | `testSubscriptionTierChangeAtRenewal`; drop the other-tier case from `testSubscriptionChargeNotGranted` and from the resume's bounded test (`-m "SimpleX badge service e2e/subscriptions/"`) | ~200 |
| S0 | move `storePlan` to shared code (structural) | existing suites | ~60 |
| T2 | `upgradeBadgeSubscription` handler, renewal lane | `testSubscriptionUpgradeNow`, `testUpgradeRequestRefused`, `testStoreRouting` | ~250 |
| T3 | `productId`, `hold_kind` migration, tier-change hold and worker send | `testAppleTierChangeHandOver`, `testAppleTierChangeStoreLags` | ~300 |
| R1a | `rebound_at`, charge index, move-binding store function | schema down/up (these changes are in `20261012_badge_subscriptions`, whose down migration drops them and every other subscription column without refusing, and the service README warns against it on a database holding subscriptions; whether it should refuse there is the owner's open call), store test | ~150 |
| R1b | `transferBadge` handler, `ended` after `transferOut`, docs | `testStoreRebind`, `testStoreRebindCapped`, `testStoreRebindOldKeyEnded`, `testStoreRebindStaleAppleTransaction`, `testTransferUncredited`, `testTransferReplayAnswered`, `testTransferKnownKeyRefused` | ~300 |
| R2 | `SRORestored` and the restore cases in core | `testRestoreOnNewDevice`, `testRestoreOnOtherProfile`, `testRestoreAfterUnfinishedPass` | ~300 |
| R3 | Restore in iOS and Kotlin, `gaps.md` | build both apps; restore on a second device and profile | ~150 |

**Blocks selling subscriptions:** every slice above. **Does not block:** T4, R4-R6.

Final checks: `-m "badge"` here; the full suite (query plans) and the Postgres build and schema tests on the host.

## Risks

- **Riskiest assumption:** that restarting the run at the clamped `t` keeps every row verifiable on all paths. L2's pure tests check this before any service code depends on it.
- **Apple behaviour from documentation:** an upgrade is immediate, the old plan is refunded pro rata, and the subscription keeps its original id; a downgrade waits for renewal; `currentEntitlements` returns the latest transaction. Confirm in Sandbox before sale. If upgrades are not refunded, the `upgrade` debit becomes a conversion instead.
- A restore soon after an earlier move may be refused by the move limit, which the app reports with no date, as `transfer_limited` carries none; one presenting an Apple transaction older than the subscription's latest is answered `payment_pending` and stays held until a hand-over brings the latest.

## Decided

- One Apple subscription group for all four subscription products, so users can change tier in the store and this plan grants it.

## Open questions (the slices assume the recommended answers)

1. **The move limit.** Decided: a payment moves again only once the credential issued to the previous key has expired, which is what bounds how many keys hold a valid credential at once. Rejected: once per calendar month, whose month boundary lets two moves through in two days; once per billing period, which would mean once a year for annual plans.
2. **Re-issue the current month on a store restore.** Recommended: yes. Otherwise a monthly subscriber, or an App Reviewer, sees nothing until the next renewal.
3. **Require the latest Apple transaction for a restore.** Recommended: yes.
4. **Keep a tier change on the same key**, where the model's §2.10 uses new keys. Recommended: the same key. The service sees changes made in the store's settings without the client, and only the client can make keys.

## Rejected alternatives

- **Re-bind on any `purchaseBadge` from another key:** two devices would move the badge back and forth on every launch.
- **`transferIn` for a store restore:** the client would treat the subscription as prepaid and stop asking for renewals.
- **Unissued months only for stores:** a restore mid-month would show nothing.
- **Send the upgrade request from the hand-over itself:** it blocks the app's purchase sweep, and a network error becomes a "Purchase error" alert.
- **Ask the store at every monthly issuance:** twelve times the store calls for annual plans, and still up to a month late.
- **Move a purchase between profiles of one install locally:** more private for that case, but it does not cover a new device, and it would be a second path.
