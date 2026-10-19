# Badge subscriptions: PR #7716 for reviewers

PR #7716, `f/badge-subscriptions` into `master`, draft: head `ffb036680`, merge base `0eeac1bb9` (master on 2026-10-19), 76 files, +5,566 −759, of which about 2,050 changed lines are Haskell, 1,250 app code and 2,800 tests. Written on 2026-10-19 from the code at the head and the run reports. Where the code and the plan [`2026-10-13-badge-subscription-renewals.md`](../2026-10-13-badge-subscription-renewals.md) differ, this follows the code, and §13 lists the differences. Core paths are under `src/Simplex/Chat/`, service paths under `apps/simplex-badge-service/src/`; line numbers are at `ffb036680`.

## 1. What it adds and why

On master a badge is prepaid: a code bought on the web, or a one-month store purchase that is built but hidden in the apps since 2026-10-16. This PR sells badges as App Store and Google Play subscriptions (supporter or legend, monthly or annual) that keep renewing without the app handing renewals over. When the paid months run out, the app sends its usual signed `issueBadge` at the shared weekly moment; the badge service asks the store whether it charged again and grants that charge's months from the store's billing-period start. The app keeps the badge until the service answers `ended`, or until 67 days of answers that renewed nothing have passed (§4).

It also adds the screens to sell, show and manage a subscription on iOS, Android and desktop, the subscription alerts, and the store-load bounds the reviews required before sale: three request lanes, and per-claim, per-minute and per-subscription store limits (§5). Renewals are found when the app asks, not from store notifications, so no background ledger writer is needed (plan §4, alternatives).

![Components: what the apps, core, the badge service and the two stores each hold, and who talks to whom](components.svg)

## 2. Buying a subscription

The app first creates the purchase record in core (`/_badge invoice`, refused while the profile shows a badge or holds a store receipt that may still be credited, `Library/Commands.hs:3627-3628`), buys with that id echoed by the store, and hands the receipt over with `/_badge purchase`. Core's receipt worker sends `purchaseBadge` under the record's keys, and the app finishes the store transaction only once core reports it credited or refused.

| | iOS (StoreKit 2) | Android (Play Billing) |
|---|---|---|
| Offered to | an Apple ID with no earlier subscription in the group (`BadgeStore.swift:142`, `:442`), since Apple would fold a new one into the old original id | everyone; the base plan's own offer, never a promotional one (`PlayStore.kt:229`) |
| App sends | the signed transaction (JWS) | the product id and purchase token |
| Claimed by | `originalTransactionId` (`PaymentService.hs:91-99`) | the token's SHA-256 |
| Service checks | the JWS offline against the pinned root (Sandbox: `test_purchase`, no API call), then the App Store Server API (`StoreReceipts/Apple.hs:240-251`) | `purchases.subscriptionsv2.get` by token, without the one-time `verifyReceipt` (`StoreReceipts/Google.hs:129-134`) |
| Acknowledgement | none exists | by the service, after the grant check and before signing and writing (`BadgeService/Service.hs:574`); never by the app (`BadgeStore.kt:281`) |

The service credits the store's latest charge if `grantableCharge` and `fundsMonthNow` allow it (`BadgeService/Service.hs:619-643`): a plan it sells (on a renewal, of the ledger's tier), a well-formed period with `periodStart <= now < periodEnd`, not revoked, not in an offer (Apple: any `offerType`; Play: a free-trial or introductory phase), a state other than `ended`, and months from the period start not yet run out, which Play's clipped start can cause. `chargeEntries` (`Badges/Ledger.hs:122-128`) dates the grant to the period start, with a lapse before and after it where months ran out; the month is then issued at now, in the transaction that claims the payment.

![Buying on iOS, step by step](buy-ios.svg)

![Buying on Android, step by step](buy-android.svg)

**First-purchase answers** (`BadgeService/Service.hs:543-655`, `docs/protocol/badges-rpc.md:37`). The claim asked about within the hour: `payment_pending` with the time left. No Play read left this minute: `rate_limited`, 1-60 s, claim not held. Store unreachable: `provider_unavailable`, an hour. Period not started, or the store says pending: `payment_pending`, an hour. Sandbox or licence tester: `test_purchase`. Already credited to another key: `receipt_used` (for Apple, before any claim check). Play subscription ended, or Play's HTTP 410: `receipt_invalid`, as a resubscription gets a new token. Revoked, in an offer, no charge yet, period over, months run out, a plan not sold, or an Apple subscription ended: `payment_pending`, a day. Malformed period: `internal`. No API key, or no `[google]`: `provider_not_configured`. Any shorter `retryAfter` is raised to the hour (`:807-810`).

**Two acknowledge paths.** `acknowledgeReceipt` needs the `VerifiedStoreTransaction` that only `verifyReceipt` produces (`BadgeService/StoreReceipts.hs:80-88`), so it serves one-time purchases. A Play subscription is routed to `purchaseSubscription` at `BadgeService/Service.hs:506`, before `verifyReceipt` at `:508`, so it never has one; `acknowledgeSubscription` reads the subscription again and acknowledges it unless Play reports it already is (`StoreReceipts/Google.hs:104-111`). Apple sets neither. Both paths acknowledge before signing and writing (`:574-584`), so a failure after the acknowledgement leaves a paid subscription that Play will no longer refund, credited only by a later retry (§14).

**Apple claims by original transaction id.** `applePayloadRef` (`PaymentService.hs:91-99`) names an auto-renewable transaction by `originalTransactionId` and any other by `transactionId`. The client's claim, the service's claim and the verifier (`StoreReceipts/Apple.hs:138-141`) all use it, so every renewal resolves to the subscription's record. A held Apple record adopts a later, non-revoked transaction and is due at once (`Store/Badges.hs:141-150`, `:187-194`). Because a store lists a purchase on every device of the account, hand-overs from that list are marked `listed` and join only a record core already has (`:153-164`).

## 3. Renewing, failing and ending

**Client** (`Library/Commands.hs:5672-5739`). A purchase whose replica holds a charge credit is a subscription (`Store/Badges.hs:468-471`), due at zero months too (`:5878-5885`). A request that issued nothing waits for the next daily moment of the shown credential's expiry, whatever `retryAfter` said (`:5905-5909`, `:5990-5993`), and a due request waits for the wake the last pass stored (`:5689-5693`), so activations do not each reach the service.

**Service** (`BadgeService/Service.hs:703-790`). While coverage runs, issuing is unchanged and asks no store. After it, the store is asked at most once an hour per subscription, stamped before asking (`:797-800`). A grantable new charge is written with the month in one transaction that re-reads the tip (`:760-772`); otherwise the charge and the state are recorded and the answer carries no credential.

**Failing and ending.** Billing retry (Apple status 3 or 4, Play grace or hold) gets no credential: the badge stays, Your badge says "Payment problem", the app asks daily, and contacts show the badge active until 7 days past the shown credential's expiry. An `ended` answer retires the badge in the same pass (`:6034-6041`) with the `subscription_ended` alert.

**The client's copy of the state**, `badge_purchases.subscription_status`, is written by the service's answers (at credit, `:6070`; any answer past the coverage the client asserted, `:5977`) and by the device's report, `/_badge subscription` (`:5386-5407`), which never replaces `billingRetry` or `ended` with `renewing`. The report exists because the service never sees a cancellation inside a paid period.

![The service's renewal check](renewal-check.svg)

![Subscription states: the store answers that lead to each, and what each shows](subscription-states.svg)

## 4. The retirement window

The run of unrenewed answers is two columns, `unrenewed_since` and `last_unrenewed_at` (`M20261017_badge_subscriptions`), read as one `Maybe UnrenewedRun` (`Store/Badges.hs:432-433`). There is no timer: the rule runs in the badge worker's pass (`Library/Commands.hs:5700-5712`).

- On any pass after which the ledger covers now, the run is cleared ("covers nothing" is `L.paidThrough balance' <= now`, 007 D3).
- After an answered request that left nothing covered, a run whose `last_unrenewed_at` is under 67 days old moves it to now; otherwise a fresh run starts. This is the fresh-run rule: a gap that long (the app closed, the service silent) may hide a renewal (007 D4, D8).
- The badge retires on `ended`, or once the window has passed since `unrenewed_since` (`windowPassedSince . unrenewedSince`, `:5710`).

**Why `provider_unavailable` can retire and a timeout never does.** A `BSPError` is an answer: `requestBadgeIssue` returns it as `Nothing` and the pass goes on with the old balance (`:5696`, `:5988-5993`). A timeout throws, recording the failure and the next wake, and the pass stops before the rule (`:5530-5533`), so neither the run nor the ledger changes. A service that answers but never reaches its store therefore retires the badge after 67 days (007 D2), while one that never replies keeps it (005 D15): retiring without an answer would retire a returning subscriber on a network failure at launch (006 R4-1).

**Where 67 days comes from.** `60 * nominalDay + badgeGraceInterval` (`:5847-5848`). 60 days is the longest a store retries a failed renewal with Play's pause off: Apple's billing retry, and Play's grace period plus account hold by default. 7 days is the contacts' display grace (`Badges.hs:172-173`), which also covers Play's period ending days after the ledger's. Both come from store documentation, not observation.

**Accepted costs** (007 plan §8). An app opened less than once per 67 days never retires on a persistent fault. A recovery inside a shorter gap, followed by one answer that renews nothing, can retire a payer. Sixty-seven days of such answers retire a payer whose store may still be charging, and a resumed charge is never credited.

![The retirement window: two columns, the fresh-run rule, the 67-day bound](retirement-window.svg)

## 5. Lanes and limits

`requestLane` (`BadgeService/Service.hs:329-352`) puts each request on one of three serial lanes, where a store call can take up to 10 s. The purchase lane takes Play purchases and App Store purchases whose unverified payload says auto-renewable. The renewal lane takes an `issueBadge` signed by its purchase key whose subscription is due a store check (judged a minute ahead), or whose routing read failed. The main lane takes everything else and never waits on a store, and no purchase, real or made up, can delay a renewal (007 slice 2).

- **Renewal:** one store call per subscription per hour, failures included, which only a purchase-key holder can trigger. No per-minute cap.
- **Subscription purchases, per claim** (run 005 D6; Play in slice 6): App Store claims by original id after the offline check, Play claims by token hash, each asked once an hour whoever presents it; at most 10,000 per store, and a full table asks without recording (`:815-839`); a claim no store was asked about is released (`:553`, `:565`). Rejected: a column on the payment row, a global rate limit, keying by purchase key or unverified text, keying by transaction id. Cost: a first purchase answered "not started" or "unreachable" waits an hour, not 300 s.
- **Play subscription purchases, per minute** (run 007 D6): at most 300 subscription checks a minute on the purchase path, a tenth of Play's default quota of 3,000 that renewals share, counted after the claim check (the acknowledgement's read is not counted); over it, `rate_limited` and the claim is not held (`:841-850`). It uses the web limiter's fixed-window rule, `takeFrom` (`BadgeService/Web/Server.hs:136-140`). Rejected: counting from the claim map, a bucket per purchase key, refusing once the map is full, a pre-filter on the token.
- **Play one-time purchases** also wait on the purchase lane, with neither limit: each made-up token is a `purchases.products.get` call. This predates the PR and run 007 ticketed it separately.
- **Concurrency:** the main and renewal lanes both append to a ledger, guarded by a tip re-read while transactions run one at a time (SQLite, or Postgres at `--pool-size 1`).

![Request lanes and the store limits on each](request-lanes.svg)

## 6. Badge RPC and chat API

**Badge RPC** (`docs/protocol/badges-rpc.md`, `.schema.json`; version stays 1). `badgeCredential` gains an optional `subscription` (`renewing`, `cancelled`, `billingRetry`, `ended`; anything else reads as not ended). The `charge {chargeId}` credit is now written, dated to the store's period start. `purchaseBadge` and `issueBadge` gain the answers of §2 and §3. No error code is new.

**Chat API** (`Controller.hs`, `Library/Commands.hs:6517-6518`). `/_badge purchase` gains `listed`; `/_badge subscription <state> <payment>` is new (left out of the bot API docs); `CRStoreReceiptNotRecorded` and `BREStorePurchaseOpen` are new; `BadgeState.subscription {planMonths, status}` is new and fills `renewsAt` and `willRenew`; alert kind `subscription_cancelled` is new. The apps decode an unknown alert kind or state instead of failing the badge state, which keeps a newer core readable by an older app over remote desktop.

## 7. Types by module

- **Core.** `Badges/Types.hs`: `SubscriptionState`, `BadgeSubscription`, `StoreReceiptOrigin`, `BASubscriptionCancelled`. `Badges/Ledger.hs`: `chargeEntries`. `PaymentService.hs`: `appleTransactionId` renamed `appleTransactionRef`, plus `applePayloadRef` and `AppleTransactionFacts`. `Store/Badges.hs`: `UnrenewedRun`, `PurchaseKind = PKPrepaid | PKSubscription BadgeSubscription`.
- **Service.** `StoreReceipts.hs`: `SubscriptionRef`, `SubscriptionCheck`, `StoreCharge`, the verifier's `checkSubscription` and `acknowledgeGoogleSubscription`, the receipt's `subscriptionRef` and `acknowledgeSubscription`. `Catalog.hs`: `StoreProduct = StoreOneTime | StoreSubscription`, `storePlan`. `Config.hs`: `AppleServerApi`. `Store.hs`: `PurchaseSubscription`. `Service.hs`: `RequestLane`, `PurchaseChecks`, `NotGranted`.

## 8. Database changes

| Database | Table | Change | Migration |
|---|---|---|---|
| Core | `badge_purchases` | `subscription_status`, `unrenewed_since`, `last_unrenewed_at` (SQLite `TEXT`; Postgres `TEXT`, `TIMESTAMPTZ`); the down step first clears `subscription_cancelled` acks, which master cannot decode | `M20261017_badge_subscriptions`, both backends |
| Core | `badge_ledger` | read only: the purchase kind and plan months come from the latest charge credit | none |
| Service | `sx_badge_service_subscription_charges` | new: charge id, payment, store charge ref (Apple transaction id, hashed Play order id), plan, period, amount, currency; unique `(payment_id, provider_charge_ref)` | `20261012_badge_subscriptions`, both backends |
| Service | `sx_badge_service_badge_ledger` | `charge_id`, unique, so the schema grants a charge at most once | same |
| Service | `sx_badge_service_payments` | `subscription_state`, `subscription_checked_at`, `provider_token` (a Play subscription's token, a bearer secret, never logged or returned); drops the never-written `subscription_renews_at`, `grace_until`, `cancelled` | same |

`M20261017_badge_subscriptions` exists because `M20261016_badge_store_receipts` shipped in master (#7587): run 008 restored that file to master's bytes and moved this branch's columns into a new migration, which also lets a downgrade to master run. The SQLite `chat_schema.sql` is regenerated; the Postgres one was regenerated on the host in `ffb036680`.

## 9. The store checks

The state mapping is in the states diagram above.

- **App Store** (`StoreReceipts/Apple.hs:223-397`). An ES256 token for the in-app purchase key, reused for 15 minutes; `GET /inApps/v1/subscriptions/{originalTransactionId}`. Everything Apple signed is checked against the pinned roots, and a failure is `internal`, never `receipt_invalid`. Family-shared: `receipt_invalid`; Sandbox: `test_purchase`; 401 and 403: `internal`; 404, 429, 5xx and timeouts: unreachable. The three key settings go together (`BadgeService/Config.hs:364-370`).
- **Google Play** (`StoreReceipts/Google.hs:129-205`). `subscriptionsv2.get` by token; paused read as active (005 D14); exactly one line item; the charge ref is the SHA-256 of the order id; the period starts the plan's months before `expiryTime`; the offer flag comes from the current `offerPhase`, not `offerDetails`.
- **Dev mock** (`StoreReceipts/Mock.hs`). Renews the plan the product or test token names, kept in memory, so a restart ends its subscriptions; it acknowledges nothing.

## 10. The apps

- **Both.** Subscriptions are handed to core instead of finished uncredited; the sweep and store listener hand over as `listed`, Purchase and Restore as the device's own. After the sweep, a device with a subscription reports its store's state. Your badge shows "Renews {date} · {price}", "Renewing", "Payment problem", "Ends {date}" or "Ended", with Manage subscription; Check your order adds the auto-renewal disclosure, policy links and Restore, which run 005 removed from Your badge.
- **iOS** (`BadgeStore.swift`). Subscriptions are sold only while `Transaction.all` shows none earlier, checked again in `purchase()` (`:195-231`, recheck `:200-206`); a revoked subscription transaction is finished without handing over (`:357`); Manage opens `AppStore.showManageSubscriptions`; Restore runs `AppStore.sync()`.
- **Kotlin** (`BadgeStore.kt`, `PlayStore.kt`). The sweep queries subscriptions too, skipping them only where Play does not support them (`PlayStore.kt:108-116`); a held one-time product is acknowledged, a subscription never (`PlayStore.kt:252-259`); Manage opens Play's page; desktop and remote hosts show the state without store rows.
- Run 003 plan §4 lists the 15 deliberate platform differences.

## 11. Alerts

Core derives one alert per purchase, the first row that applies (`derivedBadgeAlert`, `Library/Commands.hs:5743-5759`); both apps show it in the single chat-list banner slot, and it can be dismissed or snoozed for a day.

| Kind | Raised for a subscription when | Banner |
|---|---|---|
| `subscription_ended` | past paidThrough and no longer shown (retired by `ended` or the window) | "Your subscription ended" |
| `payment_issue` | state `billingRetry` | "Badge payment problem", warning |
| `issue_failed` | a renewal failure that cannot clear, or any once the shown credential has expired | "Badge renewal failed", warning |
| `subscription_cancelled` | state `cancelled`, paidThrough still ahead | "Subscription cancelled" |
| `renewal_approaching` | annual plan, state `renewing`, the last 3 days before paidThrough | "Your badge renews soon", with the price |

A subscription still shown past paidThrough raises no `support_ended`: it is waiting for its store. Monthly plans get no reminder, as reminders cannot be turned off (003 P9).

## 12. Tests

**Added:** 76 examples. `tests/BadgeTests.hs`, 6 (charge rows on every renewal path, JSON, badge state, alert derivation, Apple refs). `tests/Bots/BadgeService/BotTests.hs`, 49, mostly end to end (purchase, renewal, grant once, the hourly bound, billing retry, ending, the retirement window in 8 tests, lanes and limits, adoption, listed hand-overs, device reports, alerts). `StoreVerifierTests.hs`, 17, against a new fake App Store Server API and FakePlay's subscription routes. `ConfigTests.hs`, 2. `WebTests.hs`, 2 (the migration's down and up, a charge credit's round trip), plus one changed.

**Last recorded results.**

- GitHub CI `build` on `d50488460`, the last commit with Haskell code, was **cancelled**. GHC 9.6.3 on Ubuntu 24.04: 1,805 examples, 0 failures, 46 pending; GHC 8.10.7: 1,788 and 0; Ubuntu 22.04 with 9.6.3: 1 failure, in a profile test this PR does not touch (the optional "subscribed 2 connections" line). On macOS arm64 (`macos-latest`) the suite had 7 failures (the schema down-migration and strict-tables checks, three existing badge-service tests, a directory-bot test and a chat-expiry test), then printed nothing for 73 minutes until a maintainer cancelled it; master passed 1,729 examples with 0 failures on that runner, and the cause is not known. Intel macOS and Windows only built. CI skips "Save query plans".
- Container, run 009: build clean; the 682 badge examples had 6 failures under load, all passing rerun alone.
- The last full local suite that finished was run 006's, at `d479e5871`, before run 007 and both master merges: 1,675 examples, 1 failure (the agent query plans' XFTP line).

**Never built or run.** No full local run has regenerated `chat_query_plans.txt` since `d479e5871`; run 007 hand-edited it and run 009 merged it as a union. Postgres: the host regenerated the core's Postgres `chat_schema.sql` (`ffb036680`), but no report records the `+client_postgres` badge-service build or its suites, and the deployed service is Postgres-only. No report or CI run records a Swift or Kotlin build: `ios-sim` and `desktop-linux` were skipped on push, and CI's Desktop step did not run. No real store was used; the fakes follow the documentation.

## 13. What the reviews changed, and where the code differs from the plan

**Reviews.** Run 001: an App Store renewal tested through `issueBadge`, the unique charge index. Run 003: the iOS "subscribed before" guard kept fresh and rechecked at purchase (a P0), store reports only from devices with a subscription. Run 004: Play's 410 read as ended, Play order ids hashed; it escalated unbounded App Store calls on the main lane and a request at every activation, both fixed in run 005 (lanes, the claim table, the stored wake), whose own review released a claim no store was asked about (C2-R1-1, a P1). Run 006: retirement moved after an answer (R4-1, a P0), the clipped-period guard; it escalated R5-1, one answer after months away retiring a payer, which run 007 fixed with the first-unrenewed-answer rule. Runs 007 and 008: wording, two tests, the migration split. Run 009 merged master with no review loop.

**Against the plan.** The plan ruled out app screens, load bounds and a client column; the PR has all three. The purchase kind is the latest charge's plan, not an `EXISTS` flag. A stalled request waits for the next daily moment of the shown expiry, not "a day later". The plan retired only on `ended`; the code adds the 67-day window. A first purchase not started or pending waits an hour, not 300 s. Play's pause is read as active, Play's 410 as ended, and order ids are hashed.

## 14. Open items and accepted risks

Each was checked against the code at `ffb036680`.

1. **macOS CI** (above): rerun `build` on `macos-latest` before merging; the failures and the stall are unexplained.
2. **Acknowledged but not credited.** The service acknowledges a Play purchase before it signs and writes (`BadgeService/Service.hs:574-584`; one-time purchases at `:534`). If signing or the write fails, the subscription is paid and no longer refundable, and only a later retry, after the claim's hour, credits it; a persistent failure leaves the buyer with nothing.
3. **A Postgres pool above one** needs a row lock on the purchase (005 §9). Still true: `renewSubscription` only re-reads the tip (`:760-772`). Keep `--pool-size 1`.
4. **Store behaviour from documentation only:** Apple's 60-day retry and `purchaseDate` as period start; Play's 60 days of grace and hold, pause report, 410 and `offerPhase`; the 3,000-a-minute quota.
5. **A Play pause longer than 67 days** retires the badge early, and the resumed charge is never credited (run 005 plan §8).
6. **Run 005 D6's cost:** a first purchase answered "not started" or "unreachable" waits an hour, not 300 s, and a held record adopting a later transaction waits out its hour. Still true (`:543-556`).
7. **C2-R1-3:** the held answer's `retryAfter` tells another holder of the same JWS when it was last presented. Still true (`:546`).
8. **Run 007's costs:** the two residuals of §4; "a service that never answers" read as `provider_unavailable` only; and under a sustained flood of made-up Play tokens, genuine Play buyers get `rate_limited`, and past Play's three-day acknowledgement window the purchase is refunded automatically, so the buyer keeps the money and loses the badge. Made-up Play one-time tokens are not limited at all (§5).
9. **A held receipt behind a shown badge** (open since 2026-10-15). Still in the code: the worker selects held receipts only for profiles with no shown badge (`Store/Badges.hs:260`), while Android acknowledges a held one-time product anyway (`BadgeStore.kt:281`), so such a purchase waits until that badge retires and loses Play's automatic refund. The PR narrows the ways in (codes and invoices refused while a held receipt may be credited, `Library/Commands.hs:5440-5449`; listed hand-overs record nothing) and never acknowledges a subscription on the device, so a parked Play subscription is refunded after three days. An Ask to Buy approved after a code was redeemed still waits (003 P7); for an Apple subscription, Apple charges every period meanwhile and only the charge current at crediting is granted (`:619-634` reads the latest charge only). Deferred with top-ups (007 U4).
10. **Service timeouts** keep their 30 s to one hour backoff after activations (007 U3, accepted for the MVP).
11. **Test devices** that ran an earlier build of this branch or of `f/badge-in-app-dev-service` fail the new migration ("duplicate column name: subscription_status"); reset their data (008). Run 007's merge-order risk is gone with the split.
12. **Before sale** (plan §11, not built): resubscription and restore on a new device (iOS refuses to sell to an Apple ID that subscribed before), tier changes (another tier's charge is never granted), refunds (a refunded annual keeps issuing), spreading the weekly request moment (`Library/Commands.hs:5890-5892`). Store setup: Apple's Billing Grace Period off; Play's grace at its minimum, account hold, and subscription pausing off, which the 67-day bound assumes; no trial or introductory offers; Family Sharing off; the App Store Server API key and a Play service account with order permissions; quotas; store review. Store notifications are listed there too, but not as blocking sale.
13. **One `ended` is final:** a store briefly reporting an expiry around a renewal loses the renewals after it (plan §8).

## 15. Merging as it stands

- **`a2e9456e6` "use the dev badge page for testing (don't merge)"** reverts master's `c998eb1fe`: `badgePageUrl` on iOS (`BadgeStore.swift:33-35`) and Kotlin (`BadgeStore.kt:52-57`) points at the dev deployment `smp7.simplex.im`, marked TEST ONLY, so buy-in-browser would open the dev service, for every desktop and foss buyer among others. Revert it before merging.
- **`ea359edb6` "choose a badge in the app"** exactly reverts master's `898c6f126`. Where a store is available, Support SimpleX leads to Choose your badge, How long and Check your order again, so in-app buying, one-time and subscription, is reachable; desktop and foss show Buy in browser and Redeem code. Master hides in-app buying because badges were released without in-app purchases (2026-10-16). It is not marked, but merging it ships in-app sale; revert it unless that is intended.
- **Seven `fixup!` commits** are not folded (`93dd1c159`, `d2c36aaa6`, `823a17267`, `908922060`, `bceaf06a4`, `e2249c64a`, `ebc0c532b`). The PR is squash-merged, so their changes reach master inside the squash; folding them first would mean rebasing across up to three merges.

## 16. Review map

★ marks files to read carefully: money first, then state, then concurrency. Line counts are `+added −removed` against the merge base.

| Layer | File | Lines | What it does | Read |
|---|---|---|---|---|
| Service | `BadgeService/Service.hs` | +353 −85 | lanes; subscription purchase path; grant rule; renewal; per-claim and per-minute limits | ★ money, concurrency |
| Service | `BadgeService/Store.hs` | +105 −24 | payment rows with state and token; charges; ledger `charge_id` | ★ money, state |
| Service | `BadgeService/StoreReceipts/Google.hs` | +139 −47 | `subscriptionsv2` read and mapping; period; acknowledgement; 410 | ★ money |
| Service | `BadgeService/StoreReceipts.hs` | +118 −20 | receipt shape and the two acknowledge paths; store calls, timeouts, answer classes | ★ money |
| Service | `BadgeService/StoreReceipts/Apple.hs` | +223 −15 | App Store Server API client; token; signed status checks and mapping | ★ trust |
| Service | `BadgeService/Store/{SQLite,Postgres}/Migrations.hs` | +62 −1 each | `20261012_badge_subscriptions` | ★ schema |
| Service | `BadgeService/Catalog.hs` | +38 −9 | subscription products and plans | check the ids |
| Service | `StoreReceipts/Mock.hs`, `Config.hs`, `Web/Server.hs` | +51 −7, +28 −4, +10 −10 | dev mock; `[apple]` API key; `takeFrom` | skim |
| Core | `Library/Commands.hs` | +209 −68 | pass, retirement window, held wake and stall, alerts, hand-over, device reports, purchase guards | ★ state, money |
| Core | `Store/Badges.hs` | +81 −19 | receipt adoption, listed, purchase kind, unrenewed run | ★ state, concurrency |
| Core | `Badges/Ledger.hs` | +18 −5 | `chargeEntries`; charge column read | ★ money |
| Core | `PaymentService.hs` | +50 −5 | Apple claim ref; unverified facts | ★ claims |
| Core | `Store/{SQLite,Postgres}/Migrations/M20261017_badge_subscriptions.hs` | +28, +29 | client columns; down step | ★ schema |
| Core | `Badges/Types.hs`, `Badges/Service.hs`, `Badges.hs`, `Controller.hs`, `View.hs` | +60 −4, +3 −1, +1, +5 −2, +6 −2 | types, wire field, API, terminal view | read |
| Core | `Migrations.hs` (2), `simplex-chat.cabal`, `M20260915_user_badges.hs` (2) | small | registration; draft comment | skim |
| Core | `chat_query_plans.txt`, `chat_schema.sql` (2) | +50 −3, +3, +4 −1 | generated and hand-edited | check the plans |
| iOS | `BadgeStore.swift` | +146 −38 | sale guard, hand-over, revoked finish, reports, Restore, Manage; dev URL | ★ money |
| iOS | `ChatTypes.swift` | +72 −1 | decoders for unknown alert kinds and states | read: decoding compatibility |
| iOS | `BadgesYourBadgeView.swift`, `BadgesCheckOrderView.swift`, `ChatListView.swift`, `BadgesHowLongView.swift` | +54 −8, +68 −1, +48 −30, +17 −3 | Subscription section; disclosure, links, Restore; banner; period | read |
| iOS | `BadgesSupportSimplexView.swift` | +6 −26 | in-app choose restored (`ea359edb6`) | read for merging |
| iOS | `AppAPITypes.swift`, `SimpleXAPI.swift`, `ChatModel.swift`, `APITypes.swift`, `spec/client/chat-list.md` | small | API; spec | skim |
| Kotlin | `BadgeStore.kt` | +122 −47 | acknowledges one-time only; reconcile; reports; Restore; dev URL | ★ money |
| Kotlin | `android/src/google/…/PlayStore.kt` | +34 −11 | subscription query; base-plan offer; finish; Manage | ★ money |
| Kotlin | `ChatModel.kt` | +82 −10 | serializers for unknown alert kinds and states | read: decoding compatibility |
| Kotlin | `BadgesYourBadgeView.kt`, `BadgesCheckOrderView.kt`, `ChatListView.kt`, `OnboardingCards.kt`, `BadgesHowLongView.kt` | +81 −4, +67, +42 −23, +13 −8, +8 −5 | as on iOS | read |
| Kotlin | `BadgesSupportSimplexView.kt` | +11 −36 | in-app choose restored (`ea359edb6`) | read for merging |
| Kotlin | `SimpleXAPI.kt`, `strings.xml`, foss `PlayStore.kt`, `SimplexApp.kt`, `Platform.kt`, specs (2) | small to +39 | API, strings, stubs | skim |
| Docs | `badges-rpc.md`, `badges-rpc.schema.json`, service `README.md`, two `.ini.example`, bot API docs, TS and Python types | small | wire and config docs; generated types | read `badges-rpc.md` |
| Tests | `BotTests.hs`, `StoreVerifierTests.hs`, `BadgeTests.hs`, fakes, `ConfigTests.hs`, `WebTests.hs` | +1,652 −35, +486 −15, +241 −12, +264 −15, +35 −1, +45 −3 | §12 | spot-check |
