# Supporter Badges v3 — UX-first plan

**Date:** 2026-07-30
**Status:** draft — UX gaps are the current focus; model follows from UX
**Transport:** service RPC, implemented (`plans/2026-07-22-service-rpc-chat.md`, branch `rpc`): signed requests (`APISendServiceRequest.signKey`), verified signer key delivered to the service (`CEvtServiceRequest.signerKey`), one response per request, per-call timeout, no transport persistence — the client repeats the same signed request after a timeout, so every bot operation must be idempotent.
**Designs:** `/code/simplex-chat-art/screens/badges-mobile_15-*.jpg`, `_16-*-dark.jpg`

## Principles

- A profile has one **shown** badge; it may hold two (a paid supporter/legend badge and a permanent investor badge). The user chooses which is shown. Presentation timing questions are asked only when advanced privacy questions are enabled (2.5); the default behavior is immediate.
- **Core-driven:** all badge automation (claims, re-issuance, presentation, alert detection, scheduling) lives in core and surfaces as chat events. The app makes one status call to render UI, and API calls only for actions that need user confirmation (buy, cancel, switch, pause, acknowledge, redeem). Everything else just happens.
- **No bot push, considered and rejected:** store push (APNs/FCM) from the bot would bind a device push token to an order — the caller identity this design eliminates — and the Play build has no FCM at all (deliberate). Nothing in scope needs push: prices are pull-on-intent, alerts are client-derived, provider events reach the bot via webhooks and the client at its next check-in. If bot-initiated messaging is ever needed, the channel is an ordinary **opt-in SimpleX contact** riding the messenger's existing e2e notification path — never a direct push token.
- **Paid time is a months balance on a single-entry ledger** (§3): grants from payments, charges, codes, and adjustments; debits from monthly issuance and calendar lapse, computed at recording time. Use-it-or-lose-it by default; **pausable** for prepaid time (2.13 — post-MVP, §7). Annual subscriptions, multi-month one-time payments, and redeem codes are all drawn down as monthly credential re-issues — indistinguishable from monthly subscribers to peers.
- **One alert at a time:** alert kinds form a chain per badge (renewal approaching → payment failed → ended → expired); a later state supersedes an unacknowledged earlier alert. At most one current alert per profile, derived from state — never a queue (2.4).
- No dark patterns: renewal reminders on by default (opt-out), cancellation always visible, unlimited pause, no fake urgency, no celebration interstitials — the confirmation of success is the badge appearing on the profile.
- Purchase-time privacy: credential expiry rounds up to the week boundary — badges end on Sundays (UTC); profile updates go out on Mondays, including removal updates for expired badges (2.11). Payment and issuance cadence stays monthly; only the rounding is weekly.

## 1. What the existing designs cover

| Screen | Covers |
|---|---|
| `banner` | chat-list banner: "Support SimpleX — Get badge + files up to 5GB", dismissable |
| `support-simplex` | intro: funding story, "Why SimpleX is built", → Choose your level |
| `your-level` | tier choice: Supporter $7/month (2GB) vs Legend $70/month (5GB), profile preview with badge next to the name |
| `pay` | plan choice for a tier: "1 month" vs "Subscribe", Pay button, "Renews on July 22, 2026. Cancel anytime." |
| `settings` | "Supporter perks" row in Your settings |
| `whats-new` | v7.0 announcement entry |

This is the happy acquisition path, and it is correct **only for iOS/Play builds**. Missing designs: payment method + duration selector (2.1), crypto invoice screen (2.1), badge presence in the user picker (2.3), management screen (2.6), start-sharing question (2.5, only under advanced privacy questions).

Prices shown in store builds must be the store-fetched localized prices (stores bill in the storefront's local currency), not the catalog display price — the catalog price applies to Stripe/BTCPay only.

## 2. UX gaps

### 2.1 Payment method and duration (desktop / F-Droid builds)

The `pay` screen assumes store billing. Non-store builds pay by card (Stripe), BTC, or XMR (both BTCPay) — and crypto cannot auto-renew, so the method constrains the plan. The method must be chosen **on the same screen as the plan**:

- Store builds: no method UI (store rules require store billing for digital goods). One-time = 1 month; Subscribe = monthly/annual.
- Non-store builds: method selector (Card / Bitcoin / Monero), all multi-month prepaid — **no card subscriptions** (§6 resolved 11): with no email and no account, an unattended subscription cannot be cancelled after device loss — an unbounded recurring-charge liability, where prepaid loss is bounded by what was paid. The plan screen explains "payments don't renew — you choose how many months to pay for".
- **One-time payments on Stripe/crypto buy a duration: 1 / 3 / 12 months** (duration selector — new design). Paid months are granted to the ledger and drawn down by monthly re-issue, so a 12-month crypto payment looks like a monthly subscriber to peers. Pricing (§6 resolved 12): 1 = monthly price; 3 = 2 × monthly (3-for-2, 33% off); 12 = 6 × monthly (2-for-1, 50% off). The store annual subscription carries the same 2-for-1 in the consoles — safe from cancel-gaming: voluntary early cancellation runs to period end without a refund on both stores, and store-support refunds land in the existing clawback.
- Crypto payment screen (new design): amount + one-off address + QR + copy + open-in-wallet, countdown for the invoice window (BTCPay pins the fiat-to-crypto rate at invoice creation; the invoice expires in ~15–60 min). States: waiting → seen-unconfirmed ("processing") → settled; expired → "Invoice expired" with Get-new-invoice (new payment row, new invoice). Fiat-first price display, crypto amount at invoice time. Partial payment: show BTCPay's marked state, route to support. After settlement the claim returns a **receipt code** (§3 recovery) — issued for every Stripe and crypto payment (store payments recover via the store account instead); the checkout flow shows a one-time save prompt ("keep your receipt — it recovers unused months if you lose this device"), most prominently for crypto and multi-month card payments; receipts stay viewable in charge history (2.6).
- Stripe payment: opens the payment link in the system browser; the badge screen shows "Waiting for payment…" with Open-link-again; the link persists in the payment row and survives restarts.

### 2.2 Chat-list banner rules

Show after the user has ≥ 3 conversations and not in the first days after install; dismiss hides it for ~6 months; never shown while a badge is active or a purchase is in flight; expiry does not re-trigger the banner (that is an alert). Exact thresholds are a local decision at implementation time — no product decision needed now.

### 2.3 User picker — badge presence (design exploration needed)

The menu opened from the avatar in the chat-list toolbar (`apps/multiplatform/.../chatlist/UserPicker.kt`, `apps/ios/Shared/Views/ChatList/UserPicker.swift`) shows the current profile bar (renders `localBadge` — UserPicker.kt:213) and option rows.

Leading option — **badge icon, not a row**: a badge glyph on the right of the current profile bar, opening the management screen. It can carry a small indicator dot for the current alert state (payment issue / expiring / expired), at the risk of being too fiddly at that size — needs design. The icon appears when a badge is held or a payment is in flight; acquisition entry for users with no badge stays with the chat-list banner (2.2) and the Settings "Supporter perks" row.

Fallback option: a full option row in the picker (badge state text: "Supporter · until Oct 1", "renews Aug 21", "⚠ payment problem"), replacing the Settings row, with "Your chat profiles" moving to Settings to make room.

Open until designed; the icon is preferred if the alert indication reads well.

### 2.4 Alerts — one current alert, superseding

Alert kinds and when they fire:

| Alert | When | Content / actions |
|---|---|---|
| subscription renewal approaching | 3 days before `renews_at`, once per renewal cycle | price + date; OK · Remind me again (re-alerts 1 day before) · Cancel path |
| payment failed / grace | on provider grace/hold state, once per episode | "Payment problem — badge valid until {expiry}. Fix payment." → store/Stripe fix flow |
| subscription ended (cancelled earlier, paid period over) | once | "Subscription ended. Badge valid until {expiry}." + Resubscribe |
| prepaid months exhausted | when the ledger balance reaches 0, once | "Paid period ended. Badge valid until {expiry}. Renew for {price}?" |
| badge expired | at credential expiry with balance 0, no active subscription, not paused, once | "Badge expired." + Renew / Choose level |
| badge expiring (one-time/prepaid, no renewal path pending) | 3 days before credential expiry, once | OK · Remind me again (1 day before) |
| renewal charged successfully | **never** — silent; visible in charge history and the picker badge state | — |
| new monthly credential issued | never | — |

**Paused badges never alert** (2.13) — pause is an explicit user state; the management screen carries it instead.

**Multiplicity analysis.** Lifetime badges never alert; one live paid badge per profile — so all alerts stem from one badge + its active payment, and the kinds form a chain in state order. Two alerts can only overlap through accumulation while the app was closed (stale "renewal approaching" + now-true "subscription ended"), and the older one is then obsolete by construction. Rule: **a later state supersedes an unacknowledged earlier alert; at most one current alert per profile.** Multiple profiles: each alert belongs to its profile and shows in that profile's context (indicator on the profile row in the picker).

**Persistence.** No alerts table — the current alert is a pure function of badge/payment/ledger state. Stored on the badge row only: `alert_acked_episode` (the episode value — e.g. this cycle's `renews_at` — that the user acknowledged) and `alert_snooze_until` ("Remind me again"). Core emits `CEvtBadgeAlert` when derived-alert ≠ acknowledged; OK writes the episode; snooze re-emits once after the timestamp.

Opt-out: "Remind me about renewals and expiry" toggle on the management screen disables the two reminder kinds (renewal approaching, badge expiring); problem alerts (payment failed, ended, expired) are not disableable.

### 2.5 Start-sharing — a setting, not an interstitial

Presenting a new credential mid-month reveals the purchase/upgrade date to contacts, but asking every buyer about it is too complex for most people. Decided:

- Default: a newly acquired badge (purchase, upgrade, redeem) is presented immediately; switching the shown badge takes effect immediately. Perks apply from issuance in all cases.
- Badge settings (management screen) carry a static toggle: "Show new badges from Monday" — off by default.
- A new global setting, **"Show advanced privacy questions"** (Privacy & security), gates the interactive version: when on, acquisitions that change the presented badge ask "Start showing: Now / From Monday (better privacy)". When off, the static toggle governs silently.
- While presentation is delayed: the previously shown badge keeps being presented; the management screen shows the new badge as "starts Monday {date}"; model field `use_from`.

The global setting is a cross-cutting UX concept beyond badges — other candidates to tie to it: notify contacts on deletion, notify rejected contact requests, call privacy questions. Those migrations are out of scope here; the setting is introduced by this feature.

### 2.6 Management screen — no design exists

States, all derived from core status:

1. **Acquiring / payment pending** — method-specific pending UI (2.1), old badge untouched.
2. **Active prepaid** — badge, "Valid until {expiry}", "{N} months left", Pause (2.13), Subscribe entry, Upgrade entry.
3. **Active subscription** — badge, "Valid until {expiry}" and separately "Renews {renews_at} · {price}", Cancel, Upgrade, charge history (last 12: date, amount, method — amounts in the charged currency as reported by the provider; Stripe/crypto payments expose their receipt, §3), reminders toggle.
4. **Cancelled, still active** — "Subscription ends {paid_until}", "Badge valid until {expiry}", Resubscribe. Two distinct dates, always both.
5. **Payment issue** — banner over state 3; badge stays active to expiry.
6. **Paused** — badge (dimmed once the credential expires), "Paused · {N} months left", Resume (2.13).
7. **Expired** — dimmed badge, "Expired {date}", Renew / Choose level.
8. **Held badges — has vs shows.** The screen must expose the separation the model makes: the list of badges the user *has* (paid + investor, each with its validity and source), the radio for which one is *shown* on the profile, and the pending-presentation state ("starts Sep 1", 2.5). Switching triggers a profile update.
9. **Redeem code** entry (2.8).
10. Badge settings: reminders toggle (2.4), "Show new badges from Monday" (2.5).

### 2.7 Held badges and switching

- Perks: max over held active badges, independent of which is shown.
- Shown: exactly one; a switch triggers the profile update with a fresh proof (existing `setUserBadge` + broadcast mechanics — the `contact_profiles` badge columns keep their role as the presentation copy of the shown badge). Timing per 2.5 (immediate by default; question only under advanced privacy questions).
- When the shown paid badge expires and an investor badge is held: switch to investor automatically with the monthly profile update (no extra alert beyond 2.4's expiry alert).

### 2.8 Redeem codes — all badge types

The user enters a code; the service responds with the badge type **and granted months** — the code itself encodes nothing client-side. Codes can grant any badge type for 1/3/12 months (investor: lifetime). Granted months enter the ledger like any other grant: the client re-issues credentials monthly, so redeemed duration is not leaked to peers. UX: "Have a code?" on the management screen → paste → signed claim (fresh order, payment = code) → badge appears (presentation per 2.5). Errors: invalid/used/expired — plain inline errors.

### 2.9 Core engine and events

All automation is in core; nothing is app-driven. Alternatives were an app-side controller calling core APIs (rejected: duplicated per platform, dies with the app lifecycle) vs a core engine emitting events — core engine chosen.

Core engine, triggered by chat start, foreground, network restore, profile switch, and its own timers (renewal/expiry/weekly Monday boundaries):

1. resume/claim: unclaimed settled payments, subscription renewals after `renews_at`, monthly re-issue while the ledger balance is positive and the badge is not paused, unfinished crypto invoices;
2. ledger recording: at each claim/re-issue, record elapsed unpaused months as `lapse` entries and the issued month as an `issue` entry (deduction is determined when recorded — §3);
3. presentation: on/after Monday (UTC), broadcast profile updates — fresh proofs for renewed badges and arrived `use_from`, removal updates for badges that expired unrenewed (2.11);
4. alert derivation per 2.4 and `CEvtBadgeAlert` emission;
5. reconcile charges and grants: pull charges/adjustments since `lastChargeRef` with each claim/status.

Events to the app: `CEvtBadgeChanged` (any state change worth re-rendering: issued, presented, payment status, subscription status, balance), `CEvtBadgeAlert` (the current alert). The app renders; it never decides.

API calls — user-confirmed actions only: get status (render), start purchase / get invoice, claim after store payment (store flows return evidence to the app, which hands it to core), cancel (opens the store management sheet — no bot op, §4), pause / resume, switch shown badge / set start date, acknowledge / snooze alert, set reminder and presentation toggles, redeem code.

Claims are the single idempotent RPC (§4); on `ASETimeout` core retries the identical signed envelope on the next trigger — never on a poll timer.

### 2.10 Upgrades

- supporter → legend (store subscription): native upgrade flow (same subscription group, store prorates). New badge row (new keys), old row superseded.
- one-time / crypto / prepaid upgrades — **decided: deduct the full supporter price while the supporter badge is unexpired (even past its renewal point)**: legend invoice = legend price − supporter price paid. The bot computes the discounted invoice; remaining supporter months are closed with a `conversion` adjustment (§3). The client displays both numbers.
- one-time → subscription (same tier): same badge row, new payment row, current payment repoints — no new credential until the next period.
- Presentation timing of the upgraded badge per 2.5 — including investor → legend.

### 2.11 Expiry rounding, presentation day, recipient-side display

- **Week-boundary expiry**: credentials expire on Sundays (end of Sunday, UTC) — paid-through rounded up to the next week boundary. Rationale: end-of-next-month rounding gave 0–31 free days (~+50% average) — an implicit discount that undercut multi-month pricing and rewarded delaying purchase to the 1st; weekly rounding gives 0–6 days (~+10% average) and no timing incentive. Chosen over 10-day windows for fixed weekdays and calendar legibility.
- **Anonymity set**: expiry dates have weekly granularity — the cohort is a week's renewals rather than a month's. Acceptable: first presentation already reveals acquisition timing to existing contacts (2.5 defaults to immediate); the disclosed expiry is what future contacts learn, and week granularity there is a modest, deliberate trade.
- **Monday send-outs, uniform**: profile updates go out on Mondays — fresh proofs for renewed badges, **removal updates for badges that expired unrenewed** (existing `setUserBadge Nothing` mechanics). Uniform behavior prevents riding a stale badge, and the update's existence signals nothing beyond what the badge shows.
- **Recipient grace**: a peer badge with expiry E displays as active until **E + 7 days** — purely delivery-lag cover, since the Monday update normally clears or replaces the badge within a day of expiry. The dim-then-hide boundary (`BSExpiredOld`, currently 31 days) shifts by the same 7 days. Both live in the recipient's `mkBadgeStatus` display derivation; the credential's real expiry is untouched.

### 2.12 Multi-profile

Badges, payments, charges, and ledgers are per chat profile (`user_id`); the `your-level` design already shows the profile preview so the user sees which profile the badge lands on. Incognito profiles never present a badge. Profile switch re-runs the engine triggers (2.9).

### 2.13 Pause (prepaid only)

The issued credential cannot be paused — its expiry is hardwired and signed. Re-issuance can be: pause freezes both issuance and calendar lapse, preserving the months balance **without any duration limit** (a balance costs nothing to hold — the pause caps other services impose are their choice, not a necessity).

- Prepaid time only (crypto, Stripe one-time, codes). Subscriptions keep charging at the provider and cannot be paused by us; Google Play's own pause exists in the Play UI, out of our scope.
- Pause is a **signed RPC op** — the bot computes lapse independently and must record the pause, or it would lapse the months itself.
- While paused: current credential runs to its natural expiry; peers see an ordinary expiry (indistinguishable from lapse); no alerts (2.4); management screen shows "Paused · {N} months left" + Resume.
- Resume: issuance restarts at the next claim; the first credential after resume follows the normal week-boundary expiry rule (2.11).

## 3. Model

### `products` + `offers` — the two-tier catalog, not UI-driven

Tier 1 — **what you get**. `products`: `product_id` PK · `product_type` (`badge`) · `badge_type` (`supporter|legend|investor`) · `active`. Used by all builds for structure; display names and localization stay in app resources — the catalog carries codes, not strings.

Tier 2 — **how you buy it**. `offers`: `offer_id` PK · `product_id` → products · `plan` (`one_time|monthly|annual`) · `months` (1/3/12 for one-time; NULL for subscriptions) · `apple_product_id`, `google_product_id` (store offers; price comes from the store) · `price`, `currency` (non-store offers; no external ref needed — Stripe PaymentIntents and BTCPay invoices take amount directly, so there is no `stripe_price_id` unless card subscriptions ever return) · `state` (`active|deprecated|disabled`)

- **Offers are append-only**: repricing or changing a discount is a new offer row plus deprecation of the old — never a mutation (payments reference `offer_id`; history must stay true). Lifecycle: `active` (rendered + accepted) → `deprecated` (hidden, but `invoice` still accepts it — the grace window for a user who saw the old price; window length is operator policy) → `disabled` (`invoice` rejects with a refresh-catalog error). Offer state is enforced only at invoice creation: already-created invoices pin their price, and `claim` on a settled payment is always honored regardless of offer state. Store offers use the display half only; their sale lifecycle lives in the store consoles.
- **Propagation**: the `catalog` response carries `active` and `deprecated` offers (with state) and omits `disabled`. The client renders `active` only, must keep `deprecated` (they price and invoice an already-chosen variant during the grace window), and may delete offers absent from the response — except offers referenced by a local payment row, which stay for reference integrity.
- The duration selector (2.1) renders data-driven from active one-time offers; discount tiers are per-duration prices on offer rows — the multi-month pricing decision is catalog content, not code.
- Mirrors provider structure: Play Billing models subscriptions as base plans/offers under a product; Apple products sit in a subscription group — store SKUs map 1:1 to offer rows.
- Store one-time is 1 month only — no store multi-month SKUs; subscriptions cover longer commitments (§6 resolved 12). Annual is supported: credentials are issued monthly regardless, so no plan creates a correlatable expiry cohort. Seeded from app config; server-authoritative on the bot; `catalog` returns both tiers under one version.
- **Forward compatibility**: new offers of known products go live via the catalog with no release; new product types are release-gated by design (assets, perks, purchase flow). A client ignores catalog entries with unknown `product_type`/`badge_type` — stored, never rendered, no error; the user sees them after upgrading. Same pattern as peer-side `BTUnknown` (Badges.hs:88).

**Price distribution — no push, no polling.** The authoritative price moment is invoice creation (BTCPay pins fiat at invoice time; the Stripe link shows the real charge; checkout always confirms the final amount), so stale prices can never mischarge — only display can go stale. Display freshness comes from two channels that add no scheduled traffic: the unsigned `catalog` op fetched when the user opens the purchase screen (user-triggered, the client contacts the bot moments later anyway), and a catalog version/hash piggybacked on `claim`/`status` so badge holders' renewal prices refresh with their natural check-ins. The `catalog` call is **non-store builds only** (store prices come from StoreKit/Play Billing, no RPC) and **never blocks**: the screen renders instantly from the cached/shipped catalog, the response updates it in the background — purchase intent is the only trigger, a handful of calls per user lifetime. The shipped catalog is the offline fallback and first-run display for non-store prices — but **all builds use it for product structure**: which tiers/plans exist and the store SKU ids to query (both store APIs fetch products by identifier list). Store builds never use its price/currency: store prices are outside this channel entirely — set in the consoles, fetched live from StoreKit/Play Billing, with store-side consent machinery for subscription price increases. Prepaid semantics make repricing trivial — the paid price is pinned at invoice time; catalog changes affect only future purchases; discounts are catalog edits (campaigns can later use redeem codes).

### `payments` — user-initiated acts only

`payment_id` PK · `user_id` · `badge_id` → badges · `offer_id` → offers (the exact variant bought) · `months`, `amount`, `currency` (copied from the offer at purchase — offers are append-only (§ catalog), the copies keep payment history self-contained) · `provider` (`apple|google|stripe|btc|xmr|code`) · `provider_ref` (Apple original transaction id / Google purchase token / Stripe intent ref / BTCPay invoice id / code hash) · `invoice_url` (Stripe link / BTCPay checkout link) · `evidence` (Apple JWS / Google token, kept to re-claim) · `status` (`new|invoiced|pending|settled|failed|expired`) · `renews_at` (subscriptions) · `cancelled` (bot-confirmed renewal-off) · timestamps

One row per act: purchase, subscribe, upgrade, resubscribe, each crypto invoice, code redemption. Abandoned attempts stay as history. Settlement posts a grant to the ledger; there is no stored `paid_until` — remaining time is the ledger balance, and display dates are derived.

### `charges` — provider-initiated billing events

`charge_id` PK · `payment_id` → payments · `provider_charge_ref` (Stripe invoice id / Apple transaction id / Google order id) · `period_start`, `period_end` · `amount`, `currency` · `charged_at` · unique (`payment_id`, `provider_charge_ref`)

Multi-currency by nature: stores bill in the storefront's local currency and report it in the evidence (StoreKit 2 JWS `price`/`currency`, Play `priceCurrencyCode`); rows store amounts as reported. Populated from bot status/claim responses: the client sends the last charge ref it has; the bot returns all later charges plus the latest credential. Canonical provider sources: Stripe invoices listed by subscription; Apple Get Transaction History by original transaction id; Google per-renewal order ids. The first charge of a subscription is also a row. Each settled charge posts a 1-month grant to the ledger.

### `badge_ledger` — single-entry months accounting

The account unit is a **month of badge time**; money stays in `payments`/`charges` (multi-currency); base-currency valuation is bot-side reporting, not client state.

`entry_id` PK · `badge_id` (the account) · `entry_type` (`grant|issue|lapse|adjustment`) · `months` (signed) · `payment_id` / `charge_id` (source refs, for grants) · `reason` (adjustments: `goodwill|refund|conversion|transfer_in|transfer_out|correction`) · `created_at`

- **Append-only; balance = Σ months; invariant balance ≥ 0.** Single entry, not double: we hold one side of the relationship — the counterparty leg (deferred revenue vs recognized) is operator bookkeeping on the bot's own books, invisible to the protocol. Same structure as a bank statement or a wallet transaction list.
- **Deduction is determined at recording time** (use-it-or-lose-it): when the engine records, elapsed unpaused calendar months without issuance become explicit `lapse` entries; the issued month is an `issue` entry. Lapsed time is visible and explainable ("3 months lapsed while offline"), not silent date arithmetic. Pause (2.13) stops the clock for both.
- Grants: settled one-time/crypto payment → `+months`; each settled subscription charge → `+1`; code → `+months`; adjustments as below.
- The bot keeps the authoritative ledger per order (it must — crypto/code entitlements have no provider to re-derive from); the client mirrors it and records deterministically between syncs; each claim/status reconciles.
- The structure generalizes (name service, community credits, wallet views) as a *pattern* — per-feature tables, not one shared table: shared tables couple unrelated features' migrations, backup, and sensitivity.

**Adjustment catalog** (the cases a paid service hits; all become rows, not schema changes):

| Case | Entries |
|---|---|
| refund / chargeback (Apple refunds users directly — `REFUND` notification; Google voided purchases; Stripe disputes) | `adjustment(refund)` −remaining; issued credentials stand to expiry (BBS, irrevocable) — accepted overhang |
| goodwill / outage compensation (issuance failed on the bot, badge visibly lapsed) | `adjustment(goodwill)` +N with reason |
| upgrade conversion (2.10) | `adjustment(conversion)` −remaining supporter months; money-side discount on the legend invoice |
| overpaid / duplicate crypto invoice (BTCPay marks these) | `adjustment(goodwill)` +N or refund per support resolution |
| balance transfer after profile loss (new order key; support-verified) | `transfer_out` −N old account / `transfer_in` +N new account |
| leaked/abused code batch | `adjustment(correction)` −remaining on affected accounts |

### Recovery and balance transfer (no-backup loss)

Normal path: `order_priv_key` and `master_key` are in the profile backup — restore re-attaches to the order by signing. Transfer exists only for total key loss. Policy: **prove the payment, not the identity** — receipt semantics, not wallet semantics (no seed phrases), and **no wallet operations**:

- **Receipts (Stripe and crypto payments)**: every settled Stripe/BTCPay payment yields a **receipt code** — a high-entropy, checksummed, human-copyable bearer secret; the bot stores only its hash bound to the payment; redeemable once. Delivered in the claim response, kept in the app (charge history, 2.6; travels with profile backups), with a one-time save-outside-the-app prompt at checkout (2.1).
- **Stores (Apple/Google)**: no receipt needed — the store account holds the entitlement; the new client re-presents a fresh store receipt → re-bind (capped per payment and period).
- **Transfer**: a request signed by the **new** order key carries the receipt → verified → `transfer_out` voids the remaining balance on the old account, `transfer_in` grants it to the new one. Once per payment; remaining (unissued) months only — issued credentials stand, they are irrevocable anyway; fully logged.
- **Bearer risk, accepted**: a leaked receipt moves only future months, once; the legitimate owner sees the voided balance on the next claim and disputes to support.
- **Why there are no card subscriptions**: cancellation after total loss was the unsolvable case — with no email and no account, an unattended subscription keeps charging a user who can no longer reach it. Multi-month prepaid removes the whole class: worst-case loss is bounded by what was paid (§6 resolved 11). The statement-descriptor short ref (~22 chars total incl. prefix, e.g. `SIMPLEX 4F7K2`) and txid + amount/time for crypto remain support-discretionary evidence for locating payments and refunds (the txid ↔ invoice mapping is private to payer and operator).
- Both app and receipt lost, nothing else to show = end of story, same as cash.

### `badges` — one row per order

`badge_id` PK · `user_id` · `order_key` (unique — the identity), `order_priv_key`, `master_key` · `badge_type` · `product_id` · `payment_id` (current entitling payment) · `status` (`acquiring|issued|superseded|failed`) · credential columns (`key_idx`, `signature`, `badge_expiry` — `BadgeRow` conventions; investor: `badge_expiry` NULL = lifetime) · `shown` (bool) · `use_from` (presentation start — 2.5) · `paused_at` (2.13) · `alert_acked_episode`, `alert_snooze_until` (2.4) · timestamps

- Manual acts create rows; **subscription renewal and prepaid monthly re-issue update the credential in place**, appending `charges` and ledger entries. Dispute history lives in payments + charges + ledger, not in old credentials.
- Expiry is never a stored state — derived at load (existing `mkBadgeStatus`). The current alert is likewise derived, never stored (2.4). Remaining time is the ledger balance.
- Get-or-create current badge is concurrency-safe: per-user in-process lock (controller `TMap`, as calls) + single store transaction + partial unique index as backstop: one live (`acquiring|issued`) row per (`user_id`, slot), slot = investor vs paid.
- `shown`: at most one per user (partial unique index); presentation waits for `use_from`; switching updates the `contact_profiles` presentation columns via `setUserBadge`.

### Storage integration

`contact_profiles` badge columns stay exactly as they are — the presentation copy of the shown badge (`setUserBadge`, Store/Profiles.hs:375). The `badges` table is the source of truth for what the user *has*; the profile columns hold what the user *shows*; the management screen exposes exactly this separation (2.6.8).

## 4. Wire protocol (single envelope)

Every request is signed with the badge's order key; the verified signer key is the order identity (transport-checked). One envelope carries order + payment together — payment verification and badge issuance are one request:

```
{ "product": { "type": "badge", "productId": ..., "masterKey": ... },
  "op": "catalog" | "invoice" | "claim" | "status" | "pause" | "resume" | "redeem" | "transfer",
  "payment": { provider-specific: JWS / token / claim / code, "months": ... },
  "lastChargeRef": ... }
```

- `catalog`: the **only unsigned op** — current products, prices, and discount tiers; fetched on purchase intent, no order identity attached (§3 price distribution). All other ops are signed.
- `invoice` (stripe/btc/xmr): create/return the payment link or address for the requested offer (product + plan + months resolve to an `active` or `deprecated` offer — the priced variant; `disabled` rejects with a refresh-catalog error); idempotent — same key with an unsettled invoice returns the same one.
- `claim`: verify payment (Apple: bot verifies the JWS offline; Google: Publisher API; Stripe/BTCPay: webhook/API state — the claim carries no evidence, the order key is the claim), check the verified payment matches the claimed offer (store SKU ↔ offer's store product id; charged amount/currency ↔ offer price), record lapse/issue entries, issue/return credential + status + charges and ledger adjustments since `lastChargeRef` + balance. Monthly re-issue: for subscriptions requires the period's charge settled; for prepaid requires positive balance and not paused. Idempotent: repeat returns the same credential. This one op covers first issue, renewal, re-issue, and lost-response recovery.
- `status`: same response shape without issuance intent (the bot may still return a newly due credential).
- There is no `cancel` op: store cancellation happens in store UI and is reflected by the next `status`; no non-store payment renews (§6 resolved 11).
- `pause` / `resume`: prepaid only (2.13); recorded on the bot's ledger clock and mirrored locally.
- `redeem`: code → badge type + credential + granted months (the service is authoritative for what the code grants).
- `transfer`: receipt code, signed by the **new** order key → re-bind the order to this key: remaining balance moves (`transfer_out`/`transfer_in`) and the provider binding moves with it, so `status`/`claim` — and any ops added later — work normally afterwards (§3 recovery); once per receipt.
- The `product` field is the generalization point — a future product type extends the envelope, not the client schema.

## 5. Provider grounding (client-visible states)

| | invoice step | pending means | settled evidence | renewal | cancel |
|---|---|---|---|---|---|
| Apple | local: `Product.purchase()` after creating the payment row | `pending` (Ask to Buy / SCA), late result via `Transaction.updates` | signed transaction JWS (bot verifies offline; carries storefront price + currency) | store auto-renews; claim after `renews_at`; history = Get Transaction History | store management sheet |
| Google | local: `launchBillingFlow` | `purchaseState = PENDING` (cash, slow cards) | purchase token (bot verifies + acknowledges ≤ 3 days; storefront currency reported) | store auto-renews; per-renewal order ids | Play subscriptions UI |
| Stripe | RPC `invoice` → payment link | claim answers "pending" until webhook | none client-side — order key + bot webhook state | **none — prepaid 1/3/12 months, drawn down monthly, pausable; renew manually** (no card subscriptions — resolved 11) | n/a |
| BTCPay (btc/xmr) | RPC `invoice` → address/amount, rate pinned, window ~15–60 min | invoice `Processing` (seen, unconfirmed) | none client-side — invoice `Settled` at the bot | **none — prepaid 1/3/12 months, drawn down monthly, pausable; renew manually when months run out** | n/a |

## 6. Decisions

Resolved:

1. Banner thresholds — local decision at implementation time; §2.2 as written is sufficient.
2. Alerts — single current alert per profile, derived from state, later states supersede unacknowledged earlier ones; persistence = two markers on the badge row, no alerts table.
3. Reminders — 3 days before, once, with user-driven "Remind me again" 1 day before; opt-out for reminder kinds only.
4. Start-sharing — no interstitial by default: immediate presentation, static "from the 1st" toggle in badge settings; interactive question only under the new global "Show advanced privacy questions" setting.
5. Upgrade pricing for one-time/crypto/prepaid — deduct the full supporter price while the supporter badge is unexpired, even past its renewal point.
6. Charge history — last 12 in UI.
7. Redeem codes — all badge types; service returns badge type + granted months.
8. Crypto price display — fiat-first, crypto amount pinned at invoice time.
9. **Time accounting — single-entry months ledger** (§3): use-it-or-lose-it with lapse computed at recording time; subscriptions post per-charge grants; pausable for prepaid with no duration limit; months are the account unit, money stays multi-currency in charges.
10. **Receipts and transfer** — every Stripe/crypto payment yields a bearer receipt code; the receipt's only operation is recovery: it re-binds the order (remaining balance) to a new key, no wallet operations; support uses statement data (payment time + descriptor ref) as discretionary evidence; stores recover via store-account re-bind (§3 recovery).
11. **No card (Stripe) subscriptions — multi-month prepaid is the non-store model.** With no email and no account, an unattended subscription cannot be cancelled after device loss: an unbounded recurring-charge liability, where prepaid loss is bounded by what was paid. Renewal convenience is covered by the months-exhausted alert and re-purchase; the `cancel` op leaves the protocol. **The model must and does admit adding them later without breaking changes**: `payments.renews_at`/`cancelled` already serve store subscriptions, each Stripe invoice would post a per-charge ledger grant exactly like a store charge, receipts already re-bind orders to a new key (recovery, then cancel), and the statement-descriptor support path is in place — reintroduction adds only a `cancel` op (the envelope is op-extensible) and webhook renewal handling at the bot.
12. **Durations and pricing** — non-store prepaid 1/3/12 months: 3 at 2 × monthly (3-for-2, 33% off), 12 at 6 × monthly (2-for-1, 50% off); store annual at the same 2-for-1 (safe: voluntary early cancellation runs to period end unrefunded on both stores; support refunds → clawback). **No store multi-month SKUs**: stores offer one-time 1 month + monthly/annual subscriptions — subscriptions already cover longer commitments, and extra SKUs are complexity without capability. All pricing is catalog content (offers) — adjustable without release.
13. **Expiry rounding — weekly, not monthly**: badges end on Sundays (UTC); presentations and expired-badge removal updates go out on Mondays; payment/issuance cadence stays monthly (2.11). Replaces end-of-next-month rounding (0–31 free days, ~+50% average, purchase-delay gaming) with 0–6 days (~+10% average).

Open:

1. User-picker badge presence: icon on the profile bar (preferred, needs design for the alert dot) vs full option row (2.3).
2. Scope of "Show advanced privacy questions" beyond badges (deletion notifications, rejected contact requests, calls) — separate initiative, tracked here only as the setting's origin.
3. Support tooling scope at launch: refund clawback is mandatory (providers force it); transfer is deferred to phase 2 — until then support executes recovery manually against a presented receipt; support also handles disputes and lost-receipt cases (statement ref / txid evidence, discretionary).

## 7. MVP descope

MVP cuts lifecycle features, not the model. The unifying simplification: **every non-store payment is a multi-month prepaid purchase** — one flow (invoice → pay → claim → months granted to the ledger) identical for Stripe, BTC, and XMR; subscriptions exist only on stores, where the store runs the lifecycle. Ledger, charges, and receipts ship at MVP because none of them can be reconstructed later: charges have no refetch path (provider history binds to orders; keys change across recovery and new purchases), receipts must exist at payment time, and ledger drawdown semantics cannot safely be retrofitted under live balances.

### Launch set

- Tiers: supporter + legend. Store plans: monthly and annual subscriptions + one-time 1 month. Stripe/BTC/XMR: **multi-month one-time, 1/3/12 months, priced per resolved 12** — no card subscriptions (§6 resolved 11; the model admits them later).
- Protocol ops: `catalog` | `invoice` | `claim` | `status`. No `cancel` op — store cancellation is store UI, reflected by `status`; nothing non-store renews. Receipts arrive in the claim response; catalog version piggybacks on `claim`/`status` (§3 price distribution).
- **Store cancellation visibility**: the Cancel button opens the store's management sheet — Apple has no programmatic cancel API, Google's server-side cancel is a support tool, and a local-only cancel would show "cancelled" while the store keeps charging. On return the engine sends `status`; the client also reads renewal state locally (StoreKit 2 `RenewalInfo.willAutoRenew`, Play Billing `Purchase.isAutoRenewing`) and renders cancelled-active without a round trip. The bot never learns by charging (it never charges) or from client silence — it derives cancellation from the provider on every `claim`/`status` (App Store Server API `autoRenewStatus`, Play `subscriptionsv2` state). For timely and complete records — users mostly cancel in store settings without opening the app, which a client op would never see — the bot handles store notifications (below). No `cancel` op exists at all — nothing non-store renews (§6 resolved 11), and for stores it could neither perform the cancellation nor see the majority of them.
- **Bot-side provider notifications, required at MVP, all providers**: Stripe webhooks (settlement), BTCPay webhooks (invoice settled/expired), App Store Server Notifications V2, and Play RTDN (cancellations, grace/on-hold, refunds/voided purchases — door 5's timeliness depends on these; a refunded user may never claim again). Notifications only update bot records and trigger provider re-derives — **never issue from the notification payload**, and never push to the client; the client learns on its next check-in.
- Tables: `products` + `offers` (two-tier catalog, §3), `payments`, `charges`, `badge_ledger`, `badges` (without `shown`, `use_from`, `paused_at`).
- Core engine as specified (2.9): claims on triggers + timers, monthly re-issue for store subscriptions and against prepaid balance, lapse recording, Monday presentation incl. expired-badge removal updates (2.11), single-alert derivation.
- Alerts: the full 2.4 set (renewal approaching applies only to store subscriptions at MVP); reminders opt-out toggle.
- Receipts for every Stripe/crypto payment with the checkout save prompt (2.1, §3); **`transfer` op deferred** — until it ships, support executes recovery manually against a presented receipt.
- Entry points: chat-list banner, Settings "Supporter perks" row, whats-new. Management screen with all states except Paused and held-badges switching; no charge history list at MVP — the `charges` table records from day one, only the UI is deferred.
- New designs needed: management screen, payment method + duration selector, crypto invoice screen (incl. the receipt save prompt).
- Multi-profile; incognito never presents.

### One-way doors — must ship day one (or earlier)

1. **+7 day recipient display grace** (2.11) — old client versions never learn it; ship in the earliest release, ahead of purchases if possible.
2. **Week-boundary expiry (Sundays, UTC) + Monday presentation incl. removal updates for expired badges (2.11)** — peer-visible anonymity architecture.
3. **Order-key-per-badge identity + `product`-tagged envelope** — the protocol spine.
4. **Apple subscription group containing all subscription SKUs** (both tiers × monthly/annual) — store config now; makes store-native upgrades and plan switches work later with no app code (bot sees the SKU change on `status`).
5. **Refunds = stop future issuance + ledger clawback entry** — providers refund without asking.
6. **Stripe statement descriptor** with short payment ref — support evidence from day one.
7. **Receipts from the first payment** — a payment made without a receipt is never recoverable.

### Deferred, with re-add paths

| Deferred | Returns as | Why nothing is lost |
|---|---|---|
| `transfer` op | phase 2 | receipts exist from day one; support executes transfers manually in the interim |
| pause / resume | phase 2 | the ledger already models paid months; new ops + `paused_at` column |
| charge history UI (last 12, receipts per row) | phase 2 | the `charges` table records from day one — display-only deferral |
| investor + redeem codes + held-badges switching (`shown`, `use_from`, slots) | phase 2/3 | one badge per profile at MVP; investors served manually via existing `/badge add`; columns and slot rules additive |
| upgrades (2.10) | phase 2 | store-native switching enabled by door 4; bot SKU-change mapping and conversion adjustments additive |
| start-sharing question + "Show advanced privacy questions" | phase 3 | immediate presentation at MVP; setting arrives with its broader initiative |
| user-picker badge presence (icon/row) | phase 2 | blocked on design anyway; Settings row covers entry |
