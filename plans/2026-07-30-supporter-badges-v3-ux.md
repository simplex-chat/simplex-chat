# Supporter Badges v3

**Date:** 2026-07-30
**Status:** draft
**Transport:** service RPC (`plans/2026-07-22-service-rpc-chat.md`, implemented, branch `rpc`): signed requests (`APISendServiceRequest.signKey`), verified signer key delivered to the service (`CEvtServiceRequest.signerKey`), one response per request, per-call timeout, no transport persistence. The client repeats the same signed request after a timeout; every bot operation is idempotent.
**Designs:** `/code/simplex-chat-art/screens/badges-mobile_15-*.jpg`, `_16-*-dark.jpg`

## Principles

- At most one badge is shown per profile. A user holds at most two badges per profile: paid (supporter/legend) and investor (2.7).
- The core engine performs all badge automation and emits events; apps render and call APIs only for user actions (2.9).
- No bot push; the client reads state via its own requests. Bot-initiated messaging, if ever added, is an opt-in SimpleX contact.
- Paid time is a months balance on a single-entry ledger; unconsumed months lapse; credentials are issued monthly for all plans; prepaid time is pausable post-MVP (§3, 2.13).
- One current alert per profile, derived from state; a later state supersedes an unacknowledged earlier alert (2.4).
- Renewal reminders on by default with opt-out; no success screens.
- Credentials expire on Sundays (UTC); profile updates are sent on Mondays, including removal updates for expired badges; the user is shown one date — the payment date (2.11).

## 1. Existing designs

| Screen | Covers |
|---|---|
| `banner` | chat-list banner: "Support SimpleX — Get badge + files up to 5GB", dismissable |
| `support-simplex` | intro screen → Choose your level |
| `your-level` | tier choice: Supporter $7/month (2GB) vs Legend $70/month (5GB), profile preview with badge |
| `pay` | plan choice: "1 month" vs "Subscribe", Pay button, renewal date |
| `settings` | "Supporter perks" row in Your settings |
| `whats-new` | v7.0 announcement entry |

These designs cover the acquisition path for iOS/Play builds only. Missing designs:

- payment method + duration selector (2.1)
- crypto invoice screen (2.1)
- user-picker badge presence (2.3)
- management screen (2.6)
- start-sharing question (2.5)

Store builds display store-fetched localized prices; catalog prices apply to Stripe/BTCPay only.

## 2. UX

### 2.1 Payment method and duration (non-store builds)

- Store builds: no method UI. One-time 1 month; monthly/annual subscriptions.
- Non-store builds: method selector (Card / Bitcoin / Monero), all multi-month prepaid; no card subscriptions (§6.11). Plan screen copy: "payments don't renew — you choose how many months to pay for".
- Durations 1 / 3 / 12 months; prices 1× / 2× / 6× monthly (§6.12). Duration selector — new design.
- Unavailable options: disabled offers and inactive products are shown as disabled; when no option is available, the purchase screen shows an unavailable notice; the `product_unavailable` error is displayed the same way.
- Crypto payment screen (new design):
  - amount, one-off address, QR, copy, open-in-wallet, countdown (BTCPay fixes the fiat rate at invoice creation; the invoice expires in ~15–60 min);
  - states: waiting → processing (transaction seen, unconfirmed) → settled; expired → "Invoice expired" + new invoice (new payment row);
  - fiat-first display;
  - partial payment: the BTCPay marked state is shown; the user is directed to support.
- After settlement the issue response contains a receipt code (§3 recovery); one-time save prompt at checkout ("keep your receipt — it recovers unused months if you lose this device"); receipts are shown in payment history (2.6).
- Stripe: payment link opened in the system browser; "Waiting for payment…" + open-link-again; the link is stored in the payment row.

### 2.2 Chat-list banner

Shown after ≥ 3 conversations and not in the first days after install; hidden for ~6 months after dismissal; not shown while a badge is active or a payment is pending; not re-shown on expiry. Exact thresholds set at implementation.

### 2.3 User picker (design needed)

Menu opened from the avatar in the chat-list toolbar (`apps/multiplatform/.../chatlist/UserPicker.kt`, `apps/ios/Shared/Views/ChatList/UserPicker.swift`; the profile bar renders `localBadge` — UserPicker.kt:213).

- Option A (preferred): badge icon on the right of the profile bar, opens the management screen; indicator dot for the current alert. Icon shown while a badge is held or a payment is pending; acquisition entry remains the banner and the Settings row.
- Option B: full option row (state text: "Supporter · renews Aug 21", "ends Oct 22", "⚠ payment problem"), replacing the Settings row; "Your chat profiles" is moved to Settings.

### 2.4 Alerts

| Alert | When | Content / actions |
|---|---|---|
| subscription renewal approaching | 3 days before `renews_at`, once per cycle | price + date; OK · Remind me again (1 day before) · Cancel path |
| payment failed / grace | on provider grace/hold state, once per episode | "Payment problem — perks until {date}. Fix payment." → store fix flow |
| subscription ended | at paid-through, once | "Subscription ended {date}." + Resubscribe |
| prepaid ending | 3 days before paid-through, balance 0, not paused, once | "Your support ends {date}. Renew for {price}?" OK · Remind me again (1 day before) |
| support ended | at paid-through, balance 0, no active subscription, not paused, once | "Your support ended {date}." + Renew / Choose level |
| renewal charged | never — visible in charge history and picker badge state | — |
| credential issued | never | — |

No alerts for paused badges (2.13). An alert is shown in its profile's context (indicator on the profile row in the picker).

Persistence: no alerts table. On the badge row: `alert_acked_kind` + `alert_acked_episode` (the episode value, e.g. this cycle's `renews_at`) and `alert_snooze_until`. Core emits `CEvtBadgeAlert` when the derived alert differs from the acknowledged one; on OK the client stores the kind + episode; on snooze it sets `alert_snooze_until`, after which the alert is emitted once more.

Opt-out toggle "Remind me about renewals and expiry" disables renewal-approaching and prepaid-ending; the other alerts cannot be disabled.

### 2.5 Start-sharing

- Default: a newly acquired badge is presented immediately; switching the shown badge takes effect immediately.
- Perks require presentation: the recipient verifies file size against the presented proof (`maxXFTPFileSize` on `PeerBadge`); the sender's client disables the perk at paid-through (2.11). This is stated in the "Now / From Monday" question.
- Management-screen toggle: "Show new badges from Monday" — off by default.
- Global setting "Show advanced privacy questions" (Privacy & security): when on, acquisitions that change the presented badge ask "Start showing: Now / From Monday". Later candidates for the same setting: deletion notifications, rejected contact requests, call questions.
- While presentation is delayed: the previous badge remains presented; the management screen shows "starts Monday {date}"; field `use_from`.

### 2.6 Management screen (design needed)

All dates shown are paid-through (2.11):

1. **Acquiring / payment pending** — method-specific pending UI (2.1); old badge unchanged.
2. **Active prepaid** — "Ends {date}" ({N} months left); payment history (last 12; Stripe/crypto payments with receipts, §3); Pause (2.13); Subscribe; Upgrade.
3. **Active subscription** — "Renews {date} · {price}"; Cancel; Upgrade; charge history (last 12: date, amount, method, in the charged currency); reminders toggle.
4. **Cancelled, still active** — "Ends {date}"; Resubscribe.
5. **Payment issue** — banner over state 3; perks remain enabled during the provider grace period.
6. **Paused** — "Paused · {N} months left"; Resume.
7. **Expired** — "Support ended {date}"; Renew / Choose level.
8. **Held badges** — list of held badges (validity, source); radio for the shown one; pending presentation ("starts Monday {date}"). On a switch the client sends a profile update.
9. **Redeem code** entry (2.8).
10. Settings: reminders toggle (2.4); "Show new badges from Monday" (2.5).

### 2.7 Held badges and switching

- Perks apply only from the shown badge, verified against the presented proof. Holding legend while showing investor yields 2 GB (`BTLegend` alone yields 5 GB). Switch UI copy: "Sending 5 GB files requires showing the Legend badge".
- Exactly one shown; on a switch the client sends a profile update with a fresh proof (`setUserBadge` + broadcast; the presentation copy is stored in `contact_profiles`). Timing per 2.5.
- When the shown paid badge expires and an investor badge is held: the client switches the shown badge to investor with the Monday update.

### 2.8 Redeem codes

The service grants any badge type for 1/3/12 months per code (investor: lifetime); the response contains badge type + granted months. Granted months are recorded in the ledger; credentials are re-issued monthly. UX: "Have a code?" → paste → signed `redeem` (fresh order) → the badge is shown (2.5). Errors: invalid / used / expired, inline.

### 2.9 Core engine

Triggers:

- chat start
- foreground
- network restore
- profile switch
- timers: renewal, expiry, Monday boundaries

1. resume: settled payments without issuance; subscription renewals after `renews_at`; monthly re-issue while balance > 0 and not paused; invoiced/pending crypto payments;
2. `advance(now)` + `issue(now)` at each `issue`/`status` request (§3);
3. presentation on/after Monday (UTC): fresh proofs for renewed badges and for badges whose `use_from` was reached; removal updates for badges expired unrenewed;
4. alert derivation (2.4), `CEvtBadgeAlert`;
5. sync: store charges and ledger rows received after the `since` cursor (§3).

Events to the app: `CEvtBadgeChanged`, `CEvtBadgeAlert`.

API calls (user actions):

- get status
- get catalog (purchase-screen open, §3 prices)
- start purchase / get invoice
- issue after store payment (the app passes store evidence to core)
- cancel — opens the store sheet; no bot op
- pause / resume
- switch shown badge / set start date
- acknowledge / snooze alert
- reminder and presentation toggles
- redeem code

On `ASETimeout` core retries the identical signed envelope on the next trigger; never on a poll timer.

### 2.10 Upgrades

- supporter → legend (store subscription): native upgrade flow, same subscription group. New badge row (new keys); old row superseded.
- prepaid upgrades: legend invoice = legend price − full supporter price while the supporter badge is unexpired; remaining supporter months are removed by `debit(conversion)` (§3). Both amounts are displayed.
- one-time → subscription (same tier): same badge row, new payment row; `payment_id` is updated to the new payment.
- Presentation per 2.5, including investor → legend.

### 2.11 Dates

- Credential expiry = paid-through of the issued month rounded up to Sunday (end of Sunday, UTC) — `sundayAfter` (§3).
- The user is shown one date — paid-through: "Renews {date}" / "Ends {date}" / "Support ended {date}". "Badge valid until" does not appear anywhere. The sender's client disables perks at paid-through; the recipient's client enforces credential expiry + grace. Small print: "your profile badge may remain visible to contacts for a few days after your support ends."
- Monday send-outs: fresh proofs for renewed badges; removal updates (`setUserBadge Nothing`) for badges expired unrenewed.
- Recipient display: a peer badge with expiry E is displayed as active until E + 7 days; the `BSExpiredOld` boundary (31 days) is shifted by the same 7 days. Both in the recipient's `mkBadgeStatus` derivation.

### 2.12 Multi-profile

Badges, payments, charges, ledgers are per `user_id`. No badge is presented for incognito profiles.

### 2.13 Pause (prepaid only, post-MVP)

Issued credentials are not pausable (expiry is signed); re-issuance is. Pause suspends issuance and lapse, with no duration limit.

- Prepaid time only (crypto, Stripe one-time, codes).
- Signed RPC op, recorded bot-side and mirrored (`resume` transition, §3).
- While paused: the current credential remains valid to its expiry; no alerts; management screen state 6.
- On resume: issuance is resumed at the next `issue` request.

## 3. Model

### Ownership — two databases

Bot tables (no private keys, no master keys, no UI state):

- `orders` — `order_key` PK; pinned `product_id`; timestamps. Order state is derived from the ledger + provider.
- `payments` — user-initiated payments, as opposed to charges: bot-assigned `payment_ref`; per-payment `offer_id`, `provider`, `provider_ref`, receipt hash, status
- `charges`
- `badge_ledger` — authoritative
- `issuances` + cached credentials
- `codes` — code hashes, granted type/months, redemption, batch revocation
- webhook dedup

Client tables:

- `products`, `offers` — catalog cache
- `payments`
- `charges` — replica
- `badge_ledger` — replica
- `issuances` — replica
- `badges` — keys, credential, UI state

Rules:

- The replicated tables (`badge_ledger`, `issuances`, `charges`) have identical row content on both sides: bot-assigned row ids and refs. Ledger and issuance account column: `order_key` on the bot, `badge_id` on the client — 1:1, one key per badge. Charges reference payments on both sides: `payment_ref` on the bot, `bot_payment_ref` on the client.
- Synced via the `since` cursor; all ledger transitions (§ ledger) are executed by the bot only; the client stores received rows unchanged.
- Not replicated: the client's `payments` row records the local act (evidence, invoice URL, status); the bot's `payments` row records the binding (offer, `provider_ref`, receipt hash); `orders` records the pinned product; `badges` has no bot counterpart.
- Payments are product-agnostic on both sides: keyed by `order_key`, without references to product tables. The product row references its current payment; payment history is the join on `order_key`.

### `products` + `offers` — two-tier catalog

Tier 1 — `products`. All builds use it for product structure; display names and localization are app resources.

- `product_id` PK
- `product_type` — `badge`
- `badge_type` — `supporter|legend|investor`
- `active`

Tier 2 — `offers`:

- `offer_id` PK
- `product_id` → products
- `plan` — `one_time|monthly|annual`
- `months` — 1/3/12 for one-time; NULL for subscriptions
- `apple_product_id`, `google_product_id` — store offers
- `price`, `currency` — non-store offers
- `state` — `active|deprecated|disabled`

Rules:

- Offers are append-only: repricing = new offer row + deprecation of the old.
- Lifecycle: `active` (rendered + accepted) → `deprecated` (hidden; accepted by `order`; window length is operator policy) → `disabled` (`order` rejects with a refresh-catalog error).
- State is checked at invoice creation only; the amount of an existing invoice does not change; `issue` on a settled payment is always honored.
- For store offers the state controls display only; their sale lifecycle is managed in the store consoles.
- `catalog` response: `active` + `deprecated` offers with state; `disabled` omitted. The client renders `active`, retains `deprecated`, may delete offers absent from the response unless referenced by a payment row.
- The duration selector (2.1) is built from active one-time offers; discounts are per-duration prices.
- Store one-time is 1 month only (§6.12). Seeded from app config; server-authoritative on the bot.
- Forward compatibility: entries with unknown `product_type`/`badge_type` are stored, not rendered, no error. New product types require an app release; new offers of known products do not.
- Products with `active = false` are not offered; an in-flight request on one → `product_unavailable`.

Prices:

- The amount at invoice creation is authoritative: BTCPay fixes the fiat amount, the Stripe page shows the charge, the final amount is confirmed at checkout.
- Display prices: the unsigned `catalog` op on purchase-screen open (non-store builds; non-blocking — the screen is rendered from the stored catalog and updated from the response), and the full catalog included in every `issue`/`status` response; the client reconciles received rows against stored rows.
- The catalog shipped with the app is the offline fallback; store builds read prices from StoreKit/Play Billing.
- Repricing applies to future purchases only.

### `payments` — user-initiated acts

- `payment_id` PK
- `user_id`
- `order_key` — the order paid for; badge orders are rows in `badges`
- `offer_id` → offers
- `bot_payment_ref` — bot-assigned; recorded from the invoice response or sync rows
- `months`, `amount`, `currency` — copied from the offer at purchase
- `provider` — `apple|google|stripe|btc|xmr|code`
- `provider_ref` — Apple original transaction id / Google purchase token / Stripe intent ref / BTCPay invoice id / code hash
- `invoice_url`
- `evidence` — Apple JWS / Google token, stored for repeat `issue` requests
- `receipt_code` — client-side (§ recovery); the bot stores `receipt_hash`
- `status` — `new|invoiced|pending|settled|failed|expired`
- `renews_at` — subscriptions
- `cancelled` — bot-confirmed renewal-off
- `created_at`, `updated_at`

One row per act: purchase, subscribe, upgrade, resubscribe, each crypto invoice, code redemption. Abandoned attempts remain as history. On settlement the bot runs `advance` then `grant` (§ ledger). `paidThrough` = last ledger row; `badge_expiry` = last issuance.

### `charges` — provider-initiated billing events

- `charge_id` PK
- `payment_id` → payments
- `bot_payment_ref` — the synced ref; the client resolves `payment_id` from it
- `provider_charge_ref` — Stripe invoice id / Apple transaction id / Google order id
- `period_start`, `period_end`
- `amount`, `currency` — as reported by the provider
- `charged_at`
- unique (`payment_id`, `provider_charge_ref`)

Rules:

- Sources — the first charge of a subscription is also a row:
  - Stripe: invoices by subscription
  - Apple: Get Transaction History by original transaction id
  - Google: per-renewal order ids
- Synced to the client via the `since` cursor.
- For each settled charge the bot records a `grant` of the charge's period length: +1 monthly, +12 annual.
- Charges are money facts referenced by grants, not ledger rows: a grant is not recorded for every charge (webhook replays are rejected by the unique key before any grant; charges from provider history for consumed, refunded, or re-bound periods are recorded without grants), and `charged_at` differs from the accounting time.

### `badge_ledger` + `issuances`

**Operations to support:**

| # | operation | months | credential |
|---|---|---|---|
| O1 | prepaid settlement (Stripe/BTCPay) | +N | — |
| O2 | subscription charge settled | +1 monthly / +12 annual | — |
| O3 | code redemption | +N | — |
| O4 | goodwill grant | +N | — |
| O5 | transfer in | +M | — |
| O6 | refund / chargeback | −balance | issued credential remains valid |
| O7 | upgrade conversion | −balance | — |
| O8 | transfer out | −balance | — |
| O9 | correction (code abuse) | −balance | — |
| O10 | issue a month | −1 | new credential |
| O11 | lapse elapsed months | −k | — |
| O12 | repeat issue, same month | none | cached credential, no rows |
| O13 | lifetime issuance (investor) | none — no ledger | new credential |
| O14 | pause / resume (post-MVP) | 0 | — |

**Ledger state** — two values; every row contains the state after it; **the last row is the state**:

- `months` — unused months.
- `start` — the date the unused balance starts. Not changed by grants (while `months > 0`); advanced by one month per `consume` and by the lapsed count per `lapse`.

Coverage = `[start, addMonths months start)`. `paidThrough = addMonths months start` — read from the last row alone; not a `badges` column.

**Row:**

- `entry_id` PK
- `badge_id`
- `op` — `grant(source) | debit(reason) | consume | lapse | resume`
- `delta` — signed months change
- `months` — state: unused months after this row
- `start` — state: balance start after this row
- ref — bot-assigned payment ref / `charge_id` (grants)
- `created_at`

Append protocol: lock the badge's ledger → read the last row → compute the next → insert. The client stores rows verbatim from responses (`since` cursor = last `entry_id` held) and can re-verify each row from its predecessor.

**Transitions** — from the last row `(start, months)`; `advance t` is run before every issue, grant, and debit:

```
advance t:            -- time bookkeeping only: one lapse row for the fully elapsed months
  k = min months (fullMonthsBetween start t)
      -- fullMonthsBetween start t: the largest m >= 0 with addMonths m start <= t
  if k > 0: append (lapse, −k, months − k, addMonths k start)               -- O11, one row

issue t:              -- run after advance t
  requires months > 0 && start <= t && no issuance for [start, addMonths 1 start)
  sign the credential, expiry sundayAfter (addMonths 1 start)
  in one transaction: append (consume, −1, months − 1, addMonths 1 start)   -- O10
                      + issuance row for [start, addMonths 1 start)
  on signing failure: no rows; retried at the next `issue`

grant t n src:                                    -- O1–O5; t = settlement time,
  months == 0 → append (grant src, +n, n, max start t)      -- provider period start for O2
  months > 0  → append (grant src, +n, months + n, start)

debit reason:  append (debit reason, −months, 0, start)     -- O6–O9

resume t:      append (resume, 0, months, max start t)      -- O14; no row is written on pause
```

**Issuances** — separate table; no credential is issued for `grant`/`debit`/`lapse` rows, and no ledger rows are written for lifetime (O13) and cached (O12) issuance events:

- `issuance_id` PK
- `badge_id`
- `period_start`, `period_end` — NULL for lifetime
- `expiry` — `sundayAfter period_end`; NULL for lifetime
- `entry_id` → the `consume` row; NULL for lifetime
- `created_at`

`badgeExpiry` = `expiry` of the last issuance — the credential's disclosed field. The current credential is stored in the badge row's credential columns; the issuance rows are the history; the O12 check reads the last issuance's period.

**Properties:**

1. Each row's `(months, start)` equals the transition applied to its predecessor — client-verifiable per row.
2. `months ≥ 0`; the sum of `delta` equals `months`; `start` is non-decreasing.
3. `consume` rows ↔ period issuances are 1:1 (`issuances.entry_id`) — written in one transaction, after the credential is signed.
4. Re-running the `issue` op (§4) immediately appends nothing; the response is the cached credential or `status`.

**Example** — buy 3 months Tue Mar 10, 2026; app off Apr 5 – May 20; issue May 20:

| # | op | delta | months | start | note |
|---|---|---|---|---|---|
| 1 | grant(payment) | +3 | 3 | Mar 10 | paidThrough = Jun 10 |
| 2 | consume | −1 | 2 | Apr 10 | issuance: Mar 10–Apr 10, expiry Sun Apr 12 |
| 3 | lapse | −1 | 1 | May 10 | Apr 10–May 10 passed unissued; recorded May 20 |
| 4 | consume | −1 | 0 | Jun 10 | issuance: May 10–Jun 10, expiry Sun Jun 14 |

`paidThrough` after every row = Jun 10. ~Jun 7: prepaid-ending alert. Jun 10: sender-side perk disabled. Sun Jun 14: credential expires. Mon Jun 15: removal update. An additional +3 purchase on Jun 5: `months = 0` → row (grant, +3, 3, max(Jun 10, Jun 5)) → paidThrough = Sep 10.

**Adjustments:**

| Case | Rows |
|---|---|
| refund / chargeback (Apple `REFUND` notification; Google voided purchases; Stripe disputes) | `debit(refund)`; issued credentials remain valid to expiry |
| goodwill / outage compensation | `grant(goodwill) +N` with reason text |
| upgrade conversion (2.10) | `debit(conversion)` on the supporter badge; discount on the legend invoice |
| overpaid / duplicate crypto invoice | `grant(goodwill) +N` or refund, per support resolution |
| balance transfer after profile loss | `debit(transfer_out)` old badge / `grant(transfer_in) +M` new badge |
| leaked/abused code batch | `debit(correction)` on affected badges |

### Recovery and transfer

- Normal path: `order_priv_key` and `master_key` are part of the profile backup; after restore, a signed `status` request resolves the same order.
- Receipts (Stripe and crypto payments): the bot creates a receipt code for every settled payment — a high-entropy, checksummed, human-copyable bearer secret; only its hash is stored bot-side; redeemable once. Contained in the issue response; stored in the app (payment history, profile backups); save prompt at checkout (2.1).
- Stores: no receipt — the new client presents a fresh store receipt → re-bind, capped per payment and period.
- Transfer: a request signed by the new order key contains the receipt → `debit(transfer_out)` on the old badge, `grant(transfer_in)` on the new; the provider binding is reassigned to the new order. Once per receipt; unissued months only; logged. With a receipt used by another party, only unissued months are transferred, once; disputes are sent to support.
- Support evidence when the receipt is lost: the statement-descriptor ref (~22 chars total incl. prefix, e.g. `SIMPLEX 4F7K2`) and, for crypto, txid + amount/time. Discretionary; sufficient only for cancellation-level actions.
- Both app and receipt lost: no recovery.

### `badges` — one row per order

- `badge_id` PK
- `user_id`
- `order_key` — unique; the order identity
- `order_priv_key`, `master_key`
- `badge_type`
- `product_id` → products
- `payment_id` → payments — NOT NULL; the current payment. The badge row and its first payment are created in one transaction
- `status` — `acquiring|issued|superseded|failed`
- credential columns — `key_idx`, `signature`, `badge_expiry` (`BadgeRow` conventions; investor: `badge_expiry` NULL)
- `use_from` — presentation start (2.5)
- `paused_at` (2.13)
- `alert_acked_kind` + `alert_acked_episode`, `alert_snooze_until` (2.4)
- `created_at`, `updated_at`

- A row is created per manual act (purchase, upgrade, resubscribe, redeem); on subscription renewal and prepaid re-issue the credential is updated in place, with charge and ledger rows added.
- Status is derived at load (`mkBadgeStatus`); the current alert is derived (2.4). Months and `paidThrough`: last ledger row. `badge_expiry`: credential columns / last issuance.
- Get-or-create concurrency: per-user lock (controller `TMap`) + one store transaction + partial unique index — one live (`acquiring|issued`) row per (`user_id`, slot), slot = investor vs paid.
- The shown badge is referenced from the user's record (`users.shown_badge_id`) — at most one, structurally; presentation starts at `use_from`; on a switch the client updates the reference and `contact_profiles` via `setUserBadge`.

### Storage integration

`contact_profiles` badge columns remain the presentation copy of the shown badge (`setUserBadge`, Store/Profiles.hs:375). `badges` records what the user has; the profile columns record what the user shows (2.6.8).

## 4. Wire protocol

`docs/protocol/badges-rpc.schema.json` + `docs/protocol/badges-rpc.md`. Every request except `catalog` is signed with the order key and contains `orderKey`, its public part; the bot verifies `orderKey` equals the transport signer key.

| op | request | response |
|---|---|---|
| `catalog` | — | catalog |
| `order` (stripe/btc/xmr) | `offerId`, `provider` | invoice: url/address, amount, expiry |
| `issue` | `masterKey`; store evidence (JWS / token) when settling a store payment; `since` | credential + sync, or status |
| `status` | `since` | status or credential + sync |
| `redeem` | `masterKey`, `code`, `since` | credential + sync |
| `pause` / `resume` (post-MVP, 2.13) | — | status |
| `transfer` (post-MVP, §3 recovery) | `receipt` | status |

- Offer resolution: by `offerId` at `order`; by the store SKU in the evidence at `issue`; by the code at `redeem`. The order's product is pinned at creation.
- On `issue`: `advance(now)`; then: current month issued → cached credential (O12); `issue` precondition met → `issue(now)`; otherwise `status` (§3).
- Store cancellation is performed in store UI and reflected by the next `status` (§6.11).
- New product types are added in the catalog (`product_type`); the ops are unchanged.

## 5. Providers

| | invoice step | pending means | settled evidence | renewal | cancel |
|---|---|---|---|---|---|
| Apple | local: `Product.purchase()` after creating the payment row | `pending` (Ask to Buy / SCA), late result via `Transaction.updates` | signed transaction JWS (verified offline by the bot; contains storefront price + currency) | store auto-renews; issue after `renews_at`; history = Get Transaction History | store management sheet |
| Google | local: `launchBillingFlow` | `purchaseState = PENDING` | purchase token (verified + acknowledged by the bot ≤ 3 days) | store auto-renews; per-renewal order ids | Play subscriptions UI |
| Stripe | RPC `order` → invoice (payment link) | `issue` responds "pending" until the webhook is received | none client-side — signer key + bot webhook state | none — prepaid 1/3/12 months; renewed manually | n/a |
| BTCPay (btc/xmr) | RPC `order` → invoice (address/amount), rate fixed, window ~15–60 min | invoice `Processing` | none client-side — invoice `Settled` at the bot | none — prepaid 1/3/12 months; renewed manually | n/a |

## 6. Decisions

Resolved:

1. Banner thresholds — set at implementation.
2. Alerts — one derived alert per profile; two markers on the badge row; no alerts table.
3. Reminders — 3 days before, once; "Remind me again" 1 day before; opt-out covers reminder kinds only.
4. Start-sharing — immediate by default; "from Monday" toggle; interactive question only under "Show advanced privacy questions".
5. Prepaid upgrade pricing — deduct the full supporter price while the supporter badge is unexpired.
6. Charge history — last 12 in UI.
7. Redeem codes — all badge types; the service response contains type + months.
8. Crypto prices — fiat-first; crypto amount fixed at invoice time.
9. Time accounting — single-entry months ledger (§3); unconsumed months lapse; per-charge grants; pause post-MVP.
10. Receipts — every Stripe/crypto payment; recovery is the receipt's only operation; stores re-bind via the store account.
11. No card (Stripe) subscriptions — multi-month prepaid is the non-store model. Re-add path if ever needed: `cancel` op + bot webhook renewal handling; the schema suffices (`renews_at`/`cancelled`, per-charge grants, receipt re-bind, statement descriptor).
12. Durations and pricing — non-store 1/3/12 months at 1×/2×/6× monthly; store annual at 6× monthly; store offering = 1-month one-time + monthly/annual subscriptions, no store multi-month SKUs; all pricing in offers.
13. Expiry rounding — weekly: badges end Sundays (UTC); Monday presentations + removal updates; payment/issuance cadence stays monthly.
14. One user-facing date — paid-through everywhere; the sender's client disables perks at paid-through; credential expiry is internal.

Open:

1. User-picker badge presence: icon (preferred) vs row (2.3) — needs design.
2. Scope of "Show advanced privacy questions" beyond badges — separate initiative.
3. Support tooling at launch: refund clawback is mandatory; transfer is manual (support acts on a presented receipt) until the op is implemented.

## 7. MVP

Non-store payments at MVP are multi-month prepaid purchases: one flow (order → pay → issue → grant) for Stripe, BTC, XMR. Subscriptions exist only on stores. The ledger, charges, and receipts are included in the MVP.

### Launch set

- Tiers: supporter + legend + investor (redeem codes, 2.8). Store: monthly and annual subscriptions + one-time 1 month. Stripe/BTC/XMR: one-time 1/3/12 months (§6.12).
- Redeem codes for all badge types (2.8) — the reward mechanism; code generation is operator tooling, required at MVP. Held badges with switching (2.7) — a switch takes effect immediately at MVP (`use_from` deferred).
- Ops: `catalog` | `order` | `issue` | `status` | `redeem`. Receipts in issue responses; the full catalog in `issue`/`status` responses.
- Store cancellation:
  - the Cancel button opens the store management sheet; the engine sends `status` on return;
  - the client renders cancelled-active from local renewal state (StoreKit 2 `RenewalInfo.willAutoRenew`, Play Billing `Purchase.isAutoRenewing`);
  - the bot reads cancellation from the provider on every `issue`/`status` (App Store Server API `autoRenewStatus`, Play `subscriptionsv2`) and from store notifications.
- Bot-side provider notifications, required:
  - Stripe webhooks
  - BTCPay webhooks
  - App Store Server Notifications V2
  - Play RTDN
  - covering: cancellations, grace/on-hold, refunds/voided purchases
  - On a notification the bot updates its records and re-reads provider state; credentials are never issued from a notification payload; nothing is pushed to the client.
- Tables (§3 ownership):
  - `products`, `offers`
  - `payments`
  - `charges`
  - `badge_ledger`
  - `issuances`
  - `badges` — without `use_from`, `paused_at`
- Engine per 2.9: issue requests, monthly re-issue, `advance`/`issue` recording, Monday presentation incl. removal updates, alert derivation.
- Alerts: the full 2.4 set (renewal approaching: store subscriptions only); opt-out toggle.
- Receipts for every Stripe/crypto payment with the checkout save prompt; `transfer` op deferred — support executes recovery manually against a presented receipt.
- Entry points: chat-list banner, Settings "Supporter perks" row, whats-new. Management screen without the Paused state; `charges` recorded from the first release, history UI deferred.
- New designs:
  - management screen
  - method + duration selector
  - crypto invoice screen
- Multi-profile; incognito profiles never present a badge.

### Required in the first release (or earlier)

1. +7 day recipient display grace (2.11) — in the earliest release, before purchases.
2. Week-boundary expiry + Monday presentation incl. removal updates (2.11).
3. Order-key-per-badge identity (§4).
4. Apple subscription group containing all subscription SKUs (both tiers × monthly/annual).
5. Refund handling: stop issuance + `debit(refund)`.
6. Stripe statement descriptor with short payment ref.
7. Receipts from the first payment.

### Deferred

| Deferred | Returns | Re-add path |
|---|---|---|
| `transfer` op | phase 2 | receipts exist from the first release; support transfers manually until then |
| pause / resume | phase 2 | ledger `resume` transition + `paused_at` column |
| charge history UI | phase 2 | `charges` recorded from the first release |
| upgrades (2.10) | phase 2 | store subscription group in place (item 4 above); bot SKU-change mapping + conversion debits |
| start-sharing question + "Show advanced privacy questions" (`use_from`) | phase 3 | immediate presentation and switching at MVP |
| user-picker badge presence | phase 2 | the Settings row is the entry point until then |
