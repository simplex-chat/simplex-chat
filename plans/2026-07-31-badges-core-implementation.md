# Supporter Badges v3 — core implementation plan

**Date:** 2026-07-31
**Product plan:** `plans/2026-07-30-supporter-badges-v3-ux.md` (referenced below as UX §n)
**Transport:** service RPC (`plans/2026-07-22-service-rpc-chat.md`, implemented, branch `rpc`)
**Scope:** MVP launch set (UX §7): commands `getBadgeCatalog | getBadgeInvoice | purchaseBadge | issueBadge` implemented; `upgradeBadgeSubscription | pauseBadge` and the `receipt` payment are defined in the protocol, post-MVP; no `use_from`, `paused_at`.

## 1. Client schema

`src/Simplex/Chat/Store/SQLite/Migrations/M20260731_user_badges.hs` — SQLite only; the Postgres variant is written when the schema is final; registered in the migrations list and cabal at delivery step 2.

- Table mapping to UX §3:
  - `badges` → `badge_purchases`
  - `issuances` → `badge_issuances`
  - `products` + `offers` → `badge_prices` + `badge_offers`
  - `badge_ledger` → `badge_ledger`
  - `payments` and `charges` — unprefixed, product-agnostic (UX §3 ownership)
- The payment row is inserted first, the badge row in the same transaction.
- Payment columns beyond the UX §3 list (`invoice_*` client-side; `grace_until` and `exception` on both sides, sent in `paymentState`):
  - `invoice_address` — the crypto screen address (UX 2.1)
  - `invoice_crypto_amount` — the fixed crypto amount (UX 2.1)
  - `invoice_expires_at` — the countdown and the expired state (UX 2.1)
  - `grace_until` — the provider grace/hold deadline (UX 2.4 payment failed; 2.6 state 5)
  - `exception` — the provider exception state: partial or over payment (UX 2.1)
- The badge–user match for `users.shown_badge_id` is enforced in code.
- `badge_ledger` has one definition — the `badgeLedgerTable` constant in the client migration; the service schema re-uses it literally.
- Settings (reminders opt-out; "Show new badges from Monday", post-MVP) are app settings, not schema.

### `badge_ledger`

A verbatim replica of the service ledger: the service is the only author; rows arrive in statements; the last row is the balance; each row is verifiable from its predecessor.

| column | meaning |
|---|---|
| `entry_id` | local autoincrement; PK; never on the wire |
| `entry_uuid` | service-assigned UUIDv7; sent both ways; UNIQUE index |
| `badge_purchase_id` | FK → `badge_purchases` |
| `change_months` | months added or removed: `+3` credit, `−1` debit |
| `balance_months` | unused months after this entry |
| `balance_start_ts` | start of the unused balance, after this entry |
| `balance_badge_type` | the type of the unused balance, after this entry |
| `was_paused_since` | set on the entry ending a pause: paused from this time until this entry |
| `service_created_at` | service time of the entry |
| `created_at` | local time the row was stored |
| `entry_type` | `credit` \| `debit` |
| `entry_credit_type` | the `type` tag of the `credit` value — any string, known or not |
| `entry_debit_type` | the `type` tag of the `debit` value — any string, known or not |
| `entry_type_unknown` | client only, added via ALTER TABLE after the shared table: 1 when the tag is beyond the client's version; the typed reference columns stay NULL |
| `entry_type_value` | client only, added via ALTER TABLE: the credit/debit object JSON, verbatim; present only for unknown tags; re-decoded after upgrade |
| `invoice_id` | field of `credit(payment)`; FK → `payments` |
| `charge_id` | field of `credit(charge)`; FK → `charges` |
| `from_purchase_id` | field of `credit(transferIn)`; FK → `badge_purchases`, nullable — the source may not exist locally |
| `to_purchase_id` | field of `debit(upgrade)` / `debit(transferOut)`; FK → `badge_purchases` |

| kind | type | meaning | when |
|---|---|---|---|
| credit | `payment` | months credited: a settled non-recurring payment — card, crypto, code, store one-time | MVP |
| credit | `charge` | months credited: one subscription charge, the first included | MVP |
| credit | `support` | months credited by support — compensation, gift | MVP |
| credit | `transferIn` | months arriving via transfer | post-MVP |
| credit | `opening` | balance restated: the ledger is reset to the stated amount, no relation to the previous row | MVP |
| debit | `refund` | balance removed: refund or chargeback | MVP |
| debit | `upgrade` | balance converted into an upgrade's credit | post-MVP |
| debit | `transferOut` | balance transferred out | post-MVP |
| debit | `support` | balance removed by support — leaked code batch | MVP |
| debit | `badge` | one month spent on the badge — a credential issued | MVP |
| debit | `lapse` | elapsed unissued months removed | MVP |

```
entry = credit — creditType: payment {invoiceId} | charge {chargeId} | support | transferIn {fromPurchaseKey} | opening
      | debit  — debitType: refund | upgrade {toPurchaseKey} | transferOut {toPurchaseKey} | support | badge | lapse
```

The protocol entry (`StatementEntry`, §4): `entryId`, `changeMonths`, `balanceMonths`, `balanceStartTs`, `balanceBadgeType`, `wasPausedSince`?, `createdAt`, `entryType` — the same sum with wire references.

## 2. Badge RPC service schema

`plans/2026-07-31-badges-service-schema.sql` — SQLite, like the client (the Postgres variant is written when the schema is final); the bot schema, extended with the purchase/ledger layer.

## 3. Client types

Domain types — `src/Simplex/Chat/Badges/Store.hs`. Records:

- `BadgePurchase`
- `BadgePayment`
- `BadgeLedgerEntry`
- `BadgeCharge`
- `BadgeIssuance`
- `BadgeAlert`
- `UserBadgeState`

Id newtypes:

- `BadgePriceId`
- `BadgeOfferId`
- `InvoiceId`

Enums:

- `BadgePlan`
- `BadgeItemStatus`
- `BadgeProvider`
- `BadgePaymentStatus`
- `BadgePurchaseStatus`
- `BadgeAlertKind`

Tagged sums:

- `BadgePaymentMethod` — `apple` | `google` | `card {provider}` | `crypto {currency}`; the last two map to the wire `ServicePaymentMethod`
- `BadgePurchasePayment` — `apple {paymentId, jws}` | `google {paymentId, token}` | `code {code}`; the `APIPurchaseBadge` argument, mapped to the wire `ServicePayment`
- `OfferDiscount` — `discount_type` with `free_months` / `discount` (§2)
- `LedgerEntryType` — `entry_type` with `credit_type` / `debit_type` (§2)
- `LedgerCreditType`
- `LedgerDebitType`

Reused from `Simplex.Chat.Badges`:

- `BadgeType`
- `BadgeInfo`
- `BadgeRequest`
- `BadgeCredential`
- `BadgeMasterKey`

Protocol types — `src/Simplex/Chat/Badges/Service.hs`, one constructor/field per JTD member:

- `BadgeServiceRequest`
- `BadgeServiceCommand`
- `ServicePaymentMethod`
- `ServiceInvoice`
- `CardProvider`
- `CryptoCurrency`
- `CurrencyAmount`
- `ServicePayment`
- `BadgeUpgrade`
- `BadgeServiceResponse`
- `ServicePaymentDestination`
- `BadgeServiceErrorCode`
- `BadgeCatalog`
- `BadgePrice` — the store type as well
- `BadgeOffer` — the store type as well
- `BadgeStatement`
- `BadgeBalance`
- `StatementEntry`
- `StatementEntryType`
- `StatementCreditType`
- `StatementDebitType`

Instances — added at implementation; enums follow the `BadgeType` conventions (Badges.hs:114); the unions encode the JTD discriminator `type`:

- `TextEncoding`
- JSON
- `ToField` / `FromField`

## 4. RPC protocol

`docs/protocol/badges-rpc.schema.json` — the JTD schema (definitions `request` and `response`); `docs/protocol/badges-rpc.md` — the protocol description (identity, idempotency, op semantics, statement, errors).

A request is an envelope: `version`; `purchaseKey`? (optional for `getBadgeCatalog` — signed, the response adds the purchase's `statement`; required for other commands, which are signed with it); `request` — the command, discriminated on `type`. Responses discriminate on `type`; response types are badge-namespaced, fields inside are plain.

| request `type` | request fields (beyond `purchaseKey`, `version`) | response `type` | response fields |
|---|---|---|---|
| `getBadgeCatalog` (signature optional) | — | `badgeCatalog` | `catalog`<br>`badgeStatement`? (for signed requests) |
| `getBadgeInvoice` | `priceId`<br>`offerId`? (absent for 1 month at the badge price)<br>`badgeInfo {badgeType, badgeExpiry?, badgeExtra}`<br>`paymentVia` — `card`: `provider`; `crypto`: `currency`<br>`upgrade`? — `fromPurchaseKey`, `receipt`, `receiptSignature`, `balance` | `badgeInvoice` | `invoice` — `invoiceId`, `price`, `discount`?, `credit`?, `amount` (= price − discount − credit), `currency`, `expiresAt`, `paymentTo` — `card`: `provider`, `url`; `crypto`: `currency`, `address`, `cryptoAmount`<br>`badgeType`<br>`months` |
| `purchaseBadge` | `badgeRequest` — `masterKey`, `badgeInfo`<br>`payment` — `apple`: `jws`; `google`: `token`; `invoice`: `invoiceId`; `code`: `code`; `receipt`: `receipt` (transfer, post-MVP)<br>`upgrade`? — `fromPurchaseKey`, `receipt`, `receiptSignature`, `balance` | `badgeCredential` | `credential`<br>`receipt`? (not provided for lifetime badges)<br>`statement` |
| `upgradeBadgeSubscription` | `badgeRequest`<br>`payment` — `apple`: `jws`; `google`: `token`<br>`balance` | `badgeCredential` | `credential`?<br>`statement` |
| `issueBadge` | `badgeRequest`<br>`balance` | `badgeCredential` | `credential`? (absent when the balance is exhausted)<br>`statement` |
| `pauseBadge` (post-MVP) | — | `badgeCredential` | `credential`?<br>`statement` |
| any, on failure | — | `error` | `code` (incl. `payment_pending`, `code_invalid` / `code_used` / `code_expired`)<br>`message`?<br>`retryAfter`? |

`statement` — record: `entries` — ledger entries; `previousEntryId`? — matches the client's asserted entryId, absent for the full ledger.

`balance` — record: `lastEntry` — the client's last ledger entry.

`catalog` — record: `prices`, `offers`.

| item | fields |
|---|---|
| price | `priceId` — UUID<br>`badgeType` — `investor` is never priced<br>`monthPrice`<br>`currency` — `usd`<br>`status` — `active` \| `deprecated` \| `disabled`; disabled is not sent<br>`createdAt` |
| offer | `offerId` — UUID<br>`priceId`? — absent applies to any price<br>`months`<br>`discount` — `freeMonths`: `freeMonths` \| `discount`: `discount` (percent)<br>`status`<br>`createdAt` |

Prices and offers have one type each across the protocol and the store: the UUID is the primary key, `createdAt` gives the sequence, and `payments` and `badge_purchases` reference them by UUID.

Offer price: `freeMonths` → `(months − freeMonths) × monthPrice`; `discount` → `months × monthPrice × (100 − discount) / 100`, floored.

Every offer states a discount; a plain 1-month purchase has no offer — its price is `monthPrice`, and `getBadgeInvoice` omits `offerId`. The duration selector is the fixed 1-month entry plus the offers.

`getBadgeInvoice` pins the price by `priceId`, and the badge type and currency with it; the service validates `badgeInfo.badgeType` against the price. Repricing appends a price and deprecates the old one: a deprecated price is honored at invoice creation, a disabled one rejected. Deprecated prices and offers are sent so that a catalog refresh cannot remove what the client pinned; disabled ones are omitted, since the service rejects them. An offer pinned to a price dies with it; unpinned offers survive repricing.

Rendering is app-driven: tiers and durations come from app resources, and one without a price is shown disabled. Store SKUs are absent from the catalog — store builds read prices from the store and SKUs from app config, and make no `getBadgeCatalog` requests, `issueBadge` on schedule and on wake keeping the ledger current. The service holds the SKUs for webhook mapping.

## 5. Commands and events

`ChatCommand` (Controller.hs; parsers in `chatCommandP`) — the UX 2.9 user actions. UX 2.9 actions without a command:

- reminder and presentation toggles — app settings (§1)
- pause / resume and the start-sharing date — post-MVP (Scope)
- cancel — the Cancel button opens the store management sheet; on its close the management screen re-reads state via `APIGetBadgeState` (UX §7)

```haskell
| APIGetBadgeState UserId                                        -- /_badge state <userId>
| APIGetBadgeCatalog UserId                                      -- /_badge catalog <userId>; unsigned getBadgeCatalog
| APIGetBadgeInvoice {userId :: UserId, priceId :: BadgePriceId, offerId :: Maybe BadgeOfferId, paymentVia :: BadgePaymentMethod}  -- /_badge invoice <userId> <priceId> [<offerId>] <via>; via — apple | google | card | crypto
| APIPurchaseBadge {userId :: UserId, payment :: BadgePurchasePayment}  -- /_badge purchase <userId> <json>
| APISwitchShownBadge {userId :: UserId, badgePurchaseId :: Int64}  -- /_badge shown <userId> <badgePurchaseId>
| APIAckBadgeAlert {userId :: UserId, kind :: BadgeAlertKind, episode :: Text, snooze :: Bool}  -- /_badge ack <userId> <kind> <episode> <snooze>
```

| command | UX | called when |
|---|---|---|
| `APIGetBadgeState` | 2.2, 2.3, 2.6, 2.1, §7 | app start — the initial model load; a badge screen is opened or regains focus |
| `APIGetBadgeCatalog` | §3 prices | the purchase screen is opened (non-store builds); non-blocking |
| `APIGetBadgeInvoice` | 2.1 | the user taps Pay on the selected duration |
| `APIPurchaseBadge` | 2.1, 2.8, §5 | the store purchase flow returns evidence; the user submits a code (2.6.9) |
| `APISwitchShownBadge` | 2.6.8, 2.7 | the user selects the shown badge |
| `APIAckBadgeAlert` | 2.4 | the user taps OK or "Remind me again" on an alert |

Purchase is two commands because the store purchase runs in the app between them: StoreKit and Play Billing are app-platform APIs, which core cannot call, and the store may deliver the result late (`pending` / Ask to Buy — via `Transaction.updates`, including after a restart). `APIGetBadgeInvoice` obtains the invoice for a card or crypto payment; after payment `APIPurchaseBadge` presents the store evidence, and the worker sends `purchaseBadge` — verification, the grant, and the first issuance in one round trip. Issuance has no command — `issueBadge` is core-driven (UX 2.9 engine), from the balance only: settlement credits months, issuance debits them (UX §3 ledger), so the first and every repeat issuance are the same command.

- `APIGetBadgeInvoice` starts every purchase: core loads or creates the live purchase row for the badge type's slot (per-user lock + `idx_badge_purchases_live`) — a `failed` row of the same slot is reused — creates the payment row, and points the badge row's `payment_id` at it (UX §3: the current payment). For `card` and `crypto` core sends `getBadgeInvoice` with the pinned `priceId` and the `offerId` of the chosen duration, and responds with the invoice — the Stripe link or the crypto screen data (UX 2.1). For `apple` and `google` core writes the rows, generates the invoice id itself, and sends nothing, since prices come from the store and SKUs from app config (§4); the app states the store because core is the same on both platforms. The invoice fields are stored on the payment row (§1), so pending-payment screens re-render after a restart; after invoice expiry a new `APIGetBadgeInvoice` creates a new invoice and payment row (UX 2.1).
- The app passes the invoice id to the store as the account token — Apple `Product.PurchaseOption.appAccountToken` (a UUID, echoed in the signed transaction), Google `BillingFlowParams.setObfuscatedAccountId` — so the store transaction states which payment it settles, and the service reads it from the verified store payload at `purchaseBadge`.
- `APIPurchaseBadge` completes a store purchase — the only payment whose result is delivered to the app: the store hands the app the evidence, and only that evidence ties the store transaction to the purchase, because the store flow knows neither purchase keys nor the service. Core records it on the payment row; the worker sends `purchaseBadge` — the service verifies, records the credit, and issues in one round trip (§6). Card and crypto payments need no completion command and carry no evidence: the service records their settlement from the provider webhook (UX §7 notifications); the worker's next `issueBadge` returns the credential, or `payment_pending` until the webhook arrives (§4).
- `APIPurchaseBadge` with a `code` sends the same `purchaseBadge` under the user lock, and differs only in the order of the writes: keys are generated first, and the badge and payment rows (`provider = code`, `price_id` and `offer_id` NULL) are written on success in one transaction, directly `issued`, because the badge type — and with it the slot — is stated in the response (UX 2.8). A live row of the granted slot is superseded (at most two badges per profile, UX 2.7); its unconsumed months stay on its purchase — purchases are unlinkable, so the service cannot move them; recovery per UX §3 (the `receipt` payment, post-MVP). On a timeout the error is surfaced to the user; a code consumed by a lost response is restored by support (codes tooling, delivery 7).
- `APIGetBadgeState` loads the badge state into the app model at start (and on profile switch); events only update the model afterward, so without the initial read it would hold nothing at first render — the 2.2 banner is rendered from it. It reads stored state and sends nothing itself. The same call re-reads state when a badge screen is opened or regains focus, and signals the worker (§6); reconciliation results follow as `CEvtBadgeChanged`. Screen re-focus covers the returns that fire no core trigger: the store cancellation sheet close — UX §7 "the engine sends `status` on return"; the in-app sheet fires no foreground trigger — and return to a pending-payment screen after payment (UX 2.1), which on desktop produces no foreground event either.

`ChatResponse`:

```haskell
| CRBadgeState {user :: User, badgeState :: UserBadgeState}
| CRBadgeCatalog {user :: User, prices :: [BadgePrice], offers :: [BadgeOffer]}
| CRBadgeInvoice {user :: User, payment :: BadgePayment}
```

- `CRBadgeState` — the state the badge surfaces render (banner 2.2; picker 2.3; management screen 2.6); the response of every command except `APIGetBadgeInvoice` and `APIGetBadgeCatalog`. Worker results follow as `CEvtBadgeChanged`.
- `CRBadgeCatalog` — the refreshed catalog for the open purchase screen (UX §3 prices).
- `CRBadgeInvoice` — the purchase continuation (`APIGetBadgeInvoice`): `payment` holds the invoice fields — the Stripe link, or the crypto address, amount, and expiry (UX 2.1); for a store payment, the locally generated invoice id, which the app passes to the store as the account token.
- Errors: `ChatErrorType` gains `CEBadgeServiceError {badgeError :: BadgeServiceErrorCode, message :: Maybe Text, retryAfter :: Maybe Int}` — the inline code-redemption errors (UX 2.8) and the purchase-screen unavailable notice (UX 2.1).

`ChatEvent`:

```haskell
| CEvtBadgeChanged {user :: User, badgeState :: UserBadgeState}
| CEvtBadgeAlert {user :: User, alert :: BadgeAlert}
```

- `CEvtBadgeChanged` — emitted by the worker on any state change it applies (UX 2.9 events); open badge surfaces re-render from it.
- `CEvtBadgeAlert` — the derived current alert (UX 2.4); the app displays it and answers with `APIAckBadgeAlert`.

The reminders opt-out is an app setting: core emits `CEvtBadgeAlert` for all kinds; the app suppresses display of the two reminder kinds (`BARenewalApproaching` and `BAPrepaidEnding`) when reminders are off, and ignores them in `UserBadgeState.alert` for the picker indicator (UX 2.3).

## 6. BadgeManager

Runs in core. Controller state; the worker is the agent `Worker` framework the controller already uses (`getAgentWorker` / `hasWorkToDo'` / `cancelWorker`; the `TMap k Worker` fields, Controller.hs:304):

```haskell
data BadgeManager = BadgeManager
  { badgeWorkers :: TMap UserId Worker,          -- agent Worker: doWork TMVar, restart on crash
    badgeLocks :: TMap UserId Lock,              -- one signed badge op per user in flight
    badgeReads :: TMap UserId UTCTime,           -- read requests from APIGetBadgeState (step B)
    badgeBoundaries :: TMap UserId UTCTime,      -- next boundary per user
    badgeTimerAsync :: TVar (Maybe (Async ()))   -- the timer thread; sleeps to the earliest boundary
  }
```

One worker per user because badge state is per profile (UX 2.12): the worker serializes that profile's RPC ops and row writes, and profiles do not delay one another. The worker holds no task queue: a trigger only signals it, and each pass derives the work from stored state (the reconcile step below). Ops are idempotent (§4), so lost and duplicated signals are harmless.

Lifecycle:

1. start — the first signal for a user creates the worker via `getAgentWorker`; the timer thread is started at chat start (`badgeTimerAsync`).
2. signal — every trigger performs `hasWorkToDo'`: the UX 2.9 triggers (chat start, foreground, and network restore signal every user with a badge or payment row; profile switch — the switched-to user), the timer thread, and the commands — `APIGetBadgeState`, which also records the request time in `badgeReads`; `APIPurchaseBadge` after the evidence is recorded, and again for presentation after a code (step E); `APISwitchShownBadge` for presentation.
3. pass — the loop takes the signal and runs the flowchart below once per live badge row (the paid and the investor slots, §5).
4. re-run — a signal that arrives during a pass stays in `doWork`; the loop runs one more pass, so state changed mid-pass is picked up.
5. idle — between passes the loop blocks on `doWork`.
6. stop — workers are stopped with `cancelWorker` at chat stop and on user deletion; the timer thread with them.

The worker and the commands that send signed requests themselves (`APIGetBadgeInvoice` — `getBadgeInvoice`; `APIPurchaseBadge` with a code — `purchaseBadge`) take the user's lock in `badgeLocks`, so one signed command per user is in flight; the same lock guards get-or-create of the badge row (§5). The unsigned, purchase-independent `getBadgeCatalog` (`APIGetBadgeCatalog`) is sent outside the lock. `badgeLocks` follows the `withEntityLock` discipline (Library/Internal.hs:127) — `chatLock` is waited for first — so the step E broadcast creates no new lock order.

There is no read command in the protocol: every response carries the `statement`, so state advances with the work. A purchase whose balance has lapsed sends nothing, and service-side credits (support, transfer-in post-MVP) would stay unseen; the signed `getBadgeCatalog` is the check — its response adds `badgeStatement` for the signing key, and it is the same request that prices a new purchase (§4). The pass reads the `badgeReads` entry of `APIGetBadgeState`: a request newer than the last response, a lapsed balance, and no due command → signed `getBadgeCatalog`, then the entry is cleared. It is also sent after a restore (UX §3 recovery).

Timer — each pass reports the user's next boundary, the earliest of:

- `renews_at` − 3d — renewal reminder (UX 2.4)
- paid-through − 3d — prepaid ending (UX 2.4)
- paid-through — ended alerts (UX 2.4); sender-side perk cutoff (UX 2.11)
- `alert_snooze_until` — the snoozed alert is emitted once more (UX 2.4)
- next Monday 00:00 UTC — presentation and removal updates (UX 2.11)

Each pass writes its user's next boundary into `badgeBoundaries`; the timer thread sleeps until the earliest entry and signals the workers whose boundaries elapsed.

```mermaid
flowchart TD
  T[trigger: UX 2.9, timer, command] --> S[hasWorkToDo']
  S --> W{pass running?}
  W -- yes --> H[signal held -> one more pass]
  W -- no --> A[load badges + payments + last ledger rows]
  A --> B[per live badge, reconcile:\nstore evidence unverified -> purchaseBadge\npayment in status new -> getBadgeInvoice re-sent\ninvoiced/pending non-store -> issueBadge\nsubscription past renews_at -> issueBadge\nbalance > 0, month unissued -> issueBadge\nlapsed balance, read requested -> signed getBadgeCatalog]
  B --> C[request under the user lock, signed with the purchase key\non ASETimeout the same envelope is retried at the next signal]
  C --> D[apply response:\ncatalog -> badge_prices / badge_offers\nstatement: entries -> badge_ledger verbatim, an opening entry restates the balance;\npayments and charges -> the local rows\nverify credential -> update badge_purchases credential columns\nerror: terminal code -> badge failed; transient -> hold the command until retryAfter elapses]
  D --> E[presentation:\non/after Monday 00:00 UTC, or immediately on acquisition/switch:\nshown badge credential -> setUserBadge + broadcast, fresh proof per send, incognito connections skipped\nexpired unrenewed -> setUserBadge Nothing + broadcast\npaid expired + investor held -> switch shown to investor]
  E --> F[derive alert from state\nif differs from acked/snoozed -> CEvtBadgeAlert]
  F --> G[emit CEvtBadgeChanged if state changed\nwrite next boundary to badgeBoundaries]
  G --> I[block on doWork]
```

Non-store purchase:

```mermaid
sequenceDiagram
  participant UI as app
  participant C as core (BadgeManager)
  participant B as bot
  participant P as Stripe/BTCPay
  UI->>C: APIGetBadgeInvoice priceId offerId card|crypto
  C->>C: load or create the badge row; create the payment row (lock + unique index)
  C->>B: getBadgeInvoice (signed; priceId, offerId, badgeInfo, paymentVia)
  B->>P: create intent / invoice
  B-->>C: badgeInvoice (invoiceId, price, discount, amount, expiresAt, paymentTo)
  C-->>UI: CRBadgeInvoice (url / address, cryptoAmount, expiresAt)
  UI->>UI: browser (card) or the in-app invoice screen (crypto); user pays
  P-->>B: webhook: settled
  B->>B: credit the ledger
  UI->>C: APIGetBadgeState (the screen regains focus)
  C->>B: issueBadge (signed; badgeRequest, balance)
  B->>B: debit the month; issue
  B-->>C: badgeCredential (credential, receipt, statement)
  C->>C: store rows; verify credential; update badge; presentation
  C-->>UI: CEvtBadgeChanged
```

Store purchase:

```mermaid
sequenceDiagram
  participant UI as app
  participant S as StoreKit / Play Billing
  participant C as core (BadgeManager)
  participant B as bot
  participant PR as store server API
  UI->>C: APIGetBadgeInvoice priceId offerId apple|google
  C->>C: load or create the badge row; create the payment row with a local invoice id (lock + unique index)
  C-->>UI: CRBadgeInvoice (invoice id)
  UI->>S: purchase(sku from app config, accountToken = invoice id)
  S-->>UI: evidence (JWS / purchase token)
  UI->>C: APIPurchaseBadge paymentId evidence
  C->>B: purchaseBadge (signed; badgeRequest, payment: apple jws / google token)
  B->>B: verify the JWS offline (Apple)
  B->>PR: verify + acknowledge the purchase token (Google)
  B->>B: credit the ledger; debit the month; issue
  B-->>C: badgeCredential (credential, receipt, statement)
  C->>C: store rows; verify credential; update badge; presentation
  C-->>UI: CEvtBadgeChanged
```

Badge row status:

```mermaid
stateDiagram-v2
  [*] --> acquiring: APIGetBadgeInvoice
  [*] --> issued: APIPurchaseBadge with a code, created on success
  acquiring --> issued: credential verified and stored
  acquiring --> superseded: a new row takes the slot
  acquiring --> failed: terminal error (step D)
  issued --> issued: renewal / re-issue (credential updated in place)
  issued --> superseded: a new row takes the slot (a code; upgrade post-MVP)
  failed --> acquiring: get-or-create reuses the slot's failed row; payment_id repointed
```

## 7. UX coverage

Each UX plan point and its implementation home:

| UX | implementation |
|---|---|
| 2.1 method + duration selector, prices | app UI; `APIGetBadgeCatalog`; `badge_prices` / `badge_offers` (§1); the 1-month entry is priced by `monthPrice` (§4) |
| 2.1 unavailable options | a tier or duration with no price is shown disabled (§4); `offer_disabled` / `product_unavailable` |
| 2.1 crypto and Stripe screens | `APIGetBadgeInvoice` → `CRBadgeInvoice`; invoice columns (§1); payment statuses from the statement's payments; partial/over payment in `payments.exception` |
| 2.1 receipt save prompt | `receipt` in the `badgeCredential` response → `payments.receipt_code`; the prompt is app UI |
| 2.2 banner | app UI over `CRBadgeState` / `CEvtBadgeChanged` |
| 2.3 user picker | post-MVP (UX §7 Deferred); state and `alert` in place; the Settings row is the MVP entry point |
| 2.4 alerts | `BadgeAlertKind`; `alert_acked_kind` / `alert_acked_episode` / `alert_snooze_until` / `grace_until` (§1); worker step F; `CEvtBadgeAlert`; `APIAckBadgeAlert` |
| 2.5 start-sharing | post-MVP (`use_from`, Scope); immediate presentation in worker step E |
| 2.6 management screen | `UserBadgeState`; the commands table (§5) |
| 2.7 held badges, switching | `users.shown_badge_id`; `APISwitchShownBadge`; the investor fallback in step E |
| 2.8 redeem codes | `APIPurchaseBadge` with a code → `purchaseBadge`; payment `provider = code` |
| 2.9 triggers | worker lifecycle item 2 (§6) |
| 2.9 engine | the worker flowchart (§6) |
| 2.9 API calls | the commands table (§5) |
| 2.10 upgrades | post-MVP (UX §7 Deferred); the `superseded` status in place |
| 2.11 dates | bot `sundayAfter` (delivery 7); Monday in step E and the timer; `mkBadgeStatus` grace (delivery 1) |
| 2.12 multi-profile | one worker per user; the incognito skip in step E |
| 2.13 pause | post-MVP; `pauseBadge`, and `issueBadge` resumes (§4) |
| §3 ownership, tables | the §1 mapping; the service schema (§2) |
| §3 catalog rules | `badge_prices` / `badge_offers`; reconciliation (delivery 4) |
| §3 payments, charges | §1; the statement's payments and charges (§4) |
| §3 ledger, issuances | replicas (§1); the `balance` assertion and the `opening` restatement (§4); service transitions (delivery 7); tests (delivery 8) |
| §3 recovery | `payments.receipt_code`; signed `getBadgeCatalog` after restore (worker); capped store re-bind at `purchaseBadge` (delivery 7); the `receipt` payment post-MVP |
| §4 wire protocol | `Badges/Service.hs`; `docs/protocol` |
| §5 providers | delivery 7; the §5 command bullets |
| §6 decisions 11–14 | 11 — `renews_at` / `cancelled` / charges kept; 12 — catalog seed (delivery 2); 13 — `sundayAfter`; 14 — `paidThrough` in `UserBadgeState` |
| §7 MVP set | Scope; the delivery order |

## Delivery order

1. `mkBadgeStatus`: the +7-day recipient display grace and the shifted `BSExpiredOld` boundary (UX 2.11) — released before purchases (UX §7).
2. Register migration `M20260731_user_badges` (migrations list + cabal + regenerated `chat_schema.sql` / `chat_lint.sql`); the Postgres variant of the migration; the catalog seed from app config (UX §3 prices); store functions (`Store/Badges.hs`): get-or-create with lock; last-ledger-row reads; verbatim replica inserts.
3. Instances for the types in `Badges.hs` and `Badges/Service.hs` (§3) + roundtrip tests.
4. RPC codec: `docs/protocol/badges-rpc.schema.json` packets ↔ `Badges/Service.hs` types; catalog reconciliation on every `badgeCatalog` response.
5. BadgeManager §6: worker, locks, timer; reconcile/apply/presentation/alert steps; events.
6. Commands §5 + parsers + `View.hs` rendering.
7. Bot: schema §2; ledger transitions (UX §3); providers (Apple offline JWS, Google verify+acknowledge, Stripe intents+webhooks, BTCPay invoices+webhooks); codes tooling; notifications endpoints; receipt generation and hashing on settlement (UX §3 recovery); the capped store re-bind on `purchase` (UX §3 recovery); store setup — one Apple subscription group for all subscription SKUs and the Stripe statement descriptor with the short payment ref (UX §7).
8. Tests: ledger properties 1–4 (UX §3); the `balance` assertion and the `opening` restatement; `purchaseBadge` and `issueBadge` idempotency; replica equality service↔client; price and offer lifecycle at `getBadgeInvoice`; alert derivation incl. supersession; Monday presentation incl. removal updates; provider sandbox flows.
