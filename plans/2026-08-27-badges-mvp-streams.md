# Supporter badges — beta slice and stream plan

**2026-08-27** · protocol `docs/protocol/badges-rpc.{md,schema.json}` · model and copy rules `plans/2026-07-30-supporter-badges-v3-ux.md` · core API `plans/2026-07-31-badges-core-implementation.md`. The product plan's §4 wire protocol was superseded before implementation; `badges-rpc.md` is the protocol.

---

## 1. The slice

Beta ships one route to a badge: **buy a code in a browser, redeem it in the app.** The app asks no purchase questions — tier, duration and method are the site's.

Deferred, still in the protocol: in-app invoices, store purchase, subscriptions, upgrades, transfers, pause, alerts.

**First, no dependencies:** divert the apps' purchase path so nothing reaches a store charge — it takes real money today and issues no badge. The views and the store code stay in place, uncalled.

Accepted: a code is a bearer instrument. Whoever sees it can spend it, and this slice has no revocation.

## 2. State on `badges`

Other branches may carry work this does not see.

- **Written** — credential signing, verification, proofs, status, presentation; protocol types, RPC docs, JTD schema; client and service schema incl. this plan's tables; app banner, settings entry, what's-new
- **Scaffold** — badge service answers every request `unsupported_version`
- **Not written** — JSON instances for most protocol types; core client badge API, store, worker; checkout site and providers; app redeem view (title-only stub), badge-state screen, browser hand-off
- **To divert** — the app's tier/duration screens, which today lead to a store purchase

Registering the client migration needs `STRICT` on all ten SQLite tables; Postgres must not have it.

---

## 3. Contracts between streams

**The code** — `SXB-` + 20 Crockford chars in four groups, one of them a check character. Normalisation upper-cases and folds `I`/`L`→`1`, `O`→`0`, identically both sides. Only `SHA-256` of the normalised code is stored anywhere. Site codes are `HMAC(secret, orderId)`; operator codes are random.

**`redeemBadgeCode`** over service RPC — signed with an Ed25519 key generated per code and reused by a retry of it. Carries the badge master key and the code, nothing else. Returns the signed credential and a **statement**, or `code_invalid` | `code_used` | `code_expired` | `rate_limited` | `internal`.

A *statement* is an extract of the badge's ledger — the months balance, as a list of entries. The ledger is authored by the service alone; the client keeps a verbatim replica and reads its balance from the last entry.

**The catalog** — one source in the service, one total function. The site renders those totals; the browser never multiplies a price by a month count. The app does not read it this release.

The code is the only thing crossing site → app. The site never sees a purchase key, credential or ledger; the app never sees an order.

---

## 4. Stream 1 — codes for badges

### 4.1 Redemption becomes its own command

`purchaseBadge {badgeRequest, payment:{type:"code"}}` asks the caller for a tier and expiry it cannot know and the service must override.

- `redeemBadgeCode {masterKey, code}` → `badgeCredential {credential, receipt?, statement}`
- `code` leaves the payment union; `purchaseBadge` keeps the rest — `apple`, `google`, `invoice`, `receipt` — and stays unimplemented
- the tier is stated by the credential, which the client verifies
- client command `APIRedeemBadgeCode {userId, code}`

One thing here fails quietly. The request envelope carries an optional `purchaseKey`, and the service decides per command whether that key must already name a purchase — most require it and answer `unknown_purchase_key` otherwise. `redeemBadgeCode` is the exception: it *creates* the purchase, so on a first redemption the key is always unknown. Miss the special case and the lookup falls through to the default, every first redemption is refused, and nothing fails to compile because it is a runtime lookup. Worth its own test.

### 4.2 Schema

The tables, in layers:

- **money** — `invoices` (an amount owed), `payments` (an amount paid), `subscription_charges`. Generic: they carry no idea what was bought.
- **catalog** — `badge_prices` (tier → price per month), `badge_offers` (duration discounts).
- **what an invoice bought** — `badge_invoices` for a purchase, `badge_code_invoices` for a voucher. Same shape, except the second names no purchase: at the time of sale none exists, and the buyer may never be the redeemer.
- **the badge** — `badge_purchases` is the anchor: keys, tier, status, funded by either `payment_id` or `badge_code_id`. Its balance is `badge_ledger`, its credentials `badge_issuances`, and `users.shown_badge_id` names the one on show.
- **the voucher** — `badge_codes`. Consumed into a purchase; never a badge itself. The client has a copy for one reason: it holds the keys a redemption is signed with, so a retry can be the same signer (§4.3).

The beta path: the site writes an invoice and what it bought, then a code. The app redeems that code, which creates the purchase and its first issuance — and, from milestone D, its ledger.

New here are `badge_codes` (shared) and `badge_code_invoices` (service only); both are in the migration modules already. Every schema change touches four files — the shared block and the service migrations each exist twice, SQLite and Postgres, unlinked by the build.

### 4.3 Retry is safe

A redemption is signed with a key the client generates for it. If that key is generated fresh on every attempt, a timeout is unrecoverable: the service may already have redeemed the code, and the retry arrives as a *different* signer, so it reads as someone else presenting a spent code — `code_used`. The user has paid and cannot get the badge.

The fix is for a retry to be the **same** signer:

- the service's replay keys on **(code hash, verified signer)**: the same code from the same key returns the credential it already issued and writes nothing
- so the client writes the signing keys into its `badge_codes` row **before** sending, found by the code hash, and a retry reads them back

That row is a stash for the in-flight attempt. Its fate depends on the outcome:

| outcome | the row |
|---|---|
| success | kept, completed with the tier and months, and pointed at by the new purchase |
| terminal error — `code_invalid`, `code_used`, `code_expired` | deleted: the code will never work, so the keys are dead |
| timeout | **kept** — this is the case it exists for |

The keys go on the code row because **code redemption cannot create its `badge_purchases` row up front.** That row's badge-type columns are `NOT NULL` and a code carries no tier, so there is nowhere to put them until the service answers. An in-app purchase has no such problem — the tier was picked on screen, so it creates its purchase in `acquiring` immediately.

### 4.4 Service

Store layer, ledger transitions, credential signing, code minting and classification, RPC dispatcher, then the redeem and issue handlers.

**Nothing is written until the credential is signed.** Look the code up, compute the ledger changes in memory, sign — and only then open one transaction that writes the purchase, the ledger rows, the issuance and the redemption together. Signing is the step most likely to fail for reasons unrelated to the request. If the code were marked redeemed first, a signing failure would leave it spent with no credential behind it: dead, and revivable only by an operator. Signing first means a failure touches nothing and the user can simply try again.

**Errors say as little as possible.** An unknown code and a malformed one both answer `code_invalid`, so someone guessing learns nothing from the difference. A code already redeemed answers `code_used` — but only to a *different* key; the key that redeemed it gets its credential back (§4.3).

### 4.5 Core client

Badge store, commands and events, re-issue worker, redeem path.

- verify the credential against the configured issuer keys **before** writing anything
- copy the statement's entries into the ledger replica exactly as received — never compute a balance locally, never edit or invent an entry; one author means client and service cannot disagree
- on success, one transaction: write the purchase and its issuance, copy the ledger entries, complete the code row, retire the profile's previous badge of the same kind, and point the profile at the new one — then release the lock and present to contacts
- split `addUserBadge` first — it verifies, stores and broadcasts under the global chat lock in one function and raises command errors; the redeem path needs a per-user lock and a service error code

A profile shows one badge at a time and holds at most two: a paid one and an investor one. Redeeming a code fills that kind's place, and the purchase that was there moves to `superseded`. Its unspent months stay with it — purchases are unlinkable, so nothing can move a balance between them. That matters for **Add more months** (§4.6): a second code starts a new balance rather than topping up the old one.

### 4.6 Apps

- *Support SimpleX* — **Get the code** (opens the site; absent on store builds) and **Redeem the code** (everywhere)
- *Redeem code* — formats as typed, folds ambiguous characters, verifies the check character before sending, one message per service error
- *Supporter perks* — the badge, that it is shown, the date support **ends**, **Add more months**; plus the ended state
- **Diverted** — tier and duration screens leave the flow, kept compiled and uncalled; store product loading and the catalog command lose their last callers
- **Copy** — one date, the paid-through date from the ledger, never the credential's expiry; *ends*, never *renews*

### 4.7 Issuing without selling

Compensation codes need no new mechanism: minted by the operator, random rather than derived, printed once, stored as hashes.

Investor badges are not a special case: an operator mints a code of that tier and it is redeemed by the same path as any other. Nothing in the schema or the redemption flow distinguishes them.

A badge that never expires is a separate question, and deferred — the ledger holds a month count, so "forever" has no representation. Until it does, a long finite term serves: the count is a byte, so twenty years is expressible.

### 4.8 Order of work

**The ledger is stubbed until last** — redemption issues one credential and writes no ledger rows, and the statement comes back empty. Everything before D is a working badge without accounting.

**First, independent:** divert the apps' purchase path (§1).

**A — core.** Migration registered with `STRICT` and both dumps regenerated; code format in the shared library; `redeemBadgeCode` types, JSON, schema, docs and client command; service store, signing, dispatcher and redeem handler; a minimal mint command. *Done when* a minted code redeemed from the terminal puts a badge on the profile and contacts see it.

**B — apps.** Support screen with its two actions, redeem screen, badge-state screen, copy. Both platforms in step. *Done when* the same works by pasting a code into the app.

**C — unhappy paths.** Retry idempotency — keys stashed before sending, replay keyed on (code hash, signer). Error mapping for `code_invalid`, `code_used`, `code_expired` and a locally failed verification.

**D — ledger.** Transitions, credit on redemption, debit on issue, lapse; statement in the response; client replica; monthly re-issue worker. *Done when* a three-month code re-issues at the month boundary and both ledgers match row for row. Property tests here: balance never below zero, issuance debits exactly one month, lapse removes only elapsed unissued months.

### 4.9 Done means

- an operator-minted code redeems on desktop, Android and iOS and shows on the profile and in member lists
- the same code twice from one profile returns the same badge and consumes nothing
- a timeout then a retry issues exactly one badge, no operator involved
- every service error renders inline, as does a locally failed credential verification
- an operator-minted code redeems by the same path as one bought on the site, whatever its tier
- client and service ledger rows match row for row; a crossed month boundary re-issues without a restart
- redeeming a code writes no payment row

---

## 5. Stream 2 — selling codes on the web

Takes money and produces codes: a page, card and crypto payment, and whatever server side that needs. Its only output that stream 1 consumes is a code.

**What it owes stream 1**

- codes in §3's format and normalisation, with only their `SHA-256` stored
- prices from the service catalog — one source and one total function, so the site and the app cannot disagree
- `badge_code_invoices` filled with what each invoice bought; the table is already in the schema

**Its own to settle.** Hosting and deployment, endpoints, which providers, how the browser learns an order settled, how long a code stays retrievable after purchase, and what reference support resolves against. None of it reaches stream 1.

**Before the tier page ships:** it advertises a storage-duration perk — confirm that perk exists (§6) or drop the claim.

---

## 6. Stream 3 — perks

What an active badge changes for the user: XFTP file size and storage duration, granted against the badge proof the sender presents.

Integration points, in dependency order:

1. a presentation context binding a proof to the operation it authorises, so a proof lifted from a profile cannot authorise an upload *(client)*
2. recipient-side size verification against the presented proof *(client)*
3. carrying the proof to the file server on upload *(simplexmq)*
4. issuer keys in file-server config, so it can verify one *(simplexmq)*
5. per-file size and retention derived from the verified badge type *(simplexmq)*

1 and 2 are client-side and independent of the rest. Target values: supporter 2GB and 7 days, legend 5GB and 21 days.

---

## 7. Ledger — for information

The months accounting, and a mechanism inside stream 1 rather than a stream of its own: the service authors every entry, the client keeps a verbatim replica and reads the balance from the last one.

Beta needs three operations — credit on redemption, debit on issue, lapse on elapsed months — which is milestone D. The rest of the ledger's operations arrive with the features that need them.
