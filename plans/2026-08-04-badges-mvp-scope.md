# Supporter Badges v3 — MVP scope

**Date:** 2026-08-04
**Product plan:** `plans/2026-07-30-supporter-badges-v3-ux.md` — UX §n
**Core plan:** `plans/2026-07-31-badges-core-implementation.md` — §n
**Protocol:** `docs/protocol/badges-rpc.md`, `docs/protocol/badges-rpc.schema.json`
**Service schema:** `plans/2026-07-31-badges-service-schema.sql`

Order: one path end to end first, then the paths that need external systems. Each milestone ends with passing tests.

## 1. Badge service

`apps/simplex-badge-service` — a bot on core as a library, after `apps/simplex-directory-service`.

- `BadgeService/Options.hs`, `Service.hs` — the RPC handler; `Store.hs`, `Store/SQLite/Migrations.hs` — the schema (§2), with `badgeLedgerTable` imported from the client migration.
- Request handling: envelope version check, signer key against `purchaseKey`, dispatch on the command.
- Ledger writes: one transaction per command; the statement is read back from the written entries.
- Credential signing: the badge key set and `sundayAfter` expiry (UX 2.11).
- Codes: hashes, batch, redemption (UX 2.8).
- HTTP listener for provider webhooks — `warp`, as in `tests/NameResolver.hs`, or simplexmq `getHTTP2Server`. Stripe signature verification reads the raw body.

## 2. Core badge API

§5 and §6, minus alerts.

- Commands: `APIGetBadgeCatalog`, `APIGetBadgeInvoice`, `APIPurchaseBadge`, `APIGetBadgeState`, `APISwitchShownBadge`.
- Responses and events: `CRBadgeState`, `CRBadgeCatalog`, `CRBadgeInvoice`, `CEvtBadgeChanged`.
- Store functions (`Store/Badges.hs`): get-or-create under the user lock, last-entry reads, verbatim replica writes.
- `BadgeManager`: worker, locks, reconcile, apply, presentation. The timer and `CEvtBadgeAlert` follow later.
- JSON instances for `Badges/Service.hs` and `Badges/Store.hs` — `taggedObjectJSON`, with roundtrip tests.
- Register `M20261001_user_badges` in the migrations list and cabal; regenerate `chat_schema.sql` and `chat_lint.sql`.

## 3. Store integration — iOS, Android

- App Store Connect: badge products, one subscription group, sandbox testers.
- Play Console: SKUs, an internal track build, licence testers.
- Swift and Kotlin: read prices and SKUs, run the purchase with the invoice id as the account token (`appAccountToken`, `setObfuscatedAccountId`), pass the evidence to `APIPurchaseBadge`, handle late results (`Transaction.updates`, `queryPurchasesAsync`).
- Store SKUs are held in app config and in the service `store_skus` table.

Console setup starts on day one — its latency is external.

## 4. Provider integration

Direct from the service; `http-client-tls` is available through simplexmq.

| provider | outbound | inbound |
|---|---|---|
| Stripe | checkout session or payment intent | webhook: settled, refunded, subscription charge |
| BTCPay | invoice with address and amount | webhook: settled, partial, expired |
| Apple | — | server notifications v2 |
| Google | Publisher API verify and acknowledge | RTDN |

Every inbound event is recorded in `provider_events` before processing.

## 5. Tests

`tests/Bots/BadgeTests.hs`, after `tests/Bots/DirectoryTests.hs`: the service in-process, chat clients as apps.

- Provider mock — a Warp app on a free port with a mutable registry, after `tests/NameResolver.hs`: responses and webhook delivery are driven by the test.
- Coverage: code redemption; invoice, settlement, issuance; store evidence; renewal charges; replica equality service to client; idempotency of every command; price and offer lifecycle at `getBadgeInvoice`.

## 6. Ledger algebra

After the end-to-end path works.

- Every entry type against every prior state; the resulting balance, its start, and its badge type.
- Properties: the balance never falls below zero; an `opening` entry restates it absolutely; issuance debits exactly one month; lapse debits only elapsed unissued months.
- The client's balance function against the service ledger, over generated histories.

## Milestones

| # | end state | needs |
|---|---|---|
| 1 | code redemption end to end: `purchaseBadge` with a code returns a credential, the client stores the replica and shows the badge | §1 service skeleton, §2 core API, §5 harness |
| 2 | card and crypto: invoice, mocked settlement, issuance, renewal reminder date | §4 Stripe and BTCPay, webhook listener |
| 3 | store purchase on both platforms | §3, console setup |
| 4 | subscriptions: renewal charges, cancellation, grace | charges wire identifier, §4 notifications |
| 5 | ledger algebra, unit tested | §6 |

## Settle before milestone 4

- The wire identifier for charges — the service's local integer cannot be resolved by the client.
- Ledger unification: whether entries reference invoices, charges, and purchases by UUID and key rather than by local id.
