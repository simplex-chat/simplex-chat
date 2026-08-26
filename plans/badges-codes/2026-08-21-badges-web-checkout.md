# Badge purchase site and code redemption

**Date:** 2026-08-21
**Branch:** `sh/badges-codes` (off `badges`)
**Status:** approved, multi-session. This file is the source of truth; agents update the progress tracker in it.
**Supersedes:** `plans/2026-08-04-badges-mvp-scope.md` milestone 2, the in-app invoice flow. Milestone 3 (store purchase) is unaffected.
**Design:** `plans/badges-codes/badges-flow-mvp.svg` — every screen in the app and on the site, the states between them, and the site at phone width. It records five deviations from this plan, each argued in its own caption: a landing page D3 does not specify, B4 merging D3's method and checkout screens, no in-app wizard at all, the badge art on the tier cards against D2's logo-only rule, and a storage perk that has no implementation anywhere yet.
**Citation keys:** `plans/2026-07-30-supporter-badges-v3-ux.md` = **UX §n**; `plans/2026-07-31-badges-core-implementation.md` = **core §n**; `docs/protocol/badges-rpc.md` = **RPC §n**; `plans/2026-08-04-badges-mvp-scope.md` = **MVP §n** (superseded, but still the source for the provider-event rules).

---

## 1. Context

Supporter badges are designed but the payment path is unfinished.

| Area | State today |
|---|---|
| `apps/simplex-badge-service/` | Scaffold. Starts a bot, receives `CEvtServiceRequest`, answers every request with `unsupported_version` (`BadgeService/Service.hs:113-120`). No store queries, no ledger, no signing. |
| Protocol | Types and documentation are complete: `docs/protocol/badges-rpc.{md,schema.json}`, `Badges/Service.hs`, `Badges/Types.hs`, `PaymentService{,/Types}.hs`. The `code` payment (`SPCode`, `PaymentService.hs:29`) is documented at `badges-rpc.md:15,33`; `code_invalid` and `code_used` at `:64`. `code_expired` exists in `BadgeServiceErrorCode` (`Badges/Service.hs:194`) but is undocumented in `badges-rpc.md`; H6 adds it. The JTD schema types the error code as an opaque string rather than an enum. |
| Crypto | Signing and verification are complete: `Simplex.Chat.Badges` has `issueBadge`, `verifyCredential`, `badgeProof`, `verifyBadge`. `mkBadgeStatus` (`Badges.hs:130`) derives status from a supplied `badgeExpiry` and applies no week alignment. `sundayAfter` is specified in UX §3 and `badges-rpc.md:29` but has no implementation anywhere in the code; B2 adds it. |
| Client schema | Written (`M20260731_user_badges.hs`, SQLite and Postgres) but registered in neither migration list. Its 10 tables declare no `STRICT`, which the SQLite schema-dump test requires (A1). |
| iOS / Play Android | The store transaction completes (`BadgeStore.swift`, `android/src/google/.../PlayStore.kt`), but the evidence goes nowhere: the flow ends in a diagnostic alert (`BadgesPayView.kt:220`). **No badge is issued.** Verifying store evidence is out of scope here (decision 6, §6). |
| Desktop / FOSS Android | No way to pay at all. Markers: `TODO [badges] desktop and foss pay via Stripe/crypto` (`apps/multiplatform/common/.../platform/Platform.kt:39`), `TODO [badges] this build pays via Stripe/crypto` (`apps/multiplatform/android/src/foss/java/chat/simplex/app/PlayStore.kt:13`), and related markers at `views/badges/BadgeStore.kt:130` and `BadgesPayView.kt:176`. |
| `BadgesRedeemCodeView` | Title-only stub, Kotlin and Swift. Its entry point already exists on both platforms (`BadgesSupportSimplexView.kt:77-82`, `BadgesSupportSimplexView.swift:108-126`). |

The original plan closed the gap with an in-app invoice flow (`getBadgeInvoice` producing a Stripe link or crypto address rendered inside the app). That needs three new screens per platform, in-app crypto payment UI, and payment state machines in core.

**This plan replaces it with a web checkout.** The badge service serves a small static site. A user picks tier, then duration, then payment method, pays by card (Stripe) or BTC/XMR (BTCPay), and receives a **redemption code** (the *code* hereafter). The code is pasted into the app, which sends `purchaseBadge {payment: {type: "code"}}` over service RPC; the service validates it, credits the ledger, and returns the signed credential.

Outcome: desktop and FOSS builds get a working purchase path, the app needs no payment code, and the Stripe and BTCPay surface lives in one process.

---

## 2. Settled decisions

Do not re-litigate these. If one proves wrong, record why in §9 and raise it.

1. **Hosting.** The service serves the site itself. One Warp listener: `/` and `/assets/*` static from embedded bytes, `/api/*` JSON, `/webhooks/*` provider callbacks. Same origin, no CORS.
2. **No `web-build` subcommand.** Assets are embedded with `file-embed`, and the committed `web/dist/` is the inspectable artefact. A development-only `web_dir` setting serves from disk instead.
3. **One subcommand only: `codes`.** Minting promo and compensation codes is a privileged offline database write. An admin HTTP endpoint would add authenticated attack surface to the public listener; a separate binary would duplicate the database options, migrations, and store layer. The service run stays the default with no subcommand, which requires wrapping the subparser in `optional`: `hsubparser` alone makes a subcommand mandatory, as it is in `Badges/CLI.hs:45`.
4. **Deployment configuration is an `ini` file, not CLI flags** (A6). Web, issuer, code-secret and provider settings live in `badge_service.ini`, following the simplexmq server pattern (`Data.Ini`, used by the SMP, XFTP and NTF servers). Database and run-mode options stay on the command line via `coreChatOptsP`, as for the other bots. Secrets are always file paths named from the ini, never inline values, so the ini can go into configuration management while the secrets do not.
5. **`getBadgeInvoice` stays defined but unimplemented**, returning `bad_request`. Non-store payment is web-only. Do not delete it from the schema or the Haskell types; it is the re-add path if in-app payment is ever wanted.
6. **Entry-point gating.** The tier and duration screens are shared on every platform. The payment-method screen exists only where more than one method is available, which is desktop and Android `foss`.
   - Desktop and Android `foss`: "Continue in browser", opening the prefilled site URL.
   - iOS and Android `google`: **the store purchase action is removed for the duration of this plan** (G0). Store evidence is not verified and a store purchase yields no badge (§6), so the button is removed rather than left to charge users for nothing. Those app wizards end at "Redeem code", and the tier and duration screens are informational until store-evidence verification ships, which is a later plan (§6).
   - **The redemption-code entry point is on every platform**, and already exists (§1).

   The store builds get the tier and duration screens but never link out, because Apple and Google reject apps that steer to external purchase for digital goods. Desktop and Android `foss` do link out. Those screens keep pricing from the store products until G4 and G5 make every platform price from `CRBadgeCatalog`.
7. **Front-end.** TypeScript compiled by `tsc` to ES modules, served as-is. No framework, no bundler. `tsc` cannot concatenate ES modules, so the output is one `.js` per source module and the browser resolves the imports. `web/dist/` is committed so the Haskell build never needs node; D8 fails CI when `dist/` does not match `src/`.
8. **Pricing has one source**, `BadgeService/Catalog.hs`, seeded into `badge_prices` and `badge_offers` at startup, with one total function used by every server-side caller. The site, the RPC catalog, and the app all read it from there.
9. **Codes are derived, not stored.** `code = crockford32(HMAC-SHA256(codeSecret, orderId))` truncated to 95 bits plus a check character (§3, B3), so `GET /api/order/:orderId` recomputes the code on a page reload while only `SHA256(code)` is ever at rest. Operator batch codes, which have no order, use random bytes with the same hash-only storage. See B3 and B8.

---

## 3. Architecture

```
                       ┌──── simplex-badge-service (one process) ─────────┐
  browser ──HTTPS─────▶│ web listener   /  /assets/*  /api/*  /webhooks/* │
                       │           │              │           │          │
Stripe / BTCPay ─POST─▶│           │         web_orders ◀──────┘          │
                       │           │              │                      │
                       │        Catalog.hs   codes (hash only)           │
                       │           │              │                      │
   app ──service RPC──▶│ bot  purchaseBadge{code} ─▶ Ledger ─▶ BBS sign  │
                       └─────────────────────────────────────────────────┘
```

**Components.** *The service* is the `simplex-badge-service` process. *The bot* is the SimpleX client inside it that receives `CEvtServiceRequest`. *The web listener* is the Warp application beside it. They share one database and one process.

**Terminology.** These words are used strictly:

- **Order** — a `web_orders` row: one browser transaction with a payment provider, identified by `orderId`.
- **Purchase** — a `badge_purchases` row: one badge identity in the service ledger, identified by `purchaseKey`. Unqualified, "purchase" is always this row. A *store purchase* is an App Store or Play transaction and a *web purchase* is an order paid on the site; neither is a `badge_purchases` row until a code is redeemed.
- **Purchase key** — generated by the client per redemption and never reused. A second code therefore always creates a second purchase; balances do not merge, which is why tier upgrades are out of scope (§6).
- **Per-user badge lock** — the `CLBadge UserId` entity lock C3 adds, which serialises one user's signed badge operations. It is an ordinary entity lock over the shared `entityLocks` map, so it inherits `withEntityLock`'s rule that `chatLock` is waited for first. C4 and C3 hold it while writing and release it before broadcasting.
- **Slot** — the badge a purchase occupies on a profile: `paid` for `supporter` and `legend`, `investor` for `investor`. It is derived from `current_badge_type`; no column stores it. At most one purchase per slot has status `issued` at a time, and a new purchase supersedes the previous one by moving it to `superseded` (`BadgePurchaseStatus`, `Badges/Types.hs:64`). This plan uses the `paid` slot only. There is no live flag or `idx_badge_purchases_live` in the shipped schema, whatever core §5 says; the status column is the whole mechanism.
- **Issuance** — one signed credential covering one **period**, a `[periodStart, periodEnd)` month debited from a purchase's balance (B2's `issue`).
- **Checkout** — the browser flow from the `#/checkout` summary screen through `POST /api/checkout` to the result screen. A single `POST /api/checkout` call is a *checkout request*. Stripe's `Checkout Session` is that provider's own object, written out in full on first use in a step and "the session" thereafter.
- **Wizard** — a one-question-per-screen flow, always qualified: the *app wizard* (tier, duration, method, in the client) or the *site wizard* (D2's shell).
- **Flavor** — an Android build flavor, `google` or `foss`, spelled as Gradle spells it.
- `BadgePrice` always names the protocol catalog row (`Badges/Service.hs`); the clients' fetch-state wrapper is `BadgePriceState` (G4, G5).

**Linkage between an order and a purchase.** They are joined only through the code, `code = crockford32(HMAC-SHA256(codeSecret, orderId))` truncated to 95 bits plus a check character (decision 9, B3), and `@codes` is keyed by `SHA256(code)`. No row stores both an `orderId` and a `purchaseKey`, so no *stored reference* links a browser session to a SimpleX profile. That is weaker than unlinkability: a settlement-time correlation between `codes.created_at` and `web_orders.settled_at`, helped by the literal `batch = 'web'` label, links the two without `codeSecret` at all (§9, B10). The cryptographic claim — an `orderId` cannot be turned into a code without the secret — is unaffected. A holder of `codeSecret` can derive the link at will, which puts the operator inside the trust boundary for this property; unlinkability from the operator is not claimed, and `codeSecret` is guarded accordingly (§7, H5).

**End to end:**

1. The app wizard collects tier, months and method, then opens `{badgeWebBaseUrl}/?tier=legend&months=12&pay=xmr` on desktop and Android `foss`. A user may also open the site directly.
2. The site loads `GET /api/catalog`, renders the remaining questions, then calls `POST /api/checkout {priceId, offerId?, method}`. Badge type and months are derived server-side from the ids; the browser never states them.
3. The service creates the provider invoice, writes an `@invoices` row and a `web_orders` row, and returns the order, its price, and either a `payUrl` or a crypto address. D6 is the normative response shape.
4. The browser redirects for card, or shows address, QR and countdown for crypto, polling `GET /api/order/:orderId`.
5. The provider webhook settles the invoice. The service derives the code, stores its hash in `codes`, and marks the order `paid`.
6. The poll returns `{status:"paid", code:"SXB-…"}`. The code is shown large and copyable, with a QR for transferring it to a phone.
7. The user pastes the code into the app. `APIPurchaseBadge` sends `purchaseBadge{payment:{type:"code",code}}`. The service verifies it, signs the credential, then in one transaction writes `credit(payment) +N`, `debit(badge) −1` and the redemption, and returns `badgeCredential {credential, receipt, statement}`.

The code is the only data crossing from the site to the app.

---

## 4. How to work this plan

Each step below is one reviewable commit.

1. Read §2 and the progress tracker.
2. **Take G0 first, before A1.** It has no dependencies, touches only client files, and until it lands the store builds charge users for a badge they will not receive (§7). It is listed under Phase G because its siblings are, not because it is scheduled there.
3. Then take the first step whose dependencies are all ticked. Do not skip ahead; later steps assume earlier files exist.
4. Run the step's **Verify** line before ticking it. A step that builds but whose verification was not run is not done.
5. Tick the tracker row in this file and commit it with the step's work. Follow the repository's own commit style, which `git log` shows: a subject line alone, `<area>: <subject>`, lowercase, no body. `core:` for the library and the service, `ui:` for Kotlin and Swift, `plan:` for this file, comma-separated when a step spans several.
6. If a step proves wrong or under-specified, correct this file in the same commit and record the change in §9.
7. A step that adds a service module adds it to **both** the `simplex-badge-service` and the `simplex-chat-test` `other-modules` lists in `simplex-chat.cabal`; a step that adds a module under `src/` adds it to the library stanza. The test stanza compiles service sources directly (`hs-source-dirs: … apps/simplex-badge-service/src`, `simplex-chat.cabal:706`) and lists them explicitly (`:676-678`), so a module missing there fails to link the tests. The step lists `simplex-chat.cabal` in its **Files**.
8. A step that adds a test module adds it to the `simplex-chat-test` `other-modules` **and** imports its spec into `tests/Test.hs`, under a path containing `Supporter badges` or `Badge service` so the test command below selects it. A spec that needs neither a running service nor a chat controller goes under `Supporter badges`, which runs in CI. A spec that starts the service goes under `SimpleX Badge service bot`, which CI skips. A spec that needs `testBracket`'s controller but not the service goes under `Supporter badges` in its own module registered inside that bracket, so CI still runs it. A module that compiles but is not in the hspec tree never runs, and its Verify line passes vacuously. Confirm the new spec name appears in the runner output.
9. A step that changes anything under `web/src` or `web/assets` runs the npm build and commits `web/dist/` in the same commit (decision 7, D8).
10. A Verify line that asserts an automated test names the test module, and the step lists that module in its **Files** and registers it per rules 7 and 8; an existing module it merely relies on is listed as unchanged, in parentheses. A Verify line with no test module is either a manual check and says so, or a deferral of the form "covered by *step*", allowed only when that later step depends on this one. A deferred step is ticked when it builds and the deferring step owns the assertions.

**Build and test commands:**

```bash
cabal build lib:simplex-chat exe:simplex-chat simplex-badge-service
cabal test --test-options='-m "Supporter badges" -m "Badge service"'
cd apps/simplex-badge-service/web && npm run build
```

The two `-m` filters are needed because the badge tests live under two hspec paths: `describe "Supporter badges" badgeTests` (`tests/Test.hs:66`) and `xdescribe'' "SimpleX Badge service bot" badgeServiceTests` (`:92`). `--match` is a case-sensitive `isInfixOf` over the full path and repeated filters are OR-ed, so any new badge spec must sit under one of those two paths or the command will silently skip it.

`xdescribe''` skips when `CI=true` (`ChatTests/Utils.hs:110`), so the badge service tests run locally and are skipped in CI, as for the broadcast and directory bots. Every service-side Verify line must therefore be run locally; CI green is not evidence.

### Progress tracker

| Step | Title | Deps | Status |
|---|---|---|---|
| A1 | Register the client badge migration, add `STRICT` | — | ☑ |
| A2 | JSON instances for badge protocol types, and the offer `total` | — | ☑ |
| A3 | Service schema: `web_orders`, `codes`, `provider_events` | — | ☑ |
| A4 | `Catalog.hs`: internal pricing, totals, seeding | A2, A5 | ☑ |
| A5 | Cabal dependencies for the service | — | ☑ |
| A6 | `badge_service.ini`: configuration file | A3, A4, A5 | ☑ |
| B1 | Store layer: purchases, ledger, issuances, codes, catalog | A2, A3, A4, A5 | ☑ |
| B2 | `Ledger.hs`: pure transitions and property tests | A5 | ☑ |
| B3 | `Codes.hs`: derive, encode, hash, classify | A5, A6, B1 | ☑ |
| B4 | Issuer key loading and credential signing | A5, A6, B2 | ☑ |
| B5 | RPC dispatcher: envelope, version, signer, throttle | A2, A6, B1 | ☑ |
| B6 | `getBadgeCatalog` | A4, B1, B2, B5 | ☑ |
| B7 | `purchaseBadge{code}` and `issueBadge` | B1, B2, B3, B4, B5 | ☑ |
| B8 | `codes` operator subcommand | A4, B1, B3 | ☑ |
| B9 | Service address publication | A6, B5 | ☑ |
| B10 | Service integration tests | B7, B8 | ☑ |
| C1 | `Store/Badges.hs`: client badge store | A1, A2 | ☑ |
| C2 | Commands, responses, events, parsers, View | A2, B2, C1 | ☑ |
| C3 | `BadgeManager` worker | C1, C2 | ☑ |
| C4 | Redeem path wired end to end | B6, B7, B9, C3 | ☑ |
| C5 | Client and service integration tests | B10, C4 | ☐ |
| D0 | Store layer: orders, invoices, provider events | A3, A5, B1 | ☐ |
| D1 | Web project skeleton and tsc build | — | ☐ |
| D2 | Design system and site wizard shell | D1 | ☐ |
| D3 | Catalog fetch and the four site screens | D2, D4 | ☐ |
| D4 | Warp listener, asset embedding, routing | A2, A4, A6, B1, D1 | ☐ |
| D5 | URL prefill | D3 | ☐ |
| D6 | `POST /api/checkout`, provider interface, order creation | A4, B3, D0, D4 | ☐ |
| D7 | Pay button and checkout error states | D3, D6 | ☐ |
| D8 | CI check for the committed web build | D1 | ☐ |
| E1 | Provider mock harness | A5 | ☐ |
| E2 | BTCPay client | A6, D6, E1 | ☐ |
| E3 | BTCPay webhook, settlement, code creation | B3, D0, D6, E2 | ☐ |
| E4 | `GET /api/order/:orderId` and the disclosure rule | E3 | ☐ |
| E5 | Order resume, crypto payment screen, QR | D5, D7, E4 | ☐ |
| E6 | Result screen | E5 | ☐ |
| E7 | BTCPay scenario tests | B7, E3, E4 | ☐ |
| F1 | Stripe client and Checkout Session | A6, D6, E1 | ☐ |
| F2 | Stripe webhook and signature verification | E3, F1 | ☐ |
| F3 | Card return-URL resume | E5, F2 | ☐ |
| F4 | Refunds and disputes | B7, F2 | ☐ |
| F5 | Stripe scenario tests | F2, F4 | ☐ |
| G0 | Remove the store purchase action | — | ☑ |
| G1 | Kotlin: payment-method screen and browser hand-off | C2, D5, G0 | ☐ |
| G2 | Kotlin: redeem view | B8, C4, G0 | ☐ |
| G3 | Swift: redeem view | B8, C4, G0 | ☐ |
| G4 | Kotlin: catalog pricing and badge-state refresh | C4, D3, G0, G1, G2 | ☐ |
| G5 | Swift: catalog pricing and badge-state refresh | C4, D3, G0, G3 | ☐ |
| G6 | Strings and stub cleanup | G0, G1, G2, G3, G4, G5 | ☐ |
| H1 | Rate limiting and request caps | D7, E4, F2 | ☐ |
| H2 | Code lifecycle tooling | B7, B8, E4 | ☐ |
| H3 | Stuck-order reconciliation | E3, F2 | ☐ |
| H4 | Logging and redaction | E4, F2, H3 | ☐ |
| H5 | Deployment and key-management docs | B4, B9, C4, E6, F5, H1, H2, H3, H4 | ☐ |
| H6 | Protocol docs update | D3, E4, F4, H1 | ☐ |

---

## 5. Steps

### Phase A — Foundations

Phase A ends with a service that starts from an ini file against a migrated schema with the catalog seeded, and a client schema that passes the schema-dump test. Nothing is queryable over RPC yet and no provider is contacted.

None of these steps touches a payment provider. A1, A2, A3 and A5 are independent; A4 needs A2 and A5, and A6 needs A3, A4 and A5.

#### A1 — Register the client badge migration, add `STRICT`

**Files:** `src/Simplex/Chat/Store/SQLite/Migrations.hs`, `src/Simplex/Chat/Store/Postgres/Migrations.hs`, `src/Simplex/Chat/Store/SQLite/Migrations/M20260731_user_badges.hs`, `src/Simplex/Chat/Store/SQLite/Migrations/chat_schema.sql`, `src/Simplex/Chat/Store/SQLite/Migrations/chat_lint.sql`, `src/Simplex/Chat/Store/Postgres/Migrations/chat_schema.sql`, `tests/SchemaDump.hs` (unchanged; the asserting module)

**Do:**

- The migration modules exist and are listed in `simplex-chat.cabal:160,331`, but neither migration list references them. Add the import after `M20260723_contact_request_rejection` (`Migrations.hs:171`) and append to `schemaMigrations`, whose current last entry is `("20260723_contact_request_rejection", …)`:

  ```haskell
  ("20260731_user_badges", m20260731_user_badges, Just down_m20260731_user_badges)
  ```

- Do the same in the Postgres list.
- Add `STRICT` to every `CREATE TABLE` in the **SQLite** `badgeSchemaTables` (10 tables, `M20260731_user_badges.hs:21-179`). `tests/SchemaDump.hs:105-108` asserts that no table in `sqlite_master` lacks it. `chat_schema.sql` currently carries a `STRICT` suffix on all 52 of its asserted tables: 48 ending `) STRICT;` and 4 ending `) WITHOUT ROWID, STRICT;` (lines 72, 124, 317, 343). `sqlite_sequence` (`:518`) is excluded by the test's predicate, and a naive `grep -c STRICT` returns 54 because two lines contain `ON DELETE RESTRICT` (`:39`, `:59`). Every column type in `badgeSchemaTables` is already `TEXT`, `INTEGER` or `BLOB`, all legal in a `STRICT` table, so no type changes are needed.
- **Do not add `STRICT` to the Postgres variant.** `STRICT` is a SQLite table option and is a syntax error in PostgreSQL. The Postgres badge schema (`Store/Postgres/Migrations/M20260731_user_badges.hs`) stays as it is.
- Regenerate the schema dumps.

**Verify:** `cabal test --test-options='-m "Schema dump"'` passes with the regenerated files committed. The hspec path is `"Schema dump"` (`tests/Test.hs:60`), not `SchemaDump`; a wrong filter matches zero tests and passes vacuously. The Postgres dump is a separate build and spec behind `#if defined(dbPostgres)`: run `cabal test --flags=client_postgres --test-options='-m "Postgres schema dump"'` (`tests/Test.hs:52-58`) and commit its regenerated `Store/Postgres/Migrations/chat_schema.sql` too.

#### A2 — JSON instances for badge protocol types, and the offer `total`

**Files:** `src/Simplex/Chat/Badges/Service.hs`, `src/Simplex/Chat/Badges/Types.hs`, `src/Simplex/Chat/PaymentService.hs`, `src/Simplex/Chat/PaymentService/Types.hs`, `tests/BadgeTests.hs`

**Do:** Add `ToJSON` and `FromJSON` for every type in those modules that lacks them, following conventions already in the codebase:

- enums: following the `$(JQ.deriveJSON (enumJSON $ dropPrefix "BS") ''BadgeStatus)` pattern at the bottom of `src/Simplex/Chat/Badges.hs`, with each instance placed in the module that declares its type
- records: `defaultJSON`
- tagged sums (`BadgeServiceCommand`, `BadgeServiceResponse`, `StatementEntryType`, `StatementCreditType`, `StatementDebitType`, `ServicePayment`, `ServicePaymentMethod`, `ServicePaymentDestination`, `OfferDiscount`): `taggedObjectJSON` with the discriminator `type`, matching the JTD `discriminator` in `docs/protocol/badges-rpc.schema.json`
- `SCUnknown` and `SDUnknown` (`Badges/Service.hs:167,177`) must round-trip an unrecognised tag verbatim (RPC §"Statement and balance": "An unknown type is stored as received and decoded after an app upgrade")
- `BadgeServiceErrorCode` already has both instances via `TextEncoding` (`Badges/Service.hs:198-246`) and `BSEUnknown` already round-trips an unknown tag. Leave it alone.
- **`BadgePurchaseStatus` (`Badges/Types.hs:64`) and `BadgePaymentStatus` (`:60`) have no instances at all**, not `TextEncoding`, `ToField`, `FromField` or JSON, so nothing in the repo defines how they are spelled in a column. Both are persisted by B1 and C1. Write the `TextEncoding` instances by hand, following `instance TextEncoding BadgeType` (`Badges.hs:91-101`), spelling `PSAcquiring | PSIssued | PSSuperseded | PSFailed` as `acquiring | issued | superseded | failed` and `BPSNew | BPSInvoiced | BPSPending | BPSSettled | BPSFailed | BPSExpired` as `new | invoiced | pending | settled | failed | expired`. Derive the rest from that single spelling: `ToJSON`/`FromJSON` through `textToJSON` and `textParseJSON` (`Badges.hs:103-108`), and `ToField`/`FromField` through `toField . textEncode` and `fromTextField_ textDecode` (`Badges.hs:339-341`). `enumJSON` is an Aeson options builder (`simplexmq Parsers.hs:102`) and cannot define a text codec, so it is not used for these two; it stays correct for the plain enums above. Every SQL literal B1 and C1 write comes from these instances.
- `Badges/Types.hs` cannot host any of this as it stands. It declares only `DerivingStrategies`, `DuplicateRecordFields` and `GeneralizedNewtypeDeriving` (`:1-3`), and its `import Simplex.Chat.Badges` is against an explicit export list (`Badges.hs:17-51`) that exports neither `TextEncoding` nor `textEncode`, `textDecode`, `textToJSON`, `textParseJSON` or `fromTextField_`. Add `CPP`, `LambdaCase`, `OverloadedStrings` and `TemplateHaskell` to the pragma block, `import Simplex.Messaging.Encoding.String`, `import Simplex.Messaging.Agent.Store.DB (fromTextField_)`, and the `#if defined(dbPostgres)` `ToField`/`FromField` import block copied verbatim from `Badges.hs:74-80`. There is no import cycle: `Badges/Types.hs` imports `Badges.hs` and not the reverse.
- Three record fields cannot decode their own rows, all in types marked "to review" or "unconfirmed draft", so correcting them is not a protocol change; record it in §9 when the step lands. `BadgePayment.paymentId` and `BadgeIssuance.issuanceId` are `Int64` (`Badges/Types.hs:117,177`) against `TEXT NOT NULL PRIMARY KEY` columns (`M20260731_user_badges.hs:43,167`): both become `Text`. `BadgePurchase.paymentId` is `Int64` (`:106`) against `payment_id TEXT REFERENCES @payments`, which is nullable (`M20260731_user_badges.hs:99`): it becomes `Maybe Text`. `DuplicateRecordFields` is already on (`Badges/Types.hs:2`), so the two `paymentId` fields may differ.
- `BadgeIssuance` also has no field for `badge_type TEXT NOT NULL` or `credential BLOB NOT NULL` (`M20260731_user_badges.hs:170,174`), and B1's `getIssuanceForRedeemedCode` exists precisely to return that credential. Add `badgeType :: BadgeType` and `credential :: BadgeCredential`, so B1 and C1 decode the whole row into one type rather than defining a second.
- `BadgeOffer` (`Badges/Service.hs:125-133`) gains one member, `total :: Maybe CurrencyAmount`. It is optional because the store layer's `getActiveCatalog` yields offers before `catalogTotals` fills them (A4); on the wire the service fills it on every response, so a client that decodes `Nothing` is talking to a service that does not implement this plan, and renders that offer as unavailable rather than computing a price. `BadgeOffer` only ever travels service to client, so there is no client-omission case, and the member is additive at protocol version 1: an older client ignores an unrecognised key. Both catalogs, B6's RPC response and D4's `/api/catalog`, then carry the same shape, so neither the site nor the app ever computes a price (decision 8). H6 adds it to the JTD schema.

**Verify:** Roundtrip tests in `tests/BadgeTests.hs` encoding one value of every constructor and re-decoding it, plus a test that decodes a `credit` entry with tag `"futureThing"` into `SCUnknown` and re-encodes it byte-identically. Field names must match `badges-rpc.schema.json` exactly; check by hand, there is no generator. `tests/BadgeTests.hs` is a plain `Spec` registered outside the `around` bracket (`tests/Test.hs:66`), so it holds tests that need no chat controller; such a test may still open its own temporary database.

#### A3 — Service schema: `web_orders`, `codes`, `provider_events`

**Files:** `apps/simplex-badge-service/src/BadgeService/Store/SQLite/Migrations.hs`, `apps/simplex-badge-service/src/BadgeService/Store/Postgres/Migrations.hs`, `tests/Bots/BadgeServiceTests.hs`

**Do:** Add migration `20260821_badge_service_web` to `schemaMigrations`, using the existing `withPrefix servicePrefix` mechanism (`Store/SQLite/Migrations.hs:24-38`; the same block is `Store/Postgres/Migrations.hs:23-37`). None of these tables exists: the draft in `plans/2026-07-31-badges-service-schema.sql` was never implemented, and the shipped migration only reuses the client `badgeSchema` plus `receipt_hash`.

The Postgres variant uses `Text` with `[r|…|]` in place of `Query` with `[sql|…|]`, and the Postgres column types of the shipped badge schema: `BYTEA` for `BLOB`, `TIMESTAMPTZ` for timestamp `TEXT` columns, `BIGINT` for `INTEGER` and for `redeemed_purchase_id` (matching `badge_purchases.badge_purchase_id BIGINT GENERATED ALWAYS AS IDENTITY`, `Store/Postgres/Migrations/M20260731_user_badges.hs:93`), and `SMALLINT` for `months`. **No `STRICT` suffix on the Postgres side** (A1).

```sql
CREATE TABLE @web_orders(
  order_id TEXT NOT NULL PRIMARY KEY,   -- 128-bit random, base64url; a bearer capability, see E4
  invoice_id TEXT REFERENCES @invoices, -- money side reuses the existing invoices table
  -- provider invoice / session / payment-intent id; order-side only, never a code or purchase
  -- reference. Unique across providers: a collision must fail loudly rather than resolve a
  -- charge to the wrong order.
  provider_ref TEXT,
  method TEXT NOT NULL CHECK (method IN ('card','btc','xmr')),
  short_ref TEXT NOT NULL,              -- 5 Crockford chars, generated per order (D6); the reference
                                        -- support resolves by, shown on card statements (F1) and on
                                        -- the crypto payment and result screens (E5, E6)
  badge_type TEXT NOT NULL CHECK (badge_type IN ('supporter','legend')),
  price_id TEXT REFERENCES @badge_prices,
  offer_id TEXT REFERENCES @badge_offers,
  months INTEGER NOT NULL,
  -- invoiced (created, unpaid) -> pending (partial or unconfirmed) -> paid (terminal).
  -- invoiced|pending -> expired|failed; both remain recoverable to paid (E3).
  status TEXT NOT NULL CHECK (status IN ('invoiced','pending','paid','expired','failed')),
  -- amount received, in minor units of the invoice currency, at the rate the provider locked
  amount_paid INTEGER,
  settled_at TEXT,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL
) STRICT;

CREATE INDEX @idx_web_orders_invoice ON @web_orders(invoice_id);

CREATE UNIQUE INDEX @idx_web_orders_provider_ref ON @web_orders(provider_ref);

CREATE UNIQUE INDEX @idx_web_orders_short_ref ON @web_orders(short_ref);

-- No order reference, and no code_hash on @web_orders: no row may hold both an order
-- reference and a purchase reference (§3 Linkage). An order's code row is found by
-- deriving the code from orderId and hashing it.
CREATE TABLE @codes(
  code_hash BLOB NOT NULL PRIMARY KEY,
  badge_type TEXT NOT NULL CHECK (badge_type IN ('supporter','legend')),
  months INTEGER NOT NULL CHECK (months > 0), -- lifetime codes are out of scope, see §6
  batch TEXT NOT NULL,                  -- 'web' for web orders, else an operator batch name
  expires_at TEXT NOT NULL,             -- redemption deadline -> code_expired
  redeemed_purchase_id INTEGER REFERENCES @badge_purchases,
  redeemed_at TEXT,
  unredeemed_at TEXT,                   -- set by B1's unredeemCode; reopens E4's window
  revoked_at TEXT,
  created_at TEXT NOT NULL
) STRICT;

CREATE INDEX @idx_codes_batch ON @codes(batch);

CREATE TABLE @provider_events(
  provider TEXT NOT NULL,
  event_id TEXT NOT NULL,
  received_at TEXT NOT NULL,
  processed_at TEXT,                    -- NULL means the previous attempt did not complete (E3)
  PRIMARY KEY(provider, event_id)
) STRICT;
```

Reuse `@invoices` (`M20260731_user_badges.hs:24-40`) for provider, price, amount, currency, `payment_url`, `payment_address`, `payment_crypto_amount`, `expires_at` and `status`. Do not add a parallel money table. `@invoices` has no `provider_ref`, which is why `@web_orders` carries one. `@invoices.payment_crypto_currency` (`M20260731_user_badges.hs:34`) is deliberately left NULL: `@web_orders.method` is the single source and E4 derives the currency from it.

`@web_orders.status` is authoritative for the order lifecycle and drives E3, E4 and H3. `@invoices.status` is maintained in step with it in the same transaction and is read by nothing in this plan.

**Verify:** A migration up-and-down test in `tests/Bots/BadgeServiceTests.hs`; the service starts and the tables exist. The store module is selected per build, so one run covers one backend: run it once on the default build and once as `cabal test --flags=client_postgres --test-options='-m "Badge service"'`, as A1 does for the schema dumps.

#### A4 — `Catalog.hs`: internal pricing, totals, seeding

**Files:** `apps/simplex-badge-service/src/BadgeService/Catalog.hs`, `apps/simplex-badge-service/src/BadgeService/Service.hs`, `simplex-chat.cabal`, `tests/Bots/BadgeServiceTests.hs`

**Do:** The single source of pricing (decision 8). Define the default catalog as Haskell values reusing `BadgePrice` and `BadgeOffer` from `Badges/Service.hs`; do not define parallel record types.

- Prices, in minor units of `usd`: supporter `monthPrice = CurrencyAmount 700`, legend `monthPrice = CurrencyAmount 7000`, from UX §1 ("Supporter $7/month (2GB) vs Legend $70/month (5GB)"). `currency = "usd"`, `status = BISActive`. `BadgePriceId` UUIDs are written as literals so re-seeding is idempotent. `CurrencyAmount` is an integer count of minor units; no floating point appears anywhere in the pricing path.
- Offers are **pinned to a price**: four rows, one per badge type and duration (supporter 3 and 12 months, legend 3 and 12 months), each with `priceId` set and a literal UUID. An unpinned offer is never seeded, because `total` is undefined without a price. Per UX §6.12's 1x / 2x / 6x monthly pricing: 3 months as `ODFreeMonths 1`, giving `(3 − 1) × monthPrice = 2× monthly`, and 12 months as `ODFreeMonths 6`, giving `(12 − 6) × monthPrice = 6× monthly`. **`ODDiscount` cannot express the 3-month offer**: it needs 33.33%, and `OfferDiscount.discount` is a `Word8` percent (`Badges/Types.hs:54-57`). At `monthPrice = 700` the neighbours are `ODDiscount 33` → 1407 and `ODDiscount 34` → 1386, neither of which is 1400. One month has no offer and is priced at `monthPrice` (core §4).
- `offerTotal :: BadgePrice -> Maybe BadgeOffer -> CurrencyAmount` is the single total function, computing over the unwrapped `Word32` and re-wrapping the result; `CurrencyAmount` has no `Num` instance (`PaymentService/Types.hs:22`). `Nothing` means exactly one month and returns `monthPrice`; a longer duration is expressible only as an offer, so there is no unpriced multi-month path. `freeMonths → (months − freeMonths) × monthPrice`; `discount → floor (months × monthPrice × (100 − discount) / 100)`. It is the only place a total is computed.
- `catalogTotals :: BadgeCatalog -> BadgeCatalog` fills each offer's `total` (A2) with `offerTotal` applied to that offer's pinned price. Every path leaving the service applies it: B6's RPC response and D4's `/api/catalog`. No caller outside this module computes a total, so the site, the app and the charge cannot drift. `total` is response-only: it is not a column of `@badge_offers`, `seedCatalog` writes none, and `getActiveCatalog` returns every offer with `total = Nothing`. `catalogTotals` overwrites unconditionally and is therefore idempotent. It is a total function, relying on B1's invariant that every returned offer's pinned price is also returned.
- `seedCatalog :: DBStore -> IO ()` inserts if absent, by id, and never updates or deletes an existing row. Repricing appends a new price and deprecates the old (UX §3); deprecating is B1's `setPriceStatus`, never a seed edit.
- Call `seedCatalog` once at service startup, after migrations and before the bot starts. D4 places the web listener after it. B8's subcommand also calls it, so operator tooling sees the same catalog.

**Verify:** In `tests/Bots/BadgeServiceTests.hs`: seed twice on a fresh database and assert row counts are unchanged and that a price deprecated by direct SQL, B1's `setPriceStatus` not existing yet, stays deprecated. Assert `offerTotal` prices 3 months at exactly `2 × monthPrice` and 12 months at exactly `6 × monthPrice`, and that `catalogTotals` fills all four seeded offers.

#### A5 — Cabal dependencies for the service

**Files:** `simplex-chat.cabal`

**Do:**

- Add to the `simplex-badge-service` stanza: `wai`, `warp`, `http-types`, `http-client`, `http-client-tls`, `uuid`, `base64-bytestring`, `crypton`, `memory`, `file-embed`, `ini`, `time`, `containers`, `case-insensitive`. `text` is already present in both `impl(ghc …)` branches (`simplex-chat.cabal:444-449`); do not add it twice.
- Add to the `simplex-chat-test` stanza only `http-client`, `http-client-tls`, `uuid`, `file-embed`, `ini`, `case-insensitive`. The rest are already there (`:713-772`).
- Pin `warp ==3.3.*`, matching the test stanza and simplexmq's `warp ==3.3.30`. Pin `ini ==0.4.1`, matching simplexmq.
- simplexmq declares `case-insensitive`, `http-client`, `http-client-tls`, `ini ==0.4.1` and `warp ==3.3.30` inside its own `if !flag(client_library)` block. Cabal `build-depends` are not transitive, so they must be declared locally regardless of MVP §4's claim that `http-client-tls` "is available through simplexmq". simplexmq line numbers are given against the checkout beside this repo, which is not the `cabal.project:24` pin; treat every simplexmq citation in this plan as indicative.
- Each later step adds its own modules to `other-modules` in its own commit, in both stanzas (§4 rule 7); that is part of that step, not of A5.

**Verify:** `cabal build --dry-run simplex-badge-service` produces a build plan listing every added package. Manual check; if a scratch module is used to force the imports, delete it before committing.

#### A6 — `badge_service.ini`: configuration file

**Files:** `apps/simplex-badge-service/src/BadgeService/Config.hs`, `apps/simplex-badge-service/src/BadgeService/Options.hs`, `apps/simplex-badge-service/Main.hs`, `simplex-chat.cabal`, `tests/Bots/BadgeServiceTests.hs`

**Do:** Deployment configuration is an `ini` file (decision 4), read with `Data.Ini`'s `readIniFile`, `lookupValue`, `sections` and `keys`, following the simplexmq server pattern (`Simplex.Messaging.Server.Main`, `Server.CLI`; see A5 on simplexmq line numbers). Ten provider and secret keys on a command line are unmanageable, and this is the established convention for a deployed service in this codebase.

- `--config FILE` on the CLI, defaulting to `badge_service.ini` in the SimpleX application data directory (`getAppUserDataDirectory "simplex"`, `Service.hs:47`), the same `appDir` that supplies `-d`'s default. `-d` is never read back to derive it, so overriding `-d` does not move the config file. Database and run-mode options stay on the CLI via `coreChatOptsP`; they are process options, not deployment configuration.
- `Config.hs` defines the record, the parser and startup validation.
- `Config.hs` also defines `BadgeServiceEnv`, the single runtime value every handler receives, and `newBadgeServiceEnv`, which builds it once at startup after migrations and `seedCatalog`. A6 defines only the parsed config, the `DBStore` and `now`. Each later step adds the field it owns to the record and to `newBadgeServiceEnv`, listing `Config.hs` in its own Files: B3 the decoded code secret, B4 the issuer key and index, B5 the three RPC token buckets (per-signer, global failure, and the unsigned-catalog bucket), E2 the BTCPay client and webhook secret, F1 the Stripe key and webhook secret, H1 the IP buckets. D4's and E4's `Config.hs` edits add validation and a constant, not env fields. Startup fails if the issuer key (B4) or the code secret (B3) cannot be read, in the step that adds the field. The bot handlers (B5), the web listener (D4) and the reconciliation pass (H3) all take it.
- `BadgeServiceEnv` carries `now :: IO UTCTime`, defaulting to `getCurrentTime`. Every service component reads the clock through it and none calls `getCurrentTime` directly, so a test can advance service time without sleeping. The steps whose tests override it are B10 and C5.
- Secrets are always **file paths named from the ini** (decision 4). Every secret file other than `[issuer] key_file` and `[codes] secret_file` is read as one line of UTF-8, trailing whitespace stripped, and used verbatim as the provider credential in the form that provider's dashboard displays. `[codes] secret_file` is base64-decoded (B3); `[issuer] key_file` is the two-line `badge keygen` output (B4).
- Required and optional sections:
  - `[issuer]` and `[codes]` are **required**. Without them the service can neither sign a credential nor derive a code, so it refuses to start.
  - `[web]` is required whenever a provider is configured.
  - `[btcpay]` is all-or-nothing for its four connection keys, `url`, `store_id`, `api_key_file` and `webhook_secret_file`: all four or none. `xmr_method_id`, `btc_expiry_minutes` and `xmr_expiry_minutes` are optional and default as shown, so a deployment whose BTCPay instance names the Monero method differently, or which needs a longer Monero window, changes the ini rather than the code. `[stripe]` is all-or-nothing: both keys or none.
  - Inside `[web]`, `port`, `base_url` and `support_contact` are required as a group: the listener does not start without a port, D4 cannot build absolute URLs without `base_url`, and D2's footer has no fallback. `host` defaults to `127.0.0.1`, `behind_proxy` to `off`, and `web_dir` is absent in production. Omitting `[web]` entirely starts the bot alone.
  - `[service]` and `[reconcile]` are optional.
  - An unknown key in a known section is an error, checked with `Data.Ini`'s `keys`, so a typo cannot silently disable a provider.
- A method whose provider section is absent is rejected at `POST /api/checkout` with `provider_unavailable` (D6).
- `withBadgeService` (`tests/Bots/BadgeServiceTests.hs:51`) writes a temporary `badge_service.ini` into the test directory alongside a generated issuer key file and a 32-byte code secret, and sets the config-file field of `BadgeServiceOpts` to its path. That field is built as a record at `BadgeServiceTests.hs:32` and never parsed from a command line; `--config` populates the same field in production. It also accepts a `now` override, used by B10 and C5, and the three RPC bucket sizes with their starting token counts, used by B5, B6 and B10. D4 adds the `[web]` section and a free port to it, and E2 and F1 add their provider sections. Without this, every test that starts the service fails from the moment this step's required-section validation lands, including A3's and A4's. Provider sections are omitted until E2 and F1 add them.

```ini
[service]
address_file = /var/lib/simplex-badge-service/address.link   # B9, optional

[web]
port = 8080
host = 127.0.0.1                     # default; bind loopback behind a reverse proxy
base_url = https://badges.simplex.chat
support_contact = https://simplex.chat/contact        # D2 footer, the site's only contact channel
behind_proxy = off                   # H1: trust X-Forwarded-For
# web_dir = ./apps/simplex-badge-service/web   # development only (decision 2)

[issuer]
key_file = /etc/simplex/badge-issuer.keys
key_idx = 1

[codes]
secret_file = /etc/simplex/badge-code.secret
default_expiry_days = 365            # B8, E3

[btcpay]
url = https://btcpay.example.org
store_id = ...
api_key_file = /etc/simplex/btcpay.key
webhook_secret_file = /etc/simplex/btcpay.hmac
xmr_method_id = XMR-CHAIN            # Greenfield payment-method id for Monero
btc_expiry_minutes = 15              # BTCPay's own default
xmr_expiry_minutes = 60              # XMR confirmations are slower than BTC

[stripe]
secret_key_file = /etc/simplex/stripe.key
webhook_secret_file = /etc/simplex/stripe.hmac

[reconcile]
interval_seconds = 600               # H3
```

The keys above are the complete set this plan needs. A step that finds it needs another adds it here and in `Config.hs` rather than inventing a flag, and records the addition in §9.

**Verify:** In `tests/Bots/BadgeServiceTests.hs`: a missing config file, an unparsable one, an unknown key, a missing `[issuer]` or `[codes]` section, and a half-configured provider section each fail at startup with a message naming the file, and naming the offending key where there is one. A complete file starts the service, and so does an ini with `[issuer]` and `[codes]` and no provider section; D6 asserts that every checkout method then returns `provider_unavailable`.

---

### Phase B — Service: code to credential over RPC

The first end-to-end path. No payments yet; codes come from B8's operator tooling.

#### B1 — Store layer: purchases, ledger, issuances, codes, catalog

**Files:** `apps/simplex-badge-service/src/BadgeService/Store.hs`, `Store/SQLite.hs` and `Store/Postgres.hs` only where the SQL genuinely differs, `simplex-chat.cabal`, `tests/Bots/BadgeServiceTests.hs`

**Do:** Queries over the badge tables, structured after `apps/simplex-directory-service/src/Directory/Store*`. The order, invoice and provider-event functions are D0's; this step owns everything the RPC path needs.

- `Store.hs` defines `data ServiceError`, the error type of every store function, covering not-found, conflict and decode failures.
- **Transaction discipline.** Every function takes a `DB.Connection` and opens no transaction of its own. `withServiceTransaction :: DBStore -> (DB.Connection -> ExceptT ServiceError IO a) -> IO (Either ServiceError a)` is the only place a transaction is opened, and a `Left` rolls back. Command handlers call it once. This is what makes one transaction per command achievable: functions that open their own transactions cannot be composed into one.
- Purchases and payments: `getPurchaseByKey`; `createPurchase`, writing the row with status `issued`, both badge-type columns set, `purchase_key` from the request's signer key and `master_key` from the badge master key in the verified badge request, all four columns being `NOT NULL` in the shared `badgeSchema` (`M20260731_user_badges.hs:95-98`), since the service has no pre-response row either; `createCodePayment`, writing the `payments` row with a caller-minted UUID as `payment_id` (`payments.payment_id` is `TEXT NOT NULL PRIMARY KEY` with no default, `M20260731_user_badges.hs:43`), `provider = 'code'`, `invoice_id` NULL and `status = 'settled'`, and pointing the purchase row's `payment_id` at it. Statuses go through the `ToField` instances A2 adds for `BadgePurchaseStatus` (`Badges/Types.hs:64`) and `BadgePaymentStatus` (`:60`), which is the same vocabulary the client uses (C1), because both sides share `badgeSchema`.
- Ledger: `getLastLedgerEntry`, `appendLedgerEntry`, `getLedgerSince`.
- Issuances: `getIssuanceForPeriod`; `getIssuanceForRedeemedCode` (code hash → `redeemed_purchase_id` → the issuance whose period contains `redeemed_at`; B7's replay path needs this, since a purchase may have several issuances by then); `createIssuance`.
- Codes: `getCodeByHash`, returning the code row **joined to `badge_purchases`** so the caller sees the `purchase_key` behind `redeemed_purchase_id` and can distinguish a replay from another key's use; `markCodeRedeemed`; `unredeemCode`, clearing `redeemed_purchase_id` and `redeemed_at` and setting `unredeemed_at = now`, which both re-enables redemption and reopens E4's disclosure window; `insertCodes`; `revokeCode`; `revokeBatch`, setting `revoked_at` on every unrevoked code of a batch through `@idx_codes_batch` (A3), which is what B8's `codes revoke --batch` calls.
- Catalog: `getActiveCatalog` (prices and offers with status `active` or `deprecated`), which omits any offer whose `price_id` is NULL or whose pinned price is not itself returned, so every offer in the result has a resolvable price (A4's `catalogTotals` depends on this); `getPriceById`; `getOfferById`; and `setPriceStatus` / `setOfferStatus`, the only writers of a catalog status in production code. Operators use them to deprecate a price on repricing (UX §3) and tests use them to produce `deprecated` and `disabled` rows, which `seedCatalog` cannot.

There is no store function that resolves an order to a code or a purchase. That join does not exist in the schema (§3 Linkage); callers that need it derive the code from `orderId` and look it up by hash.

**Verify:** In `tests/Bots/BadgeServiceTests.hs`: create a purchase, append entries, read them back in order; `setPriceStatus` to `disabled` makes a price absent from `getActiveCatalog`, removes every offer pinned to it from the same result, and leaves both reachable via `getPriceById` and `getOfferById`; `getCodeByHash` returns the redeeming purchase key; `unredeemCode` clears both redemption columns and sets `unredeemed_at`.

#### B2 — `Ledger.hs`: pure transitions and property tests

**Files:** `src/Simplex/Chat/Badges/Months.hs`, `apps/simplex-badge-service/src/BadgeService/Ledger.hs`, `tests/Bots/BadgeLedgerTests.hs`, `simplex-chat.cabal`, `tests/Test.hs`

**Do:** Pure, database-free functions over the ledger state, so they can be tested exhaustively.

**Vocabulary mapping.** UX §3 uses one set of names and the shipped types use another. They are the same thing:

| UX §3 | Implemented (`StatementEntryType`) | DB columns |
|---|---|---|
| `grant(payment)` | `SECredit (SCPayment {invoiceId})` | `entry_type='credit'`, `entry_credit_type='payment'` |
| `grant(charge)` | `SECredit (SCCharge {chargeId})` | `…'charge'` |
| `grant(goodwill)` | `SECredit SCSupport` | `…'support'` |
| `consume` | `SEDebit SDBadge` | `entry_type='debit'`, `entry_debit_type='badge'` |
| `lapse` | `SEDebit SDLapse` | `…'lapse'` |
| `debit(refund)` | `SEDebit SDRefund` | `…'refund'` |
| `debit(conversion)` | `SEDebit (SDUpgrade {toPurchaseKey})` | `…'upgrade'` |
| `delta` / `months` / `start` | `changeMonths` / `balanceMonths` / `balanceStartTs` | `change_months` / `balance_months` / `balance_start_ts` |

This plan's prose names an entry `entry_type(entry_credit_type|entry_debit_type)`, so `credit(payment)`, `debit(badge)`, `debit(lapse)` and `debit(refund)` are the middle column's rows written in that form.

```haskell
data LedgerState = LedgerState {balanceMonths :: Int, balanceStartTs :: UTCTime, balanceBadgeType :: BadgeType}

addMonths         :: Int -> UTCTime -> UTCTime
fullMonthsBetween :: UTCTime -> UTCTime -> Int     -- largest m >= 0 with addMonths m start <= t
sundayAfter       :: UTCTime -> UTCTime

advance  :: UTCTime -> LedgerState -> Maybe (Int, LedgerState)
credit   :: UTCTime -> Int -> StatementCreditType -> LedgerState -> LedgerState
debitAll :: StatementDebitType -> LedgerState -> LedgerState
issue    :: UTCTime -> LedgerState -> Maybe (LedgerState, UTCTime, UTCTime)  -- state, periodStart, periodEnd

initialLedgerState :: UTCTime -> BadgeType -> LedgerState   -- zero balance, balanceStartTs = the given time
```

Semantics are verbatim from UX §3 "Transitions". `advance` runs before every issue, credit and debit. Boundary rules, all load-bearing and none inferable from UX §3:

- `addMonths` clamps to the last valid day of the target month, so 31 January plus one month is 28 or 29 February, and preserves the time of day.
- `sundayAfter t` returns 23:59:59 UTC of the next Sunday strictly after `t`. A `t` that already falls on a Sunday therefore yields the following Sunday, giving every badge at least one full week of validity.
- `advance` returns `Nothing` when no whole month has elapsed since `balanceStartTs`, leaving the state unchanged. It never returns `Just (0, _)`.
- `advance t` appends **one** `debit(lapse)` row, verbatim from UX §3: `k = min balanceMonths (fullMonthsBetween balanceStartTs t)`, and if `k > 0` the entry is `changeMonths = -k`, `balanceMonths' = balanceMonths - k`, `balanceStartTs' = addMonths k balanceStartTs`. The `min` is load-bearing: without it a year of absence on a zero balance would lapse twelve months and drive the balance negative, breaking property 2. The new `balanceStartTs` is the entry's `balance_start_ts`, not its creation time; the entry is created at `t`, which is what `service_created_at` and `created_at` carry. Callers therefore write one row and no ordering question arises.
- `issue` returns `Nothing` when `balanceMonths == 0` or the current month is already issued. The caller distinguishes the two by the balance, since the two cases have different responses (B7 step 4).
- `initialLedgerState` is the state of a purchase with no ledger entry: `balanceMonths = 0`, `balanceStartTs` the given time, `balanceBadgeType` the type the caller is crediting. Every transition takes a `LedgerState`, and a purchase created in the same transaction has none to read, so this is where its first credit starts.

`addMonths`, `fullMonthsBetween` and `sundayAfter` go in `src/Simplex/Chat/Badges/Months.hs`, in the library, not in the service module: C2 needs `addMonths` to render the paid-through date and the clamping rule must have one implementation. `BadgeService.Ledger` imports them and holds `LedgerState` and the transitions. Add `Simplex.Chat.Badges.Months` to the library `exposed-modules` and register `BadgeService.Ledger` in both `other-modules` lists and `Bots.BadgeLedgerTests` in the test stanza, with its spec in `tests/Test.hs` under the **`Supporter badges`** path, not under `SimpleX Badge service bot`: these tests are pure and must run in CI (§4 rules 7 and 8).

**Verify:** QuickCheck properties in `tests/Bots/BadgeLedgerTests.hs`, over generated histories:

1. every state equals the transition applied to its predecessor (UX §3 property 1)
2. `balanceMonths >= 0`; the sum of `changeMonths` equals `balanceMonths`; `balanceStartTs` is non-decreasing (UX §3 property 2)
3. `issue` debits exactly one month and yields one period, 1:1 with issuances (UX §3 property 3)
4. re-running `issue` inside an already-issued period appends nothing (UX §3 property 4)
5. `advance` debits only fully elapsed unissued months (MVP §6)
6. the worked example in UX §3 (buy 3 months Tue Mar 10, app off Apr 5 to May 20, issue May 20) reproduces that table's four rows exactly, as an explicit unit test
7. `sundayAfter` applied to a `periodEnd` that already falls on a Sunday returns the following Sunday at 23:59:59 UTC

#### B3 — `Codes.hs`: derive, encode, hash, classify

**Files:** `apps/simplex-badge-service/src/BadgeService/Codes.hs`, `apps/simplex-badge-service/src/BadgeService/Config.hs`, `tests/Bots/BadgeCodeTests.hs`, `simplex-chat.cabal`, `tests/Test.hs`

**Registration:** `BadgeService.Codes` in both `other-modules` lists; `Bots.BadgeCodeTests` in the test stanza with its spec under the **`Supporter badges`** path rather than `SimpleX Badge service bot`, since these tests are pure and must run in CI (§4 rules 7 and 8).

**Do:** `Codes.hs` is database-free. The lookup and the writes live in B7; this module supplies the pure parts.

- **Format.** `SXB-XXXXX-XXXXX-XXXXX-XXXXX`: 20 characters from the Crockford base32 alphabet, which excludes `I`, `L`, `O` and `U`. 19 data characters carry exactly 95 bits; the 20th is a check character `c = (Σ v(xᵢ)) mod 32` over the 19 data values, encoded in the same alphabet. This is deliberately **not** Crockford's own mod-37 check symbol, which needs five symbols outside the alphabet. An unweighted sum mod 32 detects every single-character substitution but not transpositions, which is accepted.
- **Order codes:** `deriveOrderCode`, `code = encode (HMAC_SHA256 codeSecret orderId)` truncated to 95 bits plus the check character. Recomputable from `orderId`, so a browser reload is answerable with no plaintext code at rest.
- `codeSecret` is read from the path in `[codes] secret_file` (A6): standard base64 on one line, trailing whitespace stripped, decoding to at least 32 bytes, rejected at startup otherwise. It is long-lived and must be backed up with the issuer key. Rotating it breaks recomputation for past orders; record that in H5.
- **Batch codes:** `generateBatchCode`, random bytes from `C.randomBytes`, same encoding. Printed once by B8 and never recoverable.
- `normalizeCode`: uppercase, strip `-` and whitespace, then strip a leading `SXB` **only when the remaining string is 20 characters long**: `S`, `X` and `B` are themselves valid data characters, so the prefix is otherwise indistinguishable from payload. Then fold `I` and `L` to `1` and `O` to `0`. A code is accepted with or without the prefix.
- `codeHash` (SHA-256), `verifyChecksum`.
- Classification, taking the row that B1's `getCodeByHash` returns together with the requesting purchase key:

  ```haskell
  data RedeemOutcome
    = RedeemOk BadgeType Int
    | RedeemInvalid
    | RedeemRevoked
    | RedeemUsedByOther
    | RedeemAlreadyRedeemedBySameKey Int64
    | RedeemExpired
  ```

  Mapping: `RedeemInvalid` and `RedeemRevoked` both to `code_invalid`, so a revoked code is indistinguishable from an unknown one on the wire and a guesser learns nothing from a revocation; support distinguishes them with `codes status` (H2). `RedeemUsedByOther` to `code_used`; `RedeemExpired` to `code_expired`. `RedeemAlreadyRedeemedBySameKey pid` is **not** an error: B7 returns the credential cached against purchase `pid`. `RedeemOk` proceeds.
- A code failing the checksum yields `RedeemInvalid` with no database lookup, so guessing attempts that fail the checksum cost no I/O; 31 of every 32 random guesses fail it.

**Verify:** Unit tests in `tests/Bots/BadgeCodeTests.hs`: encoding is deterministic and `normalizeCode` inverts the display formatting, so `normalizeCode (deriveOrderCode secret o)` is stable across calls; every single-character substitution is caught by the check character; `normalizeCode` maps `sxb 1o0i…`, `SXB-10O1…` and `10O1…` to the same value, and does not truncate a bare 20-character code whose first three characters are `SXB`; `deriveOrderCode` is deterministic for a given secret and `orderId`, and differs across `orderId`s; a code failing the checksum classifies as `RedeemInvalid` with no database lookup. The rest of the `RedeemOutcome` mapping is covered by B10, which drives every outcome through `purchaseBadge`.

#### B4 — Issuer key loading and credential signing

**Files:** `apps/simplex-badge-service/src/BadgeService/Credentials.hs`, `apps/simplex-badge-service/src/BadgeService/Config.hs`, `simplex-chat.cabal`, `tests/Bots/BadgeServiceTests.hs`

**Do:**

- Read `[issuer] key_file` and `[issuer] key_idx` (A6). Load the BBS secret key at startup and fail fast with a clear message if it is absent or malformed. `simplex-chat badge keygen` (`Badges/CLI.hs:67`) prints two labelled lines, `secret <base64url>` and `public <base64url>`, so the loader parses that format rather than a bare key.
- `key_idx` is the issuer key index passed as `issueBadge`'s first argument and embedded in the credential so a verifier can select the right public key. It defaults to 1 and must be a positive `Int`.
- Sign via the existing `Simplex.Chat.Badges.issueBadge :: Int -> BBSSecretKey -> VerifiedBadgeRequest -> IO (Either String BadgeCredential)`. Do not reimplement BBS.
- The badge expiry is `sundayAfter periodEnd`, where `sundayAfter` comes from B2's `Simplex.Chat.Badges.Months` and `periodEnd` from `issue` in B2's `Ledger.hs`. Because `issue` never yields a period beyond the funded balance, no further cap applies.
- Reject a `badgeRequest` with non-empty `badgeExtra`, which `issueBadge` already does, and surface it as `bad_request`.

**Verify:** In `tests/Bots/BadgeServiceTests.hs`: sign a credential with a test key and verify it with `verifyCredential` and the matching public key; assert `badgeExpiry` falls on a Sunday at 23:59:59 UTC, and that a `periodEnd` already on a Sunday expires on the following Sunday.

#### B5 — RPC dispatcher: envelope, version, signer, throttle

**Files:** `apps/simplex-badge-service/src/BadgeService/Service.hs`, `apps/simplex-badge-service/src/BadgeService/Config.hs`, `src/Simplex/Chat/Badges/Service.hs`, `tests/Bots/BadgeServiceTests.hs`

**Do:** Replace `handleServiceRequest` (`Service.hs:113-120`, currently a hardcoded `unsupported_version`) with the dispatcher below.

1. Decode `requestData :: J.Object` into `BadgeServiceRequest`. A decode failure is `bad_request`.
2. Version gate. Define `minSupportedBadgeVersion = 1` and `currentBadgeVersion = 1` in `src/Simplex/Chat/Badges/Service.hs`. A request with `version < minSupportedBadgeVersion` gets `unsupported_version`; the service answers within `min(request.version, currentBadgeVersion)` (`badges-rpc.md:9`). The response envelope carries no version field, since neither `BadgeServiceResponse` nor the JTD `response` definition has one, so nothing is echoed on the wire. There is no version-conditional field behaviour at version 1; the gate exists so that later versions have one. `BSERateLimited` (`Badges/Service.hs:191`, tag `rate_limited` at `:213`) and `BSPError.retryAfter :: Maybe Word32` (`:106`) already exist; use them. Only the two version constants are new.
3. Signer check, closing the `TODO` at `Service.hs:67`. `CEvtServiceRequest`'s `signerKey` must equal `purchaseKey`; a mismatch is `bad_request`. `getBadgeCatalog` may arrive unsigned; signed, it is treated like every other command, so an unknown key gets `unknown_purchase_key` (B6). **`purchaseBadge` requires a signature but not a pre-existing record**: an unknown key is the normal first-purchase case, because B7 is what creates the record. Every other command requires both a signature and an existing record, and a key with no record gets `unknown_purchase_key` (RPC §Identity).
4. Dispatch on the `BadgeServiceCommand` constructor. `BSCGetBadgeInvoice`, `BSCUpgradeBadgeSubscription` and `BSCPauseBadge` return `bad_request` (decision 5 for `getBadgeInvoice`, §6 for the other two). `getBadgeCatalog` goes to B6, and `issueBadge` and `purchaseBadge` to B7; until those steps land each returns `internal` with the message `not implemented`. `purchaseBadge` dispatches further on the `ServicePayment` constructor: `SPCode` goes to B7, and `SPApple`, `SPGoogle`, `SPInvoice` and `SPReceipt` return `bad_request`, because store-evidence verification and receipt transfer are out of scope (§6).
5. Throttle, keyed on the request's `signerKey`, plus a service-wide budget. A purchase key is self-asserted and cheap to mint, so the per-key limit of 10 failed redemptions per hour shapes an honest client's retries and nothing more. **The global budget is the control against a distributed guesser, and the code's 95 bits of entropy is the load-bearing defence.** Service RPC carries no IP, so H1's per-IP limits do not apply here.

   A *failed redemption* is a `purchaseBadge{code}` returning `code_invalid`, `code_used` or `code_expired`, including a checksum rejection that never reaches the database. Every `purchaseBadge{code}` is checked against both buckets **before** processing, so an empty bucket rejects a request whose code would have been valid; only a failed redemption debits a token. Successes and the same-key replay path debit nothing, so an honest client is never throttled by its own traffic. Both limits are in-memory token buckets held in A6's `BadgeServiceEnv`, swept every 5 minutes and forgotten on restart, as for H1's IP buckets. The per-signer bucket refills at 10/hour with burst 10 and the global failure bucket at 600/hour with burst 600. An unsigned `getBadgeCatalog` has no signer to key on and no failure budget applies to it; it is bounded by a third bucket, a global catalog bucket at 600/hour with burst 600, in the same `BadgeServiceEnv` and on the same sweep. All three bucket sizes, and their starting token counts, are overridable at startup, so B5, B6 and B10 can each set a small budget or a pre-drained bucket. `retryAfter` is the seconds until one token is available.
6. Catch-all: any exception becomes `internal`, logged with the request id, never leaking internals into the response `message`.

Keep the existing `sendChatCmd cc (APISendServiceResponse …)` reply path.

**Verify:** In `tests/Bots/BadgeServiceTests.hs`: malformed JSON gives `bad_request`; version 0 gives `unsupported_version`; a signed command whose `purchaseKey` differs from the signer gives `bad_request`; `issueBadge` from an unknown key gives `unknown_purchase_key` while `purchaseBadge` from an unknown key does not: before B7 it reaches the not-implemented handler and after B7 the code classifier answers it, so the assertion is that the response is never `unknown_purchase_key`; `pauseBadge` from a signer whose purchase row was inserted with B1's `createPurchase` gives `bad_request`, and from an unknown key gives `unknown_purchase_key`; `purchaseBadge` with an `apple` payment gives `bad_request`; with A6's harness starting the per-signer bucket at capacity 1 and **zero tokens**, a single `purchaseBadge{code}` is rejected before processing with `rate_limited` and a non-zero `retryAfter`. A capacity of 0 is not used: it never refills and so has no finite `retryAfter`. This step asserts only the pre-processing check. The failure-debit accounting of the per-signer bucket and the global budget is asserted in B10, where a redemption can fail, and the catalog bucket in B6.

#### B6 — `getBadgeCatalog`

**Files:** `apps/simplex-badge-service/src/BadgeService/Service.hs`, `tests/Bots/BadgeServiceTests.hs`

**Do:** Return `BSPBadgeCatalog {catalog, badgeStatement}` from B1's `getActiveCatalog`. Send `active` and `deprecated` prices and offers and omit `disabled` (RPC §Catalog). Apply A4's `catalogTotals` before responding, so the app renders the price the service will charge and never computes one (decision 8). An unsigned request returns the catalog alone. A signed request also returns the signing purchase's `badgeStatement`, healing the ledger first with B2's `advance now` (RPC §"Statement and balance"). Healing writes: `advance`'s `debit(lapse)` entry is persisted in the same transaction that reads the statement, so the returned balance is the stored one. This is the only read command that writes.

**Verify:** In `tests/Bots/BadgeServiceTests.hs`: a disabled price and every offer pinned to it are absent, and a deprecated price and its offers are present; a signed request from an unknown key gives `unknown_purchase_key`; with `balance_start_ts` backdated two months through B1, a signed request returns a statement whose balance equals a freshly read `getLastLedgerEntry` and appends exactly one `debit(lapse)` of −2, and an identical second request appends nothing; with the catalog bucket size overridden to 2 by A6's harness, a third unsigned `getBadgeCatalog` in the window gives `rate_limited` with a non-zero `retryAfter`, and a signed request is unaffected by that bucket.

#### B7 — `purchaseBadge{code}` and `issueBadge`

**Files:** `apps/simplex-badge-service/src/BadgeService/Service.hs`, `apps/simplex-badge-service/src/BadgeService/Store.hs`, `src/Simplex/Chat/Badges/Types.hs`, `tests/Bots/BadgeServiceTests.hs`

**Do:** Signing is an IO action, so it is performed **before** any write and no transaction is held open across it.

1. `normalizeCode`, verify the checksum, `codeHash`, then `getCodeByHash` (B1), which returns the code row joined to the redeeming purchase key.
2. Classify with B3. `RedeemAlreadyRedeemedBySameKey pid` returns the credential from `getIssuanceForRedeemedCode` and writes nothing (RPC §Idempotency). The error outcomes return their mapped codes.
3. Resolve the purchase with `getPurchaseByKey`. If it is absent, which is the normal case since C4 mints a fresh key per redemption, plan its creation for step 6 rather than writing it here; nothing is written before the signature. A repeated key is only produced by a non-standard client: credit the months to that purchase's existing ledger, write no second purchase row, and return `bad_request` if the code's `badge_type` differs from that purchase's, until tier upgrades land (§6).
4. Compute the prospective ledger state in memory with B2's pure functions: `advance now`, then `credit now months (SCPayment Nothing)`, then `issue now`. A purchase absent in step 3 has no ledger entry to read, so its state is B2's `initialLedgerState now` seeded with the code's `badge_type`. `invoiceId` is absent for code payments (`Badges/Service.hs:162`). `advance` may yield one `debit(lapse)` entry; it belongs to the write set, not to a computation that is discarded.

   `issue` returns `Nothing` in two cases, and the answer also depends on the command. The two are told apart by **`balanceStartTs > now`** — `issue`'s own already-issued guard — and NOT by the balance (§9, B10): after the last funded month is issued both hold at once, and reading the balance there would refuse a credential the service has already signed and stored. When the month it would cover is NOT already issued the balance is exhausted: go to step 6 if `advance` produced a `debit(lapse)`, writing that entry alone, and otherwise straight to step 7. When step 6 runs, the `statement` is read back inside its transaction; when nothing is written, it is read in a single read transaction. Either way it never shows a balance the database does not hold. Otherwise the current month is already issued, whatever the balance is. Fetch its credential with B1's `getIssuanceForPeriod` probed **at `now`**, not at `addMonths (-1) balanceStartTs` (§9): `now` is inside that period because the previous `issue` moved `balanceStartTs` to the start of the next unissued month, which is past `now`, while its own period start is at or before the instant that issue ran; `advance` therefore returns `Nothing` here. For `issueBadge` there is nothing to record, so return it and write nothing (RPC §Idempotency). For `purchaseBadge{code}` the credit must still be recorded, so go to step 6 with the fetched credential in place of a fresh signature and with neither a `debit(badge)` entry nor an issuance row to write, since that month's issuance and its debit already exist and B2 property 3 keeps them 1:1; only the code redemption, the payment row, any `debit(lapse)` and the `credit(payment)` entry are recorded. Neither case reaches step 5.
5. Sign the resulting period with B4. **A signing failure returns `internal` and writes nothing**; the code stays unredeemed and the client may retry it.
6. No write happens before a signature succeeds or step 4 proves one unnecessary. Then open one transaction and write, in order: the `badge_purchases` row if absent, through `createPurchase`; the `payments` row through `createCodePayment`, pointed at by the purchase through `attachPurchasePayment` only when the purchase has no payment yet (§9); the `debit(lapse)` entry `advance` produced in step 4, if any; the `credit(payment)` entry; the `debit(badge)` entry; the issuance row carrying the signed credential; and `redeemed_purchase_id` with `redeemed_at` on the code. A conflict on the code's redemption columns aborts the transaction and re-classifies from step 1.

   `@payments` has no `price_id` or `offer_id` columns (`M20260731_user_badges.hs:42-56`), so core §5's "price_id and offer_id NULL" does not apply. A code purchase writes no `@badge_invoices` row.
7. Respond `BSPBadgeCredential {credential, receipt, statement}` with `receipt = Nothing`. The receipt is the transfer instrument for unissued months and belongs to the `SPReceipt` payment this plan defers (§6); the service writes no `receipt_hash` (A3) and C4 stores nothing for it. An exhausted balance is not an error: `credential = Nothing`, with the statement showing the zero balance.

`issueBadge` performs steps 4 to 6 against an existing balance, with no code involved. It is the only command that re-issues, and C3's worker is its only caller. It is also the only command carrying an asserted cursor (`balance.lastEntry.entryId`): resolve it against this purchase's ledger and return the entries after it with `previousEntryId` echoing it, or the complete history when it names nothing (§9).

**Verify:** covered by B10.

#### B8 — `codes` operator subcommand

**Files:** `apps/simplex-badge-service/src/BadgeService/Options.hs`, `apps/simplex-badge-service/src/BadgeService/Admin.hs`, `apps/simplex-badge-service/Main.hs`, `simplex-chat.cabal`

**Do:** Restructure the option parser so the service run stays the default with no subcommand. `BadgeService/Options.hs:63` currently uses a plain `execParser $ info …` with no subparser; wrap the new subparser in `optional`, because `hsubparser` alone makes a subcommand mandatory (decision 3).

```
simplex-badge-service                                       # run the service (unchanged)
simplex-badge-service codes issue  --type supporter --months 3 --count 100 --batch promo-2026q4 [--expires YYYY-MM-DD]
simplex-badge-service codes revoke --batch promo-2026q4
simplex-badge-service codes status --code SXB-…
```

- `--config` and the `coreChatOpts` database options apply to the subcommand as well as to the service run; the subcommand loads the same ini.
- `--type` accepts `supporter` or `legend`, parsed into `BadgeType`. Any other value is a usage error. `investor` is rejected: lifetime codes are out of scope (§6).
- `--months` is required and must be at least 1; the parser rejects 0, and `@codes.months` carries `CHECK (months > 0)` (A3). A code always credits at least one month, which is what keeps B7's zero-balance branch reachable only from `issueBadge`.
- `--expires YYYY-MM-DD` defaults to `[codes] default_expiry_days` from the ini (A6, 365 by default). A past date is accepted, which is how B10 constructs an expired code; the value is stored as given, with no validation beyond the date format.
- `issue` prints plaintext codes to stdout once and stores only hashes.
- The subcommand asserts the schema is current and **fails if a migration is pending, rather than migrating**, because it may run against the database of a live service. It calls `seedCatalog`, does its work in single short transactions, and exits. It does not start the bot.

**Verify:** covered by B10: issuing 10 codes produces 10 rows with distinct hashes and no plaintext anywhere in the database file; `revoke --batch` sets `revoked_at` on exactly that batch; `status` reports unredeemed, redeemed and revoked; `--type investor` is rejected; `--expires` in the past is accepted.

#### B9 — Service address publication

**Files:** `apps/simplex-badge-service/src/BadgeService/Service.hs`, `tests/Bots/BadgeServiceTests.hs`

**Do:** The RPC path depends on clients knowing the bot's contact address. `initializeBotAddress'` (`Service.hs:110`) already creates it and prints it at startup via `showBotAddress` (`src/Simplex/Chat/Bot.hs:65-68`, gated on `logAddress = not testing`), but nothing persists it. Write the address to the path in `[service] address_file` when that key is set. A6 parses that key already, so this step adds no configuration field and does not touch `Config.hs`.

No address-file pattern exists in the repo to copy: the directory service (`Directory/Service.hs:223`) only calls the same `initializeBotAddress'`. This is new code.

The operator publishes this address; it reaches clients through `ChatConfig.badgeServiceAddress` (C2), which defaults to `Nothing` and is set in release builds. H5 documents the procedure.

**Verify:** `testBadgeServicePublishesAddressFile` (`BadgeServiceTests.hs`): starts the real `badgeService` entry point twice against a config carrying `[service] address_file`, asserts the file's contents are byte-identical after both starts and match the address the client is actually given, then has a second client run `/c` against exactly those contents and gets back "connection request sent!" rather than a parse rejection. See §9 for why this replaces the manual run originally specified.

#### B10 — Service integration tests

**Files:** `tests/Bots/BadgeServiceTests.hs`

**Do:** The harness already runs the service in-process (`withBadgeService`, `BadgeServiceTests.hs:51`) and drives a client with `/_service_request`. Replace the single `unsupported_version` test with:

- redeem a valid code and get a credential that `verifyCredential` accepts
- the ledger holds exactly `credit(payment) +N` then `debit(badge) −1`, and the statement returns both
- repeating the identical request returns the same credential and writes no new rows
- the same code from a second purchase key gives `code_used`
- an unknown code gives `code_invalid`; a past `expires_at` gives `code_expired`; a revoked code gives `code_invalid`
- with the purchase's `balance_start_ts` backdated one month through B1, `issueBadge` issues the second period; a third call inside the same month returns the cached credential
- an exhausted balance returns `credential = Nothing` with a zero-balance statement
- a second code for a different `badge_type` presented under a purchase key that already exists gives `bad_request`
- with the per-signer bucket overridden to 3, a fourth failed redemption from **one** signer gives `rate_limited` while a fresh signer is not throttled and gets its own outcome; with the global failure budget overridden to 3, a fourth failed redemption from a **fresh** signer gives `rate_limited` with a non-zero `retryAfter`, and advancing A6's clock past the refill window lets a valid code succeed again (B5). No test sleeps
- a schema assertion: no table in the service database carries both a column referencing `@web_orders` and a column referencing `@badge_purchases`, enumerated from `sqlite_master` (§3 Linkage). This assertion and B8's no-plaintext-in-the-database-file assertion below both read a SQLite file, so guard them with `#if !defined(dbPostgres)`; without the guard A3's Postgres run of `-m "Badge service"` breaks the moment this step lands. This is the regression guard for the privacy claim; it fails the day a later step adds a column joining the two
- B8's assertions: 10 codes, distinct hashes, no plaintext at rest, batch revocation, `status` output, `--type investor` rejected, past `--expires` accepted

**Verify:** `cabal test --test-options='-m "Badge service"'` passes and the run reports the new examples. Run locally: `xdescribe''` skips this spec when `CI=true`.

---

### Phase C — Core client: redeem and badge state

Phase C ends with a chat client that can redeem a code minted by B8 and show the resulting badge, with the worker re-issuing each month. Nothing in it touches a payment provider or the site.

#### C1 — `Store/Badges.hs`: client badge store

**Files:** `src/Simplex/Chat/Store/Badges.hs`, `simplex-chat.cabal` (add `Simplex.Chat.Store.Badges` to the library `exposed-modules` and `Bots.BadgeStoreTests` to the test stanza's `other-modules`), `tests/Bots/BadgeStoreTests.hs`, `tests/Test.hs`

**Do:**

- Statuses are the constructors of `BadgePurchaseStatus` (`Badges/Types.hs:64`) and `BadgePaymentStatus` (`:60`), written through the `ToField` instances A2 adds; no new status is invented. A code redemption writes only `PSIssued`, `PSSuperseded` and `BPSSettled`, spelled `issued`, `superseded` and `settled`; the rest belong to the invoice flow this plan defers (§6).
- `createCodePayment` writes the `payments` row with `provider = 'code'`, `invoice_id` NULL and `status = 'settled'`, and returns its id. `payments.payment_id` and `badge_issuances.issuance_id` are both `TEXT NOT NULL PRIMARY KEY` with no default (`M20260731_user_badges.hs:43,167`), so each caller mints a UUID with `Data.UUID.V4.nextRandom`; `uuid` is already a library dependency (`simplex-chat.cabal:376`). `badge_ledger.entry_uuid` is different: it is authored by the service and copied verbatim (core §1), never minted here. As on the service side, `payments` has no `price_id` or `offer_id` columns (`M20260731_user_badges.hs:42-56`), so core §5's "`price_id` and `offer_id` NULL" does not apply.
- `createPurchase` writes the purchase row with the purchase keypair, the badge master key, that `payment_id`, status `issued`, and `initial_badge_type` and `current_badge_type` both set to the badge type the service stated. Both columns are `NOT NULL` with no default (`M20260731_user_badges.hs:97-98`), and a code redemption does not know its badge type until the response arrives, so the row is written **on success only**, as core §5 requires for the code payment. Nothing is persisted before the send; a response lost in flight is recovered with H2's `codes unredeem`, not by reusing a stored key. Core §5 calls this row *the badge row*; this plan calls it the purchase row throughout (§3).
- `createIssuance` writes the `badge_issuances` row carrying the verified credential and its period, which C3's worker writes again for each new period.
- `supersedePurchases` moves every other purchase of the same slot for that user from `issued` to `superseded`, and `setShownPurchase` points `users.shown_badge_id` at the new row. The column exists (`M20260731_user_badges.hs:220`) and has no writer in the repo yet, so this is new code; it is separate from `setUserBadge` (`Store/Profiles.hs:375`), which writes the profile's badge columns and is what C3 calls for presentation. `supersedePurchases` derives the slot from the kept purchase's own `current_badge_type` rather than from a badge type the caller states, so a mismatched pair cannot clear the wrong slot.
- `getShownPurchase` reads the purchase `users.shown_badge_id` points at, returning the row with its purchase keypair and badge master key, which is what C2 renders and what C3 signs `issueBadge` with. It is new code: `shown_badge_id` has no reader in the repo either. **Both ends scope the purchase to the user.** `shown_badge_id` is a bare foreign key to `badge_purchases` with nothing tying it to the pointing profile, and the row it reaches carries a private key the worker signs with, so `setShownPurchase` refuses a purchase that is not this user's and `getShownPurchase` joins on `p.user_id = u.user_id`.
- `getLastBadgeLedgerEntry` reads the purchase's last `badge_ledger` row, which is what the worker compares a statement against.
- `insertLedgerEntries` writes a statement's entries into `badge_ledger` verbatim. The client is never the author (core §1). An `opening` entry restates the balance absolutely, regardless of what preceded it. It takes the wire `BadgeStatement` whole, because `previousEntryId` is what decides how the entries attach and a caller must not have to reimplement that: **present** it names an entry the client already holds and the entries append after it; **absent with entries present** the entries "attach to nothing" (`badges-rpc.md:56`) and the statement *is* the ledger, so rows the client holds that the statement does not carry are stale and are deleted — appending on an absent cursor would duplicate the ledger. An empty `entries` changes nothing either way. Insertion is `ON CONFLICT (entry_uuid) DO NOTHING` against `@idx_badge_ledger_uuid`: `purchaseBadge` and `getBadgeCatalog` return the complete history every time, so re-delivery is the normal case and must be a no-op, not merely non-crashing. Array position is the only thing that orders the entries — every entry of one command shares a `createdAt` and `serviceCreatedAt` is not on the wire — so they are inserted in the order given and never sorted.
- Unknown credit and debit tags go to `entry_type_unknown` and `entry_type_value`, for re-decoding after an app upgrade. Known tags keep `BadgeService/Store.hs`'s spellings exactly. `credit(charge)`, `credit(transferIn)`, `debit(upgrade)` and `debit(transferOut)` are refused rather than coerced, matching `BadgeService/Service.hs`'s `statementEntryType`, which refuses to put any of them on the wire in the first place.

**Verify:** In `tests/Bots/BadgeStoreTests.hs`, registered in the test stanza's `other-modules` and in `tests/Test.hs` under the **`Supporter badges store`** path inside the `testBracket` bracket, so CI runs it (§4 rules 7 and 8): insert a statement, read back the last entry, and assert an `opening` entry resets the balance regardless of what preceded it; `createCodePayment` then `createPurchase` leaves one purchase row with status `issued`, both badge-type columns set and its `payment_id` pointing at a `settled` payment; a second `createPurchase` followed by `supersedePurchases` and `setShownPurchase` leaves exactly one `issued` row in the `paid` slot and one `superseded`, with `users.shown_badge_id` on the `issued` one; `createIssuance` writes one `badge_issuances` row with the credential and its period, and a second call for the following period leaves two rows; `getShownPurchase` returns the row `setShownPurchase` pointed at, with its private key intact.

#### C2 — Commands, responses, events, parsers, View

**Files:** `src/Simplex/Chat/Controller.hs`, `src/Simplex/Chat.hs`, `src/Simplex/Chat/Library/Commands.hs`, `src/Simplex/Chat/Store/Badges.hs` (unchanged; C1's readers serve `APIGetBadgeState`), `src/Simplex/Chat/View.hs`, `src/Simplex/Chat/{Options.hs,Mobile.hs,Terminal/Main.hs}`, `apps/simplex-directory-service/src/Directory/Options.hs`, `apps/simplex-broadcast-bot/src/Broadcast/Options.hs`, `apps/simplex-badge-service/src/BadgeService/Options.hs`, `tests/ChatClient.hs`, `src/Simplex/Chat/Badges/Types.hs`, `bots/src/API/Docs/{Commands.hs,Responses.hs,Events.hs,Types.hs}`, `bots/api/TYPES.md`, `packages/simplex-chat-client/types/typescript/src/types.ts`, `packages/simplex-chat-python/src/simplex_chat/types/_types.py` (regenerated), `tests/BadgeTests.hs`, `tests/APIDocs.hs` (unchanged; the asserting module)

**Do:**

- Define `BadgePurchasePayment`. No Haskell type of that name exists in the repo; its shape is specified in core §3 (`plans/2026-07-31-badges-core-implementation.md:110`) as `apple {paymentId, jws}` | `google {paymentId, token}` | `code {code}`, mapped to the wire `ServicePayment` (`PaymentService.hs:25`). For this scope only the `code` case is needed; keep the constructor name and tag from core §3 so the store cases can be added later without a rename.
- Add the subset of core §5 this scope needs:

  ```haskell
  | APIGetBadgeState UserId                                          -- /_badge state <userId>
  | APIGetBadgeCatalog UserId                                        -- /_badge catalog <userId>
  | APIPurchaseBadge {userId :: UserId, payment :: BadgePurchasePayment}  -- /_badge purchase <userId> <json>
  ```

- Responses `CRBadgeState` and `CRBadgeCatalog`; event `CEvtBadgeChanged`; error `CEBadgeServiceError {badgeError, badgeErrorMessage, retryAfter}` for the inline redeem errors (UX §2.8). `retryAfter` is `Maybe Word32`, populated by B5's `rate_limited`. The message field is NOT `message`: a record field has one type across a whole data type and `ChatErrorType.message` is already `String` in 15 constructors (§9).
- `CRBadgeState` carries the shown badge's paid-through date, `addMonths balanceMonths balanceStartTs` over the last ledger entry, using B2's `Simplex.Chat.Badges.Months`; the clamping rule has one implementation and the client does not restate it. G6 renders it in place of `stubBillingDate()` and its Swift equivalent. UX §2.11 requires the paid-through date and forbids the phrase "badge valid until", so the credential expiry is never shown.
- `CRBadgeState` also carries `badgeWebBaseUrl`, filled by this step's `APIGetBadgeState` handler from `ChatConfig`. `ChatConfig` is not readable from Kotlin or Swift, so the site URL travels in a response rather than through the FFI, and it rides on the badge-state response, which is a local read, so the browser hand-off does not depend on the service being reachable.
- Parsers in `chatCommandP`, rendering in `View.hs`.
- `APIGetBadgeState` is handled in this step. `APIPurchaseBadge` and `APIGetBadgeCatalog` get handlers that `throwChatError $ CEBadgeServiceError {badgeError = BSEInternal, badgeErrorMessage = Just "not implemented", retryAfter = Nothing}`, following the raise convention of every other handler (`Library/Commands.hs:512`), which C4 replaces. This step therefore compiles and its unfiltered `cabal test` passes with no command unhandled. It is the same stub discipline C3 uses for `sendBadgeRequest`.
- Credential verification needs no new key plumbing: `ChatConfig.badgePublicKeys :: Map Int BBSPublicKey` (`Controller.hs:145`) already holds the issuer keys by index, and `addUserBadge` (`Library/Commands.hs:5127-5145`) already looks the credential's index up there, calls `verifyCredential` (`:5131`), calls `setUserBadge` (`:5134`) and re-presents the profile to every contact (`:5138-5145`). Tests override the map the way the profile tests do, `testCfg {badgePublicKeys = testBadgeKeys pk}` (`tests/ChatTests/Profiles.hs:295`).
- `addUserBadge` cannot be called as it stands from C3 or C4. It takes the global `chatLock` (`:5138`) and broadcasts `XInfo` to every contact, and its only current call site is unlocked and top level (`:3544`); C3 and C4 hold a per-user badge lock that already waits on `chatLock`, so calling it there inverts the lock order. It also fails with `throwCmdError` (`:5132`), which is a `CECommandError` (`Controller.hs:1688-1689`), not the `CEBadgeServiceError` C4 must surface. **Split it in this step** into three parts. `verifyUserBadge :: BadgeCredential -> CM (Either Text ()) ` (`:5129-5132`) looks up the key and verifies, and is callable under a lock. It **returns** its failures rather than throwing, because its three callers need three different outcomes: `addUserBadge` maps them back to `throwCmdError`, keeping `AddBadge` unchanged; C4 maps them to `CEBadgeServiceError`; C3 discards the credential and writes nothing. Both current failures, the unknown key index at `:5130` and the failed verification at `:5132`, become `Left`. The middle part, `setUserBadge` and the `currentUser` TVar write (`:5133-5135`), stays inline; it returns the updated `User`. `presentUserBadgeToContacts :: User -> CM ()` (`:5136-5145`) takes that `User`, acquires `chatLock` and broadcasts. `addUserBadge` becomes the three in sequence, so `AddBadge` (`:3544`) is unchanged.
- Three `ChatConfig` fields, all overridable in tests:
  - `badgeServiceAddress :: Maybe (ConnectTarget 'CMContact)`, the service's contact address, published by B9, matching `APISendServiceRequest.sendTarget` (`Controller.hs:414`). It defaults to `Nothing` in `defaultChatConfig`, which means the feature is unconfigured: C4 fails with `CEBadgeServiceError` and G1 hides the browser hand-off. Release builds set it; this plan does not carry an address literal, because the address is produced by the operator's own service run (B9, §6).
  - `badgeWebBaseUrl :: Text`, the checkout site, used by G1 to build the hand-off URL. A service ini setting cannot reach the app, so the site URL must live here. It defaults to `""` in `defaultChatConfig`, empty meaning unconfigured on the same terms as `badgeServiceAddress`. Release builds set it, and it must then equal the service's `[web] base_url` (A6), because Stripe's `success_url` derives from that side and the hand-off URL from this one; H5 documents keeping the two in step.
  - `badgeCurrentTime :: IO UTCTime`, defaulting to `getCurrentTime`. C3's worker and C4 read the clock through it, so C5 can advance client time without sleeping.

  All three are given their defaults in `defaultChatConfig` (`src/Simplex/Chat.hs:60-62`), the record's only full construction site; `Mobile.hs` and `Terminal.hs` use record update and are unaffected. `--badge-service-address LINK`, `--badge-web-url URL` and `--badge-issuer-key IDX:BASE64URL` are added to `ChatOpts` as `optBadgeServiceAddress`, `optBadgeWebUrl` and `optBadgeIssuerKeys` — named apart from the `ChatConfig` fields they override, following the existing `optFilesFolder`/`optTempDirectory` convention, so the record update in `Terminal/Main.hs` is not ambiguous — and are parsed in `chatOptsP` (`src/Simplex/Chat/Options.hs:40-57,365`); all three override `cfg` by record update in `simplexChatCLI` (`Terminal/Main.hs:24-28`), which is how §10 and H5 point a client at a locally run service. `--badge-issuer-key` is repeatable and its entries **replace** `badgePublicKeys` outright when any is given, rather than merging: the field already ships eight production keys (`src/Simplex/Chat.hs:69-79`), index 1 among them, so a merge that kept the presets would leave a locally issued credential failing to verify against the production key at the same index. `BBSPublicKey` is 96 bytes and `strEncode`s as unpadded base64url, which is the form `badge keygen` prints. Unlike `ChatConfig`, `ChatOpts` has **six** full construction sites, and every one of them must set the new fields: `chatOptsP` (`Options.hs:485`) from the new parsers, and the other five to their empty values, `mobileChatOpts` (`Mobile.hs:249`), the three bots' `mkChatOpts` (`Directory/Options.hs:217`, `Broadcast/Options.hs:84`, `BadgeService/Options.hs:76`) and `testOpts` (`tests/ChatClient.hs:115`). A missed site is only a `-Wmissing-fields` warning, not a build error, so it fails at runtime in the binary that missed it.
- **Register the new API surface in the bot API docs, or `cabal test` breaks.** `tests/APIDocs.hs:73-110` fails any command, response or event that is in neither the documented nor the exempt list. Both `cliCommands` and `undocumentedCommands` are EXEMPTION lists, not documentation: `tests/APIDocs.hs:76` unions them with the documented set, and only `chatCommandsDocs` reaches `bots/api/COMMANDS.md` and the generated clients. Add the three commands to `undocumentedCommands` (`bots/src/API/Docs/Commands.hs:335`), which is where the `API*` commands go, the two responses to `undocumentedResponses` (`Responses.hs:119`), and `CEvtBadgeChanged` to `undocumentedEvents` (`Events.hs:169`); none of the four changes a generated file. `CEBadgeServiceError` extends the documented `ChatErrorType` union (`Types.hs:224`), which does, so add `BadgeServiceErrorCode` to the type docs and commit the regenerated `bots/api/TYPES.md`, `types.ts` and `_types.py`. `describe "Bot API docs"` (`tests/Test.hs:62`) runs on the default CI build and §4's `-m` filters do not select it, so run `cabal test` unfiltered before ticking.
- `APIAckBadgeAlert`, `CEvtBadgeAlert` and `APISwitchShownBadge` are **not** added; alerts are out of scope and, with only the `paid` slot in use, there is nothing to switch between (§6).

**Verify:** In `tests/BadgeTests.hs`: command parser roundtrip tests. `cabal test` **unfiltered** passes, including `describe "Bot API docs"`, with the regenerated artefacts committed. Manual: `/_badge state 1` on a profile with no badge returns an empty `CRBadgeState` rather than an error; with no options given it reports an empty site URL rather than failing; `simplex-chat --badge-service-address LINK --badge-web-url URL --badge-issuer-key 1:KEY` starts with `/_badge state 1` reporting the overridden site URL; a credential signed by that local key verifies through `/badge add` while the same credential fails without the option, which is what proves the issuer keys REPLACE rather than merge; and `/_badge catalog 1` returns the stub `CEBadgeServiceError`. Reaching the service at the overridden address is C4's check, not this step's — this step stubs `APIGetBadgeCatalog` (§9).

#### C3 — `BadgeManager` worker

**Files:** `src/Simplex/Chat/Controller.hs`, `src/Simplex/Chat.hs`, `src/Simplex/Chat/Library/Commands.hs`, `src/Simplex/Chat/Library/Internal.hs`, `src/Simplex/Chat/Store/Shared.hs`, `tests/Bots/BadgeManagerTests.hs`, `simplex-chat.cabal`, `tests/Test.hs`

**Do:** Reduced from core §6 to this scope.

```haskell
badgeWorkers :: TMap UserId Worker    -- a ChatController field: agent Worker, doWork TMVar, restart on crash
```

- Reuse the agent `Worker` framework from simplexmq's `Simplex.Messaging.Agent.Client`. `getAgentWorker` is used at `Library/Subscriber.hs:4024,4091,4331` (imported at `:82`); `hasWorkToDo'` and `cancelWorker` are exported by the same module but have no in-repo caller yet. The `TMap … Worker` controller fields follow `deliveryTaskWorkers` and `relayRequestWorkers` (`Controller.hs:307-309`).
- Initialise `badgeWorkers` where `deliveryTaskWorkers` and `relayRequestWorkers` are initialised (`src/Simplex/Chat.hs:196-198,242-244`). `stopChatController` (`Library/Commands.hs:348`) cancels no worker map today, so this step adds the badge-worker cancellation there with `cancelWorker`; that is new code, not a pattern to copy.
- Add a `CLBadge UserId` constructor to `ChatLockEntity` (`Store/Shared.hs:68`), whose derived `Eq` and `Ord` still hold, and give it a branch in `enityLockString` (`Library/Commands.hs:3639-3646`): `CLBadge userId -> "Badge " <> tshow userId`. That case is exhaustive with no wildcard and the build uses `-Werror=incomplete-patterns` (`simplex-chat.cabal:338`), so the new constructor breaks the build until the branch exists.
- Define `withBadgeLock :: Text -> UserId -> CM a -> CM a`, `withEntityLock name . CLBadge`, with an `INLINE` pragma, beside the existing wrappers at `Library/Internal.hs:133-139`, and lock every signed badge operation with it. There is no separate lock map: the badge lock is an ordinary entity lock over the shared `entityLocks` map (`Controller.hs:295`), so the `chatLock`-first order is inherited rather than reimplemented, and no new lock order is introduced.
- The worker calls `sendBadgeRequest`, the single send path C4 implements in `Library/Commands.hs` (C4 gave it the request mode and the `User` as well, §9). At this step it is a stub with that signature returning `BSPError` with code `internal` and the message `not implemented`, so the worker compiles and its mechanics are testable; C4 gives it the lazy connection and the signing.
- Per pass, for the shown purchase: re-issue when the balance is positive and the month is unissued — the last ledger entry's `balanceMonths > 0` and `balanceStartTs <= now`, which is `issue`'s own guard and never the balance alone (B2) — by calling `issueBadge`, signed with the purchase's own key and carrying the purchase row's master key unchanged (§9). Apply the response with C1's writers, in one transaction: `insertLedgerEntries` for the statement verbatim, then `createIssuance` for the period the statement's last `debit(badge)` entry implies, with `ledgerEntryId = Nothing` (§9). The purchase row itself does not change on a re-issue, since it is already `issued`; only a new issuance row is written. Verify it with C2's `verifyUserBadge` before the transaction and call `setUserBadge` inside it; a credential that fails verification is discarded and nothing is written. **Every user gets a worker (below), so the profile a pass just wrote need not be the active one** — write the `User` it returns to the `currentUser` TVar only when that profile IS the active one, on the read-then-compare-then-write idiom already used at `Commands.hs:576,1647,4373` (§9). Release the badge lock, then call C2's `presentUserBadgeToContacts` with that `User`, which takes `chatLock`, so the monthly pass never broadcasts while holding a lock. Emit `CEvtBadgeChanged` on change.
- A response with `credential = Nothing` is not an error: it is the exhausted balance B7 defines. Store the statement's ledger rows, write no issuance, leave the profile's badge as it is, and emit `CEvtBadgeChanged` only if the stored state changed. The pass is not suppressed afterwards: a pass on a zero balance reads the last ledger entry and stops, which is cheap, and suppressing it would need a re-arming signal that nothing sends.
- Three things trigger a pass, and nothing else does:
  - the chat controller start signals every user's worker, beside the other worker starts in `startChatController`'s `start` block (`Library/Commands.hs:250-252`, `startDeliveryWorkers`, `startRelayRequestWorker_`, `startCleanupManager`), which has `users` in hand and so suits a per-user worker. `src/Simplex/Chat.hs:196-198,242-244` is `newChatController` and only initialises the maps;
  - a timer of `ChatConfig.badgePassInterval` re-signals it, a field this step adds with a default of 24 hours. A month boundary is the only event the worker waits for, so a daily wake is frequent enough, and a pass with the month already issued reads the last ledger entry and stops;
  - `APIGetBadgeState` signals it as core §5 specifies. That command fires whenever a badge screen opens or regains focus, so a user who has just crossed a month boundary sees the new credential without waiting for the timer, and G4's and G5's manual checks have a trigger they can drive.
- `hasWorkToDo'` signals the `doWork` TMVar in all three cases, and the timer is the badge worker's own loop rather than a shared scheduler. Tests drive a pass with `APIGetBadgeState` rather than the timer, so no test waits on wall-clock time.
- Out of scope here: invoice reconciliation, store evidence, the alert timer, `CEvtBadgeAlert`, and Monday presentation. Those stay deferred in §6.

**Verify:** In `tests/Bots/BadgeManagerTests.hs`, registered in the test stanza's `other-modules` and in `tests/Test.hs` under the **`Supporter badges`** path inside the `testBracket` bracket, so CI runs it (§4 rules 7 and 8): with a purchase row inserted by C1, composing the pass's two halves — `storeBadgeIssueResponse`, which the pass runs under the badge lock, and `presentBadgeChange`, which it runs with the lock released (§9) — over a `BSPBadgeCredential` whose credential was signed by a test key through `Simplex.Chat.Badges.issueBadge`, against a controller started with `testCfg {badgePublicKeys = testBadgeKeys pk}` (`tests/ChatTests/Profiles.hs:295`), writes the issuance row and the statement's ledger rows and emits `CEvtBadgeChanged`; a credential signed by a different key is rejected and writes nothing; a response with `credential = Nothing` writes the ledger rows and no issuance; a second signal arriving during a pass does not run concurrently. No service is started, and no RPC is sent, so the test needs no stub point. `tests/BadgeTests.hs` cannot host this test, being a plain `Spec` (A2). The month-boundary re-issue against the live service is asserted in C5.

#### C4 — Redeem path wired end to end

**Files:** `src/Simplex/Chat/Library/Commands.hs`

**Do:** `sendBadgeRequest`, the single send path C3's worker also calls, plus `APIPurchaseBadge` with a code, under the per-user badge lock, and `APIGetBadgeCatalog`:

- Generate the purchase keypair (Ed25519, per core §5) and the badge master key in memory, and hold them for the duration of the call. Nothing is written before the response, because the purchase row's two badge-type columns are `NOT NULL` and the badge type is not known until the service states it (C1). A fresh key per redemption is the rule (§3).
- `sendBadgeRequest :: NetworkRequestMode -> User -> Maybe C.PrivateKeyEd25519 -> BadgeServiceRequest -> CM BadgeServiceResponse` sends one RPC to `ChatConfig.badgeServiceAddress`, signed with the given key or unsigned when it is `Nothing` (B5's identity rules). The agent's service request is its own connection — it joins the address, takes the one reply and deletes the connection again — so nothing is contacted until a badge command sends something and a profile that never buys a badge never reaches the service. It takes the profile's `User` and the request mode rather than reading the active user (§9), and returns its transport failures as `BSPError internal` rather than throwing them (§9). It replaces C3's stub.
- Send `purchaseBadge`, once per tier a code can fund: the client cannot know a code's tier and B7 refuses a `badgeRequest` naming another one, so the code is presented under `supporter` then `legend` until the service stops answering `bad_request` (§9).
- On success, call C2's `verifyUserBadge` first: a credential that fails verification surfaces as `CEBadgeServiceError` and writes nothing. Its `Left` becomes `CEBadgeServiceError` with `badgeError = BSEInternal` and the `Left` text as `message`: the failure is local, and no `BadgeServiceErrorCode` denotes one. Then, in one transaction, call C1's `createCodePayment`, `createPurchase` with the response's badge type and the held keys, `insertLedgerEntries` for the statement verbatim, `createIssuance` for the credential, `supersedePurchases` for that slot, `setShownPurchase` and `setUserBadge`. Write the `User` `setUserBadge` returns to the `currentUser` TVar when that profile is the active one, on the read-then-compare-then-write idiom C3's pass uses (§9), or the in-memory user keeps the old badge. Release the per-user badge lock, then call C2's `presentUserBadgeToContacts` with that `User`, which takes `chatLock` and broadcasts the updated profile. The badge type, and with it the slot, is stated in the response (core §5). These are the **client's** tables; B7 writes the service's own `badge_purchases` row independently, and the two databases share only the ledger rows.
- Do not signal the worker: C4 has already presented the badge, and the current month is issued, so a pass would find nothing to do. The next pass comes from C3's timer or the next `APIGetBadgeState`.
- On timeout, surface the error to the user. A code consumed by a lost response is recovered with H2's `codes unredeem`.
- `APIPurchaseBadge` answers `CRBadgeState` for the profile it has just changed and emits no event (§9); its store payment cases are refused (§9).
- `APIGetBadgeCatalog` calls `sendBadgeRequest` unsigned and returns `CRBadgeCatalog`, offers included with their server-computed totals (A2, B6). It takes no user lock, stores nothing and computes nothing. A failure surfaces as `CEBadgeServiceError`, which G4 and G5 render as an unavailable price.

**Verify:** covered by C5.

#### C5 — Client and service integration tests

**Files:** `tests/Bots/BadgeServiceTests.hs`

**Do:** Extend B10's spec rather than `tests/BadgeTests.hs`. B10's spec runs under `around (testBracket …)` (`tests/Test.hs:92`) and so has `TestParams`, which `withBadgeService` requires; `tests/BadgeTests.hs` is a plain `Spec` registered outside that bracket (`:66`) and has no `TestParams`.

- Issue a code with B8's tooling, redeem it from the client, and assert the badge is shown in the profile.
- A profile with no prior connection to the service redeems a code successfully on the first attempt.
- The client's `badge_ledger` rows equal the service's row for row on `entry_uuid`, `change_months`, `balance_months`, `balance_start_ts` and `balance_badge_type`.
- A wrong code surfaces `CEBadgeServiceError code_invalid`, and no purchase row is written.
- Redeeming a second code leaves the first purchase `superseded` and the second `issued` and shown, with the first purchase's ledger rows untouched.
- `APIGetBadgeCatalog` from a profile with no prior connection returns the four seeded offers with the totals A4 asserts, and returns `CEBadgeServiceError` with the service stopped.
- With the clock advanced a month, `APIGetBadgeState` signals the worker (C3) and one `BadgeManager` pass issues the second period and emits `CEvtBadgeChanged`; the test does not wait on `badgePassInterval`.

**Verify:** `cabal test --test-options='-m "Badge service"'` passes and the run reports the new examples. Run locally.

---

### Phase D — Site and checkout endpoint

Phase D ends with a browsable, priced site wizard whose Pay button reaches a real endpoint, with the provider branches stubbed to `provider_unavailable`. Prices on the site are the server's own totals, so the phase exercises the server's pricing path rather than a browser copy of it. Nothing can be bought until E2: layout, copy, pricing arithmetic, accessibility and the error states are all reviewable here, before any provider integration exists. Order creation is reviewable as code only; the first order row is written in E2.

#### D0 — Store layer: orders, invoices, provider events

**Files:** `apps/simplex-badge-service/src/BadgeService/Store.hs`, `tests/Bots/BadgeServiceTests.hs`

**Do:** The web-order half of the store layer, split from B1 because it has no caller until D6 and belongs to the site rather than to the RPC path. It adds functions to the module B1 created, so there is no cabal edit, and it depends on B1 for `ServiceError` and `withServiceTransaction`. Same discipline as B1: every function takes a `DB.Connection` and opens no transaction of its own.

- `createOrder`, writing the `@invoices` row and the `@web_orders` row that references it in one call.
- `getOrder`, returning the order **joined to its `@invoices` row**, so one call yields amount, currency, address, crypto amount, payment URL and expiry. E4 serves the order half of its response from this; its `code` and `disclosureExpiresAt` come from a separate `getCodeByHash` on the derived code (§3 Linkage).
- `getOrderByProviderRef` and `getOrderByShortRef`, each returning at most one row; A3's unique indexes make a second impossible. H2's `--ref` subcommands resolve through the second.
- `getStuckOrders :: UTCTime -> …`, returning orders in `invoiced` or `pending` whose `@invoices.expires_at` has passed, oldest first. H3's pass reads it.
- `updateOrderStatus`, taking the new status and an optional `amountPaid`, so a partial payment or an underpaid expiry records the amount without settling (E3); `setOrderProviderRef`; `setOrderSettled`, writing `settled_at`, `amount_paid` and `status = 'paid'` together. `updateOrderStatus` and `setOrderSettled` also write the matching `@invoices.status` in the same transaction, keeping A3's invariant; nothing in this plan reads it.
- `recordProviderEvent`, returning `False` only when the event is already present **and** processed; a row with a NULL `processed_at` is reprocessed, because the previous attempt did not complete. `markProviderEventProcessed`, called inside the settlement transaction.

**Verify:** In `tests/Bots/BadgeServiceTests.hs`: `createOrder` writes both rows and `getOrder` returns the invoice fields with them; `getOrderByProviderRef` finds the order after `setOrderProviderRef` replaces the value; `getOrderByShortRef` resolves a bank-statement reference; `getStuckOrders` returns an expired `pending` order and omits a `paid` one; `updateOrderStatus` with an `amountPaid` on a `pending` order records it and leaves `settled_at` NULL; `recordProviderEvent` returns `False` on a processed replay and `True` on an unprocessed one.

#### D1 — Web project skeleton and tsc build

**Files:** `apps/simplex-badge-service/web/{package.json,package-lock.json,tsconfig.json,index.html,styles.css}`, `apps/simplex-badge-service/web/src/`, `apps/simplex-badge-service/web/assets/.gitkeep`, `apps/simplex-badge-service/web/dist/`

**Do:**

- `package.json` with exactly one devDependency, `typescript`. Commit `package-lock.json` so `npm ci` is reproducible (D8).
- `tsconfig.json`: `target: ES2020`, `module: ES2020`, `strict: true`, `outDir: dist`.
- `npm run build` emits one `.js` per source module into `dist/` and copies `web/assets/` into it. The served set is therefore `dist/` less `dev.html`, plus `index.html` and `styles.css`, which stay at `web/` and are embedded from there (D4); `styles.css` is outside `dist/` so that `web_dir` mode picks up an edit without a rebuild. `index.html` loads `<script type="module" src="…main.js">` and the browser resolves the imports relative to it, which is why D4 serves the whole set under one prefix. `tsc` cannot concatenate ES modules, since `outFile` supports only `amd`, `system` and `none`, and no bundler is added (decision 7).
- `index.html` references its assets as the placeholder tokens `@@main.js@@` and `@@styles.css@@`, and the site's contact channel as `@@support_contact@@`; D4 substitutes all three at serve time, and its substitution is generic, so later steps add assets and tokens without touching Haskell.
- Non-TypeScript assets live in `web/assets/`. `npm run build` copies that directory into `dist/` alongside the compiled modules. D2 adds the logos there and E5 the QR encoder; D1 creates the directory with a `.gitkeep`, since git cannot commit an empty one and the copy step runs from D1 onwards.
- `npm run build` also emits `web/dist/dev.html`, a copy of `index.html` with `@@styles.css@@` resolved to `../styles.css`, every other file token to `./<name>`, and `@@support_contact@@` to a placeholder string. It is for design work before D4's listener exists. **Open it over a local static server, not from a `file://` path**: browsers refuse to load ES modules from an opaque origin, so a `file://` open renders a shell with no script. `python3 -m http.server` rooted at `apps/simplex-badge-service/web`, then `/dist/dev.html`, works and keeps `../styles.css` reachable. `dev.html` is never served by the service; D4 filters it out of the asset list.
- Commit `dist/`, and say so in a header comment in `index.html`.
- Do not add a bundler, a framework, or a CSS toolchain.

**Verify:** Manual: `npm ci && npm run build` from a clean checkout produces the `dist/` modules and `dist/dev.html`; `npx tsc --noEmit` is clean.

#### D2 — Design system and site wizard shell

**Files:** `apps/simplex-badge-service/web/{styles.css,index.html}`, `apps/simplex-badge-service/web/src/{ui.ts,main.ts}`, `apps/simplex-badge-service/web/assets/logo-symbol-{light,dark}.svg` (copied from `website/src/img/new/`)

**Do:** Match `https://snrc-testing.pages.dev/`: a single centred column, `max-width: 560px`, generous whitespace, a system font stack, and the SimpleX logo as the only image: `website/src/img/new/logo-symbol-light.svg` and `logo-symbol-dark.svg`, copied into `web/assets/` and from there into `dist/` by `npm run build` (D1), referenced as two `<img>` elements in `index.html` carrying the tokens `@@logo-symbol-light.svg@@` and `@@logo-symbol-dark.svg@@`. The tokens go in `index.html` and nowhere else, because D4 substitutes only there; `styles.css` is served verbatim, so a token written into a CSS rule would reach the browser as literal text. The `prefers-color-scheme` media query that redefines the colour tokens also toggles which of the two images is displayed. The reference site is the visual target, but the specifications in this step are sufficient on their own if it is unreachable.

- Colour tokens on `:root`, redefined under `@media (prefers-color-scheme: dark)`. Accent `#0053D0` light and `#70F0F9` dark, from `website/tailwind.config.js:11,14`. Every colour has a light definition on bare `:root`.
- Options render as bordered radio cards with a 2px accent border when selected, the same visual language as `PeriodCard` (`apps/multiplatform/common/.../views/badges/BadgesPayView.kt:136`), so the app and the site read as one product.
- Site wizard shell: one question per screen, hash-routed (`#/tier`, `#/months`, `#/pay`, `#/checkout`, `#/order` for the crypto payment screen (E5), `#/code` for the result screen (E6)), no page reloads, with back and forward working via `popstate`.
- Transitions: a 150 ms opacity fade, disabled under `prefers-reduced-motion`.
- Accessibility: real `<fieldset>`, `<legend>` and `<label>` radios, keyboard navigable, `:focus-visible` rings, exactly one `<h1>` per screen.
- `main.ts`, written as a placeholder in D1, imports the shell and starts it, so `dev.html` renders something. D3 and D5 extend the same module.
- A visible error banner for failed fetches. Never a silent blank screen.
- A support contact renders in the footer on every screen, from the `@@support_contact@@` token D4 substitutes (D1). It is the site's only contact channel, and D7, E5, E6 and F3 all direct users to it.

**Verify:** Manual: serve `apps/simplex-badge-service/web` with `python3 -m http.server` and open `/dist/dev.html`, whose screens carry hardcoded placeholder options at this step, and step through every screen with the keyboard only, reaching and activating every option. In both light and dark themes at 320 px width, confirm that no screen scrolls horizontally, that the selected option card's 2px accent border is visible, and that the `:focus-visible` ring is visible against both backgrounds.

#### D3 — Catalog fetch and the four site screens

**Files:** `apps/simplex-badge-service/web/src/{catalog.ts,steps.ts,main.ts}`

**Do:** Four screens, with this base-locale copy:

1. heading *Choose your level*: Supporter and Legend, each with its month price and perk line, built from `prices`.
2. heading *How long?*: 1, 3 and 12 months. The 3- and 12-month rows show the offer's `total` from the catalog payload, selected by the chosen tier's `priceId`; the 1-month row shows that price's `monthPrice`, since A4 seeds no 1-month offer and its checkout request carries no `offerId` (D6). The 3- and 12-month rows also show the saving against the undiscounted `months × monthPrice`; the 1-month row has no offer and shows no saving. **The browser computes no chargeable amount**: A4's `offerTotal` is the only implementation of a total, D4 serves its result, and the undiscounted figure is a display-only comparison never sent to `/api/checkout`, so the price shown and the price charged cannot drift.
3. heading *How would you like to pay?*: Card, Bitcoin, Monero.
4. `#/checkout`: a summary of tier, months, total and method, with a Pay button, inert until D7 wires it.

- Rendering a minor-unit amount is integer formatting, not arithmetic on a price: divide by 100, pad the remainder to two digits, and prefix the symbol for `currency` (`usd` → `$`), rendering an unknown currency as its ISO code before the digits. `catalog.ts` holds the one formatter; no other module formats money.
- The perk line (2 GB and 5 GB files) is a constant in `catalog.ts` keyed by `badgeType`. It is not in the catalog payload, so changing a perk requires a site rebuild; H6 records that.
- A tier or duration with no `active` price renders **disabled**, not hidden (UX §2.1). A `deprecated` price or offer is never offered as a fresh choice but is honoured when it arrives as an explicit `?tier=` or `?months=` parameter, or on a resumed order. This matches D6, which accepts `deprecated` and rejects `disabled`.

**Verify:** Manual: with a fixture catalog all four screens render, removing the legend price disables that card, and the summary shows the correct total.

#### D4 — Warp listener, asset embedding, routing

**Files:** `apps/simplex-badge-service/src/BadgeService/Web/{Server.hs,Assets.hs}`, `apps/simplex-badge-service/src/BadgeService/Config.hs`, `apps/simplex-badge-service/src/BadgeService/Service.hs`, `simplex-chat.cabal`, `tests/Bots/BadgeServiceTests.hs`

**Do:**

- Warp listener on `[web] port`, bound to `[web] host`, which defaults to `127.0.0.1` so a default deployment is not exposed without a reverse proxy. Use `runSettings` with `setPort` and `setHost` from `[web] port` and `[web] host`. `Warp.run` takes a port alone and cannot bind a configured host, so it is not used. `tests/NameResolver.hs:38` shows the in-repo `Application` shape, but it uses `withApplication` on a free port; that pattern belongs to E1's mock, not to a configured listener. Configuration errors in `[web]`, including its absence, are A6's rules and are enforced there.
- Run the listener alongside the bot with `raceAny_`. `badgeServiceCLI` already does this (`Service.hs:86`), but it runs only under `--run-cli` (`Main.hs:12`). The default entry point is `badgeService` (`Service.hs:55-69`), whose `simplexChatCore … forever` loop must also be raced against the web listener, or the site will not run in production.
- `Assets.hs`: `embedDir` of `web/dist/`, plus `embedFile` of `web/index.html` and `web/styles.css`, following `Operators.hs:70`. Compute **one** SHA-256 over the whole served set at startup, the file names and bytes in sorted order, and serve every asset under that single prefix at `/assets/<buildHash>/<name>`, with `Cache-Control: public, max-age=31536000, immutable`. The hash must be per build, not per file: `tsc` does not rewrite import specifiers, so `main.js` resolves `./catalog.js` against its own directory, and a per-file hash would put every sibling module at a different prefix and 404 the whole graph. One prefix changes on any change to any asset, which is the same cache-busting property.
- `Server.hs` substitutes tokens in `index.html` on the way out, in both embedded and `web_dir` modes; `index.html` is `no-cache`. Substitution is **generic**: `@@<name>@@` resolves to `/assets/<buildHash>/<name>` for any `<name>` present in the served set, so a later step that adds an asset adds a token and nothing else. The single exception is `@@support_contact@@`, which is not a file and comes from `[web] support_contact`. A token naming a file that is not in the served set fails at startup rather than serving a broken page. `dist/dev.html` is removed from the served set by an ordinary `filter` over `embedDir`'s `[(FilePath, ByteString)]`; `embedDir` takes no predicate, so its bytes stay in the binary. It is never routed.
- `[web] web_dir` (decision 2) serves from disk instead, for front-end iteration. In that mode the build hash is recomputed per request and every asset is served `Cache-Control: no-store`, so an edited file is visible on reload; the immutable long-cache belongs to the embedded mode alone. Without this an edited `styles.css` would keep the same URL under an `immutable` response and the browser would not re-fetch it until the service restarted. Development only: document it as such and refuse paths outside the given directory.
- Extend `withBadgeService` (A6) to bind a free port with `Warp.openFreePort`, write `[web]` into the temporary ini with that `port`, `base_url = http://127.0.0.1:<port>` and a placeholder `support_contact`, and pass the base URL to the test body. Without this every later HTTP assertion has no listener to reach, and A6's rule that `[web]` is required whenever a provider is configured would make E2's and F1's harness edits fail startup.
- `[web] base_url` is validated at startup as an absolute URL. It must be `https` unless the host is `localhost` or `127.0.0.1`, which is permitted for the local mock stack (§10).
- Routes: `GET /` returns the index; `GET /assets/*`; `GET /api/catalog` returns the `BadgeCatalog` value in the RPC encoding (A2), including the offer `total` that A2 adds to `BadgeOffer`, so the site and the app parse exactly the same shape. Filtered to `active` and `deprecated`:

  ```
  { prices: [{ priceId, badgeType, monthPrice, currency, status, createdAt }],
    offers: [{ offerId, priceId, months, discount: {type:"freeMonths"|"discount", …},
               total, status, createdAt }] }
  ```

  The payload comes from B1's `getActiveCatalog` with A4's `catalogTotals` applied, never from `Catalog.hs`'s defaults, so a price deprecated or disabled in the database is reflected without a rebuild.

- Security headers on every response: `Content-Security-Policy: default-src 'self'`, `X-Content-Type-Options: nosniff`, `Referrer-Policy: no-referrer`, `X-Frame-Options: DENY`. The site loads no cross-origin resource, so `default-src 'self'` blocks nothing it needs.

**Verify:** Manual: the service serves `index.html` with every `@@…@@` token substituted, including `@@support_contact@@` from the ini, and the module graph loading without a console error; `GET /dev.html` and `GET /assets/<hash>/dev.html` both return 404; `curl -I` shows the headers and the immutable asset caching; `curl /api/catalog` matches the seeded catalog; `web_dir` picks up an edited CSS file without a rebuild. The site wizard itself is reviewable after D3.

#### D5 — URL prefill

**Files:** `apps/simplex-badge-service/web/src/{params.ts,main.ts}`

**Do:** Read `?tier=supporter|legend&months=1|3|12&pay=card|btc|xmr`. Each prefilled answer skips its screen. An unknown, disabled or unpriced value is ignored and its screen is asked; never fail on a bad parameter, because the app may be older than the catalog. With no parameters, start at screen 1.

Resuming an existing order from `?order=` belongs to E5, which owns the polling loop.

**Verify:** Manual: `?tier=legend&months=12&pay=xmr` lands directly on the `#/checkout` summary with the right total; `?tier=nonsense` starts at screen 1; `?months=12` alone asks tier and method only.

#### D6 — `POST /api/checkout`, provider interface, order creation

**Files:** `apps/simplex-badge-service/src/BadgeService/Web/Server.hs`, `apps/simplex-badge-service/src/BadgeService/Orders.hs`, `simplex-chat.cabal`, `tests/Bots/BadgeServiceTests.hs`

**Do:** The shared order-creation endpoint, provider-agnostic. It lives here rather than in Phase E because it serves all three methods and Stripe reuses it unchanged.

```
POST /api/checkout   { priceId, offerId?, method: "card"|"btc"|"xmr" }
  200 { orderId, method, badgeType, months, amount, currency, expiresAt, shortRef,
        payUrl? | address?, cryptoAmount?, cryptoCurrency? }
  400 { error: "price_disabled"|"offer_disabled"|"offer_mismatch"|"bad_request" }
  503 { error: "provider_unavailable" }
```

- `Orders.hs` defines the provider interface used by E2 and F1: `data Method = MCard | MBtc | MXmr`; `data OrderDraft` carrying badge type, months, amount, currency and `shortRef`; `data ProviderInvoice` carrying the provider's invoice id, `payUrl`, address, crypto amount and expiry; and `data ProviderError = PENetwork Text | PEStatus Int Text | PEDecode Text`, all of which map to `provider_unavailable` at the endpoint and are logged with their detail (H4). It also defines `readProviderInvoice :: Method -> Text -> IO (Either ProviderError ProviderStatus)`, keyed on `provider_ref`, where `data ProviderStatus = PSSettled CurrencyAmount UTCTime | PSPending CurrencyAmount | PSOpen | PSExpired CurrencyAmount | PSFailed`; H3 needs it to re-read a stuck order. Until E2 and F1 land, every branch of both functions returns `Left (PEStatus 503 "not configured")`, which the endpoint maps to `provider_unavailable`.
- Badge type and months are derived server-side: `priceId` gives the badge type, `offerId` gives the months, and a request with no `offerId` is exactly one month. The browser never states either, so a tampered request cannot buy a legend badge at a supporter price.
- `amount` comes from A4's `offerTotal`; the browser's figure is never trusted. `cryptoCurrency` is derived from `method`; `@invoices.payment_crypto_currency` stays NULL (A3).
- Price and offer status are checked here and only here: `deprecated` is accepted, `disabled` is rejected (RPC §Catalog).
- `orderId` is 128 random bits, base64url. It is a bearer capability for the code (A3, decision 9), so it must not be sequential or derived from anything guessable.
- `shortRef` is 5 characters from B3's Crockford alphabet, encoded with B3's encoder, drawn from a CSPRNG, unique per order, stored on `@web_orders`. On a unique-constraint violation the generator retries up to 10 times before failing the checkout request with `bad_request` and logging it (H4). The 32⁵ space is adequate for this plan's order volume; H5 records widening `shortRef` as the remedy if retries become frequent.
- The provider call goes through `createProviderInvoice :: Method -> OrderDraft -> IO (Either ProviderError ProviderInvoice)`, dispatching on method. A method whose provider section is absent from the ini is rejected the same way.
- On a successful provider call, write an `@invoices` row and a `web_orders` row with `status = 'invoiced'` and `provider_ref` set from `ProviderInvoice`, in one transaction.

**Verify:** In `tests/Bots/BadgeServiceTests.hs`: a disabled price, a disabled offer and an offer pinned to a different price are rejected before any provider call with `price_disabled`, `offer_disabled` and `offer_mismatch` respectively, the disabled rows produced with B1's `setPriceStatus` and `setOfferStatus`. An unrecognised extra key such as `months` or `amount` in the request body is ignored rather than honoured, since the request carries neither. At this step every method returns `provider_unavailable` and no order row is written, whether or not a provider section is present. No checkout can succeed until E2, so the assertions that need a written order, the `orderId` and `shortRef` uniqueness and the charged amounts, are in E2's Verify.

#### D7 — Pay button and checkout error states

**Files:** `apps/simplex-badge-service/web/src/{checkout.ts,steps.ts}`

**Do:** Wire D3's `#/checkout` Pay button to D6.

- The button POSTs `{priceId, offerId?, method}`, then redirects to `payUrl` for card and routes to `#/order` otherwise. Until E5 renders that screen, `#/order` shows the order id, address and amount from the checkout response as plain text, so no path lands on an unrouted hash.
- Error rendering, never a blank screen (D2): `provider_unavailable` as "this payment method is temporarily unavailable, try another"; `price_disabled`, `offer_disabled` and `offer_mismatch` as "these prices have changed, start again", returning to screen 1 with a refreshed catalog; `bad_request` as a generic failure with the support contact.
- Disable the button while a request is in flight, so a double click cannot create two orders.

**Verify:** Manual against a service with no provider section: pressing Pay shows the unavailable message; a disabled price shows the restart message; a double click sends one `POST /api/checkout`, counted in the browser's network panel, since no order row can be written until E2.

#### D8 — CI check for the committed web build

**Files:** `.github/workflows/build.yml`

**Do:** `web/dist/` is committed and the Haskell build embeds it, so a change to `web/src` or `web/assets` without a rebuild ships stale JavaScript or a stale asset silently, with a green build and green tests. `.github/workflows/build.yml` already triggers on `apps/simplex-badge-service/**` but has no node step.

Add a job gated on `apps/simplex-badge-service/web/**` that runs `npm ci && npm run build` and fails if `git diff --exit-code apps/simplex-badge-service/web/dist` is non-empty.

**Verify:** Manual: edit a `.ts` file without rebuilding and confirm the job fails; rebuild and confirm it passes.

---

### Phase E — BTCPay: Bitcoin and Monero

Phase E ends with a Bitcoin or Monero purchase that yields a redeemable code end to end against E1's mock. Stripe then reuses D6 and E3's `settleOrder` unchanged.

#### E1 — Provider mock harness

**Files:** `apps/badge-payment-mock/src/BadgePaymentMock.hs`, `apps/badge-payment-mock/Main.hs`, `tests/Bots/BadgePaymentMockTests.hs`, `simplex-chat.cabal`, `tests/Test.hs`

**Do:** A Warp application with a mutable registry, following the `Application` shape at `tests/NameResolver.hs:40`. Tests drive the responses it returns and push webhooks at the service. It lands before E2 and F1 so their Verify lines are runnable when those steps are done, and it covers both providers' request shapes from the start so F1 need not reopen it.

- **Library, not a test module.** The implementation lives in `apps/badge-payment-mock/src/BadgePaymentMock.hs`. A module under `tests/` is invisible to an executable, because `tests` is a `hs-source-dirs` of the test suite alone.
- Add an `executable badge-payment-mock` stanza with `hs-source-dirs: apps/badge-payment-mock, apps/badge-payment-mock/src` and `other-modules: BadgePaymentMock`, following `executable simplex-broadcast-bot`. It opens with `if flag(client_library)` / `buildable: False`, as every other executable in the file does, or `--flags=client_library` breaks. It declares its own `warp`, `wai`, `http-types`, `aeson`, `bytestring`, `crypton`, `memory`, `optparse-applicative` and `text` deps; `build-depends` are not transitive from A5.
- Add `apps/badge-payment-mock/src` to the test stanza's `hs-source-dirs` and `BadgePaymentMock` to its `other-modules`, so the tests drive the same implementation. Register `Bots.BadgePaymentMockTests` under the `Supporter badges` path (§4 rule 8).
- In tests it binds a free port via `Warp.withApplication`. Standalone it takes a fixed port:

  ```
  cabal run badge-payment-mock -- --port 9000 --service-url http://localhost:8080 \
    --btcpay-secret-file ./btcpay.hmac --stripe-secret-file ./stripe.hmac
  ```

- `GET /_invoices` lists the invoices it has created, with the provider and the `orderId` metadata. An operator has no other way to learn an invoice id.
- It serves both providers' read endpoints as well as their create endpoints: `GET /api/v1/stores/{storeId}/invoices/{id}` and `…/payment-methods` for BTCPay, `POST /v1/checkout/sessions` and `GET /v1/checkout/sessions/{id}` for Stripe, so E2's and F1's `readProviderInvoice` have something to read.
- `POST /_settle/:invoiceId` looks the invoice up, selects the provider that created it, and POSTs the matching settlement event to `{service-url}/webhooks/btcpay` or `{service-url}/webhooks/stripe`, signed with that provider's secret. `POST /_settle/:invoiceId?webhook=false` marks the invoice settled in the registry and delivers nothing, which is how H3's reconciliation pass is exercised.
- `POST /_event/:invoiceId {type, amount?}` sets the invoice's registry state and delivers the matching provider event, signed the same way. It covers every non-settlement transition the later steps drive: BTCPay `InvoiceProcessing` with a partial amount, `InvoiceExpired` with an amount received, and `InvoiceInvalid`; Stripe `checkout.session.expired`, `charge.refunded`, `charge.dispute.created` and `payment_intent.payment_failed`. `?webhook=false` applies here too, changing only what the read endpoints report, which is what H3's expiry case needs. Without this endpoint E5, E7, F4, F5 and H3 have no way to produce the states they assert.
- Every delivered event carries a **deterministic** id, `"{invoiceId}-{eventType}"` by default, and `/_invoices` reports the ids delivered so far per invoice. Re-posting the same control request therefore re-delivers the same event id, which is what E7's and F5's replay assertions need, and a test can compute the id in advance, which is what E7's unprocessed-row assertion needs. Dedup is keyed on `(provider, event_id)` (A3), so a fresh id per delivery would exercise nothing. A control request may pass `seq: n`, giving `"{invoiceId}-{eventType}-{n}"`, which is how a test delivers two distinct events of the same type on one invoice: a second larger `InvoiceProcessing` in a multi-transaction payment (E3), or a second partial `charge.refunded` (F4). Without it a same-type repeat is deduplicated away and `amount_paid` never advances.
- `POST /_fail {calls, status}` makes the next `calls` create or read requests answer with `status`, which is how E2's and H3's provider-error cases are driven. It resets itself once the count is spent.
- Every control endpoint is prefixed `_` and is absent from the providers' real APIs, so a test cannot reach one by accident through the client under test.

**Verify:** In `tests/Bots/BadgePaymentMockTests.hs`: a test starts the mock on a free port, issues a request and asserts the mock recorded it; `POST /_settle/:invoiceId` delivers a correctly signed body to a caller-supplied URL, and `?webhook=false` changes the state the read endpoint reports while delivering nothing; `POST /_event/:invoiceId` delivers each of the seven event types with the amount it was given, and twice with the same event id; `POST /_fail` makes exactly the requested number of calls fail and then stops. Webhook delivery into the service is asserted in E7 and F5.

#### E2 — BTCPay client

**Files:** `apps/simplex-badge-service/src/BadgeService/Providers/BTCPay.hs`, `apps/simplex-badge-service/src/BadgeService/Config.hs`, `simplex-chat.cabal`, `tests/Bots/BadgeServiceTests.hs`

**Do:** A client for the BTCPay Greenfield API over `http-client-tls`, configured from `[btcpay]` (A6): `url`, `store_id`, `api_key_file`, `webhook_secret_file`, required as a group.

- `createInvoice`: `POST /api/v1/stores/{storeId}/invoices` with the fiat amount and currency, `metadata.orderId`, the expiry window from `[btcpay] btc_expiry_minutes` or `xmr_expiry_minutes` by method, and `checkout.paymentMethods = ["BTC", <xmr_method_id>]`. The Monero method id is the ini value, defaulting to `XMR-CHAIN` (A6); nothing is hardcoded, so a Greenfield instance that names it differently is a configuration change. Log the method ids the store reports at startup, so a mismatch is visible in the first line of the log rather than in a failed checkout.
- `getPaymentMethods`: `GET /api/v1/stores/{storeId}/invoices/{id}/payment-methods`, for the per-method address and crypto amount.
- `createProviderInvoice` for `btc` and `xmr` calls `createInvoice` then `getPaymentMethods`, and returns the provider invoice id, address and crypto amount for the requested method inside `ProviderInvoice`. D6 writes the address and crypto amount onto the `@invoices` row and the invoice id onto `web_orders.provider_ref`, in its single transaction, so `GET /api/order/:orderId` (E4) serves them from the database and never calls the provider on a poll. A `getPaymentMethods` failure after a successful `createInvoice` is a `ProviderError` surfacing as `provider_unavailable`, and no order row is written.
- `readProviderInvoice` for `btc` and `xmr`: `GET /api/v1/stores/{storeId}/invoices/{id}`, mapping the invoice state to `ProviderStatus` for H3.
- A network failure or a non-2xx response becomes a `ProviderError`. No exception escapes to the handler.

**Verify:** In `tests/Bots/BadgeServiceTests.hs`, against E1's mock: invoice creation returns an address and a crypto amount for both methods, and a 500 from the provider yields the typed error and no order row. This is the first step at which a checkout writes an order, so it also carries D6's deferred assertions: two checkout requests yield unrelated `orderId`s and distinct `shortRef`s, and the endpoint's `amount` matches the literal expected minor-unit totals, supporter 700 / 1400 / 4200 and legend 7000 / 14000 / 42000 for 1, 3 and 12 months.

#### E3 — BTCPay webhook, settlement, code creation

**Files:** `apps/simplex-badge-service/src/BadgeService/Web/Server.hs`, `apps/simplex-badge-service/src/BadgeService/Orders.hs`, `apps/simplex-badge-service/src/BadgeService/Providers/BTCPay.hs`

**Do:** `POST /webhooks/btcpay`:

1. Verify the `BTCPay-Sig` HMAC-SHA256 over the **raw body**, captured before any JSON parsing. A bad signature is a 400 with no detail.
2. `recordProviderEvent` before processing. A duplicate whose `processed_at` is set returns 200 and stops (MVP §4: "Every inbound event is recorded in `provider_events` before processing"); a duplicate whose `processed_at` is NULL is reprocessed, since the previous attempt did not complete. Set `processed_at` in the same transaction as the settlement write.
3. `InvoiceSettled`: resolve the order with `getOrderByProviderRef` on the webhook's `invoiceId`, since the webhook body does not carry `metadata.orderId`; an `invoiceId` with no order is logged and answered 200. Then derive the code (B3), store its hash in `codes` with `batch = 'web'`, the order's badge type and months, and `expires_at = settled_at + [codes] default_expiry_days`; set `settled_at` and `status = 'paid'` on the order. All in one transaction. No column links the code row to the order (§3 Linkage).
4. `InvoiceProcessing` sets `pending`. `InvoiceExpired` sets `expired`. `InvoiceInvalid` sets `failed`: BTCPay has declared the invoice unpayable. A **partial payment leaves the order `pending`**: partials are the normal first event of a multi-transaction payment and are superseded by `InvoiceSettled`. An invoice that expires underpaid stays `expired` with `amount_paid` recorded, which E5's crypto payment screen renders as the amount received plus the support contact.

The settlement transaction of point 3 is `settleOrder`, in `Orders.hs`, taking the order, the settled amount and the time, so H3 and F2 call the same writer rather than repeating it.

Settlement is idempotent and monotonic toward `paid`: `InvoiceSettled` moves `invoiced`, `pending`, `expired` and `failed` to `paid` and writes the code row, and a second `InvoiceSettled` on a `paid` order changes nothing and writes no second code row. Only `paid` is terminal; late settlement after expiry is routine on-chain and must succeed.

**Verify:** covered by E7.

#### E4 — `GET /api/order/:orderId` and the disclosure rule

**Files:** `apps/simplex-badge-service/src/BadgeService/Web/Server.hs`, `apps/simplex-badge-service/src/BadgeService/Config.hs`, `tests/Bots/BadgeServiceTests.hs`

**Do:**

```
GET /api/order/:orderId
  200 { status: "invoiced"|"pending"|"paid"|"expired"|"failed", method: "card"|"btc"|"xmr",
        badgeType, months, amount, currency, expiresAt, shortRef,
        amountPaid?, address?, cryptoAmount?, cryptoCurrency?,
        code?, disclosureExpiresAt?  }   -- see the disclosure rule below
  404 { error: "not_found" }   -- D6's error body shape. Nothing about the order is echoed,
                               -- so an unknown orderId is indistinguishable from a guess
```

- **Code disclosure rule.** `code` is recomputed from `orderId` via `deriveOrderCode` (decision 9), so a reload works with no plaintext at rest. It is returned **only** while the code is unredeemed, not revoked, and within 30 days of `max(settled_at, unredeemed_at)`; once any of the three fails, the response carries the status without `code`. Revocation is included because F4 revokes an unredeemed code on a refund or dispute, and disclosing a code `purchaseBadge` will reject as `code_invalid` helps nobody; H2 refuses `--reveal` on the same ground.
- Sources: `settled_at` comes from the `web_orders` row the endpoint already loads by `orderId`. Redemption state comes from `codes.redeemed_at`, `codes.unredeemed_at` and `codes.revoked_at`, found by deriving the code and hashing it. No join between the two rows is needed or exists (§3 Linkage).
- `unredeemCode` (B1) clears `redeemed_at` and sets `unredeemed_at = now`, which both re-enables redemption and reopens this window for a further 30 days. H2 exposes it to operators.
- The window is `codeDisclosureDays = 30`, a named constant in `Config.hs` beside the ini parser and deliberately **not** an ini key: shortening it strands paid users and lengthening it widens the `orderId` capability, so it is a code change with a review rather than a deployment knob. The response carries `disclosureExpiresAt` alongside `code`, and E6 renders that date rather than repeating the number in copy.
- Without this rule `orderId` would be permanently equivalent to the code: anyone holding it can read the code, with no further authentication.
- `method` comes from the `web_orders` row; E5 and E6 branch on it rather than on the presence of `address`.
- `amountPaid` is in the fiat invoice currency, not in crypto. `cryptoCurrency` is derived, not read: `BTC` for `method = 'btc'`, `XMR` for `method = 'xmr'`, absent for `card`. `@invoices.payment_crypto_currency` exists but is left NULL; `method` is the single source (A3).

**Verify:** In `tests/Bots/BadgeServiceTests.hs`: a settled order returns its code; after `markCodeRedeemed` writes `redeemed_at` the response omits `code`; after `unredeemCode` sets `unredeemed_at` it returns it again; an order settled 31 days ago and unredeemed omits it; a revoked code omits it; an unknown `orderId` returns 404 with `{"error":"not_found"}`.

#### E5 — Order resume, crypto payment screen, QR

**Files:** `apps/simplex-badge-service/web/src/{checkout.ts,qr.ts,main.ts}`, `apps/simplex-badge-service/web/assets/qrcode.js` (copied from `website/src/js/`), `apps/simplex-badge-service/web/index.html`

**Do:**

- Order resume: read `?order=<orderId>` and skip straight to polling, then to `#/order` or `#/code` depending on status. This is where F3's Stripe return lands and where any reload recovers.
- Crypto payment screen (`#/order`): the fiat amount first, then the crypto amount, the address with a copy button, a QR of the payment URI, and a countdown to `expiresAt`. States, mapped from E4's `status`, all five of them: `invoiced` → waiting, `pending` → processing, `paid` → paid, which is UX §2.1's *settled*. `expired` and `failed` both offer a new invoice, which creates a new order; `failed` additionally says the provider rejected the invoice, so retrying with the same method may fail again.
- `expired` with a non-zero `amountPaid` shows the amount received in the fiat invoice currency, the `shortRef`, and the support contact, and tells the user to quote the `shortRef`. Never the `?order=` URL and never the code. The crypto shortfall is not meaningful once the invoice rate has expired.
- Poll with backoff, 2 s for the first minute then 10 s, and stop polling while the tab is hidden. A `429` response backs off to its `Retry-After` value rather than being treated as a fetch failure (H1).
- QR: reuse the vendored `website/src/js/qrcode.js`. Do not write a new encoder. Copy it to `web/assets/qrcode.js`, which `npm run build` already copies into `dist/` (D1), so D8's check stays reproducible, and load it from `index.html` with a `@@qrcode.js@@` token. D4's substitution is generic over the served set, so no Haskell change is needed. It is a minified bundle that assigns a global `QRCode` with `create`, `toCanvas`, `toDataURL` and `toString`, not an ES module, so `qr.ts` declares `declare const QRCode: { toString(text: string, opts?: object): Promise<string> }` and wraps it; `toString` returns SVG markup, which the page inlines. No CDN; the CSP forbids it.

**Verify:** Manual against E1's mock: an XMR order shows an address and settles; the countdown expires and offers a new invoice, and so does a `failed` order; reloading mid-payment restores the same screen from `?order=`; an underpaid expiry shows the amount received and the support contact.

#### E6 — Result screen

**Files:** `apps/simplex-badge-service/web/src/{result.ts,main.ts}`

**Do:** Show the code in a large monospace block with a copy button and a confirmation, a QR of the code for transferring it to a phone, the badge type and months, and the redeem instructions ("SimpleX → Settings → Supporter perks → Redeem code").

Warn that this is the only place the code is shown, and that the `?order=` URL below returns the code until the date shown beside it (`disclosureExpiresAt`, E4) and only until it is redeemed, so the code should be redeemed or saved before then. State that this URL is equivalent to the code and must be treated as such. After that date, or if the link is lost, support can recover the code from the `shortRef`, which appears on a card statement and is shown on this screen for a crypto order.

**Verify:** Manual: the code copies, the QR scans, and the result screen restores from a bookmarked `?order=` URL. The screen shows the badge type, the months, the `shortRef`, the redeem instructions, and the warning naming the `disclosureExpiresAt` date E4 returned.

#### E7 — BTCPay scenario tests

**Files:** `tests/Bots/BadgeServiceTests.hs`

**Do:** Using E1's mock: settle and get a code that redeems through B7; expire; settle after expiry and confirm the order becomes `paid` and the code row is written; replay a processed webhook and confirm no second code; insert a `@provider_events` row with a NULL `processed_at` for the event about to be delivered, then deliver it, and confirm it is reprocessed and settles (D0's `recordProviderEvent` rule); partial payment leaves the order `pending`; an underpaid expiry records `amount_paid`; `InvoiceInvalid` sets `failed`; a provider 500 at checkout gives `provider_unavailable` and writes no order row; a bad `BTCPay-Sig` gives 400.

**Verify:** `cabal test --test-options='-m "Badge service"'` passes and the run reports the new examples. Run locally.

---

### Phase F — Stripe

Phase F adds card payment on top of E3's settlement path and is the last phase before §10's end-to-end script runs.

#### F1 — Stripe client and Checkout Session

**Files:** `apps/simplex-badge-service/src/BadgeService/Providers/Stripe.hs`, `apps/simplex-badge-service/src/BadgeService/Config.hs`, `simplex-chat.cabal`, `tests/Bots/BadgeServiceTests.hs`

**Do:** Configured from `[stripe]` (A6): `secret_key_file`, `webhook_secret_file`, required as a group.

- `POST /v1/checkout/sessions` with `mode=payment` and `client_reference_id = orderId`. The session id is returned as `ProviderInvoice`'s invoice id and written to `web_orders.provider_ref` by D6.
- `success_url = {base_url}/?order={orderId}`.
- `cancel_url = {base_url}/#/pay`, with **no** order reference. A cancelled session must not leave a live capability URL in the user's history, and must not land on the polling screen, where it would wait forever for a webhook that is never coming.
- `statement_descriptor_suffix = "SIMPLEX <shortRef>"`, using the `shortRef` D6 generated (UX §7 item 6), so support can identify a payment from a bank statement.
- Wire the `card` branch of D6's `createProviderInvoice`, returning `payUrl`, and `readProviderInvoice` for `card` via `GET /v1/checkout/sessions/{id}`, mapping the session state to `ProviderStatus` for H3. `provider_ref` holds the session id until `checkout.session.completed` replaces it with the payment intent (F2), which happens only as the order becomes `paid`; H3 reads only `invoiced` and `pending` orders, so it always resolves a session id.

**Verify:** In `tests/Bots/BadgeServiceTests.hs`, against E1's mock: a card checkout returns a `payUrl` and writes the order with its `shortRef` and `provider_ref`.

#### F2 — Stripe webhook and signature verification

**Files:** `apps/simplex-badge-service/src/BadgeService/Providers/Stripe.hs`, `apps/simplex-badge-service/src/BadgeService/Web/Server.hs`, `tests/Bots/BadgeServiceTests.hs`

**Do:** `POST /webhooks/stripe`. Verify `Stripe-Signature` over the **raw body**: parse the `t=` and `v1=` parts, recompute `HMAC-SHA256(secret, "{t}.{body}")`, compare in constant time, and reject a timestamp outside a 5-minute tolerance. Dedup via `provider_events` before processing, with E3's unprocessed-retry rule.

- `checkout.session.completed` takes the same settlement path as E3, keyed by `client_reference_id`. It also writes the Checkout Session's `payment_intent` to `web_orders.provider_ref`, replacing the session id. `charge.refunded` and `charge.dispute.created` carry a payment intent rather than a `client_reference_id`, so they resolve to the order via `getOrderByProviderRef`.
- `checkout.session.expired` sets the order `expired`. It is not terminal: late settlement still moves an order to `paid` (E3).
- `payment_intent.payment_failed` is **not** acted on: it carries neither a `client_reference_id` nor, before `checkout.session.completed` fires, a `provider_ref` this service has stored, so it cannot be resolved to an order. A failed payment leaves the session open and `checkout.session.expired` follows.
- Any other event type, `payment_intent.payment_failed` included, is recorded by `recordProviderEvent`, marked processed, logged at info with its type and event id, and answered 200. The dedup row is written so a Stripe retry does not re-log it.

**Verify:** In `tests/Bots/BadgeServiceTests.hs`: unit tests for a valid signature, a wrong signature, a stale timestamp, and a body altered after signing; `checkout.session.completed` leaves `provider_ref` holding the payment intent.

#### F3 — Card return-URL resume

**Files:** `apps/simplex-badge-service/web/src/{main.ts,checkout.ts}`

**Do:** The card path leaves the site, so the return lands on `/?order=…`, which E5 resumes: poll, then show the code, or a "still processing" state if the webhook has not yet arrived. The webhook settles an order, not the redirect; never treat the return as proof of payment.

The polling screen gives up after 15 minutes and shows a "payment not received, check your bank or start again" state with the support contact, rather than polling indefinitely.

**Verify:** Manual: returning before the webhook shows "processing" and flips to the code once it lands; a cancelled Checkout Session returns to the payment-method screen with no order reference.

#### F4 — Refunds and disputes

**Files:** `apps/simplex-badge-service/src/BadgeService/Web/Server.hs`, `tests/Bots/BadgeServiceTests.hs`

**Do:** On `charge.refunded` and `charge.dispute.created`, resolve the charge to its order with `getOrderByProviderRef` on the payment intent (D0, F2), then derive the code from that `orderId` and look up its row by hash. A charge with no matching order is logged and ignored, not an error.

- If the code is unredeemed, set `revoked_at`.
- If it is already redeemed, append `debit(refund)` on that purchase's ledger (B2, UX §3 Adjustments).

A credential already issued cannot be recalled, so it stays valid until its expiry. A refunded user therefore keeps a valid badge for at most one further month; that cost is accepted rather than adding credential revocation.

BTCPay refunds are operator-initiated at the provider and have no webhook; the operator revokes or adjusts with H2's tooling. There is no automated crypto refund path (§6).

**Verify:** In `tests/Bots/BadgeServiceTests.hs`: an unredeemed code stops working; a redeemed one keeps its current credential, has a zeroed balance, and issues nothing further.

#### F5 — Stripe scenario tests

**Files:** `tests/Bots/BadgeServiceTests.hs`

**Do:** Using E1's mock: settle and get a code; replay and confirm no second code; refund an unredeemed code and confirm revocation; dispute after redemption and confirm `debit(refund)`; `checkout.session.expired` sets `expired` and a later settlement still reaches `paid`; a `payment_intent.payment_failed` event is logged and ignored; confirm the cancelled-session URL carries no order reference.

**Verify:** `cabal test --test-options='-m "Badge service"'` passes and the run reports the new examples. Run locally.

---

### Phase G — Clients

Phase G ends with every platform pricing from `CRBadgeCatalog`, redeeming codes in the app, and desktop and Android `foss` handing off to the site; no store purchase action remains on any platform.

Kotlin lives in `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/badges/` and Swift in `apps/ios/Shared/Views/Badges/`.

G0 has no dependencies and is taken before A1 (§4 rule 2): until it lands, the store builds still charge users for a badge they will not receive. It also points every platform's app wizard at the redemption entry point, which G2 and G3 then fill in. G1 needs only C2's badge-state response, which carries the site URL, plus D5, which defines the parameter grammar of the URL G1 opens. G2 and G3 start once C4 and B8 are done: B8 mints the code their manual checks redeem. G4 and G5 additionally need D3, whose site their Verify lines compare against, and the `APIGetBadgeState` wrappers added by G1 and G3, which their open-and-focus calls use. G4 also needs G2, and G5 needs G3's redeem view, because their month-boundary check needs a badge already redeemed on that platform. G1's hand-off is verified at the URL it opens. The checkout behind that URL completes for BTC and XMR once Phase E is done and for card once Phase F is done, and §10 exercises the whole chain after F5.

The "Redeem code" entry point already exists on both platforms (`BadgesSupportSimplexView.kt:77-82`, `BadgesSupportSimplexView.swift:108-126`); no step adds a second one.

#### G0 — Remove the store purchase action

**Files:** `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/badges/{BadgesPayView.kt,BadgeStore.kt}`, `apps/ios/Shared/Views/Badges/{BadgesPayView.swift,BadgeStore.swift}`

**Do:** Decision 6. Store evidence is not verified and a store purchase yields no badge (§6).

- Kotlin: remove the `purchase(...)` call (`BadgesPayView.kt:188,192-217`) and `showPurchasedAlert` (`:220-247`), which surfaces raw Play receipt fields and copies the purchase token to the clipboard.
- Swift: remove the StoreKit `purchase()` call site (`BadgesPayView.swift:174,182-218`) and `showPurchasedAlert` (`:221-249`).
- **Replace the Pay button in this step** with "Redeem code", routing to the existing entry point, on both platforms and every flavor. The duration screen then ends at the redemption entry point rather than at a charge that yields nothing. That view is a title-only stub until G2 and G3, so between this step and those the app wizard ends on an empty screen. No build from that window is released: the tree is releasable again once both redeem views exist. If a release is needed sooner, revert this step's button change alone; the removals of `purchase(...)` and `showPurchasedAlert` stay. G1 replaces the button with "Continue in browser" on desktop and `foss` only.
- `BadgeStore.kt` and `BadgeStore.swift` stay in place. After this step they are still referenced, for the price and savings text on the tier and duration screens (`BadgesPayView.kt:76,160,174`, `BadgesYourLevelView.kt:60,138`, `BadgesSupportSimplexView.kt:37`, and the Swift equivalents `BadgesPayView.swift:65,126,144,167`, `BadgesYourLevelView.swift:50,101,121`, `BadgesSupportSimplexView.swift:66`); only their purchase and receipt paths lose a caller, and those are kept because store-evidence verification will reuse them (§6). Each gains a header comment naming this plan as the reason its purchase and receipt paths are kept without a caller. G4 and G5 remove the last price callers.

**Verify:** Manual on desktop, the `google` flavor and iOS, since the change is in `commonMain` and affects every flavor: no store charge can be initiated, no raw store tokens are reachable in the UI, and the duration screen offers "Redeem code".

#### G1 — Kotlin: payment-method screen and browser hand-off

**Files:** `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/badges/{BadgesPayView.kt,BadgeWebCheckout.kt}`, `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/{platform/Platform.kt,model/SimpleXAPI.kt}`, `apps/multiplatform/android/src/foss/java/chat/simplex/app/PlayStore.kt`

**Do:**

- Add a payment-method screen for desktop and the `foss` flavor, reusing `BadgePeriod` and `PeriodCard` (`BadgesPayView.kt:29-171`) rather than writing new card components.
- Add the Kotlin wrapper for `APIGetBadgeState` in `SimpleXAPI.kt` and call it when the app wizard opens, caching the `CRBadgeState`. Nothing in the client calls that command today, so without this step's wrapper there is no `badgeWebBaseUrl` to open, and G4 and G6 have no cached state to read. G3 adds the Swift wrapper.
- Terminal action there: "Continue in browser" via `LocalUriHandler`, opening `{badgeWebBaseUrl}/?tier=…&months=…&pay=…` from that `CRBadgeState` (C2) and matching D5's parameter grammar.
- The `google` flavor keeps G0's "Redeem code" terminal action and gains no payment-method screen.
- Route the platform difference through the existing `androidIsPlayStoreBuild` hook (`Platform.kt:48`), which is overridden once in `android/src/main/java/chat/simplex/app/SimplexApp.kt:383` from the per-flavor `BuildConfig.PLAY_STORE` field (`android/build.gradle.kts:44,49`). The split therefore stays in Gradle's flavor configuration and `commonMain` needs no flavor-specific code. The `TODO [badges]` marker this resolves sits at `Platform.kt:39`.
- Replace the `TODO [badges]` Stripe/crypto markers this resolves.

**Verify:** Manual: on desktop the app wizard opens the browser with the right parameters; on the `google` flavor the duration screen still offers "Redeem code".

#### G2 — Kotlin: redeem view

**Files:** `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/badges/BadgesRedeemCodeView.kt`, `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt`

**Do:** Replace the title-only stub (`BadgesRedeemCodeView.kt:17`). The entry point already exists (`BadgesSupportSimplexView.kt:77-82`); do not add another.

- A code field that formats as the user types: uppercase, insert the `-` separators, and fold Crockford ambiguities to match B3's `normalizeCode`.
- A paste button.
- Submit calls `APIPurchaseBadge`.
- Inline errors mapped from `CEBadgeServiceError`: `code_invalid` to "This code isn't valid", `code_used` to "This code has already been used", `code_expired` to "This code has expired", `rate_limited` to a wait message using `retryAfter`. Any other code, `internal` included, renders the response's `message` with the support hint, so a locally failed credential verification (C4) is never a blank screen.
- Disable submit while in flight. The RPC has a per-call timeout, so show progress and allow a retry with the identical code; the service is idempotent.
- Success routes to the badge screen.

**Verify:** Manual on desktop against a locally run service: a good code issues a badge, and each error renders inline.

#### G3 — Swift: redeem view

**Files:** `apps/ios/Shared/Views/Badges/BadgesRedeemCodeView.swift`, `apps/ios/Shared/Model/{SimpleXAPI.swift,AppAPITypes.swift}`, `apps/ios/SimpleXChat/APITypes.swift`

The iOS command, response and event unions live in `AppAPITypes.swift` (`:15`, `:691,819,962`, `:1127`) and `ChatErrorType` in `SimpleXChat/APITypes.swift`, not in `SimpleXAPI.swift`, which holds only the API functions and the event dispatcher. Kotlin keeps all three in `SimpleXAPI.kt`, hence the asymmetry with G2.

**Do:** Mirror G2, including its default case for an unmapped error code, and add the Swift wrapper for `APIGetBadgeState` in `AppAPITypes.swift` and `SimpleXAPI.swift`, the counterpart of G1's Kotlin wrapper, which G5 calls from the badge screens. The entry point already exists (`BadgesSupportSimplexView.swift:108-126`) and the "Redeem code" terminal action on the duration screen landed in G0, so this step adds only that view and that wrapper.

**Verify:** Manual on iOS against a locally run service: a good code issues a badge, and each error renders inline.

#### G4 — Kotlin: catalog pricing and badge-state refresh

**Files:** `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/badges/{BadgesPayView.kt,BadgesYourLevelView.kt,BadgesSupportSimplexView.kt,BadgeStore.kt}`, `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt`

**Do:** Nothing else calls `APIGetBadgeCatalog`, so without this step desktop and `foss` render "—" for every price on the two screens that lead to the browser hand-off, and the `google` flavor shows Play Store prices for a store purchase G0 removed.

- Call `APIGetBadgeCatalog` (C4) when the app wizard opens; render prices, savings and totals from `CRBadgeCatalog`, using each offer's `total` (A2, A4) as the chargeable price and computing only the undiscounted comparison figure locally. Replace the store-derived Kotlin pricing, so desktop, `foss` and `google` all price from one source (decision 8, §7); G5 does the same for iOS. The 1-month option has no offer row (A4): its price is the tier's `monthPrice` read verbatim.
- Amounts arrive in minor units. Format them with one private formatter in `BadgeStore.kt`, following D3's rule: divide by 100, pad the remainder to two digits, prefix the symbol for `currency` (`usd` → `$`), and render an unknown currency as its ISO code before the digits. Never divide a total by a month count.
- Rename the fetch-state wrapper to `BadgePriceState` so it no longer collides with the protocol type `BadgePrice` (§3). `BadgePriceState.Loading` covers the fetch; `BadgePriceState.Unavailable` covers a failed fetch, a missing `active` price, or an offer whose `total` is `Nothing`, which no service implementing this plan sends (A2), and that tier or duration renders disabled rather than hidden (UX §2.1), matching D3.
- Call `APIGetBadgeState` (G1's wrapper) when a badge screen opens and when it regains focus, caching the `CRBadgeState` for G1's hand-off URL and G6's paid-through date. That call is also the client-side trigger C3's worker relies on for the month boundary, so without these call sites a crossed month is invisible until the daily timer fires.
- Handle `CEvtBadgeChanged` in the event dispatcher: refresh the cached badge state and recompose the badge screens. G2's redeem success path reads state directly and does not need this handler; only the worker's month-boundary re-issue (C3) does, and without it that re-issue is invisible until the app restarts.
- `BadgeStore`'s `load()`, `price()` and `annualSavings()` lose their last Kotlin callers, and the three `load()` call sites go with them (`BadgesPayView.kt:76`, `BadgesYourLevelView.kt:60`, `BadgesSupportSimplexView.kt:37`). The `BadgePriceState` type stays in `BadgeStore.kt`, so the module remains compiled with only its store-facing surface unused, kept for store-evidence verification (§6, G0).
- Resolves the `TODO [badges]` markers at `BadgesPayView.kt:28,176`, `BadgesYourLevelView.kt:26` and `BadgeStore.kt:130`.

**Verify:** Manual on desktop and the `google` flavor against a locally run service: all six tier and duration combinations show the same totals the site shows for the same catalog; with the service unreachable every option renders disabled and nothing crashes; with the host clock advanced a month, which both the app and the locally run service read, reopening the badge screen updates it without a restart, because opening it calls `APIGetBadgeState`, which signals the worker (C3).

#### G5 — Swift: catalog pricing and badge-state refresh

**Files:** `apps/ios/Shared/Views/Badges/{BadgesPayView.swift,BadgesYourLevelView.swift,BadgesSupportSimplexView.swift,BadgeStore.swift}`, `apps/ios/Shared/Model/{SimpleXAPI.swift,AppAPITypes.swift}`

**Do:** Mirror G4, including the open-and-focus `APIGetBadgeState` calls on G3's Swift wrapper, the rename of the Swift fetch-state wrapper to `BadgePriceState` in `BadgeStore.swift` (§3), its own private minor-unit formatter following D3's rule, and the `CEvtBadgeChanged` handling. The Swift `load()` call sites are `BadgesPayView.swift:126`, `BadgesYourLevelView.swift:101` and `BadgesSupportSimplexView.swift:66`; the price and savings calls are `BadgesPayView.swift:144,167` and `BadgesYourLevelView.swift:121`. Resolves the `TODO [badges]` markers at `BadgesPayView.swift:12` and `BadgesYourLevelView.swift:12`; `BadgesPayView.kt:176` and `BadgeStore.kt:130` are desktop and `foss` only and have no Swift counterpart.

**Verify:** Manual on iOS against a locally run service, as for G4; run the app in the simulator so the badge-state refresh sees the same advanced host clock as the service.

#### G6 — Strings and stub cleanup

**Files:** `apps/multiplatform/common/src/commonMain/resources/MR/base/strings.xml`, `apps/ios/en.lproj/Localizable.strings`, `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/badges/BadgesPayView.kt`, `apps/ios/Shared/Views/Badges/BadgesPayView.swift`

**Do:**

- All new copy as string resources, base locale only. No translations until the copy is frozen.
- Remove `stubBillingDate()` (`BadgesPayView.kt:255`, a hardcoded 2026-07-22) and the equivalent inline stub date in Swift's `billingFooter` (`BadgesPayView.swift:251-260`; Swift has no `stubBillingDate()` function). Prepaid months have no billing date, so the footer reads "Ends {paidThrough}" from `CRBadgeState` (C2) rather than a renewal date (§6, and the UX §2.11 rule C2 records).
- Clear the `TODO [badges]` markers this plan resolves.

**Verify:** Manual: `grep -rn "TODO \[badges\]"` contains none of the markers this plan resolves: `Platform.kt:39`, `foss/PlayStore.kt:13`, `BadgeStore.kt:130`, `BadgesPayView.kt:28,176,219,254`, `BadgesYourLevelView.kt:26`, `BadgesPayView.swift:12,220,252`, `BadgesYourLevelView.swift:12`, and the two `BadgesRedeemCodeView` stubs. Every remaining marker is a §6 deferral, unrelated copy work, or work outside this plan's scope, such as the management-screen gate at `BadgesSupportSimplexView.kt:35` and `.swift:22` and the handshake binding at `Library/Commands.hs:2102`. List the remainder in the commit message.

---

### Phase H — Hardening and operations

Nothing here adds a user-visible flow; H1's throttle message is the only new screen state. These steps are what make the service operable: limits, recovery tooling, reconciliation, logs and documentation.

#### H1 — Rate limiting and request caps

**Files:** `apps/simplex-badge-service/src/BadgeService/Web/Server.hs`, `apps/simplex-badge-service/src/BadgeService/Config.hs`, `apps/simplex-badge-service/web/src/checkout.ts`, `tests/Bots/BadgeServiceTests.hs`

**Do:** Per-IP token buckets, in memory, swept every 5 minutes. A restart forgets them, which is accepted.

- `/api/checkout`: 5 requests per minute per IP, burst 5. Each call reaches a payment provider.
- `/api/order/:orderId`: 60 per minute per IP, burst 20. It is polled.
- Webhook routes: no IP limit, a 64 KB body cap, gated on signature.
- `GET /` and `GET /api/catalog`: 30 per minute per IP, burst 30. `/assets/*` is unlimited, being immutable and served from memory.
- All other POSTs: 16 KB body cap.
- A throttled request returns `429 { error: "rate_limited" }` with `Retry-After`, in D6's error body shape. E5's polling loop backs off to that value.
- D7's error rendering gains a `rate_limited` case: "too many attempts, try again in {Retry-After} seconds", with the Pay button disabled for that interval.
- `[web] behind_proxy` trusts `X-Forwarded-For`; off by default so a direct deployment cannot be spoofed.

The redemption path runs over service RPC and has no IP; B5's per-signer throttle and global failure budget cover it.

**Verify:** In `tests/Bots/BadgeServiceTests.hs`: exceeding a bucket returns 429 with `Retry-After` and the `rate_limited` body; a body over the route's cap is rejected before parsing. Manual: the browser renders the wait message rather than the generic failure banner.

#### H2 — Code lifecycle tooling

**Files:** `apps/simplex-badge-service/src/BadgeService/Admin.hs`, `apps/simplex-badge-service/src/BadgeService/Options.hs`, `tests/Bots/BadgeServiceTests.hs`

**Do:**

- No expiry sweeper. Expiry is evaluated at redemption time by B3's classifier against `expires_at`, so there is no expired state at rest to maintain.
- `codes status --order <orderId>` and `codes status --ref <shortRef>`. Both print the order status, badge type, months and redemption state. With `--reveal` they additionally derive and print the plaintext code and the `?order=` resume URL, which is the support path E6 promises. `--reveal` requires an operator reason, is refused for a redeemed or revoked code, and its use is logged with the reason and the `shortRef`, never the code or the `orderId` (H4).
- Authorisation is possession of the database file and `[codes] secret_file`; there is no in-band authentication and no HTTP surface (decision 3). The operator runs it on the service host as the service user.
- `codes unredeem --order <orderId> | --ref <shortRef> | --code <SXB-…>` calls B1's `unredeemCode`, so the user can retry the same code and E4 discloses it again for a further `codeDisclosureDays`. A batch code has no order and no `shortRef`, so `--code` is its only selector; the operator holds the plaintext from `codes issue`. This covers C4's lost-response case. Reissuing a *different* code **for the same order** is impossible by construction: the code is a pure function of `orderId` (decision 9), so unredeeming is the only coherent recovery for that order. Compensation outside an order uses a batch code from `codes issue` (B8, H3).
- Order of operations for a lost response, which is the case support meets: `codes status --ref` confirms the redemption, `codes unredeem --ref` clears it, then `codes status --ref --reveal` prints the code, which `--reveal` now permits because the code is unredeemed again. Support holds only the `shortRef` (E6), so both support subcommands, `status` and `unredeem`, accept `--ref`.
- Every admin action logged with a timestamp and the operator-supplied reason.

**Verify:** In `tests/Bots/BadgeServiceTests.hs`: `unredeem --ref` on an order settled 60 days ago makes the code redeemable again through `purchaseBadge` **and** makes `GET /api/order/:orderId` return it again; `unredeem --ref` and `unredeem --order` resolve the same order; `unredeem --code` clears a redeemed batch code; `--reveal` is refused before `unredeem` and permitted after it.

#### H3 — Stuck-order reconciliation

**Files:** `apps/simplex-badge-service/src/BadgeService/Orders.hs`, `apps/simplex-badge-service/src/BadgeService/Service.hs`, `tests/Bots/BadgeServiceTests.hs`

**Do:** A periodic pass over D0's `getStuckOrders`: re-read provider state with D6's `readProviderInvoice`, keyed on `provider_ref`, and apply the same transitions the webhook would. `PSSettled amount at` runs E3's `settleOrder`, which sets `paid`, records `amount_paid` and `settled_at` and writes the code row. `PSPending amount` records `amount_paid` and sets `pending`. `PSExpired amount` records `amount_paid` and sets `expired`. `PSFailed` sets `failed`. `PSOpen` leaves the order untouched and logs it, because the provider still considers the invoice payable. A `ProviderError` leaves the order untouched and is logged (H4); the next pass retries. Missed webhooks are normal, so this is the safety net rather than an exception path.

- Run it on `[reconcile] interval_seconds` (600 by default) as a third arm of D4's `raceAny_`, beside the bot and the web listener.
- The pass also reports `expired` orders with a non-zero `amount_paid`. Those are support cases needing a manual refund or a compensating batch code; H5 documents the procedure.

**Verify:** In `tests/Bots/BadgeServiceTests.hs`: settle an invoice at the mock with `POST /_settle/:invoiceId?webhook=false` (E1), and confirm the pass finds it and writes the code row; a provider-side expiry moves the order to `expired` with its `amount_paid`; a provider error leaves the order unchanged.

#### H4 — Logging and redaction

**Files:** `apps/simplex-badge-service/src/BadgeService/Web/Server.hs`, `apps/simplex-badge-service/src/BadgeService/Orders.hs`, `tests/Bots/BadgeServiceTests.hs`

**Do:** The service takes money, so "did the webhook arrive", "why did this order never settle" and "how many checkout requests failed with `provider_unavailable`" must be answerable. Structured logs, one line per event:

- HTTP: method, path **template**, status, duration. The template is `/api/order/:orderId`; the concrete path with the order id in it is never logged.
- Every webhook: provider, event id, outcome.
- Every settlement: order `shortRef` and amount.
- Every provider call: latency and status, with the `ProviderError` detail on failure.
- H3's pass: a summary line per run and a line per order it changed.

`orderId`, codes, code hashes and secret material are never logged, since `orderId` is equivalent to the code (E4). This applies to exception and error paths too, which is where a raw path is most likely to leak. H5 covers the reverse proxy, which logs request lines the service never sees.

**Verify:** In `tests/Bots/BadgeServiceTests.hs`: the log formatter is a pure function; assert it redacts `orderId`, the code and the code hash from a settlement line, a webhook line and an exception line. Manual: create and settle a BTC order against E1's mock, capture the service output, and grep for that order's `orderId` and its derived code; both must be absent. H5 repeats the check over §10's full script.

#### H5 — Deployment and key-management docs

**Files:** `apps/simplex-badge-service/README.md`

**Do:** Write `apps/simplex-badge-service/README.md`, covering TLS termination, webhook registration at Stripe and BTCPay, the complete `badge_service.ini` reference (A6), and the operator procedures H3 surfaces: manual refund or a compensating batch code for underpaid expired orders, and BTCPay refunds, which have no webhook.

Key and configuration management:

- the BBS issuer key: losing it means no new credentials, leaking it means forged badges. Its **public** half must reach client builds as `ChatConfig.badgePublicKeys`, keyed by `[issuer] key_idx`; a client without it rejects every credential the service issues. Document adding a key index before rotating the secret, so credentials signed by either index verify during the overlap
- the `codeSecret`: rotating it breaks code recomputation for past orders (B3), and holding it is what lets an operator link an order to a purchase (§3, §7)
- the badge service contact address (B9): how it is published and how it reaches client builds
- the checkout site URL: `[web] base_url` on the service and `ChatConfig.badgeWebBaseUrl` in client builds must match, and how each is set at release
- **reverse-proxy access logs**: the path and query string of `/` and `/api/order/:orderId` contain the `orderId`, which is equivalent to the code (E4). Give an nginx `log_format` that drops the query string and the `:orderId` path segment. Without it, H4's redaction is defeated at the proxy.
- `shortRef` collisions: widen the ref if D6's retries become frequent
- **retention**: restate §6's position and its consequence, and give the operator the SQL to purge `@web_orders` rows settled more than a chosen number of days ago, noting that purging costs H2's `--ref` lookups for those orders
- the deployment values in §8 and their defaults

Document the backup procedure for both secrets, and state that `[web] web_dir` is development-only.

**Verify:** Manual: from a fresh checkout, following only the README, bring the service up against E1's standalone mock and complete §10's script. Every key named in the README exists in `Config.hs`, and every key in `Config.hs` is named in the README; check both directions.

#### H6 — Protocol docs update

**Files:** `docs/protocol/badges-rpc.md`, `docs/protocol/badges-rpc.schema.json`, `docs/protocol/badges-web.md` (new), `plans/2026-08-04-badges-mvp-scope.md`

**Do:** Record in `badges-rpc.md` that `getBadgeInvoice` is unimplemented, that non-store purchase is web-mediated, and that `purchaseBadge` accepts only `code` payments. Add `code_expired` to the documented error set; `rate_limited` is already there (`badges-rpc.md:64`). Add `badges-web.md` documenting the HTTP API: D4's `/api/catalog` (noting that the offer `total` is response-only), D6 and E4 shapes, the `429` response (H1), both webhook contracts, the order state machine, and the code disclosure rule. Record that badge perk lines are site constants, not catalog data (D3), and that `codeDisclosureDays` is a code constant, not configuration (E4). In `badges-rpc.schema.json`, note beside the error field that the code stays an opaque string so a later reader does not take the `.md` list as a schema constraint, and add the offer `total` member A2 introduced on `BadgeOffer`. Update the superseded milestone in `badges-mvp-scope.md` to point here.

**Verify:** Manual, in both directions: every route in `Web/Server.hs` appears in `badges-web.md`, and every route documented there exists in `Server.hs`; every `BadgeServiceErrorCode` constructor appears in `badges-rpc.md`. The documented `/api/catalog`, `/api/checkout` and `/api/order/:orderId` bodies are copied from the code, not from this plan, and a live response from §10's stack matches each one.

---

## 6. Explicitly out of scope

Deferred. All remain defined in the protocol and either return `bad_request` or are unused.

- Subscriptions and renewals for non-store payments. Web purchases are prepaid months only.
- `getBadgeInvoice` (in-app invoices), `upgradeBadgeSubscription`, `pauseBadge`.
- The `receipt` transfer payment, and `SPApple` / `SPGoogle` store evidence. **Store purchases yield no credential**, which is why decision 6 removes the store purchase action. Verifying Apple JWS and Google purchase tokens is a later plan; until then the redemption code is the only route to a badge on iOS and the Play build.
- Lifetime and investor badges. `@codes.months` is `NOT NULL` and `badge_type` is constrained to `supporter` and `legend`, because B2's `LedgerState` has no representation for an unbounded balance and `advance` would lapse one every month. Re-adding needs `data Balance = Months Int | Lifetime` threaded through B2 and B7.
- Tier upgrades (UX §2.10). Redeeming a code of a different tier creates a separate purchase and strands the old balance; it does not convert it.
- Badge alerts, the reminder timer, `CEvtBadgeAlert`, `APIAckBadgeAlert`.
- Monday presentation and removal updates, `use_from`.
- The +7 day recipient display grace and the shifted `BSExpiredOld` boundary in `mkBadgeStatus` (UX §2.11, UX §7 "Required in the first release" item 1). Required before launch, but not by this plan.
- The user-picker badge indicator.
- Automated crypto refunds. BTCPay refunds are operator-initiated with H2's tooling.
- A retention pass over `@web_orders` and `@codes`. Nothing here deletes a settled order or a redeemed code, so an `orderId` stays a valid input to `deriveOrderCode` for as long as the database and `codeSecret` coexist, backups included. This is deliberate: the rows are what support resolves a `shortRef` against (H2), and E4's disclosure rule already withholds the code once it is redeemed, revoked, or `codeDisclosureDays` past settlement, so retaining the row does not retain a usable capability. H5 records the exposure and the operator's option to purge settled orders older than a chosen age; adding a pass to H3's loop is a later change, not a gap in this one.
- Pulling store-evidence verification forward. G0 removes the store purchase action, so for the duration of this plan iOS and Play users obtain a badge only with a code bought in a browser, on that device or another. That is the accepted consequence of decision 6, not an open question: verifying Apple JWS and Google purchase tokens is its own plan, and if it must ship first, this plan waits rather than growing a phase.
- Provisioning and deployment: DNS, TLS certificates, the reverse proxy, the Stripe account and the BTCPay store, the production hostname that sets both `[web] base_url` and `ChatConfig.badgeWebBaseUrl`, and the production rollout. Operator work, documented in H5 and performed outside this plan.

## 7. Risks

| Risk | Mitigation |
|---|---|
| A code is a bearer secret to a paid good | 95 bits of entropy, which is the load-bearing control; the checksum is rejected before any database lookup; a global failure budget on `purchaseBadge` and a per-signer throttle that only shapes honest retries (B5); per-IP limits on the HTTP endpoints (H1) |
| `orderId` is equivalent to the code: anyone holding it can read the code from `/api/order` | `orderId` is 128 random bits, never logged (H4), never in a `Referer` (D4), and absent from the Stripe `cancel_url` (F1). `code` is withheld once the code is redeemed or revoked (F4, H2), or `codeDisclosureDays` after `max(settled_at, unredeemed_at)` (E4); `codes unredeem` reopens the window (H2). E6 tells the user the URL is equivalent to the code. H5 covers the reverse-proxy access log, which the service does not control |
| A `codeSecret` leak makes the code for any known `orderId` offline-derivable | Both secrets are backed up (H5); `orderId` is itself a 128-bit secret, so an attacker needs both. Rotating `codeSecret` destroys code recomputation for every past order (B3), so H5 documents it as a break-glass action to be taken only once every outstanding order is past `codeDisclosureDays`, not as routine hygiene |
| The operator can link a card payment to a SimpleX profile | Only with `codeSecret`, which derives the code from any `orderId` and joins it to `redeemed_purchase_id`. No row stores both identifiers (A3, §3), and B10 asserts the two-hop `web_orders → invoices → payments → badge_purchases` join is empty. But a database copy alone still correlates an order to a purchase by settlement timestamp and the `batch = 'web'` label, with no `codeSecret` — so what holds is "no stored reference", not unlinkability. E3/H5 own the mitigation (§9, B10). Unlinkability from the operator is not claimed |
| A user is charged in a store and receives nothing | The store purchase action is removed until store evidence is verified (G0, decision 6, §6); iOS and Play users reach a badge only through a redemption code |
| A webhook is missed | Every settlement path is idempotent and monotonic toward `paid` (E3); an unprocessed event is reprocessed on retry (D0, E3); H3 reconciles from provider state |
| Apple or Google reject the linked-out purchase | Decision 6: the redirect ships only on desktop and Android `foss` |
| Site and app prices drift | One source and one total function (A4). The service computes every total and both catalogs carry it (A2); the service serves it (D4), the site renders it (D3), and the apps render it from `CRBadgeCatalog` (G4, G5). No second implementation exists |
| Committed `web/dist/` goes stale | D8 fails CI when `dist/` does not match `src/` |
| A Haskell rebuild is needed for a CSS tweak | `[web] web_dir` (D4) during development; embedded assets in production |

## 8. Deployment values

No step in this plan waits on an answer. This section lists only the values an operator supplies at deployment. Every one has either a code default or the working value §10 uses, so the plan is executable end to end before any is known. The `[web]` group is required whenever a provider is configured (A6); §10 supplies it.

| Value | Default | Set in | Notes |
|---|---|---|---|
| BTCPay Monero method id | `XMR-CHAIN` | `[btcpay] xmr_method_id` | E2 logs the store's reported method ids at startup, so a mismatch shows in the log |
| Bitcoin invoice window | 15 minutes | `[btcpay] btc_expiry_minutes` | BTCPay's own default |
| Monero invoice window | 60 minutes | `[btcpay] xmr_expiry_minutes` | Longer than Bitcoin's; E5's countdown reads the value, so widening it needs no copy change |
| Checkout site hostname | `http://localhost:8080` in development; `ChatConfig.badgeWebBaseUrl` defaults to empty, meaning the feature is off | `[web] base_url` and `ChatConfig.badgeWebBaseUrl` | The two must match; H5 documents keeping them in step. §10 runs on the development value |
| Web listener port | required; `8080` in §10 | `[web] port` | Bound to `[web] host`, which does default, to `127.0.0.1` |
| Support contact | required; `https://simplex.chat/contact` in §10 | `[web] support_contact` | D2's footer, the site's only contact channel, which has no fallback |
| Reconciliation interval | 600 seconds | `[reconcile] interval_seconds` | H3 |
| Code expiry | 365 days | `[codes] default_expiry_days` | B8, E3 |

## 9. Plan changes

Append here when a step contradicts this plan: the step id, what was wrong, and what was done instead.

- **A1 — the migration entry is inserted chronologically, not appended.** The step said to append after `20260723_contact_request_rejection`, "whose current last entry is" that migration. Merging `master` into this branch added `20260813_auto_accept_group_invitations` after it, so `20260731_user_badges` now goes *between* the two, in both lists. Read the lists before editing rather than trusting a stated last entry; the same applies to any later step that appends to a shared list.
- **A1 — `Store/Postgres/Migrations/chat_schema.sql` regenerated in a later commit, and hand-filtered.** The step's Verify requires committing it, but PostgreSQL was absent when A1 landed. Once installed, the dump was regenerated and the badge tables, their constraints and indexes, and `users.shown_badge_id` with its foreign key all appear correctly. Two categories of environment noise were removed by hand before committing, and the result was checked to be a pure-addition diff containing nothing but badge objects:
  - `\restrict`/`\unrestrict` directives, which `pg_dump` emits since the August 2025 security patch. Their token is **randomised per run**, so committing them would make the file differ from itself on every regeneration.
  - `relay_request_execute_at`'s default, which the dump renders in the server's timezone: `'1970-01-01 01:00:00+01'` in the committed file versus `'1970-01-01 00:00:00+00'` here. The same instant; the committed rendering was kept to avoid a spurious flip.
- **The Postgres schema-dump spec cannot pass on Linux as written. Pre-existing, unrelated to badges.** Two independent causes, and together they explain why this dump had drifted. First, `tests/PostgresSchemaDump.hs:71` selects `sed -i ''` — BSD/macOS syntax — unless `envCI` is true, and `envCI` is `lookupEnv "CI" == Just "true"` (`tests/ChatTests/Utils.hs:117`). A developer on Linux running locally, without `CI=true`, gets the macOS branch and the spec dies on `sed: can't read /^--/d`. The flag conflates "running in CI" with "has GNU sed". Second, even with `CI=true`, the spec compares a freshly dumped schema against the committed file without stripping `\restrict`/`\unrestrict`, so any patched `pg_dump` fails the comparison non-deterministically. Fixing either is outside this plan; note both before relying on that spec.
- **A2 — three more `Int64` id fields need the same correction the step already mandates.** The step lists `BadgePurchase.paymentId`, `BadgePayment.paymentId` and `BadgeIssuance.issuanceId`. An audit of `badges-rpc.schema.json` against all four modules found the same defect in three further places, all `TEXT` columns typed as `Int64`: `StatementCreditType.SCCharge {chargeId}` (`Badges/Service.hs:163`, against `subscription_charges.charge_id TEXT NOT NULL PRIMARY KEY`), and `BadgeCharge.chargeId` and `BadgeCharge.paymentId` (`Badges/Types.hs:163-164`). `SCCharge` is the load-bearing one: it is a wire type whose `taggedObjectJSON` instance A2 writes, the schema declares `chargeId` as `string` (`badges-rpc.schema.json:230`), and Aeson would encode an `Int64` as a JSON number — so leaving it ships a payload that fails its own schema. Its sibling `SCPayment` already carries `Maybe InvoiceId`, a newtype over `Text`. A2 corrects all six.
- **`LedgerCreditType.CTPayment {invoiceId :: Int64}` and `CTCharge {chargeId :: Int64}` are wrong against their columns but are marked `-- confirmed`. `CTPayment` resolved in two steps — B7 made it `Text` (right for the service, wrong for the client), B10 made it `Maybe Text` (right for both), see below; `CTCharge` still OPEN — needs a decision, not a mechanical fix.** `invoices.invoice_id` and `subscription_charges.charge_id` are both `TEXT`. These are the DB-side twins of the wire types above, and `CTTransferIn {fromPurchaseId :: Maybe Int64}` beside them is correct because `from_purchase_id` really is `INTEGER`. A2 does not touch them: they sit outside its tagged-sum list, and altering a type someone marked confirmed is above a mechanical step. C1's `insertLedgerEntries` is the first code that would persist them, so this must be settled before C1.
- **A4 — `offerTotal` calls `error` on an impossible offer, which B6 and D4 must not let reach a request thread.** `chargeableMonths` (`BadgeService/Catalog.hs`) rejects `freeMonths >= months` with `error` rather than wrapping a `Word8` subtraction, and `seedCatalog` forces it at startup so a bad catalog kills the process before the service accepts traffic. That fences it for Phase A, where `seedCatalog` is the only writer. It stops being fenced the moment `offerTotal`/`catalogTotals` run inside request handling over rows read from the database, which is B6 (`getBadgeCatalog`) and D4 (`/api/catalog`). The bot's `processQueuedRequests` is a single-threaded `forever` loop (`BadgeService/Service.hs:96-99`), so an uncaught `error` there would take the whole service down for every user rather than failing one request — strictly worse than the mispricing the guard prevents. Before B6, either give `BadgeOffer` a smart constructor so `freeMonths >= months` is unrepresentable, or catch at the request boundary so the blast radius is one response.
- **B6 — resolves the A4 `offerTotal`/`error` hazard above with a third option: a typed absence, not either option this plan named.** Neither a smart constructor (would change `BadgeOffer`'s shape and every construction site, including the seeded defaults) nor a request-boundary catch (relies on `runHandler`'s catch-all reaching every caller, including D4's future HTTP path, which does not share that catch-all) was taken. Instead `chargeableMonths :: Word8 -> Word8 -> Maybe Word8` and `offerTotal :: BadgePrice -> Maybe BadgeOffer -> Maybe CurrencyAmount` (`BadgeService/Catalog.hs`): `freeMonths >= months` now answers `Nothing`, which A2 already defines on the wire as "no computed total, render unavailable" — so the blast radius of one bad row is one offer, not one response and not one process, and the fix is shared by every caller of `offerTotal`, present or future, not just ones inside a `catch`. `seedCatalog` still fails the process at startup, by name (`requireTotal`), for a bad *default* catalog, so the Phase A guarantee is unchanged. `handleGetBadgeCatalog` additionally logs (`logUnpricedOffers`) when a *database* row reaches this state, since nothing else would ever say why a live offer reads as unavailable. **Fix round 1** found the same hazard on `ODDiscount`'s sibling arm, which the original pass missed: `100 - percent` on a bare `Word8` wraps for `percent > 100` (a `Store.decodeDiscount` value is not range-checked on the way in), producing a 2.55x *overcharge* rather than a crash — worse than the `freeMonths` case, since it inflates the price instead of merely reading as unavailable. `discountedPercent :: Word8 -> Maybe Word8` closes it the same way, `percent > 100 -> Nothing`.
- **B6 fix round 1 — `seedCatalog`'s `requireTotal` is stricter than the `forceTotal` it replaced: an unpinned offer's `total = Nothing` is now also rejected at startup, not just an unchargeable one.** The predecessor (`forceTotal`, pre-B6) accepted `total = Nothing` unconditionally — its Haddock read "an offer whose price isn't found... gets `total = Nothing` rather than a crash, same as an unpinned offer" — because at the time nothing distinguished "unpinned" from "not chargeable" and neither was thought worth failing startup over. B6's `requireTotal` rejects `total = Nothing` for either reason: correct for today's `defaultCatalog` (every seeded offer is pinned, so only "not chargeable" is reachable), but a real behaviour change per §4 rule 6 — a future unpinned *default* offer would now fail the service at startup rather than silently seed with no total. Recorded rather than re-litigated, since the stricter behaviour is the one actually wanted (an unpinned default offer is exactly the kind of bad row `seedCatalog` should catch by name, not seed silently).
- **B6 — `BadgeService/Config.hs` is touched, outside the step's stated file list (`Service.hs`, `tests/Bots/BadgeServiceTests.hs`).** `takeCatalogBucket` (the peek-and-debit STM action for the unsigned-`getBadgeCatalog` bucket B5 added but never wired) had to be added there to satisfy the Verify line's throttle case — B5 built the bucket itself but not a way to spend from it, and that action belongs beside `checkFailureBuckets`/`debitFailureBuckets`, the other bucket operations, not in `Service.hs`. §4 rule 6 was applied to the `Catalog.hs` deviation (the two entries above) but missed this one in the original pass; recorded here on the same rule.
- **§4 — the stated build command did not work; corrected in place.** `cabal build simplex-chat simplex-badge-service` fails with `Ambiguous target 'simplex-chat'`, because `simplex-chat` names both a library and an executable component. It is now `cabal build lib:simplex-chat exe:simplex-chat simplex-badge-service`, which was run and succeeds. The test command beside it was correct as written and passes: 41 examples, 0 failures across both `Supporter badges` and `Badge service`.
- **B1 — two vocabularies are now spelled only in `BadgeService/Store.hs`, with nothing tying them to a future codec.** The ledger's `entry_credit_type`/`entry_debit_type` values (`support`, `opening`, `transfer_in`, …) and the payment provider literal `code` are written as bare strings, because no `ToField`/`FromField` instance exists for `LedgerCreditType`, `LedgerDebitType` or `PaymentProvider` anywhere in the repo. Neither is a *duplicate* spelling today, so neither is a defect. Both become one the moment a later step adds a codec: D0, E2 and F1 need a real `PaymentProvider` encoding, and whoever resolves the `LedgerCreditType` question above will need the entry-type spellings to match what B1 already wrote. Reuse these spellings rather than inventing a second set, and prefer deriving both directions from one instance, as `BadgePurchaseStatus` and `BadgeItemStatus` do.
- **B5 — the per-signer bucket sweeper is built and tested but nothing schedules it. B7 must wire it to a timer. RESOLVED by B7 below.** The original hazard is closed: `peekSignerBucket` no longer creates an entry for an unseen key, so a freshly minted keypair costs nothing, and an entry is created only by a classified failure — which also spends a token from the single shared global bucket, capping new entries per window at that bucket's capacity however many keys an attacker mints. `sweepSignerBuckets` evicts recovered entries and `sweepSignerBucketsIO` takes the injectable clock so eviction is provable without sleeping. What is missing is a caller: nothing runs the sweep on an interval, so the map only shrinks when something asks it to. This costs nothing during B5, where `debitFailureBuckets` has no caller at all and the map is provably empty on every reachable path. **B7 is the first step that makes a redemption fail, so B7 owns wiring the sweep onto a timer** — a third arm of the service's `raceAny_` alongside the bot and the reconciliation pass is the natural place.
- **A6 — a duplicated ini section or key is silently accepted, keeping only one of the two. OPEN.** `Data.Ini`'s `parseIni` is `parseOnly iniParser` over a `many`-based parser, so it essentially cannot fail on malformed-but-textual input: `many` never fails and `parseOnly` does not require full input consumption, so trailing garbage is dropped without error. `readIniFile` therefore returns `Left` only on I/O errors. Two consequences the config parser does not check for. A repeated `[section]` header discards the earlier block entirely, because `iniSections` is a `HashMap.fromList` and the last value wins; a repeated key within one section keeps the first, because the lookup takes the first match. If the surviving block is itself valid, the service starts with a silently wrong configuration — including, for instance, a secret-file path from the wrong block. Add a duplicate-section and duplicate-key check to `Config.hs`, and until then say so in the operator documentation at H5.
- **A6 — `seedCatalog` calls `getCurrentTime` directly, against the rule that only `BadgeServiceEnv.now` reads the clock.** This is structurally forced rather than an oversight: `seedCatalog` runs before `newBadgeServiceEnv` exists, since the env is built after migrations and seeding. Either seed later, or give `seedCatalog` a clock parameter, whenever a step first needs to control service time across startup.
- **B7 — `CTPayment` is now `{paymentId :: Text}`, closing the open item above.** The column it persists through is `badge_ledger.payment_id TEXT REFERENCES @payments` (`M20260731_user_badges.hs:148`), and `@payments.payment_id` is a `TEXT` primary key (`:43`) — so the field is the *payment's* id, not the invoice's, and `Text` is the only type that fits. `BadgeService.Store`'s `encodeLedgerEntryType`/`decodeLedgerEntryType` now persist and read `credit`/`payment` rows instead of refusing them, and B6's `statementEntryType` converts one to `SCPayment {invoiceId = Nothing}` instead of refusing it. The wire field is the *invoice's* id, which a code payment does not have (B7 step 4), so `Nothing` is correct for every payment this milestone writes. It stops being correct for an invoice-funded payment: `payments.invoice_id` is the join that resolves it, which `statementEntryType` (pure, connection-free) cannot do — **whichever step first credits an invoice-funded payment must resolve the invoice id before the entry reaches the statement.** `CTCharge` is untouched and still refused in both directions: subscriptions are out of scope (§6), so nothing writes one.
- **B7 — the per-signer bucket sweep now runs on a timer, closing B5's open item above.** `sweepSignerBucketsLoop` is a third arm of `raceAny_` in **both** entry points — `badgeService` (the default, which the test harness and the shipped binary use, and which had no `raceAny_` at all before: its event loop moved into `processServiceEvents`) and `badgeServiceCLI`. The interval is a named constant, `signerBucketSweepIntervalSeconds = 600`, and it is real time (`threadDelay`), not `BadgeServiceEnv.now`: it schedules the sweep rather than deciding anything, and the eviction itself still reads the injectable clock through `sweepSignerBucketsIO`, so B10 proves eviction by calling that directly rather than waiting on the loop.
- **B7 — the already-issued month's credential is fetched by probing `getIssuanceForPeriod` at `now`, not at `addMonths (-1) balanceStartTs` as step 4 said.** `addMonths` is deliberately not additive under clamping (`Badges/Months.hs`: 31 Jan + 1 month = 28 Feb, and 28 Feb − 1 month = 28 **Jan**), so stepping a month back from a clamped period boundary can land *before* the period it came from and match the issuance for the month before — returning the wrong credential, not merely none. `now` is inside the period by construction (`balanceStartTs > now` is why `issue` returned `Nothing`, and the period's start is at or before the instant that issue ran, hence at or before `now`), so it names exactly one issuance with no arithmetic at all. Step 4 corrected in place.
- **B7 — B1's `createCodePayment` is split into `createCodePayment` (the `@payments` row) and `attachPurchasePayment` (the purchase's pointer).** Step 3 requires a second code redeemed under a key that already has a purchase to credit that purchase's existing ledger, and step 6 requires the `payments` row for the `credit(payment)` entry to reference — but `@badge_purchases.payment_id` is `UNIQUE` and holds at most one payment (`M20260731_user_badges.hs:99,104`), so the combined function could only answer `SEPaymentConflict` there. The second code now gets its own `payments` row while the purchase's pointer stays on the first. `createCodePayment` had no caller before B7, so nothing else changed. Step 6 corrected in place.
- **B7 — `markCodeRedeemed` is now guarded and a new `SECodeConflict` exists.** Step 6's "a conflict on the code's redemption columns aborts the transaction and re-classifies from step 1" had nothing to detect a conflict with: B1's `UPDATE` was unguarded and would have silently overwritten another key's redemption. It is now `WHERE code_hash = ? AND redeemed_purchase_id IS NULL`, with an existence check separating `SECodeNotFound` from `SECodeConflict`. The handler retries the whole redemption exactly once on `SECodeConflict` (`redemptionAttempts = 1`); the second pass is terminal, because a redeemed code cannot become unredeemed by itself.
- **B7 — a `badgeRequest` naming a different tier than the funding is refused with `bad_request`. Not in the step, and it is a security boundary.** RPC "Commands": "the service signs exactly this content or rejects the command". Without the check, a client could present a `supporter` code (or hold a `supporter` balance) and be handed a signed **`legend`** credential, since `issueSignedBadge` overrides only `badgeExpiry` and signs the `badgeInfo` it is given. `purchaseBadge{code}` now requires `badgeRequest.badgeInfo.badgeType` to equal the code's `badge_type`, `issueBadge` requires it to equal the purchase's `current_badge_type`, and the same-key **replay** path requires it to equal the `badge_type` of the issuance being handed back — the replay returns a cached credential and signs nothing, but a mismatch there must answer like every other path rather than return a credential of the other tier.
- **B7 fix round 1 — a signing failure returns `internal`, not `bad_request`; and a non-empty `badgeExtra` is refused before anything is classified.** The first pass propagated `issueSignedBadge`'s code verbatim, and B4 collapses *every* failure to `BSEBadRequest` (`Credentials.hs:61-62`) — a BBS signing failure included, which is a service fault. `bad_request` is **terminal for the attempted command** (`docs/protocol/badges-rpc.md:64`), so that told the client never to retry a purchase whose code was still valid and still unredeemed, the exact opposite of step 5's stated reason for choosing `internal`. The one genuinely client-caused failure B4 folds into that code — a non-empty `badgeExtra`, which the RPC reserves — is now refused by both handlers up front (`badgeExtraEmpty`), before classification, planning or signing, and before any bucket is debited; everything reaching `resolveIssue`'s signer is therefore the service's own fault and maps to `internal`. B4 itself is untouched: it still discards the underlying error, so the log names only the operation.
- **B7 fix round 1 — the plan-then-sign-then-write split is safe because the request loop is single-threaded, and that is now stated as a precondition rather than implied to be handled.** `markCodeRedeemed`'s `redeemed_purchase_id IS NULL` guard and the one-retry `SECodeConflict` path read as if a concurrent redemption were a live scenario. It is not: `processServiceEvents`/`processQueuedRequests` handle one request at a time in a single `forever` thread. The guard is kept — it costs nothing and the code row is also written out of band by B8's operator tooling — but the **ledger has no equivalent**, and that is the real constraint: two overlapping commands on one purchase key would both plan from the same `getLastLedgerEntry` and both append, producing two entries whose `balance_months` derive from the same base (a silently wrong balance) and, for two `issueBadge` calls, two issuances for one period. **Anyone making dispatch concurrent must serialise per purchase key, or add a compare-and-append to the ledger, first.** Deliberately not guarded now: a compare-and-append for a race that cannot occur would be untested defensive code in B1's layer.
- **B7 — `purchaseBadge` carrying an `upgrade` is `bad_request`, before the payment is looked at.** `BSCPurchaseBadge.upgrade` is the store one-time upgrade (RPC "Upgrades"), which needs store evidence, and tier upgrades are out of scope (§6). Ignoring the field would consume the code while silently dropping what the client asked for.
- **B7 — `issueBadge` honours the asserted cursor, closing B6's "`previousEntryId` cannot be populated" gap.** `issueBadge` is the only implemented command carrying `balance.lastEntry.entryId`. A new store function, `getLedgerEntryIdByUuid`, resolves that wire uuid to the local `entry_id`, scoped to the purchase so an asserted uuid cannot probe another purchase's ledger; `purchaseStatement` now takes a `Maybe StatementCursor` carrying both halves, so `previousEntryId` always echoes the value the client actually sent and is never spelled independently of the id the query runs on. An assertion naming nothing yields the complete history, which is the RPC's other permitted answer; its third — one `opening` credit restating the balance — needs opening entries, which nothing in this milestone writes. `purchaseStatement` also takes the `badge_purchase_id` rather than the whole `BadgePurchaseRow`, since the replay path has only the id. `getBadgeCatalog` and `purchaseBadge` carry no cursor and pass `Nothing`.
- **B7 — the same-key replay path does heal the ledger, which step 2's "writes nothing" reads as forbidding.** "Writes nothing" is kept for everything the redemption would record — no second purchase, payment, credit, debit, issuance or redemption — but the statement is still read through `purchaseStatement`, which appends one `debit(lapse)` when months have genuinely lapsed since. The RPC has the service heal its own ledger before answering any statement ("Statement and balance"), B6's read-only `getBadgeCatalog` already does exactly this, and the alternative is telling the client a balance the database does not hold. A replay immediately after a timeout — the case §Idempotency is about — lapses nothing and so writes nothing.
- **B7 — three transactions per successful `purchaseBadge{code}`, not one (this entry said "two" until B10 counted them).** "One transaction per command" is kept for *writing*, which is the invariant that matters, but the count is three: `classifyRedemption`'s `lookupCode` opens one read transaction, `planTxn` opens a second, and after signing (with none open, which the step requires) `writeTxn` opens the third, which does every write and reads the statement back inside it. `IssueCached` adds a fourth, read-only, to fetch the cached credential. A command with nothing to write still opens one and writes nothing in it. The behaviour is right; only the arithmetic in this entry was wrong.
- **B7 — an existing B5 test's expected code changed.** `testBadgeServicePurchaseBadgeUnknownKeyIsNotUnknownPurchaseKey` asserted `internal` because `purchaseBadge` was unimplemented; `"UNKNOWN-CODE"` now reaches the classifier, normalizes to 11 characters and fails the check character, so it is `code_invalid`. Its load-bearing assertion — never `unknown_purchase_key` — is unchanged. B10 replaces the surrounding suite.
- **A1 — `chat_lint.sql` gains 5 fkey-index advisories, left unfixed by design.** The badge migration introduces unindexed foreign keys: `badge_invoices.offer_id`, `badge_invoices.price_id`, `badge_offers.price_id`, `badge_issuances.entry_id`, `users.shown_badge_id`. The lint output is committed literally rather than adding indexes, since index design is outside A1's scope and the repo has precedent for this (`9e000d6bc`). The first three point at rarely-mutated reference tables. The last two are the ones likely to matter under load — `badge_issuances.entry_id` for issuance lookup by ledger entry, and `users.shown_badge_id` for per-user badge display (C1's `getShownPurchase`). Decide on indexes for those two before release.
- **B9 — the step's Verify line said "Manual"; an automated test was added instead, and a genuinely standalone manual run (two separate OS processes, hand-typed `/c`) could not be completed in this environment.** A real two-process run needs a live SMP server: this sandbox has no network egress (a direct connection to a public relay times out), and building `simplexmq`'s own `smp-server` executable from the checked-out dependency source fails independently of this step — `apps/common/Web/Embedded.hs`'s `embedDir "apps/common/Web/static/.well-known/"` Template Haskell splice reports the directory missing at compile time even though it is present on disk in the checkout, an environment-specific defect unrelated to the badge service. In its place, `testBadgeServicePublishesAddressFile` calls the real, unmodified `badgeService` entry point (not a stand-in) twice via the existing harness's `withBadgeServiceConfig`/`runBadgeService`, against the harness's own local in-memory SMP server, and adds a real second client issuing `/c` against the file's own contents — proving both halves of the original Verify line (same address across two starts; the file's link is accepted, not rejected as malformed) end to end, just not via two hand-launched OS processes. `initializeBotAddress'`'s `doAutoAccept = False` for the badge service means the client-side assertion stops at "connection request sent!" rather than a full connection, which is as far as `/c` accepting the address goes for this bot. Whoever next has real network access in this environment should still do one standalone two-process run before H5 documents the operator procedure, to catch anything this substitution cannot (e.g. a real terminal's stdout formatting of the printed address).
- **B9 — the address is re-read via a second `ShowMyAddress`, not threaded from `initializeBotAddress'`'s own result.** `initializeBotAddress'` (`src/Simplex/Chat/Bot.hs`) is shared by four callers (this service, the directory service, the broadcast bot, `bot-advanced`) and returns nothing; changing its signature to hand back the resolved link would touch all four for a need only this step has. `badgePostStartHook` instead calls `sendChatCmd cc ShowMyAddress` again immediately after, which is cheap (a local store read, no network) and already proven safe by the existing two-phase test harness relying on the same command.
- **B9 — `writeAddressFile`'s three deliberate failure modes.** No address-file pattern existed anywhere in the repo to copy (the step's own **Do** section says so), so these were decided here, not inherited: (1) an unwritable path (including a parent directory that does not exist — nothing here creates one) is logged as an error and does **not** stop the service, since `initializeBotAddress'` already put the same address on stdout and nothing about serving RPC requests depends on this file; (2) same as (1) — a missing parent directory is treated as any other write failure, not auto-created, since silently creating directories from an operator-supplied config path is more surprising than helpful for a deployment file; (3) a file that already holds a *different* address is always overwritten (logging a warning first) rather than left alone, so an operator who restarts the service can never be looking at a stale address — the file is unconditionally rewritten with whatever `ShowMyAddress` reports on every start where `address_file` is configured.

- **B10 — the harness's `now` override did not exist, and B10 built it.** A6's step says `withBadgeService` "also accepts a `now` override, used by B10 and C5", but A6 shipped `newBadgeServiceEnv` with `now = getCurrentTime` hardcoded and no way to reach it from a test — the live service builds its own env in `badgePreStartHook`. B10 adds `BadgeServiceOpts.serviceClock :: IO UTCTime` (the CLI parser has no option for it and always sets `getCurrentTime`, so production is unchanged) and makes `newBadgeServiceEnv` take the clock as a parameter, which also fixes a smaller inconsistency: the two service-wide token buckets were initialised from `getCurrentTime` while every handler read `BadgeServiceEnv.now`, so under an overridden clock their refill origin would have come from a different timeline. The test harness gains `withBadgeServiceClock`, of which the existing `withBadgeServiceConfig` is now the real-clock case. **C5 needs the client-side twin (`badgeCurrentTime`) and should not assume it exists either.**
- **B10 — the single `unsupported_version` test was NOT replaced, only added to.** The step's "Replace the single `unsupported_version` test with …" was written when that was the file's only test; B5, B6, B7 and B9 have since put 46 more examples in it, and B7's own note above says "B10 replaces the surrounding suite". Nothing was removed or weakened: the 18 new examples were added alongside, and `testBadgeServiceUnsupportedVersion` still asserts the version gate.
- **B10 — `balance_start_ts` is backdated by moving the injected clock, not by seeding a row through B1.** The step says "with the purchase's `balance_start_ts` backdated one month through B1". A hand-seeded backdated ledger row has no matching `badge_issuances` row, so the second half of the same bullet — "a third call inside the same month returns the cached credential" — would reach `IssueCached` with nothing to fetch and answer `internal`. Advancing `BadgeServiceEnv.now` past a real redemption exercises the same boundary against a self-consistent database, and is what the "No test sleeps" rule points at anyway.
- **B10 — `Ledger.issue`'s own Haddock instructed the defect, and is corrected.** It told callers to "tell the two apart by the balance", which is precisely the rule that produced the bug below; it is the one comment a future caller of `issue` reads. It now says to test `balanceStartTs > t` and why the two refusal reasons are not exclusive. Correcting `planLedger` and plan §5 alone would have left the instruction in place for the next caller.
- **B10 — a B7 defect found and FIXED here: a zero balance hid an already-issued month.** `planLedger` told `IssueCached` from `IssueExhausted` by the balance alone, but `issue` returns `Nothing` for two independent reasons — an exhausted balance, and a month that is already issued. Once the LAST funded month had been issued both held at once, so a repeat `issueBadge` inside that same month answered `credential = Nothing` instead of the credential the service had already signed, stored and delivered. **Failure mode: C3's worker retries `issueBadge` after a timeout in the final funded month, is told there is no credential, and the user loses a month they paid for** — the exact case RPC §Idempotency exists for. Fixed by classifying on `balanceStartTs > now` (`issue`'s own already-issued guard) instead of `balanceMonths == 0`, which is total over both reasons on every path: `initialLedgerState` sets `balanceStartTs` to `now`, `credit` to `max balanceStartTs now` and `advance` only to boundaries at or before `now`, so a `balanceStartTs` past `now` can only have been set by a previous `issue` and an issuance covering `now` always exists there. B7's step 4 above is corrected in place. This is B7 code changed inside B10's commit range, deliberately: B10 held the reproduction. Pinned by `testBadgeServiceIssueBadgeCachedInLastFundedMonth` (one funded month, issued, then `issueBadge` again at the same instant and ten days later — both must return the cached credential), proved able to fail by reverting the classification; `testBadgeServiceIssueBadgeExhaustedBalance` holds the genuinely-exhausted month after it, and `testBadgeServiceIssueBadgeSecondPeriod` funds four months so the two cases stay separate.
- **B10 — `SECodeConflict`'s retry path is left untested, deliberately.** B7's own concern: driving it needs two redemptions of one code interleaved between classification and write, and the request loop handles one request at a time. Nothing in B10 pretends to cover it; the one mutation that reached it (a same-key replay reclassified as `RedeemOk`) proved only that the code-row guard fires, not that the retry resolves correctly.
- **B10 — the Postgres cross-check deferred since A3 was run, and passes.** `--flags=client_postgres` with `-m "Badge service"`: 64 examples, 0 failures (65 minus the one `#if !defined(dbPostgres)` example). Four environment obstacles, none of them code: the flag build reuses `dist-newstyle` and so invalidates the SQLite build in both directions; the socket directory must be short (`/tmp/pgs`, not a long scratchpad path — `sun_path` is 107 bytes); the suite needs **two** roles, `test_chat_user` (owner of `test_chat_db`) and a superuser **`postgres`**, which `Test.hs`'s `createdDropDb` bracket connects as; and `max_connections` must be raised well above the default 100 (30 of 64 examples failed on connection slots until it was 500). The two file-reading assertions are guarded, so the §3 Linkage privacy guard and the no-plaintext-at-rest check run on SQLite only — a Postgres twin over `information_schema` is a worthwhile follow-up.
- **B10 — what the §3 Linkage guard does and does not cover, in full.** It enumerates every table in the service database and flags one as holding an order (purchase) reference if it declares a foreign key to `@web_orders` (`@badge_purchases`) **or names a column after it** — `web_order_id TEXT` with no `REFERENCES` clause is invisible to `pragma_foreign_key_list`, and D0, the next step, is what adds order tables. Three anti-vacuity controls: both tables are seen by name, `@codes`'s real declared foreign key to `@badge_purchases` is seen, and a probe table carrying both columns with **no** foreign keys is created, caught and dropped before the real scan. Residual limits, all disclosed: **(a)** it is SQLite-only (`#if !defined(dbPostgres)`), and `Store/Postgres/Migrations.hs` is a separately hand-maintained file, so a linkage column added *only* there is invisible to this guard forever — a twin over `information_schema.table_constraints`/`key_column_usage` is the fix, and it is a follow-up, not done here; **(b)** the name half matches substrings, so an innocent `sort_order` column would raise a false alarm — deliberate, since the cost is one reader's minute against the privacy claim the whole web checkout rests on; **(c)** it sees only *columns*, so a join built some other way (a view, a lookup table keyed by a hash of both, an out-of-band file) passes. B8's no-plaintext-at-rest scan is guarded for the same file-reading reason and now carries a positive control (the batch label, which IS stored in the clear, must be found in the same bytes), so it can no longer pass because it read the wrong or an empty file.
- **B10 — B8's `codes` assertions run through `runAdminCmd` with stdout captured, and inspect the database only through `codes status`.** `runAdminCmd` opens its own store and creates a chat database with no user profile, which the test harness's `withTestChat` reopen cannot read (it requires an active user) — so the row-level checks are made by asking the tool itself: each of the ten printed codes resolves to an unredeemed row, and `codes revoke` reports exactly ten in the batch. The plaintext-at-rest check reads the SQLite file directly and is `#if !defined(dbPostgres)`-guarded, as is the §3 Linkage schema assertion.

- **B10 — `CTPayment` is now `{paymentId :: Maybe Text}`, and the "RESOLVED by B7" line above was misleading.** B7's ruling (`Text`) was right for the service and wrong for the client, which is the half that had not been checked. The wire carries no payment id at all — the statement's `SCPayment` carries the *invoice's* id, and a code payment has no invoice — and the client has **no `@payments` row** for a code redemption: that row is written by `createCodePayment` in the service database only. So C1, copying the service's ledger entries into the client's `badge_ledger`, must write `payment_id` as NULL, which `Text` cannot hold and which `decodeLedgerEntryType`'s `Just paymentId` pattern rejected outright — C1 would have hit this on its first write. The field is now `Maybe Text`, `encodeLedgerEntryType` passes it straight into the nullable column, `decodeLedgerEntryType` accepts a NULL on a `credit/payment` row, and the service write site says `Just paymentUuid`. **C1 writes `Nothing` and must not invent an id.**
- **B10 — the unlinkability claim has a two-hop join that the schema guard structurally cannot see, and it is now asserted directly.** `web_orders.invoice_id → @invoices ← payments.invoice_id`, then `payments.payment_id ← badge_purchases.payment_id`, is a complete order→purchase join. Nothing in the schema prevents it: it is empty only because `createCodePayment` hardcodes `invoice_id = NULL` for a code payment (`Store.hs`). `testBadgeServiceNoTableLinksOrdersToPurchases` now plants a settled web order, its invoice, a code payment and a purchase, asserts the join returns **0**, and — as a positive control — that setting `payments.invoice_id` to that order's invoice makes it return 1. **D0 and E3: setting `payments.invoice_id` to a web order's invoice breaks §7's unlinkability claim outright.** The same shape exists through `@badge_invoices` (`invoice_id` + `badge_purchase_id` in one row); nothing in this milestone writes that table, and whoever first does must not point it at a web order's invoice.
- **B10 — `markCodeRedeemed` now also guards on `revoked_at IS NULL`, and this one is load-bearing today.** The redemption half of the guard cannot fire while dispatch is single-threaded, but B8's `codes revoke` runs in a **second process** against the same database, and the service separates classification from the write by the signing IO — so a revocation issued in that window was silently overwritten and the code redeemed anyway, defeating the point of revoking a code that is being abused. With the guard, that write returns `SECodeConflict`, the existing one-retry path re-classifies, and the answer is `RedeemRevoked` → `code_invalid`. Store-level test added (`testBadgeStoreMarkCodeRedeemedRefusesRevoked`): the refusal also leaves the row untouched. Fixed in B10 rather than deferred to H2.
- **B10 — `offerTotal`'s multiplication is computed in `Word64` and range-checked back to `Word32`.** Both arms multiply a monthly price by a month count, and at 12 months a `Word32` product wraps above a monthly price of about 35,791 currency units. Unreachable at any plausible price — and so were the two *subtraction* wraps B6 closed by name, on exactly the principle that applies here: one malformed or absurd row must cost one unpriced offer, never a wrong charge. `minorUnits` refuses instead of wrapping, `Catalog.hs` remains the only place a total is computed, and no floating point enters the pricing path.
- **B10 — `badgeRequest.masterKey` is never checked against `badge_purchases.master_key`, and the concrete consequence is a recovery-flow bug for C3.** `createPurchase` stores the master key of the FIRST redemption and nothing revisits it. A second code redeemed under the same purchase key with a *rotated* master key takes the `IssueCached` path, which returns the stored issuance's credential — bound to the **old** master key, which the client cannot use, with no error anywhere. Every path that signs afresh signs the key that was sent, so the two disagree silently. C3/C4 either keep the master key stable per purchase key or the service must refuse a changed one; this is not decided anywhere yet.
- **B10 — `wasPausedSince` is copied from the last stored entry onto every row a plan appends, including a healed `debit(lapse)`.** Harmless while pausing is out of scope (nothing ever sets it), and wrong the moment pausing lands: a healed row would carry a stale pause marker. `writeLedgerPlan` and `healLedger` must be fixed **together** — they are one writer today precisely so this cannot drift, and whoever implements pause owns both.
- **B10 — a signed RPC from an unknown purchase key is unthrottled and costs a database round trip.** `checkSignerRecord` runs `getPurchaseByKey` before any bucket is consulted, and §5 rules H1's per-IP limits inapplicable to the RPC path — so nothing is scheduled to close this. Availability only: the answer is `unknown_purchase_key` and leaks nothing, but a minted keypair buys an unbounded number of indexed lookups. **For H1**, and it needs a decision, since the per-signer bucket cannot bound a key that has never failed anything.
- **B10 — B9's address file can go stale after all, in the one case its §9 entry does not cover.** That entry documents three *write* failure modes; this is a *read* failure. If `ShowMyAddress` fails transiently at startup, `publishServiceAddress` logs and returns without touching the file, so the previous contents survive — which is exactly the "never silently stale" property the entry claims. Rare and non-fatal (the address is also on stdout), but the claim is stated unconditionally. **For H5**, when the operator procedure is documented.
- **B10 — §7's unlinkability does not hold against a TIMING adversary, and that is a disclosed limit, not a bug.** `codes.created_at` for a web order is within seconds of that order's `web_orders.settled_at`, and its `batch` is the literal `'web'`, so an adversary with the service database can correlate an order to the code row it produced — and thence, once redeemed, to `redeemed_purchase_id` — **with no `codeSecret` needed**. The cryptographic claim (an order id cannot be turned into a code without the secret) is unaffected; the correlational one is what breaks. Mitigations belong to E3/H5: coarsen `codes.created_at`, or drop the `'web'` batch label, or both. §7 should say "no *stored reference* links an order to a purchase" rather than claiming unlinkability outright.
- **C1 — the client store's tests are `tests/Bots/BadgeStoreTests.hs`, not `tests/BadgeTests.hs`.** The step's Files and Verify both named `BadgeTests.hs`, which is a plain `Spec` with no `TestParams` (A2); these tests need a migrated chat database with a `users` row, which that module has no way to provide. Registered instead under the `Supporter badges store` path *inside* `testBracket`, which is precisely the case §4 rule 8 already provides for ("a spec that needs `testBracket`'s controller but not the service goes under `Supporter badges` in its own module registered inside that bracket"), and which C3's brief anticipates for the same reason. No chat controller is started; only `createDatabase`/`createUserRecordAt` are used. Files and Verify corrected above.
- **C1 — the client purchase row is a new `UserBadgePurchase`, not `Types.BadgePurchase`.** `BadgePurchase` (`Badges/Types.hs:124`, still marked *to review*) declares `priceId`, `offerId` and `credential`, and none of the three is a column of the client's `badge_purchases`: price and offer live on `badge_invoices` rows (one per invoice, so not a function of the purchase) and the credential lives on `badge_issuances` rows (one per period). Returning it would mean inventing three fields the database does not hold. `Store/Badges.hs` therefore declares `UserBadgePurchase` with exactly the table's columns, the same decision `BadgeService.Store.BadgePurchaseRow` made on the service side. **C2 must build `UserBadgeState.badges` from that row plus its own joins**, or change `BadgePurchase` — either is fine, but it is C2's call, not a rename C1 could make blind.
- **C1 — `badge_purchases.user_id` and `purchase_priv_key` are nullable and read as `Maybe`.** `20260731_user_badges` adds both with `ALTER TABLE`, which cannot add a NOT NULL column without a default, so the schema cannot express the invariant that `createPurchase` (the only writer) always sets them. `getShownPurchase` names a NULL as `SEInternalError` rather than returning a purchase whose private key is missing — the key is the whole reason the row is read back.
- **C1 — deleting a stale ledger row could in principle violate `badge_issuances.entry_id`'s foreign key.** The REPLACE path deletes `badge_ledger` rows the service no longer holds, and `badge_issuances.entry_id REFERENCES badge_ledger` has no `ON DELETE` clause. Unreachable today, because nothing in this plan writes a non-NULL `entry_id` on the client (`NewBadgeIssuance.ledgerEntryId` exists to mirror the service's record and C3 passes `Nothing`), and it can only arise if the service both drops an already-issued entry and the client had linked an issuance to it. **For C3**, which decides whether to link issuances to local ledger rows at all: if it does, the REPLACE path must null those references before deleting.
- **C1 — `payments.provider` is still written as a bare `'code'` literal on both sides.** `PaymentProvider` (`PaymentService/Types.hs:36`) has no `TextEncoding`, so `Store/Badges.hs` repeats `BadgeService/Store.hs`'s `codePaymentProviderText`. Two literals, one spelling, in two different databases that are never compared. **For D0/E2/F1**, as that entry already says: the instance should be added when a second provider needs writing.
- **C2 — `UserBadgeState` is reshaped and the draft `BadgePurchase` is deleted.** C1's entry above left this to C2. `Badges/Types.BadgePurchase` could not be the API shape: it carries `purchasePrivKey` and `masterKey`, and neither may cross the FFI (the private key signs `issueBadge`; the master key is the unlinkability secret), quite apart from the three fields C1 named. `UserBadgeState.badges` is now `[UserBadge]`, a new record holding only what the client can read and the app may see — `badgePurchaseId`, `badgeType`, `status`, `monthsLeft`, `paidThrough`, `createdAt`. `UserBadgeState` keeps `badges` and `shownBadgeId` and loses `payments`, `renewsAt`, `willRenew` and `alert`: nothing in this plan can fill them (alerts, subscriptions and renewals are §6), and a field that is permanently `Nothing`/`[]`/`False` documents a feature that does not exist. `paidThrough` is per badge rather than also at the top level, so the shown badge's paid-through date has ONE source; the app resolves it through `shownBadgeId`. The draft `BadgePurchase` is deleted rather than left unused — its row half is C1's `UserBadgePurchase` and its API half is `UserBadge` — which also removes the `import Simplex.Chat.Badges hiding (BadgePurchase (..))` clash with the unrelated `Simplex.Chat.Badges.BadgePurchase` (the payment-proof sum, which is in use). `badges` holds at most one entry today: `Store/Badges.hs` (unchanged, per the step) exposes only `getShownPurchase`, and with the paid slot the only slot in use the shown purchase IS the profile's badge.
- **C2 — the error's message field is `badgeErrorMessage :: Maybe Text`, and `retryAfter` is `Maybe Word32`.** The step and core §5 both write `CEBadgeServiceError {badgeError, message, retryAfter}`. `message` is not expressible: a Haskell record field has one type across a whole data type, and `ChatErrorType.message` is already `String` in **15** of that type's constructors (`CEInvalidChatMessage`, `CEGroupInternal`, `CEFileNotFound`, `CEFileAlreadyReceiving`, `CEFileCancelled`, `CEFileCancel`, `CEFileWrite`, `CEFileRcvChunk`, `CEFileInternal`, `CECommandError`, `CEAgentCommandError`, `CEInvalidFileDescription`, `CERelayTestError`, `CEInternalError`, `CEException`). Reusing it as `String` would have meant mapping the wire's absent message to `""`; the field is named apart instead and keeps the wire's `Maybe`. `retryAfter` is `Word32` rather than core §5's `Int` to match `BSPError.retryAfter`, which is its only source, so C4 needs no conversion. Both appear in the regenerated `TYPES.md`, `types.ts` and `_types.py`.
- **C2 — the new commands go in `undocumentedCommands`, not `cliCommands`, and neither list documents anything.** The step said to add them to `cliCommands` "so they reach `bots/api/COMMANDS.md` and both generated clients". Both lists are exemptions from `tests/APIDocs.hs:76`'s completeness check; a command reaches `COMMANDS.md` and the clients only through `chatCommandsDocs`. `cliCommands` holds the terminal-syntax commands and `undocumentedCommands` the `API*` ones, so the three `API*` commands go in the latter. The only regenerated files are therefore `bots/api/TYPES.md`, `types.ts` and `_types.py`, all three changed by `CEBadgeServiceError`/`BadgeServiceErrorCode` alone; `COMMANDS.md` and `EVENTS.md` are unchanged, and the step's Files line is corrected accordingly. Documenting the three commands properly belongs with C4, when they do something.
- **C2 — the step's last manual check contradicts the step.** "`/_badge catalog 1` reaching the service at the overridden address" cannot hold while the same step stubs `APIGetBadgeCatalog` with `not implemented`. What was checked instead: `--badge-service-address` parses a contact link and is rejected when it is not one, `/_badge catalog 1` and `/_badge purchase 1 <json>` both return the stub `CEBadgeServiceError`, and the issuer-key override is exercised end to end through `/badge add` — a credential signed by a locally generated key at index 1 verifies with `--badge-issuer-key 1:KEY` and fails without it ("does not verify against configured key"), while a credential at index 2 fails with "unknown badge key index" when only index 1 is overridden, which is what proves REPLACE rather than merge. Verify corrected above. **C4 owns the reaching-the-service check.**
- **C2 — `addUserBadge` reads `badgeCurrentTime`, not `getCurrentTime`.** The step introduces the field for C3 and C4 only. Wiring the one existing client-side badge clock read through it too costs nothing (the default is `getCurrentTime`, so `AddBadge` is unchanged) and means the field is load-bearing from the step that adds it rather than from the step that first needs to override it. **It has no behavioural test yet** — nothing in this step can observe a different clock — so C5 is the first place the override is proved to work.
- **C3 — `badgeWorkers` is a `ChatController` field, not a `BadgeManager` record.** The step's code block wraps one `TMap UserId Worker` in a newtype, from core §6's five-field record; the other four fields (`badgeLocks`, `badgeReads`, `badgeBoundaries`, `badgeTimerAsync`) are all out of this step's scope — the lock is an ordinary entity lock over `entityLocks`, the timer is the worker's own loop, and the other two serve steps §6 defers — so the wrapper would wrap exactly one field and add an indirection at every use. The field sits beside `deliveryTaskWorkers` and `relayRequestWorkers`, which the same bullet says to follow.
- **C3 — `badgeReads` is not added and `APIGetBadgeState` records nothing.** Core §6 has the command record its request time there; the only thing that reads that map is the lapsed-balance signed `getBadgeCatalog`, which this step does not send. The command signals the worker and stores nothing.
- **C3 — the issued period is read from the statement's last `debit(badge)`, and no issuance is written without one.** The step says "`createIssuance` for the new period" without saying where the period comes from, and the wire carries neither end of it: the credential's expiry is `sundayAfter periodEnd`, which is not invertible, and `addMonths` is not additive under clamping, so stepping a month back from a balance start does not name the period either (the same trap `BadgeService.Service.resolveIssue` documents). `issue` sets `balanceStartTs` to the period's END and the service writes the `debit(badge)` last, so the statement's last entry gives `periodEnd` and the `balanceStartTs` in force immediately before it gives `periodStart` — from the entry before it in the statement, or from the last entry the client held. A statement carrying no `debit(badge)` still puts its credential on the profile, because a month the user paid for is never lost (B10), but writes no issuance row.
- **C3 — `NewBadgeIssuance.ledgerEntryId` stays `Nothing`.** C1's entry above left this to C3. `badge_issuances.entry_id REFERENCES badge_ledger` has no `ON DELETE`, so linking issuances to local ledger rows would oblige every REPLACE to null those references before deleting — for a join nothing in this plan reads. The client's issuances therefore carry no ledger reference; the service's own rows still do.
- **C3 — the master key of `issueBadge` is the purchase row's, never a freshly generated one.** The service does not check `badgeRequest.masterKey` against `badge_purchases.master_key` (§9, above), so a second redemption under the same purchase key with a rotated master key takes the cached-issuance path and is answered with a credential bound to the OLD key — unusable, and with no error raised anywhere. Keeping the key stable per purchase closes that from the client side, without a service change.
- **C3 — the pass is two functions, split at the lock.** The step's Verify line names one "apply-response function", but the presentation must happen with the badge lock RELEASED (C2's `presentUserBadgeToContacts` takes `chatLock`), so the pass is `withBadgeLock … (… storeBadgeIssueResponse …) >>= presentBadgeChange`. Both halves are production functions and the tests compose them exactly as the pass does; only the stubbed send sits between them. C4's redeem path has the same obligation.
- **C3 — the worker's timer is `race_` against `threadDelay'`, not `timeout`.** `badgePassInterval` is a `NominalDiffTime` (following `cleanupManagerInterval`) and its default of a day is 86,400,000,000 microseconds, which does not fit an `Int` on 32-bit builds; `threadDelay'` takes an `Int64` and chunks it, as the delivery and relay-request workers already use it. The loop waits on `takeTMVar doWork` racing that delay, so the signal is CONSUMED before the pass reads any state and a signal arriving during a pass runs one more pass rather than being swallowed by it.
- **C3 — a badge request that fails is reported to the app.** Neither the step nor core §6 says what to do with a `BSPError` from a pass. It is surfaced with `eToView` as `CEBadgeServiceError`, as the delivery worker surfaces its failures, which is also what makes a pass observable to a test while `sendBadgeRequest` is a stub.
- **C3 — `badgeCurrentTime` is proved by C3, not by C5.** C2's entry above says C5 is the first place the override is proved to work. It is proved here: `Bots.BadgeManagerTests` injects a clock that counts and gates the passes, which is what lets the worker tests be deterministic without waiting on `badgePassInterval` or on wall-clock time at all.
- **C3 — the Postgres cross-check was not run for this step.** SQLite-only: `cabal test --test-options='-m "Supporter badges" -m "Badge service"'`, `CI` unset. The `--flags=client_postgres` cross-check B10 established is optional per-step and was skipped here rather than run; nothing in this step is guarded `#if !defined(dbPostgres)`, so there is no reason to expect a backend-specific gap, but it has not been exercised against Postgres and that remains open until it is.
- **C3 review round — a pass for a non-active profile silently switched the active user. Fixed.** `startBadgeWorkers` starts a worker for every user from `getUsers`, not just the active one, and the daily timer re-fires each — so once `sendBadgeRequest` is live (C4), a background profile crossing a month boundary would overwrite `currentUser` with itself, changing the app's active profile with nothing having asked. Latent under C3 alone because the stub never issues. Fixed with the same read-then-compare-then-write idiom already used at `Commands.hs:576,1647,4373`: `storeBadgeIssueResponse` now reads `currentUser` and only writes the pass's `User` back when its `userId` is the one already active. The Do bullet above is corrected to state this rather than "writing the `User` it returns to the `currentUser` TVar" unconditionally, which is what produced the bug — that phrasing was written from a single-profile viewpoint.
- **C3 review round — the worker loop did not gate on `waitChatStartedAndActivated`. Fixed.** Every other periodic loop in this module gates each iteration on it (`cleanupManager`, `runRelayGroupLinkChecks`, `expireChatItems`); `runBadgeWorker` did not, so after `APISuspendChat` the timer still fired passes that read the store and, once C4 lands, would attempt a signed send through a suspended agent. `lift waitChatStartedAndActivated` now sits at the top of the `forever` body, before the `race_`.
- **C3 review round — a shown purchase with no ledger rows stalled silently. Fixed with a log line.** Unreachable via C1's `createPurchase`, which always inserts an opening credit, but nothing signalled the stall if it ever happened. `issueDueBadgePeriod` now `logWarn`s the purchase and user id in that case rather than falling through to `BadgeUnchanged` with no trace.
- **C3 review round — a healed ledger's complete history could write a duplicate issuance row. Fixed.** `badge_issuances` has no uniqueness on `(badge_purchase_id, period_start)`, and `createIssuance` mints a fresh id per call; the "no duplicate issuance from a re-delivered history" argument above covers the append case but not REPLACE-after-heal, where the service returns a complete history whose last entry is a `debit(badge)` the client already holds an issuance for. `Store/Badges.hs` adds `hasIssuanceForPeriod`, checked before every `createIssuance` call.
- **C3 review round — the lock ordering that is this step's central risk has no regression test.** `badgeManagerPass` reads the user row, then takes the badge lock and runs `issueDueBadgePeriod`, then releases it before `presentBadgeChange`. This is guaranteed by source shape alone — nothing tests that a concurrent operation on the same profile cannot interleave between the lock's release and `presentUserBadgeToContacts`. `BadgeGate`'s doc comment in `BadgeManagerTests.hs` was also corrected: the gated clock parks a pass after `badgeManagerPass`'s own `getUser` but before `issueDueBadgePeriod` reads the purchase or ledger, not "before it reads any state".
- **C3 review round — `APIGetBadgeState` racing `stopChatController`'s worker-map swap can re-insert an uncancelled worker. Recorded, not fixed.** `getAgentWorker` can create and insert a fresh entry into `badgeWorkers` between the map being swapped to empty and the old workers being cancelled; that entry is then never cancelled. Same shape as a pattern already present in the agent's own worker bookkeeping elsewhere in the codebase, not introduced by this step. Left as a known gap: `stopChatController` runs once per process shutdown, and the window is one `getAgentWorker` call wide.

- **C4 — `sendBadgeRequest` also takes the `NetworkRequestMode` and the `User`.** The step's signature is `Maybe C.PrivateKeyEd25519 -> BadgeServiceRequest -> CM BadgeServiceResponse`, but the agent's service request is created under an agent user (`aUserId`) and resolves a short link or a SimpleX name under a request mode. Reading the ACTIVE user inside the send would put C3's per-profile pass on whichever profile happens to be active — the same defect class as the `currentUser` write the C3 review round fixed — so the caller states the profile instead: the two commands pass `withUserId`'s user and `processChatCommand`'s `nm`, the worker passes its own pass's user and `NRMBackground`. It is still one send path with three call sites, not a second one.
- **C4 — a redemption code is presented once per tier, because the client cannot know which tier it funds.** The step, core §5 and UX 2.8 all have the RESPONSE state the badge type — and the client does need it stated, since `badge_purchases.initial_badge_type`/`current_badge_type` are `NOT NULL` and a code carries no tier (B3: 20 opaque characters). But B7 added, beyond its own step and for good reason (§9 above), a check that refuses a `purchaseBadge` whose `badgeRequest` names a tier other than the code's with `bad_request`, because `issueSignedBadge` signs the badge info it is handed. The two cannot both hold for a client that does not already know the tier, and no client does: G2's and G3's redeem views are a code field and a paste button, and a code may also come from `codes issue` (B8) or an order the app never opened. C4 therefore presents the code under `supporter`, then under `legend` — `codeBadgeTypes`, the two tiers `codes.badge_type` admits. The probe is bounded and costs nothing but a second round trip on a legend code: the tier check runs before anything is planned, so a refusal writes nothing, leaves the code unredeemed and debits neither throttle bucket (B10 [7.4]), and the service can only ever issue the tier the code funds, so no wrong-tier credential is reachable. **The clean fix is a service one and was not taken here:** `handlePurchaseCode` could sign the CODE's tier, overriding what the request named exactly as it already overrides `badgeExpiry`, which keeps B7's security property (a credential can never exceed its funding) and makes the redemption one request again. That is a change to another step's code and to a mutation-proved security assertion (B10 item 6), so it is recorded for the plan owner rather than made by C4.
- **C4 — `sendBadgeRequest` returns its failures as `BSPError internal` and never throws.** An unconfigured `badgeServiceAddress`, a timeout, an agent failure and a response that does not decode are all answers to the request. Both callers already handle a service error — the worker reports it and keeps its state, the commands raise it as `CEBadgeServiceError` — and raising here would hand the worker's pass a chat error of a different shape for the transport half of the same operation. This is also what C3's report asked for ("a timeout must surface as a `BSPError`, not an exception").
- **C4 — nothing is written when the response cannot be recorded in full.** A credential that fails verification, one carrying no expiry, a statement with no issued period, and a `badgeCredential` with no credential at all each raise `CEBadgeServiceError` and write NOTHING — the payment, purchase, ledger, issuance and shown-badge rows are one transaction and none of them lands. This extends C3's rule for a failed verification to the redeem path's other three malformed shapes: a response we cannot record completely is not one to record partially. The code is consumed in those cases, and the recovery is the same one a lost response has, `codes unredeem` (H2).
- **C4 — `APIPurchaseBadge` answers `CRBadgeState` and emits no event.** Nothing in the plan states its response. `CEvtBadgeChanged` is documented as the worker's ("emitted whenever the badge worker changes a profile's badge state"), and the redeeming client gets the new state in the response, so emitting both would report the same change twice to the one caller that asked for it.
- **C4 — the store payment cases of `APIPurchaseBadge` are refused, not ignored.** `BPPApple` and `BPPGoogle` carry the `payments.payment_id` of a row the app's store purchase flow created, and neither that flow nor a writer for that row exists in this plan (§6). Both answer `CEBadgeServiceError internal "badge store payments are not supported"` rather than falling through to a code redemption they are not.
- **C4 — `resolveServiceTarget` is now a top-level function.** It was a `where` clause of `APISendServiceRequest`, and `sendBadgeRequest` needs the same resolution, because a badge service address may be published as a full contact request, a short link or a SimpleX name (B9). One function, two callers, no second resolution.
- **C4 — two `BadgeManagerTests` expectations changed.** They asserted C3's stub answer, `badge service error: internal, not implemented`; with the real send path a test controller, which configures no `badgeServiceAddress`, fails before contacting anything and reports `badge service error: internal, badge service address is not configured`. Same assertion, same shape, new message; no test was weakened or removed.
- **C4 — the Postgres cross-check was not run for this step.** SQLite-only, `CI` unset: `cabal test --test-options='-m "Supporter badges" -m "Badge service"'`. Nothing in this step is guarded `#if !defined(dbPostgres)` and it adds no SQL of its own — every write goes through C1's store functions — so there is no reason to expect a backend-specific gap, but it has not been exercised against Postgres.

## 10. End-to-end verification

After F5:

```bash
# 1. keys and provider secrets
simplex-chat badge keygen > issuer.keys        # two lines: "secret <b64url>", "public <b64url>"
head -c 32 /dev/urandom | base64 > code.secret
head -c 32 /dev/urandom | base64 | tr -d '\n' > btcpay.hmac
head -c 32 /dev/urandom | base64 | tr -d '\n' > btcpay.key
cp btcpay.key stripe.key; cp btcpay.hmac stripe.hmac

# 2. configuration
cat > badge_service.ini <<'EOF'
[service]
address_file = ./badge-service.link

[web]
port = 8080
host = 127.0.0.1
base_url = http://localhost:8080
support_contact = https://simplex.chat/contact

[issuer]
key_file = ./issuer.keys
key_idx = 1

[codes]
secret_file = ./code.secret
default_expiry_days = 365

[btcpay]
url = http://localhost:9000
store_id = test
api_key_file = ./btcpay.key
webhook_secret_file = ./btcpay.hmac

[stripe]
secret_key_file = ./stripe.key
webhook_secret_file = ./stripe.hmac
EOF

# 3. the provider mock (E1)
cabal run badge-payment-mock -- --port 9000 --service-url http://localhost:8080 \
  --btcpay-secret-file ./btcpay.hmac --stripe-secret-file ./stripe.hmac &

# 4. the service. -d is a path prefix: this creates ./badge_chat.db and ./badge_agent.db
simplex-badge-service -d ./badge --config ./badge_service.ini &   # or a second terminal

# 5. run a chat client against the local service (C2's terminal overrides).
# the issuer public key is the "public" line of issuer.keys: without it the client
# cannot verify the credential it is about to be issued.
simplex-chat --badge-service-address "$(cat ./badge-service.link)" \
  --badge-web-url http://localhost:8080 \
  --badge-issuer-key "1:$(awk '/^public/ {print $2}' issuer.keys)"

# 6. the site, prefilled as the app wizard would open it
open 'http://localhost:8080/?tier=legend&months=12&pay=xmr'

# 7. settle at the mock, then copy the code from the result screen
curl localhost:9000/_invoices                     # find the invoice id
curl -X POST localhost:9000/_settle/<invoiceId>   # delivers a signed webhook

# 8. redeem
/_badge catalog 1        # four offers with totals
/_badge purchase 1 {"type":"code","code":"SXB-…"}
/_badge state 1        # legend badge shown, 11 months of balance left, and the local site URL
```

Invariants that must hold:

- for the same purchase, the client's and the service's `badge_ledger` rows agree on `entry_uuid`, `change_months`, `balance_months`, `balance_start_ts`, `balance_badge_type` and the entry type — **not** on every column: `entry_id` is a per-database IDENTITY that means nothing across databases, and `payment_id` is a service-minted UUID on the service side and NULL on the client's, which has no `@payments` row for a code redemption (B10, §9). Comparing whole rows will fail; compare those six columns.
- re-sending the same `purchaseBadge` writes nothing and returns the same credential
- the same code from a second purchase key returns `code_used`
- a replayed webhook creates no second code, and an unprocessed one is reprocessed
- an invoice settling after expiry still yields its code
- no plaintext code exists anywhere in the service database: for a known test code `C`, `strings badge_chat.db | grep -F "$C"` is empty, and so is the same grep for the normalised form of `C` (uppercase, separators stripped)
- no row in the service database holds both an `orderId` and a `purchaseKey`
- no `orderId` or code appears in the logs
- the site renders correctly in light and dark themes at 320 px width, and surfaces fetch failures rather than blanking
