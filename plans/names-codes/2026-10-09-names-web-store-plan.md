# SimpleX names store: purchase and registration on the web

## Context

SimpleX names (`<label>.simplex`, an ENS fork on Ethereum) get their own store at **simplex.domains**:
1. The buyer searches a name and sees whether it is free, and at what price.
2. The buyer pays by card, Bitcoin or Monero, or redeems a code.
3. The browser makes a wallet for the name, a 12-word recovery phrase, and the service registers the name to that wallet's address.
4. The buyer keeps the phrase. Imported as the master of an app with no wallet yet, it gives the app the name's key; the app shows the name after `bind account=0` or the chain scan (out of #7475's scope). An app that already has a wallet needs account import, not built yet.

Nothing is held in custody: the name belongs to the browser's wallet from its first transaction. Badges keep their own store at **badges.simplex.chat**; to the user these are two stores, each linking the other as a secondary option. Both run on the badge service, renamed **StoreService**, and on one webapp codebase.

The app side belongs to #7530 and #7475 (`origin/ab/names-actions-api:plans/2026-09-28-name-registration-core-cli.md` and `origin/ab/wallet:docs/rfcs/2026-09-10-wallet-keys.md`): #7530 redeems codes in the app; #7475 imports a name's 12-word phrase only as the device's single master, so importing it beside an existing master is still to be built (#7475 excludes keys its master does not derive). Editing a name's records is later app work, out of #7530's scope. #7530 also specifies the service's chain client, which this work shares.

Facts that shape the design:

- **Registering for any owner:** `register()` takes the owner as a field of the `Registration` struct, and `makeCommitment` hashes the struct rather than `msg.sender`. So the service can pay for and send both transactions while the owner is the browser's address (names v2 RFC, `origin/ab/names-purchase-and-management:docs/rfcs/2026-08-05-simplex-names-v2.md:172`).
- **The wallet:** 128 bits of entropy, shown as a 12-word BIP-39 phrase. The name's owner is account 0, `m/44'/60'/0'/0/0`, the path #7475 uses for account 0.
  - #7475's device wallet is one master per device: generated with 24 words, or imported from a 12–24-word phrase.
  - On a device with no wallet yet, importing the name's phrase as the master gives it the name's key at account 0; the app shows the name only after an explicit `bind account=0` or the chain scan, which #7475 leaves out (its known limit 2). A device that already has a master cannot add it as an account; that path is not built.
- **Term:** 2 years for a paid name, the registry's minimum (730 days, `simplexmq: protocol/simplex-messaging.md`). A code registers for its own term. A year on chain is 365 days (31,536,000 seconds, as simplexmq's `protocol/simplex-messaging.md` prices it), so the duration is `years × 365` days. The term runs from registration.
- **Availability and records** come from the resolver (`simplexmq: scripts/resolver`): `GET /v2/resolve/[<keccak256(label)>].<tld>` → `available | registered{nameRecord} | reserved`. `nameRecord` lists `simplexContact` and `simplexChannel` links.
- **Name rules:**
  - characters and hyphens from #7530 `validLabel`: `a-z 0-9 -`, no leading or trailing hyphen, no `--` at positions 3–4;
  - at least 6 characters from the controller's `minCharLength`;
  - at most 63 from simplexmq's label limit;
  - simplexmq lower-cases a name when parsing it (`SimplexDomain`'s `strP`, `labelHash`) and #7530's `validLabel` admits only a–z, so the page folds upper case as typed.
- `.simplex` is not deployed; `.testing` is live on mainnet.

## Decisions (owner, 2026-10-09 and 2026-10-10)

| Topic | Decision |
|---|---|
| Stores | Names at simplex.domains, badges at badges.simplex.chat; separate to the user, each linking the other as a secondary option |
| Code | One webapp codebase with two sites (names, badges) sharing payment and card screens; each site has its own history; one service |
| Service | The badge service is renamed **StoreService** (`apps/simplex-store-service`, `Simplex.Chat.StoreService`) |
| Search | A field with a magnifying glass, as dynadot.com draws it. Clicking the glass, pressing Enter, or the Search button at the bottom all search. The name is judged when searching, not while typing; the rules line ("6 letters or more…") appears only after such a mistake |
| Results | Available with its price; taken with what uses it (address and/or channel: picture, display name, link, deep link); reserved with "Contact the SimpleX team"; being registered by another order |
| Price | Internal and changeable, possibly per name; the page shows only the searched name's price |
| Term | 2 years for a paid name, fixed; no term screen. A code registers for its own `years` (investor codes: 7, 5 or 3, by investment date, as simplex.domains says) |
| One order per name | A second order for a name with an open invoice or a registration in progress is refused (`name_pending`); search shows it as being registered |
| Wallet | The browser generates a wallet for each order, 128-bit entropy as a 12-word phrase; a taken order retargeted to another name keeps it. All key cryptography is in the browser |
| Registration | The browser sends the owner address; the service sends commit and register from its funded key, owner = that address. The page shows Committing → Waiting 60s → Registering, as in the app (canvas 9) |
| Codes | `SN6-…`, `SN7-…`, `SN20-…`: the number is the shortest name the code covers, a convention only. `SN-…` codes carry no length. The service's table is the source of truth, and the page asks the service what a code is for |
| Paying with a code | Checkout has "I have a code" under Card / Bitcoin / Monero and Pay. A code that covers the name makes its price 0, and redeeming it on the web yields the name's wallet |
| Check your code | Under the invest block, "Check your code" opens a popup that asks what a code covers. While it is applied, a covered name's price shows as $0 (investor codes) |
| Codes not sold | Codes are not sold on the web. They come from the app (an owner decision; #7530 defers in-app purchase, and the names v2 RFC has it buy a name directly), from the operator (CLI, group), or from the service for a taken order; each registers one name |
| History | "Your names" lists names and their phrases, and codes. "Check a code" adds a code to the list with what it covers |
| Invest block | The Wefunder sentence, logo and link, with a names amount, $500+ as on simplex.domains today. It is a placeholder for now (a constant, as `crowdfunding.ts`) |
| Storage | Name codes in their own table, not a kind column on the badge code table; registrations in their own table |
| Mockups | The board and screens generated from the real webapp modules (`plans/names-codes/mockups/`) |
| Crypto in the browser | Vendor `@noble/secp256k1` and `@noble/hashes` (audited, MIT, no dependencies) at pinned versions, plus the BIP-39 English wordlist. WebCrypto covers PBKDF2-SHA512 and HMAC-SHA512. This ends the webapp's "no runtime dependencies" rule for these two |
| Opened from the app | The app's link carries its own owner (`simplex.domains/#/?name=<label>&owner=<address>`), so the name is registered straight to the app's account; the page shows that address before paying. No phrase is made, there is no "I have a code" (the app redeems codes itself), and the page ends with "Back to SimpleX" |

## The names store, screen by screen

1. **Search** (first screen): the field with `.simplex` and a magnifying glass. A mistake shows the rules line. Search asks `GET /api/name/:label`, and the answer appears on the same screen:
   - available, with its price, or $0 when an applied code covers it;
   - taken, with "Used by" cards, or taken and pointing nowhere;
   - reserved;
   - being registered;
   - check failed or rate limited.
   
   Under the main button: the invest block (Wefunder) and "Check your code".
2. **Checkout:** name, term (2 years, or the code's) and total; Pay with Card / Bitcoin / Monero; and "I have a code", which opens the code entry. When a code covers the name, the total is $0 and the button reads "Register with code".
3. **Payment:** the badge payment screens, their "Buy a new code" button reading "Get a new invoice", and the card screen with the name's rows; skipped when a code pays.
4. **Wallet and registration:**
   - At checkout the browser generates the phrase, derives the address and stores the phrase locally.
   - It sends the address with the order.
   - After payment the page shows the three steps, ticked as they complete, with transaction links. A reload resumes them.
5. **Registered:** "Registered for 2 years, until 9 October 2028, to 0x5aE1…7cD3" (a code's own years when paid by code).
   - The 12-word phrase, Copy, and a QR for the app to scan.
   - "Import it in the app".
   - Warnings: the phrase owns the name, this browser keeps the only copy, write it down.

**Outcomes off the path:**
- **Taken between search and registration** (only for names registered outside the store, given one order per name): the order keeps its value.
  - Paid: the buyer can register another name at most its price, searched on the order's own page (a cheaper one refunds nothing), or take a code for the original name's length (8+ for longer names) for 2 years, issued by the service.
  - By code: the code is released, unspent, for another name.
- **Registration failed after paying:** the service's own retries gave up: "Paid, not registered", and Try again charges nothing. Refunds are manual, by support.
- **A code that does not work:** invalid or revoked, already used, reserved by another unfinished registration, expired, not yet paid, or not covering this name (the name is shorter than the code's length).
- **Payment endings:** the badge screens.

## Stages

Each stage is its own set of atomic commits; stage 0 is reviewed before any code.

### 0. Design: board, spec and mockups (review gate)

Redo `plans/names-codes/` for the store above: `names-flow.svg`, `screens/*.jpg`, the spec `2026-10-09-name-codes.md`, and the generator inputs in `mockups/`. The board's sections, as `mockups/layout.mjs` lists them:

- simplex.domains: registering a name start to finish, and what a search can find;
- paying with a code: check, apply, and codes that do not work;
- when registering does not go through;
- opened from the app;
- where it meets the app (import the phrase; grey, #7475/#7530);
- refused at checkout;
- the other ways a payment ends (shared), with the names store's phrase pages;
- your names, the badges store, and the dark theme;
- simplex.domains on a phone;
- the operator.

### 1. Rename to StoreService (pure refactor)

As #7530 §5, with StoreService for ShopService:

- A JSON test first pins every command and response tag, as #7530 proposes, and also the hand-written `BSE` error tags.
- `Simplex.Chat.Badges.Service` → `Simplex.Chat.StoreService`, with constructor prefixes `BSC`/`BSP` → `STC`/`STP` (`dropPrefix` updated in the same commit), and `BSE` → `STE` in `Simplex.Chat.Badges.Types`, whose wire tags are written by hand and stay as they are.
  - #7530 proposes `SHC`/`SHP`/`SHE`.
  - `bots/src/API/Docs/Types.hs` builds the error docs from `consSep "BSE"` and changes with it; its `STEnum*` constructors start with `STE` but are unrelated.
- `ChatConfig.badgeServiceAddress` → `storeServiceAddress`.
- `apps/simplex-badge-service` → `apps/simplex-store-service`, `BadgeService.*` → `StoreService.*`, executable `simplex-store-service`, `scripts/store-service/`, tests and hspec labels.
- Persisted names stay: the `sx_badge_service_` prefix, the migrations table and the database. This is #7530's recommendation (D2), still open there.
- `plans/names-codes/mockups/board.mjs`, `mockups/layout.mjs` and spec §13 follow the new web path.
- Coordinate with #7530 so the rename lands once.

### 2. Core: name codes, label rule, Registration encoding

- **`src/Simplex/Chat/Badges/Code.hs`** gains `NameCode` (`parseNameCode`, `randomNameCode`, `nameCodeHash`, `formatNameCode`, `nameCodeText`).
  - The grammar is `SN`, an optional length (1–2 digits), then 20 Crockford base32 body characters, with a Luhn mod-32 check over the length digits and the payload.
  - The length is shown, not trusted: the table row decides.
  - The canonical form is hashed with SHA-256.
- **`src/Simplex/Chat/Names.hs`:** `validNameLabel`.
- **The `Registration` ABI encoding and commitment**, one module that core and service share (#7530 D17), built once where it lands first.
- **Tests:** vectors shared with `web/test`.

### 3. Service: search, codes, orders

- **`[names]` in the ini:** `resolver_url`, and `tld` (`testing` until `.simplex` is deployed).
- **`GET /api/name/:label`:**
  - validates and keccak-hashes the label, then asks the resolver;
  - for a registered name, opens each `simplexContact`/`simplexChannel` link with the chat core (as `APIConnectPlan` does, which returns the link's display name and picture without connecting), with a 3 s timeout, its own rate limit and a short cache, and adds a deep link;
  - answers `available{price}`, `registered{entities}`, `reserved` or `pending`; a searched label is never logged, and is stored only when ordered.
- **`POST /api/code/check {code}`:** returns what a code covers (`minLength` or none, `years`) and whether it can still be used (`unused`, `reserved`, `used`, `revoked`, `expired`, `unpaid`), without spending it. It uses its own rate limit, since it confirms codes.
- **Prices:** `Catalog.namePrice :: [NamePrice] -> Text -> Either CatalogRefusal CurrencyAmount`, over a seeded insert-only `name_prices` table (`Catalog.defaultNamePrices`): per-length rules, overridable per name. Checkout reprices and refuses a changed price with `catalog_changed`, which now carries the new price.
- **Orders:**
  - `POST /api/invoice {kind: "name", label, price, method, owner}` creates an invoice; settlement moves the registration to `queued`.
  - `POST /api/name/register {label, owner, code}` registers with a code. It checks that the code covers the label, marks it reserved for this registration, and spends it when the registration succeeds; there is no invoice.
  - `owner` is a checksummed address.
  - One order per name: both refuse `name_pending` while another open order holds the label.
- **A `taken` order:**
  - `POST /api/registration/:id/name {label}` retargets it to another name, at most what was paid;
  - `POST /api/registration/:id/code` takes a code for the original name's length (8+ for longer names), for 2 years, instead. The service generates it and returns it once, and stores only its hash, as operator codes are stored.
- **`GET /api/registration/:id`**, the long-poll view a code registration uses, and the same object in `GET /api/invoice/:id` for a paid one: `{id, phase, label, owner, years, commitTx, registerTx, revealAfter, expiresAt, error}`. The id is a random 128-bit token, like order ids; whoever holds it can act on the registration.
- **Phases:** `unpaid` (invoice open) → `queued` (paid, or code reserved) → `committing` → `committed` → `registering` → `registered`; or `taken`, `failed`. From `taken` a paid order goes back to `queued` with its new label, or ends `code_issued`; a code order ends at `taken` with its code released. A code is reserved from `queued` until it is `registered` (spent) or `taken` (released); `failed` keeps it for Try again.
- **`POST /api/registration/:id/retry`:** from `failed`, back to `queued`, at no charge.
- **Migration `20261010_names`** (SQLite and Postgres), each table with the `sx_badge_service_` prefix:
  - **`name_codes`:** `name_code_id`, `code_hash` (unique), `min_length` (null for an `SN-` code), `years`, `code_payment_status`, `spent_at`, `revoked_at`, `expires_at`, `created_at`. A code is single-use; it is reserved from `queued` until it is `registered` (spent) or `taken` (released); `failed` keeps it for Try again.
  - **`name_prices`:** `price_id`, `min_length`, `label` (null for a length rule), `price`, `currency`, `status`, `created_at`.
  - **`name_registrations`:** `registration_id`, `label`, `owner`, `secret`, `commitment`, `phase`, transaction hashes and blocks, `years`, `expires_at`, `invoice_id` (null when paid by code), `name_code_id` (null when paid), `error`, `created_at`, `updated_at`.
  - **`name_invoices`:** `invoice_id`, `registration_id`, `price_id`, `provider_ref`.
  - **The badge tables are untouched**, so there is no table rebuild.
  - **Provider-ref lookups:** the webhook and poller queries that find an invoice by `provider_ref` cover both `badge_code_invoices` and `name_invoices`. As in `Web/Server.hs` today, a row whose provider differs from the webhook's is ignored, since `provider_ref` is unique only within each table.
- **Wire errors:** new on HTTP and in `api.ts`'s `WIRE_ERROR_CODES`: `invalid_name`, `name_taken`, `name_pending`, `code_not_covering`, `code_reserved`, and the chat protocol's code errors `code_invalid` (unknown or revoked), `code_used`, `code_expired` and `payment_pending` (an unpaid code). A retarget to a name dearer than the payment is `bad_request`.
- **Operator:**
  - `//issue name [<length>] [years <Y>] [paid|unpaid|free]`, and group `/issue name [<length>] [years <Y>]` / `/bulk name [<length>] [years <Y>] count <N>`, issue single-use codes; without a length the code is `SN-…`, and without years it covers 2;
  - `//revoke` takes either kind.

### 4. Service: registration on chain

Shares #7530 §10's chain client: JSON-RPC to the resolver host's reth node, a dry run of every transaction, serial nonces, EIP-1559 fees, receipts with replacement, and 3 confirmations. `[chain]` holds the RPC URL, chain id, the controller and resolver contract addresses, the service's key and the confirmation depth.

- **Signing (the service's own transactions only):**
  - recoverable secp256k1 (`signRecoverable`) and EIP-1559 transaction signing (`signEip1559Tx`) are in simplexmq #1887, on `names` with #1843, not `master`;
  - `names` must reach the simplexmq the service builds against.
- **Worker**, one per registration, resumable after a restart:
  1. `queued` (paid, or a code reserved) → commit `makeCommitment(Registration{label, owner = buyer's address, duration, secret (service-generated), resolver, …})` → `committing`. `duration` is 2 years for a paid name and the code's `years` for a code;
  2. → `committed` once mined → wait `minCommitmentAge`, read from the controller;
  3. → `registering`;
  4. → `registered` at receipt + 3 blocks.
  
  Each phase is published to the long poll.
- **`taken`:** the register dry run reverted `NameNotAvailable`; nothing was sent, the payment or code is kept, and the outcomes above apply.
- **`failed`:** retried; still failing → "Paid, not registered".
- **Records:** the name is registered with no records. The owner sets them from the app after import, once the app can edit records (out of #7530's scope).
- **Funding and cost:** the deployed `.testing` controller's `register` is payable in ETH, so the service's key holds ETH and pays the fee and gas, while the buyer paid a fixed USD price. A gas spike is the service's loss. #7530 §9 sends `registerWithCredit`, funded by a USD allowance, and the names v2 RFC proposes credited registration for `.simplex` (with `setRegistrarCredits`, where #7530 has `setRegistrarAllowance`); either replaces ETH funding when deployed.
- **Key risk:** the service's key holds only funds, never names, so a stolen key loses ETH, not names.

### 5. Webapp (`web/`)

- **Two sites from one build:** `build.js` emits `dist/badges/` and `dist/names/`, each a complete site root (`index.html`, `sw.js`, `assets/<hash>/`).
- **Crypto** (`wallet.ts`):
  1. 128-bit entropy from `crypto.getRandomValues`;
  2. the BIP-39 phrase with its checksum;
  3. PBKDF2-SHA512 seed (WebCrypto);
  4. BIP-32 to `m/44'/60'/0'/0/0` (HMAC-SHA512 from WebCrypto, secp256k1 from `@noble/secp256k1`);
  5. the EIP-55 address (keccak from `@noble/hashes`).
  
  Tests pin the `abandon … about` vector against #7475's address for account 0.
- **New modules:**
  - `names.ts` (pure): `validateLabel` and the result types;
  - `nameScreens.ts`: search with the glass, results, the code popup, checkout with "I have a code", the steps, the registered screen with the phrase, outcomes, history with "Check a code";
  - `namesMain.ts`: wiring, including the flow opened from the app (`#/?name=&owner=`) and a code registration's page (`?registration=<id>`).
- **Extended modules:**
  - `codes.ts` (name codes);
  - `screens.ts` (exports the helpers `nameScreens.ts` builds on; the invest block in `invoiceFailure` becomes optional; `BUY_NEW_CODE` becomes a per-store label, "Get a new invoice" for names);
  - `flow.ts` (`catalog_changed` with its new price);
  - `api.ts` (`checkName`, `checkCode`, name orders, registration views, new errors);
  - `domain.ts`/`store.ts` (names with phrase and address, and checked codes for history);
  - `order.ts`;
  - `main.ts` (the badges site links simplex.domains);
  - `crowdfunding.ts` (the names amount, a placeholder).
- **Order errors on the page:** `name_pending` and `name_taken` go back to search showing N2f or N2a; code errors from the service show where the code was entered, the popup (K5) or the checkout entry (K7), and a typo is caught on the page (K6); `invalid_name` cannot come from a page that validates, and shows N3d.
- **Styles:** the search field with the glass, result and entity cards, the steps, the phrase grid and the popup, from `mockups.css`, into `styles.css`.
- **The phrase:** stored only in this browser. It is shown on the registered screen and in history, never sent anywhere; its QR is for the app to scan.
- **Tests:** node:test in the existing style, plus the build-hash tripwire for both sites.

### 6. Deployment (`scripts/badge-service/`, renamed in stage 1)

Production is the split deployment in `scripts/badge-service/`:
- Caddy serves the folder the service exports at boot and proxies `/api` and `/webhooks`.
- The service runs with `+client_postgres` and host networking, with its ini mounted read-only.

Changes:
- **Build:** the Dockerfile's web stage builds both sites.
- **Export:** `exportWebapp` writes both, with the Stripe publishable key in each shell.
- **Caddy:**
  - `badges.simplex.chat` → `web/badges`;
  - `simplex.domains` → `web/names`;
  - both proxy `/api/*`; `/webhooks/*` stays on badges.simplex.chat, where BTCPay and Stripe already send it;
  - `apps/simplex-badge-service/README.md` (renamed in stage 1) gains the host blocks, since the Caddyfile is not in this repository, and the CSP in `web/README.md` (Stripe, `frame-ancestors`) gains simplex.domains.
- **All-in-one mode** picks the site by `Host` against `[listener] names_hosts`.
- **Chain:** the resolver and reth on loopback when they share the host. The service's key is in the mounted ini, which `Dockerfile.dockerignore` already keeps out of the image.
- **Unchanged:** the database `badge_service` and the `sx_badge_service_` prefix.

### 7. Mock (`web/mock/server.py`)

- `GET /api/name/<label>`: fixed available, taken (with entities and images), reserved and pending names, and per-name prices.
- `POST /api/code/check`: fixed codes covering 6+, 7+, 8+ and `SN-`, one used and one revoked.
- Name orders by payment and by code.
- `POST /control/registration/<id>/<phase>` steps a registration, or sends it to taken or failed; `retry`, `name` and `code` follow the service.
- Both sites, by path (`/names/`) and by `Host`.

## Order of work

- Stages 1–3 and 5–7 do not depend on the chain. They are built and tested against the fake resolver, a fake JSON-RPC node and the mock; a paid order waits in `queued` until stage 4 lands.
- **Codes differ from #7530 §6/§9** (the `SN7-` format without years in the text, their own table, none sold on the web, and the service's table rather than the text deciding what a code covers, so the app cannot check one offline); agree with #7530 before stage 2.
- Stage 4 waits for simplexmq `names` (#1887's signing) to reach the simplexmq the service builds against.
- **The app side:** importing a name's phrase beside an existing master (#7475/#7530) is needed before a web-bought name shows in an app that already has a wallet. Until then, it can be the master of a device with no wallet (the name shows after `bind account=0`), and it works in any BIP-44 wallet at account 0.

## Out of scope

- The app side: importing a phrase beside an existing master (to be added to #7475/#7530), codes in the app (#7530), record editing (later app work).
- Funding the service's key (operations).
- Renewals, transfers, in-app purchase.
- Refunds (manual, by support).
- Selling codes on the web.

## Verification

- **Haskell:** `cabal build exe:simplex-store-service` and the service and core test filters, on SQLite and Postgres. Covers:
  - name code vectors and `validNameLabel`;
  - the migration up/down;
  - `/api/name` against a fake resolver and fake link profiles;
  - `/api/code/check`;
  - orders by payment and by code, including one order per name;
  - the worker against a fake JSON-RPC node (commit, wait, register, a revert to taken, a restart mid-phase);
  - badges unchanged, wire tags pinned.
- **Web:** `npm run build` (both sites, hash committed) and `npm test`, including the wallet vectors.
- **Docker:** `docker compose build` and a run against local Postgres that exports both sites.
- **End to end on the mock with Playwright:**
  1. search (glass, Enter, button) → available → Bitcoin → steps → phrase
  2. taken, reserved, pending, invalid
  3. "Check your code" → $0 → register with code → registered for the code's years
  4. "I have a code" at checkout, and codes that do not work
  5. taken during registration → another name, and → a code
  6. failed → Try again
  7. card
  8. opened from the app
  9. history with "Check a code"
  10. both sites' cross-links
  
  Light, dark and phone, against the stage-0 board.
