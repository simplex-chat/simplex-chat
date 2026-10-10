# SimpleX names store: purchase and registration on the web

## Context

SimpleX names (`<label>.simplex`, an ENS fork on Ethereum) get their own store at **simplex.domains**. The buyer searches a name. Then:

- if it is free, the page shows its price, the buyer pays by card, Bitcoin or Monero, and the page shows the registration steps the app shows (Committing, Waiting, Registering);
- once registered, the buyer gets a claim code that moves the name into the app.

Badges keep their own store at **badges.simplex.chat**. To the user these are two stores, each with a secondary link to the other.

Both stores run on the badge service, renamed **StoreService**, and on one webapp codebase. Redemption in the app (core and UI) is #7530's: `origin/ab/names-actions-api:plans/2026-09-28-name-registration-core-cli.md`. That plan also specifies the service's chain client, which this work shares.

Facts that shape the design:

- Registering needs an owner address. Only the app's wallet has one, so the service registers each web-bought name to **its own registrar address** and holds it until the buyer claims it with the app. The name is held for real from the moment it is registered, so it cannot be taken while unclaimed.
- The term is fixed here at **2 years**, the registry's minimum (730 days, per `simplexmq: protocol/simplex-messaging.md`). It runs from the web registration, not from the claim.
- Availability and records come from the resolver (`simplexmq: scripts/resolver`): `GET /v2/resolve/[<keccak256(label)>].<tld>` → `available | registered{nameRecord} | reserved`. A registered name's `nameRecord` lists its SimpleX contact and channel links. The service opens those with its own chat core to show their display name and picture.
- Name rules:
  - characters and hyphens from #7530 `validLabel`: `a-z 0-9 -`, no leading or trailing hyphen, no `--` at positions 3–4;
  - at least 6 characters from the controller's `minCharLength`, which the resolver reads;
  - at most 63 from simplexmq's label limit;
  - core rejects upper case, so the page folds it as the buyer types.
- `.simplex` is not deployed; `.testing` is live on mainnet.

## Decisions (owner, 2026-10-09 and 2026-10-10)

| Topic | Decision |
|---|---|
| Stores | Names at simplex.domains, badges at badges.simplex.chat; separate to the user, with secondary cross-links |
| Code | One webapp codebase with two shells (names, badges) sharing payment, card and history screens; one service |
| Service | The badge service is renamed **StoreService** (`apps/simplex-store-service`, `Simplex.Chat.StoreService`) |
| Search | The buyer types a name; the service answers availability and the price for that name |
| Taken | A taken name shows what uses it: address and/or channel, with display name, picture, link and deep link |
| Reserved | Shown as reserved, with a link to contact the SimpleX team |
| Price | Internal and changeable, possibly per name. The page never shows a price list, only the price of the name searched |
| Term | 2 years, fixed. No term screen |
| Registration | After payment the service registers the name to its registrar address. The page shows Committing → Waiting 60s → Registering, as in the app (canvas 9) |
| Ownership | The service holds the name until it is claimed: a claim code, and Open in SimpleX, move it to the app's wallet |
| Secondary | "Buy a code for any name of N+ letters", for registering later in the app (#7530's `SN<len>-2Y-…` code) |
| Code format | #7530 §6: `SN6-2Y-4K2P7-TQ9M1-ZX3RB-8HJ5W`, in core `Badges/Code.hs`, mirrored in `web/src/codes.ts`. A claim code (`SC…`) is bound to its name in the service |
| Storage | Name codes in the code table with `code_kind`; registrations in their own table |
| Mockups | The board and screens generated from the real webapp modules (`plans/names-codes/mockups/`) |
| One order per name | A second order for a name with an open invoice or a registration in progress is refused (`name_pending`); search shows it as "being registered" until that order expires or finishes (2026-10-10) |
| Claim code prefix | Claim codes are `SC<len>-2Y-…`, length codes `SN<len>-2Y-…`, so the app knows from the text whether to claim or register (2026-10-10) |

## The names store, screen by screen

1. **Search** (the store's first screen): a name field with `.simplex`, validated locally as typed (length, characters). Search asks `GET /api/name/:label`, and the answer appears on the same screen:
   - **available:** "Available for $X for 2 years" with Register enabled;
   - **taken:** "Used by" and one card per address or channel, each with picture, display name, Open (deep link) and the link;
   - **reserved:** "Reserved", with "Contact the SimpleX team";
   - **being registered:** another buyer's order holds it;
   - **check failed or rate limited:** shown as such.
   
   Secondary links: "Buy a code for any name of 6+ letters" and "Badges".
2. **Checkout:** summary (name, 2 years, total), Pay with Card / Bitcoin / Monero, and Pay.
3. **Payment:** the badge payment screens, unchanged.
4. **Registering:** the three steps, each ticked as it completes, with the transaction links. A reload resumes them; they are kept by the service, not the page.
5. **Registered:** "dynamis.simplex is yours until 10 Oct 2028". The claim code, Copy, Open in SimpleX and its QR. A warning that the name points nowhere until it is claimed, and that this browser keeps the only copy.

**Outcomes off the path:**
- **Taken between search and registration:** the order keeps its value. The page offers two ways to use it:
  - search another name to register with the same order, at most what was paid; a cheaper name refunds nothing;
  - keep it as a code for any name of that length; that is worth less than what was paid if the name carried a per-name price above the length price.
  
  The page says both.
- **Registration failed after paying:** "Paid, not registered". Retrying charges nothing. If it fails for good, refunds are handled manually by support. There is no refund path in the service.
- **Payment endings:** the badge screens (partial, expired, cancelled, card failures).

**The secondary flow:** choose the shortest length (6, 7, 8+) with its price, then checkout and payment as above. The result is an unbound `SN<len>-2Y` code for the app (#7530's redemption).

## Stages

Each stage is its own set of atomic commits. Stage 0 is reviewed before any code.

### 0. Design: board, spec and mockups (review gate)

Redo `plans/names-codes/`: the board `names-flow.svg`, `screens/*.jpg`, the spec `2026-10-09-name-codes.md` and the generator `mockups/`, for the store above. The board's sections:

- the names store start to finish;
- what a search can find, including a name another order holds;
- registration steps and their outcomes;
- the secondary code flow;
- payment endings (shared);
- where it meets the app (claim; grey, #7530);
- the badges store at badges.simplex.chat, its landing and cross-link only;
- phone width;
- the operator.

The generator stays as built, apart from more room for arrow lanes above each row. `screens.js`, `mockups.css` and `layout.mjs` change.

### 1. Rename to StoreService (pure refactor)

As #7530 §5, with StoreService for ShopService:

- A JSON test first pins every command, response and error tag.
- `Simplex.Chat.Badges.Service` → `Simplex.Chat.StoreService`, with constructor prefixes `BSC`/`BSP`/`BSE` → `STC`/`STP`/`STE` (`dropPrefix` updated in the same commit). #7530 proposes `SHC`/`SHP`/`SHE` for ShopService; nothing in `src`, `apps`, `tests` or `bots` starts with `STC`/`STP`/`STE`. `ChatConfig.badgeServiceAddress` → `storeServiceAddress`.
- `apps/simplex-badge-service` → `apps/simplex-store-service`, `BadgeService.*` → `StoreService.*`, executable `simplex-store-service`, scripts, tests and hspec labels.
- Persisted names stay: the `sx_badge_service_` prefix, the migrations table and the database. This is #7530's recommendation (D2), still open there.
- Coordinate with #7530 so the rename lands once.

### 2. Core: name code kind, label rule, Registration encoding

- `src/Simplex/Chat/Badges/Code.hs` gains `NameCode` (`parseNameCode`, `randomNameCode`, `nameCodeParams`, `nameCodeHash`, `formatNameCode`, `nameCodeText`).
  - The grammar is `SN` + minLength digit (6–8) + years + `Y` + 20 body characters, with a Luhn mod-32 check over the parameters and the payload.
  - The canonical form is hashed with SHA-256.
  - The same module reads `SC` claim codes, with an identical grammar.
- `src/Simplex/Chat/Names.hs`: `validNameLabel`, `nameTier`.
- The `Registration` ABI encoding and commitment, as one module both core and service use (#7530 D17). This is shared with #7530: build it once, where it lands first.
- Tests with vectors shared with `web/test`.

### 3. Service: search, prices, checkout

- **`[names]`** in the ini: `resolver_url`, `tld` (`testing` until `.simplex` is deployed), and the internal price rule.
- **`GET /api/name/:label`:**
  - validates and keccak-hashes the label, then asks the resolver;
  - for a registered name, opens each `simplexContact`/`simplexChannel` link with the chat core (as `APIConnectPlan` does) for display name and picture, and adds a deep link;
  - answers `available{price}`, `registered{entities}` or `reserved`;
  - uses the read rate limit and never logs the label;
  - caches link profiles briefly, so one search costs at most one fetch per link;
  - fetches each link with a timeout (3 s) and under its own rate limit, since a name's records can point at any server;
  - answers `pending` for a name held by another open order.
- **Prices:** `Catalog.namePrice :: [NamePrice] -> Text -> Either CatalogRefusal CurrencyAmount`, over the rows `readCatalogRows` reads, as `priceOffer` takes badge prices. It starts from a per-length table seeded insert-only (`name_prices`), with per-name overrides possible. The page shows what the service answers. Checkout reprices and refuses a changed price with `catalog_changed`.
- **Checkout:**
  - `POST /api/invoice` with `kind: "name"` (label, the price shown, method, and the hash of a claim code made in the browser), or `kind: "nameCode"` (minLength, price, method, codeHash).
  - A name invoice writes a `name_registrations` row (label, price, state `awaiting_payment`) and an unpaid claim code row bound to it.
  - Settlement moves the registration to `paid`, which the registration worker picks up (stage 4).
- **Migration `20261010_names`** (SQLite and Postgres):
  - `badge_codes` gains `code_kind`, `min_length` and `years`, and `badge_type`/`months` become nullable. SQLite needs a table rebuild with `PRAGMA defer_foreign_keys = ON` and an up/down test with referencing rows.
  - `badge_code_invoices` gains `name_price_id`, and its `price_id` (`NOT NULL REFERENCES badge_prices` today) becomes nullable, with a CHECK that exactly one is set. That is a second SQLite table rebuild.
  - New tables `name_prices` and `name_registrations`: invoice, label, code, secret, commitment, phase, tx hashes, block numbers, `expires_at`, claimed owner, timestamps.
- **`GET /api/invoice/:id`** adds `registration {phase, label, commitTx, registerTx, revealAfter, expiresAt, error}` for a name invoice. The long poll also wakes on phase changes, which extends what `Waiters` compares.
- **A `taken` order:** `POST /api/invoice/:id/name {label}` retargets it to another name at most its price, and `POST /api/invoice/:id/unbind` turns its claim code into a length code.
- **One order per name:** creating a name invoice refuses `name_pending` while another open invoice or unfinished registration holds the label. The hold ends when that invoice expires or the registration finishes.
- **The order view** carries `claimed`, so history can show a claimed name.
- **New wire errors:** `invalid_name`, `name_taken` and `name_pending`.
- **Operator:**
  - `//issue name <6|7|8> [paid|unpaid|free]`, and group `/issue name` / `/bulk name`, for unbound length codes;
  - `//revoke`, for either kind;
  - `//reissue <reference>`: a new claim code for a held, unclaimed name whose code was lost (N7b), revoking the old one. It is CLI only, since a group posts its replies to every member.

### 4. Service: registration on chain and claiming

Shares #7530 §10's chain client: JSON-RPC to the resolver host's reth node, a dry run of every transaction, serial nonces, EIP-1559 fees, receipts with replacement, and 3 confirmations. `[chain]` holds the RPC URL, chain id, contract addresses, registrar key and confirmation depth.

**Signing:**
- Recoverable secp256k1 is in #1843, merged into simplexmq's `names` branch, not `master`.
- EIP-1559 transaction signing (`signEip1559Tx`) is on the separate branch `ab/eth-tx`.
- Both must reach the simplexmq this service builds against.

**Prerequisite, before any of this stage is built:** confirm on the SNS contract source that a registrar can register a name to itself, set its records while it owns it, and then transfer the token with the registry node following it. No source in these repositories covers that path:
- the contract source is not here;
- the names v2 RFC has the user as `owner` from the start;
- its auto-reclaim remark suggests the registry node follows the token.

If the path does not hold, ownership falls back to the buyer giving an owner address.

`minCommitmentAge` is set per deployment (60 s in the docs), so the worker reads it from the controller rather than assuming it.

- **Registration worker**, one per paid registration, resumable after a restart:
  1. `paid` → commit `makeCommitment(Registration{label, owner = registrar, duration = 2y, secret, …})` → `committing`
  2. → `committed` once mined → `waiting` until `minCommitmentAge`
  3. → `registering`: send the register call
  4. → `registered` at receipt + 3 blocks, recording `expires_at`
  
  Each phase is published to the page's long poll.
- **Failure handling:**
  - A dry run reverting with `NameNotAvailable` → `taken`. The order offers another name of at most its price, or conversion to an unbound length code.
  - Other failures → `failed`, retried; still failing → "Paid, not registered", where a retry charges nothing.
- **Claiming:** a new service command `claimName {code, owner, nameLinks}`, at the next version.
  - It checks that the code is paid, bound and unspent and that the name is held.
  - It sets the resolver records the links ask for while the service still owns the name, then transfers the token and the registry node to `owner`, and marks the code spent.
  - The core and app side are #7530's.
- **Funding:** the deployed `.testing` controller's `register` is payable in ETH, so the registrar key holds ETH. #7530's `registerWithCredit` replaces this when the `.simplex` contracts deploy. This is an open question in the spec.
- **Cost risk:** the buyer pays a fixed USD price; the service pays the contract's ETH fee plus gas when it registers. A gas spike is the service's loss.
- **Key custody risk:** the registrar key is a hot key in the ini, and it owns every unclaimed name, so whoever takes it can transfer them all.
  - Keep the window short: claim reminders on the registered screen and in history.
  - The spec asks whether held names should move to a separate holding address with a key kept offline.

### 5. Webapp (`web/`)

- **Two sites from one build:** `build.js` emits `dist/badges/` and `dist/names/`. Each is a complete site root (`index.html`, `sw.js`, `assets/<hash>/`), so each host has its own shell, service worker scope and precache, and Caddy needs no rewrites.
- **New modules:**
  - `names.ts` (pure): `validateLabel`, the search result types;
  - `nameScreens.ts`: search, checkout, steps, registered, taken-meanwhile, secondary code flow;
  - `namesMain.ts`: the names shell's wiring, reusing `flow.ts`, `order.ts`, `api.ts` and `store.ts`.
- **Extended modules:**
  - `codes.ts` (name codes);
  - `api.ts` (`checkName`, name and name-code invoices, registration in the invoice view, `invalid_name`);
  - `domain.ts`/`store.ts` (`kind`, `label`, `minLength`, `years`);
  - `order.ts` (registration phases select the steps, registered and taken screens);
  - `main.ts` (the badges shell adds the "SimpleX names" cross-link).
- **Styles:** the name field, entity cards and step list, from `mockups.css`, into `styles.css`.
- **The open-in-app link:** `openInAppUrl(code, label)` → `simplex:/name#code=…&label=…`. This is a proposal for the app team to confirm.
- **Tests:** node:test in the existing style, plus the build-hash tripwire for both shells.

### 6. Deployment (`scripts/badge-service/`, renamed `scripts/store-service/` in stage 1)

Production is the split deployment in `scripts/badge-service/`:
- Caddy serves the folder the service exports at boot (`webapp_export_dir`, bind-mounted to `./web`) and proxies `/api` and `/webhooks` to the service.
- The service runs with `+client_postgres`, `network_mode: host`, and the ini mounted read-only.

Changes:
- **Dockerfile:** the web stage builds both sites; the runtime image carries `/srv/web/badges` and `/srv/web/names`.
- **Export:** `exportWebapp` copies both sites and injects the Stripe publishable key into both shells.
- **Caddy, per host:**
  - `badges.simplex.chat` → `web/badges`;
  - `simplex.domains` → `web/names`;
  - both proxy `/api/*` to the service; `/webhooks/*` stays on one host;
  - a deployment note gives the per-host blocks and CSP (Stripe, `frame-ancestors`), since the Caddyfile is not in this repository.
- **All-in-one mode** (`serve_webapp = on`, development): the service picks the site by `Host` against `[listener] names_hosts` (default `simplex.domains`). Every other host gets badges.
- **Chain access:** with host networking, the resolver (`127.0.0.1:8000`) and reth (`127.0.0.1:8545`) are reached on loopback when they share the host, or over a private network otherwise. The registrar key lives in the mounted ini, which `Dockerfile.dockerignore` already keeps out of the image.
- **Persisted names stay** (database `badge_service`, the `sx_badge_service_` prefix). The compose service, image, binary and ini path move to the new name with the rename.

### 7. Mock (`web/mock/server.py`)

- `GET /api/name/<label>` with fixed available, taken (with entities and images) and reserved names, and per-name prices.
- Name and name-code invoices.
- `POST /control/registration/<id>/<phase>` to step a registration through committing, waiting, registering and registered, or into taken or failed. It also exposes the existing payment controls.
- Both sites: `/names/` and `/` (badges) by path, and by `Host` when it is `simplex.domains`, so either can be driven locally.

## Order of work

- Stages 1–3 and 5–7 do not depend on the chain.
  - They are built and tested against the fake resolver, a fake JSON-RPC node and the mock.
  - A paid name order waits in `paid` until stage 4 lands.
- Stage 4 waits for its prerequisite (the custodial path on the contract source) and for the two signing branches to reach the simplexmq the service builds against.

## Out of scope

- The app and core side of claiming and redemption (#7530).
- Funding the registrar wallet (operations).
- Renewals, transfers between users, in-app purchase.
- Refunds (manual, by support).

## Verification

- **Haskell:** `cabal build exe:simplex-store-service` and the service and core test filters, on SQLite and Postgres. Covers:
  - code vectors, `validNameLabel`, the migration up/down;
  - `/api/name` against a fake resolver and fake link profiles;
  - the registration worker against a fake JSON-RPC node (commit, wait, register, a revert to taken, a restart mid-phase);
  - `claimName` (records, then transfer);
  - badges unchanged, with wire tags pinned.
- **Web:** `npm run build` (both sites, hash committed) and `npm test`.
- **Docker:** `docker compose build` in `scripts/store-service/`, and a run against local Postgres that exports both sites.
- **End to end on the mock with Playwright:**
  1. search → available → Bitcoin payment → steps → registered
  2. taken with entity cards, reserved, invalid
  3. taken during registration → another name
  4. the secondary code flow
  5. card
  6. both sites' cross-links
  
  Light, dark and phone, compared against the stage-0 board.
