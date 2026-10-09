# Name codes in the badge service and webapp

## Context

SimpleX names (`<label>.simplex`, an ENS fork on Ethereum) are registered in the app with a code, as planned on `origin/ab/names-actions-api:plans/2026-09-28-name-registration-core-cli.md` (#7530). That plan says codes come from "the web store" and `//issue`, into the badge service's code table with a kind column, and that the badge service is renamed ShopService. No web store exists. This work builds it: the badge service and its webapp also sell **name codes**, with a Python mock to drive the webapp. Redemption (commit/reveal on chain, `BSCRedeemNameCode`) stays with #7530.

Facts from research that shape the design:
- A name code is an entitlement: "names of N+ letters for Y years", format `SN6-2Y-4K2P7-TQ9M1-ZX3RB-8HJ5W` (#7530 §6). It cannot be bound to a name: registration needs the owner's wallet address, which only the app has. The availability check is therefore advice, and the final screen says so.
- Contract minimum term is 730 days, so 2 years minimum. `.simplex` is not deployed yet (only `.testing` on mainnet).
- Availability comes from the resolver (`simplexmq: scripts/resolver/service/snrc-resolve.py`), `GET /v2/resolve/[<keccak256(label) hex>].<tld>` → `registration.type` = `available | registered | reserved`. It binds loopback with no CORS, so the service proxies it.
- Name rules (#7530 `validLabel`, simplexmq `nameLabelP`): `a-z 0-9 -`, lowercase-folded, no leading/trailing hyphen, no `--` at positions 3–4, 6–63 characters.
- The approved canvas (5f) has the store learn only the length. **Decided otherwise by the owner:** the page sends the name to our service for the check. The service hashes it before asking the resolver and never logs it.

## Decisions (owner, 2026-10-09)

| Topic | Decision |
|---|---|
| Name screen | Type name; page validates locally; service checks availability via resolver |
| Term | 2 years minimum, "+"/"−" stepper, max 10, price linear in years |
| Price | $50/year for 8+ letters; 7 letters $100/year; 6 letters $200/year |
| App shape | One app, one build; landing offers Badge or Name; payment/history screens shared |
| Code format | Adopt #7530 §6; implement the kind in core `Badges/Code.hs`, mirror in `web/src/codes.ts` |
| Final screen | Code, Copy, QR, and an "Open in SimpleX" link (format proposed below, for the app team) |
| Service scope | Web issuance, operator `//issue name` and group `/issue name`, ShopService rename |
| Storage | Same code table with `code_kind`; `badge_type`/`months` become nullable |
| Mockups | Static HTML on the real `styles.css`, rendered to PNG with Playwright |

## Stages

Each stage is its own set of atomic commits; stage 0 is reviewed before any code.

### 0. Design: flow board, spec and mockups (review gate)

Deliverables, all under `plans/names-codes/`:

1. **`names-flow.svg`**: one large board that shows every screen and outcome and how each screen leads to the next. It follows `plans/badges-codes/badges-flow-mvp.svg`:
   - a dark title bar: "SimpleX names: every screen, on the web and where it meets the app"
   - one tree per surface, read left to right
   - each screen drawn in its frame (a browser with URL bar on desktop, a phone frame on phone), with an ID tag and a caption: a bold "N1. Title" and a short paragraph saying why the screen exists and what it reads
   - arrows labelled with the action or outcome ("Check availability", "Continue", "Pay with Monero", "it confirms", "429")
   - arrow colours: blue for the normal path, orange for a variant or failure, grey for a platform difference, dashed for anything crossing between the site and the app
   - a footer of design notes
2. **`screens/*.png`**: each screen of the board on its own, rendered from `mockups/*.html` with Playwright Chromium. Light at 1280 wide; phone at 390; dark only for N0, N1b, N3 and N4, to check the palette.
3. **`mockups/*.html`**: one static page per screen state, built only from existing `web/public/styles.css` classes plus the new rules for the name field and the years stepper, which move into `styles.css` in stage 4.
4. **`2026-10-09-name-codes.md`**: the spec, following `plans/badges-codes/2026-08-27-badge-codes.md`. It covers the flow, the API, the codes, the schema and the copy, and embeds `screens/*.png` per section and the board at the top.

**Screen inventory**: the board holds every screen below. "Shared" means the badge screen is reused, drawn with name content.

*On the web, buying start to finish* (blue path):

| ID | Screen | Leads to |
|---|---|---|
| N0 | Landing: hero, "Get a badge" and "Get a SimpleX name" | B2 badge wizard; N1 |
| N1 | Name: empty field with `.simplex` suffix, rules line "6+ letters, a–z, 0–9, hyphens" | as typed: N1a |
| N1a | Typing: live tier and price ("7 letters · $100/year"); Check availability enabled | Check availability → N1b / N1c / N1d / N1e |
| N1b | Available: green "dynamis.simplex is available"; Continue enabled | Continue → N2 |
| N2 | Years: "− 2 years +" with minus disabled at 2, the total, a per-year line | + / − redraw it; Continue → N3 |
| N3 | Checkout: summary rows (Name, Covers "names of 7+ letters", Term, Total), Pay with Card / Bitcoin / Monero | Pay → N5 (crypto) or N8 (card) |
| N5 | Send N BTC/XMR (shared B5): QR, amount, address, reference, rate countdown | it confirms → N6 → N4 |
| N6 | Payment received, waiting to confirm (shared B5b) | settled → N4 |
| N4 | Code: tick, "Paid. Here is your name code.", code frame `SN7-2Y-…`, Copy code, "Open in SimpleX", QR, "Register it in the app", warning "Not reserved until you register it. This code covers any name of 7+ letters for 2 years." | Open in SimpleX → A1 (dashed) |

*On the web, the name screen's other outcomes* (orange):

| ID | Screen |
|---|---|
| N1c | Too short: "6 letters or more" under the field; Check disabled |
| N1d | Not allowed: characters or hyphen rules ("only a–z, 0–9 and single inner hyphens") |
| N1e | Taken: "dynamis.simplex is registered"; Continue disabled; field kept for another try |
| N1f | Reserved: "This name is reserved" |
| N1g | Check unavailable (resolver down or service 503): "Could not check now. Try again." |
| N1h | Too many checks (429): "Try again in N seconds", Check disabled for that long |

*On the web, the years screen's edges:*
- N2a: max 10 years, plus disabled
- N2b: 6-letter tier at 3 years, showing the ×4 price

*On the web, refused at checkout* (orange; shared badge screens with name summary):

| ID | Screen |
|---|---|
| N3a | Method temporarily unavailable (503 `provider_unavailable`), shared B4b |
| N3b | Prices changed (`catalog_changed`), shared B4c, back to N1 |
| N3c | Too many attempts (429), shared B4d |
| N3d | That did not go through (network / 5xx / `bad_request`), shared invoice failure |
| N3e | An order already waiting (open order line; card awaiting confirmation blocks a second order) |

*On the web, the other ways a payment ends* (orange, shared):

| ID | Screen |
|---|---|
| N5a | Part of the amount arrived |
| N5b | Cancel invoice → canceled |
| N7a | Expired, nothing received |
| N7b | Expired, part paid ("quote the reference") |
| N8 | Card form (Stripe) |
| N8a | Card form did not load |
| N8b | Card submitted, waiting to confirm |
| N8c | Taking longer than expected |
| N4a | Code not saved in this browser |
| N4b | Paid, but the code is not on this device |
| N9 | Unknown order link |

*On the web, history:* N10 "Your codes" with name rows (paid with code and the chosen name, waiting for payment, expired) beside a badge row.

*On the web, on a phone* (390 px): N0, N1b, N1e, N2, N3, N5, N4. These show what stacks (QR first), what stays side by side (payment methods) and the stepper at phone width.

*Where it meets the app* (dashed; drawn grey as "not built here", from canvas 5g):
- A1: the app's register screen opened by the link, with code and name filled in.
- A1a: the name was taken between the check and redemption; the app keeps the code for another name.

*For the operator:*
- OP1: `//issue name 7 3` in `--run-cli`, printing `Code: SN7-3Y-…`.
- OP2: group `/issue name 6 years 2` and `/bulk name 8 count 3`, with the service's replies.

**How the board is built** (owner, 2026-10-09):
- `mockups/board.mjs` (committed) reads one layout table: per screen its ID, mockup file, frame kind, section, position and caption, plus the arrows with their labels and colours.
- It renders each mockup with Playwright (`screens/*.png`) and writes `names-flow.svg`: vector title bar, section headings, frames, arrows, labels and captions, with each screen embedded as a base64 PNG.
- Editing a mockup or the layout and running `node plans/names-codes/mockups/board.mjs` regenerates both the PNGs and the board.
- Playwright is not added to `web/package.json`. The script states its one-off install, as `web/README.md` does for its rendered review.

### 1. Rename to ShopService (pure refactor)

As #7530 §5:
- Add a JSON test first that pins every `BadgeService*` command/response/error tag, so the rename provably leaves the wire unchanged.
- Rename `Simplex.Chat.Badges.Service` → `Simplex.Chat.ShopService`; `BadgeService{Request,Command,Response,Version,ErrorCode}` → `ShopService…`.
- Constructor prefixes `BSC/BSP/BSE` → `SHC/SHP/SHE`, updating each `dropPrefix` in the same commit. `ChatConfig.badgeServiceAddress` → `shopServiceAddress`.
- `apps/simplex-badge-service` → `apps/simplex-shop-service`, `BadgeService.*` → `ShopService.*`, executable `simplex-shop-service`, `scripts/badge-service`, test modules and hspec labels.
- Persisted names stay: the `sx_badge_service_` table prefix and migrations table.
- Badge-domain modules keep their names (`Simplex.Chat.Badges*`, `Store.Badges`).
- Risk: `ab/names-actions-api` plans the same rename; coordinate so only one lands.

### 2. Core: the name code kind and label rule

- `src/Simplex/Chat/Badges/Code.hs` grows a `NameCode` beside `BadgeCode`, sharing the alphabet, `charValue` and `checkValue`. API:
  - `parseNameCode :: Text -> Maybe NameCode`
  - `randomNameCode :: TVar ChaChaDRG -> Int -> Int -> IO NameCode`
  - `nameCodeParams :: NameCode -> (Int, Int)`
  - `nameCodeHash`, `formatNameCode`, `nameCodeText`
- Grammar (on the alphanumeric-filtered, upper-cased text): `SN` + one digit minLength (6–8) + 1–2 digits years (2–10) + `Y` + 20 body characters. The last body character is the Luhn mod-32 check over the values of the parameter characters and the 19 payload characters, so `SN6`→`SN8` fails.
  - Canonical: `SN62Y` + 20 characters; hashed with SHA-256, like badges.
  - Display: `SN6-2Y-XXXXX-XXXXX-XXXXX-XXXXX`.
- `src/Simplex/Chat/Names.hs`: `validNameLabel :: Text -> Either LabelError Text` (fold case, rules above) and `nameTier :: Int -> Int` (len ≥ 8 → 8, else len). #7530 reuses both.
- Tests in `tests/BadgeTests.hs`: round trips, the check over the parameters, folding, rejected grammar. Fixed vectors are shared verbatim with `web/test/codes.test.ts`.

### 3. Service (`apps/simplex-shop-service`)

**Config `[names]`** (`Config.hs`, ini example, README):
- `resolver_url` (default `http://127.0.0.1:8000`), `tld` (default `simplex`; `testing` until deployment).
- Absent: no name sales, and `/api/name/*` answers `provider_unavailable`.

**Migration `20261009_name_codes`**, SQLite and Postgres:
- `@badge_codes`:
  - add `code_kind TEXT NOT NULL DEFAULT 'badge'`, `min_length INTEGER`, `years INTEGER`
  - make `badge_type`/`months` nullable
  - add a CHECK that the badge columns are set exactly for `badge` and the name columns exactly for `name`
- `@badge_code_invoices`: `price_id` nullable, add `name_price_id TEXT REFERENCES @name_prices`, CHECK exactly one of the two is set.
- New `@name_prices(price_id PK, min_length, year_price, currency, status, created_at)`, seeded insert-only like `seedCatalog` (`Store/Invoices.hs`): `name_8`/5000, `name_7`/10000, `name_6`/20000 cents.
- Postgres: `ALTER … DROP NOT NULL` and `ADD CONSTRAINT`.
- SQLite: table rebuilds following `src/Simplex/Chat/Store/SQLite/Migrations/M20230118_recreate_smp_servers.hs`, with `PRAGMA defer_foreign_keys = ON`, because `badge_purchases` and `badge_code_invoices` reference `badge_codes`. A migration test runs up/down with referencing rows present; this is the riskiest step.

**Catalog** (`Catalog.hs`):
- `NamePrice` and `priceName :: [NamePrice] -> priceId -> years -> Either CatalogRefusal PricedName`.
- Years outside 2–10 → `bad_request`; unknown or disabled price → `catalog_changed`; reuses `offerTotal`'s amount cap.
- `defaultNameCatalog` mirrors `web/src/names.ts` and the mock.

**Store** (`Store.hs`, `Store/Invoices.hs`):
- `InvoiceRow`'s `irBadgeType`/`irMonths` become `irItem :: CodeItem = CIBadge BadgeType Word8 | CIName Word8 Word8` (minLength, years). Decoding is driven by `code_kind`.
- `getBadgeCode` filters `code_kind = 'badge'`. A name row must never decode as a badge; the SN prefix already keeps the hashes apart.
- `insertBadgeCode` and the invoice inserts write the kind.
- `revokeCode` works for both kinds.
- The row's `min_length`/`years` are authoritative at redemption (#7530), so a client that bought an 8+ code with `SN6` text gains nothing.

**Web API** (`Web/Server.hs`):
- `GET /api/name/:label`:
  - Validate with `validNameLabel` → 400 `invalid_name`.
  - Compute keccak256 (crypton `Keccak_256`), then `GET {resolver_url}/v2/resolve/[hex].{tld}` with a 3 s timeout.
  - 200 `{status: available|registered|reserved, minLength, yearPrice, currency}`.
  - Resolver failure → 503 `provider_unavailable`.
  - Read rate limit. The label is never logged.
- `POST /api/invoice` accepts `{kind:"name", priceId, years, method, codeHash}` beside the unchanged badge body.
  - The code row is written unpaid with kind `name` and the price's `min_length` and `years`.
  - The response and `GET /api/invoice/:id` carry `kind:"name", minLength, years` instead of `badgeType`/`months`.
- Settlement (`Orders.hs`), poller, webhooks and cancel stay unchanged: they work on invoice ids and code hashes.
- New wire error `invalid_name`, added to `WIRE_ERROR_CODES` and the README table.

**Operator** (`Service.hs`, `Group/Command.hs`):
- `//issue name <6|7|8> [<years 2-10>] [paid|unpaid|free]`.
- Group commands `/issue name <len> [years <Y>]` and `/bulk name <len> [years <Y>] count <B>`, single use.
- Badge parsing is unchanged; `groupCommands` params text updated.

### 4. Webapp (`web/`)

- `src/names.ts` (pure):
  - `validateLabel` (mirror of `validNameLabel`), `tierOf`
  - `NAME_CATALOG` (price ids, year prices, `MIN_YEARS = 2`, `MAX_YEARS = 10`), `nameTotal`
- `src/codes.ts`: `generateNameCode(minLength, years)`, name `normalise`/`display`/`canonical`; `hash` shared.
- `src/api.ts`:
  - `checkName(label)`
  - `createInvoice` takes a badge or name request
  - response parsers accept the `kind:"name"` shape
  - `invalid_name` added to the error codes
- `src/domain.ts`/`store.ts`: `OrderRecord` gains optional `kind: "name"`, `minLength`, `years`, `label`. The label is stored only locally, for display; a record without `kind` is a badge.
- `src/flow.ts`: `Selection` becomes badge | name. Checkout generates the matching code kind; retry on `code_conflict` stays the same.
- `src/order.ts`: `codeIssued` carries the kind and parameters; history titles such as "dynamis.simplex · 6+ letters · 2 years".
- `src/nameScreens.ts` (new, keeps `screens.ts` from growing): `nameEntry`, `nameYears`, `nameSummary`, `nameCodeIssued`. Built from exported helpers in `screens.ts` (`el`, `button`, panel, back button, `summaryRows`, `copyControl`, `qrFigure`, `investPanel`).
- `src/main.ts`:
  - The wizard's steps become per-product lists (badge: tier/months/checkout; name: name/years/checkout) on the same track/rail. Routes are `#/name`, `#/name/years`, `#/name/checkout`.
  - The landing gets the two entries.
  - Panel builders call `nameScreens`.
- `src/embed.ts`: name hashes added to `ROUTE_HASHES`.
- `public/styles.css`: the stepper and name-field rules from the mockups.
- `public/sw.js`: `PRECACHE` gains the new modules. `npm run build` rewrites the hash, and the regenerated `index.html`/`sw.js` are committed.
- Open-in-app link, one function `openInAppUrl(code, label)` → `simplex:/name#code=<display code>&label=<label>`. **Proposal only**: the app does not handle it yet and needs the app team's confirmation.
- Tests (node:test, existing style): `names.test.ts`, name-code vectors, api parsers, flow checkout for names, screens structure, main-level name wizard (the existing `main-*.test.ts` patterns), sw precache tripwire.

### 5. Mock (`web/mock/server.py`)

- `GET /api/name/<label>`: same validation. `registered` for a fixed set (e.g. `simplex`, `dynamis`); `reserved` for e.g. `support1`; `POST /control/name/<label>/<status>` overrides one, to exercise races and the resolver-down state.
- `POST /api/invoice` with `kind:"name"`: mirrors the name catalog; invoices behave as today, including all `/control/*` payment controls.
- README "Running the mock" gains the name endpoints.

## Critical files

- Core: `src/Simplex/Chat/Badges/Code.hs`, `src/Simplex/Chat/Names.hs`, `src/Simplex/Chat/Badges/Service.hs` (→ `ShopService.hs`), `tests/BadgeTests.hs`
- Service: `Config.hs`, `Catalog.hs`, `Store.hs`, `Store/Invoices.hs`, `Store/{SQLite,Postgres}/Migrations.hs`, `Web/Server.hs`, `Service.hs`, `Group/Command.hs`, `README.md`, `badge_service.ini.example`; tests under `tests/Bots/BadgeService/` (→ ShopService), with a fake resolver beside the fake Greenfield in `WebTests.hs`
- Web: `src/{names,nameScreens}.ts` (new), `src/{codes,api,domain,store,flow,order,main,embed,screens}.ts`, `public/{styles.css,sw.js,index.html}`, `mock/server.py`, `README.md`

## Out of scope

- Name redemption and the chain client (#7530).
- In-app purchase.
- The app's handling of the open-in-app link.
- Renewals.
- Fixing the stale "development stand-in" docs found earlier; a separate change.

## Verification

- Haskell: `cabal build exe:simplex-shop-service` and the test filter for the service, SQLite and Postgres builds. Covers name code vectors, `validNameLabel`, the migration up/down with referencing rows, catalog pricing, `/api/name` against a fake resolver (available/registered/reserved/down/invalid), and name invoice → settle → code row paid with kind, min_length and years. Badge tests unchanged, wire tags pinned.
- Web: `npm run build` (hash committed) and `npm test`, all green.
- End to end on the mock with Playwright Chromium:
  1. landing → name → years (stepper to 3) → checkout → BTC → partial → confirming → settle → N4 code
  2. card path
  3. taken, reserved and resolver-down names
  4. history row
- Screenshots in light, dark and phone, compared against the stage-0 mockups.
- Kotlin compile is not needed: wire tags are unchanged.
