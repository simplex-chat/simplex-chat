# SimpleX names store: purchase and registration on the web

## Context

SimpleX names (`<label>.simplex`, an ENS fork on Ethereum) get their own store at **simplex.domains**:
1. The buyer searches a name and sees whether it is free, and at what price.
2. The buyer pays by card, Bitcoin or Monero, or redeems a code.
3. The browser makes a wallet for the name, a 12-word recovery phrase, and the service registers the name to that wallet's address.
4. The buyer keeps the phrase. Imported as the master of an app with no wallet yet, it gives the app the name's key; the app shows the name after `bind account=0` or the chain scan (out of #7475's scope). An app that already has a wallet needs account import, not built yet.

Nothing is held in custody: the name belongs to the browser's wallet from its first transaction. Badges keep their own store at **badges.simplex.chat**; to the user these are two stores, each linking the other as a secondary option. Both run on the badge service, renamed **StoreService**, and on one webapp codebase.

The app side belongs to #7530 and #7475 (`origin/ab/names-actions-api:plans/2026-09-28-name-registration-core-cli.md` and `origin/ab/wallet:docs/rfcs/2026-09-10-wallet-keys.md`): #7530 redeems codes in the app; #7475 imports a name's 12-word phrase only as the device's single master, so importing it beside an existing master is still to be built (#7475 excludes keys its master does not derive). Editing a name's records in the app is later app work, out of #7530's scope; the web store links them itself (below). #7530 also specifies the service's chain client, which this work shares.

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
| Hosts | One Caddy serves both sites, which the service exports. Each site proxies `/api` and `/webhooks` to the service on its own origin: no separate API host, no CORS. Each payment account is set to one webhook URL |
| Code | Two webapps sharing logic, not screens. Badges stay in this repo, built on the library. Names live in the simplex.domains repo in that site's look, and are a submodule of simplex-chat (`apps/simplex-domains`). The shared logic (API client, payments, storage, codes, QR) is `packages/simplex-chat-store`, published as `@simplex-chat/store`. One service exports both sites |
| Service | The badge service is renamed **StoreService** (`apps/simplex-store-service`, `Simplex.Chat.StoreService`) |
| Search | One search bar, as registrars draw it (dynadot.com): a search icon, the name, a `.simplex` tag and a Search button inside; Enter searches too. The name is judged when searching, not while typing; the rules line ("6 letters or more…") appears only after such a mistake |
| Results | Available with its price; taken with what uses it (address and/or channel: picture, display name, link, deep link); reserved with "Contact the SimpleX team" |
| Price | Internal and changeable, possibly per name; the page shows only the searched name's price |
| Term | 2 years for a paid name, fixed; no term screen. A code registers for its own `years` (investor codes: 7, 5 or 3, by investment date, as simplex.domains says) |
| No hold, hash until reveal | The service sees only keccak(label) and its length until the register step, so it holds nothing: two buyers may pay for one name, and the later one gets the taken outcome with the payment kept. Once its own register is sent, search answers registered |
| Wallet | The browser generates a wallet for each order, 128-bit entropy as a 12-word phrase; a taken order retargeted to another name keeps it. All key cryptography is in the browser. The phrase is shown before paying and confirmed as written down, and everything else is derived from it, so the phrase alone finishes or restores an order if the browser loses its storage |
| Registration | The browser sends the owner address and the commitment, and reveals the name and secret after the wait; the service sends commit and register from its funded key, owner = that address. The page shows Committing → Waiting 60s → Registering, as in the app (canvas 9) |
| Codes | `SN6-…`, `SN7-…`, `SN20-…`: the number is the shortest name the code covers, a convention only; every code has one. The service's table is the source of truth, and the page asks the service what a code is for |
| Paying with a code | Checkout's order card has "I have a code" under Continue. A code that covers the name makes its price 0, and redeeming it on the web yields the name's wallet |
| Check your code | The investor card on the first page and Check a code in the header open a popup that asks what a code covers. While it is applied, a covered name's price shows as $0 (investor codes) |
| Codes not sold | Codes are not sold on the web. They come from the app (an owner decision; #7530 defers in-app purchase, and the names v2 RFC has it buy a name directly), from the operator (CLI, group), or from the service for a taken order; each registers one name |
| History | "Your names" lists names and their phrases, with Edit links, and codes. "Check a code" adds a code to the list with what it covers; "Restore with a phrase" finds an order from its phrase and name (N7c) |
| Investor card | "Investors get a name free": the Wefunder sentence and link, with a names amount, $500+ as on simplex.domains today, and Check your code; a short version sits in the checkout's order card. The amount is a placeholder for now (a constant, as `crowdfunding.ts`) |
| Storage | Name codes in their own table, not a kind column on the badge code table; registrations in their own table |
| Mockups | The board and screens generated by `plans/names-codes/mockups/`: names screens from `domains.js` on simplex.domains' fonts and tokens, the badges landing from the real badges webapp |
| Crypto in the browser | Vendor `@noble/secp256k1` and `@noble/hashes` (audited, MIT, no dependencies) at pinned versions, plus the BIP-39 English wordlist. WebCrypto covers PBKDF2-SHA512 and HMAC-SHA512. This ends the webapp's "no runtime dependencies" rule for these two |
| Address links | Optional at checkout: a contact address and/or a channel link, previewed through the service, set by the register transaction itself (the commitment's `data`). Later edits are signed in the browser with the name's key and relayed by the service, once the names v2 RFC's `SimplexResolver` is deployed; until then the phrase imported into a wallet edits them through the dApp |
| Opened from the app | The app's link carries its own owner (`simplex.domains/#/?name=<label>&owner=<address>`), so the name is registered straight to the app's account; the page shows that address before paying. No phrase is made, there is no "I have a code" (the app redeems codes itself), and the page ends with "Back to SimpleX" |

## The names store, screen by screen

1. **Search** (first screen): the search bar with `.simplex` and Search. A mistake shows the rules line. Search sends keccak(label) and its length, `GET /api/name/:hash?length=7`, and the answer appears on the same screen:
   - available, with its price, or $0 when an applied code covers it;
   - taken, with "Used by" cards, or taken and pointing nowhere;
   - reserved;
   - check failed or rate limited.
   
   Below it: how it works, what a name does, and the investor card with "Check your code".
2. **Checkout:** the optional links (N3g) and the payment method beside an order card with term (2 years, or the code's), total, Continue, and "I have a code", which opens the code entry. When a code covers the name, the total is $0 and the payment method reads "Paid with your code".
3. **Payment:** the names app's own payment pages (crypto with QR and the held rate, card, and their endings, with "Get a new invoice"), on the shared payment logic; skipped when a code pays.
4. **Wallet and registration:**
   - At checkout the browser generates the phrase, derives the address and the commitment secret from it (`secret = HMAC-SHA256(BIP-39 seed, "simplex.domains commitment " + label)`), and stores them with the name locally.
   - Continue at checkout first shows the phrase (N3h), paid or by code: it must be written down, confirmed with a checkbox, before the order is created, since nothing else can recover the name.
   - It sends the address, the name's hash and length, and the commitment with the order.
   - After payment the page shows the three steps, ticked as they complete, with transaction links. After the wait the page reveals the name and secret, so it must be open then; reopening the link finishes it, and another browser asks for the phrase (N7b → N7c).
5. **Registered:** "Registered for 2 years, until 9 October 2028, to 0x5aE1…7cD3" (a code's own years when paid by code).
   - The 12-word phrase, Copy, and a QR for the app to scan.
   - "Import it in the app".
   - Warnings: the phrase owns the name, this browser keeps the only copy, write it down.

**Outcomes off the path:**
- **Taken between search and registration** (by another registrar, or by another order here that registered first): the order keeps its value.
  - Paid: the buyer can register another name at most its price, searched on the order's own page (a cheaper one refunds nothing), or take a code for the original name's length (8+ for longer names) for 2 years, issued by the service.
  - By code: the code is released, unspent, for another name.
- **Registration failed after paying:** the service's own retries gave up: "Paid, not registered", and Try again charges nothing. Refunds are manual, by support.
- **A code that does not work:** invalid or revoked, already used, reserved by another unfinished registration, expired, not yet paid, or not covering this name (the name is shorter than the code's length).
- **Payment endings:** the badge screens.

## Stages

Each stage is its own set of atomic commits; stage 0 is reviewed before any code.

### 0. Design: board, spec and mockups (review gate)

Redo `plans/names-codes/` for the store above, in simplex.domains' look: `names-flow.svg`, `screens/*.jpg`, the spec `2026-10-09-name-codes.md`, and the generator inputs in `mockups/` (`domains.js`/`domains.css` for the names screens, `screens.js` for the other frames, `fixtures.js`, `layout.mjs`, `board.mjs`). The board's sections, as `mockups/layout.mjs` lists them:

- simplex.domains: registering a name start to finish, what a search can find, and before paying (links, then the phrase);
- paying with a code: check, apply, and codes that do not work;
- when registering does not go through;
- opened from the app;
- where it meets the app (import the phrase; grey, #7475/#7530);
- refused at checkout;
- the other ways a payment ends (the badges store's endings, on the names store's pages), with the phrase pages;
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
- `plans/names-codes/mockups/board.mjs` and spec §13 follow the new web path.
- Coordinate with #7530 so the rename lands once.

### 2. Core: name codes, label rule, Registration encoding

- **`src/Simplex/Chat/Badges/Code.hs`** gains `NameCode` (`parseNameCode`, `randomNameCode`, `nameCodeHash`, `formatNameCode`, `nameCodeText`).
  - The grammar is `SN`, a length (1–2 digits), then 20 Crockford base32 body characters, with a Luhn mod-32 check over the length digits and the payload.
  - The length is shown, not trusted: the table row decides.
  - The canonical form is hashed with SHA-256.
- **`src/Simplex/Chat/Names.hs`:** `validNameLabel`.
- **The `Registration` ABI encoding and commitment**, with the resolver `setText` calls for its `data` and the EIP-712 `SetBatch` digest, one module that core and service share (#7530 D17), built once where it lands first.
- **Tests:** vectors shared with `web/test`.

### 3. Service: search, codes, orders

- **`[names]` in the ini:** `resolver_url`, and `tld` (`testing` until `.simplex` is deployed).
- **`GET /api/name/:hash?length=N`:**
  - takes keccak256(label) and the label's length (6–63), never the label, and asks the resolver by hash;
  - for a registered name, opens each `simplexContact`/`simplexChannel` link with the chat core (as `APIConnectPlan` does, which returns the link's display name and picture without connecting), with a 3 s timeout, its own rate limit and a short cache, and adds a deep link;
  - answers `available{price, resolver}` (the resolver contract the page puts in its commitment), `registered{entities}` or `reserved`. A hash whose registration here has reached `registering` answers `registered` with no entities, since the chain has not caught up.
- **`POST /api/link/preview {link}`:** opens a SimpleX contact or channel link with the chat core, as for taken names, and answers `{type, displayName, image}` or `invalid_link`; same timeout and cache, and its own rate limit.
- **`POST /api/code/check {code}`:** returns what a code covers (`minLength`, `years`) and whether it can still be used (`unused`, `reserved`, `used`, `revoked`, `expired`, `unpaid`), without spending it. It uses its own rate limit, since it confirms codes.
- **Prices:** `Catalog.namePrice :: [NamePrice] -> LabelHash -> Int -> Either CatalogRefusal CurrencyAmount` (the label's hash and length), over a seeded insert-only `name_prices` table (`Catalog.defaultNamePrices`): per-length rules, overridable per name by the label's hash. Checkout reprices and refuses a changed price with `catalog_changed`, which now carries the new price.
- **Orders:**
  - `POST /api/invoice {kind: "name", labelHash, length, price, method, owner, commitment}` creates an invoice; settlement moves the registration to `queued`.
  - `POST /api/name/register {labelHash, length, owner, commitment, code}` registers with a code. It checks that the code covers the length, marks it reserved for this registration, and spends it when the registration succeeds; there is no invoice.
  - `owner` is a checksummed address. The page builds `commitment = makeCommitment(Registration{label, owner, duration, secret, resolver, data = the links' setText calls (empty without links), reverseRecord = false, referrer = 0})` with `secret = HMAC-SHA256(BIP-39 seed, "simplex.domains commitment " + label)`; `duration` is 2 years, or the code's years, at 365 days a year.
  - Nothing is held: any number of orders may name the same hash.
  - `POST /api/registration/:id/reveal {label, secret, records}`, sent by the page once `minCommitmentAge` has passed, gives the service the name and the links chosen at checkout (`{contact: [...], channel: [...]}`, empty when none). It checks that the label is valid, hashes to `labelHash`, has `length`, is priced at most what was paid (or covered by the code), that each link is a SimpleX link of its kind, and rebuilds `data` and the commitment; otherwise `bad_request`, and the registration stays `committed`.
- **`POST /api/registration/find {labelHash, owner, timestamp, signature}`:** for a buyer who has the phrase but not the order link. The page derives the owner from the phrase and signs `simplex.domains find <labelHash> <timestamp>` (EIP-191) with its key; the service checks the signer is `owner` and the timestamp within 10 minutes, and returns the ids of registrations for that owner and hash, or `not_found`. Its own rate limit.
- **`POST /api/name/records {labelHash, records, nonce, deadline, signature}`:** changes the links later. The page signs the names v2 RFC's EIP-712 `SetBatch{node, dataHash, nonce, deadline}` with the name's key, hashing the `data` it built itself; the service rebuilds `data` from `records`, checks the hash and relays `multicallWithSig`, paying the gas against the name's edit credits. Until the RFC's `SimplexResolver` (A2) is deployed it answers `provider_unavailable`. Once it is, the registered search answer carries the owner's `nonce` and the name's `editCredits`.
- **A `taken` order:**
  - `POST /api/registration/:id/name {labelHash, length, commitment}` retargets it to another name, at most what was paid; it commits again and the page reveals again;
  - `POST /api/registration/:id/code` takes a code for the original name's length (8+ for longer names), for 2 years, instead. The service generates it and returns it once, and stores only its hash, as operator codes are stored.
- **`GET /api/registration/:id`**, the long-poll view a code registration uses, and the same object in `GET /api/invoice/:id` for a paid one: `{id, phase, labelHash, label (null until the reveal), owner, years, commitTx, registerTx, revealAfter, expiresAt, error}`. The id is a random 128-bit token, like order ids; whoever holds it can act on the registration.
- **Phases:** `unpaid` (invoice open) → `queued` (paid, or code reserved) → `committing` → `committed` (waiting for `minCommitmentAge` and the reveal) → `registering` → `registered`; or `taken`, `failed`. A commitment not revealed within the controller's `maxCommitmentAge` lapses to `failed`. From `taken` a paid order goes back to `queued` with its new label, or ends `code_issued`; a code order ends at `taken` with its code released. A code is reserved from `queued` until it is `registered` (spent) or `taken` (released); `failed` keeps it for Try again.
- **`POST /api/registration/:id/retry`:** from `failed`, back to `queued`, at no charge.
- **Migration `20261010_names`** (SQLite and Postgres), each table with the `sx_badge_service_` prefix:
  - **`name_codes`:** `name_code_id`, `code_hash` (unique), `min_length`, `years`, `code_payment_status`, `spent_at`, `revoked_at`, `expires_at`, `created_at`. A code is single-use; it is reserved from `queued` until it is `registered` (spent) or `taken` (released); `failed` keeps it for Try again.
  - **`name_prices`:** `price_id`, `min_length`, `label_hash` (null for a length rule), `price`, `currency`, `status`, `created_at`.
  - **`name_registrations`:** `registration_id`, `label_hash`, `length`, `label`, `secret` and `records` (null until the reveal), `owner`, `commitment`, `phase`, transaction hashes and blocks, `years`, `expires_at`, `invoice_id` (null when paid by code), `name_code_id` (null when paid), `error`, `created_at`, `updated_at`.
  - **`name_invoices`:** `invoice_id`, `registration_id`, `price_id`, `provider_ref`.
  - **The badge tables are untouched**, so there is no table rebuild.
  - **Provider-ref lookups:** the webhook and poller queries that find an invoice by `provider_ref` cover both `badge_code_invoices` and `name_invoices`. As in `Web/Server.hs` today, a row whose provider differs from the webhook's is ignored, since `provider_ref` is unique only within each table.
- **Wire errors:** new on HTTP and in `api.ts`'s `WIRE_ERROR_CODES`: `invalid_name`, `name_taken`, `invalid_link`, `no_edit_credits`, `code_not_covering`, `code_reserved`, and the chat protocol's code errors `code_invalid` (unknown or revoked), `code_used`, `code_expired` and `payment_pending` (an unpaid code). A retarget to a name dearer than the payment is `bad_request`.
- **Operator:**
  - `//issue name <length> [years <Y>] [paid|unpaid|free]`, and group `/issue name <length> [years <Y>]` / `/bulk name <length> [years <Y>] count <N>`, issue single-use codes; without years a code covers 2;
  - `//revoke` takes either kind.

### 4. Service: registration on chain

Shares #7530 §10's chain client: JSON-RPC to the resolver host's reth node, a dry run of every transaction, serial nonces, EIP-1559 fees, receipts with replacement, and 3 confirmations. `[chain]` holds the RPC URL, chain id, the controller and resolver contract addresses, the service's key and the confirmation depth.

- **Signing (the service's own transactions only):**
  - recoverable secp256k1 (`signRecoverable`) and EIP-1559 transaction signing (`signEip1559Tx`) are in simplexmq #1887, on `names` with #1843, not `master`;
  - `names` must reach the simplexmq the service builds against.
- **Worker**, one per registration, resumable after a restart:
  1. `queued` (paid, or a code reserved) → send `commit(commitment)`, the page's commitment → `committing`;
  2. → `committed` once mined → wait `minCommitmentAge`, read from the controller, and the page's reveal;
  3. → `registering`: send `register(Registration{label, owner, duration, secret, …})`;
  4. → `registered` at receipt + 3 blocks.
  
  Each phase is published to the long poll.
- **`taken`:** the register dry run reverted `NameNotAvailable`; nothing was sent, the payment or code is kept, and the outcomes above apply.
- **`failed`:** retried; still failing → "Paid, not registered".
- **Records:** the links chosen at checkout, if any, are set by the register transaction: its `data` holds `setText` calls for `simplex.contact` and `simplex.channel` on the name's node. Without them the name is registered with none.
- **Funding and cost:** the deployed `.testing` controller's `register` is payable in ETH, so the service's key holds ETH and pays the fee and gas, while the buyer paid a fixed USD price. A gas spike is the service's loss. #7530 §9 sends `registerWithCredit`, funded by a USD allowance, and the names v2 RFC proposes credited registration for `.simplex` (with `setRegistrarCredits`, where #7530 has `setRegistrarAllowance`); either replaces ETH funding when deployed.
- **Key risk:** the service's key holds only funds, never names, so a stolen key loses ETH, not names.

### 5. Webapps: a shared library, the badges webapp, the names webapp

Two webapps that share logic, not screens. The badges webapp stays in `apps/simplex-badge-service/web/`. The names webapp lives in the simplex.domains repo, in that site's look, and is a submodule of simplex-chat. The shared logic is a library in `packages/`, published to npm.

**5a. The library: `packages/simplex-chat-store`, published as `@simplex-chat/store`**, as `packages/simplex-chat-webrtc` is published as `@simplex-chat/webrtc`.
- **Moved out of the badges webapp:**
  - the API client and its types (`api.ts`);
  - order and invoice polling, and the payment state: crypto, card, part paid, expiry;
  - local storage (`store.ts`, `order.ts`);
  - code parsing and check characters (`codes.ts`), gaining name codes;
  - the QR encoder (`qr.ts`);
  - `crowdfunding.ts`, gaining the names amount, a placeholder.
- **New for names:**
  - `api.ts` gains `checkName`, `checkCode`, `previewLink`, name orders, the reveal, `find`, `records`, registration views and the new errors;
  - `catalog_changed` carries the new price.
- **Shape:**
  - ES modules compiled by `tsc` (the webapp's compiler settings), shipping `dist/*.js` and `.d.ts`;
  - no DOM, no styles, no runtime dependencies;
  - its own node:test suite: the moved modules' tests, and the code and commitment vectors.
- **Publishing:** from CI on a tag. Consumers outside this repo pin an exact version (`--save-exact`, lockfile integrity).

**5b. The badges webapp, built on the library:**
- **Dependency:** `package.json` gains `"@simplex-chat/store": "file:../../../packages/simplex-chat-store"`, and `npm run build` builds the library first. The badges webapp always uses the library from this repo, never from npm.
- **Imports:** modules import `@simplex-chat/store/<module>.js`, and `tsc` checks them against the library's types.
- **Specifiers are rewritten at build:** `build.js` copies the library's `dist/*.js` into `assets/<hash>/store/` and rewrites the bare specifiers to `./store/` in the served modules.
  - The page has no bundler.
  - Its CSP allows no inline script, so an import map would need a hash or a nonce.
  - The library's files enter the build hash and `sw.js`'s `PRECACHE`, and the precache tripwire test covers them.
- **Tests:** the moved modules' tests move with them; the webapp's remaining tests import from the library.
- **Docker:** the Dockerfile's web stage copies `packages/simplex-chat-store` before `npm ci`, since the dependency is a path.
- **Views:** `main.ts` gains the link to simplex.domains. The badges screens are otherwise unchanged.

**5c. The names webapp, in the simplex.domains repo**, replacing the countdown page at launch:
- **Build:** a TypeScript build emitting a static site (`index.html`, `assets/<hash>/`, `sw.js`), the same way the badges webapp builds.
  - It depends on `@simplex-chat/store` at a pinned version from npm, and on the vendored `@noble/secp256k1` and `@noble/hashes`.
  - It copies the library into its assets and rewrites the specifiers, as 5b does.
- **Crypto** (`wallet.ts`):
  1. 128-bit entropy from `crypto.getRandomValues`;
  2. the BIP-39 phrase with its checksum;
  3. PBKDF2-SHA512 seed (WebCrypto);
  4. BIP-32 to `m/44'/60'/0'/0/0` (HMAC-SHA512 from WebCrypto, secp256k1 from `@noble/secp256k1`);
  5. the EIP-55 address (keccak from `@noble/hashes`);
  6. the commitment secret from the seed, and signatures over the `SetBatch` digest and the find message.

  Tests pin the `abandon … about` vector against #7475's address for account 0.
- **`names.ts`** (pure): `validateLabel`, keccak of the label, the `Registration` ABI encoding with the records' `setText` data, `makeCommitment` and the `SetBatch` digest. Its vectors are shared with the core encoder (#7530 D17).
- **Views**, from `plans/names-codes/mockups/domains.js` and `domains.css`, using simplex.domains' tokens, fonts and logos, light and dark:
  - the header, the search and its results, the code popup;
  - checkout with links and "I have a code", and the phrase before paying;
  - the payment pages and their endings;
  - the registration track and the registered page;
  - restore and the outcomes;
  - history and the link editor.
- **Wiring:** routes `/`, `#/checkout`, `#/names`, `#/?name=&owner=` (opened from the app), `?order=<id>` and `?registration=<id>`.
- **Order errors on the page:** `name_taken` goes back to search showing N2a. Code errors from the service show where the code was entered, the popup (K5) or the checkout entry (K7), and a typo is caught on the page (K6). Anything else is N3d.
- **The phrase:** stored only in this browser. It is shown before paying, on the registered page and in history, and never sent anywhere; its QR is for the app to scan.
- **The Stripe publishable key:** injected into the shell at export, as for badges (stage 6).
- **Tests:** node:test, as the badges webapp has them.

**5d. simplex.domains as a submodule of simplex-chat, at `apps/simplex-domains`:**
- **CI:** simplex-chat's CI builds and tests the submodule against the library in this repo, overriding the pinned version with the `file:` path. A library change that breaks names fails here before it is published.
- **Docker:** the Dockerfile's web stage builds it next to the badges webapp, so one image carries both sites.
- **Releases:** a names release reaches production by bumping the submodule pointer. Builds check it out (`git submodule update --init`).

### 6. Deployment (`scripts/badge-service/`, renamed in stage 1)

Production is the split deployment in `scripts/badge-service/`:
- Caddy serves the folders the service exports at boot and proxies `/api` and `/webhooks`; one Caddy serves both sites.
- The service runs with `+client_postgres` and host networking, with its ini mounted read-only.

Changes:
- **Two exported sites:**
  - `[listener]` gains `names_static_dir` (the built names site in the image, `/srv/names`) and `names_export_dir`, beside `static_dir` and `webapp_export_dir`.
  - `exportWebapp` writes both, with the Stripe publishable key in each shell.
  - `compose.yml` mounts the second folder.
- **Caddy:**
  - `badges.simplex.chat` serves the badges folder; `simplex.domains` serves the names folder.
  - Both proxy `/api/*` to the service, so the names site calls the API on its own origin, with no CORS.
  - `/webhooks/*` is proxied on both hosts too. Each BTCPay store and Stripe account is set to one webhook URL, or every event arrives twice: today's accounts keep badges.simplex.chat, and an account used only for names may use simplex.domains.
  - `apps/simplex-badge-service/README.md` (renamed in stage 1) gains the host blocks, since the Caddyfile is not in this repository. The names site's CSP follows the badges one in `web/README.md`.
- **All-in-one mode** (`serve_webapp = on`) picks the site by `Host` against `[listener] names_hosts`.
- **Chain:** the resolver and reth on loopback when they share the host. The service's key is in the mounted ini, which `Dockerfile.dockerignore` already keeps out of the image.
- **Unchanged:** the database `badge_service` and the `sx_badge_service_` prefix.

### 7. Mock (`web/mock/server.py`)

- `GET /api/name/<hash>?length=<n>`: fixed available, taken (with entities and images) and reserved names by hash, and per-name prices.
- `POST /api/registration/<id>/reveal`: checks as the service does.
- `POST /api/code/check`: fixed codes covering 6+, 7+ and 8+, one used and one revoked.
- `POST /api/link/preview`: fixed address and channel profiles, and an invalid link.
- `POST /api/name/records`: checks the signature against the stored owner and answers as the relay would.
- `POST /api/registration/find`: checks the signature and returns the mock's registrations for that owner.
- Name orders by payment and by code.
- `POST /control/registration/<id>/<phase>` steps a registration, or sends it to taken or failed; `retry`, `name` and `code` follow the service.
- Both sites, the badges webapp and the names submodule, by path.

## Order of work

- Stages 1–3 and 5–7 do not depend on the chain. They are built and tested against the fake resolver, a fake JSON-RPC node and the mock; a paid order waits in `queued` until stage 4 lands.
- **Codes differ from #7530 §6/§9** (the `SN7-` format without years in the text, their own table, none sold on the web, and the service's table rather than the text deciding what a code covers, so the app cannot check one offline); agree with #7530 before stage 2.
- **Stage 5 in order:** the library (5a) and the badges webapp on it (5b) first, then a published release the names webapp (5c) pins, then the submodule (5d).
- **Open: hosts.** Stage 6 as written (the service exports both sites, `/api` on each origin) or a dedicated API host with static sites and `GET /api/config` (spec §14.12); decide before stage 5b.
- Stage 4 waits for simplexmq `names` (#1887's signing) to reach the simplexmq the service builds against.
- **The app side:** importing a name's phrase beside an existing master (#7475/#7530) is needed before a web-bought name shows in an app that already has a wallet. Until then, it can be the master of a device with no wallet (the name shows after `bind account=0`), and it works in any BIP-44 wallet at account 0.

## Out of scope

- The app side: importing a phrase beside an existing master (to be added to #7475/#7530), codes in the app (#7530), record editing in the app (later app work).
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
  - orders by payment and by code, the reveal and its checks (with and without links), link previews, the records relay against a fake resolver, and two orders for one name (the later one taken);
  - the worker against a fake JSON-RPC node (commit, wait, register, a revert to taken, a restart mid-phase);
  - badges unchanged, wire tags pinned.
- **Web:** the library's tests; the badges webapp's build (hash committed) and tests on the library from this repo; the names submodule's build and tests on the same library, including the wallet vectors.
- **Docker:** `docker compose build` with the submodule checked out, and a run against local Postgres that exports both sites.
- **End to end on the mock with Playwright:**
  1. search (Enter, button) → available → the phrase, confirmed → Bitcoin → steps → registered
  2. taken, reserved, invalid
  3. "Check your code" → $0 → register with code → registered for the code's years
  4. "I have a code" at checkout, and codes that do not work
  5. links at checkout → the name points at them; Edit links
  6. storage cleared after paying → the order link asks for the phrase and finishes; Restore with a phrase from Your names
  7. taken during registration → another name, and → a code
  8. failed → Try again
  9. card
  10. opened from the app
  11. history with "Check a code"
  12. both stores' cross-links
  
  Light, dark and phone, against the stage-0 board.
