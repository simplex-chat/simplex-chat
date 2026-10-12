# SimpleX names store: simplex.domains

The names store sells SimpleX names (`<label>.simplex`) at **simplex.domains**:
1. The buyer searches a name and sees whether it is free, and at what price.
2. The buyer pays by card, Bitcoin or Monero, or redeems a code.
3. The browser makes the name's wallet, a 12-word recovery phrase, and the service registers the name to that wallet's address.
4. Nothing is held in custody. Imported as the master of an app with no wallet yet, the phrase gives the app the name's key; the app shows the name after `bind account=0` or the chain scan (out of #7475's scope). An app that already has a wallet needs account import, not built yet.

Badges keep their own store at **badges.simplex.chat**. The implementation plan is [`2026-10-09-names-web-store-plan.md`](2026-10-09-names-web-store-plan.md).

The app side:
- redeeming codes in the app is #7530's (`origin/ab/names-actions-api:plans/2026-09-28-name-registration-core-cli.md`);
- #7475's wallet (`origin/ab/wallet:docs/rfcs/2026-09-10-wallet-keys.md`) imports a name's phrase only as the device's single master; importing it beside an existing master is still to be built (#7475/#7530), since #7475 excludes keys its master does not derive;
- editing a name's records in the app is later app work, out of #7530's scope; the web store links them itself (§6).

**The board**, every screen and how each leads to the next: [`names-flow.svg`](names-flow.svg). It is generated, see [§13](#13-regenerating-the-board). View it on GitHub from the file view, not "raw": the raw host's Content-Security-Policy blocks the embedded screens.

[![names-flow.svg](names-flow.svg)](names-flow.svg)

## Contents

1. Scope
2. Two stores
3. Search
4. Price and term
5. The wallet
6. Registration
7. Codes
8. Service API
9. Storage
10. Configuration
11. The page
12. Deployment
13. Regenerating the board
14. Open questions

## 1. Scope

**In:**
- The names store: search, checkout by payment or code, the wallet in the browser, registration with its steps, and the phrase.
- Linking a contact address and channel to the name, at registration and later.
- The flow opened from the app.
- History with "Check a code".
- The StoreService side: search, codes, orders, the registration worker, operator issuing.
- The names webapp (in the simplex.domains repo, a submodule here) and the library it shares with the badges webapp.
- Deployment.
- The mock.

**Out:**
- The app side (importing a phrase beside an existing master, records in the app, codes in the app). The board draws two app screens in grey (A1, A2).
- Selling codes on the web.
- Renewals, transfers, in-app purchase.
- Refunds (manual, by support).

## 2. Two stores

| | Names | Badges |
|---|---|---|
| Host | simplex.domains | badges.simplex.chat (simplex.chat/badges redirects there) |
| First screen | Search (N1) | The badge landing (B0, unchanged but for the link) |
| Cross-link | "SimpleX badges" in the header | "SimpleX names are at simplex.domains" under the landing |
| History | "Your names" (N12) | "Your codes", as today |

Two webapps and one service. The names webapp lives in the simplex.domains repo, in that site's look (§11), and is a submodule of simplex-chat at `apps/simplex-domains`; the badges webapp stays here. They share logic, not screens: the API client, payments, storage, codes and the QR encoder, in the library `packages/simplex-chat-store`, published as `@simplex-chat/store` (as `@simplex-chat/webrtc` is). The badges webapp builds on the library from this repo; the names webapp pins a published version, and simplex-chat's CI builds it against the library here. StoreService, the renamed badge service, exports both sites. Each store keeps its own `localStorage`, since the two hosts are different origins.

## 3. Search

**The search bar (N1):** a search icon, the name, a fixed `.simplex` tag and a Search button; Enter searches too. The name is judged when searching, not while typing, and the rules line appears only after a mistake (N1b, N1c). The rule is shared by `Simplex.Chat.Names.validNameLabel` and the names app's `names.ts`:
- `a-z`, `0-9` and `-`, no leading or trailing hyphen, and no `--` at positions 3–4 (#7530's `validLabel`);
- at least 6 characters (the controller's `minCharLength`);
- at most 63 (simplexmq's label limit).

simplexmq lower-cases a name when parsing it (`SimplexDomain`'s `strP`, `labelHash`) and #7530's `validLabel` admits only a–z, so the page folds upper case as the buyer types.

**Searching:** the page validates the name, hashes it with keccak256 and sends `GET /api/name/:hash?length=7`; the length sets the price. The service asks `GET {resolver_url}/v2/resolve/[<hash hex>].<tld>`. Neither request carries the name; for a registered name the resolver still knows which name it was. The service learns a name only when a paid or code order reveals it (§6).

| Answer | The page shows |
|---|---|
| `available` | N2: available, its price for 2 years (or $0 when an applied code covers it, K2), and Register |
| `registered`, with links | N2a: "Used by", one card per `simplexContact`/`simplexChannel` link, with picture, display name, link and Open in SimpleX (deep link) |
| `registered`, no links | N2b: taken, pointing nowhere (a name bought here reads like this until its owner sets records, and so does one whose register the service has just sent) |
| `reserved` | N2c: reserved, with "Contact the SimpleX team" |
| no answer, an error | N2d (503) |
| the rate limit | N2e (429) |

**Taken names:** the service opens each link with its own chat core. `APIConnectPlan` returns the link's display name and picture from its link data without connecting. Each fetch has a 3 s timeout, its own rate limit and a short cache.

**Below the search:** how it works, what a name does (a name pointing at an address and a channel), and the investor card: Wefunder's sentence and link with a names amount (a placeholder for now), and **Check your code** (§7).

## 4. Price and term

- **Term:** 2 years for a paid name, fixed: the registry's minimum (730 days), and nothing is charged for years whose price cannot be known yet. A code registers for its own `years`; investor codes get 7, 5 or 3 by investment date, as simplex.domains says. A year on chain is 365 days (31,536,000 seconds, as simplexmq's `protocol/simplex-messaging.md` prices it), so the duration is `years × 365` days from registration: 9 October 2028 for a name registered on 10 October 2026.
- **Price:** the service's answer for the searched name; no price list is shown.
  - The rule is internal: a seeded per-length table that per-name entries, keyed by the name's hash, can override, insert-only.
  - The starting values are $100 for 8+ letters, $200 for 7 and $400 for 6, for the 2 years, in `Catalog.defaultNamePrices`.
- **Checkout** carries the shown price. The service prices again and answers `catalog_changed`, with the new price, if it moved (N3b).

## 5. The wallet

At checkout (N3), the browser makes the name's wallet:

1. 128 bits from `crypto.getRandomValues`.
2. A 12-word BIP-39 phrase, with its checksum.
3. The BIP-39 seed: PBKDF2-SHA512, 2048 rounds, no passphrase (WebCrypto).
4. BIP-32 to `m/44'/60'/0'/0/0`: HMAC-SHA512 from WebCrypto, secp256k1 from vendored `@noble/secp256k1`.
5. The EIP-55 address, with keccak-256 from vendored `@noble/hashes`.
6. The commitment secret, `secret = HMAC-SHA256(BIP-39 seed, "simplex.domains commitment " + label)` (WebCrypto), so the phrase and the name are enough to reveal.

How the phrase and address are handled:
- **Before paying (N3h):** Continue at checkout, paid or by code, first shows the phrase with Copy and a checkbox, "I have written it down", which enables Continue. Only then is the order created. A crash or cleared storage after this point loses nothing that the phrase cannot rebuild.
- **The phrase** is also stored in this browser before the order is created. Checkout warns when storage is unavailable ("This browser will not keep your recovery phrase", N3f), as the badge checkout does for codes; N7a then reminds that the written copy is the only one.
- **Restoring (N7c):** an order link opened where the phrase is not stored (N7b) asks for it; "Your names" offers "Restore with a phrase", which also asks for the name and finds the order with `POST /api/registration/find`. The page derives the owner and secret, checks the owner matches the registration, and carries on: it reveals if the registration waits for that, or shows the registered name.
- **What the service gets:** the address, the name's hash and length, and the commitment with the order; the name, the commitment secret and any links only at the reveal (§6). Never the phrase.
- **The path** is #7475's account 0. Tests pin the `abandon … about` vector to #7475's address for account 0, `0x9858EfFD…aEda94`.

N7 shows the phrase again in numbered columns, with Copy and a QR for the app to scan, and says plainly that it owns the name and that nobody else has a copy. History keeps it, behind "Show phrase".

**Importing in the app** (A1, A2) is not built as drawn. #7475's device wallet is one master per device, generated with 24 words or imported from a 12–24-word phrase. On a device with no wallet, importing the name's phrase as the master gives it the name's key at account 0, and the app shows the name after an explicit `bind account=0` or the chain scan, which #7475 leaves out (its known limit 2); a device that already has a master cannot add it as an account. The phrase also works in any BIP-44 wallet at account 0.

**One wallet per order:** a taken order retargeted to another name (N8a) keeps its address, so the same phrase owns the new name.

**Opened from the app (L1–L3):** the app's link carries the name and the app's own owner address, `simplex.domains/#/?name=<label>&owner=<address>`. The page shows that address on the search and at checkout, so a crafted link cannot pass unnoticed. It makes no wallet, offers no "I have a code" (the app redeems codes itself), and ends with "Back to SimpleX".

## 6. Registration

1. **Orders:** the page sends the name's hash and length, never the name, with the owner and a commitment it builds itself: `makeCommitment(Registration{label, owner, duration, secret, resolver, data = the links' setText calls (empty without links), reverseRecord = false, referrer = 0})`, the secret derived from the phrase (§5), `duration` 2 years or the code's years at 365 days a year, and the resolver contract from the search answer.
   - Paid: `POST /api/invoice {kind: "name", labelHash, length, price, method, owner, commitment}`; settlement moves the registration to `queued`.
   - By code: `POST /api/name/register {labelHash, length, owner, commitment, code}`, with no invoice. The code is reserved for the registration and spent when it succeeds.
2. **No hold:** the service cannot hold a name it has not seen, so it holds none. Two orders may name the same hash; whichever registers first wins, and the other ends `taken` with its payment kept.
3. **The worker**, one per registration, resumed after a restart. It uses #7530 §10's chain client (JSON-RPC to the resolver host's reth node, a dry run of every transaction, serial nonces, EIP-1559 fees, receipts with replacement):

| Phase | What happens | Page |
|---|---|---|
| `queued` (paid, or code reserved) → `committing` | Send `commit(commitment)`, the page's commitment, from the service's key | N6 |
| `committed` | Mined; wait `minCommitmentAge`, read from the controller (60 s in the docs), and the page's reveal, `POST /api/registration/:id/reveal {label, secret}` | N6a |
| `registering` | Send `register(Registration{label, owner, duration, secret, …})`, paying the contract's fee; from now search answers `registered` for this hash | N6b |
| `registered` | Receipt + 3 blocks; `expires_at` recorded | N7 (or L3) |
| `taken` | The register dry run reverted `NameNotAvailable`; nothing was sent | N8 |
| `failed` | Any other failure after the service's retries; Try again, `POST /api/registration/:id/retry`, requeues it at no charge | N9 |

- **The reveal** gives the service the name and the links. It checks that the label is valid, hashes to `labelHash`, has `length`, costs at most what was paid (or is covered by the code), that each link is a SimpleX link of its kind, and rebuilds `data` and the commitment; otherwise `bad_request`, which a page that built the order itself never meets. The page must be open after the wait; reopening the link finishes it, in this browser from storage and in another from the phrase (N7c). A commitment not revealed within the controller's `maxCommitmentAge` lapses to `failed`.
- **All phases:** `unpaid` (invoice open) → `queued` (paid, or code reserved) → the rows above. From `taken` a paid order goes back to `queued` with its new label, or ends `code_issued`; a code order ends at `taken` with its code released. A code is reserved from `queued` until it is `registered` (spent) or `taken` (released); `failed` keeps it for Try again.
- **Ownership from the start:** `register()` takes the owner as a struct field, and `makeCommitment` hashes the struct rather than `msg.sender` (names v2 RFC). So the name belongs to the buyer's address from the start, and the service's key never holds a name.
- **Links at registration (N3g):** checkout offers "Link your SimpleX address", a contact address and/or a channel link. Each is previewed through `POST /api/link/preview` (display name and picture, as on the taken-name cards), so a typo shows before paying. The page puts `setText` calls for `simplex.contact` and `simplex.channel` on the name's node into the `Registration`'s `data`, so they are covered by the commitment; the links reach the service only with the reveal, which carries them as `records`, and the register transaction sets them. Without links the name is registered with none.
- **Links later (N12b):** "Your names" has Edit links. The page signs the names v2 RFC's EIP-712 `SetBatch{node, dataHash, nonce, deadline}` with the name's key over `data` it builds itself, never a hash it is handed, and `POST /api/name/records` relays `multicallWithSig`, the service paying the gas against the name's edit credits. This needs the RFC's `SimplexResolver` (A2); until it is deployed the endpoint answers `provider_unavailable`, and the phrase imported into a wallet edits the links through the dApp, paying its own gas.
- **Progress:** each phase is published to the long poll (`GET /api/invoice/:id` or `GET /api/registration/:id`).
- **Taken:** by another registrar, or by another order here that registered first. The order keeps its value:
  - paid: another name at most its price, searched on the order's own page, `POST /api/registration/:id/name {labelHash, length, commitment}` (N8a), committed and revealed again, where a cheaper one refunds nothing and a dearer one is `bad_request`; or a code for the original name's length (8+ for longer names) for 2 years, `POST /api/registration/:id/code` (N8b), which the service generates and returns once, storing only its hash;
  - by code: the code is released, unspent, for another name.
- **Failed:** "Paid, not registered". Try again charges nothing, and refunds are manual.
- **Signing the service's transactions:** `signRecoverable` and `signEip1559Tx` in simplexmq #1887, on `names` (not `master`).

## 7. Codes

| | |
|---|---|
| Shown | `SN7-BJTBP-29596-R3HJ8-25YM7`; `SN6-…`, `SN20-…` |
| Grammar | `SN`, a 1–2 digit length, then 20 Crockford base32 characters |
| Check | Luhn mod 32 over the length digits and the 19 payload characters |
| Reading | Case-insensitive, separators optional, `I`/`L` → `1`, `O` → `0` |
| Stored | SHA-256 of the canonical form, nothing else |
| Truth | The service's `name_codes` row: `min_length`, `years`, status. The length in the text is a convention |
| Issued by | The operator (`//issue name <length> [years <Y>] [paid\|unpaid\|free]`, 2 years by default; group `/issue name <length> [years <Y>]`, `/bulk name <length> [years <Y>] count <N>`), the app (an owner decision; #7530 defers in-app purchase, and the names v2 RFC has it buy a name directly), and the service for a taken order (N8b). Not sold on the web |
| Use | Single-use: reserved from `queued` until it is `registered` (spent) or `taken` (released); `failed` keeps it for Try again |

**Check your code** (K1), on the investor card and as Check a code in the header, opens a popup. `POST /api/code/check {code}` answers what the code covers (`minLength`, `years`) and whether it is unused, reserved by an unfinished registration, used (K5), revoked, expired or unpaid, without spending it. A typo fails the check character before any request (K6).

**Use this code** applies it: a covered name shows $0 and the code's term (K2, where Remove drops it), Register with code leads to a $0 checkout (K3), and the name registers for the code's years (K8). The board's example is an investor code, `SN6-` for 7 years. A name the applied code does not cover shows its normal price, with a line saying what the code covers.

The popup's answer per state, drawn for unused, used and a typo:

| State | Line |
|---|---|
| `unused` | ✓ Unused, and what it covers (K1) |
| `reserved` | This code is being used for a name now |
| `used` | This code was already used (K5) |
| `revoked`, unknown | This code is not valid |
| `expired` | This code has expired |
| `unpaid` | This code is not paid for yet |
| typo | This code is not valid, check it for a mistyped character (K6): the check character fails before any request |

**I have a code** sits under Continue in the checkout's order card and opens the entry there (K4). A code that does not cover the name says so (K7), and the buyer can still pay.

**History:** "Check a code" (N12a) asks the same question and adds the code to the list, where Use applies it.

## 8. Service API

```json
GET /api/name/<keccak256("dynamis") hex>?length=7
200 {"status": "available", "price": 20000, "currency": "usd", "years": 2, "resolver": "0x…"}

GET /api/name/<keccak256("bakery") hex>?length=6
200 {"status": "registered", "entities": [
      {"type": "address", "displayName": "Alice's Bakery", "image": "data:image/jpg;base64,…",
       "link": "https://smp8.simplex.im/a#…", "deepLink": "simplex:/a#…"},
      {"type": "channel", "displayName": "Bakery news", "image": "data:image/jpg;base64,…",
       "link": "https://smp9.simplex.im/c#…", "deepLink": "simplex:/c#…"}]}

GET /api/name/<keccak256("support") hex>?length=7   200 {"status": "reserved"}

POST /api/code/check {"code": "SN7-BJTBP-29596-R3HJ8-25YM7"}
200 {"minLength": 7, "years": 2, "state": "unused"}
    state: unused | reserved | used | revoked | expired | unpaid

POST /api/invoice        {"kind": "name", "labelHash": "0x…", "length": 7, "price": 20000, "method": "btc", "owner": "0x5aE1…7cD3", "commitment": "0x…"}
POST /api/name/register  {"labelHash": "0x…", "length": 7, "owner": "0x5aE1…7cD3", "commitment": "0x…", "code": "SN7-BJTBP-29596-R3HJ8-25YM7"}

GET  /api/invoice/:id         … plus "registration": {id, phase, labelHash, label (null until the reveal), owner, years, commitTx, registerTx, revealAfter, expiresAt, error}
GET  /api/registration/:id    the same registration object, long-polled
POST /api/registration/:id/reveal {"label": "dynamis", "secret": "0x…",
                                   "records": {"contact": ["https://smp8.simplex.im/a#…"], "channel": []}}
POST /api/registration/:id/name {"labelHash": "0x…", "length": 7, "commitment": "0x…"}
POST /api/registration/:id/code
POST /api/registration/:id/retry

POST /api/registration/find {"labelHash": "0x…", "owner": "0x5aE1…7cD3", "timestamp": 1791640800, "signature": "0x…"}
200 {"registrations": ["r4Hq9TzW2mXc7LpN5vBd8k"]}

POST /api/link/preview {"link": "https://smp8.simplex.im/a#…"}
200 {"type": "address", "displayName": "Alice's Bakery", "image": "data:image/jpg;base64,…"}

POST /api/name/records {"labelHash": "0x…", "records": {"contact": […], "channel": […]},
                        "nonce": 3, "deadline": 1791640800, "signature": "0x…"}
```

- **Errors:** new on HTTP: `invalid_name`, `name_taken`, `invalid_link`, `no_edit_credits`, `code_not_covering`, `code_reserved`, and the chat protocol's code errors `code_invalid` (unknown or revoked), `code_used`, `code_expired` and `payment_pending` (an unpaid code). `catalog_changed` carries the new price.
- **On the page:** `name_taken` from an order goes back to search (N2a); code errors from the service show where the code was entered, the popup (K5) or the checkout entry (K7), and a typo is caught on the page (K6); anything else is N3d.
- **The registration id** is a random 128-bit token, like order ids; whoever holds it can act on the registration.
- **Rate limits:** `/api/name`, `/api/link/preview`, `/api/code/check`, `/api/registration/find` and `/api/name/records` each have their own.
- **`owner`** must be a checksummed address.

## 9. Storage

Migration `20261010_names`, in SQLite and Postgres. The badge tables (`sx_badge_service_badge_codes`, `sx_badge_service_badge_code_invoices`, …) are untouched, so there is no table rebuild.

- **`sx_badge_service_name_codes`:** `name_code_id`, `code_hash` (unique), `min_length`, `years`, `code_payment_status`, `spent_at`, `revoked_at`, `expires_at`, `created_at`.
- **`sx_badge_service_name_prices`:** `price_id`, `min_length`, `label_hash` (null for a length rule), `price`, `currency`, `status`, `created_at`.
- **`sx_badge_service_name_registrations`:** `registration_id`, `label_hash`, `length`, `label`, `secret` and `records` (null until the reveal), `owner`, `commitment`, `phase`, transaction hashes and blocks, `years`, `expires_at`, `invoice_id` (null when paid by code), `name_code_id` (null when paid), `error`, `created_at`, `updated_at`.
- **`sx_badge_service_name_invoices`:** `invoice_id`, `registration_id`, `price_id`, `provider_ref`.
  - The webhook and poller lookups by `provider_ref` cover both this table and `sx_badge_service_badge_code_invoices`. As in `Web/Server.hs` today, a row whose provider differs from the webhook's is ignored, since `provider_ref` is unique only within each table.

## 10. Configuration

```ini
[listener]
; the built names site in the image, and where it is exported at boot with the publishable key
names_static_dir = /srv/names
names_export_dir = /srv/webapp-names
; hosts that get the names site when this service serves the webapps itself (serve_webapp = on)
names_hosts = simplex.domains

[names]
resolver_url = http://127.0.0.1:8000
; simplex once that namespace is deployed
tld = testing

[chain]
rpc_url = http://127.0.0.1:8545
chain_id = 1
controller = 0x…
; the resolver contract set in each Registration
resolver = 0x…
; pays gas and the registration fee; holds funds only, never names
service_key = replace-me
confirmations = 3
```

## 11. The page

**Routes on simplex.domains:**
- `/`: search;
- `#/checkout`;
- `#/names`: history;
- `#/?name=&owner=`: opened from the app;
- `?order=<id>`: payment, registration, the phrase, and another name's search after `taken` (N8a);
- `?registration=<id>`: a registration paid with a code.

**The app**, in the simplex.domains repo (`apps/simplex-domains` here): a TypeScript build emitting a static site, with `@simplex-chat/store` pinned and the vendored `@noble` libraries. Like the badges webapp, it copies the library into its hashed assets and rewrites the bare specifiers, since the page has no bundler and its CSP allows no inline import map.
- `wallet.ts`: the crypto (§5);
- `names.ts`: rules, the `Registration` encoding, the commitment and the `SetBatch` digest;
- the views, from `mockups/domains.js` and `mockups/domains.css`;
- the Stripe publishable key, injected into the shell when the service exports the site.

**The look** is simplex.domains': its tokens, GT Walsheim and Manrope, its logos and theme toggle, light and dark, laid out as a modern registrar. A frosted header holds the store's tabs (Search, Your names, SimpleX badges), Check a code and the toggle. The first page is a large headline over one search bar with a `.simplex` tag and Search, then a "how it works" grid and the investor card. Checkout and the pages after it carry a five-step tracker (search, details, recovery phrase, payment, registration) and a sticky order card. Endings are single centred cards.

| | | |
|---|---|---|
| <img src="screens/N1.jpg" width="260"> | <img src="screens/N2.jpg" width="260"> | <img src="screens/N3.jpg" width="260"> |
| **N1** search | **N2** available, with its price | **N3** checkout, I have a code |
| <img src="screens/N6a.jpg" width="260"> | <img src="screens/N7.jpg" width="260"> | <img src="screens/N2a.jpg" width="260"> |
| **N6a** the registration track | **N7** yours, with the phrase | **N2a** taken: what uses it |
| <img src="screens/K1.jpg" width="260"> | <img src="screens/K2.jpg" width="260"> | <img src="screens/K4.jpg" width="260"> |
| **K1** check your code | **K2** covered: $0 | **K4** a code at checkout |
| <img src="screens/L1.jpg" width="260"> | <img src="screens/N12.jpg" width="260"> | <img src="screens/N8.jpg" width="260"> |
| **L1** opened from the app | **N12** your names and codes | **N8** someone was faster |

## 12. Deployment

Production is the split deployment in `scripts/badge-service/` (`scripts/store-service/` after the rename):
- Caddy serves the folders the service exports at boot and proxies `/api` and `/webhooks`; one Caddy serves both sites.
- The service runs with `+client_postgres` and host networking, with its ini mounted read-only.

What changes:
- **Build:** the Dockerfile's web stage copies `packages/simplex-chat-store`, builds the badges webapp on it, and builds the names submodule.
- **Export:** both sites are exported, each with the Stripe publishable key in its shell; `compose.yml` mounts the second folder.
- **Caddy:**
  - `badges.simplex.chat` serves the badges folder and `simplex.domains` the names folder;
  - both proxy `/api/*`, so each site calls the API on its own origin;
  - `/webhooks/*` is proxied on both hosts too. Each BTCPay store and Stripe account is set to one webhook URL, or every event arrives twice: today's accounts keep badges.simplex.chat, and an account used only for names may use simplex.domains;
  - `apps/simplex-badge-service/README.md` (`apps/simplex-store-service/` after the rename) gains the host blocks, and the names site gets a CSP like the badges one.
- **All-in-one mode** picks the site by `Host` against `names_hosts`.
- **Chain:** the resolver (`:8000`) and reth (`:8545`) on loopback when they share the host.
- **Unchanged:** the database `badge_service` and the `sx_badge_service_` prefix.

## 13. Regenerating the board

The names screens are built in the browser by `mockups/domains.js` on `mockups/domains.css`, with simplex.domains' fonts and logos; they are prototypes for the names app. `mockups/screens.js` draws the other frames: the badges landing on the compiled badges webapp, and the app and operator frames. `mockups/fixtures.js` holds the data both use. `mockups/layout.mjs` holds the captions, positions and arrows. `mockups/board.mjs` reads the badges build from `apps/simplex-badge-service/web/dist/assets` (a path that follows the stage 1 rename) and simplex.domains from a checkout beside this repo or `$SIMPLEX_DOMAINS`. It renders each frame with Playwright into `screens/<tag>.jpg` (JPEG at quality 85, animations frozen) and writes `names-flow.svg`.

```
cd apps/simplex-badge-service/web && npm install && npm run build && cd ../../..
npm install --prefix /tmp/pw playwright && npx --prefix /tmp/pw playwright install chromium
PLAYWRIGHT=/tmp/pw/node_modules/playwright/index.mjs node plans/names-codes/mockups/board.mjs
```

## 14. Open questions

1. **Importing the phrase in the app:** #7475 imports a phrase only as the device's single master, so an app that already has a wallet needs an account-import path (#7475/#7530). It decides whether a web-bought name reaches such an app.
2. **Funding and cost:** the `.testing` controller's `register` is payable in ETH. The service's key pays the fee and gas while the buyer paid a fixed USD price, so a gas spike is the service's loss. #7530 §9 sends `registerWithCredit` (a USD allowance, `setRegistrarAllowance`), and the names v2 RFC proposes credits for `.simplex` (`setRegistrarCredits`); the two differ and need coordinating.
3. **Signing:** simplexmq `names` (#1887) has to reach the simplexmq the service builds against.
4. **Coordination with #7530:** the StoreService rename (#7530 proposes `SHC`/`SHP`/`SHE`; this plan uses `STC`/`STP`/`STE`), the `Registration` encoder, the chain client, codes in the app, and four code decisions that differ from #7530 §6/§9: the `SN7-` format without years in the text, a table of their own instead of a kind column, no codes sold on the web, and the service's table rather than the text deciding what a code covers, so the app cannot check one offline.
5. **The investor amount** on the investor card, $500+ as on simplex.domains today, is a placeholder.
6. **The library's releases:** the names webapp pins a published `@simplex-chat/store`; a library change that names needs is published before the submodule bumps its pin.
7. **Payment methods:** the current simplex.domains page lists In-app, Bitcoin and Monero; this design offers card, Bitcoin and Monero. In-app purchase is deferred in #7530.
8. **Fetching link profiles** puts a load on the service that grows with searches. The cache, a timeout and a rate limit of its own bound it.
9. **simplex.domains today** is a countdown page; the store replaces it at launch, in the same tokens and fonts. The mockups' headline is "Own your name on SimpleX" where today's page says "Your SimpleX domain".
10. **Editing links later** needs the names v2 RFC's `SimplexResolver` with `multicallWithSig` and edit credits (A2), which is not deployed; until then only the dApp, paid by the owner, can change them. Links set at registration add their gas to the service's cost.
11. **The link separator** inside one `simplex.contact`/`simplex.channel` record is `;`, the `LINK_SEPARATOR` that `snrc-resolve.py` splits on and that must match the dApp's `SIMPLEX_LINK_SEPARATOR`; the page and the service encode with it.
12. **Hosts and who serves the sites.** This spec has the service export both sites, injecting the Stripe publishable key into each shell, and one Caddy serving them, each proxying `/api` and `/webhooks` on its own origin. The cleaner alternative is a dedicated API host, with both sites as static deployments: the service is API-only with one webhook URL, site releases are not tied to the service image, and pages get the key from `GET /api/config`. It costs CORS for two origins and a `connect-src` entry; the submodule would then serve CI only.
