# Name codes: web checkout

The badge checkout also sells **name codes**: a code, bought by card, Bitcoin or Monero, that registers a SimpleX name (`<label>.simplex`) in the app. This document specifies the page, the service API, the code, the storage and the operator commands. The implementation plan is [`2026-10-09-names-web-store-plan.md`](2026-10-09-names-web-store-plan.md). Redemption on chain (commit and reveal from the app's wallet key) is #7530's: `origin/ab/names-actions-api:plans/2026-09-28-name-registration-core-cli.md`.

**The board**, every screen and how each leads to the next: [`names-flow.svg`](names-flow.svg). It is generated, see [§10](#10-regenerating-the-board).

[![names-flow.svg](names-flow.svg)](names-flow.svg)

## Contents

1. Scope
2. The name code
3. Names, tiers and prices
4. Purchase sequence
5. Service API
6. Storage
7. Configuration
8. Operator commands
9. The page
10. Regenerating the board
11. Open questions

## 1. Scope

**In:**
- The webapp's name flow: name, term, checkout, payment, code.
- `GET /api/name/:label` through the resolver.
- Name invoices and the name kind in the code table.
- `//issue name` in `--run-cli` and `/issue name` / `/bulk name` in the managed group.
- The ShopService rename (plan stage 1).
- The Python mock's name endpoints.

**Out:**
- Redemption, the chain client and the app screens (#7530). The board draws two of them in grey as A1 and A1a, after canvas 5g.
- In-app purchase.
- Renewals.
- The app's handling of the open-in-app link.

## 2. The name code

A code is an entitlement: **names of N or more letters, for Y years**. It is not bound to the name the buyer checked, because registering needs the owner's wallet key, which only the app has. The page says so on checkout (N3) and on the code (N4).

| | |
|---|---|
| Shown | `SN7-2Y-9EEHX-PTHPY-NCH03-R54E6` |
| Grammar | `SN`, one digit for the minimum length (6, 7 or 8), one or two digits for the years (2–10), `Y`, then 20 body characters |
| Body | 19 payload characters from the CSPRNG and a check character, all Crockford base32 (`0-9A-Z` without `I L O U`) |
| Check | Luhn mod 32 over the values of the parameter characters (`7`, `2`, `Y`) and the 19 payload characters. Changing `SN7` to `SN6` fails it |
| Reading | Case-insensitive, separators optional, `I`/`L` read as `1` and `O` as `0`, as `Badges/Code.hs` reads badge codes |
| Canonical | `SN72Y` followed by the 20 body characters, with no separators |
| Stored | SHA-256 of the canonical form's ASCII bytes, nothing else of the code |
| Authority | The service row's `min_length` and `years`, not the text. A code bought at the 8+ price whose text says `SN6` covers 8+ letters |

`SN` is the kind, as `SB` is for badge codes; the different prefix keeps the two kinds' hashes apart. `Simplex.Chat.Badges.Code` gains `NameCode` (`parseNameCode`, `randomNameCode`, `nameCodeParams`, `nameCodeHash`, `formatNameCode`, `nameCodeText`). `web/src/codes.ts` mirrors it, and both test suites share fixed vectors.

## 3. Names, tiers and prices

- **Label rule**, shared by `Simplex.Chat.Names.validNameLabel` and `web/src/names.ts`:
  - case folded to lower;
  - `a-z`, `0-9` and `-` only;
  - no leading or trailing hyphen, and no `--` at positions 3–4;
  - 6 to 63 characters.
- **Tier:** the minimum length a code covers. Labels of 6 and 7 letters are their own tiers; 8 or more letters are tier 8.
- **Prices:** per year, in US cents, linear in years, no discount.

| Tier | Price id | Per year | 2 years | 10 years |
|---|---|---|---|---|
| 8+ letters | `name_8` | $50 | $100 | $500 |
| 7 letters | `name_7` | $100 | $200 | $1,000 |
| 6 letters | `name_6` | $200 | $400 | $2,000 |

- **Term:** 2 to 10 whole years. Two is the floor because the contract registers for no less than 730 days. The term starts at registration in the app, not at purchase.
- **Price ids are insert-only**, as badge prices are. Repricing is a new id; the old one is disabled, and a buyer holding it gets `catalog_changed` (N3b).

## 4. Purchase sequence

1. N1: the buyer types a label. The page judges the length and the characters locally (N1c, N1d) and marks the tier.
2. Check availability: `GET /api/name/<label>`. The service validates the label and hashes it, keccak256 over the ASCII bytes. It then asks `GET {resolver_url}/v2/resolve/[<hash hex>].<tld>` and maps the answer:

   | Resolver `registration.type` | Page |
   |---|---|
   | `available` | N1b |
   | `registered` | N1e |
   | `reserved` | N1f |
   | resolver failure | N1g (503) |
   | rate limit | N1h (429) |

   The service never logs or stores the label. The resolver sees only the hash.
3. N2: the buyer chooses the years. N3: the buyer chooses the method.
4. The page makes a name code for the tier and years, hashes it, and sends `POST /api/invoice` with `kind: "name"`, the price id, the years, the method and the code hash. The label is not sent.
5. The service prices the order from `name_prices`, creates the invoice at BTCPay or Stripe, and writes the invoice and an unpaid name code row in one transaction.
6. From here everything is the badge lane, unchanged:
   - the payment screens (N5–N8c), the poller, the webhooks and cancel;
   - settlement marks the code row paid and sets its expiry.
7. N4 shows the code from this browser's store, with Copy, a QR and Open in SimpleX. The link hands the code and the label to the app (A1), where #7530 registers the name. If someone took the name in between, the code is not spent and the app asks for another name it covers (A1a).

## 5. Service API

`GET /api/name/:label`, read rate limit (60 a minute per client):

```json
200 {"status": "available", "label": "dynamis", "minLength": 7, "yearPrice": 10000, "currency": "usd"}
200 {"status": "registered", "label": "privacy", "minLength": 7, "yearPrice": 10000, "currency": "usd"}
200 {"status": "reserved", "label": "support", "minLength": 7, "yearPrice": 10000, "currency": "usd"}
400 {"error": "invalid_name"}
503 {"error": "provider_unavailable"}
429 {"error": "rate_limited"}
```

The service answers `provider_unavailable` without a `[names]` section, on a resolver timeout (3 s), and on any resolver error. `label` is the folded form the page shows back.

`POST /api/invoice`, create rate limit. A body without `kind` is a badge, as today.

```json
{"kind": "name", "priceId": "name_7", "years": 2, "method": "btc", "codeHash": "<43 base64url chars>"}
```

The answer has the badge answer's shape, with `kind`, `minLength` and `years` in place of `badgeType` and `months`:

```json
{"invoiceId": "...", "kind": "name", "minLength": 7, "years": 2, "amount": 20000, "currency": "usd",
 "expiresAt": "...", "address": "...", "cryptoAmount": "...", "cryptoCurrency": "btc"}
```

| Refusal | When |
|---|---|
| `bad_request` | years outside 2–10, or a malformed body |
| `catalog_changed` | unknown or disabled price id |
| `code_conflict` | the hash is already sold; the page makes another code |
| `provider_unavailable` | no provider for the method, or the provider refused |
| `rate_limited` | the create limit |

`GET /api/invoice/:id` and `POST /api/invoice/:id/cancel` are unchanged, except that a name invoice's view carries `kind`, `minLength` and `years`. `invalid_name` joins `WIRE_ERROR_CODES` and the README's route table.

## 6. Storage

Migration `20261009_name_codes`, in SQLite and Postgres, with a down migration.

- **`sx_badge_service_badge_codes`:**
  - adds `code_kind TEXT NOT NULL DEFAULT 'badge'`, `min_length INTEGER` and `years INTEGER`;
  - `badge_type` and `months` become nullable;
  - a CHECK requires exactly the badge pair for `badge` and exactly the name pair for `name`.
- **`sx_badge_service_badge_code_invoices`:**
  - `price_id` becomes nullable;
  - adds `name_price_id TEXT REFERENCES sx_badge_service_name_prices`;
  - a CHECK requires exactly one of the two price ids.
- **New `sx_badge_service_name_prices`:** `price_id` (primary key), `min_length`, `year_price`, `currency`, `status`, `created_at`. It is seeded on every start, insert-only, from `Catalog.defaultNameCatalog`.
- **Postgres** relaxes the columns with `ALTER COLUMN … DROP NOT NULL` and adds the constraints.
- **SQLite** rebuilds both tables with `PRAGMA defer_foreign_keys = ON`, following `M20230118_recreate_smp_servers`, because purchases and invoices reference the code table. A migration test runs up and down with referencing rows present.
- **Code changes:**
  - `getBadgeCode` reads `code_kind = 'badge'` only.
  - `InvoiceRow`'s badge type and months become `CodeItem = CIBadge BadgeType months | CIName minLength years`.
  - `revokeCode` takes either kind.

## 7. Configuration

```ini
; optional: sells name codes. Without it the name check answers provider_unavailable
[names]
; the resolver of simplexmq scripts/resolver, on loopback
resolver_url = http://127.0.0.1:8000
; simplex once that namespace is deployed; testing until then
tld = testing
```

## 8. Operator commands

| Where | Command | Answers |
|---|---|---|
| `--run-cli` | `//issue name <6\|7\|8> [<years 2-10>] [paid\|unpaid\|free]` | `Code: SN7-3Y-…`, printed once |
| `--run-cli` | `//revoke <code>` | Either kind, as today |
| Managed group, moderator and above | `/issue name <6\|7\|8> [years <Y>]` | One single-use code |
| Managed group, moderator and above | `/bulk name <6\|7\|8> [years <Y>] count <B>` | B codes, one per line |

Years default to 2 and the status to `free`. The group's command menu and usage replies come from the same parameter text, as for badges (OP1, OP2).

## 9. The page

**Routes:** `#/name`, `#/name/years` and `#/name/checkout` on the existing track, beside the badge steps. The payment screens stay on `?order=`.

**Store:** an order record gains optional `kind: "name"`, `minLength`, `years` and `label`. The label is kept only in this browser, for N4 and the history. A record without `kind` is a badge.

**The button's place:** each badge step keeps the Wefunder block under its main button. That block's fixed minimum height is what keeps the button at one height on every screen. The name steps (N1, N2, N3) and the name failure screen keep an empty block of that height (`.invest`, `aria-hidden`) without the badge offer. On N3 the slot holds "You get a code for any name of 7+ letters", where the badge checkout says "Or invest $10,000+". The history list (N10) keeps the real block.

**New rules in `styles.css`:** `.name-box` (with `.tld`), `.name-status`, `.tiers`/`.tier`, `.stepper`/`.step`/`.term`, `.term-total`, `.primary.next` and `.name-mark`. All of them use the existing tokens, so the dark theme needs nothing of its own (D0–D4). They are prototyped in `mockups/mockups.css`.

### Buying, start to finish

| | | |
|---|---|---|
| <img src="screens/N0.jpg" width="260"> | <img src="screens/N1.jpg" width="260"> | <img src="screens/N1b.jpg" width="260"> |
| **N0** landing: a second way in | **N1** the name, tiers shown from the start | **N1b** available: Continue opens |
| <img src="screens/N2.jpg" width="260"> | <img src="screens/N3.jpg" width="260"> | <img src="screens/N4.jpg" width="260"> |
| **N2** the term, 2 to 10 years | **N3** checkout: what the code covers | **N4** the code, the link and two warnings |

### What the name check can answer

| | | |
|---|---|---|
| <img src="screens/N1c.jpg" width="260"> | <img src="screens/N1d.jpg" width="260"> | <img src="screens/N1e.jpg" width="260"> |
| **N1c** too short, judged locally | **N1d** not a name, judged locally | **N1e** registered |
| <img src="screens/N1f.jpg" width="260"> | <img src="screens/N1g.jpg" width="260"> | <img src="screens/N1h.jpg" width="260"> |
| **N1f** reserved | **N1g** not checked (503) | **N1h** too many checks (429) |

### The term, the code's other states, and history

| | | |
|---|---|---|
| <img src="screens/N2a.jpg" width="260"> | <img src="screens/N2b.jpg" width="260"> | <img src="screens/N4a.jpg" width="260"> |
| **N2a** ten years, the ceiling | **N2b** a 6-letter name at four times the base | **N4a** the browser refused to save it |
| <img src="screens/N4b.jpg" width="260"> | <img src="screens/N8.jpg" width="260"> | <img src="screens/N10.jpg" width="260"> |
| **N4b** paid elsewhere, no code here | **N8** card, with the name's rows | **N10** history, names beside badges |

The refusals at checkout (N3a–N3e) and the payment endings (N5–N9) are the badge screens; the board shows each one with the name's content. Four of them change for names:

- **N3b `catalogChanged`:** says "the price changed" instead of "the badge you chose".
- **N3d invoice failure:** drops the badge's invest block.
- **N8 `cardForm`:** shows the name's summary rows.
- **N4b `paidNoCode`:** titles the order by its name.

**The open-in-app link:** `simplex:/name#code=<shown code>&label=<label>`, built by one function. This format is a proposal: the app does not handle it yet, and the app team has to confirm it before release.

## 10. Regenerating the board

`mockups/screens.js` builds every screen in the browser on the webapp's compiled modules. Shared screens call the real `screens.js`, and name screens are prototypes for stage 4's `nameScreens.ts`. `mockups/layout.mjs` holds each frame's caption, position and incoming arrow. `mockups/board.mjs` renders each frame with Playwright into `screens/<tag>.jpg` and writes `names-flow.svg` around them. The screens are JPEG at quality 85, about a quarter of the PNG size.

```
cd apps/simplex-badge-service/web && npm install && npm run build && cd ../../..
npm install --prefix /tmp/pw playwright && npx --prefix /tmp/pw playwright install chromium
PLAYWRIGHT=/tmp/pw/node_modules/playwright/index.mjs node plans/names-codes/mockups/board.mjs
```

## 11. Open questions

1. **The host:** the board writes `store.simplex.chat`, after canvas 5f; the badge site is `badges.simplex.chat` today.
2. **The open-in-app link format** (§9), for the app team.
3. **`.simplex` is not deployed:** `tld = testing` until it is, so checks answer for `.testing` names meanwhile.
4. **Coordination with #7530:** it plans the same ShopService rename and code kind. Only one of the two should land each.
5. **The code's expiry:** settlement sets one year, as for badge codes. The page does not show it, since only the service knows it (#7530 D18).
