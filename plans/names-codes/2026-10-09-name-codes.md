# SimpleX names store: simplex.domains

This is the spec of the names store. The store sells SimpleX names (`<label>.simplex`) at **simplex.domains**. In short:
1. The buyer searches for a name. The page shows whether it is free, and at what price.
2. The buyer pays by card, Bitcoin or Monero, or redeems a code.
3. The browser makes the name's wallet. The page shows it as an owner code ("your name code" on the page: `SN-` and 30 characters). The service registers the name to that wallet's address, and pays the fee and the gas.
4. Nothing is held in custody. Redeeming the code in the app moves the name to the app's wallet.

Related documents:
- Badges keep their own store at **badges.simplex.chat**.
- The implementation plan is [`2026-10-09-names-web-store-plan.md`](2026-10-09-names-web-store-plan.md).
- [`2026-10-13-names-sequence.md`](2026-10-13-names-sequence.md) shows every flow as a sequence of calls, with the gaps found.

The registration primitives (the building blocks for registering on chain) are Alain's `origin/ab/names-registration-primitives-sponsored` (`plans/2026-10-12-names-sponsored-registration.md`). They are written against ens-contracts PR #35 (`SimplexController`, `SimplexSponsor`, `SimplexResolver`). This store is the plumbing and the pages around them.

The app side is separate work:
- Redeeming codes in the app. Owner codes work by transfer (`approveWithSig` and `claimWithSig`, relayed by the service). Prepaid codes are covered by #7530 (`origin/ab/names-actions-api:plans/2026-09-28-name-registration-core-cli.md`).
- Editing a name's records in the app. This is later app work, out of #7530's scope. The web store links them itself (§6).

**The board** shows every screen and how each leads to the next: [`names-flow.svg`](names-flow.svg). It is generated, see [§13](#13-regenerating-the-board). On GitHub, view it from the file view, not "raw". The raw host's Content-Security-Policy blocks the embedded screens.

[![names-flow.svg](names-flow.svg)](names-flow.svg)

## Contents

- Words used here

1. Scope
2. Two stores
3. Search
4. Price and term
5. The wallet and its owner code
6. Registration
7. Codes
8. Service API
9. Storage
10. Configuration
11. The page
12. Deployment
13. Regenerating the board
14. Open questions

## Words used here

- **Name**: `label.simplex`, a name on Ethereum, registered through the PR #35 contracts (an ENS fork).
- **Owner code**: `SN-` and 30 characters. A 128-bit secret that only the buyer keeps. It is the only key to the name.
- **Owner wallet** (owner address): the Ethereum key and address made from the owner code (BIP-39 seed, then `m/44'/60'/0'/0/0`). The name is registered to this address.
- **Commitment**: a hash of the name, the owner, a secret and the links. It hides the name while it is reserved, so nobody can see it and register it first.
- **Commit, then reveal**: the commit puts the commitment on chain. At least 60 seconds later the page reveals the name, and the register can go through. A commitment lapses after 24 hours.
- **Intent**: the owner's signature (`RegisterName`) that lets the service register the name for the owner.
- **Sponsor**: the `SimplexSponsor` contract. It pays the name's price from an allowance, so the buyer never needs ether.
- **Registrar key**: the service's own Ethereum key. It sends every transaction and pays the gas (the network fee).
- **Nonce**: a counter that stops a signature or transaction being used twice. The sponsor keeps one per owner for intents, the resolver contract one per owner for link edits, and the registrar key has its own transaction nonce.
- **Sealed**: the name and links, encrypted with a key made from the owner code. The service stores it, so the owner code alone can restore an order in another browser.
- **Resolver**: the lookup service (`snrc-resolve.py`) that answers whether a name is available, by its hash. Not to be confused with the **resolver contract** (`SimplexResolver`), which stores a name's links.
- **Links** (records): the SimpleX contact and channel links stored on a name.
- **Prepaid code**: a code such as `SN7-…` that pays for a name of a given length or longer instead of money.
- **Phase**: where a registration is: `unpaid` → `queued` → `committing` → `committed` → `registering` → `registered`, or `taken`, `failed`, `code_issued`.
- **The lag**: right after a register, the resolver may not show the name yet; the service covers it for up to an hour.
- **Long poll**: the page asks the service and the answer waits until something changes.

## 1. Scope

This section lists what the store covers and what it leaves to other work.

**In:**
- The names store: search, checkout by payment or prepaid code, the wallet in the browser, registration with its steps, and the owner code.
- Linking a contact address and channel to the name, at registration and later.
- The flow opened from the app.
- History, with "Add a code".
- The StoreService side: search, codes, orders, the registration worker, and issuing by the operator.
- The names webapp, and the library it shares with the badges webapp. The webapp lives in the simplex.domains repo, which is a submodule here.
- Deployment.
- The mock.

**Out:**
- The app side (redeeming codes, records in the app). The board draws two app screens in grey (A1, A2).
- Selling codes on the web.
- Renewals, transfers, in-app purchase.
- Refunds. Support handles them by hand.

## 2. Two stores

Names and badges are sold on two separate sites. The sites share one service and one library.

| | Names | Badges |
|---|---|---|
| Host | simplex.domains | badges.simplex.chat (simplex.chat/badges redirects there today; an iframe embed is supported and drafted on `sh/website-iframe`) |
| First screen | Search (N1) | The badge landing (B0, unchanged but for the link) |
| Cross-link | "SimpleX badges" in the header | "SimpleX names are at simplex.domains" under the landing, opening in a new tab since the badges app supports running in simplex.chat's iframe |
| History | "Your names" (N12) | "Your codes", as today |

There are two webapps and one service:
- The names webapp lives in the simplex.domains repo and uses that site's look (§11). It is a submodule of simplex-chat at `apps/simplex-domains`.
- The badges webapp stays in this repo.
- The two webapps share logic, not screens. The shared logic is the API client, payments, storage, codes and the QR encoder. It lives in the library `packages/simplex-chat-store`, published as `@simplex-chat/store` (as `@simplex-chat/webrtc` is).
- The badges webapp builds on the library from this repo. The names webapp pins a published version. simplex-chat's CI builds the names webapp against the library here.
- StoreService, the renamed badge service, exports both sites.
- Each store keeps its own `localStorage`, because the two hosts are different origins.

## 3. Search

Search tells the buyer whether a name is free and what it costs. The page sends only the name's hash, so the service does not learn the name at this point.

**The search bar (N1)** has a search icon, the name, a fixed `.simplex` tag and a Search button. Enter searches too.
- The page judges the name when the buyer searches, not while typing.
- The rules line appears only after a mistake (N1b, N1c).

**The name rules.** The characters and hyphens follow #7390's `validLabel` (`Simplex.Chat.Names.Protocol` on `origin/ab/names-mvp-edits`). That code is not yet merged where the service builds. The names app's `names.ts` mirrors it. The length is checked separately (`invalid_name`). A name must have:
- only `a-z`, `0-9` and `-`, after folding case;
- each hyphen between letters or digits, so no leading, trailing or double hyphen;
- at least 6 characters (the controller's `minCharLength`);
- at most 63 characters (simplexmq's label limit).

The character and hyphen rules match simplexmq's `nameLabelP`. That parser also takes A–Z and lower-cases them after. #7390's `validLabel` refuses `--` only at positions 3–4. It is tightened to the rule above, because such a name would never resolve.

simplexmq lower-cases a name when parsing it (`SimplexDomain`'s `strP`, `labelHash`). `validLabel` admits only a–z. So the page folds upper case as the buyer types.

**Searching** works in three steps:
1. The page validates the name and hashes it with keccak256.
2. The page sends `GET /api/name/:hash?length=7`. The length sets the price. A length outside 6–63 is `invalid_name`.
3. The service asks the resolver `GET {resolver_url}/v2/resolve/[<hash as 64 lowercase hex, no 0x>].<tld>`.

What each side learns:
- Neither request carries the name. For a registered name, the resolver still knows which name it was.
- The service learns a name only when a paid or code order reveals it (§6).

**The lag.** The resolver may report a hash as available while a registration here is:
- `registering`, or
- `registered`, with `registered_at` within the last hour.

Then search answers `registered{entities: [], tld}`, with no owner, and orders refuse the name with `name_taken`. The hour is a margin for the resolver's node lagging the chain, since the resolver itself reads `latest` with no cache. After the hour the resolver is believed, so an expired name is free again.

**Resolver failures.** Search answers `provider_unavailable` for each of these:
- the resolver's 502 `upstreamError`, for a failed status, owner or resolver read, or a lost RPC connection (for a reverted text or address read, the resolver answers empty instead);
- 400 `tldNotConfigured`;
- 502 `unknown`, without the registrar address;
- 502 `noPriceOracle`, without a readable price oracle or premium;
- a resolver that does not answer;
- a failed chain read.

The resolver's 502 `labelNotRecorded` means a label the base registrar never recorded. It cannot occur on PR #35, where every registration records its label. Search would answer it `provider_unavailable` too.

The order, `/api/name/register` and the `/name` retarget answer the same resolver failures with `name_check_unavailable` (503). They create or change nothing. They need their own error because `provider_unavailable` from invoice creation means an unavailable payment method (N3a).

| Answer | The page shows |
|---|---|
| `available` | N2: available, its price for 2 years (or $0 when an applied code covers it, K2), and Register |
| `registered`, with links | N2a: "Used by", one card per `simplexContact`/`simplexChannel` link, with picture, display name, link and Open in SimpleX (deep link) |
| `registered`, no links | N2b: taken, pointing nowhere (a name registered without links reads like this until its owner sets them, and so does one this service is registering or registered within the last hour: for that one the answer has no records, owner, resolver, `recordsNonce` or chain id, and Edit links takes the nonce, chain id and resolver from the registration view, only once it is `registered`) |
| `reserved` | N2c: reserved, with "Contact the SimpleX team" |
| no answer, an error | N2d (503) |
| the rate limit | N2e (429) |

**Taken names.** The service opens each link with its own chat core.
- For short links, `APIConnectPlan` returns the link's display name and picture from its link data, without connecting.
- A full link has no profile, so its card shows the link only.
- Each fetch has a 3 s timeout, its own rate limit and a short cache.

**Below the search** the page shows:
- how it works;
- what a name does (a name pointing at an address and a channel);
- the investor card: Wefunder's sentence and link with a names amount (a placeholder for now), and **Check your code** (§7).

## 4. Price and term

This section sets how long a name is registered and how its price is chosen.

- **Term.** A paid name is registered for 2 years, fixed.
  - This is the controller's minimum (730 days, `MIN_REGISTRATION_DURATION`).
  - Nothing is charged for years whose price cannot be known yet.
  - A code registers for its own `years`. Investor codes get 5 or 3, as simplex.domains says today.
  - A year on chain is 365 days (31,536,000 seconds, as simplexmq's `protocol/simplex-messaging.md` prices it). So the duration is `years × 365` days from registration. A name registered on 10 October 2026 runs to 9 October 2028.
- **Price.** The price is the service's answer for the searched name. The page shows no price list.
  - The rule is internal. A seeded table sets a price per length. Per-name entries, keyed by the name's hash, can override it. Prices are insert-only.
  - The starting values, for the 2 years, are $100 for 8+ letters, $200 for 7 and $400 for 6. They are in `Catalog.defaultNamePrices`.
- **Checkout** carries the shown price. The service prices the name again. If the price moved, it answers `catalog_changed` with the new price (N3b).

## 5. The wallet and its owner code

The browser makes a new wallet for each order. Its owner code is the only key to the name. This section says how the page builds the wallet, keeps the code safe, and what the service sees.

At checkout (N3), the browser makes the name's wallet:

1. It takes 128 bits from `crypto.getRandomValues`.
2. It makes the owner code: `SN-` and 30 Crockford base32 characters. They hold the entropy and the first 22 bits of its SHA-256 (WebCrypto). The code is shown in six groups of five, e.g. `SN-DWPHS-6JBFR-ZR1MA-T5HQ9-P17QM-F5GM5`.
3. It makes the BIP-39 phrase with that entropy, and its seed: PBKDF2-SHA512, 2048 rounds, no passphrase (WebCrypto).
4. It derives the key by BIP-32 to `m/44'/60'/0'/0/0`. HMAC-SHA512 comes from WebCrypto, and secp256k1 from vendored `@noble/secp256k1`.
5. It makes the EIP-55 address, with keccak-256 from vendored `@noble/hashes`.
6. It derives the commitment secret, `secret = HMAC-SHA256(seed, "simplex.domains commitment " + label)` (WebCrypto). So the code and the name are enough to reveal.
7. It derives the sealing key, `HMAC-SHA256(seed, "simplex.domains sealed")`.
   - The page encrypts the JSON `{label, contact, channel}` with AES-256-GCM. Each key holds its record value: its links joined with `;`, or "" without links.
   - The page sends the result with the order as `sealed` (unpadded base64url). So the code alone rebuilds the commitment in another browser.
   - The service refuses a `sealed` longer than 11095 characters with `bad_request`. That is the size of a 63-character label and two values at the link bound.
   - The order, register, retarget and reveal endpoints read bodies up to 16384 bytes, above today's 8192.

How the code and the address are handled:

- **One open invoice per commitment.**
  - An order whose commitment already has an open invoice gets that invoice when the order matches it. This covers a resend after a lost response.
  - Otherwise the order gets `400 {"error": "bad_request", "invoiceId": "…"}`, and the page opens that invoice.
  - The buyer uses Get a new invoice when the card form does not load or the payment details are unavailable. It cancels the old invoice first. The page orders again only after the cancel succeeds. The cancel answers lead to:
    - `funded`: the page keeps the old order.
    - `not_open`, a `200` `paid`, or a `200` `expired` with payment marks: the page re-reads the invoice. Paid, with its registration past `unpaid`: the page shows its order. Paid, with the registration still `unpaid`: the page orders again. Otherwise an invoice holding a payment is kept, and one expired with nothing received orders again.
    - a `200` still `open`: the page shows `CANCEL_STILL_OPEN` and keeps the old order.
    - `provider_unavailable`: the page keeps the old order. The service also answers this when Stripe refuses to cancel a card already processing.
  - A crypto payment the service has not yet read when the cancel goes through becomes a refund case. The poller alerts on it.
  - An order whose commitment already has a registration past `unpaid` (paid, or by code) gets that registration's order instead. For a code registration the answer has `invoiceId` null and the registration.
  - `/api/name/register` does the same. It also refuses with the same `bad_request` and `invoiceId` while the commitment has an open invoice.
  - Both checks run before the resolver. So a resend never meets its own registration as `name_taken`.
- **One order at a time (N3e).** This browser's open order may have a submitted card payment awaiting confirmation. Then checkout's Continue is off until the payment settles or expires. The badge checkout refuses a second order in the same case (it shows no Pay button). An open invoice not yet paid only shows a link to the waiting order, and Continue stays on.
- **Before paying (N3h).** Continue at checkout first shows the code, for paid and prepaid-code orders alike. It shows the code with Copy and a checkbox, "I have saved my code". The checkbox enables Continue. Only then is the order created. After this point, a crash or cleared storage loses nothing that the code cannot rebuild.
- **The code** is also stored in this browser before the order is created. When storage is unavailable, checkout warns: "This browser will not keep your name code" (N3f). The badge checkout does the same for codes. N7a then reminds the buyer that the saved copy is the only one.
- **Restoring (N7c).**
  - An order link opened where the code is not stored (N7b) asks for the code.
  - "Your names" offers "Restore with a name code". It finds the code's orders with `POST /api/registration/find`, signed by the code's key. It does not ask for the name.
  - The page then:
    1. derives the owner and the sealing key;
    2. checks that the owner matches the registration;
    3. opens `sealed` for the label and the links;
    4. derives the secret from the label, rebuilds the commitment and checks that it matches;
    5. carries on. It reveals if the registration waits for that. For a `code_issued` registration it re-derives and shows the taken code. Otherwise it shows the registered name.
- **What the service gets.**
  - With the order: the address, the name's hash and length, the commitment, and `sealed` (which it cannot open).
  - Only at the reveal (§6): the name, the commitment secret, any links, and the owner's `RegisterName` signature.
  - Never the code.
- **The path** is #7475's account 0. Tests pin the `abandon … about` vector to #7475's address for account 0, `0x9858EfFD…aEda94`. Tests also pin the branch's owner code vectors.

**After registration (N7)** the page shows the code again, with:
- Copy;
- a QR for the app to scan;
- "Show it as 12 words, for another wallet" (its BIP-39 phrase).

N7 says plainly that the code owns the name and that nobody else has a copy. History keeps the code, behind "Show code".

**Redeeming in the app** (A1, A2) is not built here. The app reads the code. The name then moves to the app's wallet on the base registrar (EIP-712 domain `SimplexNames`) in two steps:
1. The code's wallet signs `ApproveName(from, to, tokenId, ephemeralPubKey, viewTag, nonce, deadline)` with `nonce = nonces(from)`, an empty key and a zero tag. The service relays `approveWithSig`.
2. The app's wallet signs `ClaimName(from, to, tokenId, offer, nonce, deadline)` for `offer = offers(tokenId)`, with `nonce = nonces(to)`. The service relays `claimWithSig`.

After the move:
- The registry node follows the token. The records stay.
- The app finds a code's names on chain. The registrar is enumerable (`balanceOf`, `tokenOfOwnerByIndex`, `labelOf`, `nameExpires`).
- A BIP-44 wallet that imports the code's BIP-39 phrase holds the name. It can edit the name's records, paying gas.
- Moving the name still needs the recipient to claim it. PR #35's `transferFrom` reverts `RecipientMustBeCaller` unless the recipient or a registrar controller calls.

**One wallet per order.**
- A taken order retargeted to another name (N8a) keeps its address. So the same code owns the new name.
- Get a new invoice after an expired one also reuses the address. So `find` can list that expired `unpaid` registration too. The page opens the registration that is not `unpaid`, or else the newest.

**Opened from the app (L1–L3).** The app's link carries the name and the app's own owner address: `simplex.domains/#/?name=<label>&owner=<address>`.
- The page shows that address on the search and at checkout. So a crafted link cannot pass unnoticed.
- The page makes no wallet. So the order carries no `sealed`, and the page offers no links and no "I have a code". The app sets links and redeems codes itself.
- The flow ends with "Back to SimpleX".
- Open: the owner must sign the `RegisterName` intent, and here the owner is the app's wallet (§14).

## 6. Registration

This section follows a name from the order to the chain. The service commits, waits for the page to reveal the name, then registers it to the buyer's address. It also covers a name that is taken and a step that fails. The terms used are in [Words used here](#words-used-here).

1. **Orders.** The page sends the name's hash and length, never the name. With them it sends the owner and a commitment it builds itself: `makeCommitment(Registration{label, owner, duration, secret, resolver, data = one setText call per key that has links, its links joined with ";" (empty without links), reverseRecord = 0, referrer = 0})`.
   - The secret is derived from the owner code (§5).
   - `duration` is 2 years, or the prepaid code's years, at 365 days a year.
   - The resolver contract comes from the search answer.
   - This is the contract's struct, as the branch's `registrationTuple` encodes it from its `Registration` (label, TLD, owner, duration, secret, resolver, contact and channel links).

   The two kinds of order:
   - Paid: `POST /api/invoice {kind: "name", labelHash, length, price, method, owner, commitment, sealed}`. Settlement moves the registration to `queued`.
   - By prepaid code: `POST /api/name/register {labelHash, length, owner, commitment, sealed, code}`, with no invoice. A repeat answers the existing registration. The service finds it as a registration whose `name_prepaid_code_id` is this code, with the same owner and commitment, in any phase. The code is reserved for the registration and spent when the registration succeeds.

   Both kinds refuse:
   - a length outside 6–63, with `invalid_name`;
   - with `name_taken`, a name the resolver reports registered or reserved;
   - with `name_taken`, a hash the resolver reports available while a registration here is `registering`, or `registered` with `registered_at` within the last hour. This is the lag (§3): a margin for the resolver's node lagging the chain, since the resolver itself reads `latest` with no cache. After the hour the resolver is believed, so an expired name is free again.

   **Closed invoices.** An invoice that expires or is canceled leaves its registration `unpaid`.
   - A late payment of that invoice does not queue the registration. Support refunds it. This is because Get a new invoice reuses the commitment.
   - Get a new invoice makes a new order for the same name and wallet.

   **Cleanup.** 30 days after its invoice closed, an `unpaid` registration is deleted with its `name_invoices` and `invoices` rows, in one transaction.
   - A registration whose invoice received any payment, partial or late, is not deleted. It stays for support.
   - The cleanup first reads the invoice from the provider itself (`pReadInvoice`, then settlement as for any read). The poller and the provider list pass read an invoice only within about 72 hours of its creation (`settleWindow`). After that, only a webhook brings a payment.
   - Today's BTCPay handler acts only on `InvoiceProcessing`, `InvoiceSettled`, `InvoiceExpired` and `InvoiceInvalid`. It gains `InvoiceReceivedPayment` and `InvoicePaymentSettled` as read hints for a payment into a closed invoice.
   - The cleanup's read is the backstop. A payment whose webhook was lost is still recorded and alerted. If the provider does not answer, the registration is kept until the next daily run.
2. **No hold.** The service cannot hold a name it has not seen, so it holds none. Two orders may name the same hash. Whichever registers first wins. The other ends `taken`, with its payment kept.
3. **The worker.** There is one worker per registration, resumed after a restart. It uses the branch's chain client (`sendCommit`, `sendRegister`):
   - JSON-RPC to an RPC URL: the reth node on the resolver host, by #7530's decision D5;
   - a dry run of every transaction;
   - EIP-1559 fees;
   - transactions signed by the registrar key.

The worker's phases, what happens in each, and the page that shows it:

| Phase | What happens | Page |
|---|---|---|
| `queued` (paid, or prepaid code reserved) → `committing` | Send `commit(commitment)`, the page's commitment, from the registrar key | N6 |
| `committed` | Mined: a commit's outcome (its dry run, or its receipt's `status`) is checked against `commitments(commitment)`, since before the reveal there is no label: a commit that reverts, in its dry run (nothing sent) or mined with `status` 0, while its commitment is live (`UnexpiredCommitmentExists`) makes the registration `committed`, with the reveal times from `commitments()`. One that reverts with no live commitment (out of gas, or a commitment that lapsed by the read) is sent (again, if it was mined), a second later when the read falls at `commitments(commitment)` plus `maxCommitmentAge` itself, as the retry path waits at `revealBefore`; when the commitment had lapsed, `label`, `secret`, `records`, `intent_*`, `error`, the transaction hashes and blocks, `reveal_after` and `reveal_before` are also cleared before the new commit is signed, as retry does after `revealBefore`, so the page signs again. Then wait `minCommitmentAge`, read from the controller (60 s in PR #35's deployment), and the page's reveal, `POST /api/registration/:id/reveal {label, secret, records, nonce, deadline, signature}` | N6a |
| `registering` | Send `registerWithCredit(registration, nonce, deadline, signature)` to the sponsor, which spends the registrar's allowance; from now search answers `registered` for this hash | N6b |
| `registered` | A receipt with `status` 1 and `ownerOf(tokenId) == owner`, + 3 blocks; `registered_at` and `expires_at` recorded. On `status` 0 the worker reads `ownerOf(labelHash)` first (the owner holding it is `registered`), then `available(label)` to choose `taken` or `failed` | N7 (or L3) |
| `taken` | The name is no longer available: a register that reverts, in its dry run (nothing sent) or mined with `status` 0, with `NameNotAvailable` or `NameReserved` decoded, or with any other revert while `available(label)` is false, unless `ownerOf(labelHash)` is the owner, which means this registration already went through (a resumed worker or a duplicate send) and is `registered`, recording `registered_at` and `expires_at` and spending its prepaid code (the sponsor checks the intent and the allowance before availability, so a taken name can revert with those first) | N8 |
| `failed` | Any other failure after the service's retries (an `InsufficientAllowance` among them, which also alerts the operator), or a commitment not revealed in time; Try again, `POST /api/registration/:id/retry` (only from `failed`, else `bad_request`), goes in order: a lost confirmation (a mined `register_tx`, any of its hashes, with `status` 1, or `ownerOf(labelHash)` being the owner; a revert counts as not) makes it `registered`, recording `registered_at` and `expires_at` and spending its prepaid code (the operator is alerted if the sweep gave that code to another registration meanwhile); a released code now used, revoked, expired or held elsewhere answers that code error and leaves it `failed`; otherwise it requeues at no charge, re-reserving a released code, clearing `failed_at`, `error` and the register's transaction hashes and blocks, and `label`, `secret`, `records` and `intent_*` when the owner's sponsor nonce has moved, so the page reveals again. A commitment reading 0 is committed again (one still pending is waited for, and a commit reverting `UnexpiredCommitmentExists` means `committed`, with the reveal times from `commitments()`); otherwise, with `revealBefore` as `commitments(commitment)` plus `maxCommitmentAge` (`reveal_before` may be null), with at least `revealMargin` left before `revealBefore` (time for the reveal and the register) the commitment is reused, going to `committed` with the reveal times from `commitments(commitment)`, and `label`, `secret`, `records`, `intent_*` and the commit's transaction hashes and blocks are cleared when the commitment's time is not the block time of a commit of ours that mined with `status` 1, since anyone can commit a lapsed commitment again; with less, the worker waits until a second after `revealBefore` and goes on as after it; at it the worker waits a second; after it `label`, `secret`, `records`, `intent_*`, `error`, the transaction hashes and blocks, `reveal_after` and `reveal_before` are cleared and then the commitment is committed again, so the page signs again | N9 |

- **The reveal** gives the service the name, the links and the owner's signed intent.
  - It is accepted in `committed` while `label` is null.
  - Once `label` is set, an identical repeat answers 200, in that or any later phase. A different body is `bad_request`.
  - The intent is EIP-712 `RegisterName(string label,address owner,uint256 duration,address resolver,bytes[] data,uint256 nonce,uint256 deadline)` under the `SimplexSponsor` domain, signed with the owner code's key.
  - `nonce` is `SimplexSponsor.nonces(owner)`, which is 0 for a new owner code.
  - `deadline` is the view's `revealBefore`, when the commitment lapses anyway.

  The service checks that:
  - the label is valid, hashes to `labelHash` and has `length`. So its price is the registration's `price_id` row, fixed at order or retarget (or the prepaid code covers it);
  - each link is a SimpleX link of its kind and within the link bound;
  - the resolver is the default one.

  It rebuilds `data` and the commitment. Then it checks:
  - `verifyRegisterIntent`: the owner signed, the deadline has not passed, the secret is 32 bytes;
  - low s, with a check of its own. `recoverPublicKey` accepts high s, and the sponsor's `SignatureChecker` rejects it. The signature is read as exactly 65 bytes `r ‖ s ‖ v`, with `v` 27 or 28, as OpenZeppelin requires. The recovery id is `v − 27`;
  - `nonce == SimplexSponsor.nonces(owner)`, read from the chain.

  Any failed check answers `bad_request`. A page that built the order itself never meets it.
  - The intent signs the duration (contracts PR #36). So the service checks that it is what was paid: 2 years, or the prepaid code's years, at 365 days a year.
  - The page must be open after the wait. Reopening the link within 24 hours (`maxCommitmentAge` in PR #35's deploy) finishes the reveal. In this browser the page uses storage. In another browser it uses the code (N7b).
  - A commitment not revealed within the controller's `maxCommitmentAge` lapses to `failed`.
- **All phases.** `unpaid` (its invoice not settled while open) → `queued` (paid, or prepaid code reserved) → the rows above.
  - From `taken`, a paid order goes back to `queued` with its new label hash, or ends `code_issued`.
  - A prepaid-code order ends at `taken`, with its code released.
  - A prepaid code is reserved from `queued` until the registration is `registered` (code spent) or `taken` (code released).
  - `failed` keeps the code for Try again, for 30 days, then releases it.
  - A later retry reserves the code again if it is still unused. Otherwise it answers `code_used`, `code_invalid` (revoked) or `code_expired`, or `code_reserved` when another registration holds it.
  - N9 shows these errors as:
    - `code_used`: "Your code was used for another name";
    - `code_reserved`: "Your code is held by another registration; try again later";
    - `code_invalid`: "This code is not valid";
    - `code_expired`: "This code has expired".
- **Ownership from the start.** `makeCommitment` hashes the `Registration` rather than `msg.sender`. The sponsor registers the name to the struct's owner once the owner has signed the intent (PR #35). So the name belongs to the buyer's address from the start, and the registrar key never holds a name.
- **Links at registration (N3g).** Checkout offers "Point it at your SimpleX": a contact address and/or a channel link, one per key.
  - The page checks each link against the link bound: 4096 bytes of UTF-8 per record value, with a key's links joined with `;`.
  - The page previews each link (display name and picture, as on the taken-name cards) through `POST /api/link/preview`. That endpoint checks the bound too. So a typo shows before paying.
  - The page puts `setText` calls for `simplex.contact` and `simplex.channel` on the name's node into the `Registration`'s `data`. So the commitment covers them.
  - The links reach the service only with the reveal, which carries them as `records`. The register transaction sets them.
  - Without links, the name is registered with none.
- **Links later (N12b).** "Your names" lets the owner change the links after registration.
  - **When it is offered.** "Your names" has Edit links, except for a registered answer with an `owner` and null `records` (a name on another resolver or none).
  - **During the lag** the answer has neither. Edit links then takes:
    - the values from the order's links in `sealed`, or from this browser's last mined edit of that key (a mined clear sets both to "");
    - the nonce, chain id and resolver from the registration view.

    It is offered only once the phase is `registered`.
  - **Signing.** For each changed key, the page signs EIP-712 `SetText(bytes32 node,string key,string value,uint256 nonce,uint256 deadline)` under the `SimplexResolver` domain, with the name's key. `POST /api/name/records` relays `SimplexResolver.setTextWithSig`, and the service pays the gas. A `ClearRecords` signature, relayed as `clearRecordsWithSig`, clears every record at once.
  - **The nonce** is `SimplexResolver.nonces(owner)`, one counter for all of the owner's edits. The registered search answer carries it with the chain id. While search lags, the registration view carries it.
  - **Checks.** The service:
    - recovers each edit's signer. It looks repeats up by signer and nonce, and matches them by `body_hash`;
    - requires low s, as `SignatureChecker` does. The signature is read as for the reveal: 65 bytes, `v` 27 or 28;
    - requires the signer to be the name's owner;
    - requires the deadline to be ahead of the latest block.

    Otherwise it answers `bad_request`. Repeats are answered right after the signature check, before the deadline, cap, owner and nonce checks.
  - **Reverts and restarts.** Three cases are re-checked against the nonce and the record: a revert, an expiry, and a restart that finds an edit's registrar nonce used with no receipt. For a clear, the record check is both keys reading "". The re-check is needed because anyone can relay a signed edit, and a pruned node can lose a receipt.
    - An edit found applied this way answers `{tx: null}`, since the applying transaction is unknown.
    - Otherwise a revert or an expiry answers `record_reverted`.
    - Otherwise the restart case is signed again under a new registrar nonce, while its deadline is ahead.
  - **One edit at a time.** The service accepts an edit only when its nonce is the on-chain one. It answers once the edit is mined. The page signs the next edit after that answer. It uses the next nonce after a mined edit, and the same nonce after `record_reverted`. After any `bad_request`, the page re-reads the nonce from where it first read it.
  - **Limits.** PR #35 has no edit credits. So the service bounds edits with its own rate limit and a per-name cap.
  - **After redeeming**, the app's wallet owns the name and edits from the app. Your names hides Edit links once the search answer carries an owner other than the code's address.
  - **The record format.** A record holds its links joined with `;`. Removing one of several links rewrites the joined value. Clearing a key sets it to "".
- **Progress.** Each phase is published to the long poll (`GET /api/invoice/:id` or `GET /api/registration/:id`).
  - The poll holds a name invoice or a registration while its phase is the page's `seenPhase`. It holds an invoice also while its status and payment marks are unchanged.
  - An invoice paid after it closed (`paid` with phase `unpaid`) is answered at once.
  - The page stops at `registered`, `code_issued`, `taken`, `failed`, and at `unpaid` once the invoice is no longer open.
  - Today's long poll answers a paid invoice at once and tracks no phase. So a separate phase map tracks the phase. It is keyed by registration id and sits outside the poller's waiting count.
- **Taken.** A name is taken by another registrar, or by another order here that registered first. The order keeps its value.
  - **A paid order** can take another name or a prepaid code:
    - **Another name (N8a)**, at most its price, searched on the order's own page: `POST /api/registration/:id/name {labelHash, length, commitment, sealed, timestamp, signature}`.
      - The service runs the order's checks on the new hash.
      - It replaces the registration's `price_id` with the new name's price row.
      - It replaces the hash, length, commitment and sealed order.
      - It clears `label`, `secret`, `records`, `intent_*`, the transaction hashes and blocks, `reveal_after`, `reveal_before` and `error`.
      - The registration is then committed and revealed again.
      - A cheaper name refunds nothing. A dearer one is `bad_request`.
    - **A prepaid code (N8b)** for the registration's current length (8+ for longer names), for 2 years, paid and without expiry: `POST /api/registration/:id/code {codeHash, timestamp, signature}`.
    - **Signatures.** The owner signs both requests, as for `find`, because anyone holding the order link can read the registration id.
      - The signature is EIP-191, read as for the reveal (65 bytes, `v` 27 or 28).
      - It is over `simplex.domains name <registrationId> <labelHash> <length> <commitment> <sha256(sealed)> <timestamp>` or `simplex.domains code <registrationId> <codeHash> <timestamp>`.
      - `length` is in decimal.
      - `labelHash`, `commitment` and `sha256(sealed)` are lowercase `0x` hex. `sha256(sealed)` is the SHA-256 of the `sealed` string's ASCII bytes.
      - `codeHash` is the badge codes' unpadded base64url SHA-256 of the canonical code.
      - `timestamp` is in decimal Unix seconds.
    - **When allowed.** Both are only for a paid registration in `taken`, else `bad_request`. The exception: an identical `/code` repeat is answered 200 again. A page that lost a `/name` answer re-reads the view.
    - **The taken code.** The page derives the code from the owner code's seed (payload from `HMAC-SHA256(seed, "simplex.domains taken code " + registrationId)`). It sends only the code's hash. So the owner code recovers the code, and a repeat sends the same hash. A hash another code has is `code_conflict`, as for badges. For a derived code this is unreachable in practice.
    - An order opened from the app can use neither `/name` nor `/code` (gap 10 in the sequence doc).
  - **A prepaid-code order:** the code is released, unspent, for another name.
- **Failed.** The page says "Paid, not registered". Try again charges nothing. Refunds are manual.
- **Signing.** `signRecoverable` and `signEip1559Tx` are in simplexmq #1887, on `names` (not `master`). EIP-712 hashing and signer recovery are in simplexmq `ab/name-registration-primitives-sponsored`, a PR into `names`.

## 7. Codes

There are two kinds of `SN` code. The owner code is a name's key. The prepaid code pays for a name. This section defines both, then says how the page checks and applies prepaid codes.

Core holds both kinds in one sum type, so a code's meaning is in its type:

```haskell
data NameCode
  = NCOwner NameOwnerCode     -- "SN-" and 30 characters: the wallet's entropy and 22 bits of its SHA-256
  | NCPrepaid NamePrepaidCode -- "SN<len>-" and 20 characters: pays for one name of len+ letters
```

`parseNameCode` tells them apart by the body's length after `SN`, with separators removed:
- 30 characters is an owner code;
- 21 or 22 characters (the length digits and 20) is a prepaid code.

Digits after `SN` cannot decide it, since an owner code's body may start with one. Both kinds read case-insensitively, with separators optional, `I`/`L` → `1` and `O` → `0`.

| | Owner code | Prepaid code |
|---|---|---|
| Shown | `SN-DWPHS-6JBFR-ZR1MA-T5HQ9-P17QM-F5GM5` | `SN7-BJTBP-29596-R3HJ8-25YM7`; `SN6-…`, `SN20-…` |
| Body | 30 characters: 128-bit entropy and 22 bits of its SHA-256 | a 1–2 digit length, then 20 characters, the last a Luhn mod-32 check over the length digits and the 19 payload characters |
| Made by | the page, one per order | the operator (`//issue name <length> [years <Y>] [paid\|unpaid\|free]`, 2 years by default and at least 2; group `/issue name <length> [years <Y>]`, `/bulk name <length> [years <Y>] count <N>`), the app (an owner decision; #7530 defers in-app purchase), and the page for a taken order (N8b), derived from the owner code, sending only its hash. Not sold on the web |
| Means | the name's key: whoever holds it owns the name until it is redeemed | the right to register one name of that length or more, for the code's years |
| Stored by the service | never | SHA-256 of the canonical form; the `name_prepaid_codes` row (`min_length`, `years`, status) is the truth, the length in the text a convention |
| Use | redeemed in the app, moving the name (§5) | single-use: reserved from `queued` until it is `registered` (spent) or `taken` (released); `failed` keeps it for Try again, for 30 days |

The rest of this section is about prepaid codes.

**Check your code** (K1) opens a popup. It is on the investor card, and in the header as Check a code.
- `POST /api/code/check {code}` answers what the code covers (`minLength`, `years`).
- It also answers whether the code is unused, reserved by an unfinished registration, used (K5), revoked, expired or unpaid.
- It does not spend the code.
- A typo fails the check character before any request (K6).

**A name code where a prepaid code goes.** `parseNameCode` tells the kinds apart. So an owner code typed into the popup, the checkout entry or Add a code is answered on the page, sending nothing: "This is a name code. Restore it in Your names."

**Use this code** applies the code:
- A covered name shows $0 and the code's term (K2). Remove drops the code there.
- Register with code leads to a $0 checkout (K3).
- The name registers for the code's years (K8).
- A name the applied prepaid code does not cover shows its normal price, with a line saying what the code covers.

The board's example is an investor code, `SN6-` for 5 years.

The popup's answer for each state is below. The board draws unused, used and a typo.

| State | Line |
|---|---|
| `unused` | ✓ Unused, and what it covers (K1) |
| `reserved` | This code is being used for a name now |
| `used` | This code was already used (K5) |
| `revoked`, unknown | This code is not valid |
| `expired` | This code has expired |
| `unpaid` | This code is not paid for yet |
| typo | This code is not valid, check it for a mistyped character (K6): the check character fails before any request |

**I have a code** sits under Continue in the checkout's order card. It opens the code entry there (K4). A code that does not cover the name says so (K7), and the buyer can still pay.

**History.** "Add a code" (N12a) asks the same question. It adds the code to the list, where Use applies it.

## 8. Service API

This section lists the service's HTTP endpoints, with example requests and answers. The rules that apply across endpoints follow the examples.

The examples show the launch configuration, `tld = simplex`. Until `.simplex` is deployed, the service answers `testing`.

```json
GET /api/name/<keccak256("dynamis") as 0x and 64 lowercase hex>?length=7
200 {"status": "available", "price": 20000, "currency": "usd", "years": 2,
     "resolver": "0x…", "sponsor": "0x…", "chainId": 1, "tld": "simplex"}

GET /api/name/<keccak256("bakery") hex>?length=6
200 {"status": "registered", "entities": [
      {"type": "address", "displayName": "Alice's Bakery", "image": "data:image/jpeg;base64,…",
       "link": "https://smp8.simplex.im/a#…", "deepLink": "simplex:/a#…"},
      {"type": "channel", "displayName": "Bakery news", "image": "data:image/jpeg;base64,…",
       "link": "https://smp9.simplex.im/c#…", "deepLink": "simplex:/c#…"}],
     "records": {"contact": "https://smp8.simplex.im/a#…", "channel": "https://smp9.simplex.im/c#…"},   (raw record values read from the default resolver on chain, links joined with ";", null with recordsNonce for a name on another resolver or none; the resolver's split links feed only the cards; the reveal's records are arrays)
     "owner": "0x…", "resolver": "0x…", "recordsNonce": 0, "chainId": 1, "tld": "simplex"}

GET /api/name/<keccak256("support") hex>?length=7   200 {"status": "reserved"}
GET /api/name/<hash>?length=5   400 {"error": "invalid_name"}
GET /api/name/<hash of a name this service is registering>?length=7   200 {"status": "registered", "entities": [], "tld": "simplex"}

POST /api/code/check {"code": "SN7-BJTBP-29596-R3HJ8-25YM7"}
200 {"minLength": 7, "years": 2, "state": "unused"}
    state: unused | reserved | used | revoked | expired | unpaid; an unknown code is 400 {"error": "code_invalid"} (not 404, which the transport reads as not_found whatever the body)

POST /api/invoice        {"kind": "name", "labelHash": "0x…", "length": 7, "price": 20000, "method": "btc", "owner": "0x014f…9b73", "commitment": "0x…", "sealed": "…"}   (sealed: null for an order opened from the app)
200 {"invoiceId": "q7ZpL2dN9xWc4KfR8tYb1A", …the badge invoice's common fields (amount, currency, expiresAt, payment fields), flat, "kind": "name", "registration": {"id": "…", "phase": "unpaid", …}}
POST /api/name/register  {"labelHash": "0x…", "length": 7, "owner": "0x7a3C…e51D", "commitment": "0x…", "sealed": "…", "code": "SN7-BJTBP-29596-R3HJ8-25YM7"}
200 {"registrationId": "t2Jk8RmQ4vXw9NcP3yLb6g"}   (plus "invoiceId" when the commitment's registration was paid, so the page opens ?order=<invoiceId>)
POST /api/invoice for a commitment whose registration is by code: 200 {"invoiceId": null, "kind": "name", "registration": {…the view}}, no payment fields; the page opens ?registration=<id>
POST /api/invoice or /api/name/register for a commitment with a different open invoice: 400 {"error": "bad_request", "invoiceId": "q7ZpL2dN9xWc4KfR8tYb1A"}; the page opens that order

GET  /api/invoice/:id?wait=<status>&seenPhase=<phase>  … plus "registration": {id, phase, labelHash, label (null until the reveal), owner, commitment, sealed, resolver, sponsor, chainId, tld, years, commitTx, registerTx (each, among the hashes kept since they were last cleared, the one that mined with `status` 1, else the latest sent, and null when judged mined from chain state with no hash known to have mined with `status` 1), revealAfter, revealBefore, intentNonce, recordsNonce (once registered, for Edit links before search has the owner), expiresAt, error}
GET  /api/registration/:id?seenPhase=<phase>  the same registration object, long-polled: the invoice holds while its status, payment marks and (for a name invoice) phase are the ones seen, except one paid after it closed, the registration while its phase is the one seen
POST /api/registration/:id/reveal {"label": "dynamis", "secret": "0x…",
                                   "records": {"contact": ["https://smp8.simplex.im/a#…"], "channel": []},
                                   "nonce": 0, "deadline": 1791727200, "signature": "0x…"}
200 {}
POST /api/registration/:id/name {"labelHash": "0x…", "length": 7, "commitment": "0x…", "sealed": "…", "timestamp": 1791642000, "signature": "0x…"}
200 the registration object, phase queued
POST /api/registration/:id/retry
200 the registration object, phase queued, or registered after a lost confirmation
POST /api/registration/:id/code {"codeHash": "…", "timestamp": 1791642000, "signature": "0x…"}
200 {}

POST /api/registration/find {"owner": "0x014f…9b73", "timestamp": 1791641400, "signature": "0x…"}
    signature: EIP-191 over "simplex.domains find <owner as lowercase 0x hex> <timestamp in decimal Unix seconds>", 65 bytes r ‖ s ‖ v with v 27 or 28
200 {"registrations": [{"registrationId": "m8Tk3WqZ5nRc2YpJ7vLd4g", "invoiceId": "q7ZpL2dN9xWc4KfR8tYb1A", "phase": "committed", "createdAt": "2026-10-10T13:30:00Z"}]}
    newest first
    one per registration of that owner, retargeted ones included; invoiceId (the order link's id): null for a prepaid-code registration
404 {"error": "not_found"} when the owner has no registration

POST /api/link/preview {"link": "https://smp8.simplex.im/a#…"}
200 {"type": "address", "displayName": "Alice's Bakery", "image": "data:image/jpeg;base64,…"}

POST /api/name/records {"labelHash": "0x…", "key": "simplex.channel", "value": "https://smp9.simplex.im/c#…",
                        "nonce": 0, "deadline": 1791727200, "signature": "0x…"}
POST /api/name/records {"labelHash": "0x…", "clear": true, "nonce": 1, "deadline": 1791730800, "signature": "0x…"}
    key: simplex.contact | simplex.channel; deadline: signing time + 1 hour; a link not of the key's kind or a value over the link bound is invalid_link, a new edit past the per-name cap rate_limited
200 {"tx": "0x…"} once mined ({"tx": null} when it was found applied on chain with the applying transaction unknown: another relayer's, or the service's own with its receipt pruned), or 409 {"error": "record_reverted", "revert": "SignatureExpired"} and the like (null for a mined revert other than an expired deadline, whose receipt carries no reason, or a dry-run revert whose node returns no data); an identical repeat (the same body on any row) waits for the same transaction while pending, answers its tx once mined (null again for an edit found applied that way), or record_reverted again for a reverted edit; only a body on no row is new, and with a nonce whose edit is pending or mined it is bad_request; after record_reverted the nonce is free again for a new body; the nonce must equal SimplexResolver.nonces(owner) on chain, else bad_request, so edits go one at a time
```

- **Errors.** These are new on HTTP:
  - `invalid_name`;
  - `name_taken`;
  - `name_check_unavailable`: the resolver failing during an order, register or retarget;
  - `invalid_link`;
  - `record_reverted`;
  - `code_not_covering`;
  - `code_reserved`;
  - the chat protocol's code errors: `code_invalid` (unknown or revoked), `code_used`, `code_expired` and `payment_pending` (an unpaid code).

  `catalog_changed` carries the new price.
- **On the page.**
  - `name_taken` from an order goes back to search and shows its new answer (N2a or N2b, or N2c when reserved).
  - Code errors from the service show where the code was entered: the popup (K5) or the checkout entry (K7). A typo is caught on the page (K6).
  - A `bad_request` carrying an `invoiceId` opens that order.
  - `name_check_unavailable` and anything else at checkout is N3d.
  - After the order exists, the order page shows the error with the reference, and the payment is kept.
- **The registration id** is a random 128-bit token, like order ids. Whoever holds it can act on the registration.
- **Chain reads.** A failed chain read answers `provider_unavailable` (503) and changes nothing. This applies in:
  - search;
  - the reveal (the failure is not counted as one of its `bad_request` answers);
  - the records relay (the failure is not counted toward the cap);
  - the registration view, whose `intentNonce` is read only in `committed` and `recordsNonce` only in `registered`, null otherwise;
  - `/retry`.

  What counts as a failed read:
  - A revert that is the answer, as `ownerOf` for a name not held, is not a failure.
  - A null receipt counts as not mined (pending or pruned). Once its registrar nonce is used, the chain state decides: `commitments` for a commit, `ownerOf` for a register, the nonce and the record for an edit.
  - An RPC error is a failed read.

  Retries:
  - The sender re-reads an edit after a dry-run or mined revert, an expiry or a used registrar nonce. Every such re-read is retried until it succeeds, with the edit kept `pending`.
  - The worker's reads are retried, with the registration kept in its phase.
- **Rate limits.**
  - `/api/name`, `/api/link/preview`, `/api/code/check`, `/api/registration/find` and `/api/name/records` each have their own limit. It is taken after route and method matching and before any other check, as `limited` does today.
  - The profile fetch for a taken name's links has its own limit, applied per fetch.
  - `/api/name/register` shares the code check's limit, since it too reveals a code's state.
  - `GET /api/registration/:id` and the reveal take the read limit, like the invoice long poll. Each `bad_request` from the reveal counts against it.
  - `/name`, `/code` and `/retry` on a registration take the create limit.
- **Formats.** `owner` must be a checksummed address. `labelHash`, `commitment` and `secret`, in bodies and as `:hash`, are `0x` and 64 lowercase hex. Anything else is `bad_request`. `codeHash` stays the badge codes' unpadded base64url.

## 9. Storage

This section lists the new database tables. They sit beside the badge tables and change none of them.

Migration `20261010_names` runs in SQLite and Postgres. The badge tables (`sx_badge_service_badge_codes`, `sx_badge_service_badge_code_invoices`, …) are untouched, so there is no table rebuild.

- **`sx_badge_service_name_prepaid_codes`:**
  - Columns: `name_prepaid_code_id`, `code_hash` (unique), `min_length`, `years`, `code_payment_status`, `reserved_by`, `reserved_at`, `spent_at`, `revoked_at`, `expires_at`, `created_at`.
  - `reserved_by` is the registration holding the code, unique when set. `reserved_at` says when, for the operator.
  - A daily sweep releases codes whose registration is still `failed`, with a `failed_at` 30 days old.
  - Each failure sets `failed_at`, and a requeue clears it.
- **`sx_badge_service_name_prices`:** `price_id`, `min_length`, `label_hash` (null for a length rule), `price`, `currency`, `status`, `created_at`.
- **`sx_badge_service_name_registrations`:**
  - Columns: `registration_id`, `label_hash`, `length`, `label`, `secret`, `records`, `intent_nonce`, `intent_deadline`, `intent_signature`, `sealed`, `owner`, `commitment`, `phase`, transaction hashes and blocks, `reveal_after`, `reveal_before`, `years`, `expires_at`, `price_id`, `name_prepaid_code_id`, `issued_code_id`, `registered_at`, `failed_at`, `error`, `created_at`, `updated_at`.
  - `intent_nonce`, `intent_deadline` and `intent_signature` are null until the reveal.
  - Transaction hashes: every one sent, fee-bumped replacements included. Each is saved with its raw signed transaction once signed and before it is sent.
  - `reveal_after` and `reveal_before`: the commit's block time plus the controller's ages, read at the commit.
  - `price_id`: the price paid for, replaced at retarget. Null when paid by code.
  - `name_prepaid_code_id`: the code paying. Null when paid.
  - `issued_code_id`: the code taken instead of a taken name.
  - The invoice links through `name_invoices.registration_id`.
  - Indexes: by `label_hash` with `phase` (search and orders), and by `owner` (find).
  - **Restarts.** A restart rebroadcasts each saved raw transaction that has no receipt. It rebroadcasts it again after a timeout, while the node does not know it and its registrar nonce is unused.
  - Once that nonce is used, the worker reads the effect on chain before signing again. This is because a pruned node also shows a mined transaction as unknown.
    - A commit counts as mined when `commitments(commitment)` plus `maxCommitmentAge` is still ahead. The registration then becomes `committed`, with the reveal times from `commitments()`. The worker sends a commit only when the commitment reads 0 or has lapsed, and a lapsed one keeps its old time.
    - A register counts as mined when `ownerOf` is the owner.
- **`sx_badge_service_name_record_edits`:**
  - Columns: `label_hash`, `owner`, `nonce`, `key`, `value`, `deadline`, `signature`, `body_hash`, transaction hashes, `state`, `created_at`.
  - `owner` is the recovered signer. `value` is null for the clear form.
  - Transaction hashes: every one sent, each saved with its raw signed transaction once signed and before it is sent.
  - `state`: `pending`, `mined`, `reverted` and the revert.
  - Unique on owner and nonce among pending and mined rows.
  - A row found applied on chain with no receipt for any of its hashes is `mined`, with no hash known to have mined.
  - The service answers identical repeats from the table after a restart. The per-name cap counts its rows.
- **`sx_badge_service_name_invoices`:** `invoice_id`, `registration_id`, `price_id`, `provider_ref`.
  - Today's invoice queries are badge-only. So each of these gains this table: `GET /api/invoice/:id`, cancel, settlement, the poller's unpaid and overdue scans and its expiry update, and the lookups by `provider_ref`.
  - Settling a name invoice queues its registration, instead of requiring a code hash. This happens in the transaction that marks the invoice paid; the invoice's and the registration's changes are published to the long poll only after that commit. It applies only to an invoice still open.
  - An invoice settled after it expired or was canceled leaves its registration `unpaid` and alerts the operator to refund. Its order page and history row say "Paid after the invoice closed; support refunds it", with the reference.
  - The lookups by `provider_ref` cover both this table and `sx_badge_service_badge_code_invoices`.
  - As in `Web/Server.hs` today, a row whose provider differs from the webhook's is ignored. This guards against a collision between providers.
  - A ref must also be unique across the two tables. The lookup checks both and refuses a ref found in both.

## 10. Configuration

The service reads these new settings from its ini file. The comments say what each one is for.

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
; SimplexSponsor: the EIP-712 domain of RegisterName, and registerWithCredit
sponsor = 0x…
; BaseRegistrarImplementation: approveWithSig and claimWithSig when redeeming (the controller does not expose it), and the registry, as its ens()
base_registrar = 0x…
; the default resolver, the one each Registration may name
resolver = 0x…
; the registrar key: pays gas only, the sponsor's allowance pays the fee
service_key = replace-me
confirmations = 3
```

## 11. The page

This section describes the names webapp: its routes, its code, its wording and its look.

**Routes on simplex.domains:**
- `/`: search;
- `#/checkout`;
- `#/names`: history;
- `#/?name=&owner=`: opened from the app;
- `?order=<id>`: payment, registration, the owner code, and another name's search after `taken` (N8a);
- `?registration=<id>`: a registration paid with a code.

**The app** lives in the simplex.domains repo (`apps/simplex-domains` here).
- It is a TypeScript build that emits a static site.
- It pins `@simplex-chat/store` and uses the vendored `@noble` libraries.
- Like the badges webapp after stage 5b of the plan, it copies the library into its hashed assets and rewrites the bare specifiers. It must, because the page has no bundler and its CSP allows no inline import map.

Its parts:
- `wallet.ts`: the crypto (§5);
- `names.ts`: rules, the `Registration` encoding, the commitment, and the `RegisterName`, `SetText` and `ClearRecords` digests;
- the views, from `mockups/domains.js` and `mockups/domains.css`;
- the Stripe publishable key, injected into the shell when the service exports the site.

**Naming.** The UI calls the owner code "your name code". It calls a prepaid code just "code" ("Check your code", "I have a code"). Prose and code say owner code and prepaid code.

**The look.**
- It takes simplex.domains' fonts (GT Walsheim and Manrope), logos, brand blue, and a light/dark switch as simplex.domains has.
- It adds the store's own icons and a palette of its own in the same spirit, light and dark, laid out as a modern registrar.
- A frosted header holds the store's tabs (Search, Your names, SimpleX badges), Check a code and the toggle.
- The first page is a large headline over one search bar with a `.simplex` tag and Search. Below it are a "how it works" grid and the investor card.
- Checkout and the pages after it carry a five-step tracker and a sticky order card. The steps are search, details, name code, payment, registration. Opened from the app, the tracker has no name-code step. A prepaid code has no payment step.
- Endings are single centred cards.

| | | |
|---|---|---|
| <img src="screens/N1.jpg" width="260"> | <img src="screens/N2.jpg" width="260"> | <img src="screens/N3.jpg" width="260"> |
| **N1** search | **N2** available, with its price | **N3** checkout, I have a code |
| <img src="screens/N6a.jpg" width="260"> | <img src="screens/N7.jpg" width="260"> | <img src="screens/N2a.jpg" width="260"> |
| **N6a** the registration track | **N7** yours, with the owner code | **N2a** taken: what uses it |
| <img src="screens/K1.jpg" width="260"> | <img src="screens/K2.jpg" width="260"> | <img src="screens/K4.jpg" width="260"> |
| **K1** check your code | **K2** covered: $0 | **K4** a code at checkout |
| <img src="screens/L1.jpg" width="260"> | <img src="screens/N12.jpg" width="260"> | <img src="screens/N8.jpg" width="260"> |
| **L1** opened from the app | **N12** your names and codes | **N8** someone was faster |

## 12. Deployment

This section says how production changes so that one service serves both sites.

Production is the split deployment in `scripts/badge-service/` (`scripts/store-service/` after the rename). Today:
- Caddy serves the one folder the service exports at boot (`compose.yml` mounts `./web` as `/srv/webapp`, the ini's `webapp_export_dir`). It proxies `/api` and `/webhooks` on badges.simplex.chat.
- The service runs with `+client_postgres` and host networking, with its ini mounted read-only.

What changes:
- **Build:** `Dockerfile.dockerignore` lets `packages/simplex-chat-store` and `apps/simplex-domains` into the build context. The web stage copies them in the repo's layout, builds the badges webapp on the library, and builds the names submodule.
- **Export:** both sites are exported, each with the Stripe publishable key in its shell. `compose.yml` mounts the second folder.
- **Caddy:** one Caddy serves both sites.
  - `badges.simplex.chat` serves the badges folder, and `simplex.domains` the names folder.
  - Both proxy `/api/*`, so each site calls the API on its own origin.
  - `/webhooks/*` (`/webhooks/btcpay`, `/webhooks/stripe`) stays on badges.simplex.chat only, as today. BTCPay and Stripe send every event there, names payments included.
  - `apps/simplex-badge-service/README.md` (`apps/simplex-store-service/` after the rename) gains the host blocks.
  - The names site gets a CSP like the badges one, with `data:` added to its `img-src` (`'self' data: https://*.stripe.com`) for the link profiles' pictures.
  - No inline styles are allowed. So the views move the mockups' `style` attributes into classes or set them through `node.style`.
- **All-in-one mode** picks the site by `Host` against `names_hosts`.
- **Chain:** the resolver (`:8000`) and reth (`:8545`) run on loopback when they share the host.
- **Unchanged:** the database `badge_service` and the `sx_badge_service_` prefix.

## 13. Regenerating the board

The board is generated from the mockups. This section says which files draw it and how to rebuild it.

- The names screens are built in the browser by `mockups/domains.js` on `mockups/domains.css`, with simplex.domains' fonts and logos. They are prototypes for the names app.
- `mockups/screens.js` on `mockups/mockups.css` draws the other frames: the badges landing on the compiled badges webapp, and the app and operator frames.
- `mockups/fixtures.js` holds the data both use.
- `mockups/layout.mjs` holds the captions, positions and arrows.
- `mockups/board.mjs` reads the badges build from `apps/simplex-badge-service/web/dist/assets` (a path that follows the stage 1 rename). It reads simplex.domains from a checkout beside this repo or from `$SIMPLEX_DOMAINS`. It renders each frame with Playwright into `screens/<tag>.jpg` (JPEG at quality 85) and writes `names-flow.svg`.

To rebuild, run from the repo root:

```
cd apps/simplex-badge-service/web && npm install && npm run build && cd ../../..
npm install --prefix /tmp/pw playwright && npx --prefix /tmp/pw playwright install chromium
PLAYWRIGHT=/tmp/pw/node_modules/playwright/index.mjs node plans/names-codes/mockups/board.mjs
```

## 14. Open questions

These points are not decided yet.

1. **Opened from the app (L1–L3).** The owner must sign the `RegisterName` intent, and here the owner is the app's wallet. There are two options:
   - The app signs it. The page hands the app the registration to sign, then reveals with the app's signature.
   - The flow waits until the app buys names itself.
2. **Funding and cost.** `registerWithCredit` spends the registrar's allowance on `SimplexSponsor`, so the fee is not paid in ETH. The registrar key pays gas, while the buyer paid a fixed USD price. So a gas spike is the service's loss.
3. **Signing and the primitives.**
   - simplexmq `names` (#1887) and `ab/name-registration-primitives-sponsored` (EIP-712, signer recovery) have to reach the simplexmq the service builds against.
   - The primitives branch (`ab/names-registration-primitives-sponsored`) has to land. Its `cabal.project` still points at a local simplexmq.
4. **Coordination with #7530.**
   - The StoreService rename: #7530 proposes `SHC`/`SHP`/`SHE`, and this plan uses `STC`/`STP`/`STE`.
   - Codes in the app.
   - Four code decisions that differ from #7530 §6/§9:
     - the `SN7-` format, without years in the text;
     - a table of their own instead of a kind column;
     - no codes sold on the web;
     - the service's table, not the text, decides what a code covers. So the app cannot check one offline.
5. **Coordination with Alain's branch.**
   - Its `NameCode` becomes `NameOwnerCode` under the `NameCode` sum type (§7).
   - Its `RegisterName` intent carries PR #35's nonce since `965046fe1`, and the duration since `f85d53e91` (simplex-chat PR #7693, for ens-contracts PR #36). The service checks the nonce against `SimplexSponsor.nonces(owner)`. simplexmq's `EIP712` test vector (PR #1941) is a generic one, still the old type under a `SimpleX`/`2` domain; the live intent is checked against viem in `NameRegistrationTests`.
   - The StoreService rename moves its `BadgeService/Names/` modules.
6. **The investor amount** on the investor card, $500+ as on simplex.domains today, is a placeholder.
7. **The library's releases.** The names webapp pins a published `@simplex-chat/store`. A library change that names needs is published before the submodule bumps its pin.
8. **Payment methods.** The current simplex.domains page lists In-app, Bitcoin and Monero. This design offers card, Bitcoin and Monero. In-app purchase is deferred in #7530.
9. **Fetching link profiles** puts a load on the service that grows with searches. The cache, a timeout and a rate limit of its own bound it.
10. **simplex.domains today** is a countdown page. The store replaces it at launch, in the same fonts and brand colour. The mockups' headline is "Own your name on SimpleX", where today's page says "Your SimpleX domain".
11. **Editing links later** costs the service a relayed transaction per changed key. No on-chain credits meter it. PR #35 has `setTextWithSig` and `clearRecordsWithSig`, not the edit credits of the names v2 RFC (`origin/ab/names-gui:docs/rfcs/2026-08-05-simplex-names-v2.md`). The service's own cap bounds it.
12. **The link separator** inside one `simplex.contact`/`simplex.channel` record is `;`. It is the `LINK_SEPARATOR` that `snrc-resolve.py` splits on, and it must match the dApp's `SIMPLEX_LINK_SEPARATOR`. The page and the service encode with it.
