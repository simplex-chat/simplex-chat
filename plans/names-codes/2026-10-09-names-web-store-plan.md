# SimpleX names store: purchase and registration on the web

This plan builds the SimpleX names store at simplex.domains in stages 0–7, and says how each stage is verified.
The words it uses (owner code, owner wallet, commitment, intent, sponsor, registrar key, nonce, sealed, resolver, phase, the lag, long poll) are defined in the spec, [Words used here](2026-10-09-name-codes.md#words-used-here).

## Context

This section says what the store does, what it builds on, and which facts about the contracts shape the design.

SimpleX names (`<label>.simplex`, an ENS fork on Ethereum) get their own store at **simplex.domains**:
1. The buyer searches a name and sees whether it is free, and at what price.
2. The buyer pays by card, Bitcoin or Monero, or redeems a code.
3. The browser makes a wallet for the name, the owner wallet. The page shows it as an owner code ("your name code" on the page: `SN-` and 30 characters). The service registers the name to that wallet's address, paying the fee and gas.
4. The buyer keeps the code. Redeeming it in the app moves the name to the app's wallet.

Nothing is held in custody. The name belongs to the browser's wallet from its first transaction.

Badges keep their own store at **badges.simplex.chat**. To the user these are two stores, and each links the other as a secondary option. Both run on the badge service, renamed **StoreService**, and share one logic library.

What this plan builds on, and where the rest lives:
- **The registration primitives** are Alain's `origin/ab/names-registration-primitives-sponsored` (its plan `plans/2026-10-12-names-sponsored-registration.md`). They use simplexmq `ab/name-registration-primitives-sponsored` and ens-contracts PR #35 (`SimplexController`, `SimplexSponsor`, `SimplexResolver`). They provide:
  - the owner code;
  - the `Registration` encoding and the commitment;
  - the owner's EIP-712 `RegisterName` intent;
  - a chain client sending `commit` and `registerWithCredit`.
- **This plan** is the plumbing and the store around them.
- **The app side** (redeeming codes, editing records in the app) is #7530's (`origin/ab/names-actions-api:plans/2026-09-28-name-registration-core-cli.md`).
- **Every flow as a sequence of calls**, with the gaps found, is in [`2026-10-13-names-sequence.md`](2026-10-13-names-sequence.md).
- **`e2e/StoreE2E.test.ts`** runs the on-chain part against PR #35, from inside a checkout of the PR (`e2e/README.md`).

Facts that shape the design:

- **Registering for any owner:**
  - `makeCommitment` hashes the `Registration` struct, not `msg.sender`.
  - `SimplexSponsor.registerWithCredit(registration, nonce, deadline, signature)` registers to the struct's owner once the owner has signed the `RegisterName` intent. The intent carries a per-owner nonce, which the sponsor consumes in order.
  - So the service pays and sends both transactions, while the owner is the browser's address.
  - The sponsor spends the registrar's allowance. So the service holds no ETH for fees, only gas.
- **The owner code** is `SN-` and 30 Crockford base32 characters: the wallet's 128-bit entropy and the first 22 bits of its SHA-256.
  - The wallet's key is the BIP-44 key at `m/44'/60'/0'/0/0` of the 12-word BIP-39 phrase with that entropy.
  - A BIP-44 wallet importing the code's BIP-39 phrase holds the name and can edit its records, paying gas.
  - Moving the name still needs the recipient to claim it. PR #35's `transferFrom` reverts `RecipientMustBeCaller` unless the recipient or a registrar controller calls.
- **Redeeming in the app** moves the name on `BaseRegistrarImplementation`, EIP-712 domain `SimplexNames` (PR #35). No phrase import is needed. The steps:
  1. The code's wallet signs `ApproveName(from, to, tokenId, ephemeralPubKey, viewTag, nonce, deadline)` with `nonce = nonces(from)`. The key is empty and the tag zero, so there is no stealth announcement.
  2. The service relays `approveWithSig`.
  3. Once it is mined, the app's wallet signs `ClaimName(from, to, tokenId, offer, nonce, deadline)` with `offer = offers(tokenId)` read then, and `nonce = nonces(to)`. Each approval increments the offer, and `claimWithSig` reverts `NotOffered` on an older value.
  4. The service relays `claimWithSig`.
  5. The registry node follows the token. The records stay.
- **Term:**
  - A paid name is registered for 2 years, the controller's minimum: 730 days, `MIN_REGISTRATION_DURATION` in PR #35's `SimplexController`, also in `simplexmq: protocol/simplex-messaging.md`.
  - A code registers for its own term.
  - A year on chain is 365 days (31,536,000 seconds, as simplexmq's `protocol/simplex-messaging.md` prices it). So the duration is `years × 365` days.
  - The term runs from registration.
- **Availability and records** come from the resolver (`simplexmq: scripts/resolver`):
  - `GET /v2/resolve/[<keccak256(label) as 64 lowercase hex, no 0x>].<tld>` → `available | registered{nameRecord} | reserved`.
  - `nameRecord` lists `simplexContact` and `simplexChannel` links.
- **Name rules:**
  - Characters: `a-z 0-9 -` after folding case. simplexmq's `nameLabelP` (`SimplexName.hs`) also takes A–Z and lower-cases after parsing.
  - Hyphens: each hyphen has a letter or digit on both sides, as `nameLabelP` requires. So there is no leading, trailing or double hyphen anywhere.
  - #7390's `validLabel` (`origin/ab/names-mvp-edits`, `Names/Protocol.hs`, not merged into the branches this builds on) refuses `--` only at positions 3–4. It is tightened to the hyphen rule above with Alain, since a name with `--` elsewhere would sell but never resolve.
  - At least 6 characters, from the controller's `minCharLength`.
  - At most 63, from simplexmq's label limit.
  - simplexmq lower-cases a name when parsing it (`SimplexDomain`'s `strP`, `labelHash`), and `validLabel` admits only a–z. So the page folds upper case as typed.
- **Deployment:**
  - `.simplex` is not deployed. `.testing` is live on mainnet, but not with PR #35's contracts.
  - The store needs a fresh PR #35 deployment, and a deploy script for it. `e2e/deployLocal.ts` does it for a local anvil chain.
  - The PR's own scripts do not fit. They deploy a plain `PublicResolver`, no `SimplexSponsor`, and a premium price oracle.
  - The deployment holds:
    - `ENSRegistry`;
    - `BaseRegistrarImplementation` owning the TLD node (`ENSRegistry.setSubnodeOwner(0x0, labelhash(tld), baseRegistrar)`);
    - `SimplexPriceOracle`;
    - the `SimplexController` proxy, initialized with `minCommitmentAge` 60 s, `maxCommitmentAge` 86400 s and `minCharLength` 6. The PR's `deploy/simplex/00_deploy_simplex_controller.ts` sets these values; the test fixture uses 1 s;
    - `SubnameRegistrar`;
    - `SimplexResolver(ens, subnameRegistrar, controller, 0)`, with the controller as its trusted controller;
    - `SubnameRegistrar.setResolver(SimplexResolver)`;
    - `SimplexSponsor` and `setSponsor`;
    - `addController`, `setSubnameHook`;
    - `setMaxLabelLength(63)`, from `deploy/simplex/02` (the fixture leaves it unset);
    - `setDefaultResolver`, `setBeneficiary`;
    - the guardian's allowance for the registrar.

## Decisions (owner, 2026-10-09 and 2026-10-10)

The owner's decisions on the store, one per topic. The stages below implement them.

| Topic | Decision |
|---|---|
| Stores | Names at simplex.domains, badges at badges.simplex.chat. Separate to the user, each linking the other as a secondary option |
| Hosts | One Caddy serves both sites, which the service exports with the Stripe key injected. Both badges.simplex.chat and simplex.domains proxy `/api` to the service on their own origin, so there is no CORS. Webhooks stay at badges.simplex.chat/webhooks, as today |
| Code | Two webapps sharing logic, not screens. Badges stay in this repo, built on the library. Names live in the simplex.domains repo in that site's look, and are a submodule of simplex-chat (`apps/simplex-domains`). The shared logic (API client, payments, storage, codes, QR) is `packages/simplex-chat-store`, published as `@simplex-chat/store`. One service exports both sites |
| Service | The badge service is renamed **StoreService** (`apps/simplex-store-service`, `Simplex.Chat.StoreService`) |
| Search | One search bar, as registrars draw it (dynadot.com): a search icon, the name, a `.simplex` tag and a Search button inside. Enter searches too. The name is judged when searching, not while typing. The rules line ("6 letters or more…") appears only after such a mistake |
| Results | Available, with its price. Taken, with what uses it (address and/or channel: picture, display name, link, deep link). Reserved, with "Contact the SimpleX team" |
| Price | Internal and changeable, possibly per name. The page shows only the searched name's price |
| Term | 2 years for a paid name, fixed. No term screen. A code registers for its own `years` (investor codes: 5 or 3, as simplex.domains says today) |
| No hold, hash until reveal | The service sees only keccak(label) and its length until the register step, so it holds nothing. Two buyers may pay for one name, and the later one gets the taken outcome with the payment kept. Once its own register is sent, search answers registered |
| Wallet | The browser generates a wallet for each order: 128-bit entropy, shown as an owner code (`SN-…`). A taken order retargeted to another name keeps it. All key cryptography is in the browser. The code is shown before paying and confirmed as saved. Everything else is derived from it, so the code alone finishes or restores an order if the browser loses its storage. For that, the order keeps the name and links sealed under a key from the code |
| Registration | The browser sends the owner address and the commitment. After the wait it reveals the registration and signs the owner's `RegisterName` intent. The service sends `commit` and `registerWithCredit` from its registrar key, owner = that address. The page shows Committing → Waiting 60s → Registering, as in the app (canvas 9) |
| Codes | Two kinds, one sum type in core. The owner code (`SN-` and 30 characters) is a wallet: whoever holds it owns the name. The prepaid code (`SN6-…`, `SN7-…`, `SN20-…`, 20 characters) pays for one name of that many letters or more. The number is a convention; the service's table is the source of truth. The page asks the service what a prepaid code is for |
| Paying with a code | Checkout's order card has "I have a code" under Continue. A prepaid code that covers the name makes its price 0 |
| Check your code | The investor card on the first page and Check a code in the header open a popup that asks what a code covers. While it is applied, a covered name's price shows as $0 (investor codes) |
| Codes not sold | Codes are not sold on the web. They come from the app, from the operator (CLI, group), or from the page for a taken order, which the service stores as a hash. Each registers one name. Codes from the app are an owner decision: #7530 defers in-app purchase, and the names v2 RFC (`origin/ab/names-gui:docs/rfcs/2026-08-05-simplex-names-v2.md`) has in-app purchase buy a name directly |
| History | "Your names" lists names with Show code and Edit links, and prepaid codes. "Add a code" adds a prepaid code to the list with what it covers. "Restore with a name code" finds an order from its owner code alone (N7c) |
| Investor card | "Investors get a name free": the Wefunder sentence and link, with a names amount ($500+, as on simplex.domains today) and Check your code. A short version sits in the checkout's order card. The amount is a placeholder for now (a constant, as `crowdfunding.ts`) |
| Storage | Prepaid codes in their own table, not a kind column on the badge code table. Registrations in their own table. Owner codes are never stored by the service |
| Mockups | The board and screens generated by `plans/names-codes/mockups/`. The names screens come from `domains.js` on simplex.domains' fonts, logos and brand blue. The badges landing comes from the real badges webapp |
| Crypto in the browser | Vendor `@noble/secp256k1` and `@noble/hashes` (audited, MIT, no dependencies) at pinned versions, plus the BIP-39 English wordlist. WebCrypto covers SHA-256, PBKDF2-SHA512, HMAC-SHA512, HMAC-SHA256 and AES-256-GCM. They go into the names webapp in the simplex.domains repo. So the vendored crypto does not touch the badges webapp's rule (no dependencies beyond `typescript` and `@types/node`, `web/README.md`) |
| Address links | Optional at checkout: a contact address and/or a channel link, one per key on the page. A record set elsewhere may hold several, joined with `;`, and the page keeps the others when it edits one. Links are previewed through the service, and set by the register transaction itself (the commitment's `data`). Later edits: the browser signs each changed key with the name's key (`SetText`), and the service relays it as PR #35's `SimplexResolver.setTextWithSig`, paying the gas |
| Opened from the app | The app's link carries its own owner (`simplex.domains/#/?name=<label>&owner=<address>`), so the name is registered straight to the app's account. The page shows that address before paying. No owner code is made, so the order carries no `sealed`. There are no links and no "I have a code", since the app sets links and redeems codes itself. Open: the `RegisterName` intent must be signed by that owner, the app's wallet, so the app has to sign it |

## The names store, screen by screen

What the buyer sees, in order, and the outcomes off the main path.

1. **Search** (first screen): the search bar with `.simplex` and Search. A mistake shows the rules line. Search sends keccak(label) and its length, `GET /api/name/:hash?length=7`, and the answer appears on the same screen:
   - available, with its price, or $0 when an applied code covers it;
   - taken, with "Used by" cards, or taken and pointing nowhere;
   - reserved;
   - check failed or rate limited.

   Below it: how it works, what a name does, and the investor card with "Check your code".
2. **Checkout:** the optional links (N3g) and the payment method, beside an order card. The order card has the term (2 years, or the code's), the total, Continue, and "I have a code", which opens the code entry. When a code covers the name, the total is $0 and the payment method reads "Paid with your code".
3. **Payment:** the names app's own payment pages, on the shared payment logic: crypto with QR and the held rate, card, and their endings, with "Get a new invoice". Skipped when a code pays.
4. **Wallet and registration:**
   - At checkout the browser generates the owner code. It derives the address and the commitment secret from it (`secret = HMAC-SHA256(BIP-39 seed, "simplex.domains commitment " + label)`), and stores them with the name locally.
   - Continue at checkout first shows the code (N3h), whether paid or by prepaid code. It must be saved, confirmed with a checkbox, before the order is created, since nothing else can recover the name.
   - The page sends the address, the name's hash and length, the commitment and `sealed` with the order.
   - After payment the page shows the three steps, ticked as they complete, with transaction links.
   - After the wait the page reveals the registration and signs the `RegisterName` intent, so it must be open then. Reopening the link finishes it, and another browser asks for the code (N7b).
5. **Registered:** "Registered for 2 years, until 9 October 2028, to 0x014f…9b73" (a prepaid code's own years when paid by one).
   - The owner code, Copy, and a QR for the app to scan.
   - Next steps: redeem it in the app, point the name at an address, keep the code safe.

**Outcomes off the path:**
- **Taken between search and registration** (by another registrar, or by another order here that registered first): the order keeps its value.
  - Paid: the buyer has two choices.
    - Register another name at most its price, searched on the order's own page. A cheaper one refunds nothing.
    - Or take a code for the taken name's length (8+ for longer names) for 2 years. The page generates it, and the service stores its hash.
  - By code: the code is released, unspent, for another name.
- **Registration failed after paying:** the service's own retries gave up. The page shows "Paid, not registered", and Try again charges nothing. Refunds are manual, by support.
- **A code that does not work:** invalid or revoked, already used, reserved by another unfinished registration, expired, not yet paid, or not covering this name (the name is shorter than the code's length).
- **Payment endings:** the badge screens.

## Stages

The work is split into stages 0–7. Each stage is its own set of atomic commits. Stage 0 is reviewed before any code.

### 0. Design: board, spec and mockups (review gate)

This stage redraws the design for review before any code is written.

Redo `plans/names-codes/` for the store above, in simplex.domains' look:
- `names-flow.svg` and `screens/*.jpg`;
- the spec `2026-10-09-name-codes.md`;
- the generator inputs in `mockups/`: `domains.js`/`domains.css` for the names screens, `screens.js`/`mockups.css` for the other frames, `fixtures.js`, `layout.mjs`, `board.mjs`.

The board's sections, as `mockups/layout.mjs` lists them:

- simplex.domains: registering a name start to finish, what a search can find, and before paying (links, then the owner code);
- paying with a code: check, apply, and codes that do not work;
- when registering does not go through;
- opened from the app;
- where it meets the app (redeeming the code; grey, not built here);
- refused at checkout;
- the other ways a payment ends (the badges store's endings, on the names store's pages), with the owner-code pages;
- your names, the badges store, and the dark theme;
- simplex.domains on a phone;
- the operator.

### 1. Rename to StoreService (pure refactor)

This stage renames the badge service to StoreService without changing behavior. It follows #7530 §5, with StoreService for ShopService:

- A JSON test first pins every command and response tag, as #7530 proposes, and also the hand-written `BSE` error tags.
- `Simplex.Chat.Badges.Service` → `Simplex.Chat.StoreService`.
  - Constructor prefixes `BSC`/`BSP` → `STC`/`STP`, with `dropPrefix` updated in the same commit.
  - `BSE` → `STE` in `Simplex.Chat.Badges.Types`. Its wire tags are written by hand and stay as they are.
  - #7530 proposes `SHC`/`SHP`/`SHE`.
  - `bots/src/API/Docs/Types.hs` builds the error docs from `consSep "BSE"` and changes with it. Its `STEnum*` constructors start with `STE` but are unrelated.
- `ChatConfig.badgeServiceAddress` → `storeServiceAddress`.
- `apps/simplex-badge-service` → `apps/simplex-store-service`, `BadgeService.*` → `StoreService.*`, executable `simplex-store-service`, `scripts/store-service/`, tests and hspec labels.
- Persisted names stay: the `sx_badge_service_` prefix, the migrations table and the database. This is #7530's recommendation (D2), still open there.
- `plans/names-codes/mockups/board.mjs`, the regenerate note in `mockups/layout.mjs` (`NOTES`) and spec §13 follow the new web path.
- Coordinate with #7530 so the rename lands once.

### 2. Core and primitives: codes, label rule, registration

This stage puts the two kinds of code and the label rule in core, on top of Alain's primitives.

It builds on `origin/ab/names-registration-primitives-sponsored`. That branch already has the owner code, the `Registration` encoding, the commitment, the `RegisterName` intent and the chain client.
- Its intent has carried PR #35's nonce since `965046fe1`: `riNonce`, `registerWithCredit((…),uint256,uint256,bytes)`, and the `RegisterName(…,uint256 nonce,uint256 deadline)` typehash.
- The service still checks the nonce against `SimplexSponsor.nonces(owner)` itself.

- **`src/Simplex/Chat/Badges/Code.hs`:** one sum type for the two kinds of `SN` code, so a code's meaning is in its type:

  ```haskell
  data NameCode
    = NCOwner NameOwnerCode     -- "SN-" and 30 characters: the wallet's entropy and 22 bits of its SHA-256
    | NCPrepaid NamePrepaidCode -- "SN<len>-" and 20 characters: pays for one name of len+ letters
  ```

  - `parseNameCode :: Text -> Maybe NameCode` dispatches by the body's length after `SN`, separators removed:
    - 30 characters is an owner code;
    - 21 or 22 (the length digits and 20) is a prepaid code;
    - digits after `SN` cannot decide it, since an owner code's body may start with one.
  - `formatNameCode` renders either.
  - **Owner codes** are the branch's `NameCode`, renamed `NameOwnerCode` (`randomNameOwnerCode`, `nameOwnerCodeEntropy`).
    - The branch's `parseNameCode` and `formatNameCode` handle owner codes only. They become `parseNameOwnerCode` and `formatNameOwnerCode`, under the sum type's dispatching ones.
    - Their uses in `tests/BadgeTests.hs` and `WebUIMock.hs` follow.
    - Agree the rename with Alain.
  - **Prepaid codes** (`randomNamePrepaidCode`, `namePrepaidCodeHash`):
    - the length (1–2 digits), then 20 Crockford base32 characters;
    - a Luhn mod-32 check over the length digits and the payload;
    - the length is shown, not trusted: the table row decides;
    - the canonical form is hashed with SHA-256.
- **`Simplex.Chat.Names.Protocol`:** #7390's `validLabel`, tightened to simplexmq's hyphen rule, is the one character and hyphen check. The length is checked separately.
  - Its branch is not merged into #7530's or Alain's sponsored branch. So this stage ports the module, or lands after #7390.
  - Its comment there forbids a second copy, so the service reuses it rather than adding its own.
- **The `Registration` encoding** stays where the branch has it, `BadgeService/Names/Registration.hs`. The names webapp mirrors it in TypeScript with the branch's vectors.
- **Tests:** owner and prepaid code vectors, including that neither parses as the other; vectors shared with the names webapp.

### 3. Service: search, codes, orders

This stage adds the service's names endpoints: search, link previews, code checks, orders, the reveal, finding an order, link edits, the taken outcomes and retry. It also adds the tables, the wire errors and the operator commands.

- **`[names]` in the ini:** `resolver_url`, and `tld` (`testing` until `.simplex` is deployed).
  - `knownSettings` in `Config.hs` gains `[names]`, `[chain]` and the `[listener]` keys `names_static_dir`, `names_export_dir` and `names_hosts`.
  - The parser warns about and ignores any key not listed.

#### Search and previews

- **`GET /api/name/:hash?length=N`** answers a search by the name's hash. `:hash` is `0x` and 64 lowercase hex, as in request bodies.
  - **Input:**
    - It takes keccak256(label) and the label's length, never the label.
    - It refuses a length outside 6–63 with `invalid_name`.
    - It asks the resolver by hash, under its own rate limit, not the read limit the invoice long poll uses.
  - **Link cards** for a registered name:
    - It opens each `simplexContact`/`simplexChannel` link with the chat core's `APIConnectPlan`.
    - `WebEnv` has no chat controller today, since `serviceLanes` builds it without `cc`. So `cc` and the service user's id are threaded into it.
    - `APIConnectPlan` returns a short link's display name and picture without connecting. A full link has no profile, so its card shows the link only.
    - The fetch has a 3 s timeout, its own rate limit and a short cache. It adds a deep link.
  - **Answers:**
    - `available{price, currency, years, resolver, sponsor, chainId, tld}`:
      - `resolver` is the resolver contract the page puts in its commitment;
      - `sponsor` and `chainId` are for the intent's EIP-712 domain;
      - `tld` is for the records' node.
    - `registered{entities, records, owner, resolver, recordsNonce, chainId, tld}`:
      - `records` are the raw `{contact, channel}` values. They are read from the chain as `text(node, key)` on the configured default resolver contract, with `node = keccak256(namehash(tld) ‖ labelHash)`.
      - They are read raw because the resolver splits and trims values, and answers `""` for a reverted text read. That would make Edit links drop links. Reading them raw also keeps a link whose card could not be fetched.
      - They are read only when the answer's `resolver` is the default one. A name on another resolver contract or on none (PR #35 allows either without data) answers `records` and `recordsNonce` null, with its `owner`. Your names offers no Edit links for it.
      - The lag answer (below), with no `owner`, is not this case. There, Edit links takes the values from the order's links in `sealed`, or from this browser's own last mined edit of a key when it has one. A mined clear counts as an edit of both keys to "". So a second edit in the lag does not bring back removed links.
      - `owner` and `resolver` come from the resolver's answer, which reads them for the hashed node.
      - `recordsNonce` is `SimplexResolver.nonces(owner)`, read from the default resolver contract under the same condition, for edits by that owner.
    - `reserved`.
  - **Resolver failures in search** answer `provider_unavailable`:
    - The resolver's 502 `labelNotRecorded` is a registered name whose label the base registrar never recorded. It cannot occur on PR #35, where every registration records its label. Search would answer it `provider_unavailable`, as it answers the resolver's other errors.
    - Those other errors:
      - 502 `upstreamError`, for a failed status, owner or resolver read, or a lost RPC connection. A reverted text or address read it answers empty instead;
      - 400 `tldNotConfigured`;
      - 502 `unknown`, without the registrar address;
      - 502 `noPriceOracle`, without a readable price oracle or premium.
    - Also a resolver that does not answer, and a failed chain read.
  - **Resolver failures elsewhere** answer `name_check_unavailable` (503):
    - The order, `/api/name/register` and the `/name` retarget also ask the resolver by hash. On the same resolver failures they answer `name_check_unavailable` and create or change nothing.
    - They do not answer `provider_unavailable`, since the badges client reads `provider_unavailable` from invoice creation as an unavailable payment method (N3a).
    - The page shows N3d at checkout, and the error with the reference on the order page.
  - **The lag:** a name registered here may not show in the resolver yet.
    - This covers a hash the resolver reports available while a registration here is `registering`, or `registered` with `registered_at` within the last hour.
    - The hour is a margin for the resolver's node lagging the chain, since the resolver itself reads `latest` with no cache. After that the resolver is believed, so an expired name is free again.
    - Search then answers `registered{entities: [], tld}`, with no owner.
    - Edit links then takes the nonce, chain id and resolver from the registration view instead. It is offered only once the phase is `registered`; the view's `recordsNonce` is null before.
    - Once the resolver reports the name registered, it is answered like any other registered name. Its links show, and Edit links has the owner's nonce.
- **The link bound:** each record value (a key's links joined with `;`) is at most 4096 bytes of UTF-8. The service pays for each record's storage, so the value is bounded.
  - Checked by the page, by `/api/link/preview` for one link and by `/api/name/records` (`invalid_link`), and by the reveal (`bad_request`).
  - A new 4096-byte value takes about 129 storage slots, some 2.85M gas, against about 65k for its calldata and 33k for the `TextChanged` event.
  - So the register's gas limit reaches about 6.7M for two records at the bound. The branch's client takes that limit from `eth_estimateGas`, and its cap must allow it.
  - Estimated on the local anvil deployment: 446k with no records, 3.55M with one, 6.67M with two, plus about 30k when the owner's intent is checked. The gas used is about 5% lower near the bound, and 10–14% lower for short or no records.
  - Sizes: short links take about 70 bytes, and the apps' full contact and channel links about 300–400. A full contact address made with `pq_ratchet=on` in the CLI takes about 2.1 KB, so two of those in one record exceed the bound.
- **`POST /api/link/preview {link}`** shows a link's profile at checkout.
  - It checks the link's kind and the link bound. The page also checks the bound before building the commitment, since the links are fixed in it.
  - It then opens a SimpleX contact or channel link with the chat core, as for taken names.
  - It answers `{type, displayName, image}` or `invalid_link`.
  - Same timeout and cache as the link cards, and its own rate limit.
- **`POST /api/code/check {code}`** says what a prepaid code covers, without spending it.
  - An unknown code is `code_invalid`, with status 400, since the transport reads every 404 as `not_found`.
  - Otherwise it returns what the code covers (`minLength`, `years`), and whether it can still be used (`unused`, `reserved`, `used`, `revoked`, `expired`, `unpaid`).
  - It uses its own rate limit, since it confirms codes.
- **Prices:** `Catalog.namePrice :: [NamePrice] -> LabelHash -> Int -> Either CatalogRefusal CurrencyAmount` prices a name by the label's hash and length.
  - It reads a seeded insert-only `name_prices` table (`Catalog.defaultNamePrices`): per-length rules, overridable per name by the label's hash.
  - Checkout reprices, and refuses a changed price with `catalog_changed`, which now carries the new price.

#### Orders

An order carries the name's hash, the owner address, the commitment and `sealed`. It is paid by invoice or by prepaid code. Nothing is held: any number of orders may name the same hash.

- **`POST /api/invoice {kind: "name", labelHash, length, price, method, owner, commitment, sealed}`** creates a paid order. `sealed` is null for an order opened from the app, and so null in its view.
  - Its checks, in this order:
    1. The length: outside 6–63 it is `invalid_name`. Then the `sealed` check (below).
    2. An open invoice for the same commitment. If its owner, hash, length, price and method match, it answers with that invoice (a resend after a lost response). Otherwise it refuses with `400 {"error": "bad_request", "invoiceId": "…"}`. So one commitment never has two invoices open, and the page opens the existing order. A buyer who changes the method after a lost response cancels it there.
    3. A registration past `unpaid` for the same commitment (paid, or by code). It answers with that registration's order instead of a new one. So an invoice that settles while its cancel runs never gets a twin.
       - For a code registration that is `{invoiceId: null, kind: "name", registration}`, with no payment fields. The page opens it at `?registration=<id>`.
       - The library's `createInvoice` accepts it and skips its payment-field check.
    4. The resolver. It refuses with `name_taken` a name the resolver reports registered or reserved, or a hash in the lag: available at the resolver while a registration here is `registering`, or `registered` with `registered_at` within the last hour (the margin explained under search).
  - The two commitment checks run before the resolver, so a resend never meets its own registration as `name_taken`.
  - Then it creates an invoice. Settlement moves the registration to `queued`.
- **Get a new invoice** cancels the old invoice first when it may still be open, and makes the new order only after the cancel succeeds.
  - It applies from an ending where the old invoice may still be open: the card form not loading, or the payment details unavailable. A declined card stays in the form on the open invoice.
  - The cancel is `POST /api/invoice/:id/cancel` (create limit). What the page does with each answer:
    - `409 funded`: keeps the old order.
    - `409 not_open`, a `200` `paid` (settled during the cancel), or a `200` `expired` with payment marks (a payment read during the cancel): re-reads the invoice.
      - Paid, with its registration past `unpaid`: shows its order.
      - Paid, with it still `unpaid` (paid after it closed): goes on to the new order.
      - Otherwise, one holding a payment: keeps the old order, as `funded` does.
      - One expired with nothing received, as a successful cancel leaves it: goes on to the new order.
    - A `200` still `open`: shows `CANCEL_STILL_OPEN` and keeps the old order.
    - `503 provider_unavailable`: keeps it and shows the error.
  - Stripe may refuse to cancel an intent already processing. That arrives as this `503`.
    - The invoice stays open until the intent succeeds or the overdue sweep closes it, since `signalOf` reads `processing` and a failed payment's `requires_payment_method` as open.
    - Once `settleWindow` and an hour of refused cancels have passed, the sweep marks it expired locally, without cancelling at Stripe.
    - An intent that succeeds after that is a late payment, refunded.
  - The re-read is new. The badges webapp's `main.ts` shows `CANCEL_NOT_OPEN` on `not_open` and checks only a `200` answer.
  - Cancel judges `funded` only from payments the service has read.
    - So a crypto payment not yet read is invalidated with the invoice and becomes a refund case.
    - The poller already alerts on it ("expired holding … needs a refund"), and no twin registration results.
    - The badges webapp's `newInvoice()` only clears the session today.
  - Get a new invoice makes a new order for the same name and wallet, so it reuses the commitment.
- **Closed invoices and cleanup:**
  - An invoice that expires or is canceled leaves its registration `unpaid`.
  - A late payment of it does not queue it, since Get a new invoice reuses the commitment. It is kept for support to refund.
  - An `unpaid` registration is deleted, with its `name_invoices` and `invoices` rows, in one transaction 30 days after its invoice closed. One whose invoice received any payment, partial or late, stays for support.
  - The cleanup first reads the invoice from the provider itself (`pReadInvoice`, then settlement as for any read). The poller and the provider list pass read an invoice only within about 72 hours of its creation (`settleWindow`). After that only a webhook brings a payment.
  - Today's BTCPay handler acts only on `InvoiceProcessing`, `InvoiceSettled`, `InvoiceExpired` and `InvoiceInvalid`. It gains `InvoiceReceivedPayment` and `InvoicePaymentSettled`, as read hints for a payment into a closed invoice.
  - The cleanup's read is the backstop. A payment whose webhook was lost is still recorded and alerted. If the provider does not answer, the registration is kept until the next daily run.
- **`POST /api/name/register {labelHash, length, owner, commitment, sealed, code}`** registers a name paid by a prepaid code. There is no invoice.
  - It takes the code check's rate limit, since it too reveals a code's state.
  - Right after the rate limit, it checks for a repeat. A registration whose `name_prepaid_code_id` is this code, with the same owner and commitment, answers that `registrationId`. This holds in any phase, the code spent or not. So a lost response is safe even once its own registration makes the name look taken.
  - It applies the invoice's two commitment checks, at the same point (after the length and `sealed` checks, before the resolver):
    - a registration past `unpaid` for the commitment answers that registration's id, with its `invoiceId` when it was paid, so the page opens `?order=<id>`;
    - an open invoice for the commitment is refused with `400 {"error": "bad_request", "invoiceId": "…"}`. The page cancels it first, as above.
  - It refuses a length and a name as the invoice does (`invalid_name`, `name_taken`).
  - It checks that the code covers the length, and marks the code reserved for this registration.
  - It spends the code when the registration succeeds.
- **Formats:**
  - `owner` is a checksummed address.
  - `labelHash`, `commitment` and `secret`, in bodies and as `:hash`, are `0x` and 64 lowercase hex. Anything else is `bad_request`.
  - `codeHash` stays the badge codes' unpadded base64url.
- **The commitment** is built by the page:
  - `commitment = makeCommitment(Registration{label, owner, duration, secret, resolver, data = one setText call per key that has links, its links joined with ";" (empty without links), reverseRecord = 0, referrer = 0})`;
  - `secret = HMAC-SHA256(BIP-39 seed of the owner code, "simplex.domains commitment " + label)`.
  - This is the contract's struct, as the branch's `registrationTuple` encodes it from its `Registration` (label, TLD, owner, duration, secret, resolver, contact and channel links).
  - `duration` is 2 years, or the prepaid code's years, at 365 days a year.
  - Its `data` is the records' `setText` calls.
- **`sealed`** lets a browser that has only the owner code rebuild the commitment and reveal.
  - It is the JSON `{label, contact, channel}`: each key's record value, its links joined with `;`, "" without links.
  - The page encrypts it with AES-256-GCM under `HMAC-SHA256(seed, "simplex.domains sealed")` and a random 12-byte IV. The result is unpadded `base64url(iv ‖ ciphertext ‖ 16-byte tag)`; WebCrypto's output carries the tag.
  - The service stores it unread, and returns it in the registration view.
  - It refuses one longer than 11095 characters (`bad_request`). That is the base64url of the 12-byte IV, 8293 bytes of JSON for a 63-character label and two 4096-byte values, and the 16-byte tag. SimpleX links need no JSON escaping.
  - Today's body cap, `maxBodyBytes = 8192` in `Web/Server.hs`, would refuse such an order before any name check. So `/api/invoice`, `/api/name/register`, `/api/registration/:id/name` and the reveal read bodies up to a new `nameBodyBytes = 16384`. A longest order is about 11.4 KB, and a reveal with two records at the link bound under 9 KB.
- **`POST /api/registration/:id/reveal {label, secret, records, nonce, deadline, signature}`** gives the service the name, the links and the owner's signed intent, once the commitment is old enough.
  - It is accepted in `committed` while `label` is null. The page sends it once `minCommitmentAge` has passed.
  - Once `label` is set, an identical repeat answers `200`, in that or any later phase. A different body is `bad_request`.
  - The body:
    - `records`: the links chosen at checkout, `{contact: [...], channel: [...]}`, empty when none;
    - `signature`: the owner's EIP-712 `RegisterName(string label,address owner,address resolver,bytes[] data,uint256 nonce,uint256 deadline)` signature, under the `SimplexSponsor` domain;
    - `nonce`: `SimplexSponsor.nonces(owner)`, which the registration view carries as `intentNonce`. It is 0 for a new owner code;
    - `deadline`: the view's `revealBefore`. The controller refuses the commitment from then on, so a later deadline gains nothing.
  - The checks:
    1. The label is valid, hashes to `labelHash` and has `length`. So its price is the registration's `price_id` row, fixed at order or retarget, or it is covered by the prepaid code.
    2. Each link is a SimpleX link of its kind, within the link bound.
    3. The resolver is the default resolver contract.
    4. The service rebuilds `data` and the commitment.
    5. `verifyRegisterIntent`: owner signed, deadline not passed, secret 32 bytes.
    6. A low-s check of its own, since `recoverPublicKey` accepts high s and the sponsor's `SignatureChecker` rejects it. The signature is read as exactly 65 bytes `r ‖ s ‖ v`, with `v` 27 or 28, as OpenZeppelin requires. The recovery id is `v − 27`.
    7. `nonce == SimplexSponsor.nonces(owner)`, read from the chain.
  - A failed check is `bad_request`, and the registration stays `committed`.
  - The intent does not sign the duration, so the service sets it from what was paid.

#### Finding an order and editing links

- **`POST /api/registration/find {owner, timestamp, signature}`** finds orders for a buyer who has the owner code but not the order link.
  - The page derives the owner from the code and signs `simplex.domains find <owner> <timestamp>` (EIP-191) with its key.
    - `owner` is lowercase `0x` hex, and `timestamp` decimal Unix seconds.
    - The signature is read as the reveal's: 65 bytes, with `v` 27 or 28.
  - The service checks that the signer is `owner` and the timestamp is within 10 minutes.
  - It returns `{registrations: [{registrationId, invoiceId, phase, createdAt}]}`, newest first, or `not_found` when there is none.
    - One entry for each registration of that owner, retargeted ones included.
    - `invoiceId` is the order link's id, and null for a prepaid-code registration.
  - Get a new invoice reuses the wallet, so an expired order's `unpaid` registration may be listed too. The page opens the one that is not `unpaid`, or the newest.
  - It has its own rate limit.
- **`POST /api/name/records {labelHash, key, value, nonce, deadline, signature}`** (or `{labelHash, clear: true, nonce, deadline, signature}`) changes one link record later. The page signs the edit with the name's key, and the service relays it and pays the gas.
  - **The request:**
    - `key` is `simplex.contact` or `simplex.channel`. Each link must be a SimpleX link of that kind (`invalid_link`).
    - `deadline` is when the page signs, plus one hour.
    - A record holds its links joined with `;`. So removing one of several links rewrites the joined value, and clearing a key sets it to "".
  - **What the page signs:**
    - EIP-712 `SetText(bytes32 node,string key,string value,uint256 nonce,uint256 deadline)`, under the `SimplexResolver` domain, with the name's key.
    - `nonce` is `SimplexResolver.nonces(owner)`, one counter for all of the owner's edits. `GET /api/name/:hash` returns it with the chain id for a registered name, and the registration view returns it while search lags.
    - For the clear form, EIP-712 `ClearRecords(bytes32 node,uint256 nonce,uint256 deadline)` instead, under the same domain and nonce. It wipes every record of the name at once, through `clearRecordsWithSig`.
  - **Sending:** the service relays `setTextWithSig` (or `clearRecordsWithSig`), one transaction each, paying the gas. It sends through the registrar key's one sender, the worker's, so an edit and a commit never collide on the key's nonce.
  - **The checks, in order:**
    1. The records rate limit, on every request, before any other check.
    2. The key and link checks.
    3. The signer. The service recovers the `SetText`/`ClearRecords` signer (`recoverAddress`, no chain read).
       - The digest encodes `node` as `bytes32`. simplexmq's `Eip712Value` on its branch has no constructor for that, so it goes through `VUint`, not the hashing `VBytes`.
    4. Low s, as PR #35's OpenZeppelin `SignatureChecker` requires. simplexmq's `recoverPublicKey` on its branch accepts high s, which would pass here and revert on chain. The signature is read as the reveal reads it. Else `bad_request`.
    5. An identical repeat, looked up by signer and nonce and matched by `body_hash` (below), is answered now, before any deadline check.
    6. For a new edit:
       - the deadline must be ahead of the latest block's time, else `bad_request`;
       - past the per-name cap it answers `rate_limited`;
       - the signer must be the registry's `owner(node)`, read from the chain with `node = keccak256(namehash(tld) ‖ labelHash)` and the registry `baseRegistrar.ens()`;
       - the edit's nonce must equal `SimplexResolver.nonces(owner)` on chain;
       - else `bad_request`.
  - No refusal counts toward the cap, which counts `name_record_edits` rows.
  - **A new edit:**
    1. The service inserts the pending row before the edit joins the sender. So a concurrent twin loses the unique insert before any signing.
    2. It sends the edit.
    3. It answers `{tx}` once it is mined, or `record_reverted` with the revert's name when known.
  - **Naming a revert:**
    - A dry run's revert carries its data where the node returns it. Anvil and Hardhat 3.19.0 do; reth is unchecked (gap 5).
    - A mined revert's receipt has no data, and the resolver host's reth offers no `debug_traceTransaction`.
    - So a mined revert is named `SignatureExpired` when its block's timestamp is past the deadline, and is unnamed (`revert` null) otherwise.
  - **The re-read after a revert:** anyone can relay a signed edit, so a revert may mean the edit was already applied.
    - An applied edit reverts `InvalidNonce`, or `SignatureExpired` once its deadline has passed.
    - So on any revert the service first re-reads the nonce and the record.
    - An edit whose nonce the chain has passed, with the record holding its value, is answered as mined, with `{tx: null}`. For a clear, both keys must read "".
    - `{tx: null}` means the applying transaction is unknown: another relayer's, or the service's own with its receipt pruned.
  - **After a restart**, each pending edit resumes:
    - With no transaction yet and its deadline ahead, it is sent.
    - Otherwise its receipt is polled. Its saved raw transaction is rebroadcast while the node does not know it and the registrar nonce is unused.
    - Once the registrar nonce is used with no receipt found, the edit is checked on chain by its nonce and the record. If applied, it becomes `mined`, with no hash known to have mined. Otherwise it is signed again under a new registrar nonce, while its deadline is ahead.
  - **Expiry:** an edit with nothing mined, once a block `confirmations` deep has a timestamp past its deadline, gets the same re-read.
    - If applied, it becomes `mined`, with no hash known to have mined.
    - Otherwise it becomes `reverted` (`SignatureExpired`), freeing the nonce.
    - Its saved transaction is still rebroadcast until its registrar nonce is used, so later transactions of the key never wait on a gap. Mined late, it reverts `SignatureExpired` harmlessly.
  - **Repeats:** an identical repeat is the same signed body, matched on any row. It never sends another transaction.
    - While the edit is pending, the repeat waits for the same transaction.
    - Once mined, it answers its `{tx}`, or `{tx: null}` again for one found applied with the transaction unknown.
    - For a reverted edit, it answers `record_reverted` again.
    - Only a body that matches no row is checked as new. With a nonce whose edit is pending or mined, it is `bad_request`.
    - After `record_reverted` the nonce is free again for a new body.
  - **The page's order of edits:**
    - The page signs the next edit only after the answer: with the next nonce after `{tx}`, and with the same nonce after `record_reverted`, since a reverted edit leaves the on-chain nonce unchanged.
    - After any `bad_request` it re-reads `recordsNonce` from `GET /api/name/:hash`, or from the registration view while search lags.
    - So edits go one at a time and in order, and a failed one never strands later ones. The contract does not queue intents: a higher nonce reverts `InvalidNonce`.
  - Your names hides Edit links once the registered answer carries an `owner` other than the code's address (after redeeming).
  - PR #35 has no edit credits. So the service bounds edits with its own rate limit and a per-name cap.

#### Taken orders, the registration view and retry

- **A `taken` order** has two ways on, both signed by the owner, as `find` is.
  - The signature is read the same way, and the timestamp must be within 10 minutes. The owner signs because the registration id is readable by anyone holding the order link.
  - The signed messages (EIP-191):
    - `simplex.domains name <registrationId> <labelHash> <length> <commitment> <sha256(sealed)> <timestamp>`;
    - `simplex.domains code <registrationId> <codeHash> <timestamp>`.
  - Their fields:
    - `length` in decimal;
    - `labelHash`, `commitment` and `sha256(sealed)` as lowercase `0x` hex. `sha256(sealed)` is the SHA-256 of the `sealed` string's ASCII bytes;
    - `codeHash` as the badge codes' unpadded base64url SHA-256 of the canonical code;
    - `timestamp` in decimal Unix seconds.
  - An order opened from the app has no owner key on the page, so it cannot use them (gap 10).
  - **`POST /api/registration/:id/name {labelHash, length, commitment, sealed, timestamp, signature}`** retargets the order to another name.
    - Only a paid registration in `taken` may, else `bad_request`. A page whose response was lost re-reads the view, which shows the retarget.
    - It runs the order's checks on the new hash (`invalid_name`, `name_taken`).
    - It requires `namePrice` at most what was paid, else `bad_request`.
    - It replaces the registration's `price_id` with that price row, so the reveal takes that row as the new label's price.
    - It replaces the hash, length, commitment and sealed order.
    - It clears `label`, `secret`, `records`, `intent_*`, the transaction hashes and blocks, `reveal_after`, `reveal_before` and `error`. So the worker cannot register with the old intent, and the view shows no old transaction.
    - It commits again, and the page reveals again.
  - **`POST /api/registration/:id/code {codeHash, timestamp, signature}`** takes a prepaid code instead.
    - An identical repeat (the registration's `issued_code_id` has this hash) is answered 200 again. This is checked after the rate limit and the owner's signature.
    - Otherwise only a paid registration in `taken` gets a code: for its current `length` (8+ for longer names), for 2 years, paid and without expiry.
    - The page derives the code from the owner code's seed. Its payload comes from `HMAC-SHA256(seed, "simplex.domains taken code " + registrationId)`. So the owner code recovers it, and a repeat sends the same hash. The page sends only the hash.
    - `code_conflict` keeps its badge meaning: a hash another code already has. A derived code cannot be regenerated. But with the registration id in it and a 95-bit payload, a conflict is a practical impossibility, and the order page would show the error with the reference.
    - An order opened from the app cannot use this endpoint (gap 10).
    - The service inserts a `name_prepaid_codes` row for the hash and sets `issued_code_id` to it. The service sets `min_length` and `years` from the registration, not from the code's text.
- **`GET /api/registration/:id`** is the long-poll view a code registration uses. `GET /api/invoice/:id` carries the same object for a paid one.
  - Its fields: `{id, phase, labelHash, label (null until the reveal), owner, commitment, sealed, resolver, sponsor, chainId, tld, years, commitTx, registerTx, revealAfter, revealBefore, intentNonce, recordsNonce, expiresAt, error}`.
  - `commitTx` and `registerTx`: each is chosen among the hashes kept since they were last cleared. It is the one that mined with `status` 1, else the latest sent. It is null when judged mined from chain state with no hash known to have mined with `status` 1.
  - `revealBefore` is the commit's block time + `maxCommitmentAge` (24 h in PR #35's deploy).
  - `intentNonce` is `SimplexSponsor.nonces(owner)`.
  - `recordsNonce` is set once registered, for Edit links before search has the owner.
  - The id is a random 128-bit token, like order ids. Whoever holds it can act on the registration.
- **Phases:** `unpaid` → `queued` → `committing` → `committed` → `registering` → `registered`; or `taken`, `failed`.
  - `unpaid`: its invoice not settled while open.
  - `queued`: paid, or code reserved.
  - `committed`: waiting for `minCommitmentAge` and the reveal.
  - A commitment not revealed within the controller's `maxCommitmentAge` lapses to `failed`.
  - From `taken`, a paid order goes back to `queued` with its new label hash, or ends `code_issued`. A code order ends at `taken`, with its code released.
  - A code's reservation is described with its table below.
- **Chain reads:** a chain read that fails answers `provider_unavailable` (503) and changes nothing.
  - This applies in search, the reveal (not counted as its `bad_request`), the records relay (not counted toward the cap), the registration view's nonces and `/retry`.
  - The view reads nonces only in `committed` (for `intentNonce`) and in `registered` (for `recordsNonce`), null otherwise. So a chain outage never blocks an unpaid order's payment page.
  - A failed read is the node unreachable or answering an error. An RPC error is a failed read like any other.
  - These are not failed reads:
    - A revert that is itself the answer, as `ownerOf` for a name not held, counts as not this owner's.
    - A null receipt counts as not mined (pending or pruned). Once its registrar nonce is used, the chain state decides: `commitments` for a commit, `ownerOf` for a register, the nonce and the record for an edit.
  - Retries:
    - For edits, every re-read the sender makes (after a dry-run or mined revert, an expiry, or a used registrar nonce) is retried until it succeeds. The edit is kept `pending`, and its repeats wait.
    - For registrations, the worker's reads (`ownerOf`, `available`, `commitments`, receipts) are retried, with the registration kept in its phase.
- **Rate limits:**
  - Search, link preview, code check, find and records each have their own. Each is taken after route and method matching and before any other check, as `limited` does today.
  - The profile fetch for a taken name's links has its own, applied per fetch.
  - `/api/name/register` shares the code check's.
  - `GET /api/registration/:id` and the reveal take the read limit, like the invoice long poll. Each `bad_request` from the reveal counts against it.
  - `/name`, `/code` and `/retry` on a registration take the create limit.
- **`POST /api/registration/:id/retry`** is Try again. It works from `failed` only (else `bad_request`), in this order:
  1. A register of this registration that went through, with only its confirmation lost.
     - Either one of its sent register transactions has a receipt with `status` 1. Every hash is kept, fee-bumped replacements included.
     - Or `ownerOf(labelHash)` is the owner. The token id is the label hash. A revert, for a name not registered or expired, counts as not.
     - The registration becomes `registered`, with `registered_at`, `expires_at` from `nameExpires`, and its prepaid code spent, whoever holds the name now. If the sweep released the code to another registration meanwhile, the operator is alerted.
  2. A prepaid-code registration whose released code is now used, revoked, expired or held elsewhere answers that code error. It stays `failed`, with `failed_at` unchanged.
  3. Otherwise back to `queued`, at no charge.
     - It clears `failed_at`, `error` and the register's transaction hashes and blocks (step 1 has read them).
     - When `intent_nonce` is no longer `SimplexSponsor.nonces(owner)`, it also clears `label`, `secret`, `records` and `intent_*`. The view's null `label` then makes the page reveal and sign again, rather than the worker resending a dead intent. The reveal is accepted in `committed` while `label` is null.

  Back in `queued`, the worker reads `commitments(commitment)` and acts on the value.
  - **What the value means:**
    - A successful register deletes the commitment, and a lapsed one keeps its time. So 0 means never committed, not yet mined, or consumed.
    - Step 1 rules out consumed while the owner still holds the name. The commitment covers the owner, so whoever consumed it registered the name to this owner.
    - A consuming register goes undetected only once the name has left the owner (redeemed, transferred or expired) after a register this registration did not send. That register came from another registrar holding the signed intent, or from the owner's own payable register with the code's secret (gap 22).
  - **When it reads 0:**
    - With a commit of this registration pending (any of its hashes), the worker waits for that receipt.
    - Otherwise it commits again. A commit that reverts `UnexpiredCommitmentExists` (an earlier one mined meanwhile) moves it to `committed`, with `reveal_after` and `reveal_before` from `commitments(commitment)` and the controller's ages.
  - **When it reads non-zero:**
    - Its `revealBefore` is `commitments(commitment)` plus `maxCommitmentAge`, since `reveal_before` may be null.
    - A live commitment (read before that time) is reused. The registration goes straight to `committed`, with `reveal_after` and `reveal_before` from `commitments(commitment)` and the controller's ages.
    - Anyone can commit a public commitment again after it lapses. So the commitment's time may not be the block time of a commit of ours that mined with `status` 1 (a third party committed it again). Then `label`, `secret`, `records`, `intent_*` and the commit's transaction hashes and blocks are also cleared. The page signs again, and the view shows no commit of ours.
    - With less than `revealMargin` left before `revealBefore`, the worker does not reuse it. `revealMargin` is time for the page's reveal and the register to mine; its value is open (gap 29). The worker waits until a second after `revealBefore`, and goes on as below.
    - At `revealBefore` itself, the commitment can neither register (`CommitmentTooOld`) nor be committed again (`UnexpiredCommitmentExists`). So the worker waits a second.
    - After `revealBefore`, the worker clears `label`, `secret`, `records`, `intent_*`, `error`, the transaction hashes and blocks, `reveal_after` and `reveal_before`. Then it commits the commitment again with the same secret, since the old intent's deadline has passed. The view's null `label` makes the page reveal and sign again.

#### Storage, errors and the operator

- **Migration `20261010_names`** (SQLite and Postgres), each table with the `sx_badge_service_` prefix:
  - **`name_prepaid_codes`:** `name_prepaid_code_id`, `code_hash` (unique), `min_length`, `years`, `code_payment_status`, `reserved_by` (the registration holding it, unique when set), `reserved_at` (when, for the operator), `spent_at`, `revoked_at`, `expires_at`, `created_at`.
    - A code is single-use. It is reserved from `queued` until it is `registered` (spent) or `taken` (released).
    - `failed` keeps it for Try again, for 30 days. Each failure sets `failed_at`, and a requeue clears it.
    - A daily sweep releases codes whose registration is still `failed` with a `failed_at` 30 days old.
    - A later retry reserves the code again if it is still unused. Otherwise it answers `code_used`, `code_invalid` (revoked) or `code_expired`, or `code_reserved` when another registration holds it.
  - **`name_prices`:** `price_id`, `min_length`, `label_hash` (null for a length rule), `price`, `currency`, `status`, `created_at`.
  - **`name_registrations`:**
    - Columns: `registration_id`, `label_hash`, `length`, `label`, `secret`, `records`, `intent_nonce`, `intent_deadline` and `intent_signature` (null until the reveal), `sealed`, `owner`, `commitment`, `phase`, transaction hashes and blocks, `reveal_after`, `reveal_before`, `years`, `expires_at`, `price_id`, `name_prepaid_code_id`, `issued_code_id`, `registered_at`, `failed_at`, `error`, `created_at`, `updated_at`.
    - Transaction hashes: every one sent, fee-bumped replacements included. Each is saved with its raw signed transaction once signed and before it is sent.
    - `reveal_after` and `reveal_before`: the commit's block time plus the controller's ages, read at the commit.
    - `price_id`: the price paid for, replaced at retarget; null when paid by code.
    - `name_prepaid_code_id`: the code paying; null when paid.
    - `issued_code_id`: the code taken instead of a taken name.
    - The invoice links through `name_invoices.registration_id`.
    - Indexed by `label_hash` with `phase` (search and orders), and by `owner` (find).
    - A restart rebroadcasts each saved raw transaction that has no receipt. It rebroadcasts it again after a timeout, while the node does not know it and its registrar nonce is unused.
    - Once that nonce is used, the worker reads the effect on chain before signing again, since a pruned node also shows a mined transaction as unknown:
      - A commit counts as mined when `commitments(commitment)` plus `maxCommitmentAge` is still ahead. The worker sends a commit only when the commitment reads 0 or has lapsed, and a lapsed one keeps its old time. The registration becomes `committed`, with the reveal times from `commitments()`.
      - A register counts as mined when `ownerOf` is the owner.
  - **`name_record_edits`:**
    - Columns: `label_hash`, `owner` (the recovered signer), `nonce`, `key`, `value` (null for the clear form), `deadline`, `signature`, `body_hash` (of the signed request), transaction hashes, `state`, `created_at`.
    - Transaction hashes: every one sent, each saved with its raw signed transaction once signed and before it is sent.
    - `state`: `pending`, `mined`, or `reverted` with the revert's name.
    - Unique on owner and nonce among pending and mined rows. So a reverted row stays for its repeat while the nonce is reused.
    - A row found applied on chain with no receipt for any of its hashes is `mined`, with no hash known to have mined.
    - The service answers identical repeats from the table after a restart, and the per-name cap counts its rows.
  - **`name_invoices`:** `invoice_id`, `registration_id`, `price_id`, `provider_ref`.
  - **The badge tables are untouched**, so there is no table rebuild.
  - **Invoice queries:** today's are badge-only, so each gains the name side.
    - The queries: the shared `invoiceRowSelect` (inner-joined to `badge_code_invoices` and `badge_codes`); `qGetInvoice`, behind `GET /api/invoice/:id`, cancel and settlement (`settlementInvoice`); the poller's `qUnpaidRefs`, `qOverdueInvoices` and `qExpireOverdue` (whose update requires a `badge_code_invoices` row); and the lookups by `provider_ref`.
    - Cancel's `qUpdateInvoiceStatus` is a compare-and-set on the id and the observed status, with no join to the badge tables. It needs no name side.
    - Settlement also requires a code hash (`settlementCodeHash`, `qCodeHashForInvoice`), which a paid name invoice does not have. For a name, settlement moves the registration to `queued` instead:
      - only when the invoice was still open;
      - in the same database transaction that marks the invoice paid;
      - with both publishes after it commits, so a long poll never reads `paid` with a stale `unpaid`.
    - Today a settled signal marks even an expired invoice paid (`Orders.hs` `decide`, the poller's "an expired invoice can still be marked paid"), with no alert.
    - For a name invoice already expired or canceled, settlement marks it paid, leaves the registration `unpaid`, and alerts the operator to refund, since Get a new invoice reuses the commitment. The order page and history then show "Paid after the invoice closed; support refunds it" with the reference (not drawn).
    - `InvoiceRow`'s `irBadgeType` and `irMonths` become the product: a badge's type and months, or a name's registration id.
  - **Provider-ref lookups:** the webhook and poller queries that find an invoice by `provider_ref` cover both `badge_code_invoices` and `name_invoices`.
    - As in `Web/Server.hs` today, a row whose provider differs from the webhook's is ignored, as a guard against a collision between providers.
    - A ref must also be unique across the two tables. The lookup checks both, and refuses one found in both.
- **Wire errors:** new on HTTP and in `api.ts`'s `WIRE_ERROR_CODES`:
  - `invalid_name`;
  - `name_taken`;
  - `name_check_unavailable`: the resolver failing during an order, register or retarget (N3d at checkout);
  - `invalid_link`;
  - `record_reverted`;
  - `code_not_covering`;
  - `code_reserved`;
  - the chat protocol's code errors `code_invalid` (unknown or revoked), `code_used`, `code_expired` and `payment_pending` (an unpaid code).
  - A retarget to a name dearer than the payment is `bad_request`.
- **Operator:**
  - `//issue name <length> [years <Y>] [paid|unpaid|free]`, and group `/issue name <length> [years <Y>]` / `/bulk name <length> [years <Y>] count <N>`, issue single-use prepaid codes.
    - Without years, a code covers 2.
    - Fewer than 2 is refused, since the controller reverts `DurationTooShort` below 730 days.
  - `//revoke` takes a badge code or a prepaid code.

### 4. Service: registration on chain

This stage makes the service send the commit and the register from its registrar key, and track each registration through its phases.

It uses the branch's chain client (`BadgeService/Names/Chain.hs`, `sendCommit`, `sendRegister`):
- JSON-RPC to an RPC URL. By #7530's decision D5 that is the reth node on the resolver host. Its compose file runs reth `--minimal`, which likely prunes the hash-to-transaction index and old receipts the worker and `/retry` need (gap 25 in the sequence doc).
- A dry run (`eth_estimateGas`) of every transaction.
- EIP-1559 fees.
- Signed by the registrar key.

`[chain]` holds the RPC URL, chain id, the controller, sponsor, base registrar and resolver addresses, the registrar key and the confirmation depth.

- **Signing:** `signRecoverable` and `signEip1559Tx` come from simplexmq #1887, on `names`. EIP-712 hashing and signer recovery come from simplexmq `ab/name-registration-primitives-sponsored`, a PR into `names`.
  - Both must reach the simplexmq the service builds against.
  - The branch's `cabal.project` still points at a local checkout.
- **Worker**, one per registration, resumable after a restart:
  1. `queued` (paid, or a prepaid code reserved) → `sendCommit` with the page's commitment → `committing`.
  2. → `committed` once the commit's receipt is in. It records `reveal_after` and `reveal_before` from its block time and the controller's `minCommitmentAge` and `maxCommitmentAge`. Then it waits for `reveal_after` and the page's reveal.
     - Before the reveal the service has no label. So a commit's outcome (its dry run, or its receipt's `status`) is checked against `commitments(commitment)`.
     - A commit that reverts while its commitment is live (`UnexpiredCommitmentExists`) makes the registration `committed`, with the reveal times from `commitments()`. This holds for a revert in its dry run (nothing sent) or mined with `status` 0.
     - A commit that reverts with no live commitment (out of gas, or a commitment that lapsed by the read) is sent, or sent again if it was mined.
     - When the read falls at `commitments(commitment)` plus `maxCommitmentAge` itself, it is sent a second later, as the retry path waits at `revealBefore`.
     - When the commitment had lapsed, `label`, `secret`, `records`, `intent_*`, `error`, the transaction hashes and blocks, `reveal_after` and `reveal_before` are also cleared before the new commit is signed, as retry does after `revealBefore`. So the page signs again.
  3. `verifyRegisterIntent`, and the dry run of `registerWithCredit(registration, nonce, deadline, signature)` on the sponsor. Its reverts lead to `registered`, `taken` or `failed`, as below. Then → `registering` with `register_tx` saved, then the send.
  4. → `registered` at a receipt with `status` 1 and `ownerOf(tokenId) == owner`, + 3 blocks. It records `registered_at` and `expires_at`.
     - A mined transaction can still revert: the deadline passed, the name went in the same block, or the allowance ran out.
     - On `status` 0 the worker reads `ownerOf(labelHash)` first. The owner holding it means `registered`: a duplicate or resumed send.
     - Then it reads `available(label)` to choose `taken` or `failed`.

- **The long poll for phases:** each phase is published to the long poll, so the page follows the registration without asking again.
  - **Why today's long poll cannot carry it:**
    - `holdFor` in `Web/Server.hs` answers a paid invoice at once. It holds only while `?wait=` is not `paid`.
    - The waiters (`Waiters.hs`) track only the invoice status and a payment counter. Settlement, cancel and the poller's expiry sweep publish them.
    - `waitForChange` in `api.ts` throws on `paid` as terminal.
  - **A separate phase map** tracks the registration's phase.
    - It is keyed by registration id, since a prepaid-code registration has no invoice.
    - It is left out of `waitingCount`, which sets the payment poller's cadence (`waiting_seconds` against `idle_seconds`).
    - The phase is published on every phase change, by the worker or the service (settlement, `/retry`, `/name`, `/code`), after its database transaction commits.
  - **`GET /api/invoice/:id`** takes a `seenPhase` parameter beside `wait`.
    - It holds while its status and payment marks are the ones seen and, for a name invoice, its phase is `seenPhase`. It wakes on a change to any of them.
    - A paid name invoice, whose status cannot change, holds on the phase map alone. So it adds nothing to `waitingCount`.
    - One that reads `paid` with phase `unpaid` (paid after it closed) is not held.
  - **`GET /api/registration/:id`** takes `seenPhase`, and holds while the phase is the one seen.
  - **The library:**
    - `waitForChange` sends `seenPhase` for an invoice with a registration. It returns when its status, payment mark or phase changes. It treats `paid` as terminal only for an invoice without a registration.
    - A new `waitForRegistration(id, seenPhase)` long-polls `GET /api/registration/:id` the same way.
    - Both stop at `registered`, `code_issued`, `taken` and `failed`. `waitForChange` also stops at `unpaid` once the invoice is no longer `open` (expired, canceled, or paid after it closed).
    - The page polls again after the buyer's own action (a retarget, a code, Try again).
- **The branch's "left to the plumbing PR"** is this stage's checklist:
  - verify the intent before sending;
  - check the duration and price against what was paid;
  - commit only a paid order;
  - accept only the default resolver contract, bound the links and cap the gas limit;
  - validate and lower-case the label;
  - one transaction at a time from the registrar key;
  - wait for the commit's receipt and `minCommitmentAge`;
  - bound RPC responses;
  - read the revert reason from `error.data`;
  - keep the RPC URL out of logged errors;
  - add the modules to the executable;
  - pin simplexmq.

  This plan adds:
  - replacing a stuck transaction with a fee bump after a timeout (gap 3);
  - a request timeout in `rpc`. It calls `httpLbs` with no `responseTimeout`, so a stuck node would otherwise hold a search, reveal or records request for the Manager's default;
  - keeping `error.code` (3 for a revert), which `rpc` drops today. So a revert such as `ownerOf`'s for a name not held is told apart from a node failure;
  - splitting the branch's `sendTransaction` into signing (the raw transaction and its hash, keccak256 of the raw bytes) and sending, so each hash is saved before its transaction is sent.
- **`taken`:** the name is no longer available.
  - A register goes `taken` when it reverts, in its dry run (nothing sent) or mined with `status` 0:
    - with `NameNotAvailable` or `NameReserved` decoded;
    - or with any other revert while `available(label)` is false. The sponsor checks the intent and the allowance before availability, so a taken name can revert with those first.
  - The exception: `ownerOf(labelHash)` is the owner. That means this registration already went through (a resumed worker or a duplicate send). It is `registered`, recording `registered_at` and `expires_at` and spending its prepaid code.
  - The payment or prepaid code is kept, and the outcomes above apply.
- **`failed`:** retried; still failing → "Paid, not registered".
- **Records:** the links chosen at checkout, if any, are set by the register transaction.
  - Its `data` holds one `setText` call for each of `simplex.contact` and `simplex.channel` that has links, on the name's node.
  - The links are joined by `;` (`LINK_SEPARATOR` in `snrc-resolve.py`).
  - Without links the name is registered with none.
- **Funding:** `registerWithCredit` spends the registrar's allowance on the sponsor, so the registration fee is not paid in ETH. The registrar key pays only gas. A gas spike is the service's loss.
- **Key risk:** the registrar key holds gas and spends an allowance. By policy it holds no names.
  - A leaked key can spend the whole allowance:
    - registering to itself needs no signature;
    - registering to any address it controls needs only that address's signature;
    - `renewWithCredit` needs none.
  - So the allowance is kept small, and the guardian zeroes it as the kill switch.
- **Allowance:** only the guardian (`controller.beneficiary()`) sets it, with `setRegistrarAllowance(registrar, allowanceUSD)` in attoUSD. It replaces, not adds.
  - Each registration spends the name's list price for its whole duration (5 years for a 5-year code).
  - `InsufficientAllowance` alerts the operator. While `available(label)` is true it is a `failed`, never `taken`.
- **Redeeming in the app (later):** the service relays the code wallet's `approveWithSig` and the app wallet's `claimWithSig` on the base registrar. An endpoint for it comes with the app's redeem flow.

### 5. Webapps: a shared library, the badges webapp, the names webapp

This stage builds two webapps that share logic, not screens.
- The badges webapp stays in `apps/simplex-badge-service/web/` (`apps/simplex-store-service/web/` after stage 1).
- The names webapp lives in the simplex.domains repo, in that site's look, and is a submodule of simplex-chat.
- The shared logic is a library in `packages/`, published to npm.

**5a. The library: `packages/simplex-chat-store`, published as `@simplex-chat/store`**, as `packages/simplex-chat-webrtc` is published as `@simplex-chat/webrtc`.
- **Moved out of the badges webapp:**
  - From `api.ts`, split at the product:
    - The transport moves: `ApiError`, the wire error codes, the long poll and its backoff, `waitForChange`.
      - `ApiError` gains the error body's other fields, since `decodeError` keeps only `error` today. `catalog_changed` carries the new price, `record_reverted` the revert's name, and a commitment's `bad_request` the open `invoiceId`.
      - `waitForChange` stops on `bad_request`. Today it retries that with backoff, like any non-OK status except 404 (thrown) and 429 (retried after `Retry-After`).
    - The invoice's common fields move: status, amounts, crypto, expiry.
    - Each app passes in its product:
      - `createInvoice` takes the method (which the common part keeps, to check the answer's payment fields against), the product's request body, and a reader for the created invoice's product fields;
      - `parseInvoiceView`, and through it `readInvoice`, `cancelInvoice` and `waitForChange`, take a reader for the view's product fields.
    - The badge request (`CreateRequest`) and the badge fields (`badgeType`, `months`) stay in the badges webapp, so `parseCreatedInvoice` no longer requires them.
    - The readers it uses (`parse.ts`) move whole.
  - From `domain.ts`: the order statuses, methods and chains, and the invoice's server state (`OrderServerState`). The steps, the session and the order's badge fields stay.
  - From `order.ts`: the payment progress (`PaymentProgress`, `orderPhase`). History rows stay, because `OrderRecord` is badge-shaped. `selectionFromOrder` and `viewFor` stay, because they import the catalog and routing.
  - From `store.ts`: the guarded reads and writes, now private methods of the badge `Store` (`read`, `write`, `forget`), extracted over `StorageLike`, with the in-memory fallback `memoryOver` from `main.ts`. The badge session and order shapes stay in `Store`.
  - Code parsing and check characters (`codes.ts`), gaining owner codes and prepaid codes, as the core sum type.
  - The QR encoder from `qr.ts`: `encodeQr`, `paymentUri`, the `QrSymbol` type, and the steps its tests check (`generatorPoly`, `eccOf`, `rawDataModules`, `numDataCodewords`, `byteCapacity`, `alignmentPositions`, `dataCodewords`, `interleave`, `formatBits`, `versionBits`). `qrSvg` builds DOM nodes and stays in each webapp.
  - From `crowdfunding.ts`: the Wefunder link, taking its `utm_source` (`badges` today, `domains` as simplex.domains links it), and `dollars`. The perks are keyed by badge type and stay. The names app has its own placeholder amount.
- **New for names:**
  - `api.ts` gains `checkName`, `checkCode`, `previewLink`, name orders, the reveal, `find`, the retarget (`/name`), the taken code (`/code`), `/retry`, `records`, registration views with `waitForRegistration`, and the new errors;
  - `catalog_changed` carries the new price.
- **Shape:**
  - ES modules compiled by `tsc` with the webapp's strictness, but with its own `tsconfig.json`:
    - `lib` without `DOM`, so the compiler enforces no DOM. `fetch`, `Response`, `AbortSignal` and `crypto.subtle` are typed by `@types/node`.
    - `declaration` on, shipping `dist/*.js` and `.d.ts`.
    - `"exports": {"./*": "./dist/*"}` in its `package.json`. The modules are in `dist/`, and under the webapp's `moduleResolution: nodenext` a subpath like `@simplex-chat/store/<module>.js` without an exports map resolves from the package root.
  - No styles, no runtime dependencies.
  - Its own node:test suite: the code vectors, and the tests split out of the webapp's (below). The commitment and its vectors are in the names app's `names.ts`, not here.
- **Publishing:** from CI on a tag. Consumers outside this repo pin an exact version (`--save-exact`, lockfile integrity).

**5b. The badges webapp, built on the library:**
- **Dependency:** `package.json` gains `"@simplex-chat/store": "file:../../../packages/simplex-chat-store"`, and `npm run build` builds the library first.
  - `web/README.md`'s rule (no dependencies beyond `typescript` and `@types/node`) gains this one in-repo exception.
  - The badges webapp always uses the library from this repo, never from npm.
- **Imports:** modules import `@simplex-chat/store/<module>.js`, and `tsc` checks them against the library's types.
- **Specifiers are rewritten at build:** `build.js` copies the library's `dist/*.js` into `assets/<hash>/store/`, and rewrites the bare specifiers to `./store/` in the served modules.
  - The page has no bundler.
  - Its CSP allows no inline script, so an import map would need a hash or a nonce.
  - The library's files enter the build hash and are added to `sw.js`'s hand-written `PRECACHE`. `test/sw.test.ts` and `test/build.test.ts` ("what is hashed is what is served") build their expected lists from `src/*.ts`, so both gain the library's files and catch an omission.
- **Tests:**
  - `codes.test.ts` moves whole, with the five-line `timedTest` helper it imports from `boot.ts`.
  - `qr.test.ts`'s encoder cases move. The `qrSvg` cases stay with their stub DOM, and so do the payment-screen and code-screen cases, which import `screens.js` and `order.js`.
  - The parser and long-poll tests leave `test/flow.test.ts`, and its `createInvoice` cases are rewritten, since they check the badge body.
    - The `e2e:` tests that start `mock/server.py` stay.
    - The `Flow` tests in `flow-controller.test.ts` stay. Of their imports, only `api`'s transport, `orderPhase` and `StorageLike` come from the library. Its `readInvoice` parsing, 404, 429 and `realSleep` cases move to the library's tests.
  - The guarded storage and `memoryOver` get new tests in the library. Today they are reached only through the badge `Store` (`store.test.ts`) and by booting `main.ts`.
  - The webapp's remaining tests import from the library.
- **Docker:**
  - `Dockerfile.dockerignore` starts from `*`, so it gains `!packages`, `packages/*` and `!packages/simplex-chat-store`, one level at a time as the file adds paths back.
  - The web stage copies the library into a layout that mirrors the repo paths (today it works in `/web`), so the `file:` path resolves, and runs `npm ci` there.
- **Views:** `screens.ts` gains the link to simplex.domains, with `target: "_blank"` like the Wefunder link. The app supports running inside an iframe on simplex.chat (drafted on `sh/website-iframe`; simplex.chat/badges redirects today). The badges screens are otherwise unchanged.

**5c. The names webapp, in the simplex.domains repo**, replacing the countdown page at launch:
- **One order at a time:** as the badges webapp refuses a second order then (it shows no Pay button), a submitted card payment awaiting confirmation turns checkout's Continue off (N3e) until it settles or expires. An unpaid open invoice only shows a link to that order.
- **Build:** a TypeScript build emitting a static site (`index.html`, `assets/<hash>/`, `sw.js`), the same way the badges webapp builds.
  - It depends on `@simplex-chat/store` at a pinned version from npm, and on the vendored `@noble/secp256k1` and `@noble/hashes`.
  - It copies the library into its assets and rewrites the specifiers, as 5b does.
- **Crypto** (`wallet.ts`):
  1. 128-bit entropy from `crypto.getRandomValues`, formatted and parsed as the owner code by the library's `codes.ts` (SHA-256 checksum, WebCrypto);
  2. the BIP-39 phrase with that entropy, and its PBKDF2-SHA512 seed (WebCrypto);
  3. BIP-32 to `m/44'/60'/0'/0/0` (HMAC-SHA512 from WebCrypto, secp256k1 from `@noble/secp256k1`);
  4. the EIP-55 address (keccak from `@noble/hashes`);
  5. the commitment secret from the seed, and signatures over the `RegisterName` intent, `SetText` and `ClearRecords` record edits, and the EIP-191 `find`, `name` and `code` messages; the sealed label and links.

  Tests pin:
  - the `abandon … about` vector against #7475's address for account 0;
  - the branch's owner-code vectors (`BadgeTests.hs`);
  - simplexmq's BIP-44 address vectors (`tests/CoreTests/EthCryptoTests.hs` on `names`).
- **`names.ts`** (pure): `validateLabel`, keccak of the label, the `Registration` ABI encoding with the records' `setText` data, `makeCommitment`, and the `RegisterName`, `SetText` and `ClearRecords` digests.
  - Its vectors:
    - the commitment and calldata from the branch's `NameRegistrationTests.hs`, computed there with viem;
    - the `RegisterName`, `SetText` and `ClearRecords` digests, computed with viem and accepted by PR #35's `SimplexSponsor` and `SimplexResolver`, as `e2e/StoreE2E.test.ts` signs them.
  - The branch's `registerWithCredit` calldata vectors pin that digest indirectly, through the viem signatures they embed.
  - simplexmq's `EthCryptoTests.hs` on `ab/name-registration-primitives-sponsored` still hashes the type without the nonce, under a `SimpleX`/`2` domain.
- **Views**, from `plans/names-codes/mockups/domains.js` and `domains.css`, using simplex.domains' fonts, logos and brand blue with the mockups' palette, light and dark:
  - the header, the search and its results, the code popup;
  - checkout with links and "I have a code", and the owner code before paying;
  - the payment pages and their endings;
  - on the registered page, "Show it as 12 words, for another wallet": the owner code's BIP-39 phrase, for importing elsewhere;
  - the registration track and the registered page;
  - restore and the outcomes;
  - history and the link editor.
- **Wiring:** routes `/`, `#/checkout`, `#/names`, `#/?name=&owner=` (opened from the app), `?order=<id>` and `?registration=<id>`.
- **Order errors on the page:**
  - `name_taken` goes back to search and shows its new answer (N2a or N2b, or N2c for a reserved name).
  - Code errors from the service show where the code was entered: the popup (K5) or the checkout entry (K7). A typo is caught on the page (K6).
  - A `bad_request` carrying an `invoiceId` opens that order.
  - Anything else at checkout is N3d. After the order exists, the order page shows the error with the reference, and the payment is kept.
- **The owner code:** stored only in this browser. It is shown before paying, on the registered page and in history, and never sent anywhere. Its QR is for the app to scan.
- **The Stripe publishable key:** injected into the shell at export, as for badges (stage 6).
- **Tests:** node:test, as the badges webapp has them.

**5d. simplex.domains as a submodule of simplex-chat, at `apps/simplex-domains`:**
- **CI:** simplex-chat's CI builds and tests the submodule against the library in this repo, overriding the pinned version with the `file:` path. A library change that breaks names fails here before it is published.
- **Docker:** `Dockerfile.dockerignore` gains `!apps/simplex-domains`, and the Dockerfile's web stage builds it next to the badges webapp, so one image carries both sites.
- **Releases:** a names release reaches production by bumping the submodule pointer. Builds check it out (`git submodule update --init`).

### 6. Deployment (`scripts/badge-service/`, renamed in stage 1)

This stage serves both sites from the existing production deployment.

Production is the split deployment in `scripts/badge-service/`. Today:
- Caddy serves the one folder the service exports at boot (`compose.yml` mounts `./web` as `/srv/webapp`, the ini's `webapp_export_dir`), and proxies `/api` and `/webhooks` on badges.simplex.chat.
- The service runs with `+client_postgres` and host networking, with its ini mounted read-only.

Changes:
- **Two exported sites:**
  - `[listener]` gains `names_static_dir` (the built names site in the image, `/srv/names`) and `names_export_dir`, beside `static_dir` and `webapp_export_dir`.
  - `exportWebapp` writes both, with the Stripe publishable key in each shell.
  - `compose.yml` mounts the second folder.
- **Caddy:** one Caddy serves both sites.
  - `badges.simplex.chat` serves the badges folder; `simplex.domains` serves the names folder.
  - Both proxy `/api/*` to the service, so the names site calls the API on its own origin, with no CORS.
  - `/webhooks/*` (`/webhooks/btcpay`, `/webhooks/stripe`) stays on badges.simplex.chat only, as today. BTCPay and Stripe send every event there, names payments included.
  - `apps/simplex-badge-service/README.md` (renamed in stage 1) gains the host blocks, since the Caddyfile is not in this repository.
  - The names site's CSP follows the badges one in `web/README.md`, with `data:` added to its `img-src` (`'self' data: https://*.stripe.com`) for the link profiles' pictures.
  - The CSP allows no inline styles. So the views port the mockups' `style` attributes to classes, or set them through `node.style` (the avatar hue).
- **All-in-one mode** (`serve_webapp = on`) picks the site by `Host` against `[listener] names_hosts`. `contentTypeFor` gains `font/ttf` for simplex.domains' Manrope `.ttf` files, or the names build converts them to `.woff2`.
- **Chain:** the resolver and reth on loopback when they share the host. The service's key is in the mounted ini, which `Dockerfile.dockerignore` already keeps out of the image.
- **Unchanged:** the database `badge_service` and the `sx_badge_service_` prefix.

### 7. Mock (`web/mock/server.py`)

This stage extends the mock server so both webapps can be tested without the service.

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

What each stage waits for, and what to agree with others first.

- **Chain dependence:**
  - Stages 1, 2 and 5–7 do not depend on the chain.
  - Stage 3 reads the chain:
    - search's records nonce and raw records;
    - the reveal's sponsor nonce;
    - `/retry`'s `ownerOf`, `nameExpires`, register receipts and sponsor nonce;
    - the registration view's `intentNonce` and `recordsNonce`;
    - the records relay's latest block time, `owner(node)`, nonce, and, after a revert, expiry or lost receipt, the record values.
  - Stage 3 also sends to the chain (the records relay). So it is built and tested against the fake resolver and a fake JSON-RPC node.
  - Its relay sends through stage 4's one sender, so the relay ships with stage 4.
- **simplexmq dependence:**
  - These wait for simplexmq `names` (#1843, #1887's signing) and for the EIP-712 hashing and `recoverAddress` of `ab/name-registration-primitives-sponsored` to reach the simplexmq the service builds against:
    - stage 2 (on Alain's branch, whose owner code uses simplexmq's BIP-39 from #1843, and whose `Registration` encoding uses its EIP-712);
    - stage 3's relay, reveal intent check, and signed `find`, `/name` and `/code`;
    - stage 4.
  - Neither this branch's pin, `e24d537e`, nor master's, `cba13823`, has them.
  - A paid order waits in `queued` until stage 4 lands.
- **Codes differ from #7530 §6/§9:** the `SN7-` format without years in the text, their own table, none sold on the web, and the service's table rather than the text deciding what a code covers. So the app cannot check one offline. Agree with #7530 before stage 2.
- **With Alain:** agree the `NameCode` sum type (his `NameCode` becomes `NameOwnerCode`), land his primitives branch with a pinned simplexmq, and do the StoreService rename after it, since the rename moves his `BadgeService/Names/` modules.
- **Stage 5 in order:** the library (5a) and the badges webapp on it (5b) first, then a published release the names webapp (5c) pins, then the submodule (5d).
- **The app side:** redeeming an owner code in the app (the service relaying `approveWithSig` and `claimWithSig`) is needed before a web-bought name moves to the app.
  - A BIP-44 wallet importing the code's BIP-39 phrase holds the name and can edit its records, paying gas.
  - Moving it still needs the recipient to claim it, since PR #35's `transferFrom` reverts `RecipientMustBeCaller` unless the recipient or a registrar controller calls.

## Out of scope

- The app side: redeeming owner codes and prepaid codes in the app (#7530), record editing in the app (later app work).
- Funding the service's key (operations).
- Renewals, transfers, in-app purchase.
- Refunds (manual, by support).
- Selling codes on the web.

## Verification

How each part is checked before release: Haskell tests, web tests, the Docker build, and end-to-end runs on the mock.

- **Haskell:** `cabal build exe:simplex-store-service` and the service and core test filters, on SQLite and Postgres. Covers:
  - name code vectors and `validLabel` (a `--` anywhere refused, as simplexmq parses it);
  - the migration up/down;
  - `/api/name` against a fake resolver, fake link profiles and a fake JSON-RPC node:
    - nonces and the two text records on the default resolver contract;
    - a name on another resolver contract or none, answered with `records` and `recordsNonce` null;
  - `/api/code/check`;
  - orders, the reveal and the records relay:
    - orders by payment and by code;
    - the reveal and its checks: with and without links, a high-s intent refused, the sponsor nonce from a fake JSON-RPC node;
    - link previews;
    - the records relay against a fake JSON-RPC node;
    - a second order for a commitment with an open invoice, refused with its id;
    - `/api/name/register` for a commitment with an open invoice, refused, and for one with a paid registration, answered with its `registrationId` and `invoiceId`;
    - a resend of the same order, answered with the existing invoice;
    - Get a new invoice after the card form failed to load, cancelling the old one first (and re-reading on `not_open`);
    - an order for a commitment with a registration past `unpaid`, answered with that registration's order (a code one with `invoiceId` null), checked before the resolver so a resend after its own registration is not `name_taken`;
    - a cancel answering `200` `paid`, or `200` `expired` with payment marks, re-read; and `200` `open` shown as `CANCEL_STILL_OPEN`;
    - two orders for one name (the later one taken);
  - closed invoices:
    - a late payment of a closed name invoice leaving its registration `unpaid`;
    - the 30-day cleanup deleting an `unpaid` registration, but sparing one whose invoice received any payment;
    - the cleanup recording and alerting a payment the provider reports only at cleanup, and keeping the registration when the provider does not answer;
  - `name_record_edits`:
    - a wrong signer, a high-s signature or a passed deadline refused before any row or gas;
    - a pending edit resumed after a restart: sent, its saved raw transaction rebroadcast, or, its registrar nonce used with no receipt, checked on chain by its nonce and the record;
    - one past its deadline freed, its transaction still rebroadcast until its registrar nonce is used;
    - an `InvalidNonce` revert of an edit another relayer applied, and its identical repeat, answered as mined with `{tx: null}`;
    - a `SignatureExpired` revert of an edit another relayer applied, answered as mined with `{tx: null}`;
    - a restart-expired pending edit found applied becoming `mined`, not `reverted`;
    - an identical repeat after a restart;
    - a concurrent identical twin losing the unique insert before any signing;
    - a reverted edit's repeat answered `record_reverted` while a new edit with its nonce is pending;
    - a revert freeing the nonce for a new body;
    - a failed re-read after a dry-run or mined revert, retried with the edit kept `pending`;
    - the per-name cap;
  - sizes:
    - the link bound at preview, reveal and records;
    - `sealed` over 11095 characters refused at `/api/invoice`, `/api/name/register` and `/name`;
    - a longest order and a reveal with two records at the link bound passing the body cap;
  - `find`, `/name` and `/code` with their owner signatures and 10-minute window (a wrong signer and a stale timestamp refused), and `/retry` (re-reserving a released code, the lost-confirmation path);
  - the worker against a fake JSON-RPC node:
    - each phase change waking a long poll held on a paid invoice with `seenPhase`, and one held on `GET /api/registration/:id?seenPhase`;
    - an unpaid name invoice held with `seenPhase=unpaid` waking on its expiry, and one paid after it closed answered at once and not polled again;
    - commit, wait, register;
    - a revert to taken;
    - a restart mid-phase;
    - a commit reverting `UnexpiredCommitmentExists` → `committed`;
    - a commit out of gas sent again;
    - a commit reverting at the commitment's lapse second, sent again a second later;
    - a pending commit waited for;
    - a fee-bumped replacement mined under a new hash;
    - a restart rebroadcasting a saved raw transaction;
    - a dropped one whose nonce was used, checked on chain before signing again;
  - `/retry`:
    - outside `failed`, answering `bad_request`;
    - and in each step:
      - a consumed commitment after a redeem (a mined `register_tx` → `registered`);
      - a released code now used elsewhere (the code error, still `failed`, `failed_at` unchanged);
      - a moved sponsor nonce (the reveal cleared);
      - a live commitment (straight to `committed`);
      - a live re-commit by a third party, its time not that of any commit of ours (the reveal and the commit's hashes cleared, the view's `commitTx` null afterwards);
      - our own live commit after a moved sponsor nonce (its hashes kept);
      - a live commitment with less than `revealMargin` left, not reused but committed again a second after its `revealBefore`;
      - a requeue clearing `failed_at`, `error` and the register's hashes (the view's `registerTx` null afterwards);
      - a lapse to `failed`;
      - the second's wait at `revealBefore` before committing again;
  - badges unchanged, wire tags pinned.
- **Web:** the library's tests; the badges webapp's build (hash committed) and tests on the library from this repo; the names submodule's build and tests on the same library, including the wallet vectors.
- **Docker:** `docker compose build` with the submodule checked out, and a run against local Postgres that exports both sites.
- **End to end on the mock with Playwright:**
  1. search (Enter, button) → available → the owner code, confirmed as saved → Bitcoin → steps → registered
  2. taken, reserved, invalid
  3. "Check your code" → $0 → register with code → registered for the code's years
  4. "I have a code" at checkout, and codes that do not work
  5. links at checkout → the name points at them; Edit links
  6. storage cleared after paying → the order link asks for the owner code and finishes; Restore with a name code from Your names
  7. taken during registration → another name, and → a code
  8. failed → Try again
  9. card
  10. opened from the app
  11. history with "Add a code"
  12. both stores' cross-links

  Light, dark and phone, against the stage-0 board.
