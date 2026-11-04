# SimpleX names in the apps: delivery plan

Base: `names`. Repos: simplex-chat, simplexmq.

What a user does in the mobile and desktop apps once this is built: search a name, pay for it in the app store or with a code, own it, point it at an address or a channel, back up the key that owns it, and bring in a name bought on simplex.domains.

The sale's shape, the screens, their words, the store rules and the threat model are decided in `plans/2026-10-30-names-in-app-purchases.md` (#7951). This plan takes those as given and covers what is left to the names work: the wallet's signing, restore and scan, the shared registration encoder, the service's SMP surface, and the Kotlin apps. The disposition of the earlier prototypes is in §4.

## Contents

1. What exists
2. What is missing
3. Decisions
4. Disposition of earlier pull requests
5. The work
6. Order
7. Tests
8. Open questions

## 1. What exists

**Chain and web.** `.testing` is deployed, and `.simplex` is deployed next. The store service sells names over HTTP to simplex.domains: prepaid codes, card, Bitcoin and Monero; orders signed by the buyer's key; commit, reveal and `registerWithCredit` through the sponsor, with the registrar key paying the gas and an allowance paying the price (#7979, `plans/names-codes/2026-10-21-store-api.md`).

**Core.** The device wallet (#7684): one BIP-39 master per device, one hardened account per name at `m/44'/60'/n'/0/0`, a counter, each account bound to a profile, `/_wallet …`. Name lookup through the resolver, with the warnings and plans of `plans/2026-09-28-name-warnings.md`. A name is claimed on the profile's address with `/_set domain`, and in a channel's own profile for a channel. The registration encoder and the order signatures (#7693) are part of the store-service app.

**simplexmq.** BIP-32, BIP-39, BIP-44, recoverable secp256k1, keccak, EIP-55 addresses, EIP-712 hashing, RLP and EIP-1559 transactions. The resolver answers `RSLV` with the name's record, expiry, grace, reserved reason and, for an available name, the price list and the minimum label length.

**Apps.** Name lookup with the alerts of `plans/sketches/2026-09-18-names-lookup-flows.excalidraw`, setting and verifying a name on an address or a channel, and a countdown to the sale. Register, Renew and Re-register lead to simplex.domains.

**UX canvases.** The entry points, the recovery key, the hidden-profile rule and the progress states are drawn on `plans/sketches/2026-09-18-names-phase3a-registration.excalidraw` and `2026-09-23-names-backup-restore.excalidraw`, tentatively approved. #7951's screens take the purchase steps from the webapp and the rest from these canvases.

## 2. What is missing

| # | Missing | Layer |
|---|---|---|
| M1 | Signing an EIP-191 message or EIP-712 typed data with an account key | core wallet |
| M2 | Restoring a wallet from the 24 words | core wallet |
| M3 | The names an account holds, with each name's expiry | core wallet |
| M4 | Finding the accounts a phrase owns | core wallet, simplexmq |
| M5 | The registration encoder and the order messages, reachable by the core | library |
| M6 | A names surface over SMP | store service, core |
| M7 | A name code bought with a store receipt | store service, core |
| M8 | Relays for the owner code's transfer and for record edits, over SMP | store service, core |
| M9 | An order's life in the core: place, follow, reveal, retry, another name, claim | core |
| M10 | The terms of an available name in the resolver's answer | simplexmq |
| M11 | The screens for the purchase, the codes, the key and the expiry | apps |

## 3. Decisions

**D1. The app orders through the store service over SMP.** The service holds the registrar key, pays the gas and spends the allowance, as it does for the web. The app signs the order, the registration intent and the terms acceptance with its wallet; every transaction is sent by the service. The device's address stays unknown to the service, through private routing.

**D2. The service's name routes serve two transports.** The handlers for the code order, the view, the poll, the reveal, Try again, `find`, another name, the record edit and the code check become functions. Both transports call them, so the page and the app are answered by one implementation.

**D3. The search answer comes from the resolver.** Availability, the price, the minimum length, the grace date, the reserved reason and the terms come from the name lookup the app already makes. The service's catalog answers the sale's state, the store products, the term start and the service's clock. The app compares the catalog's terms hash with the resolver's before it signs.

**D4. The registration encoder is shared.** The ABI encoding, the commitment and the `RegisterName` intent are moved into the chat library, so the commitment the core signs and the calldata the service sends come from one encoder.

**D5. Kotlin before Swift.** One pass over the Kotlin screens covers Android and desktop.

**D6. A code registers a name on every build.** Store purchase is added on Android first, and on iOS behind a build-time switch, as #7951 sets out. Desktop and F-Droid builds accept a code and link to the site.

**D7. Signing is a wallet command.** It takes an account index, and the account key is used only inside the command.

**D8. Renewal is out of this plan.** The expiry date is shown, and reminders precede it. `renewWithCredit` and its store products come in a later run, before the first names expire at the end of 2028.

## 4. Disposition of earlier pull requests

| Pull request | Disposition |
|---|---|
| simplex-chat #7530 minimal name registration API | Closed; the store's order path replaces its declarations, as #7951 slice 0 sets out. Its two canvases are moved to this branch. |
| simplex-chat #7390, #7425 names CLI MVP with codes | Closed. The wallet and the store service replace the mock registrar, the lazy seed, the purchase-key credits and the `/0/k` paths. The CLI command shapes are kept (PR 22). |
| simplex-chat #7355, #7382 names v2 core, TUI and GUIs | Closed. The stealth derivation and the gift flow are left in the RFC for later work; #7951's screens supersede these. |
| simplex-chat #7561, simplexmq #1875 scan for names owned by a wallet | Replaced by PRs 3, 4 and 12. |
| simplex-chat #7566 wallet names model | Replaced by PR 11. |
| simplex-chat #7509 wallet for hidden profiles | Closed; hidden profiles are refused, as the canvases and #7951 state. |
| simplex-chat #7519 stealth transfers RFC | Left open, outside this plan. |
| simplexmq #1942 names-web example | Left open, outside this plan. |

## 5. The work

Each row is one pull request. Size is the diff a reviewer reads, apart from moved lines.

### simplexmq, on `names`

| # | Pull request | Size |
|---|---|---|
| 1 | `eth: the EIP-191 message digest`. `Simplex.Messaging.Eth.EIP191`, moved from the store service's `personalDigest`, with viem vectors. | small |
| 2 | `resolver: the terms of an available name`. The resolver reads the name policy's terms link and the community list, and fills `nameTerms`. | medium |
| 3 | `resolver: the names an address owns`. `/v2/owned-by/<address>`, with its tests. | medium |
| 4 | `smp: ROWN, the names an address owns`. The command, the server, the client, the agent's `ownedSimplexNames`, one account per relay, version 23. | large |

### simplex-chat library, on `names` after #7979

| # | Pull request | Size |
|---|---|---|
| 5 | `names: move the registration encoding into the library`. `Simplex.Chat.Names.Registration`: the ABI encoding, the commitment, the `commit` and `registerWithCredit` calldata, the `RegisterName` intent. The service imports it. | move |
| 6 | `names: move the order messages into the library`. `Simplex.Chat.Names.Order`: the order, `find`, another-name and spend messages, and the signature format. | move |
| 7 | `names: the name codes in the library`. `NameCode` with both kinds, its hash, and the constructor deriving a code from a seed and a store reference, with vectors. | small |

### Core wallet

| # | Pull request | Size |
|---|---|---|
| 8 | `wallet: sign a message with an account`. `/_wallet sign message <userId> <n> <text>`, EIP-191, 65 bytes with a low `s`. | small |
| 9 | `wallet: sign typed data with an account`. The `RegisterName` intent and the record-edit intent, EIP-712. | small |
| 10 | `wallet: restore from the recovery phrase`. `/_wallet import`, refused where a wallet exists, the counter set past the accounts found. | medium |
| 11 | `wallet: the names an account holds`. The migration, `/_wallet names`, and the expiry refreshed whenever the name is verified. | medium |
| 12 | `wallet: find the names a phrase owns`. The scan over ROWN, with a gap limit, a start index, progress events and a stop. | large |

### Core names

| # | Pull request | Size |
|---|---|---|
| 13 | `names: the store service's name commands`. The request and response types and the version bump; types only. | medium |
| 14 | `names: the catalog and the terms text`. `/_name catalog`, the clock offset, and the terms text checked against its link and its hash. | medium |
| 15 | `names: order a name with a prepaid code`. The order record and its migration, the secret, the commitment, the signed order, `/_name order`. | large |
| 16 | `names: follow an order`. The worker, the poll, the reveal with the terms acceptance, progress events, and recovery after a restart. | large |
| 17 | `names: Try again, another name, and the kept payment`. | medium |
| 18 | `names: claim a registered name`. The address and the channel claim it once the name is registered. | small |
| 19 | `names: pay with a store receipt`. The purchase record, the code derived from the receipt, the recognising token, and the answers to a used or a revoked receipt. | large |
| 20 | `names: redeem an owner code`. The two relayed signatures, the move, and the records pointed at this profile. | large |
| 21 | `names: set what a name opens`. The relayed record edit, and the edits left this year. | medium |
| 22 | `names: the CLI`. `/name …` for the active user over the `/_name …` forms. | medium |

### Store service

| # | Pull request | Size |
|---|---|---|
| 23 | `store: names over SMP — the catalog and the code check`. The dispatch, the per-command bound, the version. | medium |
| 24 | `store: names over SMP — the code order, the view and the poll`. | medium |
| 25 | `store: names over SMP — the reveal, Try again, find and another name`. | medium |
| 26 | `store: names over SMP — the record edit`. | small |
| 27 | `store: names over SMP — the owner code's approve and claim`. | medium |
| 28 | `store: a name code bought in a store`. The names catalog of products, the purchase command, the payment column, the presentations table and the daily cap. | large |
| 29 | `store: the operator command for a store payment`. Show, revoke, refund, move. | medium |

### Apps, Kotlin (Android and desktop)

| # | Pull request | Size |
|---|---|---|
| 30 | `ui: the names hub`. The Settings row, the names owned, the entry points of canvas 1a to 1e, and the recovery-key reminder. | medium |
| 31 | `ui: search a name`. The field, the checks made while typing, and the answer's states. | medium |
| 32 | `ui: what the name opens`. The address or channel row, Skip for now, and Set what it opens. | medium |
| 33 | `ui: Check your order`. The card, the term, the terms line, the code field and the store row. | medium |
| 34 | `ui: registering, done, taken and failed`. One screen with its states. | large |
| 35 | `ui: Redeem code, and Redeem a name`. The field, the scanner, the verified link back, and the move's progress. | large |
| 36 | `ui: the recovery key`. Back up, reveal and confirm, enter the 24 words, and the scan with its progress. | large |
| 37 | `ui: expiry`. The date on the row, the banner, and the reminders per profile. | medium |
| 38 | `android: the Play purchase lane`. The products, the sheet, pending purchases, Restore purchases, acknowledge and consume. | medium |

### Apps, Swift

| # | Pull request | Size |
|---|---|---|
| 39–46 | The screens of 30 to 37, in the same order and with the same states. | — |
| 47 | `ios: the App Store purchase lane`, behind the build-time switch. | medium |

## 6. Order

Three streams run at once.

| Stream | Pull requests |
|---|---|
| simplexmq | 1, 2, 3, 4 |
| library and service | 5, 6, 13, 23, 24, 25, 26, 27, 28, 29 |
| core wallet | 7, 8, 9, 10, 11, 12 |

Each pull request and what precedes it:

| Pull request | Preceded by |
|---|---|
| 12 | 4, 10 |
| 14 | 2, 13 |
| 15 | 5, 6, 7, 8, 9, 13, 14, 23, 24 |
| 16 | 11, 15, 24, 25 |
| 17 | 16, 25 |
| 18 | 16 |
| 19 | 7, 16, 28 |
| 20 | 9, 27 |
| 21 | 9, 26 |
| 22 | 15, 16, 17, 18, 19, 20, 21 |
| 30, 31 | 14, 22 |
| 32 | 21, 31 |
| 33 | 19, 32 |
| 34 | 16, 17, 33 |
| 35 | 20 |
| 36 | 10, 12 |
| 37 | 11 |
| 38 | 19, 33 |
| 39–47 | the Kotlin pull request each mirrors |

The first end-to-end result follows 1, 5, 6, 7, 8, 9, 13, 23, 24, 15, 16, 18 and 22: a CLI user with a prepaid code registers a name, and the address claims it.

## 7. Tests

- **Vectors**, against viem and the deployed ABIs: the commitment, the `commit` and `registerWithCredit` calldata, the `RegisterName` digest, the EIP-191 order digest, the derived code and the recognising token.
- **Core against the service**, with the fake chain node and the fake stores: a code order through to registered and claimed; a store receipt through to registered; the same receipt twice; a receipt whose answer was lost; a name taken between the search and the order; a failure and Try again; another name; a restart in each phase; hidden profiles refused.
- **Wallet**: the phrase restored; the counter set past the accounts found; the scan's gap limit and its start index; a signature against its vector.
- **Apps**: previews of each state named in #7951 §6.5, in light and dark, at the smallest and the largest widths and the largest text size, with the shortest and the longest name; the Android lane against the fake verifier.
- **Regeneration**: the schema dump, the query plans and the API docs.

## 8. Open questions

Each has a recommended answer.

1. **Kotlin or Swift first.** Recommended: Kotlin, which covers Android and desktop in one pass. #7951 recommends Swift first.
2. **Whether the CLI (PR 22) ships.** Recommended: yes, so that the apps' commands are exercised before a screen exists.
3. **Whether the scan (PRs 3, 4, 12) is in the first release.** Recommended: yes. A restored device registering a name on an account that already owns one is how a name is lost.
4. **Whether desktop keeps the browser link once the apps sell.** Recommended: yes, since a desktop build cannot sell in a store.
5. **Where this plan lives.** Recommended: beside #7951, which keeps its screens and store rules.
