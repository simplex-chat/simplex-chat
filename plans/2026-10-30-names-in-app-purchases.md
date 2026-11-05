# Selling SimpleX names in the apps

Base: `names` · Repos: simplex-chat, simplexmq, and simplex.domains for the site's part. Builds on the store service (https://github.com/simplex-chat/simplex-chat/pull/7979, with its terms on chain from https://github.com/simplex-chat/simplex-chat/pull/7889), the device wallet (https://github.com/simplex-chat/simplex-chat/pull/7703) and the terms in the resolver's answer (https://github.com/simplex-chat/simplexmq/pull/2003).

## Summary

What a user does in the mobile and desktop apps once this is built: search a name, pay for it in the app store or with a code, own it, point it at an address or a channel, back up the key that owns it, and bring in a name bought on simplex.domains.

| Stage | What it gives | What it holds |
|---|---|---|
| Stage 1, v1, the beta | A name in the app and on the address or channel | Search over SMP; one store product, $100 for 2 years, for any name; prepaid codes; a site-bought name's code; builds that can't pay open the site with the chosen name; the "SimpleX names" hub; the recovery key, offered right before the first payment or code; terms ticked on each order |
| Stage 2, v1.1, usable | A way back on a new phone, and changing a name | Restore from the 24 words with a scan of the wallet's accounts; a name moved in or restored attaches to the profile's address or channel; changing what a name opens; giving a name to another profile |
| Stage 3, later | What the first names need before they expire on 31 Dec 2028 | Transfers to another person, 3 a year; renewal; reminders |

A store payment, end to end:

1. The app sets a token only its wallet recognises on the purchase and opens the store's sheet.
2. The app derives a prepaid code from its wallet's seed and the store's reference of the purchase, and signs a sealed order for the chosen name, paid with that code.
3. The app sends the store receipt, the code's hash and the sealed order in one request to the store service, over SMP.
4. The service checks the receipt, saves the paid code, and only then looks at the order. It answers the two separately, so a refused order keeps the payment.
5. After at least a minute, the app reveals the name in its own request. The service registers it to the wallet's account, and the address or channel claims it.

Section 5 lists the decisions, section 8 cuts the work into pull requests by stage, and section 11 lists what the lead still decides.

## 1. Goal, stages and acceptance criteria

The goal: a person in a store build registers a SimpleX name for their address or channel, paying with the store, without a wallet, ether or a browser; and every build has a working way to get a name, or to bring in one bought on the site.

### Stage 1, v1, the beta

- Search in the app, over SMP.
- Buy in the app with Google Play and the App Store: one product, $100 for 2 years, for any name. iOS store payment is built in this stage and ships hidden in a normal release until it is tested.
- After a store payment, the app sends the payment and the sealed order in one request. The service saves the paid code before it looks at the order and answers the two separately. The reveal is its own later request.
- A prepaid code registers a name.
- A site-bought name's code moves the name into the app only if the name already opens this profile's address or channel. Or the name shows on the profile without moving, as today.
- Builds that can't pay in the app (F-Droid, desktop, and iOS while its store payment is hidden) search in the app, then open the site with the chosen name and the link it should open, and pay there. Google Play and iOS show no link to the site outside the US.
- "SimpleX names", the hub, replaces today's name screen: the profile's names, Register a name, Import, Restore purchases and the recovery key, opened from the address, a channel's info and Advanced settings.
- A name can be registered opening nothing, after a warning. Its links can be set from stage 2.
- The 24-word recovery key is set up right before the first payment or code, never before a name is found. Backing up now shows it hidden until Reveal. Backing up later leaves it to the hub's key row, with one reminder after the first name.
- The general terms are ticked on each order. A community name's terms are on their own step before the order.

### Stage 2, v1.1, usable

- Restore on a new phone from the 24 words: the app asks what each of the wallet's accounts owns, until 20 unused accounts in a row.
- A name moved in or restored attaches to the profile's address or channel, with one link edit SimpleX pays for.
- Changing what a name opens, and giving a name to another profile.
- Moving in a name that opens another profile's address.

### Stage 3, later

- Transfers to another person, with the allowance of moves: 3 a year per name.
- Renewal ("add a year") and reminders, built and released before mid-2028.

### Acceptance criteria

Stage 1:

- AC1: In the Android Play build, a user with no wallet finds a name, pays once with Play Billing, and ends with the name registered for two years to a key of the device wallet, opening their address, which shows it as verified. Test: end to end with the store service, the fake Play verifier and the fake chain node.
- AC2: A payment is never credited twice and never lost: the same request sent twice gives one code and one order; a request whose answer was lost is sent again and gets the same answers; a refused order keeps the payment; a name taken before the order or before the registration keeps the payment for another name. Tests: service and core tests for each case.
- AC3: The one product pays for any name of 6 to 63 characters for 2 years, and the product, never the client, decides the years. Test: a service test with a 6-character and a 63-character name.
- AC4: With iOS store payment hidden, the iOS build shows no purchase button; the US storefront shows the site link, and every storefront takes a code. The purchase lane compiles and runs behind the build-time switch. Test: previews of both storefronts, and a sandbox run of the lane with the switch on.
- AC5: A name bought on the site reaches the app: its owner code, typed, scanned or arriving by link, moves the name to the device wallet once the name opens this profile's address or channel; a name that doesn't is refused with its next step; a name left on the site shows on the profile as today. Test: end to end with the owner code flow and the fake chain node.
- AC6: The app never opens the store sheet while the service is in codes-only mode, or closed for the day, or before the sale, and a receipt that was paid is never refused for a mode: its order waits until the sale opens. Test: the catalog's states in previews, and a service test crediting a paid receipt in codes-only and answering its order `sale_closed`.
- AC7: A build that can't pay searches in the app, and "Buy in browser" opens the site with the chosen name and the link. Google Play and iOS outside the US show no link to the site. Test: previews per build and storefront.
- AC8: No key material exists before the user chooses to continue from the recovery key screen, which appears only after a name is found and the user pays or uses a code. Back up later skips the words, and the key row and the one reminder carry the backup. Test: a core test that search, Check your order and a cancelled sheet create no wallet; previews.
- AC9: Each order signs the terms the user ticked for it, and a community name's order signs its community terms after their own step. Test: service and core tests for changed terms.

Stage 2:

- AC10: On a new phone, the 24 words bring back every name the wallet's accounts own, found by the scan with its gap of 20, attached to this profile's address or channel by one relayed edit. Test: end to end with the fake chain node and ROWN.
- AC11: "Set what it opens" changes a name's records within its yearly allowance, and a name can be given to another profile. Test: end to end with the record edit relay.

Stage 3:

- AC12: A transfer to another person and a store-paid renewal reach the chain within their rules (section 6.11). Test: end to end. Reminders follow section 6.11. Test: previews and a core test.

## 2. Non-goals

- Subscriptions of any kind.
- Prices by name length in the app.
- Paying with cryptocurrency inside a store build.
- Changing the contracts.
- Selling codes on the web.
- Following an order placed on the site inside the app: the site finishes its own orders.
- Blind-signed codes (section 7).
- Stealth transfers (https://github.com/simplex-chat/simplex-chat/pull/7519 stays open, outside this plan).

## 3. Current state

Facts below were read at these commits: simplex-chat `names` `b4c177fb5`; the store service `sh/names-deploy` `720b5b41b`; the store spec `sh/names-design` `158f22c7e`; simplexmq `names` `d75f5686d`, the pin in `cabal.project`; the site, simplex-chat/simplex.domains `sh/names-webapp` `5228f9d6f`. Paths under `StoreService/` are in `apps/simplex-store-service/src/` at `sh/names-deploy`, and paths under `webapp/` are in simplex.domains.

The contract source is not in a public branch: ens-contracts `simplex` is not on simplex-chat/ens-contracts. Contract facts below come from the service's code and the store spec; the renewal rules in section 6.11 are the names work's reading of `contracts/simplex/SimplexController.sol` and `SimplexSponsor.sol`.

### 3.1 What a name is

- A record in an ENS fork on Ethereum; `.testing` and `.simplex` are separate deployments of the same contracts.
- Only the owner key can change or transfer a name. The service registers a name to the buyer's address through the sponsor's `registerWithCredit`, which needs the owner's signed intent; the registrar key pays the gas and the sponsor's allowance pays the price, so no ether moves for the buyer.
- At least two years: the controller refuses a registration under 730 days (`StoreService/Config.hs:287-289`, `minNameYears`). A prepaid code carries 2 to 10 years (`StoreService/Group/Command.hs:42`). A community name registers for 2 paid years whatever its code gives (`communityYears`, `StoreService/Names/Community.hs:55-56`).
- Paid years start on 1 January 2027 or at the order, whichever is later (`defaultTermStart`, `Config.hs:299-300`). The days before cost the buyer nothing. An order's duration must match within the slack (`checkDuration`, `StoreService/Names/Orders.hs:270-274`).
- Prices come from the chain. `SimplexPriceOracle` holds cents a year per label length with a base for other lengths, and the service quotes `rentPriceUSD` with a placeholder of the name's length (`StoreService/Names/Contracts.hs:175-182`). Setting one price is a chain operation: `setPrices` with base 5000 cents a year and no per-length entry. Every reader follows: the site's search, the resolver's `NamePricing`, and the allowance spent per registration. The store spec's list of $100, $200 and $400 (`2026-10-21-store-api.md:103`) is replaced by the lead's decision of one price.
- A name is 6 to 63 characters: letters, digits and hyphens (`minNameLength`, `maxNameLength`).
- After expiry, a grace period (`GRACE_PERIOD()` on the registrar, 90 days in the terms draft): the name stays its owner's, can be renewed and can't be transferred. Then anyone can register it.

### 3.2 How a name is bought on the site today

- simplex.domains, served by the store service. One term: the search answer gives `years` and `termStart`. Payment by card, Bitcoin or Monero; Lightning is added by https://github.com/simplex-chat/simplex-chat/pull/8010; a separate dApp takes ETH (`webapp/src/views/checkout.ts:32, 466`). A prepaid code `SN<length>-…` pays too; only the operator issues them.
- The browser makes a fresh wallet for each order. Its secret is the owner code, `SN-` and 30 characters, whose 128 bits are the entropy of a 12-word BIP-39 phrase (`src/Simplex/Chat/Badges/Code.hs`). The name registers to that wallet.
- Every registration accepts the name terms on chain: an order signs the hash of its name kind's terms, the reveal carries the owner's acceptance, the service relays it before the register. `terms_unavailable` and `terms_changed` refuse an order (`StoreService/Names/Orders.hs:283-290`). The terms' text is `docs/NAMES-TERMS.md` in the draft https://github.com/simplex-chat/simplex-chat/pull/8002 (closed 5 November, at `672d13e9e`); the name policy links whichever file is current.
- The search reads `/#/?name=<label>` (`webapp/src/views/search.ts:44`, `views/shell.ts:122`). It takes no link yet. Its "is yours" page tells the buyer to set the name on their address and shows the code (`webapp/src/views/order.ts:1262`).
- Before the full launch the service can run codes-only, refusing paid orders (`nCodesOnly`, `StoreService/Web/Names/Orders.hs:87, 239`).

### 3.3 The store service

- The app reaches the service over SMP with `sendServiceRequestAsync`: one connection per request, with PQ on, whose connection info bounds the request at 11106 bytes (`e2eEncConnInfoLength PQSupportOn`, simplexmq `Agent/Protocol.hs:339-343`), and a timeout the caller can set, 30 s by default (`serviceRequestTimeout`). The commands are badge commands only (`src/Simplex/Chat/StoreService.hs:77-106`). Names are served over HTTP only (`StoreService/Web/Names.hs`).
- Requests run on lanes of one thread each: Play purchases on the purchase lane, everything else on the main lane except due renewals (`requestLane`, `StoreService/Service.hs:564-590`).
- Store receipts are verified for badges: Apple signed transactions against a configured root, Play purchases through the Play Developer API; the product decides what is granted (`storeProduct`, `StoreService/Catalog.hs:146-160`). `payments(provider, provider_ref)` is unique, so one transaction funds one purchase. Play reads are bounded at 300 a minute (`maxPlayPurchaseReads`, `Service.hs:1345`). Purchases made in Apple's test environment are refused unless a dev setting credits them (`creditTestPurchases`).
- Play one-time purchases are acknowledged by the service before crediting, never consumed by it (`StoreService/StoreReceipts/Google.hs:96-100`), acknowledged by the Android app once its core holds the receipt, and consumed by the app when finished (`views/badges/BadgeStore.kt:47-48, 293-295`).
- Prepaid name codes: one row per code hash with the shortest length it covers, its years and a payment state; reserved while a registration holds it, spent when the name lands, released when the name is taken, kept 30 days after a failure for Try again (`StoreService/Names/Codes.hs`, `failedCodeHold`, `Names/Worker.hs:104-105`). An order sends the plain code and the service hashes it (`parseCodeOrder`, `Web/Names/Orders.hs:230-233`).
- An exact resend of an order is found by its hash before its timestamp is checked, so a lost answer is safe however late it is resent (`Web/Names/Orders.hs:209-214`). A signed request's timestamp is within 600 s of the service's clock (`signedRequestSeconds`).
- Another name is allowed only for a failed order or one whose links were refused (`movable`, `Names/Orders.hs:315-319`), at most 3 times (`max_order_moves`). A committed order past `revealBefore` with no reveal reads failed (`registrationAt`, `Names/View.hs:148-156`). Code orders have no cancel command.
- Record edits are relayed one transaction each, within `record_edits_per_year`, 3 by default (`StoreService/Names/Records.hs`, `Config.hs:283`). Name transfers aren't relayed: `approveWithSig` and `claimWithSig` are designed in the store spec, not built.

### 3.4 The core and the apps

- The core on `names` has the device wallet: one 24-word master per device, one hardened account per name at `m/44'/60'/n'/0/0`, each bound to a profile, with `APIGetWallet`, `APICreateWallet`, `APICreateWalletAccount`, `APIExportWalletMnemonic`, `APIDeleteWallet` (`src/Simplex/Chat/Controller.hs:451-455`). It can't sign or import yet (`docs/rfcs/2026-09-10-wallet-keys.md`, known limits 2 and 5).
- The registration encoder and the order signatures are part of the store-service app (https://github.com/simplex-chat/simplex-chat/pull/7693). The core makes and reads prepaid and owner codes (`src/Simplex/Chat/Badges/Code.hs`).
- A name is claimed on the profile's address with `/_set domain` (`APISetUserDomain`, `Commands.hs:1642`), and in a channel's own profile for a channel. Name lookup goes through `resolveNameRecord`; its RSLV is sent as `sendOrProxySMPCommand` (simplexmq `Agent/Client.hs:2018`).
- The apps set, show and verify names, with a countdown to the sale, 12 December 2026 18:00 UTC (iOS `UserAddressView.swift:923-932`; Kotlin `SetSimplexNameView.kt:33, 205`). Name alerts offer Register, Renew and Re-register, opening `https://simplex.domains/#testing`.
- The badge buy pattern: on Android the Play build sells in the app and shows "Buy in browser" only for the Play country "US" (`badgeBrowserAllowed`, `BadgeStore.kt:60-67`), and FOSS and desktop show "Buy in browser" and "Redeem badge code". On iOS store purchases are commented out (`BadgesSupportSimplexView.swift:48-70`) and the US check is `BadgeStore.swift:39-41`. The site returns with `simplexchat:/badge/code/<code>` (`ContentView.swift:508`, Kotlin `ChatListView.kt:816`), which the app confirms before redeeming.

### 3.5 simplexmq

- BIP-32, BIP-39, BIP-44, recoverable secp256k1, keccak, EIP-55 addresses, EIP-712 hashing, RLP and EIP-1559 transactions.
- The resolver answers `RSLV` with the name's record, expiry, grace and reserved reason; for an available name, `NRAvailable {pricing, nameTerms}`, where `NamePricing` holds `registrationPrices`, `basePrice` and `minLabelLength` (`src/Simplex/Messaging/Names/Record.hs:94-114`). The type carries `nameTerms` since https://github.com/simplex-chat/simplexmq/pull/2003; the resolver doesn't fill it yet.
- Finding the names an address owns is the draft https://github.com/simplex-chat/simplexmq/pull/1875.

### 3.6 The sketches and the site's design

- The two canvases are on this branch: `plans/sketches/2026-09-18-names-phase3a-registration.excalidraw` and `plans/sketches/2026-09-23-names-backup-restore.excalidraw`, moved from https://github.com/simplex-chat/simplex-chat/pull/7530 and normalized by `plans/sketches/normalize-excalidraw.py`, with every string unchanged. They are the tentatively approved flow. The screens follow them, and depart only for a good reason, usually one learned building the site. Section 6.7 shows each difference with its reason.
- The site's design: `plans/names-codes/2026-10-19-webapp-redesign.md` on `sh/names-design`, built in `webapp/` on `sh/names-webapp`. The app's badge purchase already follows the badge webapp's steps and words in the app's own components (`BadgesCheckOrderView.swift`, `BadgesHowLongView.swift`); the name screens do the same.

### 3.7 Pull requests at this plan's entry points, and earlier work

| Pull request | Relation |
|---|---|
| https://github.com/simplex-chat/simplex-chat/pull/7979 the store service selling names, base `names` | Extended: the SMP commands and the name purchase (section 6.3) |
| https://github.com/simplex-chat/simplex-chat/pull/7889 names under the name terms, merged into `sh/names-deploy` | Followed |
| https://github.com/simplex-chat/simplex-chat/pull/7675 store spec | Followed: codes, terms; its price list is replaced (3.1) |
| https://github.com/simplex-chat/simplex-chat/pull/8002 names terms of use, closed; https://github.com/simplex-chat/simplex-chat/pull/7994 placeholder texts, closed | The app reads whatever the policy links; its age rule is question Q3, its purchase terms question Q4 |
| https://github.com/simplex-chat/simplex-chat/pull/8010 Lightning | The site's fourth way to pay; nothing in the app |
| https://github.com/simplex-chat/simplex.domains/pull/6 the site | Followed; gains the link parameter and "Open in SimpleX" (section 6.8) |
| https://github.com/simplex-chat/simplex-chat/pull/7703 names feature branch | The wallet the app registers to |
| https://github.com/simplex-chat/simplex-chat/pull/7693 registration primitives, merged into `names` | Moved into the library (P5, P6) |
| https://github.com/simplex-chat/simplex-chat/pull/7932 app test backend, merged into `names` | Used by the app tests |
| https://github.com/simplex-chat/simplex-chat/pull/8026 delivery plan | Merged into this plan |

Earlier pull requests:

| Pull request | Disposition |
|---|---|
| https://github.com/simplex-chat/simplex-chat/pull/7530 minimal name registration API | Closed; the store's order path replaces its declarations. Its two canvases are on this branch. |
| https://github.com/simplex-chat/simplex-chat/pull/7390, https://github.com/simplex-chat/simplex-chat/pull/7425 names CLI MVP with codes | Closed. The wallet and the store service replace the mock registrar, the lazy seed, the purchase-key credits and the `/0/k` paths. The CLI command shapes are kept (D8). |
| https://github.com/simplex-chat/simplex-chat/pull/7428 blind-signed codes | Left out (section 7). |
| https://github.com/simplex-chat/simplex-chat/pull/7355, https://github.com/simplex-chat/simplex-chat/pull/7382 names v2 core, TUI and GUIs | Closed. The stealth derivation and the gift flow stay in the RFC for later work; this plan's screens supersede these. |
| https://github.com/simplex-chat/simplex-chat/pull/7561, https://github.com/simplex-chat/simplexmq/pull/1875 scan for names owned by a wallet | Replaced by P3, P4 and P12, in stage 2. |
| https://github.com/simplex-chat/simplex-chat/pull/7566 wallet names model | Replaced by P11. |
| https://github.com/simplex-chat/simplex-chat/pull/7509 wallet for hidden profiles | Closed; hidden profiles are refused. |
| https://github.com/simplex-chat/simplex-chat/pull/7519 stealth transfers RFC | Left open, outside this plan. |
| https://github.com/simplex-chat/simplexmq/pull/1942 names-web example | Left open, outside this plan. |

## 4. What is missing

| # | Missing | Layer | Stage |
|---|---|---|---|
| M1 | Signing an EIP-191 message or EIP-712 typed data with an account key | core wallet | 1 |
| M2 | Restoring a wallet from the 24 words | core wallet | 2 |
| M3 | The names an account holds, with each name's expiry | core wallet | 1 |
| M4 | Finding the accounts a phrase owns | core wallet, simplexmq | 2 |
| M5 | The registration encoder and the order messages, reachable by the core | library | 1 |
| M6 | A names surface over SMP | store service, core | 1 |
| M7 | A name code bought with a store receipt, in one request with its order | store service, core | 1 |
| M8 | Relays over SMP: a name code's transfer; record edits | store service, core | 1; 2 |
| M9 | An order's life in the core: place, follow, reveal, retry, another name, claim | core | 1 |
| M10 | The terms of an available name in the resolver's answer | simplexmq | 1 |
| M11 | The screens for the search, the purchase, the codes, the hub, the key and the expiry date | apps | 1, 2, 3 |
| M12 | The site's link parameter and "Open in SimpleX", and the verified link back | simplex.domains, website | 1 |
| M13 | One price on chain: `setPrices` with a base and no per-length entry | chain operation | 1 |
| M14 | Renewal and transfers to another person | store service, core, apps | 3 |

## 5. Decisions

The lead decides, and the lead's instructions come first; the decisions of 4 and 5 November are marked with their date.

- **D1. One product, one-time.** One store product for any name: $100 for 2 years, Apple's non-renewing subscription and Play's one-time product (5 November). Nothing renews by itself. Section 6.2 holds the store rules.
- **D2. A paid receipt becomes a prepaid code,** derived from the wallet's seed and the store's reference, and the name registers as with any prepaid code.
- **D3. The payment and the sealed order go in one request** (5 November), answered separately, the payment saved first. The reveal is its own later request.
- **D4. The app orders through the store service over SMP.** The service holds the registrar key, pays the gas and spends the allowance, as it does for the web. The app signs the order, the registration intent and the terms acceptance with its wallet; the service sends every transaction. The device's address stays unknown to the service, through private routing.
- **D5. The service's name routes serve two transports.** The handlers for the code order, the view, the reveal, Try again, `find`, another name, the record edit and the code check become functions. The page and the app are answered by one implementation.
- **D6. The search answer comes from the resolver.** Availability, the minimum length, the grace date, the reserved reason and the terms come from the name lookup the app already makes, so the service never learns what users search. The service's catalog answers the sale's state, the store product, the term start and the clock. The app compares the catalog's terms hash with the resolver's before it signs.
- **D7. The registration encoder is shared.** The ABI encoding, the commitment and the `RegisterName` intent move into the chat library, so the commitment the core signs and the calldata the service sends come from one encoder.
- **D8. The CLI first, then Kotlin, then Swift** (4 November). Each core pull request adds its `/name …` form and a terminal test. One pass over the Kotlin screens covers Android and desktop, and the Swift pass mirrors it.
- **D9. A code registers a name on every build.** Builds that can't pay in the app search in the app, then open the site with the chosen name and pay there (5 November). Desktop's site answers with a name code (4 November).
- **D10. Signing is a wallet command.** It takes an account index, and the account key is used only inside the command.
- **D11. A name code is a wallet, and redeeming one is a transfer** (4 November). Every build imports such a code: the app derives the code's key, and the name moves to an account of the device wallet, the code's wallet offering it with `approveWithSig` and the app's account taking it with `claimWithSig`, both relayed by the service. After the move the recovery key owns the name. In stage 1 the name moves only if it already opens this profile's address or channel (5 November).
- **D12. The recovery key comes after a name is found** (5 November), right before the first payment or code. Backing up is the user's choice at that moment; the hub's key row and one reminder carry it after. Restore comes in stage 2, with its scan (5 November, moving it from the first release).
- **D13. Each order signs the terms it was placed under,** ticked on Check your order; a community name's terms have their own step before it (5 November).
- **D14. The hub replaces today's name screen** and comes in stage 1, as the sketches draw it, under Advanced settings in settings (5 November).
- **D15. A name can be registered opening nothing,** through the sketch's ✕ and its warning.
- **D16. A taken name shows no pictures,** with a separate button for what it is used for (5 November).
- **D17. iOS store payment is built in stage 1 and ships hidden** behind a build-time switch in a normal release, until it is tested (5 November).
- **D18. Links to the site:** always on F-Droid and desktop; in the US only on Google Play and iOS (5 November).
- **D19. Yearly allowances.** SimpleX pays the fee for every registration, move, link edit and renewal, so each name has 3 link edits a year (a default the service can change) and the same for moves in and transfers. A failed registration can try another name up to 3 times. Registrations and renewals have no allowance: the buyer pays for each.
- **D20. Renewal is stage 3.** The expiry date shows from stage 1. Reminders and `renewWithCredit` with its store products come before the first names expire.

### The answers of 4 November

1. The CLI ships, and comes first, then Kotlin, then Swift (D8).
2. The scan ships with restore, which the decisions of 5 November place in stage 2 (P3, P4, P12).
3. A desktop build links to simplex.domains, which answers with a name code (D9, D11).
4. Every build imports a name code, and redeeming one transfers the name from the code's wallet to the device wallet (D11, P20, P34).
5. The delivery plan and this plan are merged into one: this document.

### Ruled out

- **Auto-renewing subscriptions:** one renewing name per store account and product group, and at most a year per period against the two-year minimum.
- **Play's prepaid plans:** one per account and product, at most a year.
- **The site alone on store builds:** outside the US a store user couldn't buy in the app at all. It stays for desktop and F-Droid.
- **Cryptocurrency inside a store build:** Apple forbids apps to use "their own mechanisms to unlock content or functionality, such as … cryptocurrencies and cryptocurrency wallets" (3.1.1); Google requires Play Billing outside the US and its alternative-billing countries.
- **Selling codes on the web:** the store spec rules them off the site (`2026-10-09-name-codes.md` §14).

## 6. Design

### 6.1 New mechanisms, each with the requirement it serves

| Mechanism | Requirement |
|---|---|
| A names catalog in the service: the store product → `NameProduct {minLength = 6, years = 2}` | AC3 |
| A prepaid code derived from the wallet's seed and the store's reference | AC2: a lost answer or a restored wallet recomputes the same code, with no re-keying |
| A token on the purchase only this wallet recognises | Another device or profile on the store account presents only what it can use |
| One service command, purchase a name: a receipt, the code's hash and an optional sealed order, answered in two parts | AC1, AC2, D3 |
| One column, the payment that paid a prepaid code (`payment_id`, unique), and one table of `receipt_used` presentations | AC2: one receipt, one code; the only hashes support may move a code to |
| A row per UTC day counting store-paid orders | The daily cap (6.3) |
| A catalog command and a terms text command | AC6, AC9: orders built by the service's clock, and the terms shown are the terms signed |
| SMP forms of the web's code check, code order, registration view, reveal, Try again, `find` and another name | D4, D5 |
| A lane of its own for name commands | Chain and store reads hold up neither badge requests nor each other's purchases |
| Owner-code relays for `approveWithSig` and `claimWithSig`, with their chain reads and a yearly bound | AC5 |
| Core commands: keep the order, hand over the receipt, read the catalog, order, follow, retry, another name, claim, redeem | AC1, AC2, AC5 |
| The purchase lanes in the apps, after the badge lanes | AC1, AC4 |
| The site's link parameter, its "Open in SimpleX", and a verified link back | AC5, AC7 |

The bridge from a receipt to a registration is the prepaid code. A fourth payment kind would add a case to every phase and to the taken and failed handling. A code the service makes and returns would be lost with a lost answer. A random code re-keyed on a second presentation would let any device on the store account revoke another's unused code. A code derived from the token couldn't serve a promo purchase, which has no token.

**The derived code.** When a receipt arrives, the core derives the code from the wallet's seed and the store's reference of the purchase (Apple's transaction id, the hash of Play's purchase token, the reference the service keys payments by): 19 bytes of HKDF-SHA512 over the BIP-39 seed, salted with the reference, under a `SimpleX…` info string of its own; each byte taken mod 32, then the Luhn character over the length digits and the payload, in the text `SN6-…`. The seed is the input, never an account key, so one account key reveals nothing else. The wallet RFC gains the derivation, simplexmq's `protocol/crypto-registry.md` gains the info strings, and test vectors pin both.

**The recognising token.** Before the sheet opens, the app sets the store's account field (Apple's `appAccountToken`, a UUID; Play's `obfuscatedAccountId`, the same text) to a random 7-byte nonce and an 8-byte tag, HKDF-SHA512 over the seed, salted with the nonce, under an info string of its own, together filling a version-4 UUID with its fixed bits set. It is new for every purchase and random-looking, so it links nothing for the store or the service. Any device lists every open purchase of the store account; it presents by itself only those whose token checks under its own seed and whose profile is this one. A purchase with no token (a promo or offer code redeemed in the store) is presented only when the user taps "Restore purchases".

### 6.2 The store product

| | App Store | Google Play |
|---|---|---|
| Type | Non-renewing subscription ("a service with a limited duration") | One-time product, marked in its details as a Tokenized Digital Asset, with the Financial features declaration updated for NFT sales |
| Identifier | `NAME_2Y_01` | `name_2y_01` |
| Display | "SimpleX name" | "SimpleX name" |
| Description | "Registers one name for 2 years." | "Registers one name for 2 years." |
| Price | The site's, $100, at each store's nearest price point | The same |

- The store's commission is SimpleX's cost: 15% of each sale while SimpleX's store earnings stay under $1M a year, once enrolled (Apple's Small Business Program; Google's first $1M, where Google's billing fee in the US, EEA and UK since 30 June 2026 makes it 10% + 5%), up to 30% above that.
- Multi-quantity is off. Play won't sell a product again until it is consumed, so one account holds one unused name purchase at a time. The app consumes (Play) or finishes (Apple) the purchase when its name registers.
- Each name is one purchase. Apple: "credits … purchased via in-app purchase may not expire", so a code row has no expiry.
- A name never unlocks a feature in the app (Apple 3.1.1, on NFTs).
- The product stays unavailable in each store until codes-only is off, so the stores themselves refuse a sheet before the sale. For iOS, it stays unsubmitted until the switch (6.9). Apple lets neither a product's ID nor its type be edited once created, and a deleted product's ID can't be reused, so the type is settled first.
- Review notes say a registered name comes back with the recovery key, not with "Restore purchases".

### 6.3 The service

```
App                                Store              Store service                    Ethereum
 | catalog -------------------------------------------> state, product, years, terms, clock
 | search the name (RSLV through the resolver)
 | Check your order: the name kept, the terms ticked
 | recovery key, the first time
 | catalog again, the name looked up again: sale closed or name gone -> no sheet
 | buy the product, token T -> sheet, payment
 | <-------------------------------- receipt with its reference R
 | code C = derive(seed, R); order signed by the service's clock
 | purchase a name {receipt, hash(C), order(C, owner, commitment, sealed, terms, signature)} ->
 |                                                    verify the receipt with the store
 |                                                    product -> any name, 2 years
 |                                                    commit: payment row, code row {hash(C), paid}
 |                                                    the order: sale, terms, name, code
 |                                                    reserve the code, queue the commit --> commit
 | <------------------------------------------------- payment: unused; order: registration id, or a refusal
 |   poll the registration; wait at least 60 s
 | reveal: label, links, signed intent, terms acceptance -> acceptance relayed, registerWithCredit -> name to the account
 | <------------------------------------------------- registered, expires
 | claim the name on the address or channel; consume (Play) or finish (Apple) the purchase
```

**Purchase a name.** `STCPurchaseName {payment :: ServicePayment, codeHash, order :: Maybe NameCodeOrder}`, signed with a fresh purchase key as badge requests are. `NameCodeOrder` holds the fields of `POST /api/name/register`, the plain code among them. A request with no order is a payment alone: a restored promo purchase with no name chosen, or an order too large to fit (below).

The payment part, in order:

1. The same receipt and the same hash: answered with the years and the code's state as it reads now (unused, reserved, used, revoked), whatever its age.
2. The product is looked up in the names catalog (Play: from the request, before any call to Play; Apple: after the signed transaction's offline check). An unknown one is refused.
3. The store verifies, with the badge refusals: invalid, pending, test purchase; and a quantity other than 1.
4. The same receipt with another hash: `receipt_used`, only after the store vouches for it, and the presentation recorded (payment, hash, first time).
5. Play is acknowledged, then one transaction inserts the payment (unique by store and transaction) and the prepaid code row `{code_hash, min_length 6, years 2, payment status paid, no expiry, payment_id}`, and commits. A verified, paid receipt of a known product is credited whatever state the service is in, so no payment is stranded.

The order part runs only after that commit, and only when the payment part answered a code state:

6. The order's code must hash to `codeHash`, else `bad_request` for the order alone.
7. The code order's own function, the one D5 extracts from `codeOrderHandler`, runs as on the web: an exact resend is answered with its registration id; otherwise the signature, the timestamp, the duration, the terms, the name and the code are checked, in the contract's order.
8. A failure in the order part leaves the payment committed, and is answered in the order part.

The answer: `STPNamePurchase {payment, order}`. `payment` is the years and the code's state, or a refusal. `order` is the registration id, or a refusal, or absent when no order was sent or the payment was refused.

| Payment part | Next step in the app |
|---|---|
| unused, reserved | Follow the order's answer |
| used | Its name registered before the app could consume it: consume or finish, find the name with the owner-signed `find` |
| revoked | "This payment was refunded, so it can't register a name."; consume or finish |
| `receipt_used` | "Bought with another recovery key"; never consumed here |
| `receipt_invalid`, `test_purchase`, `product_unavailable` | "Payment not confirmed"; unconsumed until support refunds it (iOS: the transaction finishes once its reference shows, since support credits a code) |
| `payment_pending` | Waits, nothing charged; presented again when the store says paid |
| `internal`, `provider_not_configured`, `rate_limited` | Sent again later |

| Order part | Meaning and next step in the app |
|---|---|
| registration id | Following the order |
| `sale_closed`, with the catalog state and when it reopens | "Paid, waiting for the sale"; nothing written; placed again as a plain code order when the catalog opens |
| `terms_unavailable` | "Paid, waiting to order"; placed again when the catalog has the terms |
| `terms_changed` | The terms line, or a community name's step, comes back unticked; placed again once ticked |
| `name_taken` | Taken, no stage started; the payment kept for another name |
| `order_exists` | The app finds its own order with `find` and opens it |
| `name_check_unavailable` | Retried in place |
| `bad_request` for a timestamp past 600 s (a request that waited offline) | Signed again and placed as a plain code order |
| `code_invalid` | The purchase is presented again, whose answer says why: `revoked` or `receipt_used` |

Repeats are safe by construction: step 1 answers the payment and step 7 the order, so the core resends the exact bytes of the first request after a lost answer, and the signed order is kept with the purchase record. Before the order commits, nothing about the payment depends on it. After a refusal, the paid code stays, and every later attempt is an ordinary code order: the path every prepaid code uses.

The order's refusals never reach the store, and the name isn't held while a payment waits.

Size: the whole request is bounded by 11106 bytes of connection info (3.3). An Apple signed transaction takes several kilobytes, since its header holds a certificate chain; a Play token is short; an order with one short link takes about a kilobyte, while `sealed` may reach 11095 characters for two 4096-byte links on the web. The core measures the encoded request before sending. When it doesn't fit, it sends the payment alone, then the order as a plain code order. P19 measures a real sandbox transaction with the longest short link the app makes.

Time: the core sets the request's timeout at 60 s, past the Play verify and the chain reads, and sends again on timeout.

**Store-paid codes in the code order.** `placeCodeOrder`, which the site's order and the app's both reach, checks a code row with a `payment_id`, so a derived code typed into the site meets the same checks:

- While codes-only is on or the day's cap is reached, it answers `sale_closed` before anything is written. The code stays paid.
- Try again (the worker's `requeue`, after `confirmLanded`, so a register that went through stays registered) and another name (retarget) of a store-paid code count against the cap like orders and meet the same refusal.
- While the terms of the name's kind can't be read, it answers `terms_unavailable` before anything is written, as every order does.
- An order, its reveal and a move's claim sign only the terms hash and kind the user ticked. When the catalog read before the sheet, before a signature, or before a waiting order is placed again gives another hash or kind, the terms line (for a community name, its step) comes back unticked, on Check your order before paying and on the registering screen after.

**Daily cap.** At most a set number of store-paid code orders, Try agains and other names are accepted per UTC day (a setting, starting at 50, raised as refunds prove rare), counted in a row per UTC day updated in the request's transaction, so concurrent requests can't overshoot it. Reaching it closes the sale for the day in the catalog answer, the same gate as codes-only, with the operator alerted as it nears. Refunds are watched by hand in the stores' reports at launch.

**Catalog.** The answer the site's search gives, without a name: open, codes-only, or closed for the day; years; term start; the namespace (`tld`); the chain id and the resolver and sponsor addresses the commitment and the intent sign; the hash and link of the general and the community terms (null for a kind whose text the service can't read now); what each community name is for (the service's texts); the product identifiers; the service's current time. Answered from a cache refreshed every few seconds on the main lane, since the service reads the terms links on chain. The app keeps its offset to that clock and fetches the catalog again before each signature, since an order must be within ±600 s of the service's clock and its duration within the term slack.

A name's kind before paying comes from the resolver's answer once the resolver fills `nameTerms` (P2), its hash checked against the catalog's; where an answer lacks it (a router before v24), from the catalog's community texts. Either mismatch meets `terms_changed` at the order, after paying, and the app then shows the terms line or the community step, with "Choose another name" as the secondary, which keeps the payment for a buyer who won't accept them. The app makes no per-name request to the service.

**Terms text.** A second command answers a terms text by its hash, as the site's `GET /api/terms/<hash>` does. The app shows it only after checking, as the site does (`webapp/src/api.ts:382-385`), that the answer's link is the catalog's and a git.simplex.chat raw-commit link to a `.md` file (the site's `termsLink`, which on the local test chain 31337 also takes its `http://127.0.0.1` form), that the link's keccak256 is the hash the order signs, and that the text's sha256 is the link's `#sha256=`. Otherwise it shows the terms as unavailable, so a modified service can withhold the text but not change what the user accepts.

**The other SMP commands** call the functions D5 extracts:

| Command | Web route it shares | Stage |
|---|---|---|
| Catalog | The search answer's fields | 1 |
| Terms text | `GET /api/terms/<hash>` | 1 |
| Code check | `POST /api/code/check` | 1 |
| Code order | `POST /api/name/register` | 1 |
| Registration view | `GET /api/registration/:id`, answered at once with no hold; the app polls | 1 |
| Reveal | `POST /api/registration/:id/reveal` | 1 |
| Try again | `POST /api/registration/:id/retry` | 1 |
| Find | `POST /api/registration/find` | 1 |
| Another name | `POST /api/registration/:id/name` | 1 |
| Move approve and claim, with the chain reads the app signs against | New | 1 |
| Record edit, with the edits left, their next date and the resolver nonce | `POST /api/name/records` | 2 |
| Renewal purchase | New | 3 |

**Lanes and bounds.** Name commands are matched explicitly in the service's dispatch and lane choice, whose catch-alls would otherwise take them silently. The catalog and the terms text run on the main lane; the code check checks only the database; the rest run on a names lane of their own, serial like the others, since each reads the chain or a store, so a slow node holds up neither a badge request nor the renewal lane. The service's limits on the site's routes are per IP address, which private routing hides, so each SMP command class has a service-wide per-minute bound, the catalog a much larger one, and the SMP code order checks the code's state before any chain read. A Play read takes a slot of `maxPlayPurchaseReads`, so a flood of invented receipts makes store purchases wait rather than uses up the Play quota.

**Owner-code relays.** Commands relay the code wallet's `approveWithSig` and the app wallet's `claimWithSig`, each checked as the site's record edits are: signer, nonce, deadline, the `from` address owning the token. Each is a transaction whose gas the service pays, so moves are bounded per name per 365 days by a setting like `record_edits_per_year`, 3 by default. The commands also return the code wallet's nonce, the name's offer, the app account's nonce, and whether the name policy lists the name, with its community text. The terms draft says receiving a name accepts the terms, recorded on the blockchain, and that a reserved name can be transferred but not renewed. So P26 reads in the contracts whether a claim records the receiving account's acceptance: if it does, the claim relay takes the app account's acceptance signature as the reveal does; if not, the gap goes to the terms' author.

**Refunds** are made in the stores, by their rules; a registered name can't be taken back.

- A receipt is refused when the verifiers see its refund at presentation: Apple's `revocationDate` in the transaction the app sends, Play's purchase state tied to a revoked purchase. Nothing relies on that beyond the first presentation.
- A refund after the receipt is credited isn't detected at launch: before registration as after it, it costs SimpleX that name, and the daily cap bounds both. Counting refunds automatically (App Store Server Notifications, Play's voided purchases), which would then revoke unspent codes, is later work.
- Support refunds a Play order only with revocation, which cancels the purchase in Play, so a refused or never-presented receipt can't be credited later and stops blocking the account's purchases.

**The operator command,** `//store-payment`, apart from the existing `//refund`, which records refunds of name balances. It is keyed by the payment's reference, and shows the payment and its code row. Only while no registration holds the code (a failed one holds it 30 days; a live one is answered "registering") it can:

- refund a Play payment: it takes the order number the buyer gives support and checks it with Play's `orders.get` against the reference, keeping nothing else `orders.get` returns; in the code's turn it revokes the code row by its id and records the refund as due; then it calls `orders.refund` with `revoke=true`, and a failed call is retried by running the command again;
- move a payment: only for an unused code credited at least 7 days earlier, with no registration of it in the last 7 days, and only to a hash recorded with a `receipt_used` presentation of that receipt. In one transaction in the code's turn, the old row is revoked and gives up its `payment_id` to a new row with the recorded hash, so no old registration can reserve the code again.

The service account needs Play's permission to manage orders.

**Records.** The service keeps, per payment, the store, the transaction's reference, the product, for App Store payments the price and currency, and each `receipt_used` presentation until the code is spent or moved. It stores no receipt.

### 6.4 The core

The purchase record: product, the store's reference, the receipt, code, state (paid, used), the name being bought with what it opens and the terms the user ticked, the exact signed order, and the registration it funds. It is kept with the database, so a backup or migration carries it.

| Command | CLI form | Does | Stage |
|---|---|---|---|
| Wallet sign | `/_wallet sign message <userId> <n> <text>` | EIP-191, 65 bytes with a low `s`; EIP-712 for the `RegisterName`, record-edit and transfer intents | 1 |
| Wallet names | `/_wallet names` | The names an account holds, with expiry refreshed whenever the name is verified | 1 |
| Catalog | `/_name catalog` | The catalog, with the clock offset; the terms text checked against its link and hash | 1 |
| Keep the order | `/_name keep` | The chosen name, what it opens and the ticked terms, recorded before the sheet opens | 1 |
| Purchase | `/_name purchase` | Takes a receipt, derives the code, records the purchase, signs the kept order if one waits, sends both from a worker | 1 |
| Order with a code | `/_name order` | The order record, the secret, the commitment, the signed order | 1 |
| Follow | `/_name follow` | A worker per user: polls, reveals with the terms acceptance, sends progress events, recovers after a restart | 1 |
| Try again, another name, stop | `/_name retry`, `/_name other`, `/_name stop` | The order's next step, as the screens ask | 1 |
| Claim | `/_name claim` | The address or channel claims the name once registered | 1 |
| Redeem | `/_name redeem` | The code's wallet, the names it holds read with `find` and the chain, the transfer, recorded before the first relay and resumed at start | 1 |
| Import | `/_wallet import` | Restores from the phrase, refused where a wallet exists, the counter set past the accounts found | 2 |
| Scan | `/_wallet find names` | Over ROWN, with a gap limit, a start index, progress events and a stop | 2 |
| Set what it opens | `/_name set links` | The relayed record edit, and the edits left | 2 |

- The purchase command answers once the purchase is recorded and sends from a worker, as the badge hand-over does, so the app's Play acknowledgement never waits on the network.
- The commitment secret and the sealed label and links are derived for the account, under info strings of their own. The order signs its terms' hash, and the reveal carries the owner's terms acceptance, of the new terms after the user ticks them when they changed since the commit. If the service refuses the signed registration three times, the app stops signing, as the site does, and the user decides.
- The wallet account is created when the user continues from the recovery key screen, or at the first payment or code after it, never at app start and never by a search.
- A name with no target ("Opens: nothing") signs empty links, and the claim is skipped.
- **The app controls each step.** The reveal needs the app to run, so a closed app stops the registration at the next step. The state survives; the worker resumes at start, within the order's `revealBefore`, 24 hours. Past it, the order reads failed: Try again orders again and charges nothing.
- **Stopping.** "Cancel" on "Still registering" stops the worker for this order, which never reveals. The service has no cancel for a code order, so the order lapses at its `revealBefore`, reads failed, and the code it holds pays for another name from then on, using the site's words: "Once its 24 hours to sign are over, choose another name for it." A cancelled order whose commit hasn't landed lapses the same way.
- **After a reinstall** with no database, stage 1 has no wallet to recognise a purchase: an open purchase with another wallet's token shows "Bought with another recovery key" (6.6), and support can refund the Play payment or, by the 7-day rule, move it. From stage 2 the wallet comes back from the 24 words, recognises its open store purchases by their tokens, recomputes each code and gets the same answers; a registration under way is found again with `find`.
- A purchase answered `receipt_used` is never consumed. The app presents its recorded purchase again only when the user opens the hub or that screen, to learn whether support moved it here.
- Hidden profiles are refused (section 7).

### 6.5 The apps: store lanes and open purchases

Android first; iOS gets the same code behind a build-time switch (6.9). Desktop and F-Droid never show store purchases. The app loads no store product until the catalog says the sale is open, and shows the store's price in the store's currency.

- **Purchase lane.** Android: `launchBillingFlow` for the product; the result goes to the core command; the purchase is acknowledged once the core holds it, as badges do, and consumed when its name registers. iOS: `product.purchase(options: [.appAccountToken(token)])`; the transaction is finished when its name registers. Both set the store's account field to the recognising token, never to an identifier of the user.
- **An open purchase** (paid, name not registered) is found at start from the store's own list (Play's purchases query; on Apple `Transaction.unfinished`, and `Transaction.currentEntitlements`, whose latest transaction per product counts as open when it carries this wallet's token and the service answers its code unspent) and continues. When the database still holds the chosen name, registration resumes for it; otherwise the app asks for a name.
- **One product, one open purchase.** Play sells the product again only after consumption, so any open purchase on the account blocks buying on that account. On both platforms the app offers the held payment of the same profile instead of a second one, and on iOS too it sells no second purchase while one is open, so Apple's latest-per-product list never hides an earlier payment.
- **The store-account lines.** In a shown profile, whatever another profile on the device or another wallet holds gets one line, the same whoever made it, true of the store account and naming no profile, in the place of the footer, with Pay off:

| State | Android line and secondary | iOS |
|---|---|---|
| Another profile's or wallet's open purchase | "This Google Play account already has an unused purchase of a name. Use it where it was bought. If you can't, contact the SimpleX team." · "Contact SimpleX team" (the draft carries only the purchase's reference, never its order number, so it doesn't tell a promo purchase from a paid one) | "This Apple Account … If you can't, ask Apple for a refund." · "Request a refund", StoreKit's refund sheet |
| Another's pending purchase | "This Google Play account has a purchase of a name waiting for approval. Use it where it was made once it's approved." · no contact, as nothing is charged | Blocks nothing and shows nothing; if both are later paid, each is a held payment of its own profile |
| A purchase with no token that no profile here records | "This Google Play account has an unused purchase of a name. Restore purchases to use it here." | The same with "This Apple Account" |
| A purchase the store did not confirm | "This Google Play account has a purchase of a name that the store did not confirm. Contact the SimpleX team." · "Contact SimpleX team" | Finished once its reference shows; blocks nothing |

- A purchase the store reports refunded (on Play: no longer listed after a refund with revocation) is no longer open: the app drops its record, offers no held payment, and it blocks nothing.
- **Restore purchases**, as for badges, in the hub, in store builds: a payment with no name → the search with the payment kept; one whose name is known → its order; a purchase with another wallet's token → on a phone with no key, "Bought with another recovery key" (with "Enter recovery key" from stage 2); on a phone with its own key, the store-account line "This Google Play account has an unused name purchase that can't be used in this profile. Use it where it was bought. If you can't, contact the SimpleX team."; nothing found → the badges' "Nothing to restore".
- A sandbox test settles what Apple doesn't document: a reinstall, a second device, two purchases of one product, another profile's Ask to Buy approved after a second purchase, how StoreKit lists a refunded purchase; with Play's slow test card for pending purchases.

### 6.6 Screens

The screens follow the two canvases, keep the site's way where the site taught something (6.7), and are built with the app's own components, as the badge screens follow the badge webapp: a centred title, a card of rows ending in the total, one main button. The rows below are journeys, one screen per number; words in quotes are the screen's.

On every screen: one main action at the bottom, in the same place in every state, the secondary's slot kept under it even when empty; outcomes appear in place on the same screen, except the canvas's two alerts (1·3a, 1·8a); names are counted in characters, never "letters"; every price is the store's, in its currency. A state not named here is one of the listed states' words.

The terms line, the site's: "I agree to SimpleX general terms and do not knowingly violate any trademarks.", whose "SimpleX general terms" opens the text the order signs, shown inside the app. It sits just above the button of every act that registers a name (1·3, 2·3, 1·3 with a kept payment), once the button can act, ticked for each name before the button works. A community name has no tick there: its terms are accepted on 1·2a. On `terms_changed` the line comes back unticked under the site's "The terms changed. Agree to them again.", and a community name's step comes back.

The footer of 1·3: "Alice's address shows the name once it is registered, and your contacts get your updated profile. Anyone can look up what a name opens." From a channel the first sentence reads "#Bakery Deals shows the name once it is registered."; opening nothing, "The name will open nothing until you set what it opens." The store's line, with an act paid in the store, is pinned just above the terms line so it shows whenever the button can pay: "Paying with your Google Play account lets Google and SimpleX Chat link this payment to the name. SimpleX Chat can also link every name and badge this account buys, in any profile." (iOS: "Apple Account", "Apple"). A code involves no store and doesn't add it.

#### Row 1, buy in the app (Google Play; iOS when its payment is on). Stage 1, except "I already have a key" on 1·4, "Look for my names" on 1·4a and Import's "I lost my phone"

- **1·1 Your address.** The app's address screen, its row "Get SimpleX name" without "BETA". → 1·1a. From a channel's info, the channel's row, "Let people join via name registered with this channel link.", where the channel has a link and the user owns it. From settings, 5·1. Register from the "available" alert and Re-register from an expired own name open 1·2 with the name searched.
- **1·1a SimpleX names, empty.** The hub, in place of today's name screen. "Your names", "You do not have a name yet", "A SimpleX name lets people find you without sharing a link."; rows "Register a name" ("Pay in the app, or with a code.") → 1·2, or 4·1 where the build can't pay; "Import" → 6·1. In stage 1 Import reads "Attach names you manage elsewhere."; stage 2 adds "Recover your names from a different device".
- **1·2 Register a name.** Main "Register", off until a free name; secondary "I have a code" → 2·1. Field "Search a name", `.simplex`; the name is checked for length and grammar while typed, with no request, and looked up over SMP on Return or Search, with "Checking the name…". One answer row under the field: the name, "Available", "$100.00", "2 years"; a community name followed by "Community ⓘ", whose ⓘ says what the name is for. "Buy in browser" sits under the answer in the US only. The price slot and the row keep the height of their tallest content across states, measured at the largest text size, so the main button never moves. While the product loads, the slot reads "…" and Register stays in place, off. Other answers:

| Answer | Words | Action |
|---|---|---|
| Too short, too long, not valid | "5 characters. A name has 6 characters or more: a–z, 0–9 and single hyphens between them."; "64 characters. A name has at most 63."; "Use only a–z, 0–9 and single hyphens between them." | Register off |
| Taken (an expired name reads as taken) | "Taken", no pictures | Secondary "What it's used for" → its address or channel, a tap from opening or connecting; "Is it yours? Enter its name code" → 2·1 |
| Opens your address | "Opens your address"; "If you bought it on the site, you edit what it opens there, with its name code." | Main "Show on your profile" → 1·1 with the name; secondary "Bring it into the app" → 2·1 |
| This app holds it | "Your name" | Opens its own screen |
| This profile has an order for it | "You already have an order for this name." | "Open that order" → 1·7 in its state |
| Reserved | "Reserved names are not sold here. If it is yours to use, contact the SimpleX team." | "Contact SimpleX team" |
| No answer | "This name could not be checked." | "Try again" |
| No server resolves names | Today's alert: "None of your servers are set to resolve SimpleX names. Configure servers, or use a connection link." | Network & servers |
| The terms can't be read | "Names can't be ordered right now. Try again later." | Register off; read again when the connection returns or the screen reopens |
| Before the sale | "SimpleX name sale starts in" over "12 days 03 hrs 41 min 07 sec" | "Register with code" → 2·1 |
| Codes-only | "Names can be registered with an investor code for now." | "Register with code" |
| Closed for the day | "Paying in the app reopens in 5 hrs", by the service's clock | "Register with code" |
| The store can't sell | "The store could not be reached."; "Purchases are off on this device."; "Paying with Google Play isn't available right now."; "Can't load name prices. Check your connection." | "Register with code"; read again when the connection returns or the screen reopens |
| Hidden profile | "A hidden profile can't register a name. Unhide it, or switch to another profile." | In place of Register |

When the sale opens, the price and "Register" take the place of the line and "Register with code", in place.

- **1·2a Community terms** (a community name, before 1·3). Title "Community terms"; "bakery.simplex is a community name. Its owner accepts these terms."; the text as the service answers it by its hash, with "View at commit 3f9a2c1"; "Loading the terms…" while it loads; "The community terms can't be shown now. Try again in a moment." with "Try again" when it can't. The tick: "I accept the community terms and will use this name for its community. ⓘ", whose ⓘ says which community the name is for. Main "Continue", off until ticked. A community name registers for 2 paid years.
- **1·3 Check your order.** Main "Pay $100.00" (the badge's `payText`) → 1·4 the first time, else 1·6; secondary "Use a code" → 2·1. Rows: Name; Opens, the address or channel it started from, as "Alice's address", with ✕ → 1·3a; Term "2 years"; Registered until "31 Dec 2028"; Total "$100.00". Under the card "Nothing renews automatically.", then the footer, the store's line and the terms line. States:
  - Opens: "Replaces alice.simplex on your profile." under it where the name replaces one. With no address and no channel, "Create an address first", which opens the address screen and returns here. An address or channel without a short link isn't offered: the row says it needs one, with the address screen's upgrade, since a name's records hold one short link per kind (`webapp/src/names.ts:30-36`). After ✕: "Opens: nothing".
  - A code kept from 2·1: Total becomes "Free SimpleX name", the term is the code's ("5 years", "Registered until 31 Dec 2031"; a community name 2 years, with "A community name is registered for 2 years, not your code's 5."), main "Register with your code", secondary "Remove code"; a code that doesn't cover the name, the site's line on 2·2.
  - A kept payment: Total "Covered by your payment", main "Register with your payment"; while the sale is closed, "Waits for the sale to open; open the app then to register it. dynamism.simplex isn't held meanwhile."
  - Loading: the total reads "…", the button in place, off, "Loading…"; the footer and the terms line appear once the button can act.
  - The re-check: right before the sheet opens, the app reads the catalog again and looks the name up again; while that runs the button reads "Loading…", off. A name registered or reserved since: the site's "This name is no longer available", "dynamism.simplex became unavailable after you searched for it. Nothing was charged.", main "Search another name", no sheet. A failed lookup: "This name could not be checked." with "Try again", no sheet. Other terms: the terms line back unticked. Sale closed or store gone: the total row keeps the reason, and "Use a code" stays.
  - Store-account lines (6.5): in the footer's place, Pay off.
  - Extremes, at the largest text size: a 63-character name wraps in the card and the content scrolls under the pinned lines and button; the longest store price string ("Rp 1.649.000,00") in a translated label; the button's bottom stays anchored.
- **1·3a ✕ on what it opens** (the canvas's alert). "Nobody will find you by it", "You will own dynamism.simplex, but it will not lead to your address or a channel."; "Register anyway" → 1·3, its row "Opens: nothing"; "Cancel". Its links can be set from stage 2.
- **1·4 Recovery key** (the first payment, code or move only; right before it). Title "Set up your recovery key"; "Your names live on this device. This key is the only way to get them back if you lose your phone, or to move them to another device. SimpleX keeps nothing on its servers."; a switch "Back up later | Back up now"; "Either way the key is created now. Later means you can write it down any time from SimpleX names." Main "Create a new key" → 1·4a to back up now, else 1·6; secondary, from stage 2, "I already have a key" → 6·2, with "A phone holds one key: if you had names on another phone, enter its key first."
- **1·4a Your recovery key.** "This key protects every name on this device, in every profile. Anyone who has it controls them too."; the words hidden; "Make sure no one can see your screen."; main "Reveal" → 1·5; from stage 2, "Look for my names" → 6·2a. Also from the key row on 5·2, where Reveal first asks for the device's passcode or Face ID.
- **1·5 The 24 words.** "Write the words down, in order, and keep them somewhere safe and offline. Anyone who has them can take your names."; the words; "Copy"; main "I have written it down" → 1·6 (with a code → 1·7; before a move → 3·2; from 5·2, back there). The copy is marked sensitive and expires (iOS `localOnly` with an expiration date, Android `EXTRA_IS_SENSITIVE`).
- **1·6 The store's sheet.** The platform's. Dismissed, nothing is charged → 1·3.
- **1·7 Registering.** Title "Registering dynamism.simplex"; "Keep the app open until the name is yours. If it closes, open it again within 24 hours to carry on." Three stages, each showing its number before it starts, its number highlighted while it runs, ✓ when done, ✕ where it failed: "Committing" ("Committed"), "Waiting", "Registering" ("Registered"). Under them one notice, its height kept for its longest text; then the card: "Paid", "$100.00 · Google Play" ("Price" until the service confirms; with a code, "Code", "Free SimpleX name"); "Term", "2 years"; "Reference" and the payment's reference. Main "Close", which only leaves the screen → 1·9. The title, the lead and the notice keep the height of their longest text across states, and the card keeps its three rows, measured for the shown name, so nothing moves. The notice runs through: "Confirming your payment."; "Placing your order. The wait starts once it is in."; the countdown "0:42" with "Every new name waits a little, so nobody who sees your order can take the name first."; "Registering the name. It is yours in a moment."; "This is taking longer than usual. It continues by itself."; offline after paying, "No connection: your payment is safe and is confirmed when you are back online." States of this screen:

| State | Title and lead | Notice and rows | Main; secondary |
|---|---|---|---|
| Waiting for approval (Play pending, Ask to Buy) | "Waiting for approval"; "Nothing has been charged." | No stage started. "dynamism.simplex isn't held meanwhile: if someone registers it first, this payment, once approved, pays for another name." "Price" row | "Close"; on iOS after a day "Stop waiting", since Apple lets a request lapse after 24 hours |
| Purchase cancelled by the store | "The purchase was cancelled." → 1·3 | | |
| Paid, the sale closed | "Paid, waiting for the sale"; "Your payment waits for the sale to open; open the app then to register the name." (or "to reopen in 5 hrs") | No stage started. "dynamism.simplex isn't held meanwhile." | "Close" |
| Paid, terms unreadable | "Paid, waiting to order"; "Your payment is kept. Names can't be ordered right now; open the app later to order it." | No stage started | "Close" |
| Terms changed while waiting | "Agree to the terms again"; "Your payment is kept." | "dynamism.simplex isn't held meanwhile."; the terms line or community step; asked only once the order can be placed | Order goes in once ticked; secondary "Contact SimpleX team", or "Choose another name" when the name's kind changed |
| Terms changed at the reveal | Registering | "The registration is not signed yet."; the terms line back | Signed once ticked |
| Signing refused three times | Registering | "The name could not be signed", "The service refused the signed registration." | "Try again", which reads the clock and signs again; "Close" |
| Taken | "dynamism.simplex was taken"; "Someone registered it before your order could." (a reserved name "dynamism.simplex is reserved"; before the order went in, no stage started, "dynamism.simplex became unavailable before your order went in.") | ✕ "Taken". "What you paid is kept for another name. For a refund, contact the SimpleX team with its reference." (iOS: "For a refund, ask Apple." with "Request a refund"). "Kept", "$100.00" | "Choose another name" → 1·10; "Close" |
| Failed | "Paid, not registered"; "Your payment is kept. Trying again charges nothing." | ✕ on the stage that failed. The service's reason, or "The name could not be registered yet. If Try again still fails, contact the SimpleX team with its reference."; "Charged again", "Never" | "Try again"; "Close", or "Choose another name" when the name went since |
| Another name refused by the move limit | "Your payment is kept." | "This order cannot move again", "The payment comes back for another name 30 days after the failure." | "Contact SimpleX team"; "Close" |
| Stopped, until it lapses | "Your payment is kept." | "This order can't register it. Once its 24 hours to sign are over, choose another name for it." | "Close" |
| Not confirmed | "Payment not confirmed"; "The store did not confirm this purchase. If you were charged, contact the SimpleX team." | Reference; the draft carries it and Play's order number when the purchase has one | "Contact SimpleX team"; "Close" |
| Refunded | "Payment refunded"; "This payment was refunded, so it can't register a name." | | "Close" |
| Bought with another recovery key, no key here | "Bought with another recovery key"; "This purchase belongs to another recovery key. Enter it here to use it." (stage 2; in stage 1, "… contact the SimpleX team with the reference below.") | Reference | "Enter recovery key" from stage 2; "Contact SimpleX team" (iOS "Request a refund") |
| Bought with another recovery key, after `receipt_used` | The same title; "This purchase belongs to another recovery key. Use it on the device that has that key, or contact the SimpleX team with the reference below: if it stays unused there for 7 days, they can move it to this device." | Reference | "Contact SimpleX team" |

  A store promo purchase, which was free, says "purchase" and "What you bought" where these say "payment" and "What you paid", drops "Paid, " from titles, and its card's first row reads "Store promo code", "Free SimpleX name".
- **"Still registering"** (the app reopened mid-registration; the canvas's 9a): "dynamism.simplex was paid for and the registration has not finished. Continue where it left off, or cancel and keep the payment for another name."; "Continue" → 1·7; "Cancel" stops the order (6.4). Not opened within 24 hours, the order lapses and reads Failed.
- **1·8 Yours.** Title "dynamism.simplex is yours"; "Registered until 31 Dec 2028. It opens Alice's address." (after ✕, "It opens nothing yet."); the three stages ✓; "Alice's address shows the name now."; the card. Main "Close" → the address, its row "Until 31 Dec 2028", or the channel's info. Add to calendar isn't drawn.
- **1·8a After the first name, once** (the canvas's alert, only if the key isn't written down). "Back up your recovery key", "dynamism.simplex is yours, but without the key you cannot get it back if you lose this device."; "Show me the key" → 1·4a; "Not now" → 1·9. Dismissed, it doesn't come back: the orange key row carries it. It also follows 3·3.
- **1·9 Your address, after Close.** The name's row with the order's state: "Registering", "Waiting for approval", "Waiting for the sale", "Waiting to order", "Not registered · Try again", "Terms changed · Agree again", "Taken first · Choose another name", "Your kept payment · Register a name", "Moving"; once registered "Until 31 Dec 2028", under today's header "Your SimpleX name". After a code's order is taken, the row reads "Get SimpleX name" again. No alert appears over another screen.
- **1·10 Register a name, payment kept.** Above the field "Your payment is kept. It pays for another name, for 2 years."; on a free name "Covered by your payment", main "Register with your payment" → 1·3 with it.

Support drafts add one line, "This reference links this chat to the payment", and the contact flow offers an incognito connection. After a `receipt_used` answer, the draft also carries this device's code hash for that receipt, the one recorded hash support may move the code to; after a move, support's reply says the payment moved to the asking device.

Money: the store sheet is the explicit act; the name is kept before the sheet opens and the code is derived from the receipt, so an interruption after payment is recovered from the store's purchase list; one store transaction makes one code and one registration (AC2).

#### Row 2, a prepaid code. Stage 1

- **2·1 Redeem code** (the badge's title; from 1·2's "I have a code", 1·3's "Use a code" and the "Register with code" of any answer). "A code from SimpleX lets you register a free name. A name code brings a name you own to this app."; the field, "Paste", "Tap to scan". One field takes an owner code (`SN-` and 30 characters) or a prepaid code (`SN<length>-` and 20); the core's parser tells them apart. Checked, "This code allows you to register a free name of 8 or more characters, for 5 years."; main "Use this code" → 2·2, or back to 1·3. An owner code: "Name code of dynamism.simplex."; main "Continue" → 3·1. Prepaid codes are checked by the service's code check, and owner codes with the owner-signed `find` and the chain; "Checking…" in the result's place; offline "Can't check the code. Check your connection.", the button off. The code is spent only by an order. Refusals, on the screen, the button off: "This code is not valid" ("Check it for a mistyped character."), "This code is being used for a name now" ("Each code registers one name; this one is held by an unfinished registration."), "This code was already used" ("Each code registers one name."), "This code is not valid: SimpleX withdrew it", "This code has expired" ("It can no longer register a name."), "This code is not paid for yet" ("It works once its payment is confirmed."), "This code is not valid: SimpleX did not issue it", "This is not a SimpleX names code" ("Check it and try again."), "The code could not be checked" ("Try again in a moment."), "Too many checks just now" ("Your code may be fine."), and a badge code, "This is a badge code. Redeem it in Support SimpleX."
- **2·2 Register a name, with the code.** 1·2 with its typed name kept. "Your code covers names of 8 or more characters."; the row "Free", "5 years"; main "Register" → 2·3; secondary "Remove code" → 1·2, or 4·1. A name shorter than the code covers: "Your code allows you to register a free name of 8 or more characters. This name has 6."; where the build can't pay, no price, Register off.
- **2·3 Check your order.** 1·3 paid with the code, no store line: Total "Free SimpleX name", main "Register with your code" → 1·4 the first time, then 1·7 and 1·8.
- **2·4 Taken, with a code.** 1·7's layout: "Someone registered it first", "dynamism.simplex was registered just before your order."; "Your code was not spent: use it for another name. Copy it before you close: the app doesn't keep it."; the row "Code", the code with "Copy". Main "Search another name" → 2·2; "Close". A failed registration: "Not registered", "Your code is kept. Trying again spends nothing more.", "Try again". A code no longer usable: "Your code can no longer be used for it, so trying again cannot register it.", its reason, main "Search a name". A code another registration holds: "Your code is held by another registration; try again later".

#### Row 3, a name bought on the site. Stage 1, except what is marked

- **3·1 Redeem a name** (from 2·1 with an owner code, or the site's "Open in SimpleX", as a sheet, as the badges' link sheet, with Cancel at its top; nothing moves until the user taps Redeem). Rows: Name code, shortened, ✓; Name; Until; Opens. "Redeem moves it into this app and attaches it to Alice's address: your recovery key holds it and brings it back on a new phone. Show on your profile keeps it on the site, where you edit its links." The terms line, for Redeem only. Main "Redeem" → 1·4 the first time, else 3·2; secondary "Show on your profile" → 1·1 with the name (today's Save, `APISetUserDomain`).
  - A name that opens a channel of this profile shows on that channel; one that opens both an address and a channel has two Opens rows.
  - A name that opens another address: in stage 1, Redeem stays off until its links are set on the site, and Show on your profile stays off, with "It opens an address that isn't this profile's. Set its links on the site first, or switch to the profile whose address it opens." From stage 2, Redeem attaches it here.
  - Before anything moves: a name past its expiry, "dynamism.simplex expired on" its date and "Renewing isn't available in this version yet."; a code that holds no registered name, "This code holds no registered name yet. Finish its order on the site, then open the code here again."; a name this app already holds, "dynamism.simplex is already in this app.", "Close"; a hidden profile, "A hidden profile can't add a name. Switch to another profile, then open the link again."; a reserved name says it can't be renewed; while the terms can't be read, "Names can't be ordered right now. Nothing changed. Try again later."
  - Refused: "This name code no longer holds dynamism.simplex: it was moved." On a phone with its own key, "The name is in the app on the phone you moved it to."; on a phone with none, from stage 2, "If you moved it to this app on a phone you no longer have, that phone's recovery key brings it here.", with "Enter recovery key".
- **1·4 before a move.** The first move only, as on row 1, then 3·2.
- **3·2 Moving.** 1·7's layout. "Moving dynamism.simplex"; "You can close this screen: the move goes on while the app is open, and when you open it again."; stages "Allowed", "Moving" ("Moved"), "Shown"; "The name code allows the move, then this app takes the name. SimpleX sends both steps and pays for them."; rows Name code, Until, Reference. Main "Close" → 1·9, "Moving". The core records the move, with the terms ticked, before the first relay and resumes it at start until the claim lands; a move whose terms changed since waits here with the terms line back. Offline, "No connection.". Failed, "Not moved", "dynamism.simplex is still held by its name code."; "The move could not finish. If Try again still fails, contact the SimpleX team with its reference."; "Try again"; "Close".
- **3·3 Moved.** "The name is in your app"; "dynamism.simplex is yours, held by your recovery key. The code no longer controls it."; ✓ "Allowed", "Moved", "Shown"; "Alice's address shows the name now."; main "Close" → 1·9. 1·8a follows after backing up later.

The yearly bound of moves can't stop a first move: in stage 1 a name moves in once, as nothing moves it out.

#### Row 4, builds that can't pay in the app (F-Droid and desktop; iOS while its store payment is hidden). Stage 1

- **4·1 Register a name.** 1·2, searched in the app, with no price: the row "Available", "2 years", and "Pay on simplex.domains: it opens with this name and your address." Main "Buy in browser" → the site with the name (6.8); secondary "I have a code" → 2·1. Other answers as on 1·2. Before the sale, or while the site sells for codes only, "Register with code" takes Buy in browser's place. A name that already opens this address gets 1·2's answer.
- **4·1 on iOS outside the US, while its payment is hidden.** The row "Available", "With a code"; "This version of the app cannot take payments, so a name is paid for with a code."; main "Register with code" → 2·1. Nothing names a site.
- **4·2 On the site.** The site's "is yours" page; its card "Use in the app", "Open it in SimpleX to show it on your profile, or to move it into the app. Once it moves, its links can't be edited here."; the code masked, "Show · Copy"; main "Open in SimpleX" → 3·1 with the code filled in (on a computer, "Copy", for 2·1); "Edit links". The card and the button are the site's change (6.8).

#### Row 5, SimpleX names. Stage 1; changing what a name opens comes in stage 2

- **5·1 Your settings.** The row "SimpleX names" under Advanced settings, after Network & servers and Notifications → 5·2, or 1·1a with no names.
- **5·2 SimpleX names.** "Your names": each row the name, one line and a chip: "Until 31 Dec 2028 · opens Alice's address"; "#Bakery Deals", "Moving"; "Opens nothing"; each order's state as on 1·9; a name the profile shows that the site still holds, "On the site". Then "Register a name" → 1·2; "Import" → 6·1; "Restore purchases", store builds only (6.5); "Your recovery key", "The key that protects your names", "Not written down" in orange after backing up later, until 1·5, then "Written down" in grey → 1·4a; "One key for every profile on this device." Each profile has its list. Another key entered on a phone that has its own: "This device already has a recovery key. It protects dynamism.simplex and 1 more, and a device holds one key.", "Show the key".
- **A name's own screen.** The name with Copy, its date and what it opens; "Remove name" for the name the profile shows; "Bring it into the app" → 2·1 for one the app doesn't hold.

#### Row 6, a new phone. Stage 2, except "Bought on the web"

- **6·1 Import** (from the hub). "I lost my phone": "Reclaim names from my recovery key", "Brings back every name those words own, and lets this device manage them." (stage 2). "I already own a name": "Bought on the web or in another app", "For a name that already points at one of your profiles." → the name's field and Save (stage 1).
- **6·2 Enter your recovery key.** "Enter the 24 words in the order they were shown to you."; one field, each word checked on the device as it's typed, nothing leaving the device: "11 of 24 words.", "abilty is not a recovery word.", "These 24 words do not make a key: check their order.", "24 words."; "Names found are added to Alice, the profile you are using now. Use the key on this device only: two devices with one key can register two names to the same key."; main "Get my names back" → 6·2a.
- **6·2a Looking for your names.** "Looking for names this key owns. This takes a minute."; "Found so far", each name with what it opens; "Checking key 9 …"; "The SimpleX names service is asked about every address this key could hold a name at, so it sees them together."; main "Stop looking" → 6·3 with what it found. Offline, "No connection.", and it looks again when the app is back online.
- **6·3 Your names are back.** "Found 2 names", with each name's date and what it opens; "Added to Alice. Alice's address shows dynamism.simplex now."; "Stopped after 20 unused keys in a row. If you had more names, look further."; main "Done" → 5·2, or the order from 1·4; secondary "Look further" → 6·2a, from where it stopped. None found: "No names for this key. It may be a key from another app, or its names expired and were taken.", "Try another key" → 6·2, and the key isn't kept (question Q1); opened from 1·4, a key with no names yet becomes this phone's key and the order goes on. A hidden profile: "Alice is hidden, and hidden profiles cannot own names. Switch to another profile first."
- A phone migrated with its chat database needs no restore: the key moves with it. After restoring an older chat database: "Look for your names again", "needed before registering", with Register a name off until it has run, so a key that already owns a name isn't used again.

### 6.7 Against the sketches and the site

| Step | Sketches | Site | Here | Why |
|---|---|---|---|---|
| Where it starts | Address, settings or a channel open the hub ("Register a name", "Import") | | The same, in place of today's name screen | As sketch |
| Search (1·2) | Its own screen; Register off until a free name; "I have a code" | The search page | The same; "Buy in browser" added, in the US only | As sketch; the site link is the decision for store builds |
| A taken name | "Taken, used by address and channel" in the row | Pictures of what it opens | A "What it's used for" button | Asked in review: neither fits a long name |
| What it opens (1·3) | "Points at" row; ✕ registers it opening nothing, after a warning | "Your name's links": one link field; "Skip for now" leaves it empty | "Opens" row, the address or channel it started from, with ✕ and the warning | As sketch |
| Terms (1·3, 1·2a) | Own screen, once per device | Tick on each order; "Community terms" step for community names | The site's | Each order signs the terms it was placed under, which can change; community names came after the sketches |
| Recovery key (1·4 to 1·5) | Right before the first payment or code; a switch for later; then hidden until Reveal; then the words | "Save your name code" before Check your order | The same, after Pay on the first order | As sketch: no key before a name is chosen |
| Paying (1·6) | The store's sheet; a code skips it | Card, Bitcoin, Monero or a code | The store's sheet; a code skips it | As sketch |
| Registering (1·7) | Committing · Waiting 60s · Registering | Three stages with bars, on a page that can be closed | Committed · Waiting, with a countdown · Registering; the app runs each step, so it stays open, and carries on when reopened | As sketch |
| Done (1·8, 1·9) | An alert: "The name is yours", until its date, Add to calendar; a key reminder once; the address shows it | "… is yours" | A screen with the same facts, the reminder, then the address | A screen, as the site: outcomes stay in place. Add to calendar not drawn |
| Taken, failed | "Name already taken", payment kept; "Paid, not registered", Try again | The same outcomes | The same; a refund contact added | As both; a store payment has no balance to refund on the site's page |
| A code (row 2) | "I have a code" on the search and "Use a code" on the order open "Your code" | "I have a code" | The same, opening Redeem code, as the badges | Same steps; the badge screen's look; "Get a code on the web" is gone, as no codes are sold on the web |
| Can't pay (row 4) | Pay with a code; codes sold by length on the web | Sells names; no codes sold on the web | Search in the app, then Buy in browser with the name | Codes aren't sold on the web; search in the app, by decision |
| A site-bought name (row 3) | "I already own a name": shown on the profile | Its name code edits it | Show it on the profile, or move it in with its name code | New: moved in, the recovery key holds it |
| SimpleX names (row 5) | Among the main settings; your names, Register a name, Import; the key row orange until written down | | Under Advanced settings; the rest as the sketch, plus Restore purchases, and the device's passcode before Reveal from the hub | The settings place, asked in review; the stores require Restore purchases |
| Restore (row 6) | Import, 24 words in one field, looking, found, Look further; a found name "still points" at the old phone's address; nothing found keeps the key | | As the sketch, but a found name attaches to this phone's address, and a key that finds nothing isn't kept | The name attaches, by decision; the key is question Q1 |
| Older database, migrating | "Look for your names again" before registering; migrating needs no restore | | The same, in words | As sketch |

Also from the site: the order's rows and the terms tick; the store's sheet in place of its payment screens, with no "Pay with ETH"; no transaction rows or block explorer links, since opening an explorer would show the user's IP address with the name to a third party; no "Use in the app" card, name code or QR on 1·8, since the name is already in the app's wallet; no "Being registered" on the search answer, since only the service knows an order in progress, and asking it for each searched name would tell it what users search; the name looked up again right before the store sheet, since the app's order follows the payment; "Use a code" on Check your order takes the place of the site's "I have a code" there.

### 6.8 The browser path

For F-Droid and desktop always, for Google Play and iOS in the US (`badgeBrowserAllowed`, reused), and for every user who bought on the site.

- **"Buy in browser"** (the badge's words) opens `https://simplex.domains/#/?name=dynamism&link=<the address's or channel's short link>`. The site searches at once, as its own links do, and fills its links step from `link`, checking its kind and length as its field does. Both are in the fragment, so the page's server sees neither until the page asks. The site then runs its own steps, ending on "<name> is yours" (4·2).
- **The site's change** (simplex.domains): reading `link`; the card "Use in the app" and "Open in SimpleX", whose words replace "tap Set SimpleX name" with the current "Get SimpleX name"; on a computer, Copy.
- **The link back** is a verified https link the app owns where the platform has one (Android App Links, iOS universal links), `https://simplex.chat/name/code#<code>`, with the owner code in the fragment, routed beside the badge's `/badge/code/`. It doesn't use the custom `simplexchat:` scheme, which any Android app can register. The website's association files (`apple-app-site-association`, Android's asset links) and the Android manifest gain the path; the apps' link routers handle it before connection links; the page at that address is a file of its own, outside the website's shared layouts, loading no third-party script, clearing the code from the address before anything else and telling a visitor without the app to paste it. On desktop, where https links don't open the app, the site shows the code to paste.
- **Back in the app** a name shows on the profile as today, through Import's "Bought on the web", or moves in through 3·1.

### 6.9 iOS and Apple's test environment

iOS gets the same code behind a build-time switch, as the badge buttons are today, off in stage 1's release and turned on in the version submitted with the product; a switch at run time would be a dormant feature under guideline 2.5.19. While it is off, iOS is a build that can't pay (row 4): the US storefront links to the site, and elsewhere the search answers with "Register with code".

Every purchase made by App Review or a TestFlight tester is a test purchase, which the service refuses in production; crediting one would register a real `.simplex` name for nothing. So switching on needs one of:

- **Register test purchases as test names** (recommended): the reviewer completes the whole flow and receives `dynamism.testing`, shown as such; real payments register `.simplex`. Cost: a second, small deployment of the store service, set to the `testing` namespace, Apple's sandbox host and crediting test purchases, with the app sending a transaction marked sandbox to that deployment's address; since `credit_test_purchases` credits badges too, signed with an issuer key every app trusts, that deployment answers badge commands with `unsupported_version`, or the setting applies to name products only. Anyone can join SimpleX's public TestFlight and buy in the sandbox for nothing, so that deployment credits test purchases only while the operator has opened it for a build in review, registers a few names a day for the whole service, and refuses the rest.
- Refuse test purchases: App Review can't complete a purchase.
- Show the browser path to test builds only: Apple treats a hidden feature as grounds for removal (2.5.19).

Until iOS sells names, its code path relies on App Review accepting it, a risk the badge codes share: Apple lets an app use what was bought on the developer's site "provided those items are also available as in-app purchases within the app" (3.1.3(b)), and forbids "their own mechanisms to unlock content or functionality, such as license keys" (3.1.1). Switching on removes that risk (question Q5).

### 6.10 Stage 2 mechanics

- **Restore.** `/_wallet import` restores the wallet from the 24 words, refused where a wallet exists, the counter set past the accounts found. The scan walks the wallet's accounts in order and asks what each owns, over the new `ROWN` command (P4; the resolver's `/v2/owned-by/<address>`, P3; the agent's `ownedSimplexNames`, one account per relay), until 20 unused accounts in a row, at most 200, with a start index for "Look further", progress events and a stop. What it finds binds to the active profile: the words don't say which profile held a name. More than 8 names per address stays open from https://github.com/simplex-chat/simplex-chat/pull/7561.
- **Attach.** A name moved in or restored, when it opens something else, attaches to this profile's address or channel with one relayed record edit, signed by the account, within its yearly allowance. Redeem no longer needs the name to open this profile already.
- **Set what it opens** on the name's own screen, as the site's Edit links: what it opens now, its address and its channel ("Opens nothing yet" when neither); the offered address or channel of the screen it was opened from; "Replaces" or "Adds #Bakery Deals"; "Already opens Alice's address" with "Save" off; under "Save", "2 edits left this year." "Save" sends one relayed edit and reads "Signing and sending…" in place, then returns to the row. A failure (`record_reverted`, `rate_limited`): "The links were not saved.", "Try again in a moment."; offline, "No connection." with "Save" off; a lost answer or `edit_pending`: "No answer yet", "The change may still go through. Open this again in a few minutes to see whether it did before saving again.", "Save" off until the app reads the name again; the year's edits used: "You can set what it opens again on 31 Mar 2027."
- **Give to another profile:** the name's account is rebound to the other profile (a database change), and its record re-pointed at that profile's address (one relayed edit).

### 6.11 Stage 3 mechanics

- **Transfers to another person:** the same two relayed transactions as a move (approve and claim), counted in the same allowance, 3 a year per name. The receiving account accepts the terms current for that name (the terms draft's Transfer). During the grace period a name can't be transferred.
- **Renewal.** "Add a year" one-time products, sold from the name's own screen and from the expiry warning, available to the owner during the term and the grace period; in desktop, F-Droid and the iOS US storefront, "Renew in browser" once the site sells it. The contract fixes the shape, as the names work read it in ens-contracts `simplex`:
  - a renewal is at least 365 days (`MIN_RENEWAL_DURATION`, refused at `_renewCore`), so a product adds whole years and a month can't be sold;
  - a renewal leaves the expiry at most one horizon ahead of the block: 1095 days for a paid renewal and for a sponsored community name, 4015 days for any other sponsored name (`_sponsoredHorizon`);
  - a reserved name is refused (`NameReserved`);
  - `renewWithCredit(label, duration, referrer)` takes no owner signature and spends the registrar's allowance, so the service checks that the asking wallet holds the name, and that the renewal stays within those bounds, before crediting; a renewal it can't send is refunded through support; the renewal screen carries the terms line, since new terms apply from a renewal.
- **Reminders.** The row and the hub show "Until 31 Dec 2028" from stage 1, from the expiry stored beside each wallet account, refreshed whenever the name is verified, since anyone can renew a name. In stage 3, from 30 days before: the row turns to "Expires in 21 days"; a chat-list banner with a dismiss control, dismissible once a week and above promotional banners in the last 7 days; local notifications at 30 and 7 days. Reminders belong to the profile that owns the name: the row, the hub and the banner show only the active profile's names; a notification is scheduled only for a profile whose notifications are on and removed when it is hidden or they are turned off; a hidden profile's names raise none. Notifications follow the app's preview setting. A "Renew" button appears only where a renewal can be bought; until then the warning says "Renewing isn't available in this version yet." In the grace period the rows read "Expired 31 Dec 2028", and the alert "Your name dynamism.simplex expired on 31 Dec 2028. Renew it before 31 Mar 2029.", with Renew where it can be bought. After the grace, "Re-register". The chain decides expiry, so the app warns from the stored date, without the network.

### 6.12 Where badges and names meet

Nowhere: separate products, no discount, no shared purchase. A name never unlocks an app feature, and a badge never pays for a name. They share the store service and its receipt verifiers, and on iOS, App Store purchases of both carry the same Apple Account identifier (section 7). The badge owners review their threat model's wording on linking purchases when iOS badge purchases ship.

## 7. Compatibility and security

- **Protocol.** The store service's app protocol gains the name commands of 6.3 under one version bump, `currentStoreServiceVersion` to 2, so an app can tell a service that sells names from an older one. Chat protocol and agent are unchanged in stage 1. Stage 2 adds `ROWN` to SMP (P4, version 23).
- **Database.** The core gains the order and purchase records and, beside each wallet account, the name it holds and its expiry (SQLite and Postgres, P11, P15, P19). The service gains one column on prepaid codes, added with a named unique index as the payments migration does (SQLite can't add a unique column), a table of `receipt_used` presentations, and a row per UTC day counting the cap (both backends, P27).
- **Older apps** are unaffected: they never send the new commands.
- **Threat model.** This design accepts and states what follows; changing it is the lead's decision (question Q6).
  - *The stores* learn that the account bought "SimpleX name", registering one name for 2 years, when, at what price, and from which device and network; the app never sends them the name. The name is registered on the public chain minutes after the payment, by SimpleX's registrar key, so a store that compares its purchase times with the chain can find the name a purchase paid for, unless many names are registered in the same minutes. The app also consumes or finishes the purchase when its name registers, and the store learns that time, so a payment held for days is still matched to its name. So can anyone who sees the account's purchase history or receipts.
  - *The store service* learns the store transaction, and in the same request the name's hash and length, against which it can test any name it guesses, and the owner's wallet address; at the reveal, the name and what it opens. So SimpleX links each payment to its name, as on the site. One request fixes that link in the protocol for a payment made with its order; it adds nothing the service didn't see from two requests, and a payment alone stays possible (6.3). The service sends the terms text the app shows, but can't change what the user accepts: the app checks the text against the hash it signs (6.3). The receipt doesn't say who holds the store account, but with every receipt it gets the product and the buyer's store country (Apple's storefront, Play's billing region), and from Apple the price and currency.
  - *Google Play:* SimpleX as the developer can search Play Console's orders by a buyer's full email address, and each order's purchase token hashes to the service's payment key, so SimpleX alone can find the names, and the badges, a Google account paid for. Google's payments privacy notice says it shares the buyer's personal information with the developer on Play purchases, without listing the fields, so this design assumes Play Console shows who paid for each order; P27 checks which fields and states them here. `orders.get` also returns the buyer's address fields, which the service discards. A buyer who gives support the order number links that chat to the Google account that paid.
  - *The App Store:* every transaction carries Apple's identifier of the buyer's Apple Account in this app (`appTransactionId`), the same in every purchase that account makes, on any device; so on iOS the service can tell which names, and which App Store badge purchases, one Apple Account paid for, and which addresses those names point at. Apple's API returns every purchase of the same Apple Account. The service reads nothing else from a receipt: that is how the service is written, not something a user can check.
  - *The store and the service together:* the store keeps who paid, the service which name the payment registered, and the store's transaction or order number joins the two records, so whoever obtains both learns who registered the name.
  - *The store service as the registrar* is trusted to send the registration it is given. The app runs commit and reveal as separate steps, so the service gets the name only at the reveal. It can still test guesses against the name's hash from the order, so for a name it guesses it could register first, or let the commitment lapse; it can't register the name to another owner, since the intent is the owner's signature.
  - *Chain observers* see each name, its owner address and its records, the owner's acceptance of the terms, and that SimpleX's registrar registered it. One wallet account per name keeps two names of one person from sharing an address, but two names registered one after the other can be linked by their times. The history of where a name pointed stays on the chain after it changes. A move is public: from the site's wallet to the app's account.
  - *The service's chain node,* or a hosted RPC provider if one is configured, sees each ordered name's hash when the service reads the chain at the order.
  - *The search:* the app sends the name's hash over SMP with private routing, on by default, so the router that answers sees a hash it can test guesses against, not the device's address, and the service sees nothing of the search.
  - *Codes from SimpleX:* a code support credits or the operator issues links whoever received it to the name it registers.
  - *Network:* requests reach the service through SMP with private routing, so neither the service nor its router sees the device's IP address. The stores see the device and network at purchase.
  - *The browser path:* the site sees the browser's IP address, a card payment's processor sees the card holder, and the order links both to the name. "Buy in browser" also hands the browser the name and the short link of the address or channel: both stay in the browser's history, and when the site previews the link, the service sees that link with the browser's IP address (question Q7).
  - *The recovery key:* its 24 words control every name of every profile on the device. A user who backs up later and loses the device with no chat backup loses the names, and any held payment comes back only through support; 1·4 says so, and the key row and one reminder carry it. Reveal is deliberate, and from the hub it asks for the device's passcode. "Copy" puts the words on the clipboard, marked sensitive with an expiry where the platform allows. A database backup holds the seed, as the wallet does today.
  - *A store receipt* is a bearer credential until it is first presented: whoever presents it first binds the payment to a code derived from their own wallet. The app presents by itself only purchases its own wallet's token marks; a purchase with no token goes to whichever device restores it first. So another device or profile on the same store account doesn't take a payment that carries a token, even while the buyer is offline; only someone holding the receipt outside the app could, and they would be told nothing about the name. An unused store-paid code can also be moved to another wallet by the operator on a support request; support can't tell the buyer from anyone else holding the store account's receipts, so the 7-day rule of 6.3 applies, and the request links the asking profile to the payment.
  - *A forged or replayed receipt* is refused by the verifiers; one transaction funds one code (the unique payment); the years come from the product, never the client. The service looks the product up before asking the store, and one-time Play reads share the per-minute bound, so invented receipts can't use up the Play quota. An order inside a request is checked as every order is: a refused payment leaves it unread.
  - *A stolen code* before registration pays someone else's name; the code lives in the app's database, as wallet keys do. The service sees it when the app orders a name with it, in the request that pays it when a name waits.
  - *A refund after registration* returns the whole price, so repeating it costs the buyer nothing, while each costs SimpleX the name's chain price from the registration budget and the gas, and the buyer keeps the name for two years. Both stores grant refunds on the buyer's request, by their own rules. Bounded by the daily cap, enforced by the service; a refund abuser can use up that day's cap, so other store buyers then wait for the next day. A refund before registration isn't detected at launch either, within the same cap. On iOS the service could refuse repeat refunders by Apple's account identifier, but the design keeps no account identifier, so the cap is global.
  - *Apple test purchases* never register a `.simplex` name (6.9), and the test deployment credits no badge.
  - *Users attacking the service over SMP:* private routing hides the client's address, so per-address limits don't apply; each SMP command class has a service-wide per-minute bound, and a code order is checked for its code before any chain read. A flood makes other apps' requests of that class wait for the minute.
  - *The owner code:* whoever has it owns the name until the app has moved it, after the user confirms and two relayed transactions land; after the move it no longer controls the name, but it still opens the site's order (the name and its links) and owns any balance left on it. Where the app doesn't take the link back (a phone without the app), the code opens in the browser and stays in its history; the page at that address loads no third-party script and clears the code from the address before anything else. A stronger form, for later: "Buy in browser" passes a one-time public key the app made, the site seals the owner code to it, and the link back carries only the sealed code.
  - *Notifications* about expiry (stage 3) follow the app's preview setting, so a hidden preview never shows the name on the lock screen, and a hidden profile's names raise none.
  - Not done: blind-signed codes. The service would sign a code against a payment without seeing it, so it couldn't tell which payment, and so which store account, paid for which name; this holds only if the name is also registered at a time unrelated to the payment, since the service and the store see both times. It would use the payment-alone form of the request. Listed as later work, together with that delay.
- **Hidden profiles** can't buy or register a name, and the canvases keep them out. Open store purchases are matched against every profile's records and continue only in the profile that made them; a held payment is offered only there (a hidden profile's waits until that profile is shown again); "Restore purchases" answers another profile's purchase and another wallet's with the same line; a purchase no profile records (a reinstall) is offered in the active profile, never a hidden one. In a shown profile, whatever another profile holds gets only the store-account line (6.5), which hides which profile made a purchase only as far as another device or wallet could have made it: on a store account's only SimpleX device, it can only mean a profile here. Every profile can also see whether the device has a recovery key, and those 24 words recover every profile's names, hidden ones included.

## 8. Pull requests

Each row is one pull request. Size is the diff a reviewer reads, apart from moved lines. Risk marks signing, money, the database, transfers and protocol as high. Each core pull request carries the CLI form of its command and a terminal test (D8). A rough estimate of implementing, building and testing, at this container's pace: stage 1 about two weeks, stage 2 and stage 3 about one week each. Slices of one area share a stream, and a pull request follows the ones listed before it.

### Stage 1

| # | Pull request | Size | Risk |
|---|---|---|---|
| P1 | simplexmq: `eth: the EIP-191 message digest`. `Simplex.Messaging.Eth.EIP191`, moved from the store service's `personalDigest`, with viem vectors | small | high |
| P2 | simplexmq: `resolver: the terms of an available name`. The resolver reads the name policy's terms link and the community list and fills `nameTerms`; routers that resolve names run a release with it | medium | low |
| P5 | `names: move the registration encoding into the library`. `Simplex.Chat.Names.Registration`: the ABI encoding, the commitment, the `commit` and `registerWithCredit` calldata, the `RegisterName` intent; the service imports it | move | low |
| P6 | `names: move the order messages into the library`. `Simplex.Chat.Names.Order`: the order, `find`, another-name and spend messages, and the signature format | move | low |
| P7 | `names: the name codes in the library`. `NameCode` with both kinds, its hash, the wallet a name code derives, the constructor deriving a prepaid code from a seed and a store reference, and the recognising token, with vectors; the wallet RFC's section and the crypto registry's info strings | small | high |
| P8 | `wallet: sign a message with an account`. `/_wallet sign message`, EIP-191, 65 bytes with a low `s` | small | high |
| P9 | `wallet: sign typed data with an account`. The `RegisterName` intent, the record-edit intent and the transfer intents, EIP-712 | small | high |
| P11 | `wallet: the names an account holds`. The migration, `/_wallet names`, and the expiry refreshed whenever the name is verified | medium | high |
| P13 | `names: the store service's name commands`. The request and response types, the two-part purchase answer and the version bump; types only | medium | high |
| P14 | `names: the catalog and the terms text`. `/_name catalog`, the clock offset, and the terms text checked against its link and its hash | medium | high |
| P15 | `names: order a name with a prepaid code`. The order record and its migration, the secret, the commitment, the signed order, `/_name order` | large | high |
| P16 | `names: follow an order`. The worker, the poll, the reveal with the terms acceptance, progress events, recovery after a restart, and stopping | large | high |
| P17 | `names: Try again, another name, and the kept payment` | medium | high |
| P18 | `names: claim a registered name`. The address and the channel claim it once the name is registered; a name with no target skips it | small | low |
| P19 | `names: pay with a store receipt, in one request`. The purchase record, the code derived from the receipt, the recognising token, the signed order kept and sent with the receipt, resending after a lost answer, the size check, and the answers to a used or a revoked receipt | large | high |
| P20 | `names: import a name code`. The code's wallet, the names it holds read with `find` and the chain, the transfer by `approveWithSig` and `claimWithSig`, only for a name that opens this profile's address or channel | large | high |
| P22 | `store: names over SMP — the catalog and the code check`. The dispatch, the names lane, the per-command bound, the version | medium | high |
| P23 | `store: names over SMP — the code order and the view` | medium | high |
| P24 | `store: names over SMP — the reveal, Try again, find and another name` | medium | high |
| P26 | `store: names over SMP — the name code's approve and claim`, with their yearly bound and chain reads | medium | high |
| P27 | `store: a name code bought in a store`. The product, the purchase command with its optional order, the payment column, the presentations table, `sale_closed` for store-paid codes, the daily cap, and which buyer details Play Console shows | large | high |
| P28 | `store: the operator command for a store payment`. Show, revoke, refund, move | medium | high |
| P47 | simplex.domains: `webapp: the link parameter and Open in SimpleX`. Reading `link`, the card "Use in the app", and the button | small | low |
| P29 | `ui: the names hub`. The Advanced settings row, the empty and full hub, the names owned, the entry points of the canvas, the key row, Restore purchases' row, and Import with "Bought on the web" | medium | low |
| P30 | `ui: search a name`. The field, the checks made while typing, and the answer's states, with "Buy in browser" and its link where the build allows | medium | low |
| P32 | `ui: Check your order`. The card with its Opens row and ✕, the term, the terms line, "Use a code", and the store's line | medium | low |
| P33 | `ui: registering, done, taken and failed`. One screen with its states, and "Still registering" | large | low |
| P34 | `ui: Redeem code, and Redeem a name`. The field, the scanner, the verified link back (with the website's association files and page), and the move's progress; in every build | large | high |
| P35a | `ui: the recovery key`. Set up with the switch, hidden until Reveal, the words, the orange row, and the one reminder | medium | high |
| P36a | `ui: the expiry date` on the name's row | small | low |
| P37 | `android: the Play purchase lane`. The product, the sheet, pending purchases, the store-account lines, Restore purchases, acknowledge and consume | medium | high |
| P38, P39, P41–P45 | The screens of P29, P30 and P32 to P36a in Swift, in the same order and with the same states | — | low |
| P46 | `ios: the App Store purchase lane`, behind the build-time switch | medium | high |

The chain operation M13 (`setPrices`, one base price) is the operator's, before the sale.

### Stage 2

| # | Pull request | Size | Risk |
|---|---|---|---|
| P3 | simplexmq: `resolver: the names an address owns`. `/v2/owned-by/<address>`, with its tests | medium | low |
| P4 | simplexmq: `smp: ROWN, the names an address owns`. The command, the server, the client, the agent's `ownedSimplexNames`, one account per relay, version 23 | large | high |
| P10 | `wallet: restore from the recovery phrase`. `/_wallet import`, refused where a wallet exists, the counter set past the accounts found | medium | high |
| P12 | `wallet: find the names a phrase owns`. The scan over ROWN, with a gap limit, a start index, progress events and a stop | large | high |
| P21 | `names: set what a name opens`. The relayed record edit, and the edits left this year | medium | high |
| P25 | `store: names over SMP — the record edit` | small | high |
| P48 | `names: attach a name on move or restore`. One relayed edit to this profile's address or channel, and Redeem for a name that opens something else | small | high |
| P49 | `names: give a name to another profile`. The account rebound, the record re-pointed | small | high |
| P35b | `ui: enter the 24 words`. The field, the scan with its progress, found and nothing found, and "Look for your names again" | large | high |
| P50 | `ui: set what it opens`, and giving a name to another profile, on the name's own screen | medium | low |
| P51 | The screens of P35b and P50 in Swift | — | low |

### Stage 3

| # | Pull request | Size | Risk |
|---|---|---|---|
| P52 | `store, names: transfers to another person`. The relay of a transfer, its allowance, the receiving account's acceptance | medium | high |
| P53 | `store: renew a name`. The renewal products, `renewWithCredit`, and the checks of 6.11 | medium | high |
| P54 | `names: renew a name`. The core command | medium | high |
| P36b | `ui: reminders`. The row's warning, the banner, and the notifications per profile | medium | low |
| P55 | `ui: transfer and renew`. In Kotlin, then Swift | large | low |

And when iOS is switched on, whichever stage it falls in: P56, `store: the test-purchase deployment` (6.9), and the App Store product submitted with that version.

### Fall away, from the delivery plan's cut of 46

- P31 `ui: what the name opens` and its Swift mirror P40: the Opens row and ✕ are in P32; setting it later is P50.
- P20's following and revealing of an order placed on the site in the app: the site finishes its own orders.
- P20's pointing the records at this profile: P48, stage 2.
- The per-length products and checks that P27 and P19 would have held.

### Order

Three streams run at once:

| Stream | Pull requests |
|---|---|
| simplexmq | P1, P2 |
| library and service | P5, P6, P13, P22, P23, P24, P26, P27, P28 |
| core wallet | P7, P8, P9, P11 |

| Pull request | Preceded by |
|---|---|
| P14 | P2, P13 |
| P15 | P5, P6, P7, P8, P9, P13, P14, P22, P23 |
| P16 | P11, P15, P23, P24 |
| P17 | P16, P24 |
| P18 | P16 |
| P19 | P7, P16, P27 |
| P20 | P7, P9, P16, P26 |
| P29, P30 | P14, P15 |
| P32 | P19, P30 |
| P33 | P16, P17, P32 |
| P34 | P20 |
| P35a | P8 |
| P36a | P11 |
| P37 | P19, P32 |
| P38, P39, P41–P46 | The Kotlin pull request each mirrors |
| P12 | P4, P10 |
| P21, P48 | P9, P25 |
| P35b | P10, P12 |

The first end-to-end result follows P1, P2, P5, P6, P7, P8, P9, P11, P13, P14, P15, P16, P18, P22, P23 and P24: a CLI user with a prepaid code registers a name, and the address claims it.

Pace, from risk: P5 to P16 with one lean review after each high-risk one, since later work builds on them; P19 and P27 together, reviewed as money; P37 on its own; the screens batched, not compiled in the container, each checked on its app's runner workflow; one review loop over each stage at its end.

Console work for P37 and P46: the product, multi-quantity off, the display name and description of 6.2, review notes, Google's tokenized-asset marking and declaration; the product stays unavailable until codes-only is off; for iOS, it stays unsubmitted until the switch. Before the sale, no store promo codes are issued unless "Restore purchases" has shipped.

## 9. Test plan

- **Vectors,** against viem and the deployed ABIs: the commitment, the `commit` and `registerWithCredit` calldata, the `RegisterName` digest, the EIP-191 order digest, the wallet a name code derives, the derived prepaid code and the recognising token.
- **Service:** the request with an order answered in two parts; a refused order keeping the paid code, for each refusal of 6.3; the same request resent, at any age, answered by both parts, and answered `used` once the name registered; the same receipt with another hash answered `receipt_used`, its order unread, and the presentation recorded; a request whose order's code doesn't hash to `codeHash` crediting the payment and answering `bad_request`; a failure in the order part leaving the payment committed; a request with no order; a test purchase refused; a quantity of 2 refused; an unknown product refused before any store call; a 6-character and a 63-character name paid by the product; a paid receipt credited while codes-only is on, its order refused with `sale_closed` and nothing written, then placed as a plain code order and registered once codes-only is off; an order refused with `sale_closed` after the day's cap, placed the next UTC day, with the catalog closed meanwhile; a Try again and another name past the cap refused with `sale_closed`; a store-paid code typed into the site while codes-only is on refused; an unspent code revoked by the operator command refused at its order; concurrent orders at the cap, one accepted; a move refused before 7 days, for a used or revoked code, while a registration holds the code, and to an unrecorded hash; a refund refused when `orders.get` doesn't match the reference, a refund whose Play call fails retried, and a refunded receipt with no payment row refused later; the receiving device taking a moved code; after a move, the old wallet's failed registration unable to reserve the code again; a Try again whose register went through, made while the sale is closed, answered registered; `terms_unavailable` with nothing written, then placed; one signed under older terms answered `terms_changed` and placed under the new hash; terms changed between the commit and the reveal, the reveal waiting; a community name ordered with a 5-year code registered for 2 years; the owner-code relays' checks and bounds; a store-paid code's order refused `name_taken` before any commit, its code kept; another name refused `move_limit`, and the code released by the sweep 30 days after the failure, then paying a new order; the SMP commands' per-minute bounds, and an order with a used code refused before any chain read; the names lane holding up no badge request; the test-purchase deployment refusing badge commands.
- **Core:** the code derived from the wallet and the receipt's reference, and the token, against their vectors; search, Check your order and a cancelled sheet creating no wallet; the exact request resent after a lost answer and after a restart; a request larger than the bound sent as a payment, then a plain order; a stale timestamp signed again as a plain order; the terms changed while a paid order waited, nothing signed until the new tick; a paid order waiting across a restart, placed without a new tick when the terms are unchanged and only after one when they changed; terms changed between "Redeem" and a resumed claim (when P26 finds the claim records an acceptance); a name listed as a community name between the commit and the reveal, signing nothing until the community tick; a terms text whose sha256 or link doesn't match the signed hash shown as unavailable; a restart in each phase; "Cancel" leaving the order unrevealed, reading failed past `revealBefore`, then paying another name; a registration past 24 hours reading failed and Try again ordering again; a promo-code purchase presented through "Restore purchases" with no name waiting; a purchase with another wallet's token never presented; a tokenless purchase restored on a second device after the first presented it, answered `receipt_used`; an open purchase answered `used` consumed and its name found; a refunded purchase dropped from the app's records; hidden profiles refused, with their open and pending purchases showing elsewhere only as the store-account line; a name code imported and its name moved; a name that opens another address refused in stage 1.
- **Wallet:** a signature against its vector; in stage 2, the phrase restored, the counter set past the accounts found, the scan's gap limit and its start index, and a reinstall with the restored wallet recovering an open purchase by its token, without "Restore purchases", and a registration under way.
- **Core and service end to end,** with the fake Play and App Store verifiers and the fake chain node: AC1 and AC5; a store receipt through to registered; a name taken between the search and the order; taken then another name; failed then Try again; a lost answer; a name registered with a prepaid code.
- **Apps:** previews for each screen of 6.6 and its states, the store-account lines and the hub's rows, in light and dark, the shortest and longest names, the smallest and largest widths, the largest text size, including 1·7 and 3·1 with a 63-character name; iOS storefront US and not US; Google Play US and not US; no list layout changes, so one layout. They are snapshot scenes, as the repo's are (iOS `previewScenes()` in `SimpleX Snapshots`; Kotlin `SnapshotTest.kt` up to the largest font scale), references committed, with a snapshot test asserting that the main button's, the stages' and the card's frames are equal across 1·7's states, and the search answer's button frame across its states, at the default and the largest text size. The Android lane against the fake verifier. A sandbox run on iOS covers 6.5's undocumented cases.
- **Regeneration:** the schema dump, the query plans and the API docs. Postgres builds and tests run on the host.

## 10. Risks

- **Apple's product type** is settled before the product is made (question Q8). Apple doesn't say whether the same non-renewing product can be bought while an earlier one is unfinished; the app offers the held payment before a second sheet anyway.
- **One product, one open purchase:** on Play, an unused purchase in another profile or on another device blocks buying on that account until it is used or refunded. The store-account line offers support.
- **Apple's open purchases after a reinstall:** Apple doesn't document that unfinished transactions reach a reinstalled app or another device; the app reads both lists, and a sandbox test settles it.
- **Back up later:** a user who skips the words and loses the device loses their names; in stage 1 even the words don't bring them back until stage 2 ships, though they keep owning them.
- **The wallet's known limit 5** (a database restored from an older backup can hand out an account already in use) is reachable by a purchase before stage 2's scan: two names then share an owner address, which links them publicly. Stage 2's "Look for your names again" closes it.
- **One price on chain:** the store spec's launch list has three. Until `setPrices` holds one, a short name costs the registration budget more than the store takes.
- **The contract source** isn't public. A 729-day registration refused is added to the store's end-to-end tests against the contracts; the renewal rules are rechecked when stage 3 is built.
- **Store policy on blockchain items.** Apple allows in-app purchase "to sell and sell services related to non-fungible tokens (NFTs), such as minting, listing, and transferring", and lets users view their own "provided that NFT ownership does not unlock features or functionality within the app" (3.1.1). Google requires the product to be marked as a Tokenized Digital Asset and declared. Whether an app holding the keys that own names counts as a "software wallet" under Google's cryptocurrency policy is not stated in Google's pages; the declaration form offers "my app is a non-custodial software wallet". It applies to the wallet whether or not names are sold, and is for the names work to settle before release.
- **iOS's code path before iOS sells names** (6.9): an App Review risk under 3.1.3(b), the same one the badge codes carry. Apple also asks for a restore mechanism for restorable purchases, which the hub's "Restore purchases" gives.
- **Refund abuse** after registration: bounded by the daily cap and watched by hand (section 7).
- **Linking by time** (section 7): accepted and stated at launch (question Q6).

## 11. Open questions, for the lead

Each carries the recommended answer this plan uses.

- **Q1. On a new phone, keep a recovery key that finds no names?** Restore comes in stage 2. In the sketches, a key that finds nothing stays as the phone's key from then on, and a phone refuses a second key. Recommended: no, ask for another key, since a wrong key, kept, would block the right one. If yes: a key with no names yet can be used from the start.
- **Q2. "Move" or "transfer" for your own name coming into the app?** On chain both are the same ownership transfer. Recommended: "move" for your own name, "transfer" to someone else, so people can tell the two apart. If "transfer" for both: one word, as on chain.
- **Q3. Does the app say buyers must be 18 or older?** The terms draft says you must be at least 18 to register or receive a name; the site's tick doesn't mention age. Apple's Ask to Buy lets a parent approve a child's purchase, and 1·7 treats it as an ordinary wait. Recommended: no, the terms say it, as on the site. If yes: the terms line reads "I am 18 or older and agree to…", on 3·1 too, and on the site.
- **Q4. Purchase terms at checkout?** The terms draft says it covers the use of names, not their purchase; the site shows no purchase terms. Recommended: as on the site, none of the app's own at launch: a store purchase is under the store's terms, shown in its sheet, and refunds follow the stores' rules. If wanted: a link beside the terms line, on the site too.
- **Q5. When iOS store payment is switched on.** Recommended: once a sandbox run passes, with Apple's test purchases registering a few test names a day through a second, small service deployment (6.9). If sooner, without it: App Review can't complete a purchase.
- **Q6. Linking a payment to its name.** Recommended: register at once and say so on the screen that pays, as 6.6's store line does, with section 7's statements. If the link should be weakened: delay each registration by a random time or register in batches, and consume or finish purchases at times unrelated to their registrations, so a store must guess among the names of an hour or a day (buyers wait longer, and more names are taken meanwhile); blind-signed codes then also unlink the payment from the name for SimpleX.
- **Q7. The link in the site's address.** "Buy in browser" carries the address's short link so the site fills it in, which shows that link with the browser's IP address to the service when the site previews it. Recommended: keep it, previewing only when the buyer reaches the links step, as with a pasted link. If not: the buyer pastes the link on the site.
- **Q8. Apple product type.** Recommended: non-renewing subscription, Apple's type for "a service with a limited duration", which a two-year registration is; the duties Apple gives that type (tracking the period, restoring it on the user's devices) are met by the chain and the recovery key. If consumable instead ("used once, after which it becomes depleted"): the same service path, and the risk that App Review reads a time-limited service sold as a consumable.

## 12. Out of scope (noticed, not doing)

- The iOS badge screen opens the badge site in every storefront, while its own comment says the link out is US-only (`BadgesSupportSimplexView.swift:71-78` against `BadgeStore.swift:39-41`): for the badge owners.
- Google Play's pending purchases are paid later (for example in cash at a shop), yet the badge screens, and the name screens after them, title them "Waiting for approval": for the badge owners, to change both together.
- A reminder before an unfinished order's 24 hours end is not designed.
- "Add to calendar", which the sketch draws on the name's success, is not drawn.
