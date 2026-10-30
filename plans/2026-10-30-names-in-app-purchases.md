# Selling SimpleX names in the apps

Base: master · Repo: simplex-chat · Builds on the store service (https://github.com/simplex-chat/simplex-chat/pull/7809), the device wallet (https://github.com/simplex-chat/simplex-chat/pull/7703) and registration from the app with a code (https://github.com/simplex-chat/simplex-chat/pull/7530).

## Summary

- **What exists.** A name is registered on Ethereum to a key its buyer holds; the store service commits and registers it, paying the gas from its own key and the price from an allowance. simplex.domains sells names for card, Bitcoin and Monero, and accepts prepaid codes; the apps show, verify and set names but cannot register one. Nothing sells a name through the App Store or Google Play.
- **Two facts the brief did not have.** The contract refuses a registration shorter than two years, and the site sells one fixed term, two years from 1 January 2027: the buyer does not choose a duration. So one, three or twelve months can never be a first registration; shorter steps can only be renewals, which nothing builds yet.
- **Recommended: one-time store products, one per name length, each a two-year registration.** The app derives a prepaid name code, the store takes the money, the service records the payment against the code's hash, and the app registers the name with that code exactly as it would with any prepaid code, such as an investor's. Registering from the app with a code doesn't exist yet: the names work builds it first, on the store's order path (section 10, slice 0). Nothing renews by itself; a renewal is another one-time purchase, built before the first names expire at the end of 2028.
- **Android sells at launch. iOS keeps the badge pattern** (the site in the US storefront, a code everywhere) until purchases made in Apple's test environment are handled; this plan's suggestion for that is to register them in the free `.testing` namespace. Desktop and F-Droid builds link to the site and take a code.
- **Ruled out:** auto-renewing subscriptions (one renewing name per store account, a year fronted on every first purchase, and every subscription edge case badges met); Google Play prepaid plans (one per account, at most a year); the site alone (most store users could not be sent to it).

## 1. Goal and acceptance criteria

The goal: a person in a store build of the app registers a SimpleX name for their address or channel, paying with the store, without a wallet, ether or a browser; and every build has a working way to bring in a name bought on the site.

- AC1: In the Android Play build, a user with no wallet buys a name of 8 or more characters for two years with Play Billing and ends with the name registered to a key of the device wallet, pointing at their address, which shows it as verified. Test: an end-to-end test with the store service, the fake Play verifier and the fake chain node.
- AC2: A payment is never credited twice and never lost: the same receipt sent twice gives one code; a receipt whose answer was lost is sent again and gives the same code; a name taken between search and registration keeps the payment for another name. Tests: service tests for each case.
- AC3: Neither the app nor the service can turn a purchase of one length tier into a name of a shorter length: the tier comes from the store product, never from the client. Test: a service test with a 7-letter name and an 8-letter product.
- AC4: In the iOS App Store build, the purchase button is not shown until store purchases are switched on; the US storefront shows the site link and every storefront shows code entry. Test: previews of both storefront states.
- AC5: A name bought on the site reaches the app: its owner code, typed or arriving by deep link, moves the name to the device wallet. Test: an end-to-end test with the owner code flow and the fake chain node.
- AC6: The app never opens the store sheet while the service is in codes-only mode or before the sale opens, and a receipt that was paid is never refused for a mode. Test: the app's catalog states in previews, and a service test crediting a paid receipt while codes-only is on.

## 2. Non-goals

- Renewal purchases: designed here (section 6.7), built in a later run before the first names expire.
- Subscriptions of any kind.
- Moving one name to another profile, device or person (section 8.1).
- Paying with cryptocurrency inside a store build.
- Changing the contracts, the site's pages (except the "Open in SimpleX" link of section 7) or the badge code.
- Blind-signed codes that would hide which payment bought which name (section 9).

## 3. Current state

Facts below were read at these commits of simplex-chat/simplex-chat: master `d4ca6c2284caa82dfcf7ef5159683c6384ca3e14`, the store service branch `sh/store-service` `c532b71bb816bfa2649d8bfcb0b3ac407da13995`, the store spec branch `sh/names-design` `158f22c7ea09afe1c594af5a5309f6cff50f7cbe`, the names branch `names` `6d80f6fa917f0efef5533e7cf7c33c8d1b84e76c`.

### 3.1 What a name is

- A record in an ENS fork on Ethereum; `.testing` and `.simplex` are separate deployments of the same contracts (master `docs/protocol/names-overview.md`, "Differences from ENS").
- Only the owner key can change or transfer a name. The service registers a name to the buyer's address through the sponsor contract's `registerWithCredit`, which needs the owner's signed intent, duration included; the registrar key pays the gas and the sponsor's allowance pays the price, so no ether moves for the buyer (`sh/names-design` `plans/names-codes/2026-10-09-name-codes.md` §5–6).
- **At least two years.** "The controller refuses a registration under 730 days." (`sh/store-service` `apps/simplex-store-service/src/StoreService/Config.hs:288-290`, `minNameYears = 2`; the service refuses to start with fewer, `:608-609`). The contract source is not in this repository; the figure comes from the service code and the store's API contract (`plans/names-codes/2026-10-21-store-api.md:91`).
- Paid years start on 1 January 2027; the days before are free (`Config.hs:300-301`; store API "Term").
- Prices come from the chain, by the name's length in characters (letters, digits and hyphens): $100 for 8 or more, $200 for 7, $400 for 6, for two years; no name is shorter than 6 (store API "Prices").
- After expiry, a grace period (90 days in the spec) when only the owner can renew; then anyone can register the name (`StoreService/Names/Contracts.hs:95-105`).
- `renewWithCredit` exists on the sponsor and needs no owner signature (`plans/names-codes/2026-10-09-names-web-store-plan.md:702`). Nothing in the service renews names. Whether a renewal has a minimum duration could not be checked here (contract source absent).

### 3.2 How a name is bought on the site today

- simplex.domains, served by the store service (`2026-10-09-name-codes.md` §2). One term: the search answer carries `years`, 2 at launch, and the order's duration must match it (`2026-10-21-store-api.md` "Term"; `StoreService/Names/Orders.hs:267-272`). A prepaid code carries its own years, 2 to 10 (`StoreService/Names/Codes.hs:153-156`).
- Payment: card (Stripe), Bitcoin or Monero (BTCPay), or a prepaid code `SN<length>-…` that only the operator issues. No codes are sold on the web (`2026-10-09-name-codes.md` §7, §14).
- The browser makes a fresh wallet for each order; its secret is the owner code `SN-` and 30 characters, and the name is registered to that wallet. "Redeeming the code in the app moves the name to the app's wallet" — designed, not built: the code's wallet approves and the app's wallet claims, both relayed by the service (`2026-10-09-name-codes.md:231-238`).
- The store spec leaves out "the web flow opened from the SimpleX app … dropped (owner, 2026-10-21). The app will buy names itself", "selling codes on the web" and "renewals, transfers, in-app purchase" (`2026-10-09-name-codes.md:80-83`).
- Before the full launch the site can run codes-only, refusing paid orders (`[names] codes_only`, store API "Codes-only mode").

### 3.3 The store service

- The app reaches the service over SMP, as a chat bot (`src/Simplex/Chat/StoreService.hs:77-106`), with badge commands only. Names are served to the web page over HTTP only. No branch has a store purchase path for names.
- Store receipts are verified for badges: Apple signed transactions against a configured root, Play purchases through the Play Developer API; the product decides what is granted, never the client (`StoreService/Catalog.hs:140-160`). `payments(provider, provider_ref)` is unique, so one transaction funds one purchase. Purchases made in Apple's test environment are refused unless a dev setting allows them beside the test host.
- Play one-time purchases are acknowledged by the service before crediting (`StoreService/StoreReceipts/Google.hs:96-102`), also by the Android app once its core holds the receipt, so Play never refunds it unacknowledged (master `BadgeStore.kt:293-295`), and consumed by the app later (`BadgeStore.kt:47-49`).
- The badge site's codes are made in the browser; the service stores only their hash (master `apps/simplex-badge-service/web/src/codes.ts:67-76`).
- Prepaid name codes: one row per code hash with the shortest length it covers, its years and a payment state; reserved while a registration holds it, spent when the name lands, released when the name is taken, kept 30 days for Try again after a failure (`StoreService/Names/Codes.hs`; `2026-10-09-name-codes.md` §6–7). `issueNameCode` and `revokeNameCode` exist for the operator (`Codes.hs:152-167`).

### 3.4 The core and the apps

- The core (on `names` and `sh/store-service`) has the device wallet: one 24-word master per device, one account per name, each bound to a profile (`docs/rfcs/2026-09-10-wallet-keys.md`). It cannot sign yet: signing is "a command here that returns a signature", not built; simplexmq already has recoverable signing and EIP-712 hashing.
- The core makes and reads prepaid name codes: `randomNamePrepaidCode` and `namePrepaidCodeHash` are in `src/Simplex/Chat/Badges/Code.hs` (`sh/store-service`).
- Registration from the app with a code is planned in https://github.com/simplex-chat/simplex-chat/pull/7530 (`plans/2026-09-28-name-registration-core-cli.md` on `ab/names-actions-api`): a worker per user commits and reveals through the service and claims the name on success. Not built, and planned before the store service: no owner signature, credits held by a purchase key, codes with their years in the text. The store's code order is owner-signed and served over HTTP only (`StoreService/Web/Names/Orders.hs:192`, `Names/Signatures.hs:95-113`), so the app's path to it is re-planned as slice 0 (section 10).
- The apps on master set, show and verify names; the editor shows a countdown to the sale, 12 December 2026 18:00 UTC (iOS `UserAddressView.swift:818, 921-935`; Kotlin `SetSimplexNameView.kt:31-34, 194-222`). On `names`, a lapsed own name raises "Your name %1$@ expired on %2$@. Renew it before %3$@." with Renew, and an available one "available for registration for %2$@" with Register; both open `https://simplex.domains/#testing`. Nothing warns before expiry, and the core stores no expiry.
- The badge buy pattern (master): on Android the Play build sells in the app and shows "Buy in browser" only for the Play country "US", else "Redeem badge code"; FOSS and desktop show "Buy in browser" and "Redeem badge code" (`BadgesSupportSimplexView.kt:35-89`, `BadgeStore.kt:61-67`). On iOS store purchases are commented out; the screen shows "Choose your badge", which opens the badge site in every storefront, and "Redeem code" (`BadgesSupportSimplexView.swift:48-78`); the US-only check `badgeBrowserAllowed` exists but is unused there (`BadgeStore.swift:37-41`). The site returns with `simplexchat:/badge/code/<code>`, which the app confirms before redeeming (`ContentView.swift:456-522`, `BadgesRedeemCodeView.swift:250-438`).
- The phase 3a canvases on `ab/names-actions-api` already draw registration in the app: a store build pays "once for 2 years … Nothing renews automatically" in the platform's own sheet; desktop and F-Droid register with a code; a taken name keeps the payment; a paid, unfinished registration offers Try again (`plans/sketches/2026-09-18-names-phase3a-registration.excalidraw`). This plan's screens follow them.

### 3.5 Where the brief and the code differ

- The brief: "on the site a buyer chooses how long they register a name for". The code: one term, two years, set by the service.
- The brief: one, three or twelve months as a first offer. The contract: two years at least.
- The badge write-up: the iOS "buy in browser" link shows only where the storefront allows it. Master: the iOS badge screen's site button shows in every storefront; the storefront check is in the commented-out block. Names follow the storefront rule (section 7); the badge screen is listed under section 14.

### 3.6 Open pull requests at this plan's entry points

| Pull request | Relation |
|---|---|
| https://github.com/simplex-chat/simplex-chat/pull/7809 store service | extended: a name purchase command, the names catalog |
| https://github.com/simplex-chat/simplex-chat/pull/7675 store spec | followed: codes, terms, phases |
| https://github.com/simplex-chat/simplex-chat/pull/7889 names under the name terms | followed: the app signs the terms version as the page does |
| https://github.com/simplex-chat/simplex-chat/pull/7703 names feature branch | the wallet the app registers to |
| https://github.com/simplex-chat/simplex-chat/pull/7693 registration primitives | used by the service |
| https://github.com/simplex-chat/simplex-chat/pull/7530 registration from the app | its design is replaced by the store's code order; re-planned as slice 0 |
| https://github.com/simplex-chat/simplex-chat/pull/7566, https://github.com/simplex-chat/simplex-chat/pull/7561 wallet model and scan | unchanged |
| https://github.com/simplex-chat/simplex-chat/pull/7390, https://github.com/simplex-chat/simplex-chat/pull/7425, https://github.com/simplex-chat/simplex-chat/pull/7428, https://github.com/simplex-chat/simplex-chat/pull/7355, https://github.com/simplex-chat/simplex-chat/pull/7382 earlier names designs | their choice of one-time purchases is kept; blind-signed codes are left out (section 9) |
| https://github.com/simplex-chat/simplex-chat/pull/7932 app test backend | used by the app tests |

## 4. The shapes of the sale

Each shape is described under the same headings, so each can be judged alone. "Tier" means the name-length price class: 6 characters, 7, 8 or more.

### 4.A Auto-renewing subscription

- **Products first.** Apple: a subscription group "SimpleX names", separate from the badge group, with one yearly product per tier (`NAME_SUB_8_YEAR_01`, `NAME_SUB_7_YEAR_01`, `NAME_SUB_6_YEAR_01`); a year is the longest period either store offers. Play: one subscription per tier with one yearly auto-renewing base plan. The two-year contract floor cannot be a store period: the first purchase pays a year and the service registers two, fronting the second from its allowance.
- **Consoles.** App Store Connect: a group, three products, review notes. Play Console: three subscriptions, a yearly base plan each, Resubscribe off, Pause off (the badge lessons), grace period and account hold set deliberately.
- **More than one name.** Apple: "people can only buy one subscription within a group at a time", and several subscriptions need several groups. Play documents no way to hold one subscription product twice and tells apps to check that the user is not already subscribed. So a store account can renew one name per set of products; a second renewing name needs a second group and a second set of products, a "slot" per name.
- **Service verifies and stores.** As for badge subscriptions: the original transaction or purchase token, its state on every renewal (App Store Server API, Play `subscriptionsv2`), at most hourly; plus what badges lack: which name each subscription renews, and a renewal sender that calls `renewWithCredit` once per paid period.
- **The user sees.** "Register for $X a year · renews until you cancel"; managing and cancelling in the store's settings; on cancel, the name stays until its on-chain expiry, at least a year after the last payment because of the fronted year.
- **Renewal, refund, expiry.** Renewal is automatic; billing retry and the store's grace period delay it; a failed renewal stops extending and the name lapses at its expiry plus the 90-day grace. A refund cannot take an extension back. Changing tier means nothing for a name whose length is fixed, so upgrades must be blocked. A name released after its grace can be re-registered by anyone, its old subscription still billing unless cancelled.
- **Reaching the registration.** The first verified charge credits a registration of two years; each later charge sends one renewal.
- **iOS.** The same test-environment problem as every store shape (section 6.8).
- **Cost.** The whole badge subscription machinery again (renewal checks, billing retry, pause, resubscribe, new Play tokens, plan changes), plus name binding, slots, a renewal sender, and a year of allowance per first purchase. Store commission on every renewal; one renewal transaction's gas per name per year.

### 4.B Fixed term that does not renew, tracked by the store

- **Products first.** Apple: a non-renewing subscription per tier and term (`NAME_8_2Y_NR_01` …); Apple does not track its duration, the app does. Play: a prepaid base plan on one subscription product per tier; a prepaid plan grants a period without renewing and can be topped up, but its period is at most a year, so two years needs a year plus a top-up, or a fronted year.
- **Consoles.** App Store Connect: three non-renewing products. Play Console: three subscriptions with a prepaid yearly base plan each.
- **More than one name.** Apple: a non-renewing product is renewed "by purchasing the same product again"; Apple says nothing about holding several at once, and `Transaction.currentEntitlements` shows only the latest per product. Play: a further purchase of a prepaid plan is a top-up that extends the same subscription ("only one unconsumed top-up at a time"), so it cannot buy a second name.
- **Service verifies and stores.** Apple as for one-time products (section 4.C). Play: the subscription purchase, its prepaid expiry, and each top-up as an extension of one name per account.
- **The user sees.** As section 4.C on Apple; on Play, the second name of a tier has no product to buy.
- **Renewal, refund, expiry.** Renewal is a repeat purchase (Apple) or a top-up (Play); refunds and expiry as section 4.C.
- **Reaching the registration.** As section 4.C on Apple; on Play the first purchase of a year cannot pay a two-year registration alone.
- **iOS.** As every store shape.
- **Cost.** Apple: the same as 4.C with a different product type. Play: a second purchase type in the app and the service (subscriptions for a fixed term), one name per account and tier, and a fronted year.

### 4.C One-time product per term (recommended)

- **Products first.** One product per tier and term, bought again for every name. At launch, two years in three tiers:

  | Tier | Term | App Store (consumable) | Google Play (one-time) | Price |
  |---|---|---|---|---|
  | 8 characters or more | 2 years | `NAME_8_2Y_01` | `name_8_2y_01` | the site's: $100 |
  | 7 characters | 2 years | `NAME_7_2Y_01` | `name_7_2y_01` | $200 |
  | 6 characters | 2 years | `NAME_6_2Y_01` | `name_6_2y_01` | $400 |

  Renewal products ("add a year" per tier) come in the later run of section 6.7.
- **Consoles.** App Store Connect: three consumables with review screenshots and notes. Play Console: three one-time products, each marked in its details as a Tokenized Digital Asset, and the Financial features declaration updated for NFT sales, as Google's blockchain-based content policy requires; the app consumes each product after its name registers. Nothing to switch off: one-time products have no Pause or Resubscribe.
- **More than one name.** Each name is one purchase; a product is bought again once the previous one is consumed (Play) or finished (Apple).
- **Service verifies and stores.** The receipt with the existing verifiers; the product decides the tier and years, through a names catalog beside the badge one; the payment row (unique per store transaction) and one prepaid name code row holding only the hash the app sent, paid, bound to that payment, with no expiry (Apple: "credits … purchased via in-app purchase may not expire"). Nothing else is new: registration then runs as for any prepaid code.
- **The user sees.** Section 6.5: the name, where it points, the price for two years and its end date, "Nothing renews automatically"; the store's own sheet; then committing, waiting and registering; then the name on their address.
- **Renewal, refund, expiry.** No automatic renewal: the app warns before expiry (section 8.3) and sells "add a year" later. A refund before the name registers leaves an unspent code, which the service can revoke; after it registers, the name stays to its expiry, since nothing can take it back, and the loss is the allowance and the gas. Expiry and release as every shape: grace, then free to anyone.
- **Reaching the registration.** The receipt becomes a paid prepaid code the app already holds; the app registers with it (section 6.4).
- **iOS.** As every store shape (section 6.8).
- **Cost.** Service: a catalog of six entries, two commands, one table column, a daily cap. Core: two commands and the purchase record. Apps: the product list, the purchase lane copied from badges, the pay button and the states of section 6.5. Edge cases: a lost answer, a pending payment, a name taken meanwhile, a reinstall, a refund after registration. None of the subscription surface. All of it on top of registration from the app with a code (section 10, slice 0), which every shape that registers in the app needs.

### 4.D The site only, on every build

- **Products first.** None in the stores. The site sells its two-year term; the app takes an owner code or a prepaid code.
- **Consoles.** Nothing.
- **More than one name.** As many as the site sells.
- **Service verifies and stores.** Nothing new for payment; the owner-code transfer relay (needed by every shape for names bought on the site).
- **The user sees.** Section 7: "Buy in browser" where a link out is allowed, a code field everywhere, and a deep link back.
- **Renewal, refund, expiry.** Whatever the site offers; refunds by support, as on the site today.
- **Reaching the registration.** The site registers to its own wallet; the owner code moves the name to the app.
- **iOS.** No test-environment problem; a link only in the US storefront.
- **Cost.** The smallest, and no store commission. But a store build may only link out where the store allows it, so outside those storefronts the app can only accept a code the user found elsewhere.

## 5. Recommendation

**Shape 4.C, one-time products per term, with the browser path of section 7 for builds that cannot sell.** The name already carries its own term on the chain, so the store only needs to sell the right to one registration, which is exactly what a prepaid code is. That reuses the service's codes, reservations, taken and failed handling, and the registration from the app that codes need anyway, and it avoids the whole renewal surface where every badge review finding landed. The cost to the user is buying again to renew, which is also what the site, the investors' codes and the phase 3a canvases already promise: "Nothing renews automatically."

Ruled out, each in a sentence:

- **4.A subscriptions:** a store account can renew only one name per group of products, the store's longest period is a year against a two-year floor, so every first purchase fronts a year, and it brings back renewal checks, billing retry, pause, resubscribe and plan changes for a record that changes once every few years.
- **4.B on Play (prepaid plans):** one entitlement per account and product, so a second name of a tier cannot be bought, and no plan is long enough for the two-year floor. **On Apple** it is shape 4.C with a different product type, a choice kept as an open question (section 13).
- **4.D as the store builds' only path:** outside the storefronts that allow a link out, a store user would have no way to buy from the app at all; it stays as the path for desktop, F-Droid, and iOS until its purchases are switched on.
- **Cryptocurrency inside a store build:** the stores require their own billing for digital goods.
- **Selling prepaid codes on the site for the app to redeem:** the store owner ruled codes off the site (`2026-10-09-name-codes.md` §14).

## 6. Design of the recommended shape

### 6.1 New mechanisms, each with the requirement it serves

- A **names catalog** in the service: store product → (shortest name length, years). Serves AC3: the tier comes from the product.
- **The code is derived, not stored alone.** Before the sheet opens, the app makes a random per-purchase token and derives the prepaid code from its wallet master and that token, with the product's minimum length in its text (`SN8-…`). The token travels in the receipt (Apple `appAccountToken`, Play `obfuscatedAccountId`, as badges pass their invoice id). A reinstall that restores the wallet recomputes the same code from the receipt; another device of the same store account cannot. Serves AC2 without any re-keying.
- One **service command**, purchase a name code: a store receipt and the hash of the code → a paid prepaid code row bound to that payment. Serves AC1 and AC2.
- One **column**, the payment that paid a prepaid code (`payment_id`, unique, in SQLite and Postgres). Serves AC2: one receipt, one code.
- One **service command**, the names catalog for the app: codes-only or open, the products, the years and term start, the terms version and address, and the service's clock. Serves AC6, and lets the app build orders by the service's clock.
- Two **core commands**: one makes the purchase record, its token and code before the sheet; the other hands the receipt over after it, matched to the record by the token. Serve AC1 and AC2.
- The **purchase lanes** in the apps, copied from the badge lanes. Serve AC1 and AC4.
- The **owner-code redemption**: relays in the service for the two transfer signatures and for a record edit, with their checks and bounds, and the deep link back (section 7). Serve AC5.

Removed or simplified: nothing; the plan adds the smallest path next to the code path.

Alternatives considered for the bridge from a receipt to a registration, besides the code:

- A fourth payment kind in the registration, "store receipt", next to invoice, code and balance: every phase, the taken and failed handling and Try again would gain a case; a code already has them.
- The service makes the code and returns it: the service would have to keep the code, or a lost answer would lose the payment; when the app derives it and sends only its hash, a lost answer is just sent again, and the service never sees the code, as on the badge site.
- A random code, re-keyed when a receipt comes back with another hash: Play lists an unconsumed purchase on every device of the account, so a second device would revoke the first one's unused code; a receipt is a bearer secret, not proof of the device.
- Credit bound to the app's purchase key instead of a bearer code: no backup for the user, and a new credit store in the service.

### 6.2 How a store purchase reaches the registration

```
App                                Store              Store service                    Ethereum
 | names catalog ------------------------------------> codes-only? products, years, terms, clock
 | search the name (lookup through the resolver, as today)
 | record: token T, code C = derive(wallet, T)
 | buy product (tier, 2 years), T --> sheet, payment
 | <-------------------------------- receipt carrying T
 | purchase name code {receipt, hash(C)} -----------> verify receipt with the store
 |                                                    product -> (tier, 2 years)
 |                                                    payment row, code row {hash(C), paid}
 | <------------------------------------------------- code paid
 | code order: owner, commitment, signed (slice 0) -> commit -----------------------------> commit
 |   wait at least 60 s
 | reveal, signed intent (slice 0) -----------------> registerWithCredit -----------------> name to the wallet account
 | <------------------------------------------------- registered, expires
 | claim the name on the address or channel; finish (Apple) or consume (Play) the purchase
```

### 6.3 The service

- **Catalog.** `storeNameProduct :: PaymentProvider -> Text -> Maybe NameProduct`, `NameProduct {minLength, years}`, beside `storeProduct`, holding the six launch products.
- **Command "purchase a name code".** `{payment :: ServicePayment, codeHash}`, signed with a fresh purchase key as badge requests are, routed for Play to the purchase lane as `STCPurchaseBadge` is, so a slow Play call holds nothing else up. In order: (1) the same receipt and the same hash get the stored answer, whatever its age; (2) verify the receipt with the existing verifier (refusals as for badges: invalid, pending, test purchase); (3) look the product up in the names catalog; (4) in one transaction insert the payment (unique by store and transaction) and a prepaid code row `{code_hash, min_length, years, payment status paid, no expiry, payment_id}`; acknowledge on Play before answering. A verified, paid receipt of a known product is credited whatever mode the service is in: the gate is the catalog answer before the sheet opens, and a paid receipt is never refused for a mode. The same receipt with another hash: `receipt_used`, "this payment belongs to another device". Answer: the tier, the years and the code's state.
- **Catalog command for the app.** The answer the site's search gives, without a name: codes-only or open (the sale is open when codes-only is off; the app's existing countdown to 12 December keeps its own date), years, term start, terms version and address, the product identifiers, and the service's current time, from which the app builds each order's timestamp and duration (the service checks both within ±600 s and the term slack).
- **Owner-code redemption.** Commands to relay the code wallet's `approveWithSig`, the app wallet's `claimWithSig`, and one record edit, each checked as the site's record edits are (signer, nonce, deadline, the `from` address owning the token), with a bound per name and per hour, since each relay is a transaction whose gas the service pays.
- **Daily cap.** The service registers at most a set number of store-paid names a day (a setting), alerts the operator as it nears, and counts refunds from Apple's refund notifications and Play's voided purchases, so repeated refunds cannot drain the registration budget unnoticed.
- **Refunds.** Nothing automatic at launch: the time from payment to registration is minutes, refunds come days later, and a registered name cannot be taken back. Support handles requests as for the site (`//refund`). A receipt already refunded when it is presented is refused by the existing verifiers (Apple's `revocationDate`, Play's purchase state); nothing re-checks it afterwards.
- **No change** to registration, codes, phases or the worker. The code order itself reaches the service through slice 0's commands (section 10).

### 6.4 The core

- The purchase record: product, token, code, store transaction, state (made, paid, used), and the registration it funds; kept with the database, so a backup or migration carries it.
- `APICreateNamePurchase {userId, product}` makes the record, the random token and the derived code, and returns the token for the store sheet. `APIPurchaseNameCode {userId, payment :: ServicePayment}` matches the receipt to its record by the token, sends the receipt and the code's hash, records the answer, and returns the code's state; the apps then place the code order of slice 0. Repeats are safe (6.3).
- After a reinstall, the app restores the wallet from the recovery key, finds the open store purchase, recomputes its code from the token in the receipt, and gets the same answer; a registration already under way is found again with the owner-signed `find`, as on the site.
- The wallet account is created right before the first purchase (the recovery key screen of the phase 3a canvas), not at app start.

### 6.5 The apps

Android first; iOS gets the same code behind a switch, off until section 6.8 is decided; desktop and F-Droid never show store purchases.

- **Products.** Loaded from the store by the identifiers in the catalog answer; the confirm screen shows the store's price in the store's currency.
- **Purchase lane.** Android: `launchBillingFlow` for the tier's product with the purchase's token as `obfuscatedAccountId`; the result goes to the core command; the purchase is acknowledged once the core holds it, as badges do, and consumed when its name registers. iOS: `product.purchase(options: [.appAccountToken(token)])`; the transaction is finished when its name registers. The token is random for every purchase, so it links nothing across purchases.
- **An open purchase** (paid, name not registered) is found at start from the store's own purchase list (Play's purchases query, Apple's unfinished transactions) and continues. When the app's database still holds the chosen name, registration resumes for it; only after a reinstall, or when the name was taken, does the app ask for a name. Play won't sell the same product again until it is consumed; on both platforms the app offers the held payment instead of a second one.

#### Screens

Drawn on the design page. They follow the phase 3a canvas, with the changes listed after them, and use the words of the app's existing name and badge screens. One main action, at the bottom, in the same place in every state; outcomes appear in place on the same screen, never as an alert over an empty one. Names are counted in characters (letters, digits and hyphens), never "letters".

Entry and search are the canvas's, unchanged: the address row "Get SimpleX name", the hub "SimpleX names", the channel row, Register from the "available" alert, Re-register from an expired own name (canvas 1a–1e, 2), and the search with its inline checks: at least 6 characters, letters, digits and single hyphens, taken (by an address, a channel or both), reserved, an expired name read as taken (canvas 3, 3a, 3b). In store builds the "available" alert shows the store's price, or none, so two prices never follow each other. The editor's sale countdown and "How to register a test name" give way to these in the version that registers.

1. **Available** (canvas 5): THE NAME `dynamis.simplex` · POINTS AT Alice, your address · YOU PAY 2 years, until 31 Dec 2028. Footer: "Alice's address shows the name as soon as it is registered, and your contacts receive your updated profile. Anyone can look up where a name points. Paid once: nothing renews automatically." In store builds, under the price: "Paying with your Google Play account lets Google and SimpleX link this payment to the name." (iOS: "your Apple Account"). Above the button, "I agree to the SimpleX name terms" with "Read the terms", ticked for every name before the button works (the community wording for a name on the community list). Main: "Register for $99.99" (the store's price in the store's currency, said once). Secondary: "Redeem code". The ✕ on POINTS AT registers the name leading nowhere, after canvas 5d's confirmation. When the profile already shows a name: "Replaces alice.simplex on your profile." From a channel (canvas 5h): POINTS AT #Bakery Deals, your channel, and "People who look the name up join the channel by its link." Extremes: a 63-character name wraps and the content scrolls under the pinned button; a 6-character name at $399.99.
2. **Recovery key** (canvas 7): before the device's first payment or redemption, as the canvas draws it, with "Back up later" leaving the reminder on the hub.
3. **Store sheet**: the platform's. Cancelling it returns to 1 with nothing charged.
4. **Registering** (canvas 9): Confirming your payment · Committed · Waiting 60 s · Registered. Footer: "You can leave this screen. If the app closes, registration continues when you open it again." Offline after paying: "No connection: your payment is safe and is confirmed when you are back online." Main: "Close". Secondary: "Cancel" until the name is revealed, which keeps the payment for another name (canvas 9a).
5. **Payment pending** (a Play pending purchase, Apple's Ask to Buy): Waiting for Google Play to confirm your payment. Footer: "dynamis.simplex is not held meanwhile: if someone registers it first, your payment pays for another name." Main: "Close". If the store cancels it: "Payment not completed. Nothing was charged." and 1's button returns.
6. **Done** (canvas 10, in place): every row ✓; "dynamis.simplex is yours until 31 Dec 2028. It points at Alice's address." Main: "Close".
7. **Someone was faster** (canvas 10a, in place): the last row ✕; "Someone registered dynamis.simplex first. Your payment is kept for another name of 8 characters or more." (after a pending payment: "while your payment was pending"). Main: "Choose another name".
8. **A step failed** (canvas 10b, in place): "Your payment is kept and dynamis.simplex is still available. Try again: you won't be charged twice." Main: "Try again".
9. **Not confirmed** (the service refused the receipt): Android, "Google Play did not confirm this payment. If you were charged, Google Play refunds it within 3 days." iOS, "The App Store did not confirm this payment. If you were charged, contact SimpleX Chat." Main: "Contact SimpleX Chat", which opens the team's chat with the store's order number in the draft.
10. **Already paid** (a held paid code: after a taken name, a cancel, or a reinstall): 1 with YOU PAY "Already paid · names of 8 characters or more · 2 years" and main "Register", no price. A name shorter than the payment covers: "Your payment covers names of 8 characters or more", button off. A longer, cheaper name than the one paid for: "Your payment was for a 6-character name. This name costs less; the difference isn't refunded." above the button.
11. **Sale not open, or codes-only**: YOU PAY reads "The sale opens in 12 days 03 hrs" (the existing countdown) or, in codes-only mode, "Paying in the app opens soon"; "Redeem code" takes the main button's place.
12. **Store unavailable** (offline, the store unreachable, products not loaded): the button stays in place, off, with the reason in the footer: "Can't reach Google Play. Check your connection."
13. **Hidden profile**: Register isn't offered: "Hidden profiles can't own names. Switch to another profile first." (backup canvas 9).

After the screen is left, the name's row on the address or channel screen, and the hub, carry the state and its one action: "Registering…", "Waiting for Google Play", "Not registered · Try again", "A paid name is waiting · Choose its name". Success updates the row; no alert appears over another screen.

Changes from the canvas, and why: outcomes in place instead of alerts (one main action in one place, nothing lost on dismissing); terms per name on screen 1 instead of once per device (the site's rule since 23 October, with the community wording and a fresh tick when the terms change); "characters" instead of "letters"; "Redeem code" and "Close" as in badges; Cancel kept until the reveal, as canvas 9a.

Money and irreversible steps: the store sheet is the explicit act; the code is kept before the sheet opens, so an interruption after payment is recovered from the store's purchase list; one store transaction makes one code and one registration (AC2).

### 6.6 What iOS shows meanwhile

The badge pattern, done as the storefront rule says (section 7): no purchase button; "Buy in browser" only in the US storefront. Elsewhere the hub's main action is "Redeem code": search still shows whether a name is available, with "Redeem code" in the button's place, and nothing names a site.

### 6.7 Renewal, designed for the later run

- "Add a year" one-time products per tier (`NAME_8_1Y_RENEW_01` …), sold from the name's row and from the expiry warning, available to the owner during the term and the grace period.
- The service verifies the receipt as above and sends `renewWithCredit` for one year, with no owner signature; it checks the name is held by the device wallet that asks, so a stranger's renewal of someone else's name is not sold by mistake (renewing it would still be harmless to the owner).
- Shorter steps (one or three months) are possible on the chain only if renewal has no minimum, which is to be confirmed; they are not recommended, since each costs a store commission on a small price and a transaction's gas.
- Built and released before mid-2028: the first paid names expire on 31 December 2028.

### 6.8 iOS and Apple's test environment

Every purchase made by App Review or a TestFlight tester is a test purchase, which the service refuses in production. For names, crediting one would register a real `.simplex` name for nothing. Options:

- **Register test purchases as test names** (recommended to switch purchases on): the reviewer completes the whole flow and receives `dynamis.testing`, shown as such; real payments register `.simplex`. Cost: a second, small deployment of the store service, set to the `testing` namespace, Apple's sandbox host and crediting test purchases (a service runs one namespace, and crediting test purchases requires the sandbox host), with the app sending a transaction marked sandbox to that deployment's address; about a day's work for the service and the deployment. Anyone can join SimpleX's public TestFlight and buy in the sandbox for nothing, so that deployment registers a few names a day for the whole service and refuses the rest; the `.testing` namespace has real users today, and each registration costs real gas. A deployment on a test network instead of Ethereum's main chain would cost no gas, but a reviewer couldn't look the name up in the app.
- Keep the badge pattern until Apple's environment is handled for badges too (the default at launch).
- Refuse test purchases: App Review cannot complete a purchase.
- Show the browser path to test builds only: Apple treats a hidden feature as grounds for removal (guideline 2.5.19, which took this rule over from 2.3.1 on 16 October 2026).

Until iOS sells names, its code path relies on App Review accepting it as it accepts the badge codes: Apple lets an app use what was bought on the developer's site "provided those items are also available as in-app purchases within the app" (3.1.3(b)), and forbids "their own mechanisms to unlock content or functionality, such as license keys" (3.1.1). Switching iOS purchases on removes that risk (question 6).

### 6.9 Where badges and names meet

Nowhere at launch: separate products, no discount, no shared purchase. A name never unlocks an app feature, and a badge never pays for a name. They share the store service and its receipt verifiers; on iOS, App Store purchases of both carry the same Apple Account identifier, so the service can tell that the buyer of a name also bought a badge (section 9). The badges threat model says badge purchases can't be linked to a profile; once iOS sells both, that statement needs this exception, for the badge owners to update before iOS badge purchases ship.

## 7. The browser path

For iOS until its purchases are on, desktop and F-Droid, and for every user who bought on the site.

- **Primary action, "Buy in browser"** (the badge's words; the price row above it names the site, "$100 on simplex.domains"): shown in desktop and F-Droid builds; in the iOS build only in the US storefront (the badge's `badgeBrowserAllowed`, reused); never in the Android Play build, which sells in the app. It opens `https://simplex.domains/` with the searched name in the fragment, which the site's code doesn't send and removes from the address once read; the name stays in the browser's history.
- **Secondary, "Redeem code"**: every build, in the badge's words: title "Redeem code", "Paste the code you received.", the field, Paste, Scan, and "Redeem", off until the code checks. One field takes an owner code (`SN-` and 30 characters) or a prepaid code (`SN<length>-` and 20); the core's existing parser tells them apart. Once checked, the screen says what the code is and what the button will do, and the button's label stays one line: a prepaid code, "Pays for one name of 8 characters or more, for 5 years" and "dynamis.simplex until 31 Dec 2031", then "Register"; an owner code, "bakery.simplex, registered until 31 Dec 2028", then "Add to Alice". No term shows before a code is checked: prepaid codes carry 2 to 10 years. Refusals are one line under the field with the button off, in the badge screen's words: not valid, already used, held by another registration, expired, not paid yet, doesn't cover this name.
- **Back from the page**: after the name registers, the site's "Open in SimpleX" opens the app through a verified https link the app owns where the platform has one (Android App Links, iOS universal links), with the owner code in the fragment, routed beside the badge's `/badge/code/`; it doesn't use the custom `simplexchat:` scheme, which any Android app can register. The app confirms before acting, as the badge sheet does: "Add dynamis.simplex to your profile?" / "It will be added to the profile Alice and point at her address. After this, its name code no longer controls it; your recovery key does." The recovery key is set up first if it never was. Then "Moving your name" (Close: it continues), then added. If the name's records point elsewhere, the app offers to point it at this profile's address (one relayed edit). Refusals, each with its next step: code not valid, already moved, name expired, offline. On desktop without the app's link scheme registered, the code screen opens beside the browser, as for badges.
- **Reused directly from badges**: the storefront check, the deep-link router with a new path, the code screen's field, paste and QR scanner, the confirmation sheet, the button styles. **Names' own**: the page address, the two code kinds, what redeeming does (a registration or a transfer, each with progress), and the site's link back (a change in the simplex.domains repository).
- **Without a code at all?** Possible: the app makes the order (the name's hash, its wallet address, the commitment) through the service, the page only takes the payment for that order, and the app reveals when it is paid; no owner code and no transfer. It needs order commands in the app, a page mode for orders made by the app, and reopening the web flow from the app that the store owner dropped on 21 October. Not at launch: the owner code covers every name bought on the site with one mechanism, which has to exist anyway for names bought before the app sold any.

## 8. Also decided

### 8.1 Moving a name

- **Between profiles on one device**: the name's account is rebound to the other profile (a database change) and its record re-pointed at that profile's address (one relayed edit). Not at launch.
- **To another device**: the recovery key (24 words) brings every name of the device; a database migration carries the wallet too. A store purchase is not tied to a device, but it buys a registration, not a name.
- **To another person**: approve and claim, the same two transactions as an owner code. Not at launch.

### 8.2 Expiry while the app is offline

The chain decides; the app cannot. A name expires on its date and can be renewed by its owner for the 90-day grace; after that anyone can register it. Existing conversations are unaffected. So the app warns from the expiry date it stored at registration, without the network (8.3), and the reminders start long before the date.

### 8.3 What the user sees before expiry

- The name's row on the address or channel screen always shows "Expires 31 Dec 2028". The app stores the expiry for every name its wallet holds, bought in the app or moved in from the site (the move reads it on the chain), and refreshes it from the resolver whenever the name is verified, since anyone can renew a name.
- From 30 days before: the row turns to "Expires in 21 days"; a chat-list banner with a dismiss control, dismissible once a week and above promotional banners in the last 7 days; local notifications at 30 and 7 days.
- A "Renew" button appears only where a renewal can be bought: in store builds once the renewal products exist (6.7). The site sells no renewals today, so desktop and F-Droid show the date and the warning, without Renew, until it does.
- In the grace period: the existing alert, "Your name dynamis.simplex expired on 31 Dec 2028. Renew it before 31 Mar 2029.", with Renew where it can be bought.
- After the grace: "Re-register", from the existing alert.

## 9. Compatibility and security

- **Protocol**: the store service's app protocol gains the purchase and catalog commands, the owner-code relays, and slice 0's order commands, under one version bump shared with slice 0, as badges' did. Chat protocol and agent: unchanged.
- **Database**: the core gains a purchases table (one migration, SQLite and Postgres); the service gains one column on prepaid codes (both backends).
- **Older apps**: unaffected; they never send the new commands.
- **Threat model.** This design accepts and states what follows; changing it is the owner's decision (question 16).
  - *The stores* learn that the account bought "SimpleX name, 8 characters or more, 2 years", when, at what price, and from which device and network; the app never sends them the name. But the name is registered on the public chain minutes after the payment, by SimpleX's registrar key, with a length that matches the product's tier, so a store that compares its purchase times with the chain can find the name a purchase paid for, unless many names of that tier are registered in the same minutes. So can anyone who sees the account's purchase history or receipts.
  - *The store service* learns the store transaction and, at the reveal, the name and the wallet address it registers to, so SimpleX links each payment to its name, as on the site. It does not learn who holds the store account, but with every receipt it gets the product, the price and currency, and the buyer's store country (Apple's storefront, Play's billing region). Every App Store transaction also carries Apple's identifier of the buyer's Apple Account in this app (`appTransactionId`), the same in every purchase that account makes, on any device; so on iOS the service can tell which names, and which App Store badge purchases, one Apple Account paid for, and which addresses those names point at. The stores' APIs could tell it more from one transaction: Apple's returns every purchase of the same Apple Account, Play's orders API the order's tax address. The service reads nothing beyond the product and the transaction and stores no receipt: that is how the service is written, not something a user can check.
  - *The store and the service together*: the store keeps who paid, the service which name the payment registered, and the store's transaction or order number joins the two records, so whoever obtains both learns who registered the name. A user who gives support the order number links that conversation to the payment.
  - *The store service as the registrar* is trusted to send the registration it is given: it learns the name at the reveal and could register it first or let the commitment lapse; it cannot register the name to another owner, since the intent is the owner's signature.
  - *Chain observers* see each name, its owner address and its records, and that SimpleX's registrar registered it. One wallet account per name keeps two names of one person from sharing an address, but two names registered one after the other can be linked by their times. The history of where a name pointed stays on the chain after it changes.
  - *Network*: requests reach the service through SMP with private routing, on by default, so neither the service nor its router sees the device's IP address. The stores see the device and network at purchase. On the browser path, the site sees the browser's IP address, a card payment's processor sees the card holder, and the order links both to the name; the searched name sits in the browser's history (the page removes it from the address once read).
  - *A store receipt* is a bearer credential until it is first presented: whoever presents it first binds the payment to a code of their own. The app presents only the receipts its own purchase records hold, so another device or profile on the same store account never takes a payment by itself; a second device presenting it later is told the payment belongs elsewhere.
  - *A forged or replayed receipt* is refused by the existing verifiers; one transaction funds one code (the unique payment); the tier comes from the product, never the client. The service looks the product up before asking the store, and one-time Play reads share the per-minute bound subscriptions have, so invented receipts cannot use up the Play quota.
  - *A stolen code* before registration pays someone else's name; the code lives in the app's database, as wallet keys do, and the service sees it only when it is spent.
  - *A refund after registration* returns the whole price, so repeating it costs the buyer nothing, while each costs SimpleX the name's chain price from the registration budget and the gas, and the buyer keeps the name for two years. Google Play refunds on request within 48 hours; Apple decides its own refunds. Bounded by a daily cap on the budget spent on store-paid registrations, with an alert as it nears, and counted from Apple's refund notifications and Play's voided purchases.
  - *Apple test purchases* never register a `.simplex` name (section 6.8 for where they go, and their cap: anyone can join the public TestFlight).
  - *The owner code*: whoever has it owns the name until the app has moved it, after the user confirms and two relayed transactions land; after the move it no longer controls the name, but it still opens the site's order (the name and its links) and owns any balance left on it. The move is public on chain, from the site's wallet to the app's account. The link back is a verified https link the app owns where the platform has one (Android App Links, iOS universal links), since any Android app can register the custom scheme; elsewhere the code is pasted or scanned.
  - *Notifications* about expiry follow the app's notification preview setting, so a hidden preview never shows the name on the lock screen.
  - Not done: blind-signed codes. The service would sign a code against a payment without seeing it, so it could not tell which payment, and so which store account, paid for which name; this holds only if the name is also registered at a time unrelated to the payment, since the service and the store see both times. Listed as later work, together with that delay.
- **Hidden profiles** cannot buy or register a name, as PR 7530 refuses them; open store purchases are matched against every profile's records, continue only in the profile that made them, and a purchase no profile records (a reinstall) is offered in the active profile, never a hidden one.

## 10. Slices

Each slice is a later implementation run. Estimates are for implementing, building and testing, at this container's pace. Bases: the service work builds on `sh/store-service` (or master once https://github.com/simplex-chat/simplex-chat/pull/7809 merges), the core work on `names` or `sh/store-service`; master has neither the names store nor the wallet.

0. **Prerequisite: registration from the app with a prepaid code, on the store's order path** (names work · large · high risk: signing and money). PR 7530's plan predates the store service and is a different design (no owner signature, credits held by a purchase key, codes with years in their text, its own commit and reveal commands); the store's code order replaces it. It delivers: (a) a structural move of the commitment, intent and order-message code (`StoreService/Names/Registration.hs`, `Signatures.hs`) into the library; (b) EIP-191 and EIP-712 signing per wallet account in the core, with the commitment secret and the sealed order derived for an app account; (c) SMP commands for the code order, the registration view and its poll, reveal, retry, find and another name, reusing the service's code order and its checks; (d) the service version bump this plan's commands share. This plan's slices 2, 3 and AC1 wait for it.
1. **Service: names catalog, catalog and purchase commands** (behavioural · medium · high risk: money · about 4 hours). Catalog, the two commands, payment-to-code binding, repeats, `receipt_used`, the purchase lane for Play, the daily cap on store-paid registrations, refund counting, promo-code purchases credited (they are SimpleX's own) and rewarded ones refused. Tests with the fake App Store and Play verifiers, including a real sandbox JWS decoded to confirm which fields Apple sends.
2. **Core: purchase record and commands** (behavioural · medium · high risk: money · about 4 hours). Migration, the two commands, the derived code, recovery after a reinstall, handing the code to slice 0's order. Tests against the service with fake stores.
3. **Android store lane** (behavioural · medium · high risk: money · about 3 hours). Products, sheet, token, pending, open purchases, acknowledge on hand-over, consume after registration; reviewed on its own.
4. **Screens, iOS first** (behavioural · large · low risk · about 6 hours, not compiled in the container). Screens 1–13, code entry, the iOS storefront rule, iOS's store lane behind a switch; previews per state.
5. **Screens ported to Kotlin** (behavioural · medium · low risk · about 4 hours). The same files, names and states.
6. **Owner code redemption and the link back** (behavioural · large · high risk: transfers · about 6 hours). Service relays with their bounds, core signatures, the confirmation, progress and refusals, the verified link back, pointing the records; with the site's "Open in SimpleX" (simplex.domains repository).
7. **Expiry display and reminders** (behavioural · small · low risk · about 2 hours).
8. **Later: renewal products** (6.7), and **the test-purchase deployment** (6.8) when iOS is switched on.

Console work for slices 3 and 4: the six products, review notes, Google's tokenized-asset marking and declaration; for iOS, the products stay unsubmitted until the switch.

## 11. Test plan

- Service: one receipt one code; the same receipt and hash resent, at any age; the same receipt with another hash answered `receipt_used`; a paid receipt credited while codes-only is on; a test purchase refused; an unknown product refused before any store call; a 7-character name with an 8-character code refused by the existing code check; the daily cap; the owner-code relays' checks and bounds.
- Core: the code derived from the wallet and the token; a reinstall with the restored wallet recovering an open purchase and a registration under way; a second device answered "belongs to another device"; hidden profiles refused.
- Core and service end to end, fake Play and fake chain: AC1 and AC5 flows; taken then another name; failed then Try again; a lost answer.
- Apps: previews for screens 1–13 and code entry in light and dark, the shortest and longest names, the largest text size; iOS storefront US and not US; no list layout changes, so one layout.
- Pace: slice 0 first; slices 1 and 2 built and tested together, with one lean review after each (money); slice 3 reviewed on its own; slices 4–7 batched; one review loop over the whole at the end.

## 12. Risks

- **The two-year floor is read from the service, not the contract.** The service also decodes the controller's `DurationTooShort` revert (`StoreService/Names/Revert.hs:22, 72`); a 729-day registration refused is added to the store's end-to-end tests against the contracts (`plans/names-codes/e2e/` on `sh/names-design`).
- **Renewal minimum unknown** until the contracts are read; only renewal design depends on it.
- **App Review and the consumable type.** Apple may read a two-year registration as a time-limited service that should be a non-renewing subscription; the product type is fixed when created, so it is settled before the products are made (question 4).
- **Store policy on blockchain items.** Apple allows in-app purchase "to sell and sell services related to non-fungible tokens (NFTs), such as minting, listing, and transferring", and lets users view their own "provided that NFT ownership does not unlock features or functionality within the app" (3.1.1): a name must never unlock anything in the app (section 6.9). Google requires the products to be marked as Tokenized Digital Assets and declared (section 4.C). Whether an app holding the keys that own names counts as a "software wallet" under Google's cryptocurrency policy is not stated in Google's pages; it applies to the wallet whether or not names are sold, and is for the names work to settle before release.
- **iOS's code path before iOS sells names** (section 6.8): an App Review risk under 3.1.3(b), the same one the badge codes carry.
- **Refund abuse** after registration: a refund returns the whole price, so it costs the abuser nothing; bounded by the daily cap on store-paid registrations and counted from the stores' refund notices (section 9).
- **Linking by time** (section 9): the store can match a purchase to its name by the registration's time. Accepted and stated at launch (question 16).

## 13. Open questions

Each carries the recommended answer this plan uses.

1. **How the apps sell names.** Recommended: one-time products per term (4.C). If subscriptions instead: sections 4.A's slots, name binding and renewal sender are added, and the product table changes.
2. **Terms sold.** Recommended: two years only, as the site. If five years too: three more products and a term picker on screen 1.
3. **One, three or twelve months.** Recommended: not sold; a first registration cannot be shorter than two years, and renewals go by whole years. If wanted as renewals: confirm the contract allows them, then shorter "add" products.
4. **Apple product type.** Recommended: consumable, because what is sold is one registration, spent once. If App Review asks for a non-renewing subscription, the same service path takes it with three new product identifiers.
5. **Price in the stores.** Recommended: the site's prices, so a name costs the same everywhere; the store's commission is SimpleX's cost. If the commission is added: store prices above the site's, and the confirm screen shows the store's.
6. **iOS at launch.** Recommended: the badge pattern (site in the US storefront, a code everywhere), then switch purchases on with Apple's test purchases registering a few test names a day through a second, small service deployment. If purchases at launch: that deployment is part of the first release, and iOS meets Apple's condition for site-bought names (3.1.3(b)) from the start.
7. **Bringing in a name bought on the site.** Recommended: the owner code, pasted, scanned, or brought back by a verified https link, moves the name to the app. If orders made by the app and paid on the page instead: order commands in the app and a page mode, and the dropped web flow returns.
8. **Where the site link shows.** Recommended: US storefront on iOS, never in the Android Play build, always on desktop and F-Droid. If everywhere, as the iOS badge screen does today: App Review may refuse the app outside the US.
9. **Refunds after registration.** Recommended: the name stays; nothing automatic. If refunds should be prevented: the service would have to wait days before registering, which nobody wants.
10. **Expiry reminders.** Recommended: the date on the name's row, from 30 days a row warning, a banner and local notifications, and the existing grace alert. If fewer: the row only, and more names lapse.
11. **Moving names.** Recommended: names follow the wallet (recovery key, database migration); per-name moves later.
12. **Badges and names.** Recommended: no interaction.
13. **Codes-only mode and the sale date.** Recommended: the app offers in-app purchase only when the service is out of codes-only mode and the sale date has passed; before it, screen 1 offers only a code. A receipt that was paid is credited whatever the mode.
14. **An unregistered paid purchase.** Recommended: kept open in the store until its name registers, so a reinstall recovers it and a second payment is never taken for it.
15. **When renewals ship.** Recommended: "add a year" products released before mid-2028, after the contract's renewal rules are checked. If at launch: the renewal design and the contract check move into the first runs, for names that expire two years later.
16. **Linking a payment to its name.** Recommended: register at once and say so on screen 1 ("Paying with your Google Play account lets Google and SimpleX link this payment to the name."), with section 9's statements. If the link should be weakened: delay each registration by a random time or register in batches, so a store must guess among the names of an hour or a day (buyers wait longer, and more names are taken meanwhile); blind-signed codes then also unlink the payment from the name for SimpleX.
17. **Outcomes on the registering screen or as alerts.** Recommended: in place on the screen, with the main button changing its label where it stands. If alerts, as the canvas draws them: the main action moves into a centred alert over an empty screen, and is gone once dismissed.
18. **Terms once per device or per name.** Recommended: per name, as a line ticked above the pay button, as the site asks since 23 October. If once per device: the app must still ask again when the terms change and for community names.
19. **Builds that can neither sell nor link out.** Recommended: start from "Redeem code"; search shows availability without a pay button. If they show the full price screen: it ends at a field most users can't fill.
20. **A dearer payment spent on a cheaper name.** Recommended: allow it, with "the difference isn't refunded" above the button. If refused: a taken 6-character name leaves a hard search for another one.

## 14. Out of scope (noticed, not doing)

- The iOS badge screen opens the badge site in every storefront, while its own comment says the link out is US-only (`BadgesSupportSimplexView.swift:71-78` against `BadgeStore.swift:37-41`): for the badge owners.
- The badges threat model says badge purchases can't be linked to each other or to a profile; every App Store transaction carries Apple's per-account `appTransactionId`, which that statement doesn't account for. Nothing is exposed while iOS badge purchases are off; for the badge owners before they ship.
- Name alerts open `https://simplex.domains/#testing` for Register, Renew and Re-register, including for `.simplex` names (on `names`): to follow the in-app purchase.
- Nothing warns before a name expires, and the core stores no own name's expiry (section 8.3 covers every name the wallet holds, and refreshes the date whenever the name is verified, since anyone can renew a name).
- Whether an app holding the keys that own names is a "software wallet" under Google's cryptocurrency policy: for the names work (section 12).

## 15. Follow-up runs

The slices of section 10, in order, each its own run and pull request; slice 0 belongs to the names work, and slices 1 and 2 can share one.
