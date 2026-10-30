# Selling SimpleX names in the apps

Base: master · Repo: simplex-chat · Builds on the store service (https://github.com/simplex-chat/simplex-chat/pull/7809), the device wallet (https://github.com/simplex-chat/simplex-chat/pull/7703) and registration from the app with a code (https://github.com/simplex-chat/simplex-chat/pull/7530).

## Summary

- **What exists.** A name is registered on Ethereum to a key its buyer holds; the store service commits and registers it, paying the gas from its own key and the price from an allowance. simplex.domains sells names for card, Bitcoin and Monero, and accepts prepaid codes; the apps show, verify and set names but cannot register one. Nothing sells a name through the App Store or Google Play.
- **Two facts the brief did not have.** The contract refuses a registration shorter than two years, and the site sells one fixed term, two years from 1 January 2027: the buyer does not choose a duration. So one, three or twelve months can never be a first registration; shorter steps can only be renewals, which nothing builds yet.
- **Recommended: one-time store products, one per name length, each a two-year registration.** The app makes a prepaid name code, the store takes the money, the service records the payment against the code's hash, and the app registers the name with that code exactly as it would with any prepaid code, such as an investor's. Nothing renews by itself; a renewal is another one-time purchase, built before the first names expire at the end of 2028.
- **Android sells at launch. iOS keeps the badge pattern** (the site in the US storefront, a code everywhere) until purchases made in Apple's test environment are handled; this plan's suggestion for that is to register them in the free `.testing` namespace. Desktop and F-Droid builds link to the site and take a code.
- **Ruled out:** auto-renewing subscriptions (one renewing name per store account, a year fronted on every first purchase, and every subscription edge case badges met); Google Play prepaid plans (one per account, at most a year); the site alone (most store users could not be sent to it).

## 1. Goal and acceptance criteria

The goal: a person in a store build of the app registers a SimpleX name for their address or channel, paying with the store, without a wallet, ether or a browser; and every build has a working way to bring in a name bought on the site.

- AC1: In the Android Play build, a user with no wallet buys a name of 8 or more letters for two years with Play Billing and ends with the name registered to a key of the device wallet, pointing at their address, which shows it as verified. Test: an end-to-end test with the store service, the fake Play verifier and the fake chain node.
- AC2: A payment is never credited twice and never lost: the same receipt sent twice gives one code; a receipt whose answer was lost is sent again and gives the same code; a name taken between search and registration keeps the payment for another name. Tests: service tests for each case.
- AC3: Neither the app nor the service can turn a purchase of one length tier into a name of a shorter length: the tier comes from the store product, never from the client. Test: a service test with a 7-letter name and an 8-letter product.
- AC4: In the iOS App Store build, the purchase button is not shown until store purchases are switched on; the US storefront shows the site link and every storefront shows code entry. Test: previews of both storefront states.
- AC5: A name bought on the site reaches the app: its owner code, typed or arriving by deep link, moves the name to the device wallet. Test: an end-to-end test with the owner code flow and the fake chain node.
- AC6: The service refuses store purchases of names while the store is in codes-only mode or before the sale opens, and the app never opens the store sheet then. Test: a service test, and the app's catalog state in previews.

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
- Prices come from the chain: $100 for 8 or more letters, $200 for 7, $400 for 6, for two years; no name is shorter than 6 letters (store API "Prices").
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
- Play one-time purchases are acknowledged by the service before crediting and consumed by the app later (`StoreService/StoreReceipts/Google.hs:96-102`; master `BadgeStore.kt:47-49`).
- The badge site's codes are made in the browser; the service stores only their hash (master `apps/simplex-badge-service/web/src/codes.ts:67-76`).
- Prepaid name codes: one row per code hash with the shortest length it covers, its years and a payment state; reserved while a registration holds it, spent when the name lands, released when the name is taken, kept 30 days for Try again after a failure (`StoreService/Names/Codes.hs`; `2026-10-09-name-codes.md` §6–7). `issueNameCode` and `revokeNameCode` exist for the operator (`Codes.hs:152-167`).

### 3.4 The core and the apps

- The core (on `names` and `sh/store-service`) has the device wallet: one 24-word master per device, one account per name, each bound to a profile (`docs/rfcs/2026-09-10-wallet-keys.md`). It cannot sign yet: signing is "a command here that returns a signature", not built; simplexmq already has recoverable signing and EIP-712 hashing.
- The core makes and reads prepaid name codes: `randomNamePrepaidCode` and `namePrepaidCodeHash` are in `src/Simplex/Chat/Badges/Code.hs` (`sh/store-service`).
- Registration from the app with a code is planned in https://github.com/simplex-chat/simplex-chat/pull/7530 (`plans/2026-09-28-name-registration-core-cli.md` on `ab/names-actions-api`): a worker per user commits and reveals through the service and claims the name on success. Not built.
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
| https://github.com/simplex-chat/simplex-chat/pull/7530 registration from the app | prerequisite: registering with a code |
| https://github.com/simplex-chat/simplex-chat/pull/7566, https://github.com/simplex-chat/simplex-chat/pull/7561 wallet model and scan | unchanged |
| https://github.com/simplex-chat/simplex-chat/pull/7390, https://github.com/simplex-chat/simplex-chat/pull/7425, https://github.com/simplex-chat/simplex-chat/pull/7428, https://github.com/simplex-chat/simplex-chat/pull/7355, https://github.com/simplex-chat/simplex-chat/pull/7382 earlier names designs | their choice of one-time purchases is kept; blind-signed codes are left out (section 9) |
| https://github.com/simplex-chat/simplex-chat/pull/7932 app test backend | used by the app tests |

## 4. The shapes of the sale

Each shape is described under the same headings, so each can be judged alone. "Tier" means the name-length price class: 6 letters, 7 letters, 8 or more.

### 4.A Auto-renewing subscription

- **Products first.** Apple: a subscription group "SimpleX names", separate from the badge group, with one yearly product per tier (`NAME_SUB_8_YEAR_01`, `NAME_SUB_7_YEAR_01`, `NAME_SUB_6_YEAR_01`); a year is the longest period either store offers. Play: one subscription per tier with one yearly auto-renewing base plan. The two-year contract floor cannot be a store period: the first purchase pays a year and the service registers two, fronting the second from its allowance.
- **Consoles.** App Store Connect: a group, three products, review notes. Play Console: three subscriptions, a yearly base plan each, Resubscribe off, Pause off (the badge lessons), grace period and account hold set deliberately.
- **More than one name.** Apple allows one active subscription per group, and Play one purchase per subscription product, so a store account can renew one name per tier set. A second renewing name needs a second group and a second set of products, a "slot" per name.
- **Service verifies and stores.** As for badge subscriptions: the original transaction or purchase token, its state on every renewal (App Store Server API, Play `subscriptionsv2`), at most hourly; plus what badges lack: which name each subscription renews, and a renewal sender that calls `renewWithCredit` once per paid period.
- **The user sees.** "Register for $X a year · renews until you cancel"; managing and cancelling in the store's settings; on cancel, the name stays until its on-chain expiry, at least a year after the last payment because of the fronted year.
- **Renewal, refund, expiry.** Renewal is automatic; billing retry and the store's grace period delay it; a failed renewal stops extending and the name lapses at its expiry plus the 90-day grace. A refund cannot take an extension back. Changing tier means nothing for a name whose length is fixed, so upgrades must be blocked. A name released after its grace can be re-registered by anyone, its old subscription still billing unless cancelled.
- **Reaching the registration.** The first verified charge credits a registration of two years; each later charge sends one renewal.
- **iOS.** The same test-environment problem as every store shape (section 6.8).
- **Cost.** The whole badge subscription machinery again (renewal checks, billing retry, pause, resubscribe, new Play tokens, plan changes), plus name binding, slots, a renewal sender, and a year of allowance per first purchase. Store commission on every renewal; one renewal transaction's gas per name per year.

### 4.B Fixed term that does not renew, tracked by the store

- **Products first.** Apple: a non-renewing subscription per tier and term (`NAME_8_2Y_NR_01` …); Apple does not track its duration, the app does. Play: a prepaid base plan on one subscription product per tier; a prepaid plan grants a period without renewing and can be topped up, but its period is at most a year, so two years needs a year plus a top-up, or a fronted year.
- **Consoles.** App Store Connect: three non-renewing products. Play Console: three subscriptions with a prepaid yearly base plan each.
- **More than one name.** Apple: a non-renewing product can be bought again at any time, one name per purchase. Play: a prepaid plan is one entitlement per account and product; a second purchase tops up the first instead of buying a second name.
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
  | 8 letters or more | 2 years | `NAME_8_2Y_01` | `name_8_2y_01` | the site's: $100 |
  | 7 letters | 2 years | `NAME_7_2Y_01` | `name_7_2y_01` | $200 |
  | 6 letters | 2 years | `NAME_6_2Y_01` | `name_6_2y_01` | $400 |

  Renewal products ("add a year" per tier) come in the later run of section 6.7.
- **Consoles.** App Store Connect: three consumables with review screenshots and notes. Play Console: three one-time products; the app consumes each after its name registers. Nothing to switch off: one-time products have no Pause or Resubscribe.
- **More than one name.** Each name is one purchase; a product is bought again once the previous one is consumed (Play) or finished (Apple).
- **Service verifies and stores.** The receipt with the existing verifiers; the product decides the tier and years, through a names catalog beside the badge one; the payment row (unique per store transaction) and one prepaid name code row holding only the hash the app sent, paid, bound to that payment. Nothing else is new: registration then runs as for any prepaid code.
- **The user sees.** Section 6.5: the name, where it points, the price for two years and its end date, "Nothing renews automatically"; the store's own sheet; then committing, waiting and registering; then the name on their address.
- **Renewal, refund, expiry.** No automatic renewal: the app warns before expiry (section 8.3) and sells "add a year" later. A refund before the name registers leaves an unspent code, which the service can revoke; after it registers, the name stays to its expiry, since nothing can take it back, and the loss is the allowance and the gas. Expiry and release as every shape: grace, then free to anyone.
- **Reaching the registration.** The receipt becomes a paid prepaid code the app already holds; the app registers with it (section 6.4).
- **iOS.** As every store shape (section 6.8).
- **Cost.** Service: a catalog of six entries, one command, one table column. Core: one command and the purchase record. Apps: the product list, the purchase lane copied from badges, the pay button and three states. Edge cases: a lost answer, a pending payment, a name taken meanwhile, a refund after registration. None of the subscription surface.

### 4.D The site only, on every build

- **Products first.** None in the stores. The site sells its two-year term; the app takes an owner code or a prepaid code.
- **Consoles.** Nothing.
- **More than one name.** As many as the site sells.
- **Service verifies and stores.** Nothing new for payment; the owner-code transfer relay (needed by every shape for names bought on the site).
- **The user sees.** Section 7: "Buy on simplex.domains" where a link out is allowed, a code field everywhere, and a deep link back.
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
- One **service command**, purchase a name code: a store receipt and the hash of a code the app made → a paid prepaid code row bound to that payment. Serves AC1 and AC2.
- One **column**, the payment that paid a prepaid code (`name_prepaid_codes.payment_id`, unique). Serves AC2: one receipt, one code.
- One **service command**, the names catalog for the app: whether the sale is open, the products, the years and term start, the terms version. Serves AC6.
- One **core command** that runs a store purchase: makes the code, keeps it with the purchase, sends the receipt, and hands the code to the registration of PR 7530. Serves AC1 and AC2.
- The **purchase lanes** in the apps, copied from the badge lanes. Serve AC1 and AC4.
- The **owner-code redemption** and the deep link `simplexchat:/name/code/<code>` (section 7). Serve AC5.

Removed or simplified: nothing; the plan adds the smallest path next to the code path.

Alternatives considered for the bridge from a receipt to a registration, besides the code:

- A fourth payment kind in the registration, "store receipt", next to invoice, code and balance: every phase, the taken and failed handling and Try again would gain a case; a code already has them.
- The service makes the code and returns it: the service would have to keep the code, or a lost answer would lose the payment; when the app makes it and sends only its hash, a lost answer is just sent again, and the service never sees the code, as on the badge site.
- Credit bound to the app's purchase key instead of a bearer code: no backup for the user, and a new credit store in the service.

### 6.2 How a store purchase reaches the registration

```
App                                Store              Store service                    Ethereum
 | names catalog ------------------------------------> sale open? products, years, terms
 | search the name (lookup through the resolver, as today)
 | make code C, keep it with the purchase
 | buy product (tier, 2 years) ----> sheet, payment
 | <-------------------------------- receipt
 | purchase name code {receipt, hash(C)} -----------> verify receipt with the store
 |                                                    product -> (tier, 2 years)
 |                                                    payment row, code row {hash(C), paid}
 | <------------------------------------------------- code paid
 | register with C: order, commit (PR 7530) --------> commit -----------------------------> commit
 |   wait at least 60 s
 | reveal, signed intent ---------------------------> registerWithCredit -----------------> name to the wallet account
 | <------------------------------------------------- registered, expires
 | claim the name on the address or channel; finish (Apple) or consume (Play) the purchase
```

### 6.3 The service

- **Catalog.** `storeNameProduct :: PaymentProvider -> Text -> Maybe NameProduct`, `NameProduct {minLength, years}`, beside `storeProduct`, holding the six launch products.
- **Command "purchase a name code".** `{payment :: ServicePayment, codeHash}`, signed with a fresh purchase key as badge requests are. In order: refuse while the sale is closed or codes-only; verify the receipt with the existing verifier (refusals as for badges: invalid, pending, test purchase); look the product up in the names catalog (unknown: refused); in one transaction insert the payment (unique by store and transaction) and a prepaid code row `{code_hash, min_length, years, paid, payment_id}`; acknowledge on Play before answering, as badges do. Answer: the tier, the years and the code's state.
- **Repeats.** The same receipt and the same hash: the same answer, whatever its age. The same receipt with another hash: when the code it paid is unused, the old hash is revoked and the new one takes its place (a reinstall that lost the code; only the store account that holds the receipt can present it); when the code is reserved or spent, `code_used`.
- **Catalog command for the app.** The answer the site's search gives, without a name: sale open, codes-only, years, term start, terms version and address, and the product identifiers.
- **Refunds.** Nothing automatic at launch: the time from payment to registration is minutes, refunds come days later, and a registered name cannot be taken back. Support handles requests as for the site (`//refund`). A receipt already refunded when it is presented is refused by the existing verifiers (Apple's `revocationDate`, Play's purchase state); nothing re-checks it afterwards.
- **No change** to registration, codes, phases, the worker or the web API.

### 6.4 The core

- The purchase record: product, code (kept with the database, so a database backup or migration carries it), store transaction, state (made, paid, used), and the registration it funds.
- `APIPurchaseNameCode {userId, payment :: ServicePayment, code}` (or its reply to a stored purchase): sends the receipt and the code's hash, records the answer, and returns the code's state; the apps then start the registration of PR 7530 with that code. Repeats are safe (6.3).
- When the app starts with an open store purchase and no stored code (a reinstall), it makes a new code and presents the receipt again, which re-keys an unused credit.
- The wallet account is created right before the first purchase (the recovery key screen of the phase 3a canvas), not at app start.

### 6.5 The apps

Android first; iOS gets the same code behind a switch, off until section 6.8 is decided; desktop and F-Droid never show store purchases.

- **Products.** Loaded from the store by the identifiers in the catalog answer; the confirm screen shows the store's price in the store's currency.
- **Purchase lane.** Android: `launchBillingFlow` for the tier's product; the result goes to the core command; the purchase is consumed when its name registers. iOS: `product.purchase()`; the transaction is finished when its name registers. Neither sets an account identifier that is the same across purchases.
- **An open purchase** (paid, name not registered) is found at start from the store's own purchase list (Play's purchases query, Apple's unfinished transactions) and continues. Play won't sell the same product again until it is consumed; on both platforms the app offers "Choose its name" for an open purchase of the tier instead of a second payment.

#### Screens

Drawn on the design page. Words as on the phase 3a canvas; one main action at the bottom, in the same place in every state.

1. **Register a name, available** (canvas 3–5): THE NAME `dynamis.simplex` · POINTS AT Alice, your address · YOU PAY $99.99, 2 years · "Paid once, until 31 Dec 2028. Nothing renews automatically." Main: "Register for $99.99". Secondary: "Use a code". The price is the store's; the end date is `max(1 Jan 2027, today) + 2 × 365 days`. Shortest and longest: a 6-letter name at the top tier; a 63-letter name wraps under THE NAME without moving the button.
2. **Terms** (canvas 6) and **recovery key** (canvas 7): once per device, before the first payment.
3. **Store sheet**: the platform's. Cancelling it returns to screen 1 with nothing charged.
4. **Payment pending** (Play pending purchase, Apple Ask to Buy): "Waiting for the store to confirm your payment. The name is registered as soon as it does." The main button reads "Done" and the registration starts by itself when the store confirms.
5. **Registering** (canvas 9): committing, waiting 60 s, registering; survives closing the app (canvas 9a).
6. **The name is yours** (canvas 10): "dynamis.simplex points at Alice until 31 Dec 2028."
7. **Someone was faster** (canvas 10a): "Someone registered dynamis.simplex a moment earlier. Your payment is kept: choose another name of 8 letters or more." Main: "Choose another name".
8. **Paid, not registered** (canvas 10b): "Your payment is kept and dynamis.simplex is still free. Try again; you will not be charged twice." Main: "Try again".
9. **Store did not confirm** (refused receipt): "The store did not confirm this payment. If you were charged, contact SimpleX Chat with this reference." Main: "Contact SimpleX Chat"; the reference is the store's order number.
10. **Sale not open / codes-only**: the price row reads "Names can be paid with a code until <date>"; only "Use a code" is offered.
11. **A paid name waiting** (an open purchase on start): "You paid for a name of 8 letters or more for 2 years." Main: "Choose its name".

Money and irreversible steps: the store sheet is the explicit act; the code is kept before the sheet opens, so an interruption after payment is recovered from the store's purchase list; one store transaction makes one code and one registration (AC2).

### 6.6 What iOS shows meanwhile

The badge pattern, done as the storefront rule says (section 7): no purchase button; "Buy on simplex.domains" only in the US storefront; "Enter a code" in every storefront.

### 6.7 Renewal, designed for the later run

- "Add a year" one-time products per tier (`NAME_8_1Y_RENEW_01` …), sold from the name's row and from the expiry warning, available to the owner during the term and the grace period.
- The service verifies the receipt as above and sends `renewWithCredit` for one year, with no owner signature; it checks the name is held by the device wallet that asks, so a stranger's renewal of someone else's name is not sold by mistake (renewing it would still be harmless to the owner).
- Shorter steps (one or three months) are possible on the chain only if renewal has no minimum, which is to be confirmed; they are not recommended, since each costs a store commission on a small price and a transaction's gas.
- Built and released before mid-2028: the first paid names expire on 31 December 2028.

### 6.8 iOS and Apple's test environment

Every purchase made by App Review or a TestFlight tester is a test purchase, which the service refuses in production. For names, crediting one would register a real `.simplex` name for nothing. Options:

- **Register test purchases in the `.testing` namespace** (recommended to switch purchases on): the reviewer completes the whole flow and receives `dynamis.testing`, shown as such; real payments register `.simplex`. Cost: the service holds a second chain configuration for the testing deployment and routes by the receipt's environment; the confirm screen shows the namespace it will register.
- Keep the badge pattern until Apple's environment is handled for badges too (the default at launch).
- Refuse test purchases: App Review cannot complete a purchase.
- Show the browser path to test builds only: Apple treats a feature hidden from review as grounds for rejection (guideline 2.3.1), as the badge work found.

### 6.9 Where badges and names meet

Nowhere at launch: separate products, no discount, no shared purchase. A name never unlocks an app feature, and a badge never pays for a name. They share the store service, the receipt verifiers and the store account's purchase history (section 9).

## 7. The browser path

For iOS until its purchases are on, desktop and F-Droid, and for every user who bought on the site.

- **Primary action, "Buy on simplex.domains"**: shown in desktop and F-Droid builds; in the iOS build only in the US storefront (the badge's `badgeBrowserAllowed`, reused); never in the Android Play build, which sells in the app. It opens `https://simplex.domains/` with the searched name in the fragment, which the page reads and never sends.
- **Secondary, "Enter a code"**: every build. One field takes an owner code (`SN-` and 30 characters) or a prepaid code (`SN<length>-` and 20); the core's existing parser tells them apart. A prepaid code goes to the registration of screen 1; an owner code moves its name to the device wallet.
- **Back from the page**: after the name registers, the site's "Open in SimpleX" opens `simplexchat:/name/code/<owner code>`, which the app routes beside `/badge/code/` and confirms before acting: "Add dynamis.simplex to this app? It moves from the site's wallet to this device's wallet." Then: moving (two transactions the service relays), then the name is yours. If the name's records point elsewhere, the app offers to point it at this profile's address (one relayed edit).
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

- The name's row on the address or channel screen always shows "Expires 31 Dec 2028".
- From 30 days before: the row turns to "Expires in 21 days" with Renew; a chat-list banner, lowest priority in the one banner slot, dismissible once a week; a local notification at 30 and 7 days.
- In the grace period: the existing alert, "Your name dynamis.simplex expired on 31 Dec 2028. Renew it before 31 Mar 2029." Renew opens the renewal product (6.7), which exists before the first paid names expire; in builds that link to the site, the site.
- After the grace: "Re-register", from the existing alert.

## 9. Compatibility and security

- **Protocol**: the store service's app protocol gains two commands; the service version range moves, as badges' did. Chat protocol and agent: unchanged.
- **Database**: the core gains a purchases table (one migration, SQLite and Postgres); the service gains one column on prepaid codes.
- **Older apps**: unaffected; they never send the new commands.
- **Threat model.**
  - *The stores* learn that an account bought "SimpleX name, 8 letters, 2 years" and when; never the name, which the app never gives them.
  - *The store service* learns the store transaction and, at the reveal, the name and the wallet address it registers to, so SimpleX can link a payment to a name, as on the site. It never learns who the buyer is: a signed Apple transaction and a Play purchase carry no account identity, and the app sets no account identifier that repeats across purchases. The service could ask Apple for every purchase of the same Apple account from one transaction (App Store Server API transaction history), which would link that account's names and badges; the design never calls it, and the threat model states that it is possible.
  - *Chain observers* see each name's owner address; one account per name keeps two names of one person unlinked on the chain.
  - *A forged or replayed receipt* is refused by the existing verifiers; one transaction funds one code (the unique payment); the tier comes from the product, never the client.
  - *A stolen code* before registration pays someone else's name; the code lives in the app's database, as wallet keys do.
  - *A refund after registration* gives a free name until expiry, at the cost of the allowance and the gas; accepted and watched in the stores' reports.
  - *A test purchase* never registers a `.simplex` name.
  - *The owner code in a link back*: whoever has it owns the name until it is moved; the app moves it at once, after which the code controls nothing.
  - Not done: blind-signed codes, which would hide which payment bought which name from SimpleX; a name is public and its owner address is public, so the gain is small, and it is listed as later work.

## 10. Slices

Each slice is a later implementation run. Prerequisites outside this plan, owned by the names work: the device wallet on master; signing in the core; registration from the app with a prepaid code (PR 7530). Estimates are for implementing, building and testing, in this container's pace.

1. **Service: names catalog and the purchase command** (behavioural · medium · high risk: money · about 4 hours). Catalog, command, payment-to-code binding, repeats, codes-only and sale-closed refusals. Tests with the fake App Store and Play verifiers.
2. **Core: purchase record and command** (behavioural · medium · high risk: money · about 4 hours). Migration, command, re-presenting a receipt after reinstall, handing the code to registration. Tests against the service with fake stores.
3. **Android purchase lane and screens** (behavioural · medium · low risk · about 6 hours). Products, sheet, pending, open purchase, consume after registration, screens 1–11; previews per state.
4. **iOS lane behind a switch, and the iOS browser path** (behavioural · medium · low risk · about 4 hours, not compiled in the container). The same screens; storefront rule; previews in both storefront states.
5. **Owner code redemption and the link back** (behavioural · medium · high risk: transfers · about 6 hours). Core: the two signatures and their relay; deep-link path, confirmation, transfer progress, pointing the records; with the site's "Open in SimpleX".
6. **Expiry display and reminders** (behavioural · small · low risk · about 2 hours).
7. **Later: renewal products** (6.7), and **`.testing` for Apple test purchases** (6.8) when iOS is switched on.

Console work for slice 3 and 4: the six products, review notes; for iOS, the products stay unsubmitted until the switch.

## 11. Test plan

- Service: one receipt one code; the same receipt resent; re-keying an unused credit; refusing a used one; a test purchase refused; an unknown product refused; a 7-letter name with an 8-letter code refused by the existing code check; codes-only and closed sale refused before any store call.
- Core and service end to end, fake Play and fake chain: AC1 and AC5 flows; taken then another name; failed then Try again; a lost answer.
- Apps: previews for screens 1–11 in light and dark, shortest and longest names, both list layouts not needed (no list change); iOS storefront US and not US.
- Pace: slices 1 and 2 built and tested together with one lean review after each (money); slices 3–6 batched; one review loop over the whole at the end.

## 12. Risks

- **The two-year floor is read from the service, not the contract.** The first slice's tests run against the contracts' test deployment and show a 729-day registration refused.
- **Renewal minimum unknown** until the contracts are read; only renewal design depends on it.
- **App Review and the consumable type.** Apple may read a two-year registration as a time-limited service that should be a non-renewing subscription; the product type is fixed when created, so it is settled before the products are made (question 4).
- **Store policy on blockchain items** (Apple's rules on non-fungible tokens, Google's on tokenized assets) may require wording or disclosures; checked against the policies quoted in the research before the products are submitted.
- **Refund abuse** after registration: watched; the price makes it costly to repeat.

## 13. Open questions

Each carries the recommended answer this plan uses.

1. **How the apps sell names.** Recommended: one-time products per term (4.C). If subscriptions instead: sections 4.A's slots, name binding and renewal sender are added, and the product table changes.
2. **Terms sold.** Recommended: two years only, as the site. If five years too: three more products and a term picker on screen 1.
3. **One, three or twelve months.** Recommended: not sold; a first registration cannot be shorter than two years, and renewals go by whole years. If wanted as renewals: confirm the contract allows them, then shorter "add" products.
4. **Apple product type.** Recommended: consumable, because what is sold is one registration, spent once. If App Review asks for a non-renewing subscription, the same service path takes it with three new product identifiers.
5. **Price in the stores.** Recommended: the site's prices, so a name costs the same everywhere; the store's commission is SimpleX's cost. If the commission is added: store prices above the site's, and the confirm screen shows the store's.
6. **iOS at launch.** Recommended: the badge pattern (site in the US storefront, a code everywhere), then switch purchases on with test purchases registered in `.testing`. If purchases at launch: the `.testing` routing becomes part of slice 1.
7. **Bringing in a name bought on the site.** Recommended: the owner code, typed or by link, moves the name to the app. If orders made by the app and paid on the page instead: order commands in the app and a page mode, and the dropped web flow returns.
8. **Where the site link shows.** Recommended: US storefront on iOS, never in the Android Play build, always on desktop and F-Droid. If everywhere, as the iOS badge screen does today: App Review may refuse the app outside the US.
9. **Refunds after registration.** Recommended: the name stays; nothing automatic. If refunds should be prevented: the service would have to wait days before registering, which nobody wants.
10. **Expiry reminders.** Recommended: the date on the name's row, from 30 days a row warning, a banner and local notifications, and the existing grace alert. If fewer: the row only, and more names lapse.
11. **Moving names.** Recommended: names follow the wallet (recovery key, database migration); per-name moves later.
12. **Badges and names.** Recommended: no interaction.
13. **Codes-only mode and the sale date.** Recommended: in-app purchase follows the store's codes-only switch and the sale date; before it, screen 1 offers only a code.
14. **An unregistered paid purchase.** Recommended: kept open in the store until its name registers, so a reinstall recovers it and a second payment is never taken for it.
15. **When renewals ship.** Recommended: "add a year" products released before mid-2028, after the contract's renewal rules are checked. If at launch: the renewal design and the contract check move into the first runs, for names that expire two years later.

## 14. Out of scope (noticed, not doing)

- The iOS badge screen opens the badge site in every storefront, while its own comment says the link out is US-only (`BadgesSupportSimplexView.swift:71-78` against `BadgeStore.swift:37-41`): for the badge owners.
- Name alerts open `https://simplex.domains/#testing` for Register, Renew and Re-register, including for `.simplex` names (on `names`): to follow the in-app purchase.
- Nothing warns before a name expires, and the core does not store an own name's expiry (section 8.3 covers names bought in the app).

## 15. Follow-up runs

The slices of section 10, in order, each its own run and pull request; slices 1 and 2 can share one.
