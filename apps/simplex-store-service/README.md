# SimpleX store service

`simplex-store-service` runs the whole store service as one process, two
lanes: a SimpleX chat bot that answers service RPC over a double-ratchet contact
address, and — only when `--service-config` names a `store_service.ini` — the badge-codes
web checkout (BTCPay and Stripe payment, code issuance, the poller). **Without
`--service-config` the web listener does not start at all**; the process still runs the
chat side and nothing else.

The wire protocol for the RPC side is
[`docs/protocol/badges-rpc.md`](../../docs/protocol/badges-rpc.md). The web checkout is
specified in
[`plans/badges-codes/2026-08-27-badge-codes.md`](../../plans/badges-codes/2026-08-27-badge-codes.md),
whose §9 is the `store_service.ini` reference, with the implementation plan in
[`plans/badges-codes/2026-08-31-service-btcpay.md`](../../plans/badges-codes/2026-08-31-service-btcpay.md).
Earlier plans (`2026-07-30-supporter-badges-v3-ux.md`, `2026-07-31-badges-core-implementation.md`,
`2026-08-04-badges-mvp-scope.md`) predate the web checkout and describe the RPC-only
scaffold this service started as.

At this stage the service:

- creates a double-ratchet contact address on first start (service RPC requires DR, see [`docs/protocol/badges-rpc.md`](../../docs/protocol/badges-rpc.md)),
- listens for service requests (`CEvtServiceRequest`) on that address, rejects a request whose `purchaseKey` is not the key the agent verified the signature against, and answers `redeemBadgeCode`,
- issues redemption codes, storing only their `SHA-256` in its code table,
- does not accept contact requests: the address is for RPC only,
- in service mode with `[group]` in the ini, manages one SimpleX group and serves `/issue`, `/bulk`
  and `/revoke` in it (see [Issuing codes](#issuing-codes)),
- in service mode with `--service-config`, also serves the built web app (`npm run build` in `web/`) on `badges_port` and, with `[names]`, the names site on `names_port` (see [Two sites](#two-sites)), `POST /api/invoice` and `GET /api/invoice/:id`, the BTCPay and Stripe webhook routes, and a payment poller, seeding its price/offer catalog on every start,
- owns the `sx_badge_service_`-prefixed tables and its own migrations table (`sx_badge_service_migrations`).

Every other command answers `unsupported_version`, or `unknown_purchase_key` when the key that
signed it is not one the service has stored — every command but `redeemBadgeCode` needs a
purchase that already exists.

## Build

Build prerequisites and the general contribution flow are in [`docs/CONTRIBUTING.md`](../../docs/CONTRIBUTING.md).

```
cabal build exe:simplex-store-service
```

## Run

```
simplex-store-service --help
```

- default (no `--run-cli`): background service mode, no interactive terminal.
- `--run-cli`: interactive CLI that also processes service requests (mirrors
  `simplex-directory-service --run-cli`). This mode is the chat/RPC side and the `//` commands
  below: it starts no web listener and no poller, and serves no group commands, whatever
  `--service-config` says. It still updates a code's group message when the code is redeemed or
  revoked. `[apple]`, `[google]` and `[dev]` apply to it.
- `--no-address`: skip address creation on start-up (for operators who provision the address themselves).
The service cannot sign credentials without an issuer key and refuses to start without one:

- `--issuer-key-idx IDX` — the index the apps find the matching public key under (`badgePublicKeys` in `ChatConfig`).
- `--issuer-secret SECRET` — the issuer secret from `simplex-chat badge keygen`.

The service checks the secret against the configured public key at that index and refuses to start
if they disagree: credentials signed with the wrong key cannot be verified by any client, and the
codes redeemed against them would be spent for nothing.

The key can come from `store_service.ini` instead:

```ini
[issuer]
index = 1
private_key = <secret from `simplex-chat badge keygen`>
```

`index` is the index clients verify against (`badgePublicKeys` in `ChatConfig`), and `private_key`
is the secret that signs. Both are required, and the key is checked at startup like the flags.
Rotating is a change to both and a restart, once clients trust the new index. The service logs the
index it signs with at startup.

The command line wins over the file when both `--issuer-key-idx` and `--issuer-secret` are given.
Note that a secret passed as a flag is visible to every user on the machine through `ps`, where one
in the ini is only as readable as the file: `store_service.ini` is gitignored and already holds the
provider API keys and webhook secrets.

For a local run, where no client key's secret is at hand, `[issuer] dev_keys` lists keys the service
trusts beside the clients' keys, as `<index>:<secret>` entries separated by commas, each secret from
`simplex-chat badge keygen`:

```ini
[issuer]
index = 999
private_key = <secret>
dev_keys = 999:<secret>
```

With `dev_keys`, the service refuses to start:

- unless `[names]` is on and `[chain] chain_id` is 31337, anvil's local chain;
- unless the node at `rpc_url` answers `eth_chainId` with 31337 as the service starts;
- when `[stripe]` holds a live key: `secret_key` must be a test key (`sk_test_` or `rk_test_`), and
  `publishable_key` too (`pk_test_`);
- when `[btcpay]`'s `host` is not on loopback (`127.0.0.1`, `::1` or `localhost`);
- when a dev key has the index of a key clients trust.

So a run with dev keys takes test payments only: Stripe in test mode, and a BTCPay on the same machine,
such as one on regtest. `host` must still be https, so put a TLS proxy in front of a local BTCPay and
have the service trust the proxy's certificate authority with `SYSTEM_CERTIFICATE_PATH`.

It logs a warning naming the dev keys' indexes at every start. Clients never trust a dev key, but the
service's own chat core does, for the badges it receives, so a contact's badge signed by a dev key is
accepted in that run. Leave `dev_keys` out everywhere else.

Other options:

- `--service-config INI_FILE`: path to `store_service.ini`. Omit it to run the chat/RPC side
  only; the process never starts a web listener without it, and never starts one under
  `--run-cli`, which parses and validates the whole file (`[listener]` included) but
  uses only its `[issuer]`, `[apple]`, `[google]` and `[dev]` sections. An issuer key is
  still required either way.
- `--service-name NAME`: the bot's display name, without `*`s or spaces (default `SimpleX Badges`).
- `--client-service`: use the client service certificate.
- also accepts the standard SimpleX Chat core options — database path, SMP/XFTP servers,
  `--socks-proxy`, `--log-level`/`-l`, and the rest — run `simplex-store-service --help`
  for the complete list.

### Running the web checkout

`store_service.ini` holds the listener bind address, its ports and `badges_static_dir`, an optional
`[btcpay]` section (omitting it disables Bitcoin and Monero), an optional `[stripe]`
section (omitting it disables card payments), an optional `[group]` section (omitting it
turns off the group's commands, though a group created earlier still has its code messages updated),
the poll cadence, optional `[apple]` and `[google]` sections that verify store purchases (see
[Store purchases](#store-purchases-app-store-and-google-play)), and optional `[names]` and `[chain]` sections for the names store (omitting both
turns it off), whose `[chain] service_key` is [the gas key](#the-gas-key). An optional `[community]` section gives
what each community name is for (see [Terms and community names](#terms-and-community-names)). Write every setting as
`key = value`: the parser also reads `key: value`, but drops the first character of its value. A
key written twice in one section, or a section header written twice, stops the service at startup.
`store_service.ini.example` is the committed template; `store_service.ini` itself is
gitignored, since a real one holds API keys and webhook secrets.

The full walkthrough is in
[`web/README.md`](web/README.md#running-the-real-service-against-this-build). Short version:

```
cd apps/simplex-store-service/web && npm install && npm run build && cd ../../..
cp apps/simplex-store-service/store_service.ini.example apps/simplex-store-service/store_service.ini
cabal run simplex-store-service -- \
  --issuer-key-idx IDX --issuer-secret SECRET \
  --service-config apps/simplex-store-service/store_service.ini
```

`IDX` and `SECRET` must be one of the issuer keys clients already ship, not a fresh
`simplex-chat badge keygen` pair: startup refuses a key no client could verify against.

`trust_forwarded_for` decides what the rate limiter counts. Behind a reverse proxy it must be
`on`, or every request keys on the proxy's address and the whole service shares one bucket of 60
reads and 5 checkouts a minute. Where the listener is reached directly it must be `off`, because
then the header is whatever the caller wrote. The last entry is the one read, which is the one a
proxy appends.

The BTCPay API key needs four permissions, each scoped to the one store:
`cancreateinvoice` and `canviewinvoices` for checkout and the poller,
`canviewstoresettings` to log the store's live payment methods at startup and to read the store's
rates for late crypto credits, and
`canmodifyinvoices` so `POST /api/invoice/:id/cancel` can invalidate an invoice at BTCPay
rather than only in this store.

**Do not set `speed_policy = HighSpeed` when names are sold** (`[names]` configured). Under it
BTCPay can mark a payment `Settled` with no confirmation, so the service never credits crypto that
reaches a name invoice after the invoice closed: the money stays pending, the poller alerts about it
once it is older than its read window (about 72 hours), and support settles it by hand. The service
judges each invoice by the policy it was created with, so changing the setting later credits nothing
that arrived under `HighSpeed`. With `MediumSpeed` (the template's value) or a slower policy, late
bitcoin is credited to the order's balance once confirmed.

A credit is the money's share of the invoice's price: the price times the crypto received, divided by
the crypto the invoice asked, which includes the network fee BTCPay adds (to every Monero invoice, and
to a Bitcoin invoice after a part payment), so the fee is never credited. It is also taken at the
lower of the invoice's rate and the rate when it is credited, rounded down to the cent. So a whole
late payment credits at most the price. A current rate below half the invoice's credits nothing: the
money goes to support for good.

**Overpayment.** Crypto above what an invoice asks is credited to the order's balance in the same
way, once confirmed, also on an invoice paid in time: the extra's share of the price, at the lower
rate. The service logs it at info as `names: invoice <id> was overpaid; credited <cents> cents of the
extra to the balance of registration <id>`, and an extra it cannot credit is alerted as late money is.

BTCPay keeps an invoice it has closed `Expired` (or `Invalid` after a cancel) when money arrives late.
The invoice view reports `paidInFull` on such an invoice once BTCPay reads it `PaidLate` or
`PaidOver`, or its due is 0 or less, and reports an overpaid invoice's `cryptoAmountDue` as `0`,
never below.

**Set the Monero plugin's settled-confirmation threshold (`InvoiceSettledConfirmationThreshold`) to
at least 1.** BTCPay's API does not show how many confirmations a Monero payment has, only that
threshold, so late Monero is credited only on an invoice made while it was 1 or more; it is then
credited as late bitcoin is. Left unset (the plugin's default) or at 0,
every late Monero payment stays pending and goes to support, as under `HighSpeed`. Each invoice
keeps the threshold it was made with, so raising it later credits nothing already paid.

Give the service a BTCPay store of its own, and a Stripe account of its own if card payments are
on. The poller lists every invoice and payment intent of the last three days or so, and one it did
not create and cannot read is reported in a warning once an hour until it falls out of that window.

### Card payments (Stripe)

An optional `[stripe]` section enables the card lane; omitting it disables card payments
and `POST /api/invoice` answers `provider_unavailable` for a card order, the same as an
absent `[btcpay]` does for Bitcoin and Monero. Its keys:

- `secret_key` — a **restricted** key (`rk_...`), never a full secret key (`sk_...`). Grant it
  **PaymentIntents: write**, to create and cancel intents, plus **Charges: read** — the poller
  reads an intent with `expand[]=latest_charge`, and Stripe rejects the whole read if the key
  lacks read on an expanded object (settlement then falls back to the slower list pass).
- `publishable_key` (`pk_...`) — the public key the browser mounts the Payment Element with.
- `webhook_secret` (`whsec_...`) — the signing secret of the `/webhooks/stripe` endpoint,
  configured in the Stripe Dashboard alongside it.
- `session_minutes` — how many minutes an unpaid card order stays open (1 to 1440, default 60).
  Ten minutes after that, the service cancels the payment at Stripe, and retries if that fails.
  If the order is over 72 hours old and the cancel has kept failing for an hour, the service marks
  it expired and logs an error. Check such orders in the Stripe Dashboard. After a restart, the
  hour starts again.

The service creates each payment intent with `metadata[order]`, the order's reference:
`name:0x<order hash>` for a name order (`order_hash` in `sx_badge_service_name_registrations`) or
`badge:0x<code hash>` for a badge (`code_hash` in `sx_badge_service_badge_codes`). Neither is a secret,
and support finds an intent's order from it in the Dashboard. The create also sends an
`Idempotency-Key` of that reference with the amount and currency, so a buyer who sends the same order
again after the first answer was lost gets the same intent, not a second one; Stripe keeps the key
for 24 hours.

The service fills `publishable_key` into the shell at boot: the built `index.html` ships
with an empty `<meta id="stripe-publishable-key" name="stripe-publishable-key" content="">`
(`web/public/index.html`), and the listener serves a copy with the configured key
substituted in, so the ini is the only place the key is set. The names site's shell carries the
same meta without the `id`, and is filled the same way. With no `[stripe]` section the
pristine (empty) shell is served and the page shows its development stand-in card form. The
dev mock (`web/mock/server.py`) does the same substitution from `$STRIPE_PUBLISHABLE_KEY`,
since it runs without an ini.

`POST /webhooks/stripe` verifies `Stripe-Signature` against `webhook_secret` and queues a
read, the same hint-only role as `POST /webhooks/btcpay`: the poller carries authority, so
an unverified or unreadable delivery costs nothing but a log line. A delivery signed more than
15 minutes away from the server's clock is refused with a warning, so keep the clock synced (NTP).

The reverse proxy in front of this service must send a Content-Security-Policy that
allows Stripe.js and its iframes, since Stripe forbids bundling or self-hosting its
script:

```
script-src 'self' https://js.stripe.com https://*.js.stripe.com;
frame-src https://js.stripe.com https://*.js.stripe.com https://hooks.stripe.com
```

The full policies of both sites are in [Caddy](#caddy).

**Stripe Link must be disabled in the Dashboard.** Left on, it needs `link.com` in the
CSP above and reintroduces an email prompt of its own, which this design otherwise avoids.

**Enable card only; do not enable redirect-based methods** (iDEAL, Bancontact, PayPal, and
the rest) in the Dashboard. The embedded checkout sends no `return_url`, which Stripe requires
the moment a redirect-based method is offered, so enabling one makes every checkout fail to
create. The service pins the Stripe API version it was built against, so the account's default
version does not affect the card lane.

### The checkout endpoints

The browser in `web/` is the only client of the `/api` routes; the two webhooks below are the
providers', one each for BTCPay and Stripe.
The invoice id is the only credential for reading or cancelling an order: it is 16 random bytes,
it travels in the path, and anything that logs request paths logs it. It is also what the buyer
is shown as their reference and asked to quote, so support sees it: losing it lets someone cancel
an invoice, which is why the code, which lets them take the badge, is never printed until the
service says the invoice is paid. The service keeps it out of its own logs, which name the
provider's reference instead.

| Route | Answers |
|---|---|
| `POST /api/invoice` | `{invoiceId, badgeType, months, amount, currency, expiresAt}` plus a destination: `clientSecret` for a card, or `address`, `cryptoAmount`, `cryptoCurrency`. Refuses with `code_conflict`, `catalog_changed`, `bad_request`, `provider_unavailable` or `rate_limited`. |
| `GET /api/invoice/:id` | `{status, badgeType, months, amount, currency, expiresAt}` and the same destination — no `invoiceId`, since the caller already has it — plus `amountPaid`, `cryptoAmountPaid`, `cryptoAmountDue`, `paidInFull`, `settledAt` and `requiredConfirmations` once each has a value. Never the code, which this service has never seen. |
| `GET /api/invoice/:id?wait=<status>&seenPaid=<figure>&seenFull=<0\|1>` | The same, held for up to 30 seconds while the invoice's status is still `<status>` **and** its payment is the one the caller says it has rendered. `wait=paid` and any value that is not a status answer at once, since neither can change. A status the caller has not seen, or a payment it has not seen, answers at once — the provider's verdict counts as much as the figure, because Monero reports an invoice as confirming while its figures are still zero. A request that omits `seenPaid` holds on the status alone. |
| `POST /api/invoice/:id/cancel` | Invalidates an open invoice at the provider and expires it here. Refuses a settled or expired one with `not_open`, and one that already holds a payment with `funded`. The provider is told first, and a payment landing in between does not keep the invoice open: nothing can reach that address any more, so the row is expired either way and the poller settles or reports what arrived. |
| `POST /webhooks/btcpay` | Verifies `BTCPay-Sig` over the bytes as received and queues a read. A hint only: the poller is what carries authority, so an unverified or unreadable delivery costs nothing but a log line. |
| `POST /webhooks/stripe` | Verifies `Stripe-Signature` over the bytes as received and queues a read. The card lane's equivalent of the row above, and a hint just the same: the poller carries authority, so an unverified or unreadable delivery costs nothing but a log line. |

Every `/api` refusal is `{"error": "<code>"}`. Besides the codes above, any of them can answer
`internal`, an unknown id answers `not_found`, and a wrong verb answers `method_not_allowed` —
ten codes in all, which is what the browser's `WIRE_ERROR_CODES` lists. The two webhook routes are
the exception: each answers 200, 400 or 413 with an empty body, because its provider is the only
caller and nothing it could read would change what the route does. A wrong verb on any route, those
two included, answers `method_not_allowed`.

### Store purchases (App Store and Google Play)

Each store is verified only when its section is present; without one, every receipt from that store
is answered `provider_not_configured` and the app keeps the purchase to present again. A section
missing a key, a root certificate that cannot be read or is not a root, an unreadable service
account key, or either section beside
`[dev] accept_unverified_store_receipts = on`, stops the service at startup.

```ini
[apple]
bundle_id = chat.simplex.app
root_certificates = /etc/simplex-store-service/AppleRootCA-G3.cer
api_key_id = 2X9R4HXF34
api_issuer_id = 57246542-96fe-1a63-e053-0824d011072a
api_private_key_file = /etc/simplex-store-service/AuthKey_2X9R4HXF34.p8

[google]
package_name = chat.simplex.app
service_account_file = /etc/simplex-store-service/play-service-account.json
```

- `[apple]` verifies the signed transaction offline, against the roots `root_certificates` lists,
  comma-separated, each in DER. Today that is Apple Root CA - G3 as Apple publishes it at
  <https://www.apple.com/certificateauthority/AppleRootCA-G3.cer>. Each must be self-signed.
  Verification reads no validity period, so an expired root keeps verifying. A transaction for
  another bundle id, or an intermediate no listed root signed, is answered `internal` and logged as
  an error rather than refused, since `bundle_id` may be wrong or a root missing; check both when those errors appear, unless the purchase was made in Xcode's
  StoreKit testing, which the App Store does not sign, so no root verifies it. When Apple signs
  under a new root, list it beside the old one rather than in its place: a purchase signed under the
  old root still verifies, and one answered `internal` meanwhile is credited when the app presents
  it again.
- With `api_key_id`, `api_issuer_id` and `api_private_key_file`, `[apple]` also verifies
  subscriptions: it asks the App Store Server API for a subscription's status, with a token signed
  by the in-app purchase key App Store Connect issued (the `.p8` file, its key id and the
  account's issuer id), and checks what Apple signed in the answer against the same roots. Without
  them, one-time purchases verify as before and every subscription is answered
  `provider_not_configured`. The three are set together or not at all. `api_host` defaults to the
  production host, `https://api.storekit.itunes.apple.com`, which answers nothing about a Sandbox
  transaction; a test deployment with `[dev] credit_test_purchases = on` (below) sets
  `https://api.storekit-sandbox.itunes.apple.com`. The service refuses to start with any other host,
  or with the Sandbox host while `credit_test_purchases` is off, as the Sandbox never charges anyone;
  the host's case and a trailing slash do not matter. A 401 or 403
  from Apple, the key refused, is answered `internal` and logged. Keep the key file readable by this
  service alone.
- `[google]` asks the Google Play Developer API about each purchase and subscription, and
  acknowledges it before crediting it, as the service account whose JSON key
  `service_account_file` holds. The account needs both Play Console permissions Google's
  [getting started](https://developers.google.com/android-publisher/getting_started) lists for its
  billing APIs: "View financial data, orders, and cancellation survey responses" to read a
  purchase, and "Manage orders and subscriptions" to acknowledge it. Without the second, a Play
  purchase the app has not already acknowledged itself is answered `internal` and logged as an
  error naming the acknowledgement, and the app presents it again once the permission is granted,
  which must be within three days, before Play refunds it. The key signs in
  for an access token, so keep the file readable by this service alone.
  A request that may ask a store waits on a lane of its own: Play purchases, App Store
  subscription purchases and transfers of a subscription (Restore) on one, and issue requests and
  tier raises for a subscription due its hourly store check on another. A slow or unreachable
  store therefore delays only those, not code redemptions, App Store one-time purchases or other
  renewals; and no purchase or transfer, genuine or made up, delays a renewal check. The purchase
  path asks Play about at most 300 subscriptions a minute (a purchase it credits is read once more
  to acknowledge it), so made-up tokens can spend at most a tenth of the quota renewal checks share.

While a subscription's paid coverage runs, as an annual plan's does for a year, the service asks
its store before it issues each new month, at most once an hour. When the store reports the
subscription ended, or the charge the service granted revoked, the months not yet issued are debited
as a refund and the badge ends with the month already issued; a store that fails or does not answer
ends nothing. A Play refund that leaves the subscription active is not seen: the service does not
read Play's voided purchases.

Which answers are terminal for a purchase and which leave it to be presented again is listed per
store in [`docs/protocol/badges-rpc.md`](../../docs/protocol/badges-rpc.md#commands).

The root certificates, the App Store Server API key and the Play service account key are read only
at startup, so rotating any of them takes a restart. Until then the old key keeps being used, and
each request the store refuses it for is answered `internal` and logged as an error.

Do not roll back the `20261012_badge_subscriptions` migration on a database that holds store
subscriptions. Its down migration drops their columns without refusing: every subscription becomes
a prepaid purchase that is never renewed, every Play purchase token is lost, and every ledger with a
charge, transfer or tier-change row can no longer be read, so its purchase answers `internal`.
Upgrading again restores none of it. Take a backup first if a rollback is unavoidable.

#### Store setup

The subscriptions must be sold under the ids the apps buy and the service credits, and set up as
follows:

- [ ] App Store: one subscription group holding all four subscriptions, so a change of tier or plan
  keeps the original transaction id, and with it the purchase.
- [ ] App Store levels: `SUBSCR_BADGE_LEGEND_MONTH_01` and `SUBSCR_BADGE_LEGEND_YEAR_01` at level 1,
  `SUBSCR_BADGE_SUPPORTER_MONTH_01` and `SUBSCR_BADGE_SUPPORTER_YEAR_01` at level 2. Apple charges a
  move to a higher level at once, and the apps send only Supporter to Legend as a raise.
- [ ] App Store: Billing Grace Period off, and Family Sharing off, which cannot be turned off once on:
  a family-shared subscription is refused.
- [ ] Play: the product `subscr_badge_supporter_01` with the base plans
  `subscr-badge-supporter-month-02` and `subscr-badge-supporter-year-01`, and the product
  `subscr_badge_legend_01` with `subscr-badge-legend-month-01` and `subscr-badge-legend-year-01`.
  A charge for any other base plan is not credited, and a charge for one of these under the other
  product is answered `internal`.
- [ ] Play, on each of the four base plans: Resubscribe off, pausing off, the minimum grace period,
  and account hold on.
- [ ] Both: no offers of any kind: no free trial, introductory or promotional price, or offer code.
  The service credits no App Store charge made with an offer and no Play charge in a free trial or
  introductory price.

### Accepting store receipts unverified, for local testing

```ini
[dev]
accept_unverified_store_receipts = on
```

Without a store verifier every store purchase is answered `provider_not_configured`. With this on,
the service accepts any well-formed receipt without asking or verifying anything: an Apple JWS for
the `transactionId` and `productId` its payload names, unsigned or not, and a Play product id and
token as given. It applies to both run modes, since `--run-cli` answers service requests too, and
the service logs a warning at every start while it is on. Off by default, and only `on`/`off`
parse. The service refuses to start with it on beside an `[apple]` or `[google]` section.

It answers subscriptions too, each as renewing, charged from a minute before the check for the
length of its plan:

- an App Store subscription: the plan its product names;
- a Play subscription: the base plan a test token `test.<basePlanId>.<uuid>` names, when it is a
  plan of the product (the apps' test products sign their tokens so), or else the product's
  monthly base plan, so a licence tester's purchase is credited too;
- an App Store tier raise: a charge for the raised plan, which its renewals then charge;
- a renewal: a new charge for the plan it last charged. It keeps that plan in memory only, so after
  a restart it answers a renewal of a subscription it credited before as ended, and the unissued
  months of a plan still covered are refunded at its next month.

It acknowledges nothing, so Play refunds a licence tester's subscription it credited once Play's
acknowledgement window lapses.

With it on, anyone who can reach the service address can mint badges by sending a made-up receipt.
The service starts only with an issuer key released apps trust, so they accept every badge it
mints this way, and a subscription credited so renews at every check for as long as the process
runs. Never turn it on where anyone but you can reach the service.

```ini
[dev]
accept_unverified_store_receipts = on
check_store_during_coverage = on
```

With `check_store_during_coverage` on as well, the service asks its store, the mock or a real one
with `credit_test_purchases` on (below), at every `issueBadge` for a subscription, rather than at
most once an hour and only when a month is due. A charge it reports during coverage is banked: a
charge row, the plan's months added to what is left and a later paid-through date, and those
months are issued once coverage reaches them; a deployment without the setting only records it.
So a renewal shows at once, though its credential waits for the paid month to end on the service's
clock, and after a restart the mock answers the subscription ended at once, so the months banked
are refunded. The mock charges anew at every
check, so every such request banks the plan's months again; space requests over a minute apart, as
the mock dates a charge a minute back and the app fails a ledger row dated before the one it
follows. The service logs each banked charge as
`store subscription charge banked: <n> months`, which tells a banked month
from a repeated `badge issued`. Off by default, and the service refuses to start with it on unless
`accept_unverified_store_receipts` or `credit_test_purchases` is on: a real store has not started
its next charge while coverage runs, and asking it then would spend the quota every subscriber
shares.

### Crediting test purchases, for testing against a real store

```ini
[dev]
credit_test_purchases = on
```

A purchase the store itself calls a test, an App Store Sandbox purchase (which TestFlight builds
buy in) or a Play licence tester's, is refused `test_purchase` by default, which is terminal, so it
can never be credited. With this on, the service credits it as a real one, one-time purchases and
subscriptions alike. It applies beside a real `[apple]` or `[google]` section, which still verifies
every receipt, and changes nothing beside `accept_unverified_store_receipts`, whose mock reports
no test purchase. The service logs a warning at every start while it is on, and at every test
purchase and test subscription it takes as paid. Off by default, and only `on`/`off` parse.

Beside `[apple]` the service starts with it on only when `[apple]` sets the server API key with
`api_host = https://api.storekit-sandbox.itunes.apple.com`: a Sandbox receipt verifies offline like
any other, so beside the production host, or without the key, Sandbox purchases would be credited as
real ones. Beside `[google]` it is allowed, as Play calls a purchase a test only for the licence
testers the Play Console lists.

```ini
[dev]
credit_test_purchases = on
check_store_during_coverage = on
```

With both on, the real store is asked at every `issueBadge` for a subscription, as above, to watch
its renewals by hand: a Sandbox subscription renews every few minutes.

The service starts only with an issuer key released apps trust, so they accept every badge it
credits this way. Nothing tells a production `[google]` from a test one, so never turn this on
beside the production Play app's `[google]`, or every licence tester the Play Console lists gets a
badge without paying.

## Deployment

`scripts/store-service/` holds the production deployment: the image (`Dockerfile`, built from
the repository root), `compose.yml`, the ini template `store_service.ini.example` and the egress
rule `ops/egress.sh`; `dev/` holds the scripts of the dev setup and the webapp's cases. The service
runs with `+client_postgres` and host networking; one Caddy serves both sites from the folders the
service exports at boot and proxies the API to it.

`compose.yml` mounts the folder `scripts/store-service/config/` read-only at
`/etc/simplex-store-service`. It holds `store_service.ini`, and the App Store root and the Play
service account key when `[apple]` and `[google]` are configured. The folder must exist before
`docker compose up`: compose refuses to start rather than create it empty.

### Upgrading from the badge service

The rename to StoreService changed what the operator runs:

| Before | After |
|---|---|
| `scripts/badge-service/` | `scripts/store-service/` |
| compose service `badge-service`, image `simplex-badge-service` | compose service `store-service`, image `simplex-store-service` |
| `badge_service.ini` | `store_service.ini` |
| the folder `config`, mounted read-only at `/etc/simplex-badge-service` (before it, `badge_service.ini` alone, mounted at `/etc/simplex-badge-service/badge_service.ini`) | the folder `config`, mounted read-only at `/etc/simplex-store-service` |
| `[apple] root_certificates` and `[google] service_account_file` under `/etc/simplex-badge-service/` | the same files under `/etc/simplex-store-service/` |
| the binary `simplex-badge-service` | `simplex-store-service` |
| `port = 8080` | `badges_port = 8080`, the default, which can be omitted |
| `static_dir = /srv/web` | `badges_static_dir = /srv/badges`, the image's build and the default, which can be omitted in Docker |
| `webapp_export_dir = /srv/webapp`, host folder `web` | `badges_export_dir = /srv/badges-export`, host folder `badges` |
| `names_hosts`, the hosts that got the names site by `Host` | `names_port = 8081`, the default: the names site on its own port (see [Two sites](#two-sites)) |

The service refuses to start with a key from the left column, naming the key that replaced it.
Move `scripts/badge-service/config/` to `scripts/store-service/config/` (a deployment from before
the config folder creates it and moves `badge_service.ini` into it), rename `badge_service.ini` in it
to `store_service.ini`, and change its keys and paths as the table says, the
`/etc/simplex-badge-service/` files under `[apple]` and `[google]` among them. Point Caddy's badges root at
`scripts/store-service/badges`, which the service fills at boot. Then start the new service with

```
docker compose up -d --build --remove-orphans
```

from `scripts/store-service/`. Without `--remove-orphans` the old `badge-service` container keeps
running beside the new one, on the same port and database. The database `badge_service`,
`BADGE_DB_CONN`, the `sx_badge_service_` tables and the chat database prefix
`simplex_badge_service` are unchanged.

A badge-service database upgrades in place: the store's own migrations run after every badge
migration, `20261012_badge_subscriptions` included. A store database created before the names
migration became `20261013_names` (it was `20261010_names`) does not start, as the service refuses
migrations applied out of the order it lists. Recreate such a database.

### Two sites

The image carries the badges build at `/srv/badges` and the names build at `/srv/names`, the
defaults of `badges_static_dir` and `names_static_dir`; a run outside the image sets both. The names
site (simplex.domains) comes from the `apps/simplex-domains` submodule, the simplex.domains
repository, and is configured beside the badges site in `[listener]`. Fetch the submodule before
building the image:

```
git submodule update --init apps/simplex-domains
```

A new names site ships by moving the submodule to its commit and committing that here:

```
git -C apps/simplex-domains fetch origin
git -C apps/simplex-domains checkout <commit>
git add apps/simplex-domains
```

The names site is on when, and only when, the ini has `[names]`. Its `[listener]` keys:

- `names_static_dir`: the names build, by default `/srv/names`, the image's;
- `names_export_dir`: where the service writes the names site at boot, with the Stripe
  publishable key in its shell, as `badges_export_dir` is for the badges site. `compose.yml`
  bind-mounts `./names` there (`/srv/names-export`), and `./badges` at `/srv/badges-export`;
- `names_port`: the port the names site is served on when `serve_webapp = on`, by default 8081,
  beside `badges_port` (by default 8080), which serves the badges site. The two must differ.

The service refuses to start with any of these keys but no `[names]`, and with `names_port` and
`serve_webapp = off`. It also refuses to start when a build it serves or exports is not a
directory, naming the setting, so a local run that keeps a default path stops there.

With `serve_webapp = on` (all in one), the service listens on both ports, and each serves only its
own site: every path outside `/api/` and `/webhooks/` is a file of that site, `/` and `/index.html`
its shell. Both ports answer `/api/` and `/webhooks/`, whatever the `Host`. A reverse proxy sends
each domain to its own port, as the all-in-one example in [Caddy](#caddy) does. With
`serve_webapp = off`, the service listens on `badges_port` alone, for the API and the webhooks.

### Development with a local chain

`compose.dev.yml` adds two containers, so a dev deployment needs nothing on the host but Docker:

- `chain`: an anvil chain on `127.0.0.1:8545` (chain id 31337) with the names contracts, deployed
  on its first start. Its state is kept in `./data/chain`, so restarts keep the contracts, balances
  and registered names. Its keys are anvil's public test keys, so never use it beside a real chain.
- `postgres`: PostgreSQL 17 on `127.0.0.1:5433`, user `simplex`, database `store_service`, without a
  password, kept in `./data/postgres`. The service uses it in place of `BADGE_DB_CONN`.

From a fresh clone:

```
git clone https://github.com/simplex-chat/simplex-chat && cd simplex-chat
git checkout sh/store-service && git submodule update --init apps/simplex-domains
cd scripts/store-service
echo "COMPOSE_FILE=compose.yml:compose.dev.yml" > .env
docker compose up -d --build
```

The names site is then on `http://127.0.0.1:8081` and the badges site on `http://127.0.0.1:8080`.

On its first start, the chain writes `config/store_service.ini` for its deployment, unless the file
exists; it never rewrites it, so edits there are kept. The file serves both sites all in one, with
`[names]` on the `.simplex` TLD, `[chain]` at the local chain with 1 confirmation, and `[issuer]`
signing with a public test key at index 999, which no client trusts, through `dev_keys`. That key is
in `dev/chain.sh`, as public as anvil's: never use it elsewhere. The service starts once the chain
and the database are healthy. `USER_UID` and `USER_GID` in `.env` (by default 1000) own the file,
mode 600, and its folder, mode 700.

Names are paid for with prepaid codes, so no payment provider is needed. To issue one, stop the
service and run its console against the dev database:

```
docker compose stop store-service
docker compose run --rm store-service -d postgresql://simplex@127.0.0.1:5433/store_service --run-cli --service-config /etc/simplex-store-service/store_service.ini
```

There, `//issue name 6` prints a code for one name of 6 letters or more (see
[From the command line](#from-the-command-line)). Leave with `/quit`, then
`docker compose start store-service`.

The service key's account starts with 10,000 ETH and a sponsor allowance. To top it up, or set any
account's balance:

```
docker compose exec chain chain.sh topup [address] [eth]
```

Registrations wait 10 seconds after their commit (`MIN_COMMITMENT_AGE` in `compose.dev.yml`).

Card payments in Stripe's test mode: uncomment `[stripe]` in `config/store_service.ini` (as the
`USER_UID` user, or with `sudo`), put in the test keys and the webhook secret, which
`stripe listen --forward-to 127.0.0.1:8080/webhooks/stripe` prints, and run
`docker compose restart store-service`. With `dev_keys`, the service refuses live keys, and a
BTCPay that is not on loopback.

To start over, delete the containers and every dev state together, since orders in the database
point at names on the chain. `./data` belongs to root and to Postgres's user, so:

```
docker compose down
sudo rm -rf data config
```

### Caddy

The Caddyfile is not in this repository. Its two host blocks for the split deployment
(`serve_webapp = off`), with the exported folders at `/srv/store-service/badges` and
`/srv/store-service/names` and the listener at `127.0.0.1:8080`:

```
badges.simplex.chat {
	encode zstd gzip
	handle /api/* {
		reverse_proxy 127.0.0.1:8080
	}
	handle /webhooks/* {
		reverse_proxy 127.0.0.1:8080
	}
	handle {
		root * /srv/store-service/badges
		file_server
	}
	@shell path / /index.html /sw.js
	header @shell Cache-Control "no-cache"
	header /assets/* {
		Cache-Control "public, max-age=31536000, immutable"
		match status 2xx
	}
	header Content-Security-Policy "default-src 'self'; script-src 'self' https://js.stripe.com https://*.js.stripe.com; frame-src https://js.stripe.com https://*.js.stripe.com https://hooks.stripe.com; connect-src 'self' https://api.stripe.com; img-src 'self' https://*.stripe.com; frame-ancestors 'self' https://simplex.chat https://*.simplex.chat"
}

simplex.domains {
	encode zstd gzip
	handle /api/* {
		reverse_proxy 127.0.0.1:8080
	}
	handle {
		root * /srv/store-service/names
		file_server
	}
	@shell path / /index.html /sw.js
	header @shell Cache-Control "no-cache"
	header /assets/* {
		Cache-Control "public, max-age=31536000, immutable"
		match status 2xx
	}
	header /manifest.webmanifest Content-Type "application/manifest+json"
	@media path /fonts/* /img/*
	header @media {
		Cache-Control "public, max-age=86400"
		match status 2xx
	}
	header {
		Content-Security-Policy "default-src 'self'; script-src 'self' 'sha256-i5ar+ITNdQiucbmin4/AN6DAY5LbAWeq3at+RiFoUoY=' 'sha256-lxURZtAGLIPkeIfIfFjl4UIJp4ElJWSbFqE1ktKB8hk=' 'sha256-djvAKNENifE9sLU9+NrLdzdGftMgCwpvCvH/MAbkElY=' https://js.stripe.com https://*.js.stripe.com; frame-src https://js.stripe.com https://*.js.stripe.com https://hooks.stripe.com; connect-src 'self' https://api.stripe.com; img-src 'self' data: https://*.stripe.com; style-src 'self'; frame-ancestors 'none'; base-uri 'none'; form-action 'none'"
		Strict-Transport-Security "max-age=63072000; includeSubDomains"
		X-Content-Type-Options "nosniff"
		Referrer-Policy "strict-origin-when-cross-origin"
		Cross-Origin-Opener-Policy "same-origin-allow-popups"
	}
}
```

- Both sites call `/api/*` on their own origin, so there is no CORS.
- `/webhooks/*` is proxied on badges.simplex.chat only. BTCPay and Stripe send every event there,
  names payments included.
- The shell and `/sw.js` are `no-cache`, since they name the build's hashed files, and Caddy's
  `file_server` sends no `Cache-Control` of its own. `/assets/*` file names carry a content hash.
  The long lifetimes apply to found files only, so a missing asset is not cached for a year.
- `file_server` sends no type for `.webmanifest`, which the names site's manifest needs.
- The badges policy is `web/README.md`'s. The names policy is the names webapp's (its README,
  "Hosting"), with `https://*.stripe.com` added to `img-src` beside `data:`, which the link
  profiles' pictures need. Its two hashes are those of the inline theme and font-preload scripts
  in the names `index.html`, and change with them. It allows no inline style.
- Stripe.js runs on simplex.domains itself, in the document that keeps the owner codes and prepaid
  codes in `localStorage`. The owner accepted this on 2026-10-20, rather than a Checkout redirect
  or a separate card origin. Scripts and frames are allowed only from this origin and Stripe's
  hosts.

With `serve_webapp = on`, the service serves both sites itself, so each host block proxies
everything to its site's port, `badges_port` and `names_port`:

```
badges.simplex.chat {
	encode zstd gzip
	reverse_proxy 127.0.0.1:8080
	# the Content-Security-Policy header of the badges.simplex.chat block above
}

simplex.domains {
	encode zstd gzip
	reverse_proxy 127.0.0.1:8081
	# the last header { } block, the policy with it, of the simplex.domains block above
}
```

The service sets each file's type and `Cache-Control` itself: `no-cache`, and a year for
`/assets/*`, so the names site's fonts and images are revalidated rather than kept for a day.

### Egress rule

The link previews check a link's hosts before the chat core fetches it, but the core resolves the
host again when it connects, so a name that changes its answer in between (DNS rebinding) can
reach a private address. The guard is a firewall rule: the service's new outbound connections to
unspecified, private, CGNAT, loopback, link-local, benchmarking, multicast and reserved IPv4
ranges, and to unique-local, link-local, site-local and multicast IPv6 ranges (with IPv4-mapped,
IPv4-compatible and NAT64 forms of the IPv4 ones), are rejected, and so are those to any of the
host's own addresses, public ones included, since a rebinding name can answer with them too and
reach whatever listens on the host. The exceptions are the
endpoints the service is configured to use, on their ports: the RPC node (`[chain] rpc_url`),
BTCPay (`[btcpay] host`), the database and the DNS resolvers in `/etc/resolv.conf`. Replies on
connections already open, such as Caddy's requests, pass, and so does IPv6 neighbour discovery on
the link.

`ops/egress.sh` writes this as the chain `STORE_SERVICE_EGRESS` in `iptables` and `ip6tables`, and
rebuilds it on every run. With compose's host networking it matches the service's user in
`OUTPUT` (`-m owner --uid-owner`). That rule covers every process of that user on the host, so
the service must run as a dedicated user. `--uid` is required, and the script refuses a uid whose
host account has a login shell (anything but `nologin` or `false`). Compose's `USER_UID` default
of 1000 is often the operator's own login, so create a user for the service and run compose with
its ids, set as `USER_UID` and `USER_GID` in `scripts/store-service/.env`; they are build
arguments too, so rebuild the image:

```
sudo useradd --system --no-create-home --shell /usr/sbin/nologin simplex-store
id simplex-store
mkdir -p badges names config
sudo chown -R simplex-store:simplex-store badges names
sudo chown simplex-store config/store_service.ini && sudo chmod 600 config/store_service.ini
docker compose up -d --build --remove-orphans
```

On a Docker bridge network instead, `--bridge <interface>` matches traffic from that network's
bridge interface in `DOCKER-USER` (traffic forwarded to other hosts) and `INPUT` (the host's own
addresses), and needs no `--uid`. The script refuses an interface that is not a bridge or that holds a default route (a bridge
carrying the host's uplink, such as `br0` or `vmbr0`, whose inbound connections it would refuse),
and stops when `DOCKER-USER` is missing (Docker's nftables backend, `"iptables": false` or
`"ip6tables": false` in its daemon config, or Docker not started yet): the forwarded traffic would
then pass unfiltered. Docker before 27 creates the IPv6 `DOCKER-USER` only with both
`"ip6tables": true` and `"experimental": true`. The interface is `docker0` for the default network; for another, it is the network's
`com.docker.network.bridge.name` option when set, else `br-` and the first 12 characters of its id:

```
docker network inspect -f '{{.Id}} {{index .Options "com.docker.network.bridge.name"}}' <network>
```

Install it as root from `scripts/store-service/`, here for a service uid of 998. `--db` defaults to
the environment's `BADGE_DB_CONN`, else to compose's default
(`postgresql://simplex@127.0.0.1:5432/badge_service`); `sudo` drops the environment, so pass it
when compose runs with another value:

```
sudo ops/egress.sh --dry-run --uid 998
sudo ops/egress.sh --uid 998
sudo ops/egress.sh --check --uid 998
```

- `--dry-run` prints the `iptables-restore` input an install would commit, read against the
  current rules, and changes nothing; read the allowed endpoints in it.
- `--check` exits 1 and names what differs unless every rule is in place, the chain holds nothing
  else, and each jump is the first rule of its chain.
- Names are resolved when the rule is installed: run it again after changing the ini, the
  database string or the resolvers, and after a private endpoint's address changes. A name the
  host cannot resolve (a container name) stops the install; pass its address with
  `--allow ADDRESS:PORT`, as for any other endpoint the service needs, such as a SOCKS proxy.
- Every address of this host is refused unless allowed, public ones included. An SMP or XFTP
  server on the same host, which the service reaches by its public name, needs
  `--allow <name>:<port>` (5223 for SMP, 443 for XFTP by default), as does anything else on this
  host the service connects to.
- The DNS resolvers allowed are those in `--resolv-conf` (default `/etc/resolv.conf`). With host
  networking that is what the service queries, systemd-resolved's stub included. On a bridge
  network of a host with systemd-resolved, Docker sends the containers' queries to the upstream
  servers in `/run/systemd/resolve/resolv.conf`, so pass that file.
- On a bridge network every container on it is matched, and the service cannot open connections
  to another container there unless it is listed with `--allow`. Give the service a network of its
  own.
- The rules do not survive a reboot. Run the script at boot after Docker and DNS are up, from a
  unit ordered after `docker.service` and `network-online.target`: the names in the ini must
  resolve, and bridge mode needs Docker's `DOCKER-USER`. The service is unguarded from its start
  until then. With host networking, saving the rules with `netfilter-persistent` restores them
  before Docker starts.

Check it by hand from inside the container, once the service runs. Each check below fails when the
rule is missing:

```
docker compose exec store-service bash -c ': </dev/tcp/10.255.255.1/80'
```

With host networking:

```
docker compose exec store-service bash -c ': </dev/tcp/127.0.0.1/8080'
docker compose exec store-service bash -c ': </dev/tcp/127.0.0.1/5432 && echo allowed'
```

On a bridge network, with the network's gateway (the host's address on it) and a port the host
listens on for every address, such as Caddy's 443:

```
docker network inspect -f '{{(index .IPAM.Config 0).Gateway}}' <network>
docker compose exec store-service bash -c ': </dev/tcp/<gateway>/443'
```

- A refused connection answers `Connection refused` at once, sent by the rule. Without the rule
  the connection to 10.255.255.1 waits for a timeout instead, and the one to 127.0.0.1:8080 (the
  service's own listener) or to the gateway connects.
- `allowed` is printed for the database. With the RPC node on this host, the same command with its
  port (`8545` for a local reth) prints it too.

`sudo ops/egress.sh --remove --uid 998` (or `--bridge <interface>`) deletes the jumps an install
adds for that user or network, then the chain, in `iptables` and `ip6tables`. Running it again
changes nothing, and `--check` then reports the chain absent.

### Terms and community names

The controller's name policy (`SimplexNamePolicy`, read from `namePolicy()` once at startup and
logged, or on first use when that read fails or finds none) holds the links of two sets of terms:
`nameTermsLink()`, the name terms, and `communityNameTermsLink()`, the community name terms. It lists
the community names: its bona fide list, which the contracts' owner edits on chain and the guardian
can add to. A listed name is sold under the community name terms, which include the name terms that
apply to it, and any other name under the name terms. A name is registered only after its owner
accepts the current terms of its kind on chain, once.

**Upgrading:** `[names] terms_version` and `terms_url` are no longer read: the service warns at
startup that nothing reads them, and ignores them.

A later `setNamePolicy` takes effect for the service after a restart, so restart it
at once: until then the service reads the reservations, the list, the terms and the nonces from the
old policy, so an order of a name the new policy reserves ends taken, and one of a name it lists
does not register: the page signs again until the order lapses, and only the first relay spends gas.

A community name is registered for 2 paid years, plus the free days before `[names] term_start`,
whatever years `[names] years` or its prepaid code gives, since the controller registers a community
name for at most 1095 days.

Each link names one file at one commit of simplex-chat/simplex-chat on git.simplex.chat, with the
sha256 of the file's bytes:

```
https://git.simplex.chat/simplex-chat/simplex-chat/raw/commit/<40 lower-case hex>/<path>#sha256=<64 lower-case hex>
```

The path is one or more segments of letters, digits, `_`, `-` and `.`, separated by `/`, with no empty,
`.` or `..` segment, and ends in `.md`. The terms' hash, which orders sign and the policy records with
each acceptance, is the keccak256 of the link. With `[chain] chain_id = 31337` and a node of that chain
id, the service also takes a link of a file on this machine, `http://127.0.0.1:<port>/<path>#sha256=<64
lower-case hex>`, as the dev scripts set.

The service reads both links at one block for each request that needs them, so a change on chain
applies at once. It fetches each text once, without the link's fragment, following no redirect, within
10 seconds: a `200` answer with `Content-Type` `text/plain` or `text/markdown`, of charset `utf-8` or
none, of at most 256 KB, valid UTF-8, whose sha256 is the link's. It keeps each text by its hash, and
fetches one at a time. After a failed fetch it fetches no text for 30 seconds, or for longer when the
answer's `Retry-After` asks, and logs why. `GET /api/terms/<hash>` answers `{"link", "text"}` for the
hash of either current link, `404` for any other hash or a link the service does not take, and
meanwhile `503 terms_unavailable` with `Retry-After` for a text not fetched yet.

The terms of a name's kind are unavailable while their link is not set, is not a link the service
takes, or has a text the service cannot fetch now. An available name's search answer then carries
`"terms": null`, and an order, a code order, a move or a spend of such a name answers
`503 terms_unavailable` before any payment, with `Retry-After` while the fetch waits. The service
logs both links at startup, and warns for each that leaves names unavailable. To change the terms,
the contracts' owner calls `setNameTerms(link)` or `setCommunityNameTerms(link)`; an order signed
under the old terms is asked to agree again before its reveal.

`[community]` gives what each community name is for, one line per name: the label, as it is searched,
then `=` and the text:

```
[community]
bakers = bakers and bakeries in Portugal
```

Search answers carry a listed name's text, and the registration view the text of an order that signed the community terms, which the page shows with the
name as a community name. A name the policy lists without a line here shows as a community name
without a text, and the service warns at startup, when it reads the policy, for each line naming a
name the policy does not list, which is sold as a plain name. Each label must be a valid name in lower case and listed once; each text is
1 to 200 characters with no control, format or line separator characters, and should not end with a
full stop, since the page adds one. The service refuses to start, naming the setting, on any other
entry, on `[community]` without `[names]`, on a key written twice in a section (only the first would
be read) and on a section header written twice in the file (only the last would be read). It logs
how many community names it loaded.

The `[community]` texts are read only at startup, so an edit applies after a restart; the policy's
list and its terms apply as soon as they change on chain. After a restart, search each listed name on
the names site and check that it shows as a community name.

Every order signs the terms the buyer agreed to: the 64 lower-case hex digits of the hash of the
terms of the name's kind, without `0x`, after `community/` for a listed name. The service stores them
with the order, so an open order keeps the terms it signed after a change, and every move signs the
current terms for the name it moves to, and stores them. An order or a move signed with any other
value answers `409 terms_changed` with the current terms of the name's kind, `{"hash", "link"}`, and
the name's community entry, and the page asks again; an order signed with a terms version, as pages
before signed, meets it once. An exact resend is still answered after the terms changed.

**Upgrading:** a page that sends no `terms` gets `bad_request` on every order, spend and move, so the
page and the service are deployed together.

On chain the order is the same for every name: the service commits, the page signs the reveal, the
service registers. With the reveal the page signs the acceptance of the terms of the name's kind for
the commitment, `AcceptNameTerms` or `AcceptCommunityNameTerms`, at the policy's nonce and the
reveal's deadline, and the service relays it with `commitUnderNameTermsWithSig` or
`commitUnderCommunityNameTermsWithSig` as soon as the reveal is stored, then registers. A relay or a
register refused for the acceptance (new terms, a used nonce), an acceptance of the other kind of
terms, or one signed for the name policy before a `setNamePolicy`, clears the reveal, and the page
signs again. A relay that reverts while the policy holds that acceptance for the commitment, as when
another sent it first, is followed by the register.

### The gas key

`[chain] service_key` is the registrar key: an Ethereum account (EOA) that sends every names
transaction, `commit` to the controller, each name's terms acceptance (`commitUnderNameTermsWithSig` or `commitUnderCommunityNameTermsWithSig`) to the name policy, `registerWithCredit` to the sponsor, and the owners' record
edits (`setTextWithSig`, `clearRecordsWithSig`) to the resolver. It pays only their gas, in ETH. The
names' rent is spent from its allowance on the sponsor, in USD, which the guardian sets. It holds no
names.

The commands run from `scripts/store-service/` on the service host, with Foundry's `cast`
([getfoundry.sh](https://getfoundry.sh), checked with 1.5.1) and `jq`. They read the ini through this
shell function, which takes a `[chain]` key's value without surrounding spaces, tabs or a CR:

```
ini() { sudo sed -n "/^\[chain\]/,/^\[/s/^$1[[:space:]]*=[[:space:]]*\([^[:space:]]*\).*/\1/p" "${2:-config/store_service.ini}"; }
```

Keys and the RPC URL are read through `ini`, so they stay out of the shell history. A key is in
cast's arguments, and the RPC URL in curl's, visible to `ps` while it runs.

The ini is never edited in place. `sudoedit` refuses a file in a directory the invoking user can
write (sudo(8), sudoers(5) `sudoedit_checkdir`), as `scripts/store-service/config/` of the checkout is.
`sudo <editor>` would leave swap or backup files holding the key beside it, which git does not
ignore. So each change is made in a copy in a root-only directory and copied over the ini, which
keeps its owner and mode. `next/store_service.ini` and
the other copies below are ignored by git, as every `store_service.ini` is, and are outside the
image.

#### Create the key

Fill `[names]` and `[chain]` in a copy, with a `service_key = 0x...` line under `[chain]`. Then
generate the key into that line, so it is never printed, and put the copy in place:

```
sudo install -d -m 700 next && sudo cp -p config/store_service.ini next/store_service.ini
sudoedit next/store_service.ini
cast wallet new --json | jq -r '"s/^service_key[[:space:]]*=.*/service_key = " + .[0].private_key + "/"' | sudo sed -i -f - next/store_service.ini
cast wallet address "$(ini service_key next/store_service.ini)"
sudo cp next/store_service.ini config/store_service.ini && sudo rm -r next
```

The address check prints the key's address, as in `0xB2590B3a4fa5bEdda803F5f1F605803438C3F71E`, and
fails when the line was not replaced. The key is `0x` and 64 hex digits.

The ini holds the only copy of the key. It is gitignored, every `*.ini` is kept out of the image,
and compose mounts its folder read-only. Keep it owned by the service user with mode 600 (see
[Egress rule](#egress-rule)). The service never logs the key; keep it out of logs and tickets too.
If a key was ever shown on a terminal, clear the screen and its scrollback (`clear && printf
'\e[3J'`, and `tmux clear-history` in tmux), and never show one in a recorded session.

#### Set up the shell

```
export ETH_RPC_URL="$(ini rpc_url)"
CONTROLLER=$(ini controller)
SPONSOR=$(ini sponsor)
REGISTRAR=$(cast wallet address "$(ini service_key)")
```

cast reads the node from `ETH_RPC_URL`, so no command below names it. The address check reads the
ini on disk, not the running service, which logs no address. To confirm the service runs with the
key, check that `cast nonce $REGISTRAR --block pending` rises after its next transaction.

#### Grant the allowance

Only the guardian, the controller's beneficiary, can set the allowance:

```
cast call $CONTROLLER 'beneficiary()(address)'
```

The guardian sends this call to `$SPONSOR`, with no value, from whatever wallet holds its key. Its
calldata, here for $1,000:

```
cast calldata 'setRegistrarAllowance(address,uint256)' $REGISTRAR $(cast parse-units 1000)
```

```
0x7f8ac8b8000000000000000000000000b2590b3a4fa5bedda803f5f1f605803438c3f71e00000000000000000000000000000000000000000000003635c9adc5dea00000
```

- From a key, with cast (`--interactive` prompts for the key; `--ledger` signs on a Ledger instead):

  ```
  cast send $SPONSOR 'setRegistrarAllowance(address,uint256)' $REGISTRAR $(cast parse-units 1000) --interactive
  ```

- From a multisig such as Safe: submit that calldata to the sponsor address as a transaction with
  value 0 in the Safe app.

Then anyone can check the result:

```
cast call $SPONSOR 'registrarAllowance(address)(uint256)' $REGISTRAR
```

```
1000000000000000000000 [1e21]
```

- The amount is in attoUSD: dollars × 10^18, or cents × 10^16. `cast parse-units 1000` gives $1,000.
- It replaces the allowance and does not add to it. To top it up, read what is left and send the
  new total.
- Zero is the kill switch: `setRegistrarAllowance($REGISTRAR, 0)` stops every register from that
  key in one transaction. A sender that is not the guardian is refused with `NotBeneficiary`.
- Each register spends the name's list price (`rentPriceUSD`, base plus premium) for its whole
  duration, the free days before `[names] term_start` included. For example, 2 years of a name of 8
  letters or more:

  ```
  cast call $CONTROLLER 'rentPriceUSD(string,uint256)((uint256,uint256))' examplename 63072000
  ```

  ```
  (100000000000000000000 [1e20], 0)
  ```

#### Fund the key

Send ETH on the chain of `[chain] chain_id` from any wallet, and top it up the same way. With cast:

```
cast send $REGISTRAR --value 1ether --interactive
```

The gas below was measured on a local anvil deployment of the contracts (the names webapp's
`dev/deploy.ts`), with a 63-letter label and links of 4096 bytes, the most the service accepts,
before the terms became links accepted for every name. The acceptance's figures are those of the
community names' acceptance then, whose work its two functions repeat; a plain name's register now also
reads, clears and logs its acceptance, as a community name's did:

| Transaction | Gas limit | Gas used | Limit × 100 gwei |
|---|---|---|---|
| `commit` | 58,983 | 53,521 | 0.0059 ETH |
| `commitUnderNameTermsWithSig` or `commitUnderCommunityNameTermsWithSig`, the acceptance | 95,795 | 94,791 | 0.0096 ETH |
| `registerWithCredit`, no links | 568,116 | 504,408 | 0.0568 ETH |
| `registerWithCredit`, one link | 3,673,412 | 3,489,551 | 0.3673 ETH |
| `registerWithCredit`, two links | 6,791,417 | 6,467,815 | 0.6791 ETH |
| `setTextWithSig`, a new link | 3,026,449 | 3,026,449 | 0.3026 ETH |
| `setTextWithSig`, a link replacing one | 462,289 | 462,289 | 0.0462 ETH |
| `setTextWithSig`, a link removed | 698,644 | 558,916 | 0.0699 ETH |
| `clearRecordsWithSig` | 80,312 | 80,312 | 0.0080 ETH |

- The service sets each transaction's gas limit to its estimate and its max fee to at most
  `max_fee_per_gas` (default 100 gwei). The node accepts a transaction only while the balance covers
  its limit × max fee, so the last column is what each one needs held. What it is charged is gas
  used × the block's base fee plus tip, usually far less.
- An order is one commit, one acceptance relay and one register: 0.6946 ETH at the cap with two links, 0.0723 ETH with
  none. The expected orders × 0.6946 ETH, plus 0.3026 ETH per expected link edit, pays for all of
  them even at the cap. The figures scale with `max_fee_per_gas`.
- `gas_limit_cap` (default 8,000,000) must stay above 6,791,417: a transaction whose estimate is
  above it is not sent.

#### Check the balance and transactions in flight

```
cast balance $REGISTRAR --ether
cast nonce $REGISTRAR --block latest
cast nonce $REGISTRAR --block pending
```

The balance prints as `1.000000000000000000`. The `latest` nonce counts the key's mined
transactions, and the `pending` one adds those waiting in the node's pool; the difference is what is
in flight. The same reads through JSON-RPC answer in hex wei and hex counts
(`{"jsonrpc":"2.0","id":1,"result":"0xde0b6b3a7640000"}`, which `cast to-unit 0xde0b6b3a7640000 ether`
prints as `1`):

```
curl -s -X POST -H 'content-type: application/json' --data '{"jsonrpc":"2.0","id":1,"method":"eth_getBalance","params":["'"$REGISTRAR"'","latest"]}' "$ETH_RPC_URL"
curl -s -X POST -H 'content-type: application/json' --data '{"jsonrpc":"2.0","id":1,"method":"eth_getTransactionCount","params":["'"$REGISTRAR"'","latest"]}' "$ETH_RPC_URL"
curl -s -X POST -H 'content-type: application/json' --data '{"jsonrpc":"2.0","id":1,"method":"eth_getTransactionCount","params":["'"$REGISTRAR"'","pending"]}' "$ETH_RPC_URL"
```

A transaction the node refused is in neither count. The service's own list is the `pending` rows of
its transactions table, here with compose's default database and schema:

```
psql "postgresql://simplex@127.0.0.1:5432/badge_service" -c "SELECT registrar_nonce, kind, sent_at FROM simplex_v1_chat_schema.sx_badge_service_name_transactions WHERE status = 'pending' ORDER BY registrar_nonce"
```

#### When funds or the allowance run out

`docker compose logs store-service` shows:

- `[ERROR …] names sender: nonce <n>: not sent: …`, with the node's `insufficient funds` message,
  when the key cannot pay. The transaction is saved and sent again every few seconds, so funding the
  key is enough and needs no restart. The line is logged again only when the node's message changes.
- `[ERROR …] names worker: registration <id> failed: the registrar's allowance on the sponsor is
  used up; the guardian raises it with setRegistrarAllowance` when the sponsor refuses a register
  with `InsufficientAllowance`. The order is failed; a code order keeps its code for Try again.
- `[WARN …] names sender: the latest block needs <wei> wei per gas, above max_fee_per_gas <cap>;
  the transaction waits` when fees are above the cap. Nothing is signed until they fall.
- `[WARN …] names sender: nonce <n>: not replaced: a replacement would pay over max_fee_per_gas
  <cap>` when a transaction stuck in the pool cannot be bumped within the cap.

#### Rotate the key

The service resumes its saved transactions at start and gives new ones nonces after theirs. Those
are signed by the key that sent them, so started on another key it would give that key's
transactions nonces it has not reached, and they would never be mined. Rotate only with no `pending`
row, at a quiet time: every stop takes the store offline, and each order sends its register about a
minute after its commit.

1. Keep a copy of the old ini, create the new key in another, fund it, and have the guardian grant
   it an allowance, as above for `$NEW`:

   ```
   sudo install -d -m 700 prev next
   sudo cp -p config/store_service.ini prev/store_service.ini && sudo cp -p config/store_service.ini next/store_service.ini
   cast wallet new --json | jq -r '"s/^service_key[[:space:]]*=.*/service_key = " + .[0].private_key + "/"' | sudo sed -i -f - next/store_service.ini
   OLD=$(cast wallet address "$(ini service_key prev/store_service.ini)")
   NEW=$(cast wallet address "$(ini service_key next/store_service.ini)")
   ```

2. Stop the service and count the `pending` rows:

   ```
   docker compose stop store-service
   psql "postgresql://simplex@127.0.0.1:5432/badge_service" -tAc "SELECT count(*) FROM simplex_v1_chat_schema.sx_badge_service_name_transactions WHERE status = 'pending'"
   ```

   On `0`, go to step 3. Otherwise `docker compose start store-service` on the old key, let them
   settle (`confirmations` blocks after they are mined), wait for a pause in the orders, and repeat
   this step. For a drain that new orders cannot hold up, start the old key with
   `max_fee_per_gas = 1` under `[chain]` instead:

   ```
   sudo install -d -m 700 drain && sudo cp -p config/store_service.ini drain/store_service.ini
   sudoedit drain/store_service.ini
   sudo cp drain/store_service.ini config/store_service.ini
   docker compose start store-service
   ```

   With a cap of 1 wei the service signs no new transaction. Each new one waits and logs the fee
   warning, and an owner's link edit is answered as unavailable. The saved transactions are still
   followed and settle. They are not bumped above the cap, so one that stays pending for long needs
   the old ini back (`sudo cp prev/store_service.ini config/store_service.ini` and a restart) to be
   replaced. Step 3 puts the full cap back, since `next/` was copied before.
3. Put the new ini in place, start, and check that the ini on disk now holds the new key:

   ```
   sudo cp next/store_service.ini config/store_service.ini
   docker compose start store-service
   cast wallet address "$(ini service_key)"
   ```

   It prints `$NEW`. After the next order's commit, `cast nonce $NEW --block pending` is above 0,
   and `cast nonce $OLD --block pending` stays where it was.
4. The guardian zeroes the old key's allowance, sending this calldata as in
   [Grant the allowance](#grant-the-allowance), then anyone checks it reads 0:

   ```
   cast calldata 'setRegistrarAllowance(address,uint256)' $OLD 0
   cast call $SPONSOR 'registrarAllowance(address)(uint256)' $OLD
   ```

5. Send the old key's ETH back to a plain account (a transfer to a contract such as a Safe needs
   more than the 21,000 gas used here), then delete the copies. The first send leaves 0.01 ETH for
   the fees. The second sweeps the rest, as a legacy transaction that pays exactly its gas price,
   twice the node's current one, so the balance ends at 0:

   ```
   cast balance $OLD --ether
   cast send <account> --value <balance - 0.01>ether --private-key "$(ini service_key prev/store_service.ini)"
   P=$(( 2 * $(cast gas-price) ))
   cast send <account> --legacy --gas-limit 21000 --gas-price $P --value $(( $(cast balance $OLD) - 21000 * P )) --private-key "$(ini service_key prev/store_service.ini)"
   cast balance $OLD
   sudo rm -rf prev next drain
   ```

   The last balance prints `0`. A negative value, from fees above about 150 gwei, makes cast refuse
   the sweep; run it again when they fall.

## Issuing codes

Operators issue codes two ways: from the service's own command line in `--run-cli` mode, and from the
managed group in service mode. Both are commands to a running process, so no second process
touches the service's database.

### From the command line

The command is sent to the running service in `--run-cli` mode, not a way to start it:

```
//issue <badge_type> [months] [paid|unpaid|free]
//issue supporter 12
//issue name <length> [years <Y>] [paid|unpaid|free]
//issue name 7 years 5
```

`months` defaults to 1 and must be between 1 and 255; the status defaults to `free` and records
whether the code was sold (`paid`), is awaiting payment (`unpaid`), or was issued by an operator
(`free`). Redemption refuses an `unpaid` code with `payment_pending`: the web checkout writes the
code row when the invoice is created, and settlement is what marks it paid.

`//issue name` makes a prepaid name code (`SN<length>-…`): it pays for one name of at least
`<length>` letters, 6 to 63, for `years` years, 2 to 10 (default 2), or 2 for a community name. The status is as above: an
order refuses an `unpaid` name code with `payment_pending`.

The code is printed once and only its `SHA-256` is stored, so a code that is not copied when it is
shown cannot be recovered.

A code that leaked, or that was refunded, is withdrawn the same way, badge and name codes alike:

```
//revoke <code>
```

A revoked code answers redemption with `code_invalid`, as if it had never existed, so its holder
learns nothing from trying. A client that redeemed it before the revoke still gets its own badge
back when it asks again. Revoking it again answers "already revoked" and fixes its group
message if the first revoke didn't. A code with no uses left can't be revoked, because its badges
were already given out, and the command answers with an error. A multi-use code with uses left can
be revoked, which stops the uses that remain.

Core parses `//...` into `CustomChatCommand` and leaves it to the service's `preCmdHook`, which is
why issuing codes lives in the service rather than in core. `//refund`, the other command of this
console, is in [Refunding a name balance](#refunding-a-name-balance).

### From the group

With `[group]` in `store_service.ini`, the service manages one group and serves three commands in
it: `/issue <type> [months <M>] [uses <N>]` and `/bulk <type> [months <M>] count <B>` for moderators
and above, `/revoke <code>` for admins and owners. `months` is 1 to 255, `uses` 1 to 1000 and
`count` 1 to 100; a value outside these gets the usage reply. The name forms are
`/issue name <length> [years <Y>]` and `/bulk name <length> [years <Y>] count <B>`, with the
bounds of `//issue name`; a name code issued in the group is single-use and `free`. `/revoke`
takes badge codes only: withdraw a name code with `//revoke` in `--run-cli` mode. The group has no
`/refund`. A member's role
is checked as the service last saw it, so a command sent by a moderator just demoted or removed can still run if it
reaches the service first; revoke any code the service posts for them after the change. `uses`
above 1 makes a multi-use code, tracked by a group message showing its remaining uses and the time
of the last one; when every use is redeemed, the same message says so. Every reply carrying a code is read by every member,
since the group has no private lane, so a code issued there is only as private as its least trusted
member.
Those replies are also kept as plain text in the service's chat database, so a copy of the database
holds every code issued in the group. Keep the group's visible history off: with it on, each new
member receives recent messages, and the codes in them, when they join. A multi-use code's message
carries the code, and every redemption edits it or, after a day, posts it again; either way every
current member receives it, so a member who joined after the code was issued gets the code while it
still has uses left. A message replaced by a new post stays in the group with its old count. Keep
disappearing messages off in the group and set no message TTL for the service's chats: a code's
message that expires is treated as deleted and never posted again, so its counter stops.
Every member can see when each use of a multi-use code was redeemed: the message shows the time of
the last one, and its edit times show the rest.
`/revoke <code>` names the code in an ordinary group message, so every member holds it before the
service reads the command, and the code stays redeemable until the service acts on it — for the
whole of any downtime. Revoke a code that is not already public in the group, a refunded one above
all, with `//revoke` in `--run-cli` mode. A `/revoke <code>` with nothing after the code, from a
member below admin, is answered that the code was not revoked and is now visible to the group. A
group command the service received but had not run when it stopped, or received while it ran in
`--run-cli` mode, is dropped with no reply, so resend it, or use `//revoke`.

The first member to join through the link is promoted to owner, so the operator joins before sharing
it. Keep the service an owner too: below owner it cannot update the group's command menu, and below
author it cannot post codes or replies. A failed promotion is logged at once, and an owner who left
is logged at the next start or join. Then make the member you choose owner with the `/mr` command
that the log line names, in `--run-cli` mode; the service never promotes anyone once the first
promotion was attempted.

The join link logged when the group is created stays valid: anyone who has it can join later, as a
member, and read every code posted or edited from then on. That includes a removed member, who can
rejoin through it, so removing a member does not stop them seeing new codes. Keep the log that holds
it private. The link is also stored in the `group_link` column of `sx_badge_service_group`, where it
can be read again.

If an owner deletes the group, or removes the service from it, the service logs an error on start
and stops serving the group. To create a new group, stop the service, delete the row, and start it
again: with SQLite, run `DELETE FROM sx_badge_service_group;` on the `<prefix>_chat.db` file
(`~/.simplex/simplex_badge_service_chat.db` by default), opened with `sqlcipher` and the database key
if one is set; with PostgreSQL, run
`DELETE FROM <schema-prefix>_chat_schema.sx_badge_service_group;` (`simplex_v1_chat_schema` by default).
Multi-use codes issued in the old group stay redeemable, but their messages there are no longer
updated, so revoke with `//revoke` any that should not stay live.

The group is identified by the single `sx_badge_service_group` row. Rolling back past the
`20260918_badge_group_ops` migration drops that table, so a later re-upgrade creates a second group
and orphans the first one with its members and roles; multi-use codes come back single-use with
their claims re-derived, and outstanding trackers come back unanchored. Redeemed credentials are
preserved and no code becomes redeemable again, though while the old version runs, only the holder
whose credential ends last gets it back on a retry, and any other holder of a multi-use code gets
`code_used`; every holder of a revoked code gets `code_invalid`. Rolling back means re-creating and
re-sharing the group; delete the orphaned one with `/d #'<old local name>'` in `--run-cli` mode, as its
join link still works and its messages hold every code posted there.

The configured `display_name` and `description` apply only to the group the service creates. Editing
them later is logged as not applied and changes nothing.

## Refunding a name balance

A paid name order keeps its money as a balance when it ends `taken` (another buyer registered the
name first), when it moves to a cheaper name (the difference), when a payment reaches its invoice
after the invoice closed, and when its invoice was paid more crypto than it asked (the extra). The
buyer spends a balance on another name from Your names, or asks for it back: the page tells them to
write to chat@simplex.chat with the order's reference, which is its invoice id, or its registration
id for an order without an invoice. Nothing is refunded automatically, and the service never learns
where to send money. Support records each refund in `--run-cli` mode with the service stopped, like
the other `//` commands:

```
//refund <reference>
//refund <reference> <cents>
```

1. Run `//refund <reference>` with the reference the buyer sent. It records nothing and answers the
   balance, what paid for it and the command that refunds it:

   ```
   Order <id>: balance $100.00 (credited $100.00, refunded $0.00). Paid by: order <id>: $100.00 by btc on invoice <invoice id>. To record its refund: //refund <reference> 10000
   ```

   `Paid by` starts at this order and follows every balance the money came through back to the
   first order: each order with what its invoice paid and by which method (`card`, `btc` or
   `xmr`), the part paid from the balance of the next order listed, and money credited from its
   invoice later: marked `late` when it arrived after the invoice closed, and `overpaid` when it is
   the extra of an invoice that paid the order.
2. Run the command the answer ends with. The cents must equal the whole balance: a refund zeroes it,
   and part of a balance is never refunded. Other cents record nothing, and the answer gives the
   balance.
3. The answer, `Recorded a refund of $100.00 for order <id>. Refund it now, by: …`, repeats the
   chain. Pay the balance's amount in US dollars back now, by the methods it lists. The refund is
   recorded first so that the buyer cannot spend the money while it is paid
   back. The service logs each recorded refund at info with the order's registration id and the
   cents.

A balance that an open order holds is never refunded: that order is buying another name with it and
its invoice is waiting for the difference. The command names the order instead, with its invoice's
expiry unless money reached the invoice:

- `its invoice open until <time>`: if the buyer pays it, the balance is spent; if not, it is released
  once the invoice expires and the poller, in service mode, closes the order. Run the command again
  then.
- `its invoice expired at <time>; start the service so the poller closes that order, 10 minutes
  after the expiry at the earliest, then stop it and run //refund again`: the poller runs only in
  service mode. Its first pass from 10 minutes after the expiry closes the order and releases the
  hold; a card order closes only once Stripe accepts the cancel.
- `whose invoice holds money`: a payment reached the invoice, before or after its expiry; the hold
  stays until that payment settles, which spends the balance, or the provider closes the invoice,
  which releases it.

The command also answers when the reference matches no order, when the order has no balance, and
when the cents are not the balance. A refunded balance shows `amount` 0 and its `refunded` total in
Your names; a later late payment credits it again, and a second refund adds to `refunded`.

## Running the webapp's cases against the service

`scripts/store-service/dev/contract-run.sh` runs the names webapp's end-to-end cases that need no
payment against this service in Chromium, as they run against the webapp's mock. It:

1. starts anvil on 8545 and deploys the contracts with the webapp's `dev/deploy.ts`, with a commitment
   age of 5 seconds;
2. builds the names site (`vite build`) into the run's folder;
3. writes an ini for that deployment with `[names]` and `[chain]` and no `[btcpay]` or `[stripe]`,
   with the terms version `2026-10-23` and the one community name `bakers`, which the webapp's
   community cases search for,
   serving the names site all in one on `SERVICE_PORT` and the badges site, from an empty folder, on
   `BADGES_PORT`, and signing with the test suite's
   public issuer secret (`issuerSecret` in `tests/Bots/StoreService/ConfigTests.hs`), trusted through
   `[issuer] dev_keys`. That secret is public: never use it outside this script;
4. starts the service once to make its database, stops it, and issues the prepaid codes the cases
   need with `//issue name` in `--run-cli`;
5. starts the service on 8180 (the names site) and 8181 (the badges site) and a code server on 8199,
   and runs the cases with `BASE_URL` at the names site and `MOCK_URL` at the code server.

The code server answers the cases' `POST /control/code` with the issued codes, the reset of
`/control/search` that `search.spec.ts` sends after each case as done, and any other `/control/` with
`501`, which fails the case that sends it. It passes `/api/` to the service with the case's
`X-Forwarded-For`.

It needs bash 5.1 or later (for `wait -n -p`), `cabal` (for the default `STORE_SERVICE_BIN`),
Foundry's anvil, Node.js 22.18 or later (which runs the webapp's `dev/deploy.ts` as TypeScript, as its
`dev/start.sh` does), `jq`, `curl` and `python3`; the webapp with its packages and
Playwright's Chromium (`npm install`, `npx playwright install chromium`); and the compiled
`ens-contracts` submodule (simplex-network/ens-contracts#45) of a simplex-namespace-contract checkout
(brenzi/simplex-namespace-contract#8), whose `bona-fide-terms.simplex.md` the deploy sets as the community terms,
as the webapp's `dev/start.sh` needs. The cases read the chain's addresses from the webapp's
`dev/local-chain-0.json`, so the script stops when its deployment differs from it; run the webapp's
`SLOTS=1 MIN_COMMITMENT_AGE=5 dev/start.sh` once to write it. Ports 8545, 8180, 8181 and 8199 must be free:
the script stops before starting anything when one of them answers, and stops at once when anvil,
the service or the code server exits before it answers.

```
cabal build exe:simplex-store-service
ENS_CONTRACTS_DIR=<ens-contracts> ANVIL=<anvil> scripts/store-service/dev/contract-run.sh
```

- `WEBAPP_DIR`: the names webapp (default: the `apps/simplex-domains` submodule's `webapp/`).
- `STORE_SERVICE_BIN`: the service binary (default: `cabal list-bin exe:simplex-store-service`).
- `SERVICE_PORT`, `BADGES_PORT`, `CODES_PORT`: the service's names and badges ports and the code
  server's (8180, 8181 and 8199).
- Arguments, if given, are passed to `playwright test` in place of the default cases' title filter.

The default cases cover search answers for an available name, Check your code and its rate-limit
countdown, code orders to `registered` with and without links, a code order's name code restored and
revealed in another browser, Edit links on a name registered with a code (the other Edit links case
answers its own name and record requests in the page), a link whose data cannot be read now, a
community name's marker and its Check your order wording, and a code order of a name the contracts'
owner lists on chain, whose register records its buyer's acceptance of the community terms. "search answers every kind of name" is not among them: it takes the chain down
with `/control/search`.

Each case sends its own `X-Forwarded-For`, and the ini sets `trust_forwarded_for = on`, so each case
is its own client for the service's per-client limits, as for the mock's code limit. The script stops
everything it started, deletes its folder when the cases pass, and keeps it with the logs, the ini and
Playwright's results when they fail.
