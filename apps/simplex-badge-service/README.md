# SimpleX badge service

`simplex-badge-service` runs the whole supporter-badge service as one process, two
lanes: a SimpleX chat bot that answers service RPC over a double-ratchet contact
address, and — only when `--service-config` names a `badge_service.ini` — the badge-codes
web checkout (BTCPay and Stripe payment, code issuance, the poller). **Without
`--service-config` the web listener does not start at all**; the process still runs the
chat side and nothing else.

The wire protocol for the RPC side is
[`docs/protocol/badges-rpc.md`](../../docs/protocol/badges-rpc.md). The web checkout is
specified in
[`plans/badges-codes/2026-08-27-badge-codes.md`](../../plans/badges-codes/2026-08-27-badge-codes.md),
whose §9 is the `badge_service.ini` reference, with the implementation plan in
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
- in service mode with `--service-config`, also serves the built web app (`npm run build` in `web/`), `POST /api/invoice` and `GET /api/invoice/:id`, the BTCPay and Stripe webhook routes, and a payment poller, seeding its price/offer catalog on every start,
- owns the `sx_badge_service_`-prefixed tables and its own migrations table (`sx_badge_service_migrations`).

Every other command answers `unsupported_version`, or `unknown_purchase_key` when the key that
signed it is not one the service has stored — every command but `redeemBadgeCode` needs a
purchase that already exists.

## Build

Build prerequisites and the general contribution flow are in [`docs/CONTRIBUTING.md`](../../docs/CONTRIBUTING.md).

```
cabal build exe:simplex-badge-service
```

## Run

```
simplex-badge-service --help
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

The key can come from `badge_service.ini` instead:

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
in the ini is only as readable as the file: `badge_service.ini` is gitignored and already holds the
provider API keys and webhook secrets.

Other options:

- `--service-config INI_FILE`: path to `badge_service.ini`. Omit it to run the chat/RPC side
  only; the process never starts a web listener without it, and never starts one under
  `--run-cli`, which parses and validates the whole file (`[listener] static_dir` included) but
  uses only its `[issuer]`, `[apple]`, `[google]` and `[dev]` sections. An issuer key is
  still required either way.
- `--service-name NAME`: the bot's display name, without `*`s or spaces (default `SimpleX Badges`).
- `--client-service`: use the client service certificate.
- also accepts the standard SimpleX Chat core options — database path, SMP/XFTP servers,
  `--socks-proxy`, `--log-level`/`-l`, and the rest — run `simplex-badge-service --help`
  for the complete list.

### Running the web checkout

`badge_service.ini` holds the listener bind address and `static_dir`, an optional
`[btcpay]` section (omitting it disables Bitcoin and Monero), an optional `[stripe]`
section (omitting it disables card payments), an optional `[group]` section (omitting it
turns off the group's commands, though a group created earlier still has its code messages updated)
and the poll cadence.
`badge_service.ini.example` is the committed template; `badge_service.ini` itself is
gitignored, since a real one holds API keys and webhook secrets.

The full walkthrough is in
[`web/README.md`](web/README.md#running-the-real-service-against-this-build). Short version:

```
cd apps/simplex-badge-service/web && npm install && npm run build && cd ../../..
cp apps/simplex-badge-service/badge_service.ini.example apps/simplex-badge-service/badge_service.ini
cabal run simplex-badge-service -- \
  --issuer-key-idx IDX --issuer-secret SECRET \
  --service-config apps/simplex-badge-service/badge_service.ini
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
`canviewstoresettings` to log the store's live payment methods at startup, and
`canmodifyinvoices` so `POST /api/invoice/:id/cancel` can invalidate an invoice at BTCPay
rather than only in this store.

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

The service fills `publishable_key` into the shell at boot: the built `index.html` ships
with an empty `<meta id="stripe-publishable-key" name="stripe-publishable-key" content="">`
(`web/public/index.html`), and the listener serves a copy with the configured key
substituted in, so the ini is the only place the key is set. With no `[stripe]` section the
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
root_certificates = /etc/simplex-badge-service/AppleRootCA-G3.cer
api_key_id = 2X9R4HXF34
api_issuer_id = 57246542-96fe-1a63-e053-0824d011072a
api_private_key_file = /etc/simplex-badge-service/AuthKey_2X9R4HXF34.p8

[google]
package_name = chat.simplex.app
service_account_file = /etc/simplex-badge-service/play-service-account.json
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

## Issuing codes

Operators issue codes two ways: from the service's own command line in `--run-cli` mode, and from the
managed group in service mode. Both are commands to a running process, so no second process
touches the service's database.

### From the command line

The command is sent to the running service in `--run-cli` mode, not a way to start it:

```
//issue <badge_type> [months] [paid|unpaid|free]
//issue supporter 12
```

`months` defaults to 1 and must be between 1 and 255; the status defaults to `free` and records
whether the code was sold (`paid`), is awaiting payment (`unpaid`), or was issued by an operator
(`free`). Redemption refuses an `unpaid` code with `payment_pending`: the web checkout writes the
code row when the invoice is created, and settlement is what marks it paid.

The code is printed once and only its `SHA-256` is stored, so a code that is not copied when it is
shown cannot be recovered.

A code that leaked, or that was refunded, is withdrawn the same way:

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
why issuing codes lives in the service rather than in core.

### From the group

With `[group]` in `badge_service.ini`, the service manages one group and serves three commands in
it: `/issue <type> [months <M>] [uses <N>]` and `/bulk <type> [months <M>] count <B>` for moderators
and above, `/revoke <code>` for admins and owners. `months` is 1 to 255, `uses` 1 to 1000 and
`count` 1 to 100; a value outside these gets the usage reply. A member's role is checked as the
service last saw it, so a command sent by a moderator just demoted or removed can still run if it
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
