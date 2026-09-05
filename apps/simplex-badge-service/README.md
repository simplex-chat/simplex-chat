# SimpleX badge service

`simplex-badge-service` runs the whole supporter-badge service as one process, two
lanes: a SimpleX chat bot that answers service RPC over a double-ratchet contact
address, and — only when `--service-config` names a `badge_service.ini` — the badge-codes
web checkout (BTCPay payment, code issuance, the poller; card payment is not served in
this build). **Without
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
- issues redemption codes, storing only their `SHA-256` and printing each code once,
- does not accept contact requests unless `[dev] chat_redeem` is on: the address is for RPC only,
- in service mode with `--service-config`, also serves the built web app (`npm run build` in `web/`), `POST /api/invoice` and `GET /api/invoice/:id`, the BTCPay webhook route, and a payment poller, seeding its price/offer catalog on every start,
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
  below: it starts no web listener and no poller, and `[dev] chat_redeem` does not apply to it,
  whatever `--service-config` says.
- `--no-address`: skip address creation on start-up (for operators who provision the address themselves).
The service cannot sign credentials without an issuer key and refuses to start without one:

- `--issuer-key-idx IDX` — the index the apps find the matching public key under (`badgePublicKeys` in `ChatConfig`).
- `--issuer-secret SECRET` — the issuer secret from `simplex-chat badge keygen`.

The service checks the secret against the configured public key at that index and refuses to start
if they disagree: credentials signed with the wrong key cannot be verified by any client, and the
codes redeemed against them would be spent for nothing.

The keys can come from `badge_service.ini` instead, which is where more than one can be listed:

```ini
[issuer]
default = key_1
key_1 = <secret from `simplex-chat badge keygen`>
key_3 = <secret>
```

`key_<n>` is the index clients verify against (`badgePublicKeys` in `ChatConfig`), and `default`
names the one that signs. Only that key signs; the others are listed so that rotating is a change
to `default` and a restart, with the old key still present to roll back to. Every key in the
section is checked at startup, not just the default, so a key that clients could not verify fails
before anyone rotates onto it.

The command line wins over the file when both `--issuer-key-idx` and `--issuer-secret` are given.
Note that a secret passed as a flag is visible to every user on the machine through `ps`, where one
in the ini is only as readable as the file: `badge_service.ini` is gitignored and already holds the
BTCPay API key and webhook secret.

Other options:

- `--service-config INI_FILE`: path to `badge_service.ini`. Omit it to run the chat/RPC side
  only; the process never starts a web listener without it, and never starts one under
  `--run-cli`, which parses and validates the whole file (`[listener] static_dir` included) but
  uses only its `[issuer]` section. An issuer key is still required either way.
- `--service-name NAME`: the bot's display name, without `*`s or spaces (default `SimpleX Badges`).
- `--client-service`: use the client service certificate.
- also accepts the standard SimpleX Chat core options — database path, SMP/XFTP servers,
  `--socks-proxy`, `--log-level`/`-l`, and the rest — run `simplex-badge-service --help`
  for the complete list.

### Running the web checkout

`badge_service.ini` holds the listener bind address and `static_dir`, an optional
`[btcpay]` section (omitting it disables Bitcoin and Monero) and the poll cadence.
`badge_service.ini.example` is the committed template; `badge_service.ini` itself is
gitignored, since a real one holds an API key and a webhook secret.

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

### The checkout endpoints

The browser in `web/` is the only client of the `/api` routes; the webhook below is BTCPay's.
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

Every `/api` refusal is `{"error": "<code>"}`. Besides the codes above, any of them can answer
`internal`, an unknown id answers `not_found`, and a wrong verb answers `method_not_allowed` —
ten codes in all, which is what the browser's `WIRE_ERROR_CODES` lists. The webhook route is the exception:
it answers 200, 400 or 413 with an empty body, because BTCPay is the only caller and nothing it
could read would change what it does. A wrong verb on any route, that one included, answers
`method_not_allowed`.

### Redeeming over chat, for local testing

```ini
[dev]
chat_redeem = on
```

With this on, the service accepts contact requests and answers `/redeem <code>` from a contact
with the credential as one-line JSON, ready to paste into a client as `/badge add <json>`. Off by
default, and only `on`/`off` parse, so a typo cannot silently arm it. It applies to the service
mode only; `--run-cli` ignores it.

Keep it off anywhere real. The service RPC signs over a master key only the client holds; here
there is no client key, so the service generates one and hands it over with the credential, which
means it can link every badge it issues this way. `simplex-chat badge sign` has the same property
and is the offline equivalent.

## Issuing codes

Issuing a code is an operator command sent to the running service in `--run-cli` mode, not a way
to start it — so codes are issued without a second process touching the service's database:

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
learns nothing from trying. Revoking is not repeatable: the second attempt says so.

Core parses `//...` into `CustomChatCommand` and leaves it to the service's `preCmdHook`, which is
why issuing codes lives in the service rather than in core.
