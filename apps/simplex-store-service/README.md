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
- in service mode with `--service-config`, also serves the built web app (`npm run build` in `web/`) and, on the hosts in `names_hosts`, the names site (see [Deployment](#deployment)), `POST /api/invoice` and `GET /api/invoice/:id`, the BTCPay and Stripe webhook routes, and a payment poller, seeding its price/offer catalog on every start,
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
  revoked.
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
Rotating is a change to both and a restart, once clients trust the new index.

The command line wins over the file when both `--issuer-key-idx` and `--issuer-secret` are given.
Note that a secret passed as a flag is visible to every user on the machine through `ps`, where one
in the ini is only as readable as the file: `store_service.ini` is gitignored and already holds the
provider API keys and webhook secrets.

Other options:

- `--service-config INI_FILE`: path to `store_service.ini`. Omit it to run the chat/RPC side
  only; the process never starts a web listener without it, and never starts one under
  `--run-cli`, which parses and validates the whole file (`[listener] static_dir` included) but
  uses only its `[issuer]` section. An issuer key is still required either way.
- `--service-name NAME`: the bot's display name, without `*`s or spaces (default `SimpleX Badges`).
- `--client-service`: use the client service certificate.
- also accepts the standard SimpleX Chat core options — database path, SMP/XFTP servers,
  `--socks-proxy`, `--log-level`/`-l`, and the rest — run `simplex-store-service --help`
  for the complete list.

### Running the web checkout

`store_service.ini` holds the listener bind address and `static_dir`, an optional
`[btcpay]` section (omitting it disables Bitcoin and Monero), an optional `[stripe]`
section (omitting it disables card payments), an optional `[group]` section (omitting it
turns off the group's commands, though a group created earlier still has its code messages updated)
and the poll cadence.
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

## Deployment

`scripts/store-service/` holds the production deployment: the image (`Dockerfile`, built from
the repository root), `compose.yml`, the ini template `store_service.ini.example` and the egress
rule `egress.sh`. The service runs with `+client_postgres` and host networking; one Caddy serves
both sites from the folders the service exports at boot and proxies the API to it.

### Upgrading from the badge service

The rename to StoreService changed what the operator runs:

| Before | After |
|---|---|
| `scripts/badge-service/` | `scripts/store-service/` |
| compose service `badge-service`, image `simplex-badge-service` | compose service `store-service`, image `simplex-store-service` |
| `badge_service.ini`, mounted at `/etc/simplex-badge-service/badge_service.ini` | `store_service.ini`, mounted at `/etc/simplex-store-service/store_service.ini` |
| the binary `simplex-badge-service` | `simplex-store-service` |

Move `badge_service.ini` to `scripts/store-service/store_service.ini`, and point Caddy's badges
root at `scripts/store-service/web`, which the service fills at boot. Then start the new service
with

```
docker compose up -d --build --remove-orphans
```

from `scripts/store-service/`. Without `--remove-orphans` the old `badge-service` container keeps
running beside the new one, on the same port and database. The database `badge_service`,
`BADGE_DB_CONN`, the `sx_badge_service_` tables and the chat database prefix
`simplex_badge_service` are unchanged.

### Two sites

The image carries the badges build at `/srv/web` (`static_dir`). The names site (simplex.domains)
is configured beside it in `[listener]`. The image does not build the names site yet, so mount a
names build into the container for `names_static_dir` until it does.

- `names_static_dir`: the names build, the directory `npm run build` writes in the names webapp
  (`dist/`);
- `names_export_dir`: where the service writes the names site at boot, with the Stripe
  publishable key in its shell, as `webapp_export_dir` is for the badges site. `compose.yml`
  bind-mounts `./names` there (`/srv/names-export`), and `./web` at `/srv/webapp`;
- `names_hosts`: the comma-separated hosts the names site is served on when `serve_webapp = on`.

The service refuses to start with `names_hosts` or `names_export_dir` but no `names_static_dir`.

With `serve_webapp = on` (all in one), the listener picks the site by the request's `Host`, in
lower case and without its port or a trailing dot, as `names_hosts` entries are read: a host in
`names_hosts` gets the names site, any other host or none the badges site. Every path
outside `/api/` and `/webhooks/` is a file of the chosen site, `/` and `/index.html` its shell. The
API answers on every host.

### Caddy

The Caddyfile is not in this repository. Its two host blocks, with the exported folders at
`/srv/store-service/web` and `/srv/store-service/names` and the listener at `127.0.0.1:8080`:

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
		root * /srv/store-service/web
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
		Content-Security-Policy "default-src 'self'; script-src 'self' 'sha256-i5ar+ITNdQiucbmin4/AN6DAY5LbAWeq3at+RiFoUoY=' 'sha256-Jhsw0wf9pI4icAxqab9mIjf34YdecDZeLVN+/KVZW+4=' https://js.stripe.com https://*.js.stripe.com; frame-src https://js.stripe.com https://*.js.stripe.com https://hooks.stripe.com; connect-src 'self' https://api.stripe.com; img-src 'self' data: https://*.stripe.com; style-src 'self'; frame-ancestors 'none'; base-uri 'none'; form-action 'none'"
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

`egress.sh` writes this as the chain `STORE_SERVICE_EGRESS` in `iptables` and `ip6tables`, and
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
mkdir -p web names
sudo chown -R simplex-store:simplex-store web names
sudo chown simplex-store store_service.ini && sudo chmod 600 store_service.ini
docker compose up -d --build --remove-orphans
```

On a Docker bridge network instead, `--bridge <interface>` matches traffic from that network's
bridge interface in `DOCKER-USER` (traffic forwarded to other hosts) and `INPUT` (the host's own
addresses), and needs no `--uid`. The script refuses an interface that is not a bridge or that holds a default route (a bridge
carrying the host's uplink, such as `br0` or `vmbr0`, whose inbound connections it would refuse),
and stops
when `DOCKER-USER` is missing (Docker's nftables backend, `"iptables": false` or `"ip6tables":
false` in its daemon config, or Docker not started yet): the forwarded traffic would then pass
unfiltered. The interface is `docker0` for the default network; for another, it is the network's
`com.docker.network.bridge.name` option when set, else `br-` and the first 12 characters of its id:

```
docker network inspect -f '{{.Id}} {{index .Options "com.docker.network.bridge.name"}}' <network>
```

Install it as root from `scripts/store-service/`, here for a service uid of 998. `--db` defaults to
the environment's `BADGE_DB_CONN`, else to compose's default
(`postgresql://simplex@127.0.0.1:5432/badge_service`); `sudo` drops the environment, so pass it
when compose runs with another value:

```
sudo ./egress.sh --dry-run --uid 998
sudo ./egress.sh --uid 998
sudo ./egress.sh --check --uid 998
```

- `--dry-run` prints the `iptables-restore` input an install would commit, read against the
  current rules, and changes nothing; read the allowed endpoints in it.
- `--check` exits 1 and names what differs unless every rule is in place, the chain holds nothing
  else, and each jump is the first rule of its chain.
- Names are resolved when the rule is installed: run it again after changing the ini, the
  database string or the resolvers, and after a private endpoint's address changes. A name the
  host cannot resolve (a container name) stops the install; pass its address with
  `--allow ADDRESS:PORT`, as for any other endpoint the service needs, such as a SOCKS proxy.
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

`sudo ./egress.sh --remove --uid 998` (or `--bridge <interface>`) deletes the jumps an install
adds for that user or network, then the chain, in `iptables` and `ip6tables`. Running it again
changes nothing, and `--check` then reports the chain absent.

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
`<length>` letters, 6 to 63, for `years` years, 2 to 10 (default 2). The status is as above: an
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
why issuing codes lives in the service rather than in core.

### From the group

With `[group]` in `store_service.ini`, the service manages one group and serves three commands in
it: `/issue <type> [months <M>] [uses <N>]` and `/bulk <type> [months <M>] count <B>` for moderators
and above, `/revoke <code>` for admins and owners. `months` is 1 to 255, `uses` 1 to 1000 and
`count` 1 to 100; a value outside these gets the usage reply. The name forms are
`/issue name <length> [years <Y>]` and `/bulk name <length> [years <Y>] count <B>`, with the
bounds of `//issue name`; a name code issued in the group is single-use and `free`. `/revoke`
takes badge codes only: withdraw a name code with `//revoke` in `--run-cli` mode. A member's role
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
