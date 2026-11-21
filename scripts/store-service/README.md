# Store service deployment

- `compose.yml`: the service, built from `Dockerfile`, with `./config` mounted read-only, and the
  webapps service, which copies both built sites into `./webapps/badges` and `./webapps/names` for
  Caddy with rsync and exits.
- `compose.dev.yml`: adds a local chain and database to `compose.yml`, for development.
- `Dockerfile`, `Dockerfile.dockerignore`: the image, built from the repository root.
- `store_service.ini.example`: the template of `config/store_service.ini`.
- `dev/chain.sh`: the dev chain, which also writes the dev `config/store_service.ini`.
- `dev/contract-run.sh`: runs the names webapp's end-to-end cases against the service.
- `dev/lightning.yml`, `dev/lightning.sh`: BTCPay Server 2.4.4 on Bitcoin regtest with its Lightning node and a
  payer's node, for the Lightning end-to-end tests (below).
- `config/`, `webapps/`, `data/`, `.env`, `dev/lightning.env`: local state, gitignored.

Production, with PostgreSQL at `BADGE_DB_CONN` (by default
`postgresql://simplex@127.0.0.1:5432/badge_service`), and the ini filled in before the last command:

```
git submodule update --init ../../apps/simplex-domains
install -d -m 700 config
install -d webapps
install -m 600 store_service.ini.example config/store_service.ini
docker compose up -d --build
```

Development, from a fresh clone:

```
git clone https://github.com/simplex-chat/simplex-chat && cd simplex-chat
git checkout names && git submodule update --init apps/simplex-domains
cd scripts/store-service
echo "COMPOSE_FILE=compose.yml:compose.dev.yml" > .env
install -d webapps
docker compose up -d --build
```

The names site is then on `http://127.0.0.1:8081`, the badges site on `http://127.0.0.1:8080`.

Lightning end-to-end tests, against a real BTCPay Server on regtest:

```
dev/lightning.sh
```

With this run, the containers of `dev/lightning.yml` are started, a channel is opened from the payer's
node to BTCPay's, and a store is created with `BTC-CHAIN` and `BTC-LN` enabled, BTC_USD fixed at
108000.12, a webhook to `http://host.docker.internal:8099/webhooks/btcpay` (the port is
`STORE_LIGHTNING_SERVICE_PORT`) and an API key with the four permissions the service needs. The
settings are written to `dev/lightning.env`:

| Variable | Value |
|---|---|
| `STORE_LIGHTNING_BTCPAY_URL` | BTCPay's Greenfield API, `http://127.0.0.1:49392` |
| `STORE_LIGHTNING_BTCPAY_API_KEY` | the store's API key |
| `STORE_LIGHTNING_BTCPAY_STORE_ID` | the store |
| `STORE_LIGHTNING_BTCPAY_WEBHOOK_SECRET` | the webhook's secret |
| `STORE_LIGHTNING_PAYER_URL` | the payer node's REST API, `http://127.0.0.1:35532`; an invoice is paid with `POST /v2/router/send` and `{"payment_request": "<BOLT11>", "timeout_seconds": 60, "fee_limit_sat": "1000", "no_inflight_updates": true}` |
| `STORE_LIGHTNING_SERVICE_PORT` | the port on which the store service is served in the tests, for the webhook |

With these variables in the environment, the test group `StoreService Lightning e2e` is run against
the stack from the repository root; without them it is reported as pending:

```
set -a; . scripts/store-service/dev/lightning.env; set +a
cabal test simplex-chat-test --ghc-options=-O0 --test-option=-m --test-option='StoreService Lightning e2e'
```

The two sites are paid through the same stack in a browser with `dev/contract-run.sh` and
`LIGHTNING_ENV=scripts/store-service/dev/lightning.env`, as
[apps/simplex-store-service/README.md](../../apps/simplex-store-service/README.md#running-the-webapps-cases-against-the-service)
describes.

On a second run of `dev/lightning.sh`, the channel is kept and another store is created. The stack is
removed, from `scripts/store-service`, with `docker compose -f dev/lightning.yml down -v`.

Everything else is in [apps/simplex-store-service/README.md](../../apps/simplex-store-service/README.md#deployment).
