# Store service deployment

- `compose.yml`: the service, built from `Dockerfile`, with `./config` mounted read-only.
- `compose.dev.yml`: adds a local chain and database to `compose.yml`, for development.
- `Dockerfile`, `Dockerfile.dockerignore`: the image, built from the repository root.
- `store_service.ini.example`: the template of `config/store_service.ini`.
- `dev/chain.sh`: the dev chain, which also writes the dev `config/store_service.ini`.
- `dev/contract-run.sh`: runs the names webapp's end-to-end cases against the service.
- `ops/egress.sh`: the egress rule of a production host.
- `config/`, `badges/`, `names/`, `data/`, `.env`: local state, gitignored.

Production, with PostgreSQL at `BADGE_DB_CONN` (by default
`postgresql://simplex@127.0.0.1:5432/badge_service`), and the ini filled in before the last command:

```
install -d -m 700 config
install -m 600 store_service.ini.example config/store_service.ini
docker compose up -d --build
```

Development, from a fresh clone:

```
git clone https://github.com/simplex-chat/simplex-chat && cd simplex-chat
git checkout sh/store-service && git submodule update --init apps/simplex-domains
cd scripts/store-service
echo "COMPOSE_FILE=compose.yml:compose.dev.yml" > .env
docker compose up -d --build
```

The names site is then on `http://127.0.0.1:8081`, the badges site on `http://127.0.0.1:8080`.

Everything else is in [apps/simplex-store-service/README.md](../../apps/simplex-store-service/README.md#deployment).
