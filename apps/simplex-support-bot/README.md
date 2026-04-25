# SimpleX Support Bot

A business-address bot that triages incoming support chats, optionally runs them through Grok, and routes handoffs to a team group.

## Prerequisites

- Node.js v18 or newer (v24 tested)
- `GROK_API_KEY` env var (xAI) — optional; the bot runs without it
- For the PostgreSQL backend: Linux x86_64, `libpq5` installed on the host, and a reachable PostgreSQL server

## Install & build

```bash
cd apps/simplex-support-bot
npm install      # downloads native libs + transitive deps
npm run build    # tsc
```

By default this installs the **SQLite** backend.

To use **PostgreSQL** instead, drop a `.npmrc` next to `package.json` *before* `npm install`:

```bash
echo 'simplex_backend=postgres' > .npmrc
npm install      # now pulls postgres-flavored native libs
npm run build
```

`.npmrc` lives next to the package — npm reads it natively, no extra setup.

### Switching backends

`npm install` is a no-op for already-installed deps, so editing `.npmrc` and re-running `npm install` will *not* re-trigger `simplex-chat`'s preinstall. To switch backends, force a clean install:

```bash
rm -rf node_modules
npm install      # download-libs.js re-runs and pulls the right native lib
```

## Run

```bash
mkdir -p data    # state file lives here by default

# SQLite (default)
npm start -- --team-group "Support Team"

# PostgreSQL
npm start -- --team-group "Support Team" \
             --pg-conn "postgres://user:pass@host/db"
```

The bot runs via `npm start` so npm can expose `.npmrc` settings to the process — `detectBackend()` reads `npm_config_simplex_backend` to know which backend was installed.

## Flags

Run `npm start -- --help` for the auto-generated reference. Summary:

| Flag | Backend | Required | Default | Description |
|---|---|---|---|---|
| `--team-group` | both | yes | — | team group display name |
| `--state-file` | both | no | `./data/state.json` | path to bot state JSON |
| `--sqlite-file-prefix` | sqlite | no | `./data/simplex` | DB file prefix (creates `<prefix>_chat.db`, `<prefix>_agent.db`) |
| `--sqlite-key` | sqlite | no | (unencrypted) | SQLCipher encryption key |
| `--pg-conn` | postgres | yes | — | PostgreSQL connection string |
| `--pg-schema` | postgres | no | `simplex_v1` | schema prefix used for bot tables |
| `-a` / `--auto-add-team-members` | both | no | | comma-separated `ID:name` pairs (e.g. `1:Alice,2:Bob`) |
| `--timezone` | both | no | `UTC` | IANA zone for weekend detection |
| `--complete-hours` | both | no | `3` | auto-complete chats after N hours idle (`0` disables) |
| `--card-flush-seconds` | both | no | `300` | debounce card state writes |
| `--context-file` | both | required with `GROK_API_KEY` | | text file with Grok system context |
| `-h` / `--help` | both | no | | show usage and exit |

## Environment variables

| Var | Purpose |
|---|---|
| `GROK_API_KEY` | xAI API key; enables Grok replies |
| `SIMPLEX_BACKEND` | alternative to `.npmrc` for selecting the install backend (`sqlite` or `postgres`) |

## Local development against unreleased lib changes

This package depends on `simplex-chat` from npm. To test against an in-tree version:

```bash
# In packages/simplex-chat-nodejs
npm link

# In apps/simplex-support-bot
npm link simplex-chat
```

`npm unlink simplex-chat && npm install` reverts to the registry version.

## Troubleshooting

- **`--pg-conn is required when backend is postgres`** — the postgres backend is installed but you didn't pass a connection string.
- **`libpq5` errors at startup** — install `libpq5` on the host (`apt install libpq5` on Debian/Ubuntu).
- **`ENOENT: no such file or directory, open './data/state.json'`** — the parent directory of `--state-file` must exist; `mkdir -p data` before starting.
- **Wrong backend installed** — check `node_modules/simplex-chat/libs/installed.txt`. Edit `.npmrc`, then `rm -rf node_modules && npm install` to switch (`npm install` alone won't re-run the dep's preinstall).
- **`libpq` connection error** at startup with sqlite-flavored config (or vice versa) — `.npmrc` was changed but libs weren't reinstalled. See "Switching backends" above.
