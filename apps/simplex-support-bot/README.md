# SimpleX Support Bot

A business-address bot that triages incoming support chats, optionally runs them through Grok, and routes handoffs to a team group.

## Prerequisites

- Node.js v18 or newer (v24 tested)
- `GROK_API_KEY` env var (xAI) — optional; the bot runs without it
- For the PostgreSQL backend: Linux x86_64, `libpq5` installed on the host, and a reachable PostgreSQL server

## Install & build

```bash
cd apps/simplex-support-bot
npm install
npm run build    # tsc
```

The native library is downloaded on first start into the user cache. To download it ahead of time (for example in a Dockerfile), run as the user that runs the bot:

```sh
npx simplex-chat install --backend sqlite
npx simplex-chat install --backend postgres   # Linux x86_64 only
```

Select the database with `--db sqlite` (default) or `--db postgres`.

## Run

```bash
mkdir -p data    # state file lives here by default

# SQLite (default)
npm start -- --team-group "Support Team"

# PostgreSQL
npm start -- --team-group "Support Team" \
             --db postgres --pg-conn "postgres://user:pass@host/db"
```

## Flags

Run `npm start -- --help` for the auto-generated reference. Summary:

| Flag | Backend | Required | Default | Description |
|---|---|---|---|---|
| `--team-group` | both | yes | — | team group display name |
| `--state-file` | both | no | `./data/state.json` | path to bot state JSON |
| `--db` | both | no | `sqlite` | `sqlite` or `postgres` |
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
| `SIMPLEX_LIBS_DIR` | directory with a local libsimplex build for the `--db` backend, instead of the downloaded one |

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

- **`--pg-conn is required with --db postgres`** — pass a connection string, or use `--db sqlite`.
- **`libpq5` errors at startup** — install `libpq5` on the host (`apt install libpq5` on Debian/Ubuntu).
- **`ENOENT: no such file or directory, open './data/state.json'`** — the parent directory of `--state-file` must exist; `mkdir -p data` before starting.
