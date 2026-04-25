# SimpleX Support Bot

A business-address bot that triages incoming support chats, optionally runs them through Grok, and routes handoffs to a team group.

## Prerequisites

- Node.js (v18+; v24 tested)
- For PostgreSQL backend only: `libpq5` on the host (`apt install libpq5` on Debian/Ubuntu) and Linux x86_64
- `GROK_API_KEY` env var (xAI key) — only required when you want Grok replies; the bot runs without it

## 1. Build

```bash
cd apps/simplex-support-bot
./build.sh
```

This builds `@simplex-chat/types`, `simplex-chat` (native lib), and the bot. SQLite is the default.

To build with **PostgreSQL** instead, set the env var before running `./build.sh`:

```bash
SIMPLEX_BACKEND=postgres ./build.sh
```

Or put `simplex_backend=postgres` in `.npmrc` at the repo root (picked up by all nested `npm install`s).

## 2. Run — SQLite (default)

```bash
export GROK_API_KEY=xai-...        # optional, only if you want Grok
./start.sh --team-group "Support Team"
```

Default locations:
- DB files: `./data/simplex_chat.db`, `./data/simplex_agent.db`
- State JSON: `./data/state.json`

Override any of those with flags — see [Flags](#flags).

## 3. Run — PostgreSQL

```bash
export GROK_API_KEY=xai-...        # optional
./start.sh \
  --team-group "Support Team" \
  --pg-conn "postgres://simplex:pass@localhost/simplex_bot"
```

The bot must have been built with `SIMPLEX_BACKEND=postgres` (step 1). Schema prefix defaults to `simplex_v1` — pass `--pg-schema <name>` to namespace a different prefix.

## Flags

All flags are parsed via `node:util.parseArgs` (strict mode — unknown flags are rejected).

| Flag | Backend | Required | Default | Description |
|---|---|---|---|---|
| `--team-group` | | yes | — | team group display name |
| `--state-file` | both | no | `./data/state.json` | path to bot state JSON |
| `--sqlite-file-prefix` | sqlite | no | `./data/simplex` | DB file prefix (creates `<prefix>_chat.db`, `<prefix>_agent.db`) |
| `--sqlite-key` | sqlite | no | (unencrypted) | SQLCipher encryption key |
| `--pg-conn` | postgres | yes | — | PostgreSQL connection string |
| `--pg-schema` | postgres | no | `simplex_v1` | schema prefix used for bot tables |
| `-a` / `--auto-add-team-members` | | no | | comma-separated `ID:name` pairs (e.g. `1:Alice,2:Bob`) |
| `--timezone` | | no | `UTC` | IANA zone for weekend detection |
| `--complete-hours` | | no | `3` | auto-complete chats after N hours idle (`0` disables) |
| `--card-flush-seconds` | | no | `300` | debounce card state writes |
| `--context-file` | | required with `GROK_API_KEY` | | text file with Grok system context |

Flag values containing leading dashes must use `--flag=value` form (e.g. `--sqlite-key=-abc`).

## Environment variables

| Var | Purpose |
|---|---|
| `GROK_API_KEY` | xAI API key; enables Grok replies |
| `SIMPLEX_BACKEND` | install-time selector for the native lib backend (`sqlite` or `postgres`); `npm_config_simplex_backend` from `.npmrc` works the same way |

## State file

`--state-file` holds persisted IDs between runs (team group, Grok contact). It's a local JSON regardless of DB backend. Parent directory must exist.

## Troubleshooting

- **"Option '--X' argument is ambiguous"** — use `--X=value` form when the value starts with `-`.
- **Postgres: connect errors** — check that `libpq5` is installed and the connection string is correct. The native lib errors will surface at `ChatApi.init(...)` on startup.
- **Sqlite: DB files missing** — check the directory pointed to by `--sqlite-file-prefix` exists.
- **"--pg-conn is required when backend is postgres"** — you built with the postgres backend (see `installed.txt` in `packages/simplex-chat-nodejs/libs/`) but didn't pass `--pg-conn`.
