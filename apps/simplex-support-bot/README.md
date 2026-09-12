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
| `--broadcasters` | both | no | | comma-separated `ID:name` pairs of contacts allowed to use `/broadcast` in the team group |
| `--timezone` | both | no | `UTC` | IANA zone for weekend detection |
| `--complete-hours` | both | no | `3` | auto-complete chats after N hours idle (`0` disables) |
| `--card-flush-seconds` | both | no | `300` | debounce card state writes |
| `--context-file` | both | required with `GROK_API_KEY` | | text file with Grok system context |
| `-h` / `--help` | both | no | | show usage and exit |

## Broadcasts

A contact listed in `--broadcasters` sends `/broadcast <text>` in the team group. The bot sends the text as a feed message to every customer group and every direct contact of the bot, replies that the broadcast is queued, and replies again when delivery completes or fails. The text after `/broadcast` may span several lines.

A broadcaster must be a *contact* of the bot, not only a team group member: the bot authorises the sender by `memberContactId`. The bot DMs every team group member the contact id it knows them by; pass that id to `--broadcasters` and restart.

Feeds are part of the core, so `/broadcast` needs a `libsimplex` and types built from this tree — no released version has them. See [Running against this tree](#running-against-this-tree).

## Environment variables

| Var | Purpose |
|---|---|
| `GROK_API_KEY` | xAI API key; enables Grok replies |
| `SIMPLEX_BACKEND` | alternative to `.npmrc` for selecting the install backend (`sqlite` or `postgres`) |
| `SIMPLEX_LIBS_DIR` | read by `simplex-chat`'s preinstall: install the `libsimplex` in this directory instead of downloading the release |
| `NODE_OPTIONS` | `--max-old-space-size=8192` on a bot with many chats: single API responses outgrow the default heap |

## Running against this tree

The manifest points at the released `simplex-chat` and `@simplex-chat/types`, which is what a deployment installs. `scripts/support-bot/Dockerfile` builds both from this checkout instead, with the core; by hand:

```bash
scripts/desktop/build-lib-linux.sh     # libsimplex, hours on a cold cabal store
libs=$(pwd)/apps/multiplatform/common/src/commonMain/cpp/desktop/libs/linux-$(uname -m)

(cd packages/simplex-chat-client/types/typescript && npm install && npx tsc)

cd packages/simplex-chat-nodejs
SIMPLEX_LIBS_DIR=$libs npm install
npm install --no-save ../simplex-chat-client/types/typescript
npx tsc && cp src/simplex.* dist/

cd ../../apps/simplex-support-bot
npm ci
SIMPLEX_LIBS_DIR=$libs npm install --no-save \
  ../../packages/simplex-chat-nodejs \
  ../../packages/simplex-chat-client/types/typescript
```

`SIMPLEX_LIBS_DIR` is needed in the last step too: npm re-runs the linked library's preinstall, which downloads the released libs without it. `rm -rf node_modules && npm ci` reverts.

## Troubleshooting

- **`--pg-conn is required when backend is postgres`** — the postgres backend is installed but you didn't pass a connection string.
- **`libpq5` errors at startup** — install `libpq5` on the host (`apt install libpq5` on Debian/Ubuntu).
- **`ENOENT: no such file or directory, open './data/state.json'`** — the parent directory of `--state-file` must exist; `mkdir -p data` before starting.
- **Wrong backend installed** — check `node_modules/simplex-chat/libs/installed.txt`. Edit `.npmrc`, then `rm -rf node_modules && npm install` to switch (`npm install` alone won't re-run the dep's preinstall).
- **`JavaScript heap out of memory`** — a response outgrew the default heap; restart with `NODE_OPTIONS=--max-old-space-size=8192`.
- **`libpq` connection error** at startup with sqlite-flavored config (or vice versa) — `.npmrc` was changed but libs weren't reinstalled. See "Switching backends" above.
