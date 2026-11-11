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
| `--config` | both | no | | TOML file with option values and message texts, see [Config file](#config-file) |
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
| `--dry-run` | both | no | | check config, database and state, then exit |
| `--allow-migrations` | both | no | | with `--dry-run`: apply pending migrations instead of reporting them |
| `-h` / `--help` | both | no | | show usage and exit |

## Config file

`--config bot.toml` reads option values and message texts from a TOML file. Keys are the flag names without `--`; a list is joined with commas. An option given on the command line takes precedence over the file. `--dry-run` and `--allow-migrations` are command-line only.

```toml
team-group = "SimpleX Support Team"
auto-add-team-members = ["142681:Alice"]
context-file = "/etc/support-bot/grok-context.yaml"
timezone = "UTC"

[messages]
welcome = """
Hello! This is a *SimpleX team* support bot - not an AI.
Discover public groups: [simplex.chat/directory](https://simplex.chat/directory)

Please ask any questions about SimpleX Chat."""
queue = "The team will reply to your message within {hours} hours."
```

A message left out keeps its built-in text. `{hours}` is replaced with 24 on weekdays and 48 at weekends in `--timezone`. The welcome message is updated in the bot address on the next start.

| Message | Sent when |
|---|---|
| `welcome` | a customer connects to the business address |
| `queue` | a customer's first message is queued for the team, and after `grokUnavailable` when `/grok` was the first message |
| `queueGrok` | appended to `queue` after an empty line when Grok is enabled |
| `grokActivated` | Grok joins after `/grok` |
| `teamAdded` | team members are added after `/team` |
| `teamAddedGrok` | appended to `teamAdded` on a new line when Grok is in the chat |
| `teamAlreadyInvited` | `/team` is sent again while a team member is in the chat |
| `teamLocked` | `/grok` is sent after a team member wrote in the chat |
| `noTeamMembers` | `/team` is sent with no team members configured |
| `noTeamMembersGrok` | the same, when Grok is enabled |
| `grokInviting` | Grok is being invited |
| `grokUnavailable` | Grok cannot be invited, does not join within 2 minutes, or its first answer fails |
| `grokError` | Grok's answer to a later message fails |
| `grokNoHistory` | Grok joined but could not read earlier messages |

## Environment variables

| Var | Purpose |
|---|---|
| `GROK_API_KEY` | xAI API key; enables Grok replies |
| `SIMPLEX_LIBS_DIR` | directory with a local libsimplex build for the `--db` backend, instead of the downloaded one |
| `SIMPLEX_ADDON_PATH` | locally built Node.js add-on (`simplex.node`), instead of the downloaded one |
| `NODE_OPTIONS` | `--max-old-space-size=8192` on a bot with many chats: single API responses outgrow the default heap |

## Docker

`scripts/support-bot/Dockerfile` builds libsimplex (PostgreSQL backend), the Node.js library and the bot from this checkout. The first build compiles the core and takes hours on a cold cabal cache.

```bash
PG_CONN='postgres://simplex@127.0.0.1/simplex_support_bot' \
  docker compose -f scripts/support-bot/compose.yml up --build
```

The Grok context file is read from `scripts/support-bot/config/grok-context.txt`, and `GROK_API_KEY` from the environment. `scripts/support-bot/config` is mounted at `/etc/support-bot`, so a config file placed there is passed as `--config /etc/support-bot/bot.toml`.

## Dry run

`--dry-run` opens the database, checks what the next start would do, and exits. It does not start chat, so nothing is sent to the network, and pending migrations are reported, not applied.

```bash
npm start -- --dry-run --team-group "Support Team" --db postgres --pg-conn "postgres://user:pass@host/db" \
             -a 3:alice --context-file ./data/grok-context.yaml
```

```
ok    database postgres opened, schema up to date
ok    active user: 1:Ask SimpleX Team
ok    team group 1: Support Team
FAIL  team member 3:alice has display name "bob", the bot would exit
```

A pending migration stops the checks:

```
FAIL  database: 2 migration(s) pending, a real start would apply them: 20261019_chat_layout, 20261021_badge_subscriptions
```

With `--allow-migrations`, the migrations are applied first and the checks run after them. Run it against a restored copy of the database unless the migration is intended, and with the bot stopped.

A line is `ok`, `FAIL` or `?`. `?` marks an id absent from the chat previews: with chat stopped, only chats that were used are listed, so the bot verifies such ids at startup.

The exit code is 0 when no line is `FAIL`, 1 otherwise. The checks cover the database connection and migrations, the persisted team group, Grok user and Grok contact, the `--auto-add-team-members` ids and names, and the `--context-file`.

## Local development against unreleased lib changes

This package depends on `simplex-chat` from npm. To test against an in-tree version:

```bash
# In packages/simplex-chat-nodejs
npm install && npm run build
npm link

# In apps/simplex-support-bot
npm link simplex-chat
export SIMPLEX_ADDON_PATH=../../packages/simplex-chat-nodejs/build/Release/simplex.node
```

`npm unlink simplex-chat && npm install` reverts to the registry version.

## Troubleshooting

- **`--pg-conn is required with --db postgres`** — pass a connection string, or use `--db sqlite`.
- **`libpq5` errors at startup** — install `libpq5` on the host (`apt install libpq5` on Debian/Ubuntu).
- **`ENOENT: no such file or directory, open './data/state.json'`** — the parent directory of `--state-file` must exist; `mkdir -p data` before starting.
- **`JavaScript heap out of memory`** — a response outgrew the default heap; restart with `NODE_OPTIONS=--max-old-space-size=8192`.
