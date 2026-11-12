# SimpleX Support Bot

A business-address bot that triages incoming support chats, optionally runs them through Grok, and routes handoffs to a team group.

## Prerequisites

- Node.js v18 or newer (v24 tested)
- an xAI API key in `GROK_API_KEY` or in `grok.api_key` of the config file — optional; the bot runs without it
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
| `--broadcasters` | both | no | | comma-separated `ID:name` pairs of contacts allowed to use `/feed` in the team group |
| `--timezone` | both | no | `UTC` | IANA zone for weekend detection |
| `--complete-hours` | both | no | `3` | auto-complete chats after N hours idle (`0` disables) |
| `--card-flush-seconds` | both | no | `300` | debounce card state writes |
| `--queue-size` | both | no | core default, `1024` | internal queue size of the chat core, as the CLI's `-q`/`--queue-size` |
| `--context-file` | both | required with a Grok API key | | text file with Grok system context |
| `--dry-run` | both | no | | check config, database and state, then exit |
| `--allow-migrations` | both | no | | with `--dry-run`: apply pending migrations instead of reporting them |
| `--allow-down-migrations` | both | no | | with `--dry-run`: roll back migrations that this build does not have |
| `-h` / `--help` | both | no | | show usage and exit |

## Config file

`--config config.toml` reads option values and message texts from a TOML file, as [config.toml.example](../../scripts/support-bot/config/config.toml.example) shows. A list is joined with commas. An option given on the command line takes precedence over the file. `--dry-run`, `--allow-migrations` and `--allow-down-migrations` are command-line only. `grok.api_key` is file-only, so that the key is not visible in the process list; `GROK_API_KEY` takes precedence over it. The file holds the `pg_conn` password and the Grok key, so keep it readable only by the user that runs the bot.

| Key | Option |
|---|---|
| `bot.team_group` | `--team-group` |
| `bot.auto_add_team_members` | `--auto-add-team-members` |
| `bot.broadcasters` | `--broadcasters` |
| `bot.state_file` | `--state-file` |
| `bot.timezone` | `--timezone` |
| `bot.complete_hours` | `--complete-hours` |
| `bot.card_flush_seconds` | `--card-flush-seconds` |
| `bot.queue_size` | `--queue-size` |
| `database.type` | `--db` |
| `database.pg_conn` | `--pg-conn` |
| `database.pg_schema` | `--pg-schema` |
| `database.sqlite_file_prefix` | `--sqlite-file-prefix` |
| `database.sqlite_key` | `--sqlite-key` |
| `grok.context_file` | `--context-file` |
| `grok.api_key` | `GROK_API_KEY` |

A message left out of `[messages]` keeps its built-in text. `{hours}` is replaced with 24 on weekdays and 48 at weekends in `bot.timezone`. The welcome message is updated in the bot address on the next start.

| Message | Sent when |
|---|---|
| `welcome` | a customer connects to the business address |
| `queue` | a customer's first message is queued for the team, and after `grok_unavailable` when `/grok` was the first message |
| `queue_grok` | appended to `queue` after an empty line when Grok is enabled |
| `grok_activated` | Grok joins after `/grok` |
| `team_added` | team members are added after `/team` |
| `team_added_grok` | appended to `team_added` on a new line when Grok is in the chat |
| `team_already_invited` | `/team` is sent again while a team member is in the chat |
| `team_locked` | `/grok` is sent after a team member wrote in the chat |
| `no_team_members` | `/team` is sent with no team members configured |
| `no_team_members_grok` | the same, when Grok is enabled |
| `grok_inviting` | Grok is being invited |
| `grok_unavailable` | Grok cannot be invited, does not join within 2 minutes, or its first answer fails |
| `grok_error` | Grok's answer to a later message fails |
| `grok_no_history` | Grok joined but could not read earlier messages |

## Environment variables

| Var | Purpose |
|---|---|
| `GROK_API_KEY` | xAI API key; enables Grok replies; takes precedence over `grok.api_key` of the config file |
| `SIMPLEX_LIBS_DIR` | directory with a local libsimplex build for the `--db` backend, instead of the downloaded one |
| `SIMPLEX_ADDON_PATH` | locally built Node.js add-on (`simplex.node`), instead of the downloaded one |
| `NODE_OPTIONS` | `--max-old-space-size=8192` on a bot with many chats: single API responses outgrow the default heap |

## Docker

`scripts/support-bot/Dockerfile` builds the bot on the PostgreSQL backend with one of two library sources, selected by `LIBS`:

| `LIBS` | libsimplex, Node.js add-on and npm packages | Build time |
|---|---|---|
| `release` (default) | the versions pinned by package-lock.json, downloaded and checked by SHA-256 at build time | minutes |
| `source` | built from this checkout | hours on a cold cabal cache |

```bash
cd scripts/support-bot
cp config/config.toml.example config/config.toml
docker compose up --build
LIBS=source docker compose build
```

`scripts/support-bot/config` is mounted at `/etc/support-bot` and holds `config.toml`, with `database.pg_conn` and `grok.api_key`, and the Grok context file; the bot runs as uid 1000, which must be able to read them. Other arguments are passed after the service name, for example `docker compose run --rm support-bot --config /etc/support-bot/config.toml --dry-run`.

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
FAIL  database: pending 2 migration(s): 20261019_chat_layout, 20261021_badge_subscriptions; a real start would apply them, --allow-migrations applies them now
```

With `--allow-migrations`, the migrations are applied first and the checks run after them. Run it against a restored copy of the database unless the migration is intended, and with the bot stopped.

A database migrated by a newer build also stops the checks, since the bot refuses to start on it:

```
FAIL  database: database is newer than this build, to roll back 1 migration(s): 20260905_feeds; a real start would refuse, --allow-down-migrations rolls them back
```

With `--allow-down-migrations`, those migrations are rolled back with the down SQL that the newer build stored in the database, and the checks run after it. The core rolls back only when this build's migrations are a prefix of the database's; a database whose migrations diverge from this build's is reported as a migration state error and is not changed. Rolling back deletes the data of the removed migrations, so take a backup first.

A line is `ok`, `FAIL` or `?`. `?` marks an id absent from the chat previews: with chat stopped, only chats that were used are listed, so the bot verifies such ids at startup.

The exit code is 0 when no line is `FAIL`, 1 otherwise. The checks cover the database connection and migrations, the persisted team group, Grok user and Grok contact, the `--auto-add-team-members` and `--broadcasters` ids and names, and the `--context-file`.

## Feed

A contact listed in `bot.broadcasters` sends `/feed <text>` in the team group. The bot sends the text as a feed message to every customer chat and every direct contact of the bot, replies that it is being sent, and posts again when it is sent to all chats or fails. The text may span several lines. `/feed` from another contact gets an error reply with that contact's id, which the operator adds to `bot.broadcasters`.

A broadcaster must be a contact of the bot: the bot sends every team group member the contact id it knows them by. The bot exits at start if an id is not a contact or its display name differs, as for `bot.auto_add_team_members`.

Feeds are part of the core, and no released libsimplex has them yet: build the Docker image with `LIBS=source`. The default `LIBS=release` build fails to compile the bot, because the published `@simplex-chat/types` has no feed chats.

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
