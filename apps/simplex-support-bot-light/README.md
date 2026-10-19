# simplex-support-bot-light

A [SimpleX Chat](https://simplex.chat) bot that adds a roster of people to
incoming business chats.

Anyone who connects to the bot's address gets a business chat with a welcome
message, and every active roster member is added to it. People join the roster
themselves, from a command menu in a separate roster group.

## Docker

From `apps/simplex-support-bot-light`:

```bash
cp bot-config/config.toml.example bot-config/config.toml   # required; edit before starting
printf 'USER_UID=%s\nUSER_GID=%s\n' "$(id -u)" "$(id -g)" > .env   # see Ownership
chmod 0700 state
docker compose up --build -d
docker compose logs -f support-bot-light
```

Use the template in `bot-config/`, whose paths are container paths, not the
top-level one. Place the avatar beside it if `bot.image` is set.

| Path | Mount | Notes |
| --- | --- | --- |
| `./bot-config` | `/etc/support-bot-light` (read-only) | `bot.image` resolves against this directory. |
| `./state` | `/data` | All bot state. `bot.db_prefix` must point here. |

The monitoring endpoint is published on `127.0.0.1:8080`, and the container
config must set `health.host = "0.0.0.0"`, as the template does.

Run detached. Under an attached `docker compose up`, Ctrl+C stops the container
but compose re-attaches it; press Ctrl+C twice or use
`--abort-on-container-exit`.

### State directory

`./state` holds the bot's identity and address. Deleting it produces a new
address and a new roster group, and every roster member must repeat the
handshake. Back it up.

It must be owned by the uid the container runs as, set in `.env`. Both ids
default to 1000; root is not supported. `chmod 0700` it on a shared host, since
the databases hold the bot's identity keys.

## Manual installation

```bash
uv venv && uv pip install -e ../../packages/simplex-chat-python && uv pip install -e '.[dev]'
cp config.toml.example config.toml
uv run support-bot-light --config config.toml
```

The library is installed from this repository, since the APIs the bot uses are
unreleased. `libsimplex` is downloaded on first use unless `SIMPLEX_LIBS_DIR`
points at a local build.

`--config` defaults to `config.toml` in the working directory. `Ctrl+C` stops
the bot; a second `Ctrl+C` exits immediately.

## Configuration

`config.toml.example` is the committed template; `config.toml` is gitignored.

| Key | Required | Description |
| --- | --- | --- |
| `bot.display_name` | yes | Name shown to anyone who connects. |
| `bot.image` | no | Profile image path (`.png`, `.jpg`, `.jpeg`). Relative paths resolve against the directory containing `config.toml`. The encoded image must not exceed 12500 characters, roughly a 128x128 avatar. |
| `bot.db_prefix` | yes | SQLite path prefix. Creates `<prefix>_chat.db` and `<prefix>_agent.db`. Under Docker it must point inside `/data`. |
| `bot.welcome` | yes | Message posted into each new business chat, sent as the address auto-reply. Multi-line TOML strings are supported. |
| `roster.group_name` | yes | Name of the roster group, applied when it is created. |
| `roster.member_role` | no | Role roster members receive in business chats: `observer`, `author`, `member`, `moderator`, `admin` or `owner`. Defaults to `owner`. |
| `health.enabled` | no | Set `false` to switch the monitoring endpoint off. On by default. |
| `health.host` | no | Interface the endpoint binds. Defaults to `127.0.0.1`; `0.0.0.0` under Docker. |
| `health.port` | no | Port for the endpoint. Defaults to `8080`. Setting either key makes a bind failure fatal. |

Changing `bot.welcome` or `bot.image` applies on the next start.

The first start logs two links: the business address, for customers, and the
roster group link, for people who should answer. Anyone who joins the roster
group can add themselves to every incoming chat.

## Monitoring

The bot serves `GET /health` unless `health.enabled` is `false`:

| Status | Meaning |
| --- | --- |
| `200 {"status":"ok"}` | The core answered a query against the roster group. |
| `503 {"status":"unavailable"}` | It returned an error, or did not answer within 5 seconds. |

A bot whose messaging servers are unreachable still answers `200`.

There is no authentication. Bind it to `127.0.0.1`, or to an interface only the
monitoring system can reach. If `health.host` or `health.port` is set and the
address cannot be bound, the bot exits; otherwise a busy default port only logs
a warning.

## Commands

Available in the roster group.

| Command | Effect |
| --- | --- |
| `/dm` | Join the roster. If the bot has no direct contact, it sends a contact request first; membership becomes active once that request is accepted. |
| `/list` | List active members, members who are no longer reachable, and those pending a contact request. |
| `/leave` | Leave the roster. Chats already joined are unaffected. |
| `/help` | Summarise the above. |

Leaving the roster group, or being removed from it, also takes a member off the
roster. The bot is the group's only owner, so removing another member requires a
client signed in as the bot.

## State

All state is in the databases at `bot.db_prefix`. Roster membership is stored in
each contact's `custom_data`, and the roster group is found by a marker in the
group's `custom_data` rather than by name.

Startup reconciles what downtime missed: acceptances that arrived while the bot
was stopped, members who left the roster group, and business chats left without
their roster members.

## Development

```bash
source .venv/bin/activate
ruff check && ruff format --check src tests && pyright && pytest tests/ -v
```

Scope `ruff format` to `src tests`. An unscoped run also reformats Python
fenced inside markdown files.

## Limitations

- Joining the roster never grants access to earlier conversations, including
  chats a returning customer reopens.
- `bot.display_name` cannot be changed to a name any contact, group or past
  customer already holds. The bot logs this and keeps its current name.
- Every active member is added to every incoming chat. There is no routing or
  per-customer selection.
- There is no command to remove someone else from the roster, and `/leave` does
  not remove anyone from chats they have already joined.

## License

[AGPL-3.0](../../LICENSE)
