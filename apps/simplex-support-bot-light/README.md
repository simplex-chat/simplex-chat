# simplex-support-bot-light

A [SimpleX Chat](https://simplex.chat) bot that adds a roster of people to
incoming business chats.

Anyone who connects to the bot's address gets a business chat with a welcome
message, and every active roster member is added to it. The roster is
self-service: people join it from a command menu in a separate roster group.

## Docker

The supported way to run the bot. From `apps/simplex-support-bot-light`:

```bash
cp bot-config/config.toml.example bot-config/config.toml   # required; edit it before starting
sudo chown -R 1000:1000 state && chmod 0700 state          # see Ownership below
docker compose up --build -d
docker compose logs -f support-bot-light
```

Without `bot-config/config.toml` the bot exits with `config file not found` and
Docker retries it five times before giving up.

Run it detached, as above. Under an attached `docker compose up`, Ctrl+C stops
the container but compose does not return: the restart policy brings it back and
compose re-attaches. That is a property of the policy, not of the bot, which
stops on SIGINT and SIGTERM in under half a second. To run attached, either
press Ctrl+C twice or use `docker compose up --abort-on-container-exit`.

Edit `bot-config/config.toml` before starting, and place the avatar beside it
if `bot.image` is set. See [Configuration](#configuration). Use the template in
`bot-config/`, not the top-level one: its paths are container paths.

| Path | Mount | Notes |
| --- | --- | --- |
| `./bot-config` | `/etc/support-bot-light` (read-only) | Mounted as a directory so `bot.image` resolves beside the config. |
| `./state` | `/data` | All bot state. `bot.db_prefix` must point here. |

`docker compose` publishes the monitoring endpoint on `127.0.0.1:8080`. The
container config must set `health.host = "0.0.0.0"`, as the template does: a
published port forwards to the container's address, so an endpoint bound to the
container's loopback answers nothing. See [Monitoring](#monitoring).

`./state` holds the bot's identity, published address, roster group and roster.
Deleting it produces a new address and a new roster group, and every roster
member must repeat the handshake. Back it up.

### Ownership

The container runs as uid 1000, and Docker does not adjust the ownership or
mode of a bind-mount source, so `./state` must be owned by uid 1000 or the bot
cannot create its databases. On a shared host, restrict it as well: the
databases hold the bot's identity keys.

```bash
sudo chown -R 1000:1000 state
chmod 0700 state
```

`./state` must exist before the first `docker compose up`, because Docker
creates a missing bind-mount source as root. It is tracked as an empty
directory for that reason.

## Manual installation

```bash
uv venv && uv pip install -e '.[dev]'
cp config.toml.example config.toml
uv run support-bot-light --config config.toml
```

`--config` defaults to `config.toml` in the working directory. `Ctrl+C` stops
the bot; a second `Ctrl+C` exits immediately.

## Configuration

`config.toml.example` is the committed template; `config.toml` is gitignored.

| Key | Required | Description |
| --- | --- | --- |
| `bot.display_name` | yes | Name shown to anyone who connects. |
| `bot.image` | no | Profile image path (`.png`, `.jpg`, `.jpeg`). Relative paths resolve against the directory containing `config.toml`. The encoded image must not exceed 12500 characters, which limits it to roughly a 128x128 avatar. |
| `bot.db_prefix` | yes | SQLite path prefix. Creates `<prefix>_chat.db` and `<prefix>_agent.db`. Under Docker it must point inside `/data`. |
| `bot.welcome` | yes | Message posted into each new business chat, sent as the address auto-reply. |
| `roster.group_name` | yes | Name of the roster group, applied when it is created. |
| `roster.member_role` | no | Role roster members receive in business chats: `observer`, `author`, `member`, `moderator`, `admin` or `owner`. Defaults to `owner`. `relay` is also accepted by the core but is an infrastructure role, not one for a person. |
| `health.enabled` | no | Set `false` to switch the monitoring endpoint off. On by default. |
| `health.host` | no | Interface the endpoint binds. Defaults to `127.0.0.1`; `0.0.0.0` under Docker. |
| `health.port` | no | Port for the endpoint. Defaults to `8080`. Setting either key makes a bind failure fatal. See [Monitoring](#monitoring). |

The first start logs two links:

| Link | Give to |
| --- | --- |
| Business address | Customers. |
| Roster group link | People who should answer. |

The roster group link is the access-control boundary. Anyone who joins that
group can add themselves to every incoming chat.

## Monitoring

The bot serves `GET /health` unless `health.enabled` is `false`:

| Status | Meaning |
| --- | --- |
| `200 {"status":"ok"}` | The core answered a query against the roster group. |
| `503 {"status":"unavailable"}` | It returned an error, or did not answer within 5 seconds. |

The check queries the database rather than reporting that the process is
running: the query waits on the same store lock every other operation takes, so
a wedged transaction shows up as unhealthy. On a roster-sized group it costs
well under a millisecond.

One query is outstanding at a time no matter how often the endpoint is polled.
This matters more than it sounds: the timeout bounds the wait, not the work, and
each abandoned query would hold a worker thread that the bot's own receive loop
needs — so polling a stalled core would otherwise stop the bot from receiving
anything.

It does not cover the network: a bot whose messaging servers are unreachable
still answers `200`.

There is no authentication. Bind it to `127.0.0.1`, or to an interface only the
monitoring system can reach.

An address named in the config that cannot be bound stops the bot: monitoring
that silently failed to listen reads as health. Leaving `[health]` out entirely
means neither was chosen, so a busy default port only logs a warning.

Under Docker the endpoint binds `0.0.0.0` and `docker-compose.yml` publishes it
on `127.0.0.1:8080`. Widen that mapping only for a monitoring system that has to
reach it from elsewhere. No container `healthcheck` is configured: a chat
controller that is briefly slow is not a reason to restart the bot.

## Commands

Available in the roster group.

| Command | Effect |
| --- | --- |
| `/dm` | Join the roster. If the bot has no direct contact, it sends a contact request first; membership becomes active once that request is accepted. |
| `/list` | List active members, members who are no longer reachable, and those pending a contact request. |
| `/leave` | Leave the roster. Chats already joined are unaffected. |
| `/help` | Summarise the above. |

Leaving the roster group also takes a member off the roster, and so does being
removed from it. Note that the bot is the group's only owner, so members cannot
remove each other: revoking someone who will not leave voluntarily means
removing them from the group with a client signed in as the bot.

## State

The bot keeps no state file. Roster membership is stored in each contact's
`custom_data`, and the roster group is identified by a marker in the group's
`custom_data`. Restarts resume from the databases at `bot.db_prefix`, and
renaming the roster group in a client does not affect discovery.

Every step is idempotent, and startup reconciles what a crash or downtime could
have missed: acceptances that arrived while the bot was stopped, members who
left the roster group in the meantime, and business chats that were left without
their roster members. Re-running any of it is safe.

## Development

```bash
source .venv/bin/activate
ruff check && ruff format --check src tests && pyright && pytest tests/ -v
```

Scope `ruff format` to `src tests`: an unscoped run also reformats Python
fenced inside markdown files.

## Limitations

- Reconciliation runs only at startup, so a business chat can briefly lack its
  roster members until the next restart if the bot crashes mid-setup. Chats
  whose roster pass already finished are never revisited, so joining the roster
  never grants access to earlier conversations.
- Changing `bot.display_name` after the first start is not always possible. The core keeps every
  display name unique across contacts, groups and members, so a name a past customer or a roster
  member already holds is refused. The bot logs that and keeps serving under the name it has.
- There is no command to remove someone else from the roster; see above.
- Reconnections from a known customer emit `businessRequestAlreadyAccepted`
  rather than `acceptingBusinessRequest` and are ignored, so members who joined
  the roster after that chat was created are not added to it.
- Every active member is added to every incoming chat. There is no routing or
  per-customer selection.
- `/leave` does not remove anyone from chats they have already joined.

## License

[AGPL-3.0](../../LICENSE)
