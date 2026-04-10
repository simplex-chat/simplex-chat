# SimpleX Directory Service

Chat bot for registering and searching groups. Superusers and admins are configured via CLI flags.

---

## Prerequisites

- GHC 9.6.3 — install via [GHCup](https://www.haskell.org/ghcup/)
- Cabal 3.10.2+

---

## Building

```sh
git clone https://github.com/simplex-chat/simplex-chat
cd simplex-chat
cabal build simplex-directory-service
```

The compiled binary is placed at:
```
dist-newstyle/build/<arch>-linux/ghc-<ver>/simplex-chat-<ver>/x/simplex-directory-service/build/simplex-directory-service/simplex-directory-service
```

Or run directly without installing:
```sh
cabal run simplex-directory-service -- <flags>
```

---

## Running the Bot

### Getting your contact ID (bootstrap)

`--super-users` is required and takes `CONTACT_ID:DISPLAY_NAME`. On a fresh database, use `--run-cli` with a placeholder to connect, then find your real ID:

```sh
simplex-directory-service --run-cli --super-users 999:nobody --database /path/to/db
# connect from your phone, then in the bot terminal:
/contacts           # lists connected contacts by name
/i alice            # shows full info for contact "alice", including their contact ID
# note the ID, then restart:
simplex-directory-service --super-users 2:alice --database /path/to/db
```

Both `contactId` and `localDisplayName` must match exactly.

### Minimal run

```sh
simplex-directory-service \
  --super-users 2:alice \
  --database /path/to/db
```

### All flags

| Flag | Default | Description |
|---|---|---|
| `--super-users ID:NAME[,...]` | *(required)* | Super-user contacts (comma-separated) |
| `--admin-users ID:NAME[,...]` | none | Admin-only contacts |
| `--owners-group ID:NAME` | none | Group to invite listed-group owners into |
| `-d / --database PATH` | `~/.simplex/simplex_directory_service` | DB file prefix |
| `--directory-file PATH` | none | Append-only state log |
| `--web-folder PATH` | none | Write static listing JSON + images here |
| `--no-address` | off | Skip creating/checking the bot contact address |
| `--service-name NAME` | `SimpleX Directory` | Bot display name |
| `--run-cli` | off | Run as interactive CLI (useful for bootstrap) |

---

## Hosting the Directory Page

The `web/` folder has a ready-to-use directory page — serve it with any static web server. No build step needed. Dark mode follows system preference. `directory.js` is a symlink to `website/src/js/directory.js` so it stays in sync.

The file must be served under a URL path starting with `/directory` (e.g. `http://example.com/directory.html` or `http://example.com/directory/`) — `directory.js` skips initialisation otherwise.

By default the page shows the official SimpleX directory at `https://directory.simplex.chat/data/`. To use your own bot's data instead, change this line near the bottom of `directory.js`:
```js
const simplexDirectoryDataURL = 'https://your-domain.example/data/';
```

Then run the bot with `--web-folder` pointing to that path. The bot writes `listing.json`, `promoted.json`, and group images there (refreshed every 5 minutes or on approval):
```sh
simplex-directory-service \
  --super-users 2:alice \
  --database /path/to/db \
  --web-folder /var/www/your-domain.example/data
```

---

## Command Reference

This is a reference for all commands accepted by the SimpleX Directory bot. For a guided walkthrough of group submission see [DIRECTORY.md](../../docs/DIRECTORY.md).

---

### 1. Searching

The bot sends a welcome message automatically when you connect.

| Action | Syntax | Effect |
|---|---|---|
| Search | `<text>` | Returns up to 10 groups whose name or welcome message matches; sorted by member count |
| Next page | `/next` or `.` | Next page of the most recent search |
| Recent groups | `/new` | Groups added most recently |
| All groups | `/all` | All listed groups, sorted by member count |
| Help | `/help [registration\|r\|commands\|c]` | Show registration or commands help (default: registration) |

---

### 2. Registering a Group

Registration is a three-step process — see [DIRECTORY.md](../../docs/DIRECTORY.md) for full details:

1. Invite the directory bot to your group as `admin`.
2. Add the link the bot sends you to the group's welcome message.
3. Wait for admin approval (usually within 24 hours).

If a group with the same display name is already registered (but not yet listed or suspended), the bot asks you to confirm with `/confirm`. If the name is already listed or suspended in the directory, registration is blocked.

---

### 3. Managing Your Groups (user commands)

> **Note on `<ID>` vs `<ID>:<name>`:** Commands shown with `<ID>[:<name>]` (`/role`, `/filter`, `/link`) accept just the ID — the name is optional. All other group commands (`/confirm`, `/delete`, and all admin/super-user commands) require both ID and name as `<ID>:<name>`. The bot provides the exact `ID:name` string in its messages so you can copy it directly.

#### List groups

```
/list
/ls
```

Shows all groups you have registered with their current status.

#### Confirm duplicate name

```
/confirm <ID>:<name>
```

When you invite the bot to a group whose display name matches an already-registered group, the bot pauses registration and asks for explicit confirmation. This prevents accidental registration of another group that shares a name. `/confirm` acknowledges the duplicate and proceeds with registration. `ID` and `name` are provided in the bot's prompt.

If a group with the same name is already *listed* or *suspended* in the directory, registration is blocked entirely and `/confirm` is not offered.

#### View or set default join role

```
/role <ID>[:<name>] [member|observer]
```

Omit the role argument to view the current setting. `member` (default) lets new joiners post immediately; `observer` makes them read-only until promoted.

#### View or configure anti-spam filter

```
/filter <ID>[:<name>] [preset | flags]
```

Omit the argument to view the current filter.

**Presets** (mutually exclusive):

| Preset | Effect |
|---|---|
| `off` | No filter |
| `basic` | Reject joins from profiles without images that have no name set |
| `moderate` (or `mod`) | Reject all no-name profiles; require captcha for profiles without images |
| `strong` | Reject all no-name profiles; require captcha for all profiles |

**Flags** (combine freely):

```
/filter <ID>[:<name>] [name[=all|=noimage]] [captcha[=all|=noimage]] [observer[=all|=noimage]]
```

| Flag | Condition | Effect |
|---|---|---|
| `name` | `=all` (default) or `=noimage` | Reject joins from profiles with no name set |
| `captcha` | `=all` (default) or `=noimage` | Require captcha to join |
| `observer` | `=all` (default) or `=noimage` | Make new members observers instead of members |

`=noimage` means the condition applies only to profiles that have no profile image.

#### View or upgrade group link

```
/link <ID>[:<name>]
```

Shows the current directory-managed join link. If the link is outdated the bot upgrades it.

#### Remove group from directory

```
/delete <ID>:<name>
```

Permanently removes the group from the directory. The group can be re-registered later.

---

### 4. Admin Commands

Admins receive a notification whenever a group enters the PendingApproval state. The `<approval-id>` in `/approve` is the integer from that notification (or from `/pending`); it increments each time a group re-enters the PendingApproval state so that stale approvals are rejected.

| Command | Syntax | Effect |
|---|---|---|
| Approve | `/approve <ID>:<name> <approval-id> [promote=on\|off]` | List group in directory; notifies owner |
| Suspend | `/suspend <ID>:<name>` | Hide group from directory; notifies owner |
| Resume | `/resume <ID>:<name>` | Re-list a suspended group; notifies owner |
| List recent | `/last [N]` | Show last N registered groups (default: 10) |
| List pending | `/pending [N]` | Show N groups awaiting approval (default: 10) |
| Message owner | `/owner <ID>:<name> <message>` | Forward a message to the group owner |
| Reject | `/reject <ID>:<name>` | *(Reserved, currently a no-op)* |
| Invite owner | `/invite <ID>:<name>` | Invite the group owner to the owners' group |

---

### 5. Super-User Commands

| Command | Syntax | Effect |
|---|---|---|
| Feature group | `/promote <ID>:<name> on\|off` | Add or remove group from promoted listing |
| Execute API command | `/exec <command>` or `/x <command>` | Run a raw SimpleX Chat API command |

---

### 6. Group Lifecycle

```
Invited by owner
      │
      ├── (unique name) ──────────────────────────┐
      │                                           │
      └── (duplicate name*) ─▶ PendingConfirmation│
                                    │             │
                               (/confirm)         │
                                    │             │
                                    ├─────────────┘
                                    ▼
                                  Proposed
                                    │
                                    ▼
                              PendingUpdate
                         (add bot link to welcome)
                                    │
                                    ▼
                             PendingApproval
                             (admin reviews)
                                    │
                        ┌───────────┴───────────┐
                        ▼                       ▼
                     Active              (profile change**
                    (listed)            → re-enters pending)
                 ┌────┴─────┐
                 ▼          ▼
            Suspended  SuspendedBadRoles
           (by admin)  (roles changed)
                 │          │
                 └────┬─────┘
                      ▼
                    Removed
```

\* Only when the duplicate is registered but not yet listed or suspended.
If the name is already listed or suspended, registration is blocked entirely.

\*\* Profile changes only trigger re-approval when fields other than the
directory bot link are modified. If the only change is swapping the old bot
link for the new one, or changing only whitespace in the description, the
group stays Active.

**State notes:**

- **PendingConfirmation** — bot has been invited but a group with the same display name is already registered (in a pending state). Owner must run `/confirm` to proceed. If a group with the same name is already listed or suspended, registration is blocked entirely.
- **Proposed** — name is unique (or duplicate confirmed via `/confirm`); bot is joining the group.
- **PendingUpdate** — bot has joined the group and created the group link; owner must add it to the group's welcome message.
- **PendingApproval** — submitted for review. The `approval-id` shown to admins increments each time the group re-enters this state (e.g. after a profile change). An Active group re-enters this state when its profile is modified, unless the only change is swapping the old directory bot link for the new one or changing only whitespace in the description, in which case the group stays Active.
- **Active** — listed in directory; visible in search results.
- **Suspended** — hidden from directory by admin action; owner is notified.
- **SuspendedBadRoles** — hidden automatically when the directory bot loses its `admin` role or the registering owner loses their `owner` role in the group.
- **Removed** — delisted permanently (by owner via `/delete`, or after removal from the group).
