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
# OpenSSL build configuration (required) — copy the file for your OS:
cp scripts/cabal.project.local.linux cabal.project.local   # on macOS: scripts/cabal.project.local.mac
cabal build simplex-directory-service
```

See [docs/CONTRIBUTING.md](../../docs/CONTRIBUTING.md) for the full build setup (toolchain, OpenSSL headers, branch compatibility).

Find the compiled binary with:
```sh
cabal list-bin simplex-directory-service
```

Or run directly without installing:
```sh
cabal run simplex-directory-service -- <flags>
```

---

## Running the Bot

### Getting your contact ID (bootstrap)

`--super-users` is required and takes one or more `CONTACT_ID:DISPLAY_NAME` pairs. On a fresh database you don't yet know your contact ID, so start the bot with `--run-cli` and a placeholder, connect to it, then look up your real ID:

```sh
simplex-directory-service --run-cli --super-users 999:nobody --database /path/to/db
# on startup the bot prints (and, unless --no-address, creates) its contact address:
#   "Bot's contact address is: simplex:/contact#..."
# connect to it from your SimpleX Chat app, then in the bot terminal:
/contacts           # lists connected contacts by name
/i alice            # shows full info for contact "alice", including their contact ID
# note the ID, then restart without --run-cli:
simplex-directory-service --super-users 2:alice --database /path/to/db
```

In each `--super-users` / `--admin-users` pair both `contactId` and `localDisplayName` must match the contact exactly. (A mismatched name doesn't stop the bot from sending admin notifications to that contact ID, but commands from that contact will be rejected.)

### Minimal run

```sh
simplex-directory-service \
  --super-users 2:alice \
  --database /path/to/db
```

### Flags

`simplex-directory-service` also accepts the standard SimpleX Chat core options (custom SMP/XFTP servers, `--socks-proxy`, network settings, etc.) — run `simplex-directory-service --help` for the complete list. The directory-specific options are:

| Flag | Default | Description |
|---|---|---|
| `--super-users ID:NAME[,...]` | *(required)* | Super-user contacts (comma-separated) |
| `--admin-users ID:NAME[,...]` | none | Admin-only contacts (comma-separated) |
| `--owners-group ID:NAME` | none | Group (by group ID) that owners of listed groups are invited into — automatically on listing, or via `/invite` |
| `-d / --database PATH` | `~/.simplex/simplex_directory_service` | Database file path prefix |
| `--directory-file PATH` | none | Append-only log of directory state (see [Directory state log](#directory-state-log)) |
| `--migrate-directory-file check\|import\|export\|listing` | — | Check, import (log → DB), export (DB → log), or regenerate listing files, then exit |
| `--web-folder PATH` | none | Write static listing JSON + group images here (see [Hosting the directory page](#hosting-the-directory-page)) |
| `--no-address` | off | Skip checking/creating the bot's contact address |
| `--service-name NAME` | `SimpleX Directory` | Bot display name (without `*` characters) |
| `--profile-name-limit N` | unlimited | Max display-name length allowed to connect / join groups (used by the `name` join filter) |
| `--blocked-words-file PATH` | none | Words not allowed in profiles (used by the `name` join filter and profile review) |
| `--blocked-fragments-file PATH` | none | Word fragments not allowed in profiles |
| `--blocked-extenstion-rules PATH` | none | Substitution rules that expand the blocked-words list (the flag is spelled this way in the binary) |
| `--name-spelling-file PATH` | none | Character-substitution rules for matching disguised names |
| `--captcha-generator PATH` | none | Executable that renders a captcha image; without it captchas are sent as plain text |
| `--voice-captcha-generator PATH` | none | Executable that renders a voice captcha |
| `--run-cli` | off | Run an interactive CLI alongside the bot (useful for bootstrap) |
| `-v / --version` | — | Print version and exit |

---

## Directory State Log

`--directory-file` keeps an append-only log of every change to directory state (registrations, status changes, promotions) alongside the SQLite database. It is optional but recommended for operability: it is human-inspectable and can be checked, exported, or re-imported with `--migrate-directory-file`. Without it, directory state lives only in the database.

---

## Hosting the Directory Page

The `web/` folder contains a ready-to-use page (`directory.html`) that renders a bot's directory as a searchable, paginated list — no build step. Dark mode follows the system preference.

A few things to know before deploying it:

- **Copy the files out of the repo.** `web/directory.js` is a symlink to `website/src/js/directory.js` (kept in sync with the main website), so don't edit it in place — copy `directory.html` and the *contents* of `directory.js` to your web root and edit the copy. The data URL can't be overridden from `directory.html` because `directory.js` declares it as a top-level `const`.
- **Serve it under a `/directory` path.** `directory.js` only initialises when the page path starts with `/directory` (e.g. `https://example.com/directory.html` or `https://example.com/directory/`).
- **Provide the fallback image.** Groups without a profile image (and any image that fails to load) fall back to `/img/group.svg`, resolved from the site root — copy `website/src/img/group.svg` to `<web-root>/img/group.svg`.
- **Point it at your bot's data.** Near the bottom of `directory.js`, change:
  ```js
  const simplexDirectoryDataURL = 'https://your-domain.example/data/';
  ```
  (the default is the official directory, `https://directory.simplex.chat/data/`). The page fetches `listing.json` from that URL and loads group images relative to it.
- **Optional:** the "Also available as a SimpleX chat bot" link in `directory.html` points at the official directory bot — update it to your bot's address.

Then run the bot with `--web-folder` pointing at the folder served at that URL. The bot writes `listing.json` (used by the page), `promoted.json` (the promoted subset — written but not rendered by the bundled page), and group images, refreshing every 5 minutes and immediately when a group is approved or its listing/promotion status changes:
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
| Next page | `/next` or `.` | Next page of the most recent search (after ~5 min of inactivity, or with no recent search, falls back to listing all groups) |
| Recent groups | `/new` | Groups added most recently |
| All groups | `/all` | All listed groups, sorted by member count |
| Help | `/help [registration\|r\|commands\|c]`, or `/h` | Show registration or commands help (default: registration) |

---

### 2. Registering a Group

Registration is a three-step process — see [DIRECTORY.md](../../docs/DIRECTORY.md) for full details:

1. Invite the directory bot to your group as `admin`.
2. Add the link the bot sends you to the group's welcome message.
3. Wait for admin approval (usually within a day, except holidays).

If a group with the same display name is already registered (but not yet listed or suspended), the bot asks you to confirm with `/confirm`. If the name is already listed or suspended in the directory, registration is blocked.

---

### 3. Managing Your Groups (user commands)

> **Note on `<ID>` vs `<ID>:<name>`:** Commands shown with `<ID>[:<name>]` (`/role`, `/filter`, `/link`) accept just the ID — the name is optional. `/confirm`, `/delete`, and all admin/super-user commands require both, written as `<ID>:<name>`. For user commands the ID is the registration ID shown by `/list`; for admin and super-user commands it's the group ID included in the bot's admin notifications. When the bot expects an `<ID>:<name>` argument it normally quotes the whole command for you (e.g. for `/confirm` and `/approve`), so you can copy it directly; for `/delete` you build it from the ID and name shown by `/list`.

#### List groups

```
/list
/ls
```

Shows all groups you have registered, with their current status, member count, and the `/role` and `/filter` commands for each.

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

Omit the argument to view the current filter. Filters apply to people joining via the directory-managed link.

**Presets** (mutually exclusive):

| Preset | Effect |
|---|---|
| `off` | No filter |
| `basic` | For profiles without an image: reject long or inappropriate names |
| `moderate` (or `mod`) | Reject long/inappropriate names from all profiles; require captcha from profiles without an image |
| `strong` | Reject long/inappropriate names from all profiles; require captcha from all profiles |

**Flags** (combine freely):

```
/filter <ID>[:<name>] [name[=all|=noimage]] [captcha[=all|=noimage]] [observer[=all|=noimage]]
```

| Flag | Condition | Effect |
|---|---|---|
| `name` | `=all` (default) or `=noimage` | Reject joins from profiles whose name is too long (`--profile-name-limit`) or contains blocked words/fragments (`--blocked-words-file` / `--blocked-fragments-file`) |
| `captcha` | `=all` (default) or `=noimage` | Require the joiner to solve a captcha |
| `observer` | `=all` (default) or `=noimage` | Make new members observers instead of members |

`=noimage` means the condition applies only to profiles that have no profile image (`=no_image` and `=no-image` are also accepted).

> The `name` filter only has an effect when the bot was started with `--profile-name-limit` and/or `--blocked-words-file` / `--blocked-fragments-file`; otherwise it does nothing. The `captcha` filter sends a much stronger challenge when `--captcha-generator` (or `--voice-captcha-generator`) is configured — without it the captcha text is sent as a plain message.

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
| Invite owner | `/invite <ID>:<name>` | Invite the group owner to the owners' group (requires `--owners-group`) |

---

### 5. Super-User Commands

| Command | Syntax | Effect |
|---|---|---|
| Feature group | `/promote <ID>:<name> on\|off` | Add or remove group from the promoted listing (`promoted.json`) |
| Execute API command | `/exec <command>` or `/x <command>` | Run a raw SimpleX Chat API command |

---

### 6. Group Lifecycle

Forward path, from invitation to being listed:

```
                       Invited by owner
                              │
                 ┌────────────┴────────────┐
           unique name                duplicate name*
                 │                          │
                 │                          ▼
                 │                  PendingConfirmation
                 │                          │  owner runs /confirm
                 └────────────┬─────────────┘
                              ▼
                          Proposed
                              │  bot joins the group and creates the link
                              ▼
                        PendingUpdate
                              │  owner adds the link to the group welcome
                              ▼
                       PendingApproval
                              │  admin runs /approve
                              ▼
                            Active            (listed; visible in search)
```

**Transitions out of Active:**

- → **PendingUpdate** — the directory bot link is removed from the welcome message.
- → **PendingApproval** — most other profile changes (see ** below); the `approval-id` shown to admins is bumped each time, so stale `/approve` commands are rejected.
- → **Suspended** — an admin runs `/suspend`; `/resume` re-lists the group.
- → **SuspendedBadRoles** — the directory bot loses its `admin` role, or the registering owner loses their `owner` role, in the group; automatically restored to **Active** once the roles are corrected.
- → **Removed** — the owner runs `/delete`, the owner is removed from or leaves the group, the bot is removed from the group, or the group is deleted. The group can be re-registered afterwards.

\* Only when the duplicate is registered but not yet listed or suspended. If the name is already listed or suspended, registration is blocked entirely.

\*\* Profile changes only trigger re-approval when fields other than the directory bot link are modified. If the only change is swapping the old bot link for the new one, or changing only whitespace in the description, the group stays Active.

**State notes:**

- **PendingConfirmation** — the bot was invited but a group with the same display name is already registered (in a pending state); the owner must run `/confirm` to proceed.
- **Proposed** — the name is unique (or the duplicate was confirmed via `/confirm`); the bot is joining the group.
- **PendingUpdate** — the bot has joined the group and created the join link; the owner must add it to the group's welcome message.
- **PendingApproval** — submitted for admin review. The join link works even before approval.
- **Active** — listed in the directory and visible in search results.
