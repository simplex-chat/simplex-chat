# SimpleX MCP server: reference

[README](README.md) has the short version.

`@simplex-chat/mcp` is a Claude Code plugin that gives each Claude Code session its own presence on the SimpleX network. The agent connects to people, bots, groups, channels and directories, as itself or incognito, sends messages and receives all the responses. Bots tell it which commands they understand, so every bot on the network becomes a service the agent can use. You supervise all sessions from one SimpleX group, the Main group: each session asks there before it connects, sends or leaves, and you approve or reject from your phone with one tap, or approve all its messages to a chat you trust. Five tools cover it: `connect`, `send`, `receive`, `list`, `leave`. Setup is two commands in Claude Code and one scan of a QR code.

What it isn't:
- **Not a directory or a search service.** The agent reaches chats through links, SimpleX names and the directories it connects to.
- **Not a push channel into the agent.** An MCP server can't start a turn of the agent, so news reaches it only as results of calls it makes: waits inside `send` and `receive`, or polling.
- **Not an always-on service.** The agent acts while its session runs; services that must always be up should be bots.
- **Not hosted.** It runs on your machine: one daemon for all sessions, one SimpleX profile per session.
- **Not in v0.2:**
  - an address for the agent: nobody can contact it first;
  - your approval of incoming messages;
  - files;
  - a Claude Code channel;
  - other MCP hosts.

Status: v0.2. [`TESTING.md`](TESTING.md) describes a local build and test. The design record is [`PLAN.md`](PLAN.md); the specification of the code is in [`spec/`](spec/README.md).

- [Requirements](#requirements)
- [Install](#install)
- [Options](#options)
- [Sessions and profiles](#sessions-and-profiles)
- [The Main group and its owners](#the-main-group-and-its-owners)
- [What the owner sees](#what-the-owner-sees)
- [Approval modes](#approval-modes)
- [Expiry](#expiry)
- [What the agent sees](#what-the-agent-sees)
- [Waiting for approval in Claude Code](#waiting-for-approval-in-claude-code)
- [Files](#files)
- [Recovery](#recovery)
- [Command reference](#command-reference)
- [Security](#security)

## Requirements

- Node.js 22 or later, with `npx` on `PATH`.
- Claude Code with plugin support.
- One of the platforms of the `simplex-chat` package: Linux x86-64 or arm64, macOS x86-64 or arm64, Windows x86-64.
- On Linux x86-64: glibc 2.34 or later, and the system libraries `libcrypto.so.3`, `libgmp.so.10` and `libz.so.1`.
- The SimpleX Chat app on a phone or computer, for the owner.

At its first start the daemon downloads the SimpleX core library, 29 to 50 MB depending on the platform, into `~/.cache/simplex-chat` (macOS: `~/Library/Caches/simplex-chat`, Windows: `%LOCALAPPDATA%\simplex-chat`). Unpacked on Linux, it takes about 240 MB.

## Install

In Claude Code:

```
/plugin marketplace add https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/.claude-plugin/marketplace.json
/plugin install simplex@simplex-chat
```

The marketplace `simplex-chat` lists one plugin, `simplex`, from the npm package `@simplex-chat/mcp`. The plugin adds:

| Part | What it does |
|---|---|
| MCP server `simplex` | runs `npx -y @simplex-chat/mcp@0.2.0 session` for each session; its tools are `mcp__plugin_simplex_simplex__connect` and the rest |
| SessionStart hook | runs `simplex-mcp hook session-start`: checks the Node.js version, starts the daemon, records the session title, and shows the Main group link while the group has no owner |
| PreToolUse hook | allows the plugin's tool calls without a Claude Code prompt, because you approve in the SimpleX app |
| `/simplex:link` | shows the Main group link, or its owners |
| `/simplex:reset-owners` | replaces the Main group and its owners |

The plugin's commands run the published package version through `npx`, so the first session start also fills the npx cache. Claude Code waits at most 600 seconds for the SessionStart hook.

The server entry in the plugin's `.mcp.json` sets `timeout` to 169800000 ms, 47 hours 10 minutes: the longest expiry of 23 hours, 24 hours of waiting for responses, and a 10-minute margin. Claude Code ends a tool call of this server only after that time ([Claude Code MCP docs](https://code.claude.com/docs/en/mcp.md), "Time limits on MCP tool calls").

At the first session start, Claude Code shows:

```
SimpleX MCP: scan this QR code with the SimpleX app and tap Join to supervise your agents in "SimpleX MCP":
<QR code>
https://smp4.simplex.im/g#...
```

Scan the QR code or paste the link into the app, and tap Join. The group "SimpleX MCP" appears, and its profile writes: `I'm your agents' Main group. Your Claude Code sessions ask here before they connect or send.` Claude also gets the link in its context, so it can repeat it to you.

To see the link again, run `/simplex:link`. Its output in Claude Code has no QR code; `npx -y @simplex-chat/mcp link` in a terminal prints the link with a QR code to scan.

## Options

Set in `/plugin` > Installed > simplex > Configure options, or in `/config`:

| Option | Default | Values | Effect |
|---|---|---|---|
| `main_group` | `SimpleX MCP` | a valid SimpleX display name | name of the Main group and of its profile |
| `approve` | `requests` | `requests`, `host` | what the owner approves, see [Approval modes](#approval-modes) |
| `expire_hours` | `23` | whole hours from 1 to 23 | how long an action waits for a decision, see [Expiry](#expiry) |

Claude Code passes the options to the plugin's hook and server as `CLAUDE_PLUGIN_OPTION_MAIN_GROUP`, `CLAUDE_PLUGIN_OPTION_APPROVE` and `CLAUDE_PLUGIN_OPTION_EXPIRE_HOURS`. A changed option applies from the next session start, and the daemon then uses it for all sessions: a new `main_group` renames the Main group and its profile.

An invalid value is reported at session start, and every tool call of that session returns the same text:

```
Plugin option expire_hours: hours from 1 to 23 expected, got 48. Change it in /plugin > Installed > simplex > Configure options.
```

## Sessions and profiles

Each Claude Code session ID has its own SimpleX profile, with its own chats, contacts and incognito profiles. The profile name is `SimpleX MCP - <name>`: the session name from `claude -n NAME` or `/rename`, else the name of the project folder, else `Claude Code`. The name follows the display name rules: characters a display name can't hold are left out, and it keeps at most 50 characters. A session renamed with `/rename` gets the new profile name at its next start, for example with `claude --resume`.

- Each session has a number, 1 for the first, kept across daemon restarts. Cards and owner commands use it: `Session 2`, `2:@3`.
- The session's MCP server creates its profile when it first reaches the daemon; the SessionStart hook only records the title. After `/clear`, Claude Code keeps the MCP server and its session ID, so the session keeps its profile, and no profile is created for the new ID the hook sees.
- `claude --resume` reopens the session's profile with its chats.
- A session profile is not a member of the Main group. Peers of a chat see only the profile of that session, or the incognito profile of that chat.
- One daemon runs all profiles. It opens a session's profile at the session's first request, closes it 10 minutes after the session ends when it has no action, and stops 10 minutes after the last profile closes. The next session start or tool call starts it again.
- Profiles of ended sessions stay on disk; see [Recovery](#recovery) to remove one.

## The Main group and its owners

The Main group is a SimpleX group created by the daemon's own profile, named after `main_group`. Every session posts its cards there.

- Whoever joins by the link first becomes an owner, and the link is deleted. The link works once.
- Every owner of the group approves actions, including owners added later in the app.
- When no owner remains, the daemon creates a new link; `/simplex:link` shows it, and a refused tool call includes it.
- `/simplex:link` with owners prints `Owners of "SimpleX MCP": Alice. To replace them, run /simplex:reset-owners.`
- `/simplex:reset-owners` deletes the Main group for all its members, creates a new one with the same action counter, and prints `"SimpleX MCP" was replaced. Join the new one in the SimpleX app. The link works once:` with the new link. Member IDs change in the new group, so standing approvals given in the old one no longer apply.

The skills that show and replace owners can't be invoked by the model, and no tool changes the options or the owners.

## What the owner sees

All of it is in the Main group, posted by its profile. Each card starts with the session line. Names that are valid display names are shown plain. All other text written by the agent or by peers, other names included, is shown as code, line by line; names and errors take one line. Commands and SimpleX links inside it cannot be tapped: a valid display name cannot contain `/`, and the app parses each line of a message separately.

```
SimpleX MCP: Session 1 · SimpleX MCP - website
             I want to connect to bot SimpleX Directory (@directory.simplex), as myself.
             `Find groups, channels, businesses and bots.`
             /approve 1 or /reject 1

SimpleX MCP: Session 2 · SimpleX MCP - docs
             I want to send a message to business Ridgeline, where I am incognito:
             `Is the Talus Trail waterproof?`
             /approve 2:#9 4, /approve 2:#9 all or /reject 2:#9 4

SimpleX MCP: Session 2 · SimpleX MCP - docs
             I want to leave business Ridgeline (#9).
             /approve 2:#9 12 or /reject 2:#9 12
```

After the decision the card is edited, and its commands removed:

```
SimpleX MCP: Session 2 · SimpleX MCP - docs
             I sent a message to business Ridgeline, where I am incognito:
             `Is the Talus Trail waterproof?`
             Approved by Alice.
```

Commands. `S:@N` and `S:#N` name chat `@N` or `#N` of session S:

| Command | Effect |
|---|---|
| `/approve N`, `/approve S:#9 N` | Performs action N; the cards of sends and leaves name the chat. |
| `/reject N`, `/reject S:#9 N` | Rejects action N. |
| `/approve S:#9 all` | Approves all messages of session S to chat #9: the pending ones and later ones. |
| `/revoke S:#9` | Ends that approval: each message to #9 needs approval again. |
| `/pending` | Lists pending actions of all sessions: `N  session S: WHAT  expires YYYY-MM-DD HH:MM UTC`. |
| `/list` | Lists the chats of each open session under `Session S · NAME`, at most 100 per session, one line each, with the chats approved for all messages; a last line counts the older chats. |

Action numbers are unique across sessions. `/pending` and `/list` are in the group's command menu; the cards carry the others. Each command includes its number, because the app sends a tapped command as a new message, without a quote. Only owners' commands are accepted; others get `Only owners approve actions.` `/list`, `/approve S:#9 all` and `/revoke S:#9` need the session's profile to be open: otherwise the reply is `No open session S. /list shows the open sessions.`

### Approval of all messages to a chat

After `/approve 2:#9 all`, the session's messages to #9 are sent at once, and each one is posted in the Main group after it is sent:

```
SimpleX MCP: Session 2 · SimpleX MCP - docs
             I sent a message to business Ridgeline, where I am incognito:
             `Thanks, I'll order.`
             Approved for this chat by Alice. /revoke 2:#9
```

- It covers messages only: connecting and leaving are approved one by one.
- It lasts until `/revoke`, until the chat is deleted, or until the approving owner is no longer an owner. It survives restarts: it is kept in the session's database, as custom data of that chat.
- With it, the agent can send anything it can read, without asking, to everyone in that chat.
- It applies in mode `requests` only: in mode `host`, `list` and `/list` show no approvals.

## Approval modes

The option `approve` sets what the owner approves.

| Mode | The agent's actions: connect, send, leave | Messages to the agent |
|---|---|---|
| `requests` (default) | Performed after `/approve N`; a send to a chat approved for all messages at once, with a copy posted in the Main group. | Passed to the agent. |
| `host` | Performed at once; a copy is posted in the Main group. | Passed to the agent. |

In mode `requests`, the plugin's PreToolUse hook allows its tool calls, so Claude Code does not prompt for them.

In mode `host`, approval is left to Claude Code. The session's MCP server marks `connect`, `send` and `leave` with `_meta["anthropic/requiresUserInteraction"]: true` when it starts, so Claude Code shows its permission prompt on every call of these tools; the PreToolUse hook does not skip it. In `dontAsk` mode and in `claude -p` runs Claude Code denies these calls. In both modes a new action is refused while the Main group has no owner.

## Expiry

A pending action expires after `expire_hours`, 23 by default and at most: SimpleX allows edits of a message for 24 hours, and the outcome of a pending action is an edit of its card. Cards don't show the time; `/pending` does. The last line of its card is edited to `Expired.`, and the agent's call returns `{"action": N, "status": "expired"}`. For the same reason, a `connect` waits for the other side at most until 23 hours 50 minutes after the action was created.

A result that no call has returned is dropped after the action's expiry time; a repeated call after that creates a new action. At most 20 actions of one session are pending at once, and at most 20 are being performed; a further `connect`, `send` or `leave` of that session fails until the owner decides.

Actions last only while the daemon runs. The daemon keeps a session's profile open while it has actions. When the daemon stops, the last line of each pending card is edited to `Cancelled: the server stopped.`; after a restart, `/approve` of an earlier number replies `Action N is not pending.`, and the agent's repeated call posts a new card. Action numbers continue across restarts.

## What the agent sees

Five tools:

| Tool | What it does |
|---|---|
| `connect` | Joins a contact, bot, business, group or channel by a link or a SimpleX name, as the session's profile or incognito. Returns the chat, its profile, welcome message, the commands a bot advertises, and the messages so far. |
| `send` | Sends a message and returns every message received in that chat after it, until none arrives for `idle` seconds (30 by default). A directory bot's search results arrive this way. |
| `receive` | Returns the chat's unread messages and those that arrive while it waits, waiting for one when asked. |
| `list` | Lists the session's chats with unread counts and `approved: true` for chats approved for all messages, and the session's actions without a result. |
| `leave` | Deletes a contact, or leaves a group, business chat or channel, and deletes the chat; the contact or the members are notified. |

A `send` to a chat that `list` shows with `connected: false` is refused before any card. A `send` is refused with `text: too long for its approval card; shorten it.` when its card, in its longest outcome form, would exceed 15000 bytes in the SimpleX message, which holds at most 15602; a text of many lines or multi-byte characters reaches it below 12000 characters.

Incognito: the core creates a random profile for that one chat, so peers of different chats can't link them. The tool inputs, results and errors are in [`spec/api.md`](spec/api.md).

When a tool call is refused because the Main group has no owner, the text is `The agent has no owner. Open this link in the SimpleX app and join: <link>`.

## Waiting for approval in Claude Code

An MCP server cannot start a turn of the agent: what the agent reads from the server are the results of its own tool calls. A call that waits for the owner, or for responses, stays open, and Claude Code turns it into a background task:

- A tool call in the main conversation that runs past two minutes moves to a background task. Claude continues working, and the result arrives as a task notification. When it arrives while the session is idle, Claude responds to it without a prompt (checked with Claude Code 2.1.300).
- To move calls to the background sooner, set `CLAUDE_CODE_MCP_AUTO_BACKGROUND_MS`, for example to `10000`, in the `env` block of `~/.claude/settings.json`. This applies to every MCP server.
- Calls from subagents, and calls in `claude -p` runs unless `CLAUDE_AUTO_BACKGROUND_TASKS=1`, stay in the foreground: the subagent or the run waits.
- A background task ends with the session. The action stays in the daemon while it runs; `list` shows it, and the same call again picks it up.

## Files

The data folder is `~/.simplex-mcp`, or the folder in `SIMPLEX_MCP_HOME`:

| Path | Content |
|---|---|
| `token` | access token of the daemon socket, readable only by the user |
| `lock` | PID of the running daemon |
| `daemon.sock` | the daemon's Unix socket; Windows uses the named pipe `\\.\pipe\simplex-mcp-<hash>` |
| `daemon.log` | the daemon's log; emptied at a daemon start when it is over 10 MB |
| `_hub/` | databases of the Main group's profile: `simplex_chat.db`, `simplex_agent.db` |
| `_sessions/<session ID>/` | databases of one session's profile, and `session.json` with its number, name, project folder and creation time |

The folders are readable only by the user (mode 0700), and so are the files the daemon writes itself (mode 0600), except on Windows. When the socket path is longer than 103 bytes, the socket is `simplex-mcp-<hash>.sock` in the system temporary folder, still protected by the token.

The daemon keeps the action counter as custom data of the Main group, the approvals of all messages to a chat as custom data of that chat in the session's database, and the actions in memory.

One daemon runs per data folder: a second one finds the lock and exits, because two processes on one database break each other's connections (a SimpleX server delivers a queue's messages only to the last process that subscribed to it).

Data folders of v0.1 (`~/.simplex-mcp/<name>/`) are not used by v0.2.

## Recovery

| Situation | What happens, what to do |
|---|---|
| The session start shows an error | The text follows `SimpleX MCP:`; details are in `~/.simplex-mcp/daemon.log`. The session starts without the daemon, and the next tool call tries again. |
| Node.js is older than 22 | The session start shows `SimpleX MCP needs Node.js 22 or later; found <version>.` |
| The daemon stopped during a call | The call returns `The SimpleX daemon stopped; repeat the call.`, and the next call starts a new daemon. The actions of the stopped daemon are lost; a repeated call creates a new action. |
| A newer plugin version started | It stops the older daemon and starts its own. When the two versions can't talk to each other, tool calls return `Restart Claude Code to update the SimpleX plugin.` |
| Every owner left the Main group or was removed | `connect`, `send` and `leave` are refused with the new link. Join with it, or run `/simplex:link` to see it. |
| The Main group was deleted, its profile was removed from it, or it lost its owner role | `connect`, `send` and `leave` fail with `The Main group is lost. Run /simplex:reset-owners in Claude Code.` Run `/simplex:reset-owners` and join the new group. |
| The wrong person joined by the link | Run `/simplex:reset-owners`. |
| The QR code does not scan | Run `npx -y @simplex-chat/mcp link` in a terminal, or paste the link into the app. |
| An invalid option | The session start and every tool call show the option, the value and where to change it. |
| Remove the profile of an ended session | Delete `~/.simplex-mcp/_sessions/<session ID>/` while the daemon is not running. Its chats are deleted with it, and its peers are not notified. |
| The token leaked | End all Claude Code sessions, wait until the daemon stops, delete `~/.simplex-mcp/token`, and start a new session: it creates a new token. |

The daemon stops 10 minutes after the last session profile closes, or at `SIGTERM` to the PID in `~/.simplex-mcp/lock`. It then cancels pending actions and edits their cards.

## Command reference

`npx -y @simplex-chat/mcp --help` prints:

```
Usage:
  simplex-mcp session               MCP server of one Claude Code session (run by the plugin)
  simplex-mcp daemon                the shared SimpleX daemon (started on demand)
  simplex-mcp hook session-start    Claude Code SessionStart hook (run by the plugin)
  simplex-mcp link                  show the Main group link, or its owners
  simplex-mcp reset-owners          replace the Main group and its owners

Install in Claude Code:
  /plugin marketplace add https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/.claude-plugin/marketplace.json
  /plugin install simplex@simplex-chat

Environment:
  SIMPLEX_MCP_HOME   data folder (default: ~/.simplex-mcp)
```

- `link` and `reset-owners` start the daemon when it is not running. In a terminal they print a QR code with the link; `/simplex:link` and `/simplex:reset-owners` run them without one.
- `session` exits with code 2 and `simplex-mcp session runs from the Claude Code plugin.` outside Claude Code.
- An unknown command exits with code 2 and `Unknown command. Run simplex-mcp --help.`

## Security

- The daemon listens only on its local socket, in a folder readable only by the user. Every request needs the token, compared in constant time; on Windows, where the named pipe has no folder permissions, the token is the only check.
- Any process with the token can act as any session. The token is stored only in `~/.simplex-mcp/token`.
- Only owners of the Main group approve actions. Session profiles are never members of the Main group, so a session can't approve its own actions.
- The skills that change owners can't be invoked by the model, and no tool changes the options or the owners.
- Whoever opens the link first becomes an owner. The link works once.
- Names written by peers are folded to one line, with control and format characters replaced by spaces, in the log and in the Main group; there, a name that is not a valid display name is shown as code.
- After `/approve S:#N all`, the session sends to that chat without asking; `/revoke S:#N` ends it.
- Peers of a chat connected as the session's profile see that profile; peers of an incognito chat see a random profile made for that chat only.
- The agent has no address, so nobody can contact it first.
- The agent uses private message routing: the SMP server that holds a contact's queue does not see the agent's IP address.
