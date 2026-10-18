# SimpleX MCP server

`@simplex-chat/mcp` is a local server that gives an AI agent its own presence on the SimpleX network. The agent connects to people, bots, groups, channels and directories, as itself or incognito, sends messages and receives all the responses. Bots tell it which commands they understand, so every bot on the network becomes a service the agent can use. You supervise the agent from its control chat in SimpleX: it asks there before it connects or sends, and you approve or reject from your phone with one tap. Four tools cover it: `connect`, `send`, `receive`, `list`. Setup is one command to start the agent, a scan to join its control chat, and one command to add it to Claude Code.

What it isn't:
- **Not a directory or a search service.** The agent reaches chats through links, SimpleX names and the directories it connects to.
- **Not a push channel into the agent.** An MCP server can't start a turn of the agent, so news reaches it only as results of calls it makes: waits inside `send` and `receive`, or polling.
- **Not an always-on service.** The agent acts while its session runs; services that must always be up should be bots.
- **Not hosted.** It runs on your machine, one process per agent.
- **Not in v0.1:**
  - an address for the agent: nobody can contact it first;
  - your approval of incoming messages;
  - files;
  - a Claude Code plugin or channel;
  - registration in other hosts.

Status: v0.1, implemented and in testing, not yet published to npm; `TESTING.md` describes a local build and test. The design, the tools and the scenarios are in `PLAN.md`.

- [Requirements](#requirements)
- [Quick start](#quick-start)
- [Owner approval](#owner-approval)
- [Add the agent to Claude Code](#add-the-agent-to-claude-code)
- [Approval modes](#approval-modes)
- [Expiry](#expiry)
- [What the owner sees](#what-the-owner-sees)
- [What the agent sees](#what-the-agent-sees)
- [Waiting for approval in Claude Code](#waiting-for-approval-in-claude-code)
- [Several agents](#several-agents)
- [Other MCP hosts](#other-mcp-hosts)
- [Recovery](#recovery)
- [Command reference](#command-reference)
- [Data folder](#data-folder)
- [Security](#security)

## Requirements

- Node.js 22.
- One of the platforms of the `simplex-chat` package: Linux x86-64 or arm64, macOS x86-64 or arm64, Windows x86-64.
- On Linux x86-64: glibc 2.34 or later, and the system libraries `libcrypto.so.3`, `libgmp.so.10` and `libz.so.1`.
- The SimpleX Chat app on a phone or computer, for the owner.
- Claude Code; other MCP hosts with Streamable HTTP and custom headers can be configured by hand.

At its first start the server downloads the SimpleX core library, 29 to 50 MB depending on the platform, into `~/.cache/simplex-chat` (macOS: `~/Library/Caches/simplex-chat`, Windows: `%LOCALAPPDATA%\simplex-chat`). Unpacked on Linux, it takes about 240 MB.

## Quick start

1. Start the agent's server in a terminal and keep it running. NAME, here `agent`, names its data folder and its entry in Claude Code; `--display-name` sets the name peers see, NAME by default:

   ```
   npx -y @simplex-chat/mcp serve agent
   ```

   The first start prints, after two lines about downloading the SimpleX core library and the Node.js add-on:

   ```
   SimpleX MCP server 0.1.0, agent "agent"
   Data folder: /home/alice/.simplex-mcp/agent
   Approval mode: requests
   Agent control chat "agent" created.
   Join the agent control chat in the SimpleX app. The link works once:
   <QR code>
   https://smp4.simplex.im/g#...
   Whoever joins by this link becomes an owner of "agent" and approves its actions. To approve owners yourself, start with --owners.
   Listening on http://127.0.0.1:5226/mcp
   ```

   `-y` answers npm's prompt to install the package. The server asks no questions: every choice is an option.

2. In the SimpleX app, scan the QR code or open the link, and tap Join. The chat "agent" appears in the chat list, and the agent writes: `I'm your agent. I ask here before I connect or send.` The terminal prints:

   ```
   Alice joined (member 3) and is now an owner. The link is deleted.
   Add the agent to Claude Code:
     npx -y @simplex-chat/mcp register agent
   ```

3. In another terminal, add the agent to Claude Code:

   ```
   $ npx -y @simplex-chat/mcp register agent
   Added to Claude Code: scope user, server simplex-agent.
   ```

4. Add `"mcp__simplex-agent"` to `permissions.allow` in `~/.claude/settings.json`, so Claude Code does not prompt for each call; approval happens in the SimpleX app.

5. Start Claude Code and ask it, for example: `Connect to the SimpleX Directory at https://smp4.simplex.im/a#lXUjJW5vHYQzoLYgmi8GbxkGP41_kjefFvBrdwg-0Ok and search it for trail running shoes.` In the chat "agent", the agent writes:

   ```
   I want to connect to bot SimpleX Directory, as myself.
   `Find groups, channels, businesses and bots.`
   /approve 1 or /reject 1. Expires 2026-10-18 09:02 UTC.
   ```

   Tap `/approve 1`. The agent then asks to send its search, and you approve that too. Once the name `@directory.simplex` is registered, it works as the target too, and the card shows `(@directory.simplex)`.

Without `--owners`:

- whoever joins by the link becomes an owner of the agent control chat;
- every owner approves actions, including owners added later in the app;
- the link is deleted when the first person joins.

## Owner approval

With `--owners`, only the owners listed in it approve actions. An owner added later in the app approves actions only after the server is restarted with that owner in the list.

1. Start the server with `--owners` and no list:

   ```
   npx -y @simplex-chat/mcp serve agent --owners
   ```

   The output is the same as in the quick start, with the line `The link adds an observer. Its ID and name are printed when it joins.` in place of `Whoever joins by this link ...`.

2. Join the agent control chat in the SimpleX app with the link. The link is deleted. The terminal prints, and the agent posts in the chat:

   ```
   Alice joined (member 3). Restart the server with --owners 3:Alice
   ```

   Until the restart, the agent's `connect` and `send` fail.

3. Stop the server with Ctrl-C and start it with the list:

   ```
   $ npx -y @simplex-chat/mcp serve agent --owners 3:Alice
   Owners: 3:Alice.
   Add the agent to Claude Code:
     npx -y @simplex-chat/mcp register agent
   ```

4. To add an owner, add the person to the agent control chat in the app and make them an owner there. The terminal prints, and the agent posts in the chat:

   ```
   Bob (member 7) is now an owner and can't approve actions yet. Restart the server with --owners 3:Alice,7:Bob
   ```

   When an owner leaves or is removed, the agent lists the human owners who remain:

   ```
   Alice (member 3) is no longer an owner. Restart the server with --owners 7:Bob
   ```

   When none remains: `No owners remain: restart the server with --reset-owners --owners`.

At every start with a list, the server removes the connected members of the agent control chat other than the listed owners.

## Add the agent to Claude Code

```
npx -y @simplex-chat/mcp register agent                  # all Claude Code projects
npx -y @simplex-chat/mcp register agent --scope local    # the current project only
```

`register NAME` reads the agent's port and token from its data folder, runs `claude mcp remove --scope SCOPE simplex-NAME` (SCOPE: `user`, or the `--scope` value), ignoring a missing entry, and then, for `register agent`:

```
claude mcp add-json --scope user simplex-agent '{"type":"http","url":"http://127.0.0.1:5226/mcp","headers":{"Authorization":"Bearer <token>"},"timeout":169800000}'
```

- NAME: 1 to 32 lowercase letters, digits and single hyphens, starting with a letter or digit.
- The agent's tools in Claude Code are `mcp__simplex-agent__connect` and the rest.
- `timeout` is the expiry plus 24 hours and 10 minutes, in milliseconds: the longest wait for a decision plus the longest wait for responses. After a change of `--expire` or `--port`, register the agent again; an agent on another port needs the same `--port` at every start.
- The server must be running when Claude Code starts a session. If a session started before the server, run `/mcp reconnect all` in it (Claude Code 2.1.284 or later; earlier, reconnect `simplex-agent` in `/mcp`).

Errors:

```
situation                  message
never started              Agent "agent" was never started. Start it with: npx -y @simplex-chat/mcp serve agent
claude not on PATH         Add this entry under "mcpServers" in ~/.claude.json: {"simplex-agent": {...}}
                           (--scope local: under "projects" > "<project folder>" > "mcpServers")
```

The token is created at the agent's first start and saved in its data folder. `serve agent --new-token` replaces it; the agent must then be registered again.

## Approval modes

`--approve MODE` sets what the owner approves. It is set only by this option.

| Mode | The agent's actions: connect, send | Messages to the agent |
|---|---|---|
| `requests` (default) | Performed after `/approve N`. | Passed to the agent. |
| `host` | Performed at once; a copy is posted in the agent control chat. | Passed to the agent. |

Claude Code's own permission check applies to every tool call too. In mode `requests`, add the agent's server, for example `"mcp__simplex-agent"`, to `permissions.allow` in `~/.claude/settings.json`; otherwise Claude Code prompts for each call, or in auto mode its classifier decides it.

In `host` mode, approval is left to the MCP host. The server marks `connect` and `send` with `_meta["anthropic/requiresUserInteraction"]: true`, so Claude Code shows its permission prompt on every call, in `acceptEdits`, `auto` and `bypassPermissions` modes as well, and allow rules do not skip it. In `dontAsk` mode and in `claude -p` runs Claude Code denies these calls.

## Expiry

A pending action expires after `--expire HOURS`, 23 by default and at most: SimpleX allows edits of a message for 24 hours, and every outcome is an edit of the card. The last line of its card is edited to `Expired.`, and the agent's call returns `{"action": N, "status": "expired"}`.

Actions last only while the server runs. When the server stops, the last line of each pending card is edited to `Cancelled: the server stopped.`; after a restart, `/approve` of an earlier number replies `Action N is not pending.`, and the agent's repeated call posts a new card. Action numbers continue across restarts.

## What the owner sees

All of it is in the agent control chat, posted by the agent's profile. The chat has the agent's display name; to see it under another name in your chat list, set an alias for the chat in the app. Text written by the agent or by peers is shown as code, line by line, so commands inside it cannot be tapped.

```
agent:   I want to connect to bot SimpleX Directory (@directory.simplex), as myself.
         `Find groups, channels, businesses and bots.`
         /approve 1 or /reject 1. Expires 2026-10-18 09:02 UTC.

agent:   I want to send a message to business Ridgeline, where I am incognito:
         `Is the Talus Trail waterproof?`
         /approve 4 or /reject 4. Expires 2026-10-18 09:40 UTC.
```

After the decision the card is edited, and its commands removed:

```
agent:   I sent a message to business Ridgeline, where I am incognito:
         `Is the Talus Trail waterproof?`
         Approved by Alice.
```

Commands, also in the chat's command menu:

| Command | Effect |
|---|---|
| `/approve N` | Performs action N. |
| `/reject N` | Rejects action N. |
| `/pending` | Lists pending actions. |

Each command includes its number, because the app sends a tapped command as a new message, without a quote. Every card form and reply is listed in `PLAN.md`, section "What the owner sees: the agent control chat".

## What the agent sees

Four tools:

| Tool | What it does |
|---|---|
| `connect` | Joins a contact, bot, business, group or channel by a link or a SimpleX name, as the agent's profile or incognito. Returns the chat, its profile, welcome message, the commands a bot advertises, and the messages so far. |
| `send` | Sends a message and returns every message received in that chat after it, until none arrives for `idle` seconds (30 by default). A directory bot's search results arrive this way. |
| `receive` | Returns the chat's unread messages and those that arrive while it waits, waiting for one when asked. |
| `list` | Lists the agent's chats with unread counts, and its actions without a result. |

Incognito: the core creates a random profile for that one chat, so peers of different chats can't link them. The tools' inputs, results and errors are in `PLAN.md`, section "What the agent sees: tools", and the call sequences of the scenarios in section "Sequence diagrams".

## Waiting for approval in Claude Code

An MCP server cannot start a turn of the agent: what the agent reads from the server are the results of its own tool calls. A call that waits for the owner, or for responses, stays open, and Claude Code turns it into a background task:

- A tool call in the main conversation that runs past two minutes moves to a background task. Claude continues working, and the result arrives as a task notification. When it arrives while the session is idle, Claude responds to it without a prompt (checked with Claude Code 2.1.300).
- To move calls to the background sooner, set `CLAUDE_CODE_MCP_AUTO_BACKGROUND_MS`, for example to `10000`, in the `env` block of `~/.claude/settings.json`. This applies to every MCP server.
- Calls from subagents, and calls in `claude -p` runs unless `CLAUDE_AUTO_BACKGROUND_TASKS=1`, stay in the foreground: the subagent or the run waits.
- A background task ends with the session. The action stays in the server while it runs; `list` shows it, and the same call again picks it up.

## Several agents

Each agent is its own process, with its own data folder, port, agent control chat and Claude Code server:

```
npx -y @simplex-chat/mcp serve agent-2 --port 5227
npx -y @simplex-chat/mcp register agent-2
```

## Other MCP hosts

Add an entry with the URL `http://127.0.0.1:5226/mcp` and the header `Authorization: Bearer <token>`. A host that ends a tool call early returns a timeout to the agent; the agent repeats the call with the same arguments, which picks up the same action. Hosts with a fixed tool-call limit, between 60 and 300 seconds in several hosts, need `wait` below it.

## Recovery

| Situation | What to do |
|---|---|
| The server was not running when Claude Code started | Start the server, then run `/mcp reconnect all` in Claude Code (2.1.284 or later). |
| The server restarted while Claude Code was running | Claude Code reconnects up to five times within about 30 seconds. After that, in an interactive session, run `/mcp reconnect all`; `claude -p` runs and Agent SDK sessions keep retrying for about half an hour. Open calls fail, and pending actions are cancelled; the agent's repeated call creates a new request. |
| A second server was started on the same data folder | It exits with `Another server uses <folder> (PID N).` |
| Every owner left the agent control chat or was removed | `connect` and `send` fail. Without `--owners`, the server logs `No owners in the agent control chat.` and prints a new link; join with it. With an `--owners` list, the agent posts and the server logs `No owners remain: restart the server with --reset-owners --owners`. |
| The agent was removed from the agent control chat, the chat was deleted, or the agent lost its owner role | `connect` and `send` fail, and the server logs `The agent lost its control chat. Restart the server with --reset-owners.` |
| The wrong person joined by the link | Start with `--reset-owners`. |
| The token leaked | Start with `--new-token` and register the agent again. |

`--reset-owners` deletes the agent control chat for all its members, creates a new one and prints a new link. Member IDs change in the new chat, so `--reset-owners` takes `--owners` without a list.

## Command reference

`npx -y @simplex-chat/mcp --help` prints:

```
Usage:
  simplex-mcp serve NAME [options]     run agent NAME
  simplex-mcp register NAME [options]  add agent NAME to Claude Code

serve options:
  --data DIR             data folder (default: ~/.simplex-mcp/NAME)
  --display-name TEXT    the agent's name in SimpleX (default at the first start: NAME)
  --approve MODE         what the owner approves in the agent control chat:
                           requests  the agent's connections and messages (default)
                           host      nothing; the MCP host asks
  --owners [ID:NAME,...] only these owners approve; without a list, the link
                         adds an observer and its ID is printed at the join.
                         Without --owners, whoever joins becomes an owner and
                         every owner approves.
  --expire HOURS         pending actions expire after HOURS, 1 to 23 (default: 23)
  --reset-owners         replace the owners: a new agent control chat and link
  --new-token            replace the access token; the agent must be registered again
  --port N               HTTP port on 127.0.0.1 (default: 5226)

register options:
  --scope SCOPE          user (default) or local
  --data DIR             data folder (default: ~/.simplex-mcp/NAME)

--reset-owners takes --owners without a list.
```

## Data folder

```
~/.simplex-mcp/agent/
  simplex_chat.db    SimpleX chat database: the agent's profile and chats
  simplex_agent.db   SimpleX agent database: connections and keys
  token              access token, readable only by the user
  server.json        name, port and expiry of the server, read by register
  lock               PID of the running server
```

The server keeps its action counter in the chat database, as custom data of the agent control chat, and its actions in memory.

The server keeps no log file: it writes its log to the terminal where `serve` runs, events to stdout and errors to stderr.

One server runs per data folder: a second one exits, because two processes on one database break each other's connections (a SimpleX server delivers a queue's messages only to the last process that subscribed to it).

## Security

- The server listens only on 127.0.0.1. Every request needs the token. Requests from web pages on other hosts, and requests with another `Host`, are refused (the SDK's localhost Origin and Host checks).
- Any process with the token can act as the agent.
- Without `--owners`, whoever opens the link first becomes an owner. The link works once.
- With `--owners`, only the listed owners approve.
- Peers of a chat connected as the agent see its profile; peers of an incognito chat see a random profile made for that chat only.
- The agent has no address, so nobody can contact it first.
- The agent uses private message routing: the SMP server that holds a contact's queue does not see the agent's IP address.
