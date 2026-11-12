# SimpleX MCP server: reference

[README](README.md) has the short version.

`@simplex-chat/mcp` is a local server that gives an AI agent its own presence on the SimpleX network. The agent connects to people, bots, groups, channels and directories, as itself or incognito, sends messages and files, and receives all the responses, with the files people send it. Bots tell it which commands they understand, so every bot on the network becomes a service the agent can use. You supervise the agent from its control chat in SimpleX: it asks there before it connects, sends, downloads a file or leaves, and you approve or reject from your phone with one tap, or approve all its messages to a chat you trust, or all files from it. Six tools cover it: `connect`, `send`, `receive`, `download`, `list`, `leave`. Setup is one command to start the agent, a scan to join its control chat, and one command to add it to Claude Code.

What it isn't:
- **Not a directory or a search service.** The agent reaches chats through links, SimpleX names and the directories it connects to.
- **Not a push channel into the agent.** An MCP server can't start a turn of the agent, so news reaches it only as results of calls it makes: waits inside `send` and `receive`, or polling.
- **Not an always-on service.** The agent acts while its session runs; services that must always be up should be bots.
- **Not hosted.** It runs on your machine, one process per agent.
- **Not in v0.3:**
  - an address for the agent: nobody can contact it first;
  - your approval of incoming messages;
  - a Claude Code plugin or channel;
  - registration in other hosts.

Status: v0.3. [`TESTING.md`](TESTING.md) describes a local build and test. The design, the tools and the scenarios are in [`PLAN.md`](PLAN.md).

- [Requirements](#requirements)
- [Quick start](#quick-start)
- [Owner approval](#owner-approval)
- [Add the agent to Claude Code](#add-the-agent-to-claude-code)
- [Approval modes](#approval-modes)
- [Expiry](#expiry)
- [What the owner sees](#what-the-owner-sees)
- [What the agent sees](#what-the-agent-sees)
- [Status while the agent works](#status-while-the-agent-works)
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
- curl 7.55.0 or later on the PATH of Claude Code, for the [status hooks](#status-hooks).

At its first start the server downloads the SimpleX core library, 29 to 50 MB depending on the platform, into `~/.cache/simplex-chat` (macOS: `~/Library/Caches/simplex-chat`, Windows: `%LOCALAPPDATA%\simplex-chat`). Unpacked on Linux, it takes about 240 MB.

## Quick start

1. Start the agent's server in a terminal and keep it running. NAME, here `my-agent`, names the agent's data folder and its entry in Claude Code; `--display-name` is the name people see in SimpleX, NAME by default:

   ```
   npx -y @simplex-chat/mcp serve my-agent --display-name "My Agent"
   ```

   The first start prints, after two lines about downloading the SimpleX core library and the Node.js add-on:

   ```
   SimpleX MCP server 0.3.0, agent "my-agent"
   Data folder: /home/alice/.simplex-mcp/my-agent
   Approval mode: requests
   Agent control chat "My Agent" created.
   Join the agent control chat in the SimpleX app. The link works once:
   <QR code>
   https://smp4.simplex.im/g#...
   If the QR code does not scan, restart with --qr-code large, or paste the link into the app.
   Whoever joins by this link becomes an owner of "My Agent" and approves its actions. To approve owners yourself, start with --owners.
   Listening on http://127.0.0.1:5226/mcp
   ```

   `-y` answers npm's prompt to install the package. The server asks no questions: every choice is an option. The QR code of such a link is 37 columns wide and 19 lines high; `--qr-code large` draws it twice as wide and twice as high.

2. In the SimpleX app, scan the QR code or open the link, and tap Join. The chat "My Agent" appears in the chat list, and the agent writes: `I'm your agent. I ask here before I connect, send, download a file or leave a chat.` The terminal prints:

   ```
   Alice joined (member 3) and is now an owner. The link is deleted.
   Add the agent to Claude Code:
     npx -y @simplex-chat/mcp register my-agent
   ```

3. In another terminal, add the agent to Claude Code:

   ```
   $ npx -y @simplex-chat/mcp register my-agent
   Added to Claude Code: scope user, server simplex-my-agent, status hooks in /home/alice/.claude/settings.json.
   ```

4. Add `"mcp__simplex-my-agent"` to `permissions.allow` in `~/.claude/settings.json`, so Claude Code does not prompt for each call; approval happens in the SimpleX app.

5. Start Claude Code and ask it, for example: `Connect to the SimpleX Directory at https://smp4.simplex.im/a#lXUjJW5vHYQzoLYgmi8GbxkGP41_kjefFvBrdwg-0Ok and search it for trail running shoes.` In the chat "My Agent", the agent writes:

   ```
   I want to connect to bot SimpleX Directory, as myself.
   `Find groups, channels, businesses and bots.`
   /approve 1 or /reject 1
   ```

   Tap `/approve 1`. The agent then asks to send its search, and you approve that too, or tap the card's `all` command to approve all its messages to the directory. Once the name `@directory.simplex` is registered, it works as the target too, and the card shows `(@directory.simplex)`.

Without `--owners`:

- whoever joins by the link becomes an owner of the agent control chat;
- every owner approves actions, including owners added later in the app;
- the link is deleted when the first person joins.

## Owner approval

With `--owners`, only the owners listed in it approve actions. An owner added later in the app approves actions only after the server is restarted with that owner in the list.

1. Start the server with `--owners` and no list:

   ```
   npx -y @simplex-chat/mcp serve my-agent --owners
   ```

   The output is the same as in the quick start, with the line `The link adds an observer. Its ID and name are printed when it joins.` in place of `Whoever joins by this link ...`.

2. Join the agent control chat in the SimpleX app with the link. The link is deleted. The terminal prints, and the agent posts in the chat:

   ```
   Alice joined (member 3). Restart the server with --owners 3:Alice
   ```

   Until the restart, the agent's `connect`, `send`, `download` and `leave` fail.

3. Stop the server with Ctrl-C and start it with the list:

   ```
   $ npx -y @simplex-chat/mcp serve my-agent --owners 3:Alice
   Owners: 3:Alice.
   Add the agent to Claude Code:
     npx -y @simplex-chat/mcp register my-agent
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

In the chat, the `--owners ...` part of these lines is shown as code, so a member's name in it cannot form a tappable command.

At every start with a list, the server removes the connected members of the agent control chat other than the listed owners.

## Add the agent to Claude Code

```
npx -y @simplex-chat/mcp register my-agent                  # all Claude Code projects
npx -y @simplex-chat/mcp register my-agent --scope local    # the current project only
```

`register NAME` reads the agent's port from its data folder, runs `claude mcp remove --scope SCOPE simplex-NAME` (SCOPE: `user`, or the `--scope` value), ignoring a missing entry, and then, for `register my-agent`:

```
claude mcp add-json --scope user simplex-my-agent '{"type":"http","url":"http://127.0.0.1:5226/mcp","headersHelper":"cat /home/alice/.simplex-mcp/my-agent/headers.json","timeout":169800000}'
```

- NAME: 1 to 32 lowercase letters, digits and single hyphens, starting with a letter or digit.
- The entry has no token. At every connection Claude Code runs the `headersHelper` command and sends the header it prints from `headers.json` in the data folder; on Windows the command is `type "<folder>\headers.json"`. With `--scope local`, Claude Code runs it only in a folder whose trust dialog was accepted.
- The agent's tools in Claude Code are `mcp__simplex-my-agent__connect` and the rest.
- `timeout` is the expiry plus 24 hours and 10 minutes, in milliseconds: the longest wait for a decision plus the longest wait for responses. After a change of `--expire` or `--port`, register the agent again; an agent on another port needs the same `--port` at every start.
- The server must be running when Claude Code starts a session. If a session started before the server, run `/mcp reconnect all` in it (Claude Code 2.1.284 or later; earlier, reconnect `simplex-my-agent` in `/mcp`).

When `claude mcp add-json` succeeds, `register` adds the [status hooks](#status-hooks) to the settings file of the scope, and prints its path:
- `user`: `settings.json` in `CLAUDE_CONFIG_DIR`, or in `~/.claude` when it is unset or empty;
- `local`: `.claude/settings.local.json` in the project folder.

The hooks are one group without a matcher for each of the events `UserPromptSubmit`, `PreToolUse`, `PostToolUse`, `PostToolUseFailure`, `PostToolBatch`, `PermissionRequest`, `PermissionDenied`, `SubagentStart`, `SubagentStop`, `PreCompact`, `PostCompact`, `Stop`, `StopFailure` and `SessionEnd`, with this handler:

```
{"type": "command", "command": "curl", "args": ["-q", "--noproxy", "*", "-s", "-m", "2", "-o", "/dev/null", "-H", "@/home/alice/.simplex-mcp/my-agent/hook-headers", "-H", "Content-Type: application/json", "--data-binary", "@-", "http://127.0.0.1:5226/hook"], "async": true}
```

- Claude Code starts curl without a shell, passes it each event on stdin and continues without waiting for it. curl posts the event to the agent's server, with a limit of 2 seconds, and writes the answer to `/dev/null`, or to `NUL` on Windows.
- With `-q`, curl reads no `.curlrc` file, and with `--noproxy *` it ignores the proxy variables of the environment. curl reads the token from `hook-headers` in the data folder at each post; the settings file contains only the path of that file.
- Before it adds the hooks, `register` removes every handler with this data folder's `hook-headers` from the groups of these events, and the groups left empty. Other settings and hooks, the hooks of other agents included, are kept. The file is written again with 2-space indentation.
- In an interactive session, Claude Code runs these hooks only in a folder whose trust dialog was accepted.

Errors:

```
situation                  message
never started              Agent "my-agent" was never started. Start it with: npx -y @simplex-chat/mcp serve my-agent
claude not on PATH         Add this entry under "mcpServers" in ~/.claude.json: {"simplex-my-agent": {...}}
                           (--scope local: under "projects" > "<project folder>" > "mcpServers")
invalid settings file      Added to Claude Code: scope user, server simplex-my-agent. /home/alice/.claude/settings.json is not a
                           valid settings file; add these status hooks to it: {"hooks":{...}}
```

A settings file is invalid when it is not a JSON object, when its `hooks` is neither an object nor `null`, or when the value of one of the 14 events above in `hooks` is not an array; `register` then leaves it unchanged. At each of these errors `register` exits with code 1.

The token is created at the agent's first start and saved in its data folder; every start writes it to `headers.json` and `hook-headers`. `serve my-agent --new-token` replaces it, and Claude Code sends the new token from its next connection, and curl from its next post, without a new `register`.

## Approval modes

`--approve MODE` sets what the owner approves. It is set only by this option.

| Mode | The agent's actions: connect, send, download, leave | Messages to the agent |
|---|---|---|
| `requests` (default) | Performed after `/approve N`, or `/receive N` for a download; a send to a chat approved for all messages at once, with a copy posted in the agent control chat; a download from a chat approved for all files at once, with no post, when the file is on the agent's XFTP servers. | Passed to the agent. |
| `host` | Performed at once; a copy is posted in the agent control chat, except for downloads. A download of a file on other servers fails. | Passed to the agent. |

Claude Code's own permission check applies to every tool call too. In mode `requests`, add the agent's server, for example `"mcp__simplex-my-agent"`, to `permissions.allow` in `~/.claude/settings.json`; otherwise Claude Code prompts for each call, or in auto mode its classifier decides it.

In `host` mode, approval is left to the MCP host. The server marks `connect`, `send`, `download` and `leave` with `_meta["anthropic/requiresUserInteraction"]: true`, so Claude Code shows its permission prompt on every call, in `acceptEdits`, `auto` and `bypassPermissions` modes as well, and allow rules do not skip it. In `dontAsk` mode and in `claude -p` runs Claude Code denies these calls.

## Expiry

A pending action expires after `--expire HOURS`, 23 by default and at most: SimpleX allows edits of a message for 24 hours, and the outcome of a pending action is an edit of its card. Cards don't show the time; `/pending` does. The last line of its card is edited to `Expired.`, and the agent's call returns `{"action": N, "status": "expired"}`. For the same reason, a `connect` waits for the other side at most until 23 hours 50 minutes after the action was created.

A result that no call has returned is dropped after the action's expiry time; a repeated call after that creates a new action. At most 20 actions are pending at once; a further `connect`, `send`, `download` or `leave` fails until the owner decides.

Actions last only while the server runs. When the server stops, the last line of each pending card is edited to `Cancelled: the server stopped.`, and each status message is deleted. The server waits up to 10 seconds, `--stop-wait`, for these edits and deletions to be sent: it prints `Stopping: sending 2 cancelled cards and 1 status deletion, up to 10 s. Ctrl-C to stop now.`, and a second Ctrl-C ends the wait. After a restart, `/approve` of an earlier number replies `Action N is not pending.`, and the agent's repeated call posts a new card. Action numbers continue across restarts.

## What the owner sees

All of it is in the agent control chat, posted by the agent's profile, except the status messages, which are in the chats where the agent takes prompts ([Status while the agent works](#status-while-the-agent-works)). The agent control chat has the agent's display name; to see it under another name in your chat list, set an alias for the chat in the app. Names that are valid display names are shown plain. All other text written by the agent or by peers, other names included, is shown as code, line by line; names and errors take one line. Commands and SimpleX links inside it cannot be tapped: a valid display name cannot contain `/`, and the app parses each line of a message separately.

```
My Agent: I want to connect to bot SimpleX Directory (@directory.simplex), as myself.
          `Find groups, channels, businesses and bots.`
          /approve 1 or /reject 1

My Agent: I want to send a message to business Ridgeline, where I am incognito:
          `Is the Talus Trail waterproof?`
          /approve #9 4, /approve #9 all or /reject #9 4

My Agent: I want to leave business Ridgeline (#9).
          /approve #9 12 or /reject #9 12

My Agent: I want to download a file from Alice in group Builders (#3):
          `report.pdf`, 2.4 MB
          Servers: `xftp.example.com`, unknown: downloading shows it this computer's IP address.
          /receive #3 7, /forward #3 7 or /reject #3 7
```

The download card of an image or a video shows the sender's preview. Its servers line says `Servers: known.` when the file is on the agent's XFTP servers; otherwise it names the other servers, which see this computer's IP address during the download, and the card leaves out `/receive #3 7 all`. The agent waits until the sender finishes the upload before it asks, because the file's servers are known only then. A file without a file description is not downloaded: no card appears, and the agent's call ends with an error. A send with a file names the file's type, full path and size after the text, and shows an image's or video's preview:

```
My Agent: I want to send a message to group Builders:
          `The trail map.`
          attaching image `/home/alice/trail.jpg`, 3.1 MB
          /approve #3 8, /approve #3 all or /reject #3 8
```

When the agent asks to send a private key or a file of the server's data folder, the server refuses it and posts ``Refused to send `/home/alice/.ssh/id_ed25519` to group Builders (#3): private keys and the server's own files are not sent.`` When the upload of a sent file fails, the agent posts, for example, ``The file `trail.jpg` was not sent to group Builders (#3): `File server error: Server address is incompatible with network settings.`.``; both lines are logged too.

After the decision the card is edited, and its commands removed:

```
My Agent: I sent a message to business Ridgeline, where I am incognito:
          `Is the Talus Trail waterproof?`
          Approved by Alice.
```

Commands:

| Command | Effect |
|---|---|
| `/approve N`, `/approve #9 N` | Performs a connect, send or leave action N; the cards of sends and leaves name the chat. |
| `/reject N`, `/reject #9 N` | Rejects action N. |
| `/approve #9 all` | Approves all the agent's messages to chat #9: the pending ones and later ones. |
| `/revoke #9` | Ends that approval: each message to #9 needs approval again. |
| `/receive #3 7` | The agent receives the file of download 7: the server downloads it, and the agent's call returns its path. |
| `/receive #3 7 all` | The same, and the agent receives its later downloads from chat #3 of files on its XFTP servers without asking. Other pending downloads of #3 stay pending. |
| `/receive #3 off` | Ends that: each download from #3 needs approval again. |
| `/forward #3 7` | Downloads the file of action 7 and sends it to you in the agent control chat, as a reply to the card when the file's preview and the card fit in one message. The card stays pending, and the agent gets nothing: its messages show the file as offered, without a path. The server keeps the file outside the chat's folder until `/receive #3 7`, which then uses it. |
| `/leave #9`, `/leave @4` | Makes the agent leave group #9, or delete contact @4, at once, in both approval modes, and rejects the agent's pending actions for that chat. The reply, for example `Left business Ridgeline (#9), requested by Alice.`, is logged too. |
| `/delete #9 files` | Cancels the running downloads from chat #9, which then fail, also those the core continues after a restart, and deletes the chat's files in the data folder, forwarded ones included, for a current or a left chat: `Deleted 3 files from #9, 2.1 MB.` or `No files from #9.` |
| `/pending` | Lists pending actions. |
| `/list` | Lists the agent's 100 most recent chats, one line each, and which of them are approved for all messages or all files: `, approved for this chat by NAME` and `, files approved by NAME`; a last line counts the older chats. |

After `/leave #9`, or after the agent's `leave` of #9 is performed in either mode, when the chat's folder has files, the agent posts ``Files from business Ridgeline (#9) are kept in `/home/alice/.simplex-mcp/my-agent/files/group-9`: 3 files, 2.1 MB. /delete #9 files``. The files forwarded from #9 are deleted when the agent leaves it.

`/pending`, `/list` and `/leave` are in the chat's command menu, and the cards carry the others. Tapping `/leave` in the menu puts `/leave <chat ID>` into the entry field: replace `<chat ID>` with the chat's ID from `/list`, such as `#9`, and send it. Each command includes its number, because the app sends a tapped command as a new message, without a quote. Every card form and reply is listed in [`PLAN.md`](PLAN.md), section "What the owner sees: the agent control chat".

### Approval of all messages to a chat

After `/approve #9 all`, the agent's messages to #9 are sent at once, and each one is posted in the agent control chat after it is sent:

```
My Agent: I sent a message to business Ridgeline, where I am incognito:
          `Thanks, I'll order.`
          Approved for this chat by Alice. /revoke #9
```

- It covers messages, status messages and notes only: connecting and leaving are approved one by one.
- It lasts until `/revoke`, until the chat is deleted, or until the approving owner is no longer an approver. It survives restarts: it is kept in the agent's database, as custom data of that chat.
- With it, the agent can send anything it can read, without asking, to everyone in that chat.
- It applies in mode `requests` only: in mode `host`, `list` and `/list` show no approvals.

### Receiving all files from a chat

After `/receive #3 7 all`, the agent's later downloads from #3 are performed at once, and nothing is posted in the agent control chat.

- It covers files on the agent's XFTP servers. A download of a file on other servers gets a card, as without it.
- Cards already pending in #3, other than card 7, stay pending. A file you rejected, or whose card expired, gets a card again.
- It covers downloads only, and the approval of messages covers no downloads: each is given and ended by its own command.
- It lasts as the approval of messages does, until `/receive #3 off`, and is kept next to it in the chat's custom data.
- `/list` shows it as `, files approved by Alice`.

## What the agent sees

Six tools:

| Tool | What it does |
|---|---|
| `connect` | Joins a contact, bot, business, group or channel by a link or a SimpleX name, as the agent's profile or incognito. Returns the chat, its profile, welcome message, the commands a bot advertises, and the messages so far. |
| `send` | Sends a message, with a file of this computer when asked, and returns every message received in that chat after it, until none arrives for `idle` seconds (30 by default). A directory bot's search results arrive this way. With `live: true`, it shows a note in the chat's status message at once, without an action. |
| `receive` | Returns the chat's unread messages and those that arrive while it waits, waiting for one when asked. With `typing: true`, it starts the chat's status message when it returns messages. |
| `download` | Downloads the file of a received message to the data folder and returns its full path. |
| `list` | Lists the agent's chats with unread counts and `approved: true` for chats approved for all messages, and its actions without a result. |
| `leave` | Deletes a contact, or leaves a group, business chat or channel, and deletes the chat; the contact or the members are notified. |

A `send` to a chat that `list` shows with `connected: false` is refused before any card. A `send` is refused with `text: too long for its approval card; shorten it.` when its card, in its longest outcome form, would exceed 15000 bytes in the SimpleX message, which holds at most 15602; a text of many lines or multi-byte characters reaches it below 12000 characters.

A received message with a file has a `file` field: its name, size, type (image, video, voice or file) and status. Status `offered` means `download` can fetch it; `received` comes with the path of the file that `download` downloaded; a file `/forward` sent to you stays `offered`; `rejected` means you rejected its download, or it expired; `failed` comes with the error, also for a file above the size its sender may send, and for a received file that is not in the chat's folder, for example after `/delete #3 files`. A downloaded file is named by its file ID and the extension of its name, when the extension has only letters and digits: the name the sender gave stays in the `name` field. `download` first waits, up to `wait` seconds, for the sender to finish the upload; when it has not finished by then, the call returns an error that asks to call again later. A `download` that runs past its `wait` after the approval returns status `performing`, and the same call again picks it up. A download fails when the sender cancels the file, when the message or its chat is deleted, or when no part of the file arrives for 30 minutes; the server then cancels it.

A short voice message can arrive with its file already received. The server moves such a file into the chat's folder under the same naming when it is received in full, and its message shows it as `offered`; `download` returns its path there after the approval. A partial file of a download, or of a short voice message that arrives while the server runs, is deleted when its transfer fails or is cancelled.

`send` with `file`, an absolute path, sends that file with `text` as its caption:
- `.jpg`, `.jpeg` and `.png` as an image, resized by the SimpleX core to the size the apps send, with a preview the core makes;
- `.gif` and `.webp` as an image, unchanged, with a gray preview;
- `.mp4` and `.mov` as a video, unchanged, with the first frame as its preview and its duration when `ffmpeg` and `ffprobe` are installed, otherwise a gray preview;
- other files as a file, unchanged; `as: "file"` sends an image or video this way too.

An empty file is refused, and so is `as` without `file`. A message holds at most 15602 bytes with its text, the file's name and preview, the message `reply_to` quotes and the mentioned names. The quote is the quoted text when the message is an image or video; otherwise it is the quoted content, an image's preview included. A `send` over it is refused with `text: too long for a message with this image; shorten it.`, or without a file `text: too long for the message with its quote; shorten it.` An image's preview takes up to 14000 bytes, so its text has about 1 KB.

The server checks the file by its real path, after symbolic links, and refuses files under `~/.ssh` and `~/.gnupg`, files named `id_*`, `*.pem`, `*.key`, `*.p12` and `*.pfx`, and the files under its data folder and under `~/.simplex-mcp`, except those under `files/` of its own data folder; each folder is taken by its real path too. The card shows the real path. A file whose real path, size, inode or change time differs at the send from the request is not sent: the action fails.

Incognito: the core creates a random profile for that one chat, so peers of different chats can't link them. The tools' inputs, results and errors are in [`PLAN.md`](PLAN.md), section "What the agent sees: tools", and the call sequences of the scenarios in section "Sequence diagrams".

## Status while the agent works

In mode `requests`, the server shows what the agent does in a live message, the status message, in a chat approved for all messages where the agent can post: a chat with a person, a group or a business chat. Channels and chats with bots and agents are excluded. In a group, every member sees the status message and its notes.

- The server posts the status message when messages are returned by a `receive` with `typing: true`, unless the chat has one.
- From a `receive` of a chat with `typing: true` until a `receive` of it without `typing`, the server also posts the status message at once when it receives a message there, unless the chat has one, with the label `received, waiting for the agent`: the owner sees at once that the server runs. When messages of that chat are returned by a `receive` with `typing: true` or by a `send`, the label is changed to `thinking` at once.
- With `live: true`, the text of a `send`, a note, is shown under the status line at once, and a status message is posted when the chat has none. Such a call is not an action: `status: live` is returned at once. A note with `file`, `reply_to` or `mention`, a note above 2000 characters, and a note to another chat are refused. In the description of `receive`, the agent is asked to call `send` with `live: true` and a short note in the same response as the tool call of a step that takes minutes.
- The agent's next message to that chat without `live` is sent as a new message, and the status message is then deleted for everyone.

```
My Agent: ◑ using download · 0:30
          Reading the file…
```

The apps show it under the header `LIVE`. The first line is the status line: a frame, turned at each update, a label, and the time since the start. From the 50th version, about 10 minutes after the start, the line is `◌ working since 2026-11-10 12:00 UTC`, or `◌ received, waiting for the agent since 2026-11-10 12:00 UTC` while the agent has not taken the message. A note is shown under the line until the next note.

| Label, the first that applies | When |
|---|---|
| `received, waiting for the agent` | from a message received in the chat without a status message, after a `receive` there with `typing: true`, until the agent takes the message: messages of the chat are returned by a `receive` with `typing: true` or by a `send` |
| `loading MODEL` | `--inference`: a model starts, or none is ready while one stops |
| `reading the prompt with MODEL, N tokens read` | `--inference`: a model reads the prompt |
| `generating with MODEL, N tokens/s` | `--inference`: a model generates |
| `waiting for approval` | the agent waits in a call for your decision on a card |
| `waiting for permission in Claude Code` | from Claude Code's permission prompt to the end of a tool call, a batch or the turn of its session ([Status hooks](#status-hooks)), so also during the allowed call |
| `using TOOL` | the agent runs `connect`, `send`, `download`, `list` or `leave`; TOOL is the latest of them |
| `compacting the conversation` | Claude Code compacts the conversation |
| `running a command`, `reading FILE`, `editing FILE`, `searching files`, `searching the web` or `using NAME` | Claude Code runs a tool; the latest running tool is named, as the next table shows |
| `running subagent TYPE` | a subagent of Claude Code runs; TYPE is the latest one's type |
| `working` | `--inference`: the models are idle |
| `thinking` | otherwise, in the first version of other status messages, and at once when the agent takes the message |

MODEL, shown as code, is the model that starts, or the one model of all busy slots of llama-swap. Otherwise, and for a plain llama-server, `with MODEL` is left out, and `loading MODEL` becomes `loading the model`. `, N tokens read` is left out while N is 0, and `, N tokens/s` until two polls during the generation of one task. `waiting for approval` and `using TOOL` are computed from the agent's calls in all its chats.

| Claude Code tool | Label |
|---|---|
| `Bash`, `PowerShell`, `Monitor` | `running a command` |
| `Read` | `reading FILE` |
| `Edit`, `Write`, `MultiEdit`, `NotebookEdit` | `editing FILE` |
| `Grep`, `Glob` | `searching files` |
| `WebSearch`, `WebFetch` | `searching the web` |
| another tool, except `Agent` and the agent's own tools | `using NAME` |

The labels of Claude Code's work are computed from the events of the [status hooks](#status-hooks), in all sessions the server follows. FILE is the file's name without its folder; NAME is the tool's name, without `mcp__SERVER__` for a tool of another MCP server; TYPE is the subagent's type. Each is shown as code. When a name is filtered out, the label is `reading a file`, `editing a file`, `using a tool` or `running a subagent`.

The status line is updated 3 seconds apart at first, then at a tenth of its age, at most 20 seconds apart. From the 50th version, the server checks the chat every 5 minutes, without updates. Notes are shown at once up to the 59th version; a later note is refused. With its deletion, at most 60 messages per status message are sent to each recipient.

The server deletes the status message for everyone:
- after the agent's next message to that chat without `live` is sent;
- when the agent starts to wait in a `receive` of that chat;
- when the agent leaves the chat, and at `/leave`;
- at the next update or check once the conditions above are no longer met in the chat, for example after `/revoke`, or after the agent loses its control chat;
- 23 hours after the start, at the next check or note; a note at that age is shown in a new status message;
- from the 50th version, 30 minutes after the last start or end of one of the agent's calls, at the next check, while the agent runs no call other than `receive`;
- at an error of a command for the status message, with `status of CHAT: ERROR` logged;
- when the server stops.

At the start, before it accepts calls, the server deletes its own live messages younger than 29 hours 50 minutes in the chats where these conditions are met: the status messages left by a crash.

A deleted status message is shown as marked deleted, with its last text, unless full deletion is allowed:
- in a direct chat, set "Delete for everyone" to "always" in the agent contact's preferences; "yes" is not enough, because the agent's own preference is "no";
- in a group or a business chat, turn "Delete for everyone" on in the chat's preferences; the status message is then removed for every member.

Notifications, by each recipient's apps:
- the status message is notified as a new message;
- its updates are notified only in a chat profile other than the active one, by the Android and desktop apps, once per update;
- the answer is notified as a new message, and the deletion after it is not notified;
- at a deletion without an answer, while the status message is the chat's last message, the Android app replaces a shown notification of that chat with "deleted" or "marked deleted", in the active chat profile.

### Inference endpoint

`--inference URL` names llama-swap, or a llama-server with one model; the state of its models is then shown in the labels.

- URL: http or https, without user name and password; trailing slashes, query and hash are dropped. Another URL, and `--inference` with `--approve host`, are usage errors, exit code 2.
- `SIMPLEX_MCP_INFERENCE_KEY`, when set, is sent as `Authorization: Bearer KEY` to URL only. Only visible ASCII characters are accepted in it; with another key, the server exits with `SIMPLEX_MCP_INFERENCE_KEY: visible ASCII characters expected.`, exit code 2.
- The line `Inference endpoint: URL` is printed at the start, after `Approval mode: requests`.
- At each timed update of the status line other than the first and the frozen one, the server reads `URL/running` of llama-swap, then `/props` and `/slots` of up to 4 servers of its ready models, on URL's host, or on loopback when URL is loopback. When `/running` answers 404, it reads `URL/props` and `URL/slots` of llama-server.
- The result of one poll is shared by all status messages for 3 seconds. A poll is cancelled after 1 second; an answer over 1 MiB and a redirect are refused.
- A failed poll is logged once on stderr as `inference endpoint: ERROR`, until the next successful poll; the labels are then those without `--inference`.

### Status hooks

With the hooks that `register` adds ([Add the agent to Claude Code](#add-the-agent-to-claude-code)), Claude Code passes its events to curl, which posts them to `http://127.0.0.1:PORT/hook` of the agent's server, with the token.

- The server follows a session of Claude Code from its first event with a tool of this agent, such as `mcp__simplex-my-agent__receive`, and ignores the events of other sessions. It follows at most 16 sessions, and drops the oldest one with its state.
- Of each event, the server uses only `hook_event_name`, `session_id`, `tool_name`, `tool_use_id`, `agent_id`, `agent_type`, and the base name of `tool_input.file_path` or `tool_input.notebook_path`. The rest of the event, prompts, commands, file contents and tool results included, is discarded after parsing; nothing of it is saved or logged.
- A file's base name is kept only when it consists of 1 to 64 ASCII letters, digits and characters of `_.+@-`. Tool names and subagent types follow the same rule, with `_` and `-` as the only characters besides ASCII letters and digits.
- At most 64 running tools and 64 running subagents are kept, and 256 IDs of ended tool calls; the oldest is dropped.

| Event of a followed session | State of the session after it |
|---|---|
| `PreToolUse` | its tool running, except `Agent`, the agent's own tools and a tool whose end event was received first; without `agent_id`, compaction ended |
| `PostToolUse`, `PostToolUseFailure`, `PermissionDenied` | its tool and the permission prompt ended |
| `PostToolBatch` | the tools of its agent and the permission prompt ended |
| `PermissionRequest` | a permission prompt open |
| `SubagentStart` | its subagent running |
| `SubagentStop` | its subagent ended |
| `PreCompact` | compaction running |
| `PostCompact` | compaction ended |
| `UserPromptSubmit`, `Stop`, `StopFailure`, `SessionEnd` | all its tools, subagents, permission prompt and compaction ended |

- Claude Code runs curl in the background and hides the outcome outside verbose mode (Ctrl+O), so the failures of the posts while the server is stopped are hidden too.
- The server checks the token, `Host` and `Origin` of a request to `/hook` as for `/mcp`. It answers a `POST` with 204, or with 411 without Content-Length, 413 for a body over 4 MiB and 400 for invalid JSON, and another method with 405.

## Waiting for approval in Claude Code

An MCP server cannot start a turn of the agent: what the agent reads from the server are the results of its own tool calls. A call that waits for the owner, or for responses, stays open, and Claude Code turns it into a background task:

- A tool call in the main conversation that runs past two minutes moves to a background task. Claude continues working, and the result arrives as a task notification. When it arrives while the session is idle, Claude responds to it without a prompt (checked with Claude Code 2.1.300).
- To move calls to the background sooner, set `CLAUDE_CODE_MCP_AUTO_BACKGROUND_MS`, for example to `10000`, in the `env` block of `~/.claude/settings.json`. This applies to every MCP server.
- Calls from subagents, and calls in `claude -p` runs unless `CLAUDE_AUTO_BACKGROUND_TASKS=1`, stay in the foreground: the subagent or the run waits.
- A background task ends with the session. The action stays in the server while it runs; `list` shows it, and the same call again picks it up.

## Several agents

Each agent is its own process, with its own data folder, port, agent control chat, Claude Code server and status hooks:

```
npx -y @simplex-chat/mcp serve my-agent-2 --port 5227
npx -y @simplex-chat/mcp register my-agent-2
```

## Other MCP hosts

Add an entry with the URL `http://127.0.0.1:5226/mcp` and the header `Authorization: Bearer <token>`, as in `headers.json` of the data folder; after `--new-token`, update the entry. A host that ends a tool call early returns a timeout to the agent; the agent repeats the call with the same arguments, which picks up the same action. Hosts with a fixed tool-call limit, between 60 and 300 seconds in several hosts, need `wait` below it.

## Recovery

| Situation | What to do |
|---|---|
| The server was not running when Claude Code started | Start the server, then run `/mcp reconnect all` in Claude Code (2.1.284 or later). |
| The server restarted while Claude Code was running | Claude Code reconnects up to five times within about 30 seconds. After that, in an interactive session, run `/mcp reconnect all`; `claude -p` runs and Agent SDK sessions keep retrying for about half an hour. Open calls fail, and pending actions are cancelled; the agent's repeated call creates a new request. |
| A second server was started on the same data folder | It exits with `Another server uses <folder> (PID N). If no server runs, remove <folder>/lock.` |
| Every owner left the agent control chat or was removed | `connect`, `send`, `download` and `leave` fail. Without `--owners`, the server logs `No owners in the agent control chat.` and prints a new link; join with it. With an `--owners` list, the agent posts and the server logs `No owners remain: restart the server with --reset-owners --owners`. |
| The agent was removed from the agent control chat, the chat was deleted, or the agent lost its owner role | `connect`, `send`, `download` and `leave` fail, and the server logs `The agent lost its control chat. Restart the server with --reset-owners.` |
| The QR code does not scan | Start with `--qr-code large`, or paste the link into the app. |
| The wrong person joined by the link | Start with `--reset-owners`. |
| The token leaked | Start with `--new-token`; Claude Code reads the new token at its next connection. |

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
                           requests  the agent's connections, messages, downloads
                                     and leaving chats (default)
                           host      nothing; the MCP host asks
  --owners [ID:NAME,...] only these owners approve; without a list, the link
                         adds an observer and its ID is printed at the join.
                         Without --owners, whoever joins becomes an owner and
                         every owner approves.
  --expire HOURS         pending actions expire after HOURS, 1 to 23 (default: 23)
  --stop-wait SECONDS    at stop, wait SECONDS for cancelled cards and status
                         deletions to be sent, 0 to 60 (default: 10)
  --reset-owners         replace the owners: a new agent control chat and link
  --new-token            replace the access token
  --port N               HTTP port on 127.0.0.1 (default: 5226)
  --qr-code SIZE         QR code of the link: small (default) or large
  --inference URL        poll llama-swap or llama-server at URL for the model
                         state and tokens per second shown in status messages;
                         the API key, if any, is read from
                         SIMPLEX_MCP_INFERENCE_KEY; mode requests only

register options:
  --scope SCOPE          user (default) or local
  --data DIR             data folder (default: ~/.simplex-mcp/NAME)

--reset-owners takes --owners without a list.
```

## Data folder

```
~/.simplex-mcp/my-agent/
  simplex_chat.db    SimpleX chat database: the agent's profile and chats
  simplex_agent.db   SimpleX agent database: connections and keys
  token              access token, readable only by the user
  headers.json       the token as the Authorization header for Claude Code, readable only by the user
  hook-headers       the token as the Authorization header line for curl in the status hooks, readable only by the user
  server.json        name, port and expiry of the server, read by register
  lock               PID of the running server
  tmp/               the SimpleX core's work folder for files in transfer
  files/group-3/     files downloaded from chat #3, and files received there without a download, each named by its file ID
                     and the extension of its name, such as 12.pdf; files/contact-4/ for contact @4
  files/sent/        resized copies of the images the agent sends, and copies of the files /forward sends you under their names;
                     each is deleted when its upload completes or fails, and all at start
  forwarded/group-3/ files /forward downloaded from chat #3, until /receive moves them into files/group-3/; deleted when the agent
                     leaves the chat, and by /delete #3 files
```

The folder is readable only by the user: every start sets its mode to 0700, except on Windows.

The server keeps its action counter in the chat database, as custom data of the agent control chat, the approvals of all messages to a chat and of all files from it as custom data of that chat, and its actions in memory.

The server keeps no log file: it writes its log to the terminal where `serve` runs, events to stdout and errors to stderr. Each note of a status message is written to stdout, for example `Note to contact Alice (@4): Reading the file…`.

One server runs per data folder: a second one exits, because two processes on one database break each other's connections (a SimpleX server delivers a queue's messages only to the last process that subscribed to it).

## Security

- The server listens only on 127.0.0.1. Every request needs the token. Requests from web pages on other hosts, and requests with another `Host`, are refused (the SDK's localhost Origin and Host checks).
- Any process with the token can act as the agent. The token is stored only in the data folder, in `token`, `headers.json` and `hook-headers`; Claude Code's config and the `claude mcp add-json` command line contain the command that prints `headers.json`, and the status hooks the path of `hook-headers`, not the token.
- Names written by peers are folded to one line, with control and format characters replaced by spaces, in the terminal and in the agent control chat; there, a name that is not a valid display name is shown as code.
- Without `--owners`, whoever opens the link first becomes an owner. The link works once.
- With `--owners`, only the listed owners approve.
- After `/approve #N all`, the agent sends to that chat without asking, files included; `/revoke #N` ends it.
- In such a chat, the status message and its notes are sent without asking and without a copy in the agent control chat; each note is written to the terminal. Everyone in the chat sees in the status line the names of the agent's running tools and of its model, the names of the files Claude Code reads and edits, without their folders, and the names of its other tools and subagent types, also for its work in other chats.
- With the status hooks in scope `user`, Claude Code passes the events of every session of the user to curl, which posts them to the agent's server, with prompts, commands, file contents and tool results. The server uses only the fields named in [Status hooks](#status-hooks), keeps state only for the sessions that used the agent's tools, and logs nothing. curl ignores `.curlrc` files and proxy variables, so the events and the token are sent only to 127.0.0.1.
- The server refuses to send private keys, its own files and the files under `~/.simplex-mcp`, where agents keep their data, except the files under `files/` of its own data folder; other files the agent can read are sent after approval.
- After `/receive #N 7 all`, the agent's later downloads from that chat, of files on its XFTP servers, run without asking; `/receive #N off` ends it. A file on other servers is downloaded only by `/receive` or `/forward` of the card that names them; any other download of it fails, unless the IP address is protected.
- Peers of a chat connected as the agent see its profile; peers of an incognito chat see a random profile made for that chat only.
- The agent has no address, so nobody can contact it first.
- The agent uses private message routing: the SMP server that holds a contact's queue does not see the agent's IP address.
- `SIMPLEX_MCP_INFERENCE_KEY` is sent only to the `--inference` URL; set it only for a URL on 127.0.0.1 or with https. The server reads the servers of llama-swap's models only on the URL's host, or on loopback when the URL is loopback, and refuses redirects. It shows a model ID only when the ID consists of 1 to 64 ASCII letters, digits and characters of `_.:/+@-`, and then as code; it folds the endpoint's error texts to one line.
