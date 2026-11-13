# Testing @simplex-chat/mcp v0.3

Node.js 22. Claude Code runs on the same computer as `serve`: the server listens on 127.0.0.1 only.

## Build

### Here

All commands run in `/code/simplex-chat-6/packages/simplex-chat-mcp`. The dependencies are installed by `npm install`; `simplex-chat` comes from npm and downloads libsimplex and its Node.js add-on to `~/.cache/simplex-chat` at the first start.

```
node node_modules/typescript/bin/tsc
node node_modules/jest/bin/jest.js test/unit     # about 10 s
SMP_SERVER=/code/steward/node_modules/.cache/smp-server/smp-server-v7.1.0-beta.3 XFTP_SERVER=/code/simplexmq-5/dist-newstyle/build/x86_64-linux/ghc-9.6.3/simplexmq-7.1.0.9/x/xftp-server/build/xftp-server/xftp-server node node_modules/jest/bin/jest.js test/e2e      # about 4 minutes; a local smp-server and xftp-server on free ports
```

`node_modules` is not writable here, so the end-to-end test takes the smp-server and xftp-server binaries from `SMP_SERVER` and `XFTP_SERVER` instead of downloading them to `node_modules/.cache`.

### On your own machine

1. Check out simplex-chat master; the package is `packages/simplex-chat-mcp`.
2. In that folder: `npm ci`, then `npx tsc`. `tsc` leaves the output of deleted sources in `dist`. `npm pack` and `npm publish` delete `dist` and build it again (script `prepack`). They bundle the production packages of `node_modules` (`bundleDependencies`), without a lockfile. Run `npm ci` before a pack, so that the tree of `package-lock.json` is in `node_modules`, as in the publication by `build.yml`.
3. `node dist/cli.js serve my-agent --display-name "My Agent"`. At the first start, `simplex-chat` downloads libsimplex and its Node.js add-on, with a `Downloading ...` line each on stderr.
4. Tests: `npx jest test/unit`; `npx jest test/e2e` downloads smp-server and xftp-server v7.1.0-beta.3 on Linux, and elsewhere takes `SMP_SERVER=/path/to/smp-server XFTP_SERVER=/path/to/xftp-server`.

Until the package is published, the printed hint `npx -y @simplex-chat/mcp register my-agent` stands for `node dist/cli.js register my-agent`.

### The tests of 0.2

`serve` keeps the behaviour of 0.2, except the `reply_to` of a message: it has the quoted message's sender, type and text, and is present also without an ID. The 274 unit tests of 0.2 and its 13 end-to-end tests in `scenarios` and `mcp`, from commit be668564d, pass unchanged on this source, except one expectation: `reply_to` in the unit test "describes a group message" is now `{chat: "#15", item: 30, text: ""}`. They run in a copy of the package with the `test` folder of 0.2. Run these commands from the repository's root, with `SMP_SERVER` and `XFTP_SERVER` set as above:

```
mkdir -p /tmp/v02
git archive HEAD packages/simplex-chat-mcp | tar -x -C /tmp/v02
rm -rf /tmp/v02/packages/simplex-chat-mcp/test
git archive be668564d packages/simplex-chat-mcp/test | tar -x -C /tmp/v02
ln -s "$PWD/packages/simplex-chat-mcp/node_modules" /tmp/v02/packages/simplex-chat-mcp/node_modules
cd /tmp/v02/packages/simplex-chat-mcp
node node_modules/jest/bin/jest.js test/unit test/e2e/scenarios.test.ts test/e2e/mcp.test.ts
```

## Start, join, register

1. `node dist/cli.js serve my-agent --display-name "My Agent"`. NAME, here `my-agent`, names the data folder and the Claude Code entry; `--display-name` is the name people see in SimpleX. It prints the data folder (`~/.simplex-mcp/my-agent`), creates the agent control chat "My Agent", and prints a QR code and a link that works once. For a short link the QR code has 33 modules and takes 37 columns × 19 lines; `--qr-code large` prints it at 74 × 37. Keep it running; Ctrl-C stops it. `node dist/cli.js --help` lists the options.
2. In the SimpleX app, scan the QR code, or paste the link into "Connect via link". If the code does not scan, restart with `--qr-code large`. The chat "My Agent" appears with `I'm your agent. I ask here before I connect, send, download a file or leave a chat.` The terminal prints `Alice joined (member N) and is now an owner. The link is deleted.` and the register hint.
3. `node dist/cli.js register my-agent`. It prints `Added to Claude Code: scope user, server simplex-my-agent.` Run it in a terminal with the same `CLAUDE_CONFIG_DIR` as the one where `claude` runs: `claude mcp add-json` writes to the config of its own environment.
4. Add `"mcp__simplex-my-agent"` to `permissions.allow` in `~/.claude/settings.json`, so Claude Code does not prompt for each call; approval happens in the SimpleX app.
5. Start `claude`. `/mcp` shows `simplex-my-agent` connected with 6 tools. A session started before `register` does not load the entry; start a new one.

## Try the tools

Prompts for Claude Code, one at a time. A call that waits more than 2 minutes, for example for an approval, moves to a background task, and its result arrives as a notification.

By name, first:

- `Connect to @directory.simplex as yourself.` While the name is not registered: the tool error `Name not found: This SimpleX name is not registered. Please check the name.`, and no card. Once it is registered: the card `I want to connect to bot SimpleX Directory (@directory.simplex), as myself.`; after the approval, step 1 below returns `status: known`, with no card.

The directory by its address (`docs/DIRECTORY.md`):

1. `Connect to the SimpleX Directory as yourself at https://smp4.simplex.im/a#lXUjJW5vHYQzoLYgmi8GbxkGP41_kjefFvBrdwg-0Ok and tell me which commands it advertises.`
   Card: `I want to connect to bot SimpleX Directory, as myself.` Tap `/approve 1`. The card becomes `I connected to ...` with `Approved by Alice.`; Claude gets `status: done`, the chat `@N`, `commands` and the welcome message.
2. `Search the directory for hiking groups.` Card: `I want to send a message to bot SimpleX Directory:` with the text. Approve it; Claude gets the bot's answers in `responses`.
3. `Send /help to the directory.` A bot command is sent as text.
4. `Check for new messages from the directory, waiting up to 60 seconds.` `receive` with `wait`.
5. `List your chats and pending actions.` `list`.
6. Reject one card with its `/reject @N A`: the call returns `status: rejected`; the card ends with `Rejected by Alice.` `/pending` lists the waiting cards.

Approval of all messages to a chat, and leaving:

1. `Search the directory for Monero groups.` On the card, tap `/approve @N all`. The reply is `Approved for bot SimpleX Directory (@N) by Alice. /revoke @N`, and the search is sent.
2. `Send /help to the directory.` It is sent without a card; the agent control chat then gets `I sent a message to bot SimpleX Directory:`, `` `/help` `` and `Approved for this chat by Alice. /revoke @N`. `list` shows `approved: true` for the directory.
3. Tap `/list` in the command menu: one line per chat; the directory's line ends with `, approved for this chat by Alice`.
4. Tap `/revoke @N`: `Messages to bot SimpleX Directory (@N) need approval again.` The next send gets a card.
5. `Leave the directory.` Card: `I want to delete bot SimpleX Directory (@N).` Approve it: the call returns `status: done`, and `list` no longer shows the directory.
6. Join a group from the search results, then `Leave that group.` Card: `I want to leave group <name> (#N).` Approve it: the members see that the agent left.

A channel:

1. `Search the directory for channels about trail running.` Take a channel link from the answers.
2. `Connect to <the channel link> as yourself.` Card: `I want to connect to channel <name>, as myself.` Approve it; Claude gets `type: channel`, `role: observer` and the recent posts in `messages`.
3. `Wait up to 10 minutes for a new post in that channel.` `receive` with `wait` returns the next post.

Files, in a group of yours:

1. In the app, create a group and copy its link. `Connect to <the group link> as yourself.` Approve the card.
2. Send a photo to the group from the app. `Check for new messages in that group.` The message has `file` with `type: image` and `status: offered`.
3. `Download that photo.` Card: `I want to download a file from Alice in group <name> (#N):`, with the photo's preview, the file name, its size, and `Servers: known.` when the app uploaded it to servers the agent also has. For a large video asked for right after sending it, the call waits until the upload finishes, and only then the card appears.
4. Tap `/forward #N A`: the photo arrives in the agent control chat, as a reply to the card when its preview and the card fit in one message, and the card stays pending; `~/.simplex-mcp/my-agent/files/group-N/` has no file, and the photo is in `~/.simplex-mcp/my-agent/forwarded/group-N/`.
5. Tap `/receive #N A`: the call returns `status: done` and the path, `~/.simplex-mcp/my-agent/files/group-N/<file ID>.jpg`.
6. Send a document, `Download that file.`, and tap `/receive #N A all` on its card: the reply is `Files approved for group <name> (#N) by Alice. /receive #N off`, and the download completes. The next file downloads with no card; `/list` shows `, files approved by Alice`. Tap `/receive #N off`.
7. Send another file, `Download it.`, and tap `/reject #N A`: the call returns `status: rejected`.
8. `Send the photo /home/alice/trail.jpg to that group, with the text "trail map".` Card: `I want to send a message to group <name>:`, `` `trail map` `` and ``attaching image `/home/alice/trail.jpg`, SIZE``, with the photo's preview. Approve it: the group gets the photo, resized to at most 261120 bytes, with its preview.
9. `Send ~/.ssh/id_ed25519 to that group.` The call returns the error `file: /home/alice/.ssh/id_ed25519 is refused: ...`, and the agent control chat gets ``Refused to send `/home/alice/.ssh/id_ed25519` to group <name> (#N): ...``. A symbolic link to the key gets the same refusal, with the key's real path.
10. With `ffmpeg` installed, `Send /home/alice/ride.mp4 to that group.`: the card and the message show the video's first frame and its duration; without `ffmpeg`, a gray preview and duration 0.
11. Tap `/leave #N`: after the reply, the agent posts ``Files from group <name> (#N) are kept in `~/.simplex-mcp/my-agent/files/group-N`: K files, SIZE. /delete #N files``. Tap `/delete #N files`: `Deleted K files from #N, SIZE.`; again: `No files from #N.`

Incognito, as a second agent, because the first one already knows the directory:

1. `node dist/cli.js serve scout --display-name "Scout" --port 5227`; join its control chat "Scout" with the printed link; `node dist/cli.js register scout`; add `"mcp__simplex-scout"` to `permissions.allow`; `/mcp reconnect all`.
2. `Using simplex-scout, connect to the directory incognito: https://smp4.simplex.im/a#lXUjJW5vHYQzoLYgmi8GbxkGP41_kjefFvBrdwg-0Ok` Card: `I want to connect to bot SimpleX Directory, incognito.` Approve it; Claude gets `known_as`, a random name.
3. `Using simplex-scout, list your chats.` The directory has `incognito: true` and the same `known_as`. A send card for it says `where I am incognito`.

Stop and restart:

- Stop the server while a card waits: the terminal prints `Stopping: sending 1 cancelled card, up to 10 s. Ctrl-C to stop now.`, and the card ends with `Cancelled: the server stopped.` A second Ctrl-C during the wait stops the server at once. With no card waiting, Ctrl-C stops it at once. After a restart, `/approve` of that number replies `Action N is not pending.`, and the next card has the next number.
- A second `serve my-agent` on the same data folder exits with `Another server uses ... (PID N). If no server runs, remove .../lock.` A port in use exits with `Port 5226 is in use. Start with --port N.`, before any link is printed.
- Restart with `--new-token`, without `register`: after the reconnection, `list` returns the chats. Checked with `claude -p` and an `--mcp-config` entry with `headersHelper`, Claude Code 2.1.302, 2026-10-18.

## Claude Code checks

1. C2, protocol: passed with Claude Code 2.1.302 in the default runtime, with `MCP_PROTOCOL_NEGOTIATION=legacy` and with `MCP_SDK_GENERATION=v1`. To repeat it, write `c2.json` with `{"mcpServers": {"simplex-my-agent": {"type": "http", "url": "http://127.0.0.1:5226/mcp", "headersHelper": "cat <your home folder>/.simplex-mcp/my-agent/headers.json"}}}`, and run it once as is and once with each variable before it:
   ```
   claude -p "Call the list tool and show its result." --strict-mcp-config --mcp-config c2.json --allowedTools mcp__simplex-my-agent
   ```
   Each run shows the chats, with the preset contact "Ask SimpleX Team".
2. C3, subagent: `Use a subagent to send "trail shoes" to the directory.` While the card waits, the subagent's call stays in the foreground, with no background task; it returns after `/approve`.
3. C4, reconnect: restart `serve my-agent`; in the running session, `/mcp reconnect all`; `list` returns the chats.
4. C5, register again: `node dist/cli.js register my-agent` prints `Added to Claude Code: scope user, server simplex-my-agent.`; `claude mcp list` shows one `simplex-my-agent`.
5. C6, Esc: ask for a send, and press Esc while its card waits; `List your pending actions.` shows the action with `open: false`; the same request again attaches to it, and `/approve` returns the result to that call.
6. Host mode: restart with `node dist/cli.js serve my-agent --approve host`, then `/mcp reconnect all`. `Send /help to the directory.` makes Claude Code prompt, despite the allow rule; so does `leave`. After you allow it, the agent control chat gets `I sent a message to bot SimpleX Directory:`, `` `/help` `` and `Performed without approval (mode host).` A `connect` to a link not yet joined gets the same prompt and an outcome card; a target already joined returns `status: known`, with no card.

## Claude Code plugin

### Install the plugin before it is published

The MCP server and the SessionStart, UserPromptSubmit, Stop, PostModelSwitch, PostToolUse and PostToolUseFailure hooks, and the PreToolUse hook of every tool, run `node ${CLAUDE_PLUGIN_ROOT}/dist/cli.js`. The PreToolUse hook of the plugin's tools also runs `node ${CLAUDE_PLUGIN_ROOT}/plugin/allow.js`. Their modules are bundled in the package. Claude Code 2.1.322 installs a plugin's dependencies only when the plugin's folder has `bun.lock`, `npm-shrinkwrap.json`, `package-lock.json` or `bun.lockb`. The package has none of them, so Claude Code installs nothing. In step 1, a packed build is laid out in the same way. `test/e2e/launch.test.ts` checks that layout.

1. In `packages/simplex-chat-mcp`, install the tree of `package-lock.json`, pack the build, and unpack it into a new, empty folder:

   ```
   npm ci
   npm pack --pack-destination /tmp
   mkdir /tmp/simplex-plugin
   tar -xzf /tmp/simplex-chat-mcp-0.3.0.tgz -C /tmp/simplex-plugin --strip-components 1
   ```

2. Start Claude Code in any project folder, with the plugin and its channel, and with its own data folder:

   ```
   SIMPLEX_MCP_HOME=/tmp/simplex-mcp-test claude --plugin-dir /tmp/simplex-plugin --dangerously-load-development-channels plugin:simplex@inline
   ```

   `--plugin-dir` loads the plugin for that session only, as `simplex@inline`.

3. After a change in `src/`, delete `/tmp/simplex-plugin` and repeat step 1. Then stop the service, because a new service of the same version doesn't replace it: `kill $(cat /tmp/simplex-mcp-test/service.lock)`.

### Automated tests

- `npm test` runs the unit tests of the plugin's parts:
  - the session server and its channel;
  - the service;
  - the hub with its Main group, the sessions' groups and the tool-use cards;
  - the hooks;
  - the plugin files.

  In these tests, the Stop hook reads the model of its answer from the end of a sparse 3 GiB transcript. It finds the model even when the answer's entry is written after the hook started. The UserPromptSubmit hook marks a prompt as typed only when its record has the origin `human`. Task notifications, messages of other sessions and channel prompts are not marked. A typed prompt is marked also when the transcript is created with its entry 30 reads after the hook started, as after a user's own prompt hook of 3 s. The hook sends the one-line note of each kind of Claude Code's own prompt, a teammate's message included, also when idle notifications precede it in its prompt, and none for a typed or a channel prompt or idle notifications only. A note stays on one line when the sender's name has a line break or a control character. A prompt that reaches a running turn is found by its queued command or its own later entry, and it is marked mid-turn, also when the turn's first entry is out of the tail and only its tool results are in it. Its record is taken by its text, past a meta entry with the same prompt ID; a prompt with the text of the turn's own prompt waits while it is queued. A typed prompt still queued after the reads is typed and mid-turn. The hook sends the note of Claude Code's own prompt also when a later prompt with its text is still queued after the reads, as a repeated monitor event. The hub posts the note as Claude, without a quote. A mid-turn prompt ends no tool-use card and keeps the turn's quote; an owner's prompt added to the turn adds a turn record, and the answer quotes it. The tool hooks send a step at the start of each tool use of the main conversation, with its text from the tool's input and paths relative to the session's folder, and at its end with ✅, ❌ or ⏹️ and the duration, or with `(in background)` for a launch in the background; Grep and Glob show the pattern, then the path; a tool use of a subagent or of this plugin, or without its ID or tool, sends nothing. The hub posts the start of a step as a silent live line, updates it at the step's end with live off, also while the start's send is in flight, and posts an end that arrives before its start as a silent line, and nothing for that start. Steps without an end are completed with their own text at an answer, at a prompt that starts a turn, at the last detach and at the hub's stop, not at a prompt added to the running turn, and the hub posts nothing for their later ends. A line still live 23 hours after its start is completed with its text, and the later end is posted as a new silent line. When the wall clock moves 23 hours on while the timers stand still, as in a sleep, the line is completed at the next check of the minute or at its step's end. The hub's stop waits for the updates that the last detach started. A refused step gets one refusal post, `Claude's note was not posted: ...`, as a refused note does; a failed send or update is logged, and nothing more is posted. A note and the parts of an answer but the last are silent, and the last part of an answer is important. The records follow sessions recorded with Claude Code 2.1.322 on 2026-11-12. The hooks, the session server and the service are tested with the environments of two `claude -p` runs, recorded with Claude Code 2.1.319 in `test/unit/claude-p-runs.jsonl`. One run has the entrypoint `claude-vscode` of a VS Code session, and the other is a plain `claude -p` run. The session server must answer `server/discover` with `Method not found` and negotiate the revision 2025-11-25. Claude Code 2.1.319 registers no channel for a server in the modern MCP era, 2026-07-28, so this test fails after an SDK upgrade that turns that era on.
- `npm run test:e2e`, file `test/e2e/launch.test.ts`, installs a copy of the package with `npm ci` and packs it, as `build.yml` publishes it. The test checks these facts:
  - The tarball has no lockfile.
  - Its top-level entries are the build, `node_modules` and the plugin files.
  - Exactly the production packages of `package-lock.json` are in its `node_modules`.
  - Unpacked, with nothing installed, the package runs a hook and the session server with the commands of `hooks/hooks.json` and `.mcp.json`.
  - Installed into a project offline, from the tarball alone, the package keeps its modules in its own folder, and `simplex-mcp` runs.
- `npm run test:e2e`, file `test/e2e/service.test.ts`, runs with a local smp-server and xftp-server:
  - An owner joins the Main group and the groups of two sessions, and approves `connect`, `send`, `download` and `leave` there. Chats belong to their sessions, and the owner moves one to the other session with `/give`. `/list` works after a service restart. The short description and the welcome message that the owner set in the Main group stay after the restart.
  - While files and media are off in a session's group, the card of a `send` of an image is text. After the owner's approval, the image message reaches the bot with its caption and its file, and the card is edited as text.
  - While SimpleX links are off in a session's group:
    - A `send` card is posted with the local server's SimpleX link at the end of its code span, because the core does not detect a link whose query is followed by the closing backtick.
    - For a terminal prompt with a SimpleX link, Claude posts `User's prompt was not posted: SimpleX links are not allowed for User in the group of this session; its owners can allow them in the group's preferences.`
    - For an answer with a SimpleX link, Claude posts `Claude's answer was not posted: SimpleX links are off in the group of this session; its owners can turn them on in the group's preferences.`
    - A `send` with a preset server's SimpleX link under a standing approval reaches the bot and returns `done`. In place of its card, Claude posts `Action 2 was performed; its card was not posted: SimpleX links are off in the group of this session; its owners can turn them on in the group's preferences.`
    - A bot's welcome message has a preset server's SimpleX link, which has no query. The `connect` card of that bot is not posted, and neither is a `send` card with that link. Claude posts `Action 2: SimpleX links are off in the group of this session; its owners can turn them on in the group's preferences.` and `Action 3: ...` instead. Both calls return `failed` with the text after `Action N: `.
  - While SimpleX links are on for admins only in a session's group, Claude's answer with a SimpleX link is posted. `User` is a member and can't post the terminal prompt with that link, so Claude posts `User's prompt was not posted: SimpleX links are not allowed for User in the group of this session; its owners can allow them in the group's preferences.`
  - The hooks post a prompt typed in the terminal as `User` and the answer as Claude. The session's first prompt is posted also when its hook starts before the transcript file is created. A task notification is not posted by `User`. Claude posts its line, `✅ Background command "Build" completed (exit code 0)`. For a failed task added to the running turn as a queued command, Claude posts `❌ Background command "Lint" failed with exit code 1`. The group's short description shows the model of the first answer, read from the transcript. Claude replies `Session 1 is not running.` to an owner's message to a session without Claude Code. The reply has no quote when the message is as long as the member's core sends. With the session attached, the service passes the owner's message to the session's channel stream. `User` does not post it again, raw or in the channel tag, and Claude quotes it in the answer. The owner's reply to Claude's answer `Lint passes.` reaches the stream after the quote line `> Lint passes.`, with the answer's ID and the sender `Claude`. A PreToolUse hook of Bash posts `💻 Run the tests` from Claude as a live message, and its PostToolUse hook with 2018 ms updates it to `💻 Run the tests ✅ 2.0 s`, no longer live and not edited.
  - Hooks with a new session ID, from the process of an attached session, post in that session's group. After the owner approves a tool-use card, the service sends `allow` on the session's stream. A contact and a group that accept after `connect` returned `connected: false` are in the session's `list`. Mode `host` applies after the owner approves its card, and no mode is written to session.json. After a service restart, a prompt sent before the session attached again is passed to the session, without `Session 1 is not running.`
  - A member joins the Main group with an owner's link:
    - The member sees the Main group's short description and welcome message of trust, and Claude posts the join there.
    - The member joins a session's group as an admin with `/join`. Claude posts the rights of an admin there, with the `/observer` command.
    - The member prompts and approves a tool-use card in the session's group, even after removal from the Main group. Claude replies `Unknown command.` to the member's `//compact`, and the session does not get it.
    - A member added to the session's group with a group link, as a member, prompts. Claude posts the rights of a member for that member. Claude refuses that member's `/pending` and `/!ls` once each, with the same text.
    - A file of `reply` reaches the group at once. After the owner turns off the group's files, the next one fails with `Files and media are off in the group of this session; its owners can turn them on in the group's preferences.`
    - Each join is posted once, in the Main group and in the session's group.
    - The owner makes the added member an admin in the app. Claude posts the admin's rights, and the member's `/pending` then works.
    - With the command of a join post, the owner makes that member an observer. Claude replies with the observer's rights, and the member's app then refuses to send.
    - After a change of the model, the group's files stay off, because Claude updates the group's profile from its current state.
    - `reset-owners` replaces the Main group at once while the owner is in it, without the owner's approval. Run without a terminal, it says to run `link` in a terminal. Run in a terminal, it prints the new link with its QR code.
    - Both members from the Main group keep their roles in the session's group, and they still prompt and approve there.
  - An admin from the Main group approves all messages to a chat. After the owner's `/observer` of that admin, `/list` shows the chat without the approval. Claude replies `No member ID in this group.` to `/observer` of a member who has sent `/join 1` but has not joined yet. The member then joins as an admin.
  - A new session has no chats.
  - A stdio session starts the service. It recovers after the service is killed during a call. It starts the service again and attaches without a call.
  - A SessionStart hook is ended with its descendants while its service starts, as Claude Code ends its hooks when it exits. The service keeps starting, and the next session start shows the link of that service.
  - A Main group named `SimpleX MCP` is renamed `Claude Control` at the next start of the service with the default options.

### Checks in a real session

In a session started as in step 2 above:

1. At the session start, Claude Code shows `SimpleX MCP: scan this QR code with the SimpleX app and tap Join to supervise your agents in "Claude Control":`, a QR code and the link. Ask Claude which link was shown at the session start. Claude doesn't know it. If Claude Code refuses `plugin:simplex@inline` with the development flag, checks 3, 5, 6 and 7 wait for the published plugin and `plugin:simplex@simplex-chat`.
2. Scan the QR code in the SimpleX app and tap Join. In "Claude Control", tap the invitation to "1 · <folder>" and join it.
3. Send `Reply with the word ready.` in "1 · <folder>". The answer comes from Claude, as a reply to your message.
4. Type a prompt in the terminal. It appears in the group from `User`, and the answer appears from Claude. Record whether a prompt from the group also appears from `User`, because Claude Code's documentation does not say whether a channel prompt runs the UserPromptSubmit hook. Ask Claude to run `sleep 5` with Bash in the background. When the task ends, its notification does not appear from `User`; Claude posts a line that starts with ✅ instead. Ask Claude to start a background agent that replies with one word, and to schedule a one-time prompt with CronCreate two minutes ahead. Each appears as one line from Claude. While Claude runs a foreground command, type a prompt in the terminal: it appears from `User`. Ask Claude to read a file and run `sleep 12` with Bash. A line from Claude appears for each step when it starts, `📖 Read PATH` and `💻 ...`, shown as live while the step runs, and each is completed with ✅ and its duration, `12 s` for the sleep, without an edited mark. With an app that supports silent messages, the lines and the notes do not alert, and the last part of each answer does.
5. Send `Count from 1 to 300, one number per line.` in the group, and at once send `Reply with the word two.` The second prompt waits in Claude Code's queue. Claude quotes the first message in the answer with the numbers, and the second message in the answer `two`. Neither prompt appears from `User`. The hub chooses the quote of each answer by the start times of the Stop and UserPromptSubmit hooks, and this check confirms that. Then reply to Claude's answer of check 3 with `Say it again in capitals.` Claude answers in capitals with the word of check 3, as a reply to your message. Send a photo, and reply to it with `What is in this photo?` In the transcript, the tag of the first reply contains `reply_to_from="Claude"`, and its content starts with `> ` and that answer; the content of the second starts with `> image: ` and the photo's file name.
6. Send `//help` in the group. A one-line message that starts with `/` is a command of the group, so Claude replies `Unknown command.`, and the session gets nothing. Send `!git status`. The session gets it as a prompt, and Claude answers it. Claude Code never runs a command from a channel prompt. Claude Code 2.1.319 was checked on 2026-11-08 with a test channel. It ran neither a command nor a shell command from a channel prompt that started with `/` or `!`, and it passed the text to Claude in the `<channel>` tag. After `/model sonnet`, the model was unchanged, and after `!echo`, no file was written. In the code of Claude Code 2.1.322, a channel message is queued as a prompt with `skipSlashCommands` set. Claude may act on such a text with its tools, such as Bash, under the tool-use approval of check 7.
7. Send `Run ls with Bash.` in the group. The terminal shows the permission dialog, and Claude posts the card ``I want to use `Bash`: ...`` with `/approve N or /reject N` in the group. Tap `/approve N`, and `ls` runs. Ask again and tap `/reject N`, and Claude reports the refusal. Answer a third request in the terminal. When the turn ends, Claude ends its card with `Ended.`
8. The short description of the group shows `MODEL · effort LEVEL` after the first answer. It shows the model in a session started with `--resume` too, because the Stop hook reads the model of the answer from the transcript. After `/model` in the terminal, the description shows the new model. A change of the effort level shows after the next answer. This was checked with Claude Code 2.1.322 on 2026-11-11, in `claude -p` runs with a Stop hook from `--settings`. The answer's entry in the transcript, of type `assistant` with `message.model`, was written 26 to 71 ms after the hook started. The SessionStart hook got no `model`.
9. Run `/clear`, and then send `Reply with the word cleared.` in the group. The answer comes in the same group, and no group "2 · <folder>" appears.
10. Stop the service: `kill $(cat /tmp/simplex-mcp-test/service.lock)`. After 30 seconds, without a tool call, send `Reply with the word back.` in the group. The answer comes in the group.
11. Exit Claude Code. Within 10 minutes, while the service runs, send a message in the group. Claude replies `Session 1 is not running.`
12. Start Claude Code again as in step 2, with `--continue`. No new group is opened at the session start, and a prompt from "1 · <folder>" reaches the session.
13. In a second terminal, start Claude Code as in step 2 in another folder, and exit it. The group "2 · <other folder>" appears. In the first session, `/resume` that session, and send `Reply with the word resumed.` in "1 · <folder>". The answer comes there. Exit, and start Claude Code with `--resume` of that session. A prompt from "2 · <other folder>" reaches it.
14. In another folder, run `SIMPLEX_MCP_HOME=/tmp/simplex-mcp-test claude -p "Call the list tool of the simplex server." --plugin-dir /tmp/simplex-plugin`. No new group appears, and the answer contains `This Claude Code run is not interactive, so it has no SimpleX group`. This was checked with Claude Code 2.1.322 on 2026-11-11, with a folder that held only the unpacked package. The answer contained the text, and the folder had no `node_modules/.package-lock.json`, the record of an npm install.
15. Record which values of `CLAUDE_CODE_SESSION_ATTENDED` the hooks and the MCP servers of a run get. Write the MCP config and the hook settings:

    ```
    echo '{"mcpServers": {"dump": {"command": "node", "args": ["-e", "require(\"fs\").writeFileSync(\"/tmp/env-mcp.json\", JSON.stringify(process.env))"]}}}' > /tmp/env-mcp-config.json
    echo '{"hooks": {"SessionStart": [{"hooks": [{"type": "command", "command": "node -e \"require(\\\"fs\\\").writeFileSync(\\\"/tmp/env-hook.json\\\", JSON.stringify(process.env))\""}]}]}}' > /tmp/env-settings.json
    ```

    Run `claude -p "Reply with ok." --strict-mcp-config --mcp-config /tmp/env-mcp-config.json --settings /tmp/env-settings.json` twice. Run it once in a terminal, and once from the Bash tool of the session of step 2, with `CLAUDE_CODE_ENTRYPOINT=claude-vscode` before it. After each run, `grep -o '"CLAUDE_CODE_SESSION_ATTENDED":"[01]"' /tmp/env-hook.json /tmp/env-mcp.json` shows the values. The results with Claude Code 2.1.319 on 2026-11-08:
    - In a terminal, the hook got `0`, and the MCP server got no value.
    - From the Bash tool, the hook got `0`, and the MCP server got `1`, the value of the session whose Bash tool ran it. The hook's `CLAUDE_PID` is the parent process of the MCP server.

    For the second run, the plugin's SessionStart hook marks that process in the service, and the service refuses its session server.
16. From the Bash tool of the session of step 2, run `CLAUDE_CODE_ENTRYPOINT=claude-vscode SIMPLEX_MCP_HOME=/tmp/simplex-mcp-test claude -p "Call the list tool of the simplex server." --plugin-dir /tmp/simplex-plugin` in another folder. No new group appears, and the answer contains `This Claude Code run is not interactive, so it has no SimpleX group`.
17. Load the plugin from settings, as in [Shared settings](REFERENCE.md#shared-settings), with a plugin registry of its own:

    ```
    mkdir -p /tmp/simplex-plugins/.claude-plugin
    cp -r /tmp/simplex-plugin /tmp/simplex-plugins/plugin
    echo '{"name": "simplex-local", "owner": {"name": "Test"}, "plugins": [{"name": "simplex", "source": "./plugin"}]}' > /tmp/simplex-plugins/.claude-plugin/marketplace.json
    echo '{"extraKnownMarketplaces": {"simplex-local": {"source": {"source": "directory", "path": "/tmp/simplex-plugins"}}}, "enabledPlugins": {"simplex@simplex-local": true}}' > /tmp/shared-settings.json
    ```

    In another folder, run `CLAUDE_CODE_PLUGIN_CACHE_DIR=/tmp/claude-plugins-test SIMPLEX_MCP_HOME=/tmp/simplex-mcp-test claude -p "Call the list tool of the simplex server." --settings /tmp/shared-settings.json`. The answer contains `This Claude Code run is not interactive, so it has no SimpleX group`. With `CLAUDE_CODE_PLUGIN_CACHE_DIR` set, Claude Code registers the marketplace outside `~/.claude/plugins`. The results with Claude Code 2.1.322 on 2026-11-11:
    - The answer contained the text.
    - `/tmp/claude-plugins-test/known_marketplaces.json` listed `simplex-local`.
    - The plugin's folder had no `node_modules/.package-lock.json`.
    - With the same keys in `.claude/settings.json` of the folder, and without `--settings`, the run had no tool of the plugin.
18. Start Claude Code as in step 2 with an empty data folder and a machine name: `SIMPLEX_MCP_HOME=/tmp/simplex-mcp-fresh SIMPLEX_MCP_MACHINE=laptop claude --plugin-dir /tmp/simplex-plugin --dangerously-load-development-channels plugin:simplex@inline`. Exit it before the QR code shows. Start it again the same way. The QR code shows within seconds, for the group "Claude Control - laptop". Stop that service: `kill $(cat /tmp/simplex-mcp-fresh/service.lock)`.
19. Stop the service: `kill $(cat /tmp/simplex-mcp-test/service.lock)`.
