# Testing @simplex-chat/mcp v0.1

Node.js 22. Claude Code runs on the same computer as `serve`: the server listens on 127.0.0.1 only.

## Build

### Here

All commands run in `/code/simplex-chat-6/packages/simplex-chat-mcp`. The dependencies are installed by `npm install`; `simplex-chat` comes from npm and downloads libsimplex and its Node.js add-on to `~/.cache/simplex-chat` at the first start.

```
node node_modules/typescript/bin/tsc
node node_modules/jest/bin/jest.js test/unit     # about 10 s
SMP_SERVER=/code/steward/node_modules/.cache/smp-server/smp-server-v7.1.0-beta.3 node node_modules/jest/bin/jest.js test/e2e      # about 3 minutes; a local smp-server on a free port
```

`node_modules` is not writable here, so the end-to-end test takes the smp-server binary from `SMP_SERVER` instead of downloading it to `node_modules/.cache`.

### On your own machine

1. Check out simplex-chat master; the package is `packages/simplex-chat-mcp`.
2. In that folder: `npm install`, then `npx tsc`. `tsc` leaves the output of deleted sources in `dist`; `npm pack` and `npm publish` delete `dist` first and build it again (script `prepack`).
3. `node dist/cli.js serve my-agent --display-name "My Agent"`. At the first start, `simplex-chat` downloads libsimplex and its Node.js add-on, with a `Downloading ...` line each on stderr.
4. Tests: `npx jest test/unit`; `npx jest test/e2e` downloads smp-server v7.1.0-beta.3 on Linux, and elsewhere takes `SMP_SERVER=/path/to/smp-server`.

Until the package is published, the printed hint `npx -y @simplex-chat/mcp register my-agent` stands for `node dist/cli.js register my-agent`.

## Start, join, register

1. `node dist/cli.js serve my-agent --display-name "My Agent"`. NAME, here `my-agent`, names the data folder and the Claude Code entry; `--display-name` is the name people see in SimpleX. It prints the data folder (`~/.simplex-mcp/my-agent`), creates the agent control chat "My Agent", and prints a QR code and a link that works once. For a short link the QR code has 33 modules and takes 37 columns × 19 lines; `--qr-code large` prints it at 74 × 37. Keep it running; Ctrl-C stops it. `node dist/cli.js --help` lists the options.
2. In the SimpleX app, scan the QR code, or paste the link into "Connect via link". If the code does not scan, restart with `--qr-code large`. The chat "My Agent" appears with `I'm your agent. I ask here before I connect or send.` The terminal prints `Alice joined (member N) and is now an owner. The link is deleted.` and the register hint.
3. `node dist/cli.js register my-agent`. It prints `Added to Claude Code: scope user, server simplex-my-agent.` Run it in a terminal with the same `CLAUDE_CONFIG_DIR` as the one where `claude` runs: `claude mcp add-json` writes to the config of its own environment.
4. Add `"mcp__simplex-my-agent"` to `permissions.allow` in `~/.claude/settings.json`, so Claude Code does not prompt for each call; approval happens in the SimpleX app.
5. Start `claude`. `/mcp` shows `simplex-my-agent` connected with 5 tools. A session started before `register` does not load the entry; start a new one.

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

Incognito, as a second agent, because the first one already knows the directory:

1. `node dist/cli.js serve scout --display-name "Scout" --port 5227`; join its control chat "Scout" with the printed link; `node dist/cli.js register scout`; add `"mcp__simplex-scout"` to `permissions.allow`; `/mcp reconnect all`.
2. `Using simplex-scout, connect to the directory incognito: https://smp4.simplex.im/a#lXUjJW5vHYQzoLYgmi8GbxkGP41_kjefFvBrdwg-0Ok` Card: `I want to connect to bot SimpleX Directory, incognito.` Approve it; Claude gets `known_as`, a random name.
3. `Using simplex-scout, list your chats.` The directory has `incognito: true` and the same `known_as`. A send card for it says `where I am incognito`.

Stop and restart:

- Stop the server while a card waits: the card ends with `Cancelled: the server stopped.` After a restart, `/approve` of that number replies `Action N is not pending.`, and the next card has the next number.
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
