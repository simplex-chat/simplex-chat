# Testing @simplex-chat/mcp v0.3

Node.js 22, and curl for the status hooks. Claude Code runs on the same computer as `serve`: the server listens on 127.0.0.1 only.

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
2. In that folder: `npm install`, then `npx tsc`. `tsc` leaves the output of deleted sources in `dist`; `npm pack` and `npm publish` delete `dist` first and build it again (script `prepack`).
3. `node dist/cli.js serve my-agent --display-name "My Agent"`. At the first start, `simplex-chat` downloads libsimplex and its Node.js add-on, with a `Downloading ...` line each on stderr.
4. Tests: `npx jest test/unit`; `npx jest test/e2e` downloads smp-server and xftp-server v7.1.0-beta.3 on Linux, and elsewhere takes `SMP_SERVER=/path/to/smp-server XFTP_SERVER=/path/to/xftp-server`.

Until the package is published, the printed hint `npx -y @simplex-chat/mcp register my-agent` stands for `node dist/cli.js register my-agent`.

## Start, join, register

1. `node dist/cli.js serve my-agent --display-name "My Agent"`. NAME, here `my-agent`, names the data folder and the Claude Code entry; `--display-name` is the name people see in SimpleX. It prints the data folder (`~/.simplex-mcp/my-agent`), creates the agent control chat "My Agent", and prints a QR code and a link that works once. For a short link the QR code has 33 modules and takes 37 columns × 19 lines; `--qr-code large` prints it at 74 × 37. Keep it running; Ctrl-C stops it. `node dist/cli.js --help` lists the options.
2. In the SimpleX app, scan the QR code, or paste the link into "Connect via link". If the code does not scan, restart with `--qr-code large`. The chat "My Agent" appears with `I'm your agent. I ask here before I connect, send, download a file or leave a chat.` The terminal prints `Alice joined (member N) and is now an owner. The link is deleted.` and the register hint.
3. `node dist/cli.js register my-agent`. It prints `Added to Claude Code: scope user, server simplex-my-agent, status hooks in <your home folder>/.claude/settings.json.` Run it in a terminal with the same `CLAUDE_CONFIG_DIR` as the one where `claude` runs: `claude mcp add-json` writes to the config of its own environment, and `register` writes the hooks to `settings.json` in `CLAUDE_CONFIG_DIR` when it is set and not empty.
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

Status messages, in your chat with the agent:

1. `Connect to me as yourself at <your SimpleX address>.` Approve the card, and accept the request in the app. In "My Agent", send `/approve @N all`, with the chat's ID from `/list`: the reply is `Approved for contact <your name> (@N) by <your name>. /revoke @N`.
2. `Take my prompts from that chat and answer there: wait for my messages with receive (wait 3600, typing true), answer each one in that chat with send (wait 0), and call receive again after every answer, until I say stop. For each step that takes minutes, send a short note with live: true in the same response as the step's tool call.`
3. Send a prompt from the app. A message with the header `LIVE` is shown at once: `◐ received, waiting for the agent · 0:00`, or `◐ thinking · 0:00` when `receive` returns the prompt first; then `thinking`, with the next frame at each update. Each note is shown under the line, and the terminal prints `Note to contact <your name> (@N): <the note>`.
4. The answer is received as a new message, and the status message is shown as `marked deleted`. Set "Delete for everyone" to "always" in the agent contact's preferences, and send another prompt: after the answer, the status message is removed.
5. Send a prompt, and send `/revoke @N` in "My Agent" while the status message is shown: the status message is deleted within about 20 seconds, and a card is posted for the answer.
6. With a group of yours and a second member in it: give Claude Code the group prompt of the README, approve its card, and send `/approve #N all` in "My Agent". After a message of either member, a status message is posted; both members see it and its notes, and the terminal prints `Note to group <name> (#N): <the note>`.
7. In your chat with the agent, send `stop`: the agent answers and stops calling `receive`. Send another prompt: `◐ received, waiting for the agent · 0:00` is shown at once and kept, with the next frame at each update. Ask Claude Code in the terminal to take your prompts again: when its `receive` returns the prompt, the label is `thinking` at once.

Status messages with llama-swap:

1. Start llama-swap on port 8080, and restart with `node dist/cli.js serve my-agent --inference http://127.0.0.1:8080`. The terminal prints `Inference endpoint: http://127.0.0.1:8080` after `Approval mode: requests`.
2. Send a prompt from the app. Within 10 minutes, while the status message is shown, send llama-swap a request with a long prompt for a model not yet loaded, from another terminal. The label is `loading MODEL`, then `reading the prompt with MODEL, N tokens read` and `generating with MODEL, N tokens/s`, with MODEL as code; after the request, `working`.
3. Stop llama-swap. The server writes `inference endpoint: GET http://127.0.0.1:8080/running: connect ECONNREFUSED 127.0.0.1:8080` to stderr once, and the label is `thinking` again.
4. `node dist/cli.js serve my-agent --inference http://127.0.0.1:8080 --approve host` exits with `--inference: status messages are shown only in mode requests.` With `SIMPLEX_MCP_INFERENCE_KEY="a b"`, `serve` with `--inference` exits with `SIMPLEX_MCP_INFERENCE_KEY: visible ASCII characters expected.` Both exit with code 2.

Status messages with Claude Code's work, after steps 1 and 2 of "Status messages":

1. `~/.claude/settings.json` has a group with the `curl` handler shown in REFERENCE.md, "Add the agent to Claude Code", for each of the 14 events named there; `/hooks` in Claude Code labels them as user settings. `~/.simplex-mcp/my-agent/hook-headers` holds `Authorization: Bearer <token>`, with mode 0600.
2. Send `Run sleep 90 in the shell, then tell me that it is done.` from the app. When Claude Code asks for permission to run it, the label is `waiting for permission in Claude Code` until the command ends.
3. Add `"Bash(sleep *)"` to `permissions.allow` in `~/.claude/settings.json`, and send the prompt of step 2 again: Claude Code runs the command without asking. Meanwhile, ask a second `claude` session in another terminal, which has not called the agent's tools, to search the web for SimpleX: the label is `running a command` until `sleep 90` ends.
4. Send `Use a subagent to list the files of this folder.` The labels include `running subagent TYPE`, with TYPE as code.
5. Stop `serve`, and ask a Claude Code session to run `ls`: Claude Code runs it, and the failures of the hooks are hidden. Start `serve` again, and run `/mcp reconnect all`.
6. `mkdir /tmp/cc`, `echo '[]' > /tmp/cc/settings.json`, then `CLAUDE_CONFIG_DIR=/tmp/cc node dist/cli.js register my-agent`: it prints `Added to Claude Code: scope user, server simplex-my-agent. /tmp/cc/settings.json is not a valid settings file; add these status hooks to it: {"hooks":{...}}` and exits with code 1, and the file still holds `[]`.
7. In a new folder outside a git repository, for example `/tmp/project`, `node <this folder>/dist/cli.js register my-agent --scope local` prints `Added to Claude Code: scope local, server simplex-my-agent, status hooks in /tmp/project/.claude/settings.local.json.`, and that file has the hooks.

Stop and restart:

- Stop the server while a card waits: the terminal prints `Stopping: sending 1 cancelled card, up to 10 s. Ctrl-C to stop now.`, and the card ends with `Cancelled: the server stopped.` With a status message shown too, the line is `Stopping: sending 1 cancelled card and 1 status deletion, up to 10 s. Ctrl-C to stop now.`, and the status message is deleted. A second Ctrl-C during the wait stops the server at once. With no card waiting and no status message shown, Ctrl-C stops it at once. After a restart, `/approve` of that number replies `Action N is not pending.`, and the next card has the next number.
- Stop the server with `kill -9` and the PID in `~/.simplex-mcp/my-agent/lock` while a status message is shown: at the next start, the server deletes the status message before it prints `Listening on http://127.0.0.1:5226/mcp`.
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
4. C5, register again: `node dist/cli.js register my-agent` prints `Added to Claude Code: scope user, server simplex-my-agent, status hooks in <your home folder>/.claude/settings.json.`; `claude mcp list` shows one `simplex-my-agent`, and `settings.json` has one `curl` handler of the agent for each event.
5. C6, Esc: ask for a send, and press Esc while its card waits; `List your pending actions.` shows the action with `open: false`; the same request again attaches to it, and `/approve` returns the result to that call.
6. Host mode: restart with `node dist/cli.js serve my-agent --approve host`, then `/mcp reconnect all`. `Send /help to the directory.` makes Claude Code prompt, despite the allow rule; so does `leave`. After you allow it, the agent control chat gets `I sent a message to bot SimpleX Directory:`, `` `/help` `` and `Performed without approval (mode host).` A `connect` to a link not yet joined gets the same prompt and an outcome card; a target already joined returns `status: known`, with no card.
