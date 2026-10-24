# Testing @simplex-chat/mcp v0.2

Node.js 22 or later. All commands run in `packages/simplex-chat-mcp` unless a step names another folder.

## Build and test

```
npm ci
npm run build
npm test            # unit tests, about 10 s
npm run test:e2e    # end-to-end tests with a local smp-server, a few minutes
```

- `npm run build` runs `tsc`, which leaves the output of deleted sources in `dist`; `npm pack` and `npm publish` delete `dist` first and build it again (script `prepack`).
- At the first start, `simplex-chat` downloads libsimplex and its Node.js add-on to `~/.cache/simplex-chat`, with a `Downloading ...` line each on stderr.
- The end-to-end tests download smp-server v7.1.0-beta.3 to `node_modules/.cache/smp-server` on Linux. Elsewhere, or when `node_modules` is not writable, set `SMP_SERVER=/path/to/smp-server`.

## Install the plugin from this checkout

The plugin's hooks, server and skills run `npx -y @simplex-chat/mcp@0.2.0`, with the version from `package.json`. npx takes that version from the `node_modules` of the current folder first, and from the npm registry otherwise. Before 0.2.0 is published, install the packed build into a test project and start Claude Code there:

1. Pack the build, and install it into a new project folder:

   ```
   npm pack --pack-destination /tmp
   mkdir -p /tmp/simplex-test && cd /tmp/simplex-test
   npm install --no-save /tmp/simplex-chat-mcp-0.2.0.tgz
   npx -y @simplex-chat/mcp@0.2.0 --help
   ```

   The last command prints the usage. Outside that folder it fails with `No matching version found for @simplex-chat/mcp@0.2.0.` until the version is published.

2. Create a local marketplace in the folder above the package, `packages/.claude-plugin/marketplace.json`, with a relative-path source:

   ```json
   {"name": "local", "owner": {"name": "dev"}, "plugins": [{"name": "simplex", "source": "./simplex-chat-mcp"}]}
   ```

   A relative source is read from the marketplace root, the folder that contains `.claude-plugin/`, and can't contain `..`. Check it from the repository root with `claude plugin validate ./packages/simplex-chat-mcp/..`.

3. In Claude Code, from the repository root:

   ```
   /plugin marketplace add ./packages/simplex-chat-mcp/..
   /plugin install simplex@local
   ```

   Claude Code reads a relative-path plugin of a local marketplace from its folder, so changes to `plugin/`, `hooks/`, `skills/` and `.mcp.json` apply at the next session start or after `/reload-plugins`.

4. Start `claude` in `/tmp/simplex-test`.

Instead of steps 2 and 3, `claude --plugin-dir <checkout>/packages/simplex-chat-mcp` started in `/tmp/simplex-test` loads the plugin for that session only, as `simplex@inline`, without changing the Claude Code settings. Its options then come from `--settings`, for example `--settings '{"pluginConfigs":{"simplex@inline":{"options":{"main_group":"Test Agents"}}}}'`.

After a change in `src/`, repeat step 1 and stop the running daemon, which a daemon of the same version doesn't replace: `kill $(cat ~/.simplex-mcp/lock)`. Remove `packages/.claude-plugin/` when done: it is not part of the repository.

## Manual checks

1. First session: the session start shows `SimpleX MCP: scan this QR code with the SimpleX app and tap Join to supervise your agents in "SimpleX MCP":`, a QR code and the link. Record how Claude Code displays it (multi-line, monospace) and whether the SimpleX app scans the QR code. `~/.simplex-mcp/daemon.log` starts with `SimpleX MCP daemon 0.2.0, data folder ...`. Record the time of the first start, which fills the npx cache and downloads libsimplex, against the hook's 600 s timeout.
2. Join by the link: the group "SimpleX MCP" appears with `I'm your agents' Main group. Your Claude Code sessions ask here before they connect or send.` A second session start shows no link.
3. `/simplex:link` prints `Owners of "SimpleX MCP": <your name>. To replace them, run /simplex:reset-owners.`; `npx -y @simplex-chat/mcp link` in a terminal prints the same.
4. Two concurrent sessions, the second one started with `claude -n docs`: their profiles are `SimpleX MCP - simplex-test` and `SimpleX MCP - docs`, and their cards start with `Session 1 · SimpleX MCP - simplex-test` and `Session 2 · SimpleX MCP - docs`.
5. `claude --resume` of session 2: `list` returns the chats of `SimpleX MCP - docs`.
6. Change `main_group` in `/config`, then start a new session: the Main group and its profile get the new name.
7. Mode `requests`: tool calls run without a Claude Code prompt (PreToolUse hook). Mode `host` (`approve` in `/config`, then a new session): `connect`, `send` and `leave` make Claude Code prompt; after you allow a send, the Main group gets the outcome card ending with `Performed without approval (mode host).`
8. `/simplex:reset-owners`: the old Main group is deleted, and the output is `"SimpleX MCP" was replaced. Join the new one in the SimpleX app. The link works once:` with the link. Until you join, a `connect` is refused with `The agent has no owner. Open this link in the SimpleX app and join: <link>`.
9. `/clear` in a session: `~/.simplex-mcp/_sessions` gets no new folder, and the next tool call uses the same profile; its cards keep the session number.
10. Stop the daemon while a `receive` with `wait` runs: `kill $(cat ~/.simplex-mcp/lock)`. The call returns `The SimpleX daemon stopped; repeat the call.`, and the repeated call starts a new daemon.

## Try the tools

Prompts for Claude Code, one at a time. A call that waits more than 2 minutes, for example for an approval, moves to a background task, and its result arrives as a notification. The examples are from session 1.

By name, first:

- `Connect to @directory.simplex as yourself.` While the name is not registered: the tool error `Name not found: This SimpleX name is not registered. Please check the name.`, and no card. Once it is registered: the card `I want to connect to bot SimpleX Directory (@directory.simplex), as myself.`; after the approval, step 1 below returns `status: known`, with no card.

The directory by its address (`docs/DIRECTORY.md`):

1. `Connect to the SimpleX Directory as yourself at https://smp4.simplex.im/a#lXUjJW5vHYQzoLYgmi8GbxkGP41_kjefFvBrdwg-0Ok and tell me which commands it advertises.`
   Card: `Session 1 · SimpleX MCP - simplex-test`, then `I want to connect to bot SimpleX Directory, as myself.` Tap `/approve 1`. The card becomes `I connected to ...` with `Approved by Alice.`; Claude gets `status: done`, the chat `@N`, `commands` and the welcome message.
2. `Search the directory for hiking groups.` Card: `I want to send a message to bot SimpleX Directory:` with the text. Approve it; Claude gets the bot's answers in `responses`.
3. `Send /help to the directory.` A bot command is sent as text.
4. `Check for new messages from the directory, waiting up to 60 seconds.` `receive` with `wait`.
5. `List your chats and pending actions.` `list`.
6. Reject one card with its `/reject 1:@N A`: the call returns `status: rejected`; the card ends with `Rejected by Alice.` `/pending` lists the waiting cards as `A  session 1: send to bot SimpleX Directory  expires ...`.

Approval of all messages to a chat, and leaving:

1. `Search the directory for Monero groups.` On the card, tap `/approve 1:@N all`. The reply is `Approved for bot SimpleX Directory (1:@N) by Alice. /revoke 1:@N`, and the search is sent.
2. `Send /help to the directory.` It is sent without a card; the Main group then gets `I sent a message to bot SimpleX Directory:`, `` `/help` `` and `Approved for this chat by Alice. /revoke 1:@N`. `list` shows `approved: true` for the directory.
3. Tap `/list` in the command menu: `Session 1 · SimpleX MCP - simplex-test`, then one line per chat; the directory's line ends with `, approved for this chat by Alice`.
4. Tap `/revoke 1:@N`: `Messages to bot SimpleX Directory (1:@N) need approval again.` The next send gets a card.
5. `Leave the directory.` Card: `I want to delete bot SimpleX Directory (@N).` Approve it: the call returns `status: done`, and `list` no longer shows the directory.
6. Join a group from the search results, then `Leave that group.` Card: `I want to leave group <name> (#N).` Approve it: the members see that the agent left.

A channel:

1. `Search the directory for channels about trail running.` Take a channel link from the answers.
2. `Connect to <the channel link> as yourself.` Card: `I want to connect to channel <name>, as myself.` Approve it; Claude gets `type: channel`, `role: observer` and the recent posts in `messages`.
3. `Wait up to 10 minutes for a new post in that channel.` `receive` with `wait` returns the next post.

Incognito, in a second session, because the first one already knows the directory:

1. `Connect to the directory incognito: https://smp4.simplex.im/a#lXUjJW5vHYQzoLYgmi8GbxkGP41_kjefFvBrdwg-0Ok` Card: `Session 2 · ...`, then `I want to connect to bot SimpleX Directory, incognito.` Approve it; Claude gets `known_as`, a random name.
2. `List your chats.` The directory has `incognito: true` and the same `known_as`. A send card for it says `where I am incognito`, and its commands name `2:@N`.

Stop and restart:

- Stop the daemon while a card waits: the card ends with `Cancelled: the SimpleX daemon stopped.` After a restart, `/approve` of that number replies `Action N is not pending.`, and the next card has the next number.
- `npx -y @simplex-chat/mcp daemon` while a daemon runs prints `running` and exits.

## Claude Code checks

1. Subagent: `Use a subagent to send "trail shoes" to the directory.` While the card waits, the subagent's call stays in the foreground, with no background task; it returns after `/approve`.
2. Esc: ask for a send, and press Esc while its card waits; `List your pending actions.` shows the action with `open: false`; the same request again attaches to it, and `/approve` returns the result to that call.

## Verified on 2026-10-24 (Claude Code 2.1.302, local tarball)

The `npm pack` tarball was installed into a test project, and each check ran there as `claude -p --plugin-dir <package> --setting-sources project,local --tools "" --model haiku --output-format stream-json --verbose --include-hook-events`, with a new `SIMPLEX_MCP_HOME` per data folder and the public SMP servers.

1. Hook and daemon start: PASS. The stream has the `SessionStart:startup` hook output with `systemMessage` (QR code and link) and `additionalContext`; the transcript records them as `hook_system_message` and `hook_additional_context`. `daemon.log` has `SimpleX MCP daemon 0.2.0, data folder ...`.
2. First-run time: PASS, about 10 s against the 600 s limit. `npx -y --package <tarball>` with an empty npm cache took 4.3 s; the SessionStart hook with an empty `XDG_CACHE_HOME`, which downloads libsimplex and the add-on and creates the Main group, took 5.7 s.
3. Tools without prompts: PASS. Claude called `mcp__plugin_simplex_simplex__list`, the PreToolUse hook returned `permissionDecision: allow`, and the tool returned `{"chats":[...],"actions":[]}` without `--allowedTools` or a permission mode.
4. Plugin options with `--plugin-dir`: PASS. Without values, the server gets the defaults of `plugin.json` (`CLAUDE_PLUGIN_OPTION_MAIN_GROUP=SimpleX MCP`, `CLAUDE_PLUGIN_OPTION_APPROVE=requests`, `CLAUDE_PLUGIN_OPTION_EXPIRE_HOURS=23`), and the hook gets no `CLAUDE_PLUGIN_OPTION_*` variables, so `config.ts` applies the same defaults. With `main_group` set to `Test Agents` through `--settings` (see above), the server and the hook get `Test Agents`: a new data folder logs `Main group "Test Agents" created.`, and an owned Main group is renamed, with `/status` `mainGroup: "Test Agents"` and the owner kept.
5. Session naming: PASS. `claude -n docs -p ...` gets session 2 `SimpleX MCP - docs`, with the title from the SessionStart hook; a run without `-n` gets `SimpleX MCP - simplex-test`.
6. Two concurrent sessions: PASS. Two runs started together both returned `list` results, and `/status` lists sessions 2 and 3.
7. Owner flow: PASS. A scripted person `Tester` joined by `owner.link`, got the greeting, and `/status` showed owners `["Tester"]` and no link. For `Connect to <Tester's address> as yourself`, the Main group got `Session 4 · SimpleX MCP - simplex-test` / `I want to connect to contact Tester, as myself.`; after `/approve 1` the card became `I connected to contact Tester, as myself.` / `Approved by Tester.`, and the tool returned `status: done` with `connected: true` after the 30 s idle wait.
8. Resume: PASS. `claude -p --resume <session ID of check 3>` keeps the session ID in the stream and in `CLAUDE_CODE_SESSION_ID`: the hook ran as `SessionStart:resume`, no new folder appeared in `_sessions`, and `/status` lists session 1 again.

Needs the user: how an interactive terminal shows the QR code of the SessionStart `systemMessage`, and scanning it with the SimpleX app; approving in the SimpleX app; the prompts of mode `host`; `/clear` in an interactive session.
