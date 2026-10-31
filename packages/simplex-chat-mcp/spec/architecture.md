# SimpleX MCP server: architecture

> System structure, modules, state, process lifecycle, approval model and security boundaries of `@simplex-chat/mcp` 0.2.0.
>
> Related specs: [README](README.md) | [Flows](flows.md) | [API](api.md)
> Related docs: [README](../README.md) | [Reference](../REFERENCE.md) | [PLAN](../PLAN.md)

**Source:** [`cli.ts`](../src/cli.ts) | [`session.ts`](../src/session.ts) | [`client.ts`](../src/client.ts) | [`socket.ts`](../src/socket.ts) | [`protocol.ts`](../src/protocol.ts) | [`daemon.ts`](../src/daemon.ts) | [`hub.ts`](../src/hub.ts) | [`agent.ts`](../src/agent.ts) | [`core.ts`](../src/core.ts) | [`tools.ts`](../src/tools.ts) | [`actions.ts`](../src/actions.ts) | [`control.ts`](../src/control.ts) | [`messages.ts`](../src/messages.ts) | [`hook.ts`](../src/hook.ts) | [`owner.ts`](../src/owner.ts) | [`config.ts`](../src/config.ts) | [`files.ts`](../src/files.ts) | [`qr.ts`](../src/qr.ts) | [`log.ts`](../src/log.ts)

---

## Table of contents

1. [System diagram](#1-system-diagram)
2. [Modules](#2-modules)
3. [State](#3-state)
4. [Process lifecycle](#4-process-lifecycle)
5. [Approval model](#5-approval-model)
6. [Security boundaries](#6-security-boundaries)
7. [Limits](#7-limits)

---

## 1. System diagram

```mermaid
flowchart LR
  subgraph cc["Claude Code"]
    sa["session A"]
    sb["session B"]
    hk["SessionStart hook<br/>PreToolUse hook"]
    sk["/simplex:link<br/>/simplex:reset-owners"]
  end
  subgraph pa["simplex-mcp session (per Claude session)"]
    mcp["McpServer<br/>StdioServerTransport<br/>registerTools"]
    link["DaemonLink<br/>DaemonClient"]
  end
  subgraph pd["simplex-mcp daemon (one per data folder)"]
    sock["socket.ts listen<br/>bearer token"]
    dm["Daemon<br/>routes, entries, idle"]
    hub["Hub<br/>Control, Actions"]
    ag["session agents<br/>AgentTools, ChatItemsWatch"]
    hcore["hub core<br/>profile main_group"]
    score["session cores<br/>SimpleX MCP - NAME"]
  end
  sa -->|stdio| mcp
  sb -->|stdio, own process| mcp
  hk -->|"launch.js hook session-start"| link
  sk -->|"launch.js link / reset-owners"| link
  mcp --> link
  link -->|"HTTP over daemon.sock<br/>Authorization: Bearer"| sock
  link -.->|"spawn on ENOENT or ECONNREFUSED"| pd
  sock --> dm
  dm --> hub
  dm --> ag
  hub --> hcore
  ag --> score
  hub <-->|"refusal, cards, perform"| ag
  hcore --> hdb[("_hub/<br/>simplex_chat.db<br/>simplex_agent.db")]
  score --> sdb[("_sessions/ID/<br/>databases, session.json")]
  hcore <-->|SMP| smp["SMP servers"]
  score <-->|"SMP<br/>private routing"| smp
  smp <--> owner["Owner's SimpleX app<br/>Main group"]
  smp <--> peers["Peers:<br/>contacts and bots<br/>groups and channels"]
  pd --> logf[("daemon.log")]
```

Data flow of a tool call:

1. Claude Code calls a tool of the MCP server `simplex` of the session; the SDK validates the arguments against the input schema ([`tools.ts:355-365`](../src/tools.ts#L355-L365), [`mcp-BumlNux0.cjs:1812-1817`](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L1812-L1817)).
2. The session process connects to the daemon when it has no connection: status and version check, spawn when not running, `POST /session`, then `POST /attach` held open ([`session.ts:123-140`](../src/session.ts#L123-L140), [`client.ts:32-44`](../src/client.ts#L32-L44)).
3. The session process sends `POST /call` with the session ID, the tool name and the input, relays each `{progress}` line as an MCP progress notification, and returns the `{result}` line ([`session.ts:94-113`](../src/session.ts#L94-L113)).
4. The daemon checks the token, finds or opens the session's agent, and runs `AgentTools.call` with an abort signal tied to the request ([`socket.ts:18-48`](../src/socket.ts#L18-L48), [`daemon.ts:252-269`](../src/daemon.ts#L252-L269)).
5. `AgentTools` sends chat commands to the session core; actions are created in the hub's `Actions` and shown to the owner by `Control` in the Main group ([`tools.ts:415-428`](../src/tools.ts#L415-L428), [`actions.ts:123-158`](../src/actions.ts#L123-L158)).
6. After an approval, `Actions` calls the daemon's `perform`, which runs the perform step in the action's session ([`daemon.ts:186-190`](../src/daemon.ts#L186-L190), [`tools.ts:379-388`](../src/tools.ts#L379-L388)).
7. Each core emits its events: the hub core to `Control`, a session core to its item watch and connect actions ([`hub.ts:91-104`](../src/hub.ts#L91-L104), [`agent.ts:46-59`](../src/agent.ts#L46-L59)).

---

## 2. Modules

### [2.1 `cli.ts`](../src/cli.ts)

| Symbol | Line | Contents |
|---|---|---|
| `helpText` | [8](../src/cli.ts#L8) | `--help` output |
| `Command`, `commands` | [23-32](../src/cli.ts#L23-L32) | `session`, `daemon`, `hook session-start`, `link`, `reset-owners`, `--help` |
| `parseCommand` | [34](../src/cli.ts#L34) | the joined arguments must equal one command; otherwise `ExitError` code 2 |
| `main` | [40](../src/cli.ts#L40) | dispatch; the hook reads stdin and prints its JSON output; `link` and `reset-owners` print their text |
| top-level catch | [67-76](../src/cli.ts#L67-L76) | `ExitError`: message on stderr, its code; other errors: `logError`, code 1 |

### [2.2 `session.ts`](../src/session.ts)

| Symbol | Line | Contents |
|---|---|---|
| `startSession` | [34](../src/session.ts#L34) | session ID check, options, `McpServer` with instructions, tools, stdio transport, background connect |
| `DaemonLink.connect`, `start` | [62-74](../src/session.ts#L62-L74) | one shared connect promise; `start` logs a failure on stderr |
| `DaemonLink.call` | [81](../src/session.ts#L81) | forward; one retry on status 409; failures to tool errors |
| `DaemonLink.forward` | [94](../src/session.ts#L94) | `/call` stream: progress lines, result line; a broken stream drops the connection |
| `DaemonLink.disconnect` | [116](../src/session.ts#L116) | drops the connection only when it is the current one |
| `DaemonLink.open` | [123](../src/session.ts#L123) | `ensureDaemon`, `/session`, `/attach` until its first line |
| `failureText` | [143](../src/session.ts#L143) | not running or status 503: `The SimpleX daemon stopped; repeat the call.` |

### [2.3 `client.ts`](../src/client.ts)

| Symbol | Line | Contents |
|---|---|---|
| `DaemonClient.forRoot` | [19](../src/client.ts#L19) | data folder, token, socket path |
| `DaemonClient.ensureDaemon` | [32](../src/client.ts#L32) | status; a lower daemon version is stopped; spawn when none runs; protocol check |
| `spawnDaemon` | [68](../src/client.ts#L68) | detached child with stdout and stderr to `daemon.log` and a ready pipe on fd 3 |
| `firstLine` | [91](../src/client.ts#L91) | first line of the ready pipe, spawn error, or the 5-minute timeout |
| `compareVersions` | [117](../src/client.ts#L117) | numeric comparison of the first three version parts |

### [2.4 `socket.ts`](../src/socket.ts)

| Symbol | Line | Contents |
|---|---|---|
| `SocketError` | [12](../src/socket.ts#L12) | message and HTTP status |
| `listen` | [18](../src/socket.ts#L18) | `http.Server` on the socket path: token check, route table, body read, error replies, timeouts |
| `sendJson`, `startLines` | [50-60](../src/socket.ts#L50-L60) | JSON reply; NDJSON stream |
| `isNotRunning` | [62](../src/socket.ts#L62) | `ENOENT` or `ECONNREFUSED` |
| `requestJson`, `streamLines` | [67-86](../src/socket.ts#L67-L86) | client request with a JSON reply; client request with NDJSON lines; status other than 200 to `SocketError` |
| `send` | [88](../src/socket.ts#L88) | `http.request` on the socket path, no connection pool, bearer header |

### [2.5 `protocol.ts`](../src/protocol.ts)

| Symbol | Line | Contents |
|---|---|---|
| `protocolVersion` | [5](../src/protocol.ts#L5) | `1` |
| `SessionRequest`, `TitleRequest`, `TitleReply`, `SessionReply` | [17-38](../src/protocol.ts#L17-L38) | `/session`, `/title` |
| `AttachRequest`, `CallRequest`, `CallLine` | [40-50](../src/protocol.ts#L40-L50) | `/attach`, `/call` |
| `StatusReply`, `ResetReply` | [52-60](../src/protocol.ts#L52-L60) | `/status`, `/reset-owners` |

### [2.6 `daemon.ts`](../src/daemon.ts)

| Symbol | Line | Contents |
|---|---|---|
| `HubLike`, `AgentLike`, `DaemonDeps` | [23-55](../src/daemon.ts#L23-L55) | interfaces the daemon uses; tests replace the cores |
| `Entry` | [57](../src/daemon.ts#L57) | opening promise, agent, approval mode, open attaches, running calls, idle timer |
| `Daemon.start` | [82](../src/daemon.ts#L82) | lock, session numbers, listener, hub, stop timer |
| `Daemon.routes` | [119](../src/daemon.ts#L119) | the seven routes |
| `Daemon.shutdown` | [148](../src/daemon.ts#L148) | stop sequence |
| `Daemon.perform` | [186](../src/daemon.ts#L186) | perform step in the action's session |
| `onSession`, `onTitle`, `onAttach`, `onCall` | [192-269](../src/daemon.ts#L192-L269) | route handlers |
| `applyTitle`, `rename`, `rememberTitle`, `takeTitle` | [210-237](../src/daemon.ts#L210-L237) | title of an open, a stored or an unknown session; remembered titles, at most 100 |
| `Daemon.entry`, `refuseWhenStopping`, `folder`, `openAgent` | [282-327](../src/daemon.ts#L282-L327) | session registry; status 503 while stopping, 400 for an ID that is not a session ID; new session number and name |
| `scheduleIdle`, `closeAgent`, `scheduleStop` | [329-355](../src/daemon.ts#L329-L355) | idle close of a session, idle stop of the daemon |
| `runDaemon` | [363](../src/daemon.ts#L363) | process entry: data folder, start, ready line, signal handlers |
| `productionDeps` | [387](../src/daemon.ts#L387) | token, `Hub.open`, `openSessionAgent` |
| `readyChannel` | [404](../src/daemon.ts#L404) | ready line to fd `SIMPLEX_MCP_READY_FD`, or stdout |

### [2.7 `hub.ts`](../src/hub.ts)

| Symbol | Line | Contents |
|---|---|---|
| `Hub.open` | [26](../src/hub.ts#L26) | hub core, Main group found or created, `Actions`, `Control`, owner setup, events |
| `Hub.mainGroup`, `owner` | [50-57](../src/hub.ts#L50-L57) | hub profile name; owners and link |
| `Hub.configure` | [63](../src/hub.ts#L63) | live expiry; rename of the Main group and the hub profile |
| `Hub.resetOwners` | [72](../src/hub.ts#L72) | `Control.reset`, serialized with Main group events |
| `Hub.stop` | [76](../src/hub.ts#L76) | actions stop, queued events, Main group deliveries, core close |
| `Hub.onEvent` | [91](../src/hub.ts#L91) | Main group messages and membership events |

`Actions` hooks are bound in [`hub.ts:31-37`](../src/hub.ts#L31-L37): `refusal`, `saveCounter`, `postCard` and `outcome` to `Control`; `perform` to the daemon.

### [2.8 `agent.ts`](../src/agent.ts)

| Symbol | Line | Contents |
|---|---|---|
| `sessionName` | [31](../src/agent.ts#L31) | session title, else the project folder name, else `Claude Code` |
| `profileName` | [37](../src/agent.ts#L37) | `validName("SimpleX MCP - " + name)` |
| `openSessionAgent` | [41](../src/agent.ts#L41) | session core, item watch, `AgentTools`, event dispatch, rename, close |

### [2.9 `core.ts`](../src/core.ts)

| Symbol | Line | Contents |
|---|---|---|
| `networkCommand` | [14](../src/core.ts#L14) | `/network socks=off smp-proxy=always smp-proxy-fallback=protected` |
| `openCore` | [24](../src/core.ts#L24) | `ChatApi.init` with prefix `FOLDER/simplex`, active user or a new one, network config, event buffer, `startChat`, profile update |
| `deletePresetContacts` | [42](../src/core.ts#L42) | the contact the core adds to a new user, deleted when it has no connection |
| `updatedUser` | [52](../src/core.ts#L52) | display name and peer type `agent` applied with one `apiUpdateProfile` when they differ |
| `deliveries` | [59](../src/core.ts#L59) | wait for pending deliveries to the members of one group, polled |
| `bufferEvents` | [80](../src/core.ts#L80) | events received before the handler is installed are queued and replayed in order |

### [2.10 `tools.ts`](../src/tools.ts)

| Symbol | Line | Contents |
|---|---|---|
| `ConnectInput`, `SendInput`, `ReceiveInput`, `LeaveInput` | [31-57](../src/tools.ts#L31-L57) | tool inputs |
| `PlanSummary` | [63](../src/tools.ts#L63) | card kind, chat type, name, SimpleX name, peer type, description, welcome, commands, `takesMessage` |
| `ChatFields` | [75](../src/tools.ts#L75) | chat fields of `connect` and `list` results |
| `Agent` | [89](../src/tools.ts#L89) | session number, `ChatApi`, user, `Actions`, `ChatItemsWatch`, standing approver lookup |
| schemas | [120-300](../src/tools.ts#L120-L300) | input and output JSON schemas |
| descriptions, `instructions` | [302-333](../src/tools.ts#L302-L333) | tool descriptions and server instructions |
| `definitions` | [343](../src/tools.ts#L343) | name, description, schemas and action flag of the five tools |
| `registerTools` | [355](../src/tools.ts#L355) | `registerTool` for each definition on the session's `McpServer`; `_meta` in mode `host` |
| `AgentTools.call` | [370](../src/tools.ts#L370) | handler result to `CallToolResult`; errors to `isError` |
| `AgentTools.perform` | [379](../src/tools.ts#L379) | perform step of an approved action |
| `onContactConnected`, `onUserJoinedGroup` | [390-398](../src/tools.ts#L390-L398) | completion of connect actions by events |
| `actionResult` | [434](../src/tools.ts#L434) | wait for the decision, result fields, removal |
| `agentChats`, `knownChat` | [702-715](../src/tools.ts#L702-L715) | the session's chats; a checked chat reference |
| `classifyPlan` | [811](../src/tools.ts#L811) | `connectionPlan` response to a join plan or a known chat |
| `chatSubject`, `isConnected`, `isActive`, `memberCurrent` | [911-964](../src/tools.ts#L911-L964) | chat and member predicates shared with `control.ts` |
| `actionKey` | [988](../src/tools.ts#L988) | JSON of session number, tool and action fields |
| `toolCall` | [996](../src/tools.ts#L996) | `ServerContext` to `ToolCall`: abort signal and progress notifications |

### [2.11 `actions.ts`](../src/actions.ts)

| Symbol | Line | Contents |
|---|---|---|
| `Mode`, `State` | [5-6](../src/actions.ts#L5-L6) | `requests`, `host`; `pending`, `performing`, `done`, `failed`, `rejected`, `expired` |
| `lostMessage`, `stoppingMessage` | [11-12](../src/actions.ts#L11-L12) | refusal texts |
| `ToolError` | [16](../src/actions.ts#L16) | errors the model can correct; text returned as is |
| `ActionRecord` | [31](../src/actions.ts#L31) | number, session, mode, key, times, change signal, standing flag, state, card, decider, open calls, interrupted flag, error |
| `ConnectAction`, `SendAction`, `LeaveAction` | [49-82](../src/actions.ts#L49-L82) | per-tool fields |
| `ActionHooks` | [88](../src/actions.ts#L88) | `refusal`, `saveCounter`, `postCard`, `perform`, `outcome` |
| `Actions` | [96](../src/actions.ts#L96) | expiry hours and counter, both public and live; action map, expiry timers by action number, posting and outcome promises |
| `create` | [123](../src/actions.ts#L123) | refusal checks, number, log line, counter write, card |
| `ofSession` | [160](../src/actions.ts#L160) | the actions of one session |
| `openCall`, `closeCall` | [175-182](../src/actions.ts#L175-L182) | open call count; the last call's end removes an expired decided action |
| `expireDue` | [184](../src/actions.ts#L184) | expiry of all due actions by the wall clock, unless stopping |
| `decided` | [190](../src/actions.ts#L190) | wait with progress until a decided state or the abort signal |
| `stop` | [224](../src/actions.ts#L224) | cancellation of pending and performing actions |
| `summary`, `performedText`, `progressText` | [317-353](../src/actions.ts#L317-L353) | log and progress texts |

### [2.12 `control.ts`](../src/control.ts)

| Symbol | Line | Contents |
|---|---|---|
| `ControlData` | [23](../src/control.ts#L23) | `{control: true, nextAction}` |
| `SessionView`, `Sessions` | [28-39](../src/control.ts#L28-L39) | open sessions as `Control` sees them: number, display name, approval mode, `ChatApi`, chat lookups |
| `ChatApproval`, `ChatTarget`, `OwnerRequest` | [50-64](../src/control.ts#L50-L64) | standing approval; `S:@N`; parsed owner command |
| `Control.owners`, `link` | [85-91](../src/control.ts#L85-L91) | owner names; current one-time link |
| `Control.load` | [93](../src/control.ts#L93) | lost check, group name and menu, member list |
| `Control.start` | [102](../src/control.ts#L102) | owner setup: a link when no owner exists |
| `Control.refusal` | [108](../src/control.ts#L108) | bytes left for the link texts of a connect card, card size, owner presence |
| `Control.standingApprover` | [117](../src/control.ts#L117) | approver name of a chat's standing approval |
| `Control.saveCounter` | [122](../src/control.ts#L122) | queued counter writes |
| `Control.postCard`, `outcome` | [128-136](../src/control.ts#L128-L136) | card post and card edit |
| `Control.onMessage` | [138](../src/control.ts#L138) | owner commands, queued one at a time |
| `Control.onEvent` | [150](../src/control.ts#L150) | membership changes and loss of the Main group |
| `Control.ownerCommand` | [168](../src/control.ts#L168) | command dispatch and replies |
| `Control.rename`, `reset` | [199-211](../src/control.ts#L199-L211) | new group name; new group and link |
| `Control.header` | [217](../src/control.ts#L217) | `Session S · NAME`, or `Session S` when the session is not open |
| `approveChat`, `revoke`, `chatsText` | [222-265](../src/control.ts#L222-L265) | standing approvals; `/list` grouped by session |
| `ownerJoined`, `lose`, `openLink` | [267-285](../src/control.ts#L267-L285) | first owner; lost group; one-time link |
| `findControlGroup`, `createControlChat`, `resetControlChat` | [328-356](../src/control.ts#L328-L356) | Main group lookup by marker, creation, reset |
| `isControlEvent` | [358](../src/control.ts#L358) | membership events of the Main group |
| `parseCommand` | [374](../src/control.ts#L374) | one-line `/keyword params` |
| `cardText`, `requestLines`, `commandsLine`, `pendingLine` | [381-439](../src/control.ts#L381-L439) | card text per state and the action's mode |
| `largestCard`, `encodedLength` | [441-449](../src/control.ts#L441-L449) | card size check |
| `codeSpan`, `safeName`, `mentionText` | [487-499](../src/control.ts#L487-L499) | quoting of peer text |
| `chatTarget`, `ownerRequest` | [527-550](../src/control.ts#L527-L550) | `S:@N` and command words to an `OwnerRequest` |
| `losesControl`, `isLost` | [552-567](../src/control.ts#L552-L567) | loss of the Main group |

### [2.13 `messages.ts`](../src/messages.ts)

| Symbol | Line | Contents |
|---|---|---|
| `Message` | [11](../src/messages.ts#L11) | message item of tool results |
| `ToolCall` | [29](../src/messages.ts#L29) | abort signal and progress callback of one call |
| `Collected` | [34](../src/messages.ts#L34) | collected items, change signal, IDs of the items over the first 100, time of the last change |
| `Changes` | [41](../src/messages.ts#L41) | set of wake callbacks |
| `ChatItemsWatch` | [58](../src/messages.ts#L58) | listeners per chat reference; received message items gathered by `collect` |
| `chatRef`, `isChatRef` | [107-120](../src/messages.ts#L107-L120) | `@N` and `#N` references |
| `toMessage` | [138](../src/messages.ts#L138) | chat item to `Message` |
| `chatCommand`, `getChat`, `markRead` | [172-184](../src/messages.ts#L172-L184) | raw chat commands with response type checks |
| `unreadItems` | [186](../src/messages.ts#L186) | unread items of a chat after its `initial=1` read, paged |
| `reportProgress`, `nextChange`, `awaitResponses` | [202-232](../src/messages.ts#L202-L232) | waits |
| `validName`, `viewName` | [238-264](../src/messages.ts#L238-L264) | display name rules and `@name` form |
| `fullDomainName` | [266](../src/messages.ts#L266) | `SimplexDomain` to `name.simplex` or `name.testing` |

### [2.14 `hook.ts`](../src/hook.ts), [`owner.ts`](../src/owner.ts), [`qr.ts`](../src/qr.ts)

| Symbol | Line | Contents |
|---|---|---|
| `sessionStart` | [hook.ts:28](../src/hook.ts#L28) | Node.js check, options, `ensureDaemon`, `/title`, link output; every failure as `systemMessage` |
| `linkText`, `resetOwnersText` | [owner.ts:7-15](../src/owner.ts#L7-L15) | owners, or the link with a QR code on a terminal |
| `showLink`, `resetOwners` | [owner.ts:17-26](../src/owner.ts#L17-L26) | `ensureDaemon`, then `/status` data or `/reset-owners` |
| `qrCodeText` | [qr.ts:12](../src/qr.ts#L12) | error correction level L, margin 2, two modules per text line; `ansi` colors or `plain` block characters |

### [2.15 `config.ts`](../src/config.ts), [`files.ts`](../src/files.ts), [`log.ts`](../src/log.ts)

| Symbol | Line | Contents |
|---|---|---|
| `defaultConfig`, `configVariables` | [config.ts:12-18](../src/config.ts#L12-L18) | defaults; `CLAUDE_PLUGIN_OPTION_*` names |
| `readConfig` | [config.ts:25](../src/config.ts#L25) | blank values take the default; invalid ones throw `ConfigError` |
| `dataRoot`, `hubFolder`, `sessionFolder` | [files.ts:21-37](../src/files.ts#L21-L37) | `SIMPLEX_MCP_HOME` or `~/.simplex-mcp`; `_hub`; `_sessions/<lowercase ID>` after the UUID check |
| `readSessionFile`, `writeSessionFile`, `sessionNumbers` | [files.ts:39-55](../src/files.ts#L39-L55) | `session.json` |
| `socketPath` | [files.ts:57](../src/files.ts#L57) | Unix socket or Windows pipe |
| `openLog` | [files.ts:68](../src/files.ts#L68) | `daemon.log`, truncated when over 10 MB, mode 0600 |
| `createDataFolder` | [files.ts:76](../src/files.ts#L76) | `mkdir -p` mode 0700; `chmod 0700` except on Windows |
| `acquireLock`, `releaseLock` | [files.ts:81-96](../src/files.ts#L81-L96) | `lock` with the PID |
| `serverToken` | [files.ts:98](../src/files.ts#L98) | `token`: created exclusively through a hard link of a temporary file, or read |
| `ExitError`, `oneLine`, `log`, `logError` | [log.ts:12-28](../src/log.ts#L12-L28) | exit code error; one-line folding; stdout and stderr lines |
| `errorText`, `chatErrorText` | [log.ts:30-38](../src/log.ts#L30-L38) | error text, folded to one line: the apps' wording for known error shapes, otherwise the type chain |

---

## 3. State

### 3.1 Memory of the daemon

| Item | Holder | Written by | Read by |
|---|---|---|---|
| Open sessions | `Daemon.entries` by session ID ([daemon.ts:67](../src/daemon.ts#L67)) | `entry`, idle close | routes, `perform`, `Sessions` of `Control` |
| Remembered titles | `Daemon.titles` by lowercased session ID, at most 100 ([daemon.ts:70](../src/daemon.ts#L70)) | `/title` for an ID without `session.json` | `openAgent` of a new session, which removes the entry |
| Next session number | `Daemon.nextNumber` ([daemon.ts:79](../src/daemon.ts#L79)) | constructor: largest number in `_sessions` plus 1; `openAgent` | `openAgent` |
| Actions of all sessions | `Actions.actions` map ([actions.ts:100](../src/actions.ts#L100)) | `create`, `remove`, expiry | tools, `Control`, idle check |
| Expiry timers | `Actions.expiries` ([actions.ts:101](../src/actions.ts#L101)) | `create`, a timer that fires before its time; cleared by `remove` and `stop` | |
| Approval mode of a session | `Entry.mode` ([daemon.ts:60](../src/daemon.ts#L60)); `requests` until the session's first `/session` | each `/session` of that session ([daemon.ts:194](../src/daemon.ts#L194)) | `create` through `AgentTools`, `standingApprover`, `/approve S:CHAT all` |
| Approval mode of an action | `ActionRecord.mode` ([actions.ts:34](../src/actions.ts#L34)) | `create` | state at creation, cards, the perform log line |
| Expiry hours | `Actions.expireHours` ([actions.ts:105](../src/actions.ts#L105)) | `Hub.open`, `Hub.configure` at each `/session` and `/title` | `create` |
| Open calls of an action | `ActionRecord.open` ([actions.ts:43](../src/actions.ts#L43)) | `openCall` and `closeCall` from `actionResult` ([tools.ts:435](../src/tools.ts#L435), [:443](../src/tools.ts#L443)) | `list`, expiry |
| Collected replies | `ConnectAction.messages`, `SendAction.responses` ([actions.ts:54](../src/actions.ts#L54), [:72](../src/actions.ts#L72)) | `ChatItemsWatch.collect` | `connect`, `send` results |
| Item listeners | one `ChatItemsWatch` per session ([agent.ts:44](../src/agent.ts#L44)) | `collect` | the session's event dispatch |
| Main group members | `Control.members` ([control.ts:67](../src/control.ts#L67)) | `refreshMembers` ([:302](../src/control.ts#L302)) | owner checks |
| One-time link | `Control.linkText` ([control.ts:71](../src/control.ts#L71)) | `openLink`, `deleteLink` | `/session`, `/title`, `/status`, refusals |

The session process keeps only its connection state: the connect promise and the `/attach` request ([session.ts:55-60](../src/session.ts#L55-L60)).

### 3.2 Chat database custom data

| Item | Database | Entity | Value | Written by | Read by |
|---|---|---|---|---|---|
| Main group marker and counter | hub | Main group | `{simplexMcp: {control: true, nextAction: N}}` | [`createControlChat`](../src/control.ts#L338-L349), [`saveCounter`](../src/control.ts#L122) | [`findControlGroup`](../src/control.ts#L328), [`controlDataOf`](../src/control.ts#L333) |
| Standing approval | session | contact or group of the session | `{...other keys, simplexMcp: {approvedBy, approvedById, approvedAt}}`, `approvedById` a Main group member ID | [`approveChat`](../src/control.ts#L222), [`revoke`](../src/control.ts#L238) | [`standingApprover`](../src/control.ts#L117) |

Each core stores its profile, chats, items and members in `FOLDER/simplex_chat.db`, and connections and queues in `FOLDER/simplex_agent.db` (file prefix `FOLDER/simplex`, [core.ts:26](../src/core.ts#L26)).

### 3.3 Files

ROOT is `SIMPLEX_MCP_HOME`, resolved to an absolute path, or `~/.simplex-mcp` ([files.ts:21-24](../src/files.ts#L21-L24)).

| Path | Contents | Mode | Written | Read |
|---|---|---|---|---|
| `ROOT/` | data root | 0700, set at every `createDataFolder` except on Windows | every client and daemon start ([files.ts:76](../src/files.ts#L76)) | |
| `ROOT/token` | base64url of 32 random bytes | 0600 | first client or daemon ([files.ts:98-113](../src/files.ts#L98-L113)) | every client and daemon |
| `ROOT/lock` | PID and a newline | 0600 | daemon start ([files.ts:128-136](../src/files.ts#L128-L136)) | next daemon start |
| `ROOT/daemon.sock` | Unix socket | | `listen` ([socket.ts:40-47](../src/socket.ts#L40-L47)) | clients |
| `ROOT/daemon.log` | stdout and stderr of the daemon | 0600 | spawner, truncated when over 10 MB ([files.ts:68-74](../src/files.ts#L68-L74)) | people |
| `ROOT/_hub/` | hub databases | 0700 | `openCore` ([core.ts:25](../src/core.ts#L25)) | hub core |
| `ROOT/_sessions/ID/` | session databases | 0700 | `openAgent`, `openCore` | session core |
| `ROOT/_sessions/ID/session.json` | `{number, name, project, created}` | 0600 | new session, rename, title of a stored session ([daemon.ts:215-223](../src/daemon.ts#L215-L223), [:322-324](../src/daemon.ts#L322-L324)) | `openAgent`, `sessionNumbers` |

The socket path is `ROOT/daemon.sock` when it fits in 103 bytes, else `TMP/simplex-mcp-H.sock`; on Windows it is `\\.\pipe\simplex-mcp-H`. H is the first 16 hex digits of the SHA-256 of ROOT ([files.ts:57-62](../src/files.ts#L57-L62)).

The core library is downloaded once into `CACHE/core-<release>/sqlite`, and the Node.js add-on into `CACHE/<add-on release>/`, each with a `Downloading ...` line on stderr, which goes to `daemon.log`. CACHE is `$XDG_CACHE_HOME/simplex-chat` or `~/.cache/simplex-chat`; on macOS `~/Library/Caches/simplex-chat`; on Windows `%LOCALAPPDATA%\simplex-chat` ([libs.ts:41](../../simplex-chat-nodejs/src/libs.ts#L41), [:116](../../simplex-chat-nodejs/src/libs.ts#L116), [:129](../../simplex-chat-nodejs/src/libs.ts#L129)).

---

## 4. Process lifecycle

### 4.1 SessionStart hook

`node plugin/launch.js hook session-start` runs `npx -y @simplex-chat/mcp@VERSION hook session-start`; `sessionStart` reads the hook input from stdin ([cli.ts:49-51](../src/cli.ts#L49-L51), [hook.ts:28-46](../src/hook.ts#L28-L46)):

1. Node.js major version below 22: `systemMessage` `SimpleX MCP needs Node.js 22 or later; found V.`
2. Parse `{session_id, session_title}`, read the options; a `ConfigError` becomes the `systemMessage`.
3. `ensureDaemon` ([4.3](#43-daemon-spawn-and-version-check)).
4. `POST /title` with `{id: session_id, name: session_title, config}`. The hook records the title and opens no profile; the session process creates the profile ([4.5](#45-session-profiles)).
5. Owners present, or no link: empty output. Otherwise a `systemMessage` with the link of the reply's Main group and a `plain` QR code, and `additionalContext` with the link.
6. Any error: `systemMessage` `SimpleX MCP: TEXT`; a not-running error reads `The SimpleX daemon stopped; it starts again with the next tool call.` The exit code is 0.

### 4.2 Session process

`startSession` ([session.ts:34-53](../src/session.ts#L34-L53)):

1. `CLAUDE_CODE_SESSION_ID` missing: exit 2 `simplex-mcp session runs from the Claude Code plugin.`; not a UUID: exit 2 `Not a Claude Code session ID: ID`.
2. The options are read; a `ConfigError` is kept, and every tool call returns its text as a tool error without contacting the daemon.
3. `McpServer` `simplex-mcp` with the package version and the instructions, which name no profile; the five tools are registered with the mode of this session's options.
4. The stdio transport connects; when it closes, the `/attach` request is aborted.
5. Without a config error, the connection to the daemon opens in the background, so the MCP startup is not delayed.

Connection ([session.ts:123-140](../src/session.ts#L123-L140)): `ensureDaemon`, `POST /session`, then `POST /attach` until its first line. The attach stream ending or failing drops the connection; the next call opens a new one.

Call ([session.ts:81-113](../src/session.ts#L81-L113)):

| Outcome | Result |
|---|---|
| result line | the `CallToolResult`, or the cancelled result when it is `null` |
| stream ends without a result line | connection dropped; `The SimpleX daemon stopped; repeat the call.` |
| connection error | connection dropped; the same text |
| status 409 (unknown session, before the call runs) | one retry after a new connection |
| status 503 (daemon stopping) | `The SimpleX daemon stopped; repeat the call.` |
| other `SocketError` | its message |
| the MCP request aborted | no result |

### 4.3 Daemon spawn and version check

`ensureDaemon(version)` ([client.ts:32-44](../src/client.ts#L32-L44)):

1. `GET /status`; not running (`ENOENT`, `ECONNREFUSED`) counts as no daemon.
2. A daemon with a lower version is sent `POST /stop` and counts as no daemon. A higher version is used.
3. No daemon: `spawnDaemon`, then `GET /status`.
4. A protocol other than `1`: `ExitError` `Restart Claude Code to update the SimpleX plugin.`

`spawnDaemon` ([client.ts:68-89](../src/client.ts#L68-L89)): `daemon.log` opened for append (truncated first when over 10 MB); `process.execPath dist/cli.js daemon`, detached, `windowsHide`, stdin ignored, stdout and stderr to the log, fd 3 a pipe; environment with `SIMPLEX_MCP_HOME` and `SIMPLEX_MCP_READY_FD=3`. The first line on fd 3 decides: `ready` or `running` succeed; `error: TEXT` fails with TEXT; any other end fails with `The SimpleX daemon exited; see ROOT/daemon.log`; a spawn error fails with its message; after 5 minutes it fails with `The SimpleX daemon did not start in 5 minutes.` The pipe is then destroyed and the child unreferenced.

The daemon inherits the spawner's environment, so its options are those of the session or hook that started it.

### 4.4 Daemon start

`runDaemon` ([daemon.ts:363-385](../src/daemon.ts#L363-L385)), `Daemon.start` ([:82-101](../src/daemon.ts#L82-L101)):

1. `createDataFolder(ROOT)`; the token is read or created.
2. The lock is taken; when another daemon holds it, the ready line is `running` and the process ends.
3. The next session number is computed from `_sessions/*/session.json`.
4. The socket listener starts, so a second daemon that reports `running` while the hub opens leaves its spawner a daemon to reach. Every route waits for the hub to open first, `/status` included ([daemon.ts:134-139](../src/daemon.ts#L134-L139)).
5. `Hub.open` with the options read from the environment ([hub.ts:26-48](../src/hub.ts#L26-L48)):
   1. `openCore(ROOT/_hub, main_group)`: active user, or a new one with that display name, empty full name and peer type `agent`; the network config; events buffered; `startChat`; for a new user, the preset contact `Ask SimpleX Team` is deleted (a failure is logged, not thrown; an existing user is left as it is); a different display name or peer type is applied ([core.ts:24-40](../src/core.ts#L24-L40)).
   2. The Main group is found by its marker (the latest one when several exist), or created with counter 1 ([control.ts:328-349](../src/control.ts#L328-L349)).
   3. `Actions` with the expiry and the counter from the marker; `Control` with the group name.
   4. `Control.load`: a lost group sets `Actions.lost`; otherwise the group name and command menu are updated when they differ, and members are read ([control.ts:93-100](../src/control.ts#L93-L100)).
   5. `Control.start`: without owners, `No owners in the Main group.` when other members exist, then a new one-time link with role `member` ([control.ts:102-106](../src/control.ts#L102-L106), [:280-285](../src/control.ts#L280-L285)).
   6. Buffered events are replayed, then live events are dispatched.
6. The idle stop timer starts.
7. `SimpleX MCP daemon VERSION, data folder ROOT` is logged and the ready line `ready` is written; SIGINT and SIGTERM run the stop.

An error after the lock releases it; the ready line is `error: TEXT` and the exit code 1. When the options are invalid or the hub fails to open, the requests that wait for the hub get the error with status 500, and the listener is closed before the lock is released ([daemon.ts:88-94](../src/daemon.ts#L88-L94), [:113-117](../src/daemon.ts#L113-L117)).

### 4.5 Session profiles

`Daemon.entry` and `openAgent` ([daemon.ts:282-327](../src/daemon.ts#L282-L327)):

- The first `/session`, `/attach` or `/call` for an ID creates its entry; a close of the same ID still in progress is awaited first.
- An ID that is not a session ID: status 400 `Not a Claude Code session ID: ID`, and no entry.
- A folder without `session.json`: with a `/session` request, the folder is created and `{number, name, project, created}` written, with the next number and `sessionName(title, project)`, where the title is the one remembered for the ID, which is then forgotten; without it, status 409 `Unknown session; restart the Claude Code session.`
- `openSessionAgent` opens the core with `profileName(name)`; a new profile starts with no chats. It dispatches the core's events ([agent.ts:41-77](../src/agent.ts#L41-L77)).
- A failed open removes the entry.
- `/session` carries no name and keeps the stored one; every `/session` applies its options to the hub ([daemon.ts:197](../src/daemon.ts#L197)).
- `/title` applies its options to the hub, and with a non-blank name: an open or opening profile is renamed and the name written to `session.json` ([daemon.ts:218-223](../src/daemon.ts#L218-L223)); a stored session gets the name in `session.json`, and its profile takes it at the next open; for an ID without `session.json` the name is remembered in memory, at most 100 IDs, the oldest dropped. `/title` creates no entry, folder or number, opens no profile, and leaves the idle timers alone ([daemon.ts:201-237](../src/daemon.ts#L201-L237)).
- After `/clear`, and after `--continue` or `--resume` without an ID, Claude Code gives the hook a new session ID while the session process keeps its own, so the title of the new ID stays remembered until it is dropped.

### 4.6 Event dispatch

Each `onAny` receiver is awaited by the library before the next event is read ([api.ts:242](../../simplex-chat-nodejs/src/api.ts#L242)).

Hub core ([hub.ts:91-104](../src/hub.ts#L91-L104)):

| Event | Condition | Handler |
|---|---|---|
| `newChatItems` | item of the Main group, main scope | [`Control.onMessage`](../src/control.ts#L138) |
| `joinedGroupMember`<br>`joinedGroupMemberConnecting`<br>`connectedToGroupMember`<br>`memberRole`<br>`leftMember`<br>`deletedMember`<br>`deletedMemberUser`<br>`groupDeleted` | event of the Main group | [`Control.onEvent`](../src/control.ts#L150), queued one at a time with renames and resets |
| any event | after `Hub.stop` started | ignored |

Session core ([agent.ts:46-59](../src/agent.ts#L46-L59)):

| Event | Handler |
|---|---|
| `newChatItems` | [`ChatItemsWatch.deliver`](../src/messages.ts#L88) |
| `chatItemUpdated` | [`ChatItemsWatch.update`](../src/messages.ts#L92) |
| `chatItemsDeleted` | [`ChatItemsWatch.remove`](../src/messages.ts#L96) of each `deletedChatItem` |
| `contactConnected` | [`AgentTools.onContactConnected`](../src/tools.ts#L390) |
| `userJoinedGroup` | [`AgentTools.onUserJoinedGroup`](../src/tools.ts#L395) |

Owner commands are queued in a chain of their own, independent of the membership chain ([control.ts:144-147](../src/control.ts#L144-L147)).

### 4.7 Attach, idle close and idle stop

| Timer | Start | Fires | Code |
|---|---|---|---|
| session idle, 10 min | entry created; last `/attach` closed; last call ended with no attach; `/session` with no attach | closes the profile when it has no running call and no action; otherwise starts again | [daemon.ts:329-339](../src/daemon.ts#L329-L339) |
| daemon idle, 10 min | daemon start; last entry removed | stops the daemon | [daemon.ts:349-355](../src/daemon.ts#L349-L355) |

An open `/attach` clears the session's timer ([daemon.ts:244](../src/daemon.ts#L244)); a new entry clears the daemon's ([:287](../src/daemon.ts#L287)).

### 4.8 Stop

`POST /stop`, SIGINT, SIGTERM or the idle timer run one shared stop ([daemon.ts:103-106](../src/daemon.ts#L103-L106), [:148-172](../src/daemon.ts#L148-L172)):

1. Timers are cleared; new entries are refused with status 503 `The SimpleX daemon is stopping.`
2. The listener is closed; open `/attach` and `/call` streams are destroyed, which drops the session processes' connections.
3. `Hub.stop` ([hub.ts:76-83](../src/hub.ts#L76-L83)): events are ignored; `Actions.stop` refuses new actions with `The SimpleX daemon is stopping.`, clears the expiry timers, awaits cards being posted, marks pending and performing actions interrupted, aborts the perform signal and edits their cards ([actions.ts:224-235](../src/actions.ts#L224-L235)); queued Main group events are awaited; pending deliveries to Main group members are polled every 100 ms for at most 10 s ([core.ts:59-68](../src/core.ts#L59-L68)); the hub core is closed. This is the daemon's one deliberate poll, run only during the stop, because the core emits no status event when an edit of an already delivered card is delivered, so no event marks the end of the deliveries.
4. `Cancelled K action(s).` when K > 0.
5. The session cores are closed, and closes in progress awaited.
6. The lock is removed and `Stopped.` is logged.
7. After the `/stop` reply is sent, or at once for a signal or the timer, all connections are closed and the process exits with code 0.

### 4.9 The lock

```
acquireLock(ROOT):                                   files.ts:81
  create ROOT/lock exclusively (flag wx), PID       :128-136
  exists:
    PID = number in the file
    PID valid, other than ours, running:            :85, :143-150
      LockedError "Another daemon uses ROOT (PID N). If none runs, remove ROOT/lock."
    remove the file; create it exclusively again    :86-87
      exists again: the same error
releaseLock(ROOT): remove ROOT/lock, ENOENT ignored  :90-96
```

`runDaemon` turns `LockedError` into the ready line `running` ([daemon.ts:371](../src/daemon.ts#L371)).

---

## 5. Approval model

### 5.1 Modes

| Mode | State of a new action | Card | `_meta` of `connect`, `send`, `leave` |
|---|---|---|---|
| `requests` | `pending`<br>`performing` for a send under a standing approval | card with commands<br>outcome card after a standing send | none |
| `host` | `performing` | outcome card after the perform | `{"anthropic/requiresUserInteraction": true}` |

Each session has its own mode: the daemon keeps the `approve` value of the session's latest `/session` with its entry, and `requests` before the first one ([daemon.ts:194](../src/daemon.ts#L194), [:288](../src/daemon.ts#L288)). `/title` does not change it. A new action records the mode of its session, and its state, cards and log lines follow that recorded mode ([actions.ts:136](../src/actions.ts#L136), [control.ts:383](../src/control.ts#L383)). The expiry and the Main group name follow the latest `/session` or `/title` of any session ([hub.ts:63-69](../src/hub.ts#L63-L69)). Each session process sets `_meta` from its own options at its start ([session.ts:48](../src/session.ts#L48), [tools.ts:361](../src/tools.ts#L361)). In both modes a new action is refused unless the Main group has an owner ([control.ts:113-114](../src/control.ts#L113-L114)).

### 5.2 Owners

| Term | Rule | Code |
|---|---|---|
| human | Main group member other than the hub profile's membership, status `connected` or `complete` | [control.ts:319-321](../src/control.ts#L319-L321), [tools.ts:949-951](../src/tools.ts#L949-L951) |
| owner, approver | human with role `owner` | [control.ts:310-312](../src/control.ts#L310-L312), [:323-325](../src/control.ts#L323-L325) |

Session profiles are not members of the Main group. The command sender's member record from the chat item is checked ([control.ts:143](../src/control.ts#L143), [:172](../src/control.ts#L172)). The owner count for refusals and standing approvals is taken from `Control.members`, refreshed at load and at each membership event.

| Change | Daemon reaction | Code |
|---|---|---|
| a human joins while no owner exists | link deleted, role `owner`, greeting posted, `NAME joined (member ID) and is now an owner. The link is deleted.` logged | [control.ts:161](../src/control.ts#L161), [:267-273](../src/control.ts#L267-L273) |
| owners drop to zero | `No owners in the Main group.`, new link with role `member` | [control.ts:162-165](../src/control.ts#L162-L165) |
| `deletedMemberUser`, `groupDeleted`, or the hub profile's role below `owner` | `Actions.loseControl`: pending actions set to `failed`; later cards left unedited; `The agent lost its Main group. Run /simplex:reset-owners in Claude Code.` logged | [control.ts:158](../src/control.ts#L158), [:275-278](../src/control.ts#L275-L278), [actions.ts:216-222](../src/actions.ts#L216-L222) |
| `POST /reset-owners` | the group deleted for all members (left first when current and not owner), a new one with the same counter, control regained, new link | [hub.ts:72-74](../src/hub.ts#L72-L74), [control.ts:204-211](../src/control.ts#L204-L211), [:351-356](../src/control.ts#L351-L356) |
| option `main_group` changed | group profile and hub profile renamed | [hub.ts:66-69](../src/hub.ts#L66-L69), [control.ts:199-202](../src/control.ts#L199-L202) |

### 5.3 Action state machine

Transitions, one per row:

| Event | From | To | Code |
|---|---|---|---|
| call creates the action, session mode `requests` | | `pending` | [actions.ts:136](../src/actions.ts#L136) |
| call creates the action, session mode `host` | | `performing` | [actions.ts:136](../src/actions.ts#L136) |
| `send` call creates the action under a standing approval | | `performing` | [actions.ts:136](../src/actions.ts#L136) |
| counter write or card post fails | `pending`<br>`performing` | `failed` | [actions.ts:257-268](../src/actions.ts#L257-L268) |
| stop arrives while the counter of a `performing` action is written | `performing` | `pending`, interrupted | [actions.ts:273](../src/actions.ts#L273) |
| Main group lost | `pending` | `failed` | [actions.ts:216-222](../src/actions.ts#L216-L222) |
| `/reject` | `pending` | `rejected` | [actions.ts:209-214](../src/actions.ts#L209-L214) |
| expiry time passes | `pending` | `expired` | [actions.ts:248-250](../src/actions.ts#L248-L250) |
| `/approve N`, `/approve S:CHAT N`, `/approve S:CHAT all` | `pending` | `performing` | [actions.ts:203-207](../src/actions.ts#L203-L207) |
| perform throws | `performing` | `failed` | [actions.ts:281-283](../src/actions.ts#L281-L283) |
| perform ends | `performing` | `done` | [actions.ts:279-280](../src/actions.ts#L279-L280) |

`create` ([actions.ts:123-158](../src/actions.ts#L123-L158)):

```
create(session, mode, key, fields, standingApprover?):
  state = mode host or standing ? performing : pending
  expires = now + expireHours
  refusal, first match:
    stopping                                  "The SimpleX daemon is stopping."
    Main group lost                           lostMessage
    20 actions of this session pending        pendingMessage
    20 actions of this session performing     performingMessage
    connect: link texts cut to the bytes its largest card leaves
    send or connect card over 15000 bytes     cardTooLong
    no owner                                  no-owner message with the link
  refusal: throw ToolError                    counter unchanged
  N = next; next += 1; store; log "Action N of session S: ..."
  post:
    saveCounter(next)
    state pending: card = postCard()
    either fails: state failed; log "Action N: failed: ERROR."
  after post:
    posted and performing:
      stopping: state = pending
      otherwise: perform in session S
```

An action is removed from memory when its result is returned to a call ([tools.ts:440](../src/tools.ts#L440)), or when it is decided, its open call count is 0 and its expiry time has passed; this is checked when its expiry timer fires, when it is decided and when its last open call ends ([actions.ts:253-255](../src/actions.ts#L253-L255), [:181](../src/actions.ts#L181), [:293](../src/actions.ts#L293)). The action key includes the session number, so equal calls of two sessions are two actions ([tools.ts:988-990](../src/tools.ts#L988-L990)).

### 5.4 Cards

The daemon posts a card when a `pending` action is created, and edits it with `apiUpdateChatItem` at the outcome ([control.ts:128-136](../src/control.ts#L128-L136)). For an action without a card (mode `host`, standing sends) the outcome is posted as a new message. Each card starts with `Session S · PROFILE NAME`, or `Session S` when the session is not open ([control.ts:213-220](../src/control.ts#L213-L220)); the text per state is built by [`cardText`](../src/control.ts#L381-L400); forms are listed in [API: cards](api.md#22-cards).

Outcomes are posted after `done` and `failed` ([actions.ts:286-287](../src/actions.ts#L286-L287)), after `rejected` and `expired` ([:213](../src/actions.ts#L213), [:250](../src/actions.ts#L250)), and at stop ([:232](../src/actions.ts#L232)). They are skipped after the Main group is lost ([:232](../src/actions.ts#L232), [:287](../src/actions.ts#L287), [:297](../src/actions.ts#L297)).

### 5.5 Standing approvals

| Aspect | Rule | Code |
|---|---|---|
| grant | `/approve S:CHAT all` from an owner, session S open and in mode `requests` (a session in mode `host` stores nothing and replies `Approvals apply in mode requests; session S runs in mode host.`): custom data of CHAT in session S's database gets `simplexMcp: {approvedBy, approvedById, approvedAt}`, other keys kept; pending sends of session S to CHAT with a card are approved | [control.ts:222-236](../src/control.ts#L222-L236) |
| removal | `/revoke S:CHAT`, session S open: the `simplexMcp` key is removed, other keys kept | [control.ts:238-246](../src/control.ts#L238-L246) |
| use | session mode `requests`, and `approvedById` equal to the `groupMemberId` of a current owner of the Main group | [control.ts:117-120](../src/control.ts#L117-L120) |
| scope | `send` only | [tools.ts:589](../src/tools.ts#L589) |
| decider shown | `safeName(approvedBy)` | [control.ts:119](../src/control.ts#L119) |
| storage | session database; removed with the chat by `leave` | [tools.ts:649-654](../src/tools.ts#L649-L654) |

### 5.6 Expiry

| Rule | Code |
|---|---|
| `expires = created + expireHours × 3600000`, with the expiry hours at creation; option 1 to 23, default 23 | [actions.ts:133](../src/actions.ts#L133), [config.ts:46-51](../src/config.ts#L46-L51) |
| one unreferenced timer per action, set when it is created for its expiry time, removed when it fires or the action expires, and set again when it fires before that time | [actions.ts:151](../src/actions.ts#L151), [:237-246](../src/actions.ts#L237-L246) |
| `expireDue` checks all actions at each hub core event, at the start of each `/session`, `/title`, `/attach`, `/call` and `/status` request, and before each owner command, and does nothing once the daemon stops | [actions.ts:184-188](../src/actions.ts#L184-L188), [hub.ts:93](../src/hub.ts#L93), [daemon.ts:121-125](../src/daemon.ts#L121-L125), [:141-146](../src/daemon.ts#L141-L146), [control.ts:169](../src/control.ts#L169) |
| pending and due: `expired`, log `Action N expired.`, card edited to `Expired.` | [actions.ts:248-250](../src/actions.ts#L248-L250) |
| decided, no open call, due: removed from memory, and its timer cleared; a repeated call then creates a new action | [actions.ts:253-255](../src/actions.ts#L253-L255), [:168-173](../src/actions.ts#L168-L173) |
| connect wait for the other side: at most until `created + 24 h - 10 min` | [tools.ts:113](../src/tools.ts#L113), [:486](../src/tools.ts#L486) |

Timers follow the monotonic clock, which stops while the computer sleeps, so a timer can fire hours after the expiry time, or before it when the clock is set back. `Date.now()` is compared with `expires`, and after a sleep the hub core's reconnection events or the next daemon request expire the due actions while their cards can still be edited.

### 5.7 Action counter

| Rule | Code |
|---|---|
| initial value: `nextAction` of the Main group marker, or 1 | [hub.ts:31](../src/hub.ts#L31) |
| one counter for all sessions; a new action takes `next`, then `next` grows by 1 | [actions.ts:124](../src/actions.ts#L124), [:149](../src/actions.ts#L149) |
| the new `next` is written before the card is posted | [actions.ts:259-260](../src/actions.ts#L259-L260) |
| one write at a time, also after a failed write | [control.ts:122-126](../src/control.ts#L122-L126) |
| `/reset-owners` copies the value into the new Main group | [control.ts:206](../src/control.ts#L206) |
| a refused call leaves the counter unchanged | [actions.ts:148](../src/actions.ts#L148) |

---

## 6. Security boundaries

### 6.1 Socket

Checks in order of the request listener ([socket.ts:24-36](../src/socket.ts#L24-L36)):

| Check | Rule | Failure | Code |
|---|---|---|---|
| reach | Unix socket in ROOT, mode 0700 (or under the system temporary folder for a long path); Windows named pipe | | [files.ts:57-62](../src/files.ts#L57-L62) |
| token | `Authorization` equal to `Bearer <token>`, same length, `timingSafeEqual` | 401 `{"error":"Unauthorized"}` | [socket.ts:19-25](../src/socket.ts#L19-L25) |
| route | `METHOD PATH` in the route table | 404 `{"error":"Not found"}` | [socket.ts:26-27](../src/socket.ts#L26-L27) |
| body size | 1 MiB | 413 `Body too large` | [socket.ts:97-106](../src/socket.ts#L97-L106) |
| body | empty, or JSON | 400 `Invalid JSON` | [socket.ts:107-112](../src/socket.ts#L107-L112) |
| session ID | UUID format, before it becomes a path | 500 `Not a Claude Code session ID: ID` | [files.ts:34-37](../src/files.ts#L34-L37) |
| handler error | `SocketError` status, else 500 and a log line; after headers were sent, the connection is destroyed | | [socket.ts:30-35](../src/socket.ts#L30-L35) |

Node limits: `requestTimeout` 0, `headersTimeout` 60 s, `keepAliveTimeout` 5 s ([socket.ts:37-39](../src/socket.ts#L37-L39)). A stale socket file is removed before listening, except on Windows ([:40](../src/socket.ts#L40)). Clients use no connection pool ([socket.ts:90-91](../src/socket.ts#L90-L91)). When the client closes a `/call` stream before its end, the call's signal is aborted ([daemon.ts:257-259](../src/daemon.ts#L257-L259)); the session process closes it when the MCP request is aborted ([session.ts:99-103](../src/session.ts#L99-L103)).

### 6.2 Token

- The token is created once, exclusively, by the first client or daemon, and kept in `ROOT/token` with mode 0600 ([files.ts:98-113](../src/files.ts#L98-L113)).
- Any process that reads the token can act as any session; the token is the only check on Windows, where the named pipe has no folder permissions.
- A client reads the token once when it is created ([client.ts:19-22](../src/client.ts#L19-L22)).

### 6.3 Plugin

- The PreToolUse hook allows the tools matching `mcp__plugin_simplex_simplex__.*` ([hooks/hooks.json](../hooks/hooks.json), [plugin/allow.js](../plugin/allow.js)).
- `/simplex:link` and `/simplex:reset-owners` have `disable-model-invocation: true`, and `allowed-tools` pre-approves only their own `launch.js` command for the turn that invokes them ([skills/link/SKILL.md](../skills/link/SKILL.md), [skills/reset-owners/SKILL.md](../skills/reset-owners/SKILL.md)); no tool changes the options or the owners.
- `launch.js` runs `npx` with the version from the plugin's `package.json`; on Windows through the shell, because `npx.cmd` needs one ([plugin/launch.js](../plugin/launch.js)).

### 6.4 Peer-written text

| Destination | Rule | Code |
|---|---|---|
| card and reply names | folded to one line; a valid display name is plain, others a code span | [`safeName`](../src/control.ts#L491-L494) |
| card text lines | one code span per line; backticks become `'`; a leading or trailing space becomes U+00A0; blank lines empty | [`codeLines`, `codeSpan`](../src/control.ts#L483-L489) |
| reply quote on a send card | whitespace runs to one space, first 100 characters (code points), one code span | [control.ts:414](../src/control.ts#L414) |
| link texts on a connect card | code span lines in the bytes that the largest card form leaves; the line that does not fit is cut and ends in `…` inside its code span; later lines left out | [control.ts:463-477](../src/control.ts#L463-L477) |
| mention names | valid name: `@name` or `@'name'`; others a code span | [`mentionText`](../src/control.ts#L496-L499) |
| errors on cards | folded to one line, one code span | [control.ts:394](../src/control.ts#L394) |
| log names and errors of actions | folded to one line | [actions.ts:152-153](../src/actions.ts#L152-L153), [:333-340](../src/actions.ts#L333-L340), [control.ts:272](../src/control.ts#L272) |
| tool results | returned as data; schema descriptions and instructions mark `text`, `from`, `name`, `description`, `welcome` and `commands` with their labels and params as data, not instructions; the instructions take a chat's messages as prompts only when the user said so in the session, and limit what the agent shares about its user | [tools.ts:126-130](../src/tools.ts#L126-L130), [:147-148](../src/tools.ts#L147-L148), [:158-162](../src/tools.ts#L158-L162), [:331](../src/tools.ts#L331) |

The daemon writes the tappable form `/'...'` only in the card's command line, `/'approve ...'` and `/'reject ...'` ([control.ts:425-435](../src/control.ts#L425-L435)), and in `/'revoke S:CHAT'` of standing cards and replies ([:392](../src/control.ts#L392), [:235](../src/control.ts#L235)).

### 6.5 Chat references

| Rule | Code |
|---|---|
| `^[@#][1-9]\d{0,14}$`: 1 to 15 digits; the core reads `#(2^64 + N)` as `#N` | [messages.ts:118-120](../src/messages.ts#L118-L120) |
| owner commands: `S:CHAT` with S a positive number and CHAT a chat reference | [control.ts:527-530](../src/control.ts#L527-L530) |
| a store error of the chat read is reported as `No chat CHAT.` | [tools.ts:662-664](../src/tools.ts#L662-L664), [:708-715](../src/tools.ts#L708-L715) |
| the agent's own links are refused by `connect` (`ownLink` plans) | [tools.ts:830](../src/tools.ts#L830), [:848](../src/tools.ts#L848), [:870](../src/tools.ts#L870) |
| a session reaches only its own core: its chats, its actions in `list`, its key space | [daemon.ts:252-263](../src/daemon.ts#L252-L263), [tools.ts:698](../src/tools.ts#L698) |

The Main group belongs to the hub core, so no session's tools can reach it.

### 6.6 Owner commands

| Rule | Code |
|---|---|
| source: `groupRcv` text items in the main scope of the Main group | [hub.ts:95-99](../src/hub.ts#L95-L99), [control.ts:140](../src/control.ts#L140) |
| form: one line `/keyword params`, leading spaces allowed | [control.ts:374-379](../src/control.ts#L374-L379) |
| the help reply is sent to any sender; other commands are accepted from owners only | [control.ts:171-172](../src/control.ts#L171-L172) |
| commands run one at a time in arrival order | [control.ts:144-147](../src/control.ts#L144-L147) |
| `/approve` and `/reject` need a pending action with a card, and the action's session and chat when `S:CHAT` is given | [control.ts:186-190](../src/control.ts#L186-L190) |

### 6.7 Link and network

- The join link is a group link of the Main group with role `member`; it is logged to `daemon.log`, returned by `/session`, `/title` and `/status`, and deleted at the first join of a human while no owner exists ([control.ts:280-295](../src/control.ts#L280-L295), [:267-273](../src/control.ts#L267-L273)).
- Every core sets private message routing: `smp-proxy=always`, `smp-proxy-fallback=protected`, `socks=off` ([core.ts:14](../src/core.ts#L14), [:30](../src/core.ts#L30)).
- With `incognito: true` the daemon sends `incognito=on` with the connect command ([tools.ts:518](../src/tools.ts#L518)); the core then uses a random profile for that chat.

---

## 7. Limits

| Limit | Value | Code |
|---|---|---|
| pending actions | 20 per session | [actions.ts:8](../src/actions.ts#L8) |
| actions being performed | 20 per session | [actions.ts:9](../src/actions.ts#L9) |
| expiry | 1 to 23 h, default 23 | [config.ts:46-51](../src/config.ts#L46-L51) |
| connect wait for the other side | `created + 23 h 50 min` | [tools.ts:113](../src/tools.ts#L113) |
| send and connect card | 15000 bytes JSON-escaped, in its largest form with the session line | [control.ts:15](../src/control.ts#L15), [:441-449](../src/control.ts#L441-L449) |
| error assumed in the largest card | 600 × `€` | [control.ts:19](../src/control.ts#L19) |
| `/list` | 100 chats per session, lines of at most 100 bytes | [control.ts:16-17](../src/control.ts#L16-L17) |
| `send.text` | 1 to 12000 characters | [tools.ts:185](../src/tools.ts#L185) |
| `connect.message` | 1 to 2000 characters | [tools.ts:173](../src/tools.ts#L173) |
| `send.mention` | 1 to 3 unique member IDs | [tools.ts:187](../src/tools.ts#L187) |
| `wait` | 0 to 86400 s | [tools.ts:174](../src/tools.ts#L174), [:188](../src/tools.ts#L188), [:200](../src/tools.ts#L200) |
| `idle` | 1 to 300 s, default 30 | [tools.ts:175](../src/tools.ts#L175), [:189](../src/tools.ts#L189), [:116](../src/tools.ts#L116) |
| `limit` | 1 to 100, default 50 | [tools.ts:190](../src/tools.ts#L190), [:201](../src/tools.ts#L201), [:117](../src/tools.ts#L117) |
| `connect` snapshot | last 50 items | [tools.ts:118](../src/tools.ts#L118) |
| collected items per action or call | 100, then their IDs kept as overflow until deleted | [messages.ts:7](../src/messages.ts#L7), [:81-82](../src/messages.ts#L81-L82) |
| unread page | 100 items | [messages.ts:6](../src/messages.ts#L6) |
| chat list read | one command, `count=1000000` | [tools.ts:112](../src/tools.ts#L112) |
| error details | 500 characters | [log.ts:5](../src/log.ts#L5) |
| progress interval | 60 s | [messages.ts:4](../src/messages.ts#L4) |
| longest timer delay | 2147483647 ms, the Node limit | [messages.ts:5](../src/messages.ts#L5) |
| delivery wait at stop | 10 s, polled every 100 ms; the one deliberate poll, because no event marks a delivered card edit | [core.ts:8-9](../src/core.ts#L8-L9), [:64](../src/core.ts#L64) |
| display name | 50 characters, `mkValidName` rules | [messages.ts:8-9](../src/messages.ts#L8-L9), [:238-260](../src/messages.ts#L238-L260) |
| session idle close | 10 min | [daemon.ts:18](../src/daemon.ts#L18) |
| daemon idle stop | 10 min | [daemon.ts:19](../src/daemon.ts#L19) |
| daemon start wait | 5 min | [client.ts:10](../src/client.ts#L10) |
| socket request body | 1 MiB | [socket.ts:6](../src/socket.ts#L6) |
| socket path | 103 bytes | [files.ts:9](../src/files.ts#L9) |
| `daemon.log` | truncated at spawn when over 10 MB | [files.ts:10](../src/files.ts#L10) |
