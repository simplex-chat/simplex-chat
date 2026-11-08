# SimpleX MCP server: architecture

> System structure, modules, state, process lifecycle, approval model and security boundaries of `@simplex-chat/mcp` 0.3.0: `serve` in sections 1-7, the Claude Code plugin in section 8.
>
> Related specs: [README](README.md) | [Flows](flows.md) | [API](api.md)
> Related docs: [README](../README.md) | [Reference](../REFERENCE.md) | [PLAN](../PLAN.md)

**Source:** [`cli.ts`](../src/cli.ts) | [`serve.ts`](../src/serve.ts) | [`http.ts`](../src/http.ts) | [`tools.ts`](../src/tools.ts) | [`actions.ts`](../src/actions.ts) | [`control.ts`](../src/control.ts) | [`messages.ts`](../src/messages.ts) | [`files.ts`](../src/files.ts) | [`register.ts`](../src/register.ts) | [`log.ts`](../src/log.ts) | [`core.ts`](../src/core.ts)
**Plugin source:** [`session.ts`](../src/session.ts) | [`client.ts`](../src/client.ts) | [`socket.ts`](../src/socket.ts) | [`protocol.ts`](../src/protocol.ts) | [`daemon.ts`](../src/daemon.ts) | [`hub.ts`](../src/hub.ts) | [`main-group.ts`](../src/main-group.ts) | [`user.ts`](../src/user.ts) | [`hook.ts`](../src/hook.ts) | [`owner.ts`](../src/owner.ts) | [`config.ts`](../src/config.ts) | [`qr.ts`](../src/qr.ts)

---

## Table of contents

1. [System diagram](#1-system-diagram)
2. [Modules](#2-modules)
3. [State](#3-state)
4. [Process lifecycle](#4-process-lifecycle)
5. [Approval model](#5-approval-model)
6. [Security boundaries](#6-security-boundaries)
7. [Limits](#7-limits)
8. [Claude Code plugin](#8-claude-code-plugin)

---

## 1. System diagram

```mermaid
flowchart LR
  subgraph host["MCP host"]
    cc["Claude Code"]
  end
  subgraph proc["Node.js process: simplex-mcp serve NAME"]
    http["http.ts<br/>127.0.0.1:PORT/mcp<br/>Host and Origin guards<br/>bearer token<br/>path /mcp"]
    sdk["MCP SDK<br/>createMcpHandler<br/>McpServer per request"]
    tools["tools.ts<br/>AgentTools"]
    actions["actions.ts<br/>Actions"]
    control["control.ts<br/>Control"]
    messages["messages.ts<br/>ChatItemsWatch"]
    serve["serve.ts<br/>start, events, stop"]
    files["files.ts"]
    log["log.ts"]
    lib["simplex-chat<br/>ChatApi"]
    core["SimpleX core<br/>libsimplex core-7.1.0.20"]
  end
  cli["cli.ts"] --> serve
  cli --> register["register.ts"]
  register -->|"claude mcp add-json"| cc
  cc -->|"headersHelper:<br/>cat headers.json"| data
  cc -->|"POST /mcp<br/>Authorization: Bearer"| http
  http --> sdk --> tools
  tools --> actions
  actions <--> control
  tools --> messages
  serve --> control
  serve --> tools
  serve --> messages
  serve --> files
  files --> data[("DATA:<br/>token<br/>headers.json<br/>server.json<br/>lock")]
  tools --> folder[("DATA/files:<br/>group-N, contact-N<br/>sent")]
  tools --> lib
  control --> lib
  serve --> lib
  lib --> core
  core --> db[("DATA:<br/>simplex_chat.db<br/>simplex_agent.db<br/>tmp")]
  core <-->|"SMP<br/>private routing"| smp["SMP servers"]
  core <-->|"XFTP"| xftp["XFTP servers"]
  smp <--> owner["Owner's SimpleX app<br/>agent control chat"]
  smp <--> peers["Peers:<br/>contacts and bots<br/>groups and channels"]
  log --> term["Terminal:<br/>stdout<br/>stderr"]
```

Data flow of a tool call:

1. Claude Code runs the `headersHelper` command of its entry ([`register.ts:50`](../src/register.ts#L50)) and sends the `Authorization` header it prints ([Reference](../REFERENCE.md#add-the-agent-to-claude-code)).
2. The server checks Host, Origin, token, path and readiness, then passes the request to the SDK handler ([`http.ts:19-28`](../src/http.ts#L19-L28)).
3. An `McpServer` is built from the factory for each request, and the arguments are validated against the input schema by the SDK ([`serve.ts:107-111`](../src/serve.ts#L107-L111), [`mcp-BumlNux0.cjs:1812-1817`](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L1812-L1817)).
4. The handler result is returned by `AgentTools.call` as `structuredContent` and as JSON text ([`tools.ts:536-543`](../src/tools.ts#L536-L543)).
5. The server calls `ChatApi` methods of the core ([`messages.ts:271-287`](../src/messages.ts#L271-L287)); actions are created in `Actions` and shown to the owner by `Control` ([`actions.ts:176-211`](../src/actions.ts#L176-L211)).
6. The core emits events; the handler of `serve.ts` passes them to the agent control chat and to `AgentTools.onEvent`, which passes them to the tools and the item watch ([`serve.ts:125-137`](../src/serve.ts#L125-L137), [`tools.ts:657-684`](../src/tools.ts#L657-L684)).

---

## 2. Modules

### [2.1 `cli.ts`](../src/cli.ts)

Entry point of the `simplex-mcp` binary.

| Symbol | Line | Contents |
|---|---|---|
| `helpText` | [14](../src/cli.ts#L14) | `--help` output |
| `pluginCommands` | [55](../src/cli.ts#L55) | `session`, `daemon`, `hook session-start`, `hook turn`, `link`, `reset-owners` |
| `Command` | [57](../src/cli.ts#L57) | `help`, `serve` with `ServeOptions`, `register` with `RegisterOptions`, the plugin commands |
| `parseCommand` | [63](../src/cli.ts#L63) | command, NAME and options to a `Command`; the joined arguments equal to a plugin command |
| `serveOptions` | [74](../src/cli.ts#L74) | defaults and option switch of `serve` |
| `registerOptions` | [130](../src/cli.ts#L130) | defaults and option switch of `register` |
| `OptionReader` | [147](../src/cli.ts#L147) | `value`: the next argument, required<br>`optionalValue`: the next argument, absent when missing or starting with `--` |
| `validAgentName` | [169](../src/cli.ts#L169) | `^[a-z0-9](-?[a-z0-9])*-?$`, at most 32 characters |
| `usageError` | [214](../src/cli.ts#L214) | `ExitError` with code 2 |
| `main` | [218](../src/cli.ts#L218) | `help` to stdout, `serve`, `register`; `session`, `daemon`; the hooks read stdin and print their JSON output; `link` and `reset-owners` print their text |
| top-level catch | [253-262](../src/cli.ts#L253-L262) | `ExitError`: message on stderr, its code; other errors: `logError`, code 1 |

### [2.2 `serve.ts`](../src/serve.ts)

Start sequence, event dispatch and stop sequence of one agent.

| Symbol | Line | Contents |
|---|---|---|
| `OwnerEntry` | [18](../src/serve.ts#L18) | `{memberId, name}` of `--owners` |
| `ServeOptions` | [23](../src/serve.ts#L23) | name, data folder and its `dataGiven` flag, display name, mode, owners, expiry, reset and token flags, port, QR code size, stop wait |
| `RunningServer` | [38](../src/serve.ts#L38) | token, `ChatApi`, user, `Actions`, `stop` |
| `serve` | [46](../src/serve.ts#L46) | `startServer`, then SIGINT and SIGTERM handlers: the first signal stops, a second ends the stop wait |
| `startServer` | [64](../src/serve.ts#L64) | data folder, lock, token, headers file, `ChatApi.init`, `startAgent`; cleanup on error |
| `startAgent` | [87](../src/serve.ts#L87) | user, `prepareCore`, control group, wiring of `Actions`, `AgentTools`, `Control`, HTTP, event handler, `stop` |
| `registerHint` | [157](../src/serve.ts#L157) | the `register` command line printed after the owner setup |
| `waitForCancelledCards` | [161](../src/serve.ts#L161) | the fixed wait at stop for cards edited or posted at the stop; also used by the plugin's hub |
| `controlGroup` | [177](../src/serve.ts#L177) | found, created or reset agent control chat |
| `listen` | [184](../src/serve.ts#L184) | `startHttp`; `EADDRINUSE` to `ExitError` code 1 |

Shared with the plugin, from [`core.ts`](../src/core.ts) and [`version.ts`](../src/version.ts):

| Symbol | Line | Contents |
|---|---|---|
| `prepareCore` | [core.ts:38](../src/core.ts#L38) | network config, `DATA/tmp` as the core's temp folder, mode 0700, and `DATA/files/sent` deleted |
| `updatedUser` | [core.ts:55](../src/core.ts#L55) | display name and peer type `agent` of the profile |
| `bufferEvents` | [core.ts:62](../src/core.ts#L62) | events received before the handler is installed are queued and replayed in order |
| `version` | [version.ts:1](../src/version.ts#L1) | the package version |

### [2.3 `http.ts`](../src/http.ts)

| Symbol | Line | Contents |
|---|---|---|
| `HttpOptions` | [7](../src/http.ts#L7) | port, token, `McpServerFactory`, readiness |
| `startHttp` | [14](../src/http.ts#L14) | `http.Server` on `127.0.0.1:port`: guards, SDK handler, timeouts |
| `authorized` | [34](../src/http.ts#L34) | length check and `timingSafeEqual` against `Bearer <token>` |
| `stopHttp` | [48](../src/http.ts#L48) | `server.close` and `closeAllConnections` |

### [2.4 `tools.ts`](../src/tools.ts)

Tool definitions, call handlers, connect plans, perform steps, files.

| Symbol | Line | Contents |
|---|---|---|
| `ConnectInput`, `SendInput`, `ReceiveInput`, `LeaveInput`, `DownloadInput` | [46-80](../src/tools.ts#L46-L80) | tool inputs |
| `PlanSummary` | [91](../src/tools.ts#L91) | card kind, chat type, name, SimpleX name, peer type, description, welcome, commands, `takesMessage` |
| `ChatFields` | [103](../src/tools.ts#L103) | chat fields of `connect` and `list` results |
| `Agent` | [117](../src/tools.ts#L117) | `ChatApi`, user, `Actions`, `ChatItemsWatch`, control chat reference, standing approver lookup, refused file post, data folder; for the plugin also the data root, the owning session ID, the group of `reply` and the mode setter |
| `JoinMethod` | [132](../src/tools.ts#L132) | `prepared`, `prepareChannel`, `viaAddress`, `prepareContact`, `connect` |
| schemas | [188-411](../src/tools.ts#L188-L411) | input and output JSON schemas |
| descriptions, `instructions` | [413-451](../src/tools.ts#L413-L451) | tool descriptions and server instructions; `instructions` names the host whose restart cancels actions, `server` by default |
| `replyTool` | [461](../src/tools.ts#L461) | the plugin's seventh tool, `reply` |
| `definitions` | [477](../src/tools.ts#L477) | name, description, schemas and action flag of the six tools |
| `registerTools` | [490](../src/tools.ts#L490) | `registerTool` for each definition and each extra one; `_meta` on action tools in mode `host` |
| `Download` | [502](../src/tools.ts#L502) | one download: its promise, chat, item, file ID, size, path, the last fileSource seen, received bytes and their time, check timer |
| `AgentTools` | [519](../src/tools.ts#L519) | constructor with the `Agent`, the mode or a function returning it, and the session number of the plugin; `mode` reads it at each use |
| `AgentTools.register` | [532](../src/tools.ts#L532) | `registerTools` with `AgentTools.call` |
| `AgentTools.call` | [536](../src/tools.ts#L536) | handler result to `CallToolResult`; errors to `isError` |
| `AgentTools.perform` | [545](../src/tools.ts#L545) | perform step of an approved action; for a `mode` action, the mode setter |
| `giveChat`, `owns`, `claim` | [560-578](../src/tools.ts#L560-L578) | the plugin's chat owners: a chat is the session's when no session ID is set or the chat's owner is that ID |
| `onEvent` | [657](../src/tools.ts#L657) | events to the file handlers, connect completions and the item watch; shared by `serve` and the plugin's sessions |
| `reply` | [705](../src/tools.ts#L705) | a file to the group of the plugin's session, under the file rules of `send` |
| `downloadFile`, `forwardFile`, `downloadTo` | [579-606](../src/tools.ts#L579-L606) | one download per file ID, shared by `download` and `/forward`; a forwarded file moved to the download path for `download` |
| file events | [608-645](../src/tools.ts#L608-L645) | `onNewItems`, `onFile`, `onFileComplete`, `onFileError`, `onFileCancelled`, `onFileDescription` |
| `onContactConnected`, `onUserJoinedGroup` | [647-655](../src/tools.ts#L647-L655) | completion of connect actions by events |
| `actionResult` | [742](../src/tools.ts#L742) | wait for the decision, result fields, removal |
| `attachment`, `isRefused`, `fileToSend`, `resizedCopy` | [967-1003](../src/tools.ts#L967-L1003) | the file of a `send` |
| `copyForSending`, `chatFiles`, `deleteChatFiles` | [1005-1034](../src/tools.ts#L1005-L1034) | `/forward` copies; the files of a chat, forwarded ones included; its downloads cancelled, also those the core continues after a restart |
| `leaveChat` | [1056](../src/tools.ts#L1056) | perform step of `leave` and `/leave`; the chat's forwarded files deleted |
| `download`, `fileServers`, `unknownServers` | [1066-1111](../src/tools.ts#L1066-L1111) | the `download` call before its action |
| `startDownload`, `checkDownload`, `currentFile`, `settleFile`, `settleReceived`, `moveReceived` | [1129-1196](../src/tools.ts#L1129-L1196) | a download's steps |
| `agentChats`, `knownChat` | [1266-1285](../src/tools.ts#L1266-L1285) | the agent's chats; a checked chat reference |
| `classifyPlan` | [1368](../src/tools.ts#L1368) | `apiConnectPlan` response to a join plan or a known chat |
| `chatSubject`, `isConnected`, `isActive`, `memberCurrent` | [1464-1529](../src/tools.ts#L1464-L1529) | chat and member predicates shared with `control.ts` |
| `customDataOf`, `setChatData`, `ownerOf` | [1497-1512](../src/tools.ts#L1497-L1512) | a chat's custom data; the plugin's owner key `simplexMcpOwner` |
| `fitsMessage`, `quoteContent` | [1553-1569](../src/tools.ts#L1553-L1569) | the message as the core sends it, within 15602 bytes; for `send` and the post of `/forward` |
| `realFile` | [1585](../src/tools.ts#L1585) | real path, size, inode and change time of a file |
| `attachmentMedia`, `videoFrame`, `videoDuration` | [1632-1666](../src/tools.ts#L1632-L1666) | previews and durations |
| `toolCall` | [1682](../src/tools.ts#L1682) | `ServerContext` to `ToolCall`: abort signal and progress notifications |

### [2.5 `actions.ts`](../src/actions.ts)

| Symbol | Line | Contents |
|---|---|---|
| `Mode` | [5](../src/actions.ts#L5) | `requests`, `host` |
| `State` | [6](../src/actions.ts#L6) | `pending`, `performing`, `done`, `failed`, `rejected`, `expired` |
| `Standing` | [7](../src/actions.ts#L7) | `messages`, `files` |
| `ToolError` | [17](../src/actions.ts#L17) | errors the model can correct; text returned as is |
| `Subject` | [24](../src/actions.ts#L24) | kind, name, SimpleX name, incognito flag of a card |
| `ConnRef` | [31](../src/actions.ts#L31) | pending connection, contact or group of a connect action |
| `HostTexts` | [19](../src/actions.ts#L19) | refusal texts of a lost control chat and of a stop: `lostMessage` and `stoppingMessage` for `serve`, the hub's texts for the plugin |
| `ActionRecord` | [37](../src/actions.ts#L37) | number, session number of the plugin, mode, key, times, change signal, standing flag, state, card, decider, open calls, interrupted flag, error, card image |
| `ConnectAction`, `SendAction`, `LeaveAction`, `DownloadAction` | [56-115](../src/actions.ts#L56-L115) | per-tool fields; `Attachment`, the file of a send |
| `ModeAction`, `PermissionAction` | [117-129](../src/actions.ts#L117-L129) | the plugin's actions: a switch to mode `host`; a tool use of Claude Code |
| `ActionHooks` | [135](../src/actions.ts#L135) | `refusal`, `saveCounter`, `postCard`, `perform`, `outcome` |
| `Actions` | [143](../src/actions.ts#L143) | action map, one expiry timer per action, posting and outcome promises |
| `create` | [176](../src/actions.ts#L176) | state by the action's mode, refusal checks, number, log line, counter write, card |
| `ofSession`, `remove`, `openCall`, `closeCall`, `end` | [213-236](../src/actions.ts#L213-L236) | the actions of one session; removal; open call count; an expiry before the expiry time |
| `expireDue` | [238](../src/actions.ts#L238) | expiry of every action past its expiry time |
| `decided` | [244](../src/actions.ts#L244) | wait with progress until a decided state, `until`, or the abort signal |
| `stop` | [279](../src/actions.ts#L279) | cancellation of pending and performing actions |
| `scheduleExpiry`, `expire`, `forgetExpired` | [292-310](../src/actions.ts#L292-L310) | the expiry timer of one action; its expiry; its removal once decided, without open calls and past its expiry time |
| `summary`, `performedText`, `progressText` | [376-419](../src/actions.ts#L376-L419) | log and progress texts |

Hooks are bound in [`serve.ts:96-102`](../src/serve.ts#L96-L102): `refusal`, `saveCounter`, `postCard` and `outcome` to `Control`; `perform` to `AgentTools`.

### [2.6 `control.ts`](../src/control.ts)

| Symbol | Line | Contents |
|---|---|---|
| `controlCommands` | [10](../src/control.ts#L10) | the command menu |
| `standings` | [22](../src/control.ts#L22) | custom data key and reply words of the approvals of messages and files |
| `ControlData` | [36](../src/control.ts#L36) | `{control: true, nextAction}` |
| `ChatApproval` | [59](../src/control.ts#L59) | `{approvedBy, approvedById, approvedAt}` |
| `ControlOptions`, `SessionOptions` | [41-52](../src/control.ts#L41-L52) | display name, `--owners`, register hint, QR code size; for a group of the plugin's session, its approver check and the session IDs by number |
| `OwnerRequest` | [65](../src/control.ts#L65) | parsed owner command |
| `Control.load` | [102](../src/control.ts#L102) | lost check, group profile and menu, member list |
| `Control.rename`, `Control.describe` | [111-119](../src/control.ts#L111-L119) | the plugin's group name; its short description |
| `Control.start` | [121](../src/control.ts#L121) | owner setup per `--owners` |
| `Control.refusal` | [129](../src/control.ts#L129) | bytes left for the link texts of a connect card, card size of a send, connect or download, approver presence |
| `Control.standingApprover` | [141](../src/control.ts#L141) | approver name of a chat's standing approval of messages or files |
| `Control.saveCounter`, `Control.saveData` | [146-158](../src/control.ts#L146-L158) | queued writes of the counter and of other keys of the group's custom data |
| `Control.postCard`, `Control.outcome` | [160-171](../src/control.ts#L160-L171) | card post and card edit; the kept-files message after a done leave |
| `Control.refusedFile`, `Control.fileNotSent` | [173-188](../src/control.ts#L173-L188) | posts about files |
| `Control.onMessage` | [190](../src/control.ts#L190) | owner commands, queued one at a time |
| `Control.onEvent` | [202](../src/control.ts#L202) | membership changes and loss of the control chat |
| `Control.ownerCommand` | [220](../src/control.ts#L220) | command dispatch and replies |
| `approveChat`, `revoke`, `forward`, `leave`, `deleteFiles` | [265-329](../src/control.ts#L265-L329) | owner requests |
| `give` | [331](../src/control.ts#L331) | `/give CHAT N` in a group of the plugin's session |
| `isApprover` | [524](../src/control.ts#L524) | the session's check in a group of the plugin's session; otherwise role `owner`, and the `--owners` list when given |
| `cardContent`, `fittedImage` | [477-497](../src/control.ts#L477-L497) | image and video cards with the file's preview |
| `findControlGroup`, `createControlChat`, `resetControlChat` | [536-566](../src/control.ts#L536-L566) | control group lookup, creation, reset |
| `createGroup`, `deleteGroup` | [557-571](../src/control.ts#L557-L571) | a group with its profile and custom data, shared with the plugin's groups; deletion for all members |
| `isGroupMessage` | [573](../src/control.ts#L573) | an item of one group's main scope |
| `isControlEvent` | [577](../src/control.ts#L577) | membership events of the control group |
| `parseCommand` | [593](../src/control.ts#L593) | one-line `/keyword params` |
| `cardText`, `requestLines`, `commandsLine` | [600-671](../src/control.ts#L600-L671) | card text per tool, state and mode |
| `largestCard`, `encodedLength` | [677-685](../src/control.ts#L677-L685) | card size check |
| `serversLine`, `sizeText` | [719-728](../src/control.ts#L719-L728) | servers line and sizes of download cards |
| `codeSpan`, `safeName`, `mentionText` | [738-750](../src/control.ts#L738-L750) | quoting of peer text |
| `ownerRequest` | [761](../src/control.ts#L761) | command words to an `OwnerRequest` |
| `losesControl`, `isLost` | [777-792](../src/control.ts#L777-L792) | loss of the control chat, also of the plugin's groups |
| `joinedText` | [806](../src/control.ts#L806) | `NAME joined (member N)`, shared with the Main group |
| `entryText`, `restartText` | [810-820](../src/control.ts#L810-L820) | `--owners` entries in restart lines |

### [2.7 `messages.ts`](../src/messages.ts)

| Symbol | Line | Contents |
|---|---|---|
| `Message`, `MessageFile`, `FileOffer` | [16-50](../src/messages.ts#L16-L50) | message item of tool results; its file; the file of a download |
| `ToolCall` | [57](../src/messages.ts#L57) | abort signal and progress callback of one call |
| `Collected` | [62](../src/messages.ts#L62) | collected items, change signal, IDs of the items over the first 100, time of the last change |
| `Changes` | [69](../src/messages.ts#L69) | set of wake callbacks |
| `ChatItemsWatch` | [86](../src/messages.ts#L86) | listeners per chat reference; received message items gathered by `collect` |
| `chatRef`, `anyScopeChatRef`, `isChatRef` | [135-152](../src/messages.ts#L135-L152) | `@N` and `#N` references: of the main conversation, or also of a group's member chat |
| `toMessage`, `messageFile`, `receivedFileError`, `receivedPath` | [174-213](../src/messages.ts#L174-L213) | chat item to `Message`; where a received file is: its download path or its forwarded path |
| `filePaths`, `storedName`, `chatFolder` | [215-227](../src/messages.ts#L215-L227) | `DATA/files/group-N`, `DATA/files/contact-N` or the same under `DATA/forwarded`; the download path and the forwarded path of a received file; its name, the file ID and the extension, also for the files of the plugin's prompts |
| `fileOffer`, `fileContent` | [229-254](../src/messages.ts#L229-L254) | file fields of an item; message content of a file |
| `getChat`, `receivingFiles`, `markRead` | [271-287](../src/messages.ts#L271-L287) | `apiGetChat` and `apiChatItemsRead` by chat reference; the IDs of a chat's files in status `rcvAccepted`, `rcvTransfer` or `rcvWarning`, paged |
| `unreadItems` | [289](../src/messages.ts#L289) | unread items of a chat after its `initial` read, paged |
| `reportProgress`, `nextChange`, `awaitResponses` | [305-335](../src/messages.ts#L305-L335) | waits |
| `validName`, `viewName` | [341-367](../src/messages.ts#L341-L367) | display name rules and `@name` form |

### [2.8 `files.ts`](../src/files.ts)

| Symbol | Line | Contents |
|---|---|---|
| `createDataFolder` | [85](../src/files.ts#L85) | `mkdir -p` mode 0700; `chmod 0700` except on Windows |
| `acquireLock`, `releaseLock` | [90-105](../src/files.ts#L90-L105) | `DATA/lock` with the PID; `user`, `server` or `daemon`, names the holder in the error |
| `serverToken` | [107](../src/files.ts#L107) | `DATA/token`: read, or 32 random bytes in base64url, linked into place so that concurrent first starts agree |
| `writeHeadersFile` | [132](../src/files.ts#L132) | `DATA/headers.json` |
| `writeServerFile`, `readServerFile` | [136-143](../src/files.ts#L136-L143) | `DATA/server.json` |

### [2.9 `register.ts`](../src/register.ts)

| Symbol | Line | Contents |
|---|---|---|
| `register` | [33](../src/register.ts#L33) | `claude mcp remove`, then `claude mcp add-json` |
| `registryEntry` | [46](../src/register.ts#L46) | `{type, url, headersHelper, timeout}` |
| `manualEntryText` | [55](../src/register.ts#L55) | entry text when `claude` is missing |
| `projectRoot` | [60](../src/register.ts#L60) | project key of `--scope local` |
| `runCommand` | [86](../src/register.ts#L86) | `spawnSync` with ENOENT detection; on Windows after ENOENT, once more through `cmd.exe` with `cmdArgument` quoting, and exit status 9009 as missing |

### [2.10 `log.ts`](../src/log.ts)

| Symbol | Line | Contents |
|---|---|---|
| `ExitError` | [18](../src/log.ts#L18) | message and exit code |
| `shellWord`, `shellPath` | [24-31](../src/log.ts#L24-L31) | POSIX single quotes or Windows double quotes |
| `oneLine` | [33](../src/log.ts#L33) | runs of `\p{Cc}`, `\p{Cf}`, `\p{Zl}`, `\p{Zp}` to one space |
| `log`, `logError` | [37-43](../src/log.ts#L37-L43) | stdout lines; stderr lines |
| `errorText`, `chatErrorText` | [45-53](../src/log.ts#L45-L53) | error text, folded to one line: the apps' wording for known error shapes, otherwise the type chain |
| `appErrorText` | [55-64](../src/log.ts#L55-L64) | wording of the apps' alerts for chat, agent and store errors |
| `fileError` | [197-201](../src/log.ts#L197-L201) | the error of a received file: the apps' words for its status, or the size limit of its sender |
| `serverHostname` | [246](../src/log.ts#L246) | host of a server address |
| `chainText`, `errorChain` | [250-262](../src/log.ts#L250-L262) | `SimpleX error: <type chain>: <scalar fields>`, details at most 500 characters |
| `fullDomainName` | [272](../src/log.ts#L272) | `SimplexDomain` to `name.simplex` or `name.testing` |

---

## 3. State

### 3.1 Memory

| Item | Holder | Written by | Read by |
|---|---|---|---|
| Actions | `Actions.actions` map ([actions.ts:147](../src/actions.ts#L147)) | `create`, `remove`, `forgetExpired` | tools, `Control` |
| Expiry timers | `Actions.expiries`, one per action ([actions.ts:148](../src/actions.ts#L148)) | `scheduleExpiry`, `expire`, `remove` | |
| Open calls of an action | `ActionRecord.open` ([:49](../src/actions.ts#L49)) | `openCall` and `closeCall` from `actionResult` ([tools.ts:743](../src/tools.ts#L743), [:752](../src/tools.ts#L752)) | `list`, `forgetExpired` |
| Collected replies | `ConnectAction.messages`, `SendAction.responses` ([:61](../src/actions.ts#L61), [:92](../src/actions.ts#L92)) | `ChatItemsWatch.collect` | `connect`, `send` results |
| Item listeners | `ChatItemsWatch.listeners` ([messages.ts:87](../src/messages.ts#L87)) | `collect` | event dispatch |
| Downloads | `AgentTools.downloads`, one per file ID ([tools.ts:520](../src/tools.ts#L520)) | `downloadTo` | file events, checks, `/delete CHAT files` |
| Forwards | `AgentTools.forwarding`, the copy for the post per file ID ([tools.ts:521](../src/tools.ts#L521)) | `forwardFile` | `downloadFile`, before it moves a forwarded file |
| Rejected files | `AgentTools.rejectedFiles` ([tools.ts:522](../src/tools.ts#L522)) | `downloadResult` ([:1118-1119](../src/tools.ts#L1118-L1119)) | the file field; `download`, which then skips the files approval ([:1081](../src/tools.ts#L1081)) |
| Count of file descriptions | `AgentTools.describedFiles` ([tools.ts:523-524](../src/tools.ts#L523-L524)) | `onFileDescription` | `fileServers` |
| Members of the control chat | `Control.members` ([control.ts:82](../src/control.ts#L82)) | `refreshMembers` ([:499-501](../src/control.ts#L499-L501)) | approver checks |
| Mode, expiry, owners list, stop wait | `ServeOptions` of the run; the mode also in `Actions.mode` and in each action ([actions.ts:40](../src/actions.ts#L40)) | CLI | all modules |

### 3.2 Chat database custom data

| Item | Entity | Value | Written by | Read by |
|---|---|---|---|---|
| Control marker and counter | agent control chat | `{simplexMcp: {control: true, nextAction: N}}` | [`createControlChat`](../src/control.ts#L546-L561), [`saveCounter`](../src/control.ts#L146) | [`findControlGroup`](../src/control.ts#L536), [`controlDataOf`](../src/control.ts#L541) |
| Standing approval of messages | contact or group of the agent | `{...other keys, simplexMcp: {approvedBy, approvedById, approvedAt}}` | [`approveChat`](../src/control.ts#L265), [`revoke`](../src/control.ts#L278) | [`standingApprover`](../src/control.ts#L141) |
| Files approval | contact or group of the agent | `{...other keys, simplexMcpFiles: {approvedBy, approvedById, approvedAt}}` | [`approveChat`](../src/control.ts#L265), [`revoke`](../src/control.ts#L278) | [`standingApprover`](../src/control.ts#L141) |

The profile, chats, items, members and file transfers are stored by the core in `DATA/simplex_chat.db`; connections and queues in `DATA/simplex_agent.db` (file prefix `DATA/simplex`, [serve.ts:71](../src/serve.ts#L71)).

### 3.3 Files

| Path | Contents | Mode | Written | Read |
|---|---|---|---|---|
| `DATA/` | data folder | 0700, set at every start except on Windows | [files.ts:85-88](../src/files.ts#L85-L88) | |
| `DATA/token` | base64url of 32 random bytes | 0600, set again at every write except on Windows | first start, `--new-token` ([:107-126](../src/files.ts#L107-L126)) | every start |
| `DATA/headers.json` | `{"Authorization":"Bearer <token>"}` | 0600, set again at every write except on Windows | every start ([:132-134](../src/files.ts#L132-L134)) | Claude Code `headersHelper` |
| `DATA/server.json` | `{name, port, expireHours}` | 0600, set again at every write except on Windows | every start, after the HTTP listener is bound ([serve.ts:114](../src/serve.ts#L114)) | `register` |
| `DATA/lock` | PID and a newline | 0600 | start ([files.ts:158-166](../src/files.ts#L158-L166)) | next start |
| `DATA/tmp/` | the core's temp folder: files in transfer | 0700, set at every start | [core.ts:41](../src/core.ts#L41), [:78-82](../src/core.ts#L78-L82) | the core |
| `DATA/files/group-N/`, `DATA/files/contact-N/` | downloaded files and files received without a download, named by file ID and extension | folder 0700 | the core at the download path ([tools.ts:1134-1135](../src/tools.ts#L1134-L1135)); moves ([:1620-1630](../src/tools.ts#L1620-L1630)) | `download` results, `/forward`, `/leave`, `/delete CHAT files` |
| `DATA/forwarded/group-N/`, `DATA/forwarded/contact-N/` | files `/forward` downloaded, named as above, until a download moves them to `DATA/files/` | folder 0700 | the core at the forwarded path ([tools.ts:589-596](../src/tools.ts#L589-L596), [:1134-1135](../src/tools.ts#L1134-L1135)) | `/forward`; moved by `download` ([:579-587](../src/tools.ts#L579-L587)); deleted by `leave`, `/leave` and `/delete CHAT files` ([:1016-1034](../src/tools.ts#L1016-L1034), [:1056-1064](../src/tools.ts#L1056-L1064)) |
| `DATA/files/sent/` | resized copies of sent images, and copies of the files `/forward` posts, one folder per copy | folder 0700; a resized copy 0600 | [tools.ts:992-1009](../src/tools.ts#L992-L1009), [:1036-1040](../src/tools.ts#L1036-L1040) | the core; deleted at `sndFileCompleteXFTP`, `sndFileError` and at start ([core.ts:42](../src/core.ts#L42), [tools.ts:680-682](../src/tools.ts#L680-L682), [:955](../src/tools.ts#L955)) |

The core library is downloaded once into `CACHE/core-7.1.0.20/sqlite`, and the Node.js add-on into `CACHE/nodejs-addon-cce06d8/`, each with a `Downloading ...` line on stderr. CACHE is `$XDG_CACHE_HOME/simplex-chat` or `~/.cache/simplex-chat`; on macOS `~/Library/Caches/simplex-chat`; on Windows `%LOCALAPPDATA%\simplex-chat` ([libs.ts:41-48](../../simplex-chat-nodejs/src/libs.ts#L41-L48), [:89](../../simplex-chat-nodejs/src/libs.ts#L89), [:109](../../simplex-chat-nodejs/src/libs.ts#L109), [:116](../../simplex-chat-nodejs/src/libs.ts#L116), [:129](../../simplex-chat-nodejs/src/libs.ts#L129)).

---

## 4. Process lifecycle

### 4.1 Start

1. Data folder, lock, token and headers file are prepared ([serve.ts:66-70](../src/serve.ts#L66-L70)).
2. libsimplex for SQLite is loaded by `ChatApi.init`, and the databases are migrated with confirmation `yesUp` ([serve.ts:71](../src/serve.ts#L71), [api.ts:117-126](../../simplex-chat-nodejs/src/api.ts#L117-L126)).
3. Three log lines: version and NAME, data folder, mode ([serve.ts:72-74](../src/serve.ts#L72-L74)).
4. The active user is read, or created with display name `--display-name` or NAME, empty full name and peer type `agent` ([serve.ts:89](../src/serve.ts#L89)).
5. `prepareCore` reads the network config and sets it with `smpProxyMode` `always` and `smpProxyFallback` `allowProtected` ([serve.ts:90](../src/serve.ts#L90), [core.ts:39-40](../src/core.ts#L39-L40)).
6. `prepareCore` sets `DATA/tmp` as the core's temp folder and deletes `DATA/files/sent` ([core.ts:41-42](../src/core.ts#L41-L42)).
7. Events are buffered, and the chat is started ([serve.ts:91-92](../src/serve.ts#L91-L92)).
8. A `--display-name` different from the profile, or a peer type other than `agent`, is applied with one `apiUpdateProfile` ([serve.ts:93](../src/serve.ts#L93), [core.ts:55-60](../src/core.ts#L55-L60)).
9. The agent control chat is found by its marker, created with counter 1, or reset with `--reset-owners` ([:95](../src/serve.ts#L95), [:177-182](../src/serve.ts#L177-L182)).
10. `Actions`, `ChatItemsWatch`, `AgentTools` and `Control` are created; the counter is taken from the marker ([:96-105](../src/serve.ts#L96-L105)).
11. `Control.load`: for a lost control chat, `Actions.lost` is set; otherwise the group name and command menu are updated when they differ, and members are read ([control.ts:102-109](../src/control.ts#L102-L109)).
12. The HTTP server listens; with the port in use, the process exits with code 1 ([serve.ts:113](../src/serve.ts#L113), [:184-191](../src/serve.ts#L184-L191)). Until step 14 ends, requests to `/mcp` that pass the token check get 503 with `Retry-After: 1` ([http.ts:23](../src/http.ts#L23), [serve.ts:112-113](../src/serve.ts#L112-L113), [:121](../src/serve.ts#L121)).
13. `server.json` is written ([serve.ts:114](../src/serve.ts#L114)).
14. The server runs the owner setup, `Control.start`; after an error, HTTP is closed and the error is rethrown ([serve.ts:115-120](../src/serve.ts#L115-L120)).
15. `Listening on http://127.0.0.1:PORT/mcp` ([:122](../src/serve.ts#L122)).
16. Buffered events are replayed, then live events are dispatched ([serve.ts:125](../src/serve.ts#L125), [core.ts:62-76](../src/core.ts#L62-L76)).

After an error inside `startAgent` (steps 4-16) the chat is closed; after any error once the lock is taken, the lock is released ([serve.ts:68-84](../src/serve.ts#L68-L84)).

### 4.2 Event dispatch

Each `onAny` receiver is awaited by the library before the next event is read ([api.ts:195-202](../../simplex-chat-nodejs/src/api.ts#L195-L202)). The dispatcher in [`serve.ts:125-137`](../src/serve.ts#L125-L137) passes items of the control group to `Control`, then every event to [`AgentTools.onEvent`](../src/tools.ts#L657-L684), which runs the `AgentTools` and `ChatItemsWatch` handlers below, then file errors and membership events to `Control`:

| Event | Condition | Handler |
|---|---|---|
| `newChatItems` | item of the control group, main scope | [`Control.onMessage`](../src/control.ts#L190) |
| `newChatItems` | every item | [`AgentTools.onNewItems`](../src/tools.ts#L608), then [`ChatItemsWatch.deliver`](../src/messages.ts#L116) |
| `chatItemUpdated` | | [`AgentTools.onFile`](../src/tools.ts#L618), then [`ChatItemsWatch.update`](../src/messages.ts#L120) |
| `chatItemsDeleted` | | [`ChatItemsWatch.remove`](../src/messages.ts#L124) of each `deletedChatItem` |
| `contactConnected` | | [`AgentTools.onContactConnected`](../src/tools.ts#L647) |
| `userJoinedGroup` | | [`AgentTools.onUserJoinedGroup`](../src/tools.ts#L652) |
| `rcvFileComplete` | | [`AgentTools.onFileComplete`](../src/tools.ts#L623) |
| `rcvFileSndCancelled` | | [`AgentTools.onFileCancelled`](../src/tools.ts#L635) |
| `rcvFileError` | | [`AgentTools.onFileError`](../src/tools.ts#L630) |
| `rcvFileDescrReady` | | [`AgentTools.onFileDescription`](../src/tools.ts#L642) |
| `sndFileCompleteXFTP` | | [`AgentTools.onSentFile`](../src/tools.ts#L955) |
| `sndFileError` | | [`AgentTools.onSentFile`](../src/tools.ts#L955), then [`Control.fileNotSent`](../src/control.ts#L179) |
| `joinedGroupMember`<br>`joinedGroupMemberConnecting`<br>`connectedToGroupMember`<br>`memberRole`<br>`leftMember`<br>`deletedMember`<br>`deletedMemberUser`<br>`groupDeleted` | event of the control group | [`Control.onEvent`](../src/control.ts#L202), queued one at a time |
| any event | after `stop()` started | ignored |

Owner commands are queued in a second chain, independent of the membership chain ([control.ts:196-199](../src/control.ts#L196-L199)).

### 4.3 Stop

On `SIGINT` or `SIGTERM` the server runs one stop; a later signal ends its wait for cancelled cards ([serve.ts:46-62](../src/serve.ts#L46-L62)). `RunningServer.stop` ([serve.ts:143-153](../src/serve.ts#L143-L153)):

1. Events are ignored from now on.
2. `Actions.stop` starts: new actions are refused with `The server is stopping.`; the expiry timers are cleared; cards being posted are awaited; pending and performing actions are marked interrupted; the stop signal of the performs is aborted, which ends the wait of a `connect` for its connection; their cards are edited, or posted when they have no card; outcome edits already in progress are awaited ([actions.ts:279-310](../src/actions.ts#L279-L310)).
3. In parallel with step 2, the HTTP server is closed with all connections, and open calls are aborted ([http.ts:48-52](../src/http.ts#L48-L52)).
4. `Cancelled K action(s).` when K > 0 ([serve.ts:148](../src/serve.ts#L148)).
5. When cards were edited or posted at the stop and the control chat is not lost, the server waits `--stop-wait` seconds, 10 by default, for them to be sent; a second signal ends the wait ([serve.ts:149](../src/serve.ts#L149), [:161-175](../src/serve.ts#L161-L175)).
6. `chat.close()`: the chat is stopped, then the store is closed ([api.ts:165-170](../../simplex-chat-nodejs/src/api.ts#L165-L170)).
7. The lock is removed, `Stopped.` is logged, and the process exits with code 0, or 1 after an error ([serve.ts:151-152](../src/serve.ts#L151-L152), [:52-58](../src/serve.ts#L52-L58)).

Performs in progress are not awaited; when one ends, its card stays as edited at the stop and no line is logged, because the action is interrupted ([actions.ts:340](../src/actions.ts#L340)).

### 4.4 The lock

```
acquireLock(DATA, USER = server):                    files.ts:90
  create DATA/lock exclusively (flag wx), PID       :158-166
  exists:
    PID = number in the file                        :168-171
    PID valid, other than ours, running:            :94, :173-180
      exit 1 "Another USER uses DATA (PID N). If no USER runs, remove DATA/lock."
    remove the file; create it exclusively again    :95-96
      exists again: exit 1 with the same message
releaseLock(DATA): remove DATA/lock                  :99-105
```

`isRunning`: `true` when `kill(pid, 0)` succeeds or fails with `EPERM` ([files.ts:173-180](../src/files.ts#L173-L180)).

---

## 5. Approval model

### 5.1 Modes

The mode is set by `--approve` for one run ([cli.ts:96-98](../src/cli.ts#L96-L98)).

| Mode | State of a new action | Card | `_meta` of `connect`, `send`, `download`, `leave` |
|---|---|---|---|
| `requests` | `pending`<br>`performing` for a send under a standing approval, and for a download under a files approval | card with commands<br>outcome as a new message after a standing send; none after a download without a card | none |
| `host` | `performing` | outcome as a new message after the perform; none for a download | `{"anthropic/requiresUserInteraction": true}` |

An action keeps the mode it was created in, `ActionRecord.mode`; its card and its log line follow that mode ([actions.ts:176](../src/actions.ts#L176), [:189](../src/actions.ts#L189), [control.ts:130](../src/control.ts#L130)).

Sources: [actions.ts:189](../src/actions.ts#L189), [tools.ts:496](../src/tools.ts#L496), [control.ts:164-171](../src/control.ts#L164-L171), [:532-534](../src/control.ts#L532-L534). In both modes a new action is refused unless the control chat has an approver ([control.ts:134-138](../src/control.ts#L134-L138)).

### 5.2 Owners and approvers

| Term | Rule | Code |
|---|---|---|
| human | member other than the agent's membership, status `connected` or `complete` | [control.ts:520-522](../src/control.ts#L520-L522), [tools.ts:1514-1516](../src/tools.ts#L1514-L1516) |
| human owner | human with role `owner` | [control.ts:507-509](../src/control.ts#L507-L509) |
| approver, `--owners` absent | member with role `owner` | [control.ts:524-529](../src/control.ts#L524-L529) |
| approver, `--owners` without a list | none | [control.ts:528](../src/control.ts#L528) |
| approver, `--owners ID:NAME,...` | member with role `owner` whose `groupMemberId` and `displayName`, folded to one line by `oneLine`, match an entry | [control.ts:528](../src/control.ts#L528), [:794-796](../src/control.ts#L794-L796) |

The command sender's member record from the chat item is checked ([control.ts:195](../src/control.ts#L195), [:224](../src/control.ts#L224)). The approver count for refusals and standing approvals is taken from `Control.members`, refreshed at load and at each membership event ([control.ts:206](../src/control.ts#L206), [:499-501](../src/control.ts#L499-L501)).

Owner setup at start ([control.ts:121-127](../src/control.ts#L121-L127)):

| `--owners` | Start | First join by the link |
|---|---|---|
| absent | a human owner exists: register hint<br>otherwise: `No owners in the agent control chat.` when humans exist, then a link with role `member` | link deleted, role `owner`, greeting, log line, register hint |
| without a list | no humans: link with role `observer`<br>humans exist: their IDs and the restart command, in the log and the chat | link deleted, the joined member and the restart command announced |
| `ID:NAME,...` | each entry matched to a human, or exit 2<br>role `owner` set where missing<br>other humans removed<br>link deleted<br>`Owners: ...` and register hint | |

Sources: [`startOwners`](../src/control.ts#L377), [`startObserver`](../src/control.ts#L383), [`startListed`](../src/control.ts#L388), [`ownerJoined`](../src/control.ts#L407), [`observerJoined`](../src/control.ts#L416).

Membership changes at run time ([control.ts:202-218](../src/control.ts#L202-L218)):

| Change | `--owners` | Server reaction |
|---|---|---|
| `deletedMemberUser`, `groupDeleted`, or the agent's role below `owner` | any | `Actions.loseControl`: pending actions set to `failed`; later cards left unedited; log line |
| approvers drop to zero after a human owner existed | absent | `No owners in the agent control chat.`, new link with role `member` |
| a human becomes owner outside the list | list | `<name> (member N) is now an owner and can't approve actions yet. Restart the server with --owners ...` |
| a listed owner stops being a human owner | list | `<name> (member N) is no longer an owner.` with the restart command, or `No owners remain: ...` |

### 5.3 Action state machine

Transitions, one per row:

| Event | From | To | Code |
|---|---|---|---|
| call creates the action, mode `requests` | | `pending` | [actions.ts:189](../src/actions.ts#L189) |
| call creates the action, mode `host` | | `performing` | [actions.ts:189](../src/actions.ts#L189) |
| `send` call under a standing approval, or `download` call under a files approval | | `performing` | [actions.ts:189](../src/actions.ts#L189) |
| counter write or card post fails | `pending`<br>`performing` | `failed` | [actions.ts:312-323](../src/actions.ts#L312-L323) |
| stop arrives while the counter of a `performing` action is written | `performing` | `pending`, interrupted | [actions.ts:325-330](../src/actions.ts#L325-L330) |
| control chat lost | `pending` | `failed` | [actions.ts:271-277](../src/actions.ts#L271-L277) |
| `/reject`, or `/leave CHAT` for a pending action of CHAT with a card | `pending` | `rejected` | [actions.ts:264-269](../src/actions.ts#L264-L269), [control.ts:313-315](../src/control.ts#L313-L315) |
| expiry time passes | `pending` | `expired` | [actions.ts:297-306](../src/actions.ts#L297-L306) |
| `/approve N`, `/approve CHAT N` and `/approve CHAT all` for other actions; `/receive N`, `/receive CHAT N` and `/receive CHAT N all` for downloads | `pending` | `performing` | [actions.ts:258-262](../src/actions.ts#L258-L262), [control.ts:251-260](../src/control.ts#L251-L260) |
| perform throws | `performing` | `failed` | [actions.ts:336-339](../src/actions.ts#L336-L339) |
| perform ends | `performing` | `done` | [actions.ts:334-335](../src/actions.ts#L334-L335) |

`create` ([actions.ts:176-211](../src/actions.ts#L176-L211)):

```
create(key, fields, standingApprover?, session?, mode = Actions.mode):
  state = mode host or standing ? performing : pending
  refusal, first match:
    stopping                      texts.stopping: "The server is stopping."
    control chat lost             texts.lost: lostMessage
    20 actions of the session pending       pendingMessage
    20 actions of the session performing    performingMessage
    connect: link texts cut to the bytes its largest card leaves
    send, connect or download card over 15000 bytes    cardTooLong
    no approver                   no-owner message
  refusal: throw ToolError        counter unchanged
  N = next; next += 1; store; expiry timer; log "Action N: ..."
  post:
    saveCounter(next)
    state pending: card = postCard()
    either fails: state failed; log "Action N: failed: ERROR."
  after post:
    posted and performing:
      stopping: state = pending
      otherwise: perform
```

```mermaid
stateDiagram-v2
  [*] --> pending: mode requests
  [*] --> performing: mode host, or standing
  pending --> performing: /approve, /receive
  pending --> rejected: /reject, /leave CHAT
  pending --> expired: expiry
  pending --> failed: counter or card fails, control chat lost
  performing --> done: perform ends
  performing --> failed: perform throws, counter fails
  performing --> pending: stop during the counter write
```

An action is removed from memory when its result is returned to a call ([tools.ts:749](../src/tools.ts#L749)), or once it is decided, its open call count is 0 and its expiry time has passed: checked at each decision, at the close of each call and at its expiry timer ([actions.ts:308-310](../src/actions.ts#L308-L310), [:228-231](../src/actions.ts#L228-L231), [:345-349](../src/actions.ts#L345-L349)). `serve` has one session, `undefined`, so the limits of 20 count all its actions. A `download` still performing at the end of `wait` is returned as `performing` and kept ([tools.ts:746](../src/tools.ts#L746)).

### 5.4 Cards

The server posts a card when a `pending` action is created, and edits it with `apiUpdateChatItem` at the outcome ([actions.ts:315](../src/actions.ts#L315), [control.ts:164-171](../src/control.ts#L164-L171)). For a `connect`, `send` or `leave` without a card (mode `host`, standing sends) the server posts the outcome as a new message; a `download` without a card gets none ([control.ts:532-534](../src/control.ts#L532-L534)). The text per state is built by [`cardText`](../src/control.ts#L600-L619); a card of an image or video file is an image or video message with the file's preview ([control.ts:477-497](../src/control.ts#L477-L497)); forms are listed in [API: cards](api.md#22-cards).

Outcomes are posted after `done` and `failed` ([actions.ts:342](../src/actions.ts#L342)), after `rejected` and `expired` ([:268](../src/actions.ts#L268), [:305](../src/actions.ts#L305)), and at stop ([:287](../src/actions.ts#L287)). They are skipped after the control chat is lost ([:287](../src/actions.ts#L287), [:342](../src/actions.ts#L342), [:352](../src/actions.ts#L352)).

### 5.5 Standing approvals

| Aspect | Messages | Files | Code |
|---|---|---|---|
| grant | `/approve CHAT all` from an approver in mode `requests`; pending sends of CHAT with a card are approved | `/receive CHAT N all` from an approver, on the card of download N; N is performed, and the other pending downloads of CHAT stay pending | [control.ts:259](../src/control.ts#L259), [:265-276](../src/control.ts#L265-L276) |
| mode `host` | nothing stored; reply `Approvals apply in mode requests; this server runs in mode host.` | downloads get no card, so the command has none to act on | [control.ts:233](../src/control.ts#L233) |
| removal | `/revoke CHAT`: the `simplexMcp` key is removed, other keys kept | `/receive CHAT off`: the `simplexMcpFiles` key is removed | [control.ts:278-285](../src/control.ts#L278-L285) |
| use | mode `requests`, and `approvedById` equal to the `groupMemberId` of a current approver | the same | [control.ts:141-144](../src/control.ts#L141-L144) |
| scope | `send` | `download` of a file whose servers are all known | [tools.ts:904](../src/tools.ts#L904), [:1081](../src/tools.ts#L1081) |
| decider shown | `safeName(approvedBy)` | the same | [control.ts:143](../src/control.ts#L143) |
| storage | the chat's custom data; removed with the chat by `leave` | the same | [tools.ts:1056-1064](../src/tools.ts#L1056-L1064) |

### 5.6 Expiry

| Rule | Code |
|---|---|
| `expires = created + expireHours × 3600000`, `expireHours` 1 to 23, default 23 | [actions.ts:186](../src/actions.ts#L186), [cli.ts:79](../src/cli.ts#L79), [:102-104](../src/cli.ts#L102-L104) |
| one unreferenced timer per action, set at its creation for `expires - now`, at most `maxTimer` ms; all actions past `expires` are also expired before each owner command | [actions.ts:292-295](../src/actions.ts#L292-L295), [messages.ts:8](../src/messages.ts#L8), [control.ts:221](../src/control.ts#L221) |
| pending and due: `expired`, log `Action N expired.`, card edited to `Expired.` | [actions.ts:297-306](../src/actions.ts#L297-L306) |
| decided, no open call, due: removed from memory | [actions.ts:308-310](../src/actions.ts#L308-L310) |
| connect wait for the other side: at most until `created + 24 h - 10 min` | [tools.ts:152](../src/tools.ts#L152), [:795](../src/tools.ts#L795) |

A timer follows the monotonic clock and `expires` the wall clock: a timer that fires before `expires` is set again ([actions.ts:301](../src/actions.ts#L301)).

### 5.7 Action counter

| Rule | Code |
|---|---|
| initial value: `nextAction` of the control marker, or 1 | [serve.ts:96](../src/serve.ts#L96) |
| a new action takes `next`, then `next` grows by 1 | [actions.ts:177](../src/actions.ts#L177), [:202](../src/actions.ts#L202) |
| the new `next` is written before the card is posted | [actions.ts:314-315](../src/actions.ts#L314-L315) |
| one write at a time, also after a failed write | [control.ts:146-158](../src/control.ts#L146-L158) |
| `--reset-owners` copies the value into the new control chat | [serve.ts:180-181](../src/serve.ts#L180-L181), [control.ts:563-566](../src/control.ts#L563-L566) |
| a refused call leaves the counter unchanged | [actions.ts:201](../src/actions.ts#L201) |

---

## 6. Security boundaries

### 6.1 HTTP

Checks in order of the request listener ([http.ts:19-28](../src/http.ts#L19-L28)):

| Check | Rule | Failure | Code |
|---|---|---|---|
| bind address | `127.0.0.1` only | | [http.ts:41](../src/http.ts#L41) |
| Host | hostname `localhost`, `127.0.0.1` or `[::1]`, any port | 403, JSON-RPC error | [node index.cjs:26-48](../node_modules/@modelcontextprotocol/node/dist/index.cjs#L26-L48), [server index.cjs:1520-1558](../node_modules/@modelcontextprotocol/server/dist/index.cjs#L1520-L1558) |
| Origin | absent or empty passes; otherwise the same hostnames, any port | 403, JSON-RPC error | [node index.cjs:74-96](../node_modules/@modelcontextprotocol/node/dist/index.cjs#L74-L96), [server index.cjs:1599-1644](../node_modules/@modelcontextprotocol/server/dist/index.cjs#L1599-L1644) |
| token | `Authorization` equal to `Bearer <token>`, same length, `timingSafeEqual` | 401, empty body | [http.ts:34-37](../src/http.ts#L34-L37) |
| path | pathname `/mcp` | 404, empty body | [http.ts:22](../src/http.ts#L22) |
| start | the agent control chat is started | 503, `Retry-After: 1`, empty body | [http.ts:23](../src/http.ts#L23) |
| body size | 4 MiB | 413 | [node index.cjs:395-401](../node_modules/@modelcontextprotocol/node/dist/index.cjs#L395-L401) |
| content type of POST | `application/json` | 415 | [server index.cjs:1464-1467](../node_modules/@modelcontextprotocol/server/dist/index.cjs#L1464-L1467) |
| tool arguments | input schema | `isError: true` result | [mcp-BumlNux0.cjs:1812-1817](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L1812-L1817) |
| tool results | output schema | `isError: true` result | [mcp-BumlNux0.cjs:1822-1829](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L1822-L1829) |

Node limits: `requestTimeout` 0, `timeout` 0, `headersTimeout` 60 s, `keepAliveTimeout` 5 s ([http.ts:29-32](../src/http.ts#L29-L32)). When the client closes the response stream, the call is aborted: the request signal is aborted in `toNodeHandler` on `close`, the per-request transport is closed, and `ctx.mcpReq.signal` is aborted by the protocol layer ([node index.cjs:299-303](../node_modules/@modelcontextprotocol/node/dist/index.cjs#L299-L303), [server index.cjs:86-90](../node_modules/@modelcontextprotocol/server/dist/index.cjs#L86-L90), [src-BhnGYybN.cjs:6410-6427](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs#L6410-L6427)). A result produced after the abort is discarded ([src-BhnGYybN.cjs:6536](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs#L6536)).

### 6.2 Token and `headersHelper`

- The token is created once and kept in `DATA/token`; `--new-token` replaces it ([files.ts:107-126](../src/files.ts#L107-L126)).
- `DATA/headers.json` is rewritten at every start ([files.ts:132-134](../src/files.ts#L132-L134)).
- The Claude Code entry gets the command `cat <DATA>/headers.json`, or `type "<DATA>\headers.json"` on Windows, with the path quoted for the shell ([register.ts:50](../src/register.ts#L50), [log.ts:24-31](../src/log.ts#L24-L31)). Only this command is written to the entry and to the `claude mcp add-json` arguments ([register.test.ts:50-61](../test/unit/register.test.ts#L50-L61)).
- `token`, `headers.json` and `server.json` are set to mode 0600 at every write, and the data folder to 0700 at every start, except on Windows ([files.ts:85-88](../src/files.ts#L85-L88), [:149-152](../src/files.ts#L149-L152)).

### 6.3 Peer-written text

| Destination | Rule | Code |
|---|---|---|
| card and reply names | folded to one line; a valid display name is plain, others a code span | [`safeName`](../src/control.ts#L742-L745) |
| card text lines | one code span per line; backticks become `'`; a leading or trailing space becomes U+00A0; blank lines empty | [`codeLines`, `codeSpan`](../src/control.ts#L734-L740) |
| file names and paths on cards and posts | folded to one line, one code span | [control.ts:638](../src/control.ts#L638), [:651](../src/control.ts#L651), [:185-187](../src/control.ts#L185-L187) |
| reply quote on a send card | whitespace runs to one space, first 100 characters (code points), one code span | [control.ts:639](../src/control.ts#L639) |
| link texts on a connect card | code span lines in the bytes that the largest card form leaves; the line that does not fit is cut and ends in `…` inside its code span; later lines left out | [control.ts:699-713](../src/control.ts#L699-L713) |
| mention names | valid name: `@name` or `@'name'`; others a code span | [`mentionText`](../src/control.ts#L747-L750) |
| errors on cards | folded to one line, one code span | [control.ts:613](../src/control.ts#L613) |
| `--owners` part of restart lines in the chat | one code span | [control.ts:79](../src/control.ts#L79), [:818-820](../src/control.ts#L818-L820) |
| terminal names and errors of actions | folded to one line | [actions.ts:205-206](../src/actions.ts#L205-L206), [:396-406](../src/actions.ts#L396-L406), [control.ts:78](../src/control.ts#L78) |
| tool results | returned as data; schema descriptions and instructions mark `text`, `from`, `name`, file names, downloaded files, `description`, `welcome` and `commands` with their labels and params as data, not instructions; the instructions take a chat's messages as prompts only when the user said so in the session, and limit what the agent shares about its user | [tools.ts:194](../src/tools.ts#L194), [:198](../src/tools.ts#L198), [:203](../src/tools.ts#L203), [:228-229](../src/tools.ts#L228-L229), [:239-244](../src/tools.ts#L239-L244), [:407](../src/tools.ts#L407), [:449](../src/tools.ts#L449) |

The server writes the tappable form `/'...'` only in the card's command line ([control.ts:656-671](../src/control.ts#L656-L671)), in `/'revoke CHAT'` of standing cards and replies ([:611](../src/control.ts#L611), [:275](../src/control.ts#L275)), and in `/'delete CHAT files'` of the kept-files message ([:323](../src/control.ts#L323)).

### 6.4 Chat references

| Rule | Code |
|---|---|
| `^[@#][1-9]\d{0,14}$`: 1 to 15 digits; the core reads `#(2^64 + N)` as `#N` | [messages.ts:150-152](../src/messages.ts#L150-L152), [scenarios.test.ts:491-503](../test/e2e/scenarios.test.ts#L491-L503) |
| the control chat is refused by `send`, `receive`, `download`, `leave` and the owner commands with CHAT, and left out of `list` and `/list` | [tools.ts:1221](../src/tools.ts#L1221), [:1266-1285](../src/tools.ts#L1266-L1285), [control.ts:364-371](../src/control.ts#L364-L371) |
| a store error of the chat read is reported as `No chat CHAT.` | [tools.ts:1226](../src/tools.ts#L1226), [:1283](../src/tools.ts#L1283) |
| the agent's own links are refused by `connect` (`ownLink` plans) | [tools.ts:1387](../src/tools.ts#L1387), [:1405](../src/tools.ts#L1405), [:1427](../src/tools.ts#L1427) |

### 6.5 Owner commands

| Rule | Code |
|---|---|
| source: `groupRcv` text items in the main scope of the control group | [serve.ts:129](../src/serve.ts#L129), [control.ts:192](../src/control.ts#L192), [:573-575](../src/control.ts#L573-L575) |
| form: one line `/keyword params`, leading spaces allowed | [control.ts:593-598](../src/control.ts#L593-L598) |
| the help reply is sent to any sender; other commands are accepted from approvers only | [control.ts:223-224](../src/control.ts#L223-L224) |
| commands run one at a time in arrival order | [control.ts:196-199](../src/control.ts#L196-L199) |
| `/approve`, `/reject`, `/receive` and `/forward` need a pending action with a card, and the action's chat when CHAT is given; `/receive` and `/forward` take downloads only, `/approve` the other actions only | [control.ts:249-259](../src/control.ts#L249-L259) |

### 6.6 Files

| Rule | Code |
|---|---|
| `send` refuses files named `id_*`, `*.pem`, `*.key`, `*.p12` or `*.pfx`, and files under `~/.ssh`, `~/.gnupg`, the data folder and `~/.simplex-mcp` except the agent's own `DATA/files/`, each by its real path; the refusal is posted to the control chat and logged | [tools.ts:969-983](../src/tools.ts#L969-L983), [:1585-1604](../src/tools.ts#L1585-L1604), [control.ts:173-177](../src/control.ts#L173-L177) |
| the file of a `send` is sent only when its real path, size, inode and change time at the perform are those of the request | [tools.ts:985-990](../src/tools.ts#L985-L990) |
| a sent JPEG or PNG is a resized copy made by the core | [tools.ts:992-1003](../src/tools.ts#L992-L1003) |
| `ffmpeg` and `ffprobe` read a video as MP4 or QuickTime, local files only, without standard input, 30 s and 64 MiB of output each | [tools.ts:1654-1672](../src/tools.ts#L1654-L1672) |
| a download is saved only at `DATA/files/group-N/` or `DATA/files/contact-N/`, or for `/forward` at the same name under `DATA/forwarded/`, named by its file ID and an extension of 1 to 16 ASCII letters and digits | [messages.ts:215-227](../src/messages.ts#L215-L227) |
| a forwarded file is outside `DATA/files/`, so `send` refuses it; a download moves it into `DATA/files/` after the copy for the post is made | [tools.ts:579-596](../src/tools.ts#L579-L596), [:978-983](../src/tools.ts#L978-L983) |
| a file on unknown servers gets a card that names them, also under a files approval; relays are approved only for such a card, and for every other download, and in mode `host`, the core checks the complete description and downloads a file on unknown servers only when the IP address is protected | [tools.ts:1081](../src/tools.ts#L1081), [:602](../src/tools.ts#L602), [:1135](../src/tools.ts#L1135), [control.ts:666-669](../src/control.ts#L666-L669) |
| an accepted file that the core returns to `rcvInvitation` fails its download at the next check | [tools.ts:1148](../src/tools.ts#L1148) |
| a received file is moved into the chat's folder at its completion, unless it is at the forwarded path, also from a member chat of a group, which has a download in memory as the main conversation does; a file without a download in memory, such as one whose transfer started before a restart, only when it has its full size; a partial file is deleted only for a download in memory, when it is no larger than the file | [tools.ts:1187-1196](../src/tools.ts#L1187-L1196), [:1606-1612](../src/tools.ts#L1606-L1612) |

### 6.7 Link and network

- The join link is a group link of the control chat with role `member`, or `observer` with `--owners`; it is printed with a QR code on stdout and deleted at the first join event ([control.ts:442-452](../src/control.ts#L442-L452), [:408](../src/control.ts#L408), [:417](../src/control.ts#L417)).
- At every start the server sets private message routing: `smpProxyMode` `always`, `smpProxyFallback` `allowProtected` ([core.ts:39-40](../src/core.ts#L39-L40)).
- With `incognito: true` the join methods connect incognito ([tools.ts:828](../src/tools.ts#L828)); the core then uses a random profile for that chat ([PLAN, Participants](../PLAN.md#participants)).

---

## 7. Limits

| Limit | Value | Code |
|---|---|---|
| pending actions | 20 | [actions.ts:9](../src/actions.ts#L9) |
| actions being performed | 20 | [actions.ts:10](../src/actions.ts#L10) |
| expiry | 1 to 23 h, default 23 | [cli.ts:79](../src/cli.ts#L79), [:102-104](../src/cli.ts#L102-L104) |
| connect wait for the other side | `created + 23 h 50 min` | [tools.ts:152](../src/tools.ts#L152) |
| send, connect and download card | 15000 bytes JSON-escaped, in its largest form | [control.ts:26](../src/control.ts#L26), [:677-685](../src/control.ts#L677-L685) |
| error assumed in the largest card | 600 × `€` | [control.ts:30](../src/control.ts#L30) |
| message as the core sends it | 15602 bytes, counted with 600 bytes of envelope; `send` refuses a longer one, and the post of `/forward` leaves out its quote of the card | [tools.ts:162-163](../src/tools.ts#L162-L163), [:1553-1556](../src/tools.ts#L1553-L1556), [control.ts:287-303](../src/control.ts#L287-L303) |
| `send.text` | 1 to 12000 characters | [tools.ts:266](../src/tools.ts#L266) |
| `connect.message` | 1 to 2000 characters | [tools.ts:254](../src/tools.ts#L254) |
| `send.mention` | 1 to 3 unique member IDs | [tools.ts:270](../src/tools.ts#L270) |
| `wait` | 0 to 86400 s | [tools.ts:255](../src/tools.ts#L255), [:271](../src/tools.ts#L271), [:283](../src/tools.ts#L283), [:306](../src/tools.ts#L306) |
| `idle` | 1 to 300 s, default 30 | [tools.ts:256](../src/tools.ts#L256), [:272](../src/tools.ts#L272), [:158](../src/tools.ts#L158) |
| `limit` | 1 to 100, default 50 | [tools.ts:273](../src/tools.ts#L273), [:284](../src/tools.ts#L284), [:159](../src/tools.ts#L159) |
| `connect` snapshot | last 50 items | [tools.ts:160](../src/tools.ts#L160) |
| collected items per action or call | 100, then their IDs kept as overflow until deleted | [messages.ts:10](../src/messages.ts#L10), [:109-110](../src/messages.ts#L109-L110) |
| unread page | 100 items | [messages.ts:9](../src/messages.ts#L9) |
| chat list read | one read of 1000000 chats | [tools.ts:151](../src/tools.ts#L151) |
| `download` wait, default | 60 s | [tools.ts:155](../src/tools.ts#L155) |
| download check | every 60 s | [tools.ts:156](../src/tools.ts#L156) |
| download without progress | 30 min, then cancelled | [tools.ts:157](../src/tools.ts#L157) |
| `ffmpeg`, `ffprobe` | 30 s, 64 MiB of output | [tools.ts:161](../src/tools.ts#L161), [:165](../src/tools.ts#L165) |
| stop wait | 0 to 60 s, default 10 | [cli.ts:84](../src/cli.ts#L84), [:105-107](../src/cli.ts#L105-L107) |
| error details | 500 characters | [log.ts:4](../src/log.ts#L4) |
| progress interval | 60 s | [messages.ts:7](../src/messages.ts#L7) |
| longest expiry timer | 2147483647 ms, set again when it fires early | [messages.ts:8](../src/messages.ts#L8), [actions.ts:293](../src/actions.ts#L293) |
| HTTP body | 4 MiB | [server index.cjs:254](../node_modules/@modelcontextprotocol/server/dist/index.cjs#L254) |
| SSE keep-alive | 15 s | [mcp-BumlNux0.cjs:64](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L64) |
| agent NAME | 1 to 32 characters | [cli.ts:170](../src/cli.ts#L170) |
| display name | 50 characters, `mkValidName` rules | [messages.ts:11-12](../src/messages.ts#L11-L12), [:341-363](../src/messages.ts#L341-L363) |
| port | 1 to 65535, default 5226 | [cli.ts:82](../src/cli.ts#L82), [:114-116](../src/cli.ts#L114-L116) |

---

## 8. Claude Code plugin

The plugin runs the same tools, `Actions` and `Control` as `serve`, in other processes. ROOT is the plugin's data root, `~/.simplex-mcp` or `SIMPLEX_MCP_HOME` ([files.ts:30-33](../src/files.ts#L30-L33)).

### 8.1 System diagram

```mermaid
flowchart LR
  subgraph cc["Claude Code"]
    ses["session 1, session 2"]
    hk["SessionStart, PreToolUse,<br/>UserPromptSubmit, Stop,<br/>PostModelSwitch hooks"]
    sk["/simplex:link<br/>/simplex:reset-owners"]
  end
  subgraph ps["simplex-mcp session, one per Claude Code session"]
    mcp["McpServer, stdio<br/>tools and channel"]
    link["DaemonLink<br/>DaemonClient"]
  end
  subgraph pd["simplex-mcp daemon, one per ROOT"]
    sock["socket.ts listen<br/>bearer token"]
    dm["Daemon<br/>routes, entries, idle"]
    hub["Hub<br/>MainGroup"]
    sess["Session per Claude Code session<br/>Actions, AgentTools, Control"]
    user["User"]
    hcore["core _hub<br/>profile Claude"]
    ucore["core _user<br/>profile User"]
  end
  ses -->|stdio| mcp
  hk -->|"launch.js hook ..."| link
  sk -->|"launch.js link, reset-owners"| link
  mcp --> link
  link -->|"HTTP over ROOT/daemon.sock<br/>Authorization: Bearer"| sock
  link -.->|"spawn when not running"| pd
  sock --> dm --> hub
  hub --> sess
  hub --> user
  hub --> hcore
  sess --> hcore
  user --> ucore
  hcore --> hdb[("ROOT/_hub")]
  ucore --> udb[("ROOT/_user")]
  sess --> sdb[("ROOT/_sessions/ID<br/>session.json, files")]
  hcore <-->|"SMP, XFTP<br/>private routing"| net["SimpleX servers"]
  ucore <--> net
  net <--> owner["Owner's SimpleX app<br/>Main group, session groups"]
  net <--> peers["Peers"]
  pd --> logf[("ROOT/daemon.log")]
```

Data flow of a tool call:

1. Claude Code calls a tool of the MCP server `simplex`; the SDK validates the arguments ([session.ts:54](../src/session.ts#L54), [tools.ts:490-500](../src/tools.ts#L490-L500)).
2. Without a connection, the session process checks the daemon's status and version, spawns it when it is not running, sends `POST /session` and holds `POST /attach` open ([session.ts:138-158](../src/session.ts#L138-L158), [client.ts:32-44](../src/client.ts#L32-L44)).
3. The session process sends `POST /call` with the session ID, the tool and the input, relays each `{progress}` line as an MCP progress notification, and returns the `{result}` line ([session.ts:109-128](../src/session.ts#L109-L128)).
4. The daemon checks the token, finds or opens the session's entry, and runs the session's `AgentTools.call` with a signal tied to the request ([socket.ts:18-48](../src/socket.ts#L18-L48), [daemon.ts:222-239](../src/daemon.ts#L222-L239)).
5. `AgentTools` sends chat commands to the core `_hub`; actions are created in the session's `Actions` and posted as cards in the session's group by its `Control` ([hub.ts:63-87](../src/hub.ts#L63-L87)).
6. The core emits events to `Hub.onEvent`, which passes them to the Main group, to each session's `AgentTools.onEvent` and to the `Control` of the session group they belong to ([hub.ts:372-397](../src/hub.ts#L372-L397)).

Data flow of a prompt from the session group:

1. An owner's message in the group of session N reaches `Session.onMessage` ([hub.ts:376-381](../src/hub.ts#L376-L381), [:118-125](../src/hub.ts#L118-L125)).
2. Without an attachment, the hub replies `Session N is not running.`; otherwise its file, when it has one, is received, and the line `{prompt}` is written to every `/attach` stream of the session ([hub.ts:178-184](../src/hub.ts#L178-L184), [daemon.ts:211](../src/daemon.ts#L211)).
3. The session process sends it to Claude Code as `notifications/claude/channel` ([session.ts:50-53](../src/session.ts#L50-L53), [:170-174](../src/session.ts#L170-L174)).
4. The hooks post the terminal prompts and the final answer of each turn with `POST /turn` ([hook.ts:54-62](../src/hook.ts#L54-L62), [hub.ts:138-151](../src/hub.ts#L138-L151)).

### 8.2 Modules

#### [`session.ts`](../src/session.ts)

| Symbol | Line | Contents |
|---|---|---|
| `channelInstructions`, `permissionRequest` | [18-20](../src/session.ts#L18-L20) | instructions of the channel; schema of a permission request |
| `startSession` | [37](../src/session.ts#L37) | session ID check, options, `McpServer` with the instructions and the capabilities `claude/channel` and `claude/channel/permission`, tools with `reply`, the permission request handler, stdio transport, background connect |
| `DaemonLink.connect`, `start`, `close` | [72-89](../src/session.ts#L72-L89) | one shared connect promise; `start` logs a failure on stderr; `close` ends the attachment |
| `DaemonLink.permission` | [91](../src/session.ts#L91) | `POST /permission` after the connect |
| `DaemonLink.call` | [96](../src/session.ts#L96) | forward; one repeat after status 409; failures to tool errors |
| `DaemonLink.forward` | [109](../src/session.ts#L109) | `/call` stream: progress lines, the result line; a broken stream drops the connection |
| `DaemonLink.disconnect` | [131](../src/session.ts#L131) | drops the connection only when it is the current one |
| `DaemonLink.open` | [138](../src/session.ts#L138) | `ensureDaemon`, `/session`, `/attach` until its first line; each line to the channel |
| `failureText` | [161](../src/session.ts#L161) | not running or status 503: `The SimpleX daemon stopped; repeat the call.` |
| `channelNotification` | [170](../src/session.ts#L170) | attach lines to `notifications/claude/channel` and `notifications/claude/channel/permission` |

#### [`client.ts`](../src/client.ts)

| Symbol | Line | Contents |
|---|---|---|
| `DaemonClient.forRoot` | [19](../src/client.ts#L19) | ROOT created, token, socket path |
| `DaemonClient.ensureDaemon` | [32](../src/client.ts#L32) | status; a daemon of a lower version is stopped; a spawn when none runs; protocol check |
| `spawnDaemon` | [68](../src/client.ts#L68) | detached child with stdout and stderr to `daemon.log` and a ready pipe on fd 3 |
| `firstLine` | [91](../src/client.ts#L91) | first line of the ready pipe, a spawn error, or the 5-minute timeout |
| `compareVersions` | [117](../src/client.ts#L117) | numeric comparison of the first three version parts |

#### [`socket.ts`](../src/socket.ts)

| Symbol | Line | Contents |
|---|---|---|
| `SocketError` | [12](../src/socket.ts#L12) | message and HTTP status |
| `listen` | [18](../src/socket.ts#L18) | `http.Server` on the socket path: token check, route table, body read, error replies, timeouts; a stale socket file is removed |
| `sendJson`, `startLines` | [50-60](../src/socket.ts#L50-L60) | JSON reply; NDJSON stream |
| `isNotRunning` | [62](../src/socket.ts#L62) | `ENOENT` or `ECONNREFUSED` |
| `requestJson`, `streamLines` | [67-86](../src/socket.ts#L67-L86) | client request with a JSON reply; client request with NDJSON lines; a status other than 200 to `SocketError` |
| `send` | [88](../src/socket.ts#L88) | `http.request` on the socket path, without a connection pool, bearer header |

#### [`protocol.ts`](../src/protocol.ts)

| Symbol | Line | Contents |
|---|---|---|
| `protocolVersion` | [5](../src/protocol.ts#L5) | `1` |
| `SessionRequest`, `TitleRequest` | [17-30](../src/protocol.ts#L17-L30) | `/session`, `/title` |
| `PermissionRequest`, `TurnRequest` | [32-46](../src/protocol.ts#L32-L46) | `/permission`, `/turn` |
| `AttachLine` | [48](../src/protocol.ts#L48) | `{prompt}` and `{permission}` lines of `/attach` |
| `TitleReply`, `SessionReply`, `AttachRequest`, `CallRequest`, `CallLine` | [53-74](../src/protocol.ts#L53-L74) | replies and `/call` |
| `StatusReply`, `ResetReply` | [76-84](../src/protocol.ts#L76-L84) | `/status`, `/reset-owners` |

#### [`daemon.ts`](../src/daemon.ts)

| Symbol | Line | Contents |
|---|---|---|
| `HubLike`, `SessionLike`, `DaemonDeps` | [21-51](../src/daemon.ts#L21-L51) | interfaces the daemon uses; tests replace the hub |
| `Entry` | [53](../src/daemon.ts#L53) | opening promise, session, open attachments, their writers, running calls, idle timer |
| `Daemon.start` | [78](../src/daemon.ts#L78) | lock, stored sessions and the next number, listener before the hub opens, hub, stop timer |
| `Daemon.routes`, `withHub`, `expiring` | [115-152](../src/daemon.ts#L115-L152) | the nine routes; requests wait for the hub; due actions expire first |
| `Daemon.shutdown` | [154](../src/daemon.ts#L154) | stop sequence |
| `onSession`, `onTitle`, `rename` | [177-200](../src/daemon.ts#L177-L200) | registration and options; a session opened or created at its title, its name and model |
| `onAttach`, `onCall`, `hold` | [202-249](../src/daemon.ts#L202-L249) | attachment with the session's line writer; call stream; streams refused while stopping |
| `entry`, `refuseWhenStopping`, `folder`, `openAgent` | [252-296](../src/daemon.ts#L252-L296) | session registry; status 503 while stopping, 400 for an ID that is not a session ID, 409 for an unknown one without a request; a new number and name |
| `scheduleIdle`, `scheduleStop` | [298-316](../src/daemon.ts#L298-L316) | idle close of a session, idle stop of the daemon |
| `runDaemon` | [324](../src/daemon.ts#L324) | process entry: ROOT, start, ready line, signal handlers |
| `productionDeps` | [348](../src/daemon.ts#L348) | token, `Hub.open` |
| `sessionName` | [358](../src/daemon.ts#L358) | session title, else the project folder name, else `Claude Code` |
| `readyChannel` | [364](../src/daemon.ts#L364) | ready line to fd `SIMPLEX_MCP_READY_FD`, or stdout |

#### [`hub.ts`](../src/hub.ts)

| Symbol | Line | Contents |
|---|---|---|
| constants | [18-24](../src/hub.ts#L18-L24) | profile name `Claude`, session data key, first action number, 10 s stop wait, 60 s prompt memory, 12000-byte parts, the channel tag of this plugin |
| `hubTexts` | [26](../src/hub.ts#L26) | refusal texts of a lost session group and of a stop |
| `SessionGroupData`, `InvitedMember` | [33-41](../src/hub.ts#L33-L41) | session number; members invited through the Main group with their Main group member |
| `Session` | [43](../src/hub.ts#L43) | one Claude Code session: `Actions` with the hub's texts, `AgentTools` with the session's ID and folder, `Control` of the session group with the session's approver check |
| `Session.requestMode` | [107](../src/hub.ts#L107) | `requests` at once; `host` as a `mode` action |
| `Session.onMessage` | [118](../src/hub.ts#L118) | commands to `Control`; other messages of approvers to prompts, or the reply `Session N is not running.` |
| `Session.permission` | [127](../src/hub.ts#L127) | a `permission` action in mode `requests`; its verdict to the attachments; removal when decided |
| `Session.turn` | [138](../src/hub.ts#L138) | short description; terminal prompts posted by `User` unless pushed; the end of open permissions and the answer at the end of a turn |
| `Session.endPermissions`, `fileDone` | [153-164](../src/hub.ts#L153-L164) | expiry of pending permissions; the end of a prompt's file |
| `Session.invite`, `removeOwners` | [166-176](../src/hub.ts#L166-L176) | invitation through the Main group and its record; removal of the invited members |
| `Session.prompt`, `receive`, `answer`, `pushed` | [178-207](../src/hub.ts#L178-L207) | prompt line with the file's path; the file received into the session's folder; the answer in parts with its quote; the echo check |
| `Session.approves`, `setMode` | [209-218](../src/hub.ts#L209-L218) | approver check of decision 17; the mode and its record in `session.json` |
| `Hub.open` | [244](../src/hub.ts#L244) | core `_hub`, Main group found or created, `MainGroup` load and start, `User`, the stored sessions, events |
| `Hub.configure`, `resetOwners` | [271-281](../src/hub.ts#L271-L281) | expiry of all sessions, Main group name; reset at once without owners, else a card |
| `Hub.session` | [283](../src/hub.ts#L283) | the open session renamed, or a new `Session` with its stored or new group; the owners invited to a new group |
| `Hub.sessionsText`, `join`, `inviteOwner`, `removeOwners` | [307-333](../src/hub.ts#L307-L333) | `SessionGroups` of the Main group |
| `Hub.stop` | [339](../src/hub.ts#L339) | actions stopped, events awaited, cancelled cards awaited up to 10 s, cores closed |
| `storedGroup`, `createSessionGroup` | [351-370](../src/hub.ts#L351-L370) | the group of `session.json` with a matching session number; a new group with `User` added |
| `Hub.onEvent` | [372](../src/hub.ts#L372) | event dispatch |
| `textParts` | [403](../src/hub.ts#L403) | pieces of at most 12000 encoded bytes |

#### [`main-group.ts`](../src/main-group.ts)

| Symbol | Line | Contents |
|---|---|---|
| `mainCommands`, texts | [7-15](../src/main-group.ts#L7-L15) | command menu; greeting, help reply, reset card, log line |
| `SessionGroups` | [17](../src/main-group.ts#L17) | what the Main group asks of the hub |
| `MainGroup.owners`, `isOwner` | [52-58](../src/main-group.ts#L52-L58) | human members with role `owner` while the group is not lost |
| `MainGroup.load`, `start` | [60-73](../src/main-group.ts#L60-L73) | lost check, name and menu, members; a link without owners |
| `MainGroup.onMessage`, `command` | [80-121](../src/main-group.ts#L80-L121) | `/sessions`, `/join N`, `/approve reset`, `/reject reset`, help |
| `MainGroup.onEvent`, `handleEvent`, `ownerJoined` | [92-156](../src/main-group.ts#L92-L156) | the first human who joins becomes an owner; a new link when no owner remains; new owners invited to the open sessions |
| `MainGroup.askReset`, `replace`, `decideReset` | [96-133](../src/main-group.ts#L96-L133) | reset card; a new Main group, owners removed from the session groups, a new link |
| `openLink`, `deleteLink` | [163-177](../src/main-group.ts#L163-L177) | group link with role `member` |
| `findMainGroup`, `createMainGroup` | [203-217](../src/main-group.ts#L203-L217) | the latest group with `simplexMcpMain`; a new one with history, direct messages for owners and the menu |

#### [`user.ts`](../src/user.ts)

| Symbol | Line | Contents |
|---|---|---|
| `User.open` | [16](../src/user.ts#L16) | core `_user` with profile `User`, events, a connection to `Claude` when the hub has no contact marked `simplexMcpUser` |
| `User.post` | [29](../src/user.ts#L29) | messages to the group marked with the session number |
| `User.connect` | [38](../src/user.ts#L38) | a one-time invitation of the hub, joined by `User`, `contactConnected` awaited 60 s, the hub's contact marked |
| `User.onEvent` | [48](../src/user.ts#L48) | the contact marked `simplexMcpClaude`; invitations of that contact to a group named `N · ...` joined and marked |
| `userContact` | [58](../src/user.ts#L58) | the hub's contact marked `simplexMcpUser` |

#### [`core.ts`](../src/core.ts)

| Symbol | Line | Contents |
|---|---|---|
| `openCore` | [20](../src/core.ts#L20) | folder, `ChatApi.init` with prefix `FOLDER/simplex`, active user or a new one with peer type `agent`, `prepareCore`, event buffer, `startChat`, preset contact deleted for a new user, profile update |
| `deletePresetContacts` | [45](../src/core.ts#L45) | the contact `Ask SimpleX Team`, deleted when it has no connection |

#### [`hook.ts`](../src/hook.ts), [`owner.ts`](../src/owner.ts), [`qr.ts`](../src/qr.ts)

| Symbol | Line | Contents |
|---|---|---|
| `sessionStart` | [hook.ts:37](../src/hook.ts#L37) | Node.js check, options, `ensureDaemon`, `POST /title` with the title, the folder and the model, the link message |
| `turn` | [hook.ts:54](../src/hook.ts#L54) | `POST /turn` with the prompt, the last answer, the new model and the effort level; output `{}` |
| `linkText`, `resetOwnersText` | [owner.ts:10-20](../src/owner.ts#L10-L20) | owners, or the link with a QR code on a terminal only |
| `showLink`, `resetOwners` | [owner.ts:22-33](../src/owner.ts#L22-L33) | `link`; `reset-owners`, from a terminal only without owners |
| `qrCodeText` | [qr.ts:14](../src/qr.ts#L14) | `small` and `large` with ANSI colors for terminals, `plain` for the hook message |

#### [`config.ts`](../src/config.ts), [`files.ts`](../src/files.ts)

| Symbol | Line | Contents |
|---|---|---|
| `readConfig` | [config.ts:25](../src/config.ts#L25) | `CLAUDE_PLUGIN_OPTION_*` to `Config`, defaults for blank values, `ConfigError` texts |
| `dataRoot`, `hubFolder`, `sessionFolder` | [files.ts:30-46](../src/files.ts#L30-L46) | ROOT, `ROOT/_hub`, `ROOT/_sessions/<lower-case ID>` |
| `SessionFile`, `readSessionFile`, `writeSessionFile`, `storedSessions` | [files.ts:13-64](../src/files.ts#L13-L64) | `session.json`: number, name, project, creation time, group, mode |
| `socketPath` | [files.ts:66](../src/files.ts#L66) | `ROOT/daemon.sock`; over 103 bytes `TMP/simplex-mcp-HASH.sock`; Windows `\\.\pipe\simplex-mcp-HASH` |
| `logFile`, `openLog` | [files.ts:73-83](../src/files.ts#L73-L83) | `ROOT/daemon.log`, emptied when over 10 MB, mode 0600 |

#### Plugin files

| File | Contents |
|---|---|
| [`.claude-plugin/plugin.json`](../.claude-plugin/plugin.json) | plugin `simplex`, version, description, `userConfig` |
| [`.mcp.json`](../.mcp.json) | server `simplex`: `node plugin/launch.js session`, tool call timeout, options as environment variables |
| [`hooks/hooks.json`](../hooks/hooks.json) | SessionStart: `launch.js hook session-start`; PreToolUse: `allow.js`; UserPromptSubmit, Stop, PostModelSwitch: `launch.js hook turn`, in the background |
| [`plugin/launch.js`](../plugin/launch.js) | `npx -y @simplex-chat/mcp@VERSION ARGS` in the plugin's folder, signals forwarded, exit code passed on |
| [`plugin/allow.js`](../plugin/allow.js) | PreToolUse allow decision |
| [`skills/link/SKILL.md`](../skills/link/SKILL.md), [`skills/reset-owners/SKILL.md`](../skills/reset-owners/SKILL.md) | `/simplex:link`, `/simplex:reset-owners` |
| [`/.claude-plugin/marketplace.json`](../../../.claude-plugin/marketplace.json) | marketplace `simplex-chat`, plugin source npm `@simplex-chat/mcp` |

### 8.3 State

Memory of the daemon:

| Item | Holder | Written by | Read by |
|---|---|---|---|
| Entries of open sessions | `Daemon.entries` ([daemon.ts:63](../src/daemon.ts#L63)) | `entry`, idle close, stop | routes, `status` |
| Line writers of the attachments | `Entry.writers` ([daemon.ts:58](../src/daemon.ts#L58)) | `onAttach` | `Session.onLine` |
| Stored sessions, next number | `Daemon.stored`, `nextNumber` ([daemon.ts:65-66](../src/daemon.ts#L65-L66)) | `Daemon` constructor, `openAgent` | `openHub`, `openAgent` |
| Sessions | `Hub.sessions`, by number ([hub.ts:229](../src/hub.ts#L229)) | `Hub.session` | events, Main group commands |
| Actions of a session | `Session.actions`, numbered from 1 per session group ([hub.ts:46](../src/hub.ts#L46), [:64](../src/hub.ts#L64)) | tools, `permission`, `requestMode` | `Control`, `busy` |
| Pushed prompts | `Session.prompts`, kept 60 s ([hub.ts:49](../src/hub.ts#L49)) | `prompt` | `pushed` |
| Files of prompts in transfer | `Session.receiving` ([hub.ts:50](../src/hub.ts#L50)) | `receive` | `fileDone` |
| Model and effort | `Session.description` ([hub.ts:51](../src/hub.ts#L51)) | `turn` | `turn` |
| The prompt of the turn from the group | `Session.turnItem` ([hub.ts:52](../src/hub.ts#L52)) | `prompt`; cleared by a terminal prompt and by the answer | `answer` |
| Members of the Main group, its link, the reset card | `MainGroup` ([main-group.ts:26-30](../src/main-group.ts#L26-L30)) | `refreshMembers`, `openLink`, `askReset` | owner checks, `/status` |

Memory of a session process: one connection promise and one attachment ([session.ts:65-70](../src/session.ts#L65-L70)).

Chat database custom data, all in the core `_hub` unless noted:

| Item | Entity | Value | Written by | Read by |
|---|---|---|---|---|
| Main group marker | Main group | `{simplexMcpMain: true}` | [`createMainGroup`](../src/main-group.ts#L208-L217) | [`findMainGroup`](../src/main-group.ts#L203-L206) |
| Session group | group of session N | `{simplexMcpSession: {session: N, invited: [{groupMemberId, mainMemberId}]}}` | [`createSessionGroup`](../src/hub.ts#L358-L370), [`Session.invite`](../src/hub.ts#L166-L170), [`removeOwners`](../src/hub.ts#L172-L176) | [`storedGroup`](../src/hub.ts#L351-L356), [`Session.approves`](../src/hub.ts#L209-L212) |
| Action counter | group of session N | `{simplexMcp: {control: true, nextAction}}`, merged with the other keys | [`saveCounter`, `saveData`](../src/control.ts#L146-L158) | [`Session` constructor](../src/hub.ts#L64) |
| Chat owner | contact or group of a peer | `{...other keys, simplexMcpOwner: {session: ID}}` | [`claim`](../src/tools.ts#L573-L577), [`giveChat`](../src/tools.ts#L560-L563) | [`owns`](../src/tools.ts#L569-L571) |
| Standing approvals | contact or group of a peer | as in [3.2](#32-chat-database-custom-data) | `Control` of the owning session's group | the same |
| User contact | the hub's contact of `User` | `{simplexMcpUser: true}` | [`User.connect`](../src/user.ts#L38-L46) | [`userContact`](../src/user.ts#L58-L60) |
| Claude contact | in `_user`: the contact of `Claude` | `{simplexMcpClaude: true}` | [`User.onEvent`](../src/user.ts#L49) | [`User.onEvent`](../src/user.ts#L50) |
| Session of a group | in `_user`: a session group | `{simplexMcpSession: N}` | [`User.onEvent`](../src/user.ts#L54) | [`User.post`](../src/user.ts#L30) |

Files:

| Path | Contents | Written | Read |
|---|---|---|---|
| `ROOT/` | data root, mode 0700 | every client and the daemon ([client.ts:20](../src/client.ts#L20), [daemon.ts:329](../src/daemon.ts#L329)) | |
| `ROOT/token` | the bearer token, 0600 | the first client or daemon ([files.ts:107-126](../src/files.ts#L107-L126)) | every client, the daemon |
| `ROOT/lock` | PID of the daemon | `Daemon.start` ([daemon.ts:79](../src/daemon.ts#L79)) | the next daemon |
| `ROOT/daemon.sock` | the socket | `listen` ([socket.ts:40-43](../src/socket.ts#L40-L43)) | clients |
| `ROOT/daemon.log` | the daemon's stdout and stderr | `spawnDaemon` ([client.ts:69-75](../src/client.ts#L69-L75)) | people |
| `ROOT/_hub/`, `ROOT/_user/` | databases and `tmp/` of the profiles `Claude` and `User` | [`openCore`](../src/core.ts#L20-L36) | the cores |
| `ROOT/_sessions/ID/session.json` | `{number, name, project, created, group?, mode?}`, 0600 | `openAgent`, `rename`, `Hub.session`, `setMode` ([daemon.ts:287-296](../src/daemon.ts#L287-L296), [:195-200](../src/daemon.ts#L195-L200), [hub.ts:293](../src/hub.ts#L293), [:214-218](../src/hub.ts#L214-L218)) | `Daemon` constructor, `Hub.session` |
| `ROOT/_sessions/ID/files/` | downloads of the session's chats as in [3.3](#33-files); `group/` with the files of the owners' prompts; `sent/`, deleted when the session opens | the core ([hub.ts:186-193](../src/hub.ts#L186-L193), [:290](../src/hub.ts#L290)) | tools, Claude |

### 8.4 Process lifecycle

#### Session start

1. The SessionStart hook checks Node.js 22, reads the options, starts the daemon when it is not running, and sends `POST /title` with the session ID, title, project folder, model and options ([hook.ts:37-52](../src/hook.ts#L37-L52)).
2. `/title` opens the session: from `session.json`, or with a new number and the name from the title or the folder; `Hub.session` finds the group of `session.json` or creates one, adds `User`, invites the Main group's owners, and loads its `Control` ([daemon.ts:186-193](../src/daemon.ts#L186-L193), [:287-296](../src/daemon.ts#L287-L296), [hub.ts:283-301](../src/hub.ts#L283-L301)).
3. A new title renames the group and `session.json`; the model is written to the group's short description ([daemon.ts:190-191](../src/daemon.ts#L190-L191)).
4. The hook prints the link message while the Main group has no owner, else `{}`.
5. The session process connects in the background: `/session` registers the options and requests the mode, `/attach` sets the session's line writer ([daemon.ts:177-184](../src/daemon.ts#L177-L184), [:202-220](../src/daemon.ts#L202-L220)).

#### Daemon spawn and version check

```
ensureDaemon(VERSION):                               client.ts:32
  GET /status
    not running: none
    version below VERSION: POST /stop; none
  none: spawn "node dist/cli.js daemon", wait for its ready line
    ready, running: GET /status
    error: TEXT: ExitError TEXT
    exit without a line: ExitError "The SimpleX daemon exited; see ROOT/daemon.log"
    5 minutes: ExitError "The SimpleX daemon did not start in 5 minutes."
  protocol other than 1: ExitError "Restart Claude Code to update the SimpleX plugin."
```

#### Daemon start

1. `ROOT` is created, the options are read, and the lock is taken; another running daemon makes the ready line `running` ([daemon.ts:324-334](../src/daemon.ts#L324-L334), [:78-81](../src/daemon.ts#L78-L81)).
2. The stored sessions are read and the next number set ([daemon.ts:73-76](../src/daemon.ts#L73-L76)).
3. The socket listens before the hub opens; requests wait for the hub ([daemon.ts:83-84](../src/daemon.ts#L83-L84), [:140-145](../src/daemon.ts#L140-L145)).
4. `Hub.open`: core `_hub` with profile `Claude`; the Main group found by its marker or created; `MainGroup.load` and `start`, with a link while it has no owner; core `_user` and its connection to `Claude` at the first start; a `Session` for each stored session; events ([hub.ts:244-260](../src/hub.ts#L244-L260)).
5. A failed hub open closes the listener and releases the lock; the requests that waited get status 500 ([daemon.ts:85-96](../src/daemon.ts#L85-L96)).
6. The stop timer starts; the log line `SimpleX MCP daemon VERSION, data folder ROOT`; the ready line `ready` ([daemon.ts:91](../src/daemon.ts#L91), [:336-337](../src/daemon.ts#L336-L337)).

#### Event dispatch

`Hub.onEvent` ([hub.ts:372-397](../src/hub.ts#L372-L397)), after a stop: nothing; otherwise `expireDue` of every session first:

| Event | Condition | Handler |
|---|---|---|
| `newChatItems` | item of the Main group | [`MainGroup.onMessage`](../src/main-group.ts#L80) |
| `newChatItems` | item of a session group | [`Session.onMessage`](../src/hub.ts#L118) |
| `rcvFileComplete`, `rcvFileError` | | [`Session.fileDone`](../src/hub.ts#L159) of every session |
| any event | | [`AgentTools.onEvent`](../src/tools.ts#L657) of every session |
| `sndFileError` | a chat of the session, or its group | [`Control.fileNotSent`](../src/control.ts#L179) |
| membership events | of the Main group | [`MainGroup.onEvent`](../src/main-group.ts#L92), queued |
| membership events | of a session group | [`Control.onEvent`](../src/control.ts#L202), queued |

#### Attach, idle close and idle stop

- An attachment cancels the session's idle timer; its end, when no other attachment remains, clears the line writer, ends the pending permissions and starts the timer ([daemon.ts:202-220](../src/daemon.ts#L202-L220)).
- The idle timer, 10 minutes, closes the entry when the session has no running call and no action in memory; otherwise it starts again ([daemon.ts:298-308](../src/daemon.ts#L298-L308)). A closed session keeps its `Session` in the hub, so its group answers `/list` and `/revoke` ([hub.ts:283-288](../src/hub.ts#L283-L288)).
- When no entry remains, the stop timer, 10 minutes, stops the daemon ([daemon.ts:310-316](../src/daemon.ts#L310-L316)).

#### Stop

On `SIGINT`, `SIGTERM`, `POST /stop` or the stop timer ([daemon.ts:154-175](../src/daemon.ts#L154-L175)):

1. Timers are cleared, the listener closes, and open streams are destroyed.
2. `Hub.stop`: the actions of every session are stopped, cards edited to `Cancelled: the server stopped.`; queued group events are awaited; cancelled cards get 10 s to be sent; the download checks stop; the cores `_user` and `_hub` close ([hub.ts:339-349](../src/hub.ts#L339-L349)).
3. `Cancelled K action(s).`, the lock is released, `Stopped.`; the process exits after the `/stop` reply.

### 8.5 Approval model

| Rule | Code |
|---|---|
| An approver in a session group is a member recorded in `invited` whose member in the Main group is a current owner; prompts, approvals and tool-use decisions count only from approvers | [hub.ts:209-212](../src/hub.ts#L209-L212), [control.ts:524-526](../src/control.ts#L524-L526), [main-group.ts:52-58](../src/main-group.ts#L52-L58) |
| A new action of a session is refused while its group has no approver | [control.ts:134-135](../src/control.ts#L134-L135) |
| The first human who joins the Main group while it has no owner becomes an owner, and the link is deleted | [main-group.ts:140](../src/main-group.ts#L140), [:150-156](../src/main-group.ts#L150-L156) |
| A session's mode `host` takes effect after an owner approves its `mode` card; `requests` at once | [hub.ts:107-116](../src/hub.ts#L107-L116), [tools.ts:555-556](../src/tools.ts#L555-L556) |
| While owners exist, a reset of the owners takes effect after an owner approves its card in the Main group | [hub.ts:277-281](../src/hub.ts#L277-L281), [main-group.ts:123-133](../src/main-group.ts#L123-L133) |
| A chat without an owner gets the session that connects it, at the perform or at the connection event; tools of other sessions don't show it; `/give CHAT N` moves it | [tools.ts:569-577](../src/tools.ts#L569-L577), [:815](../src/tools.ts#L815), [:873](../src/tools.ts#L873), [control.ts:331-343](../src/control.ts#L331-L343) |

Tool-use state machine, one action of tool `permission` per request:

| Event | From | To | Code |
|---|---|---|---|
| `permission_request` | | `pending`, card posted | [hub.ts:127-128](../src/hub.ts#L127-L128) |
| `/approve N` | `pending` | `performing`, `allow` pushed, then `done` | [hub.ts:130-131](../src/hub.ts#L130-L131), [tools.ts:545-558](../src/tools.ts#L545-L558) |
| `/reject N` | `pending` | `rejected`, `deny` pushed | [hub.ts:130-131](../src/hub.ts#L130-L131) |
| end of the turn, end of the last attachment, expiry | `pending` | `expired`, card edited to `Expired.` | [hub.ts:149-157](../src/hub.ts#L149-L157), [actions.ts:233-236](../src/actions.ts#L233-L236) |
| `done`, `rejected` or `expired` | | removed from the session's actions | [hub.ts:132-134](../src/hub.ts#L132-L134) |

```mermaid
stateDiagram-v2
  [*] --> pending: permission_request
  pending --> performing: /approve N, allow pushed
  performing --> done
  pending --> rejected: /reject N, deny pushed
  pending --> expired: turn end, detach, expiry
  done --> [*]: removed
  rejected --> [*]: removed
  expired --> [*]: removed
```

### 8.6 Channel and mirroring

| Direction | Message | Code |
|---|---|---|
| daemon to session | `{prompt: {text, from, item, file?}}`, `{permission: {requestId, behavior}}` on `/attach` | [protocol.ts:48-51](../src/protocol.ts#L48-L51) |
| session to Claude Code | `notifications/claude/channel` `{content, meta: {from, item, file?}}`; `notifications/claude/channel/permission` `{request_id, behavior}` | [session.ts:170-174](../src/session.ts#L170-L174) |
| Claude Code to session | `notifications/claude/channel/permission_request` `{request_id, tool_name, description, input_preview}` | [session.ts:55-58](../src/session.ts#L55-L58) |
| session to daemon | `POST /permission` | [session.ts:91-94](../src/session.ts#L91-L94) |
| hooks to daemon | `POST /turn` `{id, prompt?, answer?, model?, effort?}` | [hook.ts:54-62](../src/hook.ts#L54-L62) |

```
on owner's message m in group N:   no attachment: reply "Session N is not running."
                                   else: receive its file; pushed += (m.text, m.item, now); push {prompt}
on /turn model or effort:          short description "MODEL · effort LEVEL"
on /turn prompt p:                 p pushed in the last 60 s, raw or in this plugin's channel tag with its item: skip
                                   else: turn prompt = none; User posts parts(p)
on /turn answer a:                 end pending permissions; Claude posts parts(a), the first quoting the turn's prompt when it fits
```

Sources: [hub.ts:118-157](../src/hub.ts#L118-L157), [:178-207](../src/hub.ts#L178-L207), [:403-418](../src/hub.ts#L403-L418).

### 8.7 Security boundaries

| Rule | Code |
|---|---|
| The socket is in ROOT, mode 0700, or a named pipe on Windows; every request needs the token, compared with `timingSafeEqual` | [socket.ts:19-25](../src/socket.ts#L19-L25), [files.ts:66-71](../src/files.ts#L66-L71) |
| The Main group link is shown only in the hook's `systemMessage` and by `link` on a terminal; tool results, refusals and other output name the command instead | [hook.ts:46-48](../src/hook.ts#L46-L48), [owner.ts:10-15](../src/owner.ts#L10-L15), [control.ts:34](../src/control.ts#L34) |
| `reset-owners` without owners runs only on a terminal | [owner.ts:29](../src/owner.ts#L29) |
| The launcher runs npx in the plugin's folder | [launch.js:8](../plugin/launch.js#L8) |
| `send` and `reply` refuse the files of ROOT except `files/` of the session's own folder | [tools.ts:978-983](../src/tools.ts#L978-L983), [hub.ts:79-80](../src/hub.ts#L79-L80) |
| `User`'s messages are not prompts: `User` is not an invited owner; `User` joins only groups its `Claude` contact invites it to | [hub.ts:122](../src/hub.ts#L122), [user.ts:48-55](../src/user.ts#L48-L55) |
| The hub's own groups and the `User` contact have no owner key, so the tools of every session leave them out | [tools.ts:569-571](../src/tools.ts#L569-L571), [:1266-1274](../src/tools.ts#L1266-L1274) |

### 8.8 Limits

| Limit | Value | Code |
|---|---|---|
| socket request body | 1 MiB | [socket.ts:6](../src/socket.ts#L6) |
| idle close of a session, idle stop of the daemon | 10 min each | [daemon.ts:16-17](../src/daemon.ts#L16-L17) |
| daemon start wait | 5 min | [client.ts:10](../src/client.ts#L10) |
| `User` connection at the first start | 60 s | [user.ts:11](../src/user.ts#L11) |
| prompt memory for the echo check | 60 s | [hub.ts:22](../src/hub.ts#L22) |
| part of an answer or a terminal prompt | 12000 bytes JSON-escaped | [hub.ts:23](../src/hub.ts#L23), [:403-418](../src/hub.ts#L403-L418) |
| cancelled cards wait at stop | 10 s | [hub.ts:21](../src/hub.ts#L21) |
| `daemon.log` | emptied at a daemon start over 10 MB | [files.ts:11](../src/files.ts#L11), [:77-83](../src/files.ts#L77-L83) |
| socket path | 103 bytes | [files.ts:10](../src/files.ts#L10) |
| actions per session | 20 pending, 20 performing | [actions.ts:9-10](../src/actions.ts#L9-L10), [:195-199](../src/actions.ts#L195-L199) |
