# SimpleX MCP server: architecture

> System structure, modules, state, process lifecycle, approval model and security boundaries of `@simplex-chat/mcp` 0.3.0.
>
> Related specs: [README](README.md) | [Flows](flows.md) | [API](api.md)
> Related docs: [README](../README.md) | [Reference](../REFERENCE.md) | [PLAN](../PLAN.md)

**Source:** [`cli.ts`](../src/cli.ts) | [`serve.ts`](../src/serve.ts) | [`http.ts`](../src/http.ts) | [`tools.ts`](../src/tools.ts) | [`actions.ts`](../src/actions.ts) | [`control.ts`](../src/control.ts) | [`messages.ts`](../src/messages.ts) | [`files.ts`](../src/files.ts) | [`register.ts`](../src/register.ts) | [`log.ts`](../src/log.ts) | [`status.ts`](../src/status.ts) | [`inference.ts`](../src/inference.ts)

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
  subgraph host["MCP host"]
    cc["Claude Code"]
  end
  subgraph proc["Node.js process: simplex-mcp serve NAME"]
    http["http.ts<br/>127.0.0.1:PORT/mcp<br/>Host and Origin guards<br/>bearer token<br/>path /mcp"]
    sdk["MCP SDK<br/>createMcpHandler<br/>McpServer per request"]
    tools["tools.ts<br/>AgentTools"]
    status["status.ts<br/>Statuses"]
    inference["inference.ts<br/>inferencePoller"]
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
  tools --> status
  status --> lib
  status --> inference
  serve --> inference
  inference -->|"GET /running, /props, /slots"| llm["Inference endpoint:<br/>llama-swap or llama-server"]
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
3. An `McpServer` is built from the factory for each request, and the arguments are validated against the input schema by the SDK ([`serve.ts:116-120`](../src/serve.ts#L116-L120), [`mcp-BumlNux0.cjs:1812-1817`](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L1812-L1817)).
4. The handler result is returned by `AgentTools.call` as `structuredContent` and as JSON text ([`tools.ts:514-524`](../src/tools.ts#L514-L524)).
5. The server calls `ChatApi` methods of the core ([`messages.ts:267-283`](../src/messages.ts#L267-L283)); actions are created in `Actions` and shown to the owner by `Control` ([`actions.ts:155-186`](../src/actions.ts#L155-L186)).
6. The core emits events; the server dispatches them in one handler of `serve.ts` to the tools, the item watch and the agent control chat ([`serve.ts:134-170`](../src/serve.ts#L134-L170)).

---

## 2. Modules

### [2.1 `cli.ts`](../src/cli.ts)

Entry point of the `simplex-mcp` binary.

| Symbol | Line | Contents |
|---|---|---|
| `helpText` | [10](../src/cli.ts#L10) | `--help` output |
| `Command` | [44](../src/cli.ts#L44) | `help`, `serve` with `ServeOptions`, `register` with `RegisterOptions` |
| `parseCommand` | [49](../src/cli.ts#L49) | command, NAME and options to a `Command` |
| `serveOptions` | [58](../src/cli.ts#L58) | defaults and option switch of `serve`; `--reset-owners` with an `--owners` list and `--inference` with `--approve host` refused |
| `registerOptions` | [120](../src/cli.ts#L120) | defaults and option switch of `register` |
| `OptionReader` | [137](../src/cli.ts#L137) | `value`: the next argument, required<br>`optionalValue`: the next argument, absent when missing or starting with `--` |
| `validAgentName` | [159](../src/cli.ts#L159) | `^[a-z0-9](-?[a-z0-9])*-?$`, at most 32 characters |
| `inferenceUrl` | [184](../src/cli.ts#L184) | `--inference`: an `http` or `https` URL without user name and password, to its origin and path without trailing slashes |
| `usageError` | [210](../src/cli.ts#L210) | `ExitError` with code 2 |
| `main` | [214](../src/cli.ts#L214) | `help` to stdout, `serve`, `register` |
| top-level catch | [227-236](../src/cli.ts#L227-L236) | `ExitError`: message on stderr, its code; other errors: `logError`, code 1 |

### [2.2 `serve.ts`](../src/serve.ts)

Start sequence, event dispatch and stop sequence of one agent.

| Symbol | Line | Contents |
|---|---|---|
| `version` | [16](../src/serve.ts#L16) | the package version |
| `OwnerEntry` | [20](../src/serve.ts#L20) | `{memberId, name}` of `--owners` |
| `ServeOptions` | [25](../src/serve.ts#L25) | name, data folder and its `dataGiven` flag, display name, mode, owners, expiry, reset and token flags, port, QR code size, stop wait, inference URL |
| `RunningServer` | [41](../src/serve.ts#L41) | token, `ChatApi`, user, `Actions`, `stop` |
| `serve` | [49](../src/serve.ts#L49) | `startServer`, then SIGINT and SIGTERM handlers: the first signal stops, a second ends the stop wait |
| `startServer` | [67](../src/serve.ts#L67) | inference poller with its key, data folder, lock, token, headers file, `ChatApi.init`, `startAgent`; cleanup on error |
| `startAgent` | [92](../src/serve.ts#L92) | user, network config, temp folder, control group, wiring of `Actions`, `AgentTools` with the inference poller, `Control`, leftover status messages, HTTP, event handler, `stop` |
| `registerHint` | [191](../src/serve.ts#L191) | the `register` command line printed after the owner setup |
| `waitForStopMessages` | [195](../src/serve.ts#L195) | the fixed wait at stop for cards edited or posted at the stop and for status deletions |
| `inferenceKey` | [213](../src/serve.ts#L213) | `SIMPLEX_MCP_INFERENCE_KEY`: visible ASCII characters, otherwise `ExitError` with code 2 |
| `setTempFolder` | [219](../src/serve.ts#L219) | `DATA/tmp`, mode 0700, as the core's temp folder |
| `updatedUser` | [225](../src/serve.ts#L225) | display name and peer type `agent` of the profile |
| `controlGroup` | [232](../src/serve.ts#L232) | found, created or reset agent control chat |
| `listen` | [239](../src/serve.ts#L239) | `startHttp`; `EADDRINUSE` to `ExitError` code 1 |
| `bufferEvents` | [248](../src/serve.ts#L248) | events received before the handler is installed are queued and replayed in order |

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
| `ConnectInput`, `SendInput`, `ReceiveInput`, `LeaveInput`, `DownloadInput` | [48-84](../src/tools.ts#L48-L84) | tool inputs |
| `PlanSummary` | [90](../src/tools.ts#L90) | card kind, chat type, name, SimpleX name, peer type, description, welcome, commands, `takesMessage` |
| `ChatFields` | [102](../src/tools.ts#L102) | chat fields of `connect` and `list` results |
| `Agent` | [116](../src/tools.ts#L116) | `ChatApi`, user, `Actions`, `ChatItemsWatch`, control chat reference, standing approver lookup, refused file post, data folder, inference poller |
| `JoinMethod` | [128](../src/tools.ts#L128) | `prepared`, `prepareChannel`, `viaAddress`, `prepareContact`, `connect` |
| schemas | [184-409](../src/tools.ts#L184-L409) | input and output JSON schemas |
| descriptions, `instructions` | [411-451](../src/tools.ts#L411-L451) | tool descriptions and server instructions |
| `definitions` | [461](../src/tools.ts#L461) | name, description, schemas and action flag of the six tools |
| `Download` | [472](../src/tools.ts#L472) | one download: its promise, chat, item, file ID, size, path, the last fileSource seen, received bytes and their time, check timer |
| `AgentTools.leaving`, `AgentTools.statuses` | [495-500](../src/tools.ts#L495-L500) | the chats being left; the `Statuses`, with the eligibility read `showsStatus` after `knownChat` |
| `AgentTools.register` | [502](../src/tools.ts#L502) | `registerTool` for each definition; `_meta` in mode `host` |
| `AgentTools.call` | [514](../src/tools.ts#L514) | the call registered as running in `Statuses` until it ends; handler result to `CallToolResult`; errors to `isError` |
| `AgentTools.perform` | [526](../src/tools.ts#L526) | perform step of an approved action |
| `downloadFile`, `forwardFile`, `downloadTo` | [539-566](../src/tools.ts#L539-L566) | one download per file ID, shared by `download` and `/forward`; a forwarded file moved to the download path for `download` |
| file events | [568-605](../src/tools.ts#L568-L605) | `onNewItems`, `onFile`, `onFileComplete`, `onFileError`, `onFileCancelled`, `onFileDescription` |
| `onContactConnected`, `onUserJoinedGroup` | [607-615](../src/tools.ts#L607-L615) | completion of connect actions by events |
| `actionResult` | [653](../src/tools.ts#L653) | wait for the decision, result fields, removal |
| `showNote`, `statusRefusal`, `showsStatus` | [819-839](../src/tools.ts#L819-L839) | the note of a `send` with `live: true` and its log line; the first refusal of a status message in a chat; eligibility: `postingRefusal` and `statusRefusal` both without a refusal |
| `attachment`, `isRefused`, `fileToSend`, `resizedCopy` | [900-936](../src/tools.ts#L900-L936) | the file of a `send` |
| `copyForSending`, `chatFiles`, `deleteChatFiles` | [938-967](../src/tools.ts#L938-L967) | `/forward` copies; the files of a chat, forwarded ones included; its downloads cancelled, also those the core continues after a restart |
| `leaveChat` | [990](../src/tools.ts#L990) | perform step of `leave` and `/leave`: the chat marked as being left, its status message deleted, then the chat; the chat's forwarded files deleted |
| `download`, `fileServers`, `unknownServers` | [1006-1052](../src/tools.ts#L1006-L1052) | the `download` call before its action |
| `startDownload`, `checkDownload`, `currentFile`, `settleFile`, `settleReceived`, `moveReceived` | [1070-1137](../src/tools.ts#L1070-L1137) | a download's steps |
| `deleteLeftoverStatuses` | [1160](../src/tools.ts#L1160) | at start, the leftover status messages of each eligible chat deleted |
| `agentChats`, `knownChat` | [1215-1232](../src/tools.ts#L1215-L1232) | the agent's chats; a checked chat reference |
| `classifyPlan` | [1315](../src/tools.ts#L1315) | `apiConnectPlan` response to a join plan or a known chat |
| `chatSubject`, `isConnected`, `isActive`, `memberCurrent` | [1411-1473](../src/tools.ts#L1411-L1473) | chat and member predicates shared with `control.ts` |
| `postingRefusal` | [1449](../src/tools.ts#L1449) | a chat that is not connected, or a group where the agent's role is `observer` or `relay`; used by `send` and by `showsStatus` |
| `fitsMessage`, `quoteContent` | [1497-1513](../src/tools.ts#L1497-L1513) | the message as the core sends it, within 15602 bytes; for `send` and the post of `/forward` |
| `realFile` | [1529](../src/tools.ts#L1529) | real path, size, inode and change time of a file |
| `attachmentMedia`, `videoFrame`, `videoDuration` | [1576-1610](../src/tools.ts#L1576-L1610) | previews and durations |
| `toolCall` | [1626](../src/tools.ts#L1626) | `ServerContext` to `ToolCall`: abort signal and progress notifications |

### [2.5 `actions.ts`](../src/actions.ts)

| Symbol | Line | Contents |
|---|---|---|
| `Mode` | [5](../src/actions.ts#L5) | `requests`, `host` |
| `State` | [6](../src/actions.ts#L6) | `pending`, `performing`, `done`, `failed`, `rejected`, `expired` |
| `Standing` | [7](../src/actions.ts#L7) | `messages`, `files` |
| `ToolError` | [19](../src/actions.ts#L19) | errors the model can correct; text returned as is |
| `Subject` | [21](../src/actions.ts#L21) | kind, name, SimpleX name, incognito flag of a card |
| `ConnRef` | [28](../src/actions.ts#L28) | pending connection, contact or group of a connect action |
| `ActionRecord` | [34](../src/actions.ts#L34) | number, key, times, change signal, standing flag, state, card, decider, open calls, interrupted flag, error, card image |
| `ConnectAction`, `SendAction`, `LeaveAction`, `DownloadAction` | [51-110](../src/actions.ts#L51-L110) | per-tool fields; `Attachment`, the file of a send |
| `ActionHooks` | [116](../src/actions.ts#L116) | `refusal`, `saveCounter`, `postCard`, `perform`, `outcome` |
| `Actions` | [124](../src/actions.ts#L124) | action map, expiry timer, posting and outcome promises |
| `create` | [155](../src/actions.ts#L155) | refusal checks, number, log line, counter write, card |
| `decided` | [206](../src/actions.ts#L206) | wait with progress until a decided state, `until`, or the abort signal |
| `stop` | [240](../src/actions.ts#L240) | cancellation of pending and performing actions |
| `summary`, `performedText`, `progressText` | [315-354](../src/actions.ts#L315-L354) | log and progress texts |

Hooks are bound in [`serve.ts:104-110`](../src/serve.ts#L104-L110): `refusal`, `saveCounter`, `postCard` and `outcome` to `Control`; `perform` to `AgentTools`.

### [2.6 `control.ts`](../src/control.ts)

| Symbol | Line | Contents |
|---|---|---|
| `controlCommands` | [10](../src/control.ts#L10) | the command menu |
| `standings` | [22](../src/control.ts#L22) | custom data key and reply words of the approvals of messages and files |
| `ControlData` | [43](../src/control.ts#L43) | `{control: true, nextAction}` |
| `ChatApproval` | [60](../src/control.ts#L60) | `{approvedBy, approvedById, approvedAt}` |
| `OwnerRequest` | [66](../src/control.ts#L66) | parsed owner command |
| `Control.load` | [102](../src/control.ts#L102) | lost check, group profile and menu, member list |
| `Control.start` | [111](../src/control.ts#L111) | owner setup per `--owners` |
| `Control.refusal` | [119](../src/control.ts#L119) | bytes left for the link texts of a connect card, card size of a send, connect or download, approver presence |
| `Control.standingApprover` | [130](../src/control.ts#L130) | approver name of a chat's standing approval of messages or files |
| `Control.saveCounter` | [135](../src/control.ts#L135) | queued counter writes |
| `Control.postCard`, `Control.outcome` | [141-152](../src/control.ts#L141-L152) | card post and card edit; the kept-files message after a done leave |
| `Control.refusedFile`, `Control.fileNotSent` | [154-169](../src/control.ts#L154-L169) | posts about files |
| `Control.onMessage` | [171](../src/control.ts#L171) | owner commands, queued one at a time |
| `Control.onEvent` | [183](../src/control.ts#L183) | membership changes and loss of the control chat |
| `Control.ownerCommand` | [200](../src/control.ts#L200) | command dispatch and replies |
| `approveChat`, `revoke`, `forward`, `leave`, `deleteFiles` | [243-307](../src/control.ts#L243-L307) | owner requests |
| `cardContent`, `fittedImage` | [441-461](../src/control.ts#L441-L461) | image and video cards with the file's preview |
| `findControlGroup`, `createControlChat`, `resetControlChat` | [499-527](../src/control.ts#L499-L527) | control group lookup, creation, reset |
| `isControlEvent` | [529](../src/control.ts#L529) | membership events of the control group |
| `parseCommand` | [545](../src/control.ts#L545) | one-line `/keyword params` |
| `cardText`, `requestLines`, `commandsLine` | [552-616](../src/control.ts#L552-L616) | card text per tool, state and mode |
| `largestCard`, `encodedLength` | [622-630](../src/control.ts#L622-L630) | card size check |
| `serversLine`, `sizeText` | [664-673](../src/control.ts#L664-L673) | servers line and sizes of download cards |
| `codeSpan`, `safeName`, `mentionText` | [683-695](../src/control.ts#L683-L695) | quoting of peer text |
| `ownerRequest` | [710](../src/control.ts#L710) | command words to an `OwnerRequest` |
| `qrCodeText` | [725](../src/control.ts#L725) | terminal QR code |
| `losesControl`, `isLost` | [741-756](../src/control.ts#L741-L756) | loss of the control chat |
| `entryText`, `restartText` | [774-784](../src/control.ts#L774-L784) | `--owners` entries in restart lines |

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
| `filePaths`, `chatFolder` | [215-223](../src/messages.ts#L215-L223) | `DATA/files/group-N`, `DATA/files/contact-N` or the same under `DATA/forwarded`; the download path and the forwarded path of a received file |
| `fileOffer`, `fileContent` | [225-250](../src/messages.ts#L225-L250) | file fields of an item; message content of a file |
| `getChat`, `receivingFiles`, `markRead` | [267-283](../src/messages.ts#L267-L283) | `apiGetChat` and `apiChatItemsRead` by chat reference; the IDs of a chat's files in status `rcvAccepted`, `rcvTransfer` or `rcvWarning`, paged |
| `unreadItems` | [285](../src/messages.ts#L285) | unread items of a chat after its `initial` read, paged |
| `reportProgress`, `nextChange`, `awaitResponses` | [301-331](../src/messages.ts#L301-L331) | waits |
| `validName`, `viewName` | [337-363](../src/messages.ts#L337-L363) | display name rules and `@name` form |

### [2.8 `files.ts`](../src/files.ts)

| Symbol | Line | Contents |
|---|---|---|
| `createDataFolder` | [12](../src/files.ts#L12) | `mkdir -p` mode 0700; `chmod 0700` except on Windows |
| `acquireLock`, `releaseLock` | [17-28](../src/files.ts#L17-L28) | `DATA/lock` with the PID |
| `serverToken` | [30](../src/files.ts#L30) | `DATA/token`: read, or 32 random bytes in base64url |
| `writeHeadersFile` | [42](../src/files.ts#L42) | `DATA/headers.json` |
| `writeServerFile`, `readServerFile` | [46-53](../src/files.ts#L46-L53) | `DATA/server.json` |

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

### [2.11 `status.ts`](../src/status.ts)

Status messages: one per chat, owned by `AgentTools` ([tools.ts:496-500](../src/tools.ts#L496-L500)). Texts, labels and events are listed in [API 1.10](api.md#110-status-message).

| Symbol | Line | Contents |
|---|---|---|
| `Status` | [9](../src/status.ts#L9) | `LiveMessage`, creation time, render count, label, note, tick timer |
| constants | [18-26](../src/status.ts#L18-L26) | frames; 50 renders by ticks, 59 in all; gaps of 3 s, 20 s and 5 min; 30 min without calls; 23 h lifetime; 30 h − 10 min leftover window |
| `Statuses` | [28](../src/status.ts#L28) | statuses by chat, running calls, time of the last call start or end, deletions in progress, failed-poll flag, stop flag |
| `startCall` | [38](../src/status.ts#L38) | one running call registered, and removed by the returned function; the time of the last call set at both |
| `start` | [48](../src/status.ts#L48) | a status for a chat without one, before the stop; its first tick at once |
| `show` | [54](../src/status.ts#L54) | a note: refused after the stop; a status 23 h old replaced; refused at 59 renders; rendered at once; a new status scheduled |
| `end` | [66](../src/status.ts#L66) | the status removed, its timer cleared, `LiveMessage.end("")` kept among the deletions in progress |
| `stop` | [76](../src/status.ts#L76) | every status ended, the deletions awaited; the count of statuses with a render returned |
| `deleteLeftovers` | [84](../src/status.ts#L84) | the agent's own live message items of a chat, younger than 30 h − 10 min, read in pages of 100 and deleted with mode `broadcast` |
| `created`, `isCurrent`, `failed` | [101-114](../src/status.ts#L101-L114) | a `LiveMessage` with interval 0; after its failed command, the error is logged and the status is ended while it is current; the identity check |
| `tick` | [116](../src/status.ts#L116) | eligibility read, end conditions, label, render, next tick |
| `render`, `schedule` | [133-142](../src/status.ts#L133-L142) | one `LiveMessage.update`; the next tick on an unreferenced timer |
| `label`, `inferred` | [144-162](../src/status.ts#L144-L162) | the inference state, the pending actions with open calls and the latest running call other than `receive`; a failed poll logged unless the previous poll failed |
| `statusText`, `statusLabel`, `modelText`, `otherLabel` | [165-192](../src/status.ts#L165-L192) | the line, the note under it, the labels |

States of one chat:

| State | `statuses` entry | Renders | `endings` |
|---|---|---|---|
| none | absent | | |
| active | present | 0 to 49 | |
| frozen | present | 50 to 59 | |
| ending | absent | | its `LiveMessage.end("")` |

```
tick(chat, status):                                           status.ts:116
  shown = allowed(chat): knownChat, then showsStatus           :117, tools.ts:499
    a failure: shown false; logged while the status is current :117-120
  the status ended or replaced meanwhile: return               :121
  idle = renders ≥ 50, no running call other than receive,     :122
         30 min since the last call start or end
  not shown, 23 h since creation, or idle: end(chat); return   :123
  renders 1 to 48: label read                                  :124-128
    the status ended or replaced meanwhile: return
  renders < 50: render                                         :129
  next tick: a tenth of the age, 3 s to 20 s; 5 min from 50     :130, :138-142
```

- Each await in `tick` is followed by the identity check, and the render is called after the check without an await between them ([status.ts:121](../src/status.ts#L121), [:126](../src/status.ts#L126)).
- In `end`, the status is removed from the map before `LiveMessage.end` is called; the library's pending update is cleared there, so the deletion is the last command of the item ([status.ts:66-74](../src/status.ts#L66-L74), [api.ts:1178-1185](../../simplex-chat-nodejs/src/api.ts#L1178-L1185)).
- Commands of one `LiveMessage` are run in one chain; after a failed command, the error is logged, the status is ended, and its deletion is queued after the commands already queued ([api.ts:1187-1189](../../simplex-chat-nodejs/src/api.ts#L1187-L1189), [status.ts:111-114](../src/status.ts#L111-L114)).
- At run time, `actions`, `log`, `messages`, `simplex-chat` and `@simplex-chat/types` are imported by `status.ts`; `tools` and `inference` are imported as types only ([status.ts:1-7](../src/status.ts#L1-L7)).

### [2.12 `inference.ts`](../src/inference.ts)

The poller of `--inference`, created in `startServer` and passed to `AgentTools` in `Agent.inference` ([serve.ts:69](../src/serve.ts#L69), [:112](../src/serve.ts#L112)).

| Symbol | Line | Contents |
|---|---|---|
| `Endpoint` | [3](../src/inference.ts#L3) | URL and key |
| `InferenceState` | [8](../src/inference.ts#L8) | `loading` with a model; `prompt` with a model and the tokens read; `output` with a model and a rate; `idle` |
| `Slot` | [10](../src/inference.ts#L10) | proxy, model, slot ID, task ID, processing flag, tokens decoded, prompt tokens read, time of the answer |
| constants | [23-29](../src/inference.ts#L23-L29) | 1 s deadline, 1 MiB body, 4 proxies, 64 slots, model ID pattern, loopback host names |
| `inferencePoller` | [31](../src/inference.ts#L31) | the poll function; for a call within 3 s of the last poll's start, that poll is returned |
| `sample` | [51](../src/inference.ts#L51) | `/running` of llama-swap, or a plain llama-server at URL; the slots of the allowed proxies |
| `proxySlots` | [77](../src/inference.ts#L77) | `/props`, then `/slots` of one server |
| `slotOf`, `stateOf`, `rateOf` | [87-107](../src/inference.ts#L87-L107) | slot fields; the state of the processing slots; tokens per second |
| `get`, `bodyJson`, `failureText` | [109-141](../src/inference.ts#L109-L141) | one GET with `redirect: "error"`; the body as JSON within 1 MiB; the error text |
| `allowedProxy` | [143](../src/inference.ts#L143) | the origin of a proxy URL that may be polled |
| `modelName`, `fieldOf`, `listOf`, `countOf` | [150-164](../src/inference.ts#L150-L164) | a model ID by the pattern; reads of untyped JSON; counts as non-negative safe integers |

```
poll():                                                        inference.ts:34
  the last poll started less than 3 s ago: its promise          :36
  sample, with one 1 s deadline for all its requests            :37
  a sample of slots: the state from these slots and the         :38-44
    previous ones; these slots become the previous ones
  another sample: the previous slots emptied; the sample is the state
  a failed sample: the previous slots kept; the poll fails
sample():                                                      :51
  GET URL/running with the key                                  :53
    404: proxySlots(URL, with the key, model unknown)           :54
  entries: model by the pattern, state, proxy                   :55
  a model starting: loading, that model                         :56-57
  no model ready: a model stopping ? loading : nothing known    :58-59
  proxies: allowed proxies of ready models, distinct, at most 4 :60-64
    none: nothing known                                         :65
  proxySlots of each proxy without the key, in parallel         :66
    every read fails: the first failure                         :68
    a read gives loading: loading                               :71
    otherwise: the slots of all reads                           :72-74
proxySlots(SERVER, model):                                     :77
  GET SERVER/props; 503: loading, model                         :78-79
  is_sleeping true: an empty slot list                          :80
  GET SERVER/slots; 503: loading, model                         :81-82
  the first 64 slots, with the time of the answer               :83-84
stateOf(slots, previous):                                      :92
  no slot processes: idle                                       :94
  model: the model of the processing slots when they share one  :95
  no processing slot decoded a token: prompt, the sum of tokens read   :96-97
  otherwise: output, rate                                       :98
rateOf(decoding, previous):                                    :101
  per decoding slot, the previous slot of the same proxy, ID and task that
    processed, had decoded tokens, no more than now, at an earlier time:
    tokens decoded since, per second
  the sum, when above 0                                         :106
```

---

## 3. State

### 3.1 Memory

| Item | Holder | Written by | Read by |
|---|---|---|---|
| Actions | `Actions.actions` map ([actions.ts:128](../src/actions.ts#L128)) | `create`, `remove`, `expireDue` | tools, `Control` |
| Open calls of an action | `ActionRecord.open` ([:44](../src/actions.ts#L44)) | `actionResult` ([tools.ts:654](../src/tools.ts#L654), [:663](../src/tools.ts#L663)) | `list`, `expireDue` |
| Collected replies | `ConnectAction.messages`, `SendAction.responses` ([:56](../src/actions.ts#L56), [:87](../src/actions.ts#L87)) | `ChatItemsWatch.collect` | `connect`, `send` results |
| Item listeners | `ChatItemsWatch.listeners` ([messages.ts:87](../src/messages.ts#L87)) | `collect` | event dispatch |
| Downloads | `AgentTools.downloads`, one per file ID ([tools.ts:490](../src/tools.ts#L490)) | `downloadTo` | file events, checks, `/delete CHAT files` |
| Forwards | `AgentTools.forwarding`, the copy for the post per file ID ([tools.ts:491](../src/tools.ts#L491)) | `forwardFile` | `downloadFile`, before it moves a forwarded file |
| Rejected files | `AgentTools.rejectedFiles` ([tools.ts:492](../src/tools.ts#L492)) | `downloadResult` ([:1059-1060](../src/tools.ts#L1059-L1060)) | the file field; `download`, which then skips the files approval ([:1021](../src/tools.ts#L1021)) |
| Count of file descriptions | `AgentTools.describedFiles` ([tools.ts:493-494](../src/tools.ts#L493-L494)) | `onFileDescription` | `fileServers` |
| Chats being left | `AgentTools.leaving` ([tools.ts:495](../src/tools.ts#L495)) | `leaveChat` ([:991](../src/tools.ts#L991), [:1001](../src/tools.ts#L1001)) | `statusRefusal` ([:829](../src/tools.ts#L829)) |
| Status messages | `Statuses.statuses`, one per chat ([status.ts:29](../src/status.ts#L29)) | `start`, `show`, `end` | ticks, `show`, `stop` |
| Running calls | `Statuses.calls` and `Statuses.lastCall` ([status.ts:30-31](../src/status.ts#L30-L31)) | `startCall`, from `AgentTools.call` ([tools.ts:515](../src/tools.ts#L515), [:522](../src/tools.ts#L522)) | labels, the idle check of ticks |
| Deletions in progress | `Statuses.endings` ([status.ts:32](../src/status.ts#L32)) | `end` | `stop` |
| Failed-poll flag | `Statuses.inferenceFailed` ([status.ts:33](../src/status.ts#L33)) | `inferred` | `inferred` |
| Last poll and previous slots | `latest` and `previous` of the poller ([inference.ts:32-33](../src/inference.ts#L32-L33)) | each poll | calls within 3 s; the rate of the next poll |
| Members of the control chat | `Control.members` ([control.ts:82](../src/control.ts#L82)) | `refreshMembers` ([:463-465](../src/control.ts#L463-L465)) | approver checks |
| Mode, expiry, owners list, stop wait, inference URL | `ServeOptions` of the run | CLI | all modules |

### 3.2 Chat database custom data

| Item | Entity | Value | Written by | Read by |
|---|---|---|---|---|
| Control marker and counter | agent control chat | `{simplexMcp: {control: true, nextAction: N}}` | [`createControlChat`](../src/control.ts#L509-L520), [`saveCounter`](../src/control.ts#L135) | [`findControlGroup`](../src/control.ts#L499), [`controlDataOf`](../src/control.ts#L504) |
| Standing approval of messages | contact or group of the agent | `{...other keys, simplexMcp: {approvedBy, approvedById, approvedAt}}` | [`approveChat`](../src/control.ts#L243), [`revoke`](../src/control.ts#L256) | [`standingApprover`](../src/control.ts#L130) |
| Files approval | contact or group of the agent | `{...other keys, simplexMcpFiles: {approvedBy, approvedById, approvedAt}}` | [`approveChat`](../src/control.ts#L243), [`revoke`](../src/control.ts#L256) | [`standingApprover`](../src/control.ts#L130) |

The profile, chats, items, members and file transfers are stored by the core in `DATA/simplex_chat.db`; connections and queues in `DATA/simplex_agent.db` (file prefix `DATA/simplex`, [serve.ts:75](../src/serve.ts#L75)).

### 3.3 Files

| Path | Contents | Mode | Written | Read |
|---|---|---|---|---|
| `DATA/` | data folder | 0700, set at every start except on Windows | [files.ts:12-15](../src/files.ts#L12-L15) | |
| `DATA/token` | base64url of 32 random bytes | 0600, set again at every write except on Windows | first start, `--new-token` ([:30-36](../src/files.ts#L30-L36)) | every start |
| `DATA/headers.json` | `{"Authorization":"Bearer <token>"}` | 0600, set again at every write except on Windows | every start ([:42-44](../src/files.ts#L42-L44)) | Claude Code `headersHelper` |
| `DATA/server.json` | `{name, port, expireHours}` | 0600, set again at every write except on Windows | every start, after the HTTP listener is bound ([serve.ts:123](../src/serve.ts#L123)) | `register` |
| `DATA/lock` | PID and a newline | 0600 | start ([files.ts:64-72](../src/files.ts#L64-L72)) | next start |
| `DATA/tmp/` | the core's temp folder: files in transfer | 0700, set at every start | [serve.ts:97](../src/serve.ts#L97), [:219-223](../src/serve.ts#L219-L223) | the core |
| `DATA/files/group-N/`, `DATA/files/contact-N/` | downloaded files and files received without a download, named by file ID and extension | folder 0700 | the core at the download path ([tools.ts:1075-1076](../src/tools.ts#L1075-L1076)); moves ([:1564-1574](../src/tools.ts#L1564-L1574)) | `download` results, `/forward`, `/leave`, `/delete CHAT files` |
| `DATA/forwarded/group-N/`, `DATA/forwarded/contact-N/` | files `/forward` downloaded, named as above, until a download moves them to `DATA/files/` | folder 0700 | the core at the forwarded path ([tools.ts:549-556](../src/tools.ts#L549-L556), [:1075-1076](../src/tools.ts#L1075-L1076)) | `/forward`; moved by `download` ([:539-547](../src/tools.ts#L539-L547)); deleted by `leave`, `/leave` and `/delete CHAT files` ([:949-967](../src/tools.ts#L949-L967), [:990-1004](../src/tools.ts#L990-L1004)) |
| `DATA/files/sent/` | resized copies of sent images, and copies of the files `/forward` posts, one folder per copy | folder 0700; a resized copy 0600 | [tools.ts:925-942](../src/tools.ts#L925-L942), [:969-973](../src/tools.ts#L969-L973) | the core; deleted at `sndFileCompleteXFTP`, `sndFileError` and at start ([serve.ts:98](../src/serve.ts#L98), [:161-165](../src/serve.ts#L161-L165)) |

The core library is downloaded once into `CACHE/core-7.1.0.20/sqlite`, and the Node.js add-on into `CACHE/nodejs-addon-cce06d8/`, each with a `Downloading ...` line on stderr. CACHE is `$XDG_CACHE_HOME/simplex-chat` or `~/.cache/simplex-chat`; on macOS `~/Library/Caches/simplex-chat`; on Windows `%LOCALAPPDATA%\simplex-chat` ([libs.ts:41-48](../../simplex-chat-nodejs/src/libs.ts#L41-L48), [:89](../../simplex-chat-nodejs/src/libs.ts#L89), [:109](../../simplex-chat-nodejs/src/libs.ts#L109), [:116](../../simplex-chat-nodejs/src/libs.ts#L116), [:129](../../simplex-chat-nodejs/src/libs.ts#L129)).

---

## 4. Process lifecycle

### 4.1 Start

1. With `--inference`, `SIMPLEX_MCP_INFERENCE_KEY` is checked and the poller is created; then the data folder, lock, token and headers file are prepared ([serve.ts:69-74](../src/serve.ts#L69-L74), [:213-217](../src/serve.ts#L213-L217)).
2. libsimplex for SQLite is loaded by `ChatApi.init`, and the databases are migrated with confirmation `yesUp` ([serve.ts:75](../src/serve.ts#L75), [api.ts:117-126](../../simplex-chat-nodejs/src/api.ts#L117-L126)).
3. Three log lines: version and NAME, data folder, mode; with `--inference`, a fourth: `Inference endpoint: URL` ([serve.ts:76-79](../src/serve.ts#L76-L79)).
4. The active user is read, or created with display name `--display-name` or NAME, empty full name and peer type `agent` ([serve.ts:94](../src/serve.ts#L94)).
5. The network config is read and set with `smpProxyMode` `always` and `smpProxyFallback` `allowProtected` ([:95-96](../src/serve.ts#L95-L96)).
6. `DATA/tmp` is set as the core's temp folder, and `DATA/files/sent` is deleted ([:97-98](../src/serve.ts#L97-L98)).
7. Events are buffered, and the chat is started ([:99-100](../src/serve.ts#L99-L100)).
8. A `--display-name` different from the profile, or a peer type other than `agent`, is applied with one `apiUpdateProfile` ([:101](../src/serve.ts#L101), [:225-230](../src/serve.ts#L225-L230)).
9. The agent control chat is found by its marker, created with counter 1, or reset with `--reset-owners` ([:103](../src/serve.ts#L103), [:232-237](../src/serve.ts#L232-L237)).
10. `Actions`, `ChatItemsWatch`, `AgentTools` with the poller and its `Statuses`, and `Control` are created; the counter is taken from the marker ([:104-113](../src/serve.ts#L104-L113), [tools.ts:498-500](../src/tools.ts#L498-L500)).
11. `Control.load`: for a lost control chat, `Actions.lost` is set; otherwise the group name and command menu are updated when they differ, and members are read ([control.ts:102-109](../src/control.ts#L102-L109)).
12. Status messages left by an earlier run are deleted in each eligible chat; after the control chat is lost, every chat is ineligible. A failed chat list is logged as `status messages: ERROR`, and the start continues ([serve.ts:115](../src/serve.ts#L115), [tools.ts:1160-1165](../src/tools.ts#L1160-L1165), [:832](../src/tools.ts#L832), [status.ts:84-99](../src/status.ts#L84-L99)).
13. The HTTP server listens; with the port in use, the process exits with code 1 ([serve.ts:122](../src/serve.ts#L122), [:239-246](../src/serve.ts#L239-L246)). Until step 15 ends, requests to `/mcp` that pass the token check get 503 with `Retry-After: 1` ([http.ts:23](../src/http.ts#L23), [serve.ts:121-122](../src/serve.ts#L121-L122), [:130](../src/serve.ts#L130)).
14. `server.json` is written ([serve.ts:123](../src/serve.ts#L123)).
15. The server runs the owner setup, `Control.start`; after an error, HTTP is closed and the error is rethrown ([serve.ts:124-129](../src/serve.ts#L124-L129)).
16. `Listening on http://127.0.0.1:PORT/mcp` ([:131](../src/serve.ts#L131)).
17. Buffered events are replayed, then live events are dispatched ([:134](../src/serve.ts#L134), [:248-262](../src/serve.ts#L248-L262)).

After an error inside `startAgent` (steps 4-17) the chat is closed; after any error once the lock is taken, the lock is released ([serve.ts:72-89](../src/serve.ts#L72-L89)).

### 4.2 Event dispatch

Each `onAny` receiver is awaited by the library before the next event is read ([api.ts:195-202](../../simplex-chat-nodejs/src/api.ts#L195-L202)). The dispatcher in [`serve.ts:134-170`](../src/serve.ts#L134-L170):

| Event | Condition | Handler |
|---|---|---|
| `newChatItems` | item of the control group, main scope | [`Control.onMessage`](../src/control.ts#L171) |
| `newChatItems` | every item | [`AgentTools.onNewItems`](../src/tools.ts#L568), then [`ChatItemsWatch.deliver`](../src/messages.ts#L116) |
| `chatItemUpdated` | | [`AgentTools.onFile`](../src/tools.ts#L578), then [`ChatItemsWatch.update`](../src/messages.ts#L120) |
| `chatItemsDeleted` | | [`ChatItemsWatch.remove`](../src/messages.ts#L124) of each `deletedChatItem` |
| `contactConnected` | | [`AgentTools.onContactConnected`](../src/tools.ts#L607) |
| `userJoinedGroup` | | [`AgentTools.onUserJoinedGroup`](../src/tools.ts#L612) |
| `rcvFileComplete` | | [`AgentTools.onFileComplete`](../src/tools.ts#L583) |
| `rcvFileSndCancelled` | | [`AgentTools.onFileCancelled`](../src/tools.ts#L595) |
| `rcvFileError` | | [`AgentTools.onFileError`](../src/tools.ts#L590) |
| `rcvFileDescrReady` | | [`AgentTools.onFileDescription`](../src/tools.ts#L602) |
| `sndFileCompleteXFTP` | | [`AgentTools.onSentFile`](../src/tools.ts#L888) |
| `sndFileError` | | [`AgentTools.onSentFile`](../src/tools.ts#L888), then [`Control.fileNotSent`](../src/control.ts#L160) |
| `joinedGroupMember`<br>`joinedGroupMemberConnecting`<br>`connectedToGroupMember`<br>`memberRole`<br>`leftMember`<br>`deletedMember`<br>`deletedMemberUser`<br>`groupDeleted` | event of the control group | [`Control.onEvent`](../src/control.ts#L183), queued one at a time |
| any event | after `stop()` started | ignored |

Owner commands are queued in a second chain, independent of the membership chain ([control.ts:177-180](../src/control.ts#L177-L180)).

### 4.3 Stop

On `SIGINT` or `SIGTERM` the server runs one stop; a later signal ends its wait for cancelled cards and status deletions ([serve.ts:49-65](../src/serve.ts#L49-L65)). `RunningServer.stop` ([serve.ts:176-187](../src/serve.ts#L176-L187)):

1. Events are ignored from now on.
2. `Actions.stop` starts: new actions are refused with `The server is stopping.`; the expiry timer is cleared; cards being posted are awaited; pending and performing actions are marked interrupted; the stop signal of the performs is aborted, which ends the wait of a `connect` for its connection; their cards are edited, or posted when they have no card; outcome edits already in progress are awaited ([actions.ts:240-250](../src/actions.ts#L240-L250)).
3. In parallel with step 2, the HTTP server is closed with all connections, and open calls are aborted ([http.ts:48-52](../src/http.ts#L48-L52)).
4. `Cancelled K action(s).` when K > 0 ([serve.ts:181](../src/serve.ts#L181)).
5. `Statuses.stop`: later starts are ignored and notes refused; each status message is ended, and the deletion commands are awaited; the status messages with a render are counted ([serve.ts:182](../src/serve.ts#L182), [status.ts:76-82](../src/status.ts#L76-L82)).
6. When cards were edited or posted at the stop and the control chat is not lost, or status messages had a render, the server waits `--stop-wait` seconds, 10 by default, for them to be sent; a second signal ends the wait ([serve.ts:183](../src/serve.ts#L183), [:195-211](../src/serve.ts#L195-L211)).
7. `chat.close()`: the chat is stopped, then the store is closed ([api.ts:165-170](../../simplex-chat-nodejs/src/api.ts#L165-L170)).
8. The lock is removed, `Stopped.` is logged, and the process exits with code 0, or 1 after an error ([serve.ts:185-186](../src/serve.ts#L185-L186), [:55-61](../src/serve.ts#L55-L61)).

Performs in progress are not awaited; when one ends, its card stays as edited at the stop and no line is logged, because the action is interrupted ([actions.ts:280](../src/actions.ts#L280)).

### 4.4 The lock

```
acquireLock(DATA):                                   files.ts:17
  create DATA/lock exclusively (flag wx), PID       :64-72
  exists:
    PID = number in the file                        :74-77
    PID valid, other than ours, running:            :21, :79-86
      exit 1 "Another server uses DATA (PID N). If no server runs, remove DATA/lock."
    remove the file; create it exclusively again    :22-23
      exists again: exit 1 with the same message
releaseLock(DATA): remove DATA/lock                  :26-28
```

`isRunning`: `true` when `kill(pid, 0)` succeeds or fails with `EPERM` ([files.ts:79-86](../src/files.ts#L79-L86)).

---

## 5. Approval model

### 5.1 Modes

The mode is set by `--approve` for one run ([cli.ts:80-82](../src/cli.ts#L80-L82)).

| Mode | State of a new action | Card | `_meta` of `connect`, `send`, `download`, `leave` |
|---|---|---|---|
| `requests` | `pending`<br>`performing` for a send under a standing approval, and for a download under a files approval | card with commands<br>outcome as a new message after a standing send; none after a download without a card | none |
| `host` | `performing` | outcome as a new message after the perform; none for a download | `{"anthropic/requiresUserInteraction": true}` |

Sources: [actions.ts:166](../src/actions.ts#L166), [tools.ts:508](../src/tools.ts#L508), [control.ts:145-152](../src/control.ts#L145-L152), [:495-497](../src/control.ts#L495-L497). In both modes a new action is refused unless the control chat has an approver ([control.ts:124-127](../src/control.ts#L124-L127)).

### 5.2 Owners and approvers

| Term | Rule | Code |
|---|---|---|
| human | member other than the agent's membership, status `connected` or `complete` | [control.ts:484-486](../src/control.ts#L484-L486), [tools.ts:1458-1460](../src/tools.ts#L1458-L1460) |
| human owner | human with role `owner` | [control.ts:471-473](../src/control.ts#L471-L473) |
| approver, `--owners` absent | member with role `owner` | [control.ts:488-492](../src/control.ts#L488-L492) |
| approver, `--owners` without a list | none | [control.ts:491](../src/control.ts#L491) |
| approver, `--owners ID:NAME,...` | member with role `owner` whose `groupMemberId` and `displayName`, folded to one line by `oneLine`, match an entry | [control.ts:491](../src/control.ts#L491), [:758-760](../src/control.ts#L758-L760) |

The command sender's member record from the chat item is checked ([control.ts:176](../src/control.ts#L176), [:204](../src/control.ts#L204)). The approver count for refusals and standing approvals is taken from `Control.members`, refreshed at load and at each membership event ([control.ts:187](../src/control.ts#L187), [:463-465](../src/control.ts#L463-L465)).

Owner setup at start ([control.ts:111-117](../src/control.ts#L111-L117)):

| `--owners` | Start | First join by the link |
|---|---|---|
| absent | a human owner exists: register hint<br>otherwise: `No owners in the agent control chat.` when humans exist, then a link with role `member` | link deleted, role `owner`, greeting, log line, register hint |
| without a list | no humans: link with role `observer`<br>humans exist: their IDs and the restart command, in the log and the chat | link deleted, the joined member and the restart command announced |
| `ID:NAME,...` | each entry matched to a human, or exit 2<br>role `owner` set where missing<br>other humans removed<br>link deleted<br>`Owners: ...` and register hint | |

Sources: [`startOwners`](../src/control.ts#L341), [`startObserver`](../src/control.ts#L347), [`startListed`](../src/control.ts#L352), [`ownerJoined`](../src/control.ts#L371), [`observerJoined`](../src/control.ts#L380).

Membership changes at run time ([control.ts:183-198](../src/control.ts#L183-L198)):

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
| call creates the action, mode `requests` | | `pending` | [actions.ts:166](../src/actions.ts#L166) |
| call creates the action, mode `host` | | `performing` | [actions.ts:166](../src/actions.ts#L166) |
| `send` call under a standing approval, or `download` call under a files approval | | `performing` | [actions.ts:166](../src/actions.ts#L166) |
| counter write or card post fails | `pending`<br>`performing` | `failed` | [actions.ts:252-263](../src/actions.ts#L252-L263) |
| stop arrives while the counter of a `performing` action is written | `performing` | `pending`, interrupted | [actions.ts:265-270](../src/actions.ts#L265-L270) |
| control chat lost | `pending` | `failed` | [actions.ts:232-238](../src/actions.ts#L232-L238) |
| `/reject`, or `/leave CHAT` for a pending action of CHAT with a card | `pending` | `rejected` | [actions.ts:225-230](../src/actions.ts#L225-L230), [control.ts:291-293](../src/control.ts#L291-L293) |
| expiry time passes | `pending` | `expired` | [actions.ts:196-199](../src/actions.ts#L196-L199) |
| `/approve N`, `/approve CHAT N` and `/approve CHAT all` for other actions; `/receive N`, `/receive CHAT N` and `/receive CHAT N all` for downloads | `pending` | `performing` | [actions.ts:219-223](../src/actions.ts#L219-L223), [control.ts:229-238](../src/control.ts#L229-L238) |
| perform throws | `performing` | `failed` | [actions.ts:276-279](../src/actions.ts#L276-L279) |
| perform ends | `performing` | `done` | [actions.ts:274-275](../src/actions.ts#L274-L275) |

`create` ([actions.ts:155-186](../src/actions.ts#L155-L186)):

```
create(key, fields, standingApprover?):
  state = mode host or standing ? performing : pending
  refusal, first match:
    stopping                      "The server is stopping."
    control chat lost             lostMessage
    20 actions pending            pendingMessage
    20 actions performing         performingMessage
    connect: link texts cut to the bytes its largest card leaves
    send, connect or download card over 15000 bytes    cardTooLong
    no approver                   no-owner message
  refusal: throw ToolError        counter unchanged
  N = next; next += 1; store; log "Action N: ..."
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

An action is removed from memory when its result is returned to a call ([tools.ts:660](../src/tools.ts#L660)), or at an expiry tick when it is decided, its open call count is 0 and its expiry time has passed ([actions.ts:200-201](../src/actions.ts#L200-L201)). A `download` still performing at the end of `wait` is returned as `performing` and kept ([tools.ts:657](../src/tools.ts#L657)).

### 5.4 Cards

The server posts a card when a `pending` action is created, and edits it with `apiUpdateChatItem` at the outcome ([actions.ts:255](../src/actions.ts#L255), [control.ts:145-152](../src/control.ts#L145-L152)). For a `connect`, `send` or `leave` without a card (mode `host`, standing sends) the server posts the outcome as a new message; a `download` without a card gets none ([control.ts:495-497](../src/control.ts#L495-L497)). The text per state is built by [`cardText`](../src/control.ts#L552-L571); a card of an image or video file is an image or video message with the file's preview ([control.ts:441-461](../src/control.ts#L441-L461)); forms are listed in [API: cards](api.md#22-cards).

Outcomes are posted after `done` and `failed` ([actions.ts:282](../src/actions.ts#L282)), after `rejected` and `expired` ([:229](../src/actions.ts#L229), [:199](../src/actions.ts#L199)), and at stop ([:247](../src/actions.ts#L247)). They are skipped after the control chat is lost ([:247](../src/actions.ts#L247), [:282](../src/actions.ts#L282), [:291](../src/actions.ts#L291)).

### 5.5 Standing approvals

| Aspect | Messages | Files | Code |
|---|---|---|---|
| grant | `/approve CHAT all` from an approver in mode `requests`; pending sends of CHAT with a card are approved | `/receive CHAT N all` from an approver, on the card of download N; N is performed, and the other pending downloads of CHAT stay pending | [control.ts:237](../src/control.ts#L237), [:243-254](../src/control.ts#L243-L254) |
| mode `host` | nothing stored; reply `Approvals apply in mode requests; this server runs in mode host.` | downloads get no card, so the command has none to act on | [control.ts:213](../src/control.ts#L213) |
| removal | `/revoke CHAT`: the `simplexMcp` key is removed, other keys kept | `/receive CHAT off`: the `simplexMcpFiles` key is removed | [control.ts:256-263](../src/control.ts#L256-L263) |
| use | mode `requests`, and `approvedById` equal to the `groupMemberId` of a current approver | the same | [control.ts:130-133](../src/control.ts#L130-L133) |
| scope | `send`; status messages and notes in the chat ([API 1.10](api.md#110-status-message)), so after `/revoke` the chat's status message is deleted at its next tick | `download` of a file whose servers are all known | [tools.ts:813](../src/tools.ts#L813), [:833](../src/tools.ts#L833), [:1021](../src/tools.ts#L1021), [status.ts:123](../src/status.ts#L123) |
| decider shown | `safeName(approvedBy)` | the same | [control.ts:132](../src/control.ts#L132) |
| storage | the chat's custom data; removed with the chat by `leave` | the same | [tools.ts:990-1004](../src/tools.ts#L990-L1004) |

### 5.6 Expiry

| Rule | Code |
|---|---|
| `expires = created + expireHours × 3600000`, `expireHours` 1 to 23, default 23 | [actions.ts:163](../src/actions.ts#L163), [cli.ts:63](../src/cli.ts#L63), [:86-88](../src/cli.ts#L86-L88) |
| check every 60 s on an unreferenced timer, and before each owner command | [actions.ts:129](../src/actions.ts#L129), [control.ts:201](../src/control.ts#L201) |
| pending and due: `expired`, log `Action N expired.`, card edited to `Expired.` | [actions.ts:196-199](../src/actions.ts#L196-L199) |
| decided, no open call, due: removed from memory | [actions.ts:200-201](../src/actions.ts#L200-L201) |
| connect wait for the other side: at most until `created + 24 h - 10 min` | [tools.ts:148](../src/tools.ts#L148), [:706](../src/tools.ts#L706) |

`Date.now()` is compared with `expires`, so due actions are expired at the first tick after a sleep of the computer.

### 5.7 Action counter

| Rule | Code |
|---|---|
| initial value: `nextAction` of the control marker, or 1 | [serve.ts:104](../src/serve.ts#L104) |
| a new action takes `next`, then `next` grows by 1 | [actions.ts:156](../src/actions.ts#L156), [:178](../src/actions.ts#L178) |
| the new `next` is written before the card is posted | [actions.ts:254-255](../src/actions.ts#L254-L255) |
| one write at a time, also after a failed write | [control.ts:135-139](../src/control.ts#L135-L139) |
| `--reset-owners` copies the value into the new control chat | [serve.ts:235-236](../src/serve.ts#L235-L236), [control.ts:522-527](../src/control.ts#L522-L527) |
| a refused call leaves the counter unchanged | [actions.ts:177](../src/actions.ts#L177) |

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

- The token is created once and kept in `DATA/token`; `--new-token` replaces it ([files.ts:30-36](../src/files.ts#L30-L36)).
- `DATA/headers.json` is rewritten at every start ([files.ts:42-44](../src/files.ts#L42-L44)).
- The Claude Code entry gets the command `cat <DATA>/headers.json`, or `type "<DATA>\headers.json"` on Windows, with the path quoted for the shell ([register.ts:50](../src/register.ts#L50), [log.ts:24-31](../src/log.ts#L24-L31)). Only this command is written to the entry and to the `claude mcp add-json` arguments ([register.test.ts:50-61](../test/unit/register.test.ts#L50-L61)).
- `token`, `headers.json` and `server.json` are set to mode 0600 at every write, and the data folder to 0700 at every start, except on Windows ([files.ts:12-15](../src/files.ts#L12-L15), [:55-58](../src/files.ts#L55-L58)).

### 6.3 Peer-written text

| Destination | Rule | Code |
|---|---|---|
| card and reply names | folded to one line; a valid display name is plain, others a code span | [`safeName`](../src/control.ts#L687-L690) |
| card text lines | one code span per line; backticks become `'`; a leading or trailing space becomes U+00A0; blank lines empty | [`codeLines`, `codeSpan`](../src/control.ts#L679-L685) |
| file names and paths on cards and posts | folded to one line, one code span | [control.ts:585](../src/control.ts#L585), [:598](../src/control.ts#L598), [:166-168](../src/control.ts#L166-L168) |
| reply quote on a send card | whitespace runs to one space, first 100 characters (code points), one code span | [control.ts:586](../src/control.ts#L586) |
| link texts on a connect card | code span lines in the bytes that the largest card form leaves; the line that does not fit is cut and ends in `…` inside its code span; later lines left out | [control.ts:644-658](../src/control.ts#L644-L658) |
| mention names | valid name: `@name` or `@'name'`; others a code span | [`mentionText`](../src/control.ts#L692-L695) |
| errors on cards | folded to one line, one code span | [control.ts:565](../src/control.ts#L565) |
| `--owners` part of restart lines in the chat | one code span | [control.ts:79](../src/control.ts#L79), [:782-784](../src/control.ts#L782-L784) |
| terminal names and errors of actions | folded to one line | [actions.ts:180-181](../src/actions.ts#L180-L181), [:333-341](../src/actions.ts#L333-L341), [control.ts:78](../src/control.ts#L78) |
| tool results | returned as data; schema descriptions and instructions mark `text`, `from`, `name`, file names, downloaded files, `description`, `welcome` and `commands` with their labels and params as data, not instructions; the instructions take a chat's messages as prompts only when the user said so in the session, and limit what the agent shares about its user | [tools.ts:190](../src/tools.ts#L190), [:194](../src/tools.ts#L194), [:199](../src/tools.ts#L199), [:224-225](../src/tools.ts#L224-L225), [:235-240](../src/tools.ts#L235-L240), [:405](../src/tools.ts#L405), [:449](../src/tools.ts#L449) |

The server writes the tappable form `/'...'` only in the card's command line ([control.ts:603-616](../src/control.ts#L603-L616)), in `/'revoke CHAT'` of standing cards and replies ([:563](../src/control.ts#L563), [:253](../src/control.ts#L253)), and in `/'delete CHAT files'` of the kept-files message ([:301](../src/control.ts#L301)).

### 6.4 Chat references

| Rule | Code |
|---|---|
| `^[@#][1-9]\d{0,14}$`: 1 to 15 digits; the core reads `#(2^64 + N)` as `#N` | [messages.ts:150-152](../src/messages.ts#L150-L152), [scenarios.test.ts:502-514](../test/e2e/scenarios.test.ts#L502-L514) |
| the control chat is refused by `send`, `receive`, `download`, `leave` and the owner commands with CHAT, and left out of `list` and `/list` | [tools.ts:1169](../src/tools.ts#L1169), [:1215-1232](../src/tools.ts#L1215-L1232), [control.ts:328-335](../src/control.ts#L328-L335) |
| a store error of the chat read is reported as `No chat CHAT.` | [tools.ts:1174](../src/tools.ts#L1174), [:1230](../src/tools.ts#L1230) |
| the agent's own links are refused by `connect` (`ownLink` plans) | [tools.ts:1334](../src/tools.ts#L1334), [:1352](../src/tools.ts#L1352), [:1374](../src/tools.ts#L1374) |

### 6.5 Owner commands

| Rule | Code |
|---|---|
| source: `groupRcv` text items in the main scope of the control group | [serve.ts:139](../src/serve.ts#L139), [control.ts:173](../src/control.ts#L173) |
| form: one line `/keyword params`, leading spaces allowed | [control.ts:545-550](../src/control.ts#L545-L550) |
| the help reply is sent to any sender; other commands are accepted from approvers only | [control.ts:203-204](../src/control.ts#L203-L204) |
| commands run one at a time in arrival order | [control.ts:177-180](../src/control.ts#L177-L180) |
| `/approve`, `/reject`, `/receive` and `/forward` need a pending action with a card, and the action's chat when CHAT is given; `/receive` and `/forward` take downloads only, `/approve` the other actions only | [control.ts:227-237](../src/control.ts#L227-L237) |

### 6.6 Files

| Rule | Code |
|---|---|
| `send` refuses files named `id_*`, `*.pem`, `*.key`, `*.p12` or `*.pfx`, and files under `~/.ssh`, `~/.gnupg`, the data folder and `~/.simplex-mcp` except the agent's own `DATA/files/`, each by its real path; the refusal is posted to the control chat and logged | [tools.ts:902-916](../src/tools.ts#L902-L916), [:1529-1548](../src/tools.ts#L1529-L1548), [control.ts:154-158](../src/control.ts#L154-L158) |
| the file of a `send` is sent only when its real path, size, inode and change time at the perform are those of the request | [tools.ts:918-923](../src/tools.ts#L918-L923) |
| a sent JPEG or PNG is a resized copy made by the core | [tools.ts:925-936](../src/tools.ts#L925-L936) |
| `ffmpeg` and `ffprobe` read a video as MP4 or QuickTime, local files only, without standard input, 30 s and 64 MiB of output each | [tools.ts:1598-1616](../src/tools.ts#L1598-L1616) |
| a download is saved only at `DATA/files/group-N/` or `DATA/files/contact-N/`, or for `/forward` at the same name under `DATA/forwarded/`, named by its file ID and an extension of 1 to 16 ASCII letters and digits | [messages.ts:215-223](../src/messages.ts#L215-L223) |
| a forwarded file is outside `DATA/files/`, so `send` refuses it; a download moves it into `DATA/files/` after the copy for the post is made | [tools.ts:539-556](../src/tools.ts#L539-L556), [:911-916](../src/tools.ts#L911-L916) |
| a file on unknown servers gets a card that names them, also under a files approval; relays are approved only for such a card, and for every other download, and in mode `host`, the core checks the complete description and downloads a file on unknown servers only when the IP address is protected | [tools.ts:1021](../src/tools.ts#L1021), [:562](../src/tools.ts#L562), [:1076](../src/tools.ts#L1076), [control.ts:611-614](../src/control.ts#L611-L614) |
| an accepted file that the core returns to `rcvInvitation` fails its download at the next check | [tools.ts:1089](../src/tools.ts#L1089) |
| a received file is moved into the chat's folder at its completion, unless it is at the forwarded path, also from a member chat of a group, which has a download in memory as the main conversation does; a file without a download in memory, such as one whose transfer started before a restart, only when it has its full size; a partial file is deleted only for a download in memory, when it is no larger than the file | [tools.ts:1128-1137](../src/tools.ts#L1128-L1137), [:1550-1556](../src/tools.ts#L1550-L1556) |

### 6.7 Link and network

- The join link is a group link of the control chat with role `member`, or `observer` with `--owners`; it is printed with a QR code on stdout and deleted at the first join event ([control.ts:406-416](../src/control.ts#L406-L416), [:372](../src/control.ts#L372), [:381](../src/control.ts#L381)).
- At every start the server sets private message routing: `smpProxyMode` `always`, `smpProxyFallback` `allowProtected` ([serve.ts:95-96](../src/serve.ts#L95-L96)).
- With `incognito: true` the join methods connect incognito ([tools.ts:738](../src/tools.ts#L738)); the core then uses a random profile for that chat ([PLAN, Participants](../PLAN.md#participants)).

### 6.8 Status message and inference endpoint

| Rule | Code |
|---|---|
| status messages and notes are sent only in eligible chats: the agent can post there; a chat with a person, a group or a business chat; mode `requests`; the control chat not lost; a standing approval of messages by a current approver ([API 1.10](api.md#110-status-message)) | [tools.ts:828-839](../src/tools.ts#L828-L839), [:1449-1456](../src/tools.ts#L1449-L1456) |
| eligibility is read at each tick, so after `/revoke`, a lost approver, a lost control chat, a lost connection or an `observer` or `relay` role the status message is deleted within one gap: at most 20 s, or 5 min from 50 renders | [status.ts:116-123](../src/status.ts#L116-L123), [:138-142](../src/status.ts#L138-L142) |
| in a group, the status message, its labels and its notes are sent to the group; a status is started for any message returned by `receive` with `typing: true`, whatever its sender | [tools.ts:1198](../src/tools.ts#L1198) |
| labels are the same for every chat: running calls, waits for approval and the inference state of other chats are shown in each status message | [status.ts:144-149](../src/status.ts#L144-L149) |
| a note is sent without an action, so without a card or an outcome in the control chat; it is text only, at most 2000 characters, and each note is printed in the terminal | [tools.ts:798-799](../src/tools.ts#L798-L799), [:819-826](../src/tools.ts#L819-L826) |
| a status message gets at most 59 renders and one deletion | [status.ts:19-20](../src/status.ts#L19-L20), [:59](../src/status.ts#L59), [:129](../src/status.ts#L129) |
| model IDs are kept only when they match `^[\w.:/+@-]{1,64}$`, and they are shown in a code span; counts are kept only as non-negative safe integers | [inference.ts:27](../src/inference.ts#L27), [:150-152](../src/inference.ts#L150-L152), [:162-164](../src/inference.ts#L162-L164), [status.ts:174](../src/status.ts#L174), [:187](../src/status.ts#L187) |
| poll error texts are folded to one line; the parser's text is replaced by `invalid JSON` | [inference.ts:116](../src/inference.ts#L116), [:129-133](../src/inference.ts#L129-L133) |
| URL is `http` or `https` without user name and password; the key is read from the environment, accepted with visible ASCII characters only, and sent to URL only | [cli.ts:184-188](../src/cli.ts#L184-L188), [serve.ts:213-217](../src/serve.ts#L213-L217), [inference.ts:111](../src/inference.ts#L111) |
| a proxy of `/running` is polled at its origin, without the key, only when its host name equals URL's, or when both are `localhost`, `127.0.0.1` or `[::1]`; at most 4 distinct proxies per poll | [inference.ts:60-66](../src/inference.ts#L60-L66), [:143-148](../src/inference.ts#L143-L148) |
| requests are GET of `/running`, `/props` and `/slots` only; redirects are refused; one 1 s deadline is set for a whole poll; a body over 1 MiB, by `Content-Length` or by the bytes read, is refused; at most 64 slots of a server are read | [inference.ts:37](../src/inference.ts#L37), [:53](../src/inference.ts#L53), [:78-84](../src/inference.ts#L78-L84), [:109-134](../src/inference.ts#L109-L134) |
| `/slots` is skipped when `is_sleeping` of `/props` is true | [inference.ts:80](../src/inference.ts#L80) |
| polls are made only for renders 2 to 49, at most one per 3 s for all status messages | [status.ts:124-128](../src/status.ts#L124-L128), [inference.ts:36](../src/inference.ts#L36) |

---

## 7. Limits

| Limit | Value | Code |
|---|---|---|
| pending actions | 20 | [actions.ts:11](../src/actions.ts#L11) |
| actions being performed | 20 | [actions.ts:12](../src/actions.ts#L12) |
| expiry | 1 to 23 h, default 23 | [cli.ts:63](../src/cli.ts#L63), [:86-88](../src/cli.ts#L86-L88) |
| connect wait for the other side | `created + 23 h 50 min` | [tools.ts:148](../src/tools.ts#L148) |
| send, connect and download card | 15000 bytes JSON-escaped, in its largest form | [control.ts:26](../src/control.ts#L26), [:622-630](../src/control.ts#L622-L630) |
| error assumed in the largest card | 600 × `€` | [control.ts:30](../src/control.ts#L30) |
| message as the core sends it | 15602 bytes, counted with 600 bytes of envelope; `send` refuses a longer one, and the post of `/forward` leaves out its quote of the card | [tools.ts:158-159](../src/tools.ts#L158-L159), [:1497-1500](../src/tools.ts#L1497-L1500), [control.ts:265-281](../src/control.ts#L265-L281) |
| `send.text` | 1 to 12000 characters | [tools.ts:262](../src/tools.ts#L262) |
| `send.text` with `live` | at most 2000 characters (code points) | [tools.ts:160](../src/tools.ts#L160), [:799](../src/tools.ts#L799) |
| `connect.message` | 1 to 2000 characters | [tools.ts:250](../src/tools.ts#L250) |
| `send.mention` | 1 to 3 unique member IDs | [tools.ts:266](../src/tools.ts#L266) |
| `wait` | 0 to 86400 s | [tools.ts:251](../src/tools.ts#L251), [:267](../src/tools.ts#L267), [:280](../src/tools.ts#L280), [:304](../src/tools.ts#L304) |
| `idle` | 1 to 300 s, default 30 | [tools.ts:252](../src/tools.ts#L252), [:268](../src/tools.ts#L268), [:154](../src/tools.ts#L154) |
| `limit` | 1 to 100, default 50 | [tools.ts:269](../src/tools.ts#L269), [:281](../src/tools.ts#L281), [:155](../src/tools.ts#L155) |
| `connect` snapshot | last 50 items | [tools.ts:156](../src/tools.ts#L156) |
| collected items per action or call | 100, then their IDs kept as overflow until deleted | [messages.ts:10](../src/messages.ts#L10), [:109-110](../src/messages.ts#L109-L110) |
| unread page, leftover status page | 100 items | [messages.ts:9](../src/messages.ts#L9), [status.ts:89-93](../src/status.ts#L89-L93) |
| chat list read | one read of 1000000 chats | [tools.ts:147](../src/tools.ts#L147) |
| `download` wait, default | 60 s | [tools.ts:151](../src/tools.ts#L151) |
| download check | every 60 s | [tools.ts:152](../src/tools.ts#L152) |
| download without progress | 30 min, then cancelled | [tools.ts:153](../src/tools.ts#L153) |
| `ffmpeg`, `ffprobe` | 30 s, 64 MiB of output | [tools.ts:157](../src/tools.ts#L157), [:162](../src/tools.ts#L162) |
| stop wait | 0 to 60 s, default 10 | [cli.ts:68](../src/cli.ts#L68), [:89-91](../src/cli.ts#L89-L91) |
| error details | 500 characters | [log.ts:4](../src/log.ts#L4) |
| progress interval | 60 s | [messages.ts:7](../src/messages.ts#L7) |
| expiry tick | 60 s | [actions.ts:10](../src/actions.ts#L10) |
| HTTP body | 4 MiB | [server index.cjs:254](../node_modules/@modelcontextprotocol/server/dist/index.cjs#L254) |
| SSE keep-alive | 15 s | [mcp-BumlNux0.cjs:64](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L64) |
| agent NAME | 1 to 32 characters | [cli.ts:160](../src/cli.ts#L160) |
| display name | 50 characters, `mkValidName` rules | [messages.ts:11-12](../src/messages.ts#L11-L12), [:337-359](../src/messages.ts#L337-L359) |
| port | 1 to 65535, default 5226 | [cli.ts:66](../src/cli.ts#L66), [:98-100](../src/cli.ts#L98-L100) |
| status renders by ticks | 50; the 50th shows the frozen line | [status.ts:19](../src/status.ts#L19), [:167](../src/status.ts#L167) |
| status renders in all, with notes | 59 | [status.ts:20](../src/status.ts#L20), [:59](../src/status.ts#L59) |
| status tick gap | a tenth of the age, 3 s to 20 s; 5 min from 50 renders | [status.ts:21-23](../src/status.ts#L21-L23), [:140](../src/status.ts#L140) |
| status without calls | 30 min, from 50 renders, then deleted | [status.ts:24](../src/status.ts#L24), [:122](../src/status.ts#L122) |
| status lifetime | 23 h | [status.ts:25](../src/status.ts#L25) |
| leftover status age at start | below 30 h − 10 min | [status.ts:26](../src/status.ts#L26), [:86](../src/status.ts#L86) |
| inference poll deadline | 1 s for all requests of a poll | [inference.ts:23](../src/inference.ts#L23), [:37](../src/inference.ts#L37) |
| inference poll reuse | 3 s | [inference.ts:31](../src/inference.ts#L31), [:36](../src/inference.ts#L36) |
| inference answer body | 1 MiB | [inference.ts:24](../src/inference.ts#L24) |
| inference proxies | 4 per poll | [inference.ts:25](../src/inference.ts#L25) |
| inference slots | 64 per server | [inference.ts:26](../src/inference.ts#L26), [:84](../src/inference.ts#L84) |
| model ID | 1 to 64 characters of `[\w.:/+@-]` | [inference.ts:27](../src/inference.ts#L27) |
