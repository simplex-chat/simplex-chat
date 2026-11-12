# SimpleX MCP server: architecture

> System structure, modules, state, process lifecycle, approval model and security boundaries of `@simplex-chat/mcp` 0.2.0.
>
> Related specs: [README](README.md) | [Flows](flows.md) | [API](api.md)
> Related docs: [README](../README.md) | [Reference](../REFERENCE.md) | [PLAN](../PLAN.md)

**Source:** [`cli.ts`](../src/cli.ts) | [`serve.ts`](../src/serve.ts) | [`http.ts`](../src/http.ts) | [`tools.ts`](../src/tools.ts) | [`actions.ts`](../src/actions.ts) | [`control.ts`](../src/control.ts) | [`messages.ts`](../src/messages.ts) | [`files.ts`](../src/files.ts) | [`register.ts`](../src/register.ts) | [`log.ts`](../src/log.ts)

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
  control --> files
  files --> data[("DATA:<br/>token<br/>headers.json<br/>server.json<br/>lock")]
  files --> modelfile[("FILE of --model-file:<br/>PROVIDER/MODEL")]
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
3. An `McpServer` is built from the factory for each request, and the arguments are validated against the input schema by the SDK ([`serve.ts:114-118`](../src/serve.ts#L114-L118), [`mcp-BumlNux0.cjs:1812-1817`](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L1812-L1817)).
4. The handler result is returned by `AgentTools.call` as `structuredContent` and as JSON text ([`tools.ts:500-507`](../src/tools.ts#L500-L507)).
5. The server calls `ChatApi` methods of the core ([`messages.ts:267-283`](../src/messages.ts#L267-L283)); actions are created in `Actions` and shown to the owner by `Control` ([`actions.ts:155-186`](../src/actions.ts#L155-L186)).
6. The core emits events; the server dispatches them in one handler of `serve.ts` to the tools, the item watch and the agent control chat ([`serve.ts:132-168`](../src/serve.ts#L132-L168)).

---

## 2. Modules

### [2.1 `cli.ts`](../src/cli.ts)

Entry point of the `simplex-mcp` binary.

| Symbol | Line | Contents |
|---|---|---|
| `helpText` | [11](../src/cli.ts#L11) | `--help` output |
| `Command` | [47](../src/cli.ts#L47) | `help`, `serve` with `ServeOptions`, `register` with `RegisterOptions` |
| `parseCommand` | [52](../src/cli.ts#L52) | command, NAME and options to a `Command` |
| `serveOptions` | [61](../src/cli.ts#L61) | defaults and option switch of `serve`; `--models` and `--model-file` only together, the file in a folder disjoint from the data folder |
| `registerOptions` | [129](../src/cli.ts#L129) | defaults and option switch of `register` |
| `OptionReader` | [146](../src/cli.ts#L146) | `value`: the next argument, required<br>`optionalValue`: the next argument, absent when missing or starting with `--` |
| `validAgentName` | [168](../src/cli.ts#L168) | `^[a-z0-9](-?[a-z0-9])*-?$`, at most 32 characters |
| `modelList` | [199](../src/cli.ts#L199) | `--models` items `NAME=PROVIDER/MODEL`, names and PROVIDER/MODEL values unique |
| `usageError` | [221](../src/cli.ts#L221) | `ExitError` with code 2 |
| `main` | [225](../src/cli.ts#L225) | `help` to stdout, `serve`, `register` |
| top-level catch | [238-247](../src/cli.ts#L238-L247) | `ExitError`: message on stderr, its code; other errors: `logError`, code 1 |

### [2.2 `serve.ts`](../src/serve.ts)

Start sequence, event dispatch and stop sequence of one agent.

| Symbol | Line | Contents |
|---|---|---|
| `version` | [15](../src/serve.ts#L15) | the package version |
| `OwnerEntry` | [19](../src/serve.ts#L19) | `{memberId, name}` of `--owners` |
| `ServeOptions` | [24](../src/serve.ts#L24) | name, data folder and its `dataGiven` flag, display name, mode, owners, expiry, reset and token flags, port, QR code size, stop wait, models and their file |
| `RunningServer` | [40](../src/serve.ts#L40) | token, `ChatApi`, user, `Actions`, `stop` |
| `serve` | [48](../src/serve.ts#L48) | `startServer`, then SIGINT and SIGTERM handlers: the first signal stops, a second ends the stop wait |
| `startServer` | [66](../src/serve.ts#L66) | data folder, lock, token, headers file, the model file, `ChatApi.init`, `startAgent`; cleanup on error |
| `startAgent` | [91](../src/serve.ts#L91) | user, network config, temp folder, control group, wiring of `Actions`, `AgentTools`, `Control` with the selected model, HTTP, event handler, `stop` |
| `registerHint` | [188](../src/serve.ts#L188) | the `register` command line printed after the owner setup |
| `waitForCancelledCards` | [192](../src/serve.ts#L192) | the fixed wait at stop for cards edited or posted at the stop |
| `setTempFolder` | [208](../src/serve.ts#L208) | `DATA/tmp`, mode 0700, as the core's temp folder |
| `updatedUser` | [214](../src/serve.ts#L214) | display name and peer type `agent` of the profile |
| `controlGroup` | [221](../src/serve.ts#L221) | found, created or reset agent control chat |
| `listen` | [228](../src/serve.ts#L228) | `startHttp`; `EADDRINUSE` to `ExitError` code 1 |
| `bufferEvents` | [237](../src/serve.ts#L237) | events received before the handler is installed are queued and replayed in order |

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
| `PlanSummary` | [86](../src/tools.ts#L86) | card kind, chat type, name, SimpleX name, peer type, description, welcome, commands, `takesMessage` |
| `ChatFields` | [98](../src/tools.ts#L98) | chat fields of `connect` and `list` results |
| `Agent` | [112](../src/tools.ts#L112) | `ChatApi`, user, `Actions`, `ChatItemsWatch`, control chat reference, standing approver lookup, refused file post, data folder |
| `JoinMethod` | [123](../src/tools.ts#L123) | `prepared`, `prepareChannel`, `viaAddress`, `prepareContact`, `connect` |
| schemas | [178-401](../src/tools.ts#L178-L401) | input and output JSON schemas |
| descriptions, `instructions` | [403-441](../src/tools.ts#L403-L441) | tool descriptions and server instructions |
| `definitions` | [451](../src/tools.ts#L451) | name, description, schemas and action flag of the six tools |
| `Download` | [462](../src/tools.ts#L462) | one download: its promise, chat, item, file ID, size, path, the last fileSource seen, received bytes and their time, check timer |
| `AgentTools.register` | [488](../src/tools.ts#L488) | `registerTool` for each definition; `_meta` in mode `host` |
| `AgentTools.call` | [500](../src/tools.ts#L500) | handler result to `CallToolResult`; errors to `isError` |
| `AgentTools.perform` | [509](../src/tools.ts#L509) | perform step of an approved action |
| `downloadFile`, `forwardFile`, `downloadTo` | [522-549](../src/tools.ts#L522-L549) | one download per file ID, shared by `download` and `/forward`; a forwarded file moved to the download path for `download` |
| file events | [551-588](../src/tools.ts#L551-L588) | `onNewItems`, `onFile`, `onFileComplete`, `onFileError`, `onFileCancelled`, `onFileDescription` |
| `onContactConnected`, `onUserJoinedGroup` | [590-598](../src/tools.ts#L590-L598) | completion of connect actions by events |
| `actionResult` | [636](../src/tools.ts#L636) | wait for the decision, result fields, removal |
| `attachment`, `isRefused`, `fileToSend`, `resizedCopy` | [860-896](../src/tools.ts#L860-L896) | the file of a `send` |
| `copyForSending`, `chatFiles`, `deleteChatFiles` | [898-927](../src/tools.ts#L898-L927) | `/forward` copies; the files of a chat, forwarded ones included; its downloads cancelled, also those the core continues after a restart |
| `leaveChat` | [950](../src/tools.ts#L950) | perform step of `leave` and `/leave`; the chat's forwarded files deleted |
| `download`, `fileServers`, `unknownServers` | [960-1006](../src/tools.ts#L960-L1006) | the `download` call before its action |
| `startDownload`, `checkDownload`, `currentFile`, `settleFile`, `settleReceived`, `moveReceived` | [1024-1091](../src/tools.ts#L1024-L1091) | a download's steps |
| `agentChats`, `knownChat` | [1160-1177](../src/tools.ts#L1160-L1177) | the agent's chats; a checked chat reference |
| `classifyPlan` | [1260](../src/tools.ts#L1260) | `apiConnectPlan` response to a join plan or a known chat |
| `chatSubject`, `isConnected`, `isActive`, `memberCurrent` | [1356-1409](../src/tools.ts#L1356-L1409) | chat and member predicates shared with `control.ts` |
| `fitsMessage`, `quoteContent` | [1433-1449](../src/tools.ts#L1433-L1449) | the message as the core sends it, within 15602 bytes; for `send` and the post of `/forward` |
| `realFile` | [1465](../src/tools.ts#L1465) | real path, size, inode and change time of a file |
| `attachmentMedia`, `videoFrame`, `videoDuration` | [1512-1546](../src/tools.ts#L1512-L1546) | previews and durations |
| `toolCall` | [1562](../src/tools.ts#L1562) | `ServerContext` to `ToolCall`: abort signal and progress notifications |

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

Hooks are bound in [`serve.ts:103-109`](../src/serve.ts#L103-L109): `refusal`, `saveCounter`, `postCard` and `outcome` to `Control`; `perform` to `AgentTools`.

### [2.6 `control.ts`](../src/control.ts)

| Symbol | Line | Contents |
|---|---|---|
| `controlCommands` | [11](../src/control.ts#L11) | the command menu |
| `modelCommand` | [16](../src/control.ts#L16) | the menu entry `/model`, appended with `--models` |
| `standings` | [24](../src/control.ts#L24) | custom data key and reply words of the approvals of messages and files |
| `ControlData` | [45](../src/control.ts#L45) | `{control: true, nextAction}` |
| `ControlOptions` | [50](../src/control.ts#L50) | display name, owners, register hint, QR code size; with `--models`, the models, their file and the selected model |
| `ChatApproval` | [63](../src/control.ts#L63) | `{approvedBy, approvedById, approvedAt}` |
| `OwnerRequest` | [69](../src/control.ts#L69) | parsed owner command |
| `Control.load` | [106](../src/control.ts#L106) | lost check, group profile and menu, member list |
| `Control.start` | [116](../src/control.ts#L116) | owner setup per `--owners` |
| `Control.refusal` | [124](../src/control.ts#L124) | bytes left for the link texts of a connect card, card size of a send, connect or download, approver presence |
| `Control.standingApprover` | [135](../src/control.ts#L135) | approver name of a chat's standing approval of messages or files |
| `Control.saveCounter` | [140](../src/control.ts#L140) | queued counter writes |
| `Control.postCard`, `Control.outcome` | [146-157](../src/control.ts#L146-L157) | card post and card edit; the kept-files message after a done leave |
| `Control.refusedFile`, `Control.fileNotSent` | [159-174](../src/control.ts#L159-L174) | posts about files |
| `Control.onMessage` | [176](../src/control.ts#L176) | owner commands, queued one at a time |
| `Control.onEvent` | [188](../src/control.ts#L188) | membership changes and loss of the control chat |
| `Control.ownerCommand` | [205](../src/control.ts#L205) | command dispatch and replies |
| `approveChat`, `revoke`, `forward`, `leave`, `deleteFiles`, `switchModel` | [250-328](../src/control.ts#L250-L328) | owner requests |
| `cardContent`, `fittedImage` | [462-482](../src/control.ts#L462-L482) | image and video cards with the file's preview |
| `findControlGroup`, `createControlChat`, `resetControlChat` | [520-548](../src/control.ts#L520-L548) | control group lookup, creation, reset |
| `isControlEvent` | [550](../src/control.ts#L550) | membership events of the control group |
| `parseCommand` | [566](../src/control.ts#L566) | one-line `/keyword params` |
| `cardText`, `requestLines`, `commandsLine` | [573-637](../src/control.ts#L573-L637) | card text per tool, state and mode |
| `largestCard`, `encodedLength` | [643-651](../src/control.ts#L643-L651) | card size check |
| `serversLine`, `sizeText` | [685-694](../src/control.ts#L685-L694) | servers line and sizes of download cards |
| `codeSpan`, `safeName`, `mentionText` | [704-716](../src/control.ts#L704-L716) | quoting of peer text |
| `ownerRequest` | [731](../src/control.ts#L731) | command words to an `OwnerRequest` |
| `qrCodeText` | [747](../src/control.ts#L747) | terminal QR code |
| `losesControl`, `isLost` | [763-778](../src/control.ts#L763-L778) | loss of the control chat |
| `entryText`, `restartText` | [796-806](../src/control.ts#L796-L806) | `--owners` entries in restart lines |

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
| `ModelEntry`, `ModelOptions`, `ModelSelection` | [12-16](../src/files.ts#L12-L16) | `{name, model}` of a `--models` item; the items and FILE of `--model-file`; the same with the selected item |
| `createDataFolder` | [18](../src/files.ts#L18) | `mkdir -p` mode 0700; `chmod 0700` except on Windows |
| `acquireLock`, `releaseLock` | [23-34](../src/files.ts#L23-L34) | `DATA/lock` with the PID |
| `serverToken` | [36](../src/files.ts#L36) | `DATA/token`: read, or 32 random bytes in base64url |
| `writeHeadersFile` | [48](../src/files.ts#L48) | `DATA/headers.json` |
| `writeServerFile`, `readServerFile` | [52-59](../src/files.ts#L52-L59) | `DATA/server.json` |
| `modelAtStart` | [61](../src/files.ts#L61) | the item whose PROVIDER/MODEL equals the first line of FILE; otherwise the first item, written to FILE |
| `writeModelFile` | [68](../src/files.ts#L68) | PROVIDER/MODEL and a newline to `FILE.part`, mode 0644, set again except on Windows; then renamed to FILE |

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
| Actions | `Actions.actions` map ([actions.ts:128](../src/actions.ts#L128)) | `create`, `remove`, `expireDue` | tools, `Control` |
| Open calls of an action | `ActionRecord.open` ([:44](../src/actions.ts#L44)) | `actionResult` ([tools.ts:637](../src/tools.ts#L637), [:646](../src/tools.ts#L646)) | `list`, `expireDue` |
| Collected replies | `ConnectAction.messages`, `SendAction.responses` ([:56](../src/actions.ts#L56), [:87](../src/actions.ts#L87)) | `ChatItemsWatch.collect` | `connect`, `send` results |
| Item listeners | `ChatItemsWatch.listeners` ([messages.ts:87](../src/messages.ts#L87)) | `collect` | event dispatch |
| Downloads | `AgentTools.downloads`, one per file ID ([tools.ts:480](../src/tools.ts#L480)) | `downloadTo` | file events, checks, `/delete CHAT files` |
| Forwards | `AgentTools.forwarding`, the copy for the post per file ID ([tools.ts:481](../src/tools.ts#L481)) | `forwardFile` | `downloadFile`, before it moves a forwarded file |
| Rejected files | `AgentTools.rejectedFiles` ([tools.ts:482](../src/tools.ts#L482)) | `downloadResult` ([:1013-1014](../src/tools.ts#L1013-L1014)) | the file field; `download`, which then skips the files approval ([:975](../src/tools.ts#L975)) |
| Count of file descriptions | `AgentTools.describedFiles` ([tools.ts:483-484](../src/tools.ts#L483-L484)) | `onFileDescription` | `fileServers` |
| Members of the control chat | `Control.members` ([control.ts:86](../src/control.ts#L86)) | `refreshMembers` ([:484-486](../src/control.ts#L484-L486)) | approver checks |
| Selected model | `ControlOptions.model.selected` ([control.ts:55](../src/control.ts#L55)) | `modelAtStart` ([files.ts:61-66](../src/files.ts#L61-L66)), `switchModel` ([control.ts:325](../src/control.ts#L325)) | `/model` |
| Mode, expiry, owners list, stop wait, models and their file | `ServeOptions` of the run | CLI | all modules |

### 3.2 Chat database custom data

| Item | Entity | Value | Written by | Read by |
|---|---|---|---|---|
| Control marker and counter | agent control chat | `{simplexMcp: {control: true, nextAction: N}}` | [`createControlChat`](../src/control.ts#L530-L541), [`saveCounter`](../src/control.ts#L140) | [`findControlGroup`](../src/control.ts#L520), [`controlDataOf`](../src/control.ts#L525) |
| Standing approval of messages | contact or group of the agent | `{...other keys, simplexMcp: {approvedBy, approvedById, approvedAt}}` | [`approveChat`](../src/control.ts#L250), [`revoke`](../src/control.ts#L263) | [`standingApprover`](../src/control.ts#L135) |
| Files approval | contact or group of the agent | `{...other keys, simplexMcpFiles: {approvedBy, approvedById, approvedAt}}` | [`approveChat`](../src/control.ts#L250), [`revoke`](../src/control.ts#L263) | [`standingApprover`](../src/control.ts#L135) |

The profile, chats, items, members and file transfers are stored by the core in `DATA/simplex_chat.db`; connections and queues in `DATA/simplex_agent.db` (file prefix `DATA/simplex`, [serve.ts:74](../src/serve.ts#L74)).

### 3.3 Files

| Path | Contents | Mode | Written | Read |
|---|---|---|---|---|
| `DATA/` | data folder | 0700, set at every start except on Windows | [files.ts:18-21](../src/files.ts#L18-L21) | |
| `DATA/token` | base64url of 32 random bytes | 0600, set again at every write except on Windows | first start, `--new-token` ([:36-42](../src/files.ts#L36-L42)) | every start |
| `DATA/headers.json` | `{"Authorization":"Bearer <token>"}` | 0600, set again at every write except on Windows | every start ([:48-50](../src/files.ts#L48-L50)) | Claude Code `headersHelper` |
| `DATA/server.json` | `{name, port, expireHours}` | 0600, set again at every write except on Windows | every start, after the HTTP listener is bound ([serve.ts:121](../src/serve.ts#L121)) | `register` |
| `DATA/lock` | PID and a newline | 0600 | start ([files.ts:83-91](../src/files.ts#L83-L91)) | next start |
| `DATA/tmp/` | the core's temp folder: files in transfer | 0700, set at every start | [serve.ts:96](../src/serve.ts#L96), [:208-212](../src/serve.ts#L208-L212) | the core |
| `DATA/files/group-N/`, `DATA/files/contact-N/` | downloaded files and files received without a download, named by file ID and extension | folder 0700 | the core at the download path ([tools.ts:1029-1030](../src/tools.ts#L1029-L1030)); moves ([:1500-1510](../src/tools.ts#L1500-L1510)) | `download` results, `/forward`, `/leave`, `/delete CHAT files` |
| `DATA/forwarded/group-N/`, `DATA/forwarded/contact-N/` | files `/forward` downloaded, named as above, until a download moves them to `DATA/files/` | folder 0700 | the core at the forwarded path ([tools.ts:532-539](../src/tools.ts#L532-L539), [:1029-1030](../src/tools.ts#L1029-L1030)) | `/forward`; moved by `download` ([:522-530](../src/tools.ts#L522-L530)); deleted by `leave`, `/leave` and `/delete CHAT files` ([:909-927](../src/tools.ts#L909-L927), [:950-958](../src/tools.ts#L950-L958)) |
| `DATA/files/sent/` | resized copies of sent images, and copies of the files `/forward` posts, one folder per copy | folder 0700; a resized copy 0600 | [tools.ts:885-902](../src/tools.ts#L885-L902), [:929-933](../src/tools.ts#L929-L933) | the core; deleted at `sndFileCompleteXFTP`, `sndFileError` and at start ([serve.ts:97](../src/serve.ts#L97), [:159-163](../src/serve.ts#L159-L163)) |
| FILE of `--model-file`, outside `DATA/` | PROVIDER/MODEL of the selected model and a newline | 0644, set again at every write except on Windows | at start when its first line is no listed PROVIDER/MODEL, and at each switch; written to `FILE.part`, then renamed ([files.ts:61-72](../src/files.ts#L61-L72), [control.ts:324](../src/control.ts#L324)) | `modelAtStart`; the script that runs the agent in opencode |

The core library is downloaded once into `CACHE/core-7.1.0.20/sqlite`, and the Node.js add-on into `CACHE/nodejs-addon-cce06d8/`, each with a `Downloading ...` line on stderr. CACHE is `$XDG_CACHE_HOME/simplex-chat` or `~/.cache/simplex-chat`; on macOS `~/Library/Caches/simplex-chat`; on Windows `%LOCALAPPDATA%\simplex-chat` ([libs.ts:41-48](../../simplex-chat-nodejs/src/libs.ts#L41-L48), [:89](../../simplex-chat-nodejs/src/libs.ts#L89), [:109](../../simplex-chat-nodejs/src/libs.ts#L109), [:116](../../simplex-chat-nodejs/src/libs.ts#L116), [:129](../../simplex-chat-nodejs/src/libs.ts#L129)).

---

## 4. Process lifecycle

### 4.1 Start

1. Data folder, lock, token and headers file are prepared; with `--models`, FILE is read, and the first model is written to it when its first line is no listed PROVIDER/MODEL ([serve.ts:68-73](../src/serve.ts#L68-L73), [files.ts:61-66](../src/files.ts#L61-L66)).
2. libsimplex for SQLite is loaded by `ChatApi.init`, and the databases are migrated with confirmation `yesUp` ([serve.ts:74](../src/serve.ts#L74), [api.ts:117-126](../../simplex-chat-nodejs/src/api.ts#L117-L126)).
3. Three log lines: version and NAME, data folder, mode; with `--models`, a fourth with the selected model and FILE ([serve.ts:75-78](../src/serve.ts#L75-L78)).
4. The active user is read, or created with display name `--display-name` or NAME, empty full name and peer type `agent` ([serve.ts:93](../src/serve.ts#L93)).
5. The network config is read and set with `smpProxyMode` `always` and `smpProxyFallback` `allowProtected` ([:94-95](../src/serve.ts#L94-L95)).
6. `DATA/tmp` is set as the core's temp folder, and `DATA/files/sent` is deleted ([:96-97](../src/serve.ts#L96-L97)).
7. Events are buffered, and the chat is started ([:98-99](../src/serve.ts#L98-L99)).
8. A `--display-name` different from the profile, or a peer type other than `agent`, is applied with one `apiUpdateProfile` ([:100](../src/serve.ts#L100), [:214-219](../src/serve.ts#L214-L219)).
9. The agent control chat is found by its marker, created with counter 1, or reset with `--reset-owners` ([:102](../src/serve.ts#L102), [:221-226](../src/serve.ts#L221-L226)).
10. `Actions`, `ChatItemsWatch`, `AgentTools` and `Control` are created; the counter is taken from the marker, and the selected model is passed to `Control` ([:103-112](../src/serve.ts#L103-L112)).
11. `Control.load`: for a lost control chat, `Actions.lost` is set; otherwise the group name and command menu, with `/model` appended when `--models` is given, are updated when they differ, and members are read ([control.ts:106-114](../src/control.ts#L106-L114)).
12. The HTTP server listens; with the port in use, the process exits with code 1 ([serve.ts:120](../src/serve.ts#L120), [:228-235](../src/serve.ts#L228-L235)). Until step 14 ends, requests to `/mcp` that pass the token check get 503 with `Retry-After: 1` ([http.ts:23](../src/http.ts#L23), [serve.ts:119-120](../src/serve.ts#L119-L120), [:128](../src/serve.ts#L128)).
13. `server.json` is written ([serve.ts:121](../src/serve.ts#L121)).
14. The server runs the owner setup, `Control.start`; after an error, HTTP is closed and the error is rethrown ([serve.ts:122-127](../src/serve.ts#L122-L127)).
15. `Listening on http://127.0.0.1:PORT/mcp` ([:129](../src/serve.ts#L129)).
16. Buffered events are replayed, then live events are dispatched ([:132](../src/serve.ts#L132), [:237-251](../src/serve.ts#L237-L251)).

After an error inside `startAgent` (steps 4-16) the chat is closed; after any error once the lock is taken, the lock is released ([serve.ts:70-88](../src/serve.ts#L70-L88)). An error reading or writing FILE, such as a missing folder, ends the start in step 1, before the databases are opened.

### 4.2 Event dispatch

Each `onAny` receiver is awaited by the library before the next event is read ([api.ts:195-202](../../simplex-chat-nodejs/src/api.ts#L195-L202)). The dispatcher in [`serve.ts:132-168`](../src/serve.ts#L132-L168):

| Event | Condition | Handler |
|---|---|---|
| `newChatItems` | item of the control group, main scope | [`Control.onMessage`](../src/control.ts#L176) |
| `newChatItems` | every item | [`AgentTools.onNewItems`](../src/tools.ts#L551), then [`ChatItemsWatch.deliver`](../src/messages.ts#L116) |
| `chatItemUpdated` | | [`AgentTools.onFile`](../src/tools.ts#L561), then [`ChatItemsWatch.update`](../src/messages.ts#L120) |
| `chatItemsDeleted` | | [`ChatItemsWatch.remove`](../src/messages.ts#L124) of each `deletedChatItem` |
| `contactConnected` | | [`AgentTools.onContactConnected`](../src/tools.ts#L590) |
| `userJoinedGroup` | | [`AgentTools.onUserJoinedGroup`](../src/tools.ts#L595) |
| `rcvFileComplete` | | [`AgentTools.onFileComplete`](../src/tools.ts#L566) |
| `rcvFileSndCancelled` | | [`AgentTools.onFileCancelled`](../src/tools.ts#L578) |
| `rcvFileError` | | [`AgentTools.onFileError`](../src/tools.ts#L573) |
| `rcvFileDescrReady` | | [`AgentTools.onFileDescription`](../src/tools.ts#L585) |
| `sndFileCompleteXFTP` | | [`AgentTools.onSentFile`](../src/tools.ts#L848) |
| `sndFileError` | | [`AgentTools.onSentFile`](../src/tools.ts#L848), then [`Control.fileNotSent`](../src/control.ts#L165) |
| `joinedGroupMember`<br>`joinedGroupMemberConnecting`<br>`connectedToGroupMember`<br>`memberRole`<br>`leftMember`<br>`deletedMember`<br>`deletedMemberUser`<br>`groupDeleted` | event of the control group | [`Control.onEvent`](../src/control.ts#L188), queued one at a time |
| any event | after `stop()` started | ignored |

Owner commands are queued in a second chain, independent of the membership chain ([control.ts:182-185](../src/control.ts#L182-L185)).

### 4.3 Stop

On `SIGINT` or `SIGTERM` the server runs one stop; a later signal ends its wait for cancelled cards ([serve.ts:48-64](../src/serve.ts#L48-L64)). `RunningServer.stop` ([serve.ts:174-184](../src/serve.ts#L174-L184)):

1. Events are ignored from now on.
2. `Actions.stop` starts: new actions are refused with `The server is stopping.`; the expiry timer is cleared; cards being posted are awaited; pending and performing actions are marked interrupted; the stop signal of the performs is aborted, which ends the wait of a `connect` for its connection; their cards are edited, or posted when they have no card; outcome edits already in progress are awaited ([actions.ts:240-250](../src/actions.ts#L240-L250)).
3. In parallel with step 2, the HTTP server is closed with all connections, and open calls are aborted ([http.ts:48-52](../src/http.ts#L48-L52)).
4. `Cancelled K action(s).` when K > 0 ([serve.ts:179](../src/serve.ts#L179)).
5. When cards were edited or posted at the stop and the control chat is not lost, the server waits `--stop-wait` seconds, 10 by default, for them to be sent; a second signal ends the wait ([serve.ts:180](../src/serve.ts#L180), [:192-206](../src/serve.ts#L192-L206)).
6. `chat.close()`: the chat is stopped, then the store is closed ([api.ts:165-170](../../simplex-chat-nodejs/src/api.ts#L165-L170)).
7. The lock is removed, `Stopped.` is logged, and the process exits with code 0, or 1 after an error ([serve.ts:182-183](../src/serve.ts#L182-L183), [:54-60](../src/serve.ts#L54-L60)).

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

`isRunning`: `true` when `kill(pid, 0)` succeeds or fails with `EPERM` ([files.ts:98-105](../src/files.ts#L98-L105)).

---

## 5. Approval model

### 5.1 Modes

The mode is set by `--approve` for one run ([cli.ts:85-87](../src/cli.ts#L85-L87)).

| Mode | State of a new action | Card | `_meta` of `connect`, `send`, `download`, `leave` |
|---|---|---|---|
| `requests` | `pending`<br>`performing` for a send under a standing approval, and for a download under a files approval | card with commands<br>outcome as a new message after a standing send; none after a download without a card | none |
| `host` | `performing` | outcome as a new message after the perform; none for a download | `{"anthropic/requiresUserInteraction": true}` |

Sources: [actions.ts:166](../src/actions.ts#L166), [tools.ts:494](../src/tools.ts#L494), [control.ts:150-157](../src/control.ts#L150-L157), [:516-518](../src/control.ts#L516-L518). In both modes a new action is refused unless the control chat has an approver ([control.ts:129-132](../src/control.ts#L129-L132)).

### 5.2 Owners and approvers

| Term | Rule | Code |
|---|---|---|
| human | member other than the agent's membership, status `connected` or `complete` | [control.ts:505-507](../src/control.ts#L505-L507), [tools.ts:1394-1396](../src/tools.ts#L1394-L1396) |
| human owner | human with role `owner` | [control.ts:492-494](../src/control.ts#L492-L494) |
| approver, `--owners` absent | member with role `owner` | [control.ts:509-513](../src/control.ts#L509-L513) |
| approver, `--owners` without a list | none | [control.ts:512](../src/control.ts#L512) |
| approver, `--owners ID:NAME,...` | member with role `owner` whose `groupMemberId` and `displayName`, folded to one line by `oneLine`, match an entry | [control.ts:512](../src/control.ts#L512), [:780-782](../src/control.ts#L780-L782) |

The command sender's member record from the chat item is checked ([control.ts:181](../src/control.ts#L181), [:209](../src/control.ts#L209)). The approver count for refusals and standing approvals is taken from `Control.members`, refreshed at load and at each membership event ([control.ts:192](../src/control.ts#L192), [:484-486](../src/control.ts#L484-L486)).

Owner setup at start ([control.ts:116-122](../src/control.ts#L116-L122)):

| `--owners` | Start | First join by the link |
|---|---|---|
| absent | a human owner exists: register hint<br>otherwise: `No owners in the agent control chat.` when humans exist, then a link with role `member` | link deleted, role `owner`, greeting, log line, register hint |
| without a list | no humans: link with role `observer`<br>humans exist: their IDs and the restart command, in the log and the chat | link deleted, the joined member and the restart command announced |
| `ID:NAME,...` | each entry matched to a human, or exit 2<br>role `owner` set where missing<br>other humans removed<br>link deleted<br>`Owners: ...` and register hint | |

Sources: [`startOwners`](../src/control.ts#L362), [`startObserver`](../src/control.ts#L368), [`startListed`](../src/control.ts#L373), [`ownerJoined`](../src/control.ts#L392), [`observerJoined`](../src/control.ts#L401).

Membership changes at run time ([control.ts:188-203](../src/control.ts#L188-L203)):

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
| `/reject`, or `/leave CHAT` for a pending action of CHAT with a card | `pending` | `rejected` | [actions.ts:225-230](../src/actions.ts#L225-L230), [control.ts:298-300](../src/control.ts#L298-L300) |
| expiry time passes | `pending` | `expired` | [actions.ts:196-199](../src/actions.ts#L196-L199) |
| `/approve N`, `/approve CHAT N` and `/approve CHAT all` for other actions; `/receive N`, `/receive CHAT N` and `/receive CHAT N all` for downloads | `pending` | `performing` | [actions.ts:219-223](../src/actions.ts#L219-L223), [control.ts:236-245](../src/control.ts#L236-L245) |
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

An action is removed from memory when its result is returned to a call ([tools.ts:643](../src/tools.ts#L643)), or at an expiry tick when it is decided, its open call count is 0 and its expiry time has passed ([actions.ts:200-201](../src/actions.ts#L200-L201)). A `download` still performing at the end of `wait` is returned as `performing` and kept ([tools.ts:640](../src/tools.ts#L640)).

### 5.4 Cards

The server posts a card when a `pending` action is created, and edits it with `apiUpdateChatItem` at the outcome ([actions.ts:255](../src/actions.ts#L255), [control.ts:150-157](../src/control.ts#L150-L157)). For a `connect`, `send` or `leave` without a card (mode `host`, standing sends) the server posts the outcome as a new message; a `download` without a card gets none ([control.ts:516-518](../src/control.ts#L516-L518)). The text per state is built by [`cardText`](../src/control.ts#L573-L592); a card of an image or video file is an image or video message with the file's preview ([control.ts:462-482](../src/control.ts#L462-L482)); forms are listed in [API: cards](api.md#22-cards).

Outcomes are posted after `done` and `failed` ([actions.ts:282](../src/actions.ts#L282)), after `rejected` and `expired` ([:229](../src/actions.ts#L229), [:199](../src/actions.ts#L199)), and at stop ([:247](../src/actions.ts#L247)). They are skipped after the control chat is lost ([:247](../src/actions.ts#L247), [:282](../src/actions.ts#L282), [:291](../src/actions.ts#L291)).

### 5.5 Standing approvals

| Aspect | Messages | Files | Code |
|---|---|---|---|
| grant | `/approve CHAT all` from an approver in mode `requests`; pending sends of CHAT with a card are approved | `/receive CHAT N all` from an approver, on the card of download N; N is performed, and the other pending downloads of CHAT stay pending | [control.ts:244](../src/control.ts#L244), [:250-261](../src/control.ts#L250-L261) |
| mode `host` | nothing stored; reply `Approvals apply in mode requests; this server runs in mode host.` | downloads get no card, so the command has none to act on | [control.ts:218](../src/control.ts#L218) |
| removal | `/revoke CHAT`: the `simplexMcp` key is removed, other keys kept | `/receive CHAT off`: the `simplexMcpFiles` key is removed | [control.ts:263-270](../src/control.ts#L263-L270) |
| use | mode `requests`, and `approvedById` equal to the `groupMemberId` of a current approver | the same | [control.ts:135-138](../src/control.ts#L135-L138) |
| scope | `send` | `download` of a file whose servers are all known | [tools.ts:796](../src/tools.ts#L796), [:975](../src/tools.ts#L975) |
| decider shown | `safeName(approvedBy)` | the same | [control.ts:137](../src/control.ts#L137) |
| storage | the chat's custom data; removed with the chat by `leave` | the same | [tools.ts:950-958](../src/tools.ts#L950-L958) |

### 5.6 Expiry

| Rule | Code |
|---|---|
| `expires = created + expireHours × 3600000`, `expireHours` 1 to 23, default 23 | [actions.ts:163](../src/actions.ts#L163), [cli.ts:66](../src/cli.ts#L66), [:91-93](../src/cli.ts#L91-L93) |
| check every 60 s on an unreferenced timer, and before each owner command | [actions.ts:129](../src/actions.ts#L129), [control.ts:206](../src/control.ts#L206) |
| pending and due: `expired`, log `Action N expired.`, card edited to `Expired.` | [actions.ts:196-199](../src/actions.ts#L196-L199) |
| decided, no open call, due: removed from memory | [actions.ts:200-201](../src/actions.ts#L200-L201) |
| connect wait for the other side: at most until `created + 24 h - 10 min` | [tools.ts:143](../src/tools.ts#L143), [:689](../src/tools.ts#L689) |

`Date.now()` is compared with `expires`, so due actions are expired at the first tick after a sleep of the computer.

### 5.7 Action counter

| Rule | Code |
|---|---|
| initial value: `nextAction` of the control marker, or 1 | [serve.ts:103](../src/serve.ts#L103) |
| a new action takes `next`, then `next` grows by 1 | [actions.ts:156](../src/actions.ts#L156), [:178](../src/actions.ts#L178) |
| the new `next` is written before the card is posted | [actions.ts:254-255](../src/actions.ts#L254-L255) |
| one write at a time, also after a failed write | [control.ts:140-144](../src/control.ts#L140-L144) |
| `--reset-owners` copies the value into the new control chat | [serve.ts:224-225](../src/serve.ts#L224-L225), [control.ts:543-548](../src/control.ts#L543-L548) |
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

- The token is created once and kept in `DATA/token`; `--new-token` replaces it ([files.ts:36-42](../src/files.ts#L36-L42)).
- `DATA/headers.json` is rewritten at every start ([files.ts:48-50](../src/files.ts#L48-L50)).
- The Claude Code entry gets the command `cat <DATA>/headers.json`, or `type "<DATA>\headers.json"` on Windows, with the path quoted for the shell ([register.ts:50](../src/register.ts#L50), [log.ts:24-31](../src/log.ts#L24-L31)). Only this command is written to the entry and to the `claude mcp add-json` arguments ([register.test.ts:50-61](../test/unit/register.test.ts#L50-L61)).
- `token`, `headers.json` and `server.json` are set to mode 0600 at every write, and the data folder to 0700 at every start, except on Windows ([files.ts:18-21](../src/files.ts#L18-L21), [:74-77](../src/files.ts#L74-L77)).

### 6.3 Peer-written text

| Destination | Rule | Code |
|---|---|---|
| card and reply names | folded to one line; a valid display name is plain, others a code span | [`safeName`](../src/control.ts#L708-L711) |
| card text lines | one code span per line; backticks become `'`; a leading or trailing space becomes U+00A0; blank lines empty | [`codeLines`, `codeSpan`](../src/control.ts#L693-L699) |
| file names and paths on cards and posts | folded to one line, one code span | [control.ts:606](../src/control.ts#L606), [:619](../src/control.ts#L619), [:171-173](../src/control.ts#L171-L173) |
| reply quote on a send card | whitespace runs to one space, first 100 characters (code points), one code span | [control.ts:607](../src/control.ts#L607) |
| link texts on a connect card | code span lines in the bytes that the largest card form leaves; the line that does not fit is cut and ends in `…` inside its code span; later lines left out | [control.ts:665-679](../src/control.ts#L665-L679) |
| mention names | valid name: `@name` or `@'name'`; others a code span | [`mentionText`](../src/control.ts#L713-L716) |
| errors on cards | folded to one line, one code span | [control.ts:586](../src/control.ts#L586) |
| `--owners` part of restart lines in the chat | one code span | [control.ts:83](../src/control.ts#L83), [:804-806](../src/control.ts#L804-L806) |
| terminal names and errors of actions | folded to one line | [actions.ts:180-181](../src/actions.ts#L180-L181), [:333-341](../src/actions.ts#L333-L341), [control.ts:82](../src/control.ts#L82) |
| NAME of `/model NAME` | compared with the names of `--models`; left out of the replies, which contain only names and PROVIDER/MODEL values of `--models` | [control.ts:320-327](../src/control.ts#L320-L327) |
| the sender's name in the `/model` log line | folded to one line | [control.ts:326](../src/control.ts#L326) |
| tool results | returned as data; schema descriptions and instructions mark `text`, `from`, `name`, file names, downloaded files, `description`, `welcome` and `commands` with their labels and params as data, not instructions; the instructions take a chat's messages as prompts only when the user said so in the session, and limit what the agent shares about its user | [tools.ts:184](../src/tools.ts#L184), [:188](../src/tools.ts#L188), [:193](../src/tools.ts#L193), [:218-219](../src/tools.ts#L218-L219), [:229-234](../src/tools.ts#L229-L234), [:397](../src/tools.ts#L397), [:439](../src/tools.ts#L439) |

The server writes the tappable form `/'...'` only in the card's command line ([control.ts:624-637](../src/control.ts#L624-L637)), in `/'revoke CHAT'` of standing cards and replies ([:584](../src/control.ts#L584), [:260](../src/control.ts#L260)), in `/'delete CHAT files'` of the kept-files message ([:308](../src/control.ts#L308)), and in `/'model NAME'` of the `/model` replies, with NAME of `--models` ([:320](../src/control.ts#L320), [:327](../src/control.ts#L327)).

### 6.4 Chat references

| Rule | Code |
|---|---|
| `^[@#][1-9]\d{0,14}$`: 1 to 15 digits; the core reads `#(2^64 + N)` as `#N` | [messages.ts:150-152](../src/messages.ts#L150-L152), [scenarios.test.ts:491-503](../test/e2e/scenarios.test.ts#L491-L503) |
| the control chat is refused by `send`, `receive`, `download`, `leave` and the owner commands with CHAT, and left out of `list` and `/list` | [tools.ts:1116](../src/tools.ts#L1116), [:1160-1177](../src/tools.ts#L1160-L1177), [control.ts:349-356](../src/control.ts#L349-L356) |
| a store error of the chat read is reported as `No chat CHAT.` | [tools.ts:1121](../src/tools.ts#L1121), [:1175](../src/tools.ts#L1175) |
| the agent's own links are refused by `connect` (`ownLink` plans) | [tools.ts:1279](../src/tools.ts#L1279), [:1297](../src/tools.ts#L1297), [:1319](../src/tools.ts#L1319) |

### 6.5 Owner commands

| Rule | Code |
|---|---|
| source: `groupRcv` text items in the main scope of the control group | [serve.ts:137](../src/serve.ts#L137), [control.ts:178](../src/control.ts#L178) |
| form: one line `/keyword params`, leading spaces allowed | [control.ts:566-571](../src/control.ts#L566-L571) |
| the help reply is sent to any sender; other commands are accepted from approvers only | [control.ts:208-209](../src/control.ts#L208-L209) |
| commands run one at a time in arrival order | [control.ts:182-185](../src/control.ts#L182-L185) |
| `/approve`, `/reject`, `/receive` and `/forward` need a pending action with a card, and the action's chat when CHAT is given; `/receive` and `/forward` take downloads only, `/approve` the other actions only | [control.ts:234-244](../src/control.ts#L234-L244) |
| with `/model NAME`, only a model of `--models` is selected, by its NAME, in both modes; the server writes only its PROVIDER/MODEL to FILE | [control.ts:743](../src/control.ts#L743), [:316-328](../src/control.ts#L316-L328) |

### 6.6 Files

| Rule | Code |
|---|---|
| `send` refuses files named `id_*`, `*.pem`, `*.key`, `*.p12` or `*.pfx`, and files under `~/.ssh`, `~/.gnupg`, the data folder and `~/.simplex-mcp` except the agent's own `DATA/files/`, each by its real path; the refusal is posted to the control chat and logged | [tools.ts:862-876](../src/tools.ts#L862-L876), [:1465-1484](../src/tools.ts#L1465-L1484), [control.ts:159-163](../src/control.ts#L159-L163) |
| the file of a `send` is sent only when its real path, size, inode and change time at the perform are those of the request | [tools.ts:878-883](../src/tools.ts#L878-L883) |
| a sent JPEG or PNG is a resized copy made by the core | [tools.ts:885-896](../src/tools.ts#L885-L896) |
| `ffmpeg` and `ffprobe` read a video as MP4 or QuickTime, local files only, without standard input, 30 s and 64 MiB of output each | [tools.ts:1534-1552](../src/tools.ts#L1534-L1552) |
| a download is saved only at `DATA/files/group-N/` or `DATA/files/contact-N/`, or for `/forward` at the same name under `DATA/forwarded/`, named by its file ID and an extension of 1 to 16 ASCII letters and digits | [messages.ts:215-223](../src/messages.ts#L215-L223) |
| a forwarded file is outside `DATA/files/`, so `send` refuses it; a download moves it into `DATA/files/` after the copy for the post is made | [tools.ts:522-539](../src/tools.ts#L522-L539), [:871-876](../src/tools.ts#L871-L876) |
| a file on unknown servers gets a card that names them, also under a files approval; relays are approved only for such a card, and for every other download, and in mode `host`, the core checks the complete description and downloads a file on unknown servers only when the IP address is protected | [tools.ts:975](../src/tools.ts#L975), [:545](../src/tools.ts#L545), [:1030](../src/tools.ts#L1030), [control.ts:632-635](../src/control.ts#L632-L635) |
| an accepted file that the core returns to `rcvInvitation` fails its download at the next check | [tools.ts:1043](../src/tools.ts#L1043) |
| a received file is moved into the chat's folder at its completion, unless it is at the forwarded path, also from a member chat of a group, which has a download in memory as the main conversation does; a file without a download in memory, such as one whose transfer started before a restart, only when it has its full size; a partial file is deleted only for a download in memory, when it is no larger than the file | [tools.ts:1082-1091](../src/tools.ts#L1082-L1091), [:1486-1492](../src/tools.ts#L1486-L1492) |

### 6.7 Link and network

- The join link is a group link of the control chat with role `member`, or `observer` with `--owners`; it is printed with a QR code on stdout and deleted at the first join event ([control.ts:427-437](../src/control.ts#L427-L437), [:393](../src/control.ts#L393), [:402](../src/control.ts#L402)).
- At every start the server sets private message routing: `smpProxyMode` `always`, `smpProxyFallback` `allowProtected` ([serve.ts:94-95](../src/serve.ts#L94-L95)).
- With `incognito: true` the join methods connect incognito ([tools.ts:721](../src/tools.ts#L721)); the core then uses a random profile for that chat ([PLAN, Participants](../PLAN.md#participants)).

### 6.8 Model file

| Rule | Code |
|---|---|
| the folder of FILE is not the data folder, is not inside it and does not contain it; otherwise the CLI exits with code 2 | [cli.ts:124-125](../src/cli.ts#L124-L125) |
| `--models` items are checked at start: NAME `^[a-z][a-z0-9-]{0,15}$`, PROVIDER/MODEL `^[A-Za-z0-9][\w.-]{0,31}/[\w.:/+@-]{1,64}$`, the provider without `/` | [cli.ts:199-205](../src/cli.ts#L199-L205) |
| FILE contains only the PROVIDER/MODEL of an item of `--models` and a newline, with mode 0644 | [files.ts:61-72](../src/files.ts#L61-L72), [control.ts:324](../src/control.ts#L324) |
| FILE is replaced whole: the server writes `FILE.part` in the same folder and renames it to FILE | [files.ts:68-72](../src/files.ts#L68-L72) |
| at start, the model in the first line of FILE is selected only when it equals a PROVIDER/MODEL of `--models` | [files.ts:61-66](../src/files.ts#L61-L66) |

---

## 7. Limits

| Limit | Value | Code |
|---|---|---|
| pending actions | 20 | [actions.ts:11](../src/actions.ts#L11) |
| actions being performed | 20 | [actions.ts:12](../src/actions.ts#L12) |
| expiry | 1 to 23 h, default 23 | [cli.ts:66](../src/cli.ts#L66), [:91-93](../src/cli.ts#L91-L93) |
| connect wait for the other side | `created + 23 h 50 min` | [tools.ts:143](../src/tools.ts#L143) |
| send, connect and download card | 15000 bytes JSON-escaped, in its largest form | [control.ts:28](../src/control.ts#L28), [:643-651](../src/control.ts#L643-L651) |
| error assumed in the largest card | 600 × `€` | [control.ts:32](../src/control.ts#L32) |
| message as the core sends it | 15602 bytes, counted with 600 bytes of envelope; `send` refuses a longer one, and the post of `/forward` leaves out its quote of the card | [tools.ts:153-154](../src/tools.ts#L153-L154), [:1433-1436](../src/tools.ts#L1433-L1436), [control.ts:272-288](../src/control.ts#L272-L288) |
| `send.text` | 1 to 12000 characters | [tools.ts:256](../src/tools.ts#L256) |
| `connect.message` | 1 to 2000 characters | [tools.ts:244](../src/tools.ts#L244) |
| `send.mention` | 1 to 3 unique member IDs | [tools.ts:260](../src/tools.ts#L260) |
| `wait` | 0 to 86400 s | [tools.ts:245](../src/tools.ts#L245), [:261](../src/tools.ts#L261), [:273](../src/tools.ts#L273), [:296](../src/tools.ts#L296) |
| `idle` | 1 to 300 s, default 30 | [tools.ts:246](../src/tools.ts#L246), [:262](../src/tools.ts#L262), [:149](../src/tools.ts#L149) |
| `limit` | 1 to 100, default 50 | [tools.ts:263](../src/tools.ts#L263), [:274](../src/tools.ts#L274), [:150](../src/tools.ts#L150) |
| `connect` snapshot | last 50 items | [tools.ts:151](../src/tools.ts#L151) |
| collected items per action or call | 100, then their IDs kept as overflow until deleted | [messages.ts:10](../src/messages.ts#L10), [:109-110](../src/messages.ts#L109-L110) |
| unread page | 100 items | [messages.ts:9](../src/messages.ts#L9) |
| chat list read | one read of 1000000 chats | [tools.ts:142](../src/tools.ts#L142) |
| `download` wait, default | 60 s | [tools.ts:146](../src/tools.ts#L146) |
| download check | every 60 s | [tools.ts:147](../src/tools.ts#L147) |
| download without progress | 30 min, then cancelled | [tools.ts:148](../src/tools.ts#L148) |
| `ffmpeg`, `ffprobe` | 30 s, 64 MiB of output | [tools.ts:152](../src/tools.ts#L152), [:156](../src/tools.ts#L156) |
| stop wait | 0 to 60 s, default 10 | [cli.ts:71](../src/cli.ts#L71), [:94-96](../src/cli.ts#L94-L96) |
| error details | 500 characters | [log.ts:4](../src/log.ts#L4) |
| progress interval | 60 s | [messages.ts:7](../src/messages.ts#L7) |
| expiry tick | 60 s | [actions.ts:10](../src/actions.ts#L10) |
| HTTP body | 4 MiB | [server index.cjs:254](../node_modules/@modelcontextprotocol/server/dist/index.cjs#L254) |
| SSE keep-alive | 15 s | [mcp-BumlNux0.cjs:64](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L64) |
| agent NAME | 1 to 32 characters | [cli.ts:169](../src/cli.ts#L169) |
| display name | 50 characters, `mkValidName` rules | [messages.ts:11-12](../src/messages.ts#L11-L12), [:337-359](../src/messages.ts#L337-L359) |
| port | 1 to 65535, default 5226 | [cli.ts:69](../src/cli.ts#L69), [:103-105](../src/cli.ts#L103-L105) |
| `--models` item | NAME 1 to 16 characters, provider 1 to 32, model 1 to 64 | [cli.ts:200](../src/cli.ts#L200) |
