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
    core["SimpleX core<br/>libsimplex core-7.1.0.18"]
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
3. An `McpServer` is built from the factory for each request, and the arguments are validated against the input schema by the SDK ([`serve.ts:111-115`](../src/serve.ts#L111-L115), [`mcp-BumlNux0.cjs:1812-1817`](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L1812-L1817)).
4. The handler result is returned by `AgentTools.call` as `structuredContent` and as JSON text ([`tools.ts:493-500`](../src/tools.ts#L493-L500)).
5. The server calls `ChatApi` methods of the core ([`messages.ts:259-265`](../src/messages.ts#L259-L265)); actions are created in `Actions` and shown to the owner by `Control` ([`actions.ts:155-186`](../src/actions.ts#L155-L186)).
6. The core emits events; the server dispatches them in one handler of `serve.ts` to the tools, the item watch and the agent control chat ([`serve.ts:129-165`](../src/serve.ts#L129-L165)).

---

## 2. Modules

### [2.1 `cli.ts`](../src/cli.ts)

Entry point of the `simplex-mcp` binary.

| Symbol | Line | Contents |
|---|---|---|
| `helpText` | [10](../src/cli.ts#L10) | `--help` output |
| `Command` | [40](../src/cli.ts#L40) | `help`, `serve` with `ServeOptions`, `register` with `RegisterOptions` |
| `parseCommand` | [45](../src/cli.ts#L45) | command, NAME and options to a `Command` |
| `serveOptions` | [54](../src/cli.ts#L54) | defaults and option switch of `serve` |
| `registerOptions` | [110](../src/cli.ts#L110) | defaults and option switch of `register` |
| `OptionReader` | [127](../src/cli.ts#L127) | `value`: the next argument, required<br>`optionalValue`: the next argument, absent when missing or starting with `--` |
| `validAgentName` | [149](../src/cli.ts#L149) | `^[a-z0-9](-?[a-z0-9])*-?$`, at most 32 characters |
| `usageError` | [194](../src/cli.ts#L194) | `ExitError` with code 2 |
| `main` | [198](../src/cli.ts#L198) | `help` to stdout, `serve`, `register` |
| top-level catch | [211-220](../src/cli.ts#L211-L220) | `ExitError`: message on stderr, its code; other errors: `logError`, code 1 |

### [2.2 `serve.ts`](../src/serve.ts)

Start sequence, event dispatch and stop sequence of one agent.

| Symbol | Line | Contents |
|---|---|---|
| `version` | [15](../src/serve.ts#L15) | the package version |
| `OwnerEntry` | [19](../src/serve.ts#L19) | `{memberId, name}` of `--owners` |
| `ServeOptions` | [24](../src/serve.ts#L24) | name, data folder and its `dataGiven` flag, display name, mode, owners, expiry, reset and token flags, port, QR code size, stop wait |
| `RunningServer` | [39](../src/serve.ts#L39) | token, `ChatApi`, user, `Actions`, `stop` |
| `serve` | [47](../src/serve.ts#L47) | `startServer`, then SIGINT and SIGTERM handlers: the first signal stops, a second ends the stop wait |
| `startServer` | [65](../src/serve.ts#L65) | data folder, lock, token, headers file, `ChatApi.init`, `startAgent`; cleanup on error |
| `startAgent` | [88](../src/serve.ts#L88) | user, network config, temp folder, control group, wiring of `Actions`, `AgentTools`, `Control`, HTTP, event handler, `stop` |
| `registerHint` | [185](../src/serve.ts#L185) | the `register` command line printed after the owner setup |
| `waitForCancelledCards` | [189](../src/serve.ts#L189) | the fixed wait at stop for cards edited or posted at the stop |
| `setTempFolder` | [205](../src/serve.ts#L205) | `DATA/tmp`, mode 0700, as the core's temp folder |
| `updatedUser` | [211](../src/serve.ts#L211) | display name and peer type `agent` of the profile |
| `controlGroup` | [218](../src/serve.ts#L218) | found, created or reset agent control chat |
| `listen` | [225](../src/serve.ts#L225) | `startHttp`; `EADDRINUSE` to `ExitError` code 1 |
| `bufferEvents` | [234](../src/serve.ts#L234) | events received before the handler is installed are queued and replayed in order |

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
| `ConnectInput`, `SendInput`, `ReceiveInput`, `LeaveInput`, `DownloadInput` | [44-78](../src/tools.ts#L44-L78) | tool inputs |
| `PlanSummary` | [84](../src/tools.ts#L84) | card kind, chat type, name, SimpleX name, peer type, description, welcome, commands, `takesMessage` |
| `ChatFields` | [96](../src/tools.ts#L96) | chat fields of `connect` and `list` results |
| `Agent` | [110](../src/tools.ts#L110) | `ChatApi`, user, `Actions`, `ChatItemsWatch`, control chat reference, standing approver lookup, refused file post, data folder |
| `JoinMethod` | [121](../src/tools.ts#L121) | `prepared`, `prepareChannel`, `viaAddress`, `prepareContact`, `connect` |
| schemas | [173-396](../src/tools.ts#L173-L396) | input and output JSON schemas |
| descriptions, `instructions` | [398-436](../src/tools.ts#L398-L436) | tool descriptions and server instructions |
| `definitions` | [446](../src/tools.ts#L446) | name, description, schemas and action flag of the six tools |
| `Download` | [457](../src/tools.ts#L457) | one download: its promise, chat, item, file ID, size, path, the last fileSource seen, received bytes and their time, check timer |
| `AgentTools.register` | [481](../src/tools.ts#L481) | `registerTool` for each definition; `_meta` in mode `host` |
| `AgentTools.call` | [493](../src/tools.ts#L493) | handler result to `CallToolResult`; errors to `isError` |
| `AgentTools.perform` | [502](../src/tools.ts#L502) | perform step of an approved action |
| `downloadFile` | [515](../src/tools.ts#L515) | one download per file ID, shared by `download` and `/receive` |
| file events | [525-562](../src/tools.ts#L525-L562) | `onNewItems`, `onFile`, `onFileComplete`, `onFileError`, `onFileCancelled`, `onFileDescription` |
| `onContactConnected`, `onUserJoinedGroup` | [564-572](../src/tools.ts#L564-L572) | completion of connect actions by events |
| `actionResult` | [610](../src/tools.ts#L610) | wait for the decision, result fields, removal |
| `attachment`, `isRefused`, `fileToSend`, `resizedCopy` | [834-870](../src/tools.ts#L834-L870) | the file of a `send` |
| `copyForSending`, `chatFiles`, `deleteChatFiles` | [872-894](../src/tools.ts#L872-L894) | `/receive` copies; the files of a chat |
| `leaveChat` | [917](../src/tools.ts#L917) | perform step of `leave` and `/leave` |
| `download`, `fileServers`, `unknownServers` | [924-970](../src/tools.ts#L924-L970) | the `download` call before its action |
| `startDownload`, `checkDownload`, `currentFile`, `settleFile`, `settleReceived`, `moveReceived` | [988-1050](../src/tools.ts#L988-L1050) | a download's steps |
| `agentChats`, `knownChat` | [1119-1136](../src/tools.ts#L1119-L1136) | the agent's chats; a checked chat reference |
| `classifyPlan` | [1219](../src/tools.ts#L1219) | `apiConnectPlan` response to a join plan or a known chat |
| `chatSubject`, `isConnected`, `isActive`, `memberCurrent` | [1315-1368](../src/tools.ts#L1315-L1368) | chat and member predicates shared with `control.ts` |
| `messageLength`, `quoteContent` | [1392-1408](../src/tools.ts#L1392-L1408) | the message as the core sends it |
| `realFile` | [1424](../src/tools.ts#L1424) | real path, size, inode and change time of a file |
| `attachmentMedia`, `videoFrame`, `videoDuration` | [1465-1499](../src/tools.ts#L1465-L1499) | previews and durations |
| `toolCall` | [1515](../src/tools.ts#L1515) | `ServerContext` to `ToolCall`: abort signal and progress notifications |

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

Hooks are bound in [`serve.ts:100-106`](../src/serve.ts#L100-L106): `refusal`, `saveCounter`, `postCard` and `outcome` to `Control`; `perform` to `AgentTools`.

### [2.6 `control.ts`](../src/control.ts)

| Symbol | Line | Contents |
|---|---|---|
| `controlCommands` | [10](../src/control.ts#L10) | the command menu |
| `standings` | [22](../src/control.ts#L22) | custom data key, tool and reply words of the approvals of messages and files |
| `ControlData` | [43](../src/control.ts#L43) | `{control: true, nextAction}` |
| `ChatApproval` | [60](../src/control.ts#L60) | `{approvedBy, approvedById, approvedAt}` |
| `OwnerRequest` | [66](../src/control.ts#L66) | parsed owner command |
| `Control.load` | [101](../src/control.ts#L101) | lost check, group profile and menu, member list |
| `Control.start` | [110](../src/control.ts#L110) | owner setup per `--owners` |
| `Control.refusal` | [118](../src/control.ts#L118) | bytes left for the link texts of a connect card, card size of a send, connect or download, approver presence |
| `Control.standingApprover` | [129](../src/control.ts#L129) | approver name of a chat's standing approval of messages or files |
| `Control.saveCounter` | [134](../src/control.ts#L134) | queued counter writes |
| `Control.postCard`, `Control.outcome` | [140-151](../src/control.ts#L140-L151) | card post and card edit; the kept-files message after a done leave |
| `Control.refusedFile`, `Control.fileNotSent` | [153-168](../src/control.ts#L153-L168) | posts about files |
| `Control.onMessage` | [170](../src/control.ts#L170) | owner commands, queued one at a time |
| `Control.onEvent` | [182](../src/control.ts#L182) | membership changes and loss of the control chat |
| `Control.ownerCommand` | [199](../src/control.ts#L199) | command dispatch and replies |
| `approveChat`, `revoke`, `receive`, `leave`, `deleteFiles` | [236-300](../src/control.ts#L236-L300) | owner requests |
| `cardContent`, `fittedImage` | [434-454](../src/control.ts#L434-L454) | image and video cards with the file's preview |
| `findControlGroup`, `createControlChat`, `resetControlChat` | [492-520](../src/control.ts#L492-L520) | control group lookup, creation, reset |
| `isControlEvent` | [522](../src/control.ts#L522) | membership events of the control group |
| `parseCommand` | [538](../src/control.ts#L538) | one-line `/keyword params` |
| `cardText`, `requestLines`, `commandsLine` | [545-609](../src/control.ts#L545-L609) | card text per tool, state and mode |
| `largestCard`, `encodedLength` | [615-623](../src/control.ts#L615-L623) | card size check |
| `serversLine`, `sizeText` | [657-666](../src/control.ts#L657-L666) | servers line and sizes of download cards |
| `codeSpan`, `safeName`, `mentionText` | [676-688](../src/control.ts#L676-L688) | quoting of peer text |
| `ownerRequest` | [703](../src/control.ts#L703) | command words to an `OwnerRequest` |
| `qrCodeText` | [718](../src/control.ts#L718) | terminal QR code |
| `losesControl`, `isLost` | [734-749](../src/control.ts#L734-L749) | loss of the control chat |
| `entryText`, `restartText` | [767-777](../src/control.ts#L767-L777) | `--owners` entries in restart lines |

### [2.7 `messages.ts`](../src/messages.ts)

| Symbol | Line | Contents |
|---|---|---|
| `Message`, `MessageFile`, `FileOffer` | [15-47](../src/messages.ts#L15-L47) | message item of tool results; its file; the file of a download |
| `ToolCall` | [54](../src/messages.ts#L54) | abort signal and progress callback of one call |
| `Collected` | [59](../src/messages.ts#L59) | collected items, change signal, IDs of the items over the first 100, time of the last change |
| `Changes` | [66](../src/messages.ts#L66) | set of wake callbacks |
| `ChatItemsWatch` | [83](../src/messages.ts#L83) | listeners per chat reference; received message items gathered by `collect` |
| `chatRef`, `anyScopeChatRef`, `isChatRef` | [132-149](../src/messages.ts#L132-L149) | `@N` and `#N` references: of the main conversation, or also of a group's member chat |
| `toMessage`, `messageFile`, `receivedFileError` | [171-206](../src/messages.ts#L171-L206) | chat item to `Message` |
| `downloadPath`, `chatFolder` | [208-215](../src/messages.ts#L208-L215) | `DATA/files/group-N` or `DATA/files/contact-N`, and the file name in it |
| `fileOffer`, `fileContent` | [217-242](../src/messages.ts#L217-L242) | file fields of an item; message content of a file |
| `getChat`, `markRead` | [259-265](../src/messages.ts#L259-L265) | `apiGetChat` and `apiChatItemsRead` by chat reference |
| `unreadItems` | [267](../src/messages.ts#L267) | unread items of a chat after its `initial` read, paged |
| `reportProgress`, `nextChange`, `awaitResponses` | [283-313](../src/messages.ts#L283-L313) | waits |
| `validName`, `viewName` | [319-345](../src/messages.ts#L319-L345) | display name rules and `@name` form |

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

---

## 3. State

### 3.1 Memory

| Item | Holder | Written by | Read by |
|---|---|---|---|
| Actions | `Actions.actions` map ([actions.ts:128](../src/actions.ts#L128)) | `create`, `remove`, `expireDue` | tools, `Control` |
| Open calls of an action | `ActionRecord.open` ([:44](../src/actions.ts#L44)) | `actionResult` ([tools.ts:611](../src/tools.ts#L611), [:620](../src/tools.ts#L620)) | `list`, `expireDue` |
| Collected replies | `ConnectAction.messages`, `SendAction.responses` ([:56](../src/actions.ts#L56), [:87](../src/actions.ts#L87)) | `ChatItemsWatch.collect` | `connect`, `send` results |
| Item listeners | `ChatItemsWatch.listeners` ([messages.ts:84](../src/messages.ts#L84)) | `collect` | event dispatch |
| Downloads | `AgentTools.downloads`, one per file ID ([tools.ts:474](../src/tools.ts#L474)) | `downloadFile` | file events, checks, `/delete CHAT files` |
| Rejected files | `AgentTools.rejectedFiles` ([tools.ts:475](../src/tools.ts#L475)) | `downloadResult` ([:977-978](../src/tools.ts#L977-L978)) | the file field |
| Count of file descriptions | `AgentTools.describedFiles` ([tools.ts:476-477](../src/tools.ts#L476-L477)) | `onFileDescription` | `fileServers` |
| Members of the control chat | `Control.members` ([control.ts:81](../src/control.ts#L81)) | `refreshMembers` ([:456-458](../src/control.ts#L456-L458)) | approver checks |
| Mode, expiry, owners list, stop wait | `ServeOptions` of the run | CLI | all modules |

### 3.2 Chat database custom data

| Item | Entity | Value | Written by | Read by |
|---|---|---|---|---|
| Control marker and counter | agent control chat | `{simplexMcp: {control: true, nextAction: N}}` | [`createControlChat`](../src/control.ts#L502-L513), [`saveCounter`](../src/control.ts#L134) | [`findControlGroup`](../src/control.ts#L492), [`controlDataOf`](../src/control.ts#L497) |
| Standing approval of messages | contact or group of the agent | `{...other keys, simplexMcp: {approvedBy, approvedById, approvedAt}}` | [`approveChat`](../src/control.ts#L236), [`revoke`](../src/control.ts#L253) | [`standingApprover`](../src/control.ts#L129) |
| Files approval | contact or group of the agent | `{...other keys, simplexMcpFiles: {approvedBy, approvedById, approvedAt}}` | [`approveChat`](../src/control.ts#L236), [`revoke`](../src/control.ts#L253) | [`standingApprover`](../src/control.ts#L129) |

The profile, chats, items, members and file transfers are stored by the core in `DATA/simplex_chat.db`; connections and queues in `DATA/simplex_agent.db` (file prefix `DATA/simplex`, [serve.ts:72](../src/serve.ts#L72)).

### 3.3 Files

| Path | Contents | Mode | Written | Read |
|---|---|---|---|---|
| `DATA/` | data folder | 0700, set at every start except on Windows | [files.ts:12-15](../src/files.ts#L12-L15) | |
| `DATA/token` | base64url of 32 random bytes | 0600, set again at every write except on Windows | first start, `--new-token` ([:30-36](../src/files.ts#L30-L36)) | every start |
| `DATA/headers.json` | `{"Authorization":"Bearer <token>"}` | 0600, set again at every write except on Windows | every start ([:42-44](../src/files.ts#L42-L44)) | Claude Code `headersHelper` |
| `DATA/server.json` | `{name, port, expireHours}` | 0600, set again at every write except on Windows | every start, after the HTTP listener is bound ([serve.ts:118](../src/serve.ts#L118)) | `register` |
| `DATA/lock` | PID and a newline | 0600 | start ([files.ts:64-72](../src/files.ts#L64-L72)) | next start |
| `DATA/tmp/` | the core's temp folder: files in transfer | 0700, set at every start | [serve.ts:93](../src/serve.ts#L93), [:205-209](../src/serve.ts#L205-L209) | the core |
| `DATA/files/group-N/`, `DATA/files/contact-N/` | downloaded files and files received without a download, named by file ID and extension | folder 0700 | the core at the download path ([tools.ts:992-993](../src/tools.ts#L992-L993)); moves ([:1453-1463](../src/tools.ts#L1453-L1463)) | `download` results, `/receive`, `/leave`, `/delete CHAT files` |
| `DATA/files/sent/` | resized copies of sent images, and copies of the files `/receive` posts, one folder per copy | folder 0700; a resized copy 0600 | [tools.ts:859-876](../src/tools.ts#L859-L876), [:896-900](../src/tools.ts#L896-L900) | the core; deleted at `sndFileCompleteXFTP`, `sndFileError` and at start ([serve.ts:94](../src/serve.ts#L94), [:156-160](../src/serve.ts#L156-L160)) |

The core library is downloaded once into `CACHE/core-7.1.0.18/sqlite`, and the Node.js add-on into `CACHE/nodejs-addon-cce06d8/`, each with a `Downloading ...` line on stderr. CACHE is `$XDG_CACHE_HOME/simplex-chat` or `~/.cache/simplex-chat`; on macOS `~/Library/Caches/simplex-chat`; on Windows `%LOCALAPPDATA%\simplex-chat` ([libs.ts:41-48](../../simplex-chat-nodejs/src/libs.ts#L41-L48), [:89](../../simplex-chat-nodejs/src/libs.ts#L89), [:109](../../simplex-chat-nodejs/src/libs.ts#L109), [:116](../../simplex-chat-nodejs/src/libs.ts#L116), [:129](../../simplex-chat-nodejs/src/libs.ts#L129)).

---

## 4. Process lifecycle

### 4.1 Start

1. Data folder, lock, token and headers file are prepared ([serve.ts:67-71](../src/serve.ts#L67-L71)).
2. libsimplex for SQLite is loaded by `ChatApi.init`, and the databases are migrated with confirmation `yesUp` ([serve.ts:72](../src/serve.ts#L72), [api.ts:117-126](../../simplex-chat-nodejs/src/api.ts#L117-L126)).
3. Three log lines: version and NAME, data folder, mode ([serve.ts:73-75](../src/serve.ts#L73-L75)).
4. The active user is read, or created with display name `--display-name` or NAME, empty full name and peer type `agent` ([serve.ts:90](../src/serve.ts#L90)).
5. The network config is read and set with `smpProxyMode` `always` and `smpProxyFallback` `allowProtected` ([:91-92](../src/serve.ts#L91-L92)).
6. `DATA/tmp` is set as the core's temp folder, and `DATA/files/sent` is deleted ([:93-94](../src/serve.ts#L93-L94)).
7. Events are buffered, and the chat is started ([:95-96](../src/serve.ts#L95-L96)).
8. A `--display-name` different from the profile, or a peer type other than `agent`, is applied with one `apiUpdateProfile` ([:97](../src/serve.ts#L97), [:211-216](../src/serve.ts#L211-L216)).
9. The agent control chat is found by its marker, created with counter 1, or reset with `--reset-owners` ([:99](../src/serve.ts#L99), [:218-223](../src/serve.ts#L218-L223)).
10. `Actions`, `ChatItemsWatch`, `AgentTools` and `Control` are created; the counter is taken from the marker ([:100-109](../src/serve.ts#L100-L109)).
11. `Control.load`: for a lost control chat, `Actions.lost` is set; otherwise the group name and command menu are updated when they differ, and members are read ([control.ts:101-108](../src/control.ts#L101-L108)).
12. The HTTP server listens; with the port in use, the process exits with code 1 ([serve.ts:117](../src/serve.ts#L117), [:225-232](../src/serve.ts#L225-L232)). Until step 14 ends, requests to `/mcp` that pass the token check get 503 with `Retry-After: 1` ([http.ts:23](../src/http.ts#L23), [serve.ts:116-117](../src/serve.ts#L116-L117), [:125](../src/serve.ts#L125)).
13. `server.json` is written ([serve.ts:118](../src/serve.ts#L118)).
14. The server runs the owner setup, `Control.start`; after an error, HTTP is closed and the error is rethrown ([serve.ts:119-124](../src/serve.ts#L119-L124)).
15. `Listening on http://127.0.0.1:PORT/mcp` ([:126](../src/serve.ts#L126)).
16. Buffered events are replayed, then live events are dispatched ([:129](../src/serve.ts#L129), [:234-248](../src/serve.ts#L234-L248)).

After an error inside `startAgent` (steps 4-16) the chat is closed; after any error once the lock is taken, the lock is released ([serve.ts:69-85](../src/serve.ts#L69-L85)).

### 4.2 Event dispatch

Each `onAny` receiver is awaited by the library before the next event is read ([api.ts:195-202](../../simplex-chat-nodejs/src/api.ts#L195-L202)). The dispatcher in [`serve.ts:129-165`](../src/serve.ts#L129-L165):

| Event | Condition | Handler |
|---|---|---|
| `newChatItems` | item of the control group, main scope | [`Control.onMessage`](../src/control.ts#L170) |
| `newChatItems` | every item | [`AgentTools.onNewItems`](../src/tools.ts#L525), then [`ChatItemsWatch.deliver`](../src/messages.ts#L113) |
| `chatItemUpdated` | | [`AgentTools.onFile`](../src/tools.ts#L535), then [`ChatItemsWatch.update`](../src/messages.ts#L117) |
| `chatItemsDeleted` | | [`ChatItemsWatch.remove`](../src/messages.ts#L121) of each `deletedChatItem` |
| `contactConnected` | | [`AgentTools.onContactConnected`](../src/tools.ts#L564) |
| `userJoinedGroup` | | [`AgentTools.onUserJoinedGroup`](../src/tools.ts#L569) |
| `rcvFileComplete` | | [`AgentTools.onFileComplete`](../src/tools.ts#L540) |
| `rcvFileSndCancelled` | | [`AgentTools.onFileCancelled`](../src/tools.ts#L552) |
| `rcvFileError` | | [`AgentTools.onFileError`](../src/tools.ts#L547) |
| `rcvFileDescrReady` | | [`AgentTools.onFileDescription`](../src/tools.ts#L559) |
| `sndFileCompleteXFTP` | | [`AgentTools.onSentFile`](../src/tools.ts#L822) |
| `sndFileError` | | [`AgentTools.onSentFile`](../src/tools.ts#L822), then [`Control.fileNotSent`](../src/control.ts#L159) |
| `joinedGroupMember`<br>`joinedGroupMemberConnecting`<br>`connectedToGroupMember`<br>`memberRole`<br>`leftMember`<br>`deletedMember`<br>`deletedMemberUser`<br>`groupDeleted` | event of the control group | [`Control.onEvent`](../src/control.ts#L182), queued one at a time |
| any event | after `stop()` started | ignored |

Owner commands are queued in a second chain, independent of the membership chain ([control.ts:176-179](../src/control.ts#L176-L179)).

### 4.3 Stop

On `SIGINT` or `SIGTERM` the server runs one stop; a later signal ends its wait for cancelled cards ([serve.ts:47-63](../src/serve.ts#L47-L63)). `RunningServer.stop` ([serve.ts:171-181](../src/serve.ts#L171-L181)):

1. Events are ignored from now on.
2. `Actions.stop` starts: new actions are refused with `The server is stopping.`; the expiry timer is cleared; cards being posted are awaited; pending and performing actions are marked interrupted; the stop signal of the performs is aborted, which ends the wait of a `connect` for its connection; their cards are edited, or posted when they have no card; outcome edits already in progress are awaited ([actions.ts:240-250](../src/actions.ts#L240-L250)).
3. In parallel with step 2, the HTTP server is closed with all connections, and open calls are aborted ([http.ts:48-52](../src/http.ts#L48-L52)).
4. `Cancelled K action(s).` when K > 0 ([serve.ts:176](../src/serve.ts#L176)).
5. When cards were edited or posted at the stop and the control chat is not lost, the server waits `--stop-wait` seconds, 10 by default, for them to be sent; a second signal ends the wait ([serve.ts:177](../src/serve.ts#L177), [:189-203](../src/serve.ts#L189-L203)).
6. `chat.close()`: the chat is stopped, then the store is closed ([api.ts:165-170](../../simplex-chat-nodejs/src/api.ts#L165-L170)).
7. The lock is removed, `Stopped.` is logged, and the process exits with code 0, or 1 after an error ([serve.ts:179-180](../src/serve.ts#L179-L180), [:53-59](../src/serve.ts#L53-L59)).

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

The mode is set by `--approve` for one run ([cli.ts:76-78](../src/cli.ts#L76-L78)).

| Mode | State of a new action | Card | `_meta` of `connect`, `send`, `download`, `leave` |
|---|---|---|---|
| `requests` | `pending`<br>`performing` for a send under a standing approval, and for a download under a files approval | card with commands<br>outcome as a new message after a standing send; none after a download without a card | none |
| `host` | `performing` | outcome as a new message after the perform; none for a download | `{"anthropic/requiresUserInteraction": true}` |

Sources: [actions.ts:166](../src/actions.ts#L166), [tools.ts:487](../src/tools.ts#L487), [control.ts:144-151](../src/control.ts#L144-L151), [:488-490](../src/control.ts#L488-L490). In both modes a new action is refused unless the control chat has an approver ([control.ts:123-126](../src/control.ts#L123-L126)).

### 5.2 Owners and approvers

| Term | Rule | Code |
|---|---|---|
| human | member other than the agent's membership, status `connected` or `complete` | [control.ts:477-479](../src/control.ts#L477-L479), [tools.ts:1353-1355](../src/tools.ts#L1353-L1355) |
| human owner | human with role `owner` | [control.ts:464-466](../src/control.ts#L464-L466) |
| approver, `--owners` absent | member with role `owner` | [control.ts:481-485](../src/control.ts#L481-L485) |
| approver, `--owners` without a list | none | [control.ts:484](../src/control.ts#L484) |
| approver, `--owners ID:NAME,...` | member with role `owner` whose `groupMemberId` and `displayName`, folded to one line by `oneLine`, match an entry | [control.ts:484](../src/control.ts#L484), [:751-753](../src/control.ts#L751-L753) |

The command sender's member record from the chat item is checked ([control.ts:175](../src/control.ts#L175), [:203](../src/control.ts#L203)). The approver count for refusals and standing approvals is taken from `Control.members`, refreshed at load and at each membership event ([control.ts:186](../src/control.ts#L186), [:456-458](../src/control.ts#L456-L458)).

Owner setup at start ([control.ts:110-116](../src/control.ts#L110-L116)):

| `--owners` | Start | First join by the link |
|---|---|---|
| absent | a human owner exists: register hint<br>otherwise: `No owners in the agent control chat.` when humans exist, then a link with role `member` | link deleted, role `owner`, greeting, log line, register hint |
| without a list | no humans: link with role `observer`<br>humans exist: their IDs and the restart command, in the log and the chat | link deleted, the joined member and the restart command announced |
| `ID:NAME,...` | each entry matched to a human, or exit 2<br>role `owner` set where missing<br>other humans removed<br>link deleted<br>`Owners: ...` and register hint | |

Sources: [`startOwners`](../src/control.ts#L334), [`startObserver`](../src/control.ts#L340), [`startListed`](../src/control.ts#L345), [`ownerJoined`](../src/control.ts#L364), [`observerJoined`](../src/control.ts#L373).

Membership changes at run time ([control.ts:182-197](../src/control.ts#L182-L197)):

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
| `/reject`, or `/leave CHAT` for a pending action of CHAT with a card | `pending` | `rejected` | [actions.ts:225-230](../src/actions.ts#L225-L230), [control.ts:284-286](../src/control.ts#L284-L286) |
| expiry time passes | `pending` | `expired` | [actions.ts:196-199](../src/actions.ts#L196-L199) |
| `/approve N`, `/approve CHAT N`, `/approve CHAT all`, `/approve CHAT files` | `pending` | `performing` | [actions.ts:219-223](../src/actions.ts#L219-L223) |
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
  pending --> performing: /approve
  pending --> rejected: /reject, /leave CHAT
  pending --> expired: expiry
  pending --> failed: counter or card fails, control chat lost
  performing --> done: perform ends
  performing --> failed: perform throws, counter fails
  performing --> pending: stop during the counter write
```

An action is removed from memory when its result is returned to a call ([tools.ts:617](../src/tools.ts#L617)), or at an expiry tick when it is decided, its open call count is 0 and its expiry time has passed ([actions.ts:200-201](../src/actions.ts#L200-L201)). A `download` still performing at the end of `wait` is returned as `performing` and kept ([tools.ts:614](../src/tools.ts#L614)).

### 5.4 Cards

The server posts a card when a `pending` action is created, and edits it with `apiUpdateChatItem` at the outcome ([actions.ts:255](../src/actions.ts#L255), [control.ts:144-151](../src/control.ts#L144-L151)). For a `connect`, `send` or `leave` without a card (mode `host`, standing sends) the server posts the outcome as a new message; a `download` without a card gets none ([control.ts:488-490](../src/control.ts#L488-L490)). The text per state is built by [`cardText`](../src/control.ts#L545-L564); a card of an image or video file is an image or video message with the file's preview ([control.ts:434-454](../src/control.ts#L434-L454)); forms are listed in [API: cards](api.md#22-cards).

Outcomes are posted after `done` and `failed` ([actions.ts:282](../src/actions.ts#L282)), after `rejected` and `expired` ([:229](../src/actions.ts#L229), [:199](../src/actions.ts#L199)), and at stop ([:247](../src/actions.ts#L247)). They are skipped after the control chat is lost ([:247](../src/actions.ts#L247), [:282](../src/actions.ts#L282), [:291](../src/actions.ts#L291)).

### 5.5 Standing approvals

| Aspect | Messages | Files | Code |
|---|---|---|---|
| grant | `/approve CHAT all` from an approver in mode `requests`; pending sends of CHAT with a card are approved | `/approve CHAT files` from an approver in mode `requests`; pending downloads of CHAT with a card are approved, except those of files on unknown servers | [control.ts:236-251](../src/control.ts#L236-L251) |
| mode `host` | nothing stored; reply `Approvals apply in mode requests; this server runs in mode host.` | the same | [control.ts:212](../src/control.ts#L212) |
| removal | `/revoke CHAT`: the `simplexMcp` key is removed, other keys kept | `/revoke CHAT files`: the `simplexMcpFiles` key is removed | [control.ts:253-260](../src/control.ts#L253-L260) |
| use | mode `requests`, and `approvedById` equal to the `groupMemberId` of a current approver | the same | [control.ts:129-132](../src/control.ts#L129-L132) |
| scope | `send` | `download` of a file whose servers are all known | [tools.ts:770](../src/tools.ts#L770), [:939](../src/tools.ts#L939) |
| decider shown | `safeName(approvedBy)` | the same | [control.ts:131](../src/control.ts#L131) |
| storage | the chat's custom data; removed with the chat by `leave` | the same | [tools.ts:917-922](../src/tools.ts#L917-L922) |

### 5.6 Expiry

| Rule | Code |
|---|---|
| `expires = created + expireHours × 3600000`, `expireHours` 1 to 23, default 23 | [actions.ts:163](../src/actions.ts#L163), [cli.ts:59](../src/cli.ts#L59), [:82-84](../src/cli.ts#L82-L84) |
| check every 60 s on an unreferenced timer, and before each owner command | [actions.ts:129](../src/actions.ts#L129), [control.ts:200](../src/control.ts#L200) |
| pending and due: `expired`, log `Action N expired.`, card edited to `Expired.` | [actions.ts:196-199](../src/actions.ts#L196-L199) |
| decided, no open call, due: removed from memory | [actions.ts:200-201](../src/actions.ts#L200-L201) |
| connect wait for the other side: at most until `created + 24 h - 10 min` | [tools.ts:139](../src/tools.ts#L139), [:663](../src/tools.ts#L663) |

`Date.now()` is compared with `expires`, so due actions are expired at the first tick after a sleep of the computer.

### 5.7 Action counter

| Rule | Code |
|---|---|
| initial value: `nextAction` of the control marker, or 1 | [serve.ts:100](../src/serve.ts#L100) |
| a new action takes `next`, then `next` grows by 1 | [actions.ts:156](../src/actions.ts#L156), [:178](../src/actions.ts#L178) |
| the new `next` is written before the card is posted | [actions.ts:254-255](../src/actions.ts#L254-L255) |
| one write at a time, also after a failed write | [control.ts:134-138](../src/control.ts#L134-L138) |
| `--reset-owners` copies the value into the new control chat | [serve.ts:221-222](../src/serve.ts#L221-L222), [control.ts:515-520](../src/control.ts#L515-L520) |
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
| card and reply names | folded to one line; a valid display name is plain, others a code span | [`safeName`](../src/control.ts#L680-L683) |
| card text lines | one code span per line; backticks become `'`; a leading or trailing space becomes U+00A0; blank lines empty | [`codeLines`, `codeSpan`](../src/control.ts#L672-L678) |
| file names and paths on cards and posts | folded to one line, one code span | [control.ts:578](../src/control.ts#L578), [:591](../src/control.ts#L591), [:165-167](../src/control.ts#L165-L167) |
| reply quote on a send card | whitespace runs to one space, first 100 characters (code points), one code span | [control.ts:579](../src/control.ts#L579) |
| link texts on a connect card | code span lines in the bytes that the largest card form leaves; the line that does not fit is cut and ends in `…` inside its code span; later lines left out | [control.ts:637-651](../src/control.ts#L637-L651) |
| mention names | valid name: `@name` or `@'name'`; others a code span | [`mentionText`](../src/control.ts#L685-L688) |
| errors on cards | folded to one line, one code span | [control.ts:558](../src/control.ts#L558) |
| `--owners` part of restart lines in the chat | one code span | [control.ts:78](../src/control.ts#L78), [:775-777](../src/control.ts#L775-L777) |
| terminal names and errors of actions | folded to one line | [actions.ts:180-181](../src/actions.ts#L180-L181), [:333-341](../src/actions.ts#L333-L341), [control.ts:77](../src/control.ts#L77) |
| tool results | returned as data; schema descriptions and instructions mark `text`, `from`, `name`, file names, downloaded files, `description`, `welcome` and `commands` with their labels and params as data, not instructions; the instructions take a chat's messages as prompts only when the user said so in the session, and limit what the agent shares about its user | [tools.ts:179](../src/tools.ts#L179), [:183](../src/tools.ts#L183), [:188](../src/tools.ts#L188), [:213-214](../src/tools.ts#L213-L214), [:224-229](../src/tools.ts#L224-L229), [:392](../src/tools.ts#L392), [:434](../src/tools.ts#L434) |

The server writes the tappable form `/'...'` only in the card's command line ([control.ts:596-609](../src/control.ts#L596-L609)), in `/'revoke CHAT'` of standing cards and replies ([:556](../src/control.ts#L556), [:250](../src/control.ts#L250)), and in `/'delete CHAT files'` of the kept-files message ([:294](../src/control.ts#L294)).

### 6.4 Chat references

| Rule | Code |
|---|---|
| `^[@#][1-9]\d{0,14}$`: 1 to 15 digits; the core reads `#(2^64 + N)` as `#N` | [messages.ts:147-149](../src/messages.ts#L147-L149), [scenarios.test.ts:491-503](../test/e2e/scenarios.test.ts#L491-L503) |
| the control chat is refused by `send`, `receive`, `download`, `leave` and the owner commands with CHAT, and left out of `list` and `/list` | [tools.ts:1075](../src/tools.ts#L1075), [:1119-1136](../src/tools.ts#L1119-L1136), [control.ts:321-328](../src/control.ts#L321-L328) |
| a store error of the chat read is reported as `No chat CHAT.` | [tools.ts:1080](../src/tools.ts#L1080), [:1134](../src/tools.ts#L1134) |
| the agent's own links are refused by `connect` (`ownLink` plans) | [tools.ts:1238](../src/tools.ts#L1238), [:1256](../src/tools.ts#L1256), [:1278](../src/tools.ts#L1278) |

### 6.5 Owner commands

| Rule | Code |
|---|---|
| source: `groupRcv` text items in the main scope of the control group | [serve.ts:134](../src/serve.ts#L134), [control.ts:172](../src/control.ts#L172) |
| form: one line `/keyword params`, leading spaces allowed | [control.ts:538-543](../src/control.ts#L538-L543) |
| the help reply is sent to any sender; other commands are accepted from approvers only | [control.ts:202-203](../src/control.ts#L202-L203) |
| commands run one at a time in arrival order | [control.ts:176-179](../src/control.ts#L176-L179) |
| `/approve`, `/reject` and `/receive` need a pending action with a card, and the action's chat when CHAT is given | [control.ts:224-225](../src/control.ts#L224-L225) |

### 6.6 Files

| Rule | Code |
|---|---|
| `send` refuses files named `id_*`, `*.pem`, `*.key`, `*.p12` or `*.pfx`, and files under `~/.ssh`, `~/.gnupg`, the data folder and `~/.simplex-mcp` except the agent's own `DATA/files/`, each by its real path; the refusal is posted to the control chat and logged | [tools.ts:836-850](../src/tools.ts#L836-L850), [:1424-1443](../src/tools.ts#L1424-L1443), [control.ts:153-157](../src/control.ts#L153-L157) |
| the file of a `send` is sent only when its real path, size, inode and change time at the perform are those of the request | [tools.ts:852-857](../src/tools.ts#L852-L857) |
| a sent JPEG or PNG is a resized copy made by the core | [tools.ts:859-870](../src/tools.ts#L859-L870) |
| `ffmpeg` and `ffprobe` read a video as MP4 or QuickTime, local files only, without standard input, 30 s and 64 MiB of output each | [tools.ts:1487-1505](../src/tools.ts#L1487-L1505) |
| a download is saved only at `DATA/files/group-N/` or `DATA/files/contact-N/`, named by its file ID and an extension of 1 to 16 ASCII letters and digits | [messages.ts:208-215](../src/messages.ts#L208-L215) |
| a file on unknown servers gets a card that names them, also under a files approval; in mode `host` the core downloads it only when the IP address is protected | [tools.ts:939](../src/tools.ts#L939), [:993](../src/tools.ts#L993), [control.ts:604-607](../src/control.ts#L604-L607) |
| a file received without a download is moved into the chat's folder at its completion, when it has its full size; a partial file is deleted only for a download in memory, when it is no larger than the file | [tools.ts:1041-1050](../src/tools.ts#L1041-L1050), [:1445-1451](../src/tools.ts#L1445-L1451) |

### 6.7 Link and network

- The join link is a group link of the control chat with role `member`, or `observer` with `--owners`; it is printed with a QR code on stdout and deleted at the first join event ([control.ts:399-409](../src/control.ts#L399-L409), [:365](../src/control.ts#L365), [:374](../src/control.ts#L374)).
- At every start the server sets private message routing: `smpProxyMode` `always`, `smpProxyFallback` `allowProtected` ([serve.ts:91-92](../src/serve.ts#L91-L92)).
- With `incognito: true` the join methods connect incognito ([tools.ts:695](../src/tools.ts#L695)); the core then uses a random profile for that chat ([PLAN, Participants](../PLAN.md#participants)).

---

## 7. Limits

| Limit | Value | Code |
|---|---|---|
| pending actions | 20 | [actions.ts:11](../src/actions.ts#L11) |
| actions being performed | 20 | [actions.ts:12](../src/actions.ts#L12) |
| expiry | 1 to 23 h, default 23 | [cli.ts:59](../src/cli.ts#L59), [:82-84](../src/cli.ts#L82-L84) |
| connect wait for the other side | `created + 23 h 50 min` | [tools.ts:139](../src/tools.ts#L139) |
| send, connect and download card | 15000 bytes JSON-escaped, in its largest form | [control.ts:26](../src/control.ts#L26), [:615-623](../src/control.ts#L615-L623) |
| error assumed in the largest card | 600 × `€` | [control.ts:30](../src/control.ts#L30) |
| message as the core sends it | 15602 bytes, counted with 600 bytes of envelope | [tools.ts:149-150](../src/tools.ts#L149-L150), [:1392-1395](../src/tools.ts#L1392-L1395) |
| `send.text` | 1 to 12000 characters | [tools.ts:251](../src/tools.ts#L251) |
| `connect.message` | 1 to 2000 characters | [tools.ts:239](../src/tools.ts#L239) |
| `send.mention` | 1 to 3 unique member IDs | [tools.ts:255](../src/tools.ts#L255) |
| `wait` | 0 to 86400 s | [tools.ts:240](../src/tools.ts#L240), [:256](../src/tools.ts#L256), [:268](../src/tools.ts#L268), [:291](../src/tools.ts#L291) |
| `idle` | 1 to 300 s, default 30 | [tools.ts:241](../src/tools.ts#L241), [:257](../src/tools.ts#L257), [:145](../src/tools.ts#L145) |
| `limit` | 1 to 100, default 50 | [tools.ts:258](../src/tools.ts#L258), [:269](../src/tools.ts#L269), [:146](../src/tools.ts#L146) |
| `connect` snapshot | last 50 items | [tools.ts:147](../src/tools.ts#L147) |
| collected items per action or call | 100, then their IDs kept as overflow until deleted | [messages.ts:10](../src/messages.ts#L10), [:106-107](../src/messages.ts#L106-L107) |
| unread page | 100 items | [messages.ts:9](../src/messages.ts#L9) |
| chat list read | one read of 1000000 chats | [tools.ts:138](../src/tools.ts#L138) |
| `download` wait, default | 60 s | [tools.ts:142](../src/tools.ts#L142) |
| download check | every 60 s | [tools.ts:143](../src/tools.ts#L143) |
| download without progress | 30 min, then cancelled | [tools.ts:144](../src/tools.ts#L144) |
| `ffmpeg`, `ffprobe` | 30 s, 64 MiB of output | [tools.ts:148](../src/tools.ts#L148), [:152](../src/tools.ts#L152) |
| stop wait | 0 to 60 s, default 10 | [cli.ts:64](../src/cli.ts#L64), [:85-87](../src/cli.ts#L85-L87) |
| error details | 500 characters | [log.ts:4](../src/log.ts#L4) |
| progress interval | 60 s | [messages.ts:7](../src/messages.ts#L7) |
| expiry tick | 60 s | [actions.ts:10](../src/actions.ts#L10) |
| HTTP body | 4 MiB | [server index.cjs:254](../node_modules/@modelcontextprotocol/server/dist/index.cjs#L254) |
| SSE keep-alive | 15 s | [mcp-BumlNux0.cjs:64](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L64) |
| agent NAME | 1 to 32 characters | [cli.ts:150](../src/cli.ts#L150) |
| display name | 50 characters, `mkValidName` rules | [messages.ts:11-12](../src/messages.ts#L11-L12), [:319-341](../src/messages.ts#L319-L341) |
| port | 1 to 65535, default 5226 | [cli.ts:62](../src/cli.ts#L62), [:94-96](../src/cli.ts#L94-L96) |
