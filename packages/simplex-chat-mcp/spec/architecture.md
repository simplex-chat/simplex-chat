# SimpleX MCP server: architecture

> System structure, modules, state, process lifecycle, approval model and security boundaries of `@simplex-chat/mcp` 0.2.0.
>
> Related specs: [README](README.md) | [Flows](flows.md) | [API](api.md)
> Related docs: [README](../README.md) | [Reference](../REFERENCE.md) | [PLAN](../PLAN.md)

**Source:** [`cli.ts`](../src/cli.ts) | [`serve.ts`](../src/serve.ts) | [`http.ts`](../src/http.ts) | [`tools.ts`](../src/tools.ts) | [`actions.ts`](../src/actions.ts) | [`control.ts`](../src/control.ts) | [`messages.ts`](../src/messages.ts) | [`audio.ts`](../src/audio.ts) | [`files.ts`](../src/files.ts) | [`register.ts`](../src/register.ts) | [`log.ts`](../src/log.ts)

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
    audio["audio.ts<br/>speech-to-text<br/>text-to-speech"]
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
  tools --> audio
  audio -->|"HTTP<br/>/v1/audio/transcriptions<br/>/v1/audio/speech"| api["OpenAI-compatible<br/>audio API, --audio"]
  serve --> control
  serve --> tools
  serve --> messages
  serve --> files
  files --> data[("DATA:<br/>token<br/>headers.json<br/>server.json<br/>lock")]
  tools --> folder[("DATA/files:<br/>group-N, contact-N<br/>transcripts<br/>sent")]
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
4. The handler result is returned by `AgentTools.call` as `structuredContent` and as JSON text ([`tools.ts:515-522`](../src/tools.ts#L515-L522)).
5. The server calls `ChatApi` methods of the core ([`messages.ts:268-284`](../src/messages.ts#L268-L284)); actions are created in `Actions` and shown to the owner by `Control` ([`actions.ts:155-186`](../src/actions.ts#L155-L186)).
6. The core emits events; the server dispatches them in one handler of `serve.ts` to the tools, the item watch and the agent control chat ([`serve.ts:132-168`](../src/serve.ts#L132-L168)).

---

## 2. Modules

### [2.1 `cli.ts`](../src/cli.ts)

Entry point of the `simplex-mcp` binary.

| Symbol | Line | Contents |
|---|---|---|
| `helpText` | [11](../src/cli.ts#L11) | `--help` output |
| `Command` | [45](../src/cli.ts#L45) | `help`, `serve` with `ServeOptions`, `register` with `RegisterOptions` |
| `parseCommand` | [50](../src/cli.ts#L50) | command, NAME and options to a `Command` |
| `serveOptions` | [59](../src/cli.ts#L59) | defaults and option switch of `serve` |
| `registerOptions` | [118](../src/cli.ts#L118) | defaults and option switch of `register` |
| `OptionReader` | [135](../src/cli.ts#L135) | `value`: the next argument, required<br>`optionalValue`: the next argument, absent when missing or starting with `--` |
| `validAgentName` | [157](../src/cli.ts#L157) | `^[a-z0-9](-?[a-z0-9])*-?$`, at most 32 characters |
| `usageError` | [215](../src/cli.ts#L215) | `ExitError` with code 2 |
| `main` | [219](../src/cli.ts#L219) | `help` to stdout, `serve`, `register` |
| top-level catch | [232-241](../src/cli.ts#L232-L241) | `ExitError`: message on stderr, its code; other errors: `logError`, code 1 |

### [2.2 `serve.ts`](../src/serve.ts)

Start sequence, event dispatch and stop sequence of one agent.

| Symbol | Line | Contents |
|---|---|---|
| `version` | [16](../src/serve.ts#L16) | the package version |
| `OwnerEntry` | [20](../src/serve.ts#L20) | `{memberId, name}` of `--owners` |
| `ServeOptions` | [25](../src/serve.ts#L25) | name, data folder and its `dataGiven` flag, display name, mode, owners, expiry, reset and token flags, port, QR code size, stop wait, audio options |
| `RunningServer` | [41](../src/serve.ts#L41) | token, `ChatApi`, user, `Actions`, `stop` |
| `serve` | [49](../src/serve.ts#L49) | `startServer`, then SIGINT and SIGTERM handlers: the first signal stops, a second ends the stop wait |
| `startServer` | [67](../src/serve.ts#L67) | data folder, lock, token, headers file, `ChatApi.init`, start lines, `startAgent`; cleanup on error |
| `startAgent` | [91](../src/serve.ts#L91) | user, network config, temp folder, control group, wiring of `Actions`, `AgentTools`, `Control`, HTTP, event handler, `stop` |
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
| `ConnectInput`, `SendInput`, `ReceiveInput`, `LeaveInput`, `DownloadInput` | [48-83](../src/tools.ts#L48-L83) | tool inputs |
| `PlanSummary` | [89](../src/tools.ts#L89) | card kind, chat type, name, SimpleX name, peer type, description, welcome, commands, `takesMessage` |
| `ChatFields` | [101](../src/tools.ts#L101) | chat fields of `connect` and `list` results |
| `Agent` | [115](../src/tools.ts#L115) | `ChatApi`, user, `Actions`, `ChatItemsWatch`, control chat reference, standing approver lookup, refused file post, data folder, audio options |
| `JoinMethod` | [127](../src/tools.ts#L127) | `prepared`, `prepareChannel`, `viaAddress`, `prepareContact`, `connect` |
| schemas | [186-415](../src/tools.ts#L186-L415) | input and output JSON schemas |
| descriptions, `instructions` | [417-455](../src/tools.ts#L417-L455) | tool descriptions and server instructions |
| `definitions` | [465](../src/tools.ts#L465) | name, description, schemas and action flag of the six tools |
| `Download` | [476](../src/tools.ts#L476) | one download: its promise, chat, item, file ID, size, path, the last fileSource seen, received bytes and their time, check timer |
| `AgentTools.register` | [503](../src/tools.ts#L503) | `registerTool` for each definition; `_meta` in mode `host` |
| `AgentTools.call` | [515](../src/tools.ts#L515) | handler result to `CallToolResult`; errors to `isError` |
| `AgentTools.perform` | [524](../src/tools.ts#L524) | perform step of an approved action |
| `downloadFile`, `forwardFile`, `downloadTo` | [537-564](../src/tools.ts#L537-L564) | one download per file ID, shared by `download` and `/forward`; a forwarded file moved to the download path for `download` |
| file events | [566-623](../src/tools.ts#L566-L623) | `onNewItems`, `onFile`, `onFileComplete`, `onFileError`, `onFileCancelled`, `onFileDescription` |
| `transcribesAtArrival`, `transcribeOnArrival` | [577-594](../src/tools.ts#L577-L594) | the conditions and the run of a voice message's transcription at arrival |
| `onContactConnected`, `onUserJoinedGroup` | [625-633](../src/tools.ts#L625-L633) | completion of connect actions by events |
| `actionResult` | [671](../src/tools.ts#L671) | wait for the decision, result fields, removal |
| `attachment`, `isRefused`, `fileToSend`, `resizedCopy` | [898-934](../src/tools.ts#L898-L934) | the file of a `send` |
| `copyForSending`, `chatFiles`, `deleteChatFiles` | [936-977](../src/tools.ts#L936-L977) | `/forward` copies; the files of a chat, forwarded ones included; its downloads cancelled, also those the core continues after a restart |
| `spokenFile` | [942](../src/tools.ts#L942) | the voice file of a `send` with `voice`: speech, conversion, duration |
| `leaveChat` | [1000](../src/tools.ts#L1000) | perform step of `leave` and `/leave`; the chat's forwarded files deleted |
| `download`, `fileServers`, `unknownServers` | [1010-1056](../src/tools.ts#L1010-L1056) | the `download` call before its action |
| `performDownload` | [1068](../src/tools.ts#L1068) | the download of an approved action, then the transcription of a voice file |
| `startDownload`, `checkDownload`, `currentFile`, `settleFile`, `settleReceived`, `moveReceived` | [1077-1144](../src/tools.ts#L1077-L1144) | a download's steps |
| `isFinal` | [1167](../src/tools.ts#L1167) | a message that is neither live nor held for its transcription |
| `agentChats`, `knownChat` | [1217-1234](../src/tools.ts#L1217-L1234) | the agent's chats; a checked chat reference |
| `classifyPlan` | [1317](../src/tools.ts#L1317) | `apiConnectPlan` response to a join plan or a known chat |
| `chatSubject`, `isConnected`, `isActive`, `memberCurrent` | [1413-1466](../src/tools.ts#L1413-L1466) | chat and member predicates shared with `control.ts` |
| `isTranscribed` | [1490](../src/tools.ts#L1490) | a voice file of at most 300 s and 1500000 bytes |
| `fitsMessage`, `quoteContent` | [1494-1510](../src/tools.ts#L1494-L1510) | the message as the core sends it, within 15602 bytes; for `send` and the post of `/forward` |
| `realFile` | [1526](../src/tools.ts#L1526) | real path, size, inode and change time of a file |
| `attachmentMedia`, `videoFrame`, `mediaDuration` | [1573-1607](../src/tools.ts#L1573-L1607) | previews and durations, also of voice replies |
| `toolCall` | [1623](../src/tools.ts#L1623) | `ServerContext` to `ToolCall`: abort signal and progress notifications |

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
| `cardText`, `requestLines`, `commandsLine` | [552-617](../src/control.ts#L552-L617) | card text per tool, state and mode |
| `largestCard`, `encodedLength` | [623-631](../src/control.ts#L623-L631) | card size check |
| `serversLine`, `sizeText` | [665-674](../src/control.ts#L665-L674) | servers line and sizes of download cards |
| `codeSpan`, `safeName`, `mentionText` | [684-696](../src/control.ts#L684-L696) | quoting of peer text |
| `ownerRequest` | [711](../src/control.ts#L711) | command words to an `OwnerRequest` |
| `qrCodeText` | [726](../src/control.ts#L726) | terminal QR code |
| `losesControl`, `isLost` | [742-757](../src/control.ts#L742-L757) | loss of the control chat |
| `entryText`, `restartText` | [775-785](../src/control.ts#L775-L785) | `--owners` entries in restart lines |

### [2.7 `messages.ts`](../src/messages.ts)

| Symbol | Line | Contents |
|---|---|---|
| `Message`, `MessageFile`, `FileOffer` | [17-51](../src/messages.ts#L17-L51) | message item of tool results; its file; the file of a download |
| `ToolCall` | [58](../src/messages.ts#L58) | abort signal and progress callback of one call |
| `Collected` | [63](../src/messages.ts#L63) | collected items, change signal, IDs of the items over the first 100, time of the last change |
| `Changes` | [70](../src/messages.ts#L70) | set of wake callbacks |
| `ChatItemsWatch` | [87](../src/messages.ts#L87) | listeners per chat reference; received message items gathered by `collect` |
| `chatRef`, `anyScopeChatRef`, `isChatRef` | [136-153](../src/messages.ts#L136-L153) | `@N` and `#N` references: of the main conversation, or also of a group's member chat |
| `toMessage`, `messageFile`, `receivedFileError`, `receivedPath` | [175-214](../src/messages.ts#L175-L214) | chat item to `Message`; where a received file is: its download path or its forwarded path |
| `filePaths`, `chatFolder` | [216-224](../src/messages.ts#L216-L224) | `DATA/files/group-N`, `DATA/files/contact-N` or the same under `DATA/forwarded`; the download path and the forwarded path of a received file |
| `fileOffer`, `fileContent` | [226-251](../src/messages.ts#L226-L251) | file fields of an item; message content of a file |
| `getChat`, `receivingFiles`, `markRead` | [268-284](../src/messages.ts#L268-L284) | `apiGetChat` and `apiChatItemsRead` by chat reference; the IDs of a chat's files in status `rcvAccepted`, `rcvTransfer` or `rcvWarning`, paged |
| `unreadItems` | [286](../src/messages.ts#L286) | unread items of a chat after its `initial` read, paged until a count of final messages |
| `reportProgress`, `nextChange`, `awaitResponses` | [299-329](../src/messages.ts#L299-L329) | waits; the wait of `awaitResponses` ended at idle only when every collected item is final |
| `validName`, `viewName` | [335-361](../src/messages.ts#L335-L361) | display name rules and `@name` form |

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

### [2.11 `audio.ts`](../src/audio.ts)

| Symbol | Line | Contents |
|---|---|---|
| `AudioOptions` | [6](../src/audio.ts#L6) | `url`, `stt`, `tts`, optional `voice` of `--audio` |
| `Transcript` | [13](../src/audio.ts#L13) | `transcript`, `transcript_error` |
| `maxVoiceBytes` | [18](../src/audio.ts#L18) | 1500000 |
| `transcript` | [22](../src/audio.ts#L22) | the queue of transcriptions: one request at a time, in call order |
| `speech` | [28](../src/audio.ts#L28) | `POST URL/v1/audio/speech`; the WAV bytes |
| `writeTranscript` | [33](../src/audio.ts#L33) | `PATH.transcript.json`, mode 0600; failures logged |
| `transcriptOf` | [44](../src/audio.ts#L44) | the string fields of `PATH.transcript.json`; none when unreadable |
| `transcribe` | [53](../src/audio.ts#L53) | the size check; `POST URL/v1/audio/transcriptions` with `model` and `file`; the trimmed `text` |
| `post` | [66](../src/audio.ts#L66) | `fetch` without redirects, 10 minutes; `SERVICE failed: ERROR.` |

---

## 3. State

### 3.1 Memory

| Item | Holder | Written by | Read by |
|---|---|---|---|
| Actions | `Actions.actions` map ([actions.ts:128](../src/actions.ts#L128)) | `create`, `remove`, `expireDue` | tools, `Control` |
| Open calls of an action | `ActionRecord.open` ([:44](../src/actions.ts#L44)) | `actionResult` ([tools.ts:672](../src/tools.ts#L672), [:681](../src/tools.ts#L681)) | `list`, `expireDue` |
| Collected replies | `ConnectAction.messages`, `SendAction.responses` ([:56](../src/actions.ts#L56), [:87](../src/actions.ts#L87)) | `ChatItemsWatch.collect` | `connect`, `send` results |
| Item listeners | `ChatItemsWatch.listeners` ([messages.ts:88](../src/messages.ts#L88)) | `collect` | event dispatch |
| Downloads | `AgentTools.downloads`, one per file ID ([tools.ts:494](../src/tools.ts#L494)) | `downloadTo` | file events, checks, `/delete CHAT files` |
| Forwards | `AgentTools.forwarding`, the copy for the post per file ID ([tools.ts:495](../src/tools.ts#L495)) | `forwardFile` | `downloadFile`, before it moves a forwarded file |
| Rejected files | `AgentTools.rejectedFiles` ([tools.ts:496](../src/tools.ts#L496)) | `downloadResult` ([:1063-1064](../src/tools.ts#L1063-L1064)) | the file field; `download`, which then skips the files approval ([:1025](../src/tools.ts#L1025)) |
| Count of file descriptions | `AgentTools.describedFiles` ([tools.ts:497](../src/tools.ts#L497), [:499](../src/tools.ts#L499)) | `onFileDescription` | `fileServers` |
| Voice files in transcription at arrival, at most 20 | `AgentTools.transcribing` ([tools.ts:498](../src/tools.ts#L498)) | `onNewItems`, `transcribeOnArrival` ([:573](../src/tools.ts#L573), [:592](../src/tools.ts#L592)) | `isFinal`, `performDownload` ([:1168](../src/tools.ts#L1168), [:1073](../src/tools.ts#L1073)) |
| Queue of transcriptions | `transcriptions` ([audio.ts:20](../src/audio.ts#L20)) | `transcript` ([:22-26](../src/audio.ts#L22-L26)) | `transcript` |
| Members of the control chat | `Control.members` ([control.ts:82](../src/control.ts#L82)) | `refreshMembers` ([:463-465](../src/control.ts#L463-L465)) | approver checks |
| Mode, expiry, owners list, stop wait | `ServeOptions` of the run | CLI | all modules |

### 3.2 Chat database custom data

| Item | Entity | Value | Written by | Read by |
|---|---|---|---|---|
| Control marker and counter | agent control chat | `{simplexMcp: {control: true, nextAction: N}}` | [`createControlChat`](../src/control.ts#L509-L520), [`saveCounter`](../src/control.ts#L135) | [`findControlGroup`](../src/control.ts#L499), [`controlDataOf`](../src/control.ts#L504) |
| Standing approval of messages | contact or group of the agent | `{...other keys, simplexMcp: {approvedBy, approvedById, approvedAt}}` | [`approveChat`](../src/control.ts#L243), [`revoke`](../src/control.ts#L256) | [`standingApprover`](../src/control.ts#L130) |
| Files approval | contact or group of the agent | `{...other keys, simplexMcpFiles: {approvedBy, approvedById, approvedAt}}` | [`approveChat`](../src/control.ts#L243), [`revoke`](../src/control.ts#L256) | [`standingApprover`](../src/control.ts#L130) |

The profile, chats, items, members and file transfers are stored by the core in `DATA/simplex_chat.db`; connections and queues in `DATA/simplex_agent.db` (file prefix `DATA/simplex`, [serve.ts:74](../src/serve.ts#L74)).

### 3.3 Files

| Path | Contents | Mode | Written | Read |
|---|---|---|---|---|
| `DATA/` | data folder | 0700, set at every start except on Windows | [files.ts:12-15](../src/files.ts#L12-L15) | |
| `DATA/token` | base64url of 32 random bytes | 0600, set again at every write except on Windows | first start, `--new-token` ([:30-36](../src/files.ts#L30-L36)) | every start |
| `DATA/headers.json` | `{"Authorization":"Bearer <token>"}` | 0600, set again at every write except on Windows | every start ([:42-44](../src/files.ts#L42-L44)) | Claude Code `headersHelper` |
| `DATA/server.json` | `{name, port, expireHours}` | 0600, set again at every write except on Windows | every start, after the HTTP listener is bound ([serve.ts:121](../src/serve.ts#L121)) | `register` |
| `DATA/lock` | PID and a newline | 0600 | start ([files.ts:64-72](../src/files.ts#L64-L72)) | next start |
| `DATA/tmp/` | the core's temp folder: files in transfer | 0700, set at every start | [serve.ts:96](../src/serve.ts#L96), [:208-212](../src/serve.ts#L208-L212) | the core |
| `DATA/files/group-N/`, `DATA/files/contact-N/` | downloaded files and files received without a download, named by file ID and extension | folder 0700 | the core at the download path ([tools.ts:1082-1083](../src/tools.ts#L1082-L1083)); moves ([:1561-1571](../src/tools.ts#L1561-L1571)) | `download` results, `/forward`, `/leave`, `/delete CHAT files` |
| `DATA/files/group-N/PATH.transcript.json` and the same under `contact-N` | the transcript of the voice file PATH: `{"transcript": TEXT}` or `{"transcript_error": ERROR}` | 0600 | [audio.ts:33-42](../src/audio.ts#L33-L42) | messages and `download` results ([audio.ts:44-51](../src/audio.ts#L44-L51), [messages.ts:203](../src/messages.ts#L203), [tools.ts:1061](../src/tools.ts#L1061)); deleted with the folder |
| `DATA/forwarded/group-N/`, `DATA/forwarded/contact-N/` | files `/forward` downloaded, named as above, until a download moves them to `DATA/files/` | folder 0700 | the core at the forwarded path ([tools.ts:547-554](../src/tools.ts#L547-L554), [:1082-1083](../src/tools.ts#L1082-L1083)) | `/forward`; moved by `download` ([:537-545](../src/tools.ts#L537-L545)); deleted by `leave`, `/leave` and `/delete CHAT files` ([:959-977](../src/tools.ts#L959-L977), [:1000-1008](../src/tools.ts#L1000-L1008)) |
| `DATA/files/sent/` | resized copies of sent images, copies of the files `/forward` posts, and voice replies with their WAV, one folder per copy | folder 0700; a resized copy and a WAV 0600 | [tools.ts:923-940](../src/tools.ts#L923-L940), [:942-952](../src/tools.ts#L942-L952), [:979-983](../src/tools.ts#L979-L983) | the core; deleted at `sndFileCompleteXFTP`, `sndFileError` and at start ([serve.ts:97](../src/serve.ts#L97), [:159-163](../src/serve.ts#L159-L163)) |

The core library is downloaded once into `CACHE/core-7.1.0.20/sqlite`, and the Node.js add-on into `CACHE/nodejs-addon-cce06d8/`, each with a `Downloading ...` line on stderr. CACHE is `$XDG_CACHE_HOME/simplex-chat` or `~/.cache/simplex-chat`; on macOS `~/Library/Caches/simplex-chat`; on Windows `%LOCALAPPDATA%\simplex-chat` ([libs.ts:41-48](../../simplex-chat-nodejs/src/libs.ts#L41-L48), [:89](../../simplex-chat-nodejs/src/libs.ts#L89), [:109](../../simplex-chat-nodejs/src/libs.ts#L109), [:116](../../simplex-chat-nodejs/src/libs.ts#L116), [:129](../../simplex-chat-nodejs/src/libs.ts#L129)).

---

## 4. Process lifecycle

### 4.1 Start

1. Data folder, lock, token and headers file are prepared ([serve.ts:69-73](../src/serve.ts#L69-L73)).
2. libsimplex for SQLite is loaded by `ChatApi.init`, and the databases are migrated with confirmation `yesUp` ([serve.ts:74](../src/serve.ts#L74), [api.ts:117-126](../../simplex-chat-nodejs/src/api.ts#L117-L126)).
3. Three log lines: version and NAME, data folder, mode ([serve.ts:75-77](../src/serve.ts#L75-L77)); with `--audio`, a fourth: URL and models ([:78](../src/serve.ts#L78)).
4. The active user is read, or created with display name `--display-name` or NAME, empty full name and peer type `agent` ([serve.ts:93](../src/serve.ts#L93)).
5. The network config is read and set with `smpProxyMode` `always` and `smpProxyFallback` `allowProtected` ([:94-95](../src/serve.ts#L94-L95)).
6. `DATA/tmp` is set as the core's temp folder, and `DATA/files/sent` is deleted ([:96-97](../src/serve.ts#L96-L97)).
7. Events are buffered, and the chat is started ([:98-99](../src/serve.ts#L98-L99)).
8. A `--display-name` different from the profile, or a peer type other than `agent`, is applied with one `apiUpdateProfile` ([:100](../src/serve.ts#L100), [:214-219](../src/serve.ts#L214-L219)).
9. The agent control chat is found by its marker, created with counter 1, or reset with `--reset-owners` ([:102](../src/serve.ts#L102), [:221-226](../src/serve.ts#L221-L226)).
10. `Actions`, `ChatItemsWatch`, `AgentTools` and `Control` are created; the counter is taken from the marker ([:103-112](../src/serve.ts#L103-L112)).
11. `Control.load`: for a lost control chat, `Actions.lost` is set; otherwise the group name and command menu are updated when they differ, and members are read ([control.ts:102-109](../src/control.ts#L102-L109)).
12. The HTTP server listens; with the port in use, the process exits with code 1 ([serve.ts:120](../src/serve.ts#L120), [:228-235](../src/serve.ts#L228-L235)). Until step 14 ends, requests to `/mcp` that pass the token check get 503 with `Retry-After: 1` ([http.ts:23](../src/http.ts#L23), [serve.ts:119-120](../src/serve.ts#L119-L120), [:128](../src/serve.ts#L128)).
13. `server.json` is written ([serve.ts:121](../src/serve.ts#L121)).
14. The server runs the owner setup, `Control.start`; after an error, HTTP is closed and the error is rethrown ([serve.ts:122-127](../src/serve.ts#L122-L127)).
15. `Listening on http://127.0.0.1:PORT/mcp` ([:129](../src/serve.ts#L129)).
16. Buffered events are replayed, then live events are dispatched ([:132](../src/serve.ts#L132), [:237-251](../src/serve.ts#L237-L251)).

After an error inside `startAgent` (steps 4-16) the chat is closed; after any error once the lock is taken, the lock is released ([serve.ts:71-88](../src/serve.ts#L71-L88)).

### 4.2 Event dispatch

Each `onAny` receiver is awaited by the library before the next event is read ([api.ts:195-202](../../simplex-chat-nodejs/src/api.ts#L195-L202)). The dispatcher in [`serve.ts:132-168`](../src/serve.ts#L132-L168):

| Event | Condition | Handler |
|---|---|---|
| `newChatItems` | item of the control group, main scope | [`Control.onMessage`](../src/control.ts#L171) |
| `newChatItems` | every item | [`AgentTools.onNewItems`](../src/tools.ts#L566), then [`ChatItemsWatch.deliver`](../src/messages.ts#L117) |
| `chatItemUpdated` | | [`AgentTools.onFile`](../src/tools.ts#L596), then [`ChatItemsWatch.update`](../src/messages.ts#L121) |
| `chatItemsDeleted` | | [`ChatItemsWatch.remove`](../src/messages.ts#L125) of each `deletedChatItem` |
| `contactConnected` | | [`AgentTools.onContactConnected`](../src/tools.ts#L625) |
| `userJoinedGroup` | | [`AgentTools.onUserJoinedGroup`](../src/tools.ts#L630) |
| `rcvFileComplete` | | [`AgentTools.onFileComplete`](../src/tools.ts#L601) |
| `rcvFileSndCancelled` | | [`AgentTools.onFileCancelled`](../src/tools.ts#L613) |
| `rcvFileError` | | [`AgentTools.onFileError`](../src/tools.ts#L608) |
| `rcvFileDescrReady` | | [`AgentTools.onFileDescription`](../src/tools.ts#L620) |
| `sndFileCompleteXFTP` | | [`AgentTools.onSentFile`](../src/tools.ts#L886) |
| `sndFileError` | | [`AgentTools.onSentFile`](../src/tools.ts#L886), then [`Control.fileNotSent`](../src/control.ts#L160) |
| `joinedGroupMember`<br>`joinedGroupMemberConnecting`<br>`connectedToGroupMember`<br>`memberRole`<br>`leftMember`<br>`deletedMember`<br>`deletedMemberUser`<br>`groupDeleted` | event of the control group | [`Control.onEvent`](../src/control.ts#L183), queued one at a time |
| any event | after `stop()` started | ignored |

Owner commands are queued in a second chain, independent of the membership chain ([control.ts:177-180](../src/control.ts#L177-L180)).

### 4.3 Stop

On `SIGINT` or `SIGTERM` the server runs one stop; a later signal ends its wait for cancelled cards ([serve.ts:49-65](../src/serve.ts#L49-L65)). `RunningServer.stop` ([serve.ts:174-184](../src/serve.ts#L174-L184)):

1. Events are ignored from now on.
2. `Actions.stop` starts: new actions are refused with `The server is stopping.`; the expiry timer is cleared; cards being posted are awaited; pending and performing actions are marked interrupted; the stop signal of the performs is aborted, which ends the wait of a `connect` for its connection; their cards are edited, or posted when they have no card; outcome edits already in progress are awaited ([actions.ts:240-250](../src/actions.ts#L240-L250)).
3. In parallel with step 2, the HTTP server is closed with all connections, and open calls are aborted ([http.ts:48-52](../src/http.ts#L48-L52)).
4. `Cancelled K action(s).` when K > 0 ([serve.ts:179](../src/serve.ts#L179)).
5. When cards were edited or posted at the stop and the control chat is not lost, the server waits `--stop-wait` seconds, 10 by default, for them to be sent; a second signal ends the wait ([serve.ts:180](../src/serve.ts#L180), [:192-206](../src/serve.ts#L192-L206)).
6. `chat.close()`: the chat is stopped, then the store is closed ([api.ts:165-170](../../simplex-chat-nodejs/src/api.ts#L165-L170)).
7. The lock is removed, `Stopped.` is logged, and the process exits with code 0, or 1 after an error ([serve.ts:182-183](../src/serve.ts#L182-L183), [:55-61](../src/serve.ts#L55-L61)).

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

The mode is set by `--approve` for one run ([cli.ts:81-83](../src/cli.ts#L81-L83)).

| Mode | State of a new action | Card | `_meta` of `connect`, `send`, `download`, `leave` |
|---|---|---|---|
| `requests` | `pending`<br>`performing` for a send under a standing approval, and for a download under a files approval | card with commands<br>outcome as a new message after a standing send; none after a download without a card | none |
| `host` | `performing` | outcome as a new message after the perform; none for a download | `{"anthropic/requiresUserInteraction": true}` |

Sources: [actions.ts:166](../src/actions.ts#L166), [tools.ts:509](../src/tools.ts#L509), [control.ts:145-152](../src/control.ts#L145-L152), [:495-497](../src/control.ts#L495-L497). In both modes a new action is refused unless the control chat has an approver ([control.ts:124-127](../src/control.ts#L124-L127)).

### 5.2 Owners and approvers

| Term | Rule | Code |
|---|---|---|
| human | member other than the agent's membership, status `connected` or `complete` | [control.ts:484-486](../src/control.ts#L484-L486), [tools.ts:1451-1453](../src/tools.ts#L1451-L1453) |
| human owner | human with role `owner` | [control.ts:471-473](../src/control.ts#L471-L473) |
| approver, `--owners` absent | member with role `owner` | [control.ts:488-492](../src/control.ts#L488-L492) |
| approver, `--owners` without a list | none | [control.ts:491](../src/control.ts#L491) |
| approver, `--owners ID:NAME,...` | member with role `owner` whose `groupMemberId` and `displayName`, folded to one line by `oneLine`, match an entry | [control.ts:491](../src/control.ts#L491), [:759-761](../src/control.ts#L759-L761) |

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

An action is removed from memory when its result is returned to a call ([tools.ts:678](../src/tools.ts#L678)), or at an expiry tick when it is decided, its open call count is 0 and its expiry time has passed ([actions.ts:200-201](../src/actions.ts#L200-L201)). A `download` still performing at the end of `wait` is returned as `performing` and kept ([tools.ts:675](../src/tools.ts#L675)).

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
| scope | `send` | `download` of a file whose servers are all known; with `--audio`, the download and transcription of a voice message at its arrival | [tools.ts:833](../src/tools.ts#L833), [:1025](../src/tools.ts#L1025), [:578](../src/tools.ts#L578) |
| decider shown | `safeName(approvedBy)` | the same | [control.ts:132](../src/control.ts#L132) |
| storage | the chat's custom data; removed with the chat by `leave` | the same | [tools.ts:1000-1008](../src/tools.ts#L1000-L1008) |

### 5.6 Expiry

| Rule | Code |
|---|---|
| `expires = created + expireHours × 3600000`, `expireHours` 1 to 23, default 23 | [actions.ts:163](../src/actions.ts#L163), [cli.ts:64](../src/cli.ts#L64), [:87-89](../src/cli.ts#L87-L89) |
| check every 60 s on an unreferenced timer, and before each owner command | [actions.ts:129](../src/actions.ts#L129), [control.ts:201](../src/control.ts#L201) |
| pending and due: `expired`, log `Action N expired.`, card edited to `Expired.` | [actions.ts:196-199](../src/actions.ts#L196-L199) |
| decided, no open call, due: removed from memory | [actions.ts:200-201](../src/actions.ts#L200-L201) |
| connect wait for the other side: at most until `created + 24 h - 10 min` | [tools.ts:147](../src/tools.ts#L147), [:724](../src/tools.ts#L724) |

`Date.now()` is compared with `expires`, so due actions are expired at the first tick after a sleep of the computer.

### 5.7 Action counter

| Rule | Code |
|---|---|
| initial value: `nextAction` of the control marker, or 1 | [serve.ts:103](../src/serve.ts#L103) |
| a new action takes `next`, then `next` grows by 1 | [actions.ts:156](../src/actions.ts#L156), [:178](../src/actions.ts#L178) |
| the new `next` is written before the card is posted | [actions.ts:254-255](../src/actions.ts#L254-L255) |
| one write at a time, also after a failed write | [control.ts:135-139](../src/control.ts#L135-L139) |
| `--reset-owners` copies the value into the new control chat | [serve.ts:224-225](../src/serve.ts#L224-L225), [control.ts:522-527](../src/control.ts#L522-L527) |
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
| card and reply names | folded to one line; a valid display name is plain, others a code span | [`safeName`](../src/control.ts#L688-L691) |
| card text lines | one code span per line; backticks become `'`; a leading or trailing space becomes U+00A0; blank lines empty | [`codeLines`, `codeSpan`](../src/control.ts#L673-L679) |
| file names and paths on cards and posts | folded to one line, one code span | [control.ts:585](../src/control.ts#L585), [:599](../src/control.ts#L599), [:166-168](../src/control.ts#L166-L168) |
| reply quote on a send card | whitespace runs to one space, first 100 characters (code points), one code span | [control.ts:587](../src/control.ts#L587) |
| link texts on a connect card | code span lines in the bytes that the largest card form leaves; the line that does not fit is cut and ends in `…` inside its code span; later lines left out | [control.ts:645-659](../src/control.ts#L645-L659) |
| mention names | valid name: `@name` or `@'name'`; others a code span | [`mentionText`](../src/control.ts#L693-L696) |
| errors on cards | folded to one line, one code span | [control.ts:565](../src/control.ts#L565) |
| `--owners` part of restart lines in the chat | one code span | [control.ts:79](../src/control.ts#L79), [:783-785](../src/control.ts#L783-L785) |
| terminal names and errors of actions | folded to one line | [actions.ts:180-181](../src/actions.ts#L180-L181), [:333-341](../src/actions.ts#L333-L341), [control.ts:78](../src/control.ts#L78) |
| tool results | returned as data; schema descriptions and instructions mark `text`, `from`, `name`, file names, downloaded files, transcripts, `description`, `welcome` and `commands` with their labels and params as data, not instructions; the instructions take a chat's messages as prompts only when the user said so in the session, and limit what the agent shares about its user | [tools.ts:192](../src/tools.ts#L192), [:196](../src/tools.ts#L196), [:201](../src/tools.ts#L201), [:207](../src/tools.ts#L207), [:228-229](../src/tools.ts#L228-L229), [:239-244](../src/tools.ts#L239-L244), [:409](../src/tools.ts#L409), [:410](../src/tools.ts#L410), [:453](../src/tools.ts#L453) |
| transcripts | kept off cards and out of the log | [audio.ts:33-42](../src/audio.ts#L33-L42) |

The server writes the tappable form `/'...'` only in the card's command line ([control.ts:604-617](../src/control.ts#L604-L617)), in `/'revoke CHAT'` of standing cards and replies ([:563](../src/control.ts#L563), [:253](../src/control.ts#L253)), and in `/'delete CHAT files'` of the kept-files message ([:301](../src/control.ts#L301)).

### 6.4 Chat references

| Rule | Code |
|---|---|
| `^[@#][1-9]\d{0,14}$`: 1 to 15 digits; the core reads `#(2^64 + N)` as `#N` | [messages.ts:151-153](../src/messages.ts#L151-L153), [scenarios.test.ts:491-503](../test/e2e/scenarios.test.ts#L491-L503) |
| the control chat is refused by `send`, `receive`, `download`, `leave` and the owner commands with CHAT, and left out of `list` and `/list` | [tools.ts:1173](../src/tools.ts#L1173), [:1217-1234](../src/tools.ts#L1217-L1234), [control.ts:328-335](../src/control.ts#L328-L335) |
| a store error of the chat read is reported as `No chat CHAT.` | [tools.ts:1178](../src/tools.ts#L1178), [:1232](../src/tools.ts#L1232) |
| the agent's own links are refused by `connect` (`ownLink` plans) | [tools.ts:1336](../src/tools.ts#L1336), [:1354](../src/tools.ts#L1354), [:1376](../src/tools.ts#L1376) |

### 6.5 Owner commands

| Rule | Code |
|---|---|
| source: `groupRcv` text items in the main scope of the control group | [serve.ts:137](../src/serve.ts#L137), [control.ts:173](../src/control.ts#L173) |
| form: one line `/keyword params`, leading spaces allowed | [control.ts:545-550](../src/control.ts#L545-L550) |
| the help reply is sent to any sender; other commands are accepted from approvers only | [control.ts:203-204](../src/control.ts#L203-L204) |
| commands run one at a time in arrival order | [control.ts:177-180](../src/control.ts#L177-L180) |
| `/approve`, `/reject`, `/receive` and `/forward` need a pending action with a card, and the action's chat when CHAT is given; `/receive` and `/forward` take downloads only, `/approve` the other actions only | [control.ts:227-237](../src/control.ts#L227-L237) |

### 6.6 Files

| Rule | Code |
|---|---|
| `send` refuses files named `id_*`, `*.pem`, `*.key`, `*.p12` or `*.pfx`, and files under `~/.ssh`, `~/.gnupg`, the data folder and `~/.simplex-mcp` except the agent's own `DATA/files/`, each by its real path; the refusal is posted to the control chat and logged | [tools.ts:900-914](../src/tools.ts#L900-L914), [:1526-1545](../src/tools.ts#L1526-L1545), [control.ts:154-158](../src/control.ts#L154-L158) |
| the file of a `send` is sent only when its real path, size, inode and change time at the perform are those of the request | [tools.ts:916-921](../src/tools.ts#L916-L921) |
| a sent JPEG or PNG is a resized copy made by the core | [tools.ts:923-934](../src/tools.ts#L923-L934) |
| `ffmpeg` and `ffprobe` read a video as MP4 or QuickTime, local files only, without standard input, 30 s and 64 MiB of output each | [tools.ts:1595-1613](../src/tools.ts#L1595-L1613) |
| `ffmpeg` reads the speech of a voice reply as WAV, a local file only, without standard input, within 30 s, and writes AAC in MPEG-4 | [tools.ts:946](../src/tools.ts#L946), [:1609-1613](../src/tools.ts#L1609-L1613) |
| with `--audio` and a files approval, a voice message of at most 300 s and 1500000 bytes on known XFTP servers is downloaded at its arrival, without approved relays, and its bytes are sent to the audio API | [tools.ts:577-594](../src/tools.ts#L577-L594), [audio.ts:53-64](../src/audio.ts#L53-L64) |
| a download is saved only at `DATA/files/group-N/` or `DATA/files/contact-N/`, or for `/forward` at the same name under `DATA/forwarded/`, named by its file ID and an extension of 1 to 16 ASCII letters and digits | [messages.ts:216-224](../src/messages.ts#L216-L224) |
| a forwarded file is outside `DATA/files/`, so `send` refuses it; a download moves it into `DATA/files/` after the copy for the post is made | [tools.ts:537-554](../src/tools.ts#L537-L554), [:909-914](../src/tools.ts#L909-L914) |
| a file on unknown servers gets a card that names them, also under a files approval; relays are approved only for such a card, and for every other download, and in mode `host`, the core checks the complete description and downloads a file on unknown servers only when the IP address is protected | [tools.ts:1025](../src/tools.ts#L1025), [:560](../src/tools.ts#L560), [:1083](../src/tools.ts#L1083), [control.ts:612-615](../src/control.ts#L612-L615) |
| an accepted file that the core returns to `rcvInvitation` fails its download at the next check | [tools.ts:1096](../src/tools.ts#L1096) |
| a received file is moved into the chat's folder at its completion, unless it is at the forwarded path, also from a member chat of a group, which has a download in memory as the main conversation does; a file without a download in memory, such as one whose transfer started before a restart, only when it has its full size; a partial file is deleted only for a download in memory, when it is no larger than the file | [tools.ts:1135-1144](../src/tools.ts#L1135-L1144), [:1547-1553](../src/tools.ts#L1547-L1553) |

### 6.7 Link and network

- The join link is a group link of the control chat with role `member`, or `observer` with `--owners`; it is printed with a QR code on stdout and deleted at the first join event ([control.ts:406-416](../src/control.ts#L406-L416), [:372](../src/control.ts#L372), [:381](../src/control.ts#L381)).
- At every start the server sets private message routing: `smpProxyMode` `always`, `smpProxyFallback` `allowProtected` ([serve.ts:94-95](../src/serve.ts#L94-L95)).
- With `incognito: true` the join methods connect incognito ([tools.ts:756](../src/tools.ts#L756)); the core then uses a random profile for that chat ([PLAN, Participants](../PLAN.md#participants)).

### 6.8 Audio API

| Rule | Code |
|---|---|
| URL is set only by `--audio`, `http` or `https` without a user name or password, and logged at start | [cli.ts:177-188](../src/cli.ts#L177-L188), [serve.ts:78](../src/serve.ts#L78) |
| a transcription request: two fields, `model` and the voice file named `<file ID><extension>` | [audio.ts:56-58](../src/audio.ts#L56-L58) |
| a speech request: `model`, the voice text as `input`, `response_format` and the optional `voice` | [audio.ts:29](../src/audio.ts#L29) |
| redirects are refused; each request is aborted after 10 minutes | [audio.ts:19](../src/audio.ts#L19), [:68](../src/audio.ts#L68) |
| transcription requests are sent one at a time | [audio.ts:20-26](../src/audio.ts#L20-L26) |

---

## 7. Limits

| Limit | Value | Code |
|---|---|---|
| pending actions | 20 | [actions.ts:11](../src/actions.ts#L11) |
| actions being performed | 20 | [actions.ts:12](../src/actions.ts#L12) |
| expiry | 1 to 23 h, default 23 | [cli.ts:64](../src/cli.ts#L64), [:87-89](../src/cli.ts#L87-L89) |
| connect wait for the other side | `created + 23 h 50 min` | [tools.ts:147](../src/tools.ts#L147) |
| send, connect and download card | 15000 bytes JSON-escaped, in its largest form | [control.ts:26](../src/control.ts#L26), [:623-631](../src/control.ts#L623-L631) |
| error assumed in the largest card | 600 × `€` | [control.ts:30](../src/control.ts#L30) |
| message as the core sends it | 15602 bytes, counted with 600 bytes of envelope; `send` refuses a longer one, and the post of `/forward` leaves out its quote of the card | [tools.ts:157-158](../src/tools.ts#L157-L158), [:1494-1497](../src/tools.ts#L1494-L1497), [control.ts:265-281](../src/control.ts#L265-L281) |
| `send.text` | 1 to 12000 characters | [tools.ts:266](../src/tools.ts#L266) |
| `connect.message` | 1 to 2000 characters | [tools.ts:254](../src/tools.ts#L254) |
| `send.mention` | 1 to 3 unique member IDs | [tools.ts:270](../src/tools.ts#L270) |
| `wait` | 0 to 86400 s | [tools.ts:255](../src/tools.ts#L255), [:271](../src/tools.ts#L271), [:285](../src/tools.ts#L285), [:308](../src/tools.ts#L308) |
| `idle` | 1 to 300 s, default 30 | [tools.ts:256](../src/tools.ts#L256), [:272](../src/tools.ts#L272), [:153](../src/tools.ts#L153) |
| `limit` | 1 to 100, default 50 | [tools.ts:273](../src/tools.ts#L273), [:286](../src/tools.ts#L286), [:154](../src/tools.ts#L154) |
| `connect` snapshot | last 50 items | [tools.ts:155](../src/tools.ts#L155) |
| collected items per action or call | 100, then their IDs kept as overflow until deleted | [messages.ts:11](../src/messages.ts#L11), [:110-111](../src/messages.ts#L110-L111) |
| unread page | 100 items | [messages.ts:10](../src/messages.ts#L10) |
| chat list read | one read of 1000000 chats | [tools.ts:146](../src/tools.ts#L146) |
| `download` wait, default | 60 s | [tools.ts:150](../src/tools.ts#L150) |
| download check | every 60 s | [tools.ts:151](../src/tools.ts#L151) |
| download without progress | 30 min, then cancelled | [tools.ts:152](../src/tools.ts#L152) |
| `ffmpeg`, `ffprobe` | 30 s, 64 MiB of output | [tools.ts:156](../src/tools.ts#L156), [:160](../src/tools.ts#L160) |
| `send.voice` | 1 to 4096 characters | [tools.ts:279](../src/tools.ts#L279) |
| voice file transcribed at arrival or at `download` | 300 s declared, 1500000 bytes | [tools.ts:170](../src/tools.ts#L170), [audio.ts:18](../src/audio.ts#L18), [tools.ts:1490-1492](../src/tools.ts#L1490-L1492) |
| voice files in transcription at arrival | 20 | [tools.ts:171](../src/tools.ts#L171) |
| wait for the upload of a voice file at arrival | 60 s | [tools.ts:584](../src/tools.ts#L584) |
| audio API request | 10 min; transcriptions one at a time | [audio.ts:19](../src/audio.ts#L19), [:20-26](../src/audio.ts#L20-L26) |
| stop wait | 0 to 60 s, default 10 | [cli.ts:69](../src/cli.ts#L69), [:90-92](../src/cli.ts#L90-L92) |
| error details | 500 characters | [log.ts:4](../src/log.ts#L4) |
| progress interval | 60 s | [messages.ts:8](../src/messages.ts#L8) |
| expiry tick | 60 s | [actions.ts:10](../src/actions.ts#L10) |
| HTTP body | 4 MiB | [server index.cjs:254](../node_modules/@modelcontextprotocol/server/dist/index.cjs#L254) |
| SSE keep-alive | 15 s | [mcp-BumlNux0.cjs:64](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L64) |
| agent NAME | 1 to 32 characters | [cli.ts:158](../src/cli.ts#L158) |
| display name | 50 characters, `mkValidName` rules | [messages.ts:12-13](../src/messages.ts#L12-L13), [:335-357](../src/messages.ts#L335-L357) |
| port | 1 to 65535, default 5226 | [cli.ts:67](../src/cli.ts#L67), [:99-101](../src/cli.ts#L99-L101) |
