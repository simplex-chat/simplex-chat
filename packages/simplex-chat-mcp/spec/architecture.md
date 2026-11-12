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
    oc["opencode<br/>simplex-secrets.ts"]
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
  files --> data[("DATA:<br/>token<br/>headers.json<br/>server.json<br/>lock<br/>secrets.json")]
  files --> secrets[("SECRETS:<br/>one file per name")]
  cc -.->|"commands:<br/>cat SECRETS/NAME"| secrets
  oc -.->|"values replaced<br/>in model requests"| secrets
  oc -->|"POST /mcp"| http
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
3. An `McpServer` is built from the factory for each request, and the arguments are validated against the input schema by the SDK ([`serve.ts:113-117`](../src/serve.ts#L113-L117), [`mcp-BumlNux0.cjs:1812-1817`](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L1812-L1817)).
4. The handler result is returned by `AgentTools.call` as `structuredContent` and as JSON text ([`tools.ts:509-516`](../src/tools.ts#L509-L516)).
5. The server calls `ChatApi` methods of the core ([`messages.ts:267-283`](../src/messages.ts#L267-L283)); actions are created in `Actions` and shown to the owner by `Control` ([`actions.ts:155-186`](../src/actions.ts#L155-L186)).
6. The core emits events; the server dispatches them in one handler of `serve.ts` to the tools, the item watch and the agent control chat ([`serve.ts:131-167`](../src/serve.ts#L131-L167)).

---

## 2. Modules

### [2.1 `cli.ts`](../src/cli.ts)

Entry point of the `simplex-mcp` binary.

| Symbol | Line | Contents |
|---|---|---|
| `helpText` | [10](../src/cli.ts#L10) | `--help` output |
| `Command` | [42](../src/cli.ts#L42) | `help`, `serve` with `ServeOptions`, `register` with `RegisterOptions` |
| `parseCommand` | [47](../src/cli.ts#L47) | command, NAME and options to a `Command` |
| `serveOptions` | [56](../src/cli.ts#L56) | defaults and option switch of `serve`; `--secrets-folder` checked against the data folder |
| `registerOptions` | [117](../src/cli.ts#L117) | defaults and option switch of `register` |
| `OptionReader` | [134](../src/cli.ts#L134) | `value`: the next argument, required<br>`optionalValue`: the next argument, absent when missing or starting with `--` |
| `validAgentName` | [156](../src/cli.ts#L156) | `^[a-z0-9](-?[a-z0-9])*-?$`, at most 32 characters |
| `usageError` | [201](../src/cli.ts#L201) | `ExitError` with code 2 |
| `main` | [205](../src/cli.ts#L205) | `help` to stdout, `serve`, `register` |
| top-level catch | [218-227](../src/cli.ts#L218-L227) | `ExitError`: message on stderr, its code; other errors: `logError`, code 1 |

### [2.2 `serve.ts`](../src/serve.ts)

Start sequence, event dispatch and stop sequence of one agent.

| Symbol | Line | Contents |
|---|---|---|
| `version` | [15](../src/serve.ts#L15) | the package version |
| `OwnerEntry` | [19](../src/serve.ts#L19) | `{memberId, name}` of `--owners` |
| `ServeOptions` | [24](../src/serve.ts#L24) | name, data folder and its `dataGiven` flag, display name, mode, owners, expiry, reset and token flags, port, QR code size, stop wait, secrets folder |
| `RunningServer` | [40](../src/serve.ts#L40) | token, `ChatApi`, user, `Actions`, `stop` |
| `serve` | [48](../src/serve.ts#L48) | `startServer`, then SIGINT and SIGTERM handlers: the first signal stops, a second ends the stop wait |
| `startServer` | [66](../src/serve.ts#L66) | data folder, lock, token, headers file, `ChatApi.init`, `startAgent`; cleanup on error |
| `startAgent` | [89](../src/serve.ts#L89) | `Secrets` with `--secrets-folder`, user, network config, temp folder, control group, wiring of `Actions`, `AgentTools`, `Control`, HTTP, event handler, `stop` |
| `registerHint` | [187](../src/serve.ts#L187) | the `register` command line printed after the owner setup |
| `waitForCancelledCards` | [191](../src/serve.ts#L191) | the fixed wait at stop for cards edited or posted at the stop |
| `setTempFolder` | [207](../src/serve.ts#L207) | `DATA/tmp`, mode 0700, as the core's temp folder |
| `updatedUser` | [213](../src/serve.ts#L213) | display name and peer type `agent` of the profile |
| `controlGroup` | [220](../src/serve.ts#L220) | found, created or reset agent control chat |
| `listen` | [227](../src/serve.ts#L227) | `startHttp`; `EADDRINUSE` to `ExitError` code 1 |
| `bufferEvents` | [236](../src/serve.ts#L236) | events received before the handler is installed are queued and replayed in order |

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
| `ConnectInput`, `SendInput`, `ReceiveInput`, `LeaveInput`, `DownloadInput` | [47-81](../src/tools.ts#L47-L81) | tool inputs |
| `PlanSummary` | [87](../src/tools.ts#L87) | card kind, chat type, name, SimpleX name, peer type, description, welcome, commands, `takesMessage` |
| `ChatFields` | [99](../src/tools.ts#L99) | chat fields of `connect` and `list` results |
| `Agent` | [113](../src/tools.ts#L113) | `ChatApi`, user, `Actions`, `ChatItemsWatch`, control chat reference, standing approver lookup, refused file post, data folder, `Secrets` with `--secrets-folder` |
| `JoinMethod` | [125](../src/tools.ts#L125) | `prepared`, `prepareChannel`, `viaAddress`, `prepareContact`, `connect` |
| schemas | [180-408](../src/tools.ts#L180-L408) | input and output JSON schemas |
| descriptions, `instructions` | [410-450](../src/tools.ts#L410-L450) | tool descriptions and server instructions |
| `definitions` | [460](../src/tools.ts#L460) | name, description, schemas and action flag of the six tools |
| `Download` | [471](../src/tools.ts#L471) | one download: its promise, chat, item, file ID, size, path, the last fileSource seen, received bytes and their time, check timer |
| `AgentTools.register` | [497](../src/tools.ts#L497) | `registerTool` for each definition; `_meta` in mode `host` |
| `AgentTools.call` | [509](../src/tools.ts#L509) | handler result to `CallToolResult`; errors to `isError` |
| `AgentTools.perform` | [518](../src/tools.ts#L518) | perform step of an approved action |
| `downloadFile`, `forwardFile`, `downloadTo` | [531-558](../src/tools.ts#L531-L558) | one download per file ID, shared by `download` and `/forward`; a forwarded file moved to the download path for `download` |
| file events | [560-597](../src/tools.ts#L560-L597) | `onNewItems`, `onFile`, `onFileComplete`, `onFileError`, `onFileCancelled`, `onFileDescription` |
| `onContactConnected`, `onUserJoinedGroup` | [599-607](../src/tools.ts#L599-L607) | completion of connect actions by events |
| `actionResult` | [645](../src/tools.ts#L645) | wait for the decision, result fields, removal |
| `attachment`, `isRefused`, `fileToSend`, `resizedCopy` | [869-905](../src/tools.ts#L869-L905) | the file of a `send` |
| `copyForSending`, `chatFiles`, `deleteChatFiles` | [907-936](../src/tools.ts#L907-L936) | `/forward` copies; the files of a chat, forwarded ones included; its downloads cancelled, also those the core continues after a restart |
| `leaveChat` | [959](../src/tools.ts#L959) | perform step of `leave` and `/leave`; the chat's forwarded files deleted |
| `download`, `fileServers`, `unknownServers` | [969-1015](../src/tools.ts#L969-L1015) | the `download` call before its action |
| `startDownload`, `checkDownload`, `currentFile`, `settleFile`, `settleReceived`, `moveReceived` | [1033-1100](../src/tools.ts#L1033-L1100) | a download's steps |
| `listChats` | [1159](../src/tools.ts#L1159) | the `list` result: chats, actions, and the secrets folder and names |
| `agentChats`, `knownChat` | [1170-1187](../src/tools.ts#L1170-L1187) | the agent's chats; a checked chat reference |
| `classifyPlan` | [1270](../src/tools.ts#L1270) | `apiConnectPlan` response to a join plan or a known chat |
| `chatSubject`, `isConnected`, `isActive`, `memberCurrent` | [1366-1419](../src/tools.ts#L1366-L1419) | chat and member predicates shared with `control.ts` |
| `fitsMessage`, `quoteContent` | [1443-1459](../src/tools.ts#L1443-L1459) | the message as the core sends it, within 15602 bytes; for `send` and the post of `/forward` |
| `realFile` | [1475](../src/tools.ts#L1475) | real path, size, inode and change time of a file |
| `attachmentMedia`, `videoFrame`, `videoDuration` | [1522-1556](../src/tools.ts#L1522-L1556) | previews and durations |
| `toolCall` | [1572](../src/tools.ts#L1572) | `ServerContext` to `ToolCall`: abort signal and progress notifications |

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

Hooks are bound in [`serve.ts:102-108`](../src/serve.ts#L102-L108): `refusal`, `saveCounter`, `postCard` and `outcome` to `Control`; `perform` to `AgentTools`.

### [2.6 `control.ts`](../src/control.ts)

| Symbol | Line | Contents |
|---|---|---|
| `controlCommands` | [11](../src/control.ts#L11) | the command menu |
| `fullDelete`, `commandsReply`, `secretsOff`, `secretUsage` | [18-21](../src/control.ts#L18-L21) | the full deletion preference of the group profile; the commands reply; the replies of the secret commands without `--secrets-folder` and to an invalid `/secret` |
| `standings` | [27](../src/control.ts#L27) | custom data key and reply words of the approvals of messages and files |
| `ControlData` | [48](../src/control.ts#L48) | `{control: true, nextAction}` |
| `ChatApproval` | [66](../src/control.ts#L66) | `{approvedBy, approvedById, approvedAt}` |
| `OwnerRequest` | [72](../src/control.ts#L72) | parsed owner command |
| `Control.load` | [110](../src/control.ts#L110) | lost check, group profile with menu and full deletion, member list |
| `Control.start` | [119](../src/control.ts#L119) | owner setup per `--owners` |
| `Control.refusal` | [127](../src/control.ts#L127) | bytes left for the link texts of a connect card, card size of a send, connect or download, approver presence |
| `Control.standingApprover` | [138](../src/control.ts#L138) | approver name of a chat's standing approval of messages or files |
| `Control.saveCounter` | [143](../src/control.ts#L143) | queued counter writes |
| `Control.postCard`, `Control.outcome` | [149-160](../src/control.ts#L149-L160) | card post and card edit; the kept-files message after a done leave |
| `Control.refusedFile`, `Control.fileNotSent` | [162-177](../src/control.ts#L162-L177) | posts about files |
| `Control.onMessage` | [179](../src/control.ts#L179) | owner commands, queued one at a time; the deletion of `/secret` messages before their command |
| `Control.onEvent` | [194](../src/control.ts#L194) | membership changes and loss of the control chat |
| `Control.ownerCommand` | [211](../src/control.ts#L211) | command dispatch and replies |
| `approveChat`, `revoke`, `forward`, `leave`, `deleteFiles` | [260-324](../src/control.ts#L260-L324) | owner requests |
| `setSecret`, `removeSecret`, `secretsText` | [345-366](../src/control.ts#L345-L366) | the secret commands |
| `cardContent`, `fittedImage` | [481-501](../src/control.ts#L481-L501) | image and video cards with the file's preview |
| `findControlGroup`, `createControlChat`, `resetControlChat` | [539-567](../src/control.ts#L539-L567) | control group lookup, creation, reset |
| `isControlEvent` | [569](../src/control.ts#L569) | membership events of the control group |
| `parseCommand` | [585](../src/control.ts#L585) | one-line `/keyword params` |
| `cardText`, `requestLines`, `commandsLine` | [592-656](../src/control.ts#L592-L656) | card text per tool, state and mode |
| `largestCard`, `encodedLength` | [662-670](../src/control.ts#L662-L670) | card size check |
| `serversLine`, `sizeText` | [704-713](../src/control.ts#L704-L713) | servers line and sizes of download cards |
| `codeSpan`, `safeName`, `mentionText` | [723-735](../src/control.ts#L723-L735) | quoting of peer text |
| `ownerRequest` | [750](../src/control.ts#L750) | command words to an `OwnerRequest` |
| `qrCodeText` | [767](../src/control.ts#L767) | terminal QR code |
| `losesControl`, `isLost` | [783-798](../src/control.ts#L783-L798) | loss of the control chat |
| `entryText`, `restartText` | [816-826](../src/control.ts#L816-L826) | `--owners` entries in restart lines |

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
| `SecretRecord`, `secretName` | [12-14](../src/files.ts#L12-L14) | `{setBy, setById, setAt}` of a secret; `^[A-Z][A-Z0-9_]{0,63}$` |
| `createDataFolder` | [16](../src/files.ts#L16) | `mkdir -p` mode 0700; `chmod 0700` except on Windows |
| `acquireLock`, `releaseLock` | [21-32](../src/files.ts#L21-L32) | `DATA/lock` with the PID |
| `serverToken` | [34](../src/files.ts#L34) | `DATA/token`: read, or 32 random bytes in base64url |
| `writeHeadersFile` | [46](../src/files.ts#L46) | `DATA/headers.json` |
| `writeServerFile`, `readServerFile` | [50-57](../src/files.ts#L50-L57) | `DATA/server.json` |
| `Secrets` | [59](../src/files.ts#L59) | the secrets folder, created with `mkdir -p` mode 0700: `names`, the file names that match `secretName`, sorted; `records`, read from `DATA/secrets.json`; `set`, the value written to `NAME.part`, renamed to `NAME`, and its record; `remove`, for a name among `names` only |

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

### [2.11 `opencode/simplex-secrets.ts`](../opencode/simplex-secrets.ts)

An opencode plugin, shipped as TypeScript source in the package and run by opencode, outside the server. The folder of the secrets is read from `SIMPLEX_MCP_SECRETS` of the opencode process.

| Symbol | Line | Contents |
|---|---|---|
| `secretName` | [8](../opencode/simplex-secrets.ts#L8) | the name rule of `files.ts` |
| `seen` | [10](../opencode/simplex-secrets.ts#L10) | every form read since the process started, with its mark |
| default export | [12-23](../opencode/simplex-secrets.ts#L12-L23) | `{id: "simplex-secrets", server}`; `server` returns the hook `experimental.chat.messages.transform` |
| the hook | [15-21](../opencode/simplex-secrets.ts#L15-L21) | throws without `SIMPLEX_MCP_SECRETS`; adds the forms of the folder to `seen`; replaces the parts of each message with redacted copies, longest form first |
| `replacements` | [25-34](../opencode/simplex-secrets.ts#L25-L34) | for each file of the folder that matches `secretName`: the value without trailing newlines, its `encodeURIComponent` form and its base64 forms, each with the mark `[secret NAME]`; throws for a value of fewer than 8 characters |
| `redacted` | [36-41](../opencode/simplex-secrets.ts#L36-L41) | a string with each form replaced by its mark; an array or object copied, each item redacted; other values unchanged |
| `base64Forms` | [43-48](../opencode/simplex-secrets.ts#L43-L48) | base64 and base64url of the value after 0, 1 and 2 zero bytes, without padding and without the characters at both ends that hold bits of the adjacent bytes |

---

## 3. State

### 3.1 Memory

| Item | Holder | Written by | Read by |
|---|---|---|---|
| Actions | `Actions.actions` map ([actions.ts:128](../src/actions.ts#L128)) | `create`, `remove`, `expireDue` | tools, `Control` |
| Open calls of an action | `ActionRecord.open` ([:44](../src/actions.ts#L44)) | `actionResult` ([tools.ts:646](../src/tools.ts#L646), [:655](../src/tools.ts#L655)) | `list`, `expireDue` |
| Collected replies | `ConnectAction.messages`, `SendAction.responses` ([:56](../src/actions.ts#L56), [:87](../src/actions.ts#L87)) | `ChatItemsWatch.collect` | `connect`, `send` results |
| Item listeners | `ChatItemsWatch.listeners` ([messages.ts:87](../src/messages.ts#L87)) | `collect` | event dispatch |
| Downloads | `AgentTools.downloads`, one per file ID ([tools.ts:489](../src/tools.ts#L489)) | `downloadTo` | file events, checks, `/delete CHAT files` |
| Forwards | `AgentTools.forwarding`, the copy for the post per file ID ([tools.ts:490](../src/tools.ts#L490)) | `forwardFile` | `downloadFile`, before it moves a forwarded file |
| Rejected files | `AgentTools.rejectedFiles` ([tools.ts:491](../src/tools.ts#L491)) | `downloadResult` ([:1022-1023](../src/tools.ts#L1022-L1023)) | the file field; `download`, which then skips the files approval ([:984](../src/tools.ts#L984)) |
| Count of file descriptions | `AgentTools.describedFiles` ([tools.ts:492-493](../src/tools.ts#L492-L493)) | `onFileDescription` | `fileServers` |
| Members of the control chat | `Control.members` ([control.ts:90](../src/control.ts#L90)) | `refreshMembers` ([:503-505](../src/control.ts#L503-L505)) | approver checks |
| Mode, expiry, owners list, stop wait | `ServeOptions` of the run | CLI | all modules |

### 3.2 Chat database custom data

| Item | Entity | Value | Written by | Read by |
|---|---|---|---|---|
| Control marker and counter | agent control chat | `{simplexMcp: {control: true, nextAction: N}}` | [`createControlChat`](../src/control.ts#L549-L560), [`saveCounter`](../src/control.ts#L143) | [`findControlGroup`](../src/control.ts#L539), [`controlDataOf`](../src/control.ts#L544) |
| Standing approval of messages | contact or group of the agent | `{...other keys, simplexMcp: {approvedBy, approvedById, approvedAt}}` | [`approveChat`](../src/control.ts#L260), [`revoke`](../src/control.ts#L273) | [`standingApprover`](../src/control.ts#L138) |
| Files approval | contact or group of the agent | `{...other keys, simplexMcpFiles: {approvedBy, approvedById, approvedAt}}` | [`approveChat`](../src/control.ts#L260), [`revoke`](../src/control.ts#L273) | [`standingApprover`](../src/control.ts#L138) |

The profile, chats, items, members and file transfers are stored by the core in `DATA/simplex_chat.db`; connections and queues in `DATA/simplex_agent.db` (file prefix `DATA/simplex`, [serve.ts:73](../src/serve.ts#L73)).

### 3.3 Files

| Path | Contents | Mode | Written | Read |
|---|---|---|---|---|
| `DATA/` | data folder | 0700, set at every start except on Windows | [files.ts:16-19](../src/files.ts#L16-L19) | |
| `DATA/token` | base64url of 32 random bytes | 0600, set again at every write except on Windows | first start, `--new-token` ([:34-40](../src/files.ts#L34-L40)) | every start |
| `DATA/headers.json` | `{"Authorization":"Bearer <token>"}` | 0600, set again at every write except on Windows | every start ([:46-48](../src/files.ts#L46-L48)) | Claude Code `headersHelper` |
| `DATA/server.json` | `{name, port, expireHours}` | 0600, set again at every write except on Windows | every start, after the HTTP listener is bound ([serve.ts:120](../src/serve.ts#L120)) | `register` |
| `DATA/lock` | PID and a newline | 0600 | start ([files.ts:97-105](../src/files.ts#L97-L105)) | next start |
| `DATA/tmp/` | the core's temp folder: files in transfer | 0700, set at every start | [serve.ts:95](../src/serve.ts#L95), [:207-211](../src/serve.ts#L207-L211) | the core |
| `DATA/files/group-N/`, `DATA/files/contact-N/` | downloaded files and files received without a download, named by file ID and extension | folder 0700 | the core at the download path ([tools.ts:1038-1039](../src/tools.ts#L1038-L1039)); moves ([:1510-1520](../src/tools.ts#L1510-L1520)) | `download` results, `/forward`, `/leave`, `/delete CHAT files` |
| `DATA/forwarded/group-N/`, `DATA/forwarded/contact-N/` | files `/forward` downloaded, named as above, until a download moves them to `DATA/files/` | folder 0700 | the core at the forwarded path ([tools.ts:541-548](../src/tools.ts#L541-L548), [:1038-1039](../src/tools.ts#L1038-L1039)) | `/forward`; moved by `download` ([:531-539](../src/tools.ts#L531-L539)); deleted by `leave`, `/leave` and `/delete CHAT files` ([:918-936](../src/tools.ts#L918-L936), [:959-967](../src/tools.ts#L959-L967)) |
| `DATA/files/sent/` | resized copies of sent images, and copies of the files `/forward` posts, one folder per copy | folder 0700; a resized copy 0600 | [tools.ts:894-911](../src/tools.ts#L894-L911), [:938-942](../src/tools.ts#L938-L942) | the core; deleted at `sndFileCompleteXFTP`, `sndFileError` and at start ([serve.ts:96](../src/serve.ts#L96), [:158-162](../src/serve.ts#L158-L162)) |
| `DATA/secrets.json` | `{NAME: {setBy, setById, setAt}}`, the records of the secrets | 0600, set again at every write except on Windows | `/secret`, `/unsecret` ([files.ts:72-85](../src/files.ts#L72-L85)) | `/secrets` |
| `SECRETS/` | the folder of `--secrets-folder` | 0700 when the server creates it; the mode of an existing folder is kept | every start with `--secrets-folder` ([serve.ts:91](../src/serve.ts#L91), [files.ts:60-62](../src/files.ts#L60-L62)) | |
| `SECRETS/NAME` | the value of secret NAME, as sent, without a newline | 0600, set again at every write except on Windows | `/secret`, through `SECRETS/NAME.part` ([files.ts:72-77](../src/files.ts#L72-L77)); deleted by `/unsecret` ([:79-85](../src/files.ts#L79-L85)) | commands of the agent; the opencode plugin |

The core library is downloaded once into `CACHE/core-7.1.0.20/sqlite`, and the Node.js add-on into `CACHE/nodejs-addon-cce06d8/`, each with a `Downloading ...` line on stderr. CACHE is `$XDG_CACHE_HOME/simplex-chat` or `~/.cache/simplex-chat`; on macOS `~/Library/Caches/simplex-chat`; on Windows `%LOCALAPPDATA%\simplex-chat` ([libs.ts:41-48](../../simplex-chat-nodejs/src/libs.ts#L41-L48), [:89](../../simplex-chat-nodejs/src/libs.ts#L89), [:109](../../simplex-chat-nodejs/src/libs.ts#L109), [:116](../../simplex-chat-nodejs/src/libs.ts#L116), [:129](../../simplex-chat-nodejs/src/libs.ts#L129)).

---

## 4. Process lifecycle

### 4.1 Start

1. Data folder, lock, token and headers file are prepared ([serve.ts:68-72](../src/serve.ts#L68-L72)).
2. libsimplex for SQLite is loaded by `ChatApi.init`, and the databases are migrated with confirmation `yesUp` ([serve.ts:73](../src/serve.ts#L73), [api.ts:117-126](../../simplex-chat-nodejs/src/api.ts#L117-L126)).
3. Three log lines: version and NAME, data folder, mode ([serve.ts:74-76](../src/serve.ts#L74-L76)).
4. With `--secrets-folder`, `Secrets` is created, and the folder with it when missing ([serve.ts:91](../src/serve.ts#L91)). The active user is read, or created with display name `--display-name` or NAME, empty full name and peer type `agent` ([serve.ts:92](../src/serve.ts#L92)).
5. The network config is read and set with `smpProxyMode` `always` and `smpProxyFallback` `allowProtected` ([:93-94](../src/serve.ts#L93-L94)).
6. `DATA/tmp` is set as the core's temp folder, and `DATA/files/sent` is deleted ([:95-96](../src/serve.ts#L95-L96)).
7. Events are buffered, and the chat is started ([:97-98](../src/serve.ts#L97-L98)).
8. A `--display-name` different from the profile, or a peer type other than `agent`, is applied with one `apiUpdateProfile` ([:99](../src/serve.ts#L99), [:213-218](../src/serve.ts#L213-L218)).
9. The agent control chat is found by its marker, created with counter 1, or reset with `--reset-owners` ([:101](../src/serve.ts#L101), [:220-225](../src/serve.ts#L220-L225)).
10. `Actions`, `ChatItemsWatch`, `AgentTools` and `Control` are created; the counter is taken from the marker ([:102-111](../src/serve.ts#L102-L111)).
11. `Control.load`: for a lost control chat, `Actions.lost` is set; otherwise the group name, command menu and full deletion are updated when they differ, and members are read ([control.ts:110-117](../src/control.ts#L110-L117)).
12. The HTTP server listens; with the port in use, the process exits with code 1 ([serve.ts:119](../src/serve.ts#L119), [:227-234](../src/serve.ts#L227-L234)). Until step 14 ends, requests to `/mcp` that pass the token check get 503 with `Retry-After: 1` ([http.ts:23](../src/http.ts#L23), [serve.ts:118-119](../src/serve.ts#L118-L119), [:127](../src/serve.ts#L127)).
13. `server.json` is written ([serve.ts:120](../src/serve.ts#L120)).
14. The server runs the owner setup, `Control.start`; after an error, HTTP is closed and the error is rethrown ([serve.ts:121-126](../src/serve.ts#L121-L126)).
15. `Listening on http://127.0.0.1:PORT/mcp` ([:128](../src/serve.ts#L128)).
16. Buffered events are replayed, then live events are dispatched ([:131](../src/serve.ts#L131), [:236-250](../src/serve.ts#L236-L250)).

After an error inside `startAgent` (steps 4-16) the chat is closed; after any error once the lock is taken, the lock is released ([serve.ts:70-86](../src/serve.ts#L70-L86)).

### 4.2 Event dispatch

Each `onAny` receiver is awaited by the library before the next event is read ([api.ts:195-202](../../simplex-chat-nodejs/src/api.ts#L195-L202)). The dispatcher in [`serve.ts:131-167`](../src/serve.ts#L131-L167):

| Event | Condition | Handler |
|---|---|---|
| `newChatItems` | item of the control group, of the main conversation or a member support chat | [`Control.onMessage`](../src/control.ts#L179) |
| `newChatItems` | every item | [`AgentTools.onNewItems`](../src/tools.ts#L560), then [`ChatItemsWatch.deliver`](../src/messages.ts#L116) |
| `chatItemUpdated` | | [`AgentTools.onFile`](../src/tools.ts#L570), then [`ChatItemsWatch.update`](../src/messages.ts#L120) |
| `chatItemsDeleted` | | [`ChatItemsWatch.remove`](../src/messages.ts#L124) of each `deletedChatItem` |
| `contactConnected` | | [`AgentTools.onContactConnected`](../src/tools.ts#L599) |
| `userJoinedGroup` | | [`AgentTools.onUserJoinedGroup`](../src/tools.ts#L604) |
| `rcvFileComplete` | | [`AgentTools.onFileComplete`](../src/tools.ts#L575) |
| `rcvFileSndCancelled` | | [`AgentTools.onFileCancelled`](../src/tools.ts#L587) |
| `rcvFileError` | | [`AgentTools.onFileError`](../src/tools.ts#L582) |
| `rcvFileDescrReady` | | [`AgentTools.onFileDescription`](../src/tools.ts#L594) |
| `sndFileCompleteXFTP` | | [`AgentTools.onSentFile`](../src/tools.ts#L857) |
| `sndFileError` | | [`AgentTools.onSentFile`](../src/tools.ts#L857), then [`Control.fileNotSent`](../src/control.ts#L168) |
| `joinedGroupMember`<br>`joinedGroupMemberConnecting`<br>`connectedToGroupMember`<br>`memberRole`<br>`leftMember`<br>`deletedMember`<br>`deletedMemberUser`<br>`groupDeleted` | event of the control group | [`Control.onEvent`](../src/control.ts#L194), queued one at a time |
| any event | after `stop()` started | ignored |

Owner commands are queued in a second chain, independent of the membership chain ([control.ts:187-191](../src/control.ts#L187-L191)). The deletion of a `/secret` message, also of one in a member support chat, is queued in that chain before its command ([control.ts:182-183](../src/control.ts#L182-L183), [:188](../src/control.ts#L188), [:326-328](../src/control.ts#L326-L328)).

### 4.3 Stop

On `SIGINT` or `SIGTERM` the server runs one stop; a later signal ends its wait for cancelled cards ([serve.ts:48-64](../src/serve.ts#L48-L64)). `RunningServer.stop` ([serve.ts:173-183](../src/serve.ts#L173-L183)):

1. Events are ignored from now on.
2. `Actions.stop` starts: new actions are refused with `The server is stopping.`; the expiry timer is cleared; cards being posted are awaited; pending and performing actions are marked interrupted; the stop signal of the performs is aborted, which ends the wait of a `connect` for its connection; their cards are edited, or posted when they have no card; outcome edits already in progress are awaited ([actions.ts:240-250](../src/actions.ts#L240-L250)).
3. In parallel with step 2, the HTTP server is closed with all connections, and open calls are aborted ([http.ts:48-52](../src/http.ts#L48-L52)).
4. `Cancelled K action(s).` when K > 0 ([serve.ts:178](../src/serve.ts#L178)).
5. When cards were edited or posted at the stop and the control chat is not lost, the server waits `--stop-wait` seconds, 10 by default, for them to be sent; a second signal ends the wait ([serve.ts:179](../src/serve.ts#L179), [:191-205](../src/serve.ts#L191-L205)).
6. `chat.close()`: the chat is stopped, then the store is closed ([api.ts:165-170](../../simplex-chat-nodejs/src/api.ts#L165-L170)).
7. The lock is removed, `Stopped.` is logged, and the process exits with code 0, or 1 after an error ([serve.ts:181-182](../src/serve.ts#L181-L182), [:54-60](../src/serve.ts#L54-L60)).

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

`isRunning`: `true` when `kill(pid, 0)` succeeds or fails with `EPERM` ([files.ts:112-119](../src/files.ts#L112-L119)).

---

## 5. Approval model

### 5.1 Modes

The mode is set by `--approve` for one run ([cli.ts:78-80](../src/cli.ts#L78-L80)).

| Mode | State of a new action | Card | `_meta` of `connect`, `send`, `download`, `leave` |
|---|---|---|---|
| `requests` | `pending`<br>`performing` for a send under a standing approval, and for a download under a files approval | card with commands<br>outcome as a new message after a standing send; none after a download without a card | none |
| `host` | `performing` | outcome as a new message after the perform; none for a download | `{"anthropic/requiresUserInteraction": true}` |

Sources: [actions.ts:166](../src/actions.ts#L166), [tools.ts:503](../src/tools.ts#L503), [control.ts:153-160](../src/control.ts#L153-L160), [:535-537](../src/control.ts#L535-L537). In both modes a new action is refused unless the control chat has an approver ([control.ts:132-135](../src/control.ts#L132-L135)).

### 5.2 Owners and approvers

| Term | Rule | Code |
|---|---|---|
| human | member other than the agent's membership, status `connected` or `complete` | [control.ts:524-526](../src/control.ts#L524-L526), [tools.ts:1404-1406](../src/tools.ts#L1404-L1406) |
| human owner | human with role `owner` | [control.ts:511-513](../src/control.ts#L511-L513) |
| approver, `--owners` absent | member with role `owner` | [control.ts:528-532](../src/control.ts#L528-L532) |
| approver, `--owners` without a list | none | [control.ts:531](../src/control.ts#L531) |
| approver, `--owners ID:NAME,...` | member with role `owner` whose `groupMemberId` and `displayName`, folded to one line by `oneLine`, match an entry | [control.ts:531](../src/control.ts#L531), [:800-802](../src/control.ts#L800-L802) |

The command sender's member record from the chat item is checked ([control.ts:186](../src/control.ts#L186), [:215](../src/control.ts#L215)). The approver count for refusals and standing approvals is taken from `Control.members`, refreshed at load and at each membership event ([control.ts:198](../src/control.ts#L198), [:503-505](../src/control.ts#L503-L505)).

Owner setup at start ([control.ts:119-125](../src/control.ts#L119-L125)):

| `--owners` | Start | First join by the link |
|---|---|---|
| absent | a human owner exists: register hint<br>otherwise: `No owners in the agent control chat.` when humans exist, then a link with role `member` | link deleted, role `owner`, greeting, log line, register hint |
| without a list | no humans: link with role `observer`<br>humans exist: their IDs and the restart command, in the log and the chat | link deleted, the joined member and the restart command announced |
| `ID:NAME,...` | each entry matched to a human, or exit 2<br>role `owner` set where missing<br>other humans removed<br>link deleted<br>`Owners: ...` and register hint | |

Sources: [`startOwners`](../src/control.ts#L381), [`startObserver`](../src/control.ts#L387), [`startListed`](../src/control.ts#L392), [`ownerJoined`](../src/control.ts#L411), [`observerJoined`](../src/control.ts#L420).

Membership changes at run time ([control.ts:194-209](../src/control.ts#L194-L209)):

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
| `/reject`, or `/leave CHAT` for a pending action of CHAT with a card | `pending` | `rejected` | [actions.ts:225-230](../src/actions.ts#L225-L230), [control.ts:308-310](../src/control.ts#L308-L310) |
| expiry time passes | `pending` | `expired` | [actions.ts:196-199](../src/actions.ts#L196-L199) |
| `/approve N`, `/approve CHAT N` and `/approve CHAT all` for other actions; `/receive N`, `/receive CHAT N` and `/receive CHAT N all` for downloads | `pending` | `performing` | [actions.ts:219-223](../src/actions.ts#L219-L223), [control.ts:246-255](../src/control.ts#L246-L255) |
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

An action is removed from memory when its result is returned to a call ([tools.ts:652](../src/tools.ts#L652)), or at an expiry tick when it is decided, its open call count is 0 and its expiry time has passed ([actions.ts:200-201](../src/actions.ts#L200-L201)). A `download` still performing at the end of `wait` is returned as `performing` and kept ([tools.ts:649](../src/tools.ts#L649)).

### 5.4 Cards

The server posts a card when a `pending` action is created, and edits it with `apiUpdateChatItem` at the outcome ([actions.ts:255](../src/actions.ts#L255), [control.ts:153-160](../src/control.ts#L153-L160)). For a `connect`, `send` or `leave` without a card (mode `host`, standing sends) the server posts the outcome as a new message; a `download` without a card gets none ([control.ts:535-537](../src/control.ts#L535-L537)). The text per state is built by [`cardText`](../src/control.ts#L592-L611); a card of an image or video file is an image or video message with the file's preview ([control.ts:481-501](../src/control.ts#L481-L501)); forms are listed in [API: cards](api.md#22-cards).

Outcomes are posted after `done` and `failed` ([actions.ts:282](../src/actions.ts#L282)), after `rejected` and `expired` ([:229](../src/actions.ts#L229), [:199](../src/actions.ts#L199)), and at stop ([:247](../src/actions.ts#L247)). They are skipped after the control chat is lost ([:247](../src/actions.ts#L247), [:282](../src/actions.ts#L282), [:291](../src/actions.ts#L291)).

### 5.5 Standing approvals

| Aspect | Messages | Files | Code |
|---|---|---|---|
| grant | `/approve CHAT all` from an approver in mode `requests`; pending sends of CHAT with a card are approved | `/receive CHAT N all` from an approver, on the card of download N; N is performed, and the other pending downloads of CHAT stay pending | [control.ts:254](../src/control.ts#L254), [:260-271](../src/control.ts#L260-L271) |
| mode `host` | nothing stored; reply `Approvals apply in mode requests; this server runs in mode host.` | downloads get no card, so the command has none to act on | [control.ts:224](../src/control.ts#L224) |
| removal | `/revoke CHAT`: the `simplexMcp` key is removed, other keys kept | `/receive CHAT off`: the `simplexMcpFiles` key is removed | [control.ts:273-280](../src/control.ts#L273-L280) |
| use | mode `requests`, and `approvedById` equal to the `groupMemberId` of a current approver | the same | [control.ts:138-141](../src/control.ts#L138-L141) |
| scope | `send` | `download` of a file whose servers are all known | [tools.ts:805](../src/tools.ts#L805), [:984](../src/tools.ts#L984) |
| decider shown | `safeName(approvedBy)` | the same | [control.ts:140](../src/control.ts#L140) |
| storage | the chat's custom data; removed with the chat by `leave` | the same | [tools.ts:959-967](../src/tools.ts#L959-L967) |

### 5.6 Expiry

| Rule | Code |
|---|---|
| `expires = created + expireHours × 3600000`, `expireHours` 1 to 23, default 23 | [actions.ts:163](../src/actions.ts#L163), [cli.ts:61](../src/cli.ts#L61), [:84-86](../src/cli.ts#L84-L86) |
| check every 60 s on an unreferenced timer, and before each owner command | [actions.ts:129](../src/actions.ts#L129), [control.ts:212](../src/control.ts#L212) |
| pending and due: `expired`, log `Action N expired.`, card edited to `Expired.` | [actions.ts:196-199](../src/actions.ts#L196-L199) |
| decided, no open call, due: removed from memory | [actions.ts:200-201](../src/actions.ts#L200-L201) |
| connect wait for the other side: at most until `created + 24 h - 10 min` | [tools.ts:145](../src/tools.ts#L145), [:698](../src/tools.ts#L698) |

`Date.now()` is compared with `expires`, so due actions are expired at the first tick after a sleep of the computer.

### 5.7 Action counter

| Rule | Code |
|---|---|
| initial value: `nextAction` of the control marker, or 1 | [serve.ts:102](../src/serve.ts#L102) |
| a new action takes `next`, then `next` grows by 1 | [actions.ts:156](../src/actions.ts#L156), [:178](../src/actions.ts#L178) |
| the new `next` is written before the card is posted | [actions.ts:254-255](../src/actions.ts#L254-L255) |
| one write at a time, also after a failed write | [control.ts:143-147](../src/control.ts#L143-L147) |
| `--reset-owners` copies the value into the new control chat | [serve.ts:223-224](../src/serve.ts#L223-L224), [control.ts:562-567](../src/control.ts#L562-L567) |
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

- The token is created once and kept in `DATA/token`; `--new-token` replaces it ([files.ts:34-40](../src/files.ts#L34-L40)).
- `DATA/headers.json` is rewritten at every start ([files.ts:46-48](../src/files.ts#L46-L48)).
- The Claude Code entry gets the command `cat <DATA>/headers.json`, or `type "<DATA>\headers.json"` on Windows, with the path quoted for the shell ([register.ts:50](../src/register.ts#L50), [log.ts:24-31](../src/log.ts#L24-L31)). Only this command is written to the entry and to the `claude mcp add-json` arguments ([register.test.ts:50-61](../test/unit/register.test.ts#L50-L61)).
- `token`, `headers.json` and `server.json` are set to mode 0600 at every write, and the data folder to 0700 at every start, except on Windows ([files.ts:16-19](../src/files.ts#L16-L19), [:88-91](../src/files.ts#L88-L91)).

### 6.3 Peer-written text

| Destination | Rule | Code |
|---|---|---|
| card and reply names | folded to one line; a valid display name is plain, others a code span | [`safeName`](../src/control.ts#L727-L730) |
| card text lines | one code span per line; backticks become `'`; a leading or trailing space becomes U+00A0; blank lines empty | [`codeLines`, `codeSpan`](../src/control.ts#L719-L725) |
| file names and paths on cards and posts | folded to one line, one code span | [control.ts:625](../src/control.ts#L625), [:638](../src/control.ts#L638), [:174-176](../src/control.ts#L174-L176) |
| reply quote on a send card | whitespace runs to one space, first 100 characters (code points), one code span | [control.ts:626](../src/control.ts#L626) |
| link texts on a connect card | code span lines in the bytes that the largest card form leaves; the line that does not fit is cut and ends in `…` inside its code span; later lines left out | [control.ts:684-698](../src/control.ts#L684-L698) |
| mention names | valid name: `@name` or `@'name'`; others a code span | [`mentionText`](../src/control.ts#L732-L735) |
| errors on cards | folded to one line, one code span | [control.ts:605](../src/control.ts#L605) |
| `--owners` part of restart lines in the chat | one code span | [control.ts:87](../src/control.ts#L87), [:824-826](../src/control.ts#L824-L826) |
| terminal names and errors of actions | folded to one line | [actions.ts:180-181](../src/actions.ts#L180-L181), [:333-341](../src/actions.ts#L333-L341), [control.ts:86](../src/control.ts#L86) |
| tool results | returned as data; schema descriptions and instructions mark `text`, `from`, `name`, file names, downloaded files, `description`, `welcome` and `commands` with their labels and params as data, not instructions; the instructions take a chat's messages as prompts only when the user said so in the session, and limit what the agent shares about its user | [tools.ts:186](../src/tools.ts#L186), [:190](../src/tools.ts#L190), [:195](../src/tools.ts#L195), [:220-221](../src/tools.ts#L220-L221), [:231-236](../src/tools.ts#L231-L236), [:404](../src/tools.ts#L404), [:447](../src/tools.ts#L447) |

The server writes the tappable form `/'...'` only in the card's command line ([control.ts:643-656](../src/control.ts#L643-L656)), in `/'revoke CHAT'` of standing cards and replies ([:603](../src/control.ts#L603), [:270](../src/control.ts#L270)), in `/'delete CHAT files'` of the kept-files message ([:318](../src/control.ts#L318)), and in `/'unsecret NAME'` of the reply to `/secret`, with NAME of capital letters, digits and `_` only ([:346](../src/control.ts#L346), [:350](../src/control.ts#L350)).

### 6.4 Chat references

| Rule | Code |
|---|---|
| `^[@#][1-9]\d{0,14}$`: 1 to 15 digits; the core reads `#(2^64 + N)` as `#N` | [messages.ts:150-152](../src/messages.ts#L150-L152), [scenarios.test.ts:491-503](../test/e2e/scenarios.test.ts#L491-L503) |
| the control chat is refused by `send`, `receive`, `download`, `leave` and the owner commands with CHAT, and left out of `list` and `/list` | [tools.ts:1125](../src/tools.ts#L1125), [:1170-1187](../src/tools.ts#L1170-L1187), [control.ts:368-375](../src/control.ts#L368-L375) |
| a store error of the chat read is reported as `No chat CHAT.` | [tools.ts:1130](../src/tools.ts#L1130), [:1185](../src/tools.ts#L1185) |
| the agent's own links are refused by `connect` (`ownLink` plans) | [tools.ts:1289](../src/tools.ts#L1289), [:1307](../src/tools.ts#L1307), [:1329](../src/tools.ts#L1329) |

### 6.5 Owner commands

| Rule | Code |
|---|---|
| source: `groupRcv` text items in the main scope of the control group | [serve.ts:136](../src/serve.ts#L136), [control.ts:181](../src/control.ts#L181), [:183](../src/control.ts#L183) |
| form: one line `/keyword params`, leading spaces allowed | [control.ts:585-590](../src/control.ts#L585-L590) |
| the help reply is sent to any sender; other commands are accepted from approvers only | [control.ts:214-215](../src/control.ts#L214-L215) |
| commands run one at a time in arrival order | [control.ts:187-191](../src/control.ts#L187-L191) |
| `/approve`, `/reject`, `/receive` and `/forward` need a pending action with a card, and the action's chat when CHAT is given; `/receive` and `/forward` take downloads only, `/approve` the other actions only | [control.ts:244-254](../src/control.ts#L244-L254) |
| a `groupRcv` text item whose first word is `/secret`, in any letter case, of any sender, in the main scope or a member support chat, is deleted for all members before its command runs, and its reply is a new message without a quote | [control.ts:182-190](../src/control.ts#L182-L190) |

### 6.6 Files

| Rule | Code |
|---|---|
| `send` refuses files named `id_*`, `*.pem`, `*.key`, `*.p12` or `*.pfx`, and files under `~/.ssh`, `~/.gnupg`, the secrets folder, the data folder and `~/.simplex-mcp` except the agent's own `DATA/files/`, each by its real path; the refusal is posted to the control chat and logged | [tools.ts:871-885](../src/tools.ts#L871-L885), [:1475-1494](../src/tools.ts#L1475-L1494), [control.ts:162-166](../src/control.ts#L162-L166) |
| the file of a `send` is sent only when its real path, size, inode and change time at the perform are those of the request | [tools.ts:887-892](../src/tools.ts#L887-L892) |
| a sent JPEG or PNG is a resized copy made by the core | [tools.ts:894-905](../src/tools.ts#L894-L905) |
| `ffmpeg` and `ffprobe` read a video as MP4 or QuickTime, local files only, without standard input, 30 s and 64 MiB of output each | [tools.ts:1544-1562](../src/tools.ts#L1544-L1562) |
| a download is saved only at `DATA/files/group-N/` or `DATA/files/contact-N/`, or for `/forward` at the same name under `DATA/forwarded/`, named by its file ID and an extension of 1 to 16 ASCII letters and digits | [messages.ts:215-223](../src/messages.ts#L215-L223) |
| a forwarded file is outside `DATA/files/`, so `send` refuses it; a download moves it into `DATA/files/` after the copy for the post is made | [tools.ts:531-548](../src/tools.ts#L531-L548), [:880-885](../src/tools.ts#L880-L885) |
| a file on unknown servers gets a card that names them, also under a files approval; relays are approved only for such a card, and for every other download, and in mode `host`, the core checks the complete description and downloads a file on unknown servers only when the IP address is protected | [tools.ts:984](../src/tools.ts#L984), [:554](../src/tools.ts#L554), [:1039](../src/tools.ts#L1039), [control.ts:651-654](../src/control.ts#L651-L654) |
| an accepted file that the core returns to `rcvInvitation` fails its download at the next check | [tools.ts:1052](../src/tools.ts#L1052) |
| a received file is moved into the chat's folder at its completion, unless it is at the forwarded path, also from a member chat of a group, which has a download in memory as the main conversation does; a file without a download in memory, such as one whose transfer started before a restart, only when it has its full size; a partial file is deleted only for a download in memory, when it is no larger than the file | [tools.ts:1091-1100](../src/tools.ts#L1091-L1100), [:1496-1502](../src/tools.ts#L1496-L1502) |

### 6.7 Link and network

- The join link is a group link of the control chat with role `member`, or `observer` with `--owners`; it is printed with a QR code on stdout and deleted at the first join event ([control.ts:446-456](../src/control.ts#L446-L456), [:412](../src/control.ts#L412), [:421](../src/control.ts#L421)).
- At every start the server sets private message routing: `smpProxyMode` `always`, `smpProxyFallback` `allowProtected` ([serve.ts:93-94](../src/serve.ts#L93-L94)).
- With `incognito: true` the join methods connect incognito ([tools.ts:730](../src/tools.ts#L730)); the core then uses a random profile for that chat ([PLAN, Participants](../PLAN.md#participants)).

### 6.8 Secrets

With the secrets, values are protected from accidental exposure to the model provider. A prompt injection that makes the agent send a value away by reference, such as in a command that reads `SECRETS/NAME`, is out of scope.

| Rule | Code |
|---|---|
| a value is written only by an approver's `/secret`, to `SECRETS/NAME`, mode 0600 | [control.ts:233](../src/control.ts#L233), [:345-351](../src/control.ts#L345-L351), [files.ts:72-77](../src/files.ts#L72-L77) |
| a value is absent from the log, from the replies and from tool results: the log line and the replies name the secret, and `list` returns the names and the folder | [control.ts:349-350](../src/control.ts#L349-L350), [tools.ts:1166-1167](../src/tools.ts#L1166-L1167) |
| a `/secret` message is deleted for all members before its command, in the main conversation and in member support chats; with full deletion, its content is replaced in the agent's database and in the members' apps | [control.ts:182-190](../src/control.ts#L182-L190), [:18](../src/control.ts#L18), [:113-115](../src/control.ts#L113-L115), [Commands.hs:4294-4297](../../../src/Simplex/Chat/Library/Commands.hs#L4294-L4297), [Messages.hs:2857-2875](../../../src/Simplex/Chat/Store/Messages.hs#L2857-L2875) |
| `send` refuses files under the secrets folder, by its real path | [tools.ts:883-884](../src/tools.ts#L883-L884) |
| `/unsecret` deletes only a name among `names()`, so a name with `/` or `..` deletes nothing | [files.ts:79-81](../src/files.ts#L79-L81) |
| the secrets folder is disjoint from the data folder | [cli.ts:112-113](../src/cli.ts#L112-L113) |
| with the opencode plugin, the forms of the values are replaced in the parts of the messages that opencode passes to the hook | [simplex-secrets.ts:15-21](../opencode/simplex-secrets.ts#L15-L21) |

Limits:

- Owners' apps show a `/secret` message until its deletion reaches them.
- A message edited into a `/secret` command, and the later text of a live message, are neither deleted nor run: `onMessage` is called for new items only, not at `chatItemUpdated` ([serve.ts:134-143](../src/serve.ts#L134-L143)).
- The agent forwards the messages of the control chat to members as an owner; the forwarded messages are stored with their delivery job in `DATA/simplex_chat.db` until the job is done and 7 days old ([Subscriber.hs:1020-1027](../../../src/Simplex/Chat/Library/Subscriber.hs#L1020-L1027), [:3793-3796](../../../src/Simplex/Chat/Library/Subscriber.hs#L3793-L3796), [:4231-4236](../../../src/Simplex/Chat/Library/Subscriber.hs#L4231-L4236), [Delivery.hs:251-263](../../../src/Simplex/Chat/Store/Delivery.hs#L251-L263), [:400-409](../../../src/Simplex/Chat/Store/Delivery.hs#L400-L409), [Commands.hs:6555-6558](../../../src/Simplex/Chat/Library/Commands.hs#L6555-L6558)).
- Every process of the server's user can read the files; the agent's commands read them.
- Values that commands print are sent to the model provider by the MCP host, except for the forms replaced with the opencode plugin ([REFERENCE, Redaction](../REFERENCE.md#redaction)).
- `send` and `connect` texts are not checked for values; the approval card shows them.

---

## 7. Limits

| Limit | Value | Code |
|---|---|---|
| pending actions | 20 | [actions.ts:11](../src/actions.ts#L11) |
| actions being performed | 20 | [actions.ts:12](../src/actions.ts#L12) |
| expiry | 1 to 23 h, default 23 | [cli.ts:61](../src/cli.ts#L61), [:84-86](../src/cli.ts#L84-L86) |
| connect wait for the other side | `created + 23 h 50 min` | [tools.ts:145](../src/tools.ts#L145) |
| send, connect and download card | 15000 bytes JSON-escaped, in its largest form | [control.ts:31](../src/control.ts#L31), [:662-670](../src/control.ts#L662-L670) |
| error assumed in the largest card | 600 × `€` | [control.ts:35](../src/control.ts#L35) |
| message as the core sends it | 15602 bytes, counted with 600 bytes of envelope; `send` refuses a longer one, and the post of `/forward` leaves out its quote of the card | [tools.ts:155-156](../src/tools.ts#L155-L156), [:1443-1446](../src/tools.ts#L1443-L1446), [control.ts:282-298](../src/control.ts#L282-L298) |
| `send.text` | 1 to 12000 characters | [tools.ts:258](../src/tools.ts#L258) |
| `connect.message` | 1 to 2000 characters | [tools.ts:246](../src/tools.ts#L246) |
| `send.mention` | 1 to 3 unique member IDs | [tools.ts:262](../src/tools.ts#L262) |
| `wait` | 0 to 86400 s | [tools.ts:247](../src/tools.ts#L247), [:263](../src/tools.ts#L263), [:275](../src/tools.ts#L275), [:298](../src/tools.ts#L298) |
| `idle` | 1 to 300 s, default 30 | [tools.ts:248](../src/tools.ts#L248), [:264](../src/tools.ts#L264), [:151](../src/tools.ts#L151) |
| `limit` | 1 to 100, default 50 | [tools.ts:265](../src/tools.ts#L265), [:276](../src/tools.ts#L276), [:152](../src/tools.ts#L152) |
| `connect` snapshot | last 50 items | [tools.ts:153](../src/tools.ts#L153) |
| collected items per action or call | 100, then their IDs kept as overflow until deleted | [messages.ts:10](../src/messages.ts#L10), [:109-110](../src/messages.ts#L109-L110) |
| unread page | 100 items | [messages.ts:9](../src/messages.ts#L9) |
| chat list read | one read of 1000000 chats | [tools.ts:144](../src/tools.ts#L144) |
| `download` wait, default | 60 s | [tools.ts:148](../src/tools.ts#L148) |
| download check | every 60 s | [tools.ts:149](../src/tools.ts#L149) |
| download without progress | 30 min, then cancelled | [tools.ts:150](../src/tools.ts#L150) |
| `ffmpeg`, `ffprobe` | 30 s, 64 MiB of output | [tools.ts:154](../src/tools.ts#L154), [:158](../src/tools.ts#L158) |
| stop wait | 0 to 60 s, default 10 | [cli.ts:66](../src/cli.ts#L66), [:87-89](../src/cli.ts#L87-L89) |
| error details | 500 characters | [log.ts:4](../src/log.ts#L4) |
| progress interval | 60 s | [messages.ts:7](../src/messages.ts#L7) |
| expiry tick | 60 s | [actions.ts:10](../src/actions.ts#L10) |
| HTTP body | 4 MiB | [server index.cjs:254](../node_modules/@modelcontextprotocol/server/dist/index.cjs#L254) |
| SSE keep-alive | 15 s | [mcp-BumlNux0.cjs:64](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L64) |
| agent NAME | 1 to 32 characters | [cli.ts:157](../src/cli.ts#L157) |
| display name | 50 characters, `mkValidName` rules | [messages.ts:11-12](../src/messages.ts#L11-L12), [:337-359](../src/messages.ts#L337-L359) |
| port | 1 to 65535, default 5226 | [cli.ts:64](../src/cli.ts#L64), [:96-98](../src/cli.ts#L96-L98) |
| secret name | 1 to 64 characters: a capital letter, then capital letters, digits and `_` | [files.ts:14](../src/files.ts#L14) |
| secret value | 8 characters (code points) or more, one line | [control.ts:346](../src/control.ts#L346), [:585-590](../src/control.ts#L585-L590) |
