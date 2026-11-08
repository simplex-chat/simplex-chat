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
4. The handler result is returned by `AgentTools.call` as `structuredContent` and as JSON text ([`tools.ts:547-554`](../src/tools.ts#L547-L554)).
5. The server calls `ChatApi` methods of the core ([`messages.ts:271-287`](../src/messages.ts#L271-L287)); actions are created in `Actions` and shown to the owner by `Control` ([`actions.ts:187-222`](../src/actions.ts#L187-L222)).
6. The core emits events; the handler of `serve.ts` passes them to the agent control chat and to `AgentTools.onEvent`, which passes them to the tools and the item watch ([`serve.ts:125-137`](../src/serve.ts#L125-L137), [`tools.ts:671-698`](../src/tools.ts#L671-L698)).

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
| `Agent` | [117](../src/tools.ts#L117) | `ChatApi`, user, `Actions`, `ChatItemsWatch`, control chat reference, standing approver lookup, refused file post, data folder; for the plugin also the data root, the owning session ID, the group of `reply`, the check that only owners are in it, the mode setter and the record of a pending connection |
| `JoinMethod` | [134](../src/tools.ts#L134) | `prepared`, `prepareChannel`, `viaAddress`, `prepareContact`, `connect` |
| schemas | [190-413](../src/tools.ts#L190-L413) | input and output JSON schemas |
| descriptions, `instructions` | [415-453](../src/tools.ts#L415-L453) | tool descriptions and server instructions; `instructions` names the host whose restart cancels actions, `server` by default |
| `replyTool` | [463](../src/tools.ts#L463) | the plugin's seventh tool, `reply` |
| `definitions` | [488](../src/tools.ts#L488) | name, description, schemas and action flag of the six tools |
| `registerTools` | [501](../src/tools.ts#L501) | `registerTool` for each definition and each extra one; `_meta` on action tools in mode `host` |
| `Download` | [513](../src/tools.ts#L513) | one download: its promise, chat, item, file ID, size, path, the last fileSource seen, received bytes and their time, check timer |
| `AgentTools` | [530](../src/tools.ts#L530) | constructor with the `Agent`, the mode or a function returning it, and the session number of the plugin; `mode` reads it at each use |
| `AgentTools.register` | [543](../src/tools.ts#L543) | `registerTools` with `AgentTools.call` |
| `AgentTools.call` | [547](../src/tools.ts#L547) | handler result to `CallToolResult`; errors to `isError` |
| `AgentTools.perform` | [556](../src/tools.ts#L556) | perform step of an approved action; for a `reply` action, the file sent; for a `mode` action, the mode setter |
| `giveChat`, `owns`, `claim` | [574-592](../src/tools.ts#L574-L592) | the plugin's chat owners: a chat is the session's when no session ID is set or the chat's owner is that ID; `claim` sets the owner of a chat without one, at a connection, at a `connect` to a known chat ([`ownChat`](../src/tools.ts#L760-L765)) and for a pending connection from the hub |
| `onEvent` | [671](../src/tools.ts#L671) | events to the file handlers, connect completions and the item watch; shared by `serve` and the plugin's sessions |
| `reply`, `replyResult`, `sendFile` | [719-740](../src/tools.ts#L719-L740) | a file to the group of the plugin's session, under the file rules of `send`: at once while only owners are in the group, otherwise after the approval of a `reply` action in mode `requests`; the action's result; the post of the file |
| `downloadFile`, `forwardFile`, `downloadTo` | [593-620](../src/tools.ts#L593-L620) | one download per file ID, shared by `download` and `/forward`; a forwarded file moved to the download path for `download` |
| file events | [622-659](../src/tools.ts#L622-L659) | `onNewItems`, `onFile`, `onFileComplete`, `onFileError`, `onFileCancelled`, `onFileDescription` |
| `onContactConnected`, `onUserJoinedGroup` | [661-669](../src/tools.ts#L661-L669) | completion of connect actions by events |
| `actionResult` | [771](../src/tools.ts#L771) | wait for the decision, result fields, removal |
| `attachment`, `isRefused`, `fileToSend`, `resizedCopy` | [997-1033](../src/tools.ts#L997-L1033) | the file of a `send` |
| `copyForSending`, `chatFiles`, `deleteChatFiles` | [1035-1064](../src/tools.ts#L1035-L1064) | `/forward` copies; the files of a chat, forwarded ones included; its downloads cancelled, also those the core continues after a restart |
| `leaveChat` | [1086](../src/tools.ts#L1086) | perform step of `leave` and `/leave`; the chat's forwarded files deleted |
| `download`, `fileServers`, `unknownServers` | [1096-1141](../src/tools.ts#L1096-L1141) | the `download` call before its action |
| `startDownload`, `checkDownload`, `currentFile`, `settleFile`, `settleReceived`, `moveReceived` | [1159-1226](../src/tools.ts#L1159-L1226) | a download's steps |
| `agentChats`, `knownChat` | [1296-1315](../src/tools.ts#L1296-L1315) | the agent's chats; a checked chat reference |
| `classifyPlan` | [1398](../src/tools.ts#L1398) | `apiConnectPlan` response to a join plan or a known chat |
| `chatSubject`, `isConnected`, `isActive`, `isGone`, `memberCurrent` | [1494-1564](../src/tools.ts#L1494-L1564) | chat and member predicates shared with `control.ts` and `hub.ts`; `isGone`: status `rejected`, `removed`, `left` or `deleted` |
| `newestGroup` | [1566](../src/tools.ts#L1566) | the group with the highest ID of a list: the control chat of `serve`, the Main group, and the session group where `User` posts |
| `customDataOf`, `setChatData`, `ownerOf` | [1527-1542](../src/tools.ts#L1527-L1542) | a chat's custom data; the plugin's owner key `simplexMcpOwner` |
| `fitsMessage`, `quoteContent` | [1592-1608](../src/tools.ts#L1592-L1608) | the message as the core sends it, within 15602 bytes; for `send` and the post of `/forward` |
| `realFile` | [1624](../src/tools.ts#L1624) | real path, size, inode and change time of a file |
| `attachmentMedia`, `videoFrame`, `videoDuration` | [1671-1705](../src/tools.ts#L1671-L1705) | previews and durations |
| `toolCall` | [1721](../src/tools.ts#L1721) | `ServerContext` to `ToolCall`: abort signal and progress notifications |

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
| `ActionRecord` | [37](../src/actions.ts#L37) | number, session number of the plugin, mode, key, times, change signal, standing flag, state, card, decider, open calls, interrupted flag, ended flag, error, card image |
| `ConnectAction`, `SendAction`, `LeaveAction`, `DownloadAction` | [57-125](../src/actions.ts#L57-L125) | per-tool fields; `Attachment`, the file of a send |
| `ReplyAction`, `ModeAction`, `PermissionAction` | [98](../src/actions.ts#L98), [127-140](../src/actions.ts#L127-L140) | the plugin's actions: a file of `reply` to the session's group, with its item once sent; a switch to mode `host`; a tool use of Claude Code, with the bytes left for its input preview |
| `ActionHooks` | [146](../src/actions.ts#L146) | `refusal`, `saveCounter`, `postCard`, `perform`, `outcome` |
| `Actions` | [154](../src/actions.ts#L154) | action map, one expiry timer per action, posting and outcome promises |
| `create` | [187](../src/actions.ts#L187) | state by the action's mode, refusal checks, number, log line, counter write, card |
| `ofSession`, `remove`, `openCall`, `closeCall`, `end` | [224-248](../src/actions.ts#L224-L248) | the actions of one session; removal; open call count; an expiry before the expiry time, marked ended |
| `expireDue` | [250](../src/actions.ts#L250) | expiry of every action past its expiry time |
| `decided` | [256](../src/actions.ts#L256) | wait with progress until a decided state, `until`, or the abort signal |
| `stop` | [291](../src/actions.ts#L291) | cancellation of pending and performing actions |
| `scheduleExpiry`, `expire`, `forgetExpired` | [304-322](../src/actions.ts#L304-L322) | the expiry timer of one action; its expiry; its removal once decided, without open calls and past its expiry time |
| `summary`, `performedText`, `progressText` | [388-433](../src/actions.ts#L388-L433) | log and progress texts |

Hooks are bound in [`serve.ts:96-102`](../src/serve.ts#L96-L102): `refusal`, `saveCounter`, `postCard` and `outcome` to `Control`; `perform` to `AgentTools`.

### [2.6 `control.ts`](../src/control.ts)

| Symbol | Line | Contents |
|---|---|---|
| `controlCommands` | [11](../src/control.ts#L11) | the command menu |
| `standings` | [25](../src/control.ts#L25) | custom data key and reply words of the approvals of messages and files |
| `ControlData` | [39](../src/control.ts#L39) | `{control: true, nextAction}` |
| `ChatApproval` | [62](../src/control.ts#L62) | `{approvedBy, approvedById, approvedAt}` |
| `ControlOptions`, `SessionOptions` | [44-55](../src/control.ts#L44-L55) | display name, `--owners`, register hint, QR code size; for a group of the plugin's session, its approver check and the session IDs by number |
| `OwnerRequest` | [68](../src/control.ts#L68) | parsed owner command |
| `Control.load` | [105](../src/control.ts#L105) | lost check, group profile and menu, member list; the custom data in memory kept across the profile update |
| `Control.rename`, `Control.describe` | [114-122](../src/control.ts#L114-L122) | the plugin's group name; its short description, the custom data in memory kept |
| `Control.start` | [124](../src/control.ts#L124) | owner setup per `--owners` |
| `Control.refusal` | [132](../src/control.ts#L132) | bytes left for the link texts of a connect card and for the input preview of a tool-use card, card size of a send, connect or download, approver presence |
| `Control.hasApprover` | [145](../src/control.ts#L145) | an approver is a member |
| `Control.onlyApprovers` | [149](../src/control.ts#L149) | the members read again; each one is the group's own membership, the member of the given contact, gone, or an approver |
| `Control.standingApprover` | [155](../src/control.ts#L155) | approver name of a chat's standing approval of messages or files |
| `Control.saveCounter`, `Control.saveData` | [160-172](../src/control.ts#L160-L172) | queued writes of the counter and of other keys of the group's custom data |
| `Control.postCard`, `Control.outcome` | [174-185](../src/control.ts#L174-L185) | card post and card edit; the kept-files message after a done leave |
| `Control.refusedFile`, `Control.fileNotSent` | [187-202](../src/control.ts#L187-L202) | posts about files |
| `Control.onMessage` | [204](../src/control.ts#L204) | owner commands, queued one at a time; in a group of the plugin's session, an approver's one-line text with a keyword outside `ownerKeywords` is left to the session |
| `Control.onEvent` | [217](../src/control.ts#L217) | membership changes and loss of the control chat |
| `Control.ownerCommand` | [235](../src/control.ts#L235) | command dispatch and replies |
| `approveChat`, `revoke`, `forward`, `leave`, `deleteFiles` | [280-357](../src/control.ts#L280-L357) | owner requests |
| `give` | [359](../src/control.ts#L359) | `/give CHAT N` in a group of the plugin's session |
| `isApprover` | [552](../src/control.ts#L552) | the session's check in a group of the plugin's session; otherwise role `owner`, and the `--owners` list when given |
| `cardContent`, `fittedImage` | [505-525](../src/control.ts#L505-L525) | image and video cards with the file's preview |
| `findControlGroup`, `createControlChat`, `resetControlChat` | [564-593](../src/control.ts#L564-L593) | control group lookup, creation, reset |
| `createGroup`, `deleteGroup` | [584-598](../src/control.ts#L584-L598) | a group with its profile and custom data, shared with the plugin's groups; deletion for all members |
| `isGroupMessage` | [600](../src/control.ts#L600) | an item of one group's main scope |
| `isControlEvent` | [604](../src/control.ts#L604) | membership events of the control group |
| `parseCommand`, `isOwnerCommand` | [620-630](../src/control.ts#L620-L630) | one-line `/keyword params`; such a line with a keyword of `ownerKeywords` ([19](../src/control.ts#L19)) |
| `cardText`, `requestLines`, `commandsLine` | [632-708](../src/control.ts#L632-L708) | card text per tool, state and mode |
| `largestCard`, `encodedLength` | [714-722](../src/control.ts#L714-L722) | card size check |
| `truncatedLine`, `fittedLines` | [724-750](../src/control.ts#L724-L750) | a line cut with `…`; the code lines that fit a byte budget, the last one cut |
| `serversLine`, `sizeText` | [756-765](../src/control.ts#L756-L765) | servers line and sizes of download cards |
| `codeSpan`, `safeName`, `mentionText` | [775-787](../src/control.ts#L775-L787) | quoting of peer text |
| `ownerRequest` | [798](../src/control.ts#L798) | command words to an `OwnerRequest` |
| `losesControl`, `isLost` | [814-829](../src/control.ts#L814-L829) | loss of the control chat, also of the plugin's groups |
| `joinedText` | [843](../src/control.ts#L843) | `NAME joined (member N)`, shared with the Main group |
| `entryText`, `restartText` | [847-857](../src/control.ts#L847-L857) | `--owners` entries in restart lines |

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
| `createDataFolder` | [99](../src/files.ts#L99) | `mkdir -p` mode 0700; `chmod 0700` except on Windows |
| `acquireLock`, `releaseLock` | [104-118](../src/files.ts#L104-L118) | `DATA/lock` with the PID; `user`, `server` or `daemon`, names the holder in the error |
| `serverToken` | [120](../src/files.ts#L120) | `DATA/token`: read, made private when the user owns it, or 32 random bytes in base64url, created as the lock is |
| `saveHeldTitles`, `takeHeldTitles` | [149-163](../src/files.ts#L149-L163) | `ROOT/titles.json`, written at a daemon stop, read and removed at the next start; an unparsable file is skipped with a log line |
| `createLock`, `createExclusive` | [178-200](../src/files.ts#L178-L200) | a private temporary file with the text, linked into place, so that the file appears complete and once; where the file system refuses the link, written exclusively |
| `takeOver` | [202](../src/files.ts#L202) | the lock of a stopped process: claimed first by a link of the lock to `lock.PID.claim`, or where links fail by an exclusive write of `lock.PID.claim`, so that one starter claims it; when the lock still holds that PID, it is removed and created again; the claim removed |
| `writeHeadersFile` | [136](../src/files.ts#L136) | `DATA/headers.json` |
| `writeServerFile`, `readServerFile` | [140-147](../src/files.ts#L140-L147) | `DATA/server.json` |

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
| Actions | `Actions.actions` map ([actions.ts:158](../src/actions.ts#L158)) | `create`, `remove`, `forgetExpired` | tools, `Control` |
| Expiry timers | `Actions.expiries`, one per action ([actions.ts:159](../src/actions.ts#L159)) | `scheduleExpiry`, `expire`, `remove` | |
| Open calls of an action | `ActionRecord.open` ([:49](../src/actions.ts#L49)) | `openCall` and `closeCall` from `actionResult` ([tools.ts:772](../src/tools.ts#L772), [:781](../src/tools.ts#L781)) | `list`, `forgetExpired` |
| Collected replies | `ConnectAction.messages`, `SendAction.responses` ([:62](../src/actions.ts#L62), [:93](../src/actions.ts#L93)) | `ChatItemsWatch.collect` | `connect`, `send` results |
| Item listeners | `ChatItemsWatch.listeners` ([messages.ts:87](../src/messages.ts#L87)) | `collect` | event dispatch |
| Downloads | `AgentTools.downloads`, one per file ID ([tools.ts:531](../src/tools.ts#L531)) | `downloadTo` | file events, checks, `/delete CHAT files` |
| Forwards | `AgentTools.forwarding`, the copy for the post per file ID ([tools.ts:532](../src/tools.ts#L532)) | `forwardFile` | `downloadFile`, before it moves a forwarded file |
| Rejected files | `AgentTools.rejectedFiles` ([tools.ts:533](../src/tools.ts#L533)) | `downloadResult` ([:1148-1149](../src/tools.ts#L1148-L1149)) | the file field; `download`, which then skips the files approval ([:1111](../src/tools.ts#L1111)) |
| Count of file descriptions | `AgentTools.describedFiles` ([tools.ts:534-535](../src/tools.ts#L534-L535)) | `onFileDescription` | `fileServers` |
| Members of the control chat | `Control.members` ([control.ts:85](../src/control.ts#L85)) | `refreshMembers` ([:527-529](../src/control.ts#L527-L529)) | approver checks |
| Mode, expiry, owners list, stop wait | `ServeOptions` of the run; the mode also in `Actions.mode` and in each action ([actions.ts:40](../src/actions.ts#L40)) | CLI | all modules |

### 3.2 Chat database custom data

| Item | Entity | Value | Written by | Read by |
|---|---|---|---|---|
| Control marker and counter | agent control chat | `{simplexMcp: {control: true, nextAction: N}}` | [`createControlChat`](../src/control.ts#L573-L588), [`saveCounter`](../src/control.ts#L160) | [`findControlGroup`](../src/control.ts#L564), [`controlDataOf`](../src/control.ts#L568) |
| Standing approval of messages | contact or group of the agent | `{...other keys, simplexMcp: {approvedBy, approvedById, approvedAt}}` | [`approveChat`](../src/control.ts#L280), [`revoke`](../src/control.ts#L293) | [`standingApprover`](../src/control.ts#L155) |
| Files approval | contact or group of the agent | `{...other keys, simplexMcpFiles: {approvedBy, approvedById, approvedAt}}` | [`approveChat`](../src/control.ts#L280), [`revoke`](../src/control.ts#L293) | [`standingApprover`](../src/control.ts#L155) |

The profile, chats, items, members and file transfers are stored by the core in `DATA/simplex_chat.db`; connections and queues in `DATA/simplex_agent.db` (file prefix `DATA/simplex`, [serve.ts:71](../src/serve.ts#L71)).

### 3.3 Files

| Path | Contents | Mode | Written | Read |
|---|---|---|---|---|
| `DATA/` | data folder | 0700, set at every start except on Windows | [files.ts:99-102](../src/files.ts#L99-L102) | |
| `DATA/token` | base64url of 32 random bytes | 0600, set again at every write except on Windows | first start, `--new-token` ([:120-130](../src/files.ts#L120-L130)) | every start |
| `DATA/headers.json` | `{"Authorization":"Bearer <token>"}` | 0600, set again at every write except on Windows | every start ([:136-138](../src/files.ts#L136-L138)) | Claude Code `headersHelper` |
| `DATA/server.json` | `{name, port, expireHours}` | 0600, set again at every write except on Windows | every start, after the HTTP listener is bound ([serve.ts:114](../src/serve.ts#L114)) | `register` |
| `DATA/lock` | PID and a newline | 0600 | start ([files.ts:178-217](../src/files.ts#L178-L217)) | next start |
| `DATA/tmp/` | the core's temp folder: files in transfer | 0700, set at every start | [core.ts:41](../src/core.ts#L41), [:78-82](../src/core.ts#L78-L82) | the core |
| `DATA/files/group-N/`, `DATA/files/contact-N/` | downloaded files and files received without a download, named by file ID and extension | folder 0700 | the core at the download path ([tools.ts:1164-1165](../src/tools.ts#L1164-L1165)); moves ([:1659-1669](../src/tools.ts#L1659-L1669)) | `download` results, `/forward`, `/leave`, `/delete CHAT files` |
| `DATA/forwarded/group-N/`, `DATA/forwarded/contact-N/` | files `/forward` downloaded, named as above, until a download moves them to `DATA/files/` | folder 0700 | the core at the forwarded path ([tools.ts:603-610](../src/tools.ts#L603-L610), [:1164-1165](../src/tools.ts#L1164-L1165)) | `/forward`; moved by `download` ([:593-601](../src/tools.ts#L593-L601)); deleted by `leave`, `/leave` and `/delete CHAT files` ([:1046-1064](../src/tools.ts#L1046-L1064), [:1086-1094](../src/tools.ts#L1086-L1094)) |
| `DATA/files/sent/` | resized copies of sent images, and copies of the files `/forward` posts, one folder per copy | folder 0700; a resized copy 0600 | [tools.ts:1022-1039](../src/tools.ts#L1022-L1039), [:1066-1070](../src/tools.ts#L1066-L1070) | the core; deleted at `sndFileCompleteXFTP`, `sndFileError` and at start ([core.ts:42](../src/core.ts#L42), [tools.ts:694-696](../src/tools.ts#L694-L696), [:985](../src/tools.ts#L985)) |

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
11. `Control.load`: for a lost control chat, `Actions.lost` is set; otherwise the group name and command menu are updated when they differ, and members are read ([control.ts:105-112](../src/control.ts#L105-L112)).
12. The HTTP server listens; with the port in use, the process exits with code 1 ([serve.ts:113](../src/serve.ts#L113), [:184-191](../src/serve.ts#L184-L191)). Until step 14 ends, requests to `/mcp` that pass the token check get 503 with `Retry-After: 1` ([http.ts:23](../src/http.ts#L23), [serve.ts:112-113](../src/serve.ts#L112-L113), [:121](../src/serve.ts#L121)).
13. `server.json` is written ([serve.ts:114](../src/serve.ts#L114)).
14. The server runs the owner setup, `Control.start`; after an error, HTTP is closed and the error is rethrown ([serve.ts:115-120](../src/serve.ts#L115-L120)).
15. `Listening on http://127.0.0.1:PORT/mcp` ([:122](../src/serve.ts#L122)).
16. Buffered events are replayed, then live events are dispatched ([serve.ts:125](../src/serve.ts#L125), [core.ts:62-76](../src/core.ts#L62-L76)).

After an error inside `startAgent` (steps 4-16) the chat is closed; after any error once the lock is taken, the lock is released ([serve.ts:68-84](../src/serve.ts#L68-L84)).

### 4.2 Event dispatch

Each `onAny` receiver is awaited by the library before the next event is read ([api.ts:195-202](../../simplex-chat-nodejs/src/api.ts#L195-L202)). The dispatcher in [`serve.ts:125-137`](../src/serve.ts#L125-L137) passes items of the control group to `Control`, then every event to [`AgentTools.onEvent`](../src/tools.ts#L671-L698), which runs the `AgentTools` and `ChatItemsWatch` handlers below, then file errors and membership events to `Control`:

| Event | Condition | Handler |
|---|---|---|
| `newChatItems` | item of the control group, main scope | [`Control.onMessage`](../src/control.ts#L204) |
| `newChatItems` | every item | [`AgentTools.onNewItems`](../src/tools.ts#L622), then [`ChatItemsWatch.deliver`](../src/messages.ts#L116) |
| `chatItemUpdated` | | [`AgentTools.onFile`](../src/tools.ts#L632), then [`ChatItemsWatch.update`](../src/messages.ts#L120) |
| `chatItemsDeleted` | | [`ChatItemsWatch.remove`](../src/messages.ts#L124) of each `deletedChatItem` |
| `contactConnected` | | [`AgentTools.onContactConnected`](../src/tools.ts#L661) |
| `userJoinedGroup` | | [`AgentTools.onUserJoinedGroup`](../src/tools.ts#L666) |
| `rcvFileComplete` | | [`AgentTools.onFileComplete`](../src/tools.ts#L637) |
| `rcvFileSndCancelled` | | [`AgentTools.onFileCancelled`](../src/tools.ts#L649) |
| `rcvFileError` | | [`AgentTools.onFileError`](../src/tools.ts#L644) |
| `rcvFileDescrReady` | | [`AgentTools.onFileDescription`](../src/tools.ts#L656) |
| `sndFileCompleteXFTP` | | [`AgentTools.onSentFile`](../src/tools.ts#L985) |
| `sndFileError` | | [`AgentTools.onSentFile`](../src/tools.ts#L985), then [`Control.fileNotSent`](../src/control.ts#L193) |
| `joinedGroupMember`<br>`joinedGroupMemberConnecting`<br>`connectedToGroupMember`<br>`memberRole`<br>`leftMember`<br>`deletedMember`<br>`deletedMemberUser`<br>`groupDeleted` | event of the control group | [`Control.onEvent`](../src/control.ts#L217), queued one at a time |
| any event | after `stop()` started | ignored |

Owner commands are queued in a second chain, independent of the membership chain ([control.ts:211-214](../src/control.ts#L211-L214)).

### 4.3 Stop

On `SIGINT` or `SIGTERM` the server runs one stop; a later signal ends its wait for cancelled cards ([serve.ts:46-62](../src/serve.ts#L46-L62)). `RunningServer.stop` ([serve.ts:143-153](../src/serve.ts#L143-L153)):

1. Events are ignored from now on.
2. `Actions.stop` starts: new actions are refused with `The server is stopping.`; the expiry timers are cleared; cards being posted are awaited; pending and performing actions are marked interrupted; the stop signal of the performs is aborted, which ends the wait of a `connect` for its connection; their cards are edited, or posted when they have no card; outcome edits already in progress are awaited ([actions.ts:291-322](../src/actions.ts#L291-L322)).
3. In parallel with step 2, the HTTP server is closed with all connections, and open calls are aborted ([http.ts:48-52](../src/http.ts#L48-L52)).
4. `Cancelled K action(s).` when K > 0 ([serve.ts:148](../src/serve.ts#L148)).
5. When cards were edited or posted at the stop and the control chat is not lost, the server waits `--stop-wait` seconds, 10 by default, for them to be sent; a second signal ends the wait ([serve.ts:149](../src/serve.ts#L149), [:161-175](../src/serve.ts#L161-L175)).
6. `chat.close()`: the chat is stopped, then the store is closed ([api.ts:165-170](../../simplex-chat-nodejs/src/api.ts#L165-L170)).
7. The lock is removed, `Stopped.` is logged, and the process exits with code 0, or 1 after an error ([serve.ts:151-152](../src/serve.ts#L151-L152), [:52-58](../src/serve.ts#L52-L58)).

Performs in progress are not awaited; when one ends, its card stays as edited at the stop and no line is logged, because the action is interrupted ([actions.ts:352](../src/actions.ts#L352)).

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

`isRunning`: `true` when `kill(pid, 0)` succeeds or fails with `EPERM` ([files.ts:224-231](../src/files.ts#L224-L231)).

---

## 5. Approval model

### 5.1 Modes

The mode is set by `--approve` for one run ([cli.ts:96-98](../src/cli.ts#L96-L98)).

| Mode | State of a new action | Card | `_meta` of `connect`, `send`, `download`, `leave` |
|---|---|---|---|
| `requests` | `pending`<br>`performing` for a send under a standing approval, and for a download under a files approval | card with commands<br>outcome as a new message after a standing send; none after a download without a card | none |
| `host` | `performing` | outcome as a new message after the perform; none for a download | `{"anthropic/requiresUserInteraction": true}` |

An action keeps the mode it was created in, `ActionRecord.mode`; its card and its log line follow that mode ([actions.ts:187](../src/actions.ts#L187), [:200](../src/actions.ts#L200), [control.ts:133](../src/control.ts#L133)).

Sources: [actions.ts:200](../src/actions.ts#L200), [tools.ts:507](../src/tools.ts#L507), [control.ts:178-185](../src/control.ts#L178-L185), [:560-562](../src/control.ts#L560-L562). In both modes a new action is refused unless the control chat has an approver ([control.ts:138-142](../src/control.ts#L138-L142)).

### 5.2 Owners and approvers

| Term | Rule | Code |
|---|---|---|
| human | member other than the agent's membership, status `connected` or `complete` | [control.ts:548-550](../src/control.ts#L548-L550), [tools.ts:1544-1546](../src/tools.ts#L1544-L1546) |
| human owner | human with role `owner` | [control.ts:535-537](../src/control.ts#L535-L537) |
| approver, `--owners` absent | member with role `owner` | [control.ts:552-557](../src/control.ts#L552-L557) |
| approver, `--owners` without a list | none | [control.ts:556](../src/control.ts#L556) |
| approver, `--owners ID:NAME,...` | member with role `owner` whose `groupMemberId` and `displayName`, folded to one line by `oneLine`, match an entry | [control.ts:556](../src/control.ts#L556), [:831-833](../src/control.ts#L831-L833) |

The command sender's member record from the chat item is checked ([control.ts:209](../src/control.ts#L209), [:239](../src/control.ts#L239)). The approver count for refusals and standing approvals is taken from `Control.members`, refreshed at load and at each membership event ([control.ts:221](../src/control.ts#L221), [:527-529](../src/control.ts#L527-L529)).

Owner setup at start ([control.ts:124-130](../src/control.ts#L124-L130)):

| `--owners` | Start | First join by the link |
|---|---|---|
| absent | a human owner exists: register hint<br>otherwise: `No owners in the agent control chat.` when humans exist, then a link with role `member` | link deleted, role `owner`, greeting, log line, register hint |
| without a list | no humans: link with role `observer`<br>humans exist: their IDs and the restart command, in the log and the chat | link deleted, the joined member and the restart command announced |
| `ID:NAME,...` | each entry matched to a human, or exit 2<br>role `owner` set where missing<br>other humans removed<br>link deleted<br>`Owners: ...` and register hint | |

Sources: [`startOwners`](../src/control.ts#L405), [`startObserver`](../src/control.ts#L411), [`startListed`](../src/control.ts#L416), [`ownerJoined`](../src/control.ts#L435), [`observerJoined`](../src/control.ts#L444).

Membership changes at run time ([control.ts:217-233](../src/control.ts#L217-L233)):

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
| call creates the action, mode `requests` | | `pending` | [actions.ts:200](../src/actions.ts#L200) |
| call creates the action, mode `host` | | `performing` | [actions.ts:200](../src/actions.ts#L200) |
| `send` call under a standing approval, or `download` call under a files approval | | `performing` | [actions.ts:200](../src/actions.ts#L200) |
| counter write or card post fails | `pending`<br>`performing` | `failed` | [actions.ts:324-335](../src/actions.ts#L324-L335) |
| stop arrives while the counter of a `performing` action is written | `performing` | `pending`, interrupted | [actions.ts:337-342](../src/actions.ts#L337-L342) |
| control chat lost | `pending` | `failed` | [actions.ts:283-289](../src/actions.ts#L283-L289) |
| `/reject`, or `/leave CHAT` for a pending action of CHAT with a card | `pending` | `rejected` | [actions.ts:276-281](../src/actions.ts#L276-L281), [control.ts:341-343](../src/control.ts#L341-L343) |
| expiry time passes | `pending` | `expired` | [actions.ts:309-318](../src/actions.ts#L309-L318) |
| `/approve N`, `/approve CHAT N` and `/approve CHAT all` for other actions; `/receive N`, `/receive CHAT N` and `/receive CHAT N all` for downloads | `pending` | `performing` | [actions.ts:270-274](../src/actions.ts#L270-L274), [control.ts:266-275](../src/control.ts#L266-L275) |
| perform throws | `performing` | `failed` | [actions.ts:348-351](../src/actions.ts#L348-L351) |
| perform ends | `performing` | `done` | [actions.ts:346-347](../src/actions.ts#L346-L347) |

`create` ([actions.ts:187-222](../src/actions.ts#L187-L222)):

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

An action is removed from memory when its result is returned to a call ([tools.ts:778](../src/tools.ts#L778)), or once it is decided, its open call count is 0 and its expiry time has passed: checked at each decision, at the close of each call and at its expiry timer ([actions.ts:320-322](../src/actions.ts#L320-L322), [:239-242](../src/actions.ts#L239-L242), [:357-361](../src/actions.ts#L357-L361)). `serve` has one session, `undefined`, so the limits of 20 count all its actions. A `download` still performing at the end of `wait` is returned as `performing` and kept ([tools.ts:775](../src/tools.ts#L775)).

### 5.4 Cards

The server posts a card when a `pending` action is created, and edits it with `apiUpdateChatItem` at the outcome ([actions.ts:327](../src/actions.ts#L327), [control.ts:178-185](../src/control.ts#L178-L185)). For a `connect`, `send` or `leave` without a card (mode `host`, standing sends) the server posts the outcome as a new message; a `download` without a card gets none ([control.ts:560-562](../src/control.ts#L560-L562)). The text per state is built by [`cardText`](../src/control.ts#L632-L651); a card of a `send` or `download` of an image or video file is an image or video message with the file's preview, and a `reply` card is text ([control.ts:505-525](../src/control.ts#L505-L525)); forms are listed in [API: cards](api.md#22-cards).

Outcomes are posted after `done` and `failed` ([actions.ts:354](../src/actions.ts#L354)), after `rejected` and `expired` ([:280](../src/actions.ts#L280), [:317](../src/actions.ts#L317)), and at stop ([:299](../src/actions.ts#L299)). They are skipped after the control chat is lost ([:299](../src/actions.ts#L299), [:354](../src/actions.ts#L354), [:364](../src/actions.ts#L364)).

### 5.5 Standing approvals

| Aspect | Messages | Files | Code |
|---|---|---|---|
| grant | `/approve CHAT all` from an approver in mode `requests`; pending sends of CHAT with a card are approved | `/receive CHAT N all` from an approver, on the card of download N; N is performed, and the other pending downloads of CHAT stay pending | [control.ts:274](../src/control.ts#L274), [:280-291](../src/control.ts#L280-L291) |
| mode `host` | nothing stored; reply `Approvals apply in mode requests; this server runs in mode host.` | downloads get no card, so the command has none to act on | [control.ts:248](../src/control.ts#L248) |
| removal | `/revoke CHAT`: the `simplexMcp` key is removed, other keys kept | `/receive CHAT off`: the `simplexMcpFiles` key is removed | [control.ts:293-313](../src/control.ts#L293-L313) |
| use | mode `requests`, and `approvedById` equal to the `groupMemberId` of a current approver | the same | [control.ts:155-158](../src/control.ts#L155-L158) |
| scope | `send` | `download` of a file whose servers are all known | [tools.ts:934](../src/tools.ts#L934), [:1111](../src/tools.ts#L1111) |
| decider shown | `safeName(approvedBy)` | the same | [control.ts:157](../src/control.ts#L157) |
| storage | the chat's custom data; removed with the chat by `leave` | the same | [tools.ts:1086-1094](../src/tools.ts#L1086-L1094) |

### 5.6 Expiry

| Rule | Code |
|---|---|
| `expires = created + expireHours × 3600000`, `expireHours` 1 to 23, default 23 | [actions.ts:197](../src/actions.ts#L197), [cli.ts:79](../src/cli.ts#L79), [:102-104](../src/cli.ts#L102-L104) |
| one unreferenced timer per action, set at its creation for `expires - now`, at most `maxTimer` ms; all actions past `expires` are also expired before each owner command | [actions.ts:304-307](../src/actions.ts#L304-L307), [messages.ts:8](../src/messages.ts#L8), [control.ts:236](../src/control.ts#L236) |
| pending and due: `expired`, log `Action N expired.`, card edited to `Expired.` | [actions.ts:309-318](../src/actions.ts#L309-L318) |
| decided, no open call, due: removed from memory | [actions.ts:320-322](../src/actions.ts#L320-L322) |
| connect wait for the other side: at most until `created + 24 h - 10 min` | [tools.ts:154](../src/tools.ts#L154), [:824](../src/tools.ts#L824) |

A timer follows the monotonic clock and `expires` the wall clock: a timer that fires before `expires` is set again ([actions.ts:313](../src/actions.ts#L313)).

### 5.7 Action counter

| Rule | Code |
|---|---|
| initial value: `nextAction` of the control marker, or 1 | [serve.ts:96](../src/serve.ts#L96) |
| a new action takes `next`, then `next` grows by 1 | [actions.ts:188](../src/actions.ts#L188), [:213](../src/actions.ts#L213) |
| the new `next` is written before the card is posted | [actions.ts:326-327](../src/actions.ts#L326-L327) |
| one write at a time, also after a failed write | [control.ts:160-172](../src/control.ts#L160-L172) |
| `--reset-owners` copies the value into the new control chat | [serve.ts:180-181](../src/serve.ts#L180-L181), [control.ts:590-593](../src/control.ts#L590-L593) |
| a refused call leaves the counter unchanged | [actions.ts:212](../src/actions.ts#L212) |

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

- The token is created once and kept in `DATA/token`; `--new-token` replaces it ([files.ts:120-130](../src/files.ts#L120-L130)).
- `DATA/headers.json` is rewritten at every start ([files.ts:136-138](../src/files.ts#L136-L138)).
- The Claude Code entry gets the command `cat <DATA>/headers.json`, or `type "<DATA>\headers.json"` on Windows, with the path quoted for the shell ([register.ts:50](../src/register.ts#L50), [log.ts:24-31](../src/log.ts#L24-L31)). Only this command is written to the entry and to the `claude mcp add-json` arguments ([register.test.ts:50-61](../test/unit/register.test.ts#L50-L61)).
- `token`, `headers.json` and `server.json` are set to mode 0600 at every write, and the data folder to 0700 at every start, except on Windows ([files.ts:99-102](../src/files.ts#L99-L102), [:169-172](../src/files.ts#L169-L172)).

### 6.3 Peer-written text

| Destination | Rule | Code |
|---|---|---|
| card and reply names | folded to one line; a valid display name is plain, others a code span | [`safeName`](../src/control.ts#L779-L782) |
| card text lines | one code span per line; backticks become `'`; a leading or trailing space becomes U+00A0; blank lines empty | [`codeLines`, `codeSpan`](../src/control.ts#L771-L777) |
| file names and paths on cards and posts | folded to one line, one code span | [control.ts:670](../src/control.ts#L670), [:687](../src/control.ts#L687), [:199-201](../src/control.ts#L199-L201) |
| reply quote on a send card | whitespace runs to one space, first 100 characters (code points), one code span | [control.ts:671](../src/control.ts#L671) |
| link texts on a connect card, the input preview on a tool-use card of the plugin | code span lines in the bytes that the largest card form leaves; the line that does not fit is cut and ends in `…` inside its code span; later lines left out | [control.ts:736-750](../src/control.ts#L736-L750) |
| mention names | valid name: `@name` or `@'name'`; others a code span | [`mentionText`](../src/control.ts#L784-L787) |
| errors on cards | folded to one line, one code span | [control.ts:645](../src/control.ts#L645) |
| `--owners` part of restart lines in the chat | one code span | [control.ts:82](../src/control.ts#L82), [:855-857](../src/control.ts#L855-L857) |
| terminal names and errors of actions | folded to one line | [actions.ts:216-217](../src/actions.ts#L216-L217), [:410-420](../src/actions.ts#L410-L420), [control.ts:81](../src/control.ts#L81) |
| tool results | returned as data; schema descriptions and instructions mark `text`, `from`, `name`, file names, downloaded files, `description`, `welcome` and `commands` with their labels and params as data, not instructions; the instructions take a chat's messages as prompts only when the user said so in the session, and limit what the agent shares about its user | [tools.ts:196](../src/tools.ts#L196), [:200](../src/tools.ts#L200), [:205](../src/tools.ts#L205), [:230-231](../src/tools.ts#L230-L231), [:241-246](../src/tools.ts#L241-L246), [:409](../src/tools.ts#L409), [:451](../src/tools.ts#L451) |

The server writes the tappable form `/'...'` only in the card's command line ([control.ts:692-708](../src/control.ts#L692-L708)), in `/'revoke CHAT'` of standing cards and replies ([:643](../src/control.ts#L643), [:290](../src/control.ts#L290)), and in `/'delete CHAT files'` of the kept-files message ([:351](../src/control.ts#L351)).

### 6.4 Chat references

| Rule | Code |
|---|---|
| `^[@#][1-9]\d{0,14}$`: 1 to 15 digits; the core reads `#(2^64 + N)` as `#N` | [messages.ts:150-152](../src/messages.ts#L150-L152), [scenarios.test.ts:491-503](../test/e2e/scenarios.test.ts#L491-L503) |
| the control chat is refused by `send`, `receive`, `download`, `leave` and the owner commands with CHAT, and left out of `list` and `/list` | [tools.ts:1251](../src/tools.ts#L1251), [:1296-1315](../src/tools.ts#L1296-L1315), [control.ts:392-399](../src/control.ts#L392-L399) |
| a store error of the chat read is reported as `No chat CHAT.` | [tools.ts:1256](../src/tools.ts#L1256), [:1313](../src/tools.ts#L1313) |
| the agent's own links are refused by `connect` (`ownLink` plans) | [tools.ts:1417](../src/tools.ts#L1417), [:1435](../src/tools.ts#L1435), [:1457](../src/tools.ts#L1457) |

### 6.5 Owner commands

| Rule | Code |
|---|---|
| source: `groupRcv` text items in the main scope of the control group | [serve.ts:129](../src/serve.ts#L129), [control.ts:206](../src/control.ts#L206), [:600-602](../src/control.ts#L600-L602) |
| form: one line `/keyword params`, leading spaces allowed | [control.ts:620-625](../src/control.ts#L620-L625) |
| the help reply is sent to any sender, with `/give` in a plugin session group; other commands are accepted from approvers only | [control.ts:238-239](../src/control.ts#L238-L239) |
| commands run one at a time in arrival order | [control.ts:211-214](../src/control.ts#L211-L214) |
| `/approve`, `/reject`, `/receive` and `/forward` need a pending action with a card, and the action's chat when CHAT is given; `/receive` and `/forward` take downloads only, `/approve` the other actions only | [control.ts:264-274](../src/control.ts#L264-L274) |

### 6.6 Files

| Rule | Code |
|---|---|
| `send` refuses files named `id_*`, `*.pem`, `*.key`, `*.p12` or `*.pfx`, and files under `~/.ssh`, `~/.gnupg`, the data folder and `~/.simplex-mcp` except the agent's own `DATA/files/`, each by its real path; the refusal is posted to the control chat and logged | [tools.ts:999-1013](../src/tools.ts#L999-L1013), [:1624-1643](../src/tools.ts#L1624-L1643), [control.ts:187-191](../src/control.ts#L187-L191) |
| the file of a `send` is sent only when its real path, size, inode and change time at the perform are those of the request | [tools.ts:1015-1020](../src/tools.ts#L1015-L1020) |
| a sent JPEG or PNG is a resized copy made by the core | [tools.ts:1022-1033](../src/tools.ts#L1022-L1033) |
| `ffmpeg` and `ffprobe` read a video as MP4 or QuickTime, local files only, without standard input, 30 s and 64 MiB of output each | [tools.ts:1693-1711](../src/tools.ts#L1693-L1711) |
| a download is saved only at `DATA/files/group-N/` or `DATA/files/contact-N/`, or for `/forward` at the same name under `DATA/forwarded/`, named by its file ID and an extension of 1 to 16 ASCII letters and digits | [messages.ts:215-227](../src/messages.ts#L215-L227) |
| a forwarded file is outside `DATA/files/`, so `send` refuses it; a download moves it into `DATA/files/` after the copy for the post is made | [tools.ts:593-610](../src/tools.ts#L593-L610), [:1008-1013](../src/tools.ts#L1008-L1013) |
| a file on unknown servers gets a card that names them, also under a files approval; relays are approved only for such a card, and for every other download, and in mode `host`, the core checks the complete description and downloads a file on unknown servers only when the IP address is protected | [tools.ts:1111](../src/tools.ts#L1111), [:616](../src/tools.ts#L616), [:1165](../src/tools.ts#L1165), [control.ts:703-706](../src/control.ts#L703-L706) |
| an accepted file that the core returns to `rcvInvitation` fails its download at the next check | [tools.ts:1178](../src/tools.ts#L1178) |
| a received file is moved into the chat's folder at its completion, unless it is at the forwarded path, also from a member chat of a group, which has a download in memory as the main conversation does; a file without a download in memory, such as one whose transfer started before a restart, only when it has its full size; a partial file is deleted only for a download in memory, when it is no larger than the file | [tools.ts:1217-1226](../src/tools.ts#L1217-L1226), [:1645-1651](../src/tools.ts#L1645-L1651) |

### 6.7 Link and network

- The join link is a group link of the control chat with role `member`, or `observer` with `--owners`; it is printed with a QR code on stdout and deleted at the first join event ([control.ts:470-480](../src/control.ts#L470-L480), [:436](../src/control.ts#L436), [:445](../src/control.ts#L445)).
- At every start the server sets private message routing: `smpProxyMode` `always`, `smpProxyFallback` `allowProtected` ([core.ts:39-40](../src/core.ts#L39-L40)).
- With `incognito: true` the join methods connect incognito ([tools.ts:858](../src/tools.ts#L858)); the core then uses a random profile for that chat ([PLAN, Participants](../PLAN.md#participants)).

---

## 7. Limits

| Limit | Value | Code |
|---|---|---|
| pending actions | 20 | [actions.ts:9](../src/actions.ts#L9) |
| actions being performed | 20 | [actions.ts:10](../src/actions.ts#L10) |
| expiry | 1 to 23 h, default 23 | [cli.ts:79](../src/cli.ts#L79), [:102-104](../src/cli.ts#L102-L104) |
| connect wait for the other side | `created + 23 h 50 min` | [tools.ts:154](../src/tools.ts#L154) |
| send, connect and download card | 15000 bytes JSON-escaped, in its largest form | [control.ts:29](../src/control.ts#L29), [:714-722](../src/control.ts#L714-L722) |
| error assumed in the largest card | 600 × `€` | [control.ts:33](../src/control.ts#L33) |
| message as the core sends it | 15602 bytes, counted with 600 bytes of envelope; `send` refuses a longer one, and the post of `/forward` leaves out its quote of the card | [tools.ts:164-165](../src/tools.ts#L164-L165), [:1592-1595](../src/tools.ts#L1592-L1595), [control.ts:315-331](../src/control.ts#L315-L331) |
| `send.text` | 1 to 12000 characters | [tools.ts:268](../src/tools.ts#L268) |
| `connect.message` | 1 to 2000 characters | [tools.ts:256](../src/tools.ts#L256) |
| `send.mention` | 1 to 3 unique member IDs | [tools.ts:272](../src/tools.ts#L272) |
| `wait` | 0 to 86400 s | [tools.ts:257](../src/tools.ts#L257), [:273](../src/tools.ts#L273), [:285](../src/tools.ts#L285), [:308](../src/tools.ts#L308) |
| `idle` | 1 to 300 s, default 30 | [tools.ts:258](../src/tools.ts#L258), [:274](../src/tools.ts#L274), [:160](../src/tools.ts#L160) |
| `limit` | 1 to 100, default 50 | [tools.ts:275](../src/tools.ts#L275), [:286](../src/tools.ts#L286), [:161](../src/tools.ts#L161) |
| `connect` snapshot | last 50 items | [tools.ts:162](../src/tools.ts#L162) |
| collected items per action or call | 100, then their IDs kept as overflow until deleted | [messages.ts:10](../src/messages.ts#L10), [:109-110](../src/messages.ts#L109-L110) |
| unread page | 100 items | [messages.ts:9](../src/messages.ts#L9) |
| chat list read | one read of 1000000 chats | [tools.ts:153](../src/tools.ts#L153) |
| `download` wait, default | 60 s | [tools.ts:157](../src/tools.ts#L157) |
| download check | every 60 s | [tools.ts:158](../src/tools.ts#L158) |
| download without progress | 30 min, then cancelled | [tools.ts:159](../src/tools.ts#L159) |
| `ffmpeg`, `ffprobe` | 30 s, 64 MiB of output | [tools.ts:163](../src/tools.ts#L163), [:167](../src/tools.ts#L167) |
| stop wait | 0 to 60 s, default 10 | [cli.ts:84](../src/cli.ts#L84), [:105-107](../src/cli.ts#L105-L107) |
| error details | 500 characters | [log.ts:4](../src/log.ts#L4) |
| progress interval | 60 s | [messages.ts:7](../src/messages.ts#L7) |
| longest expiry timer | 2147483647 ms, set again when it fires early | [messages.ts:8](../src/messages.ts#L8), [actions.ts:305](../src/actions.ts#L305) |
| HTTP body | 4 MiB | [server index.cjs:254](../node_modules/@modelcontextprotocol/server/dist/index.cjs#L254) |
| SSE keep-alive | 15 s | [mcp-BumlNux0.cjs:64](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L64) |
| agent NAME | 1 to 32 characters | [cli.ts:170](../src/cli.ts#L170) |
| display name | 50 characters, `mkValidName` rules | [messages.ts:11-12](../src/messages.ts#L11-L12), [:341-363](../src/messages.ts#L341-L363) |
| port | 1 to 65535, default 5226 | [cli.ts:82](../src/cli.ts#L82), [:114-116](../src/cli.ts#L114-L116) |

---

## 8. Claude Code plugin

The plugin runs the same tools, `Actions` and `Control` as `serve`, in other processes. ROOT is the plugin's data root, `~/.simplex-mcp` or `SIMPLEX_MCP_HOME` ([files.ts:33-36](../src/files.ts#L33-L36)).

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
  hk -->|"node dist/cli.js hook ..."| link
  sk -->|"node dist/cli.js link, reset-owners"| link
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

1. Claude Code calls a tool of the MCP server `simplex`; the SDK validates the arguments ([session.ts:57](../src/session.ts#L57), [tools.ts:501-511](../src/tools.ts#L501-L511)).
2. Without a connection, the session process checks the daemon's status and version, spawns it when it is not running, sends `POST /session` and holds `POST /attach` open ([session.ts:158-180](../src/session.ts#L158-L180), [client.ts:35-47](../src/client.ts#L35-L47)).
3. The session process sends `POST /call` with the session ID, the tool and the input, relays each `{progress}` line as an MCP progress notification, and returns the `{result}` line ([session.ts:129-148](../src/session.ts#L129-L148)).
4. The daemon checks the token, finds or opens the entry of the session or of its alias, and runs the session's `AgentTools.call` with a signal tied to the request ([socket.ts:18-48](../src/socket.ts#L18-L48), [daemon.ts:342-361](../src/daemon.ts#L342-L361)).
5. `AgentTools` sends chat commands to the core `_hub`; actions are created in the session's `Actions` and posted as cards in the session's group by its `Control` ([hub.ts:75-106](../src/hub.ts#L75-L106)).
6. The core emits events to `Hub.onEvent`, which passes them to the Main group, to each session's `AgentTools.onEvent` and to the `Control` of the session group they belong to ([hub.ts:499-529](../src/hub.ts#L499-L529)).

Data flow of a prompt from the session group:

1. An owner's message in the group of session N reaches `Session.onMessage`, which queues its delivery after the earlier prompts of the session ([hub.ts:503-508](../src/hub.ts#L503-L508), [:156-162](../src/hub.ts#L156-L162)).
2. In the first 30 s after the hub opened, a delivery without an attachment waits for one. With an attachment, its file, when it has one, is received. Then, without an attachment, the hub replies `Session N is not running.`; with one, the prompt is pushed, and the line `{prompt}` is written to every `/attach` stream of the session ([hub.ts:254-268](../src/hub.ts#L254-L268), [daemon.ts:330](../src/daemon.ts#L330)).
3. The session process sends it to Claude Code as `notifications/claude/channel` ([session.ts:53-56](../src/session.ts#L53-L56), [:192-196](../src/session.ts#L192-L196)).
4. The hooks post the terminal prompts and the final answer of each turn with `POST /turn` ([hook.ts:59-78](../src/hook.ts#L59-L78), [hub.ts:182-201](../src/hub.ts#L182-L201)).

### 8.2 Modules

#### [`session.ts`](../src/session.ts)

| Symbol | Line | Contents |
|---|---|---|
| `channelInstructions`, `permissionRequest` | [21-23](../src/session.ts#L21-L23) | instructions of the channel; schema of a permission request |
| `startSession` | [40](../src/session.ts#L40) | session ID check, the text of a run that is not interactive, options, `McpServer` with the instructions and the capabilities `claude/channel` and `claude/channel/permission`, tools with `reply`, the permission request handler, stdio transport, background connect unless a text of the run or the options answers every call |
| `DaemonLink.connect`, `start`, `close`, `reconnect` | [77-109](../src/session.ts#L77-L109) | one shared connect promise; `start` logs a failure on stderr and, unless it is a `ProtocolError`, connects again later; `close` ends the attachment and the retries; `reconnect` sets one timer that starts again after the delay, 1 s, and doubles it when it fires, up to 30 s |
| `DaemonLink.permission` | [111](../src/session.ts#L111) | `POST /permission` after the connect |
| `DaemonLink.call` | [116](../src/session.ts#L116) | forward; one repeat after status 409; failures to tool errors |
| `DaemonLink.forward` | [129](../src/session.ts#L129) | `/call` stream: progress lines, the result line; a broken stream drops the connection |
| `DaemonLink.disconnect` | [151](../src/session.ts#L151) | drops the connection only when it is the current one |
| `DaemonLink.open` | [158](../src/session.ts#L158) | `ensureDaemon`, `/session` with the parent process ID, `/attach` until its first line; each line to the channel; the end of `/attach` to `reconnect` |
| `failureText` | [183](../src/session.ts#L183) | not running or status 503: `The SimpleX daemon stopped; repeat the call.` |
| `channelNotification` | [192](../src/session.ts#L192) | attach lines to `notifications/claude/channel` and `notifications/claude/channel/permission` |

#### [`client.ts`](../src/client.ts)

| Symbol | Line | Contents |
|---|---|---|
| `DaemonClient.forRoot` | [22](../src/client.ts#L22) | ROOT created, token, socket path |
| `DaemonClient.ensureDaemon` | [35](../src/client.ts#L35) | status; a daemon of a lower version is stopped; a spawn when none runs; protocol check |
| `spawnDaemon` | [71](../src/client.ts#L71) | detached child with stdout and stderr to `daemon.log`, a ready pipe on fd 3, and the environment variables of `daemonVariables` ([13](../src/client.ts#L13)) |
| `firstLine` | [94](../src/client.ts#L94) | first line of the ready pipe, a spawn error, or the 5-minute timeout |
| `compareVersions` | [120](../src/client.ts#L120) | numeric comparison of the first three version parts |

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
| `Owners`, `OwnerState` | [12-18](../src/protocol.ts#L12-L18) | the owners' names; with the Main group link, for `/title` and `/link` only |
| `SessionRequest`, `TitleRequest` | [20-35](../src/protocol.ts#L20-L35) | `/session`, `/title` |
| `PermissionRequest`, `TurnRequest` | [37-53](../src/protocol.ts#L37-L53) | `/permission`, `/turn` |
| `AttachLine` | [55](../src/protocol.ts#L55) | `{prompt}` and `{permission}` lines of `/attach` |
| `TitleReply`, `SessionReply`, `AttachRequest`, `CallRequest`, `CallLine` | [60-81](../src/protocol.ts#L60-L81) | replies and `/call` |
| `StatusReply`, `ResetReply` | [83-91](../src/protocol.ts#L83-L91) | `/status`, `/reset-owners` |

#### [`daemon.ts`](../src/daemon.ts)

| Symbol | Line | Contents |
|---|---|---|
| `HubLike`, `SessionLike`, `DaemonDeps` | [27-58](../src/daemon.ts#L27-L58) | interfaces the daemon uses; tests replace the hub |
| `Entry` | [60](../src/daemon.ts#L60) | opening promise, session, open attachments, their writers, running calls, idle timer, the session server's parent process ID and the time it was last seen |
| `Daemon.start` | [98](../src/daemon.ts#L98) | lock, stored sessions with their aliases and the next number, a closing time for each stored session without one, listener before the hub opens, hub with the stored sessions that have a group, stop timer, the titles held at the last stop, the deletion of closed groups at once and hourly with the log trim |
| `deleteClosedGroups`, `deleteGroupOf`, `markClosed` | [139-160](../src/daemon.ts#L139-L160) | the group of each stored session closed for 7 days, without an entry and not being deleted, while the daemon runs: deleted by the hub, `group` and `closed` removed from `session.json`, a log line; the deletion kept in `deleting` while it runs; a failed deletion logged as `group of session N`, the next session's deletion started; the closing time written into `session.json` |
| `Daemon.routes`, `withHub`, `expiring` | [168-206](../src/daemon.ts#L168-L206) | the ten routes; requests wait for the hub; due actions expire first |
| `Daemon.shutdown` | [208](../src/daemon.ts#L208) | stop sequence; the held titles saved |
| `onSession`, `onTitle`, `applyTitle` | [233-278](../src/daemon.ts#L233-L278) | options checked; registration with the parent process ID, named by a held title of its ID, and its signal; a title of a new ID from a process without a known session, or from a process whose title is held, held until the server of that process registers, 10 s at most, and applied only while the daemon runs; a session opened or created at its title, its name and model, and the process of the title recorded |
| `canonical`, `resolve`, `attachedSession`, `processSession`, `alias` | [280-311](../src/daemon.ts#L280-L311) | the session of an ID: its alias; for an ID with `session.json`, the ID, or for a hook the attached session of its Claude Code process, without an alias; else the session of the same Claude Code process, recorded as an alias in `session.json`; the wait of a session server for the title of its process |
| `rename` | [313](../src/daemon.ts#L313) | a new name of the session, written into `session.json` as read after the group's rename |
| `onAttach`, `onCall`, `hold` | [320-372](../src/daemon.ts#L320-L372) | attachment with the session's line writer; call stream; streams refused while stopping |
| `entry`, `refuseWhenStopping`, `folder`, `openAgent` | [374-424](../src/daemon.ts#L374-L424) | session registry; status 503 while stopping, 400 for an ID that is not a session ID, 409 for an unknown one without a request; a running deletion of the session's group awaited before `session.json` is read; a new number and name, and the aliases that already point to the new session; the closing time of a stored session removed when it opens |
| `scheduleIdle`, `scheduleStop` | [426-445](../src/daemon.ts#L426-L445) | idle close of a session with its closing time, idle stop of the daemon |
| `runDaemon` | [453](../src/daemon.ts#L453) | process entry: ROOT, start, ready line, signal handlers; for a held lock, a new start once it is released, `running` when its holder answers |
| `productionDeps` | [479](../src/daemon.ts#L479) | token, `Hub.open` |
| `processEvent` | [489](../src/daemon.ts#L489) | the next event of a PID on `titled` or `registered`, 10 s at most |
| `lockHolder` | [501](../src/daemon.ts#L501) | up to 150 checks, 100 ms apart: `released` when ROOT/lock is gone, `answers` for any reply of the socket but ENOENT and ECONNREFUSED |
| `sessionName` | [516](../src/daemon.ts#L516) | session title, else the project folder name, else `Claude Code` |
| `validConfig` | [510](../src/daemon.ts#L510) | `config` of `/session` and `/title`: a valid Main group name, the mode, hours from 1 to 23; else status 400 |
| `readyChannel` | [522](../src/daemon.ts#L522) | ready line to fd `SIMPLEX_MCP_READY_FD`, or stdout |

#### [`hub.ts`](../src/hub.ts)

| Symbol | Line | Contents |
|---|---|---|
| constants | [18-28](../src/hub.ts#L18-L28) | profile name `Claude`, session data key, first action number, 10 s stop wait, 60 s echo of a raw prompt, 12000-byte parts, 20 pushed prompts, 10 minutes for a prompt's file, 1 day for a pending connection, 30 s of held prompts after the hub opens, the channel tags of this plugin |
| `hubTexts` | [31](../src/hub.ts#L31) | refusal texts of a lost session group and of a stop |
| `roles` | [29](../src/hub.ts#L29) | member roles, lowest first, for the roles a Main group member carries into the session groups |
| `SessionGroupData`, `InvitedMember` | [38-48](../src/hub.ts#L38-L48) | session number; members invited through the Main group with their Main group member; pending connections of `connect` with their time; the approved mode |
| `Session` | [50](../src/hub.ts#L50) | one Claude Code session from its `session.json` and its group's data: `Actions` with the hub's texts and the mode of the group's data, `requests` without one, `AgentTools` with the session's ID and folder and the check that only owners are in the group, `Control` of the session group with the session's approver check, the stored model and effort |
| `Session.busy` | [122](../src/hub.ts#L122) | an action other than a `mode` card in memory |
| `Session.requestMode`, `askMode` | [135-154](../src/hub.ts#L135-L154) | `requests` at once, pending `mode` cards ended, `hostDeclined` cleared; `host` remembered and asked as a `mode` action while the group has an approver and `hostDeclined` is unset; a rejected or expired card forgets it, a rejected one sets `hostDeclined` |
| `Session.onLine` | [113-120](../src/hub.ts#L113-L120) | the writer of the session's attachments; a change wakes the held prompts |
| `Session.onMessage` | [156-162](../src/hub.ts#L156-L162) | commands to `Control`; other messages of approvers to the `delivery` chain, in arrival order |
| `Session.claimPending` | [164](../src/hub.ts#L164) | a connection of `connect` recorded in the last day that completed: its record removed, its chat claimed |
| `Session.permission` | [171](../src/hub.ts#L171) | a `permission` action in mode `requests`; its verdict to the attachments; removal when decided |
| `Session.turn` | [182](../src/hub.ts#L182) | before any await: at a prompt or an answer, the end of the permissions created before the hook started; the quote of an answer and the latest answer's hook start; the turn record of a prompt whose hook started after it; a changed description and its record in `session.json`; then terminal prompts posted by `User` unless blank, made only of this plugin's channel tags, pushed or a command, the answer, and, also after a failed post, the short description |
| `Session.endPermissions`, `fileDone` | [203-214](../src/hub.ts#L203-L214) | the end of pending permissions created before a time; the end of a prompt's file |
| `Session.invite`, `applyMembers`, `deleteGroup` | [216-252](../src/hub.ts#L216-L252) | invitation through the Main group and its record; the Main group's membership applied to the invited members, as in [API 4.2](api.md#42-main-group); the standing approvals of the session's chats revoked, the group deleted, the download checks stopped |
| `Session.deliver`, `receive`, `turnQuote`, `answer`, `post`, `pushedIn` | [254-309](../src/hub.ts#L254-L309) | after the held prompts' wait, the file received while attached; then the prompt line with the file's path, or without it and a reply after a failure, or `Session N is not running.` without an attachment; the file received into the session's folder within 10 minutes; the newest turn record before an answer, else the oldest pushed prompt not quoted, marked quoted; the answer in parts with its quote; a post to the group, sent again without its quote when the core refuses it; the pushed prompts a turn's prompt contains |
| `Session.approves`, `setMode`, `declineHost`, `store`, `savePending` | [311-336](../src/hub.ts#L311-L336) | approver check of decision 17; the mode and its record in the group's custom data; `hostDeclined` and its record; fields written into `session.json`; a pending connection recorded, those older than a day dropped |
| `Hub.open` | [363](../src/hub.ts#L363) | core `_hub`, Main group found or created, `MainGroup` load and start, `User` unless it fails, the stored sessions, events |
| `Hub.configure`, `resetOwners` | [391-401](../src/hub.ts#L391-L401) | expiry of all sessions, Main group name; reset at once without owners, else a card |
| `Hub.session` | [403](../src/hub.ts#L403) | the open session renamed, or a new `Session` with its stored or new group; the owners invited to a new group; the Main group's membership applied to a stored group |
| `Hub.sessionsText`, `join`, `inviteOwner`, `applyMembers` | [427-453](../src/hub.ts#L427-L453) | `SessionGroups` of the Main group |
| `Hub.deleteGroup` | [455](../src/hub.ts#L455) | the group of session N deleted, the session removed from the hub |
| `Hub.stop` | [466](../src/hub.ts#L466) | actions stopped, events awaited, cancelled cards awaited up to 10 s, cores closed |
| `storedGroup`, `createSessionGroup` | [478-497](../src/hub.ts#L478-L497) | the group of `session.json` with a matching session number; a new group with `User` added |
| `Hub.onEvent` | [499](../src/hub.ts#L499) | event dispatch; pending connections claimed at `contactConnected` and `userJoinedGroup` |
| `textParts` | [535](../src/hub.ts#L535) | pieces of at most 12000 encoded bytes |
| `isRefusedQuote` | [552](../src/hub.ts#L552) | `errorStore` `invalidQuote` or `chatItemNotFound` |

#### [`main-group.ts`](../src/main-group.ts)

| Symbol | Line | Contents |
|---|---|---|
| `mainCommands`, texts | [8-16](../src/main-group.ts#L8-L16) | command menu; greeting, help reply, reset card, log line |
| `SessionGroups` | [18](../src/main-group.ts#L18) | what the Main group asks of the hub |
| `MainGroup.owners`, `isOwner`, `member` | [53-63](../src/main-group.ts#L53-L63) | human members with role `owner` while the group is not lost; a member by ID as last read, of any status |
| `MainGroup.load`, `start` | [65-82](../src/main-group.ts#L65-L82) | lost check, name and menu, members; without owners, the existing link, or a new one |
| `MainGroup.onMessage`, `command` | [89-130](../src/main-group.ts#L89-L130) | `/sessions`, `/join N`, `/approve reset`, `/reject reset`, help |
| `MainGroup.onEvent`, `handleEvent`, `ownerJoined` | [101-166](../src/main-group.ts#L101-L166) | the members read again and applied to the session groups; the first human who joins becomes an owner; a new link when no owner remains; new owners invited to the open sessions |
| `MainGroup.askReset`, `replace`, `decideReset` | [105-142](../src/main-group.ts#L105-L142) | reset card; a new Main group, its members applied to the session groups, so the invited members are removed, a new link |
| `openLink`, `deleteLink` | [173-186](../src/main-group.ts#L173-L186) | group link with role `member` |
| `findMainGroup`, `createMainGroup` | [212-230](../src/main-group.ts#L212-L230) | the newest group with `simplexMcpMain`; a new one with history, direct messages for owners and the menu |

#### [`user.ts`](../src/user.ts)

| Symbol | Line | Contents |
|---|---|---|
| `User.open` | [17](../src/user.ts#L17) | core `_user` with profile `User`, events, a connection to `Claude` when the hub has no contact marked `simplexMcpUser` |
| `User.post` | [30](../src/user.ts#L30) | messages to the newest group marked with the session number where `User` is a current member; a group of the session that Claude deleted has the status `deleted` and is skipped |
| `User.connect` | [39](../src/user.ts#L39) | a one-time invitation of the hub, joined by `User`; the `contactConnected` of that invitation's connection awaited 60 s; the hub's contact marked |
| `User.onEvent` | [51](../src/user.ts#L51) | the contact marked `simplexMcpClaude`; invitations of that contact to a group named `N · ...` joined and marked |
| `userContact` | [61](../src/user.ts#L61) | the hub's contact marked `simplexMcpUser` |

#### [`core.ts`](../src/core.ts)

| Symbol | Line | Contents |
|---|---|---|
| `openCore` | [20](../src/core.ts#L20) | folder, `ChatApi.init` with prefix `FOLDER/simplex`, active user or a new one with peer type `agent`, `prepareCore`, event buffer, `startChat`, preset contact deleted for a new user, profile update |
| `deletePresetContacts` | [45](../src/core.ts#L45) | the contact `Ask SimpleX Team`, deleted when it has no connection |

#### [`hook.ts`](../src/hook.ts), [`owner.ts`](../src/owner.ts), [`qr.ts`](../src/qr.ts)

| Symbol | Line | Contents |
|---|---|---|
| `sessionStart` | [hook.ts:41](../src/hook.ts#L41) | `{}` for a run that is not interactive; Node.js check, options, `ensureDaemon`, `POST /title` with the title, the folder and the model, the link message |
| `turn` | [hook.ts:59](../src/hook.ts#L59) | `{}` for a run that is not interactive; `ensureDaemon`, then `POST /turn` with `CLAUDE_PID`, the hook process's start time `performance.timeOrigin`, the prompt and the last answer, each cut to 250,000 characters, the new model and the effort level, repeated after status 409 up to 15 times 1 s apart; output `{}` |
| `linkCommand`, `resetCommand` | [owner.ts:7-8](../src/owner.ts#L7-L8) | `npx -y @simplex-chat/mcp@VERSION link` and `reset-owners`, VERSION of this package |
| `linkText`, `resetOwnersText` | [owner.ts:10-20](../src/owner.ts#L10-L20) | the Main group's name with the owners, or with the link and a QR code on a terminal only |
| `showLink`, `resetOwners` | [owner.ts:22-35](../src/owner.ts#L22-L35) | `link`, with the link from `GET /link` while the Main group has no owner; `reset-owners`, from a terminal only without owners |
| `qrCodeText` | [qr.ts:14](../src/qr.ts#L14) | `small` and `large` with ANSI colors for terminals, `plain` for the hook message |

#### [`config.ts`](../src/config.ts), [`files.ts`](../src/files.ts)

| Symbol | Line | Contents |
|---|---|---|
| `interactive` | [config.ts:26](../src/config.ts#L26) | `false` for `CLAUDE_CODE_SESSION_ATTENDED` `0` and for `CLAUDE_CODE_ENTRYPOINT` `sdk-cli`, `sdk-ts` or `sdk-py` |
| `readConfig` | [config.ts:30](../src/config.ts#L30) | `CLAUDE_PLUGIN_OPTION_*` to `Config`, defaults for blank values, `ConfigError` texts |
| `dataRoot`, `hubFolder`, `sessionFolder` | [files.ts:33-49](../src/files.ts#L33-L49) | ROOT, `ROOT/_hub`, `ROOT/_sessions/<lower-case ID>` |
| `SessionFile`, `readSessionFile`, `writeSessionFile`, `storedSessions` | [files.ts:12-74](../src/files.ts#L12-L74) | `session.json`: number, name, project, creation time, group, aliases, model, effort, a rejected mode `host`, closing time; an unparsable file is skipped with a log line; a write goes to `session.json.tmp`, renamed into place |
| `socketPath` | [files.ts:76](../src/files.ts#L76) | `ROOT/daemon.sock`; over 103 bytes `TMP/simplex-mcp-HASH.sock`; Windows `\\.\pipe\simplex-mcp-HASH` |
| `logFile`, `trimLog`, `openLog` | [files.ts:83-97](../src/files.ts#L83-L97) | `ROOT/daemon.log`, emptied when over 10 MB, mode 0600 |

#### Plugin files

| File | Contents |
|---|---|
| [`.claude-plugin/plugin.json`](../.claude-plugin/plugin.json) | plugin `simplex`, version, description, `userConfig` |
| [`.mcp.json`](../.mcp.json) | server `simplex`: `node dist/cli.js session` in the plugin's folder, tool call timeout, options as environment variables |
| [`hooks/hooks.json`](../hooks/hooks.json) | SessionStart: `dist/cli.js hook session-start`; PreToolUse: `allow.js`; UserPromptSubmit, Stop, PostModelSwitch: `dist/cli.js hook turn`, in the background |
| [`npm-shrinkwrap.json`](../npm-shrinkwrap.json) | the dependency tree that Claude Code installs into the plugin's folder; packed with the production dependencies only, by [`scripts/strip-dev.js`](../scripts/strip-dev.js) in `prepack` and `postpack` |
| [`plugin/allow.js`](../plugin/allow.js) | PreToolUse allow decision |
| [`skills/link/SKILL.md`](../skills/link/SKILL.md), [`skills/reset-owners/SKILL.md`](../skills/reset-owners/SKILL.md) | `/simplex:link`, `/simplex:reset-owners` |
| [`/.claude-plugin/marketplace.json`](../../../.claude-plugin/marketplace.json) | marketplace `simplex-chat`, plugin source npm `@simplex-chat/mcp` |

### 8.3 State

Memory of the daemon:

| Item | Holder | Written by | Read by |
|---|---|---|---|
| Entries of open sessions | `Daemon.entries` ([daemon.ts:72](../src/daemon.ts#L72)) | `entry`, idle close, stop | routes, `status` |
| Line writers of the attachments | `Entry.writers` ([daemon.ts:65](../src/daemon.ts#L65)) | `onAttach` | `Session.onLine` |
| Stored sessions, next number | `Daemon.stored`, `nextNumber` ([daemon.ts:74-81](../src/daemon.ts#L74-L81)) | `Daemon` constructor, `openAgent` | `openHub`, `openAgent` |
| Aliases of sessions | `Daemon.aliases`, ID to session ID ([daemon.ts:75](../src/daemon.ts#L75)) | `Daemon` constructor, `alias` | `canonical`, `resolve` |
| Titles by process | `Daemon.titles`, PID to session ID, time and the name of a held title, and the `titled` signal ([daemon.ts:76-77](../src/daemon.ts#L76-L77)) | `onTitle`, `applyTitle` | `processSession`, the wait of `resolve`, `onSession` |
| Registrations by process | `Daemon.registered`, the signal of a `/session` with that PID ([daemon.ts:78](../src/daemon.ts#L78)) | `onSession` | the wait of a held title |
| Held titles | `Daemon.held`, PID to its last held `/title` request ([daemon.ts:79](../src/daemon.ts#L79)) | `onTitle`, the end of its wait | `onTitle`, `shutdown` |
| Running deletions of groups | `Daemon.deleting`, session ID to the deletion of the group of a session closed for 7 days ([daemon.ts:80](../src/daemon.ts#L80)) | `deleteClosedGroups` | `deleteClosedGroups`, `openAgent` |
| Process of a session server | `Entry.pid`, `Entry.seen` ([daemon.ts:67-68](../src/daemon.ts#L67-L68)) | `onSession`, the end of the last attachment | `processSession` |
| Sessions | `Hub.sessions`, by number ([hub.ts:348](../src/hub.ts#L348)) | `Hub.session` | events, Main group commands |
| End of the held prompts | `Hub.promptsHeldUntil`, 30 s after the stored sessions opened ([hub.ts:347](../src/hub.ts#L347), [:373](../src/hub.ts#L373)) | `Hub.open` | `Session.deliver` |
| Prompts in delivery | `Session.delivery`, a promise chain in arrival order ([hub.ts:66](../src/hub.ts#L66)) | `onMessage` | `deliver` |
| Actions of a session | `Session.actions`, numbered from 1 per session group ([hub.ts:52](../src/hub.ts#L52), [:81](../src/hub.ts#L81)) | tools, `permission`, `requestMode` | `Control`, `busy` |
| Pushed prompts | `Session.pushed`, at most 20, each until a turn's prompt matches it; `quoted` once an answer without a turn record quoted it ([hub.ts:57](../src/hub.ts#L57)) | `deliver`, `turnQuote` | `turn`, `turnQuote` |
| Files of prompts in transfer | `Session.receiving` ([hub.ts:58](../src/hub.ts#L58)) | `receive`, its 10-minute timer | `fileDone` |
| Model and effort | `Session.description`, also in `session.json` ([hub.ts:59](../src/hub.ts#L59)) | constructor, `turn` | `turn` |
| Turn records | `Session.turns`: the start time of each prompt hook that started after the latest answer's hook, with its pushed prompt, `null` for a terminal prompt or a command ([hub.ts:60-61](../src/hub.ts#L60-L61)) | `turn`; an answer removes those that started before its hook | `turnQuote` |
| A rejected mode `host` | `Session.hostDeclined`, also in `session.json` ([hub.ts:63](../src/hub.ts#L63)) | constructor, `declineHost` | `askMode`, `requestMode` |
| The requested mode | `Session.requested` ([hub.ts:62](../src/hub.ts#L62)) | `requestMode`, a rejected or expired `mode` card | `askMode` |
| Members of the Main group, its link, the reset card | `MainGroup` ([main-group.ts:27-31](../src/main-group.ts#L27-L31)) | `refreshMembers`, `openLink`, `askReset` | owner checks, `Session.applyMembers`, `/status`, `/title`, `/link` |

Memory of a session process: one connection promise, one attachment, and the retry delay with its timer ([session.ts:68-75](../src/session.ts#L68-L75)).

Chat database custom data, all in the core `_hub` unless noted:

| Item | Entity | Value | Written by | Read by |
|---|---|---|---|---|
| Main group marker | Main group | `{simplexMcpMain: true}` | [`createMainGroup`](../src/main-group.ts#L221-L230) | [`findMainGroup`](../src/main-group.ts#L212-L214) |
| Session group | group of session N | `{simplexMcpSession: {session: N, invited: [{groupMemberId, mainMemberId}], pending?: [{connId, at}], mode?}}`, `mode` the approved mode | [`createSessionGroup`](../src/hub.ts#L485-L497), [`Session.invite`](../src/hub.ts#L216-L220), [`applyMembers`](../src/hub.ts#L222-L246), [`setMode`](../src/hub.ts#L316-L319), [`savePending`](../src/hub.ts#L332-L336), [`claimPending`](../src/hub.ts#L164-L169) | [`storedGroup`](../src/hub.ts#L478-L483), [`Session` constructor](../src/hub.ts#L81), [`Session.approves`](../src/hub.ts#L311-L314), [`applyMembers`](../src/hub.ts#L222-L246), [`claimPending`](../src/hub.ts#L164-L169) |
| Action counter | group of session N | `{simplexMcp: {control: true, nextAction}}`, merged with the other keys | [`saveCounter`, `saveData`](../src/control.ts#L160-L172) | [`Session` constructor](../src/hub.ts#L81) |
| Chat owner | contact or group of a peer | `{...other keys, simplexMcpOwner: {session: ID}}` | [`claim`](../src/tools.ts#L587-L591), [`giveChat`](../src/tools.ts#L574-L577) | [`owns`](../src/tools.ts#L583-L585) |
| Standing approvals | contact or group of a peer | as in [3.2](#32-chat-database-custom-data) | `Control` of the owning session's group | the same |
| User contact | the hub's contact of `User` | `{simplexMcpUser: true}` | [`User.connect`](../src/user.ts#L39-L49) | [`userContact`](../src/user.ts#L61-L63) |
| Claude contact | in `_user`: the contact of `Claude` | `{simplexMcpClaude: true}` | [`User.onEvent`](../src/user.ts#L52) | [`User.onEvent`](../src/user.ts#L53) |
| Session of a group | in `_user`: a session group | `{simplexMcpSession: N}` | [`User.onEvent`](../src/user.ts#L57) | [`User.post`](../src/user.ts#L30) |

Files:

| Path | Contents | Written | Read |
|---|---|---|---|
| `ROOT/` | data root, mode 0700 | every client and the daemon ([client.ts:23](../src/client.ts#L23), [daemon.ts:458](../src/daemon.ts#L458)) | |
| `ROOT/token` | the bearer token, 0600 | the first client or daemon ([files.ts:120-130](../src/files.ts#L120-L130)) | every client, the daemon |
| `ROOT/lock` | PID of the daemon, linked from `ROOT/lock.PID.HEX.tmp`; a stopped daemon's lock is claimed through `ROOT/lock.PID.claim`, removed after the takeover | `Daemon.start` ([daemon.ts:99](../src/daemon.ts#L99), [files.ts:178-217](../src/files.ts#L178-L217)) | the next daemon |
| `ROOT/titles.json` | the `/title` requests held at a stop, 0600 | `shutdown` ([daemon.ts:211](../src/daemon.ts#L211)) | the next `Daemon.start`, which removes it ([daemon.ts:112](../src/daemon.ts#L112)) |
| `ROOT/daemon.sock` | the socket | `listen` ([socket.ts:40-43](../src/socket.ts#L40-L43)) | clients |
| `ROOT/daemon.log` | the daemon's stdout and stderr, emptied over 10 MB at a spawn and every hour | `spawnDaemon` ([client.ts:71-79](../src/client.ts#L71-L79)), [`trimLog`](../src/files.ts#L87-L90) | people |
| `ROOT/_hub/`, `ROOT/_user/` | databases and `tmp/` of the profiles `Claude` and `User` | [`openCore`](../src/core.ts#L20-L36) | the cores |
| `ROOT/_sessions/ID/session.json` | `{number, name, project, created, group?, aliases?, model?, effort?, hostDeclined?, closed?}`, `closed` the time the session closed, 0600, replaced whole through `session.json.tmp` | `openAgent`, `rename`, `alias`, `markClosed`, `deleteGroupOf`, `Hub.session`, `declineHost`, `store` ([daemon.ts:409-424](../src/daemon.ts#L409-L424), [:313-318](../src/daemon.ts#L313-L318), [:306-311](../src/daemon.ts#L306-L311), [:139-160](../src/daemon.ts#L139-L160), [hub.ts:413](../src/hub.ts#L413), [:321-330](../src/hub.ts#L321-L330)) | `Daemon` constructor, `resolve`, `deleteClosedGroups`, `Hub.session`, `alias`, `store` |
| `ROOT/_sessions/ID/files/` | downloads of the session's chats as in [3.3](#33-files); `group/` with the files of the owners' prompts; `sent/`, deleted when the session opens | the core ([hub.ts:270-278](../src/hub.ts#L270-L278), [:410](../src/hub.ts#L410)) | tools, Claude |

### 8.4 Process lifecycle

#### Session start

1. The SessionStart hook checks Node.js 22, reads the options, starts the daemon when it is not running, and sends `POST /title` with the session ID, `CLAUDE_PID`, title, project folder, model and options ([hook.ts:41-57](../src/hook.ts#L41-L57)).
2. `/title` resolves the ID to a session as in [API 4.6](api.md#46-socket-protocol), so that `/clear`, `/resume` and `--continue` keep the session of the running Claude Code process, with an alias only for an ID without `session.json`; a new ID from a process without a known session waits, after the reply, for the server of that process to register, up to 10 s; then `/title` opens the session: from `session.json`, or with a new number and the name from the title or the folder; `Hub.session` finds the group of `session.json` or creates one, adds `User`, invites the Main group's owners, and loads its `Control` ([daemon.ts:248-278](../src/daemon.ts#L248-L278), [:409-424](../src/daemon.ts#L409-L424), [hub.ts:403-421](../src/hub.ts#L403-L421)).
3. A new title renames the group and `session.json`; the model is written to the group's short description ([daemon.ts:276-277](../src/daemon.ts#L276-L277)).
4. The hook prints the link message while the Main group has no owner, else `{}`.
5. The session process connects in the background: `/session` resolves its ID, waiting up to 10 s for the title of its process when the ID is new, registers the options and its parent process, and requests the mode; `/attach` sets the session's line writer ([daemon.ts:233-246](../src/daemon.ts#L233-L246), [:320-340](../src/daemon.ts#L320-L340)). When `/attach` ends or the connection fails, except with a `ProtocolError`, the session process connects again ([session.ts:85-109](../src/session.ts#L85-L109)).

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
  protocol other than 1: ProtocolError "Restart Claude Code to update the SimpleX plugin."
```

#### Daemon start

1. `ROOT` is created, the options are read, and the lock is taken. A lock held by another live process is checked for up to 15 s: when it is released, the start runs again; when the socket answers, the ready line is `running`; otherwise `error: Another daemon uses ROOT (PID N). If no daemon runs, remove ROOT/lock.` and exit code 1 ([daemon.ts:453-465](../src/daemon.ts#L453-L465), [:501-508](../src/daemon.ts#L501-L508), [:98-101](../src/daemon.ts#L98-L101)).
2. The stored sessions are read, the next number set, and a stored session without a closing time gets the current time ([daemon.ts:89-96](../src/daemon.ts#L89-L96)).
3. The socket listens before the hub opens; requests wait for the hub ([daemon.ts:103-104](../src/daemon.ts#L103-L104), [:194-199](../src/daemon.ts#L194-L199)).
4. `Hub.open`: core `_hub` with profile `Claude`; the Main group found by its marker or created; `MainGroup.load` and `start`, with a link while it has no owner; core `_user` and its connection to `Claude` at the first start; a `Session` for each stored session with a group, the Main group's membership applied to its group; prompts held for 30 s; events ([hub.ts:363-380](../src/hub.ts#L363-L380), [:419](../src/hub.ts#L419), [daemon.ts:134-137](../src/daemon.ts#L134-L137)).
5. A failed hub open closes the listener and releases the lock; the requests that waited get status 500 ([daemon.ts:105-126](../src/daemon.ts#L105-L126)).
6. The stop timer and the hourly log trim start; the groups of sessions closed for 7 days are deleted then and every hour ([flow 30](flows.md#30-the-group-of-a-session-closed-for-7-days-and-a-run-that-is-not-interactive)); the titles held at the last stop are read from `ROOT/titles.json` and taken as new `/title` requests; the log line `SimpleX MCP daemon VERSION, data folder ROOT`; the ready line `ready` ([daemon.ts:111-121](../src/daemon.ts#L111-L121), [:467-468](../src/daemon.ts#L467-L468)).

#### Event dispatch

`Hub.onEvent` ([hub.ts:499-529](../src/hub.ts#L499-L529)), after a stop: nothing; otherwise `expireDue` of every session first:

| Event | Condition | Handler |
|---|---|---|
| `newChatItems` | item of the Main group | [`MainGroup.onMessage`](../src/main-group.ts#L89) |
| `newChatItems` | item of a session group | [`Session.onMessage`](../src/hub.ts#L156) |
| `rcvFileComplete`, `rcvFileError` | | [`Session.fileDone`](../src/hub.ts#L209) of every session |
| `contactConnected`, `userJoinedGroup` | a connection ID | [`Session.claimPending`](../src/hub.ts#L164) of every session |
| any event | | [`AgentTools.onEvent`](../src/tools.ts#L671) of every session |
| `sndFileError` | a chat of the session, or its group | [`Control.fileNotSent`](../src/control.ts#L193) |
| membership events | of the Main group | [`MainGroup.onEvent`](../src/main-group.ts#L101), queued in the Main group, not awaited by the event loop; the members applied to the session groups ([flow 28](flows.md#28-a-member-leaves-is-removed-or-demoted-in-the-main-group)) |
| membership events | of a session group | [`Control.onEvent`](../src/control.ts#L217), then [`Session.askMode`](../src/hub.ts#L143), queued |

#### Attach, idle close and idle stop

- An attachment cancels the session's idle timer; its end, when no other attachment remains, records the time, clears the line writer, ends the pending permissions and starts the timer ([daemon.ts:320-340](../src/daemon.ts#L320-L340)).
- The idle timer, 10 minutes, closes the entry when the session has no running call and no action in memory, and writes the closing time into `session.json`; otherwise it starts again ([daemon.ts:426-437](../src/daemon.ts#L426-L437)). A closed session keeps its `Session` in the hub, so its group answers `/list` and `/revoke` ([hub.ts:403-408](../src/hub.ts#L403-L408)).
- When no entry remains, the stop timer, 10 minutes, stops the daemon ([daemon.ts:439-445](../src/daemon.ts#L439-L445)).

#### Stop

On `SIGINT`, `SIGTERM`, `POST /stop` or the stop timer ([daemon.ts:208-231](../src/daemon.ts#L208-L231)):

1. Timers are cleared, the held titles are saved in `ROOT/titles.json`, the listener closes, and open streams are destroyed.
2. `Hub.stop`: the actions of every session are stopped, cards edited to `Cancelled: the server stopped.`; queued group events are awaited; cancelled cards get 10 s to be sent; the download checks stop; the cores `_user` and `_hub` close ([hub.ts:466-476](../src/hub.ts#L466-L476)).
3. `Cancelled K action(s).`, the lock is released, `Stopped.`; the process exits after the `/stop` reply.

### 8.5 Approval model

| Rule | Code |
|---|---|
| An approver in a session group is a member recorded in `invited` whose member in the Main group is a current owner; prompts, approvals and tool-use decisions count only from approvers | [hub.ts:311-314](../src/hub.ts#L311-L314), [control.ts:552-554](../src/control.ts#L552-L554), [main-group.ts:53-63](../src/main-group.ts#L53-L63) |
| A new action of a session is refused while its group has no approver | [control.ts:138-139](../src/control.ts#L138-L139) |
| A member who leaves the Main group or is removed from it leaves every session group; a member whose role there is lowered gets that role in every session group, also in a pending invitation | [hub.ts:222-246](../src/hub.ts#L222-L246), [main-group.ts:149](../src/main-group.ts#L149), [hub.ts:419](../src/hub.ts#L419) |
| A file of `reply` is sent at once only while every member of the group is Claude, `User`, gone or an approver; otherwise after an owner approves its card, in mode `requests`; the card shows the file's name and size only, and the caption and the preview are sent with the file | [tools.ts:719-730](../src/tools.ts#L719-L730), [control.ts:149-153](../src/control.ts#L149-L153), [:505-511](../src/control.ts#L505-L511), [:675-678](../src/control.ts#L675-L678) |
| The first human who joins the Main group while it has no owner becomes an owner, and the link is deleted | [main-group.ts:150](../src/main-group.ts#L150), [:160-166](../src/main-group.ts#L160-L166) |
| A session's mode `host` takes effect after an owner approves its `mode` card, posted once the group has an approver; after a rejection, the card is posted again only after `requests`; `requests` at once, ending a pending `mode` card; the mode is kept in the session group's custom data | [hub.ts:135-154](../src/hub.ts#L135-L154), [:321-324](../src/hub.ts#L321-L324), [:526](../src/hub.ts#L526), [tools.ts:569-570](../src/tools.ts#L569-L570) |
| While owners exist, a reset of the owners takes effect after an owner approves its card in the Main group | [hub.ts:397-401](../src/hub.ts#L397-L401), [main-group.ts:132-142](../src/main-group.ts#L132-L142) |
| A chat without an owner gets the session that connects it: at the perform, at the connection event of its action, at the connection of a pending connection the session recorded, or at a `connect` to it as a known chat; tools of other sessions don't show it; `/give CHAT N` moves it | [tools.ts:583-591](../src/tools.ts#L583-L591), [:760-765](../src/tools.ts#L760-L765), [:836](../src/tools.ts#L836), [:845](../src/tools.ts#L845), [:903](../src/tools.ts#L903), [hub.ts:164-169](../src/hub.ts#L164-L169), [:512-513](../src/hub.ts#L512-L513), [control.ts:359-371](../src/control.ts#L359-L371) |

Tool-use state machine, one action of tool `permission` per request:

| Event | From | To | Code |
|---|---|---|---|
| `permission_request` | | `pending`, card posted | [hub.ts:171-172](../src/hub.ts#L171-L172) |
| `/approve N` | `pending` | `performing`, `allow` pushed, then `done` | [hub.ts:174-175](../src/hub.ts#L174-L175), [tools.ts:556-572](../src/tools.ts#L556-L572) |
| `/reject N` | `pending` | `rejected`, `deny` pushed | [hub.ts:174-175](../src/hub.ts#L174-L175) |
| a prompt or an answer of a hook that started after the card's creation, end of the last attachment | `pending` | `expired`, card edited to `Ended.` | [hub.ts:183](../src/hub.ts#L183), [:203-207](../src/hub.ts#L203-L207), [daemon.ts:337](../src/daemon.ts#L337), [actions.ts:244-248](../src/actions.ts#L244-L248) |
| expiry | `pending` | `expired`, card edited to `Expired.` | [actions.ts:309-318](../src/actions.ts#L309-L318) |
| `done`, `rejected` or `expired` | | removed from the session's actions | [hub.ts:176-178](../src/hub.ts#L176-L178) |

```mermaid
stateDiagram-v2
  [*] --> pending: permission_request
  pending --> performing: /approve N, allow pushed
  performing --> done
  pending --> rejected: /reject N, deny pushed
  pending --> expired: later prompt or answer, detach: Ended.; expiry: Expired.
  done --> [*]: removed
  rejected --> [*]: removed
  expired --> [*]: removed
```

### 8.6 Channel and mirroring

| Direction | Message | Code |
|---|---|---|
| daemon to session | `{prompt: {text, from, item, file?}}`, `{permission: {requestId, behavior}}` on `/attach` | [protocol.ts:55-58](../src/protocol.ts#L55-L58) |
| session to Claude Code | `notifications/claude/channel` `{content, meta: {from, item, file?}}`; `notifications/claude/channel/permission` `{request_id, behavior}` | [session.ts:192-196](../src/session.ts#L192-L196) |
| Claude Code to session | `notifications/claude/channel/permission_request` `{request_id, tool_name, description, input_preview}` | [session.ts:58-61](../src/session.ts#L58-L61) |
| session to daemon | `POST /permission` | [session.ts:111-114](../src/session.ts#L111-L114) |
| hooks to daemon | `POST /turn` `{id, pid?, at?, prompt?, answer?, model?, effort?}`, `at` the hook's `performance.timeOrigin`, `prompt` and `answer` cut to 250,000 characters | [hook.ts:59-82](../src/hook.ts#L59-L82) |

```
on owner's message m in group N:   one-line "/" with a keyword of ownerKeywords: a command, not a prompt
                                   else: delivery = delivery then deliver(m), in arrival order
deliver(m):                        no attachment: wait for one until 30 s after the hub opened
                                   attached: receive its file within 10 minutes, or reply that it was not received
                                   no attachment now: reply "Session N is not running."
                                   else: pushed += (m.text, m.item, now), at most 20; push {prompt}
on /turn at time at, before any await:
  prompt p or answer a:            end pending permissions created before at
  answer a, also empty:            earlier = turns with at below at; quoted = the item of the newest earlier,
                                   or the oldest pushed not quoted, marked quoted, when earlier is empty; earlier leave turns
                                   answeredAt = max(answeredAt, at)
  prompt p:                        matched = pushed entries in this plugin's channel tags of p, or equal to p and pushed in the last 60 s
                                   at not below answeredAt: turns += (at, the oldest matched or null)
                                   matched leave pushed
  model or effort:                 a changed value: description, session.json
on /turn prompt p:                 nothing matched, p not blank, not only this plugin's channel tags, not a one-line "/" command:
                                   User posts parts(p) in the newest group of N where it is a current member
on /turn answer a, not blank:      Claude posts parts(a), the first quoting quoted when it fits
then, also after a failed post, a changed value: short description "MODEL · effort LEVEL"
post with a quote:                 the core refuses the quote with invalidQuote or chatItemNotFound: the post again without it
```

```mermaid
sequenceDiagram
  participant S as Stop hook of turn N
  participant U as UserPromptSubmit hook of turn N+1
  participant D as daemon
  Note over S,U: S starts before U; at = performance.timeOrigin of each
  U->>D: /turn {at: 3, prompt}: turns += (3, prompt N+1)
  S->>D: /turn {at: 2, answer}: quotes the newest turn record before 2, prompt N
  Note over D: a later answer with at 4 quotes prompt N+1
```

Sources: [hub.ts:156-207](../src/hub.ts#L156-L207), [:254-309](../src/hub.ts#L254-L309), [:535-555](../src/hub.ts#L535-L555).

### 8.7 Security boundaries

| Rule | Code |
|---|---|
| The socket is in ROOT, mode 0700, or a named pipe on Windows; every request needs the token, compared with `timingSafeEqual` | [socket.ts:19-25](../src/socket.ts#L19-L25), [files.ts:76-81](../src/files.ts#L76-L81) |
| The Main group link is shown only in the hook's `systemMessage` and by `link` on a terminal; tool results, refusals and other output name the command instead; of the socket's routes, only `/title` and `/link` return it | [hook.ts:51-53](../src/hook.ts#L51-L53), [owner.ts:7-15](../src/owner.ts#L7-L15), [control.ts:37](../src/control.ts#L37), [daemon.ts:185](../src/daemon.ts#L185), [:245](../src/daemon.ts#L245), [:449](../src/daemon.ts#L449) |
| `reset-owners` without owners runs only on a terminal | [owner.ts:31](../src/owner.ts#L31) |
| The plugin runs its own `dist/cli.js`, and Node.js loads its modules from the plugin's folder, so the `node_modules` of a project can't replace them | [.mcp.json:5](../.mcp.json#L5), [hooks.json:3-7](../hooks/hooks.json#L3-L7) |
| The daemon gets only the environment variables of `daemonVariables` | [client.ts:13](../src/client.ts#L13), [:78](../src/client.ts#L78) |
| `send` and `reply` refuse the files of ROOT except `files/` of the session's own folder | [tools.ts:1008-1013](../src/tools.ts#L1008-L1013), [hub.ts:96-97](../src/hub.ts#L96-L97) |
| `User`'s messages are not prompts: `User` is not an invited owner; `User` joins only groups its `Claude` contact invites it to | [hub.ts:160](../src/hub.ts#L160), [user.ts:51-58](../src/user.ts#L51-L58) |
| The hub's own groups and the `User` contact have no owner key, so the tools of every session leave them out | [tools.ts:583-585](../src/tools.ts#L583-L585), [:1296-1304](../src/tools.ts#L1296-L1304) |
| The approved mode is kept in the chat database, not in a file of ROOT | [hub.ts:81](../src/hub.ts#L81), [:316-319](../src/hub.ts#L316-L319), [files.ts:12-23](../src/files.ts#L12-L23) |
| These rules hold against Claude's tools, not against a shell running as the same user: such a shell reads ROOT, with the token and the chat databases, and sends requests to the daemon | |

### 8.8 Limits

| Limit | Value | Code |
|---|---|---|
| socket request body | 1 MiB | [socket.ts:6](../src/socket.ts#L6) |
| idle close of a session, idle stop of the daemon | 10 min each | [daemon.ts:17-18](../src/daemon.ts#L17-L18) |
| daemon start wait | 5 min | [client.ts:10](../src/client.ts#L10) |
| `User` connection at the first start | 60 s | [user.ts:12](../src/user.ts#L12) |
| echo of a raw prompt | 60 s | [hub.ts:22](../src/hub.ts#L22) |
| pushed prompts of a session | 20 | [hub.ts:24](../src/hub.ts#L24) |
| file of a prompt | 10 min | [hub.ts:25](../src/hub.ts#L25) |
| prompts held for an attachment after the hub opens | 30 s | [hub.ts:27](../src/hub.ts#L27) |
| record of a pending connection | 1 day | [hub.ts:26](../src/hub.ts#L26) |
| wait of a session server for the title of its process; a detached session server's process kept | 10 s | [daemon.ts:19](../src/daemon.ts#L19) |
| attempts of a turn the daemon refuses with status 409 | 15, 1 s apart | [hook.ts:12-14](../src/hook.ts#L12-L14) |
| group of a closed session | deleted after 7 days, checked at the start and every hour | [daemon.ts:23](../src/daemon.ts#L23), [:113](../src/daemon.ts#L113), [:120](../src/daemon.ts#L120) |
| attachment retries of a session server | 1 s, doubled up to 30 s | [session.ts:19-20](../src/session.ts#L19-L20) |
| part of an answer or a terminal prompt | 12000 bytes JSON-escaped | [hub.ts:23](../src/hub.ts#L23), [:535-550](../src/hub.ts#L535-L550) |
| cancelled cards wait at stop | 10 s | [hub.ts:21](../src/hub.ts#L21) |
| `daemon.log` | emptied over 10 MB, at a spawn and every hour | [files.ts:10](../src/files.ts#L10), [:87-90](../src/files.ts#L87-L90), [daemon.ts:20](../src/daemon.ts#L20) |
| socket path | 103 bytes | [files.ts:9](../src/files.ts#L9) |
| actions per session | 20 pending, 20 performing | [actions.ts:9-10](../src/actions.ts#L9-L10), [:206-210](../src/actions.ts#L206-L210) |
