# SimpleX MCP server: architecture

> System structure, modules, state, process lifecycle, approval model and security boundaries of `@simplex-chat/mcp` 0.3.0: `serve` in sections 1-7, the Claude Code plugin in section 8.
>
> Related specs: [README](README.md) | [Flows](flows.md) | [API](api.md)
> Related docs: [README](../README.md) | [Reference](../REFERENCE.md) | [PLAN](../PLAN.md)

**Source:** [`cli.ts`](../src/cli.ts) | [`serve.ts`](../src/serve.ts) | [`http.ts`](../src/http.ts) | [`tools.ts`](../src/tools.ts) | [`actions.ts`](../src/actions.ts) | [`control.ts`](../src/control.ts) | [`messages.ts`](../src/messages.ts) | [`files.ts`](../src/files.ts) | [`register.ts`](../src/register.ts) | [`log.ts`](../src/log.ts) | [`core.ts`](../src/core.ts)
**Plugin source:** [`session.ts`](../src/session.ts) | [`client.ts`](../src/client.ts) | [`socket.ts`](../src/socket.ts) | [`protocol.ts`](../src/protocol.ts) | [`service.ts`](../src/service.ts) | [`hub.ts`](../src/hub.ts) | [`main-group.ts`](../src/main-group.ts) | [`user.ts`](../src/user.ts) | [`hook.ts`](../src/hook.ts) | [`owner.ts`](../src/owner.ts) | [`config.ts`](../src/config.ts) | [`qr.ts`](../src/qr.ts)

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
4. The handler result is returned by `AgentTools.call` as `structuredContent` and as JSON text ([`tools.ts:544-551`](../src/tools.ts#L544-L551)).
5. The server calls `ChatApi` methods of the core ([`messages.ts:271-287`](../src/messages.ts#L271-L287)); actions are created in `Actions` and shown to the owner by `Control` ([`actions.ts:180-215`](../src/actions.ts#L180-L215)).
6. The core emits events; the handler of `serve.ts` passes them to the agent control chat and to `AgentTools.onEvent`, which passes them to the tools and the item watch ([`serve.ts:125-137`](../src/serve.ts#L125-L137), [`tools.ts:665-692`](../src/tools.ts#L665-L692)).

---

## 2. Modules

### [2.1 `cli.ts`](../src/cli.ts)

Entry point of the `simplex-mcp` binary.

| Symbol | Line | Contents |
|---|---|---|
| `helpText` | [14](../src/cli.ts#L14) | `--help` output |
| `pluginCommands` | [55](../src/cli.ts#L55) | `session`, `service`, `hook session-start`, `hook turn`, `link`, `reset-owners` |
| `Command` | [57](../src/cli.ts#L57) | `help`, `serve` with `ServeOptions`, `register` with `RegisterOptions`, the plugin commands |
| `parseCommand` | [63](../src/cli.ts#L63) | command, NAME and options to a `Command`; the joined arguments equal to a plugin command |
| `serveOptions` | [74](../src/cli.ts#L74) | defaults and option switch of `serve` |
| `registerOptions` | [130](../src/cli.ts#L130) | defaults and option switch of `register` |
| `OptionReader` | [147](../src/cli.ts#L147) | `value`: the next argument, required<br>`optionalValue`: the next argument, absent when missing or starting with `--` |
| `validAgentName` | [169](../src/cli.ts#L169) | `^[a-z0-9](-?[a-z0-9])*-?$`, at most 32 characters |
| `usageError` | [214](../src/cli.ts#L214) | `ExitError` with code 2 |
| `main` | [218](../src/cli.ts#L218) | `help` to stdout, `serve`, `register`; `session`, `service`; the hooks read stdin and print their JSON output; `link` and `reset-owners` print their text |
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
| `prepareCore` | [core.ts:37](../src/core.ts#L37) | network config, `DATA/tmp` as the core's temp folder, mode 0700, and `DATA/files/sent` deleted |
| `updatedUser` | [core.ts:54](../src/core.ts#L54) | display name and peer type `agent` of the profile |
| `bufferEvents` | [core.ts:61](../src/core.ts#L61) | events received before the handler is installed are queued and replayed in order |
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
| `Agent` | [117](../src/tools.ts#L117) | `ChatApi`, user, `Actions`, `ChatItemsWatch`, control chat reference, standing approver lookup, refused file post, data folder; for the plugin also the data root, the owning session ID, the group of `reply`, the mode setter and the record of a pending connection |
| `JoinMethod` | [133](../src/tools.ts#L133) | `prepared`, `prepareChannel`, `viaAddress`, `prepareContact`, `connect` |
| schemas | [189-412](../src/tools.ts#L189-L412) | input and output JSON schemas |
| descriptions, `instructions` | [414-452](../src/tools.ts#L414-L452) | tool descriptions and server instructions; `instructions` names the host whose restart cancels actions, `server` by default |
| `replyTool` | [462](../src/tools.ts#L462) | the plugin's seventh tool, `reply` |
| `definitions` | [485](../src/tools.ts#L485) | name, description, schemas and action flag of the six tools |
| `registerTools` | [498](../src/tools.ts#L498) | `registerTool` for each definition and each extra one; `_meta` on action tools in mode `host` |
| `Download` | [510](../src/tools.ts#L510) | one download: its promise, chat, item, file ID, size, path, the last fileSource seen, received bytes and their time, check timer |
| `AgentTools` | [527](../src/tools.ts#L527) | constructor with the `Agent`, the mode or a function returning it, and the session number of the plugin; `mode` reads it at each use |
| `AgentTools.register` | [540](../src/tools.ts#L540) | `registerTools` with `AgentTools.call` |
| `AgentTools.call` | [544](../src/tools.ts#L544) | handler result to `CallToolResult`; errors to `isError` |
| `AgentTools.perform` | [553](../src/tools.ts#L553) | perform step of an approved action; for a `mode` action, the mode setter |
| `giveChat`, `owns`, `claim` | [568-586](../src/tools.ts#L568-L586) | the plugin's chat owners: a chat is the session's when no session ID is set or the chat's owner is that ID; `claim` sets the owner of a chat without one, at a connection, at a `connect` to a known chat ([`ownChat`](../src/tools.ts#L750-L755)) and for a pending connection from the hub |
| `onEvent` | [665](../src/tools.ts#L665) | events to the file handlers, connect completions and the item watch; shared by `serve` and the plugin's sessions |
| `reply`, `sendFile` | [713-730](../src/tools.ts#L713-L730) | a file to the group of the plugin's session, under the file rules of `send`, its message with the caption and the preview checked by `fitsMessage`, sent at once without an action; the core's refusal `feature not allowed NAME` answered with the text that names the preference; the post of the file |
| `refusedFeature`, `featureOff` | [1607-1614](../src/tools.ts#L1607-L1614) | the preference NAME of a command error `feature not allowed NAME`; `NAME are off in the group of this session; its owners can turn them on in the group's preferences.`, for `reply`, the cards, `/forward` and answers of a session group |
| `downloadFile`, `forwardFile`, `downloadTo` | [587-614](../src/tools.ts#L587-L614) | one download per file ID, shared by `download` and `/forward`; a forwarded file moved to the download path for `download` |
| file events | [616-653](../src/tools.ts#L616-L653) | `onNewItems`, `onFile`, `onFileComplete`, `onFileError`, `onFileCancelled`, `onFileDescription` |
| `onContactConnected`, `onUserJoinedGroup` | [655-663](../src/tools.ts#L655-L663) | completion of connect actions by events |
| `actionResult` | [761](../src/tools.ts#L761) | wait for the decision, result fields, removal |
| `attachment`, `isRefused`, `fileToSend`, `resizedCopy` | [985-1021](../src/tools.ts#L985-L1021) | the file of a `send` |
| `copyForSending`, `chatFiles`, `deleteChatFiles` | [1023-1052](../src/tools.ts#L1023-L1052) | `/forward` copies; the files of a chat, forwarded ones included; its downloads cancelled, also those the core continues after a restart |
| `leaveChat` | [1074](../src/tools.ts#L1074) | perform step of `leave` and `/leave`; the chat's forwarded files deleted |
| `download`, `fileServers`, `unknownServers` | [1084-1129](../src/tools.ts#L1084-L1129) | the `download` call before its action |
| `startDownload`, `checkDownload`, `currentFile`, `settleFile`, `settleReceived`, `moveReceived` | [1147-1214](../src/tools.ts#L1147-L1214) | a download's steps |
| `agentChats`, `knownChat` | [1284-1303](../src/tools.ts#L1284-L1303) | the agent's chats; a checked chat reference |
| `classifyPlan` | [1386](../src/tools.ts#L1386) | `apiConnectPlan` response to a join plan or a known chat |
| `chatSubject`, `isConnected`, `isActive`, `memberCurrent`, `memberCurrentOrPending` | [1482-1551](../src/tools.ts#L1482-L1551) | chat and member predicates shared with `control.ts` and `hub.ts` |
| `customDataOf`, `setChatData`, `ownerOf` | [1515-1530](../src/tools.ts#L1515-L1530) | a chat's custom data; the plugin's owner key `simplexMcpOwner` |
| `tooLongText`, `fitsMessage`, `quoteContent` | [1571-1591](../src/tools.ts#L1571-L1591) | the refusal text of `send` and `reply` for a message too long; the message as the core sends it, within `maxMessageLength`, 15602 bytes ([163](../src/tools.ts#L163)); for `send`, `reply`, the post of `/forward`, and the parts and quotes of the plugin's posts |
| `realFile` | [1616](../src/tools.ts#L1616) | real path, size, inode and change time of a file |
| `attachmentMedia`, `videoFrame`, `videoDuration` | [1663-1697](../src/tools.ts#L1663-L1697) | previews and durations |
| `toolCall` | [1713](../src/tools.ts#L1713) | `ServerContext` to `ToolCall`: abort signal and progress notifications |

### [2.5 `actions.ts`](../src/actions.ts)

| Symbol | Line | Contents |
|---|---|---|
| `Mode` | [5](../src/actions.ts#L5) | `requests`, `host` |
| `State` | [6](../src/actions.ts#L6) | `pending`, `performing`, `done`, `failed`, `rejected`, `expired` |
| `Standing` | [7](../src/actions.ts#L7) | `messages`, `files` |
| `ToolError` | [18](../src/actions.ts#L18) | errors the model can correct; text returned as is |
| `Subject` | [26](../src/actions.ts#L26) | kind, name, SimpleX name, incognito flag of a card |
| `ConnRef` | [33](../src/actions.ts#L33) | pending connection, contact or group of a connect action |
| `HostTexts` | [20](../src/actions.ts#L20) | refusal texts of a lost control chat, of a stop and of the limit of pending actions: `lostMessage`, `stoppingMessage` and the owner's text for `serve`, the hub's texts for the plugin |
| `ActionRecord` | [39](../src/actions.ts#L39) | number, session number of the plugin, mode, key, times, change signal, standing flag, state, card, decider, open calls, interrupted flag, ended flag, error, card image |
| `ConnectAction`, `SendAction`, `LeaveAction`, `DownloadAction` | [59-118](../src/actions.ts#L59-L118) | per-tool fields; `Attachment`, the file of a send |
| `ModeAction`, `PermissionAction` | [120-133](../src/actions.ts#L120-L133) | the plugin's actions: a switch to mode `host`; a tool use of Claude Code, with the bytes left for its input preview |
| `ActionHooks` | [139](../src/actions.ts#L139) | `refusal`, `saveCounter`, `postCard`, `perform`, `outcome` |
| `Actions` | [147](../src/actions.ts#L147) | action map, one expiry timer per action, posting and outcome promises |
| `create` | [180](../src/actions.ts#L180) | state by the action's mode, refusal checks, number, log line, counter write, card |
| `ofSession`, `remove`, `openCall`, `closeCall`, `end` | [217-241](../src/actions.ts#L217-L241) | the actions of one session; removal; open call count; an expiry before the expiry time, marked ended |
| `expireDue` | [243](../src/actions.ts#L243) | expiry of every action past its expiry time |
| `decided` | [249](../src/actions.ts#L249) | wait with progress until a decided state, `until`, or the abort signal |
| `stop` | [284](../src/actions.ts#L284) | cancellation of pending and performing actions |
| `scheduleExpiry`, `expire`, `forgetExpired` | [297-316](../src/actions.ts#L297-L316) | the expiry timer of one action, at most 60 s; its expiry, or a new timer when the timer, on the monotonic clock, fires before the expiry time on the wall clock; its removal once decided, without open calls and past its expiry time; an action of `serve` that expires is kept until the next owner command or its check 60 s later, as in 0.2 |
| `summary`, `performedText`, `progressText`, `sentence` | [382-429](../src/actions.ts#L382-L429) | log and progress texts; a log line ended with a period: for an action of a session, one also after an error that ends with one; for an action of `serve`, a period after every error, as in 0.2 |

Hooks are bound in [`serve.ts:96-102`](../src/serve.ts#L96-L102): `refusal`, `saveCounter`, `postCard` and `outcome` to `Control`; `perform` to `AgentTools`.

### [2.6 `control.ts`](../src/control.ts)

| Symbol | Line | Contents |
|---|---|---|
| `qrCodeText` | [11](../src/control.ts#L11) | the function of `qr.ts` ([qr.ts:14](../src/qr.ts#L14)) exported again, where 0.2 had it |
| `controlCommands` | [13](../src/control.ts#L13) | the command menu |
| `standings` | [27](../src/control.ts#L27) | custom data key and reply words of the approvals of messages and files |
| `ControlData` | [46](../src/control.ts#L46) | `{control: true, nextAction}` |
| `ChatApproval` | [70](../src/control.ts#L70) | `{approvedBy, approvedById, approvedAt}` |
| `ControlOptions`, `SessionOptions` | [51-63](../src/control.ts#L51-L63) | display name, `--owners`, register hint, QR code size; for a group of the plugin's session, its approver check, the session IDs by number and the check of `User` |
| `OwnerRequest` | [76](../src/control.ts#L76) | parsed owner command |
| `Control.load` | [115](../src/control.ts#L115) | lost check, group profile and menu from the group's current profile, member list; the custom data in memory kept across the profile update |
| `Control.rename`, `Control.describe`, `updateProfile` | [124-140](../src/control.ts#L124-L140) | the plugin's group name; its short description; the profile updates one at a time, each from the group's current profile, the custom data in memory kept |
| `Control.start` | [142](../src/control.ts#L142) | owner setup per `--owners` |
| `Control.refusal` | [150](../src/control.ts#L150) | bytes left for the link texts of a connect card and for the input preview of a tool-use card, card size of a send, connect or download, approver presence |
| `Control.hasApprover` | [163](../src/control.ts#L163) | an approver is a member |
| `Control.standingApprover` | [167](../src/control.ts#L167) | approver name of a chat's standing approval of messages or files |
| `Control.saveCounter`, `Control.saveData` | [172-184](../src/control.ts#L172-L184) | queued writes of the counter and of other keys of the group's custom data |
| `Control.postCard`, `Control.outcome` | [186-197](../src/control.ts#L186-L197) | card post and card edit; the kept-files message after a done leave |
| `Control.refusedFile`, `Control.fileNotSent` | [199-214](../src/control.ts#L199-L214) | posts about files |
| `Control.onMessage` | [216](../src/control.ts#L216) | owner commands, queued one at a time; in a group of the plugin's session, the session passes it the one-line `/` texts, and those of `User` are dropped |
| `Control.onEvent` | [228](../src/control.ts#L228) | membership changes and loss of the control chat |
| `Control.ownerCommand` | [246](../src/control.ts#L246) | command dispatch and replies; for a text outside the requests, the command list, or `Unknown command.` in a group of the plugin's session |
| `approveChat`, `revoke`, `forward`, `leave`, `deleteFiles` | [293-360](../src/control.ts#L293-L360) | owner requests |
| `give`, `setObserver` | [362-387](../src/control.ts#L362-L387) | `/give CHAT N` and `/observer #G ID` in a group of the plugin's session; for a member current or pending, the role `observer` set when the sender's role is admin or owner and not below the member's role, the members read again, and the observer's rights as the reply |
| `isApprover` | [593-598](../src/control.ts#L593-L598) | the session's check in a group of the plugin's session; otherwise role `owner`, and the `--owners` list when given |
| `roleOrder`, `roleRank`, `canSend` | [44](../src/control.ts#L44), [880-886](../src/control.ts#L880-L886) | the roles of SimpleX from `relay` to `owner`, in the order of the core; a role above `observer` |
| `sessionApproverRoles`, `rightsText` | [40-43](../src/control.ts#L40-L43), [888-891](../src/control.ts#L888-L891) | `admin` and `owner`, the approvers of a session group; `NAME is a ROLE and RIGHTS.`, with the rights of the approvers, of the other roles that send, or of an observer |
| `postCardContent`, `sessionRefusal`, `postRefusal`, `refusedCardSubject` | [521-544](../src/control.ts#L521-L544), [756-760](../src/control.ts#L756-L760) | the post of a card; in a plugin session group, a card the core refuses with `feature not allowed Files and media` posted again as text, with its later edits as text; a card or an edit refused for another preference before the decision or at a stop replaced by `Action N: ` and the text that names it, and an action that waits for approval failed with that text; after the decision, by `Action N OUTCOME; its card was not posted: ` or `...; its card was not updated: ` and that text, OUTCOME being `was performed`, `failed`, `was rejected`, `expired` or `ended`, the action kept as it was |
| `cardContent`, `fittedImage` | [546-566](../src/control.ts#L546-L566) | image and video cards with the file's preview |
| `findControlGroup`, `createControlChat`, `resetControlChat` | [605-645](../src/control.ts#L605-L645) | control group lookup, creation, reset |
| `createGroup`, `updateGroupProfile`, `changeGroupProfile`, `deleteGroup` | [626-650](../src/control.ts#L626-L650) | a group with its profile and custom data, shared with the plugin's groups; a profile update from the group's current profile in the chat database, shared with the Main group, or, for the control chat of `serve`, from the profile read at the start, as in 0.2, each made only when the change returns a profile; deletion for all members |
| `isGroupMessage` | [652](../src/control.ts#L652) | an item of one group's main scope |
| `isControlEvent` | [656](../src/control.ts#L656) | membership events of the control group |
| `parseCommand` | [672-677](../src/control.ts#L672-L677) | one-line `/keyword params` |
| `cardText`, `requestLines`, `commandsLine` | [679-750](../src/control.ts#L679-L750) | card text per tool, state and mode |
| `largestCard`, `encodedLength` | [762-770](../src/control.ts#L762-L770) | card size check |
| `truncatedLine`, `fittedLines` | [772-798](../src/control.ts#L772-L798) | a line cut with `…`; the code lines that fit a byte budget, the last one cut |
| `serversLine`, `sizeText` | [804-813](../src/control.ts#L804-L813) | servers line and sizes of download cards |
| `codeSpan`, `safeName`, `mentionText` | [823-835](../src/control.ts#L823-L835) | quoting of peer text |
| `ownerRequest` | [846](../src/control.ts#L846) | command words to an `OwnerRequest`; `observer #G ID` only for a group of the plugin's session |
| `losesControl`, `isLost` | [863-878](../src/control.ts#L863-L878) | loss of the control chat, also of the plugin's groups |
| `joinedText` | [905](../src/control.ts#L905) | `NAME joined (member N)`, shared with the Main group |
| `entryText`, `restartText` | [909-919](../src/control.ts#L909-L919) | `--owners` entries in restart lines |

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
| `createDataFolder` | [103](../src/files.ts#L103) | `mkdir -p` mode 0700; `chmod 0700` except on Windows |
| `acquireLock`, `releaseLock` | [108-122](../src/files.ts#L108-L122) | `DATA/lock`, or the file of the given name, with the PID; `user`, `server` or `service`, names the holder in the error |
| `serverToken` | [124](../src/files.ts#L124) | `DATA/token`, or the file of the given name: read, made private when the user owns it, or 32 random bytes in base64url, created as the lock is |
| `saveHeldTitles`, `takeHeldTitles` | [153-167](../src/files.ts#L153-L167) | `ROOT/titles.json`, written at a service stop, read and removed at the next start; an unparsable file is skipped with a log line |
| `createLock`, `createExclusive` | [182-204](../src/files.ts#L182-L204) | a private temporary file with the text, linked into place, so that the file appears complete and once; where the file system refuses the link, written exclusively |
| `takeOver` | [206](../src/files.ts#L206) | the lock of a stopped process: claimed first by a link of the lock to `lock.PID.claim`, or where links fail by an exclusive write of `lock.PID.claim`, so that one starter claims it; when the lock still holds that PID, it is removed and created again; the claim removed |
| `writeHeadersFile` | [140](../src/files.ts#L140) | `DATA/headers.json` |
| `writeServerFile`, `readServerFile` | [144-151](../src/files.ts#L144-L151) | `DATA/server.json` |

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
| Actions | `Actions.actions` map ([actions.ts:151](../src/actions.ts#L151)) | `create`, `remove`, `forgetExpired` | tools, `Control` |
| Expiry timers | `Actions.expiries`, one per action ([actions.ts:152](../src/actions.ts#L152)) | `scheduleExpiry`, `expire`, `remove` | |
| Open calls of an action | `ActionRecord.open` ([:51](../src/actions.ts#L51)) | `openCall` and `closeCall` from `actionResult` ([tools.ts:762](../src/tools.ts#L762), [:771](../src/tools.ts#L771)) | `list`, `forgetExpired` |
| Collected replies | `ConnectAction.messages`, `SendAction.responses` ([:64](../src/actions.ts#L64), [:95](../src/actions.ts#L95)) | `ChatItemsWatch.collect` | `connect`, `send` results |
| Item listeners | `ChatItemsWatch.listeners` ([messages.ts:87](../src/messages.ts#L87)) | `collect` | event dispatch |
| Downloads | `AgentTools.downloads`, one per file ID ([tools.ts:528](../src/tools.ts#L528)) | `downloadTo` | file events, checks, `/delete CHAT files` |
| Forwards | `AgentTools.forwarding`, the copy for the post per file ID ([tools.ts:529](../src/tools.ts#L529)) | `forwardFile` | `downloadFile`, before it moves a forwarded file |
| Rejected files | `AgentTools.rejectedFiles` ([tools.ts:530](../src/tools.ts#L530)) | `downloadResult` ([:1136-1137](../src/tools.ts#L1136-L1137)) | the file field; `download`, which then skips the files approval ([:1099](../src/tools.ts#L1099)) |
| Count of file descriptions | `AgentTools.describedFiles` ([tools.ts:531-532](../src/tools.ts#L531-L532)) | `onFileDescription` | `fileServers` |
| Members of the control chat | `Control.members` ([control.ts:94](../src/control.ts#L94)) | `refreshMembers` ([:568-570](../src/control.ts#L568-L570)) | approver checks |
| Mode, expiry, owners list, stop wait | `ServeOptions` of the run; the mode also in `Actions.mode` and in each action ([actions.ts:42](../src/actions.ts#L42)) | CLI | all modules |

### 3.2 Chat database custom data

| Item | Entity | Value | Written by | Read by |
|---|---|---|---|---|
| Control marker and counter | agent control chat | `{simplexMcp: {control: true, nextAction: N}}` | [`createControlChat`](../src/control.ts#L615-L630), [`saveCounter`](../src/control.ts#L172) | [`findControlGroup`](../src/control.ts#L605), [`controlDataOf`](../src/control.ts#L610) |
| Standing approval of messages | contact or group of the agent | `{...other keys, simplexMcp: {approvedBy, approvedById, approvedAt}}` | [`approveChat`](../src/control.ts#L293), [`revoke`](../src/control.ts#L306) | [`standingApprover`](../src/control.ts#L167) |
| Files approval | contact or group of the agent | `{...other keys, simplexMcpFiles: {approvedBy, approvedById, approvedAt}}` | [`approveChat`](../src/control.ts#L293), [`revoke`](../src/control.ts#L306) | [`standingApprover`](../src/control.ts#L167) |

The profile, chats, items, members and file transfers are stored by the core in `DATA/simplex_chat.db`; connections and queues in `DATA/simplex_agent.db` (file prefix `DATA/simplex`, [serve.ts:71](../src/serve.ts#L71)).

### 3.3 Files

| Path | Contents | Mode | Written | Read |
|---|---|---|---|---|
| `DATA/` | data folder | 0700, set at every start except on Windows | [files.ts:103-106](../src/files.ts#L103-L106) | |
| `DATA/token` | base64url of 32 random bytes | 0600, set again at every write except on Windows | first start, `--new-token` ([:124-134](../src/files.ts#L124-L134)) | every start |
| `DATA/headers.json` | `{"Authorization":"Bearer <token>"}` | 0600, set again at every write except on Windows | every start ([:140-142](../src/files.ts#L140-L142)) | Claude Code `headersHelper` |
| `DATA/server.json` | `{name, port, expireHours}` | 0600, set again at every write except on Windows | every start, after the HTTP listener is bound ([serve.ts:114](../src/serve.ts#L114)) | `register` |
| `DATA/lock` | PID and a newline | 0600 | start ([files.ts:182-221](../src/files.ts#L182-L221)) | next start |
| `DATA/tmp/` | the core's temp folder: files in transfer | 0700, set at every start | [core.ts:40](../src/core.ts#L40), [:77-81](../src/core.ts#L77-L81) | the core |
| `DATA/files/group-N/`, `DATA/files/contact-N/` | downloaded files and files received without a download, named by file ID and extension | folder 0700 | the core at the download path ([tools.ts:1152-1153](../src/tools.ts#L1152-L1153)); moves ([:1651-1661](../src/tools.ts#L1651-L1661)) | `download` results, `/forward`, `/leave`, `/delete CHAT files` |
| `DATA/forwarded/group-N/`, `DATA/forwarded/contact-N/` | files `/forward` downloaded, named as above, until a download moves them to `DATA/files/` | folder 0700 | the core at the forwarded path ([tools.ts:597-604](../src/tools.ts#L597-L604), [:1152-1153](../src/tools.ts#L1152-L1153)) | `/forward`; moved by `download` ([:587-595](../src/tools.ts#L587-L595)); deleted by `leave`, `/leave` and `/delete CHAT files` ([:1034-1052](../src/tools.ts#L1034-L1052), [:1074-1082](../src/tools.ts#L1074-L1082)) |
| `DATA/files/sent/` | resized copies of sent images, and copies of the files `/forward` posts, one folder per copy | folder 0700; a resized copy 0600 | [tools.ts:1010-1027](../src/tools.ts#L1010-L1027), [:1054-1058](../src/tools.ts#L1054-L1058) | the core; deleted at `sndFileCompleteXFTP`, `sndFileError` and at start ([core.ts:41](../src/core.ts#L41), [tools.ts:688-690](../src/tools.ts#L688-L690), [:973](../src/tools.ts#L973)) |

The core library is downloaded once into `CACHE/core-7.1.0.20/sqlite`, and the Node.js add-on into `CACHE/nodejs-addon-cce06d8/`, each with a `Downloading ...` line on stderr. CACHE is `$XDG_CACHE_HOME/simplex-chat` or `~/.cache/simplex-chat`; on macOS `~/Library/Caches/simplex-chat`; on Windows `%LOCALAPPDATA%\simplex-chat` ([libs.ts:41-48](../../simplex-chat-nodejs/src/libs.ts#L41-L48), [:89](../../simplex-chat-nodejs/src/libs.ts#L89), [:109](../../simplex-chat-nodejs/src/libs.ts#L109), [:116](../../simplex-chat-nodejs/src/libs.ts#L116), [:129](../../simplex-chat-nodejs/src/libs.ts#L129)).

---

## 4. Process lifecycle

### 4.1 Start

1. Data folder, lock, token and headers file are prepared ([serve.ts:66-70](../src/serve.ts#L66-L70)).
2. libsimplex for SQLite is loaded by `ChatApi.init`, and the databases are migrated with confirmation `yesUp` ([serve.ts:71](../src/serve.ts#L71), [api.ts:117-126](../../simplex-chat-nodejs/src/api.ts#L117-L126)).
3. Three log lines: version and NAME, data folder, mode ([serve.ts:72-74](../src/serve.ts#L72-L74)).
4. The active user is read, or created with display name `--display-name` or NAME, empty full name and peer type `agent` ([serve.ts:89](../src/serve.ts#L89)).
5. `prepareCore` reads the network config and sets it with `smpProxyMode` `always` and `smpProxyFallback` `allowProtected` ([serve.ts:90](../src/serve.ts#L90), [core.ts:38-39](../src/core.ts#L38-L39)).
6. `prepareCore` sets `DATA/tmp` as the core's temp folder and deletes `DATA/files/sent` ([core.ts:40-41](../src/core.ts#L40-L41)).
7. Events are buffered, and the chat is started ([serve.ts:91-92](../src/serve.ts#L91-L92)).
8. A `--display-name` different from the profile, or a peer type other than `agent`, is applied with one `apiUpdateProfile` ([serve.ts:93](../src/serve.ts#L93), [core.ts:54-59](../src/core.ts#L54-L59)).
9. The agent control chat is found by its marker, created with counter 1, or reset with `--reset-owners` ([:95](../src/serve.ts#L95), [:177-182](../src/serve.ts#L177-L182)).
10. `Actions`, `ChatItemsWatch`, `AgentTools` and `Control` are created; the counter is taken from the marker ([:96-105](../src/serve.ts#L96-L105)).
11. `Control.load`: for a lost control chat, `Actions.lost` is set; otherwise the group name and command menu are updated when they differ in the control chat's profile read at the start, as in 0.2, and members are read ([control.ts:115-122](../src/control.ts#L115-L122), [:133-140](../src/control.ts#L133-L140), [:637-640](../src/control.ts#L637-L640)).
12. The HTTP server listens; with the port in use, the process exits with code 1 ([serve.ts:113](../src/serve.ts#L113), [:184-191](../src/serve.ts#L184-L191)). Until step 14 ends, requests to `/mcp` that pass the token check get 503 with `Retry-After: 1` ([http.ts:23](../src/http.ts#L23), [serve.ts:112-113](../src/serve.ts#L112-L113), [:121](../src/serve.ts#L121)).
13. `server.json` is written ([serve.ts:114](../src/serve.ts#L114)).
14. The server runs the owner setup, `Control.start`; after an error, HTTP is closed and the error is rethrown ([serve.ts:115-120](../src/serve.ts#L115-L120)).
15. `Listening on http://127.0.0.1:PORT/mcp` ([:122](../src/serve.ts#L122)).
16. Buffered events are replayed, then live events are dispatched ([serve.ts:125](../src/serve.ts#L125), [core.ts:61-75](../src/core.ts#L61-L75)).

After an error inside `startAgent` (steps 4-16) the chat is closed; after any error once the lock is taken, the lock is released ([serve.ts:68-84](../src/serve.ts#L68-L84)).

### 4.2 Event dispatch

Each `onAny` receiver is awaited by the library before the next event is read ([api.ts:195-202](../../simplex-chat-nodejs/src/api.ts#L195-L202)). The dispatcher in [`serve.ts:125-137`](../src/serve.ts#L125-L137) passes items of the control group to `Control`, then every event to [`AgentTools.onEvent`](../src/tools.ts#L665-L692), which runs the `AgentTools` and `ChatItemsWatch` handlers below, then file errors and membership events to `Control`:

| Event | Condition | Handler |
|---|---|---|
| `newChatItems` | item of the control group, main scope | [`Control.onMessage`](../src/control.ts#L216) |
| `newChatItems` | every item | [`AgentTools.onNewItems`](../src/tools.ts#L616), then [`ChatItemsWatch.deliver`](../src/messages.ts#L116) |
| `chatItemUpdated` | | [`AgentTools.onFile`](../src/tools.ts#L626), then [`ChatItemsWatch.update`](../src/messages.ts#L120) |
| `chatItemsDeleted` | | [`ChatItemsWatch.remove`](../src/messages.ts#L124) of each `deletedChatItem` |
| `contactConnected` | | [`AgentTools.onContactConnected`](../src/tools.ts#L655) |
| `userJoinedGroup` | | [`AgentTools.onUserJoinedGroup`](../src/tools.ts#L660) |
| `rcvFileComplete` | | [`AgentTools.onFileComplete`](../src/tools.ts#L631) |
| `rcvFileSndCancelled` | | [`AgentTools.onFileCancelled`](../src/tools.ts#L643) |
| `rcvFileError` | | [`AgentTools.onFileError`](../src/tools.ts#L638) |
| `rcvFileDescrReady` | | [`AgentTools.onFileDescription`](../src/tools.ts#L650) |
| `sndFileCompleteXFTP` | | [`AgentTools.onSentFile`](../src/tools.ts#L973) |
| `sndFileError` | | [`AgentTools.onSentFile`](../src/tools.ts#L973), then [`Control.fileNotSent`](../src/control.ts#L205) |
| `joinedGroupMember`<br>`joinedGroupMemberConnecting`<br>`connectedToGroupMember`<br>`memberRole`<br>`leftMember`<br>`deletedMember`<br>`deletedMemberUser`<br>`groupDeleted` | event of the control group | [`Control.onEvent`](../src/control.ts#L228), queued one at a time |
| any event | after `stop()` started | ignored |

Owner commands are queued in a second chain, independent of the membership chain ([control.ts:222-225](../src/control.ts#L222-L225)).

### 4.3 Stop

On `SIGINT` or `SIGTERM` the server runs one stop; a later signal ends its wait for cancelled cards ([serve.ts:46-62](../src/serve.ts#L46-L62)). `RunningServer.stop` ([serve.ts:143-153](../src/serve.ts#L143-L153)):

1. Events are ignored from now on.
2. `Actions.stop` starts: new actions are refused with `The server is stopping.`; the expiry timers are cleared; cards being posted are awaited; pending and performing actions are marked interrupted; the stop signal of the performs is aborted, which ends the wait of a `connect` for its connection; their cards are edited, or posted when they have no card; outcome edits already in progress are awaited ([actions.ts:284-316](../src/actions.ts#L284-L316)).
3. In parallel with step 2, the HTTP server is closed with all connections, and open calls are aborted ([http.ts:48-52](../src/http.ts#L48-L52)).
4. `Cancelled K action(s).` when K > 0 ([serve.ts:148](../src/serve.ts#L148)).
5. When cards were edited or posted at the stop and the control chat is not lost, the server waits `--stop-wait` seconds, 10 by default, for them to be sent; a second signal ends the wait ([serve.ts:149](../src/serve.ts#L149), [:161-175](../src/serve.ts#L161-L175)).
6. `chat.close()`: the chat is stopped, then the store is closed ([api.ts:165-170](../../simplex-chat-nodejs/src/api.ts#L165-L170)).
7. The lock is removed, `Stopped.` is logged, and the process exits with code 0, or 1 after an error ([serve.ts:151-152](../src/serve.ts#L151-L152), [:52-58](../src/serve.ts#L52-L58)).

Performs in progress are not awaited; when one ends, its card stays as edited at the stop and no line is logged, because the action is interrupted ([actions.ts:346](../src/actions.ts#L346)).

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

`isRunning`: `true` when `kill(pid, 0)` succeeds or fails with `EPERM` ([files.ts:228-235](../src/files.ts#L228-L235)).

---

## 5. Approval model

### 5.1 Modes

The mode is set by `--approve` for one run ([cli.ts:96-98](../src/cli.ts#L96-L98)).

| Mode | State of a new action | Card | `_meta` of `connect`, `send`, `download`, `leave` |
|---|---|---|---|
| `requests` | `pending`<br>`performing` for a send under a standing approval, and for a download under a files approval | card with commands<br>outcome as a new message after a standing send; none after a download without a card | none |
| `host` | `performing` | outcome as a new message after the perform; none for a download | `{"anthropic/requiresUserInteraction": true}` |

An action keeps the mode it was created in, `ActionRecord.mode`; its card and its log line follow that mode ([actions.ts:180](../src/actions.ts#L180), [:193](../src/actions.ts#L193), [control.ts:151](../src/control.ts#L151)).

Sources: [actions.ts:193](../src/actions.ts#L193), [tools.ts:504](../src/tools.ts#L504), [control.ts:190-197](../src/control.ts#L190-L197), [:601-603](../src/control.ts#L601-L603). In both modes a new action is refused unless the control chat has an approver ([control.ts:156-160](../src/control.ts#L156-L160)).

### 5.2 Owners and approvers

| Term | Rule | Code |
|---|---|---|
| human | member other than the agent's membership, status `connected` or `complete` | [control.ts:589-591](../src/control.ts#L589-L591), [tools.ts:1532-1534](../src/tools.ts#L1532-L1534) |
| human owner | human with role `owner` | [control.ts:576-578](../src/control.ts#L576-L578) |
| approver, `--owners` absent | member with role `owner` | [control.ts:593-598](../src/control.ts#L593-L598) |
| approver, `--owners` without a list | none | [control.ts:597](../src/control.ts#L597) |
| approver, `--owners ID:NAME,...` | member with role `owner` whose `groupMemberId` and `displayName`, folded to one line by `oneLine`, match an entry | [control.ts:597](../src/control.ts#L597), [:893-895](../src/control.ts#L893-L895) |

The command sender's member record from the chat item is checked ([control.ts:220](../src/control.ts#L220), [:250](../src/control.ts#L250)). The approver count for refusals and standing approvals is taken from `Control.members`, refreshed at load and at each membership event ([control.ts:232](../src/control.ts#L232), [:568-570](../src/control.ts#L568-L570)).

Owner setup at start ([control.ts:142-148](../src/control.ts#L142-L148)):

| `--owners` | Start | First join by the link |
|---|---|---|
| absent | a human owner exists: register hint<br>otherwise: `No owners in the agent control chat.` when humans exist, then a link with role `member` | link deleted, role `owner`, greeting, log line, register hint |
| without a list | no humans: link with role `observer`<br>humans exist: their IDs and the restart command, in the log and the chat | link deleted, the joined member and the restart command announced |
| `ID:NAME,...` | each entry matched to a human, or exit 2<br>role `owner` set where missing<br>other humans removed<br>link deleted<br>`Owners: ...` and register hint | |

Sources: [`startOwners`](../src/control.ts#L421), [`startObserver`](../src/control.ts#L427), [`startListed`](../src/control.ts#L432), [`ownerJoined`](../src/control.ts#L451), [`observerJoined`](../src/control.ts#L460).

Membership changes at run time ([control.ts:228-244](../src/control.ts#L228-L244)):

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
| call creates the action, mode `requests` | | `pending` | [actions.ts:193](../src/actions.ts#L193) |
| call creates the action, mode `host` | | `performing` | [actions.ts:193](../src/actions.ts#L193) |
| `send` call under a standing approval, or `download` call under a files approval | | `performing` | [actions.ts:193](../src/actions.ts#L193) |
| counter write or card post fails | `pending`<br>`performing` | `failed` | [actions.ts:318-329](../src/actions.ts#L318-L329) |
| stop arrives while the counter of a `performing` action is written | `performing` | `pending`, interrupted | [actions.ts:331-336](../src/actions.ts#L331-L336) |
| control chat lost | `pending` | `failed` | [actions.ts:276-282](../src/actions.ts#L276-L282) |
| `/reject`, or `/leave CHAT` for a pending action of CHAT with a card | `pending` | `rejected` | [actions.ts:269-274](../src/actions.ts#L269-L274), [control.ts:344-346](../src/control.ts#L344-L346) |
| expiry time passes | `pending` | `expired` | [actions.ts:302-312](../src/actions.ts#L302-L312) |
| `/approve N`, `/approve CHAT N` and `/approve CHAT all` for other actions; `/receive N`, `/receive CHAT N` and `/receive CHAT N all` for downloads | `pending` | `performing` | [actions.ts:263-267](../src/actions.ts#L263-L267), [control.ts:279-288](../src/control.ts#L279-L288) |
| perform throws | `performing` | `failed` | [actions.ts:342-345](../src/actions.ts#L342-L345) |
| perform ends | `performing` | `done` | [actions.ts:340-341](../src/actions.ts#L340-L341) |

`create` ([actions.ts:180-215](../src/actions.ts#L180-L215)):

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

An action is removed from memory when its result is returned to a call ([tools.ts:768](../src/tools.ts#L768)), or once it is decided, its open call count is 0 and its expiry time has passed: checked at each decision, at the close of each call and at its expiry timer ([actions.ts:314-316](../src/actions.ts#L314-L316), [:232-235](../src/actions.ts#L232-L235), [:351-355](../src/actions.ts#L351-L355)). `serve` has one session, `undefined`, so the limits of 20 count all its actions; a pending action of `serve` that expires is kept until the next owner command or its check 60 s later, so a repeated call before them gets `expired`, as in 0.2 ([actions.ts:307-309](../src/actions.ts#L307-L309), [:243-247](../src/actions.ts#L243-L247), [control.ts:247](../src/control.ts#L247)). A `download` still performing at the end of `wait` is returned as `performing` and kept ([tools.ts:765](../src/tools.ts#L765)).

### 5.4 Cards

The server posts a card when a `pending` action is created, and edits it with `apiUpdateChatItem` at the outcome ([actions.ts:321](../src/actions.ts#L321), [control.ts:190-197](../src/control.ts#L190-L197)). For a `connect`, `send` or `leave` without a card (mode `host`, standing sends) the server posts the outcome as a new message; a `download` without a card gets none ([control.ts:601-603](../src/control.ts#L601-L603)). The text per state is built by [`cardText`](../src/control.ts#L679-L698); a card of a `send` or `download` of an image or video file is an image or video message with the file's preview, or text in a plugin session group whose preference refuses files and media ([control.ts:521-566](../src/control.ts#L521-L566)); forms are listed in [API: cards](api.md#22-cards).

Outcomes are posted after `done` and `failed` ([actions.ts:348](../src/actions.ts#L348)), after `rejected` and `expired` ([:273](../src/actions.ts#L273), [:311](../src/actions.ts#L311)), and at stop ([:292](../src/actions.ts#L292)). They are skipped after the control chat is lost ([:292](../src/actions.ts#L292), [:348](../src/actions.ts#L348), [:358](../src/actions.ts#L358)).

### 5.5 Standing approvals

| Aspect | Messages | Files | Code |
|---|---|---|---|
| grant | `/approve CHAT all` from an approver in mode `requests`; pending sends of CHAT with a card are approved | `/receive CHAT N all` from an approver, on the card of download N; N is performed, and the other pending downloads of CHAT stay pending | [control.ts:287](../src/control.ts#L287), [:293-304](../src/control.ts#L293-L304) |
| mode `host` | nothing stored; reply `Approvals apply in mode requests; this server runs in mode host.` | downloads get no card, so the command has none to act on | [control.ts:259](../src/control.ts#L259) |
| removal | `/revoke CHAT`: the `simplexMcp` key is removed, other keys kept | `/receive CHAT off`: the `simplexMcpFiles` key is removed | [control.ts:306-313](../src/control.ts#L306-L313) |
| use | mode `requests`, and `approvedById` equal to the `groupMemberId` of a current approver | the same | [control.ts:167-170](../src/control.ts#L167-L170) |
| scope | `send` | `download` of a file whose servers are all known | [tools.ts:922](../src/tools.ts#L922), [:1099](../src/tools.ts#L1099) |
| decider shown | `safeName(approvedBy)` | the same | [control.ts:169](../src/control.ts#L169) |
| storage | the chat's custom data; removed with the chat by `leave` | the same | [tools.ts:1074-1082](../src/tools.ts#L1074-L1082) |

Files of `reply` in a session group need no approval and have no standing approval; the group's preference for files and media controls them ([8.5](#85-approval-model)).

### 5.6 Expiry

| Rule | Code |
|---|---|
| `expires = created + expireHours × 3600000`, `expireHours` 1 to 23, default 23 | [actions.ts:190](../src/actions.ts#L190), [cli.ts:79](../src/cli.ts#L79), [:102-104](../src/cli.ts#L102-L104) |
| one unreferenced timer per action, set at its creation for `expires - now`, at most 60 s, and set again until the expiry time; all actions past `expires` are also expired before each owner command | [actions.ts:11](../src/actions.ts#L11), [:297-300](../src/actions.ts#L297-L300), [control.ts:247](../src/control.ts#L247) |
| pending and due: `expired`, log `Action N expired.`, card edited to `Expired.` | [actions.ts:302-312](../src/actions.ts#L302-L312) |
| decided, no open call, due: removed from memory; a pending action of `serve` that expires stays until the next owner command or its check 60 s later, as in 0.2 | [actions.ts:314-316](../src/actions.ts#L314-L316), [:307-309](../src/actions.ts#L307-L309), [:243-247](../src/actions.ts#L243-L247) |
| connect wait for the other side: at most until `created + 24 h - 10 min` | [tools.ts:153](../src/tools.ts#L153), [:814](../src/tools.ts#L814) |

A timer follows the monotonic clock and `expires` the wall clock: a timer that fires before `expires` is set again, so an action whose expiry time passes while the computer sleeps expires within 60 s after it wakes ([actions.ts:305](../src/actions.ts#L305)).

### 5.7 Action counter

| Rule | Code |
|---|---|
| initial value: `nextAction` of the control marker, or 1 | [serve.ts:96](../src/serve.ts#L96) |
| a new action takes `next`, then `next` grows by 1 | [actions.ts:181](../src/actions.ts#L181), [:206](../src/actions.ts#L206) |
| the new `next` is written before the card is posted | [actions.ts:320-321](../src/actions.ts#L320-L321) |
| one write at a time, also after a failed write | [control.ts:172-184](../src/control.ts#L172-L184) |
| `--reset-owners` copies the value into the new control chat | [serve.ts:180-181](../src/serve.ts#L180-L181), [control.ts:642-645](../src/control.ts#L642-L645) |
| a refused call leaves the counter unchanged | [actions.ts:205](../src/actions.ts#L205) |

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

- The token is created once and kept in `DATA/token`; `--new-token` replaces it ([files.ts:124-134](../src/files.ts#L124-L134)).
- `DATA/headers.json` is rewritten at every start ([files.ts:140-142](../src/files.ts#L140-L142)).
- The Claude Code entry gets the command `cat <DATA>/headers.json`, or `type "<DATA>\headers.json"` on Windows, with the path quoted for the shell ([register.ts:50](../src/register.ts#L50), [log.ts:24-31](../src/log.ts#L24-L31)). Only this command is written to the entry and to the `claude mcp add-json` arguments ([register.test.ts:50-61](../test/unit/register.test.ts#L50-L61)).
- `token`, `headers.json` and `server.json` are set to mode 0600 at every write, and the data folder to 0700 at every start, except on Windows ([files.ts:103-106](../src/files.ts#L103-L106), [:173-176](../src/files.ts#L173-L176)).

### 6.3 Peer-written text

| Destination | Rule | Code |
|---|---|---|
| card and reply names | folded to one line; a valid display name is plain, others a code span | [`safeName`](../src/control.ts#L827-L830) |
| card text lines | one code span per line; backticks become `'`; a leading or trailing space becomes U+00A0; blank lines empty | [`codeLines`, `codeSpan`](../src/control.ts#L819-L825) |
| file names and paths on cards and posts | folded to one line, one code span | [control.ts:717](../src/control.ts#L717), [:702](../src/control.ts#L702), [:730](../src/control.ts#L730), [:211-213](../src/control.ts#L211-L213) |
| reply quote on a send card | whitespace runs to one space, first 100 characters (code points), one code span | [control.ts:718](../src/control.ts#L718) |
| link texts on a connect card, the input preview on a tool-use card of the plugin | code span lines in the bytes that the largest card form leaves; the line that does not fit is cut and ends in `…` inside its code span; later lines left out | [control.ts:784-798](../src/control.ts#L784-L798) |
| mention names | valid name: `@name` or `@'name'`; others a code span | [`mentionText`](../src/control.ts#L832-L835) |
| errors on cards | folded to one line, one code span | [control.ts:692](../src/control.ts#L692) |
| `--owners` part of restart lines in the chat | one code span | [control.ts:91](../src/control.ts#L91), [:917-919](../src/control.ts#L917-L919) |
| terminal names and errors of actions | folded to one line | [actions.ts:209-210](../src/actions.ts#L209-L210), [:402-412](../src/actions.ts#L402-L412), [control.ts:90](../src/control.ts#L90) |
| tool results | returned as data; schema descriptions and instructions mark `text`, `from`, `name`, file names, downloaded files, `description`, `welcome` and `commands` with their labels and params as data, not instructions; the instructions take a chat's messages as prompts only when the user said so in the session, and limit what the agent shares about its user | [tools.ts:195](../src/tools.ts#L195), [:199](../src/tools.ts#L199), [:204](../src/tools.ts#L204), [:229-230](../src/tools.ts#L229-L230), [:240-245](../src/tools.ts#L240-L245), [:408](../src/tools.ts#L408), [:450](../src/tools.ts#L450) |

The server writes the tappable form `/'...'` only in the card's command line ([control.ts:735-750](../src/control.ts#L735-L750)), in `/'revoke CHAT'` of standing cards and replies ([:690](../src/control.ts#L690), [:303](../src/control.ts#L303)), and in `/'delete CHAT files'` of the kept-files message ([:354](../src/control.ts#L354)).

### 6.4 Chat references

| Rule | Code |
|---|---|
| `^[@#][1-9]\d{0,14}$`: 1 to 15 digits; the core reads `#(2^64 + N)` as `#N` | [messages.ts:150-152](../src/messages.ts#L150-L152), [scenarios.test.ts:491-503](../test/e2e/scenarios.test.ts#L491-L503) |
| the control chat is refused by `send`, `receive`, `download`, `leave` and the owner commands with CHAT, and left out of `list` and `/list` | [tools.ts:1239](../src/tools.ts#L1239), [:1284-1303](../src/tools.ts#L1284-L1303), [control.ts:408-415](../src/control.ts#L408-L415) |
| a store error of the chat read is reported as `No chat CHAT.` | [tools.ts:1244](../src/tools.ts#L1244), [:1301](../src/tools.ts#L1301) |
| the agent's own links are refused by `connect` (`ownLink` plans) | [tools.ts:1405](../src/tools.ts#L1405), [:1423](../src/tools.ts#L1423), [:1445](../src/tools.ts#L1445) |

### 6.5 Owner commands

| Rule | Code |
|---|---|
| source: `groupRcv` text items in the main scope of the control group | [serve.ts:129](../src/serve.ts#L129), [control.ts:218](../src/control.ts#L218), [:652-654](../src/control.ts#L652-L654) |
| form: one line `/keyword params`, leading spaces allowed | [control.ts:672-677](../src/control.ts#L672-L677) |
| a text outside the requests gets the help reply of `serve`, or `Unknown command.` in a plugin session group, from any sender; other commands are accepted from approvers only, and from another sender get a reply in `serve` and none in a plugin session group | [control.ts:249-250](../src/control.ts#L249-L250) |
| commands run one at a time in arrival order | [control.ts:222-225](../src/control.ts#L222-L225) |
| `/approve`, `/reject`, `/receive` and `/forward` need a pending action with a card, and the action's chat when CHAT is given; `/receive` and `/forward` take downloads only, `/approve` the other actions only | [control.ts:277-287](../src/control.ts#L277-L287) |

### 6.6 Files

| Rule | Code |
|---|---|
| `send` refuses files named `id_*`, `*.pem`, `*.key`, `*.p12` or `*.pfx`, and files under `~/.ssh`, `~/.gnupg`, the data folder and `~/.simplex-mcp` except the agent's own `DATA/files/`, each by its real path; the refusal is posted to the control chat and logged | [tools.ts:987-1001](../src/tools.ts#L987-L1001), [:1616-1635](../src/tools.ts#L1616-L1635), [control.ts:199-203](../src/control.ts#L199-L203) |
| the file of a `send` is sent only when its real path, size, inode and change time at the perform are those of the request | [tools.ts:1003-1008](../src/tools.ts#L1003-L1008) |
| a sent JPEG or PNG is a resized copy made by the core | [tools.ts:1010-1021](../src/tools.ts#L1010-L1021) |
| `ffmpeg` and `ffprobe` read a video as MP4 or QuickTime, local files only, without standard input, 30 s and 64 MiB of output each | [tools.ts:1685-1703](../src/tools.ts#L1685-L1703) |
| a download is saved only at `DATA/files/group-N/` or `DATA/files/contact-N/`, or for `/forward` at the same name under `DATA/forwarded/`, named by its file ID and an extension of 1 to 16 ASCII letters and digits | [messages.ts:215-227](../src/messages.ts#L215-L227) |
| a forwarded file is outside `DATA/files/`, so `send` refuses it; a download moves it into `DATA/files/` after the copy for the post is made | [tools.ts:587-604](../src/tools.ts#L587-L604), [:996-1001](../src/tools.ts#L996-L1001) |
| a file on unknown servers gets a card that names them, also under a files approval; relays are approved only for such a card, and for every other download, and in mode `host`, the core checks the complete description and downloads a file on unknown servers only when the IP address is protected | [tools.ts:1099](../src/tools.ts#L1099), [:610](../src/tools.ts#L610), [:1153](../src/tools.ts#L1153), [control.ts:745-748](../src/control.ts#L745-L748) |
| an accepted file that the core returns to `rcvInvitation` fails its download at the next check | [tools.ts:1166](../src/tools.ts#L1166) |
| a received file is moved into the chat's folder at its completion, unless it is at the forwarded path, also from a member chat of a group, which has a download in memory as the main conversation does; a file without a download in memory, such as one whose transfer started before a restart, only when it has its full size; a partial file is deleted only for a download in memory, when it is no larger than the file | [tools.ts:1205-1214](../src/tools.ts#L1205-L1214), [:1637-1643](../src/tools.ts#L1637-L1643) |

### 6.7 Link and network

- The join link is a group link of the control chat with role `member`, or `observer` with `--owners`; it is printed with a QR code on stdout and deleted at the first join event ([control.ts:486-496](../src/control.ts#L486-L496), [:452](../src/control.ts#L452), [:461](../src/control.ts#L461)).
- At every start the server sets private message routing: `smpProxyMode` `always`, `smpProxyFallback` `allowProtected` ([core.ts:38-39](../src/core.ts#L38-L39)).
- With `incognito: true` the join methods connect incognito ([tools.ts:848](../src/tools.ts#L848)); the core then uses a random profile for that chat ([PLAN, Participants](../PLAN.md#participants)).

---

## 7. Limits

| Limit | Value | Code |
|---|---|---|
| pending actions | 20 | [actions.ts:9](../src/actions.ts#L9) |
| actions being performed | 20 | [actions.ts:10](../src/actions.ts#L10) |
| expiry | 1 to 23 h, default 23 | [cli.ts:79](../src/cli.ts#L79), [:102-104](../src/cli.ts#L102-L104) |
| connect wait for the other side | `created + 23 h 50 min` | [tools.ts:153](../src/tools.ts#L153) |
| send, connect and download card | 15000 bytes JSON-escaped, in its largest form | [control.ts:31](../src/control.ts#L31), [:762-770](../src/control.ts#L762-L770) |
| error assumed in the largest card | 600 × `€` | [control.ts:35](../src/control.ts#L35) |
| message as the core sends it | 15602 bytes, counted with 600 bytes of envelope; `send` refuses a longer one, and the post of `/forward` leaves out its quote of the card | [tools.ts:163-164](../src/tools.ts#L163-L164), [:1575-1578](../src/tools.ts#L1575-L1578), [control.ts:315-334](../src/control.ts#L315-L334) |
| `send.text` | 1 to 12000 characters | [tools.ts:267](../src/tools.ts#L267) |
| `connect.message` | 1 to 2000 characters | [tools.ts:255](../src/tools.ts#L255) |
| `send.mention` | 1 to 3 unique member IDs | [tools.ts:271](../src/tools.ts#L271) |
| `wait` | 0 to 86400 s | [tools.ts:256](../src/tools.ts#L256), [:272](../src/tools.ts#L272), [:284](../src/tools.ts#L284), [:307](../src/tools.ts#L307) |
| `idle` | 1 to 300 s, default 30 | [tools.ts:257](../src/tools.ts#L257), [:273](../src/tools.ts#L273), [:159](../src/tools.ts#L159) |
| `limit` | 1 to 100, default 50 | [tools.ts:274](../src/tools.ts#L274), [:285](../src/tools.ts#L285), [:160](../src/tools.ts#L160) |
| `connect` snapshot | last 50 items | [tools.ts:161](../src/tools.ts#L161) |
| collected items per action or call | 100, then their IDs kept as overflow until deleted | [messages.ts:10](../src/messages.ts#L10), [:109-110](../src/messages.ts#L109-L110) |
| unread page | 100 items | [messages.ts:9](../src/messages.ts#L9) |
| chat list read | one read of 1000000 chats | [tools.ts:152](../src/tools.ts#L152) |
| `download` wait, default | 60 s | [tools.ts:156](../src/tools.ts#L156) |
| download check | every 60 s | [tools.ts:157](../src/tools.ts#L157) |
| download without progress | 30 min, then cancelled | [tools.ts:158](../src/tools.ts#L158) |
| `ffmpeg`, `ffprobe` | 30 s, 64 MiB of output | [tools.ts:162](../src/tools.ts#L162), [:166](../src/tools.ts#L166) |
| stop wait | 0 to 60 s, default 10 | [cli.ts:84](../src/cli.ts#L84), [:105-107](../src/cli.ts#L105-L107) |
| error details | 500 characters | [log.ts:4](../src/log.ts#L4) |
| progress interval | 60 s | [messages.ts:7](../src/messages.ts#L7) |
| expiry timer | at most 60 s, set again until the expiry time | [actions.ts:11](../src/actions.ts#L11), [:298](../src/actions.ts#L298) |
| HTTP body | 4 MiB | [server index.cjs:254](../node_modules/@modelcontextprotocol/server/dist/index.cjs#L254) |
| SSE keep-alive | 15 s | [mcp-BumlNux0.cjs:64](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L64) |
| agent NAME | 1 to 32 characters | [cli.ts:170](../src/cli.ts#L170) |
| display name | 50 characters, `mkValidName` rules | [messages.ts:11-12](../src/messages.ts#L11-L12), [:341-363](../src/messages.ts#L341-L363) |
| port | 1 to 65535, default 5226 | [cli.ts:82](../src/cli.ts#L82), [:114-116](../src/cli.ts#L114-L116) |

---

## 8. Claude Code plugin

The plugin runs the same tools, `Actions` and `Control` as `serve`, in other processes. ROOT is the plugin's data root, `~/.simplex-mcp` or `SIMPLEX_MCP_HOME` ([files.ts:31-34](../src/files.ts#L31-L34)).

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
    link["ServiceLink<br/>ServiceClient"]
  end
  subgraph pd["simplex-mcp service, one per ROOT"]
    sock["socket.ts listen<br/>bearer token"]
    dm["Service<br/>routes, entries, idle"]
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
  link -->|"HTTP over ROOT/service.sock<br/>Authorization: Bearer"| sock
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
  pd --> logf[("ROOT/service.log")]
```

Data flow of a tool call:

1. Claude Code calls a tool of the MCP server `simplex`; the SDK validates the arguments ([session.ts:57](../src/session.ts#L57), [tools.ts:498-508](../src/tools.ts#L498-L508)).
2. Without a connection, the session process checks the service's status and version, spawns it when it is not running, sends `POST /session` and holds `POST /attach` open ([session.ts:160-185](../src/session.ts#L160-L185), [client.ts:35-47](../src/client.ts#L35-L47)).
3. The session process sends `POST /call` with the session ID, the tool and the input, relays each `{progress}` line as an MCP progress notification, and returns the `{result}` line ([session.ts:132-151](../src/session.ts#L132-L151)).
4. The service checks the token, finds or opens the entry of the session or of its alias, and runs the session's `AgentTools.call` with a signal tied to the request ([socket.ts:18-48](../src/socket.ts#L18-L48), [service.ts:326-345](../src/service.ts#L326-L345)).
5. `AgentTools` sends chat commands to the core `_hub`; actions are created in the session's `Actions` and posted as cards in the session's group by its `Control` ([hub.ts:68-102](../src/hub.ts#L68-L102)).
6. The core emits events to `Hub.onEvent`, which passes them to the Main group, to each session's `AgentTools.onEvent` and to the `Control` of the session group they belong to ([hub.ts:482-512](../src/hub.ts#L482-L512)).

Data flow of a prompt from the session group:

1. A message of a member in the group of session N reaches `Session.onMessage`: a one-line `/` command goes to `Control`; another message is queued after the earlier ones, and `Session.prompt` drops a message of `User` and delivers the rest ([hub.ts:486-491](../src/hub.ts#L486-L491), [:152-159](../src/hub.ts#L152-L159), [:226-229](../src/hub.ts#L226-L229)).
2. In the first 30 s after the hub opened, a delivery without an attachment waits for one. With an attachment, its file, when it has one, is received. Then, without an attachment, the hub replies `Session N is not running.`; with one, the prompt is pushed, and the line `{prompt}` is written to every `/attach` stream of the session ([hub.ts:231-245](../src/hub.ts#L231-L245), [service.ts:314](../src/service.ts#L314)).
3. The session process sends it to Claude Code as `notifications/claude/channel` ([session.ts:53-56](../src/session.ts#L53-L56), [:197-201](../src/session.ts#L197-L201)).
4. The hooks post the terminal prompts and the final answer of each turn with `POST /turn` ([hook.ts:74-93](../src/hook.ts#L74-L93), [hub.ts:179-198](../src/hub.ts#L179-L198)).

### 8.2 Modules

#### [`session.ts`](../src/session.ts)

| Symbol | Line | Contents |
|---|---|---|
| `unattendedRun`, `channelInstructions`, `permissionRequest` | [15-23](../src/session.ts#L15-L23) | the text of a run that is not interactive; instructions of the channel; schema of a permission request |
| `startSession` | [40](../src/session.ts#L40) | session ID check, the text of a run that is not interactive, options, `McpServer` with the instructions and the capabilities `claude/channel` and `claude/channel/permission`, tools with `reply`, the permission request handler, stdio transport, background connect unless a text of the run or the options answers every call |
| `ServiceLink.connect`, `start`, `close`, `reconnect` | [79-112](../src/session.ts#L79-L112) | one shared connect promise, refused at once after a 403 of `/session`; `start` logs a failure on stderr and, unless it is a `ProtocolError` or that refusal, connects again later; `close` ends the attachment and the retries; `reconnect` sets one timer that starts again after the delay, 1 s, and doubles it when it fires, up to 30 s |
| `ServiceLink.permission` | [114](../src/session.ts#L114) | `POST /permission` after the connect; nothing after the refusal |
| `ServiceLink.call` | [120](../src/session.ts#L120) | forward; one repeat after status 409, which the service returns for an unknown session before it runs the call; failures to tool errors, the text of a run that is not interactive after the refusal |
| `ServiceLink.forward` | [132](../src/session.ts#L132) | `/call` stream: progress lines, the result line; a broken stream drops the connection |
| `ServiceLink.disconnect` | [153](../src/session.ts#L153) | drops the connection only when it is the current one |
| `ServiceLink.open` | [160](../src/session.ts#L160) | `ensureService`, `/session` with the parent process ID, a 403 of it recorded as the refusal, `/attach` until its first line; each line to the channel; the end of `/attach` to `reconnect` |
| `failureText` | [188](../src/session.ts#L188) | not running or status 503: `The SimpleX service stopped; repeat the call.` |
| `channelNotification` | [197](../src/session.ts#L197) | attach lines to `notifications/claude/channel` and `notifications/claude/channel/permission` |

#### [`client.ts`](../src/client.ts)

| Symbol | Line | Contents |
|---|---|---|
| `ServiceClient.forRoot` | [22](../src/client.ts#L22) | ROOT created, token, socket path |
| `ServiceClient.ensureService` | [35](../src/client.ts#L35) | status; a service of a lower version is stopped; a spawn when none runs; protocol check |
| `spawnService` | [71](../src/client.ts#L71) | detached child with stdout and stderr to `service.log`, a ready pipe on fd 3, and the environment variables of `serviceVariables` ([13](../src/client.ts#L13)) |
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
| `protocolVersion`, `unattendedStatus` | [5-6](../src/protocol.ts#L5-L6) | `1`; 403, the status of `/session` from a process marked unattended |
| `Owners`, `OwnerState` | [13-19](../src/protocol.ts#L13-L19) | the owners' names; with the Main group link, for `/title` and `/link` only |
| `SessionRequest`, `TitleRequest` | [21-37](../src/protocol.ts#L21-L37) | `/session`, `/title`, with `unattended` for the mark of a process |
| `PermissionRequest`, `TurnRequest` | [39-55](../src/protocol.ts#L39-L55) | `/permission`, `/turn` |
| `AttachLine` | [57](../src/protocol.ts#L57) | `{prompt}` and `{permission}` lines of `/attach` |
| `TitleReply`, `SessionReply`, `AttachRequest`, `CallRequest`, `CallLine` | [62-83](../src/protocol.ts#L62-L83) | replies and `/call` |
| `StatusReply`, `ResetReply` | [85-93](../src/protocol.ts#L85-L93) | `/status`, `/reset-owners` |

#### [`service.ts`](../src/service.ts)

| Symbol | Line | Contents |
|---|---|---|
| `HubLike`, `SessionLike`, `ServiceDeps` | [26-56](../src/service.ts#L26-L56) | interfaces the service uses; tests replace the hub |
| `Entry` | [58](../src/service.ts#L58) | opening promise, session, open attachments, their writers, running calls, idle timer, the session server's parent process ID and the time it was last seen |
| `Service.start` | [93](../src/service.ts#L93) | lock, stored sessions with their aliases and the next number, listener before the hub opens, so a starter that lost the lock race gets an answer to `/status` right after `running`, hub, stop timer, the titles held at the last stop, hourly log trim |
| `Service.routes`, `withHub`, `expiring` | [137-175](../src/service.ts#L137-L175) | the ten routes; requests wait for the hub; due actions expire first |
| `Service.shutdown` | [177](../src/service.ts#L177) | stop sequence; the held titles saved |
| `onSession`, `onTitle`, `markUnattended`, `refuseUnattended`, `applyTitle` | [201-262](../src/service.ts#L201-L262) | options checked; a title with `unattended` removes the marks of 10 s ago and older, marks its process, ends a server's wait for the title of that process and opens nothing; a registration from a process marked in the last 10 s refused with 403, before and after its wait; registration with the parent process ID, named by a held title of its ID, and its signal; a title of a new ID from a process without a known session, or from a process whose title is held, held until the server of that process registers, 10 s at most, and applied only while the service runs; a session opened or created at its title, its name and model, and the process of the title recorded |
| `canonical`, `resolve`, `attachedSession`, `processSession`, `alias` | [264-295](../src/service.ts#L264-L295) | the session of an ID: its alias; for an ID with `session.json`, the ID, or for a hook the attached session of its Claude Code process, without an alias; else the session of the same Claude Code process, recorded as an alias in `session.json`; the wait of a session server for the title of its process |
| `rename` | [297](../src/service.ts#L297) | a new name of the session, written into `session.json` as read after the group's rename |
| `onAttach`, `onCall`, `hold` | [304-356](../src/service.ts#L304-L356) | attachment with the session's line writer; call stream; streams refused while stopping |
| `entry`, `refuseWhenStopping`, `folder`, `openAgent` | [357-402](../src/service.ts#L357-L402) | session registry; status 503 while stopping, 400 for an ID that is not a session ID, 409 for an unknown one without a request; a new number and name, and the aliases that already point to the new session |
| `scheduleIdle`, `scheduleStop` | [404-422](../src/service.ts#L404-L422) | idle close of a session, idle stop of the service |
| `runService` | [430](../src/service.ts#L430) | process entry: ROOT, start, ready line, signal handlers; for a held lock, a new start once it is released, `running` when its holder answers |
| `productionDeps` | [456](../src/service.ts#L456) | token, `Hub.open` |
| `processEvent` | [466](../src/service.ts#L466) | the next event of a PID on `titled` or `registered`, 10 s at most |
| `lockHolder` | [478](../src/service.ts#L478) | up to 150 checks, 100 ms apart: `released` when ROOT/service.lock is gone, `answers` for any reply of the socket but ENOENT and ECONNREFUSED |
| `sessionName` | [493](../src/service.ts#L493) | session title, else the project folder name, else `Claude Code` |
| `validConfig` | [487](../src/service.ts#L487) | `config` of `/session` and `/title`: a valid Main group name, the mode, hours from 1 to 23; else status 400 |
| `readyChannel` | [499](../src/service.ts#L499) | ready line to fd `SIMPLEX_MCP_READY_FD`, or stdout |

#### [`hub.ts`](../src/hub.ts)

| Symbol | Line | Contents |
|---|---|---|
| constants | [18-28](../src/hub.ts#L18-L28) | profile name `Claude`, session data key, first action number, 10 s stop wait, 60 s echo of a raw prompt, 20 pushed prompts, 10 minutes for a prompt's file, 1 day for a pending connection, 30 s of held prompts after the hub opens, the channel tags of this plugin, the one reply to the `/` commands of a member who is not an admin or owner |
| `hubTexts` | [30](../src/hub.ts#L30) | refusal texts of a lost session group, of a stop and of the limit of pending actions, which names the approvers |
| `SessionGroupData` | [37-41](../src/hub.ts#L37-L41) | session number; pending connections of `connect` with their time; the approved mode |
| `Session` | [43](../src/hub.ts#L43) | one Claude Code session from its `session.json` and its group's data: `Actions` with the hub's texts and the mode of the group's data, `requests` without one, `AgentTools` with the session's ID and folder and the group for `reply`, `Control` of the session group with the session's approver checks, the stored model and effort |
| `Session.busy` | [118](../src/hub.ts#L118) | an action other than a `mode` card in memory |
| `Session.requestMode`, `askMode` | [131-150](../src/hub.ts#L131-L150) | `requests` at once, pending `mode` cards ended, `hostDeclined` cleared; `host` remembered and asked as a `mode` action while the group has an approver and `hostDeclined` is unset; a rejected or expired card forgets it, a rejected one sets `hostDeclined` |
| `Session.onLine` | [109-116](../src/hub.ts#L109-L116) | the writer of the session's attachments; a change wakes the held prompts |
| `Session.onMessage` | [152-159](../src/hub.ts#L152-L159) | the one refusal of a `/` command from a member who is not an approver, by `refusesCommand`, on the `delivery` chain; one-line `/` texts to `Control`; other messages to `prompt` on the `delivery` chain, in arrival order |
| `Session.claimPending` | [161](../src/hub.ts#L161) | a connection of `connect` recorded in the last day that completed: its record removed, its chat claimed |
| `Session.permission` | [168](../src/hub.ts#L168) | a `permission` action in mode `requests`; its verdict to the attachments; removal when decided |
| `Session.turn` | [179](../src/hub.ts#L179) | before any await: at a prompt or an answer, the end of the permissions created before the hook started; the quote of an answer and the latest answer's hook start; the turn record of a prompt whose hook started after it; a changed description and its record in `session.json`; then terminal prompts posted by `User` unless blank, made only of this plugin's channel tags, pushed or a command, the answer, and, also after a failed post, the short description; a prompt or an answer that the core refuses for a group preference is replaced by Claude's post that names the preference |
| `Session.endPermissions`, `fileDone` | [200-211](../src/hub.ts#L200-L211) | the end of pending permissions created before a time; the end of a prompt's file |
| `Session.invite`, `announceRights` | [213-224](../src/hub.ts#L213-L224) | invitation through the Main group; at a member's join or role change other than `User`'s, the post of the rights of the member's role, with the `/observer` command for a role that sends |
| `Session.prompt` | [226-229](../src/hub.ts#L226-L229) | a message of `User` dropped; the rest delivered |
| `Session.deliver`, `receive`, `turnQuote`, `answer`, `postRefusal`, `post`, `pushedIn` | [231-292](../src/hub.ts#L231-L292) | after the held prompts' wait, the file received while attached; then the prompt line with the file's path, or without it and a reply after a failure, or `Session N is not running.` without an attachment; the file received into the session's folder within 10 minutes; the newest turn record before an answer, else the oldest pushed prompt not quoted, marked quoted; the answer in parts, with its quote when the quote fits with the answer's first character; in place of a post the core refuses with `feature not allowed NAME`, the subject and the text that names the preference, by `featureOff` for an answer and by `notAllowedForUser` for a prompt of `User`; a post to the group, sent again without its quote when the core refuses it; the pushed prompts a turn's prompt contains |
| `Session.queue`, `refusesCommand` | [294-300](../src/hub.ts#L294-L300) | the `delivery` chain; a one-line `/` text of a member other than `User` who is not an approver |
| `Session.approves`, `isUser`, `setMode`, `declineHost`, `store`, `savePending` | [302-330](../src/hub.ts#L302-L330) | approver check: not `User`, the role `admin` or `owner` in the session group; the member of the contact `Hub.userContactId`; the mode and its record in the group's custom data; `hostDeclined` and its record; fields written into `session.json`; a pending connection recorded, those older than a day dropped |
| `Hub.userContactId` | [341](../src/hub.ts#L341) | the hub's contact marked `simplexMcpUser`, read at the start |
| `Hub.open` | [358](../src/hub.ts#L358) | core `_hub`, Main group found or created, `MainGroup` load and start, `User` unless it fails, the contact of `User`, the stored sessions, events |
| `Hub.configure`, `resetOwners` | [387-395](../src/hub.ts#L387-L395) | expiry of all sessions, Main group name; the Main group replaced at once, also while owners exist |
| `Hub.session` | [397](../src/hub.ts#L397) | the open session renamed, or a new `Session` with its stored or new group; the owners invited to a new group |
| `Hub.sessionsText`, `join`, `inviteOwner` | [421-443](../src/hub.ts#L421-L443) | `SessionGroups` of the Main group |
| `Hub.stop` | [449](../src/hub.ts#L449) | actions stopped, events awaited, cancelled cards awaited up to 10 s, cores closed |
| `storedGroup`, `createSessionGroup` | [461-480](../src/hub.ts#L461-L480) | the group of `session.json` with a matching session number; a new group with `User` added |
| `Hub.onEvent` | [482](../src/hub.ts#L482) | event dispatch; pending connections claimed at `contactConnected` and `userJoinedGroup`; a session group's membership events to its `Control`, `announceRights` and `askMode`, one at a time |
| `notAllowedForUser` | [514](../src/hub.ts#L514) | `NAME are not allowed for User in the group of this session; its owners can allow them in the group's preferences.`, for a prompt of `User` that a group preference refuses, also one on only for roles above that of `User` |
| `textParts` | [522](../src/hub.ts#L522) | the longest runs of whole characters that each fit one message by `fitsMessage`, the first with the given quote; a binary search per part among the next `maxMessageLength` characters |
| `isRefusedQuote` | [541](../src/hub.ts#L541) | `errorStore` `invalidQuote`, `chatItemNotFound` or `largeMsg` |

#### [`main-group.ts`](../src/main-group.ts)

| Symbol | Line | Contents |
|---|---|---|
| `mainCommands`, texts | [8-18](../src/main-group.ts#L8-L18) | command menu; greeting, reply to an unknown command, log line; the welcome message, the text of the join post and the short description |
| `SessionGroups` | [20](../src/main-group.ts#L20) | what the Main group asks of the hub |
| `MainGroup.owners` | [53-55](../src/main-group.ts#L53-L55) | human members with role `owner` while the group is not lost |
| `MainGroup.load`, `start` | [57-76](../src/main-group.ts#L57-L76) | lost check, name and menu from the group's current profile, its short description and welcome message kept, members; without owners, the existing link, or a new one |
| `MainGroup.onMessage`, `command` | [83-115](../src/main-group.ts#L83-L115) | `/sessions`, `/join N`; `Unknown command.` for another |
| `MainGroup.onEvent`, `handleEvent`, `ownerJoined` | [95-140](../src/main-group.ts#L95-L140) | the members read again; the first human who joins becomes an owner; a new link when no owner remains; the statement of trust at each human's join; new owners invited to the open sessions |
| `MainGroup.replace` | [99-109](../src/main-group.ts#L99-L109) | the Main group deleted for all its members, a new one and a new link; the session groups kept |
| `openLink`, `deleteLink` | [147-160](../src/main-group.ts#L147-L160) | group link with role `member` |
| `findMainGroup`, `createMainGroup` | [185-205](../src/main-group.ts#L185-L205) | the newest group with `simplexMcpMain`; a new one with the short description and welcome message of trust, history, direct messages for owners and the menu |

#### [`user.ts`](../src/user.ts)

| Symbol | Line | Contents |
|---|---|---|
| `User.open` | [16](../src/user.ts#L16) | core `_user` with profile `User`, events, a connection to `Claude` when the hub has no contact marked `simplexMcpUser` |
| `User.post` | [29](../src/user.ts#L29) | messages to the group marked with the session number |
| `User.connect` | [38](../src/user.ts#L38) | a one-time invitation of the hub, joined by `User`; the `contactConnected` of that invitation's connection awaited 60 s; the hub's contact marked |
| `User.onEvent` | [50](../src/user.ts#L50) | the contact marked `simplexMcpClaude`; invitations of that contact to a group named `N · ...` joined and marked |
| `userContact` | [60](../src/user.ts#L60) | the hub's contact marked `simplexMcpUser` |

#### [`core.ts`](../src/core.ts)

| Symbol | Line | Contents |
|---|---|---|
| `openCore` | [19](../src/core.ts#L19) | folder, `ChatApi.init` with prefix `FOLDER/simplex`, active user or a new one with peer type `agent`, `prepareCore`, event buffer, `startChat`, preset contact deleted for a new user, profile update |
| `deletePresetContacts` | [44](../src/core.ts#L44) | the contact `Ask SimpleX Team`, deleted when it has no connection |

#### [`hook.ts`](../src/hook.ts), [`owner.ts`](../src/owner.ts), [`qr.ts`](../src/qr.ts)

| Symbol | Line | Contents |
|---|---|---|
| `sessionStart` | [hook.ts:41](../src/hook.ts#L41) | `{}` for a run that is not interactive, after the mark of its process when only `CLAUDE_CODE_SESSION_ATTENDED` makes it so; Node.js check, options, `ensureService`, `POST /title` with the title, the folder and the model, the link message |
| `markUnattended` | [hook.ts:62](../src/hook.ts#L62) | with `CLAUDE_PID`: `ensureService`, `POST /title` with `unattended`; a failure logged |
| `turn` | [hook.ts:74](../src/hook.ts#L74) | `{}` for a run that is not interactive; `ensureService`, then `POST /turn` with `CLAUDE_PID`, the hook process's start time `performance.timeOrigin`, the prompt and the last answer, each cut to 250,000 characters, the new model and the effort level, repeated after status 409 up to 15 times 1 s apart; output `{}` |
| `linkCommand`, `resetCommand` | [owner.ts:7-8](../src/owner.ts#L7-L8) | `npx -y @simplex-chat/mcp@VERSION link` and `reset-owners`, VERSION of this package |
| `linkText`, `resetOwnersText` | [owner.ts:10-20](../src/owner.ts#L10-L20) | the Main group's name with the owners, or with the link and a QR code on a terminal only |
| `showLink`, `resetOwners` | [owner.ts:22-33](../src/owner.ts#L22-L33) | `link`, with the link from `GET /link` while the Main group has no owner; `reset-owners`, the Main group replaced at once, the new link shown on a terminal only |
| `qrCodeText` | [qr.ts:14](../src/qr.ts#L14) | `small` and `large` with ANSI colors for terminals, `plain` for the hook message |

#### [`config.ts`](../src/config.ts), [`files.ts`](../src/files.ts)

| Symbol | Line | Contents |
|---|---|---|
| `interactiveEntrypoint`, `interactive` | [config.ts:26-32](../src/config.ts#L26-L32) | `false` for `CLAUDE_CODE_ENTRYPOINT` `sdk-cli`, `sdk-ts` or `sdk-py`; `interactive` also `false` for `CLAUDE_CODE_SESSION_ATTENDED` `0` |
| `Config`, `defaultConfig`, `configVariables` | [config.ts:4-18](../src/config.ts#L4-L18) | the Main group name, the mode and the expiry hours; their defaults `SimpleX MCP`, `requests` and 23; their variables |
| `readConfig` | [config.ts:34](../src/config.ts#L34) | `CLAUDE_PLUGIN_OPTION_*` to `Config`, defaults for blank values, `ConfigError` texts |
| `dataRoot`, `hubFolder`, `sessionFolder` | [files.ts:31-47](../src/files.ts#L31-L47) | ROOT, `ROOT/_hub`, `ROOT/_sessions/<lower-case ID>` |
| `SessionFile`, `readSessionFile`, `writeSessionFile`, `storedSessions` | [files.ts:11-72](../src/files.ts#L11-L72) | `session.json`: number, name, project, creation time, group, aliases, model, effort, a rejected mode `host`; an unparsable file is skipped with a log line; a write goes to `session.json.tmp`, renamed into place |
| `socketPath` | [files.ts:74](../src/files.ts#L74) | `ROOT/service.sock`; over 103 bytes `TMP/simplex-mcp-HASH.sock`; Windows `\\.\pipe\simplex-mcp-HASH` |
| `logFile`, `trimLog`, `openLog` | [files.ts:81-101](../src/files.ts#L81-L101) | `ROOT/service.log`, emptied when over 10 MB, mode 0600 |
| `serviceLock`, `serviceToken` | [files.ts:85-89](../src/files.ts#L85-L89) | `service.lock` and `ROOT/service.token`, the plugin's lock and token: names with a dot, which agent names of `serve` can't take, so that the folders of agents in `~/.simplex-mcp` are not touched |

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

Memory of the service:

| Item | Holder | Written by | Read by |
|---|---|---|---|
| Entries of open sessions | `Service.entries` ([service.ts:70](../src/service.ts#L70)) | `entry`, idle close, stop | routes, `status` |
| Line writers of the attachments | `Entry.writers` ([service.ts:63](../src/service.ts#L63)) | `onAttach` | `Session.onLine` |
| Stored sessions, next number | `Service.stored`, `nextNumber` ([service.ts:72-79](../src/service.ts#L72-L79)) | `Service` constructor, `openAgent` | `openHub`, `openAgent` |
| Aliases of sessions | `Service.aliases`, ID to session ID ([service.ts:73](../src/service.ts#L73)) | `Service` constructor, `alias` | `canonical`, `resolve` |
| Titles by process | `Service.titles`, PID to session ID, time and the name of a held title, and the `titled` signal ([service.ts:74-75](../src/service.ts#L74-L75)) | `onTitle`, `applyTitle` | `processSession`, the wait of `resolve`, `onSession` |
| Registrations by process | `Service.registered`, the signal of a `/session` with that PID ([service.ts:76](../src/service.ts#L76)) | `onSession` | the wait of a held title |
| Held titles | `Service.held`, PID to its last held `/title` request ([service.ts:77](../src/service.ts#L77)) | `onTitle`, the end of its wait | `onTitle`, `shutdown` |
| Processes of unattended runs | `Service.unattended`, PID to the time of its mark ([service.ts:78](../src/service.ts#L78)) | `onTitle` with `unattended` | `onSession` |
| Process of a session server | `Entry.pid`, `Entry.seen` ([service.ts:65-66](../src/service.ts#L65-L66)) | `onSession`, the end of the last attachment | `processSession` |
| Sessions | `Hub.sessions`, by number ([hub.ts:343](../src/hub.ts#L343)) | `Hub.session` | events, Main group commands |
| End of the held prompts | `Hub.promptsHeldUntil`, 30 s after the stored sessions opened ([hub.ts:342](../src/hub.ts#L342), [:369](../src/hub.ts#L369)) | `Hub.open` | `Session.deliver` |
| Prompts in delivery | `Session.delivery`, a promise chain in arrival order ([hub.ts:59](../src/hub.ts#L59)) | `onMessage` | `deliver` |
| Actions of a session | `Session.actions`, numbered from 1 per session group ([hub.ts:45](../src/hub.ts#L45), [:74](../src/hub.ts#L74)) | tools, `permission`, `requestMode` | `Control`, `busy` |
| Pushed prompts | `Session.pushed`, at most 20, each until a turn's prompt matches it; `quoted` once an answer without a turn record quoted it ([hub.ts:50](../src/hub.ts#L50)) | `deliver`, `turnQuote` | `turn`, `turnQuote` |
| Files of prompts in transfer | `Session.receiving` ([hub.ts:51](../src/hub.ts#L51)) | `receive`, its 10-minute timer | `fileDone` |
| Model and effort | `Session.description`, also in `session.json` ([hub.ts:52](../src/hub.ts#L52)) | constructor, `turn` | `turn` |
| Turn records | `Session.turns`: the start time of each prompt hook that started after the latest answer's hook, with its pushed prompt, `null` for a terminal prompt or a command ([hub.ts:53-54](../src/hub.ts#L53-L54)) | `turn`; an answer removes those that started before its hook | `turnQuote` |
| A rejected mode `host` | `Session.hostDeclined`, also in `session.json` ([hub.ts:56](../src/hub.ts#L56)) | constructor, `declineHost` | `askMode`, `requestMode` |
| The requested mode | `Session.requested` ([hub.ts:55](../src/hub.ts#L55)) | `requestMode`, a rejected or expired `mode` card | `askMode` |
| Members of the Main group, its link | `MainGroup` ([main-group.ts:28-31](../src/main-group.ts#L28-L31)) | `refreshMembers`, `openLink` | owner checks, the statement of trust at a join, `/status`, `/title`, `/link` |

Memory of a session process: one connection promise, one attachment, the refusal of the service, and the retry delay with its timer ([session.ts:69-77](../src/session.ts#L69-L77)).

Chat database custom data, all in the core `_hub` unless noted:

| Item | Entity | Value | Written by | Read by |
|---|---|---|---|---|
| Main group marker | Main group | `{simplexMcpMain: true}` | [`createMainGroup`](../src/main-group.ts#L195-L205) | [`findMainGroup`](../src/main-group.ts#L185-L188) |
| Session group | group of session N | `{simplexMcpSession: {session: N, pending?: [{connId, at}], mode?}}`, `mode` the approved mode | [`createSessionGroup`](../src/hub.ts#L468-L480), [`setMode`](../src/hub.ts#L310-L313), [`savePending`](../src/hub.ts#L326-L330), [`claimPending`](../src/hub.ts#L161-L166) | [`storedGroup`](../src/hub.ts#L461-L466), [`Session` constructor](../src/hub.ts#L74), [`claimPending`](../src/hub.ts#L161-L166) |
| Action counter | group of session N | `{simplexMcp: {control: true, nextAction}}`, merged with the other keys | [`saveCounter`, `saveData`](../src/control.ts#L172-L184) | [`Session` constructor](../src/hub.ts#L74) |
| Chat owner | contact or group of a peer | `{...other keys, simplexMcpOwner: {session: ID}}` | [`claim`](../src/tools.ts#L581-L585), [`giveChat`](../src/tools.ts#L568-L571) | [`owns`](../src/tools.ts#L577-L579) |
| Standing approvals | contact or group of a peer | as in [3.2](#32-chat-database-custom-data) | `Control` of the owning session's group | the same |
| User contact | the hub's contact of `User` | `{simplexMcpUser: true}` | [`User.connect`](../src/user.ts#L38-L48) | [`userContact`](../src/user.ts#L60-L62) |
| Claude contact | in `_user`: the contact of `Claude` | `{simplexMcpClaude: true}` | [`User.onEvent`](../src/user.ts#L51) | [`User.onEvent`](../src/user.ts#L52) |
| Session of a group | in `_user`: a session group | `{simplexMcpSession: N}` | [`User.onEvent`](../src/user.ts#L56) | [`User.post`](../src/user.ts#L29) |

Files:

| Path | Contents | Written | Read |
|---|---|---|---|
| `ROOT/` | data root, mode 0700 | every client and the service ([client.ts:23](../src/client.ts#L23), [service.ts:435](../src/service.ts#L435)) | |
| `ROOT/service.token` | the bearer token, 0600 | the first client or service ([files.ts:87-89](../src/files.ts#L87-L89), [:124-134](../src/files.ts#L124-L134)) | every client, the service |
| `ROOT/service.lock` | PID of the service, linked from `ROOT/service.lock.PID.HEX.tmp`; a stopped service's lock is claimed through `ROOT/service.lock.PID.claim`, removed after the takeover | `Service.start` ([service.ts:94](../src/service.ts#L94), [files.ts:182-221](../src/files.ts#L182-L221)) | the next service |
| `ROOT/titles.json` | the `/title` requests held at a stop, 0600 | `shutdown` ([service.ts:180](../src/service.ts#L180)) | the next `Service.start`, which removes it ([service.ts:106](../src/service.ts#L106)) |
| `ROOT/service.sock` | the socket | `listen` ([socket.ts:40-43](../src/socket.ts#L40-L43)) | clients |
| `ROOT/service.log` | the service's stdout and stderr, emptied over 10 MB at a spawn and every hour | `spawnService` ([client.ts:71-79](../src/client.ts#L71-L79)), [`trimLog`](../src/files.ts#L91-L94) | people |
| `ROOT/_hub/`, `ROOT/_user/` | databases and `tmp/` of the profiles `Claude` and `User` | [`openCore`](../src/core.ts#L19-L35) | the cores |
| `ROOT/_sessions/ID/session.json` | `{number, name, project, created, group?, aliases?, model?, effort?, hostDeclined?}`, 0600, replaced whole through `session.json.tmp` | `openAgent`, `rename`, `alias`, `Hub.session`, `declineHost`, `store` ([service.ts:392-402](../src/service.ts#L392-L402), [:297-302](../src/service.ts#L297-L302), [:290-295](../src/service.ts#L290-L295), [hub.ts:407](../src/hub.ts#L407), [:315-324](../src/hub.ts#L315-L324)) | `Service` constructor, `resolve`, `Hub.session`, `alias`, `store` |
| `ROOT/_sessions/ID/files/` | downloads of the session's chats as in [3.3](#33-files); `group/` with the files of the prompts from the group; `sent/`, deleted when the session opens | the core ([hub.ts:247-255](../src/hub.ts#L247-L255), [:404](../src/hub.ts#L404)) | tools, Claude |

### 8.4 Process lifecycle

#### Session start

1. The SessionStart hook checks Node.js 22, reads the options, starts the service when it is not running, and sends `POST /title` with the session ID, `CLAUDE_PID`, title, project folder, model and options ([hook.ts:41-60](../src/hook.ts#L41-L60)). In a run that is not interactive, it returns `{}`; when only `CLAUDE_CODE_SESSION_ATTENDED` `0` makes the run so, it first sends `POST /title` with `unattended`, and `/session` from that `CLAUDE_PID` gets 403 for 10 s ([flow 30](flows.md#30-a-run-that-is-not-interactive)).
2. `/title` resolves the ID to a session as in [API 4.6](api.md#46-socket-protocol), so that `/clear`, `/resume` and `--continue` keep the session of the running Claude Code process, with an alias only for an ID without `session.json`; a new ID from a process without a known session waits, after the reply, for the server of that process to register, up to 10 s; then `/title` opens the session: from `session.json`, or with a new number and the name from the title or the folder; `Hub.session` finds the group of `session.json` or creates one, adds `User`, invites the Main group's owners, and loads its `Control` ([service.ts:218-262](../src/service.ts#L218-L262), [:392-402](../src/service.ts#L392-L402), [hub.ts:397-415](../src/hub.ts#L397-L415)).
3. A new title renames the group and `session.json`; the model is written to the group's short description ([service.ts:260-261](../src/service.ts#L260-L261)).
4. The hook prints the link message while the Main group has no owner, else `{}`.
5. The session process connects in the background: `/session` resolves its ID, waiting up to 10 s for the title of its process when the ID is new, registers the options and its parent process, and requests the mode; `/attach` sets the session's line writer ([service.ts:201-216](../src/service.ts#L201-L216), [:304-324](../src/service.ts#L304-L324)). When `/attach` ends or the connection fails, except with a `ProtocolError` or a 403 of `/session`, the session process connects again ([session.ts:88-112](../src/session.ts#L88-L112)).

#### Service spawn and version check

```
ensureService(VERSION):                               client.ts:32
  GET /status
    not running: none
    version below VERSION: POST /stop; none
  none: spawn "node dist/cli.js service", wait for its ready line
    ready, running: GET /status
    error: TEXT: ExitError TEXT
    exit without a line: ExitError "The SimpleX service exited; see ROOT/service.log"
    5 minutes: ExitError "The SimpleX service did not start in 5 minutes."
  protocol other than 1: ProtocolError "Restart Claude Code to update the SimpleX plugin."
```

#### Service start

1. `ROOT` is created, the options are read, and the lock is taken. A lock held by another live process is checked for up to 15 s: when it is released, the start runs again; when the socket answers, the ready line is `running`; otherwise `error: Another service uses ROOT (PID N). If no service runs, remove ROOT/service.lock.` and exit code 1 ([service.ts:430-442](../src/service.ts#L430-L442), [:478-485](../src/service.ts#L478-L485), [:93-96](../src/service.ts#L93-L96)).
2. The stored sessions are read, with their aliases, and the next number set ([service.ts:87-91](../src/service.ts#L87-L91)).
3. The socket listens before the hub opens; requests wait for the hub ([service.ts:97-98](../src/service.ts#L97-L98), [:163-168](../src/service.ts#L163-L168)).
4. `Hub.open`: core `_hub` with profile `Claude`; the Main group found by its marker or created; `MainGroup.load` and `start`, with a link while it has no owner; core `_user` and its connection to `Claude` at the first start; a `Session` for each stored session; prompts held for 30 s; events ([hub.ts:358-376](../src/hub.ts#L358-L376), [service.ts:126-129](../src/service.ts#L126-L129)).
5. A failed hub open closes the listener and releases the lock; the requests that waited get status 500 ([service.ts:99-118](../src/service.ts#L99-L118)).
6. The stop timer and the hourly log trim start; the titles held at the last stop are read from `ROOT/titles.json` and taken as new `/title` requests; the log line `SimpleX MCP service VERSION, data folder ROOT`; the ready line `ready` ([service.ts:105-113](../src/service.ts#L105-L113), [:444-445](../src/service.ts#L444-L445)).

#### Event dispatch

`Hub.onEvent` ([hub.ts:482-512](../src/hub.ts#L482-L512)), after a stop: nothing; otherwise `expireDue` of every session first:

| Event | Condition | Handler |
|---|---|---|
| `newChatItems` | item of the Main group | [`MainGroup.onMessage`](../src/main-group.ts#L83) |
| `newChatItems` | item of a session group | [`Session.onMessage`](../src/hub.ts#L152) |
| `rcvFileComplete`, `rcvFileError` | | [`Session.fileDone`](../src/hub.ts#L206) of every session |
| `contactConnected`, `userJoinedGroup` | a connection ID | [`Session.claimPending`](../src/hub.ts#L161) of every session |
| any event | | [`AgentTools.onEvent`](../src/tools.ts#L665) of every session |
| `sndFileError` | a chat of the session, or its group | [`Control.fileNotSent`](../src/control.ts#L205) |
| membership events | of the Main group | [`MainGroup.onEvent`](../src/main-group.ts#L95), queued in the Main group, not awaited by the event loop; the statement of trust at a join ([flow 28](flows.md#28-a-member-joins-the-main-group-and-a-sessions-group-and-a-person-is-added-to-it)) |
| membership events | of a session group | [`Control.onEvent`](../src/control.ts#L228), then [`Session.announceRights`](../src/hub.ts#L217) and [`Session.askMode`](../src/hub.ts#L139), queued |
#### Attach, idle close and idle stop

- An attachment cancels the session's idle timer; its end, when no other attachment remains, records the time, clears the line writer, ends the pending permissions and starts the timer ([service.ts:304-324](../src/service.ts#L304-L324)).
- The idle timer, 10 minutes, closes the entry when the session has no running call and no action in memory; otherwise it starts again ([service.ts:404-414](../src/service.ts#L404-L414)). A closed session keeps its `Session` in the hub, so its group answers `/list` and `/revoke` ([hub.ts:397-402](../src/hub.ts#L397-L402)).
- When no entry remains, the stop timer, 10 minutes, stops the service; a new entry clears it ([service.ts:416-422](../src/service.ts#L416-L422), [:362](../src/service.ts#L362)).

#### Stop

On `SIGINT`, `SIGTERM`, `POST /stop` or the stop timer ([service.ts:177-199](../src/service.ts#L177-L199)):

1. Timers are cleared, the held titles are saved in `ROOT/titles.json`, the listener closes, and open streams are destroyed.
2. `Hub.stop`: the actions of every session are stopped, cards edited to `Cancelled: the server stopped.`; queued group events are awaited; cancelled cards get 10 s to be sent; the download checks stop; the cores `_user` and `_hub` close ([hub.ts:449-459](../src/hub.ts#L449-L459)).
3. `Cancelled K action(s).`, the lock is released, `Stopped.`; the process exits after the `/stop` reply is sent, or after its client disconnects before it is written.

### 8.5 Approval model

| Rule | Code |
|---|---|
| Rights in a session group follow the member's role there, whatever way they joined. An approver is an active member other than Claude and `User` with the role `admin` or `owner`; approvals and tool-use decisions count only from approvers | [hub.ts:302-308](../src/hub.ts#L302-L308), [control.ts:43](../src/control.ts#L43), [:580-582](../src/control.ts#L580-L582), [:589-598](../src/control.ts#L589-L598) |
| Prompts count from every member but `User`; the group's `/` commands come only from approvers; from the other members each one-line `/` text gets one reply | [hub.ts:152-159](../src/hub.ts#L152-L159), [:226-229](../src/hub.ts#L226-L229), [:298-304](../src/hub.ts#L298-L304) |
| `/join N` invites a member of the Main group as `admin`, an owner of the Main group as `owner`; the owners invited at a session start and at an owner's join are `owner`s | [hub.ts:430](../src/hub.ts#L430), [:412](../src/hub.ts#L412), [:441](../src/hub.ts#L441) |
| `/observer #G ID` sets the role `observer` of a current or pending member from a sender whose role is admin or owner and not below the member's role, as SimpleX requires; an observer neither prompts nor approves, and its standing approvals count no more from the reply on, while that member is not an admin or owner; a member invited and not joined yet is no member | [control.ts:375-387](../src/control.ts#L375-L387), [:880-882](../src/control.ts#L880-L882), [:167-170](../src/control.ts#L167-L170) |
| A new action of a session is refused while its group has no approver | [control.ts:156-157](../src/control.ts#L156-L157) |
| Changes of membership or role in the Main group, and its replacement by a reset of the owners, leave the session groups as they are | [main-group.ts:117-132](../src/main-group.ts#L117-L132), [:99-109](../src/main-group.ts#L99-L109) |
| The Main group states that everyone in it is trusted with all sessions, in its short description and welcome message, set when it is created, and a post at each join; a session group gets a post of the member's rights at each join and each role change, with the `/observer` command for a role that sends, and the reply to `/observer` states the observer's rights | [main-group.ts:16-18](../src/main-group.ts#L16-L18), [:195-205](../src/main-group.ts#L195-L205), [:127-128](../src/main-group.ts#L127-L128), [hub.ts:217-224](../src/hub.ts#L217-L224), [control.ts:386](../src/control.ts#L386), [:888-891](../src/control.ts#L888-L891) |
| A file of `reply` is sent at once, in both modes; the group's preference for files and media, which owners set and the core enforces, can refuse it; a post of Claude or `User` in a session group that a group preference refuses is replaced by Claude's post that names the preference, never dropped without one ([tools.ts:1607-1614](../src/tools.ts#L1607-L1614)); Claude's profile updates of a group start from its current profile in the chat database and change only Claude's own fields, so they keep the owners' preferences | [tools.ts:713-730](../src/tools.ts#L713-L730), [hub.ts:92](../src/hub.ts#L92), [control.ts:115-140](../src/control.ts#L115-L140), [:632-640](../src/control.ts#L632-L640), [main-group.ts:57-66](../src/main-group.ts#L57-L66) |
| The first human who joins the Main group while it has no owner becomes an owner, and the link is deleted | [main-group.ts:122](../src/main-group.ts#L122), [:134-140](../src/main-group.ts#L134-L140) |
| A session's mode `host` takes effect after an approver approves its `mode` card, posted once the group has an approver; after a rejection, the card is posted again only after `requests`; `requests` at once, ending a pending `mode` card; the mode is kept in the session group's custom data | [hub.ts:131-150](../src/hub.ts#L131-L150), [:315-318](../src/hub.ts#L315-L318), [:509](../src/hub.ts#L509), [tools.ts:563-564](../src/tools.ts#L563-L564) |
| A reset of the owners from the computer that runs Claude Code replaces the Main group at once, also while owners exist: `reset-owners`, `/simplex:reset-owners` and `POST /reset-owners` need no owner's approval | [hub.ts:393-395](../src/hub.ts#L393-L395), [owner.ts:28-33](../src/owner.ts#L28-L33), [main-group.ts:99-109](../src/main-group.ts#L99-L109) |
| A chat without an owner gets the session that connects it: at the perform, at the connection event of its action, at the connection of a pending connection the session recorded, or at a `connect` to it as a known chat; tools of other sessions don't show it; `/give CHAT N` moves it | [tools.ts:577-585](../src/tools.ts#L577-L585), [:750-755](../src/tools.ts#L750-L755), [:826](../src/tools.ts#L826), [:835](../src/tools.ts#L835), [:893](../src/tools.ts#L893), [hub.ts:161-166](../src/hub.ts#L161-L166), [:495-496](../src/hub.ts#L495-L496), [control.ts:362-373](../src/control.ts#L362-L373) |

Tool-use state machine, one action of tool `permission` per request:

| Event | From | To | Code |
|---|---|---|---|
| `permission_request` | | `pending`, card posted | [hub.ts:168-169](../src/hub.ts#L168-L169) |
| `/approve N` | `pending` | `performing`, `allow` pushed, then `done` | [hub.ts:171-172](../src/hub.ts#L171-L172), [tools.ts:553-566](../src/tools.ts#L553-L566) |
| `/reject N` | `pending` | `rejected`, `deny` pushed | [hub.ts:171-172](../src/hub.ts#L171-L172) |
| a prompt or an answer of a hook that started after the card's creation, end of the last attachment | `pending` | `expired`, card edited to `Ended.` | [hub.ts:180](../src/hub.ts#L180), [:200-204](../src/hub.ts#L200-L204), [service.ts:321](../src/service.ts#L321), [actions.ts:237-241](../src/actions.ts#L237-L241) |
| expiry | `pending` | `expired`, card edited to `Expired.` | [actions.ts:302-312](../src/actions.ts#L302-L312) |
| `done`, `rejected` or `expired` | | removed from the session's actions | [hub.ts:173-175](../src/hub.ts#L173-L175) |

```mermaid
stateDiagram-v2
  [*] --> pending: permission_request
  pending --> performing: /approve N, allow pushed
  performing --> done
  pending --> rejected: /reject N, deny pushed
  pending --> expired: later prompt or answer, detach: Ended.#59; expiry: Expired.
  done --> [*]: removed
  rejected --> [*]: removed
  expired --> [*]: removed
```

### 8.6 Channel and mirroring

| Direction | Message | Code |
|---|---|---|
| service to session | `{prompt: {text, from, item, file?}}`, `{permission: {requestId, behavior}}` on `/attach` | [protocol.ts:57-60](../src/protocol.ts#L57-L60) |
| session to Claude Code | `notifications/claude/channel` `{content, meta: {from, item, file?}}`; `notifications/claude/channel/permission` `{request_id, behavior}` | [session.ts:197-201](../src/session.ts#L197-L201) |
| Claude Code to session | `notifications/claude/channel/permission_request` `{request_id, tool_name, description, input_preview}` | [session.ts:58-62](../src/session.ts#L58-L62) |
| session to service | `POST /permission` | [session.ts:114-118](../src/session.ts#L114-L118) |
| hooks to service | `POST /turn` `{id, pid?, at?, prompt?, answer?, model?, effort?}`, `at` the hook's `performance.timeOrigin`, `prompt` and `answer` cut to 250,000 characters | [hook.ts:74-97](../src/hook.ts#L74-L97) |

```
on a message m in N:               one-line "/" text from a member who is neither an admin, an owner nor User:
                                   delivery = delivery then reply "Only admins and owners of this group can send / commands."
                                   one-line "/" text: a command, not a prompt
                                   else: delivery = delivery then prompt(m), in arrival order
prompt(m):                         from User: nothing
                                   else: deliver(m)
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
                                   User posts parts(p) in the group marked N
on /turn answer a, not blank:      Claude posts parts(a), the first quoting quoted when the quote fits with the first character of a
then, also after a failed post, a changed value: short description "MODEL · effort LEVEL"
post with a quote:                 the core refuses it with invalidQuote, chatItemNotFound or largeMsg: the post again without the quote
```

```mermaid
sequenceDiagram
  participant S as Stop hook of turn N
  participant U as UserPromptSubmit hook of turn N+1
  participant D as service
  Note over S,U: S starts before U#59; at = performance.timeOrigin of each
  U->>D: /turn {at: 3, prompt}: turns += (3, prompt N+1)
  S->>D: /turn {at: 2, answer}: quotes the newest turn record before 2, prompt N
  Note over D: a later answer with at 4 quotes prompt N+1
```

Sources: [hub.ts:152-204](../src/hub.ts#L152-L204), [:226-292](../src/hub.ts#L226-L292), [:522-539](../src/hub.ts#L522-L539).

### 8.7 Security boundaries

| Rule | Code |
|---|---|
| The socket is in ROOT, mode 0700, or a named pipe on Windows; every request needs the token, compared with `timingSafeEqual` | [socket.ts:19-25](../src/socket.ts#L19-L25), [files.ts:74-79](../src/files.ts#L74-L79) |
| The Main group link is shown only in the hook's `systemMessage` and by `link` on a terminal; tool results, refusals and other output name the command instead; of the socket's routes, only `/title` and `/link` return it | [hook.ts:54-56](../src/hook.ts#L54-L56), [owner.ts:7-15](../src/owner.ts#L7-L15), [control.ts:39](../src/control.ts#L39), [service.ts:154](../src/service.ts#L154), [:215](../src/service.ts#L215), [:426](../src/service.ts#L426) |
| `reset-owners` shows the new link only on a terminal | [owner.ts:17-20](../src/owner.ts#L17-L20), [:28-33](../src/owner.ts#L28-L33) |
| The plugin runs its own `dist/cli.js`, and Node.js loads its modules from the plugin's folder, so the `node_modules` of a project can't replace them | [.mcp.json:5](../.mcp.json#L5), [hooks.json:3-7](../hooks/hooks.json#L3-L7) |
| The service gets only the environment variables of `serviceVariables` | [client.ts:13](../src/client.ts#L13), [:78](../src/client.ts#L78) |
| `send` and `reply` refuse the files of ROOT except `files/` of the session's own folder | [tools.ts:996-1001](../src/tools.ts#L996-L1001), [hub.ts:89-90](../src/hub.ts#L89-L90) |
| `User`'s messages are neither prompts nor commands: `Session.prompt` and `Control.onMessage` drop the messages of the member of the hub's contact marked `simplexMcpUser`, read at the hub's start; `User` joins only groups its `Claude` contact invites it to | [hub.ts:227](../src/hub.ts#L227), [:306-308](../src/hub.ts#L306-L308), [:367](../src/hub.ts#L367), [control.ts:221](../src/control.ts#L221), [user.ts:50-57](../src/user.ts#L50-L57), [:60-62](../src/user.ts#L60-L62) |
| The hub's own groups and the `User` contact have no owner key, so the tools of every session leave them out | [tools.ts:577-579](../src/tools.ts#L577-L579), [:1284-1292](../src/tools.ts#L1284-L1292) |
| The approved mode is kept in the chat database, not in a file of ROOT | [hub.ts:74](../src/hub.ts#L74), [:310-313](../src/hub.ts#L310-L313), [files.ts:11-21](../src/files.ts#L11-L21) |
| These rules hold against Claude's tools, not against a shell running as the same user: such a shell reads ROOT, with the token and the chat databases, and sends requests to the service | |

### 8.8 Limits

| Limit | Value | Code |
|---|---|---|
| socket request body | 1 MiB | [socket.ts:6](../src/socket.ts#L6) |
| idle close of a session, idle stop of the service | 10 min each | [service.ts:17-18](../src/service.ts#L17-L18) |
| service start wait | 5 min | [client.ts:10](../src/client.ts#L10) |
| `User` connection at the first start | 60 s | [user.ts:11](../src/user.ts#L11) |
| echo of a raw prompt | 60 s | [hub.ts:22](../src/hub.ts#L22) |
| pushed prompts of a session | 20 | [hub.ts:23](../src/hub.ts#L23) |
| file of a prompt | 10 min | [hub.ts:24](../src/hub.ts#L24) |
| prompts held for an attachment after the hub opens | 30 s | [hub.ts:26](../src/hub.ts#L26) |
| record of a pending connection | 1 day | [hub.ts:25](../src/hub.ts#L25) |
| wait of a session server for the title of its process; a detached session server's process kept; the mark of an unattended run's process, removed at a later mark | 10 s | [service.ts:19](../src/service.ts#L19), [:237-243](../src/service.ts#L237-L243) |
| attempts of a turn the service refuses with status 409 | 15, 1 s apart | [hook.ts:12-14](../src/hook.ts#L12-L14) |
| attachment retries of a session server | 1 s, doubled up to 30 s | [session.ts:19-20](../src/session.ts#L19-L20) |
| part of an answer or a terminal prompt | one message by `fitsMessage`: 15602 bytes with the 600-byte envelope; 14962 bytes of text without a quote | [hub.ts:522-539](../src/hub.ts#L522-L539), [tools.ts:163-164](../src/tools.ts#L163-L164), [:1575-1578](../src/tools.ts#L1575-L1578) |
| cancelled cards wait at stop | 10 s | [hub.ts:21](../src/hub.ts#L21) |
| `service.log` | emptied over 10 MB, at a spawn and every hour | [files.ts:9](../src/files.ts#L9), [:91-94](../src/files.ts#L91-L94), [service.ts:20](../src/service.ts#L20) |
| socket path | 103 bytes: `sun_path` of `sockaddr_un` holds 104 bytes on macOS and 108 on Linux, with the terminating NUL | [files.ts:8](../src/files.ts#L8) |
| actions per session | 20 pending, 20 performing | [actions.ts:9-10](../src/actions.ts#L9-L10), [:199-203](../src/actions.ts#L199-L203) |
