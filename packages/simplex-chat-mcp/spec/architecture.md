# SimpleX MCP server: architecture

> System structure, modules, state, process lifecycle, approval model and security boundaries of `@simplex-chat/mcp` 0.3.0. `serve` is described in sections 1-7, and the Claude Code plugin in section 8.
>
> Related specs: [README](README.md) | [Flows](flows.md) | [API](api.md)
> Related docs: [README](../README.md) | [Reference](../REFERENCE.md) | [PLAN](../../../plans/2026-10-19-mcp-server.md)

**Source:** [`cli.ts`](../src/cli.ts) | [`serve.ts`](../src/serve.ts) | [`http.ts`](../src/http.ts) | [`tools.ts`](../src/tools.ts) | [`actions.ts`](../src/actions.ts) | [`control.ts`](../src/control.ts) | [`messages.ts`](../src/messages.ts) | [`files.ts`](../src/files.ts) | [`register.ts`](../src/register.ts) | [`log.ts`](../src/log.ts) | [`core.ts`](../src/core.ts) | [`schedule.ts`](../src/schedule.ts)
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
    schedule["schedule.ts<br/>Schedules"]
    messages["messages.ts<br/>ChatItemsWatch"]
    serve["serve.ts<br/>start, events, stop"]
    files["files.ts"]
    log["log.ts"]
    lib["simplex-chat<br/>ChatApi"]
    core["SimpleX core<br/>libsimplex core-7.1.3.0"]
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
  serve --> schedule
  control --> schedule
  tools --> schedule
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
3. An `McpServer` is built from the factory for each request, and the arguments are validated against the input schema by the SDK ([`serve.ts:109-113`](../src/serve.ts#L109-L113), [`mcp-BumlNux0.cjs:1812-1817`](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L1812-L1817)).
4. The handler result is returned by `AgentTools.call` as `structuredContent` and as JSON text ([`tools.ts:552-559`](../src/tools.ts#L552-L559)).
5. The server calls `ChatApi` methods of the core ([`messages.ts:271-287`](../src/messages.ts#L271-L287)); actions are created in `Actions` and shown to the owner by `Control` ([`actions.ts:180-215`](../src/actions.ts#L180-L215)).
6. The core emits events. The handler of `serve.ts` passes them to the agent control chat and to `AgentTools.onEvent` ([`serve.ts:128-140`](../src/serve.ts#L128-L140)). `AgentTools.onEvent` then dispatches them to the tools and the item watch ([`tools.ts:673-700`](../src/tools.ts#L673-L700)).

---

## 2. Modules

### [2.1 `cli.ts`](../src/cli.ts)

Entry point of the `simplex-mcp` binary.

| Symbol | Line | Contents |
|---|---|---|
| `helpText` | [14](../src/cli.ts#L14) | `--help` output |
| `pluginCommands` | [55](../src/cli.ts#L55) | `session`, `service`, `hook session-start`, `hook turn`, `link`, `reset-owners` |
| `Command` | [57](../src/cli.ts#L57) | `help`, `serve` with `ServeOptions`, `register` with `RegisterOptions`, the plugin commands |
| `parseCommand` | [63](../src/cli.ts#L63) | command, NAME and options to a `Command`, or the plugin command equal to the joined arguments |
| `serveOptions` | [74](../src/cli.ts#L74) | defaults and option switch of `serve` |
| `registerOptions` | [130](../src/cli.ts#L130) | defaults and option switch of `register` |
| `OptionReader` | [147](../src/cli.ts#L147) | `value`: the next argument, required<br>`optionalValue`: the next argument, absent when missing or starting with `--` |
| `validAgentName` | [169](../src/cli.ts#L169) | `^[a-z0-9](-?[a-z0-9])*-?$`, at most 32 characters |
| `usageError` | [214](../src/cli.ts#L214) | `ExitError` with code 2 |
| `main` | [218](../src/cli.ts#L218) | `help`, `serve`, `register`, `session`, `service`, the hooks, `link` and `reset-owners`. `help` prints to stdout. The hooks read stdin and print their JSON output. `link` and `reset-owners` print their text. |
| top-level catch | [253-262](../src/cli.ts#L253-L262) | `ExitError`: message on stderr, its code; other errors: `logError`, code 1 |

### [2.2 `serve.ts`](../src/serve.ts)

Start sequence, event dispatch and stop sequence of one agent.

| Symbol | Line | Contents |
|---|---|---|
| `OwnerEntry` | [19](../src/serve.ts#L19) | `{memberId, name}` of `--owners` |
| `ServeOptions` | [24](../src/serve.ts#L24) | name, data folder and its `dataGiven` flag, display name, mode, owners, expiry, reset and token flags, port, QR code size, stop wait |
| `RunningServer` | [39](../src/serve.ts#L39) | token, `ChatApi`, user, `Actions`, `stop` |
| `serve` | [47](../src/serve.ts#L47) | `startServer`, then SIGINT and SIGTERM handlers: the first signal stops, a second ends the stop wait |
| `startServer` | [65](../src/serve.ts#L65) | data folder, lock, token, headers file, `ChatApi.init`, `startAgent`; cleanup on error |
| `startAgent` | [88](../src/serve.ts#L88) | user, `prepareCore`, control group, wiring of `Actions`, `Schedules`, `AgentTools`, `Control`, HTTP, schedule timer, event handler, `stop` |
| `registerHint` | [161](../src/serve.ts#L161) | the `register` command line printed after the owner setup |
| `waitForCancelledCards` | [165](../src/serve.ts#L165) | the fixed wait at stop for cards edited or posted at the stop, used by the plugin's hub as well |
| `controlGroup` | [181](../src/serve.ts#L181) | found, created or reset agent control chat |
| `listen` | [188](../src/serve.ts#L188) | `startHttp`; `EADDRINUSE` to `ExitError` code 1 |

These symbols of [`core.ts`](../src/core.ts) and [`version.ts`](../src/version.ts) are shared with the plugin:

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
| `ConnectInput`, `SendInput`, `ReceiveInput`, `LeaveInput`, `DownloadInput` | [47-81](../src/tools.ts#L47-L81) | tool inputs |
| `PlanSummary` | [92](../src/tools.ts#L92) | card kind, chat type, name, SimpleX name, peer type, description, welcome, commands, `takesMessage` |
| `ChatFields` | [104](../src/tools.ts#L104) | chat fields of `connect` and `list` results |
| `Agent` | [118](../src/tools.ts#L118) | `ChatApi`, user, `Actions`, `ChatItemsWatch`, control chat reference, standing approver lookup, refused file post, data folder, `Schedules`. The plugin adds the data root, the owning session ID, the group of `reply`, the mode setter and the record of a pending connection. |
| `JoinMethod` | [135](../src/tools.ts#L135) | `prepared`, `prepareChannel`, `viaAddress`, `prepareContact`, `connect` |
| schemas | [191-419](../src/tools.ts#L191-L419) | input and output JSON schemas |
| descriptions, `instructions` | [421-460](../src/tools.ts#L421-L460) | tool descriptions and server instructions. `instructions` names the host at whose restart actions are cancelled, `server` by default. |
| `replyTool` | [470](../src/tools.ts#L470) | the plugin's seventh tool, `reply` |
| `definitions` | [493](../src/tools.ts#L493) | name, description, schemas and action flag of the six tools |
| `registerTools` | [506](../src/tools.ts#L506) | `registerTool` for each definition and each extra one, with `_meta` on action tools in mode `host` |
| `Download` | [518](../src/tools.ts#L518) | one download: its promise, chat, item, file ID, size, path, the last fileSource seen, received bytes and their time, check timer |
| `AgentTools` | [535](../src/tools.ts#L535) | constructor with the `Agent`, the mode or a function that returns it, and the session number of the plugin. The mode is read at each use of `mode`. |
| `AgentTools.register` | [548](../src/tools.ts#L548) | `registerTools` with `AgentTools.call` |
| `AgentTools.call` | [552](../src/tools.ts#L552) | handler result to `CallToolResult`; errors to `isError` |
| `AgentTools.perform` | [561](../src/tools.ts#L561) | perform step of an approved action, which is the mode setter for a `mode` action |
| `giveChat`, `owns`, `claim` | [576-594](../src/tools.ts#L576-L594) | the plugin's chat owners. A chat is the session's when no session ID is set or the chat's owner is that ID. `claim` sets the owner of a chat without one. It is called at a connection, and when `connect` names a known chat ([`ownChat`](../src/tools.ts#L758-L763)). The hub calls it when a pending connection completes. |
| `onEvent` | [673](../src/tools.ts#L673) | events to the file handlers, connect completions and the item watch, for `serve` and the plugin's sessions |
| `reply`, `sendFile` | [721-738](../src/tools.ts#L721-L738) | `reply` sends a file to the group of the plugin's session at once, without an action, under the file rules of `send`. Its message with the caption and the preview is checked by `fitsMessage`. The core's refusal `feature not allowed NAME` is answered with the text with the preference's name. `sendFile` posts the file. |
| `refusedFeature`, `featureOff` | [1620-1627](../src/tools.ts#L1620-L1627) | the preference NAME of a command error `feature not allowed NAME`, and the text `NAME are off in the group of this session; its owners can turn them on in the group's preferences.` The text is used for `reply`, the cards, `/forward` and the answers of a session group. |
| `downloadFile`, `forwardFile`, `downloadTo` | [595-622](../src/tools.ts#L595-L622) | one download per file ID, shared by `download` and `/forward`; a forwarded file moved to the download path for `download` |
| file events | [624-661](../src/tools.ts#L624-L661) | `onNewItems`, `onFile`, `onFileComplete`, `onFileError`, `onFileCancelled`, `onFileDescription` |
| `onContactConnected`, `onUserJoinedGroup` | [663-671](../src/tools.ts#L663-L671) | completion of connect actions by events |
| `actionResult` | [769](../src/tools.ts#L769) | wait for the decision, result fields, removal |
| `attachment`, `isRefused`, `fileToSend`, `resizedCopy` | [993-1029](../src/tools.ts#L993-L1029) | the file of a `send` |
| `copyForSending`, `chatFiles`, `deleteChatFiles` | [1031-1060](../src/tools.ts#L1031-L1060) | `/forward` copies; the files of a chat, forwarded ones included; its downloads cancelled, also those the core continues after a restart |
| `leaveChat` | [1082](../src/tools.ts#L1082) | perform step of `leave` and `/leave`; the chat's forwarded files deleted, its schedules dropped |
| `download`, `fileServers`, `unknownServers` | [1093-1138](../src/tools.ts#L1093-L1138) | the `download` call before its action |
| `startDownload`, `checkDownload`, `currentFile`, `settleFile`, `settleReceived`, `moveReceived` | [1156-1223](../src/tools.ts#L1156-L1223) | a download's steps |
| `agentChats`, `knownChat` | [1297-1316](../src/tools.ts#L1297-L1316) | the agent's chats; a checked chat reference |
| `classifyPlan` | [1399](../src/tools.ts#L1399) | `apiConnectPlan` response to a join plan or a known chat |
| `chatSubject`, `isConnected`, `isActive`, `memberCurrent`, `memberCurrentOrPending` | [1495-1564](../src/tools.ts#L1495-L1564) | chat and member predicates shared with `control.ts` and `hub.ts` |
| `customDataOf`, `setChatData`, `ownerOf` | [1528-1543](../src/tools.ts#L1528-L1543) | a chat's custom data; the plugin's owner key `simplexMcpOwner` |
| `tooLongText`, `fitsMessage`, `quoteContent` | [1584-1604](../src/tools.ts#L1584-L1604) | the refusal text of `send` and `reply` for a message that is too long. `fitsMessage` checks that the message as the core sends it, with its quote, is within `maxMessageLength`, 15602 bytes ([165](../src/tools.ts#L165)). The check is used for `send`, `reply`, the post of `/forward`, and the parts and quotes of the plugin's posts. |
| `realFile` | [1629](../src/tools.ts#L1629) | real path, size, inode and change time of a file |
| `attachmentMedia`, `videoFrame`, `videoDuration` | [1676-1710](../src/tools.ts#L1676-L1710) | previews and durations |
| `toolCall` | [1726](../src/tools.ts#L1726) | `ServerContext` to `ToolCall`: abort signal and progress notifications |

### [2.5 `actions.ts`](../src/actions.ts)

| Symbol | Line | Contents |
|---|---|---|
| `Mode` | [5](../src/actions.ts#L5) | `requests`, `host` |
| `State` | [6](../src/actions.ts#L6) | `pending`, `performing`, `done`, `failed`, `rejected`, `expired` |
| `Standing` | [7](../src/actions.ts#L7) | `messages`, `files` |
| `ToolError` | [18](../src/actions.ts#L18) | errors the model can correct; text returned as is |
| `Subject` | [26](../src/actions.ts#L26) | kind, name, SimpleX name, incognito flag of a card |
| `ConnRef` | [33](../src/actions.ts#L33) | pending connection, contact or group of a connect action |
| `HostTexts` | [20](../src/actions.ts#L20) | refusal texts of a lost control chat, of a stop and of the limit of pending actions. `serve` uses `lostMessage`, `stoppingMessage` and `pendingMessage`, where the owner is named. The plugin uses the hub's texts. |
| `ActionRecord` | [39](../src/actions.ts#L39) | number, session number of the plugin, mode, key, times, change signal, standing flag, state, card, decider, open calls, interrupted flag, ended flag, error, card image |
| `ConnectAction`, `SendAction`, `LeaveAction`, `DownloadAction` | [59-118](../src/actions.ts#L59-L118) | per-tool fields; `Attachment`, the file of a send |
| `ModeAction`, `PermissionAction` | [120-133](../src/actions.ts#L120-L133) | the plugin's actions: a switch to mode `host`, and a tool use of Claude Code with the bytes left for its input preview |
| `ActionHooks` | [139](../src/actions.ts#L139) | `refusal`, `saveCounter`, `postCard`, `perform`, `outcome` |
| `Actions` | [147](../src/actions.ts#L147) | action map, one expiry timer per action, posting and outcome promises |
| `create` | [180](../src/actions.ts#L180) | state by the action's mode, refusal checks, number, log line, counter write, card |
| `ofSession`, `remove`, `openCall`, `closeCall`, `end` | [217-241](../src/actions.ts#L217-L241) | the actions of one session; removal; open call count; an expiry before the expiry time, marked ended |
| `expireDue` | [243](../src/actions.ts#L243) | expiry of every action past its expiry time |
| `decided` | [249](../src/actions.ts#L249) | wait with progress until a decided state, `until`, or the abort signal |
| `stop` | [284](../src/actions.ts#L284) | cancellation of pending and performing actions |
| `scheduleExpiry`, `expire`, `forgetExpired` | [297-316](../src/actions.ts#L297-L316) | the expiry timer of one action, at most 60 s. `expire` expires the action, or sets a new timer when the timer, on the monotonic clock, fires before the expiry time on the wall clock. `forgetExpired` removes an action once it is decided, without open calls and past its expiry time. An action of `serve` that expires is kept longer, see [5.6](#56-expiry). |
| `summary`, `performedText`, `progressText`, `sentence` | [382-429](../src/actions.ts#L382-L429) | log and progress texts. `sentence` ends a log line with a period. For an action of a session, the line ends with one period, even after an error that ends with one. For an action of `serve`, every error is followed by a period, as in 0.2. |

Hooks are bound in [`serve.ts:97-103`](../src/serve.ts#L97-L103): `refusal`, `saveCounter`, `postCard` and `outcome` to `Control`; `perform` to `AgentTools`.

### [2.6 `control.ts`](../src/control.ts)

| Symbol | Line | Contents |
|---|---|---|
| `qrCodeText` | [12](../src/control.ts#L12) | the function of `qr.ts` ([qr.ts:14](../src/qr.ts#L14)), exported from `control.ts` as in 0.2 |
| `controlCommands` | [14](../src/control.ts#L14) | the command menu |
| `standings` | [31](../src/control.ts#L31) | custom data key and reply words of the approvals of messages and files |
| `helpTexts` | [98](../src/control.ts#L98) | the `/help` text of each topic: the commands for an empty topic, how to use a command for its keyword |
| `ControlData` | [115](../src/control.ts#L115) | `{control: true, nextAction}` |
| `ChatApproval` | [139](../src/control.ts#L139) | `{approvedBy, approvedById, approvedAt}` |
| `ControlOptions`, `SessionOptions` | [120-132](../src/control.ts#L120-L132) | display name, `--owners`, register hint and QR code size. For a group of the plugin's session, its approver check, the session IDs by number and the check of `User` are added. |
| `OwnerRequest` | [145](../src/control.ts#L145) | parsed owner command |
| `Control.load` | [188](../src/control.ts#L188) | lost check, group profile and menu through `updateProfile`, member list, and schedules of the agent's chats |
| `Control.rename`, `Control.describe`, `updateProfile` | [198-214](../src/control.ts#L198-L214) | the plugin's group name, and its short description. `updateProfile` makes one profile update at a time, and keeps the custom data in memory. A session group is updated from its current profile in the chat database. The agent control chat of `serve` is updated from the profile held in memory since the start ([209](../src/control.ts#L209)). |
| `Control.start` | [216](../src/control.ts#L216) | owner setup per `--owners` |
| `Control.refusal` | [224](../src/control.ts#L224) | bytes left for the link texts of a connect card and for the input preview of a tool-use card, card size of a send, connect or download, approver presence |
| `Control.hasApprover` | [237](../src/control.ts#L237) | an approver is a member |
| `Control.standingApprover` | [241](../src/control.ts#L241) | approver name of a chat's standing approval of messages or files |
| `Control.isCurrentApprover` | [246](../src/control.ts#L246) | whether a member ID is a current approver and the control chat is not lost; for standing approvals and schedules |
| `Control.saveCounter`, `Control.saveData` | [250-262](../src/control.ts#L250-L262) | queued writes of the counter and of other keys of the group's custom data |
| `Control.postCard`, `Control.outcome` | [264-275](../src/control.ts#L264-L275) | card post and card edit; the kept-files message after a done leave |
| `Control.refusedFile`, `Control.fileNotSent` | [277-292](../src/control.ts#L277-L292) | posts about files |
| `Control.onMessage` | [294](../src/control.ts#L294) | owner commands, queued one at a time. In a group of the plugin's session, the session passes it the one-line `/` texts, and the texts of `User` are dropped. |
| `Control.onEvent` | [306](../src/control.ts#L306) | membership changes and loss of the control chat |
| `Control.ownerCommand` | [324](../src/control.ts#L324) | command dispatch and replies. A text outside the requests is answered with the command list, or with `Unknown command.` in a group of the plugin's session. |
| `approveChat`, `revoke`, `addSchedule`, `removeSchedule`, `forward`, `leave`, `deleteFiles` | [378-473](../src/control.ts#L378-L473) | owner requests |
| `schedulesText` | [521](../src/control.ts#L521) | the `/schedules` reply |
| `give`, `setObserver` | [475-500](../src/control.ts#L475-L500) | `/give CHAT N` and `/observer #G ID` in a group of the plugin's session. `setObserver` replies `NAME's role can't be changed.` for a member with status `pending_approval` or `pending_review` ([494-495](../src/control.ts#L494-L495)). For a member who is neither current nor pending, such as an invited member who has not joined, the reply is `No member ID in this group.` ([492](../src/control.ts#L492), [`memberCurrentOrPending`](../src/tools.ts#L1562-L1564)). Only a current member's role is set to `observer`, when the sender's role is admin or owner and not below the member's role. The members are then read again, and the observer's rights are the reply. |
| `isApprover` | [713-718](../src/control.ts#L713-L718) | the session's check in a group of the plugin's session; otherwise role `owner`, and the `--owners` list when given |
| `roleOrder`, `roleRank`, `canSend` | [51](../src/control.ts#L51), [1015-1021](../src/control.ts#L1015-L1021) | the roles of SimpleX from `relay` to `owner`, in the order of the core; a role above `observer` |
| `sessionApproverRoles`, `rightsText` | [47-50](../src/control.ts#L47-L50), [1023-1026](../src/control.ts#L1023-L1026) | `admin` and `owner`, the approvers of a session group. `rightsText` is `NAME is a ROLE and RIGHTS.`, with the rights of the approvers, of the other roles that send, or of an observer. |
| `postCardContent`, `sessionRefusal`, `postRefusal`, `refusedCardSubject` | [641-664](../src/control.ts#L641-L664), [876-880](../src/control.ts#L876-L880) | the post of a card. In a plugin session group, a card that the core refuses with `feature not allowed Files and media` is posted again as text, and its later edits are text. A card or an edit refused for another preference before the decision or at a stop is replaced by `Action N: ` and the text with the preference's name. An action that waits for approval then fails with that text. After the decision, the replacement is `Action N OUTCOME; its card was not posted: ` or `...; its card was not updated: ` with that text, where OUTCOME is `was performed`, `failed`, `was rejected`, `expired` or `ended`. The action is kept as it was. |
| `cardContent`, `fittedImage` | [666-686](../src/control.ts#L666-L686) | image and video cards with the file's preview |
| `findControlGroup`, `createControlChat`, `resetControlChat` | [725-765](../src/control.ts#L725-L765) | control group lookup, creation, reset |
| `createGroup`, `updateGroupProfile`, `changeGroupProfile`, `deleteGroup` | [746-770](../src/control.ts#L746-L770) | a group with its profile and custom data, for `serve` and the plugin's groups. A session group or the Main group is updated from its current profile in the chat database. The agent control chat of `serve` is updated from the profile held in memory since the start, as in 0.2. Each update is made only when the change returns a profile. A group is deleted for all members. |
| `isGroupMessage` | [772](../src/control.ts#L772) | an item of one group's main scope |
| `isControlEvent` | [776](../src/control.ts#L776) | membership events of the control group |
| `parseCommand` | [792-797](../src/control.ts#L792-L797) | one-line `/keyword params` |
| `cardText`, `requestLines`, `commandsLine` | [799-870](../src/control.ts#L799-L870) | card text per tool, state and mode |
| `largestCard`, `encodedLength` | [882-890](../src/control.ts#L882-L890) | card size check |
| `truncatedLine`, `fittedLines` | [892-918](../src/control.ts#L892-L918) | a line cut with `…`; the code lines that fit a byte budget, the last one cut |
| `serversLine`, `sizeText` | [924-933](../src/control.ts#L924-L933) | servers line and sizes of download cards |
| `codeSpan`, `safeName`, `mentionText` | [943-955](../src/control.ts#L943-L955) | quoting of peer text |
| `schedulesOf`, `utcOffset` | [966-975](../src/control.ts#L966-L975) | valid schedule entries of a chat's custom data; the server's UTC offset |
| `ownerRequest` | [977](../src/control.ts#L977) | command words to an `OwnerRequest`, with `observer #G ID` only for a group of the plugin's session |
| `losesControl`, `isLost` | [998-1013](../src/control.ts#L998-L1013) | loss of the control chat or of a plugin group |
| `joinedText` | [1040](../src/control.ts#L1040) | `NAME joined (member N)`, shared with the Main group |
| `entryText`, `restartText` | [1044-1054](../src/control.ts#L1044-L1054) | `--owners` entries in restart lines |

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
| `filePaths`, `storedName`, `chatFolder` | [215-227](../src/messages.ts#L215-L227) | `DATA/files/group-N`, `DATA/files/contact-N` or the same under `DATA/forwarded`, and the download path and the forwarded path of a received file in them. A stored file is named by its file ID and extension, and so are the files of the plugin's prompts. |
| `fileOffer`, `fileContent` | [229-254](../src/messages.ts#L229-L254) | file fields of an item; message content of a file |
| `getChat`, `receivingFiles`, `markRead` | [271-287](../src/messages.ts#L271-L287) | `apiGetChat` and `apiChatItemsRead` by chat reference; the IDs of a chat's files in status `rcvAccepted`, `rcvTransfer` or `rcvWarning`, paged |
| `unreadItems` | [289](../src/messages.ts#L289) | unread items of a chat after its `initial` read, paged |
| `reportProgress`, `nextChange`, `awaitResponses` | [305-335](../src/messages.ts#L305-L335) | waits |
| `validName`, `viewName` | [341-367](../src/messages.ts#L341-L367) | display name rules and `@name` form |

### [2.8 `files.ts`](../src/files.ts)

| Symbol | Line | Contents |
|---|---|---|
| `createDataFolder` | [103](../src/files.ts#L103) | `mkdir -p` mode 0700; `chmod 0700` except on Windows |
| `acquireLock`, `releaseLock` | [108-122](../src/files.ts#L108-L122) | `DATA/lock`, or the file of the given name, with the PID. The holder is named in the error by the `user` argument, `server` or `service`. |
| `serverToken` | [124](../src/files.ts#L124) | `DATA/token`, or the file of the given name. An existing file is read, and is made private when the user owns it. A new token is 32 random bytes in base64url, and its file is created as the lock is. |
| `saveHeldTitles`, `takeHeldTitles` | [153-167](../src/files.ts#L153-L167) | `ROOT/titles.json`, written at a service stop, and read and removed at the next start. An unparsable file is skipped with a log line. |
| `createLock`, `createExclusive` | [182-204](../src/files.ts#L182-L204) | A private temporary file with the text is linked into place, so that the file appears once and complete. Where the file system refuses the link, the file is written exclusively. |
| `takeOver` | [206](../src/files.ts#L206) | The lock of a stopped process is claimed first, so that only one starter takes it over. The claim is a link of the lock to `lock.PID.claim`, or an exclusive write of `lock.PID.claim` where links fail. When that PID is still in the lock, the lock is removed and created again. The claim is then removed. |
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

### [2.11 `schedule.ts`](../src/schedule.ts)

Scheduled prompts of the agent's chats.

| Symbol | Line | Contents |
|---|---|---|
| `cronRanges`, `cronItem` | [5-6](../src/schedule.ts#L5-L6) | value ranges of the 5 cron fields; the forms of a field's item |
| `ScheduleEntry` | [8](../src/schedule.ts#L8) | `{cron, prompt, by, byId, at}`: cron text, prompt, author's display name and member ID, ISO creation time |
| `Due` | [10](../src/schedule.ts#L10) | `{prompt, at}`: a due prompt and its due minute in UTC |
| `Schedules` | [12](../src/schedule.ts#L12) | entries per chat reference, each with its due prompt; wake listeners; timer; the approver check of `byId` |
| `set` | [19](../src/schedule.ts#L19) | a chat's entries replaced; the due prompt of an entry with the same creation time kept |
| `start`, `stop` | [24-30](../src/schedule.ts#L24-L30) | the first tick; the timer cleared |
| `listen` | [32](../src/schedule.ts#L32) | a wake callback for one chat, and its removal |
| `take`, `pending` | [38-47](../src/schedule.ts#L38-L47) | a chat's due prompts returned and cleared; their count |
| `tickAfter` | [49](../src/schedule.ts#L49) | an unreferenced timer to the next minute boundary; after the check, the next one |
| `markDue` | [57](../src/schedule.ts#L57) | each entry not due, of an approver, whose cron matches the boundary minute in local time: due, log line, wake of the chat's listeners |
| `cronError` | [71](../src/schedule.ts#L71) | refusal text of a cron |
| `cronMatches`, `fieldValues` | [78-96](../src/schedule.ts#L78-L96) | match against local time, day fields as in vixie cron; the values of a field, none when out of range |

---

## 3. State

### 3.1 Memory

| Item | Holder | Written by | Read by |
|---|---|---|---|
| Actions | `Actions.actions` map ([actions.ts:151](../src/actions.ts#L151)) | `create`, `remove`, `forgetExpired` | tools, `Control` |
| Expiry timers | `Actions.expiries`, one per action ([actions.ts:152](../src/actions.ts#L152)) | `scheduleExpiry`, `expire`, `remove` | |
| Open calls of an action | `ActionRecord.open` ([:51](../src/actions.ts#L51)) | `openCall` and `closeCall` from `actionResult` ([tools.ts:770](../src/tools.ts#L770), [:779](../src/tools.ts#L779)) | `list`, `forgetExpired` |
| Collected replies | `ConnectAction.messages`, `SendAction.responses` ([:64](../src/actions.ts#L64), [:95](../src/actions.ts#L95)) | `ChatItemsWatch.collect` | `connect`, `send` results |
| Item listeners | `ChatItemsWatch.listeners` ([messages.ts:87](../src/messages.ts#L87)) | `collect` | event dispatch |
| Downloads | `AgentTools.downloads`, one per file ID ([tools.ts:536](../src/tools.ts#L536)) | `downloadTo` | file events, checks, `/delete CHAT files` |
| Forwards | `AgentTools.forwarding`, the copy for the post per file ID ([tools.ts:537](../src/tools.ts#L537)) | `forwardFile` | `downloadFile`, before it moves a forwarded file |
| Rejected files | `AgentTools.rejectedFiles` ([tools.ts:538](../src/tools.ts#L538)) | `downloadResult` ([:1145-1146](../src/tools.ts#L1145-L1146)) | the file field; `download`, which then skips the files approval ([:1108](../src/tools.ts#L1108)) |
| Count of file descriptions | `AgentTools.describedFiles` ([tools.ts:539-540](../src/tools.ts#L539-L540)) | `onFileDescription` | `fileServers` |
| Members of the control chat | `Control.members` ([control.ts:166](../src/control.ts#L166)) | `refreshMembers` ([:688-690](../src/control.ts#L688-L690)) | approver checks |
| Schedules and due prompts | `Schedules.chats`, per chat reference: the entries, each with at most one due prompt ([schedule.ts:13](../src/schedule.ts#L13)) | `set` at load, `/schedule`, `/unschedule` and `leaveChat`; `markDue`; `take` | timer, `receive` |
| Schedule listeners | `Schedules.listeners`, wake callbacks per chat reference ([schedule.ts:14](../src/schedule.ts#L14)) | `listen`, from `receive` | `markDue` |
| Mode, expiry, owners list, stop wait | `ServeOptions` of the run. The mode is kept in `Actions.mode` and in each action as well ([actions.ts:42](../src/actions.ts#L42)). | CLI | all modules |

### 3.2 Chat database custom data

| Item | Entity | Value | Written by | Read by |
|---|---|---|---|---|
| Control marker and counter | agent control chat | `{simplexMcp: {control: true, nextAction: N}}` | [`createControlChat`](../src/control.ts#L735-L750), [`saveCounter`](../src/control.ts#L250) | [`findControlGroup`](../src/control.ts#L725), [`controlDataOf`](../src/control.ts#L730) |
| Standing approval of messages | contact or group of the agent | `{...other keys, simplexMcp: {approvedBy, approvedById, approvedAt}}` | [`approveChat`](../src/control.ts#L378), [`revoke`](../src/control.ts#L391) | [`standingApprover`](../src/control.ts#L241) |
| Files approval | contact or group of the agent | `{...other keys, simplexMcpFiles: {approvedBy, approvedById, approvedAt}}` | [`approveChat`](../src/control.ts#L378), [`revoke`](../src/control.ts#L391) | [`standingApprover`](../src/control.ts#L241) |
| Schedules | contact or group of the agent | `{...other keys, simplexMcpSchedules: [{cron, prompt, by, byId, at}, ...]}` | [`addSchedule`](../src/control.ts#L400), [`removeSchedule`](../src/control.ts#L416) | [`schedulesOf`](../src/control.ts#L966) |

The profile, chats, items, members and file transfers are stored by the core in `DATA/simplex_chat.db`; connections and queues in `DATA/simplex_agent.db` (file prefix `DATA/simplex`, [serve.ts:72](../src/serve.ts#L72)).

### 3.3 Files

| Path | Contents | Mode | Written | Read |
|---|---|---|---|---|
| `DATA/` | data folder | 0700, set at every start except on Windows | [files.ts:103-106](../src/files.ts#L103-L106) | |
| `DATA/token` | base64url of 32 random bytes | 0600, set again at every write except on Windows | first start, `--new-token` ([:124-134](../src/files.ts#L124-L134)) | every start |
| `DATA/headers.json` | `{"Authorization":"Bearer <token>"}` | 0600, set again at every write except on Windows | every start ([:140-142](../src/files.ts#L140-L142)) | Claude Code `headersHelper` |
| `DATA/server.json` | `{name, port, expireHours}` | 0600, set again at every write except on Windows | every start, after the HTTP listener is bound ([serve.ts:116](../src/serve.ts#L116)) | `register` |
| `DATA/lock` | PID and a newline | 0600 | start ([files.ts:182-221](../src/files.ts#L182-L221)) | next start |
| `DATA/tmp/` | the core's temp folder: files in transfer | 0700, set at every start | [core.ts:40](../src/core.ts#L40), [:77-81](../src/core.ts#L77-L81) | the core |
| `DATA/files/group-N/`, `DATA/files/contact-N/` | downloaded files and files received without a download, named by file ID and extension | folder 0700 | the core at the download path ([tools.ts:1161-1162](../src/tools.ts#L1161-L1162)); moves ([:1664-1674](../src/tools.ts#L1664-L1674)) | `download` results, `/forward`, `/leave`, `/delete CHAT files` |
| `DATA/forwarded/group-N/`, `DATA/forwarded/contact-N/` | files `/forward` downloaded, named as above, until a download moves them to `DATA/files/` | folder 0700 | the core at the forwarded path ([tools.ts:605-612](../src/tools.ts#L605-L612), [:1161-1162](../src/tools.ts#L1161-L1162)) | `/forward`; moved by `download` ([:595-603](../src/tools.ts#L595-L603)); deleted by `leave`, `/leave` and `/delete CHAT files` ([:1042-1060](../src/tools.ts#L1042-L1060), [:1082-1091](../src/tools.ts#L1082-L1091)) |
| `DATA/files/sent/` | resized copies of sent images, and copies of the files `/forward` posts, one folder per copy | folder 0700; a resized copy 0600 | [tools.ts:1018-1035](../src/tools.ts#L1018-L1035), [:1062-1066](../src/tools.ts#L1062-L1066) | the core; deleted at `sndFileCompleteXFTP`, `sndFileError` and at start ([core.ts:41](../src/core.ts#L41), [tools.ts:696-698](../src/tools.ts#L696-L698), [:981](../src/tools.ts#L981)) |

The core library is downloaded once into `CACHE/core-7.1.3.0/sqlite`, and the Node.js add-on into `CACHE/nodejs-addon-cce06d8/`, each with a `Downloading ...` line on stderr. CACHE is `$XDG_CACHE_HOME/simplex-chat` or `~/.cache/simplex-chat`; on macOS `~/Library/Caches/simplex-chat`; on Windows `%LOCALAPPDATA%\simplex-chat` ([libs.ts:41-48](../../simplex-chat-nodejs/src/libs.ts#L41-L48), [:89](../../simplex-chat-nodejs/src/libs.ts#L89), [:109](../../simplex-chat-nodejs/src/libs.ts#L109), [:116](../../simplex-chat-nodejs/src/libs.ts#L116), [:129](../../simplex-chat-nodejs/src/libs.ts#L129)).

---

## 4. Process lifecycle

### 4.1 Start

1. Data folder, lock, token and headers file are prepared ([serve.ts:67-71](../src/serve.ts#L67-L71)).
2. libsimplex for SQLite is loaded by `ChatApi.init`, and the databases are migrated with confirmation `yesUp` ([serve.ts:72](../src/serve.ts#L72), [api.ts:117-126](../../simplex-chat-nodejs/src/api.ts#L117-L126)).
3. Three log lines: version and NAME, data folder, mode ([serve.ts:73-75](../src/serve.ts#L73-L75)).
4. The active user is read, or created with display name `--display-name` or NAME, empty full name and peer type `agent` ([serve.ts:90](../src/serve.ts#L90)).
5. `prepareCore` reads the network config and sets it with `smpProxyMode` `always` and `smpProxyFallback` `allowProtected` ([serve.ts:91](../src/serve.ts#L91), [core.ts:38-39](../src/core.ts#L38-L39)).
6. `prepareCore` sets `DATA/tmp` as the core's temp folder and deletes `DATA/files/sent` ([core.ts:40-41](../src/core.ts#L40-L41)).
7. Events are buffered, and the chat is started ([serve.ts:92-93](../src/serve.ts#L92-L93)).
8. A `--display-name` different from the profile, or a peer type other than `agent`, is applied with one `apiUpdateProfile` ([serve.ts:94](../src/serve.ts#L94), [core.ts:54-59](../src/core.ts#L54-L59)).
9. The agent control chat is found by its marker, created with counter 1, or reset with `--reset-owners` ([:96](../src/serve.ts#L96), [:181-186](../src/serve.ts#L181-L186)).
10. `Actions`, `ChatItemsWatch`, `Schedules`, `AgentTools` and `Control` are created; the counter is taken from the marker ([:97-107](../src/serve.ts#L97-L107)).
11. `Control.load` sets `Actions.lost` for a lost control chat. Otherwise it updates the group name and command menu when they differ in the control chat's profile read at the start, as in 0.2, reads the members, and reads the schedules of the agent's chats from their custom data with one `apiGetChats` ([control.ts:188-196](../src/control.ts#L188-L196), [:207-214](../src/control.ts#L207-L214), [:757-760](../src/control.ts#L757-L760)).
12. The HTTP server listens; with the port in use, the process exits with code 1 ([serve.ts:115](../src/serve.ts#L115), [:188-195](../src/serve.ts#L188-L195)). Until step 14 ends, requests to `/mcp` that pass the token check get 503 with `Retry-After: 1` ([http.ts:23](../src/http.ts#L23), [serve.ts:114-115](../src/serve.ts#L114-L115), [:123](../src/serve.ts#L123)).
13. `server.json` is written ([serve.ts:116](../src/serve.ts#L116)).
14. The server runs the owner setup, `Control.start`; after an error, HTTP is closed and the error is rethrown ([serve.ts:117-122](../src/serve.ts#L117-L122)).
15. The schedule timer is started; `Listening on http://127.0.0.1:PORT/mcp` ([:124-125](../src/serve.ts#L124-L125)).
16. Buffered events are replayed, then live events are dispatched ([serve.ts:128](../src/serve.ts#L128), [core.ts:61-75](../src/core.ts#L61-L75)).

After an error inside `startAgent` (steps 4-16) the chat is closed; after any error once the lock is taken, the lock is released ([serve.ts:69-85](../src/serve.ts#L69-L85)).

### 4.2 Event dispatch

Each `onAny` receiver is awaited by the library before the next event is read ([api.ts:195-202](../../simplex-chat-nodejs/src/api.ts#L195-L202)). The dispatcher in [`serve.ts:128-140`](../src/serve.ts#L128-L140) passes items of the control group to `Control` first. Every event then goes to [`AgentTools.onEvent`](../src/tools.ts#L673-L700), which runs the `AgentTools` and `ChatItemsWatch` handlers below. Finally, `Control` receives file errors and membership events:

| Event | Condition | Handler |
|---|---|---|
| `newChatItems` | item of the control group, main scope | [`Control.onMessage`](../src/control.ts#L294) |
| `newChatItems` | every item | [`AgentTools.onNewItems`](../src/tools.ts#L624), then [`ChatItemsWatch.deliver`](../src/messages.ts#L116) |
| `chatItemUpdated` | | [`AgentTools.onFile`](../src/tools.ts#L634), then [`ChatItemsWatch.update`](../src/messages.ts#L120) |
| `chatItemsDeleted` | | [`ChatItemsWatch.remove`](../src/messages.ts#L124) of each `deletedChatItem` |
| `contactConnected` | | [`AgentTools.onContactConnected`](../src/tools.ts#L663) |
| `userJoinedGroup` | | [`AgentTools.onUserJoinedGroup`](../src/tools.ts#L668) |
| `rcvFileComplete` | | [`AgentTools.onFileComplete`](../src/tools.ts#L639) |
| `rcvFileSndCancelled` | | [`AgentTools.onFileCancelled`](../src/tools.ts#L651) |
| `rcvFileError` | | [`AgentTools.onFileError`](../src/tools.ts#L646) |
| `rcvFileDescrReady` | | [`AgentTools.onFileDescription`](../src/tools.ts#L658) |
| `sndFileCompleteXFTP` | | [`AgentTools.onSentFile`](../src/tools.ts#L981) |
| `sndFileError` | | [`AgentTools.onSentFile`](../src/tools.ts#L981), then [`Control.fileNotSent`](../src/control.ts#L283) |
| `joinedGroupMember`<br>`joinedGroupMemberConnecting`<br>`connectedToGroupMember`<br>`memberRole`<br>`leftMember`<br>`deletedMember`<br>`deletedMemberUser`<br>`groupDeleted` | event of the control group | [`Control.onEvent`](../src/control.ts#L306), queued one at a time |
| any event | after `stop()` started | ignored |

Owner commands are queued in a second chain, independent of the membership chain ([control.ts:300-303](../src/control.ts#L300-L303)).

### 4.3 Stop

On `SIGINT` or `SIGTERM` the server runs one stop; a later signal ends its wait for cancelled cards ([serve.ts:47-63](../src/serve.ts#L47-L63)). `RunningServer.stop` ([serve.ts:146-157](../src/serve.ts#L146-L157)):

1. Events are ignored from now on, and the schedule timer is cleared.
2. `Actions.stop` starts: new actions are refused with `The server is stopping.`; the expiry timers are cleared; cards being posted are awaited; pending and performing actions are marked interrupted; the stop signal of the performs is aborted, which ends the wait of a `connect` for its connection; their cards are edited, or posted when they have no card; outcome edits already in progress are awaited ([actions.ts:284-316](../src/actions.ts#L284-L316)).
3. In parallel with step 2, the HTTP server is closed with all connections, and open calls are aborted ([http.ts:48-52](../src/http.ts#L48-L52)).
4. `Cancelled K action(s).` when K > 0 ([serve.ts:152](../src/serve.ts#L152)).
5. When cards were edited or posted at the stop and the control chat is not lost, the server waits `--stop-wait` seconds, 10 by default, for them to be sent; a second signal ends the wait ([serve.ts:153](../src/serve.ts#L153), [:165-179](../src/serve.ts#L165-L179)).
6. `chat.close()`: the chat is stopped, then the store is closed ([api.ts:165-170](../../simplex-chat-nodejs/src/api.ts#L165-L170)).
7. The lock is removed, `Stopped.` is logged, and the process exits with code 0, or 1 after an error ([serve.ts:155-156](../src/serve.ts#L155-L156), [:53-59](../src/serve.ts#L53-L59)).

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

The mode in which an action is created is kept in `ActionRecord.mode`. The action's card and log line are written for that mode ([actions.ts:180](../src/actions.ts#L180), [:193](../src/actions.ts#L193), [control.ts:225](../src/control.ts#L225)).

Sources: [actions.ts:193](../src/actions.ts#L193), [tools.ts:512](../src/tools.ts#L512), [control.ts:268-275](../src/control.ts#L268-L275), [:721-723](../src/control.ts#L721-L723). In both modes a new action is refused unless the control chat has an approver ([control.ts:230-234](../src/control.ts#L230-L234)).

### 5.2 Owners and approvers

| Term | Rule | Code |
|---|---|---|
| human | member other than the agent's membership, status `connected` or `complete` | [control.ts:709-711](../src/control.ts#L709-L711), [tools.ts:1545-1547](../src/tools.ts#L1545-L1547) |
| human owner | human with role `owner` | [control.ts:696-698](../src/control.ts#L696-L698) |
| approver, `--owners` absent | member with role `owner` | [control.ts:713-718](../src/control.ts#L713-L718) |
| approver, `--owners` without a list | none | [control.ts:717](../src/control.ts#L717) |
| approver, `--owners ID:NAME,...` | member with role `owner` whose `groupMemberId` and `displayName`, folded to one line by `oneLine`, match an entry | [control.ts:717](../src/control.ts#L717), [:1028-1030](../src/control.ts#L1028-L1030) |

The command sender's member record from the chat item is checked ([control.ts:298](../src/control.ts#L298), [:329](../src/control.ts#L329)). The approver count for refusals, standing approvals and schedules is taken from `Control.members`, refreshed at load and at each membership event ([control.ts:310](../src/control.ts#L310), [:688-690](../src/control.ts#L688-L690)).

Owner setup at start ([control.ts:216-222](../src/control.ts#L216-L222)):

| `--owners` | Start | First join by the link |
|---|---|---|
| absent | a human owner exists: register hint<br>otherwise: `No owners in the agent control chat.` when humans exist, then a link with role `member` | link deleted, role `owner`, greeting, log line, register hint |
| without a list | no humans: link with role `observer`<br>humans exist: their IDs and the restart command, in the log and the chat | link deleted, the joined member and the restart command announced |
| `ID:NAME,...` | each entry matched to a human, or exit 2<br>role `owner` set where missing<br>other humans removed<br>link deleted<br>`Owners: ...` and register hint | |

Sources: [`startOwners`](../src/control.ts#L541), [`startObserver`](../src/control.ts#L547), [`startListed`](../src/control.ts#L552), [`ownerJoined`](../src/control.ts#L571), [`observerJoined`](../src/control.ts#L580).

Membership changes at run time ([control.ts:306-322](../src/control.ts#L306-L322)):

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
| `/reject`, or `/leave CHAT` for a pending action of CHAT with a card | `pending` | `rejected` | [actions.ts:269-274](../src/actions.ts#L269-L274), [control.ts:457-459](../src/control.ts#L457-L459) |
| expiry time passes | `pending` | `expired` | [actions.ts:302-312](../src/actions.ts#L302-L312) |
| `/approve N`, `/approve CHAT N` and `/approve CHAT all` for other actions; `/receive N`, `/receive CHAT N` and `/receive CHAT N all` for downloads | `pending` | `performing` | [actions.ts:263-267](../src/actions.ts#L263-L267), [control.ts:364-373](../src/control.ts#L364-L373) |
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

An action is removed from memory when its result is returned to a call ([tools.ts:776](../src/tools.ts#L776)). An action that is decided, without open calls and past its expiry time, is removed as well. This is checked at each decision, at the close of each call and at the action's expiry timer ([actions.ts:314-316](../src/actions.ts#L314-L316), [:232-235](../src/actions.ts#L232-L235), [:351-355](../src/actions.ts#L351-L355)). `serve` has one session, `undefined`, so all its actions are counted against the limits of 20. A pending action of `serve` that expires is kept longer, see [5.6](#56-expiry). A repeated call before its removal is therefore answered with `expired`, as in 0.2 ([actions.ts:307-309](../src/actions.ts#L307-L309), [:243-247](../src/actions.ts#L243-L247), [control.ts:325](../src/control.ts#L325)). A `download` still performing at the end of `wait` is returned as `performing` and kept ([tools.ts:773](../src/tools.ts#L773)).

### 5.4 Cards

The server posts a card when a `pending` action is created, and edits it with `apiUpdateChatItem` at the outcome ([actions.ts:321](../src/actions.ts#L321), [control.ts:268-275](../src/control.ts#L268-L275)). For a `connect`, `send` or `leave` without a card (mode `host`, standing sends) the server posts the outcome as a new message; a `download` without a card gets none ([control.ts:721-723](../src/control.ts#L721-L723)). The text per state is built by [`cardText`](../src/control.ts#L799-L818). A card of a `send` or `download` of an image or video file is an image or video message with the file's preview. In a plugin session group where files and media are refused by the group's preference, the card is text ([control.ts:641-686](../src/control.ts#L641-L686)). The forms are listed in [API: cards](api.md#22-cards).

Outcomes are posted after `done` and `failed` ([actions.ts:348](../src/actions.ts#L348)), after `rejected` and `expired` ([:273](../src/actions.ts#L273), [:311](../src/actions.ts#L311)), and at stop ([:292](../src/actions.ts#L292)). They are skipped after the control chat is lost ([:292](../src/actions.ts#L292), [:348](../src/actions.ts#L348), [:358](../src/actions.ts#L358)).

### 5.5 Standing approvals

| Aspect | Messages | Files | Code |
|---|---|---|---|
| grant | `/approve CHAT all` from an approver in mode `requests`; pending sends of CHAT with a card are approved | `/receive CHAT N all` from an approver, on the card of download N; N is performed, and the other pending downloads of CHAT stay pending | [control.ts:372](../src/control.ts#L372), [:378-389](../src/control.ts#L378-L389) |
| mode `host` | nothing stored; reply `Approvals apply in mode requests; this server runs in mode host.` | downloads get no card, so the command has none to act on | [control.ts:344](../src/control.ts#L344) |
| removal | `/revoke CHAT`: the `simplexMcp` key is removed, other keys kept | `/receive CHAT off`: the `simplexMcpFiles` key is removed | [control.ts:391-398](../src/control.ts#L391-L398) |
| use | mode `requests`, the control chat not lost, and `approvedById` equal to the `groupMemberId` of a current approver | the same | [control.ts:241-248](../src/control.ts#L241-L248) |
| scope | `send` | `download` of a file whose servers are all known | [tools.ts:930](../src/tools.ts#L930), [:1108](../src/tools.ts#L1108) |
| decider shown | `safeName(approvedBy)` | the same | [control.ts:243](../src/control.ts#L243) |
| storage | the chat's custom data; removed with the chat by `leave` | the same | [tools.ts:1082-1091](../src/tools.ts#L1082-L1091) |

Files of `reply` in a session group are sent without approval, and no standing approval applies to them. They are allowed or refused by the group's preference for files and media ([8.5](#85-approval-model)).

### 5.6 Expiry

| Rule | Code |
|---|---|
| `expires = created + expireHours × 3600000`, `expireHours` 1 to 23, default 23 | [actions.ts:190](../src/actions.ts#L190), [cli.ts:79](../src/cli.ts#L79), [:102-104](../src/cli.ts#L102-L104) |
| one unreferenced timer per action, set at its creation for `expires - now`, at most 60 s, and set again until the expiry time. Before each owner command, all actions past `expires` are expired. | [actions.ts:11](../src/actions.ts#L11), [:297-300](../src/actions.ts#L297-L300), [control.ts:325](../src/control.ts#L325) |
| pending and due: `expired`, log `Action N expired.`, card edited to `Expired.` | [actions.ts:302-312](../src/actions.ts#L302-L312) |
| decided, no open call, due: removed from memory. A pending action of `serve` that expires stays until the next owner command or its check 60 s later, as in 0.2. | [actions.ts:314-316](../src/actions.ts#L314-L316), [:307-309](../src/actions.ts#L307-L309), [:243-247](../src/actions.ts#L243-L247) |
| connect wait for the other side: at most until `created + 24 h - 10 min` | [tools.ts:155](../src/tools.ts#L155), [:822](../src/tools.ts#L822) |

A timer runs on the monotonic clock, while `expires` is a time on the wall clock. A timer that fires before `expires` is set again. So an action whose expiry time passes while the computer sleeps expires within 60 s after the computer wakes ([actions.ts:305](../src/actions.ts#L305)).

### 5.7 Action counter

| Rule | Code |
|---|---|
| initial value: `nextAction` of the control marker, or 1 | [serve.ts:97](../src/serve.ts#L97) |
| a new action takes `next`, then `next` grows by 1 | [actions.ts:181](../src/actions.ts#L181), [:206](../src/actions.ts#L206) |
| the new `next` is written before the card is posted | [actions.ts:320-321](../src/actions.ts#L320-L321) |
| one write at a time, also after a failed write | [control.ts:250-262](../src/control.ts#L250-L262) |
| `--reset-owners` copies the value into the new control chat | [serve.ts:184-185](../src/serve.ts#L184-L185), [control.ts:762-765](../src/control.ts#L762-L765) |
| a refused call leaves the counter unchanged | [actions.ts:205](../src/actions.ts#L205) |

### 5.8 Scheduled prompts

| Rule | Code |
|---|---|
| add: `/schedule CHAT CRON PROMPT` from an approver, in both modes; the entry is appended to the custom data of CHAT, at most 10 per chat | [control.ts:400-414](../src/control.ts#L400-L414) |
| removal: `/unschedule CHAT N` from an approver; the later entries of CHAT are renumbered | [control.ts:416-426](../src/control.ts#L416-L426) |
| the entries in memory are set at load, at each add and removal, and to none when the agent leaves the chat | [control.ts:195](../src/control.ts#L195), [:411](../src/control.ts#L411), [:423](../src/control.ts#L423), [tools.ts:1090](../src/tools.ts#L1090) |
| at each minute boundary, an entry becomes due when it is not due already, member `byId` is an approver, the control chat is not lost, and its cron matches the boundary minute in local time; logged | [schedule.ts:49-68](../src/schedule.ts#L49-L68), [control.ts:246-248](../src/control.ts#L246-L248) |
| one due prompt per entry, with the minute it first became due; an entry set again with the same creation time keeps it; the due prompt of a removed entry is dropped | [schedule.ts:19-22](../src/schedule.ts#L19-L22), [:61-62](../src/schedule.ts#L61-L62) |
| `receive` of CHAT: a waiting call ends when a prompt of CHAT becomes due; the due prompts are taken after the messages are marked read, and returned in `scheduled` | [tools.ts:1251](../src/tools.ts#L1251), [:1263-1280](../src/tools.ts#L1263-L1280) |
| prompts due while the server is stopped are not delivered later: due prompts are kept in memory only, and the timer starts after the owner setup | [serve.ts:124](../src/serve.ts#L124), [:148](../src/serve.ts#L148) |

When a tick is delayed by a sleep of the computer, the minute it was set for is checked, and the minutes until the next boundary are skipped ([schedule.ts:49-55](../src/schedule.ts#L49-L55)).

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
| card and reply names | folded to one line; a valid display name is plain, others a code span | [`safeName`](../src/control.ts#L947-L950) |
| card text lines | one code span per line; backticks become `'`; a leading or trailing space becomes U+00A0; blank lines empty | [`codeLines`, `codeSpan`](../src/control.ts#L939-L945) |
| file names and paths on cards and posts | folded to one line, one code span | [control.ts:837](../src/control.ts#L837), [:822](../src/control.ts#L822), [:850](../src/control.ts#L850), [:289-291](../src/control.ts#L289-L291) |
| reply quote on a send card | whitespace runs to one space, first 100 characters (code points), one code span | [control.ts:838](../src/control.ts#L838) |
| link texts on a connect card, the input preview on a tool-use card of the plugin | code span lines in the bytes that the largest card form leaves; the line that does not fit is cut and ends in `…` inside its code span; later lines left out | [control.ts:904-918](../src/control.ts#L904-L918) |
| mention names | valid name: `@name` or `@'name'`; others a code span | [`mentionText`](../src/control.ts#L952-L955) |
| errors on cards | folded to one line, one code span | [control.ts:812](../src/control.ts#L812) |
| `--owners` part of restart lines in the chat | one code span | [control.ts:163](../src/control.ts#L163), [:1052-1054](../src/control.ts#L1052-L1054) |
| terminal names and errors of actions | folded to one line | [actions.ts:209-210](../src/actions.ts#L209-L210), [:402-412](../src/actions.ts#L402-L412), [control.ts:162](../src/control.ts#L162) |
| tool results | returned as data; schema descriptions and instructions mark `text`, `from`, `name`, file names, downloaded files, `description`, `welcome` and `commands` with their labels and params as data, not instructions; the instructions take a chat's messages as prompts only when the user said so in the session, and limit what the agent shares about its user | [tools.ts:197](../src/tools.ts#L197), [:201](../src/tools.ts#L201), [:206](../src/tools.ts#L206), [:231-232](../src/tools.ts#L231-L232), [:242-247](../src/tools.ts#L242-L247), [:415](../src/tools.ts#L415), [:458](../src/tools.ts#L458) |

The server writes the tappable form `/'...'` only in the card's command line ([control.ts:855-870](../src/control.ts#L855-L870)), in `/'revoke CHAT'` of standing cards and replies ([:810](../src/control.ts#L810), [:388](../src/control.ts#L388)), in `/'delete CHAT files'` of the kept-files message ([:467](../src/control.ts#L467)), in `/'unschedule CHAT N'` of the `/schedule` replies ([:407](../src/control.ts#L407), [:413](../src/control.ts#L413)), in the `/help` texts ([:53-97](../src/control.ts#L53-L97)), and in `/schedules` lines, where an approver's prompt is shown with its `/'...'` forms ([:522](../src/control.ts#L522)).

### 6.4 Chat references

| Rule | Code |
|---|---|
| `^[@#][1-9]\d{0,14}$`: 1 to 15 digits; the core reads `#(2^64 + N)` as `#N` | [messages.ts:150-152](../src/messages.ts#L150-L152), [scenarios.test.ts:491-503](../test/e2e/scenarios.test.ts#L491-L503) |
| the control chat is refused by `send`, `receive`, `download`, `leave` and the owner commands with CHAT, and left out of `list`, `/list` and `/schedules` | [tools.ts:1248](../src/tools.ts#L1248), [:1297-1316](../src/tools.ts#L1297-L1316), [control.ts:528-535](../src/control.ts#L528-L535) |
| a store error of the chat read is reported as `No chat CHAT.` | [tools.ts:1254](../src/tools.ts#L1254), [:1314](../src/tools.ts#L1314) |
| the agent's own links are refused by `connect` (`ownLink` plans) | [tools.ts:1418](../src/tools.ts#L1418), [:1436](../src/tools.ts#L1436), [:1458](../src/tools.ts#L1458) |

### 6.5 Owner commands

| Rule | Code |
|---|---|
| source: `groupRcv` text items in the main scope of the control group | [serve.ts:132](../src/serve.ts#L132), [control.ts:296](../src/control.ts#L296), [:772-774](../src/control.ts#L772-L774) |
| form: one line `/keyword params`, leading spaces allowed | [control.ts:792-797](../src/control.ts#L792-L797) |
| A text outside the requests from any sender is answered with the commands reply of `serve`, or with `Unknown command.` in a plugin session group. `/help` replies are sent to any sender. Other commands are accepted only from approvers. A command from another sender is answered in `serve`, and left without a reply in a plugin session group. | [control.ts:327-329](../src/control.ts#L327-L329) |
| commands run one at a time in arrival order | [control.ts:300-303](../src/control.ts#L300-L303) |
| `/approve`, `/reject`, `/receive` and `/forward` need a pending action with a card, and the action's chat when CHAT is given; `/receive` and `/forward` take downloads only, `/approve` the other actions only | [control.ts:362-372](../src/control.ts#L362-L372) |
| only the key `simplexMcpSchedules` of the custom data of CHAT is changed by `/schedule` and `/unschedule` | [control.ts:410](../src/control.ts#L410), [:422](../src/control.ts#L422) |
| a scheduled prompt is returned to the agent only in `scheduled` of `receive`, apart from the chat's messages, and marked as written by the owner in the output schema and the description of `receive` | [tools.ts:350-354](../src/tools.ts#L350-L354), [:435](../src/tools.ts#L435), [:1279-1280](../src/tools.ts#L1279-L1280) |

### 6.6 Files

| Rule | Code |
|---|---|
| `send` refuses files named `id_*`, `*.pem`, `*.key`, `*.p12` or `*.pfx`, and files under `~/.ssh`, `~/.gnupg`, the data folder and `~/.simplex-mcp` except the agent's own `DATA/files/`, each by its real path; the refusal is posted to the control chat and logged | [tools.ts:995-1009](../src/tools.ts#L995-L1009), [:1629-1648](../src/tools.ts#L1629-L1648), [control.ts:277-281](../src/control.ts#L277-L281) |
| the file of a `send` is sent only when its real path, size, inode and change time at the perform are those of the request | [tools.ts:1011-1016](../src/tools.ts#L1011-L1016) |
| a sent JPEG or PNG is a resized copy made by the core | [tools.ts:1018-1029](../src/tools.ts#L1018-L1029) |
| `ffmpeg` and `ffprobe` read a video as MP4 or QuickTime, local files only, without standard input, 30 s and 64 MiB of output each | [tools.ts:1698-1716](../src/tools.ts#L1698-L1716) |
| a download is saved only at `DATA/files/group-N/` or `DATA/files/contact-N/`, or for `/forward` at the same name under `DATA/forwarded/`, named by its file ID and an extension of 1 to 16 ASCII letters and digits | [messages.ts:215-227](../src/messages.ts#L215-L227) |
| a forwarded file is outside `DATA/files/`, so `send` refuses it; a download moves it into `DATA/files/` after the copy for the post is made | [tools.ts:595-612](../src/tools.ts#L595-L612), [:1004-1009](../src/tools.ts#L1004-L1009) |
| a file on unknown servers gets a card that names them, also under a files approval; relays are approved only for such a card, and for every other download, and in mode `host`, the core checks the complete description and downloads a file on unknown servers only when the IP address is protected | [tools.ts:1108](../src/tools.ts#L1108), [:618](../src/tools.ts#L618), [:1162](../src/tools.ts#L1162), [control.ts:865-868](../src/control.ts#L865-L868) |
| an accepted file that the core returns to `rcvInvitation` fails its download at the next check | [tools.ts:1175](../src/tools.ts#L1175) |
| a received file is moved into the chat's folder at its completion, unless it is at the forwarded path, also from a member chat of a group, which has a download in memory as the main conversation does; a file without a download in memory, such as one whose transfer started before a restart, only when it has its full size; a partial file is deleted only for a download in memory, when it is no larger than the file | [tools.ts:1214-1223](../src/tools.ts#L1214-L1223), [:1650-1656](../src/tools.ts#L1650-L1656) |

### 6.7 Link and network

- The join link is a group link of the control chat with role `member`, or `observer` with `--owners`; it is printed with a QR code on stdout and deleted at the first join event ([control.ts:606-616](../src/control.ts#L606-L616), [:572](../src/control.ts#L572), [:581](../src/control.ts#L581)).
- At every start the server sets private message routing: `smpProxyMode` `always`, `smpProxyFallback` `allowProtected` ([core.ts:38-39](../src/core.ts#L38-L39)).
- With `incognito: true` the join methods connect incognito ([tools.ts:856](../src/tools.ts#L856)); the core then uses a random profile for that chat ([PLAN, Participants](../../../plans/2026-10-19-mcp-server.md#participants)).

---

## 7. Limits

| Limit | Value | Code |
|---|---|---|
| pending actions | 20 | [actions.ts:9](../src/actions.ts#L9) |
| actions being performed | 20 | [actions.ts:10](../src/actions.ts#L10) |
| expiry | 1 to 23 h, default 23 | [cli.ts:79](../src/cli.ts#L79), [:102-104](../src/cli.ts#L102-L104) |
| connect wait for the other side | `created + 23 h 50 min` | [tools.ts:155](../src/tools.ts#L155) |
| send, connect and download card | 15000 bytes JSON-escaped, in its largest form | [control.ts:35](../src/control.ts#L35), [:882-890](../src/control.ts#L882-L890) |
| error assumed in the largest card | 600 × `€` | [control.ts:42](../src/control.ts#L42) |
| message as the core sends it | 15602 bytes, counted with 600 bytes of envelope; `send` refuses a longer one, and the post of `/forward` leaves out its quote of the card | [tools.ts:165-166](../src/tools.ts#L165-L166), [:1588-1591](../src/tools.ts#L1588-L1591), [control.ts:428-447](../src/control.ts#L428-L447) |
| `send.text` | 1 to 12000 characters | [tools.ts:269](../src/tools.ts#L269) |
| `connect.message` | 1 to 2000 characters | [tools.ts:257](../src/tools.ts#L257) |
| `send.mention` | 1 to 3 unique member IDs | [tools.ts:273](../src/tools.ts#L273) |
| `wait` | 0 to 86400 s | [tools.ts:258](../src/tools.ts#L258), [:274](../src/tools.ts#L274), [:286](../src/tools.ts#L286), [:309](../src/tools.ts#L309) |
| `idle` | 1 to 300 s, default 30 | [tools.ts:259](../src/tools.ts#L259), [:275](../src/tools.ts#L275), [:161](../src/tools.ts#L161) |
| `limit` | 1 to 100, default 50 | [tools.ts:276](../src/tools.ts#L276), [:287](../src/tools.ts#L287), [:162](../src/tools.ts#L162) |
| `connect` snapshot | last 50 items | [tools.ts:163](../src/tools.ts#L163) |
| collected items per action or call | 100, then their IDs kept as overflow until deleted | [messages.ts:10](../src/messages.ts#L10), [:109-110](../src/messages.ts#L109-L110) |
| unread page | 100 items | [messages.ts:9](../src/messages.ts#L9) |
| chat list read | one read of 1000000 chats | [tools.ts:154](../src/tools.ts#L154) |
| `download` wait, default | 60 s | [tools.ts:158](../src/tools.ts#L158) |
| download check | every 60 s | [tools.ts:159](../src/tools.ts#L159) |
| download without progress | 30 min, then cancelled | [tools.ts:160](../src/tools.ts#L160) |
| `ffmpeg`, `ffprobe` | 30 s, 64 MiB of output | [tools.ts:164](../src/tools.ts#L164), [:168](../src/tools.ts#L168) |
| stop wait | 0 to 60 s, default 10 | [cli.ts:84](../src/cli.ts#L84), [:105-107](../src/cli.ts#L105-L107) |
| error details | 500 characters | [log.ts:4](../src/log.ts#L4) |
| progress interval | 60 s | [messages.ts:7](../src/messages.ts#L7) |
| expiry timer | at most 60 s, set again until the expiry time | [actions.ts:11](../src/actions.ts#L11), [:298](../src/actions.ts#L298) |
| HTTP body | 4 MiB | [server index.cjs:254](../node_modules/@modelcontextprotocol/server/dist/index.cjs#L254) |
| SSE keep-alive | 15 s | [mcp-BumlNux0.cjs:64](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L64) |
| agent NAME | 1 to 32 characters | [cli.ts:170](../src/cli.ts#L170) |
| display name | 50 characters, `mkValidName` rules | [messages.ts:11-12](../src/messages.ts#L11-L12), [:341-363](../src/messages.ts#L341-L363) |
| port | 1 to 65535, default 5226 | [cli.ts:82](../src/cli.ts#L82), [:114-116](../src/cli.ts#L114-L116) |
| schedules per chat | 10 | [control.ts:39](../src/control.ts#L39), [:407](../src/control.ts#L407) |
| scheduled prompt | 1 to 2000 characters (code points), one line | [control.ts:40](../src/control.ts#L40), [:405](../src/control.ts#L405), [:792-797](../src/control.ts#L792-L797) |
| `/schedules` reply | 100 lines of at most 100 bytes, then a count of the others | [control.ts:37-38](../src/control.ts#L37-L38), [:521-526](../src/control.ts#L521-L526) |
| schedule tick | each minute boundary | [schedule.ts:4](../src/schedule.ts#L4), [:49-55](../src/schedule.ts#L49-L55) |

---

## 8. Claude Code plugin

The plugin runs the same tools, `Actions` and `Control` as `serve`, in other processes. ROOT is the plugin's data root, `~/.simplex-mcp` or `SIMPLEX_MCP_HOME` ([files.ts:31-34](../src/files.ts#L31-L34)).

### 8.1 System diagram

```mermaid
flowchart LR
  subgraph cc["Claude Code"]
    ses["session 1, session 2"]
    hk["SessionStart, PreToolUse,<br/>UserPromptSubmit, Stop,<br/>PostModelSwitch hooks"]
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
  term["terminal"] -->|"npx ... link, reset-owners"| link
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

1. Claude Code calls a tool of the MCP server `simplex` ([session.ts:57](../src/session.ts#L57)). The SDK validates the arguments ([tools.ts:506-516](../src/tools.ts#L506-L516)).
2. Without a connection, the session process checks the service's status and version, and spawns the service when it is not running. It then sends `POST /session` and holds `POST /attach` open ([session.ts:160-185](../src/session.ts#L160-L185), [client.ts:40-52](../src/client.ts#L40-L52)).
3. The session process sends `POST /call` with the session ID, the tool and the input. It relays each `{progress}` line as an MCP progress notification, and returns the `{result}` line ([session.ts:132-151](../src/session.ts#L132-L151)).
4. The service checks the token ([socket.ts:18-48](../src/socket.ts#L18-L48)). It finds or opens the entry of the session or of its alias, and runs the session's `AgentTools.call` with a signal tied to the request ([service.ts:326-345](../src/service.ts#L326-L345)).
5. `AgentTools` sends chat commands to the core `_hub`. Actions are created in the session's `Actions`, and the session's `Control` posts them as cards in the session's group ([hub.ts:71-107](../src/hub.ts#L71-L107)).
6. The core emits events to `Hub.onEvent`. The hub passes them to the Main group, each session's `AgentTools.onEvent`, and the `Control` of the session group they belong to ([hub.ts:498-528](../src/hub.ts#L498-L528)).

Data flow of a prompt from the session group:

1. `Session.onMessage` receives a message of a member in the group of session N ([hub.ts:502-507](../src/hub.ts#L502-L507)). A one-line `/` command goes to `Control`. Another message is queued after the earlier ones ([:157-164](../src/hub.ts#L157-L164)). `Session.prompt` drops a message of `User` and delivers the rest ([:234-237](../src/hub.ts#L234-L237)).
2. In the first 30 s after the hub opened, a delivery without an attachment waits for one. With an attachment, the file of the message, if any, is received. Then, if the session is not attached, the hub replies `Session N is not running.` Otherwise the prompt is pushed, and the line `{prompt}` is written to every `/attach` stream of the session ([hub.ts:239-253](../src/hub.ts#L239-L253), [service.ts:314](../src/service.ts#L314)).
3. The session process sends it to Claude Code as `notifications/claude/channel` ([session.ts:53-56](../src/session.ts#L53-L56), [:197-201](../src/session.ts#L197-L201)).
4. The hooks post the terminal prompts and the final answer of each turn with `POST /turn` ([hook.ts:114-135](../src/hook.ts#L114-L135), [hub.ts:184-206](../src/hub.ts#L184-L206)).

### 8.2 Modules

#### [`session.ts`](../src/session.ts)

| Symbol | Line | Contents |
|---|---|---|
| `unattendedRun`, `channelInstructions`, `permissionRequest` | [15-23](../src/session.ts#L15-L23) | the text of a run that is not interactive; instructions of the channel; schema of a permission request |
| `startSession` | [40](../src/session.ts#L40) | The session ID and the options are checked. Every call is answered with a text when the run is not interactive or the options are invalid. The `McpServer` gets the instructions, the capabilities `claude/channel` and `claude/channel/permission`, the tools with `reply`, the permission request handler and a stdio transport. Without such a text, the session process connects to the service in the background. |
| `ServiceLink.connect`, `start`, `close`, `reconnect` | [79-112](../src/session.ts#L79-L112) | one shared connect promise, refused at once after a 403 of `/session`. After that refusal, and after `close`, `start` neither logs nor reconnects ([88-97](../src/session.ts#L88-L97)). Otherwise `start` logs a failure on stderr, and connects again later unless the failure is a `ProtocolError`. `close` ends the attachment and the retries. `reconnect` sets one timer that starts the connection again after the delay. The delay is 1 s at first, and doubles each time the timer fires, up to 30 s. |
| `ServiceLink.permission` | [114](../src/session.ts#L114) | `POST /permission` after the connect, and nothing after the refusal |
| `ServiceLink.call` | [120](../src/session.ts#L120) | `forward`, repeated once after status 409. The service returns 409 for an unknown session before it runs the call. Failures are returned as tool errors, with the text of a run that is not interactive after the refusal. |
| `ServiceLink.forward` | [132](../src/session.ts#L132) | the `/call` stream of progress lines and the result line. The connection is dropped when the stream breaks. |
| `ServiceLink.disconnect` | [153](../src/session.ts#L153) | drops the connection only when it is the current one |
| `ServiceLink.open` | [160](../src/session.ts#L160) | `ensureService`, then `/session` with the parent process ID. A 403 of `/session` is recorded as the refusal. `/attach` is awaited until its first line, and each line goes to the channel. When `/attach` ends, `reconnect` is called. |
| `failureText` | [188](../src/session.ts#L188) | not running or status 503: `The SimpleX service stopped; repeat the call.` |
| `channelNotification` | [197](../src/session.ts#L197) | attach lines to `notifications/claude/channel` and `notifications/claude/channel/permission` |

#### [`client.ts`](../src/client.ts)

| Symbol | Line | Contents |
|---|---|---|
| `ServiceClient.forRoot` | [27](../src/client.ts#L27) | ROOT created, token, socket path |
| `ServiceClient.ensureService` | [40](../src/client.ts#L40) | The status is read. A service of a lower version is stopped, and a new one is spawned when none runs. The protocol is checked. |
| `spawnService` | [76](../src/client.ts#L76) | the detached launcher `node -e` [`launcher`](../src/client.ts#L14-L18) with stdout and stderr to `service.log`, a ready pipe on fd 3, and the environment variables of `serviceVariables` ([13](../src/client.ts#L13)). The launcher spawns the service detached with its fd 1, 2 and 3, writes `error: TEXT` on fd 3 when that spawn fails, and exits at once, so the service is no descendant of the caller. |
| `firstLine` | [99](../src/client.ts#L99) | first line of the ready pipe, a spawn error, or the 5-minute timeout |
| `compareVersions` | [125](../src/client.ts#L125) | numeric comparison of the first three version parts |

#### [`socket.ts`](../src/socket.ts)

| Symbol | Line | Contents |
|---|---|---|
| `SocketError` | [12](../src/socket.ts#L12) | message and HTTP status |
| `listen` | [18](../src/socket.ts#L18) | `http.Server` on the socket path, with the token check, route table, body read, error replies and timeouts. A stale socket file is removed. |
| `sendJson`, `startLines` | [50-60](../src/socket.ts#L50-L60) | JSON reply; NDJSON stream |
| `isNotRunning` | [62](../src/socket.ts#L62) | `ENOENT` or `ECONNREFUSED` |
| `requestJson`, `streamLines` | [67-86](../src/socket.ts#L67-L86) | client request with a JSON reply; client request with NDJSON lines; a status other than 200 to `SocketError` |
| `send` | [88](../src/socket.ts#L88) | `http.request` on the socket path, without a connection pool, bearer header |

#### [`protocol.ts`](../src/protocol.ts)

| Symbol | Line | Contents |
|---|---|---|
| `protocolVersion`, `unattendedStatus` | [5-6](../src/protocol.ts#L5-L6) | `1`; 403, the status of `/session` from a process marked unattended |
| `Owners`, `OwnerState` | [13-19](../src/protocol.ts#L13-L19) | the owners' names; with the Main group link, for `/title` and `/link` only |
| `SessionRequest`, `TitleRequest` | [21-38](../src/protocol.ts#L21-L38) | `/session`, `/title`, with `unattended` for the mark of a process |
| `PermissionRequest`, `TurnRequest` | [40-57](../src/protocol.ts#L40-L57) | `/permission`, `/turn` |
| `AttachLine` | [59](../src/protocol.ts#L59) | `{prompt}` and `{permission}` lines of `/attach` |
| `TitleReply`, `SessionReply`, `AttachRequest`, `CallRequest`, `CallLine` | [64-85](../src/protocol.ts#L64-L85) | replies and `/call` |
| `StatusReply`, `ResetReply` | [87-95](../src/protocol.ts#L87-L95) | `/status`, `/reset-owners` |

#### [`service.ts`](../src/service.ts)

| Symbol | Line | Contents |
|---|---|---|
| `HubLike`, `SessionLike`, `ServiceDeps` | [26-56](../src/service.ts#L26-L56) | interfaces that the service uses, so that tests can replace the hub |
| `Entry` | [58](../src/service.ts#L58) | opening promise, session, open attachments, their writers, running calls, idle timer, the session server's parent process ID and the time it was last seen |
| `Service.start` | [93](../src/service.ts#L93) | The lock is taken, and the stored sessions are read with their aliases and the next number. The listener starts before the hub opens, so that a starter that lost the lock race gets an answer to `/status` right after `running`. After the hub opens, the stop timer starts, the titles held at the last stop are taken as new `/title` requests, and the hourly log trim starts. |
| `Service.routes`, `withHub`, `expiring` | [137-175](../src/service.ts#L137-L175) | the ten routes. Requests wait for the hub, and due actions expire first. |
| `Service.shutdown` | [177](../src/service.ts#L177) | stop sequence; the held titles saved |
| `onSession`, `onTitle`, `markUnattended`, `refuseUnattended`, `applyTitle` | [201-262](../src/service.ts#L201-L262) | The options are checked. For a title with `unattended`, the service removes the marks of 10 s ago and older, marks the title's process, and ends a server's wait for the title of that process. It opens no session for such a title. The service refuses a registration from a process marked in the last 10 s with 403, before and after its wait. At a registration, the service records the parent process ID, names the session by a held title of its ID, and signals the registration. The service holds a title of a new ID that comes from a process without a known session, or from a process whose title is held. The title is held until the server of that process registers, 10 s at most, and is applied only while the service runs. When a title is applied, the service opens or creates the session, sets its name, passes its model with the title's `at`, and records the process of the title. |
| `canonical`, `resolve`, `attachedSession`, `processSession`, `alias` | [264-295](../src/service.ts#L264-L295) | An alias is resolved to its session. With `session.json`, an ID is resolved to itself, or, for a hook, to the session attached by its Claude Code process, without an alias. For any other ID, a session server first waits for the title of its process. The ID is then resolved to the session of the same Claude Code process, when the process has one, and recorded as its alias in `session.json`. |
| `rename` | [297](../src/service.ts#L297) | A new name of the session is written into `session.json`, as read after the group's rename. |
| `onAttach`, `onCall`, `hold` | [304-356](../src/service.ts#L304-L356) | attachment with the session's line writer; call stream; streams refused while stopping |
| `entry`, `refuseWhenStopping`, `folder`, `openAgent` | [357-402](../src/service.ts#L357-L402) | the session registry. The status is 503 while the service stops, and 400 for an ID that is not a session ID. The status is 409 for an unknown session on the routes other than `/session` and `/title`. A new session is given a new number and name, and the aliases that already point to it. |
| `scheduleIdle`, `scheduleStop` | [404-422](../src/service.ts#L404-L422) | idle close of a session, idle stop of the service |
| `runService` | [430](../src/service.ts#L430) | the process entry: ROOT, start, ready line and signal handlers. For a held lock, the start runs again once the lock is released, and the ready line is `running` when its holder answers. |
| `productionDeps` | [456](../src/service.ts#L456) | token, `Hub.open` |
| `processEvent` | [466](../src/service.ts#L466) | the next event of a PID on `titled` or `registered`, 10 s at most |
| `lockHolder` | [478](../src/service.ts#L478) | up to 150 checks, 100 ms apart. The result is `released` when ROOT/service.lock is gone, and `answers` for any result of a socket request other than ENOENT and ECONNREFUSED. |
| `sessionName` | [493](../src/service.ts#L493) | session title, else the project folder name, else `Claude Code` |
| `validConfig` | [487](../src/service.ts#L487) | the check of `config` in `/session` and `/title`: a valid Main group name, the mode, and hours from 1 to 23. Other values are refused with status 400. |
| `readyChannel`, `writeReady` | [499](../src/service.ts#L499), [507](../src/service.ts#L507) | ready line to fd `SIMPLEX_MCP_READY_FD`, or stdout. A ready line to a reader that is gone, `EPIPE`, is dropped, and the service keeps running. |

#### [`hub.ts`](../src/hub.ts)

| Symbol | Line | Contents |
|---|---|---|
| constants | [19-29](../src/hub.ts#L19-L29) | profile name `Claude`, session data key, first action number, 10 s stop wait, 60 s echo of a raw prompt, 20 pushed prompts, 10 minutes for a prompt's file, 1 day for a pending connection, 30 s of held prompts after the hub opens, the channel tags of this plugin, the one reply to the `/` commands of a member who is not an admin or owner |
| `hubTexts` | [31](../src/hub.ts#L31) | refusal texts of a lost session group, of a stop and of the limit of pending actions, which names the approvers |
| `SessionGroupData` | [38-42](../src/hub.ts#L38-L42) | session number; pending connections of `connect` with their time; the approved mode |
| `Session` | [44](../src/hub.ts#L44) | one Claude Code session, built from its `session.json` and its group's data. `Actions` get the hub's texts and the mode of the group's data, or `requests` without one. `AgentTools` get the session's ID and folder and the group for `reply`. `Control` of the session group gets the session's approver checks. The stored model and effort are kept as well. |
| `Session.busy` | [123](../src/hub.ts#L123) | an action other than a `mode` card in memory |
| `Session.requestMode`, `askMode` | [136-155](../src/hub.ts#L136-L155) | `requests` applies at once. The hub then ends pending `mode` cards and clears `hostDeclined`. `host` is remembered and asked as a `mode` action while an approver is in the group and `hostDeclined` is unset. The request is forgotten after a rejected or expired card, and `hostDeclined` is set after a rejection. |
| `Session.onLine` | [114-121](../src/hub.ts#L114-L121) | the writer of the session's attachments. The held prompts are woken at each change. |
| `Session.onMessage` | [157-164](../src/hub.ts#L157-L164) | The hub answers a `/` command from a member who is not an approver with the one refusal, found by `refusesCommand` and posted on the `delivery` chain. The remaining one-line `/` texts go to `Control`, and other messages go to `prompt` on the `delivery` chain, in arrival order. |
| `Session.claimPending` | [166](../src/hub.ts#L166) | When a connection of `connect` recorded in the last day completes, its record is removed and its chat is claimed. |
| `Session.permission` | [173](../src/hub.ts#L173) | a `permission` action in mode `requests`. Its verdict is written to the attachments, and the action is removed when decided. |
| `Session.turn` | [184](../src/hub.ts#L184) | Before any await, at an answer or a prompt that is not mid-turn, the permissions created before its hook started are ended. An answer's quote is chosen, and its hook's start is recorded as the latest answer's hook start. A turn record is added for a prompt whose hook started no earlier than the latest answer's hook, unless the prompt is mid-turn and matched no pushed prompt. The model and the effort level of the request are taken only when its `at` is not older than the `at` of the value taken last, kept apart for each ([`newer`](../src/hub.ts#L265-L269)). A `/title` passes its model with the `at` of its SessionStart hook. A request without `at` counts as the current time. A changed description is recorded in `session.json`. After these steps, Claude posts the request's `note` without a quote, and `User` posts the terminal prompt unless its `typed` is false, or it is blank, made only of this plugin's channel tags, pushed or a command. A prompt without `typed` is posted as typed. The turn record of a prompt is kept whatever its `typed`. Claude posts the answer unless it is blank. The changed short description is set, even after a failed post. A prompt, a note or an answer that the core refuses for a group preference is replaced by Claude's post with the preference's name. |
| `Session.endPermissions`, `fileDone` | [208-219](../src/hub.ts#L208-L219) | the end of pending permissions created before a time; the end of a prompt's file |
| `Session.invite`, `announceRights` | [221-232](../src/hub.ts#L221-L232) | invitation through the Main group. At a member's join or role change, other than `User`'s, the rights of the member's role are posted. The `/observer` command is added only for a role that can send, other than owner ([230](../src/hub.ts#L230)). |
| `Session.prompt` | [234-237](../src/hub.ts#L234-L237) | a message of `User` dropped; the rest delivered |
| `Session.deliver`, `receive`, `turnQuote`, `answer`, `postRefusal`, `post`, `pushedIn` | [239-306](../src/hub.ts#L239-L306) | `deliver` waits for the held prompts, and then receives the file while the session is attached. It writes the prompt line with the file's path. After a failure of the file, the line is written without the path, and a reply is posted. Without an attachment, the reply is `Session N is not running.` `receive` receives the file into the session's folder within 10 minutes. `turnQuote` returns the newest turn record before an answer, or else the oldest pushed prompt not quoted yet, which it marks quoted. `answer` posts the answer in parts, with its quote when the quote fits with the answer's first character. `postRefusal` replaces a post that the core refuses with `feature not allowed NAME` by the subject and the text with the preference's name. The text is made by `featureOff` for an answer, and by `notAllowedForUser` for a prompt of `User`. `post` sends a post to the group, and sends it again without its quote when the core refuses the quote. `pushedIn` returns the pushed prompts contained in a turn's prompt. |
| `Session.queue`, `refusesCommand` | [308-314](../src/hub.ts#L308-L314) | the `delivery` chain; a one-line `/` text of a member other than `User` who is not an approver |
| `Session.approves`, `isUser`, `setMode`, `declineHost`, `store`, `savePending` | [316-344](../src/hub.ts#L316-L344) | approver check: not `User`, the role `admin` or `owner` in the session group; the member of the contact `Hub.userContactId`; the mode and its record in the group's custom data; `hostDeclined` and its record; fields written into `session.json`; a pending connection recorded, those older than a day dropped |
| `Hub.userContactId` | [355](../src/hub.ts#L355) | the hub's contact marked `simplexMcpUser`, read at the start |
| `Hub.open` | [372](../src/hub.ts#L372) | core `_hub`, Main group found or created, `MainGroup` load and start, `User` unless it fails, the contact of `User`, the stored sessions, events |
| `Hub.configure`, `resetOwners` | [401-409](../src/hub.ts#L401-L409) | expiry of all sessions and the Main group name. The Main group is replaced at once, even while owners exist. |
| `Hub.session` | [411](../src/hub.ts#L411) | the open session, renamed when its name changed, or a new `Session` with its stored or new group. The owners are invited to a new group. |
| `Hub.sessionsText`, `join`, `inviteOwner` | [436-458](../src/hub.ts#L436-L458) | `SessionGroups` of the Main group |
| `Hub.stop` | [464](../src/hub.ts#L464) | actions stopped, events awaited, cancelled cards awaited up to 10 s, cores closed |
| `storedGroup`, `createSessionGroup` | [477-496](../src/hub.ts#L477-L496) | the group of `session.json` with a matching session number; a new group with `User` added |
| `Hub.onEvent` | [498](../src/hub.ts#L498) | event dispatch. Pending connections are claimed at `contactConnected` and `userJoinedGroup`. A session group's membership events go to its `Control`, `announceRights` and `askMode`, one at a time. |
| `notAllowedForUser` | [530](../src/hub.ts#L530) | `NAME are not allowed for User in the group of this session; its owners can allow them in the group's preferences.` It is posted for a prompt of `User` refused by a group preference, including one that is on only for roles above that of `User`. |
| `textParts` | [538](../src/hub.ts#L538) | the longest runs of whole characters that each fit in one message by `fitsMessage`, the first with the given quote. Each part is found by a binary search among the next `maxMessageLength` characters. |
| `isRefusedQuote` | [557](../src/hub.ts#L557) | `errorStore` `invalidQuote`, `chatItemNotFound` or `largeMsg` |

#### [`main-group.ts`](../src/main-group.ts)

| Symbol | Line | Contents |
|---|---|---|
| `mainCommands`, texts | [8-18](../src/main-group.ts#L8-L18) | command menu; greeting, reply to an unknown command, log line; the welcome message, the text of the join post and the short description |
| `SessionGroups` | [20](../src/main-group.ts#L20) | what the Main group asks of the hub |
| `MainGroup.owners` | [53-55](../src/main-group.ts#L53-L55) | human members with role `owner` while the group is not lost |
| `MainGroup.load`, `start` | [57-76](../src/main-group.ts#L57-L76) | lost check, name and menu from the group's current profile, and members. The short description and welcome message are kept. Without owners, `start` takes the existing link, or creates a new one. |
| `MainGroup.onMessage`, `command` | [83-115](../src/main-group.ts#L83-L115) | `/sessions`, `/join N`; `Unknown command.` for another |
| `MainGroup.onEvent`, `handleEvent`, `ownerJoined` | [95-140](../src/main-group.ts#L95-L140) | The members are read again. The first human who joins becomes an owner. A new link is created when no owner remains. The statement of trust is posted at each human's join. New owners are invited to the open sessions. |
| `MainGroup.replace` | [99-109](../src/main-group.ts#L99-L109) | The Main group is deleted for all its members, and a new one is created with a new link. The session groups are kept. |
| `openLink`, `deleteLink` | [147-160](../src/main-group.ts#L147-L160) | group link with role `member` |
| `findMainGroup`, `createMainGroup` | [185-205](../src/main-group.ts#L185-L205) | the newest group with `simplexMcpMain`; a new one with the short description and welcome message of trust, history, direct messages for owners and the menu |

#### [`user.ts`](../src/user.ts)

| Symbol | Line | Contents |
|---|---|---|
| `User.open` | [16](../src/user.ts#L16) | core `_user` with profile `User`, events, a connection to `Claude` when the hub has no contact marked `simplexMcpUser` |
| `User.post` | [29](../src/user.ts#L29) | messages to the group marked with the session number |
| `User.connect` | [38](../src/user.ts#L38) | a one-time invitation of the hub, joined by `User`. The `contactConnected` of that invitation's connection is awaited for 60 s, and the hub's contact is then marked. |
| `User.onEvent` | [50](../src/user.ts#L50) | The contact is marked `simplexMcpClaude`. Invitations of that contact to a group named `N · ...` are joined and marked. |
| `userContact` | [60](../src/user.ts#L60) | the hub's contact marked `simplexMcpUser` |

#### [`core.ts`](../src/core.ts)

| Symbol | Line | Contents |
|---|---|---|
| `openCore` | [19](../src/core.ts#L19) | folder, `ChatApi.init` with prefix `FOLDER/simplex`, active user or a new one with peer type `agent`, `prepareCore`, event buffer, `startChat`, preset contact deleted for a new user, profile update |
| `deletePresetContacts` | [44](../src/core.ts#L44) | the contact `Ask SimpleX Team`, deleted when it has no connection |

#### [`hook.ts`](../src/hook.ts), [`owner.ts`](../src/owner.ts), [`qr.ts`](../src/qr.ts)

| Symbol | Line | Contents |
|---|---|---|
| `sessionStart` | [hook.ts:81](../src/hook.ts#L81) | `{}` for a run that is not interactive. When the run is not interactive only because of `CLAUDE_CODE_SESSION_ATTENDED`, its process is marked first. In an interactive run, the hook checks Node.js, reads the options, calls `ensureService`, sends `POST /title` with the title, the folder, the model and its own start time `performance.timeOrigin`, and returns the link message. |
| `markUnattended` | [hook.ts:102](../src/hook.ts#L102) | When `CLAUDE_PID` is set, the hook calls `ensureService` and sends `POST /title` with `unattended`. A failure is logged. |
| `turn` | [hook.ts:114](../src/hook.ts#L114) | The output is `{}`. In a run that is not interactive, the hook sends nothing. Otherwise it calls `ensureService`, then sends `POST /turn` with the fields of [8.6](#86-channel-and-mirroring). The request is tried up to 15 times, 1 s apart, while the service returns status 409. |
| `answerModel`, `promptKind`, `findInTranscript`, `readTail`, `lastAnswer`, `promptRecord`, `isPrompt`, `recordText`, `recordKind` | [hook.ts:137-222](../src/hook.ts#L137-L222) | `answerModel` finds the model of Stop's answer. It is the `message.model` of the last entry of type `assistant` without `isSidechain` in the tail of `transcript_path`, once that entry's text is the answer. No model is sent for `<synthetic>`, an answer without text, an empty path, an unreadable transcript, or an entry not written within the reads. `promptKind` finds whether a person typed the prompt of UserPromptSubmit, its note and whether it is mid-turn. Its record, found by `promptRecord`, is the newest user entry or `queued_command` attachment whose text is the prompt, without subagent entries and tool results: the entry with the hook's `prompt_id` as `promptId`, or a record after the first user entry with that ID, a tool result included. Until such an entry is in the tail, `promptRecord` waits; at the last read, and at once without `prompt_id`, it takes any record with the prompt's text. It also waits while an `enqueue` entry with the prompt's text follows the record. The prompt is typed when the record's `origin.kind` is `human`, and mid-turn when the record is not the entry with `prompt_id`. A `queued_command` attachment without origin and with the `commandMode` `task-notification` is a task notification. At the last read, a record with an `enqueue` entry after it is classified by its origin, and a typed one is mid-turn. Without a record, an `enqueue` entry with the prompt's text makes the prompt typed and mid-turn; otherwise an entry with `prompt_id` and the origin `human` makes it typed. `findInTranscript` reads the tail, the last 1 MiB plus twice the bytes of the answer or the prompt, 100 ms apart: at most 20 times for an answer and 600 times for a prompt, and it tells its search which read is the last. It treats a failed read, as of a missing file, as an empty tail. |
| `noteOf`, `taskNote`, `teammateNote`, `idleNotification`, `peerName`, `firstLine`, `decoded`, `stringOf`, `shortened` | [hook.ts:224-290](../src/hook.ts#L224-L290) | `noteOf` makes the note of a record from its origin, its `turnOrigin` and the prompt, as [api.md](api.md#48-plugin-options-files-and-hook-output) lists the lines. `taskNote` reads `<summary>`, `<event>` and `<status>` of the first `<task-notification>` block, and takes the first line of a prompt without one. `teammateNote` reads the sender, the `summary` and the message of the first `<teammate-message>` tag of a prompt without origin whose message is not an idle notification; `idleNotification` checks whether a message's JSON `type` is `idle_notification`. `firstLine`, `peerName` and `teammateNote` replace control and format characters with spaces in each first line and in the sender's name, as `oneLine` does, so a note is one line. `shortened` cuts a note to 200 characters, counted in code points. |
| `linkCommand`, `resetCommand` | [owner.ts:7-8](../src/owner.ts#L7-L8) | `npx -y @simplex-chat/mcp@VERSION link` and `reset-owners`, VERSION of this package |
| `linkText`, `resetOwnersText` | [owner.ts:10-20](../src/owner.ts#L10-L20) | the Main group's name with the owners, or with the link and a QR code on a terminal only |
| `showLink`, `resetOwners` | [owner.ts:22-33](../src/owner.ts#L22-L33) | `link` takes the link from `GET /link` while no owner is in the Main group. `reset-owners` replaces the Main group at once, and shows the new link on a terminal only. |
| `qrCodeText` | [qr.ts:14](../src/qr.ts#L14) | `small` and `large` with ANSI colors for terminals, `plain` for the hook message |

#### [`config.ts`](../src/config.ts), [`files.ts`](../src/files.ts)

| Symbol | Line | Contents |
|---|---|---|
| `interactiveEntrypoint`, `interactive` | [config.ts:27-33](../src/config.ts#L27-L33) | `false` for `CLAUDE_CODE_ENTRYPOINT` `sdk-cli`, `sdk-ts` or `sdk-py`. `interactive` is `false` for `CLAUDE_CODE_SESSION_ATTENDED` `0` as well. |
| `Config`, `defaultConfig`, `configVariables` | [config.ts:4-18](../src/config.ts#L4-L18) | the Main group name, the mode and the expiry hours; their defaults `Claude Control`, `requests` and 23; their variables |
| `readConfig` | [config.ts:35](../src/config.ts#L35) | `CLAUDE_PLUGIN_OPTION_*` to `Config`, defaults for blank values, `ConfigError` texts |
| `withMachine` | [config.ts:50](../src/config.ts#L50) | ` - MACHINE` from `SIMPLEX_MCP_MACHINE` after the Main group name, checked by `validName`, with the `ConfigError` texts of the variable |
| `dataRoot`, `hubFolder`, `sessionFolder` | [files.ts:31-47](../src/files.ts#L31-L47) | ROOT, `ROOT/_hub`, `ROOT/_sessions/<lower-case ID>` |
| `SessionFile`, `readSessionFile`, `writeSessionFile`, `storedSessions` | [files.ts:11-72](../src/files.ts#L11-L72) | `session.json` with number, name, project, creation time, group, aliases, model, effort and a rejected mode `host`. An unparsable file is skipped with a log line. Each write goes to `session.json.tmp`, which is renamed into place. |
| `socketPath` | [files.ts:74](../src/files.ts#L74) | `ROOT/service.sock`. A path over 103 bytes is replaced by `TMP/simplex-mcp-HASH.sock`, and Windows uses `\\.\pipe\simplex-mcp-HASH`. |
| `logFile`, `trimLog`, `openLog` | [files.ts:81-101](../src/files.ts#L81-L101) | `ROOT/service.log`, emptied when over 10 MB, mode 0600 |
| `serviceLock`, `serviceToken` | [files.ts:85-89](../src/files.ts#L85-L89) | `service.lock` and `ROOT/service.token`, the plugin's lock and token. Both names are written with a dot, which `serve` refuses in agent names, so the folders of agents in `~/.simplex-mcp` are not touched. |

#### Plugin files

| File | Contents |
|---|---|
| [`.claude-plugin/plugin.json`](../.claude-plugin/plugin.json) | plugin `simplex`, version, description, `userConfig` |
| [`.mcp.json`](../.mcp.json) | server `simplex`: `node dist/cli.js session` in the plugin's folder, tool call timeout, options as environment variables |
| [`hooks/hooks.json`](../hooks/hooks.json) | SessionStart: `dist/cli.js hook session-start`; PreToolUse: `allow.js`; UserPromptSubmit, Stop, PostModelSwitch: `dist/cli.js hook turn`, in the background |
| [`package.json`](../package.json) | `bundleDependencies: true`. The production packages are bundled in the package's `node_modules`, without a lockfile, so Claude Code installs nothing in the plugin's folder. [`package-lock.json`](../package-lock.json), the tree of `npm ci`, is not packed. |
| [`plugin/allow.js`](../plugin/allow.js) | PreToolUse allow decision |
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
| Sessions | `Hub.sessions`, by number ([hub.ts:357](../src/hub.ts#L357)) | `Hub.session` | events, Main group commands |
| End of the held prompts | `Hub.promptsHeldUntil`, 30 s after the stored sessions opened ([hub.ts:356](../src/hub.ts#L356), [:383](../src/hub.ts#L383)) | `Hub.open` | `Session.deliver` |
| Prompts in delivery | `Session.delivery`, a promise chain in arrival order ([hub.ts:62](../src/hub.ts#L62)) | `onMessage` | `deliver` |
| Actions of a session | `Session.actions`, numbered from 1 per session group ([hub.ts:46](../src/hub.ts#L46), [:77](../src/hub.ts#L77)) | tools, `permission`, `requestMode` | `Control`, `busy` |
| Pushed prompts | `Session.pushed`, at most 20, each kept until it is matched by a turn's prompt. An entry is marked `quoted` once it is quoted by an answer without a turn record ([hub.ts:52](../src/hub.ts#L52)). | `deliver`, `turnQuote` | `turn`, `turnQuote` |
| Files of prompts in transfer | `Session.receiving` ([hub.ts:53](../src/hub.ts#L53)) | `receive`, its 10-minute timer | `fileDone` |
| Model and effort | `Session.description`, also in `session.json` ([hub.ts:54](../src/hub.ts#L54)) | constructor, `turn` | `turn` |
| Turn records | `Session.turns`, the start time of each prompt hook that started after the latest answer's hook, with its pushed prompt, or `null` for a terminal prompt or a command ([hub.ts:56-57](../src/hub.ts#L56-L57)) | `turn`. At an answer, the records that started before its hook are removed. | `turnQuote` |
| A rejected mode `host` | `Session.hostDeclined`, also in `session.json` ([hub.ts:59](../src/hub.ts#L59)) | constructor, `declineHost` | `askMode`, `requestMode` |
| The requested mode | `Session.requested` ([hub.ts:58](../src/hub.ts#L58)) | `requestMode`, a rejected or expired `mode` card | `askMode` |
| Members of the Main group, its link | `MainGroup` ([main-group.ts:28-31](../src/main-group.ts#L28-L31)) | `refreshMembers`, `openLink` | owner checks, the statement of trust at a join, `/status`, `/title`, `/link` |

A session process keeps one connection promise, one attachment, the refusal of the service, and the retry delay with its timer in memory ([session.ts:69-77](../src/session.ts#L69-L77)).

Chat database custom data is in the core `_hub` unless noted:

| Item | Entity | Value | Written by | Read by |
|---|---|---|---|---|
| Main group marker | Main group | `{simplexMcpMain: true}` | [`createMainGroup`](../src/main-group.ts#L195-L205) | [`findMainGroup`](../src/main-group.ts#L185-L188) |
| Session group | group of session N | `{simplexMcpSession: {session: N, pending?: [{connId, at}], mode?}}`, where `mode` is the approved mode | [`createSessionGroup`](../src/hub.ts#L484-L496), [`setMode`](../src/hub.ts#L324-L327), [`savePending`](../src/hub.ts#L340-L344), [`claimPending`](../src/hub.ts#L166-L171) | [`storedGroup`](../src/hub.ts#L477-L482), [`Session` constructor](../src/hub.ts#L77), [`claimPending`](../src/hub.ts#L166-L171) |
| Action counter | group of session N | `{simplexMcp: {control: true, nextAction}}`, merged with the other keys | [`saveCounter`, `saveData`](../src/control.ts#L250-L262) | [`Session` constructor](../src/hub.ts#L77) |
| Chat owner | contact or group of a peer | `{...other keys, simplexMcpOwner: {session: ID}}` | [`claim`](../src/tools.ts#L589-L593), [`giveChat`](../src/tools.ts#L576-L579) | [`owns`](../src/tools.ts#L585-L587) |
| Standing approvals | contact or group of a peer | as in [3.2](#32-chat-database-custom-data) | `Control` of the owning session's group | the same |
| User contact | the hub's contact of `User` | `{simplexMcpUser: true}` | [`User.connect`](../src/user.ts#L38-L48) | [`userContact`](../src/user.ts#L60-L62) |
| Claude contact | in `_user`: the contact of `Claude` | `{simplexMcpClaude: true}` | [`User.onEvent`](../src/user.ts#L51) | [`User.onEvent`](../src/user.ts#L52) |
| Session of a group | in `_user`: a session group | `{simplexMcpSession: N}` | [`User.onEvent`](../src/user.ts#L56) | [`User.post`](../src/user.ts#L29) |

Files:

| Path | Contents | Written | Read |
|---|---|---|---|
| `ROOT/` | data root, mode 0700 | every client and the service ([client.ts:28](../src/client.ts#L28), [service.ts:435](../src/service.ts#L435)) | |
| `ROOT/service.token` | the bearer token, 0600 | the first client or service ([files.ts:87-89](../src/files.ts#L87-L89), [:124-134](../src/files.ts#L124-L134)) | every client, the service |
| `ROOT/service.lock` | PID of the service, linked from `ROOT/service.lock.PID.HEX.tmp`. A stopped service's lock is claimed through `ROOT/service.lock.PID.claim`, which is removed after the takeover. | `Service.start` ([service.ts:94](../src/service.ts#L94), [files.ts:182-221](../src/files.ts#L182-L221)) | the next service |
| `ROOT/titles.json` | the `/title` requests held at a stop, each with its `at`, 0600 | `shutdown` ([service.ts:180](../src/service.ts#L180)) | the next `Service.start`, which removes it ([service.ts:106](../src/service.ts#L106)) |
| `ROOT/service.sock` | the socket | `listen` ([socket.ts:40-43](../src/socket.ts#L40-L43)) | clients |
| `ROOT/service.log` | the service's stdout and stderr, emptied over 10 MB at a spawn and every hour | `spawnService` ([client.ts:76-84](../src/client.ts#L76-L84)), [`trimLog`](../src/files.ts#L91-L94) | people |
| `ROOT/_hub/`, `ROOT/_user/` | databases and `tmp/` of the profiles `Claude` and `User` | [`openCore`](../src/core.ts#L19-L35) | the cores |
| `ROOT/_sessions/ID/session.json` | `{number, name, project, created, group?, aliases?, model?, effort?, hostDeclined?}`, 0600, replaced whole through `session.json.tmp` | `openAgent`, `rename`, `alias`, `Hub.session`, `declineHost`, `store` ([service.ts:392-402](../src/service.ts#L392-L402), [:297-302](../src/service.ts#L297-L302), [:290-295](../src/service.ts#L290-L295), [hub.ts:421](../src/hub.ts#L421), [:329-338](../src/hub.ts#L329-L338)) | `Service` constructor, `resolve`, `Hub.session`, `alias`, `store` |
| `ROOT/_sessions/ID/files/` | downloads of the session's chats as in [3.3](#33-files). The files of the prompts from the group are in `group/`. `sent/` is deleted when the session opens. | the core ([hub.ts:255-263](../src/hub.ts#L255-L263), [:418](../src/hub.ts#L418)) | tools, Claude |

### 8.4 Process lifecycle

#### Session start

1. The SessionStart hook checks Node.js 22, reads the options, starts the service when it is not running, and sends `POST /title` with the session ID, `CLAUDE_PID`, its start time `at`, title, project folder, model and options ([hook.ts:81-100](../src/hook.ts#L81-L100)). In a run that is not interactive, the hook returns `{}`. When the run is not interactive only because `CLAUDE_CODE_SESSION_ATTENDED` is `0`, the hook first sends `POST /title` with `unattended`. Then the service refuses `/session` from that `CLAUDE_PID` with 403 for 10 s ([flow 30](flows.md#30-a-run-that-is-not-interactive)).
2. `/title` resolves the ID to a session as in [API 4.6](api.md#46-socket-protocol). So `/clear`, `/resume` and `--continue` keep the session of the running Claude Code process. The service gives an alias only to an ID without `session.json`. For a new ID from a process without a known session, `/title` replies first, and then waits up to 10 s for the server of that process to register. `/title` then opens the session from `session.json`, or with a new number and the name from the title or the folder. `Hub.session` finds the group of `session.json` or creates one. It adds `User`, invites the Main group's owners and loads the group's `Control` ([service.ts:218-262](../src/service.ts#L218-L262), [:392-402](../src/service.ts#L392-L402), [hub.ts:411-430](../src/hub.ts#L411-L430)).
3. At a new title, the service renames the group and `session.json`. The model, when Claude Code passes it, is written to the group's short description with the hook's `at`, so a model of a hook that started later stays ([service.ts:260-261](../src/service.ts#L260-L261)). The Stop hook of each turn sends the model of its answer from the transcript. So the model is shown in the short description from the first answer, even when the SessionStart hook has none ([hook.ts:118](../src/hook.ts#L118), [:137-145](../src/hook.ts#L137-L145)).
4. The hook prints the link message while no owner is in the Main group, and `{}` otherwise.
5. The session process connects in the background. `/session` resolves its ID, and waits up to 10 s for the title of its process when the ID is new. It then registers the options and the parent process, and requests the mode ([service.ts:201-216](../src/service.ts#L201-L216)). `/attach` sets the session's line writer ([:304-324](../src/service.ts#L304-L324)). When `/attach` ends or the connection fails, the session process connects again, except after a `ProtocolError` or a 403 of `/session` ([session.ts:88-112](../src/session.ts#L88-L112)).

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

1. `ROOT` is created, and the lock is taken. When another running process holds the lock, the service checks the lock for up to 15 s ([service.ts:430-442](../src/service.ts#L430-L442), [:478-485](../src/service.ts#L478-L485), [:93-96](../src/service.ts#L93-L96)):
   - After the lock is released, the start runs again.
   - If the socket answers, the ready line is `running`.
   - Otherwise the ready line is `error: Another service uses ROOT (PID N). If no service runs, remove ROOT/service.lock.`, and the process exits with code 1.
2. The stored sessions are read with their aliases, and the next number is set ([service.ts:87-91](../src/service.ts#L87-L91)).
3. The service listens on the socket before the hub opens, and requests wait for the hub ([service.ts:97-98](../src/service.ts#L97-L98), [:163-168](../src/service.ts#L163-L168)).
4. The service reads the options in `openHub` ([service.ts:126-129](../src/service.ts#L126-L129)). `Hub.open` then opens the core `_hub` with profile `Claude`, and finds the Main group by its marker or creates it. It runs `MainGroup.load` and `start`, with a link while no owner is in the Main group. The core `_user` opens, and connects to `Claude` at the first start. A `Session` opens for each stored session, prompts are held for 30 s, and events are dispatched ([hub.ts:372-390](../src/hub.ts#L372-L390)).
5. When the hub fails to open, the listener closes and the lock is released. The service answers the requests that waited with status 500 ([service.ts:99-118](../src/service.ts#L99-L118)).
6. The stop timer and the hourly log trim start. The titles held at the last stop are read from `ROOT/titles.json` and taken as new `/title` requests. The service logs `SimpleX MCP service VERSION, data folder ROOT` and writes the ready line `ready` ([service.ts:105-113](../src/service.ts#L105-L113), [:444-445](../src/service.ts#L444-L445)).

#### Event dispatch

After a stop, `Hub.onEvent` ignores events ([hub.ts:498-528](../src/hub.ts#L498-L528)). Otherwise it runs `expireDue` of every session first, and then dispatches the event:

| Event | Condition | Handler |
|---|---|---|
| `newChatItems` | item of the Main group | [`MainGroup.onMessage`](../src/main-group.ts#L83) |
| `newChatItems` | item of a session group | [`Session.onMessage`](../src/hub.ts#L157) |
| `rcvFileComplete`, `rcvFileError` | | [`Session.fileDone`](../src/hub.ts#L214) of every session |
| `contactConnected`, `userJoinedGroup` | a connection ID | [`Session.claimPending`](../src/hub.ts#L166) of every session |
| any event | | [`AgentTools.onEvent`](../src/tools.ts#L673) of every session |
| `sndFileError` | a chat of the session, or its group | [`Control.fileNotSent`](../src/control.ts#L283) |
| membership events | of the Main group | [`MainGroup.onEvent`](../src/main-group.ts#L95), queued in the Main group and not awaited by the event loop. It posts the statement of trust at a join ([flow 28](flows.md#28-a-member-joins-the-main-group-and-a-sessions-group-and-a-person-is-added-to-it)). |
| membership events | of a session group | [`Control.onEvent`](../src/control.ts#L306), then [`Session.announceRights`](../src/hub.ts#L225) and [`Session.askMode`](../src/hub.ts#L144), queued |

#### Attach, idle close and idle stop

- The service cancels the session's idle timer at each attachment. When the last attachment ends, the service records the time, clears the line writer, ends the pending permissions and starts the timer ([service.ts:304-324](../src/service.ts#L304-L324)).
- After 10 minutes, the idle timer closes the entry when no call of the session runs and none of its actions is in memory. Otherwise the timer starts again ([service.ts:404-414](../src/service.ts#L404-L414)). The hub keeps the `Session` of a closed session, so Claude still answers `/list` and `/revoke` in its group ([hub.ts:411-416](../src/hub.ts#L411-L416)).
- When no entry remains, the stop timer stops the service after 10 minutes. The timer is cleared when a new entry opens ([service.ts:416-422](../src/service.ts#L416-L422), [:362](../src/service.ts#L362)).

#### Stop

On `SIGINT`, `SIGTERM`, `POST /stop` or the stop timer ([service.ts:177-199](../src/service.ts#L177-L199)):

1. Timers are cleared, the held titles are saved in `ROOT/titles.json`, the listener closes, and open streams are destroyed.
2. `Hub.stop` stops the actions of every session, and edits their cards to `Cancelled: the server stopped.` It awaits the queued group events, and gives the cancelled cards 10 s to be sent. Then the download checks stop, and the cores `_user` and `_hub` close ([hub.ts:464-475](../src/hub.ts#L464-L475)).
3. The service logs `Cancelled K action(s).`, releases the lock and logs `Stopped.` The process exits after the `/stop` reply is sent, or after its client disconnects before the reply is written.

### 8.5 Approval model

| Rule | Code |
|---|---|
| Rights in a session group are set by the member's role there, however the member joined. An approver is an active member other than Claude and `User`, with the role `admin` or `owner`. Approvals and tool-use decisions are accepted only from approvers. | [hub.ts:316-322](../src/hub.ts#L316-L322), [control.ts:50](../src/control.ts#L50), [:700-702](../src/control.ts#L700-L702), [:709-718](../src/control.ts#L709-L718) |
| Prompts are accepted from every member except `User`, and the group's `/` commands only from approvers. The other members get one reply to each one-line `/` text. | [hub.ts:157-164](../src/hub.ts#L157-L164), [:234-237](../src/hub.ts#L234-L237), [:312-318](../src/hub.ts#L312-L318) |
| With `/join N`, a member of the Main group is invited as `admin`, and an owner of the Main group as `owner`. Owners invited at a session start or at an owner's join get the role `owner`. | [hub.ts:445](../src/hub.ts#L445), [:427](../src/hub.ts#L427), [:456](../src/hub.ts#L456) |
| With `/observer #G ID`, the sender sets the role `observer` of a current member. The sender's role must be admin or owner, and not below the member's role, as SimpleX requires. For a member with status `pending_approval` or `pending_review`, Claude replies `NAME's role can't be changed.` For a member who is neither current nor pending, such as an invited member who has not joined, the reply is `No member ID in this group.` An observer neither prompts nor approves. From the reply on, the standing approvals that the observer gave are not used while that member is not an admin or owner. | [control.ts:488-500](../src/control.ts#L488-L500), [:1015-1017](../src/control.ts#L1015-L1017), [:241-248](../src/control.ts#L241-L248), [tools.ts:1562-1564](../src/tools.ts#L1562-L1564) |
| A new action of a session is refused while its group has no approver | [control.ts:230-231](../src/control.ts#L230-L231) |
| Changes of membership or role in the Main group, and its replacement by a reset of the owners, leave the session groups as they are | [main-group.ts:117-132](../src/main-group.ts#L117-L132), [:99-109](../src/main-group.ts#L99-L109) |
| Everyone in the Main group is trusted with all sessions. This is stated in the group's short description and welcome message, set when the group is created, and in Claude's post at each join. In a session group, Claude posts the member's rights at each join and each role change, with the `/observer` command only for a role that can send, other than owner. The observer's rights are stated in Claude's reply to `/observer`. | [main-group.ts:16-18](../src/main-group.ts#L16-L18), [:195-205](../src/main-group.ts#L195-L205), [:127-128](../src/main-group.ts#L127-L128), [hub.ts:225-232](../src/hub.ts#L225-L232), [control.ts:499](../src/control.ts#L499), [:1023-1026](../src/control.ts#L1023-L1026) |
| A file of `reply` is sent at once, in both modes. It can be refused by the group's preference for files and media, which owners set and the core enforces. A post of Claude or `User` in a session group, when refused by a group preference, is replaced by Claude's post with the preference's name, and is never dropped without one ([tools.ts:1620-1627](../src/tools.ts#L1620-L1627)). Claude updates a group's profile from its current profile in the chat database, and changes only its own fields, so the owners' preferences are kept. | [tools.ts:721-738](../src/tools.ts#L721-L738), [hub.ts:96](../src/hub.ts#L96), [control.ts:188-214](../src/control.ts#L188-L214), [:752-760](../src/control.ts#L752-L760), [main-group.ts:57-66](../src/main-group.ts#L57-L66) |
| The first human who joins the Main group while it has no owner becomes an owner, and the link is deleted | [main-group.ts:122](../src/main-group.ts#L122), [:134-140](../src/main-group.ts#L134-L140) |
| A session runs in mode `host` after an approver approves its `mode` card. The card is posted once an approver is in the group. After a rejection, the card is posted again only after `requests`. `requests` applies at once, and the hub ends a pending `mode` card. The mode is kept in the session group's custom data. | [hub.ts:136-155](../src/hub.ts#L136-L155), [:329-332](../src/hub.ts#L329-L332), [:525](../src/hub.ts#L525), [tools.ts:571-572](../src/tools.ts#L571-L572) |
| At a reset of the owners from the computer that runs Claude Code, the service replaces the Main group at once, even while owners exist. No owner's approval is needed for `reset-owners` and `POST /reset-owners`. | [hub.ts:407-409](../src/hub.ts#L407-L409), [owner.ts:28-33](../src/owner.ts#L28-L33), [main-group.ts:99-109](../src/main-group.ts#L99-L109) |
| A chat without an owner is given to the session that connects it. The owner is set at one of four points: the perform, the connection event of its action, the completion of a pending connection that the session recorded, or a `connect` to the chat as a known chat. The tools of other sessions don't show the chat. With `/give CHAT N`, an approver moves the chat to session N. | [tools.ts:585-593](../src/tools.ts#L585-L593), [:758-763](../src/tools.ts#L758-L763), [:834](../src/tools.ts#L834), [:843](../src/tools.ts#L843), [:901](../src/tools.ts#L901), [hub.ts:166-171](../src/hub.ts#L166-L171), [:511-512](../src/hub.ts#L511-L512), [control.ts:475-486](../src/control.ts#L475-L486) |

Tool-use state machine, one action of tool `permission` per request:

| Event | From | To | Code |
|---|---|---|---|
| `permission_request` | | `pending`, card posted | [hub.ts:173-174](../src/hub.ts#L173-L174) |
| `/approve N` | `pending` | `performing`, `allow` pushed, then `done` | [hub.ts:176-177](../src/hub.ts#L176-L177), [tools.ts:561-574](../src/tools.ts#L561-L574) |
| `/reject N` | `pending` | `rejected`, `deny` pushed | [hub.ts:176-177](../src/hub.ts#L176-L177) |
| an answer, or a prompt that is not mid-turn, of a hook that started after the card's creation; end of the last attachment | `pending` | `expired`, card edited to `Ended.` | [hub.ts:185](../src/hub.ts#L185), [:208-212](../src/hub.ts#L208-L212), [service.ts:321](../src/service.ts#L321), [actions.ts:237-241](../src/actions.ts#L237-L241) |
| expiry | `pending` | `expired`, card edited to `Expired.` | [actions.ts:302-312](../src/actions.ts#L302-L312) |
| `done`, `rejected` or `expired` | | removed from the session's actions | [hub.ts:178-180](../src/hub.ts#L178-L180) |

```mermaid
stateDiagram-v2
  [*] --> pending: permission_request
  pending --> performing: /approve N, allow pushed
  performing --> done
  pending --> rejected: /reject N, deny pushed
  pending --> expired: later answer or prompt that is not mid-turn, last detach, card Ended.
  pending --> expired: expiry, card Expired.
  done --> [*]: removed
  rejected --> [*]: removed
  expired --> [*]: removed
```

### 8.6 Channel and mirroring

| Direction | Message | Code |
|---|---|---|
| service to session | `{prompt: {text, from, item, file?}}`, `{permission: {requestId, behavior}}` on `/attach` | [protocol.ts:59-62](../src/protocol.ts#L59-L62) |
| session to Claude Code | `notifications/claude/channel` `{content, meta: {from, item, file?}}`; `notifications/claude/channel/permission` `{request_id, behavior}` | [session.ts:197-201](../src/session.ts#L197-L201) |
| Claude Code to session | `notifications/claude/channel/permission_request` `{request_id, tool_name, description, input_preview}` | [session.ts:58-62](../src/session.ts#L58-L62) |
| session to service | `POST /permission` | [session.ts:114-118](../src/session.ts#L114-L118) |
| hooks to service | `POST /turn` `{id, pid?, at?, prompt?, typed?, note?, midTurn?, answer?, model?, effort?}`. `pid` is `CLAUDE_PID`, and `at` is the hook's `performance.timeOrigin`. `prompt` and `answer`, the last answer, are cut to 250,000 characters. `typed` of UserPromptSubmit is true when a person typed the prompt. `note` is the line about one of Claude Code's own prompts. `midTurn` is true for a prompt that Claude Code added to a running turn. `model` is the new model of PostModelSwitch, or the model of Stop's answer in the transcript. `effort` is the effort level. | [hook.ts:114-309](../src/hook.ts#L114-L309) |

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
  prompt p not midTurn, or answer a:   end pending permissions created before at
  answer a, also empty:            earlier = turns with at below at; quoted = the item of the newest earlier,
                                   or the oldest pushed not quoted, marked quoted, when earlier is empty; earlier leave turns
                                   answeredAt = max(answeredAt, at)
  prompt p:                        matched = pushed entries in this plugin's channel tags of p, or equal to p and pushed in the last 60 s
                                   at not below answeredAt, and not midTurn or matched not empty:
                                   turns += (at, the oldest matched or null)
                                   matched leave pushed
  model or effort:                 skipped when at is older than modelAt or effortAt, the at of the value taken last
                                   else: modelAt or effortAt = at; a changed value: description, session.json
on /turn note:                     Claude posts note, without a quote; refused for NAME: Claude posts
                                   "A note on Claude Code's prompt was not posted: " and the preference's text
on /turn prompt p:                 typed not false, nothing matched, p not blank, not only this plugin's channel tags,
                                   not a one-line "/" command:
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

Sources: [hub.ts:157-212](../src/hub.ts#L157-L212), [:234-306](../src/hub.ts#L234-L306), [:538-555](../src/hub.ts#L538-L555).

### 8.7 Security boundaries

| Rule | Code |
|---|---|
| The socket is in ROOT, mode 0700. For a path over 103 bytes, the socket is `TMP/simplex-mcp-HASH.sock`, and on Windows it is a named pipe. The service accepts a request only with the token, compared with `timingSafeEqual`. | [socket.ts:19-25](../src/socket.ts#L19-L25), [files.ts:74-79](../src/files.ts#L74-L79) |
| The Main group link is shown only in the hook's `systemMessage`, and on a terminal with `link`. In tool results, refusals and other output, the command is named instead. Of the socket's routes, only `/title`, `/link` and `/reset-owners` return a Main group link. `/reset-owners` returns the link of the new group. | [hook.ts:94-96](../src/hook.ts#L94-L96), [owner.ts:7-15](../src/owner.ts#L7-L15), [control.ts:46](../src/control.ts#L46), [service.ts:154-155](../src/service.ts#L154-L155), [:215](../src/service.ts#L215), [:426](../src/service.ts#L426), [hub.ts:407-409](../src/hub.ts#L407-L409) |
| `reset-owners` shows the new link only on a terminal | [owner.ts:17-20](../src/owner.ts#L17-L20), [:28-33](../src/owner.ts#L28-L33) |
| The plugin runs its own `dist/cli.js`. Node.js loads its modules, bundled in the package, from the plugin's folder, so they can't be replaced by the `node_modules` of a project. | [.mcp.json:5](../.mcp.json#L5), [hooks.json:3-7](../hooks/hooks.json#L3-L7), [package.json:35](../package.json#L35) |
| The service gets only the environment variables of `serviceVariables` | [client.ts:13](../src/client.ts#L13), [:83](../src/client.ts#L83) |
| `send` and `reply` refuse the files of ROOT except `files/` of the session's own folder | [tools.ts:1004-1009](../src/tools.ts#L1004-L1009), [hub.ts:93-94](../src/hub.ts#L93-L94) |
| `User`'s messages are neither prompts nor commands. `Session.prompt` and `Control.onMessage` drop the messages of the member of the hub's contact marked `simplexMcpUser`, which is read at the hub's start. `User` joins only the groups to which it is invited by its `Claude` contact. | [hub.ts:235](../src/hub.ts#L235), [:320-322](../src/hub.ts#L320-L322), [:381](../src/hub.ts#L381), [control.ts:299](../src/control.ts#L299), [user.ts:50-57](../src/user.ts#L50-L57), [:60-62](../src/user.ts#L60-L62) |
| The hub's own groups and the `User` contact have no owner key, so the tools of every session leave them out | [tools.ts:585-587](../src/tools.ts#L585-L587), [:1297-1305](../src/tools.ts#L1297-L1305) |
| The approved mode is kept in the chat database, not in a file of ROOT | [hub.ts:77](../src/hub.ts#L77), [:324-327](../src/hub.ts#L324-L327), [files.ts:11-21](../src/files.ts#L11-L21) |
| These rules hold against Claude's tools, but not against a shell running as the same user. Such a shell can read ROOT, with the token and the chat databases, and send requests to the service. | |

### 8.8 Limits

| Limit | Value | Code |
|---|---|---|
| socket request body | 1 MiB | [socket.ts:6](../src/socket.ts#L6) |
| idle close of a session, idle stop of the service | 10 min each | [service.ts:17-18](../src/service.ts#L17-L18) |
| service start wait | 5 min | [client.ts:10](../src/client.ts#L10) |
| `User` connection at the first start | 60 s | [user.ts:11](../src/user.ts#L11) |
| echo of a raw prompt | 60 s | [hub.ts:23](../src/hub.ts#L23) |
| pushed prompts of a session | 20 | [hub.ts:24](../src/hub.ts#L24) |
| file of a prompt | 10 min | [hub.ts:25](../src/hub.ts#L25) |
| prompts held for an attachment after the hub opens | 30 s | [hub.ts:27](../src/hub.ts#L27) |
| record of a pending connection | 1 day | [hub.ts:26](../src/hub.ts#L26) |
| wait of a session server for the title of its process, the time a detached session server's process is kept, and the mark of an unattended run's process, removed at a later mark | 10 s | [service.ts:19](../src/service.ts#L19), [:237-243](../src/service.ts#L237-L243) |
| attempts of a turn the service refuses with status 409 | 15, 1 s apart | [hook.ts:13-15](../src/hook.ts#L13-L15) |
| reads of the transcript for the model of Stop's answer and the origin of a prompt | 20 for the answer and 600 for the prompt, 100 ms apart, each of the last 1 MiB and twice the bytes of the answer or the prompt | [hook.ts:17-20](../src/hook.ts#L17-L20), [:153-174](../src/hook.ts#L153-L174) |
| length of a note of Claude Code's own prompt | 200 characters, the last one `…` when cut | [hook.ts:22](../src/hook.ts#L22), [:287-290](../src/hook.ts#L287-L290) |
| attachment retries of a session server | 1 s, doubled up to 30 s | [session.ts:19-20](../src/session.ts#L19-L20) |
| part of an answer or a terminal prompt | one message by `fitsMessage`: 15602 bytes with the 600-byte envelope; 14962 bytes of text without a quote | [hub.ts:538-555](../src/hub.ts#L538-L555), [tools.ts:165-166](../src/tools.ts#L165-L166), [:1588-1591](../src/tools.ts#L1588-L1591) |
| cancelled cards wait at stop | 10 s | [hub.ts:22](../src/hub.ts#L22) |
| `service.log` | emptied over 10 MB, at a spawn and every hour | [files.ts:9](../src/files.ts#L9), [:91-94](../src/files.ts#L91-L94), [service.ts:20](../src/service.ts#L20) |
| socket path | 103 bytes: `sun_path` of `sockaddr_un` holds 104 bytes on macOS and 108 on Linux, with the terminating NUL | [files.ts:8](../src/files.ts#L8) |
| actions per session | 20 pending, 20 performing | [actions.ts:9-10](../src/actions.ts#L9-L10), [:199-203](../src/actions.ts#L199-L203) |
