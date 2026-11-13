# SimpleX MCP server: architecture

> System structure, modules, state, process lifecycle, approval model and security boundaries of `@simplex-chat/mcp` 0.3.0. `serve` is described in sections 1-7, and the Claude Code plugin in section 8.
>
> Related specs: [README](README.md) | [Flows](flows.md) | [API](api.md)
> Related docs: [README](../README.md) | [Reference](../REFERENCE.md) | [PLAN](../../../plans/2026-10-19-mcp-server.md)

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

1. Claude Code runs the `headersHelper` command of its entry ([`registryEntry`](../src/register.ts)) and sends the `Authorization` header it prints ([Reference](../REFERENCE.md#add-the-agent-to-claude-code)).
2. The server checks Host, Origin, token, path and readiness, then passes the request to the SDK handler (the request listener of [`startHttp`](../src/http.ts)).
3. An `McpServer` is built from the factory for each request, and the arguments are validated against the input schema by the SDK (`factory` in [`startAgent`](../src/serve.ts), [`McpServer.validateToolInput`](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs)).
4. The handler result is returned by `AgentTools.call` as `structuredContent` and as JSON text ([`AgentTools.call`](../src/tools.ts)).
5. The server calls `ChatApi` methods of the core ([`getChat`, `receivingFiles`, `markRead`](../src/messages.ts)); actions are created in `Actions` and shown to the owner by `Control` ([`Actions.create`](../src/actions.ts)).
6. The core emits events. The handler of `serve.ts` passes them to the agent control chat and to `AgentTools.onEvent` (the event handler of [`startAgent`](../src/serve.ts)). `AgentTools.onEvent` then dispatches them to the tools and the item watch ([`AgentTools.onEvent`](../src/tools.ts)).

---

## 2. Modules

### [2.1 `cli.ts`](../src/cli.ts)

Entry point of the `simplex-mcp` binary.

| Symbol | Contents |
|---|---|
| <a id="cli-helptext"></a>`helpText` | `--help` output |
| <a id="cli-plugincommands"></a>`pluginCommands` | `session`, `service`, `hook session-start`, `hook turn`, `link`, `reset-owners` |
| <a id="cli-command"></a>`Command` | `help`, `serve` with `ServeOptions`, `register` with `RegisterOptions`, the plugin commands |
| <a id="cli-parsecommand"></a>`parseCommand` | command, NAME and options to a `Command`, or the plugin command equal to the joined arguments |
| <a id="cli-serveoptions"></a>`serveOptions` | defaults and option switch of `serve` |
| <a id="cli-registeroptions"></a>`registerOptions` | defaults and option switch of `register` |
| <a id="cli-optionreader"></a>`OptionReader` | `value`: the next argument, required<br>`optionalValue`: the next argument, absent when missing or starting with `--` |
| <a id="cli-validagentname"></a>`validAgentName` | `^[a-z0-9](-?[a-z0-9])*-?$`, at most 32 characters |
| <a id="cli-usageerror"></a>`usageError` | `ExitError` with code 2 |
| <a id="cli-main"></a>`main` | `help`, `serve`, `register`, `session`, `service`, the hooks, `link` and `reset-owners`. `help` prints to stdout. The hooks read stdin and print their JSON output. `link` and `reset-owners` print their text. |
| top-level catch of `main` | `ExitError`: message on stderr, its code; other errors: `logError`, code 1 |

### [2.2 `serve.ts`](../src/serve.ts)

Start sequence, event dispatch and stop sequence of one agent.

| Symbol | Contents |
|---|---|
| <a id="serve-ownerentry"></a>`OwnerEntry` | `{memberId, name}` of `--owners` |
| <a id="serve-serveoptions"></a>`ServeOptions` | name, data folder and its `dataGiven` flag, display name, mode, owners, expiry, reset and token flags, port, QR code size, stop wait |
| <a id="serve-runningserver"></a>`RunningServer` | token, `ChatApi`, user, `Actions`, `stop` |
| <a id="serve-serve"></a>`serve` | `startServer`, then SIGINT and SIGTERM handlers: the first signal stops, a second ends the stop wait |
| <a id="serve-startserver"></a>`startServer` | data folder, lock, token, headers file, `ChatApi.init`, `startAgent`; cleanup on error |
| <a id="serve-startagent"></a>`startAgent` | user, `prepareCore`, control group, wiring of `Actions`, `AgentTools`, `Control`, HTTP, event handler, `stop` |
| <a id="serve-registerhint"></a>`registerHint` | the `register` command line printed after the owner setup |
| <a id="serve-waitforcancelledcards"></a>`waitForCancelledCards` | the fixed wait at stop for cards edited or posted at the stop, used by the plugin's hub as well |
| <a id="serve-controlgroup"></a>`controlGroup` | found, created or reset agent control chat |
| <a id="serve-listen"></a>`listen` | `startHttp`; `EADDRINUSE` to `ExitError` code 1 |

These symbols of [`core.ts`](../src/core.ts) and [`version.ts`](../src/version.ts) are shared with the plugin:

| Symbol | Contents |
|---|---|
| <a id="core-preparecore"></a>[`prepareCore`](../src/core.ts) | network config, `DATA/tmp` as the core's temp folder, mode 0700, and `DATA/files/sent` deleted |
| <a id="core-updateduser"></a>[`updatedUser`](../src/core.ts) | display name and peer type `agent` of the profile |
| <a id="core-bufferevents"></a>[`bufferEvents`](../src/core.ts) | events received before the handler is installed are queued and replayed in order |
| <a id="version-version"></a>[`version`](../src/version.ts) | the package version |

### [2.3 `http.ts`](../src/http.ts)

| Symbol | Contents |
|---|---|
| <a id="http-httpoptions"></a>`HttpOptions` | port, token, `McpServerFactory`, readiness |
| <a id="http-starthttp"></a>`startHttp` | `http.Server` on `127.0.0.1:port`: guards, SDK handler, timeouts |
| <a id="http-authorized"></a>`authorized` | length check and `timingSafeEqual` against `Bearer <token>` |
| <a id="http-stophttp"></a>`stopHttp` | `server.close` and `closeAllConnections` |

### [2.4 `tools.ts`](../src/tools.ts)

Tool definitions, call handlers, connect plans, perform steps, files.

| Symbol | Contents |
|---|---|
| <a id="tools-inputs"></a>`ConnectInput`, `SendInput`, `ReceiveInput`, `LeaveInput`, `DownloadInput` | tool inputs |
| <a id="tools-plansummary"></a>`PlanSummary` | card kind, chat type, name, SimpleX name, peer type, description, welcome, commands, `takesMessage` |
| <a id="tools-chatfields"></a>`ChatFields` | chat fields of `connect` and `list` results |
| <a id="tools-agent"></a>`Agent` | `ChatApi`, user, `Actions`, `ChatItemsWatch`, control chat reference, standing approver lookup, refused file post, data folder. The plugin adds the data root, the owning session ID, the group of `reply`, the mode setter and the record of a pending connection. |
| <a id="tools-joinmethod"></a>`JoinMethod` | `prepared`, `prepareChannel`, `viaAddress`, `prepareContact`, `connect` |
| <a id="tools-schemas"></a>schemas, from `messageSchema` to `downloadOutput` | input and output JSON schemas |
| <a id="tools-instructions"></a>descriptions, from `connectDescription` to `leaveDescription`, and `instructions` | tool descriptions and server instructions. `instructions` names the host at whose restart actions are cancelled, `server` by default. |
| <a id="tools-replytool"></a>`replyTool` | the plugin's seventh tool, `reply` |
| <a id="tools-definitions"></a>`definitions` | name, description, schemas and action flag of the six tools |
| <a id="tools-registertools"></a>`registerTools` | `registerTool` for each definition and each extra one, with `_meta` on action tools in mode `host` |
| <a id="tools-download"></a>`Download` | one download: its promise, chat, item, file ID, size, path, the last fileSource seen, received bytes and their time, check timer |
| <a id="tools-agenttools"></a>`AgentTools` | constructor with the `Agent`, the mode or a function that returns it, and the session number of the plugin. The mode is read at each use of `mode`. |
| <a id="tools-agenttools-register"></a>`AgentTools.register` | `registerTools` with `AgentTools.call` |
| <a id="tools-agenttools-call"></a>`AgentTools.call` | handler result to `CallToolResult`; errors to `isError` |
| <a id="tools-agenttools-perform"></a>`AgentTools.perform` | perform step of an approved action, which is the mode setter for a `mode` action |
| <a id="tools-agenttools-givechat"></a>`giveChat`, `owns`, `claim` | the plugin's chat owners. A chat is the session's when no session ID is set or the chat's owner is that ID. `claim` sets the owner of a chat without one. It is called at a connection, and when `connect` names a known chat (`ownChat`). The hub calls it when a pending connection completes. |
| <a id="tools-agenttools-onevent"></a>`onEvent` | events to the file handlers, connect completions and the item watch, for `serve` and the plugin's sessions |
| <a id="tools-agenttools-reply"></a>`reply`, `sendFile` | `reply` sends a file to the group of the plugin's session at once, without an action, under the file rules of `send`. Its message with the caption and the preview is checked by `fitsMessage`. The core's refusal `feature not allowed NAME` is answered with the text with the preference's name. `sendFile` posts the file. |
| <a id="tools-refusedfeature"></a>`refusedFeature`, `featureOff` | the preference NAME of a command error `feature not allowed NAME`, and the text `NAME are off in the group of this session; its owners can turn them on in the group's preferences.` The text is used for `reply`, the cards, `/forward` and the answers of a session group. |
| <a id="tools-agenttools-downloadfile"></a>`downloadFile`, `forwardFile`, `downloadTo` | one download per file ID, shared by `download` and `/forward`; a forwarded file moved to the download path for `download` |
| <a id="tools-agenttools-onnewitems"></a>`onNewItems`, `onFile`, `onFileComplete`, `onFileError`, `onFileCancelled`, `onFileDescription` | file events |
| <a id="tools-agenttools-oncontactconnected"></a>`onContactConnected`, `onUserJoinedGroup` | completion of connect actions by events |
| <a id="tools-agenttools-actionresult"></a>`actionResult` | wait for the decision, result fields, removal |
| <a id="tools-agenttools-attachment"></a>`attachment`, `isRefused`, `fileToSend`, `resizedCopy` | the file of a `send` |
| <a id="tools-agenttools-copyforsending"></a>`copyForSending`, `chatFiles`, `deleteChatFiles` | `/forward` copies; the files of a chat, forwarded ones included; its downloads cancelled, also those the core continues after a restart |
| <a id="tools-agenttools-leavechat"></a>`leaveChat` | perform step of `leave` and `/leave`; the chat's forwarded files deleted |
| <a id="tools-agenttools-download"></a>`download`, `fileServers`, `unknownServers` | the `download` call before its action |
| <a id="tools-agenttools-startdownload"></a>`startDownload`, `checkDownload`, `currentFile`, `settleFile`, `settleReceived`, `moveReceived` | a download's steps |
| <a id="tools-agenttools-agentchats"></a>`agentChats`, `knownChat` | the agent's chats; a checked chat reference |
| <a id="tools-agenttools-selfsender"></a>`selfSender`, `toMessages` | the agent's name and peer type in a chat, its incognito name in an incognito chat, or its profile name when the incognito name can't be read; the items of one chat as messages with that name, for the results of `connect`, `send` and `receive` |
| <a id="tools-classifyplan"></a>`classifyPlan` | `apiConnectPlan` response to a join plan or a known chat |
| <a id="tools-chatsubject"></a>`chatSubject`, `isConnected`, `isActive`, `memberCurrent`, `memberCurrentOrPending` | chat and member predicates shared with `control.ts` and `hub.ts` |
| <a id="tools-customdataof"></a>`customDataOf`, `setChatData`, `ownerOf` | a chat's custom data; the plugin's owner key `simplexMcpOwner` |
| <a id="tools-fitsmessage"></a>`tooLongText`, `fitsMessage`, `quoteContent` | the refusal text of `send` and `reply` for a message that is too long. `fitsMessage` checks that the message as the core sends it, with its quote, is within `maxMessageLength`, 15602 bytes. The check is used for `send`, `reply`, the post of `/forward`, and the parts and quotes of the plugin's posts. |
| <a id="tools-realfile"></a>`realFile` | real path, size, inode and change time of a file |
| <a id="tools-attachmentmedia"></a>`attachmentMedia`, `videoFrame`, `videoDuration` | previews and durations |
| <a id="tools-toolcall"></a>`toolCall` | `ServerContext` to `ToolCall`: abort signal and progress notifications |

### [2.5 `actions.ts`](../src/actions.ts)

| Symbol | Contents |
|---|---|
| <a id="actions-mode"></a>`Mode` | `requests`, `host` |
| <a id="actions-state"></a>`State` | `pending`, `performing`, `done`, `failed`, `rejected`, `expired` |
| <a id="actions-standing"></a>`Standing` | `messages`, `files` |
| <a id="actions-toolerror"></a>`ToolError` | errors the model can correct; text returned as is |
| <a id="actions-subject"></a>`Subject` | kind, name, SimpleX name, incognito flag of a card |
| <a id="actions-connref"></a>`ConnRef` | pending connection, contact or group of a connect action |
| <a id="actions-hosttexts"></a>`HostTexts` | refusal texts of a lost control chat, of a stop and of the limit of pending actions. `serve` uses `lostMessage`, `stoppingMessage` and `pendingMessage`, where the owner is named. The plugin uses the hub's texts. |
| <a id="actions-actionrecord"></a>`ActionRecord` | number, session number of the plugin, mode, key, times, change signal, standing flag, state, card, decider, open calls, interrupted flag, ended flag, error, card image |
| <a id="actions-connectaction"></a>`ConnectAction`, `SendAction`, `LeaveAction`, `DownloadAction` | per-tool fields; `Attachment`, the file of a send |
| <a id="actions-modeaction"></a>`ModeAction`, `PermissionAction` | the plugin's actions: a switch to mode `host`, and a tool use of Claude Code with the bytes left for its input preview |
| <a id="actions-actionhooks"></a>`ActionHooks` | `refusal`, `saveCounter`, `postCard`, `perform`, `outcome` |
| <a id="actions-actions"></a>`Actions` | action map, one expiry timer per action, posting and outcome promises |
| <a id="actions-actions-create"></a>`create` | state by the action's mode, refusal checks, number, log line, counter write, card |
| <a id="actions-actions-ofsession"></a>`ofSession`, `remove`, `openCall`, `closeCall`, `end` | the actions of one session; removal; open call count; an expiry before the expiry time, marked ended |
| <a id="actions-actions-expiredue"></a>`expireDue` | expiry of every action past its expiry time |
| <a id="actions-actions-decided"></a>`decided` | wait with progress until a decided state, `until`, or the abort signal |
| <a id="actions-actions-stop"></a>`stop` | cancellation of pending and performing actions |
| <a id="actions-actions-scheduleexpiry"></a>`scheduleExpiry`, `expire`, `forgetExpired` | the expiry timer of one action, at most 60 s. `expire` expires the action, or sets a new timer when the timer, on the monotonic clock, fires before the expiry time on the wall clock. `forgetExpired` removes an action once it is decided, without open calls and past its expiry time. An action of `serve` that expires is kept longer, see [5.6](#56-expiry). |
| <a id="actions-summary"></a>`summary`, `performedText`, `progressText`, `sentence` | log and progress texts. `sentence` ends a log line with a period. For an action of a session, the line ends with one period, even after an error that ends with one. For an action of `serve`, every error is followed by a period, as in 0.2. |

Hooks are bound in [`startAgent`](../src/serve.ts), where `actions` is created: `refusal`, `saveCounter`, `postCard` and `outcome` to `Control`; `perform` to `AgentTools`.

### [2.6 `control.ts`](../src/control.ts)

| Symbol | Contents |
|---|---|
| `qrCodeText` | the function of [`qr.ts`](../src/qr.ts), exported from `control.ts` as in 0.2 |
| <a id="control-controlcommands"></a>`controlCommands` | the command menu |
| <a id="control-standings"></a>`standings` | custom data key and reply words of the approvals of messages and files |
| <a id="control-controldata"></a>`ControlData` | `{control: true, nextAction}` |
| <a id="control-chatapproval"></a>`ChatApproval` | `{approvedBy, approvedById, approvedAt}` |
| <a id="control-controloptions"></a>`ControlOptions`, `SessionOptions` | display name, `--owners`, register hint and QR code size. For a group of the plugin's session, its approver check, the session IDs by number and the check of `User` are added. |
| <a id="control-ownerrequest-type"></a>`OwnerRequest` | parsed owner command |
| <a id="control-control-load"></a>`Control.load` | lost check, group profile and menu through `updateProfile`, and member list |
| <a id="control-control-rename"></a>`Control.rename`, `Control.describe`, `updateProfile` | the plugin's group name, and its short description. `updateProfile` makes one profile update at a time, and keeps the custom data in memory. A session group is updated from its current profile in the chat database. The agent control chat of `serve` is updated from the profile held in memory since the start, by `changeGroupProfile`. |
| <a id="control-control-start"></a>`Control.start` | owner setup per `--owners` |
| <a id="control-control-refusal"></a>`Control.refusal` | bytes left for the link texts of a connect card and for the input preview of a tool-use card, card size of a send, connect or download, approver presence |
| <a id="control-control-hasapprover"></a>`Control.hasApprover` | an approver is a member |
| <a id="control-control-standingapprover"></a>`Control.standingApprover` | approver name of a chat's standing approval of messages or files |
| <a id="control-control-savecounter"></a>`Control.saveCounter`, `Control.saveData` | queued writes of the counter and of other keys of the group's custom data |
| <a id="control-control-postcard"></a>`Control.postCard`, `Control.outcome` | card post and card edit; the kept-files message after a done leave |
| <a id="control-control-refusedfile"></a>`Control.refusedFile`, `Control.fileNotSent` | posts about files |
| <a id="control-control-onmessage"></a>`Control.onMessage` | owner commands, queued one at a time. In a group of the plugin's session, the session passes it the one-line `/` texts, and the texts of `User` are dropped. |
| <a id="control-control-onevent"></a>`Control.onEvent` | membership changes and loss of the control chat |
| <a id="control-control-ownercommand"></a>`Control.ownerCommand` | command dispatch and replies. A text outside the requests is answered with the command list, or with `Unknown command.` in a group of the plugin's session. |
| <a id="control-control-approvechat"></a>`approveChat`, `revoke`, `forward`, `leave`, `deleteFiles` | owner requests |
| <a id="control-control-give"></a>`give`, `setObserver` | `/give CHAT N` and `/observer #G ID` in a group of the plugin's session. `setObserver` replies `NAME's role can't be changed.` for a member with status `pending_approval` or `pending_review`. For a member who is neither current nor pending, such as an invited member who has not joined, the reply is `No member ID in this group.` ([`memberCurrentOrPending`](../src/tools.ts)). Only a current member's role is set to `observer`, when the sender's role is admin or owner and not below the member's role. The members are then read again, and the observer's rights are the reply. |
| <a id="control-control-isapprover"></a>`isApprover` | the session's check in a group of the plugin's session; otherwise role `owner`, and the `--owners` list when given |
| <a id="control-roleorder"></a>`roleOrder`, `roleRank`, `canSend` | the roles of SimpleX from `relay` to `owner`, in the order of the core; a role above `observer` |
| <a id="control-sessionapproverroles"></a>`sessionApproverRoles`, `rightsText` | `admin` and `owner`, the approvers of a session group. `rightsText` is `NAME is a ROLE and RIGHTS.`, with the rights of the approvers, of the other roles that send, or of an observer. |
| <a id="control-control-postcardcontent"></a>`postCardContent`, `sessionRefusal`, `postRefusal`, `refusedCardSubject` | the post of a card. In a plugin session group, a card that the core refuses with `feature not allowed Files and media` is posted again as text, and its later edits are text. A card or an edit refused for another preference before the decision or at a stop is replaced by `Action N: ` and the text with the preference's name. An action that waits for approval then fails with that text. After the decision, the replacement is `Action N OUTCOME; its card was not posted: ` or `...; its card was not updated: ` with that text, where OUTCOME is `was performed`, `failed`, `was rejected`, `expired` or `ended`. The action is kept as it was. |
| <a id="control-control-cardcontent"></a>`cardContent`, `fittedImage` | image and video cards with the file's preview |
| <a id="control-findcontrolgroup"></a>`findControlGroup`, `createControlChat`, `resetControlChat` | control group lookup, creation, reset |
| <a id="control-creategroup"></a>`createGroup`, `updateGroupProfile`, `changeGroupProfile`, `deleteGroup` | a group with its profile and custom data, for `serve` and the plugin's groups. A session group or the Main group is updated from its current profile in the chat database. The agent control chat of `serve` is updated from the profile held in memory since the start, as in 0.2. Each update is made only when the change returns a profile. A group is deleted for all members. |
| <a id="control-isgroupmessage"></a>`isGroupMessage` | an item of one group's main scope |
| <a id="control-iscontrolevent"></a>`isControlEvent` | membership events of the control group |
| <a id="control-parsecommand"></a>`parseCommand` | one-line `/keyword params` |
| <a id="control-cardtext"></a>`cardText`, `requestLines`, `commandsLine` | card text per tool, state and mode |
| <a id="control-largestcard"></a>`largestCard`, `encodedLength` | card size check |
| <a id="control-truncatedline"></a>`truncatedLine`, `fittedLines` | a line cut with `…`; the code lines that fit a byte budget, the last one cut |
| <a id="control-serversline"></a>`serversLine`, `sizeText` | servers line and sizes of download cards |
| <a id="control-codespan"></a>`codeSpan`, `safeName`, `mentionText` | quoting of peer text |
| <a id="control-ownerrequest"></a>`ownerRequest` | command words to an `OwnerRequest`, with `observer #G ID` only for a group of the plugin's session |
| <a id="control-losescontrol"></a>`losesControl`, `isLost` | loss of the control chat or of a plugin group |
| <a id="control-joinedtext"></a>`joinedText` | `NAME joined (member N)`, shared with the Main group |
| <a id="control-entrytext"></a>`entryText`, `restartText` | `--owners` entries in restart lines |

### [2.7 `messages.ts`](../src/messages.ts)

| Symbol | Contents |
|---|---|
| <a id="messages-message"></a>`Message`, `MessageFile`, `FileOffer` | message item of tool results; its file; the file of a download |
| <a id="messages-toolcall"></a>`ToolCall` | abort signal and progress callback of one call |
| <a id="messages-collected"></a>`Collected` | collected items, change signal, IDs of the items over the first 100, time of the last change |
| <a id="messages-changes"></a>`Changes` | set of wake callbacks |
| <a id="messages-chatitemswatch"></a>`ChatItemsWatch` | listeners per chat reference; received message items gathered by `collect` |
| <a id="messages-chatref"></a>`chatRef`, `anyScopeChatRef`, `isChatRef` | `@N` and `#N` references: of the main conversation, or also of a group's member chat |
| <a id="messages-tomessage"></a>`toMessage`, `messageFile`, `receivedFileError`, `receivedPath` | chat item to `Message`; where a received file is: its download path or its forwarded path |
| <a id="messages-toreplyto"></a>`toReplyTo`, `replyText`, `quoteLine` | the quoted message of a reply: its ID, its sender by direction, its type other than text and its text, or its file name for an empty text; the text without bidi controls, cut to its first line, up to 200 characters with `…`; the quote line `> ` of the channel |
| <a id="messages-filepaths"></a>`filePaths`, `storedName`, `chatFolder` | `DATA/files/group-N`, `DATA/files/contact-N` or the same under `DATA/forwarded`, and the download path and the forwarded path of a received file in them. A stored file is named by its file ID and extension, and so are the files of the plugin's prompts. |
| <a id="messages-fileoffer"></a>`fileOffer`, `fileContent` | file fields of an item; message content of a file |
| <a id="messages-getchat"></a>`getChat`, `receivingFiles`, `markRead` | `apiGetChat` and `apiChatItemsRead` by chat reference; the IDs of a chat's files in status `rcvAccepted`, `rcvTransfer` or `rcvWarning`, paged |
| <a id="messages-unreaditems"></a>`unreadItems` | unread items of a chat after its `initial` read, paged |
| <a id="messages-reportprogress"></a>`reportProgress`, `nextChange`, `awaitResponses` | waits |
| <a id="messages-validname"></a>`validName`, `viewName` | display name rules and `@name` form |

### [2.8 `files.ts`](../src/files.ts)

| Symbol | Contents |
|---|---|
| <a id="files-createdatafolder"></a>`createDataFolder` | `mkdir -p` mode 0700; `chmod 0700` except on Windows |
| <a id="files-acquirelock"></a>`acquireLock`, `releaseLock` | `DATA/lock`, or the file of the given name, with the PID. The holder is named in the error by the `user` argument, `server` or `service`. |
| <a id="files-servertoken"></a>`serverToken` | `DATA/token`, or the file of the given name. An existing file is read, and is made private when the user owns it. A new token is 32 random bytes in base64url, and its file is created as the lock is. |
| <a id="files-saveheldtitles"></a>`saveHeldTitles`, `takeHeldTitles` | `ROOT/titles.json`, written at a service stop, and read and removed at the next start. An unparsable file is skipped with a log line. |
| <a id="files-createlock"></a>`createLock`, `createExclusive` | A private temporary file with the text is linked into place, so that the file appears once and complete. Where the file system refuses the link, the file is written exclusively. |
| <a id="files-takeover"></a>`takeOver` | The lock of a stopped process is claimed first, so that only one starter takes it over. The claim is a link of the lock to `lock.PID.claim`, or an exclusive write of `lock.PID.claim` where links fail. When that PID is still in the lock, the lock is removed and created again. The claim is then removed. |
| <a id="files-writeheadersfile"></a>`writeHeadersFile` | `DATA/headers.json` |
| <a id="files-writeserverfile"></a>`writeServerFile`, `readServerFile` | `DATA/server.json` |

### [2.9 `register.ts`](../src/register.ts)

| Symbol | Contents |
|---|---|
| <a id="register-register"></a>`register` | `claude mcp remove`, then `claude mcp add-json` |
| <a id="register-registryentry"></a>`registryEntry` | `{type, url, headersHelper, timeout}` |
| <a id="register-manualentrytext"></a>`manualEntryText` | entry text when `claude` is missing |
| <a id="register-projectroot"></a>`projectRoot` | project key of `--scope local` |
| <a id="register-runcommand"></a>`runCommand` | `spawnSync` with ENOENT detection; on Windows after ENOENT, once more through `cmd.exe` with `cmdArgument` quoting, and exit status 9009 as missing |

### [2.10 `log.ts`](../src/log.ts)

| Symbol | Contents |
|---|---|
| <a id="log-exiterror"></a>`ExitError` | message and exit code |
| <a id="log-shellword"></a>`shellWord`, `shellPath` | POSIX single quotes or Windows double quotes |
| <a id="log-oneline"></a>`oneLine` | runs of `\p{Cc}`, `\p{Cf}`, `\p{Zl}`, `\p{Zp}` to one space |
| <a id="log-log"></a>`log`, `logError` | stdout lines; stderr lines |
| <a id="log-errortext"></a>`errorText`, `chatErrorText` | error text, folded to one line: the apps' wording for known error shapes, otherwise the type chain |
| <a id="log-apperrortext"></a>`appErrorText` | wording of the apps' alerts for chat, agent and store errors |
| <a id="log-fileerror"></a>`fileError` | the error of a received file: the apps' words for its status, or the size limit of its sender |
| <a id="log-serverhostname"></a>`serverHostname` | host of a server address |
| <a id="log-chaintext"></a>`chainText`, `errorChain` | `SimpleX error: <type chain>: <scalar fields>`, details at most 500 characters |
| <a id="log-fulldomainname"></a>`fullDomainName` | `SimplexDomain` to `name.simplex` or `name.testing` |

---

## 3. State

### 3.1 Memory

| Item | Holder | Written by | Read by |
|---|---|---|---|
| Actions | [`Actions.actions`](../src/actions.ts) map | `create`, `remove`, `forgetExpired` | tools, `Control` |
| Expiry timers | [`Actions.expiries`](../src/actions.ts), one per action | `scheduleExpiry`, `expire`, `remove` | |
| Open calls of an action | [`ActionRecord.open`](../src/actions.ts) | `openCall` and `closeCall` from [`actionResult`](../src/tools.ts) | `list`, `forgetExpired` |
| Collected replies | [`ConnectAction.messages`, `SendAction.responses`](../src/actions.ts) | `ChatItemsWatch.collect` | `connect`, `send` results |
| Item listeners | [`ChatItemsWatch.listeners`](../src/messages.ts) | `collect` | event dispatch |
| Downloads | [`AgentTools.downloads`](../src/tools.ts), one per file ID | `downloadTo` | file events, checks, `/delete CHAT files` |
| Forwards | [`AgentTools.forwarding`](../src/tools.ts), the copy for the post per file ID | `forwardFile` | `downloadFile`, before it moves a forwarded file |
| Rejected files | [`AgentTools.rejectedFiles`](../src/tools.ts) | [`downloadResult`](../src/tools.ts) | the file field; [`download`](../src/tools.ts), which then skips the files approval |
| Count of file descriptions | [`AgentTools.describedFiles`](../src/tools.ts), with the change signal `fileDescriptions` | `onFileDescription` | `fileServers` |
| Members of the control chat | [`Control.members`](../src/control.ts) | [`refreshMembers`](../src/control.ts) | approver checks |
| Mode, expiry, owners list, stop wait | `ServeOptions` of the run. The mode is kept in `Actions.mode` and in each action as well, [`ActionRecord.mode`](../src/actions.ts). | CLI | all modules |

### 3.2 Chat database custom data

| Item | Entity | Value | Written by | Read by |
|---|---|---|---|---|
| Control marker and counter | agent control chat | `{simplexMcp: {control: true, nextAction: N}}` | [`createControlChat`](../src/control.ts), [`saveCounter`](../src/control.ts) | [`findControlGroup`](../src/control.ts), [`controlDataOf`](../src/control.ts) |
| Standing approval of messages | contact or group of the agent | `{...other keys, simplexMcp: {approvedBy, approvedById, approvedAt}}` | [`approveChat`](../src/control.ts), [`revoke`](../src/control.ts) | [`standingApprover`](../src/control.ts) |
| Files approval | contact or group of the agent | `{...other keys, simplexMcpFiles: {approvedBy, approvedById, approvedAt}}` | [`approveChat`](../src/control.ts), [`revoke`](../src/control.ts) | [`standingApprover`](../src/control.ts) |

The profile, chats, items, members and file transfers are stored by the core in `DATA/simplex_chat.db`; connections and queues in `DATA/simplex_agent.db` (file prefix `DATA/simplex`, [`startServer`](../src/serve.ts)).

### 3.3 Files

| Path | Contents | Mode | Written | Read |
|---|---|---|---|---|
| `DATA/` | data folder | 0700, set at every start except on Windows | [`createDataFolder`](../src/files.ts) | |
| `DATA/token` | base64url of 32 random bytes | 0600, set again at every write except on Windows | first start, `--new-token` ([`serverToken`](../src/files.ts)) | every start |
| `DATA/headers.json` | `{"Authorization":"Bearer <token>"}` | 0600, set again at every write except on Windows | every start ([`writeHeadersFile`](../src/files.ts)) | Claude Code `headersHelper` |
| `DATA/server.json` | `{name, port, expireHours}` | 0600, set again at every write except on Windows | every start, after the HTTP listener is bound ([`startAgent`](../src/serve.ts)) | `register` |
| `DATA/lock` | PID and a newline | 0600 | start ([`createLock`, `createExclusive`, `takeOver`](../src/files.ts)) | next start |
| `DATA/tmp/` | the core's temp folder: files in transfer | 0700, set at every start | [`prepareCore`, `setTempFolder`](../src/core.ts) | the core |
| `DATA/files/group-N/`, `DATA/files/contact-N/` | downloaded files and files received without a download, named by file ID and extension | folder 0700 | the core at the download path ([`startDownload`](../src/tools.ts)); moves ([`moveFile`](../src/tools.ts)) | `download` results, `/forward`, `/leave`, `/delete CHAT files` |
| `DATA/forwarded/group-N/`, `DATA/forwarded/contact-N/` | files `/forward` downloaded, named as above, until a download moves them to `DATA/files/` | folder 0700 | the core at the forwarded path ([`forwardFile`, `startDownload`](../src/tools.ts)) | `/forward`; moved by `download` ([`downloadFile`](../src/tools.ts)); deleted by `leave`, `/leave` and `/delete CHAT files` ([`deleteChatFiles`, `leaveChat`](../src/tools.ts)) |
| `DATA/files/sent/` | resized copies of sent images, and copies of the files `/forward` posts, one folder per copy | folder 0700; a resized copy 0600 | [`resizedCopy`, `copyForSending`, `sentCopyPath`](../src/tools.ts) | the core; deleted at `sndFileCompleteXFTP`, `sndFileError` and at start ([`prepareCore`](../src/core.ts), [`AgentTools.onEvent`, `onSentFile`](../src/tools.ts)) |

The core library is downloaded once into `CACHE/core-7.1.3.0/sqlite`, and the Node.js add-on into `CACHE/nodejs-addon-cce06d8/`, each with a `Downloading ...` line on stderr. CACHE is `$XDG_CACHE_HOME/simplex-chat` or `~/.cache/simplex-chat`; on macOS `~/Library/Caches/simplex-chat`; on Windows `%LOCALAPPDATA%\simplex-chat` ([`cacheRoot`, `resolveLibsDir`, `resolveAddonPath`, `installLibs`, `installAddon`](../../simplex-chat-nodejs/src/libs.ts)).

---

## 4. Process lifecycle

### 4.1 Start

1. Data folder, lock, token and headers file are prepared ([`startServer`](../src/serve.ts)).
2. libsimplex for SQLite is loaded by `ChatApi.init`, and the databases are migrated with confirmation `yesUp` ([`startServer`](../src/serve.ts), [`ChatApi.init`](../../simplex-chat-nodejs/src/api.ts)).
3. Three log lines: version and NAME, data folder, mode ([`startServer`](../src/serve.ts)).
4. The active user is read, or created with display name `--display-name` or NAME, empty full name and peer type `agent` ([`startAgent`](../src/serve.ts)).
5. `prepareCore` reads the network config and sets it with `smpProxyMode` `always` and `smpProxyFallback` `allowProtected` ([`startAgent`](../src/serve.ts), [`prepareCore`](../src/core.ts)).
6. `prepareCore` sets `DATA/tmp` as the core's temp folder and deletes `DATA/files/sent` ([`prepareCore`](../src/core.ts)).
7. Events are buffered, and the chat is started ([`startAgent`](../src/serve.ts)).
8. A `--display-name` different from the profile, or a peer type other than `agent`, is applied with one `apiUpdateProfile` ([`startAgent`](../src/serve.ts), [`updatedUser`](../src/core.ts)).
9. The agent control chat is found by its marker, created with counter 1, or reset with `--reset-owners` ([`startAgent`, `controlGroup`](../src/serve.ts)).
10. `Actions`, `ChatItemsWatch`, `AgentTools` and `Control` are created; the counter is taken from the marker ([`startAgent`](../src/serve.ts)).
11. `Control.load` sets `Actions.lost` for a lost control chat. Otherwise it updates the group name and command menu when they differ in the control chat's profile read at the start, as in 0.2, and reads the members ([`Control.load`, `updateProfile`, `changeGroupProfile`](../src/control.ts)).
12. The HTTP server listens; with the port in use, the process exits with code 1 ([`startAgent`, `listen`](../src/serve.ts)). Until step 14 ends, requests to `/mcp` that pass the token check get 503 with `Retry-After: 1` ([`startHttp`](../src/http.ts), the `started` flag of [`startAgent`](../src/serve.ts)).
13. `server.json` is written ([`startAgent`](../src/serve.ts)).
14. The server runs the owner setup, `Control.start`; after an error, HTTP is closed and the error is rethrown ([`startAgent`](../src/serve.ts)).
15. `Listening on http://127.0.0.1:PORT/mcp` ([`startAgent`](../src/serve.ts)).
16. Buffered events are replayed, then live events are dispatched ([`startAgent`](../src/serve.ts), [`bufferEvents`](../src/core.ts)).

After an error inside `startAgent` (steps 4-16) the chat is closed; after any error once the lock is taken, the lock is released ([`startServer`](../src/serve.ts)).

### 4.2 Event dispatch

Each `onAny` receiver is awaited by the library before the next event is read ([`ChatApi.runEventsLoop`](../../simplex-chat-nodejs/src/api.ts)). The dispatcher, the event handler of [`startAgent`](../src/serve.ts), passes items of the control group to `Control` first. Every event then goes to [`AgentTools.onEvent`](../src/tools.ts), which runs the `AgentTools` and `ChatItemsWatch` handlers below. Finally, `Control` receives file errors and membership events:

| Event | Condition | Handler |
|---|---|---|
| `newChatItems` | item of the control group, main scope | [`Control.onMessage`](../src/control.ts) |
| `newChatItems` | every item | [`AgentTools.onNewItems`](../src/tools.ts), then [`ChatItemsWatch.deliver`](../src/messages.ts) |
| `chatItemUpdated` | | [`AgentTools.onFile`](../src/tools.ts), then [`ChatItemsWatch.update`](../src/messages.ts) |
| `chatItemsDeleted` | | [`ChatItemsWatch.remove`](../src/messages.ts) of each `deletedChatItem` |
| `contactConnected` | | [`AgentTools.onContactConnected`](../src/tools.ts) |
| `userJoinedGroup` | | [`AgentTools.onUserJoinedGroup`](../src/tools.ts) |
| `rcvFileComplete` | | [`AgentTools.onFileComplete`](../src/tools.ts) |
| `rcvFileSndCancelled` | | [`AgentTools.onFileCancelled`](../src/tools.ts) |
| `rcvFileError` | | [`AgentTools.onFileError`](../src/tools.ts) |
| `rcvFileDescrReady` | | [`AgentTools.onFileDescription`](../src/tools.ts) |
| `sndFileCompleteXFTP` | | [`AgentTools.onSentFile`](../src/tools.ts) |
| `sndFileError` | | [`AgentTools.onSentFile`](../src/tools.ts), then [`Control.fileNotSent`](../src/control.ts) |
| `joinedGroupMember`<br>`joinedGroupMemberConnecting`<br>`connectedToGroupMember`<br>`memberRole`<br>`leftMember`<br>`deletedMember`<br>`deletedMemberUser`<br>`groupDeleted` | event of the control group | [`Control.onEvent`](../src/control.ts), queued one at a time |
| any event | after `stop()` started | ignored |

Owner commands are queued in a second chain, independent of the membership chain ([`Control.onMessage`](../src/control.ts)).

### 4.3 Stop

On `SIGINT` or `SIGTERM` the server runs one stop; a later signal ends its wait for cancelled cards ([`serve`](../src/serve.ts)). `RunningServer.stop`, made by [`startAgent`](../src/serve.ts):

1. Events are ignored from now on.
2. `Actions.stop` starts: new actions are refused with `The server is stopping.`; the expiry timers are cleared; cards being posted are awaited; pending and performing actions are marked interrupted; the stop signal of the performs is aborted, which ends the wait of a `connect` for its connection; their cards are edited, or posted when they have no card; outcome edits already in progress are awaited ([`Actions.stop`](../src/actions.ts)).
3. In parallel with step 2, the HTTP server is closed with all connections, and open calls are aborted ([`stopHttp`](../src/http.ts)).
4. `Cancelled K action(s).` when K > 0 (`stop` in [`startAgent`](../src/serve.ts)).
5. When cards were edited or posted at the stop and the control chat is not lost, the server waits `--stop-wait` seconds, 10 by default, for them to be sent; a second signal ends the wait (`stop` in [`startAgent`](../src/serve.ts), [`waitForCancelledCards`](../src/serve.ts)).
6. `chat.close()`: the chat is stopped, then the store is closed ([`ChatApi.close`](../../simplex-chat-nodejs/src/api.ts)).
7. The lock is removed, `Stopped.` is logged, and the process exits with code 0, or 1 after an error (`stop` in [`startAgent`](../src/serve.ts), [`serve`](../src/serve.ts)).

Performs in progress are not awaited; when one ends, its card stays as edited at the stop and no line is logged, because the action is interrupted ([`Actions.runPerform`](../src/actions.ts)).

### 4.4 The lock

```
acquireLock(DATA, USER = server):                    files.ts
  create DATA/lock exclusively (flag wx), PID       createLock
  exists:
    PID = number in the file                        lockPid
    PID valid, other than ours, running:            isRunning, lockedError
      exit 1 "Another USER uses DATA (PID N). If no USER runs, remove DATA/lock."
    remove the file; create it exclusively again    takeOver
      exists again: exit 1 with the same message
releaseLock(DATA): remove DATA/lock
```

`isRunning`: `true` when `kill(pid, 0)` succeeds or fails with `EPERM` ([`isRunning`](../src/files.ts)).

---

## 5. Approval model

### 5.1 Modes

The mode is set by `--approve` for one run ([`serveOptions`](../src/cli.ts)).

| Mode | State of a new action | Card | `_meta` of `connect`, `send`, `download`, `leave` |
|---|---|---|---|
| `requests` | `pending`<br>`performing` for a send under a standing approval, and for a download under a files approval | card with commands<br>outcome as a new message after a standing send; none after a download without a card | none |
| `host` | `performing` | outcome as a new message after the perform; none for a download | `{"anthropic/requiresUserInteraction": true}` |

The mode in which an action is created is kept in `ActionRecord.mode`. The action's card and log line are written for that mode ([`Actions.create`](../src/actions.ts), [`Control.refusal`](../src/control.ts)).

Sources: [`Actions.create`](../src/actions.ts), [`registerTools`](../src/tools.ts), [`Control.outcome`, `hasOutcomeCard`](../src/control.ts). In both modes a new action is refused unless the control chat has an approver ([`Control.refusal`](../src/control.ts)).

### 5.2 Owners and approvers

| Term | Rule | Code |
|---|---|---|
| human | member other than the agent's membership, status `connected` or `complete` | [`Control.isHuman`](../src/control.ts), [`isActive`](../src/tools.ts) |
| human owner | human with role `owner` | [`Control.humanOwners`](../src/control.ts) |
| approver, `--owners` absent | member with role `owner` | [`Control.isApprover`](../src/control.ts) |
| approver, `--owners` without a list | none | [`Control.isApprover`](../src/control.ts) |
| approver, `--owners ID:NAME,...` | member with role `owner` whose `groupMemberId` and `displayName`, folded to one line by `oneLine`, match an entry | [`Control.isApprover`, `listsMember`](../src/control.ts) |

The command sender's member record from the chat item is checked ([`Control.onMessage`, `ownerCommand`](../src/control.ts)). The approver count for refusals and standing approvals is taken from `Control.members`, refreshed at load and at each membership event ([`Control.onEvent`, `refreshMembers`](../src/control.ts)).

Owner setup at start ([`Control.start`](../src/control.ts)):

| `--owners` | Start | First join by the link |
|---|---|---|
| absent | a human owner exists: register hint<br>otherwise: `No owners in the agent control chat.` when humans exist, then a link with role `member` | link deleted, role `owner`, greeting, log line, register hint |
| without a list | no humans: link with role `observer`<br>humans exist: their IDs and the restart command, in the log and the chat | link deleted, the joined member and the restart command announced |
| `ID:NAME,...` | each entry matched to a human, or exit 2<br>role `owner` set where missing<br>other humans removed<br>link deleted<br>`Owners: ...` and register hint | |

Sources: [`startOwners`](../src/control.ts), [`startObserver`](../src/control.ts), [`startListed`](../src/control.ts), [`ownerJoined`](../src/control.ts), [`observerJoined`](../src/control.ts).

Membership changes at run time ([`Control.onEvent`](../src/control.ts)):

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
| call creates the action, mode `requests` | | `pending` | [`Actions.create`](../src/actions.ts) |
| call creates the action, mode `host` | | `performing` | [`Actions.create`](../src/actions.ts) |
| `send` call under a standing approval, or `download` call under a files approval | | `performing` | [`Actions.create`](../src/actions.ts) |
| counter write or card post fails | `pending`<br>`performing` | `failed` | [`Actions.post`](../src/actions.ts) |
| stop arrives while the counter of a `performing` action is written | `performing` | `pending`, interrupted | [`Actions.afterPosting`](../src/actions.ts) |
| control chat lost | `pending` | `failed` | [`Actions.loseControl`](../src/actions.ts) |
| `/reject`, or `/leave CHAT` for a pending action of CHAT with a card | `pending` | `rejected` | [`Actions.reject`](../src/actions.ts), [`Control.leave`](../src/control.ts) |
| expiry time passes | `pending` | `expired` | [`Actions.expire`](../src/actions.ts) |
| `/approve N`, `/approve CHAT N` and `/approve CHAT all` for other actions; `/receive N`, `/receive CHAT N` and `/receive CHAT N all` for downloads | `pending` | `performing` | [`Actions.approve`](../src/actions.ts), the action requests of [`Control.ownerCommand`](../src/control.ts) |
| perform throws | `performing` | `failed` | [`Actions.runPerform`](../src/actions.ts) |
| perform ends | `performing` | `done` | [`Actions.runPerform`](../src/actions.ts) |

`create` ([`Actions.create`](../src/actions.ts)):

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

An action is removed from memory when its result is returned to a call ([`actionResult`](../src/tools.ts)). An action that is decided, without open calls and past its expiry time, is removed as well. This is checked at each decision, at the close of each call and at the action's expiry timer ([`Actions.forgetExpired`, `closeCall`, `decide`](../src/actions.ts)). `serve` has one session, `undefined`, so all its actions are counted against the limits of 20. A pending action of `serve` that expires is kept longer, see [5.6](#56-expiry). A repeated call before its removal is therefore answered with `expired`, as in 0.2 ([`Actions.expire`, `expireDue`](../src/actions.ts), [`Control.ownerCommand`](../src/control.ts)). A `download` still performing at the end of `wait` is returned as `performing` and kept ([`actionResult`](../src/tools.ts)).

### 5.4 Cards

The server posts a card when a `pending` action is created, and edits it with `apiUpdateChatItem` at the outcome ([`Actions.post`](../src/actions.ts), [`Control.outcome`](../src/control.ts)). For a `connect`, `send` or `leave` without a card (mode `host`, standing sends) the server posts the outcome as a new message; a `download` without a card gets none ([`hasOutcomeCard`](../src/control.ts)). The text per state is built by [`cardText`](../src/control.ts). A card of a `send` or `download` of an image or video file is an image or video message with the file's preview. In a plugin session group where files and media are refused by the group's preference, the card is text ([`Control.postCardContent`, `cardContent`, `fittedImage`](../src/control.ts)). The forms are listed in [API: cards](api.md#22-cards).

Outcomes are posted after `done` and `failed` ([`Actions.runPerform`](../src/actions.ts)), after `rejected` and `expired` ([`Actions.reject`, `expire`](../src/actions.ts)), and at stop ([`Actions.stop`](../src/actions.ts)). They are skipped after the control chat is lost ([`Actions.stop`, `runPerform`, `updateCard`](../src/actions.ts)).

### 5.5 Standing approvals

| Aspect | Messages | Files | Code |
|---|---|---|---|
| grant | `/approve CHAT all` from an approver in mode `requests`; pending sends of CHAT with a card are approved | `/receive CHAT N all` from an approver, on the card of download N; N is performed, and the other pending downloads of CHAT stay pending | [`Control.ownerCommand`, `approveChat`](../src/control.ts) |
| mode `host` | nothing stored; reply `Approvals apply in mode requests; this server runs in mode host.` | downloads get no card, so the command has none to act on | [`Control.ownerCommand`](../src/control.ts) |
| removal | `/revoke CHAT`: the `simplexMcp` key is removed, other keys kept | `/receive CHAT off`: the `simplexMcpFiles` key is removed | [`Control.revoke`](../src/control.ts) |
| use | mode `requests`, and `approvedById` equal to the `groupMemberId` of a current approver | the same | [`Control.standingApprover`](../src/control.ts) |
| scope | `send` | `download` of a file whose servers are all known | [`AgentTools.send`, `download`](../src/tools.ts) |
| decider shown | `safeName(approvedBy)` | the same | [`Control.standingApprover`](../src/control.ts) |
| storage | the chat's custom data; removed with the chat by `leave` | the same | [`AgentTools.leaveChat`](../src/tools.ts) |

Files of `reply` in a session group are sent without approval, and no standing approval applies to them. They are allowed or refused by the group's preference for files and media ([8.5](#85-approval-model)).

### 5.6 Expiry

| Rule | Code |
|---|---|
| `expires = created + expireHours × 3600000`, `expireHours` 1 to 23, default 23 | [`Actions.create`](../src/actions.ts), [`serveOptions`](../src/cli.ts) |
| one unreferenced timer per action, set at its creation for `expires - now`, at most 60 s, and set again until the expiry time. Before each owner command, all actions past `expires` are expired. | [`expiryCheck`, `Actions.scheduleExpiry`](../src/actions.ts), [`Control.ownerCommand`](../src/control.ts) |
| pending and due: `expired`, log `Action N expired.`, card edited to `Expired.` | [`Actions.expire`](../src/actions.ts) |
| decided, no open call, due: removed from memory. A pending action of `serve` that expires stays until the next owner command or its check 60 s later, as in 0.2. | [`Actions.forgetExpired`, `expire`, `expireDue`](../src/actions.ts) |
| connect wait for the other side: at most until `created + 24 h - 10 min` | [`editWindow`, `AgentTools.performConnect`](../src/tools.ts) |

A timer runs on the monotonic clock, while `expires` is a time on the wall clock. A timer that fires before `expires` is set again. So an action whose expiry time passes while the computer sleeps expires within 60 s after the computer wakes ([`Actions.expire`](../src/actions.ts)).

### 5.7 Action counter

| Rule | Code |
|---|---|
| initial value: `nextAction` of the control marker, or 1 | [`startAgent`](../src/serve.ts) |
| a new action takes `next`, then `next` grows by 1 | [`Actions.create`](../src/actions.ts) |
| the new `next` is written before the card is posted | [`Actions.post`](../src/actions.ts) |
| one write at a time, also after a failed write | [`Control.saveCounter`, `saveData`](../src/control.ts) |
| `--reset-owners` copies the value into the new control chat | [`controlGroup`](../src/serve.ts), [`resetControlChat`](../src/control.ts) |
| a refused call leaves the counter unchanged | [`Actions.create`](../src/actions.ts) |

---

## 6. Security boundaries

### 6.1 HTTP

Checks in order of the request listener of [`startHttp`](../src/http.ts):

| Check | Rule | Failure | Code |
|---|---|---|---|
| bind address | `127.0.0.1` only | | [`startHttp`](../src/http.ts) |
| Host | hostname `localhost`, `127.0.0.1` or `[::1]`, any port | 403, JSON-RPC error | [`hostHeaderValidation`, `localhostHostValidation`](../node_modules/@modelcontextprotocol/node/dist/index.cjs), [`validateHostHeader`, `localhostAllowedHostnames`](../node_modules/@modelcontextprotocol/server/dist/index.cjs) |
| Origin | absent or empty passes; otherwise the same hostnames, any port | 403, JSON-RPC error | [`originValidation`, `localhostOriginValidation`](../node_modules/@modelcontextprotocol/node/dist/index.cjs), [`validateOriginHeader`, `localhostAllowedOrigins`](../node_modules/@modelcontextprotocol/server/dist/index.cjs) |
| token | `Authorization` equal to `Bearer <token>`, same length, `timingSafeEqual` | 401, empty body | [`authorized`](../src/http.ts) |
| path | pathname `/mcp` | 404, empty body | [`startHttp`](../src/http.ts) |
| start | the agent control chat is started | 503, `Retry-After: 1`, empty body | [`startHttp`](../src/http.ts) |
| body size | 4 MiB | 413 | [`toWebRequest`](../node_modules/@modelcontextprotocol/node/dist/index.cjs) |
| content type of POST | `application/json` | 415 | `handle` in [`createMcpHandler`](../node_modules/@modelcontextprotocol/server/dist/index.cjs) |
| tool arguments | input schema | `isError: true` result | [`McpServer.validateToolInput`](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs) |
| tool results | output schema | `isError: true` result | [`McpServer.validateToolOutput`](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs) |

Node limits: `requestTimeout` 0, `timeout` 0, `headersTimeout` 60 s, `keepAliveTimeout` 5 s ([`startHttp`](../src/http.ts)). When the client closes the response stream, the call is aborted: the request signal is aborted in `toNodeHandler` on `close`, the per-request transport is closed, and `ctx.mcpReq.signal` is aborted by the protocol layer ([`toNodeHandler`](../node_modules/@modelcontextprotocol/node/dist/index.cjs), [`PerRequestHTTPServerTransport.handleMessage`](../node_modules/@modelcontextprotocol/server/dist/index.cjs), [`Protocol._onclose`](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs)). A result produced after the abort is discarded ([`Protocol._onrequest`](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs)).

### 6.2 Token and `headersHelper`

- The token is created once and kept in `DATA/token`; `--new-token` replaces it ([`serverToken`](../src/files.ts)).
- `DATA/headers.json` is rewritten at every start ([`writeHeadersFile`](../src/files.ts)).
- The Claude Code entry gets the command `cat <DATA>/headers.json`, or `type "<DATA>\headers.json"` on Windows, with the path quoted for the shell ([`registryEntry`](../src/register.ts), [`shellWord`, `shellPath`](../src/log.ts)). Only this command is written to the entry and to the `claude mcp add-json` arguments ([test "removes the entry before adding it, with no token in the command"](../test/unit/register.test.ts)).
- `token`, `headers.json` and `server.json` are set to mode 0600 at every write, and the data folder to 0700 at every start, except on Windows ([`createDataFolder`, `writePrivateFile`](../src/files.ts)).

### 6.3 Peer-written text

| Destination | Rule | Code |
|---|---|---|
| card and reply names | folded to one line; a valid display name is plain, others a code span | [`safeName`](../src/control.ts) |
| card text lines | one code span per line; backticks become `'`; a leading or trailing space becomes U+00A0; blank lines empty | [`codeLines`, `codeSpan`](../src/control.ts) |
| file names and paths on cards and posts | folded to one line, one code span | [`requestLines`, `Control.fileNotSent`](../src/control.ts) |
| reply quote on a send card | whitespace runs to one space, first 100 characters (code points), one code span | [`requestLines`](../src/control.ts) |
| quote for Claude, in `reply_to` and in the quote line of a reply | the first line, trimmed, without the bidi controls U+202A-U+202E and U+2066-U+2069, up to 200 characters (grapheme clusters, by `Intl.Segmenter`), the type included in the quote line, a space before `…` dropped, and `…` when the line is longer or more lines follow. Every Unicode line break ends a line. An empty quote line is `>` | [`replyText`, `quoteLine`, `trimmed`](../src/messages.ts) |
| link texts on a connect card, the input preview on a tool-use card of the plugin | code span lines in the bytes that the largest card form leaves; the line that does not fit is cut and ends in `…` inside its code span; later lines left out | [`fittedLines`](../src/control.ts) |
| mention names | valid name: `@name` or `@'name'`; others a code span | [`mentionText`](../src/control.ts) |
| errors on cards | folded to one line, one code span | [`cardText`](../src/control.ts) |
| `--owners` part of restart lines in the chat | one code span | [`inChat`, `restartText`](../src/control.ts) |
| terminal names and errors of actions | folded to one line | [`Actions.create`, `performedText`](../src/actions.ts), [`inLog`](../src/control.ts) |
| tool results | returned as data; schema descriptions and instructions mark `text`, `from`, `name`, file names, downloaded files, `description`, `welcome` and `commands` with their labels and params as data, not instructions; the instructions take a chat's messages as prompts only when the user said so in the session, and limit what the agent shares about its user | [`messageSchema`, `commandSchema`, `chatFieldSchemas`, `downloadOutput`, `instructions`](../src/tools.ts) |

The server writes the tappable form `/'...'` only in the card's command line ([`commandsLine`](../src/control.ts)), in `/'revoke CHAT'` of standing cards and replies ([`cardText`, `Control.approveChat`](../src/control.ts)), and in `/'delete CHAT files'` of the kept-files message ([`Control.leftChatFilesNote`](../src/control.ts)).

### 6.4 Chat references

| Rule | Code |
|---|---|
| `^[@#][1-9]\d{0,14}$`: 1 to 15 digits; the core reads `#(2^64 + N)` as `#N` | [`isChatRef`](../src/messages.ts), [test "a chat reference over 15 digits, which the core reads as the agent control chat, is refused"](../test/e2e/scenarios.test.ts) |
| the control chat is refused by `send`, `receive`, `download`, `leave` and the owner commands with CHAT, and left out of `list` and `/list` | [`AgentTools.receive`, `agentChats`, `knownChat`](../src/tools.ts), [`Control.agentChat`](../src/control.ts) |
| a store error of the chat read is reported as `No chat CHAT.` | [`AgentTools.receive`, `knownChat`](../src/tools.ts) |
| the agent's own links are refused by `connect` (`ownLink` plans) | [`classifyPlan`](../src/tools.ts) |

### 6.5 Owner commands

| Rule | Code |
|---|---|
| source: `groupRcv` text items in the main scope of the control group | [`startAgent`](../src/serve.ts), [`Control.onMessage`, `isGroupMessage`](../src/control.ts) |
| form: one line `/keyword params`, leading spaces allowed | [`parseCommand`](../src/control.ts) |
| A text outside the requests from any sender is answered with the help reply of `serve`, or with `Unknown command.` in a plugin session group. Other commands are accepted only from approvers. A command from another sender is answered in `serve`, and left without a reply in a plugin session group. | [`Control.ownerCommand`](../src/control.ts) |
| commands run one at a time in arrival order | [`Control.onMessage`](../src/control.ts) |
| `/approve`, `/reject`, `/receive` and `/forward` need a pending action with a card, and the action's chat when CHAT is given; `/receive` and `/forward` take downloads only, `/approve` the other actions only | [`Control.ownerCommand`](../src/control.ts) |

### 6.6 Files

| Rule | Code |
|---|---|
| `send` refuses files named `id_*`, `*.pem`, `*.key`, `*.p12` or `*.pfx`, and files under `~/.ssh`, `~/.gnupg`, the data folder and `~/.simplex-mcp` except the agent's own `DATA/files/`, each by its real path; the refusal is posted to the control chat and logged | [`AgentTools.attachment`, `isRefused`, `realFile`, `realFolder`](../src/tools.ts), [`Control.refusedFile`](../src/control.ts) |
| the file of a `send` is sent only when its real path, size, inode and change time at the perform are those of the request | [`AgentTools.fileToSend`](../src/tools.ts) |
| a sent JPEG or PNG is a resized copy made by the core | [`AgentTools.resizedCopy`](../src/tools.ts) |
| `ffmpeg` and `ffprobe` read a video as MP4 or QuickTime, local files only, without standard input, 30 s and 64 MiB of output each | [`videoFrame`, `videoDuration`, `commandOutput`](../src/tools.ts) |
| a download is saved only at `DATA/files/group-N/` or `DATA/files/contact-N/`, or for `/forward` at the same name under `DATA/forwarded/`, named by its file ID and an extension of 1 to 16 ASCII letters and digits | [`filePaths`, `storedName`, `chatFolder`](../src/messages.ts) |
| a forwarded file is outside `DATA/files/`, so `send` refuses it; a download moves it into `DATA/files/` after the copy for the post is made | [`AgentTools.downloadFile`, `forwardFile`, `isRefused`](../src/tools.ts) |
| a file on unknown servers gets a card that names them, also under a files approval; relays are approved only for such a card, and for every other download, and in mode `host`, the core checks the complete description and downloads a file on unknown servers only when the IP address is protected | [`AgentTools.download`, `downloadTo`, `startDownload`](../src/tools.ts), [`commandsLine`](../src/control.ts) |
| an accepted file that the core returns to `rcvInvitation` fails its download at the next check | [`AgentTools.checkDownload`](../src/tools.ts) |
| a received file is moved into the chat's folder at its completion, unless it is at the forwarded path, also from a member chat of a group, which has a download in memory as the main conversation does; a file without a download in memory, such as one whose transfer started before a restart, only when it has its full size; a partial file is deleted only for a download in memory, when it is no larger than the file | [`AgentTools.moveReceived`, `deletePartial`, `fileBytes`](../src/tools.ts) |

### 6.7 Link and network

- The join link is a group link of the control chat with role `member`, or `observer` with `--owners`; it is printed with a QR code on stdout and deleted at the first join event ([`Control.openLink`, `ownerJoined`, `observerJoined`](../src/control.ts)).
- At every start the server sets private message routing: `smpProxyMode` `always`, `smpProxyFallback` `allowProtected` ([`prepareCore`](../src/core.ts)).
- With `incognito: true` the join methods connect incognito ([`AgentTools.join`](../src/tools.ts)); the core then uses a random profile for that chat ([PLAN, Participants](../../../plans/2026-10-19-mcp-server.md#participants)).

---

## 7. Limits

| Limit | Value | Code |
|---|---|---|
| pending actions | 20 | [`maxPending`](../src/actions.ts) |
| actions being performed | 20 | [`maxPerforming`](../src/actions.ts) |
| expiry | 1 to 23 h, default 23 | [`serveOptions`](../src/cli.ts) |
| connect wait for the other side | `created + 23 h 50 min` | [`editWindow`](../src/tools.ts) |
| send, connect and download card | 15000 bytes JSON-escaped, in its largest form | [`maxCardLength`, `largestCard`, `encodedLength`](../src/control.ts) |
| error assumed in the largest card | 600 × `€` | [`longestError`](../src/control.ts) |
| message as the core sends it | 15602 bytes, counted with 600 bytes of envelope; `send` refuses a longer one, and the post of `/forward` leaves out its quote of the card | [`maxMessageLength`, `messageEnvelope`, `fitsMessage`](../src/tools.ts), [`Control.forward`](../src/control.ts) |
| `send.text` | 1 to 12000 characters | [`sendSchema`](../src/tools.ts) |
| `connect.message` | 1 to 2000 characters | [`connectSchema`](../src/tools.ts) |
| `send.mention` | 1 to 3 unique member IDs | [`sendSchema`](../src/tools.ts) |
| `wait` | 0 to 86400 s | [`connectSchema`, `sendSchema`, `receiveSchema`, `downloadSchema`](../src/tools.ts) |
| `idle` | 1 to 300 s, default 30 | [`connectSchema`, `sendSchema`, `defaultIdle`](../src/tools.ts) |
| `limit` | 1 to 100, default 50 | [`sendSchema`, `receiveSchema`, `defaultLimit`](../src/tools.ts) |
| `connect` snapshot | last 50 items | [`messagesOnConnect`](../src/tools.ts) |
| collected items per action or call | 100, then their IDs kept as overflow until deleted | [`maxCollected`, `ChatItemsWatch.collect`](../src/messages.ts) |
| unread page | 100 items | [`pageSize`](../src/messages.ts) |
| chat list read | one read of 1000000 chats | [`maxChats`](../src/tools.ts) |
| `download` wait, default | 60 s | [`downloadWait`](../src/tools.ts) |
| download check | every 60 s | [`downloadCheck`](../src/tools.ts) |
| download without progress | 30 min, then cancelled | [`downloadStall`](../src/tools.ts) |
| `ffmpeg`, `ffprobe` | 30 s, 64 MiB of output | [`mediaTimeout`, `maxOutputBytes`](../src/tools.ts) |
| stop wait | 0 to 60 s, default 10 | [`serveOptions`](../src/cli.ts) |
| error details | 500 characters | [`maxDetails`](../src/log.ts) |
| progress interval | 60 s | [`progressInterval`](../src/messages.ts) |
| expiry timer | at most 60 s, set again until the expiry time | [`expiryCheck`, `Actions.scheduleExpiry`](../src/actions.ts) |
| HTTP body | 4 MiB | [`DEFAULT_MAX_REQUEST_BODY_SIZE`](../node_modules/@modelcontextprotocol/server/dist/index.cjs) |
| SSE keep-alive | 15 s | [`DEFAULT_SSE_KEEP_ALIVE_MS`](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs) |
| agent NAME | 1 to 32 characters | [`validAgentName`](../src/cli.ts) |
| display name | 50 characters, `mkValidName` rules | [`prohibitedNameChars`, `maxNameLength`, `validName`](../src/messages.ts) |
| port | 1 to 65535, default 5226 | [`serveOptions`](../src/cli.ts) |

---

## 8. Claude Code plugin

The plugin runs the same tools, `Actions` and `Control` as `serve`, in other processes. ROOT is the plugin's data root, `~/.simplex-mcp` or `SIMPLEX_MCP_HOME` ([`dataRoot`](../src/files.ts)).

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

1. Claude Code calls a tool of the MCP server `simplex` ([`startSession`](../src/session.ts)). The SDK validates the arguments ([`registerTools`](../src/tools.ts)).
2. Without a connection, the session process checks the service's status and version, and spawns the service when it is not running. It then sends `POST /session` and holds `POST /attach` open ([`ServiceLink.open`](../src/session.ts), [`ServiceClient.ensureService`](../src/client.ts)).
3. The session process sends `POST /call` with the session ID, the tool and the input. It relays each `{progress}` line as an MCP progress notification, and returns the `{result}` line ([`ServiceLink.forward`](../src/session.ts)).
4. The service checks the token ([`listen`](../src/socket.ts)). It finds or opens the entry of the session or of its alias, and runs the session's `AgentTools.call` with a signal tied to the request ([`Service.onCall`](../src/service.ts)).
5. `AgentTools` sends chat commands to the core `_hub`. Actions are created in the session's `Actions`, and the session's `Control` posts them as cards in the session's group (the constructor of [`Session`](../src/hub.ts)).
6. The core emits events to `Hub.onEvent`. The hub passes them to the Main group, each session's `AgentTools.onEvent`, and the `Control` of the session group they belong to ([`Hub.onEvent`](../src/hub.ts)).

Data flow of a prompt from the session group:

1. `Session.onMessage` receives a message of a member in the group of session N ([`Hub.onEvent`](../src/hub.ts)). A one-line `/` command goes to `Control`. Another message is queued after the earlier ones, with the `replyTo` of its quote when it is a reply ([`Session.onMessage`](../src/hub.ts), [`toReplyTo`](../src/messages.ts)). `Session.prompt` drops a message of `User` and delivers the rest ([`Session.prompt`](../src/hub.ts)).
2. In the first 30 s after the hub opened, a delivery without an attachment waits for one. With an attachment, the file of the message, if any, is received. Then, if the session is not attached, the hub replies `Session N is not running.` Otherwise the prompt, after the quote line of a reply, is pushed, and the line `{prompt}` is written to every `/attach` stream of the session ([`Session.deliver`](../src/hub.ts), [`quoteLine`](../src/messages.ts), [`Service.onAttach`](../src/service.ts)).
3. The session process sends it to Claude Code as `notifications/claude/channel`, with `reply_to` and `reply_to_from` of a reply ([`startSession`, `channelNotification`](../src/session.ts)).
4. The hooks post the terminal prompts and the final answer of each turn with `POST /turn` ([`turn`](../src/hook.ts), [`Session.turn`](../src/hub.ts)).

### 8.2 Modules

#### [`session.ts`](../src/session.ts)

| Symbol | Contents |
|---|---|
| <a id="session-unattendedrun"></a>`unattendedRun`, `channelInstructions`, `permissionRequest` | the text of a run that is not interactive; instructions of the channel; schema of a permission request |
| <a id="session-startsession"></a>`startSession` | The session ID and the options are checked. Every call is answered with a text when the run is not interactive or the options are invalid. The `McpServer` gets the instructions, the capabilities `claude/channel` and `claude/channel/permission`, the tools with `reply`, the permission request handler and a stdio transport. Without such a text, the session process connects to the service in the background. |
| <a id="session-servicelink-connect"></a>`ServiceLink.connect`, `start`, `close`, `reconnect` | one shared connect promise, refused at once after a 403 of `/session`. After that refusal, and after `close`, `start` neither logs nor reconnects. Otherwise `start` logs a failure on stderr, and connects again later unless the failure is a `ProtocolError`. `close` ends the attachment and the retries. `reconnect` sets one timer that starts the connection again after the delay. The delay is 1 s at first, and doubles each time the timer fires, up to 30 s. |
| <a id="session-servicelink-permission"></a>`ServiceLink.permission` | `POST /permission` after the connect, and nothing after the refusal |
| <a id="session-servicelink-call"></a>`ServiceLink.call` | `forward`, repeated once after status 409. The service returns 409 for an unknown session before it runs the call. Failures are returned as tool errors, with the text of a run that is not interactive after the refusal. |
| <a id="session-servicelink-forward"></a>`ServiceLink.forward` | the `/call` stream of progress lines and the result line. The connection is dropped when the stream breaks. |
| <a id="session-servicelink-disconnect"></a>`ServiceLink.disconnect` | drops the connection only when it is the current one |
| <a id="session-servicelink-open"></a>`ServiceLink.open` | `ensureService`, then `/session` with the parent process ID. A 403 of `/session` is recorded as the refusal. `/attach` is awaited until its first line, and each line goes to the channel. When `/attach` ends, `reconnect` is called. |
| <a id="session-failuretext"></a>`failureText` | not running or status 503: `The SimpleX service stopped; repeat the call.` |
| <a id="session-channelnotification"></a>`channelNotification` | attach lines to `notifications/claude/channel`, with `reply_to` and `reply_to_from` from `replyTo`, and `notifications/claude/channel/permission` |

#### [`client.ts`](../src/client.ts)

| Symbol | Contents |
|---|---|
| <a id="client-serviceclient-forroot"></a>`ServiceClient.forRoot` | ROOT created, token, socket path |
| <a id="client-serviceclient-ensureservice"></a>`ServiceClient.ensureService` | The status is read. A service of a lower version is stopped, and a new one is spawned when none runs. The protocol is checked. |
| <a id="client-spawnservice"></a>`spawnService` | the detached launcher `node -e` `launcher` with stdout and stderr to `service.log`, a ready pipe on fd 3, and the environment variables of `serviceVariables`. The launcher spawns the service detached with its fd 1, 2 and 3, writes `error: TEXT` on fd 3 when that spawn fails, and exits at once, so the service is no descendant of the caller. |
| <a id="client-firstline"></a>`firstLine` | first line of the ready pipe, a spawn error, or the 5-minute timeout |
| <a id="client-compareversions"></a>`compareVersions` | numeric comparison of the first three version parts |

#### [`socket.ts`](../src/socket.ts)

| Symbol | Contents |
|---|---|
| <a id="socket-socketerror"></a>`SocketError` | message and HTTP status |
| <a id="socket-listen"></a>`listen` | `http.Server` on the socket path, with the token check, route table, body read, error replies and timeouts. A stale socket file is removed. |
| <a id="socket-sendjson"></a>`sendJson`, `startLines` | JSON reply; NDJSON stream |
| <a id="socket-isnotrunning"></a>`isNotRunning` | `ENOENT` or `ECONNREFUSED` |
| <a id="socket-requestjson"></a>`requestJson`, `streamLines` | client request with a JSON reply; client request with NDJSON lines; a status other than 200 to `SocketError` |
| <a id="socket-send"></a>`send` | `http.request` on the socket path, without a connection pool, bearer header |

#### [`protocol.ts`](../src/protocol.ts)

| Symbol | Contents |
|---|---|
| <a id="protocol-protocolversion"></a>`protocolVersion`, `unattendedStatus` | `1`; 403, the status of `/session` from a process marked unattended |
| <a id="protocol-owners"></a>`Owners`, `OwnerState` | the owners' names; with the Main group link, for `/title` and `/link` only |
| <a id="protocol-sessionrequest"></a>`SessionRequest`, `TitleRequest` | `/session`, `/title`, with `unattended` for the mark of a process |
| <a id="protocol-permissionrequest"></a>`PermissionRequest`, `TurnRequest` | `/permission`, `/turn` |
| <a id="protocol-attachline"></a>`AttachLine` | `{prompt}` lines, with the `replyTo` of a reply, and `{permission}` lines of `/attach` |
| <a id="protocol-titlereply"></a>`TitleReply`, `SessionReply`, `AttachRequest`, `CallRequest`, `CallLine` | replies and `/call` |
| <a id="protocol-statusreply"></a>`StatusReply`, `ResetReply` | `/status`, `/reset-owners` |

#### [`service.ts`](../src/service.ts)

| Symbol | Contents |
|---|---|
| <a id="service-hublike"></a>`HubLike`, `SessionLike`, `ServiceDeps` | interfaces that the service uses, so that tests can replace the hub |
| <a id="service-entry"></a>`Entry` | opening promise, session, open attachments, their writers, running calls, idle timer, the session server's parent process ID and the time it was last seen |
| <a id="service-service-start"></a>`Service.start` | The lock is taken, and the stored sessions are read with their aliases and the next number. The listener starts before the hub opens, so that a starter that lost the lock race gets an answer to `/status` right after `running`. After the hub opens, the stop timer starts, the titles held at the last stop are taken as new `/title` requests, and the hourly log trim starts. |
| <a id="service-service-routes"></a>`Service.routes`, `withHub`, `expiring` | the ten routes. Requests wait for the hub, and due actions expire first. |
| <a id="service-service-shutdown"></a>`Service.shutdown` | stop sequence; the held titles saved |
| <a id="service-service-onsession"></a>`onSession`, `onTitle`, `markUnattended`, `refuseUnattended`, `applyTitle` | The options are checked. For a title with `unattended`, the service removes the marks of 10 s ago and older, marks the title's process, and ends a server's wait for the title of that process. It opens no session for such a title. The service refuses a registration from a process marked in the last 10 s with 403, before and after its wait. At a registration, the service records the parent process ID, names the session by a held title of its ID, and signals the registration. The service holds a title of a new ID that comes from a process without a known session, or from a process whose title is held. The title is held until the server of that process registers, 10 s at most, and is applied only while the service runs. When a title is applied, the service opens or creates the session, sets its name, passes its model with the title's `at`, and records the process of the title. |
| <a id="service-service-canonical"></a>`canonical`, `resolve`, `attachedSession`, `processSession`, `alias` | An alias is resolved to its session. With `session.json`, an ID is resolved to itself, or, for a hook, to the session attached by its Claude Code process, without an alias. For any other ID, a session server first waits for the title of its process. The ID is then resolved to the session of the same Claude Code process, when the process has one, and recorded as its alias in `session.json`. |
| <a id="service-service-rename"></a>`rename` | A new name of the session is written into `session.json`, as read after the group's rename. |
| <a id="service-service-onattach"></a>`onAttach`, `onCall`, `hold` | attachment with the session's line writer; call stream; streams refused while stopping |
| <a id="service-service-entry"></a>`entry`, `refuseWhenStopping`, `folder`, `openAgent` | the session registry. The status is 503 while the service stops, and 400 for an ID that is not a session ID. The status is 409 for an unknown session on the routes other than `/session` and `/title`. A new session is given a new number and name, and the aliases that already point to it. |
| <a id="service-service-scheduleidle"></a>`scheduleIdle`, `scheduleStop` | idle close of a session, idle stop of the service |
| <a id="service-runservice"></a>`runService` | the process entry: ROOT, start, ready line and signal handlers. For a held lock, the start runs again once the lock is released, and the ready line is `running` when its holder answers. |
| <a id="service-productiondeps"></a>`productionDeps` | token, `Hub.open` |
| <a id="service-processevent"></a>`processEvent` | the next event of a PID on `titled` or `registered`, 10 s at most |
| <a id="service-lockholder"></a>`lockHolder` | up to 150 checks, 100 ms apart. The result is `released` when ROOT/service.lock is gone, and `answers` for any result of a socket request other than ENOENT and ECONNREFUSED. |
| <a id="service-sessionname"></a>`sessionName` | session title, else the project folder name, else `Claude Code` |
| <a id="service-validconfig"></a>`validConfig` | the check of `config` in `/session` and `/title`: a valid Main group name, the mode, and hours from 1 to 23. Other values are refused with status 400. |
| <a id="service-readychannel"></a>`readyChannel`, `writeReady` | ready line to fd `SIMPLEX_MCP_READY_FD`, or stdout. A ready line to a reader that is gone, `EPIPE`, is dropped, and the service keeps running. |

#### [`hub.ts`](../src/hub.ts)

| Symbol | Contents |
|---|---|
| <a id="hub-constants"></a>constants, from `profileName` to `commandsApproversOnly` | profile name `Claude`, session data key, first action number, 10 s stop wait, 60 s echo of a raw prompt, 20 pushed prompts, 10 minutes for a prompt's file, 1 day for a pending connection, 30 s of held prompts after the hub opens, the channel tags of this plugin, the one reply to the `/` commands of a member who is not an admin or owner |
| <a id="hub-hubtexts"></a>`hubTexts` | refusal texts of a lost session group, of a stop and of the limit of pending actions, which names the approvers |
| <a id="hub-sessiongroupdata"></a>`SessionGroupData` | session number; pending connections of `connect` with their time; the approved mode |
| <a id="hub-session"></a>`Session` | one Claude Code session, built from its `session.json` and its group's data. `Actions` get the hub's texts and the mode of the group's data, or `requests` without one. `AgentTools` get the session's ID and folder and the group for `reply`. `Control` of the session group gets the session's approver checks. The stored model and effort are kept as well. |
| <a id="hub-session-busy"></a>`Session.busy` | an action other than a `mode` card in memory |
| <a id="hub-session-requestmode"></a>`Session.requestMode`, `askMode` | `requests` applies at once. The hub then ends pending `mode` cards and clears `hostDeclined`. `host` is remembered and asked as a `mode` action while an approver is in the group and `hostDeclined` is unset. The request is forgotten after a rejected or expired card, and `hostDeclined` is set after a rejection. |
| <a id="hub-session-online"></a>`Session.onLine` | the writer of the session's attachments. The held prompts are woken at each change. |
| <a id="hub-session-onmessage"></a>`Session.onMessage` | The hub answers a `/` command from a member who is not an approver with the one refusal, found by `refusesCommand` and posted on the `delivery` chain. The remaining one-line `/` texts go to `Control`, and other messages go to `prompt` on the `delivery` chain, in arrival order. The `replyTo` of a reply is computed here from its quote, with the hub's profile name as Claude's name, and passed with the message. |
| <a id="hub-session-claimpending"></a>`Session.claimPending` | When a connection of `connect` recorded in the last day completes, its record is removed and its chat is claimed. |
| <a id="hub-session-permission"></a>`Session.permission` | a `permission` action in mode `requests`. Its verdict is written to the attachments, and the action is removed when decided. |
| <a id="hub-session-turn"></a>`Session.turn` | Before any await, at an answer or a prompt that is not mid-turn, the permissions created before its hook started are ended. An answer's quote is chosen, and its hook's start is recorded as the latest answer's hook start. A turn record is added for a prompt whose hook started no earlier than the latest answer's hook, unless the prompt is mid-turn and matched no pushed prompt. The model and the effort level of the request are taken only when its `at` is not older than the `at` of the value taken last, kept apart for each (`newer`). A `/title` passes its model with the `at` of its SessionStart hook. A request without `at` counts as the current time. A changed description is recorded in `session.json`. After these steps, Claude posts the request's `note` without a quote, and `User` posts the terminal prompt unless its `typed` is false, or it is blank, made only of this plugin's channel tags, pushed or a command. A prompt without `typed` is posted as typed. The turn record of a prompt is kept whatever its `typed`. Claude posts the answer unless it is blank. The changed short description is set, even after a failed post. A prompt, a note or an answer that the core refuses for a group preference is replaced by Claude's post with the preference's name. |
| <a id="hub-session-endpermissions"></a>`Session.endPermissions`, `fileDone` | the end of pending permissions created before a time; the end of a prompt's file |
| <a id="hub-session-invite"></a>`Session.invite`, `announceRights` | invitation through the Main group. At a member's join or role change, other than `User`'s, the rights of the member's role are posted. The `/observer` command is added only for a role that can send, other than owner. |
| <a id="hub-session-prompt"></a>`Session.prompt` | a message of `User` dropped; the rest delivered |
| <a id="hub-session-deliver"></a>`Session.deliver`, `receive`, `turnQuote`, `answer`, `postRefusal`, `post`, `pushedIn` | `deliver` waits for the held prompts, and then receives the file while the session is attached. It writes the prompt line with the file's path. For a reply, it writes the given `replyTo` as the quote line before the text, in the pushed prompt and in the line's `text`, and its ID and sender as the line's `replyTo`. After a failure of the file, the line is written without the path, and a reply is posted. Without an attachment, the reply is `Session N is not running.` `receive` receives the file into the session's folder within 10 minutes. `turnQuote` returns the newest turn record before an answer, or else the oldest pushed prompt not quoted yet, which it marks quoted. `answer` posts the answer in parts, with its quote when the quote fits with the answer's first character. `postRefusal` replaces a post that the core refuses with `feature not allowed NAME` by the subject and the text with the preference's name. The text is made by `featureOff` for an answer, and by `notAllowedForUser` for a prompt of `User`. `post` sends a post to the group, and sends it again without its quote when the core refuses the quote. `pushedIn` returns the pushed prompts contained in a turn's prompt. |
| <a id="hub-session-queue"></a>`Session.queue`, `refusesCommand` | the `delivery` chain; a one-line `/` text of a member other than `User` who is not an approver |
| <a id="hub-session-approves"></a>`Session.approves`, `isUser`, `setMode`, `declineHost`, `store`, `savePending` | approver check: not `User`, the role `admin` or `owner` in the session group; the member of the contact `Hub.userContactId`; the mode and its record in the group's custom data; `hostDeclined` and its record; fields written into `session.json`; a pending connection recorded, those older than a day dropped |
| `Hub.userContactId` | the hub's contact marked `simplexMcpUser`, read at the start |
| <a id="hub-hub-open"></a>`Hub.open` | core `_hub`, Main group found or created, `MainGroup` load and start, `User` unless it fails, the contact of `User`, the stored sessions, events |
| <a id="hub-hub-configure"></a>`Hub.configure`, `resetOwners` | expiry of all sessions and the Main group name. The Main group is replaced at once, even while owners exist. |
| <a id="hub-hub-session"></a>`Hub.session` | the open session, renamed when its name changed, or a new `Session` with its stored or new group. The owners are invited to a new group. |
| <a id="hub-hub-sessionstext"></a>`Hub.sessionsText`, `join`, `inviteOwner` | `SessionGroups` of the Main group |
| <a id="hub-hub-stop"></a>`Hub.stop` | actions stopped, events awaited, cancelled cards awaited up to 10 s, cores closed |
| <a id="hub-hub-storedgroup"></a>`storedGroup`, `createSessionGroup` | the group of `session.json` with a matching session number; a new group with `User` added |
| <a id="hub-hub-onevent"></a>`Hub.onEvent` | event dispatch. Pending connections are claimed at `contactConnected` and `userJoinedGroup`. A session group's membership events go to its `Control`, `announceRights` and `askMode`, one at a time. |
| <a id="hub-notallowedforuser"></a>`notAllowedForUser` | `NAME are not allowed for User in the group of this session; its owners can allow them in the group's preferences.` It is posted for a prompt of `User` refused by a group preference, including one that is on only for roles above that of `User`. |
| <a id="hub-textparts"></a>`textParts` | the longest runs of whole characters that each fit in one message by `fitsMessage`, the first with the given quote. Each part is found by a binary search among the next `maxMessageLength` characters. |
| <a id="hub-isrefusedquote"></a>`isRefusedQuote` | `errorStore` `invalidQuote`, `chatItemNotFound` or `largeMsg` |

#### [`main-group.ts`](../src/main-group.ts)

| Symbol | Contents |
|---|---|
| <a id="main-group-maincommands"></a>`mainCommands`, texts from `mainKey` to `shortDescr` | command menu; greeting, reply to an unknown command, log line; the welcome message, the text of the join post and the short description |
| <a id="main-group-sessiongroups"></a>`SessionGroups` | what the Main group asks of the hub |
| <a id="main-group-maingroup-owners"></a>`MainGroup.owners` | human members with role `owner` while the group is not lost |
| <a id="main-group-maingroup-load"></a>`MainGroup.load`, `start` | lost check, name and menu from the group's current profile, and members. The short description and welcome message are kept. Without owners, `start` takes the existing link, or creates a new one. |
| <a id="main-group-maingroup-onmessage"></a>`MainGroup.onMessage`, `command` | `/sessions`, `/join N`; `Unknown command.` for another |
| <a id="main-group-maingroup-onevent"></a>`MainGroup.onEvent`, `handleEvent`, `ownerJoined` | The members are read again. The first human who joins becomes an owner. A new link is created when no owner remains. The statement of trust is posted at each human's join. New owners are invited to the open sessions. |
| <a id="main-group-maingroup-replace"></a>`MainGroup.replace` | The Main group is deleted for all its members, and a new one is created with a new link. The session groups are kept. |
| <a id="main-group-maingroup-openlink"></a>`openLink`, `deleteLink` | group link with role `member` |
| <a id="main-group-findmaingroup"></a>`findMainGroup`, `createMainGroup` | the newest group with `simplexMcpMain`; a new one with the short description and welcome message of trust, history, direct messages for owners and the menu |

#### [`user.ts`](../src/user.ts)

| Symbol | Contents |
|---|---|
| <a id="user-user-open"></a>`User.open` | core `_user` with profile `User`, events, a connection to `Claude` when the hub has no contact marked `simplexMcpUser` |
| <a id="user-user-post"></a>`User.post` | messages to the group marked with the session number |
| <a id="user-user-connect"></a>`User.connect` | a one-time invitation of the hub, joined by `User`. The `contactConnected` of that invitation's connection is awaited for 60 s, and the hub's contact is then marked. |
| <a id="user-user-onevent"></a>`User.onEvent` | The contact is marked `simplexMcpClaude`. Invitations of that contact to a group named `N · ...` are joined and marked. |
| <a id="user-usercontact"></a>`userContact` | the hub's contact marked `simplexMcpUser` |

#### [`core.ts`](../src/core.ts)

| Symbol | Contents |
|---|---|
| <a id="core-opencore"></a>`openCore` | folder, `ChatApi.init` with prefix `FOLDER/simplex`, active user or a new one with peer type `agent`, `prepareCore`, event buffer, `startChat`, preset contact deleted for a new user, profile update |
| <a id="core-deletepresetcontacts"></a>`deletePresetContacts` | the contact `Ask SimpleX Team`, deleted when it has no connection |

#### [`hook.ts`](../src/hook.ts), [`owner.ts`](../src/owner.ts), [`qr.ts`](../src/qr.ts)

| Symbol | Contents |
|---|---|
| <a id="hook-sessionstart"></a>[`sessionStart`](../src/hook.ts) | `{}` for a run that is not interactive. When the run is not interactive only because of `CLAUDE_CODE_SESSION_ATTENDED`, its process is marked first. In an interactive run, the hook checks Node.js, reads the options, calls `ensureService`, sends `POST /title` with the title, the folder, the model and its own start time `performance.timeOrigin`, and returns the link message. |
| <a id="hook-markunattended"></a>[`markUnattended`](../src/hook.ts) | When `CLAUDE_PID` is set, the hook calls `ensureService` and sends `POST /title` with `unattended`. A failure is logged. |
| <a id="hook-turn"></a>[`turn`](../src/hook.ts) | The output is `{}`. In a run that is not interactive, the hook sends nothing. Otherwise it calls `ensureService`, then sends `POST /turn` with the fields of [8.6](#86-channel-and-mirroring). The request is tried up to 15 times, 1 s apart, while the service returns status 409. |
| <a id="hook-answermodel"></a>[`answerModel`, `promptKind`, `findInTranscript`, `readTail`, `lastAnswer`, `promptRecord`, `isPrompt`, `recordText`, `recordKind`](../src/hook.ts) | `answerModel` finds the model of Stop's answer. It is the `message.model` of the last entry of type `assistant` without `isSidechain` in the tail of `transcript_path`, once that entry's text is the answer. No model is sent for `<synthetic>`, an answer without text, an empty path, an unreadable transcript, or an entry not written within the reads. `promptKind` finds whether a person typed the prompt of UserPromptSubmit, its note and whether it is mid-turn. Its record, found by `promptRecord`, is the newest user entry or `queued_command` attachment whose text is the prompt, without subagent entries and tool results: the entry with the hook's `prompt_id` as `promptId`, or a record after the first user entry with that ID, a tool result included. Until such an entry is in the tail, `promptRecord` waits; at the last read, and at once without `prompt_id`, it takes any record with the prompt's text. It also waits while an `enqueue` entry with the prompt's text follows the record. The prompt is typed when the record's `origin.kind` is `human`, and mid-turn when the record is not the entry with `prompt_id`. A `queued_command` attachment without origin and with the `commandMode` `task-notification` is a task notification. At the last read, a record with an `enqueue` entry after it is classified by its origin, and a typed one is mid-turn. Without a record, an `enqueue` entry with the prompt's text makes the prompt typed and mid-turn; otherwise an entry with `prompt_id` and the origin `human` makes it typed. `findInTranscript` reads the tail, the last 1 MiB plus twice the bytes of the answer or the prompt, 100 ms apart: at most 20 times for an answer and 600 times for a prompt, and it tells its search which read is the last. It treats a failed read, as of a missing file, as an empty tail. |
| <a id="hook-noteof"></a>[`noteOf`, `taskNote`, `teammateNote`, `idleNotification`, `peerName`, `firstLine`, `decoded`, `stringOf`, `shortened`](../src/hook.ts) | `noteOf` makes the note of a record from its origin, its `turnOrigin` and the prompt, as [api.md](api.md#48-plugin-options-files-and-hook-output) lists the lines. `taskNote` reads `<summary>`, `<event>` and `<status>` of the first `<task-notification>` block, and takes the first line of a prompt without one. `teammateNote` reads the sender, the `summary` and the message of the first `<teammate-message>` tag of a prompt without origin whose message is not an idle notification; `idleNotification` checks whether a message's JSON `type` is `idle_notification`. `firstLine`, `peerName` and `teammateNote` replace control and format characters with spaces in each first line and in the sender's name, as `oneLine` does, so a note is one line. `shortened` cuts a note to 200 characters, counted in code points. |
| <a id="owner-linkcommand"></a>[`linkCommand`, `resetCommand`](../src/owner.ts) | `npx -y @simplex-chat/mcp@VERSION link` and `reset-owners`, VERSION of this package |
| <a id="owner-linktext"></a>[`linkText`, `resetOwnersText`](../src/owner.ts) | the Main group's name with the owners, or with the link and a QR code on a terminal only |
| <a id="owner-showlink"></a>[`showLink`, `resetOwners`](../src/owner.ts) | `link` takes the link from `GET /link` while no owner is in the Main group. `reset-owners` replaces the Main group at once, and shows the new link on a terminal only. |
| <a id="qr-qrcodetext"></a>[`qrCodeText`](../src/qr.ts) | `small` and `large` with ANSI colors for terminals, `plain` for the hook message |

#### [`config.ts`](../src/config.ts), [`files.ts`](../src/files.ts)

| Symbol | Contents |
|---|---|
| <a id="config-interactiveentrypoint"></a>[`interactiveEntrypoint`, `interactive`](../src/config.ts) | `false` for `CLAUDE_CODE_ENTRYPOINT` `sdk-cli`, `sdk-ts` or `sdk-py`. `interactive` is `false` for `CLAUDE_CODE_SESSION_ATTENDED` `0` as well. |
| <a id="config-config"></a>[`Config`, `defaultConfig`, `configVariables`](../src/config.ts) | the Main group name, the mode and the expiry hours; their defaults `Claude Control`, `requests` and 23; their variables |
| <a id="config-readconfig"></a>[`readConfig`](../src/config.ts) | `CLAUDE_PLUGIN_OPTION_*` to `Config`, defaults for blank values, `ConfigError` texts |
| <a id="config-withmachine"></a>[`withMachine`](../src/config.ts) | ` - MACHINE` from `SIMPLEX_MCP_MACHINE` after the Main group name, checked by `validName`, with the `ConfigError` texts of the variable |
| <a id="files-dataroot"></a>[`dataRoot`, `hubFolder`, `sessionFolder`](../src/files.ts) | ROOT, `ROOT/_hub`, `ROOT/_sessions/<lower-case ID>` |
| <a id="files-sessionfile"></a>[`SessionFile`, `readSessionFile`, `writeSessionFile`, `storedSessions`](../src/files.ts) | `session.json` with number, name, project, creation time, group, aliases, model, effort and a rejected mode `host`. An unparsable file is skipped with a log line. Each write goes to `session.json.tmp`, which is renamed into place. |
| <a id="files-socketpath"></a>[`socketPath`](../src/files.ts) | `ROOT/service.sock`. A path over 103 bytes is replaced by `TMP/simplex-mcp-HASH.sock`, and Windows uses `\\.\pipe\simplex-mcp-HASH`. |
| <a id="files-logfile"></a>[`logFile`, `trimLog`, `openLog`](../src/files.ts) | `ROOT/service.log`, emptied when over 10 MB, mode 0600 |
| <a id="files-servicelock"></a>[`serviceLock`, `serviceToken`](../src/files.ts) | `service.lock` and `ROOT/service.token`, the plugin's lock and token. Both names are written with a dot, which `serve` refuses in agent names, so the folders of agents in `~/.simplex-mcp` are not touched. |

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
| Entries of open sessions | [`Service.entries`](../src/service.ts) | `entry`, idle close, stop | routes, `status` |
| Line writers of the attachments | [`Entry.writers`](../src/service.ts) | `onAttach` | `Session.onLine` |
| Stored sessions, next number | [`Service.stored`, `nextNumber`](../src/service.ts) | `Service` constructor, `openAgent` | `openHub`, `openAgent` |
| Aliases of sessions | [`Service.aliases`](../src/service.ts), ID to session ID | `Service` constructor, `alias` | `canonical`, `resolve` |
| Titles by process | [`Service.titles`](../src/service.ts), PID to session ID, time and the name of a held title, and the `titled` signal | `onTitle`, `applyTitle` | `processSession`, the wait of `resolve`, `onSession` |
| Registrations by process | [`Service.registered`](../src/service.ts), the signal of a `/session` with that PID | `onSession` | the wait of a held title |
| Held titles | [`Service.held`](../src/service.ts), PID to its last held `/title` request | `onTitle`, the end of its wait | `onTitle`, `shutdown` |
| Processes of unattended runs | [`Service.unattended`](../src/service.ts), PID to the time of its mark | `onTitle` with `unattended` | `onSession` |
| Process of a session server | [`Entry.pid`, `Entry.seen`](../src/service.ts) | `onSession`, the end of the last attachment | `processSession` |
| Sessions | [`Hub.sessions`](../src/hub.ts), by number | `Hub.session` | events, Main group commands |
| End of the held prompts | [`Hub.promptsHeldUntil`](../src/hub.ts), 30 s after the stored sessions opened | `Hub.open` | `Session.deliver` |
| Prompts in delivery | [`Session.delivery`](../src/hub.ts), a promise chain in arrival order | `onMessage` | `deliver` |
| Actions of a session | [`Session.actions`](../src/hub.ts), numbered from 1 per session group | tools, `permission`, `requestMode` | `Control`, `busy` |
| Pushed prompts | [`Session.pushed`](../src/hub.ts), at most 20, each kept until it is matched by a turn's prompt. An entry is marked `quoted` once it is quoted by an answer without a turn record. | `deliver`, `turnQuote` | `turn`, `turnQuote` |
| Files of prompts in transfer | [`Session.receiving`](../src/hub.ts) | `receive`, its 10-minute timer | `fileDone` |
| Model and effort | [`Session.description`](../src/hub.ts), also in `session.json` | constructor, `turn` | `turn` |
| Turn records | [`Session.turns`](../src/hub.ts), the start time of each prompt hook that started after the latest answer's hook, with its pushed prompt, or `null` for a terminal prompt or a command | `turn`. At an answer, the records that started before its hook are removed. | `turnQuote` |
| A rejected mode `host` | [`Session.hostDeclined`](../src/hub.ts), also in `session.json` | constructor, `declineHost` | `askMode`, `requestMode` |
| The requested mode | [`Session.requested`](../src/hub.ts) | `requestMode`, a rejected or expired `mode` card | `askMode` |
| Members of the Main group, its link | [`MainGroup.members`, `linkText`](../src/main-group.ts) | `refreshMembers`, `openLink` | owner checks, the statement of trust at a join, `/status`, `/title`, `/link` |

A session process keeps one connection promise, one attachment, the refusal of the service, and the retry delay with its timer in memory (the fields of [`ServiceLink`](../src/session.ts)).

Chat database custom data is in the core `_hub` unless noted:

| Item | Entity | Value | Written by | Read by |
|---|---|---|---|---|
| Main group marker | Main group | `{simplexMcpMain: true}` | [`createMainGroup`](../src/main-group.ts) | [`findMainGroup`](../src/main-group.ts) |
| Session group | group of session N | `{simplexMcpSession: {session: N, pending?: [{connId, at}], mode?}}`, where `mode` is the approved mode | [`createSessionGroup`](../src/hub.ts), [`setMode`](../src/hub.ts), [`savePending`](../src/hub.ts), [`claimPending`](../src/hub.ts) | [`storedGroup`](../src/hub.ts), [`Session` constructor](../src/hub.ts), [`claimPending`](../src/hub.ts) |
| Action counter | group of session N | `{simplexMcp: {control: true, nextAction}}`, merged with the other keys | [`saveCounter`, `saveData`](../src/control.ts) | [`Session` constructor](../src/hub.ts) |
| Chat owner | contact or group of a peer | `{...other keys, simplexMcpOwner: {session: ID}}` | [`claim`](../src/tools.ts), [`giveChat`](../src/tools.ts) | [`owns`](../src/tools.ts) |
| Standing approvals | contact or group of a peer | as in [3.2](#32-chat-database-custom-data) | `Control` of the owning session's group | the same |
| User contact | the hub's contact of `User` | `{simplexMcpUser: true}` | [`User.connect`](../src/user.ts) | [`userContact`](../src/user.ts) |
| Claude contact | in `_user`: the contact of `Claude` | `{simplexMcpClaude: true}` | [`User.onEvent`](../src/user.ts) | [`User.onEvent`](../src/user.ts) |
| Session of a group | in `_user`: a session group | `{simplexMcpSession: N}` | [`User.onEvent`](../src/user.ts) | [`User.post`](../src/user.ts) |

Files:

| Path | Contents | Written | Read |
|---|---|---|---|
| `ROOT/` | data root, mode 0700 | every client and the service ([`ServiceClient.forRoot`](../src/client.ts), [`runService`](../src/service.ts)) | |
| `ROOT/service.token` | the bearer token, 0600 | the first client or service ([`serviceToken`, `serverToken`](../src/files.ts)) | every client, the service |
| `ROOT/service.lock` | PID of the service, linked from `ROOT/service.lock.PID.HEX.tmp`. A stopped service's lock is claimed through `ROOT/service.lock.PID.claim`, which is removed after the takeover. | [`Service.start`](../src/service.ts), with [`createLock`, `createExclusive`, `takeOver`](../src/files.ts) | the next service |
| `ROOT/titles.json` | the `/title` requests held at a stop, each with its `at`, 0600 | [`Service.shutdown`](../src/service.ts) | the next [`Service.start`](../src/service.ts), which removes it |
| `ROOT/service.sock` | the socket | [`listen`](../src/socket.ts) | clients |
| `ROOT/service.log` | the service's stdout and stderr, emptied over 10 MB at a spawn and every hour | [`spawnService`](../src/client.ts), [`trimLog`](../src/files.ts) | people |
| `ROOT/_hub/`, `ROOT/_user/` | databases and `tmp/` of the profiles `Claude` and `User` | [`openCore`](../src/core.ts) | the cores |
| `ROOT/_sessions/ID/session.json` | `{number, name, project, created, group?, aliases?, model?, effort?, hostDeclined?}`, 0600, replaced whole through `session.json.tmp` | [`Service.openAgent`, `rename`, `alias`](../src/service.ts), [`Hub.session`, `Session.declineHost`, `store`](../src/hub.ts) | `Service` constructor, `resolve`, `Hub.session`, `alias`, `store` |
| `ROOT/_sessions/ID/files/` | downloads of the session's chats as in [3.3](#33-files). The files of the prompts from the group are in `group/`. `sent/` is deleted when the session opens. | the core ([`Session.receive`, `Hub.session`](../src/hub.ts)) | tools, Claude |

### 8.4 Process lifecycle

#### Session start

1. The SessionStart hook checks Node.js 22, reads the options, starts the service when it is not running, and sends `POST /title` with the session ID, `CLAUDE_PID`, its start time `at`, title, project folder, model and options ([`sessionStart`](../src/hook.ts)). In a run that is not interactive, the hook returns `{}`. When the run is not interactive only because `CLAUDE_CODE_SESSION_ATTENDED` is `0`, the hook first sends `POST /title` with `unattended`. Then the service refuses `/session` from that `CLAUDE_PID` with 403 for 10 s ([flow 30](flows.md#30-a-run-that-is-not-interactive)).
2. `/title` resolves the ID to a session as in [API 4.6](api.md#46-socket-protocol). So `/clear`, `/resume` and `--continue` keep the session of the running Claude Code process. The service gives an alias only to an ID without `session.json`. For a new ID from a process without a known session, `/title` replies first, and then waits up to 10 s for the server of that process to register. `/title` then opens the session from `session.json`, or with a new number and the name from the title or the folder. `Hub.session` finds the group of `session.json` or creates one. It adds `User`, invites the Main group's owners and loads the group's `Control` ([`Service.onTitle`, `markUnattended`, `refuseUnattended`, `applyTitle`, `openAgent`](../src/service.ts), [`Hub.session`](../src/hub.ts)).
3. At a new title, the service renames the group and `session.json`. The model, when Claude Code passes it, is written to the group's short description with the hook's `at`, so a model of a hook that started later stays ([`Service.applyTitle`](../src/service.ts)). The Stop hook of each turn sends the model of its answer from the transcript. So the model is shown in the short description from the first answer, even when the SessionStart hook has none ([`turn`, `answerModel`](../src/hook.ts)).
4. The hook prints the link message while no owner is in the Main group, and `{}` otherwise.
5. The session process connects in the background. `/session` resolves its ID, and waits up to 10 s for the title of its process when the ID is new. It then registers the options and the parent process, and requests the mode ([`Service.onSession`](../src/service.ts)). `/attach` sets the session's line writer ([`Service.onAttach`](../src/service.ts)). When `/attach` ends or the connection fails, the session process connects again, except after a `ProtocolError` or a 403 of `/session` ([`ServiceLink.start`, `close`, `reconnect`](../src/session.ts)).

#### Service spawn and version check

```
ServiceClient.ensureService(VERSION):                 client.ts
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

1. `ROOT` is created, and the lock is taken. When another running process holds the lock, the service checks the lock for up to 15 s ([`runService`, `lockHolder`, `Service.start`](../src/service.ts)):
   - After the lock is released, the start runs again.
   - If the socket answers, the ready line is `running`.
   - Otherwise the ready line is `error: Another service uses ROOT (PID N). If no service runs, remove ROOT/service.lock.`, and the process exits with code 1.
2. The stored sessions are read with their aliases, and the next number is set (the constructor of [`Service`](../src/service.ts)).
3. The service listens on the socket before the hub opens, and requests wait for the hub ([`Service.start`, `withHub`](../src/service.ts)).
4. The service reads the options in `openHub` ([`Service.openHub`](../src/service.ts)). `Hub.open` then opens the core `_hub` with profile `Claude`, and finds the Main group by its marker or creates it. It runs `MainGroup.load` and `start`, with a link while no owner is in the Main group. The core `_user` opens, and connects to `Claude` at the first start. A `Session` opens for each stored session, prompts are held for 30 s, and events are dispatched ([`Hub.open`](../src/hub.ts)).
5. When the hub fails to open, the listener closes and the lock is released. The service answers the requests that waited with status 500 ([`Service.start`](../src/service.ts)).
6. The stop timer and the hourly log trim start. The titles held at the last stop are read from `ROOT/titles.json` and taken as new `/title` requests. The service logs `SimpleX MCP service VERSION, data folder ROOT` and writes the ready line `ready` ([`Service.start`, `runService`](../src/service.ts)).

#### Event dispatch

After a stop, `Hub.onEvent` ignores events ([`Hub.onEvent`](../src/hub.ts)). Otherwise it runs `expireDue` of every session first, and then dispatches the event:

| Event | Condition | Handler |
|---|---|---|
| `newChatItems` | item of the Main group | [`MainGroup.onMessage`](../src/main-group.ts) |
| `newChatItems` | item of a session group | [`Session.onMessage`](../src/hub.ts) |
| `rcvFileComplete`, `rcvFileError` | | [`Session.fileDone`](../src/hub.ts) of every session |
| `contactConnected`, `userJoinedGroup` | a connection ID | [`Session.claimPending`](../src/hub.ts) of every session |
| any event | | [`AgentTools.onEvent`](../src/tools.ts) of every session |
| `sndFileError` | a chat of the session, or its group | [`Control.fileNotSent`](../src/control.ts) |
| membership events | of the Main group | [`MainGroup.onEvent`](../src/main-group.ts), queued in the Main group and not awaited by the event loop. It posts the statement of trust at a join ([flow 28](flows.md#28-a-member-joins-the-main-group-and-a-sessions-group-and-a-person-is-added-to-it)). |
| membership events | of a session group | [`Control.onEvent`](../src/control.ts), then [`Session.announceRights`](../src/hub.ts) and [`Session.askMode`](../src/hub.ts), queued |

#### Attach, idle close and idle stop

- The service cancels the session's idle timer at each attachment. When the last attachment ends, the service records the time, clears the line writer, ends the pending permissions and starts the timer ([`Service.onAttach`](../src/service.ts)).
- After 10 minutes, the idle timer closes the entry when no call of the session runs and none of its actions is in memory. Otherwise the timer starts again ([`Service.scheduleIdle`](../src/service.ts)). The hub keeps the `Session` of a closed session, so Claude still answers `/list` and `/revoke` in its group ([`Hub.session`](../src/hub.ts)).
- When no entry remains, the stop timer stops the service after 10 minutes. The timer is cleared when a new entry opens ([`Service.scheduleStop`, `entry`](../src/service.ts)).

#### Stop

On `SIGINT`, `SIGTERM`, `POST /stop` or the stop timer ([`Service.shutdown`](../src/service.ts)):

1. Timers are cleared, the held titles are saved in `ROOT/titles.json`, the listener closes, and open streams are destroyed.
2. `Hub.stop` stops the actions of every session, and edits their cards to `Cancelled: the server stopped.` It awaits the queued group events, and gives the cancelled cards 10 s to be sent. Then the download checks stop, and the cores `_user` and `_hub` close ([`Hub.stop`](../src/hub.ts)).
3. The service logs `Cancelled K action(s).`, releases the lock and logs `Stopped.` The process exits after the `/stop` reply is sent, or after its client disconnects before the reply is written.

### 8.5 Approval model

| Rule | Code |
|---|---|
| Rights in a session group are set by the member's role there, however the member joined. An approver is an active member other than Claude and `User`, with the role `admin` or `owner`. Approvals and tool-use decisions are accepted only from approvers. | [`Session.approves`, `isUser`](../src/hub.ts), [`sessionApproverRoles`, `Control.approvers`, `isHuman`, `isApprover`](../src/control.ts) |
| Prompts are accepted from every member except `User`, and the group's `/` commands only from approvers. The other members get one reply to each one-line `/` text. | [`Session.onMessage`, `prompt`, `refusesCommand`, `approves`](../src/hub.ts) |
| With `/join N`, a member of the Main group is invited as `admin`, and an owner of the Main group as `owner`. Owners invited at a session start or at an owner's join get the role `owner`. | [`Hub.join`, `session`, `inviteOwner`](../src/hub.ts) |
| With `/observer #G ID`, the sender sets the role `observer` of a current member. The sender's role must be admin or owner, and not below the member's role, as SimpleX requires. For a member with status `pending_approval` or `pending_review`, Claude replies `NAME's role can't be changed.` For a member who is neither current nor pending, such as an invited member who has not joined, the reply is `No member ID in this group.` An observer neither prompts nor approves. From the reply on, the standing approvals that the observer gave are not used while that member is not an admin or owner. | [`Control.setObserver`, `roleRank`, `Control.standingApprover`](../src/control.ts), [`memberCurrentOrPending`](../src/tools.ts) |
| A new action of a session is refused while its group has no approver | [`Control.refusal`](../src/control.ts) |
| Changes of membership or role in the Main group, and its replacement by a reset of the owners, leave the session groups as they are | [`MainGroup.handleEvent`, `replace`](../src/main-group.ts) |
| Everyone in the Main group is trusted with all sessions. This is stated in the group's short description and welcome message, set when the group is created, and in Claude's post at each join. In a session group, Claude posts the member's rights at each join and each role change, with the `/observer` command only for a role that can send, other than owner. The observer's rights are stated in Claude's reply to `/observer`. | [`welcome`, `joinRights`, `shortDescr`, `createMainGroup`, `MainGroup.handleEvent`](../src/main-group.ts), [`Session.announceRights`](../src/hub.ts), [`Control.setObserver`, `rightsText`](../src/control.ts) |
| A file of `reply` is sent at once, in both modes. It can be refused by the group's preference for files and media, which owners set and the core enforces. A post of Claude or `User` in a session group, when refused by a group preference, is replaced by Claude's post with the preference's name, and is never dropped without one ([`refusedFeature`, `featureOff`](../src/tools.ts)). Claude updates a group's profile from its current profile in the chat database, and changes only its own fields, so the owners' preferences are kept. | [`AgentTools.reply`, `sendFile`](../src/tools.ts), the `replyChat` of the [`Session`](../src/hub.ts) constructor, [`Control.load`, `rename`, `describe`, `updateProfile`, `updateGroupProfile`, `changeGroupProfile`](../src/control.ts), [`MainGroup.load`](../src/main-group.ts) |
| The first human who joins the Main group while it has no owner becomes an owner, and the link is deleted | [`MainGroup.handleEvent`, `ownerJoined`](../src/main-group.ts) |
| A session runs in mode `host` after an approver approves its `mode` card. The card is posted once an approver is in the group. After a rejection, the card is posted again only after `requests`. `requests` applies at once, and the hub ends a pending `mode` card. The mode is kept in the session group's custom data. | [`Session.requestMode`, `askMode`, `declineHost`, `Hub.onEvent`](../src/hub.ts), [`AgentTools.perform`](../src/tools.ts) |
| At a reset of the owners from the computer that runs Claude Code, the service replaces the Main group at once, even while owners exist. No owner's approval is needed for `reset-owners` and `POST /reset-owners`. | [`Hub.resetOwners`](../src/hub.ts), [`resetOwners`](../src/owner.ts), [`MainGroup.replace`](../src/main-group.ts) |
| A chat without an owner is given to the session that connects it. The owner is set at one of four points: the perform, the connection event of its action, the completion of a pending connection that the session recorded, or a `connect` to the chat as a known chat. The tools of other sessions don't show the chat. With `/give CHAT N`, an approver moves the chat to session N. | [`AgentTools.owns`, `claim`, `ownChat`, `performConnect`, `markConnected`](../src/tools.ts), [`Session.claimPending`, `Hub.onEvent`](../src/hub.ts), [`Control.give`](../src/control.ts) |

Tool-use state machine, one action of tool `permission` per request:

| Event | From | To | Code |
|---|---|---|---|
| `permission_request` | | `pending`, card posted | [`Session.permission`](../src/hub.ts) |
| `/approve N` | `pending` | `performing`, `allow` pushed, then `done` | [`Session.permission`](../src/hub.ts), [`AgentTools.perform`](../src/tools.ts) |
| `/reject N` | `pending` | `rejected`, `deny` pushed | [`Session.permission`](../src/hub.ts) |
| an answer, or a prompt that is not mid-turn, of a hook that started after the card's creation; end of the last attachment | `pending` | `expired`, card edited to `Ended.` | [`Session.turn`, `endPermissions`](../src/hub.ts), [`Service.onAttach`](../src/service.ts), [`Actions.end`](../src/actions.ts) |
| expiry | `pending` | `expired`, card edited to `Expired.` | [`Actions.expire`](../src/actions.ts) |
| `done`, `rejected` or `expired` | | removed from the session's actions | [`Session.permission`](../src/hub.ts) |

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
| service to session | `{prompt: {text, from, item, file?, replyTo?: {item?, from?}}}`, `{permission: {requestId, behavior}}` on `/attach` | [`AttachLine`](../src/protocol.ts) |
| session to Claude Code | `notifications/claude/channel` `{content, meta: {from, item, file?, reply_to?, reply_to_from?}}`; `notifications/claude/channel/permission` `{request_id, behavior}` | [`channelNotification`](../src/session.ts) |
| Claude Code to session | `notifications/claude/channel/permission_request` `{request_id, tool_name, description, input_preview}` | the notification handler of [`startSession`](../src/session.ts) |
| session to service | `POST /permission` | [`ServiceLink.permission`](../src/session.ts) |
| hooks to service | `POST /turn` `{id, pid?, at?, prompt?, typed?, note?, midTurn?, answer?, model?, effort?}`. `pid` is `CLAUDE_PID`, and `at` is the hook's `performance.timeOrigin`. `prompt` and `answer`, the last answer, are cut to 250,000 characters. `typed` of UserPromptSubmit is true when a person typed the prompt. `note` is the line about one of Claude Code's own prompts. `midTurn` is true for a prompt that Claude Code added to a running turn. `model` is the new model of PostModelSwitch, or the model of Stop's answer in the transcript. `effort` is the effort level. | [`turn`, `answerModel`, `promptKind`, `cut`](../src/hook.ts) |

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
                                   else: text = quote line of m's replyTo, a line break and m.text, for a reply; m.text otherwise
                                         pushed += (text, m.item, now), at most 20; push {prompt: {text, ..., replyTo: {item, from}}}
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

Sources: [`Session.onMessage`, `turn`, `endPermissions`, `prompt`, `deliver`, `receive`, `newer`, `turnQuote`, `answer`, `postRefusal`, `post`, `pushedIn`, `textParts`](../src/hub.ts).

### 8.7 Security boundaries

| Rule | Code |
|---|---|
| The socket is in ROOT, mode 0700. For a path over 103 bytes, the socket is `TMP/simplex-mcp-HASH.sock`, and on Windows it is a named pipe. The service accepts a request only with the token, compared with `timingSafeEqual`. | [`listen`](../src/socket.ts), [`socketPath`](../src/files.ts) |
| The Main group link is shown only in the hook's `systemMessage`, and on a terminal with `link`. In tool results, refusals and other output, the command is named instead. Of the socket's routes, only `/title`, `/link` and `/reset-owners` return a Main group link. `/reset-owners` returns the link of the new group. | [`sessionStart`](../src/hook.ts), [`linkCommand`, `resetCommand`, `linkText`](../src/owner.ts), [`noSessionApprover`](../src/control.ts), [`Service.routes`, `onSession`, `status`](../src/service.ts), [`Hub.resetOwners`](../src/hub.ts) |
| `reset-owners` shows the new link only on a terminal | [`resetOwnersText`, `resetOwners`](../src/owner.ts) |
| The plugin runs its own `dist/cli.js`. Node.js loads its modules, bundled in the package, from the plugin's folder, so they can't be replaced by the `node_modules` of a project. | [`mcpServers.simplex.args`](../.mcp.json), [`hooks`](../hooks/hooks.json), [`bundleDependencies`](../package.json) |
| The service gets only the environment variables of `serviceVariables` | [`serviceVariables`, `spawnService`](../src/client.ts) |
| `send` and `reply` refuse the files of ROOT except `files/` of the session's own folder | [`AgentTools.isRefused`](../src/tools.ts), the `agent` of the [`Session`](../src/hub.ts) constructor |
| `User`'s messages are neither prompts nor commands. `Session.prompt` and `Control.onMessage` drop the messages of the member of the hub's contact marked `simplexMcpUser`, which is read at the hub's start. `User` joins only the groups to which it is invited by its `Claude` contact. | [`Session.prompt`, `isUser`, `Hub.open`](../src/hub.ts), [`Control.onMessage`](../src/control.ts), [`User.onEvent`, `userContact`](../src/user.ts) |
| The hub's own groups and the `User` contact have no owner key, so the tools of every session leave them out | [`AgentTools.owns`, `agentChats`](../src/tools.ts) |
| The approved mode is kept in the chat database, not in a file of ROOT | [`Session`](../src/hub.ts) constructor, [`Session.setMode`](../src/hub.ts), [`SessionFile`](../src/files.ts) |
| These rules hold against Claude's tools, but not against a shell running as the same user. Such a shell can read ROOT, with the token and the chat databases, and send requests to the service. | |

### 8.8 Limits

| Limit | Value | Code |
|---|---|---|
| socket request body | 1 MiB | [`maxBody`](../src/socket.ts) |
| idle close of a session, idle stop of the service | 10 min each | [`idleClose`, `idleStop`](../src/service.ts) |
| service start wait | 5 min | [`serviceStartTimeout`](../src/client.ts) |
| `User` connection at the first start | 60 s | [`connectTimeout`](../src/user.ts) |
| echo of a raw prompt | 60 s | [`promptEcho`](../src/hub.ts) |
| pushed prompts of a session | 20 | [`maxPushed`](../src/hub.ts) |
| file of a prompt | 10 min | [`fileWait`](../src/hub.ts) |
| prompts held for an attachment after the hub opens | 30 s | [`attachWait`](../src/hub.ts) |
| record of a pending connection | 1 day | [`pendingDays`](../src/hub.ts) |
| wait of a session server for the title of its process, the time a detached session server's process is kept, and the mark of an unattended run's process, removed at a later mark | 10 s | [`processWait`, `Service.markUnattended`](../src/service.ts) |
| attempts of a turn the service refuses with status 409 | 15, 1 s apart | [`unknownSession`, `turnAttempts`, `turnRetryMs`](../src/hook.ts) |
| reads of the transcript for the model of Stop's answer and the origin of a prompt | 20 for the answer and 600 for the prompt, 100 ms apart, each of the last 1 MiB and twice the bytes of the answer or the prompt | [`answerReads`, `promptReads`, `transcriptReadMs`, `transcriptTailBytes`, `findInTranscript`, `readTail`](../src/hook.ts) |
| length of a note of Claude Code's own prompt | 200 characters, the last one `…` when cut | [`maxNote`, `shortened`](../src/hook.ts) |
| attachment retries of a session server | 1 s, doubled up to 30 s | [`firstRetry`, `lastRetry`](../src/session.ts) |
| part of an answer or a terminal prompt | one message by `fitsMessage`: 15602 bytes with the 600-byte envelope; 14962 bytes of text without a quote | [`textParts`](../src/hub.ts), [`maxMessageLength`, `messageEnvelope`, `fitsMessage`](../src/tools.ts) |
| cancelled cards wait at stop | 10 s | [`cardsWait`](../src/hub.ts) |
| `service.log` | emptied over 10 MB, at a spawn and every hour | [`maxLogBytes`, `trimLog`](../src/files.ts), [`logCheck`](../src/service.ts) |
| socket path | 103 bytes: `sun_path` of `sockaddr_un` holds 104 bytes on macOS and 108 on Linux, with the terminating NUL | [`maxSocketPath`](../src/files.ts) |
| actions per session | 20 pending, 20 performing | [`maxPending`, `maxPerforming`, `Actions.create`](../src/actions.ts) |
