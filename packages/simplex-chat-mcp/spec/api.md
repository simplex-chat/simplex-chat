# SimpleX MCP server: API

> The six MCP tools, the agent control chat commands, replies and cards, and the CLI of `@simplex-chat/mcp` 0.3.0. The Claude Code plugin's tools, groups, channel, socket protocol, commands and files are in section 4.
>
> Related specs: [README](README.md) | [Architecture](architecture.md) | [Flows](flows.md)

**Source:** [`tools.ts`](../src/tools.ts) | [`control.ts`](../src/control.ts) | [`cli.ts`](../src/cli.ts) | [`messages.ts`](../src/messages.ts) | [`actions.ts`](../src/actions.ts) | [`session.ts`](../src/session.ts) | [`service.ts`](../src/service.ts) | [`hub.ts`](../src/hub.ts) | [`main-group.ts`](../src/main-group.ts) | [`hook.ts`](../src/hook.ts) | [`owner.ts`](../src/owner.ts) | [`protocol.ts`](../src/protocol.ts)

---

## Table of contents

1. [Tools](#1-tools)
   - [1.1 Common rules](#11-common-rules)
   - [1.2 `connect`](#12-connect)
   - [1.3 `send`](#13-send)
   - [1.4 `receive`](#14-receive)
   - [1.5 `download`](#15-download)
   - [1.6 `list`](#16-list)
   - [1.7 `leave`](#17-leave)
   - [1.8 Message, file and command objects](#18-message-file-and-command-objects)
   - [1.9 Action refusals](#19-action-refusals)
2. [Agent control chat](#2-agent-control-chat)
   - [2.1 Commands and replies](#21-commands-and-replies)
   - [2.2 Cards](#22-cards)
   - [2.3 Other posts](#23-other-posts)
   - [2.4 Membership messages](#24-membership-messages)
   - [2.5 Group profile](#25-group-profile)
3. [CLI](#3-cli)
   - [3.1 `serve`](#31-serve)
   - [3.2 `register`](#32-register)
   - [3.3 Exit codes and messages](#33-exit-codes-and-messages)
   - [3.4 Terminal lines](#34-terminal-lines)
4. [Claude Code plugin](#4-claude-code-plugin)
   - [4.1 Tools in the plugin](#41-tools-in-the-plugin)
   - [4.2 Main group](#42-main-group)
   - [4.3 Session groups](#43-session-groups)
   - [4.4 Tool results in the plugin](#44-tool-results-in-the-plugin)
   - [4.5 Channel](#45-channel)
   - [4.6 Socket protocol](#46-socket-protocol)
   - [4.7 Plugin commands](#47-plugin-commands)
   - [4.8 Plugin options, files and hook output](#48-plugin-options-files-and-hook-output)

---

## 1. Tools

### 1.1 Common rules

<a id="common-rules"></a>

| Rule | Code |
|---|---|
| Tools are registered on each per-request `McpServer` with description, input schema, output schema and, in mode `host`, `_meta: {"anthropic/requiresUserInteraction": true}` for `connect`, `send`, `download` and `leave` | [`definitions`, `registerTools`, `AgentTools.register`](../src/tools.ts) |
| Tool descriptions and server instructions; the instructions name the agent's display name | [`connectDescription` to `leaveDescription`, `instructions`](../src/tools.ts), [`startAgent`](../src/serve.ts) |
| Arguments are validated by the SDK; a failure is a result with `isError: true` and the text `Input validation error: Invalid arguments for tool NAME: ERRORS` | [`McpServer.validateToolInput`](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs) |
| `additionalProperties: false` is set in every input schema | [`connectSchema`, `sendSchema`, `receiveSchema`, `listSchema`, `leaveSchema`, `downloadSchema`](../src/tools.ts) |
| A result is sent as `structuredContent` and as its JSON in one text item | [`AgentTools.call`](../src/tools.ts) |
| A tool error is sent as `isError: true` with one text item: the `ToolError` text, or the `errorText` of other errors | [`AgentTools.call`](../src/tools.ts), [`errorText`](../src/log.ts) |
| An aborted call ends with the result `Cancelled.`, `isError: true`, which the SDK discards | [`cancelled`, `registerTools`](../src/tools.ts) |
| With a `progressToken` in `_meta`, `notifications/progress` is sent at the start of each wait and every 60 s, with `progress` 0, 1, 2, ... and a `message` | [`toolCall`](../src/tools.ts), [`reportProgress`](../src/messages.ts) |
| The call is aborted when the client closes the response stream or sends `notifications/cancelled`; the action is kept | [`Protocol._oncancel`, `_onclose`](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs) |
| A call with the same tool and action fields as an action in memory is attached to it. The key is the JSON array of the session number, the tool and the fields, with the session number `undefined` for `serve` | [`actionKey`, `AgentTools.action`](../src/tools.ts), [`Actions.find`](../src/actions.ts) |
| Messages are marked read after the last wait of a call that is not aborted | [`AgentTools.connectedChat`, `sendResult`, `receive`](../src/tools.ts) |

Action fields of the key:

| Tool | Fields |
|---|---|
| `connect` | `target`<br>`incognito` (default `false`)<br>`message` (default `null`) |
| `send` | `chat`<br>`text` (default `null`)<br>`file` (default `null`)<br>`as` (default `null`)<br>`reply_to` (default `null`)<br>`mention` (default `null`) |
| `download` | `chat`<br>`item` |
| `leave` | `chat` |

Result status of the four action tools ([`actionResult`](../src/tools.ts)):

| Status | Meaning | Extra fields |
|---|---|---|
| `done` | performed | per tool |
| `failed` | failure of the card post, the counter write or the perform, or loss of the control chat | `error` |
| `rejected` | an approver sent `/reject` | |
| `expired` | no decision before the expiry time | |
| `known` | `connect` only: the target is an existing chat past the prepared state; no action | chat fields |
| `performing` | `download` only: `wait` passed after the approval; the download continues | |

### 1.2 `connect`

<a id="tool-connect"></a>Handler: [`AgentTools.connect`](../src/tools.ts). Perform: [`performConnect`](../src/tools.ts). Schemas: input [`connectSchema`](../src/tools.ts), output [`connectOutput`](../src/tools.ts).

Input:

| Field | Type | Range | Default | Rule |
|---|---|---|---|---|
| `target` | string | 1 character or more | required | SimpleX link (`https://...`, `simplex:/...`), contact name `@name.simplex` or channel name `#name.simplex`; whitespace refused |
| `incognito` | boolean | | `false` | `true`: the join methods take `incognito` true |
| `message` | string | 1 to 2000 characters | | first message of a contact request; plans with `takesMessage` only |
| `wait` | number | 0 to 86400 | 60 | seconds from the approval: for the other side to accept and, with `message`, for its first reply |
| `idle` | number | 1 to 300 | 30 | seconds without a new message, after which the server ends the wait for messages |

Output fields by case:

| Case | Fields | Code |
|---|---|---|
| `known` | `status` and the chat fields of the known chat. The plugin's rules for a known chat are in [4.1](#41-tools-in-the-plugin) | [`AgentTools.connect`, `ownChat`](../src/tools.ts) |
| `done`, chat unknown | `action`, `status`, `type`, `connected: false`, plan fields `name`, `peer_type`, `description`, `welcome`, `commands`, `known_as` when present | [`AgentTools.connectResult`, `planFields`](../src/tools.ts) |
| `done`, chat known | `action`, `status`, chat fields, `messages` | [`AgentTools.connectedChat`](../src/tools.ts) |
| `failed` | `action`, `status`, `error` | [`AgentTools.actionResult`](../src/tools.ts) |
| `rejected`, `expired` | `action`, `status` | [`AgentTools.actionResult`](../src/tools.ts) |

<a id="chat-fields"></a>Chat fields ([`chatFields`](../src/tools.ts)):

| Field | Contact | Group, business chat or channel |
|---|---|---|
| `chat` | `@contactId` | `#groupId` |
| `type` | `contact` | `business` with `businessChat`; `channel` with `useRelays` or group type `channel`; otherwise `group` |
| `name` | profile display name | group display name |
| `simplex_name` | `@` + `contactDomain.domain` when verified | `#` + `groupDomainClaim.domain` when verified |
| `peer_type` | profile peer type, when present | from the plan, when present |
| `description` | profile `shortDescr` | group `shortDescr` |
| `welcome` | from the plan: link data message | group `description` |
| `commands` | profile `preferences.commands` | `groupPreferences.commands` |
| `known_as` | incognito: `customUserProfile` of `apiContactInfo` | incognito: the membership's display name |
| `role` | | the agent's member role |
| `connected` | active connection status `ready` | membership status `connected` or `complete` |

`messages`: message items among the last 50 items of the chat when the result is built, replaced by their collected copies, followed by the items collected since the connection; live items are left out; new items are marked read ([`AgentTools.connectedChat`](../src/tools.ts)).

Tool errors:

| Text | Condition | Code |
|---|---|---|
| `Not a SimpleX link or name: TARGET` | whitespace in `target`, or `invalidConnReq` from `apiConnectPlan` | [`AgentTools.plan`](../src/tools.ts) |
| an app error text, for example `Name not found: This SimpleX name is not registered. Please check the name.`, or `SimpleX error: CHAIN: DETAILS` | other errors of `apiConnectPlan`, or a plan of type `error` | [`AgentTools.plan`, `classifyPlan`](../src/tools.ts), [`chatErrorText`](../src/log.ts) |
| `TARGET can't be joined now: PLAN.` | `invitationLink` other than `ok` and `known`<br>`contactAddress` other than `ok`, `known`, `contactViaAddress` and `connectingConfirmReconnect`<br>`groupLink` other than `ok`, `known` and `connectingConfirmReconnect` | `notJoinable` in [`classifyPlan`](../src/tools.ts) |
| `message: TARGET is not a contact or business address with short link data, or its contact exists without a connection.` | `message` given and the plan has `takesMessage: false` | [`AgentTools.connect`](../src/tools.ts) |
| refusals | see [1.9](#19-action-refusals) | |

`takesMessage` is `true` for a contact address with short link data, for a known prepared contact, and for a known prepared business group ([`classifyPlan`](../src/tools.ts)).

### 1.3 `send`

<a id="tool-send"></a>Handler: [`AgentTools.send`](../src/tools.ts). Result: [`sendResult`](../src/tools.ts). Perform: [`performSend`](../src/tools.ts). Schemas: input [`sendSchema`](../src/tools.ts), output [`sendOutput`](../src/tools.ts).

Input:

| Field | Type | Range | Default | Rule |
|---|---|---|---|---|
| `chat` | string | | required | `@N` or `#N` of the agent |
| `text` | string | 1 to 12000 characters | required without `file` | the message text, or the caption of `file` |
| `file` | string | 1 character or more | | absolute path of a file of this computer |
| `as` | string | `file` | | sends an image or video unchanged, as a file; only with `file` |
| `reply_to` | integer | 1 or more | | item ID of a message in `chat` |
| `mention` | integer array | 1 to 3, unique, each 1 or more | | member IDs of a group; names missing from `text` are prefixed as `@name` |
| `wait` | number | 0 to 86400 | 60 | seconds from the send; from the call when it attaches to a done action |
| `idle` | number | 1 to 300 | 30 | seconds without a new response, once one arrived |
| `limit` | integer | 1 to 100 | 50 | most responses returned |

<a id="file-types"></a>File types by extension, lower-cased ([`attachmentTypes`, `otherFile`, `attachmentMedia`](../src/tools.ts)):

| Extension | Message type | Sent file | Preview and duration |
|---|---|---|---|
| `.jpg`, `.jpeg`, `.png` | image | `core.resizeImage(..., Image)`, written to `DATA/files/sent/`; the original when resizing fails | `core.resizeImage(..., MessagePreview)`; the fixed preview when it fails |
| `.gif`, `.webp` | image | unchanged | the fixed preview |
| `.mp4`, `.mov` | video | unchanged | the first frame from `ffmpeg`, resized as a preview; duration from `ffprobe`, rounded; the fixed preview and 0 when they fail |
| other, or `as: "file"` | file | unchanged | |

The fixed preview is a 16 × 9 mid-gray PNG ([`fixedPreview`](../src/tools.ts)). `ffmpeg` and `ffprobe` run with `-protocol_whitelist file -f mov`, 30 s and 64 MB of output each ([`videoFrame`, `videoDuration`, `commandOutput`](../src/tools.ts)).

Output:

| Field | Presence | Value |
|---|---|---|
| `action` | always | action number |
| `status` | always | `done`, `failed`, `rejected`, `expired` |
| `chat` | `done` | the chat reference |
| `item` | `done` | item ID of the sent message |
| `responses` | `done` | received message items with IDs above `item`, in arrival order, live items left out, at most `limit` |
| `more` | `done` | collected responses not returned, live ones and overflow included |
| `error` | `failed` | error text |

Tool errors:

| Text | Condition | Code |
|---|---|---|
| `text: required without file.` | neither `text` nor `file` | [`AgentTools.send`](../src/tools.ts) |
| `as: applies only with file.` | `as` without `file` | [`AgentTools.send`](../src/tools.ts) |
| `No chat CHAT. list shows the agent's chats.` | invalid reference, the control chat, or a store error when the chat is read | [`AgentTools.knownChat`, `unknownChat`](../src/tools.ts) |
| `The agent is not connected to CHAT and can't post; list shows connected: false.` | the chat is not connected | [`AgentTools.send`](../src/tools.ts) |
| `The agent has role ROLE in CHAT and can't post.` | membership role `observer` or `relay` | [`AgentTools.send`](../src/tools.ts) |
| `reply_to: CHAT has no item N.` | the read of `around=N` fails, or the item is absent from it | [`AgentTools.quotedItem`](../src/tools.ts) |
| `reply_to: item N in CHAT is not a message.` | the item's content is not `rcvMsgContent` or `sndMsgContent` | [`AgentTools.quotedItem`](../src/tools.ts) |
| `mention: CHAT is a contact chat; mentions apply to groups and channels.` | `mention` with `@N` | [`AgentTools.mentionedMembers`](../src/tools.ts) |
| `mention: CHAT has no member N.` | ID absent from `apiListMembers` | [`AgentTools.mentionedMembers`](../src/tools.ts) |
| `mention: members N and M have the same name.` | two mentioned members with the same display name | [`AgentTools.mentionedMembers`](../src/tools.ts) |
| `file: an absolute path is expected.`<br>`file: no file at FILE.`<br>`file: PATH is not a file.`<br>`file: PATH can't be read.` | the path, after symbolic links | [`realFile`](../src/tools.ts) |
| `file: PATH is refused: private keys and the server's own files are not sent.` | a file named `id_*`, `*.pem`, `*.key`, `*.p12` or `*.pfx`, or under `~/.ssh`, `~/.gnupg`, or the data folder or `~/.simplex-mcp` outside `DATA/files/`; each folder by its real path | [`AgentTools.attachment`, `isRefused`](../src/tools.ts) |
| `file: PATH is empty.` | size 0 | [`AgentTools.attachment`](../src/tools.ts) |
| `text: too long for the message with its quote; shorten it.`<br>`text: too long for a message with this TYPE; shorten it.` | the message, as the core sends it, above 15602 bytes | [`AgentTools.send`, `tooLongText`, `fitsMessage`, `quoteContent`](../src/tools.ts) |
| refusals | see [1.9](#19-action-refusals) | |

The message is measured as 600 bytes plus the JSON of its content, the file's name, the quote and the mentioned names. The quote is the quoted text, or its file's name when the text is empty; a sent image or video quotes it as text, otherwise an image's preview is kept ([`fitsMessage`, `quoteContent`](../src/tools.ts)).

The perform fails with `file: PATH changed after the request.` when the file's real path, size, inode or change time differs from the request ([`AgentTools.fileToSend`](../src/tools.ts)). A resized copy is deleted at `sndFileCompleteXFTP`, at `sndFileError` and after a failed send ([`AgentTools.performSend`, `onSentFile`, `deleteSentCopy`, `isSentCopy`, `onEvent`](../src/tools.ts)).

### 1.4 `receive`

<a id="tool-receive"></a>Handler: [`AgentTools.receive`](../src/tools.ts). Schemas: input [`receiveSchema`](../src/tools.ts), output [`receiveOutput`](../src/tools.ts).

Input:

| Field | Type | Range | Default | Rule |
|---|---|---|---|---|
| `chat` | string | | required | `@N` or `#N` of the agent |
| `wait` | number | 0 to 86400 | 0 | seconds to wait when no message is new |
| `limit` | integer | 1 to 100 | 50 | most messages returned |

Output (all fields always present):

| Field | Value |
|---|---|
| `chat` | the chat reference |
| `messages` | unread received messages in chat order, then received messages that arrived during the call; live items left out; at most `limit` |
| `more` | new messages not returned, overflow, and unread items left unread by the page reads |

Tool errors: `No chat CHAT. list shows the agent's chats.` for an invalid reference, the control chat, or a store error of the first read, `initial` 1 ([`AgentTools.receive`](../src/tools.ts)); other chat errors, a store error of a later page read included, as their error text ([`errorText`, `chatErrorText`](../src/log.ts)).

Messages are read by `receive` without an action or an approval.

### 1.5 `download`

<a id="tool-download"></a>Handler: [`AgentTools.download`](../src/tools.ts). Result: [`downloadResult`](../src/tools.ts). Perform: [`performDownload`](../src/tools.ts), [`downloadFile`](../src/tools.ts). Schemas: input [`downloadSchema`](../src/tools.ts), output [`downloadOutput`](../src/tools.ts).

Input:

| Field | Type | Range | Default | Rule |
|---|---|---|---|---|
| `chat` | string | | required | `@N` or `#N` of the agent |
| `item` | integer | 1 or more | required | item ID of a received message with a file |
| `wait` | number | 0 to 86400 | 60 | seconds from the call to wait for the sender's upload before the card; seconds from the start of the perform, or from a call that attaches to a performing action, for the download |

Output:

| Field | Presence | Value |
|---|---|---|
| `action` | always | action number |
| `status` | always | `done`, `performing`, `failed`, `rejected`, `expired` |
| `chat`, `item` | `done` | the message |
| `path` | `done` | `DATA/files/FOLDER/<file ID><extension>` |
| `error` | `failed` | error text |

FOLDER is `group-N` for `#N` and `contact-N` for `@N`; the extension is the file name's extension when it has 1 to 16 ASCII letters and digits ([`filePaths`, `storedName`, `chatFolder`](../src/messages.ts)). `/forward` downloads a file to `DATA/forwarded/FOLDER/` under the same name; a later perform of the download moves it to the download path, after the copy for the post is made ([`AgentTools.downloadFile`, `forwardFile`](../src/tools.ts)).

Steps before the action:

| Step | Code |
|---|---|
| a file in status `rcvInvitation`, or an XFTP file: the file's servers from `apiGetChatItemInfo`; while none, the call waits for `rcvFileDescrReady` until `wait` seconds after the call, with the progress `The sender is uploading the file of item N in CHAT; waiting until TIME.` | [`AgentTools.fileServers`](../src/tools.ts) |
| unknown servers: the file's servers that are not among the XFTP servers of `apiGetUserServers`, of no operator or of an enabled one, compared without their passwords | [`AgentTools.unknownServers`](../src/tools.ts) |
| without unknown servers, and for a file ID not in rejected files, the chat's files approval, read again, makes the action performed at once | [`AgentTools.download`](../src/tools.ts) |

Perform: `apiReceiveFile(fileId, path, approvedRelays)`, for a file in status `rcvInvitation`. `approvedRelays` is true when the action's mode is `requests` and unknown servers are named on its card, and false otherwise. The core then checks the complete description ([`AgentTools.downloadTo`, `startDownload`](../src/tools.ts)). The download ends at `rcvFileComplete`, `rcvFileError`, `rcvFileSndCancelled` or `chatItemUpdated` of its file, or at a check every 60 s ([`AgentTools.onFile`, `onFileComplete`, `onFileError`, `onFileCancelled`, `checkDownload`, `currentFile`, `settleFile`, `settleReceived`, `moveReceived`, `failDownload`, `settleDownload`](../src/tools.ts)).

Failure texts of the perform:

| Text | Condition | Code |
|---|---|---|
| the file's error, as in the file field | the file failed | [`AgentTools.settleFile`](../src/tools.ts) |
| `The sender cancelled the file.` | `rcvFileSndCancelled` | [`AgentTools.onFileCancelled`](../src/tools.ts) |
| `The message or its chat was deleted.` | a check finds no message, contact or group | [`deletedItem`, `AgentTools.currentFile`](../src/tools.ts) |
| `The download made no progress for 30 minutes and was cancelled.` | no rise of `rcvProgress` for 30 minutes; the file is cancelled | [`stalledDownload`, `AgentTools.checkDownload`](../src/tools.ts) |
| `The received file is not in the agent's files folder.` | the file is received and neither the download path nor the forwarded path has a file | [`missingFile`](../src/messages.ts), [`AgentTools.settleReceived`](../src/tools.ts) |
| `Your owner deleted the files of this chat.` | `/delete CHAT files` | [`deletedFiles`, `AgentTools.deleteChatFiles`](../src/tools.ts) |
| `The file is on unknown servers that were not approved.` | a check reads an accepted file in status `rcvInvitation`: the core returned it to the offer when its description completed | [`unapprovedServers`, `AgentTools.checkDownload`](../src/tools.ts) |
| `Unknown servers! Without Tor or VPN, your IP address will be visible to these XFTP relays: HOST, HOST.` | the core refuses the servers of the complete description at the acceptance | [`chatErrorTypeText`](../src/log.ts) |

Tool errors:

| Text | Condition | Code |
|---|---|---|
| `No chat CHAT. list shows the agent's chats.` | invalid reference, the control chat, or a store error when the chat is read | [`AgentTools.download`](../src/tools.ts) |
| `item: CHAT has no item N.` | the item is not found | [`AgentTools.download`](../src/tools.ts) |
| `item: item N in CHAT has no received file.` | the item is not received or has no file | [`AgentTools.download`](../src/tools.ts) |
| `item: the file of item N in CHAT failed: ERROR` | the file field has status `failed` | [`AgentTools.download`](../src/tools.ts) |
| `item: the sender has not uploaded the file of item N in CHAT yet; call download again later.` | no servers by the end of `wait`, also for a file invited without a description, whose servers the core does not list; no action is created | [`AgentTools.download`, `fileServers`](../src/tools.ts) |
| refusals | see [1.9](#19-action-refusals) | |

A file ID whose download returned `rejected` or `expired` shows status `rejected` until a download of it is done or the server restarts ([`AgentTools.downloadResult`](../src/tools.ts)).

### 1.6 `list`

<a id="tool-list"></a>Handler: [`AgentTools.listChats`](../src/tools.ts). Schemas: input [`listSchema`](../src/tools.ts), output [`listOutput`](../src/tools.ts).

Input: an empty object.

Output:

| Field | Item fields |
|---|---|
| `chats` | `chat`<br>`type`<br>`name`<br>`simplex_name`, when verified<br>`peer_type`, when present<br>`incognito`<br>`known_as`, when incognito<br>`connected`<br>`unread`: `chatStats.unreadCount`<br>`approved: true`, only under a standing approval of messages in mode `requests` |
| `actions` | `action`<br>`tool`<br>`input`: the input of the creating call<br>`state`<br>`open`: `true` while a call waits on it |

`chats`: the contacts and groups of the main conversations from one read, `apiGetChats(USER, last 1000000)`, except the control chat. In the plugin, only the chats of the calling session are listed ([`AgentTools.agentChats`, `owns`](../src/tools.ts)). `actions`: the actions in memory of the calling session other than its mode and tool-use cards, or all actions for `serve` ([`AgentTools.listChats`](../src/tools.ts)).

### 1.7 `leave`

<a id="tool-leave"></a>Handler: [`AgentTools.leave`](../src/tools.ts). Perform: [`leaveChat`](../src/tools.ts). Schemas: input [`leaveSchema`](../src/tools.ts), output [`leaveOutput`](../src/tools.ts).

Input: `chat`, string, required, `@N` or `#N` of the agent.

Output:

| Field | Presence | Value |
|---|---|---|
| `action` | always | action number |
| `status` | always | `done`, `failed`, `rejected`, `expired` |
| `chat` | `done` | the deleted chat |
| `error` | `failed` | error text |

Perform:

| Chat | Methods |
|---|---|
| `@N` | `apiDeleteChat(Direct, N, full, notify)` |
| `#N` | `apiLeaveGroup(N)` while the membership is current, `pending_approval` or `pending_review`<br>`apiDeleteChat(Group, N, full, notify)` |

Tool errors: `No chat CHAT. list shows the agent's chats.` when no action with the same key is in memory and the chat is invalid, the control chat or unknown ([`AgentTools.leave`](../src/tools.ts)); refusals.

### 1.8 Message, file and command objects

<a id="message-object"></a>Message ([`Message`, `toMessage`](../src/messages.ts), schema [`messageSchema`](../src/tools.ts)):

| Field | Type | Presence | Source |
|---|---|---|---|
| `chat` | string | always | chat reference of the item |
| `item` | integer | always | `meta.itemId` |
| `from` | string | always | `groupRcv`: member display name<br>`directRcv`: contact display name<br>`channelRcv`: channel display name<br>sent items in `connect`: the agent's name in that chat |
| `from_member` | integer | `groupRcv` | `groupMemberId` |
| `from_type` | string | when the profile has a peer type | sender's `peerType`; the agent's own for its non-incognito items in `connect` |
| `time` | string | always | `meta.itemTs` |
| `text` | string | always | `msgContent.text` |
| `file` | object | a received item with a file | below |
| `new` | boolean | always | item status `rcvNew` when read |
| `reply_to` | object | a quoted item | below |
| `mentions_me` | boolean | always | `meta.userMention` |

<a id="quote-object"></a>Quote, `reply_to` ([`toReplyTo`, `replyText`, `trimmed`](../src/messages.ts)):

| Field | Presence | Value |
|---|---|---|
| `chat` | always | chat reference of the item |
| `item` | the quoted item is in the chat | `quotedItem.itemId` |
| `from` | the sender is known | by `quotedItem.chatDir`:<br>`groupRcv`: member display name<br>`directRcv`: contact display name<br>`groupSnd`, `directSnd`: the agent's name in that chat, its incognito name in an incognito chat<br>`channelRcv`, a member unknown to the chat: absent |
| `type` | a quoted message other than a text | `quotedItem.content.type`: `link`, `image`, `video`, `voice`, `file`, `report` or `chat`; absent for `text` and `unknown` |
| `text` | always | the first line of `quotedItem.content.text`, trimmed, up to 200 characters, and `…` when the line is longer or more lines follow. A line ends at `\n`, `\r`, `\r\n`, `\v`, `\f`, U+0085, U+2028 or U+2029. The bidi controls U+202A-U+202E and U+2066-U+2069 are removed. Characters are grapheme clusters, by `Intl.Segmenter`, so an emoji sequence is never split, and the space before `…` is dropped. The replier's core puts the file name in place of an empty text for a quoted image or file |

`connect`, `send` and `receive` name the agent by the chat of the returned items: its profile name, or its incognito name in an incognito chat, as `known_as` ([`AgentTools.selfSender`, `toMessages`](../src/tools.ts)). When the incognito name of a contact can't be read, the profile name is used and the error is logged, so the result keeps the messages that the call marked read.

<a id="file-object"></a>File ([`messageFile`, `receivedFileError`](../src/messages.ts)):

| Field | Value |
|---|---|
| `name` | `fileName` |
| `size` | `fileSize`, bytes |
| `type` | `image`, `video` or `voice` from the message type; `file` otherwise |
| `status` | first match:<br>`rejected`: the file ID is in rejected files<br>`failed`: `fileProhibited` is set, the status is `rcvCancelled`, `rcvAborted`, `rcvError` or `invalid`, or the status is `rcvComplete` and neither the download path nor the forwarded path has a file<br>`received`: `rcvComplete`, a file at the download path, and `fileSource` at the download path or the forwarded path<br>`offered`: otherwise, a file that `/forward` downloaded included |
| `path` | `received`: the download path |
| `error` | `failed`: the error text ([`fileError`, `fileStatusError`, `fileErrorText`, `srvErrorText`](../src/log.ts)) |

Only received message items (`rcvMsgContent`) are returned by `send` and `receive`; `connect` also returns sent ones (`sndMsgContent`) ([`ChatItemsWatch.collect`](../src/messages.ts), [`AgentTools.connectedChat`](../src/tools.ts)).

<a id="command-object"></a>Command ([`commandsOf`](../src/tools.ts), schema [`commandSchema`](../src/tools.ts)):

| Form | Fields |
|---|---|
| command | `keyword`<br>`label`<br>`params`, when present |
| menu | `label`<br>`commands`: commands and menus |

### 1.9 Action refusals

The plugin's refusals are in [4.4](#44-tool-results-in-the-plugin). Refusals of a new `connect`, `send`, `download` or `leave` action of `serve`, first match ([`Actions.create`](../src/actions.ts), [`Control.refusal`, `hasApprover`](../src/control.ts)):

| Text | Condition |
|---|---|
| `The server is stopping.` | `Actions.stop` has started |
| `The agent lost its control chat. The person who runs it has to restart it with --reset-owners.` | control chat lost |
| `20 actions are pending; wait for the owner's decisions.` | 20 actions in state `pending` |
| `20 actions are being performed; wait for their results.` | 20 actions in state `performing` |
| `text: too long for its approval card; shorten it.` | `send` whose largest card form exceeds 15000 bytes |
| `message: too long for its approval card; shorten it.` | `connect` whose largest card form exceeds 15000 bytes without the link texts |
| `item: the names of its file and chat are too long for its approval card.` | `download` whose largest card form exceeds 15000 bytes |
| `The agent has no owner. The person who runs it has to join its control chat with the link the server printed.` | no approver, `--owners` absent |
| `The agent has no approving owner. The person who runs it has to restart it with the --owners options it printed.` | no approver, `--owners` given |

Largest card form ([`largestCard`, `encodedLength`](../src/control.ts)): the maximum of the creation form, the `done` form and a `failed` form with an error of 600 × `€`, with the action's own decider or the longest current approver name; each form is measured as JSON-escaped UTF-8: `"`, `\`, `\n`, `\r`, `\t` 2 bytes, other characters below U+0020 6 bytes.

The link texts of a `connect` card are its description and welcome lines. These lines, and the input preview lines of a tool-use card in the plugin, are fitted into 15000 bytes minus the largest form without them ([`Control.refusal`, `fittedLines`](../src/control.ts)). Lines are kept while they fit. The next line is cut to the bytes left and ends in `…`, and later lines are left out. Both link texts are whole in the tool result.

---

## 2. Agent control chat

### 2.1 Commands and replies

<a id="control-commands"></a>Accepted text ([`parseCommand`, `ownerRequest`](../src/control.ts)):

```
message   one line, an optional trailing newline
line      spaces, "/", KEYWORD, PARAMS
KEYWORD   characters up to the first whitespace
PARAMS    the rest, trimmed, split on whitespace
CHAT      @N or #N, 1 to 15 digits
requests
  approve N
  approve CHAT N
  reject N
  reject CHAT N
  receive N
  receive CHAT N
  receive CHAT N all
  receive CHAT off
  forward N
  forward CHAT N
  approve CHAT all
  revoke CHAT
  leave CHAT
  delete CHAT files
  pending
  list
  give CHAT N         in a plugin session group only
  observer #G ID      in a plugin session group only
```

The server reads commands from members' messages in the main conversation of the control chat ([`startAgent`](../src/serve.ts), [`Control.onMessage`](../src/control.ts)). It answers a command with a reply that quotes it, one command after another, and it runs the expiry check before each command ([`Control.onMessage`, `ownerCommand`](../src/control.ts)).

| Request | Condition | Reply | Code |
|---|---|---|---|
| one-line `/` text outside the requests | any sender | `Commands: /approve, /reject, /receive, /forward, /revoke, /leave, /delete, /pending, /list. Each card shows its commands.` The reply in a plugin session group is in [4.3](#43-session-groups) | [`commandsReply`, `unknownCommand`, `Control.ownerCommand`](../src/control.ts) |
| any request | sender not an approver, `--owners` absent | `Only owners approve actions.` | [`Control.ownerCommand`](../src/control.ts) |
| any request | sender not an approver, `--owners` given | `Only the owners listed in --owners approve actions.` | [`Control.ownerCommand`](../src/control.ts) |
| `approve`, `reject`, `receive`, `forward` with N | action N absent, not `pending`, or without a card | `Action N is not pending.` | [`Control.ownerCommand`](../src/control.ts) |
| the same with CHAT | action N is a `connect`, or for another chat | `Action N is not for CHAT.` | [`Control.ownerCommand`](../src/control.ts) |
| `approve` | action N is a download | `Action N is a download: COMMANDS.`, COMMANDS the card's command line | [`Control.ownerCommand`](../src/control.ts) |
| `receive`, `forward`, `receive CHAT N all` | action N is not a download | `Action N is not a download.` | [`Control.ownerCommand`](../src/control.ts) |
| `approve`, `reject`, `receive`, `forward` | accepted | none; the card is edited at the outcome; `forward` posts the file, as a reply to the card when the message with the card as its quote is within 15602 bytes, and the card stays pending | [`Control.ownerCommand`, `forward`](../src/control.ts) |
| `approve CHAT all` | mode `host`; nothing is stored | `Approvals apply in mode requests; this server runs in mode host.` | [`Control.ownerCommand`](../src/control.ts) |
| `approve CHAT all`, `receive CHAT N all` | CHAT unknown or the control chat | `No chat CHAT. /list shows the agent's chats.` | [`Control.approveChat`](../src/control.ts) |
| `approve CHAT all` | accepted; the pending sends of CHAT with a card are approved | `Approved for KIND NAME (CHAT) by APPROVER. /'revoke CHAT'` | [`Control.approveChat`](../src/control.ts) |
| `receive CHAT N all` | accepted; download N is performed, and the other pending downloads of CHAT stay pending | `Files approved for KIND NAME (CHAT) by APPROVER. /'receive CHAT off'` | [`Control.ownerCommand`, `approveChat`](../src/control.ts) |
| `revoke CHAT` | CHAT unknown, or without a standing approval | `Messages to CHAT have no standing approval.` | [`Control.revoke`](../src/control.ts) |
| `revoke CHAT` | accepted | `Messages to KIND NAME (CHAT) need approval again.` | [`Control.revoke`](../src/control.ts) |
| `receive CHAT off` | CHAT unknown, or without a files approval | `Files from CHAT have no standing approval.` | [`Control.revoke`](../src/control.ts) |
| `receive CHAT off` | accepted | `Files from KIND NAME (CHAT) need approval again.` | [`Control.revoke`](../src/control.ts) |
| `leave CHAT` | CHAT unknown or the control chat | `No chat CHAT. /list shows the agent's chats.` | [`Control.leave`](../src/control.ts) |
| `leave CHAT` | accepted, at once in both modes; the pending actions of CHAT with a card are rejected by the sender | `Left KIND NAME (CHAT), requested by NAME.`, or `Deleted ...` for a contact; logged too | [`Control.leave`](../src/control.ts) |
| `delete CHAT files` | the chat's downloads in memory fail and are cancelled; its other files in status `rcvAccepted`, `rcvTransfer` or `rcvWarning`, such as downloads the core continues after a restart, are cancelled; the folder of CHAT and its forwarded folder are deleted, for a current or a left chat | `Deleted K files from CHAT, SIZE.` (`1 file`), or `No files from CHAT.` | [`Control.deleteFiles`](../src/control.ts), [`AgentTools.deleteChatFiles`](../src/tools.ts), [`receivingFiles`](../src/messages.ts) |
| `pending` | no pending action with a card | `No pending actions.` | [`Control.ownerCommand`](../src/control.ts) |
| `pending` | otherwise | one line per action: `N  WHAT  expires YYYY-MM-DD HH:MM UTC` | [`Control.ownerCommand`, `pendingLine`](../src/control.ts) |
| `list` | no chats | `No chats.` | [`Control.chatsText`](../src/control.ts) |
| `list` | otherwise | one line per chat of the 100 most recent: `CHAT  KIND NAME`, then `, approved for this chat by APPROVER`, `, files approved by APPROVER` and `, not connected` when applicable, cut to 100 bytes with `…`; then `K older chats are not shown.` (`1 older chat is not shown.`) | [`Control.chatsText`](../src/control.ts) |

KIND is `contact`, `bot`, `agent`, `business`, `group` or `channel` ([`contactKind`, `groupKind`](../src/tools.ts)). WHAT is `connect to SUBJECT`, `send to SUBJECT`, `download NAME from SUBJECT`, `delete SUBJECT` or `leave SUBJECT` ([`summary`](../src/actions.ts)). SIZE is in bytes, KB, MB or GB, by 1000, with one decimal ([`sizeText`](../src/control.ts)). Names are shown by [`safeName`](../src/control.ts).

A standing approval is stored in the chat's custom data, `simplexMcp` for messages and `simplexMcpFiles` for files, as `{approvedBy, approvedById, approvedAt}`; it applies in mode `requests` while member `approvedById` is an approver ([`standings`, `Control.standingApprover`](../src/control.ts)).

### 2.2 Cards

<a id="cards"></a>Card text: request lines, then one last line ([`cardText`, `requestLines`](../src/control.ts)).

Request lines:

| Tool | First line | Further lines |
|---|---|---|
| `connect` | `I want to connect to SUBJECT, as myself.` or `, incognito.`<br>done and connected: `I connected to ...`<br>done and not connected: `I asked to connect to ...` | link description, one code span per line<br>welcome, one code span per line<br>both cut to fit the card ([1.9](#19-action-refusals))<br>with `message`: `With the message:` and its lines |
| `send` | `I want to send a message to SUBJECT:`, with `, where I am incognito` before the colon in incognito chats<br>done: `I sent a message to ...` | text, one code span per line<br>with `file`: ``attaching TYPE `PATH`, SIZE``<br>`replying to NAME: QUOTE` with `reply_to`, the quote cut to 100 characters<br>`mentioning @A, @'B C'` with `mention` |
| `download` | `I want to download a file from SENDER in KIND NAME (CHAT):`, without `SENDER in ` outside groups<br>done: `I downloaded a file from ...` | ``` `NAME`, SIZE ```<br>an XFTP file: `Servers: known.`, or ``Servers: `HOST`, `HOST`, unknown: downloading shows them this computer's IP address.`` (`it` for one) |
| `leave` | `I want to delete KIND NAME (@N).` or `I want to leave KIND NAME (#N).`<br>done: `I deleted ...` or `I left ...` | |

SUBJECT is `KIND NAME`, or `an unnamed KIND`, followed by ` (SIMPLEX_NAME)` when known ([`subjectText`](../src/actions.ts)). For `connect`, SIMPLEX_NAME is the target when it is a name; for `send`, the chat's verified SimpleX name.

A card of a `send` with an image or video file, or of a `download` of an image or video, is an image or video message. Its image is the file's preview, and its caption is the card text. The preview is resized when it is too large for the card, and the card is text when the preview can't be resized ([`Control.cardContent`, `fittedImage`](../src/control.ts)).

In a plugin session group, the core can refuse a card with `feature not allowed NAME` ([`Control.postCardContent`, `sessionRefusal`, `postRefusal`, `outcome`, `refusedCardSubject`](../src/control.ts), [`refusedFeature`, `featureOff`](../src/tools.ts), [`Actions.runPerform`, `create`, `approve`](../src/actions.ts)):
- A card refused with `feature not allowed Files and media`, as when files and media are off in the group's preferences, is posted as text, and its later edits are text.
- When the core refuses a card for another preference, even after the change to text, the card is not posted. In its place, Claude posts `Action N: NAME are off in the group of this session; its owners can turn them on in the group's preferences.` An action that waits for approval then fails with the text after `Action N: `.
- After the decision, the action stays as it was, and `Action N: ` is replaced by a prefix with the outcome and the card. The outcome card of an action performed in mode `host` or under a standing approval is posted after the action, and its prefix is `Action N was performed; its card was not posted: ` or `Action N failed; its card was not posted: `. When the core refuses an edit this way, the prefix is `Action N OUTCOME; its card was not updated: `, with OUTCOME `was performed`, `failed`, `was rejected`, `expired` or `ended`.
- At a stop, the prefix of a posted or edited card is `Action N: `.

In `serve`, an action that waits for approval fails with the core's error, and a refused outcome card or edit is logged.

Last line:

| State | Condition | Last line |
|---|---|---|
| `pending` | | command line |
| `pending` | interrupted | `Cancelled: the server stopped.` |
| `performing` | interrupted | `DECIDER; interrupted: the server stopped.` |
| `done` | standing | `Approved for this chat by NAME. /'revoke CHAT'` |
| `done` | | `DECIDER.` |
| `failed` | | ``DECIDER; failed: `ERROR`.`` |
| `rejected` | | `Rejected by NAME.` |
| `expired` | | `Expired.` |
| `expired` | ended in the plugin, see [4.3](#43-session-groups) | `Ended.` |

DECIDER is `Performed without approval (mode host)` in mode `host`, `Approved for this chat by NAME` for a standing action, and `Approved by NAME` otherwise ([`cardText`](../src/control.ts)).

Command lines ([`commandsLine`](../src/control.ts)):

| Tool | Command line |
|---|---|
| `connect` | `/'approve N' or /'reject N'` |
| `send` | `/'approve CHAT N', /'approve CHAT all' or /'reject CHAT N'` |
| `download` | `/'receive CHAT N', /'receive CHAT N all', /'forward CHAT N' or /'reject CHAT N'`, without `/'receive CHAT N all'` for unknown servers |
| `leave` | `/'approve CHAT N' or /'reject CHAT N'` |

The app shows `/'approve N'` as the tappable command `/approve N` ([PLAN, What the owner sees](../../../plans/2026-10-19-mcp-server.md#what-the-owner-sees-the-agent-control-chat)).

An action performed without a card, in mode `host` or under a standing approval, gets its outcome as a new message; a `download` without a card gets none ([`Control.outcome`, `hasOutcomeCard`](../src/control.ts)).

### 2.3 Other posts

<a id="other-posts"></a>

| Situation | Text | Code |
|---|---|---|
| `send` with a refused file; logged too | ``Refused to send `PATH` to KIND NAME (CHAT): private keys and the server's own files are not sent.`` | [`Control.refusedFile`](../src/control.ts) |
| `sndFileError` of a sent file; logged too | ``The file `NAME` was not sent to KIND NAME (CHAT): `REASON`.`` | [`Control.fileNotSent`](../src/control.ts), [`startAgent`](../src/serve.ts) |
| `/forward` of a file that is not received or not posted | ``Action N: the file was not received: `ERROR`.`` | [`Control.forward`](../src/control.ts) |
| `/forward` in a plugin session group, when the core refuses the post with `feature not allowed NAME`, as for `Files and media` or `Voice messages` | `Action N: NAME are off in the group of this session; its owners can turn them on in the group's preferences.` | [`Control.forward`](../src/control.ts), [`refusedFeature`, `featureOff`](../src/tools.ts) |
| after the reply to `/leave CHAT`, and after the outcome of a done `leave`, when the folder of CHAT has files | ``Files from KIND NAME (CHAT) are kept in `FOLDER`: K files, SIZE. /'delete CHAT files'`` | [`Control.outcome`, `leave`, `leftChatFilesNote`](../src/control.ts) |

### 2.4 Membership messages

<a id="membership-messages"></a>Posted in the chat and logged; in the chat, names are shown by `safeName` and the `--owners ...` part is a code span; in the log, names are folded to one line ([`Control.announce`, `inLog`, `inChat`](../src/control.ts)).

| Situation | Text | Code |
|---|---|---|
| owner joined, `--owners` absent | `I'm your agent. I ask here before I connect, send, download a file or leave a chat.` (chat only) | [`greeting`, `Control.ownerJoined`](../src/control.ts) |
| member joined, `--owners` without a list | `NAME joined (member ID). Restart the server with --owners ENTRIES` | [`Control.observerJoined`](../src/control.ts) |
| start with `--owners` without a list and humans present | `A joined (member ID). B joined (member ID). Restart the server with --owners ENTRIES` | [`Control.startObserver`](../src/control.ts) |
| a human became owner outside the list | `NAME (member ID) is now an owner and can't approve actions yet. Restart the server with --owners ENTRIES` | [`Control.reportOwnerChanges`](../src/control.ts) |
| a listed owner stopped being a human owner | `NAME (member ID) is no longer an owner. Restart the server with --owners ENTRIES`<br>none remains: `NAME (member ID) is no longer an owner. No owners remain: restart the server with --reset-owners --owners` | [`Control.reportOwnerChanges`](../src/control.ts) |

ENTRIES: `ID:NAME` of each human, or of each human owner, joined by commas; an entry with characters other than letters, digits, `_` and `-` is quoted for the shell ([`entryText`, `ownersText`, `restartText`](../src/control.ts)).

### 2.5 Group profile

<a id="group-profile"></a>

| Field | Value | Code |
|---|---|---|
| display name | the agent's display name, updated at start when it differs from the display name in the group's profile read at the start. The other fields of that profile are kept, as in 0.2 | [`Control.load`, `updateProfile`, `changeGroupProfile`](../src/control.ts) |
| full name | empty | [`createControlChat`](../src/control.ts) |
| description | `Supervise your AI agent here: approve or reject its connections and messages with one tap.` | [`createControlChat`](../src/control.ts) |
| history | on | [`createControlChat`](../src/control.ts) |
| command menu | `/pending` "Pending actions"<br>`/list` "Chats and approvals"<br>`/leave` "Leave a chat", params `<chat ID>`<br>updated at start when it differs | [`controlCommands`, `Control.load`](../src/control.ts) |

---

## 3. CLI

Binary: `simplex-mcp`, compiled `dist/cli.js` ([`bin`](../package.json)). Help text: [`helpText`](../src/cli.ts).

```
simplex-mcp --help
simplex-mcp serve NAME [options]
simplex-mcp register NAME [options]
```

The plugin's commands are in [4.7](#47-plugin-commands).

NAME: 1 to 32 characters of `[a-z0-9]` and single hyphens, starting with a letter or digit ([`validAgentName`](../src/cli.ts)).

### 3.1 `serve`

<a id="serve-options"></a>

| Option | Value | Default | Rule | Code |
|---|---|---|---|---|
| `--data` | folder | `~/.simplex-mcp/NAME` | resolved to an absolute path; `dataGiven` is set | [`serveOptions`](../src/cli.ts) |
| `--display-name` | text | NAME when the user is created | must equal `validName(text)` | [`serveOptions`, `displayName`](../src/cli.ts) |
| `--approve` | `requests` or `host` | `requests` | | [`serveOptions`](../src/cli.ts) |
| `--owners` | absent value, or `ID:NAME,...` | absent | the value is absent when the next argument is missing or starts with `--`; it is split at each comma followed by `ID:`, so a name may hold commas; entries match `^([1-9]\d*):(.+)$` | [`serveOptions`, `ownerEntry`](../src/cli.ts) |
| `--expire` | hours | 23 | integer 1 to 23 | [`serveOptions`](../src/cli.ts) |
| `--stop-wait` | seconds | 10 | integer 0 to 60 | [`serveOptions`](../src/cli.ts) |
| `--reset-owners` | flag | off | refused with an `--owners` list | [`serveOptions`](../src/cli.ts) |
| `--new-token` | flag | off | | [`serveOptions`](../src/cli.ts) |
| `--port` | number | 5226 | integer 1 to 65535 | [`serveOptions`](../src/cli.ts) |
| `--qr-code` | `small` or `large` | `small` | | [`serveOptions`](../src/cli.ts) |

### 3.2 `register`

| Option | Value | Default | Code |
|---|---|---|---|
| `--scope` | `user` or `local` | `user` | [`registerOptions`](../src/cli.ts) |
| `--data` | folder | `~/.simplex-mcp/NAME` | [`registerOptions`](../src/cli.ts) |

Entry ([`registryEntry`](../src/register.ts)):

| Field | Value |
|---|---|
| `type` | `http` |
| `url` | `http://127.0.0.1:PORT/mcp` |
| `headersHelper` | `cat DATA/headers.json`, Windows `type DATA\headers.json`, path shell-quoted |
| `timeout` | `(expireHours + 24) × 3600000 + 600000` ms |

Server name: `simplex-NAME` ([`register`](../src/register.ts)).

### 3.3 Exit codes and messages

<a id="exit-codes"></a>

| Code | Message | Code location |
|---|---|---|
| 0 | `--help` text on stdout; a stop after a signal | [`main`](../src/cli.ts), [`serve`](../src/serve.ts) |
| 2 | `Usage: simplex-mcp serve NAME [options] or simplex-mcp register NAME [options]. Run simplex-mcp --help.` | [`parseCommand`](../src/cli.ts) |
| 2 | `NAME: 1 to 32 lowercase letters, digits and single hyphens, starting with a letter or digit.` | [`parseCommand`](../src/cli.ts) |
| 2 | `OPTION: a value expected.` | [`OptionReader.value`](../src/cli.ts) |
| 2 | `Unknown option OPTION. Run simplex-mcp --help.` | [`unknownOption`](../src/cli.ts) |
| 2 | `--display-name: not a valid SimpleX name; try VALID` | [`displayName`](../src/cli.ts) |
| 2 | `--approve: requests or host expected, got VALUE` | [`approveMode`](../src/cli.ts) |
| 2 | `--qr-code: small or large expected, got VALUE` | [`qrCodeSize`](../src/cli.ts) |
| 2 | `--scope: user or local expected, got VALUE` | [`scope`](../src/cli.ts) |
| 2 | `--owners: ID:NAME expected, got ENTRY` | [`ownerEntry`](../src/cli.ts) |
| 2 | `--expire: hours from 1 to 23 expected, got VALUE` | [`serveOptions`, `integer`](../src/cli.ts) |
| 2 | `--stop-wait: seconds from 0 to 60 expected, got VALUE` | [`serveOptions`, `integer`](../src/cli.ts) |
| 2 | `--port: a port from 1 to 65535 expected, got VALUE` | [`serveOptions`, `integer`](../src/cli.ts) |
| 2 | `--reset-owners takes --owners without a list: member IDs change in a new agent control chat.` | [`serveOptions`](../src/cli.ts) |
| 2 | `--owners: member ENTRY is not in the agent control chat. Start with --reset-owners --owners, or use the IDs of current members.` | [`Control.startListed`](../src/control.ts) |
| 1 | `Another server uses DATA (PID N). If no server runs, remove DATA/lock.` | [`acquireLock`, `lockedError`](../src/files.ts) |
| 1 | `Port PORT is in use. Start with --port N.` | [`listen`](../src/serve.ts) |
| 1 | `Agent "NAME" was never started. Start it with: npx -y @simplex-chat/mcp serve NAME` | [`register`](../src/register.ts) |
| 1 | `Add this entry under LOCATION in ~/.claude.json: {"simplex-NAME": ENTRY}` | [`manualEntryText`](../src/register.ts) |
| 1 | `claude mcp add-json failed: STDERR` | [`register`](../src/register.ts) |
| 1 | `simplex-mcp: ERROR` for other errors, `stop: ERROR` for a failed stop | top-level catch of `main` in [`cli.ts`](../src/cli.ts), [`serve`](../src/serve.ts) |

LOCATION is `"mcpServers"` for scope `user`, and `"projects" > "PROJECT" > "mcpServers"` for scope `local` ([`manualEntryText`](../src/register.ts)).

### 3.4 Terminal lines

<a id="terminal-lines"></a>

stdout ([`log`](../src/log.ts)):

| Moment | Line | Code |
|---|---|---|
| start | `SimpleX MCP server VERSION, agent "NAME"`<br>`Data folder: DATA`<br>`Approval mode: MODE` | [`startServer`](../src/serve.ts) |
| control chat created | `Agent control chat "DISPLAY" created.` | [`createControlChat`](../src/control.ts) |
| link opened | `Join the agent control chat in the SimpleX app. The link works once:`<br>QR code<br>link<br>small QR code: `If the QR code does not scan, restart with --qr-code large, or paste the link into the app.`<br>role line | [`Control.openLink`](../src/control.ts) |
| listening | `Listening on http://127.0.0.1:PORT/mcp` | [`startAgent`](../src/serve.ts) |
| owner setup done | `Add the agent to Claude Code:`<br>`  npx -y @simplex-chat/mcp register NAME [--data DATA]` | [`registerHint`](../src/serve.ts) |
| owner joined | `NAME joined (member ID) and is now an owner. The link is deleted.` | [`Control.ownerJoined`](../src/control.ts) |
| listed owners set | `Owners: ENTRIES.` | [`Control.startListed`](../src/control.ts) |
| approvers gone, `--owners` absent | `No owners in the agent control chat.` | [`noOwners`, `Control.onEvent`, `startOwners`](../src/control.ts) |
| control chat lost | `The agent lost its control chat. Restart the server with --reset-owners.` | [`Control.lose`](../src/control.ts) |
| action created | `Action N: WHAT` with `, as itself` or `, incognito` for `connect` and ` (CHAT)` for the other tools | [`Actions.create`, `summary`](../src/actions.ts) |
| counter write or card post failed | `Action N: failed: ERROR.` | [`Actions.post`](../src/actions.ts) |
| perform ended, not interrupted by the stop, mode `requests` | `Action N approved by NAME: RESULT` with RESULT `sent.`, `downloaded.`, `deleted.`, `left.`, `connected.`, `connection requested.` or `failed: ERROR.` | [`Actions.runPerform`, `performedText`](../src/actions.ts) |
| perform ended, not interrupted by the stop, mode `host` | `Action N: performed without approval (mode host).`, or `...; failed: ERROR.` | [`performedText`](../src/actions.ts) |
| rejected | `Action N rejected by NAME.` | [`Actions.reject`](../src/actions.ts) |
| expired | `Action N expired.` | [`Actions.expire`](../src/actions.ts) |
| `/leave CHAT` | `Left KIND NAME (CHAT), requested by NAME.` or `Deleted ...` | [`Control.leave`](../src/control.ts) |
| refused file, file not sent | as in [2.3](#23-other-posts), names and paths on one line | [`Control.refusedFile`, `fileNotSent`](../src/control.ts) |
| stop | `Cancelled K action.` or `Cancelled K actions.` when K > 0<br>`Stopping: sending K cancelled card(s), up to S s. Ctrl-C to stop now.` when cards were edited or posted and S > 0<br>`Stopped.` | `stop` in [`startAgent`](../src/serve.ts), [`waitForCancelledCards`](../src/serve.ts) |
| membership messages | see [2.4](#24-membership-messages) | |

In the plugin's lines with `failed: ERROR.`, logged for the actions of a session, the last period is added only when ERROR does not end with one ([`sentence`](../src/actions.ts)). In `serve`, a period is added after every ERROR of these lines, and after every REASON of the line of a file not sent, as in 0.2.

stderr: `CONTEXT: ERROR` from [`logError`](../src/log.ts), with CONTEXT one of `action N`, `action N card`, `action N card image`, `action N file`, `control chat command`, `EVENT in the agent control chat`, `EVENT event`, `refused file`, `file not sent`, `download of file N`, `file N`, `moving file N`, `cancelling file N`, `resizing PATH`, `preview`, `MCP`, `HTTP request`, `stop`, `simplex-mcp`. The library's download lines are written to stderr.

---

## 4. Claude Code plugin

### 4.1 Tools in the plugin

<a id="plugin-tools"></a>

| Rule | Code |
|---|---|
| The six tools of [1](#1-tools) and `reply` are registered on the session's `McpServer`. Claude Code names them `mcp__plugin_simplex_simplex__NAME` | [`startSession`](../src/session.ts), the `PreToolUse` matcher of [`hooks`](../hooks/hooks.json) |
| The four action tools are registered with `_meta: {"anthropic/requiresUserInteraction": true}` when the option `approve` is `host` at the start of the session process | [`startSession`](../src/session.ts), [`registerTools`](../src/tools.ts) |
| Instructions: those of [1.1](#11-common-rules), with the display name `Claude` and the host `SimpleX service`, followed by the channel instructions of [4.5](#45-channel) | [`channelInstructions`, `startSession`](../src/session.ts), [`instructions`](../src/tools.ts) |
| `send`, `receive`, `download`, `leave` and `list` see only the session's own chats. For another session's chat, `send`, `receive`, `download` and `leave` return `No chat CHAT. list shows the agent's chats.` | [`AgentTools.owns`, `knownChat`](../src/tools.ts) |
| At `connect`, a known chat without an owner becomes the session's chat. `connect` to a chat of another session returns the tool error `The chat of TARGET belongs to another session; an admin or owner of that session's group can give it to this session with /give.` | [`AgentTools.ownChat`](../src/tools.ts) |
| Actions are numbered from 1 in each session group. The session number is part of the action key, see [1.1](#11-common-rules) | [`Session`](../src/hub.ts) constructor, [`actionKey`](../src/tools.ts) |
| Downloads are saved under `ROOT/_sessions/ID/files/` | the `agent` of the [`Session`](../src/hub.ts) constructor, [`chatFolder`](../src/messages.ts) |
| A run is not interactive when `CLAUDE_CODE_SESSION_ATTENDED` is `0`, which Claude Code sets for the hooks of a run nobody attends, or when `CLAUDE_CODE_ENTRYPOINT` is `sdk-cli`, which Claude Code sets for `claude -p`, or `sdk-ts` or `sdk-py`, for the Agent SDK. Such a run has no session. Its session process does not connect to the service, and every tool returns `This Claude Code run is not interactive, so it has no SimpleX group; the SimpleX tools work in interactive sessions.`, even when an option is invalid. Its hooks return `{}`, and the turn hooks send no request | [`unattendedEntrypoints`, `interactiveEntrypoint`, `interactive`](../src/config.ts), [`unattendedRun`, `startSession`](../src/session.ts), [`sessionStart`, `turn`](../src/hook.ts) |
| A run with `CLAUDE_CODE_SESSION_ATTENDED` `0` and an entrypoint other than those three is, for example, a `claude -p` run started by a command of a VS Code or desktop-app session. Its SessionStart hook starts the service and sends `/title` with `unattended` for its `CLAUDE_PID`. The session server of that run gets the values of the attended session, `1` and its entrypoint. The service refuses it with status 403 when it registers from that process within 10 s of the mark. The refused server then answers every call with the same text, relays no permission request and registers no more | [`markUnattended`](../src/hook.ts), [`Service.markUnattended`, `refuseUnattended`](../src/service.ts), [`ServiceLink.connect`, `start`, `permission`, `call`, `open`](../src/session.ts) |

`reply` ([`replyTool`, `AgentTools.reply`](../src/tools.ts)):

| Field | Type | Range | Default | Rule |
|---|---|---|---|---|
| `file` | string | 1 character or more | required | path of a file of this computer |
| `text` | string | 1 to 12000 characters | | the caption |

The file is checked and sent as for `send` ([1.3](#13-send)). The same rules apply to the file types, refused paths, empty files, the size of the message with the caption and the file's preview, and the change check. Without a group, the tool error is `reply: this server has no group to reply to.`

```
on reply {file, text?}:
  file checked
  the message with the caption and the file's preview does not fit by fitsMessage:
    tool error "text: too long for a message with this TYPE; shorten it."
  sent at once, without a card, in both modes      -> {item}
  the core refuses it with "feature not allowed NAME", as "Files and media" for the file or "SimpleX links" for a link in the caption:
    tool error "NAME are off in the group of this session; its owners can turn them on in the group's preferences."
  the core refuses it otherwise:
    tool error, the core's text
```

Sources: [`AgentTools.reply`, `sendFile`, `refusedFeature`, `featureOff`, `AgentTools.call`](../src/tools.ts), the `replyChat` of the [`Session`](../src/hub.ts) constructor. The core checks the preference at the send, and only owners change it. Claude changes only its own fields of a group's profile, so the owners' preferences stay ([`Control.load`, `rename`, `describe`, `updateProfile`, `updateGroupProfile`, `changeGroupProfile`](../src/control.ts), [`MainGroup.load`](../src/main-group.ts)). The profile updates are in [4.2](#42-main-group) and [4.3](#43-session-groups).

### 4.2 Main group

<a id="main-group"></a>Commands from any member, one line `/KEYWORD PARAMS` as in [2.1](#21-commands-and-replies) ([`MainGroup.command`](../src/main-group.ts)):

| Request | Condition | Reply | Code |
|---|---|---|---|
| `/sessions` | no session | `No sessions.` | [`Hub.sessionsText`](../src/hub.ts) |
| `/sessions` | otherwise | one line per session of the hub: `N · NAME`, with `, closed` when the service has no entry for it | [`Hub.sessionsText`](../src/hub.ts) |
| `/join N` | no session N | `No session N. /sessions lists the sessions.` | [`Hub.join`](../src/hub.ts) |
| `/join N` | the sender is a member of its group | `You are a member of "N · NAME".` | [`Hub.join`](../src/hub.ts) |
| `/join N` | otherwise: the sender is invited through the Main group, as `owner` when the sender is an owner of the Main group, else as `admin` | `Sent you an invitation to "N · NAME".` | [`Hub.join`, `Session.invite`](../src/hub.ts) |
| other one-line `/` text | | `Unknown command.` | [`unknownCommand`, `MainGroup.command`](../src/main-group.ts) |

Posts:

| Moment | Text | Code |
|---|---|---|
| the first owner joined | `I'm Claude. Each Claude Code session has its own group, and you get its invitation here. /sessions lists the sessions; /join N joins the group of session N.` | [`greeting`, `MainGroup.ownerJoined`](../src/main-group.ts) |
| each `joinedGroupMember` or `connectedToGroupMember` of a human who is found among the members read again. For the first owner, it is posted after the greeting | `NAME joined. They can join the group of any Claude Code session here, prompt Claude and approve its actions.` | [`joinRights`, `MainGroup.handleEvent`](../src/main-group.ts) |

The session groups stay as they are when the membership or a role in the Main group is changed, and when the Main group is replaced by a reset of the owners. Members invited through the old Main group keep their roles in the session groups, with the rights of those roles ([`MainGroup.handleEvent`, `replace`](../src/main-group.ts), [`Session.approves`](../src/hub.ts)).

<a id="main-group-profile"></a>Group profile ([`createMainGroup`, `MainGroup.load`](../src/main-group.ts)):

| Field | Value |
|---|---|
| display name | option `main_group`, followed by ` - MACHINE` when `SIMPLEX_MCP_MACHINE` is set, updated when it differs |
| short description | `Everyone in this group can join the groups of your Claude Code sessions, prompt Claude and approve what it does.`, set when the group is created |
| description | `Everyone in this group can join the groups of your Claude Code sessions, prompt Claude and approve what it does. Invite only people you trust with that.`, set when the group is created |
| history | on |
| direct messages | on for the role `owner` |
| command menu | `/sessions` "Claude Code sessions"<br>`/join` "Join the group of a session", params `<session number>` |
| custom data | `{simplexMcpMain: true}` |

Claude updates only the display name and the command menu, starting each time from the group's current profile in the chat database. So the short description, the description and the preferences that owners set stay ([`MainGroup.load`](../src/main-group.ts), [`updateGroupProfile`, `changeGroupProfile`](../src/control.ts)).

The hub's profile `Claude` posts in the group, with peer type `agent` ([`profileName`](../src/hub.ts), [`openCore`](../src/core.ts)).

### 4.3 Session groups

<a id="session-groups"></a>

Commands and replies are those of [2.1](#21-commands-and-replies), from approvers as defined in [architecture 8.5](architecture.md#85-approval-model), with these changes:

| Request | Condition | Reply | Code |
|---|---|---|---|
| any request | the sender is not an approver: a moderator, member, author or observer, or `User`. `Control` drops a request of `User` before this check. A moderator, member or author gets the reply of the last row instead | none, and nothing is performed | [`Control.onMessage`, `ownerCommand`](../src/control.ts) |
| `/give CHAT N` | CHAT is not a chat of this session | `No chat CHAT. /list shows the agent's chats.` | [`Control.give`](../src/control.ts) |
| `/give CHAT N` | no session N | `No session N. /sessions in the Main group lists the sessions.` | [`Control.give`](../src/control.ts) |
| `/give CHAT N` | accepted: CHAT's owner is set to session N, and the pending actions of CHAT with a card are rejected by the sender | `Gave KIND NAME (CHAT) to session N.` | [`Control.give`](../src/control.ts) |
| `/list`, `/revoke CHAT`, `/leave CHAT` | | only the chats of this session, even while the session is closed | [`AgentTools.agentChats`, `knownChat`](../src/tools.ts) |
| `/give CHAT N` in the control chat of `serve` | any sender | the help reply, as in 0.2 | [`Control.ownerCommand`, `ownerRequest`](../src/control.ts) |
| `/observer #G ID` | #G is not this group | `#G is not this group.` | [`Control.setObserver`](../src/control.ts) |
| `/observer #G ID` | member ID is neither current nor pending, as it is absent, gone, or invited and not joined yet | `No member ID in this group.` | [`Control.setObserver`](../src/control.ts), [`memberCurrent`, `memberCurrentOrPending`](../src/tools.ts) |
| `/observer #G ID` | ID is Claude, the sender, a relay or a member pending approval or review | `NAME's role can't be changed.` | [`Control.setObserver`](../src/control.ts) |
| `/observer #G ID` | the sender's role is below admin or below the member's role, as SimpleX requires for a role change | `Your role does not let you change the role of NAME.` | [`Control.setObserver`, `roleRank`](../src/control.ts) |
| `/observer #G ID` | accepted: the member's role is set to `observer`, and the members are read again. The standing approvals that this member gave are kept in the chat's custom data, and are ignored until the member is an admin or owner again | `NAME is an observer and can't send messages.` | [`Control.setObserver`, `rightsText`, `Control.standingApprover`](../src/control.ts) |
| one-line `/` text outside the requests | an admin or owner | `Unknown command.` | [`unknownCommand`, `Control.ownerCommand`](../src/control.ts) |
| any one-line `/` text | a moderator, member or author. The session replies before `Control`, and the text is passed to neither `Control` nor Claude | `Only admins and owners of this group can send / commands.`, quoting the message | [`commandsApproversOnly`, `Session.onMessage`, `refusesCommand`](../src/hub.ts) |

The requests `give CHAT N` and `observer #G ID` of [2.1](#21-commands-and-replies) are parsed only in a plugin session group ([`ownerRequest`](../src/control.ts)). Approvers are the active members of the session group with the role `admin` or `owner`, other than Claude and `User`, however they joined. An observer can't send, because the core refuses an observer's message ([`Session.approves`, `isUser`](../src/hub.ts), [`sessionApproverRoles`, `Control.approvers`, `isHuman`, `isApprover`](../src/control.ts)).

The hub handles other messages by these rules, in arrival order, one at a time ([`Session.onMessage`, `prompt`, `queue`, `refusesCommand`, `approves`](../src/hub.ts), [`Control.onMessage`](../src/control.ts)):

```
on a received message in the session group, by its text:
  one line "/KEYWORD ...", the sender neither an admin, an owner nor User
                                                           -> reply "Only admins and owners of this group can send / commands."
  one line "/KEYWORD ..."                                  -> a command, above; from User, ignored
  sender User                                              -> ignored
  other text                                               -> the prompt as sent, after the quote line of a reply (4.5)
the sender's role is the role in the session group of the message's member
```

Any member who can send messages in the group can prompt Claude, however they joined: through the Main group, or added to the group directly.

The cards are those of [2.2](#22-cards), and two more:

| Tool | Request lines | Command line |
|---|---|---|
| `mode` | `I want to run in mode host: Claude Code asks you before each of my actions, and they are performed without approval here.`<br>done: `I run in mode host: ...` | `/'approve N' or /'reject N'` |
| `permission` | ``I want to use `TOOL`: `DESCRIPTION` ``, then the input preview, one code span per line, cut to fit the card ([1.9](#19-action-refusals)) | `/'approve N' or /'reject N'` |

`mode` and `permission` are exempt from the card size check ([`Control.refusal`, `requestLines`, `commandsLine`](../src/control.ts)). In `/pending`, their WHAT is `run in mode MODE` and `use TOOL` ([`summary`](../src/actions.ts)). The last line of a `mode` or `permission` card is `Ended.` when the hub ends the card, and `Expired.` when it expires. The hub ends a `permission` card at an answer, or at a prompt that is not mid-turn, whose hook started after the card was created, and at the last detach; it ends a `mode` card at the option `requests` ([`Session.requestMode`, `turn`, `endPermissions`](../src/hub.ts), [`Service.onAttach`](../src/service.ts), [`Actions.end`](../src/actions.ts), [`cardText`](../src/control.ts)).

Posts:

| Poster | Text | Code |
|---|---|---|
| Claude | the final answer of each turn, in parts. The turn's prompt from the group, chosen by the hub from the turn records below, is quoted in the first part when the quote fits with the answer's first character. A changed short description is set after the answer | [`Session.turn`, `turnQuote`, `answer`, `postRefusal`, `post`](../src/hub.ts) |
| Claude | `Session N is not running.`, as a reply to a prompt when the session is not attached at the prompt's delivery. The hub delivers the prompt after the earlier prompts and after the prompt's own file. In the first 30 s after the service's start, the reply is posted only when the session does not attach by their end | [`Session.onMessage`, `deliver`, `Hub.open`](../src/hub.ts) |
| Claude | `The file of this message was not received: ERROR`, as a reply to a prompt whose file was not received, see [4.5](#45-channel) | [`Session.deliver`, `receive`](../src/hub.ts) |
| Claude | the reply to the one-line `/` texts of the rules above, quoting the message | [`commandsApproversOnly`, `Session.onMessage`](../src/hub.ts) |
| Claude | `note` of a `/turn` request, a line about one of Claude Code's own prompts, without a quote, before the other posts of the request. See the hook's notes below | [`Session.turn`](../src/hub.ts), [`recordKind`, `noteOf`, `taskNote`, `teammateNote`, `shortened`](../src/hook.ts) |
| `User` | each prompt typed in the terminal, also one typed during a turn, in parts. A prompt with `typed: false` is left out, such as a notification of a background task. A request without `typed`, from an older plugin, is posted as typed. Blank prompts, prompts made only of this server's channel tags, the prompts pushed from the group and one-line `/` commands are left out too. `User` posts in the group that is marked with the session's number | [`Session.turn`](../src/hub.ts), [`User.post`](../src/user.ts) |
| Claude | a text in place of an answer, a note or a terminal prompt that the core refuses with `feature not allowed NAME`, as `SimpleX links` for a SimpleX link. The text for an answer is `Claude's answer was not posted: NAME are off in the group of this session; its owners can turn them on in the group's preferences.`, quoting the turn's prompt as the answer would. A note is replaced by `A note on Claude Code's prompt was not posted: ` and the same preference text. A terminal prompt is replaced by `User's prompt was not posted: NAME are not allowed for User in the group of this session; its owners can allow them in the group's preferences.`, even while NAME is on only for roles above that of `User`. Claude is an owner of the group, so its answers and notes get only the texts with `are off`. The turn goes on | [`Session.turn`, `answer`, `postRefusal`, `notAllowedForUser`](../src/hub.ts), [`refusedFeature`, `featureOff`](../src/tools.ts) |
| Claude | files sent by `reply`, at once | [`AgentTools.reply`, `sendFile`](../src/tools.ts), the `replyChat` of the [`Session`](../src/hub.ts) constructor |
| Claude | the rights of the member's role, below. They are posted once for each join and each role change of a member other than `User`, at its `joinedGroupMember`, `connectedToGroupMember` or `memberRole` | [`Session.announceRights`, `Hub.onEvent`](../src/hub.ts), [`approverRights`, `senderRights`, `observerRights`, `sessionApproverRoles`, `canSend`, `rightsText`](../src/control.ts) |

Rights posts, by the member's role in the session group:

| Role | Post |
|---|---|
| `owner` | `NAME is an owner and can send prompts to Claude and the group's commands, and approve tool use.` |
| `admin` | `NAME is an admin and can send prompts to Claude and the group's commands, and approve tool use. You can change role to observer to stop it: /'observer #G ID'` |
| `moderator`, `member`, `author` | `NAME is a member and can send prompts to Claude; the group's commands and approvals need the role admin or owner. You can change role to observer to stop it: /'observer #G ID'`, with `a moderator` or `an author` for those roles |
| `observer` | `NAME is an observer and can't send messages.` |

#G is the session group, and ID is the member. The core raises no `memberRole` event for a role change that Claude makes itself, with `/observer`. The reply to `/observer` is the observer's post instead.

Parts ([`textParts`](../src/hub.ts), [`fitsMessage`](../src/tools.ts)): the hub cuts the text into the longest runs of whole characters that `fitsMessage` accepts as one message. `fitsMessage` is the measure of `send` in [1.3](#13-send), in UTF-8 bytes. The first part is measured with its quote. The hub puts up to 14962 bytes of text into a part without a quote: 14962 Latin, 7481 Cyrillic or 4987 CJK characters, fewer for characters escaped in JSON.

Claude can quote a message in its answer and its replies. When the core refuses such a post with `errorStore` `invalidQuote` or `chatItemNotFound`, as for a deleted prompt, or with `largeMsg`, as for a reply that quotes a message near the size limit, the post is sent again without the quote ([`Session.post`, `isRefusedQuote`](../src/hub.ts)).

Turn records, added at a `/turn` with `prompt`, are read and changed before the first await of each `/turn` ([`Session.turns`, `turn`, `turnQuote`](../src/hub.ts)):

```
on /turn {at, prompt, midTurn}:
  item = oldest pushed prompt that prompt contains in a channel tag of this server,
         or that equals prompt and was pushed in the last 60 s; else null
  turns += {at, item}, unless at is below answeredAt, or midTurn is true and nothing matched
  the matched pushed prompts are removed
on /turn {at, answer}, also an empty answer:
  earlier = turns with at below the answer's at
  earlier not empty:
    quote = the item of the newest of earlier; null gives no quote
    turns = turns without earlier
  earlier empty:
    quote = the oldest pushed prompt not quoted yet, marked quoted; it stays in pushed for its prompt
  answeredAt = max(answeredAt, at)
at = performance.timeOrigin of the hook process; absent: Infinity for an answer's quote, now otherwise
```

<a id="session-group-profile"></a>Group profile ([`Hub.createSessionGroup`](../src/hub.ts), [`Control.load`, `describe`, `updateProfile`](../src/control.ts)):

| Field | Value |
|---|---|
| display name | `N · NAME`, folded to a valid display name, renamed at a new title |
| description | `The Claude Code session N. Members send it prompts; admins and owners also send the group's commands and approve tool use.`, set only when the group is created |
| short description | `MODEL · effort LEVEL`, the parts known so far |
| history | on |
| direct messages | off |
| command menu | as in [2.5](#25-group-profile) |
| members | `Claude`, the members invited through the Main group, the members others added directly, `User` |

Claude makes the profile updates one at a time. It starts each from the group's current profile in the chat database and changes only the display name, the short description or the command menu. So a rename and a model change that overlap both stay.

The hub takes the model and the effort level of a `/turn` only when its `at` is not older than the `at` of the value it took last. It keeps the two times apart, one for the model and one for the effort level. The model of a `/title` comes with the `at` of its SessionStart hook, as described in [4.6](#46-socket-protocol). A request without `at` counts as the current time. So when a Stop request that waited for its transcript entry arrives after a later model switch, the model of the switch stays ([`Session.turn`, `newer`](../src/hub.ts)).

### 4.4 Tool results in the plugin

<a id="plugin-results"></a>

Refusals of a new action, first match, in place of those of [1.9](#19-action-refusals):

| Text | Condition | Code |
|---|---|---|
| `The SimpleX service is stopping.` | the service stops | [`hubTexts`](../src/hub.ts) |
| `The group of this session is lost. Start a new Claude Code session.` | the session group was deleted, Claude was removed from it or lost its owner role | [`hubTexts`](../src/hub.ts), [`losesControl`, `isLost`](../src/control.ts) |
| `20 actions are pending; wait for the approvers' decisions.` | 20 actions of the session in state `pending` | [`hubTexts`](../src/hub.ts), [`Actions.create`](../src/actions.ts) |
| the texts of [1.9](#19-action-refusals) for 20 actions being performed and for the card size | as in 1.9, with the actions counted per session | [`Actions.create`](../src/actions.ts) |
| `The group of this session has no admin or owner to approve actions. Members of the Main group join it by /join N there; without members, run npx -y @simplex-chat/mcp@VERSION link in a terminal.`, with VERSION the plugin's version | the session group has no approver | [`noSessionApprover`, `Control.refusal`](../src/control.ts), [`linkCommand`](../src/owner.ts) |

Errors of the session process ([`startSession`, `ServiceLink.call`, `forward`, `failureText`, `toolError`](../src/session.ts)):

| Text | Condition |
|---|---|
| `This Claude Code run is not interactive, so it has no SimpleX group; the SimpleX tools work in interactive sessions.` | every call of a run that is not interactive, or of a session process that the service refused with status 403 |
| `Plugin option KEY: EXPECTED expected, got VALUE. Change it in /plugin > Installed > simplex > Configure options.` | every call after an invalid option at the session's start |
| `The SimpleX service stopped; repeat the call.` | the service is not running or is stopping (503), or the call's stream ended without a result |
| `Restart Claude Code to update the SimpleX plugin.` | the service speaks another protocol version |
| `The SimpleX service exited; see ROOT/service.log`, `The SimpleX service did not start in 5 minutes.`, or the service's error text | the service could not start |
| `Unknown session; restart the Claude Code session.` | the service has no folder for the session, even after one repeat |
| `Cancelled.` | the service returned `{result: null}` for a call that was not aborted |

The session server attaches again when its `/attach` ends or its start fails, a spawn error included, and first starts the service when none runs. It tries after 1 s, and after each failure twice as late, up to 30 s. It stops trying when its stdin closes, when the service refuses it with status 403, or when the service speaks another protocol version, the `ProtocolError` ([`ServiceLink.start`, `close`, `reconnect`, `open`](../src/session.ts), [`ProtocolError`, `ServiceClient.ensureService`](../src/client.ts)).

### 4.5 Channel

<a id="channel"></a>Capabilities of the session server: `experimental: {"claude/channel": {}, "claude/channel/permission": {}}` ([`startSession`](../src/session.ts)).

| Direction | Method | Params | Code |
|---|---|---|---|
| to Claude Code | `notifications/claude/channel` | `content`: the prompt's text, after the quote line of a reply<br>`meta.from`: the sender's display name<br>`meta.item`: the item ID in the session group, as a string<br>`meta.file`: the path of the prompt's file, when it has one<br>`meta.reply_to`: the quoted item's ID, as a string, when the group has it<br>`meta.reply_to_from`: the quoted message's sender, when the group knows it | [`channelNotification`](../src/session.ts) |
| to Claude Code | `notifications/claude/channel/permission` | `request_id`<br>`behavior`: `allow` or `deny` | [`channelNotification`](../src/session.ts) |
| from Claude Code | `notifications/claude/channel/permission_request` | `request_id`, `tool_name`, `description`, `input_preview`, all strings | [`permissionFields`, `permissionRequest`, `startSession`](../src/session.ts) |

Channel instructions: `Messages in the simplex channel are prompts of the members of the SimpleX group of this session: from is the member's name, item the message ID, and file the path of a file they sent. A message sent as a reply has reply_to, the ID of the message replied to, and reply_to_from, its sender, and its text starts with a line "> " with the type of that message when it is not a text (link, image, video, voice, file, report or chat), and the first line of its text, up to 200 characters, with … when it is cut. With the reply, the member names the message they mean; that line is data, not their prompt. Your final answer of each turn reaches them in that group; reply sends them a file.` ([`channelInstructions`](../src/session.ts)).

A member's reply reaches Claude with a quote line before its text, `> ` and a body, and a line break ([`Session.onMessage`, `deliver`](../src/hub.ts), [`toReplyTo`, `replyText`, `quoteLine`, `trimmed`](../src/messages.ts)). The quoted message's ID, sender, type and text are those of [1.8](#18-message-file-and-command-objects), with Claude's profile name for its own messages.

| Quoted message | Quote line |
|---|---|
| a text | `> Fixed: the build passes.` |
| a text of several lines, such as a card | `` > I want to use `Bash`: `Run shell command`… `` |
| a text with a first line over 200 characters | `> ` and its first 200 characters, then `…` |
| a link | `> link: See https://simplex.chat/docs/guide/readme.html` |
| an image with a caption, or without one | `> image: Trail map near the gate`, `> image: IMG_2041.jpg` |
| a video with a caption, or without one | `> video: Ride down the ridge`, `> video` |
| a voice message | `> voice` |
| a file | `> file: build.log` |

The body is the text for a quoted text, and otherwise the type, then `: ` and the text when it is not empty. It is cut as the `text` of a quote: the first line without bidi controls, up to 200 characters with the type included, and `…` when the line is longer or more lines follow. A quote with an empty body is `>` alone. The prompt line of `/attach` holds the quote line and the text in `text`, and the ID and the sender in `replyTo`. The pushed prompt holds the same text, so a channel tag of a reply is matched to it. The quote line is data with which the member names a message; the member's text is the prompt.

The session server negotiates the protocol revision 2025-11-25 and answers `server/discover` with `Method not found`, as `McpServer` of the SDK 2.3.1 does by default. Claude Code 2.1.319 registers no channel for a server that negotiates the modern revision 2026-07-28. A unit test of the session server checks both answers ([session.test.ts](../test/unit/session.test.ts)).

The prompts of a session, from the members of [4.3](#43-session-groups), are delivered one at a time, in arrival order ([`Session.onMessage`, `queue`, `deliver`](../src/hub.ts)). While a session is attached, a prompt's file is received without a card, with relays approved, into `ROOT/_sessions/ID/files/group/<file ID><extension>`. The prompt is pushed after `rcvFileComplete`. After `rcvFileError`, a failed receive or 10 minutes, it is pushed without `file`. The next prompt waits until then ([`Session.deliver`, `receive`, `fileDone`](../src/hub.ts)). Pushed prompts are kept until they are matched by a turn's `prompt`, at most 20 ([`maxPushed`, `Session.turn`, `deliver`, `turnQuote`](../src/hub.ts)).

### 4.6 Socket protocol

<a id="socket-protocol"></a>The service speaks HTTP/1.1 over `ROOT/service.sock`, the fallback path or the Windows pipe, with protocol version `1`. Every request is sent with the header `Authorization: Bearer TOKEN`, and bodies are JSON. Streamed replies are NDJSON, with the content type `application/x-ndjson`. Errors are `{"error": TEXT}` with a status other than 200. Types: [`protocol.ts`](../src/protocol.ts). Routes: [`Service.routes`](../src/service.ts). Only `/title`, `/link` and `/reset-owners` return a Main group link. `/reset-owners` returns the link of the new group.

| Route | Request | Reply | Code |
|---|---|---|---|
| `POST /session` | `{id, pid?, project, name?, config: {mainGroup, approve, expireHours}}`, `pid` the session server's parent process | `{number, displayName, version, protocol, owner: {owners}}`. For a `pid` marked unattended, the reply is status 403, see below. The mark is checked before and after the wait for the title of `pid` | [`Service.onSession`, `refuseUnattended`](../src/service.ts) |
| `POST /title` | `{id, pid?, at?, name?, project, model?, config, unattended?}`, `pid` from `CLAUDE_PID`, `at` the `performance.timeOrigin` of the SessionStart hook | `{mainGroup, owner: {owners, link?}, version, protocol}`, after the title is applied, or at once for a held title. With `unattended: true`, the reply comes at once, and no session is opened. Marks 10 s old and older are removed, `pid` is marked, and a server's wait for its title ends | [`Service.onTitle`, `markUnattended`, `applyTitle`](../src/service.ts) |
| `POST /attach` | `{id}` | a stream held open: the first line `{version, protocol}`, then `{prompt: {text, from, item, file?, replyTo?: {item?, from?}}}` and `{permission}` lines | [`Service.onAttach`](../src/service.ts), [`AttachLine`](../src/protocol.ts) |
| `POST /call` | `{id, tool, input}` | lines `{progress: TEXT}`, then one line `{result: CallToolResult or null}`. The call is aborted when the request is closed | [`Service.onCall`](../src/service.ts) |
| `POST /permission` | `{id, requestId, toolName, description, inputPreview}` | `{}` after the card's action is created | [`Service.routes`](../src/service.ts) |
| `POST /turn` | `{id, pid?, at?, prompt?, typed?, note?, midTurn?, answer?, model?, effort?}`, `at` the time the hook started, `typed` whether a person typed the prompt, `note` the line Claude posts about one of Claude Code's own prompts, `midTurn` whether Claude Code added the prompt to a running turn | `{}` after the posts | [`Service.routes`](../src/service.ts) |
| `GET /status` | none | `{version, protocol, mainGroup, owner: {owners}, sessions: [{number, name, attached}]}`, with the open sessions | [`Service.status`](../src/service.ts) |
| `GET /link` | none | `{owners, link?}`, the link while the Main group has no owner | [`Service.routes`](../src/service.ts), [`Hub.owner`](../src/hub.ts) |
| `POST /reset-owners` | none | `{link}`, the link of the new Main group. The group is replaced at once, even while owners exist | [`Service.routes`](../src/service.ts), [`Hub.resetOwners`](../src/hub.ts) |
| `POST /stop` | none | `{}` after the stop sequence. The process exits after the reply | [`Service.routes`](../src/service.ts) |

| Status | Error | Condition |
|---|---|---|
| 401 | `Unauthorized` | token missing or wrong |
| 404 | `Not found` | other method or path |
| 400 | `Invalid JSON` | body not JSON |
| 400 | `Not a Claude Code session ID: ID` | a session route with an ID that is not a UUID |
| 400 | `Invalid config.` | `/session` or `/title` whose `config` has an invalid `mainGroup`, `approve` or `expireHours` |
| 403 | `The SessionStart hook of this process marked its run unattended.` | `/session` from a process marked unattended in the last 10 s |
| 409 | `Unknown session; restart the Claude Code session.` | `/attach`, `/call`, `/permission` or `/turn` for an ID without a folder |
| 413 | `Body too large` | body over 1 MiB |
| 503 | `The SimpleX service is stopping.` | a session route while the service stops |
| 500 | the error message | any other error, a refused `permission` action included |

Session routes are `/session`, `/title`, `/attach`, `/call`, `/permission` and `/turn` ([`Service.entry`, `refuseWhenStopping`, `folder`](../src/service.ts)). Their ID is that of a session, or an alias of one ([`Service.canonical`, `resolve`, `attachedSession`, `processSession`, `alias`](../src/service.ts)):

```
attached(pid) = the entry whose session server sent that pid and is attached
on /session, /title or /turn with id, pid:
  alias of id                                    -> that session
  a folder with session.json for id:
    /title or /turn, attached(pid)               -> that session, without an alias
    else                                         -> id
  pid absent                                     -> id
  /session, session of pid unknown: wait up to 10 s for a /title with that pid
  session of pid = attached(pid)
                ?? an entry whose session server sent that pid, seen in the last 10 s
                ?? the session of the /title with that pid in the last 10 s
  session of pid ?? id; a session other than id records id as an alias, in aliases of its session.json
on /attach, /call or /permission with id:        alias of id ?? id
on /title with id, pid: no alias, no session.json for id, session of pid unknown or a held title of pid:
  held; the title of pid = id, its name; the reply at once
  the title resolves as above once a /session with that pid arrives, or after 10 s, while the service runs
on /session that creates the session of a held title: the title's name
a new session.json:                              the aliases that already point to its ID
on a service stop:                               the held title requests, with their at, into ROOT/titles.json
on the next start:                               each one taken as a new /title
```

`/session` and `/title` without `unattended` apply `config` to the hub ([`Hub.configure`](../src/hub.ts), [`Service.onSession`, `onTitle`, `applyTitle`](../src/service.ts)):
- The hub sets the expiry of all sessions from `expireHours`.
- The hub renames the Main group when `mainGroup` differs.

`/session` requests the session's mode ([`Session.requestMode`, `askMode`, `setMode`, `declineHost`, `Hub.onEvent`](../src/hub.ts), the [`Session`](../src/hub.ts) constructor):
- `requests` applies at once. The hub ends a pending mode card and clears `hostDeclined`.
- For `host`, the hub posts the mode card when the group has an approver and `hostDeclined` is unset. It posts the card at once, or after the next membership event of the group.
- When the mode card is rejected, the hub sets `hostDeclined` in session.json.
- The hub keeps the mode of an approved card, and a later `requests`, as `mode` in the session group's custom data. The session reads it when it opens.

`/title` opens or creates the session, and renames it when the name is not blank. It passes `model` to `turn` with the request's `at`, the start time of the SessionStart hook. So the model of a hook that started later, during the wait of a held title, stays. For a held title, `/title` replies at once. The service then opens or creates the session, renames it and passes `model` when the server of that process registers, or after 10 s, while the service runs ([`Service.onTitle`, `applyTitle`, `processEvent`](../src/service.ts)). A `/title` without `at`, from an older plugin, counts as the current time.

### 4.7 Plugin commands

<a id="plugin-commands"></a>

| Command | Run by | Output |
|---|---|---|
| `simplex-mcp session` | `.mcp.json` | MCP over stdio |
| `simplex-mcp service` | `spawnService`, through the launcher `node -e LAUNCHER COMMAND ARGS`, which spawns the service with fd 1, 2 and 3 of its own and exits at once, so the service is no descendant of the hook or session process. The launcher writes `error: TEXT` when the spawn fails ([`launcher`, `spawnService`](../src/client.ts)) | the ready line `ready`, `running` or `error: TEXT` on fd `SIMPLEX_MCP_READY_FD`, or on stdout when it is unset. The line is dropped when its reader is gone, and the service keeps running ([`readyChannel`, `writeReady`](../src/service.ts)). If another running process holds ROOT/service.lock, the service checks the lock and the socket up to 150 times, 100 ms apart. It starts again once the lock is released, and its ready line is `running` once the socket answers. The log is written to stdout and stderr |
| `simplex-mcp hook session-start` | SessionStart hook | one JSON line, see [4.8](#48-plugin-options-files-and-hook-output) |
| `simplex-mcp hook turn` | UserPromptSubmit, Stop and PostModelSwitch hooks | `{}`. A prompt or an answer over 250,000 characters is cut there and ends with `…` ([`maxText`, `cut`](../src/hook.ts)). The Stop hook sends the model of its answer, read from the transcript ([`turn`, `answerModel`, `findInTranscript`, `readTail`, `lastAnswer`](../src/hook.ts)). The UserPromptSubmit hook sends `typed`, `note` and `midTurn`, read from the prompt's record in the transcript ([`turn`, `promptKind`](../src/hook.ts)) |
| `simplex-mcp link` | people | with owners, `Owners of "MAIN": A, B. To replace them, run npx -y @simplex-chat/mcp@VERSION reset-owners in a terminal.` When the service reports neither owners nor a link, `The Main group has no link. Run npx -y @simplex-chat/mcp@VERSION reset-owners in a terminal.` Otherwise, on a terminal, `Join "MAIN" in the SimpleX app. The link works once:`, a QR code and the link, and elsewhere `Run npx -y @simplex-chat/mcp@VERSION link in a terminal to see the link to "MAIN".` |
| `simplex-mcp reset-owners` | people | after `POST /reset-owners` of [4.6](#46-socket-protocol), on a terminal, `"MAIN" was replaced. Join the new one in the SimpleX app. The link works once:`, a QR code and the link, and elsewhere `"MAIN" was replaced. Run npx -y @simplex-chat/mcp@VERSION link in a terminal to see the link to the new one.` |

VERSION is the version of the package that prints the text ([`linkCommand`, `resetCommand`](../src/owner.ts)).

Environment:
- `SIMPLEX_MCP_HOME`: ROOT.
- `CLAUDE_CODE_SESSION_ID` and `CLAUDE_PROJECT_DIR`: the session.
- `CLAUDE_PID`: the Claude Code process of a hook.
- `CLAUDE_CODE_ENTRYPOINT` and `CLAUDE_CODE_SESSION_ATTENDED`: a run that is not interactive, as in [4.1](#41-tools-in-the-plugin). The session process and the hooks read both.
- `CLAUDE_PLUGIN_OPTION_MAIN_GROUP`, `CLAUDE_PLUGIN_OPTION_APPROVE` and `CLAUDE_PLUGIN_OPTION_EXPIRE_HOURS`: the options.
- `SIMPLEX_MCP_MACHINE`: the machine part of the Main group's name, see [4.8](#48-plugin-options-files-and-hook-output).
- `SIMPLEX_MCP_READY_FD`: the fd of the service's ready line.

The service is spawned with the variables of the spawning process whose names match, case-insensitively, `PATH`, `PATHEXT`, `HOME`, `USERPROFILE`, `HOMEDRIVE`, `HOMEPATH`, `TMPDIR`, `TMP`, `TEMP`, `XDG_CACHE_HOME`, `LOCALAPPDATA`, `APPDATA`, `SYSTEMROOT`, `SYSTEMDRIVE`, `WINDIR`, `COMSPEC`, `LANG`, `LC_*`, `LD_LIBRARY_PATH`, `DYLD_LIBRARY_PATH`, `HTTP_PROXY`, `HTTPS_PROXY`, `NO_PROXY`, `NODE_USE_ENV_PROXY`, `NODE_EXTRA_CA_CERTS`, `NODE_OPTIONS`, `SSL_CERT_FILE`, `SSL_CERT_DIR`, `SIMPLEX_*` and `CLAUDE_PLUGIN_OPTION_*` ([`serviceVariables`, `spawnService`](../src/client.ts)).

| Code | Message | Code location |
|---|---|---|
| 0 | the hooks, even after errors, `link` and `reset-owners`, and the service after a stop or with `running` | [`sessionStart`, `turn`](../src/hook.ts), [`runService`](../src/service.ts) |
| 2 | `simplex-mcp session runs from the Claude Code plugin.` | [`startSession`](../src/session.ts) |
| 2 | `Not a Claude Code session ID: ID` | [`startSession`](../src/session.ts) |
| 1 | `Restart Claude Code to update the SimpleX plugin.`, or a spawn error, from `link` and `reset-owners` | [`ServiceClient.ensureService`, `spawnService`, `firstLine`](../src/client.ts) |
| 1 | a failed service start, with the ready line `error: TEXT`. The line is `error: Another service uses ROOT (PID N). If no service runs, remove ROOT/service.lock.` when the lock's holder neither releases it nor answers in 150 checks | [`runService`, `lockHolder`](../src/service.ts), [`lockedError`](../src/files.ts) |
| 1 | `stop: ERROR` for a failed stop after a signal | [`runService`](../src/service.ts) |

The service's stdout and stderr are written to `service.log` ([`spawnService`](../src/client.ts)). The log is emptied when it is over 10 MB, checked at each spawn and every hour while the service runs ([`trimLog`, `openLog`](../src/files.ts), [`Service.start`](../src/service.ts)). stdout lines:

| Moment | Line | Code |
|---|---|---|
| start | `SimpleX MCP service VERSION, data folder ROOT` | [`runService`](../src/service.ts) |
| Main group created | `Main group "NAME" created.` | [`createMainGroup`](../src/main-group.ts) |
| link opened | `Main group link created. Whoever joins by it first becomes an owner of "NAME".` | [`MainGroup.openLink`](../src/main-group.ts) |
| no owners, members present | `No owners in the Main group.` | [`noOwners`, `MainGroup.start`, `handleEvent`](../src/main-group.ts) |
| owner joined | `NAME joined (member ID) and is now an owner. The link is deleted.` | [`MainGroup.ownerJoined`](../src/main-group.ts) |
| Main group lost | `The Main group is lost. Run npx -y @simplex-chat/mcp@VERSION reset-owners in a terminal.` | [`MainGroup.lose`](../src/main-group.ts) |
| session group created | `Group "N · NAME" of session N created.` | [`Hub.createSessionGroup`](../src/hub.ts) |
| session group lost | `The group "N · NAME" is lost.` | [`Control.lose`](../src/control.ts) |
| actions | as in [3.4](#34-terminal-lines), with ` of session N` after `Action N`, and `allowed.` for a tool use | [`Actions.create`, `performedText`](../src/actions.ts) |
| stop | `Cancelled K action(s).`, `Stopping: sending K cancelled card(s), up to 10 s. Ctrl-C to stop now.`, `Stopped.` | [`Service.shutdown`](../src/service.ts), [`waitForCancelledCards`](../src/serve.ts) |

stderr: `CONTEXT: ERROR` from `logError`, with CONTEXT among `Main group command`, `EVENT in the Main group`, `EVENT in the group of session N`, `prompt of session N`, `invitation to session N`, `User`, `User in session N`, `group of session N`, `session N`, `owner of CHAT`, `mode host of session N`, `title`, `ROOT/_sessions/ID/session.json`, `ROOT/titles.json`, `ROOT/service.log`, `METHOD PATH` and those of [3.4](#34-terminal-lines). Session processes and hooks write to their own stderr ([`startSession`, `ServiceLink.start`](../src/session.ts), [`markUnattended`, `turn`](../src/hook.ts)):
- A session process writes `SimpleX service: ERROR`, `SimpleX channel: ERROR` and `permission relay: ERROR`.
- The SessionStart hook of an unattended run writes `SimpleX MCP session start: ERROR`, and the turn hook `SimpleX MCP turn: ERROR`.

### 4.8 Plugin options, files and hook output

<a id="plugin-options"></a>`userConfig` of [`.claude-plugin/plugin.json`](../.claude-plugin/plugin.json), read by [`readConfig`](../src/config.ts):

| Key | Variable | Type | Default | Valid | Error |
|---|---|---|---|---|---|
| `main_group` | `CLAUDE_PLUGIN_OPTION_MAIN_GROUP` | string | `Claude Control` | equal to `validName(value)` | `a valid SimpleX name, such as VALID expected` or `a valid SimpleX name expected` |
| `approve` | `CLAUDE_PLUGIN_OPTION_APPROVE` | string | `requests` | `requests`, `host` | `requests or host expected` |
| `expire_hours` | `CLAUDE_PLUGIN_OPTION_EXPIRE_HOURS` | number | 23 | digits only, 1 to 23 | `hours from 1 to 23 expected` |

The default is used for a missing or blank variable ([`readConfig`, `mainGroup`, `approve`, `expireHours`](../src/config.ts)).

`SIMPLEX_MCP_MACHINE`, set in the environment of a machine or container and not blank, adds ` - MACHINE` to the name of `main_group` ([`readConfig`, `withMachine`](../src/config.ts)). Its errors:
- MACHINE not equal to `validName(MACHINE)`: `SIMPLEX_MCP_MACHINE: a valid SimpleX name, such as VALID expected, got MACHINE. Change it in the environment of this machine.`, or `a valid SimpleX name expected` when VALID is empty.
- the full name not equal to its `validName`, over 50 characters or with an invalid joint: `Plugin option main_group with SIMPLEX_MCP_MACHINE: a valid SimpleX name of at most 50 characters expected, got NAME. Change one of them.` The hooks receive the variables from Claude Code. The MCP server gets them from `.mcp.json`, where `${user_config.KEY}` is mapped to the same names.

| File | Content |
|---|---|
| [`.claude-plugin/plugin.json`](../.claude-plugin/plugin.json) | name `simplex`, version `0.3.0`, equal to `package.json`, description, author, homepage, repository, license, keywords, `userConfig` |
| [`.mcp.json`](../.mcp.json) | server `simplex`: `node ${CLAUDE_PLUGIN_ROOT}/dist/cli.js session` with the three option variables, and `timeout` 169800000 ms |
| [`hooks/hooks.json`](../hooks/hooks.json) | The SessionStart hook runs `node "${CLAUDE_PLUGIN_ROOT}/dist/cli.js" hook session-start`, with timeout 600 s. The command of PreToolUse, with matcher `mcp__plugin_simplex_simplex__.*`, is `node "${CLAUDE_PLUGIN_ROOT}/plugin/allow.js"`. UserPromptSubmit, Stop and PostModelSwitch use `node "${CLAUDE_PLUGIN_ROOT}/dist/cli.js" hook turn`, with `async: true` |
| [`package.json`](../package.json) | `files`: the build, `README.md` and the plugin files, without a lockfile. With `bundleDependencies: true`, the pack bundles the production packages of `node_modules`, the dependencies and their dependencies, and no development package. `prepack` deletes `dist` and builds it again. Claude Code installs a plugin's dependencies only when `bun.lock`, `npm-shrinkwrap.json`, `package-lock.json` or `bun.lockb` is in its folder. So it installs nothing here, and the plugin loads its bundled modules |
| [`package-lock.json`](../package-lock.json) | the tree that `npm ci` installs for development and in `build.yml` before the publication. Its production packages are marked `inBundle`. It is not packed |
| [`plugin/allow.js`](../plugin/allow.js) | prints `{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"allow"}}` |
| [`/.claude-plugin/marketplace.json`](../../../.claude-plugin/marketplace.json) | marketplace `simplex-chat`, plugin `simplex`, source `{"source": "npm", "package": "@simplex-chat/mcp"}` |

Hook input fields used ([`SessionStartInput`, `TurnInput`, `Origin`, `TranscriptEntry`](../src/hook.ts)):
- SessionStart: `session_id`, `session_title`, `cwd` and `model`.
- The other hooks: `session_id`, `hook_event_name`, `transcript_path`, `prompt_id`, `prompt`, `last_assistant_message`, `to_model` and `effort.level`.

The Stop hook reads `type`, `isSidechain`, `message.model` and the `text` parts of `message.content` from the last entries of the transcript ([`lastAnswer`, `transcriptEntry`, `textOf`](../src/hook.ts)). It reads the transcript up to 20 times, 100 ms apart, until Claude's last entry is the answer. A missing transcript is read again, as a transcript without that entry is ([`answerReads`, `answerModel`, `findInTranscript`](../src/hook.ts)).

The UserPromptSubmit hook finds the prompt's record among the last entries of the transcript. A record is a user entry, without subagent entries and tool results, or a `queued_command` attachment, the record of a prompt that Claude Code added to a running turn. The record is the newest one whose text equals the prompt: the entry whose `promptId` equals the hook's `prompt_id`, or a record after the first entry with that ID. Claude Code gives the hook of a prompt added to a running turn that turn's prompt ID, and the turn's tool results carry it too, so the first entry with that ID is found also when the turn's first entry is out of the tail. Until an entry with `prompt_id` is in the tail, the hook waits for it, and at the last read it takes any record with the prompt's text. While a `queue-operation` `enqueue` entry with the prompt's text follows the record, the hook waits too: the prompt is a later one with the same text, still queued. Without `prompt_id`, as in older Claude Code, it takes any record at once. `typed` is true when the record's `origin.kind` is `human`, and `midTurn` is true when the record is not the entry with `prompt_id`. Claude Code writes the entry only after the synchronous UserPromptSubmit hooks end, and it creates the transcript file with the session's first entry. So the hook reads the transcript up to 600 times, 100 ms apart. A missing transcript is read again, as a transcript without the entry is. Claude Code runs the hook of a person's prompt when it is submitted, and of its own prompts only when they are taken. So at the last read, the hook classifies a record still followed by such an `enqueue` entry by the record, and marks a typed one mid-turn: the queued prompt is the typed prompt itself, or a later prompt with the text of Claude Code's own prompt. When no record is found within these reads, an `enqueue` entry with the prompt's text after the entry with `prompt_id`, or anywhere without that entry, makes the prompt typed and mid-turn. Otherwise `typed` is true when an entry with `prompt_id` has the origin `human` ([`promptReads`, `transcriptReadMs`, `promptKind`, `findInTranscript`, `promptRecord`, `isPrompt`, `recordText`, `recordKind`](../src/hook.ts)).

The hook's `note` is a line of at most 200 characters; a longer one is cut to 199 and ends with `…`. A typed prompt, a channel prompt, a teammate's prompt of idle notifications only and another record without origin that is not scheduled have no note ([`recordKind`, `noteOf`, `taskNote`, `teammateNote`, `idleNotification`, `peerName`, `firstLine`, `decoded`, `stringOf`, `shortened`](../src/hook.ts)):

| Kind | Record | Line |
|---|---|---|
| task notification | origin `task-notification`, or a `queued_command` attachment without origin whose `commandMode` is `task-notification` | an emoji, the first line of `<summary>` or `Task notification`, and `: ` with the first line of `<event>` when there is one. Entities are decoded. The emoji is ✅ for `<status>` completed, ❌ for failed, ⏹️ for stopped or killed; without a status, 📡 with an event and 🔔 without. A prompt without a `<task-notification>` block gets 🔔 and its first line |
| message of another session | origin `peer` | `✉️ Message from NAME: ` and the first line of `body`. NAME is `name`, or `from` |
| message of a teammate | no origin; the prompt starts with `Another Claude session sent a message` and a `<teammate-message teammate_id="NAME" ...>` tag | `✉️ Message from NAME: ` and the tag's `summary`, or the first line of the message without one, from the first tag whose message is not an idle notification, a JSON `type` of `idle_notification`. A prompt with idle notifications only has no note |
| subagent report | origin `peer` with `handback` | `📋 Report from subagent NAME: ` and the first line of `handbackReportText`, or of `body` |
| scheduled prompt | no origin, `turnOrigin` `scheduled` | `⏰ Scheduled prompt: ` and the first line of the prompt |
| automatic prompt | origin `auto-continuation` | `🔄 Automatic prompt: ` and the first line of the prompt |
| another kind | another origin | `⚙️ Claude Code: ` and the first line of the prompt |

A first line is the first line with other than blank characters. In a first line and in NAME, control and format characters become spaces, so a note is one line.

SessionStart output ([`sessionStart`, `markUnattended`](../src/hook.ts)):

| Case | Output |
|---|---|
| a run that is not interactive ([4.1](#41-tools-in-the-plugin)) | `{}`, after `/title` with `unattended` when `CLAUDE_CODE_SESSION_ATTENDED` is `0` and the entrypoint is an interactive one |
| Node.js below 22 | `{"systemMessage": "SimpleX MCP needs Node.js 22 or later; found V."}` |
| owners present, or no link | `{}` |
| no owner, a link | `systemMessage`: `SimpleX MCP: scan this QR code with the SimpleX app and tap Join to supervise your agents in "MAIN":`, the `plain` QR code and the link |
| service not running after the start | `{"systemMessage": "SimpleX MCP: The SimpleX service stopped; it starts again with the next tool call."}` |
| other errors, invalid options included | `{"systemMessage": "SimpleX MCP: TEXT"}` |
