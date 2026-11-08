# SimpleX MCP server: API

> The six MCP tools, the agent control chat commands, replies and cards, and the CLI of `@simplex-chat/mcp` 0.3.0; the Claude Code plugin's tools, groups, channel, socket protocol, commands and files in section 4.
>
> Related specs: [README](README.md) | [Architecture](architecture.md) | [Flows](flows.md)

**Source:** [`tools.ts`](../src/tools.ts) | [`control.ts`](../src/control.ts) | [`cli.ts`](../src/cli.ts) | [`messages.ts`](../src/messages.ts) | [`actions.ts`](../src/actions.ts) | [`session.ts`](../src/session.ts) | [`daemon.ts`](../src/daemon.ts) | [`hub.ts`](../src/hub.ts) | [`main-group.ts`](../src/main-group.ts) | [`hook.ts`](../src/hook.ts) | [`owner.ts`](../src/owner.ts) | [`protocol.ts`](../src/protocol.ts)

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

| Rule | Code |
|---|---|
| Tools are registered on each per-request `McpServer` with description, input schema, output schema and, in mode `host`, `_meta: {"anthropic/requiresUserInteraction": true}` for `connect`, `send`, `download` and `leave` | [tools.ts:488-495](../src/tools.ts#L488-L495), [:501-511](../src/tools.ts#L501-L511), [:543-545](../src/tools.ts#L543-L545) |
| Tool descriptions and server instructions; the instructions name the agent's display name | [tools.ts:415-453](../src/tools.ts#L415-L453), [serve.ts:108](../src/serve.ts#L108) |
| Arguments are validated by the SDK; a failure is a result with `isError: true` and the text `Input validation error: Invalid arguments for tool NAME: ERRORS` | [mcp-BumlNux0.cjs:1812-1817](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L1812-L1817) |
| `additionalProperties: false` is set in every input schema | [tools.ts:261](../src/tools.ts#L261), [:278](../src/tools.ts#L278), [:289](../src/tools.ts#L289), [:292](../src/tools.ts#L292), [:300](../src/tools.ts#L300), [:311](../src/tools.ts#L311) |
| A result is sent as `structuredContent` and as its JSON in one text item | [tools.ts:550](../src/tools.ts#L550) |
| A tool error is sent as `isError: true` with one text item: the `ToolError` text, or the `errorText` of other errors | [tools.ts:552](../src/tools.ts#L552), [log.ts:45-49](../src/log.ts#L45-L49) |
| An aborted call ends with the result `Cancelled.`, `isError: true`, which the SDK discards | [tools.ts:497](../src/tools.ts#L497), [:509](../src/tools.ts#L509) |
| With a `progressToken` in `_meta`, `notifications/progress` is sent at the start of each wait and every 60 s, with `progress` 0, 1, 2, ... and a `message` | [tools.ts:1721-1731](../src/tools.ts#L1721-L1731), [messages.ts:305-309](../src/messages.ts#L305-L309) |
| The call is aborted when the client closes the response stream or sends `notifications/cancelled`; the action is kept | [src-BhnGYybN.cjs:6338-6341](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs#L6338-L6341), [:6425](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs#L6425) |
| A call with the same tool and action fields as an action in memory is attached to it; the key is the JSON of the session number, `undefined` for `serve`, the tool and the fields | [tools.ts:1713-1715](../src/tools.ts#L1713-L1715), [:756-758](../src/tools.ts#L756-L758), [actions.ts:183-185](../src/actions.ts#L183-L185) |
| Messages are marked read after the last wait of a call that is not aborted | [tools.ts:802](../src/tools.ts#L802), [:949](../src/tools.ts#L949), [:1278](../src/tools.ts#L1278) |

Action fields of the key:

| Tool | Fields |
|---|---|
| `connect` | `target`<br>`incognito` (default `false`)<br>`message` (default `null`) |
| `send` | `chat`<br>`text` (default `null`)<br>`file` (default `null`)<br>`as` (default `null`)<br>`reply_to` (default `null`)<br>`mention` (default `null`) |
| `download` | `chat`<br>`item` |
| `leave` | `chat` |

Result status of the four action tools ([`actionResult`](../src/tools.ts#L771-L783)):

| Status | Meaning | Extra fields |
|---|---|---|
| `done` | performed | per tool |
| `failed` | failure of the card post, the counter write or the perform, or loss of the control chat | `error` |
| `rejected` | an approver sent `/reject` | |
| `expired` | no decision before the expiry time | |
| `known` | `connect` only: the target is an existing chat past the prepared state; no action | chat fields |
| `performing` | `download` only: `wait` passed after the approval; the download continues | |

### 1.2 `connect`

Handler: [`AgentTools.connect`](../src/tools.ts#L742-L765). Perform: [`performConnect`](../src/tools.ts#L821-L854). Schemas: [input](../src/tools.ts#L251-L262), [output](../src/tools.ts#L317-L327).

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
| `known` | `status`, chat fields of the known chat; in the plugin, a known chat without an owner becomes the session's, and a known chat of another session is the tool error `The chat of TARGET belongs to another session; an owner can give it to this session with /give.` | [tools.ts:747](../src/tools.ts#L747), [:760-765](../src/tools.ts#L760-L765) |
| `done`, chat unknown | `action`, `status`, `type`, `connected: false`, plan fields `name`, `peer_type`, `description`, `welcome`, `commands`, `known_as` when present | [tools.ts:768](../src/tools.ts#L768), [:1486-1492](../src/tools.ts#L1486-L1492) |
| `done`, chat known | `action`, `status`, chat fields, `messages` | [tools.ts:785-805](../src/tools.ts#L785-L805) |
| `failed` | `action`, `status`, `error` | [tools.ts:776](../src/tools.ts#L776) |
| `rejected`, `expired` | `action`, `status` | [tools.ts:776](../src/tools.ts#L776) |

Chat fields ([`chatFields`](../src/tools.ts#L1342-L1369)):

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

`messages`: message items among the last 50 items of the chat when the result is built, replaced by their collected copies, followed by the items collected since the connection; live items are left out; new items are marked read ([tools.ts:787](../src/tools.ts#L787), [:797-804](../src/tools.ts#L797-L804)).

Tool errors:

| Text | Condition | Code |
|---|---|---|
| `Not a SimpleX link or name: TARGET` | whitespace in `target`, or `invalidConnReq` from `apiConnectPlan` | [tools.ts:809](../src/tools.ts#L809), [:815](../src/tools.ts#L815) |
| an app error text, for example `Name not found: This SimpleX name is not registered. Please check the name.`, or `SimpleX error: CHAIN: DETAILS` | other errors of `apiConnectPlan`, or a plan of type `error` | [tools.ts:816](../src/tools.ts#L816), [:1413](../src/tools.ts#L1413), [log.ts:51-53](../src/log.ts#L51-L53) |
| `TARGET can't be joined now: PLAN.` | `invitationLink` other than `ok` and `known`<br>`contactAddress` other than `ok`, `known`, `contactViaAddress` and `connectingConfirmReconnect`<br>`groupLink` other than `ok`, `known` and `connectingConfirmReconnect` | [tools.ts:1404](../src/tools.ts#L1404), [:1417](../src/tools.ts#L1417), [:1435](../src/tools.ts#L1435), [:1457](../src/tools.ts#L1457) |
| `message: TARGET is not a contact or business address with short link data, or its contact exists without a connection.` | `message` given and the plan has `takesMessage: false` | [tools.ts:749](../src/tools.ts#L749) |
| refusals | see [1.9](#19-action-refusals) | |

`takesMessage` is `true` for a contact address with short link data, for a known prepared contact, and for a known prepared business group ([tools.ts:1408](../src/tools.ts#L1408), [:1428](../src/tools.ts#L1428), [:1446](../src/tools.ts#L1446)).

### 1.3 `send`

Handler: [`AgentTools.send`](../src/tools.ts#L912-L937). Result: [`sendResult`](../src/tools.ts#L939-L957). Perform: [`performSend`](../src/tools.ts#L959-L983). Schemas: [input](../src/tools.ts#L264-L279), [output](../src/tools.ts#L329-L341).

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

File types by extension, lower-cased ([tools.ts:179-188](../src/tools.ts#L179-L188), [`attachmentMedia`](../src/tools.ts#L1671-L1682)):

| Extension | Message type | Sent file | Preview and duration |
|---|---|---|---|
| `.jpg`, `.jpeg`, `.png` | image | `core.resizeImage(..., Image)`, written to `DATA/files/sent/`; the original when resizing fails | `core.resizeImage(..., MessagePreview)`; the fixed preview when it fails |
| `.gif`, `.webp` | image | unchanged | the fixed preview |
| `.mp4`, `.mov` | video | unchanged | the first frame from `ffmpeg`, resized as a preview; duration from `ffprobe`, rounded; the fixed preview and 0 when they fail |
| other, or `as: "file"` | file | unchanged | |

The fixed preview is a 16 × 9 mid-gray PNG ([tools.ts:168](../src/tools.ts#L168)). `ffmpeg` and `ffprobe` run with `-protocol_whitelist file -f mov`, 30 s and 64 MB of output each ([tools.ts:1693-1711](../src/tools.ts#L1693-L1711)).

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
| `text: required without file.` | neither `text` nor `file` | [tools.ts:917](../src/tools.ts#L917) |
| `as: applies only with file.` | `as` without `file` | [tools.ts:918](../src/tools.ts#L918) |
| `No chat CHAT. list shows the agent's chats.` | invalid reference, the control chat, or a store error when the chat is read | [tools.ts:1306-1315](../src/tools.ts#L1306-L1315), [:1615-1617](../src/tools.ts#L1615-L1617) |
| `The agent is not connected to CHAT and can't post; list shows connected: false.` | the chat is not connected | [tools.ts:920](../src/tools.ts#L920) |
| `The agent has role ROLE in CHAT and can't post.` | membership role `observer` or `relay` | [tools.ts:921-924](../src/tools.ts#L921-L924) |
| `reply_to: CHAT has no item N.` | the read of `around=N` fails, or the item is absent from it | [tools.ts:1320](../src/tools.ts#L1320) |
| `reply_to: item N in CHAT is not a message.` | the item's content is not `rcvMsgContent` or `sndMsgContent` | [tools.ts:1322](../src/tools.ts#L1322) |
| `mention: CHAT is a contact chat; mentions apply to groups and channels.` | `mention` with `@N` | [tools.ts:1328](../src/tools.ts#L1328) |
| `mention: CHAT has no member N.` | ID absent from `apiListMembers` | [tools.ts:1333](../src/tools.ts#L1333) |
| `mention: members N and M have the same name.` | two mentioned members with the same display name | [tools.ts:1336](../src/tools.ts#L1336) |
| `file: an absolute path is expected.`<br>`file: no file at FILE.`<br>`file: PATH is not a file.`<br>`file: PATH can't be read.` | the path, after symbolic links | [tools.ts:1624-1635](../src/tools.ts#L1624-L1635) |
| `file: PATH is refused: private keys and the server's own files are not sent.` | a file named `id_*`, `*.pem`, `*.key`, `*.p12` or `*.pfx`, or under `~/.ssh`, `~/.gnupg`, or the data folder or `~/.simplex-mcp` outside `DATA/files/`; each folder by its real path | [tools.ts:999-1002](../src/tools.ts#L999-L1002), [:1008-1013](../src/tools.ts#L1008-L1013) |
| `file: PATH is empty.` | size 0 | [tools.ts:1003](../src/tools.ts#L1003) |
| `text: too long for the message with its quote; shorten it.`<br>`text: too long for a message with this TYPE; shorten it.` | the message, as the core sends it, above 15602 bytes | [tools.ts:928-930](../src/tools.ts#L928-L930), [:1592-1608](../src/tools.ts#L1592-L1608) |
| refusals | see [1.9](#19-action-refusals) | |

The message is measured as 600 bytes plus the JSON of its content, the file's name, the quote and the mentioned names. The quote is the quoted text, or its file's name when the text is empty; a sent image or video quotes it as text, otherwise an image's preview is kept ([tools.ts:1592-1608](../src/tools.ts#L1592-L1608)).

The perform fails with `file: PATH changed after the request.` when the file's real path, size, inode or change time differs from the request ([tools.ts:1015-1020](../src/tools.ts#L1015-L1020)). A resized copy is deleted at `sndFileCompleteXFTP`, at `sndFileError` and after a failed send ([tools.ts:980](../src/tools.ts#L980), [:985-995](../src/tools.ts#L985-L995), [:694-696](../src/tools.ts#L694-L696)).

### 1.4 `receive`

Handler: [`AgentTools.receive`](../src/tools.ts#L1249-L1284). Schemas: [input](../src/tools.ts#L281-L290), [output](../src/tools.ts#L343-L351).

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

Tool errors: `No chat CHAT. list shows the agent's chats.` for an invalid reference, the control chat, or a store error of the first read, `initial` 1 ([tools.ts:1251](../src/tools.ts#L1251), [:1255-1257](../src/tools.ts#L1255-L1257)); other chat errors, a store error of a later page read included, as their error text ([log.ts:45-53](../src/log.ts#L45-L53)).

Messages are read by `receive` without an action or an approval.

### 1.5 `download`

Handler: [`AgentTools.download`](../src/tools.ts#L1096-L1116). Result: [`downloadResult`](../src/tools.ts#L1143-L1151). Perform: [`performDownload`](../src/tools.ts#L1153-L1157), [`downloadFile`](../src/tools.ts#L593-L620). Schemas: [input](../src/tools.ts#L303-L312), [output](../src/tools.ts#L398-L413).

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

FOLDER is `group-N` for `#N` and `contact-N` for `@N`; the extension is the file name's extension when it has 1 to 16 ASCII letters and digits ([messages.ts:215-227](../src/messages.ts#L215-L227)). `/forward` downloads a file to `DATA/forwarded/FOLDER/` under the same name; a later perform of the download moves it to the download path, after the copy for the post is made ([tools.ts:593-610](../src/tools.ts#L593-L610)).

Steps before the action:

| Step | Code |
|---|---|
| a file in status `rcvInvitation`, or an XFTP file: the file's servers from `apiGetChatItemInfo`; while none, the call waits for `rcvFileDescrReady` until `wait` seconds after the call, with the progress `The sender is uploading the file of item N in CHAT; waiting until TIME.` | [tools.ts:1118-1135](../src/tools.ts#L1118-L1135) |
| unknown servers: the file's servers that are not among the XFTP servers of `apiGetUserServers`, of no operator or of an enabled one, compared without their passwords | [tools.ts:1137-1141](../src/tools.ts#L1137-L1141) |
| without unknown servers, and for a file ID not in rejected files, the chat's files approval, read again, makes the action performed at once | [tools.ts:1111](../src/tools.ts#L1111) |

Perform: `apiReceiveFile(fileId, path, approvedRelays)`, for a file in status `rcvInvitation`. `approvedRelays` is true when the action's mode is `requests` and its card named unknown servers, and false otherwise; the core then checks the complete description ([tools.ts:616](../src/tools.ts#L616), [:1159-1168](../src/tools.ts#L1159-L1168)). The download ends at `rcvFileComplete`, `rcvFileError`, `rcvFileSndCancelled` or `chatItemUpdated` of its file, or at a check every 60 s ([tools.ts:632-654](../src/tools.ts#L632-L654), [:1170-1238](../src/tools.ts#L1170-L1238)).

Failure texts of the perform:

| Text | Condition | Code |
|---|---|---|
| the file's error, as in the file field | the file failed | [tools.ts:1199-1209](../src/tools.ts#L1199-L1209) |
| `The sender cancelled the file.` | `rcvFileSndCancelled` | [tools.ts:649-654](../src/tools.ts#L649-L654) |
| `The message or its chat was deleted.` | a check finds no message, contact or group | [tools.ts:173](../src/tools.ts#L173), [:1189-1197](../src/tools.ts#L1189-L1197) |
| `The download made no progress for 30 minutes and was cancelled.` | no rise of `rcvProgress` for 30 minutes; the file is cancelled | [tools.ts:177](../src/tools.ts#L177), [:1182-1186](../src/tools.ts#L1182-L1186) |
| `The received file is not in the agent's files folder.` | the file is received and neither the download path nor the forwarded path has a file | [messages.ts:14](../src/messages.ts#L14), [tools.ts:1211-1215](../src/tools.ts#L1211-L1215) |
| `Your owner deleted the files of this chat.` | `/delete CHAT files` | [tools.ts:174](../src/tools.ts#L174), [:1046-1064](../src/tools.ts#L1046-L1064) |
| `The file is on unknown servers that were not approved.` | a check reads an accepted file in status `rcvInvitation`: the core returned it to the offer when its description completed | [tools.ts:175](../src/tools.ts#L175), [:1178](../src/tools.ts#L1178) |
| `Unknown servers! Without Tor or VPN, your IP address will be visible to these XFTP relays: HOST, HOST.` | the core refuses the servers of the complete description at the acceptance | [log.ts:78-79](../src/log.ts#L78-L79) |

Tool errors:

| Text | Condition | Code |
|---|---|---|
| `No chat CHAT. list shows the agent's chats.` | invalid reference, the control chat, or a store error when the chat is read | [tools.ts:1101](../src/tools.ts#L1101) |
| `item: CHAT has no item N.` | the item is not found | [tools.ts:1103](../src/tools.ts#L1103) |
| `item: item N in CHAT has no received file.` | the item is not received or has no file | [tools.ts:1105](../src/tools.ts#L1105) |
| `item: the file of item N in CHAT failed: ERROR` | the file field has status `failed` | [tools.ts:1106-1107](../src/tools.ts#L1106-L1107) |
| `item: the sender has not uploaded the file of item N in CHAT yet; call download again later.` | no servers by the end of `wait`, also for a file invited without a description, whose servers the core does not list; no action is created | [tools.ts:1109](../src/tools.ts#L1109), [:1133](../src/tools.ts#L1133) |
| refusals | see [1.9](#19-action-refusals) | |

A file ID whose download returned `rejected` or `expired` shows status `rejected` until a download of it is done or the server restarts ([tools.ts:1148-1149](../src/tools.ts#L1148-L1149)).

### 1.6 `list`

Handler: [`AgentTools.listChats`](../src/tools.ts#L1286-L1294). Schemas: [input](../src/tools.ts#L292), [output](../src/tools.ts#L353-L385).

Input: an empty object.

Output:

| Field | Item fields |
|---|---|
| `chats` | `chat`<br>`type`<br>`name`<br>`simplex_name`, when verified<br>`peer_type`, when present<br>`incognito`<br>`known_as`, when incognito<br>`connected`<br>`unread`: `chatStats.unreadCount`<br>`approved: true`, only under a standing approval of messages in mode `requests` |
| `actions` | `action`<br>`tool`<br>`input`: the input of the creating call<br>`state`<br>`open`: `true` while a call waits on it |

`chats`: the contacts and groups of the main conversations from one read, `apiGetChats(USER, last 1000000)`, except the control chat; in the plugin, only the chats of the calling session ([tools.ts:1296-1304](../src/tools.ts#L1296-L1304), [:583-585](../src/tools.ts#L583-L585)). `actions`: the actions in memory of the calling session without its tool-use and mode cards, all actions for `serve` ([tools.ts:1292](../src/tools.ts#L1292)).

### 1.7 `leave`

Handler: [`AgentTools.leave`](../src/tools.ts#L1072-L1080). Perform: [`leaveChat`](../src/tools.ts#L1086-L1094). Schemas: [input](../src/tools.ts#L294-L301), [output](../src/tools.ts#L387-L396).

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

Tool errors: `No chat CHAT. list shows the agent's chats.` when no action with the same key is in memory and the chat is invalid, the control chat or unknown ([tools.ts:1075-1077](../src/tools.ts#L1075-L1077)); refusals.

### 1.8 Message, file and command objects

Message ([`Message`](../src/messages.ts#L16-L28), [`toMessage`](../src/messages.ts#L174-L192), [schema](../src/tools.ts#L190-L223)):

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
| `reply_to` | object | quoted item with an ID | `{chat, item: quotedItem.itemId}` |
| `mentions_me` | boolean | always | `meta.userMention` |

File ([`messageFile`](../src/messages.ts#L194-L208)):

| Field | Value |
|---|---|
| `name` | `fileName` |
| `size` | `fileSize`, bytes |
| `type` | `image`, `video` or `voice` from the message type; `file` otherwise |
| `status` | first match:<br>`rejected`: the file ID is in rejected files<br>`failed`: `fileProhibited` is set, the status is `rcvCancelled`, `rcvAborted`, `rcvError` or `invalid`, or the status is `rcvComplete` and neither the download path nor the forwarded path has a file<br>`received`: `rcvComplete`, a file at the download path, and `fileSource` at the download path or the forwarded path<br>`offered`: otherwise, a file that `/forward` downloaded included |
| `path` | `received`: the download path |
| `error` | `failed`: the error text ([log.ts:197-244](../src/log.ts#L197-L244)) |

Only received message items (`rcvMsgContent`) are returned by `send` and `receive`; `connect` also returns sent ones (`sndMsgContent`) ([messages.ts:101](../src/messages.ts#L101), [tools.ts:797](../src/tools.ts#L797)).

Command ([`commandsOf`](../src/tools.ts#L1574-L1581), [schema](../src/tools.ts#L225-L235)):

| Form | Fields |
|---|---|
| command | `keyword`<br>`label`<br>`params`, when present |
| menu | `label`<br>`commands`: commands and menus |

### 1.9 Action refusals

Refusals of a new `connect`, `send`, `download` or `leave` action of `serve`, first match ([actions.ts:207-212](../src/actions.ts#L207-L212), [control.ts:132-153](../src/control.ts#L132-L153)); the plugin's texts are in [4.4](#44-tool-results-in-the-plugin):

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

Largest card form ([control.ts:714-722](../src/control.ts#L714-L722)): the maximum of the creation form, the `done` form and a `failed` form with an error of 600 × `€`, with the action's own decider or the longest current approver name; each form is measured as JSON-escaped UTF-8: `"`, `\`, `\n`, `\r`, `\t` 2 bytes, other characters below U+0020 6 bytes.

Link texts of a `connect` card, and the input preview of a tool-use card in the plugin ([control.ts:135-136](../src/control.ts#L135-L136), [:736-750](../src/control.ts#L736-L750)): the description and welcome lines, or the preview lines, take the bytes that the largest form leaves without them. Lines are kept while they fit; the next line is cut to the bytes left and ends in `…`; later lines are left out. The tool result keeps both texts whole.

---

## 2. Agent control chat

### 2.1 Commands and replies

Accepted text ([`parseCommand`](../src/control.ts#L620-L625), [`ownerRequest`](../src/control.ts#L798-L812)):

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
```

The server reads commands from members' messages in the main conversation of the control chat ([serve.ts:129](../src/serve.ts#L129), [control.ts:204-215](../src/control.ts#L204-L215)). It answers a command with a reply that quotes it, one command after another, and it runs the expiry check before each command ([control.ts:213](../src/control.ts#L213), [:236](../src/control.ts#L236)).

| Request | Condition | Reply | Code |
|---|---|---|---|
| one-line `/` text outside the requests | any sender | `Commands: /approve, /reject, /receive, /forward, /revoke, /leave, /delete, /pending, /list. Each card shows its commands.`; in a plugin session group, with `/give` after `/delete` | [control.ts:17-18](../src/control.ts#L17-L18), [:238](../src/control.ts#L238) |
| any request | sender not an approver, `--owners` absent | `Only owners approve actions.` | [control.ts:239](../src/control.ts#L239) |
| any request | sender not an approver, `--owners` given | `Only the owners listed in --owners approve actions.` | [control.ts:239](../src/control.ts#L239) |
| `approve`, `reject`, `receive`, `forward` with N | action N absent, not `pending`, or without a card | `Action N is not pending.` | [control.ts:264](../src/control.ts#L264) |
| the same with CHAT | action N is a `connect`, or for another chat | `Action N is not for CHAT.` | [control.ts:265](../src/control.ts#L265) |
| `approve` | action N is a download | `Action N is a download: COMMANDS.`, COMMANDS the card's command line | [control.ts:271](../src/control.ts#L271) |
| `receive`, `forward`, `receive CHAT N all` | action N is not a download | `Action N is not a download.` | [control.ts:269](../src/control.ts#L269) |
| `approve`, `reject`, `receive`, `forward` | accepted | none; the card is edited at the outcome; `forward` posts the file, as a reply to the card when the message with the card as its quote is within 15602 bytes, and the card stays pending | [control.ts:266-275](../src/control.ts#L266-L275), [:315-331](../src/control.ts#L315-L331) |
| `approve CHAT all` | mode `host`; nothing is stored | `Approvals apply in mode requests; this server runs in mode host.` | [control.ts:248](../src/control.ts#L248) |
| `approve CHAT all`, `receive CHAT N all` | CHAT unknown or the control chat | `No chat CHAT. /list shows the agent's chats.` | [control.ts:283](../src/control.ts#L283) |
| `approve CHAT all` | accepted; the pending sends of CHAT with a card are approved | `Approved for KIND NAME (CHAT) by APPROVER. /'revoke CHAT'` | [control.ts:280-291](../src/control.ts#L280-L291) |
| `receive CHAT N all` | accepted; download N is performed, and the other pending downloads of CHAT stay pending | `Files approved for KIND NAME (CHAT) by APPROVER. /'receive CHAT off'` | [control.ts:274](../src/control.ts#L274), [:288-290](../src/control.ts#L288-L290) |
| `revoke CHAT` | CHAT unknown, or without a standing approval | `Messages to CHAT have no standing approval.` | [control.ts:296](../src/control.ts#L296) |
| `revoke CHAT` | accepted | `Messages to KIND NAME (CHAT) need approval again.` | [control.ts:299](../src/control.ts#L299) |
| `receive CHAT off` | CHAT unknown, or without a files approval | `Files from CHAT have no standing approval.` | [control.ts:296](../src/control.ts#L296) |
| `receive CHAT off` | accepted | `Files from KIND NAME (CHAT) need approval again.` | [control.ts:299](../src/control.ts#L299) |
| `leave CHAT` | CHAT unknown or the control chat | `No chat CHAT. /list shows the agent's chats.` | [control.ts:335](../src/control.ts#L335) |
| `leave CHAT` | accepted, at once in both modes; the pending actions of CHAT with a card are rejected by the sender | `Left KIND NAME (CHAT), requested by NAME.`, or `Deleted ...` for a contact; logged too | [control.ts:333-347](../src/control.ts#L333-L347) |
| `delete CHAT files` | the chat's downloads in memory fail and are cancelled; its other files in status `rcvAccepted`, `rcvTransfer` or `rcvWarning`, such as downloads the core continues after a restart, are cancelled; the folder of CHAT and its forwarded folder are deleted, for a current or a left chat | `Deleted K files from CHAT, SIZE.` (`1 file`), or `No files from CHAT.` | [control.ts:354-357](../src/control.ts#L354-L357), [tools.ts:1046-1064](../src/tools.ts#L1046-L1064), [messages.ts:275-283](../src/messages.ts#L275-L283) |
| `pending` | no pending action with a card | `No pending actions.` | [control.ts:243](../src/control.ts#L243) |
| `pending` | otherwise | one line per action: `N  WHAT  expires YYYY-MM-DD HH:MM UTC` | [control.ts:242](../src/control.ts#L242), [:710-712](../src/control.ts#L710-L712) |
| `list` | no chats | `No chats.` | [control.ts:389](../src/control.ts#L389) |
| `list` | otherwise | one line per chat of the 100 most recent: `CHAT  KIND NAME`, then `, approved for this chat by APPROVER`, `, files approved by APPROVER` and `, not connected` when applicable, cut to 100 bytes with `…`; then `K older chats are not shown.` (`1 older chat is not shown.`) | [control.ts:377-390](../src/control.ts#L377-L390) |

KIND is `contact`, `bot`, `agent`, `business`, `group` or `channel` ([tools.ts:1511-1518](../src/tools.ts#L1511-L1518)). WHAT is `connect to SUBJECT`, `send to SUBJECT`, `download NAME from SUBJECT`, `delete SUBJECT` or `leave SUBJECT` ([actions.ts:388-404](../src/actions.ts#L388-L404)). SIZE is in bytes, KB, MB or GB, by 1000, with one decimal ([control.ts:762-765](../src/control.ts#L762-L765)). Names are shown by `safeName` ([control.ts:779-782](../src/control.ts#L779-L782)).

A standing approval is stored in the chat's custom data, `simplexMcp` for messages and `simplexMcpFiles` for files, as `{approvedBy, approvedById, approvedAt}`; it applies in mode `requests` while member `approvedById` is an approver ([control.ts:25-28](../src/control.ts#L25-L28), [:155-158](../src/control.ts#L155-L158)).

### 2.2 Cards

Card text: request lines, then one last line ([`cardText`](../src/control.ts#L632-L651), [`requestLines`](../src/control.ts#L653-L690)).

Request lines:

| Tool | First line | Further lines |
|---|---|---|
| `connect` | `I want to connect to SUBJECT, as myself.` or `, incognito.`<br>done and connected: `I connected to ...`<br>done and not connected: `I asked to connect to ...` | link description, one code span per line<br>welcome, one code span per line<br>both cut to fit the card ([1.9](#19-action-refusals))<br>with `message`: `With the message:` and its lines |
| `send` | `I want to send a message to SUBJECT:`, with `, where I am incognito` before the colon in incognito chats<br>done: `I sent a message to ...` | text, one code span per line<br>with `file`: ``attaching TYPE `PATH`, SIZE``<br>`replying to NAME: QUOTE` with `reply_to`, the quote cut to 100 characters<br>`mentioning @A, @'B C'` with `mention` |
| `download` | `I want to download a file from SENDER in KIND NAME (CHAT):`, without `SENDER in ` outside groups<br>done: `I downloaded a file from ...` | ``` `NAME`, SIZE ```<br>an XFTP file: `Servers: known.`, or ``Servers: `HOST`, `HOST`, unknown: downloading shows them this computer's IP address.`` (`it` for one) |
| `leave` | `I want to delete KIND NAME (@N).` or `I want to leave KIND NAME (#N).`<br>done: `I deleted ...` or `I left ...` | |

SUBJECT is `KIND NAME`, or `an unnamed KIND`, followed by ` (SIMPLEX_NAME)` when known ([actions.ts:383-386](../src/actions.ts#L383-L386)). For `connect`, SIMPLEX_NAME is the target when it is a name; for `send`, the chat's verified SimpleX name.

A card of a `send` with an image or video file, or of a `download` of an image or video, is an image or video message with the file's preview and the card text as its caption; the preview is resized to fit the card, and the card is text when it can't be resized ([control.ts:505-525](../src/control.ts#L505-L525)).

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

DECIDER is `Performed without approval (mode host)` in mode `host`, `Approved for this chat by NAME` for a standing action, and `Approved by NAME` otherwise ([control.ts:634](../src/control.ts#L634)).

Command lines ([`commandsLine`](../src/control.ts#L692-L708)):

| Tool | Command line |
|---|---|
| `connect` | `/'approve N' or /'reject N'` |
| `send` | `/'approve CHAT N', /'approve CHAT all' or /'reject CHAT N'` |
| `download` | `/'receive CHAT N', /'receive CHAT N all', /'forward CHAT N' or /'reject CHAT N'`, without `/'receive CHAT N all'` for unknown servers |
| `leave` | `/'approve CHAT N' or /'reject CHAT N'` |

The app shows `/'approve N'` as the tappable command `/approve N` ([PLAN, What the owner sees](../PLAN.md#what-the-owner-sees-the-agent-control-chat)).

An action performed without a card, in mode `host` or under a standing approval, gets its outcome as a new message; a `download` without a card gets none ([control.ts:178-185](../src/control.ts#L178-L185), [:560-562](../src/control.ts#L560-L562)).

### 2.3 Other posts

| Situation | Text | Code |
|---|---|---|
| `send` with a refused file; logged too | ``Refused to send `PATH` to KIND NAME (CHAT): private keys and the server's own files are not sent.`` | [control.ts:187-191](../src/control.ts#L187-L191) |
| `sndFileError` of a sent file; logged too | ``The file `NAME` was not sent to KIND NAME (CHAT): `REASON`.`` | [control.ts:193-202](../src/control.ts#L193-L202), [serve.ts:133](../src/serve.ts#L133) |
| `/forward` of a file that is not received or not posted | ``Action N: the file was not received: `ERROR`.`` | [control.ts:329](../src/control.ts#L329) |
| after the reply to `/leave CHAT`, and after the outcome of a done `leave`, when the folder of CHAT has files | ``Files from KIND NAME (CHAT) are kept in `FOLDER`: K files, SIZE. /'delete CHAT files'`` | [control.ts:183-184](../src/control.ts#L183-L184), [:344-352](../src/control.ts#L344-L352) |

### 2.4 Membership messages

Posted in the chat and logged; in the chat, names are shown by `safeName` and the `--owners ...` part is a code span; in the log, names are folded to one line ([control.ts:491-494](../src/control.ts#L491-L494), [:81-82](../src/control.ts#L81-L82)).

| Situation | Text | Code |
|---|---|---|
| owner joined, `--owners` absent | `I'm your agent. I ask here before I connect, send, download a file or leave a chat.` (chat only) | [control.ts:34](../src/control.ts#L34), [:439](../src/control.ts#L439) |
| member joined, `--owners` without a list | `NAME joined (member ID). Restart the server with --owners ENTRIES` | [control.ts:444-447](../src/control.ts#L444-L447) |
| start with `--owners` without a list and humans present | `A joined (member ID). B joined (member ID). Restart the server with --owners ENTRIES` | [control.ts:411-414](../src/control.ts#L411-L414) |
| a human became owner outside the list | `NAME (member ID) is now an owner and can't approve actions yet. Restart the server with --owners ENTRIES` | [control.ts:452-456](../src/control.ts#L452-L456) |
| a listed owner stopped being a human owner | `NAME (member ID) is no longer an owner. Restart the server with --owners ENTRIES`<br>none remains: `NAME (member ID) is no longer an owner. No owners remain: restart the server with --reset-owners --owners` | [control.ts:457-462](../src/control.ts#L457-L462) |

ENTRIES: `ID:NAME` of each human, or of each human owner, joined by commas; an entry with characters other than letters, digits, `_` and `-` is quoted for the shell ([control.ts:847-857](../src/control.ts#L847-L857)).

### 2.5 Group profile

| Field | Value | Code |
|---|---|---|
| display name | the agent's display name, updated at start when it differs | [control.ts:108-110](../src/control.ts#L108-L110) |
| full name | empty | [control.ts:576](../src/control.ts#L576) |
| description | `Supervise your AI agent here: approve or reject its connections and messages with one tap.` | [control.ts:577](../src/control.ts#L577) |
| history | on | [control.ts:578](../src/control.ts#L578) |
| command menu | `/pending` "Pending actions"<br>`/list` "Chats and approvals"<br>`/leave` "Leave a chat", params `<chat ID>`<br>updated at start when it differs | [control.ts:11-15](../src/control.ts#L11-L15), [:108-110](../src/control.ts#L108-L110) |

---

## 3. CLI

Binary: `simplex-mcp`, compiled `dist/cli.js` ([package.json:5-7](../package.json#L5-L7)). Help text: [cli.ts:14-53](../src/cli.ts#L14-L53).

```
simplex-mcp --help
simplex-mcp serve NAME [options]
simplex-mcp register NAME [options]
```

The plugin's commands are in [4.7](#47-plugin-commands).

NAME: 1 to 32 characters of `[a-z0-9]` and single hyphens, starting with a letter or digit ([cli.ts:169-171](../src/cli.ts#L169-L171)).

### 3.1 `serve`

| Option | Value | Default | Rule | Code |
|---|---|---|---|---|
| `--data` | folder | `~/.simplex-mcp/NAME` | resolved to an absolute path; `dataGiven` is set | [cli.ts:89-92](../src/cli.ts#L89-L92) |
| `--display-name` | text | NAME when the user is created | must equal `validName(text)` | [cli.ts:93-95](../src/cli.ts#L93-L95), [:173-177](../src/cli.ts#L173-L177) |
| `--approve` | `requests` or `host` | `requests` | | [cli.ts:96-98](../src/cli.ts#L96-L98) |
| `--owners` | absent value, or `ID:NAME,...` | absent | the value is absent when the next argument is missing or starts with `--`; it is split at each comma followed by `ID:`, so a name may hold commas; entries match `^([1-9]\d*):(.+)$` | [cli.ts:99-101](../src/cli.ts#L99-L101), [:194-198](../src/cli.ts#L194-L198) |
| `--expire` | hours | 23 | integer 1 to 23 | [cli.ts:102-104](../src/cli.ts#L102-L104) |
| `--stop-wait` | seconds | 10 | integer 0 to 60 | [cli.ts:105-107](../src/cli.ts#L105-L107) |
| `--reset-owners` | flag | off | refused with an `--owners` list | [cli.ts:108-110](../src/cli.ts#L108-L110), [:124-126](../src/cli.ts#L124-L126) |
| `--new-token` | flag | off | | [cli.ts:111-113](../src/cli.ts#L111-L113) |
| `--port` | number | 5226 | integer 1 to 65535 | [cli.ts:114-116](../src/cli.ts#L114-L116) |
| `--qr-code` | `small` or `large` | `small` | | [cli.ts:117-119](../src/cli.ts#L117-L119) |

### 3.2 `register`

| Option | Value | Default | Code |
|---|---|---|---|
| `--scope` | `user` or `local` | `user` | [cli.ts:134-136](../src/cli.ts#L134-L136) |
| `--data` | folder | `~/.simplex-mcp/NAME` | [cli.ts:137-139](../src/cli.ts#L137-L139) |

Entry ([`registryEntry`](../src/register.ts#L46-L53)):

| Field | Value |
|---|---|
| `type` | `http` |
| `url` | `http://127.0.0.1:PORT/mcp` |
| `headersHelper` | `cat DATA/headers.json`, Windows `type DATA\headers.json`, path shell-quoted |
| `timeout` | `(expireHours + 24) × 3600000 + 600000` ms |

Server name: `simplex-NAME` ([register.ts:36](../src/register.ts#L36)).

### 3.3 Exit codes and messages

| Code | Message | Code location |
|---|---|---|
| 0 | `--help` text on stdout; a stop after a signal | [cli.ts:221-223](../src/cli.ts#L221-L223), [serve.ts:53](../src/serve.ts#L53) |
| 2 | `Usage: simplex-mcp serve NAME [options] or simplex-mcp register NAME [options]. Run simplex-mcp --help.` | [cli.ts:68](../src/cli.ts#L68) |
| 2 | `NAME: 1 to 32 lowercase letters, digits and single hyphens, starting with a letter or digit.` | [cli.ts:69](../src/cli.ts#L69) |
| 2 | `OPTION: a value expected.` | [cli.ts:158](../src/cli.ts#L158) |
| 2 | `Unknown option OPTION. Run simplex-mcp --help.` | [cli.ts:211](../src/cli.ts#L211) |
| 2 | `--display-name: not a valid SimpleX name; try VALID` | [cli.ts:175](../src/cli.ts#L175) |
| 2 | `--approve: requests or host expected, got VALUE` | [cli.ts:180](../src/cli.ts#L180) |
| 2 | `--qr-code: small or large expected, got VALUE` | [cli.ts:185](../src/cli.ts#L185) |
| 2 | `--scope: user or local expected, got VALUE` | [cli.ts:190](../src/cli.ts#L190) |
| 2 | `--owners: ID:NAME expected, got ENTRY` | [cli.ts:196](../src/cli.ts#L196) |
| 2 | `--expire: hours from 1 to 23 expected, got VALUE` | [cli.ts:103](../src/cli.ts#L103), [:202](../src/cli.ts#L202) |
| 2 | `--stop-wait: seconds from 0 to 60 expected, got VALUE` | [cli.ts:106](../src/cli.ts#L106), [:202](../src/cli.ts#L202) |
| 2 | `--port: a port from 1 to 65535 expected, got VALUE` | [cli.ts:115](../src/cli.ts#L115), [:202](../src/cli.ts#L202) |
| 2 | `--reset-owners takes --owners without a list: member IDs change in a new agent control chat.` | [cli.ts:125](../src/cli.ts#L125) |
| 2 | `--owners: member ENTRY is not in the agent control chat. Start with --reset-owners --owners, or use the IDs of current members.` | [control.ts:420](../src/control.ts#L420) |
| 1 | `Another server uses DATA (PID N). If no server runs, remove DATA/lock.` | [files.ts:108-109](../src/files.ts#L108-L109), [:174-176](../src/files.ts#L174-L176) |
| 1 | `Port PORT is in use. Start with --port N.` | [serve.ts:188](../src/serve.ts#L188) |
| 1 | `Agent "NAME" was never started. Start it with: npx -y @simplex-chat/mcp serve NAME` | [register.ts:35](../src/register.ts#L35) |
| 1 | `Add this entry under LOCATION in ~/.claude.json: {"simplex-NAME": ENTRY}` | [register.ts:55-58](../src/register.ts#L55-L58) |
| 1 | `claude mcp add-json failed: STDERR` | [register.ts:42](../src/register.ts#L42) |
| 1 | `simplex-mcp: ERROR` for other errors, `stop: ERROR` for a failed stop | [cli.ts:259-260](../src/cli.ts#L259-L260), [serve.ts:54-57](../src/serve.ts#L54-L57) |

LOCATION is `"mcpServers"` for scope `user`, and `"projects" > "PROJECT" > "mcpServers"` for scope `local` ([register.ts:56](../src/register.ts#L56)).

### 3.4 Terminal lines

stdout ([log.ts:37-39](../src/log.ts#L37-L39)):

| Moment | Line | Code |
|---|---|---|
| start | `SimpleX MCP server VERSION, agent "NAME"`<br>`Data folder: DATA`<br>`Approval mode: MODE` | [serve.ts:72-74](../src/serve.ts#L72-L74) |
| control chat created | `Agent control chat "DISPLAY" created.` | [control.ts:580](../src/control.ts#L580) |
| link opened | `Join the agent control chat in the SimpleX app. The link works once:`<br>QR code<br>link<br>small QR code: `If the QR code does not scan, restart with --qr-code large, or paste the link into the app.`<br>role line | [control.ts:470-480](../src/control.ts#L470-L480) |
| listening | `Listening on http://127.0.0.1:PORT/mcp` | [serve.ts:122](../src/serve.ts#L122) |
| owner setup done | `Add the agent to Claude Code:`<br>`  npx -y @simplex-chat/mcp register NAME [--data DATA]` | [serve.ts:157-159](../src/serve.ts#L157-L159) |
| owner joined | `NAME joined (member ID) and is now an owner. The link is deleted.` | [control.ts:440](../src/control.ts#L440) |
| listed owners set | `Owners: ENTRIES.` | [control.ts:431](../src/control.ts#L431) |
| approvers gone, `--owners` absent | `No owners in the agent control chat.` | [control.ts:35](../src/control.ts#L35), [:229](../src/control.ts#L229), [:407](../src/control.ts#L407) |
| control chat lost | `The agent lost its control chat. Restart the server with --reset-owners.` | [control.ts:467](../src/control.ts#L467) |
| action created | `Action N: WHAT` with `, as itself` or `, incognito` for `connect` and ` (CHAT)` for the other tools | [actions.ts:216-217](../src/actions.ts#L216-L217), [:388-404](../src/actions.ts#L388-L404) |
| counter write or card post failed | `Action N: failed: ERROR.` | [actions.ts:332](../src/actions.ts#L332) |
| perform ended, not interrupted by the stop, mode `requests` | `Action N approved by NAME: RESULT` with RESULT `sent.`, `downloaded.`, `deleted.`, `left.`, `connected.`, `connection requested.` or `failed: ERROR.` | [actions.ts:353](../src/actions.ts#L353), [:410-420](../src/actions.ts#L410-L420) |
| perform ended, not interrupted by the stop, mode `host` | `Action N: performed without approval (mode host).`, or `...; failed: ERROR.` | [actions.ts:412](../src/actions.ts#L412) |
| rejected | `Action N rejected by NAME.` | [actions.ts:279](../src/actions.ts#L279) |
| expired | `Action N expired.` | [actions.ts:316](../src/actions.ts#L316) |
| `/leave CHAT` | `Left KIND NAME (CHAT), requested by NAME.` or `Deleted ...` | [control.ts:339](../src/control.ts#L339) |
| refused file, file not sent | as in [2.3](#23-other-posts), names and paths on one line | [control.ts:189](../src/control.ts#L189), [:200](../src/control.ts#L200) |
| stop | `Cancelled K action.` or `Cancelled K actions.` when K > 0<br>`Stopping: sending K cancelled card(s), up to S s. Ctrl-C to stop now.` when cards were edited or posted and S > 0<br>`Stopped.` | [serve.ts:148](../src/serve.ts#L148), [:161-175](../src/serve.ts#L161-L175), [:152](../src/serve.ts#L152) |
| membership messages | see [2.4](#24-membership-messages) | |

stderr: `CONTEXT: ERROR` from `logError` ([log.ts:41-43](../src/log.ts#L41-L43)), with CONTEXT one of `action N`, `action N card`, `action N card image`, `action N file`, `control chat command`, `EVENT in the agent control chat`, `EVENT event`, `refused file`, `file not sent`, `download of file N`, `file N`, `moving file N`, `cancelling file N`, `resizing PATH`, `preview`, `MCP`, `HTTP request`, `stop`, `simplex-mcp`. The library's download lines are written to stderr.

---

## 4. Claude Code plugin

### 4.1 Tools in the plugin

| Rule | Code |
|---|---|
| The six tools of [1](#1-tools) and `reply` are registered on the session's `McpServer`; Claude Code names them `mcp__plugin_simplex_simplex__NAME` | [session.ts:57](../src/session.ts#L57), [hooks.json:4](../hooks/hooks.json#L4) |
| `_meta: {"anthropic/requiresUserInteraction": true}` marks the four action tools when the option `approve` is `host` at the session process's start | [session.ts:57](../src/session.ts#L57), [tools.ts:507](../src/tools.ts#L507) |
| Instructions: those of [1.1](#11-common-rules) for the display name `Claude` and the host `SimpleX daemon`, followed by the channel's | [session.ts:21](../src/session.ts#L21), [:52](../src/session.ts#L52), [tools.ts:442-453](../src/tools.ts#L442-L453) |
| `send`, `receive`, `download`, `leave` and `list` see the session's own chats only; another session's chat is `No chat CHAT. list shows the agent's chats.` | [tools.ts:583-585](../src/tools.ts#L583-L585), [:1306-1315](../src/tools.ts#L1306-L1315) |
| `connect` to a chat of another session: `The chat of TARGET belongs to another session; an owner can give it to this session with /give.` | [tools.ts:760-765](../src/tools.ts#L760-L765) |
| Actions are numbered per session group, from 1, and the action key holds the session number | [hub.ts:81](../src/hub.ts#L81), [tools.ts:1713-1715](../src/tools.ts#L1713-L1715) |
| Downloads are saved under `ROOT/_sessions/ID/files/` | [hub.ts:96](../src/hub.ts#L96), [messages.ts:225-227](../src/messages.ts#L225-L227) |
| A run with `CLAUDE_CODE_SESSION_ATTENDED` `0`, which Claude Code sets for the hooks of a run nobody attends, or with `CLAUDE_CODE_ENTRYPOINT` `sdk-cli`, which Claude Code sets for `claude -p`, or `sdk-ts` or `sdk-py`, the Agent SDK, has no session: its session process does not connect to the daemon, every call returns `This Claude Code run is not interactive, so it has no SimpleX group; the SimpleX tools work in interactive sessions.`, before an invalid option's text, and its hooks return `{}` without a request | [config.ts:22-28](../src/config.ts#L22-L28), [session.ts:15](../src/session.ts#L15), [:45-51](../src/session.ts#L45-L51), [:66](../src/session.ts#L66), [hook.ts:42](../src/hook.ts#L42), [:60](../src/hook.ts#L60) |

`reply` ([`replyTool`](../src/tools.ts#L463-L486), [`AgentTools.reply`](../src/tools.ts#L719-L740)):

| Field | Type | Range | Default | Rule |
|---|---|---|---|---|
| `file` | string | 1 character or more | required | path of a file of this computer |
| `text` | string | 1 to 12000 characters | | the caption |

The file is checked and sent as for `send` ([1.3](#13-send)): types, refused paths, empty files and the change check. Tool error without a group: `reply: this server has no group to reply to.`

```
on reply {file, text?}:
  key = [session, "reply", {file, text ?? null}]; an action of key: attach to it
  file checked
  every member of the session group is Claude, User (its contact marked simplexMcpUser),
  a member with status rejected, removed, left or deleted, or an approver:
    sent at once                                   -> {item}
  otherwise: a reply action in mode requests, also in mode host, its card in the session group
             with the file's name and size only
    approved: sent with its caption and preview    -> {action, status: done, item}
    rejected, expired, failed                      -> {action, status, error?}
```

Sources: [tools.ts:719-740](../src/tools.ts#L719-L740), [:562-564](../src/tools.ts#L562-L564), [hub.ts:100](../src/hub.ts#L100), [control.ts:149-153](../src/control.ts#L149-L153). A member invited and not yet joined counts as a member.

### 4.2 Main group

Commands ([`MainGroup.command`](../src/main-group.ts#L125-L130)), one line `/KEYWORD PARAMS` as in [2.1](#21-commands-and-replies), from any member:

| Request | Condition | Reply | Code |
|---|---|---|---|
| `/sessions` | no session | `No sessions.` | [hub.ts:427-430](../src/hub.ts#L427-L430) |
| `/sessions` | otherwise | one line per session of the hub: `N · NAME`, with `, closed` when it has no entry | [hub.ts:427-430](../src/hub.ts#L427-L430) |
| `/join N` | no session N | `No session N. /sessions lists the sessions.` | [hub.ts:434](../src/hub.ts#L434) |
| `/join N` | the sender is a member of its group | `You are a member of "N · NAME".` | [hub.ts:435-440](../src/hub.ts#L435-L440) |
| `/join N` | otherwise: an invitation through the Main group, with the sender's role there, recorded in `invited` | `Sent you an invitation to "N · NAME".` | [hub.ts:442](../src/hub.ts#L442), [:216-220](../src/hub.ts#L216-L220) |
| `/approve reset`, `/reject reset` | sender not an owner | `Only owners decide a reset of the owners.` | [main-group.ts:133](../src/main-group.ts#L133) |
| `/approve reset`, `/reject reset` | no reset card | `No reset of the owners is requested.` | [main-group.ts:135](../src/main-group.ts#L135) |
| `/approve reset`, `/reject reset` | accepted | none; the card is edited to `Approved by NAME.` or `Rejected by NAME.`; after an approval the group is replaced | [main-group.ts:136-141](../src/main-group.ts#L136-L141) |
| other one-line `/` text | | `Commands: /sessions, /join N. Owners decide a requested reset of the owners with /'approve reset' or /'reject reset'.` | [main-group.ts:14](../src/main-group.ts#L14), [:129](../src/main-group.ts#L129) |

Posts:

| Moment | Text | Code |
|---|---|---|
| the first owner joined | `I'm Claude. Each Claude Code session has its own group, and you get its invitation here. /sessions lists the sessions; /join N joins the group of session N.` | [main-group.ts:13](../src/main-group.ts#L13), [:164](../src/main-group.ts#L164) |
| `reset-owners` while owners exist | `A terminal asks to replace the owners: this group is deleted, and a new Main group with a new link replaces it.` and `/'approve reset' or /'reject reset'`; one card at a time | [main-group.ts:15](../src/main-group.ts#L15), [:105-109](../src/main-group.ts#L105-L109) |

Membership of the Main group applied to the session groups ([`Session.applyMembers`](../src/hub.ts#L222-L246), [`Hub.applyMembers`](../src/hub.ts#L451-L453)):

```
when: a membership event of the Main group, after its members are read again   main-group.ts:149
      the open of a stored session's group, as at the daemon's start           hub.ts:419
      the replacement of the Main group, after the new group loads             main-group.ts:119
the Main group lost: nothing
for each entry {groupMemberId, mainMemberId} in invited of each session group:
  main   = the Main group's member mainMemberId; gone: absent, or status rejected, removed, left or deleted
  member = the session group's member groupMemberId, read for this check
  main gone:
    member present and not gone: removed, in a request of its own
    the entry dropped from invited
  member present, not gone, with a role above main's role:
    status invited: the invitation sent again through the Main group with main's role
    else:           its role set to main's role
  a failed request: logged as "members of session N", the entry kept
roles, lowest first: observer, author, member, moderator, admin, owner                 hub.ts:29
```

Group profile ([`createMainGroup`](../src/main-group.ts#L221-L230), [`load`](../src/main-group.ts#L65-L72)):

| Field | Value |
|---|---|
| display name | option `main_group`, updated when it differs |
| description | `Supervise your Claude Code sessions: each one has its own group, and you get its invitation here.` |
| history | on |
| direct messages | on for the role `owner` |
| command menu | `/sessions` "Claude Code sessions"<br>`/join` "Join the group of a session", params `<session number>` |
| custom data | `{simplexMcpMain: true}` |

The hub's profile `Claude` posts in the group, with peer type `agent` ([hub.ts:18](../src/hub.ts#L18), [core.ts:25](../src/core.ts#L25)).

### 4.3 Session groups

Commands and replies are those of [2.1](#21-commands-and-replies), from approvers in the sense of [architecture 8.5](architecture.md#85-approval-model), with these changes:

| Request | Condition | Reply | Code |
|---|---|---|---|
| any request | the sender is not an approver | `Only owners approve actions.` | [control.ts:239](../src/control.ts#L239) |
| `/give CHAT N` | CHAT is not a chat of this session | `No chat CHAT. /list shows the agent's chats.` | [control.ts:362](../src/control.ts#L362) |
| `/give CHAT N` | no session N | `No session N. /sessions in the Main group lists the sessions.` | [control.ts:364](../src/control.ts#L364) |
| `/give CHAT N` | accepted: CHAT's owner set to session N; the pending actions of CHAT with a card are rejected by the sender | `Gave KIND NAME (CHAT) to session N.` | [control.ts:365-370](../src/control.ts#L365-L370) |
| `/list`, `/revoke`, `/leave` | | the chats of this session, also while it is closed | [tools.ts:1296-1315](../src/tools.ts#L1296-L1315) |
| `/give CHAT N` in the control chat of `serve` | | the help reply | [control.ts:360](../src/control.ts#L360) |
| one-line `/` text whose KEYWORD is none of `approve`, `reject`, `receive`, `forward`, `revoke`, `leave`, `delete`, `give`, `pending`, `list` | the sender is an approver | none: the message is a prompt of the session | [control.ts:19](../src/control.ts#L19), [:210](../src/control.ts#L210), [hub.ts:160](../src/hub.ts#L160) |

Cards of [2.2](#22-cards), and three more:

| Tool | Request lines | Command line |
|---|---|---|
| `mode` | `I want to run in mode host: Claude Code asks you before each of my actions, and they are performed without approval here.`<br>done: `I run in mode host: ...` | `/'approve N' or /'reject N'` |
| `permission` | ``I want to use `TOOL`: `DESCRIPTION` ``, then the input preview, one code span per line, cut to fit the card ([1.9](#19-action-refusals)) | `/'approve N' or /'reject N'` |
| `reply` | `I want to send a file to KIND NAME, where members who are not owners see it:`, ``` `NAME`, SIZE ``` of the file; the card is a text message, and the caption and an image's or a video's preview reach the group with the file after the approval<br>done: `I sent a file to ...` | `/'approve N' or /'reject N'` |

`mode`, `permission` and `reply` are exempt from the card size check; a `reply` card holds only the group's name and the file's name and size ([control.ts:136-137](../src/control.ts#L136-L137), [:505-511](../src/control.ts#L505-L511), [:653-659](../src/control.ts#L653-L659), [:675-678](../src/control.ts#L675-L678), [:692-698](../src/control.ts#L692-L698)). In `/pending` their WHAT is `run in mode MODE`, `use TOOL` and `send FILE to KIND NAME` ([actions.ts:389-390](../src/actions.ts#L389-L390), [:397-398](../src/actions.ts#L397-L398)). A card of `mode` or `permission` that a turn, a detach or the option `requests` ends has the last line `Ended.`; an expired card has `Expired.` ([actions.ts:244-248](../src/actions.ts#L244-L248), [control.ts:648-649](../src/control.ts#L648-L649)).

Posts:

| Poster | Text | Code |
|---|---|---|
| Claude | the final answer of each turn, in parts of 12000 bytes, the first quoting the turn's prompt from the group when it fits, by the turn records below; then a changed short description | [hub.ts:182-201](../src/hub.ts#L182-L201), [:280-303](../src/hub.ts#L280-L303) |
| Claude | `Session N is not running.`, as a reply to an approver's message that has no attachment at its delivery, after the earlier prompts and its own file; in the first 30 s after the daemon's start, only when the session does not attach by their end | [hub.ts:156-162](../src/hub.ts#L156-L162), [:254-268](../src/hub.ts#L254-L268), [:373](../src/hub.ts#L373) |
| Claude | `The file of this message was not received: ERROR`, as a reply to an approver's message whose file failed or did not arrive in 10 minutes; the prompt is pushed without the file | [hub.ts:254-278](../src/hub.ts#L254-L278) |
| `User` | each prompt typed in the terminal, in parts of 12000 bytes, except blank prompts, prompts made only of this server's channel tags, the prompts pushed from the group and one-line `/` commands; in the newest group marked with the session's number where `User` is a current member, not in an earlier group of the session that Claude deleted | [hub.ts:186-195](../src/hub.ts#L186-L195), [user.ts:30-33](../src/user.ts#L30-L33), [tools.ts:1566-1568](../src/tools.ts#L1566-L1568) |
| Claude | files sent by `reply`, at once or after an owner approved its card | [tools.ts:719-740](../src/tools.ts#L719-L740), [:562-564](../src/tools.ts#L562-L564) |

Claude's posts with a quote, the answer and both replies, are sent again without the quote when the core refuses it with `errorStore` `invalidQuote` or `chatItemNotFound`, as for a deleted prompt ([hub.ts:297-303](../src/hub.ts#L297-L303), [:552-555](../src/hub.ts#L552-L555)).

Turn records, one per `/turn` with `prompt`, read before any await of the `/turn` ([hub.ts:60](../src/hub.ts#L60), [:182-188](../src/hub.ts#L182-L188), [:280-289](../src/hub.ts#L280-L289)):

```
on /turn {at, prompt}:
  item = oldest pushed prompt that prompt contains in a channel tag of this server,
         or that equals prompt and was pushed in the last 60 s; else null
  turns += {at, item}, unless at is below answeredAt; the matched pushed prompts are removed
on /turn {at, answer}, also an empty answer:
  earlier = turns with at below the answer's at
  quote = newest of earlier, its item; earlier leave turns
       ?? the oldest pushed prompt not quoted yet, marked quoted; it stays in pushed for its prompt
  answeredAt = max(answeredAt, at)
at = performance.timeOrigin of the hook process; absent: Infinity for an answer's quote, now otherwise
```

Group profile ([`createSessionGroup`](../src/hub.ts#L485-L497), [`Control.load`](../src/control.ts#L105-L112), [`describe`](../src/control.ts#L119-L122)):

| Field | Value |
|---|---|
| display name | `N · NAME`, folded to a valid display name, renamed at a new title |
| description | `The Claude Code session N: owners of the Main group approve its actions here.` |
| short description | `MODEL · effort LEVEL`, the parts known so far |
| history | on |
| direct messages | off |
| command menu | as in [2.5](#25-group-profile) |
| members | `Claude`, the invited members, `User` |

### 4.4 Tool results in the plugin

Refusals of a new action, first match, in place of those of [1.9](#19-action-refusals):

| Text | Condition | Code |
|---|---|---|
| `The SimpleX daemon is stopping.` | the daemon stops | [hub.ts:31](../src/hub.ts#L31) |
| `The group of this session is lost. Start a new Claude Code session.` | the session group was deleted, Claude was removed from it or lost its owner role | [hub.ts:31](../src/hub.ts#L31), [control.ts:814-829](../src/control.ts#L814-L829) |
| the 20-action limits and the card size texts of [1.9](#19-action-refusals) | per session | [actions.ts:206-211](../src/actions.ts#L206-L211) |
| `The group of this session has no owner. An owner of the Main group joins it by its invitation there; without owners, run npx -y @simplex-chat/mcp@VERSION link in a terminal.`, VERSION the plugin's version | the session group has no approver | [control.ts:37](../src/control.ts#L37), [:139](../src/control.ts#L139), [owner.ts:7](../src/owner.ts#L7) |

Errors of the session process ([session.ts:40-67](../src/session.ts#L40-L67), [:117-149](../src/session.ts#L117-L149), [:184-191](../src/session.ts#L184-L191)):

| Text | Condition |
|---|---|
| `Plugin option KEY: EXPECTED expected, got VALUE. Change it in /plugin > Installed > simplex > Configure options.` | an invalid option at the session's start; every call |
| `The SimpleX daemon stopped; repeat the call.` | the daemon is not running, is stopping (503), or its stream ended without a result |
| `Restart Claude Code to update the SimpleX plugin.` | the daemon speaks another protocol version |
| `The SimpleX daemon exited; see ROOT/daemon.log`, `The SimpleX daemon did not start in 5 minutes.`, or the daemon's error text | the daemon could not start |
| `Unknown session; restart the Claude Code session.` | after one repeat, the daemon has no folder for the session |
| `Cancelled.` | the daemon returned `{result: null}` for a call that was not aborted |

When its `/attach` ends or its start fails, also with a spawn error, the session server starts the daemon when none runs and attaches again: after 1 s, and after each failure twice as late, up to 30 s, until its stdin closes or the daemon speaks another protocol version, the `ProtocolError` ([session.ts:86-110](../src/session.ts#L86-L110), [:173-179](../src/session.ts#L173-L179), [client.ts:17](../src/client.ts#L17), [:45](../src/client.ts#L45)).

### 4.5 Channel

Capabilities of the session server: `experimental: {"claude/channel": {}, "claude/channel/permission": {}}` ([session.ts:52](../src/session.ts#L52)).

| Direction | Method | Params | Code |
|---|---|---|---|
| to Claude Code | `notifications/claude/channel` | `content`: the prompt's text<br>`meta.from`: the owner's display name<br>`meta.item`: the item ID in the session group, as a string<br>`meta.file`: the path of the prompt's file, when it has one | [session.ts:194](../src/session.ts#L194) |
| to Claude Code | `notifications/claude/channel/permission` | `request_id`<br>`behavior`: `allow` or `deny` | [session.ts:195](../src/session.ts#L195) |
| from Claude Code | `notifications/claude/channel/permission_request` | `request_id`, `tool_name`, `description`, `input_preview`, all strings | [session.ts:22-23](../src/session.ts#L22-L23), [:58-62](../src/session.ts#L58-L62) |

Channel instructions: `Messages in the simplex channel are prompts of your owners in the SimpleX group of this session: from is the owner's name, item the message ID, and file the path of a file they sent. Your final answer of each turn reaches them in that group; reply sends them a file.` ([session.ts:21](../src/session.ts#L21)).

The prompts of a session are delivered one at a time, in arrival order ([hub.ts:161](../src/hub.ts#L161), [:254-268](../src/hub.ts#L254-L268)). A prompt's file is received without a card, with relays approved, into `ROOT/_sessions/ID/files/group/<file ID><extension>`, while a session is attached; the prompt is pushed after `rcvFileComplete`, and without `file` after `rcvFileError`, a failed receive or 10 minutes; the next prompt waits until then ([hub.ts:254-278](../src/hub.ts#L254-L278), [:209-214](../src/hub.ts#L209-L214)). Pushed prompts are kept until a turn's `prompt` matches them, at most 20 ([hub.ts:24](../src/hub.ts#L24), [:188](../src/hub.ts#L188), [:266](../src/hub.ts#L266), [:282-286](../src/hub.ts#L282-L286)).

### 4.6 Socket protocol

HTTP/1.1 over `ROOT/daemon.sock`, the fallback path or the Windows pipe, protocol version `1`. Every request carries `Authorization: Bearer TOKEN`; bodies are JSON; streamed replies are NDJSON with the content type `application/x-ndjson`. Errors are `{"error": TEXT}` with a status other than 200. Types: [`protocol.ts`](../src/protocol.ts); routes: [`Daemon.routes`](../src/daemon.ts#L168-L192). Only `/title` and `/link` return the Main group link.

| Route | Request | Reply | Code |
|---|---|---|---|
| `POST /session` | `{id, pid?, project, name?, config: {mainGroup, approve, expireHours}}`, `pid` the session server's parent process | `{number, displayName, version, protocol, owner: {owners}}` | [daemon.ts:233-246](../src/daemon.ts#L233-L246) |
| `POST /title` | `{id, pid?, name?, project, model?, config}`, `pid` from `CLAUDE_PID` | `{mainGroup, owner: {owners, link?}, version, protocol}`, after the title applied, or at once for a held title | [daemon.ts:248-278](../src/daemon.ts#L248-L278) |
| `POST /attach` | `{id}` | stream held open: first line `{version, protocol}`, then `{prompt}` and `{permission}` lines | [daemon.ts:320-340](../src/daemon.ts#L320-L340) |
| `POST /call` | `{id, tool, input}` | lines `{progress: TEXT}`, then one line `{result: CallToolResult or null}`; closing the request aborts the call | [daemon.ts:342-361](../src/daemon.ts#L342-L361) |
| `POST /permission` | `{id, requestId, toolName, description, inputPreview}` | `{}` after the card's action is created | [daemon.ts:174-178](../src/daemon.ts#L174-L178) |
| `POST /turn` | `{id, pid?, at?, prompt?, answer?, model?, effort?}`, `at` the time the hook started | `{}` after the posts | [daemon.ts:179-183](../src/daemon.ts#L179-L183) |
| `GET /status` | none | `{version, protocol, mainGroup, owner: {owners}, sessions: [{number, name, attached}]}`, the open sessions | [daemon.ts:447-450](../src/daemon.ts#L447-L450) |
| `GET /link` | none | `{owners, link?}`, the link while the Main group has no owner | [daemon.ts:185](../src/daemon.ts#L185), [hub.ts:386-389](../src/hub.ts#L386-L389) |
| `POST /reset-owners` | none | `{link}` after a reset without owners; `{}` after the card | [daemon.ts:186](../src/daemon.ts#L186), [hub.ts:397-401](../src/hub.ts#L397-L401) |
| `POST /stop` | none | `{}` after the stop sequence; the process exits after the reply | [daemon.ts:187-190](../src/daemon.ts#L187-L190) |

| Status | Error | Condition |
|---|---|---|
| 401 | `Unauthorized` | token missing or wrong |
| 404 | `Not found` | other method or path |
| 400 | `Invalid JSON` | body not JSON |
| 400 | `Not a Claude Code session ID: ID` | a session route with an ID that is not a UUID |
| 400 | `Invalid config.` | `/session` or `/title` whose `config` has an invalid `mainGroup`, `approve` or `expireHours` |
| 409 | `Unknown session; restart the Claude Code session.` | `/attach`, `/call`, `/permission` or `/turn` for an ID without a folder |
| 413 | `Body too large` | body over 1 MiB |
| 503 | `The SimpleX daemon is stopping.` | a session route while the daemon stops |
| 500 | the error message | any other error, also a refused `permission` action |

Session routes are `/session`, `/title`, `/attach`, `/call`, `/permission` and `/turn` ([daemon.ts:374-407](../src/daemon.ts#L374-L407)). Their ID is that of a session, or an alias of one ([daemon.ts:280-311](../src/daemon.ts#L280-L311)):

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
  the title resolves as above once a /session with that pid arrives, or after 10 s, while the daemon runs
on /session that creates the session of a held title: the title's name
a new session.json:                              the aliases that already point to its ID
on a daemon stop:                                the held titles into ROOT/titles.json
on the next start:                               each one taken as a new /title
```

`/session` and `/title` apply `config` to the hub: the expiry of all sessions, and a rename of the Main group when `mainGroup` differs ([hub.ts:391-395](../src/hub.ts#L391-L395)). `/session` also requests its session's mode: `requests` applies at once, ends a pending mode card and clears `hostDeclined`; `host` posts the mode card when the group has an approver and `hostDeclined` is unset, at once or after the next membership event of the group. A rejected mode card sets `hostDeclined` in session.json; the mode of an approved card, and `requests` after it, is kept as `mode` in the session group's custom data, which the session reads when it opens ([hub.ts:135-154](../src/hub.ts#L135-L154), [:316-319](../src/hub.ts#L316-L319), [:321-324](../src/hub.ts#L321-L324), [:81](../src/hub.ts#L81), [:526](../src/hub.ts#L526)). `/title` opens or creates the session, renames it for a non-blank name, and passes `model` to `turn`, at once or, for a held title, after the wait ([daemon.ts:248-278](../src/daemon.ts#L248-L278), [:489-499](../src/daemon.ts#L489-L499)).

### 4.7 Plugin commands

| Command | Run by | Output |
|---|---|---|
| `simplex-mcp session` | `.mcp.json` | MCP over stdio |
| `simplex-mcp daemon` | `spawnDaemon` | ready line on fd `SIMPLEX_MCP_READY_FD`, stdout when unset: `ready`, `running` or `error: TEXT`; when another live process holds ROOT/lock, up to 150 checks 100 ms apart: a released lock starts the daemon again, a socket that answers gives `running`; the log on stdout and stderr |
| `simplex-mcp hook session-start` | SessionStart hook | one JSON line, see [4.8](#48-plugin-options-files-and-hook-output) |
| `simplex-mcp hook turn` | UserPromptSubmit, Stop and PostModelSwitch hooks | `{}`; a prompt or an answer over 250,000 characters is cut there and ends with `…` ([hook.ts:15](../src/hook.ts#L15), [:80-82](../src/hook.ts#L80-L82)) |
| `simplex-mcp link` | `/simplex:link`, people | `Owners of "MAIN": A, B. To replace them, run /simplex:reset-owners.`; without owners on a terminal `Join "MAIN" in the SimpleX app. The link works once:`, a QR code and the link, elsewhere `Run npx -y @simplex-chat/mcp@VERSION link in a terminal to see the link to "MAIN".`; without a link `The Main group has no link. Run npx -y @simplex-chat/mcp@VERSION reset-owners in a terminal.` |
| `simplex-mcp reset-owners` | `/simplex:reset-owners`, people | while owners exist: `An owner approves the reset in "MAIN". Then run npx -y @simplex-chat/mcp@VERSION link in a terminal to see the link to the new group.`; without owners on a terminal: `"MAIN" was replaced. Join the new one in the SimpleX app. The link works once:`, a QR code and the link; elsewhere `Run npx -y @simplex-chat/mcp@VERSION reset-owners in a terminal.` |

VERSION is the version of the package that prints the text ([owner.ts:7-8](../src/owner.ts#L7-L8)).

Environment: `SIMPLEX_MCP_HOME`, ROOT; `CLAUDE_CODE_SESSION_ID` and `CLAUDE_PROJECT_DIR`, the session; `CLAUDE_PID`, the Claude Code process of a hook; `CLAUDE_CODE_ENTRYPOINT`, a run that is not interactive for the values `sdk-cli`, `sdk-ts` and `sdk-py`, and `CLAUDE_CODE_SESSION_ATTENDED`, a run that is not interactive for the value `0`, both read by the session process and the hooks ([4.1](#41-tools-in-the-plugin)); `CLAUDE_PLUGIN_OPTION_MAIN_GROUP`, `CLAUDE_PLUGIN_OPTION_APPROVE` and `CLAUDE_PLUGIN_OPTION_EXPIRE_HOURS`, the options; `SIMPLEX_MCP_READY_FD`, the daemon. The daemon is spawned with the variables of the spawning process whose names match, case-insensitively, `PATH`, `PATHEXT`, `HOME`, `USERPROFILE`, `HOMEDRIVE`, `HOMEPATH`, `TMPDIR`, `TMP`, `TEMP`, `XDG_CACHE_HOME`, `LOCALAPPDATA`, `APPDATA`, `SYSTEMROOT`, `SYSTEMDRIVE`, `WINDIR`, `COMSPEC`, `LANG`, `LC_*`, `LD_LIBRARY_PATH`, `DYLD_LIBRARY_PATH`, `HTTP_PROXY`, `HTTPS_PROXY`, `NO_PROXY`, `NODE_USE_ENV_PROXY`, `NODE_EXTRA_CA_CERTS`, `NODE_OPTIONS`, `SSL_CERT_FILE`, `SSL_CERT_DIR`, `SIMPLEX_*` and `CLAUDE_PLUGIN_OPTION_*` ([client.ts:13](../src/client.ts#L13), [:78](../src/client.ts#L78)).

| Code | Message | Code location |
|---|---|---|
| 0 | the hooks, also after errors; `link` and `reset-owners`; the daemon after a stop or with `running` | [hook.ts:54-56](../src/hook.ts#L54-L56), [:74-77](../src/hook.ts#L74-L77), [daemon.ts:463](../src/daemon.ts#L463) |
| 2 | `simplex-mcp session runs from the Claude Code plugin.` | [session.ts:42](../src/session.ts#L42) |
| 2 | `Not a Claude Code session ID: ID` | [session.ts:43](../src/session.ts#L43) |
| 1 | `Restart Claude Code to update the SimpleX plugin.`, a spawn error (`link`, `reset-owners`) | [client.ts:45](../src/client.ts#L45), [:84](../src/client.ts#L84), [:111](../src/client.ts#L111) |
| 1 | daemon start failed: ready line `error: TEXT`, also `error: Another daemon uses ROOT (PID N). If no daemon runs, remove ROOT/lock.` for a lock that its holder neither releases nor answers for in 150 checks | [daemon.ts:461-465](../src/daemon.ts#L461-L465), [:501-508](../src/daemon.ts#L501-L508), [files.ts:175](../src/files.ts#L175) |
| 1 | `stop: ERROR` for a failed stop after a signal | [daemon.ts:469-474](../src/daemon.ts#L469-L474) |

`daemon.log` receives the daemon's stdout and stderr ([client.ts:77](../src/client.ts#L77)); it is emptied when over 10 MB, at each spawn and every hour while the daemon runs ([files.ts:87-96](../src/files.ts#L87-L96), [daemon.ts:114-121](../src/daemon.ts#L114-L121)). stdout lines:

| Moment | Line | Code |
|---|---|---|
| start | `SimpleX MCP daemon VERSION, data folder ROOT` | [daemon.ts:467](../src/daemon.ts#L467) |
| Main group created | `Main group "NAME" created.` | [main-group.ts:228](../src/main-group.ts#L228) |
| link opened | `Main group link created. Whoever joins by it first becomes an owner of "NAME".` | [main-group.ts:176](../src/main-group.ts#L176) |
| no owners, members present | `No owners in the Main group.` | [main-group.ts:16](../src/main-group.ts#L16), [:76](../src/main-group.ts#L76), [:152](../src/main-group.ts#L152) |
| owner joined | `NAME joined (member ID) and is now an owner. The link is deleted.` | [main-group.ts:165](../src/main-group.ts#L165) |
| Main group lost | `The Main group is lost. Run npx -y @simplex-chat/mcp@VERSION reset-owners in a terminal.` | [main-group.ts:170](../src/main-group.ts#L170) |
| reset card, decision | `The reset of the owners waits for an owner's approval in the Main group.`, `Reset of the owners: Approved by NAME.` | [main-group.ts:108](../src/main-group.ts#L108), [:139](../src/main-group.ts#L139) |
| session group created | `Group "N · NAME" of session N created.` | [hub.ts:495](../src/hub.ts#L495) |
| session group lost | `The group "N · NAME" is lost.` | [control.ts:467](../src/control.ts#L467) |
| group of a session closed 7 days deleted | `The group of session N is deleted: the session was closed for 7 days.` | [daemon.ts:154](../src/daemon.ts#L154) |
| actions | as in [3.4](#34-terminal-lines), with ` of session N` after `Action N`; `allowed.` for a tool use | [actions.ts:217](../src/actions.ts#L217), [:418](../src/actions.ts#L418) |
| stop | `Cancelled K action(s).`, `Stopping: sending K cancelled card(s), up to 10 s. Ctrl-C to stop now.`, `Stopped.` | [daemon.ts:220](../src/daemon.ts#L220), [serve.ts:164](../src/serve.ts#L164), [daemon.ts:223](../src/daemon.ts#L223) |

stderr: `CONTEXT: ERROR` from `logError`, with CONTEXT among `Main group command`, `EVENT in the Main group`, `EVENT in the group of session N`, `prompt of session N`, `invitation to session N`, `members of session N`, `User`, `User in session N`, `group of session N`, `session N`, `owner of CHAT`, `mode host of session N`, `title`, `closed sessions`, `ROOT/_sessions/ID/session.json`, `ROOT/titles.json`, `ROOT/daemon.log`, `METHOD PATH` and those of [3.4](#34-terminal-lines). A session process writes `SimpleX daemon: ERROR`, `SimpleX channel: ERROR` and `permission relay: ERROR` to its own stderr, and the turn hook `SimpleX MCP turn: ERROR` ([session.ts:55](../src/session.ts#L55), [:61](../src/session.ts#L61), [:91](../src/session.ts#L91), [hook.ts:75](../src/hook.ts#L75)).

### 4.8 Plugin options, files and hook output

`userConfig` of [`.claude-plugin/plugin.json`](../.claude-plugin/plugin.json), read by [`readConfig`](../src/config.ts#L30-L36):

| Key | Variable | Type | Default | Valid | Error |
|---|---|---|---|---|---|
| `main_group` | `CLAUDE_PLUGIN_OPTION_MAIN_GROUP` | string | `SimpleX MCP` | equal to `validName(value)` | `a valid SimpleX name, such as VALID expected` or `a valid SimpleX name expected` |
| `approve` | `CLAUDE_PLUGIN_OPTION_APPROVE` | string | `requests` | `requests`, `host` | `requests or host expected` |
| `expire_hours` | `CLAUDE_PLUGIN_OPTION_EXPIRE_HOURS` | number | 23 | digits only, 1 to 23 | `hours from 1 to 23 expected` |

A missing or blank variable takes the default ([config.ts:31-35](../src/config.ts#L31-L35)). The hooks receive the variables from Claude Code; the MCP server gets them from `.mcp.json`, which maps `${user_config.KEY}` to the same names.

| File | Content |
|---|---|
| [`.claude-plugin/plugin.json`](../.claude-plugin/plugin.json) | name `simplex`, version `0.3.0`, equal to `package.json`, description, author, homepage, repository, license, keywords, `userConfig` |
| [`.mcp.json`](../.mcp.json) | server `simplex`: `node ${CLAUDE_PLUGIN_ROOT}/dist/cli.js session` with the three option variables; `timeout` 169800000 ms |
| [`hooks/hooks.json`](../hooks/hooks.json) | SessionStart: `node "${CLAUDE_PLUGIN_ROOT}/dist/cli.js" hook session-start`, timeout 600 s; PreToolUse, matcher `mcp__plugin_simplex_simplex__.*`: `node "${CLAUDE_PLUGIN_ROOT}/plugin/allow.js"`; UserPromptSubmit, Stop and PostModelSwitch: `node "${CLAUDE_PLUGIN_ROOT}/dist/cli.js" hook turn`, `async: true` |
| [`npm-shrinkwrap.json`](../npm-shrinkwrap.json) | the package's dependency tree, from which Claude Code installs the plugin's modules into its folder; listed in `files` of `package.json`. The pack drops the development dependencies from both files: `prepack` runs [`scripts/strip-dev.js`](../scripts/strip-dev.js) after the build, which first restores both files from their `.orig` copies when those exist, saves them as `.orig`, deletes `devDependencies` from `package.json`, and drops the entries marked `dev` and the root's `devDependencies` from the shrinkwrap; `postpack` restores both files |
| [`plugin/allow.js`](../plugin/allow.js) | prints `{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"allow"}}` |
| [`skills/link/SKILL.md`](../skills/link/SKILL.md), [`skills/reset-owners/SKILL.md`](../skills/reset-owners/SKILL.md) | `/simplex:link`, `/simplex:reset-owners`: run `node "${CLAUDE_PLUGIN_ROOT}/dist/cli.js" link` or `reset-owners`, show the output; `disable-model-invocation: true`; `allowed-tools` pre-approves the command |
| [`/.claude-plugin/marketplace.json`](../../../.claude-plugin/marketplace.json) | marketplace `simplex-chat`, plugin `simplex`, source `{"source": "npm", "package": "@simplex-chat/mcp"}` |

Hook input fields used: SessionStart `session_id`, `session_title`, `cwd`, `model`; the others `session_id`, `prompt`, `last_assistant_message`, `to_model`, `effort.level` ([hook.ts:17-30](../src/hook.ts#L17-L30)). SessionStart output ([hook.ts:41-57](../src/hook.ts#L41-L57)):

| Case | Output |
|---|---|
| a run that is not interactive ([4.1](#41-tools-in-the-plugin)) | `{}`, without a request |
| Node.js below 22 | `{"systemMessage": "SimpleX MCP needs Node.js 22 or later; found V."}` |
| owners present, or no link | `{}` |
| no owner, a link | `systemMessage`: `SimpleX MCP: scan this QR code with the SimpleX app and tap Join to supervise your agents in "MAIN":`, the `plain` QR code and the link |
| daemon not running after the start | `{"systemMessage": "SimpleX MCP: The SimpleX daemon stopped; it starts again with the next tool call."}` |
| other errors, invalid options included | `{"systemMessage": "SimpleX MCP: TEXT"}` |
