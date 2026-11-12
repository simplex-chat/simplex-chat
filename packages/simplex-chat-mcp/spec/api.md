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

| Rule | Code |
|---|---|
| Tools are registered on each per-request `McpServer` with description, input schema, output schema and, in mode `host`, `_meta: {"anthropic/requiresUserInteraction": true}` for `connect`, `send`, `download` and `leave` | [tools.ts:485-492](../src/tools.ts#L485-L492), [:498-508](../src/tools.ts#L498-L508), [:540-542](../src/tools.ts#L540-L542) |
| Tool descriptions and server instructions; the instructions name the agent's display name | [tools.ts:414-452](../src/tools.ts#L414-L452), [serve.ts:108](../src/serve.ts#L108) |
| Arguments are validated by the SDK; a failure is a result with `isError: true` and the text `Input validation error: Invalid arguments for tool NAME: ERRORS` | [mcp-BumlNux0.cjs:1812-1817](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L1812-L1817) |
| `additionalProperties: false` is set in every input schema | [tools.ts:260](../src/tools.ts#L260), [:277](../src/tools.ts#L277), [:288](../src/tools.ts#L288), [:291](../src/tools.ts#L291), [:299](../src/tools.ts#L299), [:310](../src/tools.ts#L310) |
| A result is sent as `structuredContent` and as its JSON in one text item | [tools.ts:547](../src/tools.ts#L547) |
| A tool error is sent as `isError: true` with one text item: the `ToolError` text, or the `errorText` of other errors | [tools.ts:549](../src/tools.ts#L549), [log.ts:45-49](../src/log.ts#L45-L49) |
| An aborted call ends with the result `Cancelled.`, `isError: true`, which the SDK discards | [tools.ts:494](../src/tools.ts#L494), [:506](../src/tools.ts#L506) |
| With a `progressToken` in `_meta`, `notifications/progress` is sent at the start of each wait and every 60 s, with `progress` 0, 1, 2, ... and a `message` | [tools.ts:1713-1723](../src/tools.ts#L1713-L1723), [messages.ts:305-309](../src/messages.ts#L305-L309) |
| The call is aborted when the client closes the response stream or sends `notifications/cancelled`; the action is kept | [src-BhnGYybN.cjs:6338-6341](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs#L6338-L6341), [:6425](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs#L6425) |
| A call with the same tool and action fields as an action in memory is attached to it. The key is the JSON array of the session number, the tool and the fields, with the session number `undefined` for `serve` | [tools.ts:1705-1707](../src/tools.ts#L1705-L1707), [:746-748](../src/tools.ts#L746-L748), [actions.ts:176-178](../src/actions.ts#L176-L178) |
| Messages are marked read after the last wait of a call that is not aborted | [tools.ts:792](../src/tools.ts#L792), [:937](../src/tools.ts#L937), [:1266](../src/tools.ts#L1266) |

Action fields of the key:

| Tool | Fields |
|---|---|
| `connect` | `target`<br>`incognito` (default `false`)<br>`message` (default `null`) |
| `send` | `chat`<br>`text` (default `null`)<br>`file` (default `null`)<br>`as` (default `null`)<br>`reply_to` (default `null`)<br>`mention` (default `null`) |
| `download` | `chat`<br>`item` |
| `leave` | `chat` |

Result status of the four action tools ([`actionResult`](../src/tools.ts#L761-L773)):

| Status | Meaning | Extra fields |
|---|---|---|
| `done` | performed | per tool |
| `failed` | failure of the card post, the counter write or the perform, or loss of the control chat | `error` |
| `rejected` | an approver sent `/reject` | |
| `expired` | no decision before the expiry time | |
| `known` | `connect` only: the target is an existing chat past the prepared state; no action | chat fields |
| `performing` | `download` only: `wait` passed after the approval; the download continues | |

### 1.2 `connect`

Handler: [`AgentTools.connect`](../src/tools.ts#L732-L755). Perform: [`performConnect`](../src/tools.ts#L811-L844). Schemas: [input](../src/tools.ts#L250-L261), [output](../src/tools.ts#L316-L326).

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
| `known` | `status` and the chat fields of the known chat. The plugin's rules for a known chat are in [4.1](#41-tools-in-the-plugin) | [tools.ts:737](../src/tools.ts#L737), [:750-755](../src/tools.ts#L750-L755) |
| `done`, chat unknown | `action`, `status`, `type`, `connected: false`, plan fields `name`, `peer_type`, `description`, `welcome`, `commands`, `known_as` when present | [tools.ts:758](../src/tools.ts#L758), [:1474-1480](../src/tools.ts#L1474-L1480) |
| `done`, chat known | `action`, `status`, chat fields, `messages` | [tools.ts:775-795](../src/tools.ts#L775-L795) |
| `failed` | `action`, `status`, `error` | [tools.ts:766](../src/tools.ts#L766) |
| `rejected`, `expired` | `action`, `status` | [tools.ts:766](../src/tools.ts#L766) |

Chat fields ([`chatFields`](../src/tools.ts#L1330-L1357)):

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

`messages`: message items among the last 50 items of the chat when the result is built, replaced by their collected copies, followed by the items collected since the connection; live items are left out; new items are marked read ([tools.ts:777](../src/tools.ts#L777), [:787-794](../src/tools.ts#L787-L794)).

Tool errors:

| Text | Condition | Code |
|---|---|---|
| `Not a SimpleX link or name: TARGET` | whitespace in `target`, or `invalidConnReq` from `apiConnectPlan` | [tools.ts:799](../src/tools.ts#L799), [:805](../src/tools.ts#L805) |
| an app error text, for example `Name not found: This SimpleX name is not registered. Please check the name.`, or `SimpleX error: CHAIN: DETAILS` | other errors of `apiConnectPlan`, or a plan of type `error` | [tools.ts:806](../src/tools.ts#L806), [:1401](../src/tools.ts#L1401), [log.ts:51-53](../src/log.ts#L51-L53) |
| `TARGET can't be joined now: PLAN.` | `invitationLink` other than `ok` and `known`<br>`contactAddress` other than `ok`, `known`, `contactViaAddress` and `connectingConfirmReconnect`<br>`groupLink` other than `ok`, `known` and `connectingConfirmReconnect` | [tools.ts:1392](../src/tools.ts#L1392), [:1405](../src/tools.ts#L1405), [:1423](../src/tools.ts#L1423), [:1445](../src/tools.ts#L1445) |
| `message: TARGET is not a contact or business address with short link data, or its contact exists without a connection.` | `message` given and the plan has `takesMessage: false` | [tools.ts:739](../src/tools.ts#L739) |
| refusals | see [1.9](#19-action-refusals) | |

`takesMessage` is `true` for a contact address with short link data, for a known prepared contact, and for a known prepared business group ([tools.ts:1396](../src/tools.ts#L1396), [:1416](../src/tools.ts#L1416), [:1434](../src/tools.ts#L1434)).

### 1.3 `send`

Handler: [`AgentTools.send`](../src/tools.ts#L902-L925). Result: [`sendResult`](../src/tools.ts#L927-L945). Perform: [`performSend`](../src/tools.ts#L947-L971). Schemas: [input](../src/tools.ts#L263-L278), [output](../src/tools.ts#L328-L340).

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

File types by extension, lower-cased ([tools.ts:178-187](../src/tools.ts#L178-L187), [`attachmentMedia`](../src/tools.ts#L1663-L1674)):

| Extension | Message type | Sent file | Preview and duration |
|---|---|---|---|
| `.jpg`, `.jpeg`, `.png` | image | `core.resizeImage(..., Image)`, written to `DATA/files/sent/`; the original when resizing fails | `core.resizeImage(..., MessagePreview)`; the fixed preview when it fails |
| `.gif`, `.webp` | image | unchanged | the fixed preview |
| `.mp4`, `.mov` | video | unchanged | the first frame from `ffmpeg`, resized as a preview; duration from `ffprobe`, rounded; the fixed preview and 0 when they fail |
| other, or `as: "file"` | file | unchanged | |

The fixed preview is a 16 × 9 mid-gray PNG ([tools.ts:167](../src/tools.ts#L167)). `ffmpeg` and `ffprobe` run with `-protocol_whitelist file -f mov`, 30 s and 64 MB of output each ([tools.ts:1685-1703](../src/tools.ts#L1685-L1703)).

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
| `text: required without file.` | neither `text` nor `file` | [tools.ts:907](../src/tools.ts#L907) |
| `as: applies only with file.` | `as` without `file` | [tools.ts:908](../src/tools.ts#L908) |
| `No chat CHAT. list shows the agent's chats.` | invalid reference, the control chat, or a store error when the chat is read | [tools.ts:1294-1303](../src/tools.ts#L1294-L1303), [:1598-1600](../src/tools.ts#L1598-L1600) |
| `The agent is not connected to CHAT and can't post; list shows connected: false.` | the chat is not connected | [tools.ts:910](../src/tools.ts#L910) |
| `The agent has role ROLE in CHAT and can't post.` | membership role `observer` or `relay` | [tools.ts:911-914](../src/tools.ts#L911-L914) |
| `reply_to: CHAT has no item N.` | the read of `around=N` fails, or the item is absent from it | [tools.ts:1308](../src/tools.ts#L1308) |
| `reply_to: item N in CHAT is not a message.` | the item's content is not `rcvMsgContent` or `sndMsgContent` | [tools.ts:1310](../src/tools.ts#L1310) |
| `mention: CHAT is a contact chat; mentions apply to groups and channels.` | `mention` with `@N` | [tools.ts:1316](../src/tools.ts#L1316) |
| `mention: CHAT has no member N.` | ID absent from `apiListMembers` | [tools.ts:1321](../src/tools.ts#L1321) |
| `mention: members N and M have the same name.` | two mentioned members with the same display name | [tools.ts:1324](../src/tools.ts#L1324) |
| `file: an absolute path is expected.`<br>`file: no file at FILE.`<br>`file: PATH is not a file.`<br>`file: PATH can't be read.` | the path, after symbolic links | [tools.ts:1616-1627](../src/tools.ts#L1616-L1627) |
| `file: PATH is refused: private keys and the server's own files are not sent.` | a file named `id_*`, `*.pem`, `*.key`, `*.p12` or `*.pfx`, or under `~/.ssh`, `~/.gnupg`, or the data folder or `~/.simplex-mcp` outside `DATA/files/`; each folder by its real path | [tools.ts:987-990](../src/tools.ts#L987-L990), [:996-1001](../src/tools.ts#L996-L1001) |
| `file: PATH is empty.` | size 0 | [tools.ts:991](../src/tools.ts#L991) |
| `text: too long for the message with its quote; shorten it.`<br>`text: too long for a message with this TYPE; shorten it.` | the message, as the core sends it, above 15602 bytes | [tools.ts:918](../src/tools.ts#L918), [:1571-1573](../src/tools.ts#L1571-L1573), [:1575-1591](../src/tools.ts#L1575-L1591) |
| refusals | see [1.9](#19-action-refusals) | |

The message is measured as 600 bytes plus the JSON of its content, the file's name, the quote and the mentioned names. The quote is the quoted text, or its file's name when the text is empty; a sent image or video quotes it as text, otherwise an image's preview is kept ([tools.ts:1575-1591](../src/tools.ts#L1575-L1591)).

The perform fails with `file: PATH changed after the request.` when the file's real path, size, inode or change time differs from the request ([tools.ts:1003-1008](../src/tools.ts#L1003-L1008)). A resized copy is deleted at `sndFileCompleteXFTP`, at `sndFileError` and after a failed send ([tools.ts:968](../src/tools.ts#L968), [:973-983](../src/tools.ts#L973-L983), [:688-690](../src/tools.ts#L688-L690)).

### 1.4 `receive`

Handler: [`AgentTools.receive`](../src/tools.ts#L1237-L1272). Schemas: [input](../src/tools.ts#L280-L289), [output](../src/tools.ts#L342-L350).

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

Tool errors: `No chat CHAT. list shows the agent's chats.` for an invalid reference, the control chat, or a store error of the first read, `initial` 1 ([tools.ts:1239](../src/tools.ts#L1239), [:1243-1245](../src/tools.ts#L1243-L1245)); other chat errors, a store error of a later page read included, as their error text ([log.ts:45-53](../src/log.ts#L45-L53)).

Messages are read by `receive` without an action or an approval.

### 1.5 `download`

Handler: [`AgentTools.download`](../src/tools.ts#L1084-L1104). Result: [`downloadResult`](../src/tools.ts#L1131-L1139). Perform: [`performDownload`](../src/tools.ts#L1141-L1145), [`downloadFile`](../src/tools.ts#L587-L614). Schemas: [input](../src/tools.ts#L302-L311), [output](../src/tools.ts#L397-L412).

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

FOLDER is `group-N` for `#N` and `contact-N` for `@N`; the extension is the file name's extension when it has 1 to 16 ASCII letters and digits ([messages.ts:215-227](../src/messages.ts#L215-L227)). `/forward` downloads a file to `DATA/forwarded/FOLDER/` under the same name; a later perform of the download moves it to the download path, after the copy for the post is made ([tools.ts:587-604](../src/tools.ts#L587-L604)).

Steps before the action:

| Step | Code |
|---|---|
| a file in status `rcvInvitation`, or an XFTP file: the file's servers from `apiGetChatItemInfo`; while none, the call waits for `rcvFileDescrReady` until `wait` seconds after the call, with the progress `The sender is uploading the file of item N in CHAT; waiting until TIME.` | [tools.ts:1106-1123](../src/tools.ts#L1106-L1123) |
| unknown servers: the file's servers that are not among the XFTP servers of `apiGetUserServers`, of no operator or of an enabled one, compared without their passwords | [tools.ts:1125-1129](../src/tools.ts#L1125-L1129) |
| without unknown servers, and for a file ID not in rejected files, the chat's files approval, read again, makes the action performed at once | [tools.ts:1099](../src/tools.ts#L1099) |

Perform: `apiReceiveFile(fileId, path, approvedRelays)`, for a file in status `rcvInvitation`. `approvedRelays` is true when the action's mode is `requests` and unknown servers are named on its card, and false otherwise. The core then checks the complete description ([tools.ts:610](../src/tools.ts#L610), [:1147-1156](../src/tools.ts#L1147-L1156)). The download ends at `rcvFileComplete`, `rcvFileError`, `rcvFileSndCancelled` or `chatItemUpdated` of its file, or at a check every 60 s ([tools.ts:626-648](../src/tools.ts#L626-L648), [:1158-1226](../src/tools.ts#L1158-L1226)).

Failure texts of the perform:

| Text | Condition | Code |
|---|---|---|
| the file's error, as in the file field | the file failed | [tools.ts:1187-1197](../src/tools.ts#L1187-L1197) |
| `The sender cancelled the file.` | `rcvFileSndCancelled` | [tools.ts:643-648](../src/tools.ts#L643-L648) |
| `The message or its chat was deleted.` | a check finds no message, contact or group | [tools.ts:172](../src/tools.ts#L172), [:1177-1185](../src/tools.ts#L1177-L1185) |
| `The download made no progress for 30 minutes and was cancelled.` | no rise of `rcvProgress` for 30 minutes; the file is cancelled | [tools.ts:176](../src/tools.ts#L176), [:1170-1174](../src/tools.ts#L1170-L1174) |
| `The received file is not in the agent's files folder.` | the file is received and neither the download path nor the forwarded path has a file | [messages.ts:14](../src/messages.ts#L14), [tools.ts:1199-1203](../src/tools.ts#L1199-L1203) |
| `Your owner deleted the files of this chat.` | `/delete CHAT files` | [tools.ts:173](../src/tools.ts#L173), [:1034-1052](../src/tools.ts#L1034-L1052) |
| `The file is on unknown servers that were not approved.` | a check reads an accepted file in status `rcvInvitation`: the core returned it to the offer when its description completed | [tools.ts:174](../src/tools.ts#L174), [:1166](../src/tools.ts#L1166) |
| `Unknown servers! Without Tor or VPN, your IP address will be visible to these XFTP relays: HOST, HOST.` | the core refuses the servers of the complete description at the acceptance | [log.ts:78-79](../src/log.ts#L78-L79) |

Tool errors:

| Text | Condition | Code |
|---|---|---|
| `No chat CHAT. list shows the agent's chats.` | invalid reference, the control chat, or a store error when the chat is read | [tools.ts:1089](../src/tools.ts#L1089) |
| `item: CHAT has no item N.` | the item is not found | [tools.ts:1091](../src/tools.ts#L1091) |
| `item: item N in CHAT has no received file.` | the item is not received or has no file | [tools.ts:1093](../src/tools.ts#L1093) |
| `item: the file of item N in CHAT failed: ERROR` | the file field has status `failed` | [tools.ts:1094-1095](../src/tools.ts#L1094-L1095) |
| `item: the sender has not uploaded the file of item N in CHAT yet; call download again later.` | no servers by the end of `wait`, also for a file invited without a description, whose servers the core does not list; no action is created | [tools.ts:1097](../src/tools.ts#L1097), [:1121](../src/tools.ts#L1121) |
| refusals | see [1.9](#19-action-refusals) | |

A file ID whose download returned `rejected` or `expired` shows status `rejected` until a download of it is done or the server restarts ([tools.ts:1136-1137](../src/tools.ts#L1136-L1137)).

### 1.6 `list`

Handler: [`AgentTools.listChats`](../src/tools.ts#L1274-L1282). Schemas: [input](../src/tools.ts#L291), [output](../src/tools.ts#L352-L384).

Input: an empty object.

Output:

| Field | Item fields |
|---|---|
| `chats` | `chat`<br>`type`<br>`name`<br>`simplex_name`, when verified<br>`peer_type`, when present<br>`incognito`<br>`known_as`, when incognito<br>`connected`<br>`unread`: `chatStats.unreadCount`<br>`approved: true`, only under a standing approval of messages in mode `requests` |
| `actions` | `action`<br>`tool`<br>`input`: the input of the creating call<br>`state`<br>`open`: `true` while a call waits on it |

`chats`: the contacts and groups of the main conversations from one read, `apiGetChats(USER, last 1000000)`, except the control chat. In the plugin, only the chats of the calling session are listed ([tools.ts:1284-1292](../src/tools.ts#L1284-L1292), [:577-579](../src/tools.ts#L577-L579)). `actions`: the actions in memory of the calling session other than its mode and tool-use cards, or all actions for `serve` ([tools.ts:1280](../src/tools.ts#L1280)).

### 1.7 `leave`

Handler: [`AgentTools.leave`](../src/tools.ts#L1060-L1068). Perform: [`leaveChat`](../src/tools.ts#L1074-L1082). Schemas: [input](../src/tools.ts#L293-L300), [output](../src/tools.ts#L386-L395).

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

Tool errors: `No chat CHAT. list shows the agent's chats.` when no action with the same key is in memory and the chat is invalid, the control chat or unknown ([tools.ts:1063-1065](../src/tools.ts#L1063-L1065)); refusals.

### 1.8 Message, file and command objects

Message ([`Message`](../src/messages.ts#L16-L28), [`toMessage`](../src/messages.ts#L174-L192), [schema](../src/tools.ts#L189-L222)):

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

Only received message items (`rcvMsgContent`) are returned by `send` and `receive`; `connect` also returns sent ones (`sndMsgContent`) ([messages.ts:101](../src/messages.ts#L101), [tools.ts:787](../src/tools.ts#L787)).

Command ([`commandsOf`](../src/tools.ts#L1553-L1560), [schema](../src/tools.ts#L224-L234)):

| Form | Fields |
|---|---|
| command | `keyword`<br>`label`<br>`params`, when present |
| menu | `label`<br>`commands`: commands and menus |

### 1.9 Action refusals

The plugin's refusals are in [4.4](#44-tool-results-in-the-plugin). Refusals of a new `connect`, `send`, `download` or `leave` action of `serve`, first match ([actions.ts:200-205](../src/actions.ts#L200-L205), [control.ts:150-165](../src/control.ts#L150-L165)):

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

Largest card form ([control.ts:762-770](../src/control.ts#L762-L770)): the maximum of the creation form, the `done` form and a `failed` form with an error of 600 × `€`, with the action's own decider or the longest current approver name; each form is measured as JSON-escaped UTF-8: `"`, `\`, `\n`, `\r`, `\t` 2 bytes, other characters below U+0020 6 bytes.

The link texts of a `connect` card are its description and welcome lines. These lines, and the input preview lines of a tool-use card in the plugin, are fitted into 15000 bytes minus the largest form without them ([control.ts:153-154](../src/control.ts#L153-L154), [:784-798](../src/control.ts#L784-L798)). Lines are kept while they fit. The next line is cut to the bytes left and ends in `…`, and later lines are left out. Both link texts are whole in the tool result.

---

## 2. Agent control chat

### 2.1 Commands and replies

Accepted text ([`parseCommand`](../src/control.ts#L672-L677), [`ownerRequest`](../src/control.ts#L846-L861)):

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

The server reads commands from members' messages in the main conversation of the control chat ([serve.ts:129](../src/serve.ts#L129), [control.ts:216-226](../src/control.ts#L216-L226)). It answers a command with a reply that quotes it, one command after another, and it runs the expiry check before each command ([control.ts:224](../src/control.ts#L224), [:247](../src/control.ts#L247)).

| Request | Condition | Reply | Code |
|---|---|---|---|
| one-line `/` text outside the requests | any sender | `Commands: /approve, /reject, /receive, /forward, /revoke, /leave, /delete, /pending, /list. Each card shows its commands.` The reply in a plugin session group is in [4.3](#43-session-groups) | [control.ts:19-20](../src/control.ts#L19-L20), [:249](../src/control.ts#L249) |
| any request | sender not an approver, `--owners` absent | `Only owners approve actions.` | [control.ts:250](../src/control.ts#L250) |
| any request | sender not an approver, `--owners` given | `Only the owners listed in --owners approve actions.` | [control.ts:250](../src/control.ts#L250) |
| `approve`, `reject`, `receive`, `forward` with N | action N absent, not `pending`, or without a card | `Action N is not pending.` | [control.ts:277](../src/control.ts#L277) |
| the same with CHAT | action N is a `connect`, or for another chat | `Action N is not for CHAT.` | [control.ts:278](../src/control.ts#L278) |
| `approve` | action N is a download | `Action N is a download: COMMANDS.`, COMMANDS the card's command line | [control.ts:284](../src/control.ts#L284) |
| `receive`, `forward`, `receive CHAT N all` | action N is not a download | `Action N is not a download.` | [control.ts:282](../src/control.ts#L282) |
| `approve`, `reject`, `receive`, `forward` | accepted | none; the card is edited at the outcome; `forward` posts the file, as a reply to the card when the message with the card as its quote is within 15602 bytes, and the card stays pending | [control.ts:279-288](../src/control.ts#L279-L288), [:315-334](../src/control.ts#L315-L334) |
| `approve CHAT all` | mode `host`; nothing is stored | `Approvals apply in mode requests; this server runs in mode host.` | [control.ts:259](../src/control.ts#L259) |
| `approve CHAT all`, `receive CHAT N all` | CHAT unknown or the control chat | `No chat CHAT. /list shows the agent's chats.` | [control.ts:296](../src/control.ts#L296) |
| `approve CHAT all` | accepted; the pending sends of CHAT with a card are approved | `Approved for KIND NAME (CHAT) by APPROVER. /'revoke CHAT'` | [control.ts:293-304](../src/control.ts#L293-L304) |
| `receive CHAT N all` | accepted; download N is performed, and the other pending downloads of CHAT stay pending | `Files approved for KIND NAME (CHAT) by APPROVER. /'receive CHAT off'` | [control.ts:287](../src/control.ts#L287), [:301-303](../src/control.ts#L301-L303) |
| `revoke CHAT` | CHAT unknown, or without a standing approval | `Messages to CHAT have no standing approval.` | [control.ts:309](../src/control.ts#L309) |
| `revoke CHAT` | accepted | `Messages to KIND NAME (CHAT) need approval again.` | [control.ts:312](../src/control.ts#L312) |
| `receive CHAT off` | CHAT unknown, or without a files approval | `Files from CHAT have no standing approval.` | [control.ts:309](../src/control.ts#L309) |
| `receive CHAT off` | accepted | `Files from KIND NAME (CHAT) need approval again.` | [control.ts:312](../src/control.ts#L312) |
| `leave CHAT` | CHAT unknown or the control chat | `No chat CHAT. /list shows the agent's chats.` | [control.ts:338](../src/control.ts#L338) |
| `leave CHAT` | accepted, at once in both modes; the pending actions of CHAT with a card are rejected by the sender | `Left KIND NAME (CHAT), requested by NAME.`, or `Deleted ...` for a contact; logged too | [control.ts:336-350](../src/control.ts#L336-L350) |
| `delete CHAT files` | the chat's downloads in memory fail and are cancelled; its other files in status `rcvAccepted`, `rcvTransfer` or `rcvWarning`, such as downloads the core continues after a restart, are cancelled; the folder of CHAT and its forwarded folder are deleted, for a current or a left chat | `Deleted K files from CHAT, SIZE.` (`1 file`), or `No files from CHAT.` | [control.ts:357-360](../src/control.ts#L357-L360), [tools.ts:1034-1052](../src/tools.ts#L1034-L1052), [messages.ts:275-283](../src/messages.ts#L275-L283) |
| `pending` | no pending action with a card | `No pending actions.` | [control.ts:254](../src/control.ts#L254) |
| `pending` | otherwise | one line per action: `N  WHAT  expires YYYY-MM-DD HH:MM UTC` | [control.ts:253](../src/control.ts#L253), [:752-754](../src/control.ts#L752-L754) |
| `list` | no chats | `No chats.` | [control.ts:405](../src/control.ts#L405) |
| `list` | otherwise | one line per chat of the 100 most recent: `CHAT  KIND NAME`, then `, approved for this chat by APPROVER`, `, files approved by APPROVER` and `, not connected` when applicable, cut to 100 bytes with `…`; then `K older chats are not shown.` (`1 older chat is not shown.`) | [control.ts:393-406](../src/control.ts#L393-L406) |

KIND is `contact`, `bot`, `agent`, `business`, `group` or `channel` ([tools.ts:1499-1506](../src/tools.ts#L1499-L1506)). WHAT is `connect to SUBJECT`, `send to SUBJECT`, `download NAME from SUBJECT`, `delete SUBJECT` or `leave SUBJECT` ([actions.ts:382-396](../src/actions.ts#L382-L396)). SIZE is in bytes, KB, MB or GB, by 1000, with one decimal ([control.ts:810-813](../src/control.ts#L810-L813)). Names are shown by `safeName` ([control.ts:827-830](../src/control.ts#L827-L830)).

A standing approval is stored in the chat's custom data, `simplexMcp` for messages and `simplexMcpFiles` for files, as `{approvedBy, approvedById, approvedAt}`; it applies in mode `requests` while member `approvedById` is an approver ([control.ts:27-30](../src/control.ts#L27-L30), [:167-170](../src/control.ts#L167-L170)).

### 2.2 Cards

Card text: request lines, then one last line ([`cardText`](../src/control.ts#L679-L698), [`requestLines`](../src/control.ts#L700-L733)).

Request lines:

| Tool | First line | Further lines |
|---|---|---|
| `connect` | `I want to connect to SUBJECT, as myself.` or `, incognito.`<br>done and connected: `I connected to ...`<br>done and not connected: `I asked to connect to ...` | link description, one code span per line<br>welcome, one code span per line<br>both cut to fit the card ([1.9](#19-action-refusals))<br>with `message`: `With the message:` and its lines |
| `send` | `I want to send a message to SUBJECT:`, with `, where I am incognito` before the colon in incognito chats<br>done: `I sent a message to ...` | text, one code span per line<br>with `file`: ``attaching TYPE `PATH`, SIZE``<br>`replying to NAME: QUOTE` with `reply_to`, the quote cut to 100 characters<br>`mentioning @A, @'B C'` with `mention` |
| `download` | `I want to download a file from SENDER in KIND NAME (CHAT):`, without `SENDER in ` outside groups<br>done: `I downloaded a file from ...` | ``` `NAME`, SIZE ```<br>an XFTP file: `Servers: known.`, or ``Servers: `HOST`, `HOST`, unknown: downloading shows them this computer's IP address.`` (`it` for one) |
| `leave` | `I want to delete KIND NAME (@N).` or `I want to leave KIND NAME (#N).`<br>done: `I deleted ...` or `I left ...` | |

SUBJECT is `KIND NAME`, or `an unnamed KIND`, followed by ` (SIMPLEX_NAME)` when known ([actions.ts:377-380](../src/actions.ts#L377-L380)). For `connect`, SIMPLEX_NAME is the target when it is a name; for `send`, the chat's verified SimpleX name.

A card of a `send` with an image or video file, or of a `download` of an image or video, is an image or video message. Its image is the file's preview, and its caption is the card text. The preview is resized when it is too large for the card, and the card is text when the preview can't be resized ([control.ts:546-566](../src/control.ts#L546-L566)).

In a plugin session group, the core can refuse a card with `feature not allowed NAME` ([control.ts:521-544](../src/control.ts#L521-L544), [:194](../src/control.ts#L194), [:756-760](../src/control.ts#L756-L760), [tools.ts:1607-1614](../src/tools.ts#L1607-L1614), [actions.ts:338-349](../src/actions.ts#L338-L349), [:213](../src/actions.ts#L213), [:266](../src/actions.ts#L266)):
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

DECIDER is `Performed without approval (mode host)` in mode `host`, `Approved for this chat by NAME` for a standing action, and `Approved by NAME` otherwise ([control.ts:681](../src/control.ts#L681)).

Command lines ([`commandsLine`](../src/control.ts#L735-L750)):

| Tool | Command line |
|---|---|
| `connect` | `/'approve N' or /'reject N'` |
| `send` | `/'approve CHAT N', /'approve CHAT all' or /'reject CHAT N'` |
| `download` | `/'receive CHAT N', /'receive CHAT N all', /'forward CHAT N' or /'reject CHAT N'`, without `/'receive CHAT N all'` for unknown servers |
| `leave` | `/'approve CHAT N' or /'reject CHAT N'` |

The app shows `/'approve N'` as the tappable command `/approve N` ([PLAN, What the owner sees](../PLAN.md#what-the-owner-sees-the-agent-control-chat)).

An action performed without a card, in mode `host` or under a standing approval, gets its outcome as a new message; a `download` without a card gets none ([control.ts:190-197](../src/control.ts#L190-L197), [:601-603](../src/control.ts#L601-L603)).

### 2.3 Other posts

| Situation | Text | Code |
|---|---|---|
| `send` with a refused file; logged too | ``Refused to send `PATH` to KIND NAME (CHAT): private keys and the server's own files are not sent.`` | [control.ts:199-203](../src/control.ts#L199-L203) |
| `sndFileError` of a sent file; logged too | ``The file `NAME` was not sent to KIND NAME (CHAT): `REASON`.`` | [control.ts:205-214](../src/control.ts#L205-L214), [serve.ts:133](../src/serve.ts#L133) |
| `/forward` of a file that is not received or not posted | ``Action N: the file was not received: `ERROR`.`` | [control.ts:328-331](../src/control.ts#L328-L331) |
| `/forward` in a plugin session group, when the core refuses the post with `feature not allowed NAME`, as for `Files and media` or `Voice messages` | `Action N: NAME are off in the group of this session; its owners can turn them on in the group's preferences.` | [control.ts:328-331](../src/control.ts#L328-L331), [tools.ts:1607-1614](../src/tools.ts#L1607-L1614) |
| after the reply to `/leave CHAT`, and after the outcome of a done `leave`, when the folder of CHAT has files | ``Files from KIND NAME (CHAT) are kept in `FOLDER`: K files, SIZE. /'delete CHAT files'`` | [control.ts:195-196](../src/control.ts#L195-L196), [:347-355](../src/control.ts#L347-L355) |

### 2.4 Membership messages

Posted in the chat and logged; in the chat, names are shown by `safeName` and the `--owners ...` part is a code span; in the log, names are folded to one line ([control.ts:507-510](../src/control.ts#L507-L510), [:90-91](../src/control.ts#L90-L91)).

| Situation | Text | Code |
|---|---|---|
| owner joined, `--owners` absent | `I'm your agent. I ask here before I connect, send, download a file or leave a chat.` (chat only) | [control.ts:36](../src/control.ts#L36), [:455](../src/control.ts#L455) |
| member joined, `--owners` without a list | `NAME joined (member ID). Restart the server with --owners ENTRIES` | [control.ts:460-463](../src/control.ts#L460-L463) |
| start with `--owners` without a list and humans present | `A joined (member ID). B joined (member ID). Restart the server with --owners ENTRIES` | [control.ts:427-430](../src/control.ts#L427-L430) |
| a human became owner outside the list | `NAME (member ID) is now an owner and can't approve actions yet. Restart the server with --owners ENTRIES` | [control.ts:468-472](../src/control.ts#L468-L472) |
| a listed owner stopped being a human owner | `NAME (member ID) is no longer an owner. Restart the server with --owners ENTRIES`<br>none remains: `NAME (member ID) is no longer an owner. No owners remain: restart the server with --reset-owners --owners` | [control.ts:473-478](../src/control.ts#L473-L478) |

ENTRIES: `ID:NAME` of each human, or of each human owner, joined by commas; an entry with characters other than letters, digits, `_` and `-` is quoted for the shell ([control.ts:909-919](../src/control.ts#L909-L919)).

### 2.5 Group profile

| Field | Value | Code |
|---|---|---|
| display name | the agent's display name, updated at start when it differs from the display name in the group's profile read at the start. The other fields of that profile are kept, as in 0.2 | [control.ts:117-121](../src/control.ts#L117-L121), [:133-140](../src/control.ts#L133-L140), [:637-640](../src/control.ts#L637-L640) |
| full name | empty | [control.ts:618](../src/control.ts#L618) |
| description | `Supervise your AI agent here: approve or reject its connections and messages with one tap.` | [control.ts:619](../src/control.ts#L619) |
| history | on | [control.ts:620](../src/control.ts#L620) |
| command menu | `/pending` "Pending actions"<br>`/list` "Chats and approvals"<br>`/leave` "Leave a chat", params `<chat ID>`<br>updated at start when it differs | [control.ts:13-17](../src/control.ts#L13-L17), [:117-121](../src/control.ts#L117-L121) |

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
| 2 | `--owners: member ENTRY is not in the agent control chat. Start with --reset-owners --owners, or use the IDs of current members.` | [control.ts:436](../src/control.ts#L436) |
| 1 | `Another server uses DATA (PID N). If no server runs, remove DATA/lock.` | [files.ts:112-113](../src/files.ts#L112-L113), [:178-180](../src/files.ts#L178-L180) |
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
| control chat created | `Agent control chat "DISPLAY" created.` | [control.ts:622](../src/control.ts#L622) |
| link opened | `Join the agent control chat in the SimpleX app. The link works once:`<br>QR code<br>link<br>small QR code: `If the QR code does not scan, restart with --qr-code large, or paste the link into the app.`<br>role line | [control.ts:486-496](../src/control.ts#L486-L496) |
| listening | `Listening on http://127.0.0.1:PORT/mcp` | [serve.ts:122](../src/serve.ts#L122) |
| owner setup done | `Add the agent to Claude Code:`<br>`  npx -y @simplex-chat/mcp register NAME [--data DATA]` | [serve.ts:157-159](../src/serve.ts#L157-L159) |
| owner joined | `NAME joined (member ID) and is now an owner. The link is deleted.` | [control.ts:456](../src/control.ts#L456) |
| listed owners set | `Owners: ENTRIES.` | [control.ts:447](../src/control.ts#L447) |
| approvers gone, `--owners` absent | `No owners in the agent control chat.` | [control.ts:37](../src/control.ts#L37), [:240](../src/control.ts#L240), [:423](../src/control.ts#L423) |
| control chat lost | `The agent lost its control chat. Restart the server with --reset-owners.` | [control.ts:483](../src/control.ts#L483) |
| action created | `Action N: WHAT` with `, as itself` or `, incognito` for `connect` and ` (CHAT)` for the other tools | [actions.ts:209-210](../src/actions.ts#L209-L210), [:382-396](../src/actions.ts#L382-L396) |
| counter write or card post failed | `Action N: failed: ERROR.` | [actions.ts:326](../src/actions.ts#L326) |
| perform ended, not interrupted by the stop, mode `requests` | `Action N approved by NAME: RESULT` with RESULT `sent.`, `downloaded.`, `deleted.`, `left.`, `connected.`, `connection requested.` or `failed: ERROR.` | [actions.ts:347](../src/actions.ts#L347), [:402-412](../src/actions.ts#L402-L412) |
| perform ended, not interrupted by the stop, mode `host` | `Action N: performed without approval (mode host).`, or `...; failed: ERROR.` | [actions.ts:404](../src/actions.ts#L404) |
| rejected | `Action N rejected by NAME.` | [actions.ts:272](../src/actions.ts#L272) |
| expired | `Action N expired.` | [actions.ts:310](../src/actions.ts#L310) |
| `/leave CHAT` | `Left KIND NAME (CHAT), requested by NAME.` or `Deleted ...` | [control.ts:342](../src/control.ts#L342) |
| refused file, file not sent | as in [2.3](#23-other-posts), names and paths on one line | [control.ts:201](../src/control.ts#L201), [:212](../src/control.ts#L212) |
| stop | `Cancelled K action.` or `Cancelled K actions.` when K > 0<br>`Stopping: sending K cancelled card(s), up to S s. Ctrl-C to stop now.` when cards were edited or posted and S > 0<br>`Stopped.` | [serve.ts:148](../src/serve.ts#L148), [:161-175](../src/serve.ts#L161-L175), [:152](../src/serve.ts#L152) |
| membership messages | see [2.4](#24-membership-messages) | |

In the plugin's lines with `failed: ERROR.`, logged for the actions of a session, the last period is added only when ERROR does not end with one ([actions.ts:427-429](../src/actions.ts#L427-L429)). In `serve`, a period is added after every ERROR of these lines, and after every REASON of the line of a file not sent, as in 0.2.

stderr: `CONTEXT: ERROR` from `logError` ([log.ts:41-43](../src/log.ts#L41-L43)), with CONTEXT one of `action N`, `action N card`, `action N card image`, `action N file`, `control chat command`, `EVENT in the agent control chat`, `EVENT event`, `refused file`, `file not sent`, `download of file N`, `file N`, `moving file N`, `cancelling file N`, `resizing PATH`, `preview`, `MCP`, `HTTP request`, `stop`, `simplex-mcp`. The library's download lines are written to stderr.

---

## 4. Claude Code plugin

### 4.1 Tools in the plugin

| Rule | Code |
|---|---|
| The six tools of [1](#1-tools) and `reply` are registered on the session's `McpServer`. Claude Code names them `mcp__plugin_simplex_simplex__NAME` | [session.ts:57](../src/session.ts#L57), [hooks.json:4](../hooks/hooks.json#L4) |
| The four action tools are registered with `_meta: {"anthropic/requiresUserInteraction": true}` when the option `approve` is `host` at the start of the session process | [session.ts:57](../src/session.ts#L57), [tools.ts:504](../src/tools.ts#L504) |
| Instructions: those of [1.1](#11-common-rules), with the display name `Claude` and the host `SimpleX service`, followed by the channel instructions of [4.5](#45-channel) | [session.ts:21](../src/session.ts#L21), [:52](../src/session.ts#L52), [tools.ts:441-452](../src/tools.ts#L441-L452) |
| `send`, `receive`, `download`, `leave` and `list` see only the session's own chats. For another session's chat, `send`, `receive`, `download` and `leave` return `No chat CHAT. list shows the agent's chats.` | [tools.ts:577-579](../src/tools.ts#L577-L579), [:1294-1303](../src/tools.ts#L1294-L1303) |
| At `connect`, a known chat without an owner becomes the session's chat. `connect` to a chat of another session returns the tool error `The chat of TARGET belongs to another session; an admin or owner of that session's group can give it to this session with /give.` | [tools.ts:750-755](../src/tools.ts#L750-L755) |
| Actions are numbered from 1 in each session group. The session number is part of the action key, see [1.1](#11-common-rules) | [hub.ts:75](../src/hub.ts#L75), [tools.ts:1705-1707](../src/tools.ts#L1705-L1707) |
| Downloads are saved under `ROOT/_sessions/ID/files/` | [hub.ts:90](../src/hub.ts#L90), [messages.ts:225-227](../src/messages.ts#L225-L227) |
| A run is not interactive when `CLAUDE_CODE_SESSION_ATTENDED` is `0`, which Claude Code sets for the hooks of a run nobody attends, or when `CLAUDE_CODE_ENTRYPOINT` is `sdk-cli`, which Claude Code sets for `claude -p`, or `sdk-ts` or `sdk-py`, for the Agent SDK. Such a run has no session. Its session process does not connect to the service, and every tool returns `This Claude Code run is not interactive, so it has no SimpleX group; the SimpleX tools work in interactive sessions.`, even when an option is invalid. Its hooks return `{}`, and the turn hooks send no request | [config.ts:22-32](../src/config.ts#L22-L32), [session.ts:15](../src/session.ts#L15), [:45-51](../src/session.ts#L45-L51), [:66](../src/session.ts#L66), [hook.ts:49-52](../src/hook.ts#L49-L52), [:82](../src/hook.ts#L82) |
| A run with `CLAUDE_CODE_SESSION_ATTENDED` `0` and an entrypoint other than those three is, for example, a `claude -p` run started by a command of a VS Code or desktop-app session. Its SessionStart hook starts the service and sends `/title` with `unattended` for its `CLAUDE_PID`. The session server of that run gets the values of the attended session, `1` and its entrypoint. The service refuses it with status 403 when it registers from that process within 10 s of the mark. The refused server then answers every call with the same text, relays no permission request and registers no more | [hook.ts:69-79](../src/hook.ts#L69-L79), [service.ts:237-248](../src/service.ts#L237-L248), [session.ts:79-80](../src/session.ts#L79-L80), [:92](../src/session.ts#L92), [:115](../src/session.ts#L115), [:127](../src/session.ts#L127), [:162-165](../src/session.ts#L162-L165) |

`reply` ([`replyTool`](../src/tools.ts#L462-L483), [`AgentTools.reply`](../src/tools.ts#L713-L730)):

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

Sources: [tools.ts:713-730](../src/tools.ts#L713-L730), [:1607-1614](../src/tools.ts#L1607-L1614), [:549](../src/tools.ts#L549), [hub.ts:93](../src/hub.ts#L93). The core checks the preference at the send, and only owners change it. Claude changes only its own fields of a group's profile, so the owners' preferences stay ([control.ts:115-140](../src/control.ts#L115-L140), [:632-640](../src/control.ts#L632-L640), [main-group.ts:57-66](../src/main-group.ts#L57-L66)). The profile updates are in [4.2](#42-main-group) and [4.3](#43-session-groups).

### 4.2 Main group

Commands from any member, one line `/KEYWORD PARAMS` as in [2.1](#21-commands-and-replies) ([`MainGroup.command`](../src/main-group.ts#L111-L115)):

| Request | Condition | Reply | Code |
|---|---|---|---|
| `/sessions` | no session | `No sessions.` | [hub.ts:430-433](../src/hub.ts#L430-L433) |
| `/sessions` | otherwise | one line per session of the hub: `N · NAME`, with `, closed` when the service has no entry for it | [hub.ts:430-433](../src/hub.ts#L430-L433) |
| `/join N` | no session N | `No session N. /sessions lists the sessions.` | [hub.ts:437](../src/hub.ts#L437) |
| `/join N` | the sender is a member of its group | `You are a member of "N · NAME".` | [hub.ts:438-443](../src/hub.ts#L438-L443) |
| `/join N` | otherwise: the sender is invited through the Main group, as `owner` when the sender is an owner of the Main group, else as `admin` | `Sent you an invitation to "N · NAME".` | [hub.ts:439](../src/hub.ts#L439), [:216-218](../src/hub.ts#L216-L218) |
| other one-line `/` text | | `Unknown command.` | [main-group.ts:14](../src/main-group.ts#L14), [:114](../src/main-group.ts#L114) |

Posts:

| Moment | Text | Code |
|---|---|---|
| the first owner joined | `I'm Claude. Each Claude Code session has its own group, and you get its invitation here. /sessions lists the sessions; /join N joins the group of session N.` | [main-group.ts:13](../src/main-group.ts#L13), [:138](../src/main-group.ts#L138) |
| each `joinedGroupMember` or `connectedToGroupMember` of a human who is found among the members read again. For the first owner, it is posted after the greeting | `NAME joined. They can join the group of any Claude Code session here, prompt Claude and approve its actions.` | [main-group.ts:17](../src/main-group.ts#L17), [:127-128](../src/main-group.ts#L127-L128) |

The session groups stay as they are when the membership or a role in the Main group is changed, and when the Main group is replaced by a reset of the owners. Members invited through the old Main group keep their roles in the session groups, with the rights of those roles ([main-group.ts:117-132](../src/main-group.ts#L117-L132), [:99-109](../src/main-group.ts#L99-L109), [hub.ts:311-313](../src/hub.ts#L311-L313)).

Group profile ([`createMainGroup`](../src/main-group.ts#L195-L205), [`load`](../src/main-group.ts#L57-L66)):

| Field | Value |
|---|---|
| display name | option `main_group`, updated when it differs |
| short description | `Everyone in this group can join the groups of your Claude Code sessions, prompt Claude and approve what it does.`, set when the group is created |
| description | `Everyone in this group can join the groups of your Claude Code sessions, prompt Claude and approve what it does. Invite only people you trust with that.`, set when the group is created |
| history | on |
| direct messages | on for the role `owner` |
| command menu | `/sessions` "Claude Code sessions"<br>`/join` "Join the group of a session", params `<session number>` |
| custom data | `{simplexMcpMain: true}` |

Claude updates only the display name and the command menu, starting each time from the group's current profile in the chat database. So the short description, the description and the preferences that owners set stay ([main-group.ts:57-66](../src/main-group.ts#L57-L66), [control.ts:632-640](../src/control.ts#L632-L640)).

The hub's profile `Claude` posts in the group, with peer type `agent` ([hub.ts:18](../src/hub.ts#L18), [core.ts:24](../src/core.ts#L24)).

### 4.3 Session groups

Commands and replies are those of [2.1](#21-commands-and-replies), from approvers as defined in [architecture 8.5](architecture.md#85-approval-model), with these changes:

| Request | Condition | Reply | Code |
|---|---|---|---|
| any request | the sender is not an approver: a moderator, member, author or observer, or `User`. `Control` drops a request of `User` before this check. A moderator, member or author gets the reply of the last row instead | none, and nothing is performed | [control.ts:221](../src/control.ts#L221), [:250](../src/control.ts#L250) |
| `/give CHAT N` | CHAT is not a chat of this session | `No chat CHAT. /list shows the agent's chats.` | [control.ts:364](../src/control.ts#L364) |
| `/give CHAT N` | no session N | `No session N. /sessions in the Main group lists the sessions.` | [control.ts:366](../src/control.ts#L366) |
| `/give CHAT N` | accepted: CHAT's owner is set to session N, and the pending actions of CHAT with a card are rejected by the sender | `Gave KIND NAME (CHAT) to session N.` | [control.ts:367-372](../src/control.ts#L367-L372) |
| `/list`, `/revoke CHAT`, `/leave CHAT` | | only the chats of this session, even while the session is closed | [tools.ts:1284-1303](../src/tools.ts#L1284-L1303) |
| `/give CHAT N` in the control chat of `serve` | any sender | the help reply, as in 0.2 | [control.ts:249](../src/control.ts#L249), [:857](../src/control.ts#L857) |
| `/observer #G ID` | #G is not this group | `#G is not this group.` | [control.ts:376](../src/control.ts#L376) |
| `/observer #G ID` | member ID is neither current nor pending, as it is absent, gone, or invited and not joined yet | `No member ID in this group.` | [control.ts:377-379](../src/control.ts#L377-L379), [tools.ts:1536-1551](../src/tools.ts#L1536-L1551) |
| `/observer #G ID` | ID is Claude, the sender, a relay or a member pending approval or review | `NAME's role can't be changed.` | [control.ts:380-382](../src/control.ts#L380-L382) |
| `/observer #G ID` | the sender's role is below admin or below the member's role, as SimpleX requires for a role change | `Your role does not let you change the role of NAME.` | [control.ts:383](../src/control.ts#L383), [:880-882](../src/control.ts#L880-L882) |
| `/observer #G ID` | accepted: the member's role is set to `observer`, and the members are read again. The standing approvals that this member gave are kept in the chat's custom data, and are ignored until the member is an admin or owner again | `NAME is an observer and can't send messages.` | [control.ts:384-386](../src/control.ts#L384-L386), [:888-891](../src/control.ts#L888-L891), [:167-170](../src/control.ts#L167-L170) |
| one-line `/` text outside the requests | an admin or owner | `Unknown command.` | [control.ts:20](../src/control.ts#L20), [:249](../src/control.ts#L249) |
| any one-line `/` text | a moderator, member or author. The session replies before `Control`, and the text is passed to neither `Control` nor Claude | `Only admins and owners of this group can send / commands.`, quoting the message | [hub.ts:28](../src/hub.ts#L28), [:157](../src/hub.ts#L157), [:307-309](../src/hub.ts#L307-L309) |

The requests `give CHAT N` and `observer #G ID` of [2.1](#21-commands-and-replies) are parsed only in a plugin session group ([control.ts:857-858](../src/control.ts#L857-L858)). Approvers are the active members of the session group with the role `admin` or `owner`, other than Claude and `User`, however they joined. An observer can't send, because the core refuses an observer's message ([hub.ts:311-317](../src/hub.ts#L311-L317), [control.ts:43](../src/control.ts#L43), [:580-582](../src/control.ts#L580-L582), [:589-598](../src/control.ts#L589-L598)).

The hub handles other messages by these rules, in arrival order, one at a time ([hub.ts:153-160](../src/hub.ts#L153-L160), [:229-232](../src/hub.ts#L229-L232), [:303-313](../src/hub.ts#L303-L313), [control.ts:219-221](../src/control.ts#L219-L221)):

```
on a received message in the session group, by its text:
  one line "/KEYWORD ...", the sender neither an admin, an owner nor User
                                                           -> reply "Only admins and owners of this group can send / commands."
  one line "/KEYWORD ..."                                  -> a command, above; from User, ignored
  sender User                                              -> ignored
  other text                                               -> the prompt as sent
the sender's role is the role in the session group of the message's member
```

Any member who can send messages in the group can prompt Claude, however they joined: through the Main group, or added to the group directly.

The cards are those of [2.2](#22-cards), and two more:

| Tool | Request lines | Command line |
|---|---|---|
| `mode` | `I want to run in mode host: Claude Code asks you before each of my actions, and they are performed without approval here.`<br>done: `I run in mode host: ...` | `/'approve N' or /'reject N'` |
| `permission` | ``I want to use `TOOL`: `DESCRIPTION` ``, then the input preview, one code span per line, cut to fit the card ([1.9](#19-action-refusals)) | `/'approve N' or /'reject N'` |

`mode` and `permission` are exempt from the card size check ([control.ts:155](../src/control.ts#L155), [:700-706](../src/control.ts#L700-L706), [:735-740](../src/control.ts#L735-L740)). In `/pending`, their WHAT is `run in mode MODE` and `use TOOL` ([actions.ts:383-384](../src/actions.ts#L383-L384)). The last line of a `mode` or `permission` card is `Ended.` when the hub ends the card at a turn, a detach or the option `requests`, and `Expired.` when it expires ([actions.ts:237-241](../src/actions.ts#L237-L241), [control.ts:695-696](../src/control.ts#L695-L696)).

Posts:

| Poster | Text | Code |
|---|---|---|
| Claude | the final answer of each turn, in parts. The turn's prompt from the group, chosen by the hub from the turn records below, is quoted in the first part when the quote fits with the answer's first character. A changed short description is set after the answer | [hub.ts:180-201](../src/hub.ts#L180-L201), [:266-295](../src/hub.ts#L266-L295) |
| Claude | `Session N is not running.`, as a reply to a prompt when the session is not attached at the prompt's delivery. The hub delivers the prompt after the earlier prompts and after the prompt's own file. In the first 30 s after the service's start, the reply is posted only when the session does not attach by their end | [hub.ts:153-160](../src/hub.ts#L153-L160), [:234-248](../src/hub.ts#L234-L248), [:378](../src/hub.ts#L378) |
| Claude | `The file of this message was not received: ERROR`, as a reply to a prompt whose file was not received, see [4.5](#45-channel) | [hub.ts:234-258](../src/hub.ts#L234-L258) |
| Claude | the reply to the one-line `/` texts of the rules above, quoting the message | [hub.ts:28](../src/hub.ts#L28), [:157](../src/hub.ts#L157) |
| `User` | each prompt typed in the terminal, in parts. Blank prompts, prompts made only of this server's channel tags, the prompts pushed from the group and one-line `/` commands are left out. `User` posts in the group that is marked with the session's number | [hub.ts:184-195](../src/hub.ts#L184-L195), [user.ts:29-32](../src/user.ts#L29-L32) |
| Claude | a text in place of an answer or a terminal prompt that the core refuses with `feature not allowed NAME`, as `SimpleX links` for a SimpleX link. The text for an answer is `Claude's answer was not posted: NAME are off in the group of this session; its owners can turn them on in the group's preferences.`, quoting the turn's prompt as the answer would. A terminal prompt is replaced by `User's prompt was not posted: NAME are not allowed for User in the group of this session; its owners can allow them in the group's preferences.`, even while NAME is on only for roles above that of `User`. Claude is an owner of the group, so only the first text applies to its answers. The turn goes on | [hub.ts:195](../src/hub.ts#L195), [:277-287](../src/hub.ts#L277-L287), [:523-525](../src/hub.ts#L523-L525), [tools.ts:1607-1614](../src/tools.ts#L1607-L1614) |
| Claude | files sent by `reply`, at once | [tools.ts:713-730](../src/tools.ts#L713-L730), [hub.ts:93](../src/hub.ts#L93) |
| Claude | the rights of the member's role, below. They are posted once for each join and each role change of a member other than `User`, at its `joinedGroupMember`, `connectedToGroupMember` or `memberRole` | [hub.ts:220-227](../src/hub.ts#L220-L227), [:518](../src/hub.ts#L518), [control.ts:40-43](../src/control.ts#L40-L43), [:884-891](../src/control.ts#L884-L891) |

Rights posts, by the member's role in the session group:

| Role | Post |
|---|---|
| `owner` | `NAME is an owner and can send prompts to Claude and the group's commands, and approve tool use.` |
| `admin` | `NAME is an admin and can send prompts to Claude and the group's commands, and approve tool use. You can change role to observer to stop it: /'observer #G ID'` |
| `moderator`, `member`, `author` | `NAME is a member and can send prompts to Claude; the group's commands and approvals need the role admin or owner. You can change role to observer to stop it: /'observer #G ID'`, with `a moderator` or `an author` for those roles |
| `observer` | `NAME is an observer and can't send messages.` |

#G is the session group, and ID is the member. The core raises no `memberRole` event for a role change that Claude makes itself, with `/observer`. The reply to `/observer` is the observer's post instead.

Parts ([hub.ts:531-548](../src/hub.ts#L531-L548), [tools.ts:1575-1578](../src/tools.ts#L1575-L1578)): the hub cuts the text into the longest runs of whole characters that `fitsMessage` accepts as one message. `fitsMessage` is the measure of `send` in [1.3](#13-send), in UTF-8 bytes. The first part is measured with its quote. The hub puts up to 14962 bytes of text into a part without a quote: 14962 Latin, 7481 Cyrillic or 4987 CJK characters, fewer for characters escaped in JSON.

Claude can quote a message in its answer and its replies. When the core refuses such a post with `errorStore` `invalidQuote` or `chatItemNotFound`, as for a deleted prompt, or with `largeMsg`, as for a reply that quotes a message near the size limit, the post is sent again without the quote ([hub.ts:289-295](../src/hub.ts#L289-L295), [:550-553](../src/hub.ts#L550-L553)).

Turn records, one per `/turn` with `prompt`, are read and changed before the first await of each `/turn` ([hub.ts:54](../src/hub.ts#L54), [:180-186](../src/hub.ts#L180-L186), [:266-275](../src/hub.ts#L266-L275)):

```
on /turn {at, prompt}:
  item = oldest pushed prompt that prompt contains in a channel tag of this server,
         or that equals prompt and was pushed in the last 60 s; else null
  turns += {at, item}, unless at is below answeredAt; the matched pushed prompts are removed
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

Group profile ([`createSessionGroup`](../src/hub.ts#L477-L489), [`Control.load`](../src/control.ts#L115-L122), [`describe`](../src/control.ts#L129-L131), [`updateProfile`](../src/control.ts#L133-L140)):

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

The hub takes the model and the effort level of a `/turn` only when its `at` is not older than the `at` of the value it took last. It keeps the two times apart, one for the model and one for the effort level. The model of a `/title` comes with the `at` of its SessionStart hook, as described in [4.6](#46-socket-protocol). A request without `at` counts as the current time. So when a Stop request that waited for its transcript entry arrives after a later model switch, the model of the switch stays ([hub.ts:187-193](../src/hub.ts#L187-L193), [:260-264](../src/hub.ts#L260-L264)).

### 4.4 Tool results in the plugin

Refusals of a new action, first match, in place of those of [1.9](#19-action-refusals):

| Text | Condition | Code |
|---|---|---|
| `The SimpleX service is stopping.` | the service stops | [hub.ts:30](../src/hub.ts#L30) |
| `The group of this session is lost. Start a new Claude Code session.` | the session group was deleted, Claude was removed from it or lost its owner role | [hub.ts:30](../src/hub.ts#L30), [control.ts:863-878](../src/control.ts#L863-L878) |
| `20 actions are pending; wait for the approvers' decisions.` | 20 actions of the session in state `pending` | [hub.ts:30](../src/hub.ts#L30), [actions.ts:199-204](../src/actions.ts#L199-L204) |
| the texts of [1.9](#19-action-refusals) for 20 actions being performed and for the card size | as in 1.9, with the actions counted per session | [actions.ts:199-204](../src/actions.ts#L199-L204) |
| `The group of this session has no admin or owner to approve actions. Members of the Main group join it by /join N there; without members, run npx -y @simplex-chat/mcp@VERSION link in a terminal.`, with VERSION the plugin's version | the session group has no approver | [control.ts:39](../src/control.ts#L39), [:157](../src/control.ts#L157), [owner.ts:7](../src/owner.ts#L7) |

Errors of the session process ([session.ts:40-67](../src/session.ts#L40-L67), [:120-151](../src/session.ts#L120-L151), [:188-195](../src/session.ts#L188-L195)):

| Text | Condition |
|---|---|
| `This Claude Code run is not interactive, so it has no SimpleX group; the SimpleX tools work in interactive sessions.` | every call of a run that is not interactive, or of a session process that the service refused with status 403 |
| `Plugin option KEY: EXPECTED expected, got VALUE. Change it in /plugin > Installed > simplex > Configure options.` | every call after an invalid option at the session's start |
| `The SimpleX service stopped; repeat the call.` | the service is not running or is stopping (503), or the call's stream ended without a result |
| `Restart Claude Code to update the SimpleX plugin.` | the service speaks another protocol version |
| `The SimpleX service exited; see ROOT/service.log`, `The SimpleX service did not start in 5 minutes.`, or the service's error text | the service could not start |
| `Unknown session; restart the Claude Code session.` | the service has no folder for the session, even after one repeat |
| `Cancelled.` | the service returned `{result: null}` for a call that was not aborted |

The session server attaches again when its `/attach` ends or its start fails, a spawn error included, and first starts the service when none runs. It tries after 1 s, and after each failure twice as late, up to 30 s. It stops trying when its stdin closes, when the service refuses it with status 403, or when the service speaks another protocol version, the `ProtocolError` ([session.ts:88-112](../src/session.ts#L88-L112), [:177-183](../src/session.ts#L177-L183), [client.ts:17](../src/client.ts#L17), [:45](../src/client.ts#L45)).

### 4.5 Channel

Capabilities of the session server: `experimental: {"claude/channel": {}, "claude/channel/permission": {}}` ([session.ts:52](../src/session.ts#L52)).

| Direction | Method | Params | Code |
|---|---|---|---|
| to Claude Code | `notifications/claude/channel` | `content`: the prompt's text<br>`meta.from`: the sender's display name<br>`meta.item`: the item ID in the session group, as a string<br>`meta.file`: the path of the prompt's file, when it has one | [session.ts:198](../src/session.ts#L198) |
| to Claude Code | `notifications/claude/channel/permission` | `request_id`<br>`behavior`: `allow` or `deny` | [session.ts:199](../src/session.ts#L199) |
| from Claude Code | `notifications/claude/channel/permission_request` | `request_id`, `tool_name`, `description`, `input_preview`, all strings | [session.ts:22-23](../src/session.ts#L22-L23), [:58-62](../src/session.ts#L58-L62) |

Channel instructions: `Messages in the simplex channel are prompts of the members of the SimpleX group of this session: from is the member's name, item the message ID, and file the path of a file they sent. Your final answer of each turn reaches them in that group; reply sends them a file.` ([session.ts:21](../src/session.ts#L21)).

The session server negotiates the protocol revision 2025-11-25 and answers `server/discover` with `Method not found`, as `McpServer` of the SDK 2.3.1 does by default. Claude Code 2.1.319 registers no channel for a server that negotiates the modern revision 2026-07-28. A unit test of the session server checks both answers ([session.test.ts](../test/unit/session.test.ts)).

The prompts of a session, from the members of [4.3](#43-session-groups), are delivered one at a time, in arrival order ([hub.ts:159](../src/hub.ts#L159), [:303-305](../src/hub.ts#L303-L305), [:234-248](../src/hub.ts#L234-L248)). While a session is attached, a prompt's file is received without a card, with relays approved, into `ROOT/_sessions/ID/files/group/<file ID><extension>`. The prompt is pushed after `rcvFileComplete`. After `rcvFileError`, a failed receive or 10 minutes, it is pushed without `file`. The next prompt waits until then ([hub.ts:234-258](../src/hub.ts#L234-L258), [:209-214](../src/hub.ts#L209-L214)). Pushed prompts are kept until they are matched by a turn's `prompt`, at most 20 ([hub.ts:23](../src/hub.ts#L23), [:186](../src/hub.ts#L186), [:246](../src/hub.ts#L246), [:268-272](../src/hub.ts#L268-L272)).

### 4.6 Socket protocol

The service speaks HTTP/1.1 over `ROOT/service.sock`, the fallback path or the Windows pipe, with protocol version `1`. Every request is sent with the header `Authorization: Bearer TOKEN`, and bodies are JSON. Streamed replies are NDJSON, with the content type `application/x-ndjson`. Errors are `{"error": TEXT}` with a status other than 200. Types: [`protocol.ts`](../src/protocol.ts). Routes: [`Service.routes`](../src/service.ts#L137-L161). Only `/title`, `/link` and `/reset-owners` return a Main group link. `/reset-owners` returns the link of the new group.

| Route | Request | Reply | Code |
|---|---|---|---|
| `POST /session` | `{id, pid?, project, name?, config: {mainGroup, approve, expireHours}}`, `pid` the session server's parent process | `{number, displayName, version, protocol, owner: {owners}}`. For a `pid` marked unattended, the reply is status 403, see below. The mark is checked before and after the wait for the title of `pid` | [service.ts:201-216](../src/service.ts#L201-L216), [:245-248](../src/service.ts#L245-L248) |
| `POST /title` | `{id, pid?, at?, name?, project, model?, config, unattended?}`, `pid` from `CLAUDE_PID`, `at` the `performance.timeOrigin` of the SessionStart hook | `{mainGroup, owner: {owners, link?}, version, protocol}`, after the title is applied, or at once for a held title. With `unattended: true`, the reply comes at once, and no session is opened. Marks 10 s old and older are removed, `pid` is marked, and a server's wait for its title ends | [service.ts:218-262](../src/service.ts#L218-L262) |
| `POST /attach` | `{id}` | a stream held open: the first line `{version, protocol}`, then `{prompt}` and `{permission}` lines | [service.ts:304-324](../src/service.ts#L304-L324) |
| `POST /call` | `{id, tool, input}` | lines `{progress: TEXT}`, then one line `{result: CallToolResult or null}`. The call is aborted when the request is closed | [service.ts:326-345](../src/service.ts#L326-L345) |
| `POST /permission` | `{id, requestId, toolName, description, inputPreview}` | `{}` after the card's action is created | [service.ts:143-147](../src/service.ts#L143-L147) |
| `POST /turn` | `{id, pid?, at?, prompt?, answer?, model?, effort?}`, `at` the time the hook started | `{}` after the posts | [service.ts:148-152](../src/service.ts#L148-L152) |
| `GET /status` | none | `{version, protocol, mainGroup, owner: {owners}, sessions: [{number, name, attached}]}`, with the open sessions | [service.ts:424-427](../src/service.ts#L424-L427) |
| `GET /link` | none | `{owners, link?}`, the link while the Main group has no owner | [service.ts:154](../src/service.ts#L154), [hub.ts:391-394](../src/hub.ts#L391-L394) |
| `POST /reset-owners` | none | `{link}`, the link of the new Main group. The group is replaced at once, even while owners exist | [service.ts:155](../src/service.ts#L155), [hub.ts:402-404](../src/hub.ts#L402-L404) |
| `POST /stop` | none | `{}` after the stop sequence. The process exits after the reply | [service.ts:156-159](../src/service.ts#L156-L159) |

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

Session routes are `/session`, `/title`, `/attach`, `/call`, `/permission` and `/turn` ([service.ts:357-390](../src/service.ts#L357-L390)). Their ID is that of a session, or an alias of one ([service.ts:264-295](../src/service.ts#L264-L295)):

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

`/session` and `/title` without `unattended` apply `config` to the hub ([hub.ts:396-400](../src/hub.ts#L396-L400), [service.ts:213](../src/service.ts#L213), [:221](../src/service.ts#L221), [:232](../src/service.ts#L232), [:259](../src/service.ts#L259)):
- The hub sets the expiry of all sessions from `expireHours`.
- The hub renames the Main group when `mainGroup` differs.

`/session` requests the session's mode ([hub.ts:132-151](../src/hub.ts#L132-L151), [:319-322](../src/hub.ts#L319-L322), [:324-327](../src/hub.ts#L324-L327), [:75](../src/hub.ts#L75), [:518](../src/hub.ts#L518)):
- `requests` applies at once. The hub ends a pending mode card and clears `hostDeclined`.
- For `host`, the hub posts the mode card when the group has an approver and `hostDeclined` is unset. It posts the card at once, or after the next membership event of the group.
- When the mode card is rejected, the hub sets `hostDeclined` in session.json.
- The hub keeps the mode of an approved card, and a later `requests`, as `mode` in the session group's custom data. The session reads it when it opens.

`/title` opens or creates the session, and renames it when the name is not blank. It passes `model` to `turn` with the request's `at`, the start time of the SessionStart hook. So the model of a hook that started later, during the wait of a held title, stays. For a held title, `/title` replies at once. The service then opens or creates the session, renames it and passes `model` when the server of that process registers, or after 10 s, while the service runs ([service.ts:218-262](../src/service.ts#L218-L262), [:466-476](../src/service.ts#L466-L476)). A `/title` without `at`, from an older plugin, counts as the current time.

### 4.7 Plugin commands

| Command | Run by | Output |
|---|---|---|
| `simplex-mcp session` | `.mcp.json` | MCP over stdio |
| `simplex-mcp service` | `spawnService` | the ready line `ready`, `running` or `error: TEXT` on fd `SIMPLEX_MCP_READY_FD`, or on stdout when it is unset. If another running process holds ROOT/service.lock, the service checks the lock and the socket up to 150 times, 100 ms apart. It starts again once the lock is released, and its ready line is `running` once the socket answers. The log is written to stdout and stderr |
| `simplex-mcp hook session-start` | SessionStart hook | one JSON line, see [4.8](#48-plugin-options-files-and-hook-output) |
| `simplex-mcp hook turn` | UserPromptSubmit, Stop and PostModelSwitch hooks | `{}`. A prompt or an answer over 250,000 characters is cut there and ends with `…` ([hook.ts:16](../src/hook.ts#L16), [:155-157](../src/hook.ts#L155-L157)). The Stop hook sends the model of its answer, read from the transcript ([hook.ts:85](../src/hook.ts#L85), [:103-153](../src/hook.ts#L103-L153)) |
| `simplex-mcp link` | people | with owners, `Owners of "MAIN": A, B. To replace them, run npx -y @simplex-chat/mcp@VERSION reset-owners in a terminal.` When the service reports neither owners nor a link, `The Main group has no link. Run npx -y @simplex-chat/mcp@VERSION reset-owners in a terminal.` Otherwise, on a terminal, `Join "MAIN" in the SimpleX app. The link works once:`, a QR code and the link, and elsewhere `Run npx -y @simplex-chat/mcp@VERSION link in a terminal to see the link to "MAIN".` |
| `simplex-mcp reset-owners` | people | after `POST /reset-owners` of [4.6](#46-socket-protocol), on a terminal, `"MAIN" was replaced. Join the new one in the SimpleX app. The link works once:`, a QR code and the link, and elsewhere `"MAIN" was replaced. Run npx -y @simplex-chat/mcp@VERSION link in a terminal to see the link to the new one.` |

VERSION is the version of the package that prints the text ([owner.ts:7-8](../src/owner.ts#L7-L8)).

Environment:
- `SIMPLEX_MCP_HOME`: ROOT.
- `CLAUDE_CODE_SESSION_ID` and `CLAUDE_PROJECT_DIR`: the session.
- `CLAUDE_PID`: the Claude Code process of a hook.
- `CLAUDE_CODE_ENTRYPOINT` and `CLAUDE_CODE_SESSION_ATTENDED`: a run that is not interactive, as in [4.1](#41-tools-in-the-plugin). The session process and the hooks read both.
- `CLAUDE_PLUGIN_OPTION_MAIN_GROUP`, `CLAUDE_PLUGIN_OPTION_APPROVE` and `CLAUDE_PLUGIN_OPTION_EXPIRE_HOURS`: the options.
- `SIMPLEX_MCP_READY_FD`: the fd of the service's ready line.

The service is spawned with the variables of the spawning process whose names match, case-insensitively, `PATH`, `PATHEXT`, `HOME`, `USERPROFILE`, `HOMEDRIVE`, `HOMEPATH`, `TMPDIR`, `TMP`, `TEMP`, `XDG_CACHE_HOME`, `LOCALAPPDATA`, `APPDATA`, `SYSTEMROOT`, `SYSTEMDRIVE`, `WINDIR`, `COMSPEC`, `LANG`, `LC_*`, `LD_LIBRARY_PATH`, `DYLD_LIBRARY_PATH`, `HTTP_PROXY`, `HTTPS_PROXY`, `NO_PROXY`, `NODE_USE_ENV_PROXY`, `NODE_EXTRA_CA_CERTS`, `NODE_OPTIONS`, `SSL_CERT_FILE`, `SSL_CERT_DIR`, `SIMPLEX_*` and `CLAUDE_PLUGIN_OPTION_*` ([client.ts:13](../src/client.ts#L13), [:78](../src/client.ts#L78)).

| Code | Message | Code location |
|---|---|---|
| 0 | the hooks, even after errors, `link` and `reset-owners`, and the service after a stop or with `running` | [hook.ts:64-66](../src/hook.ts#L64-L66), [:97-100](../src/hook.ts#L97-L100), [service.ts:440](../src/service.ts#L440) |
| 2 | `simplex-mcp session runs from the Claude Code plugin.` | [session.ts:42](../src/session.ts#L42) |
| 2 | `Not a Claude Code session ID: ID` | [session.ts:43](../src/session.ts#L43) |
| 1 | `Restart Claude Code to update the SimpleX plugin.`, or a spawn error, from `link` and `reset-owners` | [client.ts:45](../src/client.ts#L45), [:84](../src/client.ts#L84), [:111](../src/client.ts#L111) |
| 1 | a failed service start, with the ready line `error: TEXT`. The line is `error: Another service uses ROOT (PID N). If no service runs, remove ROOT/service.lock.` when the lock's holder neither releases it nor answers in 150 checks | [service.ts:438-442](../src/service.ts#L438-L442), [:478-485](../src/service.ts#L478-L485), [files.ts:179](../src/files.ts#L179) |
| 1 | `stop: ERROR` for a failed stop after a signal | [service.ts:446-451](../src/service.ts#L446-L451) |

The service's stdout and stderr are written to `service.log` ([client.ts:77](../src/client.ts#L77)). The log is emptied when it is over 10 MB, checked at each spawn and every hour while the service runs ([files.ts:91-100](../src/files.ts#L91-L100), [service.ts:107-113](../src/service.ts#L107-L113)). stdout lines:

| Moment | Line | Code |
|---|---|---|
| start | `SimpleX MCP service VERSION, data folder ROOT` | [service.ts:444](../src/service.ts#L444) |
| Main group created | `Main group "NAME" created.` | [main-group.ts:203](../src/main-group.ts#L203) |
| link opened | `Main group link created. Whoever joins by it first becomes an owner of "NAME".` | [main-group.ts:150](../src/main-group.ts#L150) |
| no owners, members present | `No owners in the Main group.` | [main-group.ts:15](../src/main-group.ts#L15), [:70](../src/main-group.ts#L70), [:124](../src/main-group.ts#L124) |
| owner joined | `NAME joined (member ID) and is now an owner. The link is deleted.` | [main-group.ts:139](../src/main-group.ts#L139) |
| Main group lost | `The Main group is lost. Run npx -y @simplex-chat/mcp@VERSION reset-owners in a terminal.` | [main-group.ts:144](../src/main-group.ts#L144) |
| session group created | `Group "N · NAME" of session N created.` | [hub.ts:487](../src/hub.ts#L487) |
| session group lost | `The group "N · NAME" is lost.` | [control.ts:483](../src/control.ts#L483) |
| actions | as in [3.4](#34-terminal-lines), with ` of session N` after `Action N`, and `allowed.` for a tool use | [actions.ts:210](../src/actions.ts#L210), [:410](../src/actions.ts#L410) |
| stop | `Cancelled K action(s).`, `Stopping: sending K cancelled card(s), up to 10 s. Ctrl-C to stop now.`, `Stopped.` | [service.ts:189](../src/service.ts#L189), [serve.ts:164](../src/serve.ts#L164), [service.ts:192](../src/service.ts#L192) |

stderr: `CONTEXT: ERROR` from `logError`, with CONTEXT among `Main group command`, `EVENT in the Main group`, `EVENT in the group of session N`, `prompt of session N`, `invitation to session N`, `User`, `User in session N`, `group of session N`, `session N`, `owner of CHAT`, `mode host of session N`, `title`, `ROOT/_sessions/ID/session.json`, `ROOT/titles.json`, `ROOT/service.log`, `METHOD PATH` and those of [3.4](#34-terminal-lines). Session processes and hooks write to their own stderr ([session.ts:55](../src/session.ts#L55), [:61](../src/session.ts#L61), [:93](../src/session.ts#L93), [hook.ts:77](../src/hook.ts#L77), [:98](../src/hook.ts#L98)):
- A session process writes `SimpleX service: ERROR`, `SimpleX channel: ERROR` and `permission relay: ERROR`.
- The SessionStart hook of an unattended run writes `SimpleX MCP session start: ERROR`, and the turn hook `SimpleX MCP turn: ERROR`.

### 4.8 Plugin options, files and hook output

`userConfig` of [`.claude-plugin/plugin.json`](../.claude-plugin/plugin.json), read by [`readConfig`](../src/config.ts#L34-L40):

| Key | Variable | Type | Default | Valid | Error |
|---|---|---|---|---|---|
| `main_group` | `CLAUDE_PLUGIN_OPTION_MAIN_GROUP` | string | `SimpleX MCP` | equal to `validName(value)` | `a valid SimpleX name, such as VALID expected` or `a valid SimpleX name expected` |
| `approve` | `CLAUDE_PLUGIN_OPTION_APPROVE` | string | `requests` | `requests`, `host` | `requests or host expected` |
| `expire_hours` | `CLAUDE_PLUGIN_OPTION_EXPIRE_HOURS` | number | 23 | digits only, 1 to 23 | `hours from 1 to 23 expected` |

The default is used for a missing or blank variable ([config.ts:35-38](../src/config.ts#L35-L38), [:43](../src/config.ts#L43), [:50](../src/config.ts#L50), [:56](../src/config.ts#L56)). The hooks receive the variables from Claude Code. The MCP server gets them from `.mcp.json`, where `${user_config.KEY}` is mapped to the same names.

| File | Content |
|---|---|
| [`.claude-plugin/plugin.json`](../.claude-plugin/plugin.json) | name `simplex`, version `0.3.0`, equal to `package.json`, description, author, homepage, repository, license, keywords, `userConfig` |
| [`.mcp.json`](../.mcp.json) | server `simplex`: `node ${CLAUDE_PLUGIN_ROOT}/dist/cli.js session` with the three option variables, and `timeout` 169800000 ms |
| [`hooks/hooks.json`](../hooks/hooks.json) | The SessionStart hook runs `node "${CLAUDE_PLUGIN_ROOT}/dist/cli.js" hook session-start`, with timeout 600 s. The command of PreToolUse, with matcher `mcp__plugin_simplex_simplex__.*`, is `node "${CLAUDE_PLUGIN_ROOT}/plugin/allow.js"`. UserPromptSubmit, Stop and PostModelSwitch use `node "${CLAUDE_PLUGIN_ROOT}/dist/cli.js" hook turn`, with `async: true` |
| [`package.json`](../package.json) | `files`: the build, `README.md` and the plugin files, without a lockfile ([8-15](../package.json#L8-L15)). With `bundleDependencies: true` ([35](../package.json#L35)), the pack bundles the production packages of `node_modules`, the dependencies and their dependencies, and no development package. `prepack` deletes `dist` and builds it again ([24](../package.json#L24)). Claude Code installs a plugin's dependencies only when `bun.lock`, `npm-shrinkwrap.json`, `package-lock.json` or `bun.lockb` is in its folder. So it installs nothing here, and the plugin loads its bundled modules |
| [`package-lock.json`](../package-lock.json) | the tree that `npm ci` installs for development and in `build.yml` before the publication. Its production packages are marked `inBundle`. It is not packed |
| [`plugin/allow.js`](../plugin/allow.js) | prints `{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"allow"}}` |
| [`/.claude-plugin/marketplace.json`](../../../.claude-plugin/marketplace.json) | marketplace `simplex-chat`, plugin `simplex`, source `{"source": "npm", "package": "@simplex-chat/mcp"}` |

Hook input fields used ([hook.ts:22-37](../src/hook.ts#L22-L37)):
- SessionStart: `session_id`, `session_title`, `cwd` and `model`.
- The other hooks: `session_id`, `hook_event_name`, `transcript_path`, `prompt`, `last_assistant_message`, `to_model` and `effort.level`.

The Stop hook reads `type`, `isSidechain`, `message.model` and the `text` parts of `message.content` from the last entries of the transcript ([hook.ts:130-153](../src/hook.ts#L130-L153)).

SessionStart output ([hook.ts:48-79](../src/hook.ts#L48-L79)):

| Case | Output |
|---|---|
| a run that is not interactive ([4.1](#41-tools-in-the-plugin)) | `{}`, after `/title` with `unattended` when `CLAUDE_CODE_SESSION_ATTENDED` is `0` and the entrypoint is an interactive one |
| Node.js below 22 | `{"systemMessage": "SimpleX MCP needs Node.js 22 or later; found V."}` |
| owners present, or no link | `{}` |
| no owner, a link | `systemMessage`: `SimpleX MCP: scan this QR code with the SimpleX app and tap Join to supervise your agents in "MAIN":`, the `plain` QR code and the link |
| service not running after the start | `{"systemMessage": "SimpleX MCP: The SimpleX service stopped; it starts again with the next tool call."}` |
| other errors, invalid options included | `{"systemMessage": "SimpleX MCP: TEXT"}` |
