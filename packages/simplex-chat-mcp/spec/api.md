# SimpleX MCP server: API

> The six MCP tools, the agent control chat commands, replies and cards, and the CLI of `@simplex-chat/mcp` 0.2.0.
>
> Related specs: [README](README.md) | [Architecture](architecture.md) | [Flows](flows.md)

**Source:** [`tools.ts`](../src/tools.ts) | [`control.ts`](../src/control.ts) | [`cli.ts`](../src/cli.ts) | [`messages.ts`](../src/messages.ts) | [`actions.ts`](../src/actions.ts)

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
   - [1.10 Voice messages](#110-voice-messages)
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

---

## 1. Tools

### 1.1 Common rules

| Rule | Code |
|---|---|
| Tools are registered on each per-request `McpServer` with description, input schema, output schema and, in mode `host`, `_meta: {"anthropic/requiresUserInteraction": true}` for `connect`, `send`, `download` and `leave` | [tools.ts:465-472](../src/tools.ts#L465-L472), [:503-513](../src/tools.ts#L503-L513) |
| With `--audio`, `voice` is in the input schema of `send` | [tools.ts:279](../src/tools.ts#L279), [:507](../src/tools.ts#L507) |
| Tool descriptions and server instructions; the instructions name the agent's display name | [tools.ts:417-455](../src/tools.ts#L417-L455), [serve.ts:115](../src/serve.ts#L115) |
| Arguments are validated by the SDK; a failure is a result with `isError: true` and the text `Input validation error: Invalid arguments for tool NAME: ERRORS` | [mcp-BumlNux0.cjs:1812-1817](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L1812-L1817) |
| `additionalProperties: false` is set in every input schema | [tools.ts:259](../src/tools.ts#L259), [:276](../src/tools.ts#L276), [:289](../src/tools.ts#L289), [:292](../src/tools.ts#L292), [:300](../src/tools.ts#L300), [:311](../src/tools.ts#L311) |
| A result is sent as `structuredContent` and as its JSON in one text item | [tools.ts:518](../src/tools.ts#L518) |
| A tool error is sent as `isError: true` with one text item: the `ToolError` text, or the `errorText` of other errors | [tools.ts:520](../src/tools.ts#L520), [log.ts:45-49](../src/log.ts#L45-L49) |
| An aborted call ends with the result `Cancelled.`, `isError: true`, which the SDK discards | [tools.ts:474](../src/tools.ts#L474), [:511](../src/tools.ts#L511) |
| With a `progressToken` in `_meta`, `notifications/progress` is sent at the start of each wait and every 60 s, with `progress` 0, 1, 2, ... and a `message` | [tools.ts:1623-1633](../src/tools.ts#L1623-L1633), [messages.ts:299-303](../src/messages.ts#L299-L303) |
| The call is aborted when the client closes the response stream or sends `notifications/cancelled`; the action is kept | [src-BhnGYybN.cjs:6338-6341](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs#L6338-L6341), [:6425](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs#L6425) |
| A call with the same tool and action fields as an action in memory is attached to it | [tools.ts:1615-1617](../src/tools.ts#L1615-L1617), [actions.ts:151-153](../src/actions.ts#L151-L153) |
| Messages are marked read after the last wait of a call that is not aborted | [tools.ts:702](../src/tools.ts#L702), [:849](../src/tools.ts#L849), [:1199](../src/tools.ts#L1199) |

Action fields of the key:

| Tool | Fields |
|---|---|
| `connect` | `target`<br>`incognito` (default `false`)<br>`message` (default `null`) |
| `send` | `chat`<br>`text` (default `null`)<br>`file` (default `null`)<br>`as` (default `null`)<br>`reply_to` (default `null`)<br>`mention` (default `null`)<br>`voice` (default `null`) |
| `download` | `chat`<br>`item` |
| `leave` | `chat` |

Result status of the four action tools ([`actionResult`](../src/tools.ts#L671-L683)):

| Status | Meaning | Extra fields |
|---|---|---|
| `done` | performed | per tool |
| `failed` | failure of the card post, the counter write or the perform, or loss of the control chat | `error` |
| `rejected` | an approver sent `/reject` | |
| `expired` | no decision before the expiry time | |
| `known` | `connect` only: the target is an existing chat past the prepared state; no action | chat fields |
| `performing` | `download` only: `wait` passed after the approval; the download continues | |

### 1.2 `connect`

Handler: [`AgentTools.connect`](../src/tools.ts#L652-L665). Perform: [`performConnect`](../src/tools.ts#L721-L752). Schemas: [input](../src/tools.ts#L249-L260), [output](../src/tools.ts#L317-L327).

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
| `known` | `status`, chat fields of the known chat | [tools.ts:657](../src/tools.ts#L657) |
| `done`, chat unknown | `action`, `status`, `type`, `connected: false`, plan fields `name`, `peer_type`, `description`, `welcome`, `commands`, `known_as` when present | [tools.ts:668](../src/tools.ts#L668), [:1405-1411](../src/tools.ts#L1405-L1411) |
| `done`, chat known | `action`, `status`, chat fields, `messages` | [tools.ts:685-705](../src/tools.ts#L685-L705) |
| `failed` | `action`, `status`, `error` | [tools.ts:676](../src/tools.ts#L676) |
| `rejected`, `expired` | `action`, `status` | [tools.ts:676](../src/tools.ts#L676) |

Chat fields ([`chatFields`](../src/tools.ts#L1261-L1288)):

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

`messages`: message items among the last 50 items of the chat when the result is built, replaced by their collected copies, followed by the items collected since the connection; live items are left out; new items are marked read ([tools.ts:687](../src/tools.ts#L687), [:697-704](../src/tools.ts#L697-L704)).

Tool errors:

| Text | Condition | Code |
|---|---|---|
| `Not a SimpleX link or name: TARGET` | whitespace in `target`, or `invalidConnReq` from `apiConnectPlan` | [tools.ts:709](../src/tools.ts#L709), [:715](../src/tools.ts#L715) |
| an app error text, for example `Name not found: This SimpleX name is not registered. Please check the name.`, or `SimpleX error: CHAIN: DETAILS` | other errors of `apiConnectPlan`, or a plan of type `error` | [tools.ts:716](../src/tools.ts#L716), [:1332](../src/tools.ts#L1332), [log.ts:51-53](../src/log.ts#L51-L53) |
| `TARGET can't be joined now: PLAN.` | `invitationLink` other than `ok` and `known`<br>`contactAddress` other than `ok`, `known`, `contactViaAddress` and `connectingConfirmReconnect`<br>`groupLink` other than `ok`, `known` and `connectingConfirmReconnect` | [tools.ts:1323](../src/tools.ts#L1323), [:1336](../src/tools.ts#L1336), [:1354](../src/tools.ts#L1354), [:1376](../src/tools.ts#L1376) |
| `message: TARGET is not a contact or business address with short link data, or its contact exists without a connection.` | `message` given and the plan has `takesMessage: false` | [tools.ts:659](../src/tools.ts#L659) |
| refusals | see [1.9](#19-action-refusals) | |

`takesMessage` is `true` for a contact address with short link data, for a known prepared contact, and for a known prepared business group ([tools.ts:1327](../src/tools.ts#L1327), [:1347](../src/tools.ts#L1347), [:1365](../src/tools.ts#L1365)).

### 1.3 `send`

Handler: [`AgentTools.send`](../src/tools.ts#L809-L837). Result: [`sendResult`](../src/tools.ts#L839-L857). Perform: [`performSend`](../src/tools.ts#L859-L884). Schemas: [input](../src/tools.ts#L262-L277), [output](../src/tools.ts#L329-L341).

Input:

| Field | Type | Range | Default | Rule |
|---|---|---|---|---|
| `chat` | string | | required | `@N` or `#N` of the agent |
| `text` | string | 1 to 12000 characters | required without `file` and `voice` | the message text, or the caption of `file` or `voice` |
| `file` | string | 1 character or more | | absolute path of a file of this computer |
| `as` | string | `file` | | sends an image or video unchanged, as a file; only with `file` |
| `reply_to` | integer | 1 or more | | item ID of a message in `chat` |
| `mention` | integer array | 1 to 3, unique, each 1 or more | | member IDs of a group; names missing from `text` are prefixed as `@name` |
| `voice` | string | 1 to 4096 characters | | with `--audio` only: the text spoken by the audio API and sent as a voice message; not with `file` ([1.10](#110-voice-messages)) |
| `wait` | number | 0 to 86400 | 60 | seconds from the send; from the call when it attaches to a done action |
| `idle` | number | 1 to 300 | 30 | seconds without a new response, once one arrived |
| `limit` | integer | 1 to 100 | 50 | most responses returned |

File types by extension, lower-cased ([tools.ts:174-183](../src/tools.ts#L174-L183), [`attachmentMedia`](../src/tools.ts#L1573-L1584)):

| Extension | Message type | Sent file | Preview and duration |
|---|---|---|---|
| `.jpg`, `.jpeg`, `.png` | image | `core.resizeImage(..., Image)`, written to `DATA/files/sent/`; the original when resizing fails | `core.resizeImage(..., MessagePreview)`; the fixed preview when it fails |
| `.gif`, `.webp` | image | unchanged | the fixed preview |
| `.mp4`, `.mov` | video | unchanged | the first frame from `ffmpeg`, resized as a preview; duration from `ffprobe`, rounded; the fixed preview and 0 when they fail |
| other, or `as: "file"` | file | unchanged | |

The fixed preview is a 16 × 9 mid-gray PNG ([tools.ts:161](../src/tools.ts#L161)). `ffmpeg` and `ffprobe` run with `-protocol_whitelist file -f mov`, 30 s and 64 MB of output each ([tools.ts:1595-1613](../src/tools.ts#L1595-L1613)). With `voice`, `voice.m4a`, made from the speech of the audio API, is sent as a voice message with its duration ([1.10](#110-voice-messages)).

Output:

| Field | Presence | Value |
|---|---|---|
| `action` | always | action number |
| `status` | always | `done`, `failed`, `rejected`, `expired` |
| `chat` | `done` | the chat reference |
| `item` | `done` | item ID of the sent message |
| `responses` | `done` | received message items with IDs above `item`, in arrival order, live and held items left out ([1.10](#110-voice-messages)), at most `limit` |
| `more` | `done` | collected responses not returned, live and held ones and overflow included |
| `error` | `failed` | error text |

Tool errors:

| Text | Condition | Code |
|---|---|---|
| `text: required without file.`, with `--audio` `text: required without file or voice.` | none of `text`, `file` and `voice` | [tools.ts:814](../src/tools.ts#L814) |
| `voice: not with file.` | `voice` with `file` | [tools.ts:815](../src/tools.ts#L815) |
| `as: applies only with file.` | `as` without `file` | [tools.ts:816](../src/tools.ts#L816) |
| `No chat CHAT. list shows the agent's chats.` | invalid reference, the control chat, or a store error when the chat is read | [tools.ts:1227-1234](../src/tools.ts#L1227-L1234), [:1517-1519](../src/tools.ts#L1517-L1519) |
| `The agent is not connected to CHAT and can't post; list shows connected: false.` | the chat is not connected | [tools.ts:818](../src/tools.ts#L818) |
| `The agent has role ROLE in CHAT and can't post.` | membership role `observer` or `relay` | [tools.ts:819-822](../src/tools.ts#L819-L822) |
| `reply_to: CHAT has no item N.` | the read of `around=N` fails, or the item is absent from it | [tools.ts:1239](../src/tools.ts#L1239) |
| `reply_to: item N in CHAT is not a message.` | the item's content is not `rcvMsgContent` or `sndMsgContent` | [tools.ts:1241](../src/tools.ts#L1241) |
| `mention: CHAT is a contact chat; mentions apply to groups and channels.` | `mention` with `@N` | [tools.ts:1247](../src/tools.ts#L1247) |
| `mention: CHAT has no member N.` | ID absent from `apiListMembers` | [tools.ts:1252](../src/tools.ts#L1252) |
| `mention: members N and M have the same name.` | two mentioned members with the same display name | [tools.ts:1255](../src/tools.ts#L1255) |
| `file: an absolute path is expected.`<br>`file: no file at FILE.`<br>`file: PATH is not a file.`<br>`file: PATH can't be read.` | the path, after symbolic links | [tools.ts:1526-1537](../src/tools.ts#L1526-L1537) |
| `file: PATH is refused: private keys and the server's own files are not sent.` | a file named `id_*`, `*.pem`, `*.key`, `*.p12` or `*.pfx`, or under `~/.ssh`, `~/.gnupg`, or the data folder or `~/.simplex-mcp` outside `DATA/files/`; each folder by its real path | [tools.ts:900-903](../src/tools.ts#L900-L903), [:909-914](../src/tools.ts#L909-L914) |
| `file: PATH is empty.` | size 0 | [tools.ts:904](../src/tools.ts#L904) |
| `text: too long for the message with its quote; shorten it.`<br>`text: too long for a message with this TYPE; shorten it.` | the message, as the core sends it, above 15602 bytes; a send with `voice` is measured as a voice message of 300 s named `voice.m4a`, TYPE `voice` | [tools.ts:826-829](../src/tools.ts#L826-L829), [:1494-1510](../src/tools.ts#L1494-L1510), [:184](../src/tools.ts#L184) |
| refusals | see [1.9](#19-action-refusals) | |

The message is measured as 600 bytes plus the JSON of its content, the file's name, the quote and the mentioned names. The quote is the quoted text, or its file's name when the text is empty; a sent image or video quotes it as text, otherwise an image's preview is kept ([tools.ts:1494-1510](../src/tools.ts#L1494-L1510)).

The perform fails with `file: PATH changed after the request.` when the file's real path, size, inode or change time differs from the request ([tools.ts:916-921](../src/tools.ts#L916-L921)). With `voice`, an error of the audio API or of `ffmpeg` is the failure text of the perform ([1.10](#110-voice-messages)). A resized copy is deleted at `sndFileCompleteXFTP`, at `sndFileError` and after a failed send ([tools.ts:881](../src/tools.ts#L881), [:886-896](../src/tools.ts#L886-L896), [serve.ts:159-163](../src/serve.ts#L159-L163)).

### 1.4 `receive`

Handler: [`AgentTools.receive`](../src/tools.ts#L1171-L1205). Schemas: [input](../src/tools.ts#L281-L290), [output](../src/tools.ts#L343-L351).

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
| `messages` | unread received messages in chat order, then received messages that arrived during the call; live and held items left out ([1.10](#110-voice-messages)); at most `limit` |
| `more` | new messages not returned, overflow, and unread items left unread by the page reads |

Tool errors: `No chat CHAT. list shows the agent's chats.` for an invalid reference, the control chat, or a store error of the first read, `initial` 1 ([tools.ts:1173](../src/tools.ts#L1173), [:1177-1179](../src/tools.ts#L1177-L1179)); other chat errors, a store error of a later page read included, as their error text ([log.ts:45-53](../src/log.ts#L45-L53)).

Messages are read by `receive` without an action or an approval.

### 1.5 `download`

Handler: [`AgentTools.download`](../src/tools.ts#L1010-L1031). Result: [`downloadResult`](../src/tools.ts#L1058-L1066). Perform: [`performDownload`](../src/tools.ts#L1068-L1075), [`downloadFile`](../src/tools.ts#L537-L564). Schemas: [input](../src/tools.ts#L303-L312), [output](../src/tools.ts#L398-L415).

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
| `transcript`, `transcript_error` | `done` | the fields of the file's transcript file, when present ([1.10](#110-voice-messages)) |
| `error` | `failed` | error text |

FOLDER is `group-N` for `#N` and `contact-N` for `@N`; the extension is the file name's extension when it has 1 to 16 ASCII letters and digits ([messages.ts:216-224](../src/messages.ts#L216-L224)). `/forward` downloads a file to `DATA/forwarded/FOLDER/` under the same name; a later perform of the download moves it to the download path, after the copy for the post is made ([tools.ts:537-554](../src/tools.ts#L537-L554)).

Steps before the action:

| Step | Code |
|---|---|
| a file in status `rcvInvitation`, or an XFTP file: the file's servers from `apiGetChatItemInfo`; while none, the call waits for `rcvFileDescrReady` until `wait` seconds after the call, with the progress `The sender is uploading the file of item N in CHAT; waiting until TIME.` | [tools.ts:1033-1050](../src/tools.ts#L1033-L1050) |
| unknown servers: the file's servers that are not among the XFTP servers of `apiGetUserServers`, of no operator or of an enabled one, compared without their passwords | [tools.ts:1052-1056](../src/tools.ts#L1052-L1056) |
| without unknown servers, and for a file ID not in rejected files, the chat's files approval, read again, makes the action performed at once | [tools.ts:1025](../src/tools.ts#L1025) |

Perform: `apiReceiveFile(fileId, path, approvedRelays)`, for a file in status `rcvInvitation`. `approvedRelays` is true in mode `requests` for a download whose card named unknown servers, and false otherwise; the core then checks the complete description ([tools.ts:560](../src/tools.ts#L560), [:1077-1086](../src/tools.ts#L1077-L1086)). The download ends at `rcvFileComplete`, `rcvFileError`, `rcvFileSndCancelled` or `chatItemUpdated` of its file, or at a check every 60 s ([tools.ts:596-618](../src/tools.ts#L596-L618), [:1088-1156](../src/tools.ts#L1088-L1156)). With `--audio`, a voice file is then transcribed in the perform ([1.10](#110-voice-messages)).

Failure texts of the perform:

| Text | Condition | Code |
|---|---|---|
| the file's error, as in the file field | the file failed | [tools.ts:1117-1127](../src/tools.ts#L1117-L1127) |
| `The sender cancelled the file.` | `rcvFileSndCancelled` | [tools.ts:613-618](../src/tools.ts#L613-L618) |
| `The message or its chat was deleted.` | a check finds no message, contact or group | [tools.ts:165](../src/tools.ts#L165), [:1107-1115](../src/tools.ts#L1107-L1115) |
| `The download made no progress for 30 minutes and was cancelled.` | no rise of `rcvProgress` for 30 minutes; the file is cancelled | [tools.ts:169](../src/tools.ts#L169), [:1100-1104](../src/tools.ts#L1100-L1104) |
| `The received file is not in the agent's files folder.` | the file is received and neither the download path nor the forwarded path has a file | [messages.ts:15](../src/messages.ts#L15), [tools.ts:1129-1133](../src/tools.ts#L1129-L1133) |
| `Your owner deleted the files of this chat.` | `/delete CHAT files` | [tools.ts:166](../src/tools.ts#L166), [:959-977](../src/tools.ts#L959-L977) |
| `The file is on unknown servers that were not approved.` | a check reads an accepted file in status `rcvInvitation`: the core returned it to the offer when its description completed | [tools.ts:167](../src/tools.ts#L167), [:1096](../src/tools.ts#L1096) |
| `Unknown servers! Without Tor or VPN, your IP address will be visible to these XFTP relays: HOST, HOST.` | the core refuses the servers of the complete description at the acceptance | [log.ts:78-79](../src/log.ts#L78-L79) |

Tool errors:

| Text | Condition | Code |
|---|---|---|
| `No chat CHAT. list shows the agent's chats.` | invalid reference, the control chat, or a store error when the chat is read | [tools.ts:1015](../src/tools.ts#L1015) |
| `item: CHAT has no item N.` | the item is not found | [tools.ts:1017](../src/tools.ts#L1017) |
| `item: item N in CHAT has no received file.` | the item is not received or has no file | [tools.ts:1019](../src/tools.ts#L1019) |
| `item: the file of item N in CHAT failed: ERROR` | the file field has status `failed` | [tools.ts:1020-1021](../src/tools.ts#L1020-L1021) |
| `item: the sender has not uploaded the file of item N in CHAT yet; call download again later.` | no servers by the end of `wait`, also for a file invited without a description, whose servers the core does not list; no action is created | [tools.ts:1023](../src/tools.ts#L1023), [:1048](../src/tools.ts#L1048) |
| refusals | see [1.9](#19-action-refusals) | |

A file ID whose download returned `rejected` or `expired` shows status `rejected` until a download of it is done or the server restarts ([tools.ts:1063-1064](../src/tools.ts#L1063-L1064)).

### 1.6 `list`

Handler: [`AgentTools.listChats`](../src/tools.ts#L1207-L1215). Schemas: [input](../src/tools.ts#L292), [output](../src/tools.ts#L353-L385).

Input: an empty object.

Output:

| Field | Item fields |
|---|---|
| `chats` | `chat`<br>`type`<br>`name`<br>`simplex_name`, when verified<br>`peer_type`, when present<br>`incognito`<br>`known_as`, when incognito<br>`connected`<br>`unread`: `chatStats.unreadCount`<br>`approved: true`, only under a standing approval of messages in mode `requests` |
| `actions` | `action`<br>`tool`<br>`input`: the input of the creating call<br>`state`<br>`open`: `true` while a call waits on it |

`chats`: the contacts and groups of the main conversations from one read, `apiGetChats(USER, last 1000000)`, except the control chat ([tools.ts:1217-1225](../src/tools.ts#L1217-L1225)). `actions`: every action in memory ([tools.ts:1213](../src/tools.ts#L1213)).

### 1.7 `leave`

Handler: [`AgentTools.leave`](../src/tools.ts#L985-L994). Perform: [`leaveChat`](../src/tools.ts#L1000-L1008). Schemas: [input](../src/tools.ts#L294-L301), [output](../src/tools.ts#L387-L396).

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

Tool errors: `No chat CHAT. list shows the agent's chats.` when no action with the same key is in memory and the chat is invalid, the control chat or unknown ([tools.ts:988-990](../src/tools.ts#L988-L990)); refusals.

### 1.8 Message, file and command objects

Message ([`Message`](../src/messages.ts#L17-L29), [`toMessage`](../src/messages.ts#L175-L193), [schema](../src/tools.ts#L186-L221)):

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

File ([`messageFile`](../src/messages.ts#L195-L209)):

| Field | Value |
|---|---|
| `name` | `fileName` |
| `size` | `fileSize`, bytes |
| `type` | `image`, `video` or `voice` from the message type; `file` otherwise |
| `status` | first match:<br>`rejected`: the file ID is in rejected files<br>`failed`: `fileProhibited` is set, the status is `rcvCancelled`, `rcvAborted`, `rcvError` or `invalid`, or the status is `rcvComplete` and neither the download path nor the forwarded path has a file<br>`received`: `rcvComplete`, a file at the download path, and `fileSource` at the download path or the forwarded path<br>`offered`: otherwise, a file that `/forward` downloaded included |
| `path` | `received`: the download path |
| `error` | `failed`: the error text ([log.ts:197-244](../src/log.ts#L197-L244)) |
| `transcript` | `received`: the string `transcript` of the transcript file ([1.10](#110-voice-messages)) |
| `transcript_error` | `received`: the string `transcript_error` of the transcript file, without `transcript` |

Only received message items (`rcvMsgContent`) are returned by `send` and `receive`; `connect` also returns sent ones (`sndMsgContent`) ([messages.ts:102](../src/messages.ts#L102), [tools.ts:697](../src/tools.ts#L697)).

Command ([`commandsOf`](../src/tools.ts#L1472-L1479), [schema](../src/tools.ts#L223-L233)):

| Form | Fields |
|---|---|
| command | `keyword`<br>`label`<br>`params`, when present |
| menu | `label`<br>`commands`: commands and menus |

### 1.9 Action refusals

Refusals of a new `connect`, `send`, `download` or `leave` action, first match ([actions.ts:172-177](../src/actions.ts#L172-L177), [control.ts:119-128](../src/control.ts#L119-L128)):

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

Largest card form ([control.ts:623-631](../src/control.ts#L623-L631)): the maximum of the creation form, the `done` form and a `failed` form with an error of 600 × `€`, with the action's own decider or the longest current approver name; each form is measured as JSON-escaped UTF-8: `"`, `\`, `\n`, `\r`, `\t` 2 bytes, other characters below U+0020 6 bytes.

Link texts of a `connect` card ([control.ts:122](../src/control.ts#L122), [:645-659](../src/control.ts#L645-L659)): the description and welcome lines take the bytes that the largest form leaves without them. Lines are kept while they fit; the next line is cut to the bytes left and ends in `…`; later lines are left out. The tool result keeps both texts whole.

### 1.10 Voice messages

With `--audio` ([3.1](#31-serve)), voice messages are transcribed and voice replies spoken by the audio API at URL ([`audio.ts`](../src/audio.ts)).

A received voice message is transcribed at its arrival under these conditions ([`transcribesAtArrival`](../src/tools.ts#L577-L579)):

| Condition | Code |
|---|---|
| `--audio` given | [tools.ts:578](../src/tools.ts#L578) |
| the main conversation of a contact or group | [messages.ts:136-138](../src/messages.ts#L136-L138) |
| a received message with a file of type `voice`, a duration of at most 300 s and a size of at most 1500000 bytes | [tools.ts:570-571](../src/tools.ts#L570-L571), [:1490-1492](../src/tools.ts#L1490-L1492), [:170](../src/tools.ts#L170), [audio.ts:18](../src/audio.ts#L18) |
| file status `rcvInvitation`, protocol XFTP, no file error | [tools.ts:578](../src/tools.ts#L578) |
| fewer than 20 voice files in transcription; the control chat not lost | [tools.ts:171](../src/tools.ts#L171), [:578](../src/tools.ts#L578) |
| the chat's files approval by a current approver, in mode `requests` | [control.ts:130-133](../src/control.ts#L130-L133) |

Steps of the arrival ([`transcribeOnArrival`](../src/tools.ts#L581-L594)):

| Step | Code |
|---|---|
| the file ID added to `transcribing`, before `ChatItemsWatch.deliver` | [tools.ts:573](../src/tools.ts#L573), [serve.ts:139-140](../src/serve.ts#L139-L140) |
| the file's servers, waiting for the upload until 60 s after the arrival; none: `item: the sender has not uploaded the file of item N in CHAT yet; call download again later.` | [tools.ts:584](../src/tools.ts#L584), [:1033-1050](../src/tools.ts#L1033-L1050) |
| unknown servers: `The file is on unknown servers that were not approved.` | [tools.ts:585](../src/tools.ts#L585) |
| the files approval read again; absent, or the control chat lost: no download | [tools.ts:586](../src/tools.ts#L586) |
| the download, `approvedRelays` false, then the transcription | [tools.ts:587](../src/tools.ts#L587) |
| an error logged as `voice message of file N: ERROR` | [tools.ts:589](../src/tools.ts#L589) |
| the item read again; the file ID removed from `transcribing`; the item passed to the item watch as an update | [tools.ts:591-593](../src/tools.ts#L591-L593) |

A message is final when it is not live and its file ID is not in `transcribing` ([`isFinal`](../src/tools.ts#L1167-L1169)). Unread pages are read by `receive` until `limit + 1` final messages, and only final messages are returned; only final responses are returned by `send`, and its wait is ended by `idle` only when every collected response is final ([messages.ts:286-297](../src/messages.ts#L286-L297), [:321-329](../src/messages.ts#L321-L329), [tools.ts:846-848](../src/tools.ts#L846-L848), [:1180](../src/tools.ts#L1180), [:1185](../src/tools.ts#L1185)). A held message is kept unread and counted in `more`. Messages are not held by `connect`.

Speech-to-text ([`transcript`](../src/audio.ts#L22-L26), [`transcribe`](../src/audio.ts#L53-L64), [`post`](../src/audio.ts#L66-L74)):

| Item | Value |
|---|---|
| request | `POST URL/v1/audio/transcriptions`, a multipart form: `model` STT, `file` the bytes of the voice file, named `<file ID><extension>` |
| order | one request at a time, in the order of the calls |
| limits | 10 minutes from the request's turn; redirects refused; files over 1500000 bytes not sent |
| result | the string `text` of the JSON response, trimmed |
| errors | `Speech-to-text failed: HTTP N.`<br>`Speech-to-text failed: The operation was aborted due to timeout.`<br>`Speech-to-text failed: fetch failed.`<br>`Speech-to-text failed: no text.`<br>`Speech-to-text failed: the file has more than 1500000 bytes.` |

The transcript file is `PATH.transcript.json` beside the voice file PATH, mode 0600: `{"transcript": TEXT}`, or `{"transcript_error": ERROR}` with the error logged as `transcription of file N: ERROR` ([`writeTranscript`](../src/audio.ts#L33-L42)). Its string fields are read by `transcriptOf`; none are read from a missing or unreadable file ([audio.ts:44-51](../src/audio.ts#L44-L51), [messages.ts:203](../src/messages.ts#L203)).

`download`, with `--audio`, for a voice file within the limits: after the download, the file's arrival run is awaited, and the file is transcribed when its transcript file lacks `transcript`; the fields of the transcript file are added to the result ([tools.ts:1068-1075](../src/tools.ts#L1068-L1075), [:1061](../src/tools.ts#L1061)).

Voice replies, the perform of a `send` with `voice` ([`spokenFile`](../src/tools.ts#L942-L952), [`speech`](../src/audio.ts#L28-L31), [`performSend`](../src/tools.ts#L859-L884)):

| Step | Value |
|---|---|
| speech | `POST URL/v1/audio/speech`, JSON `{model: TTS, input: voice, response_format: "wav", voice: VOICE}`, `voice` only with VOICE; 10 minutes; redirects refused |
| WAV | `DATA/files/sent/send-XXXXXX/voice.m4a.wav`, mode 0600 |
| conversion | `ffmpeg -nostdin -v error -protocol_whitelist file -f wav -i voice.m4a.wav -ac 1 -ar 16000 -c:a aac -b:a 32k -f ipod voice.m4a`, 30 s ([tools.ts:946](../src/tools.ts#L946)) |
| duration | `ffprobe -f mov`, rounded; 0 when it fails ([tools.ts:1599-1607](../src/tools.ts#L1599-L1607)) |
| message | the file `voice.m4a`; `{type: "voice", text: the caption with the mentions, duration}` |
| failure | the folder deleted; `Text-to-speech failed: HTTP N.`, `Text-to-speech failed: The operation was aborted due to timeout.`, `Text-to-speech failed: fetch failed.`, or the error of `ffmpeg` |

The folder of `voice.m4a` is deleted as a resized copy is ([1.3](#13-send)).

---

## 2. Agent control chat

### 2.1 Commands and replies

Accepted text ([`parseCommand`](../src/control.ts#L545-L550), [`ownerRequest`](../src/control.ts#L711-L724)):

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

The server reads commands from members' messages in the main conversation of the control chat ([serve.ts:137](../src/serve.ts#L137), [control.ts:171-181](../src/control.ts#L171-L181)). It answers a command with a reply that quotes it, one command after another, and it runs the expiry check before each command ([control.ts:179](../src/control.ts#L179), [:201](../src/control.ts#L201)).

| Request | Condition | Reply | Code |
|---|---|---|---|
| one-line `/` text outside the requests | any sender | `Commands: /approve, /reject, /receive, /forward, /revoke, /leave, /delete, /pending, /list. Each card shows its commands.` | [control.ts:16](../src/control.ts#L16), [:203](../src/control.ts#L203) |
| any request | sender not an approver, `--owners` absent | `Only owners approve actions.` | [control.ts:204](../src/control.ts#L204) |
| any request | sender not an approver, `--owners` given | `Only the owners listed in --owners approve actions.` | [control.ts:204](../src/control.ts#L204) |
| `approve`, `reject`, `receive`, `forward` with N | action N absent, not `pending`, or without a card | `Action N is not pending.` | [control.ts:227](../src/control.ts#L227) |
| the same with CHAT | action N is a `connect`, or for another chat | `Action N is not for CHAT.` | [control.ts:228](../src/control.ts#L228) |
| `approve` | action N is a download | `Action N is a download: COMMANDS.`, COMMANDS the card's command line | [control.ts:234](../src/control.ts#L234) |
| `receive`, `forward`, `receive CHAT N all` | action N is not a download | `Action N is not a download.` | [control.ts:232](../src/control.ts#L232) |
| `approve`, `reject`, `receive`, `forward` | accepted | none; the card is edited at the outcome; `forward` posts the file, as a reply to the card when the message with the card as its quote is within 15602 bytes, and the card stays pending | [control.ts:229-238](../src/control.ts#L229-L238), [:265-281](../src/control.ts#L265-L281) |
| `approve CHAT all` | mode `host`; nothing is stored | `Approvals apply in mode requests; this server runs in mode host.` | [control.ts:213](../src/control.ts#L213) |
| `approve CHAT all`, `receive CHAT N all` | CHAT unknown or the control chat | `No chat CHAT. /list shows the agent's chats.` | [control.ts:246](../src/control.ts#L246) |
| `approve CHAT all` | accepted; the pending sends of CHAT with a card are approved | `Approved for KIND NAME (CHAT) by APPROVER. /'revoke CHAT'` | [control.ts:243-254](../src/control.ts#L243-L254) |
| `receive CHAT N all` | accepted; download N is performed, and the other pending downloads of CHAT stay pending | `Files approved for KIND NAME (CHAT) by APPROVER. /'receive CHAT off'` | [control.ts:237](../src/control.ts#L237), [:251-253](../src/control.ts#L251-L253) |
| `revoke CHAT` | CHAT unknown, or without a standing approval | `Messages to CHAT have no standing approval.` | [control.ts:259](../src/control.ts#L259) |
| `revoke CHAT` | accepted | `Messages to KIND NAME (CHAT) need approval again.` | [control.ts:262](../src/control.ts#L262) |
| `receive CHAT off` | CHAT unknown, or without a files approval | `Files from CHAT have no standing approval.` | [control.ts:259](../src/control.ts#L259) |
| `receive CHAT off` | accepted | `Files from KIND NAME (CHAT) need approval again.` | [control.ts:262](../src/control.ts#L262) |
| `leave CHAT` | CHAT unknown or the control chat | `No chat CHAT. /list shows the agent's chats.` | [control.ts:285](../src/control.ts#L285) |
| `leave CHAT` | accepted, at once in both modes; the pending actions of CHAT with a card are rejected by the sender | `Left KIND NAME (CHAT), requested by NAME.`, or `Deleted ...` for a contact; logged too | [control.ts:283-297](../src/control.ts#L283-L297) |
| `delete CHAT files` | the chat's downloads in memory fail and are cancelled; its other files in status `rcvAccepted`, `rcvTransfer` or `rcvWarning`, such as downloads the core continues after a restart, are cancelled; the folder of CHAT and its forwarded folder are deleted, for a current or a left chat | `Deleted K files from CHAT, SIZE.` (`1 file`), or `No files from CHAT.` | [control.ts:304-307](../src/control.ts#L304-L307), [tools.ts:959-977](../src/tools.ts#L959-L977), [messages.ts:272-280](../src/messages.ts#L272-L280) |
| `pending` | no pending action with a card | `No pending actions.` | [control.ts:208](../src/control.ts#L208) |
| `pending` | otherwise | one line per action: `N  WHAT  expires YYYY-MM-DD HH:MM UTC` | [control.ts:207](../src/control.ts#L207), [:619-621](../src/control.ts#L619-L621) |
| `list` | no chats | `No chats.` | [control.ts:325](../src/control.ts#L325) |
| `list` | otherwise | one line per chat of the 100 most recent: `CHAT  KIND NAME`, then `, approved for this chat by APPROVER`, `, files approved by APPROVER` and `, not connected` when applicable, cut to 100 bytes with `…`; then `K older chats are not shown.` (`1 older chat is not shown.`) | [control.ts:313-326](../src/control.ts#L313-L326) |

KIND is `contact`, `bot`, `agent`, `business`, `group` or `channel` ([tools.ts:1430-1437](../src/tools.ts#L1430-L1437)). WHAT is `connect to SUBJECT`, `send to SUBJECT`, `download NAME from SUBJECT`, `delete SUBJECT` or `leave SUBJECT` ([actions.ts:315-327](../src/actions.ts#L315-L327)). SIZE is in bytes, KB, MB or GB, by 1000, with one decimal ([control.ts:671-674](../src/control.ts#L671-L674)). Names are shown by `safeName` ([control.ts:688-691](../src/control.ts#L688-L691)).

A standing approval is stored in the chat's custom data, `simplexMcp` for messages and `simplexMcpFiles` for files, as `{approvedBy, approvedById, approvedAt}`; it applies in mode `requests` while member `approvedById` is an approver ([control.ts:22-25](../src/control.ts#L22-L25), [:130-133](../src/control.ts#L130-L133)).

### 2.2 Cards

Card text: request lines, then one last line ([`cardText`](../src/control.ts#L552-L571), [`requestLines`](../src/control.ts#L573-L602)).

Request lines:

| Tool | First line | Further lines |
|---|---|---|
| `connect` | `I want to connect to SUBJECT, as myself.` or `, incognito.`<br>done and connected: `I connected to ...`<br>done and not connected: `I asked to connect to ...` | link description, one code span per line<br>welcome, one code span per line<br>both cut to fit the card ([1.9](#19-action-refusals))<br>with `message`: `With the message:` and its lines |
| `send` | `I want to send a message to SUBJECT:`, with `, where I am incognito` before the colon in incognito chats<br>done: `I sent a message to ...` | text, one code span per line<br>with `file`: ``attaching TYPE `PATH`, SIZE``<br>with `voice`: `attaching as a voice message:`, then the voice text, one code span per line<br>`replying to NAME: QUOTE` with `reply_to`, the quote cut to 100 characters<br>`mentioning @A, @'B C'` with `mention` |
| `download` | `I want to download a file from SENDER in KIND NAME (CHAT):`, without `SENDER in ` outside groups<br>done: `I downloaded a file from ...` | ``` `NAME`, SIZE ```<br>an XFTP file: `Servers: known.`, or ``Servers: `HOST`, `HOST`, unknown: downloading shows them this computer's IP address.`` (`it` for one) |
| `leave` | `I want to delete KIND NAME (@N).` or `I want to leave KIND NAME (#N).`<br>done: `I deleted ...` or `I left ...` | |

SUBJECT is `KIND NAME`, or `an unnamed KIND`, followed by ` (SIMPLEX_NAME)` when known ([actions.ts:310-313](../src/actions.ts#L310-L313)). For `connect`, SIMPLEX_NAME is the target when it is a name; for `send`, the chat's verified SimpleX name.

A card of a `send` with an image or video file, or of a `download` of an image or video, is an image or video message with the file's preview and the card text as its caption; the preview is resized to fit the card, and the card is text when it can't be resized ([control.ts:441-461](../src/control.ts#L441-L461)).

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

DECIDER is `Performed without approval (mode host)` in mode `host`, `Approved for this chat by NAME` for a standing action, and `Approved by NAME` otherwise ([control.ts:554](../src/control.ts#L554)).

Command lines ([`commandsLine`](../src/control.ts#L604-L617)):

| Tool | Command line |
|---|---|
| `connect` | `/'approve N' or /'reject N'` |
| `send` | `/'approve CHAT N', /'approve CHAT all' or /'reject CHAT N'` |
| `download` | `/'receive CHAT N', /'receive CHAT N all', /'forward CHAT N' or /'reject CHAT N'`, without `/'receive CHAT N all'` for unknown servers |
| `leave` | `/'approve CHAT N' or /'reject CHAT N'` |

The app shows `/'approve N'` as the tappable command `/approve N` ([PLAN, What the owner sees](../PLAN.md#what-the-owner-sees-the-agent-control-chat)).

An action performed without a card, in mode `host` or under a standing approval, gets its outcome as a new message; a `download` without a card gets none ([control.ts:145-152](../src/control.ts#L145-L152), [:495-497](../src/control.ts#L495-L497)).

### 2.3 Other posts

| Situation | Text | Code |
|---|---|---|
| `send` with a refused file; logged too | ``Refused to send `PATH` to KIND NAME (CHAT): private keys and the server's own files are not sent.`` | [control.ts:154-158](../src/control.ts#L154-L158) |
| `sndFileError` of a sent file; logged too | ``The file `NAME` was not sent to KIND NAME (CHAT): `REASON`.`` | [control.ts:160-169](../src/control.ts#L160-L169), [serve.ts:161-163](../src/serve.ts#L161-L163) |
| `/forward` of a file that is not received or not posted | ``Action N: the file was not received: `ERROR`.`` | [control.ts:279](../src/control.ts#L279) |
| after the reply to `/leave CHAT`, and after the outcome of a done `leave`, when the folder of CHAT has files | ``Files from KIND NAME (CHAT) are kept in `FOLDER`: K files, SIZE. /'delete CHAT files'`` | [control.ts:150-151](../src/control.ts#L150-L151), [:294-302](../src/control.ts#L294-L302) |

### 2.4 Membership messages

Posted in the chat and logged; in the chat, names are shown by `safeName` and the `--owners ...` part is a code span; in the log, names are folded to one line ([control.ts:427-430](../src/control.ts#L427-L430), [:78-79](../src/control.ts#L78-L79)).

| Situation | Text | Code |
|---|---|---|
| owner joined, `--owners` absent | `I'm your agent. I ask here before I connect, send, download a file or leave a chat.` (chat only) | [control.ts:31](../src/control.ts#L31), [:375](../src/control.ts#L375) |
| member joined, `--owners` without a list | `NAME joined (member ID). Restart the server with --owners ENTRIES` | [control.ts:380-383](../src/control.ts#L380-L383) |
| start with `--owners` without a list and humans present | `A joined (member ID). B joined (member ID). Restart the server with --owners ENTRIES` | [control.ts:347-350](../src/control.ts#L347-L350) |
| a human became owner outside the list | `NAME (member ID) is now an owner and can't approve actions yet. Restart the server with --owners ENTRIES` | [control.ts:388-392](../src/control.ts#L388-L392) |
| a listed owner stopped being a human owner | `NAME (member ID) is no longer an owner. Restart the server with --owners ENTRIES`<br>none remains: `NAME (member ID) is no longer an owner. No owners remain: restart the server with --reset-owners --owners` | [control.ts:393-398](../src/control.ts#L393-L398) |

ENTRIES: `ID:NAME` of each human, or of each human owner, joined by commas; an entry with characters other than letters, digits, `_` and `-` is quoted for the shell ([control.ts:775-785](../src/control.ts#L775-L785)).

### 2.5 Group profile

| Field | Value | Code |
|---|---|---|
| display name | the agent's display name, updated at start when it differs | [control.ts:105-107](../src/control.ts#L105-L107) |
| full name | empty | [control.ts:512](../src/control.ts#L512) |
| description | `Supervise your AI agent here: approve or reject its connections and messages with one tap.` | [control.ts:513](../src/control.ts#L513) |
| history | on | [control.ts:514](../src/control.ts#L514) |
| command menu | `/pending` "Pending actions"<br>`/list` "Chats and approvals"<br>`/leave` "Leave a chat", params `<chat ID>`<br>updated at start when it differs | [control.ts:10-14](../src/control.ts#L10-L14), [:105-107](../src/control.ts#L105-L107) |

---

## 3. CLI

Binary: `simplex-mcp`, compiled `dist/cli.js` ([package.json:5-7](../package.json#L5-L7)). Help text: [cli.ts:11-43](../src/cli.ts#L11-L43).

```
simplex-mcp --help
simplex-mcp serve NAME [options]
simplex-mcp register NAME [options]
```

NAME: 1 to 32 characters of `[a-z0-9]` and single hyphens, starting with a letter or digit ([cli.ts:157-159](../src/cli.ts#L157-L159)).

### 3.1 `serve`

| Option | Value | Default | Rule | Code |
|---|---|---|---|---|
| `--data` | folder | `~/.simplex-mcp/NAME` | resolved to an absolute path; `dataGiven` is set | [cli.ts:74-77](../src/cli.ts#L74-L77) |
| `--display-name` | text | NAME when the user is created | must equal `validName(text)` | [cli.ts:78-80](../src/cli.ts#L78-L80), [:161-165](../src/cli.ts#L161-L165) |
| `--approve` | `requests` or `host` | `requests` | | [cli.ts:81-83](../src/cli.ts#L81-L83) |
| `--owners` | absent value, or `ID:NAME,...` | absent | the value is absent when the next argument is missing or starts with `--`; it is split at each comma followed by `ID:`, so a name may hold commas; entries match `^([1-9]\d*):(.+)$` | [cli.ts:84-86](../src/cli.ts#L84-L86), [:195-199](../src/cli.ts#L195-L199) |
| `--expire` | hours | 23 | integer 1 to 23 | [cli.ts:87-89](../src/cli.ts#L87-L89) |
| `--stop-wait` | seconds | 10 | integer 0 to 60 | [cli.ts:90-92](../src/cli.ts#L90-L92) |
| `--reset-owners` | flag | off | refused with an `--owners` list | [cli.ts:93-95](../src/cli.ts#L93-L95), [:112-114](../src/cli.ts#L112-L114) |
| `--new-token` | flag | off | | [cli.ts:96-98](../src/cli.ts#L96-L98) |
| `--port` | number | 5226 | integer 1 to 65535 | [cli.ts:99-101](../src/cli.ts#L99-L101) |
| `--qr-code` | `small` or `large` | `small` | | [cli.ts:102-104](../src/cli.ts#L102-L104) |
| `--audio` | `URL,STT,TTS` or `URL,STT,TTS,VOICE` | absent | 3 or 4 non-empty parts; URL `http:` or `https:`, without a user name or password; its trailing slashes removed | [cli.ts:105-107](../src/cli.ts#L105-L107), [:177-188](../src/cli.ts#L177-L188) |

### 3.2 `register`

| Option | Value | Default | Code |
|---|---|---|---|
| `--scope` | `user` or `local` | `user` | [cli.ts:122-124](../src/cli.ts#L122-L124) |
| `--data` | folder | `~/.simplex-mcp/NAME` | [cli.ts:125-127](../src/cli.ts#L125-L127) |

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
| 0 | `--help` text on stdout; a stop after a signal | [cli.ts:222-224](../src/cli.ts#L222-L224), [serve.ts:56](../src/serve.ts#L56) |
| 2 | `Usage: simplex-mcp serve NAME [options] or simplex-mcp register NAME [options]. Run simplex-mcp --help.` | [cli.ts:53](../src/cli.ts#L53) |
| 2 | `NAME: 1 to 32 lowercase letters, digits and single hyphens, starting with a letter or digit.` | [cli.ts:54](../src/cli.ts#L54) |
| 2 | `OPTION: a value expected.` | [cli.ts:146](../src/cli.ts#L146) |
| 2 | `Unknown option OPTION. Run simplex-mcp --help.` | [cli.ts:212](../src/cli.ts#L212) |
| 2 | `--display-name: not a valid SimpleX name; try VALID` | [cli.ts:163](../src/cli.ts#L163) |
| 2 | `--approve: requests or host expected, got VALUE` | [cli.ts:168](../src/cli.ts#L168) |
| 2 | `--qr-code: small or large expected, got VALUE` | [cli.ts:173](../src/cli.ts#L173) |
| 2 | `--audio: URL,STT,TTS or URL,STT,TTS,VOICE with an http or https URL expected, got VALUE` | [cli.ts:180](../src/cli.ts#L180) |
| 2 | `--scope: user or local expected, got VALUE` | [cli.ts:191](../src/cli.ts#L191) |
| 2 | `--owners: ID:NAME expected, got ENTRY` | [cli.ts:197](../src/cli.ts#L197) |
| 2 | `--expire: hours from 1 to 23 expected, got VALUE` | [cli.ts:88](../src/cli.ts#L88), [:203](../src/cli.ts#L203) |
| 2 | `--stop-wait: seconds from 0 to 60 expected, got VALUE` | [cli.ts:91](../src/cli.ts#L91), [:203](../src/cli.ts#L203) |
| 2 | `--port: a port from 1 to 65535 expected, got VALUE` | [cli.ts:100](../src/cli.ts#L100), [:203](../src/cli.ts#L203) |
| 2 | `--reset-owners takes --owners without a list: member IDs change in a new agent control chat.` | [cli.ts:113](../src/cli.ts#L113) |
| 2 | `--owners: member ENTRY is not in the agent control chat. Start with --reset-owners --owners, or use the IDs of current members.` | [control.ts:356](../src/control.ts#L356) |
| 1 | `Another server uses DATA (PID N). If no server runs, remove DATA/lock.` | [files.ts:21-23](../src/files.ts#L21-L23), [:60-62](../src/files.ts#L60-L62) |
| 1 | `Port PORT is in use. Start with --port N.` | [serve.ts:232](../src/serve.ts#L232) |
| 1 | `Agent "NAME" was never started. Start it with: npx -y @simplex-chat/mcp serve NAME` | [register.ts:35](../src/register.ts#L35) |
| 1 | `Add this entry under LOCATION in ~/.claude.json: {"simplex-NAME": ENTRY}` | [register.ts:55-58](../src/register.ts#L55-L58) |
| 1 | `claude mcp add-json failed: STDERR` | [register.ts:42](../src/register.ts#L42) |
| 1 | `simplex-mcp: ERROR` for other errors, `stop: ERROR` for a failed stop | [cli.ts:238-239](../src/cli.ts#L238-L239), [serve.ts:57-60](../src/serve.ts#L57-L60) |

LOCATION is `"mcpServers"` for scope `user`, and `"projects" > "PROJECT" > "mcpServers"` for scope `local` ([register.ts:56](../src/register.ts#L56)).

### 3.4 Terminal lines

stdout ([log.ts:37-39](../src/log.ts#L37-L39)):

| Moment | Line | Code |
|---|---|---|
| start | `SimpleX MCP server VERSION, agent "NAME"`<br>`Data folder: DATA`<br>`Approval mode: MODE`<br>with `--audio`: `Audio: URL, speech-to-text STT, text-to-speech TTS`, with `, voice VOICE` | [serve.ts:75-77](../src/serve.ts#L75-L77), [:78](../src/serve.ts#L78) |
| control chat created | `Agent control chat "DISPLAY" created.` | [control.ts:518](../src/control.ts#L518) |
| link opened | `Join the agent control chat in the SimpleX app. The link works once:`<br>QR code<br>link<br>small QR code: `If the QR code does not scan, restart with --qr-code large, or paste the link into the app.`<br>role line | [control.ts:406-416](../src/control.ts#L406-L416) |
| listening | `Listening on http://127.0.0.1:PORT/mcp` | [serve.ts:129](../src/serve.ts#L129) |
| owner setup done | `Add the agent to Claude Code:`<br>`  npx -y @simplex-chat/mcp register NAME [--data DATA]` | [serve.ts:188-190](../src/serve.ts#L188-L190) |
| owner joined | `NAME joined (member ID) and is now an owner. The link is deleted.` | [control.ts:376](../src/control.ts#L376) |
| listed owners set | `Owners: ENTRIES.` | [control.ts:367](../src/control.ts#L367) |
| approvers gone, `--owners` absent | `No owners in the agent control chat.` | [control.ts:32](../src/control.ts#L32), [:194](../src/control.ts#L194), [:343](../src/control.ts#L343) |
| control chat lost | `The agent lost its control chat. Restart the server with --reset-owners.` | [control.ts:403](../src/control.ts#L403) |
| action created | `Action N: WHAT` with `, as itself` or `, incognito` for `connect` and ` (CHAT)` for the other tools | [actions.ts:180-181](../src/actions.ts#L180-L181), [:315-327](../src/actions.ts#L315-L327) |
| counter write or card post failed | `Action N: failed: ERROR.` | [actions.ts:260](../src/actions.ts#L260) |
| perform ended, not interrupted by the stop, mode `requests` | `Action N approved by NAME: RESULT` with RESULT `sent.`, `downloaded.`, `deleted.`, `left.`, `connected.`, `connection requested.` or `failed: ERROR.` | [actions.ts:281](../src/actions.ts#L281), [:333-341](../src/actions.ts#L333-L341) |
| perform ended, not interrupted by the stop, mode `host` | `Action N: performed without approval (mode host).`, or `...; failed: ERROR.` | [actions.ts:335](../src/actions.ts#L335) |
| rejected | `Action N rejected by NAME.` | [actions.ts:228](../src/actions.ts#L228) |
| expired | `Action N expired.` | [actions.ts:198](../src/actions.ts#L198) |
| `/leave CHAT` | `Left KIND NAME (CHAT), requested by NAME.` or `Deleted ...` | [control.ts:289](../src/control.ts#L289) |
| refused file, file not sent | as in [2.3](#23-other-posts), names and paths on one line | [control.ts:156](../src/control.ts#L156), [:167](../src/control.ts#L167) |
| stop | `Cancelled K action.` or `Cancelled K actions.` when K > 0<br>`Stopping: sending K cancelled card(s), up to S s. Ctrl-C to stop now.` when cards were edited or posted and S > 0<br>`Stopped.` | [serve.ts:179](../src/serve.ts#L179), [:192-206](../src/serve.ts#L192-L206), [:183](../src/serve.ts#L183) |
| membership messages | see [2.4](#24-membership-messages) | |

stderr: `CONTEXT: ERROR` from `logError` ([log.ts:41-43](../src/log.ts#L41-L43)), with CONTEXT one of `action N`, `action N card`, `action N card image`, `action N file`, `control chat command`, `EVENT in the agent control chat`, `EVENT event`, `refused file`, `file not sent`, `download of file N`, `file N`, `moving file N`, `cancelling file N`, `voice message of file N`, `transcription of file N`, `resizing PATH`, `preview`, `MCP`, `HTTP request`, `stop`, `simplex-mcp`. The library's download lines are written to stderr.
