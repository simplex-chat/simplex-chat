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
| Tools are registered on each per-request `McpServer` with description, input schema, output schema and, in mode `host`, `_meta: {"anthropic/requiresUserInteraction": true}` for `connect`, `send`, `download` and `leave` | [tools.ts:477-484](../src/tools.ts#L477-L484), [:490-500](../src/tools.ts#L490-L500), [:532-534](../src/tools.ts#L532-L534) |
| Tool descriptions and server instructions; the instructions name the agent's display name | [tools.ts:413-451](../src/tools.ts#L413-L451), [serve.ts:108](../src/serve.ts#L108) |
| Arguments are validated by the SDK; a failure is a result with `isError: true` and the text `Input validation error: Invalid arguments for tool NAME: ERRORS` | [mcp-BumlNux0.cjs:1812-1817](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L1812-L1817) |
| `additionalProperties: false` is set in every input schema | [tools.ts:259](../src/tools.ts#L259), [:276](../src/tools.ts#L276), [:287](../src/tools.ts#L287), [:290](../src/tools.ts#L290), [:298](../src/tools.ts#L298), [:309](../src/tools.ts#L309) |
| A result is sent as `structuredContent` and as its JSON in one text item | [tools.ts:539](../src/tools.ts#L539) |
| A tool error is sent as `isError: true` with one text item: the `ToolError` text, or the `errorText` of other errors | [tools.ts:541](../src/tools.ts#L541), [log.ts:45-49](../src/log.ts#L45-L49) |
| An aborted call ends with the result `Cancelled.`, `isError: true`, which the SDK discards | [tools.ts:486](../src/tools.ts#L486), [:498](../src/tools.ts#L498) |
| With a `progressToken` in `_meta`, `notifications/progress` is sent at the start of each wait and every 60 s, with `progress` 0, 1, 2, ... and a `message` | [tools.ts:1682-1692](../src/tools.ts#L1682-L1692), [messages.ts:305-309](../src/messages.ts#L305-L309) |
| The call is aborted when the client closes the response stream or sends `notifications/cancelled`; the action is kept | [src-BhnGYybN.cjs:6338-6341](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs#L6338-L6341), [:6425](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs#L6425) |
| A call with the same tool and action fields as an action in memory is attached to it; the key is the JSON of the session number, `undefined` for `serve`, the tool and the fields | [tools.ts:1674-1676](../src/tools.ts#L1674-L1676), [:729-731](../src/tools.ts#L729-L731), [actions.ts:172-174](../src/actions.ts#L172-L174) |
| Messages are marked read after the last wait of a call that is not aborted | [tools.ts:773](../src/tools.ts#L773), [:919](../src/tools.ts#L919), [:1248](../src/tools.ts#L1248) |

Action fields of the key:

| Tool | Fields |
|---|---|
| `connect` | `target`<br>`incognito` (default `false`)<br>`message` (default `null`) |
| `send` | `chat`<br>`text` (default `null`)<br>`file` (default `null`)<br>`as` (default `null`)<br>`reply_to` (default `null`)<br>`mention` (default `null`) |
| `download` | `chat`<br>`item` |
| `leave` | `chat` |

Result status of the four action tools ([`actionResult`](../src/tools.ts#L742-L754)):

| Status | Meaning | Extra fields |
|---|---|---|
| `done` | performed | per tool |
| `failed` | failure of the card post, the counter write or the perform, or loss of the control chat | `error` |
| `rejected` | an approver sent `/reject` | |
| `expired` | no decision before the expiry time | |
| `known` | `connect` only: the target is an existing chat past the prepared state; no action | chat fields |
| `performing` | `download` only: `wait` passed after the approval; the download continues | |

### 1.2 `connect`

Handler: [`AgentTools.connect`](../src/tools.ts#L715-L736). Perform: [`performConnect`](../src/tools.ts#L792-L824). Schemas: [input](../src/tools.ts#L249-L260), [output](../src/tools.ts#L315-L325).

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
| `known` | `status`, chat fields of the known chat; in the plugin, a known chat of another session is the tool error `The chat of TARGET belongs to another session; an owner can give it to this session with /give.` | [tools.ts:720](../src/tools.ts#L720), [:733-735](../src/tools.ts#L733-L735) |
| `done`, chat unknown | `action`, `status`, `type`, `connected: false`, plan fields `name`, `peer_type`, `description`, `welcome`, `commands`, `known_as` when present | [tools.ts:739](../src/tools.ts#L739), [:1456-1462](../src/tools.ts#L1456-L1462) |
| `done`, chat known | `action`, `status`, chat fields, `messages` | [tools.ts:756-776](../src/tools.ts#L756-L776) |
| `failed` | `action`, `status`, `error` | [tools.ts:747](../src/tools.ts#L747) |
| `rejected`, `expired` | `action`, `status` | [tools.ts:747](../src/tools.ts#L747) |

Chat fields ([`chatFields`](../src/tools.ts#L1312-L1339)):

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

`messages`: message items among the last 50 items of the chat when the result is built, replaced by their collected copies, followed by the items collected since the connection; live items are left out; new items are marked read ([tools.ts:758](../src/tools.ts#L758), [:768-775](../src/tools.ts#L768-L775)).

Tool errors:

| Text | Condition | Code |
|---|---|---|
| `Not a SimpleX link or name: TARGET` | whitespace in `target`, or `invalidConnReq` from `apiConnectPlan` | [tools.ts:780](../src/tools.ts#L780), [:786](../src/tools.ts#L786) |
| an app error text, for example `Name not found: This SimpleX name is not registered. Please check the name.`, or `SimpleX error: CHAIN: DETAILS` | other errors of `apiConnectPlan`, or a plan of type `error` | [tools.ts:787](../src/tools.ts#L787), [:1383](../src/tools.ts#L1383), [log.ts:51-53](../src/log.ts#L51-L53) |
| `TARGET can't be joined now: PLAN.` | `invitationLink` other than `ok` and `known`<br>`contactAddress` other than `ok`, `known`, `contactViaAddress` and `connectingConfirmReconnect`<br>`groupLink` other than `ok`, `known` and `connectingConfirmReconnect` | [tools.ts:1374](../src/tools.ts#L1374), [:1387](../src/tools.ts#L1387), [:1405](../src/tools.ts#L1405), [:1427](../src/tools.ts#L1427) |
| `message: TARGET is not a contact or business address with short link data, or its contact exists without a connection.` | `message` given and the plan has `takesMessage: false` | [tools.ts:722](../src/tools.ts#L722) |
| refusals | see [1.9](#19-action-refusals) | |

`takesMessage` is `true` for a contact address with short link data, for a known prepared contact, and for a known prepared business group ([tools.ts:1378](../src/tools.ts#L1378), [:1398](../src/tools.ts#L1398), [:1416](../src/tools.ts#L1416)).

### 1.3 `send`

Handler: [`AgentTools.send`](../src/tools.ts#L882-L907). Result: [`sendResult`](../src/tools.ts#L909-L927). Perform: [`performSend`](../src/tools.ts#L929-L953). Schemas: [input](../src/tools.ts#L262-L277), [output](../src/tools.ts#L327-L339).

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

File types by extension, lower-cased ([tools.ts:177-186](../src/tools.ts#L177-L186), [`attachmentMedia`](../src/tools.ts#L1632-L1643)):

| Extension | Message type | Sent file | Preview and duration |
|---|---|---|---|
| `.jpg`, `.jpeg`, `.png` | image | `core.resizeImage(..., Image)`, written to `DATA/files/sent/`; the original when resizing fails | `core.resizeImage(..., MessagePreview)`; the fixed preview when it fails |
| `.gif`, `.webp` | image | unchanged | the fixed preview |
| `.mp4`, `.mov` | video | unchanged | the first frame from `ffmpeg`, resized as a preview; duration from `ffprobe`, rounded; the fixed preview and 0 when they fail |
| other, or `as: "file"` | file | unchanged | |

The fixed preview is a 16 × 9 mid-gray PNG ([tools.ts:166](../src/tools.ts#L166)). `ffmpeg` and `ffprobe` run with `-protocol_whitelist file -f mov`, 30 s and 64 MB of output each ([tools.ts:1654-1672](../src/tools.ts#L1654-L1672)).

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
| `text: required without file.` | neither `text` nor `file` | [tools.ts:887](../src/tools.ts#L887) |
| `as: applies only with file.` | `as` without `file` | [tools.ts:888](../src/tools.ts#L888) |
| `No chat CHAT. list shows the agent's chats.` | invalid reference, the control chat, or a store error when the chat is read | [tools.ts:1276-1285](../src/tools.ts#L1276-L1285), [:1576-1578](../src/tools.ts#L1576-L1578) |
| `The agent is not connected to CHAT and can't post; list shows connected: false.` | the chat is not connected | [tools.ts:890](../src/tools.ts#L890) |
| `The agent has role ROLE in CHAT and can't post.` | membership role `observer` or `relay` | [tools.ts:891-894](../src/tools.ts#L891-L894) |
| `reply_to: CHAT has no item N.` | the read of `around=N` fails, or the item is absent from it | [tools.ts:1290](../src/tools.ts#L1290) |
| `reply_to: item N in CHAT is not a message.` | the item's content is not `rcvMsgContent` or `sndMsgContent` | [tools.ts:1292](../src/tools.ts#L1292) |
| `mention: CHAT is a contact chat; mentions apply to groups and channels.` | `mention` with `@N` | [tools.ts:1298](../src/tools.ts#L1298) |
| `mention: CHAT has no member N.` | ID absent from `apiListMembers` | [tools.ts:1303](../src/tools.ts#L1303) |
| `mention: members N and M have the same name.` | two mentioned members with the same display name | [tools.ts:1306](../src/tools.ts#L1306) |
| `file: an absolute path is expected.`<br>`file: no file at FILE.`<br>`file: PATH is not a file.`<br>`file: PATH can't be read.` | the path, after symbolic links | [tools.ts:1585-1596](../src/tools.ts#L1585-L1596) |
| `file: PATH is refused: private keys and the server's own files are not sent.` | a file named `id_*`, `*.pem`, `*.key`, `*.p12` or `*.pfx`, or under `~/.ssh`, `~/.gnupg`, or the data folder or `~/.simplex-mcp` outside `DATA/files/`; each folder by its real path | [tools.ts:969-972](../src/tools.ts#L969-L972), [:978-983](../src/tools.ts#L978-L983) |
| `file: PATH is empty.` | size 0 | [tools.ts:973](../src/tools.ts#L973) |
| `text: too long for the message with its quote; shorten it.`<br>`text: too long for a message with this TYPE; shorten it.` | the message, as the core sends it, above 15602 bytes | [tools.ts:898-900](../src/tools.ts#L898-L900), [:1553-1569](../src/tools.ts#L1553-L1569) |
| refusals | see [1.9](#19-action-refusals) | |

The message is measured as 600 bytes plus the JSON of its content, the file's name, the quote and the mentioned names. The quote is the quoted text, or its file's name when the text is empty; a sent image or video quotes it as text, otherwise an image's preview is kept ([tools.ts:1553-1569](../src/tools.ts#L1553-L1569)).

The perform fails with `file: PATH changed after the request.` when the file's real path, size, inode or change time differs from the request ([tools.ts:985-990](../src/tools.ts#L985-L990)). A resized copy is deleted at `sndFileCompleteXFTP`, at `sndFileError` and after a failed send ([tools.ts:950](../src/tools.ts#L950), [:955-965](../src/tools.ts#L955-L965), [:680-682](../src/tools.ts#L680-L682)).

### 1.4 `receive`

Handler: [`AgentTools.receive`](../src/tools.ts#L1219-L1254). Schemas: [input](../src/tools.ts#L279-L288), [output](../src/tools.ts#L341-L349).

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

Tool errors: `No chat CHAT. list shows the agent's chats.` for an invalid reference, the control chat, or a store error of the first read, `initial` 1 ([tools.ts:1221](../src/tools.ts#L1221), [:1225-1227](../src/tools.ts#L1225-L1227)); other chat errors, a store error of a later page read included, as their error text ([log.ts:45-53](../src/log.ts#L45-L53)).

Messages are read by `receive` without an action or an approval.

### 1.5 `download`

Handler: [`AgentTools.download`](../src/tools.ts#L1066-L1086). Result: [`downloadResult`](../src/tools.ts#L1113-L1121). Perform: [`performDownload`](../src/tools.ts#L1123-L1127), [`downloadFile`](../src/tools.ts#L579-L606). Schemas: [input](../src/tools.ts#L301-L310), [output](../src/tools.ts#L396-L411).

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

FOLDER is `group-N` for `#N` and `contact-N` for `@N`; the extension is the file name's extension when it has 1 to 16 ASCII letters and digits ([messages.ts:215-227](../src/messages.ts#L215-L227)). `/forward` downloads a file to `DATA/forwarded/FOLDER/` under the same name; a later perform of the download moves it to the download path, after the copy for the post is made ([tools.ts:579-596](../src/tools.ts#L579-L596)).

Steps before the action:

| Step | Code |
|---|---|
| a file in status `rcvInvitation`, or an XFTP file: the file's servers from `apiGetChatItemInfo`; while none, the call waits for `rcvFileDescrReady` until `wait` seconds after the call, with the progress `The sender is uploading the file of item N in CHAT; waiting until TIME.` | [tools.ts:1088-1105](../src/tools.ts#L1088-L1105) |
| unknown servers: the file's servers that are not among the XFTP servers of `apiGetUserServers`, of no operator or of an enabled one, compared without their passwords | [tools.ts:1107-1111](../src/tools.ts#L1107-L1111) |
| without unknown servers, and for a file ID not in rejected files, the chat's files approval, read again, makes the action performed at once | [tools.ts:1081](../src/tools.ts#L1081) |

Perform: `apiReceiveFile(fileId, path, approvedRelays)`, for a file in status `rcvInvitation`. `approvedRelays` is true when the action's mode is `requests` and its card named unknown servers, and false otherwise; the core then checks the complete description ([tools.ts:602](../src/tools.ts#L602), [:1129-1138](../src/tools.ts#L1129-L1138)). The download ends at `rcvFileComplete`, `rcvFileError`, `rcvFileSndCancelled` or `chatItemUpdated` of its file, or at a check every 60 s ([tools.ts:618-640](../src/tools.ts#L618-L640), [:1140-1208](../src/tools.ts#L1140-L1208)).

Failure texts of the perform:

| Text | Condition | Code |
|---|---|---|
| the file's error, as in the file field | the file failed | [tools.ts:1169-1179](../src/tools.ts#L1169-L1179) |
| `The sender cancelled the file.` | `rcvFileSndCancelled` | [tools.ts:635-640](../src/tools.ts#L635-L640) |
| `The message or its chat was deleted.` | a check finds no message, contact or group | [tools.ts:171](../src/tools.ts#L171), [:1159-1167](../src/tools.ts#L1159-L1167) |
| `The download made no progress for 30 minutes and was cancelled.` | no rise of `rcvProgress` for 30 minutes; the file is cancelled | [tools.ts:175](../src/tools.ts#L175), [:1152-1156](../src/tools.ts#L1152-L1156) |
| `The received file is not in the agent's files folder.` | the file is received and neither the download path nor the forwarded path has a file | [messages.ts:14](../src/messages.ts#L14), [tools.ts:1181-1185](../src/tools.ts#L1181-L1185) |
| `Your owner deleted the files of this chat.` | `/delete CHAT files` | [tools.ts:172](../src/tools.ts#L172), [:1016-1034](../src/tools.ts#L1016-L1034) |
| `The file is on unknown servers that were not approved.` | a check reads an accepted file in status `rcvInvitation`: the core returned it to the offer when its description completed | [tools.ts:173](../src/tools.ts#L173), [:1148](../src/tools.ts#L1148) |
| `Unknown servers! Without Tor or VPN, your IP address will be visible to these XFTP relays: HOST, HOST.` | the core refuses the servers of the complete description at the acceptance | [log.ts:78-79](../src/log.ts#L78-L79) |

Tool errors:

| Text | Condition | Code |
|---|---|---|
| `No chat CHAT. list shows the agent's chats.` | invalid reference, the control chat, or a store error when the chat is read | [tools.ts:1071](../src/tools.ts#L1071) |
| `item: CHAT has no item N.` | the item is not found | [tools.ts:1073](../src/tools.ts#L1073) |
| `item: item N in CHAT has no received file.` | the item is not received or has no file | [tools.ts:1075](../src/tools.ts#L1075) |
| `item: the file of item N in CHAT failed: ERROR` | the file field has status `failed` | [tools.ts:1076-1077](../src/tools.ts#L1076-L1077) |
| `item: the sender has not uploaded the file of item N in CHAT yet; call download again later.` | no servers by the end of `wait`, also for a file invited without a description, whose servers the core does not list; no action is created | [tools.ts:1079](../src/tools.ts#L1079), [:1103](../src/tools.ts#L1103) |
| refusals | see [1.9](#19-action-refusals) | |

A file ID whose download returned `rejected` or `expired` shows status `rejected` until a download of it is done or the server restarts ([tools.ts:1118-1119](../src/tools.ts#L1118-L1119)).

### 1.6 `list`

Handler: [`AgentTools.listChats`](../src/tools.ts#L1256-L1264). Schemas: [input](../src/tools.ts#L290), [output](../src/tools.ts#L351-L383).

Input: an empty object.

Output:

| Field | Item fields |
|---|---|
| `chats` | `chat`<br>`type`<br>`name`<br>`simplex_name`, when verified<br>`peer_type`, when present<br>`incognito`<br>`known_as`, when incognito<br>`connected`<br>`unread`: `chatStats.unreadCount`<br>`approved: true`, only under a standing approval of messages in mode `requests` |
| `actions` | `action`<br>`tool`<br>`input`: the input of the creating call<br>`state`<br>`open`: `true` while a call waits on it |

`chats`: the contacts and groups of the main conversations from one read, `apiGetChats(USER, last 1000000)`, except the control chat; in the plugin, only the chats of the calling session ([tools.ts:1266-1274](../src/tools.ts#L1266-L1274), [:569-571](../src/tools.ts#L569-L571)). `actions`: the actions in memory of the calling session, all actions for `serve` ([tools.ts:1262](../src/tools.ts#L1262)).

### 1.7 `leave`

Handler: [`AgentTools.leave`](../src/tools.ts#L1042-L1050). Perform: [`leaveChat`](../src/tools.ts#L1056-L1064). Schemas: [input](../src/tools.ts#L292-L299), [output](../src/tools.ts#L385-L394).

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

Tool errors: `No chat CHAT. list shows the agent's chats.` when no action with the same key is in memory and the chat is invalid, the control chat or unknown ([tools.ts:1045-1047](../src/tools.ts#L1045-L1047)); refusals.

### 1.8 Message, file and command objects

Message ([`Message`](../src/messages.ts#L16-L28), [`toMessage`](../src/messages.ts#L174-L192), [schema](../src/tools.ts#L188-L221)):

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

Only received message items (`rcvMsgContent`) are returned by `send` and `receive`; `connect` also returns sent ones (`sndMsgContent`) ([messages.ts:101](../src/messages.ts#L101), [tools.ts:768](../src/tools.ts#L768)).

Command ([`commandsOf`](../src/tools.ts#L1535-L1542), [schema](../src/tools.ts#L223-L233)):

| Form | Fields |
|---|---|
| command | `keyword`<br>`label`<br>`params`, when present |
| menu | `label`<br>`commands`: commands and menus |

### 1.9 Action refusals

Refusals of a new `connect`, `send`, `download` or `leave` action of `serve`, first match ([actions.ts:196-201](../src/actions.ts#L196-L201), [control.ts:129-139](../src/control.ts#L129-L139)); the plugin's texts are in [4.4](#44-tool-results-in-the-plugin):

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

Largest card form ([control.ts:677-685](../src/control.ts#L677-L685)): the maximum of the creation form, the `done` form and a `failed` form with an error of 600 × `€`, with the action's own decider or the longest current approver name; each form is measured as JSON-escaped UTF-8: `"`, `\`, `\n`, `\r`, `\t` 2 bytes, other characters below U+0020 6 bytes.

Link texts of a `connect` card ([control.ts:132](../src/control.ts#L132), [:699-713](../src/control.ts#L699-L713)): the description and welcome lines take the bytes that the largest form leaves without them. Lines are kept while they fit; the next line is cut to the bytes left and ends in `…`; later lines are left out. The tool result keeps both texts whole.

---

## 2. Agent control chat

### 2.1 Commands and replies

Accepted text ([`parseCommand`](../src/control.ts#L593-L598), [`ownerRequest`](../src/control.ts#L761-L775)):

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

The server reads commands from members' messages in the main conversation of the control chat ([serve.ts:129](../src/serve.ts#L129), [control.ts:190-200](../src/control.ts#L190-L200)). It answers a command with a reply that quotes it, one command after another, and it runs the expiry check before each command ([control.ts:198](../src/control.ts#L198), [:221](../src/control.ts#L221)).

| Request | Condition | Reply | Code |
|---|---|---|---|
| one-line `/` text outside the requests | any sender | `Commands: /approve, /reject, /receive, /forward, /revoke, /leave, /delete, /pending, /list. Each card shows its commands.` | [control.ts:16](../src/control.ts#L16), [:223](../src/control.ts#L223) |
| any request | sender not an approver, `--owners` absent | `Only owners approve actions.` | [control.ts:224](../src/control.ts#L224) |
| any request | sender not an approver, `--owners` given | `Only the owners listed in --owners approve actions.` | [control.ts:224](../src/control.ts#L224) |
| `approve`, `reject`, `receive`, `forward` with N | action N absent, not `pending`, or without a card | `Action N is not pending.` | [control.ts:249](../src/control.ts#L249) |
| the same with CHAT | action N is a `connect`, or for another chat | `Action N is not for CHAT.` | [control.ts:250](../src/control.ts#L250) |
| `approve` | action N is a download | `Action N is a download: COMMANDS.`, COMMANDS the card's command line | [control.ts:256](../src/control.ts#L256) |
| `receive`, `forward`, `receive CHAT N all` | action N is not a download | `Action N is not a download.` | [control.ts:254](../src/control.ts#L254) |
| `approve`, `reject`, `receive`, `forward` | accepted | none; the card is edited at the outcome; `forward` posts the file, as a reply to the card when the message with the card as its quote is within 15602 bytes, and the card stays pending | [control.ts:251-260](../src/control.ts#L251-L260), [:287-303](../src/control.ts#L287-L303) |
| `approve CHAT all` | mode `host`; nothing is stored | `Approvals apply in mode requests; this server runs in mode host.` | [control.ts:233](../src/control.ts#L233) |
| `approve CHAT all`, `receive CHAT N all` | CHAT unknown or the control chat | `No chat CHAT. /list shows the agent's chats.` | [control.ts:268](../src/control.ts#L268) |
| `approve CHAT all` | accepted; the pending sends of CHAT with a card are approved | `Approved for KIND NAME (CHAT) by APPROVER. /'revoke CHAT'` | [control.ts:265-276](../src/control.ts#L265-L276) |
| `receive CHAT N all` | accepted; download N is performed, and the other pending downloads of CHAT stay pending | `Files approved for KIND NAME (CHAT) by APPROVER. /'receive CHAT off'` | [control.ts:259](../src/control.ts#L259), [:273-275](../src/control.ts#L273-L275) |
| `revoke CHAT` | CHAT unknown, or without a standing approval | `Messages to CHAT have no standing approval.` | [control.ts:281](../src/control.ts#L281) |
| `revoke CHAT` | accepted | `Messages to KIND NAME (CHAT) need approval again.` | [control.ts:284](../src/control.ts#L284) |
| `receive CHAT off` | CHAT unknown, or without a files approval | `Files from CHAT have no standing approval.` | [control.ts:281](../src/control.ts#L281) |
| `receive CHAT off` | accepted | `Files from KIND NAME (CHAT) need approval again.` | [control.ts:284](../src/control.ts#L284) |
| `leave CHAT` | CHAT unknown or the control chat | `No chat CHAT. /list shows the agent's chats.` | [control.ts:307](../src/control.ts#L307) |
| `leave CHAT` | accepted, at once in both modes; the pending actions of CHAT with a card are rejected by the sender | `Left KIND NAME (CHAT), requested by NAME.`, or `Deleted ...` for a contact; logged too | [control.ts:305-319](../src/control.ts#L305-L319) |
| `delete CHAT files` | the chat's downloads in memory fail and are cancelled; its other files in status `rcvAccepted`, `rcvTransfer` or `rcvWarning`, such as downloads the core continues after a restart, are cancelled; the folder of CHAT and its forwarded folder are deleted, for a current or a left chat | `Deleted K files from CHAT, SIZE.` (`1 file`), or `No files from CHAT.` | [control.ts:326-329](../src/control.ts#L326-L329), [tools.ts:1016-1034](../src/tools.ts#L1016-L1034), [messages.ts:275-283](../src/messages.ts#L275-L283) |
| `pending` | no pending action with a card | `No pending actions.` | [control.ts:228](../src/control.ts#L228) |
| `pending` | otherwise | one line per action: `N  WHAT  expires YYYY-MM-DD HH:MM UTC` | [control.ts:227](../src/control.ts#L227), [:673-675](../src/control.ts#L673-L675) |
| `list` | no chats | `No chats.` | [control.ts:361](../src/control.ts#L361) |
| `list` | otherwise | one line per chat of the 100 most recent: `CHAT  KIND NAME`, then `, approved for this chat by APPROVER`, `, files approved by APPROVER` and `, not connected` when applicable, cut to 100 bytes with `…`; then `K older chats are not shown.` (`1 older chat is not shown.`) | [control.ts:349-362](../src/control.ts#L349-L362) |

KIND is `contact`, `bot`, `agent`, `business`, `group` or `channel` ([tools.ts:1481-1488](../src/tools.ts#L1481-L1488)). WHAT is `connect to SUBJECT`, `send to SUBJECT`, `download NAME from SUBJECT`, `delete SUBJECT` or `leave SUBJECT` ([actions.ts:376-390](../src/actions.ts#L376-L390)). SIZE is in bytes, KB, MB or GB, by 1000, with one decimal ([control.ts:725-728](../src/control.ts#L725-L728)). Names are shown by `safeName` ([control.ts:742-745](../src/control.ts#L742-L745)).

A standing approval is stored in the chat's custom data, `simplexMcp` for messages and `simplexMcpFiles` for files, as `{approvedBy, approvedById, approvedAt}`; it applies in mode `requests` while member `approvedById` is an approver ([control.ts:22-25](../src/control.ts#L22-L25), [:141-144](../src/control.ts#L141-L144)).

### 2.2 Cards

Card text: request lines, then one last line ([`cardText`](../src/control.ts#L600-L619), [`requestLines`](../src/control.ts#L621-L654)).

Request lines:

| Tool | First line | Further lines |
|---|---|---|
| `connect` | `I want to connect to SUBJECT, as myself.` or `, incognito.`<br>done and connected: `I connected to ...`<br>done and not connected: `I asked to connect to ...` | link description, one code span per line<br>welcome, one code span per line<br>both cut to fit the card ([1.9](#19-action-refusals))<br>with `message`: `With the message:` and its lines |
| `send` | `I want to send a message to SUBJECT:`, with `, where I am incognito` before the colon in incognito chats<br>done: `I sent a message to ...` | text, one code span per line<br>with `file`: ``attaching TYPE `PATH`, SIZE``<br>`replying to NAME: QUOTE` with `reply_to`, the quote cut to 100 characters<br>`mentioning @A, @'B C'` with `mention` |
| `download` | `I want to download a file from SENDER in KIND NAME (CHAT):`, without `SENDER in ` outside groups<br>done: `I downloaded a file from ...` | ``` `NAME`, SIZE ```<br>an XFTP file: `Servers: known.`, or ``Servers: `HOST`, `HOST`, unknown: downloading shows them this computer's IP address.`` (`it` for one) |
| `leave` | `I want to delete KIND NAME (@N).` or `I want to leave KIND NAME (#N).`<br>done: `I deleted ...` or `I left ...` | |

SUBJECT is `KIND NAME`, or `an unnamed KIND`, followed by ` (SIMPLEX_NAME)` when known ([actions.ts:371-374](../src/actions.ts#L371-L374)). For `connect`, SIMPLEX_NAME is the target when it is a name; for `send`, the chat's verified SimpleX name.

A card of a `send` with an image or video file, or of a `download` of an image or video, is an image or video message with the file's preview and the card text as its caption; the preview is resized to fit the card, and the card is text when it can't be resized ([control.ts:477-497](../src/control.ts#L477-L497)).

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

DECIDER is `Performed without approval (mode host)` in mode `host`, `Approved for this chat by NAME` for a standing action, and `Approved by NAME` otherwise ([control.ts:602](../src/control.ts#L602)).

Command lines ([`commandsLine`](../src/control.ts#L656-L671)):

| Tool | Command line |
|---|---|
| `connect` | `/'approve N' or /'reject N'` |
| `send` | `/'approve CHAT N', /'approve CHAT all' or /'reject CHAT N'` |
| `download` | `/'receive CHAT N', /'receive CHAT N all', /'forward CHAT N' or /'reject CHAT N'`, without `/'receive CHAT N all'` for unknown servers |
| `leave` | `/'approve CHAT N' or /'reject CHAT N'` |

The app shows `/'approve N'` as the tappable command `/approve N` ([PLAN, What the owner sees](../PLAN.md#what-the-owner-sees-the-agent-control-chat)).

An action performed without a card, in mode `host` or under a standing approval, gets its outcome as a new message; a `download` without a card gets none ([control.ts:164-171](../src/control.ts#L164-L171), [:532-534](../src/control.ts#L532-L534)).

### 2.3 Other posts

| Situation | Text | Code |
|---|---|---|
| `send` with a refused file; logged too | ``Refused to send `PATH` to KIND NAME (CHAT): private keys and the server's own files are not sent.`` | [control.ts:173-177](../src/control.ts#L173-L177) |
| `sndFileError` of a sent file; logged too | ``The file `NAME` was not sent to KIND NAME (CHAT): `REASON`.`` | [control.ts:179-188](../src/control.ts#L179-L188), [serve.ts:133](../src/serve.ts#L133) |
| `/forward` of a file that is not received or not posted | ``Action N: the file was not received: `ERROR`.`` | [control.ts:301](../src/control.ts#L301) |
| after the reply to `/leave CHAT`, and after the outcome of a done `leave`, when the folder of CHAT has files | ``Files from KIND NAME (CHAT) are kept in `FOLDER`: K files, SIZE. /'delete CHAT files'`` | [control.ts:169-170](../src/control.ts#L169-L170), [:316-324](../src/control.ts#L316-L324) |

### 2.4 Membership messages

Posted in the chat and logged; in the chat, names are shown by `safeName` and the `--owners ...` part is a code span; in the log, names are folded to one line ([control.ts:463-466](../src/control.ts#L463-L466), [:78-79](../src/control.ts#L78-L79)).

| Situation | Text | Code |
|---|---|---|
| owner joined, `--owners` absent | `I'm your agent. I ask here before I connect, send, download a file or leave a chat.` (chat only) | [control.ts:31](../src/control.ts#L31), [:411](../src/control.ts#L411) |
| member joined, `--owners` without a list | `NAME joined (member ID). Restart the server with --owners ENTRIES` | [control.ts:416-419](../src/control.ts#L416-L419) |
| start with `--owners` without a list and humans present | `A joined (member ID). B joined (member ID). Restart the server with --owners ENTRIES` | [control.ts:383-386](../src/control.ts#L383-L386) |
| a human became owner outside the list | `NAME (member ID) is now an owner and can't approve actions yet. Restart the server with --owners ENTRIES` | [control.ts:424-428](../src/control.ts#L424-L428) |
| a listed owner stopped being a human owner | `NAME (member ID) is no longer an owner. Restart the server with --owners ENTRIES`<br>none remains: `NAME (member ID) is no longer an owner. No owners remain: restart the server with --reset-owners --owners` | [control.ts:429-434](../src/control.ts#L429-L434) |

ENTRIES: `ID:NAME` of each human, or of each human owner, joined by commas; an entry with characters other than letters, digits, `_` and `-` is quoted for the shell ([control.ts:810-820](../src/control.ts#L810-L820)).

### 2.5 Group profile

| Field | Value | Code |
|---|---|---|
| display name | the agent's display name, updated at start when it differs | [control.ts:105-107](../src/control.ts#L105-L107) |
| full name | empty | [control.ts:549](../src/control.ts#L549) |
| description | `Supervise your AI agent here: approve or reject its connections and messages with one tap.` | [control.ts:550](../src/control.ts#L550) |
| history | on | [control.ts:551](../src/control.ts#L551) |
| command menu | `/pending` "Pending actions"<br>`/list` "Chats and approvals"<br>`/leave` "Leave a chat", params `<chat ID>`<br>updated at start when it differs | [control.ts:10-14](../src/control.ts#L10-L14), [:105-107](../src/control.ts#L105-L107) |

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
| 2 | `--owners: member ENTRY is not in the agent control chat. Start with --reset-owners --owners, or use the IDs of current members.` | [control.ts:392](../src/control.ts#L392) |
| 1 | `Another server uses DATA (PID N). If no server runs, remove DATA/lock.` | [files.ts:94-96](../src/files.ts#L94-L96), [:154-156](../src/files.ts#L154-L156) |
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
| control chat created | `Agent control chat "DISPLAY" created.` | [control.ts:553](../src/control.ts#L553) |
| link opened | `Join the agent control chat in the SimpleX app. The link works once:`<br>QR code<br>link<br>small QR code: `If the QR code does not scan, restart with --qr-code large, or paste the link into the app.`<br>role line | [control.ts:442-452](../src/control.ts#L442-L452) |
| listening | `Listening on http://127.0.0.1:PORT/mcp` | [serve.ts:122](../src/serve.ts#L122) |
| owner setup done | `Add the agent to Claude Code:`<br>`  npx -y @simplex-chat/mcp register NAME [--data DATA]` | [serve.ts:157-159](../src/serve.ts#L157-L159) |
| owner joined | `NAME joined (member ID) and is now an owner. The link is deleted.` | [control.ts:412](../src/control.ts#L412) |
| listed owners set | `Owners: ENTRIES.` | [control.ts:403](../src/control.ts#L403) |
| approvers gone, `--owners` absent | `No owners in the agent control chat.` | [control.ts:32](../src/control.ts#L32), [:214](../src/control.ts#L214), [:379](../src/control.ts#L379) |
| control chat lost | `The agent lost its control chat. Restart the server with --reset-owners.` | [control.ts:439](../src/control.ts#L439) |
| action created | `Action N: WHAT` with `, as itself` or `, incognito` for `connect` and ` (CHAT)` for the other tools | [actions.ts:205-206](../src/actions.ts#L205-L206), [:376-390](../src/actions.ts#L376-L390) |
| counter write or card post failed | `Action N: failed: ERROR.` | [actions.ts:320](../src/actions.ts#L320) |
| perform ended, not interrupted by the stop, mode `requests` | `Action N approved by NAME: RESULT` with RESULT `sent.`, `downloaded.`, `deleted.`, `left.`, `connected.`, `connection requested.` or `failed: ERROR.` | [actions.ts:341](../src/actions.ts#L341), [:396-406](../src/actions.ts#L396-L406) |
| perform ended, not interrupted by the stop, mode `host` | `Action N: performed without approval (mode host).`, or `...; failed: ERROR.` | [actions.ts:398](../src/actions.ts#L398) |
| rejected | `Action N rejected by NAME.` | [actions.ts:267](../src/actions.ts#L267) |
| expired | `Action N expired.` | [actions.ts:304](../src/actions.ts#L304) |
| `/leave CHAT` | `Left KIND NAME (CHAT), requested by NAME.` or `Deleted ...` | [control.ts:311](../src/control.ts#L311) |
| refused file, file not sent | as in [2.3](#23-other-posts), names and paths on one line | [control.ts:175](../src/control.ts#L175), [:186](../src/control.ts#L186) |
| stop | `Cancelled K action.` or `Cancelled K actions.` when K > 0<br>`Stopping: sending K cancelled card(s), up to S s. Ctrl-C to stop now.` when cards were edited or posted and S > 0<br>`Stopped.` | [serve.ts:148](../src/serve.ts#L148), [:161-175](../src/serve.ts#L161-L175), [:152](../src/serve.ts#L152) |
| membership messages | see [2.4](#24-membership-messages) | |

stderr: `CONTEXT: ERROR` from `logError` ([log.ts:41-43](../src/log.ts#L41-L43)), with CONTEXT one of `action N`, `action N card`, `action N card image`, `action N file`, `control chat command`, `EVENT in the agent control chat`, `EVENT event`, `refused file`, `file not sent`, `download of file N`, `file N`, `moving file N`, `cancelling file N`, `resizing PATH`, `preview`, `MCP`, `HTTP request`, `stop`, `simplex-mcp`. The library's download lines are written to stderr.

---

## 4. Claude Code plugin

### 4.1 Tools in the plugin

| Rule | Code |
|---|---|
| The six tools of [1](#1-tools) and `reply` are registered on the session's `McpServer`; Claude Code names them `mcp__plugin_simplex_simplex__NAME` | [session.ts:54](../src/session.ts#L54), [hooks.json:4](../hooks/hooks.json#L4) |
| `_meta: {"anthropic/requiresUserInteraction": true}` marks the four action tools when the option `approve` is `host` at the session process's start | [session.ts:54](../src/session.ts#L54), [tools.ts:496](../src/tools.ts#L496) |
| Instructions: those of [1.1](#11-common-rules) for the display name `Claude` and the host `SimpleX daemon`, followed by the channel's | [session.ts:18](../src/session.ts#L18), [:49](../src/session.ts#L49), [tools.ts:440-451](../src/tools.ts#L440-L451) |
| `send`, `receive`, `download`, `leave` and `list` see the session's own chats only; another session's chat is `No chat CHAT. list shows the agent's chats.` | [tools.ts:569-571](../src/tools.ts#L569-L571), [:1276-1285](../src/tools.ts#L1276-L1285) |
| `connect` to a chat of another session: `The chat of TARGET belongs to another session; an owner can give it to this session with /give.` | [tools.ts:733-735](../src/tools.ts#L733-L735) |
| Actions are numbered per session group, from 1, and the action key holds the session number | [hub.ts:64](../src/hub.ts#L64), [tools.ts:1674-1676](../src/tools.ts#L1674-L1676) |
| Downloads are saved under `ROOT/_sessions/ID/files/` | [hub.ts:79](../src/hub.ts#L79), [messages.ts:225-227](../src/messages.ts#L225-L227) |

`reply` ([`replyTool`](../src/tools.ts#L461-L475), [`AgentTools.reply`](../src/tools.ts#L705-L713)):

| Field | Type | Range | Default | Rule |
|---|---|---|---|---|
| `file` | string | 1 character or more | required | path of a file of this computer |
| `text` | string | 1 to 12000 characters | | the caption |

Output: `{item}`, the item ID of the message in the session group. The file is checked and sent as for `send` ([1.3](#13-send)): types, refused paths, empty files and the change check; it is sent without a card. Tool error without a group: `reply: this server has no group to reply to.`

### 4.2 Main group

Commands ([`MainGroup.command`](../src/main-group.ts#L116-L121)), one line `/KEYWORD PARAMS` as in [2.1](#21-commands-and-replies), from any member:

| Request | Condition | Reply | Code |
|---|---|---|---|
| `/sessions` | no session | `No sessions.` | [hub.ts:307-310](../src/hub.ts#L307-L310) |
| `/sessions` | otherwise | one line per session of the hub: `N · NAME`, with `, closed` when it has no entry | [hub.ts:307-310](../src/hub.ts#L307-L310) |
| `/join N` | no session N | `No session N. /sessions lists the sessions.` | [hub.ts:314](../src/hub.ts#L314) |
| `/join N` | the sender is a member of its group | `You are a member of "N · NAME".` | [hub.ts:315-320](../src/hub.ts#L315-L320) |
| `/join N` | otherwise: an invitation through the Main group, with the sender's role there, recorded in `invited` | `Sent you an invitation to "N · NAME".` | [hub.ts:322](../src/hub.ts#L322), [:166-170](../src/hub.ts#L166-L170) |
| `/approve reset`, `/reject reset` | sender not an owner | `Only owners decide a reset of the owners.` | [main-group.ts:124](../src/main-group.ts#L124) |
| `/approve reset`, `/reject reset` | no reset card | `No reset of the owners is requested.` | [main-group.ts:126](../src/main-group.ts#L126) |
| `/approve reset`, `/reject reset` | accepted | none; the card is edited to `Approved by NAME.` or `Rejected by NAME.`; after an approval the group is replaced | [main-group.ts:127-132](../src/main-group.ts#L127-L132) |
| other one-line `/` text | | `Commands: /sessions, /join N. Owners decide a requested reset of the owners with /'approve reset' or /'reject reset'.` | [main-group.ts:13](../src/main-group.ts#L13), [:120](../src/main-group.ts#L120) |

Posts:

| Moment | Text | Code |
|---|---|---|
| the first owner joined | `I'm Claude. Each Claude Code session has its own group, and you get its invitation here. /sessions lists the sessions; /join N joins the group of session N.` | [main-group.ts:12](../src/main-group.ts#L12), [:154](../src/main-group.ts#L154) |
| `reset-owners` while owners exist | `A terminal asks to replace the owners: this group is deleted, and a new Main group with a new link replaces it.` and `/'approve reset' or /'reject reset'`; one card at a time | [main-group.ts:14](../src/main-group.ts#L14), [:96-100](../src/main-group.ts#L96-L100) |

Group profile ([`createMainGroup`](../src/main-group.ts#L208-L217), [`load`](../src/main-group.ts#L60-L67)):

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
| any request | the sender is not an approver | `Only owners approve actions.` | [control.ts:224](../src/control.ts#L224) |
| `/give CHAT N` | CHAT is not a chat of this session | `No chat CHAT. /list shows the agent's chats.` | [control.ts:334](../src/control.ts#L334) |
| `/give CHAT N` | no session N | `No session N. /sessions in the Main group lists the sessions.` | [control.ts:336](../src/control.ts#L336) |
| `/give CHAT N` | accepted: CHAT's owner set to session N; the pending actions of CHAT with a card are rejected by the sender | `Gave KIND NAME (CHAT) to session N.` | [control.ts:337-342](../src/control.ts#L337-L342) |
| `/list`, `/revoke`, `/leave` | | the chats of this session, also while it is closed | [tools.ts:1266-1285](../src/tools.ts#L1266-L1285) |
| `/give CHAT N` in the control chat of `serve` | | the help reply | [control.ts:332](../src/control.ts#L332) |

Cards of [2.2](#22-cards), and two more:

| Tool | Request lines | Command line |
|---|---|---|
| `mode` | `I want to run in mode host: Claude Code asks you before each of my actions, and they are performed without approval here.`<br>done: `I run in mode host: ...` | `/'approve N' or /'reject N'` |
| `permission` | ``I want to use `TOOL`: `DESCRIPTION` ``, then the input preview, one code span per line | `/'approve N' or /'reject N'` |

Both are exempt from the card size check ([control.ts:133](../src/control.ts#L133), [:621-627](../src/control.ts#L621-L627), [:656-660](../src/control.ts#L656-L660)). In `/pending` their WHAT is `run in mode MODE` and `use TOOL` ([actions.ts:377-378](../src/actions.ts#L377-L378)).

Posts:

| Poster | Text | Code |
|---|---|---|
| Claude | the final answer of each turn, in parts of 12000 bytes, the first quoting the turn's prompt from the group when it fits | [hub.ts:195-201](../src/hub.ts#L195-L201) |
| Claude | `Session N is not running.`, as a reply to an approver's message while the session has no attachment | [hub.ts:123](../src/hub.ts#L123) |
| `User` | each prompt typed in the terminal, in parts of 12000 bytes, except the prompts pushed from the group | [hub.ts:144-147](../src/hub.ts#L144-L147), [user.ts:29-32](../src/user.ts#L29-L32) |
| Claude | files sent by `reply` | [tools.ts:705-713](../src/tools.ts#L705-L713) |

Group profile ([`createSessionGroup`](../src/hub.ts#L358-L370), [`Control.load`](../src/control.ts#L102-L109), [`describe`](../src/control.ts#L116-L119)):

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
| `The SimpleX daemon is stopping.` | the daemon stops | [hub.ts:26](../src/hub.ts#L26) |
| `The group of this session is lost. Start a new Claude Code session.` | the session group was deleted, Claude was removed from it or lost its owner role | [hub.ts:26](../src/hub.ts#L26), [control.ts:777-792](../src/control.ts#L777-L792) |
| the 20-action limits and the card size texts of [1.9](#19-action-refusals) | per session | [actions.ts:195-200](../src/actions.ts#L195-L200) |
| `The group of this session has no owner. An owner of the Main group joins it by its invitation there; without owners, run npx -y @simplex-chat/mcp link in a terminal.` | the session group has no approver | [control.ts:34](../src/control.ts#L34), [:135](../src/control.ts#L135) |

Errors of the session process ([session.ts:37-63](../src/session.ts#L37-L63), [:96-128](../src/session.ts#L96-L128), [:161-168](../src/session.ts#L161-L168)):

| Text | Condition |
|---|---|
| `Plugin option KEY: EXPECTED expected, got VALUE. Change it in /plugin > Installed > simplex > Configure options.` | an invalid option at the session's start; every call |
| `The SimpleX daemon stopped; repeat the call.` | the daemon is not running, is stopping (503), or its stream ended without a result |
| `Restart Claude Code to update the SimpleX plugin.` | the daemon speaks another protocol version |
| `The SimpleX daemon exited; see ROOT/daemon.log`, `The SimpleX daemon did not start in 5 minutes.`, or the daemon's error text | the daemon could not start |
| `Unknown session; restart the Claude Code session.` | after one repeat, the daemon has no folder for the session |
| `Cancelled.` | the daemon returned `{result: null}` for a call that was not aborted |

### 4.5 Channel

Capabilities of the session server: `experimental: {"claude/channel": {}, "claude/channel/permission": {}}` ([session.ts:49](../src/session.ts#L49)).

| Direction | Method | Params | Code |
|---|---|---|---|
| to Claude Code | `notifications/claude/channel` | `content`: the prompt's text<br>`meta.from`: the owner's display name<br>`meta.item`: the item ID in the session group, as a string<br>`meta.file`: the path of the prompt's file, when it has one | [session.ts:171](../src/session.ts#L171) |
| to Claude Code | `notifications/claude/channel/permission` | `request_id`<br>`behavior`: `allow` or `deny` | [session.ts:172](../src/session.ts#L172) |
| from Claude Code | `notifications/claude/channel/permission_request` | `request_id`, `tool_name`, `description`, `input_preview`, all strings | [session.ts:19-20](../src/session.ts#L19-L20), [:55-58](../src/session.ts#L55-L58) |

Channel instructions: `Messages in the simplex channel are prompts of your owners in the SimpleX group of this session: from is the owner's name, item the message ID, and file the path of a file they sent. Your final answer of each turn reaches them in that group; reply sends them a file.` ([session.ts:18](../src/session.ts#L18)).

A prompt's file is received without a card, with relays approved, into `ROOT/_sessions/ID/files/group/<file ID><extension>`; the prompt is pushed after `rcvFileComplete`, and dropped after `rcvFileError` ([hub.ts:186-193](../src/hub.ts#L186-L193), [:159-164](../src/hub.ts#L159-L164)).

### 4.6 Socket protocol

HTTP/1.1 over `ROOT/daemon.sock`, the fallback path or the Windows pipe, protocol version `1`. Every request carries `Authorization: Bearer TOKEN`; bodies are JSON; streamed replies are NDJSON with the content type `application/x-ndjson`. Errors are `{"error": TEXT}` with a status other than 200. Types: [`protocol.ts`](../src/protocol.ts); routes: [`Daemon.routes`](../src/daemon.ts#L115-L138).

| Route | Request | Reply | Code |
|---|---|---|---|
| `POST /session` | `{id, project, name?, config: {mainGroup, approve, expireHours}}` | `{number, displayName, version, protocol, owner: {owners, link?}}` | [daemon.ts:177-184](../src/daemon.ts#L177-L184) |
| `POST /title` | `{id, name?, project, model?, config}` | `{mainGroup, owner, version, protocol}` | [daemon.ts:186-193](../src/daemon.ts#L186-L193) |
| `POST /attach` | `{id}` | stream held open: first line `{version, protocol}`, then `{prompt}` and `{permission}` lines | [daemon.ts:202-220](../src/daemon.ts#L202-L220) |
| `POST /call` | `{id, tool, input}` | lines `{progress: TEXT}`, then one line `{result: CallToolResult or null}`; closing the request aborts the call | [daemon.ts:222-239](../src/daemon.ts#L222-L239) |
| `POST /permission` | `{id, requestId, toolName, description, inputPreview}` | `{}` after the card's action is created | [daemon.ts:121-125](../src/daemon.ts#L121-L125) |
| `POST /turn` | `{id, prompt?, answer?, model?, effort?}` | `{}` after the posts | [daemon.ts:126-130](../src/daemon.ts#L126-L130) |
| `GET /status` | none | `{version, protocol, mainGroup, owner, sessions: [{number, name, attached}]}`, the open sessions | [daemon.ts:318-321](../src/daemon.ts#L318-L321) |
| `POST /reset-owners` | none | `{link}` after a reset without owners; `{}` after the card | [daemon.ts:132](../src/daemon.ts#L132), [hub.ts:277-281](../src/hub.ts#L277-L281) |
| `POST /stop` | none | `{}` after the stop sequence; the process exits after the reply | [daemon.ts:133-136](../src/daemon.ts#L133-L136) |

| Status | Error | Condition |
|---|---|---|
| 401 | `Unauthorized` | token missing or wrong |
| 404 | `Not found` | other method or path |
| 400 | `Invalid JSON` | body not JSON |
| 400 | `Not a Claude Code session ID: ID` | a session route with an ID that is not a UUID |
| 409 | `Unknown session; restart the Claude Code session.` | `/attach`, `/call`, `/permission` or `/turn` for an ID without a folder |
| 413 | `Body too large` | body over 1 MiB |
| 503 | `The SimpleX daemon is stopping.` | a session route while the daemon stops |
| 500 | the error message | any other error, also a refused `permission` action |

Session routes are `/session`, `/title`, `/attach`, `/call`, `/permission` and `/turn` ([daemon.ts:252-285](../src/daemon.ts#L252-L285)). `/session` and `/title` apply `config` to the hub: the expiry of all sessions, and a rename of the Main group when `mainGroup` differs ([hub.ts:271-275](../src/hub.ts#L271-L275)). `/session` also requests its session's mode. `/title` opens or creates the session, renames it for a non-blank name, and passes `model` to `turn`.

### 4.7 Plugin commands

| Command | Run by | Output |
|---|---|---|
| `simplex-mcp session` | `.mcp.json` | MCP over stdio |
| `simplex-mcp daemon` | `spawnDaemon` | ready line on fd `SIMPLEX_MCP_READY_FD`, stdout when unset: `ready`, `running` or `error: TEXT`; the log on stdout and stderr |
| `simplex-mcp hook session-start` | SessionStart hook | one JSON line, see [4.8](#48-plugin-options-files-and-hook-output) |
| `simplex-mcp hook turn` | UserPromptSubmit, Stop and PostModelSwitch hooks | `{}` |
| `simplex-mcp link` | `/simplex:link`, people | `Owners of "MAIN": A, B. To replace them, run /simplex:reset-owners.`; without owners on a terminal `Join "MAIN" in the SimpleX app. The link works once:`, a QR code and the link, elsewhere `Run npx -y @simplex-chat/mcp link in a terminal to see the link to "MAIN".`; without a link `The Main group has no link. Run npx -y @simplex-chat/mcp reset-owners in a terminal.` |
| `simplex-mcp reset-owners` | `/simplex:reset-owners`, people | while owners exist: `An owner approves the reset in "MAIN". Then run npx -y @simplex-chat/mcp link in a terminal to see the link to the new group.`; without owners on a terminal: `"MAIN" was replaced. Join the new one in the SimpleX app. The link works once:`, a QR code and the link; elsewhere `Run npx -y @simplex-chat/mcp reset-owners in a terminal.` |

Environment: `SIMPLEX_MCP_HOME`, ROOT; `CLAUDE_CODE_SESSION_ID` and `CLAUDE_PROJECT_DIR`, the session; `CLAUDE_PLUGIN_OPTION_MAIN_GROUP`, `CLAUDE_PLUGIN_OPTION_APPROVE` and `CLAUDE_PLUGIN_OPTION_EXPIRE_HOURS`, the options; `SIMPLEX_MCP_READY_FD`, the daemon.

| Code | Message | Code location |
|---|---|---|
| 0 | the hooks, also after errors; `link` and `reset-owners`; the daemon after a stop or with `running` | [hook.ts:49-51](../src/hook.ts#L49-L51), [:58-61](../src/hook.ts#L58-L61), [daemon.ts:332](../src/daemon.ts#L332) |
| 2 | `simplex-mcp session runs from the Claude Code plugin.` | [session.ts:39](../src/session.ts#L39) |
| 2 | `Not a Claude Code session ID: ID` | [session.ts:40](../src/session.ts#L40) |
| 1 | `Restart Claude Code to update the SimpleX plugin.`, a spawn error (`link`, `reset-owners`) | [client.ts:42](../src/client.ts#L42), [:81](../src/client.ts#L81), [:108](../src/client.ts#L108) |
| 1 | daemon start failed: ready line `error: TEXT` | [daemon.ts:333-334](../src/daemon.ts#L333-L334) |
| 1 | `stop: ERROR` for a failed stop after a signal | [daemon.ts:338-343](../src/daemon.ts#L338-L343) |

`daemon.log` receives the daemon's stdout and stderr ([client.ts:74](../src/client.ts#L74)). stdout lines:

| Moment | Line | Code |
|---|---|---|
| start | `SimpleX MCP daemon VERSION, data folder ROOT` | [daemon.ts:336](../src/daemon.ts#L336) |
| Main group created | `Main group "NAME" created.` | [main-group.ts:215](../src/main-group.ts#L215) |
| link opened | `Main group link created. Whoever joins by it first becomes an owner of "NAME".` | [main-group.ts:166](../src/main-group.ts#L166) |
| no owners, members present | `No owners in the Main group.` | [main-group.ts:15](../src/main-group.ts#L15), [:71](../src/main-group.ts#L71), [:142](../src/main-group.ts#L142) |
| owner joined | `NAME joined (member ID) and is now an owner. The link is deleted.` | [main-group.ts:155](../src/main-group.ts#L155) |
| Main group lost | `The Main group is lost. Run npx -y @simplex-chat/mcp reset-owners in a terminal.` | [main-group.ts:160](../src/main-group.ts#L160) |
| reset card, decision | `The reset of the owners waits for an owner's approval in the Main group.`, `Reset of the owners: Approved by NAME.` | [main-group.ts:99](../src/main-group.ts#L99), [:130](../src/main-group.ts#L130) |
| session group created | `Group "N · NAME" of session N created.` | [hub.ts:368](../src/hub.ts#L368) |
| session group lost | `The group "N · NAME" is lost.` | [control.ts:439](../src/control.ts#L439) |
| actions | as in [3.4](#34-terminal-lines), with ` of session N` after `Action N`; `allowed.` for a tool use | [actions.ts:206](../src/actions.ts#L206), [:404](../src/actions.ts#L404) |
| stop | `Cancelled K action(s).`, `Stopping: sending K cancelled card(s), up to 10 s. Ctrl-C to stop now.`, `Stopped.` | [daemon.ts:164](../src/daemon.ts#L164), [serve.ts:164](../src/serve.ts#L164), [daemon.ts:167](../src/daemon.ts#L167) |

stderr: `CONTEXT: ERROR` from `logError`, with CONTEXT among `Main group command`, `EVENT in the Main group`, `EVENT in the group of session N`, `prompt of session N`, `invitation to session N`, `members of session N`, `User in session N`, `group of session N`, `mode MODE of session N`, `METHOD PATH` and those of [3.4](#34-terminal-lines). A session process writes `SimpleX daemon: ERROR`, `SimpleX channel: ERROR` and `permission relay: ERROR` to its own stderr, and the turn hook `SimpleX MCP turn: ERROR` ([session.ts:52](../src/session.ts#L52), [:57](../src/session.ts#L57), [:82](../src/session.ts#L82), [hook.ts:59](../src/hook.ts#L59)).

### 4.8 Plugin options, files and hook output

`userConfig` of [`.claude-plugin/plugin.json`](../.claude-plugin/plugin.json), read by [`readConfig`](../src/config.ts#L25-L31):

| Key | Variable | Type | Default | Valid | Error |
|---|---|---|---|---|---|
| `main_group` | `CLAUDE_PLUGIN_OPTION_MAIN_GROUP` | string | `SimpleX MCP` | equal to `validName(value)` | `a valid SimpleX name, such as VALID expected` or `a valid SimpleX name expected` |
| `approve` | `CLAUDE_PLUGIN_OPTION_APPROVE` | string | `requests` | `requests`, `host` | `requests or host expected` |
| `expire_hours` | `CLAUDE_PLUGIN_OPTION_EXPIRE_HOURS` | number | 23 | digits only, 1 to 23 | `hours from 1 to 23 expected` |

A missing or blank variable takes the default ([config.ts:26-30](../src/config.ts#L26-L30)). The hooks receive the variables from Claude Code; the MCP server gets them from `.mcp.json`, which maps `${user_config.KEY}` to the same names.

| File | Content |
|---|---|
| [`.claude-plugin/plugin.json`](../.claude-plugin/plugin.json) | name `simplex`, version `0.3.0`, equal to `package.json`, description, author, homepage, repository, license, keywords, `userConfig` |
| [`.mcp.json`](../.mcp.json) | server `simplex`: `node ${CLAUDE_PLUGIN_ROOT}/plugin/launch.js session` with the three option variables; `timeout` 169800000 ms |
| [`hooks/hooks.json`](../hooks/hooks.json) | SessionStart: `node "${CLAUDE_PLUGIN_ROOT}/plugin/launch.js" hook session-start`, timeout 600 s; PreToolUse, matcher `mcp__plugin_simplex_simplex__.*`: `node "${CLAUDE_PLUGIN_ROOT}/plugin/allow.js"`; UserPromptSubmit, Stop and PostModelSwitch: `node "${CLAUDE_PLUGIN_ROOT}/plugin/launch.js" hook turn`, `async: true` |
| [`plugin/launch.js`](../plugin/launch.js) | `npx -y @simplex-chat/mcp@VERSION ARGS` in the plugin's folder, with `CLAUDE_PROJECT_DIR` kept, or set to the folder it was started in (`npx.cmd` through the shell on Windows), stdio inherited, SIGINT and SIGTERM forwarded, the child's exit code passed on, 1 after a signal; a spawn error prints `simplex-mcp: ERROR` and exits 1 |
| [`plugin/allow.js`](../plugin/allow.js) | prints `{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"allow"}}` |
| [`skills/link/SKILL.md`](../skills/link/SKILL.md), [`skills/reset-owners/SKILL.md`](../skills/reset-owners/SKILL.md) | `/simplex:link`, `/simplex:reset-owners`: run `launch.js link` or `launch.js reset-owners`, show the output; `disable-model-invocation: true`; `allowed-tools` pre-approves the command |
| [`/.claude-plugin/marketplace.json`](../../../.claude-plugin/marketplace.json) | marketplace `simplex-chat`, plugin `simplex`, source `{"source": "npm", "package": "@simplex-chat/mcp"}` |

Hook input fields used: SessionStart `session_id`, `session_title`, `cwd`, `model`; the others `session_id`, `prompt`, `last_assistant_message`, `to_model`, `effort.level` ([hook.ts:13-26](../src/hook.ts#L13-L26)). SessionStart output ([hook.ts:37-52](../src/hook.ts#L37-L52)):

| Case | Output |
|---|---|
| Node.js below 22 | `{"systemMessage": "SimpleX MCP needs Node.js 22 or later; found V."}` |
| owners present, or no link | `{}` |
| no owner, a link | `systemMessage`: `SimpleX MCP: scan this QR code with the SimpleX app and tap Join to supervise your agents in "MAIN":`, the `plain` QR code and the link |
| daemon not running after the start | `{"systemMessage": "SimpleX MCP: The SimpleX daemon stopped; it starts again with the next tool call."}` |
| other errors, invalid options included | `{"systemMessage": "SimpleX MCP: TEXT"}` |
