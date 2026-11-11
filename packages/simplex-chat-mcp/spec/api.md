# SimpleX MCP server: API

> The six MCP tools, the status message, the agent control chat commands, replies and cards, and the CLI of `@simplex-chat/mcp` 0.3.0.
>
> Related specs: [README](README.md) | [Architecture](architecture.md) | [Flows](flows.md)

**Source:** [`tools.ts`](../src/tools.ts) | [`control.ts`](../src/control.ts) | [`cli.ts`](../src/cli.ts) | [`messages.ts`](../src/messages.ts) | [`actions.ts`](../src/actions.ts) | [`status.ts`](../src/status.ts) | [`inference.ts`](../src/inference.ts)

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
   - [1.10 Status message](#110-status-message)
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
| Tools are registered on each per-request `McpServer` with description, input schema, output schema and, in mode `host`, `_meta: {"anthropic/requiresUserInteraction": true}` for `connect`, `send`, `download` and `leave` | [tools.ts:461-468](../src/tools.ts#L461-L468), [:502-512](../src/tools.ts#L502-L512) |
| Tool descriptions and server instructions; the instructions name the agent's display name | [tools.ts:411-451](../src/tools.ts#L411-L451), [serve.ts:117](../src/serve.ts#L117) |
| Arguments are validated by the SDK; a failure is a result with `isError: true` and the text `Input validation error: Invalid arguments for tool NAME: ERRORS` | [mcp-BumlNux0.cjs:1812-1817](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L1812-L1817) |
| `additionalProperties: false` is set in every input schema | [tools.ts:255](../src/tools.ts#L255), [:273](../src/tools.ts#L273), [:285](../src/tools.ts#L285), [:288](../src/tools.ts#L288), [:296](../src/tools.ts#L296), [:307](../src/tools.ts#L307) |
| A result is sent as `structuredContent` and as its JSON in one text item | [tools.ts:518](../src/tools.ts#L518) |
| A tool error is sent as `isError: true` with one text item: the `ToolError` text, or the `errorText` of other errors | [tools.ts:520](../src/tools.ts#L520), [log.ts:45-49](../src/log.ts#L45-L49) |
| An aborted call ends with the result `Cancelled.`, `isError: true`, which the SDK discards | [tools.ts:470](../src/tools.ts#L470), [:510](../src/tools.ts#L510) |
| With a `progressToken` in `_meta`, `notifications/progress` is sent at the start of each wait and every 60 s, with `progress` 0, 1, 2, ... and a `message` | [tools.ts:1626-1636](../src/tools.ts#L1626-L1636), [messages.ts:301-305](../src/messages.ts#L301-L305) |
| The call is aborted when the client closes the response stream or sends `notifications/cancelled`; the action is kept | [src-BhnGYybN.cjs:6338-6341](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs#L6338-L6341), [:6425](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs#L6425) |
| A call with the same tool and action fields as an action in memory is attached to it; a `send` with `live: true` is never attached | [tools.ts:1618-1620](../src/tools.ts#L1618-L1620), [:794](../src/tools.ts#L794), [actions.ts:151-153](../src/actions.ts#L151-L153) |
| Messages are marked read after the last wait of a call that is not aborted | [tools.ts:684](../src/tools.ts#L684), [:851](../src/tools.ts#L851), [:1196](../src/tools.ts#L1196) |

Action fields of the key:

| Tool | Fields |
|---|---|
| `connect` | `target`<br>`incognito` (default `false`)<br>`message` (default `null`) |
| `send` | `chat`<br>`text` (default `null`)<br>`file` (default `null`)<br>`as` (default `null`)<br>`reply_to` (default `null`)<br>`mention` (default `null`) |
| `download` | `chat`<br>`item` |
| `leave` | `chat` |

Result status of the four action tools ([`actionResult`](../src/tools.ts#L653-L665)):

| Status | Meaning | Extra fields |
|---|---|---|
| `done` | performed | per tool |
| `failed` | failure of the card post, the counter write or the perform, or loss of the control chat | `error` |
| `rejected` | an approver sent `/reject` | |
| `expired` | no decision before the expiry time | |
| `known` | `connect` only: the target is an existing chat past the prepared state; no action | chat fields |
| `performing` | `download` only: `wait` passed after the approval; the download continues | |
| `live` | `send` with `live: true` only: the text is shown in the status message; no action | `chat` |

### 1.2 `connect`

Handler: [`AgentTools.connect`](../src/tools.ts#L634-L647). Perform: [`performConnect`](../src/tools.ts#L703-L734). Schemas: [input](../src/tools.ts#L245-L256), [output](../src/tools.ts#L313-L323).

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
| `known` | `status`, chat fields of the known chat | [tools.ts:639](../src/tools.ts#L639) |
| `done`, chat unknown | `action`, `status`, `type`, `connected: false`, plan fields `name`, `peer_type`, `description`, `welcome`, `commands`, `known_as` when present | [tools.ts:650](../src/tools.ts#L650), [:1403-1409](../src/tools.ts#L1403-L1409) |
| `done`, chat known | `action`, `status`, chat fields, `messages` | [tools.ts:667-687](../src/tools.ts#L667-L687) |
| `failed` | `action`, `status`, `error` | [tools.ts:658](../src/tools.ts#L658) |
| `rejected`, `expired` | `action`, `status` | [tools.ts:658](../src/tools.ts#L658) |

Chat fields ([`chatFields`](../src/tools.ts#L1259-L1286)):

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

`messages`: message items among the last 50 items of the chat when the result is built, replaced by their collected copies, followed by the items collected since the connection; live items are left out; new items are marked read ([tools.ts:669](../src/tools.ts#L669), [:679-686](../src/tools.ts#L679-L686)).

Tool errors:

| Text | Condition | Code |
|---|---|---|
| `Not a SimpleX link or name: TARGET` | whitespace in `target`, or `invalidConnReq` from `apiConnectPlan` | [tools.ts:691](../src/tools.ts#L691), [:697](../src/tools.ts#L697) |
| an app error text, for example `Name not found: This SimpleX name is not registered. Please check the name.`, or `SimpleX error: CHAIN: DETAILS` | other errors of `apiConnectPlan`, or a plan of type `error` | [tools.ts:698](../src/tools.ts#L698), [:1330](../src/tools.ts#L1330), [log.ts:51-53](../src/log.ts#L51-L53) |
| `TARGET can't be joined now: PLAN.` | `invitationLink` other than `ok` and `known`<br>`contactAddress` other than `ok`, `known`, `contactViaAddress` and `connectingConfirmReconnect`<br>`groupLink` other than `ok`, `known` and `connectingConfirmReconnect` | [tools.ts:1321](../src/tools.ts#L1321), [:1334](../src/tools.ts#L1334), [:1352](../src/tools.ts#L1352), [:1374](../src/tools.ts#L1374) |
| `message: TARGET is not a contact or business address with short link data, or its contact exists without a connection.` | `message` given and the plan has `takesMessage: false` | [tools.ts:641](../src/tools.ts#L641) |
| refusals | see [1.9](#19-action-refusals) | |

`takesMessage` is `true` for a contact address with short link data, for a known prepared contact, and for a known prepared business group ([tools.ts:1325](../src/tools.ts#L1325), [:1345](../src/tools.ts#L1345), [:1363](../src/tools.ts#L1363)).

### 1.3 `send`

Handler: [`AgentTools.send`](../src/tools.ts#L791-L817). Result: [`sendResult`](../src/tools.ts#L841-L859). Perform: [`performSend`](../src/tools.ts#L861-L886). Note: [`showNote`](../src/tools.ts#L819-L826). Schemas: [input](../src/tools.ts#L258-L274), [output](../src/tools.ts#L325-L337).

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
| `live` | boolean | | `false` | `true`: `text` is shown at once in the status message of `chat` ([1.10](#110-status-message)), and no action is created; `text` only, at most 2000 characters (code points); `wait`, `idle` and `limit` are ignored |

File types by extension, lower-cased ([tools.ts:173-182](../src/tools.ts#L173-L182), [`attachmentMedia`](../src/tools.ts#L1576-L1587)):

| Extension | Message type | Sent file | Preview and duration |
|---|---|---|---|
| `.jpg`, `.jpeg`, `.png` | image | `core.resizeImage(..., Image)`, written to `DATA/files/sent/`; the original when resizing fails | `core.resizeImage(..., MessagePreview)`; the fixed preview when it fails |
| `.gif`, `.webp` | image | unchanged | the fixed preview |
| `.mp4`, `.mov` | video | unchanged | the first frame from `ffmpeg`, resized as a preview; duration from `ffprobe`, rounded; the fixed preview and 0 when they fail |
| other, or `as: "file"` | file | unchanged | |

The fixed preview is a 16 × 9 mid-gray PNG ([tools.ts:163](../src/tools.ts#L163)). `ffmpeg` and `ffprobe` run with `-protocol_whitelist file -f mov`, 30 s and 64 MB of output each ([tools.ts:1598-1616](../src/tools.ts#L1598-L1616)).

Output:

| Field | Presence | Value |
|---|---|---|
| `action` | without `live` | action number |
| `status` | always | `done`, `failed`, `rejected`, `expired`; `live` with `live: true` |
| `chat` | `done`, `live` | the chat reference |
| `item` | `done` | item ID of the sent message |
| `responses` | `done` | received message items with IDs above `item`, in arrival order, live items left out, at most `limit` |
| `more` | `done` | collected responses not returned, live ones and overflow included |
| `error` | `failed` | error text |

Only `status` is required by the output schema ([tools.ts:336](../src/tools.ts#L336)).

Tool errors, in the order of the checks:

| Text | Condition | Code |
|---|---|---|
| `text: required without file.` | neither `text` nor `file` | [tools.ts:796](../src/tools.ts#L796) |
| `as: applies only with file.` | `as` without `file` | [tools.ts:797](../src/tools.ts#L797) |
| `live: applies to text only; send file, reply_to and mention without live.` | `live` with `file`, `reply_to` or `mention` | [tools.ts:798](../src/tools.ts#L798) |
| `text: at most 2000 characters with live.` | `live` with `text` over 2000 code points | [tools.ts:799](../src/tools.ts#L799), [:160](../src/tools.ts#L160) |
| `No chat CHAT. list shows the agent's chats.` | invalid reference, the control chat, or a store error when the chat is read | [tools.ts:1225-1232](../src/tools.ts#L1225-L1232), [:1520-1522](../src/tools.ts#L1520-L1522) |
| `The agent is not connected to CHAT and can't post; list shows connected: false.` | the chat is not connected | [tools.ts:801-802](../src/tools.ts#L801-L802), [:1450](../src/tools.ts#L1450) |
| `The agent has role ROLE in CHAT and can't post.` | membership role `observer` or `relay` | [tools.ts:801-802](../src/tools.ts#L801-L802), [:1451-1454](../src/tools.ts#L1451-L1454) |
| `No chat CHAT. list shows the agent's chats.` | `live` while `leave` or `/leave` of the chat runs | [tools.ts:820-821](../src/tools.ts#L820-L821), [:829](../src/tools.ts#L829) |
| `live: CHAT is a channel or a chat with a bot or an agent; status messages are shown in chats with people, groups and business chats.` | `live` in a channel, or in a direct chat whose contact has peer type `bot` or `agent` | [tools.ts:830](../src/tools.ts#L830), [:1428-1435](../src/tools.ts#L1428-L1435) |
| `live: status messages are shown in mode requests only; this server runs in mode host.` | `live` in mode `host` | [tools.ts:831](../src/tools.ts#L831) |
| `live: The agent lost its control chat. The person who runs it has to restart it with --reset-owners.` | `live` after the control chat is lost | [tools.ts:832](../src/tools.ts#L832), [actions.ts:14](../src/actions.ts#L14) |
| `live: your owner's approval of all messages to CHAT is required; list shows approved: true for such chats.` | `live` in a chat without a standing approval of messages by a current approver | [tools.ts:833](../src/tools.ts#L833), [control.ts:130-133](../src/control.ts#L130-L133) |
| `The server is stopping.` | `live` after the stop started | [status.ts:55](../src/status.ts#L55), [actions.ts:15](../src/actions.ts#L15) |
| `live: the update limit of the status message of CHAT is reached; send your answer without live.` | `live` when the status message has 59 renders | [status.ts:58-59](../src/status.ts#L58-L59) |
| `reply_to: CHAT has no item N.` | the read of `around=N` fails, or the item is absent from it | [tools.ts:1237](../src/tools.ts#L1237) |
| `reply_to: item N in CHAT is not a message.` | the item's content is not `rcvMsgContent` or `sndMsgContent` | [tools.ts:1239](../src/tools.ts#L1239) |
| `mention: CHAT is a contact chat; mentions apply to groups and channels.` | `mention` with `@N` | [tools.ts:1245](../src/tools.ts#L1245) |
| `mention: CHAT has no member N.` | ID absent from `apiListMembers` | [tools.ts:1250](../src/tools.ts#L1250) |
| `mention: members N and M have the same name.` | two mentioned members with the same display name | [tools.ts:1253](../src/tools.ts#L1253) |
| `file: an absolute path is expected.`<br>`file: no file at FILE.`<br>`file: PATH is not a file.`<br>`file: PATH can't be read.` | the path, after symbolic links | [tools.ts:1529-1540](../src/tools.ts#L1529-L1540) |
| `file: PATH is refused: private keys and the server's own files are not sent.` | a file named `id_*`, `*.pem`, `*.key`, `*.p12` or `*.pfx`, or under `~/.ssh`, `~/.gnupg`, or the data folder or `~/.simplex-mcp` outside `DATA/files/`; each folder by its real path | [tools.ts:902-905](../src/tools.ts#L902-L905), [:911-916](../src/tools.ts#L911-L916) |
| `file: PATH is empty.` | size 0 | [tools.ts:906](../src/tools.ts#L906) |
| `text: too long for the message with its quote; shorten it.`<br>`text: too long for a message with this TYPE; shorten it.` | the message, as the core sends it, above 15602 bytes | [tools.ts:807-809](../src/tools.ts#L807-L809), [:1497-1513](../src/tools.ts#L1497-L1513) |
| refusals | see [1.9](#19-action-refusals) | |

The message is measured as 600 bytes plus the JSON of its content, the file's name, the quote and the mentioned names. The quote is the quoted text, or its file's name when the text is empty; a sent image or video quotes it as text, otherwise an image's preview is kept ([tools.ts:1497-1513](../src/tools.ts#L1497-L1513)).

The perform fails with `file: PATH changed after the request.` when the file's real path, size, inode or change time differs from the request ([tools.ts:918-923](../src/tools.ts#L918-L923)). A resized copy is deleted at `sndFileCompleteXFTP`, at `sndFileError` and after a failed send ([tools.ts:883](../src/tools.ts#L883), [:888-898](../src/tools.ts#L888-L898), [serve.ts:161-165](../src/serve.ts#L161-L165)). After `apiSendMessages` returns, the status message of `chat` is deleted ([tools.ts:877](../src/tools.ts#L877), [1.10](#110-status-message)).

### 1.4 `receive`

Handler: [`AgentTools.receive`](../src/tools.ts#L1167-L1203). Schemas: [input](../src/tools.ts#L276-L286), [output](../src/tools.ts#L339-L347).

Input:

| Field | Type | Range | Default | Rule |
|---|---|---|---|---|
| `chat` | string | | required | `@N` or `#N` of the agent |
| `wait` | number | 0 to 86400 | 0 | seconds to wait when no message is new |
| `limit` | integer | 1 to 100 | 50 | most messages returned |
| `typing` | boolean | | `false` | `true`: when the call returns messages, a status message is started in `chat` ([1.10](#110-status-message)) |

Output (all fields always present):

| Field | Value |
|---|---|
| `chat` | the chat reference |
| `messages` | unread received messages in chat order, then received messages that arrived during the call; live items left out; at most `limit` |
| `more` | new messages not returned, overflow, and unread items left unread by the page reads |

Tool errors: `No chat CHAT. list shows the agent's chats.` for an invalid reference, the control chat, or a store error of the first read, `initial` 1 ([tools.ts:1169](../src/tools.ts#L1169), [:1173-1175](../src/tools.ts#L1173-L1175)); other chat errors, a store error of a later page read included, as their error text ([log.ts:45-53](../src/log.ts#L45-L53)).

Messages are read by `receive` without an action or an approval.

When no message is new and `wait` is above 0, the status message of `chat` is deleted before the wait ([tools.ts:1182-1183](../src/tools.ts#L1182-L1183)). With `typing: true`, the status message is started after the returned messages are marked read, unless the call was aborted ([tools.ts:1195-1198](../src/tools.ts#L1195-L1198)).

### 1.5 `download`

Handler: [`AgentTools.download`](../src/tools.ts#L1006-L1027). Result: [`downloadResult`](../src/tools.ts#L1054-L1062). Perform: [`performDownload`](../src/tools.ts#L1064-L1068), [`downloadFile`](../src/tools.ts#L539-L566). Schemas: [input](../src/tools.ts#L299-L308), [output](../src/tools.ts#L394-L409).

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

FOLDER is `group-N` for `#N` and `contact-N` for `@N`; the extension is the file name's extension when it has 1 to 16 ASCII letters and digits ([messages.ts:215-223](../src/messages.ts#L215-L223)). `/forward` downloads a file to `DATA/forwarded/FOLDER/` under the same name; a later perform of the download moves it to the download path, after the copy for the post is made ([tools.ts:539-556](../src/tools.ts#L539-L556)).

Steps before the action:

| Step | Code |
|---|---|
| a file in status `rcvInvitation`, or an XFTP file: the file's servers from `apiGetChatItemInfo`; while none, the call waits for `rcvFileDescrReady` until `wait` seconds after the call, with the progress `The sender is uploading the file of item N in CHAT; waiting until TIME.` | [tools.ts:1029-1046](../src/tools.ts#L1029-L1046) |
| unknown servers: the file's servers that are not among the XFTP servers of `apiGetUserServers`, of no operator or of an enabled one, compared without their passwords | [tools.ts:1048-1052](../src/tools.ts#L1048-L1052) |
| without unknown servers, and for a file ID not in rejected files, the chat's files approval, read again, makes the action performed at once | [tools.ts:1021](../src/tools.ts#L1021) |

Perform: `apiReceiveFile(fileId, path, approvedRelays)`, for a file in status `rcvInvitation`. `approvedRelays` is true in mode `requests` for a download whose card named unknown servers, and false otherwise; the core then checks the complete description ([tools.ts:562](../src/tools.ts#L562), [:1070-1079](../src/tools.ts#L1070-L1079)). The download ends at `rcvFileComplete`, `rcvFileError`, `rcvFileSndCancelled` or `chatItemUpdated` of its file, or at a check every 60 s ([tools.ts:578-600](../src/tools.ts#L578-L600), [:1081-1149](../src/tools.ts#L1081-L1149)).

Failure texts of the perform:

| Text | Condition | Code |
|---|---|---|
| the file's error, as in the file field | the file failed | [tools.ts:1110-1120](../src/tools.ts#L1110-L1120) |
| `The sender cancelled the file.` | `rcvFileSndCancelled` | [tools.ts:595-600](../src/tools.ts#L595-L600) |
| `The message or its chat was deleted.` | a check finds no message, contact or group | [tools.ts:167](../src/tools.ts#L167), [:1100-1108](../src/tools.ts#L1100-L1108) |
| `The download made no progress for 30 minutes and was cancelled.` | no rise of `rcvProgress` for 30 minutes; the file is cancelled | [tools.ts:171](../src/tools.ts#L171), [:1093-1097](../src/tools.ts#L1093-L1097) |
| `The received file is not in the agent's files folder.` | the file is received and neither the download path nor the forwarded path has a file | [messages.ts:14](../src/messages.ts#L14), [tools.ts:1122-1126](../src/tools.ts#L1122-L1126) |
| `Your owner deleted the files of this chat.` | `/delete CHAT files` | [tools.ts:168](../src/tools.ts#L168), [:949-967](../src/tools.ts#L949-L967) |
| `The file is on unknown servers that were not approved.` | a check reads an accepted file in status `rcvInvitation`: the core returned it to the offer when its description completed | [tools.ts:169](../src/tools.ts#L169), [:1089](../src/tools.ts#L1089) |
| `Unknown servers! Without Tor or VPN, your IP address will be visible to these XFTP relays: HOST, HOST.` | the core refuses the servers of the complete description at the acceptance | [log.ts:78-79](../src/log.ts#L78-L79) |

Tool errors:

| Text | Condition | Code |
|---|---|---|
| `No chat CHAT. list shows the agent's chats.` | invalid reference, the control chat, or a store error when the chat is read | [tools.ts:1011](../src/tools.ts#L1011) |
| `item: CHAT has no item N.` | the item is not found | [tools.ts:1013](../src/tools.ts#L1013) |
| `item: item N in CHAT has no received file.` | the item is not received or has no file | [tools.ts:1015](../src/tools.ts#L1015) |
| `item: the file of item N in CHAT failed: ERROR` | the file field has status `failed` | [tools.ts:1016-1017](../src/tools.ts#L1016-L1017) |
| `item: the sender has not uploaded the file of item N in CHAT yet; call download again later.` | no servers by the end of `wait`, also for a file invited without a description, whose servers the core does not list; no action is created | [tools.ts:1019](../src/tools.ts#L1019), [:1044](../src/tools.ts#L1044) |
| refusals | see [1.9](#19-action-refusals) | |

A file ID whose download returned `rejected` or `expired` shows status `rejected` until a download of it is done or the server restarts ([tools.ts:1059-1060](../src/tools.ts#L1059-L1060)).

### 1.6 `list`

Handler: [`AgentTools.listChats`](../src/tools.ts#L1205-L1213). Schemas: [input](../src/tools.ts#L288), [output](../src/tools.ts#L349-L381).

Input: an empty object.

Output:

| Field | Item fields |
|---|---|
| `chats` | `chat`<br>`type`<br>`name`<br>`simplex_name`, when verified<br>`peer_type`, when present<br>`incognito`<br>`known_as`, when incognito<br>`connected`<br>`unread`: `chatStats.unreadCount`<br>`approved: true`, only under a standing approval of messages in mode `requests` |
| `actions` | `action`<br>`tool`<br>`input`: the input of the creating call<br>`state`<br>`open`: `true` while a call waits on it |

`chats`: the contacts and groups of the main conversations from one read, `apiGetChats(USER, last 1000000)`, except the control chat ([tools.ts:1215-1223](../src/tools.ts#L1215-L1223)). `actions`: every action in memory ([tools.ts:1211](../src/tools.ts#L1211)).

### 1.7 `leave`

Handler: [`AgentTools.leave`](../src/tools.ts#L975-L984). Perform: [`leaveChat`](../src/tools.ts#L990-L1004). Schemas: [input](../src/tools.ts#L290-L297), [output](../src/tools.ts#L383-L392).

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

The chat is marked as being left from before its status message is deleted until these methods end; the status message is deleted first ([tools.ts:991-993](../src/tools.ts#L991-L993), [:1000-1002](../src/tools.ts#L1000-L1002)).

Tool errors: `No chat CHAT. list shows the agent's chats.` when no action with the same key is in memory and the chat is invalid, the control chat or unknown ([tools.ts:978-980](../src/tools.ts#L978-L980)); refusals.

### 1.8 Message, file and command objects

Message ([`Message`](../src/messages.ts#L16-L28), [`toMessage`](../src/messages.ts#L174-L192), [schema](../src/tools.ts#L184-L217)):

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

Only received message items (`rcvMsgContent`) are returned by `send` and `receive`; `connect` also returns sent ones (`sndMsgContent`) ([messages.ts:101](../src/messages.ts#L101), [tools.ts:679](../src/tools.ts#L679)).

Command ([`commandsOf`](../src/tools.ts#L1479-L1486), [schema](../src/tools.ts#L219-L229)):

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

Largest card form ([control.ts:622-630](../src/control.ts#L622-L630)): the maximum of the creation form, the `done` form and a `failed` form with an error of 600 × `€`, with the action's own decider or the longest current approver name; each form is measured as JSON-escaped UTF-8: `"`, `\`, `\n`, `\r`, `\t` 2 bytes, other characters below U+0020 6 bytes.

Link texts of a `connect` card ([control.ts:122](../src/control.ts#L122), [:644-658](../src/control.ts#L644-L658)): the description and welcome lines take the bytes that the largest form leaves without them. Lines are kept while they fit; the next line is cut to the bytes left and ends in `…`; later lines are left out. The tool result keeps both texts whole.

### 1.10 Status message

A status message is one live message of the agent in a chat, edited while the agent works and deleted afterwards ([`Statuses`](../src/status.ts#L28-L163)).

Eligibility, all of ([`showsStatus`](../src/tools.ts#L837-L839)):

| Condition | Code |
|---|---|
| the agent can post: the chat is connected and, in a group, the agent's role is other than `observer` and `relay` | [`postingRefusal`](../src/tools.ts#L1449-L1456) |
| the chat is not being left by `leave` or `/leave` | [tools.ts:829](../src/tools.ts#L829) |
| the chat is a direct chat with a contact of peer type other than `bot` and `agent`, a group or a business chat | [tools.ts:830](../src/tools.ts#L830), [:1428-1435](../src/tools.ts#L1428-L1435) |
| mode `requests` | [tools.ts:831](../src/tools.ts#L831) |
| the control chat is not lost | [tools.ts:832](../src/tools.ts#L832) |
| a standing approval of messages to the chat, by a current approver | [tools.ts:833](../src/tools.ts#L833), [control.ts:130-133](../src/control.ts#L130-L133) |

The chat and its eligibility are read again at each tick, the first tick included ([tools.ts:498-500](../src/tools.ts#L498-L500), [status.ts:117-123](../src/status.ts#L117-L123)). For a `send` with `live: true`, they are checked with the chat read of the call ([1.3](#13-send)).

Start:

| Event | Effect | Code |
|---|---|---|
| `receive` with `typing: true` returns messages, from any sender; the chat has no status, and the stop has not started | a status is created, and its first tick runs at once; when the chat is not eligible at that tick, the status ends without a post | [tools.ts:1198](../src/tools.ts#L1198), [status.ts:48-52](../src/status.ts#L48-L52) |
| `send` with `live: true` passes its checks; the chat has no status, or one created at least 23 hours earlier, which is deleted first | a status is created with the note and rendered at once; its next tick is in 3 s | [status.ts:54-64](../src/status.ts#L54-L64) |

Text ([`statusText`](../src/status.ts#L165-L169)):

| Part | Value |
|---|---|
| line of renders 1 to 49 | `FRAME LABEL · M:SS`; FRAME is `◐`, `◓`, `◑`, `◒` in turn, from `◐`; M:SS is the time since the status was created |
| line of render 50 and later | `◌ working since YYYY-MM-DD HH:MM UTC`, the time the status was created ([messages.ts:333-335](../src/messages.ts#L333-L335)) |
| text | the line; after a `send` with `live: true`, the line, a newline and the text of the last such call |

Labels, first match ([`statusLabel`](../src/status.ts#L171-L192)):

| Label | Condition |
|---|---|
| ``loading `MODEL` ``, or `loading the model` when the ID is unknown | `--inference`: state `starting` of a model of `/running`; state `stopping` of a model while 0 models are ready; or 503 from `/props` or `/slots` of a polled server |
| ``reading the prompt with `MODEL`, N tokens read`` | `--inference`: processing slots, each with `n_decoded` 0; N is the sum of `n_prompt_tokens_processed` of these slots |
| ``generating with `MODEL`, N tokens/s`` | `--inference`: a processing slot with `n_decoded` above 0; N is the rate between two polls, rounded |
| `waiting for approval` | a pending action has an open call |
| `using TOOL` | a running call other than `receive`; TOOL is the tool of the latest such call |
| `working` | `--inference`: slots were read, 0 of them processing; or a polled server sleeps: `is_sleeping` of `/props` is true |
| `thinking` | otherwise, and at the first render |

- `` with `MODEL` `` is shown when all processing slots belong to one model with a known ID; `, N tokens read` when N is above 0; `, N tokens/s` when the same slot and task were read in the previous poll, processing with tokens already decoded, and more tokens were decoded since then ([inference.ts:92-107](../src/inference.ts#L92-L107)).
- A model ID is kept only when it matches `^[\w.:/+@-]{1,64}$`, and it is shown as a code span ([inference.ts:27](../src/inference.ts#L27), [:150-152](../src/inference.ts#L150-L152), [status.ts:186-188](../src/status.ts#L186-L188)).
- The label is read at renders 2 to 49, after the inference poll, and it is the same for every chat ([status.ts:124-128](../src/status.ts#L124-L128), [:144-149](../src/status.ts#L144-L149)).
- Every tool call is registered as running from its start to its end ([tools.ts:515](../src/tools.ts#L515), [:521-523](../src/tools.ts#L521-L523), [status.ts:38-46](../src/status.ts#L38-L46)).
- After a failed poll, the label is chosen as without `--inference` ([status.ts:151-162](../src/status.ts#L151-L162)).

Schedule ([`tick`](../src/status.ts#L116-L131), [`schedule`](../src/status.ts#L138-L142)):

| Rule | Value |
|---|---|
| renders by ticks | one per tick while the renders are below 50 |
| gap to the next tick, renders below 50 | a tenth of the status's age, at least 3 s, at most 20 s |
| gap to the next tick, renders 50 or more | 5 min |
| renders by notes | one per `send` with `live: true`, at once, while the renders are below 59 |
| commands | the first render is a live send, later renders are live updates; a text equal to the last sent one is skipped ([api.ts:1160-1171](../../simplex-chat-nodejs/src/api.ts#L1160-L1171), [:1199-1213](../../simplex-chat-nodejs/src/api.ts#L1199-L1213)) |

The note of the last `send` with `live: true` is kept under the line until the next note or the deletion.

Deletion, at the first of:

| Event | Code |
|---|---|
| `send` without `live` in the chat, after `apiSendMessages` of its perform returns | [tools.ts:877](../src/tools.ts#L877) |
| `receive` of the chat, when nothing is new and `wait` is above 0 | [tools.ts:1182-1183](../src/tools.ts#L1182-L1183) |
| `leave` or `/leave` of the chat, before the chat is deleted | [tools.ts:993](../src/tools.ts#L993) |
| a tick: the chat is not eligible, or its read fails | [status.ts:117-123](../src/status.ts#L117-L123) |
| a tick: 23 hours after the status was created | [status.ts:123](../src/status.ts#L123) |
| `send` with `live: true`, 23 hours or more after the status was created; a new status is created with the note | [status.ts:57](../src/status.ts#L57) |
| a tick: 50 renders or more, no running call other than `receive`, and 30 minutes since a call last started or ended | [status.ts:122-123](../src/status.ts#L122-L123) |
| a failed send or update of the status | [status.ts:102](../src/status.ts#L102), [:111-114](../src/status.ts#L111-L114) |
| the stop | [status.ts:76-82](../src/status.ts#L76-L82), [serve.ts:182](../src/serve.ts#L182) |

The deletion is `apiDeleteChatItems` of the item with mode `broadcast`, sent only when the item was posted ([status.ts:66-74](../src/status.ts#L66-L74), [api.ts:1178-1213](../../simplex-chat-nodejs/src/api.ts#L1178-L1213)).

At start, before the HTTP listener, each eligible chat is read in pages of 100 items from the last one, until a page is short or starts with an item older than 30 h − 10 min; the agent's own live message items younger than that and not marked deleted are deleted with mode `broadcast` ([serve.ts:115](../src/serve.ts#L115), [tools.ts:1160-1165](../src/tools.ts#L1160-L1165), [status.ts:84-99](../src/status.ts#L84-L99)).

---

## 2. Agent control chat

### 2.1 Commands and replies

Accepted text ([`parseCommand`](../src/control.ts#L545-L550), [`ownerRequest`](../src/control.ts#L710-L723)):

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

The server reads commands from members' messages in the main conversation of the control chat ([serve.ts:139](../src/serve.ts#L139), [control.ts:171-181](../src/control.ts#L171-L181)). It answers a command with a reply that quotes it, one command after another, and it runs the expiry check before each command ([control.ts:179](../src/control.ts#L179), [:201](../src/control.ts#L201)).

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
| `delete CHAT files` | the chat's downloads in memory fail and are cancelled; its other files in status `rcvAccepted`, `rcvTransfer` or `rcvWarning`, such as downloads the core continues after a restart, are cancelled; the folder of CHAT and its forwarded folder are deleted, for a current or a left chat | `Deleted K files from CHAT, SIZE.` (`1 file`), or `No files from CHAT.` | [control.ts:304-307](../src/control.ts#L304-L307), [tools.ts:949-967](../src/tools.ts#L949-L967), [messages.ts:271-279](../src/messages.ts#L271-L279) |
| `pending` | no pending action with a card | `No pending actions.` | [control.ts:208](../src/control.ts#L208) |
| `pending` | otherwise | one line per action: `N  WHAT  expires YYYY-MM-DD HH:MM UTC` | [control.ts:207](../src/control.ts#L207), [:618-620](../src/control.ts#L618-L620) |
| `list` | no chats | `No chats.` | [control.ts:325](../src/control.ts#L325) |
| `list` | otherwise | one line per chat of the 100 most recent: `CHAT  KIND NAME`, then `, approved for this chat by APPROVER`, `, files approved by APPROVER` and `, not connected` when applicable, cut to 100 bytes with `…`; then `K older chats are not shown.` (`1 older chat is not shown.`) | [control.ts:313-326](../src/control.ts#L313-L326) |

KIND is `contact`, `bot`, `agent`, `business`, `group` or `channel` ([tools.ts:1428-1435](../src/tools.ts#L1428-L1435)). WHAT is `connect to SUBJECT`, `send to SUBJECT`, `download NAME from SUBJECT`, `delete SUBJECT` or `leave SUBJECT` ([actions.ts:315-327](../src/actions.ts#L315-L327)). SIZE is in bytes, KB, MB or GB, by 1000, with one decimal ([control.ts:670-673](../src/control.ts#L670-L673)). Names are shown by `safeName` ([control.ts:687-690](../src/control.ts#L687-L690)).

A standing approval is stored in the chat's custom data, `simplexMcp` for messages and `simplexMcpFiles` for files, as `{approvedBy, approvedById, approvedAt}`; it applies in mode `requests` while member `approvedById` is an approver ([control.ts:22-25](../src/control.ts#L22-L25), [:130-133](../src/control.ts#L130-L133)).

### 2.2 Cards

Card text: request lines, then one last line ([`cardText`](../src/control.ts#L552-L571), [`requestLines`](../src/control.ts#L573-L601)).

Request lines:

| Tool | First line | Further lines |
|---|---|---|
| `connect` | `I want to connect to SUBJECT, as myself.` or `, incognito.`<br>done and connected: `I connected to ...`<br>done and not connected: `I asked to connect to ...` | link description, one code span per line<br>welcome, one code span per line<br>both cut to fit the card ([1.9](#19-action-refusals))<br>with `message`: `With the message:` and its lines |
| `send` | `I want to send a message to SUBJECT:`, with `, where I am incognito` before the colon in incognito chats<br>done: `I sent a message to ...` | text, one code span per line<br>with `file`: ``attaching TYPE `PATH`, SIZE``<br>`replying to NAME: QUOTE` with `reply_to`, the quote cut to 100 characters<br>`mentioning @A, @'B C'` with `mention` |
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

Command lines ([`commandsLine`](../src/control.ts#L603-L616)):

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
| `sndFileError` of a sent file; logged too | ``The file `NAME` was not sent to KIND NAME (CHAT): `REASON`.`` | [control.ts:160-169](../src/control.ts#L160-L169), [serve.ts:163-165](../src/serve.ts#L163-L165) |
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

ENTRIES: `ID:NAME` of each human, or of each human owner, joined by commas; an entry with characters other than letters, digits, `_` and `-` is quoted for the shell ([control.ts:774-784](../src/control.ts#L774-L784)).

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

Binary: `simplex-mcp`, compiled `dist/cli.js` ([package.json:5-7](../package.json#L5-L7)). Help text: [cli.ts:10-42](../src/cli.ts#L10-L42).

```
simplex-mcp --help
simplex-mcp serve NAME [options]
simplex-mcp register NAME [options]
```

NAME: 1 to 32 characters of `[a-z0-9]` and single hyphens, starting with a letter or digit ([cli.ts:159-161](../src/cli.ts#L159-L161)).

### 3.1 `serve`

| Option | Value | Default | Rule | Code |
|---|---|---|---|---|
| `--data` | folder | `~/.simplex-mcp/NAME` | resolved to an absolute path; `dataGiven` is set | [cli.ts:73-76](../src/cli.ts#L73-L76) |
| `--display-name` | text | NAME when the user is created | must equal `validName(text)` | [cli.ts:77-79](../src/cli.ts#L77-L79), [:163-167](../src/cli.ts#L163-L167) |
| `--approve` | `requests` or `host` | `requests` | | [cli.ts:80-82](../src/cli.ts#L80-L82) |
| `--owners` | absent value, or `ID:NAME,...` | absent | the value is absent when the next argument is missing or starts with `--`; it is split at each comma followed by `ID:`, so a name may hold commas; entries match `^([1-9]\d*):(.+)$` | [cli.ts:83-85](../src/cli.ts#L83-L85), [:190-194](../src/cli.ts#L190-L194) |
| `--expire` | hours | 23 | integer 1 to 23 | [cli.ts:86-88](../src/cli.ts#L86-L88) |
| `--stop-wait` | seconds | 10 | integer 0 to 60 | [cli.ts:89-91](../src/cli.ts#L89-L91) |
| `--reset-owners` | flag | off | refused with an `--owners` list | [cli.ts:92-94](../src/cli.ts#L92-L94), [:111-113](../src/cli.ts#L111-L113) |
| `--new-token` | flag | off | | [cli.ts:95-97](../src/cli.ts#L95-L97) |
| `--port` | number | 5226 | integer 1 to 65535 | [cli.ts:98-100](../src/cli.ts#L98-L100) |
| `--qr-code` | `small` or `large` | `small` | | [cli.ts:101-103](../src/cli.ts#L101-L103) |
| `--inference` | URL | absent | an `http` or `https` URL without user name and password; kept as its origin and path, trailing slashes removed; refused with `--approve host` | [cli.ts:104-106](../src/cli.ts#L104-L106), [:114-116](../src/cli.ts#L114-L116), [:184-188](../src/cli.ts#L184-L188) |

With `--inference`, `SIMPLEX_MCP_INFERENCE_KEY` is read before the data folder is prepared. A set key is accepted with 1 or more visible ASCII characters (0x21 to 0x7e) only, and it is sent as `Authorization: Bearer KEY` to URL only; the proxies of `/running` are polled without it ([serve.ts:69](../src/serve.ts#L69), [:213-217](../src/serve.ts#L213-L217), [inference.ts:66](../src/inference.ts#L66), [:111](../src/inference.ts#L111)). The polls are described in [Architecture 2.12](architecture.md#212-inferencets).

### 3.2 `register`

| Option | Value | Default | Code |
|---|---|---|---|
| `--scope` | `user` or `local` | `user` | [cli.ts:124-126](../src/cli.ts#L124-L126) |
| `--data` | folder | `~/.simplex-mcp/NAME` | [cli.ts:127-129](../src/cli.ts#L127-L129) |

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
| 0 | `--help` text on stdout; a stop after a signal | [cli.ts:217-219](../src/cli.ts#L217-L219), [serve.ts:56](../src/serve.ts#L56) |
| 2 | `Usage: simplex-mcp serve NAME [options] or simplex-mcp register NAME [options]. Run simplex-mcp --help.` | [cli.ts:52](../src/cli.ts#L52) |
| 2 | `NAME: 1 to 32 lowercase letters, digits and single hyphens, starting with a letter or digit.` | [cli.ts:53](../src/cli.ts#L53) |
| 2 | `OPTION: a value expected.` | [cli.ts:148](../src/cli.ts#L148) |
| 2 | `Unknown option OPTION. Run simplex-mcp --help.` | [cli.ts:207](../src/cli.ts#L207) |
| 2 | `--display-name: not a valid SimpleX name; try VALID` | [cli.ts:165](../src/cli.ts#L165) |
| 2 | `--approve: requests or host expected, got VALUE` | [cli.ts:170](../src/cli.ts#L170) |
| 2 | `--qr-code: small or large expected, got VALUE` | [cli.ts:175](../src/cli.ts#L175) |
| 2 | `--scope: user or local expected, got VALUE` | [cli.ts:180](../src/cli.ts#L180) |
| 2 | `--owners: ID:NAME expected, got ENTRY` | [cli.ts:192](../src/cli.ts#L192) |
| 2 | `--expire: hours from 1 to 23 expected, got VALUE` | [cli.ts:87](../src/cli.ts#L87), [:198](../src/cli.ts#L198) |
| 2 | `--stop-wait: seconds from 0 to 60 expected, got VALUE` | [cli.ts:90](../src/cli.ts#L90), [:198](../src/cli.ts#L198) |
| 2 | `--port: a port from 1 to 65535 expected, got VALUE` | [cli.ts:99](../src/cli.ts#L99), [:198](../src/cli.ts#L198) |
| 2 | `--reset-owners takes --owners without a list: member IDs change in a new agent control chat.` | [cli.ts:112](../src/cli.ts#L112) |
| 2 | `--inference: an http or https URL without user name and password expected, got VALUE` | [cli.ts:186](../src/cli.ts#L186) |
| 2 | `--inference: status messages are shown only in mode requests.` | [cli.ts:115](../src/cli.ts#L115) |
| 2 | `SIMPLEX_MCP_INFERENCE_KEY: visible ASCII characters expected.`, with `--inference`, for a set key, empty or with a character outside 0x21 to 0x7e, before the data folder is prepared | [serve.ts:69](../src/serve.ts#L69), [:215](../src/serve.ts#L215) |
| 2 | `--owners: member ENTRY is not in the agent control chat. Start with --reset-owners --owners, or use the IDs of current members.` | [control.ts:356](../src/control.ts#L356) |
| 1 | `Another server uses DATA (PID N). If no server runs, remove DATA/lock.` | [files.ts:21-23](../src/files.ts#L21-L23), [:60-62](../src/files.ts#L60-L62) |
| 1 | `Port PORT is in use. Start with --port N.` | [serve.ts:243](../src/serve.ts#L243) |
| 1 | `Agent "NAME" was never started. Start it with: npx -y @simplex-chat/mcp serve NAME` | [register.ts:35](../src/register.ts#L35) |
| 1 | `Add this entry under LOCATION in ~/.claude.json: {"simplex-NAME": ENTRY}` | [register.ts:55-58](../src/register.ts#L55-L58) |
| 1 | `claude mcp add-json failed: STDERR` | [register.ts:42](../src/register.ts#L42) |
| 1 | `simplex-mcp: ERROR` for other errors, `stop: ERROR` for a failed stop | [cli.ts:233-234](../src/cli.ts#L233-L234), [serve.ts:57-60](../src/serve.ts#L57-L60) |

LOCATION is `"mcpServers"` for scope `user`, and `"projects" > "PROJECT" > "mcpServers"` for scope `local` ([register.ts:56](../src/register.ts#L56)).

### 3.4 Terminal lines

stdout ([log.ts:37-39](../src/log.ts#L37-L39)):

| Moment | Line | Code |
|---|---|---|
| start | `SimpleX MCP server VERSION, agent "NAME"`<br>`Data folder: DATA`<br>`Approval mode: MODE`<br>`Inference endpoint: URL` with `--inference` | [serve.ts:76-79](../src/serve.ts#L76-L79) |
| control chat created | `Agent control chat "DISPLAY" created.` | [control.ts:518](../src/control.ts#L518) |
| link opened | `Join the agent control chat in the SimpleX app. The link works once:`<br>QR code<br>link<br>small QR code: `If the QR code does not scan, restart with --qr-code large, or paste the link into the app.`<br>role line | [control.ts:406-416](../src/control.ts#L406-L416) |
| listening | `Listening on http://127.0.0.1:PORT/mcp` | [serve.ts:131](../src/serve.ts#L131) |
| owner setup done | `Add the agent to Claude Code:`<br>`  npx -y @simplex-chat/mcp register NAME [--data DATA]` | [serve.ts:191-193](../src/serve.ts#L191-L193) |
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
| `send` with `live: true` shown | `Note to KIND NAME (CHAT): TEXT`, or `an unnamed KIND`; the name and the text folded to one line | [tools.ts:824](../src/tools.ts#L824), [actions.ts:310-313](../src/actions.ts#L310-L313) |
| `/leave CHAT` | `Left KIND NAME (CHAT), requested by NAME.` or `Deleted ...` | [control.ts:289](../src/control.ts#L289) |
| refused file, file not sent | as in [2.3](#23-other-posts), names and paths on one line | [control.ts:156](../src/control.ts#L156), [:167](../src/control.ts#L167) |
| stop | `Cancelled K action.` or `Cancelled K actions.` when K > 0<br>`Stopping: sending PARTS, up to S s. Ctrl-C to stop now.` when PARTS is not empty and S > 0; PARTS is `K cancelled card(s)` when cards were edited or posted and the control chat is not lost, and `M status deletion(s)` when status messages had a render, joined by ` and `<br>`Stopped.` | [serve.ts:181-183](../src/serve.ts#L181-L183), [:195-211](../src/serve.ts#L195-L211), [:186](../src/serve.ts#L186) |
| membership messages | see [2.4](#24-membership-messages) | |

stderr: `CONTEXT: ERROR` from `logError` ([log.ts:41-43](../src/log.ts#L41-L43)), with CONTEXT one of `action N`, `action N card`, `action N card image`, `action N file`, `control chat command`, `EVENT in the agent control chat`, `EVENT event`, `refused file`, `file not sent`, `download of file N`, `file N`, `moving file N`, `cancelling file N`, `resizing PATH`, `preview`, `MCP`, `HTTP request`, `stop`, `simplex-mcp`, `status of CHAT`, `inference endpoint`, `status messages`. The library's download lines are written to stderr.

| CONTEXT | Error | Code |
|---|---|---|
| `status of CHAT` | a failed send, update or deletion of a status message; a failed eligibility read of a current status; a failed tick; a failed leftover deletion | [status.ts:51](../src/status.ts#L51), [:97](../src/status.ts#L97), [:111-114](../src/status.ts#L111-L114), [:117-118](../src/status.ts#L117-L118), [:141](../src/status.ts#L141) |
| `inference endpoint` | the first failed poll after the start or after a successful poll | [status.ts:157-159](../src/status.ts#L157-L159) |
| `status messages` | a failed chat list before the leftover deletion | [serve.ts:115](../src/serve.ts#L115) |

A poll error is `GET URL: REASON`, folded to one line, with REASON `HTTP N`, `invalid JSON`, `body over 1 MiB`, `no answer within 1 s`, or the message of the first error aggregated in the error's cause, of the cause, or of the error itself, in this order ([inference.ts:109-141](../src/inference.ts#L109-L141)).
