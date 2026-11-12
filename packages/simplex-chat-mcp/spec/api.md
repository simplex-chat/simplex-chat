# SimpleX MCP server: API

> The six MCP tools, the status message, the agent control chat commands, replies and cards, the CLI and the hook endpoint of `@simplex-chat/mcp` 0.3.0.
>
> Related specs: [README](README.md) | [Architecture](architecture.md) | [Flows](flows.md)

**Source:** [`tools.ts`](../src/tools.ts) | [`control.ts`](../src/control.ts) | [`cli.ts`](../src/cli.ts) | [`messages.ts`](../src/messages.ts) | [`actions.ts`](../src/actions.ts) | [`status.ts`](../src/status.ts) | [`inference.ts`](../src/inference.ts) | [`register.ts`](../src/register.ts) | [`http.ts`](../src/http.ts) | [`activity.ts`](../src/activity.ts)

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
   - [1.11 `/abort`](#111-abort)
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
4. [Hook endpoint](#4-hook-endpoint)
   - [4.1 Request](#41-request)
   - [4.2 Events](#42-events)

---

## 1. Tools

### 1.1 Common rules

| Rule | Code |
|---|---|
| Tools are registered on each per-request `McpServer` with description, input schema, output schema and, in mode `host`, `_meta: {"anthropic/requiresUserInteraction": true}` for `connect`, `send`, `download` and `leave` | [tools.ts:464-471](../src/tools.ts#L464-L471), [:507-517](../src/tools.ts#L507-L517) |
| Tool descriptions and server instructions; the instructions name the agent's display name | [tools.ts:414-454](../src/tools.ts#L414-L454), [serve.ts:121](../src/serve.ts#L121) |
| Arguments are validated by the SDK; a failure is a result with `isError: true` and the text `Input validation error: Invalid arguments for tool NAME: ERRORS` | [mcp-BumlNux0.cjs:1812-1817](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L1812-L1817) |
| `additionalProperties: false` is set in every input schema | [tools.ts:258](../src/tools.ts#L258), [:276](../src/tools.ts#L276), [:288](../src/tools.ts#L288), [:291](../src/tools.ts#L291), [:299](../src/tools.ts#L299), [:310](../src/tools.ts#L310) |
| A result is sent as `structuredContent` and as its JSON in one text item | [tools.ts:523](../src/tools.ts#L523) |
| A tool error is sent as `isError: true` with one text item: the `ToolError` text, or the `errorText` of other errors | [tools.ts:525](../src/tools.ts#L525), [log.ts:45-49](../src/log.ts#L45-L49) |
| An aborted call ends with the result `Cancelled.`, `isError: true`, which the SDK discards | [tools.ts:473](../src/tools.ts#L473), [:515](../src/tools.ts#L515) |
| With a `progressToken` in `_meta`, `notifications/progress` is sent at the start of each wait and every 60 s, with `progress` 0, 1, 2, ... and a `message` | [tools.ts:1653-1663](../src/tools.ts#L1653-L1663), [messages.ts:301-305](../src/messages.ts#L301-L305) |
| The call is aborted when the client closes the response stream or sends `notifications/cancelled`; the action is kept | [src-BhnGYybN.cjs:6338-6341](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs#L6338-L6341), [:6425](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs#L6425) |
| A call with the same tool and action fields as an action in memory is attached to it; a `send` with `live: true` is never attached | [tools.ts:1645-1647](../src/tools.ts#L1645-L1647), [:817](../src/tools.ts#L817), [actions.ts:151-153](../src/actions.ts#L151-L153) |
| Messages are marked read after the last wait of a call that is not aborted | [tools.ts:707](../src/tools.ts#L707), [:874](../src/tools.ts#L874), [:1223](../src/tools.ts#L1223) |

Action fields of the key:

| Tool | Fields |
|---|---|
| `connect` | `target`<br>`incognito` (default `false`)<br>`message` (default `null`) |
| `send` | `chat`<br>`text` (default `null`)<br>`file` (default `null`)<br>`as` (default `null`)<br>`reply_to` (default `null`)<br>`mention` (default `null`) |
| `download` | `chat`<br>`item` |
| `leave` | `chat` |

Result status of the four action tools ([`actionResult`](../src/tools.ts#L676-L688)):

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

Handler: [`AgentTools.connect`](../src/tools.ts#L657-L670). Perform: [`performConnect`](../src/tools.ts#L726-L757). Schemas: [input](../src/tools.ts#L248-L259), [output](../src/tools.ts#L316-L326).

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
| `known` | `status`, chat fields of the known chat | [tools.ts:662](../src/tools.ts#L662) |
| `done`, chat unknown | `action`, `status`, `type`, `connected: false`, plan fields `name`, `peer_type`, `description`, `welcome`, `commands`, `known_as` when present | [tools.ts:673](../src/tools.ts#L673), [:1430-1436](../src/tools.ts#L1430-L1436) |
| `done`, chat known | `action`, `status`, chat fields, `messages` | [tools.ts:690-710](../src/tools.ts#L690-L710) |
| `failed` | `action`, `status`, `error` | [tools.ts:681](../src/tools.ts#L681) |
| `rejected`, `expired` | `action`, `status` | [tools.ts:681](../src/tools.ts#L681) |

Chat fields ([`chatFields`](../src/tools.ts#L1286-L1313)):

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

`messages`: message items among the last 50 items of the chat when the result is built, replaced by their collected copies, followed by the items collected since the connection; live items are left out; new items are marked read ([tools.ts:692](../src/tools.ts#L692), [:702-709](../src/tools.ts#L702-L709)).

Tool errors:

| Text | Condition | Code |
|---|---|---|
| `Not a SimpleX link or name: TARGET` | whitespace in `target`, or `invalidConnReq` from `apiConnectPlan` | [tools.ts:714](../src/tools.ts#L714), [:720](../src/tools.ts#L720) |
| an app error text, for example `Name not found: This SimpleX name is not registered. Please check the name.`, or `SimpleX error: CHAIN: DETAILS` | other errors of `apiConnectPlan`, or a plan of type `error` | [tools.ts:721](../src/tools.ts#L721), [:1357](../src/tools.ts#L1357), [log.ts:51-53](../src/log.ts#L51-L53) |
| `TARGET can't be joined now: PLAN.` | `invitationLink` other than `ok` and `known`<br>`contactAddress` other than `ok`, `known`, `contactViaAddress` and `connectingConfirmReconnect`<br>`groupLink` other than `ok`, `known` and `connectingConfirmReconnect` | [tools.ts:1348](../src/tools.ts#L1348), [:1361](../src/tools.ts#L1361), [:1379](../src/tools.ts#L1379), [:1401](../src/tools.ts#L1401) |
| `message: TARGET is not a contact or business address with short link data, or its contact exists without a connection.` | `message` given and the plan has `takesMessage: false` | [tools.ts:664](../src/tools.ts#L664) |
| refusals | see [1.9](#19-action-refusals) | |

`takesMessage` is `true` for a contact address with short link data, for a known prepared contact, and for a known prepared business group ([tools.ts:1352](../src/tools.ts#L1352), [:1372](../src/tools.ts#L1372), [:1390](../src/tools.ts#L1390)).

### 1.3 `send`

Handler: [`AgentTools.send`](../src/tools.ts#L814-L840). Result: [`sendResult`](../src/tools.ts#L864-L884). Perform: [`performSend`](../src/tools.ts#L886-L911). Note: [`showNote`](../src/tools.ts#L842-L849). Schemas: [input](../src/tools.ts#L261-L277), [output](../src/tools.ts#L328-L340).

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

File types by extension, lower-cased ([tools.ts:176-185](../src/tools.ts#L176-L185), [`attachmentMedia`](../src/tools.ts#L1603-L1614)):

| Extension | Message type | Sent file | Preview and duration |
|---|---|---|---|
| `.jpg`, `.jpeg`, `.png` | image | `core.resizeImage(..., Image)`, written to `DATA/files/sent/`; the original when resizing fails | `core.resizeImage(..., MessagePreview)`; the fixed preview when it fails |
| `.gif`, `.webp` | image | unchanged | the fixed preview |
| `.mp4`, `.mov` | video | unchanged | the first frame from `ffmpeg`, resized as a preview; duration from `ffprobe`, rounded; the fixed preview and 0 when they fail |
| other, or `as: "file"` | file | unchanged | |

The fixed preview is a 16 × 9 mid-gray PNG ([tools.ts:166](../src/tools.ts#L166)). `ffmpeg` and `ffprobe` run with `-protocol_whitelist file -f mov`, 30 s and 64 MB of output each ([tools.ts:1625-1643](../src/tools.ts#L1625-L1643)).

Output:

| Field | Presence | Value |
|---|---|---|
| `action` | without `live` | action number |
| `status` | always | `done`, `failed`, `rejected`, `expired`; `live` with `live: true` |
| `chat` | `done`, `live` | the chat reference |
| `item` | `done` | item ID of the sent message |
| `responses` | `done` | received message items with IDs above `item`, in arrival order, live items and an `/abort` of [1.11](#111-abort) left out, at most `limit` |
| `more` | `done` | collected responses not returned, live ones and overflow included |
| `error` | `failed` | error text |

Only `status` is required by the output schema ([tools.ts:339](../src/tools.ts#L339)).

Tool errors, in the order of the checks:

| Text | Condition | Code |
|---|---|---|
| `text: required without file.` | neither `text` nor `file` | [tools.ts:819](../src/tools.ts#L819) |
| `as: applies only with file.` | `as` without `file` | [tools.ts:820](../src/tools.ts#L820) |
| `live: applies to text only; send file, reply_to and mention without live.` | `live` with `file`, `reply_to` or `mention` | [tools.ts:821](../src/tools.ts#L821) |
| `text: at most 2000 characters with live.` | `live` with `text` over 2000 code points | [tools.ts:822](../src/tools.ts#L822), [:163](../src/tools.ts#L163) |
| `No chat CHAT. list shows the agent's chats.` | invalid reference, the control chat, or a store error when the chat is read | [tools.ts:1252-1259](../src/tools.ts#L1252-L1259), [:1547-1549](../src/tools.ts#L1547-L1549) |
| `The agent is not connected to CHAT and can't post; list shows connected: false.` | the chat is not connected | [tools.ts:824-825](../src/tools.ts#L824-L825), [:1477](../src/tools.ts#L1477) |
| `The agent has role ROLE in CHAT and can't post.` | membership role `observer` or `relay` | [tools.ts:824-825](../src/tools.ts#L824-L825), [:1478-1481](../src/tools.ts#L1478-L1481) |
| `No chat CHAT. list shows the agent's chats.` | `live` while `leave` or `/leave` of the chat runs | [tools.ts:843-844](../src/tools.ts#L843-L844), [:852](../src/tools.ts#L852) |
| `live: CHAT is a channel or a chat with a bot or an agent; status messages are shown in chats with people, groups and business chats.` | `live` in a channel, or in a direct chat whose contact has peer type `bot` or `agent` | [tools.ts:853](../src/tools.ts#L853), [:1455-1462](../src/tools.ts#L1455-L1462) |
| `live: status messages are shown in mode requests only; this server runs in mode host.` | `live` in mode `host` | [tools.ts:854](../src/tools.ts#L854) |
| `live: The agent lost its control chat. The person who runs it has to restart it with --reset-owners.` | `live` after the control chat is lost | [tools.ts:855](../src/tools.ts#L855), [actions.ts:14](../src/actions.ts#L14) |
| `live: your owner's approval of all messages to CHAT is required; list shows approved: true for such chats.` | `live` in a chat without a standing approval of messages by a current approver | [tools.ts:856](../src/tools.ts#L856), [control.ts:130-133](../src/control.ts#L130-L133) |
| `The server is stopping.` | `live` after the stop started | [status.ts:67](../src/status.ts#L67), [actions.ts:15](../src/actions.ts#L15) |
| `live: the update limit of the status message of CHAT is reached; send your answer without live.` | `live` when the status message has 59 renders | [status.ts:70-71](../src/status.ts#L70-L71) |
| `reply_to: CHAT has no item N.` | the read of `around=N` fails, or the item is absent from it | [tools.ts:1264](../src/tools.ts#L1264) |
| `reply_to: item N in CHAT is not a message.` | the item's content is not `rcvMsgContent` or `sndMsgContent` | [tools.ts:1266](../src/tools.ts#L1266) |
| `mention: CHAT is a contact chat; mentions apply to groups and channels.` | `mention` with `@N` | [tools.ts:1272](../src/tools.ts#L1272) |
| `mention: CHAT has no member N.` | ID absent from `apiListMembers` | [tools.ts:1277](../src/tools.ts#L1277) |
| `mention: members N and M have the same name.` | two mentioned members with the same display name | [tools.ts:1280](../src/tools.ts#L1280) |
| `file: an absolute path is expected.`<br>`file: no file at FILE.`<br>`file: PATH is not a file.`<br>`file: PATH can't be read.` | the path, after symbolic links | [tools.ts:1556-1567](../src/tools.ts#L1556-L1567) |
| `file: PATH is refused: private keys and the server's own files are not sent.` | a file named `id_*`, `*.pem`, `*.key`, `*.p12` or `*.pfx`, or under `~/.ssh`, `~/.gnupg`, or the data folder or `~/.simplex-mcp` outside `DATA/files/`; each folder by its real path | [tools.ts:927-930](../src/tools.ts#L927-L930), [:936-941](../src/tools.ts#L936-L941) |
| `file: PATH is empty.` | size 0 | [tools.ts:931](../src/tools.ts#L931) |
| `text: too long for the message with its quote; shorten it.`<br>`text: too long for a message with this TYPE; shorten it.` | the message, as the core sends it, above 15602 bytes | [tools.ts:830-832](../src/tools.ts#L830-L832), [:1524-1540](../src/tools.ts#L1524-L1540) |
| refusals | see [1.9](#19-action-refusals) | |

The message is measured as 600 bytes plus the JSON of its content, the file's name, the quote and the mentioned names. The quote is the quoted text, or its file's name when the text is empty; a sent image or video quotes it as text, otherwise an image's preview is kept ([tools.ts:1524-1540](../src/tools.ts#L1524-L1540)).

The perform fails with `file: PATH changed after the request.` when the file's real path, size, inode or change time differs from the request ([tools.ts:943-948](../src/tools.ts#L943-L948)). A resized copy is deleted at `sndFileCompleteXFTP`, at `sndFileError` and after a failed send ([tools.ts:908](../src/tools.ts#L908), [:913-923](../src/tools.ts#L913-L923), [serve.ts:164-168](../src/serve.ts#L164-L168)). After `apiSendMessages` returns, the status message of `chat` is deleted ([tools.ts:902](../src/tools.ts#L902), [1.10](#110-status-message)). In a typing chat ([1.4](#14-receive)), a status message is started after the returned responses are marked read, unless the call was aborted ([tools.ts:874-876](../src/tools.ts#L874-L876)).

### 1.4 `receive`

Handler: [`AgentTools.receive`](../src/tools.ts#L1192-L1230). Schemas: [input](../src/tools.ts#L279-L289), [output](../src/tools.ts#L342-L350).

Input:

| Field | Type | Range | Default | Rule |
|---|---|---|---|---|
| `chat` | string | | required | `@N` or `#N` of the agent |
| `wait` | number | 0 to 86400 | 0 | seconds to wait when no message is new |
| `limit` | integer | 1 to 100 | 50 | most messages returned |
| `typing` | boolean | | `false` | `true`: when the call returns messages, a status message is started in `chat`, and `chat` is a typing chat until a `receive` of it without `typing` ([1.10](#110-status-message)) |

Output (all fields always present):

| Field | Value |
|---|---|
| `chat` | the chat reference |
| `messages` | unread received messages in chat order, then received messages that arrived during the call; live items and an `/abort` of [1.11](#111-abort) left out; at most `limit` |
| `more` | new messages not returned, overflow, and unread items left unread by the page reads |

Tool errors: `No chat CHAT. list shows the agent's chats.` for an invalid reference, the control chat, or a store error of the first read, `initial` 1 ([tools.ts:1194](../src/tools.ts#L1194), [:1200-1202](../src/tools.ts#L1200-L1202)); other chat errors, a store error of a later page read included, as their error text ([log.ts:45-53](../src/log.ts#L45-L53)).

Messages are read by `receive` without an action or an approval.

When no message is new and `wait` is above 0, the status message of `chat` is deleted before the wait ([tools.ts:1209-1210](../src/tools.ts#L1209-L1210)). With `typing: true`, the status message is started after the returned messages are marked read, unless the call was aborted ([tools.ts:1222-1225](../src/tools.ts#L1222-L1225)). The typing chats are kept in memory: `chat` is added by a `receive` with `typing: true` and removed by one without it, before the chat is read ([tools.ts:499](../src/tools.ts#L499), [:1195-1196](../src/tools.ts#L1195-L1196)).

### 1.5 `download`

Handler: [`AgentTools.download`](../src/tools.ts#L1031-L1052). Result: [`downloadResult`](../src/tools.ts#L1079-L1087). Perform: [`performDownload`](../src/tools.ts#L1089-L1093), [`downloadFile`](../src/tools.ts#L544-L571). Schemas: [input](../src/tools.ts#L302-L311), [output](../src/tools.ts#L397-L412).

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

FOLDER is `group-N` for `#N` and `contact-N` for `@N`; the extension is the file name's extension when it has 1 to 16 ASCII letters and digits ([messages.ts:215-223](../src/messages.ts#L215-L223)). `/forward` downloads a file to `DATA/forwarded/FOLDER/` under the same name; a later perform of the download moves it to the download path, after the copy for the post is made ([tools.ts:544-561](../src/tools.ts#L544-L561)).

Steps before the action:

| Step | Code |
|---|---|
| a file in status `rcvInvitation`, or an XFTP file: the file's servers from `apiGetChatItemInfo`; while none, the call waits for `rcvFileDescrReady` until `wait` seconds after the call, with the progress `The sender is uploading the file of item N in CHAT; waiting until TIME.` | [tools.ts:1054-1071](../src/tools.ts#L1054-L1071) |
| unknown servers: the file's servers that are not among the XFTP servers of `apiGetUserServers`, of no operator or of an enabled one, compared without their passwords | [tools.ts:1073-1077](../src/tools.ts#L1073-L1077) |
| without unknown servers, and for a file ID not in rejected files, the chat's files approval, read again, makes the action performed at once | [tools.ts:1046](../src/tools.ts#L1046) |

Perform: `apiReceiveFile(fileId, path, approvedRelays)`, for a file in status `rcvInvitation`. `approvedRelays` is true in mode `requests` for a download whose card named unknown servers, and false otherwise; the core then checks the complete description ([tools.ts:567](../src/tools.ts#L567), [:1095-1104](../src/tools.ts#L1095-L1104)). The download ends at `rcvFileComplete`, `rcvFileError`, `rcvFileSndCancelled` or `chatItemUpdated` of its file, or at a check every 60 s ([tools.ts:601-623](../src/tools.ts#L601-L623), [:1106-1174](../src/tools.ts#L1106-L1174)).

Failure texts of the perform:

| Text | Condition | Code |
|---|---|---|
| the file's error, as in the file field | the file failed | [tools.ts:1135-1145](../src/tools.ts#L1135-L1145) |
| `The sender cancelled the file.` | `rcvFileSndCancelled` | [tools.ts:618-623](../src/tools.ts#L618-L623) |
| `The message or its chat was deleted.` | a check finds no message, contact or group | [tools.ts:170](../src/tools.ts#L170), [:1125-1133](../src/tools.ts#L1125-L1133) |
| `The download made no progress for 30 minutes and was cancelled.` | no rise of `rcvProgress` for 30 minutes; the file is cancelled | [tools.ts:174](../src/tools.ts#L174), [:1118-1122](../src/tools.ts#L1118-L1122) |
| `The received file is not in the agent's files folder.` | the file is received and neither the download path nor the forwarded path has a file | [messages.ts:14](../src/messages.ts#L14), [tools.ts:1147-1151](../src/tools.ts#L1147-L1151) |
| `Your owner deleted the files of this chat.` | `/delete CHAT files` | [tools.ts:171](../src/tools.ts#L171), [:974-992](../src/tools.ts#L974-L992) |
| `The file is on unknown servers that were not approved.` | a check reads an accepted file in status `rcvInvitation`: the core returned it to the offer when its description completed | [tools.ts:172](../src/tools.ts#L172), [:1114](../src/tools.ts#L1114) |
| `Unknown servers! Without Tor or VPN, your IP address will be visible to these XFTP relays: HOST, HOST.` | the core refuses the servers of the complete description at the acceptance | [log.ts:78-79](../src/log.ts#L78-L79) |

Tool errors:

| Text | Condition | Code |
|---|---|---|
| `No chat CHAT. list shows the agent's chats.` | invalid reference, the control chat, or a store error when the chat is read | [tools.ts:1036](../src/tools.ts#L1036) |
| `item: CHAT has no item N.` | the item is not found | [tools.ts:1038](../src/tools.ts#L1038) |
| `item: item N in CHAT has no received file.` | the item is not received or has no file | [tools.ts:1040](../src/tools.ts#L1040) |
| `item: the file of item N in CHAT failed: ERROR` | the file field has status `failed` | [tools.ts:1041-1042](../src/tools.ts#L1041-L1042) |
| `item: the sender has not uploaded the file of item N in CHAT yet; call download again later.` | no servers by the end of `wait`, also for a file invited without a description, whose servers the core does not list; no action is created | [tools.ts:1044](../src/tools.ts#L1044), [:1069](../src/tools.ts#L1069) |
| refusals | see [1.9](#19-action-refusals) | |

A file ID whose download returned `rejected` or `expired` shows status `rejected` until a download of it is done or the server restarts ([tools.ts:1084-1085](../src/tools.ts#L1084-L1085)).

### 1.6 `list`

Handler: [`AgentTools.listChats`](../src/tools.ts#L1232-L1240). Schemas: [input](../src/tools.ts#L291), [output](../src/tools.ts#L352-L384).

Input: an empty object.

Output:

| Field | Item fields |
|---|---|
| `chats` | `chat`<br>`type`<br>`name`<br>`simplex_name`, when verified<br>`peer_type`, when present<br>`incognito`<br>`known_as`, when incognito<br>`connected`<br>`unread`: `chatStats.unreadCount`<br>`approved: true`, only under a standing approval of messages in mode `requests` |
| `actions` | `action`<br>`tool`<br>`input`: the input of the creating call<br>`state`<br>`open`: `true` while a call waits on it |

`chats`: the contacts and groups of the main conversations from one read, `apiGetChats(USER, last 1000000)`, except the control chat ([tools.ts:1242-1250](../src/tools.ts#L1242-L1250)). `actions`: every action in memory ([tools.ts:1238](../src/tools.ts#L1238)).

### 1.7 `leave`

Handler: [`AgentTools.leave`](../src/tools.ts#L1000-L1009). Perform: [`leaveChat`](../src/tools.ts#L1015-L1029). Schemas: [input](../src/tools.ts#L293-L300), [output](../src/tools.ts#L386-L395).

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

The chat is marked as being left from before its status message is deleted until these methods end; the status message is deleted first ([tools.ts:1016-1018](../src/tools.ts#L1016-L1018), [:1025-1027](../src/tools.ts#L1025-L1027)).

Tool errors: `No chat CHAT. list shows the agent's chats.` when no action with the same key is in memory and the chat is invalid, the control chat or unknown ([tools.ts:1003-1005](../src/tools.ts#L1003-L1005)); refusals.

### 1.8 Message, file and command objects

Message ([`Message`](../src/messages.ts#L16-L28), [`toMessage`](../src/messages.ts#L174-L192), [schema](../src/tools.ts#L187-L220)):

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

Only received message items (`rcvMsgContent`) are returned by `send` and `receive`; `connect` also returns sent ones (`sndMsgContent`) ([messages.ts:101](../src/messages.ts#L101), [tools.ts:702](../src/tools.ts#L702)).

Command ([`commandsOf`](../src/tools.ts#L1506-L1513), [schema](../src/tools.ts#L222-L232)):

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

A status message is one live message of the agent in a chat, edited while the agent works and deleted afterwards, or ended with `◼ aborted` at an `/abort` of [1.11](#111-abort) ([`Statuses`](../src/status.ts#L32-L183)).

Eligibility, all of ([`showsStatus`](../src/tools.ts#L860-L862)):

| Condition | Code |
|---|---|
| the agent can post: the chat is connected and, in a group, the agent's role is other than `observer` and `relay` | [`postingRefusal`](../src/tools.ts#L1476-L1483) |
| the chat is not being left by `leave` or `/leave` | [tools.ts:852](../src/tools.ts#L852) |
| the chat is a direct chat with a contact of peer type other than `bot` and `agent`, a group or a business chat | [tools.ts:853](../src/tools.ts#L853), [:1455-1462](../src/tools.ts#L1455-L1462) |
| mode `requests` | [tools.ts:854](../src/tools.ts#L854) |
| the control chat is not lost | [tools.ts:855](../src/tools.ts#L855) |
| a standing approval of messages to the chat, by a current approver | [tools.ts:856](../src/tools.ts#L856), [control.ts:130-133](../src/control.ts#L130-L133) |

The chat and its eligibility are read again at each tick, the first tick included ([tools.ts:503-505](../src/tools.ts#L503-L505), [status.ts:137-143](../src/status.ts#L137-L143)). For a `send` with `live: true`, they are checked with the chat read of the call ([1.3](#13-send)).

Start:

| Event | Effect | Code |
|---|---|---|
| a received message, not live, in the main conversation of a typing chat ([1.4](#14-receive)), from any sender; the chat has no status, and the stop has not started | a waiting status is created with the label `received, waiting for the agent`, and its first tick runs at once; when the chat is not eligible at that tick, the status ends without a post | [tools.ts:580-583](../src/tools.ts#L580-L583), [status.ts:52-56](../src/status.ts#L52-L56) |
| `receive` with `typing: true`, or `send` in a typing chat, returns messages, from any sender; the stop has not started | no status: a status is created with the label `thinking`, as above<br>a waiting status: the status is taken: its label is set to `thinking`, and it is rendered at once when it has 1 to 58 renders<br>another status: unchanged | [tools.ts:876](../src/tools.ts#L876), [:1225](../src/tools.ts#L1225), [status.ts:58-64](../src/status.ts#L58-L64) |
| `send` with `live: true` passes its checks; the chat has no status, or one created at least 23 hours earlier, which is deleted first | a status is created with the note and rendered at once; its next tick is in 3 s | [status.ts:66-76](../src/status.ts#L66-L76) |

Text ([`statusText`](../src/status.ts#L185-L189)):

| Part | Value |
|---|---|
| line of renders 1 to 49 | `FRAME LABEL · M:SS`; FRAME is `◐`, `◓`, `◑`, `◒` in turn, from `◐`; M:SS is the time since the status was created |
| line of render 50 and later | `◌ working since YYYY-MM-DD HH:MM UTC`, or `◌ received, waiting for the agent since YYYY-MM-DD HH:MM UTC` for a waiting status; the time the status was created ([status.ts:187](../src/status.ts#L187), [messages.ts:333-335](../src/messages.ts#L333-L335)) |
| offer | with `--abort-file`, ` · /abort`; otherwise empty ([status.ts:155](../src/status.ts#L155)) |
| text | the line and the offer; after a `send` with `live: true`, the line, the offer, a newline and the text of the last such call ([status.ts:188](../src/status.ts#L188)) |

Labels, first match ([`statusLabel`](../src/status.ts#L191-L228)):

| Label | Condition |
|---|---|
| `received, waiting for the agent` | a waiting status, until it is taken; its label is not read ([status.ts:28](../src/status.ts#L28), [:144](../src/status.ts#L144)) |
| ``loading `MODEL` ``, or `loading the model` when the ID is unknown | `--inference`: state `starting` of a model of `/running`; state `stopping` of a model while 0 models are ready; or 503 from `/props` or `/slots` of a polled server |
| ``reading the prompt with `MODEL`, N tokens read`` | `--inference`: processing slots, each with `n_decoded` 0; N is the sum of `n_prompt_tokens_processed` of these slots |
| ``generating with `MODEL`, N tokens/s`` | `--inference`: a processing slot with `n_decoded` above 0; N is the rate between two polls, rounded |
| `waiting for approval` | a pending action has an open call |
| `waiting for permission in Claude Code` | the permission flag of an agent session is set ([4.2](#42-events)) |
| `using TOOL` | a running call other than `receive`; TOOL is the tool of the latest such call |
| `compacting the conversation` | the compaction flag of an agent session is set |
| ``reading `FILE` ``, or `reading a file` | the latest running Claude Code tool is `Read` |
| ``editing `FILE` ``, or `editing a file` | the latest running Claude Code tool is `Edit`, `Write`, `MultiEdit` or `NotebookEdit` |
| `running a command` | the latest running Claude Code tool is `Bash`, `PowerShell` or `Monitor` |
| `searching files` | the latest running Claude Code tool is `Grep` or `Glob` |
| `searching the web` | the latest running Claude Code tool is `WebSearch` or `WebFetch` |
| ``using `TOOL` ``, or `using a tool` | the latest running Claude Code tool is another tool; TOOL is its name, and for an MCP tool `mcp__SERVER__TOOL` the part after `mcp__SERVER__` |
| ``running subagent `TYPE` ``, or `running a subagent` | a subagent of an agent session runs; TYPE is the `agent_type` of the latest one |
| `working` | `--inference`: slots were read, 0 of them processing; or a polled server sleeps: `is_sleeping` of `/props` is true |
| `thinking` | otherwise; at the first render of a status other than a waiting one, and at the take of a waiting status |

- `` with `MODEL` `` is shown when all processing slots belong to one model with a known ID; `, N tokens read` when N is above 0; `, N tokens/s` when the same slot and task were read in the previous poll, processing with tokens already decoded, and more tokens were decoded since then ([inference.ts:92-107](../src/inference.ts#L92-L107)).
- A model ID is kept only when it matches `^[\w.:/+@-]{1,64}$`, and it is shown as a code span ([inference.ts:27](../src/inference.ts#L27), [:55](../src/inference.ts#L55), [:150-153](../src/inference.ts#L150-L153), [status.ts:206-208](../src/status.ts#L206-L208), [:226-228](../src/status.ts#L226-L228)).
- The labels of Claude Code are chosen from the events of the status hooks of `register` ([3.2](#32-register), [4](#4-hook-endpoint)). FILE is kept only when it matches `^[\w.+@-]{1,64}$`, and a tool name and TYPE only when they match `^[\w-]{1,64}$`; otherwise the second form is shown. FILE, TOOL and TYPE are shown as code spans ([activity.ts:21-22](../src/activity.ts#L21-L22), [:36](../src/activity.ts#L36), [:64](../src/activity.ts#L64), [:103-106](../src/activity.ts#L103-L106), [status.ts:210-228](../src/status.ts#L210-L228)).
- The label is read at renders 2 to 49 of a status other than a waiting one, after the inference poll, and it is the same for every chat ([status.ts:144-148](../src/status.ts#L144-L148), [:164-169](../src/status.ts#L164-L169)).
- Every tool call is registered as running from its start to its end ([tools.ts:520](../src/tools.ts#L520), [:526-528](../src/tools.ts#L526-L528), [status.ts:42-50](../src/status.ts#L42-L50)).
- After a failed poll, the label is chosen as without `--inference` ([status.ts:171-182](../src/status.ts#L171-L182)).

Schedule ([`tick`](../src/status.ts#L136-L151), [`schedule`](../src/status.ts#L158-L162)):

| Rule | Value |
|---|---|
| renders by ticks | one per tick while the renders are below 50 |
| gap to the next tick, renders below 50 | a tenth of the status's age, at least 3 s, at most 20 s |
| gap to the next tick, renders 50 or more | 5 min |
| renders by notes | one per `send` with `live: true`, at once, while the renders are below 59 |
| render by a take | one, at once, when the waiting status has 1 to 58 renders ([status.ts:63](../src/status.ts#L63)) |
| commands | the first render is a live send, later renders are live updates; a text equal to the last sent one is skipped ([api.ts:1160-1171](../../simplex-chat-nodejs/src/api.ts#L1160-L1171), [:1199-1213](../../simplex-chat-nodejs/src/api.ts#L1199-L1213)) |

The note of the last `send` with `live: true` is kept under the line until the next note or the deletion.

Deletion, at the first of:

| Event | Code |
|---|---|
| `send` without `live` in the chat, after `apiSendMessages` of its perform returns | [tools.ts:902](../src/tools.ts#L902) |
| `receive` of the chat, when nothing is new and `wait` is above 0 | [tools.ts:1209-1210](../src/tools.ts#L1209-L1210) |
| `leave` or `/leave` of the chat, before the chat is deleted | [tools.ts:1018](../src/tools.ts#L1018) |
| a tick: the chat is not eligible, or its read fails | [status.ts:137-143](../src/status.ts#L137-L143) |
| a tick: 23 hours after the status was created | [status.ts:143](../src/status.ts#L143) |
| `send` with `live: true`, 23 hours or more after the status was created; a new status is created with the note | [status.ts:69](../src/status.ts#L69) |
| a tick: 50 renders or more, no running call other than `receive`, and 30 minutes since a call last started or ended | [status.ts:142-143](../src/status.ts#L142-L143) |
| a failed send or update of the status | [status.ts:122](../src/status.ts#L122), [:131-134](../src/status.ts#L131-L134) |
| the stop | [status.ts:96-102](../src/status.ts#L96-L102), [serve.ts:185](../src/serve.ts#L185) |

The deletion is `apiDeleteChatItems` of the item with mode `broadcast`, sent only when the item was posted ([status.ts:78-86](../src/status.ts#L78-L86), [api.ts:1178-1213](../../simplex-chat-nodejs/src/api.ts#L1178-L1213)).

At start, before the HTTP listener, each eligible chat is read in pages of 100 items from the last one, until a page is short or starts with an item older than 30 h − 10 min; the agent's own live message items younger than that and not marked deleted are deleted with mode `broadcast` ([serve.ts:119](../src/serve.ts#L119), [tools.ts:1185-1190](../src/tools.ts#L1185-L1190), [status.ts:104-119](../src/status.ts#L104-L119)).

### 1.11 `/abort`

With `--abort-file FILE` ([3.1](#31-serve)), a message `/abort` in a chat whose status message is shown is counted in FILE for the script that runs the agent, and every status message is ended ([`onNewItems`](../src/tools.ts#L573-L586), [`abortAgent`](../src/tools.ts#L588-L599)).

Recognition at the `newChatItems` event, all of:

| Condition | Code |
|---|---|
| a received message item (`rcvMsgContent`), not live, in the main conversation of a contact or group chat; a member chat of a group is excluded | [tools.ts:580-581](../src/tools.ts#L580-L581), [messages.ts:135-137](../src/messages.ts#L135-L137) |
| `--abort-file` is set, and the status of the chat has 1 or more renders | [status.ts:88-90](../src/status.ts#L88-L90) |
| a text message whose text, without whitespace at both ends, is `/abort` | [tools.ts:582](../src/tools.ts#L582) |

Any sender qualifies: a contact, or any member of a group or business chat. During the first eligibility read of a status, at 0 renders, `/abort` is not offered. Other messages are handled as in [1.10](#110-status-message): a waiting status is queued in a typing chat ([tools.ts:583](../src/tools.ts#L583)).

Steps, in order:

| Step | Code |
|---|---|
| the item ID is added to `AgentTools.abortItems`, kept for the run | [tools.ts:500](../src/tools.ts#L500), [:589](../src/tools.ts#L589) |
| the item is marked read, without a wait; a failure is logged as `abort in CHAT: ERROR` | [tools.ts:590](../src/tools.ts#L590) |
| `Abort by SENDER in KIND NAME (CHAT).` on stdout ([3.4](#34-terminal-lines)) | [tools.ts:591-592](../src/tools.ts#L591-L592) |
| the count is incremented and written to FILE ([3.1](#31-serve)) | [tools.ts:594](../src/tools.ts#L594), [files.ts:65](../src/files.ts#L65) |
| every status is ended: one with 1 or more renders with the text `◼ aborted`, one with 0 renders with `""` | [tools.ts:595](../src/tools.ts#L595), [status.ts:92-94](../src/status.ts#L92-L94) |
| a failure of the two steps above: `abort in CHAT: ERROR` on stderr; the statuses are kept | [tools.ts:593-598](../src/tools.ts#L593-L598) |

- `onNewItems` returns the items whose ID is outside `abortItems`, and only these are delivered to the item watch: no `/abort` is collected as a response of a `send`, and a `receive` that waits is not woken by it ([serve.ts:145](../src/serve.ts#L145), [tools.ts:585](../src/tools.ts#L585)).
- The items of `abortItems` are left out of the unread items that `receive` reads, also an item read before it was marked read ([tools.ts:1204](../src/tools.ts#L1204)).
- The end with `◼ aborted` is a final update of the posted item, not live; for a status not yet posted, nothing is sent ([status.ts:78-86](../src/status.ts#L78-L86), [api.ts:1199-1213](../../simplex-chat-nodejs/src/api.ts#L1199-L1213)). A status message then gets at most 59 renders and the final text.
- After an abort, every status is ended, so `/abort` is offered in no chat until a status is rendered again; a second `/abort` of the same event is an ordinary message.
- At the start of a `receive` that waits, the status of its chat is ended ([1.4](#14-receive)); an `/abort` that arrives then is an ordinary message, which that `receive` returns, unless a new status was shown by a note before it.
- The count in memory is incremented before the write, also when the write fails ([files.ts:65](../src/files.ts#L65)).
- The endings are awaited at the stop, and the statuses ended by an abort are not counted as status deletions ([status.ts:96-102](../src/status.ts#L96-L102)).
- A fragment of the line that starts with `/` is parsed by the core as a command ([Markdown.hs:358](../../../src/Simplex/Chat/Markdown.hs#L358), [:443-449](../../../src/Simplex/Chat/Markdown.hs#L443-L449)). The apps make a command tappable only in a direct chat with a contact of peer type `bot` and in a group whose profile lists commands, when sending in the chat is ready, and at a tap they send the text message `/COMMAND` in the chat ([ChatModel.kt:1855-1859](../../../apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L1855-L1859), [FramedItemView.kt:517](../../../apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FramedItemView.kt#L517), [:533-544](../../../apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FramedItemView.kt#L533-L544), [ChatTypes.swift:2443-2449](../../../apps/ios/SimpleXChat/ChatTypes.swift#L2443-L2449), [MsgContentView.swift:140](../../../apps/ios/Shared/Views/Chat/ChatItem/MsgContentView.swift#L140)). The agent's profile has peer type `agent` ([serve.ts:97](../src/serve.ts#L97)), so in a direct chat with the agent, `/abort` is shown as text.

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

The server reads commands from members' messages in the main conversation of the control chat ([serve.ts:143](../src/serve.ts#L143), [control.ts:171-181](../src/control.ts#L171-L181)). It answers a command with a reply that quotes it, one command after another, and it runs the expiry check before each command ([control.ts:179](../src/control.ts#L179), [:201](../src/control.ts#L201)).

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
| `delete CHAT files` | the chat's downloads in memory fail and are cancelled; its other files in status `rcvAccepted`, `rcvTransfer` or `rcvWarning`, such as downloads the core continues after a restart, are cancelled; the folder of CHAT and its forwarded folder are deleted, for a current or a left chat | `Deleted K files from CHAT, SIZE.` (`1 file`), or `No files from CHAT.` | [control.ts:304-307](../src/control.ts#L304-L307), [tools.ts:974-992](../src/tools.ts#L974-L992), [messages.ts:271-279](../src/messages.ts#L271-L279) |
| `pending` | no pending action with a card | `No pending actions.` | [control.ts:208](../src/control.ts#L208) |
| `pending` | otherwise | one line per action: `N  WHAT  expires YYYY-MM-DD HH:MM UTC` | [control.ts:207](../src/control.ts#L207), [:618-620](../src/control.ts#L618-L620) |
| `list` | no chats | `No chats.` | [control.ts:325](../src/control.ts#L325) |
| `list` | otherwise | one line per chat of the 100 most recent: `CHAT  KIND NAME`, then `, approved for this chat by APPROVER`, `, files approved by APPROVER` and `, not connected` when applicable, cut to 100 bytes with `…`; then `K older chats are not shown.` (`1 older chat is not shown.`) | [control.ts:313-326](../src/control.ts#L313-L326) |

KIND is `contact`, `bot`, `agent`, `business`, `group` or `channel` ([tools.ts:1455-1462](../src/tools.ts#L1455-L1462)). WHAT is `connect to SUBJECT`, `send to SUBJECT`, `download NAME from SUBJECT`, `delete SUBJECT` or `leave SUBJECT` ([actions.ts:315-327](../src/actions.ts#L315-L327)). SIZE is in bytes, KB, MB or GB, by 1000, with one decimal ([control.ts:670-673](../src/control.ts#L670-L673)). Names are shown by `safeName` ([control.ts:687-690](../src/control.ts#L687-L690)).

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
| `sndFileError` of a sent file; logged too | ``The file `NAME` was not sent to KIND NAME (CHAT): `REASON`.`` | [control.ts:160-169](../src/control.ts#L160-L169), [serve.ts:166-168](../src/serve.ts#L166-L168) |
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

Binary: `simplex-mcp`, compiled `dist/cli.js` ([package.json:5-7](../package.json#L5-L7)). Help text: [cli.ts:10-45](../src/cli.ts#L10-L45).

```
simplex-mcp --help
simplex-mcp serve NAME [options]
simplex-mcp register NAME [options]
```

NAME: 1 to 32 characters of `[a-z0-9]` and single hyphens, starting with a letter or digit ([cli.ts:168-170](../src/cli.ts#L168-L170)).

### 3.1 `serve`

| Option | Value | Default | Rule | Code |
|---|---|---|---|---|
| `--data` | folder | `~/.simplex-mcp/NAME` | resolved to an absolute path; `dataGiven` is set | [cli.ts:76-79](../src/cli.ts#L76-L79) |
| `--display-name` | text | NAME when the user is created | must equal `validName(text)` | [cli.ts:80-82](../src/cli.ts#L80-L82), [:172-176](../src/cli.ts#L172-L176) |
| `--approve` | `requests` or `host` | `requests` | | [cli.ts:83-85](../src/cli.ts#L83-L85) |
| `--owners` | absent value, or `ID:NAME,...` | absent | the value is absent when the next argument is missing or starts with `--`; it is split at each comma followed by `ID:`, so a name may hold commas; entries match `^([1-9]\d*):(.+)$` | [cli.ts:86-88](../src/cli.ts#L86-L88), [:199-203](../src/cli.ts#L199-L203) |
| `--expire` | hours | 23 | integer 1 to 23 | [cli.ts:89-91](../src/cli.ts#L89-L91) |
| `--stop-wait` | seconds | 10 | integer 0 to 60 | [cli.ts:92-94](../src/cli.ts#L92-L94) |
| `--reset-owners` | flag | off | refused with an `--owners` list | [cli.ts:95-97](../src/cli.ts#L95-L97), [:117-119](../src/cli.ts#L117-L119) |
| `--new-token` | flag | off | | [cli.ts:98-100](../src/cli.ts#L98-L100) |
| `--port` | number | 5226 | integer 1 to 65535 | [cli.ts:101-103](../src/cli.ts#L101-L103) |
| `--qr-code` | `small` or `large` | `small` | | [cli.ts:104-106](../src/cli.ts#L104-L106) |
| `--inference` | URL | absent | an `http` or `https` URL without user name and password; kept as its origin and path, trailing slashes removed; refused with `--approve host` | [cli.ts:107-109](../src/cli.ts#L107-L109), [:120-122](../src/cli.ts#L120-L122), [:193-197](../src/cli.ts#L193-L197) |
| `--abort-file` | file | absent | resolved to an absolute path; refused with `--approve host`; refused when FILE is under the data folder, or the data folder is FILE's folder or under it, compared by the prefixes `join(PATH, sep)` without following symbolic links | [cli.ts:110-112](../src/cli.ts#L110-L112), [:123-125](../src/cli.ts#L123-L125) |

With `--inference`, `SIMPLEX_MCP_INFERENCE_KEY` is read before the data folder is prepared. A set key is accepted with 1 or more visible ASCII characters (0x21 to 0x7e) only, and it is sent as `Authorization: Bearer KEY` to URL only; the proxies of `/running` are polled without it ([serve.ts:71](../src/serve.ts#L71), [:216-220](../src/serve.ts#L216-L220), [inference.ts:66](../src/inference.ts#L66), [:111](../src/inference.ts#L111)). The polls are described in [Architecture 2.12](architecture.md#212-inferencets).

With `--abort-file`, the abort counter is created in `startServer` after the headers files, before `ChatApi.init`: FILE's first line is read, and a first line of 1 to 15 digits is kept as the count; otherwise, also for a missing FILE, `0` and a newline are written. At each `/abort` of [1.11](#111-abort), 1 is added to the count, and the count is written. FILE is written through `FILE.part`: the text is written to `FILE.part` with mode 0644, set again by `chmod` except on Windows, and `FILE.part` is renamed to FILE. At a missing folder, a folder at FILE or a refused write during the start, the lock is released, and the process exits with code 1 and `simplex-mcp: ERROR` ([serve.ts:77](../src/serve.ts#L77), [:89-91](../src/serve.ts#L89-L91), [files.ts:60-77](../src/files.ts#L60-L77), [cli.ts:242-243](../src/cli.ts#L242-L243)). The script that reads FILE is described in [Reference](../REFERENCE.md#abort).

### 3.2 `register`

| Option | Value | Default | Code |
|---|---|---|---|
| `--scope` | `user` or `local` | `user` | [cli.ts:133-135](../src/cli.ts#L133-L135) |
| `--data` | folder | `~/.simplex-mcp/NAME` | [cli.ts:136-138](../src/cli.ts#L136-L138) |

Entry ([`registryEntry`](../src/register.ts#L54-L61)):

| Field | Value |
|---|---|
| `type` | `http` |
| `url` | `http://127.0.0.1:PORT/mcp` |
| `headersHelper` | `cat DATA/headers.json`, Windows `type DATA\headers.json`, path shell-quoted |
| `timeout` | `(expireHours + 24) × 3600000 + 600000` ms |

Server name: `simplex-NAME` ([register.ts:38](../src/register.ts#L38)).

Status hooks, written after `claude mcp add-json` succeeds ([register.ts:46-51](../src/register.ts#L46-L51), [`statusHooks`](../src/register.ts#L63-L66)):

| Part | Value | Code |
|---|---|---|
| settings file, scope `user` | `CONFIG/settings.json`; CONFIG is `CLAUDE_CONFIG_DIR`, or `~/.claude` when it is unset or empty | [register.ts:35](../src/register.ts#L35), [:69](../src/register.ts#L69) |
| settings file, scope `local` | `PROJECT/.claude/settings.local.json`; PROJECT is the git repository root of the current folder, the main repository of a worktree, or the current folder outside a repository | [register.ts:70-71](../src/register.ts#L70-L71), [:107-131](../src/register.ts#L107-L131) |
| events | `UserPromptSubmit`<br>`PreToolUse`<br>`PostToolUse`<br>`PostToolUseFailure`<br>`PostToolBatch`<br>`PermissionRequest`<br>`PermissionDenied`<br>`SubagentStart`<br>`SubagentStop`<br>`PreCompact`<br>`PostCompact`<br>`Stop`<br>`StopFailure`<br>`SessionEnd` | [register.ts:10](../src/register.ts#L10) |
| matcher group | one per event, without a matcher, with one handler | [register.ts:65](../src/register.ts#L65) |
| handler | `type` `command`<br>`command` `curl`<br>`args` below<br>`async` `true`: Claude Code does not wait for it | [register.ts:64](../src/register.ts#L64) |
| `args` | `-q`: first, so the curlrc files are skipped<br>`--noproxy *`: the proxy variables ignored<br>`-s`: the progress meter and error messages suppressed<br>`-m 2`: at most 2 s<br>`-o /dev/null`, `-o NUL` on Windows: the answer discarded<br>`-H @DATA/hook-headers`: the `Authorization` header line of the file<br>`-H Content-Type: application/json`<br>`--data-binary @-`: a POST of the event JSON from standard input<br>`http://127.0.0.1:PORT/hook` | [register.ts:64](../src/register.ts#L64) |

- The settings file is parsed as JSON; an absent file is taken as `{}` ([register.ts:78-85](../src/register.ts#L78-L85)).
- In each event, every handler whose `args` contain `@DATA/hook-headers` is removed, a matcher group left without handlers is removed, and the new group is appended. Other keys, events, groups and handlers are kept, the hooks of agents with other data folders included ([register.ts:87-101](../src/register.ts#L87-L101)).
- The folder of the settings file is created when missing; the file is written with 2-space indentation and a final newline ([register.ts:49-50](../src/register.ts#L49-L50)).
- The settings file holds the path of `DATA/hook-headers`, not the token ([register.test.ts:84-92](../test/unit/register.test.ts#L84-L92)).

### 3.3 Exit codes and messages

| Code | Message | Code location |
|---|---|---|
| 0 | `--help` text on stdout; a stop after a signal | [cli.ts:226-228](../src/cli.ts#L226-L228), [serve.ts:58](../src/serve.ts#L58) |
| 2 | `Usage: simplex-mcp serve NAME [options] or simplex-mcp register NAME [options]. Run simplex-mcp --help.` | [cli.ts:55](../src/cli.ts#L55) |
| 2 | `NAME: 1 to 32 lowercase letters, digits and single hyphens, starting with a letter or digit.` | [cli.ts:56](../src/cli.ts#L56) |
| 2 | `OPTION: a value expected.` | [cli.ts:157](../src/cli.ts#L157) |
| 2 | `Unknown option OPTION. Run simplex-mcp --help.` | [cli.ts:216](../src/cli.ts#L216) |
| 2 | `--display-name: not a valid SimpleX name; try VALID` | [cli.ts:174](../src/cli.ts#L174) |
| 2 | `--approve: requests or host expected, got VALUE` | [cli.ts:179](../src/cli.ts#L179) |
| 2 | `--qr-code: small or large expected, got VALUE` | [cli.ts:184](../src/cli.ts#L184) |
| 2 | `--scope: user or local expected, got VALUE` | [cli.ts:189](../src/cli.ts#L189) |
| 2 | `--owners: ID:NAME expected, got ENTRY` | [cli.ts:201](../src/cli.ts#L201) |
| 2 | `--expire: hours from 1 to 23 expected, got VALUE` | [cli.ts:90](../src/cli.ts#L90), [:207](../src/cli.ts#L207) |
| 2 | `--stop-wait: seconds from 0 to 60 expected, got VALUE` | [cli.ts:93](../src/cli.ts#L93), [:207](../src/cli.ts#L207) |
| 2 | `--port: a port from 1 to 65535 expected, got VALUE` | [cli.ts:102](../src/cli.ts#L102), [:207](../src/cli.ts#L207) |
| 2 | `--reset-owners takes --owners without a list: member IDs change in a new agent control chat.` | [cli.ts:118](../src/cli.ts#L118) |
| 2 | `--inference: an http or https URL without user name and password expected, got VALUE` | [cli.ts:195](../src/cli.ts#L195) |
| 2 | `--inference: status messages are shown only in mode requests.` | [cli.ts:121](../src/cli.ts#L121) |
| 2 | `--abort-file: status messages are shown only in mode requests.` | [cli.ts:123](../src/cli.ts#L123) |
| 2 | `--abort-file: a file in a folder disjoint from the data folder expected, got FILE` | [cli.ts:124-125](../src/cli.ts#L124-L125) |
| 2 | `SIMPLEX_MCP_INFERENCE_KEY: visible ASCII characters expected.`, with `--inference`, for a set key, empty or with a character outside 0x21 to 0x7e, before the data folder is prepared | [serve.ts:71](../src/serve.ts#L71), [:218](../src/serve.ts#L218) |
| 2 | `--owners: member ENTRY is not in the agent control chat. Start with --reset-owners --owners, or use the IDs of current members.` | [control.ts:356](../src/control.ts#L356) |
| 1 | `Another server uses DATA (PID N). If no server runs, remove DATA/lock.` | [files.ts:21-23](../src/files.ts#L21-L23), [:79-81](../src/files.ts#L79-L81) |
| 1 | `Port PORT is in use. Start with --port N.` | [serve.ts:246](../src/serve.ts#L246) |
| 1 | `Agent "NAME" was never started. Start it with: npx -y @simplex-chat/mcp serve NAME` | [register.ts:37](../src/register.ts#L37) |
| 1 | `Add this entry under LOCATION in ~/.claude.json: {"simplex-NAME": ENTRY}` | [register.ts:74-76](../src/register.ts#L74-L76) |
| 1 | `claude mcp add-json failed: STDERR` | [register.ts:45](../src/register.ts#L45) |
| 1 | `Added to Claude Code: scope SCOPE, server simplex-NAME. FILE is not a valid settings file; add these status hooks to it: {"hooks":HOOKS}`, when FILE is not a JSON object, its `hooks` is other than an object or `null`, or the value of an event of [3.2](#32-register) is not an array; the entry is added, and FILE is left unchanged | [register.ts:47-48](../src/register.ts#L47-L48), [:87-90](../src/register.ts#L87-L90) |
| 1 | `simplex-mcp: ERROR` for other errors, `stop: ERROR` for a failed stop | [cli.ts:242-243](../src/cli.ts#L242-L243), [serve.ts:59-62](../src/serve.ts#L59-L62) |

LOCATION is `"mcpServers"` for scope `user`, and `"projects" > "PROJECT" > "mcpServers"` for scope `local` ([register.ts:69](../src/register.ts#L69), [:71](../src/register.ts#L71)).

### 3.4 Terminal lines

stdout ([log.ts:37-39](../src/log.ts#L37-L39)):

| Moment | Line | Code |
|---|---|---|
| start | `SimpleX MCP server VERSION, agent "NAME"`<br>`Data folder: DATA`<br>`Approval mode: MODE`<br>`Inference endpoint: URL` with `--inference` | [serve.ts:79-82](../src/serve.ts#L79-L82) |
| control chat created | `Agent control chat "DISPLAY" created.` | [control.ts:518](../src/control.ts#L518) |
| link opened | `Join the agent control chat in the SimpleX app. The link works once:`<br>QR code<br>link<br>small QR code: `If the QR code does not scan, restart with --qr-code large, or paste the link into the app.`<br>role line | [control.ts:406-416](../src/control.ts#L406-L416) |
| listening | `Listening on http://127.0.0.1:PORT/mcp` | [serve.ts:135](../src/serve.ts#L135) |
| owner setup done | `Add the agent to Claude Code:`<br>`  npx -y @simplex-chat/mcp register NAME [--data DATA]` | [serve.ts:194-196](../src/serve.ts#L194-L196) |
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
| `send` with `live: true` shown | `Note to KIND NAME (CHAT): TEXT`, or `an unnamed KIND`; the name and the text folded to one line | [tools.ts:847](../src/tools.ts#L847), [actions.ts:310-313](../src/actions.ts#L310-L313) |
| `/abort` recognized ([1.11](#111-abort)) | `Abort by SENDER in KIND NAME (CHAT).`, or `an unnamed KIND`; the names folded to one line | [tools.ts:591-592](../src/tools.ts#L591-L592), [actions.ts:310-313](../src/actions.ts#L310-L313) |
| `/leave CHAT` | `Left KIND NAME (CHAT), requested by NAME.` or `Deleted ...` | [control.ts:289](../src/control.ts#L289) |
| refused file, file not sent | as in [2.3](#23-other-posts), names and paths on one line | [control.ts:156](../src/control.ts#L156), [:167](../src/control.ts#L167) |
| stop | `Cancelled K action.` or `Cancelled K actions.` when K > 0<br>`Stopping: sending PARTS, up to S s. Ctrl-C to stop now.` when PARTS is not empty and S > 0; PARTS is `K cancelled card(s)` when cards were edited or posted and the control chat is not lost, and `M status deletion(s)` when status messages had a render, joined by ` and `<br>`Stopped.` | [serve.ts:184-186](../src/serve.ts#L184-L186), [:198-214](../src/serve.ts#L198-L214), [:189](../src/serve.ts#L189) |
| membership messages | see [2.4](#24-membership-messages) | |
| `register` done | `Added to Claude Code: scope SCOPE, server simplex-NAME, status hooks in FILE.` | [register.ts:51](../src/register.ts#L51), [cli.ts:232](../src/cli.ts#L232) |

stderr: `CONTEXT: ERROR` from `logError` ([log.ts:41-43](../src/log.ts#L41-L43)), with CONTEXT one of `action N`, `action N card`, `action N card image`, `action N file`, `control chat command`, `EVENT in the agent control chat`, `EVENT event`, `refused file`, `file not sent`, `download of file N`, `file N`, `moving file N`, `cancelling file N`, `resizing PATH`, `preview`, `MCP`, `HTTP request`, `stop`, `simplex-mcp`, `status of CHAT`, `inference endpoint`, `status messages`, `abort in CHAT`. The library's download lines are written to stderr.

| CONTEXT | Error | Code |
|---|---|---|
| `status of CHAT` | a failed send, update or deletion of a status message; a failed eligibility read of a current status; a failed tick; a failed leftover deletion | [status.ts:55](../src/status.ts#L55), [:117](../src/status.ts#L117), [:131-134](../src/status.ts#L131-L134), [:137-138](../src/status.ts#L137-L138), [:161](../src/status.ts#L161) |
| `inference endpoint` | the first failed poll after the start or after a successful poll | [status.ts:177-179](../src/status.ts#L177-L179) |
| `status messages` | a failed chat list before the leftover deletion | [serve.ts:119](../src/serve.ts#L119) |
| `abort in CHAT` | a failed mark-read of an `/abort` item; a failed write of the abort count ([1.11](#111-abort)) | [tools.ts:590](../src/tools.ts#L590), [:596-597](../src/tools.ts#L596-L597) |

A poll error is `GET URL: REASON`, folded to one line, with REASON `HTTP N`, `invalid JSON`, `body over 1 MiB`, `no answer within 1 s`, or the message of the first error aggregated in the error's cause, of the cause, or of the error itself, in this order ([inference.ts:109-141](../src/inference.ts#L109-L141)).

---

## 4. Hook endpoint

### 4.1 Request

Claude Code runs the status hooks of [3.2](#32-register) at each event of a session that reads their settings file, and curl posts the event's JSON input to `/hook` ([http.ts:26-30](../src/http.ts#L26-L30), [`receiveHook`](../src/http.ts#L63-L72)).

Checks in order:

| Check | Rule | Failure | Code |
|---|---|---|---|
| Host, Origin | as for `/mcp` ([Architecture 6.1](architecture.md#61-http)) | 403 | [http.ts:24](../src/http.ts#L24) |
| token | `Authorization` equal to `Bearer <token>` | 401 | [http.ts:25](../src/http.ts#L25) |
| method | `POST` | 405, `Allow: POST` | [http.ts:64](../src/http.ts#L64) |
| `Content-Length` | a number | 411 | [http.ts:65-66](../src/http.ts#L65-L66) |
| body size | `Content-Length` at most 4 MiB | 413 | [http.ts:16](../src/http.ts#L16), [:67](../src/http.ts#L67) |
| body | JSON | 400 | [http.ts:68-69](../src/http.ts#L68-L69) |

- After the checks, the event is recorded, and the request is answered with 204 and an empty body ([http.ts:70-71](../src/http.ts#L70-L71), [serve.ts:126](../src/serve.ts#L126)).
- `/hook` is answered before the readiness check of `/mcp`, so events are recorded from the moment the listener is bound ([http.ts:26-32](../src/http.ts#L26-L32)).
- After an exception in the handling of the request, the connection is destroyed ([http.ts:28](../src/http.ts#L28)).

### 4.2 Events

A `session_id` is taken as an agent session from its first event whose `tool_name` starts with `mcp__simplex-NAME__`, the prefix of this server's tools in Claude Code. Events of other sessions, and events without `session_id`, are discarded. At most 16 agent sessions are kept; with a 17th, the earliest one is removed with its state ([activity.ts:39-42](../src/activity.ts#L39-L42), [:87-93](../src/activity.ts#L87-L93)).

Fields read; other fields of the body are discarded ([`record`](../src/activity.ts#L34-L81), [`fileName`](../src/activity.ts#L103-L106)):

| Field | Kept when |
|---|---|
| `hook_event_name`<br>`session_id`<br>`tool_use_id`<br>`agent_id` | a string |
| `tool_name`<br>`agent_type` | it matches `^[\w-]{1,64}$` |
| `tool_input.file_path`, or `tool_input.notebook_path` when `file_path` is not a string | its base name matches `^[\w.+@-]{1,64}$`; only the base name is kept |

An event without `agent_id` is taken as an event of the main agent.

Effects on the agent session, by `hook_event_name` ([activity.ts:43-80](../src/activity.ts#L43-L80)):

| Event | Effect |
|---|---|
| `PreToolUse` | without `agent_id`: the compaction flag is cleared<br>a running tool is recorded by `tool_use_id`, with `agent_id`, the tool name and the file name; skipped without `tool_use_id`, for a tool of this server, for `Agent`, and for an ID kept as ended |
| `PostToolUse`<br>`PostToolUseFailure`<br>`PermissionDenied` | the permission flag is cleared<br>the tool of `tool_use_id` is removed; an ID without a tool is kept as ended |
| `PostToolBatch` | the permission flag is cleared<br>the running tools with the same `agent_id` are removed |
| `PermissionRequest` | the permission flag is set |
| `SubagentStart` | a running subagent is recorded by `agent_id`, with `agent_type` |
| `SubagentStop` | the subagent of `agent_id` is removed |
| `PreCompact` | the compaction flag is set |
| `PostCompact` | the compaction flag is cleared |
| `UserPromptSubmit`<br>`Stop`<br>`StopFailure`<br>`SessionEnd` | the running tools and subagents of the session are removed, and both flags are cleared ([activity.ts:95-100](../src/activity.ts#L95-L100)) |

- Running tools and subagents are kept up to 64 each, and ended IDs up to 256; beyond a limit, the earliest entry is removed ([activity.ts:18-20](../src/activity.ts#L18-L20), [:108-111](../src/activity.ts#L108-L111)).
- Claude Code runs each hook in a separate process, so the posts of one session may be received out of order; for an end event received before its `PreToolUse`, the ID is kept as ended, and the late `PreToolUse` is skipped ([activity.ts:46](../src/activity.ts#L46), [:53-55](../src/activity.ts#L53-L55)).
- Labels are chosen from the state of all agent sessions together: the permission flag and the compaction flag of any of them, the running tool recorded last and the subagent recorded last ([`state`](../src/activity.ts#L83-L85), [1.10](#110-status-message)).
