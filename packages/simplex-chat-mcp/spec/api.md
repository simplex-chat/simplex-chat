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
| Tools are registered on each per-request `McpServer` with description, input schema, output schema and, in mode `host`, `_meta: {"anthropic/requiresUserInteraction": true}` for `connect`, `send`, `download` and `leave` | [tools.ts:446-453](../src/tools.ts#L446-L453), [:481-491](../src/tools.ts#L481-L491) |
| Tool descriptions and server instructions; the instructions name the agent's display name | [tools.ts:398-436](../src/tools.ts#L398-L436), [serve.ts:112](../src/serve.ts#L112) |
| Arguments are validated by the SDK; a failure is a result with `isError: true` and the text `Input validation error: Invalid arguments for tool NAME: ERRORS` | [mcp-BumlNux0.cjs:1812-1817](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L1812-L1817) |
| `additionalProperties: false` is set in every input schema | [tools.ts:244](../src/tools.ts#L244), [:261](../src/tools.ts#L261), [:272](../src/tools.ts#L272), [:275](../src/tools.ts#L275), [:283](../src/tools.ts#L283), [:294](../src/tools.ts#L294) |
| A result is sent as `structuredContent` and as its JSON in one text item | [tools.ts:496](../src/tools.ts#L496) |
| A tool error is sent as `isError: true` with one text item: the `ToolError` text, or the `errorText` of other errors | [tools.ts:498](../src/tools.ts#L498), [log.ts:45-49](../src/log.ts#L45-L49) |
| An aborted call ends with the result `Cancelled.`, `isError: true`, which the SDK discards | [tools.ts:455](../src/tools.ts#L455), [:489](../src/tools.ts#L489) |
| With a `progressToken` in `_meta`, `notifications/progress` is sent at the start of each wait and every 60 s, with `progress` 0, 1, 2, ... and a `message` | [tools.ts:1515-1525](../src/tools.ts#L1515-L1525), [messages.ts:283-287](../src/messages.ts#L283-L287) |
| The call is aborted when the client closes the response stream or sends `notifications/cancelled`; the action is kept | [src-BhnGYybN.cjs:6338-6341](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs#L6338-L6341), [:6425](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs#L6425) |
| A call with the same tool and action fields as an action in memory is attached to it | [tools.ts:1507-1509](../src/tools.ts#L1507-L1509), [actions.ts:151-153](../src/actions.ts#L151-L153) |
| Messages are marked read after the last wait of a call that is not aborted | [tools.ts:641](../src/tools.ts#L641), [:786](../src/tools.ts#L786), [:1101](../src/tools.ts#L1101) |

Action fields of the key:

| Tool | Fields |
|---|---|
| `connect` | `target`<br>`incognito` (default `false`)<br>`message` (default `null`) |
| `send` | `chat`<br>`text` (default `null`)<br>`file` (default `null`)<br>`as` (default `null`)<br>`reply_to` (default `null`)<br>`mention` (default `null`) |
| `download` | `chat`<br>`item` |
| `leave` | `chat` |

Result status of the four action tools ([`actionResult`](../src/tools.ts#L610-L622)):

| Status | Meaning | Extra fields |
|---|---|---|
| `done` | performed | per tool |
| `failed` | failure of the card post, the counter write or the perform, or loss of the control chat | `error` |
| `rejected` | an approver sent `/reject` | |
| `expired` | no decision before the expiry time | |
| `known` | `connect` only: the target is an existing chat past the prepared state; no action | chat fields |
| `performing` | `download` only: `wait` passed after the approval; the download continues | |

### 1.2 `connect`

Handler: [`AgentTools.connect`](../src/tools.ts#L591-L604). Perform: [`performConnect`](../src/tools.ts#L660-L691). Schemas: [input](../src/tools.ts#L234-L245), [output](../src/tools.ts#L300-L310).

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
| `known` | `status`, chat fields of the known chat | [tools.ts:596](../src/tools.ts#L596) |
| `done`, chat unknown | `action`, `status`, `type`, `connected: false`, plan fields `name`, `peer_type`, `description`, `welcome`, `commands`, `known_as` when present | [tools.ts:607](../src/tools.ts#L607), [:1307-1313](../src/tools.ts#L1307-L1313) |
| `done`, chat known | `action`, `status`, chat fields, `messages` | [tools.ts:624-644](../src/tools.ts#L624-L644) |
| `failed` | `action`, `status`, `error` | [tools.ts:615](../src/tools.ts#L615) |
| `rejected`, `expired` | `action`, `status` | [tools.ts:615](../src/tools.ts#L615) |

Chat fields ([`chatFields`](../src/tools.ts#L1163-L1190)):

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

`messages`: message items among the last 50 items of the chat when the result is built, replaced by their collected copies, followed by the items collected since the connection; live items are left out; new items are marked read ([tools.ts:626](../src/tools.ts#L626), [:636-643](../src/tools.ts#L636-L643)).

Tool errors:

| Text | Condition | Code |
|---|---|---|
| `Not a SimpleX link or name: TARGET` | whitespace in `target`, or `invalidConnReq` from `apiConnectPlan` | [tools.ts:648](../src/tools.ts#L648), [:654](../src/tools.ts#L654) |
| an app error text, for example `Name not found: This SimpleX name is not registered. Please check the name.`, or `SimpleX error: CHAIN: DETAILS` | other errors of `apiConnectPlan`, or a plan of type `error` | [tools.ts:655](../src/tools.ts#L655), [:1234](../src/tools.ts#L1234), [log.ts:51-53](../src/log.ts#L51-L53) |
| `TARGET can't be joined now: PLAN.` | `invitationLink` other than `ok` and `known`<br>`contactAddress` other than `ok`, `known`, `contactViaAddress` and `connectingConfirmReconnect`<br>`groupLink` other than `ok`, `known` and `connectingConfirmReconnect` | [tools.ts:1225](../src/tools.ts#L1225), [:1238](../src/tools.ts#L1238), [:1256](../src/tools.ts#L1256), [:1278](../src/tools.ts#L1278) |
| `message: TARGET is not a contact or business address with short link data, or its contact exists without a connection.` | `message` given and the plan has `takesMessage: false` | [tools.ts:598](../src/tools.ts#L598) |
| refusals | see [1.9](#19-action-refusals) | |

`takesMessage` is `true` for a contact address with short link data, for a known prepared contact, and for a known prepared business group ([tools.ts:1229](../src/tools.ts#L1229), [:1249](../src/tools.ts#L1249), [:1267](../src/tools.ts#L1267)).

### 1.3 `send`

Handler: [`AgentTools.send`](../src/tools.ts#L748-L774). Result: [`sendResult`](../src/tools.ts#L776-L794). Perform: [`performSend`](../src/tools.ts#L796-L820). Schemas: [input](../src/tools.ts#L247-L262), [output](../src/tools.ts#L312-L324).

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

File types by extension, lower-cased ([tools.ts:162-171](../src/tools.ts#L162-L171), [`attachmentMedia`](../src/tools.ts#L1465-L1476)):

| Extension | Message type | Sent file | Preview and duration |
|---|---|---|---|
| `.jpg`, `.jpeg`, `.png` | image | `core.resizeImage(..., Image)`, written to `DATA/files/sent/`; the original when resizing fails | `core.resizeImage(..., MessagePreview)`; the fixed preview when it fails |
| `.gif`, `.webp` | image | unchanged | the fixed preview |
| `.mp4`, `.mov` | video | unchanged | the first frame from `ffmpeg`, resized as a preview; duration from `ffprobe`, rounded; the fixed preview and 0 when they fail |
| other, or `as: "file"` | file | unchanged | |

The fixed preview is a 16 × 9 mid-gray PNG ([tools.ts:153](../src/tools.ts#L153)). `ffmpeg` and `ffprobe` run with `-protocol_whitelist file -f mov`, 30 s and 64 MB of output each ([tools.ts:1487-1505](../src/tools.ts#L1487-L1505)).

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
| `text: required without file.` | neither `text` nor `file` | [tools.ts:753](../src/tools.ts#L753) |
| `as: applies only with file.` | `as` without `file` | [tools.ts:754](../src/tools.ts#L754) |
| `No chat CHAT. list shows the agent's chats.` | invalid reference, the control chat, or a store error when the chat is read | [tools.ts:1129-1136](../src/tools.ts#L1129-L1136), [:1415-1417](../src/tools.ts#L1415-L1417) |
| `The agent is not connected to CHAT and can't post; list shows connected: false.` | the chat is not connected | [tools.ts:756](../src/tools.ts#L756) |
| `The agent has role ROLE in CHAT and can't post.` | membership role `observer` or `relay` | [tools.ts:757-760](../src/tools.ts#L757-L760) |
| `reply_to: CHAT has no item N.` | the read of `around=N` fails, or the item is absent from it | [tools.ts:1141](../src/tools.ts#L1141) |
| `reply_to: item N in CHAT is not a message.` | the item's content is not `rcvMsgContent` or `sndMsgContent` | [tools.ts:1143](../src/tools.ts#L1143) |
| `mention: CHAT is a contact chat; mentions apply to groups and channels.` | `mention` with `@N` | [tools.ts:1149](../src/tools.ts#L1149) |
| `mention: CHAT has no member N.` | ID absent from `apiListMembers` | [tools.ts:1154](../src/tools.ts#L1154) |
| `mention: members N and M have the same name.` | two mentioned members with the same display name | [tools.ts:1157](../src/tools.ts#L1157) |
| `file: an absolute path is expected.`<br>`file: no file at FILE.`<br>`file: PATH is not a file.`<br>`file: PATH can't be read.` | the path, after symbolic links | [tools.ts:1424-1435](../src/tools.ts#L1424-L1435) |
| `file: PATH is refused: private keys and the server's own files are not sent.` | a file named `id_*`, `*.pem`, `*.key`, `*.p12` or `*.pfx`, or under `~/.ssh`, `~/.gnupg`, or the data folder or `~/.simplex-mcp` outside `DATA/files/`; each folder by its real path | [tools.ts:836-839](../src/tools.ts#L836-L839), [:845-850](../src/tools.ts#L845-L850) |
| `file: PATH is empty.` | size 0 | [tools.ts:840](../src/tools.ts#L840) |
| `text: too long for the message with its quote; shorten it.`<br>`text: too long for a message with this TYPE; shorten it.` | the message, as the core sends it, above 15602 bytes | [tools.ts:764-766](../src/tools.ts#L764-L766), [:1392-1408](../src/tools.ts#L1392-L1408) |
| refusals | see [1.9](#19-action-refusals) | |

The message is measured as 600 bytes plus the JSON of its content, the file's name, the quote and the mentioned names. The quote is the quoted text, or its file's name when the text is empty; a sent image or video quotes it as text, otherwise an image's preview is kept ([tools.ts:1392-1408](../src/tools.ts#L1392-L1408)).

The perform fails with `file: PATH changed after the request.` when the file's real path, size, inode or change time differs from the request ([tools.ts:852-857](../src/tools.ts#L852-L857)). A resized copy is deleted at `sndFileCompleteXFTP`, at `sndFileError` and after a failed send ([tools.ts:817](../src/tools.ts#L817), [:822-832](../src/tools.ts#L822-L832), [serve.ts:156-160](../src/serve.ts#L156-L160)).

### 1.4 `receive`

Handler: [`AgentTools.receive`](../src/tools.ts#L1073-L1107). Schemas: [input](../src/tools.ts#L264-L273), [output](../src/tools.ts#L326-L334).

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

Tool errors: `No chat CHAT. list shows the agent's chats.` for an invalid reference, the control chat, or a store error of the first read, `initial` 1 ([tools.ts:1075](../src/tools.ts#L1075), [:1079-1081](../src/tools.ts#L1079-L1081)); other chat errors, a store error of a later page read included, as their error text ([log.ts:45-53](../src/log.ts#L45-L53)).

Messages are read by `receive` without an action or an approval.

### 1.5 `download`

Handler: [`AgentTools.download`](../src/tools.ts#L924-L945). Result: [`downloadResult`](../src/tools.ts#L972-L980). Perform: [`performDownload`](../src/tools.ts#L982-L986), [`downloadFile`](../src/tools.ts#L515-L523). Schemas: [input](../src/tools.ts#L286-L295), [output](../src/tools.ts#L381-L396).

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

FOLDER is `group-N` for `#N` and `contact-N` for `@N`; the extension is the file name's extension when it has 1 to 16 ASCII letters and digits ([messages.ts:208-215](../src/messages.ts#L208-L215)).

Steps before the action:

| Step | Code |
|---|---|
| an XFTP file: the file's servers from `apiGetChatItemInfo`; while none, the call waits for `rcvFileDescrReady` until `wait` seconds after the call, with the progress `The sender is uploading the file of item N in CHAT; waiting until TIME.` | [tools.ts:947-964](../src/tools.ts#L947-L964) |
| unknown servers: the file's servers that are not among the XFTP servers of `apiGetUserServers`, of no operator or of an enabled one, compared without their passwords | [tools.ts:966-970](../src/tools.ts#L966-L970) |
| without unknown servers, the chat's files approval, read again, makes the action performed at once | [tools.ts:939](../src/tools.ts#L939) |

Perform: `apiReceiveFile(fileId, path, approvedRelays)`, `approvedRelays` true in mode `requests` and false in mode `host`, for a file in status `rcvInvitation` ([tools.ts:988-994](../src/tools.ts#L988-L994)). The download ends at `rcvFileComplete`, `rcvFileError`, `rcvFileSndCancelled` or `chatItemUpdated` of its file, or at a check every 60 s ([tools.ts:535-557](../src/tools.ts#L535-L557), [:996-1062](../src/tools.ts#L996-L1062)).

Failure texts of the perform:

| Text | Condition | Code |
|---|---|---|
| the file's error, as in the file field | the file failed | [tools.ts:1023-1033](../src/tools.ts#L1023-L1033) |
| `The sender cancelled the file.` | `rcvFileSndCancelled` | [tools.ts:552-557](../src/tools.ts#L552-L557) |
| `The message or its chat was deleted.` | a check finds no message, contact or group | [tools.ts:157](../src/tools.ts#L157), [:1013-1021](../src/tools.ts#L1013-L1021) |
| `The download made no progress for 30 minutes and was cancelled.` | no rise of `rcvProgress` for 30 minutes; the file is cancelled | [tools.ts:160](../src/tools.ts#L160), [:1006-1010](../src/tools.ts#L1006-L1010) |
| `The received file is not in the agent's files folder.` | the file is received and the download path has no file | [messages.ts:13](../src/messages.ts#L13), [tools.ts:1035-1039](../src/tools.ts#L1035-L1039) |
| `Your owner deleted the files of this chat.` | `/delete CHAT files` | [tools.ts:158](../src/tools.ts#L158), [:885-894](../src/tools.ts#L885-L894) |

Tool errors:

| Text | Condition | Code |
|---|---|---|
| `No chat CHAT. list shows the agent's chats.` | invalid reference, the control chat, or a store error when the chat is read | [tools.ts:929](../src/tools.ts#L929) |
| `item: CHAT has no item N.` | the item is not found | [tools.ts:931](../src/tools.ts#L931) |
| `item: item N in CHAT has no received file.` | the item is not received or has no file | [tools.ts:933](../src/tools.ts#L933) |
| `item: the file of item N in CHAT failed: ERROR` | the file field has status `failed` | [tools.ts:934-935](../src/tools.ts#L934-L935) |
| `item: the sender has not uploaded the file of item N in CHAT yet; call download again later.` | no servers by the end of `wait` | [tools.ts:962](../src/tools.ts#L962) |
| refusals | see [1.9](#19-action-refusals) | |

A file ID whose download returned `rejected` or `expired` shows status `rejected` until a download of it is done or the server restarts ([tools.ts:977-978](../src/tools.ts#L977-L978)).

### 1.6 `list`

Handler: [`AgentTools.listChats`](../src/tools.ts#L1109-L1117). Schemas: [input](../src/tools.ts#L275), [output](../src/tools.ts#L336-L368).

Input: an empty object.

Output:

| Field | Item fields |
|---|---|
| `chats` | `chat`<br>`type`<br>`name`<br>`simplex_name`, when verified<br>`peer_type`, when present<br>`incognito`<br>`known_as`, when incognito<br>`connected`<br>`unread`: `chatStats.unreadCount`<br>`approved: true`, only under a standing approval of messages in mode `requests` |
| `actions` | `action`<br>`tool`<br>`input`: the input of the creating call<br>`state`<br>`open`: `true` while a call waits on it |

`chats`: the contacts and groups of the main conversations from one read, `apiGetChats(USER, last 1000000)`, except the control chat ([tools.ts:1119-1127](../src/tools.ts#L1119-L1127)). `actions`: every action in memory ([tools.ts:1115](../src/tools.ts#L1115)).

### 1.7 `leave`

Handler: [`AgentTools.leave`](../src/tools.ts#L902-L911). Perform: [`leaveChat`](../src/tools.ts#L917-L922). Schemas: [input](../src/tools.ts#L277-L284), [output](../src/tools.ts#L370-L379).

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

Tool errors: `No chat CHAT. list shows the agent's chats.` when no action with the same key is in memory and the chat is invalid, the control chat or unknown ([tools.ts:905-907](../src/tools.ts#L905-L907)); refusals.

### 1.8 Message, file and command objects

Message ([`Message`](../src/messages.ts#L15-L27), [`toMessage`](../src/messages.ts#L171-L189), [schema](../src/tools.ts#L173-L206)):

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

File ([`messageFile`](../src/messages.ts#L191-L206)):

| Field | Value |
|---|---|
| `name` | `fileName` |
| `size` | `fileSize`, bytes |
| `type` | `image`, `video` or `voice` from the message type; `file` otherwise |
| `status` | first match:<br>`rejected`: the file ID is in rejected files<br>`failed`: `fileProhibited` is set, the status is `rcvCancelled`, `rcvAborted`, `rcvError` or `invalid`, or the status is `rcvComplete` and the download path has no file<br>`received`: `rcvComplete` with `fileSource` at the download path<br>`offered`: otherwise |
| `path` | `received`: the download path |
| `error` | `failed`: the error text ([log.ts:197-244](../src/log.ts#L197-L244)) |

Only received message items (`rcvMsgContent`) are returned by `send` and `receive`; `connect` also returns sent ones (`sndMsgContent`) ([messages.ts:98](../src/messages.ts#L98), [tools.ts:636](../src/tools.ts#L636)).

Command ([`commandsOf`](../src/tools.ts#L1374-L1381), [schema](../src/tools.ts#L208-L218)):

| Form | Fields |
|---|---|
| command | `keyword`<br>`label`<br>`params`, when present |
| menu | `label`<br>`commands`: commands and menus |

### 1.9 Action refusals

Refusals of a new `connect`, `send`, `download` or `leave` action, first match ([actions.ts:172-177](../src/actions.ts#L172-L177), [control.ts:118-127](../src/control.ts#L118-L127)):

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

Largest card form ([control.ts:615-623](../src/control.ts#L615-L623)): the maximum of the creation form, the `done` form and a `failed` form with an error of 600 × `€`, with the action's own decider or the longest current approver name; each form is measured as JSON-escaped UTF-8: `"`, `\`, `\n`, `\r`, `\t` 2 bytes, other characters below U+0020 6 bytes.

Link texts of a `connect` card ([control.ts:121](../src/control.ts#L121), [:637-651](../src/control.ts#L637-L651)): the description and welcome lines take the bytes that the largest form leaves without them. Lines are kept while they fit; the next line is cut to the bytes left and ends in `…`; later lines are left out. The tool result keeps both texts whole.

---

## 2. Agent control chat

### 2.1 Commands and replies

Accepted text ([`parseCommand`](../src/control.ts#L538-L543), [`ownerRequest`](../src/control.ts#L703-L716)):

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
  approve CHAT all
  revoke CHAT
  approve CHAT files
  revoke CHAT files
  leave CHAT
  delete CHAT files
  pending
  list
```

The server reads commands from members' messages in the main conversation of the control chat ([serve.ts:134](../src/serve.ts#L134), [control.ts:170-180](../src/control.ts#L170-L180)). It answers a command with a reply that quotes it, one command after another, and it runs the expiry check before each command ([control.ts:178](../src/control.ts#L178), [:200](../src/control.ts#L200)).

| Request | Condition | Reply | Code |
|---|---|---|---|
| one-line `/` text outside the requests | any sender | `Commands: /approve, /reject, /receive, /revoke, /leave, /delete, /pending, /list. Each card shows its commands.` | [control.ts:16](../src/control.ts#L16), [:202](../src/control.ts#L202) |
| any request | sender not an approver, `--owners` absent | `Only owners approve actions.` | [control.ts:203](../src/control.ts#L203) |
| any request | sender not an approver, `--owners` given | `Only the owners listed in --owners approve actions.` | [control.ts:203](../src/control.ts#L203) |
| `approve`, `reject`, `receive` | action N absent, not `pending`, or without a card | `Action N is not pending.` | [control.ts:224](../src/control.ts#L224) |
| `approve CHAT N`, `reject CHAT N`, `receive CHAT N` | action N is a `connect`, or for another chat | `Action N is not for CHAT.` | [control.ts:225](../src/control.ts#L225) |
| `receive` | action N is not a download | `Action N has no file.` | [control.ts:230](../src/control.ts#L230) |
| `approve`, `reject`, `receive` | accepted | none; the card is edited at the outcome; `receive` posts the file as a reply to the card, which stays pending | [control.ts:226-231](../src/control.ts#L226-L231), [:262-274](../src/control.ts#L262-L274) |
| `approve CHAT all`, `approve CHAT files` | mode `host`; nothing is stored | `Approvals apply in mode requests; this server runs in mode host.` | [control.ts:212](../src/control.ts#L212) |
| `approve CHAT all`, `approve CHAT files` | CHAT unknown or the control chat | `No chat CHAT. /list shows the agent's chats.` | [control.ts:239](../src/control.ts#L239) |
| `approve CHAT all` | accepted; the pending sends of CHAT with a card are approved | `Approved for KIND NAME (CHAT) by APPROVER. /'revoke CHAT'` | [control.ts:236-251](../src/control.ts#L236-L251) |
| `approve CHAT files` | accepted; the pending downloads of CHAT with a card are approved, except those from unknown servers | `Files approved for KIND NAME (CHAT) by APPROVER. /'revoke CHAT files'`, with `, except downloads from unknown servers` after APPROVER while such downloads stay pending | [control.ts:244-250](../src/control.ts#L244-L250) |
| `revoke CHAT` | CHAT unknown, or without a standing approval | `Messages to CHAT have no standing approval.` | [control.ts:256](../src/control.ts#L256) |
| `revoke CHAT` | accepted | `Messages to KIND NAME (CHAT) need approval again.` | [control.ts:259](../src/control.ts#L259) |
| `revoke CHAT files` | CHAT unknown, or without a files approval | `Files from CHAT have no standing approval.` | [control.ts:256](../src/control.ts#L256) |
| `revoke CHAT files` | accepted | `Files from KIND NAME (CHAT) need approval again.` | [control.ts:259](../src/control.ts#L259) |
| `leave CHAT` | CHAT unknown or the control chat | `No chat CHAT. /list shows the agent's chats.` | [control.ts:278](../src/control.ts#L278) |
| `leave CHAT` | accepted, at once in both modes; the pending actions of CHAT with a card are rejected by the sender | `Left KIND NAME (CHAT), requested by NAME.`, or `Deleted ...` for a contact; logged too | [control.ts:276-290](../src/control.ts#L276-L290) |
| `delete CHAT files` | the chat's downloads in memory fail and are cancelled; the folder of CHAT is deleted, for a current or a left chat | `Deleted K files from CHAT, SIZE.` (`1 file`), or `No files from CHAT.` | [control.ts:297-300](../src/control.ts#L297-L300), [tools.ts:885-894](../src/tools.ts#L885-L894) |
| `pending` | no pending action with a card | `No pending actions.` | [control.ts:207](../src/control.ts#L207) |
| `pending` | otherwise | one line per action: `N  WHAT  expires YYYY-MM-DD HH:MM UTC` | [control.ts:206](../src/control.ts#L206), [:611-613](../src/control.ts#L611-L613) |
| `list` | no chats | `No chats.` | [control.ts:318](../src/control.ts#L318) |
| `list` | otherwise | one line per chat of the 100 most recent: `CHAT  KIND NAME`, then `, approved for this chat by APPROVER`, `, files approved by APPROVER` and `, not connected` when applicable, cut to 100 bytes with `…`; then `K older chats are not shown.` (`1 older chat is not shown.`) | [control.ts:306-319](../src/control.ts#L306-L319) |

KIND is `contact`, `bot`, `agent`, `business`, `group` or `channel` ([tools.ts:1332-1339](../src/tools.ts#L1332-L1339)). WHAT is `connect to SUBJECT`, `send to SUBJECT`, `download NAME from SUBJECT`, `delete SUBJECT` or `leave SUBJECT` ([actions.ts:315-327](../src/actions.ts#L315-L327)). SIZE is in bytes, KB, MB or GB, by 1000, with one decimal ([control.ts:663-666](../src/control.ts#L663-L666)). Names are shown by `safeName` ([control.ts:680-683](../src/control.ts#L680-L683)).

A standing approval is stored in the chat's custom data, `simplexMcp` for messages and `simplexMcpFiles` for files, as `{approvedBy, approvedById, approvedAt}`; it applies in mode `requests` while member `approvedById` is an approver ([control.ts:22-25](../src/control.ts#L22-L25), [:129-132](../src/control.ts#L129-L132)).

### 2.2 Cards

Card text: request lines, then one last line ([`cardText`](../src/control.ts#L545-L564), [`requestLines`](../src/control.ts#L566-L594)).

Request lines:

| Tool | First line | Further lines |
|---|---|---|
| `connect` | `I want to connect to SUBJECT, as myself.` or `, incognito.`<br>done and connected: `I connected to ...`<br>done and not connected: `I asked to connect to ...` | link description, one code span per line<br>welcome, one code span per line<br>both cut to fit the card ([1.9](#19-action-refusals))<br>with `message`: `With the message:` and its lines |
| `send` | `I want to send a message to SUBJECT:`, with `, where I am incognito` before the colon in incognito chats<br>done: `I sent a message to ...` | text, one code span per line<br>with `file`: ``attaching TYPE `PATH`, SIZE``<br>`replying to NAME: QUOTE` with `reply_to`, the quote cut to 100 characters<br>`mentioning @A, @'B C'` with `mention` |
| `download` | `I want to download a file from SENDER in KIND NAME (CHAT):`, without `SENDER in ` outside groups<br>done: `I downloaded a file from ...` | ``` `NAME`, SIZE ```<br>an XFTP file: `Servers: known.`, or ``Servers: `HOST`, `HOST`, unknown: downloading shows them this computer's IP address.`` (`it` for one) |
| `leave` | `I want to delete KIND NAME (@N).` or `I want to leave KIND NAME (#N).`<br>done: `I deleted ...` or `I left ...` | |

SUBJECT is `KIND NAME`, or `an unnamed KIND`, followed by ` (SIMPLEX_NAME)` when known ([actions.ts:310-313](../src/actions.ts#L310-L313)). For `connect`, SIMPLEX_NAME is the target when it is a name; for `send`, the chat's verified SimpleX name.

A card of a `send` with an image or video file, or of a `download` of an image or video, is an image or video message with the file's preview and the card text as its caption; the preview is resized to fit the card, and the card is text when it can't be resized ([control.ts:434-454](../src/control.ts#L434-L454)).

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

DECIDER is `Performed without approval (mode host)` in mode `host`, `Approved for this chat by NAME` for a standing action, and `Approved by NAME` otherwise ([control.ts:547](../src/control.ts#L547)).

Command lines ([`commandsLine`](../src/control.ts#L596-L609)):

| Tool | Command line |
|---|---|
| `connect` | `/'approve N' or /'reject N'` |
| `send` | `/'approve CHAT N', /'approve CHAT all' or /'reject CHAT N'` |
| `download` | `/'approve CHAT N', /'approve CHAT files', /'receive CHAT N' or /'reject CHAT N'`, without `/'approve CHAT files'` for unknown servers |
| `leave` | `/'approve CHAT N' or /'reject CHAT N'` |

The app shows `/'approve N'` as the tappable command `/approve N` ([PLAN, What the owner sees](../PLAN.md#what-the-owner-sees-the-agent-control-chat)).

An action performed without a card, in mode `host` or under a standing approval, gets its outcome as a new message; a `download` without a card gets none ([control.ts:144-151](../src/control.ts#L144-L151), [:488-490](../src/control.ts#L488-L490)).

### 2.3 Other posts

| Situation | Text | Code |
|---|---|---|
| `send` with a refused file; logged too | ``Refused to send `PATH` to KIND NAME (CHAT): private keys and the server's own files are not sent.`` | [control.ts:153-157](../src/control.ts#L153-L157) |
| `sndFileError` of a sent file; logged too | ``The file `NAME` was not sent to KIND NAME (CHAT): `REASON`.`` | [control.ts:159-168](../src/control.ts#L159-L168), [serve.ts:158-160](../src/serve.ts#L158-L160) |
| `/receive` of a file that is not received or not posted | ``Action N: the file was not received: `ERROR`.`` | [control.ts:272](../src/control.ts#L272) |
| after the reply to `/leave CHAT`, and after the outcome of a done `leave`, when the folder of CHAT has files | ``Files from KIND NAME (CHAT) are kept in `FOLDER`: K files, SIZE. /'delete CHAT files'`` | [control.ts:149-150](../src/control.ts#L149-L150), [:287-295](../src/control.ts#L287-L295) |

### 2.4 Membership messages

Posted in the chat and logged; in the chat, names are shown by `safeName` and the `--owners ...` part is a code span; in the log, names are folded to one line ([control.ts:420-423](../src/control.ts#L420-L423), [:77-78](../src/control.ts#L77-L78)).

| Situation | Text | Code |
|---|---|---|
| owner joined, `--owners` absent | `I'm your agent. I ask here before I connect, send, download a file or leave a chat.` (chat only) | [control.ts:31](../src/control.ts#L31), [:368](../src/control.ts#L368) |
| member joined, `--owners` without a list | `NAME joined (member ID). Restart the server with --owners ENTRIES` | [control.ts:373-376](../src/control.ts#L373-L376) |
| start with `--owners` without a list and humans present | `A joined (member ID). B joined (member ID). Restart the server with --owners ENTRIES` | [control.ts:340-343](../src/control.ts#L340-L343) |
| a human became owner outside the list | `NAME (member ID) is now an owner and can't approve actions yet. Restart the server with --owners ENTRIES` | [control.ts:381-385](../src/control.ts#L381-L385) |
| a listed owner stopped being a human owner | `NAME (member ID) is no longer an owner. Restart the server with --owners ENTRIES`<br>none remains: `NAME (member ID) is no longer an owner. No owners remain: restart the server with --reset-owners --owners` | [control.ts:386-391](../src/control.ts#L386-L391) |

ENTRIES: `ID:NAME` of each human, or of each human owner, joined by commas; an entry with characters other than letters, digits, `_` and `-` is quoted for the shell ([control.ts:767-777](../src/control.ts#L767-L777)).

### 2.5 Group profile

| Field | Value | Code |
|---|---|---|
| display name | the agent's display name, updated at start when it differs | [control.ts:104-106](../src/control.ts#L104-L106) |
| full name | empty | [control.ts:505](../src/control.ts#L505) |
| description | `Supervise your AI agent here: approve or reject its connections and messages with one tap.` | [control.ts:506](../src/control.ts#L506) |
| history | on | [control.ts:507](../src/control.ts#L507) |
| command menu | `/pending` "Pending actions"<br>`/list` "Chats and approvals"<br>`/leave` "Leave a chat", params `<chat ID>`<br>updated at start when it differs | [control.ts:10-14](../src/control.ts#L10-L14), [:104-106](../src/control.ts#L104-L106) |

---

## 3. CLI

Binary: `simplex-mcp`, compiled `dist/cli.js` ([package.json:5-7](../package.json#L5-L7)). Help text: [cli.ts:10-38](../src/cli.ts#L10-L38).

```
simplex-mcp --help
simplex-mcp serve NAME [options]
simplex-mcp register NAME [options]
```

NAME: 1 to 32 characters of `[a-z0-9]` and single hyphens, starting with a letter or digit ([cli.ts:149-151](../src/cli.ts#L149-L151)).

### 3.1 `serve`

| Option | Value | Default | Rule | Code |
|---|---|---|---|---|
| `--data` | folder | `~/.simplex-mcp/NAME` | resolved to an absolute path; `dataGiven` is set | [cli.ts:69-72](../src/cli.ts#L69-L72) |
| `--display-name` | text | NAME when the user is created | must equal `validName(text)` | [cli.ts:73-75](../src/cli.ts#L73-L75), [:153-157](../src/cli.ts#L153-L157) |
| `--approve` | `requests` or `host` | `requests` | | [cli.ts:76-78](../src/cli.ts#L76-L78) |
| `--owners` | absent value, or `ID:NAME,...` | absent | the value is absent when the next argument is missing or starts with `--`; it is split at each comma followed by `ID:`, so a name may hold commas; entries match `^([1-9]\d*):(.+)$` | [cli.ts:79-81](../src/cli.ts#L79-L81), [:174-178](../src/cli.ts#L174-L178) |
| `--expire` | hours | 23 | integer 1 to 23 | [cli.ts:82-84](../src/cli.ts#L82-L84) |
| `--stop-wait` | seconds | 10 | integer 0 to 60 | [cli.ts:85-87](../src/cli.ts#L85-L87) |
| `--reset-owners` | flag | off | refused with an `--owners` list | [cli.ts:88-90](../src/cli.ts#L88-L90), [:104-106](../src/cli.ts#L104-L106) |
| `--new-token` | flag | off | | [cli.ts:91-93](../src/cli.ts#L91-L93) |
| `--port` | number | 5226 | integer 1 to 65535 | [cli.ts:94-96](../src/cli.ts#L94-L96) |
| `--qr-code` | `small` or `large` | `small` | | [cli.ts:97-99](../src/cli.ts#L97-L99) |

### 3.2 `register`

| Option | Value | Default | Code |
|---|---|---|---|
| `--scope` | `user` or `local` | `user` | [cli.ts:114-116](../src/cli.ts#L114-L116) |
| `--data` | folder | `~/.simplex-mcp/NAME` | [cli.ts:117-119](../src/cli.ts#L117-L119) |

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
| 0 | `--help` text on stdout; a stop after a signal | [cli.ts:201-203](../src/cli.ts#L201-L203), [serve.ts:54](../src/serve.ts#L54) |
| 2 | `Usage: simplex-mcp serve NAME [options] or simplex-mcp register NAME [options]. Run simplex-mcp --help.` | [cli.ts:48](../src/cli.ts#L48) |
| 2 | `NAME: 1 to 32 lowercase letters, digits and single hyphens, starting with a letter or digit.` | [cli.ts:49](../src/cli.ts#L49) |
| 2 | `OPTION: a value expected.` | [cli.ts:138](../src/cli.ts#L138) |
| 2 | `Unknown option OPTION. Run simplex-mcp --help.` | [cli.ts:191](../src/cli.ts#L191) |
| 2 | `--display-name: not a valid SimpleX name; try VALID` | [cli.ts:155](../src/cli.ts#L155) |
| 2 | `--approve: requests or host expected, got VALUE` | [cli.ts:160](../src/cli.ts#L160) |
| 2 | `--qr-code: small or large expected, got VALUE` | [cli.ts:165](../src/cli.ts#L165) |
| 2 | `--scope: user or local expected, got VALUE` | [cli.ts:170](../src/cli.ts#L170) |
| 2 | `--owners: ID:NAME expected, got ENTRY` | [cli.ts:176](../src/cli.ts#L176) |
| 2 | `--expire: hours from 1 to 23 expected, got VALUE` | [cli.ts:83](../src/cli.ts#L83), [:182](../src/cli.ts#L182) |
| 2 | `--stop-wait: seconds from 0 to 60 expected, got VALUE` | [cli.ts:86](../src/cli.ts#L86), [:182](../src/cli.ts#L182) |
| 2 | `--port: a port from 1 to 65535 expected, got VALUE` | [cli.ts:95](../src/cli.ts#L95), [:182](../src/cli.ts#L182) |
| 2 | `--reset-owners takes --owners without a list: member IDs change in a new agent control chat.` | [cli.ts:105](../src/cli.ts#L105) |
| 2 | `--owners: member ENTRY is not in the agent control chat. Start with --reset-owners --owners, or use the IDs of current members.` | [control.ts:349](../src/control.ts#L349) |
| 1 | `Another server uses DATA (PID N). If no server runs, remove DATA/lock.` | [files.ts:21-23](../src/files.ts#L21-L23), [:60-62](../src/files.ts#L60-L62) |
| 1 | `Port PORT is in use. Start with --port N.` | [serve.ts:229](../src/serve.ts#L229) |
| 1 | `Agent "NAME" was never started. Start it with: npx -y @simplex-chat/mcp serve NAME` | [register.ts:35](../src/register.ts#L35) |
| 1 | `Add this entry under LOCATION in ~/.claude.json: {"simplex-NAME": ENTRY}` | [register.ts:55-58](../src/register.ts#L55-L58) |
| 1 | `claude mcp add-json failed: STDERR` | [register.ts:42](../src/register.ts#L42) |
| 1 | `simplex-mcp: ERROR` for other errors, `stop: ERROR` for a failed stop | [cli.ts:217-218](../src/cli.ts#L217-L218), [serve.ts:55-58](../src/serve.ts#L55-L58) |

LOCATION is `"mcpServers"` for scope `user`, and `"projects" > "PROJECT" > "mcpServers"` for scope `local` ([register.ts:56](../src/register.ts#L56)).

### 3.4 Terminal lines

stdout ([log.ts:37-39](../src/log.ts#L37-L39)):

| Moment | Line | Code |
|---|---|---|
| start | `SimpleX MCP server VERSION, agent "NAME"`<br>`Data folder: DATA`<br>`Approval mode: MODE` | [serve.ts:73-75](../src/serve.ts#L73-L75) |
| control chat created | `Agent control chat "DISPLAY" created.` | [control.ts:511](../src/control.ts#L511) |
| link opened | `Join the agent control chat in the SimpleX app. The link works once:`<br>QR code<br>link<br>small QR code: `If the QR code does not scan, restart with --qr-code large, or paste the link into the app.`<br>role line | [control.ts:399-409](../src/control.ts#L399-L409) |
| listening | `Listening on http://127.0.0.1:PORT/mcp` | [serve.ts:126](../src/serve.ts#L126) |
| owner setup done | `Add the agent to Claude Code:`<br>`  npx -y @simplex-chat/mcp register NAME [--data DATA]` | [serve.ts:185-187](../src/serve.ts#L185-L187) |
| owner joined | `NAME joined (member ID) and is now an owner. The link is deleted.` | [control.ts:369](../src/control.ts#L369) |
| listed owners set | `Owners: ENTRIES.` | [control.ts:360](../src/control.ts#L360) |
| approvers gone, `--owners` absent | `No owners in the agent control chat.` | [control.ts:32](../src/control.ts#L32), [:193](../src/control.ts#L193), [:336](../src/control.ts#L336) |
| control chat lost | `The agent lost its control chat. Restart the server with --reset-owners.` | [control.ts:396](../src/control.ts#L396) |
| action created | `Action N: WHAT` with `, as itself` or `, incognito` for `connect` and ` (CHAT)` for the other tools | [actions.ts:180-181](../src/actions.ts#L180-L181), [:315-327](../src/actions.ts#L315-L327) |
| counter write or card post failed | `Action N: failed: ERROR.` | [actions.ts:260](../src/actions.ts#L260) |
| perform ended, not interrupted by the stop, mode `requests` | `Action N approved by NAME: RESULT` with RESULT `sent.`, `downloaded.`, `deleted.`, `left.`, `connected.`, `connection requested.` or `failed: ERROR.` | [actions.ts:281](../src/actions.ts#L281), [:333-341](../src/actions.ts#L333-L341) |
| perform ended, not interrupted by the stop, mode `host` | `Action N: performed without approval (mode host).`, or `...; failed: ERROR.` | [actions.ts:335](../src/actions.ts#L335) |
| rejected | `Action N rejected by NAME.` | [actions.ts:228](../src/actions.ts#L228) |
| expired | `Action N expired.` | [actions.ts:198](../src/actions.ts#L198) |
| `/leave CHAT` | `Left KIND NAME (CHAT), requested by NAME.` or `Deleted ...` | [control.ts:282](../src/control.ts#L282) |
| refused file, file not sent | as in [2.3](#23-other-posts), names and paths on one line | [control.ts:155](../src/control.ts#L155), [:166](../src/control.ts#L166) |
| stop | `Cancelled K action.` or `Cancelled K actions.` when K > 0<br>`Stopping: sending K cancelled card(s), up to S s. Ctrl-C to stop now.` when cards were edited or posted and S > 0<br>`Stopped.` | [serve.ts:176](../src/serve.ts#L176), [:189-203](../src/serve.ts#L189-L203), [:180](../src/serve.ts#L180) |
| membership messages | see [2.4](#24-membership-messages) | |

stderr: `CONTEXT: ERROR` from `logError` ([log.ts:41-43](../src/log.ts#L41-L43)), with CONTEXT one of `action N`, `action N card`, `action N card image`, `action N file`, `control chat command`, `EVENT in the agent control chat`, `EVENT event`, `refused file`, `file not sent`, `download of file N`, `file N`, `moving file N`, `cancelling file N`, `resizing PATH`, `preview`, `MCP`, `HTTP request`, `stop`, `simplex-mcp`. The library's download lines are written to stderr.
