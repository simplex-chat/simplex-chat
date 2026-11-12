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
| Tools are registered on each per-request `McpServer` with description, input schema, output schema and, in mode `host`, `_meta: {"anthropic/requiresUserInteraction": true}` for `connect`, `send`, `download` and `leave` | [tools.ts:460-467](../src/tools.ts#L460-L467), [:497-507](../src/tools.ts#L497-L507) |
| Tool descriptions and server instructions; the instructions name the agent's display name | [tools.ts:410-450](../src/tools.ts#L410-L450), [serve.ts:114](../src/serve.ts#L114) |
| Arguments are validated by the SDK; a failure is a result with `isError: true` and the text `Input validation error: Invalid arguments for tool NAME: ERRORS` | [mcp-BumlNux0.cjs:1812-1817](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L1812-L1817) |
| `additionalProperties: false` is set in every input schema | [tools.ts:251](../src/tools.ts#L251), [:268](../src/tools.ts#L268), [:279](../src/tools.ts#L279), [:282](../src/tools.ts#L282), [:290](../src/tools.ts#L290), [:301](../src/tools.ts#L301) |
| A result is sent as `structuredContent` and as its JSON in one text item | [tools.ts:512](../src/tools.ts#L512) |
| A tool error is sent as `isError: true` with one text item: the `ToolError` text, or the `errorText` of other errors | [tools.ts:514](../src/tools.ts#L514), [log.ts:45-49](../src/log.ts#L45-L49) |
| An aborted call ends with the result `Cancelled.`, `isError: true`, which the SDK discards | [tools.ts:469](../src/tools.ts#L469), [:505](../src/tools.ts#L505) |
| With a `progressToken` in `_meta`, `notifications/progress` is sent at the start of each wait and every 60 s, with `progress` 0, 1, 2, ... and a `message` | [tools.ts:1572-1582](../src/tools.ts#L1572-L1582), [messages.ts:301-305](../src/messages.ts#L301-L305) |
| The call is aborted when the client closes the response stream or sends `notifications/cancelled`; the action is kept | [src-BhnGYybN.cjs:6338-6341](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs#L6338-L6341), [:6425](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs#L6425) |
| A call with the same tool and action fields as an action in memory is attached to it | [tools.ts:1564-1566](../src/tools.ts#L1564-L1566), [actions.ts:151-153](../src/actions.ts#L151-L153) |
| Messages are marked read after the last wait of a call that is not aborted | [tools.ts:676](../src/tools.ts#L676), [:821](../src/tools.ts#L821), [:1151](../src/tools.ts#L1151) |

Action fields of the key:

| Tool | Fields |
|---|---|
| `connect` | `target`<br>`incognito` (default `false`)<br>`message` (default `null`) |
| `send` | `chat`<br>`text` (default `null`)<br>`file` (default `null`)<br>`as` (default `null`)<br>`reply_to` (default `null`)<br>`mention` (default `null`) |
| `download` | `chat`<br>`item` |
| `leave` | `chat` |

Result status of the four action tools ([`actionResult`](../src/tools.ts#L645-L657)):

| Status | Meaning | Extra fields |
|---|---|---|
| `done` | performed | per tool |
| `failed` | failure of the card post, the counter write or the perform, or loss of the control chat | `error` |
| `rejected` | an approver sent `/reject` | |
| `expired` | no decision before the expiry time | |
| `known` | `connect` only: the target is an existing chat past the prepared state; no action | chat fields |
| `performing` | `download` only: `wait` passed after the approval; the download continues | |

### 1.2 `connect`

Handler: [`AgentTools.connect`](../src/tools.ts#L626-L639). Perform: [`performConnect`](../src/tools.ts#L695-L726). Schemas: [input](../src/tools.ts#L241-L252), [output](../src/tools.ts#L307-L317).

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
| `known` | `status`, chat fields of the known chat | [tools.ts:631](../src/tools.ts#L631) |
| `done`, chat unknown | `action`, `status`, `type`, `connected: false`, plan fields `name`, `peer_type`, `description`, `welcome`, `commands`, `known_as` when present | [tools.ts:642](../src/tools.ts#L642), [:1358-1364](../src/tools.ts#L1358-L1364) |
| `done`, chat known | `action`, `status`, chat fields, `messages` | [tools.ts:659-679](../src/tools.ts#L659-L679) |
| `failed` | `action`, `status`, `error` | [tools.ts:650](../src/tools.ts#L650) |
| `rejected`, `expired` | `action`, `status` | [tools.ts:650](../src/tools.ts#L650) |

Chat fields ([`chatFields`](../src/tools.ts#L1214-L1241)):

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

`messages`: message items among the last 50 items of the chat when the result is built, replaced by their collected copies, followed by the items collected since the connection; live items are left out; new items are marked read ([tools.ts:661](../src/tools.ts#L661), [:671-678](../src/tools.ts#L671-L678)).

Tool errors:

| Text | Condition | Code |
|---|---|---|
| `Not a SimpleX link or name: TARGET` | whitespace in `target`, or `invalidConnReq` from `apiConnectPlan` | [tools.ts:683](../src/tools.ts#L683), [:689](../src/tools.ts#L689) |
| an app error text, for example `Name not found: This SimpleX name is not registered. Please check the name.`, or `SimpleX error: CHAIN: DETAILS` | other errors of `apiConnectPlan`, or a plan of type `error` | [tools.ts:690](../src/tools.ts#L690), [:1285](../src/tools.ts#L1285), [log.ts:51-53](../src/log.ts#L51-L53) |
| `TARGET can't be joined now: PLAN.` | `invitationLink` other than `ok` and `known`<br>`contactAddress` other than `ok`, `known`, `contactViaAddress` and `connectingConfirmReconnect`<br>`groupLink` other than `ok`, `known` and `connectingConfirmReconnect` | [tools.ts:1276](../src/tools.ts#L1276), [:1289](../src/tools.ts#L1289), [:1307](../src/tools.ts#L1307), [:1329](../src/tools.ts#L1329) |
| `message: TARGET is not a contact or business address with short link data, or its contact exists without a connection.` | `message` given and the plan has `takesMessage: false` | [tools.ts:633](../src/tools.ts#L633) |
| refusals | see [1.9](#19-action-refusals) | |

`takesMessage` is `true` for a contact address with short link data, for a known prepared contact, and for a known prepared business group ([tools.ts:1280](../src/tools.ts#L1280), [:1300](../src/tools.ts#L1300), [:1318](../src/tools.ts#L1318)).

### 1.3 `send`

Handler: [`AgentTools.send`](../src/tools.ts#L783-L809). Result: [`sendResult`](../src/tools.ts#L811-L829). Perform: [`performSend`](../src/tools.ts#L831-L855). Schemas: [input](../src/tools.ts#L254-L269), [output](../src/tools.ts#L319-L331).

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

File types by extension, lower-cased ([tools.ts:169-178](../src/tools.ts#L169-L178), [`attachmentMedia`](../src/tools.ts#L1522-L1533)):

| Extension | Message type | Sent file | Preview and duration |
|---|---|---|---|
| `.jpg`, `.jpeg`, `.png` | image | `core.resizeImage(..., Image)`, written to `DATA/files/sent/`; the original when resizing fails | `core.resizeImage(..., MessagePreview)`; the fixed preview when it fails |
| `.gif`, `.webp` | image | unchanged | the fixed preview |
| `.mp4`, `.mov` | video | unchanged | the first frame from `ffmpeg`, resized as a preview; duration from `ffprobe`, rounded; the fixed preview and 0 when they fail |
| other, or `as: "file"` | file | unchanged | |

The fixed preview is a 16 × 9 mid-gray PNG ([tools.ts:159](../src/tools.ts#L159)). `ffmpeg` and `ffprobe` run with `-protocol_whitelist file -f mov`, 30 s and 64 MB of output each ([tools.ts:1544-1562](../src/tools.ts#L1544-L1562)).

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
| `text: required without file.` | neither `text` nor `file` | [tools.ts:788](../src/tools.ts#L788) |
| `as: applies only with file.` | `as` without `file` | [tools.ts:789](../src/tools.ts#L789) |
| `No chat CHAT. list shows the agent's chats.` | invalid reference, the control chat, or a store error when the chat is read | [tools.ts:1180-1187](../src/tools.ts#L1180-L1187), [:1466-1468](../src/tools.ts#L1466-L1468) |
| `The agent is not connected to CHAT and can't post; list shows connected: false.` | the chat is not connected | [tools.ts:791](../src/tools.ts#L791) |
| `The agent has role ROLE in CHAT and can't post.` | membership role `observer` or `relay` | [tools.ts:792-795](../src/tools.ts#L792-L795) |
| `reply_to: CHAT has no item N.` | the read of `around=N` fails, or the item is absent from it | [tools.ts:1192](../src/tools.ts#L1192) |
| `reply_to: item N in CHAT is not a message.` | the item's content is not `rcvMsgContent` or `sndMsgContent` | [tools.ts:1194](../src/tools.ts#L1194) |
| `mention: CHAT is a contact chat; mentions apply to groups and channels.` | `mention` with `@N` | [tools.ts:1200](../src/tools.ts#L1200) |
| `mention: CHAT has no member N.` | ID absent from `apiListMembers` | [tools.ts:1205](../src/tools.ts#L1205) |
| `mention: members N and M have the same name.` | two mentioned members with the same display name | [tools.ts:1208](../src/tools.ts#L1208) |
| `file: an absolute path is expected.`<br>`file: no file at FILE.`<br>`file: PATH is not a file.`<br>`file: PATH can't be read.` | the path, after symbolic links | [tools.ts:1475-1486](../src/tools.ts#L1475-L1486) |
| `file: PATH is refused: private keys and the server's own files are not sent.` | a file named `id_*`, `*.pem`, `*.key`, `*.p12` or `*.pfx`, or under `~/.ssh`, `~/.gnupg` or the secrets folder, or under the data folder or `~/.simplex-mcp` outside `DATA/files/`; each folder by its real path | [tools.ts:871-874](../src/tools.ts#L871-L874), [:880-885](../src/tools.ts#L880-L885) |
| `file: PATH is empty.` | size 0 | [tools.ts:875](../src/tools.ts#L875) |
| `text: too long for the message with its quote; shorten it.`<br>`text: too long for a message with this TYPE; shorten it.` | the message, as the core sends it, above 15602 bytes | [tools.ts:799-801](../src/tools.ts#L799-L801), [:1443-1459](../src/tools.ts#L1443-L1459) |
| refusals | see [1.9](#19-action-refusals) | |

The message is measured as 600 bytes plus the JSON of its content, the file's name, the quote and the mentioned names. The quote is the quoted text, or its file's name when the text is empty; a sent image or video quotes it as text, otherwise an image's preview is kept ([tools.ts:1443-1459](../src/tools.ts#L1443-L1459)).

The perform fails with `file: PATH changed after the request.` when the file's real path, size, inode or change time differs from the request ([tools.ts:887-892](../src/tools.ts#L887-L892)). A resized copy is deleted at `sndFileCompleteXFTP`, at `sndFileError` and after a failed send ([tools.ts:852](../src/tools.ts#L852), [:857-867](../src/tools.ts#L857-L867), [serve.ts:158-162](../src/serve.ts#L158-L162)).

### 1.4 `receive`

Handler: [`AgentTools.receive`](../src/tools.ts#L1123-L1157). Schemas: [input](../src/tools.ts#L271-L280), [output](../src/tools.ts#L333-L341).

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

Tool errors: `No chat CHAT. list shows the agent's chats.` for an invalid reference, the control chat, or a store error of the first read, `initial` 1 ([tools.ts:1125](../src/tools.ts#L1125), [:1129-1131](../src/tools.ts#L1129-L1131)); other chat errors, a store error of a later page read included, as their error text ([log.ts:45-53](../src/log.ts#L45-L53)).

Messages are read by `receive` without an action or an approval.

### 1.5 `download`

Handler: [`AgentTools.download`](../src/tools.ts#L969-L990). Result: [`downloadResult`](../src/tools.ts#L1017-L1025). Perform: [`performDownload`](../src/tools.ts#L1027-L1031), [`downloadFile`](../src/tools.ts#L531-L558). Schemas: [input](../src/tools.ts#L293-L302), [output](../src/tools.ts#L393-L408).

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

FOLDER is `group-N` for `#N` and `contact-N` for `@N`; the extension is the file name's extension when it has 1 to 16 ASCII letters and digits ([messages.ts:215-223](../src/messages.ts#L215-L223)). `/forward` downloads a file to `DATA/forwarded/FOLDER/` under the same name; a later perform of the download moves it to the download path, after the copy for the post is made ([tools.ts:531-548](../src/tools.ts#L531-L548)).

Steps before the action:

| Step | Code |
|---|---|
| a file in status `rcvInvitation`, or an XFTP file: the file's servers from `apiGetChatItemInfo`; while none, the call waits for `rcvFileDescrReady` until `wait` seconds after the call, with the progress `The sender is uploading the file of item N in CHAT; waiting until TIME.` | [tools.ts:992-1009](../src/tools.ts#L992-L1009) |
| unknown servers: the file's servers that are not among the XFTP servers of `apiGetUserServers`, of no operator or of an enabled one, compared without their passwords | [tools.ts:1011-1015](../src/tools.ts#L1011-L1015) |
| without unknown servers, and for a file ID not in rejected files, the chat's files approval, read again, makes the action performed at once | [tools.ts:984](../src/tools.ts#L984) |

Perform: `apiReceiveFile(fileId, path, approvedRelays)`, for a file in status `rcvInvitation`. `approvedRelays` is true in mode `requests` for a download whose card named unknown servers, and false otherwise; the core then checks the complete description ([tools.ts:554](../src/tools.ts#L554), [:1033-1042](../src/tools.ts#L1033-L1042)). The download ends at `rcvFileComplete`, `rcvFileError`, `rcvFileSndCancelled` or `chatItemUpdated` of its file, or at a check every 60 s ([tools.ts:570-592](../src/tools.ts#L570-L592), [:1044-1112](../src/tools.ts#L1044-L1112)).

Failure texts of the perform:

| Text | Condition | Code |
|---|---|---|
| the file's error, as in the file field | the file failed | [tools.ts:1073-1083](../src/tools.ts#L1073-L1083) |
| `The sender cancelled the file.` | `rcvFileSndCancelled` | [tools.ts:587-592](../src/tools.ts#L587-L592) |
| `The message or its chat was deleted.` | a check finds no message, contact or group | [tools.ts:163](../src/tools.ts#L163), [:1063-1071](../src/tools.ts#L1063-L1071) |
| `The download made no progress for 30 minutes and was cancelled.` | no rise of `rcvProgress` for 30 minutes; the file is cancelled | [tools.ts:167](../src/tools.ts#L167), [:1056-1060](../src/tools.ts#L1056-L1060) |
| `The received file is not in the agent's files folder.` | the file is received and neither the download path nor the forwarded path has a file | [messages.ts:14](../src/messages.ts#L14), [tools.ts:1085-1089](../src/tools.ts#L1085-L1089) |
| `Your owner deleted the files of this chat.` | `/delete CHAT files` | [tools.ts:164](../src/tools.ts#L164), [:918-936](../src/tools.ts#L918-L936) |
| `The file is on unknown servers that were not approved.` | a check reads an accepted file in status `rcvInvitation`: the core returned it to the offer when its description completed | [tools.ts:165](../src/tools.ts#L165), [:1052](../src/tools.ts#L1052) |
| `Unknown servers! Without Tor or VPN, your IP address will be visible to these XFTP relays: HOST, HOST.` | the core refuses the servers of the complete description at the acceptance | [log.ts:78-79](../src/log.ts#L78-L79) |

Tool errors:

| Text | Condition | Code |
|---|---|---|
| `No chat CHAT. list shows the agent's chats.` | invalid reference, the control chat, or a store error when the chat is read | [tools.ts:974](../src/tools.ts#L974) |
| `item: CHAT has no item N.` | the item is not found | [tools.ts:976](../src/tools.ts#L976) |
| `item: item N in CHAT has no received file.` | the item is not received or has no file | [tools.ts:978](../src/tools.ts#L978) |
| `item: the file of item N in CHAT failed: ERROR` | the file field has status `failed` | [tools.ts:979-980](../src/tools.ts#L979-L980) |
| `item: the sender has not uploaded the file of item N in CHAT yet; call download again later.` | no servers by the end of `wait`, also for a file invited without a description, whose servers the core does not list; no action is created | [tools.ts:982](../src/tools.ts#L982), [:1007](../src/tools.ts#L1007) |
| refusals | see [1.9](#19-action-refusals) | |

A file ID whose download returned `rejected` or `expired` shows status `rejected` until a download of it is done or the server restarts ([tools.ts:1022-1023](../src/tools.ts#L1022-L1023)).

### 1.6 `list`

Handler: [`AgentTools.listChats`](../src/tools.ts#L1159-L1168). Schemas: [input](../src/tools.ts#L282), [output](../src/tools.ts#L343-L380).

Input: an empty object.

Output:

| Field | Item fields |
|---|---|
| `chats` | `chat`<br>`type`<br>`name`<br>`simplex_name`, when verified<br>`peer_type`, when present<br>`incognito`<br>`known_as`, when incognito<br>`connected`<br>`unread`: `chatStats.unreadCount`<br>`approved: true`, only under a standing approval of messages in mode `requests` |
| `actions` | `action`<br>`tool`<br>`input`: the input of the creating call<br>`state`<br>`open`: `true` while a call waits on it |
| `secrets` | an object, only with `--secrets-folder`:<br>`folder`: SECRETS, the server's path<br>`names`: the names of the secrets, sorted |

`chats`: the contacts and groups of the main conversations from one read, `apiGetChats(USER, last 1000000)`, except the control chat ([tools.ts:1170-1178](../src/tools.ts#L1170-L1178)). `actions`: every action in memory ([tools.ts:1165](../src/tools.ts#L1165)). `secrets`: the folder and the names read at each call; values are not returned ([tools.ts:1166-1167](../src/tools.ts#L1166-L1167), [files.ts:64-66](../src/files.ts#L64-L66)). The `list` description and the server instructions state the use of a secret: inside a shell command, as `"$(cat FOLDER/NAME)"`, never by its value ([tools.ts:431](../src/tools.ts#L431), [:448](../src/tools.ts#L448)).

### 1.7 `leave`

Handler: [`AgentTools.leave`](../src/tools.ts#L944-L953). Perform: [`leaveChat`](../src/tools.ts#L959-L967). Schemas: [input](../src/tools.ts#L284-L291), [output](../src/tools.ts#L382-L391).

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

Tool errors: `No chat CHAT. list shows the agent's chats.` when no action with the same key is in memory and the chat is invalid, the control chat or unknown ([tools.ts:947-949](../src/tools.ts#L947-L949)); refusals.

### 1.8 Message, file and command objects

Message ([`Message`](../src/messages.ts#L16-L28), [`toMessage`](../src/messages.ts#L174-L192), [schema](../src/tools.ts#L180-L213)):

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

Only received message items (`rcvMsgContent`) are returned by `send` and `receive`; `connect` also returns sent ones (`sndMsgContent`) ([messages.ts:101](../src/messages.ts#L101), [tools.ts:671](../src/tools.ts#L671)).

Command ([`commandsOf`](../src/tools.ts#L1425-L1432), [schema](../src/tools.ts#L215-L225)):

| Form | Fields |
|---|---|
| command | `keyword`<br>`label`<br>`params`, when present |
| menu | `label`<br>`commands`: commands and menus |

### 1.9 Action refusals

Refusals of a new `connect`, `send`, `download` or `leave` action, first match ([actions.ts:172-177](../src/actions.ts#L172-L177), [control.ts:127-136](../src/control.ts#L127-L136)):

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

Largest card form ([control.ts:662-670](../src/control.ts#L662-L670)): the maximum of the creation form, the `done` form and a `failed` form with an error of 600 × `€`, with the action's own decider or the longest current approver name; each form is measured as JSON-escaped UTF-8: `"`, `\`, `\n`, `\r`, `\t` 2 bytes, other characters below U+0020 6 bytes.

Link texts of a `connect` card ([control.ts:130](../src/control.ts#L130), [:684-698](../src/control.ts#L684-L698)): the description and welcome lines take the bytes that the largest form leaves without them. Lines are kept while they fit; the next line is cut to the bytes left and ends in `…`; later lines are left out. The tool result keeps both texts whole.

---

## 2. Agent control chat

### 2.1 Commands and replies

Accepted text ([`parseCommand`](../src/control.ts#L585-L590), [`ownerRequest`](../src/control.ts#L750-L765)):

```
message   one line, an optional trailing newline
line      spaces, "/", KEYWORD, PARAMS
KEYWORD   characters up to the first whitespace
PARAMS    the rest, trimmed, split on whitespace
CHAT      @N or #N, 1 to 15 digits
VALUE     PARAMS without its first word and the whitespace after it
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
  secret NAME VALUE
  unsecret NAME
  secrets
```

The server reads commands from members' messages in the main conversation of the control chat ([serve.ts:136](../src/serve.ts#L136), [control.ts:179-192](../src/control.ts#L179-L192)). It answers a command with a reply that quotes it, one command after another, and it runs the expiry check before each command ([control.ts:189](../src/control.ts#L189), [:212](../src/control.ts#L212)).

A received message whose first word is `/secret`, in any letter case, from any member, while the control chat is not lost ([control.ts:181-183](../src/control.ts#L181-L183)):

- is deleted for all members by `apiDeleteMemberChatItem`, queued after the earlier commands; after a failed deletion, the command is not run, and the error is logged ([control.ts:187-191](../src/control.ts#L187-L191));
- in a member support chat, is deleted and gets no reply ([control.ts:183](../src/control.ts#L183));
- outside the accepted text, such as a message of several lines, gets the usage reply of `secret`, from any sender ([control.ts:185](../src/control.ts#L185), [:189](../src/control.ts#L189));
- gets its reply as a new message, without a quote ([control.ts:190](../src/control.ts#L190)).

| Request | Condition | Reply | Code |
|---|---|---|---|
| one-line `/` text outside the requests | any sender | `Commands: /approve, /reject, /receive, /forward, /revoke, /leave, /delete, /pending, /list, /secret, /unsecret, /secrets. Each card shows its commands.` | [control.ts:19](../src/control.ts#L19), [:214](../src/control.ts#L214) |
| any request | sender not an approver, `--owners` absent | `Only owners approve actions.` | [control.ts:215](../src/control.ts#L215) |
| any request | sender not an approver, `--owners` given | `Only the owners listed in --owners approve actions.` | [control.ts:215](../src/control.ts#L215) |
| `approve`, `reject`, `receive`, `forward` with N | action N absent, not `pending`, or without a card | `Action N is not pending.` | [control.ts:244](../src/control.ts#L244) |
| the same with CHAT | action N is a `connect`, or for another chat | `Action N is not for CHAT.` | [control.ts:245](../src/control.ts#L245) |
| `approve` | action N is a download | `Action N is a download: COMMANDS.`, COMMANDS the card's command line | [control.ts:251](../src/control.ts#L251) |
| `receive`, `forward`, `receive CHAT N all` | action N is not a download | `Action N is not a download.` | [control.ts:249](../src/control.ts#L249) |
| `approve`, `reject`, `receive`, `forward` | accepted | none; the card is edited at the outcome; `forward` posts the file, as a reply to the card when the message with the card as its quote is within 15602 bytes, and the card stays pending | [control.ts:246-255](../src/control.ts#L246-L255), [:282-298](../src/control.ts#L282-L298) |
| `approve CHAT all` | mode `host`; nothing is stored | `Approvals apply in mode requests; this server runs in mode host.` | [control.ts:224](../src/control.ts#L224) |
| `approve CHAT all`, `receive CHAT N all` | CHAT unknown or the control chat | `No chat CHAT. /list shows the agent's chats.` | [control.ts:263](../src/control.ts#L263) |
| `approve CHAT all` | accepted; the pending sends of CHAT with a card are approved | `Approved for KIND NAME (CHAT) by APPROVER. /'revoke CHAT'` | [control.ts:260-271](../src/control.ts#L260-L271) |
| `receive CHAT N all` | accepted; download N is performed, and the other pending downloads of CHAT stay pending | `Files approved for KIND NAME (CHAT) by APPROVER. /'receive CHAT off'` | [control.ts:254](../src/control.ts#L254), [:268-270](../src/control.ts#L268-L270) |
| `revoke CHAT` | CHAT unknown, or without a standing approval | `Messages to CHAT have no standing approval.` | [control.ts:276](../src/control.ts#L276) |
| `revoke CHAT` | accepted | `Messages to KIND NAME (CHAT) need approval again.` | [control.ts:279](../src/control.ts#L279) |
| `receive CHAT off` | CHAT unknown, or without a files approval | `Files from CHAT have no standing approval.` | [control.ts:276](../src/control.ts#L276) |
| `receive CHAT off` | accepted | `Files from KIND NAME (CHAT) need approval again.` | [control.ts:279](../src/control.ts#L279) |
| `leave CHAT` | CHAT unknown or the control chat | `No chat CHAT. /list shows the agent's chats.` | [control.ts:302](../src/control.ts#L302) |
| `leave CHAT` | accepted, at once in both modes; the pending actions of CHAT with a card are rejected by the sender | `Left KIND NAME (CHAT), requested by NAME.`, or `Deleted ...` for a contact; logged too | [control.ts:300-314](../src/control.ts#L300-L314) |
| `delete CHAT files` | the chat's downloads in memory fail and are cancelled; its other files in status `rcvAccepted`, `rcvTransfer` or `rcvWarning`, such as downloads the core continues after a restart, are cancelled; the folder of CHAT and its forwarded folder are deleted, for a current or a left chat | `Deleted K files from CHAT, SIZE.` (`1 file`), or `No files from CHAT.` | [control.ts:321-324](../src/control.ts#L321-L324), [tools.ts:918-936](../src/tools.ts#L918-L936), [messages.ts:271-279](../src/messages.ts#L271-L279) |
| `pending` | no pending action with a card | `No pending actions.` | [control.ts:219](../src/control.ts#L219) |
| `pending` | otherwise | one line per action: `N  WHAT  expires YYYY-MM-DD HH:MM UTC` | [control.ts:218](../src/control.ts#L218), [:658-660](../src/control.ts#L658-L660) |
| `list` | no chats | `No chats.` | [control.ts:342](../src/control.ts#L342) |
| `list` | otherwise | one line per chat of the 100 most recent: `CHAT  KIND NAME`, then `, approved for this chat by APPROVER`, `, files approved by APPROVER` and `, not connected` when applicable, cut to 100 bytes with `…`; then `K older chats are not shown.` (`1 older chat is not shown.`) | [control.ts:330-343](../src/control.ts#L330-L343) |
| `secret`, `unsecret`, `secrets` | `--secrets-folder` absent | `Secrets are off: the server runs without --secrets-folder.` | [control.ts:20](../src/control.ts#L20), [:232-237](../src/control.ts#L232-L237) |
| `secret NAME VALUE` | NAME not 1 to 64 capital letters, digits and `_` starting with a letter, or VALUE of fewer than 8 characters | `Usage: /secret NAME VALUE on one line. NAME: capital letters, digits and _, starting with a letter, up to 64 characters. VALUE: 8 characters or more.` | [control.ts:21](../src/control.ts#L21), [:346](../src/control.ts#L346), [files.ts:14](../src/files.ts#L14) |
| `secret NAME VALUE` | accepted; VALUE written to `SECRETS/NAME`, replacing an earlier value; the setter and the time written to `DATA/secrets.json`; logged | `Secret NAME is set. /'unsecret NAME'` | [control.ts:345-351](../src/control.ts#L345-L351) |
| `unsecret NAME` | NAME not among the secrets | `No such secret. /secrets lists them.` | [control.ts:354](../src/control.ts#L354) |
| `unsecret NAME` | accepted; `SECRETS/NAME` and its record deleted; logged | `Secret NAME is removed.` | [control.ts:353-357](../src/control.ts#L353-L357) |
| `secrets` | no secrets | `No secrets.` | [control.ts:365](../src/control.ts#L365) |
| `secrets` | otherwise | one line per name, sorted: `NAME  set by SETTER  YYYY-MM-DD HH:MM UTC`, or `NAME` for a file without a record | [control.ts:359-366](../src/control.ts#L359-L366) |

KIND is `contact`, `bot`, `agent`, `business`, `group` or `channel` ([tools.ts:1383-1390](../src/tools.ts#L1383-L1390)). WHAT is `connect to SUBJECT`, `send to SUBJECT`, `download NAME from SUBJECT`, `delete SUBJECT` or `leave SUBJECT` ([actions.ts:315-327](../src/actions.ts#L315-L327)). SIZE is in bytes, KB, MB or GB, by 1000, with one decimal ([control.ts:710-713](../src/control.ts#L710-L713)). Names are shown by `safeName` ([control.ts:727-730](../src/control.ts#L727-L730)).

A standing approval is stored in the chat's custom data, `simplexMcp` for messages and `simplexMcpFiles` for files, as `{approvedBy, approvedById, approvedAt}`; it applies in mode `requests` while member `approvedById` is an approver ([control.ts:27-30](../src/control.ts#L27-L30), [:138-141](../src/control.ts#L138-L141)).

SECRETS is the folder of `--secrets-folder`. The secrets are its files whose names have 1 to 64 capital letters, digits and `_`, starting with a letter; a value is written to `NAME.part` and renamed to `NAME`, both mode 0600 ([files.ts:14](../src/files.ts#L14), [:59-86](../src/files.ts#L59-L86)). The record of a secret in `DATA/secrets.json` is `{setBy, setById, setAt}`: the setter's display name and member ID, and the time. A secret is kept until `unsecret`, also when its setter is no longer an approver. In mode `host`, the secret commands work as in mode `requests`.

### 2.2 Cards

Card text: request lines, then one last line ([`cardText`](../src/control.ts#L592-L611), [`requestLines`](../src/control.ts#L613-L641)).

Request lines:

| Tool | First line | Further lines |
|---|---|---|
| `connect` | `I want to connect to SUBJECT, as myself.` or `, incognito.`<br>done and connected: `I connected to ...`<br>done and not connected: `I asked to connect to ...` | link description, one code span per line<br>welcome, one code span per line<br>both cut to fit the card ([1.9](#19-action-refusals))<br>with `message`: `With the message:` and its lines |
| `send` | `I want to send a message to SUBJECT:`, with `, where I am incognito` before the colon in incognito chats<br>done: `I sent a message to ...` | text, one code span per line<br>with `file`: ``attaching TYPE `PATH`, SIZE``<br>`replying to NAME: QUOTE` with `reply_to`, the quote cut to 100 characters<br>`mentioning @A, @'B C'` with `mention` |
| `download` | `I want to download a file from SENDER in KIND NAME (CHAT):`, without `SENDER in ` outside groups<br>done: `I downloaded a file from ...` | ``` `NAME`, SIZE ```<br>an XFTP file: `Servers: known.`, or ``Servers: `HOST`, `HOST`, unknown: downloading shows them this computer's IP address.`` (`it` for one) |
| `leave` | `I want to delete KIND NAME (@N).` or `I want to leave KIND NAME (#N).`<br>done: `I deleted ...` or `I left ...` | |

SUBJECT is `KIND NAME`, or `an unnamed KIND`, followed by ` (SIMPLEX_NAME)` when known ([actions.ts:310-313](../src/actions.ts#L310-L313)). For `connect`, SIMPLEX_NAME is the target when it is a name; for `send`, the chat's verified SimpleX name.

A card of a `send` with an image or video file, or of a `download` of an image or video, is an image or video message with the file's preview and the card text as its caption; the preview is resized to fit the card, and the card is text when it can't be resized ([control.ts:481-501](../src/control.ts#L481-L501)).

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

DECIDER is `Performed without approval (mode host)` in mode `host`, `Approved for this chat by NAME` for a standing action, and `Approved by NAME` otherwise ([control.ts:594](../src/control.ts#L594)).

Command lines ([`commandsLine`](../src/control.ts#L643-L656)):

| Tool | Command line |
|---|---|
| `connect` | `/'approve N' or /'reject N'` |
| `send` | `/'approve CHAT N', /'approve CHAT all' or /'reject CHAT N'` |
| `download` | `/'receive CHAT N', /'receive CHAT N all', /'forward CHAT N' or /'reject CHAT N'`, without `/'receive CHAT N all'` for unknown servers |
| `leave` | `/'approve CHAT N' or /'reject CHAT N'` |

The app shows `/'approve N'` as the tappable command `/approve N` ([PLAN, What the owner sees](../PLAN.md#what-the-owner-sees-the-agent-control-chat)).

An action performed without a card, in mode `host` or under a standing approval, gets its outcome as a new message; a `download` without a card gets none ([control.ts:153-160](../src/control.ts#L153-L160), [:535-537](../src/control.ts#L535-L537)).

### 2.3 Other posts

| Situation | Text | Code |
|---|---|---|
| `send` with a refused file; logged too | ``Refused to send `PATH` to KIND NAME (CHAT): private keys and the server's own files are not sent.`` | [control.ts:162-166](../src/control.ts#L162-L166) |
| `sndFileError` of a sent file; logged too | ``The file `NAME` was not sent to KIND NAME (CHAT): `REASON`.`` | [control.ts:168-177](../src/control.ts#L168-L177), [serve.ts:160-162](../src/serve.ts#L160-L162) |
| `/forward` of a file that is not received or not posted | ``Action N: the file was not received: `ERROR`.`` | [control.ts:296](../src/control.ts#L296) |
| after the reply to `/leave CHAT`, and after the outcome of a done `leave`, when the folder of CHAT has files | ``Files from KIND NAME (CHAT) are kept in `FOLDER`: K files, SIZE. /'delete CHAT files'`` | [control.ts:158-159](../src/control.ts#L158-L159), [:311-319](../src/control.ts#L311-L319) |

### 2.4 Membership messages

Posted in the chat and logged; in the chat, names are shown by `safeName` and the `--owners ...` part is a code span; in the log, names are folded to one line ([control.ts:467-470](../src/control.ts#L467-L470), [:86-87](../src/control.ts#L86-L87)).

| Situation | Text | Code |
|---|---|---|
| owner joined, `--owners` absent | `I'm your agent. I ask here before I connect, send, download a file or leave a chat.` (chat only) | [control.ts:36](../src/control.ts#L36), [:415](../src/control.ts#L415) |
| member joined, `--owners` without a list | `NAME joined (member ID). Restart the server with --owners ENTRIES` | [control.ts:420-423](../src/control.ts#L420-L423) |
| start with `--owners` without a list and humans present | `A joined (member ID). B joined (member ID). Restart the server with --owners ENTRIES` | [control.ts:387-390](../src/control.ts#L387-L390) |
| a human became owner outside the list | `NAME (member ID) is now an owner and can't approve actions yet. Restart the server with --owners ENTRIES` | [control.ts:428-432](../src/control.ts#L428-L432) |
| a listed owner stopped being a human owner | `NAME (member ID) is no longer an owner. Restart the server with --owners ENTRIES`<br>none remains: `NAME (member ID) is no longer an owner. No owners remain: restart the server with --reset-owners --owners` | [control.ts:433-438](../src/control.ts#L433-L438) |

ENTRIES: `ID:NAME` of each human, or of each human owner, joined by commas; an entry with characters other than letters, digits, `_` and `-` is quoted for the shell ([control.ts:816-826](../src/control.ts#L816-L826)).

### 2.5 Group profile

| Field | Value | Code |
|---|---|---|
| display name | the agent's display name, updated at start when it differs | [control.ts:113-115](../src/control.ts#L113-L115) |
| full name | empty | [control.ts:552](../src/control.ts#L552) |
| description | `Supervise your AI agent here: approve or reject its connections and messages with one tap.` | [control.ts:553](../src/control.ts#L553) |
| history | on | [control.ts:554](../src/control.ts#L554) |
| full deletion | on: the content of a message deleted by the agent is replaced, in the agent's database and in the members' apps; set again at start when it is not on | [control.ts:18](../src/control.ts#L18), [:113-115](../src/control.ts#L113-L115), [:554](../src/control.ts#L554) |
| command menu | `/pending` "Pending actions"<br>`/list` "Chats and approvals"<br>`/leave` "Leave a chat", params `<chat ID>`<br>`/secrets` "Secrets"<br>updated at start when it differs | [control.ts:11-16](../src/control.ts#L11-L16), [:113-115](../src/control.ts#L113-L115) |

---

## 3. CLI

Binary: `simplex-mcp`, compiled `dist/cli.js` ([package.json:5-7](../package.json#L5-L7)). Help text: [cli.ts:10-40](../src/cli.ts#L10-L40).

```
simplex-mcp --help
simplex-mcp serve NAME [options]
simplex-mcp register NAME [options]
```

NAME: 1 to 32 characters of `[a-z0-9]` and single hyphens, starting with a letter or digit ([cli.ts:156-158](../src/cli.ts#L156-L158)).

### 3.1 `serve`

| Option | Value | Default | Rule | Code |
|---|---|---|---|---|
| `--data` | folder | `~/.simplex-mcp/NAME` | resolved to an absolute path; `dataGiven` is set | [cli.ts:71-74](../src/cli.ts#L71-L74) |
| `--display-name` | text | NAME when the user is created | must equal `validName(text)` | [cli.ts:75-77](../src/cli.ts#L75-L77), [:160-164](../src/cli.ts#L160-L164) |
| `--approve` | `requests` or `host` | `requests` | | [cli.ts:78-80](../src/cli.ts#L78-L80) |
| `--owners` | absent value, or `ID:NAME,...` | absent | the value is absent when the next argument is missing or starts with `--`; it is split at each comma followed by `ID:`, so a name may hold commas; entries match `^([1-9]\d*):(.+)$` | [cli.ts:81-83](../src/cli.ts#L81-L83), [:181-185](../src/cli.ts#L181-L185) |
| `--expire` | hours | 23 | integer 1 to 23 | [cli.ts:84-86](../src/cli.ts#L84-L86) |
| `--stop-wait` | seconds | 10 | integer 0 to 60 | [cli.ts:87-89](../src/cli.ts#L87-L89) |
| `--reset-owners` | flag | off | refused with an `--owners` list | [cli.ts:90-92](../src/cli.ts#L90-L92), [:109-111](../src/cli.ts#L109-L111) |
| `--new-token` | flag | off | | [cli.ts:93-95](../src/cli.ts#L93-L95) |
| `--port` | number | 5226 | integer 1 to 65535 | [cli.ts:96-98](../src/cli.ts#L96-L98) |
| `--qr-code` | `small` or `large` | `small` | | [cli.ts:99-101](../src/cli.ts#L99-L101) |
| `--secrets-folder` | folder | absent | resolved to an absolute path; refused when it is the data folder, inside it, or contains it, compared as resolved paths; created at start with mode 0700 when missing; absent: the secret commands are off | [cli.ts:102-104](../src/cli.ts#L102-L104), [:112-113](../src/cli.ts#L112-L113), [serve.ts:91](../src/serve.ts#L91), [files.ts:60-62](../src/files.ts#L60-L62) |

### 3.2 `register`

| Option | Value | Default | Code |
|---|---|---|---|
| `--scope` | `user` or `local` | `user` | [cli.ts:121-123](../src/cli.ts#L121-L123) |
| `--data` | folder | `~/.simplex-mcp/NAME` | [cli.ts:124-126](../src/cli.ts#L124-L126) |

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
| 0 | `--help` text on stdout; a stop after a signal | [cli.ts:208-210](../src/cli.ts#L208-L210), [serve.ts:55](../src/serve.ts#L55) |
| 2 | `Usage: simplex-mcp serve NAME [options] or simplex-mcp register NAME [options]. Run simplex-mcp --help.` | [cli.ts:50](../src/cli.ts#L50) |
| 2 | `NAME: 1 to 32 lowercase letters, digits and single hyphens, starting with a letter or digit.` | [cli.ts:51](../src/cli.ts#L51) |
| 2 | `OPTION: a value expected.` | [cli.ts:145](../src/cli.ts#L145) |
| 2 | `Unknown option OPTION. Run simplex-mcp --help.` | [cli.ts:198](../src/cli.ts#L198) |
| 2 | `--display-name: not a valid SimpleX name; try VALID` | [cli.ts:162](../src/cli.ts#L162) |
| 2 | `--approve: requests or host expected, got VALUE` | [cli.ts:167](../src/cli.ts#L167) |
| 2 | `--qr-code: small or large expected, got VALUE` | [cli.ts:172](../src/cli.ts#L172) |
| 2 | `--scope: user or local expected, got VALUE` | [cli.ts:177](../src/cli.ts#L177) |
| 2 | `--owners: ID:NAME expected, got ENTRY` | [cli.ts:183](../src/cli.ts#L183) |
| 2 | `--expire: hours from 1 to 23 expected, got VALUE` | [cli.ts:85](../src/cli.ts#L85), [:189](../src/cli.ts#L189) |
| 2 | `--stop-wait: seconds from 0 to 60 expected, got VALUE` | [cli.ts:88](../src/cli.ts#L88), [:189](../src/cli.ts#L189) |
| 2 | `--port: a port from 1 to 65535 expected, got VALUE` | [cli.ts:97](../src/cli.ts#L97), [:189](../src/cli.ts#L189) |
| 2 | `--reset-owners takes --owners without a list: member IDs change in a new agent control chat.` | [cli.ts:110](../src/cli.ts#L110) |
| 2 | `--secrets-folder: a folder disjoint from the data folder expected, got DIR` | [cli.ts:113](../src/cli.ts#L113) |
| 2 | `--owners: member ENTRY is not in the agent control chat. Start with --reset-owners --owners, or use the IDs of current members.` | [control.ts:396](../src/control.ts#L396) |
| 1 | `Another server uses DATA (PID N). If no server runs, remove DATA/lock.` | [files.ts:25-27](../src/files.ts#L25-L27), [:93-95](../src/files.ts#L93-L95) |
| 1 | `Port PORT is in use. Start with --port N.` | [serve.ts:231](../src/serve.ts#L231) |
| 1 | `Agent "NAME" was never started. Start it with: npx -y @simplex-chat/mcp serve NAME` | [register.ts:35](../src/register.ts#L35) |
| 1 | `Add this entry under LOCATION in ~/.claude.json: {"simplex-NAME": ENTRY}` | [register.ts:55-58](../src/register.ts#L55-L58) |
| 1 | `claude mcp add-json failed: STDERR` | [register.ts:42](../src/register.ts#L42) |
| 1 | `simplex-mcp: ERROR` for other errors, `stop: ERROR` for a failed stop | [cli.ts:224-225](../src/cli.ts#L224-L225), [serve.ts:56-59](../src/serve.ts#L56-L59) |

LOCATION is `"mcpServers"` for scope `user`, and `"projects" > "PROJECT" > "mcpServers"` for scope `local` ([register.ts:56](../src/register.ts#L56)).

### 3.4 Terminal lines

stdout ([log.ts:37-39](../src/log.ts#L37-L39)):

| Moment | Line | Code |
|---|---|---|
| start | `SimpleX MCP server VERSION, agent "NAME"`<br>`Data folder: DATA`<br>`Approval mode: MODE` | [serve.ts:74-76](../src/serve.ts#L74-L76) |
| control chat created | `Agent control chat "DISPLAY" created.` | [control.ts:558](../src/control.ts#L558) |
| link opened | `Join the agent control chat in the SimpleX app. The link works once:`<br>QR code<br>link<br>small QR code: `If the QR code does not scan, restart with --qr-code large, or paste the link into the app.`<br>role line | [control.ts:446-456](../src/control.ts#L446-L456) |
| listening | `Listening on http://127.0.0.1:PORT/mcp` | [serve.ts:128](../src/serve.ts#L128) |
| owner setup done | `Add the agent to Claude Code:`<br>`  npx -y @simplex-chat/mcp register NAME [--data DATA]` | [serve.ts:187-189](../src/serve.ts#L187-L189) |
| owner joined | `NAME joined (member ID) and is now an owner. The link is deleted.` | [control.ts:416](../src/control.ts#L416) |
| listed owners set | `Owners: ENTRIES.` | [control.ts:407](../src/control.ts#L407) |
| approvers gone, `--owners` absent | `No owners in the agent control chat.` | [control.ts:37](../src/control.ts#L37), [:205](../src/control.ts#L205), [:383](../src/control.ts#L383) |
| control chat lost | `The agent lost its control chat. Restart the server with --reset-owners.` | [control.ts:443](../src/control.ts#L443) |
| action created | `Action N: WHAT` with `, as itself` or `, incognito` for `connect` and ` (CHAT)` for the other tools | [actions.ts:180-181](../src/actions.ts#L180-L181), [:315-327](../src/actions.ts#L315-L327) |
| counter write or card post failed | `Action N: failed: ERROR.` | [actions.ts:260](../src/actions.ts#L260) |
| perform ended, not interrupted by the stop, mode `requests` | `Action N approved by NAME: RESULT` with RESULT `sent.`, `downloaded.`, `deleted.`, `left.`, `connected.`, `connection requested.` or `failed: ERROR.` | [actions.ts:281](../src/actions.ts#L281), [:333-341](../src/actions.ts#L333-L341) |
| perform ended, not interrupted by the stop, mode `host` | `Action N: performed without approval (mode host).`, or `...; failed: ERROR.` | [actions.ts:335](../src/actions.ts#L335) |
| rejected | `Action N rejected by NAME.` | [actions.ts:228](../src/actions.ts#L228) |
| expired | `Action N expired.` | [actions.ts:198](../src/actions.ts#L198) |
| `/leave CHAT` | `Left KIND NAME (CHAT), requested by NAME.` or `Deleted ...` | [control.ts:306](../src/control.ts#L306) |
| `/secret NAME VALUE` accepted | `Secret NAME set by NAME.`, without the value | [control.ts:349](../src/control.ts#L349) |
| `/unsecret NAME` accepted | `Secret NAME removed by NAME.` | [control.ts:355](../src/control.ts#L355) |
| refused file, file not sent | as in [2.3](#23-other-posts), names and paths on one line | [control.ts:164](../src/control.ts#L164), [:175](../src/control.ts#L175) |
| stop | `Cancelled K action.` or `Cancelled K actions.` when K > 0<br>`Stopping: sending K cancelled card(s), up to S s. Ctrl-C to stop now.` when cards were edited or posted and S > 0<br>`Stopped.` | [serve.ts:178](../src/serve.ts#L178), [:191-205](../src/serve.ts#L191-L205), [:182](../src/serve.ts#L182) |
| membership messages | see [2.4](#24-membership-messages) | |

stderr: `CONTEXT: ERROR` from `logError` ([log.ts:41-43](../src/log.ts#L41-L43)), with CONTEXT one of `action N`, `action N card`, `action N card image`, `action N file`, `control chat command`, `EVENT in the agent control chat`, `EVENT event`, `refused file`, `file not sent`, `download of file N`, `file N`, `moving file N`, `cancelling file N`, `resizing PATH`, `preview`, `MCP`, `HTTP request`, `stop`, `simplex-mcp`. The library's download lines are written to stderr.
