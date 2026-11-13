# SimpleX MCP server: API

> The six MCP tools, the status message, the agent control chat commands, replies and cards, and the CLI of `@simplex-chat/mcp` 0.3.0. The Claude Code plugin's tools, groups, channel, socket protocol, commands and files are in section 4.
>
> Related specs: [README](README.md) | [Architecture](architecture.md) | [Flows](flows.md)

**Source:** [`tools.ts`](../src/tools.ts) | [`control.ts`](../src/control.ts) | [`cli.ts`](../src/cli.ts) | [`messages.ts`](../src/messages.ts) | [`actions.ts`](../src/actions.ts) | [`status.ts`](../src/status.ts) | [`inference.ts`](../src/inference.ts) | [`session.ts`](../src/session.ts) | [`service.ts`](../src/service.ts) | [`hub.ts`](../src/hub.ts) | [`main-group.ts`](../src/main-group.ts) | [`hook.ts`](../src/hook.ts) | [`owner.ts`](../src/owner.ts) | [`protocol.ts`](../src/protocol.ts)

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
| Tools are registered on each per-request `McpServer` with description, input schema, output schema and, in mode `host`, `_meta: {"anthropic/requiresUserInteraction": true}` for `connect`, `send`, `download` and `leave` | [tools.ts:495-502](../src/tools.ts#L495-L502), [:508-518](../src/tools.ts#L508-L518), [:554-556](../src/tools.ts#L554-L556) |
| Tool descriptions and server instructions; the instructions name the agent's display name | [tools.ts:422-462](../src/tools.ts#L422-L462), [serve.ts:113](../src/serve.ts#L113) |
| Arguments are validated by the SDK; a failure is a result with `isError: true` and the text `Input validation error: Invalid arguments for tool NAME: ERRORS` | [mcp-BumlNux0.cjs:1812-1817](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L1812-L1817) |
| `additionalProperties: false` is set in every input schema | [tools.ts:266](../src/tools.ts#L266), [:284](../src/tools.ts#L284), [:296](../src/tools.ts#L296), [:299](../src/tools.ts#L299), [:307](../src/tools.ts#L307), [:318](../src/tools.ts#L318) |
| A result is sent as `structuredContent` and as its JSON in one text item | [tools.ts:562](../src/tools.ts#L562) |
| A tool error is sent as `isError: true` with one text item: the `ToolError` text, or the `errorText` of other errors | [tools.ts:564](../src/tools.ts#L564), [log.ts:45-49](../src/log.ts#L45-L49) |
| An aborted call ends with the result `Cancelled.`, `isError: true`, which the SDK discards | [tools.ts:504](../src/tools.ts#L504), [:516](../src/tools.ts#L516) |
| With a `progressToken` in `_meta`, `notifications/progress` is sent at the start of each wait and every 60 s, with `progress` 0, 1, 2, ... and a `message` | [tools.ts:1777-1787](../src/tools.ts#L1777-L1787), [messages.ts:305-309](../src/messages.ts#L305-L309) |
| The call is aborted when the client closes the response stream or sends `notifications/cancelled`; the action is kept | [src-BhnGYybN.cjs:6338-6341](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs#L6338-L6341), [:6425](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs#L6425) |
| A call with the same tool and action fields as an action in memory is attached to it; a `send` with `live: true` is never attached. The key is the JSON array of the session number, the tool and the fields, with the session number `undefined` for `serve` | [tools.ts:1769-1771](../src/tools.ts#L1769-L1771), [:763-765](../src/tools.ts#L763-L765), [:922](../src/tools.ts#L922), [actions.ts:176-178](../src/actions.ts#L176-L178) |
| Messages are marked read after the last wait of a call that is not aborted | [tools.ts:809](../src/tools.ts#L809), [:976](../src/tools.ts#L976), [:1320](../src/tools.ts#L1320) |

Action fields of the key:

| Tool | Fields |
|---|---|
| `connect` | `target`<br>`incognito` (default `false`)<br>`message` (default `null`) |
| `send` | `chat`<br>`text` (default `null`)<br>`file` (default `null`)<br>`as` (default `null`)<br>`reply_to` (default `null`)<br>`mention` (default `null`) |
| `download` | `chat`<br>`item` |
| `leave` | `chat` |

Result status of the four action tools ([`actionResult`](../src/tools.ts#L778-L790)):

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

Handler: [`AgentTools.connect`](../src/tools.ts#L749-L772). Perform: [`performConnect`](../src/tools.ts#L828-L861). Schemas: [input](../src/tools.ts#L256-L267), [output](../src/tools.ts#L324-L334).

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
| `known` | `status` and the chat fields of the known chat. The plugin's rules for a known chat are in [4.1](#41-tools-in-the-plugin) | [tools.ts:754](../src/tools.ts#L754), [:767-772](../src/tools.ts#L767-L772) |
| `done`, chat unknown | `action`, `status`, `type`, `connected: false`, plan fields `name`, `peer_type`, `description`, `welcome`, `commands`, `known_as` when present | [tools.ts:775](../src/tools.ts#L775), [:1529-1535](../src/tools.ts#L1529-L1535) |
| `done`, chat known | `action`, `status`, chat fields, `messages` | [tools.ts:792-812](../src/tools.ts#L792-L812) |
| `failed` | `action`, `status`, `error` | [tools.ts:783](../src/tools.ts#L783) |
| `rejected`, `expired` | `action`, `status` | [tools.ts:783](../src/tools.ts#L783) |

Chat fields ([`chatFields`](../src/tools.ts#L1385-L1412)):

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

`messages`: message items among the last 50 items of the chat when the result is built, replaced by their collected copies, followed by the items collected since the connection; live items are left out; new items are marked read ([tools.ts:794](../src/tools.ts#L794), [:804-811](../src/tools.ts#L804-L811)).

Tool errors:

| Text | Condition | Code |
|---|---|---|
| `Not a SimpleX link or name: TARGET` | whitespace in `target`, or `invalidConnReq` from `apiConnectPlan` | [tools.ts:816](../src/tools.ts#L816), [:822](../src/tools.ts#L822) |
| an app error text, for example `Name not found: This SimpleX name is not registered. Please check the name.`, or `SimpleX error: CHAIN: DETAILS` | other errors of `apiConnectPlan`, or a plan of type `error` | [tools.ts:823](../src/tools.ts#L823), [:1456](../src/tools.ts#L1456), [log.ts:51-53](../src/log.ts#L51-L53) |
| `TARGET can't be joined now: PLAN.` | `invitationLink` other than `ok` and `known`<br>`contactAddress` other than `ok`, `known`, `contactViaAddress` and `connectingConfirmReconnect`<br>`groupLink` other than `ok`, `known` and `connectingConfirmReconnect` | [tools.ts:1447](../src/tools.ts#L1447), [:1460](../src/tools.ts#L1460), [:1478](../src/tools.ts#L1478), [:1500](../src/tools.ts#L1500) |
| `message: TARGET is not a contact or business address with short link data, or its contact exists without a connection.` | `message` given and the plan has `takesMessage: false` | [tools.ts:756](../src/tools.ts#L756) |
| refusals | see [1.9](#19-action-refusals) | |

`takesMessage` is `true` for a contact address with short link data, for a known prepared contact, and for a known prepared business group ([tools.ts:1451](../src/tools.ts#L1451), [:1471](../src/tools.ts#L1471), [:1489](../src/tools.ts#L1489)).

### 1.3 `send`

Handler: [`AgentTools.send`](../src/tools.ts#L919-L942). Result: [`sendResult`](../src/tools.ts#L966-L984). Perform: [`performSend`](../src/tools.ts#L986-L1011). Note: [`showNote`](../src/tools.ts#L944-L951). Schemas: [input](../src/tools.ts#L269-L285), [output](../src/tools.ts#L336-L348).

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

File types by extension, lower-cased ([tools.ts:184-193](../src/tools.ts#L184-L193), [`attachmentMedia`](../src/tools.ts#L1727-L1738)):

| Extension | Message type | Sent file | Preview and duration |
|---|---|---|---|
| `.jpg`, `.jpeg`, `.png` | image | `core.resizeImage(..., Image)`, written to `DATA/files/sent/`; the original when resizing fails | `core.resizeImage(..., MessagePreview)`; the fixed preview when it fails |
| `.gif`, `.webp` | image | unchanged | the fixed preview |
| `.mp4`, `.mov` | video | unchanged | the first frame from `ffmpeg`, resized as a preview; duration from `ffprobe`, rounded; the fixed preview and 0 when they fail |
| other, or `as: "file"` | file | unchanged | |

The fixed preview is a 16 × 9 mid-gray PNG ([tools.ts:173](../src/tools.ts#L173)). `ffmpeg` and `ffprobe` run with `-protocol_whitelist file -f mov`, 30 s and 64 MB of output each ([tools.ts:1749-1767](../src/tools.ts#L1749-L1767)).

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

Only `status` is required by the output schema ([tools.ts:347](../src/tools.ts#L347)).

Tool errors, in the order of the checks:

| Text | Condition | Code |
|---|---|---|
| `text: required without file.` | neither `text` nor `file` | [tools.ts:924](../src/tools.ts#L924) |
| `as: applies only with file.` | `as` without `file` | [tools.ts:925](../src/tools.ts#L925) |
| `live: applies to text only; send file, reply_to and mention without live.` | `live` with `file`, `reply_to` or `mention` | [tools.ts:926](../src/tools.ts#L926) |
| `text: at most 2000 characters with live.` | `live` with `text` over 2000 code points | [tools.ts:927](../src/tools.ts#L927), [:170](../src/tools.ts#L170) |
| `No chat CHAT. list shows the agent's chats.` | invalid reference, the control chat, or a store error when the chat is read | [tools.ts:1349-1358](../src/tools.ts#L1349-L1358), [:1662-1664](../src/tools.ts#L1662-L1664) |
| `The agent is not connected to CHAT and can't post; list shows connected: false.` | the chat is not connected | [tools.ts:929-930](../src/tools.ts#L929-L930), [:1588](../src/tools.ts#L1588) |
| `The agent has role ROLE in CHAT and can't post.` | membership role `observer` or `relay` | [tools.ts:929-930](../src/tools.ts#L929-L930), [:1589-1592](../src/tools.ts#L1589-L1592) |
| `No chat CHAT. list shows the agent's chats.` | `live` while `leave` or `/leave` of the chat runs | [tools.ts:945-946](../src/tools.ts#L945-L946), [:954](../src/tools.ts#L954) |
| `live: CHAT is a channel or a chat with a bot or an agent; status messages are shown in chats with people, groups and business chats.` | `live` in a channel, or in a direct chat whose contact has peer type `bot` or `agent` | [tools.ts:955](../src/tools.ts#L955), [:1554-1561](../src/tools.ts#L1554-L1561) |
| `live: status messages are shown in mode requests only; this server runs in mode host.` | `live` in mode `host` | [tools.ts:956](../src/tools.ts#L956) |
| `live: The agent lost its control chat. The person who runs it has to restart it with --reset-owners.` | `live` after the control chat is lost | [tools.ts:957](../src/tools.ts#L957), [actions.ts:13](../src/actions.ts#L13) |
| `live: your owner's approval of all messages to CHAT is required; list shows approved: true for such chats.` | `live` in a chat without a standing approval of messages by a current approver | [tools.ts:958](../src/tools.ts#L958), [control.ts:167-170](../src/control.ts#L167-L170) |
| `The server is stopping.` | `live` after the stop started | [status.ts:55](../src/status.ts#L55), [actions.ts:14](../src/actions.ts#L14) |
| `live: the update limit of the status message of CHAT is reached; send your answer without live.` | `live` when the status message has 59 renders | [status.ts:58-59](../src/status.ts#L58-L59) |
| `reply_to: CHAT has no item N.` | the read of `around=N` fails, or the item is absent from it | [tools.ts:1363](../src/tools.ts#L1363) |
| `reply_to: item N in CHAT is not a message.` | the item's content is not `rcvMsgContent` or `sndMsgContent` | [tools.ts:1365](../src/tools.ts#L1365) |
| `mention: CHAT is a contact chat; mentions apply to groups and channels.` | `mention` with `@N` | [tools.ts:1371](../src/tools.ts#L1371) |
| `mention: CHAT has no member N.` | ID absent from `apiListMembers` | [tools.ts:1376](../src/tools.ts#L1376) |
| `mention: members N and M have the same name.` | two mentioned members with the same display name | [tools.ts:1379](../src/tools.ts#L1379) |
| `file: an absolute path is expected.`<br>`file: no file at FILE.`<br>`file: PATH is not a file.`<br>`file: PATH can't be read.` | the path, after symbolic links | [tools.ts:1680-1691](../src/tools.ts#L1680-L1691) |
| `file: PATH is refused: private keys and the server's own files are not sent.` | a file named `id_*`, `*.pem`, `*.key`, `*.p12` or `*.pfx`, or under `~/.ssh`, `~/.gnupg`, or the data folder or `~/.simplex-mcp` outside `DATA/files/`; each folder by its real path | [tools.ts:1027-1030](../src/tools.ts#L1027-L1030), [:1036-1041](../src/tools.ts#L1036-L1041) |
| `file: PATH is empty.` | size 0 | [tools.ts:1031](../src/tools.ts#L1031) |
| `text: too long for the message with its quote; shorten it.`<br>`text: too long for a message with this TYPE; shorten it.` | the message, as the core sends it, above 15602 bytes | [tools.ts:935](../src/tools.ts#L935), [:1635-1637](../src/tools.ts#L1635-L1637), [:1639-1655](../src/tools.ts#L1639-L1655) |
| refusals | see [1.9](#19-action-refusals) | |

The message is measured as 600 bytes plus the JSON of its content, the file's name, the quote and the mentioned names. The quote is the quoted text, or its file's name when the text is empty; a sent image or video quotes it as text, otherwise an image's preview is kept ([tools.ts:1639-1655](../src/tools.ts#L1639-L1655)).

The perform fails with `file: PATH changed after the request.` when the file's real path, size, inode or change time differs from the request ([tools.ts:1043-1048](../src/tools.ts#L1043-L1048)). A resized copy is deleted at `sndFileCompleteXFTP`, at `sndFileError` and after a failed send ([tools.ts:1008](../src/tools.ts#L1008), [:1013-1023](../src/tools.ts#L1013-L1023), [:705-707](../src/tools.ts#L705-L707)). After `apiSendMessages` returns, the status message of `chat` is deleted ([tools.ts:1002](../src/tools.ts#L1002), [1.10](#110-status-message)).

### 1.4 `receive`

Handler: [`AgentTools.receive`](../src/tools.ts#L1290-L1327). Schemas: [input](../src/tools.ts#L287-L297), [output](../src/tools.ts#L350-L358).

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

Tool errors: `No chat CHAT. list shows the agent's chats.` for an invalid reference, the control chat, or a store error of the first read, `initial` 1 ([tools.ts:1292](../src/tools.ts#L1292), [:1296-1298](../src/tools.ts#L1296-L1298)); other chat errors, a store error of a later page read included, as their error text ([log.ts:45-53](../src/log.ts#L45-L53)).

Messages are read by `receive` without an action or an approval.

When no message is new and `wait` is above 0, the status message of `chat` is deleted before the wait ([tools.ts:1306-1307](../src/tools.ts#L1306-L1307)). With `typing: true`, the status message is started after the returned messages are marked read, unless the call was aborted ([tools.ts:1319-1322](../src/tools.ts#L1319-L1322)).

### 1.5 `download`

Handler: [`AgentTools.download`](../src/tools.ts#L1130-L1150). Result: [`downloadResult`](../src/tools.ts#L1177-L1185). Perform: [`performDownload`](../src/tools.ts#L1187-L1191), [`downloadFile`](../src/tools.ts#L604-L631). Schemas: [input](../src/tools.ts#L310-L319), [output](../src/tools.ts#L405-L420).

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

FOLDER is `group-N` for `#N` and `contact-N` for `@N`; the extension is the file name's extension when it has 1 to 16 ASCII letters and digits ([messages.ts:215-227](../src/messages.ts#L215-L227)). `/forward` downloads a file to `DATA/forwarded/FOLDER/` under the same name; a later perform of the download moves it to the download path, after the copy for the post is made ([tools.ts:604-621](../src/tools.ts#L604-L621)).

Steps before the action:

| Step | Code |
|---|---|
| a file in status `rcvInvitation`, or an XFTP file: the file's servers from `apiGetChatItemInfo`; while none, the call waits for `rcvFileDescrReady` until `wait` seconds after the call, with the progress `The sender is uploading the file of item N in CHAT; waiting until TIME.` | [tools.ts:1152-1169](../src/tools.ts#L1152-L1169) |
| unknown servers: the file's servers that are not among the XFTP servers of `apiGetUserServers`, of no operator or of an enabled one, compared without their passwords | [tools.ts:1171-1175](../src/tools.ts#L1171-L1175) |
| without unknown servers, and for a file ID not in rejected files, the chat's files approval, read again, makes the action performed at once | [tools.ts:1145](../src/tools.ts#L1145) |

Perform: `apiReceiveFile(fileId, path, approvedRelays)`, for a file in status `rcvInvitation`. `approvedRelays` is true when the action's mode is `requests` and unknown servers are named on its card, and false otherwise. The core then checks the complete description ([tools.ts:627](../src/tools.ts#L627), [:1193-1202](../src/tools.ts#L1193-L1202)). The download ends at `rcvFileComplete`, `rcvFileError`, `rcvFileSndCancelled` or `chatItemUpdated` of its file, or at a check every 60 s ([tools.ts:643-665](../src/tools.ts#L643-L665), [:1204-1272](../src/tools.ts#L1204-L1272)).

Failure texts of the perform:

| Text | Condition | Code |
|---|---|---|
| the file's error, as in the file field | the file failed | [tools.ts:1233-1243](../src/tools.ts#L1233-L1243) |
| `The sender cancelled the file.` | `rcvFileSndCancelled` | [tools.ts:660-665](../src/tools.ts#L660-L665) |
| `The message or its chat was deleted.` | a check finds no message, contact or group | [tools.ts:178](../src/tools.ts#L178), [:1223-1231](../src/tools.ts#L1223-L1231) |
| `The download made no progress for 30 minutes and was cancelled.` | no rise of `rcvProgress` for 30 minutes; the file is cancelled | [tools.ts:182](../src/tools.ts#L182), [:1216-1220](../src/tools.ts#L1216-L1220) |
| `The received file is not in the agent's files folder.` | the file is received and neither the download path nor the forwarded path has a file | [messages.ts:14](../src/messages.ts#L14), [tools.ts:1245-1249](../src/tools.ts#L1245-L1249) |
| `Your owner deleted the files of this chat.` | `/delete CHAT files` | [tools.ts:179](../src/tools.ts#L179), [:1074-1092](../src/tools.ts#L1074-L1092) |
| `The file is on unknown servers that were not approved.` | a check reads an accepted file in status `rcvInvitation`: the core returned it to the offer when its description completed | [tools.ts:180](../src/tools.ts#L180), [:1212](../src/tools.ts#L1212) |
| `Unknown servers! Without Tor or VPN, your IP address will be visible to these XFTP relays: HOST, HOST.` | the core refuses the servers of the complete description at the acceptance | [log.ts:78-79](../src/log.ts#L78-L79) |

Tool errors:

| Text | Condition | Code |
|---|---|---|
| `No chat CHAT. list shows the agent's chats.` | invalid reference, the control chat, or a store error when the chat is read | [tools.ts:1135](../src/tools.ts#L1135) |
| `item: CHAT has no item N.` | the item is not found | [tools.ts:1137](../src/tools.ts#L1137) |
| `item: item N in CHAT has no received file.` | the item is not received or has no file | [tools.ts:1139](../src/tools.ts#L1139) |
| `item: the file of item N in CHAT failed: ERROR` | the file field has status `failed` | [tools.ts:1140-1141](../src/tools.ts#L1140-L1141) |
| `item: the sender has not uploaded the file of item N in CHAT yet; call download again later.` | no servers by the end of `wait`, also for a file invited without a description, whose servers the core does not list; no action is created | [tools.ts:1143](../src/tools.ts#L1143), [:1167](../src/tools.ts#L1167) |
| refusals | see [1.9](#19-action-refusals) | |

A file ID whose download returned `rejected` or `expired` shows status `rejected` until a download of it is done or the server restarts ([tools.ts:1182-1183](../src/tools.ts#L1182-L1183)).

### 1.6 `list`

Handler: [`AgentTools.listChats`](../src/tools.ts#L1329-L1337). Schemas: [input](../src/tools.ts#L299), [output](../src/tools.ts#L360-L392).

Input: an empty object.

Output:

| Field | Item fields |
|---|---|
| `chats` | `chat`<br>`type`<br>`name`<br>`simplex_name`, when verified<br>`peer_type`, when present<br>`incognito`<br>`known_as`, when incognito<br>`connected`<br>`unread`: `chatStats.unreadCount`<br>`approved: true`, only under a standing approval of messages in mode `requests` |
| `actions` | `action`<br>`tool`<br>`input`: the input of the creating call<br>`state`<br>`open`: `true` while a call waits on it |

`chats`: the contacts and groups of the main conversations from one read, `apiGetChats(USER, last 1000000)`, except the control chat. In the plugin, only the chats of the calling session are listed ([tools.ts:1339-1347](../src/tools.ts#L1339-L1347), [:594-596](../src/tools.ts#L594-L596)). `actions`: the actions in memory of the calling session other than its mode and tool-use cards, or all actions for `serve` ([tools.ts:1335](../src/tools.ts#L1335)).

### 1.7 `leave`

Handler: [`AgentTools.leave`](../src/tools.ts#L1100-L1108). Perform: [`leaveChat`](../src/tools.ts#L1114-L1128). Schemas: [input](../src/tools.ts#L301-L308), [output](../src/tools.ts#L394-L403).

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

The chat is marked as being left from before its status message is deleted until these methods end; the status message is deleted first ([tools.ts:1115-1117](../src/tools.ts#L1115-L1117), [:1124-1126](../src/tools.ts#L1124-L1126)).

Tool errors: `No chat CHAT. list shows the agent's chats.` when no action with the same key is in memory and the chat is invalid, the control chat or unknown ([tools.ts:1103-1105](../src/tools.ts#L1103-L1105)); refusals.

### 1.8 Message, file and command objects

Message ([`Message`](../src/messages.ts#L16-L28), [`toMessage`](../src/messages.ts#L174-L192), [schema](../src/tools.ts#L195-L228)):

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

Only received message items (`rcvMsgContent`) are returned by `send` and `receive`; `connect` also returns sent ones (`sndMsgContent`) ([messages.ts:101](../src/messages.ts#L101), [tools.ts:804](../src/tools.ts#L804)).

Command ([`commandsOf`](../src/tools.ts#L1617-L1624), [schema](../src/tools.ts#L230-L240)):

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

### 1.10 Status message

A status message is one live message of the agent in a chat, edited while the agent works and deleted afterwards ([`Statuses`](../src/status.ts#L28-L163)).

Eligibility, all of ([`showsStatus`](../src/tools.ts#L962-L964)):

| Condition | Code |
|---|---|
| the agent can post: the chat is connected and, in a group, the agent's role is other than `observer` and `relay` | [`postingRefusal`](../src/tools.ts#L1587-L1594) |
| the chat is not being left by `leave` or `/leave` | [tools.ts:954](../src/tools.ts#L954) |
| the chat is a direct chat with a contact of peer type other than `bot` and `agent`, a group or a business chat | [tools.ts:955](../src/tools.ts#L955), [:1554-1561](../src/tools.ts#L1554-L1561) |
| mode `requests` | [tools.ts:956](../src/tools.ts#L956) |
| the control chat is not lost | [tools.ts:957](../src/tools.ts#L957) |
| a standing approval of messages to the chat, by a current approver | [tools.ts:958](../src/tools.ts#L958), [control.ts:167-170](../src/control.ts#L167-L170) |

The chat and its eligibility are read again at each tick, the first tick included ([tools.ts:546-548](../src/tools.ts#L546-L548), [status.ts:117-123](../src/status.ts#L117-L123)). For a `send` with `live: true`, they are checked with the chat read of the call ([1.3](#13-send)).

Start:

| Event | Effect | Code |
|---|---|---|
| `receive` with `typing: true` returns messages, from any sender; the chat has no status, and the stop has not started | a status is created, and its first tick runs at once; when the chat is not eligible at that tick, the status ends without a post | [tools.ts:1322](../src/tools.ts#L1322), [status.ts:48-52](../src/status.ts#L48-L52) |
| `send` with `live: true` passes its checks; the chat has no status, or one created at least 23 hours earlier, which is deleted first | a status is created with the note and rendered at once; its next tick is in 3 s | [status.ts:54-64](../src/status.ts#L54-L64) |

Text ([`statusText`](../src/status.ts#L165-L169)):

| Part | Value |
|---|---|
| line of renders 1 to 49 | `FRAME LABEL · M:SS`; FRAME is `◐`, `◓`, `◑`, `◒` in turn, from `◐`; M:SS is the time since the status was created |
| line of render 50 and later | `◌ working since YYYY-MM-DD HH:MM UTC`, the time the status was created ([messages.ts:337-339](../src/messages.ts#L337-L339)) |
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
- Every tool call is registered as running from its start to its end ([tools.ts:559](../src/tools.ts#L559), [:565-567](../src/tools.ts#L565-L567), [status.ts:38-46](../src/status.ts#L38-L46)).
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
| `send` without `live` in the chat, after `apiSendMessages` of its perform returns | [tools.ts:1002](../src/tools.ts#L1002) |
| `receive` of the chat, when nothing is new and `wait` is above 0 | [tools.ts:1306-1307](../src/tools.ts#L1306-L1307) |
| `leave` or `/leave` of the chat, before the chat is deleted | [tools.ts:1117](../src/tools.ts#L1117) |
| a tick: the chat is not eligible, or its read fails | [status.ts:117-123](../src/status.ts#L117-L123) |
| a tick: 23 hours after the status was created | [status.ts:123](../src/status.ts#L123) |
| `send` with `live: true`, 23 hours or more after the status was created; a new status is created with the note | [status.ts:57](../src/status.ts#L57) |
| a tick: 50 renders or more, no running call other than `receive`, and 30 minutes since a call last started or ended | [status.ts:122-123](../src/status.ts#L122-L123) |
| a failed send or update of the status | [status.ts:102](../src/status.ts#L102), [:111-114](../src/status.ts#L111-L114) |
| the stop | [status.ts:76-82](../src/status.ts#L76-L82), [serve.ts:154](../src/serve.ts#L154) |

The deletion is `apiDeleteChatItems` of the item with mode `broadcast`, sent only when the item was posted ([status.ts:66-74](../src/status.ts#L66-L74), [api.ts:1178-1213](../../simplex-chat-nodejs/src/api.ts#L1178-L1213)).

At start, before the HTTP listener, each eligible chat is read in pages of 100 items from the last one, until a page is short or starts with an item older than 30 h − 10 min; the agent's own live message items younger than that and not marked deleted are deleted with mode `broadcast` ([serve.ts:111](../src/serve.ts#L111), [tools.ts:1283-1288](../src/tools.ts#L1283-L1288), [status.ts:84-99](../src/status.ts#L84-L99)).

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

The server reads commands from members' messages in the main conversation of the control chat ([serve.ts:134](../src/serve.ts#L134), [control.ts:216-226](../src/control.ts#L216-L226)). It answers a command with a reply that quotes it, one command after another, and it runs the expiry check before each command ([control.ts:224](../src/control.ts#L224), [:247](../src/control.ts#L247)).

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
| `delete CHAT files` | the chat's downloads in memory fail and are cancelled; its other files in status `rcvAccepted`, `rcvTransfer` or `rcvWarning`, such as downloads the core continues after a restart, are cancelled; the folder of CHAT and its forwarded folder are deleted, for a current or a left chat | `Deleted K files from CHAT, SIZE.` (`1 file`), or `No files from CHAT.` | [control.ts:357-360](../src/control.ts#L357-L360), [tools.ts:1074-1092](../src/tools.ts#L1074-L1092), [messages.ts:275-283](../src/messages.ts#L275-L283) |
| `pending` | no pending action with a card | `No pending actions.` | [control.ts:254](../src/control.ts#L254) |
| `pending` | otherwise | one line per action: `N  WHAT  expires YYYY-MM-DD HH:MM UTC` | [control.ts:253](../src/control.ts#L253), [:752-754](../src/control.ts#L752-L754) |
| `list` | no chats | `No chats.` | [control.ts:405](../src/control.ts#L405) |
| `list` | otherwise | one line per chat of the 100 most recent: `CHAT  KIND NAME`, then `, approved for this chat by APPROVER`, `, files approved by APPROVER` and `, not connected` when applicable, cut to 100 bytes with `…`; then `K older chats are not shown.` (`1 older chat is not shown.`) | [control.ts:393-406](../src/control.ts#L393-L406) |

KIND is `contact`, `bot`, `agent`, `business`, `group` or `channel` ([tools.ts:1554-1561](../src/tools.ts#L1554-L1561)). WHAT is `connect to SUBJECT`, `send to SUBJECT`, `download NAME from SUBJECT`, `delete SUBJECT` or `leave SUBJECT` ([actions.ts:382-396](../src/actions.ts#L382-L396)). SIZE is in bytes, KB, MB or GB, by 1000, with one decimal ([control.ts:810-813](../src/control.ts#L810-L813)). Names are shown by `safeName` ([control.ts:827-830](../src/control.ts#L827-L830)).

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

In a plugin session group, the core can refuse a card with `feature not allowed NAME` ([control.ts:521-544](../src/control.ts#L521-L544), [:194](../src/control.ts#L194), [:756-760](../src/control.ts#L756-L760), [tools.ts:1671-1678](../src/tools.ts#L1671-L1678), [actions.ts:338-349](../src/actions.ts#L338-L349), [:213](../src/actions.ts#L213), [:266](../src/actions.ts#L266)):
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

The app shows `/'approve N'` as the tappable command `/approve N` ([PLAN, What the owner sees](../../../plans/2026-10-19-mcp-server.md#what-the-owner-sees-the-agent-control-chat)).

An action performed without a card, in mode `host` or under a standing approval, gets its outcome as a new message; a `download` without a card gets none ([control.ts:190-197](../src/control.ts#L190-L197), [:601-603](../src/control.ts#L601-L603)).

### 2.3 Other posts

| Situation | Text | Code |
|---|---|---|
| `send` with a refused file; logged too | ``Refused to send `PATH` to KIND NAME (CHAT): private keys and the server's own files are not sent.`` | [control.ts:199-203](../src/control.ts#L199-L203) |
| `sndFileError` of a sent file; logged too | ``The file `NAME` was not sent to KIND NAME (CHAT): `REASON`.`` | [control.ts:205-214](../src/control.ts#L205-L214), [serve.ts:138](../src/serve.ts#L138) |
| `/forward` of a file that is not received or not posted | ``Action N: the file was not received: `ERROR`.`` | [control.ts:328-331](../src/control.ts#L328-L331) |
| `/forward` in a plugin session group, when the core refuses the post with `feature not allowed NAME`, as for `Files and media` or `Voice messages` | `Action N: NAME are off in the group of this session; its owners can turn them on in the group's preferences.` | [control.ts:328-331](../src/control.ts#L328-L331), [tools.ts:1671-1678](../src/tools.ts#L1671-L1678) |
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

Binary: `simplex-mcp`, compiled `dist/cli.js` ([package.json:5-7](../package.json#L5-L7)). Help text: [cli.ts:14-57](../src/cli.ts#L14-L57).

```
simplex-mcp --help
simplex-mcp serve NAME [options]
simplex-mcp register NAME [options]
```

The plugin's commands are in [4.7](#47-plugin-commands).

NAME: 1 to 32 characters of `[a-z0-9]` and single hyphens, starting with a letter or digit ([cli.ts:179-181](../src/cli.ts#L179-L181)).

### 3.1 `serve`

| Option | Value | Default | Rule | Code |
|---|---|---|---|---|
| `--data` | folder | `~/.simplex-mcp/NAME` | resolved to an absolute path; `dataGiven` is set | [cli.ts:93-96](../src/cli.ts#L93-L96) |
| `--display-name` | text | NAME when the user is created | must equal `validName(text)` | [cli.ts:97-99](../src/cli.ts#L97-L99), [:183-187](../src/cli.ts#L183-L187) |
| `--approve` | `requests` or `host` | `requests` | | [cli.ts:100-102](../src/cli.ts#L100-L102) |
| `--owners` | absent value, or `ID:NAME,...` | absent | the value is absent when the next argument is missing or starts with `--`; it is split at each comma followed by `ID:`, so a name may hold commas; entries match `^([1-9]\d*):(.+)$` | [cli.ts:103-105](../src/cli.ts#L103-L105), [:210-214](../src/cli.ts#L210-L214) |
| `--expire` | hours | 23 | integer 1 to 23 | [cli.ts:106-108](../src/cli.ts#L106-L108) |
| `--stop-wait` | seconds | 10 | integer 0 to 60 | [cli.ts:109-111](../src/cli.ts#L109-L111) |
| `--reset-owners` | flag | off | refused with an `--owners` list | [cli.ts:112-114](../src/cli.ts#L112-L114), [:131-133](../src/cli.ts#L131-L133) |
| `--new-token` | flag | off | | [cli.ts:115-117](../src/cli.ts#L115-L117) |
| `--port` | number | 5226 | integer 1 to 65535 | [cli.ts:118-120](../src/cli.ts#L118-L120) |
| `--qr-code` | `small` or `large` | `small` | | [cli.ts:121-123](../src/cli.ts#L121-L123) |
| `--inference` | URL | absent | an `http` or `https` URL without user name and password; kept as its origin and path, trailing slashes removed; refused with `--approve host` | [cli.ts:124-126](../src/cli.ts#L124-L126), [:134-136](../src/cli.ts#L134-L136), [:204-208](../src/cli.ts#L204-L208) |

With `--inference`, `SIMPLEX_MCP_INFERENCE_KEY` is read before the data folder is prepared. A set key is accepted with 1 or more visible ASCII characters (0x21 to 0x7e) only, and it is sent as `Authorization: Bearer KEY` to URL only; the proxies of `/running` are polled without it ([serve.ts:68](../src/serve.ts#L68), [:185-189](../src/serve.ts#L185-L189), [inference.ts:66](../src/inference.ts#L66), [:111](../src/inference.ts#L111)). The polls are described in [Architecture 2.12](architecture.md#212-inferencets).

### 3.2 `register`

| Option | Value | Default | Code |
|---|---|---|---|
| `--scope` | `user` or `local` | `user` | [cli.ts:144-146](../src/cli.ts#L144-L146) |
| `--data` | folder | `~/.simplex-mcp/NAME` | [cli.ts:147-149](../src/cli.ts#L147-L149) |

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
| 0 | `--help` text on stdout; a stop after a signal | [cli.ts:237-239](../src/cli.ts#L237-L239), [serve.ts:55](../src/serve.ts#L55) |
| 2 | `Usage: simplex-mcp serve NAME [options] or simplex-mcp register NAME [options]. Run simplex-mcp --help.` | [cli.ts:72](../src/cli.ts#L72) |
| 2 | `NAME: 1 to 32 lowercase letters, digits and single hyphens, starting with a letter or digit.` | [cli.ts:73](../src/cli.ts#L73) |
| 2 | `OPTION: a value expected.` | [cli.ts:168](../src/cli.ts#L168) |
| 2 | `Unknown option OPTION. Run simplex-mcp --help.` | [cli.ts:227](../src/cli.ts#L227) |
| 2 | `--display-name: not a valid SimpleX name; try VALID` | [cli.ts:185](../src/cli.ts#L185) |
| 2 | `--approve: requests or host expected, got VALUE` | [cli.ts:190](../src/cli.ts#L190) |
| 2 | `--qr-code: small or large expected, got VALUE` | [cli.ts:195](../src/cli.ts#L195) |
| 2 | `--scope: user or local expected, got VALUE` | [cli.ts:200](../src/cli.ts#L200) |
| 2 | `--owners: ID:NAME expected, got ENTRY` | [cli.ts:212](../src/cli.ts#L212) |
| 2 | `--expire: hours from 1 to 23 expected, got VALUE` | [cli.ts:107](../src/cli.ts#L107), [:218](../src/cli.ts#L218) |
| 2 | `--stop-wait: seconds from 0 to 60 expected, got VALUE` | [cli.ts:110](../src/cli.ts#L110), [:218](../src/cli.ts#L218) |
| 2 | `--port: a port from 1 to 65535 expected, got VALUE` | [cli.ts:119](../src/cli.ts#L119), [:218](../src/cli.ts#L218) |
| 2 | `--reset-owners takes --owners without a list: member IDs change in a new agent control chat.` | [cli.ts:132](../src/cli.ts#L132) |
| 2 | `--inference: an http or https URL without user name and password expected, got VALUE` | [cli.ts:206](../src/cli.ts#L206) |
| 2 | `--inference: status messages are shown only in mode requests.` | [cli.ts:135](../src/cli.ts#L135) |
| 2 | `SIMPLEX_MCP_INFERENCE_KEY: visible ASCII characters expected.`, with `--inference`, for a set key, empty or with a character outside 0x21 to 0x7e, before the data folder is prepared | [serve.ts:68](../src/serve.ts#L68), [:187](../src/serve.ts#L187) |
| 2 | `--owners: member ENTRY is not in the agent control chat. Start with --reset-owners --owners, or use the IDs of current members.` | [control.ts:436](../src/control.ts#L436) |
| 1 | `Another server uses DATA (PID N). If no server runs, remove DATA/lock.` | [files.ts:112-113](../src/files.ts#L112-L113), [:178-180](../src/files.ts#L178-L180) |
| 1 | `Port PORT is in use. Start with --port N.` | [serve.ts:202](../src/serve.ts#L202) |
| 1 | `Agent "NAME" was never started. Start it with: npx -y @simplex-chat/mcp serve NAME` | [register.ts:35](../src/register.ts#L35) |
| 1 | `Add this entry under LOCATION in ~/.claude.json: {"simplex-NAME": ENTRY}` | [register.ts:55-58](../src/register.ts#L55-L58) |
| 1 | `claude mcp add-json failed: STDERR` | [register.ts:42](../src/register.ts#L42) |
| 1 | `simplex-mcp: ERROR` for other errors, `stop: ERROR` for a failed stop | [cli.ts:275-276](../src/cli.ts#L275-L276), [serve.ts:56-59](../src/serve.ts#L56-L59) |

LOCATION is `"mcpServers"` for scope `user`, and `"projects" > "PROJECT" > "mcpServers"` for scope `local` ([register.ts:56](../src/register.ts#L56)).

### 3.4 Terminal lines

stdout ([log.ts:37-39](../src/log.ts#L37-L39)):

| Moment | Line | Code |
|---|---|---|
| start | `SimpleX MCP server VERSION, agent "NAME"`<br>`Data folder: DATA`<br>`Approval mode: MODE`<br>`Inference endpoint: URL` with `--inference` | [serve.ts:75-78](../src/serve.ts#L75-L78) |
| control chat created | `Agent control chat "DISPLAY" created.` | [control.ts:622](../src/control.ts#L622) |
| link opened | `Join the agent control chat in the SimpleX app. The link works once:`<br>QR code<br>link<br>small QR code: `If the QR code does not scan, restart with --qr-code large, or paste the link into the app.`<br>role line | [control.ts:486-496](../src/control.ts#L486-L496) |
| listening | `Listening on http://127.0.0.1:PORT/mcp` | [serve.ts:127](../src/serve.ts#L127) |
| owner setup done | `Add the agent to Claude Code:`<br>`  npx -y @simplex-chat/mcp register NAME [--data DATA]` | [serve.ts:163-165](../src/serve.ts#L163-L165) |
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
| `send` with `live: true` shown | `Note to KIND NAME (CHAT): TEXT`, or `an unnamed KIND`; the name and the text folded to one line | [tools.ts:949](../src/tools.ts#L949), [actions.ts:377-380](../src/actions.ts#L377-L380) |
| `/leave CHAT` | `Left KIND NAME (CHAT), requested by NAME.` or `Deleted ...` | [control.ts:342](../src/control.ts#L342) |
| refused file, file not sent | as in [2.3](#23-other-posts), names and paths on one line | [control.ts:201](../src/control.ts#L201), [:212](../src/control.ts#L212) |
| stop | `Cancelled K action.` or `Cancelled K actions.` when K > 0<br>`Stopping: sending PARTS, up to S s. Ctrl-C to stop now.` when PARTS is not empty and S > 0; PARTS is `K cancelled card(s)` when cards were edited or posted and the control chat is not lost, and `M status deletion(s)` when status messages had a render, joined by ` and `<br>`Stopped.` | [serve.ts:153-155](../src/serve.ts#L153-L155), [:167-183](../src/serve.ts#L167-L183), [:158](../src/serve.ts#L158) |
| membership messages | see [2.4](#24-membership-messages) | |

In the plugin's lines with `failed: ERROR.`, logged for the actions of a session, the last period is added only when ERROR does not end with one ([actions.ts:427-429](../src/actions.ts#L427-L429)). In `serve`, a period is added after every ERROR of these lines, and after every REASON of the line of a file not sent, as in 0.2.

stderr: `CONTEXT: ERROR` from `logError` ([log.ts:41-43](../src/log.ts#L41-L43)), with CONTEXT one of `action N`, `action N card`, `action N card image`, `action N file`, `control chat command`, `EVENT in the agent control chat`, `EVENT event`, `refused file`, `file not sent`, `download of file N`, `file N`, `moving file N`, `cancelling file N`, `resizing PATH`, `preview`, `MCP`, `HTTP request`, `stop`, `simplex-mcp`, `status of CHAT`, `inference endpoint`, `status messages`. The library's download lines are written to stderr.

| CONTEXT | Error | Code |
|---|---|---|
| `status of CHAT` | a failed send, update or deletion of a status message; a failed eligibility read of a current status; a failed tick; a failed leftover deletion | [status.ts:51](../src/status.ts#L51), [:97](../src/status.ts#L97), [:111-114](../src/status.ts#L111-L114), [:117-118](../src/status.ts#L117-L118), [:141](../src/status.ts#L141) |
| `inference endpoint` | the first failed poll after the start or after a successful poll | [status.ts:157-159](../src/status.ts#L157-L159) |
| `status messages` | a failed chat list before the leftover deletion | [serve.ts:111](../src/serve.ts#L111) |

A poll error is `GET URL: REASON`, folded to one line, with REASON `HTTP N`, `invalid JSON`, `body over 1 MiB`, `no answer within 1 s`, or the message of the first error aggregated in the error's cause, of the cause, or of the error itself, in this order ([inference.ts:109-141](../src/inference.ts#L109-L141)).

---

## 4. Claude Code plugin

### 4.1 Tools in the plugin

| Rule | Code |
|---|---|
| The six tools of [1](#1-tools) and `reply` are registered on the session's `McpServer`. Claude Code names them `mcp__plugin_simplex_simplex__NAME` | [session.ts:57](../src/session.ts#L57), [hooks.json:4](../hooks/hooks.json#L4) |
| The four action tools are registered with `_meta: {"anthropic/requiresUserInteraction": true}` when the option `approve` is `host` at the start of the session process | [session.ts:57](../src/session.ts#L57), [tools.ts:514](../src/tools.ts#L514) |
| Instructions: those of [1.1](#11-common-rules), with the display name `Claude` and the host `SimpleX service`, followed by the channel instructions of [4.5](#45-channel) | [session.ts:21](../src/session.ts#L21), [:52](../src/session.ts#L52), [tools.ts:451-462](../src/tools.ts#L451-L462) |
| `send`, `receive`, `download`, `leave` and `list` see only the session's own chats. For another session's chat, `send`, `receive`, `download` and `leave` return `No chat CHAT. list shows the agent's chats.` | [tools.ts:594-596](../src/tools.ts#L594-L596), [:1349-1358](../src/tools.ts#L1349-L1358) |
| At `connect`, a known chat without an owner becomes the session's chat. `connect` to a chat of another session returns the tool error `The chat of TARGET belongs to another session; an admin or owner of that session's group can give it to this session with /give.` | [tools.ts:767-772](../src/tools.ts#L767-L772) |
| Actions are numbered from 1 in each session group. The session number is part of the action key, see [1.1](#11-common-rules) | [hub.ts:75](../src/hub.ts#L75), [tools.ts:1769-1771](../src/tools.ts#L1769-L1771) |
| Downloads are saved under `ROOT/_sessions/ID/files/` | [hub.ts:90](../src/hub.ts#L90), [messages.ts:225-227](../src/messages.ts#L225-L227) |
| A run is not interactive when `CLAUDE_CODE_SESSION_ATTENDED` is `0`, which Claude Code sets for the hooks of a run nobody attends, or when `CLAUDE_CODE_ENTRYPOINT` is `sdk-cli`, which Claude Code sets for `claude -p`, or `sdk-ts` or `sdk-py`, for the Agent SDK. Such a run has no session. Its session process does not connect to the service, and every tool returns `This Claude Code run is not interactive, so it has no SimpleX group; the SimpleX tools work in interactive sessions.`, even when an option is invalid. Its hooks return `{}`, and the turn hooks send no request | [config.ts:22-32](../src/config.ts#L22-L32), [session.ts:15](../src/session.ts#L15), [:45-51](../src/session.ts#L45-L51), [:66](../src/session.ts#L66), [hook.ts:82-85](../src/hook.ts#L82-L85), [:115](../src/hook.ts#L115) |
| A run with `CLAUDE_CODE_SESSION_ATTENDED` `0` and an entrypoint other than those three is, for example, a `claude -p` run started by a command of a VS Code or desktop-app session. Its SessionStart hook starts the service and sends `/title` with `unattended` for its `CLAUDE_PID`. The session server of that run gets the values of the attended session, `1` and its entrypoint. The service refuses it with status 403 when it registers from that process within 10 s of the mark. The refused server then answers every call with the same text, relays no permission request and registers no more | [hook.ts:102-112](../src/hook.ts#L102-L112), [service.ts:237-248](../src/service.ts#L237-L248), [session.ts:79-80](../src/session.ts#L79-L80), [:92](../src/session.ts#L92), [:115](../src/session.ts#L115), [:127](../src/session.ts#L127), [:162-165](../src/session.ts#L162-L165) |

`reply` ([`replyTool`](../src/tools.ts#L472-L493), [`AgentTools.reply`](../src/tools.ts#L730-L747)):

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

Sources: [tools.ts:730-747](../src/tools.ts#L730-L747), [:1671-1678](../src/tools.ts#L1671-L1678), [:564](../src/tools.ts#L564), [hub.ts:93](../src/hub.ts#L93). The core checks the preference at the send, and only owners change it. Claude changes only its own fields of a group's profile, so the owners' preferences stay ([control.ts:115-140](../src/control.ts#L115-L140), [:632-640](../src/control.ts#L632-L640), [main-group.ts:57-66](../src/main-group.ts#L57-L66)). The profile updates are in [4.2](#42-main-group) and [4.3](#43-session-groups).

### 4.2 Main group

Commands from any member, one line `/KEYWORD PARAMS` as in [2.1](#21-commands-and-replies) ([`MainGroup.command`](../src/main-group.ts#L111-L115)):

| Request | Condition | Reply | Code |
|---|---|---|---|
| `/sessions` | no session | `No sessions.` | [hub.ts:431-434](../src/hub.ts#L431-L434) |
| `/sessions` | otherwise | one line per session of the hub: `N · NAME`, with `, closed` when the service has no entry for it | [hub.ts:431-434](../src/hub.ts#L431-L434) |
| `/join N` | no session N | `No session N. /sessions lists the sessions.` | [hub.ts:438](../src/hub.ts#L438) |
| `/join N` | the sender is a member of its group | `You are a member of "N · NAME".` | [hub.ts:439-444](../src/hub.ts#L439-L444) |
| `/join N` | otherwise: the sender is invited through the Main group, as `owner` when the sender is an owner of the Main group, else as `admin` | `Sent you an invitation to "N · NAME".` | [hub.ts:440](../src/hub.ts#L440), [:217-219](../src/hub.ts#L217-L219) |
| other one-line `/` text | | `Unknown command.` | [main-group.ts:14](../src/main-group.ts#L14), [:114](../src/main-group.ts#L114) |

Posts:

| Moment | Text | Code |
|---|---|---|
| the first owner joined | `I'm Claude. Each Claude Code session has its own group, and you get its invitation here. /sessions lists the sessions; /join N joins the group of session N.` | [main-group.ts:13](../src/main-group.ts#L13), [:138](../src/main-group.ts#L138) |
| each `joinedGroupMember` or `connectedToGroupMember` of a human who is found among the members read again. For the first owner, it is posted after the greeting | `NAME joined. They can join the group of any Claude Code session here, prompt Claude and approve its actions.` | [main-group.ts:17](../src/main-group.ts#L17), [:127-128](../src/main-group.ts#L127-L128) |

The session groups stay as they are when the membership or a role in the Main group is changed, and when the Main group is replaced by a reset of the owners. Members invited through the old Main group keep their roles in the session groups, with the rights of those roles ([main-group.ts:117-132](../src/main-group.ts#L117-L132), [:99-109](../src/main-group.ts#L99-L109), [hub.ts:312-314](../src/hub.ts#L312-L314)).

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
| `/list`, `/revoke CHAT`, `/leave CHAT` | | only the chats of this session, even while the session is closed | [tools.ts:1339-1358](../src/tools.ts#L1339-L1358) |
| `/give CHAT N` in the control chat of `serve` | any sender | the help reply, as in 0.2 | [control.ts:249](../src/control.ts#L249), [:857](../src/control.ts#L857) |
| `/observer #G ID` | #G is not this group | `#G is not this group.` | [control.ts:376](../src/control.ts#L376) |
| `/observer #G ID` | member ID is neither current nor pending, as it is absent, gone, or invited and not joined yet | `No member ID in this group.` | [control.ts:377-379](../src/control.ts#L377-L379), [tools.ts:1600-1615](../src/tools.ts#L1600-L1615) |
| `/observer #G ID` | ID is Claude, the sender, a relay or a member pending approval or review | `NAME's role can't be changed.` | [control.ts:380-382](../src/control.ts#L380-L382) |
| `/observer #G ID` | the sender's role is below admin or below the member's role, as SimpleX requires for a role change | `Your role does not let you change the role of NAME.` | [control.ts:383](../src/control.ts#L383), [:880-882](../src/control.ts#L880-L882) |
| `/observer #G ID` | accepted: the member's role is set to `observer`, and the members are read again. The standing approvals that this member gave are kept in the chat's custom data, and are ignored until the member is an admin or owner again | `NAME is an observer and can't send messages.` | [control.ts:384-386](../src/control.ts#L384-L386), [:888-891](../src/control.ts#L888-L891), [:167-170](../src/control.ts#L167-L170) |
| one-line `/` text outside the requests | an admin or owner | `Unknown command.` | [control.ts:20](../src/control.ts#L20), [:249](../src/control.ts#L249) |
| any one-line `/` text | a moderator, member or author. The session replies before `Control`, and the text is passed to neither `Control` nor Claude | `Only admins and owners of this group can send / commands.`, quoting the message | [hub.ts:28](../src/hub.ts#L28), [:157](../src/hub.ts#L157), [:308-310](../src/hub.ts#L308-L310) |

The requests `give CHAT N` and `observer #G ID` of [2.1](#21-commands-and-replies) are parsed only in a plugin session group ([control.ts:857-858](../src/control.ts#L857-L858)). Approvers are the active members of the session group with the role `admin` or `owner`, other than Claude and `User`, however they joined. An observer can't send, because the core refuses an observer's message ([hub.ts:312-318](../src/hub.ts#L312-L318), [control.ts:43](../src/control.ts#L43), [:580-582](../src/control.ts#L580-L582), [:589-598](../src/control.ts#L589-L598)).

The hub handles other messages by these rules, in arrival order, one at a time ([hub.ts:153-160](../src/hub.ts#L153-L160), [:230-233](../src/hub.ts#L230-L233), [:304-314](../src/hub.ts#L304-L314), [control.ts:219-221](../src/control.ts#L219-L221)):

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

`mode` and `permission` are exempt from the card size check ([control.ts:155](../src/control.ts#L155), [:700-706](../src/control.ts#L700-L706), [:735-740](../src/control.ts#L735-L740)). In `/pending`, their WHAT is `run in mode MODE` and `use TOOL` ([actions.ts:383-384](../src/actions.ts#L383-L384)). The last line of a `mode` or `permission` card is `Ended.` when the hub ends the card, and `Expired.` when it expires. The hub ends a `permission` card at an answer, or at a prompt that is not mid-turn, whose hook started after the card was created, and at the last detach; it ends a `mode` card at the option `requests` ([hub.ts:135](../src/hub.ts#L135), [:181](../src/hub.ts#L181), [:204-208](../src/hub.ts#L204-L208), [service.ts:321](../src/service.ts#L321), [actions.ts:237-241](../src/actions.ts#L237-L241), [control.ts:695-696](../src/control.ts#L695-L696)).

Posts:

| Poster | Text | Code |
|---|---|---|
| Claude | the final answer of each turn, in parts. The turn's prompt from the group, chosen by the hub from the turn records below, is quoted in the first part when the quote fits with the answer's first character. A changed short description is set after the answer | [hub.ts:180-202](../src/hub.ts#L180-L202), [:267-296](../src/hub.ts#L267-L296) |
| Claude | `Session N is not running.`, as a reply to a prompt when the session is not attached at the prompt's delivery. The hub delivers the prompt after the earlier prompts and after the prompt's own file. In the first 30 s after the service's start, the reply is posted only when the session does not attach by their end | [hub.ts:153-160](../src/hub.ts#L153-L160), [:235-249](../src/hub.ts#L235-L249), [:379](../src/hub.ts#L379) |
| Claude | `The file of this message was not received: ERROR`, as a reply to a prompt whose file was not received, see [4.5](#45-channel) | [hub.ts:235-259](../src/hub.ts#L235-L259) |
| Claude | the reply to the one-line `/` texts of the rules above, quoting the message | [hub.ts:28](../src/hub.ts#L28), [:157](../src/hub.ts#L157) |
| Claude | `note` of a `/turn` request, a line about one of Claude Code's own prompts, without a quote, before the other posts of the request. See the hook's notes below | [hub.ts:195](../src/hub.ts#L195), [hook.ts:217-290](../src/hook.ts#L217-L290) |
| `User` | each prompt typed in the terminal, also one typed during a turn, in parts. A prompt with `typed: false` is left out, such as a notification of a background task. A request without `typed`, from an older plugin, is posted as typed. Blank prompts, prompts made only of this server's channel tags, the prompts pushed from the group and one-line `/` commands are left out too. `User` posts in the group that is marked with the session's number | [hub.ts:184-196](../src/hub.ts#L184-L196), [user.ts:29-32](../src/user.ts#L29-L32) |
| Claude | a text in place of an answer, a note or a terminal prompt that the core refuses with `feature not allowed NAME`, as `SimpleX links` for a SimpleX link. The text for an answer is `Claude's answer was not posted: NAME are off in the group of this session; its owners can turn them on in the group's preferences.`, quoting the turn's prompt as the answer would. A note is replaced by `A note on Claude Code's prompt was not posted: ` and the same preference text. A terminal prompt is replaced by `User's prompt was not posted: NAME are not allowed for User in the group of this session; its owners can allow them in the group's preferences.`, even while NAME is on only for roles above that of `User`. Claude is an owner of the group, so its answers and notes get only the texts with `are off`. The turn goes on | [hub.ts:195-196](../src/hub.ts#L195-L196), [:278-288](../src/hub.ts#L278-L288), [:524-526](../src/hub.ts#L524-L526), [tools.ts:1671-1678](../src/tools.ts#L1671-L1678) |
| Claude | files sent by `reply`, at once | [tools.ts:730-747](../src/tools.ts#L730-L747), [hub.ts:93](../src/hub.ts#L93) |
| Claude | the rights of the member's role, below. They are posted once for each join and each role change of a member other than `User`, at its `joinedGroupMember`, `connectedToGroupMember` or `memberRole` | [hub.ts:221-228](../src/hub.ts#L221-L228), [:519](../src/hub.ts#L519), [control.ts:40-43](../src/control.ts#L40-L43), [:884-891](../src/control.ts#L884-L891) |

Rights posts, by the member's role in the session group:

| Role | Post |
|---|---|
| `owner` | `NAME is an owner and can send prompts to Claude and the group's commands, and approve tool use.` |
| `admin` | `NAME is an admin and can send prompts to Claude and the group's commands, and approve tool use. You can change role to observer to stop it: /'observer #G ID'` |
| `moderator`, `member`, `author` | `NAME is a member and can send prompts to Claude; the group's commands and approvals need the role admin or owner. You can change role to observer to stop it: /'observer #G ID'`, with `a moderator` or `an author` for those roles |
| `observer` | `NAME is an observer and can't send messages.` |

#G is the session group, and ID is the member. The core raises no `memberRole` event for a role change that Claude makes itself, with `/observer`. The reply to `/observer` is the observer's post instead.

Parts ([hub.ts:532-549](../src/hub.ts#L532-L549), [tools.ts:1639-1642](../src/tools.ts#L1639-L1642)): the hub cuts the text into the longest runs of whole characters that `fitsMessage` accepts as one message. `fitsMessage` is the measure of `send` in [1.3](#13-send), in UTF-8 bytes. The first part is measured with its quote. The hub puts up to 14962 bytes of text into a part without a quote: 14962 Latin, 7481 Cyrillic or 4987 CJK characters, fewer for characters escaped in JSON.

Claude can quote a message in its answer and its replies. When the core refuses such a post with `errorStore` `invalidQuote` or `chatItemNotFound`, as for a deleted prompt, or with `largeMsg`, as for a reply that quotes a message near the size limit, the post is sent again without the quote ([hub.ts:290-296](../src/hub.ts#L290-L296), [:551-554](../src/hub.ts#L551-L554)).

Turn records, added at a `/turn` with `prompt`, are read and changed before the first await of each `/turn` ([hub.ts:54](../src/hub.ts#L54), [:180-186](../src/hub.ts#L180-L186), [:267-276](../src/hub.ts#L267-L276)):

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

Group profile ([`createSessionGroup`](../src/hub.ts#L478-L490), [`Control.load`](../src/control.ts#L115-L122), [`describe`](../src/control.ts#L129-L131), [`updateProfile`](../src/control.ts#L133-L140)):

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

The hub takes the model and the effort level of a `/turn` only when its `at` is not older than the `at` of the value it took last. It keeps the two times apart, one for the model and one for the effort level. The model of a `/title` comes with the `at` of its SessionStart hook, as described in [4.6](#46-socket-protocol). A request without `at` counts as the current time. So when a Stop request that waited for its transcript entry arrives after a later model switch, the model of the switch stays ([hub.ts:187-193](../src/hub.ts#L187-L193), [:261-265](../src/hub.ts#L261-L265)).

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

The prompts of a session, from the members of [4.3](#43-session-groups), are delivered one at a time, in arrival order ([hub.ts:159](../src/hub.ts#L159), [:304-306](../src/hub.ts#L304-L306), [:235-249](../src/hub.ts#L235-L249)). While a session is attached, a prompt's file is received without a card, with relays approved, into `ROOT/_sessions/ID/files/group/<file ID><extension>`. The prompt is pushed after `rcvFileComplete`. After `rcvFileError`, a failed receive or 10 minutes, it is pushed without `file`. The next prompt waits until then ([hub.ts:235-259](../src/hub.ts#L235-L259), [:210-215](../src/hub.ts#L210-L215)). Pushed prompts are kept until they are matched by a turn's `prompt`, at most 20 ([hub.ts:23](../src/hub.ts#L23), [:186](../src/hub.ts#L186), [:247](../src/hub.ts#L247), [:269-273](../src/hub.ts#L269-L273)).

### 4.6 Socket protocol

The service speaks HTTP/1.1 over `ROOT/service.sock`, the fallback path or the Windows pipe, with protocol version `1`. Every request is sent with the header `Authorization: Bearer TOKEN`, and bodies are JSON. Streamed replies are NDJSON, with the content type `application/x-ndjson`. Errors are `{"error": TEXT}` with a status other than 200. Types: [`protocol.ts`](../src/protocol.ts). Routes: [`Service.routes`](../src/service.ts#L137-L161). Only `/title`, `/link` and `/reset-owners` return a Main group link. `/reset-owners` returns the link of the new group.

| Route | Request | Reply | Code |
|---|---|---|---|
| `POST /session` | `{id, pid?, project, name?, config: {mainGroup, approve, expireHours}}`, `pid` the session server's parent process | `{number, displayName, version, protocol, owner: {owners}}`. For a `pid` marked unattended, the reply is status 403, see below. The mark is checked before and after the wait for the title of `pid` | [service.ts:201-216](../src/service.ts#L201-L216), [:245-248](../src/service.ts#L245-L248) |
| `POST /title` | `{id, pid?, at?, name?, project, model?, config, unattended?}`, `pid` from `CLAUDE_PID`, `at` the `performance.timeOrigin` of the SessionStart hook | `{mainGroup, owner: {owners, link?}, version, protocol}`, after the title is applied, or at once for a held title. With `unattended: true`, the reply comes at once, and no session is opened. Marks 10 s old and older are removed, `pid` is marked, and a server's wait for its title ends | [service.ts:218-262](../src/service.ts#L218-L262) |
| `POST /attach` | `{id}` | a stream held open: the first line `{version, protocol}`, then `{prompt}` and `{permission}` lines | [service.ts:304-324](../src/service.ts#L304-L324) |
| `POST /call` | `{id, tool, input}` | lines `{progress: TEXT}`, then one line `{result: CallToolResult or null}`. The call is aborted when the request is closed | [service.ts:326-345](../src/service.ts#L326-L345) |
| `POST /permission` | `{id, requestId, toolName, description, inputPreview}` | `{}` after the card's action is created | [service.ts:143-147](../src/service.ts#L143-L147) |
| `POST /turn` | `{id, pid?, at?, prompt?, typed?, note?, midTurn?, answer?, model?, effort?}`, `at` the time the hook started, `typed` whether a person typed the prompt, `note` the line Claude posts about one of Claude Code's own prompts, `midTurn` whether Claude Code added the prompt to a running turn | `{}` after the posts | [service.ts:148-152](../src/service.ts#L148-L152) |
| `GET /status` | none | `{version, protocol, mainGroup, owner: {owners}, sessions: [{number, name, attached}]}`, with the open sessions | [service.ts:424-427](../src/service.ts#L424-L427) |
| `GET /link` | none | `{owners, link?}`, the link while the Main group has no owner | [service.ts:154](../src/service.ts#L154), [hub.ts:392-395](../src/hub.ts#L392-L395) |
| `POST /reset-owners` | none | `{link}`, the link of the new Main group. The group is replaced at once, even while owners exist | [service.ts:155](../src/service.ts#L155), [hub.ts:403-405](../src/hub.ts#L403-L405) |
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

`/session` and `/title` without `unattended` apply `config` to the hub ([hub.ts:397-401](../src/hub.ts#L397-L401), [service.ts:213](../src/service.ts#L213), [:221](../src/service.ts#L221), [:232](../src/service.ts#L232), [:259](../src/service.ts#L259)):
- The hub sets the expiry of all sessions from `expireHours`.
- The hub renames the Main group when `mainGroup` differs.

`/session` requests the session's mode ([hub.ts:132-151](../src/hub.ts#L132-L151), [:320-323](../src/hub.ts#L320-L323), [:325-328](../src/hub.ts#L325-L328), [:75](../src/hub.ts#L75), [:519](../src/hub.ts#L519)):
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
| `simplex-mcp hook turn` | UserPromptSubmit, Stop and PostModelSwitch hooks | `{}`. A prompt or an answer over 250,000 characters is cut there and ends with `…` ([hook.ts:16](../src/hook.ts#L16), [:307-309](../src/hook.ts#L307-L309)). The Stop hook sends the model of its answer, read from the transcript ([hook.ts:118](../src/hook.ts#L118), [:137-145](../src/hook.ts#L137-L145), [:153-185](../src/hook.ts#L153-L185)). The UserPromptSubmit hook sends `typed`, `note` and `midTurn`, read from the prompt's record in the transcript ([:119](../src/hook.ts#L119), [:147-151](../src/hook.ts#L147-L151)) |
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
| 0 | the hooks, even after errors, `link` and `reset-owners`, and the service after a stop or with `running` | [hook.ts:97-99](../src/hook.ts#L97-L99), [:131-134](../src/hook.ts#L131-L134), [service.ts:440](../src/service.ts#L440) |
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
| session group created | `Group "N · NAME" of session N created.` | [hub.ts:488](../src/hub.ts#L488) |
| session group lost | `The group "N · NAME" is lost.` | [control.ts:483](../src/control.ts#L483) |
| actions | as in [3.4](#34-terminal-lines), with ` of session N` after `Action N`, and `allowed.` for a tool use | [actions.ts:210](../src/actions.ts#L210), [:410](../src/actions.ts#L410) |
| stop | `Cancelled K action(s).`, `Stopping: sending K cancelled card(s), up to 10 s. Ctrl-C to stop now.`, `Stopped.` | [service.ts:189](../src/service.ts#L189), [serve.ts:170-172](../src/serve.ts#L170-L172), [service.ts:192](../src/service.ts#L192) |

stderr: `CONTEXT: ERROR` from `logError`, with CONTEXT among `Main group command`, `EVENT in the Main group`, `EVENT in the group of session N`, `prompt of session N`, `invitation to session N`, `User`, `User in session N`, `group of session N`, `session N`, `owner of CHAT`, `mode host of session N`, `title`, `ROOT/_sessions/ID/session.json`, `ROOT/titles.json`, `ROOT/service.log`, `METHOD PATH` and those of [3.4](#34-terminal-lines). Session processes and hooks write to their own stderr ([session.ts:55](../src/session.ts#L55), [:61](../src/session.ts#L61), [:93](../src/session.ts#L93), [hook.ts:110](../src/hook.ts#L110), [:132](../src/hook.ts#L132)):
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

Hook input fields used ([hook.ts:27-70](../src/hook.ts#L27-L70)):
- SessionStart: `session_id`, `session_title`, `cwd` and `model`.
- The other hooks: `session_id`, `hook_event_name`, `transcript_path`, `prompt_id`, `prompt`, `last_assistant_message`, `to_model` and `effort.level`.

The Stop hook reads `type`, `isSidechain`, `message.model` and the `text` parts of `message.content` from the last entries of the transcript ([hook.ts:176-185](../src/hook.ts#L176-L185), [:292-305](../src/hook.ts#L292-L305)). It reads the transcript up to 20 times, 100 ms apart, until Claude's last entry is the answer. A missing transcript is read again, as a transcript without that entry is ([hook.ts:17](../src/hook.ts#L17), [:137-145](../src/hook.ts#L137-L145), [:153-161](../src/hook.ts#L153-L161)).

The UserPromptSubmit hook finds the prompt's record among the last entries of the transcript. A record is a user entry, without subagent entries and tool results, or a `queued_command` attachment, the record of a prompt that Claude Code added to a running turn. The record is the newest one whose text equals the prompt: the entry whose `promptId` equals the hook's `prompt_id`, or a record after the first entry with that ID. Claude Code gives the hook of a prompt added to a running turn that turn's prompt ID, and the turn's tool results carry it too, so the first entry with that ID is found also when the turn's first entry is out of the tail. Until an entry with `prompt_id` is in the tail, the hook waits for it, and at the last read it takes any record with the prompt's text. While a `queue-operation` `enqueue` entry with the prompt's text follows the record, the hook waits too: the prompt is a later one with the same text, still queued. Without `prompt_id`, as in older Claude Code, it takes any record at once. `typed` is true when the record's `origin.kind` is `human`, and `midTurn` is true when the record is not the entry with `prompt_id`. Claude Code writes the entry only after the synchronous UserPromptSubmit hooks end, and it creates the transcript file with the session's first entry. So the hook reads the transcript up to 600 times, 100 ms apart. A missing transcript is read again, as a transcript without the entry is. Claude Code runs the hook of a person's prompt when it is submitted, and of its own prompts only when they are taken. So at the last read, the hook classifies a record still followed by such an `enqueue` entry by the record, and marks a typed one mid-turn: the queued prompt is the typed prompt itself, or a later prompt with the text of Claude Code's own prompt. When no record is found within these reads, an `enqueue` entry with the prompt's text after the entry with `prompt_id`, or anywhere without that entry, makes the prompt typed and mid-turn. Otherwise `typed` is true when an entry with `prompt_id` has the origin `human` ([hook.ts:18-19](../src/hook.ts#L18-L19), [:147-161](../src/hook.ts#L147-L161), [:187-222](../src/hook.ts#L187-L222)).

The hook's `note` is a line of at most 200 characters; a longer one is cut to 199 and ends with `…`. A typed prompt, a channel prompt, a teammate's prompt of idle notifications only and another record without origin that is not scheduled have no note ([hook.ts:217-290](../src/hook.ts#L217-L290)):

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

SessionStart output ([hook.ts:81-112](../src/hook.ts#L81-L112)):

| Case | Output |
|---|---|
| a run that is not interactive ([4.1](#41-tools-in-the-plugin)) | `{}`, after `/title` with `unattended` when `CLAUDE_CODE_SESSION_ATTENDED` is `0` and the entrypoint is an interactive one |
| Node.js below 22 | `{"systemMessage": "SimpleX MCP needs Node.js 22 or later; found V."}` |
| owners present, or no link | `{}` |
| no owner, a link | `systemMessage`: `SimpleX MCP: scan this QR code with the SimpleX app and tap Join to supervise your agents in "MAIN":`, the `plain` QR code and the link |
| service not running after the start | `{"systemMessage": "SimpleX MCP: The SimpleX service stopped; it starts again with the next tool call."}` |
| other errors, invalid options included | `{"systemMessage": "SimpleX MCP: TEXT"}` |
