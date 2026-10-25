# SimpleX MCP server: API

> The five MCP tools, the agent control chat commands, replies and cards, and the CLI of `@simplex-chat/mcp` 0.1.0.
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
   - [1.5 `list`](#15-list)
   - [1.6 `leave`](#16-leave)
   - [1.7 Message and command objects](#17-message-and-command-objects)
   - [1.8 Action refusals](#18-action-refusals)
2. [Agent control chat](#2-agent-control-chat)
   - [2.1 Commands and replies](#21-commands-and-replies)
   - [2.2 Cards](#22-cards)
   - [2.3 Membership messages](#23-membership-messages)
   - [2.4 Group profile](#24-group-profile)
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
| Tools are registered on each per-request `McpServer` with description, input schema, output schema and, in mode `host`, `_meta: {"anthropic/requiresUserInteraction": true}` for `connect`, `send` and `leave` | [tools.ts:355-365](../src/tools.ts#L355-L365) |
| Tool descriptions and server instructions; the instructions name the agent's display name | [tools.ts:301-332](../src/tools.ts#L301-L332), [serve.ts:109](../src/serve.ts#L109) |
| Arguments are validated by the SDK with Ajv; a failure is a result with `isError: true` and the text `Input validation error: Invalid arguments for tool NAME: ERRORS` | [mcp-BumlNux0.cjs:1812-1817](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L1812-L1817) |
| `additionalProperties: false` is set in every input schema | [tools.ts:177](../src/tools.ts#L177), [:192](../src/tools.ts#L192), [:203](../src/tools.ts#L203), [:206](../src/tools.ts#L206), [:214](../src/tools.ts#L214) |
| A result is sent as `structuredContent` and as its JSON in one text item | [tools.ts:370](../src/tools.ts#L370) |
| A tool error is sent as `isError: true` with one text item: the `ToolError` text, or the `errorText` of other errors | [tools.ts:372](../src/tools.ts#L372), [log.ts:39-43](../src/log.ts#L39-L43) |
| With a `progressToken` in `_meta`, `notifications/progress` is sent at the start of each wait and every 60 s, with `progress` 0, 1, 2, ... and a `message` | [tools.ts:1011-1021](../src/tools.ts#L1011-L1021), [messages.ts:200-204](../src/messages.ts#L200-L204) |
| The call is aborted when the client closes the response stream or sends `notifications/cancelled`; the action is kept | [src-BhnGYybN.cjs:6338-6341](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs#L6338-L6341), [:6425](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs#L6425) |
| A call with the same tool and action fields as an action in memory is attached to it | [tools.ts:1003-1005](../src/tools.ts#L1003-L1005) |
| Messages are marked read after the last wait of a call that is not aborted | [tools.ts:460-461](../src/tools.ts#L460-L461), [:598-600](../src/tools.ts#L598-L600), [:679-680](../src/tools.ts#L679-L680) |

Action fields of the key:

| Tool | Fields |
|---|---|
| `connect` | `target`<br>`incognito` (default `false`)<br>`message` (default `null`) |
| `send` | `chat`<br>`text`<br>`reply_to` (default `null`)<br>`mention` (default `null`) |
| `leave` | `chat` |

Result status of the three action tools:

| Status | Meaning | Extra fields |
|---|---|---|
| `done` | performed | per tool |
| `failed` | failure of the card post, the counter write or the perform, or loss of the control chat | `error` |
| `rejected` | an approver sent `/reject` | |
| `expired` | no decision before the expiry time | |
| `known` | `connect` only: the target is an existing chat past the prepared state; no action | chat fields |

### 1.2 `connect`

Handler: [`AgentTools.connect`](../src/tools.ts#L412-L425). Perform: [`performConnect`](../src/tools.ts#L480-L512). Schemas: [input](../src/tools.ts#L167-L178), [output](../src/tools.ts#L220-L230).

Input:

| Field | Type | Range | Default | Rule |
|---|---|---|---|---|
| `target` | string | 1 character or more | required | SimpleX link (`https://...`, `simplex:/...`), contact name `@name.simplex` or channel name `#name.simplex`; whitespace refused |
| `incognito` | boolean | | `false` | `true`: `incognito=on` in the connect command |
| `message` | string | 1 to 2000 characters | | first message of a contact request; plans with `takesMessage` only |
| `wait` | number | 0 to 86400 | 60 | seconds from the approval: for the other side to accept and, with `message`, for its first reply |
| `idle` | number | 1 to 300 | 30 | seconds without a new message, after which the server ends the wait for messages |

Output fields by case:

| Case | Fields | Code |
|---|---|---|
| `known` | `status`, chat fields of the known chat | [tools.ts:417](../src/tools.ts#L417) |
| `done`, chat unknown | `action`, `status`, `type`, `connected: false`, plan fields `name`, `peer_type`, `description`, `welcome`, `commands`, `known_as` when present | [tools.ts:428](../src/tools.ts#L428), [:899-905](../src/tools.ts#L899-L905) |
| `done`, chat known | `action`, `status`, chat fields, `messages` | [tools.ts:444-464](../src/tools.ts#L444-L464) |
| `failed` | `action`, `status`, `error` | [tools.ts:435](../src/tools.ts#L435) |
| `rejected`, `expired` | `action`, `status` | [tools.ts:435](../src/tools.ts#L435) |

Chat fields ([`chatFields`](../src/tools.ts#L740-L767)):

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
| `known_as` | incognito: `customUserProfile` of `/_info @C` | incognito: the membership's display name |
| `role` | | the agent's member role |
| `connected` | active connection status `ready` | membership status `connected` or `complete` |

`messages`: message items among the last 50 items of the chat when the result is built, replaced by their collected copies, followed by the items collected since the connection; live items are left out; new items are marked read ([tools.ts:446](../src/tools.ts#L446), [:456-463](../src/tools.ts#L456-L463)).

Tool errors:

| Text | Condition | Code |
|---|---|---|
| `Not a SimpleX link or name: TARGET` | whitespace in `target`, or `invalidConnReq` from the plan command | [tools.ts:468](../src/tools.ts#L468), [:474](../src/tools.ts#L474) |
| an app error text, for example `Name not found: This SimpleX name is not registered. Please check the name.`, or `SimpleX error: CHAIN: DETAILS` | other errors of the plan command, or a plan of type `error` | [tools.ts:475](../src/tools.ts#L475), [:826](../src/tools.ts#L826), [log.ts:45-47](../src/log.ts#L45-L47) |
| `TARGET can't be joined now: PLAN.` | `invitationLink ownLink` or `connecting`<br>`contactAddress ownLink` or `connectingProhibit`<br>`groupLink ownLink`, `connectingProhibit`, `noRelays` or `updateRequired` | [tools.ts:817](../src/tools.ts#L817), [:830](../src/tools.ts#L830), [:848](../src/tools.ts#L848), [:870](../src/tools.ts#L870) |
| `message: TARGET is not a contact or business address with short link data, or its contact exists without a connection.` | `message` given and the plan has `takesMessage: false` | [tools.ts:419](../src/tools.ts#L419) |
| refusals | see [1.8](#18-action-refusals) | |

`takesMessage` is `true` for a contact address with short link data, for a known prepared contact, and for a known prepared business group ([tools.ts:821](../src/tools.ts#L821), [:841](../src/tools.ts#L841), [:859](../src/tools.ts#L859)).

### 1.3 `send`

Handler: [`AgentTools.send`](../src/tools.ts#L573-L588). Result: [`sendResult`](../src/tools.ts#L590-L608). Perform: [`performSend`](../src/tools.ts#L610-L629). Schemas: [input](../src/tools.ts#L180-L193), [output](../src/tools.ts#L232-L244).

Input:

| Field | Type | Range | Default | Rule |
|---|---|---|---|---|
| `chat` | string | | required | `@N` or `#N` of the agent |
| `text` | string | 1 to 12000 characters | required | sent as one text message |
| `reply_to` | integer | 1 or more | | item ID in `chat` |
| `mention` | integer array | 1 to 3, unique, each 1 or more | | member IDs of a group; names missing from `text` are prefixed as `@name` |
| `wait` | number | 0 to 86400 | 60 | seconds from the send; from the call when it attaches to a done action |
| `idle` | number | 1 to 300 | 30 | seconds without a new response, once one arrived |
| `limit` | integer | 1 to 100 | 50 | most responses returned |

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
| `No chat CHAT. list shows the agent's chats.` | invalid reference, the control chat, or a store error when the chat is read | [tools.ts:714-721](../src/tools.ts#L714-L721) |
| `The agent has role ROLE in CHAT and can't post.` | membership role `observer` or `relay` | [tools.ts:576-579](../src/tools.ts#L576-L579) |
| `reply_to: CHAT has no item N.` | the read of `around=N` fails, or the item is absent from it | [tools.ts:723-728](../src/tools.ts#L723-L728) |
| `reply_to: item N in CHAT is not a message.` | the item's content is not `rcvMsgContent` or `sndMsgContent` | [tools.ts:722](../src/tools.ts#L722) |
| `mention: CHAT is a contact chat; mentions apply to groups and channels.` | `mention` with `@N` | [tools.ts:731](../src/tools.ts#L731) |
| `mention: CHAT has no member N.` | ID absent from `/_members` | [tools.ts:735](../src/tools.ts#L735) |
| `mention: members N and M have the same name.` | two mentioned members with the same display name | [tools.ts:735](../src/tools.ts#L735) |
| refusals | see [1.8](#18-action-refusals) | |

### 1.4 `receive`

Handler: [`AgentTools.receive`](../src/tools.ts#L653-L686). Schemas: [input](../src/tools.ts#L195-L204), [output](../src/tools.ts#L246-L254).

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

Tool errors: `No chat CHAT. list shows the agent's chats.` for an invalid reference, the control chat, or a store error of the first read, `initial=1` ([tools.ts:655](../src/tools.ts#L655), [:659-661](../src/tools.ts#L659-L661)); other chat errors, a store error of a later page read included, as their error text ([log.ts:39-47](../src/log.ts#L39-L47)).

Messages are read by `receive` without an action or an approval.

### 1.5 `list`

Handler: [`AgentTools.listChats`](../src/tools.ts#L688-L696). Schemas: [input](../src/tools.ts#L206), [output](../src/tools.ts#L256-L288).

Input: an empty object.

Output:

| Field | Item fields |
|---|---|
| `chats` | `chat`<br>`type`<br>`name`<br>`simplex_name`, when verified<br>`peer_type`, when present<br>`incognito`<br>`known_as`, when incognito<br>`connected`<br>`unread`: `chatStats.unreadCount`<br>`approved: true`, only under a standing approval in mode `requests` |
| `actions` | `action`<br>`tool`<br>`input`: the input of the creating call<br>`state`<br>`open`: `true` while a call waits on it |

`chats`: the contacts and groups from one read, `/_get chats USER pcc=off count=1000000`, except the control chat ([tools.ts:698-712](../src/tools.ts#L698-L712)). `actions`: every action in memory ([tools.ts:694](../src/tools.ts#L694)).

### 1.6 `leave`

Handler: [`AgentTools.leave`](../src/tools.ts#L631-L640). Perform: [`performLeave`](../src/tools.ts#L646-L651). Schemas: [input](../src/tools.ts#L208-L215), [output](../src/tools.ts#L290-L299).

Input: `chat`, string, required, `@N` or `#N` of the agent.

Output:

| Field | Presence | Value |
|---|---|---|
| `action` | always | action number |
| `status` | always | `done`, `failed`, `rejected`, `expired` |
| `chat` | `done` | the deleted chat |
| `error` | `failed` | error text |

Perform:

| Chat | Commands |
|---|---|
| `@N` | `/_delete @N full` |
| `#N` | `/_leave #N` while the membership is current, `pending_approval` or `pending_review`<br>`/_delete #N full` |

Tool errors: `No chat CHAT. list shows the agent's chats.` when no action with the same key is in memory and the chat is invalid, the control chat or unknown ([tools.ts:634-636](../src/tools.ts#L634-L636)); refusals.

### 1.7 Message and command objects

Message ([`Message`](../src/messages.ts#L11-L22), [`toMessage`](../src/messages.ts#L135-L152), [schema](../src/tools.ts#L119-L139)):

| Field | Type | Presence | Source |
|---|---|---|---|
| `chat` | string | always | chat reference of the item |
| `item` | integer | always | `meta.itemId` |
| `from` | string | always | `groupRcv`: member display name<br>`directRcv`: contact display name<br>`channelRcv`: channel display name<br>sent items in `connect`: the agent's name in that chat |
| `from_member` | integer | `groupRcv` | `groupMemberId` |
| `from_type` | string | when the profile has a peer type | sender's `peerType`; the agent's own for its non-incognito items in `connect` |
| `time` | string | always | `meta.itemTs` |
| `text` | string | always | `msgContent.text` |
| `new` | boolean | always | item status `rcvNew` when read |
| `reply_to` | object | quoted item with an ID | `{chat, item: quotedItem.itemId}` |
| `mentions_me` | boolean | always | `meta.userMention` |

Only received message items (`rcvMsgContent`) are returned by `send` and `receive`; `connect` also returns sent ones (`sndMsgContent`) ([messages.ts:73](../src/messages.ts#L73), [:123-125](../src/messages.ts#L123-L125)).

Command ([`commandsOf`](../src/tools.ts#L985-L992), [schema](../src/tools.ts#L141-L151)):

| Form | Fields |
|---|---|
| command | `keyword`<br>`label`<br>`params`, when present |
| menu | `label`<br>`commands`: commands and menus |

### 1.8 Action refusals

Refusals of a new `connect`, `send` or `leave` action, first match ([actions.ts:136](../src/actions.ts#L136), [control.ts:105-111](../src/control.ts#L105-L111)):

| Text | Condition |
|---|---|
| `The server is stopping.` | `Actions.stop` has started |
| `The agent lost its control chat. The person who runs it has to restart it with --reset-owners.` | control chat lost |
| `20 actions are pending; wait for the owner's decisions.` | 20 actions in state `pending` |
| `20 actions are being performed; wait for their results.` | 20 actions in state `performing` |
| `text: too long for its approval card; shorten it.` | `send` whose largest card form exceeds 15000 bytes |
| `message: too long for its approval card; shorten it.` | `connect` whose largest card form exceeds 15000 bytes without the link texts |
| `The agent has no owner. The person who runs it has to join its control chat with the link the server printed.` | no approver, `--owners` absent |
| `The agent has no approving owner. The person who runs it has to restart it with the --owners options it printed.` | no approver, `--owners` given |

Largest card form ([control.ts:476-484](../src/control.ts#L476-L484)): the maximum of the creation form, the `done` form and a `failed` form with an error of 600 × `€`, with the action's own decider or the longest current approver name; each form is measured as JSON-escaped UTF-8: `"`, `\`, `\n`, `\r`, `\t` 2 bytes, other characters below U+0020 6 bytes.

Link texts of a `connect` card ([control.ts:111](../src/control.ts#L111), [:508-522](../src/control.ts#L508-L522)): the description and welcome lines take the bytes that the largest form leaves without them. Lines are kept while they fit; the next line is cut to the bytes left and ends in `…`; later lines are left out. The tool result keeps both texts whole.

---

## 2. Agent control chat

### 2.1 Commands and replies

Accepted text ([`parseCommand`](../src/control.ts#L410-L415), [`ownerRequest`](../src/control.ts#L521-L530)):

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
  approve CHAT all
  revoke CHAT
  pending
  list
```

The server ignores other text. It answers a command with a reply that quotes it ([control.ts:142](../src/control.ts#L142)), and it runs the expiry check before each command ([control.ts:164](../src/control.ts#L164)).

| Request | Condition | Reply | Code |
|---|---|---|---|
| one-line `/` text outside the requests | any sender | `Commands: /approve, /reject, /revoke, /pending, /list. Each card shows its commands.` | [control.ts:15](../src/control.ts#L15), [:166](../src/control.ts#L166) |
| any request | sender not an approver, `--owners` absent | `Only owners approve actions.` | [control.ts:167](../src/control.ts#L167) |
| any request | sender not an approver, `--owners` given | `Only the owners listed in --owners approve actions.` | [control.ts:167](../src/control.ts#L167) |
| `approve`, `reject` | action N absent, not `pending`, or without a card | `Action N is not pending.` | [control.ts:182](../src/control.ts#L182) |
| `approve CHAT N`, `reject CHAT N` | action N is a `connect`, or for another chat | `Action N is not for CHAT.` | [control.ts:183](../src/control.ts#L183) |
| `approve`, `reject` | accepted | none; the card is edited at the outcome | [control.ts:184-187](../src/control.ts#L184-L187) |
| `approve CHAT all` | mode `host`; nothing is stored | `Approvals apply in mode requests; this server runs in mode host.` | [control.ts:182](../src/control.ts#L182) |
| `approve CHAT all` | CHAT unknown or the control chat | `No chat CHAT. /list shows the agent's chats.` | [control.ts:194](../src/control.ts#L194) |
| `approve CHAT all` | accepted | `Approved for KIND NAME (CHAT) by APPROVER. /'revoke CHAT'` | [control.ts:202](../src/control.ts#L202) |
| `revoke CHAT` | CHAT unknown, or without a standing approval | `Messages to CHAT have no standing approval.` | [control.ts:207](../src/control.ts#L207) |
| `revoke CHAT` | accepted | `Messages to KIND NAME (CHAT) need approval again.` | [control.ts:210](../src/control.ts#L210) |
| `pending` | no pending action with a card | `No pending actions.` | [control.ts:171](../src/control.ts#L171) |
| `pending` | otherwise | one line per action: `N  WHAT  expires YYYY-MM-DD HH:MM UTC` | [control.ts:170](../src/control.ts#L170), [:472-474](../src/control.ts#L472-L474) |
| `list` | no chats | `No chats.` | [control.ts:220](../src/control.ts#L220) |
| `list` | otherwise | one line per chat: `CHAT  KIND NAME`, then `, approved for this chat by APPROVER` and `, not connected` when applicable | [control.ts:213-221](../src/control.ts#L213-L221) |

KIND is `contact`, `bot`, `agent`, `business`, `group` or `channel` ([tools.ts:928-935](../src/tools.ts#L928-L935)). WHAT is `connect to SUBJECT`, `send to SUBJECT`, `delete SUBJECT` or `leave SUBJECT` ([actions.ts:268-278](../src/actions.ts#L268-L278)). Names are shown by `safeName` ([control.ts:498-501](../src/control.ts#L498-L501)).

### 2.2 Cards

Card text: request lines, then one last line ([`cardText`](../src/control.ts#L417-L436), [`requestLines`](../src/control.ts#L438-L459)).

Request lines:

| Tool | First line | Further lines |
|---|---|---|
| `connect` | `I want to connect to SUBJECT, as myself.` or `, incognito.`<br>done and connected: `I connected to ...`<br>done and not connected: `I asked to connect to ...` | link description, one code span per line<br>welcome, one code span per line<br>both cut to fit the card ([1.8](#18-action-refusals))<br>with `message`: `With the message:` and its lines |
| `send` | `I want to send a message to SUBJECT:`, with `, where I am incognito` before the colon in incognito chats<br>done: `I sent a message to ...` | text, one code span per line<br>`replying to NAME: QUOTE` with `reply_to`<br>`mentioning @A, @'B C'` with `mention` |
| `leave` | `I want to delete KIND NAME (@N).` or `I want to leave KIND NAME (#N).`<br>done: `I deleted ...` or `I left ...` | |

SUBJECT is `KIND NAME`, or `an unnamed KIND`, followed by ` (SIMPLEX_NAME)` when known ([actions.ts:263-266](../src/actions.ts#L263-L266)). For `connect`, SIMPLEX_NAME is the target when it is a name; for `send`, the chat's verified SimpleX name.

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

DECIDER is `Performed without approval (mode host)` in mode `host`, `Approved for this chat by NAME` for a standing send, and `Approved by NAME` otherwise ([control.ts:419](../src/control.ts#L419)).

Command lines ([`commandsLine`](../src/control.ts#L461-L470)):

| Tool | Command line |
|---|---|
| `connect` | `/'approve N' or /'reject N'` |
| `send` | `/'approve CHAT N', /'approve CHAT all' or /'reject CHAT N'` |
| `leave` | `/'approve CHAT N' or /'reject CHAT N'` |

The app shows `/'approve N'` as the tappable command `/approve N` ([PLAN, What the owner sees](../PLAN.md#what-the-owner-sees-the-agent-control-chat)).

### 2.3 Membership messages

Posted in the chat and logged; in the chat, names are shown by `safeName` and the `--owners ...` part is a code span; in the log, names are folded to one line ([control.ts:322-325](../src/control.ts#L322-L325), [:64-65](../src/control.ts#L64-L65)).

| Situation | Text | Code |
|---|---|---|
| owner joined, `--owners` absent | `I'm your agent. I ask here before I connect or send.` (chat only) | [control.ts:19](../src/control.ts#L19), [:270](../src/control.ts#L270) |
| member joined, `--owners` without a list | `NAME joined (member ID). Restart the server with --owners ENTRIES` | [control.ts:275-278](../src/control.ts#L275-L278) |
| start with `--owners` without a list and humans present | `A joined (member ID). B joined (member ID). Restart the server with --owners ENTRIES` | [control.ts:242-245](../src/control.ts#L242-L245) |
| a human became owner outside the list | `NAME (member ID) is now an owner and can't approve actions yet. Restart the server with --owners ENTRIES` | [control.ts:283-287](../src/control.ts#L283-L287) |
| a listed owner stopped being a human owner | `NAME (member ID) is no longer an owner. Restart the server with --owners ENTRIES`<br>none remains: `NAME (member ID) is no longer an owner. No owners remain: restart the server with --reset-owners --owners` | [control.ts:288-293](../src/control.ts#L288-L293) |

ENTRIES: `ID:NAME` of each human, or of each human owner, joined by commas; an entry with characters other than letters, digits, `_` and `-` is quoted for the shell ([control.ts:581-591](../src/control.ts#L581-L591)).

### 2.4 Group profile

| Field | Value | Code |
|---|---|---|
| display name | the agent's display name, updated at start when it differs | [control.ts:91-93](../src/control.ts#L91-L93) |
| full name | empty | [control.ts:377](../src/control.ts#L377) |
| description | `Supervise your AI agent here: approve or reject its connections and messages with one tap.` | [control.ts:378](../src/control.ts#L378) |
| history | on | [control.ts:379](../src/control.ts#L379) |
| command menu | `/pending` "Pending actions"<br>`/list` "Chats and approvals"<br>updated at start when it differs | [control.ts:10-13](../src/control.ts#L10-L13), [:91-93](../src/control.ts#L91-L93) |

---

## 3. CLI

Binary: `simplex-mcp`, compiled `dist/cli.js` ([package.json:5-7](../package.json#L5-L7)). Help text: [cli.ts:10-35](../src/cli.ts#L10-L35).

```
simplex-mcp --help
simplex-mcp serve NAME [options]
simplex-mcp register NAME [options]
```

NAME: 1 to 32 characters of `[a-z0-9]` and single hyphens, starting with a letter or digit ([cli.ts:142-144](../src/cli.ts#L142-L144)).

### 3.1 `serve`

| Option | Value | Default | Rule | Code |
|---|---|---|---|---|
| `--data` | folder | `~/.simplex-mcp/NAME` | resolved to an absolute path; `dataGiven` is set | [cli.ts:65-68](../src/cli.ts#L65-L68) |
| `--display-name` | text | NAME when the user is created | must equal `validName(text)` | [cli.ts:69-71](../src/cli.ts#L69-L71), [:146-150](../src/cli.ts#L146-L150) |
| `--approve` | `requests` or `host` | `requests` | | [cli.ts:72-74](../src/cli.ts#L72-L74) |
| `--owners` | absent value, or `ID:NAME,...` | absent | the value is absent when the next argument is missing or starts with `--`; it is split at each comma followed by `ID:`, so a name may hold commas; entries match `^([1-9]\d*):(.+)$` | [cli.ts:75-77](../src/cli.ts#L75-L77), [:167-171](../src/cli.ts#L167-L171) |
| `--expire` | hours | 23 | integer 1 to 23 | [cli.ts:78-80](../src/cli.ts#L78-L80) |
| `--reset-owners` | flag | off | refused with an `--owners` list | [cli.ts:81-83](../src/cli.ts#L81-L83), [:97-99](../src/cli.ts#L97-L99) |
| `--new-token` | flag | off | | [cli.ts:84-86](../src/cli.ts#L84-L86) |
| `--port` | number | 5226 | integer 1 to 65535 | [cli.ts:87-89](../src/cli.ts#L87-L89) |
| `--qr-code` | `small` or `large` | `small` | | [cli.ts:90-92](../src/cli.ts#L90-L92) |

### 3.2 `register`

| Option | Value | Default | Code |
|---|---|---|---|
| `--scope` | `user` or `local` | `user` | [cli.ts:107-109](../src/cli.ts#L107-L109) |
| `--data` | folder | `~/.simplex-mcp/NAME` | [cli.ts:110-112](../src/cli.ts#L110-L112) |

Entry ([`registryEntry`](../src/register.ts#L43-L50)):

| Field | Value |
|---|---|
| `type` | `http` |
| `url` | `http://127.0.0.1:PORT/mcp` |
| `headersHelper` | `cat DATA/headers.json`, Windows `type DATA\headers.json`, path shell-quoted |
| `timeout` | `(expireHours + 24) × 3600000 + 600000` ms |

Server name: `simplex-NAME` ([register.ts:33](../src/register.ts#L33)).

### 3.3 Exit codes and messages

| Code | Message | Code location |
|---|---|---|
| 0 | `--help` text on stdout; a stop after a signal | [cli.ts:194-196](../src/cli.ts#L194-L196), [serve.ts:53](../src/serve.ts#L53) |
| 2 | `Usage: simplex-mcp serve NAME [options] or simplex-mcp register NAME [options]. Run simplex-mcp --help.` | [cli.ts:45](../src/cli.ts#L45) |
| 2 | `NAME: 1 to 32 lowercase letters, digits and single hyphens, starting with a letter or digit.` | [cli.ts:46](../src/cli.ts#L46) |
| 2 | `OPTION: a value expected.` | [cli.ts:131](../src/cli.ts#L131) |
| 2 | `Unknown option OPTION. Run simplex-mcp --help.` | [cli.ts:184](../src/cli.ts#L184) |
| 2 | `--display-name: not a valid SimpleX name; try VALID` | [cli.ts:148](../src/cli.ts#L148) |
| 2 | `--approve: requests or host expected, got VALUE` | [cli.ts:153](../src/cli.ts#L153) |
| 2 | `--qr-code: small or large expected, got VALUE` | [cli.ts:158](../src/cli.ts#L158) |
| 2 | `--scope: user or local expected, got VALUE` | [cli.ts:163](../src/cli.ts#L163) |
| 2 | `--owners: ID:NAME expected, got ENTRY` | [cli.ts:169](../src/cli.ts#L169) |
| 2 | `--expire: hours from 1 to 23 expected, got VALUE` | [cli.ts:79](../src/cli.ts#L79), [:175](../src/cli.ts#L175) |
| 2 | `--port: a port from 1 to 65535 expected, got VALUE` | [cli.ts:88](../src/cli.ts#L88), [:175](../src/cli.ts#L175) |
| 2 | `--reset-owners takes --owners without a list: member IDs change in a new agent control chat.` | [cli.ts:98](../src/cli.ts#L98) |
| 2 | `--owners: member ENTRY is not in the agent control chat. Start with --reset-owners --owners, or use the IDs of current members.` | [control.ts:251](../src/control.ts#L251) |
| 1 | `Another server uses DATA (PID N). If no server runs, remove DATA/lock.` | [files.ts:21](../src/files.ts#L21), [:23](../src/files.ts#L23) |
| 1 | `Port PORT is in use. Start with --port N.` | [serve.ts:205](../src/serve.ts#L205) |
| 1 | `Agent "NAME" was never started. Start it with: npx -y @simplex-chat/mcp serve NAME` | [register.ts:32](../src/register.ts#L32) |
| 1 | `Add this entry under LOCATION in ~/.claude.json: {"simplex-NAME": ENTRY}` | [register.ts:52-55](../src/register.ts#L52-L55) |
| 1 | `claude mcp add-json failed: STDERR` | [register.ts:39](../src/register.ts#L39) |
| 1 | `simplex-mcp: ERROR` for other errors, `stop: ERROR` for a failed stop | [cli.ts:210-211](../src/cli.ts#L210-L211), [serve.ts:55-56](../src/serve.ts#L55-L56) |

LOCATION is `"mcpServers"` for scope `user`, and `"projects" > "PROJECT" > "mcpServers"` for scope `local` ([register.ts:53](../src/register.ts#L53)).

### 3.4 Terminal lines

stdout ([log.ts:31-33](../src/log.ts#L31-L33)):

| Moment | Line | Code |
|---|---|---|
| start | `SimpleX MCP server VERSION, agent "NAME"`<br>`Data folder: DATA`<br>`Approval mode: MODE` | [serve.ts:72-74](../src/serve.ts#L72-L74) |
| control chat created | `Agent control chat "DISPLAY" created.` | [control.ts:383](../src/control.ts#L383) |
| link opened | `Join the agent control chat in the SimpleX app. The link works once:`<br>QR code<br>link<br>small QR code: `If the QR code does not scan, restart with --qr-code large, or paste the link into the app.`<br>role line | [control.ts:304-310](../src/control.ts#L304-L310) |
| listening | `Listening on http://127.0.0.1:PORT/mcp` | [serve.ts:120](../src/serve.ts#L120) |
| owner setup done | `Add the agent to Claude Code:`<br>`  npx -y @simplex-chat/mcp register NAME [--data DATA]` | [serve.ts:164-166](../src/serve.ts#L164-L166) |
| owner joined | `NAME joined (member ID) and is now an owner. The link is deleted.` | [control.ts:271](../src/control.ts#L271) |
| listed owners set | `Owners: ENTRIES.` | [control.ts:262](../src/control.ts#L262) |
| approvers gone, `--owners` absent | `No owners in the agent control chat.` | [control.ts:20](../src/control.ts#L20), [:157](../src/control.ts#L157), [:238](../src/control.ts#L238) |
| control chat lost | `The agent lost its control chat. Restart the server with --reset-owners.` | [control.ts:298](../src/control.ts#L298) |
| action created | `Action N: WHAT` with `, as itself` or `, incognito` for `connect` and ` (CHAT)` for `send` and `leave` | [actions.ts:140-141](../src/actions.ts#L140-L141), [:268-278](../src/actions.ts#L268-L278) |
| counter write or card post failed | `Action N: failed: ERROR.` | [actions.ts:232](../src/actions.ts#L232) |
| perform ended, not interrupted by the stop, mode `requests` | `Action N approved by NAME: RESULT.` with RESULT `sent`, `deleted`, `left`, `connected`, `connection requested` or `failed: ERROR` | [actions.ts:238](../src/actions.ts#L238), [:284-291](../src/actions.ts#L284-L291) |
| perform ended, not interrupted by the stop, mode `host` | `Action N: performed without approval (mode host).`, or `...; failed: ERROR.` | [actions.ts:286](../src/actions.ts#L286) |
| rejected | `Action N rejected by NAME.` | [actions.ts:188](../src/actions.ts#L188) |
| expired | `Action N expired.` | [actions.ts:158](../src/actions.ts#L158) |
| stop | `Cancelled K action.` or `Cancelled K actions.` when K > 0<br>`Stopped.` | [serve.ts:155](../src/serve.ts#L155), [:159](../src/serve.ts#L159) |
| membership messages | see [2.3](#23-membership-messages) | |

stderr: `CONTEXT: ERROR` from `logError` ([log.ts:35-37](../src/log.ts#L35-L37)), with CONTEXT one of `action N`, `action N card`, `control chat command`, `EVENT in the agent control chat`, `EVENT event`, `MCP`, `HTTP request`, `delivery wait`, `stop`, `simplex-mcp`. The library's download lines are written to stderr, and its event processing errors to stdout ([libs.ts:114](../../simplex-chat-nodejs/src/libs.ts#L114), [api.ts:187](../../simplex-chat-nodejs/src/api.ts#L187)).
