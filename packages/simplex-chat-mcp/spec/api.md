# SimpleX MCP server: API

> The five MCP tools, the Main group commands, replies and cards, the socket protocol, the CLI and the plugin of `@simplex-chat/mcp` 0.2.0.
>
> Related specs: [README](README.md) | [Architecture](architecture.md) | [Flows](flows.md)

**Source:** [`tools.ts`](../src/tools.ts) | [`control.ts`](../src/control.ts) | [`daemon.ts`](../src/daemon.ts) | [`protocol.ts`](../src/protocol.ts) | [`cli.ts`](../src/cli.ts) | [`config.ts`](../src/config.ts) | [`messages.ts`](../src/messages.ts) | [`actions.ts`](../src/actions.ts)

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
   - [1.9 Session process errors](#19-session-process-errors)
2. [Main group](#2-main-group)
   - [2.1 Commands and replies](#21-commands-and-replies)
   - [2.2 Cards](#22-cards)
   - [2.3 Messages and group profile](#23-messages-and-group-profile)
3. [Socket protocol](#3-socket-protocol)
4. [CLI](#4-cli)
   - [4.1 Commands](#41-commands)
   - [4.2 Exit codes and messages](#42-exit-codes-and-messages)
   - [4.3 Daemon log](#43-daemon-log)
5. [Plugin](#5-plugin)
   - [5.1 Options](#51-options)
   - [5.2 Files](#52-files)
   - [5.3 SessionStart output](#53-sessionstart-output)

---

## 1. Tools

### 1.1 Common rules

| Rule | Code |
|---|---|
| Tools are registered on the session's `McpServer` with description, input schema, output schema and, in mode `host`, `_meta: {"anthropic/requiresUserInteraction": true}` for `connect`, `send` and `leave` | [tools.ts:354-364](../src/tools.ts#L354-L364) |
| Tool descriptions and server instructions; the instructions name no profile, because they are sent before the daemon is reached | [tools.ts:301-332](../src/tools.ts#L301-L332), [session.ts:47](../src/session.ts#L47) |
| Arguments are validated by the SDK with Ajv; a failure is a result with `isError: true` and the text `Input validation error: Invalid arguments for tool NAME: ERRORS` | [mcp-BumlNux0.cjs:1812-1817](../node_modules/@modelcontextprotocol/server/dist/mcp-BumlNux0.cjs#L1812-L1817) |
| `additionalProperties: false` is set in every input schema | [tools.ts:177](../src/tools.ts#L177), [:192](../src/tools.ts#L192), [:203](../src/tools.ts#L203), [:206](../src/tools.ts#L206), [:214](../src/tools.ts#L214) |
| A result is sent as `structuredContent` and as its JSON in one text item | [tools.ts:372](../src/tools.ts#L372) |
| A tool error is sent as `isError: true` with one text item: the `ToolError` text, or the `errorText` of other errors | [tools.ts:374](../src/tools.ts#L374), [log.ts:30-34](../src/log.ts#L30-L34) |
| With a `progressToken` in `_meta`, `notifications/progress` is sent at the start of each wait and every 60 s, with `progress` 0, 1, 2, ... and a `message`; the daemon sends each message as a `{progress}` line and the session process forwards it | [tools.ts:995-1004](../src/tools.ts#L995-L1004), [messages.ts:202-206](../src/messages.ts#L202-L206), [daemon.ts:234](../src/daemon.ts#L234), [session.ts:99-103](../src/session.ts#L99-L103) |
| The call is aborted when Claude Code cancels the request: the session process closes its `/call` request and the daemon aborts the call's signal; the action is kept | [session.ts:99-103](../src/session.ts#L99-L103), [daemon.ts:228-230](../src/daemon.ts#L228-L230) |
| A call of the same session with the same tool and action fields as an action in memory is attached to it | [tools.ts:987-989](../src/tools.ts#L987-L989) |
| Messages are marked read after the last wait of a call that is not aborted | [tools.ts:462-463](../src/tools.ts#L462-L463), [:600-602](../src/tools.ts#L600-L602), [:682-683](../src/tools.ts#L682-L683) |

Action fields of the key, after the session number:

| Tool | Fields |
|---|---|
| `connect` | `target`<br>`incognito` (default `false`)<br>`message` (default `null`) |
| `send` | `chat`<br>`text`<br>`reply_to` (default `null`)<br>`mention` (default `null`) |
| `leave` | `chat` |

Result status of the three action tools:

| Status | Meaning | Extra fields |
|---|---|---|
| `done` | performed | per tool |
| `failed` | failure of the card post, the counter write or the perform, or loss of the Main group | `error` |
| `rejected` | an owner sent `/reject` | |
| `expired` | no decision before the expiry time | |
| `known` | `connect` only: the target is an existing chat past the prepared state; no action | chat fields |

### 1.2 `connect`

Handler: [`AgentTools.connect`](../src/tools.ts#L414-L427). Perform: [`performConnect`](../src/tools.ts#L482-L513). Schemas: [input](../src/tools.ts#L167-L178), [output](../src/tools.ts#L220-L230).

Input:

| Field | Type | Range | Default | Rule |
|---|---|---|---|---|
| `target` | string | 1 character or more | required | SimpleX link (`https://...`, `simplex:/...`), contact name `@name.simplex` or channel name `#name.simplex`; whitespace refused |
| `incognito` | boolean | | `false` | `true`: `incognito=on` in the connect command |
| `message` | string | 1 to 2000 characters | | first message of a contact request; plans with `takesMessage` only |
| `wait` | number | 0 to 86400 | 60 | seconds from the approval: for the other side to accept and, with `message`, for its first reply |
| `idle` | number | 1 to 300 | 30 | seconds without a new message, after which the wait for messages ends |

Output fields by case:

| Case | Fields | Code |
|---|---|---|
| `known` | `status`, chat fields of the known chat | [tools.ts:419](../src/tools.ts#L419) |
| `done`, chat unknown | `action`, `status`, `type`, `connected: false`, plan fields `name`, `peer_type`, `description`, `welcome`, `commands`, `known_as` when present | [tools.ts:430](../src/tools.ts#L430), [:898-904](../src/tools.ts#L898-L904) |
| `done`, chat known | `action`, `status`, chat fields, `messages` | [tools.ts:446-466](../src/tools.ts#L446-L466) |
| `failed` | `action`, `status`, `error` | [tools.ts:437](../src/tools.ts#L437) |
| `rejected`, `expired` | `action`, `status` | [tools.ts:437](../src/tools.ts#L437) |

Chat fields ([`chatFields`](../src/tools.ts#L739-L766)):

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
| `role` | | the session profile's member role |
| `connected` | active connection status `ready` | membership status `connected` or `complete` |

`messages`: message items among the last 50 items of the chat when the result is built, replaced by their collected copies, followed by the items collected since the connection; live items are left out; new items are marked read ([tools.ts:448](../src/tools.ts#L448), [:458-465](../src/tools.ts#L458-L465)).

Tool errors:

| Text | Condition | Code |
|---|---|---|
| `Not a SimpleX link or name: TARGET` | whitespace in `target`, or `invalidConnReq` from the plan command | [tools.ts:470](../src/tools.ts#L470), [:476](../src/tools.ts#L476) |
| an app error text, for example `Name not found: This SimpleX name is not registered. Please check the name.`, or `SimpleX error: CHAIN: DETAILS` | other errors of the plan command, or a plan of type `error` | [tools.ts:477](../src/tools.ts#L477), [:825](../src/tools.ts#L825), [log.ts:36-38](../src/log.ts#L36-L38) |
| `TARGET can't be joined now: PLAN.` | `invitationLink ownLink` or `connecting`<br>`contactAddress ownLink` or `connectingProhibit`<br>`groupLink ownLink`, `connectingProhibit`, `noRelays` or `updateRequired` | [tools.ts:816](../src/tools.ts#L816), [:829](../src/tools.ts#L829), [:847](../src/tools.ts#L847), [:869](../src/tools.ts#L869) |
| `message: TARGET is not a contact or business address with short link data, or its contact exists without a connection.` | `message` given and the plan has `takesMessage: false` | [tools.ts:421](../src/tools.ts#L421) |
| refusals | see [1.8](#18-action-refusals) | |

`takesMessage` is `true` for a contact address with short link data, for a known prepared contact, and for a known prepared business group ([tools.ts:820](../src/tools.ts#L820), [:840](../src/tools.ts#L840), [:858](../src/tools.ts#L858)).

### 1.3 `send`

Handler: [`AgentTools.send`](../src/tools.ts#L574-L590). Result: [`sendResult`](../src/tools.ts#L592-L610). Perform: [`performSend`](../src/tools.ts#L612-L631). Schemas: [input](../src/tools.ts#L180-L193), [output](../src/tools.ts#L232-L244).

Input:

| Field | Type | Range | Default | Rule |
|---|---|---|---|---|
| `chat` | string | | required | `@N` or `#N` of the session |
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
| `No chat CHAT. list shows the agent's chats.` | invalid reference, or a store error when the chat is read | [tools.ts:707-714](../src/tools.ts#L707-L714), [:983-985](../src/tools.ts#L983-L985) |
| `The agent is not connected to CHAT and can't post; list shows connected: false.` | the chat is not connected | [tools.ts:577](../src/tools.ts#L577) |
| `The agent has role ROLE in CHAT and can't post.` | membership role `observer` or `relay` | [tools.ts:578-581](../src/tools.ts#L578-L581) |
| `reply_to: CHAT has no item N.` | the read of `around=N` fails, or the item is absent from it | [tools.ts:717-719](../src/tools.ts#L717-L719) |
| `reply_to: item N in CHAT is not a message.` | the item's content is not `rcvMsgContent` or `sndMsgContent` | [tools.ts:720](../src/tools.ts#L720) |
| `mention: CHAT is a contact chat; mentions apply to groups and channels.` | `mention` with `@N` | [tools.ts:725](../src/tools.ts#L725) |
| `mention: CHAT has no member N.` | ID absent from `/_members` | [tools.ts:730](../src/tools.ts#L730) |
| `mention: members N and M have the same name.` | two mentioned members with the same display name | [tools.ts:733](../src/tools.ts#L733) |
| refusals | see [1.8](#18-action-refusals) | |

### 1.4 `receive`

Handler: [`AgentTools.receive`](../src/tools.ts#L655-L689). Schemas: [input](../src/tools.ts#L195-L204), [output](../src/tools.ts#L246-L254).

Input:

| Field | Type | Range | Default | Rule |
|---|---|---|---|---|
| `chat` | string | | required | `@N` or `#N` of the session |
| `wait` | number | 0 to 86400 | 0 | seconds to wait when no message is new |
| `limit` | integer | 1 to 100 | 50 | most messages returned |

Output (all fields always present):

| Field | Value |
|---|---|
| `chat` | the chat reference |
| `messages` | unread received messages in chat order, then received messages that arrived during the call; live items left out; at most `limit` |
| `more` | new messages not returned, overflow, and unread items left unread by the page reads |

Tool errors: `No chat CHAT. list shows the agent's chats.` for an invalid reference or a store error of the first read, `initial=1` ([tools.ts:657](../src/tools.ts#L657), [:661-663](../src/tools.ts#L661-L663)); other chat errors, a store error of a later page read included, as their error text ([log.ts:30-38](../src/log.ts#L30-L38)).

Messages are read by `receive` without an action or an approval.

### 1.5 `list`

Handler: [`AgentTools.listChats`](../src/tools.ts#L691-L699). Schemas: [input](../src/tools.ts#L206), [output](../src/tools.ts#L256-L288).

Input: an empty object.

Output:

| Field | Item fields |
|---|---|
| `chats` | `chat`<br>`type`<br>`name`<br>`simplex_name`, when verified<br>`peer_type`, when present<br>`incognito`<br>`known_as`, when incognito<br>`connected`<br>`unread`: `chatStats.unreadCount`<br>`approved: true`, only under a standing approval in mode `requests` |
| `actions` | `action`<br>`tool`<br>`input`: the input of the creating call<br>`state`<br>`open`: `true` while a call waits on it |

`chats`: the contacts and groups of the session's profile from one read, `/_get chats USER pcc=off count=1000000`, that have a chat reference ([tools.ts:701-705](../src/tools.ts#L701-L705)). `actions`: the session's actions in memory ([tools.ts:697](../src/tools.ts#L697)).

### 1.6 `leave`

Handler: [`AgentTools.leave`](../src/tools.ts#L633-L642). Perform: [`performLeave`](../src/tools.ts#L648-L653). Schemas: [input](../src/tools.ts#L208-L215), [output](../src/tools.ts#L290-L299).

Input: `chat`, string, required, `@N` or `#N` of the session.

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

Tool errors: `No chat CHAT. list shows the agent's chats.` when no action with the same key is in memory and the chat is invalid or unknown ([tools.ts:636-638](../src/tools.ts#L636-L638)); refusals.

### 1.7 Message and command objects

Message ([`Message`](../src/messages.ts#L11-L22), [`toMessage`](../src/messages.ts#L138), [schema](../src/tools.ts#L119-L139)):

| Field | Type | Presence | Source |
|---|---|---|---|
| `chat` | string | always | chat reference of the item |
| `item` | integer | always | `meta.itemId` |
| `from` | string | always | `groupRcv`: member display name<br>`directRcv`: contact display name<br>`channelRcv`: channel display name<br>sent items in `connect`: the session profile's name in that chat |
| `from_member` | integer | `groupRcv` | `groupMemberId` |
| `from_type` | string | when the profile has a peer type | sender's `peerType`; the session profile's own for its non-incognito items in `connect` |
| `time` | string | always | `meta.itemTs` |
| `text` | string | always | `msgContent.text` |
| `new` | boolean | always | item status `rcvNew` when read |
| `reply_to` | object | quoted item with an ID | `{chat, item: quotedItem.itemId}` |
| `mentions_me` | boolean | always | `meta.userMention` |

Only received message items (`rcvMsgContent`) are returned by `send` and `receive`; `connect` also returns sent ones (`sndMsgContent`) ([messages.ts:73](../src/messages.ts#L73), [:126-128](../src/messages.ts#L126-L128)).

Command ([`commandsOf`](../src/tools.ts#L969-L976), [schema](../src/tools.ts#L141-L151)):

| Form | Fields |
|---|---|
| command | `keyword`<br>`label`<br>`params`, when present |
| menu | `label`<br>`commands`: commands and menus |

### 1.8 Action refusals

Refusals of a new `connect`, `send` or `leave` action, first match ([actions.ts:143-148](../src/actions.ts#L143-L148), [control.ts:107-115](../src/control.ts#L107-L115)):

| Text | Condition |
|---|---|
| `The server is stopping.` | `Actions.stop` has started |
| `The Main group is lost. Run /simplex:reset-owners in Claude Code.` | Main group lost |
| `20 actions are pending; wait for the owner's decisions.` | 20 actions of this session in state `pending` |
| `20 actions are being performed; wait for their results.` | 20 actions of this session in state `performing` |
| `text: too long for its approval card; shorten it.` | `send` whose largest card form exceeds 15000 bytes |
| `message: too long for its approval card; shorten it.` | `connect` whose largest card form exceeds 15000 bytes without the link texts |
| `The agent has no owner. Open this link in the SimpleX app and join: LINK` | no owner, a link exists |
| `The agent has no owner. Run /simplex:link in Claude Code.` | no owner, no link |

Largest card form ([control.ts:441-449](../src/control.ts#L441-L449)): the maximum of the creation form, the `done` form and a `failed` form with an error of 600 × `€`, each with the session line, with the action's own decider or the longest current owner name; each form is measured as JSON-escaped UTF-8: `"`, `\`, `\n`, `\r`, `\t` 2 bytes, other characters below U+0020 6 bytes.

Link texts of a `connect` card ([control.ts:111](../src/control.ts#L111), [:463-477](../src/control.ts#L463-L477)): the description and welcome lines take the bytes that the largest form leaves without them. Lines are kept while they fit; the next line is cut to the bytes left and ends in `…`; later lines are left out. The tool result keeps both texts whole.

### 1.9 Session process errors

Tool errors produced by the session process instead of the daemon ([session.ts:34-53](../src/session.ts#L34-L53), [:81-113](../src/session.ts#L81-L113), [:143-150](../src/session.ts#L143-L150)):

| Text | Condition |
|---|---|
| `Plugin option KEY: EXPECTED expected, got VALUE. Change it in /plugin > Installed > simplex > Configure options.` | an invalid option at the session's start; every call |
| `The SimpleX daemon stopped; repeat the call.` | the daemon is not running, is stopping (503), or its stream ended without a result |
| `Restart Claude Code to update the SimpleX plugin.` | the daemon speaks another protocol version |
| a spawn error: `The SimpleX daemon exited; see ROOT/daemon.log`, `The SimpleX daemon did not start in 5 minutes.`, or the daemon's `error: TEXT` | the daemon could not start |
| `Unknown session; restart the Claude Code session.` | the session has no folder and no `/session` succeeded, after one retry |
| `Cancelled.` | the daemon returned no result for a call that was not aborted |

---

## 2. Main group

### 2.1 Commands and replies

Accepted text ([`parseCommand`](../src/control.ts#L374-L379), [`ownerRequest`](../src/control.ts#L540-L550)):

```
message   one line, an optional trailing newline
line      spaces, "/", KEYWORD, PARAMS
KEYWORD   characters up to the first whitespace
PARAMS    the rest, trimmed, split on whitespace
CHAT      @N or #N, 1 to 15 digits
TARGET    S:CHAT, S a session number from 1
requests
  approve N
  approve TARGET N
  reject N
  reject TARGET N
  approve TARGET all
  revoke TARGET
  pending
  list
```

The daemon ignores text that does not start with `/` or has several lines. It answers a command with a reply that quotes it ([control.ts:146](../src/control.ts#L146)), and it runs the expiry check before each command ([control.ts:169](../src/control.ts#L169)).

| Request | Condition | Reply | Code |
|---|---|---|---|
| one-line `/` text outside the requests | any sender | `Commands: /approve, /reject, /revoke, /pending, /list. Each card shows its commands.` | [control.ts:13](../src/control.ts#L13), [:171](../src/control.ts#L171) |
| any request | sender not an owner | `Only owners approve actions.` | [control.ts:172](../src/control.ts#L172) |
| `approve`, `reject` | action N absent, not `pending`, or without a card | `Action N is not pending.` | [control.ts:188](../src/control.ts#L188) |
| `approve TARGET N`, `reject TARGET N` | action N is a `connect`, or for another session or chat | `Action N is not for S:CHAT.` | [control.ts:189-191](../src/control.ts#L189-L191) |
| `approve`, `reject` | accepted | none; the card is edited at the outcome | [control.ts:192-195](../src/control.ts#L192-L195) |
| `approve TARGET all` | mode `host`; nothing is stored | `Approvals apply in mode requests; this server runs in mode host.` | [control.ts:181](../src/control.ts#L181) |
| `approve TARGET all`, `revoke TARGET` | session S not open | `No open session S. /list shows the open sessions.` | [control.ts:225](../src/control.ts#L225), [:240](../src/control.ts#L240), [:536-538](../src/control.ts#L536-L538) |
| `approve TARGET all` | CHAT unknown in session S | `No chat S:CHAT. /list shows the agents' chats.` | [control.ts:227](../src/control.ts#L227) |
| `approve TARGET all` | accepted | `Approved for KIND NAME (S:CHAT) by APPROVER. /'revoke S:CHAT'` | [control.ts:235](../src/control.ts#L235) |
| `revoke TARGET` | CHAT unknown, or without a standing approval | `Messages to S:CHAT have no standing approval.` | [control.ts:242](../src/control.ts#L242) |
| `revoke TARGET` | accepted | `Messages to KIND NAME (S:CHAT) need approval again.` | [control.ts:245](../src/control.ts#L245) |
| `pending` | no pending action with a card | `No pending actions.` | [control.ts:176](../src/control.ts#L176) |
| `pending` | otherwise | one line per action: `N  session S: WHAT  expires YYYY-MM-DD HH:MM UTC` | [control.ts:175](../src/control.ts#L175), [:437-439](../src/control.ts#L437-L439) |
| `list` | no open session | `No open sessions.` | [control.ts:250](../src/control.ts#L250) |
| `list` | otherwise | per open session: `Session S · NAME`, then one line per chat, at most 100: `S:CHAT  KIND NAME`, with `, approved for this chat by APPROVER` and `, not connected` when applicable, cut to 100 bytes with `…`; then `K older chats are not shown.` when more exist | [control.ts:248-265](../src/control.ts#L248-L265) |

KIND is `contact`, `bot`, `agent`, `business`, `group` or `channel` ([tools.ts:927-934](../src/tools.ts#L927-L934)). WHAT is `connect to SUBJECT`, `send to SUBJECT`, `delete SUBJECT` or `leave SUBJECT` ([actions.ts:290-300](../src/actions.ts#L290-L300)). Names are shown by `safeName` ([control.ts:491-494](../src/control.ts#L491-L494)).

### 2.2 Cards

Card text: the session line, request lines, then one last line ([`cardText`](../src/control.ts#L381-L400), [`requestLines`](../src/control.ts#L402-L423)).

Session line: `Session S · PROFILE NAME`, or `Session S` when the session is not open ([control.ts:218-221](../src/control.ts#L218-L221)).

Request lines:

| Tool | First line | Further lines |
|---|---|---|
| `connect` | `I want to connect to SUBJECT, as myself.` or `, incognito.`<br>done and connected: `I connected to ...`<br>done and not connected: `I asked to connect to ...` | link description, one code span per line<br>welcome, one code span per line<br>both cut to fit the card ([1.8](#18-action-refusals))<br>with `message`: `With the message:` and its lines |
| `send` | `I want to send a message to SUBJECT:`, with `, where I am incognito` before the colon in incognito chats<br>done: `I sent a message to ...` | text, one code span per line<br>`replying to NAME: QUOTE` with `reply_to`<br>`mentioning @A, @'B C'` with `mention` |
| `leave` | `I want to delete KIND NAME (@N).` or `I want to leave KIND NAME (#N).`<br>done: `I deleted ...` or `I left ...` | |

SUBJECT is `KIND NAME`, or `an unnamed KIND`, followed by ` (SIMPLEX_NAME)` when known ([actions.ts:285-288](../src/actions.ts#L285-L288)). For `connect`, SIMPLEX_NAME is the target when it is a name; for `send`, the chat's verified SimpleX name.

Last line:

| State | Condition | Last line |
|---|---|---|
| `pending` | | command line |
| `pending` | interrupted | `Cancelled: the server stopped.` |
| `performing` | interrupted | `DECIDER; interrupted: the server stopped.` |
| `done` | standing | `Approved for this chat by NAME. /'revoke S:CHAT'` |
| `done` | | `DECIDER.` |
| `failed` | | ``DECIDER; failed: `ERROR`.`` |
| `rejected` | | `Rejected by NAME.` |
| `expired` | | `Expired.` |

DECIDER is `Performed without approval (mode host)` in mode `host`, `Approved for this chat by NAME` for a standing send, and `Approved by NAME` otherwise ([control.ts:383](../src/control.ts#L383)).

Command lines ([`commandsLine`](../src/control.ts#L425-L435)):

| Tool | Command line |
|---|---|
| `connect` | `/'approve N' or /'reject N'` |
| `send` | `/'approve S:CHAT N', /'approve S:CHAT all' or /'reject S:CHAT N'` |
| `leave` | `/'approve S:CHAT N' or /'reject S:CHAT N'` |

The app shows `/'approve N'` as the tappable command `/approve N`.

### 2.3 Messages and group profile

| Moment | Text in the Main group | Code |
|---|---|---|
| the first owner joined | `I'm your agents' Main group. Your Claude Code sessions ask here before they connect or send.` | [control.ts:20](../src/control.ts#L20), [:271](../src/control.ts#L271) |

Group profile:

| Field | Value | Code |
|---|---|---|
| display name | option `main_group`, updated at start and at a changed option | [control.ts:95-96](../src/control.ts#L95-L96), [:200-203](../src/control.ts#L200-L203) |
| full name | empty | [control.ts:341](../src/control.ts#L341) |
| description | `Supervise your AI agents here: approve or reject their connections and messages with one tap.` | [control.ts:342](../src/control.ts#L342) |
| history | on | [control.ts:343](../src/control.ts#L343) |
| command menu | `/pending` "Pending actions"<br>`/list` "Chats and approvals"<br>updated at start when it differs | [control.ts:8-11](../src/control.ts#L8-L11), [:95-96](../src/control.ts#L95-L96) |

The hub profile that posts in the group has the same display name and peer type `agent` ([hub.ts:27](../src/hub.ts#L27), [core.ts:25](../src/core.ts#L25)).

---

## 3. Socket protocol

HTTP/1.1 over `ROOT/daemon.sock` (or the fallback path, or the Windows pipe), protocol version `1`. Every request carries `Authorization: Bearer TOKEN`; bodies are JSON; streamed replies are NDJSON with content type `application/x-ndjson`. Errors are `{"error": TEXT}` with a status other than 200. Types: [`protocol.ts`](../src/protocol.ts); routes: [`Daemon.routes`](../src/daemon.ts#L103-L116).

| Route | Request | Reply | Code |
|---|---|---|---|
| `POST /session` | `{id, name?, project, config: {mainGroup, approve, expireHours}}` | `{number, displayName, version, protocol, owner: {owners, link?}}` | [daemon.ts:162-170](../src/daemon.ts#L162-L170) |
| `POST /title` | `{id, name?, project, config}` | `{mainGroup, owner: {owners, link?}, version, protocol}` | [daemon.ts:172-208](../src/daemon.ts#L172-L208) |
| `POST /attach` | `{id}` | stream held open; first line `{version, protocol}` | [daemon.ts:210-221](../src/daemon.ts#L210-L221) |
| `POST /call` | `{id, tool, input}` | lines `{progress: TEXT}`, then one line `{result: CallToolResult or null}`; closing the request aborts the call | [daemon.ts:223-240](../src/daemon.ts#L223-L240) |
| `GET /status` | none | `{version, protocol, mainGroup, owner: {owners, link?}, sessions: [{number, name, attached}]}` | [daemon.ts:328-331](../src/daemon.ts#L328-L331) |
| `POST /reset-owners` | none | `{link}` | [daemon.ts:110](../src/daemon.ts#L110) |
| `POST /stop` | none | `{}` after the stop sequence; the process exits after the reply | [daemon.ts:111-114](../src/daemon.ts#L111-L114) |

| Status | Error | Condition |
|---|---|---|
| 401 | `Unauthorized` | token missing or wrong |
| 404 | `Not found` | other method or path |
| 400 | `Invalid JSON` | body not JSON |
| 400 | `Not a Claude Code session ID: ID` | `/session`, `/title`, `/attach` or `/call` with an ID that is not a UUID |
| 409 | `Unknown session; restart the Claude Code session.` | `/attach` or `/call` for an ID without a folder |
| 413 | `Body too large` | body over 1 MiB |
| 503 | `The SimpleX daemon is stopping.` | `/session`, `/title`, `/attach` or `/call` while the daemon stops |
| 500 | the error message | any other error |

`/session` and `/title` apply `config` to the hub: mode and expiry for all sessions, and a rename when `mainGroup` differs ([hub.ts:63-71](../src/hub.ts#L63-L71)).

`/title` with a non-blank name renames an open or opening profile, writes the name to the `session.json` of a stored session, or remembers it in memory for an ID without one, at most 100 IDs, the oldest dropped. It creates no entry, folder or number, and leaves the idle timers alone. A new session's `/session` without a name takes the remembered title, and the daemon forgets it.

---

## 4. CLI

### 4.1 Commands

Binary: `simplex-mcp`, compiled `dist/cli.js` ([package.json:5-7](../package.json#L5-L7)). Help text: [cli.ts:8-21](../src/cli.ts#L8-L21).

| Command | Run by | Output |
|---|---|---|
| `simplex-mcp session` | `.mcp.json` | MCP over stdio |
| `simplex-mcp daemon` | `spawnDaemon` | ready line on fd `SIMPLEX_MCP_READY_FD` (stdout when unset): `ready`, `running` or `error: TEXT`; log on stdout and stderr |
| `simplex-mcp hook session-start` | SessionStart hook | one JSON line ([5.3](#53-sessionstart-output)) |
| `simplex-mcp link` | `/simplex:link`, people | `Owners of "MAIN": A, B. To replace them, run /simplex:reset-owners.`, or `Join "MAIN" in the SimpleX app. The link works once:`, a QR code on a terminal, and the link, or `The Main group has no link. Run /simplex:reset-owners.` |
| `simplex-mcp reset-owners` | `/simplex:reset-owners`, people | `"MAIN" was replaced. Join the new one in the SimpleX app. The link works once:`, a QR code on a terminal, and the link |
| `simplex-mcp --help` | people | the help text |

Environment: `SIMPLEX_MCP_HOME` (data root), `CLAUDE_CODE_SESSION_ID` and `CLAUDE_PROJECT_DIR` (session), `CLAUDE_PLUGIN_OPTION_MAIN_GROUP`, `CLAUDE_PLUGIN_OPTION_APPROVE`, `CLAUDE_PLUGIN_OPTION_EXPIRE_HOURS` (options), `SIMPLEX_MCP_READY_FD` (daemon).

### 4.2 Exit codes and messages

| Code | Message | Code location |
|---|---|---|
| 0 | the help text on stdout; the hook, also after errors; `link` and `reset-owners`; the daemon after a stop or with `running` | [cli.ts:42-57](../src/cli.ts#L42-L57), [daemon.ts:342](../src/daemon.ts#L342), [:371](../src/daemon.ts#L371) |
| 2 | `Unknown command. Run simplex-mcp --help.` | [cli.ts:36](../src/cli.ts#L36) |
| 2 | `simplex-mcp session runs from the Claude Code plugin.` | [session.ts:36](../src/session.ts#L36) |
| 2 | `Not a Claude Code session ID: ID` | [session.ts:37](../src/session.ts#L37) |
| 1 | `Restart Claude Code to update the SimpleX plugin.` (`link`, `reset-owners`) | [client.ts:42](../src/client.ts#L42) |
| 1 | `The SimpleX daemon exited; see ROOT/daemon.log`, `The SimpleX daemon did not start in 5 minutes.`, or the daemon's error text (`link`, `reset-owners`) | [client.ts:81](../src/client.ts#L81), [:108](../src/client.ts#L108) |
| 1 | daemon start failed: ready line `error: TEXT` | [daemon.ts:343-344](../src/daemon.ts#L343-L344) |
| 1 | `stop: ERROR` for a failed stop after a signal | [daemon.ts:348-353](../src/daemon.ts#L348-L353) |
| 1 | `simplex-mcp: ERROR` for other errors | [cli.ts:73-74](../src/cli.ts#L73-L74) |

### 4.3 Daemon log

`daemon.log` receives the daemon's stdout and stderr ([client.ts:74](../src/client.ts#L74)). stdout lines ([log.ts:22-24](../src/log.ts#L22-L24)):

| Moment | Line | Code |
|---|---|---|
| start | `SimpleX MCP daemon VERSION, data folder ROOT` | [daemon.ts:346](../src/daemon.ts#L346) |
| Main group created | `Main group "NAME" created.` | [control.ts:347](../src/control.ts#L347) |
| no owners, members present | `No owners in the Main group.` | [control.ts:103](../src/control.ts#L103), [:163](../src/control.ts#L163) |
| link opened | `Main group link (works once): LINK`<br>`Whoever joins by this link becomes an owner of "NAME" and approves the agents' actions.` | [control.ts:283-284](../src/control.ts#L283-L284) |
| owner joined | `NAME joined (member ID) and is now an owner. The link is deleted.` | [control.ts:272](../src/control.ts#L272) |
| Main group lost | `The agent lost its Main group. Run /simplex:reset-owners in Claude Code.` | [control.ts:277](../src/control.ts#L277) |
| action created | `Action N of session S: WHAT` with `, as itself` or `, incognito` for `connect` and ` (CHAT)` for `send` and `leave` | [actions.ts:151-152](../src/actions.ts#L151-L152), [:290-300](../src/actions.ts#L290-L300) |
| counter write or card post failed | `Action N: failed: ERROR.` | [actions.ts:239](../src/actions.ts#L239) |
| perform ended, not interrupted, mode `requests` | `Action N approved by NAME: RESULT.` with RESULT `sent`, `deleted`, `left`, `connected`, `connection requested` or `failed: ERROR` | [actions.ts:260](../src/actions.ts#L260), [:306-313](../src/actions.ts#L306-L313) |
| perform ended, not interrupted, mode `host` | `Action N: performed without approval (mode host).`, or `...; failed: ERROR.` | [actions.ts:308](../src/actions.ts#L308) |
| rejected | `Action N rejected by NAME.` | [actions.ts:207](../src/actions.ts#L207) |
| expired | `Action N expired.` | [actions.ts:177](../src/actions.ts#L177) |
| stop | `Cancelled K action.` or `Cancelled K actions.` when K > 0<br>`Stopped.` | [daemon.ts:128](../src/daemon.ts#L128), [:134](../src/daemon.ts#L134) |

stderr: `CONTEXT: ERROR` from `logError` ([log.ts:26-28](../src/log.ts#L26-L28)), with CONTEXT one of `action N`, `action N card`, `Main group command`, `EVENT in the Main group`, `Main group rename`, `owner reset`, `EVENT event`, `METHOD PATH`, `delivery wait`, `close session`, `stop`. The library's download lines are written to stderr. The session process writes `SimpleX daemon: ERROR` to its own stderr when its background connection fails ([session.ts:72](../src/session.ts#L72)).

---

## 5. Plugin

### 5.1 Options

`userConfig` of [`.claude-plugin/plugin.json`](../.claude-plugin/plugin.json), read by [`readConfig`](../src/config.ts#L25-L31):

| Key | Variable | Type | Default | Valid | Error |
|---|---|---|---|---|---|
| `main_group` | `CLAUDE_PLUGIN_OPTION_MAIN_GROUP` | string | `SimpleX MCP` | equal to `validName(value)` | `a valid SimpleX name, such as VALID expected` or `a valid SimpleX name expected` |
| `approve` | `CLAUDE_PLUGIN_OPTION_APPROVE` | string | `requests` | `requests`, `host` | `requests or host expected` |
| `expire_hours` | `CLAUDE_PLUGIN_OPTION_EXPIRE_HOURS` | number | 23 | digits only, 1 to 23 | `hours from 1 to 23 expected` |

A missing or blank variable takes the default. The error text is `Plugin option KEY: EXPECTED, got VALUE. Change it in /plugin > Installed > simplex > Configure options.` ([config.ts:53-55](../src/config.ts#L53-L55)). The hook receives the variables from Claude Code; the MCP server gets them from `.mcp.json`, which maps `${user_config.KEY}` to the same names.

### 5.2 Files

| File | Content |
|---|---|
| [`.claude-plugin/plugin.json`](../.claude-plugin/plugin.json) | name `simplex`, version `0.2.0` (equal to `package.json`), description, author, homepage, repository, license, keywords, `userConfig` |
| [`.mcp.json`](../.mcp.json) | server `simplex`: `node ${CLAUDE_PLUGIN_ROOT}/plugin/launch.js session` with the three option variables |
| [`hooks/hooks.json`](../hooks/hooks.json) | SessionStart: `node "${CLAUDE_PLUGIN_ROOT}/plugin/launch.js" hook session-start`, timeout 600 s; PreToolUse, matcher `mcp__plugin_simplex_simplex__.*`: `node "${CLAUDE_PLUGIN_ROOT}/plugin/allow.js"` |
| [`plugin/launch.js`](../plugin/launch.js) | `npx -y @simplex-chat/mcp@VERSION ARGS` (`npx.cmd` through the shell on Windows), stdio inherited, SIGINT and SIGTERM forwarded, the child's exit code passed on (1 after a signal); a spawn error prints `simplex-mcp: ERROR` and exits 1 |
| [`plugin/allow.js`](../plugin/allow.js) | prints `{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"allow"}}` |
| [`skills/link/SKILL.md`](../skills/link/SKILL.md) | `/simplex:link`: runs `launch.js link`, shows its output; `disable-model-invocation: true` |
| [`skills/reset-owners/SKILL.md`](../skills/reset-owners/SKILL.md) | `/simplex:reset-owners`: runs `launch.js reset-owners`, shows its output; `disable-model-invocation: true` |
| [`/.claude-plugin/marketplace.json`](../../../.claude-plugin/marketplace.json) | marketplace `simplex-chat`, plugin `simplex`, source `{"source": "npm", "package": "@simplex-chat/mcp"}` |

`package.json` `files` ships these with `dist` and `README.md` ([package.json:8-16](../package.json#L8-L16)). The plugin runs from the package root without `node_modules`; `launch.js` and `allow.js` use no dependencies.

### 5.3 SessionStart output

Hook input fields used: `session_id`, `session_title`, `cwd` ([hook.ts:13-17](../src/hook.ts#L13-L17)). The hook posts `/title`, and MAIN is the reply's `mainGroup`. Output ([hook.ts:29-47](../src/hook.ts#L29-L47)):

| Case | Output |
|---|---|
| Node.js below 22 | `{"systemMessage": "SimpleX MCP needs Node.js 22 or later; found V."}` |
| owners present, or no link | `{}` |
| no owner, a link | `systemMessage`: `SimpleX MCP: scan this QR code with the SimpleX app and tap Join to supervise your agents in "MAIN":`, the `plain` QR code and the link<br>`hookSpecificOutput`: `{hookEventName: "SessionStart", additionalContext: "The SimpleX Main group \"MAIN\" has no owner yet. Tell your user to open this link in the SimpleX app and join: LINK"}` |
| daemon not running after the start | `{"systemMessage": "SimpleX MCP: The SimpleX daemon stopped; it starts again with the next tool call."}` |
| other errors, invalid options included | `{"systemMessage": "SimpleX MCP: TEXT"}` |
