# SimpleX MCP server: flows

> Sequence diagrams of the main flows, each with notes and source locations.
>
> Related specs: [README](README.md) | [Architecture](architecture.md) | [API](api.md)

Participants:

| Name | Participant |
|---|---|
| Claude | Claude Code: the model, its MCP client and its hooks |
| Hook | `simplex-mcp hook session-start` |
| Session | `simplex-mcp session`, the stdio MCP server of one Claude Code session |
| Daemon | `simplex-mcp daemon`: listener, routes and session registry |
| Hub | `Hub` with `Control` and `Actions`, and the hub core |
| Tools | `AgentTools` of one session, with its session core |
| Owner | the owner in the SimpleX app |
| Peer | a contact, bot, group or channel |

`#G` is the Main group, `S` a session number, `ID` a Claude Code session ID, `LINK` a link as `connFullLink [connShortLink]`. Chat commands are shown as the daemon sends them. Card and progress texts with a semicolon are shown with a comma in the diagrams; the exact texts are in the notes and in [API](api.md).

---

## Table of contents

1. [First session start and the owner joins](#1-first-session-start-and-the-owner-joins)
2. [Session process start and attach](#2-session-process-start-and-attach)
3. [`connect` with approval](#3-connect-with-approval)
4. [`send` with responses](#4-send-with-responses)
5. [`receive` with `wait`](#5-receive-with-wait)
6. [`send` under a standing approval, and `/revoke`](#6-send-under-a-standing-approval-and-revoke)
7. [`leave`](#7-leave)
8. [Mode `host`](#8-mode-host)
9. [Expiry](#9-expiry)
10. [A lost call and its re-attachment](#10-a-lost-call-and-its-re-attachment)
11. [Idle close and idle stop](#11-idle-close-and-idle-stop)
12. [Daemon stop with pending actions](#12-daemon-stop-with-pending-actions)
13. [Daemon crash and a new daemon](#13-daemon-crash-and-a-new-daemon)
14. [A newer version](#14-a-newer-version)
15. [`reset-owners`](#15-reset-owners)

---

## 1. First session start and the owner joins

```mermaid
sequenceDiagram
  participant C as Claude
  participant K as Hook
  participant D as Daemon
  participant H as Hub
  participant O as Owner
  C->>K: SessionStart {session_id, session_title, cwd}
  K->>K: Node.js 22 check, options
  K->>K: ROOT mode 0700, token
  K->>D: GET /status (ENOENT: no daemon)
  K->>D: spawn dist/cli.js daemon, detached, fd 3 pipe, log to daemon.log
  D->>D: lock, session numbers
  D->>H: Hub.open(options)
  H->>H: /_create user {"displayName":"SimpleX MCP","peerType":"agent"}
  H->>H: /network socks=off smp-proxy=always smp-proxy-fallback=protected
  H->>H: startChat, events buffered
  H->>H: /_groups USER (no marker)
  H->>H: /_group USER {SimpleX MCP, history on, menu pending and list}
  H->>H: /_set custom #G {"simplexMcp":{"control":true,"nextAction":1}}
  H->>H: load: /_members #G
  H->>H: start: /_delete link #G, /_create link #G member
  D->>D: listen on daemon.sock, idle stop timer
  D->>K: fd 3: ready
  K->>D: GET /status
  K->>D: POST /title {id, name, project, config}
  D->>H: configure(config)
  D->>D: no entry, no session.json: remember NAME for ID
  D-->>K: {mainGroup, owner: {owners: [], link}, version, protocol}
  K-->>C: systemMessage: QR code and link; additionalContext: link
  O->>H: joins by the link
  H->>H: joinedGroupMember: /_delete link #G, /_member role #G M owner
  H->>O: I'm your agents' Main group. Your Claude Code sessions ask here before they connect or send.
  H->>H: log: NAME joined (member M) and is now an owner. The link is deleted.
```

Notes:

- At first use, libsimplex and the Node.js add-on are downloaded, with one `Downloading ...` line each in `daemon.log`; the first `npx` run also fills the npx cache. The hook's timeout is 600 s, and the spawner waits for the ready line at most 5 minutes ([hooks.json](../hooks/hooks.json), [client.ts:108](../src/client.ts#L108)).
- The hook records the title and creates no profile, folder or number; the session process creates the profile ([2](#2-session-process-start-and-attach)). When the session process reaches the daemon first, `/title` renames its profile instead ([daemon.ts:172-194](../src/daemon.ts#L172-L194)).
- The profile name is `SimpleX MCP - ` and the session title, else the project folder name, else `Claude Code`, through `validName` ([agent.ts:29-37](../src/agent.ts#L29-L37)).
- The QR code in the hook message is `plain`: block characters without colors ([qr.ts:27-31](../src/qr.ts#L27-L31)).
- With an owner present, the hook output is `{}`.
- The daemon runs `ownerJoined` for a `joinedGroupMember` event of a human while no owner exists ([control.ts:161](../src/control.ts#L161), [:267-273](../src/control.ts#L267-L273)).

Source: [hook.ts:29-47](../src/hook.ts#L29-L47), [client.ts:32-89](../src/client.ts#L32-L89), [daemon.ts:79-96](../src/daemon.ts#L79-L96), [:334-356](../src/daemon.ts#L334-L356), [hub.ts:26-48](../src/hub.ts#L26-L48), [core.ts:21-35](../src/core.ts#L21-L35), [control.ts:92-105](../src/control.ts#L92-L105), [:280-285](../src/control.ts#L280-L285), [:338-349](../src/control.ts#L338-L349), [daemon.ts:172-208](../src/daemon.ts#L172-L208).

---

## 2. Session process start and attach

```mermaid
sequenceDiagram
  participant C as Claude
  participant S as Session
  participant D as Daemon
  C->>S: spawn: launch.js session (CLAUDE_CODE_SESSION_ID, options)
  S->>S: session ID check, options
  S-->>C: initialize: instructions, five tools
  par background connect
    S->>D: GET /status, version and protocol check
    S->>D: POST /session {id, project, config}
    D->>D: _sessions/ID/session.json {number: 1, name: remembered NAME}
    D->>D: open core "SimpleX MCP - NAME"
    D-->>S: {number, displayName, owner}
    S->>D: POST /attach {id}
    D-->>S: {version, protocol}, stream held open
  end
  Note over D: entry.attached = 1, idle timer cleared
  C->>S: stdio closes
  S->>D: /attach aborted
  Note over D: attached = 0, idle timer starts
```

Notes:

- Tools are registered before the daemon is reached, so the MCP startup does not wait for the daemon; a tool call before the connection is ready awaits the same connect promise ([session.ts:47-52](../src/session.ts#L47-L52), [:62-68](../src/session.ts#L62-L68)).
- A session started with an invalid option does not connect; every tool call returns the option error ([session.ts:38-48](../src/session.ts#L38-L48)).
- The session process sends `/session` without a name. A new session takes the title the hook recorded for its ID, else the project folder name; a stored session keeps its name ([session.ts:46](../src/session.ts#L46), [daemon.ts:287-298](../src/daemon.ts#L287-L298)).
- After `/clear`, and after `--continue` or `--resume` without an ID, the hook gets a new session ID while the session process keeps its own; the daemon remembers the new ID's title in memory only, at most 100 IDs, so no profile is created for it.
- `claude --resume` starts a session process with the same session ID: the daemon reopens `_sessions/ID/` with the same number and chats.

Source: [session.ts:34-53](../src/session.ts#L34-L53), [:123-140](../src/session.ts#L123-L140), [daemon.ts:162-170](../src/daemon.ts#L162-L170), [:210-221](../src/daemon.ts#L210-L221), [:287-298](../src/daemon.ts#L287-L298).

---

## 3. `connect` with approval

```mermaid
sequenceDiagram
  participant C as Claude
  participant S as Session
  participant D as Daemon
  participant T as Tools
  participant H as Hub
  participant O as Owner
  participant P as Peer
  C->>S: tools/call connect {target: "@directory.simplex", wait, idle}
  S->>D: POST /call {id, tool: connect, input}
  D->>T: call(connect, input, {signal, progress})
  T->>H: find(key): none
  T->>T: /_connect plan USER @directory.simplex
  T->>T: classifyPlan: prepareContact with domain
  T->>H: create(S, key, connect fields)
  H->>H: refusal: none; /_set custom #G (counter)
  H->>O: Session S · NAME / I want to connect to bot X (@directory.simplex), as myself. / approve N or reject N
  T->>H: decided(action, call)
  H-->>S: {progress: "Action N waits for an approver until TIME."}
  S-->>C: notifications/progress
  O->>H: /approve N
  H->>D: perform(action)
  D->>T: perform(action) in session S
  T->>T: /_connect plan, /_prepare contact USER LINK domain=directory.simplex {link data}
  T->>T: /_connect contact @C incognito=off
  P->>T: accepts: contactConnected, collect @C
  H->>O: card edited: ... I connected to bot X (@directory.simplex), as myself. Approved by Alice.
  T->>T: /_get chat @C count=50, wait until idle, /_read chat items
  T-->>D: {action: N, status: done, chat: @C, connected: true, messages}
  D-->>S: {result}
  S-->>C: result
```

Notes:

- The plan is read again in the perform step ([tools.ts:486](../src/tools.ts#L486)).
- Join commands per plan ([tools.ts:515-543](../src/tools.ts#L515-L543)):

| Plan | Commands |
|---|---|
| known prepared contact or group | `/_connect contact @C incognito=X [json M]`<br>`/_connect group #G incognito=X [json M]` |
| channel link (`direct` false, with link data) | `/_prepare group USER LINK direct=off [domain=D] JSON`<br>`/_connect group #G incognito=X` |
| contact address with link data, and a SimpleX name or a message | `/_prepare contact USER LINK [domain=D] JSON`<br>`/_connect contact @C incognito=X [json M]`, or `/_connect group #G ...` for a business |
| `contactViaAddress` | `/_connect contact USER incognito=X CONTACT_ID` |
| other links | `/_connect USER incognito=X LINK` |

- After a failed prepared connect, the daemon deletes the prepared chat with `notify=off` ([tools.ts:545-552](../src/tools.ts#L545-L552)).
- The connection is matched by `contactConnected` or `userJoinedGroup` of the session's core: by contact or group ID, or by the pending connection ID ([tools.ts:389-397](../src/tools.ts#L389-L397), [:558-563](../src/tools.ts#L558-L563)).
- In the perform step the daemon waits for the other side until `startedAt + wait`, at most until `created + 23 h 50 min`, and then reports `connected: false` ([tools.ts:485](../src/tools.ts#L485), [:509-512](../src/tools.ts#L509-L512)).
- Without `message`, the idle time is counted from the connection; with `message`, from the first reply ([tools.ts:569](../src/tools.ts#L569), [messages.ts:227](../src/messages.ts#L227)).
- For a target the session is connected to, the result is `status: known` with its chat fields, and no action is created ([tools.ts:419](../src/tools.ts#L419)).
- Exact progress texts: `Action N waits for an approver until TIME.`, then `Action N waits for the connection until TIME.`, then `K messages in CHAT; waiting until TIME.` ([actions.ts:315-326](../src/actions.ts#L315-L326), [tools.ts:454](../src/tools.ts#L454)).

Source: [session.ts:94-113](../src/session.ts#L94-L113), [daemon.ts:223-240](../src/daemon.ts#L223-L240), [tools.ts:414-466](../src/tools.ts#L414-L466), [:482-572](../src/tools.ts#L482-L572), [:810-873](../src/tools.ts#L810-L873), [actions.ts:124-202](../src/actions.ts#L124-L202), [control.ts:138-198](../src/control.ts#L138-L198).

---

## 4. `send` with responses

```mermaid
sequenceDiagram
  participant C as Claude
  participant T as Tools
  participant H as Hub
  participant O as Owner
  participant P as Peer
  C->>T: send {chat: #15, text, reply_to, mention: [44], wait, idle, limit}
  T->>T: /_get chat #15 count=0, connected check
  T->>T: /_get chat #15 around=REPLY_TO count=0
  T->>T: /_members #15
  T->>H: standingApprover(chatInfo): none
  T->>H: create(S, key, send fields)
  H->>O: Session S · NAME / I want to send a message to group X: `text` replying to ... mentioning @Switchback / approve S:#15 N, approve S:#15 all or reject S:#15 N
  O->>H: /approve S:#15 N
  H->>T: performSend
  T->>T: collect #15 items with ID above the sent item
  T->>T: /_send #15 json [{msgContent, mentions, quotedItemId}]
  P->>T: replies S+1, S+2: collected
  H->>O: card edited: ... Approved by Alice.
  T->>T: wait until idle seconds after the last reply, or sentAt + wait
  T->>T: /_read chat items #15 S+1,S+2
  T-->>C: {action: N, status: done, chat: #15, item: S, responses, more: 0}
```

Notes:

- Checks before the action: chat reference, connected chat, role `observer` or `relay` refused, `reply_to` item present, mentioned members present and the chat a group ([tools.ts:574-583](../src/tools.ts#L574-L583)).
- The daemon starts collecting before the send and, after it, drops items with IDs up to the sent item ([tools.ts:614](../src/tools.ts#L614), [:624-626](../src/tools.ts#L624-L626)).
- Mentioned names missing from the text are prefixed as `@name` ([tools.ts:616-617](../src/tools.ts#L616-L617)).
- While a collected message is live, the daemon waits for its final update; live items are left out of `responses` and counted in `more` ([messages.ts:225-227](../src/messages.ts#L225-L227), [tools.ts:601](../src/tools.ts#L601), [:607](../src/tools.ts#L607)).
- At most 100 replies are kept per action; later ones are counted as overflow ([messages.ts:81-82](../src/messages.ts#L81-L82)).

Source: [tools.ts:574-631](../src/tools.ts#L574-L631), [messages.ts:71-86](../src/messages.ts#L71-L86), [:224-232](../src/messages.ts#L224-L232), [control.ts:412-417](../src/control.ts#L412-L417).

---

## 5. `receive` with `wait`

```mermaid
sequenceDiagram
  participant C as Claude
  participant T as Tools
  participant W as ChatItemsWatch
  participant P as Peer
  C->>T: receive {chat: #12, wait: 3600, limit: 50}
  T->>W: collect #12 into arrived
  T->>T: /_get chat #12 initial=1: unreadCount 0
  T-->>C: progress: No new messages in #12, waiting until TIME.
  P->>W: post, item 40: arrived
  T->>T: /_read chat items #12 40
  T->>W: stop collecting
  T-->>C: {chat: #12, messages: [40], more: 0}
```

Notes:

- With unread items the daemon reads `around=<minUnreadItemId> count=0`, then pages of `after=<last> count=100` until it holds `limit + 1` unread, non-live messages or a page is short ([messages.ts:186-196](../src/messages.ts#L186-L196)).
- Unread items other than messages in the read pages are marked read with the returned messages ([tools.ts:666](../src/tools.ts#L666), [:683](../src/tools.ts#L683)).
- `more`: the arrived messages not returned, the overflow and the unread items not read ([tools.ts:685](../src/tools.ts#L685)).
- Items received while the pages are read are returned once ([tools.ts:665-668](../src/tools.ts#L665-L668)).
- Exact progress text: `No new messages in CHAT; waiting until TIME.` ([tools.ts:672](../src/tools.ts#L672)).

Source: [tools.ts:655-689](../src/tools.ts#L655-L689), [messages.ts:186-232](../src/messages.ts#L186-L232).

---

## 6. `send` under a standing approval, and `/revoke`

```mermaid
sequenceDiagram
  participant C as Claude
  participant T as Tools
  participant H as Hub
  participant O as Owner
  O->>H: /approve 2:#9 all (on a pending send card of session 2)
  H->>T: session 2: /_get chat #9 count=0
  H->>T: session 2: /_set custom #9 {"simplexMcp":{"approvedBy":"Alice","approvedById":3,"approvedAt":"..."}}
  H->>H: approve each pending send of session 2 to #9 with a card
  H->>O: Approved for business X (2:#9) by Alice. /'revoke 2:#9'
  C->>T: send {chat: #9, text}
  T->>T: /_get chat #9 count=0
  T->>H: standingApprover: Alice
  T->>H: create(2, key, fields, "Alice"): performing, standing
  H->>H: saveCounter (no card)
  H->>T: performSend: /_send #9 json [...]
  H->>O: Session 2 · NAME / I sent a message to business X: `text` / Approved for this chat by Alice. /'revoke 2:#9'
  T-->>C: {action, status: done, chat: #9, item, responses, more}
  O->>H: /revoke 2:#9
  H->>T: session 2: /_get chat #9 count=0, /_set custom #9 {other keys}
  H->>O: Messages to business X (2:#9) need approval again.
```

Notes:

- The approval is stored in the session's database and used in mode `requests` while Main group member `approvedById` is an owner ([control.ts:117-120](../src/control.ts#L117-L120)).
- `/approve S:CHAT all` and `/revoke S:CHAT` need session S to be open; otherwise the reply is `No open session S. /list shows the open sessions.` ([control.ts:224-225](../src/control.ts#L224-L225), [:239-240](../src/control.ts#L239-L240)).
- For sends that were pending at `/approve S:CHAT all`, the card ends with the plain `Approved by Alice.` ([control.ts:232-234](../src/control.ts#L232-L234), [:383](../src/control.ts#L383)).
- `connect` and `leave` actions are created without the standing approver, so they stay pending ([tools.ts:425](../src/tools.ts#L425), [:640](../src/tools.ts#L640)).

Source: [control.ts:223-246](../src/control.ts#L223-L246), [tools.ts:588](../src/tools.ts#L588), [actions.ts:124-157](../src/actions.ts#L124-L157), [:244-262](../src/actions.ts#L244-L262).

---

## 7. `leave`

```mermaid
sequenceDiagram
  participant C as Claude
  participant T as Tools
  participant H as Hub
  participant O as Owner
  C->>T: leave {chat: #9}
  T->>H: find(key): none
  T->>T: /_get chat #9 count=0
  T->>H: create(S, key, leave fields)
  H->>O: Session S · NAME / I want to leave business X (#9). / approve S:#9 N or reject S:#9 N
  O->>H: /approve S:#9 N
  H->>T: performLeave
  T->>T: /_get chat #9 count=0
  T->>T: /_leave #9 (membership current or pending)
  T->>T: /_delete #9 full
  H->>O: card edited: ... I left business X (#9). Approved by Alice.
  T-->>C: {action: N, status: done, chat: #9}
```

Notes:

- For a contact, the card text is `I want to delete KIND NAME (@N).`, and the perform step sends `/_delete @N full` ([actions.ts:302-304](../src/actions.ts#L302-L304), [tools.ts:649](../src/tools.ts#L649)).
- The delete mode is `full` without `notify=off`, so the core notifies the contact or the members.
- The daemon looks for an attached action before it reads the chat, so a repeated call after the deletion is attached to the done action ([tools.ts:635-637](../src/tools.ts#L635-L637)).

Source: [tools.ts:633-653](../src/tools.ts#L633-L653), [control.ts:418-421](../src/control.ts#L418-L421).

---

## 8. Mode `host`

```mermaid
sequenceDiagram
  participant C as Claude
  participant S as Session
  participant T as Tools
  participant H as Hub
  participant O as Owner
  Note over C,S: the session started with approve = host
  C->>S: tools/list
  S-->>C: connect, send, leave with _meta {"anthropic/requiresUserInteraction": true}
  C->>C: Claude Code asks its user
  C->>S: send {chat: @4, text}
  S->>T: POST /call
  T->>H: create: performing
  H->>H: saveCounter (no card)
  H->>T: performSend
  H->>H: log: Action N: performed without approval (mode host).
  H->>O: Session S · NAME / I sent a message to bot X: `text` / Performed without approval (mode host).
  T-->>C: {action: N, status: done, ...}
```

Notes:

- `_meta` follows the option of the session's own start; the daemon's mode follows the latest `/session` or `/title` of any session ([session.ts:48](../src/session.ts#L48), [hub.ts:63-66](../src/hub.ts#L63-L66)).
- New actions are still refused unless the Main group has an owner ([control.ts:113-114](../src/control.ts#L113-L114)).
- In mode `host`, `undefined` is returned by `standingApprover`, so standing approvals are ignored by `send`, `list` and `/list` ([control.ts:119](../src/control.ts#L119)).
- `_meta` is set on `connect`, `send` and `leave` only ([tools.ts:360](../src/tools.ts#L360)).

Source: [tools.ts:354-364](../src/tools.ts#L354-L364), [actions.ts:136](../src/actions.ts#L136), [:260](../src/actions.ts#L260), [control.ts:383](../src/control.ts#L383).

---

## 9. Expiry

```mermaid
sequenceDiagram
  participant C as Claude
  participant H as Hub
  participant O as Owner
  C->>H: send call creates pending action N at T
  H->>O: card N
  Note over H: expires = T + expireHours at T
  loop every 60 s, and before each owner command
    H->>H: expireDue
  end
  H->>H: pending and due: expired
  H->>O: card edited: ... Expired.
  H-->>C: {action: N, status: expired}
  H->>H: removed with the result
  O->>H: /approve N
  H->>O: Action N is not pending.
```

Notes:

- A decided action with an open count of 0 is removed at the first tick after its expiry time ([actions.ts:179-181](../src/actions.ts#L179-L181)).
- Expiry times are listed in the `/pending` reply ([control.ts:437-439](../src/control.ts#L437-L439)).
- A changed `expire_hours` applies to actions created after the next `/session` or `/title` ([hub.ts:65](../src/hub.ts#L65), [actions.ts:133](../src/actions.ts#L133)).
- The connect wait ends at `created + 23 h 50 min`, within the 24 hours in which a message can be edited ([tools.ts:112](../src/tools.ts#L112), [:485](../src/tools.ts#L485)).

Source: [actions.ts:102](../src/actions.ts#L102), [:172-183](../src/actions.ts#L172-L183), [control.ts:169](../src/control.ts#L169), [:397-398](../src/control.ts#L397-L398).

---

## 10. A lost call and its re-attachment

```mermaid
sequenceDiagram
  participant C as Claude
  participant S as Session
  participant D as Daemon
  participant T as Tools
  participant O as Owner
  C->>S: send {chat: @4, text: "t"}
  S->>D: POST /call
  D->>T: create: pending, open 1
  C->>S: notifications/cancelled (or Esc)
  S->>D: /call request closed
  D->>T: signal aborted: decided false, open 0
  C->>S: list
  S->>D: POST /call list
  D-->>C: actions: [{action: N, tool: send, input, state: pending, open: false}]
  O->>T: /approve N: perform, done, replies collected
  C->>S: send {chat: @4, text: "t"}
  S->>D: POST /call
  T->>T: find(key): action N
  T->>T: doneAtCall: wait counted from now
  T-->>C: {action: N, status: done, item, responses, more}
```

Notes:

- The key is the session number, the tool name and the action fields: `target`, `incognito`, `message` for `connect`; `chat`, `text`, `reply_to`, `mention` for `send`; `chat` for `leave` ([tools.ts:415](../src/tools.ts#L415), [:584](../src/tools.ts#L584), [:635](../src/tools.ts#L635), [:987-989](../src/tools.ts#L987-L989)).
- The `wait`, `idle` and `limit` of the attaching call are used ([tools.ts:595](../src/tools.ts#L595)).
- For a call aborted before `create`, no action is created ([tools.ts:424](../src/tools.ts#L424), [:587](../src/tools.ts#L587), [:639](../src/tools.ts#L639)).
- Abort path: the SDK aborts the handler's signal; the session process passes it to its `/call` request; the daemon aborts the call's controller when the request closes before the reply ends, and writes no result ([session.ts:99-103](../src/session.ts#L99-L103), [daemon.ts:227-235](../src/daemon.ts#L227-L235)).

Source: [tools.ts:433-444](../src/tools.ts#L433-L444), [:592-610](../src/tools.ts#L592-L610), [:697](../src/tools.ts#L697), [actions.ts:185-196](../src/actions.ts#L185-L196), [messages.ts:208-222](../src/messages.ts#L208-L222).

---

## 11. Idle close and idle stop

```mermaid
sequenceDiagram
  participant S as Session
  participant D as Daemon
  participant T as Tools
  S->>D: /attach closes (Claude Code session ended)
  D->>D: attached 0: session idle timer, 10 min
  alt a call runs or the session has actions
    D->>D: timer starts again
  else
    D->>D: entry removed
    D->>T: close the session core
    D->>D: no entries: daemon idle timer, 10 min
  end
  D->>D: stop (section 12), exit 0
```

Notes:

- A new `/session`, `/attach` or `/call` for any ID clears the daemon idle timer; a new request for a closing ID waits for the close and reopens the profile ([daemon.ts:253-298](../src/daemon.ts#L253-L298)). `/title` leaves both idle timers alone.
- The daemon idle timer also runs from the daemon start until the first entry ([daemon.ts:90](../src/daemon.ts#L90)).

Source: [daemon.ts:210-221](../src/daemon.ts#L210-L221), [:300-326](../src/daemon.ts#L300-L326).

---

## 12. Daemon stop with pending actions

```mermaid
sequenceDiagram
  participant X as SIGTERM, /stop or idle timer
  participant D as Daemon
  participant H as Hub
  participant T as Tools
  participant S as Session
  participant O as Owner
  X->>D: stop()
  D->>D: timers cleared, new entries refused (503)
  D->>D: listener closed; /attach and /call streams destroyed
  D-->>S: streams end: calls return "The SimpleX daemon stopped; repeat the call."
  D->>H: Hub.stop
  H->>H: events ignored; Actions.stop: pending and performing interrupted
  H->>O: pending: ... Cancelled: the server stopped.
  H->>O: performing: ... Approved by Alice, interrupted: the server stopped.
  H->>H: /sql chat, /sql agent: pending deliveries to #G members, every 100 ms, at most 10 s
  H->>H: close the hub core
  D->>D: log: Cancelled 2 actions.
  D->>T: close each session core
  D->>D: remove ROOT/lock, log: Stopped.
  D->>D: exit 0 (after the /stop reply)
  Note over D,O: next start: counter continues, /approve of an old number: Action N is not pending.
```

Notes:

- The interrupted card line is `DECIDER; interrupted: the server stopped.` ([control.ts:390](../src/control.ts#L390)).
- A host-mode action whose counter is being written at the stop is set to `pending` and cancelled without a perform ([actions.ts:247](../src/actions.ts#L247)).
- The perform signal is aborted after the actions are marked interrupted: a `connect` waiting for its connection ends, and an interrupted perform logs no line ([actions.ts:225](../src/actions.ts#L225), [:259](../src/actions.ts#L259), [tools.ts:509-511](../src/tools.ts#L509-L511)).
- An error of a `/sql` query of the delivery wait is logged as `delivery wait: ERROR` and ends the wait ([core.ts:54-62](../src/core.ts#L54-L62)).

Source: [daemon.ts:98-142](../src/daemon.ts#L98-L142), [:348-355](../src/daemon.ts#L348-L355), [hub.ts:77-84](../src/hub.ts#L77-L84), [actions.ts:219-229](../src/actions.ts#L219-L229), [core.ts:44-52](../src/core.ts#L44-L52).

---

## 13. Daemon crash and a new daemon

```mermaid
sequenceDiagram
  participant C as Claude
  participant S as Session
  participant D1 as Daemon (crashed)
  participant D2 as New daemon
  C->>S: receive {chat, wait}
  S->>D1: POST /call
  D1--xS: connection reset
  S->>S: drop the connection
  S-->>C: The SimpleX daemon stopped; repeat the call.
  C->>S: receive {chat, wait}
  S->>D2: GET /status: ECONNREFUSED or ENOENT
  S->>D2: spawn; lock: PID not running, file replaced
  S->>D2: /session, /attach, /call
  D2-->>C: result
```

Notes:

- The crashed daemon's actions are lost; their cards keep their commands, and `/approve` of their numbers replies `Action N is not pending.` ([control.ts:188](../src/control.ts#L188)).
- A stale socket file is removed before the new daemon listens ([socket.ts:40](../src/socket.ts#L40)); a stale lock is replaced when its PID is not running ([files.ts:81-88](../src/files.ts#L81-L88)).
- Status 409 for a session the new daemon has not seen yet is retried once after a new connection ([session.ts:87-88](../src/session.ts#L87-L88)).

Source: [session.ts:81-121](../src/session.ts#L81-L121), [:143-146](../src/session.ts#L143-L146), [client.ts:32-44](../src/client.ts#L32-L44).

---

## 14. A newer version

```mermaid
sequenceDiagram
  participant N as Hook or session of version B
  participant A as Daemon of version A
  participant B as Daemon of version B
  N->>A: GET /status: {version: A, protocol: 1}
  alt A lower than B
    N->>A: POST /stop
    A->>A: stop sequence, exit 0
    N->>B: spawn: ready
  else A equal or higher, protocol 1
    N->>A: use it
  end
  Note over N: protocol other than 1: "Restart Claude Code to update the SimpleX plugin."
```

Notes:

- Versions are compared by their first three numeric parts ([client.ts:117-122](../src/client.ts#L117-L122)).
- Session processes of version A lose their streams when A stops; their next call connects to B, whose protocol must match.

Source: [client.ts:32-62](../src/client.ts#L32-L62), [daemon.ts:111-114](../src/daemon.ts#L111-L114).

---

## 15. `reset-owners`

```mermaid
sequenceDiagram
  participant C as Claude
  participant R as reset-owners
  participant H as Hub
  participant O as Owners
  C->>R: /simplex:reset-owners: launch.js reset-owners
  R->>H: ensureDaemon, POST /reset-owners
  H->>H: serialized with Main group events
  H->>H: loseControl: pending actions failed
  H->>H: /_leave #G when current and not owner
  H->>O: /_delete #G full notify (group deleted for all members)
  H->>H: new #G with the same counter, regainControl, load
  H->>H: /_create link #G2 member
  H-->>R: {link}
  R-->>C: "SimpleX MCP" was replaced. Join the new one in the SimpleX app. The link works once: LINK
```

Notes:

- Run in a terminal, the output has an `ansi` QR code before the link; through the skill it has none, because stdout is not a terminal ([owner.ts:13-30](../src/owner.ts#L13-L30)).
- Member IDs change in the new group, so standing approvals given in the old one stop applying ([control.ts:117-120](../src/control.ts#L117-L120)).
- Until someone joins, new actions are refused with the new link ([control.ts:113-114](../src/control.ts#L113-L114)).

Source: [owner.ts:21-26](../src/owner.ts#L21-L26), [hub.ts:73-75](../src/hub.ts#L73-L75), [control.ts:205-212](../src/control.ts#L205-L212), [:351-356](../src/control.ts#L351-L356).
