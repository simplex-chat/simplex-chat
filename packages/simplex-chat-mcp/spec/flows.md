# SimpleX MCP server: flows

> Sequence diagrams of the main flows, each with notes and source locations.
>
> Related specs: [README](README.md) | [Architecture](architecture.md) | [API](api.md)

Participants:

| Letter | Participant |
|---|---|
| P | the person at the terminal |
| A | the agent in Claude Code |
| H | the HTTP listener with the MCP SDK |
| T | `AgentTools` |
| X | `Actions` |
| K | `Control` |
| C | the SimpleX core behind `ChatApi` |
| O | the owner in the SimpleX app |
| D | a peer |
| L | `Statuses`, the status messages |
| I | the inference endpoint of `--inference` |

`#G` is the agent control chat, `USER` the agent's user ID, `DATA` the data folder. Calls to the core are shown as the `ChatApi` methods the server calls, with their arguments in short. Card and progress texts with a semicolon are shown with a comma in the diagrams; the exact texts are in [API](api.md).

---

## Table of contents

1. [First start and the owner joins](#1-first-start-and-the-owner-joins)
2. [`register`](#2-register)
3. [`connect` with approval](#3-connect-with-approval)
4. [`send` with responses](#4-send-with-responses)
5. [`receive` with `wait`](#5-receive-with-wait)
6. [`send` under a standing approval, and `/revoke`](#6-send-under-a-standing-approval-and-revoke)
7. [`leave`](#7-leave)
8. [Mode `host`](#8-mode-host)
9. [Expiry](#9-expiry)
10. [A lost call and its re-attachment](#10-a-lost-call-and-its-re-attachment)
11. [Shutdown with pending actions](#11-shutdown-with-pending-actions)
12. [`--owners` setup](#12---owners-setup)
13. [`download` with approval](#13-download-with-approval)
14. [`/forward`](#14-forward)
15. [`send` with a file](#15-send-with-a-file)
16. [`/leave` and `/delete CHAT files`](#16-leave-and-delete-chat-files)
17. [Status message while the agent works](#17-status-message-while-the-agent-works)

---

## 1. First start and the owner joins

```mermaid
sequenceDiagram
  participant P as Person
  participant S as serve.ts
  participant C as Core
  participant K as Control
  participant O as Owner
  P->>S: simplex-mcp serve my-agent --display-name "My Agent"
  S->>S: DATA mode 0700, lock, token, headers.json
  S->>C: ChatApi.init DATA/simplex
  S->>P: version line, data folder, approval mode
  S->>C: apiGetActiveUser (none)
  S->>C: apiCreateActiveUser {displayName: "My Agent", fullName: "", peerType: agent}
  S->>C: apiSetNetworkConfig smpProxyMode always, smpProxyFallback allowProtected
  S->>C: /_temp_folder DATA/tmp
  S->>S: DATA/files/sent deleted
  S->>C: startChat, events buffered
  S->>C: apiListGroups USER (no control marker)
  S->>C: apiNewGroup USER {My Agent, history on, menu pending, list and leave}
  S->>C: apiSetGroupCustomData #G {"simplexMcp":{"control":true,"nextAction":1}}
  S->>P: Agent control chat "My Agent" created.
  S->>K: load
  K->>C: apiListMembers #G
  S->>C: apiGetChats USER: leftover status messages, none at the first start
  S->>S: HTTP listens on 127.0.0.1:5226, /mcp answers 503 until the control chat is started
  S->>S: server.json
  K->>C: apiDeleteGroupLink #G (groupLinkNotFound ignored)
  K->>C: apiCreateGroupLink #G member
  K->>P: join line, QR code, link, QR hint, owner line
  S->>P: Listening on http://127.0.0.1:5226/mcp
  O->>C: joins by the link
  C->>S: joinedGroupMember
  S->>K: onEvent, queued
  K->>C: apiListMembers #G
  K->>C: apiDeleteGroupLink #G
  K->>C: apiSetMembersRole #G [3] owner
  K->>C: apiListMembers #G
  K->>O: I'm your agent. I ask here before I connect, send, download a file or leave a chat.
  K->>P: Alice joined (member 3) and is now an owner. The link is deleted.
  K->>P: Add the agent to Claude Code: npx -y @simplex-chat/mcp register my-agent
```

Notes:

- At first use, libsimplex and the Node.js add-on are downloaded, with one `Downloading ...` line each on stderr ([libs.ts:114-135](../../simplex-chat-nodejs/src/libs.ts#L114-L135)).
- With `--data`, `--data <folder>` is appended to the register hint, shell-quoted ([serve.ts:191-193](../src/serve.ts#L191-L193)).
- With `--qr-code small`, the line `If the QR code does not scan, ...` is printed after the link ([control.ts:412](../src/control.ts#L412)).
- Events that arrive before the HTTP listener starts are buffered and handled in order ([serve.ts:248-262](../src/serve.ts#L248-L262)).
- The server runs `ownerJoined` for every `joinedGroupMember` event of a human without `--owners`, and the link is deleted in that handler ([control.ts:189-190](../src/control.ts#L189-L190), [:371-378](../src/control.ts#L371-L378)).

Source: [cli.ts:214-225](../src/cli.ts#L214-L225), [serve.ts:67-131](../src/serve.ts#L67-L131), [:219-237](../src/serve.ts#L219-L237), [files.ts:12-48](../src/files.ts#L12-L48), [http.ts:14-46](../src/http.ts#L14-L46), [control.ts:102-117](../src/control.ts#L102-L117), [:341-345](../src/control.ts#L341-L345), [:406-425](../src/control.ts#L406-L425), [:499-520](../src/control.ts#L499-L520), [tools.ts:1160-1165](../src/tools.ts#L1160-L1165), [:1215-1223](../src/tools.ts#L1215-L1223).

---

## 2. `register`

```mermaid
sequenceDiagram
  participant P as Person
  participant R as register.ts
  participant L as claude CLI
  participant CC as Claude Code
  participant H as HTTP
  P->>R: simplex-mcp register my-agent [--scope local] [--data DIR]
  R->>R: read DATA/server.json
  alt server.json missing
    R->>P: Agent "my-agent" was never started. Start it with: ... (exit 1)
  end
  R->>L: claude mcp remove --scope user simplex-my-agent
  R->>L: claude mcp add-json --scope user simplex-my-agent {"type":"http","url":"http://127.0.0.1:5226/mcp","headersHelper":"cat DATA/headers.json","timeout":169800000}
  alt claude not found
    R->>P: Add this entry under "mcpServers" in ~/.claude.json: {...} (exit 1)
  else add-json exit status other than 0
    R->>P: claude mcp add-json failed: STDERR (exit 1)
  end
  R->>P: Added to Claude Code: scope user, server simplex-my-agent.
  CC->>CC: headersHelper: cat DATA/headers.json
  CC->>H: POST /mcp with Authorization: Bearer TOKEN
```

Notes:

- `simplex-mcp` ignores the exit status of `claude mcp remove` and stops at that step only when the `claude` binary is missing ([register.ts:38-39](../src/register.ts#L38-L39)).
- On Windows, a missing `claude` is run again through `cmd.exe`, with each argument quoted for it ([register.ts:86-101](../src/register.ts#L86-L101)).
- `timeout` is `(expireHours + 24) × 3600000 + 600000` ms from `server.json` ([register.ts:51](../src/register.ts#L51)).
- With `--scope local`, the project folder in the manual entry is the git repository root, the main repository of a worktree, or the current folder outside a repository ([register.ts:56](../src/register.ts#L56), [:60-84](../src/register.ts#L60-L84)).

Source: [cli.ts:120-135](../src/cli.ts#L120-L135), [:223](../src/cli.ts#L223), [register.ts:33-58](../src/register.ts#L33-L58), [files.ts:50-53](../src/files.ts#L50-L53).

---

## 3. `connect` with approval

```mermaid
sequenceDiagram
  participant A as Agent
  participant H as HTTP and SDK
  participant T as AgentTools
  participant X as Actions
  participant K as Control
  participant C as Core
  participant O as Owner
  participant D as Peer
  A->>H: tools/call connect {target: "@directory.simplex", wait, idle}
  H->>T: validated input, abort signal, progress token
  T->>X: find(key): none
  T->>C: apiConnectPlan USER @directory.simplex
  C-->>T: contactAddress ok, planSimplexName
  T->>T: classifyPlan: prepareContact with domain
  T->>X: create(key, connect fields)
  X->>K: refusal: none
  X->>K: saveCounter: apiSetGroupCustomData #G
  X->>K: postCard
  K->>O: I want to connect to bot X (@directory.simplex), as myself. /'approve N' or /'reject N'
  T->>X: decided(action, call)
  X-->>A: progress: Action N waits for an approver until TIME.
  O->>C: /approve N
  C->>K: newChatItems
  K->>X: approve(action, "Alice")
  X->>T: performConnect
  T->>C: apiConnectPlan USER @directory.simplex
  T->>C: apiPrepareContact USER LINK linkData directory.simplex
  T->>C: apiConnectPreparedContact @C incognito false
  C-->>T: startedConnectionToContact
  D->>C: accepts the request
  C->>T: contactConnected: markConnected, collect @C
  T-->>X: perform ends
  X->>K: outcome
  K->>O: card edited: I connected to bot X (@directory.simplex), as myself. Approved by Alice.
  T->>C: apiGetChat @C count 50
  T->>T: wait until idle or startedAt + wait
  T->>C: apiChatItemsRead @C, new item IDs
  T->>X: remove(action)
  T-->>A: {action: N, status: done, chat: @C, connected: true, messages}
```

Notes:

- The plan is read again in the perform step ([tools.ts:707](../src/tools.ts#L707)).
- Join methods per plan ([tools.ts:736-764](../src/tools.ts#L736-L764), [:1315-1378](../src/tools.ts#L1315-L1378)):

| Plan | Methods |
|---|---|
| known prepared contact or group | `apiConnectPreparedContact(C, incognito, message)`<br>`apiConnectPreparedGroup(G, incognito, message)` |
| channel link (`direct` false, with link data) | `apiPrepareGroup(USER, LINK, false, linkData, domain)`<br>`apiConnectPreparedGroup(G, incognito)` |
| contact address with link data, and a SimpleX name or a message | `apiPrepareContact(USER, LINK, linkData, domain)`<br>`apiConnectPreparedContact(C, incognito, message)`, or `apiConnectPreparedGroup` for a business |
| `contactViaAddress` | `apiConnectContactViaAddress(USER, incognito, CONTACT_ID)` |
| other links | `apiConnectPreparedLink(USER, incognito, LINK)` |

- After a failed prepared connect, the server deletes the prepared chat without notifying ([tools.ts:766-773](../src/tools.ts#L766-L773)).
- The connection is matched by `contactConnected` or `userJoinedGroup`: by contact or group ID, or by the pending connection ID ([tools.ts:607-615](../src/tools.ts#L607-L615), [:775-780](../src/tools.ts#L775-L780)).
- In the perform step the server waits for the other side until `startedAt + wait`, at most until `created + 23 h 50 min`, and then reports `connected: false` ([tools.ts:148](../src/tools.ts#L148), [:706](../src/tools.ts#L706), [:730-733](../src/tools.ts#L730-L733)).
- Without `message`, the idle time is counted from the connection; with `message`, from the first reply ([tools.ts:786](../src/tools.ts#L786), [messages.ts:323-331](../src/messages.ts#L323-L331)).
- For a target the agent is connected to, the result is `status: known` with its chat fields, and no action is created ([tools.ts:639](../src/tools.ts#L639)).
- Exact progress texts: `Action N waits for an approver until TIME.`, then `Action N waits for the connection until TIME.`, then `K messages in CHAT; waiting until TIME.` ([actions.ts:343-354](../src/actions.ts#L343-L354), [tools.ts:675](../src/tools.ts#L675)).

Source: [tools.ts:634-689](../src/tools.ts#L634-L689), [:703-789](../src/tools.ts#L703-L789), [actions.ts:155-186](../src/actions.ts#L155-L186), [:206-223](../src/actions.ts#L206-L223), [control.ts:141-152](../src/control.ts#L141-L152).

---

## 4. `send` with responses

```mermaid
sequenceDiagram
  participant A as Agent
  participant T as AgentTools
  participant X as Actions
  participant K as Control
  participant C as Core
  participant O as Owner
  participant D as Peer
  A->>T: send {chat: #15, text, reply_to, mention: [44], wait, idle, limit}
  T->>C: apiGetChat #15 count 0
  T->>C: apiGetChat #15 around REPLY_TO count 0
  T->>C: apiListMembers #15
  T->>C: apiGetChat #15 count 0
  T->>K: standingApprover(chatInfo, messages): none
  T->>X: create(key, send fields)
  X->>K: card size check and approvers
  K->>O: I want to send a message to group X: `text` replying to ... mentioning @Switchback /'approve #15 N', /'approve #15 all' or /'reject #15 N'
  O->>C: /approve #15 N
  K->>X: approve(action, "Alice")
  X->>T: performSend
  T->>T: collect #15 items with ID above the sent item
  T->>C: apiSendMessages #15 [{msgContent, mentions, quotedItemId}]
  C-->>T: sent item S
  D->>C: replies S+1, S+2
  C->>T: newChatItems: collected
  X->>K: outcome
  K->>O: card edited: I sent a message to group X: `text` ... Approved by Alice.
  T->>T: wait until idle seconds after the last reply, or sentAt + wait
  T->>C: apiChatItemsRead #15 [S+1, S+2]
  T->>X: remove(action)
  T-->>A: {action: N, status: done, chat: #15, item: S, responses, more: 0}
```

Notes:

- Checks before the action: `text` or `file`, the chat connected, role `observer` or `relay` refused, `reply_to` item a message, mentioned members present and the chat a group, and the message length with its quote ([tools.ts:796-809](../src/tools.ts#L796-L809)).
- The server starts collecting before the send and, after it, drops items with IDs up to the sent item ([tools.ts:864](../src/tools.ts#L864), [:878-880](../src/tools.ts#L878-L880)).
- Mentioned names missing from the text are prefixed as `@name` ([tools.ts:1488-1491](../src/tools.ts#L1488-L1491)).
- While a collected message is live, the server waits for its final update; live items are left out of `responses` and counted in `more` ([messages.ts:323-331](../src/messages.ts#L323-L331), [tools.ts:850](../src/tools.ts#L850), [:856](../src/tools.ts#L856)).
- At most 100 replies are kept per action; later ones are counted as overflow ([messages.ts:10](../src/messages.ts#L10), [:109-110](../src/messages.ts#L109-L110)).
- With `live: true`, the call is never attached to an action, and after the role check its text is shown in the status message of the chat ([tools.ts:794](../src/tools.ts#L794), [:803](../src/tools.ts#L803), [17](#17-status-message-while-the-agent-works)).

Source: [tools.ts:791-886](../src/tools.ts#L791-L886), [messages.ts:99-114](../src/messages.ts#L99-L114), [:323-331](../src/messages.ts#L323-L331), [control.ts:583-589](../src/control.ts#L583-L589).

---

## 5. `receive` with `wait`

```mermaid
sequenceDiagram
  participant A as Agent
  participant T as AgentTools
  participant W as ChatItemsWatch
  participant C as Core
  participant D as Peer
  A->>T: receive {chat: #12, wait: 3600, limit: 50}
  T->>W: collect #12 into arrived
  T->>C: apiGetChat #12 initial 1
  C-->>T: chatStats.unreadCount 0
  T-->>A: progress: No new messages in #12, waiting until TIME.
  D->>C: post, item 40
  C->>W: newChatItems: arrived
  T->>C: apiChatItemsRead #12 [40]
  T->>W: stop collecting
  T-->>A: {chat: #12, messages: [40], more: 0}
```

Notes:

- With unread items the server reads `around` the first unread item, `count` 0, then pages of `after` the last item, `count` 100, until it holds `limit + 1` unread, non-live messages or a page is short ([messages.ts:285-299](../src/messages.ts#L285-L299)).
- Unread items other than messages in the read pages are marked read with the returned messages ([tools.ts:1178](../src/tools.ts#L1178), [:1196](../src/tools.ts#L1196)).
- `more`: the arrived messages not returned, the overflow and the unread items not read ([tools.ts:1199](../src/tools.ts#L1199)).
- Items received while the pages are read are returned once ([tools.ts:1177-1180](../src/tools.ts#L1177-L1180)).
- Exact progress text: `No new messages in CHAT; waiting until TIME.` ([tools.ts:1185](../src/tools.ts#L1185)).
- Before the wait, the status message of the chat is deleted; with `typing: true`, a status message is started when messages are returned ([tools.ts:1182-1183](../src/tools.ts#L1182-L1183), [:1198](../src/tools.ts#L1198), [17](#17-status-message-while-the-agent-works)).

Source: [tools.ts:1167-1203](../src/tools.ts#L1167-L1203), [messages.ts:285-331](../src/messages.ts#L285-L331).

---

## 6. `send` under a standing approval, and `/revoke`

```mermaid
sequenceDiagram
  participant A as Agent
  participant T as AgentTools
  participant X as Actions
  participant K as Control
  participant C as Core
  participant O as Owner
  O->>C: /approve #9 all (on a pending send card)
  C->>K: newChatItems
  K->>C: apiGetChat #9 count 0
  K->>C: apiSetGroupCustomData #9 {"simplexMcp":{"approvedBy":"Alice","approvedById":3,"approvedAt":"..."}}
  K->>X: approve each pending send to #9 with a card
  K->>O: Approved for business X (#9) by Alice. /'revoke #9'
  A->>T: send {chat: #9, text}
  T->>C: apiGetChat #9 count 0
  T->>K: standingApprover: Alice
  T->>X: create(key, fields, "Alice"): performing, standing
  X->>K: saveCounter (no card)
  X->>T: performSend
  T->>C: apiSendMessages #9 [...]
  X->>K: outcome
  K->>O: I sent a message to business X: `text` Approved for this chat by Alice. /'revoke #9'
  T-->>A: {action, status: done, chat: #9, item, responses, more}
  O->>C: /revoke #9
  K->>C: apiGetChat #9 count 0
  K->>C: apiSetGroupCustomData #9 {other keys}
  K->>O: Messages to business X (#9) need approval again.
```

Notes:

- The approval is used in mode `requests` while member `approvedById` is an approver ([control.ts:130-133](../src/control.ts#L130-L133)).
- For sends that were pending at `/approve CHAT all`, the card ends with the plain `Approved by Alice.` ([control.ts:554](../src/control.ts#L554), [:563](../src/control.ts#L563)).
- `connect` and `leave` actions are created without the standing approver, so they stay pending ([tools.ts:645](../src/tools.ts#L645), [:982](../src/tools.ts#L982)).
- `/receive CHAT N all` and `/receive CHAT off` work the same way for downloads, with the key `simplexMcpFiles`, except that download N alone is performed, and the other pending downloads of the chat stay pending ([control.ts:22-25](../src/control.ts#L22-L25), [:251-253](../src/control.ts#L251-L253), [tools.ts:1021](../src/tools.ts#L1021)).
- After `/revoke #9`, the status message of #9 is deleted at its next tick, and a `send` with `live: true` to #9 is refused ([status.ts:116-123](../src/status.ts#L116-L123), [tools.ts:833](../src/tools.ts#L833), [17](#17-status-message-while-the-agent-works)).

Source: [control.ts:243-263](../src/control.ts#L243-L263), [:337-339](../src/control.ts#L337-L339), [tools.ts:813-815](../src/tools.ts#L813-L815), [actions.ts:155-186](../src/actions.ts#L155-L186), [:265-283](../src/actions.ts#L265-L283).

---

## 7. `leave`

```mermaid
sequenceDiagram
  participant A as Agent
  participant T as AgentTools
  participant X as Actions
  participant K as Control
  participant C as Core
  participant O as Owner
  A->>T: leave {chat: #9}
  T->>X: find(key): none
  T->>C: apiGetChat #9 count 0
  T->>X: create(key, leave fields)
  K->>O: I want to leave business X (#9). /'approve #9 N' or /'reject #9 N'
  O->>C: /approve #9 N
  K->>X: approve(action, "Alice")
  X->>T: leaveChat(#9)
  T->>C: apiDeleteChatItems #9 [status item] broadcast (when #9 has a status message)
  T->>C: apiGetChat #9 count 0
  T->>C: apiLeaveGroup #9 (membership current or pending)
  T->>C: apiDeleteChat #9 full, notify
  T->>T: DATA/forwarded/group-9 deleted
  X->>K: outcome
  K->>O: card edited: I left business X (#9). Approved by Alice.
  K->>O: Files from business X (#9) are kept in `DATA/files/group-9`: K files, SIZE. /'delete #9 files' (when the folder has files)
  T-->>A: {action: N, status: done, chat: #9}
```

Notes:

- For a contact, the card text is `I want to delete KIND NAME (@N).`, and the perform step calls `apiDeleteChat` for the contact, full, with notify ([control.ts:590-593](../src/control.ts#L590-L593), [tools.ts:994](../src/tools.ts#L994)).
- The delete notifies, so the core notifies the contact or the members ([tools.ts:994](../src/tools.ts#L994), [:998](../src/tools.ts#L998)).
- The server looks for an attached action before it reads the chat, so a repeated call after the deletion is attached to the done action ([tools.ts:977-980](../src/tools.ts#L977-L980)).
- From before the status deletion until the chat is deleted, #9 is marked as being left: a `send` with `live: true` to #9 is refused with `No chat #9. list shows the agent's chats.` until the membership is marked left in `apiLeaveGroup`, and after that with `The agent is not connected to #9 and can't post; list shows connected: false.`; a status started meanwhile ends at its first tick without a post ([tools.ts:990-1002](../src/tools.ts#L990-L1002), [:801-802](../src/tools.ts#L801-L802), [:829](../src/tools.ts#L829), [:1446](../src/tools.ts#L1446)).

Source: [tools.ts:975-1004](../src/tools.ts#L975-L1004), [control.ts:145-152](../src/control.ts#L145-L152), [:299-302](../src/control.ts#L299-L302).

---

## 8. Mode `host`

```mermaid
sequenceDiagram
  participant P as Person
  participant A as Agent
  participant H as HTTP and SDK
  participant T as AgentTools
  participant X as Actions
  participant K as Control
  participant O as Owner
  Note over P,H: the person started serve my-agent --approve host
  A->>H: tools/list
  H-->>A: connect, send, download, leave with _meta {"anthropic/requiresUserInteraction": true}
  A->>A: Claude Code asks its user
  A->>T: send {chat: @4, text}
  T->>X: create: performing
  X->>K: saveCounter (no card)
  X->>T: performSend
  X->>P: Action N: performed without approval (mode host).
  X->>K: outcome
  K->>O: I sent a message to bot X: `text` Performed without approval (mode host).
  T-->>A: {action: N, status: done, ...}
```

Notes:

- New actions are still refused unless the control chat has an approver ([control.ts:124-127](../src/control.ts#L124-L127)).
- In mode `host`, `standingApprover` returns `undefined`, so standing approvals are ignored by `send`, `download`, `list` and `/list`, and `/approve CHAT all` stores nothing; downloads get no card, so `/receive CHAT N all` has no card to act on ([control.ts:132](../src/control.ts#L132), [:213](../src/control.ts#L213)).
- A download in mode `host` is received without the approval of unknown servers, and it gets no card and no outcome post ([tools.ts:562](../src/tools.ts#L562), [:1076](../src/tools.ts#L1076), [control.ts:495-497](../src/control.ts#L495-L497)).
- The kept-files message also follows a done `leave` in mode `host` ([control.ts:150-151](../src/control.ts#L150-L151)).

Source: [tools.ts:502-512](../src/tools.ts#L502-L512), [actions.ts:166](../src/actions.ts#L166), [:281](../src/actions.ts#L281), [control.ts:145-152](../src/control.ts#L145-L152), [:554](../src/control.ts#L554).

---

## 9. Expiry

```mermaid
sequenceDiagram
  participant A as Agent
  participant X as Actions
  participant K as Control
  participant O as Owner
  A->>X: send call creates pending action N at T
  K->>O: card N
  Note over X: expires = T + expireHours
  loop every 60 s, and before each owner command
    X->>X: expireDue
  end
  X->>X: pending and due: expired
  X->>K: outcome
  K->>O: card edited: ... Expired.
  X-->>A: {action: N, status: expired}
  X->>X: removed with the result
  O->>K: /approve N
  K->>O: Action N is not pending.
```

Notes:

- A decided action with an open count of 0 is removed at the first tick after its expiry time ([actions.ts:200-201](../src/actions.ts#L200-L201)).
- Expiry times are listed in the `/pending` reply ([control.ts:618-620](../src/control.ts#L618-L620)).
- The server ends the connect wait at `created + 23 h 50 min`, within the 24 hours in which a message can be edited ([tools.ts:148](../src/tools.ts#L148), [:706](../src/tools.ts#L706)).

Source: [actions.ts:129](../src/actions.ts#L129), [:163](../src/actions.ts#L163), [:193-204](../src/actions.ts#L193-L204), [control.ts:201](../src/control.ts#L201), [:227](../src/control.ts#L227).

---

## 10. A lost call and its re-attachment

```mermaid
sequenceDiagram
  participant A as Agent
  participant H as HTTP and SDK
  participant T as AgentTools
  participant X as Actions
  participant O as Owner
  A->>H: send {chat: @4, text: "t"}
  H->>T: call
  T->>X: create: pending, open 1
  A->>H: closes the response stream
  H->>T: ctx.mcpReq.signal aborted
  T->>X: decided: false, open 0
  Note over H: result discarded by the SDK
  A->>T: list
  T-->>A: actions: [{action: N, tool: send, input, state: pending, open: false}]
  O->>X: /approve N
  X->>X: perform, done, replies collected
  A->>T: send {chat: @4, text: "t"}
  T->>X: find(key): action N
  T->>T: doneAtCall: wait counted from now
  T->>X: remove(action)
  T-->>A: {action: N, status: done, item, responses, more}
```

Notes:

- The key is the tool name with the action fields: `target`, `incognito`, `message` for `connect`; `chat`, `text`, `file`, `as`, `reply_to`, `mention` for `send`; `chat`, `item` for `download`; `chat` for `leave` ([tools.ts:635](../src/tools.ts#L635), [:793](../src/tools.ts#L793), [:977](../src/tools.ts#L977), [:1008](../src/tools.ts#L1008), [:1618-1620](../src/tools.ts#L1618-L1620)).
- The `wait`, `idle` and `limit` of the attaching call are used ([tools.ts:844](../src/tools.ts#L844)).
- For a call aborted before `create`, the server returns before it creates an action ([tools.ts:644](../src/tools.ts#L644), [:814](../src/tools.ts#L814), [:981](../src/tools.ts#L981), [:1022](../src/tools.ts#L1022)).
- Abort path in the SDK: on `res` close the web request is aborted ([node index.cjs:299-303](../node_modules/@modelcontextprotocol/node/dist/index.cjs#L299-L303)); the per-request transport is closed ([server index.cjs:86-90](../node_modules/@modelcontextprotocol/server/dist/index.cjs#L86-L90)); the handler signal is aborted ([src-BhnGYybN.cjs:6425](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs#L6425)).

Source: [tools.ts:653-665](../src/tools.ts#L653-L665), [:841-859](../src/tools.ts#L841-L859), [:1211](../src/tools.ts#L1211), [actions.ts:206-217](../src/actions.ts#L206-L217), [messages.ts:307-321](../src/messages.ts#L307-L321).

---

## 11. Shutdown with pending actions

```mermaid
sequenceDiagram
  participant P as Person
  participant S as serve.ts
  participant X as Actions
  participant K as Control
  participant H as HTTP
  participant C as Core
  participant O as Owner
  P->>S: Ctrl-C (SIGINT)
  S->>S: stopped: events ignored
  par actions
    S->>X: stop()
    X->>X: stopping, timer cleared, cards being posted awaited
    X->>X: pending and performing: interrupted, performs aborted
    X->>K: outcome of each
    K->>O: pending: ... Cancelled: the server stopped.
    K->>O: performing: ... Approved by Alice, interrupted: the server stopped.
  and HTTP
    S->>H: close, closeAllConnections (open calls aborted)
  end
  S->>P: Cancelled 2 actions.
  S->>C: apiDeleteChatItems of each status message, broadcast, awaited
  S->>P: Stopping: sending 2 cancelled cards and 1 status deletion, up to 10 s. Ctrl-C to stop now.
  S->>S: wait 10 s, or until a second Ctrl-C
  S->>C: close
  S->>S: remove DATA/lock
  S->>P: Stopped.
  Note over P,O: next start: counter continues, /approve of an old number: Action N is not pending.
```

Notes:

- The interrupted card line is `DECIDER; interrupted: the server stopped.` ([control.ts:561](../src/control.ts#L561)).
- An action of mode `host` or of a standing approval whose counter is being written at the stop is set to `pending` and cancelled without a perform ([actions.ts:265-270](../src/actions.ts#L265-L270)).
- After a crash, the cards of that run are left with their commands; `/approve` of their numbers gets the reply `Action N is not pending.` ([control.ts:227](../src/control.ts#L227)).
- The stop signal of the performs is aborted after the actions are marked interrupted: a `connect` waiting for its connection ends, and an interrupted perform logs no line ([actions.ts:245-246](../src/actions.ts#L245-L246), [:280](../src/actions.ts#L280), [tools.ts:730-731](../src/tools.ts#L730-L731)).
- The status messages are ended after the HTTP server is closed and `Actions.stop` returns; later starts are ignored, and notes are refused ([serve.ts:179-182](../src/serve.ts#L179-L182), [status.ts:76-82](../src/status.ts#L76-L82)).
- The wait applies only when cards were edited or posted at the stop, a `download` without a card excluded, or status messages had a render, and `--stop-wait` is above 0. Cards are counted only while the control chat is not lost ([serve.ts:182-183](../src/serve.ts#L182-L183), [:195-211](../src/serve.ts#L195-L211)).

Source: [serve.ts:49-65](../src/serve.ts#L49-L65), [:176-187](../src/serve.ts#L176-L187), [:195-211](../src/serve.ts#L195-L211), [status.ts:76-82](../src/status.ts#L76-L82), [actions.ts:240-250](../src/actions.ts#L240-L250), [http.ts:48-52](../src/http.ts#L48-L52), [control.ts:552-571](../src/control.ts#L552-L571).

---

## 12. `--owners` setup

```mermaid
sequenceDiagram
  participant P as Person
  participant K as Control
  participant C as Core
  participant O as Owner Alice
  participant B as Member Bob
  P->>K: serve my-agent --owners
  K->>C: apiCreateGroupLink #G observer
  K->>P: The link adds an observer. Its ID and name are printed when it joins.
  O->>C: joins by the link
  C->>K: joinedGroupMember
  K->>C: apiDeleteGroupLink #G
  K->>P: Alice joined (member 3). Restart the server with --owners 3:Alice
  K->>O: Alice joined (member 3). Restart the server with `--owners 3:Alice`
  P->>K: Ctrl-C, then serve my-agent --owners 3:Alice
  K->>C: apiSetMembersRole #G [3] owner
  K->>C: apiRemoveMembers #G [ID], for each other human
  K->>C: apiDeleteGroupLink #G
  K->>P: Owners: 3:Alice.
  K->>P: Add the agent to Claude Code: ...
  O->>C: adds Bob, makes Bob an owner
  C->>K: memberRole
  K->>P: Bob (member 7) is now an owner and can't approve actions yet. Restart ... --owners 3:Alice,7:Bob
  K->>O: same line, the --owners part as code
  B->>K: /approve N
  K->>B: Only the owners listed in --owners approve actions.
```

Notes:

- Until the restart with a list, new actions are refused: `The agent has no approving owner. ...` ([control.ts:125-127](../src/control.ts#L125-L127)).
- An entry must match a human by member ID and display name, folded to one line as the restart line prints it, or the server exits with code 2 ([control.ts:354-358](../src/control.ts#L354-L358), [:758-760](../src/control.ts#L758-L760)).
- With `--reset-owners`, the server deletes the control chat for all members and creates a new one with the same counter; the CLI refuses the option with an `--owners` list ([control.ts:522-527](../src/control.ts#L522-L527), [serve.ts:236](../src/serve.ts#L236), [cli.ts:111-113](../src/cli.ts#L111-L113)).
- When a listed owner leaves and none remains: `... is no longer an owner. No owners remain: restart the server with --reset-owners --owners` ([control.ts:393-398](../src/control.ts#L393-L398)).

Source: [control.ts:347-369](../src/control.ts#L347-L369), [:380-399](../src/control.ts#L380-L399), [:406-416](../src/control.ts#L406-L416), [:758-784](../src/control.ts#L758-L784).

---

## 13. `download` with approval

```mermaid
sequenceDiagram
  participant A as Agent
  participant T as AgentTools
  participant X as Actions
  participant K as Control
  participant C as Core
  participant O as Owner
  A->>T: download {chat: #15, item: 30}
  T->>X: find(key): none
  T->>C: apiGetChat #15 count 0
  T->>C: apiGetChat #15 around 30 count 0
  T->>C: apiGetChatItemInfo #15 30: fileXftpServers
  opt no servers yet
    T-->>A: progress: The sender is uploading the file of item 30 in #15, waiting until TIME.
    C->>T: rcvFileDescrReady
    T->>C: apiGetChatItemInfo #15 30
  end
  T->>C: apiGetUserServers USER: no unknown servers
  T->>C: apiGetChat #15 count 0
  T->>K: standingApprover(chatInfo, files): none
  T->>X: create(key, download fields)
  K->>O: I want to download a file from Switchback in group Trail (#15): `report.pdf`, 2.4 MB, Servers: known. /'receive #15 N', /'receive #15 N all', /'forward #15 N' or /'reject #15 N'
  O->>C: /receive #15 N
  K->>X: approve(action, "Alice")
  X->>T: performDownload: downloadFile
  T->>C: apiGetChat #15 around 30 count 0
  T->>C: apiReceiveFile 7 DATA/files/group-15/7.pdf
  C->>T: rcvFileComplete
  T->>T: a file at the download path: resolved
  X->>K: outcome
  K->>O: card edited: I downloaded a file from ... Approved by Alice.
  T-->>A: {action: N, status: done, chat: #15, item: 30, path}
```

Notes:

- A check every 60 s reads the item: a deleted message or chat fails the download; 30 minutes without a rise of `rcvProgress` fail and cancel it ([tools.ts:1081-1108](../src/tools.ts#L1081-L1108)).
- A download still running `wait` seconds after the perform starts returns `status: performing`; the same call again attaches to the action ([tools.ts:1054-1057](../src/tools.ts#L1054-L1057)).
- `/approve #15 N` on a download card gets the reply `Action N is a download: ...` with the card's command line ([control.ts:234](../src/control.ts#L234)).
- A file on unknown servers gets a card that names their hosts and leaves out `/'receive CHAT N all'` ([control.ts:594-599](../src/control.ts#L594-L599), [:611-614](../src/control.ts#L611-L614), [:664-668](../src/control.ts#L664-L668)). Only such a card approves the relays; the core checks the complete description of every other download, which fails at the acceptance, or at the next check when the description completes later ([tools.ts:562](../src/tools.ts#L562), [:1089](../src/tools.ts#L1089)).
- The card of an image or video shows the sender's preview ([control.ts:441-461](../src/control.ts#L441-L461)).
- A file the core receives without a request, a short voice message, also in a member chat of a group, is moved into the chat's folder at `rcvFileComplete`; one whose transfer started before a restart, only when it has its full size. Its message then shows it as `offered`, and `download` returns its path after the approval ([tools.ts:568-588](../src/tools.ts#L568-L588), [:1128-1137](../src/tools.ts#L1128-L1137), [messages.ts:194-208](../src/messages.ts#L194-L208)).

Source: [tools.ts:1006-1149](../src/tools.ts#L1006-L1149), [control.ts:573-616](../src/control.ts#L573-L616), [serve.ts:144-160](../src/serve.ts#L144-L160).

---

## 14. `/forward`

```mermaid
sequenceDiagram
  participant A as Agent
  participant T as AgentTools
  participant K as Control
  participant C as Core
  participant O as Owner
  O->>C: /forward #15 N (on a pending download card)
  C->>K: newChatItems
  K->>T: forwardFile(action N)
  T->>C: apiReceiveFile 7 DATA/forwarded/group-15/7.pdf
  C->>T: rcvFileComplete: resolved with the forwarded path
  T->>T: copy for the post: DATA/files/sent/send-XXXXXX/report.pdf
  K->>C: apiSendMessages #G [{fileSource: the copy, msgContent: file, quotedItemId: card N when the message fits}]
  C->>T: sndFileCompleteXFTP: the copy's folder deleted
  Note over K,O: the card stays pending; the agent's message shows the file as offered
  O->>C: /receive #15 N
  K->>T: performDownload: downloadFile
  T->>T: the forwarded file moved to DATA/files/group-15/7.pdf
  T-->>A: {action: N, status: done, chat: #15, item: 30, path}
```

Notes:

- `/forward` gets no reply; a download or post that fails is posted as ``Action N: the file was not received: `ERROR`.``, and a failed post deletes the copy ([control.ts:265-281](../src/control.ts#L265-L281)).
- The copy is named by the name the sender gave ([tools.ts:938-942](../src/tools.ts#L938-L942)).
- The post keeps the sender's preview of an image or video; it replies to the card when the message, with the card as its quote, is within 15602 bytes by the length rule of `send` ([control.ts:265-281](../src/control.ts#L265-L281), [tools.ts:1497-1513](../src/tools.ts#L1497-L1513)).
- The download is shared with the agent's: one download per file ID; the move to the download path waits for the copy of the post ([tools.ts:539-566](../src/tools.ts#L539-L566)).
- The forwarded path is outside `DATA/files/`, so `send` refuses the file there; it is deleted when the agent leaves the chat, and by `/delete CHAT files` ([tools.ts:911-916](../src/tools.ts#L911-L916), [:949-967](../src/tools.ts#L949-L967), [:990-1004](../src/tools.ts#L990-L1004)).

Source: [control.ts:221-239](../src/control.ts#L221-L239), [:265-281](../src/control.ts#L265-L281), [tools.ts:539-556](../src/tools.ts#L539-L556), [:888-898](../src/tools.ts#L888-L898), [:938-942](../src/tools.ts#L938-L942).

---

## 15. `send` with a file

```mermaid
sequenceDiagram
  participant A as Agent
  participant T as AgentTools
  participant X as Actions
  participant K as Control
  participant C as Core
  participant O as Owner
  A->>T: send {chat: #15, text: "trail map", file: "/home/alice/trail.jpg"}
  T->>C: apiGetChat #15 count 0
  T->>T: real path, size, inode, change time
  T->>T: refused paths checked
  T->>T: preview: core.resizeImage(file, MessagePreview)
  T->>T: message length checked
  T->>X: create(key, send fields with the file)
  K->>O: image card with the preview: I want to send a message to group Trail: `trail map`, attaching image `/home/alice/trail.jpg`, SIZE /'approve #15 N', ...
  O->>C: /approve #15 N
  X->>T: performSend
  T->>T: the file again: same real path, size, inode, change time
  T->>T: core.resizeImage(file, Image): DATA/files/sent/send-XXXXXX/trail.jpg
  T->>C: apiSendMessages #15 [{fileSource: the copy, msgContent: image with the preview}]
  C->>T: sndFileCompleteXFTP: the copy's folder deleted
```

Notes:

- A refused file ends the call with an error, and the refusal is posted to the control chat and logged ([tools.ts:902-905](../src/tools.ts#L902-L905), [control.ts:154-158](../src/control.ts#L154-L158)).
- A video's preview is the first frame from `ffmpeg` and its duration comes from `ffprobe`; a GIF or WebP gets the fixed preview ([tools.ts:1576-1610](../src/tools.ts#L1576-L1610)).
- A changed file fails the perform: ``file: PATH changed after the request.`` ([tools.ts:918-923](../src/tools.ts#L918-L923)).
- `sndFileError` deletes the copy too, and the failure is posted to the control chat and logged ([serve.ts:163-165](../src/serve.ts#L163-L165), [control.ts:160-169](../src/control.ts#L160-L169)).

Source: [tools.ts:791-817](../src/tools.ts#L791-L817), [:861-936](../src/tools.ts#L861-L936), [control.ts:441-461](../src/control.ts#L441-L461), [:583-589](../src/control.ts#L583-L589).

---

## 16. `/leave` and `/delete CHAT files`

```mermaid
sequenceDiagram
  participant P as Person
  participant T as AgentTools
  participant X as Actions
  participant K as Control
  participant C as Core
  participant O as Owner
  participant D as Peer
  O->>C: /leave #9
  K->>T: knownChat #9
  K->>T: leaveChat #9
  T->>C: apiDeleteChatItems #9 [status item] broadcast (when #9 has a status message)
  T->>C: apiLeaveGroup #9, apiDeleteChat #9 full, notify
  C->>D: the agent left the group
  T->>T: DATA/forwarded/group-9 deleted
  K->>P: Left business Ridgeline (#9), requested by Alice.
  K->>X: reject each pending action of #9 with a card
  K->>O: Left business Ridgeline (#9), requested by Alice.
  K->>O: Files from business Ridgeline (#9) are kept in `DATA/files/group-9`: 3 files, 2.1 MB. /'delete #9 files'
  O->>C: /delete #9 files
  K->>T: deleteChatFiles #9
  T->>T: each download of #9: failed, apiCancelFile, its partial file deleted
  T->>C: apiGetChat #9 count 100, then before the first item, while a page has 100
  T->>C: apiCancelFile for each other file in rcvAccepted, rcvTransfer or rcvWarning
  T->>T: DATA/files/group-9 and DATA/forwarded/group-9 deleted
  K->>O: Deleted 3 files from #9, 2.1 MB.
```

Notes:

- `/leave` acts at once in both modes, and works for a contact as `Deleted bot X (@4), requested by Alice.` ([control.ts:283-297](../src/control.ts#L283-L297)).
- With `/leave`, the status message of the chat is deleted first, as with `leave` ([7](#7-leave), [tools.ts:990-993](../src/tools.ts#L990-L993)).
- The line `Left ...` goes to the log and to the owners; the members of #9 see only that the agent left.
- The files message is posted after the reply, and only when the folder has files ([control.ts:294-295](../src/control.ts#L294-L295), [:309-311](../src/control.ts#L309-L311)).
- `/delete CHAT files` works for a current or a left chat; the failed downloads return `Your owner deleted the files of this chat.`, and the reply counts the forwarded files too ([tools.ts:949-967](../src/tools.ts#L949-L967)).
- The paged reads find the downloads the core continues after a restart, which have no download in memory; for a left chat they find none ([messages.ts:271-279](../src/messages.ts#L271-L279), [tools.ts:956-962](../src/tools.ts#L956-L962)).

Source: [control.ts:283-307](../src/control.ts#L283-L307), [tools.ts:944-967](../src/tools.ts#L944-L967), [:990-1004](../src/tools.ts#L990-L1004).

---

## 17. Status message while the agent works

```mermaid
sequenceDiagram
  participant P as Person
  participant A as Agent
  participant T as AgentTools
  participant L as Statuses
  participant I as Inference endpoint
  participant X as Actions
  participant C as Core
  participant O as Owner
  Note over T,O: #9 is a group, approved with /approve #9 all, the server runs with --inference
  O->>C: summarize README.md (item 40 in #9)
  A->>T: receive {chat: #9, wait: 3600, typing: true}
  T->>C: apiGetChat #9 initial 1, then the unread items
  T->>C: apiChatItemsRead #9 [40]
  T->>L: start(#9)
  T-->>A: {chat: #9, messages: [40], more: 0}
  L->>T: allowed(#9): knownChat, showsStatus
  L->>C: apiSendMessages #9 [◐ thinking · 0:00] live: item S
  loop each tick, a tenth of the age after the previous one, 3 s to 20 s
    L->>T: allowed(#9): knownChat, showsStatus
    L->>I: GET /running, then /props and /slots of each allowed proxy, at most once per 3 s
    L->>C: apiUpdateChatItem #9 S [◑ generating, 31 tokens/s · 0:18] live
  end
  A->>T: send {chat: #9, text: "Reading the file…", live: true}
  T->>C: apiGetChat #9 count 0
  T->>T: postingRefusal and statusRefusal: none
  T->>L: show(#9, note)
  L->>C: apiUpdateChatItem #9 S [◒ generating, 31 tokens/s · 0:21, the note under it] live
  T->>P: Note to group Team (#9): Reading the file…
  T-->>A: {status: live, chat: #9}
  A->>T: send {chat: #9, text: "The README describes …", wait: 0}
  T->>X: create(key, send fields, "Alice"): performing, standing
  X->>T: performSend
  T->>C: apiSendMessages #9 [The README describes …]
  T->>L: end(#9)
  L->>C: apiDeleteChatItems #9 [S] broadcast
  T-->>A: {action: N, status: done, chat: #9, item, responses: [], more: 0}
```

Notes:

- Eligibility is read before the first post and at each tick: the agent can post in #9, #9 is a chat with a person, a group or a business chat and is not being left, the mode is `requests`, the control chat is not lost, and #9 has a standing approval of messages ([tools.ts:828-839](../src/tools.ts#L828-L839), [:1449-1456](../src/tools.ts#L1449-L1456), [status.ts:116-123](../src/status.ts#L116-L123)). When #9 is not eligible at the first tick, the status ends without a post.
- The status is started for any message returned with `typing: true`, whatever its sender; a start in a chat with a status is ignored ([tools.ts:1198](../src/tools.ts#L1198), [status.ts:48-52](../src/status.ts#L48-L52)).
- The label is read for renders 2 to 49, after the inference poll; the labels and their order are in [API 1.10](api.md#110-status-message) ([status.ts:124-128](../src/status.ts#L124-L128), [:144-149](../src/status.ts#L144-L149), [:171-192](../src/status.ts#L171-L192)).
- At the 50th render, the line is `◌ working since YYYY-MM-DD HH:MM UTC`; later ticks are run every 5 min without a render, and notes are rendered up to the 59th render ([status.ts:129-130](../src/status.ts#L129-L130), [:140](../src/status.ts#L140), [:167](../src/status.ts#L167)).
- A note is checked as a `send` up to the role check, then by `statusRefusal`; it is rendered at once without an action, and the result is returned without a wait ([tools.ts:796-803](../src/tools.ts#L796-L803), [:819-826](../src/tools.ts#L819-L826), [status.ts:54-64](../src/status.ts#L54-L64)).
- The answer is a new message; the status message is deleted after `apiSendMessages` returns, and it is kept after a failed answer ([tools.ts:867-877](../src/tools.ts#L867-L877)). The outcome of the answer is posted to the control chat as in [6](#6-send-under-a-standing-approval-and-revoke).
- Other deletions: a `receive` of #9 that waits, `leave` and `/leave`, a tick of a chat that is not eligible, 23 hours, 30 minutes without calls from 50 renders, a failed send or update of the status, the stop ([API 1.10](api.md#110-status-message)).
- A render queued while the first send of a status runs is sent after it. When that send fails, a new live item is posted for the queued render, and that item is removed by the deletion queued after it ([api.ts:1187-1189](../../simplex-chat-nodejs/src/api.ts#L1187-L1189), [:1199-1208](../../simplex-chat-nodejs/src/api.ts#L1199-L1208), [status.ts:111-114](../src/status.ts#L111-L114)).
- Polls are shared by all status messages for 3 s; one 1 s deadline is set for all requests of a poll; a failed poll is logged as `inference endpoint: ERROR` unless the previous one failed ([inference.ts:34-48](../src/inference.ts#L34-L48), [status.ts:151-162](../src/status.ts#L151-L162)).

Source: [tools.ts:791-839](../src/tools.ts#L791-L839), [:861-886](../src/tools.ts#L861-L886), [:1167-1203](../src/tools.ts#L1167-L1203), [status.ts:28-192](../src/status.ts#L28-L192), [inference.ts:31-107](../src/inference.ts#L31-L107).
