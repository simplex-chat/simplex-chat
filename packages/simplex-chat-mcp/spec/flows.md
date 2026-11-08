# SimpleX MCP server: flows

> Sequence diagrams of the main flows of `@simplex-chat/mcp` 0.3.0, each with notes and source locations: `serve` in flows 1-16, the Claude Code plugin in flows 17-30.
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

In flows 1-16, `#G` is the agent control chat, `USER` the agent's user ID, `DATA` the data folder. Calls to the core are shown as the `ChatApi` methods the server calls, with their arguments in short. Card and progress texts with a semicolon are shown with a comma in the diagrams; the exact texts are in [API](api.md).

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

Claude Code plugin ([participants](#claude-code-plugin-flows)):

17. [First session start and the owner joins](#17-first-session-start-and-the-owner-joins)
18. [Session process start and attach](#18-session-process-start-and-attach)
19. [A tool call with approval in the session group](#19-a-tool-call-with-approval-in-the-session-group)
20. [A prompt from the session group](#20-a-prompt-from-the-session-group)
21. [A terminal prompt and the answer](#21-a-terminal-prompt-and-the-answer)
22. [A tool use approved in the session group](#22-a-tool-use-approved-in-the-session-group)
23. [Mode `host` for a session](#23-mode-host-for-a-session)
24. [`/give`](#24-give)
25. [`reset-owners`](#25-reset-owners)
26. [Idle close, idle stop and a newer version](#26-idle-close-idle-stop-and-a-newer-version)
27. [`/clear`, `--continue` and `/resume` in one session](#27-clear---continue-and-resume-in-one-session)
28. [A member leaves, is removed or demoted in the Main group](#28-a-member-leaves-is-removed-or-demoted-in-the-main-group)
29. [`reply` while members who are not owners are in the group](#29-reply-while-members-who-are-not-owners-are-in-the-group)
30. [The group of a session closed for 7 days, and a run that is not interactive](#30-the-group-of-a-session-closed-for-7-days-and-a-run-that-is-not-interactive)

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
- With `--data`, `--data <folder>` is appended to the register hint, shell-quoted ([serve.ts:157-159](../src/serve.ts#L157-L159)).
- With `--qr-code small`, the line `If the QR code does not scan, ...` is printed after the link ([control.ts:476](../src/control.ts#L476)).
- Events that arrive before the HTTP listener starts are buffered and handled in order ([core.ts:62-76](../src/core.ts#L62-L76)).
- The server runs `ownerJoined` for every `joinedGroupMember` event of a human without `--owners`, and the link is deleted in that handler ([control.ts:224-225](../src/control.ts#L224-L225), [:435-442](../src/control.ts#L435-L442)).

Source: [cli.ts:218-251](../src/cli.ts#L218-L251), [serve.ts:64-122](../src/serve.ts#L64-L122), [core.ts:38-82](../src/core.ts#L38-L82), [files.ts:99-142](../src/files.ts#L99-L142), [http.ts:14-46](../src/http.ts#L14-L46), [control.ts:105-130](../src/control.ts#L105-L130), [:405-409](../src/control.ts#L405-L409), [:470-489](../src/control.ts#L470-L489), [:564-588](../src/control.ts#L564-L588).

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

Source: [cli.ts:130-145](../src/cli.ts#L130-L145), [:227](../src/cli.ts#L227), [register.ts:33-58](../src/register.ts#L33-L58), [files.ts:144-147](../src/files.ts#L144-L147).

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

- The plan is read again in the perform step ([tools.ts:825](../src/tools.ts#L825)).
- Join methods per plan ([tools.ts:856-884](../src/tools.ts#L856-L884), [:1398-1461](../src/tools.ts#L1398-L1461)):

| Plan | Methods |
|---|---|
| known prepared contact or group | `apiConnectPreparedContact(C, incognito, message)`<br>`apiConnectPreparedGroup(G, incognito, message)` |
| channel link (`direct` false, with link data) | `apiPrepareGroup(USER, LINK, false, linkData, domain)`<br>`apiConnectPreparedGroup(G, incognito)` |
| contact address with link data, and a SimpleX name or a message | `apiPrepareContact(USER, LINK, linkData, domain)`<br>`apiConnectPreparedContact(C, incognito, message)`, or `apiConnectPreparedGroup` for a business |
| `contactViaAddress` | `apiConnectContactViaAddress(USER, incognito, CONTACT_ID)` |
| other links | `apiConnectPreparedLink(USER, incognito, LINK)` |

- After a failed prepared connect, the server deletes the prepared chat without notifying ([tools.ts:886-893](../src/tools.ts#L886-L893)).
- The connection is matched by `contactConnected` or `userJoinedGroup`: by contact or group ID, or by the pending connection ID ([tools.ts:661-669](../src/tools.ts#L661-L669), [:895-900](../src/tools.ts#L895-L900)). In the plugin, a pending connection is also recorded in the session group's data, and its chat becomes the session's when it connects after the call returned, within a day of the record ([tools.ts:836](../src/tools.ts#L836), [hub.ts:164-169](../src/hub.ts#L164-L169), [:512-513](../src/hub.ts#L512-L513)).
- In the perform step the server waits for the other side until `startedAt + wait`, at most until `created + 23 h 50 min`, and then reports `connected: false` ([tools.ts:154](../src/tools.ts#L154), [:824](../src/tools.ts#L824), [:850-853](../src/tools.ts#L850-L853)).
- Without `message`, the idle time is counted from the connection; with `message`, from the first reply ([tools.ts:907](../src/tools.ts#L907), [messages.ts:327-335](../src/messages.ts#L327-L335)).
- For a target the agent is connected to, the result is `status: known` with its chat fields, and no action is created; in the plugin, the chat becomes the session's when it has no owner ([tools.ts:747](../src/tools.ts#L747), [:760-765](../src/tools.ts#L760-L765)).
- Exact progress texts: `Action N waits for an approver until TIME.`, then `Action N waits for the connection until TIME.`, then `K messages in CHAT; waiting until TIME.` ([actions.ts:422-433](../src/actions.ts#L422-L433), [tools.ts:793](../src/tools.ts#L793)).

Source: [tools.ts:742-807](../src/tools.ts#L742-L807), [:821-910](../src/tools.ts#L821-L910), [actions.ts:187-222](../src/actions.ts#L187-L222), [:256-274](../src/actions.ts#L256-L274), [control.ts:174-185](../src/control.ts#L174-L185).

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

- Checks before the action: `text` or `file`, the chat connected, role `observer` or `relay` refused, `reply_to` item a message, mentioned members present and the chat a group, and the message length with its quote ([tools.ts:917-930](../src/tools.ts#L917-L930)).
- The server starts collecting before the send and, after it, drops items with IDs up to the sent item ([tools.ts:962](../src/tools.ts#L962), [:975-977](../src/tools.ts#L975-L977)).
- Mentioned names missing from the text are prefixed as `@name` ([tools.ts:1583-1586](../src/tools.ts#L1583-L1586)).
- While a collected message is live, the server waits for its final update; live items are left out of `responses` and counted in `more` ([messages.ts:327-335](../src/messages.ts#L327-L335), [tools.ts:948](../src/tools.ts#L948), [:954](../src/tools.ts#L954)).
- At most 100 replies are kept per action; later ones are counted as overflow ([messages.ts:10](../src/messages.ts#L10), [:109-110](../src/messages.ts#L109-L110)).

Source: [tools.ts:912-983](../src/tools.ts#L912-L983), [messages.ts:99-114](../src/messages.ts#L99-L114), [:327-335](../src/messages.ts#L327-L335), [control.ts:668-674](../src/control.ts#L668-L674).

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

- With unread items the server reads `around` the first unread item, `count` 0, then pages of `after` the last item, `count` 100, until it holds `limit + 1` unread, non-live messages or a page is short ([messages.ts:289-303](../src/messages.ts#L289-L303)).
- Unread items other than messages in the read pages are marked read with the returned messages ([tools.ts:1261](../src/tools.ts#L1261), [:1278](../src/tools.ts#L1278)).
- `more`: the arrived messages not returned, the overflow and the unread items not read ([tools.ts:1280](../src/tools.ts#L1280)).
- Items received while the pages are read are returned once ([tools.ts:1260-1263](../src/tools.ts#L1260-L1263)).
- Exact progress text: `No new messages in CHAT; waiting until TIME.` ([tools.ts:1267](../src/tools.ts#L1267)).

Source: [tools.ts:1249-1284](../src/tools.ts#L1249-L1284), [messages.ts:289-335](../src/messages.ts#L289-L335).

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

- The approval is used in mode `requests` while member `approvedById` is an approver ([control.ts:155-158](../src/control.ts#L155-L158)).
- For sends that were pending at `/approve CHAT all`, the card ends with the plain `Approved by Alice.` ([control.ts:634](../src/control.ts#L634), [:643](../src/control.ts#L643)).
- `connect` and `leave` actions are created without the standing approver, so they stay pending ([tools.ts:753](../src/tools.ts#L753), [:1079](../src/tools.ts#L1079)).
- `/receive CHAT N all` and `/receive CHAT off` work the same way for downloads, with the key `simplexMcpFiles`, except that download N alone is performed, and the other pending downloads of the chat stay pending ([control.ts:25-28](../src/control.ts#L25-L28), [:288-290](../src/control.ts#L288-L290), [tools.ts:1111](../src/tools.ts#L1111)).

Source: [control.ts:280-313](../src/control.ts#L280-L313), [:401-403](../src/control.ts#L401-L403), [tools.ts:934-936](../src/tools.ts#L934-L936), [actions.ts:187-222](../src/actions.ts#L187-L222), [:337-355](../src/actions.ts#L337-L355).

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

- For a contact, the card text is `I want to delete KIND NAME (@N).`, and the perform step calls `apiDeleteChat` for the contact, full, with notify ([control.ts:679-682](../src/control.ts#L679-L682), [tools.ts:1087](../src/tools.ts#L1087)).
- The delete notifies, so the core notifies the contact or the members ([tools.ts:1087](../src/tools.ts#L1087), [:1090](../src/tools.ts#L1090)).
- The server looks for an attached action before it reads the chat, so a repeated call after the deletion is attached to the done action ([tools.ts:1074-1076](../src/tools.ts#L1074-L1076)).

Source: [tools.ts:1072-1094](../src/tools.ts#L1072-L1094), [control.ts:178-185](../src/control.ts#L178-L185), [:349-352](../src/control.ts#L349-L352).

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

- New actions are still refused unless the control chat has an approver ([control.ts:138-142](../src/control.ts#L138-L142)).
- In mode `host`, `standingApprover` returns `undefined`, so standing approvals are ignored by `send`, `download`, `list` and `/list`, and `/approve CHAT all` stores nothing; downloads get no card, so `/receive CHAT N all` has no card to act on ([control.ts:157](../src/control.ts#L157), [:248](../src/control.ts#L248)).
- A download in mode `host` is received without the approval of unknown servers, and it gets no card and no outcome post ([tools.ts:616](../src/tools.ts#L616), [:1165](../src/tools.ts#L1165), [control.ts:560-562](../src/control.ts#L560-L562)).
- The kept-files message also follows a done `leave` in mode `host` ([control.ts:183-184](../src/control.ts#L183-L184)).

Source: [tools.ts:543-545](../src/tools.ts#L543-L545), [actions.ts:200](../src/actions.ts#L200), [:353](../src/actions.ts#L353), [control.ts:178-185](../src/control.ts#L178-L185), [:634](../src/control.ts#L634).

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
  Note over X: expires = T + expireHours, timer set for expires - now
  X->>X: timer fires, or expireDue before an owner command
  X->>X: pending and due: expired
  X->>K: outcome
  K->>O: card edited: ... Expired.
  X-->>A: {action: N, status: expired}
  X->>X: removed with the result
  O->>K: /approve N
  K->>O: Action N is not pending.
```

Notes:

- A decided action with an open count of 0 is removed once its expiry time has passed: at its timer, at a later decision or at the close of its last call ([actions.ts:320-322](../src/actions.ts#L320-L322)).
- A timer that fires before the expiry time, on the monotonic clock, is set again ([actions.ts:313](../src/actions.ts#L313)).
- Expiry times are listed in the `/pending` reply ([control.ts:710-712](../src/control.ts#L710-L712)).
- The server ends the connect wait at `created + 23 h 50 min`, within the 24 hours in which a message can be edited ([tools.ts:154](../src/tools.ts#L154), [:824](../src/tools.ts#L824)).

Source: [actions.ts:197](../src/actions.ts#L197), [:250-254](../src/actions.ts#L250-L254), [:304-322](../src/actions.ts#L304-L322), [control.ts:236](../src/control.ts#L236), [:264](../src/control.ts#L264).

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

- The key is the session number, `undefined` for `serve`, the tool name and the action fields: `target`, `incognito`, `message` for `connect`; `chat`, `text`, `file`, `as`, `reply_to`, `mention` for `send`; `chat`, `item` for `download`; `chat` for `leave` ([tools.ts:743](../src/tools.ts#L743), [:914](../src/tools.ts#L914), [:1074](../src/tools.ts#L1074), [:1098](../src/tools.ts#L1098), [:1713-1715](../src/tools.ts#L1713-L1715)).
- The `wait`, `idle` and `limit` of the attaching call are used ([tools.ts:942](../src/tools.ts#L942)).
- For a call aborted before `create`, the server returns before it creates an action ([tools.ts:752](../src/tools.ts#L752), [:935](../src/tools.ts#L935), [:1078](../src/tools.ts#L1078), [:1112](../src/tools.ts#L1112)).
- Abort path in the SDK: on `res` close the web request is aborted ([node index.cjs:299-303](../node_modules/@modelcontextprotocol/node/dist/index.cjs#L299-L303)); the per-request transport is closed ([server index.cjs:86-90](../node_modules/@modelcontextprotocol/server/dist/index.cjs#L86-L90)); the handler signal is aborted ([src-BhnGYybN.cjs:6425](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs#L6425)).

Source: [tools.ts:771-783](../src/tools.ts#L771-L783), [:939-957](../src/tools.ts#L939-L957), [:1292](../src/tools.ts#L1292), [actions.ts:256-268](../src/actions.ts#L256-L268), [messages.ts:311-325](../src/messages.ts#L311-L325).

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
  S->>P: Stopping: sending 2 cancelled cards, up to 10 s. Ctrl-C to stop now.
  S->>S: wait 10 s, or until a second Ctrl-C
  S->>C: close
  S->>S: remove DATA/lock
  S->>P: Stopped.
  Note over P,O: next start: counter continues, /approve of an old number: Action N is not pending.
```

Notes:

- The interrupted card line is `DECIDER; interrupted: the server stopped.` ([control.ts:641](../src/control.ts#L641)).
- An action of mode `host` or of a standing approval whose counter is being written at the stop is set to `pending` and cancelled without a perform ([actions.ts:337-342](../src/actions.ts#L337-L342)).
- After a crash, the cards of that run are left with their commands; `/approve` of their numbers gets the reply `Action N is not pending.` ([control.ts:264](../src/control.ts#L264)).
- The stop signal of the performs is aborted after the actions are marked interrupted: a `connect` waiting for its connection ends, and an interrupted perform logs no line ([actions.ts:297-298](../src/actions.ts#L297-L298), [:352](../src/actions.ts#L352), [tools.ts:850-851](../src/tools.ts#L850-L851)).
- The wait applies only when cards were edited or posted at the stop, a `download` without a card excluded, and `--stop-wait` is above 0 ([serve.ts:149](../src/serve.ts#L149), [:161-175](../src/serve.ts#L161-L175)).

Source: [serve.ts:46-62](../src/serve.ts#L46-L62), [:143-153](../src/serve.ts#L143-L153), [:161-175](../src/serve.ts#L161-L175), [actions.ts:291-322](../src/actions.ts#L291-L322), [http.ts:48-52](../src/http.ts#L48-L52), [control.ts:632-651](../src/control.ts#L632-L651).

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

- Until the restart with a list, new actions are refused: `The agent has no approving owner. ...` ([control.ts:140-142](../src/control.ts#L140-L142)).
- An entry must match a human by member ID and display name, folded to one line as the restart line prints it, or the server exits with code 2 ([control.ts:418-422](../src/control.ts#L418-L422), [:831-833](../src/control.ts#L831-L833)).
- With `--reset-owners`, the server deletes the control chat for all members and creates a new one with the same counter; the CLI refuses the option with an `--owners` list ([control.ts:590-602](../src/control.ts#L590-L602), [serve.ts:181](../src/serve.ts#L181), [cli.ts:124-126](../src/cli.ts#L124-L126)).
- When a listed owner leaves and none remains: `... is no longer an owner. No owners remain: restart the server with --reset-owners --owners` ([control.ts:457-462](../src/control.ts#L457-L462)).

Source: [control.ts:411-433](../src/control.ts#L411-L433), [:444-463](../src/control.ts#L444-L463), [:470-480](../src/control.ts#L470-L480), [:831-857](../src/control.ts#L831-L857).

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

- A check every 60 s reads the item: a deleted message or chat fails the download; 30 minutes without a rise of `rcvProgress` fail and cancel it ([tools.ts:1170-1197](../src/tools.ts#L1170-L1197)).
- A download still running `wait` seconds after the perform starts returns `status: performing`; the same call again attaches to the action ([tools.ts:1143-1146](../src/tools.ts#L1143-L1146)).
- `/approve #15 N` on a download card gets the reply `Action N is a download: ...` with the card's command line ([control.ts:271](../src/control.ts#L271)).
- A file on unknown servers gets a card that names their hosts and leaves out `/'receive CHAT N all'` ([control.ts:683-688](../src/control.ts#L683-L688), [:703-706](../src/control.ts#L703-L706), [:756-760](../src/control.ts#L756-L760)). Only such a card approves the relays; the core checks the complete description of every other download, which fails at the acceptance, or at the next check when the description completes later ([tools.ts:616](../src/tools.ts#L616), [:1178](../src/tools.ts#L1178)).
- The card of an image or video shows the sender's preview ([control.ts:505-525](../src/control.ts#L505-L525)).
- A file the core receives without a request, a short voice message, also in a member chat of a group, is moved into the chat's folder at `rcvFileComplete`; one whose transfer started before a restart, only when it has its full size. Its message then shows it as `offered`, and `download` returns its path after the approval ([tools.ts:622-642](../src/tools.ts#L622-L642), [:1217-1226](../src/tools.ts#L1217-L1226), [messages.ts:194-208](../src/messages.ts#L194-L208)).

Source: [tools.ts:1096-1238](../src/tools.ts#L1096-L1238), [control.ts:653-708](../src/control.ts#L653-L708), [tools.ts:671-698](../src/tools.ts#L671-L698).

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

- `/forward` gets no reply; a download or post that fails is posted as ``Action N: the file was not received: `ERROR`.``, and a failed post deletes the copy ([control.ts:315-331](../src/control.ts#L315-L331)).
- The copy is named by the name the sender gave ([tools.ts:1035-1039](../src/tools.ts#L1035-L1039)).
- The post keeps the sender's preview of an image or video; it replies to the card when the message, with the card as its quote, is within 15602 bytes by the length rule of `send` ([control.ts:315-331](../src/control.ts#L315-L331), [tools.ts:1592-1608](../src/tools.ts#L1592-L1608)).
- The download is shared with the agent's: one download per file ID; the move to the download path waits for the copy of the post ([tools.ts:593-620](../src/tools.ts#L593-L620)).
- The forwarded path is outside `DATA/files/`, so `send` refuses the file there; it is deleted when the agent leaves the chat, and by `/delete CHAT files` ([tools.ts:1008-1013](../src/tools.ts#L1008-L1013), [:1046-1064](../src/tools.ts#L1046-L1064), [:1086-1094](../src/tools.ts#L1086-L1094)).

Source: [control.ts:258-276](../src/control.ts#L258-L276), [:315-331](../src/control.ts#L315-L331), [tools.ts:593-610](../src/tools.ts#L593-L610), [:985-995](../src/tools.ts#L985-L995), [:1035-1039](../src/tools.ts#L1035-L1039).

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

- A refused file ends the call with an error, and the refusal is posted to the control chat and logged ([tools.ts:999-1002](../src/tools.ts#L999-L1002), [control.ts:187-191](../src/control.ts#L187-L191)).
- A video's preview is the first frame from `ffmpeg` and its duration comes from `ffprobe`; a GIF or WebP gets the fixed preview ([tools.ts:1671-1705](../src/tools.ts#L1671-L1705)).
- A changed file fails the perform: ``file: PATH changed after the request.`` ([tools.ts:1015-1020](../src/tools.ts#L1015-L1020)).
- `sndFileError` deletes the copy too, and the failure is posted to the control chat and logged ([tools.ts:694-696](../src/tools.ts#L694-L696), [serve.ts:133](../src/serve.ts#L133), [control.ts:193-202](../src/control.ts#L193-L202)).

Source: [tools.ts:912-937](../src/tools.ts#L912-L937), [:959-1033](../src/tools.ts#L959-L1033), [control.ts:505-525](../src/control.ts#L505-L525), [:668-674](../src/control.ts#L668-L674).

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

- `/leave` acts at once in both modes, and works for a contact as `Deleted bot X (@4), requested by Alice.` ([control.ts:333-347](../src/control.ts#L333-L347)).
- The line `Left ...` goes to the log and to the owners; the members of #9 see only that the agent left.
- The files message is posted after the reply, and only when the folder has files ([control.ts:344-345](../src/control.ts#L344-L345), [:373-375](../src/control.ts#L373-L375)).
- `/delete CHAT files` works for a current or a left chat; the failed downloads return `Your owner deleted the files of this chat.`, and the reply counts the forwarded files too ([tools.ts:1046-1064](../src/tools.ts#L1046-L1064)).
- The paged reads find the downloads the core continues after a restart, which have no download in memory; for a left chat they find none ([messages.ts:275-283](../src/messages.ts#L275-L283), [tools.ts:1053-1059](../src/tools.ts#L1053-L1059)).

Source: [control.ts:333-357](../src/control.ts#L333-L357), [tools.ts:1041-1064](../src/tools.ts#L1041-L1064), [:1086-1094](../src/tools.ts#L1086-L1094).

---

## Claude Code plugin flows

Participants of the plugin flows:

| Letter | Participant |
|---|---|
| CC | Claude Code |
| HK | a hook, `node dist/cli.js hook ...` in the plugin's folder |
| S | the session process, `simplex-mcp session` |
| D | the daemon: socket routes and entries |
| M | `MainGroup` |
| N | the `Session` of session N, with its `Control` and `Actions` |
| T | the session's `AgentTools` |
| C | the core `_hub`, profile `Claude` |
| U | `User` with the core `_user` |
| O | an owner in the SimpleX app |

`#M` is the Main group, `#G` the group of session N, ROOT the data root.

---

## 17. First session start and the owner joins

```mermaid
sequenceDiagram
  participant CC as Claude Code
  participant HK as SessionStart hook
  participant D as Daemon
  participant M as MainGroup
  participant N as Session 1
  participant C as Core _hub
  participant U as User
  participant O as Owner
  CC->>HK: {session_id, session_title, cwd, model}
  HK->>D: GET /status: not running
  HK->>D: spawn simplex-mcp daemon
  D->>D: lock, stored sessions, socket listens
  D->>C: openCore ROOT/_hub "Claude"
  D->>C: apiNewGroup "SimpleX MCP", {simplexMcpMain: true}
  M->>C: apiGetGroupLinkStr #M: none, apiCreateGroupLink #M member
  D->>U: openCore ROOT/_user "User"
  U->>C: joins the hub's one-time link; the contact of its connection marked simplexMcpUser
  D-->>HK: ready
  HK->>D: POST /title {id, pid, name, project, model, config}
  D->>D: ROOT/_sessions/ID/session.json {number: 1, name}
  D->>N: Hub.session
  N->>C: apiNewGroup "1 · NAME", {simplexMcpSession: {session: 1, invited: []}}
  N->>C: apiAddMember #G User
  U->>C: apiJoinGroup #G, marked {simplexMcpSession: 1}
  N->>C: apiUpdateGroupProfile #G shortDescr MODEL
  D-->>HK: {mainGroup, owner: {owners: [], link}}
  HK-->>CC: systemMessage: scan this QR code ..., QR code, link
  O->>C: joins #M by the link
  C->>M: joinedGroupMember
  M->>C: apiDeleteGroupLink #M, apiSetMembersRole #M owner
  M->>O: I'm Claude. Each Claude Code session has its own group ...
  M->>N: inviteOwner: apiAddMemberViaGroup #G via #M, role owner
  N->>C: apiSetGroupCustomData #G invited [{groupMemberId, mainMemberId}]
  O->>C: joins #G from the invitation in #M
```

Notes:

- The link reaches the person only: the hook's `systemMessage` carries it, and no hook context or tool result does ([hook.ts:51-53](../src/hook.ts#L51-L53)).
- The core deletes the preset contact of a new profile, both in `_hub` and in `_user` ([core.ts:29](../src/core.ts#L29), [:45-53](../src/core.ts#L45-L53)).
- `User` connects at the first start only: later starts find the hub's contact marked `simplexMcpUser`. The contact is the one whose connection is that of the invitation the hub created; when `User` does not connect in 60 s, the daemon runs without it, and terminal prompts are not posted ([user.ts:22](../src/user.ts#L22), [:39-49](../src/user.ts#L39-L49), [:61-63](../src/user.ts#L61-L63), [hub.ts:371](../src/hub.ts#L371)).
- A later daemon start without owners keeps the Main group's link ([main-group.ts:74-82](../src/main-group.ts#L74-L82)).
- Owners are invited to the groups of open sessions when they join; the groups of closed sessions are joined by `/join N` ([hub.ts:445-449](../src/hub.ts#L445-L449), [:432-443](../src/hub.ts#L432-L443)).

Source: [hook.ts:41-57](../src/hook.ts#L41-L57), [client.ts:35-92](../src/client.ts#L35-L92), [daemon.ts:98-127](../src/daemon.ts#L98-L127), [:248-278](../src/daemon.ts#L248-L278), [hub.ts:363-380](../src/hub.ts#L363-L380), [:403-421](../src/hub.ts#L403-L421), [:485-497](../src/hub.ts#L485-L497), [main-group.ts:144-166](../src/main-group.ts#L144-L166), [user.ts:17-58](../src/user.ts#L17-L58).

---

## 18. Session process start and attach

```mermaid
sequenceDiagram
  participant CC as Claude Code
  participant S as Session process
  participant D as Daemon
  participant N as Session N
  CC->>S: node dist/cli.js session, with CLAUDE_CODE_SESSION_ID
  CC->>S: initialize
  S-->>CC: tools, instructions, capabilities claude/channel and claude/channel/permission
  S->>D: GET /status, version and protocol
  S->>D: POST /session {id, pid: parent process, project, config}
  D->>D: resolve id: alias; id with session.json; session of pid, or a wait of up to 10 s for the title of pid
  D->>N: configure, requestMode(approve)
  D-->>S: {number, displayName, owner}
  S->>D: POST /attach {id}
  D-->>S: {version, protocol}
  D->>N: onLine = the entry's writers
  D-->>S: the /attach stream ends: the daemon stopped
  S->>S: after 1 s, then twice as late after each failure, up to 30 s
  S->>D: GET /status, spawn when none runs, POST /session, POST /attach
```

Notes:

- An invalid option, or before it the text of a run that is not interactive, is kept as the result of every call, and the session process does not connect ([session.ts:44-51](../src/session.ts#L44-L51), [:65](../src/session.ts#L65)); see [flow 30](#30-the-group-of-a-session-closed-for-7-days-and-a-run-that-is-not-interactive).
- `/session` for a session without a folder creates it; `/attach`, `/call`, `/permission` and `/turn` answer 409 for it, and `call` registers again once ([daemon.ts:409-424](../src/daemon.ts#L409-L424), [session.ts:116-127](../src/session.ts#L116-L127)).
- The end of the `/attach` stream, or a failed connection, also a failed spawn, drops the connection and connects again, so the channel works again without a call; one timer serves each attempt, so the delay doubles once per failed attempt; another protocol version, the `ProtocolError`, ends the retries ([session.ts:85-109](../src/session.ts#L85-L109), [:172-178](../src/session.ts#L172-L178), [client.ts:45](../src/client.ts#L45)).

Source: [session.ts:40-66](../src/session.ts#L40-L66), [:158-180](../src/session.ts#L158-L180), [daemon.ts:233-340](../src/daemon.ts#L233-L340).

---

## 19. A tool call with approval in the session group

```mermaid
sequenceDiagram
  participant CC as Claude Code
  participant S as Session process
  participant D as Daemon
  participant T as AgentTools
  participant N as Session N
  participant C as Core _hub
  participant O as Owner
  CC->>S: tools/call connect {target}
  S->>D: POST /call {id, tool: connect, input}
  D->>T: call, signal of the request
  T->>N: Actions.create, mode of the session
  N->>O: card in #G: I want to connect to bot X, as myself. /'approve N' or /'reject N'
  D-->>S: {progress: Action N waits for an approver until TIME.}
  S-->>CC: notifications/progress
  O->>C: /approve N in #G
  C->>N: newChatItems: Control.onMessage
  N->>T: perform: performConnect
  T->>C: apiSetContactCustomData @C {simplexMcpOwner: {session: ID}}
  N->>O: card edited: I connected to bot X, as myself. Approved by Alice.
  D-->>S: {result}
  S-->>CC: the result
```

Notes:

- The approver check of the session group counts only invited members whose member in the Main group is an owner ([hub.ts:311-314](../src/hub.ts#L311-L314)).
- Without an approver in `#G`, the action is refused with the text of [API 4.4](api.md#44-tool-results-in-the-plugin).
- The connected chat gets the session's ID as its owner ([tools.ts:587-591](../src/tools.ts#L587-L591), [:845](../src/tools.ts#L845)).

Source: [daemon.ts:342-361](../src/daemon.ts#L342-L361), [hub.ts:75-106](../src/hub.ts#L75-L106), [tools.ts:742-765](../src/tools.ts#L742-L765).

---

## 20. A prompt from the session group

```mermaid
sequenceDiagram
  participant O as Owner
  participant C as Core _hub
  participant N as Session N
  participant D as Daemon
  participant S as Session process
  participant CC as Claude Code
  O->>C: message in #G, with a file
  C->>N: newChatItems: Session.onMessage
  N->>N: not a command of the group, sender an approver
  N->>N: delivery = delivery then deliver: after the earlier prompts of the session
  opt no attachment, in the first 30 s after the hub opened
    N->>N: wait for an attachment until then
  end
  opt attached, with a file
    N->>C: apiReceiveFile FILE_ID ROOT/_sessions/ID/files/group/FILE_ID.EXT, relays approved
    alt rcvFileComplete within 10 minutes
      C->>N: rcvFileComplete: fileDone
    else rcvFileError, a failed receive, or 10 minutes
      N->>O: reply: The file of this message was not received: ERROR
    end
  end
  alt no attachment now
    N->>O: reply: Session N is not running.
  else attached
    N->>N: pushed += (text, item), at most 20
    N->>D: onLine {prompt: {text, from, item, file?}}
    D->>S: the line on /attach
    S->>CC: notifications/claude/channel {content, meta: {from, item, file?}}
  end
```

Notes:

- Messages of members who are not approvers, and one-line `/` commands with a keyword of the group's commands, are not prompts; an approver's other one-line `/` text, such as `/model sonnet`, is a prompt ([hub.ts:160](../src/hub.ts#L160), [control.ts:19](../src/control.ts#L19), [:210](../src/control.ts#L210), [:627-630](../src/control.ts#L627-L630)).
- Prompts reach the session in arrival order: a prompt waits for the file of an earlier prompt ([hub.ts:161](../src/hub.ts#L161)).
- After a daemon start, a prompt that arrives before its session attaches again waits up to 30 s from the hub's open, so a prompt sent while the daemon was stopped reaches the session ([hub.ts:254-256](../src/hub.ts#L254-L256), [:373](../src/hub.ts#L373)).
- A session started without the channel flag gets the notification and drops it.

Source: [hub.ts:156-162](../src/hub.ts#L156-L162), [:254-278](../src/hub.ts#L254-L278), [daemon.ts:330](../src/daemon.ts#L330), [session.ts:53-56](../src/session.ts#L53-L56), [:192-196](../src/session.ts#L192-L196).

---

## 21. A terminal prompt and the answer

```mermaid
sequenceDiagram
  participant P as Person
  participant CC as Claude Code
  participant HK as Turn hook
  participant D as Daemon
  participant N as Session N
  participant U as User
  participant C as Core _hub
  participant O as Owner
  P->>CC: prompt typed in the terminal
  CC->>HK: UserPromptSubmit {session_id, prompt}, in the background
  HK->>D: ensureDaemon, POST /turn {id, pid, at, prompt}
  D->>N: turn
  N->>N: end pending permissions created before at
  N->>N: no pushed prompt matched: turns += (at, null)
  N->>U: post(N, parts), unless a "/" command
  U->>U: the newest group marked N where User is a current member
  U->>O: the prompt in #G, from User
  CC->>CC: turn ends
  CC->>HK: Stop {session_id, last_assistant_message, effort}
  HK->>D: ensureDaemon, POST /turn {id, pid, at, answer, effort}
  D->>N: turn
  N->>N: end pending permissions created before at
  N->>N: quote = the newest turn record before at; it and the older ones leave turns
  N->>N: a changed effort: description, session.json
  N->>C: apiSendMessages #G parts, the first quoting the quote when it came from the group
  opt the core refuses the quote
    N->>C: apiSendMessages #G parts without the quote
  end
  C->>O: the answer, from Claude
  N->>C: apiUpdateGroupProfile #G shortDescr MODEL · effort LEVEL, when it changed
```

Notes:

- `at` is `performance.timeOrigin` of the hook process. An answer quotes the newest prompt record whose hook started before its own, also when the request of the next prompt arrives first, or while the answer's request waits for the short description ([hook.ts:59](../src/hook.ts#L59), [hub.ts:182-188](../src/hub.ts#L182-L188), [:280-289](../src/hub.ts#L280-L289)).
- A prompt from the group is not posted again: the pushed prompts in `<channel source="plugin:simplex:simplex" ... item="I" ...>` tags of the prompt with their item, or equal to the raw prompt and pushed in the last 60 s, leave the pushed prompts, and the oldest of them is the item of the turn record. Without a turn record before the answer, the answer quotes the oldest pushed prompt not quoted yet and marks it; it stays pushed, so that a prompt request handled after the Stop request still matches it and `User` does not post it ([hub.ts:24-28](../src/hub.ts#L24-L28), [:186-188](../src/hub.ts#L186-L188), [:282-286](../src/hub.ts#L282-L286), [:305-309](../src/hub.ts#L305-L309)).
- An empty answer takes its turn record too, without a post ([hub.ts:184](../src/hub.ts#L184), [:196](../src/hub.ts#L196)).
- A prompt request whose hook started before an answer already handled records no turn, so that a later answer does not quote its prompt ([hub.ts:185](../src/hub.ts#L185), [:187](../src/hub.ts#L187)).
- A blank prompt, as Claude Code sends for a prompt with images only, a prompt made only of this plugin's channel tags, as after a daemon restart lost the pushed prompts, and a prompt that is a one-line `/` command, such as `/simplex:link`, are not posted ([hub.ts:195](../src/hub.ts#L195)).
- An answer whose quote the core refuses with `invalidQuote` or `chatItemNotFound`, as for a deleted prompt, is posted without the quote; so are the replies `Session N is not running.` and `The file of this message was not received: ERROR` ([hub.ts:297-303](../src/hub.ts#L297-L303), [:552-555](../src/hub.ts#L552-L555)).
- The hook cuts a prompt or an answer over 250,000 characters there, ending with `…`, so that the request fits the socket's 1 MiB ([hook.ts:15](../src/hook.ts#L15), [:80-82](../src/hook.ts#L80-L82)).
- A card of a tool use that started after the hook is kept: the hook's start time `at` is earlier than its creation ([hook.ts:59](../src/hook.ts#L59), [hub.ts:203-207](../src/hub.ts#L203-L207)).
- PostModelSwitch sends `{id, model}`; only the short description changes ([hook.ts:63](../src/hook.ts#L63), [hub.ts:189-200](../src/hub.ts#L189-L200)).
- A Stop request with a changed effort posts the answer before it updates the short description, so that a prompt request handled meanwhile posts after the answer; the description is updated also when a post fails ([hub.ts:194-200](../src/hub.ts#L194-L200)).
- A turn hook starts the daemon when none runs, repeats a turn refused with 409 up to 15 times 1 s apart, past the 10 s a held title waits, prints `{}`, and logs errors on its stderr ([hook.ts:59-78](../src/hook.ts#L59-L78)).
- `User` keeps its copy of a group that Claude deleted, as for a session closed for 7 days, with the status `deleted` and the same session number; it posts in the newest group of the session where it is a current member, so the prompts of a resumed session reach its new group ([user.ts:30-33](../src/user.ts#L30-L33), [tools.ts:1553-1568](../src/tools.ts#L1553-L1568)).

Source: [hooks.json:5-7](../hooks/hooks.json#L5-L7), [hub.ts:182-201](../src/hub.ts#L182-L201), [:280-303](../src/hub.ts#L280-L303), [:535-555](../src/hub.ts#L535-L555), [user.ts:30-33](../src/user.ts#L30-L33).

---

## 22. A tool use approved in the session group

```mermaid
sequenceDiagram
  participant CC as Claude Code
  participant S as Session process
  participant D as Daemon
  participant N as Session N
  participant O as Owner
  CC->>S: notifications/claude/channel/permission_request {request_id, tool_name, description, input_preview}
  CC->>CC: the terminal dialog stays open
  S->>D: POST /permission {id, requestId, toolName, description, inputPreview}
  D->>N: permission
  N->>O: card in #G: I want to use `Bash`: `DESCRIPTION`, preview, /'approve N' or /'reject N'
  O->>N: /approve N
  N->>N: performing
  N->>D: onLine {permission: {requestId, behavior: allow}}
  D->>S: the line on /attach
  S->>CC: notifications/claude/channel/permission {request_id, behavior: allow}
  N->>N: done, removed
  N->>O: card edited: ... Approved by Alice.
```

Notes:

- `/reject N` pushes `deny`; the card ends with `Rejected by NAME.` ([hub.ts:174-175](../src/hub.ts#L174-L175)).
- The first answer applies, in the terminal or in the group. A card still pending at a prompt or an answer whose hook started after the card, or when the last attachment ends, ends with `Ended.` ([hub.ts:183](../src/hub.ts#L183), [:203-207](../src/hub.ts#L203-L207), [daemon.ts:337](../src/daemon.ts#L337), [control.ts:648-649](../src/control.ts#L648-L649)).
- The input preview is cut to fit the card ([control.ts:136](../src/control.ts#L136), [:736-750](../src/control.ts#L736-L750)).
- The card is posted in mode `requests` also for a session in mode `host` ([hub.ts:172](../src/hub.ts#L172)).

Source: [session.ts:58-61](../src/session.ts#L58-L61), [:111-114](../src/session.ts#L111-L114), [daemon.ts:174-178](../src/daemon.ts#L174-L178), [hub.ts:171-179](../src/hub.ts#L171-L179), [control.ts:656-659](../src/control.ts#L656-L659).

---

## 23. Mode `host` for a session

```mermaid
sequenceDiagram
  participant S as Session process
  participant D as Daemon
  participant N as Session N
  participant O as Owner
  S->>D: POST /session {config: {approve: host}}
  D->>N: requestMode(host): requested = host
  alt the group has no approver
    O->>N: joins #G: membership event, Control.onEvent, askMode
  end
  N->>O: card in #G: I want to run in mode host ... /'approve N' or /'reject N'
  O->>N: /approve N
  N->>N: perform: setMode(host), mode host in the custom data of #G
  N->>O: card edited: I run in mode host ... Approved by Alice.
  S->>D: POST /session {config: {approve: requests}}
  D->>N: requestMode(requests): a pending mode card ends with Ended.; at once, mode requests in the custom data of #G
```

Notes:

- One `mode` card is pending at a time; a rejected or expired card forgets the request until the next `/session` ([hub.ts:143-154](../src/hub.ts#L143-L154)).
- A rejected card sets `hostDeclined` in `session.json`: later `/session` requests of `host` post no card, also after a daemon restart, until a `/session` with `requests` clears it ([hub.ts:76-80](../src/hub.ts#L76-L80), [:139](../src/hub.ts#L139), [:144](../src/hub.ts#L144), [:148](../src/hub.ts#L148), [:321-324](../src/hub.ts#L321-L324)).
- The approved mode is read from the custom data of `#G` when the session opens, `requests` without one; `session.json` holds no mode ([hub.ts:81](../src/hub.ts#L81), [:316-319](../src/hub.ts#L316-L319)).
- A pending `mode` card does not keep the session open ([hub.ts:122-124](../src/hub.ts#L122-L124)).
- The session process marks the action tools for Claude Code's confirmation from the option at its own start, before the owner decides ([session.ts:57](../src/session.ts#L57)).

Source: [hub.ts:135-154](../src/hub.ts#L135-L154), [:316-330](../src/hub.ts#L316-L330), [:526](../src/hub.ts#L526), [tools.ts:569-570](../src/tools.ts#L569-L570).

---

## 24. `/give`

```mermaid
sequenceDiagram
  participant O as Owner
  participant K as Control of session 1
  participant T as AgentTools of session 1
  participant C as Core _hub
  O->>K: /give @4 2 in the group of session 1
  K->>T: agentChat @4: a chat of session 1
  K->>K: session 2 has an ID
  K->>T: giveChat @4 ID2
  T->>C: apiSetContactCustomData @4 {simplexMcpOwner: {session: ID2}}
  K->>K: pending actions of @4 with a card rejected by Alice
  K->>O: Gave bot X (@4) to session 2.
```

Source: [control.ts:359-371](../src/control.ts#L359-L371), [tools.ts:574-577](../src/tools.ts#L574-L577), [hub.ts:423-425](../src/hub.ts#L423-L425).

---

## 25. `reset-owners`

```mermaid
sequenceDiagram
  participant P as Person
  participant CC as Claude Code
  participant D as Daemon
  participant M as MainGroup
  participant N as Sessions
  participant O as Owner
  P->>CC: /simplex:reset-owners
  CC->>D: node dist/cli.js reset-owners: POST /reset-owners
  alt owners exist
    D->>M: askReset
    M->>O: A terminal asks to replace the owners ... /'approve reset' or /'reject reset'
    D-->>CC: An owner approves the reset in "SimpleX MCP". Then run npx -y @simplex-chat/mcp@VERSION link ...
    O->>M: /approve reset
    M->>O: card edited: ... Approved by Alice.
    M->>M: the group deleted for all members, a new Main group with a link
    M->>N: applyMembers: each invited member removed from each session group, one per request
    P->>D: npx -y @simplex-chat/mcp@VERSION link in a terminal
    D-->>P: Join "SimpleX MCP" in the SimpleX app. The link works once:, QR code, link
  else no owners, from Claude Code
    CC-->>P: Run npx -y @simplex-chat/mcp@VERSION reset-owners in a terminal.
  end
```

Notes:

- VERSION is the plugin's version, so that the command runs the daemon's version ([owner.ts:7-8](../src/owner.ts#L7-L8)).

- Without owners, `reset-owners` in a terminal replaces the group at once and prints the new link with a QR code ([owner.ts:28-35](../src/owner.ts#L28-L35), [hub.ts:397-401](../src/hub.ts#L397-L401)).
- One reset card is pending at a time; `/reject reset` leaves the group ([main-group.ts:105-109](../src/main-group.ts#L105-L109), [:132-142](../src/main-group.ts#L132-L142)).

Source: [main-group.ts:111-123](../src/main-group.ts#L111-L123), [hub.ts:222-246](../src/hub.ts#L222-L246), [:451-453](../src/hub.ts#L451-L453).

---

## 26. Idle close, idle stop and a newer version

```mermaid
sequenceDiagram
  participant S as Session process
  participant D as Daemon
  participant N as Session N
  participant S2 as Session process of a newer plugin
  S->>D: the /attach stream ends with Claude Code
  D->>N: onLine = undefined, endPermissions
  D->>D: idle timer, 10 min
  D->>N: no call, no action in memory: entry closed, open false, closed time in session.json
  D->>D: no entry: stop timer, 10 min
  D->>D: stop: actions cancelled, cards edited, cores closed, lock released
  S2->>D: GET /status: version lower than its own
  S2->>D: POST /stop
  S2->>D: spawn a daemon of its version
```

Notes:

- A call or an attachment cancels the stop timer by creating an entry ([daemon.ts:374-384](../src/daemon.ts#L374-L384)).
- A daemon of a higher version is kept; another protocol version makes clients fail with `Restart Claude Code to update the SimpleX plugin.` ([client.ts:35-47](../src/client.ts#L35-L47)).

Source: [daemon.ts:208-231](../src/daemon.ts#L208-L231), [:331-339](../src/daemon.ts#L331-L339), [:426-445](../src/daemon.ts#L426-L445), [hub.ts:466-476](../src/hub.ts#L466-L476).

---

## 27. `/clear`, `--continue` and `/resume` in one session

Claude Code keeps the session server's `CLAUDE_CODE_SESSION_ID` from its spawn, gives the hooks a new `session_id` after `/clear` and `/resume`, and with `--continue` may spawn the server with a startup ID other than the resumed one. The hooks send `CLAUDE_PID`, and the session server its parent process, the same Claude Code process.

```mermaid
sequenceDiagram
  participant CC as Claude Code, process P
  participant S as Session process
  participant HK as Hooks
  participant D as Daemon
  participant N as Session of A
  Note over CC,N: /clear in session A, the server attached as A
  CC->>HK: SessionStart {session_id: B, source: clear}, CLAUDE_PID P
  HK->>D: POST /title {id: B, pid: P}
  D->>D: B unknown; entry A has pid P and is attached: alias B to A, in A's session.json
  D->>N: rename, model
  HK->>D: POST /turn {id: B, pid: P, answer}
  D->>N: turn, posted in A's group
  Note over CC,N: claude --continue: the server spawned with startup ID X, the resumed session R
  CC->>S: CLAUDE_CODE_SESSION_ID X
  S->>D: POST /session {id: X, pid: P}
  D->>D: X unknown, no folder: wait up to 10 s for a /title of P
  CC->>HK: SessionStart {session_id: R, source: resume}, CLAUDE_PID P
  HK->>D: POST /title {id: R, pid: P}
  D->>D: the session of R opened, the title of P recorded
  D->>D: the wait of X ends: alias X to R
  D-->>S: {number of R}
  S->>D: POST /attach {id: X}: the entry of R
  Note over CC,N: /resume R2 in session A, R2 with its own session.json
  CC->>HK: SessionStart {session_id: R2, source: resume}, CLAUDE_PID P
  HK->>D: POST /title {id: R2, pid: P}
  D->>D: R2 has session.json; entry A has pid P and is attached: A, without an alias
  HK->>D: POST /turn {id: R2, pid: P, answer}
  D->>N: turn, posted in A's group
  Note over CC,N: later, claude --resume R2 in process Q
  S->>D: POST /session {id: R2, pid: Q}
  D-->>S: {number of R2}: its own group and chats
  Note over CC,N: the title of a new ID F before the server of P registers, as with --fork-session or /clear during a re-attach
  HK->>D: POST /title {id: F, pid: P}
  D->>D: F unknown, no session of P: title of P = F; the title is held
  D-->>HK: the reply
  S->>D: POST /session {id: A, pid: P}
  D->>D: A has session.json: A; the held title resumes: entry A has pid P: alias F to A
  D->>N: rename, model
```

Notes:

- A session server's process is followed while it is attached and for 10 s after it detached, and a title's process for 10 s; a new Claude Code process with a reused PID later starts its own session ([daemon.ts:299-304](../src/daemon.ts#L299-L304)).
- Only an ID without `session.json` gets an alias. A hook of an ID with `session.json` goes to the session attached from its process, in memory, else to its own session; a session server with such an ID registers it ([daemon.ts:284-293](../src/daemon.ts#L284-L293)).
- A title of a new ID from a process without a known session is answered at once and applied after the session server of that process registers, or after 10 s without one; then it opens a session of its own. So `/clear` during the re-attach of a session server after a daemon start, and `--fork-session` in either order, join the new ID to the session the server registers ([daemon.ts:233-278](../src/daemon.ts#L233-L278), [:489-499](../src/daemon.ts#L489-L499)).
- A session created for an ID records the aliases that already point to it in its first `session.json` ([daemon.ts:415-416](../src/daemon.ts#L415-L416)).
- Turn hooks of an ID whose title is held get 409 and repeat up to 15 times 1 s apart, past the 10 s ([hook.ts:13](../src/hook.ts#L13)).
- A second title of a process whose title is held is held too, so that two `/clear` within the wait join both new IDs to the session the server registers. A daemon stop saves the held titles in `ROOT/titles.json`; the next start takes them as new titles ([daemon.ts:112](../src/daemon.ts#L112), [:211](../src/daemon.ts#L211), [:251-261](../src/daemon.ts#L251-L261)).
- Aliases are read from `session.json` at the daemon's start, so later hooks and calls with B or X reach the same session ([daemon.ts:89-96](../src/daemon.ts#L89-L96), [:306-311](../src/daemon.ts#L306-L311)).
- Without `CLAUDE_PID`, before Claude Code 2.1.214, a new session server waits the 10 s and then registers its own ID.

Source: [hook.ts:50](../src/hook.ts#L50), [:63](../src/hook.ts#L63), [:84-87](../src/hook.ts#L84-L87), [session.ts:53](../src/session.ts#L53), [daemon.ts:233-311](../src/daemon.ts#L233-L311).

---

## 28. A member leaves, is removed or demoted in the Main group

```mermaid
sequenceDiagram
  participant B as Owner Bob
  participant C as Core _hub
  participant M as MainGroup
  participant N as Sessions
  B->>C: removes Alice from #M, or Alice leaves #M
  C->>M: deletedMember or leftMember in #M
  M->>C: apiListMembers #M: Alice removed or left
  M->>N: applyMembers
  N->>C: apiListMembers #G, for each session group
  N->>C: apiRemoveMembers #G [Alice's member], in a request of its own
  N->>C: apiSetGroupCustomData #G, invited without Alice
  B->>C: sets Carol's role in #M from owner to member
  C->>M: memberRole in #M
  M->>N: applyMembers
  alt Carol joined #G
    N->>C: apiSetMembersRole #G [Carol's member] member
  else Carol's invitation to #G is pending
    N->>C: apiAddMemberViaGroup #G via #M, Carol, member: the invitation sent again with the role
  end
```

Notes:

- The same check runs when the group of a stored session opens, as at the daemon's start, so changes made while the daemon was stopped apply then, and after the Main group is replaced, which removes every invited member ([flow 25](#25-reset-owners); [hub.ts:419](../src/hub.ts#L419), [main-group.ts:119](../src/main-group.ts#L119)).
- It runs in the queue of the Main group's events, before a new link or the invitations of new owners ([main-group.ts:144-158](../src/main-group.ts#L144-L158)).
- An entry whose member in `#G` is gone already is dropped without a request. A failed request is logged as `members of session N`, and the entry is kept for the next check ([hub.ts:233-244](../src/hub.ts#L233-L244)).
- Each removal is a request of its own: the core refuses to remove several members in one request when one of them is an admin or an owner.
- Approvals count only from current owners of the Main group also before this check ([hub.ts:311-314](../src/hub.ts#L311-L314)).

Source: [main-group.ts:61-63](../src/main-group.ts#L61-L63), [:144-158](../src/main-group.ts#L144-L158), [hub.ts:222-246](../src/hub.ts#L222-L246), [:403-421](../src/hub.ts#L403-L421), [:451-453](../src/hub.ts#L451-L453), [tools.ts:1548-1551](../src/tools.ts#L1548-L1551).

---

## 29. `reply` while members who are not owners are in the group

```mermaid
sequenceDiagram
  participant CC as Claude Code
  participant T as AgentTools
  participant N as Session N
  participant C as Core _hub
  participant O as Owner
  CC->>T: reply {file, text}
  T->>T: the file checked
  T->>N: ownersOnly: userContact, Control.onlyApprovers
  N->>C: apiListMembers #G
  alt only Claude, User, gone members and approvers
    T->>C: apiSendMessages #G with the file
    T-->>CC: {item}
  else a member who is not an owner, also one with a pending invitation
    T->>N: Actions.create reply, mode requests
    N->>O: text card in #G: I want to send a file to group N · NAME, where members who are not owners see it: `NAME`, SIZE /'approve N' or /'reject N'
    O->>N: /approve N
    N->>T: perform: apiSendMessages #G with the file, its caption and its preview
    N->>O: card edited: I sent a file ... Approved by Alice.
    T-->>CC: {action, status: done, item}
  end
```

Notes:

- The card is posted in mode `requests` also for a session in mode `host`: `reply` is not an action tool, so Claude Code does not ask before it ([tools.ts:729](../src/tools.ts#L729), [:485](../src/tools.ts#L485)).
- A call that ends without a result attaches to the same action when it is repeated with the same file and text ([tools.ts:722-724](../src/tools.ts#L722-L724)).
- `/reject N` returns `{action, status: rejected}`, an expired card `{action, status: expired}`.
- The card holds the file's name and size, not its caption, path or preview, so members who are not owners see only these before the approval ([control.ts:505-511](../src/control.ts#L505-L511), [:675-678](../src/control.ts#L675-L678)).

Source: [tools.ts:719-740](../src/tools.ts#L719-L740), [:562-564](../src/tools.ts#L562-L564), [hub.ts:100](../src/hub.ts#L100), [control.ts:149-153](../src/control.ts#L149-L153), [:675-678](../src/control.ts#L675-L678).

---

## 30. The group of a session closed for 7 days, and a run that is not interactive

```mermaid
sequenceDiagram
  participant CC as Claude Code
  participant HK as Hooks
  participant S as Session process
  participant D as Daemon
  participant N as Session N
  participant C as Core _hub
  D->>D: idle close of session N: closed = now in session.json
  D->>D: start: closed = now for each stored session without it
  D->>D: at the start and every hour: deleteClosedGroups
  D->>N: Hub.deleteGroup(N): closed for 7 days, no entry, not being deleted
  N->>C: standing approvals of the session's chats removed
  N->>C: deleteGroup #G for all members
  D->>D: session.json without group and closed, a log line; a failure logged, the next session deleted
  Note over CC,C: a claude --resume of session N, also while its group is deleted
  D->>D: openAgent: the running deletion of N awaited, then session.json read
  D->>N: Hub.session: a new group, the owners invited
  Note over CC,S: claude -p: CLAUDE_CODE_ENTRYPOINT sdk-cli; a run nobody attends: CLAUDE_CODE_SESSION_ATTENDED 0
  CC->>HK: SessionStart, UserPromptSubmit, Stop
  HK-->>CC: {}, without a request
  CC->>S: tools/call
  S-->>CC: This Claude Code run is not interactive, so it has no SimpleX group ...
```

Notes:

- The session keeps its number and chats; its messages need approval again. The opening of a session removes its closing time ([daemon.ts:418-422](../src/daemon.ts#L418-L422)).
- The hub opens only the stored sessions with a group, so `/sessions` no longer lists a session whose group was deleted ([daemon.ts:134-137](../src/daemon.ts#L134-L137), [hub.ts:455-460](../src/hub.ts#L455-L460)).
- A resume that arrives while the daemon deletes the session's group waits for the deletion, so the session gets a new group and invitations, and `session.json` keeps the new group ([daemon.ts:143-146](../src/daemon.ts#L143-L146), [:410](../src/daemon.ts#L410)).
- A failed deletion is logged as `group of session N`, and the deletions of the other sessions go on; the failed one keeps its group and is tried again at the next hourly check ([daemon.ts:143](../src/daemon.ts#L143)).
- Claude Code sets `CLAUDE_CODE_ENTRYPOINT` for its hooks and its MCP servers: `sdk-cli` for `claude -p`, `cli` for an interactive session; the Agent SDK sets `sdk-ts` or `sdk-py`. An entrypoint already in the environment is kept, except `cli`, so a `claude -p` run started by a command of a VS Code or desktop-app session keeps `claude-vscode` or `claude-desktop`.
- Claude Code also sets `CLAUDE_CODE_SESSION_ATTENDED`, `1` or `0`, for its hooks, `0` for a run nobody attends, such as that `claude -p` run. Its MCP servers get the variable only from the environment of the command that started Claude Code, the value of another session; a `1` there leaves the decision to the entrypoint. Claude Code 2.1.319, checked with a `claude -p` run from the Bash tool of a session: the hooks got `0`, the MCP server the parent's `1`.
- The session process of such a run sees `claude-vscode` and its parent's `1`, so it registers: its hooks send no title, and its `/session` creates a session and a group after 10 s.

Source: [daemon.ts:23](../src/daemon.ts#L23), [:92-95](../src/daemon.ts#L92-L95), [:113](../src/daemon.ts#L113), [:120](../src/daemon.ts#L120), [:139-160](../src/daemon.ts#L139-L160), [:434](../src/daemon.ts#L434), [hub.ts:248-252](../src/hub.ts#L248-L252), [config.ts:22-28](../src/config.ts#L22-L28), [hook.ts:42](../src/hook.ts#L42), [:60](../src/hook.ts#L60), [session.ts:45](../src/session.ts#L45), [:65](../src/session.ts#L65).
