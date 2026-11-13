# SimpleX MCP server: flows

> Sequence diagrams of the main flows of `@simplex-chat/mcp` 0.3.0, each with notes and source locations. `serve` is shown in flows 1-16 and 32, and the Claude Code plugin in flows 17-31.
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

In flows 1-16 and 32, `#G` is the agent control chat, `USER` the agent's user ID, `DATA` the data folder. Calls to the core are shown as the `ChatApi` methods the server calls, with their arguments in short. Card and progress texts with a semicolon are shown with a comma in the diagrams; the exact texts are in [API](api.md).

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
28. [A member joins the Main group and a session's group, and a person is added to it](#28-a-member-joins-the-main-group-and-a-sessions-group-and-a-person-is-added-to-it)
29. [`reply`](#29-reply)
30. [A run that is not interactive](#30-a-run-that-is-not-interactive)
31. [A prompt of Claude Code](#31-a-prompt-of-claude-code)

`serve`, continued:

32. [`/secret` and its use](#32-secret-and-its-use)

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
  S->>C: apiNewGroup USER {My Agent, history on, full deletion on, menu pending, list, leave and secrets}
  S->>C: apiSetGroupCustomData #35;G {"simplexMcp":{"control":true,"nextAction":1}}
  S->>P: Agent control chat "My Agent" created.
  S->>K: load
  K->>C: apiListMembers #35;G
  S->>S: HTTP listens on 127.0.0.1:5226, /mcp answers 503 until the control chat is started
  S->>S: server.json
  K->>C: apiDeleteGroupLink #35;G (groupLinkNotFound ignored)
  K->>C: apiCreateGroupLink #35;G member
  K->>P: join line, QR code, link, QR hint, owner line
  S->>P: Listening on http://127.0.0.1:5226/mcp
  O->>C: joins by the link
  C->>S: joinedGroupMember
  S->>K: onEvent, queued
  K->>C: apiListMembers #35;G
  K->>C: apiDeleteGroupLink #35;G
  K->>C: apiSetMembersRole #35;G [3] owner
  K->>C: apiListMembers #35;G
  K->>O: I'm your agent. I ask here before I connect, send, download a file or leave a chat.
  K->>P: Alice joined (member 3) and is now an owner. The link is deleted.
  K->>P: Add the agent to Claude Code: npx -y @simplex-chat/mcp register my-agent
```

Notes:

- At first use, libsimplex and the Node.js add-on are downloaded, with one `Downloading ...` line each on stderr ([libs.ts:114-135](../../simplex-chat-nodejs/src/libs.ts#L114-L135)).
- With `--data`, `--data <folder>` is appended to the register hint, shell-quoted ([serve.ts:159-161](../src/serve.ts#L159-L161)).
- With `--qr-code small`, the line `If the QR code does not scan, ...` is printed after the link ([control.ts:532](../src/control.ts#L532)).
- Events that arrive before the HTTP listener starts are buffered and handled in order ([core.ts:61-75](../src/core.ts#L61-L75)).
- The server runs `ownerJoined` for every `joinedGroupMember` event of a human without `--owners`, and the link is deleted in that handler ([control.ts:246-247](../src/control.ts#L246-L247), [:491-498](../src/control.ts#L491-L498)).
- With `--secrets-folder`, the folder is created before the user is read, when missing ([serve.ts:90](../src/serve.ts#L90)).

Source: [cli.ts:225-258](../src/cli.ts#L225-L258), [serve.ts:65-124](../src/serve.ts#L65-L124), [core.ts:37-81](../src/core.ts#L37-L81), [files.ts:107-150](../src/files.ts#L107-L150), [http.ts:14-46](../src/http.ts#L14-L46), [control.ts:123-156](../src/control.ts#L123-L156), [:461-465](../src/control.ts#L461-L465), [:526-545](../src/control.ts#L526-L545), [:645-670](../src/control.ts#L645-L670).

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

Source: [cli.ts:137-152](../src/cli.ts#L137-L152), [:234](../src/cli.ts#L234), [register.ts:33-58](../src/register.ts#L33-L58), [files.ts:152-155](../src/files.ts#L152-L155).

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
  X->>K: saveCounter: apiSetGroupCustomData #35;G
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

- The plan is read again in the perform step ([tools.ts:823](../src/tools.ts#L823)).
- Join methods per plan ([tools.ts:854-882](../src/tools.ts#L854-L882), [:1395-1458](../src/tools.ts#L1395-L1458)):

| Plan | Methods |
|---|---|
| known prepared contact or group | `apiConnectPreparedContact(C, incognito, message)`<br>`apiConnectPreparedGroup(G, incognito, message)` |
| channel link (`direct` false, with link data) | `apiPrepareGroup(USER, LINK, false, linkData, domain)`<br>`apiConnectPreparedGroup(G, incognito)` |
| contact address with link data, and a SimpleX name or a message | `apiPrepareContact(USER, LINK, linkData, domain)`<br>`apiConnectPreparedContact(C, incognito, message)`, or `apiConnectPreparedGroup` for a business |
| `contactViaAddress` | `apiConnectContactViaAddress(USER, incognito, CONTACT_ID)` |
| other links | `apiConnectPreparedLink(USER, incognito, LINK)` |

- After a failed prepared connect, the server deletes the prepared chat without notifying ([tools.ts:884-891](../src/tools.ts#L884-L891)).
- The connection is matched by `contactConnected` or `userJoinedGroup`: by contact or group ID, or by the pending connection ID ([tools.ts:663-671](../src/tools.ts#L663-L671), [:893-898](../src/tools.ts#L893-L898)). In the plugin, the pending connection ID is also recorded in the session group's custom data. When the connection completes after the call returned, within a day of the record, its chat becomes the session's ([tools.ts:834](../src/tools.ts#L834), [hub.ts:162-167](../src/hub.ts#L162-L167), [:505-506](../src/hub.ts#L505-L506)).
- In the perform step the server waits for the other side until `startedAt + wait`, at most until `created + 23 h 50 min`, and then reports `connected: false` ([tools.ts:155](../src/tools.ts#L155), [:822](../src/tools.ts#L822), [:848-851](../src/tools.ts#L848-L851)).
- Without `message`, the idle time is counted from the connection; with `message`, from the first reply ([tools.ts:905](../src/tools.ts#L905), [messages.ts:327-335](../src/messages.ts#L327-L335)).
- For a target the agent is connected to, the result is `status: known` with its chat fields, and no action is created ([tools.ts:745](../src/tools.ts#L745)). In the plugin, the chat becomes the session's when it has no owner ([tools.ts:758-763](../src/tools.ts#L758-L763)).
- Exact progress texts: `Action N waits for an approver until TIME.`, then `Action N waits for the connection until TIME.`, then `K messages in CHAT; waiting until TIME.` ([actions.ts:414-425](../src/actions.ts#L414-L425), [tools.ts:791](../src/tools.ts#L791)).

Source: [tools.ts:740-805](../src/tools.ts#L740-L805), [:819-908](../src/tools.ts#L819-L908), [actions.ts:180-215](../src/actions.ts#L180-L215), [:249-267](../src/actions.ts#L249-L267), [control.ts:194-205](../src/control.ts#L194-L205).

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
  A->>T: send {chat: #35;15, text, reply_to, mention: [44], wait, idle, limit}
  T->>C: apiGetChat #35;15 count 0
  T->>C: apiGetChat #35;15 around REPLY_TO count 0
  T->>C: apiListMembers #35;15
  T->>C: apiGetChat #35;15 count 0
  T->>K: standingApprover(chatInfo, messages): none
  T->>X: create(key, send fields)
  X->>K: card size check and approvers
  K->>O: I want to send a message to group X: `text` replying to ... mentioning @Switchback /'approve #35;15 N', /'approve #35;15 all' or /'reject #35;15 N'
  O->>C: /approve #35;15 N
  K->>X: approve(action, "Alice")
  X->>T: performSend
  T->>T: collect #35;15 items with ID above the sent item
  T->>C: apiSendMessages #35;15 [{msgContent, mentions, quotedItemId}]
  C-->>T: sent item S
  D->>C: replies S+1, S+2
  C->>T: newChatItems: collected
  X->>K: outcome
  K->>O: card edited: I sent a message to group X: `text` ... Approved by Alice.
  T->>T: wait until idle seconds after the last reply, or sentAt + wait
  T->>C: apiChatItemsRead #35;15 [S+1, S+2]
  T->>X: remove(action)
  T-->>A: {action: N, status: done, chat: #35;15, item: S, responses, more: 0}
```

Notes:

- Checks before the action: `text` or `file`, the chat connected, role `observer` or `relay` refused, `reply_to` item a message, mentioned members present and the chat a group, and the message length with its quote ([tools.ts:915-926](../src/tools.ts#L915-L926)).
- The server starts collecting before the send and, after it, drops items with IDs up to the sent item ([tools.ts:958](../src/tools.ts#L958), [:971-973](../src/tools.ts#L971-L973)).
- Mentioned names missing from the text are prefixed as `@name` ([tools.ts:1571-1574](../src/tools.ts#L1571-L1574)).
- While a collected message is live, the server waits for its final update; live items are left out of `responses` and counted in `more` ([messages.ts:327-335](../src/messages.ts#L327-L335), [tools.ts:944](../src/tools.ts#L944), [:950](../src/tools.ts#L950)).
- At most 100 replies are kept per action; later ones are counted as overflow ([messages.ts:10](../src/messages.ts#L10), [:109-110](../src/messages.ts#L109-L110)).

Source: [tools.ts:910-979](../src/tools.ts#L910-L979), [messages.ts:99-114](../src/messages.ts#L99-L114), [:327-335](../src/messages.ts#L327-L335), [control.ts:755-761](../src/control.ts#L755-L761).

---

## 5. `receive` with `wait`

```mermaid
sequenceDiagram
  participant A as Agent
  participant T as AgentTools
  participant W as ChatItemsWatch
  participant C as Core
  participant D as Peer
  A->>T: receive {chat: #35;12, wait: 3600, limit: 50}
  T->>W: collect #35;12 into arrived
  T->>C: apiGetChat #35;12 initial 1
  C-->>T: chatStats.unreadCount 0
  T-->>A: progress: No new messages in #35;12, waiting until TIME.
  D->>C: post, item 40
  C->>W: newChatItems: arrived
  T->>C: apiChatItemsRead #35;12 [40]
  T->>W: stop collecting
  T-->>A: {chat: #35;12, messages: [40], more: 0}
```

Notes:

- With unread items the server reads `around` the first unread item, `count` 0, then pages of `after` the last item, `count` 100, until it holds `limit + 1` unread, non-live messages or a page is short ([messages.ts:289-303](../src/messages.ts#L289-L303)).
- Unread items other than messages in the read pages are marked read with the returned messages ([tools.ts:1257](../src/tools.ts#L1257), [:1274](../src/tools.ts#L1274)).
- `more`: the arrived messages not returned, the overflow and the unread items not read ([tools.ts:1276](../src/tools.ts#L1276)).
- Items received while the pages are read are returned once ([tools.ts:1256-1259](../src/tools.ts#L1256-L1259)).
- Exact progress text: `No new messages in CHAT; waiting until TIME.` ([tools.ts:1263](../src/tools.ts#L1263)).

Source: [tools.ts:1245-1280](../src/tools.ts#L1245-L1280), [messages.ts:289-335](../src/messages.ts#L289-L335).

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
  O->>C: /approve #35;9 all (on a pending send card)
  C->>K: newChatItems
  K->>C: apiGetChat #35;9 count 0
  K->>C: apiSetGroupCustomData #35;9 {"simplexMcp":{"approvedBy":"Alice","approvedById":3,"approvedAt":"..."}}
  K->>X: approve each pending send to #35;9 with a card
  K->>O: Approved for business X (#35;9) by Alice. /'revoke #35;9'
  A->>T: send {chat: #35;9, text}
  T->>C: apiGetChat #35;9 count 0
  T->>K: standingApprover: Alice
  T->>X: create(key, fields, "Alice"): performing, standing
  X->>K: saveCounter (no card)
  X->>T: performSend
  T->>C: apiSendMessages #35;9 [...]
  X->>K: outcome
  K->>O: I sent a message to business X: `text` Approved for this chat by Alice. /'revoke #35;9'
  T-->>A: {action, status: done, chat: #35;9, item, responses, more}
  O->>C: /revoke #35;9
  K->>C: apiGetChat #35;9 count 0
  K->>C: apiSetGroupCustomData #35;9 {other keys}
  K->>O: Messages to business X (#35;9) need approval again.
```

Notes:

- The approval is used in mode `requests` while member `approvedById` is an approver ([control.ts:175-178](../src/control.ts#L175-L178)).
- For sends that were pending at `/approve CHAT all`, the card ends with the plain `Approved by Alice.` ([control.ts:721](../src/control.ts#L721), [:730](../src/control.ts#L730)).
- `connect` and `leave` actions are created without the standing approver, so they stay pending ([tools.ts:751](../src/tools.ts#L751), [:1075](../src/tools.ts#L1075)).
- `/receive CHAT N all` and `/receive CHAT off` work the same way for downloads, with the key `simplexMcpFiles`, except that download N alone is performed, and the other pending downloads of the chat stay pending ([control.ts:32-35](../src/control.ts#L32-L35), [:318-320](../src/control.ts#L318-L320), [tools.ts:1107](../src/tools.ts#L1107)).

Source: [control.ts:310-330](../src/control.ts#L310-L330), [:457-459](../src/control.ts#L457-L459), [tools.ts:930-932](../src/tools.ts#L930-L932), [actions.ts:180-215](../src/actions.ts#L180-L215), [:331-349](../src/actions.ts#L331-L349).

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
  A->>T: leave {chat: #35;9}
  T->>X: find(key): none
  T->>C: apiGetChat #35;9 count 0
  T->>X: create(key, leave fields)
  K->>O: I want to leave business X (#35;9). /'approve #35;9 N' or /'reject #35;9 N'
  O->>C: /approve #35;9 N
  K->>X: approve(action, "Alice")
  X->>T: leaveChat(#35;9)
  T->>C: apiGetChat #35;9 count 0
  T->>C: apiLeaveGroup #35;9 (membership current or pending)
  T->>C: apiDeleteChat #35;9 full, notify
  T->>T: DATA/forwarded/group-9 deleted
  X->>K: outcome
  K->>O: card edited: I left business X (#35;9). Approved by Alice.
  K->>O: Files from business X (#35;9) are kept in `DATA/files/group-9`: K files, SIZE. /'delete #35;9 files' (when the folder has files)
  T-->>A: {action: N, status: done, chat: #35;9}
```

Notes:

- For a contact, the card text is `I want to delete KIND NAME (@N).`, and the perform step calls `apiDeleteChat` for the contact, full, with notify ([control.ts:762-765](../src/control.ts#L762-L765), [tools.ts:1083](../src/tools.ts#L1083)).
- The delete notifies, so the core notifies the contact or the members ([tools.ts:1083](../src/tools.ts#L1083), [:1086](../src/tools.ts#L1086)).
- The server looks for an attached action before it reads the chat, so a repeated call after the deletion is attached to the done action ([tools.ts:1070-1072](../src/tools.ts#L1070-L1072)).

Source: [tools.ts:1068-1090](../src/tools.ts#L1068-L1090), [control.ts:198-205](../src/control.ts#L198-L205), [:369-372](../src/control.ts#L369-L372).

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

- New actions are still refused unless the control chat has an approver ([control.ts:164-168](../src/control.ts#L164-L168)).
- In mode `host`, `standingApprover` returns `undefined`, so standing approvals are ignored by `send`, `download`, `list` and `/list`, and `/approve CHAT all` stores nothing; downloads get no card, so `/receive CHAT N all` has no card to act on ([control.ts:177](../src/control.ts#L177), [:270](../src/control.ts#L270)).
- A download in mode `host` is received without the approval of unknown servers, and it gets no card and no outcome post ([tools.ts:618](../src/tools.ts#L618), [:1161](../src/tools.ts#L1161), [control.ts:641-643](../src/control.ts#L641-L643)).
- The kept-files message also follows a done `leave` in mode `host` ([control.ts:203-204](../src/control.ts#L203-L204)).
- The secret commands work as in mode `requests` ([control.ts:282-287](../src/control.ts#L282-L287)).

Source: [tools.ts:548-550](../src/tools.ts#L548-L550), [actions.ts:193](../src/actions.ts#L193), [:347](../src/actions.ts#L347), [control.ts:198-205](../src/control.ts#L198-L205), [:721](../src/control.ts#L721).

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
  Note over X: expires = T + expireHours, timer set for expires - now, at most 60 s
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

- A decided action with an open count of 0 is removed at its timer, at a later decision or at the close of its last call, once its expiry time has passed ([actions.ts:314-316](../src/actions.ts#L314-L316)). A pending action of `serve` that expires without a call stays until the next owner command, or until its check 60 s later. A repeated call before then attaches to it, and its status is `expired`, as in 0.2 ([:307-309](../src/actions.ts#L307-L309), [:243-247](../src/actions.ts#L243-L247)).
- A timer waits at most 60 s, on the monotonic clock. When it fires before the expiry time, it is set again. So an action whose expiry time passes while the computer sleeps expires within 60 s after the computer wakes ([actions.ts:11](../src/actions.ts#L11), [:305](../src/actions.ts#L305)).
- Expiry times are listed in the `/pending` reply ([control.ts:792-794](../src/control.ts#L792-L794)).
- The server ends the connect wait at `created + 23 h 50 min`, within the 24 hours in which a message can be edited ([tools.ts:155](../src/tools.ts#L155), [:822](../src/tools.ts#L822)).

Source: [actions.ts:190](../src/actions.ts#L190), [:243-247](../src/actions.ts#L243-L247), [:297-316](../src/actions.ts#L297-L316), [control.ts:258](../src/control.ts#L258), [:294](../src/control.ts#L294).

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

- The key is made of the session number, `undefined` for `serve`, the tool name and the action fields ([tools.ts:741](../src/tools.ts#L741), [:912](../src/tools.ts#L912), [:1070](../src/tools.ts#L1070), [:1094](../src/tools.ts#L1094), [:1714-1716](../src/tools.ts#L1714-L1716)):
  - `connect`: `target`, `incognito`, `message`
  - `send`: `chat`, `text`, `file`, `as`, `reply_to`, `mention`
  - `download`: `chat`, `item`
  - `leave`: `chat`
- The `wait`, `idle` and `limit` of the attaching call are used ([tools.ts:938](../src/tools.ts#L938)).
- For a call aborted before `create`, the server returns before it creates an action ([tools.ts:750](../src/tools.ts#L750), [:931](../src/tools.ts#L931), [:1074](../src/tools.ts#L1074), [:1108](../src/tools.ts#L1108)).
- Abort path in the SDK: on `res` close the web request is aborted ([node index.cjs:299-303](../node_modules/@modelcontextprotocol/node/dist/index.cjs#L299-L303)); the per-request transport is closed ([server index.cjs:86-90](../node_modules/@modelcontextprotocol/server/dist/index.cjs#L86-L90)); the handler signal is aborted ([src-BhnGYybN.cjs:6425](../node_modules/@modelcontextprotocol/server/dist/src-BhnGYybN.cjs#L6425)).

Source: [tools.ts:769-781](../src/tools.ts#L769-L781), [:935-953](../src/tools.ts#L935-L953), [:1288](../src/tools.ts#L1288), [actions.ts:249-261](../src/actions.ts#L249-L261), [messages.ts:311-325](../src/messages.ts#L311-L325).

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

- The interrupted card line is `DECIDER; interrupted: the server stopped.` ([control.ts:728](../src/control.ts#L728)).
- An action of mode `host` or of a standing approval whose counter is being written at the stop is set to `pending` and cancelled without a perform ([actions.ts:331-336](../src/actions.ts#L331-L336)).
- After a crash, the cards of that run are left with their commands; `/approve` of their numbers gets the reply `Action N is not pending.` ([control.ts:294](../src/control.ts#L294)).
- The stop signal of the performs is aborted after the actions are marked interrupted: a `connect` waiting for its connection ends, and an interrupted perform logs no line ([actions.ts:290-291](../src/actions.ts#L290-L291), [:346](../src/actions.ts#L346), [tools.ts:848-849](../src/tools.ts#L848-L849)).
- The wait applies only when cards were edited or posted at the stop, a `download` without a card excluded, and `--stop-wait` is above 0 ([serve.ts:151](../src/serve.ts#L151), [:163-177](../src/serve.ts#L163-L177)).

Source: [serve.ts:47-63](../src/serve.ts#L47-L63), [:145-155](../src/serve.ts#L145-L155), [:163-177](../src/serve.ts#L163-L177), [actions.ts:284-316](../src/actions.ts#L284-L316), [http.ts:48-52](../src/http.ts#L48-L52), [control.ts:719-738](../src/control.ts#L719-L738).

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
  K->>C: apiCreateGroupLink #35;G observer
  K->>P: The link adds an observer. Its ID and name are printed when it joins.
  O->>C: joins by the link
  C->>K: joinedGroupMember
  K->>C: apiDeleteGroupLink #35;G
  K->>P: Alice joined (member 3). Restart the server with --owners 3:Alice
  K->>O: Alice joined (member 3). Restart the server with `--owners 3:Alice`
  P->>K: Ctrl-C, then serve my-agent --owners 3:Alice
  K->>C: apiSetMembersRole #35;G [3] owner
  K->>C: apiRemoveMembers #35;G [ID], for each other human
  K->>C: apiDeleteGroupLink #35;G
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

- Until the restart with a list, new actions are refused: `The agent has no approving owner. ...` ([control.ts:166-168](../src/control.ts#L166-L168)).
- An entry must match a human by member ID and display name, folded to one line as the restart line prints it, or the server exits with code 2 ([control.ts:474-478](../src/control.ts#L474-L478), [:935-937](../src/control.ts#L935-L937)).
- With `--reset-owners`, the server deletes the control chat for all members and creates a new one with the same counter; the CLI refuses the option with an `--owners` list ([control.ts:682-694](../src/control.ts#L682-L694), [serve.ts:183](../src/serve.ts#L183), [cli.ts:129-131](../src/cli.ts#L129-L131)).
- When a listed owner leaves and none remains: `... is no longer an owner. No owners remain: restart the server with --reset-owners --owners` ([control.ts:513-518](../src/control.ts#L513-L518)).

Source: [control.ts:467-489](../src/control.ts#L467-L489), [:500-519](../src/control.ts#L500-L519), [:526-536](../src/control.ts#L526-L536), [:935-961](../src/control.ts#L935-L961).

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
  A->>T: download {chat: #35;15, item: 30}
  T->>X: find(key): none
  T->>C: apiGetChat #35;15 count 0
  T->>C: apiGetChat #35;15 around 30 count 0
  T->>C: apiGetChatItemInfo #35;15 30: fileXftpServers
  opt no servers yet
    T-->>A: progress: The sender is uploading the file of item 30 in #35;15, waiting until TIME.
    C->>T: rcvFileDescrReady
    T->>C: apiGetChatItemInfo #35;15 30
  end
  T->>C: apiGetUserServers USER: no unknown servers
  T->>C: apiGetChat #35;15 count 0
  T->>K: standingApprover(chatInfo, files): none
  T->>X: create(key, download fields)
  K->>O: I want to download a file from Switchback in group Trail (#35;15): `report.pdf`, 2.4 MB, Servers: known. /'receive #35;15 N', /'receive #35;15 N all', /'forward #35;15 N' or /'reject #35;15 N'
  O->>C: /receive #35;15 N
  K->>X: approve(action, "Alice")
  X->>T: performDownload: downloadFile
  T->>C: apiGetChat #35;15 around 30 count 0
  T->>C: apiReceiveFile 7 DATA/files/group-15/7.pdf
  C->>T: rcvFileComplete
  T->>T: a file at the download path: resolved
  X->>K: outcome
  K->>O: card edited: I downloaded a file from ... Approved by Alice.
  T-->>A: {action: N, status: done, chat: #35;15, item: 30, path}
```

Notes:

- A check every 60 s reads the item: a deleted message or chat fails the download; 30 minutes without a rise of `rcvProgress` fail and cancel it ([tools.ts:1166-1193](../src/tools.ts#L1166-L1193)).
- A download still running `wait` seconds after the perform starts returns `status: performing`; the same call again attaches to the action ([tools.ts:1139-1142](../src/tools.ts#L1139-L1142)).
- `/approve #15 N` on a download card gets the reply `Action N is a download: ...` with the card's command line ([control.ts:301](../src/control.ts#L301)).
- A file on unknown servers gets a card that names their hosts and leaves out `/'receive CHAT N all'` ([control.ts:766-771](../src/control.ts#L766-L771), [:785-788](../src/control.ts#L785-L788), [:844-848](../src/control.ts#L844-L848)). Only such a card approves the relays; the core checks the complete description of every other download, which fails at the acceptance, or at the next check when the description completes later ([tools.ts:618](../src/tools.ts#L618), [:1174](../src/tools.ts#L1174)).
- The card of an image or video shows the sender's preview ([control.ts:586-606](../src/control.ts#L586-L606)).
- In a plugin session group, the core refuses a card that the group's preferences do not allow ([control.ts:561-584](../src/control.ts#L561-L584), [:796-800](../src/control.ts#L796-L800), [actions.ts:338-349](../src/actions.ts#L338-L349)):
  - While the group's preference does not allow files and media, a card with a preview is posted as text and edited as text.
  - A card refused by another preference before the decision is replaced by `Action N: ` and the text that names the preference. The action then fails.
  - The outcome card of an action performed in mode `host` or under a standing approval is posted after the decision. When it is refused, it is replaced by `Action N was performed; its card was not posted: ` or `Action N failed; its card was not posted: `, and that text.
  - A refused edit of a card is replaced by `Action N OUTCOME; its card was not updated: ` and that text. OUTCOME is `was performed`, `failed`, `was rejected`, `expired` or `ended`.
  - After the decision, the action is not changed by a refused card.
- A file the core receives without a request, a short voice message, also in a member chat of a group, is moved into the chat's folder at `rcvFileComplete`; one whose transfer started before a restart, only when it has its full size. Its message then shows it as `offered`, and `download` returns its path after the approval ([tools.ts:624-644](../src/tools.ts#L624-L644), [:1213-1222](../src/tools.ts#L1213-L1222), [messages.ts:194-208](../src/messages.ts#L194-L208)).

Source: [tools.ts:1092-1234](../src/tools.ts#L1092-L1234), [control.ts:740-790](../src/control.ts#L740-L790), [tools.ts:673-700](../src/tools.ts#L673-L700).

---

## 14. `/forward`

```mermaid
sequenceDiagram
  participant A as Agent
  participant T as AgentTools
  participant K as Control
  participant C as Core
  participant O as Owner
  O->>C: /forward #35;15 N (on a pending download card)
  C->>K: newChatItems
  K->>T: forwardFile(action N)
  T->>C: apiReceiveFile 7 DATA/forwarded/group-15/7.pdf
  C->>T: rcvFileComplete: resolved with the forwarded path
  T->>T: copy for the post: DATA/files/sent/send-XXXXXX/report.pdf
  K->>C: apiSendMessages #35;G [{fileSource: the copy, msgContent: file, quotedItemId: card N when the message fits}]
  C->>T: sndFileCompleteXFTP: the copy's folder deleted
  Note over K,O: the card stays pending#59; the agent's message shows the file as offered
  O->>C: /receive #35;15 N
  K->>T: performDownload: downloadFile
  T->>T: the forwarded file moved to DATA/files/group-15/7.pdf
  T-->>A: {action: N, status: done, chat: #35;15, item: 30, path}
```

Notes:

- `/forward` gets no reply; a download or post that fails is posted as ``Action N: the file was not received: `ERROR`.``, and a failed post deletes the copy ([control.ts:332-351](../src/control.ts#L332-L351)). In a plugin session group, the core may refuse the post with `feature not allowed NAME`, as for `Files and media` or `Voice messages`. Claude then posts `Action N: NAME are off in the group of this session; its owners can turn them on in the group's preferences.`
- The copy is named by the name the sender gave ([tools.ts:1031-1035](../src/tools.ts#L1031-L1035)).
- The post keeps the sender's preview of an image or video; it replies to the card when the message, with the card as its quote, is within 15602 bytes by the length rule of `send` ([control.ts:332-351](../src/control.ts#L332-L351), [tools.ts:1584-1600](../src/tools.ts#L1584-L1600)).
- The download is shared with the agent's: one download per file ID; the move to the download path waits for the copy of the post ([tools.ts:595-622](../src/tools.ts#L595-L622)).
- The forwarded path is outside `DATA/files/`, so `send` refuses the file there; it is deleted when the agent leaves the chat, and by `/delete CHAT files` ([tools.ts:1004-1009](../src/tools.ts#L1004-L1009), [:1042-1060](../src/tools.ts#L1042-L1060), [:1082-1090](../src/tools.ts#L1082-L1090)).

Source: [control.ts:288-306](../src/control.ts#L288-L306), [:332-351](../src/control.ts#L332-L351), [tools.ts:595-612](../src/tools.ts#L595-L612), [:981-991](../src/tools.ts#L981-L991), [:1031-1035](../src/tools.ts#L1031-L1035).

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
  A->>T: send {chat: #35;15, text: "trail map", file: "/home/alice/trail.jpg"}
  T->>C: apiGetChat #35;15 count 0
  T->>T: real path, size, inode, change time
  T->>T: refused paths checked
  T->>T: preview: core.resizeImage(file, MessagePreview)
  T->>T: message length checked
  T->>X: create(key, send fields with the file)
  K->>O: image card with the preview: I want to send a message to group Trail: `trail map`, attaching image `/home/alice/trail.jpg`, SIZE /'approve #35;15 N', ...
  O->>C: /approve #35;15 N
  X->>T: performSend
  T->>T: the file again: same real path, size, inode, change time
  T->>T: core.resizeImage(file, Image): DATA/files/sent/send-XXXXXX/trail.jpg
  T->>C: apiSendMessages #35;15 [{fileSource: the copy, msgContent: image with the preview}]
  C->>T: sndFileCompleteXFTP: the copy's folder deleted
```

Notes:

- A refused file ends the call with an error, and the refusal is posted to the control chat and logged ([tools.ts:995-998](../src/tools.ts#L995-L998), [control.ts:207-211](../src/control.ts#L207-L211)).
- A video's preview is the first frame from `ffmpeg` and its duration comes from `ffprobe`; a GIF or WebP gets the fixed preview ([tools.ts:1672-1706](../src/tools.ts#L1672-L1706)).
- A changed file fails the perform: ``file: PATH changed after the request.`` ([tools.ts:1011-1016](../src/tools.ts#L1011-L1016)).
- `sndFileError` deletes the copy too, and the failure is posted to the control chat and logged ([tools.ts:696-698](../src/tools.ts#L696-L698), [serve.ts:135](../src/serve.ts#L135), [control.ts:213-222](../src/control.ts#L213-L222)).

Source: [tools.ts:910-933](../src/tools.ts#L910-L933), [:955-1029](../src/tools.ts#L955-L1029), [control.ts:586-606](../src/control.ts#L586-L606), [:755-761](../src/control.ts#L755-L761).

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
  O->>C: /leave #35;9
  K->>T: knownChat #35;9
  K->>T: leaveChat #35;9
  T->>C: apiLeaveGroup #35;9, apiDeleteChat #35;9 full, notify
  C->>D: the agent left the group
  T->>T: DATA/forwarded/group-9 deleted
  K->>P: Left business Ridgeline (#35;9), requested by Alice.
  K->>X: reject each pending action of #35;9 with a card
  K->>O: Left business Ridgeline (#35;9), requested by Alice.
  K->>O: Files from business Ridgeline (#35;9) are kept in `DATA/files/group-9`: 3 files, 2.1 MB. /'delete #35;9 files'
  O->>C: /delete #35;9 files
  K->>T: deleteChatFiles #35;9
  T->>T: each download of #35;9: failed, apiCancelFile, its partial file deleted
  T->>C: apiGetChat #35;9 count 100, then before the first item, while a page has 100
  T->>C: apiCancelFile for each other file in rcvAccepted, rcvTransfer or rcvWarning
  T->>T: DATA/files/group-9 and DATA/forwarded/group-9 deleted
  K->>O: Deleted 3 files from #35;9, 2.1 MB.
```

Notes:

- `/leave` acts at once in both modes, and works for a contact as `Deleted bot X (@4), requested by Alice.` ([control.ts:353-367](../src/control.ts#L353-L367)).
- The line `Left ...` goes to the log and to the owners; the members of #9 see only that the agent left.
- The files message is posted after the reply, and only when the folder has files ([control.ts:364-365](../src/control.ts#L364-L365), [:406-408](../src/control.ts#L406-L408)).
- `/delete CHAT files` works for a current or a left chat; the failed downloads return `Your owner deleted the files of this chat.`, and the reply counts the forwarded files too ([tools.ts:1042-1060](../src/tools.ts#L1042-L1060)).
- The paged reads find the downloads the core continues after a restart, which have no download in memory; for a left chat they find none ([messages.ts:275-283](../src/messages.ts#L275-L283), [tools.ts:1049-1055](../src/tools.ts#L1049-L1055)).

Source: [control.ts:353-377](../src/control.ts#L353-L377), [tools.ts:1037-1060](../src/tools.ts#L1037-L1060), [:1082-1090](../src/tools.ts#L1082-L1090).

---

## Claude Code plugin flows

Participants of the plugin flows:

| Letter | Participant |
|---|---|
| CC | Claude Code |
| HK | a hook, `node dist/cli.js hook ...` in the plugin's folder |
| S | the session process, `simplex-mcp session` |
| D | the service, with its socket routes and entries |
| M | `MainGroup` |
| N | the `Session` of session N, with its `Control` and `Actions` |
| T | the session's `AgentTools` |
| C | the core `_hub` with the profile `Claude` |
| U | `User` with the core `_user` |
| O | an owner in the SimpleX app |

`#M` is the Main group, `#G` the group of session N, ROOT the data root.

---

## 17. First session start and the owner joins

```mermaid
sequenceDiagram
  participant CC as Claude Code
  participant HK as SessionStart hook
  participant D as Service
  participant M as MainGroup
  participant N as Session 1
  participant C as Core _hub
  participant U as User
  participant O as Owner
  CC->>HK: {session_id, session_title, cwd, model}
  HK->>D: GET /status: not running
  HK->>D: spawn simplex-mcp service through a launcher, which exits at once
  D->>D: lock, stored sessions, socket listens
  D->>C: openCore ROOT/_hub "Claude"
  D->>C: apiNewGroup "Claude Control", short description and welcome of trust, {simplexMcpMain: true}
  M->>C: apiGetGroupLinkStr #35;M: none, apiCreateGroupLink #35;M member
  D->>U: openCore ROOT/_user "User"
  U->>C: joins the hub's one-time link#59; the contact of its connection marked simplexMcpUser
  D-->>HK: ready
  HK->>D: POST /title {id, pid, at, name, project, model, config}
  D->>D: ROOT/_sessions/ID/session.json {number: 1, name}
  D->>N: Hub.session
  N->>C: apiNewGroup "1 · NAME", {simplexMcpSession: {session: 1}}
  N->>C: apiAddMember #35;G User
  U->>C: apiJoinGroup #35;G, marked {simplexMcpSession: 1}
  N->>C: apiUpdateGroupProfile #35;G shortDescr MODEL
  D-->>HK: {mainGroup, owner: {owners: [], link}}
  HK-->>CC: systemMessage: scan this QR code ..., QR code, link
  O->>C: joins #35;M by the link
  C->>M: joinedGroupMember
  M->>C: apiDeleteGroupLink #35;M, apiSetMembersRole #35;M owner
  M->>O: I'm Claude. Each Claude Code session has its own group ...
  M->>O: Alice joined. They can join the group of any Claude Code session here, prompt Claude and approve its actions.
  M->>N: inviteOwner: apiAddMemberViaGroup #35;G via #35;M, role owner
  O->>C: joins #35;G from the invitation in #35;M
  C->>N: joinedGroupMember
  N->>O: Alice is an owner and can send prompts to Claude and the group's commands, and approve tool use.
```

Notes:

- Only the person gets the link. It is passed in the hook's `systemMessage`, and it is in no hook context or tool result ([hook.ts:94-96](../src/hook.ts#L94-L96)).
- The service is spawned by a launcher, `node -e LAUNCHER COMMAND ARGS`, which exits at once, so the service is no descendant of the hook. When Claude Code exits during the hook, it ends the hook and the hook's descendants, and the service keeps starting. The service drops its ready line when the hook is gone, and the next session start shows the link ([client.ts:14-18](../src/client.ts#L14-L18), [:79](../src/client.ts#L79), [service.ts:507-513](../src/service.ts#L507-L513)).
- The Main group's name is `main_group`, `Claude Control` by default, followed by ` - MACHINE` when `SIMPLEX_MCP_MACHINE` is set ([config.ts:40](../src/config.ts#L40), [:50-57](../src/config.ts#L50-L57)).
- The service deletes the preset contact of a new profile, in `_hub` and in `_user` ([core.ts:28](../src/core.ts#L28), [:44-52](../src/core.ts#L44-L52)).
- `User` connects only at the first start. At later starts, the service finds the hub's contact marked `simplexMcpUser`, the contact whose connection belongs to the invitation that the hub created. When `User` does not connect within 60 s, the service runs without it, and terminal prompts are not posted ([user.ts:21](../src/user.ts#L21), [:38-48](../src/user.ts#L38-L48), [:60-62](../src/user.ts#L60-L62), [hub.ts:376](../src/hub.ts#L376)).
- At a later start without owners, the service keeps the Main group's link ([main-group.ts:68-76](../src/main-group.ts#L68-L76)).
- Owners are invited to the groups of the open sessions when they join the Main group. The groups of closed sessions are joined with `/join N` ([hub.ts:449-453](../src/hub.ts#L449-L453), [:436-447](../src/hub.ts#L436-L447)).
- The trust given to the Main group's members is stated in the group's short description and welcome message, which Claude sets when it creates the group ([main-group.ts:16-18](../src/main-group.ts#L16-L18), [:195-205](../src/main-group.ts#L195-L205)). Claude also states it in a post at each human's `joinedGroupMember` or `connectedToGroupMember` ([main-group.ts:127-128](../src/main-group.ts#L127-L128)). The joiner is found as in [flow 28](#28-a-member-joins-the-main-group-and-a-sessions-group-and-a-person-is-added-to-it).
- Claude changes only the name and the menu of the Main group's profile, and starts each change from the group's current profile. So the short description, the welcome message and the preferences that owners set are kept ([main-group.ts:57-66](../src/main-group.ts#L57-L66), [control.ts:672-680](../src/control.ts#L672-L680)).
- At each join other than `User`'s, Claude posts in the session group the rights of the joiner's role. For a role that can send, other than owner, the post ends with the command that makes the joiner an observer ([hub.ts:221-228](../src/hub.ts#L221-L228), [:519](../src/hub.ts#L519)).
- The service reads the hub's contact of `User` after `User` opens, so that the sessions recognize `User`'s messages ([hub.ts:377](../src/hub.ts#L377)).

Source: [hook.ts:81-100](../src/hook.ts#L81-L100), [client.ts:40-97](../src/client.ts#L40-L97), [service.ts:93-119](../src/service.ts#L93-L119), [:218-262](../src/service.ts#L218-L262), [hub.ts:368-386](../src/hub.ts#L368-L386), [:407-425](../src/hub.ts#L407-L425), [:478-490](../src/hub.ts#L478-L490), [main-group.ts:117-140](../src/main-group.ts#L117-L140), [user.ts:16-57](../src/user.ts#L16-L57).

---

## 18. Session process start and attach

```mermaid
sequenceDiagram
  participant CC as Claude Code
  participant S as Session process
  participant D as Service
  participant N as Session N
  CC->>S: node dist/cli.js session, with CLAUDE_CODE_SESSION_ID
  CC->>S: initialize
  S-->>CC: tools, instructions, capabilities claude/channel and claude/channel/permission
  S->>D: GET /status, version and protocol
  S->>D: POST /session {id, pid: parent process, project, config}
  D->>D: resolve id: alias#59; id with session.json#59; session of pid, or a wait of up to 10 s for the title of pid
  D->>N: configure, requestMode(approve)
  D-->>S: {number, displayName, owner}
  S->>D: POST /attach {id}
  D-->>S: {version, protocol}
  D->>N: onLine = the entry's writers
  D-->>S: the /attach stream ends: the service stopped
  S->>S: after 1 s, then twice as late after each failure, up to 30 s
  S->>D: GET /status, spawn when none runs, POST /session, POST /attach
```

Notes:

- With an invalid option, or in a run that is not interactive, the session process does not connect to the service, even at a permission request of Claude Code. For every call, it returns the option's error. In a run that is not interactive, the text of [flow 30](#30-a-run-that-is-not-interactive) is returned instead, also with an invalid option ([session.ts:44-51](../src/session.ts#L44-L51), [:57](../src/session.ts#L57), [:59](../src/session.ts#L59), [:66](../src/session.ts#L66)).
- For a session without a folder, the service creates the folder at `/session`, and answers `/attach`, `/call`, `/permission` and `/turn` with 409. After a 409 to a call, the session process registers again in `call` and repeats the call once ([service.ts:392-402](../src/service.ts#L392-L402), [session.ts:120-130](../src/session.ts#L120-L130)).
- When the `/attach` stream ends, or a connection or a spawn fails, the session process drops the connection and connects again. So the channel works again without a tool call. A single timer is set for each attempt, so the delay doubles once per failed attempt. The session process stops retrying at another protocol version, the `ProtocolError`, and at a 403 of `/session` ([session.ts:88-112](../src/session.ts#L88-L112), [:177-183](../src/session.ts#L177-L183), [client.ts:50](../src/client.ts#L50)).

Source: [session.ts:40-67](../src/session.ts#L40-L67), [:160-185](../src/session.ts#L160-L185), [service.ts:201-324](../src/service.ts#L201-L324).

---

## 19. A tool call with approval in the session group

```mermaid
sequenceDiagram
  participant CC as Claude Code
  participant S as Session process
  participant D as Service
  participant T as AgentTools
  participant N as Session N
  participant C as Core _hub
  participant O as Owner
  CC->>S: tools/call connect {target}
  S->>D: POST /call {id, tool: connect, input}
  D->>T: call, signal of the request
  T->>N: Actions.create, mode of the session
  N->>O: card in #35;G: I want to connect to bot X, as myself. /'approve N' or /'reject N'
  D-->>S: {progress: Action N waits for an approver until TIME.}
  S-->>CC: notifications/progress
  O->>C: /approve N in #35;G
  C->>N: newChatItems: Control.onMessage
  N->>T: perform: performConnect
  T->>C: apiSetContactCustomData @C {simplexMcpOwner: {session: ID}}
  N->>O: card edited: I connected to bot X, as myself. Approved by Alice.
  D-->>S: {result}
  S-->>CC: the result
```

Notes:

- Approvers in the session group are its active members with the role `admin` or `owner`, other than Claude and `User`, however they joined ([hub.ts:312-318](../src/hub.ts#L312-L318), [control.ts:620-622](../src/control.ts#L620-L622)).
- Without an approver in `#G`, the action is refused with the text of [API 4.4](api.md#44-tool-results-in-the-plugin).
- The session's ID is set as the owner of the connected chat ([tools.ts:589-593](../src/tools.ts#L589-L593), [:843](../src/tools.ts#L843)).

Source: [service.ts:326-345](../src/service.ts#L326-L345), [hub.ts:69-103](../src/hub.ts#L69-L103), [tools.ts:740-763](../src/tools.ts#L740-L763).

---

## 20. A prompt from the session group

```mermaid
sequenceDiagram
  participant O as Member
  participant C as Core _hub
  participant N as Session N
  participant D as Service
  participant S as Session process
  participant CC as Claude Code
  O->>C: message in #35;G, with a file
  C->>N: newChatItems: Session.onMessage
  alt one-line "/" text from a moderator, member or author
    N->>N: delivery = delivery then the reply
    N->>O: reply: Only admins and owners of this group can send / commands.
  else one-line "/" text
    N->>N: Control.onMessage: from User, dropped, else a command of the group, queued
  else another message
    N->>N: delivery = delivery then prompt: after the earlier messages of the session
    alt from User
      N->>N: nothing
    else a prompt
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
    end
  end
```

Notes:

- Every message of a member other than `User` is a prompt, however the member joined, except a one-line `/` text. Claude answers a one-line `/` text of a moderator, member or author with `Only admins and owners of this group can send / commands.` From an admin or owner, a one-line `/` text is a command of the group. For one outside the group's requests, such as `/model sonnet`, the reply is `Unknown command.` ([hub.ts:153-160](../src/hub.ts#L153-L160), [:230-233](../src/hub.ts#L230-L233), [:308-314](../src/hub.ts#L308-L314), [control.ts:260](../src/control.ts#L260), [:712-717](../src/control.ts#L712-L717)).
- The session handles messages in arrival order. So a prompt is delivered after the file of an earlier prompt. The reply to a one-line `/` text of a moderator, member or author is posted after the earlier prompts ([hub.ts:157](../src/hub.ts#L157), [:159](../src/hub.ts#L159), [:304-306](../src/hub.ts#L304-L306)).
- After a service start, a prompt that arrives before its session attaches again is held up to 30 s from the hub's open. So a prompt sent while the service was stopped is still delivered to the session ([hub.ts:235-237](../src/hub.ts#L235-L237), [:379](../src/hub.ts#L379)).
- In a session started without the channel flag, Claude Code receives the notification and drops it.

Source: [hub.ts:153-160](../src/hub.ts#L153-L160), [:230-259](../src/hub.ts#L230-L259), [service.ts:314](../src/service.ts#L314), [session.ts:53-56](../src/session.ts#L53-L56), [:197-201](../src/session.ts#L197-L201).

---

## 21. A terminal prompt and the answer

```mermaid
sequenceDiagram
  participant P as Person
  participant CC as Claude Code
  participant HK as Turn hook
  participant T as Transcript
  participant D as Service
  participant N as Session N
  participant U as User
  participant C as Core _hub
  participant O as Owner
  P->>CC: prompt typed in the terminal
  CC->>HK: UserPromptSubmit {session_id, transcript_path, prompt_id, prompt}, in the background
  CC->>T: the prompt's entry, with origin human, after the synchronous UserPromptSubmit hooks
  loop up to 600 reads, 100 ms apart, until the entry of prompt_id is found
    HK->>T: read the last 1 MiB and twice the prompt's bytes, a missing file as empty
  end
  HK->>D: ensureService, POST /turn {id, pid, at, prompt, typed}
  D->>N: turn
  N->>N: end pending permissions created before at
  N->>N: no pushed prompt matched: turns += (at, null), unless at is below answeredAt
  N->>U: post(N, parts), when typed and not a "/" command
  U->>O: the prompt in #35;G, from User
  CC->>CC: turn ends
  CC->>HK: Stop {session_id, transcript_path, last_assistant_message, effort}
  CC->>T: the answer's entry, within about 100 ms
  loop up to 20 reads, 100 ms apart, until Claude's last entry is the answer
    HK->>T: read the last 1 MiB and twice the answer's bytes
  end
  HK->>D: ensureService, POST /turn {id, pid, at, answer, model, effort}
  D->>N: turn
  N->>N: end pending permissions created before at
  N->>N: quote = the newest turn record before at#59; it and the older ones leave turns
  N->>N: a changed model or effort, when at is not older than that of the value taken last: description, session.json
  N->>C: apiSendMessages #35;G parts, the first quoting the quote when it came from the group and fits with the first character
  opt the core refuses the quote
    N->>C: apiSendMessages #35;G parts without the quote
  end
  C->>O: the answer, from Claude
  N->>C: apiUpdateGroupProfile #35;G shortDescr MODEL · effort LEVEL, when it changed
```

Notes:

- `at` is `performance.timeOrigin` of the hook process, the hook's start time. In an answer, the hub quotes the newest prompt record whose hook started before the answer's hook. This record is quoted also when the request of the next prompt arrives first, and while the answer's request waits for the short description ([hook.ts:114](../src/hook.ts#L114), [hub.ts:180-186](../src/hub.ts#L180-L186), [:267-276](../src/hub.ts#L267-L276)).
- `User` does not post a prompt that came from the group. The hub matches a pushed prompt to the terminal prompt in either case:
  - its item and its text are in a `<channel source="plugin:simplex:simplex" ... item="I" ...>` tag of the prompt;
  - its text is equal to the raw prompt, and it was pushed in the last 60 s.

  Matched prompts are removed from the pushed prompts, and the oldest of them becomes the item of the turn record ([hub.ts:22](../src/hub.ts#L22), [:27](../src/hub.ts#L27), [:184-186](../src/hub.ts#L184-L186), [:298-302](../src/hub.ts#L298-L302)).
- For an answer without an earlier turn record, the hub quotes the oldest pushed prompt not quoted yet, and marks it quoted. That prompt stays pushed, so the hub still matches it to a prompt request handled after the Stop request, and `User` does not post it ([hub.ts:269-273](../src/hub.ts#L269-L273)).
- For an empty answer, the hub removes the turn record as for any other answer, but posts nothing ([hub.ts:182](../src/hub.ts#L182), [:197](../src/hub.ts#L197)).
- No turn record is made for a prompt request whose hook started before an answer that was already handled. So that prompt is not quoted in a later answer ([hub.ts:183](../src/hub.ts#L183), [:185](../src/hub.ts#L185)).
- The UserPromptSubmit hook runs for every prompt of the session that queries the model, also for Claude Code's own prompts. It marks a prompt as typed only when the prompt's record in the transcript has the origin `human`. Task notifications, messages of other sessions and channel prompts have other origins. The record is the newest one whose text is the prompt: a user entry, or a `queued_command` attachment of a prompt that Claude Code added to a running turn. It is the entry with `prompt_id` as `promptId`, or a record after the first entry with that ID, tool results included, so a long turn whose first entry is out of the tail is found by its tool results. While a `queue-operation` `enqueue` entry with the prompt's text follows the record, the hook waits: the prompt is a later one with that text, still queued. Claude Code writes the entry only after the synchronous UserPromptSubmit hooks end, such as a slow hook in the user's settings, and it creates the transcript file with the session's first entry. So the hook reads the transcript up to 600 times, 100 ms apart, and it treats a missing file as empty. Until the entry with `prompt_id` is in the tail, the hook waits for it; at the last read it takes any record with the prompt's text, and without `prompt_id` it does so at once. Claude Code runs the hook of a person's prompt when it is submitted, and of its own prompts only when they are taken. So at the last read, the hook classifies a record still followed by such an `enqueue` entry by the record, and marks a typed one mid-turn: the queued prompt is the typed prompt itself, or a later prompt with the text of Claude Code's own prompt, as a repeated monitor event. When no record is written within the reads, an `enqueue` entry with the prompt's text after the entry with `prompt_id`, or anywhere without that entry, makes the prompt typed and mid-turn. Otherwise the prompt is typed when an entry with `prompt_id` has the origin `human` ([hook.ts:18-19](../src/hook.ts#L18-L19), [:119](../src/hook.ts#L119), [:147-161](../src/hook.ts#L147-L161), [:187-222](../src/hook.ts#L187-L222)).
- Claude Code adds a prompt that arrives during a turn to that turn, and gives its hook the running turn's prompt ID. The hook sends `midTurn: true` for a prompt whose record is not the entry with `prompt_id`. A mid-turn prompt ends no tool-use card. A prompt adds a turn record only when its hook started no earlier than the hook of the last answer handled (`answeredAt`), and a mid-turn prompt only when it also matched a pushed prompt. So the answer of the turn quotes the turn's prompt, or an owner's prompt added to the turn ([hub.ts:181](../src/hub.ts#L181), [:185](../src/hub.ts#L185)).
- The hook sends `note`, a line about one of Claude Code's own prompts, and Claude posts it without a quote ([hook.ts:217-290](../src/hook.ts#L217-L290), [hub.ts:195](../src/hub.ts#L195)). A typed prompt, a channel prompt and a teammate's prompt of idle notifications only have no note, nor has another record without origin that is not scheduled. The line is cut to 200 characters and ends with `…`. A note that a group preference refuses is replaced by `A note on Claude Code's prompt was not posted: ` and the preference's text.

  | Kind | Record | Line |
  |---|---|---|
  | task notification | origin `task-notification`, or a `queued_command` attachment without origin whose `commandMode` is `task-notification` | an emoji, the first line of `<summary>`, and `: ` with the first line of `<event>` when there is one, with entities decoded. The emoji is ✅ for `<status>` completed, ❌ for failed, ⏹️ for stopped or killed, and without a status 📡 with an event or 🔔 without. Without a `<task-notification>` block: 🔔 and the first line |
  | message of another session | origin `peer` | `✉️ Message from NAME: ` and the first line of `body` |
  | message of a teammate | no origin, a `<teammate-message teammate_id="NAME" ...>` tag after `Another Claude session sent a message` | `✉️ Message from NAME: ` and the tag's `summary`, or the first line of the message, from the first tag that is not an idle notification; nothing for idle notifications only |
  | subagent report | origin `peer` with `handback` | `📋 Report from subagent NAME: ` and the first line of `handbackReportText`, or of `body` |
  | scheduled prompt | no origin, `turnOrigin` `scheduled` | `⏰ Scheduled prompt: ` and the first line |
  | automatic prompt | origin `auto-continuation` | `🔄 Automatic prompt: ` and the first line |
  | another kind | another origin | `⚙️ Claude Code: ` and the first line |
- These terminal prompts are not posted ([hub.ts:196](../src/hub.ts#L196)):
  - a prompt with `typed: false`, such as a notification of a background task. A request without `typed`, from an older plugin, is posted as typed;
  - a blank prompt, which Claude Code sends for a prompt with images only;
  - a prompt made only of this plugin's channel tags, as after the pushed prompts were lost in a service restart;
  - a one-line `/` command, such as `/compact`.
- An answer and a terminal prompt are posted in parts. Each part is the longest run of whole characters that fits in one message, as `send` measures it, and the first part of an answer is measured with its quote. The hub searches each part among the next 15602 characters. No command reply is posted to a part that `User` posts and that starts with `/` ([hub.ts:196](../src/hub.ts#L196), [:278-288](../src/hub.ts#L278-L288), [:532-549](../src/hub.ts#L532-L549), [tools.ts:1584-1587](../src/tools.ts#L1584-L1587), [control.ts:231](../src/control.ts#L231)).
- When the core refuses the quote of an answer with `invalidQuote` or `chatItemNotFound`, as for a deleted prompt, Claude posts the answer without the quote. It does the same when the core refuses the message with the quote as too large, with `largeMsg`. Claude also posts these replies without the quote in the same cases ([hub.ts:290-296](../src/hub.ts#L290-L296), [:551-554](../src/hub.ts#L551-L554)):
  - `Session N is not running.`
  - `The file of this message was not received: ERROR`
  - the reply to a one-line `/` text of a moderator, member or author
- When the core refuses an answer or a terminal prompt with `feature not allowed NAME`, Claude posts a text in its place, and the turn goes on ([hub.ts:196](../src/hub.ts#L196), [:278-288](../src/hub.ts#L278-L288), [:524-526](../src/hub.ts#L524-L526)). NAME is, for example, `SimpleX links` for a SimpleX link while that preference is off. A prompt of `User` is also refused while the preference is on only for roles above that of `User`. The texts are:
  - for an answer, `Claude's answer was not posted: NAME are off in the group of this session; its owners can turn them on in the group's preferences.`, quoting the turn's prompt as the answer would;
  - for a terminal prompt, `User's prompt was not posted: NAME are not allowed for User in the group of this session; its owners can allow them in the group's preferences.`
- The hook cuts a prompt or an answer over 250,000 characters at that length and appends `…`, so that the request fits within the socket's limit of 1 MiB ([hook.ts:16](../src/hook.ts#L16), [:307-309](../src/hook.ts#L307-L309)).
- A card of a tool use that started after the hook stays pending, because the hook's start time `at` is earlier than the card's creation ([hook.ts:114](../src/hook.ts#L114), [hub.ts:204-208](../src/hub.ts#L204-L208)).
- The Stop hook sends the model of its answer ([hook.ts:17](../src/hook.ts#L17), [:19-21](../src/hook.ts#L19-L21), [:118](../src/hook.ts#L118), [:137-145](../src/hook.ts#L137-L145), [:153-185](../src/hook.ts#L153-L185)):
  - The model is the `message.model` of Claude's last entry in the transcript, an entry of type `assistant` without `isSidechain`. It is taken once the text of that entry is equal to the answer.
  - Only the end of the transcript is read, 1 MiB plus twice the answer's size in bytes. The hook reads it up to 20 times, 100 ms apart, as Claude Code writes the transcript every 100 ms.
  - No model is sent for the model `<synthetic>`, for an answer without text, for an empty `transcript_path` or an unreadable transcript, and when the answer's entry is not written within the reads.
- PostModelSwitch sends `{id, pid, at, model, effort}`, with `effort` when the hook's input has it ([hook.ts:118-120](../src/hook.ts#L118-L120)). Nothing is posted for it. When the model or effort changed, the hub writes them to `session.json` and updates the short description ([hub.ts:187-201](../src/hub.ts#L187-L201)).
- The hub takes a model or an effort level only from a request whose `at` is not older than the `at` of the value it took last. It keeps the two times apart, one for the model and one for the effort level. The model of a `/title` comes with the `at` of its SessionStart hook, so a hook that started during the wait of a held title keeps its model ([flow 27](#27-clear---continue-and-resume-in-one-session)). A request without `at` counts as the current time. So when a Stop request that waited for its transcript entry arrives after a later PostModelSwitch request, the model of the switch stays ([hub.ts:261-265](../src/hub.ts#L261-L265)).
- For a Stop request with a changed model or effort, the hub posts the answer before it updates the short description. So the prompt of a request handled meanwhile is posted after the answer. The description is updated also when a post fails ([hub.ts:194-201](../src/hub.ts#L194-L201)).
- A turn hook starts the service when none runs. It repeats a turn refused with 409 up to 15 times, 1 s apart, which is longer than the 10 s wait of a held title. It prints `{}` and logs errors on its stderr ([hook.ts:114-135](../src/hook.ts#L114-L135)). Claude Code gives a hook without `timeout` 600 s, more than the 60 s of a prompt's reads and the 15 s of repeats.

Source: [hooks.json:5-7](../hooks/hooks.json#L5-L7), [hub.ts:180-202](../src/hub.ts#L180-L202), [:267-296](../src/hub.ts#L267-L296), [:532-549](../src/hub.ts#L532-L549), [user.ts:29-32](../src/user.ts#L29-L32).

---

## 22. A tool use approved in the session group

```mermaid
sequenceDiagram
  participant CC as Claude Code
  participant S as Session process
  participant D as Service
  participant N as Session N
  participant O as Owner
  CC->>S: notifications/claude/channel/permission_request {request_id, tool_name, description, input_preview}
  CC->>CC: the terminal dialog stays open
  S->>D: POST /permission {id, requestId, toolName, description, inputPreview}
  D->>N: permission
  N->>O: card in #35;G: I want to use `Bash`: `DESCRIPTION`, preview, /'approve N' or /'reject N'
  O->>N: /approve N
  N->>N: performing
  N->>D: onLine {permission: {requestId, behavior: allow}}
  D->>S: the line on /attach
  S->>CC: notifications/claude/channel/permission {request_id, behavior: allow}
  N->>N: done, removed
  N->>O: card edited: ... Approved by Alice.
```

Notes:

- On `/reject N`, the session pushes `deny`, and the card ends with `Rejected by NAME.` ([hub.ts:172-173](../src/hub.ts#L172-L173)).
- The first answer, in the terminal or in the group, applies. The last line of a card that is still pending becomes `Ended.` at an answer, or at a prompt that is not mid-turn, whose hook started after the card was created, and when the last attachment of the session ends. A prompt that Claude Code adds to the running turn ends no card ([hub.ts:181](../src/hub.ts#L181), [:204-208](../src/hub.ts#L204-L208), [service.ts:321](../src/service.ts#L321), [control.ts:735-736](../src/control.ts#L735-L736)).
- The input preview is cut to fit the card ([control.ts:162](../src/control.ts#L162), [:824-838](../src/control.ts#L824-L838)).
- The card is posted in mode `requests` also for a session in mode `host` ([hub.ts:170](../src/hub.ts#L170)).

Source: [session.ts:58-62](../src/session.ts#L58-L62), [:114-118](../src/session.ts#L114-L118), [service.ts:143-147](../src/service.ts#L143-L147), [hub.ts:169-177](../src/hub.ts#L169-L177), [control.ts:743-746](../src/control.ts#L743-L746).

---

## 23. Mode `host` for a session

```mermaid
sequenceDiagram
  participant S as Session process
  participant D as Service
  participant N as Session N
  participant O as Owner
  S->>D: POST /session {config: {approve: host}}
  D->>N: requestMode(host): requested = host
  alt the group has no approver
    O->>N: joins #35;G as an admin or owner, or gets one of these roles: membership event, Control.onEvent, announceRights, askMode
  end
  N->>O: card in #35;G: I want to run in mode host ... /'approve N' or /'reject N'
  O->>N: /approve N
  N->>N: perform: setMode(host), mode host in the custom data of #35;G
  N->>O: card edited: I run in mode host ... Approved by Alice.
  S->>D: POST /session {config: {approve: requests}}
  D->>N: requestMode(requests): a pending mode card ends with Ended.#59; at once, mode requests in the custom data of #35;G
```

Notes:

- At most one `mode` card is pending at a time. After a card is rejected or expires, the request is forgotten until the next `/session` ([hub.ts:140-151](../src/hub.ts#L140-L151)).
- When a card is rejected, the hub sets `hostDeclined` in `session.json`. While it is set, no card is posted for a `/session` request of `host`, also after a service restart. It is cleared at a `/session` with `requests` ([hub.ts:70-74](../src/hub.ts#L70-L74), [:136](../src/hub.ts#L136), [:141](../src/hub.ts#L141), [:145](../src/hub.ts#L145), [:325-328](../src/hub.ts#L325-L328)).
- When the session opens, the approved mode is read from the custom data of `#G`, and is `requests` when none is stored. No mode is stored in `session.json` ([hub.ts:75](../src/hub.ts#L75), [:320-323](../src/hub.ts#L320-L323)).
- An idle session is closed also while a `mode` card is pending ([hub.ts:119-121](../src/hub.ts#L119-L121)).
- The session process marks the action tools for Claude Code's confirmation by the option read at its own start, before the owner decides ([session.ts:57](../src/session.ts#L57)).

Source: [hub.ts:132-151](../src/hub.ts#L132-L151), [:320-334](../src/hub.ts#L320-L334), [:519](../src/hub.ts#L519), [tools.ts:571-572](../src/tools.ts#L571-L572).

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

Source: [control.ts:379-390](../src/control.ts#L379-L390), [tools.ts:576-579](../src/tools.ts#L576-L579), [hub.ts:427-429](../src/hub.ts#L427-L429).

---

## 25. `reset-owners`

```mermaid
sequenceDiagram
  participant P as Person
  participant R as reset-owners
  participant D as Service
  participant M as MainGroup
  participant N as Sessions
  participant O as Owner
  P->>R: npx -y @simplex-chat/mcp@VERSION reset-owners in a terminal
  R->>D: POST /reset-owners
  D->>M: replace, also while owners exist
  M->>O: the Main group deleted for all its members
  M->>M: a new Main group with a link
  Note over M,N: the session groups and the roles of their members stay as they are
  D-->>R: {link}
  alt on a terminal
    R-->>P: "Claude Control" was replaced. Join the new one in the SimpleX app. The link works once:, QR code, link
  else without a terminal, as by Claude's tools
    R-->>P: "Claude Control" was replaced. Run npx -y @simplex-chat/mcp@VERSION link in a terminal to see the link to the new one.
  end
```

Notes:

- VERSION is the plugin's version, so that the command runs the service's version ([owner.ts:7-8](../src/owner.ts#L7-L8)).
- `reset-owners` replaces the group at once, also while owners exist, without their approval. On a terminal, it prints the new link with a QR code. Without a terminal, it names the `link` command instead, so the link is never in Claude's context ([owner.ts:17-20](../src/owner.ts#L17-L20), [:28-33](../src/owner.ts#L28-L33), [hub.ts:403-405](../src/hub.ts#L403-L405)).
- The members who joined the session groups from the old Main group stay there with their roles and the rights of those roles ([hub.ts:312-314](../src/hub.ts#L312-L314)).

Source: [main-group.ts:99-109](../src/main-group.ts#L99-L109).

---

## 26. Idle close, idle stop and a newer version

```mermaid
sequenceDiagram
  participant S as Session process
  participant D as Service
  participant N as Session N
  participant S2 as Session process of a newer plugin
  S->>D: the /attach stream ends with Claude Code
  D->>N: onLine = undefined, endPermissions
  D->>D: idle timer, 10 min
  D->>N: no call, no action in memory: entry closed, open false
  D->>D: no entry: stop timer, 10 min
  D->>D: stop: actions cancelled, cards edited, cores closed, lock released
  S2->>D: GET /status: version lower than its own
  S2->>D: POST /stop
  S2->>D: spawn a service of its version
```

Notes:

- The service cancels the stop timer when it creates an entry for a call or an attachment ([service.ts:357-367](../src/service.ts#L357-L367)).
- A service of a higher version is kept. When the service speaks another protocol version, clients fail with `Restart Claude Code to update the SimpleX plugin.` ([client.ts:40-52](../src/client.ts#L40-L52)).

Source: [service.ts:177-199](../src/service.ts#L177-L199), [:315-323](../src/service.ts#L315-L323), [:404-422](../src/service.ts#L404-L422), [hub.ts:459-469](../src/hub.ts#L459-L469).

---

## 27. `/clear`, `--continue` and `/resume` in one session

Claude Code keeps the session server's `CLAUDE_CODE_SESSION_ID` from its spawn. After `/clear` and `/resume`, it gives the hooks a new `session_id`. With `--continue`, it may spawn the server with a startup ID other than the resumed one. The hooks send `CLAUDE_PID`, and the session server sends its parent process. Both are the same Claude Code process.

```mermaid
sequenceDiagram
  participant CC as Claude Code, process P
  participant S as Session process
  participant HK as Hooks
  participant D as Service
  participant N as Session of A
  Note over CC,N: /clear in session A, the server attached as A
  CC->>HK: SessionStart {session_id: B, source: clear}, CLAUDE_PID P
  HK->>D: POST /title {id: B, pid: P}
  D->>D: B unknown#59; entry A has pid P and is attached: alias B to A, in A's session.json
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
  D->>D: R2 has session.json#59; entry A has pid P and is attached: A, without an alias
  HK->>D: POST /turn {id: R2, pid: P, answer}
  D->>N: turn, posted in A's group
  Note over CC,N: later, claude --resume R2 in process Q
  S->>D: POST /session {id: R2, pid: Q}
  D-->>S: {number of R2}: its own group and chats
  Note over CC,N: the title of a new ID F before the server of P registers, as with --fork-session or /clear during a re-attach
  HK->>D: POST /title {id: F, pid: P}
  D->>D: F unknown, no session of P: title of P = F#59; the title is held
  D-->>HK: the reply
  S->>D: POST /session {id: A, pid: P}
  D->>D: A has session.json: A#59; the held title resumes: entry A has pid P: alias F to A
  D->>N: rename, model
```

Notes:

- The service maps a process to the session of its session server while the server is attached, and for 10 s after it detached. It maps a process to the session of its title for 10 s. So a new Claude Code process with a reused PID later starts its own session ([service.ts:283-288](../src/service.ts#L283-L288)).
- The service gives an alias only to an ID without `session.json`. A hook of an ID with `session.json` goes to the session attached from its process, without an alias, or else to its own session. A session server with such an ID registers its own session ([service.ts:268-277](../src/service.ts#L268-L277)).
- The service answers a title of a new ID from a process without a known session at once, and holds the title. It applies the title when the session server of that process registers, or after 10 s without one. In the second case, the service opens a session of its own for the title's ID. So after `/clear` during the re-attach of a session server after a service start, and after `--fork-session` in either order, the new ID is joined to the session that the server registers ([service.ts:201-262](../src/service.ts#L201-L262), [:466-476](../src/service.ts#L466-L476)).
- When a session is created for an ID, the existing aliases of that ID are written to its first `session.json` ([service.ts:397-398](../src/service.ts#L397-L398)).
- Turn hooks of an ID whose title is held get 409, and repeat the turn as in [flow 21](#21-a-terminal-prompt-and-the-answer) ([hook.ts:14](../src/hook.ts#L14)).
- A second title of a process whose title is held is held too. So after two `/clear` within the wait, both new IDs are joined to the session that the server registers. At a stop, the service saves the held title requests in `ROOT/titles.json`, each with its `at`, and at its next start it takes them as new titles ([service.ts:106](../src/service.ts#L106), [:180](../src/service.ts#L180), [:222-232](../src/service.ts#L222-L232)).
- Aliases are read from `session.json` at the service's start, so the service routes later hooks and calls with B or X to the same session ([service.ts:87-91](../src/service.ts#L87-L91), [:290-295](../src/service.ts#L290-L295)).
- Without `CLAUDE_PID`, before Claude Code 2.1.214, a new session server waits the 10 s and then registers its own ID.

Source: [hook.ts:93](../src/hook.ts#L93), [:120](../src/hook.ts#L120), [:311-314](../src/hook.ts#L311-L314), [session.ts:53](../src/session.ts#L53), [service.ts:201-295](../src/service.ts#L201-L295).

---

## 28. A member joins the Main group and a session's group, and a person is added to it

```mermaid
sequenceDiagram
  participant A as Owner Alice
  participant B as Bob
  participant G as Carol
  participant C as Core _hub
  participant M as MainGroup
  participant N as Session N
  A->>B: a group link of #35;M, or an invitation of a contact
  B->>C: joins #35;M
  C->>M: connectedToGroupMember, or joinedGroupMember by the hub's link
  M->>C: apiListMembers #35;M: Bob a current member
  M->>B: Bob joined. They can join the group of any Claude Code session here, prompt Claude and approve its actions.
  B->>M: /join N in #35;M
  M->>N: Hub.join: role admin, or owner for an owner of #35;M
  N->>C: apiAddMemberViaGroup #35;G via #35;M, role admin
  M->>B: Sent you an invitation to "N · NAME".
  B->>C: joins #35;G
  C->>N: joinedGroupMember: Control.onEvent, announceRights, askMode
  N->>B: Bob is an admin and can send prompts to Claude and the group's commands, and approve tool use. You can change role to observer to stop it: /'observer #35;G ID'
  A->>G: a group link of #35;G with role member, or an invitation of a contact with a role
  G->>C: joins #35;G
  C->>N: connectedToGroupMember: Control.onEvent, announceRights, askMode
  N->>B: Carol is a member and can send prompts to Claude ... You can change role to observer to stop it: /'observer #35;G ID'
  G->>C: a message in #35;G, /pending, /!ls
  C->>N: newChatItems: Session.onMessage, a prompt of Carol
  N->>G: to /pending: Only admins and owners of this group can send / commands.
  N->>G: to /!ls: Only admins and owners of this group can send / commands.
  A->>C: Carol's role set to admin in the app
  C->>N: memberRole: Control.onEvent, announceRights, askMode
  N->>A: Carol is an admin and can send prompts to Claude and the group's commands, and approve tool use. You can change role to observer to stop it: /'observer #35;G ID'
  A->>N: /observer #35;G ID of Carol
  N->>C: apiSetMembersRole #35;G [ID] observer, apiListMembers #35;G
  N->>A: Carol is an observer and can't send messages.
  Note over G,C: Carol's app refuses to send as an observer
  Note over A,N: Bob leaves #35;M, is removed from it, or gets another role there
  C->>M: leftMember, deletedMember or memberRole in #35;M
  M->>C: apiListMembers #35;M#59; the session groups stay as they are
```

Notes:

- A member's rights depend on the member's role in the session group, not on the way the member joined or on the member's role in the Main group. Admins and owners approve and send the group's `/` commands. The session groups stay as they are at changes in the Main group ([hub.ts:217-219](../src/hub.ts#L217-L219), [:312-314](../src/hub.ts#L312-L314), [main-group.ts:117-132](../src/main-group.ts#L117-L132)).
- With `/join N`, a member of the Main group gets an invitation as an admin, and an owner of the Main group as an owner ([hub.ts:440](../src/hub.ts#L440)).
- When a member of the group changes a member's role, the core raises `memberRole`, and Claude posts the new rights. For a role change by Claude, as with `/observer`, the core raises no event, and Claude states the observer's rights in its reply to `/observer` ([hub.ts:221-228](../src/hub.ts#L221-L228), [control.ts:401-403](../src/control.ts#L401-L403), [:930-933](../src/control.ts#L930-L933)).
- `/observer #G ID` is accepted from a sender whose role is admin or owner and not below the member's role, as SimpleX requires for a role change. Only a current member's role changes ([control.ts:392-404](../src/control.ts#L392-L404), [tools.ts:1558-1560](../src/tools.ts#L1558-L1560)):
  - For a member with the status `pending_approval` or `pending_review`, Claude replies `NAME's role can't be changed.`
  - `No member ID in this group.` is the reply for a member who is neither current nor pending, such as an invited member who has not joined.

  After the role change, the members are read again. So the standing approvals that the new observer gave are not counted while that member is not an admin or owner. They stay in the chat's custom data and are counted again when the member becomes one again ([control.ts:175-178](../src/control.ts#L175-L178)).
- Claude finds the joiner of the Main group's post among the refreshed members, because in `connectedToGroupMember` the member's status is the one before the connection. The post is made only for a human joiner ([main-group.ts:127-128](../src/main-group.ts#L127-L128)).
- Claude posts nothing when `User` joins a session group or when its role changes ([hub.ts:224](../src/hub.ts#L224), [:316-318](../src/hub.ts#L316-L318)).
- One post is made for each join. For each connection, the core raises one of two events:
  - `joinedGroupMember` for a member who joined through the hub, by its invitation or link;
  - `connectedToGroupMember` for a member whom another member added.

  The end-to-end test counts one post for each join in both groups ([test/e2e/service.test.ts](../test/e2e/service.test.ts)).
- A person added to the group directly has the rights of the role that the adding member set, as a member from the Main group has the rights of its role ([hub.ts:230-233](../src/hub.ts#L230-L233), [:312-314](../src/hub.ts#L312-L314)).
- Claude answers `/join N` from a member of the group of session N with `You are a member of "N · NAME".` ([hub.ts:439-444](../src/hub.ts#L439-L444)).
- At a reset of the owners, the session groups and their members stay as they are ([flow 25](#25-reset-owners)).

Source: [main-group.ts:111-115](../src/main-group.ts#L111-L115), [:117-132](../src/main-group.ts#L117-L132), [hub.ts:217-228](../src/hub.ts#L217-L228), [:436-447](../src/hub.ts#L436-L447), [:519](../src/hub.ts#L519), [control.ts:392-404](../src/control.ts#L392-L404), [:926-933](../src/control.ts#L926-L933).

---

## 29. `reply`

```mermaid
sequenceDiagram
  participant CC as Claude Code
  participant T as AgentTools
  participant N as Session N
  participant C as Core _hub
  participant A as Owner Alice
  CC->>T: reply {file, text}
  T->>T: the file checked, and its message with the caption and the preview by fitsMessage
  T-->>CC: too long: text: too long for a message with this TYPE#59; shorten it.
  T->>C: apiSendMessages #35;G with the file, its caption and its preview
  alt the group's preference for files and media allows the files of Claude's role
    C-->>T: the sent item
    T-->>CC: {item}
  else it does not
    C-->>T: error: feature not allowed Files and media
    T-->>CC: Files and media are off in the group of this session#59; its owners can turn them on in the group's preferences.
  end
  A->>C: the group's preferences changed in the app
  N->>C: a later profile update of #35;G, for the model or the name: apiGetChat #35;G count 0
  N->>C: apiUpdateGroupProfile #35;G, the current profile with Claude's fields changed
```

Notes:

- `reply` creates no action and posts no card, in both modes. It is not an action tool, so Claude Code does not ask before it either ([tools.ts:721-738](../src/tools.ts#L721-L738), [:490](../src/tools.ts#L490)).
- The core checks the preference at the send, by the sender's role. Only owners change the preference.
- At each profile update of `#G`, Claude sends the whole profile. It starts each update from the group's current profile in the chat database, and changes only the name, the short description and the commands, and turns full deletion on. So the owners' other preferences are kept ([control.ts:123-148](../src/control.ts#L123-L148), [:672-680](../src/control.ts#L672-L680)).

Source: [tools.ts:721-738](../src/tools.ts#L721-L738), [:557](../src/tools.ts#L557), [hub.ts:93](../src/hub.ts#L93), [control.ts:672-680](../src/control.ts#L672-L680).

---

## 30. A run that is not interactive

```mermaid
sequenceDiagram
  participant CC as Claude Code, claude -p
  participant HK as Hooks
  participant S as Session process
  participant D as Service
  Note over CC,D: claude -p or the Agent SDK: CLAUDE_CODE_ENTRYPOINT sdk-cli, sdk-ts or sdk-py for the hooks and the server
  CC->>HK: SessionStart, UserPromptSubmit, Stop
  HK-->>CC: {}, without a request
  CC->>S: tools/call
  S-->>CC: This Claude Code run is not interactive, so it has no SimpleX group ...
  Note over CC,D: claude -p from a command of a VS Code session: entrypoint claude-vscode#59; the hooks get ATTENDED 0 and CLAUDE_PID P, the server ATTENDED 1 and the parent process P
  CC->>HK: SessionStart
  HK->>D: ensureService, POST /title {id, pid: P, project, config, unattended: true}
  D->>D: marks of 10 s ago and older removed#59; P marked#59; a /session of P waiting for its title ends its wait
  HK-->>CC: {}
  S->>D: POST /session {id, pid: P}, before or after the mark
  D-->>S: 403
  CC->>S: tools/call
  S-->>CC: This Claude Code run is not interactive, so it has no SimpleX group ...
  CC->>HK: UserPromptSubmit, Stop
  HK-->>CC: {}, without a request
```

Notes:

- Claude Code sets `CLAUDE_CODE_ENTRYPOINT` for its hooks and its MCP servers. The value is `sdk-cli` for `claude -p`, and `cli` for an interactive session. The Agent SDK sets `sdk-ts` or `sdk-py`. Claude Code keeps an entrypoint already in the environment, except `cli`. So a `claude -p` run started by a command of a VS Code or desktop-app session keeps `claude-vscode` or `claude-desktop`.
- For its hooks, Claude Code sets `CLAUDE_CODE_SESSION_ATTENDED` to `1` or `0`, and to `0` for a run nobody attends. Its MCP servers get the variable only from the environment of the command that started Claude Code. In the run recorded with Claude Code 2.1.319 in [`claude-p-runs.jsonl`](../test/unit/claude-p-runs.jsonl), the hooks got `0` and a `CLAUDE_PID` equal to the server's parent process. The server got the parent session's `1` and `CLAUDE_PID`.
- The service checks the mark before and after the wait of `/session` for the title of its process. So the session server is refused whether its `/session` arrives before or after the hook's mark. A mark is valid for 10 s, and after that it is removed at the next mark ([service.ts:203-205](../src/service.ts#L203-L205), [:237-248](../src/service.ts#L237-L248)).
- A refused server answers every call with that text, relays no permission request and does not register again ([session.ts:79-80](../src/session.ts#L79-L80), [:92](../src/session.ts#L92), [:115](../src/session.ts#L115), [:127](../src/session.ts#L127), [:162-165](../src/session.ts#L162-L165)).
- A hook without `CLAUDE_PID` sends no mark. A failed mark is logged as `SimpleX MCP session start` ([hook.ts:102-112](../src/hook.ts#L102-L112)).

Source: [config.ts:23-33](../src/config.ts#L23-L33), [hook.ts:81-85](../src/hook.ts#L81-L85), [:115](../src/hook.ts#L115), [service.ts:221](../src/service.ts#L221), [:237-248](../src/service.ts#L237-L248), [session.ts:45](../src/session.ts#L45).

---

## 31. A prompt of Claude Code

```mermaid
sequenceDiagram
  participant CC as Claude Code
  participant HK as UserPromptSubmit hook
  participant T as Transcript
  participant D as Service
  participant N as Session N
  participant O as Owner
  Note over CC,O: at a turn's start
  CC->>HK: {prompt_id of the new turn, prompt: a task notification}
  CC->>T: the prompt's entry, origin task-notification
  HK->>T: the entry with prompt_id whose text is the prompt
  HK->>D: POST /turn {prompt, typed: false, note}
  D->>N: end tool-use cards created before at#59; turns += (at, null), unless at is below answeredAt
  N->>O: Claude: ✅ Background command "Build" completed (exit code 0)
  Note over CC,O: during the turn
  CC->>HK: {prompt_id of the running turn, prompt: a task notification}
  CC->>T: a queued_command attachment with the prompt and its origin
  HK->>T: the attachment after the entry with prompt_id
  HK->>D: POST /turn {prompt, typed: false, note, midTurn: true}
  D->>N: cards stay pending, and a turn record needs a matched pushed prompt
  N->>O: Claude: ❌ Background command "Lint" failed with exit code 1
  CC->>D: Stop hook: POST /turn {answer}
  N->>O: Claude: the answer, without a quote
```

Notes:

- The note is posted before the answer of its turn, in the order in which the hooks' requests arrive. The UserPromptSubmit hook of a turn starts before its Stop hook ([hub.ts:195-197](../src/hub.ts#L195-L197)).
- A mid-turn prompt ends no tool-use card. A prompt adds a turn record only when its hook started no earlier than the hook of the last answer handled (`answeredAt`), and a mid-turn prompt only when it also matched a pushed prompt, an owner's prompt from the group. So the answer quotes the turn's prompt, or an owner's prompt added to the turn ([hub.ts:181](../src/hub.ts#L181), [:185](../src/hub.ts#L185), [:267-276](../src/hub.ts#L267-L276)).
- A person's prompt typed during a turn runs its hook at once. Its record is written later, as a `queued_command` attachment when Claude Code adds it to the running turn, or as the entry of the next turn. `User` posts it as typed and mid-turn ([hook.ts:187-203](../src/hook.ts#L187-L203)).

Source: [hook.ts:147-151](../src/hook.ts#L147-L151), [:187-290](../src/hook.ts#L187-L290), [hub.ts:180-202](../src/hub.ts#L180-L202).

---

## 32. `/secret` and its use

`SECRETS` is the folder of `--secrets-folder`.

```mermaid
sequenceDiagram
  participant P as Person
  participant K as Control
  participant C as Core
  participant O as Owner
  participant D as Member
  participant A as Agent
  participant T as AgentTools
  O->>C: /secret GITHUB_TOKEN VALUE
  C->>K: newChatItems
  K->>K: first word /secret: queued after the earlier commands
  K->>C: apiDeleteMemberChatItem #35;G [item]
  C->>O: the message deleted: moderated by the agent
  C->>D: the message deleted: moderated by the agent
  K->>K: ownerCommand: approver, --secrets-folder, NAME and VALUE checked
  K->>K: Secrets.set: SECRETS/GITHUB_TOKEN.part written and renamed to SECRETS/GITHUB_TOKEN, the record in DATA/secrets.json
  K->>P: Secret GITHUB_TOKEN set by Alice.
  K->>C: apiSendMessages #35;G [Secret GITHUB_TOKEN is set. /'unsecret GITHUB_TOKEN'], without a quote
  A->>T: list
  T-->>A: {chats, actions, secrets: {folder: SECRETS, names: [GITHUB_TOKEN]}}
  A->>A: a shell command with "$(cat SECRETS/GITHUB_TOKEN)"
```

Notes:

- While the control chat is not lost, every `/secret` message is deleted, also one from a member who is not an approver, one with an invalid NAME or VALUE, and one in a member support chat, which gets no reply ([control.ts:226-231](../src/control.ts#L226-L231), [:233](../src/control.ts#L233)).
- After a failed deletion the command does not run, and `control chat command: ERROR` is logged ([control.ts:232-236](../src/control.ts#L232-L236)).
- With full deletion on, the message is replaced by a moderated item in the agent's database and at each member; a member who receives the message after the deletion stores it as moderated ([Commands.hs:4294-4297](../../../src/Simplex/Chat/Library/Commands.hs#L4294-L4297), [Subscriber.hs:2490-2499](../../../src/Simplex/Chat/Library/Subscriber.hs#L2490-L2499), [:2228-2232](../../../src/Simplex/Chat/Library/Subscriber.hs#L2228-L2232), [:2268-2274](../../../src/Simplex/Chat/Library/Subscriber.hs#L2268-L2274)).
- `/unsecret NAME` and `/secrets` are answered with replies that quote them, as other commands ([control.ts:235](../src/control.ts#L235)).
- The value is absent from the log line, the reply and the `list` result ([control.ts:429-430](../src/control.ts#L429-L430), [tools.ts:1289-1290](../src/tools.ts#L1289-L1290)).

Source: [control.ts:224-237](../src/control.ts#L224-L237), [:257-287](../src/control.ts#L257-L287), [:425-446](../src/control.ts#L425-L446), [files.ts:157-184](../src/files.ts#L157-L184), [tools.ts:1282-1291](../src/tools.ts#L1282-L1291).
