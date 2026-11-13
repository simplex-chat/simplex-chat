# SimpleX MCP: the list of sessions in the Main group

Status: approved by the user on 2026-11-13 and implemented on `ep/mcp-session-list`, on top of `ep/mcp-activity`. The user answered Q1 A and Q2 B on 2026-11-13; decisions 16 and 17 apply Q2 B. Code references: master `97bbbd9ac`. Claude Code references: version 2.1.322 and its binary at the byte offsets given, and the 27 session records of Claude Code 2.1.260 to 2.1.322 on this machine, read on 2026-11-13. Decisions of the 0.3.0 plan are cited as "0.3.0 decision N".

## 1. Goal

Roadmap line, [ROADMAP.md:17](../packages/simplex-chat-mcp/ROADMAP.md): "One list in the Main group shows every session and its state: working, waiting for you or idle."

The user's words, 2026-11-11, on the lead's proposal "`/sessions` showing each session as running, waiting for approval or idle":

> 5 - yes, via message editing, so main group is a list of sessions with their status (including "waiting") - we already use it for support group, and it doesn't require reposting. Or we could add protocol support for moving messages down, it would help support bot too (though a bit of re-engineering with not so much value)

On a separate post for waiting sessions, in the same message:

> I think we have an idea of "important messages" to indicate waiting, and limit alerts to them only. A separate "waiting" seems wrong.

Outcome:
- Claude keeps one message in the Main group with every session of the data folder, and edits it when a state changes. Each running session has a line with its state, and one last line names the sessions that are not running.
- The states are those of Claude Code itself: working, waiting and idle. A session also waits for you while a card in its group waits for a decision.
- `/sessions` replies with the same text.

## 2. What users see

After the first owner joins the Main group, Claude posts its greeting, as in 0.3.0, and then the list. The list shows every session of the data folder (Q2 B):

```
🟠 3 · simplex-chat: working
🔵 6 · website: waiting for you, 2 approvals
🔵 7 · docs: waiting for you in Claude Code
🟢 8 · app: idle
⚪ Not running: 5 · bio, 4 · notes, 2 · openclaw, 1 · first
```

A session is running while a Claude Code is attached to it, or while cards wait in its group. Each running session has its own line, by session number. The last line, `⚪ Not running: `, names every other session, newest first, as `N · NAME` separated by `, `. Without any session, the list is `No sessions.`

Lines of running sessions:
```
line                                        when, the first that applies
🔵 N · NAME: waiting for you, K approvals    K cards wait for a decision in the session's group; "1 approval" for one
🔵 N · NAME: waiting for you in Claude Code  Claude Code waits for an answer in its own window, such as a dialog
🟠 N · NAME: working                         Claude Code works: a turn, or a subagent or another delegated task
🟢 N · NAME: idle                            Claude Code waits for the next prompt, also while a background command runs
▶️ N · NAME: running                         Claude Code is attached, and its state can't be read
```

The list is one message, so it is cut to the size of one message:
- The lines of running sessions are kept whole.
- The `⚪` line names the sessions that are not running, newest first, while the text still fits. The rest is counted at its end: `⚪ Not running: 212 · api, 211 · docs, 190 older sessions` (`1 older session` for one). When no name fits, the line is `⚪ Not running: 210 older sessions`.
- The text of a message takes at most 14,962 bytes as JSON encodes it, a line break taking 2. That holds at least 62 lines of running sessions with names of 50 four-byte characters, and the `⚪` line after them. With more running sessions than fit, the core refuses the post or the edit, and the list keeps its last text.

How the list changes:
- The emojis have the colors of Claude Code's own state indicator: green for Idle, orange for Working…, blue for Waiting.
- A tool-use card counts only while Claude Code shows its permission dialog, or while its state can't be read. When someone answers the dialog in the terminal, the card stays in the group until the turn ends (0.3.0 decision 29), and the line shows the session working again.
- When Claude Code ends, its session moves into the `⚪` line at once.
- Claude edits the list within 2 seconds of a change. An edit alerts nobody, and the apps mark the message "edited".
- Messages posted after the list, such as each owner's invitation to a new session's group, move it up (Q1 A).
- When the list is 23 hours old, Claude posts a new list at the bottom and deletes the old one for everyone. The core lets Claude edit a message only within 24 hours.
- The list's own posts are silent once receivers support silent messages (`ep/mcp-silent-messages`). Until then a new list alerts as a normal message.
- When the service stops, Claude edits the list once more: every session is in the `⚪` line.

Limits of the list's message:
```
limit                         value                                       what Claude does
size of one message           15,602 bytes with a 600-byte envelope;      the ⚪ line keeps the newest names that fit and
                              14,962 bytes of text as JSON encodes it      counts the rest; running lines are kept whole
editing window of the core    24 hours from the post                      a new list at 23 hours; the old one deleted
deletion for everyone         30 hours from the post                      a refused deletion is logged; the old list keeps
                                                                          its last text
edit rate                     the text is rendered every 2 seconds        an edit only when the text changed
```

`/sessions` replies with the text of the list, cut to the size of one message with its quote of the command.

Before and after:
```
moment                                  0.3.0                                 this pull request
the first owner joins the Main group    the greeting                          the greeting, then the list
a session starts                        the owners' invitations               the invitations; the session's line in the list
Claude works on a turn                  nothing in the Main group             🟠 working
a card waits in the session's group     the card                              the card; 🔵 waiting for you, 1 approval
a dialog waits in Claude Code           nothing                               🔵 waiting for you in Claude Code
a turn ends, or is interrupted          the answer in the session's group     🟢 idle
Claude Code ends                        nothing                               the session moves into the ⚪ line
/sessions                               N · NAME, and ", closed"              the text of the list
the list is 23 hours old                -                                     a new list at the bottom; the old one deleted
```

A turn with a tool-use card:

```mermaid
sequenceDiagram
  participant CC as Claude Code
  participant R as record of Claude Code
  participant H as hub, every 2 s
  participant M as Main group
  participant O as owner's app
  CC->>R: status busy
  H->>R: read
  H->>M: edit the list: 🟠 1 · app: working
  M->>O: the list updated, no alert
  CC->>R: status waiting, a permission dialog
  CC->>H: permission request: a card in the session's group
  H->>M: edit: 🔵 1 · app: waiting for you, 1 approval
  O->>H: /approve 3 in the session's group
  CC->>R: status busy
  H->>M: edit: 🟠 1 · app: working
  CC->>R: status idle, the turn ends
  H->>M: edit: 🟢 1 · app: idle
```

The first owner joins, and the list is replaced after 23 hours:

```mermaid
sequenceDiagram
  participant H as hub
  participant M as Main group
  participant O as owner's app
  O->>M: joins by the link
  H->>M: the greeting
  H->>M: the list, silent
  H->>H: Main group data: the list's item and its time
  Note over H,O: each change: the list edited
  Note over H,O: 23 hours after the list was posted
  H->>M: a new list, silent
  H->>H: Main group data: the new item and its time
  H->>M: the old list deleted for everyone
  M->>O: the new list at the bottom, the old one shown as deleted
```

## 3. Questions for the user

Q1. Where does the list stay as it changes? Answered A by the user on 2026-11-13.
- A, recommended: Claude edits one message in place. Edits alert nobody. Messages posted after it move it up: each session start sends every owner an invitation in the Main group, and each command gets a reply there. Claude posts a new list at the bottom once a day, because the core edits a message only within 24 hours. Cost: one new message a day while the service runs.
- B: Claude keeps the list as the last message. When another message follows it, Claude posts the list again at the bottom at the next change and deletes the old one, as the support bot does with its cards ([cards.ts:257-287](../apps/simplex-support-bot/src/cards.ts#L257-L287)). The Main group's chat-list preview then shows the list. Cost: a new message at about every session start, because of the invitations, and a deleted message left in place each time.
- C: a core and app change that moves an edited message down, as the user proposed for the support bot too. It is a later pull request; A applies until then.

Q2. Which sessions does the list show? Answered B by the user on 2026-11-13. Decisions 16 and 17 keep the list readable as it grows: the sessions that are not running share one line, newest first, and the oldest are counted when the message is full.
- A, recommended in the first draft: the sessions whose Claude Code runs, the ended ones until the service closes them 10 minutes later, and any session with cards waiting. `/sessions` lists every session with its state. The list holds as many lines as sessions run at once.
- B: every session of the data folder, ended ones as not running. A data folder keeps every session, and session numbers only grow, so the list grows with every session ever started, up to the size of one message.

## 4. Decisions

Facts:
1. Today `/sessions` replies with one line per session of the hub, `N · NAME`, with `, closed` for a session the service has no entry for ([hub.ts:431-434](../packages/simplex-chat-mcp/src/hub.ts#L431-L434), [main-group.ts:112](../packages/simplex-chat-mcp/src/main-group.ts#L112)). The hub opens every stored session at its start ([hub.ts:378](../packages/simplex-chat-mcp/src/hub.ts#L378), [files.ts:65-72](../packages/simplex-chat-mcp/src/files.ts#L65-L72)), and a new session gets the next number ([service.ts:89](../packages/simplex-chat-mcp/src/service.ts#L89), [:398](../packages/simplex-chat-mcp/src/service.ts#L398)).
2. Claude Code keeps a record of each running process in `CONFIG/sessions/PID.json`. CONFIG is "the Claude Code config directory ($CLAUDE_CONFIG_DIR or ~/.claude)" (binary text at 151817147); the folder is `Owe` (286261242), and `Bn` writes `${process.pid}.json` there (287247536).
3. Claude Code writes `status` and `waitingFor` to its record at each change of either (`Ftt({status:bn,waitingFor:fo})` at 324103663, `Ftt` at 287249749). `status` is `waiting` while a dialog, an elicitation, a worker request or a sandbox request waits for the person; otherwise it is `busy` while a query runs or delegated work is active, and `idle` else (`mEe` and `$$o`, 324091852 and 324092046). `idle` is written as `shell` while a background Bash task runs (324103201, `Rmo` at 291522871). The terminal shows the three states as Idle in green, Working… in orange and Waiting in blue (`q$o` at 324091373).
4. Claude Code writes the record in place with `writeFile` of `fs/promises` (import at 287237817), so a read during a write can return a partial file.
5. All 27 records on this machine, of Claude Code 2.1.260 to 2.1.322, have `status`: 18 `idle`, 4 `busy`, 4 `waiting` (`permission prompt` twice, `dialog open`, `input needed`) and 1 `shell`. The record of the lead's session, `4044369.json` of 2.1.322, said `busy` at 08:44 UTC on 2026-11-13 while its background agents worked.
6. The session server sends its parent process as `pid` in `POST /session` ([session.ts:53](../packages/simplex-chat-mcp/src/session.ts#L53)). On this machine the plugin's session server 4044485 has the parent 4044369, `claude --resume ...`, whose record is `~/.claude/sessions/4044369.json`. Its environment has `CLAUDE_CONFIG_DIR`, as Claude Code's has; Claude Code passes its environment to MCP servers (`ep/mcp-fixes` decision 9).
7. Claude Code runs no Stop hook after a user interrupt: "Does not run if the stoppage occurred due to a user interrupt. API errors fire StopFailure instead." (https://code.claude.com/docs/en/hooks, Stop). A typed prompt runs its UserPromptSubmit hook when it is submitted, also during a turn, and can wait for its own turn (`plans/2026-11-12-mcp-harness-notes.md`, facts 5 and 6).
8. The core edits a sent message only within one day of its time: `editable = deletable && isNothing itemForwarded`, with `deletable` true while `currentTs - itemTs < nominalDay` ([Messages.hs:538-550](../src/Simplex/Chat/Messages.hs#L538-L550)). An edit of another item is refused with `invalidChatItemUpdate` ([Commands.hs:861](../src/Simplex/Chat/Library/Commands.hs#L861), [:883](../src/Simplex/Chat/Library/Commands.hs#L883)). Deletion for everyone is allowed for a day and 6 hours ([Commands.hs:956](../src/Simplex/Chat/Library/Commands.hs#L956)).
9. An update is sent without the notification flag ([Protocol.hs:1339-1348](../src/Simplex/Chat/Protocol.hs#L1339-L1348)). The apps alert for an update only when it creates the item ([SimpleXAPI.kt:3825-3843](../apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L3825-L3843), [SimpleXAPI.swift:3209-3219](../apps/ios/Shared/Model/SimpleXAPI.swift#L3209-L3219)), and the chat list shows the update of a chat's last item ([ChatModel.kt:679-681](../apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L679-L681)). An edit of an item that is not live marks it edited ([Commands.hs:877](../src/Simplex/Chat/Library/Commands.hs#L877)).
10. Each new session group gets an invitation for each owner of the Main group ([hub.ts:421-423](../packages/simplex-chat-mcp/src/hub.ts#L421-L423)), sent over the Main group and saved there as an item ([Commands.hs:4416-4418](../src/Simplex/Chat/Library/Commands.hs#L4416-L4418)); "The invited member sees it in group A" ([2026-10-10-invite-member-via-group.md](../docs/rfcs/2026-10-10-invite-member-via-group.md)).
11. Tool-use cards end at the next prompt or answer and at the last detach ([hub.ts:181](../packages/simplex-chat-mcp/src/hub.ts#L181), [:204-208](../packages/simplex-chat-mcp/src/hub.ts#L204-L208), [service.ts:321](../packages/simplex-chat-mcp/src/service.ts#L321)).
12. Actions cancelled by a stop stay `pending` with `interrupted` set, and their cards say `Cancelled: the server stopped.` ([actions.ts:284-295](../packages/simplex-chat-mcp/src/actions.ts#L284-L295), [control.ts:683-686](../packages/simplex-chat-mcp/src/control.ts#L683-L686)).
13. The Main group's custom data is `{simplexMcpMain: true}` ([main-group.ts:12](../packages/simplex-chat-mcp/src/main-group.ts#L12), [:202](../packages/simplex-chat-mcp/src/main-group.ts#L202)), and `findMainGroup` finds the group by that key ([main-group.ts:185-188](../packages/simplex-chat-mcp/src/main-group.ts#L185-L188)).

14. One message holds at most 15,602 bytes as the plugin measures it: 600 for the envelope, and the JSON of the content, the file name, the quote and the mentions ([tools.ts:163-164](../packages/simplex-chat-mcp/src/tools.ts#L163-L164), [:1575-1578](../packages/simplex-chat-mcp/src/tools.ts#L1575-L1578)). A text without a quote then takes at most 14,962 bytes as JSON encodes it. A session's display name has at most 50 code points ([messages.ts:12](../packages/simplex-chat-mcp/src/messages.ts#L12), [:361](../packages/simplex-chat-mcp/src/messages.ts#L361), [hub.ts:528-530](../packages/simplex-chat-mcp/src/hub.ts#L528-L530)).

Choices:

15. The list is one message of Claude in the Main group, edited in place, with a new message when it is 23 hours old (Q1 A).
16. The list shows every session of the data folder (Q2 B). A running session, with a Claude Code attached or with cards waiting, has its own line, by number. The last line, `⚪ Not running: `, names every other session as `N · NAME`, newest first, separated by `, `. Without any session, the text is `No sessions.` `/sessions` replies with the same text.
17. The text fits one message by `fitsMessage` (fact 14), without a quote for the list, and with the quote of the command for the reply to `/sessions`. The lines of running sessions are kept whole. The `⚪` line takes the most names, newest first, for which the text with the count of the rest, `, K older sessions` (`1 older session`), still fits; when no name fits, it is `⚪ Not running: K older sessions`. A line of a running session takes at most 239 bytes: 5 for the emoji and the space, 200 for the name, 32 for `: waiting for you in Claude Code` and 2 for the line break. So at least 62 such lines fit with the `⚪` line. With more, the core refuses the post or the edit as too large, the refusal is logged, and the list keeps its last text.
18. The state of a session comes from Claude Code's record (facts 2-5), not from the hooks. A turn's end is not visible to the hooks after an interrupt, and a typed prompt that waits for its own turn runs its hook before the previous turn ends (fact 7).
19. The session server sends the path of its record in `POST /session` as `record`: `CLAUDE_CONFIG_DIR` or `~/.claude`, then `sessions`, then its parent's PID with `.json`. The service passes it to the session. A later `/session` of the session replaces it.
20. Every 2 seconds the hub reads the record of each session with an attachment, and renders the list. A record that does not parse keeps the last status read (fact 4). A missing path, a missing file or another status gives `running`.
21. The lines of running sessions are those of section 2. The cards are the session's pending actions without those cancelled by a stop (fact 12). A tool-use card counts while the record says `waiting`, or while the record can't be read.
22. Claude posts the first list only while the Main group has an owner, so it follows the greeting. The item and its time are kept in the Main group's custom data as `simplexMcpList: {item, at}`, next to `simplexMcpMain`, so a restarted service edits the same list.
23. Claude edits the list only when its text changed since the last post or edit; the first render after a start always edits.
24. Claude replaces a list that is 23 hours old, or whose edit the core refuses with `invalidChatItemUpdate` or `chatItemNotFound`: it posts the new list, saves its item and time, and deletes the old one for everyone. A refused deletion is logged; the old list then keeps its last text, as after a service stopped for over 30 hours.
25. Claude posts the list with `priority: -1`, the silent priority of `ep/mcp-silent-messages`, as `ep/mcp-activity` posts its step lines. The core and the apps ignore it until they support it.
26. At its stop, the hub renders the list once more before the cores close, with every session in the `⚪` line: the stop cancels every card (fact 12).
27. A reset of the owners creates a new Main group without the list; the list is posted there once an owner joins.
28. The protocol version stays 1. `record` is optional: an older service ignores it, and a session of an older session server shows `running`.
29. The greeting and the command menu stay: `/sessions` still lists the sessions.

## 5. Design

Options for the source of a session's state:
```
option                                 producer                         consumer: hub                    wire                  accuracy
R. Claude Code's record (chosen)       Claude Code writes it at each    reads small files every 2 s      + record in          Claude Code's own state: interrupts,
                                       change; the session server       of attached sessions             POST /session        queued prompts, dialogs, subagents
                                       sends its path
H. turn hooks                          + StopFailure and a              turn start and end times,        Stop and StopFailure  after an interrupt: idle only by an
                                       Notification hook for            ordering rules of async hooks    send answer ""        idle notice 60 s later; a typed prompt
                                       idle_prompt                                                                             with its own turn: idle while it runs;
                                                                                                                               dialogs: unseen
S. step hooks of ep/mcp-activity       depends on that pull request     a step means working             step                  a turn without tools: unseen
```
H costs more at the producer and the consumer, and is still wrong after interrupts and for queued typed prompts (fact 7). S misses turns without tool use and depends on another pull request. R reads an internal file of Claude Code, as 0.3.0 decision 121 reads its transcript; a change of its format shows `running`.

Options for the list:
```
option                                       messages                               edits                cost
L1. one message, edited in place (chosen)    one, and one a day                     one per change       the list moves up behind invitations (Q1 A)
L2. one message, kept last                   one per session start, deleted ones    one per change       Q1 B
L3. one message per session                  one per session start, N a day         one per change       N messages to scan; the old cards stay
L4. /sessions with states only               one reply per request                  none                 no overview without asking
L5. state in each session group's name       none in the Main group                 a profile update     a profile update to every member
                                                                                    per change           of a group for each change
```
L2 and L3 cost more messages and alerts; L4 does not show the state unasked; L5 is not in the Main group and costs a profile update per change.

Options for every session in one message (Q2 B):
```
option                                          size of the message                  cost
F1. a line per running session, one ⚪ line     the running lines and one            the oldest names are counted
    of the others, newest first (chosen)        wrapped line                         once the message is full
F2. a line per session, the oldest cut          a line per session ever started      hundreds of lines to scroll past
F3. several messages, as pages                  up to one message each               more posts and edits; not one list
F4. ranges of numbers for the others            the running lines and the ranges     the names of ended sessions are lost
```
F2 makes the running sessions hard to find among the ended ones, F3 costs a message per page and breaks the one list, and F4 drops the names that `/join N` needs.

Options for when the list is rendered:
```
option                                     cost
P. every 2 s, edit when the text changed   one timer; up to 2 s late
   (chosen)
E. on events                               a file watch per record, and a notification from Actions at each
                                           create and decision, from attach, detach, open, close and rename
```
E needs a watch of the records anyway, since Claude Code writes them without telling the plugin, and adds a call at seven places. P reads the same files and the hub's memory and catches every change.

Changes:
```
file               function or type               change
src/protocol.ts    SessionRequest                 + record?: string
src/session.ts     startSession                   the request gets record (decision 19)
src/service.ts     SessionLike                    + record?: string
                   onSession                      agent.record = request.record
src/hub.ts         constants                      listCheckMs = 2 s; claudeStates, unknownState
                   Session                        + record, + claudeStatus: the last status read; readRecord();
                                                  stateLine(), the line of a running session, section 2, or none;
                                                  attached, a private getter
                   Hub                            + stopped made public, read by Session.attached; + listTimer, listing
                   Hub.open                       starts the list timer, unref'd
                   Hub.showSessions, new          reads the records of the attached sessions; main.showList(sessionsText)
                   Hub.sessionsText(quote?)       the text of decisions 16 and 17, fitted with the quote when given;
                                                  "No sessions." without any session
                   Hub.stop                       clears the timer; waits for a running render; the last render before
                                                  the cores close
                   notRunningLine, new function   the ⚪ line with the first K names and the count of the rest
src/main-group.ts  constants                      listKey simplexMcpList, listAge 23 h
                   SessionGroups.sessionsText     + the quote of the command, for /sessions
                   MainGroup                      + list {item, at} from the custom data; + shown text
                   MainGroup.onMessage, command   the command's content passed as the quote of /sessions
                   MainGroup.showList, new        decisions 22-25, serialized with the group's events; replaceList,
                                                  postList; isRefusedEdit, listOf
                   MainGroup.post                 returns the item's ID; an optional priority
                   MainGroup.replace              the list cleared with the old group
```

Session state:
```
readRecord():
  not attached or no record path: claude = undefined
  read and parse record: claude = its status
    no file: claude = undefined
    unparsable: claude unchanged

cards = actions.pending() without interrupted ones,
        and without permission cards when claude is known and not "waiting"
attached = onLine set and the hub not stopping
running = attached, or cards > 0

line, of a running session:
  cards > 0              🔵 N · NAME: waiting for you, K approval(s)
  claude = "waiting"     🔵 N · NAME: waiting for you in Claude Code
  claude = "busy"        🟠 N · NAME: working
  claude = idle, shell   🟢 N · NAME: idle
  otherwise              ▶️ N · NAME: running
```

Text of the list, and of the reply to `/sessions` with its quote:
```
sessionsText(quote):
  no session: "No sessions."
  lines = the lines of the running sessions, by number
  names = "N · NAME" of the other sessions, by number from the newest
  rest(k) = "K older sessions", "1 older session" for one, with K = the count of names - k
  ⚪(k) = "⚪ Not running: " + the first k names, and rest(k) when K > 0, joined by ", "
  text(k) = lines, then ⚪(k), joined by line breaks; lines alone when names is empty
  k = the largest k for which text(k) fits one message with quote (fact 14), or 0
  text = text(k)
```

Hub and Main group:
```
every 2 s, and once at the stop:
  for each attached session: readRecord()
  main.showList(sessionsText(no quote))

showList(text), after the group's earlier events:
  the group lost: nothing
  no list:  an owner in the group: post text, silent; list = {item, now}; save list; shown = text
  list older than 23 h: replace
  text = shown: nothing
  edit the list to text; shown = text
    refused with invalidChatItemUpdate or chatItemNotFound: replace

replace:
  post text, silent; old = list; list = {item, now}; save list; shown = text
  delete old for everyone; a refusal logged
save list: Main group custom data = {simplexMcpMain: true, simplexMcpList: list}
```

```mermaid
stateDiagram-v2
  state "🟢 idle" as idle
  state "🟠 working" as working
  state "🔵 waiting for you" as waiting
  state "⚪ in the line Not running" as stopped
  [*] --> stopped: the session is created
  stopped --> idle: Claude Code attaches
  idle --> working: record busy
  working --> waiting: a card, or record waiting
  idle --> waiting: a card
  waiting --> working: the card decided, record busy
  waiting --> idle: the card decided, record idle
  working --> idle: record idle or shell
  idle --> stopped: Claude Code ends
  working --> stopped: Claude Code ends
  stopped --> waiting: a card of an ended session
```

A session stays in the list for as long as the data folder keeps it. A session attached while its record can't be read is `▶️ running` in place of idle, working and waiting in Claude Code.

```mermaid
flowchart LR
  CC[Claude Code process PID] -->|status at each change| R["CONFIG/sessions/PID.json"]
  SS[session server, child of PID] -->|POST /session with record| SV[service]
  SV -->|Session.record| H[hub, every 2 s]
  R -->|read| H
  C[cards of each session] --> H
  H -->|text changed| MG[MainGroup.showList]
  MG -->|edit, or a new list after 23 h| M[Main group]
  MG -->|simplexMcpList: item and time| D[Main group custom data]
```

## 6. Compatibility

- Older SimpleX apps: every app version applies edits and deletions for everyone. Apps without silent messages alert for the list's posts: the first one and one a day.
- Older Claude Code: every record on this machine since 2.1.260 has `status` (fact 5). Without a readable record, attached sessions show `▶️ running`.
- Older session servers, of Claude Code sessions started before the update: their `/session` has no `record`, so their sessions show `▶️ running` until Claude Code restarts.
- Older services: a newer hook stops an older service ([client.ts:37-40](../packages/simplex-chat-mcp/src/client.ts#L37-L40)); an older service ignores `record`.
- Older data folders: the Main group has no `simplexMcpList`, so Claude posts a list at the first render while the group has an owner. In a group whose owner joined long ago, it follows the existing messages. Every session stored before the update is in it, in the `⚪` line until its Claude Code attaches.
- `serve`: unchanged; it has no Main group.

## 7. Acceptance and tests

Done when:
```
#   criterion                                                                                checked by
1   after the greeting, Claude posts the list of every session, silent: the running ones    unit: sessions; e2e
    by number, then the ⚪ line, newest first
2   each line follows Claude Code's record: busy, waiting, idle, shell; an attached         unit: sessions
    session without a record is running; a partial record keeps the last state
3   cards count: tool-use cards only while the record says waiting or is unknown;           unit: sessions
    cancelled cards never
4   the list is edited only when its text changes; a restarted service edits the same       unit: sessions
    list, found in the Main group's data
5   a list 23 hours old, or whose edit is refused, is replaced and the old one deleted      unit: sessions
    for everyone; a refused deletion is logged
6   no list without an owner; after a reset of the owners, a new list once an owner joins   unit: sessions
7   at the stop, every session is in the ⚪ line                                             unit: sessions
8   /sessions replies with the text of the list, fitted with its quote                       unit: sessions; e2e
9   the text fits one message: the ⚪ line keeps the newest names that fit and counts the    unit: sessions
    older ones; with no name room, only the count; running lines are never cut
10  the session server sends record from CLAUDE_CONFIG_DIR or ~/.claude and its parent's   unit: session; unit: service
    PID, and the service passes it to the session
11  a real session: working, waiting for you with a card, waiting for you in Claude Code,   TESTING check
    idle after an interrupt, in the ⚪ line after an exit
```

Tests:
- `test/unit/sessions.test.ts`, in "Main group", new:
  - "posts the list of every session after the greeting, silent, and edits it as Claude Code's records and the cards change: working, waiting for you, waiting for you in Claude Code, idle, and the ⚪ line after an exit". It calls `Hub.showSessions`, so it compiles only with this change; the e2e test below fails on master.
  - "fits the list in one message: the ⚪ line keeps the newest names that fit and counts the older ones, only the count when no name fits, and every line of a running session", with 400 stored sessions, with names of 50 four-byte characters, and with the quote of `/sessions`.
  - "counts a tool-use card only while Claude Code shows its dialog or its record is unknown, every other card, and no card cancelled by a stop".
  - "keeps a session's last state while its record is written, and shows running without a record, without its file or with another status".
  - "posts a new list and deletes the old one for everyone when the list is 23 hours old or its edit is refused, logs a refused deletion, and edits the list of the Main group's data after a restart".
  - "posts no list while the Main group has no owner, and a new one in the new Main group after a reset of the owners once an owner joins".
  - "puts every session in the ⚪ line at the hub's stop".
- `test/unit/sessions.test.ts`, changed: "lists the sessions and invites a member to the group of a session as admin by /join" expects the text of the list.
- `test/unit/session.test.ts`, changed: the registered request has `record`, with and without `CLAUDE_CONFIG_DIR`.
- `test/unit/service.test.ts`, changed: "/session passes its record to the session".
- `test/e2e/service.test.ts`, new: "the Main group shows every session, the running ones with their states from Claude Code's record and the cards, edited in place". A session with a record file attaches; the test writes `busy`, starts a `connect` whose card waits, approves it, and writes `idle`; Alice's app gets the list and its updates. It fails on master: no list arrives. The `/sessions` expectations of the first test are changed to the text of the list.
- `TESTING.md`, a real-session check: two sessions show in the list. `sleep 20` with Bash shows 🟠. `Run ls with Bash.` from the group shows 🔵 with 1 approval; after `/approve N` it shows 🟠, and 🟢 when the turn ends. Esc during a turn shows 🟢 within 2 s. A question of Claude in the terminal, such as AskUserQuestion, shows 🔵 in Claude Code. Exiting Claude Code moves the session into the ⚪ line. `/sessions` replies with the same text.
- Run: `npm test --prefix packages/simplex-chat-mcp`, and the end-to-end tests as TESTING.md states; the reduced build runs them in CI. No app changes, so no app test workflows.

## 8. Docs to change

- `README.md:34`: step 3 says that the Main group lists the sessions with their states.
- `REFERENCE.md`, "The Main group" (547-575): the list, its lines, the `⚪` line and its cut, edits, the daily replacement, the stop; the `/sessions` bullet with the text of the list.
- `ROADMAP.md:17`: the line is removed.
- `spec/api.md` 4.2: the `/sessions` rows, a "Posts" row for the list, the custom data `simplexMcpList`; 4.6: `record` of `POST /session`.
- `spec/architecture.md` 8.2: `Hub.showSessions`, `sessionsText`, `stop`, `stateLine`, `Session.record`, `readRecord`, `MainGroup.showList`; 8.3: the memory rows of `Session.record`, `Session.claude` and `MainGroup.list`, the custom data row, and Claude Code's record among the files read.
- `spec/flows.md`: flow 17 ends with the list; a new flow, the list of sessions, with the replacement.
- `TESTING.md`: the automated tests and the real-session check.

## 9. Conflict areas with the other items

```
file                              where                                        items
src/hub.ts                        Session fields, Hub.open, Hub.stop           mcp-activity (steps, endSteps in Hub.stop),
                                                                               mcp-replies, mcp-live-answers
src/service.ts                    SessionLike, onSession                       mcp-activity (endSteps in SessionLike, onAttach),
                                                                               mcp-fixes (runService, readyChannel)
src/session.ts                    startSession request (53)                    mcp-replies (channelInstructions, channelNotification)
src/protocol.ts                   SessionRequest (21-27)                       mcp-replies (AttachLine), mcp-activity (TurnRequest)
src/main-group.ts                 MainGroup, showList                          mcp-threads, when sessions become threads
test/unit/sessions.test.ts        "Main group" (1380-1531)                     mcp-fixes (group name in texts)
test/e2e/service.test.ts          /sessions in the first test (217-229)        mcp-fixes (group name, link tests)
README.md:32-34                   steps 2-3                                    mcp-fixes (Claude Control)
REFERENCE.md "The Main group"     547-575                                      mcp-fixes (Claude Control)
ROADMAP.md                        line 17 and neighbours                       mcp-fixes adds a line; mcp-flat-sessions and
                                                                               mcp-agent-profile remove lines
spec/api.md 4.2, 4.6;             tables several items extend                  mcp-activity, mcp-replies, mcp-fixes
architecture.md 8.2, 8.3
priority of the list's posts      the silent priority                          mcp-silent-messages, mcp-activity
the line of a session             model, effort or usage in the line           mcp-session-usage, if it puts them there
Claude Code's record              the start of work                            mcp-live-answers, which can read the same status
```

## 10. Estimate

```
part                                        source lines   test lines
hub.ts, main-group.ts                       115            210 unit
session.ts, service.ts, protocol.ts         5              20 unit
e2e                                                        50
docs                                        60 lines of text
```
About one working day, with two review rounds.
