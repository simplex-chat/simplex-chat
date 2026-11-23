# SimpleX MCP: the session board in the Main group

Status: renders answered by the user on 2026-11-21 (section 3); implemented on `ep/mcp-session-board`. Code references: master `3682e5459`. Claude Code references: version 2.1.311, its binary `/usr/local/bin/claude`, and the transcripts and config of this machine, read on 2026-11-21. Replaces decisions 15, 16 and 22-24 of [2026-11-13-mcp-session-list.md](2026-11-13-mcp-session-list.md).

## 1. Goal

The maintainer supervises many Claude Code sessions, his own and those of employees, from the Main group. The Main group shows each session's state, but neither the Claude account of a session nor its goal and recap. The user, 2026-11-20:

> each session should be one message with structured information and each change should be an edit to this message, and the last message should be a summary of all sessions (N sessions, 5 running, etc.) as this summary shows up in chat list.

> We don't need to see actual goal, only whether it's set, met or not. Recap as description is better.

> we don't need descriptions if there is no recap I think

Outcome:
- The Main group has one message per session and a summary of all sessions, each edited at each change.
- Each session message and each session group's short description show the account, the model, the effort level and the goal state.
- The session group's description is the latest recap of Claude Code, and empty without one.

## 2. What users see

### 2.1 Main group, "Claude Control"

Four sessions: working, waiting for you, idle and stopped. Each block is one message; the last one is the summary. `*TEXT*` is bold in the apps.

```
🟢 3 · simplex-chat: working
*WebRTC device link architecture survey*
alice@example.com · claude-opus-4-7 · effort high · goal set
I mapped how to move desktop–mobile linking onto WebRTC through the Haskell core and published the WebRTC world map page. Next I need your answers to the four decisions on the page, starting with scope, before I plan the prototype.
```
```
🟠 4 · mcp-board: waiting for you, 2 approvals
*Session board in the Main group*
bob@example.com · claude-sonnet-4-5 · effort medium
```
```
🔴 5 · docs: idle
*Release notes for 6.5*
alice@example.com · claude-opus-4-7 · effort high · goal met
The release notes for 6.5 are written and the pull request is open. Next, you review the wording of the security section.
```
```
⚪ 2 · webrtc: stopped
*WebRTC call reconnection*
carol@example.com · claude-opus-4-7 · effort xhigh · goal failed
```
```
4 sessions ⚪ 🟢 🟠 🔴
```

While no other post follows it, the chat list shows the Main group with the summary as its last message:

```
Claude Control                                   16:52
4 sessions ⚪ 🟢 🟠 🔴
```

Lines of a session message, in this order:
```
line           text                                                  shown when
state          the line of /sessions today: 🟠 waiting for you, K    always; ⚪ N · NAME: stopped for a session
               approvals; ⛔️ waiting for you in Claude Code;           that has a message and neither a card nor
               🟢 working; 🔴 idle; ⚙️ working in the background;        an attached Claude Code
               ▶️ running
title          *AI TITLE*, Claude Code's title of the conversation    the transcript has one, and it differs from NAME
details        ACCOUNT · MODEL · effort LEVEL · goal STATE            any part known; unknown parts left out
recap          the latest recap, without "(disable recaps in /config)" the transcript has one
```

Summary (Q3): `N sessions` and the state emoji of each session with a message, by session number, separated by spaces; `1 session` for one; `No sessions.` without a message.

Messages and the summary change only by edits, when their text changes (Q5). A new running session's message is posted after the summary, which then is no longer last; a later protocol option that moves an edited message to the bottom will restore that. A message whose edit the core refuses, with `invalidChatItemUpdate` or `chatItemNotFound`, as after the 24-hour edit window, is deleted for everyone and posted again; so is the summary. Session 3 re-posted:

```
⚪ 2 · webrtc: stopped
...
(message deleted)                                ← the old message of session 3, where the group's full deletion is off
🟠 4 · mcp-board: waiting for you, 2 approvals
...
🔴 5 · docs: idle
...
4 sessions ⚪ 🟢 🟠 🔴
🟢 3 · simplex-chat: working
*WebRTC device link architecture survey*
alice@example.com · claude-opus-4-7 · effort high · goal set
I mapped how to move …
```

A stopped session's message is deleted for everyone 23 hours after its post, within the 30-hour window of deletion. A stopped session that starts again gets its message edited, or a new one when its message was deleted.

While subagents or workflows work, the message has a line `🤖 2 agents working · 1 stalled 2:10 · ⚙️ review-changes Verify 3/8` after the details, from the work of [2026-11-21-mcp-subagents.md](2026-11-21-mcp-subagents.md), which this pull request includes. The session group's descriptions are updated at most once a minute.

`/sessions` replies as today: one line per running session and the ⚪ line of the others.

### 2.2 Session group "3 · simplex-chat"

Group info in the apps:
```
3 · simplex-chat
alice@example.com · claude-opus-4-7 · effort high · goal set          ← short description
─────────
I mapped how to move desktop–mobile linking onto WebRTC through the   ← description, "read more"
Haskell core and published the WebRTC world map page. Next I need
your answers to the four decisions on the page, starting with scope,
before I plan the prototype.
```

Before the first recap the description is empty. A new group is created without a description; an existing group's description `The Claude Code session N. …` is removed at its first profile update after the change.

Each change of the short description or the description shows `updated group profile` in the session group, as each model change does today ([Subscriber.hs `xGrpInfo`](../src/Simplex/Chat/Library/Subscriber.hs), `sameGroupProfileInfo` in [Internal.hs](../src/Simplex/Chat/Library/Internal.hs)).

## 3. Questions for the user

Answered by the user on 2026-11-21:
```
Q1  the stopped state       ⚪ N · NAME: stopped
Q2  the title line          *AI TITLE* when it differs from NAME
Q3  the summary             one state emoji per session, separated by spaces, in session order: "4 sessions 🟠 🟢 ⛔️ ⚙️"
Q4  the old description     removed at the next profile update
Q5  the summary             only edited, at a change; never posted again, except after a refused edit;
                            board messages likewise change only by edits
Q6  spec pointer comments   kept, as the codebase does
```

## 4. Decisions

Facts:
1. Claude Code's global config is `${CLAUDE_CONFIG_DIR || homedir}/.claude.json`: `configPath:P(process.env.CLAUDE_CONFIG_DIR||ce(),\`.claude${X2()}.json\`)`, where `X2()` is the OAuth file suffix, empty in production; a non-empty legacy `CONFIG/.config.json` is read first (binary, `F()` and `Rpr()`). Its `oauthAccount.emailAddress` is the account's email; a session with an API key has no `oauthAccount`. On this machine the file has 92 KB and mode 0600, and Claude Code rewrites it during a session.
2. A goal is recorded as transcript entries `{type: "attachment", attachment: {type: "goal_status", …}}` (binary, `tnr`, `dEt`, `Y3e`, `P_n`, `h4o`):
   - set, or still active after a check: `met: false`, with `sentinel: true` when set or restated;
   - met: `met: true` without `sentinel`;
   - failed: `met: false, failed: true`;
   - cleared by `/goal clear` (`dEt`), or no longer active at a restatement (`Y3e`): `met: true, sentinel: true`.
   Claude Code's own active goal is the last such entry when neither `met` nor `failed` (`P_n`).
3. A recap is a transcript entry `{type: "system", subtype: "away_summary", content}` (binary, `yzs`). Claude Code generates it from the terminal's focus and the transcript, skipped while background work is pending, and appends ` (disable recaps in /config)` to the first recaps of a process (`this.#a<lGo`). No hook runs at its write.
4. Claude Code's title is a transcript entry `{type: "ai-title", aiTitle}`, repeated through the transcript.
5. A transcript grows to hundreds of megabytes: this session's has over 512 MB.
6. Every hook input has `transcript_path` (hook.ts `TurnInput`; Claude Code passes it to SessionStart too).
7. A profile change of a group creates an `updated group profile` item for each member when anything but the preferences changes (fact of section 2.2).
8. The core edits a message within 24 hours of its post and deletes it for everyone within 30 hours (2026-11-13 plan, fact 8).

Choices:
9. The service reads the account, the goal, the recap and the title of each attached session every 2 s, with the record status (`Session.readRecord`, with `readStatus` and `readClaudeCode`).
10. Account: the session server sends `globalConfig`, the absolute path of fact 1 without the legacy file, in `POST /session`, next to `record`. The service reads the file when its modification time differs from the last read, and takes `oauthAccount.emailAddress`; a missing file, a missing field or a file that does not parse leaves the account unknown, and a partial write keeps the last value.
11. Transcript: the SessionStart hook sends `transcript`, its `transcript_path`, in `/title`; SessionStart runs at a start, a resume, `/clear` and a compaction. The session keeps the latest. The service reads only the bytes appended since its last read, from the last line break read; at a new path, and when more than 4 MiB were appended, it reads the last 4 MiB and drops the first partial line. Only lines that contain `"goal_status"`, `"away_summary"` or `"ai-title"` are parsed, and entries with `isSidechain: true` are skipped. A file shorter than the offset is read again from its last 4 MiB.
12. Goal state, from the last goal entry read (fact 2): `met: true` with `sentinel` → none; `met: true` → met; `failed: true` → failed; else → set. At a new transcript path the goal, the recap and the title are cleared.
13. Recap: the content of the last recap read, without the suffix of fact 3, trimmed, cut to 1,000 characters with `…`.
14. The account, the goal, the recap, the title and the transcript path are stored in `session.json`, as the model and the effort level are, so a restarted service shows them before the session attaches.
15. Details text, shared by the session message and the short description: `ACCOUNT · MODEL · effort LEVEL · goal STATE`, unknown parts left out. The session group's profile is updated when the details or the recap change: short description = details, description = recap, or none without a recap.
16. Board: one message per session that is running or has a message, by `MainGroup.showBoard`, every 2 s and at the stop. The items and their times are kept in the Main group's custom data as `simplexMcpBoard: {summary?: {item, at}, sessions: {session, item, at}[]}`.
17. A session message is posted, with priority -1, for a running session without one, while the group has an owner. It is edited when its text changed. When the core refuses its edit, the old one is deleted for everyone and, for a running session, a new one is posted. A stopped session's message is deleted 23 hours after its post. A message of a session the hub does not know is deleted.
18. The summary is posted with priority -1 once, at the first render with a session message while the group has an owner. Afterwards it is only edited when its text changed; when the core refuses the edit, it is deleted for everyone and posted again (Q5).
19. The custom data is saved once per render, after its posts. When the save fails, the items posted in that render are deleted for everyone and the error is logged; the next render tries again.
20. The old list, `simplexMcpList`, is deleted for everyone at the first render, and its key removed.
21. A reset of the owners creates a new Main group without the board; the board is posted there once an owner joins.
22. `/sessions` keeps its reply ([api.md 4.2](../packages/simplex-chat-mcp/spec/api.md#main-group)).
23. The protocol version stays 1: `globalConfig` and `transcript` are optional, and an older service ignores them.

## 5. Design

Where the account is read:
```
option                                   cost
S. the service, path from the session    a stat per session every 2 s, a parse per write of the file;
   server (chosen)                       follows a /login during the session
H. each hook                             a 92 KB parse per tool use; nothing while idle
P. the session server, once at start     misses a /login during the session
```

Where the goal, the recap and the title are read:
```
option                                   cost
S. the service, appended bytes every     a stat and the appended bytes per attached session every 2 s
   2 s (chosen)
H. the Stop hook                         misses the recap, written without a hook (fact 3); a tail read per turn
```

Where the board's items are kept:
```
option                                   cost
M. Main group custom data (chosen)       one save per render with posts; cleared with the group at a reset
G. each session group's custom data      items of one group kept in another; stale after a reset of the owners
```

Message flow:
```mermaid
sequenceDiagram
  participant CC as Claude Code
  participant F as record, config, transcript
  participant K as hooks
  participant SS as session server
  participant S as service, every 2 s
  participant G as session group
  participant M as Main group
  SS->>S: POST /session record, globalConfig
  K->>S: POST /title with transcript
  CC->>F: status, oauthAccount, goal_status, away_summary, ai-title
  S->>F: read status; config when changed; transcript appended bytes
  S->>G: details or recap changed: short description, description
  alt no message of a running session
    S->>M: post the session message
    S->>M: no summary yet: post the summary
  else text changed
    S->>M: edit the session message
    S->>M: edit the summary when its emojis changed
  else edit refused
    S->>M: delete the old one; post again unless stopped
  else stopped, 23 h after the post
    S->>M: delete the message
  end
  S->>M: save simplexMcpBoard
```

Render:
```
showBoard(sessions by number):
  the group lost: nothing
  the old list: delete it
  for each session:
    entry = board.sessions[session]
    no entry:                running and an owner: post
    stopped, 23 h old:       delete
    text = shown:            keep
    edit; refused:           delete; running: post
  for each entry of a session the hub does not have: delete
  summary = "N sessions" + the emojis of the sessions with a message
    none: a message and an owner: post
    text ≠ shown: edit; refused: delete, post
  save board when it changed; failed: delete this render's posts, log
```

Session state, `Session.readRecord`:
```
not attached or hub stopped: status cleared; the rest kept
record      → status                       as today
config      → account                      mtime changed: parse; error keeps the last value
transcript  → goal, recap, title           appended bytes from offset (decision 11)
changed     → store in session.json; control.describe(details, recap)
```

Changes:
```
file                 change
src/protocol.ts      SessionRequest + globalConfig?; TitleRequest + transcript?
src/session.ts       startSession: globalConfig
src/hook.ts          sessionStart: transcript
src/service.ts       SessionLike + globalConfig?, transcript?; onSession, applyTitle set them
src/claude-code.ts   new: AccountReader, TranscriptReader (goal, recap, title)
src/files.ts         SessionFile + account, goal, recap, title, transcript
src/hub.ts           Session.readRecord with readStatus and readClaudeCode, state, details, describe,
                     boardSession; Hub.showSessions: main.showBoard; the session group without description
src/main-group.ts    showBoard replaces showList; summaryText; simplexMcpBoard; the old list deleted
src/control.ts       describe(shortDescr, description)
```

## 6. Compatibility

- Older session servers send no `globalConfig`: the account is unknown until Claude Code restarts.
- Sessions started before the update send no `transcript`: goal, recap and title stay unknown until their next SessionStart.
- Older Claude Code without goals or recaps: those parts are left out.
- Older data folders: the old list is deleted; the board is posted at the first render with an owner.

## 7. Acceptance and tests

```
#   criterion                                                                   checked by
1   the account follows the global config, also after a write; a partial        unit: claude-code
    write keeps the last value
2   goal state from the last goal entry: set, met, failed, none after clear     unit: claude-code
3   recap without the suffix; only appended bytes are read; a new path clears   unit: claude-code
4   session server sends globalConfig with and without CLAUDE_CONFIG_DIR;       unit: session, hook, service
    SessionStart sends transcript; the service passes both to the session
5   short description and description follow details and recap                  unit: sessions
6   one message per running session and the summary; edits on change            unit: sessions; e2e
7   re-post after a refused edit; a stopped session's message deleted at 23 h   unit: sessions
8   the old list deleted; the board saved in the Main group's data              unit: sessions
```

## 8. Docs to change

- README.md: step 3, the board.
- REFERENCE.md, "The Main group": the board, its lines and summary; the session group's descriptions.
- spec/api.md 4.2, 4.3, 4.6; spec/architecture.md: `readState`, `showBoard`, `claude-code.ts`; spec/flows.md: the board flow.
