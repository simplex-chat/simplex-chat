# SimpleX MCP: one Claude Code process per conversation

## Problem

Two Claude Code processes on one conversation, as after a second `claude --continue`, share its SimpleX group: both get each prompt of the group, and both post their answers there.

## Rule

On Linux and macOS, while the Claude Code process of a session's last `/session` runs, the service refuses the requests of another process for that session with status 423 and TEXT:

`Claude Code process PID already uses the SimpleX group of this conversation. Exit that process, or exit this one and start a new conversation.`

| Request of the other process | Sender | Where TEXT is shown |
|---|---|---|
| `POST /title`, applied at once | SessionStart hook | the session start: `SimpleX MCP: TEXT` |
| `POST /title`, held for its session server | SessionStart hook | the service log; the reply comes at once |
| `POST /session` | session server | the result of each SimpleX tool |
| `POST /turn` | turn hook | the hook's stderr |
| `POST /turn` | display hook and status line command | the reply is dropped |
| `POST /ask` | PermissionRequest and Elicitation hooks | the hook's stderr; Claude Code shows its own dialog |

- A process runs while its PID has the start time read at its `/session`; a reused PID has another start time. On macOS the start time is read in the C locale and UTC.
- A request without a PID is served, and so is a request while the recorded process has no start time, as on other systems.
- When the recorded process changes during the read of its start time, the check runs again, so of two registrations at once only one is served.
- The session server sends `/attach`, `/call` and `/permission` only after a `/session` that succeeded.
- The title of the other process is recorded before the refusal, so its session server resolves to the same session.
- A refused process is mapped to the session for 60 s, longer than the 30 s between registrations of its server, so a held title of a forked ID, as with `--fork-session`, and a title after `/clear` in the refused process resolve to that session and are refused; no session is opened for those IDs.
- The last refused title of a process is applied when that process registers the session.
- A registration that changes the recorded process writes its PID and start time into `session.json` as `claudeProcess`, or removes them when the start time is unknown; a new entry starts with them, so the recorded process keeps the session across a service restart.
- The session server registers again at most 30 s apart; its first registration after the first process ended is served.

```mermaid
sequenceDiagram
  participant P as Claude Code process P
  participant Q as Claude Code process Q
  participant D as Service
  P->>D: POST /session {id: R, pid: P}
  D->>D: entry R and session.json: pid P, start time of P
  Q->>D: POST /title {id: R, pid: Q}
  D->>D: P has its start time: refusal of Q for R, with its title, 60 s
  D-->>Q: 423 TEXT, shown at the session start
  Q->>D: POST /session {id: X, pid: Q}
  D->>D: X aliased to R; P has its start time
  D-->>Q: 423 TEXT, returned by each SimpleX tool
  Q->>D: POST /turn or /ask {id: R, pid: Q}
  D-->>Q: 423
  Note over P,D: P ends
  Q->>D: POST /session {id: X, pid: Q}, at most 30 s later
  D->>D: the start time of P differs: entry R and session.json: pid Q
  D->>D: Q's refused title applied
  D-->>Q: the session
```

## Changes

- `src/service.ts`: `Service.refuseOtherProcess`, called by:
  - `onSession`
  - `applyTitle`
  - `onAsk`
  - the `/turn` route
- `src/service.ts`: `refusals`, read by `processSession`; the refused title applied by `onSession`.
- `src/service.ts`: `claudeProcess` written by `onSession` and read by `entry`.
- `src/service.ts`: `processStart` runs `ps` with `LC_ALL=C` and `TZ=UTC`.
- `src/files.ts`: `SessionFile.claudeProcess`.
- The refusal in:
  - `spec/api.md`, socket protocol
  - `spec/architecture.md`, `onSession`, `processStart`, `SessionFile`
  - `spec/flows.md`, flow 27
  - `REFERENCE.md`, tool approvals, plugin recovery, data folder
  - `TESTING.md`, check 13

## Tests

`test/unit/service.test.ts`:
- A second process: `/title`, `/session` with a startup ID, `/turn` and `/ask` refused with the PID of the first; the hooks of the first process with a new session ID served.
- A second process served after the first ended, and after the PID of the first passed to a later process.
- Two processes that register at once after the first ended: one refused.
- A forked ID's held title from a second process refused after its session server's refusal, with no session for that ID.
- A title of a new ID from a refused process 20 s after its server's refusal: refused, with no session for that ID.
- The refused title of a second process applied when it registers after the first ended.
- A second process refused after a service restart, and served once the first ended.
- The tests of a second registration of a session: the first process ended before it.
