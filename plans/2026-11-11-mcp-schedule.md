# SimpleX MCP server: scheduled prompts

Status: draft for review. Decided on 2026-11-11. Code references: master `2205de390f`.

## Decisions

1. The server holds the schedules and returns each due prompt in `receive` of its chat, for an agent that calls `receive` of the chat in a loop.
2. Owner commands in the agent control chat: `/schedule CHAT CRON PROMPT`, `/unschedule CHAT N` and `/schedules`; accepted from approvers only, in both modes. A deployment needs no configuration change.
3. Cron syntax: 5 fields, numbers only, vixie cron's rule for the two day fields, in the local time of the server process.
4. The schedules of a chat are stored in its custom data: at most 10, each prompt at most 2000 characters; they are deleted with the chat.
5. A schedule applies while its author is an approver, as a standing approval does.
6. One due prompt per schedule, kept in memory; the minutes while the server is stopped are skipped.
7. Its own pull request from master, not stacked.
8. Budget: about 140 changed lines in `src`, at most 20% more; the user accepted the 201 changed lines after the review rounds.
9. Plugin: the group of each session accepts the same commands for the session's chats, and `receive` of the chat returns its due prompts, as in `serve`. Delivery through the channel is left to a later change.

## Owner commands

```
command                       effect
/schedule CHAT CRON PROMPT    adds schedule N of CHAT, N its position in the chat's list
/unschedule CHAT N            removes schedule N of CHAT; the later ones are renumbered
/schedules                    lists the schedules of the agent's chats
```

Parsed by `ownerRequest`:
```
command       parameters
/schedule     CHAT: a chat reference
              CRON: the next 5 words, joined by single spaces
              PROMPT: the rest of the line
/unschedule   CHAT: a chat reference
              N: digits
/schedules    none
```
- Another form gets the commands reply: `Commands: /approve, /reject, /receive, /forward, /revoke, /leave, /delete, /pending, /list, /schedule, /unschedule, /schedules. Each card shows its commands.`

Replies:
```
request                       reply
/schedule CHAT CRON PROMPT    Schedule 1 of bot SimpleX Directory (@4): 0 7 * * 1-5, server time UTC+02:00. /'unschedule @4 1'
/unschedule CHAT N            Schedule 1 of bot SimpleX Directory (@4) is removed.
/schedules                    @4  1  0 7 * * 1-5  Find trail shoes in size 44.
                              #2  1  0 7 * * *  Check the trail report.
/schedules, none              No schedules.
```

Refusals, in the order of the checks:
```
situation                                        reply
sender not an approver                           Only owners approve actions. (--owners: Only the owners listed in --owners approve actions.)
CHAT not a chat of the agent, or the agent       No chat @99. /list shows the agent's chats.
control chat
a word of CRON not of the forms of section Cron  Schedule: 5 cron fields expected, such as 0 7 * * *.
a field out of range                             Schedule: cron field 8 is out of range.   (the field as typed)
PROMPT over 2000 characters (code points)        Schedule: the prompt is over 2000 characters.
CHAT with 10 schedules                           Schedule: bot SimpleX Directory (@4) has 10 schedules; /'unschedule @4 N' removes one.
/unschedule, N not a schedule of CHAT            @4 has no schedule 3.
```

- `/schedules`: the chats in the order of `/list`, then N; the prompt on one line; each line cut to 100 bytes with "…", as `/list` lines; the first 100 lines, then "K more schedules are not shown." ("1 more schedule is not shown."). Schedules that do not apply are listed too.
- The command menu: `/schedules`, "Scheduled prompts", after `/list`; the menu of an existing agent control chat is updated at start (`load`).
- Log lines, names on one line: `Schedule 1 of @4 added by Alice: 0 7 * * 1-5.`, `Schedule 1 of @4 removed by Alice.`
- `/approve CHAT all`, `/revoke CHAT`, `/receive CHAT N all` and `/receive CHAT off` keep the schedules in the custom data; `/schedule` and `/unschedule` keep the approvals.

Changes:
```
file          function            change
control.ts    controlCommands     /schedules "Scheduled prompts"
              commandsReply       the schedule commands
              OwnerRequest        schedule
                                  unschedule
                                  schedules
              ownerRequest        the three forms above
              ownerCommand        the three commands
              addSchedule         new
              removeSchedule      new
              schedulesText       new: the /schedules reply
              schedulesOf         new: the valid entries of simplexMcpSchedules
              utcOffset           new: UTC±HH:MM of the process's time zone
              isCurrentApprover   new: true for the member ID of a current approver, false while the control chat is lost; used by standingApprover and Schedules
              load                the schedules of the agent's chats, after the members
              constructor         schedules
schedule.ts   Schedules           new
              cronError           new
              cronMatches         new
              fieldValues         new
serve.ts      startAgent          one Schedules, passed to AgentTools and Control; started after control.start; stopped at the stop, before the actions
hub.ts        Session             one Schedules per session, passed to AgentTools and Control
              Hub.session         schedules started after control.load
              Hub.stop            schedules stopped before the actions
tools.ts      Agent               schedules, optional
              receiveOutput       scheduled
              receiveDescription  the sentence on scheduled
              receive             a due prompt ends the wait; scheduled taken after markRead
              leaveChat           the chat's schedules in memory dropped
```

## Storage

Key `simplexMcpSchedules` of the chat's custom data, an array of entries:
```
field    value
cron     the 5 fields, joined by single spaces
prompt   one line, 1 to 2000 characters
by       the approver's display name
byId     the approver's groupMemberId
at       ISO time of the addition; the key of the entry's due prompt in memory
```
- The other keys of the custom data are kept. Writes: `apiSetContactCustomData` or `apiSetGroupCustomData`, as approvals.
- Read at start, after the members, unless the control chat is lost: the chats of one `apiGetChats` read (`agentChats`).
- Valid entries: the five fields, byId a number and the others strings, and a cron without an error. Other entries are left out, and dropped at the next write of the chat's schedules.
- At `leave` and `/leave`, the chat is deleted with its custom data (`apiDeleteChat`), and its schedules in memory are dropped (`leaveChat`).

## Cron

```
field          range
minute         0-59
hour           0-23
day of month   1-31
month          1-12
day of week    0-7; 0 and 7 are Sunday
```

```
item     values
*        the field's range
N        N
A-B      A to B
*/S      every S-th value of the range, from its start
A-B/S    every S-th value from A to B
```
- A field is an item or a comma list of items, numbers only.
- Out of range: a value outside the field's range, A above B, or S equal to 0.
- Day of month and day of week: when neither field starts with `*`, a day matched by one of them suffices, as in vixie cron; otherwise a day is matched by both.
- Times: the local time of the server process, in the time zone of TZ, otherwise of the system. The reply to `/schedule` names the UTC offset at that moment.
- A local minute skipped by a daylight saving change is not matched; a repeated one is matched twice.

## Delivery

`Schedules` (`schedule.ts`):
```
method               effect
set(chat, entries)   the chat's entries replaced; the due prompt of a current entry with the same at is kept
start()              an unreferenced timer at the next minute boundary
stop()               the timer cleared
listen(chat, wake)   wake called when a prompt of chat is made due; result: the function that ends it
take(chat)           the due prompts of chat, in entry order; they are cleared
pending(chat)        the count of due prompts of chat
```

At minute boundary B:
```
each chat, each entry N of it:
  entry without a due prompt, member byId an approver (Control.isCurrentApprover), cron matched by the local time of B:
    due = {prompt, at: B as YYYY-MM-DD HH:MM UTC}
    log "Scheduled prompt N of CHAT is due."
    wake the listeners of CHAT
the next timer: the first minute boundary after max(now, B)
```
- When a timer fires late, the schedules are matched at B, the minute it was set for; the minutes until the next boundary after the firing are skipped. After a timer that fires early, the next one is set after B, so each minute is matched once.
- Due prompts are kept in memory: they are dropped at a restart.
- `serve.ts`: one `Schedules`, with `control.isCurrentApprover` as its approver check; passed to `AgentTools` (`schedules`) and `Control`; started after `control.start()`, when HTTP answers calls; stopped at the stop, before the actions.
- `hub.ts`: one `Schedules` per session, with the session's `control.isCurrentApprover`; passed to its `AgentTools` and `Control`; started after `control.load()` in `Hub.session`; stopped at `Hub.stop`, before the actions.

```mermaid
sequenceDiagram
  participant O as Owner
  participant C as Control
  participant S as Schedules
  participant T as AgentTools
  participant A as Agent
  O->>C: /schedule #15 0 7 * * 1-5 Post the trail report.
  C->>C: custom data of #15: simplexMcpSchedules with entry 1
  C->>S: set(#15, entries)
  C->>O: Schedule 1 of group ... (#15): 0 7 * * 1-5, server time UTC+01:00. /'unschedule #15 1'
  A->>T: receive {chat: #15, wait: 3600}
  T->>S: listen(#15)
  Note over S: 07:00 local time: entry 1 matched
  S->>T: wake
  T->>T: markRead; the call not aborted
  T->>S: take(#15)
  T->>A: {chat: #15, messages: [], more: 0, scheduled: [{prompt: "Post the trail report.", at: "2026-11-11 06:00 UTC"}]}
```

## receive

Output field `scheduled`, an array:
```
field    value
prompt   the prompt
at       the minute it was made due, YYYY-MM-DD HH:MM UTC
```
- Present only when a prompt of the chat is due; `required` is unchanged.
- A call with a due prompt of its chat returns at once, as with a new message; a waiting call ends when a prompt of its chat is made due.
- The prompts are taken after `markRead` and the abort check after it: a call that ends without a result leaves them due.
- Schema descriptions:
  ```
  field              description
  scheduled          Prompts your owner scheduled for this chat that are due; present only when one is due.
  scheduled.prompt   The prompt, written by your owner.
  scheduled.at       The minute it became due, in UTC.
  ```
- `receiveDescription`, a third line: "scheduled holds the prompts your owner scheduled for this chat in the agent control chat; handle each as a prompt from your owner and answer in this chat."
- The server instructions are unchanged.

## Threat model

```
actor                          capability                                     mitigation
approver                       prompts for the agent in any of its chats      the trust of an approver; the agent's actions after a prompt follow the approval mode, as any action
approver                       storage in the chat's custom data              10 schedules per chat, 2000 characters each
former approver                schedules added before the removal             skipped while byId is not an approver
other member of the agent      commands, also those tappable in /schedules    accepted from approvers only
control chat                   lines
peer of CHAT                   messages in CHAT, unchanged                    the server returns prompts in scheduled, apart from the chat's messages
```
- While the control chat is lost, every schedule is skipped and owner commands are ignored.

## Tests

- `test/unit/schedule.test.ts`, new:
  - cron forms, ranges and errors
  - day of week 0 and 7
  - vixie cron's rule for the day fields
  - matching in local time
  - checks at minute boundaries, with the due log line
  - one due prompt per entry
  - due prompts kept for entries set again, dropped for removed ones
  - the approver rule
  - listeners
  - one timer, none after the stop
  - a timer firing early or late
- `test/unit/commands.test.ts`:
  - the replies of `/schedule`, `/unschedule` and `/schedules`
  - every refusal
  - malformed schedule commands answered with the commands reply
  - renumbering, and the due prompt of a removed schedule dropped
  - the first 100 lines of `/schedules` and the count of the others
  - other custom data and the approvals kept
  - log lines with names on one line
  - mode host
  - the load at start, with entries of another form and an author who is not an approver
  - a lost control chat
  - the schedules of a chat left by `/leave`
  - the command menu with `/schedules`
- `test/unit/messages.test.ts`, `receive`:
  - scheduled returned once, with `wait` 0, and at once with `wait`
  - a waiting call ended by a due prompt
  - the prompts of other chats left due
  - an aborted call leaving them due
- `test/unit/http.test.ts`: the receive description and output schema; scheduled in `structuredContent`.
- `test/unit/fixtures.ts`: `clockStart`, `dueAt`, `scheduleEntry`.
- `jest.config.js`: TZ `Pacific/Chatham`, a zone with a 45-minute offset.

## Docs

- `REFERENCE.md`:
  - the schedule commands
  - the receive field `scheduled`
  - the log lines
  - the time zone
  - a short paragraph with an example, in section "Prompt the agent from the app"
- `TESTING.md`: a step with `* * * * *`.
- `plans/2026-10-19-mcp-server.md`: sections Schedules and Scheduled prompt; the schedule rows and lines of the other sections; decisions.
- `spec/`: api, architecture, flows and README, with line anchors remapped from `git diff -U0 2205de390f`.
- `package.json`: version unchanged.
