# SimpleX MCP server 0.3.0: live status message and inference speed

Status: draft for review. Design of 2026-11-10; Q8 (groups) and Q18 (size budgets) decided on 2026-11-11; the other questions as recommended. Code references: master `f8776692df`; library references: `packages/simplex-chat-nodejs/src/api.ts` at `f8776692df`, published as `simplex-chat` 7.1.0-beta.10.2.

Dependencies:
1. `LiveMessage` of `simplex-chat` 7.1.0-beta.10.2 (`api.ts:1134-1214`), used unchanged, with interval 0.
2. With `--inference`: llama-swap, or one llama-server, at the URL.

## Decisions

1. Q1: the answer is a new message; the status message is deleted for everyone after `apiSendMessages` of the answer returns.
2. Q2: the "marked deleted" stub is accepted. In a direct chat it is removed when the owner sets "Delete for everyone" to "always" for the agent contact; in a group, when full deletion is on in the group's preferences for the agent's role.
3. Q3: frames ◐ ◓ ◑ ◒; ◌ in the frozen line.
4. Q4: the labels of section Text and labels.
5. Q5: gaps of a tenth of the age, from 3 s to 20 s; 50 renders, about 10 minutes; 59 with notes; 23 hours.
6. Q6: in the frozen phase, the status is deleted 30 minutes after the last start or end of a call; running `receive` calls are left out.
7. Q7: leftovers of a crash are deleted at start in eligible chats when younger than 30 h − 10 min; a chat that lost its approval keeps its leftover.
8. Q8, decided 2026-11-11 in place of the recommendation: direct chats with people, groups and business chats are eligible; channels and direct chats with bots and agents are excluded. The status is started at each `receive` with `typing` that returns messages, from any sender. Every member sees the status, its labels and its notes.
9. Q9: `typing` is an input of `receive` only; in the README prompt, each answer is sent with `wait 0`.
10. Q10: notes are shown only in the status message of their chat; each note is printed in the terminal.
11. Q11: the key is read from `SIMPLEX_MCP_INFERENCE_KEY`, visible ASCII only, and sent to URL only.
12. Q12: `/props` and `/slots` are read at proxies on the URL's host, and at loopback proxies of a loopback URL.
13. Q13: the model ID is shown in the loading, prompt and output labels when one model is busy, filtered by `^[\w.:/+@-]{1,64}$`.
14. Q14: the post of the status is accepted with its notification, as a new item.
15. Q15, changed by the group decision: `postingRefusal` is extracted from `send` and shared with `showsStatus`.
16. Q16: spec anchors are rewritten by a script; each rewritten anchor is checked against its recorded target text.
17. Q17: version 0.3.0; pull request title `mcp server: live status while the agent works`.
18. Q18, decided 2026-11-11: `status.ts` 192 lines, `inference.ts` 164 lines; up to 20% above them is allowed.
19. Q19: `instructions()` stays unchanged; its length for display names over 15 characters is a separate change.
20. Q20: the agent starts a status with `typing` or a note; the owner ends the statuses of a chat with `/revoke CHAT`, together with the approval of answers. An owner command for statuses alone is a later change, if wanted.
21. Q21: `--inference` with `--approve host` is a usage error.
22. Q22: a failed command of a status is logged, and the status is deleted; at a later note, a new status is started.

## Status message

Chats:
```
chat                                                    status message
direct chat, contact peer type neither bot nor agent    shown
group                                                   shown
business chat                                           shown
channel                                                 refused
direct chat with a bot or an agent                      refused
```

Conditions, read at each tick, at each note and at the start scan (`showsStatus`):
```
condition       rule
connection      isConnected: contact connection ready; membership connected or complete
role            groups: other than observer and relay (postingRefusal)
leave           the chat not in AgentTools.leaving
kind            chatSubject(chatInfo).kind other than bot, agent and channel
mode            requests
control chat    actions.lost unset
approval        standingApprover(chatInfo, "messages") defined
```

### Text and labels

```
part      value
frame     ◐ ◓ ◑ ◒; render n has frame (n − 1) mod 4
label     the first row of the labels that applies
elapsed   M:SS since creation
line      FRAME LABEL · ELAPSED, renders 1 to 49
frozen    ◌ working since YYYY-MM-DD HH:MM UTC, from render 50
text      LINE, or LINE + "\n" + NOTE
```

```
state                                                         label
loading: a model starting or answering 503, or one stopping   loading `MODEL`, or loading the model
  while none is ready
prompt: processing slots, none with n_decoded > 0             reading the prompt[ with `MODEL`][, N tokens read]
output: a processing slot with n_decoded > 0                  generating[ with `MODEL`][, N tokens/s]
an action pending with open > 0                               waiting for approval
a running call other than receive                             using TOOL, of the latest one
idle: every slot read is idle, or the server sleeps           working
otherwise                                                     thinking
```

- The first render is `◐ thinking · 0:00`. The label is read again for renders 2 to 49; a note is rendered with the last label.
- `N tokens read` is shown above 0; `N tokens/s` is rounded.

### Schedule

```
rule            value
first render    after the first read of the conditions; with a note, at once
gap             min(max(age / 10, 3 s), 20 s) while renders < 50
frozen          render 50; then a tick every 5 min, without a render
notes           rendered at once while renders < 59
messages        at most 60: 59 renders and the deletion
lifetime        23 h; at a note after it, the status is deleted and a new one is started
```

```
age          renders
15 s         6
30 s         11
1 min        18
2 min        25
3 min 42 s   32, after the first 20 s gap
5 min        35
9 min 42 s   50, the frozen line
```

- Each tick is scheduled when the previous one ends, so the real counts are at or below the table.
- `LiveMessage` with interval 0: the first update posts the item (`/_send CHAT live=on json ...`); later updates (`/_update item CHAT ID live=on json ...`) are queued at the next macrotask, merged within it, and skipped when the text is unchanged (`api.ts:1160-1171`, `:1209`).

### Lifecycle

```
Status
live        LiveMessage of the chat, interval 0, onError: failed
started     Date.now() at creation
renders     0 to 59
label       label of the last render; "thinking" at creation
note        text of the last send with live: true
timer       the next tick, unref'd
```

```
state     statuses.get(chat)   renders    endings
none      undefined
active    Status               0 to 49
frozen    Status               50 to 59
ending    undefined                       its live.end("")
```

```
event                                  condition                                   effect                                         state
receive returns messages, typing true  no status, not stopping                     created; tick at once                          active
first tick                             conditions unmet, or their read fails       end, before any post                           ending
send live: true, accepted              no status, or one 23 h old, ended first     created; note rendered; tick in 3 s            active
send live: true, accepted              renders < 59                                note rendered                                  active | frozen
send live: true                        renders = 59                                tool error                                     frozen
tick                                   conditions unmet, or their read fails       end                                            ending
tick                                   age ≥ 23 h                                  end                                            ending
tick                                   renders ≥ 50, every running call a          end                                            ending
                                         receive, 30 min since lastCall
tick                                   renders < 50                                label read for renders 2 to 49; render;        active | frozen
                                                                                     next tick
tick                                   renders ≥ 50                                tick in 5 min                                  frozen
tick after an await                    the status ended or replaced                return                                         unchanged
a receive of the chat waits            a status                                    end                                            ending
send performed                         a status of its chat                        end, after apiSendMessages returns             ending
leave of the chat, or the owner's      a status                                    end, awaited before the chat is deleted        ending
  /leave
a command of the status fails          the status current                          "status of CHAT: ERROR" logged; end            ending
a command of the status fails          the status ended                            "status of CHAT: ERROR" logged                 unchanged
stop                                   each status                                 end; deletions awaited                         ending
the deletion settles                   any                                         removed from endings                           none
```

- `end` removes the status from the map, clears its timer, then calls `live.end("")`: `/_delete item CHAT ID broadcast` after the queued commands, when an item was posted.
- After a failed first post, a render queued before the failure settles is posted as a second live item, which is removed by the deletion queued in `failed`.
- Calls: in `AgentTools.call`, `startCall` adds one object `{tool}` before `run`; its end function, called in `finally`, removes that object; `lastCall` is set at both.

Start scan, before HTTP listens, for each chat of `agentChats()` that meets the conditions:
```
pages      /_get chat CHAT count=100, then before=FIRST count=100 while a page holds 100 and its first item is younger than the window
items      sndMsgContent, itemLive true, itemDeleted unset, itemTs younger than 30 h − 10 min
deletion   /_delete item CHAT IDS broadcast, one command per chat with such items
error      logged "status of CHAT: ERROR"; a failed chat list: "status messages: ERROR"
```

## send with live

Checks, in order:
```
text: required without file.
as: applies only with file.
live: applies to text only; send file, reply_to and mention without live.
text: at most 2000 characters with live.
No chat CHAT. list shows the agent's chats.
The agent is not connected to CHAT and can't post; list shows connected: false.
The agent has role ROLE in CHAT and can't post.
No chat CHAT. list shows the agent's chats.   (the chat being left)
live: CHAT is a channel or a chat with a bot or an agent; status messages are shown in chats with people, groups and business chats.
live: status messages are shown in mode requests only; this server runs in mode host.
live: The agent lost its control chat. The person who runs it has to restart it with --reset-owners.
live: your owner's approval of all messages to CHAT is required; list shows approved: true for such chats.
an aborted call: no result
The server is stopping.
live: the update limit of the status message of CHAT is reached; send your answer without live.
```

- Text length is counted in code points.
- A live call never attaches to an action; the counter write, the card, the log lines of actions and the wait for responses are skipped.
- Result: `{status: "live", chat}`. `sendOutput.required` is `["status"]`.
- Log line, stdout: `Note to SUBJECT (CHAT): TEXT`, the subject without its SimpleX name, names and text folded by `oneLine`.

## receive with typing

- With `wait` above 0 and nothing new, the status of the chat is deleted before the wait, with or without `typing`.
- After `/_read chat items` and the abort check, the status of the chat is started when `typing` is true and messages are returned.

## Inference endpoint

```
item                         value
--inference URL              http or https, without user name and password; kept as origin and path without trailing slashes
SIMPLEX_MCP_INFERENCE_KEY    read in startServer before the data folder is created; empty or other than 0x21-0x7e: exit 2
                             "SIMPLEX_MCP_INFERENCE_KEY: visible ASCII characters expected."
start log line               "Inference endpoint: URL", after "Approval mode: MODE"
usage errors, exit 2         --inference: an http or https URL without user name and password expected, got VALUE
                             --inference: status messages are shown only in mode requests.
```

Polls:
```
step            rule
when            a tick that reads the label: renders 2 to 49
sharing         a call within 3 s of the start of the last poll returns that poll's promise
deadline        one AbortSignal.timeout(1 s) for all requests of a poll
llama-swap      GET URL/running with the key; then, in parallel, at most 4 distinct allowed proxy origins of models in state ready:
                GET PROXY/props, then GET PROXY/slots unless the /props answer is 503 or has is_sleeping true; without the key
llama-server    a 404 of /running: GET URL/props and URL/slots, with the key
allowed proxy   http or https, without user name and password; hostname equal to URL's, or both among localhost, 127.0.0.1, [::1]
request         GET; redirect "error"; body up to 1 MiB by Content-Length and by the bytes read
slots           the first 64 of each proxy; counts accepted only as non-negative safe integers
```

States:
```
from the poll                                                state
/running: a model in state starting                          loading, that model
/running: none ready, one stopping                           loading
a 503 of /props or /slots                                    loading, the proxy's model
slots: a processing slot with n_decoded > 0                  output: model, rate
slots: processing slots, each with n_decoded 0               prompt: model, tokens read
slots: none processing                                       idle
none ready and none stopping, or no allowed proxy            undefined
```

- Model: the model of all processing slots when they share one; IDs outside `^[\w.:/+@-]{1,64}$` are dropped.
- Tokens read: the sum of `n_prompt_tokens_processed` of the processing slots.
- Rate: for each generating slot, the slot of the previous poll with the same proxy, `id` and `id_task`, processing, with `0 < n_decoded ≤` the current one; Δ`n_decoded` per second between the two `/slots` reads; the sum, when above 0.
- The previous slots are those of the last poll that read slots; they are cleared at a poll that gives loading or undefined, and kept at a failed poll.
- Failures: `/running` outside 2xx and 404; `/props` or `/slots` of a plain llama-server outside 2xx and 503; every allowed proxy failing, with the first proxy's error; a timeout, a redirect, invalid JSON or a body over 1 MiB.
- Error texts: `GET URL/PATH: ` and `HTTP N`, `invalid JSON`, `body over 1 MiB`, `no answer within 1 s`, or the message of `cause.errors[0]`, `cause` or the error, folded by `oneLine`.
- The first failed poll, and the first after each success, is logged as `inference endpoint: ERROR`.

## Start and stop

Start, in `startAgent` after `control.load()`: `tools.deleteLeftoverStatuses()`, before HTTP listens.

Stop:
1. events ignored;
2. actions stopped, while HTTP is closed;
3. `Cancelled K actions.`;
4. `tools.statuses.stop()`: each status deleted, the deletions awaited; M = statuses with renders > 0;
5. `waitForStopMessages(cards, --stop-wait, endWait, M)`: `Stopping: sending N cancelled cards and M status deletions, up to S s. Ctrl-C to stop now.`, each part when above 0;
6. `chat.close()`.

## Limits

```
item                          limit
note                          2000 code points, text only
renders per status            59; up to 50 by ticks
messages per status           60, with the deletion
status lifetime               23 h
leftover window               30 h − 10 min
poll                          1 per 3 s; 1 s deadline; 1 MiB per body; 4 proxies; 64 slots per proxy
model ID                      ^[\w.:/+@-]{1,64}$, shown in a code span
key                           visible ASCII; sent to URL only
error texts                   folded by oneLine; "invalid JSON" in place of the parser's text
```

## Changes

```
file           function or item           change
tools.ts       SendInput                  live
               sendSchema                 live
               ReceiveInput               typing
               receiveSchema              typing
               sendOutput                 status live; required ["status"]
               sendDescription            the live line; result status live
               receiveDescription         the typing line
               Agent                      inference
               ToolName                   exported
               AgentTools                 statuses: Statuses, with allowed = showsStatus of knownChat; leaving: chats being left
               call                       startCall before run; its end in finally
               send                       live: no attach, the live checks, showNote; posting checks by postingRefusal
               showNote                   new
               statusRefusal              new
               showsStatus                new
               performSend                statuses.end(chat) after apiSendMessages returns
               leaveChat                  leaving; statuses.end(chat), awaited, before the deletion
               deleteLeftoverStatuses     new
               receive                    typing; statuses.end(chat) before a wait; statuses.start(chat) after markRead
               postingRefusal             new: extracted from send
status.ts      Statuses, statusText,      new, 192 lines
               statusLabel
inference.ts   inferencePoller            new, 164 lines
serve.ts       ServeOptions               inference
               startServer                the poller and the key check before the data folder; "Inference endpoint: URL"
               startAgent                 Agent.inference; deleteLeftoverStatuses after control.load(); statuses stopped before the wait
               waitForStopMessages        renamed from waitForCancelledCards; parameter deletions, default 0
               inferenceKey               new
cli.ts         helpText                   --stop-wait for status deletions; --inference
               serveOptions               --inference; refused with --approve host
               inferenceUrl               new
messages.ts    pageSize                   exported
package.json   version                    0.3.0, with package-lock.json
```

`control.ts`, `actions.ts`, `http.ts`, `register.ts`, `files.ts` and `log.ts` are unchanged.

## Tests

- `test/unit/fixtures.ts`: `apiSendMessages` and `apiUpdateChatItem` record `live: true` when set; `apiDeleteChatItems` records `deletedItems`.
- `test/unit/status.test.ts` (new): the schedule; frames and elapsed time; the frozen line and 5-minute checks; label ranks; model and counts in labels; deletion when the conditions are unmet, at a failed read, at 23 h, and in the frozen phase 30 min after the last call; notes under the line, refused at render 59; a status 23 h old replaced at a note; no tick render after a label read during which notes reached render 50; one status per chat; a tick after the deletion; failed commands; stop; poll failure logging; leftovers page by page.
- `test/unit/inference.test.ts` (new), a local HTTP server: loading; proxies, at most 4, without the key; tokens/s between polls, summed; tokens read; idle; fractional and negative counts; 64 slots; the model of one busy model; a sleeping server; 503; a plain llama-server with the key; allowed proxies; one poll per 3 s; one 1 s deadline per poll; failure texts, folded; a failing proxy; model ID filter; GET of the three paths only.
- `test/unit/messages.test.ts`: `receive` with `typing` in approved direct chats, groups and business chats, and in each refused case; deletion when a receive of the chat waits, after the answer, before leave, after `/revoke`, after the control chat is lost; the status kept when the answer fails; `send` with `live`: the note, its log line, an aborted call, the refusals, a pending send with the same text; a call counted as running until it ends; leftovers read only in eligible chats.
- `test/unit/commands.test.ts`: `waitForStopMessages` with status deletions; the key refused with exit code 2 before the data folder exists.
- `test/unit/http.test.ts`: the `live` and `typing` schemas and descriptions; status `live`; each description, and `instructions` for a 15-character name, within 2048 characters.
- `test/unit/options.test.ts`: `--inference` forms, usage errors, help lines.
- `test/e2e/scenarios.test.ts`: a direct chat: status, note, answer, deletion, one outcome post, stop with the deletion, the leftover deleted at the next start; a group: status, note, deletion after the answer.

## Docs

- `README.md`: the stop line with status deletions; in the prompt, the chat where the agent takes prompts, a direct chat or a group, with `receive` (`wait 3600`, `typing true`) and answers with `send` (`wait 0`); a paragraph on the status message, "Delete for everyone", the active chat profile and `--inference`; a safety item on notes.
- `REFERENCE.md`: section "Status while the agent works"; the tool rows of `send` and `receive`; the stop line; the command help; notes on stdout; the key in Security; v0.3.
- `TESTING.md`: v0.3; the stop line; status steps without and with llama-swap.
- `PLAN.md`: version; the status message and the inference endpoint; the `send` and `receive` tables; processing; tests; decisions of 2026-11-11.
- `spec/`: version 0.3.0; `status.ts` and `inference.ts`; the anchors of `tools.ts`, `serve.ts` and `cli.ts` rewritten (decision 16).

## Pull request

- One draft pull request into `simplex-chat/simplex-chat` master, titled `mcp server: live status while the agent works`, linking this file.
- Changed paths: `packages/simplex-chat-mcp/**` and `plans/`; the reduced build runs the `mcp` scope.
