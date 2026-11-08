# SimpleX MCP 0.3.0: Claude Code plugin and SimpleX channel

Status: draft for review. Code references: master `2abd48450` (MCP 0.2.0); #7810 at `a7953a767`. Claude Code references: https://code.claude.com/docs/en/channels, https://code.claude.com/docs/en/channels-reference.

Outcome:
- Claude Code users install SimpleX with `/plugin install`, scan one QR code and join the Main group.
- Each Claude Code session has its own group with its owners; one `Claude` profile holds the chats of all sessions.
- Owners prompt a running session in its group; the session receives the prompt without polling.
- Owners approve the session's tool use (`Bash`, `Write`, `Edit`) in its group.
- `simplex-mcp serve` keeps serving other MCP hosts over HTTP.

## Implementation goal

One PR that publishes `@simplex-chat/mcp` 0.3.0: Claude Code users install the plugin, join one Main group, and run each Claude Code session from its own SimpleX group, with prompts from the phone, the session mirrored, and tool use approved there; `serve` is unchanged for other hosts.

Done when:
```
#   criterion                                                                                   checked by
1   /plugin install; first session shows the QR code in the terminal, never in Claude's context  unit test of the hook output; real session
2   an owner joins the Main group and gets a "Tap to join" card for each session's group          end-to-end test
3   an owner's message in a session group reaches the running session as a channel event          real session with the development flag
4   Claude's answer is posted by Claude; a prompt typed in the terminal is posted by User          end-to-end test of the hooks; real session
5   a Bash prompt becomes a card; /approve runs the tool, /reject refuses it                      real session
6   connect, send, download and leave cards in the session group; chats owned by sessions; /give  end-to-end tests
7   the group description shows the model and effort and follows a change                         unit test; real session
8   decisions 17-23 hold                                                                          unit tests
9   serve: all 0.2 unit and end-to-end tests pass unchanged                                      CI
10  source lines written on top of sh/mcp-known: at most 1,200                                    diff count
11  one page presents the PR; an independent reviewer confirms it matches the diff               review report
```

Real-session checks run in an interactive Claude Code session started with `--dangerously-load-development-channels plugin:simplex@simplex-chat`; the user runs them or allows them.

## Decisions

Agreed on 2026-11-06:
1. `serve`, `register` and HTTP stay.
2. #7810's code is adopted with `serve` kept; shum reviews the result.
3. The plugin is released as 0.3.0.

Decided on 2026-11-07:
4. One PR: branch `ep/mcp-plugin` from `sh/mcp-known` in the agents' fork, against master; it replaces #7810.
5. The PR reaches the user as one page that presents it; an independent reviewer checks the page against the diff before the user sees it.
6. D1 (b): one group per session. The hub invites the Main group's owners to it through the Main group; other members of the Main group join it by `/join N`.
7. D2 (a): every tool prompt of a session is relayed to its group.
8. D3 (a), replaced by D8 (a): peers see `Claude`, or an incognito name in a chat connected incognito.
9. D4 (a): files in prompts and in replies are in this PR.
10. D5 (a): the user asks Anthropic to list the channel after 0.3.0 is published.
11. D6 (a): the hub, the plugin's main profile, speaks for Claude: it posts the session's answers and cards in the session's group. Its profile name is `Claude`.
12. D7 (a): prompts typed in the terminal are posted in the session's group by a local profile named `User`, one per data root, a member of every session group.
13. D8 (a): one `Claude` profile, in the daemon's one process, serves all sessions of a data root: it hosts the Main group and the session groups and holds the chats with peers. A chat belongs to the session that connected it; tools of other sessions do not show it; owners hand it to another session with `/give CHAT N` in the session's group.
14. D9 (a): the session group's description shows the model and the effort level and is updated when they change; they are changed in the terminal or from the Claude app by Remote Control.
15. D10 (b): 0.3.0 ships with a group per session; the plugin moves each session group into a session of the Main group when the apps support group sessions. A UX proposal for group sessions is prepared separately, now.

Design:
16. One code base: `serve` and the plugin share `Control`, `Actions` and `AgentTools`.
17. Prompts, approvals and tool-use verdicts count only from session group members whose Main group member is an owner.
18. The owner link reaches the person only: the `systemMessage` of the SessionStart hook and `simplex-mcp link` in a terminal; hook context and tool results never contain it.
19. A session's approval mode changes to `host` only after an owner approves a card in the session's group.
20. While owners exist, an owner reset runs only after an owner approves a card in the Main group; without owners it runs from a terminal.
21. The launcher runs `npx` with the plugin folder as its working folder.
22. Refused paths cover the whole data root.
23. Standing approvals of closed sessions are listed by `/list` and removed by `/revoke` in the session's group.
24. The channel is part of the plugin's session server; the daemon pushes events on the session's `/attach` stream.
25. The session and its group exist from the session's start.
26. Claude's final answer of every turn is posted by `Claude` from the Stop hook's `last_assistant_message`; the `reply` tool sends files only.

Chat ownership, D8 (a):
```
ChatOwner, in the chat's custom data
  session        Claude Code session ID that connected the chat
```

```
on connect by session s:     owner = s
on any tool call by s:       chats with owner = s only
on "/give CHAT N" by owner:  owner of CHAT = session N
on resume of s:              its chats are shown again
```

## Model and effort

Sources (https://code.claude.com/docs/en/hooks, https://code.claude.com/docs/en/settings, https://code.claude.com/docs/en/remote-control):
```
value    read from                                          updated on
model    SessionStart model (optional)                       PostModelSwitch to_model
effort   effort.level, common field of PreToolUse,           the next such event; no event for an effort change
         PostToolUse and Stop
```

```
on SessionStart(m):       group description = model m
on PostModelSwitch(t):    group description = model t
on Stop or tool event(e): if e.effort.level differs: group description = effort e.effort.level
```

Changing them from SimpleX, per the docs:
```
route                                   effect on the running session
model or effortLevel in settings files  none: read once at session start
hook output                             none: no field sets them
channel message "/model sonnet"         not documented; tested in this PR, added if it runs as a command
Remote Control from the Claude app      changes both
```

## Components

```mermaid
flowchart LR
  subgraph App[SimpleX app]
    O[owners]
  end
  subgraph Host[computer]
    CC[Claude Code session N] -- stdio MCP --> S[simplex-mcp session]
    S -- Unix socket --> D[daemon]
    D --> H[Claude profile]
    D --> U[User profile]
    SV[simplex-mcp serve, HTTP] --> A0[agent profile]
  end
  H -- Main group --> O
  H -- session N group --> O
  U -- session N group --> O
  H <-- SimpleX --> P[peers]
  A0 <-- SimpleX --> P
```

```
process               runs                                   holds
simplex-mcp session   one per Claude Code session, stdio     tools, channel capability
daemon                one per data root, Unix socket          Claude and User profiles, actions
Claude                inside the daemon                       Main group, session groups, owners, cards, chats with peers
User                  inside the daemon                       membership in every session group
simplex-mcp serve     one per agent, HTTP 127.0.0.1           agent profile, control chat
```

## Groups

```
group            host  members                                     holds
Main group       hub   owners and other members                    QR link, invitation cards, /join, /sessions, owner reset
session N group  hub   Main group owners, invited by the hub       prompts, replies, action and tool-use cards of session N
                       Main group members who sent /join N
```

```
preference         Main group                  session N group
history            on                          on
direct messages    on, role owner              off
commands           /join, /sessions            /approve, /reject, /receive, /forward, /revoke, /pending, /list, /leave, /delete
name               the plugin's main_group     N · session title or project folder
```

Direct messages in the Main group are on for the owner role, because the core sends an invitation through a member connection only where direct messages are allowed (`docs/rfcs/2026-10-10-invite-member-via-group.md`). Joining needs a SimpleX app with chat version 22: v7.1.0 or later.

Session group custom data:
```
SessionGroupData
  session          session number
  invited          list of InvitedMember
InvitedMember
  groupMemberId    member in the session group
  mainMemberId     member in the Main group it was invited through
```

```
on session start (new id):   create the session group; for each Main group owner: apiAddMemberViaGroup(group, main, owner, owner); record InvitedMember
on owner joined Main group:  for each open session: invite the owner as above
on "/join N" by member m:    if session N exists: apiAddMemberViaGroup(group N, main, m, role of m in the Main group); record InvitedMember
on message in group N:       m = InvitedMember of sender; if m and Main group member m.mainMemberId is an owner: handle; else ignore
on session resume:           reuse the group recorded in session.json
```

Commands in a session group take no session prefix: `/approve 7`, `/receive @3 9`, `/leave #2`.

### Session start

```mermaid
sequenceDiagram
  participant C as Claude Code
  participant S as session N
  participant H as hub
  participant M as Main group
  participant O as owner
  C->>S: SessionStart
  S->>H: POST /session
  H->>H: create group "N · title"
  H->>M: invitation through the owner's Main group connection
  M->>O: card "Tap to join", for you only
  O->>H: joins the session group
```

## Channel

Session server capabilities:
```
capability                                  value
experimental.claude/channel                  {}
experimental.claude/channel/permission       {}
tools                                       {}
```

Daemon to session, lines on `/attach`:
```
line                                         sent when
{prompt: {text, from, item, file?}}          an owner's message arrives in the session group
{permission: {requestId, behavior}}          an owner decides a tool-use card
```

Session to Claude Code:
```
notification                                 params
notifications/claude/channel                 content: text
                                             meta.from: owner name
                                             meta.item: session group item ID
                                             meta.file: path
notifications/claude/channel/permission      request_id
                                             behavior: allow | deny
```

Claude Code to session, then session to daemon:
```
notification                                       route
notifications/claude/channel/permission_request    POST /permission {id, requestId, toolName, description, inputPreview}
```

New tool:
```
tool    input                       action
reply   file, text?                 sends the file to the session group, text as its caption; no card
```

Server instructions, added: messages in a `simplex` channel are the owners' prompts; the final answer of the turn reaches them; `reply` sends them files.

## Session mirroring

Hooks of the plugin (https://code.claude.com/docs/en/hooks):
```
hook               input used                 action
UserPromptSubmit   prompt                     User posts it in the session group
Stop               last_assistant_message     Claude posts it in the session group, quoting the turn's prompt
```

```
on UserPromptSubmit(p):  if p equals a prompt the daemon pushed to this session in the last 60 s: skip; else post p
on Stop(m):              post m; if the turn's prompt came from the group: quote it
```

The docs do not state whether a channel prompt fires UserPromptSubmit; the skip covers both cases.

```
UserProfile
  folder           ROOT/_user
  name             User
  contact          Claude, connected once at the first start
  groups           every session group, added by Claude through the contact, joins automatically
  visible to       session group members only
```

Claude ignores User's messages as prompts: User was not invited through a Main group owner (decision 17).

A prompt to a session started without the channel flag is dropped by Claude Code; the hub cannot detect it.

Files: an owner's file in a prompt is received by the hub without a card into `ROOT/_sessions/<id>/files/group/<file ID><extension>`; `meta.file` is its path. `reply` with `file` sends it to the session group under the refused-path rules of `send`.

### Prompt flow

```mermaid
sequenceDiagram
  participant O as owner
  participant H as hub
  participant D as daemon
  participant S as session N
  participant C as Claude Code
  O->>H: message in session N's group
  H->>H: sender's Main group member an owner?
  H->>D: prompt for N
  D->>S: {prompt} on /attach
  S->>C: notifications/claude/channel
  C->>C: turn ends
  C->>S: Stop hook, last_assistant_message
  S->>D: POST /answer
  D->>H: post in session N's group
  H->>O: answer, quoting the prompt
```

### Tool-use approval

```mermaid
sequenceDiagram
  participant C as Claude Code
  participant S as session N
  participant D as daemon
  participant H as hub
  participant O as owner
  C->>S: permission_request (abcde, Bash, preview)
  S->>D: POST /permission
  D->>H: new action, tool "permission"
  H->>O: card in session N's group: "Run Bash?" /approve K /reject K
  O->>H: /approve K
  H->>D: verdict allow
  D->>S: {permission} on /attach
  S->>C: notifications/claude/channel/permission allow
```

```
PermissionAction
  number          action number
  session         session number
  requestId       five letters from Claude Code
  toolName
  description
  inputPreview
  state           pending | allowed | denied | ended
  decidedBy       owner name
```

```
state     next
pending   allowed | denied | ended
allowed   final
denied    final
ended     final
```

```
on permission_request(r):    action = permission(session, r); post card in the session group
on /approve K by owner:      if action K pending: state = allowed; push {permission: allow}
on /reject K by owner:       if action K pending: state = denied; push {permission: deny}
on session detached:         for each pending permission of the session: state = ended; edit card
```

An answer at the terminal wins; Claude Code drops the later remote verdict.

## Launch

```
state                         command
listed by Anthropic           claude --channels plugin:simplex@simplex-chat
before the listing            claude --dangerously-load-development-channels plugin:simplex@simplex-chat
Team and Enterprise plans     an admin adds simplex@simplex-chat to allowedChannelPlugins
```

The development flag works in interactive sessions only. Without either flag the plugin works without prompts and tool-use relay.

## Pull request

```mermaid
flowchart LR
  K[sh/mcp-known, 49 commits] --> M[merge of master]
  M --> G[session groups, owner rules]
  G --> C[channel, 0.3.0]
  C --> P[PR against master]
  P -. replaces .-> X[#7810]
  P --> V[page presenting the PR, checked against the diff by an independent reviewer]
```

```
branch   ep/mcp-plugin, from sh/mcp-known, in the agents' fork
base     master
commits  #7810's 49 commits
         merge of master: serve, register and HTTP kept; 0.2 features in session groups
         one Claude profile, session groups and owner rules: decisions 6, 11, 13, 17-23
         channel and mirroring: decisions 7, 9, 12, 14, 24-26, reply tool, version 0.3.0
```

Size budget, source lines written on top of `sh/mcp-known`, without tests:
```
part                                  budget
merge, session groups, owner rules    750
channel and mirroring                 450
```

`serve` and its end-to-end tests keep passing. `build.yml` publishes 0.3.0 after the merge.
