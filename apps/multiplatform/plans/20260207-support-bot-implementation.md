# SimpleX Support Bot — Implementation Plan

## Context

SimpleX Chat needs a support bot that handles customer inquiries via business chat, optionally routes to Grok AI, and escalates to human team members. The bot implements the product spec in `apps/multiplatform/plans/20260207-support-bot.md`. Codebase at `apps/simplex-chat-support-bot/`.

## Table of Contents

1. [Architecture](#1-architecture)
2. [Project Structure](#2-project-structure)
3. [Configuration](#3-configuration)
4. [State Machine](#4-state-machine)
5. [Two-ChatClient Coordination](#5-two-chatclient-coordination)
6. [Bot Initialization](#6-bot-initialization)
7. [Event Processing](#7-event-processing)
8. [Message Routing](#8-message-routing)
9. [Team Forwarding](#9-team-forwarding)
10. [Grok API Integration](#10-grok-api-integration)
11. [One-Way Gate Logic](#11-one-way-gate-logic)
12. [Bot Commands Setup](#12-bot-commands-setup)
13. [Message Templates](#13-message-templates)
14. [Error Handling](#14-error-handling)
15. [Implementation Sequence](#15-implementation-sequence)
16. [Verification](#16-verification)

---

## 1. Architecture

```
┌──────────────────────────────────────────────┐
│            Support Bot Process (TS)           │
│                                               │
│  mainClient ──WS──> simplex-chat CLI (5225)   │  Business address, event loop
│  grokClient ──WS──> simplex-chat CLI (5226)   │  Grok identity, auto-join groups
│                                               │
│  conversations: Map<groupId, State>           │
│  grokGroupMap: Map<mainGroupId, grokGroupId>  │
│  GrokApiClient → api.x.ai/v1/chat/completions│
└──────────────────────────────────────────────┘
```

Two `simplex-chat` CLI servers must run externally:
- CLI 1 (port 5225): Main bot profile with business address
- CLI 2 (port 5226): Grok agent profile (pre-configured as contact of main bot)

The bot connects to both via WebSocket using the `simplex-chat` npm package.

## 2. Project Structure

```
apps/simplex-chat-support-bot/
├── package.json          # deps: simplex-chat, @simplex-chat/types
├── tsconfig.json         # ES2022, Node module resolution
├── src/
│   ├── index.ts          # Entry: parse args, create clients, run
│   ├── config.ts         # CLI arg parsing, env vars, Config type
│   ├── bot.ts            # SupportBot class: event loop, dispatch
│   ├── state.ts          # ConversationState union, transitions
│   ├── grok.ts           # Grok xAI API client, system prompt, history
│   ├── messages.ts       # All user-facing message templates
│   └── util.ts           # extractText, isWeekend, logging
├── docs/
│   └── simplex-context.md # Curated SimpleX docs injected into Grok prompt
```

## 3. Configuration

**CLI args:**

| Arg | Required | Default | Purpose |
|-----|----------|---------|---------|
| `--main-port` | No | 5225 | Main bot CLI WebSocket port |
| `--grok-port` | No | 5226 | Grok agent CLI WebSocket port |
| `--team-group` | Yes | — | GroupId for team message forwarding |
| `--team-members` | Yes | — | Comma-separated contactIds of team members |
| `--grok-contact-id` | Yes | — | ContactId of Grok agent in main bot's contacts |
| `--group-links` | No | "" | Public group link(s) for welcome message |
| `--timezone` | No | "UTC" | IANA timezone for weekend detection |

**Env vars:** `GROK_API_KEY` (required) — xAI API key.

```typescript
interface Config {
  mainPort: number
  grokPort: number
  teamGroupId: number
  teamMemberContactIds: number[]
  grokContactId: number
  groupLinks: string
  timezone: string
  grokApiKey: string
}
```

Parse via `process.argv` iteration (no deps needed). Fail fast on missing required args.

## 4. State Machine

Keyed by `groupId` of business chat group. In-memory only (acceptable for MVP — restart resets conversations; team group retains forwarded messages).

```typescript
type ConversationState =
  | { type: "welcome" }
  | { type: "teamQueue"; userMessages: string[] }  // tracks msgs for Grok context
  | { type: "grokMode"; grokMemberGId: number; history: GrokMessage[] }
  | { type: "teamPending"; teamMemberGId: number; grokMemberGId?: number; history?: GrokMessage[] }
  | { type: "teamLocked"; teamMemberGId: number }
```

**Key**: `teamQueue.userMessages` accumulates user messages so they can be forwarded to Grok as initial context on `/grok` activation (spec: "Your message(s) have been forwarded").

Transitions:
```
welcome ──(1st msg)──> teamQueue (msg stored in userMessages)
teamQueue ──(more msgs)──> teamQueue (appended to userMessages)
teamQueue ──(/grok)──> grokMode (userMessages → initial Grok history)
teamQueue ──(/team)──> teamPending
grokMode ──(/team)──> teamPending (carries grokMemberGId + history)
grokMode ──(msg)──> grokMode (history appended)
teamPending ──(team member msg)──> teamLocked (grok removed)
teamPending ──(/grok, grok present)──> still teamPending (grok answers)
teamLocked ──(/grok)──> reply "team mode", stay locked
```

## 5. Two-ChatClient Coordination

**Problem**: When main bot invites Grok agent to a business chat group, the Grok agent's local `groupId` differs from the main bot's `groupId` (different databases).

**Solution**: Shared in-process maps correlated via `memberId` (protocol-level, same across both databases).

```typescript
const pendingGrokJoins = new Map<string, number>()  // memberId → mainGroupId
const grokGroupMap = new Map<number, number>()       // mainGroupId → grokGroupId
```

Flow:
1. Main bot: `apiAddMember(mainGroupId, grokContactId, "member")` → response has `member.memberId`
2. Store: `pendingGrokJoins.set(member.memberId, mainGroupId)`
3. Grok agent event loop: `receivedGroupInvitation` → `evt.groupInfo.membership.memberId` matches → `apiJoinGroup(evt.groupInfo.groupId)` → store mapping
4. Send Grok response: `grokClient.apiSendTextMessage(ChatType.Group, grokGroupMap.get(mainGroupId), text)`

**Grok agent event loop** (minimal, in same process):
```typescript
async function runGrokAgentLoop(grokClient: ChatClient): Promise<void> {
  for await (const r of grokClient.msgQ) {
    const evt = r instanceof Promise ? await r : r
    if (evt.type === "receivedGroupInvitation") {
      const memberId = evt.groupInfo.membership.memberId
      const mainGroupId = pendingGrokJoins.get(memberId)
      if (mainGroupId !== undefined) {
        pendingGrokJoins.delete(memberId)
        const grokLocalGroupId = evt.groupInfo.groupId
        grokGroupMap.set(mainGroupId, grokLocalGroupId)
        await grokClient.apiJoinGroup(grokLocalGroupId)
      }
    }
    // Ignore all other events
  }
}
```

## 6. Bot Initialization

In `index.ts`:
1. Parse config
2. Connect both ChatClients (`ChatClient.create("ws://localhost:PORT")`)
3. Verify both have active user profiles
4. Register bot commands (`/grok`, `/team`) and set `peerType: "bot"` via `apiUpdateProfile`
5. Ensure main bot has business address with auto-accept and welcome auto-reply
6. Start Grok agent event loop (background, same process)
7. Start main bot event loop

Business address setup:
```typescript
await mainClient.enableAddressAutoAccept(
  mainUser.userId,
  { type: "text", text: welcomeMessage(config.groupLinks) },
  true  // businessAddress = true
)
```

The auto-reply handles Step 1 (Welcome) automatically on connect.

## 7. Event Processing

Main event loop in `SupportBot.run()` iterates `mainClient.msgQ`:

| Event | Action |
|-------|--------|
| `acceptingBusinessRequest` | Initialize conversation state as `welcome` |
| `newChatItems` (group, business, customer rcv) | Dispatch to `onCustomerMessage` |
| `newChatItems` (group, business, team member rcv) | Dispatch to `onTeamMemberMessage` |
| `leftMember` / `deletedMemberUser` / `groupDeleted` | Clean up conversation state |
| Everything else | Ignore |

**Identifying senders in `newChatItems`:**
- `chatItem.chatDir.type === "groupRcv"` → received from `chatItem.chatDir.groupMember`
- `chatItem.chatDir.type === "groupSnd"` → our own message (skip)
- Customer: `chatDir.groupMember.memberId === groupInfo.businessChat.customerId`
- Team member: `chatDir.groupMember.groupMemberId === state.teamMemberGId`
- Grok agent: `chatDir.groupMember.groupMemberId === state.grokMemberGId` (skip — we sent it via grokClient)

**Extracting text:**
```typescript
function extractText(content: CIContent): string | null {
  if (content.type === "rcvMsgContent" || content.type === "sndMsgContent") {
    const mc = content.msgContent
    if (mc.type === "text" || mc.type === "link" || mc.type === "file") return mc.text
  }
  return null
}
```

## 8. Message Routing

`onCustomerMessage(groupId, groupInfo, text, state)`:

| State | `/grok` | `/team` | Other text |
|-------|---------|---------|------------|
| `welcome` | — | — | Forward to team, reply with queue msg → `teamQueue` (store msg) |
| `teamQueue` | Activate Grok (with accumulated msgs) → `grokMode` | Activate team → `teamPending` | Forward to team, append to `userMessages` |
| `grokMode` | (ignored, already grok) | Activate team → `teamPending` | Forward to Grok API + team |
| `teamPending` (grok present) | Forward to Grok | (ignored, already team) | No forwarding (team sees directly) |
| `teamPending` (no grok) | Reply "team mode" | — | No forwarding (team sees directly) |
| `teamLocked` | Reply "team mode" | — | No forwarding needed (team sees directly) |

## 9. Team Forwarding

Forward messages to the designated team group:
```typescript
async forwardToTeam(groupId: number, groupInfo: GroupInfo, text: string): Promise<void> {
  const customerName = groupInfo.groupProfile.displayName || `group-${groupId}`
  const fwd = `[${customerName} #${groupId}]\n${text}`
  await this.mainClient.apiSendTextMessage(ChatType.Group, this.config.teamGroupId, fwd)
}
```

Adding team member to business chat:
```typescript
async activateTeam(groupId: number, currentState: ConversationState): Promise<void> {
  const teamContactId = this.config.teamMemberContactIds[0]
  const member = await this.mainClient.apiAddMember(groupId, teamContactId, "member")
  this.conversations.set(groupId, {
    type: "teamPending",
    teamMemberGId: member.groupMemberId,
    grokMemberGId: currentState.type === "grokMode" ? currentState.grokMemberGId : undefined,
    history: currentState.type === "grokMode" ? currentState.history : undefined,
  })
  await this.sendGroupMessage(groupId, teamAddedMessage(this.config.timezone))
}
```

## 10. Grok API Integration

**`grok.ts`** wraps xAI's OpenAI-compatible API:
- Endpoint: `https://api.x.ai/v1/chat/completions`
- Model: `grok-3`
- System prompt: "Privacy expert and SimpleX Chat evangelist" + curated docs from `docs/simplex-context.md`
- Per-conversation history: last 20 messages (trim from front)
- API key from `GROK_API_KEY` env var

```typescript
class GrokApiClient {
  private docsContext: string  // loaded from file at startup

  async chat(history: GrokMessage[], userMessage: string): Promise<string> {
    const messages = [
      { role: "system", content: this.systemPrompt() },
      ...history.slice(-20),
      { role: "user", content: userMessage },
    ]
    const resp = await fetch("https://api.x.ai/v1/chat/completions", {
      method: "POST",
      headers: { "Content-Type": "application/json", Authorization: `Bearer ${this.apiKey}` },
      body: JSON.stringify({ model: "grok-3", messages, max_tokens: 2048 }),
    })
    if (!resp.ok) throw new Error(`Grok API: ${resp.status}`)
    const data = await resp.json()
    return data.choices[0].message.content
  }
}
```

**Activating Grok:**
1. `apiAddMember(groupId, grokContactId, "member")` — invites Grok to business chat
2. Store `pendingGrokJoins` mapping
3. Wait for Grok agent to join (poll `grokGroupMap` with timeout)
4. Build initial Grok history from `state.userMessages` (accumulated in teamQueue)
5. Forward all accumulated messages to Grok API in a single call
6. Send bot activation message first, then Grok response via `grokClient.apiSendTextMessage`
7. State transitions to `grokMode`

**Fallback**: If Grok API fails, send error message and revert to teamQueue state.

## 11. One-Way Gate Logic

Per spec: "/grok permanently disabled ONLY after team joins AND team member sends a message."

```typescript
async onTeamMemberMessage(groupId: number, state: ConversationState): Promise<void> {
  if (state.type !== "teamPending") return

  // Remove Grok if present
  if (state.grokMemberGId) {
    try { await this.mainClient.apiRemoveMembers(groupId, [state.grokMemberGId]) } catch {}
    grokGroupMap.delete(groupId)
  }

  // Lock: /grok permanently disabled
  this.conversations.set(groupId, { type: "teamLocked", teamMemberGId: state.teamMemberGId })
}
```

Timeline:
1. User sends `/team` → bot adds team member → state = `teamPending` (Grok still usable if present)
2. Team member sends a message → state = `teamLocked`, Grok removed
3. Any subsequent `/grok` → "You are now in team mode. A team member will reply to your message."

## 12. Bot Commands Setup

Register `/grok` and `/team` via profile update:

```typescript
const botCommands: T.ChatBotCommand[] = [
  { type: "command", keyword: "grok", label: "Ask Grok AI" },
  { type: "command", keyword: "team", label: "Switch to team" },
]
await mainClient.apiUpdateProfile(mainUser.userId, {
  displayName, fullName, shortDescr, image, contactLink,
  peerType: T.ChatPeerType.Bot,
  preferences: { ...preferences, commands: botCommands },
})
```

## 13. Message Templates

All in `messages.ts`, verbatim from spec:

- **Welcome** (auto-reply): "Hello! Feel free to ask any question about SimpleX Chat..."
- **Team queue** (after 1st msg): "Thank you for your message, it is forwarded to the team. It may take a team member up to {24/48} hours to reply. Click /grok if..."
- **Grok activated**: "*You are now chatting with Grok...*"
- **Team added**: "A team member has been added and will reply within {24/48} hours..."
- **Team mode locked**: "You are now in team mode. A team member will reply to your message."

Weekend detection: `Intl.DateTimeFormat("en-US", { timeZone, weekday: "short" })` → "Sat"/"Sun".

## 14. Error Handling

| Scenario | Handling |
|----------|----------|
| CLI disconnection | Event loop exits → log, process exits (let process manager restart) |
| Grok API error | Catch, send "Grok temporarily unavailable", revert to teamQueue |
| `apiAddMember` fails | Catch, send error message to user |
| `apiRemoveMembers` fails | Ignore (member may have left already) |
| Grok join timeout (30s) | Send "Grok unavailable", stay in current state |
| Customer leaves group | Clean up conversation state on `leftMember` event |
| Group deleted | Clean up on `groupDeleted` event |
| Grok leaves during teamPending | Clear Grok reference, keep teamPending |
| Team member leaves | Revert to teamQueue |

## 15. Implementation Sequence

**Phase 1: Scaffold** — `package.json`, `tsconfig.json`, `config.ts`, `index.ts`, `util.ts`
- Create project, install deps, implement config parsing
- Connect both ChatClients, verify user profiles, set up business address
- Verify: both clients connect and print profiles

**Phase 2: State machine + event loop** — `state.ts`, `bot.ts`, `messages.ts`
- Define `ConversationState` type
- Implement `SupportBot` class with event loop
- Handle `acceptingBusinessRequest` → init state
- Handle `newChatItems` → customer message dispatch
- Implement Welcome → TeamQueue transition + team forwarding
- Verify: customer connects, sends msg, bot replies with queue info, msg forwarded

**Phase 3: Grok integration** — `grok.ts`, updates to `bot.ts`
- Implement `GrokApiClient` with system prompt + doc injection
- Implement Grok agent event loop (auto-join groups)
- Implement `activateGrok`: add member, ID mapping, activation message
- Implement `forwardToGrok`: API call → response via Grok client
- Verify: /grok works, user sees Grok responses under Grok profile

**Phase 4: Team mode + one-way gate** — updates to `bot.ts`
- Implement `activateTeam`: add team member
- Implement `onTeamMemberMessage`: detect team msg → lock → remove Grok
- Implement `/grok` rejection in `teamLocked`
- Verify: full flow including gate lock

**Phase 5: Polish** — bot commands, edge cases, docs
- Register /grok and /team as bot commands via profile update
- Handle customer leave, group delete, Grok timeout, error messages
- Write `simplex-context.md` for Grok prompt injection
- End-to-end test

## 16. Verification

1. **Start infrastructure**: Two `simplex-chat` CLIs on ports 5225/5226
2. **Initialize profiles**: Create "Support Bot" and "Grok" user profiles, establish contact between them
3. **Run bot**: `npx ts-node src/index.ts --team-group X --team-members Y --grok-contact-id Z`
4. **Test welcome flow**: Connect from a third SimpleX client to bot's business address → verify welcome message
5. **Test first message**: Send a question → verify forwarded to team group, queue reply received
6. **Test /grok**: Send `/grok` → verify Grok joins as separate participant, responses appear from Grok profile
7. **Test /team from grok**: Send `/team` → verify team member added, send team member message → verify /grok locked, Grok removed
8. **Test weekend**: Change timezone to a weekend timezone, verify "48 hours" in messages

### Critical Reference Files
- `packages/simplex-chat-client/typescript/src/client.ts` — ChatClient API
- `packages/simplex-chat-client/types/typescript/src/events.ts` — Event types (CEvt)
- `packages/simplex-chat-client/types/typescript/src/types.ts` — Domain types
- `packages/simplex-chat-client/types/typescript/src/commands.ts` — Command builders (CC)
- `packages/simplex-chat-client/typescript/examples/squaring-bot.js` — Reference pattern
