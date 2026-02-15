# SimpleX Support Bot — Implementation Plan

## 1. Executive Summary

SimpleX Chat support bot — standalone Node.js app using `simplex-chat-nodejs` native NAPI binding. Two `ChatApi` instances (main bot + Grok agent identity) in one process, each with own SQLite database. No external CLI processes. Implements 4-step flow: Welcome → TeamQueue → GrokMode/TeamPending → TeamLocked.

## 2. Architecture

```
┌─────────────────────────────────────────────────┐
│          Support Bot Process (Node.js)           │
│                                                  │
│  mainChat: ChatApi ← ChatApi.init("./data/bot")  │
│    • Business address, event routing, state mgmt │
│    • DB: data/bot_chat.db + data/bot_agent.db    │
│                                                  │
│  grokChat: ChatApi ← ChatApi.init("./data/grok") │
│    • Grok identity, auto-joins groups            │
│    • DB: data/grok_chat.db + data/grok_agent.db  │
│                                                  │
│  State: derived from group composition + chat DB  │
│  grokGroupMap: Map<mainGroupId, grokGroupId>     │
│  GrokApiClient → api.x.ai/v1/chat/completions   │
└─────────────────────────────────────────────────┘
```

- Single Node.js process, no external dependencies except Grok API
- Two `ChatApi` instances via native NAPI — each embeds simplex-chat core
- Business address auto-accept creates a group per customer (business chat = special group)
- Grok agent is a separate identity that gets invited as group member, making Grok appear as a separate participant per spec
- Cross-instance group ID correlation via protocol-level `memberId` (string, same across both databases)

## 3. Project Structure

```
apps/simplex-support-bot/
├── package.json          # deps: simplex-chat, @simplex-chat/types
├── tsconfig.json         # ES2022, strict, Node16 module resolution
├── src/
│   ├── index.ts          # Entry: parse config, init instances, run
│   ├── config.ts         # CLI arg parsing, ID:name validation, Config type
│   ├── bot.ts            # SupportBot class: stateless state derivation, event dispatch, routing
│   ├── state.ts          # GrokMessage type
│   ├── grok.ts           # GrokApiClient: xAI API wrapper, system prompt, history
│   ├── messages.ts       # All user-facing message templates
│   └── util.ts           # isWeekend, logging helpers
├── data/                 # SQLite databases (created at runtime)
└── docs/
    └── simplex-context.md  # Curated SimpleX docs injected into Grok system prompt
```

## 4. Configuration

All runtime state (team group ID, Grok contact ID) is auto-resolved and persisted to `{dbPrefix}_state.json`. No manual IDs needed for core entities.

**CLI args:**

| Arg | Required | Default | Format | Purpose |
|-----|----------|---------|--------|---------|
| `--db-prefix` | No | `./data/bot` | path | Main bot database file prefix |
| `--grok-db-prefix` | No | `./data/grok` | path | Grok agent database file prefix |
| `--team-group` | Yes | — | `name` | Team group display name (auto-created if absent) |
| `--team-members` | No | `""` | `ID:name,...` | Comma-separated team member contacts (optional) |
| `--group-links` | No | `""` | string | Public group link(s) for welcome message |
| `--timezone` | No | `"UTC"` | IANA tz | For weekend detection (24h vs 48h) |

**Env vars:** `GROK_API_KEY` (required) — xAI API key.

```typescript
interface Config {
  dbPrefix: string
  grokDbPrefix: string
  teamGroup: {id: number; name: string}  // id=0 at parse time, resolved at startup
  teamMembers: {id: number; name: string}[]  // optional, empty if not provided
  grokContactId: number | null  // resolved at startup from state file
  groupLinks: string
  timezone: string
  grokApiKey: string
}
```

**State file** — `{dbPrefix}_state.json`:
```json
{"teamGroupId": 123, "grokContactId": 4, "grokGroupMap": {"100": 200}}
```

Team group ID, Grok contact ID, and Grok group map are persisted to ensure the bot reconnects to the same entities across restarts. The Grok group map (`mainGroupId → grokLocalGroupId`) is updated on every Grok join/leave event.

**Grok contact resolution** (auto-establish):
1. Read `grokContactId` from state file → validate it exists in `apiListContacts`
2. If not found: create invitation link (`apiCreateLink`), connect Grok agent (`apiConnectActiveUser`), wait for `contactConnected` (60s), persist new contact ID
3. If Grok contact is unavailable, bot runs but `/grok` returns "temporarily unavailable"

**Team group resolution** (auto-create):
1. Read `teamGroupId` from state file → validate it exists in `apiListGroups`
2. If not found: create with `apiNewGroup`, persist new group ID

**Team group invite link lifecycle:**
1. Delete any stale link from previous run: `apiDeleteGroupLink` (best-effort)
2. Create invite link: `apiCreateGroupLink(teamGroupId, GroupMemberRole.Member)`
3. Display link on stdout for team members to join
4. Schedule deletion after 10 minutes: `apiDeleteGroupLink(teamGroupId)`
5. On shutdown (SIGINT/SIGTERM), delete link before exit (idempotent, best-effort)

**Team member validation** (optional):
- If `--team-members` provided: validate each contact ID/name pair via `apiListContacts`, fail-fast on mismatch
- If not provided: bot runs without team members; `/team` returns "No team members are available yet"

## 5. State Derivation (Stateless)

State is derived from group composition (`apiListMembers`) and chat history (`apiGetChat` via `sendChatCmd`). No in-memory `conversations` map — survives restarts naturally.

**Derived states:**

| Condition | Equivalent State |
|-----------|-----------------|
| No bot `groupSnd` containing "forwarded to the team" | welcome |
| No Grok member, no team member, bot has sent queue reply | teamQueue |
| Grok member present (active) | grokMode |
| Team member present, hasn't sent message | teamPending |
| Team member present, has sent message | teamLocked |

**State derivation helpers:**
- `getGroupComposition(groupId)` → `{grokMember, teamMember}` from `apiListMembers`
- `isFirstCustomerMessage(groupId)` → checks if bot has sent "forwarded to the team" via `apiGetChat`
- `getGrokHistory(groupId, grokMember, customerId)` → reconstructs Grok conversation from chat history
- `getCustomerMessages(groupId, customerId)` → accumulated customer messages from chat history
- `hasTeamMemberSentMessage(groupId, teamMember)` → teamPending vs teamLocked from chat history

**Transitions (same as stateful approach):**
```
welcome ──(1st user msg)──> teamQueue (forward to team + queue reply)
teamQueue ──(user msg)──> teamQueue (forward to team)
teamQueue ──(/grok)──> grokMode (invite Grok, send accumulated msgs to API)
teamQueue ──(/team)──> teamPending (add team member)
grokMode ──(user msg)──> grokMode (forward to Grok API + team)
grokMode ──(/team)──> teamPending (remove Grok, add team member)
teamPending ──(team member msg)──> teamLocked (implicit via hasTeamMemberSentMessage)
teamPending ──(/grok)──> reply "team mode"
teamLocked ──(/grok)──> reply "team mode", stay locked
teamLocked ──(any)──> no action (team sees directly)
```

## 6. Two-Instance Coordination

**Problem:** When main bot invites Grok agent to a business group, Grok agent's local `groupId` differs (different databases).

**Solution:** In-process maps correlated via protocol-level `memberId` (string, same across databases).

```typescript
const pendingGrokJoins = new Map<string, number>()      // memberId → mainGroupId
const grokGroupMap = new Map<number, number>()           // mainGroupId → grokLocalGroupId
const reverseGrokMap = new Map<number, number>()         // grokLocalGroupId → mainGroupId
const grokJoinResolvers = new Map<number, () => void>()  // mainGroupId → resolve fn
```

**Flow:**
1. Main bot: `mainChat.apiAddMember(mainGroupId, grokContactId, "member")` → response `member.memberId`
2. Store: `pendingGrokJoins.set(member.memberId, mainGroupId)`
3. Grok agent receives `receivedGroupInvitation` event → `evt.groupInfo.membership.memberId` matches → `grokChat.apiJoinGroup(evt.groupInfo.groupId)` → store bidirectional mapping (but do NOT resolve waiter yet)
4. Grok agent receives `connectedToGroupMember` event → `reverseGrokMap` lookup → resolve waiter (Grok is now fully connected and can send messages)
5. Send Grok response: `grokChat.apiSendTextMessage([T.ChatType.Group, grokGroupMap.get(mainGroupId)!], text)`

**Important:** `apiJoinGroup` sends the join request, but Grok is not fully connected until the `connectedToGroupMember` event fires. Sending messages before this results in "not current member" errors.

**Grok agent event subscriptions:**
```typescript
grokChat.on("receivedGroupInvitation", async ({groupInfo}) => {
  const memberId = groupInfo.membership.memberId
  const mainGroupId = pendingGrokJoins.get(memberId)
  if (mainGroupId !== undefined) {
    pendingGrokJoins.delete(memberId)
    await grokChat.apiJoinGroup(groupInfo.groupId)
    // Set maps but don't resolve waiter — wait for connectedToGroupMember
    grokGroupMap.set(mainGroupId, groupInfo.groupId)
    reverseGrokMap.set(groupInfo.groupId, mainGroupId)
  }
})

grokChat.on("connectedToGroupMember", ({groupInfo}) => {
  const mainGroupId = reverseGrokMap.get(groupInfo.groupId)
  if (mainGroupId === undefined) return
  const resolver = grokJoinResolvers.get(mainGroupId)
  if (resolver) {
    grokJoinResolvers.delete(mainGroupId)
    resolver()
  }
})
```

## 7. Bot Initialization

**Main bot** uses `bot.run()` for setup automation (address, profile, commands), with only `events` parameter for full routing control:

```typescript
let supportBot: SupportBot  // set after bot.run returns

const [mainChat, mainUser, mainAddress] = await bot.run({
  profile: {displayName: "SimpleX Support", fullName: ""},
  dbOpts: {dbFilePrefix: config.dbPrefix},
  options: {
    addressSettings: {
      businessAddress: true,
      autoAccept: true,
      welcomeMessage: welcomeMessage(config.groupLinks),
    },
    commands: [
      {type: "command", keyword: "grok", label: "Ask Grok AI"},
      {type: "command", keyword: "team", label: "Connect to team"}
    ],
    useBotProfile: true,
  },
  events: {
    acceptingBusinessRequest: (evt) => supportBot?.onBusinessRequest(evt),
    newChatItems: (evt) => supportBot?.onNewChatItems(evt),
    chatItemUpdated: (evt) => supportBot?.onChatItemUpdated(evt),
    leftMember: (evt) => supportBot?.onLeftMember(evt),
    connectedToGroupMember: (evt) => supportBot?.onMemberConnected(evt),
    newMemberContactReceivedInv: (evt) => supportBot?.onMemberContactReceivedInv(evt),
  },
})
```

**Grok agent** uses direct ChatApi:
```typescript
const grokChat = await ChatApi.init(config.grokDbPrefix)
let grokUser = await grokChat.apiGetActiveUser()
if (!grokUser) grokUser = await grokChat.apiCreateActiveUser({displayName: "Grok AI", fullName: ""})
await grokChat.startChat()
// Subscribe Grok event handlers
grokChat.on("receivedGroupInvitation", async (evt) => supportBot?.onGrokGroupInvitation(evt))
grokChat.on("connectedToGroupMember", (evt) => supportBot?.onGrokMemberConnected(evt))
```

**Startup resolution** (after init, before event loop):
1. Read `{dbPrefix}_state.json` for persisted `grokContactId` and `teamGroupId`
2. Enable auto-accept DM contacts from group members: `sendChatCmd("/_set accept member contacts ${mainUser.userId} on")`
3. `mainChat.apiListContacts(mainUser.userId)` → log contacts list, resolve Grok contact (from state or auto-establish via `apiCreateLink` + `apiConnectActiveUser` + `wait("contactConnected", 60000)`)
4. `sendChatCmd("/_groups${mainUser.userId}")` → resolve team group (from state or auto-create via `apiNewGroup` + persist)
5. Ensure direct messages enabled on team group: `apiUpdateGroupProfile(teamGroupId, {groupPreferences: {directMessages: {enable: On}}})` for existing groups; included in `apiNewGroup` for new groups
6. Delete stale invite link (best-effort), then `apiCreateGroupLink(teamGroupId, Member)` → display, schedule 10min deletion
7. If `--team-members` provided: validate each contact ID/name pair via contacts list, fail-fast on mismatch
8. On SIGINT/SIGTERM → delete invite link with `apiDeleteGroupLink`, then exit

## 8. Event Processing

**Main bot event handlers:**

| Event | Handler | Action |
|-------|---------|--------|
| `acceptingBusinessRequest` | `onBusinessRequest` | Enable file uploads on business group via `apiUpdateGroupProfile` |
| `newChatItems` | `onNewChatItems` | For each chatItem: identify sender, extract text, dispatch to routing. Also handles `/add` in team group. |
| `chatItemUpdated` | `onChatItemUpdated` | Forward message edits to team group (update forwarded message text) |
| `leftMember` | `onLeftMember` | If customer left → cleanup grok maps. If Grok left → cleanup grok maps. If team member left → add replacement if engaged (`hasTeamMemberSentMessage`), else revert to queue (implicit). |
| `connectedToGroupMember` | `onMemberConnected` | Log for debugging |
| `newMemberContactReceivedInv` | `onMemberContactReceivedInv` | Log DM contact from team group member (auto-accepted via `/_set accept member contacts`) |

**Grok agent event handlers:**

| Event | Handler | Action |
|-------|---------|--------|
| `receivedGroupInvitation` | `onGrokGroupInvitation` | Match `memberId` → `apiJoinGroup` → set bidirectional maps (waiter NOT resolved yet) |
| `connectedToGroupMember` | `onGrokMemberConnected` | Resolve `grokJoinResolvers` waiter — Grok is now fully connected and can send messages |

We do NOT use `onMessage`/`onCommands` from `bot.run()` — all routing is done in the `newChatItems` event handler for full control over state-dependent command handling.

**Message processing in `newChatItems` (stateless):**
```typescript
// For each chatItem in evt.chatItems:
// 1. Handle /add command in team group (if groupId === teamGroup.id)
// 2. Skip non-business-chat groups
// 3. Skip groupSnd (own messages)
// 4. Skip non-groupRcv
// 5. Identify sender:
//    - Customer: sender.memberId === businessChat.customerId
//    - Team member: sender.memberContactId matches teamMembers config
// 6. For non-customer messages: forward team member messages to team group
// 7. For customer messages: derive state from group composition (getGroupComposition)
//    - Team member present → handleTeamMode
//    - Grok member present → handleGrokMode
//    - Neither present → handleNoSpecialMembers (welcome or teamQueue)
```

**Command detection** — use `util.ciBotCommand()` for `/grok` and `/team`; all other text (including unrecognized `/commands`) is routed as "other text" per spec ("Unrecognized commands: treated as normal messages in the current mode").

## 9. Message Routing Table

Customer message routing (derived state → action):

| State | Input | Actions | API Calls | Next State |
|-------|-------|---------|-----------|------------|
| `welcome` | any text | Forward to team, send queue reply, send `/add` command | `mainChat.apiSendTextMessage([Group, teamGroupId], fwd)` + `mainChat.apiSendTextMessage([Group, groupId], queueMsg)` + `mainChat.apiSendTextMessage([Group, teamGroupId], addCmd)` | `teamQueue` |
| `teamQueue` | `/grok` | Activate Grok (invite, wait join, send accumulated msgs to Grok API, relay response) | `mainChat.apiAddMember(groupId, grokContactId, "member")` + `mainChat.apiSendTextMessage([Group, groupId], grokActivatedMsg)` + wait for join + `grokChat.apiSendTextMessage([Group, grokLocalGId], grokResponse)` | `grokMode` |
| `teamQueue` | `/team` | Add team member | `mainChat.apiAddMember(groupId, teamContactId, "member")` + `mainChat.apiSendTextMessage([Group, groupId], teamAddedMsg)` | `teamPending` |
| `teamQueue` | other text | Forward to team | `mainChat.apiSendTextMessage([Group, teamGroupId], fwd)` | `teamQueue` |
| `grokMode` | `/grok` | Ignore (already in grok mode) | — | `grokMode` |
| `grokMode` | `/team` | Remove Grok, add team member | `mainChat.apiRemoveMembers(groupId, [grokMemberGId])` + `mainChat.apiAddMember(groupId, teamContactId, "member")` + `mainChat.apiSendTextMessage([Group, groupId], teamAddedMsg)` | `teamPending` |
| `grokMode` | other text | Forward to Grok API + forward to team | Grok API call + `grokChat.apiSendTextMessage([Group, grokLocalGId], response)` + `mainChat.apiSendTextMessage([Group, teamGroupId], fwd)` | `grokMode` |
| `teamPending` | `/grok` | Reply "team mode" | `mainChat.apiSendTextMessage([Group, groupId], teamLockedMsg)` | `teamPending` |
| `teamPending` | `/team` | Ignore (already team) | — | `teamPending` |
| `teamPending` | other text | No forwarding (team sees directly in group) | — | `teamPending` |
| `teamLocked` | `/grok` | Reply "team mode" | `mainChat.apiSendTextMessage([Group, groupId], teamLockedMsg)` | `teamLocked` |
| `teamLocked` | `/team` | Ignore | — | `teamLocked` |
| `teamLocked` | other text | No action (team sees directly) | — | `teamLocked` |

## 10. Team Forwarding

```typescript
async forwardToTeam(groupId: number, groupInfo: T.GroupInfo, text: string): Promise<void> {
  const name = groupInfo.groupProfile.displayName || `group-${groupId}`
  const fwd = `${name}:${groupId}: ${text}`
  await this.mainChat.apiSendTextMessage(
    [T.ChatType.Group, this.config.teamGroup.id],
    fwd
  )
}

async activateTeam(groupId: number, grokMember: T.GroupMember | undefined): Promise<void> {
  // Remove Grok immediately if present (per spec: "When switching to team mode, Grok is removed")
  if (grokMember) {
    try { await this.mainChat.apiRemoveMembers(groupId, [grokMember.groupMemberId]) } catch {}
    this.cleanupGrokMaps(groupId)
  }
  if (this.config.teamMembers.length === 0) {
    await this.sendToGroup(groupId, "No team members are available yet. Please try again later or click /grok.")
    return
  }
  const teamContactId = this.config.teamMembers[0].id
  const member = await this.addOrFindTeamMember(groupId, teamContactId)  // handles groupDuplicateMember
  if (!member) {
    await this.sendToGroup(groupId, "Sorry, there was an error adding a team member. Please try again.")
    return
  }
  await this.sendToGroup(groupId, teamAddedMessage(this.config.timezone))
}

// Helper: handles groupDuplicateMember error (team member already in group from previous session)
private async addOrFindTeamMember(groupId: number, teamContactId: number): Promise<GroupMember | null> {
  try {
    return await this.mainChat.apiAddMember(groupId, teamContactId, "member")
  } catch (err: any) {
    if (err?.chatError?.errorType?.type === "groupDuplicateMember") {
      const members = await this.mainChat.apiListMembers(groupId)
      return members.find(m => m.memberContactId === teamContactId) ?? null
    }
    throw err
  }
}
```

## 11. Grok API Integration

```typescript
class GrokApiClient {
  constructor(private apiKey: string, private docsContext: string) {}

  async chat(history: GrokMessage[], userMessage: string): Promise<string> {
    const messages = [
      {role: "system", content: this.systemPrompt()},
      ...history.slice(-20),
      {role: "user", content: userMessage},
    ]
    const resp = await fetch("https://api.x.ai/v1/chat/completions", {
      method: "POST",
      headers: {"Content-Type": "application/json", Authorization: `Bearer ${this.apiKey}`},
      body: JSON.stringify({model: "grok-3", messages, max_tokens: 2048}),
    })
    if (!resp.ok) throw new Error(`Grok API ${resp.status}: ${await resp.text()}`)
    const data = await resp.json()
    return data.choices[0].message.content
  }

  private systemPrompt(): string {
    return `You are a support assistant for SimpleX Chat, answering questions inside the app as instant messages on mobile. You are a privacy expert who knows SimpleX Chat apps, network, design choices, and trade-offs.\n\nGuidelines:\n- Be concise. Keep answers short enough to read comfortably on a phone screen.\n- Answer simple questions in 1-2 sentences.\n- For how-to questions, give brief numbered steps — no extra explanation unless needed.\n- For design questions, give the key reason in 1-2 sentences, then trade-offs only if asked.\n- For criticism, briefly acknowledge the concern and explain the design choice.\n- If you don't know something, say so honestly.\n- Do not use markdown formatting...\n- Avoid filler, preambles, and repeating the question back.\n\n${this.docsContext}`
  }
}
```

**Activating Grok** (on `/grok` in teamQueue):
1. `mainChat.apiAddMember(groupId, grokContactId, "member")` → stores `pendingGrokJoins.set(member.memberId, groupId)`
2. Send bot activation message: `mainChat.apiSendTextMessage([Group, groupId], grokActivatedMsg)`
3. Wait for Grok join via `waitForGrokJoin(groupId, 30000)` — Promise-based waiter resolved by `onGrokMemberConnected` (fires on `grokChat.connectedToGroupMember`), times out after 30s
4. Re-check group composition (user may have sent `/team` concurrently — abort if team member appeared)
5. Get accumulated customer messages from chat history via `getCustomerMessages(groupId, customerId)`
6. Call Grok API with accumulated messages
7. Re-check group composition again after API call (another event may have changed it)
8. Send response via Grok identity: `grokChat.apiSendTextMessage([Group, grokGroupMap.get(groupId)!], response)`

**Fallback:** If Grok API fails → remove Grok from group, cleanup grok maps, send "Grok temporarily unavailable" message.

## 12. One-Way Gate Logic

Per spec: "When switching to team mode, Grok is removed" and "once the user switches to team mode, /grok command is permanently disabled." Grok removal happens immediately in `activateTeam` (section 10).

**Stateless one-way gate:** The gate is derived from group composition + chat history:
- Team member present → `handleTeamMode` → `/grok` replies "team mode"
- `hasTeamMemberSentMessage()` determines teamPending vs teamLocked:
  - If team member has NOT sent a message and leaves → reverts to teamQueue (implicit, no state to update)
  - If team member HAS sent a message and leaves → replacement team member added

Timeline per spec:
1. User sends `/team` → Grok removed immediately (if present) → team member added → teamPending (derived)
2. `/grok` in teamPending → reply "team mode" (Grok already gone, command disabled)
3. Team member sends message → teamLocked (derived via `hasTeamMemberSentMessage`)
4. Any subsequent `/grok` → reply "You are now in team mode. A team member will reply to your message."

## 13. Message Templates (verbatim from spec)

```typescript
// Welcome (auto-reply via business address)
function welcomeMessage(groupLinks: string): string {
  return `Hello! Feel free to ask any question about SimpleX Chat.\n*Only SimpleX Chat team has access to your messages.* This is a SimpleX Chat team bot — it is not any LLM or AI.${groupLinks ? `\n*Join public groups*: ${groupLinks}` : ""}\nPlease send questions in English, you can use translator.`
}

// After first message (teamQueue)
function teamQueueMessage(timezone: string): string {
  const hours = isWeekend(timezone) ? "48" : "24"
  return `Your message is forwarded to the team. A reply may take up to ${hours} hours.\n\nIf your question is about SimpleX Chat, click /grok for an instant AI answer (non-sensitive questions only). Click /team to switch back any time.`
}

// Grok activated
const grokActivatedMessage = `*You are now chatting with Grok. You can send questions in any language.* Your message(s) have been forwarded.\nSend /team at any time to switch to a human team member.`

// Team added
function teamAddedMessage(timezone: string): string {
  const hours = isWeekend(timezone) ? "48" : "24"
  return `A team member has been added and will reply within ${hours} hours. You can keep describing your issue — they will see the full conversation.`
}

// Team mode locked
const teamLockedMessage = "You are now in team mode. A team member will reply to your message."
```

**Weekend detection:**
```typescript
function isWeekend(timezone: string): boolean {
  const day = new Intl.DateTimeFormat("en-US", {timeZone: timezone, weekday: "short"}).format(new Date())
  return day === "Sat" || day === "Sun"
}
```

## 14. Complete API Call Map (100% Coverage)

| # | Operation | When | ChatApi Instance | Method | Parameters | Response Type | Error Handling |
|---|-----------|------|-----------------|--------|------------|---------------|----------------|
| 1 | Init main bot | Startup | mainChat | `bot.run()` (wraps `ChatApi.init`) | dbFilePrefix, profile, addressSettings | `[ChatApi, User, UserContactLink \| undefined]` | Exit on failure |
| 2 | Init Grok agent | Startup | grokChat | `ChatApi.init(grokDbPrefix)` | dbFilePrefix | `ChatApi` | Exit on failure |
| 3 | Get/create Grok user | Startup | grokChat | `apiGetActiveUser()` / `apiCreateActiveUser(profile)` | profile: {displayName: "Grok AI"} | `User` | Exit on failure |
| 4 | Start Grok chat | Startup | grokChat | `startChat()` | — | void | Exit on failure |
| 5 | Resolve team group | Startup | mainChat | Read `{dbPrefix}_state.json` → `sendChatCmd("/_groups${userId}")` find by persisted ID, or `apiNewGroup(userId, {groupPreferences: {directMessages: {enable: On}}})` + persist | userId, groupProfile | `GroupInfo[]` / `GroupInfo` | Exit on failure |
| 5a | Ensure DM on team group | Startup (existing group) | mainChat | `apiUpdateGroupProfile(teamGroupId, {groupPreferences: {directMessages: {enable: On}}})` | groupId, groupProfile | `GroupInfo` | Exit on failure |
| 5b | Create team group invite link | Startup | mainChat | `apiDeleteGroupLink(groupId)` (best-effort) then `apiCreateGroupLink(groupId, Member)` | groupId, memberRole | `string` (invite link) | Exit on failure |
| 5c | Delete team group invite link | 10min timer or shutdown | mainChat | `apiDeleteGroupLink(groupId)` | groupId | `void` | Log error (best-effort) |
| 6 | Enable auto-accept DM contacts | Startup | mainChat | `sendChatCmd("/_set accept member contacts ${userId} on")` | userId | — | Log warning |
| 6a | List contacts | Startup | mainChat | `apiListContacts(userId)` | userId | `Contact[]` | Exit on failure |
| 6b | Validate team members | Startup (if `--team-members` provided) | mainChat | Match contacts by ID/name | contact list | — | Exit if ID:name mismatch |
| 7 | Auto-establish Grok contact | Startup (if not in state file) | mainChat | `apiCreateLink(userId)` | userId | `string` (invitation link) | Exit on failure |
| 8 | Auto-establish Grok contact | Startup (if not in state file) | grokChat | `apiConnectActiveUser(invLink)` | connLink | `ConnReqType` | Exit on failure |
| 9 | Auto-establish Grok contact | Startup (if not in state file) | mainChat | `wait("contactConnected", 60000)` | event, timeout | `ChatEvent \| undefined` | Exit on timeout |
| 10 | Send msg to customer | Various | mainChat | `apiSendTextMessage([Group, groupId], text)` | chat, text | `AChatItem[]` | Log error |
| 11 | Forward to team | welcome→teamQueue, teamQueue msg, grokMode msg | mainChat | `apiSendTextMessage([Group, teamGroupId], fwd)` | chat, formatted text | `AChatItem[]` | Log error |
| 12 | Invite Grok to group | /grok in teamQueue | mainChat | `apiAddMember(groupId, grokContactId, "member")` | groupId, contactId, role | `GroupMember` | Send error msg, stay in teamQueue |
| 13 | Grok joins group | receivedGroupInvitation | grokChat | `apiJoinGroup(groupId)` | groupId | `GroupInfo` | Log error |
| 14 | Grok sends response | After Grok API reply | grokChat | `apiSendTextMessage([Group, grokLocalGId], text)` | chat, text | `AChatItem[]` | Send error msg via mainChat |
| 15 | Invite team member | /team | mainChat | `apiAddMember(groupId, teamContactId, "member")` | groupId, contactId, role | `GroupMember` | Send error msg to customer |
| 16 | Remove Grok | /team from grokMode | mainChat | `apiRemoveMembers(groupId, [grokMemberGId])` | groupId, memberIds | `GroupMember[]` | Ignore (may have left) |
| 17 | Update bot profile | Startup (via bot.run) | mainChat | `apiUpdateProfile(userId, profile)` | userId, profile with peerType+commands | `UserProfileUpdateSummary` | Log warning |
| 18 | Set address settings | Startup (via bot.run) | mainChat | `apiSetAddressSettings(userId, settings)` | userId, {businessAddress, autoAccept, welcomeMessage} | void | Exit on failure |
| 19 | List group members | `groupDuplicateMember` fallback | mainChat | `apiListMembers(groupId)` | groupId | `GroupMember[]` | Log error |

## 15. Error Handling

| Scenario | Handling |
|----------|----------|
| ChatApi init fails | Log error, exit (let process manager restart) |
| Grok API error (HTTP/timeout) | Remove Grok from group, cleanup grok maps, send "Grok temporarily unavailable" message |
| Grok API error during conversation | Remove Grok from group, cleanup grok maps, send "Grok temporarily unavailable" message (next message → teamQueue via stateless derivation) |
| `apiAddMember` fails (Grok) | `mainChat.apiSendTextMessage` error msg, stay in teamQueue (stateless) |
| `apiAddMember` fails (team) | `mainChat.apiSendTextMessage` error msg, stay in current state (stateless) |
| `apiRemoveMembers` fails | Catch and ignore (member may have left) |
| Grok join timeout (30s) | `mainChat.apiSendTextMessage` "Grok unavailable", stay in teamQueue (stateless) |
| Customer leaves (`leftMember` where member is customer) | Cleanup grokGroupMap entry |
| Grok leaves during grokMode | Cleanup grokGroupMap entry (next message → teamQueue via stateless derivation) |
| Team member leaves (pending, not engaged) | No action needed; next message → teamQueue via stateless derivation |
| Team member leaves (locked, engaged) | Add replacement team member (`addReplacementTeamMember`) |
| Grok contact unavailable (`grokContactId === null`) | `/grok` returns "Grok is temporarily unavailable" message |
| No team members configured (`teamMembers.length === 0`) | `/team` returns "No team members are available yet" message |
| Grok agent connection lost | Log error; Grok features unavailable until restart |
| `apiSendTextMessage` fails | Log error, continue (message lost but bot stays alive) |
| Team member config validation fails | Print descriptive error with actual vs expected name, exit |
| `groupDuplicateMember` on `apiAddMember` | Catch error, call `apiListMembers` to find existing member by `memberContactId`, use existing `groupMemberId` |
| Restart: any business chat group | State derived from group composition + chat history (no explicit re-initialization needed) |

## 16. Implementation Sequence

**Phase 1: Scaffold**
- Create project: `package.json`, `tsconfig.json`
- Implement `config.ts`: CLI arg parsing, ID:name format (team members), `Config` type
- Implement `index.ts`: init both ChatApi instances, auto-resolve Grok contact and team group from state file, verify profiles
- Implement `util.ts`: `isWeekend`, logging
- **Verify:** Both instances init, print user profiles, Grok contact established, team group created

**Phase 2: Stateless event processing**
- Implement `state.ts`: `GrokMessage` type
- Implement `bot.ts`: `SupportBot` class with stateless state derivation helpers
- Handle `acceptingBusinessRequest` → enable file uploads on business group
- Handle `newChatItems` → sender identification → derive state from group composition → dispatch
- Implement welcome detection (`isFirstCustomerMessage`) + team forwarding
- Implement `messages.ts`: all templates
- **Verify:** Customer connects → welcome auto-reply → sends msg → forwarded to team group → queue reply received

**Phase 3: Grok integration**
- Implement `grok.ts`: `GrokApiClient` with system prompt + docs injection
- Implement Grok agent event handler (`receivedGroupInvitation` → auto-join)
- Implement `activateGrok`: null guard for `grokContactId`, add member, ID mapping, wait for join, Grok API call, send response via grokChat
- Implement `forwardToGrok`: ongoing message routing in grokMode
- **Verify:** `/grok` → Grok joins as separate participant → Grok responses appear from Grok profile

**Phase 4: Team mode + one-way gate**
- Implement `activateTeam`: empty teamMembers guard, remove Grok if present, add team member
- Implement `handleTeamMode`: `/grok` rejection when team member present
- Implement `hasTeamMemberSentMessage`: teamPending vs teamLocked derivation
- **Verify:** Full flow: teamQueue → /grok → grokMode → /team → Grok removed + teamPending → /grok rejected → team msg → teamLocked

**Phase 5: Polish + edge cases**
- Handle edge cases: customer leave, Grok timeout, member leave
- Team group invite link lifecycle: create on startup, delete after 10min or on shutdown
- Graceful shutdown (SIGINT/SIGTERM)
- Write `docs/simplex-context.md` for Grok prompt injection
- End-to-end test all flows

**Phase 6: Extra features (beyond MVP)**
- Edit forwarding: `chatItemUpdated` → forward edits to team group (update forwarded message)
- Team member reply forwarding: team member messages in business chats → forwarded to team group
- `/add` command: team members send `/add groupId:name` in team group → bot adds them to the customer group
- Grok group map persistence: `grokGroupMap` persisted to state file → survives restarts
- Profile images: bot and Grok agent have profile images set on startup

## 17. Self-Review Requirement

**Mandatory for all implementation subagents:**

Each code artifact must undergo adversarial self-review/fix loop:
1. Write/edit code
2. Self-review against this plan: check correctness, completeness, consistency, all state transitions covered, all API calls match the plan, all error cases handled
3. Fix any issues found
4. Repeat review until **2 consecutive zero-issue passes**
5. Only then report completion
6. User reviews and provides feedback
7. If changes needed → return to step 1 (review cycle restarts)
8. Done when: 2 clean LLM passes AND user finds no issues

Any edit restarts the review cycle. Batch changes within a round.

## 18. Verification

**Startup** (all auto-resolution happens automatically):
```bash
cd apps/simplex-support-bot
npm install
GROK_API_KEY=xai-... npx ts-node src/index.ts \
  --team-group SupportTeam \
  --timezone America/New_York \
  --group-links "https://simplex.chat/contact#..."
```

On first startup, the bot auto-establishes the Grok contact and creates the team group, persisting both IDs to `{dbPrefix}_state.json`. It prints:
```
Team group invite link (expires in 10 min):
https://simplex.chat/contact#...
```

Team members scan/click the link to join the team group. After 10 minutes, the link is deleted. On subsequent startups, the existing Grok contact and team group are resolved by persisted ID (not by name — safe even with duplicate group names) and a fresh team group invite link is created.

**With optional team members** (for pre-validated contacts):
```bash
GROK_API_KEY=xai-... npx ts-node src/index.ts \
  --team-group SupportTeam \
  --team-members 2:Alice,3:Bob \
  --timezone America/New_York
```

**Test scenarios:**
1. Connect from SimpleX client to bot's business address → verify welcome message
2. Send question → verify forwarded to team group with `CustomerName:groupId: ` prefix, queue reply received
3. Send `/grok` → verify Grok joins as separate participant, responses appear from "Grok AI" profile
4. Send text in grokMode → verify Grok response + forwarded to team
5. Send `/team` → verify Grok removed, team member added, team added message
6. Send `/grok` after `/team` (before team member message) → verify "team mode" reply
7. Send team member message → verify state locked, `/grok` still rejected
8. Test weekend: set timezone to weekend timezone → verify "48 hours" in messages
9. Customer disconnects → verify state cleanup
10. Grok API failure → verify error message, graceful fallback to teamQueue
11. Team group auto-creation: start with a new group name → verify group created, ID persisted to state file, team group invite link displayed
12. Team group invite link deletion: wait 10 minutes → verify link deleted; kill bot → verify link deleted on shutdown
13. Team group persistence: restart bot → verify same group ID used from state file (not a new group)
14. Team group recovery: delete persisted group externally → restart bot → verify new group created and state file updated
15. Grok contact auto-establish: first startup with empty state file → verify Grok contact created and persisted
16. Grok contact persistence: restart bot → verify same Grok contact ID used from state file
17. Grok contact recovery: delete persisted contact externally → restart bot → verify new contact established and state file updated
18. No team members: start without `--team-members` → send `/team` → verify "No team members are available yet" message
19. Null grokContactId: if Grok contact unavailable → send `/grok` → verify "Grok is temporarily unavailable" message
20. Restart recovery: customer message in unknown group → re-init to teamQueue, forward to team (no queue reply)
21. Restart recovery: after re-init, `/grok` works in re-initialized group
22. Grok join waiter: `onGrokGroupInvitation` alone does NOT resolve waiter — `onGrokMemberConnected` required
23. groupDuplicateMember: `/team` when team member already in group → `apiListMembers` lookup, transition to teamPending
24. groupDuplicateMember: member not found in list → error message, stay in current state
25. DM contact received: `newMemberContactReceivedInv` from team group → logged, no crash
26. Direct messages enabled on team group (via `groupPreferences`) for both new and existing groups

### Critical Reference Files

- **Native library API:** `packages/simplex-chat-nodejs/src/api.ts` (ChatApi class — all methods)
- **Bot automation:** `packages/simplex-chat-nodejs/src/bot.ts` (bot.run — setup helper)
- **Utilities:** `packages/simplex-chat-nodejs/src/util.ts` (ciContentText, ciBotCommand, chatInfoRef)
- **Types:** `packages/simplex-chat-client/types/typescript/src/types.ts` (BusinessChatInfo, GroupMember, CIDirection, etc.)
- **Events:** `packages/simplex-chat-client/types/typescript/src/events.ts` (CEvt — all event types)
- **Product spec:** `apps/multiplatform/plans/20260207-support-bot.md`
