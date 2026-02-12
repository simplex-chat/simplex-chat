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
│  conversations: Map<groupId, ConversationState>  │
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
apps/simplex-chat-support-bot/
├── package.json          # deps: simplex-chat, @simplex-chat/types
├── tsconfig.json         # ES2022, strict, Node16 module resolution
├── src/
│   ├── index.ts          # Entry: parse config, init instances, run
│   ├── config.ts         # CLI arg parsing, ID:name validation, Config type
│   ├── bot.ts            # SupportBot class: state mgmt, event dispatch, routing
│   ├── state.ts          # ConversationState union type
│   ├── grok.ts           # GrokApiClient: xAI API wrapper, system prompt, history
│   ├── messages.ts       # All user-facing message templates (verbatim from spec)
│   └── util.ts           # isWeekend, logging helpers
├── data/                 # SQLite databases (created at runtime)
└── docs/
    └── simplex-context.md  # Curated SimpleX docs injected into Grok system prompt
```

## 4. Configuration — ID:name Format

All entity references use `ID:name` format. The bot validates each pair at startup against live data from `apiListContacts()` / `apiListGroups()`.

**CLI args:**

| Arg | Required | Default | Format | Purpose |
|-----|----------|---------|--------|---------|
| `--db-prefix` | No | `./data/bot` | path | Main bot database file prefix |
| `--grok-db-prefix` | No | `./data/grok` | path | Grok agent database file prefix |
| `--team-group` | Yes | — | `ID:name` | Group for forwarding customer messages to team |
| `--team-members` | Yes | — | `ID:name,...` | Comma-separated team member contacts |
| `--grok-contact` | Yes* | — | `ID:name` | Grok agent's contactId in main bot's database |
| `--group-links` | No | `""` | string | Public group link(s) for welcome message |
| `--timezone` | No | `"UTC"` | IANA tz | For weekend detection (24h vs 48h) |
| `--first-run` | No | false | flag | Auto-establish contact between bot and Grok agent |

*`--grok-contact` required unless `--first-run` is used.

**Env vars:** `GROK_API_KEY` (required) — xAI API key.

```typescript
interface Config {
  dbPrefix: string
  grokDbPrefix: string
  teamGroup: {id: number; name: string}
  teamMembers: {id: number; name: string}[]
  grokContact: {id: number; name: string} | null  // null during first-run
  groupLinks: string
  timezone: string
  grokApiKey: string
  firstRun: boolean
}
```

**ID:name parsing:**
```typescript
function parseIdName(s: string): {id: number; name: string} {
  const i = s.indexOf(":")
  if (i < 1) throw new Error(`Invalid ID:name format: "${s}"`)
  return {id: parseInt(s.slice(0, i), 10), name: s.slice(i + 1)}
}
```

**Startup validation** (exact API calls):

| What | API Call | Validation |
|------|----------|------------|
| Team group | `mainChat.apiListGroups(userId)` → find by `groupId === config.teamGroup.id` | Assert `groupProfile.displayName === config.teamGroup.name` |
| Team members | `mainChat.apiListContacts(userId)` → find each by `contactId` | Assert `profile.displayName === member.name` for each |
| Grok contact | `mainChat.apiListContacts(userId)` → find by `contactId === config.grokContact.id` | Assert `profile.displayName === config.grokContact.name` |

Fail-fast with descriptive error on any mismatch.

## 5. State Machine

Keyed by `groupId` of business chat group. In-memory (restart resets; team group retains forwarded messages).

```typescript
type ConversationState =
  | {type: "welcome"}
  | {type: "teamQueue"; userMessages: string[]}
  | {type: "grokMode"; grokMemberGId: number; history: GrokMessage[]}
  | {type: "teamPending"; teamMemberGId: number}
  | {type: "teamLocked"; teamMemberGId: number}
```

`teamQueue.userMessages` accumulates user messages for Grok initial context on `/grok` activation.

**Transitions:**
```
welcome ──(1st user msg)──> teamQueue
teamQueue ──(user msg)──> teamQueue (append to userMessages)
teamQueue ──(/grok)──> grokMode (userMessages → initial Grok history)
teamQueue ──(/team)──> teamPending (add team member)
grokMode ──(user msg)──> grokMode (forward to Grok API, append to history)
grokMode ──(/team)──> teamPending (remove Grok immediately, add team member)
teamPending ──(team member msg)──> teamLocked
teamPending ──(/grok)──> reply "team mode"
teamLocked ──(/grok)──> reply "team mode", stay locked
teamLocked ──(any)──> no action (team sees directly)
```

## 6. Two-Instance Coordination

**Problem:** When main bot invites Grok agent to a business group, Grok agent's local `groupId` differs (different databases).

**Solution:** In-process maps correlated via protocol-level `memberId` (string, same across databases).

```typescript
const pendingGrokJoins = new Map<string, number>()  // memberId → mainGroupId
const grokGroupMap = new Map<number, number>()       // mainGroupId → grokLocalGroupId
const reverseGrokMap = new Map<number, number>()     // grokLocalGroupId → mainGroupId
```

**Flow:**
1. Main bot: `mainChat.apiAddMember(mainGroupId, grokContactId, "member")` → response `member.memberId`
2. Store: `pendingGrokJoins.set(member.memberId, mainGroupId)`
3. Grok agent receives `receivedGroupInvitation` event → `evt.groupInfo.membership.memberId` matches → `grokChat.apiJoinGroup(evt.groupInfo.groupId)` → store bidirectional mapping
4. Send Grok response: `grokChat.apiSendTextMessage([T.ChatType.Group, grokGroupMap.get(mainGroupId)!], text)`

**Grok agent event subscriptions:**
```typescript
grokChat.on("receivedGroupInvitation", async ({groupInfo}) => {
  const memberId = groupInfo.membership.memberId
  const mainGroupId = pendingGrokJoins.get(memberId)
  if (mainGroupId !== undefined) {
    pendingGrokJoins.delete(memberId)
    grokGroupMap.set(mainGroupId, groupInfo.groupId)
    reverseGrokMap.set(groupInfo.groupId, mainGroupId)
    await grokChat.apiJoinGroup(groupInfo.groupId)
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
      {type: "command", keyword: "team", label: "Switch to team"},
    ],
    useBotProfile: true,
  },
  events: {
    acceptingBusinessRequest: (evt) => supportBot?.onBusinessRequest(evt),
    newChatItems: (evt) => supportBot?.onNewChatItems(evt),
    leftMember: (evt) => supportBot?.onLeftMember(evt),
    deletedMemberUser: (evt) => supportBot?.onDeletedMemberUser(evt),
    groupDeleted: (evt) => supportBot?.onGroupDeleted(evt),
    connectedToGroupMember: (evt) => supportBot?.onMemberConnected(evt),
  },
})
```

**Grok agent** uses direct ChatApi:
```typescript
const grokChat = await ChatApi.init(config.grokDbPrefix)
let grokUser = await grokChat.apiGetActiveUser()
if (!grokUser) grokUser = await grokChat.apiCreateActiveUser({displayName: "Grok AI", fullName: ""})
await grokChat.startChat()
// Subscribe Grok event handlers (receivedGroupInvitation)
```

**First-run mode** (`--first-run`):
1. Both instances init and create users
2. Main bot: `mainChat.apiCreateLink(mainUser.userId)` → invitation link
3. Grok agent: `grokChat.apiConnectActiveUser(invLink)`
4. Main bot: `mainChat.wait("contactConnected", 60000)` — wait for connection
5. Print: "Grok contact established. ContactId=X. Use: --grok-contact X:GrokAI"
6. Exit (user restarts without `--first-run`)

**Startup validation** (after init, before event loop):
1. `mainChat.apiListContacts(mainUser.userId)` → validate `--team-members` and `--grok-contact` ID:name pairs
2. `mainChat.apiListGroups(mainUser.userId)` → validate `--team-group` ID:name pair

## 8. Event Processing

**Main bot event handlers:**

| Event | Handler | Action |
|-------|---------|--------|
| `acceptingBusinessRequest` | `onBusinessRequest` | `conversations.set(groupInfo.groupId, {type: "welcome"})` |
| `newChatItems` | `onNewChatItems` | For each chatItem: identify sender, extract text, dispatch to routing |
| `leftMember` | `onLeftMember` | If customer left → delete state. If team member left → revert to teamQueue. If Grok left during grokMode → revert to teamQueue. |
| `deletedMemberUser` | `onDeletedMemberUser` | Bot removed from group → delete state |
| `groupDeleted` | `onGroupDeleted` | Delete state, delete grokGroupMap entry |
| `connectedToGroupMember` | `onMemberConnected` | Log for debugging |

We do NOT use `onMessage`/`onCommands` from `bot.run()` — all routing is done in the `newChatItems` event handler for full control over state-dependent command handling.

**Sender identification in `newChatItems`:**
```typescript
for (const ci of evt.chatItems) {
  const {chatInfo, chatItem} = ci
  if (chatInfo.type !== "group") continue
  const groupInfo = chatInfo.groupInfo
  if (!groupInfo.businessChat) continue  // only process business chats
  const groupId = groupInfo.groupId
  const state = conversations.get(groupId)
  if (!state) continue

  if (chatItem.chatDir.type === "groupSnd") continue  // our own message
  if (chatItem.chatDir.type !== "groupRcv") continue
  const sender = chatItem.chatDir.groupMember

  const isCustomer = sender.memberId === groupInfo.businessChat.customerId
  const isTeamMember = (state.type === "teamPending" || state.type === "teamLocked")
    && sender.groupMemberId === state.teamMemberGId
  const isGrok = state.type === "grokMode"
    && state.grokMemberGId === sender.groupMemberId

  if (isGrok) continue  // skip Grok messages (we sent them via grokChat)
  if (isCustomer) onCustomerMessage(groupId, groupInfo, chatItem, state)
  else if (isTeamMember) onTeamMemberMessage(groupId, state)
}
```

**Command detection** — use `util.ciBotCommand()` for `/grok` and `/team`; all other text (including unrecognized `/commands`) is routed as "other text" per spec ("Unrecognized commands: treated as normal messages in the current mode"):
```typescript
function extractText(chatItem: T.ChatItem): string | null {
  const text = util.ciContentText(chatItem)
  return text?.trim() || null
}

// In onCustomerMessage:
const cmd = util.ciBotCommand(chatItem)
if (cmd?.keyword === "grok") { /* handle /grok */ }
else if (cmd?.keyword === "team") { /* handle /team */ }
else { /* handle as normal text message, including unrecognized /commands */ }
```

## 9. Message Routing Table

`onCustomerMessage(groupId, groupInfo, chatItem, state)`:

| State | Input | Actions | API Calls | Next State |
|-------|-------|---------|-----------|------------|
| `welcome` | any text | Forward to team, send queue reply | `mainChat.apiSendTextMessage([Group, teamGroupId], fwd)` + `mainChat.apiSendTextMessage([Group, groupId], queueMsg)` | `teamQueue` (store msg) |
| `teamQueue` | `/grok` | Activate Grok (invite, wait join, send accumulated msgs to Grok API, relay response) | `mainChat.apiAddMember(groupId, grokContactId, "member")` + `mainChat.apiSendTextMessage([Group, groupId], grokActivatedMsg)` + wait for join + `grokChat.apiSendTextMessage([Group, grokLocalGId], grokResponse)` | `grokMode` |
| `teamQueue` | `/team` | Add team member | `mainChat.apiAddMember(groupId, teamContactId, "member")` + `mainChat.apiSendTextMessage([Group, groupId], teamAddedMsg)` | `teamPending` |
| `teamQueue` | other text | Forward to team, append to userMessages | `mainChat.apiSendTextMessage([Group, teamGroupId], fwd)` | `teamQueue` |
| `grokMode` | `/grok` | Ignore (already in grok mode) | — | `grokMode` |
| `grokMode` | `/team` | Remove Grok, add team member | `mainChat.apiRemoveMembers(groupId, [grokMemberGId])` + `mainChat.apiAddMember(groupId, teamContactId, "member")` + `mainChat.apiSendTextMessage([Group, groupId], teamAddedMsg)` | `teamPending` |
| `grokMode` | other text | Forward to Grok API + forward to team | Grok API call + `grokChat.apiSendTextMessage([Group, grokLocalGId], response)` + `mainChat.apiSendTextMessage([Group, teamGroupId], fwd)` | `grokMode` (append history) |
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
  const fwd = `[${name} #${groupId}]\n${text}`
  await this.mainChat.apiSendTextMessage(
    [T.ChatType.Group, this.config.teamGroup.id],
    fwd
  )
}

async activateTeam(groupId: number, state: ConversationState): Promise<void> {
  // Remove Grok immediately if present (per spec: "When switching to team mode, Grok is removed")
  if (state.type === "grokMode") {
    try { await this.mainChat.apiRemoveMembers(groupId, [state.grokMemberGId]) } catch {}
    const grokLocalGId = grokGroupMap.get(groupId)
    grokGroupMap.delete(groupId)
    if (grokLocalGId) reverseGrokMap.delete(grokLocalGId)
  }
  const teamContactId = this.config.teamMembers[0].id  // round-robin or first available
  const member = await this.mainChat.apiAddMember(groupId, teamContactId, "member")
  this.conversations.set(groupId, {
    type: "teamPending",
    teamMemberGId: member.groupMemberId,
  })
  await this.mainChat.apiSendTextMessage(
    [T.ChatType.Group, groupId],
    teamAddedMessage(this.config.timezone)
  )
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
    return `You are a privacy expert and SimpleX Chat evangelist...\n\n${this.docsContext}`
  }
}
```

**Activating Grok** (on `/grok` in teamQueue):
1. `mainChat.apiAddMember(groupId, grokContactId, "member")` → stores `pendingGrokJoins.set(member.memberId, groupId)`
2. Send bot activation message: `mainChat.apiSendTextMessage([Group, groupId], grokActivatedMsg)`
3. Wait for Grok join: poll `grokGroupMap.has(groupId)` with 30s timeout (or use `mainChat.wait("connectedToGroupMember", pred, 30000)`)
4. Build initial Grok history from `state.userMessages`
5. Call Grok API with accumulated messages
6. Send response via Grok identity: `grokChat.apiSendTextMessage([Group, grokGroupMap.get(groupId)!], response)`
7. Transition to `grokMode` with history

**Fallback:** If Grok API fails → send error message via `mainChat.apiSendTextMessage`, keep accumulated messages, stay in `teamQueue`.

## 12. One-Way Gate Logic

Per spec: "When switching to team mode, Grok is removed" and "once the user switches to team mode, /grok command is permanently disabled." Grok removal happens immediately in `activateTeam` (section 10). The one-way gate locks the state after team member engages:

```typescript
async onTeamMemberMessage(groupId: number, state: ConversationState): Promise<void> {
  if (state.type !== "teamPending") return
  this.conversations.set(groupId, {type: "teamLocked", teamMemberGId: state.teamMemberGId})
}
```

Timeline per spec:
1. User sends `/team` → Grok removed immediately (if present) → team member added → state = `teamPending`
2. `/grok` in `teamPending` → reply "team mode" (Grok already gone, command disabled)
3. Team member sends message → `onTeamMemberMessage` → state = `teamLocked`
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
  return `Thank you for your message, it is forwarded to the team.\nIt may take a team member up to ${hours} hours to reply.\n\nClick /grok if your question is about SimpleX apps or network, is not sensitive, and you want Grok LLM to answer it right away. *Your previous message and all subsequent messages will be forwarded to Grok* until you click /team. You can ask Grok questions in any language and it will not see your profile name.\n\nWe appreciate if you try Grok: you can learn a lot about SimpleX Chat from it. It is objective, answers the way our team would, and it saves our team time.`
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
| 5 | Validate team group | Startup | mainChat | `apiListGroups(userId)` | userId | `GroupInfo[]` | Exit if ID:name mismatch |
| 6 | Validate contacts | Startup | mainChat | `apiListContacts(userId)` | userId | `Contact[]` | Exit if ID:name mismatch |
| 7 | First-run: create link | First-run | mainChat | `apiCreateLink(userId)` | userId | `string` (invitation link) | Exit on failure |
| 8 | First-run: connect | First-run | grokChat | `apiConnectActiveUser(invLink)` | connLink | `ConnReqType` | Exit on failure |
| 9 | First-run: wait | First-run | mainChat | `wait("contactConnected", 60000)` | event, timeout | `ChatEvent \| undefined` | Exit on timeout |
| 10 | Send msg to customer | Various | mainChat | `apiSendTextMessage([Group, groupId], text)` | chat, text | `AChatItem[]` | Log error |
| 11 | Forward to team | welcome→teamQueue, teamQueue msg, grokMode msg | mainChat | `apiSendTextMessage([Group, teamGroupId], fwd)` | chat, formatted text | `AChatItem[]` | Log error |
| 12 | Invite Grok to group | /grok in teamQueue | mainChat | `apiAddMember(groupId, grokContactId, "member")` | groupId, contactId, role | `GroupMember` | Send error msg, stay in teamQueue |
| 13 | Grok joins group | receivedGroupInvitation | grokChat | `apiJoinGroup(groupId)` | groupId | `GroupInfo` | Log error |
| 14 | Grok sends response | After Grok API reply | grokChat | `apiSendTextMessage([Group, grokLocalGId], text)` | chat, text | `AChatItem[]` | Send error msg via mainChat |
| 15 | Invite team member | /team | mainChat | `apiAddMember(groupId, teamContactId, "member")` | groupId, contactId, role | `GroupMember` | Send error msg to customer |
| 16 | Remove Grok | /team from grokMode | mainChat | `apiRemoveMembers(groupId, [grokMemberGId])` | groupId, memberIds | `GroupMember[]` | Ignore (may have left) |
| 17 | Update bot profile | Startup (via bot.run) | mainChat | `apiUpdateProfile(userId, profile)` | userId, profile with peerType+commands | `UserProfileUpdateSummary` | Log warning |
| 18 | Set address settings | Startup (via bot.run) | mainChat | `apiSetAddressSettings(userId, settings)` | userId, {businessAddress, autoAccept, welcomeMessage} | void | Exit on failure |

## 15. Error Handling

| Scenario | Handling |
|----------|----------|
| ChatApi init fails | Log error, exit (let process manager restart) |
| Grok API error (HTTP/timeout) | `mainChat.apiSendTextMessage` "Grok temporarily unavailable", revert to `teamQueue` |
| `apiAddMember` fails (Grok) | `mainChat.apiSendTextMessage` error msg, stay in `teamQueue` |
| `apiAddMember` fails (team) | `mainChat.apiSendTextMessage` error msg, stay in current state |
| `apiRemoveMembers` fails | Catch and ignore (member may have left) |
| Grok join timeout (30s) | `mainChat.apiSendTextMessage` "Grok unavailable", stay in `teamQueue` |
| Customer leaves (`leftMember` where member is customer) | Delete conversation state, delete grokGroupMap entry |
| Group deleted | Delete conversation state, delete grokGroupMap entry |
| Grok leaves during `grokMode` | Revert to `teamQueue`, delete grokGroupMap entry |
| Team member leaves | Revert to `teamQueue` (accumulate messages again) |
| Bot removed from group (`deletedMemberUser`) | Delete conversation state |
| Grok agent connection lost | Log error; Grok features unavailable until restart |
| `apiSendTextMessage` fails | Log error, continue (message lost but bot stays alive) |
| Config validation fails | Print descriptive error with actual vs expected name, exit |

## 16. Implementation Sequence

**Phase 1: Scaffold**
- Create project: `package.json`, `tsconfig.json`
- Implement `config.ts`: CLI arg parsing, ID:name format, `Config` type
- Implement `index.ts`: init both ChatApi instances, verify profiles
- Implement `util.ts`: `isWeekend`, logging
- **Verify:** Both instances init, print user profiles, validate config

**Phase 2: State machine + event loop**
- Implement `state.ts`: `ConversationState` union type
- Implement `bot.ts`: `SupportBot` class with `conversations` map
- Handle `acceptingBusinessRequest` → init state as `welcome`
- Handle `newChatItems` → sender identification → customer message dispatch
- Implement welcome → teamQueue transition + team forwarding
- Implement `messages.ts`: all templates
- **Verify:** Customer connects → welcome auto-reply → sends msg → forwarded to team group → queue reply received

**Phase 3: Grok integration**
- Implement `grok.ts`: `GrokApiClient` with system prompt + docs injection
- Implement Grok agent event handler (`receivedGroupInvitation` → auto-join)
- Implement `activateGrok`: add member, ID mapping, wait for join, Grok API call, send response via grokChat
- Implement `forwardToGrok`: ongoing message routing in grokMode
- **Verify:** `/grok` → Grok joins as separate participant → Grok responses appear from Grok profile

**Phase 4: Team mode + one-way gate**
- Implement `activateTeam`: remove Grok if present, add team member
- Implement `onTeamMemberMessage`: detect team msg → lock state
- Implement `/grok` rejection in `teamPending` and `teamLocked`
- **Verify:** Full flow: teamQueue → /grok → grokMode → /team → Grok removed + teamPending → /grok rejected → team msg → teamLocked

**Phase 5: Polish + first-run**
- Implement `--first-run` auto-contact establishment
- Handle edge cases: customer leave, group delete, Grok timeout, member leave
- Write `docs/simplex-context.md` for Grok prompt injection
- End-to-end test all flows

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

**First-run setup:**
```bash
cd apps/simplex-chat-support-bot
npm install
npx ts-node src/index.ts --first-run --db-prefix ./data/bot --grok-db-prefix ./data/grok
# → Prints: "Grok contact established. ContactId=X. Use: --grok-contact X:GrokAI"
```

**Normal run:**
```bash
npx ts-node src/index.ts \
  --team-group 1:SupportTeam \
  --team-members 2:Alice,3:Bob \
  --grok-contact 4:GrokAI \
  --timezone America/New_York \
  --group-links "https://simplex.chat/contact#..."
```

**Test scenarios:**
1. Connect from SimpleX client to bot's business address → verify welcome message
2. Send question → verify forwarded to team group with `[CustomerName #groupId]` prefix, queue reply received
3. Send `/grok` → verify Grok joins as separate participant, responses appear from "Grok AI" profile
4. Send text in grokMode → verify Grok response + forwarded to team
5. Send `/team` → verify Grok removed, team member added, team added message
6. Send `/grok` after `/team` (before team member message) → verify "team mode" reply
7. Send team member message → verify state locked, `/grok` still rejected
8. Test weekend: set timezone to weekend timezone → verify "48 hours" in messages
9. Customer disconnects → verify state cleanup
10. Grok API failure → verify error message, graceful fallback to teamQueue

### Critical Reference Files

- **Native library API:** `packages/simplex-chat-nodejs/src/api.ts` (ChatApi class — all methods)
- **Bot automation:** `packages/simplex-chat-nodejs/src/bot.ts` (bot.run — setup helper)
- **Utilities:** `packages/simplex-chat-nodejs/src/util.ts` (ciContentText, ciBotCommand, chatInfoRef)
- **Types:** `packages/simplex-chat-client/types/typescript/src/types.ts` (BusinessChatInfo, GroupMember, CIDirection, etc.)
- **Events:** `packages/simplex-chat-client/types/typescript/src/events.ts` (CEvt — all event types)
- **Product spec:** `apps/multiplatform/plans/20260207-support-bot.md`
