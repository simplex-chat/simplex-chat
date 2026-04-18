# SimpleX Support Bot — Implementation Plan

## 1. Executive Summary

SimpleX Chat support bot — standalone Node.js app using `simplex-chat-nodejs` native NAPI binding. Single `ChatApi` instance with two user profiles (main bot + Grok agent) sharing one SQLite database. A `profileMutex` serializes all profile-switching + SimpleX API calls. Team sees active conversations as cards in a dashboard group — no text forwarding. Implements flow: Welcome → Queue → Grok/Team-Pending → Team.

## 2. Architecture

```
┌─────────────────────────────────────────────────┐
│          Support Bot Process (Node.js)           │
│                                                  │
│  chat: ChatApi ← ChatApi.init("./data/simplex")  │
│    Single database, two user profiles            │
│                                                  │
│  mainUserId  ← "Ask SimpleX Team" profile        │
│    • Business address, event routing, state mgmt │
│    • Controls group membership                   │
│                                                  │
│  grokUserId  ← "Grok" profile                    │
│    • Joins customer groups as Member             │
│    • Sends Grok responses into groups            │
│                                                  │
│  profileMutex: serialize apiSetActiveUser + call │
│  GrokApiClient → api.x.ai/v1/chat/completions   │
└─────────────────────────────────────────────────┘
```

- Single Node.js process, single `ChatApi` instance via native NAPI
- Two user profiles in one database — resolved at startup via `apiListUsers()` by display name
- `profileMutex` serializes `apiSetActiveUser(userId)` + the subsequent SimpleX API call. Grok HTTP API calls run **outside** the mutex.
- Events delivered for all profiles — routed by `event.user` field (main → main handler, Grok → Grok handler)
- Business address auto-accept creates a group per customer
- Grok is a second profile invited as a Member — appears as a separate participant
- No cross-profile ID mapping needed — Grok profile uses its own local group IDs from its own events

## 3. Project Structure

```
apps/simplex-support-bot/
├── package.json          # deps: simplex-chat, @simplex-chat/types, async-mutex
├── tsconfig.json         # ES2022, strict, Node16 module resolution
├── src/
│   ├── index.ts          # Entry: parse config, init instance, run
│   ├── config.ts         # CLI arg parsing, ID:name validation, Config type
│   ├── bot.ts            # SupportBot class: state derivation, event dispatch, cards
│   ├── cards.ts          # Card formatting, debouncing, lifecycle
│   ├── grok.ts           # GrokApiClient: xAI API wrapper, system prompt, history
│   ├── messages.ts       # All user-facing message templates
│   └── util.ts           # isWeekend, profileMutex, logging helpers
└── data/                 # SQLite databases (created at runtime)
```

The Grok system-prompt / context file is supplied at runtime via `--context-file <path>` (see §4). It is not part of the repo tree.

## 4. Configuration

**CLI flags:**

| Flag | Required | Default | Format | Purpose |
|------|----------|---------|--------|---------|
| `--db-prefix` | No | `./data/simplex` | path | Database file prefix (both profiles share it) |
| `--team-group` | Yes | — | `name` | Team group display name (auto-created if absent, resolved by persisted ID on restarts) |
| `--auto-add-team-members` / `-a` | No | `""` | `ID:name,...` | Comma-separated team member contacts. Validated at startup — exits on mismatch. |
| `--context-file` | Required when `GROK_API_KEY` set | — | path | Grok system-prompt file (SimpleX documentation context). `parseConfig` throws if `GROK_API_KEY` is set without this flag. |
| `--timezone` | No | `"UTC"` | IANA tz | For weekend detection (24h vs 48h). Weekend = Sat 00:00 – Sun 23:59 in this tz. |
| `--complete-hours` | No | `3` | number | Hours of customer inactivity after last team/Grok reply before auto-completing a conversation (✅) |
| `--card-flush-seconds` | No | `300` | number | Seconds between card dashboard update flushes |

**Env vars:** `GROK_API_KEY` (optional) — xAI API key. If unset or empty, the bot starts with Grok support fully disabled: it logs `"No GROK_API_KEY provided, disabling Grok support"`, skips Grok profile/contact setup and event handler registration, omits `/grok` from the bot command list, drops the `/grok` clause from customer-facing messages, and treats any `/grok` the customer still types as an unknown command.

```typescript
interface Config {
  dbPrefix: string
  teamGroup: {id: number; name: string}  // id=0 at parse time, resolved at startup
  teamMembers: {id: number; name: string}[]
  grokContactId: number | null  // always restored from state file at startup (even when Grok API is disabled, so the one-way gate can identify and remove Grok members)
  timezone: string
  completeHours: number  // default 3
  cardFlushSeconds: number  // default 300
  contextFile: string | null  // path to Grok system-prompt file; required when grokApiKey !== null
  grokApiKey: string | null  // null when GROK_API_KEY is not set → Grok disabled
}
```

**State file** — `{dbPrefix}_state.json` (co-located with DB files):
```json
{"teamGroupId": 123, "grokContactId": 4}
```

Only two keys. All other state is derived from chat history, group metadata, or `customData`.

**Grok contact resolution** (state-file lookup always runs; contact establishment only when enabled):
1. Read `grokContactId` from state file → validate via `apiListContacts` → set `config.grokContactId` (this always runs, even when `grokApiKey === null`, so the one-way gate can identify and remove Grok members from groups)
2. If not found and `grokEnabled`: main profile creates one-time invite link, Grok profile connects, wait `contactConnected` (60s), persist new contact ID
3. If unavailable (with Grok otherwise enabled), bot runs but `/grok` returns "temporarily unavailable"
4. If `grokApiKey === null`: the Grok profile is not resolved or created, no invite link is issued — but `config.grokContactId` is still set from the state file if the contact exists.

**Team group resolution** (auto-create):
1. Read `teamGroupId` from state file → validate via group list
2. If not found: create with `apiNewGroup`, persist new group ID
3. If found: compare `fullGroupPreferences` (directMessages, fullDelete, commands) and displayName with desired values. Only call `apiUpdateGroupProfile` if something differs — avoids unnecessary SMP relay round-trips on every restart.

**Team group invite link lifecycle:**
1. Delete stale link (best-effort), create new link, print to stdout. Creation is best-effort — if the SMP relay is unreachable, the error is logged and the bot continues without an invite link. The 10-minute deletion timer is only scheduled if creation succeeded.
2. Delete after 10 minutes. On SIGINT/SIGTERM, delete before exit. Deletion must go through `profileMutex` with `apiSetActiveUser(mainUserId)` — the active user may be the Grok profile at the time the timer fires or the signal arrives.

**Team member validation:**
- If `--auto-add-team-members` (`-a`) provided: validate each contact ID/name pair, fail-fast on mismatch
- If not provided: `/team` tells customers "no team members available yet"

## 5. State Derivation (Stateless)

State is derived from group composition (`apiListMembers`), the group's `customData` (persisted in SimpleX's database), and chat history (only for the TEAM vs TEAM-PENDING discrimination). No in-memory conversations map — survives restarts.

**First message detection:** the group's `customData` has no `cardItemId` until the bot has produced its first response in the group. `isFirstCustomerMessage(groupId)` returns `true` iff `customData.cardItemId` is absent. No message-text introspection.

**Derived states:**

| Condition | State |
|-----------|-------|
| No `cardItemId` in `customData` | WELCOME |
| `cardItemId` present, no Grok member, no team member | QUEUE |
| Grok member present, no team member present | GROK |
| Team member present, no team member has sent a message | TEAM-PENDING |
| Team member present, team member has sent a message | TEAM |

TEAM-PENDING takes priority over GROK when both Grok and team are present (after `/team` but before team member's first message). `/grok` remains available in TEAM-PENDING — if Grok is not yet in the group, it gets invited; if already present, the command is ignored.

**State derivation helpers:**
- `getGroupComposition(groupId)` → `{grokMember, teamMembers}` from `apiListMembers`
- `isFirstCustomerMessage(groupId)` → returns `true` iff `customData.cardItemId` is absent
- `hasTeamMemberSentMessage(groupId)` → TEAM-PENDING vs TEAM from chat history
- `getLastCustomerMessageTime(groupId)` → for card wait time calculation
- `getLastTeamOrGrokMessageTime(groupId)` → for auto-complete threshold check

**Transitions:**
```
WELCOME ──(1st msg)──────> QUEUE (send queue msg, create card 🆕)
WELCOME ──(/grok 1st)────> GROK (skip queue msg, create card 🤖)
WELCOME ──(/team 1st)────> TEAM-PENDING (skip queue msg, add team members, create card 👋)
QUEUE ──(/grok)──────────> GROK (invite Grok, update card)
QUEUE ──(/team)──────────> TEAM-PENDING (add team members, update card)
GROK ──(/team)───────────> TEAM-PENDING (add all team members, Grok stays, update card)
GROK ──(user msg)────────> GROK (Grok responds, update card)
TEAM-PENDING ──(/grok)───> invite Grok if not present, else ignore (state stays TEAM-PENDING)
TEAM-PENDING ──(/team)───> reply "already invited" (guarded by customData.teamInvited)
TEAM-PENDING ──(team msg)> TEAM (remove Grok, disable /grok permanently, update card)
TEAM ──(/grok)───────────> reply "team mode", stay TEAM
```

## 6. Card-Based Dashboard

The team group is a live dashboard. The bot maintains exactly one message ("card") per active customer conversation. Cards are deleted and reposted on changes — the group is always a current snapshot.

### Card format

Card is two messages. **Message 1 (card text):**
```
[ICON] *[Customer Name]* · [wait] · [N msgs]
[STATE][· agent1, agent2, ...]
"[last message(s), truncated]"
```

**Message 2 (join command — separate single-line message):**
```
/join [id]:[name]
```

**Icons:**

| Icon | Condition |
|------|-----------|
| 🆕 | QUEUE — first message < 5 min ago |
| 🟡 | QUEUE — waiting < 2 h |
| 🔴 | QUEUE — waiting > 2 h |
| 🤖 | GROK — Grok handling |
| 👋 | TEAM — team added, no reply yet |
| 💬 | TEAM — team has replied, conversation active (customer replied after team) |
| ⏰ | TEAM — customer follow-up unanswered > 2 h |
| ✅ | Done — no customer reply for `completeHours` (default 3h) after last team/Grok message |

**State labels:** `Queue`, `Grok`, `Team – pending`, `Team`

**Agents:** comma-separated display names of team members in the group. Omitted when none.

**Message count:** All messages in chat history except the bot's own (`groupSnd` from main profile).

**Message preview:** Last several messages, most recent last, separated by ` / `. Newlines in message text are replaced with spaces to prevent card layout bloat from spam. The customer's display name is also sanitized (newlines → spaces) for the card header, but the `/join` command uses the raw name so it matches the actual group profile. Newest messages are prioritized — when the total exceeds ~1000 chars, the oldest messages are truncated (with `[truncated]` prepended) while the newest are always shown. When truncation occurs, the first visible message is guaranteed to have a sender prefix even if it was a continuation in the original sequence. Each message is prefixed with the sender's name (`Name: message`) on the first message in a consecutive run from that sender - subsequent messages from the same sender omit the prefix until a different sender's message appears. Sender identification: Grok contact is detected by `grokContactId` and labeled "Grok"; the customer is identified by matching `memberId` to the group's `customerId` and labeled with their display name; all other members use their `memberProfile.displayName`. Bot's own messages (`groupSnd`) are excluded. Each message truncated to ~200 chars. Media-only messages show type labels: `[image]`, `[file]`, `[voice]`, `[video]`.

**Join command:** `/join groupId:name` — `groupId` is the customer group's ID, `name` is the customer's display name. Names with spaces single-quoted: `/join 42:'First Last'`.

### Card lifecycle

**Tracking:** `{cardItemId, joinItemId, complete?, teamInvited?}` stored in customer group's `customData` via `apiSetGroupCustomData`. `cardItemId`/`joinItemId` are message IDs; `complete` flags the auto-completed state; `teamInvited` flags that `/team` has been invoked at least once (gates the duplicate-invite path). Read back from `groupInfo.customData`. Single source of truth — survives restarts. All writes go through `CardManager.mergeCustomData` to preserve fields across independent write paths.

**Create** — on first customer message (→ QUEUE) or `/grok` as first message (→ GROK):
1. Compose card text + `/join` command
2. Post both as separate messages via `apiSendMessages` (batch) → get two `chatItemId`s. The `/join` command MUST be a separate single-line message because SimpleX's Markdown parser (`parseMaybeMarkdownList`) only renders the full line (including arguments) as a clickable command for single-line messages; in multi-line messages the inline parser stops at whitespace.
3. Write `{cardItemId, joinItemId}` to customer group's `customData`

**Update** (delete + repost) — on every subsequent event (new customer msg, team/Grok reply, state change, agent join):
1. Read `{cardItemId, joinItemId}` from `customData`
2. Delete old card + join command via `apiDeleteChatItems([Group, teamGroupId], [cardItemId, joinItemId], "broadcast")` — ignore errors
3. Post new card text + `/join` command as two messages → get new IDs
4. Overwrite `customData` with new `{cardItemId, joinItemId}`

**Debouncing:** Card updates debounced globally — pending changes flushed every `cardFlushSeconds` seconds (default 300, configurable via `--card-flush-seconds`). Within a batch, each group's card reposted at most once with latest state.

**Wait time rules:** Time since the customer's last unanswered message. For ✅ (auto-completed) conversations, the wait field shows the literal string "done". If customer sends a follow-up, wait time resets to count from that message.

**Auto-complete:** A conversation is marked ✅ when `completeHours` (default 3h, configurable via `--complete-hours`) have passed since the last team/Grok message **without any customer reply**. The card debounce flush (every 15 min) checks elapsed time and transitions to ✅ when the threshold is met. Customer follow-up at any point — including after ✅ — reverts to the derived active icon (👋/💬/⏰ for team states, 🟡/🔴 for queue), and wait time resets from that message.

**Card icon state machine (TEAM states):**
```
Team added, no reply yet         → 👋
Team replied                     → 💬
Customer follow-up unanswered >2h → ⏰
No customer reply for completeHours → ✅
Customer sends after ✅           → back to 💬 or ⏰ (derived from wait time)
```

**Cleanup** — customer leaves: card remains (TBD retention), clear `customData`.

**Restart recovery:** On startup, `CardManager.refreshAllCards()` lists all groups, finds those with `customData.cardItemId` set and `customData.complete` not set, sorts by `cardItemId` ascending (higher ID = more recently updated), and re-posts them oldest-first so the most recently active cards end up at the bottom of the team group. Completed cards (`complete: true`) and old/pre-bot groups (no `customData`) are skipped. Old card messages are deleted before reposting; deletion failures (e.g., >24h old) are silently ignored. Individual card failures are caught and logged without aborting the batch.

### Card implementation

```typescript
class CardManager {
  private pendingUpdates = new Set<number>()  // groupIds with pending updates
  private flushInterval: NodeJS.Timeout

  constructor(private chat: ChatApi, private config: Config, private mainUserId: number,
              flushIntervalMs = 300 * 1000) {
    this.flushInterval = setInterval(() => this.flush(), flushIntervalMs)
    this.flushInterval.unref()
  }

  scheduleUpdate(groupId: number): void {
    this.pendingUpdates.add(groupId)
  }

  async createCard(groupId: number, groupInfo: T.GroupInfo): Promise<void> {
    const {text, joinCmd} = await this.composeCard(groupId, groupInfo)
    // Send card text and /join as separate messages via apiSendMessages (batch).
    // /join must be standalone single-line so the client renders it as clickable.
    const items = await this.chat.apiSendMessages(chatRef, [
      {msgContent: {type: "text", text}, mentions: {}},
      {msgContent: {type: "text", text: joinCmd}, mentions: {}},
    ])
    await this.chat.apiSetGroupCustomData(groupId, {
      cardItemId: items[0].chatItem.meta.itemId,
      joinItemId: items[1].chatItem.meta.itemId,
    })
  }

  async flush(): Promise<void> {
    const groups = [...this.pendingUpdates]
    this.pendingUpdates.clear()
    for (const groupId of groups) {
      await this.updateCard(groupId)
    }
  }

  async refreshAllCards(): Promise<void> {
    const groups = await this.chat.apiListGroups(mainUserId)
    const activeCards = groups
      .filter(g => typeof g.customData?.cardItemId === "number" && !g.customData?.complete)
      .map(g => ({groupId: g.groupId, cardItemId: g.customData.cardItemId}))
    // Sort ascending by cardItemId (higher = more recently updated)
    activeCards.sort((a, b) => a.cardItemId - b.cardItemId)
    for (const {groupId} of activeCards) {
      try { await this.updateCard(groupId) }
      catch (err) { logError(`Startup card refresh failed for group ${groupId}`, err) }
    }
  }

  private async updateCard(groupId: number): Promise<void> {
    // Read customData via apiListGroups
    const customData = ...  // {cardItemId, joinItemId?} from groupInfo.customData
    if (!customData?.cardItemId) return
    // Delete old card + join command messages
    try {
      await this.chat.apiDeleteChatItems(Group, teamGroupId,
        [customData.cardItemId, customData.joinItemId].filter(Boolean), "broadcast")
    } catch {}  // card may already be deleted
    const {text, joinCmd, complete} = await this.composeCard(groupId, groupInfo)
    const items = await this.chat.apiSendMessages(chatRef, [
      {msgContent: {type: "text", text}, mentions: {}},
      {msgContent: {type: "text", text: joinCmd}, mentions: {}},
    ])
    const data = {
      cardItemId: items[0].chatItem.meta.itemId,
      joinItemId: items[1].chatItem.meta.itemId,
      ...(complete ? {complete: true} : {}),
    }
    await this.chat.apiSetGroupCustomData(groupId, data)
  }

  private async composeCard(groupId: number, groupInfo: T.GroupInfo): Promise<{text: string, joinCmd: string, complete: boolean}> {
    // Icon, state, agents, preview (with sender-name prefixes), /join — per spec format
    // buildPreview(chatItems, customerName, customerId) — prefixes each sender's first message in a run
    // Preview messages joined with blue "/" separator: " !3 /! " (SimpleX markdown for blue colored text)
    // Message text is escaped via escapeStyledMarkdown() before joining — inserts U+200B after "!"
    // when followed by a color trigger (1-6,r,g,b,y,c,m,-) to prevent false markdown interpretation.
    // No escape mechanism exists in the SimpleX markdown parser for "!" styled text.
    // complete = (icon === "✅")
  }
}
```

## 7. Bot Initialization

**Main bot** uses `bot.run()` with `events` parameter:

```typescript
let supportBot: SupportBot

const [chat, mainUser, mainAddress] = await bot.run({
  profile: {displayName: "Ask SimpleX Team", fullName: "", image: supportImage},
  dbOpts: {dbFilePrefix: config.dbPrefix},
  options: {
    addressSettings: {
      businessAddress: true,
      autoAccept: true,
      welcomeMessage,
    },
    commands: [
      {type: "command", keyword: "grok", label: "Ask Grok"},
      {type: "command", keyword: "team", label: "Switch to team"},
    ],
    useBotProfile: true,
  },
  events: {
    acceptingBusinessRequest: (evt) => supportBot?.onBusinessRequest(evt),
    newChatItems: (evt) => supportBot?.onNewChatItems(evt),
    chatItemUpdated: (evt) => supportBot?.onChatItemUpdated(evt),
    chatItemReaction: (evt) => supportBot?.onChatItemReaction(evt),
    leftMember: (evt) => supportBot?.onLeftMember(evt),
    joinedGroupMember: (evt) => supportBot?.onJoinedGroupMember(evt),
    connectedToGroupMember: (evt) => supportBot?.onMemberConnected(evt),
    newMemberContactReceivedInv: (evt) => supportBot?.onMemberContactReceivedInv(evt),
    contactConnected: (evt) => supportBot?.onContactConnected(evt),
    contactSndReady: (evt) => supportBot?.onContactSndReady(evt),
  },
})
```

Note: `/grok` and `/team` registered as customer commands via `bot.run()`. `/join` registered as a team group command separately — after team group is resolved, call `apiUpdateGroupProfile(teamGroupId, groupProfile)` with `groupPreferences` including the `/join` command definition. Customer sending `/join` in a customer group → treated as ordinary message (unrecognized command).

**Grok profile** — resolved from same ChatApi instance:

```typescript
const users = await chat.apiListUsers()
// Accept the legacy "Grok AI" display name for profiles created before the rename.
let grokUser = users.find(u => u.displayName === "Grok" || u.displayName === "Grok AI")
if (!grokUser) {
  grokUser = await chat.apiCreateActiveUser({displayName: "Grok", fullName: "", image: grokImage})
  // apiCreateActiveUser sets Grok as active — switch back to main
  await chat.apiSetActiveUser(mainUser.userId)
} else {
  // If profile changed (e.g. new image or legacy "Grok AI" → "Grok"), update and push to contacts
  const grokProfile = {displayName: "Grok", fullName: "", image: grokImage}
  const current = util.fromLocalProfile(grokUser.profile)
  if (current.image !== grokProfile.image || current.displayName !== grokProfile.displayName || current.fullName !== grokProfile.fullName) {
    await chat.apiSetActiveUser(grokUser.userId)
    await chat.apiUpdateProfile(grokUser.userId, grokProfile)
    await chat.apiSetActiveUser(mainUser.userId)
  }
}
```

**Profile mutex** — all SimpleX API calls go through:

```typescript
import {Mutex} from "async-mutex"

const profileMutex = new Mutex()

async function withProfile<T>(userId: number, fn: () => Promise<T>): Promise<T> {
  return profileMutex.runExclusive(async () => {
    await chat.apiSetActiveUser(userId)
    return fn()
  })
}
```

Grok HTTP API calls are made **outside** the mutex to avoid blocking.

**Profile images:** Both profiles have base64-encoded JPEG profile pictures (128x128, quality 85, under the 12,500-char data URI limit enforced by iOS/Android clients) set via the `image` field in `T.Profile`. The images are defined as `data:image/jpg;base64,...` string constants in `index.ts`. The main profile image is passed to `bot.run()` which handles update-on-change automatically. The Grok profile image is passed to `apiCreateActiveUser()` on first run; on subsequent runs, the bot compares the current profile against the desired one using `util.fromLocalProfile()` and calls `apiUpdateProfile()` if any field differs — this sends the update to all Grok contacts.

**Startup sequence:**
0. **Active user recovery:** On restart, the active user may be Grok (if the previous run was killed mid-profile-switch). `bot.run()` uses `apiGetActiveUser()` and would rename Grok → `duplicateName` error. Fix: pre-init the DB with a temporary `ChatApi`, check active user, if not "Ask SimpleX Team" then `startChat()` + find the main user via `apiListUsers()` + `apiSetActiveUser()`, then `close()`. This ensures `bot.run()` always finds the correct active user.
1. `bot.run()` → init ChatApi, create/resolve main profile (with profile image), business address. Print business address link to stdout.
2. Resolve Grok profile via `apiListUsers()` (create with profile image if missing; if existing, compare profile and update via `apiUpdateProfile()` if changed — pushes to contacts)
3. Read `{dbPrefix}_state.json` for `teamGroupId` and `grokContactId`
4. Enable auto-accept DM contacts: `apiSetAutoAcceptMemberContacts(mainUser.userId, true)`
5. List contacts, resolve Grok contact (from state or auto-establish)
6. Resolve team group (from state or auto-create)
7. Ensure direct messages + delete for everyone enabled on team group (conditional — only updates profile if preferences or name differ from desired)
8. Create team group invite link (best-effort), schedule 10min deletion if created
9. Validate `--auto-add-team-members` (`-a`) if provided
10. Register Grok event handlers on `chat` (filtered by `event.user === grokUserId`)
10b. Refresh stale cards: `CardManager.refreshAllCards()` — lists all groups, skips those with `customData.complete` or no `customData.cardItemId`, sorts remaining by `cardItemId` ascending, re-posts oldest-first so newest cards land at the bottom of team group
11. On SIGINT/SIGTERM → delete invite link, exit

**Grok event registration** (same ChatApi, filtered by profile):

```typescript
chat.on("receivedGroupInvitation", async (evt) => {
  if (evt.user.userId !== grokUserId) return
  supportBot?.onGrokGroupInvitation(evt)
})
chat.on("newChatItems", async (evt) => {
  if (evt.user.userId !== grokUserId) return
  supportBot?.onGrokNewChatItems(evt)
})
chat.on("connectedToGroupMember", (evt) => {
  if (evt.user.userId !== grokUserId) return
  supportBot?.onGrokMemberConnected(evt)
})
```

## 8. Event Processing

**Main profile event handlers:**

| Event | Handler | Action |
|-------|---------|--------|
| `acceptingBusinessRequest` | `onBusinessRequest` | Enable file uploads + visible history on business group |
| `newChatItems` | `onNewChatItems` | Route: team group → handle `/join`; customer group → derive state, dispatch; direct message → reply with business address link |
| `chatItemUpdated` | `onChatItemUpdated` | Schedule card update |
| `leftMember` | `onLeftMember` | Customer left → cleanup, card remains. Grok left → cleanup. Team member left → revert if no message sent. |
| `joinedGroupMember` | `onJoinedGroupMember` | Team group joiner (link-join): initiate DM via `apiCreateMemberContact` + `apiSendMemberContactInvitation`. Fires for any member joining via group invite link. |
| `connectedToGroupMember` | `onMemberConnected` | In team group: send DM with contact ID (if not already sent by `onJoinedGroupMember`). In customer group: promote to Owner (unless customer or Grok). |
| `chatItemReaction` | `onChatItemReaction` | Team/Grok reaction in customer group → schedule card update (auto-complete) |
| `newMemberContactReceivedInv` | `onMemberContactReceivedInv` | Team group member DM contact received: send contact ID message immediately (dedup via `sentTeamDMs`) |
| `contactConnected` | `onContactConnected` | Deliver pending DM if queued (dedup via `sentTeamDMs`) |
| `contactSndReady` | `onContactSndReady` | Deliver pending DM if queued (dedup via `sentTeamDMs`) |

**Grok profile event handlers:**

| Event | Handler | Action |
|-------|---------|--------|
| `receivedGroupInvitation` | `onGrokGroupInvitation` | Look up `pendingGrokJoins`; if found, auto-accept via `apiJoinGroup`; if not found (race), buffer in `bufferedGrokInvitations` for `activateGrok` to drain |
| `connectedToGroupMember` | `onGrokMemberConnected` | Grok now fully connected — read last 100 msgs from own view, call Grok API, send initial response |
| `newChatItems` | `onGrokNewChatItems` | Batch dedup: collect last customer text message per group in the event. Skip groups with `grokInitialResponsePending` set (initial combined response in flight). For the selected message: read last 100 msgs, call Grok API, send response. Non-text (images, files, voice) → ignored by Grok (card update handled by main profile). |

**Message routing in `onNewChatItems` (main profile):**

```typescript
// For each chatItem:
// 1. Direct message (not group) → reply with business address link, stop
// 2. Team group (groupId === teamGroupId) → handle /join command
// 3. Skip non-business-chat groups
// 4. Skip groupSnd (own messages)
// 5. Identify sender via businessChat.customerId
// 6. Team member message → check if first team text (trigger one-way gate: remove Grok, disable /grok), schedule card update
// 7. Team member or Grok reaction → schedule card update (auto-complete)
// 8. Customer message → derive state, dispatch:
//    - WELCOME: create card, send queue msg (or handle /grok first msg → WELCOME→GROK, skip queue)
//    - QUEUE: /grok → invite Grok; /team → add ALL configured team members; else schedule card update
//    - GROK: /team → add ALL configured team members (Grok stays); else schedule card update
//    - TEAM-PENDING: /grok → invite Grok if not present, else ignore; /team → reply "already invited" (check `customData.teamInvited`); else no action
//    - TEAM: /grok → reply "team mode"; else no action
```

## 9. One-Way Gate

The gate is stateless — derived from group composition + chat history. The initial `/team` guard against duplicate invites now uses the `customData.teamInvited` flag rather than history scanning.

1. User sends `/team` → ALL configured `--auto-add-team-members` (`-a`) added to group (promoted to Owner on connect) → Grok stays if present → TEAM-PENDING
2. Repeat `/team` → detected via `customData.teamInvited === true` → reply with `teamAlreadyInvitedMessage`
3. `/grok` still works in TEAM-PENDING (if Grok not present, invite it; if present, ignore — Grok responds to customer messages)
4. Any team member sends first text message in customer group → **gate triggers**:
   - Remove Grok from group (`apiRemoveMembers`)
   - `/grok` permanently disabled → replies: "You are now in team mode. A team member will reply to your message."
   - State = TEAM (derived via `hasTeamMemberSentMessage`)
5. Detection: in `onNewChatItems`, when sender is a team member, check `hasTeamMemberSentMessage` — if this is the first, trigger gate.

**Edge cases:**
- All team members leave before sending → reverts to QUEUE (stateless)
- Team member leaves after sending → state stays TEAM (derived from chat history); customer can send `/team` again to re-add team members

## 10. Grok Integration

Grok is a **second user profile** in the same ChatApi instance. Self-contained: watches its own events, reads history from its own view, calls Grok HTTP API, sends responses.

### Grok-disabled mode (no `GROK_API_KEY`)

If `GROK_API_KEY` is unset or empty, `parseConfig` returns `grokApiKey: null` (via `process.env.GROK_API_KEY || null`, so `GROK_API_KEY=` is treated the same as unset; no throw) and `index.ts` derives `grokEnabled = config.grokApiKey !== null`. When `grokEnabled === false`:

- Startup logs: `"No GROK_API_KEY provided, disabling Grok support"`.
- **`config.grokContactId` is still restored from the state file** (the lookup runs unconditionally before the `if (grokEnabled)` block). This ensures `getGroupComposition` can identify Grok members so the one-way gate can remove them when a team member sends a text message — even while Grok API is disabled. Without this, Grok members would become "phantom" members: physically present in groups but invisible to the state machine, preventing the gate from firing and causing dual responses (Grok + team) if Grok is later re-enabled.
- The Grok profile is not resolved or created (no `apiListUsers`/`apiCreateActiveUser` for "Grok"; no invite link issued).
- `GrokApiClient` is not instantiated.
- `SupportBot` receives `grokApi = null` and `grokUserId = null`.
- Bot command list registered at startup contains only `/team` — `/grok` is not advertised.
- Grok event handlers (`receivedGroupInvitation`, `connectedToGroupMember`, Grok-side `newChatItems`) are not registered. Handlers that are shared with the main profile (e.g. `onMemberConnected`) remain correct because their Grok checks are guarded by `this.config.grokContactId !== null`.
- Customer-facing messages (`queueMessage`, `noTeamMembersMessage`) accept a `grokEnabled` flag and drop the `/grok` clause when false.
- If the customer still types `/grok` manually, `processMainChatItem` rewrites `cmd` to `null` when `rawCmd?.keyword === "grok" && !this.grokEnabled`, so the dispatcher treats it as an unrecognized command (same as any other plain text).
- Defense in depth: `activateGrok` and `processGrokChatItem` short-circuit on entry when `this.grokApi === null`; `withGrokProfile` throws if called with `grokUserId === null`.

Type signatures affected:
- `Config.grokApiKey: string | null`
- `SupportBot` constructor: `grokApi: GrokApiClient | null, grokUserId: number | null`
- `queueMessage(timezone: string, grokEnabled: boolean): string`
- `noTeamMembersMessage(grokEnabled: boolean): string` (was a plain `const string`)

### Grok join flow

**Critical:** `activateGrok` awaits `waitForGrokJoin(120s)` which depends on future events dispatched through the same sequential event loop (`runEventsLoop` in api.ts). Awaiting it in an event handler deadlocks — the event loop is blocked waiting for events it can't dispatch. **Solution:** All `activateGrok` calls use `fireAndForget()` — tracked but not awaited. Tests call `bot.flush()` to await completion.

**Main profile side (invite + failure detection):**
0. Send `grokInvitingMessage` ("Inviting Grok, please wait...")
1. `apiAddMember(groupId, grokContactId, Member)` → get `member.memberId`. If `groupDuplicateMember` (customer sent `/grok` again before join completed), silent return — the in-flight activation handles the outcome.
2. Store `pendingGrokJoins.set(memberId, mainGroupId)`
3. Drain `bufferedGrokInvitations` — if the `receivedGroupInvitation` event arrived during step 1's await (race condition), process it now.
4. Set `grokInitialResponsePending.add(groupId)` — suppresses per-message responses from `onGrokNewChatItems` for this group until the initial combined response completes. Without this gate, the message backlog arriving via `newChatItems` would trigger individual per-message responses racing with the initial combined response — producing duplicate replies (e.g., 3 replies for 2 messages).
5. `waitForGrokJoin(120s)` — awaits resolver from Grok profile's `connectedToGroupMember` (step 8 below)
6. Timeout → notify customer (`grokUnavailableMessage`), send queue message if was WELCOME→GROK, fall back to QUEUE, clear `grokInitialResponsePending`

**Grok profile side (independent, triggered by its own events):**
7. `receivedGroupInvitation` → look up `pendingGrokJoins` by `evt.groupInfo.membership.memberId`. If found, auto-accept via `apiJoinGroup(groupId)`, set up `grokGroupMap` and `reverseGrokMap`. If not found (race: event arrived before step 2), buffer in `bufferedGrokInvitations` for step 3. Grok is NOT yet connected — cannot read history or send messages.
8. `connectedToGroupMember` → Grok now fully connected. Uses `reverseGrokMap` to find `mainGroupId`, resolves `grokJoinResolvers` — this unblocks step 5.

**Back in `activateGrok` (after step 5 resolves):**
9. Read visible history — last 100 messages — build Grok API context (customer messages → `user` role)
10. If no customer messages found (visible history disabled or API failed), send generic greeting asking customer to repeat their question
11. Call Grok HTTP API (outside mutex)
12. Send response via `apiSendTextMessage` (through mutex with Grok profile)
13. Clear `grokInitialResponsePending` (via `finally` block — runs on success, failure, or early return). After this, per-message responses from `onGrokNewChatItems` resume normally for subsequent customer messages.

```typescript
const pendingGrokJoins = new Map<string, number>()       // memberId → mainGroupId
const bufferedGrokInvitations = new Map<string, CEvt.ReceivedGroupInvitation>()  // memberId → buffered event
const grokGroupMap = new Map<number, number>()            // mainGroupId → grokLocalGroupId
const reverseGrokMap = new Map<number, number>()          // grokLocalGroupId → mainGroupId
const grokJoinResolvers = new Map<number, () => void>()   // mainGroupId → resolve fn
const grokInitialResponsePending = new Set<number>()      // mainGroupIds where activateGrok is sending initial response
```

### Per-message Grok conversation

Grok profile's `onGrokNewChatItems` handler:
1. **Batch deduplication:** When multiple customer messages arrive in a single `newChatItems` event (e.g., rapid messages delivered as a batch), collect the last customer message per group. Only the last message triggers a Grok API call — earlier messages are included in the history context via `apiGetChat`. Without this, each message in the batch would trigger a separate API call, and earlier calls would include later messages in their history (already in the group) — producing incoherent responses that reference messages "from the future" and duplicate replies.
2. **Initial response gate:** Skip groups where `grokInitialResponsePending` is set (checked via `reverseGrokMap` to translate Grok's local groupId to mainGroupId). This prevents per-message responses from racing with the initial combined response in `activateGrok`.
3. Only trigger for `groupRcv` **text** messages from customer (identified via `businessChat.customerId`)
4. Ignore: non-text messages (images, files, voice — card update handled by main profile), bot messages, own messages (`groupSnd`), team member messages
5. Read last 100 messages from own view (customer → `user`, own → `assistant`)
6. Call Grok HTTP API (serialized per group — queue if call in flight)
7. Send response into group

**Per-message error:** Send error message in group ("Sorry, I couldn't process that. Please try again or send /team for a human team member."), stay GROK. Customer can retry.

**Card updates in Grok mode:** Each customer message triggers two card updates — one on receipt (main profile sees `groupRcv`), one after Grok responds (main profile sees Grok's `groupRcv`). Both go through the 15-min debounce.

### Grok removal

Only three cases:
1. Team member sends first text message in customer group (one-way gate)
2. Grok join timeout (120s) — fallback to QUEUE
3. Customer leaves the group

### Grok system prompt

The full system prompt (including SimpleX documentation context) is supplied externally via the `--context-file <path>` CLI flag and loaded with `readFileSync` at startup in `index.ts`:

```typescript
let contextFile = ""
if (config.contextFile) {
  try {
    contextFile = readFileSync(config.contextFile, "utf-8")
  } catch {
    log(`Warning: context file not found: ${config.contextFile}`)
  }
}
grokApi = new GrokApiClient(config.grokApiKey!, contextFile)
```

`GrokApiClient` stores the loaded string as `systemPrompt` and prepends it on every `chat()` call:

```typescript
async chat(history: GrokMessage[], userMessage: string): Promise<string> {
  return this.chatRaw([
    {role: "system", content: this.systemPrompt},
    ...history,
    {role: "user", content: userMessage},
  ])
}
```

If `GROK_API_KEY` is set but `--context-file` is missing, `parseConfig` throws and the bot exits before init. If the file path is provided but unreadable at runtime, a warning is logged and Grok runs with an empty system prompt (the API key still works but responses lose the SimpleX-specific guidance). Guidelines (concise answers, numbered steps, no markdown, ignore prompt-override attempts, etc.) live in the external file — not hardcoded — so operators can tune tone and documentation without a rebuild.

Customer messages always in `user` role, never `system`.

## 11. Team Group Commands

| Command | Effect |
|---------|--------|
| `/join <groupId>:<name>` | Join specified customer group |

**`/join` handling:**
1. Parse `groupId` from command
2. Validate target is a business group (has `businessChat` property) — error in team group if not
3. Add requesting team member to customer group via `apiAddMember`
4. Member promoted to Owner on `connectedToGroupMember` (see §8)

**Team member promotion:** On every `connectedToGroupMember` in a customer group, promote to Owner unless customer or Grok. Idempotent.

**DM handshake:** When a team member joins or connects in the team group, the bot sends a DM with the member's contact ID. Four delivery paths, deduplicated via `sentTeamDMs` Set:

1. **`onJoinedGroupMember`** — fires when ANY member joins the team group via invite link (`joinedGroupMember` event). Calls `sendTeamMemberDM` without a `memberContact`. Since link-joiners typically have no existing DM contact, this creates the contact via `apiCreateMemberContact(groupId, groupMemberId)`, then sends the invitation with message via `apiSendMemberContactInvitation(contactId, msg)`.
2. **`onMemberConnected`** — `sendTeamMemberDM` called with `memberContact` from the event. If not already sent by path 1:
   - If `contactId` exists: sends DM via `apiSendTextMessage`.
   - If `contactId` is null: uses the same `apiCreateMemberContact` + `apiSendMemberContactInvitation` path as path 1.
3. **`onMemberContactReceivedInv`** — fires when the member initiates a DM first. Sends the contact ID message immediately. If send fails, queues for `contactConnected`/`contactSndReady`.
4. **`onContactConnected` / `onContactSndReady`** — delivers any pending DM queued by paths 1, 2, or 3.

DM message:
> Added you to be able to invite you to customer chats later, keep this contact. Your contact ID is `N:name`

## 12. Message Templates

```typescript
const welcomeMessage = `Hello! This is a *SimpleX team* support bot - not an AI.
Please ask any question about SimpleX Chat.`

function queueMessage(timezone: string, grokEnabled: boolean): string {
  const hours = isWeekend(timezone) ? "48" : "24"
  const base = `The team will reply to your message within ${hours} hours.`
  if (!grokEnabled) return base
  return `${base}

If your question is about SimpleX, click /grok for an *instant Grok answer*.

Send /team to switch back.`
}

const grokActivatedMessage = `*You are chatting with Grok* - use any language.`

function teamAddedMessage(timezone: string, grokPresent: boolean): string {
  const hours = isWeekend(timezone) ? "48" : "24"
  const base = `We will reply within ${hours} hours.`
  if (!grokPresent) return base
  return `${base}
Grok will be answering your questions until then.`
}

const teamAlreadyInvitedMessage = "A team member has already been invited to this conversation and will reply when available."

const teamLockedMessage = "You are now in team mode. A team member will reply to your message."

function noTeamMembersMessage(grokEnabled: boolean): string {
  return grokEnabled
    ? "No team members are available yet. Please try again later or click /grok."
    : "No team members are available yet. Please try again later."
}

const grokInvitingMessage = "Inviting Grok, please wait..."

const grokUnavailableMessage = "Grok is temporarily unavailable. Please try again later or send /team for a human team member."

const grokErrorMessage = "Sorry, I couldn't process that. Please try again or send /team for a human team member."

const grokNoHistoryMessage = "I just joined but couldn't see your earlier messages. Could you repeat your question?"
```

`teamAddedMessage` takes a second `grokPresent` argument — when the customer switches from GROK → TEAM-PENDING (Grok still in the group until the gate triggers), the message appends a second line telling the customer Grok will keep answering until the team replies. Callers detect this by checking the current group composition for a Grok member before sending.

**Weekend detection:**
```typescript
function isWeekend(timezone: string): boolean {
  const day = new Intl.DateTimeFormat("en-US", {timeZone: timezone, weekday: "short"}).format(new Date())
  return day === "Sat" || day === "Sun"
}
```

## 13. Direct Message Handling

If a user contacts the bot via a regular direct-message address (not business address), the bot replies with the business address link and does not continue the conversation. The reply is guarded by `chatItem.content.type === "rcvMsgContent"` — only actual text messages trigger the business address reply. System events on the DM contact (e.g. `contactConnected`, `rcvDirectEvent`) are ignored to prevent spam.

## 14. Persistent State

**State file:** `{dbPrefix}_state.json` — only two keys:

| Key | Type | Why persisted |
|-----|------|---------------|
| `teamGroupId` | number | Team group created once on first run |
| `grokContactId` | number | Bot↔Grok contact takes 60s to establish |

**Not persisted:**

| State | Where it lives |
|-------|---------------|
| `cardItemId`, `joinItemId`, `complete`, `teamInvited` | Customer group's `customData` |
| User profile IDs | Resolved via `apiListUsers()` by display name |
| Message counts, timestamps | Derived from chat history |
| Customer name | Group display name |
| `pendingGrokJoins` | In-flight during 120s window only |
| `grokInitialResponsePending` | In-flight during `activateGrok` initial response only |
| Owner promotion | Idempotent on every `memberConnected` |

**Failure modes:**
- State file deleted → new team group created, Grok contact re-established (60s delay)
- Grok remains in groups it was already in — self-contained, continues responding via own events

## 15. Error Handling

| Scenario | Handling |
|----------|----------|
| ChatApi init fails | Exit (let process manager restart) |
| Active user is Grok on restart | Pre-init DB, find main user, set active, close — before `bot.run()` |
| Grok join timeout (120s) | Notify customer, fall back to QUEUE |
| Grok API error (initial or per-message) | Send error in group, stay GROK. Customer can retry or `/team`. |
| `apiAddMember` fails | Send error msg, stay in current state |
| `groupDuplicateMember` on Grok invite | Silent return — in-flight activation handles the outcome (customer sent `/grok` again before join completed) |
| `apiRemoveMembers` fails | Ignore (member may have left) |
| `apiDeleteChatItems` fails (card) | Ignore, post new card, overwrite `customData` |
| Customer leaves | Cleanup in-memory state, card remains |
| Team member leaves (no message sent) | Revert to QUEUE (stateless) |
| Team member leaves (message sent) | Logged; customer can `/team` to re-add |
| No `--auto-add-team-members` (`-a`) configured | `/team` → "no team members available yet" |
| `grokContactId` unavailable | `/grok` → "temporarily unavailable" |
| `groupDuplicateMember` | Catch, `apiListMembers` to find existing member |

## 16. API Call Map

| # | Operation | Instance | Method | When |
|---|-----------|----------|--------|------|
| 1 | Init bot | main | `bot.run()` | Startup |
| 2 | List users | chat | `apiListUsers()` | Startup — resolve profiles |
| 3 | Create Grok user | chat | `apiCreateActiveUser()` | First run |
| 4 | Set active user | chat | `apiSetActiveUser(userId)` | Before every API call (via mutex) |
| 5 | Resolve team group | main | `apiNewGroup()` / state file | Startup |
| 6 | Create team invite link | main | `apiCreateGroupLink()` | Startup |
| 7 | Delete team invite link | main | `apiDeleteGroupLink()` | 10min / shutdown |
| 8 | Auto-accept DM | main | `apiSetAutoAcceptMemberContacts(userId, true)` | Startup |
| 9 | List contacts | main | `apiListContacts()` | Startup — validate members |
| 10 | Establish Grok contact | main+grok | `apiCreateLink()` + `apiConnectActiveUser()` | First run |
| 11 | Update group profile | main | `apiUpdateGroupProfile()` | Business request; startup (conditional — only if preferences differ) |
| 12 | Send msg to customer | main | `apiSendTextMessage([Group, gId], text)` | Various |
| 13 | Post card to team group | main | `apiSendMessages(chatRef, [{card text}, {/join cmd}])` | Card create/update — two messages per card |
| 14 | Delete card + join cmd | main | `apiDeleteChatItems([Group, teamGId], [cardItemId, joinItemId], "broadcast")` | Card update |
| 15 | Set customData | main | `apiSetGroupCustomData(gId, data)` | Card lifecycle |
| 16 | Invite Grok | main | `apiAddMember(gId, grokContactId, Member)` | `/grok` |
| 17 | Grok joins | grok | `apiJoinGroup(gId)` | `receivedGroupInvitation` |
| 18 | Grok reads history | grok | `apiGetChat([Group, gId], 100)` | After join + per message |
| 19 | Grok sends response | grok | `apiSendTextMessage([Group, gId], text)` | After API call |
| 20 | Add team member | main | `apiAddMember(gId, teamContactId, Member)` | `/team`, `/join` |
| 21 | Promote to Owner | main | `apiSetMembersRole(gId, [memberId], Owner)` | `connectedToGroupMember` |
| 22 | Remove Grok | main | `apiRemoveMembers(gId, [memberId])` | Gate trigger / timeout / leave |
| 23 | List members | main | `apiListMembers(gId)` | State derivation, duplicate check |
| 24 | Register team commands | main | `apiUpdateGroupProfile(teamGId, profile)` | Startup — register `/join` in team group |
| 25 | Get group info | main | `apiListGroups()` + find by ID | Card compose — read `customData.cardItemId` from `groupInfo` |
| 26 | Create DM contact | main | `apiCreateMemberContact(gId, memberId)` | `joinedGroupMember` / `onMemberConnected` — bot-initiated DM with team member |
| 27 | Send DM invitation | main | `apiSendMemberContactInvitation(contactId, msg)` | After #26 — sends invite with message in one step |

## 17. Implementation Sequence

**Phase 1: Scaffold**
- `package.json`, `tsconfig.json`, `config.ts`, `util.ts` (isWeekend, profileMutex)
- `index.ts`: init ChatApi, resolve both profiles, state file, startup sequence
- **Verify:** Instance inits, profiles resolved, Grok contact established, team group created

**Phase 2: Event processing + cards**
- `bot.ts`: SupportBot class, state derivation helpers, event dispatch
- `cards.ts`: CardManager — format, debounce, lifecycle (create/update/cleanup)
- `messages.ts`: all templates
- Handle `acceptingBusinessRequest` → enable file uploads + visible history
- Handle `newChatItems` → WELCOME/QUEUE routing, card creation
- Handle DM → reply with business address link
- **Verify:** Customer connects → welcome → sends msg → card appears in team group → queue reply

**Phase 3: Grok integration**
- `grok.ts`: GrokApiClient with system prompt + docs
- Grok event handlers (invitation → join, newChatItems → respond)
- `/grok` activation: invite, wait join, Grok reads history + responds independently
- `/grok` as first message (WELCOME → GROK, skip queue)
- Per-message Grok conversation + serialization per group
- **Verify:** `/grok` → Grok joins as separate participant → responds from "Grok"

**Phase 4: Team mode + one-way gate**
- `/team` → add team members, Grok stays
- One-way gate: detect first team text → remove Grok, disable `/grok`
- `/join` command in team group (validate business group, add member, promote Owner)
- DM handshake with team members
- Team member promotion on `connectedToGroupMember`
- **Verify:** Full flow: QUEUE → /grok → GROK → /team → TEAM-PENDING → team msg → TEAM

**Phase 5: Polish**
- Edge cases: customer leave, Grok timeout, member leave, restart recovery
- Team group invite link lifecycle
- Graceful shutdown
- Supply Grok context via `--context-file <path>` at runtime (required when `GROK_API_KEY` is set)
- End-to-end test all flows

## 18. Self-Review Requirement

Each code artifact must undergo adversarial self-review/fix loop:
1. Write/edit code
2. Self-review against this plan: correctness, completeness, all state transitions, all API calls, all error cases
3. Fix issues found
4. Repeat until **2 consecutive zero-issue passes**
5. Report completion → user reviews → if changes needed, restart from step 1

## 19. Verification

**Startup:**
```bash
cd apps/simplex-support-bot
npm install
# With Grok support:
GROK_API_KEY=xai-... npx ts-node src/index.ts \
  --team-group SupportTeam \
  --timezone America/New_York \
  --context-file ./context.md

# Without Grok (logs "No GROK_API_KEY provided, disabling Grok support"):
npx ts-node src/index.ts \
  --team-group SupportTeam \
  --timezone America/New_York
```

**Test scenarios:**
1. Connect → verify welcome message, business address link printed to stdout
2. Send question → verify card appears in team group (🆕), queue reply received
3. `/grok` → verify Grok joins, responses from "Grok", card updates to 🤖
4. `/grok` as first message → verify WELCOME→GROK, no queue message, card 🤖
5. `/team` in GROK → verify team added, Grok stays, card 👋 Team-pending
6. `/grok` in TEAM-PENDING → verify Grok still responds
7. Team member sends text → verify Grok removed, `/grok` rejected, card → 💬
8. `/grok` in TEAM → verify "team mode" rejection
9. `/team` when already invited → verify "already invited" message
10. Card debouncing: multiple rapid events → verify single card update per 15min flush
11. `/join` from team group → verify team member added to customer group, promoted to Owner
12. `/join` with non-business group → verify error
13. Weekend → verify "48 hours"
14. Customer leaves → verify cleanup, card remains
15. Grok timeout → verify fallback to QUEUE, queue message sent
16. Grok API error (per-message) → verify error in group, stays GROK
17. Grok no-history fallback → verify generic greeting sent
18. Non-text message in GROK mode → verify no Grok API call, card updated
19. Team/Grok reaction → verify card auto-complete (✅ icon, "done")
20. DM contact text message → verify business address link reply
21. DM contact non-message event (e.g. contactConnected) → verify no reply (rcvMsgContent guard)
22. DM handshake via `joinedGroupMember` → team member joins team group via link → verify `apiCreateMemberContact` + `apiSendMemberContactInvitation` called, contact ID message sent
23. DM handshake via `connectedToGroupMember` → verify contact ID message sent (dedup with #22)
24. Restart → verify same team group + Grok contact from state file, cards resume via `customData`
25. No `--auto-add-team-members` (`-a`) → `/team` → verify "no team members available"
26. `groupDuplicateMember` → verify `apiListMembers` fallback
27. Team member leaves (no message sent) → verify revert to QUEUE
28. Team member leaves (message sent), customer sends `/team` → verify re-adds team members
29. Card preview sender prefixes → verify first message in each consecutive sender run gets `Name:` prefix, subsequent same-sender messages do not
30. `/team` after all team members left → verify re-adds team members (not "already invited")

### Critical Reference Files

- **Native library API:** `packages/simplex-chat-nodejs/src/api.ts`
- **Bot automation:** `packages/simplex-chat-nodejs/src/bot.ts`
- **Utilities:** `packages/simplex-chat-nodejs/src/util.ts`
- **Types:** `packages/simplex-chat-client/types/typescript/src/types.ts`
- **Events:** `packages/simplex-chat-client/types/typescript/src/events.ts`
- **Product spec:** `apps/simplex-support-bot/plans/20260207-support-bot.md`

## 20. Testing

Vitest 1.x (Node 18 compatible). All tests verify **observable behavior** — messages sent, members added/removed, cards posted/deleted, API calls made — never internal state.

### 20.1 Mock Infrastructure

**Approach:** Vite resolve aliases redirect native-dependent packages to lightweight JS stubs at build time. Tests import from TypeScript source (`./src/bot.js`) — Vitest transpiles inline, so mocks apply before any code runs.

**Files:**

| File | Purpose |
|------|---------|
| `bot.test.ts` | All tests (co-located with source) |
| `vitest.config.ts` | Resolve aliases, globals, timeout |
| `test/__mocks__/simplex-chat.js` | CJS stub: `api.ChatApi`, `util.ciContentText`, `util.ciBotCommand`, `util.contactAddressStr` |
| `test/__mocks__/simplex-chat-types.js` | CJS stub: `T.ChatType`, `T.GroupMemberRole`, `T.GroupMemberStatus`, `T.GroupFeatureEnabled`, `T.CIDeleteMode` |

```typescript
// vitest.config.ts
export default defineConfig({
  test: { globals: true, testTimeout: 10000 },
  resolve: {
    alias: {
      "simplex-chat": path.resolve(__dirname, "test/__mocks__/simplex-chat.js"),
      "@simplex-chat/types": path.resolve(__dirname, "test/__mocks__/simplex-chat-types.js"),
    },
  },
})
```

**`MockChatApi`** — inline class in `bot.test.ts`:

- **Tracking arrays:** `sent`, `added`, `removed`, `joined`, `deleted`, `customData`, `roleChanges`, `profileUpdates`, `memberContacts`, `memberContactInvitations`
- **Simulated DB:** `members` (Map), `chatItems` (Map), `groups` (Map), `activeUserId`
- **Failure injection:** `apiAddMemberWillFail(err?)`, `apiDeleteChatItemsWillFail()`
- **Query helpers:** `sentTo(groupId)`, `lastSentTo(groupId)`, `sentDirect(contactId)`
- `apiSendTextMessage` returns `[{chatItem: {meta: {itemId: N}}}]` — auto-incrementing IDs
- `apiGetChat` returns from `chatItems` map with `chatInfo.groupInfo` from `groups` map
- `apiCreateMemberContact(groupId, groupMemberId)` — returns a contact object with auto-incrementing `contactId`. Tracks calls in `memberContacts` array.
- `apiSendMemberContactInvitation(contactId, msg)` — returns a contact object. Tracks calls in `memberContactInvitations` array.

**`MockGrokApi`** — inline class:

- `calls` array tracks `{history, message}` for each `chat()` call
- `willRespond(text)` / `willFail()` control responses
- Resets to default response `"Grok answer"` after each failure

**Key design:** no `vi.mock()` hoisting — resolve aliases intercept all `require()`/`import()` before module evaluation. Console output silenced via `vi.spyOn(console, "log/error")`.

### 20.2 Factory Helpers & Event Builders

Tests construct events via composable helpers:

```typescript
// Factory helpers
makeConfig(overrides?)                // Config with defaults (team group, 2 team members, UTC)
makeGroupInfo(groupId, opts?)         // GroupInfo with businessChat, customerId, etc.
makeUser(userId)                      // {userId, profile: {displayName}}
makeChatItem(opts)                    // ChatItem with dir/text/memberId/msgType
makeAChatItem(chatItem, groupId?)     // AChatItem wrapping chatItem + groupInfo

// Member factories — typed member objects
makeTeamMember(contactId, name?, groupMemberId?)  // team member with standard memberId pattern
makeGrokMember(groupMemberId?)                     // Grok member (default groupMemberId=7777)
makeCustomerMember(status?)                        // customer member

// Event builders — return full newChatItems events
customerMessage(text, groupId?)                    // from customer in customer group
customerNonTextMessage(groupId?)                   // non-text (image) from customer
teamMemberMessage(text, contactId?, groupId?)      // from team member
grokResponseMessage(text, groupId?)                // from Grok in customer group
directMessage(text, contactId)                     // from direct contact
teamGroupMessage(text, senderContactId?)           // in team group
grokViewCustomerMessage(text, msgType?)            // customer msg arriving in Grok's view

// Event factories — return full lifecycle events
connectedEvent(groupId, member, memberContact?)    // connectedToGroupMember
leftEvent(groupId, member)                         // leftMember (auto-sets Left status)
updatedEvent(groupId, chatItem, userId?)           // chatItemUpdated
reactionEvent(groupId, added)                      // chatItemReaction
joinedEvent(groupId, member, userId?)              // joinedGroupMember

// History builders — add to mock chatItems map
addBotMessage(text, groupId?)
addCustomerMessageToHistory(text, groupId?)
addTeamMemberMessageToHistory(text, contactId?, groupId?)
addGrokMessageToHistory(text, groupId?)

// Assertion helpers — intention-revealing, with debuggable failure messages
expectSentToGroup(groupId, substring)     // message containing substring sent to group
expectNotSentToGroup(groupId, substring)  // no message containing substring sent to group
expectDmSent(contactId, substring)        // DM containing substring sent to contact
expectAnySent(substring)                  // any message (group or DM) containing substring
expectMemberAdded(groupId, contactId)     // apiAddMember called with groupId + contactId
expectCardDeleted(cardItemId)             // apiDeleteChatItems called with cardItemId
expectMemberContactCreated(groupId, memberId)  // apiCreateMemberContact called
expectMemberContactInvSent(contactId)          // apiSendMemberContactInvitation called
```

### 20.3 State Setup Helpers

Each helper reaches a specific state, composing from simpler helpers:

```typescript
async function reachQueue(groupId?)       // send first msg → QUEUE (adds queue msg to history)
async function reachGrok(groupId?)        // reachQueue → /grok → simulateGrokJoinSuccess → GROK
async function reachTeamPending(groupId?) // reachQueue → /team → TEAM-PENDING
async function reachTeam(groupId?)        // reachTeamPending → add team member to mock → team msg → TEAM
```

**`simulateGrokJoinSuccess(mainGroupId?)`** — simulates the async Grok join flow:
1. Waits 10ms (lets `activateGrok` reach `waitForGrokJoin`)
2. Fires `onGrokGroupInvitation` (Grok accepts invite)
3. Fires `onGrokMemberConnected` (Grok fully connected → resolver called)

Called as: `const p = simulateGrokJoinSuccess(); await bot.onNewChatItems(...); await p;`

### 20.4 Test Catalog (129 tests, 27 suites)

#### 1. Welcome & First Message (4 tests)
- first message → queue reply + card created with /join command
- non-text first message → no queue reply, no card
- second message → no duplicate queue reply
- unrecognized /command → treated as normal message (triggers queue)

#### 2. /grok Activation (5 tests)
- /grok from QUEUE → Grok invited, grokActivatedMessage sent (after join confirms)
- /grok as first message → WELCOME→GROK, no queue message, card created
- /grok in TEAM → rejected with teamLockedMessage
- /grok when grokContactId is null → grokUnavailableMessage
- /grok as first message + Grok join fails → queue message sent as fallback

#### 3. Grok Conversation (10 tests)
- Grok per-message: reads history, calls API, sends response
- customer non-text → no Grok API call
- Grok API error → grokErrorMessage sent
- Grok ignores bot commands from customer
- Grok ignores non-customer messages
- Grok ignores own messages (groupSnd)
- batch: multiple customer messages in one event → only last triggers Grok API call
- batch: messages from different groups → each group gets one response
- batch: non-customer messages filtered, only customer messages trigger response

#### 4. /team Activation (4 tests)
- /team from QUEUE → ALL team members added, teamAddedMessage sent
- /team as first message → WELCOME→TEAM-PENDING, no queue message
- /team when already activated (members present) → teamAlreadyInvitedMessage
- /team with no team members → noTeamMembersMessage

#### 5. One-Way Gate (5 tests)
- team member first TEXT → Grok removed if present
- team member empty text → Grok NOT removed
- /grok after gate → teamLockedMessage
- customer text in TEAM → no bot reply, card update scheduled
- /grok in TEAM-PENDING → invite Grok if not present

#### 6. Team Member Lifecycle (6 tests)
- team member connected → promoted to Owner
- customer connected → NOT promoted
- Grok connected → NOT promoted
- all team members leave → reverts to QUEUE
- /team after all members left (TEAM-PENDING, no msg sent) → re-adds members
- /team after all members left (TEAM, msg was sent) → re-adds members

#### 7. Card Dashboard (6 tests)
- first message creates card with customer name + /join
- /join single-quotes names with spaces
- card update deletes old, posts new
- apiDeleteChatItems failure → ignored, new card posted
- customData stores cardItemId through flush cycle
- customer leaves → customData cleared

#### 8. Card Debouncing (4 tests)
- rapid schedules → single card update on flush
- multiple groups pending → each reposted once
- card create is immediate (not debounced)
- flush with no pending → no-op

#### 9. Card Format & State Derivation (6 tests)
- QUEUE state derived (no Grok/team)
- WELCOME state derived (customData has no cardItemId)
- GROK state derived (Grok member present)
- TEAM-PENDING derived (team present, no team message)
- TEAM derived (team present + message sent)
- message count excludes bot's own

#### 10. /join Command (4 tests)
- /join groupId:name → team member added
- /join non-business group → error
- /join non-existent groupId → error
- customer /join in customer group → treated as normal message

#### 11. DM Handshake (6 tests)
- team member joins team group → DM with contact ID
- name with spaces → single-quoted
- pending DM delivered on contactConnected
- team member with no DM contact → creates member contact via `apiCreateMemberContact` and sends invitation via `apiSendMemberContactInvitation`
- joinedGroupMember in team group → creates member contact via `apiCreateMemberContact` and sends invitation via `apiSendMemberContactInvitation`
- no duplicate DM when sendTeamMemberDM succeeds AND onMemberContactReceivedInv fires

#### 12. Direct Messages (3 tests)
- regular DM → business address link reply
- DM without business address → no reply
- non-message DM event (e.g. contactConnected) → no reply (rcvMsgContent guard)

#### 13. Business Request (1 test)
- acceptingBusinessRequest → enables file uploads + visible history

#### 14. chatItemUpdated Handler (3 tests)
- business group → card update scheduled
- non-business group → ignored
- wrong user → ignored

#### 15. Reactions (2 tests)
- reaction added → card update scheduled
- reaction removed → no card update

#### 16. Customer Leave (4 tests)
- customer leaves → customData cleared
- Grok leaves → maps cleaned, no crash
- team member leaves → logged, no crash
- leftMember in non-business group → ignored

#### 17. Error Handling (3 tests)
- apiAddMember fails (Grok) → grokUnavailableMessage
- groupDuplicateMember on Grok invite → only inviting message, no result (in-flight activation handles outcome)
- groupDuplicateMember on /team → apiListMembers fallback

#### 18. Profile / Event Filtering (4 tests)
- newChatItems from Grok profile → ignored by main handler
- Grok events from main profile → ignored by Grok handlers
- own messages (groupSnd) → ignored
- non-business group messages → ignored

#### 19. Grok Join Flow (5 tests)
- receivedGroupInvitation → apiJoinGroup called (full async flow)
- unmatched Grok invitation → buffered (not joined until activateGrok drains)
- buffered invitation drained after pendingGrokJoins set → apiJoinGroup called
- per-message responses suppressed during activateGrok initial response (grokInitialResponsePending gate)
- per-message responses resume after activateGrok completes

#### 20. Grok No-History Fallback (1 test)
- Grok joins but sees no customer messages → grokNoHistoryMessage

#### 21. Non-customer card updates (2 tests)
- Grok response → card update scheduled
- team member message → card update scheduled

#### 22. End-to-End Flows (3 tests)
- WELCOME → QUEUE → /team → TEAM-PENDING → team msg → TEAM
- WELCOME → /grok first msg → GROK
- multiple concurrent conversations are independent

#### 23. Message Templates (5 tests)
- welcomeMessage includes/omits group links
- grokActivatedMessage content
- teamLockedMessage content
- queueMessage mentions hours

#### 24. isFirstCustomerMessage detection (2 tests)
- returns true when `customData` is empty
- returns false once `cardItemId` is set

#### 25. Card Preview Sender Prefixes (14 tests)
- single customer message → name prefix
- consecutive same-sender → prefix only on first
- alternating senders → each run gets prefix
- Grok messages → "Grok:" prefix
- team member messages → display name prefix
- bot messages (groupSnd) → excluded
- non-text content → media label ([image], [voice], etc.)
- empty messages → skipped
- truncation at maxTotal and maxPer limits (newest messages kept, oldest truncated)
- customer identified by memberId (not contactId)
- newlines in message text → replaced with spaces
- newlines in customer display name → sanitized in card header, raw name preserved in /join command

#### 26. Restart Card Recovery (10 tests)
- refreshAllCards refreshes groups with active cards
- no active cards → no-op
- ignores groups without cardItemId in customData
- orders by cardItemId ascending (oldest first, newest last)
- skips cards marked complete
- deletes old card before reposting
- ignores delete failure (>24h old card)
- card flush writes complete: true for auto-completed conversations
- card flush clears complete flag when conversation becomes active again
- continues on individual card failure

#### 27. joinedGroupMember Event Filtering (2 tests)
- joinedGroupMember in non-team group → ignored
- joinedGroupMember from wrong user → ignored

### 20.5 Conventions

- **File:** `bot.test.ts` (co-located with source, imports from `./src/*.js`)
- **Framework:** Vitest 1.x (Node 18 compatible) with `describe`/`test`/`beforeEach`
- **Mocking:** Vite resolve aliases (not `vi.mock`) — prevents native addon loading
- **Titles:** plain English, `→` separates action from outcome
- **Assertions:** verify observable effects only — messages, API calls, card content
- **No internal state assertions** — never peek at private fields
- **Each test is self-contained** — `beforeEach(() => setup())` creates fresh mocks
- **State helpers compose** — `reachTeam()` calls `reachTeamPending()` which calls `reachQueue()`
- **Grok join simulation** — `simulateGrokJoinSuccess()` uses 10ms setTimeout to fire events during `waitForGrokJoin` await. Tests call `await bot.flush()` after simulation to await fire-and-forget `activateGrok` completion.
- **No fake timers** — real timers everywhere; flush called explicitly via `cards.flush()` and `bot.flush()`

### 20.6 Test Coverage Notes

**Covered vs plan catalog:**
- §20.4 items 1-13, 15, 17-27 fully covered (129 tests across 27 suites)
- §20.4 item 14 (Weekend Detection) — not unit-tested; `isWeekend` depends on `Intl.DateTimeFormat(new Date())`, would need clock mocking
- §20.4 item 16 (Profile Mutex) — not unit-tested; mutex serialization is verified implicitly through all other tests (MockChatApi tracks activeUserId)
- §20.4 item 19 (Startup & State Persistence) — not unit-tested; tests `index.ts` startup which requires native ChatApi. Integration test only. This includes `deleteInviteLink` (profileMutex + `apiSetActiveUser` before `apiDeleteGroupLink`), the conditional `apiUpdateGroupProfile` (compare `fullGroupPreferences` before calling), and the best-effort `apiCreateGroupLink` (catch + log on SMP relay failure) — all are in startup code and cannot be covered by MockChatApi-based tests.

**Known plan items NOT implemented (conscious gaps, not test gaps):**
- Per-group Grok API call serialization (plan §10) — not implemented or tested
- Team member replacement on leave after sending (plan §15) — not implemented
