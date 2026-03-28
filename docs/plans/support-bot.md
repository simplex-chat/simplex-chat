# Support Bot: Implementation Plan

## Table of Contents

- [1. Architecture](#1-architecture)
- [2. Plan](#2-plan)
  - [A. Haskell API & TypeScript Wrappers (done)](#a-haskell-api--typescript-wrappers-done)
  - [B. Switch to Typed API](#b-switch-to-typed-api)
  - [C. Persistence Overhaul](#c-persistence-overhaul)
  - [D. Abstract Agent Architecture](#d-abstract-agent-architecture)
  - [E. Robustness](#e-robustness)
  - [F. Regular Contact Support](#f-regular-contact-support)
  - [G. Notification & Formatting Cleanup](#g-notification--formatting-cleanup)
  - [H. Tests](#h-tests)
- [3. Implementation Order](#3-implementation-order)

---

## 1. Architecture

Two SimpleX Chat instances (separate databases) in one process:
- **Main bot** (`data/bot_*`): Accepts customers via business address or regular contact address, manages groups, forwards messages
- **Grok agent** (`data/grok_*`): Separate identity, joins customer groups to send AI responses

### Source Files

| File | Purpose |
|------|---------|
| `src/index.ts` | Entry point, dual ChatApi init, startup, event wiring, shutdown |
| `src/bot.ts` | `SupportBot` class: event handlers, message routing, Grok/team activation |
| `src/grok.ts` | `GrokApiClient`: xAI API wrapper, system prompt with docs context |
| `src/config.ts` | CLI arg parsing, `GROK_API_KEY` env var |
| `src/startup.ts` | `resolveDisplayNameConflict()`: direct SQLite access (to be removed) |
| `src/messages.ts` | User-facing message templates |
| `src/state.ts` | `GrokMessage` type |
| `src/util.ts` | `isWeekend()`, `log()`, `logError()` |

### State Machine

```mermaid
flowchart TD
    W([Welcome])
    TQ([TeamQueue])
    GM([GrokMode])
    TP([TeamPending])
    TL([TeamLocked])

    W -->|1st text| TQ
    W -->|/grok| GM
    W -->|/team| TP

    TQ -->|customer text| TQ
    TQ -->|/grok| GM
    TQ -->|/team| TP

    GM -->|customer text| GM
    GM -->|"/team (remove Grok)"| TP

    TP -->|customer text| TP
    TP -->|team member msg| TL

    TL -->|customer text| TL
```

State is derived from group composition + chat history — not stored explicitly. The code uses 3 internal labels (`GROK`, `TEAM`, `QUEUE`) in prefixed headers on messages forwarded to the team group; Welcome/TeamPending/TeamLocked are implicit from message history. Note: there is no transition from GrokMode back to TeamQueue — the only commands are `/grok` and `/team`, so once Grok is active, the customer either continues with Grok or moves to team. TeamLocked is terminal (once a team member engages, the group stays locked).

**Bug:** `activateTeam` (bot.ts:933) accepts `_grokMember` but ignores it — never calls `apiRemoveMembers`. Grok stays in the group until a team member sends a message, which reactively triggers removal (bot.ts:647-659). See section E for the fix.

### Business Address Flow (existing)

1. Customer connects → platform auto-creates business group with welcome message
2. Customer sends first message → bot forwards to team group (prefixed), sends queue reply, sends `/add` to team group
3. `/grok` → bot invites Grok agent, calls xAI API, Grok responds as group member
4. `/team` → bot should remove Grok, invites team member (Grok removal currently broken — see bug callout above)
5. Team member messages → "team locked"

### Regular Contact Flow (target)

1. Customer connects → `contactConnected` → bot sends welcome
2. Customer sends direct message → bot creates support group, invites team member as **owner**
3. Bot forwards customer messages into group (as `groupSnd` with prefix)
4. Team member replies in group → bot relays to customer's direct chat
5. `/grok` in direct chat → bot invites Grok into support group, relays responses to direct chat
6. `/team` in direct chat → bot removes Grok, invites team member

| Aspect | Business Address | Regular Contact |
|--------|-----------------|-----------------|
| Group creation | Platform auto-creates | Bot creates via `apiNewGroup` |
| Customer in group | Yes (member) | No (direct chat only) |
| Team sees customer | Directly in group | Bot forwards (prefixed) |
| Team replies | Customer sees in group | Bot relays to direct chat |
| Team member role | Owner | Owner |
| Grok responses | Customer sees in group | Bot relays to direct chat |
| Customer identification | `businessChat.customerId` | `customData.contactId` on group |
| State derivation | `groupRcv` from customer | `groupSnd` from bot (forwarded) |

### Grok Join Protocol

Two-phase confirmation using protocol-level `memberId` (same across both databases):

1. Main bot: `apiAddMember(groupId, grokContactId, GroupMemberRole.Member)` → returns `GroupMember`
2. Stores `pendingGrokJoins.set(member.memberId, mainGroupId)` (`memberId` is a protocol-level `string`, distinct from `groupMemberId` which is a DB-local `number`)
3. Grok receives `receivedGroupInvitation` → matches `memberId` → `apiJoinGroup` → sets bidirectional maps
4. Grok receives `connectedToGroupMember` → resolves waiter
5. Main bot gathers customer messages, calls xAI API, Grok sends response

Race condition guard: after API call, re-checks group composition — if team member appeared, removes Grok. See section E for the fix to a related race in the join protocol.

---

## 2. Plan

### A. Haskell API & TypeScript Wrappers (done)

**PR:** https://github.com/simplex-chat/simplex-chat/pull/6691

Added Haskell commands `APISetGroupCustomData`, `APISetContactCustomData` with parsers, processors, and `chatCommandsDocsData` entries. Added `APISetUserAutoAcceptMemberContacts` to docs. Fixed `APIListGroups` parser space bug. Auto-generated TypeScript `cmdString` functions via codegen test suite. Added native `ChatApi` wrappers: `apiSetGroupCustomData`, `apiSetContactCustomData`, `apiSetAutoAcceptMemberContacts`, `apiGetChat` (manual wrapper).

Raw commands to replace in the bot:

| Current | Replacement |
|---------|-------------|
| `sendChatCmd("/_get chat #${groupId} count=${count}")` (bot.ts:316, bot.ts:1222) | `apiGetChat(ChatType.Group, groupId, count)` |
| `sendChatCmd("/_groups${userId}")` (index.ts:178) | `apiListGroups(userId)` |
| `sendChatCmd("/_set accept member contacts ${userId} on")` (index.ts:136) | `apiSetAutoAcceptMemberContacts(userId, true)` |
| `sendChatCmd("/_create member contact #${groupId} ${memberId}")` (bot.ts:526) | Keep raw — low priority |
| `sendChatCmd("/_invite member contact @${contactId}")` (bot.ts:536) | Keep raw — low priority |

---

### B. Switch to Typed API

**Why:** Stage A added typed wrappers but the bot still uses raw `sendChatCmd` calls. This stage replaces them with the typed wrappers and defines the `customData` sum types needed for persistence.

#### Replace raw commands

| Current | Replacement |
|---------|-------------|
| `sendChatCmd("/_get chat #${groupId} count=${count}")` (bot.ts:316 on `mainChat`, bot.ts:1222 on `grokChat`) | `apiGetChat(ChatType.Group, groupId, count)` — both ChatApi instances need this |
| `sendChatCmd("/_groups${userId}")` (index.ts:178) | `apiListGroups(userId)` |
| `sendChatCmd("/_set accept member contacts ${userId} on")` (index.ts:136) | `apiSetAutoAcceptMemberContacts(userId, true)` |

#### Delete `state.ts`

`state.ts` only contains the `GrokMessage` type. Move it to `types.ts` as a neutral `AgentMessage` (or inline it in the agent interface from stage D).

#### Define custom data sum types

These types are specific to the support bot — they live in `apps/simplex-support-bot/src/types.ts`, not in the shared library. The library's `customData` stays as generic `object | undefined`. Discriminated unions give exhaustive `switch` matching, compile-time field access checks, and self-documenting JSON schemas.

```typescript
// SupportGroupData — stored in GroupInfo.customData

export type SupportGroupData =
  | SupportGroupData.Customer
  | SupportGroupData.CustomerContact
  | SupportGroupData.Team

export namespace SupportGroupData {
  export type Tag = "customer" | "customerContact" | "team"

  interface Interface {
    type: Tag
  }

  export interface Customer extends Interface {
    type: "customer"
    agentLocalGroupId?: number
    lastProcessedItemId?: number
  }

  export interface CustomerContact extends Interface {
    type: "customerContact"
    contactId: number
    agentLocalGroupId?: number
    lastProcessedItemId?: number
  }

  export interface Team extends Interface {
    type: "team"
  }
}

// SupportContactData — stored in Contact.customData

export type SupportContactData =
  | SupportContactData.Agent
  | SupportContactData.Customer

export namespace SupportContactData {
  export type Tag = "agent" | "customer"

  interface Interface {
    type: Tag
  }

  export interface Agent extends Interface {
    type: "agent"
  }

  export interface Customer extends Interface {
    type: "customer"
    supportGroupId: number
  }
}
```

#### JSON in DB — what each field is for

Group — **business customer** (`"customer"`):
```json
{"type": "customer", "agentLocalGroupId": 200, "lastProcessedItemId": 1234}
```
- `type: "customer"` — marks this group as a customer support group (vs team or unmanaged). Startup scans all groups for this to rebuild maps.
- `agentLocalGroupId` — the corresponding group ID in the AI agent's database. Needed to route the agent's responses back to the correct main group. Set when the agent is activated, absent otherwise.
- `lastProcessedItemId` — the last chat item the bot successfully forwarded from this group. On restart, the bot fetches items after this ID and forwards any it missed. See section C for rationale.

Group — **regular contact customer** (`"customerContact"`):
```json
{"type": "customerContact", "contactId": 42, "agentLocalGroupId": 200, "lastProcessedItemId": 1234}
```
- `type: "customerContact"` — same as `"customer"` but for regular contacts (no `businessChat` field). The bot created this group, not the platform.
- `contactId` — the direct-chat contact this group was created for. Needed for reverse forwarding: when a team member replies in this group, the bot relays the message to this contact's direct chat.
- `agentLocalGroupId` — same as above.
- `lastProcessedItemId` — same as above.

Group — **team**:
```json
{"type": "team"}
```
- `type: "team"` — identifies the single team coordination group. Startup finds this to know where to forward customer notifications. Replaces `teamGroupId` from `_state.json`.

Contact — **AI agent**:
```json
{"type": "agent"}
```
- `type: "agent"` — identifies which contact is the AI agent. Needed on startup to rebuild the agent contact reference, and to filter out the agent from customer routing (don't create support groups for infrastructure contacts). Replaces `grokContactId` from `_state.json`.

Contact — **regular customer**:
```json
{"type": "customer", "supportGroupId": 100}
```
- `type: "customer"` — marks this contact as a customer (tag is unambiguous — `SupportContactData` is only stored on contacts, `SupportGroupData` on groups).
- `supportGroupId` — the support group created for this contact. Needed to route incoming direct messages to the correct group without scanning all groups.

#### Type-safe access

```typescript
function isSupportGroupData(cd: object | undefined): cd is SupportGroupData {
  const d = cd as SupportGroupData | undefined
  if (!d?.type) return false
  switch (d.type) {
    case "customer":
    case "customerContact":
    case "team":
      return true
    default: {
      const _: never = d // compile error if a variant is added without updating this switch
      return false
    }
  }
}
```

---

### C. Persistence Overhaul

**Why:** The bot currently persists state in two places: `_state.json` (team group ID, Grok contact ID, Grok group mappings) and in-memory maps (forwarded items, join resolvers). Both are problematic:
- `_state.json` uses non-atomic `writeFileSync` — crash during write corrupts state
- If the DB is restored from backup but `_state.json` isn't (or vice versa), IDs desync and the bot silently breaks
- In-memory maps (`grokGroupMap`, `reverseGrokMap`) are lost on restart — Grok sessions in progress fail

The fix: store all persistent state in the SimpleX DB itself via `customData` on groups and contacts. The DB is already backed up, replicated, and crash-safe. `_state.json` is eliminated entirely.

#### What changes

- **Delete** `_state.json` support — the full `BotState` interface (index.ts:12-20) and all its fields:
  - `teamGroupId`, `grokContactId` → derived from `customData` on startup
  - `grokGroupMap` → derived from `customData.agentLocalGroupId` on startup
  - `newItems` (team notification tracking) → ephemeral, rebuilt from chat history or dropped
  - `groupLastActive` → ephemeral, rebuilt from chat history timestamps
  - `groupMetadata` (`firstContact`, `msgCount`, `customerName`) → derivable from chat history + group member info on startup
  - `groupPendingInfo` (`lastEventType`, `lastEventFrom`, `lastEventTimestamp`, `lastMessageFrom`) → ephemeral notification state, rebuilt from recent chat history on startup
- **Delete** `readState`/`writeState` functions and all `on*Changed` callbacks (`onGrokMapChanged`, `onNewItemsChanged`, `onGroupLastActiveChanged`, `onGroupMetadataChanged`, `onGroupPendingInfoChanged`)
- **Delete** `startup.ts` (direct SQLite access via `execSync`/`sqlite3` CLI) — `bot.run()` with `useBotProfile: true` handles display name conflicts natively
- **Tag on creation:** team group → `{type: "team"}`, agent contact → `{type: "agent"}`, customer groups → `{type: "customer"}` or `{type: "customerContact", contactId}`
- **Store** `agentLocalGroupId` in group `customData` on agent join
- **Track** `lastProcessedItemId` in group `customData` — updated after each successful forward. On restart, the bot fetches items after this ID and forwards any it missed. This avoids needing a `customData` column on `ChatItem` (which would require a DB migration on every SimpleX Chat client, not just the bot) and avoids per-item write pairs (marking pending before send, clearing after). One write per forwarded message is enough — if the bot crashes mid-forward, it just re-forwards from the last checkpoint.

#### Startup recovery (replaces _state.json)

Run steps 1-7 before wiring event handlers — all maps must be fully populated before events fire.

Note: `apiListGroups` returns `GroupInfoSummary[]` (properties nested under `.groupInfo`). `apiListContacts` returns `Contact[]` directly.

1. `apiListGroups(userId)` → find team group by `.groupInfo.customData.type === "team"`
2. `apiListContacts(userId)` → find agent contact by `.customData.type === "agent"`
3. Build `grokGroupMap` from groups where `.groupInfo.customData.agentLocalGroupId` is set
4. Build `reverseGrokMap` as inverse of `grokGroupMap`
5. Build `contactToGroupMap` from contacts where `customData.type === "customer"` (has `supportGroupId`)
6. Validate all maps — remove entries where the referenced group/contact no longer exists. Downgrade stale `customData` rather than clearing it: a `customerContact` group whose `contactId` points to a deleted contact gets downgraded to `{type: "customer"}` via `apiSetGroupCustomData(groupId, {type: "customer"})` — this preserves the group as a known support group while removing the dangling reference. A `customer` contact whose `supportGroupId` points to a deleted group gets its `customData` cleared via `apiSetContactCustomData(contactId)`. Covers entities deleted while bot was offline.
7. For each customer group, `apiGetChat(ChatType.Group, groupId, 100)`:
   - Forward any items after `lastProcessedItemId` that were missed (crash recovery)
   - Rebuild `welcomeCompleted`, `groupLastActive`, `groupMetadata`, and `groupPendingInfo`
   - Best-effort: if a group has no recent history, maps start empty (first new message repopulates)

#### Ephemeral state (RAM only)

| Map | Purpose | Recovery |
|-----|---------|----------|
| `pendingGrokJoins` | In-flight Grok invitations | User retries `/grok` |
| `grokJoinResolvers` | Promise callbacks for Grok join | Timeout after 30s |
| `grokGroupMap` | `mainGroupId → agentLocalGroupId` | Rebuilt on startup |
| `reverseGrokMap` | `agentLocalGroupId → mainGroupId` | Rebuilt on startup |
| `contactToGroupMap` | `contactId → supportGroupId` | Rebuilt on startup |
| `grokFullyConnected` | `Set<groupId>` — groups where Grok's `connectedToGroupMember` has fired | User retries `/grok` |
| `pendingTeamDMs` | `Map<contactId, string>` — pending DMs for newly-created team member contacts | Ephemeral, safe to lose |
| `pendingOwnerRole` | `Set<"groupId:groupMemberId">` — pending owner role assignments | Ephemeral, safe to lose |
| `pendingGroupCreations` | `Set<contactId>` guard against duplicates (new — section F) | Ephemeral, safe to lose (clean up contactId on success or failure) |
| `welcomeCompleted` | `Set<groupId>` — tracks groups past welcome state | Ephemeral, rebuilt from chat history on startup |
| `groupLastActive` | `Map<groupId, timestamp>` — last activity per customer group | Rebuilt from chat history timestamps on startup |
| `groupMetadata` | `Map<groupId, GroupMetadata>` — `{firstContact, msgCount, customerName}` per group | Rebuilt from chat history + group member info on startup |
| `groupPendingInfo` | `Map<groupId, GroupPendingInfo>` — `{lastEventType, lastEventFrom, lastEventTimestamp, lastMessageFrom}` | Rebuilt from recent chat history on startup |
| `forwardedItems` | `Map<"groupId:itemId", {teamItemId, header, sender}>` — maps customer items to team items for edit forwarding + reply threading | Lost on restart; edit forwarding and reply threading degrade gracefully. Stage G decides if this should be simplified. |
| `lastTeamItemByGroup` | `Map<groupId, chatItemId>` — last team group item per customer group for reply-to threading | Lost on restart; first post-restart message unthreaded, subsequent messages re-thread. Stage G decides fate. |
| `newItems` | `Map<groupId, {teamItemId, timestamp, originalText}>` — `[NEW]` marker tracking | Currently persisted via `_state.json` (entries < 24h). Stage G decides if [NEW] markers are worth the complexity. |

---

### D. Abstract Agent Architecture

**Why:** The bot code hardcodes Grok throughout — `grokGroupMap`, `reverseGrokMap`, `activateGrok`, `grokChat`, `GrokApiClient`. The sum types already use neutral names (`agent`, `agentLocalGroupId`), but the implementation doesn't match. The AI provider should be a pluggable dependency so the bot can work with any AI service.

#### Extract agent interface

```typescript
interface AIAgent {
  readonly contactId: number
  readonly chat: ChatApi
  getResponse(messages: {role: string, content: string}[]): Promise<string>
}
```

#### Rename internal maps and functions

| Current | New |
|---------|-----|
| `grokGroupMap` | `agentGroupMap` |
| `reverseGrokMap` | `reverseAgentMap` |
| `grokFullyConnected` | `agentFullyConnected` |
| `pendingGrokJoins` | `pendingAgentJoins` |
| `grokJoinResolvers` | `agentJoinResolvers` |
| `activateGrok()` | `activateAgent()` |
| `onGrokGroupInvitation()` | `onAgentGroupInvitation()` |
| `grokChat` | `agentChat` |

#### Rename config and CLI args

| Current | New |
|---------|-----|
| `--grok-db-prefix` | `--agent-db-prefix` |
| `config.grokDbPrefix` | `config.agentDbPrefix` |
| `config.grokContactId` | `config.agentContactId` |
| `config.grokApiKey` | `config.agentApiKey` |
| `GROK_API_KEY` env var | `AGENT_API_KEY` (keep `GROK_API_KEY` as fallback) |

Add `--agent-model` CLI arg (replaces hardcoded `grok-3` at grok.ts:29).

#### Move Grok-specific logic behind the interface

- `GrokApiClient` implements `AIAgent` — keeps xAI API calls, system prompt, model config
- `bot.ts` only references `AIAgent`, never `GrokApiClient` directly
- Agent provider selected via config (currently only Grok, but the interface allows others)
- `GrokMessage` type in `state.ts` → absorbed into `AIAgent` interface or `types.ts`

---

### E. Robustness

**Why:** Several existing bugs and missing error handling need fixing before adding new features. Stale map entries on restart are handled by section C's startup validation (step 6).

- **Agent API timeout**: `fetch()` has no `AbortController`. Add 30s timeout:
  ```typescript
  const controller = new AbortController()
  const timer = setTimeout(() => controller.abort(), 30000)
  try {
    const resp = await fetch(url, {...opts, signal: controller.signal})
  } finally {
    clearTimeout(timer)
  }
  ```
- **Fix `activateTeam` agent removal** (bot.ts:933): Currently ignores `_grokMember` parameter — agent stays in the group until a team member reactively triggers removal (bot.ts:647-659). Fix: call `apiRemoveMembers(groupId, [agentMember.groupMemberId])` immediately in `activateTeam`.
- **Agent join race**: If `connectedToGroupMember` fires before `onAgentGroupInvitation` sets maps, use `pendingAgentJoins` as fallback.
- **Add try/catch to `onChatItemReaction`** (bot.ts:432): Unlike `onNewChatItems`, this handler has no error wrapping — a bad cast at line 437 can throw unhandled.

---

### F. Regular Contact Support

**Why:** The bot currently only works with business address groups where the platform auto-creates a group and the customer is a group member. For regular contacts (legacy or non-business address), the customer sends direct messages — there's no group, no `businessChat` field, and all the existing guards silently drop the message. Supporting regular contacts requires a parallel message flow where the bot creates a support group itself and relays messages between the customer's direct chat and the group.

Note: The codebase already has partial support (`onContactConnected` handler, `processChatItem` routing for direct messages, `addOrFindTeamMember` using `Owner` role). The work below builds on that foundation.

#### `processDirectMessage(contact, chatItem)`

```mermaid
flowchart TD
    A[Direct message received] --> B{Own message?}
    B -->|directSnd| Z[Skip]
    B -->|directRcv| C{Known non-customer?}
    C -->|Grok or team member| Z
    C -->|Customer| D{contact.customData.supportGroupId?}
    D -->|Missing| E[createSupportGroup]
    E --> F[apiNewGroup + customData tagging]
    F --> G[apiAddMember — team as Owner]
    G --> H[Forward + queue reply + /add]
    D -->|Exists| I{Bot command?}
    I -->|/grok| J[activateGrok → relay to direct chat]
    I -->|/team| K[activateTeam]
    I -->|text| L[Forward to group + team group]
```

1. Skip `directSnd` (bot's own messages)
2. Skip non-customer contacts (Grok agent, team members) — prevents creating support groups for infrastructure contacts
3. Look up `contact.customData` as `SupportContactData` → find existing `supportGroupId`
4. If no group → `createSupportGroup(contact)`:
   - Guard first: if `contactId` already in `pendingGroupCreations`, return (another handler is creating it). Otherwise add `contactId` to `pendingGroupCreations` before any async work.
   - `apiNewGroup(userId, {displayName: "Direct chat with <name>", fullName: "", groupPreferences: {files: {enable: GroupFeatureEnabled.On}, directMessages: {enable: GroupFeatureEnabled.On}}})`
   - `apiSetGroupCustomData(groupId, {type: "customerContact", contactId})`
   - `apiSetContactCustomData(contactId, {type: "customer", supportGroupId: groupId})`
   - `apiAddMember(groupId, teamMembers[0].id, GroupMemberRole.Owner)`
   - Remove `contactId` from `pendingGroupCreations` in a `finally` block (clean up on success or failure)
5. Forward to support group (prefixed `groupSnd`) + team group
6. First message → send queue reply to direct chat, send `/add` to team group
7. `/grok` → `activateGrok(supportGroupId)` — sets `agentLocalGroupId` in group `customData` on agent join (per section C), relay response to direct chat
8. `/team` → `activateTeam(supportGroupId)`

#### Reverse forwarding (group → direct chat)

When a `groupRcv` message arrives in a group with `customData.type === "customerContact"`:
- The customer isn't in this group — they're in a direct chat with the bot
- Team member or Grok message → bot relays to customer's direct chat via `apiSendTextMessage([ChatType.Direct, contactId], text)`. The customer sees all relayed messages from the bot — this is correct, the bot is the intermediary.
- Bot's own `groupSnd` messages (forwarded customer messages) are skipped to prevent echo loops

#### Code changes

**Business-only guards to replace** (4 sites — each currently returns early for non-business groups):
- `onLeftMember` (bot.ts:355): `if (!bc) return`
- `onChatItemUpdated` (bot.ts:397): `if (!groupInfo.businessChat) return`
- `onChatItemReaction` (bot.ts:437): `if (!groupInfo?.businessChat) return`
- `processChatItem` (bot.ts:610): `if (!groupInfo.businessChat) return`

Replace each with `customData`-based routing using `isSupportGroupData` type guard. After the guard, `businessChat.customerId` is used at 5 additional sites (bot.ts:358, 445, 616, 809, 857) — replace with `customData`-based identification. The downstream helpers `getCustomerMessages` (bot.ts:296) and `getGrokHistory` (bot.ts:280) also compare `memberId === customerId` internally — these are adapted in the dual-mode section below.

**Direct message handler** (bot.ts:584-598): Currently sends "use my business address" rejection to ALL direct messages, including from Grok and team members. Replace entire block with `processDirectMessage` flow.

**Adapt `getCustomerMessages`/`getGrokHistory`** for dual mode:
  - Business: filter `groupRcv` where `ci.chatDir.groupMember.memberId === customerId` (customer is in the group, `memberId` is the protocol-level string on `GroupMember`)
  - Regular: filter `groupSnd` (bot's forwarded messages with prefix), strip prefix (customer isn't in the group — bot forwarded their messages)
  - Note: `getGrokHistory` in regular mode must handle mixed `groupRcv` (Grok's messages) + `groupSnd` (bot's forwarded customer messages) in the same loop

**Event handlers to extend:**
- `contactConnected` (bot.ts:495): Currently only handles pending team DMs. Add: if contact is not team/Grok, send welcome message (reuse `welcomeMessage` from messages.ts)
- `newChatItems` + `chatItemUpdated`: Handle direct chats (not just groups)
- `leftMember` (bot.ts:351): Add cleanup for regular contact groups — clear `customData` on the associated contact when its support group is disbanded

**Event handlers to add:**
- `groupDeleted`: Clean up maps (`agentGroupMap`, `contactToGroupMap`). For `customerContact` groups, extract `contactId` from `customData` and clear the contact's `customData` via `apiSetContactCustomData(contactId)`. For `customer` (business) groups, no contact cleanup needed — the customer is identified by `businessChat.customerId` (a group member), not a direct contact.
- `contactDeletedByContact` (CEvt, not CRResp): If the contact has `customData.type === "customer"`, remove its `contactToGroupMap` entry and delete the orphaned support group via `apiDeleteChat(ChatType.Group, supportGroupId)` (prevents both stale map entries and orphaned groups with a `contactId` pointing at a deleted contact — symmetric with `groupDeleted` which clears the contact's `customData`)

---

### G. Notification & Formatting Cleanup

**Why:** The bot has complex notification formatting logic — `[NEW]` markers (with `!1 NEW!` color prefix), state-prefixed headers (`[QUEUE]`, `[GROK]`, `[TEAM]`), `groupMetadata`, `groupPendingInfo`, and three ephemeral maps (`forwardedItems`, `lastTeamItemByGroup`, `newItems`) that exist solely for team group formatting. Some of this may not be needed after the persistence overhaul, and the rest should be simplified.

#### Ephemeral maps to evaluate

- `forwardedItems` — used for edit forwarding (updating team group when customer edits) and reply-to threading. Lost on restart; edits and threading degrade gracefully. Decide: keep as-is, simplify, or drop edit forwarding.
- `lastTeamItemByGroup` — used for auto-threading all messages from a customer group into a single team group thread. Lost on restart. Decide: keep (low cost) or drop.
- `newItems` — used for `[NEW]` markers on first customer message in team group. Currently persisted via `_state.json` with 24h expiry. Decide: keep with `customData` persistence, or drop `[NEW]` entirely.

#### Message templates (`messages.ts`)

6 templates to review for neutral agent naming and regular contact mode:
- `welcomeMessage(groupLinks)` — needs variant for regular contacts (no business address context)
- `teamQueueMessage(timezone)` — references `/grok` command by name
- `grokActivatedMessage` — rename to agent-neutral
- `teamAddedMessage(timezone)` — OK as-is
- `teamLockedMessage` — OK as-is
- `teamAlreadyAddedMessage` — OK as-is

#### Formatting

- Review `formatForwardMessage` (bot.ts:189-205) — color-coded prefixes (`!1`, `!2`, `!5`) and state markers
- Simplify `groupMetadata` — `customerName` is derivable from group members, `msgCount` may not be needed
- Simplify `groupPendingInfo` — evaluate if the `/pending` command notification logic can be streamlined

---

### H. Tests

**Why:** The bot already has 232 tests (Vitest, `bot.test.ts`, 4482 lines) with comprehensive mocking (`MockChatApi`, `MockGrokApi`) and a DSL for readable tests (`customer.sends`, `teamMember.wasInvited`, `grokAgent.joins`). The existing tests cover business address flows, state transitions, error handling, race conditions, edit forwarding, reply threading, NEW markers, and team commands. This stage extends the test suite for the new functionality — it does not start from scratch.

#### Existing coverage (no changes needed)

State transitions (welcome → queue → grok/team → locked), error handling (Grok timeout, API failure, member add failure), race conditions (team member during Grok join), edit forwarding, reply-to threading, NEW markers, /add /inviteall /invitenew /pending commands, weekend hours, message truncation, DM contact creation, business request media upload, Grok system prompt, group map persistence.

#### New tests to add

- Persistence recovery from `customData` (simulate restart, rebuild maps from `apiListGroups`/`apiListContacts`)
- `customData` tagging on creation (group and contact custom data set atomically with creation)
- `lastProcessedItemId` crash recovery (items after checkpoint re-forwarded on restart)
- Agent abstraction (AIAgent interface works with mock provider, not just Grok)
- Regular contact flow end-to-end (direct message → group creation → forward → team reply → relay back)
- `/agent` and `/team` commands in direct chat (not just group)
- `contactConnected` sends welcome message to new non-team/non-agent contacts
- Contact filtering (agent / team member direct messages → ignored, not routed as customer)
- Team member added as Owner in regular contact groups (not Member)
- Both modes coexisting (business address + regular contact customers simultaneously)
- Reverse forwarding: team/agent messages in `customerContact` groups relayed to direct chat; bot's own `groupSnd` skipped
- `groupDeleted` event cleans up maps and clears `customData` on associated contact
- `contactDeletedByContact` event cleans up `contactToGroupMap` and deletes orphaned support group
- `leftMember` event clears `customData` on associated contact for regular contact groups
- `/team` removes agent immediately (not deferred until team member sends message)
- Agent API timeout: fetch aborts after 30s, customer receives error message

---

## 3. Implementation Order

```mermaid
graph LR
    A["A. Haskell API +<br/>TS wrappers ✅"] --> B["B. Switch to<br/>typed API"]
    B --> C["C. Persistence<br/>overhaul"]
    C --> D["D. Abstract<br/>agent arch"]
    D --> E["E. Robustness"]
    E --> F["F. Regular<br/>contacts"]
    F --> G["G. Notification<br/>cleanup"]
    G --> H["H. Tests"]
```

| Order | Section | Status | Depends on | Files |
|-------|---------|--------|------------|-------|
| 1 | A. Haskell API + TS wrappers | **done** ([PR #6691](https://github.com/simplex-chat/simplex-chat/pull/6691)) | — | Controller.hs, Commands.hs, Docs/Commands.hs, api.ts |
| 2 | B. Switch to typed API | | A | bot.ts, index.ts, types.ts (new) |
| 3 | C. Persistence | | B | index.ts, bot.ts; delete startup.ts |
| 4 | D. Abstract agent architecture | | C | bot.ts, grok.ts, types.ts |
| 5 | E. Robustness | | D | bot.ts, grok.ts |
| 6 | F. Regular contacts | | E | bot.ts, index.ts |
| 7 | G. Notification cleanup | | F | bot.ts, messages.ts |
| 8 | H. Tests | | G | bot.test.ts |
