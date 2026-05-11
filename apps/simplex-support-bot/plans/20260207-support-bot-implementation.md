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
│  mainUserId  ← non-Grok user (default name:      │
│                "Ask SimpleX Team")               │
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
- Two user profiles in one database. The main profile is returned directly from `bot.run()`. The Grok profile's `userId` is persisted to `state.json` as `grokUserId` on the first run (when the bot creates it); subsequent runs identify Grok strictly by that persisted ID (never by display name, which a rename would invalidate). The main profile's displayName is set only on fresh-DB user creation (`"Ask SimpleX Team"`) and is never rewritten by bot code thereafter — `bot.run()` is invoked with `updateProfile: false`. Bot commands (`/grok`, `/team`) are never pushed via global `apiUpdateProfile`; instead they sync lazily per-group in `sendToGroup` — the first send to each group triggers `syncGroupCommands(groupId)`, which verifies the group's `groupPreferences.commands` against `desiredCommands` and calls `apiUpdateGroupProfile` if different (scoped broadcast to that group's members only). Subsequent sends to the same group are cache hits.
- `profileMutex` serializes `apiSetActiveUser(userId)` + the subsequent SimpleX API call. Grok HTTP API calls run **outside** the mutex.
- Events delivered for all profiles — routed by `event.user` field (main → main handler, Grok → Grok handler)
- Business address auto-accept creates a group per customer
- Grok is a second profile invited as a Member — appears as a separate participant
- No cross-profile ID mapping needed — Grok profile uses its own local group IDs from its own events

## 3. Project Structure

```
apps/simplex-support-bot/
├── package.json          # deps: simplex-chat, @simplex-chat/types, async-mutex; devDeps: vitest, @types/node
├── tsconfig.json         # ES2022, strict, Node16 module resolution
├── vitest.config.ts      # test runner config, path aliases for mocks
├── src/
│   ├── index.ts          # Entry: parse config, init instance, run
│   ├── config.ts         # CLI arg parsing, ID:name validation, Config type
│   ├── bot.ts            # SupportBot class: state derivation, event dispatch, cards
│   ├── cards.ts          # Card formatting, debouncing, lifecycle
│   ├── grok.ts           # GrokApiClient: xAI API wrapper, system prompt, history
│   ├── messages.ts       # All user-facing message templates
│   └── util.ts           # isWeekend, profileMutex, logging helpers
├── bot.test.ts           # Vitest suite (154 tests, 31 describes)
├── test/
│   └── __mocks__/
│       ├── simplex-chat.js        # MockChatApi + utility re-exports
│       └── simplex-chat-types.js  # enum re-exports for tests
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
| `--timezone` | No | `"UTC"` | IANA tz | For weekend detection (24h vs 48h). Weekend = Sat 00:00 – Sun 23:59 in this tz. `parseConfig` validates the value by constructing a probe `Intl.DateTimeFormat` and throws with a clear error on `RangeError` (invalid IANA zone) — bot exits before init. |
| `--complete-hours` | No | `3` | integer ≥ 0 | Hours of customer inactivity after last team/Grok reply before auto-completing a conversation (✅). `parseConfig` rejects non-numeric, negative, or `NaN` values with a fail-fast error. `0` is allowed and disables auto-complete. |
| `--card-flush-seconds` | No | `300` | integer ≥ 0 | Seconds between card dashboard update flushes. `parseConfig` rejects non-numeric, negative, or `NaN` values with a fail-fast error. `0` is allowed and disables periodic flush (card updates still occur on explicit `scheduleUpdate` callers but never auto-drain). |

**Env vars:** `GROK_API_KEY` (optional) — xAI API key. If unset or empty, the bot starts with Grok support fully disabled: it logs `"No GROK_API_KEY provided, disabling Grok support"`, skips Grok profile/contact setup and event handler registration, omits `/grok` from the bot command list, drops the `/grok` clause from customer-facing messages, and treats any `/grok` the customer still types as an unknown command.

**Numeric argument validation:** `parseConfig` MUST validate every numeric flag (`--complete-hours`, `--card-flush-seconds`) using a helper that throws on non-finite or negative results, rather than raw `parseInt`:

```typescript
function parseNonNegativeInt(raw: string, flag: string): number {
  const n = parseInt(raw, 10)
  if (!Number.isFinite(n) || n < 0) {
    throw new Error(`${flag} must be a non-negative integer, got "${raw}"`)
  }
  return n
}

const completeHours     = parseNonNegativeInt(optionalArg(args, "--complete-hours",     "3"),   "--complete-hours")
const cardFlushSeconds  = parseNonNegativeInt(optionalArg(args, "--card-flush-seconds", "300"), "--card-flush-seconds")
```

Rationale: `parseInt("foo", 10)` returns `NaN`, and `NaN * 3600_000 === NaN`. Every subsequent comparison (`now - lastTeamGrokTime >= completeMs`) is `false`, so the feature silently becomes a no-op — auto-complete never fires, cards never auto-refresh — and the operator has no signal that they typo'd a flag. Failing fast at startup surfaces the typo before customers interact. `0` is explicitly allowed as a valid "disable" setting.

**Timezone validation:** `parseConfig` MUST validate `--timezone` by constructing a probe `Intl.DateTimeFormat`:

```typescript
try {
  new Intl.DateTimeFormat("en-US", {timeZone: timezone, weekday: "short"})
} catch (err) {
  throw new Error(`--timezone "${timezone}" is not a valid IANA time zone: ${(err as Error).message}`)
}
```

Rationale: `isWeekend` is called from `queueMessage` and `teamAddedMessage` — both run on the hot customer message path. `new Intl.DateTimeFormat(..., {timeZone: <invalid>, ...})` throws `RangeError: Invalid time zone specified` at every call. Without startup validation, a typo in `--timezone` turns every `/grok`, `/team`, or first-customer-message dispatch into an unhandled error that crashes the per-item handler (though the outer try/catch in `onNewChatItems` contains it, customers receive no reply at all). Validating once at startup surfaces the typo in the operator's console before any customer interaction.

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

Only two keys. All other state is persisted in the group's `customData` (per-conversation state, card IDs) or derived from group metadata (`apiListMembers`). Display data like message counts is read from chat history on demand.

**Grok contact resolution** (state-file lookup always runs; contact establishment only when enabled):
1. Read `grokContactId` from state file → validate via `apiListContacts` → set `config.grokContactId` (this always runs, even when `grokApiKey === null`, so the one-way gate can identify and remove Grok members from groups)
2. If not found and `grokEnabled`: main profile creates one-time invite link, Grok profile connects, wait for a `contactConnected` event filtered by profile identity (60s — see "Grok contact identification" below), persist the resulting `contactId` atomically before proceeding.
3. If unavailable (with Grok otherwise enabled), bot runs but `/grok` returns "temporarily unavailable"
4. If `grokApiKey === null`: the Grok profile is not resolved or created, no invite link is issued — but `config.grokContactId` is still set from the state file if the contact exists.

### Grok contact identification

`grokContactId` is written once and used forever — it is the single identifier for every subsequent Grok check (one-way gate, `onMemberConnected` skip, `isGrok` in card rendering). Identification MUST be narrowly scoped so that the `contactId` stored is unambiguously Grok's and no other contact completing a handshake in the 60s establishment window can be latched by mistake.

Use the predicate form of `ChatApi.wait`. The signature (defined in `node_modules/simplex-chat/src/api.ts:217`) is:

```typescript
wait<K extends CEvt.Tag>(
  event: K,
  predicate: ((event: ChatEvent & {type: K}) => boolean) | undefined,
  timeout: number,
): Promise<ChatEvent & {type: K} | undefined>
```

The implementation (api.ts:234) keeps the subscriber attached when the predicate returns `false`, so non-matching events are silently discarded and the wait continues until a matching event arrives or the timeout fires.

Identification accepts only a `contactConnected` event observed by the MAIN profile (the profile whose `apiCreateLink` issued the invite, and whose `contactId` we persist and later pass to `apiAddMember`) whose connecting contact's profile `displayName` equals the Grok profile's displayName:

```typescript
const grokProfileName = grokUser.profile.displayName   // "Grok" (canonical)
const evt = await chat.wait(
  "contactConnected",
  (e) =>
    e.user.userId === mainUser.userId &&
    e.contact.profile.displayName === grokProfileName,
  60_000,
)
if (!evt) {
  console.error(`Timeout waiting for Grok contact (60s, displayName="${grokProfileName}"). ` +
    `Check SMP relay availability or re-run after clearing state. Exiting.`)
  process.exit(1)
}
config.grokContactId = evt.contact.contactId
state.grokContactId = config.grokContactId
writeState(stateFilePath, state)   // atomic: tmp-file + rename (see §13 state persistence)
log(`Grok contact established: ID=${config.grokContactId} (displayName="${grokProfileName}")`)
```

Filter rationale:
- `e.user.userId === mainUser.userId` selects the main profile's view of the handshake. Both profiles observe the handshake (the Grok-side event describes the main profile as the `contact`); only the main-side event carries the `contactId` we need for subsequent `apiAddMember` calls.
- `e.contact.profile.displayName === grokProfileName` accepts only the contact whose profile matches the Grok profile just created/updated. This rejects stray inbound contacts (late business-request acceptance, operator test DM, a reconnect of an existing contact) that may complete in the same 60s window. The displayName is read from `evt.contact.profile`, which is `LocalProfile` (see `@simplex-chat/types/src/types.ts:2867`).

`grokProfileName` is captured from `grokUser.profile.displayName` immediately before the wait, so whichever name the Grok profile was created/updated with earlier in startup is the exact string matched here.

Single-tenant deployment caveat: if a human contact happens to set its SimpleX displayName to the literal `"Grok"` and completes a handshake with the main profile in the 60s window, the displayName filter alone cannot distinguish them. MVP is single-tenant and Grok's profile is created by the bot itself, so this is not expected in practice; deployments that need stronger guarantees can add a second filter (e.g. `e.contact.profile.image === grokImage` — the bot knows the exact image bytes it assigned to the Grok profile).

Persistence: `writeState` is atomic (tmp-file + `fs.renameSync`, see §13 "State persistence") so a crash between identification and persistence cannot corrupt the state file. `state.grokContactId` is flushed to disk BEFORE proceeding to bot event wiring — if the process dies after wiring but before persistence, the next startup would issue a second invite link and leave the first Grok contact orphaned in the database.

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

Per-conversation state is stored in the group's `customData` and written at the moment the bot handles each transition (customer's first message, `/grok`, `/team`, team member's first message). On subsequent events `deriveState` returns the stored state as-is — composition changes (team members leaving, Grok leaving) do **not** demote the stored state. The customer's mode (e.g. "waiting for a team response") is meaningful even when no team member is currently present; keeping the state preserves that. Composition is read only by specific handlers (e.g. the `/team` duplicate-invite guard). No chat-history scans for state decisions. No in-memory conversations map — survives restarts.

**WELCOME detection:** customData has no `state` field until the bot handles the first transition. `deriveState` returns `WELCOME` precisely when `customData.state` is absent.

**Type vs. persisted state.** The `ConversationState` union in `cards.ts` enumerates all five conceptual states (`WELCOME | QUEUE | GROK | TEAM-PENDING | TEAM`) so event handlers and composition can reason about them uniformly. However, `WELCOME` is NEVER written to `customData.state` — the runtime invariant is "persisted state ∈ {QUEUE, GROK, TEAM-PENDING, TEAM}; absence of the `state` field derives as WELCOME". The `isConversationState` guard in `cards.ts` rejects `WELCOME` on read to preserve this invariant (any stale `state: "WELCOME"` from a crashed transition is treated as absent). Do NOT introduce a separate `PersistedState` type in MVP — the invariant is small enough to enforce at two choke points: `getRawCustomData` on read and the dispatch handlers on write.

**State-write matrix:**

| Bot-observed event | `customData.state` written |
|---|---|
| *(initial — no customData yet)* | *(absent ⇒ WELCOME)* |
| Customer's first non-command message | `QUEUE` |
| `/grok` handled — Grok invited | `GROK` |
| `/team` handled — team members added (written at handler time; does not wait for team acceptance) | `TEAM-PENDING` |
| First team-member text message observed | `TEAM` |

**State is authoritative and monotonic.** Once written, `customData.state` persists across member leave/join events. The only path that clears it is the existing `onLeftMember` handler when the customer themselves leaves — at that point the entire customData is cleared.

**Failure-path revert is CAS-guarded.** `activateGrok` runs fire-and-forget, so its `setStateOnFail` revert (`QUEUE`) can race with a concurrent transition (e.g. `/team` writing `TEAM-PENDING` while `waitForGrokJoin` is pending). To preserve monotonicity, `revertStateOnFail` is a compare-and-set: it only writes `setStateOnFail` if `customData.state === "GROK"` (the optimistic value both call sites write before invoking `activateGrok`). If another handler has since stamped a different state, the revert is skipped — the in-flight transition wins and stays.

TEAM-PENDING takes priority over GROK when both Grok and team are present (after `/team` but before team member's first message). `/grok` remains available in TEAM-PENDING — if Grok is not yet in the group, it gets invited; if already present, the command is ignored.

**State derivation helpers:**
- `getGroupComposition(groupId)` → `{grokMember, teamMembers}` from `apiListMembers` — used for card rendering and the `/team` duplicate-invite guard.
- `deriveState(groupId)` → reads `customData.state`. Returns `WELCOME` iff `customData.state` is absent. No composition lookup.
- `getLastCustomerMessageTime(groupId)` / `getLastTeamOrGrokMessageTime(groupId)` → chat-history timestamp reads used by the card renderer for wait-time and auto-complete only (display, not state).

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
TEAM-PENDING ──(/team)───> reply "already invited" (if team members still present; else re-add silently)
TEAM-PENDING ──(team msg)> TEAM (remove Grok, disable /grok permanently, update card)
TEAM ──(/grok)───────────> reply "team mode", stay TEAM
```

## 6. Card-Based Dashboard

The team group is a live dashboard. The bot maintains exactly one message ("card") per active customer conversation. Cards are deleted and reposted on changes — the group is always a current snapshot.

### Card format

Card is a single message. The join command is the final line of the card text — there is no separate join message.

```
[ICON] *[Customer Name]* · [wait] · [N msgs]
[STATE][· agent1, agent2, ...]
"[last message(s), truncated]"
/'join [id]'
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

**Message preview:** Last several messages, most recent last, separated by ` / `. Newlines in message text are replaced with spaces to prevent card layout bloat from spam. The customer's display name is sanitized (newlines → spaces) for the card header; the `/join` command embeds only the numeric group id. Newest messages are prioritized — when the total exceeds ~500 chars (`maxTotal = 500` in `composeCard`), the oldest messages are truncated (with `[truncated]` prepended) while the newest are always shown. When truncation occurs, the first visible message is guaranteed to have a sender prefix even if it was a continuation in the original sequence. Each message is prefixed with the sender's name (`Name: message`) on the first message in a consecutive run from that sender - subsequent messages from the same sender omit the prefix until a different sender's message appears. Sender identification: Grok contact is detected by `grokContactId` and labeled "Grok"; the customer is identified by matching `memberId` to the group's `customerId` and labeled with their display name; all other members use their `memberProfile.displayName`. Bot's own messages (`groupSnd`) are excluded. Each message truncated to ~200 chars. Media-only messages show type labels: `[image]`, `[file]`, `[voice]`, `[video]`.

**Join command:** the final line of the card renders as `/'join <groupId>'` where `<groupId>` is the customer group's numeric ID. The outer single quotes around `join <groupId>` are rendered by SimpleX clients as a clickable quoted command; tapping it sends `/join <groupId>` back to the team group. The handler does not pattern-match the message text — it uses the framework's structured command parser (`util.ciBotCommand`) which returns `{keyword: "join", params: "<groupId>"}` directly from the chat item. The handler then converts `params` to an integer via `Number.parseInt(params, 10)` and rejects anything that is not a positive integer. There is no legacy `/join <groupId>:<name>` form — the card never emits it, so the handler never needs to strip it.

### Card lifecycle

**Tracking:** `{state, cardItemId, complete?}` stored in customer group's `customData` via `apiSetGroupCustomData`. `state` is the canonical conversation state (`QUEUE | GROK | TEAM-PENDING | TEAM`); `cardItemId` is the team-group chat item ID for the (single) card message; `complete` flags the auto-completed state. Absence of `state` means WELCOME. Written at event time by the dispatch handlers — `/grok` handler writes `GROK` on invite; `/team` handler writes `TEAM-PENDING` immediately (does not wait for team acceptance); first observed team-member text message writes `TEAM`; first customer text message writes `QUEUE`. Read back from `groupInfo.customData` — single source of truth, survives restarts. All writes go through `CardManager.mergeCustomData` to preserve fields across independent write paths.

**Create** — on first customer message (→ QUEUE) or `/grok` as first message (→ GROK):
1. Compose card text (including the `/'join <groupId>'` final line)
2. Post it via `apiSendMessages(chatRef, [{msgContent: {type: "text", text}, mentions: {}}])` → get one `chatItemId`. The card is a single message; the `/'join <id>'` line is clickable because SimpleX clients render the slash-prefixed single-quoted token as a clickable command even inside a multi-line message.
3. Write `{cardItemId}` to customer group's `customData`

**Update** (delete + repost) — on every subsequent event (new customer msg, team/Grok reply, state change, agent join):
1. Read `{cardItemId}` from `customData`
2. Delete old card via `apiDeleteChatItems([Group, teamGroupId], [cardItemId], "broadcast")`. Per `simplex-chat/src/api.ts:436-445` the call either returns `T.ChatItemDeletion[]` (possibly empty if the item no longer exists) or throws `ChatCommandError`. Both outcomes are acceptable: the surrounding `try { ... } catch { /* log and continue */ }` allows execution to proceed whether the item was still present, already gone, or the server returned a transient error.
3. Post new card as a single message via `apiSendMessages` → get new `cardItemId`. **On failure** the partial-failure policy below applies: log, re-queue this groupId into `pendingUpdates`, return without writing `customData`.
4. Write `{cardItemId, complete?}` to `customData` via `mergeCustomData`. **On failure** the tracking-write policy below applies.

**Debouncing:** Card updates debounced globally — pending changes flushed every `cardFlushSeconds` seconds (default 300, configurable via `--card-flush-seconds`). Within a batch, each group's card reposted at most once with latest state.

**Wait time rules:** Time since the customer's last unanswered message. For ✅ (auto-completed) conversations, the wait field shows the literal string "done". If customer sends a follow-up, wait time resets to count from that message.

**Auto-complete:** A conversation is marked ✅ when `completeHours` (default 3h, configurable via `--complete-hours`) have passed since the last team/Grok message **without any customer reply**. The card debounce flush (every 300 seconds / 5 min, configurable via `--card-flush-seconds`) checks elapsed time and transitions to ✅ when the threshold is met. Customer follow-up at any point — including after ✅ — reverts to the derived active icon (👋/💬/⏰ for team states, 🟡/🔴 for queue), and wait time resets from that message.

**Card icon derivation (TEAM states) — computed at each card render by comparing the timestamps of the most recent customer and team/Grok messages in the group; nothing about the icon is stored:**
```
Team added, no reply yet         → 👋
Team replied                     → 💬
Customer follow-up unanswered >2h → ⏰
No customer reply for completeHours → ✅
Customer sends after ✅           → back to 💬 or ⏰ (derived from wait time)
```

**Cleanup** — customer leaves: card remains (TBD retention), clear `customData`.

**Restart recovery:** On startup, `CardManager.refreshAllCards()` lists all groups, finds those with `customData.cardItemId` set and `customData.complete` not set, sorts by `cardItemId` ascending (higher ID = more recently updated), and re-posts them oldest-first so the most recently active cards end up at the bottom of the team group. Completed cards (`complete: true`) and old/pre-bot groups (no `customData`) are skipped. Old card messages are deleted before reposting; deletion failures (e.g., >24h old) are silently ignored. Individual card failures are caught and logged without aborting the batch.

### Partial-failure and retry policy

`createCard` and `updateCard` perform a multi-step sequence (delete + send + customData write). To design the correct policy we MUST be explicit about which failures the SimpleX core already handles for us vs. which surface to the bot:

**SimpleX core semantics** (per `simplex-chat/src/api.ts` JSDoc):
- `apiSendMessages` — "Network usage: background". The call returns `newChatItems` once the chat item is CREATED LOCALLY (written to SQLite) and the SMP broadcast is QUEUED. The core's background machinery retries relay delivery transparently — **the bot never observes a transient relay failure from `apiSendMessages`**. A thrown `ChatCommandError` means the local create step itself failed: permission denied, chat does not exist, invalid content, DB locked/corrupted.
- `apiDeleteChatItems` — "Network usage: background". Same pattern: local delete + queued broadcast + core-managed delivery retry. A thrown error means the local delete step failed (item not found, permission, DB error).
- `apiSetGroupCustomData` — "Network usage: **no**". Pure local SQLite write, no SMP involvement at all. A thrown error means a local DB error.

Consequence: failures surfaced to the bot are **terminal local errors** (bad state, DB problem, permission change), not transient network blips. Retrying the same operation against the same DB/relay state will usually hit the same error. Retry value comes from the narrow slice of genuinely transient local conditions — a brief SQLite lock held by a concurrent write, a race with group-state mutation elsewhere in the same process — where the next attempt sees a different state.

This reshapes the policy: the bot does not need aggressive retry for "network" reasons (core handles that), and compensating actions for customData-write failure are rarely useful (if the pure-local customData write fails, the retry's customData write will almost certainly fail for the same reason). The bot needs a light safety net: re-queue on any step failure, let the flush loop try again at most once per `cardFlushSeconds`, and on persistent failure accept that operator intervention is needed.

Policy (applies to both `createCard` and `updateCard`):

**Any step fails** — whether step 2 (delete), step 3 (send), or step 4 (customData write):
- Log via `logError` with `{groupId, step, err}` so the operator can diagnose the underlying cause (permission change, DB corruption, bot removed from team group, etc).
- Re-add `groupId` to `pendingUpdates` via `this.scheduleUpdate(groupId)`.
- Return. Do NOT attempt compensating actions (no compensating delete for tracking-write failure — the scenario where send succeeds locally but customData write fails requires the SQLite DB to be healthy-then-unhealthy between two synchronous calls in the same transaction window, which is not a realistic transient state; the retry path handles any resulting duplicate by reading the stale `cardItemId` and deleting it on the next update attempt).

**Flush dispatch** — the current `flush` loop calls `updateCard` unconditionally and `updateCard` returns early when `customData.cardItemId` is unset. This silently drops the retry path for a failed `createCard` — the group is in `pendingUpdates` but nothing will ever create a card for it. Replace with a single `flushOne(groupId)` that reads `customData` once and dispatches to create or update:

```typescript
private async flushOne(groupId: number): Promise<void> {
  const groupInfo = await this.getGroupInfo(groupId)
  if (!groupInfo) return                            // group deleted
  const customData = this.deriveCustomData(groupInfo)
  if (customData.complete) return                   // ✅ conversations don't auto-repost
  if (typeof customData.cardItemId === "number") {
    await this.updateCard(groupId, groupInfo)
  } else {
    await this.createCard(groupId, groupInfo)
  }
}

async flush(): Promise<void> {
  const groups = [...this.pendingUpdates]
  this.pendingUpdates.clear()
  for (const groupId of groups) {
    try { await this.flushOne(groupId) }
    catch (err) {
      logError(`flush failed for group ${groupId}`, err)
      this.scheduleUpdate(groupId)                  // re-queue on any thrown error
    }
  }
}
```

Retry behavior for each failure point under this design:

| Failure point | `customData` after failure | Retry's `flushOne` path | Retry outcome if condition cleared |
|---|---|---|---|
| `createCard` send fails | `cardItemId` absent | create-path | fresh card posted, `customData` written |
| `updateCard` delete fails | old `cardItemId` still set | update-path | delete retried (idempotent — see below) + send + write |
| `updateCard` send fails (delete succeeded) | old (now-deleted) `cardItemId` still set | update-path | delete retried against stale ID — tolerated (see below) — then send + write |
| `updateCard` write fails (send succeeded, duplicate may exist) | old `cardItemId` still set, new card orphaned in team group | update-path | delete retried against stale old ID — tolerated — new card posted, tracking written; **leaked** new card from the failed attempt persists until operator removes it |

**Delete idempotency on retry** — `apiDeleteChatItems` against already-deleted IDs returns either an empty `ChatItemDeletion[]` or throws `ChatCommandError`. The step-2 `try { ... } catch { logError(...) }` swallows both; execution proceeds to step 3. Do NOT escalate a step-2 error to the partial-failure policy — that would create a retry loop for a permanent condition (items past the 24h deletion window will throw on every retry forever).

**Persistent failures** — if the underlying condition is not transient (bot removed from team group, DB corruption, permission revoked), every retry hits the same error and the group stays in `pendingUpdates` indefinitely, logging at each flush. MVP accepts this — the operator-visible log stream makes the problem diagnosable. A bounded-retry-with-backoff-and-giveup strategy can be added later without changing the failure-point table above.

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
    const {text} = await this.composeCard(groupId, groupInfo)
    // Single-message card — the `/'join <id>'` line is the final line of `text`.
    const items = await this.chat.apiSendMessages(chatRef, [
      {msgContent: {type: "text", text}, mentions: {}},
    ])
    await this.chat.apiSetGroupCustomData(groupId, {
      cardItemId: items[0].chatItem.meta.itemId,
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
    const customData = ...  // {cardItemId} from groupInfo.customData
    if (!customData?.cardItemId) return
    // Delete old card message
    try {
      await this.chat.apiDeleteChatItems(Group, teamGroupId,
        [customData.cardItemId], "broadcast")
    } catch {}  // card may already be deleted
    const {text, complete} = await this.composeCard(groupId, groupInfo)
    const items = await this.chat.apiSendMessages(chatRef, [
      {msgContent: {type: "text", text}, mentions: {}},
    ])
    const data = {
      cardItemId: items[0].chatItem.meta.itemId,
      ...(complete ? {complete: true} : {}),
    }
    await this.chat.apiSetGroupCustomData(groupId, data)
  }

  private async composeCard(groupId: number, groupInfo: T.GroupInfo): Promise<{text: string, complete: boolean}> {
    // Icon, state, agents, preview (with sender-name prefixes), /'join <id>' — per spec format
    // The final line of `text` is `/'join <groupId>'` — clickable in SimpleX clients.
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
    updateProfile: false,  // bot code never rewrites displayName/image/etc.
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

Note: `/grok` and `/team` are passed in `options.commands` so `bot.run()` has a profile to use when `apiCreateActiveUser` is needed on a fresh DB, but since `updateProfile: false` is set, `bot.run()` never writes the profile on subsequent runs. The user profile's `preferences.commands` is intentionally not pushed globally at startup — broadcasting `XInfo` to every contact is not wanted. Instead, the `SupportBot` takes `desiredCommands` as a constructor argument and syncs commands lazily per-group: `sendToGroup` (`src/bot.ts`) always calls `syncGroupCommands(groupId)` before dispatching the message. That helper reads the group via `apiGetChat(Group, groupId, 0)` (local, no network), and if `groupPreferences.commands` differs from `desiredCommands`, issues `apiUpdateGroupProfile` with the merged profile. `apiUpdateGroupProfile` broadcasts `XGrpInfo`/`XGrpPrefs` to group members only (scoped to the chat audience). Already-synced groups are cached in `syncedGroups: Set<number>` so subsequent sends skip the read entirely — the first send per group costs one local read; every later send is a cache hit. Earlier drafts used a regex on the outgoing text to skip the sync when no command keyword appeared; that optimization was removed because the cache already makes repeated syncs free and the parser was a fragile source of correctness bugs. `/join` is registered as a team group command separately — after team group is resolved, call `apiUpdateGroupProfile(teamGroupId, groupProfile)` with `groupPreferences` including the `/join` command definition. Customer sending `/join` in a customer group → treated as ordinary message (unrecognized command).

**Grok profile** — resolved from same ChatApi instance. Grok is identified strictly by the `userId` persisted in `state.json`; there is no by-name fallback (a renamed profile would otherwise be silently mistaken):

```typescript
let grokUser: T.User | null = null
if (state.grokUserId !== undefined) {
  const users = await chat.apiListUsers()
  grokUser = users.find(u => u.user.userId === state.grokUserId)?.user ?? null
  if (!grokUser) {
    throw new Error(
      `Persisted Grok userId=${state.grokUserId} not found in DB. ` +
      `Either restore the user or delete state.json to re-create Grok.`
    )
  }
} else {
  // First run: create Grok and persist its userId immediately.
  grokUser = await chat.apiCreateActiveUser({displayName: "Grok", fullName: "", image: grokImage})
  // apiCreateActiveUser sets Grok as active — switch back to main
  await chat.apiSetActiveUser(mainUser.userId)
  state.grokUserId = grokUser.userId
  writeState(stateFilePath, state)
}

// Refresh Grok's profile if it has drifted from the canonical values.
const grokProfile = {displayName: "Grok", fullName: "", image: grokImage}
const current = util.fromLocalProfile(grokUser.profile)
if (current.image !== grokProfile.image || current.displayName !== grokProfile.displayName || current.fullName !== grokProfile.fullName) {
  await chat.apiSetActiveUser(grokUser.userId)
  await chat.apiUpdateProfile(grokUser.userId, grokProfile)
  await chat.apiSetActiveUser(mainUser.userId)
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

**Per-group customData mutex** — `mergeCustomData` and `clearCustomData` must be serialized per customer group. `mergeCustomData` has two awaits (read via `getRawCustomData` → `apiListGroups`, then write via `apiSetGroupCustomData`); between them the event loop runs, so two concurrent async chains operating on the same `groupId` can both read the same snapshot, both produce a merged object, and the second write clobbers the first's patch.

Concrete call sites that can overlap on one `groupId`:
- `processMainChatItem` writing `state` transitions (WELCOME→QUEUE, WELCOME→GROK, QUEUE→GROK, one-way gate →TEAM)
- `activateGrok`'s `revertStateOnFail` (fire-and-forget) racing with subsequent customer messages
- `activateTeam` writing `TEAM-PENDING` racing with `/grok` or another `/team` on the same group
- `CardManager.flush → updateCard` writing `{cardItemId, complete}` racing with dispatch writing `state`
- `createCard` writing `{cardItemId}` immediately after dispatch writes `state`

The CAS-on-state inside `revertStateOnFail` guards only the `state` key — other keys (`cardItemId`, `complete`) can still be lost when spread from a stale snapshot.

Implementation:

```typescript
// In CardManager
private customDataMutexes = new Map<number, Mutex>()

private getCustomDataMutex(groupId: number): Mutex {
  let m = this.customDataMutexes.get(groupId)
  if (!m) { m = new Mutex(); this.customDataMutexes.set(groupId, m) }
  return m
}

async mergeCustomData(groupId: number, patch: Partial<CardData>): Promise<void> {
  return this.getCustomDataMutex(groupId).runExclusive(async () => {
    const current = (await this.getRawCustomData(groupId)) ?? {}
    const merged = {...current, ...patch}
    for (const key of Object.keys(merged) as (keyof CardData)[]) {
      if (merged[key] === undefined) delete merged[key]
    }
    await this.withMainProfile(() => this.chat.apiSetGroupCustomData(groupId, merged))
  })
}

async clearCustomData(groupId: number): Promise<void> {
  return this.getCustomDataMutex(groupId).runExclusive(() =>
    this.withMainProfile(() => this.chat.apiSetGroupCustomData(groupId))
  )
}
```

Nesting rule: the per-group customData mutex is the **outer** lock; `profileMutex` (via `withMainProfile`) is the **inner** lock. Never acquire them in the opposite order, and never hold the customData mutex while calling an external (non-SimpleX) async function — this prevents cross-group deadlock and keeps the critical section short.

Cleanup: entries in `customDataMutexes` are bounded by the number of customer groups. Removing the entry on `onLeftMember(customer)` is sufficient (the group's `customData` is also cleared at that point). Skip this refinement in MVP if acceptable — a long-running bot with many customers accumulates a few bytes per group.

**Profile images:** Both profiles have base64-encoded JPEG profile pictures (128x128, quality 85, under the 12,500-char data URI limit enforced by iOS/Android clients) set via the `image` field in `T.Profile`. The images are defined as `data:image/jpg;base64,...` string constants in `index.ts`. The main profile image is passed to `bot.run()` which handles update-on-change automatically. The Grok profile image is passed to `apiCreateActiveUser()` on first run; on subsequent runs, the bot compares the current profile against the desired one using `util.fromLocalProfile()` and calls `apiUpdateProfile()` if any field differs — this sends the update to all Grok contacts.

**Startup sequence:**
0. **Active user recovery + name preservation:** Two related safeguards.

    **(a) Active user recovery.** On restart, the active user may be Grok (if the previous run was killed mid-profile-switch). `bot.run()` uses `apiGetActiveUser()` and would then operate against Grok's `userId` as if it were the main user. Fix: when `state.grokUserId` is set (i.e. this is not the very first run), pre-init the DB with a temporary `ChatApi` and compare the active user's `userId` against `state.grokUserId`. If they match, `apiListUsers()` + `apiSetActiveUser()` to the single non-Grok user — throw loudly if zero or multiple candidates exist, rather than silently picking. Close the temporary `ChatApi` before `bot.run()` reopens it. Identification is by userId, never by display name; a renamed Grok profile would defeat name matching.

    **(b) Never rewrite the main profile.** The core auto-creates a preset contact named `"Ask SimpleX Team"` in every user's DB (`src/Simplex/Chat/Library/Internal.hs:2749`, exact name from commit `362bdc328` 2025-07-12). That collides with the bot's preferred main-profile displayName within the user's `display_names` namespace (`UNIQUE (user_id, local_display_name)`), so any attempt to rename the main profile to `"Ask SimpleX Team"` fails with `duplicateName`. Worse, `bot.run`'s internal `updateBotUserProfile` (`packages/simplex-chat-nodejs/dist/bot.js:176`) re-syncs image, preferences, and `contactLink` on every startup, and on a DB where `users.local_display_name` has drifted from `contact_profiles.display_name`, the fast path (`src/Simplex/Chat/Store/Profiles.hs:311`) silently rewrites the customer-facing `contact_profiles.display_name`. Fix: pass `options.updateProfile: false` to `bot.run()` so the bot code never calls `apiUpdateProfile` on its own initiative. Whatever displayName the CLI saw is what stays.

    **(c) Lazy per-group command sync.** The bot's command list (`/grok`, `/team`) is synced lazily and per-group, not globally. `sendToGroup` (in `src/bot.ts`) unconditionally calls `syncGroupCommands(groupId)` before dispatching the message. That helper uses `apiGetChat(Group, groupId, 0)` (local DB read, no network) to read the current `groupProfile.groupPreferences.commands`, and if it doesn't match `desiredCommands`, issues `apiUpdateGroupProfile` with the commands merged in. `apiUpdateGroupProfile` broadcasts `XGrpInfo`/`XGrpPrefs` to group members only — scoped to the chat audience, never the whole contact list. Groups confirmed in-sync are cached in `syncedGroups: Set<number>` so the first send per group costs one local read; every later send is a cache hit. No `apiUpdateProfile` (global XInfo broadcast) is ever invoked by bot code. Earlier drafts gated the sync behind a regex match on the outgoing text (to skip the read when no `/keyword` appeared); that optimization was removed because the cache already made repeated syncs free and the parser was a fragile source of correctness bugs.
1. `bot.run()` → init ChatApi, create/resolve main profile (with profile image), business address. Print business address link to stdout.
2. Resolve Grok profile: if `state.grokUserId` is set, look it up by ID via `apiListUsers()` (throw if missing); otherwise create via `apiCreateActiveUser()` and persist the new `userId`. Then compare the resolved profile against the canonical `{displayName, fullName, image}` and call `apiUpdateProfile()` if anything changed — pushes to Grok's contacts.
3. Read `{dbPrefix}_state.json` for `teamGroupId` and `grokContactId`
4. Enable auto-accept DM contacts: `apiSetAutoAcceptMemberContacts(mainUser.userId, true)`
5. List contacts, resolve Grok contact (from state or auto-establish)
6. Resolve team group (from state or auto-create)
7. Ensure direct messages + delete for everyone enabled on team group (conditional — only updates profile if preferences or name differ from desired)
8. Create team group invite link (best-effort), schedule 10min deletion if created
9. Validate `--auto-add-team-members` (`-a`) if provided
10. Register Grok event handlers on `chat` (filtered by `event.user === grokUserId`)
10b. Refresh stale cards: `CardManager.refreshAllCards()` — lists all groups, skips those with `customData.complete` or no `customData.cardItemId`, sorts remaining by `cardItemId` ascending, re-posts oldest-first so newest cards land at the bottom of team group
11. On SIGINT/SIGTERM → `clearTimeout(inviteLinkTimer)` (noop if already deleted), `cards.destroy()` (stops the card-flush interval), `deleteInviteLink()` (profileMutex-gated `apiDeleteGroupLink`), `process.exit(0)`. Signal handler is reentrant-safe: an `inviteLinkDeleted` flag prevents double-deletion; `clearTimeout`/`clearInterval` are no-op on undefined.

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
// 7. Customer message → derive state, dispatch:
//    - WELCOME: create card, send queue msg (or handle /grok first msg → WELCOME→GROK, skip queue)
//    - QUEUE: /grok → invite Grok; /team → add ALL configured team members; else schedule card update
//    - GROK: /team → add ALL configured team members (Grok stays); else schedule card update
//    - TEAM-PENDING: /grok → invite Grok if not present, else ignore; /team → if team members still present, reply "already invited"; if all team members have left, re-add silently (state stays TEAM-PENDING); else no action
//    - TEAM: /grok → reply "team mode"; else no action
```

## 9. One-Way Gate

The gate is event-driven and persists its transitions. The initial `/team` guard reads `customData.state` AND group composition: if state is already `TEAM-PENDING`/`TEAM` **and** team members are still present, the bot replies `teamAlreadyInvitedMessage` without re-adding. If state is `TEAM-PENDING`/`TEAM` but all team members have left, the bot re-adds them (state stays `TEAM-PENDING`). The first-team-message detection writes `state: 'TEAM'` into customData at the moment the bot observes the message, then removes Grok and disables `/grok`.

1. User sends `/team` → ALL configured `--auto-add-team-members` (`-a`) added to group (each promoted to Owner at invite time via `apiSetMembersRole`, re-asserted on connect as fallback) → Grok stays if present → TEAM-PENDING
2. Repeat `/team` → detected via `customData.state ∈ {TEAM-PENDING, TEAM}` **and team members still present** → reply with `teamAlreadyInvitedMessage`. If team members have since left, re-add them silently (state stays `TEAM-PENDING`).
3. `/grok` still works in TEAM-PENDING (if Grok not present, invite it; if present, ignore — Grok responds to customer messages)
4. Any team member sends first text message in customer group → **gate triggers**:
   - Remove Grok from group (`apiRemoveMembers`)
   - `/grok` permanently disabled → replies: "You are now in team mode. A team member will reply to your message."
   - State = `TEAM` (written as `customData.state = 'TEAM'` at observation time)
5. Detection: in `onNewChatItems`, when sender is a team member and `customData.state !== 'TEAM'`, trigger the gate and write `state: 'TEAM'` via `mergeCustomData`.

**Edge cases:**
- All team members leave before sending → state stays `TEAM-PENDING` (customer is still waiting for a response); sending `/team` re-adds them without the "already invited" reply.
- Team member leaves after sending → state stays `TEAM` (`customData.state` persists). Customer can send `/team` again to re-add team members.

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
- `SupportBot` constructor: `chat, grokApi: GrokApiClient | null, config, mainUserId, grokUserId: number | null, desiredCommands: T.ChatBotCommand[]` — `desiredCommands` is required (used by `sendToGroup`'s lazy per-group commands sync; see §20.4 suite 30 and the §7 "Note" describing `syncGroupCommands`).
- `queueMessage(timezone: string, grokEnabled: boolean): string`
- `noTeamMembersMessage(grokEnabled: boolean): string` (was a plain `const string`)

### Grok join flow

**Critical:** `activateGrok` awaits `waitForGrokJoin(120s)` which depends on future events dispatched through the same sequential event loop (`runEventsLoop` in api.ts). Awaiting it in an event handler deadlocks — the event loop is blocked waiting for events it can't dispatch. **Solution:** All `activateGrok` calls use `fireAndForget()` — tracked but not awaited. Tests call `bot.flush()` to await completion.

**Main profile side (invite + failure detection):**
0. Send `grokInvitingMessage` ("Inviting Grok, please wait...")
1. **Set `grokInitialResponsePending.add(groupId)` FIRST** — the gate must be raised before any operation that could make Grok recognizable to `onGrokNewChatItems`. Specifically: before `apiAddMember`, before `pendingGrokJoins` is set, and before `bufferedGrokInvitations` is drained (which populates `reverseGrokMap`). Without this ordering, the sequence `apiAddMember → pendingGrokJoins.set → drain → reverseGrokMap.set → gate.add` contains a window where `reverseGrokMap` identifies the group as a Grok-active group but the gate is still DOWN. A customer message arriving in that window triggers a per-message response concurrent with the initial combined response — producing duplicate Grok replies. Every error path below MUST clear the gate.
2. **Pre-check via `apiListMembers`**: silent return if Grok is already in the group in any non-terminal status (covers `GSMemInvited`, which the SimpleX API would otherwise resend the invitation for without throwing). Then `apiAddMember(groupId, grokContactId, Member)` → get `member.memberId`. On `groupDuplicateMember` (race between pre-check and add — Grok joined as Connected meanwhile), **clear the gate** and silent return — the in-flight activation handles the outcome. On any other error, clear the gate, revert state, send `grokUnavailableMessage`.
3. Store `pendingGrokJoins.set(memberId, mainGroupId)`
4. Drain `bufferedGrokInvitations` — if the `receivedGroupInvitation` event arrived during step 2's await (race condition), process it now. (The gate is already up from step 1, so `onGrokNewChatItems` suppresses any per-message responses during drain and the subsequent join.)
5. `waitForGrokJoin(120s)` — awaits resolver from Grok profile's `connectedToGroupMember` (step 8 below)
6. Timeout → notify customer (`grokUnavailableMessage`), send queue message if was WELCOME→GROK, fall back to QUEUE (CAS-guarded: only if `customData.state` is still `GROK` — a concurrent `/team` that switched to `TEAM-PENDING` is respected), clear `grokInitialResponsePending`

**Grok profile side (independent, triggered by its own events):**
7. `receivedGroupInvitation` → look up `pendingGrokJoins` by `evt.groupInfo.membership.memberId`. If found, auto-accept via `apiJoinGroup(groupId)`, set up `grokGroupMap` and `reverseGrokMap`. If not found (race: event arrived before step 2), buffer in `bufferedGrokInvitations` for step 3. Grok is NOT yet connected — cannot read history or send messages.
8. `connectedToGroupMember` → Grok now fully connected. Uses `reverseGrokMap` to find `mainGroupId`, resolves `grokJoinResolvers` — this unblocks step 5.

**Back in `activateGrok` (after step 5 resolves):**
9. Read visible history — last 100 messages — build Grok API context (customer messages → `user` role)
10. If no customer messages found (visible history disabled or API failed), send generic greeting asking customer to repeat their question
11. Call Grok HTTP API (outside mutex)
12. Send response via `apiSendTextMessage` (through mutex with Grok profile)
13. Clear `grokInitialResponsePending` (via `finally` block — runs on success, failure, or early return). After this, per-message responses from `onGrokNewChatItems` resume normally for subsequent customer messages. Note: because the gate is raised at step 1 (before any other work), the `finally` block MUST be wired to cover every code path from step 1 onward — including the `groupDuplicateMember` silent-return and all revert/timeout branches — otherwise per-message responses stay suppressed indefinitely for the affected group.

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
6. Call Grok HTTP API — different groups' calls run concurrently (see "Cross-group Grok parallelism" below). Per-group serialization of overlapping in-flight calls is NOT implemented in MVP (see §20.6).
7. Send response into group

**Per-message error:** Send error message in group ("Sorry, I couldn't process that. Please try again or send /team for a human team member."), stay GROK. Customer can retry.

**Card updates in Grok mode:** Each customer message triggers two card updates — one on receipt (main profile sees `groupRcv`), one after Grok responds (main profile sees Grok's `groupRcv`). Both go through the 300-second debounce (default `--card-flush-seconds`).

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

### Grok HTTP request timeout

Every `fetch` to `api.x.ai/v1/chat/completions` MUST pass an `AbortSignal.timeout(60_000)` (60-second default). Without a timeout, a stuck TCP connection or an unresponsive server blocks the awaiting call indefinitely; because `processGrokChatItem` runs under the Grok profile's sequential event dispatch, a single hung call stalls per-message responses for ALL customer groups using Grok — and the same hang in `activateGrok`'s initial-response path leaves `grokInitialResponsePending` stuck (gate never released) until the process is killed.

Implementation in `GrokApiClient.chatRaw`:

```typescript
const response = await fetch("https://api.x.ai/v1/chat/completions", {
  method: "POST",
  headers: { ... },
  body: JSON.stringify({ ... }),
  signal: AbortSignal.timeout(60_000),
})
```

On abort, `fetch` rejects with a `DOMException` whose `name === "TimeoutError"` (or `"AbortError"` on older runtimes). Callers treat this identically to other `chat()` failures:
- `processGrokChatItem` → sends `grokErrorMessage` to the customer group, conversation stays GROK.
- `activateGrok` initial-response path → logs, sends `grokUnavailableMessage`, lets the `finally` block clear `grokInitialResponsePending`.

Rationale for 60s: typical xAI responses return in 1–10s; a 60s ceiling accommodates cold-start / heavy-load latencies while still bounding worst-case per-customer wait. Not exposed as a CLI flag in MVP — a later iteration can add `--grok-timeout-seconds` if operator tuning is needed.

### Cross-group Grok parallelism

`onGrokNewChatItems` MUST dispatch per-group work concurrently. A naïve `for (const ci of lastPerGroup.values()) { await this.processGrokChatItem(ci) }` serializes calls across unrelated customer groups — if xAI takes 3s per call and five customers message in one event batch, customer #5 waits ~15s instead of ~3s. This is pure latency amplification with no ordering benefit (the groups are independent; within-group order is already preserved by batch deduplication picking the last message).

Implementation:

```typescript
async onGrokNewChatItems(evt: CEvt.NewChatItems): Promise<void> {
  const lastPerGroup = new Map<number, T.AChatItem>()
  for (const ci of evt.chatItems) {
    // filter: groupRcv, customer text, not bot/team
    // keep last per groupId
  }
  await Promise.allSettled(
    [...lastPerGroup.values()].map((ci) => this.processGrokChatItem(ci)),
  )
}
```

Why `Promise.allSettled` (not `Promise.all`): one group's Grok API failure MUST NOT cancel or reject pending work for other groups. Each `processGrokChatItem` already handles its own errors (sends `grokErrorMessage`, logs); the outer handler only needs to wait until all per-group tasks finish before returning control to the event dispatcher.

Concurrency bound: the number of distinct customer groups that have new Grok-eligible messages in a single event batch — typically ≤ the SimpleX batch-delivery size, practically small. No global semaphore needed in MVP. If xAI rate limits become a concern, add a shared semaphore later; orthogonal to this fix.

Ordering guarantees preserved:
- Within a group, batch deduplication still picks only the latest message and earlier messages appear in the history context via `apiGetChat`.
- Across groups, there is no ordering requirement — each customer group is an independent conversation.
- The per-group gate (`grokInitialResponsePending`) still serializes against `activateGrok`'s initial response; this is a group-local check unaffected by cross-group parallelism.

## 11. Team Group Commands

| Command | Effect |
|---------|--------|
| `/join <groupId>` | Join specified customer group |

**`/join` handling:**
1. Extract `{keyword, params}` from the chat item with `util.ciBotCommand(chatItem)`. The framework already parses the leading `/keyword` and returns the trimmed remainder as `params` — the handler does not run its own regex over the message text. Cards emit `/'join <groupId>'`; a team-member tap delivers a chat item whose text is `/join <groupId>`, which `ciBotCommand` returns as `{keyword: "join", params: "<groupId>"}`.
2. Convert `params` to a number with `const targetGroupId = Number.parseInt(params, 10)`. If `Number.isNaN(targetGroupId) || targetGroupId <= 0`, reply in the team group with `Error: invalid group id "${params}"` and return. No regex, no `split(":")`, no legacy fallback — operators must use the numeric form (which is what the card always emits).
3. Validate target is a business group (has `businessChat` property) — error in team group if not.
4. Add requesting team member to customer group via `addOrFindTeamMember` (which calls `apiAddMember` + immediately `apiSetMembersRole(Owner)`).
5. On connect, `connectedToGroupMember` re-asserts Owner as an idempotent fallback (see §8).

**Team member promotion:** Promotion happens at two points, both idempotent:
- **At invite time** — immediately after `apiAddMember`, `addOrFindTeamMember` calls `apiSetMembersRole(groupId, [memberId], Owner)`. The call is wrapped in try/catch: if the member is not yet connected and the API rejects, it's silently ignored (the connect-time promotion covers the fallback). SimpleX persists the role on `GSMemInvited` members so the role is active when they accept. This is only called for *newly invited* members — the pre-check in `addOrFindTeamMember` returns early for any member already in the group in a non-terminal status, so an already-invited member is not re-promoted.
- **On connect** — every `connectedToGroupMember` event in a customer group promotes to Owner unless the member is the customer or Grok. Idempotent.

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

**State file:** `{dbPrefix}_state.json` — three keys:

| Key | Type | Why persisted |
|-----|------|---------------|
| `teamGroupId` | number | Team group created once on first run |
| `grokContactId` | number | Bot↔Grok contact takes 60s to establish |
| `grokUserId` | number | Identifies the Grok user by ID across restarts; prevents silent mis-matching if the Grok profile is ever renamed |

**Not persisted:**

| State | Where it lives |
|-------|---------------|
| `state`, `cardItemId`, `complete` | Customer group's `customData` |
| `mainUserId` | Returned by `bot.run()` on startup; created fresh per DB |
| Message counts, timestamps | Derived from chat history |
| Customer name | Group display name |
| `pendingGrokJoins` | In-flight during 120s window only |
| `grokInitialResponsePending` | In-flight during `activateGrok` initial response only |
| Owner promotion | Idempotent: fired at invite time in `addOrFindTeamMember` and again on every `memberConnected` |

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
| Team member leaves (no message sent) | State stays `TEAM-PENDING` (`customData.state` persists). Customer's next `/team` re-adds silently. |
| Team member leaves (message sent) | State stays `TEAM` (`customData.state` persists). Customer's next `/team` re-adds silently. |
| No `--auto-add-team-members` (`-a`) configured | `/team` → "no team members available yet" |
| `grokContactId` unavailable | `/grok` → "temporarily unavailable" |
| Member already in group when `/team` re-runs | `addOrFindTeamMember` pre-checks via `apiListMembers` and skips BOTH `apiAddMember` and the invite-time `apiSetMembersRole(Owner)` entirely if the contact is present in any non-terminal status (so an `Invited`-but-not-yet-accepted member is never re-invited — the SimpleX API would otherwise resend the invitation for `GSMemInvited` — and is never re-promoted) |

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
| 13 | Post card to team group | main | `apiSendMessages(chatRef, [{card text with /'join <id>' final line}])` | Card create/update — one message per card |
| 14 | Delete card | main | `apiDeleteChatItems([Group, teamGId], [cardItemId], "broadcast")` | Card update |
| 15 | Set customData | main | `apiSetGroupCustomData(gId, data)` | Card lifecycle |
| 16 | Invite Grok | main | `apiAddMember(gId, grokContactId, Member)` | `/grok` |
| 17 | Grok joins | grok | `apiJoinGroup(gId)` | `receivedGroupInvitation` |
| 18 | Grok reads history | grok | `apiGetChat([Group, gId], 100)` | After join + per message |
| 19 | Grok sends response | grok | `apiSendTextMessage([Group, gId], text)` | After API call |
| 20 | Add team member | main | `apiAddMember(gId, teamContactId, Member)` | `/team`, `/join` — only when not already in group |
| 21 | Promote to Owner | main | `apiSetMembersRole(gId, [memberId], Owner)` | Immediately after #20 (invite-time) AND `connectedToGroupMember` (fallback) |
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
10. Card debouncing: multiple rapid events → verify single card update per 300s flush (default)
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
26. Repeated `/team` while members are still in `Invited` status → verify `apiAddMember` is NOT called again (pre-check in `addOrFindTeamMember` returns the existing member)
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

### 20.4 Test Catalog (154 tests, 31 suites)

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

#### 3. Grok Conversation (11 tests)
- Grok per-message: reads history, calls API, sends response
- customer non-text → no Grok API call
- Grok API error → grokErrorMessage sent
- Grok ignores bot commands from customer
- Grok ignores non-customer messages
- Grok ignores own messages (groupSnd)
- batch: multiple customer messages in one event → only last triggers Grok API call
- batch: messages from different groups → each group gets one response
- batch: non-customer messages filtered, only customer messages trigger response
- batch: across groups → Grok calls overlap in-flight (parallel `Promise.allSettled` dispatch, proven via gated `MockGrokApi.chat`)

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

#### 5b. One-Way Gate with Grok Disabled (2 tests)
- team text removes Grok even when grokApi is null
- Grok does not respond when disabled even if grokContactId is set

#### 6. Team Member Lifecycle (6 tests)
- team member connected → promoted to Owner
- customer connected → NOT promoted
- Grok connected → NOT promoted
- all team members leave → reverts to QUEUE
- /team after all members left (TEAM-PENDING, no msg sent) → re-adds members
- /team after all members left (TEAM, msg was sent) → re-adds members

#### 7. Card Dashboard (7 tests)
- first message creates card with customer name + /join
- card final line is `/'join <groupId>'` (single-quoted, numeric id only, no `:name` suffix)
- card update deletes old, posts new
- apiDeleteChatItems failure → ignored, new card posted
- customData stores cardItemId through flush cycle
- concurrent `mergeCustomData` on same group → both patches survive (per-group `customDataMutex` serializes read-modify-write; without the mutex the second write clobbers the first)
- customer leaves → customData cleared

#### 8. Card Debouncing (5 tests)
- rapid schedules → single card update on flush
- multiple groups pending → each reposted once
- card create is immediate (not debounced)
- flush with no pending → no-op
- flush on group with no `cardItemId` → `createCard` posts a new card (proves `flushOne` dispatches to create-path so a failed `createCard` retries)

#### 9. Card Format & State Derivation (6 tests)
- QUEUE state derived (no Grok/team)
- WELCOME state derived (customData has no cardItemId)
- GROK state derived (Grok member present)
- TEAM-PENDING derived (team present, no team message)
- TEAM derived (team present + message sent)
- message count excludes bot's own

#### 10. /join Command (6 tests)
- /join <groupId> (the only accepted form) → team member added; `params` from `ciBotCommand` is parsed via `Number.parseInt`, no regex
- /join <groupId>:<name> (historic suffix) → still parses because `Number.parseInt("<groupId>:<name>", 10)` stops at the colon — handler does not strip the suffix deliberately; the suffix is never emitted by the card
- /join with non-numeric `params` (e.g. `/join abc`) → error reply in team group, no `apiAddMember` call
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
- /grok while Grok already present (any non-terminal status, including `Invited`) → pre-check silent-returns, no `apiAddMember` call. Plus race coverage: simulated `groupDuplicateMember` thrown by `apiAddMember` → silent return, no further state change
- /team while team member already present (any non-terminal status, including `Invited`) → `apiAddMember` not called for that member

#### 18. Profile / Event Filtering (4 tests)
- newChatItems from Grok profile → ignored by main handler
- Grok events from main profile → ignored by Grok handlers
- own messages (groupSnd) → ignored
- non-business group messages → ignored

#### 19. Grok Join Flow (6 tests)
- receivedGroupInvitation → apiJoinGroup called (full async flow)
- unmatched Grok invitation → buffered (not joined until activateGrok drains)
- buffered invitation drained after pendingGrokJoins set → apiJoinGroup called
- per-message responses suppressed during activateGrok initial response (grokInitialResponsePending gate)
- per-message responses resume after activateGrok completes
- activateGrok `groupDuplicateMember` path → gate cleared by outer `finally` (subsequent per-message event still triggers Grok; proves the outer `try/finally` covers every exit path from the entry-time `gate.add`, not just the initial-response section)

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

#### 24. State persistence in customData (5 tests)
- `deriveState` returns `WELCOME` when `customData.state` is absent
- first customer non-command message → handler writes `customData.state = "QUEUE"`
- `/grok` handler → writes `customData.state = "GROK"`
- `/team` handler → writes `customData.state = "TEAM-PENDING"` immediately (before team member accepts)
- first team-member text message → gate writes `customData.state = "TEAM"`; state persists when team member subsequently leaves (not demoted to `QUEUE`)

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
- newlines in customer display name → sanitized in card header (card header is the only place the display name appears; `/join` is numeric id only)

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

#### 28. parseConfig Validation (6 tests)
- `--complete-hours` non-numeric → throws with message including the flag name and raw value
- `--complete-hours` negative → throws
- `--card-flush-seconds` non-numeric → throws
- `--timezone` invalid IANA → throws (probe `Intl.DateTimeFormat` at parse time)
- `--complete-hours 0` → accepted (disables auto-complete)
- valid IANA timezone → accepted

#### 29. GrokApiClient HTTP timeout (1 test)
- `chat()` calls `AbortSignal.timeout(60_000)` and passes the signal to `fetch` (spies on `AbortSignal.timeout` and on `globalThis.fetch`; proves the timeout is wired through without waiting 60s of wall-clock)

#### 30. Command sync in sendToGroup (5 tests)
Covers the lazy per-group commands sync introduced with `updateProfile: false`. `sendToGroup` unconditionally calls `syncGroupCommands(groupId)` before dispatching. That helper reads the group via `apiGetChat` (local-only) and issues `apiUpdateGroupProfile` with the merged `groupPreferences.commands` only if the current list doesn't match `desiredCommands`. Groups are cached in `syncedGroups: Set<number>` per process, so later sends skip the read entirely.
- first send → one `apiUpdateGroupProfile` call with `groupPreferences.commands = desiredCommands`; existing `groupProfile.displayName` / `fullName` preserved in the payload; message still delivered (text content is irrelevant — sync always runs)
- group already has desired commands in DB → no `apiUpdateGroupProfile` call, but `syncedGroups` is still populated (next send with different DB state still skips — cache honored)
- cache: two sends to same group → sync fires only once; both messages delivered
- different groups → each synced independently
- existing `groupPreferences` fields (e.g. `files`, `reactions`) are preserved in the update payload; only `commands` changes

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
- **No fake timers** — real timers everywhere; flush called explicitly via `cards.flush()` and `bot.flush()`. Suite 29 spies on `AbortSignal.timeout` rather than advancing a fake clock so it does not need fake timers either.

### 20.6 Test Coverage Notes

**Covered vs plan catalog:**
- §20.4 suites 1-13, 15, 17-30 plus 5b fully covered (154 tests across 31 suites)
- Weekend detection (`util.isWeekend`) — not unit-tested; depends on `Intl.DateTimeFormat(new Date())`, would need clock mocking. Not present in the §20.4 catalog.
- Profile Mutex serialization — not a standalone suite in §20.4; verified implicitly through all other tests (MockChatApi tracks activeUserId).
- Startup & state persistence (`index.ts` path) — not unit-tested; requires native ChatApi. Integration-test only. Includes `deleteInviteLink` (profileMutex + `apiSetActiveUser` before `apiDeleteGroupLink`), the conditional `apiUpdateGroupProfile` (compare `fullGroupPreferences` before calling), the best-effort `apiCreateGroupLink` (catch + log on SMP relay failure), the predicate-filtered `chat.wait("contactConnected", ...)` used to identify the Grok contact (§4), and the team-group `/join` command registration with `params: "groupId"` (§11). Not in §20.4 catalog.

**Known plan items NOT implemented (conscious gaps, not test gaps):**
- Per-group Grok API call serialization (plan §10) — not implemented or tested
- Team member replacement on leave after sending — out of MVP scope. No plan section currently asserts it as a requirement; if added later, specify in SPEC §4.2 "Team replies" and implementation plan §15 "Error Handling" together.
