# SimpleX Support Bot — Product Specification

## Table of Contents

1. [What](#1-what)
2. [Why](#2-why)
3. [Principles](#3-principles)
4. [Flows](#4-flows)
   - [User flow](#41-user-flow)
   - [Team flow](#42-team-flow)
5. [Architecture](#5-architecture)
   - [CLI overview](#51-cli-overview)
   - [Bot architecture](#52-bot-architecture)
   - [Grok integration](#53-grok-integration)
   - [Persistent state](#54-persistent-state)

---

## 1. What

A support bot for SimpleX Chat. Customers connect via a business address and get a private group where they can ask questions. The bot triages inquiries through AI (Grok) or human team members. The team sees all active conversations as cards in a single dashboard group.

## 2. Why

- **Instant answers.** Grok handles common questions about SimpleX Chat without team involvement.
- **Organized routing.** Every customer conversation appears as a card in the team group — the team sees everything in one place without joining individual conversations.
- **No external tooling.** Everything runs inside SimpleX Chat. No ticketing system, no separate dashboard.
- **Privacy.** Customers talk to the bot in private groups. Only the team sees the messages.

---

## 3. Principles

- **Opt-in**: Grok is never used unless the user explicitly chooses it.
- **User in control**: The user can switch to Grok or team before a team member replies. Once a team member sends a message, the conversation stays with the team. The user always knows who they are talking to.
- **Minimal friction**: No upfront choices or setup — the user just sends their question.
- **Ultimate transparency**: The user always knows whether they are talking to a bot, Grok, or a human, and what happens with their messages.

---

## 4. Flows

### 4.1 User Flow

#### Step 1 — Welcome (on connect, no choices, no friction)

When a user scans the support bot's QR code or clicks its address link, SimpleX creates a **business group** — a special group type where the customer is a fixed member identified by a stable `customerId`, and the bot is the host. The bot auto-accepts the connection and enables file uploads and visible history on the group.

If a user contacts the bot via a regular direct-message address instead of the business address, the bot replies with the business address link and does not continue the conversation.

Bot sends the welcome message automatically as part of the connection handshake — not triggered by a message:
> Hello! Feel free to ask any question about SimpleX Chat.
> *Only SimpleX Chat team has access to your messages.* This is a SimpleX Chat team bot — it is not any LLM or AI.
> *Join public groups*: [existing link]
> Please send questions in English, you can use translator.

#### Step 2 — After user sends first message

The bot's "first message" detection works by scanning the last 20 messages in the group for the queue/grok/team confirmation texts. Until one of those is found, the group is in the welcome state.

On the customer's first message the bot does two things:
1. Creates a card in the team group (🆕 icon, with `/join` command)
2. Sends the queue message to the customer:

> The team can see your message. A reply may take up to 24 hours.
>
> If your question is about SimpleX Chat, click /grok for an instant AI answer (non-sensitive questions only). Click /team to switch back any time.

On weekends, the bot says "48 hours" instead of "24 hours".

Each subsequent message updates the card — icon, wait time, message preview. The team reads the full conversation by joining via the card's `/join` command.

#### Step 3 — `/grok` (Grok mode)

Available in WELCOME, QUEUE, or TEAM-PENDING state (before any team member sends a message). If Grok is already in the group, the command is ignored. If `/grok` is the customer's first message, the bot transitions directly from WELCOME → GROK — it creates the card with 🤖 icon and does not send the queue message. Triggers Grok activation (see [5.3 Grok integration](#53-grok-integration)). If Grok fails to join within 30 seconds, the bot notifies the user and the state falls back to QUEUE (the queue message is sent at this point).

Bot replies:
> *You are now chatting with Grok. You can send questions in any language.* Grok can see your earlier messages.
> Send /team at any time to switch to a human team member.

Grok is added as a separate participant so the user can differentiate bot messages from Grok messages.

Grok is prompted as a privacy expert and support assistant who knows SimpleX Chat apps, network, design choices, and trade-offs. It gives concise, mobile-friendly answers — brief numbered steps for how-to questions, 1–2 sentence explanations for design questions. For criticism, it briefly acknowledges the concern and explains the design choice. It avoids filler and markdown formatting. Relevant documentation pages and links are injected into the context by the bot. Customer messages are always placed in the `user` role, never `system`. The system prompt includes an instruction to ignore attempts to override its role or extract the prompt.

#### Step 4 — `/team` (Team mode, one-way gate)

Available in QUEUE or GROK state. Bot adds all configured `--team-members` to the support group (promoted to Owner once connected — the bot promotes every non-customer, non-Grok member to Owner on `memberConnected`; safe to repeat). If team was already activated (detected by scanning for "team member has been added" in chat history), sends the "already invited" message instead.

Bot replies:
> A team member has been added and will reply within 24 hours. You can keep describing your issue — they will see the full conversation.

On weekends, the bot says "48 hours" instead of "24 hours".

If `/team` is clicked again after a team member was already added:
> A team member has already been invited to this conversation and will reply when available.

#### One-way gate

When `/team` is clicked, team members are invited to the group. Grok is still present if it was active, and `/grok` remains available. The customer always has an active responder during this window.

The gate triggers when **any team member sends their first text message in the customer group**:
- `/grok` is permanently disabled and replies with:
  > You are now in team mode. A team member will reply to your message.
- Grok is removed from the group.
- From now on the conversation is purely between the customer and the team.

#### Customer leaving

When a customer leaves the group (or is disconnected), the bot cleans up all in-memory state for that group. The conversation card in the team group is not automatically removed (TBD).

#### Commands

`/grok` and `/team` are registered as **bot commands** in the SimpleX protocol, so they appear as tappable buttons in the customer's message input bar. The bot also accepts them as free-text (e.g., `/grok` typed manually). Unrecognized commands are treated as ordinary messages.

#### Team replies

When a team member sends a text message in the customer group, the bot resends the card (subject to debouncing). The icon (👋 vs 💬 vs ⏰) is derived from recent chat history: if the most recent message in the group is from the customer, they are waiting; if from the team, the team is waiting. Wait time reflects the most recent unanswered message.

### 4.2 Team Flow

#### Setup

The team group is created automatically on first run. Its name is set via the `--team-group` CLI argument. The group ID is written to the state file; subsequent runs reuse the same group. On every startup the bot re-applies group preferences (direct messages enabled, team commands registered as tappable buttons).

On every startup the bot generates a fresh invite link for the team group, prints it to stdout, and deletes it after 10 minutes (or on graceful shutdown). Any stale link from a previous run is deleted first.

The operator shares the link with team members. They must join within the 10-minute window. When a team member joins, the bot automatically establishes a direct-message contact with them and sends:

> Added you to be able to invite you to customer chats later, keep this contact. Your contact ID is `N:name`

This ID is needed for `--team-members` config. The DM is sent via a two-step handshake: the bot initiates a member contact, the team member accepts the DM invitation, and the message is delivered on connection.

Team members are configured as a single comma-separated `--team-members` flag (e.g., `--team-members "42:alice,55:bob"`), using the IDs from the DMs above. The bot validates every configured member against its contact list at startup and exits if any ID is missing or the display name does not match.

Until team members are configured, `/team` commands from customers cannot add anyone to a conversation. The bot logs an error and notifies the customer.

#### Dashboard — card-based live view

The team group is **not a conversation stream**. It is a live dashboard of all active support conversations. The bot maintains exactly one message (a "card") per active conversation. Whenever anything changes — a new customer message, a state transition, an agent joining — the bot **deletes the existing card and posts a new one**. The group's message list is therefore always a current snapshot: scroll up to see everything open right now.

**Trust assumption:** All team group members see all card previews, including customer message content. The team group is a trusted space — only authorized team members should be given access.

#### Card format

Each card has five parts:

```
[ICON] *[Customer Name]* · [wait] · [N msgs]
[STATE][· agent1, agent2, ...]
"[last message(s), truncated]"
/join [id]:[name]
```

**Icon / urgency signal**

| Icon | Condition |
|------|-----------|
| 🆕 | QUEUE — first message arrived < 5 min ago |
| 🟡 | QUEUE — waiting for team response < 2 h |
| 🔴 | QUEUE — waiting > 2 h with no team response |
| 🤖 | GROK — Grok is handling the conversation |
| 👋 | TEAM — team member added, no reply yet |
| 💬 | TEAM — team member has replied; conversation active |
| ⏰ | TEAM — customer sent a follow-up, team hasn't replied in > 2 h |

**Wait time** — time since the customer's last unanswered message. For conversations where the team has replied and the customer hasn't followed up, time since last message from either side.

**State label**

| Value | Meaning |
|-------|---------|
| `Queue` | No agent or Grok yet |
| `Grok` | Grok is the active responder |
| `Team – pending` | Team member added, hasn't replied yet (takes priority over `Grok` if both are present) |
| `Team` | Team member engaged |

**Agents** — comma-separated display names of all team members currently in the group. Omitted when no team member has joined.

**Message preview** — the last several messages, most recent last, separated by ` / `. In Grok mode, Grok responses are included and prefixed with `Grok:`. Each individual message is truncated to ~200 characters with `[truncated]` appended at the end of that message. Messages are included in reverse order until the total preview reaches ~1000 characters; if older messages are cut off, `[truncated]` is prepended at the beginning of the preview. Media messages show a content-type tag: `[image]`, `[file]`, etc.

**Join command** — `/join id:name` lets any team member tap to join the group instantly. Names containing spaces are single-quoted: `/join id:'First Last'`.

The icon in line 1 is the sole urgency indicator — no reactions are used.

#### Card examples

---

**1. Brand new conversation**

```
🆕 *Alice Johnson* · just now · 1 msg
Queue
"I can't connect to my contacts after updating to 6.3."
/join 42:Alice
```

---

**2. Queue — short wait, two short messages combined in preview**

```
🟡 *Emma Webb* · 20m · 2 msgs
Queue
"Hi" / "Is anyone there? I have an urgent question about my keys"
/join 88:Emma
```

---

**3. Queue — urgent, no response in over 2 hours**

```
🔴 *Maria Santos* · 3h 20m · 6 msgs
Queue
"I reset my phone and now all conversations are gone" / "I tried reinstalling but nothing changed" / "Please help, I've lost access to all my conversations after resetting my phone…"
/join 38:Maria
```

---

**4. Grok mode — Grok is handling it**

```
🤖 *David Kim* · 1h 5m · 8 msgs
Grok
"Which encryption algorithm does SimpleX use for messages?" / "Grok: SimpleX uses double ratchet with NaCl crypto_box for end-to-end encryption…[truncated]" / "And what about metadata protection?"
/join 29:David
```

---

**5. Team invited — no reply yet**

```
👋 *Sarah Miller* · 2h 10m · 5 msgs
Team – pending · evan
"Notifications completely stopped working after I updated my phone OS. I'm on Android 14…"
/join 55:Sarah
```

---

**6. Team active — two agents, name with spaces**

```
💬 *François Dupont* · 30m · 14 msgs
Team · evan, alex
"OK merci, I will try this and let you know."
/join 61:'François Dupont'
```

---

**7. Team overdue — customer follow-up unanswered > 2 h**

```
⏰ *Wang Fang* · 4h · 19 msgs
Team · alex
"The app crashes when I open large groups" / "I tried what you suggested but it still doesn't work. Any other ideas?"
/join 73:Wang
```

---

#### Card lifecycle

**Tracking: group customData.** The bot stores the current card's team group message ID (`cardItemId`) in the customer group's `customData` via `apiSetGroupCustomData(groupId, {cardItemId})`. This is the single source of truth for which team group message is the card for a given customer. It survives restarts because `customData` is in the database.

**Create** — when the customer sends their first message (triggering the Step 2 queue message) or `/grok` as their first message (WELCOME → GROK, skipping Step 2):
1. Bot composes the card (🆕 for first message, 🤖 for `/grok` as first message; customer name, message preview, `/join` command)
2. Bot posts it to the team group via `apiSendTextMessage` → receives back the `chatItemId`
3. Bot writes `{cardItemId: chatItemId}` into the customer group's `customData`

**Update** (delete + repost) — on every subsequent event: new customer message, team member reply in the customer group, state change (QUEUE → GROK, GROK → TEAM, GROK → QUEUE on join timeout, etc.), agent joining. Card updates are debounced per customer group — at most one update per 5 seconds. Rapid messages are batched into a single card repost.
1. Bot reads `cardItemId` from the customer group's `customData`
2. Bot deletes the old card in the team group via `apiDeleteChatItem(teamGroupId, cardItemId, "broadcast")` (delete for everyone)
3. Bot composes the new card (updated icon, wait time, message count, preview)
4. Bot posts new card to the team group → receives new `chatItemId`
5. Bot overwrites `customData` with the new `{cardItemId: newChatItemId}`

If `apiDeleteChatItem` fails (e.g., card was already deleted due to a prior crash), the bot ignores the error and proceeds to post the new card. The new `cardItemId` overwrites `customData`, recovering the lifecycle.

Because the old card is deleted and the new one is posted at the bottom, the most recently updated conversations always appear last in the team group.

**Cleanup** — when the customer leaves the group:
1. Bot reads `cardItemId` from `customData`
2. Card is **not deleted** — it remains in the team group until a retention policy is added (resolved state TBD)
3. Bot clears the `cardItemId` from `customData`

**Restart recovery** — on startup, the bot does not need to rebuild any card tracking. Each customer group's `customData` already contains the `cardItemId` pointing to the correct team group message. The next event for that group reads `customData` and resumes the delete-repost cycle normally.

#### Team commands

Team members use these commands in the team group:

| Command | Effect |
|---------|--------|
| `/join <groupId>:<name>` | Join the specified customer group (promoted to Owner once connected) |

#### Joining a customer group

When a team member taps `/join`, the bot first verifies that the target `groupId` is a business group hosted by the main profile (i.e., has a `businessChat` property). If not, the bot replies with an error in the team group and does nothing. If valid, the bot adds the team member to the customer group (promoted to Owner once connected). From within the customer group, the team member chats directly with the customer. Their messages trigger card updates in the team group (icon change, wait time reset). The customer sees the team member as a real group participant.

#### Edge cases

| Situation | What happens |
|-----------|-------------|
| All team members leave before any sends a message | State reverts to QUEUE (stateless derivation — no team member present) |
| Customer leaves | All in-memory state cleaned up; card remains (TBD) |
| No `--team-members` configured | `/team` tells customer "no team members available yet" |
| Team member already in customer group | `apiListMembers` lookup finds existing member — no error |

---

## 5. Architecture

### 5.1 CLI Overview

```
GROK_API_KEY=... node dist/index.js --team-group "Support Team" [options]
```

**Environment variables:**

| Var | Required | Purpose |
|-----|----------|---------|
| `GROK_API_KEY` | Yes | xAI API key for Grok |

**CLI flags:**

| Flag | Required | Default | Format | Purpose |
|------|----------|---------|--------|---------|
| `--db-prefix` | No | `./data/simplex` | path | Database file prefix (both profiles share it) |
| `--team-group` | Yes | — | `name` | Team group display name (auto-created if absent, resolved by persisted ID on restarts) |
| `--team-members` | No | `""` | `ID:name,...` | Comma-separated team member contacts. Validated at startup — exits on mismatch. Without this, `/team` tells customers no members available. |
| `--group-links` | No | `""` | string | Public group link(s) for welcome message |
| `--timezone` | No | `"UTC"` | IANA tz | For weekend detection (24h vs 48h). Weekend is Saturday 00:00 through Sunday 23:59 in this timezone. |

**Why `--team-members` uses `ID:name`:** Contact IDs are local to the bot's database — not discoverable externally. The bot DMs each team member their ID when they join the team group. The name is validated at startup to catch stale IDs pointing to the wrong contact.

**Customer commands** (registered in customer groups via `bot.run`):

| Command | Available | Effect |
|---------|-----------|--------|
| `/grok` | Before any team member sends a message | Enter Grok mode |
| `/team` | QUEUE or GROK state | Add team members, permanently enter Team mode once any replies |

**Unrecognized commands** are treated as normal messages in the current mode.

**Team commands** (registered in team group via `groupPreferences`):

| Command | Effect |
|---------|--------|
| `/join <groupId>:<name>` | Join the specified customer group (promoted to Owner once connected) |

### 5.2 Bot Architecture

The bot process runs a single `ChatApi` instance with **two user profiles**:

- **Main profile** — the support bot's account ("Ask SimpleX Team"). Owns the business address, hosts all business groups, communicates with customers, communicates with the team group, and controls group membership. On startup the bot checks the main profile for an existing business address via `apiGetUserAddress`; if none exists (first run), it creates one via `apiCreateBusinessAddress`. The address is stored in the SimpleX database as part of the profile — it survives restarts and state file loss without re-creation. The business address link is printed to stdout on every startup.
- **Grok profile** — the Grok agent's account ("Grok AI"). Is invited into customer groups as a Member. Sends Grok's responses so they appear to come from the Grok AI identity.

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
│  grokUserId  ← "Grok AI" profile                 │
│    • Joins customer groups as Member             │
│    • Sends Grok responses into groups            │
│                                                  │
│  profileMutex: serialize apiSetActiveUser + call │
│  GrokApiClient → api.x.ai/v1/chat/completions   │
└─────────────────────────────────────────────────┘
```

Before each SimpleX API call, the bot switches to the appropriate profile via `apiSetActiveUser(userId)`. All profile-switching and SimpleX API calls are serialized through a mutex to prevent interleaving. The Grok HTTP API call (external network request to xAI) is made **outside** the mutex — only the profile switch + SimpleX read/send calls need serialization. This prevents a slow Grok response from blocking all other bot operations.

**Event delivery is profile-independent.** ChatApi delivers events for all user profiles in the database, not just the active one. Every event includes a `user` field identifying which profile it belongs to. `apiSetActiveUser` only affects the context for write/send API calls — it does not filter event subscription. The bot routes events by checking `event.user`: main profile events go to the main handler, Grok profile events go to the Grok handler.

The Grok profile is self-contained: it watches its own events (`newChatItems`, `receivedGroupInvitation`), calls the Grok HTTP API, and sends responses — all using group IDs from its own events. The main profile only controls Grok's group membership (invite/remove) and reflects Grok's responses in the team group card.

### 5.3 Grok Integration

Grok is not a service call hidden behind the bot's account. It is a **second user profile** within the same SimpleX Chat process and database. The customer sees messages from "Grok AI" as a real group participant — not from the support bot. This is what makes Grok transparent to the user.

The Grok profile is **self-contained**: it watches its own events, reads group history through its own view, calls the Grok HTTP API, and sends responses — all using its own local group IDs from its own events. No cross-profile ID mapping is needed.

#### Startup: establishing the bot↔Grok contact

On first run (no state file), the bot must establish a SimpleX contact between the main and Grok profiles:

1. Main profile creates a one-time invite link
2. Grok profile connects to it
3. The bot waits up to 60 seconds for `contactConnected` to fire
4. The resulting `grokContactId` is written to the state file

On subsequent runs, the bot looks up `grokContactId` from the state file and verifies it still exists in the main profile's contact list. If not (e.g., database was wiped), the contact is re-established.

#### Per-conversation: how Grok joins a group

When a customer sends `/grok`:

**Main profile side (failure detection):**
1. Main profile: `apiAddMember(groupId, grokContactId, Member)` — invites the Grok contact to the customer's business group
2. The `member.memberId` is stored in an in-memory map `pendingGrokJoins: memberId → mainGroupId`
3. Main profile receives `connectedToGroupMember` for any member connecting in the group. The bot checks the event's `memberId` against `pendingGrokJoins` — only a match resolves the 30-second promise. This promise is only for failure detection — if it times out, the bot notifies the customer and falls back to QUEUE.

**Grok profile side (independent, triggered by its own events):**
4. Grok profile receives a `receivedGroupInvitation` event and auto-accepts via `apiJoinGroup(groupId)` (using the group ID from its own event)
5. Grok profile reads visible history from the group — the last 100 messages — to build the initial Grok API context (customer messages → `user` role)
6. Grok profile calls the Grok HTTP API with this context
7. Grok profile sends the response into the group via `apiSendTextMessage([Group, groupId], response)` — visible to the customer as a message from "Grok AI"

**Card update:** Main profile sees Grok's response as `groupRcv` and updates the team group card (same mechanism as ongoing Grok messages).

**Visible history** must be enabled on customer groups (the bot enables it alongside file uploads in the business request handler). This allows Grok to read the full conversation history after joining, rather than only seeing messages sent after it joined. If Grok reads history and finds no customer messages (e.g., visible history was disabled or the API call failed), it sends a generic greeting asking the customer to repeat their question.

#### Per-message: ongoing Grok conversation

After the initial response, the Grok profile watches its own `newChatItems` events. It only triggers a Grok API call for `groupRcv` messages from the customer — identified via `businessChat.customerId` on the group's `groupInfo` (accessible to all members). Messages from the bot (main profile), from Grok itself (`groupSnd`), and from team members are ignored. Non-text messages (images, files, voice) do not trigger Grok API calls but still trigger a card update in the team group. Every subsequent customer text message in a group where Grok is a member:
1. Triggers a card update in the team group (via the main profile, which sees the customer message as `groupRcv`)
2. Grok profile receives the message via its own event, rebuilds history by reading the last 100 messages from its own view of the group (Grok's messages → `assistant` role, customer's messages → `user` role)
3. Grok profile calls the Grok HTTP API and sends the response into the group using the group ID from its own event
4. Main profile sees Grok's response as `groupRcv` and updates the team group card

In Grok mode, each customer message triggers two card updates — one on receipt (reflecting the new message and updated wait time) and one after Grok responds. This gives the team real-time visibility into active Grok conversations.

If the Grok HTTP API call fails or times out for a per-message request, the Grok profile sends an error message into the group: "Sorry, I couldn't process that. Please try again or send /team for a human team member." Grok remains in the group and the state stays GROK — the customer can retry by sending another message.

Grok API calls are serialized per customer group — if a new customer message arrives while a Grok API call is in flight, it is queued and processed after the current call completes. This ensures Grok's history includes its own prior response before handling the next message.

#### Grok removal

Grok is removed from the group (via main profile `apiRemoveMembers`) in three cases:
1. Team member sends their first text message in the customer group
2. Grok join fails (30-second timeout) — graceful fallback to QUEUE, bot notifies the customer
3. Customer leaves the group

### 5.4 Persistent State

The bot writes a single JSON file (`{dbPrefix}_state.json`) that survives restarts. It uses the same `--db-prefix` as the SimpleX database files, so the state file is always co-located with the database (e.g. `./data/simplex_state.json` alongside `./data/simplex_chat.db` and `./data/simplex_agent.db`). This ensures backups and migrations that copy the database directory also capture the bot state.

#### Why a state file at all?

SimpleX Chat's own database stores the full message history and group membership, but it does not store the bot's derived knowledge — things like which team group was created on first run, or which contact is the established bot↔Grok link. All other derived state (message counts, timestamps, last sender) is re-derived from chat history or group metadata on demand.

#### What is persisted and why

| Key | Type | Why persisted | What breaks without it |
|-----|------|---------------|------------------------|
| `teamGroupId` | number | The bot creates the team group on first run; subsequent runs must find the same group | Bot creates a new empty team group on every restart; all team members lose their dashboard |
| `grokContactId` | number | Establishing a bot↔Grok contact takes up to 60 seconds and is a one-time setup | Every restart requires a 60-second re-connection; if it fails the bot exits |

User profile IDs (`mainUserId`, `grokUserId`) are **not** persisted — they are resolved at startup by calling `apiListUsers()` and matching by display name (the bot creates both profiles with known names).

#### What is NOT persisted and why

| State | Where it lives instead |
|-------|----------------------|
| `cardItemId` (per group) | Stored in the group's customData — the team group message ID for this customer's card |
| Last customer message time | Derived from most recent customer message in chat history |
| Message count | Derived from message count in chat history (all messages except the bot's own) |
| Customer name | Always available from the group's display name |
| Who sent last message | Derived from recent chat history |
| `welcomeCompleted` | Rebuilt on demand: `isFirstCustomerMessage` scans recent history |
| `pendingGrokJoins` | In-flight during the 30-second join window only |
| Owner role promotion | Not tracked — on every `memberConnected` in a customer group, the bot promotes the member to Owner unless it's the customer or Grok. Idempotent, survives restarts. |
| `pendingTeamDMs` | Messages queued to greet team members — simply not sent if lost |
| `grokJoinResolvers`, `grokFullyConnected` | Pure async synchronization primitives — always empty at startup |

#### Failure modes

If the state file is deleted or corrupted:
- A new team group is created. Team members must re-join it.
- The bot↔Grok contact is re-established (60-second startup delay).
- Grok remains in any groups it was already a member of. Since the Grok profile watches its own events, it will continue responding to customer messages in those groups without any additional recovery — no cross-profile state needs to be rebuilt.
