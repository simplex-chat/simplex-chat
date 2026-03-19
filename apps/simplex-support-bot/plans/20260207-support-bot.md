# SimpleX Support Bot — Product Specification

## Principles

- **Opt-in**: Grok is never used unless the user explicitly chooses it.
- **User in control**: The user can switch between Grok and team at any time, and always knows who they are talking to. Once a team member engages, the conversation stays with the team.
- **Minimal friction**: No upfront choices or setup — the user just sends their question.
- **Ultimate transparency**: The user always knows whether they are talking to a bot, Grok, or a human, and what happens with their messages.

## Step 1 — Welcome (on connect, no choices, no friction)

Bot sends:
> Hello! Feel free to ask any question about SimpleX Chat.
> *Only SimpleX Chat team has access to your messages.* This is a SimpleX Chat team bot — it is not any LLM or AI.
> *Join public groups*: [existing link]
> Please send questions in English, you can use translator.

No mention of Grok, no choices. User simply types their question. Messages are forwarded to the team — never to any third party.

## Step 2 — After user sends first message

Bot replies:
> Your message is forwarded to the team. A reply may take up to 24 hours.
>
> If your question is about SimpleX Chat, click /grok for an instant AI answer (non-sensitive questions only). Click /team to switch back any time.

On weekends, the bot says "48 hours" instead of "24 hours".

The bot creates a card for this conversation in the team group dashboard.

## Step 3 — `/grok` (Grok mode)

Bot replies:
> *You are now chatting with Grok. You can send questions in any language.* Your message(s) have been forwarded.
> Send /team at any time to switch to a human team member.

Grok is added as a separate participant so the user can differentiate bot messages from Grok messages.

Grok is prompted as a privacy expert and support assistant who knows SimpleX Chat apps, network, design choices, and trade-offs. It gives concise, mobile-friendly answers — brief numbered steps for how-to questions, 1–2 sentence explanations for design questions. For criticism, it briefly acknowledges the concern and explains the design choice. It avoids filler and markdown formatting. Relevant documentation pages and links are injected into the context by the bot.

## Step 4 — `/team` (Team mode, one-way gate)

Bot adds the first configured team member to the support group as Owner and replies:
> A team member has been added and will reply within 24 hours. You can keep describing your issue — they will see the full conversation.

On weekends, the bot says "48 hours" instead of "24 hours".

If `/team` is clicked again after a team member was already added:
> A team member has already been invited to this conversation and will reply when available.

**One-way gate:** once a team member sends their first text message in the customer group, Grok is removed. From the moment a team member joins the group, `/grok` is permanently disabled and replies with:
> You are now in team mode. A team member will reply to your message.

## Team group — live dashboard

The team group is **not a conversation stream**. It is a live dashboard of all active support conversations. The bot maintains exactly one message (a "card") per active conversation. Whenever anything changes — a new customer message, a state transition, an agent joining — the bot **deletes the existing card and posts a new one**. The group's message list is therefore always a current snapshot: scroll up to see everything open right now.

### Card format

Each card has five parts:

```
[ICON] *[Customer Name]* · [wait] · [N msgs]
[STATE][· agent1, agent2, ...]
"[last customer message(s), truncated]"
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
| `Team – pending` | Team member added, hasn't replied yet |
| `Team` | Team member engaged |

**Agents** — comma-separated display names of all team members currently in the group. Omitted when no team member has joined.

**Message preview** — last customer message, truncated to ~180 characters. If the last customer message is short (< 60 chars), the previous customer message is prepended, separated by ` / `. Media messages show a content-type tag: `[image]`, `[file]`, etc.

**Join command** — `/join id:name` lets any team member tap to join the group instantly. Names containing spaces are single-quoted: `/join id:'First Last'`.

The icon in line 1 is the sole urgency indicator — no reactions are used.

### Card examples

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
"Please help, I've lost access to all my conversations after resetting my phone…"
/join 38:Maria
```

---

**4. Grok mode — Grok is handling it**

```
🤖 *David Kim* · 1h 5m · 8 msgs
Grok
"Which encryption algorithm does SimpleX use for messages?"
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
"I tried what you suggested but it still doesn't work. Any other ideas?"
/join 73:Wang
```

---

### Notes on card lifecycle

- The card is **first created** when the customer sends their first message (same event that triggers the Step 2 queue message to the customer). At this point the card has the 🆕 icon.
- The card is **deleted and reposted** on every subsequent event: new customer message, team member reply in the customer group, state change (QUEUE → GROK, GROK → TEAM, etc.), agent joining.
- The card is **not deleted** when a conversation is resolved — it remains in the group until the bot restarts or a retention policy is added. (Resolved state TBD.)
- Cards appear at the **bottom of the group** in posting order, so the most recently updated card is always last.

## User flow (detailed)

This section describes every event that can occur in a customer conversation, in order, from the bot's perspective.

### Connection

When a user scans the support bot's QR code or clicks its address link, SimpleX creates a **business group** — a special group type where the customer is a fixed member identified by a stable `customerId`, and the bot is the host. The bot auto-accepts the connection and enables file uploads on the group. The welcome message (Step 1) is sent automatically as part of the connection handshake — it is not triggered by a message.

If a user contacts the bot via a regular direct-message address instead of the business address, the bot replies with the business address link and does not continue the conversation.

### First message

The bot's "first message" detection works by scanning the last 20 messages in the group for the queue/grok/team confirmation texts. Until one of those is found, the group is in the welcome state.

On the customer's first message the bot does two things:
1. Creates a card in the team group (🆕 icon)
2. Sends `teamQueueMessage` to the customer (the 24/48h notice + Grok/team options)

Each subsequent message updates the card — icon, wait time, message preview. The team reads the full conversation by joining via the card's `/join` command.

### Commands

`/grok` and `/team` are registered as **bot commands** in the SimpleX protocol, so they appear as tappable buttons in the customer's message input bar. The bot also accepts them as free-text (e.g., `/grok` typed manually). Unrecognised commands are treated as ordinary messages.

**`/grok`** — available in QUEUE state (both before and after the first message). Triggers Grok activation (see Grok agent flow). If Grok fails to join within 30 seconds, the bot notifies the user and the state remains QUEUE.

**`/team`** — available in QUEUE or GROK state. Adds the first configured team member to the group as Owner. If team was already activated (detected by scanning for "team member has been added" in chat history), sends the "already invited" message instead.

### Team replies

When a team member sends a text message in the customer group, the bot records `lastEventFrom = "team"` in `groupPendingInfo` and immediately resends the card. The icon changes from 👋 to 💬, signalling to other team members that this conversation is being handled.

When the customer subsequently replies, `lastEventFrom` becomes `"customer"` and `groupLastActive` is updated. The card is resent again: if the customer has been waiting less than 2 hours the icon is 👋; if more than 2 hours it escalates to ⏰.

This means `groupPendingInfo` is the sole source of truth for whether the team has replied since the customer's last message. It is persisted so the distinction survives a bot restart (see Persistent state).

### One-way gate

The gate has two distinct moments:
1. **`/team` is clicked**: the team member is invited to the group. Grok is still present if it was active. Once the team member joins (reaches Active state), `/grok` is permanently disabled.
2. **Team member sends their first text message in the customer group**: Grok is removed from the group at this point. From now on the conversation is purely between the customer and the team.

### Customer leaving

When a customer leaves the group (or is disconnected), the bot cleans up all in-memory and persisted state for that group: Grok maps, message counters, pending info, last-active timestamp. The conversation card in the team group is not automatically removed (TBD).

---

## Grok agent architecture

Grok is not a service call hidden behind the bot's account. It is a **second SimpleX Chat process** with its own user profile, its own SQLite database, and its own network identity. The customer sees messages from "Grok AI" as a real group participant — not from the support bot. This is what makes Grok transparent to the user.

### Two processes, one bot

The bot process runs two `ChatApi` instances side by side:

- **`mainChat`** — the support bot's account ("Ask SimpleX Team"). Hosts all business groups, communicates with customers, communicates with the team group, and controls group membership.
- **`grokChat`** — the Grok agent's account ("Grok AI"). Is invited into customer groups as a Member. Sends Grok's responses so they appear to come from the Grok AI identity.

Both instances live in the same process and share memory through the `SupportBot` class. Only `mainChat` does the Grok API calls; `grokChat` only sends the resulting responses into the group.

### Startup: establishing the bot↔Grok contact

On first run (no state file), the bot must establish a SimpleX contact between `mainChat` and `grokChat`:

1. `mainChat` creates a one-time invite link
2. `grokChat` connects to it as a regular contact
3. The bot waits up to 60 seconds for `contactConnected` to fire
4. The resulting `grokContactId` is written to the state file

On subsequent runs, the bot looks up `grokContactId` from the state file and verifies it still exists in `mainChat`'s contact list. If not (e.g., database was wiped), the contact is re-established.

### Per-conversation: how Grok joins a group

When a customer sends `/grok`:

1. `mainChat.apiAddMember(groupId, grokContactId, Member)` — the main bot invites the Grok contact to the customer's business group
2. The `member.memberId` (a stable group-scoped ID) is stored in `pendingGrokJoins: memberId → mainGroupId`
3. `grokChat` receives a `receivedGroupInvitation` event and auto-accepts via `grokChat.apiJoinGroup(grokGroupId)`
4. `grokGroupMap` is updated: `mainGroupId → grokLocalGroupId`. The two accounts see the same physical group under different local IDs; this map bridges them.
5. `grokChat` fires `connectedToGroupMember` once fully joined, resolving a 30-second promise in `activateGrok`
6. The bot calls the Grok HTTP API with all prior customer messages as the initial context (so Grok has the full conversation history, not just the most recent message)
7. The response is sent via `grokChat.apiSendTextMessage([Group, grokLocalGroupId], response)` — visible to the customer as a message from "Grok AI"
8. The team group card is updated to reflect the Grok response

### Per-message: ongoing Grok conversation

After the initial response, every subsequent customer text message:
1. Triggers a card update in the team group
2. Triggers a `grokApi.chat(history, text)` call — history is rebuilt each time by reading the last 100 messages from the Grok agent's view of the group (`grokChat.apiGetChat(grokLocalGroupId, 100)`) and mapping Grok's messages to `assistant` role and the customer's messages to `user` role
3. The response is sent from `grokChat` into the group; the team group card is updated

### The double group ID problem

SimpleX assigns local group IDs per account. The same group has a different numeric ID in `mainChat` (e.g. `42`) and in `grokChat` (e.g. `7`). The `grokGroupMap` (`mainGroupId → grokLocalGroupId`) and `reverseGrokMap` (`grokLocalGroupId → mainGroupId`) translate between the two namespaces. Both maps are persisted so a restart doesn't lose active Grok conversations.

### Grok removal

Grok is removed from the group (via `mainChat.apiRemoveMembers`) in three cases:
1. Team member sends their first text message in the customer group
2. Grok API or join fails — graceful fallback, bot notifies the customer
3. Customer leaves the group

On removal, `cleanupGrokMaps` deletes both map entries and persists the change.

---

## Persistent state

The bot writes a single JSON file (`{dbPrefix}_state.json`) that survives restarts. This section explains what is in it, why each piece is there, and what breaks without it.

### Why a state file at all?

SimpleX Chat's own database stores the full message history and group membership, but it does not store the bot's derived knowledge — things like "which local group ID does Grok see for customer group 42?" or "when did this customer first contact us?". That knowledge exists only in the bot's memory and must be written to disk to survive a restart.

### What is persisted and why

| Key | Type | Why persisted | What breaks without it |
|-----|------|---------------|------------------------|
| `teamGroupId` | number | The bot creates the team group on first run; subsequent runs must find the same group | Bot creates a new empty team group on every restart; all team members lose their dashboard |
| `grokContactId` | number | Establishing a bot↔Grok contact takes up to 60 seconds and is a one-time setup | Every restart requires a 60-second re-connection; if it fails the bot exits |
| `grokGroupMap` | {mainGroupId: grokGroupId} | Bridges the two accounts' different local IDs for the same group | Any conversation where Grok was active when the bot restarted can no longer receive Grok responses; Grok is stranded in the group |
| `groupLastActive` | {groupId: timestamp} | Records the last customer message time per group | Dashboard card wait times cannot be computed accurately after restart |
| `groupMetadata` | {groupId: {firstContact, msgCount, customerName}} | Accumulated data that grows over the life of a conversation | Message counters reset to 1 after restart; "first contact" timestamp is lost; dashboard cards show wrong elapsed times |
| `groupPendingInfo` | {groupId: {lastEventFrom, lastEventType, lastEventTimestamp, lastMessageFrom}} | Tracks who sent the last event so the card icon (👋 vs 💬 vs ⏰) can be computed correctly | After restart, the bot knows the state (QUEUE/GROK/TEAM) from live group membership, but cannot tell whether the team has replied since the customer's last message. All TEAM conversations show 👋 until the next event arrives to re-establish the distinction. |

### What is NOT persisted and why

| State | Why ephemeral |
|-------|---------------|
| `welcomeCompleted` | Rebuilt on demand: `isFirstCustomerMessage` scans chat history for the queue/grok/team confirmation texts. Cheap enough that persistence adds no value. |
| `pendingGrokJoins` | In-flight during the 30-second Grok join window only. If the bot restarts during this window, the join either completes or the 30-second timeout has long passed. |
| `pendingOwnerRole` | Set between invite and connect, typically a few seconds. If lost, the owner role isn't set, but the team member can still participate as a Member. |
| `pendingTeamDMs` | Messages queued to greet team members. Lost on restart; the DM is simply not sent. |
| `grokJoinResolvers`, `grokFullyConnected` | Pure async synchronisation primitives. Always empty at startup. |

### Pruning on restore

Not all persisted state is loaded unconditionally on restart:

- **`groupLastActive`** entries older than 48 hours are discarded — they fall outside the window of any bulk-invite command and would only accumulate indefinitely

### Failure modes

If the state file is deleted or corrupted:
- A new team group is created. Team members must re-join it.
- The bot↔Grok contact is re-established (60-second startup delay).
- All active Grok conversations lose their group mapping. Grok remains in those groups as a silent, disconnected participant until the customer or team removes it.
- Message counters and first-contact timestamps reset. Dashboard cards show artificially low counts and short elapsed times.
- Card icons for TEAM conversations degrade to 👋 (no reply yet) until the next event in each conversation arrives, because the bot can no longer tell whether the team has replied since the customer's last message. QUEUE and GROK icons are unaffected — their urgency is derived from `groupLastActive` and live group membership, both of which survive the restart.

---

## Team group setup

### Group creation

The team group is created automatically on first run. Its name is set via the `--team-group` CLI argument. The group ID is written to the state file; subsequent runs reuse the same group. On every startup the bot also re-applies group preferences (direct messages enabled, team commands registered as tappable buttons).

### Joining the team group

On every startup the bot generates a fresh invite link for the team group, prints it to stdout, and deletes it after 10 minutes (or on graceful shutdown). Any stale link from a previous run is deleted first.

The operator shares the link with team members. They must join within the 10-minute window. When a team member joins, the bot automatically establishes a direct-message contact with them and sends:

> Added you to be able to invite you to customer chats later, keep this contact. Your contact ID is `N:name`

This ID is needed for the next step. The DM is sent via a two-step handshake: the bot initiates a member contact, the team member accepts the DM invitation, and the message is delivered on connection.

### Configuring team members

Team members are configured as `--team-members id:name` CLI arguments, using the IDs from the DMs above. The bot validates every configured member against its contact list at startup and exits if any ID is missing or the display name does not match.

Until team members are configured, `/team` commands from customers cannot add anyone to a conversation. The bot logs an error and notifies the customer.

---

## Customer commands

| Command | Available | Effect |
|---------|-----------|--------|
| `/grok` | Before team escalation | Enter Grok mode |
| `/team` | Grok mode or before escalation | Add team member, permanently enter Team mode |

**Unrecognized commands** are treated as normal messages in the current mode.

## Team commands (in team group only)

| Command | Effect |
|---------|--------|
| `/join <groupId>:<name>` | Join the specified customer group as Owner |
