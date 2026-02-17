# 04 — Channel Creation Flow (§4.4)

## Table of Contents
1. [Overview](#1-overview)
2. [Prerequisites & Dependencies](#2-prerequisites--dependencies)
3. [Data Model](#3-data-model)
4. [Implementation Plan](#4-implementation-plan)
5. [Wireframes](#5-wireframes)
6. [Design Rationale](#6-design-rationale)
7. [Edge Cases](#7-edge-cases)
8. [Testing Notes](#8-testing-notes)

---

## 1. Overview

**What**: "Create channel" entry in New Chat sheet → creation wizard with 3 phases: form (name/image) → relay selection & connection → channel link.

**Why**: Channels use `APINewPublicGroup` with relay infrastructure, fundamentally different from group creation.

**User impact**: Guided channel creation with relay progress feedback and link sharing.

---

## 2. Prerequisites & Dependencies

- **§4.1 (API Type Updates)**: `apiNewPublicGroup`, `UserChatRelay`, `GroupRelay`, `RelayStatus` types.
- **Backend §3.2**: Relay status events (`groupLinkRelaysUpdated`).
- **§4.5 (Relay Management)**: User must have relays configured (preset relays always exist — Network & servers validates that user can't remove/disable all relays).

---

## 3. Data Model

### API Command
```swift
case apiNewPublicGroup(userId: Int64, incognito: Bool, relayIds: [Int64], groupProfile: GroupProfile)
```

### API Response
```swift
case publicGroupCreated(user: UserRef, groupInfo: GroupInfo, groupLink: GroupLink, groupRelays: [GroupRelay])
```

Returns group link immediately. Relay statuses update via `groupLinkRelaysUpdated` events.

### Relay Status Progression
```
RSNew → RSInvited → RSAccepted → RSActive
```

### Backend Behavior (Two Phases)

**Synchronous phase**: `apiNewPublicGroup` creates the group and sends relay invitations. If any invitation fails to be **sent**, the entire API call fails — channel is NOT created.

**Asynchronous phase**: After all invitations are sent successfully, the API succeeds. Relays then independently accept invitations (RSNew → RSInvited → RSAccepted → RSActive). Partial failure is possible here (some relays accept, others don't), but there is no recovery mechanism yet.

---

## 4. Implementation Plan

### 4.1 Entry Point — `NewChatMenuButton.swift`

Add "Create channel" NavigationLink after "Create group" (line ~127).

### 4.2 Creation View — `AddChannelView.swift` (new file)

Modeled on `AddGroupView.swift`. Three phases controlled by `@State`:

```
Phase 1: createChannelView()       — name + image form
Phase 2: relaySelectionView()      — relay selection → create → connection status
Phase 3: channelLinkView()         — link sharing (GroupLinkView pattern)
```

Phase 2 is a separate view that serves dual purpose: first relay selection, then connection monitoring after API call.

### 4.3 Phase transitions

```
Phase 1 → Phase 2:
  User enters name/image, taps "Continue" → pushes relay selection view

Phase 2 (selection mode):
  Shows relay list with toggles, "Create" button
  createChannel() called:
    → apiNewPublicGroup(relayIds: selectedRelayIds, ...)
    → on success: store groupInfo, groupLink, groupRelays → switch to status mode
    → on failure: show error alert with Retry

Phase 2 (status mode):
  → listen for groupLinkRelaysUpdated events
  → update groupRelays state
  → "Proceed" enabled when all relays Active
  → "Skip waiting" shown when ≥1 relay Active but not all

Phase 3:
  → show QR code + share/copy link (like GroupLinkView with creatingGroup: true)
  → "Continue" toolbar button → dismiss all sheets, open channel
```

---

## 5. Wireframes

### 5.1 Phase 1 — Creation Form

Follows `AddGroupView` layout: centered image picker, pencil icon + placeholder text field (no label), continue button.

```
┌─────────────────────────────────┐
│  < Create channel               │
├─────────────────────────────────┤
│                                 │
│         ┌─────────┐            │
│         │         │            │
│         │  [cam]  │            │
│         │         │            │
│         └─────────┘            │
│                                 │
├─────────────────────────────────┤
│  ✏️  Enter channel name...      │
│                                 │
│  ➤  Continue                    │
│                                 │
│  Your profile **alice** will    │
│  be shared with channel relays. │
│  Subscribers see only the       │
│  channel name.                  │
└─────────────────────────────────┘
```

**Notes**:
- Text field: pencil icon + placeholder only (matches `AddGroupView.groupNameTextField()`)
- No incognito toggle for MVP (owner identity hidden from subscribers by design)
- "Continue" disabled until name valid; pushes to Phase 2 (relay selection & connection view)
- Actual "Create channel" button is in Phase 2 after relay selection

### 5.2 Phase 2 — Relay Selection (Before API Call)

Separate view showing user's relays with toggles. All enabled relays pre-selected.

```
┌─────────────────────────────────┐
│  < Channel relays               │
├─────────────────────────────────┤
│  SIMPLEX CHAT RELAYS            │
│  ☑ relay1.simplex.im            │
│  ☑ relay2.simplex.im            │
│  ☑ relay3.simplex.im            │
├─────────────────────────────────┤
│  YOUR RELAYS                    │
│  ☐ myrelay.example.com          │
├─────────────────────────────────┤
│                                 │
│  ┌─────────────────────────┐   │
│  │     Create channel       │   │
│  └─────────────────────────┘   │
│                                 │
│  Select relays for the channel. │
│  At least one relay required.   │
└─────────────────────────────────┘
```

### 5.3 Phase 2 — Relay Connection Status (After API Success)

Same view transitions to show per-relay status. Toggles replaced by status indicators. Back button hidden (channel already created, can't undo — matches `GroupLinkView` with `creatingGroup: true`).

```
┌─────────────────────────────────┐
│  Creating channel...            │
├─────────────────────────────────┤
│                                 │
│         ┌─────────┐            │
│         │ [image] │            │
│         └─────────┘            │
│       SimpleX News              │
│                                 │
├─────────────────────────────────┤
│  CHAT RELAYS                    │
│                                 │
│  relay1.simplex.im              │
│  ✅ Active                      │
│                                 │
│  relay2.simplex.im              │
│  🔄 Invited                     │
│                                 │
│  relay3.simplex.im              │
│  🔄 Invited                     │
│                                 │
├─────────────────────────────────┤
│                                 │
│  ┌─────────────────────────┐   │
│  │       Proceed            │   │
│  └─────────────────────────┘   │
│  (enabled when all Active)      │
│                                 │
│       Skip waiting              │
│  (shown when ≥1 Active,         │
│   not all Active yet)           │
│                                 │
└─────────────────────────────────┘
```

**Relay status indicators**:
| Status | Display |
|--------|---------|
| RSNew | `● New` |
| RSInvited | `🔄 Invited` |
| RSAccepted | `🔄 Accepted` |
| RSActive | `✅ Active` |

**"Skip waiting"** only visible when ≥1 relay Active but not all.

**"Proceed"** navigates to Phase 3 (channel link view).

### 5.4 Phase 2 — All Relays Active

```
┌─────────────────────────────────┐
│  Channel created                │
├─────────────────────────────────┤
│         ┌─────────┐            │
│         │ [image] │            │
│         └─────────┘            │
│       SimpleX News              │
├─────────────────────────────────┤
│  CHAT RELAYS                    │
│  relay1.simplex.im   ✅ Active  │
│  relay2.simplex.im   ✅ Active  │
│  relay3.simplex.im   ✅ Active  │
├─────────────────────────────────┤
│  ┌─────────────────────────┐   │
│  │       Proceed            │   │
│  └─────────────────────────┘   │
└─────────────────────────────────┘
```

### 5.5 Phase 3 — Channel Link

Modeled on `GroupLinkView` with `creatingGroup: true`. Shows channel link for sharing before navigating to the channel.

```
┌─────────────────────────────────┐
│  Channel link          Continue │
├─────────────────────────────────┤
│                                 │
│  You can share a link or QR     │
│  code — anyone can use it to    │
│  join the channel.              │
│                                 │
├─────────────────────────────────┤
│  ┌─────────────────────────┐   │
│  │                         │   │
│  │      [QR Code]          │   │
│  │                         │   │
│  └─────────────────────────┘   │
│                                 │
│  🔗  Share link                 │
│                                 │
└─────────────────────────────────┘
```

**"Continue"** in toolbar → dismisses all sheets, opens channel chat.

### 5.6 Phase 1 Alt A — Inline Relay Selection

Relay toggles as a section in the creation form (no separate relay view). Here "Create channel" directly calls the API since relay selection is inline:

```
┌─────────────────────────────────┐
│  < Create channel               │
├─────────────────────────────────┤
│         ┌─────────┐            │
│         │  [cam]  │            │
│         └─────────┘            │
├─────────────────────────────────┤
│  ✏️  Enter channel name...      │
│                                 │
│  ☑  Create channel              │
│                                 │
│  Your profile **alice** will    │
│  be shared with channel relays. │
├─────────────────────────────────┤
│  CHAT RELAYS                    │
│  ☑ relay1.simplex.im    preset  │
│  ☑ relay2.simplex.im    preset  │
│  ☐ myrelay.example.com  custom  │
│                                 │
│  Select relays for the channel. │
│  Configure in Network settings. │
└─────────────────────────────────┘
```

### 5.6 Phase 1 Alt B — No Relay Selection

Use 3 enabled relays automatically. Info-only summary in form:

```
│  ☑  Create channel              │
│                                 │
│  CHAT RELAYS                    │
│  ● relay1.simplex.im    preset  │
│  ● relay2.simplex.im    preset  │
│  ● relay3.simplex.im    preset  │
│                                 │
│  Preset relays are used.        │
│  Configure in Network settings. │
```

### 5.7 Error — API Failure: Invitation Send Failed (Current)

Synchronous phase: any relay invitation fails to be sent → whole API fails, channel NOT created. Alert with Retry.

```
┌──────────────────────────────────────┐
│                                      │
│         Error creating channel       │
│                                      │
│  Relay invitation failed:            │
│  relay2.simplex.im: connection       │
│  timeout                             │
│                                      │
│  ┌──────────┐  ┌──────────────────┐  │
│  │    OK    │  │      Retry       │  │
│  └──────────┘  └──────────────────┘  │
│                                      │
└──────────────────────────────────────┘
```

**OK** → stays on relay selection (user can adjust relays and try again).
**Retry** → calls `apiNewPublicGroup` again with same parameters.

### 5.8 Error — Async: Partial Relay Acceptance Failure (Current)

Asynchronous phase: API succeeded (all invitations sent), but some relays fail to accept. Channel IS created. No recovery mechanism currently.

```
┌─────────────────────────────────┐
│  Channel created                │
├─────────────────────────────────┤
│         ┌─────────┐            │
│         │ [image] │            │
│         └─────────┘            │
│       SimpleX News              │
├─────────────────────────────────┤
│  CHAT RELAYS                    │
│                                 │
│  relay1.simplex.im              │
│  ✅ Active                      │
│                                 │
│  relay2.simplex.im              │
│  ❌ Failed to accept            │
│                                 │
│  relay3.simplex.im              │
│  ✅ Active                      │
│                                 │
│  ⚠️  1 relay failed. Channel    │
│  works with remaining relays.   │
├─────────────────────────────────┤
│  ┌─────────────────────────┐   │
│  │       Proceed            │   │
│  └─────────────────────────┘   │
└─────────────────────────────────┘
```

"Proceed" enabled because ≥1 relay is Active. Failed relays are shown but not actionable.

### 5.9 Error — Async: All Relays Fail to Accept (Current)

All invitations were sent (API succeeded), but no relay accepts.

```
┌─────────────────────────────────┐
│  Channel created                │
├─────────────────────────────────┤
│         ┌─────────┐            │
│         │ [image] │            │
│         └─────────┘            │
│       SimpleX News              │
├─────────────────────────────────┤
│  CHAT RELAYS                    │
│  relay1.simplex.im  ❌ Failed   │
│  relay2.simplex.im  ❌ Failed   │
│  relay3.simplex.im  ❌ Failed   │
│                                 │
│  All relays failed to accept.   │
│  Channel won't work without     │
│  active relays.                 │
├─────────────────────────────────┤
│  ┌─────────────────────────┐   │
│  │       Proceed            │   │  ← disabled
│  └─────────────────────────┘   │
│                                 │
│  ┌─────────────────────────┐   │
│  │    Delete channel        │   │
│  └─────────────────────────┘   │
└─────────────────────────────────┘
```

### 5.10 Future Improvement 1 — Partial Sync Failure

Backend improved to succeed if at least some relay invitations are sent. Channel created with subset of relays; unsent relays show as failed.

```
┌──────────────────────────────────────┐
│                                      │
│         Channel created              │
│                                      │
│  2 of 3 relay invitations sent.      │
│  relay2.simplex.im: send failed.     │
│                                      │
│  ┌──────────────────────────────┐    │
│  │        Retry failed          │    │
│  └──────────────────────────────┘    │
│                                      │
└──────────────────────────────────────┘
```

### 5.11 Future Improvement 2 — Retry for Async Acceptance Failures

Backend adds retry for relays that fail to accept invitations. Failed relays in status view become actionable.

```
│  relay2.simplex.im              │
│  ❌ Failed to accept    [Retry] │
```

"Retry" re-sends the invitation to the specific relay. On success, relay re-enters RSNew → RSInvited → RSAccepted → RSActive progression.

---

## 6. Design Rationale

### Phase separation (Form → Relay Selection & Connection → Link)

Follows `AddGroupView` pattern: form → post-creation view. Adding the link phase matches the incognito group flow (`GroupLinkView` with `creatingGroup: true`).

### Relay selection: Separate view (primary) recommended

- Clear separation: name/image form stays simple like AddGroupView
- Relay selection view transitions naturally into connection status (same view, dual purpose)
- User focuses on one thing at a time
- Alt A (inline) clutters the creation form with relay infrastructure details
- Alt B (no selection) is too rigid — user may want different relay sets per channel

### "Proceed" instead of "Open channel"

User needs to see and share the channel link before entering the channel. "Proceed" → link view → "Continue" matches the incognito group creation flow.

### "Skip waiting" constraint

Only after ≥1 Active relay. Without any active relays, the channel can't deliver messages, so skipping is meaningless.

### Error model

Two distinct failure modes reflect backend architecture:
- **Sync** (invitation sending): all-or-nothing currently → simple Retry
- **Async** (invitation acceptance): partial failure possible, no recovery → show status, let user proceed with working relays

Future improvements add granularity to both phases independently.

---

## 7. Edge Cases

1. **Network loss during creation**: API call fails. Show error alert with Retry.
2. **Network loss during relay connection**: Relays stuck in RSNew/RSInvited. User can "Skip waiting" once ≥1 is Active, or dismiss sheet (channel exists, relays continue in background).
3. **Sheet dismissed during relay connection**: Channel already created. Appears in chat list. Relays finish connecting in background.
4. **Channel name validation**: Same as groups — `validDisplayName()`, trim whitespace.
5. **Duplicate names**: Backend allows. No client-side check.
6. **Sheet height**: Add ~44pt for new "Create channel" row. Update `sheetHeight`.

---

## 8. Testing Notes

1. Entry point: "Create channel" in NewChatSheet after "Create group"
2. Form: empty name → disabled; valid name → enabled
3. Image picker: same behavior as group creation
4. Relay selection: all enabled relays pre-selected; toggle works; ≥1 required
5. API success → relay selection view transitions to status mode
6. API failure → alert with OK + Retry
7. Relay status updates via `groupLinkRelaysUpdated` → UI updates per-relay
8. "Skip waiting" only visible when ≥1 Active, not all Active
9. "Proceed" → channel link view with QR + share
10. "Continue" → dismisses sheets, opens channel
11. Partial async failure → shows failed relays, Proceed still enabled
12. All async failure → Proceed disabled, Delete channel shown
