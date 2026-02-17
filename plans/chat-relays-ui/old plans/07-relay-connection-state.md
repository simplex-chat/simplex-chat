# 07 — Relay Connection State During Join (§4.7)

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

**What**: Show relay connection progress during the channel join flow. Three phases with distinct UI:
1. **Pre-join**: Show relay addresses above "Join channel" button so user can evaluate before joining.
2. **Sync phase** (on button tap): Hidden behind button spinner. If ALL relays fail → alert with Retry.
3. **Async phase** (after API success): Per-relay connection status with spinners in bottom panel. Transitions to observer view when complete.

**Why**: Users need transparency about which relays serve a channel before joining, and connection feedback afterward. The two-phase error model (sync all-or-nothing, async per-relay) requires distinct UI handling.

**User impact**: Informed join decision (see relay addresses), clear connection progress (per-relay spinners), and appropriate error handling for each failure mode.

---

## 2. Prerequisites & Dependencies

- **Backend `APIConnectPreparedGroup`**: Relay path must work (sync concurrent connection + async retry).
- **§4.1 (API Type Updates)**: `GroupRelay`, `RelayStatus` types in Swift.
- **§4.2 (Channel Visual Distinction)**: `useRelays` field on `GroupInfo`.
- **Relay addresses in PreparedGroup**: Relay short link domains must be available before join. **Backend change required** — see §3.3 for gap analysis. Until implemented, fall back to Alt A.
- **Async events**: `CEvtGroupLinkConnecting`, `CEvtUserJoinedGroup`, and relay failure events must be surfaced to iOS.

---

## 3. Data Model

### 3.1 Backend State Machine (APIConnectPreparedGroup)

```
User taps "Join channel"
  │
  ▼
Sync phase: mapConcurrently connectToRelay (all relays)
  │
  ├─ ALL fail → throw best error (prefer temp) → UI alert
  │              User can Retry → re-enter sync phase
  │
  ├─ SOME succeed, SOME fail (temp) → setPreparedGroupStartedConnection
  │   │  retryRelayConnectionAsync for temp failures
  │   │  return CRStartedConnectionToGroup
  │   ▼
  │   Async phase: per-relay connection events
  │     CEvtGroupLinkConnecting → relay connecting
  │     CEvtUserJoinedGroup     → relay connected
  │     (first connected host creates chat items)
  │
  └─ ALL succeed → same as above, no retries needed
```

### 3.2 Relay Member Statuses (joining member perspective)

The joining member sees relay members as `GroupMember` with `memberCategory = GCHostMember`:

```
GSMemAccepted  → "Pending"     (relay member created, not yet connecting)
GSMemConnecting → "Connecting"  (CEvtGroupLinkConnecting received)
GSMemConnected  → "Connected"   (CEvtUserJoinedGroup received)
```

Failures: relay member stays in non-connected state. Permanent failures emit `TEGroupLinkRejected` (a terminal event, not a `CEvt`). Note: `CEvtGroupLinkConnecting` is emitted on both `XGrpLinkInv` (relay connecting) and `XGrpLinkReject` (relay rejected) — the event alone does not distinguish success from rejection.

### 3.3 Pre-Join Relay Addresses

**Backend gap**: Current `PreparedGroup` type has only `connLinkToConnect`, `connLinkPreparedConnection`, `connLinkStartedConnection`, `welcomeSharedMsgId`, `requestSharedMsgId` — no relay fields. Relay addresses (`UserContactData.relays :: [ShortLinkContact]`) are fetched from the server's mutable link data inside `APIConnectPreparedGroup` (at join time), not during `APIPrepareGroup`.

**Required backend change**: To show relay addresses before join, `APIPrepareGroup` (or a new API) must resolve the short link and return relay domains in the `PreparedGroup` response. Until this backend change is implemented, fall back to Alt A (no pre-join relay display).

---

## 4. Implementation Plan

### 4.1 `ComposeView.swift` — Pre-Join Relay Address Bar

**Location**: Above `connectButtonView` (L414-419), when `nextConnectPrepared && useRelays`.

Add a compact bar showing relay short addresses between `ContextProfilePickerView` and the join button. Relay addresses come from the prepared group's link data.

```swift
if chat.chatInfo.groupInfo?.nextConnectPrepared == true {
    ContextProfilePickerView(...)
    Divider()
    if let relays = chat.chatInfo.groupInfo?.preparedRelayAddresses {
        relayAddressBar(relays)  // NEW
    }
    connectButtonView("Join channel", icon: "megaphone.fill", connect: connectPreparedGroup)
}
```

### 4.2 `ComposeView.swift` — Join Button Label

When `useRelays`, change label from "Join group" to "Join channel" and icon from `person.2.fill` to `megaphone.fill` (or channel-appropriate icon).

### 4.3 Sync Phase Error Handling

`connectPreparedGroup()` already shows alert on failure via `apiConnectResponseAlert`. For channels, the alert should offer "Retry":
- Temp error (all relays failed with temp errors) → "Failed to connect. Retry?" with Retry/Cancel buttons
- Permanent error → "Failed to join channel." with OK button

### 4.4 `ChatView.swift` — Async Connection Progress Panel

**Location**: Bottom of ChatView, replacing compose area during async connecting phase.

**Condition**: `useRelays && connLinkStartedConnection == true && !allRelaysFinished`

Show per-relay status list with spinners instead of compose bar:

```swift
@ViewBuilder
private func channelConnectionPanel(relays: [RelayConnectionStatus]) -> some View {
    VStack(spacing: 6) {
        Text("Connecting to channel...")
            .font(.callout)
            .foregroundColor(theme.colors.secondary)
        ForEach(relays) { relay in
            HStack {
                relayStatusIndicator(relay)  // spinner, check, or X
                Text(relay.displayAddress)
                    .font(.caption)
                Spacer()
            }
            .padding(.horizontal, 16)
        }
    }
    .padding(.vertical, 8)
}
```

### 4.5 Transition States

Track relay connection progress via events. When all relays finish (connected or permanently failed):

- **All connected**: Hide connection panel → show "you are observer" disabled compose bar
- **Some permanently failed**: Show summary bar ("2 of 3 relays connected") above "you are observer" compose bar
- **All permanently failed**: Show inoperable warning in chat + warning badge in chat list

### 4.6 ChatBannerView — Channel Context Text

When `useRelays`:
- Pre-join: "Tap Join channel" (instead of "Tap Join group")
- After join started: no special context text (just channel name/description)

### 4.7 Reactive Event Handling

Listen for backend events that update relay member statuses:
- `CEvtGroupLinkConnecting` → update relay member status (emitted on both `XGrpLinkInv` and `XGrpLinkReject` — check member status to distinguish connecting from rejected)
- `CEvtUserJoinedGroup` → update relay status to "connected" (CON event on GCHostMember)
- `TEGroupLinkRejected` → mark relay as permanently failed (terminal event, not `CEvt`; provides `GroupRejectionReason`)

---

## 5. Wireframes

### 5.1 Pre-Join — Relay Addresses + Join Button

User has opened a channel link. Chat shows ChatBannerView. Bottom area shows relay addresses above the join button.

```
┌──────────────────────────────────────────┐
│  < [📢] SimpleX News                ... │
│         Channel                          │
├──────────────────────────────────────────┤
│                                          │
│                                          │
│          ┌────────────┐                  │
│          │  [📢 img]  │                  │
│          └────────────┘                  │
│         SimpleX News                     │
│     News and updates from                │
│     the SimpleX Chat team                │
│                                          │
│     Tap Join channel                     │
│                                          │
│                                          │
│                                          │
├──────────────────────────────────────────┤
│  alice (you)                          v  │
├──────────────────────────────────────────┤
│  Relays:                                 │
│  relay1.simplex.im                       │
│  relay2.simplex.im                       │
│  relay3.simplex.im                       │
├──────────────────────────────────────────┤
│         [    Join channel    ]           │
└──────────────────────────────────────────┘
```

**Notes**:
- ChatBannerView: channel image, name, description, "Tap Join channel" context (requires adding `useRelays` branch to `chatContext` in ChatBannerView — currently only has "Tap Join group")
- Profile picker: "alice (you)" with dropdown to select profile / go incognito
- Relay address bar: shows relay short link domains (e.g., `relay1.simplex.im`). **Requires backend change** (§3.3) — relay addresses not currently in PreparedGroup. Until implemented, fall back to Alt A (no pre-join relay display).
- "Join channel" button: full-width, 60pt, megaphone icon

### 5.2 Sync Phase — All Relays Failed (Alert)

User tapped "Join channel", all relay connections failed synchronously.

```
         ┌──────────────────────────┐
         │  Connection failed       │
         │                          │
         │  Failed to connect to    │
         │  channel relays.         │
         │                          │
         │  [ Retry ]    [ Cancel ] │
         └──────────────────────────┘
```

**Notes**:
- Alert shown via existing `apiConnectResponseAlert` mechanism
- "Retry" re-calls `apiConnectPreparedGroup`
- "Cancel" returns to pre-join state (relay addresses + join button still visible)
- Relay list NOT shown yet — sync phase is behind the button

### 5.3 Async Connecting — Per-Relay Spinners

API returned success (`CRStartedConnectionToGroup`). Chat shows ChatBannerView. Bottom panel shows relay connection status.

```
┌──────────────────────────────────────────┐
│  < [📢] SimpleX News                ... │
│         Channel                          │
├──────────────────────────────────────────┤
│                                          │
│          ┌────────────┐                  │
│          │  [📢 img]  │                  │
│          └────────────┘                  │
│         SimpleX News                     │
│     News and updates from                │
│     the SimpleX Chat team                │
│                                          │
│                                          │
│                                          │
│                                          │
│                                          │
├──────────────────────────────────────────┤
│  Connecting to channel...                │
│                                          │
│  ✅  relay1.simplex.im                   │
│  🔄  relay2.simplex.im                   │
│  🔄  relay3.simplex.im                   │
└──────────────────────────────────────────┘
```

**Notes**:
- ChatBannerView: no more "Tap Join channel" context (connLinkStartedConnection is now true)
- Bottom panel replaces compose area
- Per-relay: spinner (🔄) for connecting, checkmark (✅) for connected
- No overall progress bar — individual spinners per relay
- Messages may start arriving through connected relays (displayed normally above the panel)

### 5.4 All Connected — Normal Observer View

All relays connected. Connection panel hidden. Normal channel view.

```
┌──────────────────────────────────────────┐
│  < [📢] SimpleX News                ... │
│         Channel                          │
├──────────────────────────────────────────┤
│                                          │
│          ┌────────────┐                  │
│          │  [📢 img]  │                  │
│          └────────────┘                  │
│         SimpleX News                     │
│     News and updates from                │
│     the SimpleX Chat team                │
│                                          │
│  [📢]  SimpleX News                     │
│  ┌──────────────────────────────────┐   │
│  │ Welcome to the official SimpleX │   │
│  │ News channel!                    │   │
│  │                       5:00 PM  ✓ │   │
│  └──────────────────────────────────┘   │
│                                          │
├──────────────────────────────────────────┤
│  ┌──────────────────────────────┐  [+]  │
│  │ you are observer             │  [📷] │
│  └──────────────────────────────┘  [➤]  │
└──────────────────────────────────────────┘
```

**Notes**:
- ChatBannerView at top, then welcome message (first channel message)
- "you are observer" inside disabled compose field (matching plan 03 observer pattern)
- Connection panel fully hidden — clean transition to normal observer view

### 5.5 Partial Permanent Failure — Summary Bar

Some relays connected, some permanently failed. After all relays finish, replace detailed list with summary.

```
┌──────────────────────────────────────────┐
│  < [📢] SimpleX News                ... │
│         Channel                          │
├──────────────────────────────────────────┤
│                                          │
│  (ChatBannerView + messages)             │
│                                          │
├──────────────────────────────────────────┤
│  ⚠️ 2 of 3 relays connected             │
├──────────────────────────────────────────┤
│  ┌──────────────────────────────┐  [+]  │
│  │ you are observer             │  [📷] │
│  └──────────────────────────────┘  [➤]  │
└──────────────────────────────────────────┘
```

**Notes**:
- Summary bar between messages and compose area
- Yellow warning icon + count text
- Channel is partially functional — messages arrive through connected relays
- "you are observer" compose bar still shown below summary

### 5.6 All Permanently Failed — Inoperable Warning

All relays permanently failed. Channel cannot function.

**In chat**:
```
┌──────────────────────────────────────────┐
│  < [📢] SimpleX News                ... │
│         Channel                          │
├──────────────────────────────────────────┤
│                                          │
│  (ChatBannerView)                        │
│                                          │
├──────────────────────────────────────────┤
│  ❌ Channel not connected                │
│     All relays failed to connect.        │
└──────────────────────────────────────────┘
```

**In chat list**:
```
┌────────────────────────────────────────────────┐
│ ┌────┐                                         │
│ │ 📢 │  SimpleX News           3:42 PM         │
│ │  ⚠ │  Channel not connected                  │
│ └────┘                                         │
└────────────────────────────────────────────────┘
```

**Notes**:
- No "you are observer" compose bar — channel is inoperable
- Chat list shows warning icon overlay on channel avatar + "Channel not connected" preview text
- No compose area at all — just the error banner at bottom

### 5.7 Alt A — No Pre-Join Relay Display

Don't show relay addresses before joining. Just the join button (current behavior for groups).

```
├──────────────────────────────────────────┤
│  alice (you)                          v  │
├──────────────────────────────────────────┤
│         [    Join channel    ]           │
└──────────────────────────────────────────┘
```

Simpler, but user can't evaluate relay infrastructure before joining.

### 5.8 Alt B — Relay Addresses in ChatBannerView

Instead of a separate bar, show relay domains inside the ChatBannerView card:

```
│          ┌────────────┐                  │
│          │  [📢 img]  │                  │
│          └────────────┘                  │
│         SimpleX News                     │
│     News and updates from                │
│     the SimpleX Chat team                │
│                                          │
│     Relays: simplex.im (3)              │
│                                          │
│     Tap Join channel                     │
```

More compact, but mixes channel info with infrastructure details.

### 5.9 Alt C — Minimal Connecting State

Instead of per-relay list, show a single "Connecting..." with spinner:

```
├──────────────────────────────────────────┤
│  🔄 Connecting to channel...             │
└──────────────────────────────────────────┘
```

Simpler, but no per-relay visibility. Acceptable fallback if backend doesn't emit per-relay events.

### 5.10 Alt D — Connection Status at Top

Instead of a bottom panel, show relay connection status below the navigation bar (via `.safeAreaInset(edge: .top)`, same pattern as `searchToolbar()`). This keeps the compose area free for the observer bar during connecting.

**Variant 1 — Full (per-relay list at top)**:
```
┌──────────────────────────────────────────┐
│  < [📢] SimpleX News                ... │
│         Channel                          │
├──────────────────────────────────────────┤
│  Connecting to channel...                │
│  ✅ relay1.simplex.im                    │
│  🔄 relay2.simplex.im                    │
│  🔄 relay3.simplex.im                    │
├──────────────────────────────────────────┤
│                                          │
│  (ChatBannerView)                        │
│                                          │
├──────────────────────────────────────────┤
│  ┌──────────────────────────────┐  [+]  │
│  │ you are observer             │  [📷] │
│  └──────────────────────────────┘  [➤]  │
└──────────────────────────────────────────┘
```

**Variant 2 — Concise (single-line summary at top)**:
```
┌──────────────────────────────────────────┐
│  < [📢] SimpleX News                ... │
│         Channel                          │
├──────────────────────────────────────────┤
│  🔄 Connecting... 1 of 3 relays         │
├──────────────────────────────────────────┤
│                                          │
│  (ChatBannerView + messages)             │
│                                          │
├──────────────────────────────────────────┤
│  ┌──────────────────────────────┐  [+]  │
│  │ you are observer             │  [📷] │
│  └──────────────────────────────┘  [➤]  │
└──────────────────────────────────────────┘
```

**Notes**:
- Top placement uses existing `.safeAreaInset(edge: .top)` pattern (like `searchToolbar()`)
- Observer compose bar is visible immediately (not replaced by connection panel)
- Full variant shows per-relay detail; concise variant is a single progress line
- Collapses to summary or disappears when all relays finish (same transition rules as primary)
- Disadvantage: reduces visible chat content area during connecting

### 5.11 Alt E — Inoperable Warning Variants

**Variant 1 — Prominent in-chat warning card**:
```
│  ┌──────────────────────────────────┐   │
│  │  ❌ Channel not connected        │   │
│  │                                   │   │
│  │  All relays failed to connect.   │   │
│  │  This channel cannot receive     │   │
│  │  messages.                        │   │
│  │                                   │   │
│  └──────────────────────────────────┘   │
```

**Variant 2 — Chat list with distinct badge**:
```
│ ┌────┐                                         │
│ │ 📢 │  SimpleX News           3:42 PM         │
│ │ ❌ │  Not connected                           │
│ └────┘                                         │
```

**Variant 3 — Chat list with offline indicator (subtle)**:
```
│ ┌────┐                                         │
│ │ 📢 │  SimpleX News (offline)  3:42 PM        │
│ │    │  Last message...                         │
│ └────┘                                         │
```

---

## 6. Design Rationale

### Pre-join relay display (Primary) > No relay display (Alt A)

- Users should know which infrastructure relays their messages before committing to join
- SimpleX philosophy: full transparency about infrastructure
- Small addition — typically 3 relay addresses
- Helps privacy-conscious users make informed decisions

### Relay addresses as separate bar (Primary) > In ChatBannerView (Alt B)

- ChatBannerView is for channel identity (name, description, image)
- Relay addresses are infrastructure detail — separate concern
- Easier to add/remove without modifying ChatBannerView layout
- Bar placement (between profile picker and button) creates natural information flow: identity → relays → action

### Per-relay spinners (Primary) > Minimal spinner (Alt C)

- Transparency about per-relay progress
- Only 3 relays typically — small list
- Helps debug connectivity issues
- Consistent with ChannelRelaysView (plan 06)

### Connection panel at bottom (Primary) > At top (Alt E)

- Bottom panel replaces compose area naturally — observer can't send anyway during connecting
- Top placement competes with navigation bar and reduces chat content area
- Bottom is where "you are observer" will eventually appear — same zone, natural transition
- However, top placement allows observer bar to be visible immediately, which may feel more polished

### Summary bar after completion > Keep detailed list

- Detailed list is only useful during active connection
- After all relays finish, per-relay detail is noise
- Summary bar is compact and informative for permanent failures
- Clean transition to normal observer view

### Two-phase error model

- **Sync phase (behind button)**: All-or-nothing from UI perspective. Backend handles partial retries internally. User only sees failure if ALL relays fail → simple alert with Retry.
- **Async phase (visible)**: Per-relay progress with spinners. User sees granular status. Transitions to summary when all finish.
- This separation keeps the common case simple (button → connecting → connected) while providing full detail when needed.

---

## 7. Edge Cases

1. **Relay addresses not available pre-join**: Current `PreparedGroup` does NOT contain relay addresses (see §3.3). Until backend adds relay domains to `APIPrepareGroup` response, fall back to Alt A (no pre-join relay display, just join button).

2. **Instant connection**: All relays connect immediately (fast network). Connection panel appears briefly then transitions to observer view. Consider minimum 0.5s display to prevent jarring flash.

3. **Messages arrive before all relays connected**: Display normally. Messages come through already-connected relays. Connection panel stays at bottom while messages appear above.

4. **User navigates away during async phase**: Connection continues in background. When user returns, show current state (connecting/connected/failed).

5. **Single relay channel**: Only one relay. Progress shows single item. Still useful for connection feedback.

6. **Temp failure during async phase**: Backend retries automatically. UI shows spinner continuing. No user intervention needed.

7. **Mix of temp and permanent failures**: Some relays permanently fail, others still retrying. Show permanent failures as X, retrying as spinner. Transition to summary only when all relays are either connected or permanently failed (no more retries pending).

8. **Channel with no relays in GroupInfo**: Should not happen for channels. Fallback: show generic "Connecting..." (Alt C behavior).

9. **Sync phase partial failure with retry**: Backend handles this internally (retries temp-failed relays async). UI only sees the API response: success or all-fail. No intermediate state visible.

---

## 8. Testing Notes

1. **Pre-join**: Verify relay addresses appear above "Join channel" button for channels
2. **Pre-join (no relays)**: Verify graceful fallback when relay addresses not available
3. **Join button**: Verify "Join channel" label and channel icon (not "Join group")
4. **Sync all-fail**: Mock all relay failures → verify alert with Retry button
5. **Sync all-fail retry**: After Retry, verify second attempt proceeds correctly
6. **Sync partial success**: Mock some succeed, some fail → verify API returns success, async phase starts
7. **Async connecting**: Verify per-relay spinners appear, no progress bar
8. **Async relay connected**: Mock CEvtUserJoinedGroup → verify spinner → checkmark transition
9. **All connected**: Verify connection panel hidden, "you are observer" compose bar appears
10. **ChatBannerView**: Verify channel image, name, description render correctly in all states
11. **Welcome message**: Verify welcome message appears after first relay connects
12. **Partial permanent failure**: Verify summary bar "N of M relays connected" appears
13. **All permanently failed**: Verify in-chat error banner and chat list warning
14. **Navigation away/back**: Leave during connecting, return → verify current state shown
15. **Message during connecting**: Verify messages display above connection panel
