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

**What**: Show relay connection progress when a member joins a channel. The progress is displayed **inside ChatView** — above or instead of the compose area — not as a separate modal or view. The user lands in the channel's chat immediately and sees relay connection status inline.

**Why**: When joining a channel, the member needs to connect to multiple relays. This takes time. Showing per-relay progress directly in the chat view gives the user immediate context and avoids blocking navigation.

**User impact**: After joining a channel link, the user is taken directly to the channel chat. At the bottom of the chat (where the compose bar normally is), they see relay connection progress. Once all relays are connected, the progress disappears and the normal observer UI appears (reactions only, no compose).

---

## 2. Prerequisites & Dependencies

- **Backend §3.2 (Relay Connection State Events)**: Backend must emit per-relay connection state updates. Without this, the UI can only show a generic "Connecting..." spinner.
- **§4.1 (API Type Updates)**: `GroupRelay`, `RelayStatus` types needed for state display.
- **§4.2 (Channel Visual Distinction)**: `useRelays` field to detect channel context.
- **Existing connect plan flow**: `planAndConnect` / `APIPrepareGroup` → `APIConnectPreparedGroup` flow must support relay groups (backend §3.1).

---

## 3. Data Model

### 3.1 Relay Connection State Events

Backend emits events when relay connection state changes during join. Expected shape:

```swift
// ChatResponse variant (from backend §3.2)
case groupRelayStatus(groupInfo: GroupInfo, relay: GroupRelay)
```

Or batch update:
```swift
case groupRelaysUpdated(groupInfo: GroupInfo, relays: [GroupRelay])
```

### 3.2 RelayStatus Progression During Join

For a joining member, relays progress through states:
```
RSNew → RSInvited → RSAccepted → RSActive
```

The member sees this as:
```
New       → "Pending"    (not yet contacted)
Invited   → "Connecting" (relay invitation sent)
Accepted  → "Connected"  (relay accepted, setting up)
Active    → "Active"     (ready to receive messages)
```

### 3.3 Channel Join State in ChatView

ChatView needs to know:
1. Is this a channel? (`groupInfo.useRelays`)
2. Are we currently joining? (relays not all active)
3. Per-relay status for display

The channel's `GroupInfo` already has `groupRelays: [GroupRelay]?`. The connection progress is derived from the relay statuses.

---

## 4. Implementation Plan

### 4.1 `Shared/Views/Chat/ChatView.swift` — Connection Progress in Compose Area

**Location**: Bottom of ChatView, where the compose bar appears

**Current behavior**: For observer members in groups, the compose bar is already hidden and replaced with a reactions-only bar. For channels, members are observers by default.

**Change**: When a channel's relays are not all active (still connecting), show a connection progress view instead of the reactions bar:

```swift
// In ChatView's bottom area:
@ViewBuilder
private func bottomBar() -> some View {
    if case let .group(groupInfo, _) = chat.chatInfo,
       groupInfo.useRelays,
       let relays = groupInfo.groupRelays,
       !relays.allSatisfy({ $0.relayStatus == .active }) {
        // Show relay connection progress
        channelConnectionProgress(groupInfo: groupInfo, relays: relays)
    } else if /* existing observer check */ {
        // Existing reactions-only bar
    } else {
        // Existing compose bar
    }
}
```

### 4.2 Connection Progress Component

**New component** (inline in ChatView or extracted):

```swift
@ViewBuilder
private func channelConnectionProgress(groupInfo: GroupInfo, relays: [GroupRelay]) -> some View {
    let activeCount = relays.filter { $0.relayStatus == .active }.count
    let totalCount = relays.count

    VStack(spacing: 8) {
        Divider()

        HStack {
            Image(systemName: "megaphone")
                .foregroundColor(theme.colors.secondary)
            Text("Connecting to channel...")
                .font(.callout)
                .foregroundColor(theme.colors.secondary)
            Spacer()
            Text("\(activeCount)/\(totalCount)")
                .font(.caption)
                .foregroundColor(theme.colors.secondary)
        }
        .padding(.horizontal, 16)

        // Per-relay status indicators
        ForEach(relays) { relay in
            HStack(spacing: 8) {
                Circle()
                    .fill(relayStatusColor(relay.relayStatus))
                    .frame(width: 8, height: 8)
                Text(relayDisplayName(relay))
                    .font(.caption)
                    .foregroundColor(theme.colors.onBackground)
                Spacer()
                Text(relay.relayStatus.displayText)
                    .font(.caption)
                    .foregroundColor(theme.colors.secondary)
            }
            .padding(.horizontal, 16)
        }

        // Progress bar
        ProgressView(value: Double(activeCount), total: Double(totalCount))
            .padding(.horizontal, 16)
            .padding(.bottom, 8)
    }
    .background(theme.colors.background)
}
```

### 4.3 Reactive State Updates

**Location**: ChatView's event handling

**Change**: Listen for relay status change events and update `groupInfo.groupRelays`:

```swift
.onReceive(chatModel.relayStatusUpdates) { update in
    if update.groupId == chat.chatInfo.apiId {
        // Update relay status in groupInfo
        if var gInfo = chat.chatInfo.groupInfo {
            gInfo.groupRelays = update.relays
            // Update chat info in model
        }
    }
}
```

The exact mechanism depends on how the backend §3.2 events are surfaced to the UI. Options:
1. Published property on ChatModel
2. NotificationCenter notification
3. Direct ChatItem update (service message)

### 4.4 Transition to Normal State

When all relays become active:
1. Connection progress view disappears
2. Normal observer UI appears (reactions bar)
3. Optionally, a service message is inserted: "Connected to channel"

The transition should be smooth — no jarring view change.

### 4.5 `Shared/Views/Chat/ComposeMessage/ComposeView.swift` — No Changes Needed

The compose view is already hidden for observer members. The connection progress replaces the compose area at a higher level (ChatView), so ComposeView doesn't need modification.

---

## 5. Wireframes

### 5.1 Primary Design — Connection Progress in ChatView

```
┌──────────────────────────────────────────┐
│  < [📢] SimpleX News               ...  │
│         Channel                          │
├──────────────────────────────────────────┤
│                                          │
│  ─── Connected to channel ───           │
│  (or: welcome message displayed here)    │
│                                          │
│                                          │
│                                          │
│                                          │
│                                          │
│                                          │
│                                          │
├──────────────────────────────────────────┤
│  📢 Connecting to channel...      2/3   │
│                                          │
│  ● relay1.simplex.im         Active      │
│  ○ relay2.simplex.im         Connecting  │
│  ● relay3.simplex.im         Active      │
│                                          │
│  ━━━━━━━━━━━━━━━━━━▒▒▒▒▒▒▒▒            │
└──────────────────────────────────────────┘
```

### 5.2 Primary Design — All Connected

```
┌──────────────────────────────────────────┐
│  < [📢] SimpleX News               ...  │
│         Channel                          │
├──────────────────────────────────────────┤
│                                          │
│  ─── Connected to channel ───           │
│                                          │
│  [📢]  SimpleX News                     │
│  ┌──────────────────────────────────┐   │
│  │ Welcome! This is the official   │   │
│  │ SimpleX News channel.           │   │
│  │                       5:00 PM ✓ │   │
│  └──────────────────────────────────┘   │
│                                          │
├──────────────────────────────────────────┤
│  Reactions: [👍] [❤️] [+]               │
└──────────────────────────────────────────┘
```

Once all relays are active, the connection progress disappears and the normal observer reactions bar appears.

### 5.3 Primary Design — Connection Starting

```
┌──────────────────────────────────────────┐
│  < [📢] SimpleX News               ...  │
│         Channel                          │
├──────────────────────────────────────────┤
│                                          │
│                                          │
│        (empty chat area)                 │
│                                          │
│                                          │
│                                          │
│                                          │
├──────────────────────────────────────────┤
│  📢 Connecting to channel...      0/3   │
│                                          │
│  ○ relay1.simplex.im         Pending     │
│  ○ relay2.simplex.im         Pending     │
│  ○ relay3.simplex.im         Pending     │
│                                          │
│  ━▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒        │
└──────────────────────────────────────────┘
```

### 5.4 Alternative Design A — Minimal Progress (Spinner Only)

Instead of per-relay detail, show a compact spinner in the compose area:

```
├──────────────────────────────────────────┤
│  📢 Connecting to channel...  [spinner]  │
└──────────────────────────────────────────┘
```

### 5.5 Alternative Design B — Banner at Top

Show connection progress as a banner at the top of the chat, not at the bottom:

```
┌──────────────────────────────────────────┐
│  < [📢] SimpleX News               ...  │
├──────────────────────────────────────────┤
│  Connecting... 2 of 3 relays    [━━▒▒]  │
├──────────────────────────────────────────┤
│                                          │
│  (chat messages area)                    │
│                                          │
├──────────────────────────────────────────┤
│  Reactions: [👍] [❤️] [+]               │
└──────────────────────────────────────────┘
```

### 5.6 State Variations

**Network error on a relay**:
```
│  ● relay1.simplex.im         Active      │
│  ✕ relay2.simplex.im         Error       │
│  ● relay3.simplex.im         Active      │
│                                          │
│  Some relays could not connect.          │
│  Messages may be delayed.                │
```

**All relays failed**:
```
│  📢 Connection failed                   │
│                                          │
│  ✕ relay1.simplex.im         Error       │
│  ✕ relay2.simplex.im         Error       │
│  ✕ relay3.simplex.im         Error       │
│                                          │
│  Could not connect to channel relays.    │
│  [Retry]                                 │
```

**Partial success (enough to function)**:
```
│  📢 Connected to channel          2/3   │
│                                          │
│  ● relay1.simplex.im         Active      │
│  ✕ relay2.simplex.im         Error       │
│  ● relay3.simplex.im         Active      │
│                                          │
│  ━━━━━━━━━━━━━━━━━━━━━━━━━▒▒▒▒▒        │
```

Transitions to normal reactions bar since enough relays are active.

---

## 6. Design Rationale

**In-ChatView (Primary) > Separate modal/view**:
- User is already in the channel — no context switching needed
- Connection progress is temporary — fits naturally in the transient compose area
- Follows the pattern of ConnectProgressManager which shows progress inline
- Users can still see chat header, welcome messages, etc. while connecting

**Bottom placement (Primary) > Top banner (Alt B)**:
- Bottom placement replaces the compose area which is already unavailable during connection
- Top banner competes with navigation bar and chat header
- Bottom is where the user's eye naturally rests when waiting for something

**Per-relay detail (Primary) > Spinner only (Alt A)**:
- Transparency about what's happening — user can see which relays are connecting
- Helps debug connectivity issues (can see which relay is slow/failing)
- Only 3 relays typically — doesn't take much space
- Consistent with channel info relay list (§4.6)

---

## 7. Edge Cases

1. **User joins channel with 1 relay**: Progress shows single relay status. Still useful for connection feedback.

2. **Instant connection**: All relays connect immediately (fast network). Progress briefly flashes and transitions to normal view. Consider minimum display time of ~0.5s to prevent jarring flash.

3. **User navigates away during connection**: Connection continues in background. When user returns, current state is shown (or normal view if all connected).

4. **Message arrives before all relays connected**: Display the message normally. Connection progress doesn't block message display — messages can arrive through already-active relays.

5. **Channel with no relays in groupInfo**: Should not happen for channels, but handle gracefully — show generic "Connecting..." without per-relay detail.

6. **Relay status goes backward** (Active → Error): Backend reconnection event. Update UI to show error state. This is edge case for network instability.

7. **Very slow connection**: After timeout (e.g., 60s), show "Taking longer than expected. Check your network connection." but don't auto-cancel.

8. **User force-closes app during connection**: On relaunch, the channel is in the chat list. Connection resumes automatically in background. Chat view shows current state.

---

## 8. Testing Notes

1. **Entry point**: Join channel link → navigate to channel chat → verify connection progress appears in compose area
2. **Per-relay progress**: Mock relay status events → verify each relay row updates
3. **Progress bar**: Verify progress bar reflects active/total ratio
4. **Transition**: All relays become active → verify progress disappears and reactions bar appears
5. **Messages during connection**: Mock message arrival while connecting → verify message displays normally above progress area
6. **Error state**: Mock relay error → verify error indicator and message shown
7. **Retry**: After all-failed state, verify "Retry" button reconnects
8. **Partial success**: 2 of 3 relays active → verify normal view appears with functional channel
9. **Navigation away/back**: Leave chat during connection, return → verify current state displayed
10. **ChatView layout**: Verify connection progress doesn't overlap with messages or navigation
