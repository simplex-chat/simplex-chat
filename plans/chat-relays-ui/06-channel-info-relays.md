# 06 — Show Relays in Channel Info (§4.6)

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

**What**: Show relay list with connection status in the channel info screen (GroupChatInfoView). When a group has `useRelays = true`, display a "Relays" section showing each relay's name and status. Owner sees relay management options; members see read-only status.

**Why**: Channel owners need to monitor relay health. Channel members benefit from knowing the relay infrastructure serving their channel. This replaces the member list section (which is irrelevant for channels — members don't see each other).

**User impact**: Channel info screen shows relay infrastructure instead of a member list, clearly communicating the channel's architecture.

---

## 2. Prerequisites & Dependencies

- **§4.2 (Channel Visual Distinction)**: `useRelays` field on `GroupInfo` must exist for conditional rendering.
- **§4.1 (API Type Updates)**: `GroupRelay`, `RelayStatus` types must exist in Swift.
- **Can start as soon as types exist**. No backend dependency beyond the types.

---

## 3. Data Model

### 3.1 GroupRelay (from Types.hs:995)

```haskell
data GroupRelay = GroupRelay
  { groupRelayId :: Int64,
    groupMemberId :: Int64,
    userChatRelayId :: Int64,
    relayStatus :: RelayStatus,
    relayLink :: Maybe Text
  }
```

### 3.2 RelayStatus (from Types.hs:1004)

```haskell
data RelayStatus = RSNew | RSInvited | RSAccepted | RSActive
```

### 3.3 GroupInfo Extension

GroupInfo needs `groupRelays` field:
```swift
public struct GroupInfo {
    // ... existing fields ...
    public var useRelays: Bool = false
    public var groupRelays: [GroupRelay]?  // nil when not loaded or not a channel
}
```

### 3.4 Status Display Mapping

```
RSNew      → "New"        (grey dot)
RSInvited  → "Invited"    (yellow dot)
RSAccepted → "Accepted"   (blue dot)
RSActive   → "Active"     (green dot)
```

### 3.5 Owner vs Member View Logic

```
Owner (groupInfo.isOwner):
  - See relay names, addresses, status
  - Can tap relay for details
  - See "Add relay" option (post-MVP)
  - Can remove relays (post-MVP)

Member (!groupInfo.isOwner):
  - See relay names and status only
  - Read-only view
  - Relay address details hidden (privacy)
```

---

## 4. Implementation Plan

### 4.1 `Shared/Views/Chat/Group/GroupChatInfoView.swift` — Add Relay Section

**Location**: After `infoActionButtons()` section, conditionally replacing or supplementing the member list section.

**Current structure** (simplified):
```swift
List {
    groupInfoHeader()
    localAliasTextEdit()
    infoActionButtons()

    Section {  // group link, support, reports
        if groupInfo.canAddMembers && groupInfo.businessChat == nil { ... }
        ...
    }

    Section {  // edit, welcome, preferences
        if groupInfo.isOwner && groupInfo.businessChat == nil { ... }
        ...
    }

    Section { /* send receipts, TTL */ }

    // MEMBER LIST SECTION
    if !groupInfo.nextConnectPrepared {
        Section(header: Text("\(members.count + 1) members")) {
            if groupInfo.canAddMembers { addMembersButton() }
            // member rows...
        }
    }

    Section { /* clear, delete, leave */ }
}
```

**Change**: Add relay section conditionally when `useRelays`:
```swift
// After infoActionButtons(), before the edit/welcome section:
if groupInfo.useRelays {
    channelRelaysSection()
}

// The member list section changes:
if !groupInfo.nextConnectPrepared {
    if groupInfo.useRelays {
        // For channels: show subscriber count (if available), no member list
        // Members only see relays, not other members
    } else {
        // Existing member list section
        Section(header: ...) { ... }
    }
}
```

**channelRelaysSection**:
```swift
@ViewBuilder
private func channelRelaysSection() -> some View {
    Section(header: Text("Relays")) {
        if let relays = groupInfo.groupRelays {
            ForEach(relays) { relay in
                relayStatusRow(relay)
            }
        } else {
            Text("Loading relays...")
                .foregroundColor(theme.colors.secondary)
        }
    } footer: {
        Text("Relays forward messages to channel subscribers.")
    }
}
```

**relayStatusRow**:
```swift
private func relayStatusRow(_ relay: GroupRelay) -> some View {
    HStack {
        Text(relayDisplayName(relay))
        Spacer()
        relayStatusIndicator(relay.relayStatus)
    }
}

private func relayStatusIndicator(_ status: RelayStatus) -> some View {
    HStack(spacing: 4) {
        Circle()
            .fill(relayStatusColor(status))
            .frame(width: 8, height: 8)
        Text(status.text)
            .font(.caption)
            .foregroundColor(theme.colors.secondary)
    }
}

private func relayStatusColor(_ status: RelayStatus) -> Color {
    switch status {
    case .new: return .gray
    case .invited: return .yellow
    case .accepted: return .blue
    case .active: return .green
    }
}
```

### 4.2 Member List Suppression for Channels

**Location**: Member list section in GroupChatInfoView

**Change**: When `useRelays`, hide the full member list. Channels may have thousands of subscribers — loading and displaying all is impractical. Show subscriber count if available:

```swift
if !groupInfo.nextConnectPrepared {
    if !groupInfo.useRelays {
        // Existing member list
        Section(header: ...) { ... }
    }
    // For channels: no member list shown
    // Subscriber count shown in header or relay section footer
}
```

### 4.3 Edit/Preferences Labels for Channels

**Location**: Section labels in GroupChatInfoView

**Change**: When `useRelays`, adjust labels:
- "Edit group" → "Edit channel"
- "Group preferences" → "Channel preferences"
- "Only group owners can change group preferences." → "Only channel owners can change channel preferences."

Follow existing `businessChat` pattern which already does label switching:
```swift
let label: LocalizedStringKey = (
    groupInfo.businessChat == nil
    ? "Only group owners can change group preferences."
    : "Only chat owners can change preferences."
)
```

Extend to check `useRelays`:
```swift
let label: LocalizedStringKey = (
    groupInfo.useRelays
    ? "Only channel owners can change channel preferences."
    : groupInfo.businessChat == nil
        ? "Only group owners can change group preferences."
        : "Only chat owners can change preferences."
)
```

### 4.4 Suppress Invite Members for Channels

**Location**: "Invite members" / addMembersButton in GroupChatInfoView

**Change**: When `useRelays`, hide the "Invite members" button. Channels gain members through the channel link, not manual invites.

```swift
if groupInfo.canAddMembers && !groupInfo.useRelays {
    addMembersButton()
}
```

### 4.5 Suppress Group Link for Channels

**Location**: Group link section

**Change**: Channel links are managed differently (they include relay info). For MVP, hide the group link button for channels. Channel link management can be added later with proper relay-aware link generation.

---

## 5. Wireframes

### 5.1 Primary Design — Channel Info (Owner View)

```
┌─────────────────────────────────┐
│  < Channel info                 │
├─────────────────────────────────┤
│                                 │
│         ┌─────────┐            │
│         │ [📢 img]│            │
│         └─────────┘            │
│       SimpleX News              │
│                                 │
├─────────────────────────────────┤
│  🔍 Search  │  🔇 Mute         │
├─────────────────────────────────┤
│  RELAYS                         │
│                                 │
│  relay1.simplex.im    ● Active  │
│  relay2.simplex.im    ● Active  │
│  relay3.simplex.im    ● Active  │
│                                 │
│  Relays forward messages to     │
│  channel subscribers.           │
├─────────────────────────────────┤
│  ✏️  Edit channel             > │
│  👋  Welcome message          > │
│  ⚙️  Channel preferences     > │
│                                 │
│  Only channel owners can change │
│  channel preferences.           │
├─────────────────────────────────┤
│  Disappearing messages     OFF  │
│  Chat theme                   > │
├─────────────────────────────────┤
│  🗑  Clear chat                 │
│  ❌  Delete channel             │
│  🚪  Leave channel              │
└─────────────────────────────────┘
```

### 5.2 Primary Design — Channel Info (Member View)

```
┌─────────────────────────────────┐
│  < Channel info                 │
├─────────────────────────────────┤
│                                 │
│         ┌─────────┐            │
│         │ [📢 img]│            │
│         └─────────┘            │
│       SimpleX News              │
│                                 │
├─────────────────────────────────┤
│  🔍 Search  │  🔇 Mute         │
├─────────────────────────────────┤
│  RELAYS                         │
│                                 │
│  relay1.simplex.im    ● Active  │
│  relay2.simplex.im    ● Active  │
│  relay3.simplex.im    ● Active  │
│                                 │
├─────────────────────────────────┤
│  ⚙️  Channel preferences     > │
│                                 │
├─────────────────────────────────┤
│  Disappearing messages     OFF  │
│  Chat theme                   > │
├─────────────────────────────────┤
│  🗑  Clear chat                 │
│  🚪  Leave channel              │
└─────────────────────────────────┘
```

### 5.3 Alternative Design A — Relay Section Collapsed by Default

```
├─────────────────────────────────┤
│  RELAYS (3)                   v │
│  (tap to expand)                │
├─────────────────────────────────┤

Expanded:
├─────────────────────────────────┤
│  RELAYS (3)                   ^ │
│                                 │
│  relay1.simplex.im    ● Active  │
│  relay2.simplex.im    ● Active  │
│  relay3.simplex.im    ● Active  │
├─────────────────────────────────┤
```

### 5.4 Alternative Design B — Relay Status Summary Only

Instead of listing each relay, show a summary:

```
├─────────────────────────────────┤
│  CHANNEL INFO                   │
│  3 relays · all active          │
│  (or: 2 of 3 relays active)    │
├─────────────────────────────────┤
```

### 5.5 State Variations

**Relay connecting (not all active)**:
```
│  RELAYS                         │
│                                 │
│  relay1.simplex.im    ● Active  │
│  relay2.simplex.im    ○ Invited │
│  relay3.simplex.im    ● Active  │
│                                 │
│  Some relays are still          │
│  connecting.                    │
```

**No relays loaded**:
```
│  RELAYS                         │
│                                 │
│  Loading...                     │
│                                 │
```

**Relay error state** (post-MVP):
```
│  relay2.simplex.im    ● Error   │
│  (red dot, "Error" label)       │
```

---

## 6. Design Rationale

**Show individual relays (Primary) > Summary only (Alt B)**:
- Transparency: users can see which relays serve their channel
- Debugging: if one relay has issues, users can identify it
- SimpleX philosophy: full transparency about infrastructure
- Minimal additional UI complexity

**Always expanded (Primary) > Collapsed (Alt A)**:
- Channel has 3 relays typically — not enough to warrant collapsing
- Relay status is important information that should be visible immediately
- Collapsed sections add interaction cost for no benefit with small lists

**Conditional sections based on useRelays**:
- Follows existing `businessChat` conditional pattern
- Minimal code changes — add `useRelays` checks alongside existing `businessChat` checks
- Avoids creating a separate ChannelInfoView (which would duplicate 80%+ of GroupChatInfoView)

---

## 7. Edge Cases

1. **Channel with no relays loaded**: `groupRelays` is nil or empty. Show "Loading..." or empty state.

2. **Channel with all relays failed**: All relays in non-active state. Show warning footer: "No active relays. Channel may not be functioning."

3. **Owner vs member relay visibility**: Owner sees relay addresses (needed for management). Members see relay names but addresses may be truncated or hidden for privacy.

4. **Relay count changes**: If relays are added/removed (post-MVP), the section updates dynamically. Use `@Binding` or `@ObservedObject` pattern for reactive updates.

5. **Subscriber count**: Backend §3.3 (Member Count for Channels) is needed for accurate count. Until then, hide subscriber count or show "Channel" without a count.

6. **Group link button**: Hidden for channels. Channel link is the relay-aware link, managed differently. Post-MVP work to add "Channel link" section.

7. **"Invite members" button**: Hidden for channels. Members join via channel link only.

---

## 8. Testing Notes

1. **Conditional rendering**: Verify relay section appears only when `groupInfo.useRelays == true`
2. **Regular group**: Verify relay section does NOT appear for regular groups
3. **Relay list**: Verify all relays from `groupInfo.groupRelays` are displayed
4. **Status indicators**: Verify correct color/label for each `RelayStatus` value
5. **Owner view**: Verify edit/welcome/preferences buttons visible for owner
6. **Member view**: Verify edit/welcome buttons hidden for non-owner
7. **Label changes**: Verify "Channel preferences" (not "Group preferences") when `useRelays`
8. **No member list**: Verify member list section is hidden for channels
9. **No invite button**: Verify "Invite members" hidden for channels
10. **No group link**: Verify group link button hidden for channels
