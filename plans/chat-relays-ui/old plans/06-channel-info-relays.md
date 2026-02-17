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

**What**: Add "Chat relays" NavigationLink in channel info that opens a separate relay list view (`ChannelRelaysView`). Adapt existing `GroupChatInfoView` section structure for channels with appropriate label changes. Existing section order and structure is preserved — no rearrangement.

**Why**: Channel owners need to monitor relay health. Separating relays into their own view keeps channel info clean while providing full detail on demand. Reusing the existing GroupChatInfoView structure avoids duplicating 80%+ of the view.

**User impact**: Channel info feels familiar (same structure as group info). "Channel link" and "Chat relays" buttons appear below action buttons. Relay details are one tap away.

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

---

## 4. Implementation Plan

### Existing GroupChatInfoView Section Structure (verified, lines 74-190)

```
1. groupInfoHeader()                          // avatar, name, description
2. localAliasTextEdit()                       // "Set chat name..."
3. infoActionButtons()                        // Search, [Add members], Mute
4. Section (links/support):                   // L91-107
     groupLinkButton()                        // if canAddMembers && !businessChat
     memberSupportButton()                    // if !businessChat && >= moderator
     GroupReportsChatNavLink                   // if canModerate
     UserSupportChatNavLink                   // if memberActive && (< moderator || supportChat)
5. Section (edit/prefs):                      // L109-125
     editGroupButton()                        // if isOwner && !businessChat
     addOrEditWelcomeMessage()                // if description || (isOwner && !businessChat)
     GroupPreferencesButton                   // always
     Footer: "Only group owners can change group preferences."
6. Section (local settings):                  // L127-141
     sendReceiptsOption()                     // or disabled variant for >20 members
     NavigationLink → ChatWallpaperEditorSheet  // "Chat theme"
     ChatTTLOption                            // "Delete messages after"
     Footer: "Delete chat messages from your device."
7. Members section:                           // L143-172, if !nextConnectPrepared
     Header: "N members"
     addMembersButton()                       // if canAddMembers
     searchFieldView, member rows
8. Destructive section:                       // L174-182
     clearChatButton(), deleteGroupButton(), leaveGroupButton()
```

**Note**: There is no standalone "Disappearing messages" toggle in this view. Disappearing messages is a group preference accessed via GroupPreferencesButton → GroupPreferencesView. "Delete messages after" (ChatTTLOption) is local cleanup, a different feature.

### 4.1 Section 4 (links/support) — Add Channel Link and Chat Relays

**Location**: L91-107.

**Changes**:
- `groupLinkButton()`: relabel to "Channel link" / "Create channel link" when `useRelays`
- NEW: `channelRelaysButton()` NavigationLink when `useRelays`, placed after channel link
- Existing items (memberSupportButton, GroupReportsChatNavLink, UserSupportChatNavLink) unchanged — their existing conditions naturally apply to channels

```swift
// Existing condition extended:
if groupInfo.canAddMembers && groupInfo.businessChat == nil {
    groupLinkButton()  // label: "Channel link" when useRelays
}
// NEW:
if groupInfo.useRelays {
    channelRelaysButton()  // NavigationLink → ChannelRelaysView
}
// Remaining items unchanged
```

### 4.2 Section 5 (edit/prefs) — Channel Labels

**Location**: L109-125.

When `useRelays`, adjust labels following existing `businessChat` pattern:
- `editGroupButton()` label: "Edit group profile" → "Edit channel profile"
- `GroupPreferencesButton` label: "Group preferences" → "Channel preferences"
- Footer: "Only channel owners can change channel preferences."

### 4.3 Section 7 (members) — Subscriber Count

**Location**: L143-172.

When `useRelays`: replace full member list with subscriber count header only. Channels may have thousands of subscribers — loading all is impractical. Suppress `addMembersButton` and member rows.

```swift
if !groupInfo.nextConnectPrepared {
    if groupInfo.useRelays {
        Section(header: Text("\(members.count + 1) subscribers")) {
            // Empty — no member rows for channels
        }
    } else {
        // Existing member list
    }
}
```

### 4.4 Section 8 (destructive) — Channel Labels

**Location**: L174-182.

When `useRelays`:
- "Delete group" → "Delete channel"
- "Leave group" → "Leave channel"

### 4.5 Section 6 (local settings) — Hide Send Receipts for Channels

**Location**: L127-141.

When `useRelays`, hide `sendReceiptsOption()`. Observers can't send messages, and relay-mediated delivery changes receipt semantics. Only "Chat theme" and "Delete messages after" remain.

### 4.6 `ChannelRelaysView.swift` — New Separate View

Destination for "Chat relays" NavigationLink. Shows relay list with status indicators. Both owner and member see the same view (read-only relay status).

### 4.7 `infoActionButtons()` — Suppress Invite for Channels

When `useRelays`, hide `addMembersActionButton`. Channel members join via channel link, not manual invites.

### 4.8 `groupLinkButton()` — Channel Link Label

When `useRelays`:
- "Create group link" → "Create channel link"
- "Group link" → "Channel link"
- Navigation title: "Channel link"

---

## 5. Wireframes

### 5.1 Channel Info — Owner View

Preserves existing GroupChatInfoView section structure. Channel-specific: "Channel link" and "Chat relays" in links section; channel labels; subscriber count instead of member list.

```
┌──────────────────────────────────────┐
│  < Channel info                      │
├──────────────────────────────────────┤
│                                      │
│           ┌─────────┐               │
│           │ [📢 img]│               │
│           └─────────┘               │
│         SimpleX News                 │
│         News and updates             │
│                                      │
│  Set chat name...                    │
├──────────────────────────────────────┤
│  🔍 Search  │  🔇 Mute              │
├──────────────────────────────────────┤
│  🔗  Channel link                 >  │
│  📡  Chat relays                  >  │
│  💬  Chats with members           >  │
│  ⚠️  Member reports               >  │
│                                      │
├──────────────────────────────────────┤
│  ✏️  Edit channel profile         >  │
│  👋  Welcome message              >  │
│  ⚙️  Channel preferences         >  │
│                                      │
│  Only channel owners can change      │
│  channel preferences.                │
├──────────────────────────────────────┤
│  Chat theme                       >  │
│  Delete messages after            >  │
│                                      │
│  Delete chat messages from           │
│  your device.                        │
├──────────────────────────────────────┤
│  150 SUBSCRIBERS                     │
│  (no member rows / only for owner)   │
├──────────────────────────────────────┤
│  🗑  Clear chat                      │
│  ❌  Delete channel                  │
│  🚪  Leave channel                   │
└──────────────────────────────────────┘
```

**Notes**:
- "Channel link" and "Chat relays" are NavigationLinks in the links section (Section 4)
- "Chat relays" opens separate `ChannelRelaysView` (see §5.3)
- "Chats with members" / "Member reports" are existing items shown via existing conditions (owner is >= moderator)
- "Channel preferences" contains Disappearing messages setting (not a separate field)
- "Delete messages after" is ChatTTLOption (local cleanup), different from Disappearing messages (group preference)
- "150 SUBSCRIBERS" is the section header, no member rows below
- Send receipts row omitted for channels — observers can't send messages, receipt behavior is relay-mediated

### 5.2 Channel Info — Member/Observer View

Non-admin members don't see Channel link, Edit, Welcome message, or Delete channel.

```
┌──────────────────────────────────────┐
│  < Channel info                      │
├──────────────────────────────────────┤
│                                      │
│           ┌─────────┐               │
│           │ [📢 img]│               │
│           └─────────┘               │
│         SimpleX News                 │
│         News and updates             │
│                                      │
│  Set chat name...                    │
├──────────────────────────────────────┤
│  🔍 Search  │  🔇 Mute              │
├──────────────────────────────────────┤
│  📡  Chat relays                  >  │
│  💬  Chat with admins             >  │
│                                      │
├──────────────────────────────────────┤
│  ⚙️  Channel preferences         >  │
│                                      │
│  Only channel owners can change      │
│  channel preferences.                │
├──────────────────────────────────────┤
│  Chat theme                       >  │
│  Delete messages after            >  │
│                                      │
│  Delete chat messages from           │
│  your device.                        │
├──────────────────────────────────────┤
│  150 SUBSCRIBERS                     │
├──────────────────────────────────────┤
│  🗑  Clear chat                      │
│  🚪  Leave channel                   │
└──────────────────────────────────────┘
```

**Notes**:
- No "Channel link" (requires canAddMembers = admin+)
- "Chat relays" visible — members see relay status (read-only)
- "Chat with admins" is existing UserSupportChatNavLink (shown when memberActive && < moderator)
- No "Edit channel profile" (requires isOwner)
- "Welcome message" appears if channel has a description (`groupProfile.description != nil`) — omitted from wireframe for simplicity
- No "Delete channel" (requires canDelete)

### 5.3 ChannelRelaysView — Separate Relay List (Primary)

Opened via "Chat relays" NavigationLink. Same view for owner and member.

```
┌──────────────────────────────────────┐
│  < Chat relays                       │
├──────────────────────────────────────┤
│                                      │
│  relay1.simplex.im         ● Active  │
│  relay2.simplex.im         ● Active  │
│  relay3.simplex.im         ● Active  │
│                                      │
│  Relays forward messages to          │
│  channel subscribers.                │
└──────────────────────────────────────┘
```

**Notes**:
- Navigation title: "Chat relays"
- Each row: relay name + colored status dot + status text
- Footer explains relay purpose
- Read-only for MVP (no add/remove relay actions — that's post-MVP)

### 5.4 Alt A — Inline Relay Section in Channel Info

Instead of a separate view, show relays directly in channel info (between action buttons and edit section):

```
├──────────────────────────────────────┤
│  🔗  Channel link                 >  │
│                                      │
├──────────────────────────────────────┤
│  RELAYS                              │
│                                      │
│  relay1.simplex.im         ● Active  │
│  relay2.simplex.im         ● Active  │
│  relay3.simplex.im         ● Active  │
│                                      │
│  Relays forward messages to          │
│  channel subscribers.                │
├──────────────────────────────────────┤
│  ✏️  Edit channel profile         >  │
│  ...                                 │
```

### 5.5 Alt B — Owner Member List

Instead of subscriber count only, owner sees a member list (for managing admins):

```
├──────────────────────────────────────┤
│  150 SUBSCRIBERS                     │
│                                      │
│  🔍 Search members...               │
│  alice (you)              owner      │
│  bob                      admin      │
│  charlie                  admin      │
│  ... (loaded members)                │
│                                      │
├──────────────────────────────────────┤
```

**Notes**:
- Only shown for owner/admin — members see count only
- Limited to loaded members (not all subscribers)
- Useful for managing channel admins

### 5.6 State Variations

**Relays connecting (not all active)**:
```
│  relay1.simplex.im         ● Active  │
│  relay2.simplex.im         ○ Invited │
│  relay3.simplex.im         ● Active  │
│                                      │
│  Some relays are still               │
│  connecting.                         │
```

**Relays loading**:
```
│  Loading relays...                   │
```

**All relays failed** (post-MVP):
```
│  relay1.simplex.im         ● Error   │
│  relay2.simplex.im         ● Error   │
│  relay3.simplex.im         ● Error   │
│                                      │
│  No active relays. Channel may       │
│  not be functioning.                 │
```

---

## 6. Design Rationale

### Separate relay view (Primary) > Inline relay section (Alt A)

- Keeps channel info clean — relay details are secondary information
- Channel info preserves the same compact feel as group info
- Relay view can be extended with management features (post-MVP) without cluttering channel info
- Follows the pattern of "Channel link" navigating to GroupLinkView — relay status is similarly a detail view

### Inline relay section (Alt A) considerations

- Fewer taps to see relay status — good for monitoring
- But adds visual weight to channel info
- Reasonable alternative for channels with few relays (typically 3)

### Subscriber count only (Primary) > Owner member list (Alt B)

- Channels may have thousands of subscribers — loading all is impractical
- Most subscribers are observers with no individual identity visible to each other
- Owner typically manages a small set of admins — could be a separate admin management view

### Owner member list (Alt B) considerations

- Owner needs to manage admins (promote, remove)
- Could show only admins/moderators instead of all subscribers
- Reasonable post-MVP addition

### Preserving GroupChatInfoView section structure

- Follows existing `businessChat` conditional pattern
- Minimal code changes — add `useRelays` checks alongside existing conditions
- Avoids creating a separate ChannelInfoView (would duplicate 80%+ of logic)
- Every existing section maps to a channel equivalent

---

## 7. Edge Cases

1. **Channel with no relays loaded**: `groupRelays` is nil or empty. ChannelRelaysView shows "Loading relays..." placeholder.

2. **Channel with all relays failed**: All relays in non-active state. Show warning footer in ChannelRelaysView.

3. **Owner vs member view**: Both see same ChannelRelaysView (read-only relay status). Owner-specific management is post-MVP.

4. **Subscriber count accuracy**: Backend §3.3 (Member Count for Channels) needed for accurate count. Until then, use `members.count + 1` (loaded members).

5. **Channel link**: Uses existing `groupLinkButton()` with relabeled text. `GroupLinkView` may need relay-aware link generation in future.

6. **"Invite members" suppressed**: `addMembersActionButton` and `addMembersButton` hidden when `useRelays`. Members join via channel link.

7. **businessChat AND useRelays**: These are independent flags. If both set (unlikely), `useRelays` checks should take priority for label changes.

---

## 8. Testing Notes

1. **Channel info**: Verify "Channel link" and "Chat relays" appear in links section for channels
2. **ChannelRelaysView**: Verify navigation from "Chat relays" opens separate view with relay list
3. **Regular group**: Verify "Chat relays" does NOT appear; normal member list shown
4. **Status indicators**: Verify correct color/label for each RelayStatus value in ChannelRelaysView
5. **Owner view**: Verify Channel link, Edit channel profile, Welcome message visible for owner
6. **Member view**: Verify Channel link and Edit hidden; Chat relays and Chat with admins visible
7. **Label changes**: Verify "Channel preferences" (not "Group preferences") when useRelays
8. **Subscriber count**: Verify "N subscribers" header replaces member list for channels
9. **No invite button**: Verify addMembersActionButton and addMembersButton hidden for channels
10. **Disappearing messages**: Verify it is NOT a separate field — only accessible via Channel preferences
11. **Delete messages after**: Verify ChatTTLOption present in local settings section for channels
