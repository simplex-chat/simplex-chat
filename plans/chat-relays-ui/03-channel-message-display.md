# 03 — Channel Message Display (§4.3)

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

**What**: Handle the `CIChannelRcv` chat item direction in iOS message rendering. Channel messages display the channel name and icon as the sender (not a member name), using the existing `showGroupAsSender` rendering path.

**Why**: `CIChannelRcv` is a new `CIDirection` case sent by the backend for messages received in channels. Without handling it, channel messages would crash or be invisible.

**User impact**: Messages in channels show the channel's avatar and name as the sender, making it clear the message is "from the channel" rather than from an individual member.

---

## 2. Prerequisites & Dependencies

- **Backend**: `CIChannelRcv` already exists in Haskell (`Messages.hs:290`). Backend sends it. Swift just needs to decode it.
- **§4.2 (Channel Visual Distinction)**: Should be done first so the channel icon is correct in message avatars.
- **§4.1 (API Type Updates)**: `.channelRcv` must be added to Swift `CIDirection` enum.
- **Can start immediately** (the type addition is small and self-contained).

---

## 3. Data Model

### 3.1 CIDirection in Haskell (Messages.hs)

```haskell
data CIDirection (c :: MsgDirectionI) where
  CIDirectSnd :: CIDirection 'MDSnd
  CIDirectRcv :: CIDirection 'MDRcv
  CIGroupSnd :: CIDirection 'MDSnd
  CIGroupRcv :: GroupMember -> CIDirection 'MDRcv
  CIChannelRcv :: CIDirection 'MDRcv          -- NEW: no member attached
  CILocalSnd :: CIDirection 'MDSnd
  CILocalRcv :: CIDirection 'MDRcv
```

Key difference from `CIGroupRcv`: `CIChannelRcv` has **no `GroupMember` parameter**. The message is from the channel itself, not from any particular member.

### 3.2 CIDirection in Swift (ChatTypes.swift ~line 3432)

Current Swift enum:
```swift
public enum CIDirection: Decodable, Hashable {
    case directSnd
    case directRcv
    case groupSnd
    case groupRcv(groupMember: GroupMember)
    case localSnd
    case localRcv
}
```

Add:
```swift
    case channelRcv  // NEW — no associated member
```

### 3.3 CIMeta.showGroupAsSender

Already exists (`ChatTypes.swift ~line 3477`):
```swift
public struct CIMeta: Decodable, Hashable {
    // ...
    public var showGroupAsSender: Bool
    // ...
}
```

For `CIChannelRcv` items, the backend sets `showGroupAsSender = true`. The existing rendering path in ChatView.swift already handles this flag — it shows the group avatar and group name instead of a member avatar and member name.

### 3.4 Message Rendering Data Flow

```
Backend sends ChatItem with:
  chatDir = CIChannelRcv
  meta.showGroupAsSender = true

iOS receives, decodes:
  ci.chatDir = .channelRcv
  ci.meta.showGroupAsSender = true

ChatView.swift message rendering:
  Currently only enters group avatar/name path when:
    case let .groupRcv(member) = ci.chatDir
  Must ALSO enter that path when:
    case .channelRcv = ci.chatDir

  Inside the path, showGroupAsSender already triggers:
    - Group profile image (or chatIconName fallback)
    - Group display name
    - "group" role label (may need adjustment for channels)
```

---

## 4. Implementation Plan

### 4.1 `SimpleXChat/ChatTypes.swift` — Add `.channelRcv` to CIDirection

**Location**: `CIDirection` enum (~line 3432)

**Change**:
```swift
public enum CIDirection: Decodable, Hashable {
    case directSnd
    case directRcv
    case groupSnd
    case groupRcv(groupMember: GroupMember)
    case channelRcv    // NEW
    case localSnd
    case localRcv
}
```

**Decoding**: Update the `init(from decoder:)` to handle `"channelRcv"` tag. Follow existing pattern:
```swift
// In CIDirection's Decodable init:
case "channelRcv": self = .channelRcv
```

**Hashable/Equatable**: `.channelRcv` has no associated values, so synthesized conformance works.

### 4.2 `SimpleXChat/ChatTypes.swift` — CIDirection Computed Properties

Several computed properties on `CIDirection` need updating:

```swift
// sent property (returns Bool)
public var sent: Bool {
    switch self {
    case .directSnd, .groupSnd, .localSnd: return true
    case .directRcv, .groupRcv, .localRcv: return false
    case .channelRcv: return false  // NEW
    }
}
```

Check all `switch` statements on `CIDirection` in the file and add `.channelRcv` handling. It should behave like `.groupRcv` in most cases (it's a received message in a group context).

### 4.3 `Shared/Views/Chat/ChatView.swift` — Message Rendering

**Location**: Group message rendering block (~lines 1829-1895)

**Current code** (simplified):
```swift
if case let .groupRcv(member) = ci.chatDir,
   case let .group(groupInfo, _) = chat.chatInfo {
    // Shows member/group avatar depending on showGroupAsSender
    HStack(alignment: .top, spacing: 0) {
        // Avatar logic
        if memCount == 1 && (member.memberRole > .member || ci.meta.showGroupAsSender) {
            if ci.meta.showGroupAsSender {
                // Group avatar + group name
                ProfileImage(imageStr: groupInfo.image, iconName: groupInfo.chatIconName, ...)
            } else {
                // Member avatar + member name
                MemberProfileImage(member, ...)
            }
        }
        // Message bubble
        chatItemWithMenu(ci, ...)
    }
}
```

**Change**: Add `.channelRcv` handling. Two approaches:

**Approach A (Preferred)**: Handle `.channelRcv` in the same block, synthesizing the rendering without a member:
```swift
if case let .group(groupInfo, _) = chat.chatInfo {
    if case let .groupRcv(member) = ci.chatDir {
        // existing member-based rendering
    } else if case .channelRcv = ci.chatDir {
        // channel rendering — always show group as sender
        // No member to show, use group avatar + name directly
        HStack(alignment: .top, spacing: 0) {
            // Always show group avatar for channel messages
            ProfileImage(imageStr: groupInfo.image, iconName: groupInfo.chatIconName, ...)
            // Message bubble
            chatItemWithMenu(ci, ...)
        }
    }
}
```

**Approach B**: Since `showGroupAsSender` is always `true` for `CIChannelRcv`, factor out the "show as group" rendering into a shared function and call it from both `.groupRcv` (when `showGroupAsSender`) and `.channelRcv`.

Approach A is simpler. Approach B avoids duplication if the rendering logic is complex. Decide based on how much code the `showGroupAsSender` path contains.

### 4.4 `Shared/Views/Chat/ChatView.swift` — Message Grouping

Messages are grouped by consecutive same-sender for avatar display optimization (show avatar only on first message in a group). The grouping logic checks `ci.chatDir`:

**Location**: Message grouping / `memCount` calculation

**Change**: `CIChannelRcv` messages should always be considered "same sender" (the channel), so they group together. Ensure the grouping comparator treats consecutive `.channelRcv` items as same sender.

### 4.5 `Shared/Views/ChatList/ChatPreviewView.swift` — Preview Text

**Location**: Chat preview message rendering (~line 300)

**Current**: `showGroupAsSender` already suppresses member name prefix:
```swift
messageText(..., sender: cItem.meta.showGroupAsSender ? nil : cItem.memberDisplayName, ...)
```

**Change needed**: `cItem.memberDisplayName` may crash or return empty for `.channelRcv` (no member). Ensure `memberDisplayName` handles `.channelRcv` gracefully (return `nil`).

Check the `memberDisplayName` computed property on `ChatItem`:
```swift
public var memberDisplayName: String? {
    if case let .groupRcv(groupMember) = chatDir {
        return groupMember.displayName
    }
    return nil
}
```

Add `.channelRcv` case — return `nil` (no member to display):
```swift
public var memberDisplayName: String? {
    switch chatDir {
    case let .groupRcv(groupMember): return groupMember.displayName
    case .channelRcv: return nil
    default: return nil
    }
}
```

### 4.6 Other CIDirection Switch Exhaustiveness

Search for all `switch ci.chatDir` or `case .groupRcv` pattern matches in Swift code. Each needs `.channelRcv` handling. Key files:

- `ChatView.swift` — message rendering, reactions, context menus
- `ChatPreviewView.swift` — preview text
- `ChatItemView.swift` — item layout
- `ComposeView.swift` — reply context
- `ChatItemInfoView.swift` — item details
- `ChatTypes.swift` — computed properties

For each, `.channelRcv` should behave like `.groupRcv` but without member data.

---

## 5. Wireframes

### 5.1 Primary Design — Channel Messages

```
┌─────────────────────────────────────────────┐
│  < [📢] SimpleX News                   ... │
│         Channel                             │
├─────────────────────────────────────────────┤
│                                             │
│  [📢 img]  SimpleX News                    │
│  ┌───────────────────────────────────────┐  │
│  │ We're excited to announce v7.0!      │  │
│  │ New channel feature allows large     │  │
│  │ public groups with relay-forwarded   │  │
│  │ messages.                            │  │
│  │                           3:42 PM  ✓ │  │
│  └───────────────────────────────────────┘  │
│                                             │
│  ┌───────────────────────────────────────┐  │
│  │ Check out the blog post:             │  │
│  │ simplex.chat/blog/v7                 │  │
│  │                           3:45 PM  ✓ │  │
│  └───────────────────────────────────────┘  │
│                                             │
│  [📢 img]  SimpleX News                    │
│  ┌───────────────────────────────────────┐  │
│  │ We're also working on...             │  │
│  │                           4:10 PM  ✓ │  │
│  └───────────────────────────────────────┘  │
│                                             │
│  ─────────────────────────────────────────  │
│  (compose area hidden for observers)        │
│  Reactions: [👍] [❤️] [+]                   │
└─────────────────────────────────────────────┘
```

**Key elements**:
- Channel avatar (profile image or megaphone fallback) shown for first message in group
- Channel name shown as sender
- Consecutive messages from same channel grouped (avatar hidden on 2nd message)
- Avatar shown again after time gap (same as member grouping rules)
- Compose bar hidden for observer role (existing behavior)
- Reaction bar shown (existing observer behavior)

### 5.2 Primary Design — Chat List Preview

```
┌────────────────────────────────────────────────┐
│ ┌────┐                                         │
│ │ 📢 │  SimpleX News           3:42 PM         │
│ │    │  Latest update about v7...       ● 3     │
│ └────┘                                         │
└────────────────────────────────────────────────┘
```

No sender prefix in preview (already handled by `showGroupAsSender`).

### 5.3 Alternative Design — Compact Channel Header

Instead of showing channel avatar + name on every first message, show a thin header bar:

```
┌─────────────────────────────────────────────┐
│  ───── SimpleX News · 3:42 PM ─────        │
│  ┌───────────────────────────────────────┐  │
│  │ We're excited to announce v7.0!      │  │
│  │                           3:42 PM  ✓ │  │
│  └───────────────────────────────────────┘  │
│                                             │
│  ┌───────────────────────────────────────┐  │
│  │ Check out the blog post:             │  │
│  │                           3:45 PM  ✓ │  │
│  └───────────────────────────────────────┘  │
│                                             │
│  ───── SimpleX News · 4:10 PM ─────        │
│  ┌───────────────────────────────────────┐  │
│  │ We're also working on...             │  │
│  └───────────────────────────────────────┘  │
└─────────────────────────────────────────────┘
```

### 5.4 State Variations

**Single message**:
- Always shows avatar + name (first message in group)

**Mixed content (message + image + file)**:
- Same rendering as group messages, just with channel avatar instead of member avatar

**Forwarded message indicator**:
- If the channel message was originally forwarded from another chat, the forwarding indicator still appears

**Deleted/moderated message**:
- Shows "[deleted]" placeholder with channel avatar, same as group moderation

**Loading state**:
- N/A — messages arrive via existing real-time pipeline

---

## 6. Design Rationale

**Primary (avatar + name like group messages) > Alternative (compact header bar)**:
- Consistent with existing group message rendering — users already understand the pattern
- `showGroupAsSender` path already implements this exact rendering
- Compact header would require new view component and different grouping logic
- Avatar provides visual anchor, especially in channels with profile images

**No member associated with CIChannelRcv**:
- Correct by design: channel messages are "from the channel", not from a specific person
- The owner's identity is deliberately hidden from members in channels
- This matches the `showGroupAsSender` intent — present messages as group-authored

---

## 7. Edge Cases

1. **CIChannelRcv in non-group chat context**: Should not happen (backend only sends this direction for group chats). If it occurs, render as a simple received message without avatar.

2. **Reply/quote to channel message**: When quoting a `.channelRcv` message, the quote should show "SimpleX News" (channel name) as the quoted sender, not a member name.

3. **Reactions to channel messages**: Work the same as group reactions. Observer members can add reactions (existing behavior).

4. **Search results**: Channel messages in search should show channel name as context, not member name.

5. **Message info view**: Tapping message info on a `.channelRcv` item should not show member details (no member). Show channel info or minimal info.

6. **Old client receiving CIChannelRcv**: If Swift CIDirection decoding fails for unknown `"channelRcv"` tag, the item falls back to `CIInvalidJSON`. This is acceptable — old clients can't join relay groups anyway (version gated).

7. **Consecutive `.groupRcv` and `.channelRcv`**: In theory, a channel could have both member messages (from admin sending as member) and channel messages. Grouping should treat these as different senders (different avatar).

---

## 8. Testing Notes

1. **Decode test**: Add JSON test for `CIDirection` with `"channelRcv"` tag — verify decodes to `.channelRcv`
2. **Rendering test**: Mock a `ChatItem` with `.channelRcv` direction and `showGroupAsSender = true` — verify channel avatar and name appear
3. **Preview test**: Verify chat list preview for channel messages has no sender prefix
4. **Grouping test**: Verify consecutive `.channelRcv` messages group correctly (avatar shown only on first)
5. **Exhaustiveness**: Build with all warnings — verify no missing cases for `.channelRcv` in switch statements
6. **memberDisplayName**: Verify returns `nil` for `.channelRcv` items
7. **Quote/reply**: Quote a channel message and verify the quoted sender shows channel name
