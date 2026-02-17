# 02 — Channel Visual Distinction (§4.2)

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

**What**: Visually distinguish channels from regular groups in the chat list, chat header, and group info. Channels display a megaphone icon instead of the group person icon, and show "Channel" label where appropriate.

**Why**: Users must instantly recognize whether a chat is a channel (one-way broadcast) vs. a group (many-to-many). This distinction drives user expectations about interaction mode (read-only vs. participatory).

**User impact**: Every channel appears with a distinct icon and label throughout the app. No functional behavior changes — purely visual.

---

## 2. Prerequisites & Dependencies

- **Backend**: None. `useRelays` field already exists on `GroupInfo` in Haskell (Types.hs:467). Just needs Swift-side decoding.
- **§4.1 (API Type Updates)**: Technically depends on `useRelays` being decoded in Swift `GroupInfo`, but the field addition is trivial and can be done inline here.
- **Can start immediately**.

---

## 3. Data Model

### 3.1 GroupInfo Extension (Swift)

Current `GroupInfo` in `ChatTypes.swift` has no `useRelays` field. In Haskell:

```haskell
-- Types.hs line 467
data GroupInfo = GroupInfo
  { ...
    useRelays :: BoolDef,
    ...
  }

-- Types.hs line 2006
newtype BoolDef = BoolDef {isTrue :: Bool}
  deriving newtype (Eq, Show, ToJSON)

instance FromJSON BoolDef where
  parseJSON v = BoolDef <$> parseJSON v
  omittedField = Just (BoolDef False)  -- defaults to False when absent
```

Swift addition to `GroupInfo`:
```swift
// In GroupInfo struct
var useRelays: Bool  // decoded from BoolDef (just a Bool in JSON)
```

Since `BoolDef` has `omittedField = Just (BoolDef False)`, the JSON field may be absent. Swift side must handle this with a default of `false`.

### 3.2 chatIconName Extension

Current pattern in `GroupInfo` (ChatTypes.swift ~line 2378):
```swift
public var chatIconName: String {
    switch businessChat?.chatType {
    case .none: "person.2.circle.fill"
    case .business: "briefcase.circle.fill"
    case .customer: "person.crop.circle.fill"
    }
}
```

This is consumed by:
- `ChatInfoImage` — avatar in various views
- `ChatPreviewView` — chat list rows
- `CIGroupInvitationView` — group invitation messages
- `ChatInfoToolbar` — chat header
- `GroupChatInfoView` — info screen header

---

## 4. Implementation Plan

### 4.1 `SimpleXChat/ChatTypes.swift` — Add `useRelays` to GroupInfo

**Location**: `GroupInfo` struct definition (~line 2334)

**Change**: Add `useRelays` field with default `false`:
```swift
public struct GroupInfo: Identifiable, Decodable, NamedChat, Hashable {
    // ... existing fields ...
    public var useRelays: Bool = false  // NEW — channels use relays
    // ... existing fields ...
}
```

Use `= false` default so that JSON from older backends (without the field) still decodes correctly. This mirrors `BoolDef`'s `omittedField = Just (BoolDef False)`.

### 4.2 `SimpleXChat/ChatTypes.swift` — Update `chatIconName`

**Location**: `chatIconName` computed property (~line 2378)

**Change**: Add channel check before businessChat switch:
```swift
public var chatIconName: String {
    if useRelays { return "megaphone.fill" }
    switch businessChat?.chatType {
    case .none: return "person.2.circle.fill"
    case .business: return "briefcase.circle.fill"
    case .customer: return "person.crop.circle.fill"
    }
}
```

This single change propagates the channel icon to all consumers of `chatIconName`.

### 4.3 `Shared/Views/ChatList/ChatPreviewView.swift` — Channel Preview Adjustments

**Location**: Chat preview rendering (~line 300)

**Current behavior**: For groups, shows sender name prefix in message preview:
```swift
messageText(..., sender: cItem.meta.showGroupAsSender ? nil : cItem.memberDisplayName, ...)
```

**Change needed**: Channel messages already use `showGroupAsSender = true`, so sender prefix is already suppressed. The icon change from §4.2 is sufficient. No additional changes needed here for MVP.

**Optional enhancement** (post-MVP): Show subscriber count instead of member count in preview subtitle.

### 4.4 `Shared/Views/Chat/ChatInfoToolbar.swift` — Channel Label

**Location**: Chat header toolbar

**Change**: When `groupInfo.useRelays`, show "Channel" subtitle under the group name. Follow existing pattern for business chat labels.

Look for where group member count or status text is shown in the toolbar, and conditionally replace with "Channel" for relay groups.

### 4.5 `Shared/Views/Chat/Group/GroupChatInfoView.swift` — Channel Label in Info

**Location**: Group info header section

**Change**: When `groupInfo.useRelays`, show "Channel" label in the info header area, similar to how business chats show their type.

---

## 5. Wireframes

### 5.1 Primary Design — Chat List

```
REGULAR GROUP:
┌────────────────────────────────────────────────┐
│ ┌────┐                                         │
│ │👥👥│  Team Chat              3:42 PM         │
│ │    │  alice: Hey everyone...          ● 1     │
│ └────┘                                         │
└────────────────────────────────────────────────┘

CHANNEL:
┌────────────────────────────────────────────────┐
│ ┌────┐                                         │
│ │ 📢 │  SimpleX News           3:42 PM         │
│ │    │  Latest update about...          ● 3     │
│ └────┘                                         │
└────────────────────────────────────────────────┘

CHANNEL (with profile image):
┌────────────────────────────────────────────────┐
│ ┌────┐                                         │
│ │ img│  SimpleX News           3:42 PM         │
│ │    │  Latest update about...          ● 3     │
│ └────┘                                         │
└────────────────────────────────────────────────┘
```

Note: When a channel has a profile image, the image is shown (same as groups). The megaphone icon is only the fallback when no image is set. This is the existing `ChatInfoImage` behavior — it uses `chatIconName` only when `image` is nil.

### 5.2 Primary Design — Chat Header

```
┌────────────────────────────────────────────────┐
│  <  [📢]  SimpleX News                    ...  │
│            Channel                              │
└────────────────────────────────────────────────┘

vs. regular group:
┌────────────────────────────────────────────────┐
│  <  [👥]  Team Chat                       ...  │
│            5 members                            │
└────────────────────────────────────────────────┘
```

### 5.3 Alternative Design A — Badge Overlay

Instead of changing the icon, overlay a small megaphone badge on the group avatar:

```
┌────────────────────────────────────────────────┐
│ ┌────┐                                         │
│ │ img│  SimpleX News           3:42 PM         │
│ │  📢│  Latest update about...          ● 3     │
│ └────┘                                         │
└────────────────────────────────────────────────┘
```

Small megaphone badge in bottom-right corner of the avatar circle. Works regardless of whether the channel has a profile image.

### 5.4 Alternative Design B — "Channel" Label in Chat List

```
┌────────────────────────────────────────────────┐
│ ┌────┐                                         │
│ │ img│  SimpleX News           3:42 PM         │
│ │    │  Channel · Latest update...      ● 3     │
│ └────┘                                         │
└────────────────────────────────────────────────┘
```

Add "Channel ·" prefix to the preview subtitle.

### 5.5 State Variations

**No profile image set**:
- Icon: `megaphone.fill` in circle (from `chatIconName`)
- Same layout as groups without images

**Channel with unread messages**:
- Same unread badge behavior as groups (blue dot + count)

**Muted channel**:
- Same mute icon behavior as muted groups

**Channel invitation**:
- `CIGroupInvitationView` already uses `chatIconName` — will show megaphone automatically

---

## 6. Design Rationale

**Primary (icon change via chatIconName) > Alternative A (badge overlay)**:
- Simpler implementation: single property change vs. overlay view composition
- Consistent with businessChat pattern (different icon, not badge)
- Badge overlay would require changes to `ChatInfoImage` layout
- Megaphone is universally understood for broadcast/channel concept

**Primary > Alternative B ("Channel" label)**:
- Label takes horizontal space from message preview
- Icon is more immediately recognizable than text label
- Label approach requires per-view changes; icon approach is automatic

---

## 7. Edge Cases

1. **Channel with businessChat set**: Currently impossible (mutual exclusion in backend). If both were set, `useRelays` check runs first, so channel icon wins. Document this precedence.

2. **GroupInfo from older backend without useRelays field**: `= false` default handles this — groups render as before.

3. **Remote desktop connection**: `useRelays` field uses `BoolDef` with `omittedField`, so older remote hosts that don't send this field will default to `false`. No "invalid chat" risk.

4. **Channel profile image**: When set, profile image is displayed (not megaphone icon). The megaphone is only the fallback icon. This is correct — channels with images should show their image.

---

## 8. Testing Notes

1. **Visual regression**: Verify all existing group icons unchanged when `useRelays = false`
2. **Channel icon**: Create a group with `useRelays = true` in test data, verify megaphone appears in:
   - Chat list
   - Chat header
   - Group info screen
   - Group invitation message
3. **JSON compatibility**: Test decoding `GroupInfo` JSON without `useRelays` field — must default to `false`
4. **Remote desktop**: Verify channel info renders correctly when remote host sends/omits `useRelays`
