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

**What**: Handle `CIChannelRcv` direction in iOS message rendering. Channel messages display the channel name and icon as sender via the existing `showGroupAsSender` path.

**Why**: Without handling, channel messages would crash or fall back to `CIInvalidJSON`.

**User impact**: Messages in channels show the channel's avatar and name as sender, making it clear messages are "from the channel" rather than from an individual.

---

## 2. Prerequisites & Dependencies

- **§4.2 (Channel Visual Distinction)**: Channel icon should be correct in message avatars.
- **§4.1 (API Type Updates)**: `.channelRcv` added to Swift `CIDirection` (done).
- **Can start immediately** — type addition is self-contained.

---

## 3. Data Model

### CIDirection

Haskell `CIChannelRcv` has **no `GroupMember` parameter** — message is from the channel itself.

Swift (already added in §4.1):
```swift
case channelRcv  // no associated member
```

### Rendering Path

Backend sets `showGroupAsSender = true` for `CIChannelRcv` items. The existing rendering in ChatView.swift (lines 1829-1895) shows group avatar + name when this flag is set. `.channelRcv` items should enter this same code path.

---

## 4. Implementation Plan

### 4.1 `ChatView.swift` — Message Rendering (~line 1829)

Extend the group message rendering block to handle `.channelRcv`:
```swift
if case let .group(groupInfo, _) = chat.chatInfo {
    if case let .groupRcv(member) = ci.chatDir {
        // existing member-based rendering
    } else if case .channelRcv = ci.chatDir {
        // channel rendering — always show group avatar + name
    }
}
```

### 4.2 `ChatView.swift` — Message Grouping

Consecutive `.channelRcv` items should be treated as "same sender" (the channel) for avatar grouping.

### 4.3 `ChatTypes.swift` — CIDirection Computed Properties

All `switch` statements on `CIDirection` need `.channelRcv` handling. It behaves like `.groupRcv` but returns `nil` for member-related properties.

### 4.4 `ChatPreviewView.swift` — Preview Text (~line 300)

`memberDisplayName` already returns `nil` for `.channelRcv` (no member to display). `showGroupAsSender` suppresses sender prefix. No sender prefix in channel previews.

### 4.5 Exhaustive Switch Audit

Check all `switch ci.chatDir` / `case .groupRcv` in:
- `ChatItemView.swift`, `ComposeView.swift`, `ChatItemInfoView.swift`
- `ChatItemsMerger.swift` (done — `.channelRcv: 3`)

---

## 5. Wireframes

### 5.1 Primary Design — Channel Messages (Subscriber View)

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
├─────────────────────────────────────────────┤
│  ┌─────────────────────────────────┐  [+]  │
│  │ you are observer                │  [📷] │
│  └─────────────────────────────────┘  [➤]  │
└─────────────────────────────────────────────┘
```

**Key elements**:
- Channel avatar shown for first message in a group; hidden on consecutive messages
- Avatar shown again after time gap (same grouping rules as member messages)
- Compose field **visible but disabled** with italic "you are observer" text inside the field
- Tapping disabled compose area shows alert: "You can't send messages!" / "Please contact group admin."
- This matches existing observer behavior in groups (ComposeView.swift:718-725, `disabledText` parameter)

### 5.2 Primary Design — Owner View

```
┌─────────────────────────────────────────────┐
│  < [📢] SimpleX News                   ... │
│         Channel                             │
├─────────────────────────────────────────────┤
│                                             │
│  [📢 img]  SimpleX News                    │
│  ┌───────────────────────────────────────┐  │
│  │ We're excited to announce v7.0!      │  │
│  │                           3:42 PM  ✓ │  │
│  └───────────────────────────────────────┘  │
│                                             │
├─────────────────────────────────────────────┤
│  📢 Posting as SimpleX News                 │
│  ┌─────────────────────────────────┐  [+]  │
│  │ Type a message...               │  [📷] │
│  └─────────────────────────────────┘  [➤]  │
└─────────────────────────────────────────────┘
```

Owner sees enabled compose bar with "Posting as [Channel Name]" label. Messages are sent with `sendAsGroup: true`.

### 5.3 Primary Design — Chat List Preview

```
┌────────────────────────────────────────────────┐
│ ┌────┐                                         │
│ │ 📢 │  SimpleX News           3:42 PM         │
│ │    │  Latest update about v7...       ● 3     │
│ └────┘                                         │
└────────────────────────────────────────────────┘
```

No sender prefix in preview (handled by `showGroupAsSender`).

### 5.4 Alternative Design — Compact Channel Header

Instead of avatar + name on first message, show a thin centered header:

```
│  ───── SimpleX News · 3:42 PM ─────        │
│  ┌───────────────────────────────────────┐  │
│  │ We're excited to announce v7.0!      │  │
│  │                           3:42 PM  ✓ │  │
│  └───────────────────────────────────────┘  │
```

### 5.5 State Variations

- **Single message**: Always shows avatar + name
- **Mixed content (image + file)**: Same rendering, channel avatar instead of member avatar
- **Deleted/moderated**: "[deleted]" placeholder with channel avatar
- **Reply/quote**: Quoted sender shows channel name, not member name

---

## 6. Design Rationale

**Primary (avatar + name) > Alternative (compact header)**:
- Consistent with existing group message rendering
- `showGroupAsSender` path already implements this
- Compact header would need new view component

**Observer compose area not hidden**:
- Matches existing group observer behavior (ComposeView shows disabled bar + label)
- Keeps consistent UX — users see "you are observer" in both groups and channels

---

## 7. Edge Cases

1. **CIChannelRcv in non-group context**: Should not happen. Render as simple received message.
2. **Reply/quote**: Show channel name as quoted sender.
3. **Reactions**: Work same as groups. Observers can react (existing behavior).
4. **Message info view**: No member details. Show channel info.
5. **Old client**: Falls back to `CIInvalidJSON` (version-gated).
6. **Mixed .groupRcv and .channelRcv**: Different senders for grouping purposes.

---

## 8. Testing Notes

1. JSON decode test for `CIDirection` with `"channelRcv"` tag
2. Rendering: channel avatar + name appear for `.channelRcv` items
3. Preview: no sender prefix in chat list
4. Grouping: consecutive `.channelRcv` items group correctly
5. Exhaustiveness: build with warnings — no missing `.channelRcv` cases
6. Observer compose: disabled bar with "you are observer" text
