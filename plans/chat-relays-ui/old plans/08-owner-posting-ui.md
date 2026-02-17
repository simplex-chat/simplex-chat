# 08 — Owner Posting UI (§4.8)

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

**What**: When the channel owner composes a message, always send as channel (`asGroup = true` in the API call). Show a visual indicator "Posting as [Channel Name]" above the compose bar.

**Why**: In channels, the owner's messages should appear as "from the channel" (via `showGroupAsSender` / `CIChannelRcv`), not from the owner personally. The UI must enforce this for MVP — no toggle to send as member.

**User impact**: Channel owners see a clear label showing they're posting as the channel, not as themselves. All their messages appear to subscribers as "from SimpleX News" (or whatever the channel name is).

---

## 2. Prerequisites & Dependencies

- **§4.2 (Channel Visual Distinction)**: `useRelays` field on `GroupInfo` to detect channel context.
- **Backend**: The `apiSendMessages` function already supports sending to groups. For channels, the owner sends with `scope = nil` (no special scope), and the backend handles `showGroupAsSender` based on the channel configuration. No special `asGroup` parameter needed in the API call for MVP — backend automatically sets `showGroupAsSender = true` for channel messages.
- **Can start immediately** (pending §4.2 for `useRelays` detection).

---

## 3. Data Model

### 3.1 Sending Flow Analysis

Current send path in `ComposeView.swift`:
```swift
func send(_ msgs: [ComposedMessage], live: Bool, ttl: Int?) async -> [ChatItem] {
    // ...
    await apiSendMessages(
        type: chat.chatInfo.chatType,
        id: chat.chatInfo.apiId,
        scope: chat.chatInfo.groupChatScope(),
        live: live,
        ttl: ttl,
        composedMessages: msgs
    )
}
```

For channels:
- `chatType` = `.group`
- `scope` = `nil` (main group scope, not member support)
- Backend automatically handles channel semantics (owner messages get `showGroupAsSender = true`)

### 3.2 Send Mechanism

Per launch plan §2.7: "UI always passes `asGroup=True`. Backend does not enforce — owners retain the API option to send as member for future use."

**VERIFICATION NEEDED BEFORE IMPLEMENTATION**: Confirm with backend whether `apiSendMessages` requires an explicit `asGroup` parameter or whether the backend automatically applies `showGroupAsSender = true` for all messages in channels with `useRelays = true`. The implementation path differs:

- **If automatic**: No send-side changes needed — only add the visual "Posting as" indicator.
- **If explicit `asGroup` parameter required**: Add `asGroup: true` to the `apiSendMessages` call when `chat.chatInfo.groupInfo?.useRelays == true`.

Either way, the visual indicator (§4.1 below) is needed regardless.

### 3.3 Owner Detection

```swift
// In ChatView / ComposeView context:
if case let .group(groupInfo, _) = chat.chatInfo,
   groupInfo.useRelays,
   groupInfo.isOwner {
    // Owner posting in channel — show indicator
}
```

`groupInfo.isOwner` checks `groupInfo.membership.memberRole == .owner`.

---

## 4. Implementation Plan

### 4.1 `Shared/Views/Chat/ComposeMessage/ComposeView.swift` — Channel Posting Indicator

**Location**: Above the compose text field, as a contextual label

**Pattern**: Similar to how quote/edit/forward context appears above the compose bar:
- `ComposeView` already shows context items above the text field (reply quotes, edit context, forward items)
- Channel posting indicator is a simpler version — a static label when in channel owner context

**Change**: Add a channel posting banner:

```swift
// In ComposeView's body, before the text field:
@ViewBuilder
private func channelPostingBanner() -> some View {
    if case let .group(groupInfo, _) = chat.chatInfo,
       groupInfo.useRelays,
       groupInfo.membership.memberRole >= .owner {
        HStack(spacing: 6) {
            Image(systemName: "megaphone")
                .foregroundColor(theme.colors.secondary)
                .font(.caption)
            Text("Posting as \(groupInfo.displayName)")
                .font(.caption)
                .foregroundColor(theme.colors.secondary)
        }
        .padding(.horizontal, 12)
        .padding(.vertical, 4)
    }
}
```

Insert this before the compose text field in the VStack.

### 4.2 Admin Posting Behavior

For admins (not owners) in channels — they may also be able to post as the channel. Check the role threshold:

Per launch plan §2.7: "Non-owner/non-admin members can only send reactions (observer role enforced by existing role system)."

So admins can post too. Adjust the role check:
```swift
groupInfo.membership.memberRole >= .admin
```

This means both owners and admins see the "Posting as [Channel]" banner and their messages are sent as channel messages.

### 4.3 Send Flow Changes (Depends on §3.2 Verification)

**Path A — Backend automatic**: No changes to the send function. The compose view sends normally and the backend applies `showGroupAsSender = true` automatically.

**Path B — Explicit parameter needed**: Add `asGroup` to the send call:
```swift
func send(_ msgs: [ComposedMessage], live: Bool, ttl: Int?) async -> [ChatItem] {
    let asGroup = chat.chatInfo.groupInfo?.useRelays == true
    await apiSendMessages(
        type: chat.chatInfo.chatType,
        id: chat.chatInfo.apiId,
        scope: chat.chatInfo.groupChatScope(),
        live: live,
        ttl: ttl,
        asGroup: asGroup,  // NEW parameter if needed
        composedMessages: msgs
    )
}
```

### 4.4 Compose Bar Availability

For channel members who are observers (most members), the compose bar is disabled with italic "you are observer" text — the existing `userCantSendReason` logic in ChatTypes.swift:1580 handles this when `memberRole == .observer`. Only owners and admins see the active compose bar in channels.

**Verification**: The existing code checks `groupInfo.membership.memberRole == .observer` and returns `("you are observer", "Please contact group admin.")` which feeds into ComposeView's `disabledText` parameter. This works for channels automatically since channels are groups with observer members.

---

## 5. Wireframes

### 5.1 Primary Design — Owner Compose View

```
┌──────────────────────────────────────────┐
│  < [📢] SimpleX News               ...  │
│         Channel                          │
├──────────────────────────────────────────┤
│                                          │
│  [📢]  SimpleX News                     │
│  ┌──────────────────────────────────┐   │
│  │ Previous channel message...     │   │
│  │                       3:42 PM ✓ │   │
│  └──────────────────────────────────┘   │
│                                          │
│                                          │
│                                          │
├──────────────────────────────────────────┤
│  📢 Posting as SimpleX News              │
│  ┌──────────────────────────────┐  ┌──┐ │
│  │ Type a message...            │  │➤ │ │
│  └──────────────────────────────┘  └──┘ │
└──────────────────────────────────────────┘
```

### 5.2 Primary Design — Owner Composing with Attachments

```
├──────────────────────────────────────────┤
│  📢 Posting as SimpleX News              │
│  ┌──────────┐                            │
│  │ [image]  │ (attached photo)           │
│  └──────────┘                            │
│  ┌──────────────────────────────┐  ┌──┐ │
│  │ Caption for the image...    │  │➤ │ │
│  └──────────────────────────────┘  └──┘ │
└──────────────────────────────────────────┘
```

### 5.3 Primary Design — Owner Replying to Message

```
├──────────────────────────────────────────┤
│  📢 Posting as SimpleX News              │
│  ┌─ Replying to ────────────────────┐   │
│  │ Previous message text...         │   │
│  └──────────────────────────────────┘   │
│  ┌──────────────────────────────┐  ┌──┐ │
│  │ Reply text...                │  │➤ │ │
│  └──────────────────────────────┘  └──┘ │
└──────────────────────────────────────────┘
```

### 5.4 Alternative Design A — Inline Label in Compose Field

Instead of a separate banner above, show the channel name as placeholder prefix:

```
│  ┌──────────────────────────────┐  ┌──┐ │
│  │ 📢 SimpleX News: Type...    │  │➤ │ │
│  └──────────────────────────────┘  └──┘ │
```

### 5.5 Alternative Design B — No Visual Indicator

Since all messages from owners automatically appear as channel messages, perhaps no indicator is needed — the sent messages immediately show up with the channel avatar, confirming the behavior.

### 5.6 State Variations

**Admin posting** (not owner):
```
│  📢 Posting as SimpleX News              │
```
Same banner — admins post as the channel too.

**Member/observer** (disabled compose bar):
```
│  ┌──────────────────────────────┐  [+]  │
│  │ you are observer             │  [📷] │
│  └──────────────────────────────┘  [➤]  │
```
Disabled compose field with italic "you are observer" text (existing pattern from `userCantSendReason` in ChatTypes.swift:1580).

---

## 6. Design Rationale

**Banner above compose (Primary) > Inline placeholder (Alt A)**:
- Banner is always visible, even when typing
- Placeholder disappears when user starts typing — the context is lost
- Banner pattern matches existing quote/edit context display
- More prominent and clear

**Banner (Primary) > No indicator (Alt B)**:
- Users might not realize their messages will appear as "from channel"
- Explicit confirmation prevents confusion
- Low implementation cost for significant UX clarity
- Especially important for new channel owners unfamiliar with the feature

**Admin + Owner can post**:
- Channels benefit from multiple posters (admins help manage content)
- All admin posts appear as "from channel" — consistent for subscribers
- Match launch plan §2.7 which mentions admin capability

---

## 7. Edge Cases

1. **Owner with no active relays**: Owner can still compose, but messages may not be delivered until relays are active. Show warning or disable compose if no relays active? For MVP, allow composing — messages queue and deliver when relays connect.

2. **Owner incognito**: Channels don't support incognito creation. If somehow incognito, the channel posting banner still shows channel name (not the owner's real name).

3. **Live messages**: Channel owners can send live messages (typing visible in real-time). The live message indicator should also show the channel context.

4. **File/media attachments**: Attachments work the same way. The banner stays visible above the attachment preview.

5. **Voice messages**: Same as text — banner visible during recording.

6. **Multiple owners** (post-MVP): All owners see the same banner and all post as channel.

7. **Owner demoted to admin**: If role changes while ChatView is open, the banner should still appear (admins can post too). If demoted below admin, banner disappears and compose bar shows disabled "you are observer" text.

---

## 8. Testing Notes

1. **Banner visibility**: Owner opens channel → verify "Posting as [Channel]" banner appears above compose field
2. **Admin visibility**: Admin opens channel → verify same banner appears
3. **Observer compose**: Regular member opens channel → verify disabled compose bar with "you are observer" text
4. **Send as channel**: Owner sends message → verify the message appears with channel avatar/name (CIChannelRcv rendering)
5. **Banner persistence**: Type text, attach file → verify banner stays visible
6. **Reply context**: Reply to a message → verify banner stays above the reply context
7. **No banner for groups**: Open regular group → verify no "Posting as" banner
8. **Role change**: Mock role change from owner to observer while in chat → verify compose bar disappears
