# 03 — Owner Compose & Subscriber Label

## TOC
1. [Summary](#summary)
2. [Context](#context)
3. [Changes](#changes) — 3.1 sendAsGroup · 3.2 Broadcast placeholder · 3.3 Subscriber label · 3.4 Share Extension · 3.5 Forward messages
4. [Verification](#verification)

## Summary
Five changes: (1) owner/admin sends with `sendAsGroup: true`, (2) compose placeholder shows "Broadcast" for owner/admin, (3) subscribers see "you are subscriber" instead of compose field, (4) Share Extension `sendAsGroup`, (5) forward messages `sendAsGroup`. Implements product plan §2.2 (`plans/chat-relays-ui/ios-channels-product-plan.md`).

## Context
- `apiSendMessages` already has `sendAsGroup: Bool = false` parameter (SimpleXAPI.swift:527).
- `ComposeView.send()` at line 1301 calls `apiSendMessages` without passing `sendAsGroup`.
- `apiForwardChatItems` already has `sendAsGroup: Bool = false` parameter (SimpleXAPI.swift:494).
- `ComposeView.forwardItems()` at line 1328 calls `apiForwardChatItems` without passing `sendAsGroup`.
- `SendMessageView` receives `placeholder` from `ComposeView.sendMessageView()` (line 682).
- `userCantSendReason` (ChatTypes.swift:1564) returns compose-disabling label/alert tuples.
- `ShareAPI.swift:71` has explicit `TODO [relays]` comment for `sendAsGroup`.

## Changes

### 1. Pass `sendAsGroup: true` for channel owner/admin

**File:** `apps/ios/Shared/Views/Chat/ComposeMessage/ComposeView.swift`
**Location:** `send()` function, lines 1301–1326

**Current code (lines 1304–1311):**
```swift
: await apiSendMessages(
    type: chat.chatInfo.chatType,
    id: chat.chatInfo.apiId,
    scope: chat.chatInfo.groupChatScope(),
    live: live,
    ttl: ttl,
    composedMessages: msgs
) {
```

Note: the call is the right side of a ternary inside an `if let`, ending with `) {` on line 1311.

**New code:**
```swift
: await apiSendMessages(
    type: chat.chatInfo.chatType,
    id: chat.chatInfo.apiId,
    scope: chat.chatInfo.groupChatScope(),
    sendAsGroup: chat.chatInfo.groupInfo?.useRelays == true && chat.chatInfo.groupInfo?.membership.memberRole >= .admin,
    live: live,
    ttl: ttl,
    composedMessages: msgs
) {
```

The `sendAsGroup` parameter is inserted AFTER `scope` and BEFORE `live`, matching the `apiSendMessages` function signature (SimpleXAPI.swift:527):
```swift
func apiSendMessages(type: ChatType, id: Int64, scope: GroupChatScope?, sendAsGroup: Bool = false, live: Bool = false, ttl: Int? = nil, composedMessages: [ComposedMessage]) async -> [ChatItem]?
```

### 2. "Broadcast" placeholder for channel owner/admin

**File:** `apps/ios/Shared/Views/Chat/ComposeMessage/ComposeView.swift`
**Location:** `sendMessageView()` call site, line 448

**Current code (line 448):**
```swift
sendMessageView(disableSendButton)
```

**New code:**
```swift
sendMessageView(
    disableSendButton,
    placeholder: chat.chatInfo.groupInfo.map { gi in
        gi.useRelays && gi.membership.memberRole >= .admin
            ? NSLocalizedString("Broadcast", comment: "compose placeholder for channel owner")
            : nil
    } ?? nil
)
```

The `placeholder` parameter defaults to `nil` and falls through to `composeState.placeholder` ("Message") inside `SendMessageView` (SendMessageView.swift:68).

**Note on types:** `chat.chatInfo.groupInfo` is `GroupInfo?`. The `.map { }` returns `String??` (closure returns `String?`). The `?? nil` collapses to `String?`. This compiles and is the idiomatic optional-map pattern used elsewhere in the codebase.

### 3. "you are subscriber" label for channel subscribers

**File:** `apps/ios/SimpleXChat/ChatTypes.swift`
**Location:** `userCantSendReason` computed property, inside `.group` case, `.none` scope branch, lines 1578–1581

**Current code:**
```swift
case .none:
    if groupInfo.membership.memberPending { return ("reviewed by admins", "Please contact group admin.") }
    if groupInfo.membership.memberRole == .observer { return ("you are observer", "Please contact group admin.") }
    return nil
```

**New code:** Insert `useRelays` check BEFORE the `memberPending` check:

```swift
case .none:
    if groupInfo.useRelays && groupInfo.membership.memberRole < .admin { return ("you are subscriber", nil) }
    if groupInfo.membership.memberPending { return ("reviewed by admins", "Please contact group admin.") }
    if groupInfo.membership.memberRole == .observer { return ("you are observer", "Please contact group admin.") }
    return nil
```

**Why `< .admin` not `== .observer`:** In channels, all non-admin/owner members are subscribers and cannot send. The `.observer` role check alone wouldn't cover `.member` role subscribers. Using `< .admin` covers all subscriber roles (including `.moderator` if it exists in channels).

**Priority over `memberPending`:** This check is placed BEFORE `memberPending` intentionally. In channels, pending members are still subscribers — "you are subscriber" is the correct label regardless of pending state.

**Why `alertMessage: nil`:** Tapping the disabled compose area shows an alert (ChatView.swift:154–157). When `alertMessage` is `nil`, `showAlertMsg` shows the title "You can't send messages!" with no body text. This is acceptable for channel subscribers.

### 4. Share Extension — `sendAsGroup: true`

**File:** `apps/ios/SimpleX SE/ShareAPI.swift`
**Location:** Line 71

**Current code:**
```swift
sendAsGroup: false, // TODO [relays] pass sendAsGroup=true for channel owner
```

**New code:**
```swift
sendAsGroup: chatInfo.groupInfo?.useRelays == true && chatInfo.groupInfo?.membership.memberRole >= .admin,
```

### 5. Forward messages — `sendAsGroup: true`

**File:** `apps/ios/Shared/Views/Chat/ComposeMessage/ComposeView.swift`
**Location:** `forwardItems()` function, lines 1328–1338

**Current code (lines 1329–1338):**
```swift
if let chatItems = await apiForwardChatItems(
    toChatType: chat.chatInfo.chatType,
    toChatId: chat.chatInfo.apiId,
    toScope: chat.chatInfo.groupChatScope(),
    fromChatType: fromChatInfo.chatType,
    fromChatId: fromChatInfo.apiId,
    fromScope: fromChatInfo.groupChatScope(),
    itemIds: forwardedItems.map { $0.id },
    ttl: ttl
) {
```

**New code:** Insert `sendAsGroup` parameter after `toScope`, matching the `apiForwardChatItems` signature (SimpleXAPI.swift:494):

```swift
if let chatItems = await apiForwardChatItems(
    toChatType: chat.chatInfo.chatType,
    toChatId: chat.chatInfo.apiId,
    toScope: chat.chatInfo.groupChatScope(),
    sendAsGroup: chat.chatInfo.groupInfo?.useRelays == true && chat.chatInfo.groupInfo?.membership.memberRole >= .admin,
    fromChatType: fromChatInfo.chatType,
    fromChatId: fromChatInfo.apiId,
    fromScope: fromChatInfo.groupChatScope(),
    itemIds: forwardedItems.map { $0.id },
    ttl: ttl
) {
```

## Verification
- Build succeeds.
- Channel owner sees "Broadcast" placeholder in compose field.
- Channel subscriber sees "you are subscriber" label over disabled compose.
- Owner's sent messages arrive with `showGroupAsSender = true` (rendered as channel messages by ChatView).
- Share Extension sends with `sendAsGroup: true` for channel owner.
- Forwarded messages to channels sent with `sendAsGroup: true`.
- Regular groups are unaffected (no `useRelays`).
