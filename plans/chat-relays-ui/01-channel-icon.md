# 01 — Channel Icon

## TOC
1. [Summary](#summary)
2. [Context](#context)
3. [Change](#change)
4. [Verification](#verification)

## Summary
Replace the default group icon with `antenna.radiowaves.left.and.right` for channels (`groupInfo.useRelays == true`). One code change in one file. Implements product plan §2.1 (`plans/chat-relays-ui/ios-channels-product-plan.md`).

## Context
- **Discriminator:** `groupInfo.useRelays: Bool` (ChatTypes.swift:2338)
- Channel = group with `useRelays = true`. No new types needed.
- The icon is used everywhere via `GroupInfo.chatIconName` computed property.

## Change

**File:** `apps/ios/SimpleXChat/ChatTypes.swift`
**Location:** `chatIconName` computed property, lines 2382–2388

**Current code:**
```swift
public var chatIconName: String {
    switch businessChat?.chatType {
    case .none: "person.2.circle.fill"
    case .business: "briefcase.circle.fill"
    case .customer: "person.crop.circle.fill"
    }
}
```

**New code:**
```swift
public var chatIconName: String {
    if useRelays { return "antenna.radiowaves.left.and.right" }
    switch businessChat?.chatType {
    case .none: "person.2.circle.fill"
    case .business: "briefcase.circle.fill"
    case .customer: "person.crop.circle.fill"
    }
}
```

**Why before the switch:** The `useRelays` check must come BEFORE `businessChat` to ensure channels always get the channel icon regardless of any future `businessChat` state.

## Verification
- Build succeeds (`apps/ios/SimpleX.xcodeproj`).
- No other files need changes — `chatIconName` is the single source for group icon throughout chat list, chat header toolbar, chat info header, and message avatars.
