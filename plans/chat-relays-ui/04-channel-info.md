# 04 — Channel Info

## TOC
1. [Summary](#summary)
2. [Prerequisites](#prerequisites)
3. [Context](#context)
4. [Section order](#section-order)
5. [Changes](#changes) — 5.1–5.8 Structural changes · 5.9 Label replacements · 5.10 ChannelMembersView · 5.11 ChannelRelaysView · 5.12 RelayStatus.text
6. [Verification](#verification)

## Summary
Modify `GroupChatInfoView.swift` for `useRelays == true`: channel-specific labels, section structure, new `ChannelMembersView` and `ChannelRelaysView` sub-views. Add `RelayStatus.text` shared helper to ChatTypes.swift. Implements product plan §2.4 (`plans/chat-relays-ui/ios-channels-product-plan.md`).

## Prerequisites
Before starting, ask the user to create these empty files in Xcode (right-click target folder → New File → Swift File):
- `apps/ios/Shared/Views/Chat/Group/ChannelMembersView.swift`
- `apps/ios/Shared/Views/Chat/Group/ChannelRelaysView.swift`

This auto-registers them in `SimpleX.xcodeproj/project.pbxproj`.

## Context
- Product plan: `plans/chat-relays-ui/ios-channels-product-plan.md` §2.4.
- `GroupChatInfoView.swift` — full group info page. Sections in `body` (lines 67–200).
- Existing conditional pattern: `groupInfo.businessChat == nil ? "group label" : "business label"` — extend with `useRelays` checks.
- `RelayStatus` (ChatTypes.swift:2497): `rsNew`, `rsInvited`, `rsAccepted`, `rsActive`.
- `GroupRelay` (ChatTypes.swift:2504): `groupRelayId`, `groupMemberId`, `userChatRelayId`, `relayStatus: RelayStatus`, `relayLink: String?`. Hostname parsed from `relayLink` simplex:// URI.

## Section Order (from product plan wireframes)

**Owner view:**
1. Channel link / Owners & subscribers
2. Edit channel profile / Welcome message
3. Chat theme / Delete messages after
4. Chat relays / Clear chat / Delete channel

**Subscriber view:**
1. Channel link / Owners
2. Welcome message
3. Chat theme / Delete messages after
4. Chat relays / Clear chat / Leave channel

## Changes

### 1. Section 1 — Link + Members (lines 91–107)

**Replace** section content. When `useRelays`, show only `groupLinkButton()` + `channelMembersButton()` (no support/reports — broadcast model):

```swift
Section {
    if groupInfo.useRelays {
        groupLinkButton()
        channelMembersButton()
    } else {
        // existing code unchanged (lines 92–106)
    }
} header: {
    Text("")
}
```

**Add `channelMembersButton()`** near `groupLinkButton` (~line 534):

```swift
private func channelMembersButton() -> some View {
    let label: LocalizedStringKey = groupInfo.membership.memberRole >= .admin
        ? "Owners & subscribers"
        : "Owners"
    return NavigationLink {
        ChannelMembersView(chat: chat, groupInfo: groupInfo)
            .navigationTitle(label)
            .modifier(ThemedBackground(grouped: true))
            .navigationBarTitleDisplayMode(.large)
    } label: {
        Label(label, systemImage: "person.2")
    }
}
```

### 2. `groupLinkButton()` — Conditional labels (line 534)

Add `useRelays` ternary to both labels:
- `groupInfo.useRelays ? "Create channel link" : "Create group link"` (when `groupLink == nil`)
- `groupInfo.useRelays ? "Channel link" : "Group link"` (when link exists)

### 3. `groupLinkDestinationView()` — Nav title (line 554)

Change to: `.navigationBarTitle(groupInfo.useRelays ? "Channel link" : "Group link")`

### 4. Section 2 — Profile + Welcome (lines 109–125)

When `useRelays`: hide `GroupPreferencesButton`, drop `businessChat` guard (channels have no businessChat), suppress footer:

```swift
Section {
    if groupInfo.useRelays {
        if groupInfo.isOwner { editGroupButton() }
        if groupInfo.groupProfile.description != nil || groupInfo.isOwner {
            addOrEditWelcomeMessage()
        }
    } else {
        // existing code unchanged (lines 110–123)
    }
} footer: {
    if !groupInfo.useRelays {
        // existing footer unchanged
    }
}
```

For channel subscribers with no welcome message: no rows → SwiftUI renders empty section as invisible (correct).

### 5. `editGroupButton()` — Label (line 653)

Change to: `Label(groupInfo.useRelays ? "Edit channel profile" : "Edit group profile", systemImage: "pencil")`

### 6. Section 3 — Theme + TTL (lines 127–141)

Wrap send receipts in `if !groupInfo.useRelays { ... }` (channels are broadcast, no receipts). Theme + TTL unchanged.

### 7. Section 4 — Members list (lines 143–172)

Hide inline members list for channels: `if !groupInfo.nextConnectPrepared && !groupInfo.useRelays {`.

### 8. Section 5 — Actions (lines 174–182)

Insert `channelRelaysButton()` at top when `useRelays`. Hide "Leave" for sole channel owner (use "Delete" instead):

```swift
if !groupInfo.useRelays || !groupInfo.isOwner
    || members.filter({ $0.wrapped.memberRole == .owner && $0.wrapped.groupMemberId != groupInfo.membership.groupMemberId }).count > 0 {
    leaveGroupButton()
}
```

**Why exclude current user:** `chatModel.groupMembers` includes the current user. Without the `groupMemberId` filter, the count would always be `> 0` for owners, defeating the guard.

**Add `channelRelaysButton()`:**

```swift
private func channelRelaysButton() -> some View {
    NavigationLink {
        ChannelRelaysView(groupInfo: groupInfo)
            .navigationTitle("Chat relays")
            .modifier(ThemedBackground(grouped: true))
            .navigationBarTitleDisplayMode(.large)
    } label: {
        Label("Chat relays", systemImage: "server.rack")
    }
}
```

### 9. Label replacements — "group" → "channel"

All follow the same pattern: prepend `groupInfo.useRelays ? "Channel text" :` before the existing `businessChat` ternary.

| Function (line) | Current label expression | Channel label |
|---|---|---|
| `deleteGroupButton` (675) | `businessChat == nil ? "Delete group" : "Delete chat"` | `"Delete channel"` |
| `deleteGroupAlert` (705) | `businessChat == nil ? "Delete group?" : "Delete chat?"` | `"Delete channel?"` |
| `leaveGroupButton` (694) | `businessChat == nil ? "Leave group" : "Leave chat"` | `"Leave channel"` |
| `leaveGroupAlert` title (742) | `businessChat == nil ? "Leave group?" : "Leave chat?"` | `"Leave channel?"` |
| `leaveGroupAlert` message (743) | `"You will stop receiving messages from this group/chat. Chat history will be preserved."` | `"You will stop receiving messages from this channel. Chat history will be preserved."` |
| `showRemoveMemberAlert` (795) | `"Member will be removed from group/chat - this cannot be undone!"` | `"Member will be removed from channel - this cannot be undone!"` |

**Note on `showRemoveMemberAlert`:** This function (line 793) uses `NSLocalizedString(...)` pattern (not `LocalizedStringKey`). Apply the ternary inside `NSLocalizedString`: `groupInfo.useRelays ? NSLocalizedString("Member will be removed from channel - this cannot be undone!", comment: "alert message") : existing_ternary`.

**Exception — `deleteGroupAlertMessage` (line 838):** More complex (returns `Text`, two branches for `memberCurrent`):

```swift
func deleteGroupAlertMessage(_ groupInfo: GroupInfo) -> Text {
    groupInfo.useRelays ? (
        groupInfo.membership.memberCurrent
            ? Text("Channel will be deleted for all subscribers - this cannot be undone!")
            : Text("Channel will be deleted for you - this cannot be undone!")
    ) : groupInfo.businessChat == nil ? (
        groupInfo.membership.memberCurrent
            ? Text("Group will be deleted for all members - this cannot be undone!")
            : Text("Group will be deleted for you - this cannot be undone!")
    ) : (
        groupInfo.membership.memberCurrent
            ? Text("Chat will be deleted for all members - this cannot be undone!")
            : Text("Chat will be deleted for you - this cannot be undone!")
    )
}
```

### 10. NEW FILE: `ChannelMembersView.swift`

**Path:** `apps/ios/Shared/Views/Chat/Group/ChannelMembersView.swift`

**Properties:** `chat: Chat`, `groupInfo: GroupInfo`, `@EnvironmentObject var chatModel: ChatModel`, `@EnvironmentObject var theme: AppTheme`, `@State private var scrollToItemId: ChatItem.ID? = nil`.

**Body:** `List` with two sections:
- **"Owners" section:** If current user is owner, show `memberRow(groupInfo.membership, user: true)`. Then `ForEach(owners)` with `memberRow(member.wrapped, user: false)`.
- **Subscribers section** (only if `memberRole >= .admin`): header `"\(subscribers.count) subscribers"`. `ForEach(subscribers)` with member rows.

**Filtering:** `allMembers = chatModel.groupMembers.filter { s != .memLeft && s != .memRemoved }`. `owners = .filter { memberRole >= .owner }`. `subscribers = .filter { memberRole < .owner }`.

**`memberRow(_ gMember: GMember, user: Bool)`:** Takes `GMember` (not `GroupMember`) because `GroupMemberInfoView` requires `GMember` as `@ObservedObject`. Extract `let member = gMember.wrapped` for display properties. Layout: `HStack` with `MemberProfileImage(member, size: 38)`, VStack of display name + connection status. Non-user rows wrapped in `NavigationLink` to `GroupMemberInfoView(groupInfo: groupInfo, chat: chat, groupMember: gMember, scrollToItemId: $scrollToItemId)`.

**For the user row:** Call `memberRow(GMember(groupInfo.membership), user: true)` — wrap the `GroupMember` in `GMember()` for consistent typing.

**Inline verified shield** (mirrors private `MemberRowView.memberVerifiedShield`):
```swift
member.verified
    ? (Text(Image(systemName: "checkmark.shield")) + textSpace)
        .font(.caption).baselineOffset(2).kerning(-2)
        .foregroundColor(theme.colors.secondary) + nameText
    : nameText
```

**Inline connection status** (mirrors private `MemberRowView.memberConnStatus`): user → `"you"`, `connDisabled` → `"disabled"`, `connInactive` → `"inactive"`, else → `member.memberStatus.shortText`.

**Role text:** `role == .member ? "" : role.text`, `.font(.caption)`, `.foregroundColor(theme.colors.secondary)`.

**Note:** `scrollToItemId` is required by `GroupMemberInfoView` — managed as `@State` here, unused in channel context but must be passed. `MemberProfileImage` and `GroupMemberInfoView` are public. `MemberRowView` internals are `private`, hence inline duplication.

### 11. NEW FILE: `ChannelRelaysView.swift`

**Path:** `apps/ios/Shared/Views/Chat/Group/ChannelRelaysView.swift`

**Properties:** `groupInfo: GroupInfo`, `@State private var groupRelays: [GroupRelay] = []`.

**Body:** `List` with one `Section`. `ForEach(groupRelays)` with `HStack { Text(relayDisplayName(relay)); Spacer(); relayStatusIndicator(relay.relayStatus) }`. Footer: `"Chat relays forward messages to channel subscribers."`. Empty state: `"No relay data available"`.

**`loadRelays()`** called `.onAppear` — stub body: `func loadRelays() { /* TODO: call apiGetGroupRelays(groupId: groupInfo.groupId) when backend implements it */ }`.

**Status by role:** Owner sees `RelayStatus` via `relay.relayStatus.text`. Subscriber status TBD — for MVP, uses `RelayStatus.text` for both.

**Helpers:**
- `relayDisplayName`: `hostFromRelayLink(relay.relayLink)` if available, else `"relay\(relay.groupRelayId)"`.
- `relayStatusIndicator`: colored `Circle` (green=active, red=new, orange=other) + `status.text` caption.

**`hostFromRelayLink` — file-scope shared helper** (non-obvious parsing):

```swift
func hostFromRelayLink(_ link: String) -> String {
    guard let atIdx = link.firstIndex(of: "@") else { return link }
    let afterAt = link[link.index(after: atIdx)...]
    if let colonIdx = afterAt.firstIndex(of: ":") {
        return String(afterAt[afterAt.startIndex..<colonIdx])
    }
    return String(afterAt)
}
```

### 12. `RelayStatus.text` — shared computed property

**File:** `apps/ios/SimpleXChat/ChatTypes.swift`
**Location:** After `RelayStatus` enum (~line 2503):

```swift
extension RelayStatus {
    public var text: LocalizedStringKey {
        switch self {
        case .rsNew: "New"
        case .rsInvited: "Connecting"
        case .rsAccepted: "Accepted"
        case .rsActive: "Active"
        }
    }
}
```

Similar to `GroupMemberRole.text` (ChatTypes.swift:2756), which returns `String` via `NSLocalizedString`. This version uses `LocalizedStringKey` string literals since it's only used in SwiftUI `Text()` views.

## Verification
- Build succeeds.
- Channel owner info: sections in product plan order (link+owners&subscribers / profile+welcome / theme+TTL / relays+clear+delete).
- Channel subscriber info: link+owners / welcome (if exists) / theme+TTL / relays+clear+leave.
- All "group" labels replaced with "channel" when `useRelays`.
- Regular groups and business chats unaffected.
- Inline members list hidden for channels (ChannelMembersView sub-view instead).
- Send receipts hidden for channels.
- `RelayStatus.text` returns localized labels for all 4 cases.
- Relay hostnames parsed from `relayLink` URI via `hostFromRelayLink`.
