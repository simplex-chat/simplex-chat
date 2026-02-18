# 02 — Channel Message Rendering

## TOC
1. [Summary](#summary)
2. [Context](#context)
3. [Changes](#changes) — 3.1 chatItemListView · 3.2 shouldShowAvatar · 3.3 showMemberImage · 3.4 getItemSeparation · 3.5 memberToModerate · 3.6 Switch audit
4. [Verification](#verification)

## Summary
Handle `CIDirection.channelRcv` in ChatView so channel messages render with the group avatar + group name as sender (same visual as `showGroupAsSender` path for `.groupRcv`). Inline the layout in the `.channelRcv` branch — no helper extraction. Implements product plan §2.2 (`plans/chat-relays-ui/ios-channels-product-plan.md`).

## Context
- `CIDirection.channelRcv` (ChatTypes.swift:3464) — exists in type system, no associated `GroupMember`.
- `ChatItemsMerger.swift` already handles `.channelRcv` (no change needed there).
- `ChatPreviewView.swift` — channel messages with `showGroupAsSender=true` already suppress sender prefix (no change needed).
- The `.groupRcv` rendering path in ChatView already has `showGroupAsSender` logic that shows group profile image + group name. Channel messages should always use this visual.
- `.channelRcv.sent` is `false` (ChatTypes.swift:3475).
- `chatItemListView` (ChatView.swift:1824) is inside `ChatItemWithMenu` struct, which has both `let chatItem: ChatItem` (struct property, line 1594) and `_ ci: ChatItem` (function parameter). Existing code uses `chatItem.chatDir.sent` for alignment (line 1872).

## Changes

### 1. `chatItemListView` — add `.channelRcv` branch

**File:** `apps/ios/Shared/Views/Chat/ChatView.swift`
**Location:** `chatItemListView`, line 1831

**Current code (line 1831):**
```swift
if case let .groupRcv(member) = ci.chatDir,
   case let .group(groupInfo, _) = chat.chatInfo {
```

This `if case` only matches `.groupRcv`. The `.channelRcv` case falls through to the generic `else` branch (line 1928) which renders without avatar/sender.

**Change:** Add `.channelRcv` branch BEFORE the existing `.groupRcv` branch. Change `if` to `else if` on the existing `.groupRcv` line. The `.groupRcv` path stays entirely unchanged.

Layout mirrors the `.groupRcv showGroupAsSender` path (lines 1843–1907) but simplified: no `prevMember`/`memCount` logic, always group avatar, `"channel"` role. No `showGroupAsSender` check — `.channelRcv` always renders as group-as-sender by design. Layout duplication acceptable per CODE.md (semantic types warrant separate pattern arms).

**Insert BEFORE line 1831**, and change `if` on line 1831 to `} else if`:

```swift
if case .channelRcv = ci.chatDir,
   case let .group(groupInfo, _) = chat.chatInfo {
    if showAvatar {
        VStack(alignment: .leading, spacing: 4) {
            if ci.content.showMemberName {
                Group {
                    Group {
                        if #available(iOS 16.0, *) {
                            MemberLayout(spacing: 16, msgWidth: msgWidth) {
                                Text(groupInfo.chatViewName)
                                    .lineLimit(1)
                                Text(NSLocalizedString("channel", comment: "shown as sender role for channel messages"))
                                    .fontWeight(.semibold)
                                    .lineLimit(1)
                                    .padding(.trailing, 8)
                            }
                        } else {
                            HStack(spacing: 16) {
                                Text(groupInfo.chatViewName)
                                    .lineLimit(1)
                                Text(NSLocalizedString("channel", comment: "shown as sender role for channel messages"))
                                    .fontWeight(.semibold)
                                    .lineLimit(1)
                                    .layoutPriority(1)
                            }
                        }
                    }
                    .frame(
                        maxWidth: maxWidth,
                        alignment: chatItem.chatDir.sent ? .trailing : .leading
                    )
                }
                .font(.caption)
                .foregroundStyle(.secondary)
                .padding(.leading, memberImageSize + 14 + (selectedChatItems != nil && ci.canBeDeletedForSelf ? 12 + 24 : 0))
                .padding(.top, 3)
            }
            HStack(alignment: .center, spacing: 0) {
                if selectedChatItems != nil && ci.canBeDeletedForSelf {
                    SelectedChatItem(ciId: ci.id, selectedChatItems: $selectedChatItems)
                        .padding(.trailing, 12)
                }
                HStack(alignment: .top, spacing: 10) {
                    ProfileImage(imageStr: groupInfo.image, iconName: groupInfo.chatIconName, size: memberImageSize, backgroundColor: theme.colors.background)
                        .simultaneousGesture(TapGesture().onEnded {
                            showChatInfoSheet = true
                        })
                    chatItemWithMenu(ci, range, maxWidth, itemSeparation)
                        .onPreferenceChange(DetermineWidth.Key.self) { msgWidth = $0 }
                }
            }
        }
        .padding(.bottom, bottomPadding)
        .padding(.trailing)
        .padding(.leading, 12)
    } else {
        HStack(alignment: .center, spacing: 0) {
            if selectedChatItems != nil && ci.canBeDeletedForSelf {
                SelectedChatItem(ciId: ci.id, selectedChatItems: $selectedChatItems)
                    .padding(.leading, 12)
            }
            chatItemWithMenu(ci, range, maxWidth, itemSeparation)
                .padding(.trailing)
                .padding(.leading, 10 + memberImageSize + 12)
        }
        .padding(.bottom, bottomPadding)
    }
} else if case let .groupRcv(member) = ci.chatDir,
```

**The existing `.groupRcv` path (lines 1832–1927) stays entirely unchanged.**

### 2. `shouldShowAvatar` — ChatView.swift:1653

**Current code (lines 1653–1669):**
```swift
func shouldShowAvatar(_ current: ChatItem, _ older: ChatItem?) -> Bool {
    let oldIsGroupRcv = switch older?.chatDir {
    case .groupRcv: true
    default: false
    }
    let sameMember = switch (older?.chatDir, current.chatDir) {
    case (.groupRcv(let oldMember), .groupRcv(let member)):
        oldMember.memberId == member.memberId
    default:
        false
    }
    if case .groupRcv = current.chatDir, (older == nil || (!oldIsGroupRcv || !sameMember)) {
        return true
    } else {
        return false
    }
}
```

**Change:** Add `.channelRcv` handling. Channel messages always show avatar for the first in a consecutive sequence (group avatar). Two consecutive `.channelRcv` items are "same sender".

```swift
func shouldShowAvatar(_ current: ChatItem, _ older: ChatItem?) -> Bool {
    let oldIsGroupRcv = switch older?.chatDir {
    case .groupRcv: true
    case .channelRcv: true
    default: false
    }
    let sameMember = switch (older?.chatDir, current.chatDir) {
    case (.groupRcv(let oldMember), .groupRcv(let member)):
        oldMember.memberId == member.memberId
    case (.channelRcv, .channelRcv):
        true
    default:
        false
    }
    if case .groupRcv = current.chatDir, (older == nil || (!oldIsGroupRcv || !sameMember)) {
        return true
    } else if case .channelRcv = current.chatDir, (older == nil || (!oldIsGroupRcv || !sameMember)) {
        return true
    } else {
        return false
    }
}
```

### 3. `showMemberImage` — ChatView.swift:2027

**Current code (lines 2027–2033):**
```swift
private func showMemberImage(_ member: GroupMember, _ prevItem: ChatItem?) -> Bool {
    switch (prevItem?.chatDir) {
    case .groupSnd: return true
    case let .groupRcv(prevMember): return prevMember.groupMemberId != member.groupMemberId
    default: return false
    }
}
```

**Note:** This function is only called from the `.groupRcv` rendering path. The `.channelRcv` path does NOT call it. However, if `prevItem` has `.channelRcv` direction, the `default` branch returns `false`, which is wrong — it should return `true` (different sender type means show image). Add a case:

```swift
private func showMemberImage(_ member: GroupMember, _ prevItem: ChatItem?) -> Bool {
    switch (prevItem?.chatDir) {
    case .groupSnd: return true
    case let .groupRcv(prevMember): return prevMember.groupMemberId != member.groupMemberId
    case .channelRcv: return true
    default: return false
    }
}
```

### 4. `getItemSeparation` — ChatView.swift:1634

**Current code (line 1639, single line):**
```swift
let sameMemberAndDirection = if case .groupRcv(let prevGroupMember) = prevItem.chatDir, case .groupRcv(let groupMember) = chatItem.chatDir {
    groupMember.groupMemberId == prevGroupMember.groupMemberId
} else {
    chatItem.chatDir.sent == prevItem.chatDir.sent
}
```

**Change:** Add `.channelRcv` consecutive case. Insert `else if` between the `groupRcv` block and the `else` block:

```swift
let sameMemberAndDirection = if case .groupRcv(let prevGroupMember) = prevItem.chatDir, case .groupRcv(let groupMember) = chatItem.chatDir {
    groupMember.groupMemberId == prevGroupMember.groupMemberId
} else if case .channelRcv = chatItem.chatDir, case .channelRcv = prevItem.chatDir {
    true
} else {
    chatItem.chatDir.sent == prevItem.chatDir.sent
}
```

### 5. Exhaustive switch audit — `memberToModerate`

**File:** `apps/ios/SimpleXChat/ChatTypes.swift`
**Location:** `memberToModerate` function, lines 3234–3246

**Current code:**
```swift
public func memberToModerate(_ chatInfo: ChatInfo) -> (GroupInfo, GroupMember?)? {
    switch (chatInfo, chatDir) {
    case let (.group(groupInfo, _), .groupRcv(groupMember)):
        ...
    case let (.group(groupInfo, _), .groupSnd):
        ...
    default: return nil
    }
}
```

**Change:** Add `.channelRcv` case — channel messages cannot be moderated per-member (no member identity), so return `nil`. Insert before `default: return nil`:

```swift
case (.group, .channelRcv):
    return nil
```

### 6. Other switch statements — comprehensive audit

Audited all iOS files with `switch chatDir` or `case .groupRcv`: ChatItemView, ComposeView, ChatItemInfoView, ChatItemsMerger, CIRcvDecryptionError, CIMemberCreatedContactView — all safe via `default:` catch-all or explicit `.channelRcv` handling. No additional changes needed.

## Verification
- Build succeeds.
- Channel messages (`.channelRcv`) show group avatar + "channel name / channel" sender label.
- Consecutive `.channelRcv` items group without repeating avatar.
- `.groupRcv` messages still render correctly (no regression).
- `memberToModerate` returns `nil` for `.channelRcv`.
