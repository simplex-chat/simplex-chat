# 07 — Join Channel

## TOC
1. [Summary](#summary)
2. [Prerequisites](#prerequisites)
3. [Context](#context)
3. [Changes](#changes) — 3.1 planAndConnect detection · 3.2 prepareAndOpenChannel · 3.3 ChatBannerView relay info · 3.4 ComposeView "Join channel" · 3.5 Connection progress · 3.6 Relay data storage
4. [Verification](#verification)

## Summary
Detect channel links in `planAndConnect`, prepare the channel and navigate to ChatView (no popup alert). Show relay info in ChatBannerView pre-join. After join, display relay connection progress. Implements product plan §2.6 (`plans/chat-relays-ui/ios-channels-product-plan.md`).

## Prerequisites
- **Plan 04 must be completed first** — provides `channelRelayHostnames` storage pattern concept.
- **Plan 06 must be completed first** — provides `.groupLinkRelaysUpdated` notification name (in `ViewModifiers.swift`) and `processReceivedMsg` handler (in `SimpleXAPI.swift`).

## Context
- Product plan: `plans/chat-relays-ui/ios-channels-product-plan.md` §2.6.
- `planAndConnect` — join flow dispatcher in `NewChatView.swift`. `GroupLinkPlan.ok` case at line 1332.
- `GroupShortLinkInfo` (AppAPITypes.swift:1339) — `direct: Bool`, `groupRelays: [String]`, `sharedGroupId: String?`.
- Channel detection: non-empty `groupSLinkInfo_.groupRelays`.
- `apiPrepareGroup` (SimpleXAPI.swift:1112) → `ChatData`. `apiConnectPreparedGroup` (SimpleXAPI.swift:1138).
- `showPrepareGroupAlert` (NewChatView.swift:1073–1106) — popup alert. Channels skip this.
- `ChatBannerView` (ChatView.swift:911–1001) — avatar+name+context. `chatContext` (line 1003–1043) returns `"Tap Join group"` for prepared groups. **Does not currently have `@EnvironmentObject var chatModel: ChatModel` — must be added.**
- `connectButtonView` (ComposeView.swift:646–659). Used at line 414.
- `connectingText` (ChatView.swift:1046–1064) — "connecting…" for `memAccepted`.
- `PreparedGroup` (ChatTypes.swift:2406–2410) — **no relay fields**.
- `groupLinkRelaysUpdated` notification (ViewModifiers.swift).
- `hostFromRelayLink` (ChannelRelaysView.swift), `RelayStatus.text` (ChatTypes.swift).

## Changes

### 1. Channel detection in `planAndConnect`

**File:** `apps/ios/Shared/Views/NewChat/NewChatView.swift`
**Location:** `GroupLinkPlan.ok` case, line 1332

Add `isChannel` check. Wrap existing code (lines 1333–1355) in `else` block. New channel branch first.

**Important rename:** The existing code uses `_groupSLinkInfo_` (leading underscore, since the value was unused). Rename to `groupSLinkInfo_` (no leading underscore) because the channel branch now uses it:

```swift
case let .ok(groupSLinkInfo_, groupSLinkData_):  // was _groupSLinkInfo_
    let isChannel = !(groupSLinkInfo_?.groupRelays ?? []).isEmpty
    if isChannel {
        if let groupSLinkData = groupSLinkData_ {
            logger.debug("planAndConnect, .groupLink, .ok, channel link with short link data")
            await prepareAndOpenChannel(
                connectionLink: connectionLink,
                groupShortLinkData: groupSLinkData,
                groupShortLinkInfo: groupSLinkInfo_,
                dismiss: dismiss,
                cleanup: cleanup
            )
        } else {
            logger.debug("planAndConnect, .groupLink, .ok, channel link without short link data")
            await MainActor.run {
                showAskCurrentOrIncognitoProfileSheet(
                    title: NSLocalizedString("Join channel", comment: "new chat sheet title"),
                    connectionLink: connectionLink,
                    connectionPlan: connectionPlan,
                    dismiss: dismiss,
                    cleanup: cleanup
                )
            }
        }
    } else {
        // existing code unchanged (lines 1333–1355)
    }
```

### 2. `prepareAndOpenChannel`

**New function** near `showPrepareGroupAlert` (~line 1073):

```swift
private func prepareAndOpenChannel(
    connectionLink: CreatedConnLink,
    groupShortLinkData: GroupShortLinkData,
    groupShortLinkInfo: GroupShortLinkInfo?,
    dismiss: Bool,
    cleanup: (() -> Void)?
) async {
    do {
        let chat = try await apiPrepareGroup(
            connLink: connectionLink,
            directLink: groupShortLinkInfo?.direct ?? true,
            groupShortLinkData: groupShortLinkData
        )
        await MainActor.run {
            if let relays = groupShortLinkInfo?.groupRelays, !relays.isEmpty,
               case let .group(gInfo, _) = chat.chatInfo {
                ChatModel.shared.channelRelayHostnames[gInfo.groupId] = relays
            }
            ChatModel.shared.addChat(Chat(chat))
            openKnownChat(chat.id, dismiss: dismiss, cleanup: cleanup)
        }
    } catch let error {
        logger.error("prepareAndOpenChannel error: \(error.localizedDescription)")
        await MainActor.run {
            showAlert(
                NSLocalizedString("Error opening channel", comment: "alert title"),
                message: responseError(error)
            )
            cleanup?()
        }
    }
}
```

Key differences from `showPrepareGroupAlert`: no popup, directly calls API + navigates. Passes `directLink` (resolves TODO at line 1332). Stores relay hostnames in `ChatModel.channelRelayHostnames`.

### 3. ChatBannerView relay info

**File:** `apps/ios/Shared/Views/Chat/ChatView.swift`

**3a.** In `chatContext` (line 1022–1025), change prepared group label. The existing `case .none:` block has other branches after `nextConnectPrepared` — keep them unchanged. Only modify the inner return value:

```swift
case .none:
    if groupInfo.nextConnectPrepared {
        if groupInfo.useRelays { "Tap Join channel" }
        else { "Tap Join group" }
    } else if ...  // existing branches unchanged
```

**3b.** Add `@EnvironmentObject var chatModel: ChatModel` to `ChatBannerView`.

**3c.** Add `@State private var relaysExpanded = false` to `ChatBannerView`.

**3d.** In `ChatBannerView.body`, after `chatContext` Text, add expandable relay info:

```swift
if case let .group(groupInfo, _) = chat.chatInfo,
   groupInfo.useRelays, groupInfo.nextConnectPrepared,
   let relays = chatModel.channelRelayHostnames[groupInfo.groupId], !relays.isEmpty {
    relayInfoRow(relays)
}
```

**3e. `relayInfoRow`** on `ChatBannerView`: `VStack` with `Button` toggling `relaysExpanded` (showing `"\(relays.count) relay(s)"` + chevron in `.font(.callout)`, `.foregroundStyle(.secondary)`). When expanded: `ForEach(relays, id: \.self)` showing hostname in `.font(.caption)`, `.foregroundStyle(.secondary)`, `.padding(.leading, 8)`.

### 4. ComposeView "Join channel" button

**File:** `apps/ios/Shared/Views/Chat/ComposeMessage/ComposeView.swift`
**Location:** Line 414–419

Inside the existing `if chat.chatInfo.groupInfo?.businessChat == nil` branch (line 415–416), replace the `connectButtonView` call. Keep the surrounding `if/else` structure with `sendContactRequestView` for business chats unchanged:

```swift
if chat.chatInfo.groupInfo?.businessChat == nil {
    let isChannel = chat.chatInfo.groupInfo?.useRelays == true
    connectButtonView(
        isChannel ? "Join channel" : "Join group",
        icon: isChannel ? "antenna.radiowaves.left.and.right" : "person.2.fill",
        connect: connectPreparedGroup
    )
} else {
    // existing sendContactRequestView unchanged
}
```

### 5. Connection progress display

**File:** `apps/ios/Shared/Views/Chat/ChatView.swift`

**5a.** Add `@State private var channelRelayProgress: [GroupRelay] = []`.

**5b.** Subscribe — attach `.onReceive` modifier to the outer `ZStack` in `viewBody` (same level as other modifiers like `.onAppear`):

```swift
.onReceive(NotificationCenter.default.publisher(for: .groupLinkRelaysUpdated)) { notification in
    guard case let .group(groupInfo, _) = chat.chatInfo,
          groupInfo.useRelays,
          let userInfo = notification.userInfo,
          let groupId = userInfo["groupId"] as? Int64,
          groupId == groupInfo.groupId,
          let relays = userInfo["groupRelays"] as? [GroupRelay] else { return }
    channelRelayProgress = relays
    if relays.allSatisfy({ $0.relayStatus == .rsActive }) {
        channelRelayProgress = []
        ChatModel.shared.channelRelayHostnames.removeValue(forKey: groupInfo.groupId)
    }
}
```

**5c.** Insert between the `connectingText` block (ends line 133) and the `if selectedChatItems == nil` guard (line 134) that contains `ComposeView`. This places the progress bar above the compose area:

```swift
if !channelRelayProgress.isEmpty {
    let activeCount = channelRelayProgress.filter { $0.relayStatus == .rsActive }.count
    let total = channelRelayProgress.count
    VStack(alignment: .leading, spacing: 4) {
        ProgressView(value: Double(activeCount), total: Double(max(total, 1)))
        Text("Connecting… \(activeCount)/\(total) relays")
            .font(.caption).foregroundStyle(.secondary)
    }
    .padding(.horizontal, 12).padding(.vertical, 4)
}
```

### 6. Relay data storage on ChatModel

**File:** `apps/ios/Shared/Model/ChatModel.swift`
Add near `@Published var groupMembers` (around line 70):

```swift
@Published var channelRelayHostnames: [Int64: [String]] = [:]
```

Runtime-only, not persisted. Keyed by `groupId`. Set in `prepareAndOpenChannel`, read by `ChatBannerView`, cleaned up when all relays active.

## Verification
- Tapping channel link: directly prepares + navigates (no popup).
- ChatBannerView: "Tap Join channel" + expandable relay count/hostnames.
- ComposeView: "Join channel" with antenna icon.
- After join: progress bar "Connecting… N/M relays", disappears when all active.
- Sync failure: error alert.
- Regular group links: unchanged (existing popup flow).
- `channelRelayHostnames` cleaned up on completion.
