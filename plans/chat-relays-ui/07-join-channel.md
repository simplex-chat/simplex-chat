# 07 — Join Channel

## TOC
1. [Summary](#summary)
2. [Prerequisites](#prerequisites)
3. [Context](#context)
4. [Changes](#changes) — 4.1 planAndConnect detection · 4.2 prepareAndOpenChannel · 4.3 ChatBannerView label · 4.4 ComposeView "Join channel" · 4.5 Pre-join relay info & connection progress · 4.6 Relay data storage
5. [Known Limitations](#known-limitations)
6. [Verification](#verification)

## Summary
Detect channel links in `planAndConnect`, prepare the channel and navigate to ChatView (no popup alert). Show relay info bar above compose view pre-join. After join, display relay connection progress in the same area. Implements product plan §2.6 (`plans/chat-relays-ui/ios-channels-product-plan.md`).

## Prerequisites
- **Plan 04 must be completed first** — provides `hostFromRelayLink` (module-scope function in `ChannelRelaysView.swift`) and `RelayStatus.text` (extension in `ChatTypes.swift`).
- **Plan 06 must be completed first** — provides `.groupLinkRelaysUpdated` notification name (in `ViewModifiers.swift`) and `processReceivedMsg` handler (in `SimpleXAPI.swift`).

## Context
- Product plan: `plans/chat-relays-ui/ios-channels-product-plan.md` §2.6.
- `planAndConnect` — join flow dispatcher in `NewChatView.swift`. `GroupLinkPlan.ok` case at line 1332.
- `GroupShortLinkInfo` (AppAPITypes.swift:1339) — `direct: Bool`, `groupRelays: [String]`, `sharedGroupId: String?`.
- Channel detection: non-empty `groupSLinkInfo_.groupRelays`.
- `apiPrepareGroup` (SimpleXAPI.swift:1112) → `ChatData`. `apiConnectPreparedGroup` (SimpleXAPI.swift:1138).
- `showPrepareGroupAlert` (NewChatView.swift:1073–1106) — popup alert. Channels skip this.
- `connectButtonView` (ComposeView.swift:646–659). Used at line 414.
- `connectingText` (ChatView.swift:128–133) — insert pre-join relay info and post-join progress after this block, before `ComposeView` (line 134).
- `PreparedGroup` (ChatTypes.swift:2406–2410) — no relay fields.
- `GroupInfo.nextConnectPrepared` (ChatTypes.swift:2356) — `true` when `preparedGroup != nil && !connLinkStartedConnection`. Becomes `false` after `APIConnectPreparedGroup` sets `connLinkStartedConnection = true`.
- `groupLinkRelaysUpdated` notification (ViewModifiers.swift, Plan 06).
- `hostFromRelayLink` (ChannelRelaysView.swift, Plan 04), `RelayStatus.text` (ChatTypes.swift, Plan 04).
- `ChatView` has `@EnvironmentObject var chatModel: ChatModel` (line 17). Use `chatModel.channelRelayHostnames` in view body, `ChatModel.shared` in non-view code.
- **Relay re-fetch at connect time:** `APIConnectPreparedGroup` re-fetches the relay list from the SMP server via `getShortLinkConnReq` before connecting (Commands.hs:1994–2000). Pre-join relay data is informational only — the authoritative relay list is always fetched fresh.

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
                // Runtime-only: informational relay list for pre-join display.
                // Not persisted — lost on app restart (see Known Limitations).
                // APIConnectPreparedGroup re-fetches fresh relays at connect time.
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

### 3. ChatBannerView — label change only

**File:** `apps/ios/Shared/Views/Chat/ChatView.swift`

In `chatContext` (line 1022–1025), change prepared group label:

```swift
case .none:
    if groupInfo.nextConnectPrepared {
        if groupInfo.useRelays { "Tap Join channel" }
        else { "Tap Join group" }
    } else if ...  // existing branches unchanged
```

**No other ChatBannerView changes.** No `@EnvironmentObject`, no relay display in the banner.

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

### 5. Pre-join relay info & post-join connection progress

Both displayed above the compose area, between `connectingText` (ends line 133) and the `if selectedChatItems == nil` guard (line 134) that contains `ComposeView`. Pre-join and post-join are effectively mutually exclusive: `nextConnectPrepared` is `true` before join, `false` after (backend sets `connLinkStartedConnection = true`). The post-join `channelRelayProgress` is populated via async notification only after connection starts, by which time `nextConnectPrepared` is already false.

**Note:** Implement Section 6 (ChatModel property) before Sections 2 and 5, which reference `channelRelayHostnames`.

**5a.** Add state variables to `ChatView` (near other `@State` declarations, line 30):

```swift
@State private var relaysExpanded = false
@State private var channelRelayProgress: [GroupRelay] = []
```

**5b.** Subscribe to relay progress — attach `.onReceive` modifier to the outer `ZStack` in `viewBody` (same level as other `.onAppear`/`.onReceive` modifiers):

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
        chatModel.channelRelayHostnames.removeValue(forKey: groupInfo.groupId)
    }
}
```

**5c.** Insert between `connectingText` block (ends line 133) and the `if selectedChatItems == nil` guard (line 134):

```swift
// Pre-join: relay info when channel is prepared but not yet joined
if case let .group(groupInfo, _) = chat.chatInfo,
   groupInfo.useRelays, groupInfo.nextConnectPrepared,
   let relays = chatModel.channelRelayHostnames[groupInfo.groupId], !relays.isEmpty {
    VStack(alignment: .leading, spacing: 4) {
        Button {
            relaysExpanded.toggle()
        } label: {
            HStack {
                Text("\(relays.count) relay(s)")
                Spacer()
                Image(systemName: relaysExpanded ? "chevron.down" : "chevron.right")
            }
            .font(.callout)
            .foregroundStyle(.secondary)
        }
        if relaysExpanded {
            ForEach(relays, id: \.self) { relay in
                Text(hostFromRelayLink(relay))
                    .font(.caption)
                    .foregroundStyle(.secondary)
                    .padding(.leading, 8)
            }
        }
    }
    .padding(.horizontal, 12).padding(.vertical, 4)
}

// Post-join: connection progress
if !channelRelayProgress.isEmpty {
    VStack(alignment: .leading, spacing: 4) {
        let activeCount = channelRelayProgress.filter { $0.relayStatus == .rsActive }.count
        let total = channelRelayProgress.count
        ProgressView(value: Double(activeCount), total: Double(max(total, 1)))
        Button {
            relaysExpanded.toggle()
        } label: {
            HStack {
                Text("Connecting… \(activeCount)/\(total) relays")
                Spacer()
                Image(systemName: relaysExpanded ? "chevron.down" : "chevron.right")
            }
            .font(.caption)
            .foregroundStyle(.secondary)
        }
        if relaysExpanded {
            ForEach(channelRelayProgress) { relay in
                HStack {
                    Text(hostFromRelayLink(relay.relayLink ?? ""))
                    Spacer()
                    Circle()
                        .fill(relay.relayStatus == .rsActive ? .green : relay.relayStatus == .rsNew ? .red : .orange)
                        .frame(width: 8, height: 8)
                    Text(relay.relayStatus.text)
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }
                .font(.caption)
                .padding(.leading, 8)
            }
        }
    }
    .padding(.horizontal, 12).padding(.vertical, 4)
}
```

### 6. Relay data storage on ChatModel

**File:** `apps/ios/Shared/Model/ChatModel.swift`
Add near `@Published var groupMembers` (around line 70):

```swift
/// Runtime-only relay link data for pre-join channel display.
/// Populated from GroupShortLinkInfo.groupRelays during prepareAndOpenChannel.
/// Not persisted — lost on app restart. See Known Limitations below.
/// APIConnectPreparedGroup re-fetches fresh relays at connect time,
/// so stale/missing display data does not affect join functionality.
@Published var channelRelayHostnames: [Int64: [String]] = [:]
```

Keyed by `groupId`. Set in `prepareAndOpenChannel`, read by ChatView pre-join relay info, cleaned up when all relays active.

## Known Limitations

Document these edge cases in code comments at the relevant locations (`prepareAndOpenChannel`, `channelRelayHostnames` declaration, and the pre-join relay info view in ChatView):

1. **App restart between prepare and join:** `channelRelayHostnames` is runtime-only and lost on restart. After restart, `ChatBannerView` still shows "Tap Join channel" (from persisted `useRelays` + `nextConnectPrepared`), and the "Join channel" button works (from persisted `preparedGroup`). Only the pre-join relay list above compose is missing. The actual join re-fetches fresh relay data from the SMP server, so connection is unaffected.

2. **Why not persisted:** `APIConnectPreparedGroup` re-fetches the relay list from the SMP server via `getShortLinkConnReq` before connecting (Commands.hs:1994–2000). This is by design — the channel owner may update relays between prepare and join. Persisting stale hostnames would be misleading. The pre-join display is informational only ("relays advertised at the time this link was opened").

3. **Post-join progress depends on `groupLinkRelaysUpdated` notification** (established in Plan 06). If the backend does not send `CEvtGroupLinkRelaysUpdated` during the joining flow, the progress bar will not appear. The channel still functions — progress display is cosmetic.

**Instruction to implementing agent:** Add `// TODO [channels] runtime-only, see plan 07 Known Limitations` comments at: (a) `channelRelayHostnames` declaration, (b) the assignment in `prepareAndOpenChannel`, (c) the pre-join relay info guard in ChatView.

## Verification
- Tapping channel link: directly prepares + navigates (no popup).
- ChatBannerView: "Tap Join channel" label (no relay info in banner).
- Above compose (pre-join): expandable relay count/hostnames from `channelRelayHostnames`.
- ComposeView: "Join channel" with antenna icon.
- After join: progress bar "Connecting… N/M relays" above compose, disappears when all active.
- Sync failure: error alert.
- Regular group links: unchanged (existing popup flow).
- App restart: join still works, pre-join relay display absent (acceptable, documented).
