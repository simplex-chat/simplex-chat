# 06 — Channel Creation

## TOC
1. [Summary](#summary)
2. [Prerequisites](#prerequisites)
3. [Context](#context)
4. [Changes](#changes) — 4.0 processReceivedMsg handler · 4.1 NewChatMenuButton · 4.2 AddChannelView
5. [Verification](#verification)

## Summary
Add "Create channel" menu item and 3-step creation flow: profile → relay connection progress → channel link. Add `processReceivedMsg` handler for `groupLinkRelaysUpdated` event. Implements product plan §2.3 (`plans/chat-relays-ui/ios-channels-product-plan.md`).

## Prerequisites
1. **Plan 04 must be completed first** — provides `hostFromRelayLink` (module-scope function in `ChannelRelaysView.swift`) and `RelayStatus.text` (extension in `ChatTypes.swift`).
2. Ask the user to create this empty file in Xcode (right-click `NewChat` group → New File → Swift File):
   - `apps/ios/Shared/Views/NewChat/AddChannelView.swift`

## Context
- Product plan: `plans/chat-relays-ui/ios-channels-product-plan.md` §2.3.
- Entry point: `NewChatMenuButton.swift` lines 120–127 (Section containing "Create group").
- `apiNewPublicGroup(incognito:relayIds:groupProfile:)` (SimpleXAPI.swift:1813) → `(GroupInfo, GroupLink, [GroupRelay])`.
- `CEvtGroupLinkRelaysUpdated` (AppAPITypes.swift:1095) — event exists, NOT handled in `processReceivedMsg`.
- `GroupRelay` (ChatTypes.swift:2504), `RelayStatus` (ChatTypes.swift:2497).
- `hostFromRelayLink` — file-scope helper in ChannelRelaysView.swift.
- `RelayStatus.text` — computed property in ChatTypes.swift extension.
- `getUserServers()` (SimpleXAPI.swift:770) — fetches operator servers for relay ID lookup.
- `GroupLinkView(creatingGroup: true)` — existing QR+share view, "Continue" toolbar item.
- **Failed relay status:** Backend must send `groupLinkRelaysUpdated` with failure state. UI displays whatever status backend provides.

## Changes

### 0. `processReceivedMsg` handler for `groupLinkRelaysUpdated`

**File:** `apps/ios/Shared/Model/SimpleXAPI.swift`
**Location:** After `.groupUpdated` case (line 2553):

```swift
case let .groupLinkRelaysUpdated(user, groupInfo, groupLink, groupRelays):
    if active(user) {
        await MainActor.run { m.updateGroup(groupInfo) }
        NotificationCenter.default.post(
            name: .groupLinkRelaysUpdated,
            object: nil,
            userInfo: ["groupId": groupInfo.groupId, "groupLink": groupLink, "groupRelays": groupRelays]
        )
    }
```

**Add notification name** in `Notification.Name` extension (ViewModifiers.swift:22–24):

```swift
static let groupLinkRelaysUpdated = Notification.Name("groupLinkRelaysUpdated")
```

### 1. NewChatMenuButton — "Create channel" item

Insert after "Create group" NavigationLink (line 127), before the Section's closing `}` (line 128):

```swift
NavigationLink {
    AddChannelView()
        .navigationTitle("Create channel")
        .modifier(ThemedBackground(grouped: true))
        .navigationBarTitleDisplayMode(.large)
} label: {
    Label("Create channel", systemImage: "antenna.radiowaves.left.and.right")
}
```

### 2. AddChannelView — three-phase creation flow

**File:** `apps/ios/Shared/Views/NewChat/AddChannelView.swift`

Three-phase `@State`-driven view following `AddGroupView` pattern but using `apiNewPublicGroup`.

**Imports:** `import SwiftUI`, `import SimpleXChat`.

**Environment:** `@EnvironmentObject var m: ChatModel`, `@EnvironmentObject var theme: AppTheme`.

**State:** Profile (`profile: GroupProfile`, `chosenImage`, image picker booleans, `showInvalidNameAlert`), phase (`groupInfo: GroupInfo?`, `groupLink: GroupLink?`, `groupRelays: [GroupRelay]`, `creationInProgress`, `showLinkStep`), UI (`relayListExpanded`, `alert: AddChannelAlert?`).

**Alert enum:**

```swift
enum AddChannelAlert: Identifiable {
    case error(title: String, message: String)
    case retryableError(title: String, message: String)
    case proceedWithPartialRelays(connectedCount: Int, totalCount: Int)
    var id: String { switch self { case .error: "error"; case .retryableError: "retryableError"; case .proceedWithPartialRelays: "proceedPartial" } }
}
```

**Body:** `showLinkStep && groupInfo != nil` → linkStep. `groupInfo != nil` → progressStep. Else → profileStep.

#### Step 1 — Profile

Layout (follows AddGroupView lines 69–90):
- Centered `ProfileImage` with `editImageButton` + camera/library pickers (same `.confirmationDialog` + `.fullScreenCover` + `.sheet` as AddGroupView).
- Channel name `TextField` with `validDisplayName` + `createInvalidNameAlert`.
- "Configure relays..." `NavigationLink` → `NetworkAndServers()` with `.navigationTitle("Your servers")`.
- "Create channel" `Button` → `createChannel()`. Disabled when name invalid or `creationInProgress`.
- Footer: "Your profile will be shared with chat relays and subscribers." + "Random relays will be selected from the list of enabled chat relays."
- Image resize: `resizeImageToStrSize(cropToSquare(image), maxDataSize: 12500)` in `.onChange(of: chosenImage)`.

#### `createChannel()` + `getEnabledRelayIds()`

```swift
private func createChannel() {
    profile.displayName = profile.displayName.trimmingCharacters(in: .whitespaces)
    profile.groupPreferences = GroupPreferences(history: GroupPreference(enable: .on))
    creationInProgress = true
    Task {
        do {
            let relayIds = try await getEnabledRelayIds()
            guard !relayIds.isEmpty else {
                await MainActor.run {
                    creationInProgress = false
                    alert = .error(
                        title: NSLocalizedString("No relays enabled", comment: "alert title"),
                        message: NSLocalizedString("Enable at least one chat relay in Network & Servers.", comment: "alert message")
                    )
                }
                return
            }
            let (gInfo, gLink, gRelays) = try await apiNewPublicGroup(
                incognito: false, relayIds: relayIds, groupProfile: profile
            )
            let c = Chat(chatInfo: .group(groupInfo: gInfo, groupChatScope: nil), chatItems: [])
            await MainActor.run {
                m.addChat(c)
                groupInfo = gInfo; groupLink = gLink; groupRelays = gRelays
                creationInProgress = false
            }
        } catch {
            await MainActor.run {
                creationInProgress = false
                alert = .retryableError(
                    title: NSLocalizedString("Error creating channel", comment: "alert title"),
                    message: responseError(error)
                )
            }
        }
    }
}

private func getEnabledRelayIds() async throws -> [Int64] {
    let servers = try await getUserServers()
    return servers.flatMap { op in
        (op.chatRelays ?? []).filter { $0.enabled && !$0.deleted }.compactMap { $0.chatRelayId }
    }
}
```

#### Step 2 — Progress

**Function signature:** `private func progressStepView(_ gInfo: GroupInfo) -> some View`. The body routing unwraps `groupInfo` state via `if let gInfo = groupInfo { progressStepView(gInfo) }`, so `gInfo` is a non-optional `GroupInfo` parameter available throughout this function including modifiers.

Layout:
- `.navigationTitle("Creating channel…")`, `.navigationBarBackButtonHidden(true)`, Cancel toolbar → `dismissAllSheets(animated: true)`.
- Read-only channel profile (centered `ProfileImage` + name).
- `ProgressView(value: Double(activeCount), total: Double(max(total, 1)))`.
- Tappable relay count (`Button` toggling `relayListExpanded`) showing `"N/M relays connected"` + chevron.
- Expanded: `ForEach(groupRelays)` with `relayDisplayName` + `relayStatusIndicator` (same helpers as ChannelRelaysView.swift).
- "Channel link" button: enabled when `activeCount > 0`. Partial → `.proceedWithPartialRelays` alert. All active → `showLinkStep = true`.

**Notification listener:**

```swift
.onReceive(NotificationCenter.default.publisher(for: .groupLinkRelaysUpdated)) { notification in
    guard let userInfo = notification.userInfo,
          let groupId = userInfo["groupId"] as? Int64,
          groupId == gInfo.groupId,
          let relays = userInfo["groupRelays"] as? [GroupRelay] else { return }
    groupRelays = relays
    if relays.allSatisfy({ $0.relayStatus == .rsActive }) { showLinkStep = true }
}
```

**Alert handler:**

```swift
.alert(item: $alert) { alertItem in
    switch alertItem {
    case let .error(title, message):
        Alert(title: Text(title), message: Text(message))
    case let .retryableError(title, message):
        Alert(title: Text(title), message: Text(message),
              primaryButton: .default(Text("Retry")) { createChannel() },
              secondaryButton: .cancel())
    case let .proceedWithPartialRelays(connected, total):
        Alert(title: Text("Not all relays connected"),
              message: Text("Channel will start working with \(connected) of \(total) relays. Proceed?"),
              primaryButton: .default(Text("Proceed")) { showLinkStep = true },
              secondaryButton: .cancel(Text("Wait")))
    }
}
```

#### Step 3 — Link

**Function signature:** `private func linkStepView(_ gInfo: GroupInfo) -> some View`. Same unwrap pattern as Step 2.

Wrap `GroupLinkView(groupId: gInfo.groupId, groupLink: $groupLink, groupLinkMemberRole: .constant(.member), showTitle: false, creatingGroup: true, linkCreatedCb: { ... })`. Callback: dismiss all sheets + `ItemsModel.shared.loadOpenChat(gInfo.id)` after 0.5s delay.

Nav title: "Channel link". `groupLink` already set from `apiNewPublicGroup`, so QR displays immediately (GroupLinkView `onAppear` guard: `if groupLink == nil`).

**Back navigation:** `GroupLinkView(creatingGroup: true)` hides back button, adds "Continue". Accept for MVP.

#### Helpers

Define as `private func` inside `AddChannelView`. `hostFromRelayLink` is a module-scope free function defined in `ChannelRelaysView.swift` — accessible from any file in the same target.

```swift
private func relayDisplayName(_ relay: GroupRelay) -> String {
    if let link = relay.relayLink { return hostFromRelayLink(link) }
    return "relay\(relay.groupRelayId)"
}

private func relayStatusIndicator(_ status: RelayStatus) -> some View {
    HStack(spacing: 4) {
        Circle()
            .fill(status == .rsActive ? .green : status == .rsNew ? .red : .orange)
            .frame(width: 8, height: 8)
        Text(status.text).font(.caption).foregroundStyle(.secondary)
    }
}
```

## Verification
- Build succeeds.
- "Create channel" in New Chat menu after "Create group" with antenna icon.
- Step 1: name + image + "Configure relays..." + button (disabled when invalid). "No relays enabled" error.
- Step 2: progress bar via notification. Tappable relay list. "Channel link" enabled ≥1 active. Partial → warning. All active → auto-advance.
- Step 3: QR + share + "Continue" → opens channel chat.
- Sync failure: Retry / Cancel alert.
- Regular group creation unaffected.
