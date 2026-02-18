# 05 — Chat Relay Management (Network & Servers)

## TOC
1. [Summary](#summary)
2. [Prerequisites](#prerequisites)
3. [Context](#context)
4. [Changes](#changes) — 4.1 Shared helpers · 4.2 Relay list sections · 4.3 Add server dialog · 4.4 ChatRelayView + ChatRelayViewLink · 4.5 noChatRelays warning · 4.6 duplicateChatRelay errors · 4.7 Notes
5. [Verification](#verification)

## Summary
Add "CHAT RELAYS" sections to OperatorView (preset relays) and YourServersView (custom relays). Create `ChatRelayView` for relay detail. Handle `noChatRelays` warning and `duplicateChatRelay*` errors. Implements product plan §2.5 (`plans/chat-relays-ui/ios-channels-product-plan.md`).

## Prerequisites
Before starting, ask the user to create this empty file in Xcode (right-click `NetworkAndServers` group → New File → Swift File):
- `apps/ios/Shared/Views/UserSettings/NetworkAndServers/ChatRelayView.swift`

## Context
- Product plan: `plans/chat-relays-ui/ios-channels-product-plan.md` §2.5.
- `UserOperatorServers.chatRelays: [UserChatRelay]?` (AppAPITypes.swift:1726).
- `UserChatRelay` (AppAPITypes.swift:1934): `chatRelayId`, `address`, `name`, `domains`, `preset`, `tested: Bool?`, `enabled`, `deleted`, `createdAt`.
- `ProtocolServerView` / `ProtocolServerViewLink` — existing server detail/list-row patterns to follow.
- `showTestStatus(server:)` (ProtocolServerView.swift:166) — test status icon pattern.
- `deleteSMPServer` / `deleteXFTPServer` (ProtocolServersView.swift:248/265) — delete pattern.
- `UserServersWarning.noChatRelays` (AppAPITypes.swift:1767), `UserServersError.duplicateChatRelayName/Address` (1775–1776) — already defined.
- `validateServers_` (NetworkAndServers.swift:362) — currently discards warnings with `_warns`.
- Product plan wireframe: relay hostname as nav title via `relay.domains.first`.

## Changes

### 1. Shared helpers

**1a. `bindingForChatRelays`** (NetworkAndServers.swift, near `globalServersError` ~line 399):

```swift
func bindingForChatRelays(_ userServers: Binding<[UserOperatorServers]>, _ opIndex: Int) -> Binding<[UserChatRelay]> {
    Binding(
        get: { userServers[opIndex].wrappedValue.chatRelays ?? [] },
        set: { userServers[opIndex].wrappedValue.chatRelays = $0 }
    )
}
```

**1b. `deleteChatRelay`** (ProtocolServersView.swift, after `deleteXFTPServer` ~line 280):

```swift
func deleteChatRelay(
    _ userServers: Binding<[UserOperatorServers]>,
    _ operatorServersIndex: Int,
    _ serverIndexSet: IndexSet
) {
    if let idx = serverIndexSet.first {
        let relay = userServers[operatorServersIndex].wrappedValue.chatRelays?[idx]
        if let relay = relay {
            if relay.chatRelayId == nil {
                userServers[operatorServersIndex].wrappedValue.chatRelays?.remove(at: idx)
            } else {
                var updatedRelay = relay
                updatedRelay.deleted = true
                userServers[operatorServersIndex].wrappedValue.chatRelays?[idx] = updatedRelay
            }
        }
    }
}
```

**1c. `showRelayTestStatus`** (ProtocolServerView.swift, after `showTestStatus` ~line 177):

```swift
@ViewBuilder func showRelayTestStatus(relay: UserChatRelay) -> some View {
    switch relay.tested {
    case .some(true): Image(systemName: "checkmark").foregroundColor(.green)
    case .some(false): Image(systemName: "multiply").foregroundColor(.red)
    case .none: Color.clear
    }
}
```

### 2. "CHAT RELAYS" sections in OperatorView + YourServersView

Both views get identical relay list sections, placed **before** the SMP "Use for messages" section. Common pattern:

```swift
if let chatRelays = userServers[operatorIndex].chatRelays,
   !chatRelays.filter({ !$0.deleted }).isEmpty {
    Section {
        ForEach(bindingForChatRelays($userServers, operatorIndex)) { relay in
            if !relay.wrappedValue.deleted {
                ChatRelayViewLink(
                    userServers: $userServers,
                    serverErrors: $serverErrors,
                    relay: relay,
                    backLabel: BACK_LABEL,
                    selectedRelay: $selectedServer
                )
            } else { EmptyView() }
        }
        // YourServersView ONLY: add .onDelete { deleteChatRelay + validateServers_ }
    } header: {
        Text("Chat relays").foregroundColor(theme.colors.secondary)
    } footer: {
        Text("Chat relays forward messages in channels you create.").foregroundColor(theme.colors.secondary)
    }
}
```

| | OperatorView (line 70) | YourServersView (line 41) |
|---|---|---|
| `BACK_LABEL` | `"\(userServers[operatorIndex].operator_.tradeName) servers"` | `"Your servers"` |
| `.onDelete` | None (preset) | `deleteChatRelay` + `validateServers_` |

Both views already have `@State private var selectedServer: String? = nil`. Relay rows share this binding.

### 3. "Add server" dialog (ProtocolServersView.swift)

**3a.** Add state (line 25): `@State private var newChatRelayNavLinkActive = false`

**3b.** Add hidden NavLink inside "Add server" Section (after existing hidden NavLink, line 124–126):

```swift
NavigationLink(isActive: $newChatRelayNavLinkActive) {
    NewChatRelayView(userServers: $userServers, serverErrors: $serverErrors, operatorIndex: operatorIndex)
        .navigationTitle("New chat relay")
        .navigationBarTitleDisplayMode(.large)
        .modifier(ThemedBackground(grouped: true))
} label: { EmptyView() }
.frame(width: 1, height: 1).hidden()
```

**3c.** Add to `.confirmationDialog` (line 151): `Button("Chat relay") { newChatRelayNavLinkActive = true }`

**3d. `NewChatRelayView`** (add to ChatRelayView.swift):
- **Props:** `@Environment(\.dismiss) var dismiss`, `@EnvironmentObject var theme: AppTheme`, `@Binding var userServers: [UserOperatorServers]`, `@Binding var serverErrors: [UserServersError]`, `var operatorIndex: Int`, `@State private var address: String = ""`.
- **Body:** `List` with `Section("Relay address")` containing `TextEditor(text: $address)` (`.autocorrectionDisabled(true)`, `.autocapitalization(.none)`), and `Section` with "Add relay" button. Button creates `UserChatRelay(chatRelayId: nil, address: trimmed, name: "", domains: [], preset: false, tested: nil, enabled: true, deleted: false)`, appends to `userServers[operatorIndex].chatRelays` (creating array if nil), calls `validateServers_($userServers, $serverErrors)`, dismisses. Button disabled when address empty.

### 4. ChatRelayView + ChatRelayViewLink

**File:** `apps/ios/Shared/Views/UserSettings/NetworkAndServers/ChatRelayView.swift`

Contains three structs: `ChatRelayView`, `ChatRelayViewLink`, `NewChatRelayView`.

**`ChatRelayView`** — follows `ProtocolServerView` pattern:
- **Props:** `@Binding userServers`, `@Binding serverErrors`, `@Binding relay: UserChatRelay`, `@State relayToEdit: UserChatRelay`, `backLabel: LocalizedStringKey`, `@State testing = false`.
- **Body:** `ZStack` with `presetRelay()` or `customRelay()`, overlaid `ProgressView` when testing.
- **`presetRelay()`:** Read-only address (`Text` + `.textSelection(.enabled)`), "Test relay" button + `showRelayTestStatus`, "Use for new channels" toggle on `relayToEdit.enabled`.
- **`customRelay()`:** Same but editable `TextEditor` for address (`.autocorrectionDisabled(true)`) + "Delete relay" destructive button (sets `deleted = true`, saves, dismisses).
- **Back button:** `.modifier(BackButton(...))` saves `relay = relayToEdit` + `validateServers_` + dismiss.
- **`testRelay()`:** STUB — `apiTestChatRelay` doesn't exist yet. Button present but non-functional.

**`ChatRelayViewLink`** — follows `ProtocolServerViewLink` pattern:
- **Props:** `@Binding userServers`, `@Binding serverErrors`, `@Binding relay: UserChatRelay`, `backLabel: LocalizedStringKey`, `@Binding selectedRelay: String?`.
- **Body:** `NavigationLink(tag: relay.id, selection: $selectedRelay)` → `ChatRelayView(userServers: $userServers, serverErrors: $serverErrors, relay: $relay, relayToEdit: relay, backLabel: backLabel)`. Note: pass `relayToEdit: relay` (the value, not binding) to initialize the `@State` copy-on-edit.
- **Nav title:** `relay.domains.first ?? (relay.name.isEmpty ? "Chat relay" : relay.name)`.
- **Label:** `HStack` with test status icon (or `slash.circle` if disabled), relay display name (`relay.domains.first ?? (relay.name.isEmpty ? relay.address : relay.name)`), dimmed when disabled.

### 5. `noChatRelays` warning propagation

**5a.** Add `serverWarnings` to `ServerSettings` (ChatListView.swift:67): `public var serverWarnings: [UserServersWarning] = []`. Also add to `SaveableSettings` initializer (line 64).

**5b.** Reset on server load (NetworkAndServers.swift:144): add `ss.servers.serverWarnings = []` after `ss.servers.serverErrors = []`.

**5c.** Extend `validateServers_` (NetworkAndServers.swift:362) with optional warnings binding:

```swift
func validateServers_(
    _ userServers: Binding<[UserOperatorServers]>,
    _ serverErrors: Binding<[UserServersError]>,
    _ serverWarnings: Binding<[UserServersWarning]>? = nil
)
```

Inside the existing function body (which is wrapped in `Task { do { ... } catch { ... } }`), make two changes:
1. Change `let (errs, _warns) = try await validateServers(...)` to `let (errs, warns) = try await validateServers(...)`
2. Inside the existing `await MainActor.run { ... }` block, after `serverErrors.wrappedValue = errs`, add: `serverWarnings?.wrappedValue = warns`

Existing 2-arg call sites continue working via default `nil`.

**5d.** Initial validation (NetworkAndServers.swift, after line 144): `validateServers_($ss.servers.userServers, $ss.servers.serverErrors, $ss.servers.serverWarnings)`

**5e.** Display warning in footer (NetworkAndServers.swift:111–117):

```swift
if let warnStr = globalServersWarning(ss.servers.serverWarnings) {
    Text(warnStr).foregroundColor(theme.colors.secondary)
}
```

Helper:

```swift
func globalServersWarning(_ serverWarnings: [UserServersWarning]) -> String? {
    for warn in serverWarnings {
        switch warn {
        case let .noChatRelays(user):
            let text = NSLocalizedString("No chat relays enabled. Channels require at least one relay.", comment: "servers warning")
            if let user = user {
                return String.localizedStringWithFormat(NSLocalizedString("For chat profile %@:", comment: "servers warning"), user.localDisplayName) + " " + text
            } else { return text }
        }
    }
    return nil
}
```

### 6. `duplicateChatRelay*` errors (AppAPITypes.swift:1778–1797)

Add before `default: return nil` in `UserServersError.globalError`:

```swift
case let .duplicateChatRelayName(duplicateChatRelay):
    return String.localizedStringWithFormat(NSLocalizedString("Duplicate chat relay name: %@", comment: "servers error"), duplicateChatRelay)
case let .duplicateChatRelayAddress(_, duplicateAddress):
    return String.localizedStringWithFormat(NSLocalizedString("Duplicate chat relay address: %@", comment: "servers error"), duplicateAddress)
```

### 7. Notes

- **`serversCanBeSaved`:** No change — `UserOperatorServers` includes `chatRelays` in `Equatable`.
- **`TestServersButton`:** Deferred until `apiTestChatRelay` backend exists.

## Verification
- Build succeeds.
- Operator page: "CHAT RELAYS" section before "Use for messages".
- Your Servers: "CHAT RELAYS" section before "Message servers".
- Relay detail: preset = read-only + toggle; custom = editable + delete.
- Test button present but non-functional (stub).
- No relays enabled → warning in footer.
- Duplicate relay errors shown.
- Saving works (relay changes detected by existing Equatable).
- Regular server sections unaffected.
