# 05 — Relay Management Settings (§4.5)

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

**What**: User-level relay configuration screen in Network & Servers settings. Lists preset and custom relays. Supports add, remove, enable/disable, and test connectivity. Follows the existing `ProtocolServersView` / `UserServer` management pattern.

**Why**: Channel owners need relays configured before creating channels. Relay management is infrastructure-level configuration, alongside SMP and XFTP servers.

**User impact**: Users can see which relays are available, add custom relays, test connectivity, and manage their relay infrastructure — all from the familiar server settings area.

---

## 2. Prerequisites & Dependencies

- **§4.1 (API Type Updates)**: `UserChatRelay` type must exist in Swift.
- **Backend §2.5 (`APITestChatRelay`)**: Test command must be implemented for the "Test relays" button to work. Can stub the button if backend isn't ready.
- **Backend API**: Commands for listing, adding, removing, updating relays must exist. These likely parallel the `APIGetUserServers` / `APISetUserServers` pattern.
- **Preset relays**: `Operators/Presets.hs` has 3 placeholder relays. Real addresses needed for production (§2.6).

---

## 3. Data Model

### 3.1 UserChatRelay (from Operators.hs:262)

```haskell
data UserChatRelay' s = UserChatRelay
  { chatRelayId :: DBEntityId' s,
    address :: ShortLinkContact,
    name :: Text,
    domains :: [Text],
    preset :: Bool,
    tested :: Maybe Bool,
    enabled :: Bool,
    deleted :: Bool
  }
```

Swift:
```swift
public struct UserChatRelay: Identifiable, Decodable, Equatable {
    public var chatRelayId: Int64
    public var address: String       // ShortLinkContact serialized
    public var name: String
    public var domains: [String]
    public var preset: Bool
    public var tested: Bool?         // nil=untested, true=pass, false=fail
    public var enabled: Bool
    public var deleted: Bool

    public var id: Int64 { chatRelayId }
}
```

### 3.2 PresetOperator Context

Relays are stored under operators, similar to SMP/XFTP servers (`Operators.hs:281-289`):
```haskell
data PresetOperator = PresetOperator
  { ...
    chatRelays :: [NewUserChatRelay],
    useChatRelays :: Int
  }
```

### 3.3 API Commands (Expected)

Based on existing server management patterns:
```swift
// List user's chat relays
func apiGetUserChatRelays() async throws -> [UserChatRelay]

// Update user's chat relays (add/remove/modify)
func apiSetUserChatRelays(relays: [UserChatRelay]) async throws -> [UserChatRelay]

// Test a specific relay
func apiTestChatRelay(chatRelayId: Int64) async throws -> Maybe<ProtocolTestFailure>
```

### 3.4 Relay Status Display Mapping

```
tested == nil   → No icon (untested)
tested == true  → Green checkmark (passed)
tested == false → Red X (failed)
enabled == false → Greyed out row
deleted == true  → Not shown in UI
```

---

## 4. Implementation Plan

### 4.1 `Shared/Views/UserSettings/NetworkAndServers/NetworkAndServers.swift` — Add Entry Point

**Location**: After XFTP servers section, before advanced settings

**Change**: Add NavigationLink to ChatRelaysView:
```swift
Section {
    NavigationLink {
        ChatRelaysView()
    } label: {
        HStack {
            Image(systemName: "megaphone")
                .frame(width: 24, alignment: .center)
            Text("Chat relays")
        }
    }
} header: {
    Text("Chat relays")
} footer: {
    Text("Relays forward messages in channels you create.")
}
```

### 4.2 `Shared/Views/UserSettings/NetworkAndServers/ChatRelaysView.swift` — New File

**Pattern**: Follows `YourServersView` (ProtocolServersView.swift:14-186) structure:

```swift
struct ChatRelaysView: View {
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    @Environment(\.editMode) private var editMode
    @State private var relays: [UserChatRelay] = []
    @State private var testing = false
    @State private var showAddRelay = false
    @State private var newRelayNavLinkActive = false
    @State private var showScanRelay = false
    @State private var alert: SomeAlert?

    var body: some View {
        relaysView()
            .opacity(testing ? 0.4 : 1)
            .overlay {
                if testing {
                    ProgressView().scaleEffect(2)
                }
            }
            .allowsHitTesting(!testing)
            .onAppear { loadRelays() }
            .navigationTitle("Chat relays")
            .alert(item: $alert) { $0.alert }
    }

    private func relaysView() -> some View {
        List {
            // PRESET RELAYS SECTION
            presetRelaysSection()

            // YOUR RELAYS SECTION
            customRelaysSection()

            // ADD RELAY
            addRelaySection()

            // TEST & INFO
            testSection()
        }
    }
}
```

**Key sections**:

**Preset relays**:
```swift
private func presetRelaysSection() -> some View {
    let presetRelays = relays.filter { $0.preset && !$0.deleted }
    return Group {
        if !presetRelays.isEmpty {
            Section(header: Text("SimpleX Chat relays")) {
                ForEach(presetRelays) { relay in
                    relayRow(relay)
                }
            }
        }
    }
}
```

**Custom relays**:
```swift
private func customRelaysSection() -> some View {
    let customRelays = relays.filter { !$0.preset && !$0.deleted }
    return Group {
        if !customRelays.isEmpty {
            Section(header: Text("Your relays")) {
                ForEach(customRelays) { relay in
                    relayRow(relay)
                }
                .onDelete { indexSet in
                    deleteCustomRelays(indexSet)
                }
            }
        }
    }
}
```

**Relay row**:
```swift
private func relayRow(_ relay: UserChatRelay) -> some View {
    HStack {
        VStack(alignment: .leading) {
            Text(relay.name)
                .foregroundColor(relay.enabled ? theme.colors.onBackground : theme.colors.secondary)
            if !relay.domains.isEmpty {
                Text(relay.domains.joined(separator: ", "))
                    .font(.caption)
                    .foregroundColor(theme.colors.secondary)
            }
        }
        Spacer()
        // Test status icon
        if let tested = relay.tested {
            Image(systemName: tested ? "checkmark" : "xmark")
                .foregroundColor(tested ? .green : .red)
        }
    }
}
```

### 4.3 `Shared/Views/UserSettings/NetworkAndServers/ChatRelayView.swift` — New File

Individual relay detail/edit view. Shows:
- Relay name
- Relay address
- Domains
- Enable/disable toggle
- Test button
- Delete button (custom relays only)

```swift
struct ChatRelayView: View {
    @Binding var relay: UserChatRelay
    var onDelete: (() -> Void)?

    var body: some View {
        List {
            Section {
                HStack {
                    Text("Name")
                    Spacer()
                    Text(relay.name)
                        .foregroundColor(.secondary)
                }
                HStack {
                    Text("Address")
                    Spacer()
                    Text(relay.address)
                        .foregroundColor(.secondary)
                        .lineLimit(1)
                        .truncationMode(.middle)
                }
            }

            Section {
                Toggle("Enabled", isOn: $relay.enabled)
            }

            Section {
                Button("Test relay") { testRelay() }
                    .disabled(!relay.enabled)
            }

            if !relay.preset, let onDelete {
                Section {
                    Button("Delete relay", role: .destructive) { onDelete() }
                }
            }
        }
        .navigationTitle(relay.name)
    }
}
```

### 4.4 `Shared/Model/SimpleXAPI.swift` — API Functions

```swift
func apiGetUserChatRelays() async throws -> [UserChatRelay] {
    let userId = try currentUserId("apiGetUserChatRelays")
    let r = await chatSendCmd(.apiGetUserChatRelays(userId: userId))
    if case let .userChatRelays(_, relays) = r { return relays }
    throw r
}

func apiTestChatRelay(chatRelayId: Int64) async throws -> Maybe<ProtocolTestFailure> {
    // ...
}
```

### 4.5 `SimpleXChat/ChatTypes.swift` — UserChatRelay Type

Add `UserChatRelay` struct as defined in §3.1.

---

## 5. Wireframes

### 5.1 Primary Design — Relay List

```
┌─────────────────────────────────┐
│  < Chat relays                  │
├─────────────────────────────────┤
│  SIMPLEX CHAT RELAYS            │
│                                 │
│  relay1.simplex.im         [*]  │
│  simplex.im                     │
│                                 │
│  relay2.simplex.im         [*]  │
│  simplex.im                     │
│                                 │
│  relay3.simplex.im         [*]  │
│  simplex.im                     │
│                                 │
├─────────────────────────────────┤
│  YOUR RELAYS                    │
│                                 │
│  myrelay.example.com       [!]  │
│  example.com                    │
│                                 │
│  + Add relay...                 │
│                                 │
├─────────────────────────────────┤
│  Test all relays                │
│  How to use chat relays       > │
│                                 │
│  Chat relays forward messages   │
│  in channels you create.        │
└─────────────────────────────────┘

Legend:
  [*] = green checkmark (test passed)
  [!] = red X (test failed)
  (no icon) = untested
```

### 5.2 Primary Design — Individual Relay

```
┌─────────────────────────────────┐
│  < relay1.simplex.im            │
├─────────────────────────────────┤
│  Name       relay1.simplex.im   │
│  Address    simplex:/a/...      │
│  Domains    simplex.im          │
│                                 │
├─────────────────────────────────┤
│  Enabled               [ON ]   │
│                                 │
├─────────────────────────────────┤
│  Test relay                     │
│  (Result: Passed / Failed)      │
│                                 │
└─────────────────────────────────┘
```

### 5.3 Primary Design — Add Relay Dialog

```
         ┌───────────────────────┐
         │  Add relay            │
         │                       │
         │  Enter relay manually │
         │  Scan relay QR code   │
         │                       │
         │  Cancel               │
         └───────────────────────┘
```

### 5.4 Alternative Design A — Flat List (No Sections)

All relays in a single list without preset/custom separation:

```
┌─────────────────────────────────┐
│  Chat relays                    │
├─────────────────────────────────┤
│  relay1.simplex.im  preset [*]  │
│  relay2.simplex.im  preset [*]  │
│  relay3.simplex.im  preset [*]  │
│  myrelay.example.com       [!]  │
│                                 │
│  + Add relay...                 │
│  Test all relays                │
└─────────────────────────────────┘
```

### 5.5 Alternative Design B — Inline in NetworkAndServers

Instead of a separate view, show relays inline in the NetworkAndServers view:

```
┌─────────────────────────────────┐
│  Network & servers              │
├─────────────────────────────────┤
│  SMP SERVERS                    │
│  ...                            │
├─────────────────────────────────┤
│  XFTP SERVERS                   │
│  ...                            │
├─────────────────────────────────┤
│  CHAT RELAYS                    │
│  relay1.simplex.im         [*]  │
│  relay2.simplex.im         [*]  │
│  relay3.simplex.im         [*]  │
│  + Add relay...                 │
│  Test relays                    │
├─────────────────────────────────┤
│  ADVANCED                       │
│  ...                            │
└─────────────────────────────────┘
```

### 5.6 State Variations

**No relays at all** (unlikely — presets should exist):
```
┌─────────────────────────────────┐
│  Chat relays                    │
├─────────────────────────────────┤
│                                 │
│  No relays configured.          │
│                                 │
│  + Add relay...                 │
│                                 │
│  Chat relays are needed to      │
│  create channels.               │
└─────────────────────────────────┘
```

**Testing in progress**:
```
┌─────────────────────────────────┐
│  Chat relays                    │
├─────────────────────────────────┤
│  (dimmed, non-interactive)      │
│                                 │
│        ┌──────┐                │
│        │ [~~] │                │
│        │      │  Testing...    │
│        └──────┘                │
│                                 │
│  relay1.simplex.im         [*]  │
│  relay2.simplex.im         [ ]  │
│  relay3.simplex.im         [ ]  │
└─────────────────────────────────┘
```

**Test failures alert**:
```
         ┌───────────────────────┐
         │  Tests failed!        │
         │                       │
         │  Some relays failed   │
         │  the test:            │
         │  myrelay.example.com: │
         │  Connection refused   │
         │                       │
         │       [OK]            │
         └───────────────────────┘
```

---

## 6. Design Rationale

**Separate ChatRelaysView (Primary) > Flat list (Alt A)**:
- Sections clearly distinguish preset (managed by SimpleX) from custom (user-added)
- Follows existing ProtocolServersView pattern — consistency
- Preset relays may have different UI affordances (no delete, maybe no disable)

**Separate view (Primary) > Inline in NetworkAndServers (Alt B)**:
- NetworkAndServers is already dense — adding relay rows inline would make it too long
- Separate view provides room for per-relay details, add/test actions
- Follows existing pattern: SMP/XFTP each have their own sub-views

**Following ProtocolServersView pattern**:
- Proven UX pattern in the app
- Users familiar with the interaction model
- Code reuse potential (test button, add dialog, row rendering)
- Same `tested: Bool?` status indicator pattern

---

## 7. Edge Cases

1. **Deleting a relay with active channels**: Backend checks `group_relays` references. If relay is used by active channels, mark as `deleted` but keep functional. UI should warn: "This relay is used by N channels. It will continue serving existing channels."

2. **Disabling all relays**: Allowed, but channel creation will be blocked (no enabled relays). Show info text: "Enable at least one relay to create channels."

3. **Adding duplicate relay**: Backend should reject duplicate addresses. Show error message.

4. **Invalid relay address**: Validate format before submitting. Show inline error for malformed addresses.

5. **Test timeout**: Relay test may take time. Show spinner on the row being tested. Timeout after reasonable period and mark as failed.

6. **Preset relay unavailable**: If a preset relay fails testing, show the failure but don't allow deletion. User can disable it and use alternatives.

7. **Network offline**: Test buttons disabled or tests fail immediately with "No network" error.

8. **Multiple users**: Relay configuration is per-user (follows `UserServer` pattern). Each user profile has independent relay settings.

---

## 8. Testing Notes

1. **Navigation**: Settings > Network & Servers > Chat relays — verify NavigationLink works
2. **Preset relays**: Verify 3 preset relays appear in "SimpleX Chat relays" section
3. **Add custom relay**: Add via manual entry — verify appears in "Your relays" section
4. **Add via QR**: Scan relay QR code — verify adds to list
5. **Delete custom**: Swipe-to-delete custom relay — verify removed
6. **Delete preset**: Verify preset relays cannot be deleted
7. **Enable/disable**: Toggle relay — verify visual state change
8. **Test single**: Test individual relay — verify checkmark/X appears
9. **Test all**: Tap "Test all relays" — verify all enabled relays tested, spinner shown
10. **Test failure**: Mock relay test failure — verify red X and alert
11. **Empty state**: Remove all custom relays, disable all presets — verify empty state message
12. **Per-user isolation**: Switch user profiles — verify independent relay configurations
