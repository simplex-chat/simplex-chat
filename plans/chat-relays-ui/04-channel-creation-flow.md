# 04 — Channel Creation Flow (§4.4)

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

**What**: "Create channel" entry point in the New Chat sheet, leading to a creation wizard: name/image input -> creation with the user's enabled relays -> real-time relay status feedback -> navigate to channel. The wizard uses whichever relays are enabled in the user's relay configuration (see §4.5 Relay Management Settings for how users configure relays).

**Why**: Channel creation is fundamentally different from group creation: it requires relay server infrastructure, uses `APINewPublicGroup` instead of `APINewGroup`, and needs a relay connection status phase.

**User impact**: Users can create channels from the same place they create groups. The flow guides them through channel-specific setup and shows relay connection progress.

---

## 2. Prerequisites & Dependencies

- **§4.1 (API Type Updates)**: `apiNewPublicGroup` wrapper, `UserChatRelay`, `GroupRelay`, `RelayStatus` types must exist in Swift.
- **Backend §3.2 (Relay Connection State Events)**: Backend must emit per-relay status events during channel creation. Without this, the relay status step shows a spinner but no per-relay progress.
- **§4.5 (Relay Management)**: Relay configuration must exist so the user has relays available. If no relays configured, creation should show an appropriate message.
- **Backend `APINewPublicGroup` exists** (Controller.hs:513): `APINewPublicGroup {userId, incognito, relayIds :: NonEmpty Int64, groupProfile}`

---

## 3. Data Model

### 3.1 API Command

Haskell (`Controller.hs:513`):
```haskell
| APINewPublicGroup {userId :: UserId, incognito :: IncognitoEnabled, relayIds :: NonEmpty Int64, groupProfile :: GroupProfile}
```

Swift wrapper needed:
```swift
func apiNewPublicGroup(incognito: Bool, relayIds: [Int64], groupProfile: GroupProfile) throws -> GroupInfo
```

### 3.2 UserChatRelay

From `Operators.hs:262`:
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
    public var address: String
    public var name: String
    public var domains: [String]
    public var preset: Bool
    public var tested: Bool?
    public var enabled: Bool
    public var deleted: Bool

    public var id: Int64 { chatRelayId }
}
```

### 3.3 Relay Status Events

Backend emits events during channel creation showing relay progress. Expected response type (§3.2 — TBD):
```swift
// Placeholder — exact type depends on backend §3.2 implementation
case relayConnectionState(groupId: Int64, relays: [GroupRelay])
```

### 3.4 GroupRelay

From `Types.hs:995`:
```haskell
data GroupRelay = GroupRelay
  { groupRelayId :: Int64,
    groupMemberId :: Int64,
    userChatRelayId :: Int64,
    relayStatus :: RelayStatus,
    relayLink :: Maybe Text
  }
```

### 3.5 Creation Flow Data

```
User input:
  - displayName: String
  - image: UIImage? (optional)
  - incognito: Bool

System provides:
  - relayIds: [Int64] (from configured/preset UserChatRelays)

API call:
  apiNewPublicGroup(incognito: incognito, relayIds: relayIds, groupProfile: profile)

Response:
  GroupInfo (with useRelays = true, groupRelays populated)
```

---

## 4. Implementation Plan

### 4.1 `Shared/Views/NewChat/NewChatMenuButton.swift` — Add Entry Point

**Location**: `NewChatSheet.viewBody()` Section (lines 98-128), after "Create group" NavigationLink

**Change**: Add "Create channel" NavigationLink:
```swift
NavigationLink {
    AddChannelView()
        .navigationTitle("Create channel")
        .modifier(ThemedBackground(grouped: true))
        .navigationBarTitleDisplayMode(.large)
} label: {
    Label("Create channel", systemImage: "megaphone.fill")
}
```

Also update `sheetHeight` calculation (line 69) to account for the new row:
```swift
let sheetHeight: CGFloat = showArchive ? 650 : 575  // increased from 575/500
```

### 4.2 `Shared/Views/NewChat/AddChannelView.swift` — New File

**Pattern**: Based on `AddGroupView.swift` structure, with these differences:
- Shows relay info instead of incognito toggle (or in addition to)
- Calls `apiNewPublicGroup` instead of `apiNewGroup`
- After creation, shows relay status phase instead of member invite
- No "Add Members" step (channels don't have manual member adds)

**Structure**:
```swift
struct AddChannelView: View {
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    @Environment(\.dismiss) var dismiss: DismissAction

    @State private var chat: Chat?
    @State private var groupInfo: GroupInfo?
    @State private var profile = GroupProfile(displayName: "", fullName: "")
    @FocusState private var focusDisplayName
    @State private var showChooseSource = false
    @State private var showImagePicker = false
    @State private var showTakePhoto = false
    @State private var chosenImage: UIImage? = nil
    @State private var creationInProgress = false

    var body: some View {
        if let chat = chat, let groupInfo = groupInfo {
            // Phase 2: Show relay connection status
            ChannelRelayStatusView(chat: chat, groupInfo: groupInfo)
        } else {
            // Phase 1: Channel creation form
            createChannelView()
        }
    }

    func createChannelView() -> some View { ... }
    func createChannel() { ... }
}
```

**createChannel() function**:
```swift
func createChannel() {
    focusDisplayName = false
    creationInProgress = true
    Task {
        do {
            profile.displayName = profile.displayName.trimmingCharacters(in: .whitespaces)
            profile.groupPreferences = GroupPreferences(history: GroupPreference(enable: .on))

            // Get enabled relay IDs from ChatModel
            let relayIds = m.userChatRelays
                .filter { $0.enabled && !$0.deleted }
                .map { $0.chatRelayId }

            guard !relayIds.isEmpty else {
                // Show error: no relays configured
                return
            }

            let gInfo = try await apiNewPublicGroup(
                incognito: false,  // channels always non-incognito for MVP
                relayIds: relayIds,
                groupProfile: profile
            )

            await m.loadGroupMembers(gInfo)
            let c = Chat(chatInfo: .group(groupInfo: gInfo, groupChatScope: nil), chatItems: [])
            await MainActor.run {
                m.addChat(c)
                groupInfo = gInfo
                chat = c
                creationInProgress = false
            }
        } catch {
            await MainActor.run {
                creationInProgress = false
                AlertManager.shared.showAlert(
                    Alert(title: Text("Error creating channel"),
                          message: Text(responseError(error)))
                )
            }
        }
    }
}
```

### 4.3 `Shared/Views/NewChat/ChannelRelayStatusView.swift` — New File (Optional)

Shows relay connection progress after channel creation. Could be inline in `AddChannelView` or a separate view.

**Structure**:
```swift
struct ChannelRelayStatusView: View {
    var chat: Chat
    @State var groupInfo: GroupInfo

    var body: some View {
        List {
            Section {
                // Channel name + image
                channelHeader()
            }

            Section(header: Text("Chat relays")) {
                if let relays = groupInfo.groupRelays {
                    ForEach(relays) { relay in
                        relayStatusRow(relay)
                    }
                }
            } footer: {
                Text("Connecting to relays. Your channel will be ready when relays are active.")
            }

            Section {
                // "Open channel" button - enabled when all relays active
                Button("Open channel") {
                    dismissAllSheets(animated: true) {
                        ItemsModel.shared.loadOpenChat(groupInfo.id)
                    }
                }
                .disabled(!allRelaysActive)

                // "Skip waiting" button - enabled when at least one relay active
                if !allRelaysActive && someRelaysActive {
                    Button("Skip waiting") {
                        dismissAllSheets(animated: true) {
                            ItemsModel.shared.loadOpenChat(groupInfo.id)
                        }
                    }
                    .foregroundColor(theme.colors.secondary)
                }
            }
        }
        .onReceive(/* relay state change notifications */) { ... }
    }
}
```

### 4.4 `Shared/Model/SimpleXAPI.swift` — API Wrapper

**Add**:
```swift
func apiNewPublicGroup(incognito: Bool, relayIds: [Int64], groupProfile: GroupProfile) async throws -> GroupInfo {
    let userId = try currentUserId("apiNewPublicGroup")
    let r = await chatSendCmd(.apiNewPublicGroup(userId: userId, incognito: incognito, relayIds: relayIds, groupProfile: groupProfile))
    if case let .groupCreated(_, groupInfo) = r { return groupInfo }
    throw r
}
```

### 4.5 `Shared/Model/AppAPITypes.swift` — ChatCommand Extension

**Add** to `ChatCommand` enum:
```swift
case apiNewPublicGroup(userId: Int64, incognito: Bool, relayIds: [Int64], groupProfile: GroupProfile)
```

**Add** encoding in `cmdString`:
```swift
case let .apiNewPublicGroup(userId, incognito, relayIds, groupProfile):
    return "/_new public group \(userId) \(onOff(incognito)) \(relayIds.map(String.init).joined(separator: ",")) \(encodeJSON(groupProfile))"
```

---

## 5. Wireframes

### 5.1 Primary Design — Creation Form (Phase 1)

```
┌─────────────────────────────────┐
│  < Create channel               │
├─────────────────────────────────┤
│                                 │
│         ┌─────────┐            │
│         │         │            │
│         │  [cam]  │            │
│         │         │            │
│         └─────────┘            │
│      (tap to add image)        │
│                                 │
├─────────────────────────────────┤
│  Channel name                   │
│  ┌─────────────────────────┐   │
│  │ Enter channel name...   │   │
│  └─────────────────────────┘   │
│                                 │
│  [checkmark] Create channel     │
└─────────────────────────────────┘
```

### 5.2 Primary Design — Relay Status (Phase 2)

```
┌─────────────────────────────────┐
│  < Creating channel...          │
├─────────────────────────────────┤
│                                 │
│         ┌─────────┐            │
│         │ [image] │            │
│         └─────────┘            │
│       SimpleX News              │
│                                 │
├─────────────────────────────────┤
│  CHAT RELAYS                    │
│                                 │
│  relay1.simplex.im              │
│  Active                    [*]  │
│                                 │
│  relay2.simplex.im              │
│  Connecting...             [~]  │
│                                 │
│  relay3.simplex.im              │
│  Active                    [*]  │
│                                 │
├─────────────────────────────────┤
│                                 │
│  ┌─────────────────────────┐   │
│  │     Open channel        │   │
│  └─────────────────────────┘   │
│                                 │
│       Skip waiting              │
│                                 │
└─────────────────────────────────┘
```

### 5.3 Primary Design — All Relays Active

```
┌─────────────────────────────────┐
│  Channel created                │
├─────────────────────────────────┤
│                                 │
│         ┌─────────┐            │
│         │ [image] │            │
│         └─────────┘            │
│       SimpleX News              │
│                                 │
├─────────────────────────────────┤
│  CHAT RELAYS                    │
│                                 │
│  relay1.simplex.im              │
│  Active                    [*]  │
│                                 │
│  relay2.simplex.im              │
│  Active                    [*]  │
│                                 │
│  relay3.simplex.im              │
│  Active                    [*]  │
│                                 │
├─────────────────────────────────┤
│                                 │
│  ┌─────────────────────────┐   │
│  │     Open channel        │   │  <-- now enabled
│  └─────────────────────────┘   │
│                                 │
└─────────────────────────────────┘
```

### 5.4 Alternative Design A — Single-Page Form with Inline Relay Status

Instead of two phases, show relay status inline below the creation form. The form stays visible, and relays connect in background:

```
┌─────────────────────────────────┐
│  Create channel                 │
├─────────────────────────────────┤
│  [image]  SimpleX News          │
│                                 │
│  [checkmark] Create channel     │
│  (greyed out / already tapped)  │
├─────────────────────────────────┤
│  CONNECTING RELAYS              │
│  relay1.simplex.im   Active [*] │
│  relay2.simplex.im   ...    [~] │
│  relay3.simplex.im   Active [*] │
│                                 │
│  ┌─────────────────────────┐   │
│  │     Open channel        │   │
│  └─────────────────────────┘   │
└─────────────────────────────────┘
```

### 5.5 Alternative Design B — Modal Dialog for Relay Status

Pop up a modal sheet over the creation form showing relay progress:

```
         ┌───────────────────┐
         │  Connecting...    │
         │                   │
         │  relay1  Active   │
         │  relay2  ...      │
         │  relay3  Active   │
         │                   │
         │  [Open channel]   │
         └───────────────────┘
```

### 5.6 State Variations

**No relays configured**:
```
┌─────────────────────────────────┐
│  Create channel                 │
├─────────────────────────────────┤
│  [image picker]                 │
│  [name field]                   │
│                                 │
│  [checkmark] Create channel     │
│  (DISABLED)                     │
├─────────────────────────────────┤
│  CHAT RELAYS                    │
│                                 │
│  No relays configured.          │
│  Add relays in Settings >       │
│  Network & servers > Chat relays│
│                                 │
└─────────────────────────────────┘
```

**Relay connection error**:
```
│  relay2.simplex.im              │
│  Connection failed         [!]  │
│  Tap to retry                   │
```

**All relays failed**:
```
│  ┌─────────────────────────┐   │
│  │     Open channel        │   │  <-- disabled
│  └─────────────────────────┘   │
│                                 │
│  All relays failed to connect.  │
│  Check relay settings and       │
│  try again.                     │
│                                 │
│  [Delete channel]               │
```

**Creation API error**:
```
Alert: "Error creating channel"
Message: [error detail]
[OK]
```

---

## 6. Design Rationale

**Two-phase approach (Primary) > Single-page (Alt A)**:
- Clear state transition: user completes input, then waits for infrastructure
- Prevents accidental edits during relay connection
- Follows iOS wizard pattern (commit action -> show results)
- Similar to AddGroupView's transition to AddGroupMembersView

**Two-phase > Modal dialog (Alt B)**:
- Modal can be dismissed accidentally
- Two-phase gives more screen real estate for relay status
- Modal pattern is for quick confirmations, not multi-relay progress tracking

**No incognito toggle for channels**:
- Channel owner identity is shared with relay servers (necessary for relay protocol)
- Subscribers don't see owner identity (they see channel name via `showGroupAsSender`)
- Incognito makes less sense when owner is always hidden from subscribers

---

## 7. Edge Cases

1. **Empty relay list**: Disable "Create channel" button. Show explanation linking to relay settings.

2. **All relays disabled**: Same as empty — no enabled relays available for channel creation.

3. **Partial relay failure**: Some relays connect, others fail. "Open channel" enabled if at least one relay active. Show warning about reduced redundancy.

4. **Network loss during creation**: API call may fail. Show standard error alert with retry suggestion.

5. **Network loss during relay connection**: Relays stuck in "Connecting" state. Show timeout message after reasonable period. Allow "Open channel" with active relays.

6. **Channel name validation**: Same rules as group names — `validDisplayName()`. Trim whitespace.

7. **Duplicate channel name**: Backend allows duplicate names. No client-side uniqueness check needed.

8. **User dismisses sheet during relay connection**: Channel already created in backend. It appears in chat list with whatever relay state it has. Relays will continue connecting in background.

9. **Sheet height**: The new "Create channel" row adds ~44pt to the sheet. Update `sheetHeight` accordingly.

---

## 8. Testing Notes

1. **Entry point**: Verify "Create channel" appears in NewChatSheet after "Create group"
2. **Form validation**: Empty name -> button disabled. Valid name -> enabled
3. **Image picker**: Same behavior as group creation image picker
4. **API call**: Mock `apiNewPublicGroup` success -> transitions to phase 2
5. **API call failure**: Mock error -> alert shown, stays on form
6. **Relay status updates**: Mock relay state events -> UI updates in real-time
7. **Open channel**: Tapping "Open channel" -> dismisses all sheets, opens channel chat
8. **Skip waiting**: Available when some (not all) relays active -> opens channel
9. **No relays**: When `userChatRelays` is empty -> "Create channel" disabled or shows explanation
10. **Sheet height**: Verify sheet doesn't clip with the additional row on iOS 16+ with oneHandUI
