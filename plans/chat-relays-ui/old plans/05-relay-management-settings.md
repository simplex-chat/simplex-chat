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

**What**: Integrate chat relay management into the existing Network & Servers settings hierarchy. Preset relays appear in each operator's view (alongside SMP and XFTP servers). Custom relays appear in "Your servers". Follows established `UserServer` / `ProtocolServerViewLink` patterns exactly.

**Why**: Relays are operator infrastructure like SMP/XFTP servers — they belong in the same management hierarchy, not in a separate view.

**User impact**: Relay management feels natural alongside existing server configuration. No new navigation patterns to learn.

---

## 2. Prerequisites & Dependencies

- **§4.1 (API Type Updates)**: `UserChatRelay` type, `chatRelays` field on `UserOperatorServers`.
- **Backend `APITestChatRelay`** (§2.5): For test button. Can stub if not ready.
- **Preset relays in `Operators/Presets.hs`**: Real addresses needed (§2.6).
- **`chatRelays` already on `UserOperatorServers`**: Field exists (`[UserChatRelay]? = nil`), loaded/saved alongside `smpServers`/`xftpServers`.

---

## 3. Data Model

### Existing Hierarchy

```
UserOperatorServers
├── operator: ServerOperator?     // nil = "Your servers"
├── smpServers: [UserServer]
├── xftpServers: [UserServer]
└── chatRelays: [UserChatRelay]?  // NEW — same level as SMP/XFTP
```

Each operator's `UserOperatorServers` contains its preset relays (e.g., SimpleX Chat preset relays). The `operator == nil` slot contains user-added custom relays.

### UserChatRelay (already implemented in §4.1)

```swift
struct UserChatRelay: Identifiable, Codable, Equatable, Hashable {
    var chatRelayId: Int64?
    var address: String
    var name: String
    var domains: [String]
    var preset: Bool
    var tested: Bool?       // nil=untested, true=pass, false=fail
    var enabled: Bool
    var deleted: Bool
    var createdAt = Date()
    // id, ==, CodingKeys — matching UserServer pattern
}
```

### Relay Test Status Display

Same as `UserServer`:
```
tested == nil   → no icon (untested)
tested == true  → green checkmark
tested == false → red X
enabled == false → slash.circle in secondary
```

---

## 4. Implementation Plan

### 4.1 `OperatorView.swift` — Add Relay Sections

Add "Chat relays" and "Added chat relays" sections after the XFTP sections. Pattern: identical to how "Message servers" (preset SMP) and "Added message servers" (custom SMP) are rendered.

```
Existing OperatorView sections:
  Operator info + use toggle
  Use for messages (SMP roles)
  Message servers (preset SMP)
  Added message servers (custom SMP)
  Use for files (XFTP roles)
  Media & file servers (preset XFTP)
  Added media & file servers (custom XFTP)
  Test all

New sections (after XFTP, before Test all):
  Chat relays (preset relays, where preset && !deleted)
  Added chat relays (custom relays under operator, where !preset && !deleted)
```

Relay rows use a new `ChatRelayViewLink` (analogous to `ProtocolServerViewLink`) that navigates to `ChatRelayView` (analogous to `ProtocolServerView`).

### 4.2 `YourServersView` (in ProtocolServersView.swift) — Add Relay Section

Add "Chat relays" section after XFTP section. Shows custom relays from the `operator == nil` slot. Swipe-to-delete, same pattern as `deleteSMPServer`/`deleteXFTPServer`.

### 4.3 `ChatRelayViewLink` — New View (in ProtocolServersView.swift or new file)

Analogous to `ProtocolServerViewLink`. NavigationLink row showing:
- Relay name (primary text)
- Relay domains (caption, secondary)
- Status icon: `showTestStatus` / duplicate/disabled indicators

### 4.4 `ChatRelayView` — New View

Analogous to `ProtocolServerView`. Two modes:
- **Preset**: Read-only address, test + enable toggle
- **Custom**: Editable address, test + enable toggle + delete

**Note**: Unlike `ProtocolServerView` (which has no delete button — deletion is via swipe in parent list), `ChatRelayView` adds an explicit "Delete relay" button in custom mode for clarity. This is a minor deviation from the pattern.

### 4.5 `TestServersButton` — Extend

`TestServersButton` currently tests SMP and XFTP servers. Extend to also test chat relays. Add `chatRelays: Binding<[UserChatRelay]>?` parameter (optional, nil when no relays). Uses `apiTestChatRelay` (new API, analogous to `testProtoServer`).

### 4.6 `EditButton` Toolbar — Extend

Both `OperatorView` and `YourServersView` show `EditButton()` only when non-preset/non-deleted SMP or XFTP servers exist. Extend visibility condition to also check for non-preset/non-deleted relays.

### 4.7 Add Relay Flow

Extend `YourServersView`'s existing "Add server" confirmationDialog (currently: "Enter server manually" / "Scan server QR code") with a third option "Add chat relay". This navigates to a relay-specific address entry view. Alternatively, add a separate "Add relay" button in the relay section. QR scan via existing `ScanProtocolServer`, extended to detect relay addresses.

### 4.8 Validation

`validateServers_` already handles `UserServersError.duplicateChatRelayName` and `duplicateChatRelayAddress`. The relay section rows should show duplicate indicators same as SMP/XFTP.

**Note**: Relay duplicate errors currently fall into the `default: return nil` arm of `globalError`/`globalSMPError`/`globalXFTPError`, so they don't produce global banner errors. A `globalChatRelayError` computed property may be needed, or relay errors can remain inline-only.

---

## 5. Wireframes

### 5.1 OperatorView — With Relay Section

Shown inside an operator view (e.g., "SimpleX Chat servers"). New relay sections appear after XFTP servers.

```
┌──────────────────────────────────────┐
│  < SimpleX Chat servers              │
├──────────────────────────────────────┤
│  OPERATOR                            │
│  Use servers                  [ON ]  │
│                                      │
├──────────────────────────────────────┤
│  USE FOR MESSAGES                    │
│  To receive                   [ON ]  │
│  For private routing          [ON ]  │
│                                      │
├──────────────────────────────────────┤
│  MESSAGE SERVERS                     │
│  ✅  smp1.simplex.im                 │
│  ✅  smp2.simplex.im                 │
│                                      │
├──────────────────────────────────────┤
│  USE FOR FILES                       │
│  To send                      [ON ]  │
│                                      │
├──────────────────────────────────────┤
│  MEDIA & FILE SERVERS                │
│  ✅  xftp1.simplex.im                │
│  ✅  xftp2.simplex.im                │
│                                      │
├──────────────────────────────────────┤
│  CHAT RELAYS                         │
│  ✅  relay1.simplex.im               │
│      simplex.im                      │
│  ✅  relay2.simplex.im               │
│      simplex.im                      │
│  ✅  relay3.simplex.im               │
│      simplex.im                      │
│                                      │
│  Relays forward messages in          │
│  channels you create.                │
│                                      │
├──────────────────────────────────────┤
│  Test servers                        │
└──────────────────────────────────────┘
```

**Notes**:
- "CHAT RELAYS" section placed after XFTP, before "Test servers"
- Each relay row shows name (primary) + domains (caption) + test status icon
- Relay rows navigate to `ChatRelayView` on tap (like SMP/XFTP rows → `ProtocolServerView`)
- No relay roles toggle (unlike SMP/XFTP which have storage/proxy roles)
- Footer explains relay purpose
- If user adds custom relays under this operator, an "ADDED CHAT RELAYS" section appears below (same pattern as "Added message servers" / "Added media & file servers")

### 5.2 YourServersView — With Relay Section

"Your servers" view shows custom relays alongside custom SMP/XFTP servers.

```
┌──────────────────────────────────────┐
│  < Your servers                      │
├──────────────────────────────────────┤
│  MESSAGE SERVERS                     │
│  ✅  mysmp.example.com               │
│                                      │
├──────────────────────────────────────┤
│  MEDIA & FILE SERVERS                │
│  ✅  myxftp.example.com              │
│                                      │
├──────────────────────────────────────┤
│  CHAT RELAYS                         │
│  ❌  myrelay.example.com             │
│      example.com                     │
│                                      │
├──────────────────────────────────────┤
│  Add server...                       │
│                                      │
├──────────────────────────────────────┤
│  Test servers                        │
│  How to use your servers          >  │
└──────────────────────────────────────┘
```

**Notes**:
- Custom relays in "CHAT RELAYS" section, same level as message/file servers
- Swipe-to-delete on relay rows (same as custom SMP/XFTP)
- "Add server..." dialog gains a relay option (see §5.4)

### 5.3 ChatRelayView — Individual Relay Detail

Follows `ProtocolServerView` pattern.

**Preset relay** (read-only address):
```
┌──────────────────────────────────────┐
│  < relay1.simplex.im                 │
├──────────────────────────────────────┤
│  PRESET RELAY                        │
│  relay1.simplex.im                   │
│  Domains: simplex.im                 │
│                                      │
├──────────────────────────────────────┤
│  USE RELAY                           │
│  Test relay                     ✅   │
│  Use for new channels         [ON ]  │
│                                      │
└──────────────────────────────────────┘
```

**Custom relay** (editable):
```
┌──────────────────────────────────────┐
│  < myrelay.example.com              │
├──────────────────────────────────────┤
│  YOUR RELAY ADDRESS                  │
│  ┌──────────────────────────────┐   │
│  │ simplex:/...                 │   │
│  └──────────────────────────────┘   │
│                                      │
├──────────────────────────────────────┤
│  USE RELAY                           │
│  Test relay                     ❌   │
│  Use for new channels         [ON ]  │
│                                      │
├──────────────────────────────────────┤
│  ┌──────────────────────────────┐   │
│  │                              │   │
│  │        [QR Code]             │   │
│  │                              │   │
│  └──────────────────────────────┘   │
│                                      │
├──────────────────────────────────────┤
│  🗑  Delete relay                    │
│                                      │
└──────────────────────────────────────┘
```

### 5.4 Add Server Dialog — Extended

Existing "Add server" confirmationDialog (currently: "Enter server manually" / "Scan server QR code") gains a third option:

```
         ┌──────────────────────────┐
         │  Add server              │
         │                          │
         │  Enter server manually   │
         │  Add chat relay          │
         │  Scan server QR code     │
         │                          │
         │  Cancel                  │
         └──────────────────────────┘
```

- "Enter server manually" → existing `NewServerView` (SMP/XFTP address entry, unchanged)
- "Add chat relay" → relay-specific address entry view (new)
- "Scan server QR code" → existing `ScanProtocolServer`, extended to detect relay addresses

### 5.5 Alt A — Separate ChatRelaysView

Instead of integrating into OperatorView/YourServersView, a standalone "Chat relays" NavigationLink in NetworkAndServers:

```
NetworkAndServers:
├── SimpleX Chat        >  (OperatorView — SMP + XFTP only)
├── Flux                >  (OperatorView — SMP + XFTP only)
├── Your servers        >  (YourServersView — SMP + XFTP only)
├── Chat relays         >  (NEW standalone view)
│   └── ChatRelaysView
│       ├── SIMPLEX CHAT RELAYS (preset)
│       ├── YOUR RELAYS (custom)
│       ├── Add relay...
│       └── Test relays
├── Advanced settings   >
└── Save servers
```

### 5.6 State Variation — Testing in Progress

Same pattern as existing server testing — dimmed list with spinner overlay:

```
┌──────────────────────────────────────┐
│  (dimmed, non-interactive)           │
│                                      │
│            ┌──────┐                  │
│            │ [~~] │  Testing...      │
│            └──────┘                  │
│                                      │
│  CHAT RELAYS                         │
│  ✅  relay1.simplex.im               │
│      relay2.simplex.im               │
│      relay3.simplex.im               │
│                                      │
└──────────────────────────────────────┘
```

### 5.7 State Variation — Operator Disabled

When operator toggle "Use servers" is OFF, all server sections (including relays) are hidden. Same existing behavior — `OperatorView` conditionally shows sections only when `operator.enabled`.

---

## 6. Design Rationale

### Integrated into operator views (Primary) > Standalone ChatRelaysView (Alt A)

- Relays are operator infrastructure, same category as SMP/XFTP
- `chatRelays` field already lives on `UserOperatorServers` alongside `smpServers`/`xftpServers`
- Keeps operator views as single source of truth for all operator services
- No new NavigationLink in the already-dense NetworkAndServers root
- Users configure everything per-operator in one place
- Follows principle: don't introduce new navigation patterns when existing ones work

### Following ProtocolServerViewLink / ProtocolServerView pattern

- Proven UX pattern in the app
- Same `tested: Bool?` status indicator
- Same preset/custom distinction
- Same soft-delete lifecycle
- Same test button + enable toggle

### Relay rows show name + domains (not raw address)

- `UserChatRelay` has `name` and `domains` fields (unlike `UserServer` which only has `server` address string)
- Name is more readable than the `simplex:/a/...` address
- Domains shown as caption (like subtitle) for context
- Matches how operators show `serverDomains`

---

## 7. Edge Cases

1. **chatRelays is nil**: Guard on optional — show no relay section if nil. Backend may not yet return relays for older protocol versions.
2. **Deleting relay used by channels**: Backend checks references. UI warns: "This relay is used by active channels."
3. **Disabling all relays across all operators**: `UserServersWarning.noChatRelays` is a soft warning (not a hard error). Currently warnings are not yet processed in `validateServers_` (`// TODO [relays] process warnings`). When implemented, this should show a warning banner but not prevent saving.
4. **Adding duplicate relay**: `UserServersError.duplicateChatRelayAddress` — show inline error indicator on the row (same as duplicate SMP/XFTP).
5. **Test timeout**: Spinner on row being tested. Timeout → mark as failed.
6. **Preset relay failing test**: Show failure but don't allow deletion. User can disable.
7. **Multiple operators with relays**: Each operator's relays appear in their own OperatorView. This is the natural per-operator grouping.

---

## 8. Testing Notes

1. **Operator view**: Verify relay section appears after XFTP in OperatorView when operator has relays
2. **Your servers**: Verify relay section appears in YourServersView for custom relays
3. **Preset relays**: Cannot delete, can disable, can test
4. **Custom relays**: Can delete (swipe), can disable, can test, can edit address
5. **Add relay**: "Add server" dialog includes "Chat relay" option
6. **Test single relay**: Test button in ChatRelayView — checkmark/X appears
7. **Test all**: "Test servers" button tests SMP + XFTP + relays
8. **Operator disabled**: Relay section hidden when operator toggle is OFF
9. **chatRelays nil**: No relay section shown, no crash
10. **Validation**: Duplicate relay name/address shows inline error
11. **Save flow**: Relay changes included in "Save servers" alongside SMP/XFTP changes
