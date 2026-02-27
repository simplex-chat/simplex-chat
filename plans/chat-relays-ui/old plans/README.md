# Chat Relays UI — iOS Implementation Plans & Wireframes

## Context

Chat Relays enable large public channels where messages flow owner -> relay -> members, replacing N-to-N connections. The core backend is ~75% complete. This plan covers creating detailed implementation plans and ASCII wireframes for all iOS UI work items (launch plan sections 4.1-4.8) needed for MVP launch, saved to `plans/chat-relays-ui/`.

## Table of Contents

1. [Executive Summary](#1-executive-summary)
2. [Deliverables & Output Structure](#2-deliverables--output-structure)
3. [Key Design Decisions](#3-key-design-decisions)
4. [Per-Item Summaries with Wireframe Samples](#4-per-item-summaries)
5. [Dependency Map](#5-dependency-map)
6. [Verification Process](#6-verification-process)

---

## 1. Executive Summary

8 UI work items from launch plan §4.1-4.8 require detailed implementation plans and wireframes. Each plan will include: approach analysis, file-level change list, data flow diagrams, ASCII wireframes (primary + alternative designs), state variations, and implementation notes.

**Key architectural insight**: The existing codebase already has partial support for channels through `showGroupAsSender` (used in ChatView:1843 and ChatPreviewView:300), `GroupInfo.businessChat` conditional icon pattern, and `UserServer` management pattern in settings. The channel UI work largely extends existing patterns rather than creating new ones.

---

## 2. Deliverables & Output Structure

```
plans/chat-relays-ui/
  README.md                          — Index, dependency map, glossary (this file)
  01-api-type-updates.md             — §4.1: Swift type additions (FIRST — no wireframes)
  02-channel-visual-distinction.md   — §4.2: Chat list icons, labels, badges
  03-channel-message-display.md      — §4.3: CIChannelRcv rendering, "from channel"
  04-channel-creation-flow.md        — §4.4: Full creation wizard with relay selection
  05-relay-management-settings.md    — §4.5: User-level relay configuration
  06-channel-info-relays.md          — §4.6: Relay list in channel info screen
  07-relay-connection-state.md       — §4.7: Join progress with per-relay status
  08-owner-posting-ui.md             — §4.8: Compose mode for channel owners
```

**Launch plan traceability** (our §4.x → original launch plan §4.x):

| Our §  | File | Launch Plan § | Topic |
|--------|------|---------------|-------|
| §4.1 | 01 | §4.8 | API Type Updates |
| §4.2 | 02 | §4.1 | Channel Visual Distinction |
| §4.3 | 03 | §4.2 | Channel Message Display |
| §4.4 | 04 | §4.3 | Channel Creation Flow |
| §4.5 | 05 | §4.4 | Relay Management Settings |
| §4.6 | 06 | §4.5 | Show Relays in Channel Info |
| §4.7 | 07 | §4.6 | Relay Connection State |
| §4.8 | 08 | §4.7 | Owner Posting UI |

Each plan file (02-08) follows this template:
```
# Title
## Overview (what, why, user impact)
## Prerequisites & Dependencies
## Data Model (types involved)
## Implementation Plan (file-by-file changes)
## Wireframes
  ### Primary Design
  ### Alternative Design
  ### State Variations (loading, error, empty)
## Design Rationale (why primary > alternative)
## Edge Cases
## Testing Notes
```

---

## 3. Key Design Decisions

### 3.1 Channel Icon

**Decision**: `megaphone.fill` for channels

**Rationale**: Extends existing `chatIconName` pattern on `GroupInfo`:
- Groups: `person.2.circle.fill`
- Business: `briefcase.circle.fill`
- Channels (new): `megaphone.fill`

**Alternatives considered**:
- `antenna.radiowaves.left.and.right` — too technical, suggests radio/broadcast infrastructure
- `dot.radiowaves.left.and.right` — similar issue, also used for AirDrop
- `speaker.wave.2.fill` — suggests audio, not messaging

The icon change is in `GroupInfo.chatIconName`, which is already consumed by `ChatInfoImage`, `ChatPreviewView`, and `CIGroupInvitationView`.

### 3.2 Channel Creation Entry Point

**Decision**: Separate "Create channel" NavigationLink in `NewChatSheet` (line 120-128 of NewChatMenuButton.swift), placed after "Create group".

**Wireframe sample** (from §4.4 plan):
```
┌─────────────────────────────────┐
│  New message                    │
├─────────────────────────────────┤
│  🔗  Create 1-time link      > │
│  📷  Scan / Paste link        > │
│  👥  Create group             > │
│  📢  Create channel           > │
├─────────────────────────────────┤
│  📦  Archived contacts        > │
├─────────────────────────────────┤
│  YOUR CONTACTS                  │
│  ...                            │
└─────────────────────────────────┘
```

**Alternative considered**: Toggle inside AddGroupView (group/channel switch). Rejected because: (a) channels have fundamentally different creation flow (relay selection, no member invites), (b) mixing concepts confuses users, (c) channels deserve first-class entry point for discoverability.

### 3.3 Relay Management Placement

**Decision**: New "Chat relays" NavigationLink in `NetworkAndServers.swift`, placed after XFTP servers section, before advanced settings.

**Rationale**: Relays are infrastructure like SMP/XFTP servers. Users configure them at the same level. Follows `ProtocolServersView` / `UserServer` pattern for list management, test buttons, status indicators.

### 3.4 Channel Info View

**Decision**: Extend `GroupChatInfoView` with conditional sections (not a separate view).

**Rationale**: Channels ARE groups with `useRelays = true`. Follows existing `businessChat` conditional pattern. Adding a separate view would duplicate 80%+ of GroupChatInfoView logic.

**Changes to GroupChatInfoView**:
- Show relay section (when `useRelays`) instead of "Add members" button
- Show relay list with status indicators
- Hide "Invite members" button (channels don't have manual member invites)
- Potentially show member count (from relay-reported count, not loaded member list)

### 3.5 CIChannelRcv Direction

**Decision**: Add `.channelRcv` case to Swift `CIDirection` enum. Messages with this direction render through existing `showGroupAsSender` path.

**Key code path** (ChatView.swift:1829-1895):
```swift
// Existing code at line 1831:
if case let .groupRcv(member) = ci.chatDir,
   case let .group(groupInfo, _) = chat.chatInfo {
    // Shows member avatar + name
}
// New handling needed for .channelRcv:
// Shows group avatar + group name (uses showGroupAsSender path)
```

The `showGroupAsSender` rendering already handles this at lines 1843-1895 — shows group image, group name, "group" role label. `CIChannelRcv` items should flow through this path.

---

## 4. Per-Item Summaries

### §4.1 API Type Updates (FIRST)

**Scope**: Swift type additions/changes derived from Haskell diff — no wireframes.
**Complexity**: Medium

**New types**: `RelayStatus`, `GroupRelay`, `UserChatRelay`, `GroupShortLinkInfo`, `UserServersWarning`
**Modified types**: `CIDirection` (+channelRcv), `GroupMemberRole` (+relay), `GroupInfo` (+useRelays, +relayOwnStatus), `GroupProfile` (+groupLink), `User` (+userChatRelay), `UserOperatorServers` (+chatRelays), `UserServersError` (+relay cases), `GroupLinkPlan.ok` (+groupSLinkInfo_), `ChatErrorType` (+chatRelayExists)
**API changes**: `apiSendMessages` (+sendAsGroup), `apiForwardChatItems` (+sendAsGroup), `apiPrepareGroup` (+directLink), new `apiNewPublicGroup` command
**New responses/events**: `publicGroupCreated`, `groupLinkRelaysUpdated`, `userServersValidation` (+serverWarnings)

See `01-api-type-updates.md` for full details, wire formats, and Haskell↔Swift mapping table.

### §4.2 Channel Visual Distinction

**Scope**: Different icon/badge for channels in chat list; "Channel" label.
**Complexity**: Low
**Backend dependency**: None

**Files to modify**:
- `SimpleXChat/ChatTypes.swift` — Add `useRelays: BoolDef` to GroupInfo (from Haskell), update `chatIconName`
- `Shared/Views/ChatList/ChatPreviewView.swift` — Optional "Channel" subtitle or megaphone badge
- `Shared/Views/Chat/ChatInfoToolbar.swift` — Channel label in chat header

**Wireframe** (chat list row):
```
┌──────────────────────────────────────────┐
│ [📢]  SimpleX News        3:42 PM       │
│        Latest update about...     ●  3   │
└──────────────────────────────────────────┘
vs. regular group:
┌──────────────────────────────────────────┐
│ [👥]  Team Chat           3:42 PM       │
│        alice: Hey everyone...     ●  1   │
└──────────────────────────────────────────┘
```

### §4.3 "Message from Channel" Display

**Scope**: Handle `CIChannelRcv` in message rendering pipeline.
**Complexity**: Low-Medium
**Backend dependency**: None (CIChannelRcv exists in Haskell, just needs Swift side)

**Files to modify**:
- `SimpleXChat/ChatTypes.swift` — Add `.channelRcv` to `CIDirection`
- `Shared/Views/Chat/ChatView.swift` — Handle `.channelRcv` in message grouping/avatar logic
- `Shared/Views/ChatList/ChatPreviewView.swift` — Channel message preview (no sender prefix)

**Wireframe** (channel message in chat):
```
┌─ SimpleX News Channel ──────────────────┐
│                                          │
│  [📢 img]  SimpleX News                 │
│  ┌────────────────────────────────────┐  │
│  │ We're excited to announce v7.0!   │  │
│  │ New channel feature allows...     │  │
│  │                          3:42 PM ✓│  │
│  └────────────────────────────────────┘  │
│                                          │
│  [📢 img]  SimpleX News                 │
│  ┌────────────────────────────────────┐  │
│  │ Check out the blog post:          │  │
│  │ simplex.chat/blog/v7              │  │
│  │                          3:45 PM ✓│  │
│  └────────────────────────────────────┘  │
│                                          │
│  ─────── Reactions only ───────          │
│  (compose area hidden for observers)     │
└──────────────────────────────────────────┘
```

### §4.4 Channel Creation Flow

**Scope**: Full wizard: name/desc -> relay selection -> creation with status feedback.
**Complexity**: High
**Backend dependency**: §3.2 (relay connection state events)

**New files**:
- `Shared/Views/NewChat/AddChannelView.swift` — Main creation view
- `Shared/Views/NewChat/ChannelRelaySelectionView.swift` — Relay picker (optional, could be inline)

**Files to modify**:
- `Shared/Views/NewChat/NewChatMenuButton.swift` — Add "Create channel" entry (after line 127)
- `Shared/Model/SimpleXAPI.swift` — Add `apiNewPublicGroup` wrapper

**Flow**:
```
NewChatSheet
  └─> "Create channel"
      └─> AddChannelView (Step 1: Name + Image + Description)
          └─> "Create" button
              └─> AddChannelView (Step 2: Relay Status)
                  ├─ Relay 1: ● Invited → ● Accepted → ● Active
                  ├─ Relay 2: ● Invited → ● Accepted → ● Active
                  └─ Relay 3: ● Invited → ...
                  └─> "Done" (all active) → Navigate to channel
```

**Wireframe** (creation form):
```
┌─────────────────────────────────┐
│  Create channel                 │
├─────────────────────────────────┤
│         [  📷  ]                │
│      (channel image)            │
│                                 │
│  ✏️  Enter channel name...      │
│                                 │
│  ☑  Create channel              │
└─────────────────────────────────┘
```

**Wireframe** (relay status during creation):
```
┌─────────────────────────────────┐
│  Creating channel...            │
├─────────────────────────────────┤
│                                 │
│  relay1.simplex.im              │
│  ✅ Active                      │
│                                 │
│  relay2.simplex.im              │
│  🔄 Connecting...               │
│                                 │
│  relay3.simplex.im              │
│  ✅ Active                      │
│                                 │
│  [      Open channel      ]     │
│  (enabled when all active)      │
│                                 │
│  [      Skip waiting      ]     │
│  (dimmed, proceed with active)  │
└─────────────────────────────────┘
```

### §4.5 Relay Management (User Settings)

**Scope**: List relays; add/remove/edit; test connectivity.
**Complexity**: Medium
**Backend dependency**: §2.5 (APITestChatRelay)

**New files**:
- `Shared/Views/UserSettings/NetworkAndServers/ChatRelaysView.swift` — Relay list
- `Shared/Views/UserSettings/NetworkAndServers/ChatRelayView.swift` — Individual relay detail

**Files to modify**:
- `Shared/Views/UserSettings/NetworkAndServers/NetworkAndServers.swift` — Add relay section
- `SimpleXChat/ChatTypes.swift` — Add `UserChatRelay` type
- `Shared/Model/SimpleXAPI.swift` — Add relay API functions

**Pattern**: Follows `ProtocolServersView` exactly:
- List with preset/custom sections
- Swipe-to-delete for custom relays
- Test button with `tested: Bool?` status indicator (checkmark/X/blank)
- Add relay via text input or QR scan
- Enable/disable toggle per relay

**Wireframe** (relay list):
```
┌─────────────────────────────────┐
│  Chat relays                    │
├─────────────────────────────────┤
│  SIMPLEX CHAT RELAYS            │
│  ● relay1.simplex.im        ✅ │
│  ● relay2.simplex.im        ✅ │
│  ● relay3.simplex.im        ✅ │
│                                 │
│  YOUR RELAYS                    │
│  ● myrelay.example.com      ❌ │
│  + Add relay...                 │
│                                 │
│  [ Test all relays ]            │
│                                 │
│  Chat relays forward messages   │
│  in channels you create.        │
└─────────────────────────────────┘
```

### §4.6 Show Relays in Channel Info

**Scope**: Relay list with status in channel info screen.
**Complexity**: Medium
**Backend dependency**: None

**Files to modify**:
- `Shared/Views/Chat/Group/GroupChatInfoView.swift` — Add relay section, conditionally shown when `useRelays`

**Wireframe** (channel info, relay section):
```
┌─────────────────────────────────┐
│  SimpleX News                   │
│  📢  [channel image]            │
│  150 subscribers                │
├─────────────────────────────────┤
│  🔍 Search  │  🔇 Mute         │
├─────────────────────────────────┤
│  RELAYS                         │
│  relay1.simplex.im     ● Active │
│  relay2.simplex.im     ● Active │
│  relay3.simplex.im     ● Active │
│                                 │
│  Relays forward messages to     │
│  channel subscribers.           │
├─────────────────────────────────┤
│  ✏️  Edit channel             > │
│  👋  Welcome message          > │
│  ⚙️  Channel preferences     > │
├─────────────────────────────────┤
│  ...                            │
└─────────────────────────────────┘
```

### §4.7 Relay Connection State During Join

**Scope**: Progress feedback when joining a channel, shown inside ChatView above/instead of compose area.
**Complexity**: Medium
**Backend dependency**: §3.2 (relay connection state events)

**Files to modify**:
- `Shared/Views/Chat/ChatView.swift` — Connection progress component in compose area
- `Shared/Views/Chat/ComposeMessage/ComposeView.swift` — No changes (already hidden for observers)

**Wireframe** (join progress in ChatView):
```
┌──────────────────────────────────────────┐
│  < [📢] SimpleX News               ...  │
│         Channel                          │
├──────────────────────────────────────────┤
│                                          │
│  (chat area — welcome message etc.)      │
│                                          │
├──────────────────────────────────────────┤
│  📢 Connecting to channel...      2/3   │
│                                          │
│  ● relay1.simplex.im         Active      │
│  ○ relay2.simplex.im         Connecting  │
│  ● relay3.simplex.im         Active      │
│                                          │
│  ━━━━━━━━━━━━━━━━━━▒▒▒▒▒▒▒▒            │
└──────────────────────────────────────────┘
```

### §4.8 Owner Posting UI

**Scope**: Compose always sends as channel for MVP.
**Complexity**: Low
**Backend dependency**: None

**Files to modify**:
- `Shared/Views/Chat/ComposeMessage/ComposeView.swift` — Detect channel context, set `asGroup: true`
- `Shared/Model/SimpleXAPI.swift` — Ensure `apiSendMessages` passes `asGroup` parameter

**Wireframe** (owner compose):
```
┌─────────────────────────────────┐
│  SimpleX News Channel           │
│  ─────────────────────────────  │
│  (messages)                     │
│                                 │
│  ─────────────────────────────  │
│  📢 Posting as SimpleX News     │
│  ┌─────────────────────────┐    │
│  │ Type a message...     📎│    │
│  └─────────────────────────┘ ➤  │
└─────────────────────────────────┘
```

**Design note**: The label "Posting as [Channel Name]" appears above compose bar when user is owner in a channel. This makes the "message from channel" behavior visible to the owner.

---

## 5. Dependency Map

```
Can start immediately:
  §4.1 API Type Updates (should be done first — other items depend on types)
  §4.2 Channel Visual Distinction
  §4.3 "Message from Channel" Display
  §4.6 Show Relays in Channel Info
  §4.8 Owner Posting UI

Depends on §4.1 (types must exist):
  All other items use the new types

Depends on backend §3.2 (relay state events):
  §4.4 Channel Creation Flow
  §4.7 Relay Connection State During Join

Depends on backend §2.5 (test command):
  §4.5 Relay Management Settings

Recommended implementation order:
  1. §4.1 API Type Updates
  2. §4.2 Channel Visual Distinction
  3. §4.3 "Message from Channel" Display
  4. §4.8 Owner Posting UI
  5. §4.6 Show Relays in Channel Info
  6. §4.5 Relay Management Settings
  7. §4.4 Channel Creation Flow
  8. §4.7 Relay Connection State During Join
```

---

## 6. Verification Process

After creating all 9 files in `plans/chat-relays-ui/`:

1. **Completeness check**: Every launch plan §4.x item has a corresponding plan file
2. **File path verification**: All referenced source files exist in codebase
3. **Pattern verification**: All referenced code patterns (line numbers, function names) verified against actual source
4. **Type consistency**: Swift types match Haskell types (UserChatRelay, GroupRelay, RelayStatus, CIChannelRcv)
5. **Dependency consistency**: No circular dependencies, all prerequisites listed
6. **Wireframe completeness**: Primary + alternative + state variations for each view
7. **Adversarial self-review**: 2 consecutive clean passes on each file and on the full set
