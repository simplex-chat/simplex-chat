# 01 — API Type Updates (§4.1)

**Implementation Priority: FIRST — all other UI items depend on these types.**

## Table of Contents
1. [Overview](#1-overview)
2. [Prerequisites & Dependencies](#2-prerequisites--dependencies)
3. [Data Model](#3-data-model)
4. [Implementation Plan](#4-implementation-plan)
5. [Type Mapping Reference](#5-type-mapping-reference)
6. [Edge Cases](#6-edge-cases)
7. [Testing Notes](#7-testing-notes)

---

## 1. Overview

**What**: Add all Swift type definitions needed for channel/relay UI. This includes new types (`UserChatRelay`, `GroupRelay`, `RelayStatus`), new enum cases (`CIDirection.channelRcv`), new fields on existing types (`GroupInfo.useRelays`, `GroupInfo.groupRelays`), and new API command/response wrappers.

**Why**: All other UI items (§4.2-4.8) depend on these types. This must be completed first to unblock parallel UI development.

**User impact**: None directly — pure type definitions. But enables all visible UI changes.

**No wireframes** — this is a data model/API item.

---

## 2. Prerequisites & Dependencies

- **Backend**: All Haskell types already exist. This is purely Swift-side mirroring.
- **No UI dependency** — can start immediately.
- **Blocks**: All other §4.x items.

---

## 3. Data Model

### 3.1 New Types to Add

#### UserChatRelay (from Operators.hs:262)

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

#### GroupRelay (from Types.hs:995)

```haskell
data GroupRelay = GroupRelay
  { groupRelayId :: Int64,
    groupMemberId :: Int64,
    userChatRelayId :: Int64,
    relayStatus :: RelayStatus,
    relayLink :: Maybe Text
  }
```

#### RelayStatus (from Types.hs:1004)

```haskell
data RelayStatus = RSNew | RSInvited | RSAccepted | RSActive
```

JSON encoding: `"new"`, `"invited"`, `"accepted"`, `"active"` (via textEncode/textDecode).

#### BoolDef (from Types.hs:2006)

```haskell
newtype BoolDef = BoolDef {isTrue :: Bool}
```

JSON: encoded as plain `Bool`. `omittedField = Just (BoolDef False)` — defaults to `false` when absent.

### 3.2 Existing Type Extensions

#### GroupInfo — new fields

```haskell
data GroupInfo = GroupInfo
  { ...
    useRelays :: BoolDef,       -- NEW
    ...
    groupRelays :: Maybe [GroupRelay],  -- NEW (may need verification)
    ...
  }
```

#### CIDirection — new case

```haskell
data CIDirection (c :: MsgDirectionI) where
  ...
  CIChannelRcv :: CIDirection 'MDRcv   -- NEW
```

#### GroupMemberRole — new case

```haskell
data GroupMemberRole = ... | GRRelay   -- NEW relay role
```

### 3.3 API Commands & Responses

#### New Commands

```haskell
| APINewPublicGroup {userId :: UserId, incognito :: IncognitoEnabled, relayIds :: NonEmpty Int64, groupProfile :: GroupProfile}
| APITestChatRelay Int64   -- test relay by chatRelayId
| APIGetUserChatRelays UserId
| APISetUserChatRelays UserId [UserChatRelay]
```

#### New/Modified Responses

```haskell
-- Existing response used for new command:
| CRGroupCreated User GroupInfo    -- used by both APINewGroup and APINewPublicGroup

-- Potentially new:
| CRUserChatRelays User [UserChatRelay]
| CRGroupRelayStatus GroupInfo GroupRelay   -- relay state change event
```

---

## 4. Implementation Plan

### 4.1 `SimpleXChat/ChatTypes.swift` — New Types

**Location**: Near existing group-related types

#### UserChatRelay

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

#### GroupRelay

```swift
public struct GroupRelay: Identifiable, Decodable, Equatable {
    public var groupRelayId: Int64
    public var groupMemberId: Int64
    public var userChatRelayId: Int64
    public var relayStatus: RelayStatus
    public var relayLink: String?

    public var id: Int64 { groupRelayId }
}
```

#### RelayStatus

```swift
public enum RelayStatus: String, Decodable, Equatable {
    case new = "new"
    case invited = "invited"
    case accepted = "accepted"
    case active = "active"

    public var displayText: String {
        switch self {
        case .new: return NSLocalizedString("New", comment: "relay status")
        case .invited: return NSLocalizedString("Invited", comment: "relay status")
        case .accepted: return NSLocalizedString("Accepted", comment: "relay status")
        case .active: return NSLocalizedString("Active", comment: "relay status")
        }
    }
}
```

### 4.2 `SimpleXChat/ChatTypes.swift` — GroupInfo Extension

**Location**: `GroupInfo` struct (~line 2334)

**Add fields**:
```swift
public struct GroupInfo: Identifiable, Decodable, NamedChat, Hashable {
    // ... existing fields ...
    public var useRelays: Bool = false          // decoded from BoolDef, defaults false
    public var groupRelays: [GroupRelay]? = nil // optional, may not be present
    // ... existing fields ...
}
```

**Note on BoolDef**: In JSON, `BoolDef` encodes as plain `Bool`. With `omittedField`, the field may be absent. Using `= false` default in Swift handles both cases (absent = false, present = decoded value).

**Note on Hashable**: `GroupRelay` must conform to `Hashable` since `GroupInfo` conforms to `Hashable`. Add `Hashable` to `GroupRelay`:
```swift
public struct GroupRelay: Identifiable, Decodable, Equatable, Hashable { ... }
```

### 4.3 `SimpleXChat/ChatTypes.swift` — chatIconName Update

**Location**: `chatIconName` computed property on `GroupInfo` (~line 2378)

**Change**:
```swift
public var chatIconName: String {
    if useRelays { return "megaphone.fill" }
    switch businessChat?.chatType {
    case .none: return "person.2.circle.fill"
    case .business: return "briefcase.circle.fill"
    case .customer: return "person.crop.circle.fill"
    }
}
```

### 4.4 `SimpleXChat/ChatTypes.swift` — CIDirection Extension

**Location**: `CIDirection` enum (~line 3432)

**Add case**:
```swift
public enum CIDirection: Decodable, Hashable {
    case directSnd
    case directRcv
    case groupSnd
    case groupRcv(groupMember: GroupMember)
    case channelRcv   // NEW
    case localSnd
    case localRcv
}
```

**Update Decodable init**:
```swift
// In CIDirection init(from:):
case "channelRcv": self = .channelRcv
```

**Update all computed properties** that switch on CIDirection:
- `sent` → `.channelRcv: return false`
- Any other switch statements — add `.channelRcv` case

### 4.5 `SimpleXChat/ChatTypes.swift` — GroupMemberRole Extension

**Location**: `GroupMemberRole` enum

**Add case** (if not already present):
```swift
public enum GroupMemberRole: String, Identifiable, CaseIterable, Comparable, Codable {
    // ... existing cases ...
    case relay = "relay"   // NEW — relay member role
}
```

**Ordering**: `GRRelay` should sort between observer and member in the role hierarchy, or at a separate level. Check Haskell ordering to match.

### 4.6 `Shared/Model/AppAPITypes.swift` — ChatCommand Extension

**Add commands**:

```swift
case apiNewPublicGroup(userId: Int64, incognito: Bool, relayIds: [Int64], groupProfile: GroupProfile)
case apiGetUserChatRelays(userId: Int64)
case apiSetUserChatRelays(userId: Int64, relays: [UserChatRelay])
case apiTestChatRelay(chatRelayId: Int64)
```

**Add cmdString encoding**:
```swift
case let .apiNewPublicGroup(userId, incognito, relayIds, groupProfile):
    return "/_new public group \(userId) \(onOff(incognito)) \(relayIds.map(String.init).joined(separator: ",")) \(encodeJSON(groupProfile))"

case let .apiGetUserChatRelays(userId):
    return "/_get user relays \(userId)"

case let .apiSetUserChatRelays(userId, relays):
    return "/_set user relays \(userId) \(encodeJSON(relays))"

case let .apiTestChatRelay(chatRelayId):
    return "/_test chat relay \(chatRelayId)"
```

**Note**: The exact command strings must match the Haskell `ChatCommand` parser. Verify against `Commands.hs` parser patterns.

### 4.7 `Shared/Model/AppAPITypes.swift` — ChatResponse Extension

**Add response cases** (if not already present):

```swift
case userChatRelays(user: User, relays: [UserChatRelay])
case groupRelayStatus(groupInfo: GroupInfo, relay: GroupRelay)
```

### 4.8 `Shared/Model/SimpleXAPI.swift` — API Wrapper Functions

```swift
func apiNewPublicGroup(incognito: Bool, relayIds: [Int64], groupProfile: GroupProfile) async throws -> GroupInfo {
    let userId = try currentUserId("apiNewPublicGroup")
    let r = await chatSendCmd(.apiNewPublicGroup(userId: userId, incognito: incognito, relayIds: relayIds, groupProfile: groupProfile))
    if case let .groupCreated(_, groupInfo) = r { return groupInfo }
    throw r
}

func apiGetUserChatRelays() async throws -> [UserChatRelay] {
    let userId = try currentUserId("apiGetUserChatRelays")
    let r = await chatSendCmd(.apiGetUserChatRelays(userId: userId))
    if case let .userChatRelays(_, relays) = r { return relays }
    throw r
}

func apiSetUserChatRelays(relays: [UserChatRelay]) async throws -> [UserChatRelay] {
    let userId = try currentUserId("apiSetUserChatRelays")
    let r = await chatSendCmd(.apiSetUserChatRelays(userId: userId, relays: relays))
    if case let .userChatRelays(_, updatedRelays) = r { return updatedRelays }
    throw r
}

func apiTestChatRelay(chatRelayId: Int64) async throws {
    let r = await chatSendCmd(.apiTestChatRelay(chatRelayId: chatRelayId))
    // Handle response — may return test result or error
}
```

### 4.9 `Shared/Model/ChatModel.swift` — Model Properties

**Add** relay storage to ChatModel:
```swift
@Published var userChatRelays: [UserChatRelay] = []
```

---

## 5. Type Mapping Reference

| Haskell Type | Module | Swift Type | Swift File |
|---|---|---|---|
| `UserChatRelay` | Operators.hs:262 | `UserChatRelay` | ChatTypes.swift |
| `GroupRelay` | Types.hs:995 | `GroupRelay` | ChatTypes.swift |
| `RelayStatus` | Types.hs:1004 | `RelayStatus` | ChatTypes.swift |
| `BoolDef` | Types.hs:2006 | `Bool` (with default) | ChatTypes.swift (inline) |
| `CIChannelRcv` | Messages.hs:290 | `.channelRcv` | ChatTypes.swift |
| `GRRelay` | Types/Shared.hs | `.relay` | ChatTypes.swift |
| `APINewPublicGroup` | Controller.hs:513 | `.apiNewPublicGroup` | AppAPITypes.swift |
| `GroupInfo.useRelays` | Types.hs:467 | `useRelays: Bool` | ChatTypes.swift |
| `GroupInfo.groupRelays` | Types.hs | `groupRelays: [GroupRelay]?` | ChatTypes.swift |

---

## 6. Edge Cases

1. **BoolDef absent in JSON**: `useRelays` field missing → defaults to `false`. This handles backward compatibility with older backends.

2. **groupRelays absent**: `groupRelays` field missing → defaults to `nil`. UI shows "Loading..." or empty state for relay sections.

3. **CIDirection unknown tag**: If an even newer backend sends a direction tag the client doesn't know, the `CIDirection` decoder should fall through to `CIInvalidJSON`. Ensure the decoder has a default case or the parent `ChatItem` handles decode failure.

4. **RelayStatus unknown value**: If backend adds new statuses (e.g., "error", "disconnected"), the `String` rawValue decoder will fail. Add an `unknown` case or handle gracefully:
    ```swift
    public init(from decoder: Decoder) throws {
        let value = try decoder.singleValueContainer().decode(String.self)
        self = RelayStatus(rawValue: value) ?? .new  // fallback to .new
    }
    ```

5. **GroupMemberRole ordering**: `GRRelay` must sort correctly in the role hierarchy. Verify Haskell `Ord` instance matches Swift `Comparable`.

6. **Hashable conformance**: `GroupInfo` is `Hashable`. New fields (`useRelays`, `groupRelays`) must be hashable. `Bool` and `[GroupRelay]?` are hashable if `GroupRelay` is.

7. **Encodable for UserChatRelay**: If `apiSetUserChatRelays` sends relays as JSON, `UserChatRelay` needs `Encodable` (or `Codable`).

---

## 7. Testing Notes

1. **JSON decode tests**: Add to `JSONTests.hs` (or Swift JSON tests):
   - `UserChatRelay` round-trip
   - `GroupRelay` decode
   - `RelayStatus` decode for all 4 values
   - `CIDirection` decode with `"channelRcv"`
   - `GroupInfo` decode with and without `useRelays` field
   - `GroupInfo` decode with and without `groupRelays` field

2. **Default values**: Test `GroupInfo` decode without `useRelays` → `false`

3. **Unknown RelayStatus**: Test decode with unknown string → graceful fallback

4. **CIDirection exhaustiveness**: Build with all warnings — verify no missing cases in switch statements

5. **API command strings**: Verify `cmdString` output matches Haskell parser expectations for each new command

6. **Type conformances**: Verify `Identifiable`, `Decodable`, `Equatable`, `Hashable` compile correctly for all new types

7. **ChatModel integration**: Verify `userChatRelays` property updates correctly when API response received
