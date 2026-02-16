# 01 — API Type Updates (§4.1)

**Implementation Priority: FIRST — all other UI items depend on these types.**

## Table of Contents
1. [Overview](#1-overview)
2. [New Types](#2-new-types)
3. [Existing Type Changes](#3-existing-type-changes)
4. [API Command Changes](#4-api-command-changes)
5. [API Response/Event Changes](#5-api-responseevent-changes)
6. [Type Mapping Reference](#6-type-mapping-reference)
7. [Edge Cases](#7-edge-cases)

---

## 1. Overview

Swift type additions and modifications to mirror Haskell changes in the `chat-relays` branch. Derived from `git diff master..HEAD` — no invented types.

**Scope**: Types needed for channel UI: "as group" chat items, sending/forwarding as group, creating/joining channels with relays, managing chat relays. Excludes: protocol-only types (`GroupRelayInvitation`, `RelayRequestData`), key types (`GroupRootKey`, `GroupKeys`, `memberPubKey`).

**Files**:
- `SimpleXChat/ChatTypes.swift` — new types, field additions
- `Shared/Model/AppAPITypes.swift` — command/response changes
- `Shared/Model/SimpleXAPI.swift` — API wrapper changes

---

## 2. New Types

### 2.1 RelayStatus

Haskell (`Types.hs`): `data RelayStatus = RSNew | RSInvited | RSAccepted | RSActive`

```swift
public enum RelayStatus: String, Decodable, Equatable, Hashable {
    case rsNew = "new"
    case rsInvited = "invited"
    case rsAccepted = "accepted"
    case rsActive = "active"
}
```

JSON: `"new"`, `"invited"`, `"accepted"`, `"active"` (via `enumJSON $ dropPrefix "RS"`).

### 2.2 GroupRelay

Haskell (`Types.hs`):
```haskell
data GroupRelay = GroupRelay
  { groupRelayId :: Int64,
    groupMemberId :: GroupMemberId,
    userChatRelayId :: Int64,
    relayStatus :: RelayStatus,
    relayLink :: Maybe ShortLinkContact  -- serialized as String
  }
```

```swift
public struct GroupRelay: Identifiable, Decodable, Equatable, Hashable {
    public var groupRelayId: Int64
    public var groupMemberId: Int64
    public var userChatRelayId: Int64
    public var relayStatus: RelayStatus
    public var relayLink: String?
    public var id: Int64 { groupRelayId }
}
```

### 2.3 UserChatRelay

Haskell (`Operators.hs`):
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

```swift
public struct UserChatRelay: Identifiable, Codable, Equatable, Hashable {
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

Needs `Codable` (not just `Decodable`) because it's sent back to backend in `UserOperatorServers`.

### 2.4 GroupShortLinkInfo

Haskell (`Controller.hs`):
```haskell
data GroupShortLinkInfo = GroupShortLinkInfo
  { direct :: Bool,
    groupRelays :: [ShortLinkContact],
    sharedGroupId :: Maybe B64UrlByteString
  }
```

```swift
public struct GroupShortLinkInfo: Decodable, Hashable {
    public var direct: Bool
    public var groupRelays: [String]
    public var sharedGroupId: String?
}
```

Used in `GroupLinkPlan.ok` to distinguish direct groups from relay channels during connect plan.

### 2.5 UserServersWarning

Haskell (`Operators.hs`):
```haskell
data UserServersWarning = USWNoChatRelays {user :: Maybe User}
```

```swift
public enum UserServersWarning: Decodable {
    case noChatRelays(user: UserRef?)
}
```

JSON tag prefix: `USW` → `noChatRelays`.

---

## 3. Existing Type Changes

### 3.1 CIDirection — add `.channelRcv`

Haskell (`Messages.hs`): adds `CIChannelRcv :: CIDirection 'MDRcv` (no associated `GroupMember`).

Current Swift (`ChatTypes.swift:3432`): 6 cases, synthesized `Decodable`, no custom `init(from:)`.

**Change**: Add case + update all switches:
```swift
public enum CIDirection: Decodable, Hashable {
    // existing...
    case channelRcv   // NEW — no associated member
}
```

Switches to update:
- `sent` → `.channelRcv: return false`
- `sameDirection` → `.channelRcv` matches `.channelRcv` only

Since `CIDirection` uses synthesized `Decodable`, the new case decodes automatically from `{"channelRcv":{}}` JSON.

### 3.2 GroupMemberRole — add `.relay` and unknown handling

Haskell (`Types/Shared.hs`): adds `GRRelay` (between observer and author) and `GRUnknown Text` for forward compat.

Current Swift (`ChatTypes.swift:2719`): `String` rawValue enum, `Comparable` via `comparisonValue`.

**Change**:
```swift
public enum GroupMemberRole: String, Identifiable, CaseIterable, Comparable, Codable, Hashable {
    case relay = "relay"       // NEW — below observer
    case observer
    case author
    // ... existing ...
}
```

Update `comparisonValue`: relay = 0, observer = 1, author = 2, member = 3, moderator = 4, admin = 5, owner = 6.

For `GRUnknown`: Add custom `init(from:)` that falls back to `.relay` (or a new `.unknown` case) for unrecognized strings, and custom `encode(to:)` that stores the original raw string for round-trip fidelity. Alternatively, rely on parent decode failure handling (`CIInvalidJSON`), but this degrades the whole chat item.

### 3.3 GroupInfo — add `useRelays` and `relayOwnStatus`

Haskell (`Types.hs`):
```haskell
data GroupInfo = GroupInfo
  { groupId :: GroupId,
    useRelays :: BoolDef,           -- NEW
    relayOwnStatus :: Maybe RelayStatus, -- NEW
    localDisplayName :: GroupName,
    ...
  }
```

Current Swift (`ChatTypes.swift:2334`): no `useRelays`, no `relayOwnStatus`.

**Change**: Add fields to `GroupInfo`:
```swift
public var useRelays: Bool       // BoolDef → Bool (always present from local backend)
public var relayOwnStatus: RelayStatus?  // relay's own status in this group
```

**BoolDef handling**: `BoolDef` is a Haskell newtype for remote desktop compatibility (defaults to `false` when absent in JSON). The local backend always serializes it as a plain `Bool`, so Swift uses `Bool` directly.

**Note**: `GroupInfo` does NOT have `groupRelays: [GroupRelay]?`. The old plan invented this field. Instead, relays come via `CRPublicGroupCreated` response and `CEvtGroupLinkRelaysUpdated` event as separate parameters.

### 3.4 GroupProfile — add `groupLink`

Haskell (`Types.hs`): adds `groupLink :: Maybe ShortLinkContact` between `image` and `groupPreferences`.

Current Swift (`ChatTypes.swift:2412`): has explicit `init`, no `groupLink`.

**Change**: Add field + update init:
```swift
public var groupLink: String?    // NEW — ShortLinkContact serialized as String
```

Update the explicit `init` to include `groupLink: String? = nil`.

### 3.5 User — add `userChatRelay`

Haskell (`Types.hs`): adds `userChatRelay :: BoolDef` to `User`.

Current Swift (`ChatTypes.swift:23`): no `userChatRelay`.

**Change**:
```swift
public var userChatRelay: Bool   // BoolDef → Bool
```

### 3.6 UserOperatorServers — add `chatRelays`

Haskell (`Operators.hs`): adds `chatRelays :: [UserChatRelay]` to `UserOperatorServers`.

Current Swift (`AppAPITypes.swift:1705`): `operator`, `smpServers`, `xftpServers` only.

**Change**:
```swift
struct UserOperatorServers: Identifiable, Equatable, Codable {
    var `operator`: ServerOperator?
    var smpServers: [UserServer]
    var xftpServers: [UserServer]
    var chatRelays: [UserChatRelay]?   // NEW — nil when absent for compat
}
```

### 3.7 UserServersError — add relay cases

Haskell (`Operators.hs`): adds two cases:
```haskell
| USEDuplicateChatRelayName {duplicateChatRelay :: Text}
| USEDuplicateChatRelayAddress {duplicateChatRelay :: Text, duplicateAddress :: ShortLinkContact}
```

Current Swift (`AppAPITypes.swift:1748`): 4 cases.

**Change**: Add:
```swift
case duplicateChatRelayName(duplicateChatRelay: String)
case duplicateChatRelayAddress(duplicateChatRelay: String, duplicateAddress: String)
```

### 3.8 GroupLinkPlan — add `groupSLinkInfo_`

Haskell (`Controller.hs`): `GLPOk` gains first field `groupSLinkInfo_ :: Maybe GroupShortLinkInfo`.

Current Swift (`AppAPITypes.swift:1328`): `case ok(groupSLinkData_: GroupShortLinkData?)`.

**Change**:
```swift
case ok(groupSLinkInfo_: GroupShortLinkInfo?, groupSLinkData_: GroupShortLinkData?)
```

### 3.9 ChatErrorType — add `chatRelayExists`

Haskell (`Controller.hs`): adds `CEChatRelayExists` between `CEUserExists` and `CEDifferentActiveUser`.

**File**: `SimpleXChat/APITypes.swift` (line 716), NOT `AppAPITypes.swift`.

**Change**: Add between `userExists` (line 723) and `differentActiveUser` (line 725):
```swift
case chatRelayExists
```

### 3.10 StoreError — add 5 relay-related variants

Haskell (`Store/Shared.hs`): adds 5 new `StoreError` constructors. JSON via `sumTypeJSON $ dropPrefix "SE"`.

**File**: `SimpleXChat/APITypes.swift` (line 795).

**Change**: Add:
```swift
case relayUserNotFound
case duplicateMemberId
case userChatRelayNotFound(chatRelayId: Int64)
case groupRelayNotFound(groupRelayId: Int64)
case groupRelayNotFoundByMemberId(groupMemberId: Int64)
```

`StoreError` uses synthesized `Decodable` with no fallback — missing cases cause the entire `ChatError` to fail decoding. These must be added.

---

## 4. API Command Changes

### 4.1 apiSendMessages — add `sendAsGroup`

Haskell: `SendRef` gains `ShowGroupAsSender` on `SRGroup`. Wire: `/_send #<id>(as_group=on) live=...`

The `asGroupP` parser: `("(as_group=" *> onOffP <* ')') <|> pure False` — parens, no space, appended directly to ref.

Current Swift (`AppAPITypes.swift:46`):
```swift
case apiSendMessages(type: ChatType, id: Int64, scope: GroupChatScope?, live: Bool, ttl: Int?, composedMessages: [ComposedMessage])
```

**Change**: Add `sendAsGroup: Bool` parameter:
```swift
case apiSendMessages(type: ChatType, id: Int64, scope: GroupChatScope?, sendAsGroup: Bool, live: Bool, ttl: Int?, composedMessages: [ComposedMessage])
```

Update `cmdString` (`AppAPITypes.swift:231`):
```swift
let asGroup = sendAsGroup ? "(as_group=on)" : ""
return "/_send \(ref(type, id, scope: scope))\(asGroup) live=\(onOff(live)) ttl=\(ttlStr) json \(msgs)"
```

Note: `(as_group=on)` is appended directly to ref string with NO space.

Update `apiSendMessages` wrapper (`SimpleXAPI.swift:527`) and all callers to pass `sendAsGroup: false` by default.

### 4.2 apiForwardChatItems — add `sendAsGroup`

Haskell: adds `sendAsGroup :: ShowGroupAsSender` between `toChatRef` and `fromChatRef`. Wire: `/_forward #<toId> as_group=on #<fromId> ...`

Parser: `(" as_group=" *> onOffP <|> pure False)` — space before, no parens, between refs.

Current Swift (`AppAPITypes.swift:62`):
```swift
case apiForwardChatItems(toChatType: ChatType, toChatId: Int64, toScope: GroupChatScope?, fromChatType: ChatType, fromChatId: Int64, fromScope: GroupChatScope?, itemIds: [Int64], ttl: Int?)
```

**Change**: Add `sendAsGroup: Bool` after `toScope`:
```swift
case apiForwardChatItems(toChatType: ChatType, toChatId: Int64, toScope: GroupChatScope?, sendAsGroup: Bool, fromChatType: ChatType, fromChatId: Int64, fromScope: GroupChatScope?, itemIds: [Int64], ttl: Int?)
```

Update `cmdString` (`AppAPITypes.swift:253`):
```swift
let asGroup = sendAsGroup ? " as_group=on" : ""
return "/_forward \(ref(toChatType, toChatId, scope: toScope))\(asGroup) \(ref(fromChatType, fromChatId, scope: fromScope)) \(itemIds...) ttl=\(ttlStr)"
```

Note: ` as_group=on` has a space before it, no parens, inserted between the two refs.

### 4.3 apiPrepareGroup — add `directLink`

Haskell: adds `DirectLink` (type alias for `Bool`) between `CreatedLinkContact` and `GroupShortLinkData`. Wire: `/_prepare group <userId> <link> direct=on|off <json>`, default `true` when absent.

Current Swift (`AppAPITypes.swift:127`):
```swift
case apiPrepareGroup(userId: Int64, connLink: CreatedConnLink, groupShortLinkData: GroupShortLinkData)
```

**Change**: Add `directLink: Bool`:
```swift
case apiPrepareGroup(userId: Int64, connLink: CreatedConnLink, directLink: Bool, groupShortLinkData: GroupShortLinkData)
```

Update `cmdString` (`AppAPITypes.swift:330`):
```swift
return "/_prepare group \(userId) \(connLink.connFullLink) \(connLink.connShortLink ?? "") direct=\(onOff(directLink)) \(encodeJSON(groupShortLinkData))"
```

### 4.4 apiNewPublicGroup — new command

Haskell:
```haskell
| APINewPublicGroup {userId :: UserId, incognito :: IncognitoEnabled, relayIds :: NonEmpty Int64, groupProfile :: GroupProfile}
```

Wire: `/_public group <userId> incognito=on|off <relayIds_csv> <json>`

Parser in `Commands.hs`: `"/_public group " *> (APINewPublicGroup <$> A.decimal <*> incognitoOnOffP <*> _strP <* A.space <*> jsonP)`

Note: `_strP` reads a space-prefixed comma-separated list of Int64s (the `relayIds`), followed by a space then JSON.

**Add** to `ChatCommand`:
```swift
case apiNewPublicGroup(userId: Int64, incognito: Bool, relayIds: [Int64], groupProfile: GroupProfile)
```

Note: Haskell uses `NonEmpty Int64` — callers must ensure at least one relay ID is passed.

`cmdString`:
```swift
case let .apiNewPublicGroup(userId, incognito, relayIds, groupProfile):
    return "/_public group \(userId) incognito=\(onOff(incognito)) \(relayIds.map(String.init).joined(separator: ",")) \(encodeJSON(groupProfile))"
```

**Add** wrapper in `SimpleXAPI.swift`:
```swift
func apiNewPublicGroup(incognito: Bool, relayIds: [Int64], groupProfile: GroupProfile) async throws -> (GroupInfo, GroupLink, [GroupRelay]) {
    let userId = try currentUserId("apiNewPublicGroup")
    let r = await chatSendCmd(.apiNewPublicGroup(userId: userId, incognito: incognito, relayIds: relayIds, groupProfile: groupProfile))
    if case let .publicGroupCreated(_, groupInfo, groupLink, groupRelays) = r {
        return (groupInfo, groupLink, groupRelays)
    }
    throw r
}
```

---

## 5. API Response/Event Changes

### 5.1 CRPublicGroupCreated — new response

Haskell:
```haskell
| CRPublicGroupCreated {user :: User, groupInfo :: GroupInfo, groupLink :: GroupLink, groupRelays :: [GroupRelay]}
```

**Add** to `ChatResponse2`:
```swift
case publicGroupCreated(user: UserRef, groupInfo: GroupInfo, groupLink: GroupLink, groupRelays: [GroupRelay])
```

### 5.2 CEvtGroupLinkRelaysUpdated — new event

Haskell:
```haskell
| CEvtGroupLinkRelaysUpdated {user :: User, groupInfo :: GroupInfo, groupLink :: GroupLink, groupRelays :: [GroupRelay]}
```

**Add** to `ChatEvent`:
```swift
case groupLinkRelaysUpdated(user: UserRef, groupInfo: GroupInfo, groupLink: GroupLink, groupRelays: [GroupRelay])
```

### 5.3 CRUserServersValidation — add `serverWarnings`

Haskell: field added: `serverWarnings :: [UserServersWarning]`.

Current Swift (`AppAPITypes.swift:660`):
```swift
case userServersValidation(user: UserRef, serverErrors: [UserServersError])
```

**Change**:
```swift
case userServersValidation(user: UserRef, serverErrors: [UserServersError], serverWarnings: [UserServersWarning])
```

---

## 6. Type Mapping Reference

| Haskell | Swift | File | Notes |
|---|---|---|---|
| `RelayStatus` (Types.hs) | `RelayStatus` | ChatTypes.swift | enum, String raw |
| `GroupRelay` (Types.hs) | `GroupRelay` | ChatTypes.swift | struct |
| `UserChatRelay` (Operators.hs) | `UserChatRelay` | ChatTypes.swift | struct, Codable |
| `GroupShortLinkInfo` (Controller.hs) | `GroupShortLinkInfo` | AppAPITypes.swift | struct |
| `UserServersWarning` (Operators.hs) | `UserServersWarning` | AppAPITypes.swift | enum |
| `BoolDef` (Types.hs) | `Bool` | inline | always present from local backend |
| `ShortLinkContact` (Types.hs) | `String` | inline | type alias in Haskell |
| `CIChannelRcv` (Messages.hs) | `.channelRcv` | ChatTypes.swift | no member |
| `GRRelay` (Types/Shared.hs) | `.relay` | ChatTypes.swift | below observer |
| `GroupInfo.useRelays` | `useRelays: Bool` | ChatTypes.swift | BoolDef |
| `GroupInfo.relayOwnStatus` | `relayOwnStatus: RelayStatus?` | ChatTypes.swift | |
| `GroupProfile.groupLink` | `groupLink: String?` | ChatTypes.swift | ShortLinkContact |
| `User.userChatRelay` | `userChatRelay: Bool` | ChatTypes.swift | BoolDef |
| `UserOperatorServers.chatRelays` | `chatRelays: [UserChatRelay]?` | AppAPITypes.swift | |
| `GLPOk.groupSLinkInfo_` | `groupSLinkInfo_: GroupShortLinkInfo?` | AppAPITypes.swift | new first param |
| `SendRef.ShowGroupAsSender` | `sendAsGroup: Bool` | AppAPITypes.swift | on apiSendMessages |
| `APIForwardChatItems.sendAsGroup` | `sendAsGroup: Bool` | AppAPITypes.swift | |
| `APIPrepareGroup.DirectLink` | `directLink: Bool` | AppAPITypes.swift | |
| `APINewPublicGroup` | `.apiNewPublicGroup` | AppAPITypes.swift | new command |
| `CRPublicGroupCreated` | `.publicGroupCreated` | AppAPITypes.swift | new response |
| `CEvtGroupLinkRelaysUpdated` | `.groupLinkRelaysUpdated` | AppAPITypes.swift | new event |
| `CEChatRelayExists` | `.chatRelayExists` | APITypes.swift | new error |
| `SERelayUserNotFound` | `.relayUserNotFound` | APITypes.swift | new StoreError |
| `SEDuplicateMemberId` | `.duplicateMemberId` | APITypes.swift | new StoreError |
| `SEUserChatRelayNotFound` | `.userChatRelayNotFound` | APITypes.swift | new StoreError |
| `SEGroupRelayNotFound` | `.groupRelayNotFound` | APITypes.swift | new StoreError |
| `SEGroupRelayNotFoundByMemberId` | `.groupRelayNotFoundByMemberId` | APITypes.swift | new StoreError |

**NOT included** (per scope):
- `GroupRelayInvitation`, `RelayRequestData` — protocol only
- `GroupRootKey`, `GroupKeys`, `memberPubKey` — key management
- `GetUserChatRelays`, `SetUserChatRelays` — CLI-only commands; relays managed via existing `UserOperatorServers` API
- `GroupInfo.groupRelays` — does NOT exist in Haskell; relays come as separate response/event params

---

## 7. Edge Cases

1. **BoolDef fields**: `useRelays`/`userChatRelay` are `Bool` in Swift. Always present from local backend. Remote desktop compat handled on Haskell side only.

2. **CIDirection unknown tag**: Synthesized Decodable will throw for unknown tags. Parent `ChatItem` decode catches this via `CIInvalidJSON` fallback path.

3. **GroupMemberRole unknown value**: `GRUnknown Text` in Haskell catches forward-compat unknown roles. In Swift, unknown raw values will fail `String`-based decode. Need custom `init(from:)` that stores the raw string and falls back to a `.unknown` case, plus custom `encode(to:)` that re-emits the original string for round-trip fidelity. Without this, any `GroupMember` with an unknown role will fail to decode (degrading to `CIInvalidJSON`), and cannot be re-encoded if round-tripped.

4. **Wire format precision**:
   - Send: `(as_group=on)` directly appended to ref, no space
   - Forward: ` as_group=on` with leading space, no parens
   - PrepareGroup: ` direct=on|off` with leading space, defaults `true` when absent

5. **GroupProfile.groupLink in Encodable**: `GroupProfile` is `Codable`. New `groupLink` field must be included in encoding. Since it defaults to `nil`, `encodeIfPresent` handles it automatically with synthesized `Encodable`.

6. **UserOperatorServers.chatRelays compatibility**: Using `[UserChatRelay]?` (optional) ensures old backends that don't include `chatRelays` in JSON still decode correctly.

7. **CIQDirection quote encoding change**: In Haskell, `null` chatDir in quotes now maps to `channelRcv` (was `groupRcv(nil)` before). If Swift's `CIQDirection` decode assumed `null` = anonymous group quote, it must now handle `{"channelRcv":{}}` for channel message quotes.

8. **StoreError has no decode fallback**: `StoreError` uses synthesized `Decodable` with no `CIInvalidJSON`-style fallback. New variants MUST be added or the entire `ChatError` will fail to decode when the backend emits them.
