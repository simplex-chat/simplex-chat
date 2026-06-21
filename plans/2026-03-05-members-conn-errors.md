# Save Permanent Connection Errors for Group Members

## Context

When a group member's connection handshake fails with a permanent error (e.g., `CONN NOT_ACCEPTED`, `SMP AUTH`, `AGENT A_VERSION`), the ERR event is logged to the UI event stream and discarded. The member record stays stuck in a "connecting" `GroupMemberStatus` (like `memIntroduced`, `memAccepted`) forever. Users see perpetual "connecting" with no explanation and no way to know whether to wait or re-invite.

**Root cause**: `agentMsgConnStatus` (Subscriber.hs:376) only maps success events (`CONF`, `INFO`, `JOINED`, `CON`) to status transitions. The ERR handler for group members (Subscriber.hs:1054-1056) only logs to UI and completes the command — no status or error is persisted.

## Solution Summary

Add `ConnError {connError :: Text}` constructor to `ConnStatus`. Error text is encoded in the `conn_status TEXT` column as `"error <text>"` via `TextEncoding`, and in JSON via `sumTypeJSON` (following `GSSError`/`CIFileStatus` pattern). No new DB column, no migration. When a non-temporary ERR arrives before connection is ready, transition to `ConnError` and notify UI. Messages are not queued for errored connections.

## Technical Design

### Error classification

Use `temporaryOrHostError` from `Simplex.Messaging.Agent.Client` (simplexmq Client.hs:1486, exported at line 60):
- Returns `True` for NETWORK, TIMEOUT, HOST, TEVersion, INACTIVE, CRITICAL-with-restart → **do not save**
- Returns `False` for AUTH, CONN errors, VERSION, INTERNAL, etc. → **save as permanent error**

Guard: only save when connection is not `ConnReady` and not already `ConnError`. Post-handshake errors (when `connStatus == ConnReady`) are handled by existing `processConnMERR` (AUTH counters, QUOTA counters).

### Data flow

```
Agent ERR event
  → Subscriber.hs processGroupMessage ERR handler
  → guard: connStatus is not ConnReady, not ConnError, not temporaryOrHostError
  → DB: UPDATE connections SET conn_status = 'error <tshow err>'
  → emit: CEvtGroupMemberUpdated user gInfo m m'
  → iOS: upsertGroupMember updates model → UI re-renders
```

### DB encoding

`conn_status TEXT NOT NULL` already exists. `ConnError` encodes as `"error " <> errText` using `TextEncoding` (same as `GSSError`). No migration needed — new text values are valid in the existing column.

### JSON encoding

Replace manual `ToJSON`/`FromJSON` instances with `$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "Conn") ''ConnStatus)`. This follows the `GroupSndStatus`/`CIFileStatus` pattern — `sumTypeJSON` is already imported in Types.hs (line 60).

JSON format (platform-dependent via `sumTypeJSON`):
- iOS: `{"error": {"connError": "SMP AUTH"}}` (ObjectWithSingleField)
- Android/Desktop: `{"type": "error", "connError": "SMP AUTH"}` (TaggedObject)
- Nullary cases: `{"ready": {}}` / `{"type": "ready"}` (not plain `"ready"` strings)

Note: `ConnSndReady` JSON tag changes from `"snd-ready"` to `"sndReady"` (`dropPrefix "Conn"` applies `fstToLower`). This is safe — JSON is core→UI within same build. Swift auto-synthesis matches on `sndReady` case name.

### Clear on recovery

When CON event arrives, `agentMsgConnStatus` returns `Just ConnReady`, `updateConnStatus` overwrites `conn_status` to `"ready"`. Error is implicitly cleared — no special cleanup needed.

### ConnStatus state machine update

```
Existing transitions (unchanged):
  ConnNew → ConnRequested → ConnAccepted → ConnSndReady → ConnReady
  ConnNew → ConnJoined → ConnSndReady → ConnReady
  ConnPrepared → ConnJoined → ConnSndReady → ConnReady
  Any → ConnDeleted

New transitions:
  Any pre-ready state → ConnError (on permanent ERR)
  ConnError → ConnReady (on successful CON — recovery)
  ConnError → ConnDeleted (on connection deletion)
```

### Pattern match safety audit

Traced every ConnStatus pattern match across Haskell (10 files), Swift (6 files), Kotlin (3 files).

**Must update (exhaustive matches):**

| Location | Change |
|---|---|
| Types.hs textEncode/textDecode (~1703) | Add ConnError encoding/decoding |
| Types.hs ToJSON/FromJSON (~1696) | Replace with `sumTypeJSON` TH splice |
| Swift ConnStatus.initiated | Add `case .error: return nil` |
| Kotlin ConnStatus.initiated | Add `Error -> null` (follow-up) |

**Must update (behavioral):**

| Location | Current behavior | Fix |
|---|---|---|
| Internal.hs memberSendAction (line 2041) | ConnError falls to `otherwise -> pendingOrForwarded` — messages queued for permanently errored connections | Add pattern guard `ConnError {} <- connStatus -> Nothing` |

**Verified safe — no changes needed:**

| Pattern | Sites | Why safe |
|---|---|---|
| `== ConnReady` / `== ConnSndReady` | 12 sites (connReady, Contact.ready, GroupMember.ready, sndReady, readyMemberConn, xftpSndFileTransfer) | ConnError ≠ these → excluded from "ready" paths |
| `== ConnPrepared` | 8 sites (joinPreparedConn, nextConnectPrepared, isContactCard, contactRequestPlan) | ConnError ≠ ConnPrepared → doesn't trigger join/prepare logic |
| `== ConnNew` | 4 sites (contactConnInitiated, nextAcceptContactRequest, APIPrepareContact) | ConnError ≠ ConnNew → doesn't trigger new-connection logic |
| `!= ConnDeleted` (DB WHERE) | 6 sites (getConnectionEntity, *ConnsToSub) | ConnError ≠ ConnDeleted → errored connections remain findable and subscribable (correct — enables recovery via CON). **Add TODO comments** at each site to consider whether ConnError connections should be excluded. |
| `updateConnectionStatusFromTo` | 3 sites | Compares current to specific `fromStatus` — ConnError won't accidentally match |
| `readyMemberConn` (Internal.hs:2078) | 1 site | `connStatus == ConnReady \|\| == ConnSndReady` — ConnError → `otherwise = Nothing` (correct) |
| `connDisabled`/`connInactive` | 6 sites | Derived from error counters, not connStatus |
| `agentMsgConnStatus` | 1 site | Only produces ConnSndReady/ConnRequested/ConnReady — no ConnError output |

## Implementation Plan

### 1. Haskell: ConnStatus type

**File: `src/Simplex/Chat/Types.hs`**

**ConnStatus** (~line 1673): Add constructor after `ConnDeleted`:
```haskell
  | ConnError {connError :: Text}
```
Record syntax for `sumTypeJSON` field name in JSON. `deriving (Eq, Show, Read)` unchanged.

**TextEncoding instance** (~line 1703) — for DB storage:
```haskell
  textEncode = \case
    ...
    ConnError err -> "error " <> err
  textDecode s
    | Just err <- T.stripPrefix "error " s = Just (ConnError err)
    | otherwise = case s of
        "new" -> Just ConnNew
        ... (existing cases unchanged)
        _ -> Nothing
```

Note: `textDecode` changes from `\case` to named parameter `s` to support `stripPrefix` guard.

**JSON instances** (~lines 1696-1701): Replace manual instances with TH splice:
```haskell
-- Remove:
--   instance FromJSON ConnStatus where parseJSON = textParseJSON "ConnStatus"
--   instance ToJSON ConnStatus where toJSON = J.String . textEncode; toEncoding = JE.text . textEncode
-- Add:
$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "Conn") ''ConnStatus)
```

`sumTypeJSON` and `dropPrefix` already imported (line 60). `FromField`/`ToField` instances unchanged — still use `TextEncoding` for DB.

**`connReady`** (line 1597): No change — `== ConnReady || == ConnSndReady`, `ConnError _` naturally returns `False`.

### 2. Haskell: Subscriber.hs — save error on permanent ERR

**File: `src/Simplex/Chat/Library/Subscriber.hs`**

Extend existing import (line 74):
```haskell
import Simplex.Messaging.Agent.Client (temporaryOrHostError, getAgentWorker, ...)
```

Update ERR handler in `processGroupMessage` (line 1054-1056). Current:
```haskell
ERR err -> do
  eToView $ ChatErrorAgent err (AgentConnId agentConnId) (Just connEntity)
  when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
```

New:
```haskell
ERR err -> do
  eToView $ ChatErrorAgent err (AgentConnId agentConnId) (Just connEntity)
  when (corrId /= "") $ withCompletedCommand conn agentMsg $ \_cmdData -> pure ()
  let Connection {connStatus = cs} = conn
  case cs of
    ConnReady -> pure ()
    ConnError _ -> pure ()
    _ | temporaryOrHostError err -> pure ()
      | otherwise -> do
          let errText = tshow err
          withStore' $ \db -> updateConnectionStatus db conn (ConnError errText)
          let conn' = conn {connStatus = ConnError errText}
              m' = m {activeConn = Just conn'}
          toView $ CEvtGroupMemberUpdated user gInfo m m'
```

Note: `let Connection {connStatus = cs} = conn` destructures via pattern binding, avoiding ambiguous `connStatus conn` field selector under `DuplicateRecordFields`.

No new store function — reuses existing `updateConnectionStatus` (Direct.hs:937) which calls `updateConnectionStatus_` → `textEncode` → stores `"error SMP AUTH"` in `conn_status`.

### 3. Haskell: memberSendAction — don't queue for errored connections

**File: `src/Simplex/Chat/Library/Internal.hs`**

Update `memberSendAction` (line 2040-2044). Current:
```haskell
    Just conn@Connection {connStatus}
      | connDisabled conn || connStatus == ConnDeleted || memberStatus == GSMemRejected -> Nothing
      | connInactive conn -> Just MSAPending
      | connStatus == ConnSndReady || connStatus == ConnReady -> sendBatchedOrSeparate conn
      | otherwise -> pendingOrForwarded
```

Add pattern guard after first guard (can't use `==` with associated data):
```haskell
    Just conn@Connection {connStatus}
      | connDisabled conn || connStatus == ConnDeleted || memberStatus == GSMemRejected -> Nothing
      | ConnError {} <- connStatus -> Nothing
      | connInactive conn -> Just MSAPending
      | connStatus == ConnSndReady || connStatus == ConnReady -> sendBatchedOrSeparate conn
      | otherwise -> pendingOrForwarded
```

### 4. Swift: ConnStatus enum

**File: `apps/ios/SimpleXChat/ChatTypes.swift`** (~line 2301)

Change from `String`-backed raw value enum to enum with associated value. Auto-synthesized `Decodable` handles `sumTypeJSON` format (same as `GroupSndStatus`, `CIFileStatus`):

```swift
public enum ConnStatus: Decodable, Hashable {
    case new
    case prepared
    case joined
    case requested
    case accepted
    case sndReady
    case ready
    case deleted
    case error(connError: String)

    var initiated: Bool? {
        switch self {
        case .new: return true
        case .prepared: return false
        case .joined: return false
        case .requested: return true
        case .accepted: return true
        case .sndReady: return nil
        case .ready: return nil
        case .deleted: return nil
        case .error: return nil
        }
    }
}
```

No custom `init(from:)` needed. `Hashable`/`Equatable` auto-synthesized. Existing equality checks like `connStatus == .ready` still compile (nullary cases).

### 5. Swift: Connection computed property

**File: `apps/ios/SimpleXChat/ChatTypes.swift`**

Add computed property to `Connection` struct (~line 2092, after `connStatus`):
```swift
public var connError: String? {
    if case let .error(err) = connStatus { return err }
    return nil
}
```

### 6. Swift: Member list status

**File: `apps/ios/Shared/Views/Chat/Group/GroupChatInfoView.swift`**

Update `memberConnStatus` function (~line 457). Insert error check FIRST (before `connDisabled`/`connInactive`):
```swift
private func memberConnStatus(_ member: GroupMember) -> LocalizedStringKey {
    if case .error = member.activeConn?.connStatus {
        return "connection error"
    } else if member.activeConn?.connDisabled ?? false {
        return "disabled"
    } else if member.activeConn?.connInactive ?? false {
        return "inactive"
    } else {
        return member.memberStatus.shortText
    }
}
```

**File: `apps/ios/Shared/Views/Chat/Group/MemberSupportView.swift`**

Update `memberStatus` function (line 198). Insert error check FIRST (before `connDisabled` at line 199):
```swift
    if case .error = member.activeConn?.connStatus {
        return "connection error"
    } else if member.activeConn?.connDisabled ?? false {
```

### 7. Swift: Member info error display

**File: `apps/ios/Shared/Views/Chat/Group/GroupMemberInfoView.swift`**

Add error display section after the `connStats` section (~line 190):
```swift
if let connError = member.activeConn?.connError {
    Section(header: Text("Connection error").foregroundColor(theme.colors.secondary)) {
        Text(connError)
            .foregroundColor(theme.colors.secondary)
            .font(.callout)
            .textSelection(.enabled)
    }
}
```

## Files Changed Summary

| Layer | File | Change |
|-------|------|--------|
| Core | `Types.hs` | Add `ConnError {connError :: Text}` to ConnStatus, update TextEncoding, replace JSON with `sumTypeJSON` TH splice |
| Logic | `Subscriber.hs` | Import `temporaryOrHostError`, handle permanent ERR for group members |
| Logic | `Internal.hs` | Add `ConnError` guard to `memberSendAction` → return `Nothing` |
| iOS | `ChatTypes.swift` | ConnStatus: auto-synthesized Decodable with `.error(connError:)`, Connection: `connError` computed property |
| iOS | `GroupChatInfoView.swift` | Show "connection error" in `memberConnStatus` (first check) |
| iOS | `MemberSupportView.swift` | Show "connection error" in `memberStatus` (first check) |
| iOS | `GroupMemberInfoView.swift` | Show error description section |

## Verification

1. **Build Haskell**: `cabal build --ghc-options -O0`
2. **Build iOS**: Verify Swift compiles — existing `connStatus == .ready` comparisons still work (nullary cases)
3. **JSON format**: Verify `sumTypeJSON` output matches Swift auto-synthesis expectations (nullary: `{"ready": {}}`, error: `{"error": {"connError": "..."}}`)
4. **Backward compat**: New `"error ..."` values in `conn_status` only appear after code update. Old code cannot parse them (downgrade risk, same as any new enum value).
5. **Recovery**: CON event → `updateConnectionStatus_ ConnReady` → overwrites `"error ..."` with `"ready"` in DB
6. **memberSendAction**: Verify messages are NOT queued for ConnError connections

## Out of Scope (immediate follow-up)

**Kotlin/Android/Desktop**: `ConnStatus` enum in `ChatModel.kt:2640` needs custom serializer for `sumTypeJSON` format (TaggedObject: `{"type": "error", "connError": "..."}`) + `Connection` needs `connError` computed property + member status UI. Must be updated before Android/Desktop builds from this commit. Existing bug at `GroupChatInfoView.kt:883` (`connDisabled` checked twice, should be `connInactive` on second check).
