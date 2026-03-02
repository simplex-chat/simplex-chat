# Plan 3: Subscriber-Side — relay_status Column on group_members

## Problem Statement

When a subscriber joins a channel via `APIConnectPreparedGroup` (Commands.hs:1991), relay connections happen in parallel via `connectToRelay` (line 2035). If some relays permanently fail while others succeed, the `CRStartedConnectionToGroup` response includes `RelayConnectionResult` with `relayError` set, but no field on the relay member records the failure. The subscriber sees a perpetual yellow "connecting" indicator.

## Solution Summary

Add `relay_status` TEXT column to `group_members` table. Add `relayStatus :: Maybe RelayStatus` to Haskell `GroupMember` and Swift `GroupMember`. In `APIConnectPreparedGroup`, after partitioning failed/succeeded relays, update permanently failed relay members' `relay_status` to `RSFailed` (from Plan 1). Re-read members before building `relayResults` so `CRStartedConnectionToGroup` carries updated members. iOS checks `relayStatus` for failure display.

**Prerequisite**: Plan 1 (adds `RSFailed` to `RelayStatus` in Types/Shared.hs and ChatTypes.swift) MUST be implemented first.

## Detailed Technical Design

### Data Flow

```
APIConnectPreparedGroup (Commands.hs:1991)
  → mapConcurrently connectToRelay (line 2008)
  → partition failed/succeeded (line 2009-2010)
  → else branch (some succeeded, line 2020):
    → NEW: for permanently failed relays, UPDATE group_members SET relay_status = 'failed'
    → NEW: re-read updated members
    → build relayResults with updated members (line 2027)
    → return CRStartedConnectionToGroup with relayResults
  → iOS: connectPreparedGroup (ComposeView.swift:1044) receives relayResults
  → groupMembers = relayResults.map { GMember($0.relayMember) }
  → ComposeView: check m.wrappedValue.relayStatus == .rsFailed → red indicator
```

### What Changes

| Layer | File | Change |
|-------|------|--------|
| DB migration | New migration file | `ALTER TABLE group_members ADD COLUMN relay_status TEXT;` |
| Haskell types | Types.hs:992 | Add `relayStatus :: Maybe RelayStatus` to GroupMember |
| Haskell store | Store/Shared.hs:727 | Add `m.relay_status` to `groupMemberQuery` SELECT |
| Haskell store | Store/Shared.hs:670 | Add `Maybe RelayStatus` to `GroupMemberRow` type alias |
| Haskell store | Store/Shared.hs:700 | Add `relayStatus` to `toGroupMember` tuple destructuring |
| Haskell store | Store/Groups.hs (new) | Add `updateMemberRelayStatus` function |
| Haskell logic | Library/Commands.hs:2020-2028 | Update permanently failed relays, re-read members |
| Swift types | ChatTypes.swift:2606 | Add `public var relayStatus: RelayStatus?` to GroupMember |
| Swift UI | ComposeView.swift:782-795 | Check `relayStatus` for failure indicator |
| Swift UI | ChannelRelaysView.swift:86-94 | Check `relayStatus` in `relayConnStatus` |

### Key Design Decisions

**Why relay_status, not member_status?** `member_status` (GroupMemberStatus) tracks group membership lifecycle (invited→accepted→connected→...). Relay connection failure is orthogonal — the member is still "accepted" in the group, its relay just failed. A separate field avoids polluting the membership state machine.

**Why reuse RelayStatus?** `RelayStatus` already has the right semantics. Adding `RSFailed` (Plan 1) provides the failure state. The subscriber's `relay_status` mirrors the owner's `group_relays.relay_status` semantically.

**Nullable**: Most group members are NOT relays. Only relay members (`memberRole == GRRelay`) will have a non-null value.

**Swift Decodable backward compatibility**: Swift auto-derived `Decodable` treats missing optional fields as `nil`. Adding `relayStatus: RelayStatus?` is safe — JSON from servers that don't yet send `relay_status` decodes with `relayStatus = nil`.

**No Connection dependency**: Unlike Plan 2, this updates `group_members` directly — works even when `activeConn = Nothing` (failure before `createConnReqConnection`).

**Async failures**: Consider async connection failures: ERR events in Subscriber.hs:1121 for relay members could update `relay_status` to `RSFailed` using `updateMemberRelayStatus`.

## Detailed Implementation Steps

### Step 1: DB Migration

**New file:** `src/Simplex/Chat/Store/SQLite/Migrations/M20260223_relay_member_status.hs`

```sql
-- UP
ALTER TABLE group_members ADD COLUMN relay_status TEXT;
-- DOWN
ALTER TABLE group_members DROP COLUMN relay_status;
```

Register in `Migrations.hs` after `m20260222_chat_relays`.

### Step 2: Haskell — Add field to GroupMember

**File:** `src/Simplex/Chat/Types.hs` (line 992)

After `relayLink :: Maybe ShortLinkContact`:
```haskell
relayStatus :: Maybe RelayStatus
```

JSON serialization: `$(JQ.deriveJSON defaultJSON ''GroupMember)` auto-derives — new Maybe field serializes as `null` when Nothing.

### Step 3: Haskell — Update DB parsing

**File:** `src/Simplex/Chat/Store/Shared.hs`

**GroupMemberRow type alias** (line 670): Add `Maybe RelayStatus` as the last element in the final tuple segment (after `Maybe ShortLinkContact`).

**groupMemberQuery** (line 727): Add `m.relay_status` immediately after `m.relay_link`:
```sql
m.support_chat_last_msg_from_member_ts, m.member_pub_key, m.relay_link, m.relay_status,
```

**toGroupMember** (line 700): Update tuple destructuring — current pattern ends with `..., memberPubKey, relayLink)`. Change to `..., memberPubKey, relayLink, relayStatus)`. The `GroupMember {..}` construction at line 718 auto-fills `relayStatus` from the new binding.

### Step 4: Haskell — Add update function

**File:** `src/Simplex/Chat/Store/Groups.hs`

Near `updateGroupMemberStatus` (line 1709):
```haskell
updateMemberRelayStatus :: DB.Connection -> GroupMember -> RelayStatus -> IO ()
updateMemberRelayStatus db GroupMember {groupMemberId} relayStatus = do
  currentTs <- getCurrentTime
  DB.execute db
    "UPDATE group_members SET relay_status = ?, updated_at = ? WHERE group_member_id = ?"
    (relayStatus, currentTs, groupMemberId)
```

### Step 5: Haskell — Update APIConnectPreparedGroup

**File:** `src/Simplex/Chat/Library/Commands.hs` (lines 2020-2028)

In the `else` branch, after async retry (line 2026), before building `relayResults` (line 2027):

```haskell
-- Existing: async retry temporary failures
let retryable = [(l, m) | r@(l, m, _) <- failed, isTempErr r]
void $ mapConcurrently (uncurry $ retryRelayConnectionAsync gInfo') retryable
-- NEW: mark permanently failed relay members
let permanent = [m | r@(_, m, _) <- failed, not (isTempErr r)]
forM_ permanent $ \failedMember ->
  withFastStore' $ \db -> updateMemberRelayStatus db failedMember RSFailed
-- NEW: re-read permanently failed members
let permanentIds = S.fromList [groupMemberId' m | m <- permanent]
relayResults <- forM rs $ \(_, m, r) ->
  if groupMemberId' m `S.member` permanentIds
  then do
    m' <- withFastStore $ \db -> getGroupMember db vr user groupId (groupMemberId' m)
    pure $ RelayConnectionResult m' (leftToMaybe r)
  else pure $ RelayConnectionResult m (leftToMaybe r)
pure $ CRStartedConnectionToGroup user gInfo'' incognitoProfile relayResults
```

Note: `import qualified Data.Set as S` already exists at Commands.hs:47. `isTempErr` (line 2032) is in the `where` clause of the enclosing case expression — it IS in scope within the `else` block.

### Step 6: Swift — Add relayStatus field

**File:** `SimpleXChat/ChatTypes.swift`

Add to GroupMember struct after `relayLink` (line ~2606):
```swift
public var relayStatus: RelayStatus?
```

### Step 7: Swift UI — Show failure indicator

**ComposeView.swift** subscriber relay bar (lines 782-795):
```swift
let isFailed = m.relayStatus == .rsFailed
Circle()
    .fill(m.memberStatus == .memConnected ? .green : isFailed ? .red : .yellow)
Text(m.memberStatus == .memConnected ? "connected" : isFailed ? "failed" : "connecting")
```

**ChannelRelaysView.swift** `relayConnStatus` (lines 86-94), add at top:
```swift
if member.relayStatus == .rsFailed {
    return "failed"
}
```

## Complexity Assessment

| Aspect | Count |
|--------|-------|
| DB migration | 1 (ALTER TABLE ADD COLUMN) |
| Haskell files to modify | 5 (Types.hs, Shared.hs, Groups.hs, Commands.hs, Migrations.hs) |
| New Haskell functions | 1 (updateMemberRelayStatus) |
| Swift files to modify | 3 (ChatTypes, ComposeView, ChannelRelaysView) |
| Pattern match impact | 0 — Maybe field has no exhaustive matches. RelayStatus changes handled by Plan 1 prerequisite. |
| Risk of regression | **LOW** |

## Limitations

- **DB migration required**: Adds nullable column to `group_members`. Performance impact minimal.
- **Depends on Plan 1**: Requires RSFailed in RelayStatus enum.
- **Dual semantics**: Owner uses `group_relays.relay_status`, subscriber uses `group_members.relay_status` — two locations for conceptually similar data.
- **Binary**: Only NULL (not failed) or RSFailed. No intermediate lifecycle tracking.
- **No recovery path**: Once RSFailed, no automatic retry mechanism.
