# Plan 4: Subscriber-Side — New GroupMemberStatus.memFailed

## Problem Statement

Same as Plans 2-3: when a subscriber joins a channel via `APIConnectPreparedGroup` (Commands.hs:1991), permanently failed relay connections show as "connecting" forever. The relay member stays at its initial `memberStatus` (`GSMemAccepted`, set by `getCreateRelayForMember` at Groups.hs:1357) with no transition to a failure state.

## Solution Summary

Add `GSMemFailed` / `.memFailed` case to `GroupMemberStatus` enum. In `APIConnectPreparedGroup`, after partitioning failed/succeeded relays (line 2009-2010), update permanently failed relay members to `GSMemFailed`. Re-read members before building `relayResults` so `CRStartedConnectionToGroup` carries updated members. UI already switches on `memberStatus` — add `.memFailed` branch. Requires DB migration to add row to `group_member_status_predicates` table.

## Detailed Technical Design

### Data Flow

```
APIConnectPreparedGroup (Commands.hs:1991)
  → mapConcurrently connectToRelay (line 2008)
  → partition failed/succeeded (line 2009-2010)
  → else branch (some succeeded, line 2020):
    → NEW: for permanently failed relays, updateGroupMemberStatus to GSMemFailed
    → NEW: re-read updated members
    → build relayResults with updated members (line 2027)
    → return CRStartedConnectionToGroup with relayResults
  → iOS: connectPreparedGroup (ComposeView.swift:1044) receives relayResults
  → groupMembers = relayResults.map { GMember($0.relayMember) }
  → ComposeView: memberStatus == .memFailed → red indicator
```

### What Changes

| Layer | File | Change |
|-------|------|--------|
| DB migration | New migration file | INSERT ('failed', 0) into group_member_status_predicates |
| Haskell types | Types.hs:1211 | Add `GSMemFailed` case |
| Haskell encoding | Types.hs:1301-1334 | Add "failed" text encode/decode |
| Haskell predicates | Types.hs:1232-1299 | Add `GSMemFailed` to `memberActive`, `memberCurrent'`, `memberRemoved` (all exhaustive) |
| Haskell logic | Library/Commands.hs:2020-2028 | Update permanently failed relay members, re-read members |
| Swift types | ChatTypes.swift:2855-2870 | Add `.memFailed = "failed"` case |
| Swift predicates | ChatTypes.swift:2670-2716 | Add to `memberActive`, `memberCurrent` |
| Swift text | ChatTypes.swift:2872-2910 | Add to `text` and `shortText` properties |
| Swift UI | ComposeView.swift:782-795 | Check `memberStatus == .memFailed` for red indicator |
| Swift UI | ChannelRelaysView.swift:86-94 | Check `memberStatus` in status display |

### Key Design Decisions

**Why memberStatus?** ComposeView ALREADY switches on `memberStatus` to determine relay display (line 792: `m.memberStatus == .memConnected ? .green : .yellow`). Adding `.memFailed` to this existing pattern is the smallest UI change — no new field access patterns.

**Semantics of memFailed**: The relay member's connection to the subscriber permanently failed. `memberActive` → False, `memberCurrent` → False (relay is not a functioning member from subscriber's perspective), `memberRemoved` → False (not intentionally removed).

**group_member_status_predicates**: Must add `('failed', 0)` — a failed relay member should NOT count toward `summary_current_members_count`. This is a DB migration.

**No Connection dependency**: Uses `updateGroupMemberStatus` (Groups.hs:1709) which updates `group_members.member_status` directly — works even when `activeConn = Nothing`.

**Async failures**: Consider async connection failures: ERR events in Subscriber.hs:1121 for relay members could call `updateGroupMemberStatus` with `GSMemFailed`, then emit `CEvtConnectedToGroupMember` (misleading name but avoids new event plumbing).

## Detailed Implementation Steps

### Step 1: Haskell — Add GSMemFailed

**File:** `src/Simplex/Chat/Types.hs` (line 1211)

Add after `GSMemCreator`:
```haskell
  | GSMemCreator
  | GSMemFailed  -- relay member whose connection permanently failed
  deriving (Eq, Show, Ord)
```

Note: Enum derives `Ord` — `GSMemFailed` as last case gets highest ordinal. No Ord-dependent sorting on `memberStatus` exists in the codebase (all usages are `==` checks or pattern matches), so position is safe.

### Step 2: Haskell — Update TextEncoding

**File:** `src/Simplex/Chat/Types.hs` (lines 1301-1334)

`textDecode`: add `"failed" -> Just GSMemFailed`
`textEncode`: add `GSMemFailed -> "failed"`

### Step 3: Haskell — Update exhaustive predicate functions

**File:** `src/Simplex/Chat/Types.hs`

**`memberActive`** (lines 1232-1248) — 15-case exhaustive:
```haskell
GSMemFailed -> False
```

**`memberCurrent'`** (lines 1265-1281) — 15-case exhaustive, used by SQL triggers:
```haskell
GSMemFailed -> False
```

**`memberRemoved`** (lines 1283-1299) — 15-case exhaustive:
```haskell
GSMemFailed -> False
```

**`memberPending`** (lines 1253-1257) — uses catch-all `_ -> False`: no change needed.

### Step 4: Haskell — Verify other GroupMemberStatus pattern matches

Compiler will enforce exhaustive completeness. Additionally:

- **View.hs**: ~9 case statements on `memberStatus` — all use `_ -> ...` catch-all, won't break compilation.
- **Subscriber.hs, Commands.hs, Internal.hs**: Most use equality checks (`== GSMemConnected`) or `memberActive`/`memberCurrent`, not exhaustive case statements.
- **SQL triggers** (M20250919_group_summary.hs): Use `group_member_status_predicates` table — handled by Step 5 migration.

### Step 5: DB Migration

**New file:** `src/Simplex/Chat/Store/SQLite/Migrations/M20260223_mem_failed_status.hs`

```sql
-- UP
INSERT INTO group_member_status_predicates(member_status, current_member)
VALUES ('failed', 0);
-- DOWN
DELETE FROM group_member_status_predicates WHERE member_status = 'failed';
```

Register in `Migrations.hs` after `m20260222_chat_relays`.

### Step 6: Haskell — Update APIConnectPreparedGroup

**File:** `src/Simplex/Chat/Library/Commands.hs` (lines 2020-2028)

In the `else` branch, after async retry (line 2026), before building `relayResults` (line 2027):

```haskell
-- Existing: async retry temporary failures
let retryable = [(l, m) | r@(l, m, _) <- failed, isTempErr r]
void $ mapConcurrently (uncurry $ retryRelayConnectionAsync gInfo') retryable
-- NEW: mark permanently failed relay members
let permanent = [m | r@(_, m, _) <- failed, not (isTempErr r)]
forM_ permanent $ \failedMember ->
  withFastStore' $ \db -> updateGroupMemberStatus db (userId user) failedMember GSMemFailed
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

Note: `updateGroupMemberStatus` (Groups.hs:1709) signature: `DB.Connection -> UserId -> GroupMember -> GroupMemberStatus -> IO ()`. `userId` is extracted from `user@User{userId}` bound at line 1991 via `withUser`. `isTempErr` (line 2032) is in scope. `import qualified Data.Set as S` already exists at Commands.hs:47.

### Step 7: Swift — Add .memFailed

**File:** `SimpleXChat/ChatTypes.swift`

Add to enum (after `.memCreator`, line 2870):
```swift
case memFailed = "failed"
```

Add to `text` property (line 2872-2889):
```swift
case .memFailed: return "relay failed"
```

Add to `shortText` property (line 2892-2909):
```swift
case .memFailed: return "failed"
```

### Step 8: Swift — Update exhaustive predicates

**File:** `SimpleXChat/ChatTypes.swift`

**`memberActive`** (lines 2670-2687) — 15-case exhaustive:
```swift
case .memFailed: return false
```

**`memberCurrent`** (lines 2690-2707) — 15-case exhaustive:
```swift
case .memFailed: return false
```

**`memberPending`** (lines 2710-2715) — uses `default`: no change needed.

### Step 9: Swift UI — Show failure indicator

**ComposeView.swift** subscriber relay bar (lines 782-795):
```swift
let isFailed = m.memberStatus == .memFailed
Circle()
    .fill(m.memberStatus == .memConnected ? .green : isFailed ? .red : .yellow)
Text(m.memberStatus == .memConnected ? "connected" : isFailed ? "failed" : "connecting")
```

**ChannelRelaysView.swift** `relayConnStatus` (lines 86-94), add at top:
```swift
if member.memberStatus == .memFailed {
    return "failed"
}
```

### Step 10: Verify other Swift memberStatus usage

Compiler enforces exhaustive switches — `text` and `shortText` will require `.memFailed` case.

**ChannelMembersView** (line 21-24) currently filters: `s != .memLeft && s != .memRemoved`. A `.memFailed` relay member will still appear in the subscriber list — this is DESIRED so the subscriber can see which relay failed. The member row shows "failed" status via the `shortText` property.

## Complexity Assessment

| Aspect | Count |
|--------|-------|
| DB migration | 1 (INSERT into predicates table) |
| Haskell files to modify | 3+ (Types.hs, Commands.hs, Migrations.hs + compiler-discovered) |
| Haskell exhaustive matches | 3 known 15-case (memberActive, memberCurrent', memberRemoved) + textEncode/textDecode |
| Swift files to modify | 3+ (ChatTypes, ComposeView, ChannelRelaysView + compiler-discovered) |
| Swift exhaustive matches | 4 known (memberActive, memberCurrent, text, shortText) |
| Risk of regression | **MODERATE** — 16-case enums with 3-4 exhaustive matches per language |

## Limitations

- **Overloads memberStatus semantics**: GroupMemberStatus tracks group membership lifecycle, not connection health. `memFailed` is really about connection failure, not membership failure.
- **Naming**: `GSMemFailed` suggests the member failed to join the group. The actual meaning is narrower: the relay connection failed.
- **Pattern match overhead**: Every exhaustive GroupMemberStatus match (3 in Haskell, 4+ in Swift) must add the new case.
- **group_member_status_predicates dependency**: DB migration must stay in sync with Haskell enum.
- **No recovery path**: Once GSMemFailed, no automatic retry mechanism.
