# Plan 2: Subscriber-Side — New ConnStatus Case (ConnFailed)

## Problem Statement

When a subscriber joins a channel via `APIConnectPreparedGroup` (Commands.hs:1991), relay connections happen in parallel via `connectToRelay` (line 2035). If some relays permanently fail while others succeed, the `CRStartedConnectionToGroup` response includes `RelayConnectionResult` with `relayError` set, but the relay member's `activeConn` stays at `ConnNew`/`ConnPrepared` forever — or `activeConn` may be `Nothing` if failure occurred before `createConnReqConnection` (Commands.hs:3359). The subscriber sees a perpetual yellow "connecting" indicator.

## Solution Summary

Add `ConnFailed` case to `ConnStatus`. In `APIConnectPreparedGroup`, after partitioning failed/succeeded relays (line 2009-2010), update permanently failed relay members' connection status to `ConnFailed`. Re-read members before building `relayResults` so `CRStartedConnectionToGroup` carries updated members. iOS checks `connStatus` for failure display.

**Prerequisite**: Plan 1 (RSFailed on RelayStatus) should be implemented first — it's independent but establishes the pattern.

## Detailed Technical Design

### Data Flow

```
APIConnectPreparedGroup (Commands.hs:1991)
  → mapConcurrently connectToRelay (line 2008)
  → partition failed/succeeded (line 2009-2010)
  → else branch (some succeeded, line 2020):
    → NEW: for permanently failed relays with activeConn, update conn_status to ConnFailed
    → NEW: re-read updated members
    → build relayResults with updated members (line 2027)
    → return CRStartedConnectionToGroup with relayResults
  → iOS: connectPreparedGroup (ComposeView.swift:1044) receives relayResults
  → groupMembers = relayResults.map { GMember($0.relayMember) }
  → ComposeView: check m.activeConn?.connStatus == .failed → red indicator
```

### What Changes

| Layer | File | Change |
|-------|------|--------|
| Haskell types | Types.hs:1727-1776 | Add `ConnFailed` case to ConnStatus |
| Haskell encoding | Types.hs:1757-1776 | Add "failed" text encoding/decoding |
| Haskell pattern matches | 10 files, ~20-30 exhaustive case statements | Add `ConnFailed` to every exhaustive match |
| Haskell logic | Library/Commands.hs:2020-2028 | Update permanently failed relay connections, re-read members |
| Swift types | ChatTypes.swift:2304-2328 | Add `.failed = "failed"` case |
| Swift pattern matches | ChatTypes.swift | Add to `initiated` computed property |
| Swift UI | ComposeView.swift:782-795 | Check `connStatus` for failed relays |
| Swift UI | ChannelRelaysView.swift:86-94 | Check `connStatus` in `relayConnStatus` |

### Key Design Decisions

**ConnFailed semantics**: Terminal state like `ConnDeleted`, but for unintentional failure vs intentional removal. Treat as terminal in all business logic.

**Connection may not exist**: If `connectToRelay` fails before `createConnReqConnection` (Commands.hs:3359) — e.g. during `getShortLinkConnReq` (line 2044) — no Connection record exists. In this case `activeConn = Nothing` and we cannot set ConnFailed. The relay member still has `relayError` in the response, but no persisted failure state on the connection. This is a **fundamental limitation** of the ConnStatus approach.

**No DB migration**: `conn_status` is stored as TEXT — new "failed" value stores/reads without schema change.

**Async failures**: Consider async connection failures: ERR events in Subscriber.hs:1121 for relay members could update `conn_status` to `ConnFailed` using existing `updateConnectionStatus_` (Direct.hs:978).

## Detailed Implementation Steps

### Step 1: Haskell — Add ConnFailed to ConnStatus

**File:** `src/Simplex/Chat/Types.hs` (line 1727)

Add `ConnFailed` after `ConnDeleted`:
```haskell
  | ConnDeleted
  | ConnFailed
  deriving (Eq, Show, Read)
```

Update `textEncode`: `ConnFailed -> "failed"`
Update `textDecode`: `"failed" -> Just ConnFailed`

### Step 2: Haskell — Fix all exhaustive pattern matches

~47 references across 9 files, but most are equality checks (`==`). Only ~2-3 exhaustive pattern matches require adding `ConnFailed`: `textEncode` in Types.hs and `initiated` in Swift ChatTypes.swift (compiler-enforced). `textDecode` has catch-all but should be updated for correctness.

Treat `ConnFailed` as terminal (like `ConnDeleted`) in all business logic.

Key files: Types.hs, Commands.hs, Subscriber.hs, Internal.hs, Store/Direct.hs, Store/Shared.hs, Store/Connections.hs, Store/Groups.hs, View.hs.

### Step 3: Haskell — Update APIConnectPreparedGroup

**File:** `src/Simplex/Chat/Library/Commands.hs` (lines 2020-2028)

In the `else` branch, after async retry of temp failures (line 2026), before building `relayResults` (line 2027):

```haskell
-- Existing: async retry temporary failures
let retryable = [(l, m) | r@(l, m, _) <- failed, isTempErr r]
void $ mapConcurrently (uncurry $ retryRelayConnectionAsync gInfo') retryable
-- NEW: mark permanently failed relay connections
let permanent = [m | r@(_, m, _) <- failed, not (isTempErr r)]
forM_ permanent $ \GroupMember {activeConn} ->
  forM_ activeConn $ \conn ->
    withStore' $ \db -> updateConnectionStatus_ db (dbConnId conn) ConnFailed
-- NEW: re-read permanently failed members to get updated activeConn
let permanentIds = S.fromList [groupMemberId' m | m <- permanent]
relayResults <- forM rs $ \(_, m, r) ->
  if groupMemberId' m `S.member` permanentIds
  then do
    m' <- withFastStore $ \db -> getGroupMember db vr user groupId (groupMemberId' m)
    pure $ RelayConnectionResult m' (leftToMaybe r)
  else pure $ RelayConnectionResult m (leftToMaybe r)
pure $ CRStartedConnectionToGroup user gInfo'' incognitoProfile relayResults
```

Note: Requires `import qualified Data.Set as S`. `updateConnectionStatus_` (Direct.hs:978) takes `Int64` connId + `ConnStatus`. When `activeConn = Nothing`, `forM_` skips — no update, but `relayError` still carries the error.

### Step 4: Swift — Add .failed to ConnStatus

**File:** `SimpleXChat/ChatTypes.swift` (line 2304-2312)

Add case:
```swift
case failed = "failed"
```

Update `initiated` property — add `case .failed: return nil`.

### Step 5: Swift UI — Show failed relay indicator

**ComposeView.swift** subscriber relay bar (lines 782-795):
```swift
let isFailed = m.activeConn?.connStatus == .failed
Circle()
    .fill(m.memberStatus == .memConnected ? .green : isFailed ? .red : .yellow)
Text(m.memberStatus == .memConnected ? "connected" : isFailed ? "failed" : "connecting")
```

**ChannelRelaysView.swift** `relayConnStatus` (lines 86-94), add at top:
```swift
if member.activeConn?.connStatus == .failed {
    return "failed"
}
```

## Complexity Assessment

| Aspect | Count |
|--------|-------|
| Haskell files to modify | 10+ |
| Pattern matches to update | ~2-3 exhaustive (textEncode, Swift initiated, textDecode optional) |
| Swift files to modify | 3 (ChatTypes, ComposeView, ChannelRelaysView) |
| DB migrations | 0 |
| Risk of regression | **MODERATE** — few exhaustive matches but semantic pollution of general-purpose enum |

## Limitations

- **Blast radius perception**: ConnStatus has ~47 references across 9 files, but only ~2-3 exhaustive matches need updating. The real risk is semantic pollution of a general-purpose enum, not pattern match volume.
- **Connection may not exist**: If failure occurs before `createConnReqConnection`, `activeConn = Nothing` and ConnFailed cannot be set. Failure is still in `relayError` but not persisted.
- **Semantic mismatch**: ConnStatus is a general-purpose connection lifecycle enum. Adding a relay-specific failure state pollutes it.
- **No recovery path**: Once ConnFailed, no automatic mechanism to retry.
