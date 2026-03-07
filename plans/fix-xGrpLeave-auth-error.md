# Fix: AUTH Error on ACK After `xGrpLeave` Deletes Sender Connection

## Root Cause

In `xGrpLeave` (Subscriber.hs:2794-2795), `deleteMemberConnection m` deletes the **sender's** agent connection — the same `agentConnId` that `withAckMessage` will ACK on. The ACK then fails with AUTH error, which is visible to the user:

```
[#team bob, groupId: 1, memberId: 2, connId: 2] error: connection authorization failed
- this could happen if connection was deleted, secured with different credentials,
or due to a bug - please re-create the connection
```

The `shouldDelConns` mechanism already exists to skip ACK (line 1393-1394), but it only activates for relay users via `createDeliveryTasks`:

```haskell
shouldDelConns <-
  if isUserGrpFwdRelay gInfo' && not (blockedByAdmin m)
    then createDeliveryTasks ...
    else pure False          -- always False for non-relay users
```

Note: `xGrpDel` (line 2809) has the same bug for non-relay users — it deletes all group connections including the sender's, but `shouldDelConns = False`.

## Call Graph

```
MSG received on agentConnId (member's connection)
  └─ withAckMessage "group msg" agentConnId msgMeta ...
       └─ action runs:
            └─ processAChatMsg → processEvent → XGrpLeave → xGrpLeave
                 ├─ deleteMemberConnection m        ← DELETES agentConnId
                 │   └─ deleteAgentConnectionAsync'  (agent schedules SMP queue deletion)
                 │   └─ updateConnectionStatus db conn ConnDeleted
                 └─ returns Just DJSGroup{...}
            └─ shouldDelConns = False               ← for non-relay users
       └─ unless False $ ackMsg msgMeta Nothing
            └─ ackMessageAsync a "" agentConnId msgId Nothing
                 ← ACK sent on DELETED connection → AUTH error
```

## Recommended Fix

After the message processing fold, check if the sender's connection was marked `ConnDeleted` in DB (by `xGrpLeave` or `xGrpDel`). Use this to skip ACK via the existing `shouldDelConns` mechanism.

Two files, ~10 lines total:

### 1. `src/Simplex/Chat/Store/Direct.hs` — Add function

```haskell
isConnectionDeleted :: DB.Connection -> Int64 -> IO Bool
isConnectionDeleted db connId_ =
  maybe True (== ConnDeleted) <$>
    maybeFirstRow fromOnly
      (DB.query db "SELECT conn_status FROM connections WHERE connection_id = ?" (Only connId_))
```

Follows the exact pattern of `updateConnectionStatusFromTo` (line 943) which already queries the same column.

### 2. `src/Simplex/Chat/Library/Subscriber.hs` — Lines 865-871

Before:
```haskell
newDeliveryTasks <- reverse <$> foldM (processAChatMsg gInfo' m' tags eInfo) [] aChatMsgs
shouldDelConns <-
  if isUserGrpFwdRelay gInfo' && not (blockedByAdmin m)
    then createDeliveryTasks gInfo' m' newDeliveryTasks
    else pure False
withRcpt <- checkSendRcpt $ rights aChatMsgs
pure (withRcpt, shouldDelConns)
```

After:
```haskell
newDeliveryTasks <- reverse <$> foldM (processAChatMsg gInfo' m' tags eInfo) [] aChatMsgs
senderConnDeleted <- withStore' $ \db -> isConnectionDeleted db connId
shouldDelConns <-
  if isUserGrpFwdRelay gInfo' && not (blockedByAdmin m)
    then (|| senderConnDeleted) <$> createDeliveryTasks gInfo' m' newDeliveryTasks
    else pure senderConnDeleted
withRcpt <- checkSendRcpt $ rights aChatMsgs
pure (withRcpt, shouldDelConns)
```

`connId` is already in scope from `conn@Connection {connId, ...}` at line 674.

## Why This Is Correct

- Skips ACK only when the connection is already deleted (primary key lookup, microseconds)
- Covers both `xGrpLeave` (deletes sender conn) and `xGrpDel` for non-relay users (deletes all conns)
- No type changes, no signature changes, no changes to `xGrpLeave` itself
- `shouldDelConns` semantics ("connection is gone, don't ACK") are preserved

## Why Skipping ACK Is Safe

The connection is being deleted. The SMP queue will be deleted momentarily by `deleteAgentConnectionAsync'`. Any redelivery attempt by the server hits the deleted queue and is discarded.

## Contrast With Other Handlers

| Handler | Connection deleted | ACK connection | Conflict? | Current handling |
|---|---|---|---|---|
| `xGrpLeave` | sender's (`agentConnId`) | `agentConnId` | **YES** | None (bug) |
| `xGrpDel` (non-relay) | all group conns | `agentConnId` | **YES** | None (bug) |
| `xGrpDel` (relay) | none (deferred) | `agentConnId` | No | `DJRelayRemoved` → `shouldDelConns = True` |
| `xGrpMemDel` | deleted member's conn | sender's conn | No | N/A |
