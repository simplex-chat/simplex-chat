# Plan: Relay Request Worker Retry Limit

## Context

The relay request worker (`runRelayRequestWorker`) processes channel setup requests sequentially using a single worker (`relayRequestWorkerKey = 1`). When a request requires network calls to an unreachable server (e.g., fetching group link data via `getShortLinkConnReq'`), the worker retries indefinitely via `withRetryInterval` + `retryTmpError` — temp/host errors call `loop` with no limit. This blocks all subsequent relay requests from processing.

This is an attack vector: a channel owner can create a channel link on a server unreachable by the relay, causing the relay request worker to retry forever and blocking all other channel setup requests.

## Approach

Follow the XFTP worker retry pattern (`runXFTPDelWorker` in `simplexmq/src/Simplex/FileTransfer/Agent.hs:667`):

1. **Track retries and delay in DB**: Add `relay_request_retries` and `relay_request_delay` columns to the `groups` table
2. **Order by retries**: Query for next work item ordered by `relay_request_retries ASC, created_at ASC` — items with fewer retries are processed first, stuck items get pushed to the back
3. **Limit consecutive retries**: Replace `withRetryInterval` with `withRetryIntervalCount`, limiting to a small number of consecutive retries per pickup cycle (3, matching XFTP's `xftpConsecutiveRetries`). After the limit, the worker yields and picks the next item.
4. **Store delay for resumption**: On each retry, store the current backoff delay in DB. On next pickup, resume backoff from the stored delay (XFTP pattern: `ri {initialInterval = d, increaseAfter = 0}`)
5. **Expire old requests**: Mark relay requests older than 1 day as failed before querying for next item

### How this neutralizes the attack

- Attacker's request gets picked up, retried 3 times with backoff (~15s total), then yielded
- Worker picks the next item by retry count — legitimate requests (retries=0) go first
- Attacker's request accumulates retries, always processed last
- After 1 day, the request is marked failed and permanently excluded

---

## Detailed changes

### 1. Database migration

New migration: `M20260429_relay_request_retries.hs`

```sql
ALTER TABLE groups ADD COLUMN relay_request_retries INTEGER DEFAULT 0;
ALTER TABLE groups ADD COLUMN relay_request_delay INTEGER;
```

**Files:**
- `src/Simplex/Chat/Store/SQLite/Migrations/M20260429_relay_request_retries.hs` (new)
- `src/Simplex/Chat/Store/SQLite/Migrations.hs` (register)
- `src/Simplex/Chat/Store/Postgres/Migrations/M20260429_relay_request_retries.hs` (new)
- `src/Simplex/Chat/Store/Postgres/Migrations.hs` (register)
- `simplex-chat.cabal` (add modules)

### 2. Add delay field to RelayRequestData

**File:** `src/Simplex/Chat/Types.hs`

```haskell
data RelayRequestData = RelayRequestData
  { relayInvId :: InvitationId,
    reqGroupLink :: ShortLinkContact,
    reqChatVRange :: VersionRangeChat,
    relayRequestDelay :: Maybe Int64
  }
```

The delay is needed at the worker level to resume backoff from the stored position.

### 3. Update store functions

**File:** `src/Simplex/Chat/Store/RelayRequests.hs`

**`getNextPendingRelayRequest`** — three changes:
- Before querying, mark requests older than 1 day as failed
- Order by `relay_request_retries ASC, created_at ASC` instead of `group_id ASC`
- SELECT and return `relay_request_delay` in the data query

```haskell
getNextPendingRelayRequest db = do
  markOldRelayRequestsFailed db
  getWorkItem "relay request" getNextRequestGroupId getRelayRequestData (markRelayRequestFailed db)
  where
    getNextRequestGroupId =
      maybeFirstRow fromOnly $
        DB.query db
          [sql|
            SELECT group_id FROM groups
            WHERE relay_own_status = ?
              AND relay_request_failed = 0
              AND relay_request_err_reason IS NULL
            ORDER BY relay_request_retries ASC, created_at ASC
            LIMIT 1
          |]
          (Only RSInvited)
    getRelayRequestData groupId =
      firstRow' toRelayRequestData (SEGroupNotFound groupId) $
        DB.query db
          [sql|
            SELECT relay_request_inv_id, relay_request_group_link,
                   relay_request_peer_chat_min_version, relay_request_peer_chat_max_version,
                   relay_request_delay
            FROM groups WHERE group_id = ?
          |]
          (Only groupId)
      where
        toRelayRequestData (Just relayInvId, Just reqGroupLink, Just minV, Just maxV, relayRequestDelay) =
          Right (groupId, RelayRequestData {relayInvId, reqGroupLink, reqChatVRange = fromMaybe (versionToRange maxV) $ safeVersionRange minV maxV, relayRequestDelay})
        toRelayRequestData _ = Left $ SEInternalError "missing relay request data"
```

**New function: `markOldRelayRequestsFailed`**:

```haskell
markOldRelayRequestsFailed :: DB.Connection -> IO ()
markOldRelayRequestsFailed db = do
  cutoffTs <- addUTCTime (-nominalDay) <$> getCurrentTime
  currentTs <- getCurrentTime
  DB.execute db
    [sql|
      UPDATE groups
      SET relay_request_failed = 1, updated_at = ?
      WHERE relay_own_status = ?
        AND relay_request_failed = 0
        AND relay_request_err_reason IS NULL
        AND created_at < ?
    |]
    (currentTs, RSInvited, cutoffTs)
```

**New function: `updateRelayRequestRetries`**:

```haskell
updateRelayRequestRetries :: DB.Connection -> GroupId -> Int64 -> IO ()
updateRelayRequestRetries db groupId delay = do
  currentTs <- getCurrentTime
  DB.execute db
    "UPDATE groups SET relay_request_retries = relay_request_retries + 1, relay_request_delay = ?, updated_at = ? WHERE group_id = ?"
    (delay, currentTs, groupId)
```

Export `updateRelayRequestRetries` from module.

### 4. Worker changes

**File:** `src/Simplex/Chat/Library/Subscriber.hs`

**Import change**: Add `withRetryIntervalCount` to the import from `Simplex.Messaging.Agent.RetryInterval`.

**Replace `withRetryInterval` with limited retry** in `runRelayRequestOperation`:

```haskell
runRelayRequestOperation vr user uclId =
  withWork_ a doWork (withStore' getNextPendingRelayRequest) $
    \(groupId, rrd@RelayRequestData {relayRequestDelay}) -> do
      ri <- asks $ reconnectInterval . agentConfig . config
      let ri' = maybe ri (\d -> ri {initialInterval = d, increaseAfter = 0}) relayRequestDelay
      withRetryIntervalLimit ri' $ \delay loop -> do
        liftIO $ waitWhileSuspended a
        liftIO $ waitForUserNetwork a
        processRelayRequest groupId rrd `catchAllErrors` retryTmpError loop groupId delay
  where
    maxConsecutiveRetries :: Int
    maxConsecutiveRetries = 3
    withRetryIntervalLimit :: RetryInterval -> (Int64 -> CM () -> CM ()) -> CM ()
    withRetryIntervalLimit ri action =
      withRetryIntervalCount ri $ \n delay loop ->
        when (n < maxConsecutiveRetries) $ action delay loop
    retryTmpError :: CM () -> GroupId -> Int64 -> ChatError -> CM ()
    retryTmpError loop groupId delay = \case
      ChatErrorAgent {agentError} | temporaryOrHostError agentError -> do
        withStore' $ \db -> updateRelayRequestRetries db groupId delay
        loop
      e -> do
        withStore' $ \db -> setRelayRequestErr db groupId (tshow e)
        eToView e
```

Key changes from current code:
- `withRetryInterval` → `withRetryIntervalCount` wrapped in local `withRetryIntervalLimit`
- Resume from stored delay via `ri'` (XFTP pattern)
- `retryTmpError` now takes `delay` parameter, stores it with incremented retry count before calling `loop`
- After `maxConsecutiveRetries` (3), the `when` guard exits, worker picks next item

---

## Files to modify

| File | Change |
|------|--------|
| `src/Simplex/Chat/Store/SQLite/Migrations/M20260429_relay_request_retries.hs` | New migration |
| `src/Simplex/Chat/Store/SQLite/Migrations.hs` | Register migration |
| `src/Simplex/Chat/Store/Postgres/Migrations/M20260429_relay_request_retries.hs` | New migration |
| `src/Simplex/Chat/Store/Postgres/Migrations.hs` | Register migration |
| `simplex-chat.cabal` | Add migration modules |
| `src/Simplex/Chat/Types.hs` | Add `relayRequestDelay` to `RelayRequestData` |
| `src/Simplex/Chat/Store/RelayRequests.hs` | Retry ordering, TTL expiry, `updateRelayRequestRetries` |
| `src/Simplex/Chat/Library/Subscriber.hs` | Limited retry with delay storage |

## Verification

1. **Build**: `cabal build --ghc-options=-O0`
2. **Run relay tests**: `cabal test simplex-chat-test --test-options='-m "relay"'`
3. **Scenarios**:
   - Request to unreachable server: retried 3 times per cycle, pushed to back of queue, aged out after 1 day
   - Request to reachable server: succeeds on first attempt, unaffected by changes
   - Multiple pending requests: stuck request doesn't block others — items with fewer retries processed first
   - App restart with pending requests: `hasPendingRelayRequests` starts worker, old requests marked failed on first iteration

## Known considerations

1. **Single stuck item re-pickup**: If only one request is pending and it's stuck, the worker picks it up repeatedly (3 retries each cycle, immediate re-pickup). This is acceptable — backoff grows via stored delay, and the request ages out after 1 day. The main protection is that other requests aren't blocked.

2. **`hasPendingRelayRequests` unchanged**: Old requests (>1 day) still match the `hasPendingRelayRequests` query at startup. The worker starts, `getNextPendingRelayRequest` marks them failed, then finds no valid items and waits. This is correct — the cleanup happens lazily on first iteration.

3. **Delay resumption across pickups**: Stored delay resumes backoff at the last level (XFTP pattern). After many cycles, delay reaches `maxInterval` and stays there. This means retry frequency stabilizes at a low rate for stuck items.

4. **Permanent errors unchanged**: Non-temp errors (validation, logic) still call `setRelayRequestErr` immediately, permanently excluding the item. The retry mechanism only affects `temporaryOrHostError`.

5. **`withWork_` re-signals work**: After the action returns (hitting max consecutive retries), `withWork_` has already called `hasWork` (re-signaling the doWork TMVar). The outer `forever` loop immediately proceeds to the next iteration. This is the desired behavior — the worker processes all pending items before waiting.

6. **Interaction with `markRelayRequestFailed`**: The `getWorkItem` pattern calls `markRelayRequestFailed` only when `getRelayRequestData` throws an exception (corrupted data). This is orthogonal to the retry mechanism and remains unchanged.

7. **Migration column defaults**: `relay_request_retries DEFAULT 0` ensures existing pending requests start with 0 retries. `relay_request_delay` is nullable (NULL = use default reconnectInterval), matching the `Maybe Int64` field.
