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
5. **Expire old requests**: On temp error, before retrying, check if the request is older than 1 day and has 10+ retries — if so, mark as failed instead of retrying. Both conditions must hold — a request that's old but has few retries may just have been delayed, while a request with many retries that's recent is still being actively worked on.

### How this neutralizes the attack

- Attacker's request gets picked up, retried 3 times with backoff (~15s total), then yielded
- Worker picks the next item by retry count — legitimate requests (retries=0) go first
- Attacker's request accumulates retries, always processed last
- After 1 day and 10+ retries, the request is marked failed and permanently excluded

---

## Detailed changes

### 1. Database migration

New migration: `M20260429_relay_request_retries.hs`

```sql
ALTER TABLE groups ADD COLUMN relay_request_retries INTEGER NOT NULL DEFAULT 0;
ALTER TABLE groups ADD COLUMN relay_request_delay INTEGER;
```

**Files:**
- `src/Simplex/Chat/Store/SQLite/Migrations/M20260429_relay_request_retries.hs` (new)
- `src/Simplex/Chat/Store/SQLite/Migrations.hs` (register)
- `src/Simplex/Chat/Store/Postgres/Migrations/M20260429_relay_request_retries.hs` (new)
- `src/Simplex/Chat/Store/Postgres/Migrations.hs` (register)
- `simplex-chat.cabal` (add modules)

### 2. Extend RelayRequestData

**File:** `src/Simplex/Chat/Types.hs`

```haskell
data RelayRequestData = RelayRequestData
  { relayInvId :: InvitationId,
    reqGroupLink :: ShortLinkContact,
    reqChatVRange :: VersionRangeChat,
    relayRequestDelay :: Maybe Int64,
    relayRequestRetries :: Int,
    relayRequestCreatedAt :: UTCTime
  }
```

- `relayRequestDelay`: resume backoff from stored position (XFTP pattern)
- `relayRequestRetries`: current retry count, used with `relayRequestCreatedAt` to decide expiry in `retryTmpError`
- `relayRequestCreatedAt`: group creation time, used for the 1-day expiry check

### 3. Update store functions

**File:** `src/Simplex/Chat/Store/RelayRequests.hs`

**`getNextPendingRelayRequest`** — two changes:
- Order by `relay_request_retries ASC, created_at ASC` instead of `group_id ASC`
- SELECT and return `relay_request_delay`, `relay_request_retries`, `created_at` in the data query

```haskell
getNextPendingRelayRequest db =
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
                   relay_request_delay, relay_request_retries, created_at
            FROM groups WHERE group_id = ?
          |]
          (Only groupId)
      where
        toRelayRequestData (Just relayInvId, Just reqGroupLink, Just minV, Just maxV, relayRequestDelay, relayRequestRetries, relayRequestCreatedAt) =
          Right (groupId, RelayRequestData {relayInvId, reqGroupLink, reqChatVRange = fromMaybe (versionToRange maxV) $ safeVersionRange minV maxV, relayRequestDelay, relayRequestRetries, relayRequestCreatedAt})
        toRelayRequestData _ = Left $ SEInternalError "missing relay request data"
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

Export `updateRelayRequestRetries` and `markRelayRequestFailed` from module (the latter is currently internal, used only as a callback in `getWorkItem`).

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
        processRelayRequest groupId rrd `catchAllErrors` retryTmpError loop groupId rrd delay
  where
    maxConsecutiveRetries :: Int
    maxConsecutiveRetries = 3
    withRetryIntervalLimit :: RetryInterval -> (Int64 -> CM () -> CM ()) -> CM ()
    withRetryIntervalLimit ri action =
      withRetryIntervalCount ri $ \n delay loop ->
        when (n < maxConsecutiveRetries) $ action delay loop
    retryTmpError :: CM () -> GroupId -> RelayRequestData -> Int64 -> ChatError -> CM ()
    retryTmpError loop groupId RelayRequestData {relayRequestRetries, relayRequestCreatedAt} delay = \case
      ChatErrorAgent {agentError} | temporaryOrHostError agentError -> do
        currentTs <- liftIO getCurrentTime
        if relayRequestRetries >= 10 && diffUTCTime currentTs relayRequestCreatedAt > nominalDay
          then withStore' $ \db -> markRelayRequestFailed db groupId
          else do
            withStore' $ \db -> updateRelayRequestRetries db groupId delay
            loop
      e -> do
        withStore' $ \db -> setRelayRequestErr db groupId (tshow e)
        eToView e
```

Key changes from current code:
- `withRetryInterval` → `withRetryIntervalCount` wrapped in local `withRetryIntervalLimit`
- Resume from stored delay via `ri'` (XFTP pattern)
- `retryTmpError` receives the full `RelayRequestData` record and destructures the fields it needs
- On temp error: checks if request is older than 1 day with 10+ retries — if so, marks as failed instead of retrying; otherwise increments retries and calls `loop`
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
| `src/Simplex/Chat/Types.hs` | Add `relayRequestDelay`, `relayRequestRetries`, `relayRequestCreatedAt` to `RelayRequestData` |
| `src/Simplex/Chat/Store/RelayRequests.hs` | Retry ordering, `updateRelayRequestRetries` |
| `src/Simplex/Chat/Library/Subscriber.hs` | Limited retry with delay storage, expiry check in `retryTmpError` |

## Verification

1. **Build**: `cabal build --ghc-options=-O0`
2. **Run relay tests**: `cabal test simplex-chat-test --test-options='-m "relay"'`
3. **Scenarios**:
   - Request to unreachable server: retried 3 times per cycle, pushed to back of queue, marked failed after 1 day and 10+ retries
   - Request to reachable server: succeeds on first attempt, unaffected by changes
   - Multiple pending requests: stuck request doesn't block others — items with fewer retries processed first
   - App restart with expired pending requests: worker starts, picks up expired request, attempts it — if it succeeds (server now reachable), completes normally; if it fails, `retryTmpError` marks it failed

## Known considerations

1. **Single stuck item re-pickup**: If only one request is pending and it's stuck, the worker picks it up repeatedly (3 retries each cycle, immediate re-pickup). This is acceptable — backoff grows via stored delay, and the request is marked failed after 1 day and 10+ retries. The main protection is that other requests aren't blocked.

2. **`hasPendingRelayRequests` unchanged**: Expired requests still match the `hasPendingRelayRequests` query at startup, so the worker starts. It picks them up, attempts processing — if the server became reachable, the request succeeds normally. If it fails, `retryTmpError` checks the expiry condition and marks it failed. This is strictly better than filtering at query time: expired items get one last chance.

3. **Delay resumption across pickups**: Stored delay resumes backoff at the last level (XFTP pattern). After many cycles, delay reaches `maxInterval` and stays there. This means retry frequency stabilizes at a low rate for stuck items.

4. **Permanent errors unchanged**: Non-temp errors (validation, logic) still call `setRelayRequestErr` immediately, permanently excluding the item. The retry mechanism only affects `temporaryOrHostError`.

5. **`withWork_` re-signals work**: After the action returns (hitting max consecutive retries), `withWork_` has already called `hasWork` (re-signaling the doWork TMVar). The outer `forever` loop immediately proceeds to the next iteration. This is the desired behavior — the worker processes all pending items before waiting.

6. **`retries` count is from pickup time**: The `relayRequestRetries` value in `retryTmpError` is the count loaded when the item was picked up. Within a single pickup cycle (up to 3 consecutive retries), `updateRelayRequestRetries` increments the DB count but the local value stays the same. The expiry check uses the pickup-time count, which is at most 3 behind the DB. This is acceptable — the threshold (10) has margin.

7. **Migration column defaults**: `relay_request_retries NOT NULL DEFAULT 0` ensures existing pending requests start with 0 retries. `relay_request_delay` is nullable (NULL = use default reconnectInterval), matching the `Maybe Int64` field.
