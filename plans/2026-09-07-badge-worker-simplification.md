# Badge worker: one thread, one sleep

Today each user has two threads: a worker, and a sleeper that outlives each pass and signals it. That is where the `Weak ThreadId`, the swap-and-kill in `scheduleBadgeWake`, and the sleeper half of `stopBadgeWorkers` come from. The next wake is the earliest of four separately-derived times — a retry after a failed request, the snooze expiry, the stall floor, and the ledger's next renewal — and behind the first sit a per-purchase `(elapsed, delay)` map and three functions to advance it.

This assumes one badge per user. Most of what collapses here collapses because the maps keyed by purchase have a single key, and because `updateUserBadges` and `updateBadgePurchase` become one function, `updateUserBadge`.

```haskell
data BadgeWorker = BadgeWorker
  { badgeWorkerAsync :: Async (),
    badgeWork :: TMVar ()
  }

badgeWorkers :: TMap UserId (SessionVar BadgeWorker)
badgeSeq :: TVar Int
```

The loop, which the rest of this fills in:

```haskell
runBadgeWorker :: User -> TMVar () -> CM ()
runBadgeWorker user badgeWork = do
  emitted <- newTVarIO Nothing
  ri <- asks $ badgeRetryInterval . config
  forever $ do
    at_ <- withRetryInterval ri $ \_ loop -> do
      now <- badgeNow
      let done = pure $ Just $ badgeStalledInterval `addUTCTime` now
      updateUserBadge user emitted now `catchAllErrors` retryBadgeError loop done
    now <- badgeNow
    liftIO $ waitBadgeWake badgeWork now at_
```

The clock is read twice because a retry sequence can run for hours, so the wait needs a fresh reading rather than the one the pass started from.

## Waiting

`registerDelay` makes the deadline an STM value, so the wait is a single transaction over the timer and the signal — no second thread, and the transaction reports which of the two woke it:

```haskell
waitBadgeWake :: TMVar () -> UTCTime -> Maybe UTCTime -> IO ()
waitBadgeWake badgeWork now = \case
  Nothing -> atomically $ takeTMVar badgeWork
  Just at -> waitFor $ diffToMicroseconds $ diffUTCTime at now
  where
    waitFor time
      | time <= 0 = pure ()
      | otherwise = do
          let maxWait = min time $ fromIntegral (maxBound :: Int)
          timer <- registerDelay $ fromIntegral maxWait
          signalled <- atomically $ do
            w <- tryTakeTMVar badgeWork
            fired <- readTVar timer
            unless (isJust w || fired) retry
            pure $ isJust w
          unless signalled $ waitFor $ time - maxWait
```

This is `threadDelay'` with an escape hatch, and it keeps that function's names so it reads as one. Nothing survives the wait, so there is no handle to keep and nothing to kill. `Nothing` means sleep until signalled.

`registerDelay` takes an `Int`, so the loop is needed for the same reason `threadDelay'` has one: `maxBound :: Int` is ~292,000 years on 64-bit and ~36 minutes on 32-bit. A month is one sleep everywhere except legacy armv7a, which re-arms — as it already does today. Counting down the remaining time rather than re-reading the clock keeps this correct when a test has shifted `badgeCurrentTime`.

Taking the signal and reading the timer in one transaction means a signal is never consumed and discarded.

## Retrying

`withRetryInterval` holds the backoff in its own recursion — no map, no `TVar`, nothing persisted:

```haskell
retryBadgeError :: CM a -> CM a -> ChatError -> CM a
retryBadgeError loop done e = eToView e >> if badgeErrorRetry e then loop else done
```

Same shape as `retryOnError` (`FileTransfer/Agent.hs:260`), except that it classifies with `badgeErrorRetry` rather than `temporaryOrHostError`, which does not cover `AGENT (A_SERVICE ASETimeout)` — the likeliest renewal failure, and using it would leave the retry inert for the common case.

`updateUserBadge` returns the next wake, as `updateBadgePurchase` does today. Retry-or-stop is a property of the error rather than a return value, so no result type is needed and `BadgeRetry` collapses to `badgeErrorRetry :: ChatError -> Bool`.

A service error is the exception, since it is returned rather than thrown: `requestBadgeIssue` becomes `Either (Maybe UTCTime) LedgerBalance`, where `Left` means do not retry and wake then. That is where a `retryAfter` hint lands — `withRetryInterval` owns the sleeping, so a wait the service names becomes a wake time — and where a terminal code lands as the stall floor. Floor the hint at `initialInterval`, since a service answering `0` would otherwise spin the worker, but do not cap it: a service that names a long wait is denying issuance, which it can already do by refusing, and `paidThrough` remains a wake candidate regardless of what it says.

**The stall floor is a day.** A terminal failure is not retried, and the ledger boundary is a whole term away. The client's own state cannot change without the service, so the only thing that can make the next attempt succeed is the service being repaired — a day is slow enough not to press a service already failing, and fast enough to recover well inside the badge's 8-14 days of headroom.

**The loop cannot die.** Catching everything except cancellation means no exception ends it, and a failure that reaches the top returns the stall floor as its wake, so a persistent fault is one attempt a day rather than a hot loop. That replaces the `Worker`'s rate-limited restart.

## When renewal is due

Renewal is driven by the **credential's expiry**, not the ledger's period end. A badge whose period has ended but whose credential is still valid needs nothing done — the holder keeps their perks, and the service is not asked early.

The expiry rounding moves one day later, from the Monday after the period to the Tuesday: `endOfSundayAfter`'s `8 - dayOfWeek` becomes `9 - dayOfWeek`. Periods ending anywhere in one Monday-to-Sunday week still share a single expiry, so the anonymity set is unchanged; it now falls on a weekday in every timezone, where Monday 00:00 UTC is Sunday evening in the Americas and puts a failed renewal on a weekend for support. The function is renamed for the day it now returns, and the range in `testSundayExpiry` widens from 1-7 days to 2-8.

Renewal splits into two steps, normally a day apart:

- **Request**, on the Monday — while the held credential is still valid, so a failure has a day of slack before anything is visible.
- **Present**, on the Tuesday — as the old credential lapses, so the profile broadcast does not correlate with the request that produced it.

Neither needs a marker. Both derive from the shown credential's expiry, which the profile already stores, and from whether a newer issuance exists, which `presentIssuedBadge` already compares:

- **Request** when the shown credential expires within a day, months remain, and the newest issuance is still the one shown — so nothing has been requested yet. A successful request moves the newest expiry a month out, and the condition stops holding by itself.
- **Present** when the newest issuance differs from what is shown and the shown credential has expired — or when nothing is shown at all, which is the state a crash between the issuance write and the profile write leaves behind, and which `testPresentationCatchesUp` covers.

Ordering falls out of that: presenting cannot precede requesting, because nothing differs until the request succeeds.

`badgeBoundary` becomes the next of three moments: `shownExpiry - 1 day`, `shownExpiry`, and `paidThrough`. The ledger's `balanceStartTs` goes, since renewal is no longer month-aligned — but `paidThrough` stays, and for a different reason from the other two. The credential's expiry window is what covers renewal: the client renews around it to join the anonymity set, and the recipients' grace period keeps the badge honoured while that happens. `paidThrough` is when entitlement itself ends. The worker has to be there for it, to retire the badge and raise the alert that tells the user to buy again; waiting for the credential to expire would leave them wearing a badge they have stopped paying for.

Missing a week is safe. A worker whose first run is the Wednesday finds both conditions true and does both in one pass: that renewal loses its anonymity benefit, and nothing else changes, which is the same property every other wake here has.

## The pass

```haskell
updateUserBadge :: User -> TVar (Maybe BadgeOccurrence) -> UTCTime -> CM (Maybe UTCTime)
updateUserBadge user emitted now = do
  (p, balance) <- ...
  retired <- retireExpiredBadge user p now balance
  balance' <-
    if retired
      then pure balance
      else do
        b <- if requestDue p balance now then requestBadgeIssue ... else pure balance
        when (presentDue p now) $ presentIssuedBadge user p now
        pure b
  emitBadgeAlert user emitted p now balance'
  pure $ earliestTime [badgeBoundary now p balance', snoozeAt p now]
```

Retiring ends the renewal half of the pass. It means `paidThrough <= now`, so every funded month has already passed and a request could only write lapse rows — it cannot issue, because `advanceBalance` consumes the balance before `issueMonth` sees it. Reconciling the ledger for a badge that is over is not worth a round trip; a later redemption reconciles it anyway by asserting the last entry.

The guard is on `retired` rather than on the purchase's `shown` field because `p` was read before retirement and its `shown` is stale within the pass. `presentIssuedBadge` has its own `not shown` check, which covers later passes but not this one.

The alert stays outside the guard: support-ended fires exactly when the balance is exhausted, which is the pass that retires.

Retirement comes first because it needs no service and reads only stored state, and because an unbounded retry does not return while a failure lasts — anything after the request is unreachable meanwhile. Moving it ahead of the request is behaviour-preserving: `paidThrough` is invariant under issue and lapse, only a grant moves it, and `BSCIssueBadge` never produces one, so it gives the same answer either side.

## Starting and stopping

Chat start, `/_app activate`, `APIGetBadgeState` and a redemption can each ask for the worker at the same moment. Exactly one thread must be started, and every caller must come away holding it.

`getAgentWorker'` manages that by doing the lookup and the create in a single STM transaction, which works only because creating a `Worker` allocates a few TVars and nothing else. Starting a thread is IO and cannot happen inside a transaction, so for us the lookup and the create come apart, and two callers can both find nothing.

`SessionVar` closes the gap by putting a `TMVar` in the map instead of the value — the map holds the promise of a worker rather than a worker. `getSessVar` either inserts an empty one and returns `Left`, meaning you are the creator, or finds an existing one and returns `Right`. Exactly one caller gets `Left`.

```haskell
withGetSessVar' badgeSeq userId badgeWorkers now startWorker signalExisting
  where
    startWorker v = do
      badgeWork <- newTMVarIO ()          -- full: a new worker has work to do
      a <- async $ runBadgeWorker user badgeWork
      let w = BadgeWorker {badgeWorkerAsync = a, badgeWork}
      w <$ atomically (putTMVar (sessionVar v) w)
    signalExisting v = do
      w <- atomically $ readTMVar $ sessionVar v
      w <$ atomically (void $ tryPutTMVar (badgeWork w) ())
```

`readTMVar` blocks the other callers until the creator fills the var, so they signal the one worker rather than starting a second. The hazard is the creator dying between those two steps, leaving an empty var everyone waits on forever; `withGetSessVar'` wraps the creating branch in `bracketOnError` and drops it from the map so the next caller creates a fresh one.

Shutdown follows `closeAgentClient`, which stops a `TMap k (SessionVar (Async ()))` the same way: swap the map out, then for each var fork a thread that waits on `readTMVar` and `uninterruptibleCancel`s what it finds. Waiting rather than skipping is what catches a worker created after the swap; the fork is so shutdown does not block on it.

## What goes away

| removed | replaced by |
| --- | --- |
| `Worker`, `getAgentWorker'`, `cancelWorker`, restart accounting | `Async`, `badgeWork`, `SessionVar` |
| `scheduleBadgeWake`, `Weak ThreadId`, `killWeakThread`, sleeper cleanup | `waitBadgeWake` |
| `BadgeAttempt`, `nextBadgeAttempt`, `badgeAttemptDelay` | `withRetryInterval`'s own recursion |
| `BadgeMemory` and its two maps | one `TVar (Maybe BadgeOccurrence)`, once a user has one badge |
| `BadgeRetry`'s two constructors | `badgeErrorRetry :: ChatError -> Bool` |

Wake candidates go from four to three. The one that goes is the retry, because `withRetryInterval` sleeps between attempts rather than returning a time; the next request or present day, the snooze expiry, and the stall floor — or a wait the service named — all remain.

Unchanged: everything derived from stored state so a wake early, late or missed changes only timing; the alert-occurrence memory; `badgeCurrentTime`; `RetryInterval` in config, both as the existing pattern and so a test can shorten it. Signalling `badgeWork` replaces `startBadgeWork` at the same call sites.
