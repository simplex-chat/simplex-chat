# Optimized subscription

## Problem

The `subscribeUserConnections` function has a few problems that affect UX on app start:

1. It loads entity data that isn't used until result processing. This produces a memory spike and takes CPU time to parse all the data.
2. Subscription results are processed synchronously after the agent finishes all the batches for user. The app wouldn't see connections as active until the slowest server responds or timeouts.
3. User subscriptions are processed sequentially. A currently active user is given a first round of subs, but the remaining are blocked. If a user profile is switched right away, the new user may start receiving updates with even more lag.

## Solution

Functions that fetch connections and entities are reduced to return only connection IDs. The filters should be moved from Haskell into specialized SQL queries that only return `[ConnId]`.

With the connection list on hands the agent subscriber thread forks off to do its thing and process batch results. This allows outer loop to start collecting connections for the remaining users.

Successful results are communicated with a new `UP srv conns` message emitted from agent when a batch finishes its processing. The `conns` payload would be a list of connections that actually have just switched subs from "pending" to "active". The UP handling machinery processes status updates just as it would in a server reconnect event. This would update connection state in chat apps as soon as the server responds, keeping app and core in tighter sync.

`reconnectSMPClient` should stop sending UPs to prevent double processing of the same result. The `okConns` membership test it currently uses is the same "did not belong to an active connection" that the batch result would use.

Sending results with UP allows to reduce summary responses to a bunch of counters so no entity data would be needed for CLI here:

```haskell
| CRContactSubSummary {user :: User, okSubs :: Int, errSubs :: Int}
| CRUserGroupLinksSubSummary {user :: User, okSubs :: Int, errSubs :: Int}
| CRMemberSubSummary {user :: User, okSubs :: Int, errSubs :: Int}
| CRPendingSubSummary {user :: User, okSubs :: Int, errSubs :: Int}
```

Subscription errors are reported to API as `CRNetworkStatuses` as ususal, but the active subs are removed from the list as they are already handled by `UP`.

Subscription errors for CLI (when connection error reporting is enabled) are reported with the types reduced to a textual name:

```haskell
| CRContactSubError {user :: User, contactName :: ContactName, chatError :: ChatError}
| CRMemberSubError {user :: User, groupName :: GroupName, contactName :: ContactName, chatError :: ChatError}
| CRSndFileSubError {user :: User, sndFileTransfer :: Text, chatError :: ChatError}
| CRRcvFileSubError {user :: User, rcvFileTransfer :: Text, chatError :: ChatError}
```

> A generic constructor could be used instead, but then it would contain extra fields to form a message in View if matching the current output is desired.

The textual names for connections can be requested for the subset of all connections that needs them with the same procedure that's used in `UP` handling.
