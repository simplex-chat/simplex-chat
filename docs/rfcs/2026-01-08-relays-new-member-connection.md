# Connection of new member to chat relays

## Problem

Naive implementation of new member connection to chat relays can lead to partial failures (some relays fail to connect), or requires recovery or clean up.

After group record is prepared from short link, naive flow is as follows (APIConnectPreparedGroup):

```
User clicks "Connect"
  -> Fetch relay links from group link (sync getConnShortLink)
  -> For each relay:
      -> Fetch ConnectionRequestUri from relay link (sync getConnShortLink)
      -> Join connection (sync joinConnection)
```

Orthogonal smaller problem:

If new member chooses to connect to group incognito, same incognito profile should be sent to all group relays.

## Solution

### Join Connection step

"Join connection" is the main step, let's consider it first.

#### Option 1: Synchronous approach with catches + recovery

Keep all relay connections synchronous, catch on failure to continue for remaining relays, recovery for failed relays. All relays failing would mean full command failure, offer user retry.

For partial failures it would require to track which relays succeeded/failed, then trigger recovery, basically recreating what asynchronous command processing already does.

#### Option 2: First relay sync, then async

Connect to first relay synchronously, connect to remaining asynchronously (using joinConnectionAsync).

Choice of "first" relay is arbitrary and we may be choosing the one with worse network.

Mixed (double) implementation - for "first" and remaining relays.

#### Option 3: All relays async

In this case agent already handles connection reliability, downside is no immediate failure visible to user on temporary network errors for all relays (for example, client is offline).

UI already handles "connecting..." state, so async path doesn't hurt UX much other than in mentioned case. UI stays in "connecting..." until at least one relay connection succeeds.

If all relay connections permanently fail, update state for UI - requires permanent error handling for connection creation on continuation (agent responses in Subscriber). Track relay connection states to detect "all failed", possibly on connection status, TBC at implementation.

Pros:
- Simple flow: loop through relays, start async connections.
- Async agent commands provide recovery.

### Link fetches

We considered handling retries for Join step, but no retry mechanism for link fetch. If it's synchronous and fails for a given relay, it would result in permanent failure to connect to relay, without additional recovery logic.

#### Option 1: Asynchronous command with continuation

New agent asynchronous command + complexity in chat Subscriber logic. Seems overkill.

#### Option 2: Per-relay "relay connection" worker

An additional state machine, possibly based on relay member records as work items. Also overkill.

#### Option 3: Make all link fetches synchronously before proceeding

To avoid adding background recovery mechanisms for link fetching per relay, we could fetch all links data synchronously, and only then connect to relays asynchronously.

In case any relay link fetch fails, user would be given option to retry. (Whole operation fails and is retried)

Group link fetch is also synchronous (retrieve list of relay links), and also leads to immediate user retry.

### On the incognito profile issue

This should be addressed regardless of which approach to connection we choose. The incognito profile should be:

1. Created once before starting any relay connections;
2. Passed to all relays on connection attempts.

In case of synchronous approach and re-use of existing logic, it means `connectViaContact` should accept an optional profile (not just flag).

### Overall proposed connection flow

```
User clicks "Connect"
  -> Fetch relay links from group link (sync getConnShortLink)
  -> For each relay: Fetch ConnectionRequestUri from relay link (sync getConnShortLink)
  -> Once all links are resolved, proceed - create incognito profile ONCE for all relays, if needed
  -> For each relay: Start async connection attempt (joinConnectionAsync)
  -> Agent handles connection retries internally
  -> Subscriber handles JOINED events and errors for each relay
     - At least one relay JOINED -> group becomes functional
     - All relays permanently fail -> show failure to user
```

Link fetches being synchronous in conjunction with asynchronous relay connections allows for similar UI reactivity to current single-connection flows:
- Network failures during link fetches require user retry;
- Connection attempts are retried by agent on network failures;
- Link fetches passing ensures client is not offline when starting async connection attempts (unless user goes offline in-between, but window is very small, and connections would be retried anyway).
