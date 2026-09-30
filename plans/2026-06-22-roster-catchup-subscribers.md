# Roster catch-up for channel subscribers

Continues the public-groups roster work (privileged roster, member keys, `VersionRoster` monotonic gate, task-047's `Maybe VersionRoster` on `XGrpMemRole`/`XGrpMemDel`).

## Problem

A channel subscriber learns the full roster only on join and on resume (`serveRoster`, Subscriber.hs:1174/1269), then tracks it via forwarded `XGrpMemRole`/`XGrpMemDel` deltas, each carrying an incrementing `VersionRoster`. The gate (`applyAtRosterVersion`, Subscriber.hs:3248) accepts any delta at `v >= cur` and advances to `v`.

If the subscriber misses intermediate deltas and then receives one at a version more than one above its known version, it advances to `v` and applies that one change — but the roster changes carried by the skipped versions (other members' roles, removals, keys) are lost until the next resume. The subscriber can then reject a freshly-promoted member's signed action (`RGEMsgBadSignature`) for a key it never learned.

Catch-up: on detecting a gap, the subscriber asks the relay that forwarded the delta to re-serve the full roster, which carries the complete set and keys.

## Design

Four pieces. No schema change, no new store function.

### 1. Owner sends the roster before the delta (the enabler)

Today the owner sends `XGrpMemRole`/`XGrpMemDel` first, then `broadcastRoster` (Commands.hs:2754/2885). The relay forwards the delta to subscribers *before* its own roster-blob transfer completes, so for the whole transfer its stored `roster_blob` lags its `roster_version`. A catch-up request in that window serves a stale blob the subscriber rejects (`notBelowRoster`) — which is the common case, not an edge.

Fix: in `APIMembersRole` / `APIRemoveMembers`, reorder to **apply the change to the owner's members → `broadcastRoster v` → send the delta**. This restructures `changeRoleCurrentMems` / `deleteMemsSend` to separate "apply to owner DB" from "send delta to relays" (the roster is still built after the change, so it reflects the new roles / excludes the removed member).

Effect, relying on FIFO order of the owner→relay connection (a guarantee the roster design already assumes): the relay applies and stores the blob at `v` (`setGroupLiveRoster`) before it processes and forwards the delta at `v`. So a relay's `roster_version` always reflects its stored blob, and any request triggered by a forwarded delta finds a current blob. The now-redundant delta hits the existing no-op suppression (`fromRole == memRole`, Subscriber.hs:3298, added in task-047) and is still forwarded to subscribers — no new relay logic, subscribers still see the delta.

Behavioral change to core delivery (all relays/subscribers) → its own commit. Implementation must confirm FIFO holds owner→relay (the test will expose a violation). Residual: a failed roster send (rare; already `catchAllErrors`) leaves that relay's gate briefly above its blob until the next change — heals on resume.

### 2. New event `XGrpRosterRequest VersionRoster` (Protocol.hs)

A directed subscriber→relay control message carrying the subscriber's pre-gap version, so the relay can skip serving when it holds nothing newer.

```haskell
XGrpRosterRequest :: VersionRoster -> ChatMsgEvent 'Json
```

Wire plumbing (standard single-`VersionRoster` `'Json` event): GADT constructor; `CMEventTag` `XGrpRosterRequest_`; `strEncode` `"x.grp.roster.request"`; `strP` case; `toCMEventTag`; `appJsonToCM` (`XGrpRosterRequest <$> p "version"`); `chatToAppMessage` (`o ["version" .= v]`). NOT in `isForwardedGroupMsg` (point-to-point). NOT in `requiresSignature` (subscribers/observers have no key).

### 3. Subscriber detects the gap and requests (Subscriber.hs, `applyAtRosterVersion`)

`applyAtRosterVersion` already reads `cur`, compares `v >= cur`, advances the gate — the one path shared by `xGrpMemRole` and `xGrpMemDel`. In its accepting branch, when the receiver is a subscriber (`not (isUserGrpFwdRelay gInfo)`) and `cur = Just c` with `v > c + 1`, send `XGrpRosterRequest c` to the relay that forwarded the delta, then run the action unchanged (the delta is still applied; the roster heals `c+1 .. v-1`).

To target the forwarding relay, thread it as `Maybe GroupMember` through `xGrpMemRole` / `xGrpMemDel` / `applyAtRosterVersion`: `Just m` on the forwarded path (`m` in scope in `processForwardedMsg`, Subscriber.hs:3804/3806), `Nothing` on the direct path (1092/1096; the receiver there is a relay and never requests). Send via `sendGroupMessage' user gInfo [relay] (XGrpRosterRequest c)`.

One forwarding relay only, not all relays: avoids N² requests under concurrent multi-relay gaps, and the owner-signed roster needs no cross-relay verification. The gate advances to `v` on the first gap in a batch, so later `+1` deltas aren't gaps — normally one request per batch.

### 4. Relay serves on request (Subscriber.hs, `processEvent` + new `xGrpRosterRequest`)

The request arrives on the direct member connection, dispatched in `processEvent` (alongside other direct group events, ~Subscriber.hs:1103):

```haskell
XGrpRosterRequest reqVer -> Nothing <$ xGrpRosterRequest gInfo' m'' reqVer
```

```haskell
xGrpRosterRequest :: GroupInfo -> GroupMember -> VersionRoster -> CM ()
xGrpRosterRequest gInfo m reqVer =
  when (isUserGrpFwdRelay gInfo) $ do
    cur <- withStore' $ \db -> getGroupRosterVersion db gInfo
    when (maybe True (> reqVer) cur) $ serveRoster user gInfo m
```

Reuses `serveRoster` unchanged (signed header + inline blob chunks to the one requester — the join/resume path). The version check serves only when the relay holds something newer than the requester, rate-limiting same-version spam. With piece 1, the served blob is current, so the subscriber accepts it (`notBelowRoster`) and `rosterCompletion` heals the skipped set and keys. `serveRoster` is a no-op without a stored roster.

## Files touched

- `src/Simplex/Chat/Library/Commands.hs` — reorder owner sends in `APIMembersRole`/`APIRemoveMembers` (restructure `changeRoleCurrentMems`/`deleteMemsSend`).
- `src/Simplex/Chat/Protocol.hs` — new event, end to end.
- `src/Simplex/Chat/Library/Subscriber.hs` — gap detection + request in `applyAtRosterVersion`; thread forwarding relay through `xGrpMemRole`/`xGrpMemDel`; new `xGrpRosterRequest`; one `processEvent` dispatch line.
- No schema change, no new store fn (reuses `getGroupRosterVersion`, `getGroupRelayMembers`, `getGroupRoster`, `serveRoster`).
- `tests/ChatTests/` — one channel catch-up test.

## Tests

A `channels` test asserting a subscriber that observes a version gap recovers the skipped set. The hard part is staging a deterministic gap (FIFO forwarding doesn't drop deltas). Candidate: two relays where an intermediate delta reaches only one path, so the subscriber receives a later delta at a jumped version; assert it emits exactly one `XGrpRosterRequest`, the relay re-serves, and a member promoted in the skipped interval is afterwards known (its signed action accepted, no `RGEMsgBadSignature`). Confirm the gap is reliably reproducible before finalizing; fall back to asserting "a forwarded delta at a jumped version triggers one request + re-serve" if the two-relay setup is flaky. Iterate with `-m "channels"`.

## Commits

1. Reorder owner sends (roster before delta) — relay blob is current before it forwards deltas.
2. Protocol: add `XGrpRosterRequest`.
3. Relay: `xGrpRosterRequest` + `processEvent` dispatch.
4. Subscriber: gap detection + request to the forwarding relay.
5. Test + schema/plan regen.

Build and run `-m "channels"` after each.
