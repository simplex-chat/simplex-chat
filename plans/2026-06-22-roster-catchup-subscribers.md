# Roster catch-up for channel subscribers

Date: 2026-06-22. Builds on the public-groups roster work (privileged roster, member keys, the `VersionRoster` monotonic gate, and the task-047 enrichment of `XGrpMemRole`/`XGrpMemDel` with `Maybe VersionRoster`).

## Problem

In a channel a subscriber learns the full roster once — served by the relay on join and re-served on resume (QCONT/SENT, `serveRoster`, Subscriber.hs:1174/1269) — and thereafter tracks it only through forwarded `XGrpMemRole`/`XGrpMemDel` deltas, each carrying an incrementing `VersionRoster`. The subscriber's monotonic gate (`applyAtRosterVersion`, Subscriber.hs:3248) accepts any delta at version `v >= cur` and advances `roster_version` to `v`.

If the subscriber misses intermediate deltas (a scope-limited or dropped forward, divergence between two relays, a delivery hiccup) but then receives a delta at a version more than one above its known version, it advances the gate to `v` and applies that one change — but the roster changes carried by the skipped versions (other members' promotions, removals, keys) are silently lost until the next join or resume re-serve. A subscriber can then reject a legitimately-promoted member's signed action (`RGEMsgBadSignature`) because it never learned that member's key.

Catch-up makes this self-healing: when a subscriber detects a version gap, it asks its relay(s) to re-serve the full roster, which carries the complete privileged set and their keys at the relay's current version.

## Design

Three pieces, no schema change, no new store function.

### 1. New chat event `XGrpRosterRequest` (Protocol.hs)

A directed control message a subscriber sends to a relay, mirroring `XGrpRosterAck` (also a directed subscriber→relay message, also unsigned, also not forwarded).

```haskell
XGrpRosterRequest :: VersionRoster -> ChatMsgEvent 'Json
```

It carries the subscriber's currently-known roster version (the pre-gap `cur`), so the relay can skip serving when it holds nothing newer. Wire plumbing follows `XGrpRosterAck` exactly:

- `ChatMsgEvent` GADT constructor (after `XGrpRosterAck`).
- `CMEventTag` constructor `XGrpRosterRequest_`.
- `strEncode`: `XGrpRosterRequest_ -> "x.grp.roster.request"`.
- `strP` (`ACMEventTag` parser): `"x.grp.roster.request" -> XGrpRosterRequest_`.
- `toCMEventTag`: `XGrpRosterRequest {} -> XGrpRosterRequest_`.
- `appJsonToCM`: `XGrpRosterRequest_ -> XGrpRosterRequest <$> p "version"`.
- `chatToAppMessage` params: `XGrpRosterRequest v -> o ["version" .= v]`.
- NOT added to `isForwardedGroupMsg` — it is point-to-point, never forwarded into the group.
- NOT added to `requiresSignature` — subscribers (observers) have no member key.

### 2. Subscriber detects the gap and requests (Subscriber.hs, `applyAtRosterVersion`)

`applyAtRosterVersion` already reads `cur`, compares `v >= cur`, and advances the gate. Add gap detection in the same place — it is the one path both `xGrpMemRole` and `xGrpMemDel` share, and it already holds both versions.

In the accepting branch, when the receiver is a subscriber (`not (isUserGrpFwdRelay gInfo)`) and `cur = Just c` with `v > c + 1`, send `XGrpRosterRequest c` to the subscriber's relay members and then proceed with the action unchanged. The delta is a valid owner-signed monotonic change and is still applied (the gate advances to `v`); the requested roster heals the members skipped between `c` and `v`.

Send target and idiom reuse the existing subscriber pattern (Subscriber.hs:3938):

```haskell
relays <- withStore' $ \db -> getGroupRelayMembers db cxt user gInfo
unless (null relays) $ void $ sendGroupMessage' user gInfo relays (XGrpRosterRequest c)
```

Sending to all of the subscriber's relays (not just the one that forwarded the triggering delta) is deliberate: the forwarding relay is not in scope here, relays are few, and a single relay may itself be the stale one. The responses are de-duplicated downstream — each relay's re-serve is an independent per-source transfer, superseded by `cleanupRosterTransfer`, and gated by `notBelowRoster` so only a blob at or above the current version is applied.

Within one event batch the gate advances to `v` on the first gap event, so subsequent `v+1` deltas in the same batch are not gaps — normally at most one request per batch.

### 3. Relay serves on request (Subscriber.hs, `processEvent` + new `xGrpRosterRequest`)

The request arrives at the relay on the direct member connection, handled in the direct `processEvent` dispatch next to `XGrpRosterAck` (Subscriber.hs:1103):

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

This reuses `serveRoster` as-is (the stated intent): it reads the persisted signed roster + blob (`getGroupRoster`) and sends the signed header plus inline blob chunks to the one requesting member — the same per-member path used on join and QCONT resume. The version check serves only when the relay's gate is newer than the requester's known version, which both avoids redundant serves and rate-limits a member that requests at its current version. `serveRoster` is already a no-op when there is no persisted roster, so a non-relay receiving this message does nothing.

## Why the heal is sound, and its bound

The owner re-broadcasts the full signed roster to relays on every privileged change (`broadcastRoster`, called after role-change/removal commands at Commands.hs:2776/2909), so a relay's persisted `roster_blob` tracks the latest version. When a subscriber requests at `c`, the relay serves its current blob (version `>= v` in the steady state), the subscriber's `notBelowRoster` guard accepts it, and `rosterCompletion` applies the full privileged set and keys — healing every member skipped between `c` and `v`.

Bound (documented, not engineered around): there is a narrow race where a relay has applied a delta at `v` (gate `= v`) but its matching full-roster blob transfer from the owner has not yet completed (blob `< v`). A serve in that window delivers a blob below the subscriber's gate and is rejected by `notBelowRoster`, making the request a no-op. Because the owner broadcasts a fresh full roster on every subsequent change — and the relay re-serves on the subscriber's next QCONT/SENT resume — the gap heals at the next change or reconnect regardless. This is strictly better than today (skipped deltas persist until the next join/resume), and the residual window is best-effort by design.

## Files touched

- `src/Simplex/Chat/Protocol.hs` — new event, end to end (7 edit sites above).
- `src/Simplex/Chat/Library/Subscriber.hs` — gap detection in `applyAtRosterVersion`; new `xGrpRosterRequest`; one `processEvent` dispatch line. No change to `processForwardedMsg` (the request is never forwarded).
- No schema change (reuses `roster_version`, `roster_blob`). No new store function (reuses `getGroupRosterVersion`, `getGroupRelayMembers`, `getGroupRoster`, `serveRoster`).
- `tests/ChatTests/` — one channel catch-up test (below).

## Tests

Add a `channels` test asserting that a subscriber which observes a version gap recovers the skipped privileged set. The genuinely hard part is staging a deterministic gap, because normal FIFO forwarding does not drop deltas — the test must construct one. Candidate approach to validate during implementation:

- Two relays. A subscriber receives a later delta from relay B while its gate still reflects an earlier version because an intermediate delta reached only relay A's path (e.g. a member-scoped removal not delivered to this subscriber). Assert the subscriber emits `XGrpRosterRequest`, the relay re-serves, and a member promoted in the skipped interval is afterwards known — verified by that member's signed action being accepted (no `RGEMsgBadSignature`) or by the other-subscriber role line resolving.

If a reliable two-relay gap proves too flaky in the harness, fall back to a focused assertion that a forwarded delta at a jumped version triggers exactly one request and a re-serve. Treat the test design as the riskiest item and confirm it before finalizing. Iterate with `-m "channels"` per the project's minimize-scope convention; add `-m "image"` only if inline-file machinery changes (it should not here).

## Open decisions (recommendation in brackets)

1. Request payload — carry `VersionRoster` so the relay can skip redundant serves and rate-limit, vs. an empty request that always re-serves. [Carry it — small, mirrors `XGrpRosterAck`, defends against a small-request → large-blob amplification.]
2. Send to all of the subscriber's relays vs. only the forwarding relay. [All — relays are few, the forwarding relay is out of scope at the detection point, and a single relay may be the stale one; responses self-dedupe.]
3. Accept the narrow relay-blob-lag race as best-effort (healed on next change/resume) vs. adding machinery to guarantee a blob `>= v` at request time. [Accept — the guarantee would add cost to the roster/serve path for a self-healing window.]

## Commit split

1. Protocol: add `XGrpRosterRequest` end to end. Builds; no behavior change yet.
2. Subscriber: relay handler `xGrpRosterRequest` + `processEvent` dispatch (relay can serve on request).
3. Subscriber: gap detection + request send in `applyAtRosterVersion` (subscriber drives catch-up).
4. Test + schema/plan regen.

Build and run `-m "channels"` after each step.
