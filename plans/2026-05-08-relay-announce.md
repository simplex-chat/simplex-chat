# Plan: owner-pushed relay announcement (`XGrpRelayNew`)

## Goal
Subscribers learn of newly added relays immediately via an owner-pushed event,
rather than only on next channel open via `syncSubscriberRelays`.

## Wire-format
- New event: `XGrpRelayNew :: ShortLinkContact -> ChatMsgEvent 'Json`, tag `x.grp.relay.new`.
- Add to `isForwardedGroupMsg` in `Protocol.hs`.
- Add to required-signed-by-owner table in `docs/protocol/channels-protocol.md`.
  Reuses existing `CBGroup`-prefixed signing infrastructure.

## Owner — send site
- In LINK callback at `Subscriber.hs:1305-1322`, after the fold over relays
  that drives `RSAccepted → RSActive` transitions.
- Collect `relayLink` for every relay that transitioned to Active in this callback.
- If non-empty, build `events = XGrpRelayNew rl1 :| [XGrpRelayNew rl2, ...]`
  and call `sendGroupMessages user gInfo Nothing False otherRelays events`.
- Recipients: channel's currently-connected relays minus the newly-active ones
  (the announced relays don't need self-announcement).
- Batched shape is defensive, not load-bearing. The receive-loop group lock
  serializes `XGrpRelayAcpt` handling and the subsequent
  `setGroupLinkDataAsync` → LINK chain, so each LINK callback typically
  transitions at most one relay. Coding the send as a `NonEmpty` of
  `XGrpRelayNew` events keeps the path correct if the agent ever consolidates
  link-data writes.

## Relay — forward only
- `processEvent` (Subscriber.hs:980-1032) gets a new case:
  `XGrpRelayNew _ -> pure $ Just (DeliveryTaskContext (DJSGroup ...) False)`.
- No local handler — relay does NOT create a member record for the announced
  relay (departure from `XGrpMemNew` semantics; relays don't connect to other
  relays of the same channel).
- Forwarding is verbatim through binary-batch format, signature preserved.
- Old relay (no tag): `_ -> messageError "unsupported"` path drops the message.
  Fallback: subscriber's on-open `syncSubscriberRelays` still works.

## Subscriber — receive
- Add case in `processForwardedMsg` (Subscriber.hs:3357-3378):
  `XGrpRelayNew rl -> withAuthor XGrpRelayNew_ $ \author -> connectToRelayAsync user gInfo rl`.
- Author resolution + signature verification via existing `withAuthor` /
  `withVerifiedMsg` machinery — same boundary as `XGrpInfo` etc. today.
- `FwdChannel` (channel-as-sender) is NOT valid for this event — it is
  administrative and must be attributed to a signing owner.

## Subscriber — `connectToRelayAsync` helper
Place in `Internal.hs`. Both event handler and `syncSubscriberRelays` call it.
Body:

  1. Look up active (`memberCurrent`) relay-member row by `relay_link`.
     - If found AND has active connection → skip (already in flight or done).
     - If found but no active connection → use it; proceed.
     - If not found → create new relay-member row (with `relay_link`, role
       `GRRelay`, status `GSMemAccepted`, no member-id/key/profile yet).
  2. `getAgentConnShortLinkAsync user CFGetRelayDataJoin Nothing relayLink`
     → returns `(cmdId, agentConnId)`.
  3. `createRelayMemberConnectionAsync` binds those to the relay-member.
  4. Return. Continuation is the existing `CFGetRelayDataJoin` LDATA callback
     at Subscriber.hs:1131-1160 (updates relay-member with member-id/key/profile,
     calls `joinAgentConnectionAsync` → eventual `CON` flips status to
     `GSMemConnected`).

A `GSMemLeft` historical row for the same `relay_link` is left in place
(displays "removed by operator"). Lookup must filter to `memberCurrent`.

## Idempotence and races
- Receive loop wraps each agent message in `withEntityLock` keyed by the
  connection's lock entity (Subscriber.hs:115-117). Relay-member connections
  resolve to `CLGroup groupId` (Store/Connections.hs:51-65).
- `APIGetUpdatedGroupLinkData` already uses `withGroupLock "syncSubscriberRelays" groupId`.
- Same key on both paths → event handler and open-channel command cannot
  interleave for a given group_id. No additional lock needed.
- Inside the lock, "active row + active conn" check is sufficient. No
  `justCreated` flag, no per-link mutex.

## `syncSubscriberRelays` migration
- Move from `Commands.hs` to `Internal.hs`.
- "Add" half: replace synchronous `connectToRelay` with `connectToRelayAsync`.
- "Remove" half (Commands.hs:3623-3633): unchanged.
- `connectToRelay` (sync) deletable once event-driven path is wired and
  no caller remains.

## Old client compatibility
- Old subscriber: parses `XGrpRelayNew` as `XUnknown`, ignores. On-open
  `syncSubscriberRelays` is the fallback path.
- Old relay: drops the message in `processEvent`'s default branch. Subscribers
  on those relays fall back to on-open sync. Acceptable graceful degradation.

## Test surface
- Owner adds relay → existing subscribers (online) receive `XGrpRelayNew` and
  connect without channel open.
- Channel with two existing relays: owner adds a third relay; both existing
  relays forward `XGrpRelayNew` for the new relay to subscribers in parallel
  → shared-msg-id dedup leaves only one copy reaching the helper; subscriber
  connects to the announced relay exactly once.
- `XGrpRelayNew` arrives while subscriber is mid-`APIGetUpdatedGroupLinkData`
  for the same relay → group lock serializes; no double connection.
- Subscriber re-add scenario: previous `GSMemLeft` row for same `relay_link`
  → new active row created, old row preserved for history.
- Old subscriber receives forwarded `XGrpRelayNew` → ignored, channel-open
  sync still recovers.
- Owner with old relay → relay drops the event; subscribers learn on open.
- Bad signature on `XGrpRelayNew` → rejected with bad-signature event.

## Files touched (anticipated)
- `src/Simplex/Chat/Protocol.hs` — event constructor, tag, JSON encode/parse,
  `isForwardedGroupMsg`.
- `src/Simplex/Chat/Library/Internal.hs` — `connectToRelayAsync` helper,
  `syncSubscriberRelays` moved here.
- `src/Simplex/Chat/Library/Subscriber.hs` — owner send (LINK callback),
  relay forward-only `processEvent` case, subscriber forwarded
  `processForwardedMsg` case.
- `src/Simplex/Chat/Library/Commands.hs` — remove sync `connectToRelay`,
  `APIGetUpdatedGroupLinkData` calls async helper.
- `src/Simplex/Chat/Store/Groups.hs` — adjust relay-member lookup to filter
  on `memberCurrent`.
- `docs/protocol/channels-protocol.md` — signing-required table row,
  relay-addition subsection.
- `tests/ChatTests/...` — tests per "Test surface" above.
