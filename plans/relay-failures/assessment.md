# Partial Permanent Relay Failure — Assessment

## Problem

When a channel is created (owner) or joined (subscriber), relay connections happen asynchronously. If some relays permanently fail while others succeed, failed relays show as "connecting"/"invited" forever — indistinguishable from in-progress connections.

## Plan 1: Owner — RSFailed (prerequisite for Plans 3-4)

Add `RSFailed` to `RelayStatus` enum. Set on permanent ERR event in Subscriber.hs:1121 for relay members when local user is owner. Emit `CEvtGroupLinkRelaysUpdated`. UI shows red indicator.

- **Trigger**: ERR event (post-creation async failure), NOT during initial creation (which is all-or-nothing via `mapConcurrently`)
- **Blast radius**: LOW — RelayStatus has 4 cases, few exhaustive matches
- **DB migration**: None
- **Complexity**: ~30 lines Haskell, ~10 lines Swift
- **Verdict**: Must implement regardless of subscriber plan choice

## Subscriber Plans Comparison

All subscriber plans share the same insertion point: `APIConnectPreparedGroup` (Commands.hs:2020-2028), after partitioning failed/succeeded relays. Update DB for permanently failed relays, re-read members, build `relayResults` with updated members in `CRStartedConnectionToGroup` response. No new events needed — `RelayConnectionResult.relayError` already carries the error.

|  | Plan 2: ConnStatus.connFailed | Plan 3: relay_status column | Plan 4: memFailed |
|---|---|---|---|
| **Mechanism** | New ConnStatus enum case | New nullable column on group_members | New GroupMemberStatus enum case |
| **Haskell blast radius** | ~47 refs across 9 files, ~2-3 exhaustive matches | 5 files, 0 exhaustive match changes | 3+ files, 3 exhaustive matches + encoding |
| **Swift blast radius** | 3 files | 3 files | 3+ files, 4 exhaustive matches |
| **DB migration** | None | ALTER TABLE ADD COLUMN | INSERT into predicates table |
| **Persists on restart** | Yes (if Connection exists) | Yes | Yes |
| **Connection dependency** | **YES** — cannot set if activeConn = Nothing | No | No |
| **Semantic fit** | POOR — ConnStatus is connection lifecycle | MODERATE — separate concern, dual storage | MODERATE — memberStatus is membership lifecycle |
| **UI integration** | New access pattern (`activeConn?.connStatus`) | New access pattern (`relayStatus`) | BEST — UI already switches on `memberStatus` |
| **Risk of regression** | MODERATE (semantic pollution) | LOWEST | MODERATE |

## Assessment

**Plan 2 (ConnStatus) — NOT recommended.** Disproportionate blast radius (~20-30 exhaustive matches across 10 files) for a narrow relay-only use case. Additionally, ConnFailed cannot be set when `activeConn = Nothing` (failure before connection record creation) — a fundamental gap.

**Plan 3 (relay_status column) — cleanest architecturally.** Dedicated field for a dedicated concern, no pollution of existing enums, no Connection dependency. Cost: nullable column (NULL for 99.9% of members), dual storage with owner's `group_relays.relay_status`. Lowest regression risk.

**Plan 4 (memFailed) — smallest UI diff.** ComposeView already switches on `memberStatus`. Cost: overloads GroupMemberStatus with connection-health semantics; `memFailed` naming is misleading (suggests member failed to join, not relay connection failed). Moderate regression risk (compiler-enforced exhaustive matches).

## Recommendation

Plan 1 (owner) + Plan 3 (subscriber) — cleanest separation of concerns, lowest regression risk, no Connection dependency. Plan 4 is a close second if avoiding new DB columns is preferred.

## File Index

- `plan1-owner-rsfailed.md` — Owner: add RSFailed to RelayStatus (trigger: ERR event in Subscriber.hs)
- `plan2-subscriber-connstatus.md` — Subscriber: new ConnStatus case (not recommended)
- `plan3-subscriber-relay-status-column.md` — Subscriber: relay_status column on group_members
- `plan4-subscriber-memfailed.md` — Subscriber: new GroupMemberStatus case
