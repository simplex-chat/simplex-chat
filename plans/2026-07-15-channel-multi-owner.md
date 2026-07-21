# Channel multi-owner

N equal owners per channel, per `docs/protocol/channels-overview.md` §Governance ("v7: Multiple owners, any-owner-decides"). Any owner can take any administrative action, including destroying the channel. No coordination between owners.

## 1. What an owner must do

1. Publish content, and publish as the channel.
2. Sign admin messages subscribers accept — profile, prefs, member removal, role changes, relay announcements, deletion.
3. Change member/moderator/admin roles — i.e. sign the roster.
4. Write link data — relays, profile, member count, the owners list.
5. Add another owner.

Only the creator can do 2, 4, 5 today; 3 works for one owner at a time. Each is enforced somewhere different. 4 reaches into simplexmq.

## 2. Current state

**simplexmq is already built for this, unwired.** `UserContactData.owners :: [OwnerAuth]`, `validateLinkOwners` (each entry signed by root *or an earlier entry*), `decryptLinkData` (accepts data signed by root *or any listed owner*) — all multi-owner-correct. `ShortLinkCreds.linkRootSigKey` exists, commented *"in case the current user is not the original owner"*. `RKEY :: NonEmpty RcvPublicAuthKey` and `recipientKeys :: NonEmpty` shipped in `b7a95422` *"smp server: short links and owners for channels"* at SMP client v4 = current, so every channel-capable server accepts it. **The agent never calls RKEY.**

**Blockers in simplexmq:**
- `AgentStore.hs:2514` — `linkRootSigKey = Nothing` on every load, no column (`-- TODO linkRootSigKey should be stored in a separate field`). The non-root branch of `validateOwners` is dead code.
- `Crypto/ShortLink.hs:80` — `newOwnerAuth` generates the key pair locally; adding a remote owner must sign a supplied public key. The formula is duplicated in chat at `Internal.hs:1519`.

**The single-owner assumption is one function.** `groupLinkData` (`Internal.hs:1509`, `-- TODO [relays] owner: set owners on updating link data (multi-owner)`) rebuilds `owners` on every publish as a singleton of *self*, and as `[]` without `GRKPrivate`. So a second owner is never published; two owners publishing would evict each other; a non-root owner would publish an empty list, making link data unreadable to everyone.

**Owners are not in the roster, by design.** `isRosterRole = GRMember|GRModerator|GRAdmin` (`Internal.hs:1281`: *"owners are on the link, not the roster"*); link data is the sole authority for owner keys (`Groups.hs:3533`). Preserved. `updateGroupFromLinkData` carries only profile + count, so a new owner goes into link data **and** is announced by a signed `x.grp.mem.role`.

**`APIMembersRole … GROwner` is not rejected today** — only the iOS/Kotlin pickers block it. Via CLI it half-works: skips the owner-only guard (`isRosterRole GROwner == False`), sends no key, publishes nothing, and leaves the promoted device throwing `SEGroupLinkNotFound` from `updatePublicGroupData` on every `x.grp.mem.new`. §3 replaces that path.

## 3. Protocol

`APIMembersRole … GROwner` on a channel triggers this instead of a plain role change: two-phase, async, returns "invitation sent".

The invitee must already be a channel member — its key is then known, and its relay connections exist (a subscriber creates `GRRelay`/`GCHostMember` rows, so `getGroupRelayMembers` returns them for a promoted subscriber unchanged). **Its key must also be verified out of band (§8).**

Four events, all `requiresSignature`, over the existing relay connections, delivered in M's support scope (the owner→member private channel, `MSMember`). The events are **role-generic**: they promote to a named role, and role-specific data rides in an optional block. Only `GROwner` is implemented, but the packet is shaped to extend to other roles without a wire break.

```
x.grp.promote.inv     { invitationId, memberRole, roleData? }   -- O1 -> M
x.grp.promote.acpt    { invitationId, memberKey, roleData? }    -- M  -> O1
x.grp.promote.reject  { invitationId }                          -- M  -> O1
x.grp.promote.cancel  { invitationId }                          -- O1 -> M
```

- `memberRole` — the role offered; the generic discriminator. Shown in M's prompt.
- `roleData` — an optional role-scoped block, present iff the role needs one; validated as required-for-`GROwner`, absent otherwise. For owner: `{ linkRcvId }` in the invitation, `{ linkRcvKey }` in the acceptance. Concretely a sum tagged by role — `PromotionRoleData = PRDOwner OwnerPromoteData | …`. Forward-compat for a *future* role is not free: `omittedField` only supplies a default when the key is **absent**, so a present block with an unknown role tag would fail to parse on an old client, not be ignored. Since only owner exists now this is latent, but the parser should decode an unrecognised `roleData` tag to an opaque/ignored variant (not error) so a future role degrades gracefully — the same discipline as `XUnknown`/`CIInvalidJSON` elsewhere.
- `invitationId` — binds accept/reject/cancel to one invitation; blocks relay replay.
- `memberKey` — M's existing member key, restated on acceptance so O1 binds the verified key. Generic across roles: O1 already holds it and **must** check they match and abort otherwise, so a promotion can never introduce a key.
- `linkRcvId` / `linkRcvKey` — owner-only, inside `roleData`: the link queue's recipient ID, and a fresh recipient auth **public** key M generates.

Everything else M needs to write link data is public or derivable: `shortLinkKey`/`shortLinkId` from the channel link; fixed-data plaintext (hence `rootPubKey`) via `LGET`, re-encrypted under a fresh nonce — readers check `sha3_256(fd) == linkKey` and the root signature over the *plaintext*.

`linkRcvId` authorises nothing on its own (`Server.hs:1249` verifies every recipient command against `recipientKeys`), which is equally why the relay carrying these learns nothing.

**Acceptance is not automatic, and either side can withdraw.** On `x.grp.promote.inv`, M stores a pending promotion and surfaces it (§7); it does not act. Only when the user confirms does M generate `linkRcvKey`, send `x.grp.promote.acpt`, and move the record to accepted-pending-completion. Decline sends `x.grp.promote.reject` and clears it; O1 marks the invitation rejected and notifies its UI, mirroring the relay-rejection path (`x.grp.relay.reject`). O1 can withdraw a still-pending invitation with `x.grp.promote.cancel`, which clears M's pending record and its banner. An ignored invitation stays pending until it expires on the `relayRequestExpiry` pattern, or is superseded by a fresh invitation. Cancel racing acceptance is benign: if it arrives before the role event, O1 simply does not complete the worker; if after, the owner is already committed and cancel is a no-op (removal is §12).

This is consent, and — combined with §8 — a second lock. §8 gives O1 M's out-of-band-verified key; the acceptance is signed by M's real key, which O1 checks equals the verified one, so a relay that never delivered the invitation cannot forge an acceptance. It does **not** by itself stop the §8 relay-substitution attack (a malicious relay short-circuits M entirely) — that remains the verified key's job.

Promotion is single-target: `APIMembersRole` takes a member list, but `newRole == GROwner` on a channel accepts exactly one member (each promotion is a separate verified, async, consent-gated flow), mirroring the existing `"can't change role of multiple members … or new role is admin"` guard (`Commands.hs:2879`).

**Open — the delivery transport is wrong as written.** "M's support scope" (`MSMember`) is not a private owner↔M channel: it fans out to M **plus every moderator, admin and owner** (`Subscriber.hs:4276-4290`, `Groups.hs:1273`), a sender cannot target one owner, and a plain observer has no support scope until one is initiated. So routing `x.grp.promote.*` through it leaks the governance action to non-owner moderators. This is the same root gap as owner-removal's key-attribution (§12): the codebase has no **owner-only** delivery/visibility — only `≥ moderator` support scope and public broadcast. Options: (a) accept moderator visibility of promotions (they see the fact, not any secret — `linkRcvId`/`linkRcvKey` are inert/public); (b) build an owner-scoped delivery (fan out to `getGroupOwners` + the target M, excluding mods) — reused by §12; (c) a direct O1↔M member connection (private E2E, but requires M be directly reachable, narrowing who can be promoted). Recommend (b): one primitive unblocks both this and removal.

O1 on receiving `x.grp.promote.acpt`, via a durable worker (mirror `runRelayRequestWorker` — multi-step, cross-network, must survive a crash):

1. Add `linkRcvKey` to the link queue's recipient keys (see below — `RKEY` as shipped cannot do this).
2. `LSET` — publish `owners = [oa1, …, oaN, mkOwnerAuth memberId2 memberKey signingKey]`.
3. `x.grp.mem.role memberId2 GROwner (Just memberKey)` → relays → subscribers.

**Step 1 needs the key set readable, and `RKEY` given compare-and-swap.** `RKEY :: NonEmpty RcvPublicAuthKey` *replaces* the whole list (`updateKeys st sq rKeys = … q {recipientKeys = rKeys}`), so a writer must know every key it wants to keep — and nothing reads it back: `QueueInfo` carries `qiSnd`/`qiNtf`/`qiSub`/`qiSize`/`qiMsg` and no keys. The creator knows the set, having issued every RKEY. **A promoted owner does not**, so owner 2 adding owner 3 would RKEY a list built from its own knowledge and silently evict owner 1's link-write capability.

Add the keys to `QueueInfo`: `QUE` is already a recipient-authorised command returning queue state, so this is a field, not a mechanism, and only a recipient sees it. `RKEY` then takes an expected fingerprint of the set, as `LSET` does (§4.2), or two owners adding concurrently each replace it with a list missing the other's addition; the rejection returns the current set, so a retry needs no second read. `recipientKeys :: NonEmpty` (`QueueStore.hs:37`) prevents emptying it. One concurrency pattern for both mutable server-side objects.

The set is **anonymous** — public keys with nothing tying a key to an owner. That is sufficient here: adding means appending and keeping the rest, which needs no idea whose the others are. Removing a *named* owner does need that mapping, and it is part of removal's design (§12), not this one.

**The role event is the commit point.** M must not treat itself as an owner on sending the acceptance, only on receiving `x.grp.mem.role` for itself — which the existing `xGrpMemRole` self-branch (`Subscriber.hs:3338`) already applies. O1 sends it only after 1 and 2 succeed, so the event means *the link says you are an owner*. Set the role earlier and any failure or crash between acceptance and LSET leaves M believing it is an owner while absent from the chain: it would write link data and be refused, and sign admin messages that every subscriber rejects, since its key is not in the published owners.

Each step must therefore be idempotent and resumable — on restart the worker re-reads the published chain and skips what has landed. If O1 never completes, M stays pending, holding an unused `linkRcvId` and a `linkRcvKey` that was never RKEY'd, and the invitation expires on the same pattern as `relayRequestExpiry`.

LSET precedes the broadcast so a joiner in the window learns the owner from the link, not by TOFU. Once published, M's entry cannot be evicted by another owner's write: `owners` merges as published-plus-mine (§4.2).

## 4. Concurrency

No consensus between owners: "any-owner-decides" is defined by its absence, owners are sleeping phones reachable only through relays (a relay could partition them and freeze governance), adding owners *is* reconfiguration — the hardest part of consensus — and quorum would mean losing an owner can stop the channel, inverting the point of the feature.

Convergence without coordination instead. The two shared states differ in storage, so they take different standard answers: link data is one copy on a server → optimistic concurrency; the roster has a copy per relay and no central store → last-writer-wins with a deterministic timestamp. The subscriber count stops being shared state once relay-derived.

### 4.1 Roster: same version, two owners, multiple relays

```
O1 -> R1,R2 : roster N (content A)
O2 -> R1,R2 : roster N (content B)
R1 sees A then B: fresh = N >= N -> applies B    -> R1 = B
R2 sees B then A: fresh = N >= N -> applies A    -> R2 = A
```

Permanent divergence: `fresh = maybe True (v >=) gate` (`Subscriber.hs:3302`; `notBelowRoster` `:3430` for the blob) is not a total order, so at equal versions the winner is whoever arrived last.

**Fix: tie-break on the author's `memberId`** — accept iff `(v, authorMemberId) >= (gate_v, gate_authorMemberId)`. `memberId` is 12 random bytes, identical on every device, so every node picks the same winner and both relays converge on B regardless of order. Keep `>=` not `>`: an equal tuple from the same owner must still accept (`Subscriber.hs:3288-3291`). `roster_sending_owner_gm_id` exists on `groups` but is a **local row id**, not comparable across devices → add `roster_version_owner_id BLOB`.

The tie-break must be applied at **three** gates, not two — the owner↔owner convergence path is the roster *blob*, which is applied later at `rosterCompletion` (`Subscriber.hs:3461`), whose gate is version-only (`maybe False (pendingVer < cur)`) and which persists the local `ownerGMId`, not a cross-device author. Two same-version blobs from different owners arrive as independent per-source transfers that both pass the header gate (`:3430`); whichever *completes* last wins at `:3461` regardless of author, so nodes still diverge. So: thread the author `memberId` into `setGroupLiveRoster` and store it in `roster_version_owner_id`, and apply the tuple compare at all of `applyAtRosterVersion` (`:3302`), the header `notBelowRoster` (`:3430`), and the completion gate (`:3461`).

**Rosters must reach owners.** `sendRosterBlob` targets `getGroupRelayMembers` only and the relay→subscriber broadcast was deliberately removed, so an owner never sees another's roster and its counter goes stale. Relays forward `XGrpRoster` + `BFileChunk`s to `GROwner` members excluding the author — bounded fan-out, owners only.

The tie-break makes every node *converge* — this part is sound and is new work (the gate today is `>=`, arrival-order LWW, `Subscriber.hs:3304`; no tie-break exists). Two things the earlier framing got wrong, both confirmed against the code:

**Loss is silent — there is no detection path.** Applying an adopted roster (`processRosterEntries`, `Subscriber.hs:3497`) is a full overwrite: every named member's role is set, and any privileged member absent from the blob is reverted to default. It overwrites the receiving owner's own just-made change, and **no pending/optimistic state records what that owner changed**, so nothing can notice the revert — the owner's UI shows only an ordinary `CEvtMemberRole` attributed to the other owner. "UI surfaces the revert" has no mechanism.

**Whole-value LWW over-loses.** Role changes are per-member and mostly compose: O1 promoting X and O2 promoting Y do not conflict, yet LWW drops one. Treating the roster as a whole value (like the profile) is wrong for it.

Resolution (decision): the roster wants a **lightweight per-member reconciliation**, not whole-value LWW. Persist the owner's own outstanding role deltas; on adopting a newer roster, re-apply the deltas whose member the winner did not also touch (converges to the union), and surface only a genuine same-member conflict for the user to resolve. This is a small step toward the overview's deferred "linearly ordered signed roster log", stopping well short of it. The alternative — ship whole-value LWW with silent loss — is simpler but drops compatible concurrent changes and cannot honour the "surface the revert" promise.

Frontier logic is unaffected: the counter stays one shared sequence, so no gaps in `nextCompleteVersion`.

### 4.2 Link data: concurrent LSET

One queue on one server, so writes serialise and stored state never diverges. Two defects make a lost write possible and invisible:

- **Every write is a blind whole-object overwrite.** `groupLinkData` rebuilds the entire `UserContactData` from local state — each publish asserts *the whole link is what I think it is*, including fields the writer knows nothing about. Right for one writer, wrong for N.
- **An owner's confirmation is its own echo.** `notify $ LINK link userLinkData` (`Agent.hs:1813`) returns the data the *client sent*, not the server's state, and relay activation (`Subscriber.hs:1411-1458`) reads `relays` out of that echo to promote relays to `RSActive`. So an owner marks a relay active because it asked for it. With N writers: the owner adds a relay, another owner's publish drops it, the relay's own `checkRelayServedGroups` sees its link missing and goes `RSInactive`, and the adding owner still shows it active. Nothing reads back the truth.

Every loss here is damaging — a dropped relay silently stops serving, a dropped owner entry leaves an accepted invitee unpublished, a dropped profile update vanishes.

**Count comes from relays.** `publicMemberCount` is published per join/leave from six sites (`Subscriber.hs:3137/3152/3170/3658/3705`, `Commands.hs:3035`). The number is `summary_current_members_count` — a DB-trigger count of rows in *that device's* `group_members` (`M20250919_group_summary.hs:48-81`), i.e. "members I know about". A promoted owner's is permanently low: `introduceInChannel` announces joiners only to `owners <> adminsMods`, so observers never learn of other observers. Sole-publisher-creator only defers it (wrong in perpetuity once they leave); introducing every subscriber to a new owner is O(N) messages and rows for one integer.

The count is **already relay-derived** — an owner has no connection to subscribers, so its tally is just relay-sent `XGrpMemNew`s counted. So the relay states its count in the join/leave announcements it already sends: an optional field on `XGrpMemNew` and its departure counterparts, no new event, no new fan-out. Owners mirror it (max across relays) instead of deriving it. Same trust, no bookkeeping.

**One owner publishes it: the first in the chain, the next taking over if it stops.** All owners still track it, so this is who writes, not who knows — handover needs no catch-up. `owner_auth_index` is identical on every device, so the choice is deterministic and needs no election. Otherwise N owners write per join, N-1 rejected, and the link becomes a target N of them constantly move — which would starve human writes.

Publishing stays per join. `updatePublicGroupData` keeps its trigger — it does double duty and must not be dropped: there is no relay-removal command, a relay is removed by removing the relay *member* (`Commands.hs:3033-3035`) and leaves the link because `getPublishableGroupRelays` requires `member_status = GSMemConnected` (`Store/Groups.hs:1470`); a relay leaving arrives as `xGrpLeave` (`Subscriber.hs:3705`), same path.

This also disposes of the wider worry: the count was the only thing needing a *complete* member list, so a promoted owner's partial one stops mattering. Moderation learns members lazily on first post (`Subscriber.hs:4195`); the roster arrives as a served snapshot on join.

Residual, accepted for now: the single publisher still writes per join, and on a very busy channel a concurrent human write (profile/relay) can be CAS-rejected and retried repeatedly against the stream of count writes — bounded by the join rate, not a tight loop. Per-join was chosen over coalescing deliberately (a moving count signals life); rate-limiting the count publish is the lever if this proves painful in practice.

A relay can state a false number, but it can already do that by fabricating or withholding announcements — the threat model records count manipulation as detectable-not-prevented, unchanged.

**Writes become deltas.** `LGET` → apply only this write's delta → `LSET`:

| field | semantics | rule |
|---|---|---|
| `direct` | constant | preserve |
| `owners` | append-only ordered chain (§4.3) | published, my entry appended |
| `relays` | set with meaningful removals | published, ± the one relay this write touches — never my whole local list |
| `groupProfile` | whole object | overwrite only if this write *is* a profile change |
| `publicMemberCount` | relay-reported | overwrite only if this write carries a newer report |

This kills two more failures: a publish can no longer evict another owner, and a profile edit can no longer resurrect a removed relay.

**The merge-and-retry lives in one place.** Callers stop building link data and pass a delta; one function takes the published data, applies the delta, `LSET`s, and on rejection re-merges and retries, bounded. The structure already fits: every group link write funnels through `groupLinkData` via exactly two callers, `setGroupLinkData` (sync) and `setGroupLinkDataAsync` (async). Channel creation is exempt — first write, nothing to conflict with.

The async path loops chat-side, not in the agent: `setConnShortLinkAsync` enqueues fully-built data and executes it later (`Agent.hs:1811`), so a rejection can arrive long after the caller returned, and the agent cannot re-merge — merge policy is chat's. So async callers enqueue a delta to a per-group link worker, which also serialises this owner's own writes.

**Compare-and-swap on `LSET`.** "Write this, but only if what is stored is still what I read." The writer passes a fingerprint — a hash of the stored user-data bytes its merge was based on — and the server stores only if the current bytes still hash to that; else it rejects, stores nothing, **and returns the current data**, so the retry needs no read.

```
without CAS                              with CAS
O1: warm at {News, [R1,R2]}              O1: warm at {News, [R1,R2]}  digest a
O2: warm at {News, [R1,R2]}              O2: warm at {News, [R1,R2]}  digest a
O1: LSET {News,  [R1,R2,R3]}   stored    O1: LSET expect a {News, [R1,R2,R3]}  -> OK, now b
O2: LSET {Daily, [R1,R2]}      stored    O2: LSET expect a {Daily, [R1,R2]}    -> REJECTED
                                              + returns {News, [R1,R2,R3]}, digest b
= R3 silently gone, stops serving.       O2: LSET expect b {Daily, [R1,R2,R3]} -> OK
  O1 still shows it active.              = both changes survive.
```

| | uncontended | contended |
|---|---|---|
| today (single writer) | 1 — `LSET` | n/a |
| merge, write blind | 1 — `LSET` | 1, silently lost |
| merge, write, verify | 3 — `LGET`, `LSET`, `LGET` | 3, hole remains |
| **compare-and-swap** | **1 — `LSET`** | **2 — rejected `LSET`, `LSET`** |

So it is not a correctness tax on the cheap design; it *is* the cheap design, and it removes machinery: no verify read, no durable per-write intent, no retry-until-observed loop, no residual hole.

Details:
- The fingerprint is of the **stored ciphertext** — the server has no key, so it can only hash bytes it holds (`queueData qr`). It doubles as a version token for free: a fresh nonce per write changes the bytes even for identical plaintext.
- A writer knows it only from bytes it has seen: its own successful write (hashed on the way out), an `LGET`, or a rejection. Another owner's write is otherwise opaque — the fresh nonce means it cannot be re-derived from local plaintext. Persisted next to the link credentials.
- **A write is rejected exactly when another owner wrote since you last read or wrote.** Alternating owners pay one extra round trip per alternation. That is optimistic concurrency working as intended.
- It never reaches chat: the agent encrypts, so it computes and keeps the fingerprint with the link credentials, refreshing on every successful write, `LGET` and rejection. Chat gets success, or a conflict carrying the current data decrypted.
- Returning data on rejection leaks nothing: `LGET` needs no signature for a contact queue (`Server.hs:1256`), so link data is already public to any link holder. `LNK` (`Protocol.hs:698`) already carries a whole `QueueLinkData`, so the size is routine. Needs a distinct `BrokerMsg`, not an `ErrorType` — not shaped for a 13.8 KB payload.

**A rejection is not a conflict and does not reach the user.** It says *something* changed, usually not what this writer is changing. Re-read, and if my field is untouched, re-merge and retry — always safe. The user sees a save one round trip later. Only if my field *also* changed is it a real conflict: set-elements still re-merge; whole values yield and report the winning value. That report is the only thing a user sees, it means another owner really did edit the same field seconds earlier, and writing blind gives no error at all in that case — the edit just disappears. Retries are bounded; only exhausting them is a failure.

**On rejection, per write:**

| write | trigger | link write | on rejection |
|---|---|---|---|
| profile / prefs | `runUpdateGroupProfile`, user-facing | **sync** | re-merge, retry; on real conflict abort before the local commit and show the winning value |
| relay add | relay's acceptance → worker | async | worker's existing retry |
| relay remove | member removal → `updatePublicGroupData` | async | worker's existing retry |
| owner add | acceptance → worker (§3) | async | worker's existing retry |
| subscriber count | join / leave | async | drop; the next join publishes again |

**Nothing new is recorded.** A rejection is immediate, so no writer must remember what it wanted in order to notice later that it vanished — that bookkeeping is a cost of writing blind. Sync writes fail to the user, who retries. Async writes already have durable retry state and backoff (`relay_request_retries`/`_delay`/`_execute_at` on `groups`, via `retryTmpError`, `Subscriber.hs:4404`) because they must survive a crash regardless; a rejection is a textbook temporary error, so classifying it as one puts it in the existing loop, and the existing expiry surfaces a relay error if attempts keep failing. Progress is already a state machine — a relay sits at `RSAccepted`/`RSAcknowledgedRoster` until the write lands — so a rejection leaves it pending, nothing to revert. Only fix: activation must key off an accepted write, not the `LINK` echo.

**Profile ordering.** `runUpdateGroupProfile` commits locally (`Commands.hs:4016`) *before* the sync link write (`:4034`), so any failure — rejection, or a plain network error today — leaves the profile changed here and nowhere else. `groupLinkData` is pure, so building link data needs no prior commit: write first, then commit and broadcast. Fixes the existing silent-divergence bug too.

**Learning other owners' changes.** No periodic worker. Three triggers suffice: a rejection returns the current data; a newly promoted owner does one `LGET` to seed its count; and channel open reconciles via the existing `APIGetUpdatedGroupLinkData` (`Commands.hs:1909`) — which already exists and is *owner-excluded* in both layers, backend (`:1918`) and UI (`ChatView.swift:771`, an `else if` that routes owners elsewhere). Removing that exclusion is the whole change. This matters beyond the UI: an owner must connect to relays another owner added, or its posts never reach their subscribers, and `syncSubscriberRelays` is exactly the function that connects to relays listed in link data.

Two owners genuinely disagreeing — one adding a relay, the other removing the same one — is not a race; last writer wins.

### 4.3 Owner chain ordering

`validateLinkOwners` requires each entry to be signed by root or an **earlier** entry. Since owner 2 can add owner 3 with its own key, order is load-bearing and must be persisted, not derived. Two owners concurrently appending is an LSET conflict — one re-merges and retries, appending after the other.

## 5. Backend work (simplex-chat)

**Persist the owners chain.** `owner_auth_sig BLOB NULL` + `owner_auth_index INTEGER NULL` on `group_members` (migration + cabal + SQLite and Postgres). The key is already there as `member_pub_key`, so it cannot drift from the member record, and the index carries §4.3's order.

- `Internal.hs:1509` `groupLinkData` — becomes "apply this delta to the published data": read the stored chain, delete the `GRKPrivate`-derived singleton and the `_ -> []` fallback. Its two callers become the single place owning merge, CAS and bounded retry (§4.2).
- Per-group link worker for async writers (relay connected, roster acked, count) — they enqueue deltas, since the agent cannot re-merge on rejection.
- `Internal.hs:1458` `updatePublicGroupData` — trigger unchanged (still per join/leave, and the only publish for relay removal/departure); number now from relay reports; published only by the designated owner (§4.2).
- `XGrpMemNew` + departure counterparts gain an optional relay-stated count, filled from the number the relay's own `updatePublicGroupData` branch already computes. Owners store it per relay, use the max.
- `Internal.hs:1458`, `Commands.hs:4011` `runUpdateGroupProfile`, `Commands.hs:2713` `APIAddGroupRelays` — gate on `memberRole' membership == GROwner` but need "can write link data" (an owner mid-handover cannot). One predicate, all three.
- `Commands.hs:3268` `APIAddGroupShortLink` — asserts no role at all; same gate.
- `Commands.hs:2872` `APIMembersRole` — route `newRole == GROwner` into §3 (send `x.grp.promote.inv`, do not change the role); require `memberVerifiedCode` on the invitee (§8). The invitation carries the verified key; the acceptance's `memberKey` must equal it or abort, or verification is worthless.
- Pending-promotion record on each side, on the member row (columns, mirroring `relay_request_*`): offered role, `invitationId`, worker status (invited / accepted-pending / rejected), and `linkRcvId`/`linkRcvKey`. On O1 the invitee's row; on M its own membership row.
- A role-generic proposal type surfaced on `GroupMember`, `Maybe MemberRoleProposal` with `MRProposed GroupMemberRole | MRRejected GroupMemberRole` (the "proposed role" the promoter UI shows, §7). Derived from the record: invited/accepted-pending → `MRProposed`, rejected → `MRRejected`; cleared on commit or cancel. Plus a `promotionPending`-style predicate mirroring `memberPending`/`gmRequiresAttention` to drive M's banner and unread badge.
- New service chat item on M — a `RcvGroupEvent` constructor (e.g. `RGEMemberPromotionInvited {role}`) created in M's support scope, so the invitation shows as unread exactly as `RGENewMemberPendingReview` does for knocking. Cleared/replaced on accept, reject, cancel, or commit.
- New commands: `APIAcceptRolePromotion groupId` (M generates owner `roleData`, sends `x.grp.promote.acpt`, marks accepted-pending), `APIRejectRolePromotion groupId` (sends `x.grp.promote.reject`, clears), `APICancelRolePromotion groupId groupMemberId` (O1 sends `x.grp.promote.cancel`, clears the proposal). Role-generic names, owner-only behaviour today. Handlers in `Commands.hs`; wrappers + strings in both apps.
- New events `x.grp.promote.inv`/`.acpt`/`.reject`/`.cancel` (`Protocol.hs`, all `requiresSignature`; role-generic packet per §3). `.reject`/`.cancel` mirror `XGrpRelayReject`.
- Inbound handlers in `Subscriber.hs`: `.inv` on M creates the record + service item + UI event; `.acpt` on O1 starts the durable worker; `.reject`/`.cancel` clear the record and proposal on the respective side. **`.inv` must verify the sender is an owner** (owner-signed, key resolved from link data) and reject otherwise — a moderator is inside M's support scope and `memberCanSend` permits any `≥ GRModerator` to send into it (`Subscriber.hs:1707`), so without this check a moderator could inject a promotion invitation. Likewise `.acpt`/`.reject`/`.cancel` match an existing pending record by `invitationId` on the acting side and are ignored otherwise (so the support-scope fan-out to other owners/mods is inert for them).
- **Relay + recipient forward dispatch for the promotion events** — they travel owner→relay→M (and back), so the relay's `processEvent` (`Subscriber.hs:1070`, else `:1121` drops as "unsupported message") must return a `DJSMemberSupport` delivery task for them, and the recipient's `processForwardedMsg` (`:3887`, else `:3910`) must handle them; keep the `isForwardedGroupMsg` set (`Protocol.hs:552`) in sync. Without these three sites the relay silently drops every promotion event and the flow cannot complete.
- `Commands.hs:4016` — reorder: write link, then commit and broadcast (§4.2).
- `Subscriber.hs:3345` `allowCreate` — widen so an owner-signed `x.grp.mem.role` with a key can TOFU-create a `GROwner`; `isRosterRole GROwner == False` blocks it today, so subscribers can never materialise a new owner.
- `Commands.hs:2936` `mKey` — send the key on owner promotion, not only when a roster version is present.
- `Subscriber.hs:4195` — "owners are already known to every member" is false for a newly promoted owner; disseminate its profile.
- `Subscriber.hs:1422` — `publicMemberCount > 1` assumes one owner (self-documented TODO). Use a subscriber count or `> ownerCount`.
- `Commands.hs:1918` `syncSubscriberRelays` — run for owners other than the publisher (§4.2).
- `Subscriber.hs:1411-1458` — relay activation must key off an accepted write, not the `LINK` echo, which is self-confirming.
- `Commands.hs:1192` — share-link signing requires `GRKPrivate`, so a promoted owner silently emits `ownerSig = Nothing`. Sign with any listed owner's member key; `verifyLinkOwner` already resolves by `ownerId`.
- `Store/Groups.hs:2320` `updateRelayGroupKeys` — `forM_ owners` exists but `getGroupMemberIdViaMemberId` throws for an owner the relay has no row for, aborting the transaction. Create it — reuse `createLinkOwnerMember`, which the subscriber path already uses for this.
- `Commands.hs:5254` `checkRelayServedGroups` — the relay's periodic worker re-reads link data but binds only `relays`, discarding `owners`. Refresh owners; it is the relay's only path to owner changes.
- `Store/Groups.hs:1117` `getHostMember` — `firstRow` on `GCHostMember`; relays model exactly one owner.
- Roster: tie-break gate (`Subscriber.hs:3302`, `:3430`), `roster_version_owner_id` column, relays forward `XGrpRoster` + chunks to owners (§4.1).
- Durable owner-request worker + table, mirroring the relay-request worker.

## 6. simplexmq work

- `Crypto/ShortLink.hs` — extract `mkOwnerAuth :: OwnerId -> PublicKeyEd25519 -> PrivateKeyEd25519 -> OwnerAuth`; redefine `newOwnerAuth` on it; export. Kills the cross-repo duplicate.
- Persist `linkRootSigKey` — `rcv_queues` column + migration in **both** the SQLite and Postgres agent-store trees (`Agent/Store/{SQLite,Postgres}/Migrations`, selected by `-fclient_postgres`), read by the shared `AgentStore.hs:2514`.
- Recipient keys readable + `RKEY` compare-and-swap (§3): add the keys to `QueueInfo` (`QUE` is already recipient-authorised), and give `RKEY` an expected fingerprint with the rejection returning the current set — the same shape as `LSET`, so one pattern covers both. Without the read a promoted owner cannot RKEY without evicting the others; without the fingerprint two concurrent additions clobber. Plus the agent API.
- Link-write API for an owner that does not own the queue. Prefer a standalone `setForeignLinkData` taking explicit creds over faking an `RcvQueue`/`ContactConnection`: the latter needs an `rcvDhSecret` owner 2 must not have, and risks it subscribing to owner 1's queue and racing on inbound requests.
- CAS on `LSET` (§4.2): new command tag carrying the expected fingerprint; new `BrokerMsg` for the rejection carrying current user data. **Version on the right axis:** these are `Command Recipient`/`BrokerMsg` values, encoded via `ProtocolEncoding SMPVersion` branching on `VersionSMP` (like `shortLinksSMPVersion = 15`) — so gate them on a bump of `currentServerSMPRelayVersion` (`VersionSMP` 18 → 19, `Transport.hs:234`), **not** `currentSMPClientVersion` (that rides only in the client↔client envelope and gates nothing server-facing). Same for RKEY-CAS and the QueueInfo keys field. Server check beside `lnkId' /= lnkId -> err AUTH` (`Server.hs:1484`) — bytes to hash and return are already in `queueData qr`, so no server storage change (the *agent* still needs a fingerprint column, below). Agent keeps the fingerprint with link creds.
- **Old-server behaviour differs by operation and must be explicit.** LSET below v19: fall back to a blind write — lossy but not corrupting, since owners still merge append-only after an `LGET`. RKEY below v19: **must fail closed, not fall back** — `updateKeys` replaces the whole list and QUE can't read it back, so a blind RKEY *evicts* the other owners' link-write keys (the exact harm §3 prevents). So **gate owner-add (and multi-owner generally) on the link queue's negotiated server version ≥ 19**; on an older host, block promotion with a clear error rather than degrade. This also resolves the §2/§4.2 tension: a v4-era server cannot host a safe multi-owner channel.
- Fingerprint persistence: a new `rcv_queues` **agent-store** column (both SQLite and Postgres trees, `-fclient_postgres`) holds the CAS fingerprint, so it survives restart; without it an owner must `LGET` before its first post-restart write.
- `LINK` notifies with the client's own `userLinkData` (`Agent.hs:1813`) — return the server's state or drop the payload, so callers cannot mistake it for confirmation.

## 7. UI

Already ahead in places: owners are pluralised (`ownersContributorsCountStr`, `ChatInfoToolbar.swift:134` / `ChatView.kt:1550`), the members button checks `members.contains { $0.wrapped.memberRole >= .owner }` (`GroupChatInfoView.swift:128` / `.kt:650`), leave has a `hasOtherOwner` escape hatch (`.swift:250` / `.kt:822`). The gap: `isOwner` is self-relative and count-blind (`membership.memberRole == .owner && membership.memberCurrent`), used ~30× as if it meant *the* owner.

**No model of link-write capability** — `grep groupKeys|GRKPrivate|rootKey` across `apps/` returns zero hits. Two proxies, both wrong once owners are equal:
- `isOwner && groupLink != nil` (`GroupChatInfoView.swift:111`, the `// TODO [relays] allow other owners to manage channel link` site; `.kt:638`). The `groupLink != nil` half is the accidental gate: only the creator has a `user_contact_links` row.
- `isOwner` alone for relay add/remove and status fetch (`ChannelRelaysView.swift:26/48/83`, `.kt:39/90/109`).

Replace both with a backend-supplied `canManageLink` on `GroupInfo` — not equivalent to `isOwner`: an owner mid-handover cannot write. Gate link, relay and domain-claim UI on it.

**Promoter side (O1), in `GroupMemberInfoView`.** Keep the role picker (`canChangeRoleTo`, `ChatTypes.swift:3060` / `ChatModel.kt:2652`) — add `.owner` to it for channels. Selecting owner does **not** change the role inline; it sends the invitation and the member enters the `MemberRoleProposal` state. A new **conditional row below the picker** reflects it: `MRProposed .owner` → "Owner — invitation sent" with a **Cancel** action (`APICancelRolePromotion`); `MRRejected .owner` → "Owner promotion declined", with the option to re-invite. The row is absent when there is no proposal. Warn prominently when the invitee's key is unverified and route to the existing verification screen before the invitation can be sent — unlike contact verification the consequence is not one conversation but the channel's trust chain (§8).

**Invitee side (M).** The invitation is surfaced two ways, mirroring knocking:
- A **service chat item** in M's support scope (`RGEMemberPromotionInvited`, §5), so it shows as unread and pulls M into the support chat — the same mechanism as `RGENewMemberPendingReview`.
- An **accept/reject banner** in that support chat: an above-compose bar mirroring `ContextPendingMemberActionsView` (`ComposeView.swift:432`) / `ComposeContextPendingMemberActionsView.kt` (`ComposeView.kt:1594`), gated on `scope == memberSupport(self) && promotionPending`. Decline is one tap plus confirm. Accept opens a confirmation stating who invited, the role, and that any owner can delete the channel (§8) — heavier than member admission's one-tap, because it is an identity-level grant.

- Owners section in channel info; pending-owner and pending-invitation state.
- A role change can revert when it loses a tie-break (§4.1) — surface it.
- `ChatView.swift:771` / `Commands.hs:1918` — owners are excluded from the on-open link reconcile; remove the exclusion (§4.2).
- **iOS-only:** `GroupChatInfoView.swift:303` skips `apiGetGroupLink` unless `isOwner`; Kotlin fetches unconditionally (`ChatView.kt:403/428`). Reconcile.
- RULE-19 (`apps/ios/product/rules.md:112`) says a *sole* owner cannot leave. The info view honours it via `hasOtherOwner`; the chat-list menus drop the clause — `showLeaveGroup = … && !(groupInfo.useRelays && groupInfo.isOwner)` (`ChatListNavLink.swift:272`, `:247`; `ChatListNavLinkView.kt:319`, `:341`) — so any channel owner loses Leave in the list even when others exist.

## 8. Security

**The invitee's key must be verified out of band before promoting.** By default an owner's copy of a subscriber's key is relay-asserted: the owner has no connection to subscribers, so everything it knows arrived as `XGrpMemNew`, which is unsigned and relay-authored (`Subscriber.hs:3125`). The code defends only the reverse direction — a receiver with a roster-established key refuses a relay's differing assertion (`:3131-3134`) — and a plain subscriber is not on the roster, so the owner has nothing to check against. M's own signed key proof (`encodeXMemberConnInfo`, `Internal.hs:2234`) is verified by the relay (`memberJoinRequestViaRelay`, `Subscriber.hs:1684`) and never forwarded. So a relay that substituted a member's key at join could be promoted to owner and then sign rosters and admin messages itself. No in-protocol signature closes this: the substituted key *is* the relay's.

Out-of-band verification closes it and already exists. `APIVerifyGroupMember` (`Commands.hs:2021`) routes channels to `verifyChannelMemberCode`, whose code is `channelMemberCode ownKey memKey` (`Types.hs:1911`) — a hash over *both* member public keys, sorted so each side computes the same value; `setGroupMemberVerified` persists `memberVerifiedCode`. Promotion is gated on it. That is what makes routing `x.grp.promote.*` over relays sound — not the messages, but the key they are built on.

**Any owner can destroy the channel.** `Server.hs:1249` — any recipient key authorises any recipient command, including `DEL` and `LDEL`, with no per-command scoping. Consistent with any-owner-decides; recorded because it is stronger than any chat-level action: the link address is in the published profile, so its loss cannot be repaired by re-publishing.

**RKEY as shipped is a replace with no compare-and-swap**, so two owners writing the list concurrently evict each other; last writer wins. §3 gives it a fingerprint, so a loser is rejected and retries against the current list instead. Removal is then replace-minus-one under the same rule; `recipientKeys :: NonEmpty` (`QueueStore.hs:37`) prevents emptying the list, so two owners removing each other cannot leave the link unwritable — one survives, arbitrarily.

**Chain order is load-bearing, and entries are never deleted.** A reordered or partially-published chain makes link data unreadable to every client and bricks joins. Publish in `owner_auth_index` order and validate locally before `LSET` — the agent rejects a bad chain anyway, but as `CMD PROHIBITED`, not a legible error. Dropping an entry breaks every owner it transitively signed, so removal revokes the key and keeps the entry (§12).

**A co-owner can hostile-take-over link-write.** RKEY replaces the whole key set, and `recipientKeys :: NonEmpty` only forbids emptying it, so a malicious owner can RKEY the set down to one key it alone controls, evicting every other owner from writing link data. CAS makes this deliberate rather than accidental but does not prevent it. Accepted under any-owner-decides — it is the same trust level as "any owner can destroy the channel" — but stated so it is a conscious choice.

**Auto-connecting to co-owner-added relays is unreviewed.** Removing the owner-exclusion on `syncSubscriberRelays` (§4.2) means every owner (and joiner) automatically connects to any relay any owner published, so a malicious co-owner can insert an attacker-controlled relay the others adopt without review. Also within the any-owner-decides trust model, but a relay sees content, so worth an owner-facing audit surface later.

**Verification enforcement rests on an unstated invariant.** §8's mitigation only holds if the acceptance's `memberKey` is checked equal to the *exact key that was out-of-band verified* — `memberVerifiedCode` is a hash of both keys, so the enforcement must compare the key O1 signs into the chain against the one whose code was verified, and re-verification must be forced if M's key changed since. State this as a hard precondition on the promotion command.

**Creator anonymity is weaker than the overview states.** It claims owners are indistinguishable "provided multiple owners were signed by the root key". Once owner 2 signs owner 3 with its own key the chain shows who delegated to whom, and root-signed entries identify the creator's cohort. Qualify it.

## 9. Tests

Channel tests: `describe "channels"` (`tests/ChatTests/Groups.hs:254`, ~70). `-m "channels"` selects all of them but **not** `chatRelayTests` (`ChatRelays.hs:33`), where the owner-signature suite lives (`:243-380`).

Infrastructure is close: `memberJoinChannel'` already takes `owners :: [TestCC]` and `Groups.hs:9840` passes a 2-element list. Gaps: `promoteChannelMember` (`:8773`) hardcodes one owner and one relay; `prepareChannel*` takes one `owner` and registers relays under its `/relays` (precedent for a second registration at `ChatRelays.hs:351`, why `setupRelay` returns the link); profiles run out at `frank`, so **2 owners + 1 relay + 3 subscribers** is the ceiling; `it` has a 90s timeout (`Utils.hs:94`) that 6 clients plus `threadDelay 1000000` between roster steps will approach.

Patterns: `<###` (`getInAnyOrder`, `Utils.hs:449`) with `EndsWith` where the actor prefix is nondeterministic (unresolved profiles render by member-id hash); `withCCTransaction` SQL for state with no console output (`checkMemberRole`, `Groups.hs:9846`) — the served roster arrives async and silently. `testChannelOwnerKeyAfterLinkUpdate` (`:9545`) is the owner-key template; its `COUNT(1) … member_role = 'owner' AND member_pub_key IS NOT NULL` → `[[1]]` must become `[[2]]`.

Cases: owner 2 promoted, verified by an existing subscriber (TOFU) and a later joiner (link); owner 2 posts as the channel; owner 2's admin message verified by a subscriber that only saw owner 1; owner 2 adds a relay; owner 2 adds owner 3 (chain order); owner 1 offline throughout. §4.1 needs a deterministic race test: two owners, two relays, same version, forced interleaving — both relays and all subscribers converge, loser self-corrects. Adversarial, modelled on `ChatRelays.hs:277-287`: replayed `OwnerAuth`; relay substituting an owner key; replayed `x.grp.promote.acpt`; chain published out of order (must fail closed, and *before* `LSET`); promotion attempted on an unverified key.

## 10. Docs

`channels-overview.md` §Governance — v7 to current; qualify creator anonymity; record the §4.1 tie-break. `channels-protocol.md` — new §Owner addition, `x.grp.promote.*` in the signing table. Closes `Internal.hs:1508`, `Subscriber.hs:1426`, `:1451`, `Groups.hs:2320`.

## 11. Implementation gaps to close (found in review)

These do not change the design but are load-bearing pieces the sections above under-specify; an implementer that misses one ships a broken feature.

- **Pending-promotion storage** is member-row columns (per-invitee), not a table — the "durable owner-request worker + table" phrasing is wrong (relay-request state is columns on `groups`, no table). Same shape, on the invitee's / own membership row.
- **Thread the whole `OwnerAuth` into owner records.** `createLinkOwnerMember` (`Groups.hs:3508`) and `updateRelayGroupKeys` (`Groups.hs:2321`) drop `authOwnerSig`; `owner_auth_sig`/`owner_auth_index` stay NULL unless every owner-materialisation site is extended, and a non-creator owner then cannot rebuild the ordered chain to publish (§4.3).
- **Per-relay count storage.** "Max across relays" that can *decrease* needs each relay's last report retained (a column on the relay's `group_members` row); the single `public_member_count` scalar only supports a never-decreasing running max.
- **Client model plumbing.** `canManageLink` (on `GroupInfo`) and `Maybe MemberRoleProposal` + `promotionPending` (on `GroupMember`) must be added to the Haskell types, their JSON, and both Swift and Kotlin structs — with `omittedField`/nullable defaults for remote-desktop forward-compat. None exist today (`grep` is empty across `apps/`).
- **New `RcvGroupEvent` case** (`RGEMemberPromotionInvited`) needs Swift + Kotlin enum cases + strings; both enums are exhaustive with no catch-all, so a missing case throws on M's own device.
- **Invitee banner is new code, not a mirror.** M's own support scope is `memberSupport(nil)`, and the existing pending-member bars are gated on a non-null scope member, so they are skipped; a new composer branch keyed on the self scope, reading the proposal from `groupInfo.membership`, is required on both platforms.
- **Picker gate.** `canChangeRoleTo` hard-returns `[.observer,.member]` for channels behind `isOwner` (`ChatTypes.swift:3092`, `ChatModel.kt:2668`); add `.owner` and decide its gate (link-write capability, not bare `isOwner`).
- **Relay status is a read**, retained mid-handover — gate relay *management* on `canManageLink` but keep relay *status display* on `isOwner`.

## 12. Open

**Owner removal.** Shape settled, details open. Revoke the leaver's link-write key, broadcast the removal, and **keep its `OwnerAuth` entry**, so every owner it transitively signed stays valid and `validateLinkOwners` never breaks. The chain becomes append-only for good — which is already its merge rule (§4.2) — and the entry is inert once the key is revoked, since a signature that cannot be written reaches no one. Enforcement is at SMP, not in the chain: dropping the key from `recipientKeys` means the removed owner can no longer authorise *any* recipient command, including adding a key back.

Open:
- **Attributing a recipient key to an owner.** The server's key set is anonymous (§3), which suffices for adding but not for removing a named owner: you must know which key is theirs. Neither home is clean. On the server it would mean telling the SMP operator which app-level identity holds which key. In link data it is worse than it looks: link data is encrypted under a key derived from the link URI, so *anyone holding the link* can read it — an SMP operator who joins the channel could match the published map against its own `recipientKeys` and learn which owner is behind each request, tying an owner's session to its channel identity. The map wants to be owner-only — distributed among owners rather than published — which is a mechanism this plan does not have. Settle before designing removal; it is the reason removal is not a small addition.
- **A revocation marker.** Otherwise a new joiner reading link data recreates the removed owner as `GROwner` — `createLinkOwnerMember` builds owner records from the chain, so an entry left for signature validity would also read as current authority. A flag on the entry, or a list of revoked member IDs in chat's `userData`; either is chat-side.
- **The chain is append-only and link data is capped** at 13,784 bytes, shared with the profile and its image; an entry is ~108 bytes. Owner churn is therefore bounded, and exceeding the cap would brick the link.

The attribution problem is the **same root gap as promotion delivery (§3)**: both need data restricted to owners, which the codebase cannot express (only `≥ moderator` scope and public broadcast exist). An owner-scoped distribution primitive — deliver/store a signed `memberId → linkRcvKey` map among `getGroupOwners` only — solves both, and is the piece to design first. Until it exists, removal cannot ship; the feature launches add-only.

Note removal is **not a security boundary against a malicious owner**: any owner can already destroy the channel (§8), and could have added a second recipient key it controls (any owner may RKEY the set) before being removed. It is an administrative operation for cooperative cases.

**Last-owner protection.** Missing today, independent of removal: `APIMembersRole` blocks only `selfSelected`, so two owners can already demote each other into an ownerless channel, and RULE-19 is UI-only. Worth adding regardless.

**Count-publisher handover.** "The next owner takes over when the first stops writing" needs a rule, but not a good one: getting it wrong means two owners briefly publishing the same relay-sourced number, one rejected. Settle during implementation.

**Disseminating the fingerprint after a write.** An owner could tell the others what the link now hashes to, so the next writer is warm. **Broadcasting the fingerprint alone is unsafe:** it attests *"I merged onto exactly these bytes"*, so an owner adopting one for content it does not hold passes the check while merging onto a stale view — accepted *and* clobbers, worse than the rejection it avoids. Safe forms cost more than they save: broadcasting content verbatim spends ~13.8 KB on every other owner per write whether or not they write next; hashing the plaintext would make a bare-fingerprint broadcast self-verifying but needs byte-exact agreement to ever hit. Not worth it while writes are weekly; revisit if owners alternate often.
