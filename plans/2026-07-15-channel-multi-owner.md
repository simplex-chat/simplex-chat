# Channel multi-owner

A channel can have N equal owners, each acting independently, per `docs/protocol/channels-overview.md` §Governance ("Near-term (v7): Multiple owners, any-owner-decides"). Owners are fully equal: any owner can take any administrative action, including destroying the channel. No coordination between owners is required for any action.

## 1. What an owner must be able to do

1. Publish content, and publish as the channel.
2. Sign administrative messages that subscribers accept — profile, prefs, member removal, role changes, relay announcements, channel deletion.
3. Change member/moderator/admin roles, which means signing the roster.
4. Write the channel's link data — add/remove relays, update the profile and member count, and add the next owner.
5. Add another owner.

Today only the creator can do 2, 4 and 5, and 3 works only for one owner at a time. Each is enforced in a different place, so each needs its own change; item 4 is the one that reaches into simplexmq.

## 2. Current state

**The protocol layer is already built for this; the client is not.** `UserContactData.owners :: [OwnerAuth]`, `validateLinkOwners` (chain: each entry signed by root *or an earlier entry*), and `decryptLinkData` (accepts data signed by root *or any listed owner*) exist in simplexmq and are multi-owner-correct. `ShortLinkCreds.linkRootSigKey` exists with the comment *"in case the current user is not the original owner"*.

**SMP already supports multi-writer links.** `RKEY :: NonEmpty RcvPublicAuthKey -> Command Recipient` and `recipientKeys :: NonEmpty` landed in commit `b7a95422` *"smp server: short links and owners for channels"* — "support multiple rcv keys", with tests — at `shortLinksSMPClientVersion = 4 = currentSMPClientVersion`, so every server that can host a channel already accepts it. **The agent never calls RKEY.** This is the mechanism for item 4.

**Two blockers in simplexmq:**
- `AgentStore.hs:2514` — `linkRootSigKey = Nothing` on every load, with `-- TODO linkRootSigKey should be stored in a separate field`. There is no column, so the non-root branch of `validateOwners` is dead code.
- `Crypto/ShortLink.hs:80` — `newOwnerAuth` *generates the key pair locally*. Adding a remote owner must sign an externally supplied public key. The formula `sign(signingKey, ownerId <> encodePubKey ownerKey)` is duplicated in chat at `Internal.hs:1519`.

**The single-owner assumption is concentrated in one function.** `groupLinkData` (`Internal.hs:1509`, carrying `-- TODO [relays] owner: set owners on updating link data (multi-owner)`) rebuilds `owners` on every publish as a singleton containing *only the publishing user*, and as `[]` unless the user holds `GRKPrivate`. So (a) a second owner is never published; (b) once two owners can publish, each publish evicts the other; (c) a non-root owner would publish an empty owners list, making the link data unreadable to everyone.

**Owners are deliberately not in the roster.** `isRosterRole = GRMember | GRModerator | GRAdmin` (`Internal.hs:1281`: *"owners are on the link, not the roster"*); link data is the sole authority for owner keys (`Groups.hs:3533`: *"Role and key stay owner-authoritative … so taking either from an in-band relayed intro would let a compromised relay substitute them"*). This is preserved. Since `updateGroupFromLinkData` carries only profile + member count and runs only on connect/preview, a new owner goes into link data **and** is announced by a signed `x.grp.mem.role`, so existing subscribers learn it promptly rather than at their next join.

**`APIMembersRole … GROwner` on a channel is currently not rejected by the backend** — only by the iOS/Kotlin pickers. Via CLI it half-works: it skips the owner-only guard (`isRosterRole GROwner == False`), sends no key, publishes nothing to the link, and leaves the promoted device throwing `SEGroupLinkNotFound` from `updatePublicGroupData` on every `x.grp.mem.new`. The flow in §3 replaces that path.

## 3. Protocol

`APIMembersRole … GROwner` on a channel triggers this flow instead of performing a plain role change. It is two-phase and asynchronous: the command returns "invitation sent" and the role lands later.

Owner 2 must already be a channel member. Their member key is then already known to relays and subscribers, and the relay connections they will publish through already exist — a subscriber creates `GRRelay`/`GCHostMember` rows, so `getGroupRelayMembers` already returns them for a promoted subscriber and the transport works unchanged.

Two events, both `requiresSignature`, both over the existing relay connections to the invitee. They are named for promotion rather than ownership because the shape is general: a later release promoting to any role reuses them unchanged, and only the link-write fields are owner-specific.

```
x.grp.promote.inv    { invitationId, memberRole, linkRcvId? }   -- O1 -> M
x.grp.promote.acpt   { invitationId, memberKey, linkRcvKey? }   -- M  -> O1
```

- `memberRole` rides in the invitation even though acceptance is automatic, so a later release can let the invitee accept or reject a named role without a wire change.
- `invitationId` binds the acceptance to one invitation, so a relay cannot replay an old one.
- `memberKey` is the invitee's existing channel member public key — already signing its messages, already known to relays and subscribers. No new signing key, and not new information: O1 already holds it, and O1 **must** check the two match and abort otherwise, so a promotion can never introduce a key.
- `linkRcvId` is the link queue's recipient ID; `linkRcvKey` is a fresh SMP recipient auth **public** key the invitee generates. Both appear only for an owner promotion, since only an owner writes link data.

Everything else owner 2 needs in order to write link data is public or derivable: `shortLinkKey` and `shortLinkId` from the channel link, and the fixed-data plaintext (hence `rootPubKey`) via `LGET`, re-encrypted under a fresh nonce — readers check `sha3_256(fd) == linkKey` and the root signature over the *plaintext*, not the ciphertext.

**There is no third message handing over credentials afterwards.** `linkRcvId` rides in the invitation because it authorises nothing on its own: `Server.hs:1249` verifies *every* recipient command against `recipientKeys`, so without a private key whose public half O1 has RKEY'd, the recipient ID is inert — which is equally why the relay carrying these messages learns nothing useful. An invitee that declines is never RKEY'd and never appears in the link, so it holds a useless identifier and nothing else. And M needs no confirmation message: it learns it is an owner from the `x.grp.mem.role` broadcast, which it receives as an ordinary member.

O1 on acceptance, driven by a durable worker (mirroring `runRelayRequestWorker`: multi-step, cross-network, must survive a crash mid-way):

1. `RKEY` — add `linkRcvKey` to the link queue's recipient keys.
2. `LSET` — publish `owners = [oa1, …, oaN, mkOwnerAuth memberId2 memberKey signingKey]`.
3. `x.grp.mem.role memberId2 GROwner (Just memberKey)` to relays → forwarded to subscribers.

LSET precedes the role broadcast so a joiner in the window learns owner 2 from the link — the authority — rather than by TOFU.

A relay cannot forge an acceptance: it is signed by M's key and bound to `invitationId`. It can drop the exchange, which is a liveness failure, not an escalation. What makes this sound is not the messages themselves but that M's key was verified out of band before the promotion — §8.

## 4. Concurrency

There is no consensus protocol between owners, and there should not be. "Any-owner-decides" is defined by the absence of coordination, so a propose-then-commit round would contradict the governance model rather than implement it. It would also fail on the facts: owners are sleeping phones with no direct connections to each other, so a quorum could take hours and a two-owner channel would need both online to do anything; owners reach each other only through relays, so a relay could partition them and freeze governance, which is a capability the threat model specifically denies it; membership changes are reconfiguration, the hardest part of any consensus protocol, and adding owners is precisely what this feature does. Worst of all it inverts the point of the feature — multi-owner exists so a channel survives losing an owner, and quorum means losing one can stop the channel dead.

What is needed instead is convergence without coordination, and the two pieces of shared state have different storage, so they take different standard answers: link data is a single copy on a server, so it wants optimistic concurrency (§4.2); the roster has a copy on every relay and no central store, so it wants a last-writer-wins register with a deterministic timestamp (§4.1). The subscriber count stops being shared state at all once it is relay-derived.

### 4.1 Roster: same version from two owners, multiple relays

With O1 and O2 both at version N-1 and both making a change:

```
O1 -> R1,R2 : roster N (content A)
O2 -> R1,R2 : roster N (content B)
R1 sees A then B: fresh = N >= N -> applies B    -> R1 = B
R2 sees B then A: fresh = N >= N -> applies A    -> R2 = A
```

Both relays accepted both, in different orders, and now serve different rosters to different subscribers, permanently. The cause is that `fresh = maybe True (v >=) gate` (`Subscriber.hs:3302`, and `notBelowRoster` at `:3430` for the blob) is not a total order — at equal versions the outcome depends on arrival order.

**Make the version a total order by tie-breaking on the author's `memberId`.** Accept iff `(v, authorMemberId) >= (gate_v, gate_authorMemberId)` lexicographically. `memberId` is 12 random bytes and identical on every device, so every node computes the same winner. `roster_sending_owner_gm_id` already exists on `groups` but is a **local row id**, not comparable across devices; add `roster_version_owner_id BLOB` beside `roster_version`.

The same interleaving, with `memberId(O2) > memberId(O1)`:

```
R1 sees A -> gate (N,O1); B: (N,O2) >= (N,O1) -> accept -> R1 = B
R2 sees B -> gate (N,O2); A: (N,O1) >= (N,O2) -> reject -> R2 = B
```

Both relays converge on B regardless of order, as do subscribers and O1. `>=` is retained rather than `>` to preserve the existing same-owner behaviour, which is deliberate (`Subscriber.hs:3288-3291`: a preceding signed event may already have advanced `roster_version` to this blob's version) — an equal tuple from the same owner still accepts.

**Rosters must reach owners.** Today `sendRosterBlob` targets `getGroupRelayMembers` only, and the relay→subscriber broadcast was deliberately removed, so an owner never sees another owner's roster and its counter goes stale. **Relays forward `XGrpRoster` and its `BFileChunk`s to members with role `GROwner`, excluding the author** — a bounded fan-out, owners only, not all subscribers. This keeps every owner's version current so collisions stay rare, and it is what lets O1 receive (N,O2), lose the tie-break, and self-correct to the network's state.

The losing owner's change is dropped: O1 adopts B and the change it made reverts. There is no automatic retry — an owner re-applying its delta at N+1 would be two owners fighting over a whole-value object, the same reason profile edits are not retried (§4.2). The UI surfaces the revert so the user can re-apply if they still want it.

The gap/frontier logic is unaffected: the counter remains a single shared sequence (each owner computes `local + 1`), so the tie-break introduces no gaps in `nextCompleteVersion`.

### 4.2 Link data: concurrent LSET

There is one link queue on one server, so writes serialise and the stored state never diverges. Two things make a lost write both possible and invisible.

**Every write is a blind whole-object overwrite.** `groupLinkData` rebuilds the entire `UserContactData` from local state, so each publish asserts *the whole link is what I think it is* — including fields the writer has no knowledge of. Correct for a single writer, wrong for N.

**An owner's confirmation is its own echo.** `notify $ LINK link userLinkData` (`Agent.hs:1813`) returns *the data the client sent*, not the server's state, and the relay-activation reconciliation (`Subscriber.hs:1411-1458`) reads `relays` out of that echo to promote relays to `RSActive`. So an owner marks a relay active because it asked for it to be listed. With one writer that is harmless — what you sent is what is stored. With N writers it is the reason nobody notices: the owner adds a relay, another owner's concurrent publish drops it, the relay's own `checkRelayServedGroups` sees its link missing and goes `RSInactive`, and the owner that added it still shows it active. Nothing in the system reads back the truth.

Any loss here is damaging in some way — a dropped relay silently stops serving, a dropped owner entry leaves an accepted invitee unpublished, a dropped profile or prefs update simply vanishes — so the design below is about eliminating losses and detecting the rest, not about making them rare.

**First, fix where the subscriber count comes from.** It keeps updating on every join — a count that visibly moves is what makes a channel look alive, and every other write is rare — but no owner can compute it correctly, so the number must come from elsewhere.

`publicMemberCount` is published on every join and leave: `updatePublicGroupData` is reached from six sites (`Subscriber.hs:3137/3152/3170/3658/3705`, `Commands.hs:3035`), gated on `memberRole' membership == GROwner`. The number it publishes is `summary_current_members_count`, a DB-trigger count of rows in *that device's* `group_members` (`M20250919_group_summary.hs:48-81`) — "members I know about", not a global truth. A promoted owner's is systematically low, and permanently so: `introduceInChannel` announces a joiner only to `owners <> adminsMods`, so observers never learn of other observers and a promoted owner never learns the ones who joined before it. With N owners the published number would flap between each owner's private tally on every join. Making the creator the sole publisher only defers it — the number would then be wrong in perpetuity once the creator left. Introducing every existing subscriber to a new owner would fix the arithmetic at O(N) messages and O(N) rows per new owner, for a single displayed integer.

The fix is to notice **the count is already relay-derived.** An owner has no connection to subscribers; every member it knows arrived as an `XGrpMemNew` from a relay, so its "own" count is a tally of relay-sent announcements. Taking the number from the relay directly is exactly the same trust with none of the bookkeeping — and the relay already computes it (`updatePublicGroupData`'s relay branch calls `updatePublicMemberCount`); it simply cannot publish it, being neither an owner nor a link writer.

**So the relay states its subscriber count in the join and leave announcements it already sends to owners** — an optional field on `XGrpMemNew` and its departure counterparts, forward-compatible, no new event and no new fan-out. The owner mirrors it instead of deriving from `summary_current_members_count`, taking the maximum across relays, since subscribers connect to every listed relay and each relay's count approximates the total. A newly promoted owner seeds from the published link data on its first reconcile tick, so it is correct immediately even on a quiet channel where no announcement is due.

**Only one owner publishes it: the first in the chain, the next taking over when that one stops writing.** Every owner still tracks the count from the relay reports, so this is a choice of who writes, not who knows — a handover needs no catch-up, and the number never depends on which owner made it. The chain order (`owner_auth_index`) is already stored and identical on every device, so the choice is deterministic and needs no election. Without this, N owners would each write on every join: N times the traffic for one integer, of which N-1 are rejected and discarded, since a losing owner's fingerprint stays stale and it loses every time. It also keeps the link from being a target that N owners are constantly moving, which is what would otherwise starve human writes (§4.2).

Publishing stays per join, so the count moves as joins arrive and the channel looks alive. `updatePublicGroupData` keeps its trigger unchanged; only the *source* of the number, and which owner publishes it, change. That matters, because it does double duty and must not simply be dropped: there is no relay-removal command — a relay is removed by removing the relay member through `APIRemoveMembers` (`Commands.hs:3033-3035`), and it leaves the link because `getPublishableGroupRelays` requires `member_status = GSMemConnected` (`Store/Groups.hs:1470`); a relay leaving of its own accord arrives as `xGrpLeave` (`Subscriber.hs:3705`) and publishes the same way. Keeping the per-join publish keeps both working.

This makes owners symmetric — the published number no longer depends on which owner wrote it — and survives the creator leaving. It also disposes of the wider worry: the count was the only thing that needed a *complete* member list, so a promoted owner's partial one stops mattering. Moderation learns members lazily, on their first post (`Subscriber.hs:4195`), which is exactly when they are needed; the roster arrives as a served snapshot on join.

A relay can state a false number, but it can already do that by fabricating or withholding announcements — the threat model records count manipulation as detectable-not-prevented, and detection is unchanged: an owner comparing across relays sees the discrepancy.

**Second, make LSET a read-modify-write of the published data rather than a re-derivation from local state:** `LGET` → decode → apply only this write's delta → `LSET`. Each field then has a defined merge rule:

| field | semantics | rule |
|---|---|---|
| `direct` | constant for a channel | preserve |
| `owners` | append-only ordered chain (§4.3) | published, with my new entry appended |
| `relays` | set with meaningful removals | published, plus or minus the one relay this write adds or removes — never this owner's whole local list |
| `groupProfile` | whole object | overwrite only if this write *is* a profile change |
| `publicMemberCount` | relay-reported telemetry | overwrite only if this write carries a newer relay report |

This subsumes two further failures: an owner's publish can no longer evict another owner, and a profile edit can no longer resurrect a relay another owner removed, because the relay set comes from the published data rather than a stale local snapshot.

**The merge-and-retry lives in one place, not at every call site.** Callers stop building link data and instead say what they want changed — a delta: this profile, this relay added, this relay removed, this owner appended, this count. One function owns the whole cycle: take the published data, apply the delta by the table above, `LSET`, and on rejection re-read, re-apply the same delta and write again, bounded, returning a conflict only if the delta's own field moved underneath it. The merge rules exist once, the retry exists once, and a caller cannot get either wrong because it never sees them.

The structure already fits: every group link write funnels through `groupLinkData` (`Internal.hs:1509`) via exactly two callers, `setGroupLinkData` (sync) and `setGroupLinkDataAsync` (async). So `groupLinkData` — today "build the whole thing from local state" — becomes "apply this delta to the published state", and the two callers become the one place that loops. Channel creation is unaffected: it is the first write, with nothing to conflict with.

The async path needs the loop on the chat side, not the agent's. `setConnShortLinkAsync` enqueues the fully-built data into the agent's command queue and executes it later (`Agent.hs:1811`), so under compare-and-swap it could be rejected long after the caller returned — and the agent cannot re-merge, since merge policy is chat's and the agent cannot read the content. So the async callers (relay connected, roster acked, member count) enqueue a delta to a per-group link worker that runs the same loop off the message-processing thread. That also serialises this owner's own writes, so it never competes with itself.

**Third, add an owner-side reconcile tick.** An owner must also *learn* what other owners published, and there is no mechanism for it today: `checkRelayServedGroups` (`Commands.hs:5254`) polls the link on a timer but iterates `getRelayServedGroups`, i.e. groups this client serves *as a relay*, so an owner never runs it; and `syncSubscriberRelays` runs only inside `APIGetUpdatedGroupLinkData` (`Commands.hs:1909-1921`), a user-initiated command, and is skipped for owners outright (`:1918`).

So this is new: a periodic `LGET` of the channel's own link for groups where the user is an owner — the same shape and interval as the relay's check, and the natural home for it is alongside that worker. It does three jobs: adopt what other owners published (`syncSubscriberRelays` already implements the relay-set half; it needs to run for owners other than the one that just published), seed a newly promoted owner's subscriber count, and keep this owner's CAS fingerprint warm.

**Fourth, compare-and-swap on `LSET`.** With the count on a single publisher, every remaining write is human-initiated and rare, so conflicts are rare — but a lost one is still damaging, and the client-side alternative cannot prevent one. That alternative, for the record: merge onto the warm copy and `LSET` blind. A concurrent write is still lost and the writer cannot tell at the time, so reconciliation is after the fact, on the tick — the owner compares the published data against what it believes and either re-applies its change or reports it overwritten. Detecting the loss at all means durably recording what it intended, since its local state already shows the change as applied, and the loss surfaces only at the next tick. Detecting at write time instead needs a verify `LGET` after every write: a third round trip that still leaves a hole, for a clobber landing after the verify. So it is more machinery, slower, and still lossy.

**Compare-and-swap.** "Write this, but only if what is stored is still what I read." The writer passes a fingerprint — a hash of the stored user-data bytes its merge was based on — and the server stores the new data only if the current bytes still hash to that value; otherwise it rejects and stores nothing. A rejection *is* the detection, so no verify read is needed: re-read, re-merge, retry. Lossless.

The fingerprint is of the **stored ciphertext**, not the plaintext: the server never decrypts anything, so it can only hash the bytes it already holds (`queueData qr`). It works as a version token for free, because every write carries a fresh nonce and therefore changes the stored bytes even when the plaintext is identical.

**Where a writer gets the fingerprint.** Only two ways, because it is a hash of ciphertext: either you produced those bytes, or you read them. Your own successful write gives it to you — the stored bytes are the bytes you just sent, hashed on the way out. Otherwise the reconcile tick's `LGET` gives it to you. There is no third source: **another owner's write is opaque until you read it**, and you cannot re-derive its fingerprint from your local plaintext, since the nonce is fresh on every write. It is persisted next to the link credentials, so a restart does not force a read.

**Precisely: a write is rejected exactly when another owner has written since you last read or wrote.** So owners alternating do pay — the first write after another owner's is rejected, costing the rejection plus a read, and only then is that owner warm again. That is optimistic concurrency working as intended: you pay only when a conflict actually happened, which for human-paced writes means once per alternation.

A rejection is not free, though. `LSET` always uploads the user data padded to a fixed 13,784 bytes (`userDataPaddedLength`, `Crypto/ShortLink.hs:45`), so a rejected write wastes ~13.8 KB whatever it was changing. Two consequences. The tick must not be tuned to *prevent* rejections by keeping fingerprints warm — polling every owner on a short interval costs far more than the rejections it would save on writes that happen weekly; its frequency should be set by its real jobs, and a stale fingerprint is simply one of the things it happens to fix. And a writer frequent enough to be rejected *every time* would be a real cost rather than a tidiness concern — the subscriber count is the only one, and it is why that has a single designated publisher.

**The fingerprint never reaches chat.** The agent performs the encryption, so it is the only layer that sees the stored bytes; it computes the fingerprint, keeps it with the link credentials it already stores, and refreshes it on every successful write and every `LGET`. Chat calls `setConnShortLink` as it does now and gets back success or a conflict error; on conflict it re-reads, re-merges and calls again. Merge policy stays in chat, link crypto stays in the agent.

**Why not hash the plaintext instead**, so every owner could compute the fingerprint without having seen the ciphertext? It does not reduce rejections: any compare-and-swap token — ciphertext hash, plaintext hash, or a plain version counter — is stale exactly when another owner has written since you last read or wrote, so the rejection rate is identical. What plaintext hashing would buy is a *deterministic* token, computable by anyone who knows the content — so an owner could keep it warm from the in-band messages it already receives (`XGrpInfo`, `XGrpRelayNew`, `XGrpMemNew`) instead of reading. Three things spoil that. The owner would have to reconstruct the stored plaintext byte-for-byte, including the JSON encoding of `userData`, and any divergence yields a mismatch that no retry resolves without a read. Those in-band messages are not a reliable ordered log — relays may drop or reorder them and there is no total order across owners — so a shadow copy drifts. And the server cannot compute a plaintext hash at all, having no key, so it would have to store a client-supplied opaque token: a storage change, and one no party can verify, where the ciphertext hash is computed on demand from bytes the server already holds. Rejections are only frequent for a high-frequency writer, and the only one is the subscriber count, which its single designated publisher already settles — one writer is always warm.

```
without CAS                              with CAS
O1: LGET -> {News, [R1,R2]}              O1: LGET -> {News, [R1,R2]}   digest a
O2: LGET -> {News, [R1,R2]}              O2: LGET -> {News, [R1,R2]}   digest a
O1: LSET {News,  [R1,R2,R3]}   stored    O1: LSET expect a {News, [R1,R2,R3]}    -> OK, now b
O2: LSET {Daily, [R1,R2]}      stored    O2: LSET expect a {Daily, [R1,R2]}      -> REJECTED
                                         O2: LGET -> {News, [R1,R2,R3]}  digest b
= R3 silently gone. It stops serving.    O2: LSET expect b {Daily, [R1,R2,R3]}   -> OK
  O1 still shows it active.              = both changes survive.
```

Operations per change:

| | uncontended | contended |
|---|---|---|
| today (single writer) | 1 — `LSET` | n/a |
| merge, write blind | 1 — `LSET` | 1, silently lost |
| merge, write, verify | 3 — `LGET`, `LSET`, `LGET` | 3, hole remains |
| **compare-and-swap** | **1 — `LSET`** | 3 — rejected `LSET`, `LGET`, `LSET` |

So it is not a correctness tax on top of the cheap design; it *is* the cheap design. It is as cheap as today in the common case, because the fingerprint is already known, and it *removes* machinery rather than adding it: no verify read, no durable per-write intent, no retry-until-observed loop, no residual hole to document.

Server-side it is a check beside the existing `lnkId' /= lnkId -> err AUTH` in the `LSET` handler (`Server.hs:1484`) — the data to hash is already there as `queueData qr`, so no storage change and no read-side change, since `LGET` already returns the exact ciphertext. Costs: a new command tag (`LSET`'s encoding is positional, so extending it in place is messy and a new tag version-gates cleanly), a new error for the rejection — none of the existing `ErrorType` constructors fit — an SMP version bump (`currentSMPClientVersion` 4 → 5), and agent plumbing. Against a server that predates it there is no compare-and-swap available, so writes fall back to merge-and-write-blind with its silent-loss window; that fallback is needed regardless, since the link lives on whatever server the creator chose.

**A rejection is not a conflict, and it does not reach the user.** It says *something* changed, usually not the thing this writer is changing — an owner editing the profile is rejected just as readily by another owner adding a relay. So the rule is: re-read, and if the field I am changing is untouched, re-merge and retry; that is always safe, whatever the field. The user sees a save that took one extra round trip, not an error. Only if my field *also* changed is it a genuine conflict, and only then does the field's nature matter — set-elements (relays, owners) still re-merge and converge; whole values (profile, prefs) cannot, because retrying would be two owners overwriting each other, so the writer yields and reports the winning value. That report is the one thing a user ever sees, it means another owner really did edit the same field seconds earlier, and they should see it — writing blind produces no error at all in that same case and the edit simply disappears. Retries are bounded, and only exhausting them is a real failure.

**What would make rejections user-visible is a high-frequency writer**, and the count is the only candidate — which is why it has a single designated publisher (§4.2, above). Left to every owner it would make the link a target that N of them are constantly moving, and a human profile edit could be rejected, re-read, re-merged and rejected again by the next join. With one publisher the link has one occasional writer, and rejections are what they should be: rare, internal, and invisible.

**Where the local state sits relative to the write decides what a rejection costs**, and it differs per operation:

| write | trigger | link write | on rejection |
|---|---|---|---|
| profile / prefs | `runUpdateGroupProfile`, user-facing | **sync** | re-merge and retry; on a genuine conflict abort before the local commit and show the user the winning value |
| relay add | relay's acceptance → worker | async | worker's existing retry |
| relay remove | member removal → `updatePublicGroupData` | async | worker's existing retry |
| owner add | acceptance → worker (§3) | async | worker's existing retry |
| subscriber count | join / leave | async | drop it; the next join publishes again |

**Nothing new is recorded anywhere.** Because a rejection is immediate, no writer has to remember what it wanted in order to notice later that it went missing — that bookkeeping is a cost of (A), not of (B). A sync write fails to the user, who retries, exactly as with any other error.

Nor do the async writes need bespoke retry logic. They are already durable multi-step network flows with retry state and backoff (`relay_request_retries`/`_delay`/`_execute_at` on `groups`, driven by `retryTmpError`, `Subscriber.hs:4404`), because they must survive a crash regardless of concurrency. A rejection is a textbook temporary error — the next attempt will almost certainly succeed — so classifying it as one puts it straight into the existing loop, and the existing expiry surfaces a relay error to the UI if attempts keep failing. Their progress is already tracked by a state machine — a relay sits at `RSAccepted`/`RSAcknowledgedRoster` until the write lands and only then becomes `RSActive` — so a rejection leaves it pending, with nothing to revert. The only fix there is that activation must key off an accepted write rather than the `LINK` echo. Failing outright and asking the user to re-add a relay would be worse for no saving: re-adding means re-inviting a relay that has already accepted.

The profile is the one that needs reordering. `runUpdateGroupProfile` commits the new profile locally (`Commands.hs:4016`) *before* the synchronous link write (`:4034`), so any failure — a CAS rejection, or just a network error today — leaves the profile changed on this device and nowhere else. Building the link data from the new profile does not require persisting it first (`groupLinkData` is pure), so the order should be: merge and write, then commit locally and broadcast. Then a rejection retries invisibly, a genuine conflict aborts with the local state untouched and the user sees the other owner's value, and the pre-existing "profile silently diverges when the link write fails" bug goes away with it.

Two owners genuinely disagreeing — one adding a relay, the other removing the same one — is not a race and no merge rule resolves it; last writer wins.

### 4.3 Owner chain ordering

`validateLinkOwners` requires each entry to be signed by root or by an entry **earlier in the list**. Since owner 2 can add owner 3 using its own key, order is load-bearing and must be persisted, not derived. Two owners concurrently appending an owner is an LSET race (§4.2): one append is lost and its invitee is left having accepted but never published — hence the worker's verify-and-republish step in §3.

## 5. Backend work (simplex-chat)

**Persist the owners chain.** Add `owner_auth_sig BLOB NULL` and `owner_auth_index INTEGER NULL` to `group_members` (migration + cabal + SQLite and Postgres). The public key is already there as `member_pub_key`, so this cannot drift from the member record by construction, and the index carries the order §4.3 requires.

**Restructure the publish path as a delta (§4.2).** `groupLinkData` currently returns a whole `UserContactData` built from local state, and both callers (`setGroupLinkData`, `setGroupLinkDataAsync`, `Internal.hs:1427/1437`) hand it straight to the agent. It must instead take the published data and a delta describing what this write changes, and return the merged object. That is the single change that makes multi-owner publishing safe; the alternative — leaving it a snapshot and hoping owners do not collide — cannot be made correct.

- `Internal.hs:1509` `groupLinkData` — becomes "apply this delta to the published data" rather than "build the whole thing from local state": read the stored chain, delete the `GRKPrivate`-derived singleton and the `_ -> []` fallback. Its two callers, `setGroupLinkData` (sync) and `setGroupLinkDataAsync` (async), become the single place that owns merge, compare-and-swap and bounded retry (§4.2) — no caller sees any of it.
- Per-group link-update worker for the async writers (relay connected, roster acked, member count): they enqueue a delta rather than fully-built data, because `setConnShortLinkAsync` executes the agent's queued command later (`Agent.hs:1811`) and the agent cannot re-merge on rejection. Also serialises this owner's own writes.
- `Internal.hs:1458` `updatePublicGroupData` — trigger unchanged (it still publishes per join/leave, and is also the only publish for relay removal and departure); the number now comes from the relays' reports rather than `summary_current_members_count`, and count publishes are coalesced to at most one per few seconds (§4.2). A relay-set change is not coalesced.
- `XGrpMemNew` and its departure counterparts gain an optional relay-stated subscriber count; the relay fills it from the number its own `updatePublicGroupData` branch already computes. Owner side stores it per relay and uses the maximum across active relays, seeding from published link data on first reconcile.
- `Internal.hs:1458` `updatePublicGroupData`, `Commands.hs:4011` `runUpdateGroupProfile`, `Commands.hs:2713` `APIAddGroupRelays` — all gate on `memberRole' membership == GROwner` but need "can write link data" (an owner mid-handover cannot). One predicate, used at all three.
- `Commands.hs:4016` `runUpdateGroupProfile` — commits the profile locally before the synchronous link write at `:4034`. Reorder to write first, then commit and broadcast (§4.2); this also fixes the existing case where a failed link write silently diverges the profile.
- `Commands.hs:3268` `APIAddGroupShortLink` asserts no role at all — add the same gate.
- `Commands.hs:2872` `APIMembersRole` — route `newRole == GROwner` on a channel into the §3 flow, and check `memberVerifiedCode` is set on the invitee (§8); the promotion signs that key into the owners chain, so an unverified one is a relay assertion.
- The invitation must carry the key O1 verified, and the acceptance's `memberKey` must equal it — otherwise abort. Verification is worthless if the key can change between verifying and promoting.
- `Subscriber.hs:3345` `allowCreate` — widen so an owner-signed `x.grp.mem.role` carrying a key can TOFU-create a `GROwner`; `isRosterRole GROwner == False` blocks it today, so subscribers can never materialise a new owner.
- `Commands.hs:2936` `mKey` — send the key on owner promotion, not only when a roster version is present.
- `Subscriber.hs:4195` — the "owners are already known to every member" skip is false for a newly promoted owner; disseminate their profile.
- `Subscriber.hs:1422` — `publicMemberCount > 1` assumes exactly one owner. Use a subscriber count or `> ownerCount`.
- `Subscriber.hs:1411-1458` — the `LINK` handler promotes relays to `RSActive`/`RSInactive` from `auData`, which is the client's own sent data echoed back (`Agent.hs:1813`), not the server's state. Relay activation must key off an accepted write (a CAS-confirmed `LSET`, or the reconcile tick under fallback), not the echo, which is self-confirming.
- New owner-side reconcile worker (§4.2): a periodic `LGET` for groups where the user is an owner. None exists — `checkRelayServedGroups` (`Commands.hs:5254`) is relay-side (`getRelayServedGroups`), and `syncSubscriberRelays` runs only from the user-initiated `APIGetUpdatedGroupLinkData` (`Commands.hs:1909`) and is skipped for owners (`:1918`).
- `Commands.hs:1192` — share-link owner signing requires `GRKPrivate`, so a promoted owner silently emits `ownerSig = Nothing`. Sign with the member key of any listed owner; `verifyLinkOwner` already resolves by `ownerId`.
- `Store/Groups.hs:2320` `updateRelayGroupKeys` — the `forM_ owners` loop exists but `getGroupMemberIdViaMemberId` throws for an owner the relay has no row for, aborting the transaction. Create the record — reuse `createLinkOwnerMember`, which the subscriber path already uses for exactly this.
- `Commands.hs:5254` `checkRelayServedGroups` — the relay's periodic worker already re-reads link data but binds only `relays`, discarding `owners` from the same `UserContactData`. Refresh owners here; it is the relay's only path to learning of owner changes.
- `Store/Groups.hs:1117` `getHostMember` — `firstRow` on `GCHostMember`; relays model exactly one owner.
- Roster: tie-break gate (`Subscriber.hs:3302`, `:3430`), `roster_version_owner_id` column, relays forward `XGrpRoster` + chunks to owners (§4.1).
- New durable owner-request worker + table, mirroring the relay-request worker.

## 6. simplexmq work

- `Crypto/ShortLink.hs` — extract `mkOwnerAuth :: OwnerId -> PublicKeyEd25519 -> PrivateKeyEd25519 -> OwnerAuth`; redefine `newOwnerAuth` on top of it; export. Removes the formula duplicated across the two repos.
- Persist `linkRootSigKey` — new `rcv_queues` column + migration (`AgentStore.hs:2514`). Without it the non-root branch of `validateOwners` cannot work.
- RKEY agent API — add a recipient key to a contact-link queue.
- Compare-and-swap on `LSET` (§4.2): a new command tag carrying an expected fingerprint of the stored user data, a new `ErrorType` for the rejection, `currentSMPClientVersion` 4 → 5. The server check goes beside `lnkId' /= lnkId -> err AUTH` in the `LSET` handler (`Server.hs:1484`); the bytes to hash are already in `queueData qr`, so no storage change. Agent side: keep the fingerprint with the link credentials, refresh on every successful write and every `LGET`, and surface a conflict to chat as a distinct error. Fall back to writing blind against servers below v5.
- A link-write API for an owner who does not own the queue. Prefer a standalone `setForeignLinkData` taking explicit creds over materialising a fake `RcvQueue`/`ContactConnection` on owner 2's device: the latter needs an `rcvDhSecret` owner 2 does not have, and risks owner 2 subscribing to owner 1's queue and racing it on inbound requests.
- `LINK` currently notifies with the client's own `userLinkData` (`Agent.hs:1813`). Either return the server's state or drop the payload, so callers cannot mistake it for confirmation.
- If CAS is agreed (§11): an `LSET` variant taking an expected digest of the stored user data, rejected on mismatch; the check sits beside the existing `lnkId' /= lnkId -> err AUTH` in `Server.hs:1484`. The client already has the bytes to compute the digest from its `LGET`, so no read-side change.

## 7. UI

The UI already pluralises owners (`ownersContributorsCountStr`, `ChatInfoToolbar.swift:134` / `ChatView.kt:1550`), already checks `members.contains { $0.wrapped.memberRole >= .owner }` for the members button (`GroupChatInfoView.swift:128` / `.kt:650`), and already has a `hasOtherOwner` escape hatch on leave (`GroupChatInfoView.swift:250` / `.kt:822`). The gap: `isOwner` is self-relative and count-blind (`membership.memberRole == .owner && membership.memberCurrent`) and is used ~30× as if it meant *the* owner.

**The UI has no model of link-write capability.** `grep groupKeys|GRKPrivate|rootKey` across `apps/ios` and `apps/multiplatform` returns zero hits — no key material on `GroupInfo` or `GroupMember`. It uses two proxies, both wrong once owners are equal:

- **`isOwner && groupLink != nil`** (`GroupChatInfoView.swift:111` — the site of `// TODO [relays] allow other owners to manage channel link (requires protocol changes to share link ownership)`; `.kt:638`). The `groupLink != nil` half is the accidental single-owner gate: only the creator has a `user_contact_links` row, so only they reach the link-management screen.
- **`isOwner`** alone for relay add/remove and relay status fetch (`ChannelRelaysView.swift:26/48/83`, `.kt:39/90/109`).

Both should be replaced by a `canManageLink` flag on `GroupInfo`, supplied by the backend. It is not equivalent to `isOwner`: an owner mid-handover, or one whose `x.grp.owner.creds` never arrived, cannot write the link. Gate link, relay and domain-claim UI on it.

- `canChangeRoleTo` (`ChatTypes.swift:3060`, `ChatModel.kt:2652`) hard-codes `[.observer, .member]` for channels. Add `.owner`, and handle that selecting it starts an async two-phase flow (pending → owner, or failed), not an immediate role change.
- Promoting to owner warns when the invitee's key is unverified, and routes to the existing member-verification screen (`APIVerifyGroupMember` → `verifyChannelMemberCode`). This is the only thing standing between a substituted key and an owner-signed chain (§8), so the warning has to be prominent rather than a footnote — and unlike contact verification, the consequence is not "this conversation" but the channel's trust chain.
- New: owners section in channel info; pending-owner state.
- A role change an owner made can revert when it loses a tie-break (§4.1). Surface it.
- **iOS-only:** `GroupChatInfoView.swift:303` skips the `apiGetGroupLink` fetch unless `isOwner`; Kotlin fetches unconditionally (`ChatView.kt:403/428`). Reconcile.
- RULE-19 (`apps/ios/product/rules.md:112`) says a *sole* owner must not be able to leave. The info view honours it via `hasOtherOwner`, but the chat-list swipe/context menus drop the clause — `showLeaveGroup = … && !(groupInfo.useRelays && groupInfo.isOwner)` (`ChatListNavLink.swift:272`, `:247`; `ChatListNavLinkView.kt:319`, `:341`) — so any channel owner loses Leave in the list even when other owners exist.

## 8. Security

**The invitee's key must be verified out of band before promoting to owner.** By default an owner's copy of a subscriber's key is relay-asserted: the owner has no connection to subscribers, so everything it knows about a member arrived as `XGrpMemNew`, which is unsigned and relay-authored (`Subscriber.hs:3125`). The code defends only the *reverse* direction — a receiver holding a roster-established key refuses a relay's differing assertion (`:3131-3134`) — and a plain subscriber is not on the roster, so on the owner there is nothing to check against. The joining member's own signed key proof (`encodeXMemberConnInfo`, `Internal.hs:2234`) is verified by the relay (`memberJoinRequestViaRelay`, `Subscriber.hs:1684`) and never forwarded. So a relay that substituted a member's key at join could be promoted to owner by an owner that believes it is promoting that member, and would then sign rosters and administrative messages in its own right. No in-protocol signature closes this: the substituted key *is* the relay's, so the relay signs the acceptance with it.

Out-of-band verification closes it, and the mechanism already exists. `APIVerifyGroupMember` (`Commands.hs:2021`) routes channels to `verifyChannelMemberCode`, whose code is `channelMemberCode ownKey memKey` (`Types.hs:1911`) — a hash over *both* member public keys, sorted so each side computes the same value. Two members comparing it out of band therefore detect a substitution of either key, and `setGroupMemberVerified` persists the result as `memberVerifiedCode`. This is the path to the key the relay did not mediate, and it is the same mechanism SimpleX already uses for contacts.

So promotion to owner is gated on it: the UI warns when the invitee's key is unverified, and the promotion is a deliberate act taken against a verified key. That is what makes routing `x.grp.promote.*` over relays sound — not that the messages are safe in themselves, but that the key they are built on was established outside the relay's reach.

**Any owner can destroy the channel.** `Server.hs:1249` — `vc SRecipient _ = verifyQueue $ \q -> verifiedWithKeys $ recipientKeys (snd q)`: any recipient key authorises any recipient command, including `DEL` (destroy the queue) and `LDEL` (destroy the link data), with no per-command scoping. This is consistent with any-owner-decides, and is recorded because it is stronger than any chat-level action — the link address is embedded in the published profile, so its loss cannot be repaired by re-publishing.

**RKEY revocation is a race.** `updateKeys` replaces the whole list, so O1 can evict O2 and O2 can evict O1; last writer wins. Owner removal at the SMP level is not a clean operation.

**Chain order is load-bearing.** `validateLinkOwners` rejects an entry not signed by root or an earlier entry, so a reordered or partially-published chain makes the link data unreadable to every client and bricks joins. Publish in `owner_auth_index` order and validate the chain locally before `LSET` — the agent rejects a bad chain anyway, but as `CMD PROHIBITED`, not a legible chat error.

**Creator anonymity is weaker than the overview states.** It claims all owners are indistinguishable "provided multiple owners were signed by the root key". Once owner 2 signs owner 3 with its own key, the chain reveals who delegated to whom, and root-signed entries identify the creator's cohort. Qualify the claim in the docs.

## 9. Tests

Channel tests live under `describe "channels"` (`tests/ChatTests/Groups.hs:254`, ~70 tests); `-m "channels"` selects all of them but **not** `chatRelayTests` (`tests/ChatTests/ChatRelays.hs:33`), where the owner-signature suite ("share channel card", `:243-380`) lives.

Infrastructure is close: `memberJoinChannel'` already takes `owners :: [TestCC]` and fans out the "introduced … in the channel" assertion; `Groups.hs:9840` already passes a 2-element list. Gaps: `promoteChannelMember` (`Groups.hs:8773`) hardcodes one owner and one relay; every `prepareChannel*` helper takes one `owner` and registers relays under that owner's `/relays` (precedent for a second registration at `ChatRelays.hs:351`, which is why `setupRelay` returns the link); named profiles run out at `frank`, so **2 owners + 1 relay + 3 subscribers** is the ceiling without adding profiles; the custom `it` imposes a **90s timeout** (`Utils.hs:94`) that 6 clients plus the customary `threadDelay 1000000` between roster steps will approach.

Patterns to reuse: `<###` (`getInAnyOrder`, `Utils.hs:449`) with `EndsWith` for lines whose actor prefix is nondeterministic (an unresolved profile renders by member-id hash); `withCCTransaction` SQL for state that produces no console output (`checkMemberRole`, `Groups.hs:9846`) — the served roster arrives async and silently. `testChannelOwnerKeyAfterLinkUpdate` (`Groups.hs:9545`) is the owner-key template, and its assertion (`COUNT(1) … member_role = 'owner' AND member_pub_key IS NOT NULL` → `[[1]]`) is what must become `[[2]]`.

Cases: owner 2 promoted, verified by an existing subscriber (TOFU path) and by a later joiner (link path); owner 2 posts as the channel; owner 2's admin message verified by a subscriber that only ever saw owner 1; owner 2 adds a relay; owner 2 adds owner 3 (chain order); owner 1 offline throughout. The §4.1 race needs a deterministic test: two owners, two relays, same version, forced interleaving, asserting both relays and all subscribers converge on the same roster and the losing owner self-corrects. Adversarial, modelled on `ChatRelays.hs:277-287`: replaying an owner's `OwnerAuth`; a relay substituting an owner key; a replayed `x.grp.owner.acpt`; a chain published out of order (must fail closed, and fail *before* `LSET`).

## 10. Docs

`channels-overview.md` §Governance — move v7 to current; qualify the creator-anonymity claim; record the §4.1 tie-break and the §4.2 residual. `channels-protocol.md` — new §Owner addition, and the `x.grp.owner.*` events in the signing table. Closes four TODOs: `Internal.hs:1508`, `Subscriber.hs:1426`, `Subscriber.hs:1451`, `Groups.hs:2320`.

## 11. Open questions

**Owner removal.** Out of scope here, and not yet designed. Three things make it harder than it looks: removing a chain entry invalidates every owner it transitively signed (§4.3); SMP-level revocation is a mutual-eviction race (§8); and there is **no last-owner protection anywhere** in the backend — `APIMembersRole` blocks only `selfSelected`, so two owners can already demote each other into an ownerless, unrecoverable channel, and RULE-19 is UI-only. The backend last-owner guard is worth adding regardless of how removal is designed.

**Disseminating the fingerprint after a write (§4.2).** An owner could tell the others what the link now hashes to, so the next one to write is warm and is not rejected. **Broadcasting the fingerprint alone is unsafe and must not be done.** The fingerprint's entire job is to attest *"I merged onto exactly these bytes"*; an owner that adopts a fingerprint for content it does not hold will pass the check while merging onto its stale view, so the write is accepted *and* silently clobbers — strictly worse than the rejection it avoids, because a rejection is at least honest.

The safe forms both cost more than they save here. Broadcasting the new content *verbatim* alongside the fingerprint works — the receiver replaces view and fingerprint together, and must not instead re-apply a delta to its own view, since any divergence yields a valid fingerprint over wrong bytes and the same clobber. But it spends ~13.8 KB per write on every other owner, whether or not any of them writes next, whereas a rejection costs one owner ~13.8 KB up and ~13.8 KB back, and only when it actually writes before its next tick. With one owner doing most of the writing — the normal case — the broadcast is pure waste. Alternatively, hashing the plaintext rather than the ciphertext would make a fingerprint-only broadcast self-verifying, since a receiver could accept it only if its own view hashes to the same value and otherwise fall back to reading; that is the only sound way to send a bare fingerprint, but it needs byte-exact agreement to ever hit, and it carries the server-side costs in §4.2.

Not worth it now: with the count on a single designated publisher, the writes this would optimise happen weekly. Revisit if owners turn out to alternate often.

**Handover of the count publisher (§4.2).** "The next owner takes over when the first stops writing" needs a rule — how long a gap counts as stopped, and how a resumed first owner steps back — but it does not need to be a good rule, since the cost of getting it wrong is two owners briefly both publishing the same relay-sourced number, one of them rejected. Settle it during implementation.
